with GNAT.IO;

package body MIN.Target is

   procedure Debug (Str : String) is
   begin
      if True then
         GNAT.IO.Put_Line (GNAT.IO.Standard_Error, "MIN DBG: " & Str);
      end if;
   end Debug;

   procedure Debug_CRC (Str : String) is
   begin
      if False then
         GNAT.IO.Put_Line (GNAT.IO.Standard_Error, "MIN DBG: " & Str);
      end if;
   end Debug_CRC;

   TRANSPORT_IDLE_TIMEOUT_MS : constant := 1_000;
   TRANSPORT_FRAME_RETRANSMIT_TIMEOUT_MS : constant := 50;
   TRANSPORT_ACK_RETRANSMIT_TIMEOUT_MS : constant := 25;
   TRANSPORT_MAX_WINDOW_SIZE : constant := 16;

   HEADER_BYTE : constant := 16#aa#;
   STUFF_BYTE : constant := 16#55#;
   EOF_BYTE : constant := 16#55#;
   ACK : constant := 16#FF#;
   RESET : constant := 16#FE#;

   TRANSPORT_FIFO_SIZE_FRAMES_MASK : constant :=
     (2 ** TRANSPORT_FIFO_SIZE_FRAMES_BITS) - 1;

   TRANSPORT_FIFO_SIZE_FRAME_DATA_MASK : constant :=
     (2 ** TRANSPORT_FIFO_SIZE_FRAME_DATA_BITS) - 1;

   function ON_WIRE_SIZE (P : Unsigned_16) return Unsigned_16
   is (P + 11);

   Payload_Ring_Buffer :
     Storage_Array (0 .. TRANSPORT_FIFO_MAX_FRAME_DATA - 1);
   Now                 : Unsigned_32 := 0;

   procedure Stuffed_TX_Byte
     (Self : in out Min_Context; Byte : Storage_Element; CRC : Boolean) is
   begin
      --  Transmit the byte
      TX_Byte (Self.Port, Byte);

      if CRC then
         Step (Self.Tx_Checksum, Byte);
      end if;

      --  See if an additional stuff byte is needed
      if Byte = HEADER_BYTE then
         Self.Tx_Header_Byte_Countdown := Self.Tx_Header_Byte_Countdown - 1;
         if Self.Tx_Header_Byte_Countdown = 0 then
            TX_Byte (Self.Port, STUFF_BYTE); -- Stuff byte
            Self.Tx_Header_Byte_Countdown := 2;
         end if;
      else
         Self.Tx_Header_Byte_Countdown := 2;
      end if;
   end Stuffed_TX_Byte;

   procedure On_Wire_Bytes
     (Self       : in out Min_Context;
      Id_Control : Storage_Element;
      Seq        : Storage_Element;
      P          : Payload)
   is
      Checksum : Unsigned_32;
   begin
      Self.Tx_Header_Byte_Countdown := 2;
      Init (Self.Tx_Checksum);

      TX_Start (Self.Port);

      --  Header is 3 bytes; because unstuffed will reset receiver immediately
      TX_Byte (Self.Port, HEADER_BYTE);
      TX_Byte (Self.Port, HEADER_BYTE);
      TX_Byte (Self.Port, HEADER_BYTE);

      Stuffed_TX_Byte (Self, Id_Control, True);
      if (Id_Control and 16#80#) /= 0 then
         --  Send the sequence number if it is a transport frame
         Stuffed_TX_Byte (Self, Seq, True);
      end if;

      Stuffed_TX_Byte (Self, P'Length, True);

      for Elt of P loop
         Stuffed_TX_Byte (Self, Elt, True);
      end loop;

      Checksum := Finalize (Self.Tx_Checksum);

      --  Network order is big-endian. A decent compiler will spot that this
      --  is extracting bytes and will use efficient instructions.
      Stuffed_TX_Byte
        (Self, Storage_Element (Shift_Right (Checksum, 24) and 16#FF#), False);
      Stuffed_TX_Byte
        (Self, Storage_Element (Shift_Right (Checksum, 16) and 16#FF#), False);
      Stuffed_TX_Byte
        (Self, Storage_Element (Shift_Right (Checksum, 8) and 16#FF#), False);
      Stuffed_TX_Byte
        (Self, Storage_Element (Shift_Right (Checksum, 0) and 16#FF#), False);

      --  Ensure end-of-frame doesn't contain 0xaa and confuse search for
      --  start-of-frame.
      TX_Byte (Self.Port, EOF_BYTE);

      TX_Finished (Self.Port);
   end On_Wire_Bytes;

   procedure Transport_FIFO_Pop (Self : in out Min_Context) is
   begin
      pragma Assert (Self.T_FIFO.N_Frames /= 0);
      declare
         Idx   : constant Transport_Frame_Index := Self.T_FIFO.Head_Idx;
         Frame : Transport_Frame renames Self.T_FIFO.Frames (Idx);
      begin
         pragma
           Assert
             (Self.T_FIFO.N_Ring_Buffer_Bytes
                >= Unsigned_16 (Frame.Payload_Len));

         Self.T_FIFO.N_Frames := Self.T_FIFO.N_Frames - 1;
         Self.T_FIFO.Head_Idx := Self.T_FIFO.Head_Idx + 1;
         --  MOD TYPES!!! Self.T_FIFO.Head_Idx := Self.T_FIFO.Head_Idx and
         --  TRANSPORT_FIFO_SIZE_FRAMES_MASK;
         Self.T_FIFO.N_Ring_Buffer_Bytes :=
           Self.T_FIFO.N_Ring_Buffer_Bytes - Unsigned_16 (Frame.Payload_Len);
      end;
   end Transport_FIFO_Pop;

   function Transport_FIFO_Push
     (Self      : in out Min_Context;
      Data_Size : Unsigned_16;
      Success   : out Boolean) return Transport_Frame_Index
   is
      FIFO    : Transport_FIFO renames Self.T_FIFO;
      Ret_Idx : Transport_Frame_Index := 0;
   begin
      Success := False;

      if FIFO.N_Frames < TRANSPORT_FIFO_MAX_FRAMES then
         --  Is there space in the ring buffer for the frame payload?
         if FIFO.N_Ring_Buffer_Bytes
           <= TRANSPORT_FIFO_MAX_FRAME_DATA - Data_Size
         then
            FIFO.N_Frames := FIFO.N_Frames + 1;
            if FIFO.N_Frames > FIFO.N_Frames_Max then
               --  High-water mark of FIFO (for diagnostic purposes)
               FIFO.N_Frames_Max := FIFO.N_Frames;
            end if;

            --  Create FIFO entry
            Ret_Idx := FIFO.Tail_Idx;
            Success := True;
            FIFO.Frames (Ret_Idx).Payload_Offset :=
              FIFO.Ring_Buffer_Tail_Offset;

            --  Claim ring buffer space
            FIFO.N_Ring_Buffer_Bytes := FIFO.N_Ring_Buffer_Bytes + Data_Size;
            if FIFO.N_Ring_Buffer_Bytes > FIFO.N_Ring_Buffer_Bytes_Max then
               --  High-water mark of ring buffer usage
               --  (for diagnostic purposes)
               FIFO.N_Ring_Buffer_Bytes_Max := FIFO.N_Ring_Buffer_Bytes;
            end if;

            --  FIXME: use Ada modulo type
            FIFO.Ring_Buffer_Tail_Offset :=
              FIFO.Ring_Buffer_Tail_Offset + Data_Size;
            FIFO.Ring_Buffer_Tail_Offset :=
              FIFO.Ring_Buffer_Tail_Offset
              and Unsigned_16 (TRANSPORT_FIFO_SIZE_FRAME_DATA_MASK);

            --  Claim FIFO space
            FIFO.Tail_Idx := FIFO.Tail_Idx + 1;
            --  MOD TYPES!!! FIFO.Tail_Idx := FIFO.Tail_Idx and
            --    TRANSPORT_FIFO_SIZE_FRAMES_MASK;

         else
            null; -- No FIFO payload space
         end if;

      else
         null; -- No FIFO frame slots
      end if;

      return Ret_Idx;
   end Transport_FIFO_Push;

   function Transport_FIFO_Get
     (Self : Min_Context; N : Storage_Element) return Transport_Frame_Index is
   begin
      pragma
        Compile_Time_Error
          (Integer (Unsigned_8'Last) < Integer (Transport_Frame_Index'Last),
           "");
      return Self.T_FIFO.Head_Idx + Transport_Frame_Index (N);
   end Transport_FIFO_Get;

   function Find_Retransmit_Frame
     (Self : in out Min_Context) return Transport_Frame_Index
   is
      FIFO                    : Transport_FIFO renames Self.T_FIFO;
      Idx, Oldest_Frame       : Transport_Frame_Index;
      Oldest_Elapsed, Elapsed : Unsigned_32;
      Window_Size             : constant Storage_Element :=
        FIFO.Sn_Max - FIFO.Sn_Min;
   begin

      pragma Assert (Window_Size > 0);
      pragma Assert (Window_Size < FIFO.N_Frames);

      --  Start with the head of the queue and call this the oldest
      Oldest_Frame := FIFO.Head_Idx;
      Oldest_Elapsed := Now - FIFO.Frames (Oldest_Frame).Last_Sent_Time_Ms;

      Idx := FIFO.Head_Idx;

      for I in 0 .. Window_Size - 1 loop
         Elapsed := Now - FIFO.Frames (Idx).Last_Sent_Time_Ms;
         if Elapsed > Oldest_Elapsed then
            Oldest_Elapsed := Elapsed;
            Oldest_Frame := Idx;
         end if;
         Idx := Idx + 1;
      end loop;

      return Oldest_Frame;
   end Find_Retransmit_Frame;

   procedure Transport_FIFO_Send
     (Self : in out Min_Context; Frame : in out Transport_Frame) is
   begin
      Debug
        ("FIFO Send Offset:"
         & Frame.Payload_Offset'Img
         & " Len:"
         & Frame.Payload_Len'Img);

      On_Wire_Bytes
        (Self,
         Frame.Min_ID or 16#80#,
         Frame.Seq,
         Payload_Ring_Buffer
           (Storage_Offset (Frame.Payload_Offset)
            .. Storage_Offset
                 (Frame.Payload_Offset + Unsigned_16 (Frame.Payload_Len)
                  - 1)));

      Frame.Last_Sent_Time_Ms := Now;
   end Transport_FIFO_Send;

   procedure Send_ACK (Self : in out Min_Context) is
   begin
      --  In the embedded end we don't reassemble out-of-order frames and
      --  so never ask for retransmits. Payload is
      --  always the same as the sequence number.

      Debug ("Send Ack: seq=" & Self.T_FIFO.Rn'Img);
      if ON_WIRE_SIZE (0) <= TX_Space (Self.Port) then
         On_Wire_Bytes (Self, ACK, Self.T_FIFO.Rn, (0 => Self.T_FIFO.Rn));
         Self.T_FIFO.Last_Sent_Ack_Time_Ms := Now;
      end if;
   end Send_ACK;

   procedure Send_Reset (Self : in out Min_Context) is
   begin
      if ON_WIRE_SIZE (0) <= TX_Space (Self.Port) then
         On_Wire_Bytes (Self, RESET, 0, (1 .. 0 => 0));
      end if;
   end Send_Reset;

   procedure Transport_FIFO_Reset (Self : in out Min_Context) is
   begin
      Self.T_FIFO.N_Frames := 0;
      Self.T_FIFO.Head_Idx := 0;
      Self.T_FIFO.Tail_Idx := 0;
      Self.T_FIFO.N_Ring_Buffer_Bytes := 0;
      Self.T_FIFO.Ring_Buffer_Tail_Offset := 0;
      Self.T_FIFO.Sn_Max := 0;
      Self.T_FIFO.Sn_Min := 0;
      Self.T_FIFO.Rn := 0;

      --  Reset the timers
      Self.T_FIFO.Last_Received_Anything_Ms := Now;
      Self.T_FIFO.Last_Sent_Ack_Time_Ms := Now;
      Self.T_FIFO.Last_Received_Frame_Ms := 0;

      Self.Rx_Frame_State := RX_State'First;
   end Transport_FIFO_Reset;

   function Queue_Frame
     (Self : in out Min_Context; Id : Unsigned_8; P : Payload) return Boolean
   is
      Success  : Boolean;
      Frame_Id : constant Transport_Frame_Index :=
        Transport_FIFO_Push (Self, P'Length, Success);
      Offset   : Unsigned_16;
   begin
      if Success then
         declare
            Frame : Transport_Frame renames Self.T_FIFO.Frames (Frame_Id);
         begin
            --  Copy frame details into frame slot, copy payload into ring
            --  buffer.
            Frame.Min_ID := Storage_Element (Id) and 16#3F#;
            Frame.Payload_Len := P'Length;
            Offset := Frame.Payload_Offset;
            for Elt of P loop
               Payload_Ring_Buffer (Storage_Offset (Offset)) := Elt;
               Offset := (Offset + 1) and TRANSPORT_FIFO_SIZE_FRAME_DATA_MASK;
            end loop;
            return True;
         end;
      else
         Self.T_FIFO.Dropped_Frames := Self.T_FIFO.Dropped_Frames + 1;
         return False;
      end if;
   end Queue_Frame;

   function Has_Space_For_Frame
     (Self : Min_Context; Payload_Len : Unsigned_8) return Boolean is
   begin
      return
        Self.T_FIFO.N_Frames < TRANSPORT_FIFO_MAX_FRAMES
        and then Self.T_FIFO.N_Ring_Buffer_Bytes
                 <= (TRANSPORT_FIFO_MAX_FRAME_DATA
                     - Unsigned_16 (Payload_Len));
   end Has_Space_For_Frame;

   procedure Send_Frame
     (Self : in out Min_Context; Id : Unsigned_8; P : Payload) is
   begin
      if ON_WIRE_SIZE (P'Length) <= TX_Space (Self.Port) then
         On_Wire_Bytes (Self, Storage_Element (Id) and 16#3F#, 0, P);
      end if;
   end Send_Frame;

   procedure Valid_Frame_Received (Self : in out Min_Context) is
      FIFO       : Transport_FIFO renames Self.T_FIFO;
      Id_Control : constant Storage_Element := Self.Rx_Frame_Id_Control;
      Id         : constant Unsigned_8 := Unsigned_8 (Id_Control and 16#3F#);
      Load_Len   : constant Storage_Element := Self.Rx_Control;
      Load       : constant Payload :=
        (if Load_Len > 0
         then Self.Rx_Frame_Payload_Buf (0 .. Payload_Index (Load_Len - 1))
         else (1 .. 0 => 0));
   begin
      if Transport_Enabled then
         declare
            Seq           : constant Storage_Element := Self.Rx_Frame_Seq;
            Num_Acked     : Storage_Element;
            Num_Nacked    : Storage_Element;
            Num_In_Window : Storage_Element;

            Idx : Transport_Frame_Index;
         begin
            FIFO.Last_Received_Anything_Ms := Now;

            case Id_Control is
               when ACK =>
                  --  If we get an ACK then we remove all the acknowledged
                  --  frames with seq < rn.
                  --  The payload byte specifies the number of NACKed frames:
                  --  how many we want retransmitted because they have gone
                  --  missing. But we need to make sure we don't accidentally
                  --  ACK too many because of a stale ACK from an old session
                  Num_Acked := Seq - FIFO.Sn_Min;
                  Num_Nacked := Load (Load'First) - Seq;
                  Num_In_Window := FIFO.Sn_Max - FIFO.Sn_Min;

                  if Num_Acked <= Num_In_Window then
                     FIFO.Sn_Min := Seq;
                     pragma Assert (FIFO.N_Frames >= Num_In_Window);
                     pragma
                       Assert (Num_In_Window <= TRANSPORT_MAX_WINDOW_SIZE);
                     pragma Assert (Num_Nacked <= TRANSPORT_MAX_WINDOW_SIZE);

                     --  Now pop off all the frames up to
                     --  (but not including) rn.
                     --  The ACK contains Rn; all frames before Rn are ACKed
                     --  and can be removed from the window.
                     Debug
                       ("Received ACK seq="
                        & Seq'Img
                        & " num_acked="
                        & Num_Acked'Img
                        & " num_nacked="
                        & Num_Nacked'Img);
                     if Num_Acked /= 0 then
                        for I in 0 .. Num_Acked - 1 loop
                           Transport_FIFO_Pop (Self);
                        end loop;
                     end if;

                     if Num_Nacked /= 0 then
                        Idx := FIFO.Head_Idx;
                        --  Now retransmit the number of frames that were
                        --  requested.
                        for I in 0 .. Num_Nacked - 1 loop
                           Transport_FIFO_Send (Self, FIFO.Frames (Idx));
                           Idx := Idx + 1;
                        end loop;
                     end if;
                  else
                     Debug ("Received spurious ACK seq=" & Seq'Img);
                     FIFO.Spurious_Acks := FIFO.Spurious_Acks + 1;
                  end if;

               when RESET =>
                  --  If we get a RESET demand then we reset the transport
                  --  protocol (empty the FIFO, reset the sequence numbers,
                  --  etc.) We don't send anything, we just do it. The other
                  --  end can send frames to see if this end is alive
                  --  (pings, etc.) or just wait to get application frames.
                  Debug ("Received reset");
                  FIFO.Resets_Received := FIFO.Resets_Received + 1;
                  Transport_FIFO_Reset (Self);

               when others =>
                  if (Id_Control and 16#80#) /= 0 then
                     --  Incoming application frames

                     --  Reset the activity time (an idle connection will be
                     --  stalled).
                     FIFO.Last_Received_Frame_Ms := Now;

                     if Seq = FIFO.Rn then
                        --  Accept this frame as matching the sequence number
                        --  we were looking for.

                        --  Now looking for the next one in the sequence
                        FIFO.Rn := FIFO.Rn + 1;

                        --  Always send an ACK back for the frame we received
                        --  ACKs are short (should be about 9 microseconds to
                        --  send on the wire) and this will cut the latency
                        --  down. We also periodically send an ACK in case
                        --  the ACK was lost, and in any case frames are
                        --  re-sent.
                        Send_ACK (Self);

                        --  Now ready to pass this up to the application
                        --  handlers.

                        --  Pass frame up to application handler to deal with
                        Debug
                          ("Incoming app transport frame seq="
                           & Seq'Img
                           & ", id="
                           & Id'Img
                           & ", payload len="
                           & Load_Len'Img);

                        Application_Handler (Id, Load, Self.Port);
                     else
                        --  Discard this frame because we aren't looking for
                        --  it: it's either a dupe because it was retransmitted
                        --  when our ACK didn't get through in time, or else
                        --  it's further on in the sequence and others got
                        --  dropped.
                        FIFO.Sequence_Mismatch_Drop :=
                          FIFO.Sequence_Mismatch_Drop + 1;
                        Debug
                          ("Received mismatched frame seq="
                           & Seq'Img
                           & ", looking for seq="
                           & FIFO.Rn'Img);
                     end if;
                  else
                     --  Not a transport frame
                     Debug
                       ("Incoming app frame id="
                        & Id'Img
                        & ", payload len="
                        & Load_Len'Img);
                     Application_Handler (Id, Load, Self.Port);
                  end if;
            end case;
         end;
      else
         Application_Handler (Id, Load, Self.Port);
      end if;
   end Valid_Frame_Received;

   procedure RX_Byte (Self : in out Min_Context; Byte : Storage_Element) is
      CRC : Unsigned_32;
   begin
      --  Debug ("RX_Byte:" & Byte'Img & " State " & Self.Rx_Frame_State'Img);

      --  Regardless of state, three header bytes means "start of frame" and
      --  should reset the frame buffer and be ready to receive frame data
      --
      --  Two in a row in over the frame means to expect a stuff byte.

      --  Debug ("Self.Rx_Header_Bytes_Seen:" & Self.Rx_Header_Bytes_Seen'Img);

      if Self.Rx_Header_Bytes_Seen = 2 then
         Self.Rx_Header_Bytes_Seen := 0;
         if Byte = HEADER_BYTE then
            Self.Rx_Frame_State := RECEIVING_ID_CONTROL;
            return;
         end if;
         if Byte = STUFF_BYTE then
            --  Discard this byte; carry on receiving on the next character
            return;
         else
            --  Something has gone wrong, give up on this frame and look for
            --  header again.
            Self.Rx_Frame_State := SEARCHING_FOR_SOF;
            return;
         end if;
      end if;

      if Byte = HEADER_BYTE then
         Self.Rx_Header_Bytes_Seen := Self.Rx_Header_Bytes_Seen + 1;
      else
         Self.Rx_Header_Bytes_Seen := 0;
      end if;

      case Self.Rx_Frame_State is
         when SEARCHING_FOR_SOF =>
            null;

         when RECEIVING_ID_CONTROL =>
            Self.Rx_Frame_Id_Control := Byte;
            Self.Rx_Frame_Payload_Bytes := 0;
            Init (Self.Rx_Checksum);
            Step (Self.Rx_Checksum, Byte);

            if (Byte and 16#80#) /= 0 then
               if Transport_Enabled then
                  Self.Rx_Frame_State := RECEIVING_SEQ;
               else
                  Self.Rx_Frame_State := SEARCHING_FOR_SOF;
               end if;

            else
               Self.Rx_Frame_Seq := 0;
               Self.Rx_Frame_State := RECEIVING_LENGTH;
            end if;

         when RECEIVING_SEQ =>
            Self.Rx_Frame_Seq := Byte;
            Step (Self.Rx_Checksum, Byte);
            Self.Rx_Frame_State := RECEIVING_LENGTH;

         when RECEIVING_LENGTH =>
            Self.Rx_Frame_Length := Byte;
            Self.Rx_Control := Byte;
            Step (Self.Rx_Checksum, Byte);
            if Self.Rx_Frame_Length > 0 then
               --  Can reduce the RAM size by compiling limits to frame sizes
               if Self.Rx_Frame_Length <= MAX_PAYLOAD then
                  Self.Rx_Frame_State := RECEIVING_PAYLOAD;
               else
                  --  Frame dropped because it's longer than any frame we can
                  --  buffer.
                  Debug
                    ("Dropping frame because length"
                     & Self.Rx_Frame_Length'Img
                     & " > MAX_PAYLOAD "
                     & MAX_PAYLOAD'Img);
                  Self.Rx_Frame_State := SEARCHING_FOR_SOF;
               end if;
            else
               Self.Rx_Frame_State := RECEIVING_CHECKSUM_3;
            end if;

         when RECEIVING_PAYLOAD =>
            Self.Rx_Frame_Payload_Buf (Self.Rx_Frame_Payload_Bytes) := Byte;
            Self.Rx_Frame_Payload_Bytes := Self.Rx_Frame_Payload_Bytes + 1;
            Step (Self.Rx_Checksum, Byte);
            Self.Rx_Frame_Length := Self.Rx_Frame_Length - 1;
            if Self.Rx_Frame_Length = 0 then
               Self.Rx_Frame_State := RECEIVING_CHECKSUM_3;
            end if;

         when RECEIVING_CHECKSUM_3 =>
            Self.Rx_Frame_Checksum := Shift_Left (Unsigned_32 (Byte), 24);
            Self.Rx_Frame_State := RECEIVING_CHECKSUM_2;

         when RECEIVING_CHECKSUM_2 =>
            Self.Rx_Frame_Checksum :=
              Self.Rx_Frame_Checksum or Shift_Left (Unsigned_32 (Byte), 16);
            Self.Rx_Frame_State := RECEIVING_CHECKSUM_1;

         when RECEIVING_CHECKSUM_1 =>
            Self.Rx_Frame_Checksum :=
              Self.Rx_Frame_Checksum or Shift_Left (Unsigned_32 (Byte), 8);
            Self.Rx_Frame_State := RECEIVING_CHECKSUM_0;

         when RECEIVING_CHECKSUM_0 =>
            Self.Rx_Frame_Checksum :=
              Self.Rx_Frame_Checksum or Shift_Left (Unsigned_32 (Byte), 0);

            CRC := Finalize (Self.Rx_Checksum);

            if Self.Rx_Frame_Checksum /= CRC then
               Debug
                 ("Checksum failed, received "
                  & Self.Rx_Frame_Checksum'Img
                  & ", computed "
                  & CRC'Img);
               --  Frame fails the checksum and so is dropped
               Self.Rx_Frame_State := SEARCHING_FOR_SOF;
            else
               --  Checksum passes, go on to check for the end-of-frame marker
               Self.Rx_Frame_State := RECEIVING_EOF;
            end if;

         when RECEIVING_EOF =>
            if Byte = 16#55# then
               --  Frame received OK, pass up data to handler
               Valid_Frame_Received (Self);
            else
               --  else discard
               Debug ("Received invalid EOF" & Byte'Img);
            end if;
            --  Look for next frame
            Self.Rx_Frame_State := SEARCHING_FOR_SOF;
      end case;
   end RX_Byte;

   procedure Poll (Self : in out Min_Context; Buffer : Storage_Array) is
      FIFO : Transport_FIFO renames Self.T_FIFO;
   begin
      for Elt of Buffer loop
         RX_Byte (Self, Elt);
      end loop;

      Now := Time_Ms;
      declare
         Remote_Connected : constant Boolean :=
           (Now - Self.T_FIFO.Last_Received_Anything_Ms)
           < TRANSPORT_IDLE_TIMEOUT_MS;
         Remote_Active    : constant Boolean :=
           (Now - Self.T_FIFO.Last_Received_Frame_Ms)
           < TRANSPORT_IDLE_TIMEOUT_MS;

         Window_Size : Storage_Element;
      begin

         --  This sends one new frame or resends one old frame
         Window_Size := FIFO.Sn_Max - FIFO.Sn_Min;
         if Window_Size < TRANSPORT_MAX_WINDOW_SIZE
           and then FIFO.N_Frames > Window_Size
         then
            --  There are new frames we can send; but don't even bother if
            --  there's no buffer space for them.
            declare
               Frame : Transport_Frame
                 renames Self.T_FIFO.Frames
                           (Transport_FIFO_Get (Self, Window_Size));
            begin
               if ON_WIRE_SIZE (Unsigned_16 (Frame.Payload_Len))
                 <= TX_Space (Self.Port)
               then
                  Frame.Seq := FIFO.Sn_Max;
                  Transport_FIFO_Send (Self, Frame);

                  --  Move window on
                  FIFO.Sn_Max := FIFO.Sn_Max + 1;
               end if;
            end;
         else
            --  Sender cannot send new frames so resend old ones
            --  (if there's anyone there)
            if Window_Size > 0 and then Remote_Connected then
               --  There are unacknowledged frames. Can re-send an old frame.
               --  Pick the least recently sent one.
               declare
                  Oldest_Frame : Transport_Frame
                    renames Self.T_FIFO.Frames (Find_Retransmit_Frame (Self));
               begin
                  if (Now - Oldest_Frame.Last_Sent_Time_Ms)
                    >= TRANSPORT_FRAME_RETRANSMIT_TIMEOUT_MS
                  then
                     --  Resending oldest frame if there's a chance there's
                     --  enough space to send it.
                     if ON_WIRE_SIZE (Unsigned_16 (Oldest_Frame.Payload_Len))
                       <= TX_Space (Self.Port)
                     then
                        Debug ("Resending oldest frame (" &
                                 Oldest_Frame.Min_ID'Img & ")");
                        Transport_FIFO_Send (Self, Oldest_Frame);
                     end if;
                  end if;
               end;
            end if;
         end if;

         --  Periodically transmit the ACK with the rn value, unless the line
         --  has gone idle.
         if (Now - FIFO.Last_Sent_Ack_Time_Ms)
           > TRANSPORT_ACK_RETRANSMIT_TIMEOUT_MS
         then
            if Remote_Active then
               Send_ACK (Self);
            end if;
         end if;
      end;
   end Poll;

   procedure Init (Self : in out Min_Context; P : Unsigned_8) is
   begin
      Self.Port := P;
      Self.Rx_Header_Bytes_Seen := 0;
      Self.Rx_Frame_State := SEARCHING_FOR_SOF;

      Self.T_FIFO.Spurious_Acks := 0;
      Self.T_FIFO.Sequence_Mismatch_Drop := 0;
      Self.T_FIFO.Dropped_Frames := 0;
      Self.T_FIFO.Resets_Received := 0;
      Self.T_FIFO.N_Ring_Buffer_Bytes_Max := 0;
      Self.T_FIFO.N_Frames_Max := 0;
      Transport_FIFO_Reset (Self);
   end Init;

   procedure Transport_Reset
     (Self : in out Min_Context; Inform_Other_Side : Boolean) is
   begin
      if Inform_Other_Side then
         Send_Reset (Self);
      end if;

      --  Throw our frames away
      Transport_FIFO_Reset (Self);
   end Transport_Reset;

end MIN.Target;

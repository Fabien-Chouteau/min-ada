with Ada.Text_IO;

package body MIN.Host is

   HEADER_BYTE : constant := 16#aa#;
   STUFF_BYTE : constant := 16#55#;
   EOF_BYTE : constant := 16#55#;
   ACK : constant := 16#FF#;
   RESET : constant := 16#FE#;

   subtype Dispatch is Instance'Class;

   ------------
   -- To_Str --
   ------------

   function To_Str (Data : Storage_Array) return String is
      Ret : String (1 .. Data'Length * 3);
      Idx : Natural := Ret'First;

      To_HEX : constant String (1 .. 16) := "0123456789ABCDEF";
   begin
      for Elt of Data loop
         Ret (Idx) := To_HEX (Integer ((Elt / 2**4) mod 16 + 1));
         Ret (Idx + 1) := To_HEX (Integer (Elt mod 16 + 1));
         Ret (Idx + 2) := ' ';
         Idx := Idx + 3;
      end loop;
      return Ret;
   end To_Str;

   -----------
   -- Debug --
   -----------

   procedure Debug (Str : String) is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                            "Ada Host DBG: " & Str);
   end Debug;

   ----------
   -- Init --
   ----------

   procedure Init (This : in out Instance) is
   begin
      This.Transport_Reset;
   end Init;

   ---------------------
   -- Transport_Reset --
   ---------------------

   procedure Transport_Reset (This : in out Instance) is
   begin
      This.Send_Reset;
      This.Transport_FIFO_Reset;
      This.RX_Reset;
   end Transport_Reset;

   ----------------
   -- Send_Frame --
   ----------------

   procedure Send_Frame (This    : in out Instance;
                         Id      :        Unsigned_8;
                         Payload :        Storage_Array)
   is
   begin
      if Payload'Length > 255 then
         raise Program_Error;
      elsif Id > 63 then
         raise Program_Error;
      end if;

      declare
         Frame : constant MIN_Frame :=
           (Payload_Len    => Payload'Length,
            Payload        => Payload,
            Id             => Id,
            Seq            => 0,
            Transport      => False,
            Ack_Or_Reset   => False,
            Last_Sent_Time => ART.Clock);

         On_Wire_Bytes : constant Storage_Array := This.On_Wire_Bytes (Frame);
      begin
         Dispatch (This).Serial_Write (On_Wire_Bytes);
      end;
   end Send_Frame;

   -----------------
   -- Queue_Frame --
   -----------------

   procedure Queue_Frame (This    : in out Instance;
                          Id      :        Unsigned_8;
                          Payload :        Storage_Array)
   is
   begin
      if Payload'Length > 255 then
         raise Program_Error;
      elsif Id > 63 then
         raise Program_Error;
      end if;

      if Natural (This.Transport_FIFO.Length) < This.Transport_FIFO_Size
      then
         declare
            Frame : constant MIN_Frame :=
              (Payload_Len    => Payload'Length,
               Payload        => Payload,
               Id             => Id,
               Seq            => This.Sn_Max,
               Transport      => True,
               Ack_Or_Reset   => False,
               Last_Sent_Time => ART.Clock);
         begin
            This.Transport_FIFO.Append (Frame);
         end;
      else
         This.Dropped_Frames := This.Dropped_Frames + 1;
         raise Program_Error with "No space in transport FIFO Queue";
      end if;
   end Queue_Frame;

   ----------
   -- Poll --
   ----------

   function Poll (This : in out Instance) return MIN_Frame_Lists.List is
      use ART;
      Now : constant ART.Time := ART.Clock;

      Remote_Connected : constant Boolean :=
        (Now - This.Last_Received_Anything_Time) < This.Idle_Timeout;

      Remote_Active : constant Boolean :=
        (Now - This.Last_Received_Frame_Time) < This.Idle_Timeout;

      Window_Size : Unsigned_8;
   begin

      This.RX_Bytes (Dispatch (This).Serial_Read_All);

      Window_Size := This.Sn_Max - This.Sn_Min;

      if Window_Size < This.Max_Window_Size
        and then
          Natural (This.Transport_FIFO.Length) > Natural (Window_Size)
      then
         declare
            Frame : constant MIN_Frame_Lists.Reference_Type :=
              This.Transport_FIFO_Get;
         begin
            --  Frames Still to Send
            Frame.Seq := This.Sn_Max;
            This.Last_Sent_Frame_Time := ART.Clock;
            Frame.Last_Sent_Time := ART.Clock;
            Debug ("Sending new frame " &
                     "id=" & Frame.Id'Img & " " &
                     " seq=" & Frame.Seq'Img);

            This.Transport_FIFO_Send (Frame);
            This.Sn_Max := This.Sn_Max + 1;
         end;
      else
         --  Maybe retransmits
         if Window_Size > 0 and then Remote_Connected then
            declare
               Oldest_Frame : constant MIN_Frame_Lists.Reference_Type :=
                 This.Find_Oldest_Frame;
            begin
               if (Now - Oldest_Frame.Last_Sent_Time)
                 > This.Frame_Retransmit_Timeout
               then
                  Debug ("Resending old frame id=" & Oldest_Frame.Id'Img &
                           " seq=" & Oldest_Frame.Seq'Img);
                  This.Transport_FIFO_Send (Oldest_Frame);
               end if;
            end;
         end if;
      end if;

      --  Periodically transmit ACK
      if (Now - This.Last_Sent_Ack_Time) > This.Ack_Retrasmit_Timeout then
         if Remote_Active then
            Debug ("Periodic send of ACK");
            This.Send_Ack;
         end if;
      end if;

      return Res : MIN_Frame_Lists.List do
         Res.Move (This.RX_List);
      end return;
   end Poll;

   --------------------------
   -- Transport_FIFO_Reset --
   --------------------------

   procedure Transport_FIFO_Reset (This : in out Instance) is
      Now : constant ART.Time := ART.Clock;
   begin
      This.Transport_FIFO.Clear;

      This.Last_Received_Anything_Time := Now;
      This.Last_Sent_Ack_Time := Now;
      This.Last_Sent_Frame_Time := Now;
      This.Last_Received_Frame_Time := Now;
      This.Sn_Min := 0;
      This.Sn_Max := 0;
      This.Rn := 0;
   end Transport_FIFO_Reset;

   ------------------------
   -- Transport_FIFO_Pop --
   ------------------------

   procedure Transport_FIFO_Pop (This : in out Instance) is
   begin
      This.Transport_FIFO.Delete_First;
   end Transport_FIFO_Pop;

   ------------------------
   -- Transport_FIFO_Get --
   ------------------------

   function Transport_FIFO_Get (This : aliased in out Instance)
                                return MIN_Frame_Lists.Reference_Type
   is
   begin
      return This.Transport_FIFO.Reference (This.Transport_FIFO.Last);
   end Transport_FIFO_Get;

   -------------------------
   -- Transport_FIFO_Send --
   -------------------------

   procedure Transport_FIFO_Send
     (This  : in out Instance;
      Frame :        MIN_Frame_Lists.Reference_Type)
   is
      Now : constant ART.Time := ART.Clock;
      On_Wire_Bytes : constant Storage_Array := This.On_Wire_Bytes (Frame);
   begin
      Frame.Last_Sent_Time := Now;
      This.Last_Sent_Frame_Time := Now;
      Dispatch (This).Serial_Write (On_Wire_Bytes);
   end Transport_FIFO_Send;

   --------------
   -- Send_Ack --
   --------------

   procedure Send_Ack (This : in out Instance) is
      Ack_Frame : constant MIN_Frame :=
        (Payload_Len  => 1,
         Payload      => (1 => Storage_Element (This.Rn)),
         Id           => ACK,
         Seq          => This.Rn,
         Transport    => True,
         Ack_Or_Reset => True,
         others => <>);

      On_Wire_Bytes : constant Storage_Array := This.On_Wire_Bytes (Ack_Frame);
   begin
      Dispatch (This).Serial_Write (On_Wire_Bytes);
   end Send_Ack;

   ---------------
   -- Send_Nack --
   ---------------

   procedure Send_Nack (This : in out Instance; To : Unsigned_8) is
      Nack_Frame : constant MIN_Frame :=
        (Payload_Len  => 1,
         Payload      => (1 => Storage_Element (To)),
         Id           => ACK,
         Seq          => This.Rn,
         Transport    => True,
         Ack_Or_Reset => True,
         others => <>);

      On_Wire_Bytes : constant Storage_Array :=
        This.On_Wire_Bytes (Nack_Frame);
   begin
      Dispatch (This).Serial_Write (On_Wire_Bytes);
   end Send_Nack;

   ----------------
   -- Send_Reset --
   ----------------

   procedure Send_Reset (This : in out Instance) is
      Reset_Frame : constant MIN_Frame :=
        (Payload_Len  => 0,
         Payload      => (1 .. 0 => 0),
         Id           => RESET,
         Seq          => 0,
         Transport    => True,
         Ack_Or_Reset => True,
         others => <>);

      On_Wire_Bytes : constant Storage_Array :=
        This.On_Wire_Bytes (Reset_Frame);
   begin
      Dispatch (This).Serial_Write (On_Wire_Bytes);
   end Send_Reset;

   --------------
   -- RX_Reset --
   --------------

   procedure RX_Reset (This : in out Instance) is
   begin
      This.Stashed_RX_Dict.Clear;
      --  TODO: This.Rx_List.Clear;
   end RX_Reset;

   --------------------
   -- Frame_Received --
   --------------------

   procedure Frame_Received (This    : in out Instance;
                             Id      :        Unsigned_8;
                             Payload :        Storage_Array;
                             Min_Seq :        Unsigned_8)
   is
      Now : constant ART.Time := ART.Clock;
   begin

      This.Last_Received_Anything_Time := Now;

      if (Id and 16#80#) /= 0 then
         case Id is
            when ACK =>
               Debug ("Receveid Ack");

               --  The ACK number indicates the serial number of the next
               --  packet wanted, so any previous packets can be marked off.
               declare
                  Number_Acked : constant Integer :=
                    (Integer (Min_Seq) - Integer (This.Sn_Min)) mod 256;
                  Number_In_Window : constant Integer :=
                    (Integer (This.Sn_Max) - Integer (This.Sn_Min)) mod 256;

                  New_Number_In_Window : Integer;
               begin
                  --  Need to guard against old ACKs from an old session still
                  --  turning up. Number acked will be 1 if there are no frames
                  --  in the window.
                  if Number_Acked < Number_In_Window then
                     Debug ("Number ACKed =" & Number_Acked'Img);
                     This.Sn_Min := Min_Seq;

                     pragma Assert
                       (Integer (This.Transport_FIFO.Length) >=
                            Number_In_Window);
                     pragma Assert
                       (Number_In_Window <= Integer (This.Max_Window_Size));

                     New_Number_In_Window :=
                       (Integer (This.Sn_Max) - Integer (This.Sn_Min)) mod 256;

                     if New_Number_In_Window + Number_Acked /= Number_In_Window
                     then
                        raise Program_Error;
                     end if;

                     for X in 1 .. Number_Acked loop
                        This.Transport_FIFO_Pop;
                     end loop;
                  else
                     if Number_In_Window > 0 then -- FIXME: Always true??
                        Debug ("Spurious ACK: Sn_Min=" & This.Sn_Min'Img &
                                 " Sn_Max=" & This.Sn_Max'Img &
                                 " Min_Seq=" & Min_Seq'Img &
                                 " Payload[0]=" & Payload (Payload'First)'Img);
                        This.Spurious_Acks := This.Spurious_Acks + 1;
                     end if;
                  end if;
               end;

            when RESET =>
               Debug ("RESET received" & Min_Seq'Img);

               This.resets_received := This.resets_received + 1;
               This.Transport_FIFO_Reset;
               This.RX_Reset;

            when others =>
               --  Min frame received
               declare
                  Frame : constant MIN_Frame :=
                    (Payload_Len => Payload'Length,
                     Payload     => Payload,
                     Id          => Id,
                     Seq         => Min_Seq,
                     Transport   => True,
                     others => <>);
               begin
                  This.Last_Received_Frame_Time := Now;

                  if Min_Seq = This.Rn then
                     Debug ("MIN application frame received " &
                              "(min_id=" & Unsigned_8'Image (Id and 16#3F#) &
                              " seq=" & Min_Seq'Img &
                              ")");

                     This.RX_List.Append (Frame);

                     --  We want this frame. Now see if there are stashed
                     --  frames it joins up with and 'receive' those.
                     This.Rn := This.Rn + 1;

                     while This.Stashed_RX_Dict.Contains (This.Rn) loop
                        declare
                           Stashed_Frame : constant MIN_Frame :=
                             This.Stashed_RX_Dict.Element (This.Rn);
                        begin
                           Debug ("MIN application stashed frame recovered" &
                                    "(rn=" & This.Rn'Img &
                                    " min_id=" & Stashed_Frame.Id'Img &
                                    " seq=" & Stashed_Frame.Seq'Img & "");
                           This.Stashed_RX_Dict.Delete (This.Rn);
                           This.RX_List.Append (Stashed_Frame);
                           This.Rn := This.Rn + 1;

                           if This.Nack_Outstanding.Valid
                             and then
                               This.Rn = This.Nack_Outstanding.Value
                           then
                              --  The missing frames we asked for have joined
                              --  up with the main sequence.
                              This.Nack_Outstanding := (Valid => False);
                           end if;
                        end;
                     end loop;

                     --  If there are stashed frames left then it means that
                     --  the stashed ones have missing frames in the sequence.
                     if not This.Nack_Outstanding.Valid
                       and then
                         not This.Stashed_RX_Dict.Is_Empty
                     then
                        --  We can send a NACK to ask for those too, starting
                        --  with the earliest sequence number.
                        Debug ("Stale frames in the stashed area; resetting");
                        declare
                           Earliest_Seq : constant Unsigned_8 :=
                             Earliest_Seq_Id (This.Stashed_RX_Dict);
                        begin

                           --  Check it's within the window size from us
                           if Earliest_Seq - This.Rn < This.RX_Window_Size then
                              This.Nack_Outstanding := (True, Earliest_Seq);
                              This.Send_Nack (Earliest_Seq);
                           else
                              --  Something has gone wrong here: stale stuff is
                              --  hanging around, give up and reset.
                              Debug
                                ("Stale frames in the stashed area;" &
                                   " resetting");
                              This.Nack_Outstanding := (Valid => False);
                              This.Stashed_RX_Dict.Clear;
                              This.Send_Ack;
                           end if;
                        end;
                     else
                        This.Send_Ack;
                        Debug ("Sending ACK for min ID=" &
                                 Unsigned_8'Image (Id and 16#3F#) &
                                 " with Rn=" & This.Rn'Img);
                     end if;

                  else
                    --  If the frames come within the window size in the future
                    --  sequence range then we accept them and assume some were
                    --  missing (They may also be duplicates, in which case we
                    --  store them over the top of the old ones)

                     if (Min_Seq - This.Rn) < This.RX_Window_Size then
                        --  We want to only NACK a range of frames once,
                        --  not each time otherwise we will overload with
                        --  retransmissions.

                        if not This.Nack_Outstanding.Valid then
                           --  If we are missing specific frames then send a
                           --  NACK to specifically request them.
                           Debug ("Sending NACK for min ID=" &
                                    Unsigned_8'Image (Id and 16#3F#) &
                                    " with seq=" & This.Rn'Img &
                                    " to=" & Min_Seq'Img);

                           This.Send_Nack (Min_Seq);
                           This.Nack_Outstanding := (True, Min_Seq);
                        else
                           Debug ("(Outstanding NACK)");
                        end if;

                        --  Hang on to this frame because we will join it up
                        --  later with the missing ones that are re-sent
                        This.Stashed_RX_Dict.Insert (Min_Seq, Frame);
                        Debug
                          ("MIN application frame stashed" &
                             "(min_id=" & Unsigned_8'Image (Id and 16#3F#) &
                             " seq=" & Min_Seq'Img & ")");

                     else
                        Debug
                          ("Frame stale? Discarding" &
                             "(min_id=" & Unsigned_8'Image (Id and 16#3F#) &
                             " seq=" & Min_Seq'Img & ")");
                        if This.Stashed_RX_Dict.Contains (Min_Seq)
                          and then
                            Payload /=
                              This.Stashed_RX_Dict.Element (Min_Seq).Payload
                        then
                           Debug ("Inconsistency between frame constants");
                        end if;

                        --  Out of range (may be an old retransmit duplicate
                        --  that we don't want) - throw it away
                        This.sequence_mismatch_drops :=
                          This.sequence_mismatch_drops + 1;
                     end if;
                  end if;
               end;
         end case;
      else
         declare
            Frame : constant MIN_Frame := (Payload_Len => Payload'Length,
                                           Payload     => Payload,
                                           Id          => Id,
                                           Seq         => 0,
                                           Transport   => False,
                                           others => <>);
         begin
            This.RX_List.Append (Frame);
         end;
      end if;
   end Frame_Received;

   --------------
   -- RX_Bytes --
   --------------

   procedure RX_Bytes (This : in out Instance;
                       Data :        Storage_Array)
   is
      Compute_Checksum : Unsigned_32;
      Byte : Unsigned_8;
   begin
      Debug ("Recevied bytes: " & To_Str (Data));
      for Elt of Data loop
         Byte := Unsigned_8 (Elt);

         if This.rx_header_bytes_seen = 2 then
            This.rx_header_bytes_seen := 0;
            case Byte is
               when HEADER_BYTE =>
                  This.rx_frame_state := RECEIVING_ID_CONTROL;
               when STUFF_BYTE =>
                  --  Discard this byte; carry on receiving the next character
                  null;
               when others =>
                  --  By here something must have gone wrong, give up on this
                  --  frame and look for new header.
                  This.rx_frame_state := SEARCHING_FOR_SOF;
            end case;
         else
            if Byte = HEADER_BYTE then
               This.rx_header_bytes_seen := This.rx_header_bytes_seen + 1;
            else
               This.rx_header_bytes_seen := 0;
            end if;

            case This.rx_frame_state is
               when SEARCHING_FOR_SOF =>
                  null;

               when RECEIVING_ID_CONTROL =>
                  This.rx_frame_id_control := Byte;

                  This.rx_frame_state := (if (Byte and 16#80#) /= 0
                                          then RECEIVING_SEQ
                                          else RECEIVING_LENGTH);

               when RECEIVING_SEQ =>
                  This.rx_frame_seq := Byte;
                  This.rx_frame_state := RECEIVING_LENGTH;

               when RECEIVING_LENGTH =>
                  This.rx_frame_length := Byte;
                  This.rx_control := Byte;
                  This.rx_frame_buf.Clear;

                  This.rx_frame_state := (if This.rx_frame_length /= 0
                                          then RECEIVING_PAYLOAD
                                          else RECEIVING_CHECKSUM_3);

               when RECEIVING_PAYLOAD =>
                  This.rx_frame_buf.Append (Elt);
                  This.rx_frame_length := This.rx_frame_length - 1;
                  if This.rx_frame_length = 0 then
                     This.rx_frame_state := RECEIVING_CHECKSUM_3;
                  end if;

               when RECEIVING_CHECKSUM_3 =>
                  This.rx_frame_checksum :=
                    Shift_Left (Unsigned_32 (Byte), 24);
                  This.rx_frame_state := RECEIVING_CHECKSUM_2;

               when RECEIVING_CHECKSUM_2 =>
                  This.rx_frame_checksum :=
                    This.rx_frame_checksum or
                    Shift_Left (Unsigned_32 (Byte), 16);
                  This.rx_frame_state := RECEIVING_CHECKSUM_1;

               when RECEIVING_CHECKSUM_1 =>
                  This.rx_frame_checksum :=
                    This.rx_frame_checksum or
                    Shift_Left (Unsigned_32 (Byte), 8);
                  This.rx_frame_state := RECEIVING_CHECKSUM_0;

               when RECEIVING_CHECKSUM_0 =>
                  This.rx_frame_checksum :=
                    This.rx_frame_checksum or Unsigned_32 (Byte);

                  if (This.rx_frame_id_control and 16#80#) /= 0 then
                     Compute_Checksum :=
                       CRC32 ((1 => Storage_Element (This.rx_frame_id_control),
                               2 => Storage_Element (This.rx_frame_seq),
                               3 => Storage_Element (This.rx_control)) &
                                To_Array (This.rx_frame_buf));
                  else
                     Compute_Checksum :=
                       CRC32 ((1 => Storage_Element (This.rx_frame_id_control),
                               2 => Storage_Element (This.rx_control)) &
                                To_Array (This.rx_frame_buf));
                  end if;

                  if This.rx_frame_checksum /= Compute_Checksum then
                     Debug ("CRC mismatch (" & This.rx_frame_checksum'Img &
                              " vs" & Compute_Checksum'Img &
                              "), frame dropped");
                     --  Frame fails checksum, is dropped
                     This.rx_frame_state := SEARCHING_FOR_SOF;
                  else
                     --  Checksum passes, wait for EOF
                     This.rx_frame_state := RECEIVING_EOF;
                  end if;

               when RECEIVING_EOF =>

                  if Byte = EOF_BYTE then
                     --  Frame received OK, pass up frame for handling
                     This.Frame_Received
                       (Id      => This.rx_frame_id_control,
                        Payload => To_Array (This.rx_frame_buf),
                        Min_Seq => This.rx_frame_seq);
                  else
                     Debug ("No EOF received, dropping frame");
                  end if;

                  --  Look for next frame
                  This.rx_frame_state := SEARCHING_FOR_SOF;
            end case;
         end if;
      end loop;
   end RX_Bytes;

   -----------------------
   -- Find_Oldest_Frame --
   -----------------------

   function Find_Oldest_Frame (This : in out Instance)
                               return MIN_Frame_Lists.Reference_Type
   is
      use ART;
      Now : constant Time := Clock;
      Elapsed : Time_Span;
      Longest_Elapsed : Time_Span := Time_Span_First;
      Oldest_Frame : constant MIN_Frame_Lists.Reference_Type :=
        This.Transport_FIFO.Reference (This.Transport_FIFO.First);
   begin
      if This.Transport_FIFO.Is_Empty then
         raise Program_Error;
      end if;

      for C in This.Transport_FIFO.Iterate loop
         Elapsed := Now - This.Transport_FIFO.Reference (C).Last_Sent_Time;
         if Elapsed >= Longest_Elapsed then
            Oldest_Frame := This.Transport_FIFO.Reference (C);
            Longest_Elapsed := Elapsed;
         end if;
      end loop;

      return Oldest_Frame;
   end Find_Oldest_Frame;

   ------------------
   -- CRC_To_Bytes --
   ------------------

   function CRC_To_Bytes (CRC : Unsigned_32) return Storage_Array
   is ((1 => Storage_Element (Shift_Right (CRC, 24) and 16#FF#),
        2 => Storage_Element (Shift_Right (CRC, 16) and 16#FF#),
        3 => Storage_Element (Shift_Right (CRC, 8) and 16#FF#),
        4 => Storage_Element (Shift_Right (CRC, 0) and 16#FF#)));

   -------------------
   -- On_Wire_Bytes --
   -------------------

   function On_Wire_Bytes (This  : in out Instance;
                           Frame :        MIN_Frame)
                           return Storage_Array
   is
      Id : constant Unsigned_8 := (if Frame.Transport
                                   then Frame.Id or 16#80#
                                   else Frame.Id);

      Prolog : constant Storage_Array :=
        (1 => Storage_Element (Id),
         2 => Storage_Element (Frame.Seq),
         3 => Storage_Element (Frame.Payload_Len)
        ) & Frame.Payload;

      CRC : constant Unsigned_32 := CRC32 (Prolog);

      Raw : constant Storage_Array :=
        Prolog & CRC_To_Bytes (CRC);

      Stuffed : Storage_Vectors.Vector;
      Count : Natural := 0;
   begin

      Stuffed.Append (HEADER_BYTE);
      Stuffed.Append (HEADER_BYTE);
      Stuffed.Append (HEADER_BYTE);

      for Elt of Raw loop
         Stuffed.Append (Elt);
         if Elt = HEADER_BYTE then
            Count := Count + 1;
            if Count = 2 then
               Stuffed.Append (STUFF_BYTE);
               Count := 0;
            end if;
         else
            Count := 0;
         end if;
      end loop;

      --  Back to an array
      return To_Array (Stuffed);
   end On_Wire_Bytes;

   -----------
   -- CRC32 --
   -----------

   function CRC32 (Data  : Storage_Array;
                   Start : Unsigned_32 := 16#FFFF_FFFF#)
                   return Unsigned_32
   is
      CRC : CRC_Context;
   begin
      Init (CRC, Start);
      for Elt of Data loop
         Step (CRC, Elt);
      end loop;
      return Finalize (CRC);
   end CRC32;

   ---------------------
   -- Earliest_Seq_Id --
   ---------------------

   function Earliest_Seq_Id (Map : MIN_Frame_Maps.Map) return Unsigned_8 is
      Res : Unsigned_8 := Unsigned_8'Last;
   begin
      for C in Map.Iterate loop
         if MIN_Frame_Maps.Key (C) < Res then
            Res := MIN_Frame_Maps.Key (C);
         end if;
      end loop;
      return Res;
   end Earliest_Seq_Id;

   --------------
   -- To_Array --
   --------------

   function To_Array (Vect : Storage_Vectors.Vector) return Storage_Array is

      Result : Storage_Array (1 .. Storage_Offset (Vect.Length));
      Idx : Storage_Offset := Result'First;
   begin
      for Elt of Vect loop
         Result (Idx) := Elt;
         Idx := Idx + 1;
      end loop;
      return Result;
   end To_Array;

end MIN.Host;

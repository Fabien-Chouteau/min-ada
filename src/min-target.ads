with Interfaces;              use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

generic
   with
     procedure Application_Handler
       (Id : Unsigned_8; P : Storage_Array; Port : Unsigned_8);
   with function Time_Ms return Unsigned_32;
   with function TX_Space (Port : Unsigned_8) return Unsigned_16;
   with procedure TX_Byte (Port : Unsigned_8; Byte : Storage_Element);
   with procedure TX_Start (Port : Unsigned_8);
   with procedure TX_Finished (Port : Unsigned_8);
package MIN.Target is

   Transport_Enabled : constant Boolean := True;

   MAX_PAYLOAD : constant := 255;
   TRANSPORT_FIFO_SIZE_FRAMES_BITS : constant := 4;
   TRANSPORT_FIFO_SIZE_FRAME_DATA_BITS : constant := 10;

   TRANSPORT_FIFO_MAX_FRAMES : constant :=
     2 ** TRANSPORT_FIFO_SIZE_FRAMES_BITS;
   TRANSPORT_FIFO_MAX_FRAME_DATA : constant :=
     2 ** TRANSPORT_FIFO_SIZE_FRAME_DATA_BITS;

   pragma
     Compile_Time_Error
       (TRANSPORT_FIFO_MAX_FRAMES > 256,
        "Transport FIFO frames cannot exceed 256");

   pragma
     Compile_Time_Error
       (TRANSPORT_FIFO_MAX_FRAME_DATA > 65_536,
        "Transport FIFO data allocated cannot exceed 64Kbytes");

   type Transport_Frame is record
      Last_Sent_Time_Ms : Unsigned_32;
      --  When frame was last sent (used for re-send timeouts)

      Payload_Offset : Unsigned_16;
      --  Where in the ring buffer the payload is

      Payload_Len : Unsigned_8;
      --  How big the payload is

      Min_ID : Storage_Element;
      --  ID of frame

      Seq : Storage_Element;
      --  Sequence number of frame
   end record;

   type Transport_Frame_Index is mod TRANSPORT_FIFO_MAX_FRAMES;

   type Transport_Frame_Array is
     array (Transport_Frame_Index) of Transport_Frame;

   type Transport_FIFO is record
      Frames                    : Transport_Frame_Array;
      Last_Sent_Ack_Time_Ms     : Unsigned_32 := 0;
      Last_Received_Anything_Ms : Unsigned_32 := 0;
      Last_Received_Frame_Ms    : Unsigned_32 := 0;
      Dropped_Frames            : Unsigned_32 := 0; -- Diagnostic Counters
      Spurious_Acks             : Unsigned_32 := 0;
      Sequence_Mismatch_Drop    : Unsigned_32 := 0;
      Resets_Received           : Unsigned_32 := 0;
      N_Ring_Buffer_Bytes       : Unsigned_16 :=
        0; -- Number Of Bytes Used In The Payload Ring Buffer
      N_Ring_Buffer_Bytes_Max   : Unsigned_16 :=
        0; -- Largest Number Of Bytes Ever Used
      Ring_Buffer_Tail_Offset   : Unsigned_16 :=
        0; -- Tail Of The Payload Ring Buffer
      N_Frames                  : Storage_Element :=
        0;  -- Number Of Frames In The FIFO
      N_Frames_Max              : Storage_Element :=
        0;  -- Larger Number Of Frames In The FIFO
      Head_Idx                  : Transport_Frame_Index :=
        0;  -- Where Frames Are Taken From In The FIFO
      Tail_Idx                  : Transport_Frame_Index :=
        0;  -- Where New Frames Are Added
      Sn_Min                    : Storage_Element :=
        0;  -- Sequence Numbers For Transport Protocol
      Sn_Max                    : Storage_Element := 0;
      Rn                        : Storage_Element := 0;
   end record;

   subtype Payload_Index is Storage_Offset range 0 .. MAX_PAYLOAD - 1;
   subtype Payload is Storage_Array;

   type RX_State is
     (SEARCHING_FOR_SOF,
      RECEIVING_ID_CONTROL,
      RECEIVING_SEQ,
      RECEIVING_LENGTH,
      RECEIVING_PAYLOAD,
      RECEIVING_CHECKSUM_3,
      RECEIVING_CHECKSUM_2,
      RECEIVING_CHECKSUM_1,
      RECEIVING_CHECKSUM_0,
      RECEIVING_EOF);

   type Min_Context is record
      T_FIFO                   :
        Transport_FIFO;               -- T-MIN queue of outgoing frames
      Rx_Frame_Payload_Buf     :
        Payload
          (Payload_Index'First
           .. Payload_Index'Last);        -- Payload Received So Far
      Rx_Frame_Checksum        : Unsigned_32 :=
        0;       -- Checksum Received Over The Wire
      Rx_Checksum              :
        CRC_Context;             -- Calculated Checksum For Receiving Frame
      Tx_Checksum              :
        CRC_Context;             -- Calculated Checksum For Sending Frame
      Rx_Header_Bytes_Seen     : Unsigned_8 :=
        0;     -- Countdown Of Header Bytes To Reset State
      Rx_Frame_State           : RX_State :=
        SEARCHING_FOR_SOF;           -- State Of Receiver
      Rx_Frame_Payload_Bytes   : Storage_Offset :=
        0;   -- Length Of Payload Received So Far
      Rx_Frame_Id_Control      : Storage_Element :=
        0;      -- ID And Control Bit Of Frame Being Received
      Rx_Frame_Seq             : Storage_Element :=
        0;             -- Sequence Number Of Frame Being Received
      Rx_Frame_Length          : Storage_Element :=
        0;          -- Length Of Frame
      Rx_Control               : Storage_Element :=
        0;               -- Control Byte
      Tx_Header_Byte_Countdown : Unsigned_8 := 0; -- Count Out The Header Bytes
      Port                     : Unsigned_8 :=
        0;                     -- Number of the port associated with the context
   end record;

   procedure Init (Self : in out Min_Context; P : Unsigned_8);

   function Queue_Frame
     (Self : in out Min_Context; Id : Unsigned_8; P : Payload) return Boolean;

   function Has_Space_For_Frame
     (Self : Min_Context; Payload_Len : Unsigned_8) return Boolean;

   procedure Send_Frame
     (Self : in out Min_Context; Id : Unsigned_8; P : Payload);

   procedure Poll (Self : in out Min_Context; Buffer : Storage_Array);

   procedure Transport_Reset
     (Self : in out Min_Context; Inform_Other_Side : Boolean);

end MIN.Target;

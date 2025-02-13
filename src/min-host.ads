with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

with Ada.Real_Time;

with GNAT.Serial_Communications;

package MIN.Host is

   package ART renames Ada.Real_Time;

   type MIN_Frame (Payload_Len : Storage_Count) is record
      Payload : Storage_Array (1 .. Payload_Len);
      Id, Seq : Unsigned_8 := 0;
      Transport : Boolean := False;
      Ack_Or_Reset : Boolean := False;
      Last_Sent_Time : ART.Time := ART.Time_First;
   end record;

   package MIN_Frame_Lists
   is new Ada.Containers.Indefinite_Doubly_Linked_Lists (MIN_Frame);

   package MIN_Frame_Maps
   is new Ada.Containers.Indefinite_Ordered_Maps (Unsigned_8, MIN_Frame);

   package Storage_Vectors
   is new Ada.Containers.Vectors (Storage_Count, Storage_Element);

   function To_Array (Vect : Storage_Vectors.Vector) return Storage_Array;

   type U8_Option (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Value : Unsigned_8 := 0;
         when False =>
            null;
      end case;
   end record;

   type Instance is abstract tagged limited record
      Transport_FIFO_Size : Natural := 100;
      Ack_Retrasmit_Timeout : ART.Time_Span := ART.Milliseconds (25);
      Max_Window_Size : Unsigned_8 := 8;
      Idle_Timeout : ART.Time_Span := ART.Milliseconds (3000);
      Frame_Retransmit_Timeout : ART.Time_Span := ART.Milliseconds (50);
      RX_Window_Size : Unsigned_8 := 16;

      --  Stats about the link
      longest_transport_fifo : Natural := 0;
      Dropped_Frames : Natural := 0;
      Spurious_Acks : Natural := 0;
      mismatched_acks : Natural := 0;
      duplicate_frames : Natural := 0;
      retransmitted_frames : Natural := 0;
      resets_received : Natural := 0;
      sequence_mismatch_drops : Natural := 0;

      --  State of transport FIFO
      Transport_FIFO : MIN_Frame_Lists.List;
      Last_Sent_Ack_Time : ART.Time := ART.Clock;
      Last_Received_Anything_Time : ART.Time := ART.Clock;
      Last_Received_Frame_Time : ART.Time := ART.Clock;
      Last_Sent_Frame_Time : ART.Time := ART.Clock;

      --  State for receiving a MIN frame
      rx_frame_buf : Storage_Vectors.Vector;
      rx_header_bytes_seen : Natural := 0;
      rx_frame_state : RX_State := SEARCHING_FOR_SOF;
      rx_frame_checksum : Unsigned_32 := 0;
      rx_frame_id_control : Unsigned_8 := 0;
      rx_frame_seq : Unsigned_8 := 0;
      rx_frame_length : Unsigned_8 := 0;
      rx_control : Unsigned_8 := 0;
      --  accepted_min_frames = []
      RX_List : MIN_Frame_Lists.List;
      Stashed_RX_Dict : MIN_Frame_Maps.Map;

      --  Sequence numbers
      Rn : Unsigned_8 := 0;
      --  Sequence Number expected to be received next

      Sn_Min : Unsigned_8 := 0;
      --  Sequence Number of the first frame currently in the sending window

      Sn_Max : Unsigned_8 := 0;
      --  Next sequence number to use for sending a frame

      Nack_Outstanding : U8_Option := (Valid => False);
      --  NACK status
   end record;

   procedure Serial_Write (This : in out Instance; Data : Storage_Array)
   is abstract;

   function Serial_Any (This : in out Instance) return Boolean
   is abstract;

   function Serial_Read_All (This : in out Instance) return Storage_Array
   is abstract;

   procedure Serial_Close (This : in out Instance)
   is abstract;

   procedure Init (This : in out Instance);

   procedure Transport_Reset (This : in out Instance);

   procedure Send_Frame (This    : in out Instance;
                         Id      :        Unsigned_8;
                         Payload :        Storage_Array);

   procedure Queue_Frame (This    : in out Instance;
                          Id      :        Unsigned_8;
                          Payload :        Storage_Array);

   function Poll (This : in out Instance) return MIN_Frame_Lists.List;

private

   procedure Transport_FIFO_Reset (This : in out Instance);
   procedure Transport_FIFO_Pop (This : in out Instance);
   function Transport_FIFO_Get (This : aliased in out Instance)
                                return MIN_Frame_Lists.Reference_Type;
   procedure Transport_FIFO_Send
     (This  : in out Instance;
      Frame :        MIN_Frame_Lists.Reference_Type);

   procedure Send_Ack (This : in out Instance);
   procedure Send_Nack (This : in out Instance; To : Unsigned_8);
   procedure Send_Reset (This : in out Instance);
   procedure RX_Reset (This : in out Instance);

   procedure Frame_Received (This    : in out Instance;
                             Id      :        Unsigned_8;
                             Payload :        Storage_Array;
                             Min_Seq :        Unsigned_8);
   procedure RX_Bytes (This : in out Instance;
                       Data :        Storage_Array);

   function Find_Oldest_Frame (This : in out Instance)
                               return MIN_Frame_Lists.Reference_Type;

   function On_Wire_Bytes (This  : in out Instance;
                           Frame :        MIN_Frame)
                           return Storage_Array;

   function CRC32 (Data  : Storage_Array;
                   Start : Unsigned_32 := 16#FFFF_FFFF#)
                   return Unsigned_32;

   function Earliest_Seq_Id (Map : MIN_Frame_Maps.Map) return Unsigned_8
     with Pre => not Map.Is_Empty;

end MIN.Host;

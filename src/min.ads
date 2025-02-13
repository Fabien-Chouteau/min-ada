with Interfaces;              use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

package MIN is

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


   --  TODO: Move to private part
   type CRC_Context is record
      CRC : Unsigned_32;
   end record;
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

private

   procedure Init (Self : in out CRC_Context;
                   Start : Unsigned_32 := 16#FFFF_FFFF#);
   procedure Step (Self : in out CRC_Context; Byte : Storage_Element);

   function Finalize (Self : CRC_Context) return Unsigned_32;

   HEADER_BYTE : constant := 16#aa#;
   STUFF_BYTE : constant := 16#55#;
   EOF_BYTE : constant := 16#55#;
   ACK : constant := 16#FF#;
   RESET : constant := 16#FE#;

   subtype Payload_Index is Storage_Offset range 0 .. MAX_PAYLOAD - 1;
   subtype Payload is Storage_Array;


end MIN;

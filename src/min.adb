package body MIN is

   ----------
   -- Init --
   ----------

   procedure Init (Self : in out CRC_Context;
                   Start : Unsigned_32 := 16#FFFF_FFFF#)
   is
   begin
      Self.CRC := Start;
      --  Debug_CRC ("CRC Init " & Self.CRC'Img);
   end Init;

   ----------
   -- Step --
   ----------

   procedure Step (Self : in out CRC_Context; Byte : Storage_Element) is
      Mask : Unsigned_32;
   begin
      --  Debug_CRC ("CRC " & Self.CRC'Img & " Add" & Byte'Img);
      Self.CRC := Self.CRC xor Unsigned_32 (Byte);
      for J in 0 .. 7 loop
         Mask := -(Self.CRC and 1);
         Self.CRC := Shift_Right (Self.CRC, 1) xor (16#EDB8_8320# and Mask);
      end loop;
      --  Debug_CRC ("CRC after" & Self.CRC'Img);
   end Step;

   --------------
   -- Finalize --
   --------------

   function Finalize (Self : CRC_Context) return Unsigned_32 is
   begin
      --  Debug_CRC ("CRC Finalize" & Unsigned_32'Image (not Self.CRC));
      return not Self.CRC;
   end Finalize;

end MIN;

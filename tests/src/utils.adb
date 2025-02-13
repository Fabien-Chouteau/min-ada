with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time;

package body Utils is

   Start_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

   -------------
   -- Time_MS --
   -------------

   function Time_MS return Interfaces.Unsigned_32 is
      use Ada.Real_Time;

      Now  : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      Span : constant Ada.Real_Time.Time_Span := Now - Start_Time;
   begin
      return Unsigned_32 (To_Duration (Span) * 1000.0);
   end Time_MS;

   -------------
   -- Corrupt --
   -------------

   function Corrupt (This : in out Corrupter;
                     Data : Storage_Element)
                     return Storage_Element
   is
      use Bit_Index_Rand;
      use Ada.Numerics.Float_Random;

      Out_Data : Storage_Element;
   begin
      if Random (This.F_Rand) < This.Proba then
         Out_Data := Data xor
           Storage_Element (Shift_Left (Unsigned_8 (1), Random (This.B_Rand)));
         Put_Line (Standard_Error,
                   This.Prefix & " !!!! Inserting corruption " &
                     Out_Data'Img & " was" &  Data'Img & " !!!!");
         return Out_Data;
      else
         return Data;
      end if;
   end Corrupt;

   -------------
   -- Corrupt --
   -------------

   function Corrupt (This : in out Corrupter;
                     Data : Storage_Array)
                     return Storage_Array
   is
      Ret : Storage_Array (Data'Range);
   begin
      for Idx in Data'Range loop
         Ret (Idx) := This.Corrupt (Data (Idx));
      end loop;
      return Ret;
   end Corrupt;

   -----------
   -- Count --
   -----------

   function Count (This : Corrupter) return Natural
   is (This.Count);

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Corrupter) is
   begin
      This.Count := 0;
      Bit_Index_Rand.Reset (This.B_Rand);
      Ada.Numerics.Float_Random.Reset (This.F_Rand);
   end Reset;

   ---------------------
   -- Set_Probability --
   ---------------------

   procedure Set_Probability (This : in out Corrupter;
                              Proba : Float)
   is
   begin
      This.Proba := Proba;
   end Set_Probability;

   ------------
   -- Create --
   ------------

   function Create (DBG_Prefix : String := "") return Corrupter
   is
   begin
      return (Len => DBG_Prefix'Length, Prefix => DBG_Prefix, others => <>);
   end Create;

   ------------------
   -- Send_To_Host --
   ------------------

   procedure Send_To_Host (This : in out Test_Host; Data : Storage_Array) is
   begin
      for Elt of Data loop
         This.To_Host.Append (Elt);
      end loop;
   end Send_To_Host;

   -------------------
   -- Get_From_Host --
   -------------------

   function Get_From_Host (This : in out Test_Host) return Storage_Array is
      Ret : constant Storage_Array := MIN.Host.To_Array (This.From_Host);
   begin
      This.From_Host.Clear;
      return Ret;
   end Get_From_Host;

   ------------------
   -- Serial_Write --
   ------------------

   overriding
   procedure Serial_Write (This : in out Test_Host; Data : Storage_Array) is
   begin
      for Elt of Data loop
         This.From_Host.Append (Elt);
      end loop;
   end Serial_Write;

   ----------------
   -- Serial_Any --
   ----------------

   overriding
   function Serial_Any (This : in out Test_Host) return Boolean is
   begin
      return not This.To_Host.Is_Empty;
   end Serial_Any;

   ---------------------
   -- Serial_Read_All --
   ---------------------

   overriding
   function Serial_Read_All (This : in out Test_Host) return Storage_Array is
   begin
      return MIN.Host.To_Array (This.To_Host);
   end Serial_Read_All;

   ------------------
   -- Serial_Close --
   ------------------

   overriding
   procedure Serial_Close (This : in out Test_Host) is
   begin
      null;
   end Serial_Close;

end Utils;

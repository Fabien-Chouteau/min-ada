with System.Storage_Elements; use System.Storage_Elements;
with Interfaces;              use Interfaces;
with MIN.Target;
with MIN.Host;

with Ada.Text_IO;              use Ada.Text_IO;
with System;

with Min_C_Binding;
with Utils; use Utils;

procedure Tests is

   Ada_Corrupt : Corrupter := Create ("MIN Ada DBG");

   procedure Debug (Str : String) is
   begin
      Put_Line (Standard_Error, "MIN Ada DBG: " & Str);
   end Debug;

   function Time_Ms return Unsigned_32 is
   begin
      return Utils.Time_MS;
   end Time_Ms;

   procedure Application_Handler
     (Id : Unsigned_8; P : Storage_Array; Port : Unsigned_8) is
   begin
      Debug ("Prod Got Frame" & Id'Img);
   end Application_Handler;

   function TX_Space (Port : Unsigned_8) return Unsigned_16 is
   begin
      return 256;
   end TX_Space;

   procedure TX_Byte (Port : Unsigned_8; Byte : Storage_Element) is
   begin
      Debug ("TX_Byte" & Port'Img & Byte'Img);
      Min_C_Binding.Send_To_Client ((0 => Ada_Corrupt.Corrupt (Byte)));
   end TX_Byte;

   procedure TX_Start (Port : Unsigned_8) is
   begin
      --  Debug ("TX_Start" & Port'Img);
      null;
   end TX_Start;

   procedure TX_Finished (Port : Unsigned_8) is
   begin
      --  Debug ("TX_Finished" & Port'Img);
      null;
   end TX_Finished;

   package MIN_Prod is new
     MIN.Target
       (Application_Handler => Application_Handler,
        Time_Ms             => Time_Ms,
        TX_Space            => TX_Space,
        TX_Byte             => TX_Byte,
        TX_Start            => TX_Start,
        TX_Finished         => TX_Finished);

   Prod : MIN_Prod.Min_Context;
   Byte : Storage_Element;

   Unused : Boolean;

   Hst : Utils.Test_Host;

begin

   Hst.Queue_Frame (42, (0 .. 4 => 41));

   loop
      declare
         Data : constant Storage_Array := Hst.Get_From_Host;
         Frames : constant MIN.Host.MIN_Frame_Lists.List := Hst.Poll;
      begin
         MIN_Prod.Poll (Prod, Data);
         for Elt of Data loop
            Put_Line (Elt'Img);
         end loop;
         Put_Line ("Frame count:" & Frames.Length'Img);
      end;
   end loop;
   return;

   Ada_Corrupt.Set_Probability (0.05);

   MIN_Prod.Init (Prod, 0);
   Unused := MIN_Prod.Queue_Frame (Prod, 1, (0 .. 4 => 41));
   Unused := MIN_Prod.Queue_Frame (Prod, 2, (0 .. 4 => 42));
   Unused := MIN_Prod.Queue_Frame (Prod, 3, (0 .. 4 => 43));

   Unused := Min_C_Binding.Queue_Frame (1, (0 .. 4 => 21));
   Unused := Min_C_Binding.Queue_Frame (2, (0 .. 4 => 22));
   Unused := Min_C_Binding.Queue_Frame (3, (0 .. 4 => 23));
   loop
      --  Debug
      --    ("Loop (To_Prod" & To_Prod.Extent'Img & ") (To_Cons" &
      --     To_Cons.Extent'Img & ")");

      MIN_Prod.Poll (Prod, Min_C_Binding.Get_From_Client);
      Min_C_Binding.Poll;
   end loop;

   --  MIN_TX.Transport_Reset (T, False);
   --  MIN_RX.Transport_Reset (R, False);
   --
   --  Unused := MIN_TX.Queue_Frame (T, 2, (0 .. 4 => 42));
   --
   --  MIN_TX.Poll (T, (1 .. 0 => 0));
   --  MIN_RX.Poll (R, Pipe_Buffer (0 .. Pipe_Index - 1));
   delay 10.0;
end Tests;

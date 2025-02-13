with System.Storage_Elements; use System.Storage_Elements;
with Interfaces;              use Interfaces;
with MIN.Target;

with Ada.Streams;              use Ada.Streams;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with Ada.Command_Line;         use Ada.Command_Line;
with GNAT.Sockets;             use GNAT.Sockets;

with System;

procedure Tests_Client is

   Socket       : Socket_Type;
   Addr         : Sock_Addr_Type;
   Channel_Sock : GNAT.Sockets.Stream_Access;
   Channel_Out  : Text_Streams.Stream_Access;
   Buffer       : Stream_Element_Array (1 .. 1);
   Last         : Stream_Element_Count;

   procedure Debug (Str : String) is
   begin
      Put_Line (Standard_Error, Str);
   end Debug;

   function Time_Ms return Unsigned_32 is
   begin
      return 0;
   end Time_Ms;

   procedure Application_Handler
     (Id : Unsigned_8; P : Storage_Array; Port : Unsigned_8) is
   begin
      Debug ("Prod Got Frame" & Id'Img);
   end Application_Handler;

   function TX_Space (Port : Unsigned_8) return Unsigned_16 is
   begin
      return 100;
   end TX_Space;

   procedure TX_Byte (Port : Unsigned_8; Byte : Storage_Element) is
   begin
      Put_Line ("TX ->" &  Byte'Img);
      Storage_Element'Write (Channel_Sock, Byte);
   end TX_Byte;

   procedure TX_Start (Port : Unsigned_8) is
   begin
      Debug ("TX_Start" & Port'Img);
   end TX_Start;

   procedure TX_Finished (Port : Unsigned_8) is
   begin
      Debug ("TX_Finished" & Port'Img);
   end TX_Finished;

   package MIN_Inst is new
     MIN.Target
       (Application_Handler => Application_Handler,
        Time_Ms             => Time_Ms,
        TX_Space            => TX_Space,
        TX_Byte             => TX_Byte,
        TX_Start            => TX_Start,
        TX_Finished         => TX_Finished);

   Ctx : MIN_Inst.Min_Context;

   Unused : Boolean;
begin

   if Argument_Count /= 2 then
      Put_Line (Current_Error, "Usage: " & Command_Name & " host port");
      Set_Exit_Status (1);
      return;
   end if;

   Create_Socket (Socket);
   Addr.Addr := Addresses (Get_Host_By_Name (Argument (1)), 1);
   Addr.Port := Port_Type'Value (Argument (2));
   Connect_Socket (Socket, Addr);

   Set_Socket_Option
     (Socket,
      Socket_Level,
      (Receive_Timeout,
       Timeout => 0.001));

   Channel_Sock := Stream (Socket);
   Channel_Out := Stream (Current_Output);

   MIN_Inst.Init (Ctx, 0);

   --  Unused := MIN_Inst.Queue_Frame (Ctx, 1, (0 .. 1 => 42));
   --  Unused := MIN_Inst.Queue_Frame (Ctx, 2, (0 .. 2 => 42));
   --  Unused := MIN_Inst.Queue_Frame (Ctx, 3, (0 .. 3 => 42));
   --  Unused := MIN_Inst.Queue_Frame (Ctx, 4, (0 .. 4 => 42));
   --  Unused := MIN_Inst.Queue_Frame (Ctx, 5, (0 .. 5 => 42));
   --  Unused := MIN_Inst.Queue_Frame (Ctx, 6, (0 .. 6 => 42));

   loop
      declare
      begin
         Read (Channel_Sock.all, Buffer, Last);
         exit when Last = 0;
         for Elt of Buffer (1 .. Last) loop
            MIN_Inst.Poll (Ctx, (0 .. 0 => Storage_Element (Elt)));
         end loop;
      exception
         when GNAT.Sockets.Socket_Error =>
            null;
      end;

      MIN_Inst.Poll (Ctx, (1 .. 0 => 0));
   end loop;

end Tests_Client;

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Utils; use Utils;

package body Min_C_Binding is

   Corruption_Enabled : constant Boolean := True;

   package Payload_Lists
   is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Received_Payload);

   package Byte_Lists
   is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Storage_Element);

   RX_Payloads : Payload_Lists.List;
   Incoming_Data, Outgoing_Data : Byte_Lists.List;

   C_Corrupt : Corrupter := Create ("MIN C DBG");

   --  Callbacks

   pragma Warnings (Off, "all instances*");
   procedure Application_Handler_Internal (Id   : Unsigned_8;
                                           Buf  : System.Address;
                                           Len  : Unsigned_8;
                                           Port : Unsigned_8);
   pragma Export (C, Application_Handler_Internal, "min_application_handler");

   function Time_MS return Interfaces.Unsigned_32;
   pragma Export (C, Time_MS, "min_time_ms");

   function TX_Space (Port : Unsigned_8) return Unsigned_16;
   pragma Export (C, TX_Space, "min_tx_space");

   procedure TX_Byte (Port, Byte : Unsigned_8);
   pragma Export (C, TX_Byte, "min_tx_byte");

   procedure TX_Start (Port : Unsigned_8);
   pragma Export (C, TX_Start, "min_tx_start");

   procedure TX_Finished (Port : Unsigned_8);
   pragma Export (C, TX_Finished, "min_tx_finished");

   pragma Warnings (On, "all instances*");

   -----------
   -- To_SA --
   -----------

   function To_SA (List : in out Byte_Lists.List) return Storage_Array is
      Data : Storage_Array (1 .. Storage_Offset (List.Length));
   begin
      for Elt of Data loop
         Elt := List.First_Element;
         List.Delete_First;
      end loop;
      return Data;
   end To_SA;

   -----------
   -- Debug --
   -----------

   procedure Debug (Str : String) is
   begin
      Put_Line (Standard_Error, "MIN C DBG " & Str);
   end Debug;

   ----------------------------------
   -- Application_Handler_Internal --
   ----------------------------------

   procedure Application_Handler_Internal (Id   : Unsigned_8;
                                           Buf  : System.Address;
                                           Len  : Unsigned_8;
                                           Port : Unsigned_8)
   is
      Payload : Storage_Array (1 .. Storage_Offset (Len))
        with Address => Buf;
   begin
      Debug ("C_MIN got payload Id:" & Id'Img & " Port:" & Port'Img &
               " Len:" & Len'Img);
      RX_Payloads.Append ((Len     => Payload'Length,
                           Id      => Id,
                           Port    => Port,
                           Payload => Payload));
   end Application_Handler_Internal;

   -------------
   -- Time_MS --
   -------------

   function Time_MS return Interfaces.Unsigned_32 is
   begin
      return Utils.Time_MS;
   end Time_MS;

   --------------
   -- TX_Space --
   --------------

   function TX_Space (Port : Unsigned_8) return Unsigned_16 is
      pragma Unreferenced (Port);
   begin
      return 1024;
   end TX_Space;

   -------------
   -- TX_Byte --
   -------------

   procedure TX_Byte (Port, Byte : Unsigned_8) is
      pragma Unreferenced (Port);

   begin
      Outgoing_Data.Append
        (if Corruption_Enabled
         then C_Corrupt.Corrupt (Storage_Element (Byte))
         else Storage_Element (Byte));
   end TX_Byte;

   --------------
   -- TX_Start --
   --------------

   procedure TX_Start (Port : Unsigned_8)
   is
   begin
      null;
   end TX_Start;

   -----------------
   -- TX_Finished --
   -----------------

   procedure TX_Finished (Port : Unsigned_8) is
   begin
      null;
   end TX_Finished;

   -----------------
   -- Queue_Frame --
   -----------------

   function Queue_Frame (Id : Unsigned_8;
                         P  : System.Storage_Elements.Storage_Array)
                         return Boolean
   is
   begin
      return Boolean (C_Queue_Frame (C_Get_Ctx, Id, P'Address, P'Length));
   end Queue_Frame;

   -------------------------
   -- Has_Space_For_Frame --
   -------------------------

   function Has_Space_For_Frame (Len : Unsigned_8) return Boolean is
   begin
      return Boolean (C_Queue_Has_Space_For_Frame (C_Get_Ctx, Len));
   end Has_Space_For_Frame;

   ----------
   -- Poll --
   ----------

   procedure Poll is
      Incoming : constant Storage_Array := To_SA (Incoming_Data);
   begin
      C_Poll (C_Get_Ctx, Incoming'Address, Incoming'Length);
   end Poll;

   ---------------------
   -- Transport_Reset --
   ---------------------

   procedure Transport_Reset (Inform_Other_Side : Boolean) is
   begin
      C_Transport_Reset (C_Get_Ctx, C_bool (Inform_Other_Side));
   end Transport_Reset;

   ---------------
   -- Send_Byte --
   ---------------

   procedure Send_To_Client (Data : Storage_Array) is
   begin
      for Elt of Data loop
         Incoming_Data.Append (Elt);
      end loop;
   end Send_To_Client;

   -----------------------
   -- Payload_Available --
   -----------------------

   function Payload_Available return Boolean is
   begin
      return not RX_Payloads.Is_Empty;
   end Payload_Available;

   --------------------
   -- Get_RX_Payload --
   --------------------

   function Get_RX_Payload return Received_Payload is
      Payload : constant Received_Payload := RX_Payloads.First_Element;
   begin
      RX_Payloads.Delete_First;
      return Payload;
   end Get_RX_Payload;

   -----------------
   -- Get_Tx_Data --
   -----------------

   function Get_From_Client return Storage_Array
   is (To_SA (Outgoing_Data));

end Min_C_Binding;

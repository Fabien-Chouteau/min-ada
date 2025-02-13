with System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;

package Min_C_Binding is

   function Queue_Frame (Id : Unsigned_8;
                         P  : Storage_Array)
                         return Boolean;

   function Has_Space_For_Frame (Len : Unsigned_8) return Boolean;

   procedure Poll;

   procedure Transport_Reset (Inform_Other_Side : Boolean);

   --  Test interfacing

   procedure Send_To_Client (Data : Storage_Array);

   type Received_Payload (Len : Storage_Count) is record
      Id, Port : Unsigned_8;
      Payload : Storage_Array (1 .. Len);
   end record;

   function Payload_Available return Boolean;
   function Get_RX_Payload return Received_Payload;

   function Get_From_Client return Storage_Array;

private

   type MIN_Context is new System.Address;

   function C_Get_Ctx return MIN_Context;
   pragma Import (C, C_Get_Ctx, "get_ctx");

   function C_Queue_Frame (Self    : MIN_Context;
                           Id      : Unsigned_8;
                           Payload : System.Address;
                           Len     : Unsigned_8)
                           return C_bool;
   pragma Import (C, C_Queue_Frame, "min_queue_frame");

   function C_Queue_Has_Space_For_Frame (Self    : MIN_Context;
                                         Len     : Unsigned_8)
                                         return C_bool;
   pragma Import (C,
                  C_Queue_Has_Space_For_Frame,
                  "min_queue_has_space_for_frame");

   procedure C_Poll (Self : MIN_Context;
                     Buf  : System.Address;
                     Len  : Unsigned_32);
   pragma Import (C, C_Poll, "min_poll");

   procedure C_Transport_Reset (Self              : MIN_Context;
                                Inform_Other_Side : C_bool);
   pragma Import (C, C_Transport_Reset, "min_transport_reset");

end Min_C_Binding;

with Ada.Text_IO; use Ada.Text_IO;

with System.Storage_Elements; use System.Storage_Elements;
with MIN.Host;

with Min_C_Binding;
with Utils;

procedure Test_Ada_Host_To_C_Client is

   Host : Utils.Test_Host;

begin

   loop
      declare
         Host_Frames : constant MIN.Host.MIN_Frame_Lists.List :=
           Host.Poll;

         Host_To_Client : constant Storage_Array :=
           Host.Get_From_Host;

         Client_To_Host : constant Storage_Array :=
           Min_C_Binding.Get_From_Client;
      begin
         Host.Send_To_Host (Client_To_Host);
         Min_C_Binding.Send_To_Client (Host_To_Client);
         Min_C_Binding.Poll;

         for Frame of Host_Frames loop
            Put_Line (Frame.Id'Img);
         end loop;

      end;
   end loop;
end Test_Ada_Host_To_C_Client;

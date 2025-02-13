with System.Storage_Elements; use System.Storage_Elements;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;
with Interfaces;
with MIN.Host;

package Utils is

   function Time_MS return Interfaces.Unsigned_32;

   --  Data corruption util

   type Corrupter (<>) is tagged limited private;

   function Corrupt (This : in out Corrupter;
                     Data : Storage_Element)
                     return Storage_Element;

   function Corrupt (This : in out Corrupter;
                     Data : Storage_Array)
                     return Storage_Array;

   function Count (This : Corrupter) return Natural;
   --  Return the number of bytes corrupted so far

   procedure Reset (This : in out Corrupter);
   --  Reset the corrupted count and RNG

   procedure Set_Probability (This : in out Corrupter;
                              Proba : Float);

   function Create (DBG_Prefix : String := "")
                    return Corrupter;

   --  Test Host
   type Test_Host is new MIN.Host.Instance with record
      To_Host : MIN.Host.Storage_Vectors.Vector;
      From_Host : MIN.Host.Storage_Vectors.Vector;
   end record;

   procedure Send_To_Host (This : in out Test_Host; Data : Storage_Array);
   function Get_From_Host (This : in out Test_Host) return Storage_Array;

   overriding
   procedure Serial_Write (This : in out Test_Host; Data : Storage_Array);

   overriding
   function Serial_Any (This : in out Test_Host) return Boolean;

   overriding
   function Serial_Read_All (This : in out Test_Host) return Storage_Array;

   overriding
   procedure Serial_Close (This : in out Test_Host);

private

   subtype Bit_Index is Natural range 0 .. 7;
   package Bit_Index_Rand is new Ada.Numerics.Discrete_Random (Bit_Index);

   type Corrupter (Len : Natural) is tagged limited record
      Prefix : String (1 .. Len) := (others => ' ');

      B_Rand : Bit_Index_Rand.Generator;
      F_Rand : Ada.Numerics.Float_Random.Generator;

      Proba : Float := 0.0005;
      Count : Natural := 0;
   end record;

end Utils;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors; 
with Ada.Containers.Hashed_Maps;

package Helpers is

   function Int_Image (Int : Integer) return String;

   ----------------------------------------------------------------------

   package Int_Vectors is new Vectors
     (Index_Type => Natural, Element_Type => Integer);
   subtype Int_Vector is Int_Vectors.Vector;

   package Int_Sorter is new Int_Vectors.Generic_Sorting;

   ----------------------------------------------------------------------

   function Integer_Hash (Key : Integer) return Ada.Containers.Hash_Type;

   package Integer_Hashed_Maps is new Hashed_Maps
     (Key_Type => Integer, Element_Type => Integer, Hash => Integer_Hash, Equivalent_Keys => "=");
   subtype Int_Hash is Integer_Hashed_Maps.Map;

   ----------------------------------------------------------------------

   procedure Skip_Whitespace (Line : String; Idx : in out Integer);

   function Char_to_Int(C : Character) return Integer;

end Helpers;

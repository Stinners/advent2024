with Ada.Strings;         use Ada.Strings;
with Ada.Strings.Fixed;   use Ada.Strings.Fixed;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Helpers is

   function Int_Image (Int : Integer) return String is
   begin
      return Trim (Int'Img, Left);
   end Int_Image;

   function Integer_Hash (Key : Integer) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Key);
   end Integer_Hash;

   --------------------------------------------------------------------------

   procedure Skip_Whitespace (Line : String; Idx : in out Integer) is
   begin
      while Idx <= Line'Last and then Line (Idx) in ' ' | ASCII.HT | ASCII.LF | ASCII.CR loop
         Idx := @ + 1;
      end loop;
   end Skip_Whitespace;

   --------------------------------------------------------------------------

end Helpers;

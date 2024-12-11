with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers;      use Ada.Containers;
with Ada.Containers.Hashed_Maps;

package body Day11 is

   function Hash_LL_Int (Key : Long_Long_Integer) return Hash_Type is (Hash_Type (Key));

   package Stone_Hash is new Hashed_Maps
     (Key_Type        => Long_Long_Integer, Element_Type => Long_Long_Integer, Hash => Hash_LL_Int,
      Equivalent_Keys => "=");
   subtype Stone_Line is Stone_Hash.Map;
   use Stone_Hash;

   -----------------------------------------------------------------

   function Parse_Input (File : File_Acc) return Stone_Line is
      Stones            : Stone_Vec;
      Line              : String (1 .. 100);
      Idx, Last, Number : Integer := 1;
      Long_Number       : Long_Long_Integer;
   begin
      Get_Line (File.all, Line, Last);

      while Idx < Last loop
         Get (Item => Number, From => Line (Idx .. Last), Last => Last);
         Long_Number := Long_Long_Integer (Number);

         -- TODO create the hashmap

         Idx := @ + 1;
      end loop;

      return Stones;
   end Parse_Input;

   -----------------------------------------------------------------

   function Blink (Stones : Stone_Vec) return Stone_Vec is
      Next_Stones : Stone_Vec;
      Half        : Integer;
   begin

      for Stone of Stones loop
         declare
            Stone_Img : constant String  := Stone'Img;
            Dig       : constant String  := Stone'Img (2 .. Stone_Img'Last);
            N_Digits  : constant Integer := Dig'Length;
         begin
            if Stone = 0 then
               Next_Stones.Append (1);
            elsif N_Digits mod 2 = 0 then
               Half := N_Digits / 2;
               Next_Stones.Append (Long_Long_Integer'Value (Dig (Dig'First + Half .. Dig'Last)));
               Next_Stones.Append (Long_Long_Integer'Value (Dig (Dig'First .. Dig'Last - Half)));
            else
               Next_Stones.Append (Stone * 2_024);
            end if;
         end;
      end loop;

      return Next_Stones;

   end Blink;

   -----------------------------------------------------------------

   function Solve (File : File_Acc) return Solution is
      Part1, Part2 : Integer   := 0;
      Stones       : Stone_Vec := Parse_Input (File);
   begin

      for I in 1 .. 25 loop
         Stones := Blink (Stones);
      end loop;

      Part1 := Integer (Stones.Length);

      return (Part1 => Part1, Part2 => Part2);
   end Solve;

end Day11;

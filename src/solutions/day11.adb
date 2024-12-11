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

   procedure Insert_Stones (Stones : in out Stone_Line; Number, Amount : Long_Long_Integer) is
   begin
      if Stones.Contains (Number) then
         Stones (Number) := Stones (Number) + Amount;
      else
         Stones.Insert (Number, Amount);
      end if;
   end Insert_Stones;

   -----------------------------------------------------------------

   function Parse_Input (File : File_Acc) return Stone_Line is
      Stones            : Stone_Line;
      Line              : String (1 .. 100);
      Idx, Last, Number : Integer := 1;
      Long_Number       : Long_Long_Integer;
   begin
      Get_Line (File.all, Line, Last);

      while Idx < Last loop
         Get (Item => Number, From => Line (Idx .. Last), Last => Last);
         Long_Number := Long_Long_Integer (Number);
         Insert_Stones (Stones, Long_Number, 1);
         Idx := @ + 1;
      end loop;

      return Stones;
   end Parse_Input;

   -----------------------------------------------------------------

   function Blink (Stones : Stone_Line) return Stone_Line is
      Next_Stones : Stone_Line;
      Half        : Integer;
   begin

      for Number of Stones loop
         declare
            Amount    : constant Long_Long_Integer := Stones (Number);
            Stone_Img : constant String            := Number'Img;
            Dig       : constant String            := Stone_Img (2 .. Stone_Img'Last);
            N_Digits  : constant Integer           := Dig'Length;
         begin
            if Number = 0 then
               Insert_Stones (Next_Stones, 1, Amount);
            elsif N_Digits mod 2 = 0 then
               Half := N_Digits / 2;
               Insert_Stones
                 (Next_Stones, Long_Long_Integer'Value (Dig (Dig'First + Half .. Dig'Last)),
                  Amount);
               Insert_Stones
                 (Next_Stones, Long_Long_Integer'Value (Dig (Dig'First .. Dig'Last - Half)),
                  Amount);
            else
               Insert_Stones (Next_Stones, Number * 2_024, Amount);
            end if;
         end;
      end loop;

      return Next_Stones;

   end Blink;

   -----------------------------------------------------------------

   function Solve (File : File_Acc) return Solution is
      Part1, Part2 : Long_Long_Integer := 0;
      Stones       : Stone_Line        := Parse_Input (File);
   begin

      for I in 1 .. 25 loop
         Stones := Blink (Stones);
      end loop;

      for Number of Stones loop
         Part1 := @ + Stones (Number);
      end loop;

      Part1 := Long_Long_Integer (Stones.Length);
      Put_Line (Part1'Img);

      return (Part1 => 0, Part2 => 0);
   end Solve;

end Day11;

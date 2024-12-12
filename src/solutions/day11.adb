with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers;      use Ada.Containers;
with Ada.Containers.Hashed_Maps;

package body Day11 is

   --  We need to do the mod 2**32, otherwise the key becomes too large for Hash_type to compute
   function Hash_LL_Int (Key : Long_Long_Integer) return Hash_Type is 
     (Hash_Type (Key mod 2**32));

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
      Idx, Last, Number : Integer := 0;
      Long_Number       : Long_Long_Integer;
   begin
      Get_Line (File.all, Line, Last);

      while Idx < Last loop
         Get (Item => Number, From => Line (Idx + 1 .. Last), Last => Idx);
         Long_Number := Long_Long_Integer (Number);
         Insert_Stones (Stones, Long_Number, 1);
      end loop;
    
      return Stones;
   end Parse_Input;

   -----------------------------------------------------------------

   function Blink (Stones : Stone_Line) return Stone_Line is
      Next_Stones : Stone_Line;
   begin
      for Stone in Stones.Iterate loop
         declare
            Number    : constant Long_Long_Integer := Key (Stone);
            Amount    : constant Long_Long_Integer := Element (Stone);
            Dig       : constant String            := Number'Img (2 .. Number'Img'Last);
            N_Digits  : constant Integer           := Dig'Length;
            Half      : constant Integer           := N_Digits / 2;
         begin
            if Number = 0 then
               Insert_Stones (Next_Stones, 1, Amount);
            elsif N_Digits mod 2 = 0 then
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
   
   function Blink_N_Times(Stones : in out Stone_Line; N : Integer) return Long_Long_Integer is 
       Sum : Long_Long_Integer := 0;
   begin 
       for I in 1 .. N loop 
          Stones := Blink (Stones);
       end loop;

       for Number of Stones loop
          Sum := @ + Number;
       end loop;

       return Sum;
   end Blink_N_Times;

   -----------------------------------------------------------------

   function Solve (File : File_Acc) return Solution is
      Part1, Part2 : Long_Long_Integer;
      Stones       : Stone_Line := Parse_Input (File);
   begin
      Part1 := Blink_N_Times(Stones, 25);
      Part2 := Blink_N_Times(Stones, 50);

      Put_Line ("Part1:" & Part1'Img);
      Put_Line ("Part2:" & Part2'Img);

      return (Part1 => 0, Part2 => 0);
   end Solve;

end Day11;

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Containers;      use Ada.Containers;
with Ada.Containers.Hashed_Maps;

package body Day11 is
    type U64 is range 0 .. 2**64-1;
    package U64_IO is new Ada.Text_IO.Integer_IO(U64);

   function Hash_U64 (Key : U64) return Hash_Type is 
     (Hash_Type (Key mod 2**32));

   package Stone_Hash is new Hashed_Maps
     (Key_Type        => U64, Element_Type => U64, Hash => Hash_U64,
      Equivalent_Keys => "=");
   subtype Stone_Line is Stone_Hash.Map;
   use Stone_Hash;

   -----------------------------------------------------------------

   procedure Insert_Stones (Stones : in out Stone_Line; Number, Amount : U64) is
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
      Long_Number       : U64;
   begin
      while not End_Of_File(File.all) loop
          U64_IO.Get(Item => Long_Number, File => File.all);
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
            Number    : constant U64     := Key (Stone);
            Amount    : constant U64     := Element (Stone);
            Dig       : constant String  := Number'Img (2 .. Number'Img'Last);
            N_Digits  : constant Integer := Dig'Length;
            Half      : constant Integer := N_Digits / 2;
         begin
            if Number = 0 then
               Insert_Stones (Next_Stones, 1, Amount);
            elsif N_Digits mod 2 = 0 then
               Insert_Stones
                 (Next_Stones, U64'Value (Dig (Dig'First + Half .. Dig'Last)), Amount);
               Insert_Stones
                 (Next_Stones, U64'Value (Dig (Dig'First .. Dig'Last - Half)), Amount);
            else
               Insert_Stones (Next_Stones, Number * 2_024, Amount);
            end if;
         end;
      end loop;

      return Next_Stones;
   end Blink;

   -----------------------------------------------------------------
   
   function Blink_N_Times(Stones : in out Stone_Line; N : Integer) return U64 is 
       Sum : U64 := 0;
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
      Part1, Part2 : U64;
      Stones       : Stone_Line := Parse_Input (File);
   begin
      Part1 := Blink_N_Times(Stones, 25);
      Part2 := Blink_N_Times(Stones, 50);

      Put_Line ("Part1:" & Part1'Img);
      Put_Line ("Part2:" & Part2'Img);

      return (Part1 => 0, Part2 => 0);
   end Solve;

end Day11;

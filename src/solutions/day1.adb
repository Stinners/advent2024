with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Helpers; use Helpers;
use Helpers.Integer_Hashed_Maps;

package body Day1 is

   --------------------------------------------------------------------------

   procedure Parse_Input (File : File_Acc; Left, Right : out Int_Vector) is
      Line         : String (1 .. 50);
      Number, Last : Integer;
      Idx          : Natural;
   begin
      while not End_Of_File (File.all) loop
         Get_Line (File.all, Line, Last);

         Get (Item => Number, From => Line (1 .. Line'Last), Last => Idx);
         Left.Append (Number);

         Get (Item => Number, From => Line (Idx + 2 .. Line'Last), Last => Idx);
         Right.Append (Number);
      end loop;
   end Parse_Input;

   --------------------------------------------------------------------------

   function Count_Occurrances (Nums : Int_Vector) return Int_Hash is
      Result : Int_Hash;
      Elem   : Cursor;
   begin

      for Num of Nums loop
         Elem := Result.Find (Num);
         if Elem /= No_Element then
            Result.Replace (Num, Element (Elem) + 1);
         else
            Result.Insert (Num, 1);
         end if;
      end loop;

      return Result;
   end Count_Occurrances;

   --------------------------------------------------------------------------

   function Solve (File : File_Acc) return Solution is
      Left, Right             : Int_Vector;
      Part1, Part2, Left_Elem : Integer := 0;
      Counts                  : Int_Hash;
   begin
      Parse_Input (File, Left, Right);
      Counts := Count_Occurrances (Left);

      Int_Sorter.Sort (Left);
      Int_Sorter.Sort (Right);

      for Idx in Left.First_Index .. Left.Last_Index loop
         Left_Elem := Left (Idx);

         Part1 := @ + abs (Left_Elem - Right (Idx));

         if Counts.Contains (Left_Elem) then
            Part2 := @ + Left_Elem * Element (Counts.Find (Left_Elem));
         end if;
      end loop;

      return (Part1 => Part1, Part2 => Part2);
   end Solve;

end Day1;

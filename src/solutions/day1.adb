with Ada.Text_IO; use Ada.Text_IO;

with Helpers;

package body Day1 is
   package H renames Helpers;

   type Input_Type is record
      Left  : H.Int_Vector;
      Right : H.Int_Vector;
   end record;

   function Parse_Input (File : File_Acc) return Input_Type is
      Line                      : String (1 .. 50);
      Left_Num, Right_Num, Last : Integer;
      Pairs                     : Input_Type;
      Idx                       : Natural;
   begin

      while not End_Of_File (File.all) loop
         Get_Line (File.all, Line, Last);
         Idx       := Line'First;
         Left_Num  := H.Read_Int (Line, Idx);
         Right_Num := H.Read_Int (Line, Idx);

         Pairs.Left.Append (Left_Num);
         Pairs.Right.Append (Right_Num);
      end loop;

      return Pairs;
   end Parse_Input;

   --------------------------------------------------------------------------

   function Count_Occurrances (Nums : H.Int_Vector) return H.Int_Hash is
      use H.Integer_Hashed_Maps;

      Result : H.Int_Hash;
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
      use H.Integer_Hashed_Maps;

      Input                   : constant Input_Type := Parse_Input (File);
      Left                    : H.Int_Vector        := Input.Left;
      Right                   : H.Int_Vector        := Input.Right;
      Part1, Part2, Left_Elem : Integer             := 0;
      Counts : constant H.Int_Hash := Count_Occurrances (Right);
   begin

      H.Int_Sorter.Sort (Left);
      H.Int_Sorter.Sort (Right);

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

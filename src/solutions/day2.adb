with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Helpers;

package body Day2 is
   package H renames Helpers;

   function Is_Line_Safe (Numbers : H.Int_Vector) return Boolean is
      Difference, Last_Difference : Integer := 0;
      Is_Safe                     : Boolean := True;
      Number                      : Integer;
   begin
      for Idx in 1 .. Numbers.Last_Index loop
         Number     := Numbers (Idx);
         Difference := Number - Numbers (Idx - 1);

         Is_Safe :=
           abs (Difference) >= 1 and then abs (Difference) <= 3
           and then (Difference * Last_Difference) >= 0;

         if not Is_Safe then
            return False;
         end if;

         Last_Difference := Difference;

      end loop;

      return Is_Safe;
   end Is_Line_Safe;

   ------------------------------------------------------------------------

   function Parse_Line (File : File_Acc) return H.Int_Vector is
      Number, Idx, Last : Integer := 0;
      Line              : String (1 .. 50);
      Numbers           : H.Int_Vector;
   begin
      Get_Line (File.all, Line, Last);

      while Idx < Last loop
         Get (Item => Number, From => Line (Idx + 1 .. Last), Last => Idx);
         Numbers.Append (Number);
      end loop;

      return Numbers;
   end Parse_Line;

   ------------------------------------------------------------------------

   function Solve (File : File_Acc) return Solution is
      Part1, Part2          : Integer := 0;
      Numbers, Skip_Numbers : H.Int_Vector;
   begin
      while not End_Of_File (File.all) loop
         Numbers := Parse_Line (File);

         if Is_Line_Safe (Numbers) then
            Part1 := @ + 1;
            Part2 := @ + 1;
         else
            for Skip in Numbers.First_Index .. Numbers.Last_Index loop
               Skip_Numbers := Numbers;
               Skip_Numbers.Delete (Skip);
               if Is_Line_Safe (Skip_Numbers) then
                  Part2 := @ + 1;
                  exit;
               end if;
            end loop;
         end if;

      end loop;

      return (Part1 => Part1, Part2 => Part2);
   end Solve;

end Day2;

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

--  Not 364
with Helpers;

package body Day2 is
   package H renames Helpers;

   function Is_Line_Safe (Numbers : H.Int_Vector) return Boolean is
      Difference, Last_Difference : Integer := 0;
      Last_Number                 : Integer := Numbers.First_Element;
      Is_Safe                     : Boolean := True;
      Number                      : Integer;
   begin

      for Idx in 1 .. Numbers.Last_Index loop
         Number     := Numbers (Idx);
         Difference := Number - Last_Number;

         --  Check if the difference is small enough
         Is_Safe := abs (Difference) >= 1 and then abs (Difference) <= 3;

         --  Check the ordering is consistent
         Is_Safe := Is_Safe and then (Difference * Last_Difference) >= 0;

         Last_Number     := Number;
         Last_Difference := Difference;

         if not Is_Safe then
            exit;
         end if;

      end loop;

      return Is_Safe;
   end Is_Line_Safe;

   ------------------------------------------------------------------------

   procedure Remove_Idx (Source : H.Int_Vector; Dest : in out H.Int_Vector; Skip : Integer) is
   begin
      Dest.Clear;
      for Idx in Source.First_Index .. Source.Last_Index loop
         if Idx /= Skip then
            Dest.Append (Source (Idx));
         end if;
      end loop;
   end Remove_Idx;

   ------------------------------------------------------------------------

   function Solve (File : File_Acc) return Solution is
      Last, Part1, Part2    : Integer := 0;
      Numbers, Skip_Numbers : H.Int_Vector;
      Idx, Number           : Integer;
      Line                  : String (1 .. 50);
   begin
      while not End_Of_File (File.all) loop
         Get_Line (File.all, Line, Last);

         Numbers.Clear;
         Idx := 0;
         while Idx < Last loop
            Get (Item => Number, From => Line (Idx + 1 .. Last), Last => Idx);
            Numbers.Append (Number);
         end loop;

         if Is_Line_Safe (Numbers) then
            Part1 := @ + 1;
            Part2 := @ + 1;
         else
            for Skip in Numbers.First_Index .. Numbers.Last_Index loop
               Remove_Idx (Numbers, Skip_Numbers, Skip);
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

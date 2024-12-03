with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Regpat; use GNAT.Regpat;

package body Day3 is

   function Match_To_Int (Str : String; This_Match : Match_Location) return Integer is
   begin
      return Integer'Value (Str (This_Match.First .. This_Match.Last));
   end Match_To_Int;

   --------------------------------------------------------------------------

   function Find_Muls_In_Line (Haystack : String; Last : Integer) return Integer is
      Re      : constant Pattern_Matcher := Compile ("mul\((\d+)\,(\d+)\)");
      Matches : Match_Array (0 .. 2);
      Total   : Integer                  := 0;
      Idx     : Integer                  := 1;
   begin
      loop
         Match (Re, Haystack (Idx .. Last), Matches);

         if Matches (0) = No_Match then
            exit;
         end if;

         Total := @ + Match_To_Int (Haystack, Matches (1)) * Match_To_Int (Haystack, Matches (2));

         Idx := Matches (0).Last + 1;
      end loop;
      return Total;
   end Find_Muls_In_Line;

   --------------------------------------------------------------------------

   function Find_Muls_With_Controls
     (Haystack : String; Last : Integer; Enabled : in out Boolean) return Integer
   is
      Re          : constant Pattern_Matcher := Compile ("mul\((\d+)\,(\d+)\)|don't\(\)|do\(\)");
      Matches     : Match_Array (0 .. 2);
      Total       : Integer                  := 0;
      Idx         : Integer                  := 1;
      First_Chars : String (1 .. 3);
   begin
      loop
         Match (Re, Haystack (Idx .. Last), Matches);

         if Matches (0) = No_Match then
            exit;
         end if;

         First_Chars := Haystack (Matches (0).First .. Matches (0).First + 2);

         if First_Chars = "do(" then
            Enabled := True;
         elsif First_Chars = "don" then
            Enabled := False;
         elsif First_Chars = "mul" and then Enabled then
            Total :=
              @ + Match_To_Int (Haystack, Matches (1)) * Match_To_Int (Haystack, Matches (2));
         end if;

         Idx := Matches (0).Last + 1;

      end loop;
      return Total;
   end Find_Muls_With_Controls;

   --------------------------------------------------------------------------

   function Solve (File : File_Acc) return Solution is
      Last, Part1, Part2 : Integer := 0;
      Line               : String (1 .. 10_000);
      Enabled            : Boolean := True;
   begin

      while not End_Of_File (File.all) loop
         Get_Line (File.all, Line, Last);
         Part1 := @ + Find_Muls_In_Line (Line, Last);
         Part2 := @ + Find_Muls_With_Controls (Line, Last, Enabled);
      end loop;

      return (Part1 => Part1, Part2 => Part2);
   end Solve;

end Day3;

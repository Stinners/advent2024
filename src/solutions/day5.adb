with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers;

with Helpers;

package body Day5 is
   package H renames Helpers;

   -----------------------------------------------------------------

   type Rule is array (0 .. 1) of Integer;

   package Rule_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural, Element_Type => Rule);
   subtype Rule_Vector is Rule_Vectors.Vector;

   -----------------------------------------------------------------

   use H.Int_Vectors;
   subtype Update is H.Int_Vector;

   package Update_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural, Element_Type => Update);
   subtype Update_Vector is Update_Vectors.Vector;

   -----------------------------------------------------------------

   procedure Parse_Input (File : File_Acc; Rules : out Rule_Vector; Updates : out Update_Vector) is
      Reading_Updates : Boolean := False;
      Line            : String (1 .. 150);
      Idx, Last       : Integer;
   begin
      while not End_Of_File (File.all) loop
         Idx := 1;
         Get_Line (File.all, Line, Last);

         --  Empty Line between rules and updates
         if Last = 0 then
            Reading_Updates := True;

            --  Read rule Line
         elsif not Reading_Updates then
            declare
               This_Rule : Rule;
            begin
               Get (Item => This_Rule (0), From => Line (Idx .. Last), Last => Idx);
               Get (Item => This_Rule (1), From => Line (Idx + 2 .. Last), Last => Idx);
               Rules.Append (This_Rule);
            end;

            --  Read Update line
         elsif Reading_Updates then
            declare
               This_Update : Update;
               Num         : Integer;
            begin
               while Idx <= Last loop
                  Get (Item => Num, From => Line (Idx .. Last), Last => Idx);
                  This_Update.Append (Num);
                  Idx := @ + 2;
               end loop;
               Updates.Append (This_Update);
            end;
         end if;

      end loop;

   end Parse_Input;

   -----------------------------------------------------------------

   function Is_Line_Correctly_Ordered (Rules : Rule_Vector; Line : Update) return Boolean is
      Seen_Second : Boolean;
   begin
      for Rule of Rules loop
         Seen_Second := False;

         for Page of Line loop
            if Page = Rule (1) then
               Seen_Second := True;
            elsif Page = Rule (0) and then Seen_Second then
               return False;
            end if;
         end loop;

      end loop;
      return True;
   end Is_Line_Correctly_Ordered;

   -----------------------------------------------------------------

   function Solve (File : File_Acc) return Solution is

      Rules        : Rule_Vector;
      Updates      : Update_Vector;
      Part1, Part2 : Integer := 0;

      function Compare (Left, Right : Integer) return Boolean is
      begin
         for Rule of Rules loop
            if Rule (0) = Right and then Rule (1) = Left then
               return False;
            end if;
         end loop;
         return True;
      end Compare;

      package Update_Sort is new H.Int_Vectors.Generic_Sorting (Compare);

   begin

      Parse_Input (File, Rules, Updates);

      for Line of Updates loop
         if Is_Line_Correctly_Ordered (Rules, Line) then
            Part1 := @ + Line (Integer (H.Int_Vectors.Length (Line)) / 2);

         else
            Update_Sort.Sort (Line);
            Part2 := @ + Line (Integer (H.Int_Vectors.Length (Line)) / 2);
         end if;
      end loop;

      return (Part1 => Part1, Part2 => Part2);
   end Solve;

end Day5;

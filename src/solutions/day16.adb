with Ada.Text_IO; use Ada.Text_IO;

with Vector2D; use Vector2D;

package body Day16 is

   -------------------------------------------------------

   type Grid_Type is array (1 .. 150, 1 .. 150) of Integer;

   -------------------------------------------------------

   procedure Parse_Input (File : File_Acc; Grid : out Grid_Type; Size : out Vec2D) is
      Line           : String (1 .. 150);
      Y, Last, Score : Integer := 0;
      Char           : Character;
   begin
      while not End_Of_File (File.all) loop
         Get_Line (File.all, Line, Last);
         Y := @ + 1;

         for X in Line'First .. Last loop
            Char := Line (X);
            if Char = 'S' then
               Score := 0;
            elsif Char = '#' then
               Score := Integer'First;
            else
               Score := Integer'Last;
            end if;

            Grid (X, Y) := Score;
         end loop;
      end loop;
      Size := (X => Last, Y => Y);
   end Parse_Input;

   -------------------------------------------------------

   procedure Path_Find (Grid : in out Grid_Type; Pos : Vec2D; Dir : Direction) is
      Next_Cost : Integer;
      Cost      : constant Integer := Grid (Pos.X, Pos.Y);
      Next_Pos  : Vec2D;
   begin

      -- Straight Ahead
      Next_Pos  := Pos + Dir;
      Next_Cost := Cost + 1;
      if Next_Cost < Grid (Next_Pos.X, Next_Pos.Y) then
         Grid (Next_Pos.X, Next_Pos.Y) := Next_Cost;
         Path_Find (Grid, Next_Pos, Dir);
      end if;

      -- Turn Right
      Next_Pos  := Pos + Turn_Anti_Clockwise (Dir);
      Next_Cost := Cost + 1_001;
      if Next_Cost < Grid (Next_Pos.X, Next_Pos.Y) then
         Grid (Next_Pos.X, Next_Pos.Y) := Next_Cost;
         Path_Find (Grid, Next_Pos, Turn_Anti_Clockwise (Dir));
      end if;

      -- Turn Left
      Next_Pos  := Pos + Turn_Clockwise (Dir);
      Next_Cost := Cost + 1_001;
      if Next_Cost < Grid (Next_Pos.X, Next_Pos.Y) then
         Grid (Next_Pos.X, Next_Pos.Y) := Next_Cost;
         Path_Find (Grid, Next_Pos, Turn_Clockwise (Dir));
      end if;

   end Path_Find;

   -------------------------------------------------------

   function Solve (File : File_Acc) return Solution is
      Part1, Part2 : Integer := 0;
      Grid         : Grid_Type;
      Size         : Vec2D;
   begin
      Parse_Input (File, Grid, Size);
      Path_Find (Grid, (X => 2, Y => Size.Y - 1), Right);

      return (Part1 => Grid (Size.X - 1, 2), Part2 => Part2);
   end Solve;

end Day16;

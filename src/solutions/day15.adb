with Ada.Text_IO; use Ada.Text_IO;

with Vector2D; use Vector2D;

package body Day15 is

   type Grid_Type is array (1 .. 120, 1 .. 120) of Character;

   ------------------------------------------------------

   procedure Parse_Grid (File : File_Acc; Grid : out Grid_Type; Size, Start : out Vec2D) is
      Line    : String (1 .. 60);
      Y, Last : Integer := 0;
   begin
      loop
         Get_Line (File.all, Line, Last);
         Y := @ + 1;

         for X in Line'First .. Last loop
            Grid (X, Y) := Line (X);

            if Grid (X, Y) = '@' then
               Start := (x => X, Y => Y);
            end if;

         end loop;

         exit when Last = 0;

         Size.X := Last;
         Size.Y := Y;

      end loop;
   end Parse_Grid;

   ------------------------------------------------------

   function Create_Grid2 (Old_Grid : Grid_Type; Size : Vec2D) return Grid_Type is
      Tile : Character;
      Grid : Grid_Type;
      X2   : Integer;
   begin
      for Y in 1 .. Size.Y loop
         for X in 1 .. Size.X loop
            Tile := Old_Grid (X, Y);
            X2   := 2 * X;

            if Tile = '#' then
               Grid (X2 - 1, Y) := '#';
               Grid (X2, Y)     := '#';
            elsif Tile = 'O' then
               Grid (X2 - 1, Y) := '[';
               Grid (X2, Y)     := ']';
            elsif Tile = '.' then
               Grid (X2 - 1, Y) := '.';
               Grid (X2, Y)     := '.';
            elsif Tile = '@' then
               Grid (X2 - 1, Y) := '@';
               Grid (X2, Y)     := '.';
            end if;

         end loop;
      end loop;

      return Grid;
   end Create_Grid2;

   ------------------------------------------------------

   function Other_Box_Side (Grid : Grid_Type; Start : Vec2D) return Vec2D is
      This_Char : constant Character := Grid (Start.X, Start.Y);
   begin
      if This_Char = '[' then
         return Start + Unit_Vectors (Right);
      elsif This_Char = ']' then
         return Start + Unit_Vectors (Left);
      else
         raise Program_Error with "Invalid Box Character:" & This_Char'Img;
      end if;
   end Other_Box_Side;

   ------------------------------------------------------

   function Check_Can_Move (Grid : Grid_Type; Start, Dir : Vec2D) return Boolean is
      Next_Pos    : constant Vec2D     := Start + Dir;
      Next_Char   : constant Character := Grid (Next_Pos.X, Next_Pos.Y);
      Is_Vertical : constant Boolean   := Dir.Y /= 0;
      Other_Side  : Vec2D;
   begin
      --  Generic Rules
      if Next_Char = '#' then
         return False;

      elsif Next_Char = '.' then
         return True;

         --  Part 1 Rule
      elsif Next_Char = 'O' then
         return Check_Can_Move (Grid, Next_Pos, Dir);

         --  Part 2 Rules
      elsif (Next_Char = '[' or else Next_Char = ']') and then not Is_Vertical then
         return Check_Can_Move (Grid, Next_Pos, Dir);

         --  Width 2 boxes can only move vertically if both parts can move
      elsif (Next_Char = '[' or else Next_Char = ']') and then Is_Vertical then
         Other_Side := Other_Box_Side (Grid, Next_Pos);

         return
           Check_Can_Move (Grid, Next_Pos, Dir) and then Check_Can_Move (Grid, Other_Side, Dir);

      else
         raise Program_Error with "Invalid Character Type";

      end if;

      --  Part2 Rules
   end Check_Can_Move;

   ------------------------------------------------------

   procedure Do_Move (Grid : in out Grid_Type; Start, Dir : Vec2D) is
      Next_Pos    : constant Vec2D     := Start + Dir;
      This_Char   : constant Character := Grid (Start.X, Start.Y);
      Next_Char   : constant Character := Grid (Next_Pos.X, Next_Pos.Y);
      Is_Vertical : constant Boolean   := Dir.Y /= 0;
      Other_Side  : Vec2D;
   begin
      --  Move the other side of the box if necessary
      if (Next_Char = '[' or else Next_Char = ']') and then Is_Vertical then
         Other_Side := Other_Box_Side (Grid, Next_Pos);
         Do_Move (Grid, Other_Side, Dir);
      end if;

      -- Check if the block ahead needs to be move d
      if Next_Char = 'O' or else Next_Char = '[' or else Next_Char = ']' then
         Do_Move (Grid, Next_Pos, Dir);
      end if;

      --  Do the actual move
      Grid (Next_Pos.X, Next_Pos.Y) := This_Char;
      Grid (Start.X, Start.Y)       := '.';

   end Do_Move;

   ------------------------------------------------------

   procedure Calculate_Path (File : File_Acc; Grid : in out Grid_Type; Start : Vec2D) is
      Line       : String (1 .. 1_200);
      Last       : Integer;
      Dir, Robot : Vec2D;
      Char       : Character;
   begin

      Robot := Start;
      while not End_Of_File (File.all) loop
         Get_Line (File.all, Line, Last);

         for Idx in Line'First .. Last loop
            Char := Line (Idx);

            if Char = '>' then
               Dir := Unit_Vectors (Right);
            elsif Char = '^' then
               Dir := Unit_Vectors (Up);
            elsif Char = '<' then
               Dir := Unit_Vectors (Left);
            elsif Char = 'v' then
               Dir := Unit_Vectors (Down);
            end if;

            if Check_Can_Move (Grid, Robot, Dir) then
               Do_Move (Grid, Robot, Dir);
               Robot := @ + Dir;
            end if;
         end loop;
      end loop;

   end Calculate_Path;

   ------------------------------------------------------

   function Solve (File : File_Acc) return Solution is
      Part1, Part2  : Integer := 0;
      Grid, Grid2   : Grid_Type;
      Size, Size2   : Vec2D;
      Robot, Robot2 : Vec2D;
   begin
      Parse_Grid (File, Grid, Size, Robot);
      Grid2    := Create_Grid2 (Grid, Size);
      Robot2   := Robot;
      Robot2.X := Robot.X * 2 - 1;
      Size2    := Size;
      Size2.X  := Size2.X * 2;

      ------------------------------------------

      Calculate_Path (File, Grid2, Robot2);

      for Y in 1 .. Size2.Y loop
         for X in 1 .. Size2.X loop
            if Grid2 (X, Y) = 'O' or else Grid2 (X, Y) = '[' then
               Part1 := @ + (X - 1) + 100 * (Y - 1);
            end if;
         end loop;
      end loop;

      return (Part1 => Part1, Part2 => Part2);
   end Solve;

end Day15;

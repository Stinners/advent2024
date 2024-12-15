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
      X2 : Integer;
   begin
      for Y in 1 .. Size.Y  loop
         for X in 1 .. Size.X loop
            Tile := Old_Grid (X, Y);
            X2 := 2 * X;

            if Tile = '#' then
               Grid (X2-1 ,  Y)     := '#';
               Grid (X2, Y) := '#';
            elsif Tile = 'O' then
               Grid (X2-1, Y)     := '[';
               Grid (X2, Y) := ']';
            elsif Tile = '.' then
               Grid (X2-1 , Y)     := '.';
               Grid (X2, Y) := '.';
            elsif Tile = '@' then
               Grid (X2-1, Y)     := '@';
               Grid (X2, Y) := '.';
            end if;

         end loop;
      end loop;

      return Grid;
   end Create_Grid2;

   ------------------------------------------------------

   --  Have two Move functions for part 2 
   -- Side to side movement is basically the same as part 1 
   -- Up and down movement finds both sides of the box and calleds Move2 on both of them 

   function Move (Grid : in out Grid_Type; Start, Dir : Vec2D) return Boolean is
      This_Char : constant Character := Grid (Start.X, Start.Y);
      Next_Pos  : constant Vec2D     := Start + Dir;
      Next_Char : constant Character := Grid (Next_Pos.X, Next_Pos.Y);
      Can_Move  : Boolean;
   begin
      if Next_Char = '#' then
         Can_Move := False;
      elsif Next_Char = '.' then
         Can_Move := True;
      elsif Next_Char = 'O' then
         Can_Move := Move (Grid, Next_Pos, Dir);
      end if;

      if Can_Move then
         Grid (Start.X, Start.Y)       := '.';
         Grid (Next_Pos.X, Next_Pos.Y) := This_Char;
      end if;

      return Can_Move;
   end Move;

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

            if Move (Grid, Robot, Dir) then
               Robot := @ + Dir;
            end if;

         end loop;
      end loop;

   end Calculate_Path;

   ------------------------------------------------------

   procedure Print_Grid (Grid : Grid_Type; Size : Vec2D) is
   begin
      for Y in 1 .. Size.Y loop
         for X in 1 .. Size.X loop
            Put (Grid (X, Y));
         end loop;
         Put_Line ("");
      end loop;
   end Print_Grid;

   ------------------------------------------------------

   function Solve (File : File_Acc) return Solution is
      Part1, Part2  : Integer := 0;
      Grid, Grid2   : Grid_Type;
      Size, Size2   : Vec2D;
      Robot, Robot2 : Vec2D;
   begin
      Parse_Grid (File, Grid, Size, Robot);
      Grid2 := Create_Grid2 (Grid, Size);
      Calculate_Path (File, Grid, Robot);

      for Y in 1 .. Size.Y loop
         for X in 1 .. Size.X loop
            if Grid (X, Y) = 'O' then
               Part1 := @ + (X - 1) + 100 * (Y - 1);
            end if;
         end loop;
      end loop;

      ------------------------------------------

      Size2 := (Size with delta X => Size.X * 2);
      Put_Line(Size2'Img);
      Print_Grid(Grid2, Size2);



      return (Part1 => Part1, Part2 => Part2);
   end Solve;

end Day15;

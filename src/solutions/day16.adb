with Ada.Text_IO; use Ada.Text_IO;

with Vector2D; use Vector2D;

package body Day16 is

   -------------------------------------------------------

   -- Every tile has 4 possible states, representing the four different directions 
   type Grid_Type is array (1 .. 150, 1 .. 150, Direction) of Integer;

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
            if Char = '#' then
               Score := Integer'First;
            else
               Score := Integer'Last-1000;
            end if;
            
            for Dir in Direction loop 
                Grid (X, Y, Dir) := Score;
            end loop;
         end loop;
      end loop;
      Size := (X => Last, Y => Y);
   end Parse_Input;

   -------------------------------------------------------

   procedure Path_Find (Grid : in out Grid_Type; Pos : Vec2D; Dir : Direction) is
      Next_Cost : Integer;
      Next_Pos  : Vec2D;
      Next_Dir  : Direction;
   begin
      -- Move ahead
      Next_Pos  := Pos + Dir;
      Next_Cost := Grid (Pos.X, Pos.Y, Dir) + 1;
      if Next_Cost < Grid (Next_Pos.X, Next_Pos.Y, Dir) then
         Grid (Next_Pos.X, Next_Pos.Y, Dir) := Next_Cost;
         Path_Find (Grid, Next_Pos, Dir);
      end if;

      Next_Cost := Grid (Pos.X, Pos.Y, Dir) + 1_000;
      
      -- Turn Left
      Next_Dir := Turn_Anti_Clockwise (Dir);
      if Next_Cost < Grid (Pos.X, Pos.Y, Next_Dir) then
         Grid (Pos.X, Pos.Y, Next_Dir) := Next_Cost;
         Path_Find (Grid, Pos, Next_Dir);
      end if;

      -- Turn Right
      Next_Dir := Turn_Clockwise (Dir);
      if Next_Cost < Grid (Pos.X, Pos.Y, Next_Dir) then
         Grid (Pos.X, Pos.Y, Next_Dir) := Next_Cost;
         Path_Find (Grid, Pos, Next_Dir);
      end if;

   end Path_Find;

   -------------------------------------------------------
   
   procedure Count_Seats
        (Visited : in out Vec2D_Set; Grid : Grid_Type; Pos : Vec2D; Dir : Direction)
   is 
       Next_Pos  : Vec2D;
       Next_Dir  : Direction := Dir;
       Cost      : constant Integer := Grid (Pos.X, Pos.Y, Dir);
       Next_Cost : Integer;
   begin
       Visited.Include(Pos);

       Next_Cost := Cost - 1;
       Next_Pos := Pos - Dir;
       if Grid (Next_Pos.X, Next_Pos.Y, Next_Dir) = Next_Cost then
           Count_Seats(Visited, Grid, Next_Pos, Next_Dir);
       end if;

       Next_Cost := Cost - 1000;
       Next_Pos := Pos;

       Next_Dir := Turn_Clockwise (Dir);
       if Grid (Next_Pos.X, Next_Pos.Y, Next_Dir) = Next_Cost then
           Count_Seats(Visited, Grid, Next_Pos, Next_Dir);
       end if;

       Next_Dir := Turn_Anti_Clockwise (Dir);
       if Grid (Next_Pos.X, Next_Pos.Y, Next_Dir) = Next_Cost then
           Count_Seats(Visited, Grid, Next_Pos, Next_Dir);
       end if;
   end Count_Seats;

   -------------------------------------------------------

   function Solve (File : File_Acc) return Solution is
      Part1, Part2  : Integer := Integer'Last;
      Grid          : Grid_Type;
      Size          : Vec2D;
      Finish, Start : Vec2D;
      Seats         : Vec2D_Set;
   begin
      Parse_Input (File, Grid, Size);

      Start  := (X => 2, Y => Size.Y - 1);
      Finish := (X => Size.X - 1, Y => 2);

      Grid (Start.X, Start.Y, Right) := 0;

      Path_Find (Grid, Start, Right);
      Count_Seats (Seats, Grid, Finish, Up);

      for Dir in Direction loop 
          Part1 := Integer'Min(Part1, Grid(Finish.X, Finish.Y, Dir));
      end loop;
      Part2 := Integer(Seats.Length);

      return (Part1 => Part1, Part2 => Part2);
   end Solve;

end Day16;

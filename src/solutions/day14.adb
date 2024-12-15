with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with GNAT.Regpat;    use GNAT.Regpat;
with Ada.Containers.Vectors;

with Vector2D; use Vector2D;

package body Day14 is

   type Quadrents is (Top_Left, Top_Right, Bottom_Left, Botton_Right);
   type Quad_Counts_Type is array (Quadrents) of Integer;

   ---------------------------------------------------------

   type Robot_Type is record
      Pos : Vec2D;
      Vel : Vec2D;
   end record;

   package Robot_Vecs is new Vectors (Index_Type => Natural, Element_Type => Robot_Type);
   subtype Robot_Vec is Robot_Vecs.Vector;
   use Robot_Vecs;

   ---------------------------------------------------------

   function Match_To_Int (Line : String; M : Match_Location) return Integer is
     (Integer'Value (Line (M.First .. M.Last)));

   Line_RE : constant Pattern_Matcher := Compile ("p=(\d+)\,(\d+) v=(-?\d+)\,(-?\d+)");

   --------------------------------------------------------------------------

   function Parse_Input (File : File_Acc) return Robot_Vec is
      Robots  : Robot_Vec;
      Robot   : Robot_Type;
      Matches : Match_Array (0 .. 4);
      Line    : String (1 .. 30);
      Last    : Integer;
   begin
      while not End_Of_File (File.all) loop
         Get_Line (File.all, Line, Last);
         Match (Line_RE, Line (1 .. Last), Matches);

         Robot :=
           ((X => Match_To_Int (Line, Matches (1)) + 1, Y => Match_To_Int (Line, Matches (2)) + 1),
            (X => Match_To_Int (Line, Matches (3)), Y => Match_To_Int (Line, Matches (4))));
         Robots.Append (Robot);
      end loop;

      return Robots;
   end Parse_Input;

   ---------------------------------------------------------

   --  Accound for size information contained in the problem text rather
   --  than the input fle
   function Get_Size (Robots : Robot_Vec) return Vec2D is
   begin
      if Robots.Length < 20 then
         return (X => 11, Y => 7);
      else
         return (X => 101, Y => 103);
      end if;
   end Get_Size;

   ---------------------------------------------------------

   --  Assume none of the robots are moving so fast they can wrap around
   --  twice in a single step
   function Wrap (Pos, Size : Integer) return Integer is
   begin
      if Pos > Size then
         return Pos - Size;
      elsif Pos < 1 then
         return Pos + Size;
      else
         return Pos;
      end if;
   end Wrap;

   ---------------------------------------------------------

   procedure Step (Robot : in out Robot_Type; Size : Vec2D) is
   begin
      Robot.Pos   := @ + Robot.Vel;
      Robot.Pos.X := Wrap (Robot.Pos.X, Size.X);
      Robot.Pos.y := Wrap (Robot.Pos.Y, Size.Y);
   end Step;

   ---------------------------------------------------------

   --  The domain is always an odd number in each direction
   function Count_Robots_in_Quadrents (Robots : Robot_Vec; Size : Vec2D) return Quad_Counts_Type is
      Count : Quad_Counts_Type := [0, 0, 0, 0];
      X_Mid : constant Integer := Size.X / 2 + 1;
      Y_Mid : constant Integer := Size.Y / 2 + 1;
   begin

      for Robot of Robots loop
         if Robot.Pos.X < X_Mid then
            if Robot.Pos.Y < Y_Mid then
               Count (Top_Left) := @ + 1;
            elsif Robot.Pos.Y > Y_Mid then
               Count (Bottom_Left) := @ + 1;
            end if;

         elsif Robot.Pos.X > X_Mid then
            if Robot.Pos.Y < Y_Mid then
               Count (Top_Right) := @ + 1;
            elsif Robot.Pos.Y > Y_Mid then
               Count (Botton_Right) := @ + 1;
            end if;

         end if;
      end loop;

      return Count;
   end Count_Robots_in_Quadrents;

   ---------------------------------------------------------

   function Count_Neighbours (Robots : Robot_Vec) return Integer is
      Neighbours : Integer := 0;
      Distance   : Vec2D;
   begin
      for I in Robots.First_Index .. Robots.Last_Index loop
         for J in Robots.First_Index .. I - 1 loop
            Distance := Robots (I).Pos - Robots (J).Pos;
            if Distance.X + Distance.Y = 1 then
               Neighbours := @ + 1;
            end if;
         end loop;
      end loop;
      return Neighbours;
   end Count_Neighbours;

   ---------------------------------------------------------

   function Solve (File : File_Acc) return Solution is
      Part1, Min_Step : Integer        := 0;
      Max, This       : Integer        := 0;
      Robots          : Robot_Vec      := Parse_Input (File);
      Size            : constant Vec2D := Get_Size (Robots);
      Quads           : Quad_Counts_Type;
   begin
      Put_Line ("This one takes a while");

      for Seconds in 1 .. Size.X * Size.Y loop
         for Robot of Robots loop
            Step (Robot, Size);
         end loop;

         if Seconds = 100 then
            Quads := Count_Robots_in_Quadrents (Robots, Size);
            Part1 :=
              Quads (Top_Left) * Quads (Top_Right) * Quads (Bottom_Left) * Quads (Botton_Right);
         end if;

         This := Count_Neighbours (Robots);
         if This > Max then
            Max      := This;
            Min_Step := Seconds;
         end if;

      end loop;

      return (Part1 => Part1, Part2 => Min_Step);
   end Solve;

end Day14;

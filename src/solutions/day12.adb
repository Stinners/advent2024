with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Sets;

with Vector2D; use Vector2D;

package body Day12 is

   type Garden_Type is array (1 .. 150, 1 .. 150) of Character;

   -----------------------------------------------------------

   type Edge is record
      Loc  : Vec2D;
      Side : Direction;
   end record;

   function Hash_Edge (Key : Edge) return Hash_Type is
     (Vec2D_Hash (Key.Loc) xor Hash_Type (Direction'Pos (Key.Side)));

   package Edge_Sets is new Hashed_Sets
     (Element_Type => Edge, Hash => Hash_Edge, Equivalent_Elements => "=");
   subtype Edge_Set is Edge_Sets.Set;
   use Edge_Sets;

   -----------------------------------------------------------

   procedure Parse_Input (File : File_Acc; Garden : out Garden_Type; Size : out Vec2D) is
      Line    : String (1 .. 150);
      Y, Last : Integer := 0;
   begin
      while not End_Of_File (File.all) loop
         Get_Line (File.all, Line, Last);
         Y := @ + 1;

         for X in Line'First .. Last loop
            Garden (X, Y) := Line (X);
         end loop;
      end loop;

      Size := (X => Last, Y => Y);
   end Parse_Input;

   -----------------------------------------------------------

   function Find_Number_Edges (Edges : in out Edge_Set) return Integer is
      Count                 : Integer := 0;
      Start_Edge, Next_Edge : Edge;
      Check_Directions      : array (1 .. 2) of Vec2D;
   begin

      while Edges.Length /= 0 loop
         Start_Edge := Element (Edges.First);
         Edges.Delete (Start_Edge);
         Count := @ + 1;

         if Start_Edge.Side = Up or else Start_Edge.Side = Down then
            Check_Directions := [Unit_Vectors (Right), Unit_Vectors (Left)];
         elsif Start_Edge.Side = Right or else Start_Edge.Side = Left then
            Check_Directions := [Unit_Vectors (Up), Unit_Vectors (Down)];
         end if;

         for Dir of Check_Directions loop
            Next_Edge     := Start_Edge;
            Next_Edge.Loc := @ + Dir;

            while Edges.Contains (Next_Edge) loop
               Edges.Delete (Next_Edge);
               Next_Edge.Loc := @ + Dir;
            end loop;
         end loop;
      end loop;

      return Count;
   end Find_Number_Edges;

   -----------------------------------------------------------

   function Find_Cost
     (Garden : Garden_Type; Start, Size : Vec2D; Visited : in out Vec2D_Set) return Solution
   is
      Area, Perimiter, N_Edges     : Integer   := 0;
      To_Check                     : Vec2D_Vec := [Start];
      This_Point, Next_Point, Step : Vec2D     := Start;
      This_Plant                   : Character;
      Edges                        : Edge_Set;
   begin
      This_Plant := Garden (Start.X, Start.Y);

      while To_Check.Length /= 0 loop
         This_Point := To_Check.Last_Element;
         To_Check.Delete_Last;
         if not Visited.Contains (This_Point) then
            Visited.Include (This_Point);
            Area := @ + 1;

            for Dir in Direction loop
               Step       := Unit_Vectors (Dir);
               Next_Point := This_Point + Step;

               if not In_Bounds (Next_Point, Size) then
                  Edges.Include ((This_Point, Dir));

               else
                  if This_Plant /= Garden (Next_Point.X, Next_Point.Y) then
                     Edges.Include ((This_Point, Dir));
                  elsif not Visited.Contains (Next_Point) then
                     To_Check.Append (Next_Point);
                  end if;
               end if;
            end loop;
         end if;
      end loop;

      Perimiter := Integer (Edges.Length);
      N_Edges   := Find_Number_Edges (Edges);

      return (Part1 => Perimiter * Area, Part2 => N_Edges * Area);
   end Find_Cost;

   -----------------------------------------------------------

   function Solve (File : File_Acc) return Solution is
      Part1, Part2 : Integer := 0;
      Garden       : Garden_Type;
      Visited      : Vec2D_Set;
      Size         : Vec2D;
      Part         : Solution;
   begin
      Parse_Input (File, Garden, Size);

      for X in Garden'First (1) .. Size.X loop
         for Y in Garden'First (2) .. Size.Y loop
            Part  := Find_Cost (Garden, (X, Y), Size, Visited);
            Part1 := @ + Part.Part1;
            Part2 := @ + Part.Part2;
         end loop;
      end loop;

      return (Part1 => Part1, Part2 => Part2);
   end Solve;

end Day12;

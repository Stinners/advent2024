with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Containers.Hashed_Sets;
with Ada.Containers; use Ada.Containers;

package body Day6 is

   type Pos is record
      X : Integer;
      Y : Integer;
   end record;

   function "+" (L, R : Pos) return Pos is ((X => L.X + R.X, Y => L.Y + R.Y));

   function Pos_Hash (Key : Pos) return Hash_Type is (Hash_Type (Key.X) xor Hash_Type (Key.Y));

   package Pos_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Pos, Hash => Pos_Hash, Equivalent_Elements => "=");
   subtype Pos_Set is Pos_Sets.Set;
   use Pos_Sets;

   --------------------------------------------------------

   type Direction is (Up, Down, Left, Right);
   type Dir_Array is array (Direction) of Pos;
   Unit_Vecs : constant Dir_Array :=
     [(X => 0, Y => -1), (X => 0, Y => 1), (X => -1, Y => 0), (X => 1, Y => 0)];

   --------------------------------------------------------

   type Guard_State is record
      Dir : Direction := Up;
      Loc : Pos;
   end record;

   function Guard_Hash (Key : Guard_State) return Hash_Type is
     (Pos_Hash (Key.Loc) xor Hash_Type (Direction'Pos (Key.Dir)));

   package Guard_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Guard_State, Hash => Guard_Hash, Equivalent_Elements => "=");
   subtype Path is Guard_Sets.Set;
   use Guard_Sets;

   --------------------------------------------------------

   procedure Parse_Input
     (File : File_Acc; Obstructions : out Pos_Set; Guard : out Guard_State; Size : out Pos)
   is
      Line : String (1 .. 150);
      Last : Integer;
      Y    : Integer := 0;
      Char : Character;
   begin

      while not End_Of_File (File.all) loop
         Get_Line (File.all, Line, Last);
         Y := @ + 1;

         for X in 1 .. Last loop
            Char := Line (X);

            if Char = '#' then
               Obstructions.Insert ((X => X, Y => Y));
            elsif Char = '^' then
               Guard.Loc := (X => X, Y => Y);
            end if;

         end loop;

      end loop;

      Size := (X => Last, Y => Y);

   end Parse_Input;

   --------------------------------------------------------

   procedure Turn (Guard : in out Guard_State) is
   begin
      --!pp off
      Guard.Dir := (case Guard.Dir is 
         when Up => Right, 
         when Right => Down, 
         when Down => Left,
         when Left => Up);
      --!pp on
   end Turn;

   --------------------------------------------------------

   procedure Step (Guard : in out Guard_State; Obstructions : Pos_Set) is
      Next     : Pos;
      Step_Vec : Pos;
   begin

      loop
         Next     := Guard.Loc;
         Step_Vec := Unit_Vecs (Guard.Dir);
         Next     := @ + Step_Vec;

         exit when not Obstructions.Contains (Next);

         Turn (Guard);

      end loop;

      Guard.Loc := Next;
   end Step;

   --------------------------------------------------------

   function On_Grid (Guard : Pos; Size : Pos) return Boolean is
     (Guard.X >= 1 and then Guard.Y >= 1 and then Guard.X <= Size.X and then Guard.Y <= Size.Y);

   --------------------------------------------------------

   function Solve (File : File_Acc) return Solution is
      Part2                 : Integer := 0;
      Obstructions, Visited : Pos_Set;
      Guard_Start, Guard    : Guard_State;
      Grid_Size             : Pos;
      Guard_Path            : Path;
   begin
      Parse_Input (File, Obstructions, Guard_Start, Grid_Size);

      Guard := Guard_Start;
      while On_Grid (Guard.Loc, Grid_Size) loop
         Visited.Include (Guard.Loc);
         Step (Guard, Obstructions);
      end loop;

      for Candidate of Visited loop
         if Candidate /= Guard_Start.Loc then
            Obstructions.Include (Candidate);
            Guard := Guard_Start;
            Guard_Path.Clear;

            loop
               -- Check if we've left the grid
               exit when not On_Grid (Guard.Loc, Grid_Size);

               -- Check if we have a cycle
               if Guard_Path.Contains (Guard) then
                  Part2 := @ + 1;
                  exit;
               end if;

               -- Update the path
               Guard_Path.Include (Guard);
               Step (Guard, Obstructions);

            end loop;

            Obstructions.Delete (Candidate);
         end if;
      end loop;

      return (Part1 => Integer (Visited.Length), Part2 => Part2);
   end Solve;

end Day6;

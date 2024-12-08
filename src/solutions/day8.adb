with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Indefinite_Vectors;

package body Day8 is

   type Point is record
      X : Integer;
      Y : Integer;
   end record;

   function "-" (L, R : Point) return Point is ((X => L.X - R.X, Y => L.Y - R.Y));
   function "+" (L, R : Point) return Point is ((X => L.X + R.X, Y => L.Y + R.Y));

   function "/" (P : Point; Scalar : Integer) return Point is
     ((X => P.X / Scalar, Y => P.Y / Scalar));

   ------------------------------------------------------------------

   package Point_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural, Element_Type => Point);
   subtype Point_Vec is Point_Vectors.Vector;
   use Point_Vectors;

   ------------------------------------------------------------------

   function Point_Hash (Key : Point) return Hash_Type is (Hash_Type (Key.X) xor Hash_Type (Key.Y));

   package Point_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Point, Hash => Point_Hash, Equivalent_Elements => "=");
   subtype Point_Set is Point_Sets.Set;

   ------------------------------------------------------------------

   function Char_Hash (Key : Character) return Ada.Containers.Hash_Type is
     (Hash_Type (Character'Pos (Key)));

   package Antenna_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Character, Element_Type => Point_Vec, Hash => Char_Hash, Equivalent_Keys => "=");
   subtype Antenna_Map is Antenna_Maps.Map;
   use Antenna_Maps;

   ------------------------------------------------------------------

   procedure Parse_File (File : File_Acc; Antennae : out Antenna_Map; Size : out Point) is
      Y, Last    : Integer := 0;
      Line       : String (1 .. 100);
      Char       : Character;
      This_Point : Point;
   begin

      while not End_Of_File (File.all) loop
         Get_Line (File.all, Line, Last);
         Y := @ + 1;

         for X in Line'First .. Last loop
            Char := Line (X);
            if Char /= '.' then

               if not Antennae.Contains (Char) then
                  Antennae.Insert (Char, Point_Vectors.Empty_Vector);
               end if;
               This_Point := (X => X, Y => Y);
               Antennae (Char).Append (This_Point);

            end if;
         end loop;

      end loop;

      Size := (X => Last, Y => Y);

   end Parse_File;

   ------------------------------------------------------------------

   function In_Bounds (P, Size : Point) return Boolean is
     (P.X >= 1 and then P.Y >= 1 and then P.X <= Size.X and then P.Y <= Size.Y);

   ------------------------------------------------------------------

   procedure Insert_Antinode (Antinodes : in out Point_Set; Antinode, Size : Point) is
   begin
      if In_Bounds (Antinode, Size) then
         Antinodes.Include (Antinode);
      end if;
   end Insert_Antinode;

   ------------------------------------------------------------------

   function GCD (P : Point) return Integer is
      Min       : constant Integer := Integer'Min (P.X, P.Y);
      Max       : constant Integer := Integer'Max (P.X, P.Y);
      Remainder : Integer          := Min;
      Next      : Integer          := Max rem Min;
   begin
      while Next /= 0 loop
         Remainder := Next;
         Next      := Min rem Remainder;
      end loop;
      return Remainder;
   end GCD;

   ------------------------------------------------------------------

   procedure Find_All_Antinodes
     (Antennae : Antenna_Map; Size : Point; Antinodes1, Antinodes2 : out Point_Set)
   is
      Antennae_Locs : Point_Vec;
   begin
      for Antenna_Type in Antennae.Iterate loop
         Antennae_Locs := Element (Antenna_Type);
         for I in Antennae_Locs.First_Index .. Antennae_Locs.Last_Index loop
            for J in (I + 1) .. Antennae_Locs.Last_Index loop

               declare
                  Antenna1     : constant Point := Antennae_Locs (I);
                  Antenna2     : constant Point := Antennae_Locs (J);
                  Displacement : constant Point := Antenna1 - Antenna2;
                  Antinode     : Point          := Antenna1;
               begin

                  -- Part 1
                  Insert_Antinode (Antinodes1, Antenna1 + Displacement, Size);
                  Insert_Antinode (Antinodes1, Antenna2 - Displacement, Size);

                  -- Part 2
                  while In_Bounds (Antinode, Size) loop
                     Antinodes2.Include (Antinode);
                     Antinode := @ - Displacement;
                  end loop;

                  Antinode := Antenna2;
                  while In_Bounds (Antinode, Size) loop
                     Antinodes2.Include (Antinode);
                     Antinode := @ + Displacement;
                  end loop;

               end;

            end loop;
         end loop;
      end loop;
   end Find_All_Antinodes;

   ------------------------------------------------------------------

   function Solve (File : File_Acc) return Solution is
      Antinodes1, Antinodes2 : Point_Set;
      Antennae               : Antenna_Map;
      Size                   : Point;
   begin
      Parse_File (File, Antennae, Size);
      Find_All_Antinodes (Antennae, Size, Antinodes1, Antinodes2);
      return (Part1 => Integer (Antinodes1.Length), Part2 => Integer (Antinodes2.Length));
   end Solve;

end Day8;

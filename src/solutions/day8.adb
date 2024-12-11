with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;

with Vector2D; use Vector2D;

package body Day8 is
    use Vec2D_Vectors;

   function Char_Hash (Key : Character) return Ada.Containers.Hash_Type is
     (Hash_Type (Character'Pos (Key)));

   package Antenna_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Character, Element_Type => Vec2D_Vec, Hash => Char_Hash, Equivalent_Keys => "=");
   subtype Antenna_Map is Antenna_Maps.Map;
   use Antenna_Maps;

   ------------------------------------------------------------------

   procedure Parse_File (File : File_Acc; Antennae : out Antenna_Map; Size : out Vec2D) is
      Y, Last    : Integer := 0;
      Line       : String (1 .. 100);
      Char       : Character;
      Point      : Vec2D;
   begin

      while not End_Of_File (File.all) loop
         Get_Line (File.all, Line, Last);
         Y := @ + 1;

         for X in Line'First .. Last loop
            Char := Line (X);
            if Char /= '.' then

               if not Antennae.Contains (Char) then
                  Antennae.Insert (Char, Vec2D_Vectors.Empty_Vector);
               end if;
               Point := (X => X, Y => Y);
               Antennae (Char).Append (Point);

            end if;
         end loop;

      end loop;

      Size := (X => Last, Y => Y);

   end Parse_File;

   ------------------------------------------------------------------

   procedure Insert_Antinode (Antinodes : in out Vec2D_Set; Antinode, Size : Vec2D) is
   begin
      if In_Bounds (Antinode, Size) then
         Antinodes.Include (Antinode);
      end if;
   end Insert_Antinode;

   ------------------------------------------------------------------

   procedure Find_All_Antinodes
     (Antennae : Antenna_Map; Size : Vec2D; Antinodes1, Antinodes2 : out Vec2d_Set)
   is
      Antennae_Locs : Vec2D_Vec;
   begin
      for Antenna_Type in Antennae.Iterate loop
         Antennae_Locs := Element (Antenna_Type);
         for I in Antennae_Locs.First_Index .. Antennae_Locs.Last_Index loop
            for J in (I + 1) .. Antennae_Locs.Last_Index loop

               declare
                  Antenna1     : constant Vec2D := Antennae_Locs (I);
                  Antenna2     : constant Vec2D := Antennae_Locs (J);
                  Displacement : constant Vec2D := Antenna1 - Antenna2;
                  Antinode     : Vec2D          := Antenna1;
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
      Antinodes1, Antinodes2 : Vec2D_Set;
      Antennae               : Antenna_Map;
      Size                   : Vec2D;
   begin
      Parse_File (File, Antennae, Size);
      Find_All_Antinodes (Antennae, Size, Antinodes1, Antinodes2);
      return (Part1 => Integer (Antinodes1.Length), Part2 => Integer (Antinodes2.Length));
   end Solve;

end Day8;

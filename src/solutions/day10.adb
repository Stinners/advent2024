with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Sets;

with Vector2D; use Vector2D;
with Helpers;  use Helpers;

package body Day10 is
    use Vec2D_Vectors;

    type Map_Type is array (1 .. 50, 1 .. 50) of Integer; 

    type Trail_Stats is record
        Score  : Integer := 0;
        Rating : Integer := 0;
    end record;

    function Vec2D_Vec_Hash (Key : Vec2D_Vec) return Hash_Type is
        Hash : Hash_Type := 0;
    begin 
        for V of Key loop
            Hash := Hash xor Vec2D_Hash(v);
        end loop;
        return Hash;
    end Vec2D_Vec_Hash;

    package Trail_Sets is new Hashed_Sets 
        (Element_Type => Vec2D_Vec, Hash => Vec2D_Vec_Hash, Equivalent_Elements => "=");
    subtype Trail_Set is Trail_Sets.Set;


---------------------------------------------------------------
    
    procedure Parse_File(File : File_Acc; Map : out Map_Type; Size : out Vec2D) is 
        Y, Last : Integer := 0;
        Line    : String (1 .. 50);
    begin 
        while not End_Of_File(File.all) loop 
            Get_Line(File.all, Line, Last);
            Y := @ + 1;
            for X in Line'First .. Last loop 
                Map (X,Y) := Char_to_Int (Line (X));
            end loop;
        end loop;

        Size := (X => Last, Y => Y);
    end Parse_File;

--------------------------------------------------------------
    
    function Find_Trail_Rating
        (Map : Map_Type; Start_Position : Vec2D; Size : Vec2D) return Trail_Stats 
    is 
        Next                : Vec2D;
        Trails, Next_Trails : Trail_Set;
        Trail_Ends          : Vec2D_Set;
        Next_Trail          : Vec2D_Vec;
    begin 
        Trails.Include([Start_Position]);

        for Height in 1 .. 9 loop 

            for Trail of Trails loop 
                for Dir of Unit_Vectors loop
                    Next := Trail.Last_Element + Dir;

                    if In_Bounds(Next, Size) and then Map (Next.X, Next.Y) = Height then 
                        Next_Trail := Trail & Next;
                        Next_Trails.Include(Next_Trail);
                    end if;
                end loop;
            end loop;

            Trail_Sets.Move(Source => Next_Trails, Target => Trails);
        end loop;

        for Trail of Trails loop 
            Trail_Ends.Include(Trail.Last_Element);
        end loop;

        return (Score => Integer (Trail_Ends.Length), Rating => Integer (Trails.Length));
    end Find_Trail_Rating;

---------------------------------------------------------------

   function Solve (File : File_Acc) return Solution is
      Part1, Part2 : Integer := 0;
      Map          : Map_Type;
      Size         : Vec2D;
      Trail        : Trail_Stats;
   begin
      Parse_File(File, Map, Size);

       --  Part 1 
       for X in 1 .. Size.X loop 
           for Y in 1 .. Size.y loop 
               if Map (X,Y) = 0 then 
                   Trail := Find_Trail_Rating (Map, (X,Y), Size);
                   Part1 := @ + Trail.Score;
                   Part2 := @ + Trail.Rating;
               end if;
           end loop;
       end loop;

      return (Part1 => Part1, Part2 => Part2);
   end Solve;

end Day10;

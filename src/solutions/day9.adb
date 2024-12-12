with Ada.Text_IO;                           use Ada.Text_IO;
with Ada.Containers;                        use Ada.Containers;
with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;

with Helpers; use Helpers;

package body Day9 is

   function Parse_File (File : File_Acc) return Int_Vector is
      N_Blocks, Block_Idx, Last, Block_Label : Integer := 0;
      Line                                   : String (1 .. 22_000);
      Empty_Space                            : Boolean := False;
      Memory                                 : Int_Vector;
   begin
      Get_Line (File.all, Line, Last);

      for Idx in 1 .. Last loop
         N_Blocks := Integer'Value ([1 => Line (Idx)]);

         if Empty_Space then
            Block_Label := -1;
         else
            Block_Label := Block_Idx;
            Block_Idx   := @ + 1;
         end if;

         Empty_Space := not Empty_Space;

         Memory.Append (Block_Label, Count_Type (N_Blocks));
      end loop;

      return Memory;
   end Parse_File;

   -------------------------------------------------------

   procedure Compact_Memory (Memory : in out Int_Vector) is
      First_Empty : Integer := Memory.First_Index;
      Last_Page   : Integer := Memory.Last_Index;
   begin
      loop

         -- Find the first Empty Block
         while Memory (First_Empty) /= -1 loop
            First_Empty := @ + 1;
         end loop;

         while Memory (Last_Page) = -1 loop
            Last_Page := @ - 1;
         end loop;

         exit when First_Empty > Last_Page;

         Memory (First_Empty) := Memory (Last_Page);
         Memory (Last_Page)   := -1;

      end loop;
   end Compact_Memory;

   -------------------------------------------------------

   procedure Compact_Memory_Part2 (Memory : in out Int_Vector) is
      File_Start, File_End    : Integer := Memory.Last_Index;
      Empty_Start             : Integer;
      Current_File            : Integer := Integer'Last;
      Block_Size              : Integer;
      In_Empty, Found_Block   : Boolean := False;
   begin
       Compacting:
       loop 
           --  Move the File_End index back until we find the end of the next block 
           for Idx in reverse Memory.First_Index .. File_Start loop 
               File_End := Idx;
               exit when Memory(Idx) /= -1 and then Memory(Idx) < Current_File;
           end loop;

           exit when File_End = Memory.First_Index;
           Current_File := Memory(File_End);
           File_Start := File_End;

           --  Find the start of that block 
           for Idx in reverse Memory.First_Index .. File_End loop
               exit when Memory (Idx) /= Current_File;
               File_Start := Idx;
           end loop;
           Block_Size := File_End - File_Start;

           --  Look for an empty section of the correct size 
           for Empty_End in Memory.First_Index .. File_Start loop 

                if not In_Empty and Memory(Empty_End) = -1 then 
                    Empty_Start := Empty_End;
                    In_Empty := True;
                elsif In_Empty and Memory (Empty_End) /= -1 then 
                    In_Empty := False;
                end if;
                
                Found_Block := In_Empty and then (Empty_End - Empty_Start) = Block_Size;
                exit when Found_Block;

           end loop;

            --  If we find one then replace the contents of the blocks
            if Found_Block then
                for I in 0 .. Block_Size loop 
                    Memory (Empty_Start + I) := Current_File;
                    Memory (File_Start + I) := -1;
                end loop;
            end if;

           File_End := File_Start - 1;
           exit when File_End <= Memory.First_Index;

        end loop Compacting;
   end Compact_Memory_Part2;

   -------------------------------------------------------

   function Checksum (Memory : Int_Vector) return Big_Integer is
      Sum      : Big_Integer := 0;
      Position : Big_Integer := 0;
   begin

      for Number of Memory loop
          if Number /= -1 then 
             Sum      := @ + (Position * To_Big_Integer (Number));
          end if;
          Position := @ + 1;
      end loop;

      return Sum;
   end Checksum;

   -------------------------------------------------------
   --
   function Solve (File : File_Acc) return Solution is
      Part1, Part2    : Big_Integer := 0;
      Memory, Memory2 : Int_Vector;
   begin
      Memory := Parse_File (File);
      Memory2 := Memory;

      Compact_Memory (Memory);
      Part1 := Checksum (Memory);

      Compact_Memory_Part2(Memory2);
      Part2 := Checksum(Memory2);

      Put_Line ("Part1: " & Part1'Img);
      Put_Line ("Part2: " & Part2'Img);
      return (Part1 => 0, Part2 => 0);
   end Solve;

end Day9;

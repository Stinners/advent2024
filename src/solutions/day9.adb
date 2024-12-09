with Ada.Text_IO;                           use Ada.Text_IO;
with Ada.Containers;                        use Ada.Containers;
with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Containers.Indefinite_Vectors;

with Helpers;

package body Day9 is
   package H renames Helpers;

   -------------------------------------------------------

   function Parse_File (File : File_Acc) return H.Int_Vector is
      N_Blocks, Block_Idx, Last, Block_Label : Integer := 0;
      Line                                   : String (1 .. 22_000);
      Empty_Space                            : Boolean := False;
      Memory                                 : H.Int_Vector;
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

   procedure Compact_Memory (Memory : in out H.Int_Vector) is
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

   procedure Compact_Memory_Part2 (Memory : in out H.Int_Vector) is
      File_Start, File_End : Integer := Memory.Last_Index;
      Current_File : Integer := 1000;
      First_Empty : Integer := Memory.First_Index;
      Last_Page   : Integer := Memory.Last_Index;
   begin
      loop

         -- Find the start of the pages


      end loop;
   end Compact_Memory;


   -------------------------------------------------------

   procedure Compact_Memory (Memory : in out H.Int_Vector) is
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

   function Checksum (Memory : H.Int_Vector) return Big_Integer is
      Sum      : Big_Integer := 0;
      Position : Big_Integer := 0;
   begin

      for Number of Memory loop
         exit when Number = -1;
         Sum      := @ + (Position * To_Big_Integer (Number));
         Position := @ + 1;
      end loop;

      return Sum;
   end Checksum;

   -------------------------------------------------------
   -- 7891029320 is too low
   --  6349606724455
   function Solve (File : File_Acc) return Solution is
      Part1, Part2 : Big_Integer := 0;
      Memory       : H.Int_Vector;
   begin
      Memory := Parse_File (File);
      Compact_Memory (Memory);
      Part1 := Checksum (Memory);

      Put_Line (Part1'Img);
      return (Part1 => 0, Part2 => 0);
   end Solve;

end Day9;

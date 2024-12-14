with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Numerics.Generic_Real_Arrays;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

package body Day13 is

   package Double_Arrays is new Ada.Numerics.Generic_Real_Arrays (Long_Long_Float);
   use Double_Arrays;

   package Double_IO is new Ada.Text_IO.Float_IO (Long_Long_Float);
   use Double_IO;

   type Prize_Type is record
      Buttons : Real_Matrix (1 .. 2, 1 .. 2);
      Prize   : Real_Vector (1 .. 2);
   end record;

   package Prize_Vecs is new Vectors (Index_Type => Natural, Element_Type => Prize_Type);
   subtype Prize_Vec is Prize_Vecs.Vector;
   use Prize_Vecs;

   --------------------------------------------------------

   function Parse_Fixed_Width_Integers (File : File_Acc; Jump1, Jump2 : Integer) return Real_Vector
   is
      Line      : String (1 .. 100);
      Idx, Last : Integer := 1;
      Values    : Real_Vector (1 .. 2);
   begin
      Get_Line (File.all, Line, Last);
      Get (Item => Values (1), From => Line (Idx + Jump1 .. Last), Last => Idx);
      Get (Item => Values (2), From => Line (Idx + Jump2 .. Last), Last => Idx);
      return Values;
   end Parse_Fixed_Width_Integers;

   --------------------------------------------------------

   function Parse_Prize (File : File_Acc) return Prize_Vec is
      Last         : Integer;
      Empty_String : String (1 .. 1);
      Prize        : Prize_Type;
      Line_Nums    : Real_Vector (1 .. 2);
      Prizes       : Prize_Vec;
   begin
      loop
         Prize := (Buttons => <>, Prize => <>);
         for Row in 1 .. 2 loop
            Line_Nums              := Parse_Fixed_Width_Integers (File, 12, 5);
            Prize.Buttons (1, Row) := Line_Nums (1);
            Prize.Buttons (2, Row) := Line_Nums (2);
         end loop;

         Prize.Prize := Parse_Fixed_Width_Integers (File, 9, 5);
         Prizes.Append (Prize);

         exit when End_Of_File (File.all);
         Get_Line (File.all, Empty_String, Last);

      end loop;

      return Prizes;
   end Parse_Prize;

   --------------------------------------------------------

   function Is_Whole (Num : Long_Long_Float) return Boolean is
      Thresh : constant Long_Long_Float := 1.0e-3;
   begin
      Put_Line (Long_Long_Float'Remainder (Num, 1.0)'Img);
      return abs (Long_Long_Float'Remainder (Num, 1.0)) < Thresh;
   end Is_Whole;

   --------------------------------------------------------

   --  We're assuming none of the button pairs are linearly dependent
   function Find_Cost (Prize : Prize_Type; Shift : Long_Long_Float := 0.0) return Long_Integer is
      Shift_Vec : constant Real_Vector          := [Shift, Shift];
      Candidate : constant Real_Vector (1 .. 2) := Solve (Prize.Buttons, Prize.Prize + Shift_Vec);
   begin
      Put_Line (Candidate'Img);
      if Is_Whole (Candidate (1)) and then Is_Whole (Candidate (2)) then
         Put_Line ("Works");
         return Long_Integer (Candidate (1) * 3.0 + Candidate (2));
      else
         return 0;
      end if;

   end Find_Cost;

   --------------------------------------------------------

   --  32067 is the solution
   --  57036339346405 is too low
   function Solve (File : File_Acc) return Solution is
      Part1, Part2 : Long_Integer := 0;
      Prizes       : Prize_Vec;
   begin
      Prizes := Parse_Prize (File);

      for Prize of Prizes loop
         Part1 := @ + Find_Cost (Prize);
         Part2 := @ + Find_Cost (Prize, 10_000_000_000_000.0);
      end loop;

      Put_Line (Part1'Img);
      Put_Line (Part2'Img);

      return (Part1 => 0, Part2 => 0);
   end Solve;

end Day13;

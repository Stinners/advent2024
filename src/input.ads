with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors; use Ada.Containers;

package Input is

   Input_Exception : exception;

   type Run_Type is (Example, Problem);

   type File_Acc is access all File_Type;

   type Solution is record
      Part1 : Integer;
      Part2 : Integer;
   end record;

   type Run is record
      Day        : Integer;
      Input_File : Run_Type;
   end record;

   function Read_Command_Line_Input return Run;

   function Get_Input_File (Command : Run) return File_Acc;

   package Line_Vec_Pkg is new Indefinite_Vectors (Index_Type => Positive, Element_Type => String);
   subtype Line_Vec is Line_Vec_Pkg.Vector;

   function Read_Lines (File : File_Acc) return Line_Vec;

end Input;

with Ada.Characters.Handling; use Ada.Characters.Handling;

with Helpers;

with Ada.Command_Line;

package body Input is
   package CLI renames Ada.Command_Line;

   package H renames Helpers;

   ---  Parse Day Input  ---

   function Parse_Day_Input (Arg : Standard.String) return Integer is
   begin
      return Integer'Value (Arg);
   exception
      when Constraint_Error =>
         raise Input_Exception
           with "First Argument must be an integer, argument was: '" & Arg &
           "'";
   end Parse_Day_Input;

   ---  Parse Type Input  ---

   function Parse_Type_Input (Arg : Standard.String) return Run_Type is
   begin
      return Run_Type'Value (Arg);
   exception
      when Constraint_Error =>
         raise Input_Exception
           with "Second Argument must be an either 'example' or 'problem'" &
           ", argument was: '" & Arg & "'";
   end Parse_Type_Input;

   ---  Read Command_Line_Input  ---

   function Read_Command_Line_Input return Run is
      Day_Number : Integer;
      Input_Type : Run_Type;
   begin
      if CLI.Argument_Count < 2 then
         raise Input_Exception
           with "Please enter two inputs: a day number and either 'example' or 'problem'" &
           "e.g. '2 problem'";
      end if;

      Day_Number := Parse_Day_Input (CLI.Argument (1));
      Input_Type := Parse_Type_Input (CLI.Argument (2));

      return (Day => Day_Number, Input_File => Input_Type);
   end Read_Command_Line_Input;

   --- Get Access to the Input File ---

   function Get_Input_File (Command : Run) return File_Acc is
      Dir      : constant String          := To_Lower (Command.Input_File'Img);
      Day_Num  : constant String          := H.Int_Image (Command.Day);
      Filepath : constant Standard.String := Dir & "s/day" & Day_Num & ".txt";
      File     : constant File_Acc        := new File_Type;
   begin
      Open (File => File.all, Mode => In_File, Name => Filepath);
      return File;
   exception
      when Name_Error =>
         raise Input_Exception with "File: '" & Filepath & "' does not exist";
   end Get_Input_File;

   --- Get Lines from a File ---

   function Read_Lines (File : File_Acc) return Line_Vec is
      Lines : Line_Vec;
      Line  : String (1 .. 100);
      Last  : Natural;
   begin
      while not End_Of_File (File.all) loop
         Get_Line (File.all, Line, Last);
         Put_Line (Line (1 .. Last));
      end loop;
      return Lines;
   end Read_Lines;

end Input;

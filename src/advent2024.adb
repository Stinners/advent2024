with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Input; use Input;

with Day1;
with Day2;
with Day3;
with Day4;
with Day5;
with Day6;
with Day7;
with Day8;

procedure Advent2024 is
   Run_Command : Input.Run;
   File        : Input.File_Acc;
   Answer      : Input.Solution;
begin

   begin
      Run_Command := Input.Read_Command_Line_Input;
      File        := Input.Get_Input_File (Run_Command);

      --!pp off
      Answer :=
        (case Run_Command.Day is
           when 1 => Day1.Solve (File),
           when 2 => Day2.Solve (File),
           when 3 => Day3.Solve (File),
           when 4 => Day4.Solve (File),
           when 5 => Day5.Solve (File),
           when 6 => Day6.Solve (File),
           when 7 => Day7.Solve (File),
           when 8 => Day8.Solve (File),
           when others => raise Program_Error with "Solution not finished");
      --!pp on

      -- The real results of day 7 are Big_Integers, so we need to handle them
      -- specially
      if Run_Command.Day /= 7 then
         Put_Line ("Part 1:" & Answer.Part1'Img);
         Put_Line ("Part 2:" & Answer.Part2'Img);
      end if;

   exception
      when E : Input.Input_Exception =>
         Put_Line (Exception_Message (E));

   end;

   if File /= null and then Is_Open (File.all) then
      Close (File.all);
   end if;
end Advent2024;

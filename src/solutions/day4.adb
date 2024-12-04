with Ada.Text_IO; use Ada.Text_IO;

package body Day4 is

   type Char_Grid is array (1 .. 150, 1 .. 150) of Character;
   type Dir_vec is array (1 .. 2) of Integer;
   type Step_Vec is array (1 .. 4) of Dir_vec;

   type Wordsearch is record
      Grid   : Char_Grid;
      Height : Natural;
      Width  : Natural;
   end record;

   function Build_Grid (File : File_Acc) return Wordsearch is
      Grid   : Char_Grid;
      Line   : String (1 .. 200);
      Last   : Integer;
      Line_N : Integer := 0;
   begin

      while not End_Of_File (File.all) loop
         Get_Line (File.all, Line, Last);
         Line_N := @ + 1;

         for Char in 1 .. Last loop
            Grid (Char, Line_N) := Line (Char);
         end loop;

      end loop;

      return (Grid => Grid, Width => Last, Height => Line_N);
   end Build_Grid;

   --------------------------------------------------------------------------

   function Is_Xmas (Grid : Wordsearch; X, Y : Natural; Step : Dir_vec) return Boolean is
      Candidate : String (1 .. 4);
      Idx_X     : Integer          := X;
      Idx_Y     : Integer          := Y;
      X_Bound   : constant Integer := X + Step (1) * 3;
      Y_Bound   : constant Integer := Y + Step (2) * 3;
   begin

      if (X_Bound > Grid.Width or else X_Bound < 1)
        or else (Y_Bound > Grid.Height or else Y_Bound < 1)
      then
         return False;
      end if;

      for Idx in 1 .. 4 loop
         Candidate (Idx) := Grid.Grid (Idx_X, Idx_Y);

         Idx_X := @ + Step (1);
         Idx_Y := @ + Step (2);
      end loop;
      return Candidate = "XMAS" or else Candidate = "SAMX";
   end Is_Xmas;

   --------------------------------------------------------------------------

   function Is_X_Mas (Grid : Wordsearch; X, Y : Natural) return Boolean is
      Candidate1, Candidate2 : String (1 .. 3);
   begin
      if X + 2 > Grid.Width or else Y + 2 > Grid.Height then
         return False;
      end if;

      for Idx in 0 .. 2 loop
         Candidate1 (Idx + 1) := Grid.Grid (X + Idx, Y + Idx);
         Candidate2 (Idx + 1) := Grid.Grid (X + Idx, Y - Idx + 2);
      end loop;

      return
        (Candidate1 = "MAS" or else Candidate1 = "SAM")
        and then (Candidate2 = "MAS" or else Candidate2 = "SAM");

   end Is_X_Mas;

   --------------------------------------------------------------------------

   function Solve (File : File_Acc) return Solution is
      Grid         : constant Wordsearch := Build_Grid (File);
      Part1, Part2 : Integer             := 0;
      Steps        : constant Step_Vec   := [[1, 0], [0, 1], [1, 1], [-1, 1]];
   begin

      for Y in 1 .. Grid.Height loop
         for X in 1 .. Grid.Width loop

            --  Part 1  --
            for Step of Steps loop
               if Is_Xmas (Grid, X, Y, Step) then
                  Part1 := @ + 1;
               end if;
            end loop;

            --  Part 2  --
            if Is_X_Mas (Grid, X, Y) then
               Part2 := @ + 1;
            end if;

         end loop;
      end loop;

      return (Part1 => Part1, Part2 => Part2);
   end Solve;

end Day4;

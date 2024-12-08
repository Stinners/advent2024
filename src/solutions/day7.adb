with Ada.Text_IO;                           use Ada.Text_IO;
with Ada.Integer_Text_IO;                   use Ada.Integer_Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;

with Helpers;

package body Day7 is
   package H renames Helpers;

   type Calibration is record
      Target  : Big_Integer;
      Numbers : H.Int_Vector;
   end record;

   ------------------------------------------------------------------

   function Find_Colon (Line : String; Last : Integer) return Integer is
   begin
      for Idx in Line'First .. Last loop
         if Line (Idx) = ':' then
            return Idx;
         end if;
      end loop;
      return Last;
   end Find_Colon;

   ------------------------------------------------------------------

   function Parse_Line (Line : String; Last : Integer) return Calibration is
      Idx    : Integer := 1;
      Number : Integer;
      Cal    : Calibration;
   begin
      Idx        := Find_Colon (Line, Last);
      Cal.Target := From_String (Line (Line'First .. Idx - 1));

      while Idx < Last loop
         Idx := @ + 1;
         Get (Item => Number, From => Line (Idx .. Last), Last => Idx);
         Cal.Numbers.Append (Number);
      end loop;

      return Cal;
   end Parse_Line;

   ------------------------------------------------------------------

   function Can_Be_True (Cal : Calibration; Total : Big_Integer; Idx : Integer) return Boolean is
      Mul, Sum : Big_Integer;
   begin

      --  Return immediatly if we're higher than the target
      if Total > Cal.Target then
         return False;
      end if;

      --  If we're at the end
      if Idx > Cal.Numbers.Last_Index then
         return Total = Cal.Target;

         --  On the first case set both posibilities to the inital number
      elsif Total = 0 then
         Mul := To_Big_Integer (Cal.Numbers.First_Element);
         Sum := To_Big_Integer (Cal.Numbers.First_Element);

         --  Otherwise calculate the next two posibilities
      else
         Mul := Total * To_Big_Integer (Cal.Numbers (Idx));
         Sum := Total + To_Big_Integer (Cal.Numbers (Idx));

      end if;

      -- else recurse
      return (Can_Be_True (Cal, Mul, Idx + 1) or else Can_Be_True (Cal, Sum, Idx + 1));

   end Can_Be_True;

   ------------------------------------------------------------------

   function Can_Be_True_Part2 (Cal : Calibration; Total : Big_Integer; Idx : Integer) return Boolean
   is
      Mul, Sum, Cat : Big_Integer;
      Next          : Integer;
   begin

      --  Return immediatly if we're higher than the target
      if Total > Cal.Target then
         return False;
      end if;

      --  If we're at the end
      if Idx > Cal.Numbers.Last_Index then
         return Total = Cal.Target;

         --  On the first case set both posibilities to the inital number
      elsif Total = 0 then
         Mul := To_Big_Integer (Cal.Numbers.First_Element);
         Sum := To_Big_Integer (Cal.Numbers.First_Element);
         Cat := To_Big_Integer (Cal.Numbers.First_Element);

         --  Otherwise calculate the next two posibilities
      else
         Next := Cal.Numbers (Idx);
         Mul  := Total * To_Big_Integer (Next);
         Sum  := Total + To_Big_Integer (Next);

         declare
            Next_Img : constant String := Next'Img;
         begin
            Cat := From_String (Total'Img & Next_Img (2 .. Next_Img'Last));
         end;
      end if;

      -- else recurse
      return
        (Can_Be_True_Part2 (Cal, Mul, Idx + 1) or else Can_Be_True_Part2 (Cal, Sum, Idx + 1)
         or else Can_Be_True_Part2 (Cal, Cat, Idx + 1));

   end Can_Be_True_Part2;

   ------------------------------------------------------------------

   function Solve (File : File_Acc) return Solution is
      Part1, Part2 : Big_Integer := 0;
      Last         : Integer;
      Line         : String (1 .. 100);
      Cal          : Calibration;
   begin

      while not End_Of_File (File.all) loop
         Get_Line (File.all, Line, Last);
         Cal := Parse_Line (Line, Last);

         if Can_Be_True (Cal, 0, Cal.Numbers.First_Index) then
            Part1 := @ + Cal.Target;
         end if;

         if Can_Be_True_Part2 (Cal, 0, Cal.Numbers.First_Index) then
            Part2 := @ + Cal.Target;
         end if;

      end loop;

      Put_Line ("Part1:" & Part1'Img);
      Put_Line ("Part2:" & Part2'Img);

      return (Part1 => 0, Part2 => 0);
   end Solve;

end Day7;

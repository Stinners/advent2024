package body Vector2D is

   -------------------------------------------------------
   --  Basic Operators  --

   function "-" (L, R : Vec2D) return Vec2D is (X => L.X - R.X, Y => L.Y - R.Y);

   function "+" (L, R : Vec2D) return Vec2D is (X => L.X + R.X, Y => L.Y + R.Y);

   function "/" (V : Vec2D; Scalar : Integer) return Vec2D is
     (X => V.X / Scalar, Y => V.Y / Scalar);

   function "*" (Scalar : Integer; V : Vec2D) return Vec2D is
     (X => Scalar * V.X, Y => Scalar * V.Y);

   function Vec2D_Hash (V : Vec2D) return Hash_Type is (Hash_Type (V.X) xor Hash_Type (V.Y));

   -------------------------------------------------------
   --  Useful Functions  --

   --  Assumes the bounds start at 1
   --  32 is not right
   function In_Bounds (V, Size : Vec2D) return Boolean is
     (V.X >= 1 and then V.Y >= 1 and then V.X <= Size.X and then V.Y <= Size.Y);

   function GCD (V : Vec2D) return Integer is
      Min       : constant Integer := Integer'Min (V.X, V.Y);
      Max       : constant Integer := Integer'Max (V.X, V.Y);
      Remainder : Integer          := Min;
      Next      : Integer          := Max rem Min;
   begin
      while Next /= 0 loop
         Remainder := Next;
         Next      := Min rem Remainder;
      end loop;
      return Remainder;
   end GCD;

   function Manhattan_Norm (U, V : Vec2D) return Integer is
      Distance : constant Vec2d := U + V;
   begin
      return Distance.X + Distance.Y;
   end Manhattan_Norm;

   -----------------------------------------------------------

   function "+" (V : Vec2D; Dir : Direction) return Vec2D is (V + Unit_Vectors (Dir));
   function "-" (V : Vec2D; Dir : Direction) return Vec2D is (V - Unit_Vectors (Dir));

   ------------------------------------------------------------------

   function Turn_Clockwise (Dir : Direction) return Direction is
     ((case dir is when Up => Right, when Right => Down, when Down => Left, when Left => Up));

   function Turn_Anti_Clockwise (Dir : Direction) return Direction is
     ((case dir is when Up => Left, when Left => Down, when Down => Right, when Right => Up));

   ------------------------------------------------------------------

end Vector2D;

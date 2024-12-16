with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;

package Vector2D is

   type Vec2D is record
      X : Integer;
      Y : Integer;
   end record;

   function "-" (L, R : Vec2D) return Vec2D;
   function "+" (L, R : Vec2D) return Vec2D;

   function "/" (V : Vec2D; Scalar : Integer) return Vec2D;
   function "*" (Scalar : Integer; V : Vec2D) return Vec2D;

   ------------------------------------------------------------------

   package Vec2D_Vectors is new Vectors (Index_Type => Natural, Element_Type => Vec2D);
   subtype Vec2D_Vec is Vec2D_Vectors.Vector;

   ------------------------------------------------------------------

   function Vec2D_Hash (V : Vec2D) return Hash_Type;

   package Vec2D_Sets is new Hashed_Sets
     (Element_Type => Vec2D, Hash => Vec2D_Hash, Equivalent_Elements => "=");
   subtype Vec2D_Set is Vec2D_Sets.Set;

   -------------------------------------------------------

   type Direction is (Up, Down, Left, Right);
   type Unit_Vector_Type is array (Direction) of Vec2D;
   Unit_Vectors : constant Unit_Vector_Type :=
     [(X => 0, Y => -1), (X => 0, Y => 1), (X => -1, Y => 0), (X => 1, Y => 0)];

   type Neighbour_Vector_Type is array (1 .. 8) of Vec2D;
   Neighbour_Vectors : constant Neighbour_Vector_Type :=
     [(0, -1), (0, 1), (-1, 0), (1, 0), (-1, 1), (1, -1), (1, 1), (-1, -1)];

   function "+" (V : Vec2D; Dir : Direction) return Vec2D;

   ------------------------------------------------------------------

   function In_Bounds (V, Size : Vec2D) return Boolean;

   function GCD (V : Vec2D) return Integer;

   function Manhattan_Norm (U, V : Vec2D) return Integer;

   ------------------------------------------------------------------

   function Turn_Clockwise (Dir : Direction) return Direction;
   function Turn_Anti_Clockwise (Dir : Direction) return Direction;

   ------------------------------------------------------------------

end Vector2D;

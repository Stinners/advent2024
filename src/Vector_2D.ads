with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Indefinite_Vectors;

package Vector_2D is

   type Vec2D is record
      X : Integer;
      Y : Integer;
   end record;

   function "-" (L, R : Vec2D) return Vec2D;
   function "+" (L, R : Vec2D) return Vec2D;

   function "/" (Scalar : Integer; V : Vec2D) return Vec2D;
   function "*" (V : Vec2D; Scalar : Integer) return Vec2D;

   ------------------------------------------------------------------

   package Vec2D_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural, Element_Type => Vec2D);
   subtype Vec2D_Vec is Vector_2D_Vectors.Vector;

   ------------------------------------------------------------------

   function Vec2D_Hash (Key : Vec2D) return Hash_Type;

   package Vec2D_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Vec2D, Hash => Vec2D_Hash, Equivalent_Elements => "=");
   subtype Vec2D_Set is Vec2D_Sets.Set;

   ------------------------------------------------------------------

   function In_Bounds (P, Size : Vec2D) return Boolean;

   function GCD (P : Point) return Integer;

   ------------------------------------------------------------------

end Vector_2D;

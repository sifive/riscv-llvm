package Bitfield is
   -- Read and write of a non-integer scalar bitfield.
   type T is record
      B : Boolean;
      F : Float;
   end record;
   for T use record
     B at 0 range 1 .. 1;
     F at 0 range 2 .. 33;
   end record;
   function R (X : T) return Boolean;
   function R (X : T) return Float;
   procedure W (X : in out T; B : Boolean);
   procedure W (X : in out T; F : Float);
end;

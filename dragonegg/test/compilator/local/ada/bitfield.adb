package body Bitfield is
   function R (X : T) return Boolean is
   begin
      return X.B;
   end;
   function R (X : T) return Float is
   begin
      return X.F;
   end;
   procedure W (X : in out T; B : Boolean) is
   begin
      X.B := B;
   end;
   procedure W (X : in out T; F : Float) is
   begin
      X.F := F;
   end;
end;

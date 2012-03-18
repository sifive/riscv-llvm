package body Non_LValue is
   function A (Y : U) return String is
   begin
      return Y.X.B;
   end;
end;

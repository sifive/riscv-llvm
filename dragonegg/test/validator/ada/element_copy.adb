-- RUN: %dragonegg -S -O2 %s -o - | FileCheck %s
package body Element_Copy is
-- CHECK: @element_copy__variablesizedfieldIP
-- CHECK: store i8 105,

   function F return VariableSizedField is
-- CHECK: @element_copy__f
      X : VariableSizedField;
-- CHECK: store i8 105,
   begin
      return X;
   end;
end;

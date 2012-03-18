-- RUN: %dragonegg -S %s -o - | FileCheck %s
package Constant_Fold is
-- CHECK-NOT: ptrtoint
  Error : exception;
end;

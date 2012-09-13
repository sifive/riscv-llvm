-- RUN: %dragonegg -S -O2 %s -o - | FileCheck %s

package body ABI_Overflow is
   procedure Foo (X : T) is
-- CHECK: foo
-- CHECK-NOT: store i64
   begin
      null;
   end;
end;

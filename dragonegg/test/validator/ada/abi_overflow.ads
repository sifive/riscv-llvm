-- RUN: true
package ABI_Overflow is
   type T is array (1 .. 3) of Character;
   procedure Foo (X : T);
end;

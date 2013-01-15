// RUN: %eggdragon -O0 -g -S %s -o - | %dragonegg -c -xassembler - -o /dev/null
// PR1013
// Check to make sure debug symbols use the correct name for globals and
// functions.  Will not assemble if it fails to.

int foo __asm__("f\001oo");

int bar() {
  return foo;
}

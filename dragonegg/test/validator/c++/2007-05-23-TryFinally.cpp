// RUN: %dragonegg %s -S -O2 -o - | FileCheck %s

struct One { };
struct Two { };

void handle_unexpected () {
// CHECK: resume
// CHECK: resume
  try
  {
    throw;
  }
  catch (One &)
  {
    throw Two ();
  }
}

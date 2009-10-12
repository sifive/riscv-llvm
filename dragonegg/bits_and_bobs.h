// Place to keep various things that will need to be sorted out someday.
#ifndef BITS_AND_BOBS_H
#define BITS_AND_BOBS_H

union tree_node;

// emit_global_to_llvm - Emit the specified VAR_DECL to LLVM as a global
// variable.
// FIXME: Should not be here
void emit_global_to_llvm(union tree_node*);

extern bool flag_odr;

#endif

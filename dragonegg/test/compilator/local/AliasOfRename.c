// PR15090

extern int *A (); extern __typeof (A) A __asm__ ("AA");
// A -> AA (extern), renaming.


extern __typeof (A) B; extern __typeof (B) B __asm__ ("BB");
// B -> BB (extern), renaming.

int *B () { return 0; }
// BB defined.


extern __typeof (B) C __asm__ ("B");
// C -> B (extern), renaming.

extern __typeof (B) C __attribute__ ((alias ("BB")));
// B an alias for BB.

extern __typeof (B) A __attribute__ ((weak, alias ("B")));
// AA a weak alias for B.

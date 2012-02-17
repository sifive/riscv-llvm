void yo() {
double __x;
register long double __value;
register int __ignore;
unsigned short int __cw;
unsigned short int __cwtmp;
__asm __volatile ("fnstcw %3\n\t"
"movzwl %3, %1\n\t"
"andl $0xf3ff, %1\n\t"
"orl $0x0400, %1\n\t"       /* rounding down */
"movw %w1, %2\n\t"
"fldcw %2\n\t"
"frndint\n\t"
"fldcw %3"
: "=t" (__value), "=&q" (__ignore), "=m" (__cwtmp),
"=m" (__cw)
: "0" (__x));
}

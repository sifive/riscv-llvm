void
fd_zero (void)
{
  long int x;
  int d0, d1;
  __asm__ __volatile__ ("cld; rep; " "stosq":"=c" (d0), "=D" (d1):"a" (0),
			"0" (1), "1" (&x):"memory");
}

int
foo (unsigned long *addr)
{
  int __ret_pu;
  unsigned long __pu_val;
  return (
	   {
  asm volatile ("call __put_user_" "8": "=a" (__ret_pu): "0" (__pu_val), "c" (addr):"ebx");
	   __ret_pu;
	   });
}

static struct
{
  unsigned long address;
  unsigned short segment;
} bios32_indirect;
unsigned long
bios32_service (unsigned long service)
{
  unsigned char return_code;	/* %al */
  unsigned long address;	/* %ebx */
  unsigned long length;		/* %ecx */
  unsigned long entry;		/* %edx */
  unsigned long flags;

  local_irq_save (flags);
__asm__ ("lcall *(%%edi); cld": "=a" (return_code), "=b" (address), "=c" (length), "=d" (entry):"0" (service),
	   "1" (0),
	   "D" (&bios32_indirect));
}

//int x(void)
//{
//  unsigned long in;
//  int out;
//  asm("insn %0" : "=r" (out) : "0" (in));
//
//  return out;
//}

int test(unsigned long b) {
  int a;
  asm volatile("nop " : "=a" (a) :"0" (b));
  return a;
}

int
bar ()
{
  char c;
asm ("sarl $10, %%eax": "=a" (c):"0" (1000000));
  return c;
}

//int
//test7 (unsigned long long b)
//{
//  int a;
//  asm volatile ("foo %0 %1":"=a" (a):"0" (b));	// expected-error {{input with type 'unsigned long long' matching output with type 'int'}}
//  return a;
//}

//unsigned long long
//test8 (unsigned long long b)
//{
//  int a;
//  asm volatile ("bar %1":"=a" (b):"0" (a));	// expected-error {{input with type 'unsigned long long' matching output with type 'int'}}
//  return b;
//}

// PR3373
unsigned
t11 (signed char input)
{
  unsigned output;
__asm__ ("nop": "=a" (output):"0" (input));
  return output;
}

// PR3373
unsigned char
t12 (unsigned input)
{
  unsigned char output;
__asm__ ("nop": "=a" (output):"0" (input));
  return output;
}

unsigned char
t13 (unsigned input)
{
  unsigned char output;
__asm__ ("nop": "=a" (output):"0" (input));
  return output;
}

struct large
{
  int x[1000];
};

unsigned long
t15 (int x, struct large *P)
{
__asm__ ("nop ": "=r" (x):"m" (*P), "0" (x));
  return x;
}

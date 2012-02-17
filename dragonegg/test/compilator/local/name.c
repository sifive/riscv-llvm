unsigned bswap(unsigned) __asm__("swap");
int main(void) {
  return bswap(1);
}

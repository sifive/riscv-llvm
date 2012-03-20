// RUN: %dragonegg -xc++ %s -S -o /dev/null 2>&1 | not grep warning
// rdar://5683899
void** f(void **Buckets, unsigned NumBuckets) {
  return Buckets + NumBuckets;
}


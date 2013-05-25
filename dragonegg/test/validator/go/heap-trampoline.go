// RUN: %dragonegg -S %s -o - | FileCheck %s
// CHECK-NOT: builtin
// XFAIL: gcc-4.5
// PR16015

package main

import "os"

func main() {
	dummy := 0
	performance := func(N int) int {
		for rep := 0; rep < N; rep++ {
			dummy++
		}
		return 0
	}
	os.Exit(performance(123))
}

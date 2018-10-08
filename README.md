# RISC-V LLVM
## About
This repository adds support for various aspects of the RISC-V
instruction set to the Clang C/C++ compiler and LLVM back end.

The "master" branch follows official LLVM.

The "dev" branch at the moment contains mostly patches developed by the
[LowRISC project](https://github.com/lowRISC/riscv-llvm)
that have not yet been upstreamed to the official LLVM repository. It is updated from the
master branch regularly. As more patches are committed to upstream LLVM the differences will
become smaller, but the intention is to add other patches here so people can conveniently
use them in advance of them being accepted into upstream.

### How can I build LLVM+Clang and use it to cross-compile for a riscv target?

As well as this repository, you will need the RISC-V gcc toolchain. If you don't have RISC-V hardware then
you will want to have QEMU to run your programs.

The following bash commands have been tested on a fresh Ubuntu install
(e.g. Amazon EC2 Ubuntu 16.04 LTS ami-0e32ec5bc225539f5 and 18.04 LTS ami-0bbe6b35405ecebdb)
and are known to result in a working RISC-V LLVM.

If you already have recent RISC-V gnu tools and/or qemu in your PATH then you can omit the steps to
download and build those.

At the moment the clang driver does not successfully find the gnu linker and libraries so you
need to explicitly use gcc to link your object files. This should be fixed soon.

By default clang is generating rv32imac/rv64imac code but you can override this, and in particular
if you have floating point hardware you can add "-march=rv32gc" (or rv64gc) to the clang
command line to get FPU instructions with a soft float ABI (scalar FP function arguments and results
passed in integer registers).

    sudo apt-get update
    sudo apt-get -y dist-upgrade
    sudo apt-get -y install \
      binutils build-essential libtool texinfo \
      gzip zip unzip patchutils curl git \
      make cmake ninja-build automake bison flex gperf \
      grep sed gawk python bc \
      zlib1g-dev libexpat1-dev libmpc-dev \
      libglib2.0-dev libfdt-dev libpixman-1-dev 
    
    mkdir riscv
    cd riscv
    mkdir _install
    export PATH=`pwd`/_install/bin:$PATH

    # gcc, binutils, newlib
    git clone --recursive https://github.com/riscv/riscv-gnu-toolchain
    pushd riscv-gnu-toolchain
    ./configure --prefix=`pwd`/../_install --enable-multilib
    make -j`nproc`
    popd

    # qemu
    git clone https://github.com/riscv/riscv-qemu
    pushd riscv-qemu
    ./configure --prefix=`pwd`/../_install --target-list=riscv64-linux-user,riscv32-linux-user
    make -j`nproc` install
    popd

    # LLVM
    git clone https://github.com/sifive/riscv-llvm
    pushd riscv-llvm
    mkdir _build
    cd _build
    cmake -G Ninja -DCMAKE_BUILD_TYPE="Release" \
      -DBUILD_SHARED_LIBS=True -DLLVM_USE_SPLIT_DWARF=True \
      -DCMAKE_INSTALL_PREFIX="../../_install" \
      -DLLVM_OPTIMIZED_TABLEGEN=True -DLLVM_BUILD_TESTS=False \
      -DDEFAULT_SYSROOT="../../_install/riscv64-unknown-elf" \
      -DLLVM_DEFAULT_TARGET_TRIPLE="riscv64-unknown-elf" \
      -DLLVM_TARGETS_TO_BUILD="" -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD="RISCV" \
      ../llvm
    cmake --build . --target install
    popd

    # Sanity test your new RISC-V LLVM
    cat >hello.c <<END
    #include <stdio.h>
    
    int main(){
      printf("Hello RISCV!\n");
      return 0;
    }
    END
    
    # 32 bit
    clang -O -c hello.c --target=riscv32
    riscv64-unknown-elf-gcc hello.o -o hello -march=rv32imac -mabi=ilp32
    qemu-riscv32 hello
    
    # 64 bit
    clang -O -c hello.c
    riscv64-unknown-elf-gcc hello.o -o hello -march=rv64imac -mabi=lp64
    qemu-riscv64 hello

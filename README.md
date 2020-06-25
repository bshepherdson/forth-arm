# ForthARM

**Superceded:** My [FCC project](https://github.com/shepheb/fcc) is a much more complete, robust, standard and optimized Forth for ARMv7 32-bit and x86_64.

This is a Forth implementation for ARM processors. It was written from scratch and as a hobby project. It is currently not in a working state.

Feel free to use it (BSD3 license), but expect bugs.

## Building

Contains a Makefile, just run make to build the core assembly code in `forth.s`. `forth.fs` contains the higher-level implementations of things like control structures.

You'll need make and the gcc suite, either running on or cross-compiling for ARM.

I've written and tested it on ARMv6, for the Raspberry Pi. I don't think it uses any v6-specific instructions, and it doesn't use the FPU, so it should be portable to other ARM cores.

\ An implementation of the Z-machine version 3 for my Forth ARM.
\ (c) 2013 Braden Shepherdson

\ Overall design:
\ - The Z-machine has a main memory, stack, program counter (PC) and stack pointer (SP).
\   - These are all stored separately.
\   - These are found at the variables M0, STACKTOP, PC and SP.
\   - Care must be taken not to confuse the Z-machine SP with Forth's stack pointer.
\ - The Z-machine is a 16-bit big-endian machine; this interpreter runs on little-endian 32-bit ARM.
\   - Therefore reading and writing needs to be done carefully.
\   - ARM has LDRH for loading unsigned halfwords, and LDRSH for loading signed halfwords.
\     - These have been captured with the Forth words H@ and H!.
\     - To correct the endianness, we have REV16 (unsigned) and REVSH (signed)
\     - All of this requires that we have three loading and two saving functions:
\       RB, RW, RWS (signed), WB and WW.
\ - We can't read values properly before we can read the header, and check for things
\   like dynamic memory. Therefore functions to access the header will use raw loads.


\ Let's begin with some basic memory access words. These expect real, 32-bit addresses
\ and read or write the appropriately-sized word with the right byte order.
: RAW_BYTE ( ra -- b ) C@ ;
: RAW_WORD ( ra -- uw ) H@ BITSWAPH ;
: RAW_WORD_S ( ra -- sw ) H@S BITSWAPHS ;
: STORE_RAW_BYTE ( b ra -- ) C! ;
: STORE_RAW_WORD ( w ra -- ) BITSWAPH H! ;

\ Stores the real address of Z-machine address 0 on the heap.
VARIABLE M0

\ Code responsible for loading the story file
\ Takes a Forth string, copies it into a C string, and calls open(2)
: LOAD_STORY ( str len -- )
    HERE @ M0 ! \ Set M0 properly
    OVER + ( str len+HERE)
    0 SWAP C! \ write the null terminator. ( str )
    O_RDONLY SWAP OPEN \ fd
    \ TODO: Handle error (negative fd)
    BEGIN
        1024 OVER HERE @ SWAP ( fd len buf fd )
        READ ( fd read_bytes )
        ?DUP
    WHILE
        \ Move HERE forward by the amount read
        HERE +!
    REPEAT
    ( fd )
    CLOSE DROP ( )
;


\ These words give the locations of various items in the headers. They don't actually read the locations.
: 







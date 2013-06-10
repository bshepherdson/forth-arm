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

\ Debug helpers
: .. DUP . ;
: ..H HEX .. DECIMAL ;

\ Let's begin with some basic memory access words. These expect real, 32-bit addresses
\ and read or write the appropriately-sized word with the right byte order.
: RB  ( ra -- b ) C@ ;
: RW  ( ra -- uw ) H@ BITSWAPH ;
: RWS ( ra -- sw ) H@S BITSWAPHS ;
: WB  ( b ra -- ) C! ;
: WW  ( w ra -- ) BITSWAPH H! ;

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
\ All of these are constants that hold the byte address of a value in the header.
0  CONSTANT HDR_VERSION
1  CONSTANT HDR_FLAGS1
4  CONSTANT HDR_HIGH_MEM
6  CONSTANT HDR_PC0
8  CONSTANT HDR_DICTIONARY
10 CONSTANT HDR_OBJECT_TABLE
12 CONSTANT HDR_GLOBAL_VARIABLES
14 CONSTANT HDR_STATIC_MEM
16 CONSTANT HDR_FLAGS2
24 CONSTANT HDR_ABBREVIATIONS_TABLE
26 CONSTANT HDR_LENGTH
28 CONSTANT HDR_CHECKSUM
30 CONSTANT HDR_INTERPRETER_NUMBER
31 CONSTANT HDR_INTERPRETER_VERSION
32 CONSTANT HDR_SCREEN_HEIGHT
33 CONSTANT HDR_SCREEN_WIDTH
50 CONSTANT HDR_STANDARD_REVISION

\ Memory accessing words. These turn Z-machine addresses into real ones.
: BA ( ba -- ra ) M0 @ + ;
: WA ( wa -- ra ) 2 * BA ;
: PA ( pa -- ra ) WA ; \ v3 specific


\ Bit manipulation words
: BIT ( w n -- ? ) 1 SWAP << AND 0> ;
: SETBIT ( w n -- w ) 1 SWAP << OR ;
: CLEARBIT ( w n -- w )
    1 SWAP << INVERT \ This has resulted in, eg: ffff758c,
    -1 16 >> AND ;   \ So we mask off the upper 16 bits (0000758c)


\ Text encoding! D: So much complication for little gain.
\ Overall strategy:
\ - One word does the right thing for each 5-bit Z-character.
\ - One word decodes 16-bit words into 3 Z-characters and calls the first.
\ - One word for each of: shifting, ordinary characters, literals, abbreviations.
\ Metadata:
\ - A flag for a literal coming: LITERAL_ACTIVE
\ - A variable to hold the contents of a literal: LITERAL_CHAR
\ - A variable for the current alphabet: ALPHABET, which points to the function for printing that alphabet.
\ - A word that notes which abbreviation alphabet list is in play, or 0 for none.

VARIABLE LITERAL_ACTIVE
VARIABLE LITERAL_CHAR
VARIABLE ALPHABET
VARIABLE ABBREVIATION


\ Alphabet words: Take a 5-bit Z-character and print it
: ALPHABET0 ( zchar -- ) 91 + EMIT ; \ lower case letters
: ALPHABET1 ( zchar -- )
    59 + EMIT \ upper case letters
    LIT ALPHABET0 ALPHABET !
;

\ Symbol table for A2
VARIABLE A2_TABLE

: INIT_A2 ( table -- )
    DUP A2_TABLE !
    46 OVER C!      \ .
    44 OVER 1 + C!  \ ,
    33 OVER 2 + C!  \ !
    63 OVER 3 + C!  \ ?
    95 OVER 4 + C!  \ _
    35 OVER 5 + C!  \ #
    39 OVER 6 + C!  \ '
    34 OVER 7 + C!  \ "
    47 OVER 8 + C!  \ /
    92 OVER 9 + C!  \ \
    45 OVER 10 + C! \ -
    58 OVER 11 + C! \ :
    40 OVER 12 + C! \ (
    41 SWAP 13 + C! \ )
;
14 ALLOT \ Leaves the address on the stack
INIT_A2  \ Consumes it to populate the table.


\ Handle 7 (\n), digits (8-18) and symbols (19-31) separately
: ALPHABET2 ( zchar -- )
    DUP 6 = IF
        DROP
        1 LITERAL_ACTIVE !
        0 LITERAL_CHAR !
    ELSE DUP 7 = IF
        10 EMIT DROP
    ELSE DUP 18 < IF
        40 + EMIT \ 8 -> 48 = ASCII 0
    ELSE
        18 - A2_TABLE @ + C@ EMIT
    THEN THEN THEN
    ['] ALPHABET0 ALPHABET !
;

\ Handles a literal char
: PRINT_LITERAL ( zchar -- )
    LITERAL_ACTIVE @ CASE
    1 OF LITERAL_CHAR ! 2 LITERAL_ACTIVE ! ENDOF
    2 OF LITERAL_CHAR @ 5 << OR EMIT   0 LITERAL_ACTIVE ! 0 LITERAL_CHAR ! ENDOF
    ENDCASE
;

VARIABLE PRINT_STRING_FORWARD

: PRINT_ABBREVIATION ( zchar -- )
    2 *
    ABBREVIATION @ 0 ABBREVIATION ! ( zchar abbrev_row )
    \ Compute the offset into the abbreviation table.
    1- 64 * + \ ( offset )
    HDR_ABBREVIATIONS_TABLE ( offset ba_into_header )
    BA RW ( offset ba_of_table )
    + ( ba_of_entry )
    BA RW ( wa_of_string )
    WA PRINT_STRING_FORWARD @ EXECUTE
;


: INIT_STRINGS ( -- )
    FALSE LITERAL_ACTIVE !
    0 LITERAL_CHAR !
    LIT ALPHABET0 ALPHABET !
    0 ABBREVIATION !
; INIT_STRINGS


\ This is the big daddy: it takes a single Z-char and does the right thing.
: PRINT_ZCHAR ( zchar -- )
    ABBREVIATION @ 0> IF
        PRINT_ABBREVIATION
    ELSE
        LITERAL_ACTIVE @ IF
            PRINT_LITERAL
        ELSE
            DUP 5 > IF
                ALPHABET @ EXECUTE
            ELSE
                DUP 0= IF
                    DROP 32 EMIT
                ELSE
                    DUP 4 < IF
                        ABBREVIATION !
                    ELSE
                        4 = IF
                            LIT ALPHABET1 ALPHABET !
                        ELSE
                            LIT ALPHABET2 ALPHABET !
                        THEN
                    THEN
                THEN
            THEN
        THEN
    THEN
;

\ Takes a 16-bit word representing a 3 Z-character blocks, and returns those three
\ Z-characters, with a flag on top representing whether the end bit was set (true for set).
: DECODE_ZCHAR ( w -- c b a ? )
              DUP 31 AND  \ ( w c )
    SWAP 5 >> DUP 31 AND  \ ( c w b )
    SWAP 5 >> DUP 31 AND  \ ( c b w a )
    SWAP 5 >>             \ ( c b a ? )
;

\ Prints a Z-character word. Returns true if it should stop.
: PRINT_ZCHAR_WORD ( w -- ? )
    DECODE_ZCHAR >R \ set aside the flag. ( c b a )
    \ .. -ROT .. -ROT .. -ROT CR
    PRINT_ZCHAR
    PRINT_ZCHAR
    PRINT_ZCHAR
    R>
;

\ Takes a real address to a Z-character string. Prints the string.
: PRINT_STRING ( ra -- )
    BEGIN
        DUP RW \ ( ra w )
        PRINT_ZCHAR_WORD \ ( ra ? )
        NOT
    WHILE
        2 + ( ra -- ra' )
    REPEAT
    DROP ( )
    \ Reset the printing state.
    INIT_STRINGS
;

: INIT_PRINTING LIT PRINT_STRING PRINT_STRING_FORWARD ! ;
INIT_PRINTING



\ Testing
S" zmachine/Zork1.z3" LOAD_STORY
81944 BA PRINT_STRING

\ 5385
\ 0101 0011 1000 0101
\ 0 10100 11100 00101
\   20    28    5

\ 183b
\ 0001 1000 0011 1011
\ 0 00110 00001 11011
\   6     1     27

\ Yep, 5, 6, 1, 27 = literal, 00001 11011, that is, 00 0011 1011 = 0x3b = ASCII ;

\ 8020
\ 1000 0000 0010 0000
\ 1 00000 00001 00000
\ space, then abbrev 1,0: "the "


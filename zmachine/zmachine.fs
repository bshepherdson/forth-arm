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


: MIN ( a b -- min )
    2DUP < IF DROP ELSE NIP THEN
;

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

\ Returns the number of bytes long a string is.
: STRLEN ( ra -- uw )
    0
    BEGIN
        OVER RW 15 BIT NOT
    WHILE ( str len )
        2 + SWAP 2 + SWAP
    REPEAT
    NIP ( len )
;

: INIT_PRINTING LIT PRINT_STRING PRINT_STRING_FORWARD ! ;
INIT_PRINTING



\ Metadata for the interpreter lives in three Forth variables and the stack.
\ Allocating space for the stack. 1K words.
1024 CELLS DUP ALLOT +
VARIABLE STACKTOP
STACKTOP !

\ Use of the stack by routines:
\ SP points at the present stack value.
\ A frame pointer (FP) points at the frame data for the last routine call.
\ So with some extra values on the stack, we might have:

\ local4
\ local3
\ local2
\ local1
\ old_FP
\ old_SP
\ return_address <-- SP starts pointing here. FP points here.
\ some value
\ some value <-- SP points here now.

\ So local N is at FP + 4n + 8 (real address)
\ And on a return, old_FP, old_SP and the return address are loaded.


\ These are all real addresses.
VARIABLE PC
VARIABLE SP
VARIABLE FP

STACKTOP @ SP !
0 FP !
\ PC is set during startup.

\ Some convenience words for handling the PC.
: PC++ 1 PC +! ;
: PC+2 2 PC +! ;
\ Gets the byte at PC and advances PC by 1.
: PC@ ( -- b ) PC @ RB PC++ ;
: PC@W ( -- w ) PC @ RW PC+2 ;



\ Interpreters
\ Instruction encoding is a complex PITA.
\ Arguments come in three types: large constant, small constant, variable.
\ Generally, the arguments are delivered on the stack to the implementing function.
\ This is easy for 0OP, 1OP and 2OP instructions.
\ For VAR instructions, the number of arguments is the topmost item of the stack.

\ There is a master interpreter in ZINTERP. It computes the form of instruction
\ and hands off to ZINTERP_SHORT, ZINTERP_LONG, and ZINTERP_VARIABLE.
\ These sub-interpreters collect the argument types, and place them on the stack before
\ calling ZINTERP_0OP, ZINTERP_1OP, or ZINTERP_2OP.
\ Variable form and arg number are special.
\ Eventually the actual instruction handling functions are called.

\ Argument handlers
\ NB: These store values on the interpreter's stack in Z-machine order, not host order.
\ This is in case of programs trying to access them directly.
: PUSH ( val -- ) 4 SP -! SP @ WW ;
: POP ( -- val )  SP @ RW 4 SP +! ;

\ Turns a local number into a real address.
: LOCAL ( num -- ra ) 4 * 8 + FP @ + ;
\ Turns a global number into a real address.
: GLOBAL ( num -- ra ) 16 - 2 *    HDR_GLOBAL_VARIABLES BA RW   + BA ;

: SMALLCONSTANT ( -- val ) PC@ ;
: LARGECONSTANT ( -- val ) PC@W ;

: GETARG ( type -- arg )
    ." Getarg for type " .. CR
    CASE
    0 OF LARGECONSTANT ENDOF
    1 OF SMALLCONSTANT ENDOF
    2 OF
        SMALLCONSTANT ( var )
        DUP 0= IF
            DROP POP
        ELSE
            DUP 16 < IF
                LOCAL RW
            ELSE
                GLOBAL RW
            THEN
        THEN
    ENDOF
    ENDCASE
;

\ By default, variables and large constants are read unsigned.
\ Any instruction that wants to use signed values must call SIGN on its arguments.
: SIGN ( uw -- sw ) BITSWAPHS BITSWAPHS ;



\ Instruction helpers

\ Stores the value on the stack into the target given at PC.
: STORE ( val -- )
    PC@ ( val target -- )
    DUP 0= IF
        DROP PUSH
    ELSE
        DUP 16 < IF
            LOCAL WW
        ELSE
            GLOBAL WW
        THEN
    THEN
;


\ Returns from a routine.
\ Return address from the stack points at the last byte of the call instruction, which gives the storage byte for the return target.
: RETURN ( val -- )
    FP     @ PC !
    FP 4+  @ SP !
    FP 8 + @ FP !
    STORE
;


\ Branch to another instruction, if the flag matches the condition bit.
: ZBRANCH ( ? -- )
    0> \ make sure the flag is 0 or 1.
    PC@ ( ? byte1 )
    DUP 7 BIT ROT ( byte1 ? ? )
    = IF ( byte1 )
        \ Perform the branch
        \ Check for long vs. short branch offset
        DUP 6 BIT NOT ( byte1 long? )
        SWAP 63 AND SWAP ( offset_short long? )
        IF ( offset_short )
            \ Long form, load the second byte
            8 << PC@ OR ( offset )
        THEN ( offset )

        CASE
        0 OF 0 RETURN ENDOF
        1 OF 1 RETURN ENDOF

        2 - PC +!
        ENDCASE ( )
    ELSE ( byte1 )
        \ Not branching. Skip the second byte if necessary
        6 BIT NOT IF PC++ THEN
    THEN
;


\ Object support
\ v3 specific all up in this shiz

\ Returns unsigned values.
: PROP_DEFAULTS ( propnum -- value ) 1- 2 *   HDR_OBJECT_TABLE BA RW   + BA RW ;

: OBJ_TABLE_TOP ( -- ra ) HDR_OBJECT_TABLE BA RW 62 + BA ;

: OBJ_ADDR ( objnum -- ra ) 1- 9 * OBJ_TABLE_TOP + ;

: PARENT  ( objnum -- ra ) OBJ_ADDR 4+ ;
: SIBLING ( objnum -- ra ) OBJ_ADDR 5 + ;
: CHILD   ( objnum -- ra ) OBJ_ADDR 6 + ;

\ Address of the table, not the pointer.
\ Actually points at the length byte for the short name.
: OBJ_PROP_TABLE ( objnum -- ra ) OBJ_ADDR 7 + RW ;

: OBJ_SHORT_NAME ( objnum -- ra ) OBJ_PROP_TABLE 1+ ;

\ address of the size byte of the first property
: OBJ_PROP_TOP ( objnum -- ra )
    OBJ_PROP_TABLE DUP RB ( table len )
    2 * 1+
;

: PROP_SIZE ( prop_ra -- size_in_bytes )
    RB 5 >> 1+
;

: PROP_DATA ( prop_ra -- ra' ) 1+ ;

: PROP_NUM ( prop_ra -- n ) RB 31 AND ;

: PROP_NEXT ( prop_ra -- ra' )
    DUP PROP_SIZE ( ra size )
    1+ + ( ra' )
;

\ Returns the address of the size byte, or 0 if the object doesn't have it.
: FIND_PROP ( prop obj -- ra )
    OBJ_PROP_TOP ( prop ra )
    SWAP ( ra prop )
    BEGIN
        OVER PROP_NUM ( ra prop num )
        OVER < ( ra prop ? )
    WHILE ( ra prop ) \ loops while the found number is larger
        SWAP PROP_NEXT SWAP
    REPEAT ( ra prop )
    \ At this point, we've either found the prop or missed it.
    OVER PROP_NUM ( ra prop num )
    OVER = IF ( ra prop )
        DROP ( ra )
    ELSE ( ra prop )
        2DROP 0
    THEN
;



\ Takes an xt for the function to apply on the attr
: ATTR_DO ( obj attr xt[ byte bit -- byte' ] -- )
    >R \ set aside the xt
    DUP 3 >> ( obj attr byte )
    ROT ( attr byte obj )
    OBJ_ADDR ( attr byte ra )
    + ( attr ra )
    SWAP 31 SWAP - ( ra bit )
    7 AND ( ra bit )
    OVER RB SWAP ( ra byte bit )
    R> EXECUTE ( ra byte' )
    SWAP WB ( )
;
: CLEAR_ATTR ( obj attr -- ) ['] CLEARBIT ATTR_DO ;
: SET_ATTR ( obj attr -- ) ['] SETBIT ATTR_DO ;

: TEST_ATTR ( obj attr -- ? )
    DUP 3 >> ( obj attr byte )
    ROT OBJ_ADDR ( attr byte ra )
    + ( attr ra )
    SWAP 31 SWAP - ( ra bit )
    7 AND ( ra bit )
    SWAP RB ( bit byte )
    SWAP BIT ( ? )
;


\ Counting up all the APIs for objects that I want.
\ SIBLING/CHILD/PARENT: obj num -> address of that cell
\ property length from property address
\ remove an object from its parent's child chain
\ print an object's short description
\ testing, setting and clearing attrs (objnum, attr num)
\ jin containment
\ inserting an object into a sibling chain
\ retrieve a property (obj, prop)
\ retrieve a property address (obj, prop)
\ find the next property (obj, prop)
\ put a property (obj, prop, value)
\

\ 0OP instructions
: 0OP_RTRUE ( -- ) 1 RETURN ;

: 0OP_RFALSE ( -- ) 0 RETURN ;

: 0OP_PRINT ( -- )
    PC @ ( str )
    DUP PRINT_STRING ( str )
    STRLEN PC +! ( )
;

: 0OP_PRINT_RET ( -- )
    0OP_PRINT
    1 RETURN
;

: 0OP_NOP ;

\ TODO - Implement save and restore.
: 0OP_SAVE ZBRANCH ;
: 0OP_RESTORE ;

VARIABLE RESTART_FORWARD
: 0OP_RESTART RESTART_FORWARD @ EXECUTE ;

: 0OP_RET_POPPED POP RETURN ;

: 0OP_POP POP DROP ;

: 0OP_QUIT PROCESS_EXIT ;

: 0OP_NEW_LINE CR ;

\ TODO - Implement
: 0OP_SHOW_STATUS ;

\ TODO - Implement for real
: 0OP_VERIFY 1 ZBRANCH ;

14 CELLS ARRAY OPS_0OPS
: INIT_0OPS ( -- )
    ['] 0OP_RTRUE       0  OPS_0OPS !
    ['] 0OP_RFALSE      1  OPS_0OPS !
    ['] 0OP_PRINT       2  OPS_0OPS !
    ['] 0OP_PRINT_RET   3  OPS_0OPS !
    ['] 0OP_NOP         4  OPS_0OPS !
    ['] 0OP_SAVE        5  OPS_0OPS !
    ['] 0OP_RESTORE     6  OPS_0OPS !
    ['] 0OP_RESTART     7  OPS_0OPS !
    ['] 0OP_RET_POPPED  8  OPS_0OPS !
    ['] 0OP_POP         9  OPS_0OPS !
    ['] 0OP_QUIT        10 OPS_0OPS !
    ['] 0OP_NEW_LINE    11 OPS_0OPS !
    ['] 0OP_SHOW_STATUS 12 OPS_0OPS !
    ['] 0OP_VERIFY      13 OPS_0OPS !
; INIT_0OPS

: ZINTERP_0OP ( opcode -- )
    176 - OPS_0OPS @ EXECUTE
;



\ 1OP instructions
: 1OP_JZ ( arg -- ) 0= ZBRANCH ;

: 1OP_GET_SIBLING ( arg -- ) SIBLING RB DUP STORE 0> ZBRANCH ;
: 1OP_GET_CHILD   ( arg -- ) CHILD   RB DUP STORE 0> ZBRANCH ;
: 1OP_GET_PARENT  ( arg -- ) PARENT  RB DUP STORE 0> ZBRANCH ;

: 1OP_GET_PROP_LEN ( arg -- ) PROP_LEN STORE ;

: INCDEC ( var amount -- amount' )
    SWAP ( amount var )
    DUP 0= IF
        DROP POP + DUP PUSH ( amount' )
    ELSE
        DUP 16 < IF LOCAL ELSE GLOBAL THEN ( amount ra )
        DUP ( amount ra ra )
        RWS ( amount ra val )
        ROT + ( ra val' )
        OVER WW ( val' )
    THEN
;

: 1OP_INC ( arg -- )  1 INCDEC DROP ;
: 1OP_DEC ( arg -- ) -1 INCDEC DROP ;

: 1OP_PRINT_ADDR ( ba -- ) BA PRINT_STRING ;

\ 1OP_CALL_1S

: 1OP_REMOVE_OBJ ( obj -- )
    \ Note this object number and then move up to the parent.
    DUP PARENT RB ( obj parent )
    DUP 0> IF \ do nothing if this object is parentless
        DUP CHILD RB ( obj parent child )
        ROT ( parent child obj )
        2DUP = IF ( p c o )
            NIP ( p o )
            DUP SIBLING RB ( p o s )
            ROT CHILD ( o s p_ra )
            WB ( o )
            0 OVER SIBLING WB ( o )
            0 SWAP PARENT WB ( )
        ELSE ( p c o )
            \ loop through the sibling chain until we find this object
            >R ( p c )
            BEGIN
                DUP SIBLING RB ( p c s )
                R> DUP >R ( p c s o )
                OVER <>
            WHILE ( p c s )
                \ No match, keep following the links
                NIP DUP SIBLING RB ( p c' s' )
            REPEAT
            \ ( p c s ) Now s == o, so complete the link.
            SIBLING RB ( p c s2 )
            SWAP ( p s2 c )
            SIBLING WB ( p )
            DROP ( )
        THEN
    ELSE ( obj parent )
        2DROP
    THEN
;

: 1OP_PRINT_OBJ ( obj -- )
    OBJ_SHORT_NAME PRINT_STRING
;

: 1OP_RETURN ( val -- ) RETURN ;

\ Address is an offset to apply to the PC. Treat it as signed.
: 1OP_JUMP ( offset -- )
    SIGN PC +!
;

: 1OP_PRINT_PADDR ( pa -- ) PA PRINT_STRING ;

: 1OP_LOAD ( var -- )
    DUP 0= IF
        DROP POP
    ELSE
        DUP 16 < IF LOCAL ELSE GLOBAL THEN
        RW
    THEN
    STORE
;

: 1OP_NOT ( val -- )
    INVERT STORE
;

16 CELLS ARRAY OPS_1OPS
: INIT_1OPS ( -- )
    ['] 1OP_JZ                   0  OPS_1OPS !
    ['] 1OP_GET_SIBLING          1  OPS_1OPS !
    ['] 1OP_GET_CHILD            2  OPS_1OPS !
    ['] 1OP_GET_PARENT           3  OPS_1OPS !
    ['] 1OP_GET_PROP_LEN         4  OPS_1OPS !
    ['] 1OP_INC                  5  OPS_1OPS !
    ['] 1OP_DEC                  6  OPS_1OPS !
    ['] 1OP_PRINT_ADDR           7  OPS_1OPS !
    \ ['] 1OP_CALL_1S  8  OPS_1OPS !
    ['] 1OP_REMOVE_OBJ           9  OPS_1OPS !
    ['] 1OP_PRINT_OBJ            10 OPS_1OPS !
    ['] 1OP_RET                  11 OPS_1OPS !
    ['] 1OP_JUMP                 12 OPS_1OPS !
    ['] 1OP_PRINT_PADDR          13 OPS_1OPS !
    ['] 1OP_LOAD                 14 OPS_1OPS !
    ['] 1OP_NOT                  15 OPS_1OPS !
; INIT_1OPS


: ZINTERP_1OP ( arg opcode -- )
    128 - OPS_1OPS @ EXECUTE
;


\ 2OP instructions

\ 0 does not exist
: 2OP_JE ( b a -- )
    = ZBRANCH
;

\ These look backwards but aren't. Remember that the stack has arg1 on top.
: 2OP_JL ( b a -- )
    SIGN SWAP SIGN SWAP
    > ZBRANCH
;

: 2OP_JG ( b a -- )
    SIGN SWAP SIGN SWAP
    < ZBRANCH
;

: 2OP_DEC_CHK ( val var -- )
    SWAP SIGN SWAP
    DUP 0= IF ( val var )
        DROP POP 1- DUP PUSH ( val val' )
    ELSE ( val var )
        DUP 16 < IF LOCAL ELSE GLOBAL THEN ( val ra )
        DUP RWS 1- ( val ra val' )
        DUP ROT ( val val' val' ra )
        WW ( val val' )
    THEN
    > ZBRANCH
;

: 2OP_INC_CHK
    SWAP SIGN SWAP
    DUP 0= IF ( val var )
        DROP POP 1+ DUP PUSH ( val val' )
    ELSE ( val var )
        DUP 16 < IF LOCAL ELSE GLOBAL THEN ( val ra )
        DUP RWS 1+ ( val ra val' )
        DUP ROT ( val val' val' ra )
        WW ( val val' )
    THEN
    < ZBRANCH
;

: 2OP_JIN ( o2 o1 -- )
    PARENT RB = ZBRANCH
;

\ Branch if all the flags in the bitmap are set. That is, if bitmap & flags == flags
: 2OP_TEST ( flags bitmap -- )
    OVER AND = ZBRANCH
;

: 2OP_OR ( b a -- ) OR STORE ;
: 2OP_AND ( b a -- ) AND STORE ;

: 2OP_TEST_ATTR ( attr obj -- )
    SWAP TEST_ATTR ZBRANCH
;

: 2OP_SET_ATTR ( attr obj -- ) SWAP SET_ATTR ;
: 2OP_CLEAR_ATTR ( attr obj -- ) SWAP CLEAR_ATTR ;

: 2OP_STORE ( val var -- )
    DUP 0= IF
        DROP PUSH
    ELSE
        DUP 16 < IF LOCAL ELSE GLOBAL THEN
        WW
    THEN
;

: 2OP_INSERT_OBJ ( dest obj -- )
    2DUP PARENT WB \ set the parent of obj to dest ( d o )
    OVER CHILD RB ( d o c )
    OVER SIBLING WB \ set the sibling of obj to the child of dest ( d o )
    SWAP CHILD WB \ and set o as the child of d ( )
;

: 2OP_LOADW ( word-index array -- ) 2 * + BA RW STORE ;
: 2OP_LOADB ( byte-index array -- ) + BA RB STORE ;

: 2OP_GET_PROP ( prop obj -- )
    2DUP FIND_PROP ( prop obj ra_of_size_byte )
    DUP 0= IF ( p o ra )
        2DROP PROP_DEFAULTS ( val )
    ELSE ( prop obj ra )
        -ROT 2DROP ( ra )
        DUP PROP_SIZE ( ra size )
        CASE
        1 OF PROP_DATA RB ENDOF
        2 OF PROP_DATA RW ENDOF
        ENDCASE ( val )
    THEN
;

: 2OP_GET_PROP_ADDR ( prop obj -- )
    FIND_PROP ( ra )
    DUP 0> IF
        PROP_DATA ( ra )
        M0 @ - ( ba )
    THEN ( ba )
    STORE
;

: 2OP_GET_NEXT_PROP ( prop obj -- )
    OVER 0= IF ( prop obj ) \ If given 0, return the first prop
        NIP ( obj )
        OBJ_PROP_TOP ( ra )
        PROP_NUM ( prop )
    ELSE ( prop obj )
        FIND_PROP ( ra )
        PROP_NEXT ( ra )
        PROP_NUM ( num )
    THEN
    STORE
;

: 2OP_ADD ( b a -- )
    SIGN SWAP SIGN
    + STORE
;

: 2OP_SUB ( b a -- )
    SIGN SWAP SIGN ( a b )
    - STORE
;

: 2OP_MUL ( b a -- )
    SIGN SWAP SIGN ( a b )
    * STORE
;

\ TODO - fix division for negative operands
: 2OP_DIV ( b a -- )
    SIGN SWAP SIGN ( a b )
    / STORE
;

: 2OP_MOD ( b a -- )
    SIGN SWAP SIGN ( a b )
    MOD STORE
;


25 CELLS ARRAY OPS_1OPS
: INIT_2OPS ( -- )
    \ No 0
    ['] 2OP_JE                   1  OPS_2OPS !
    ['] 2OP_JL                   2  OPS_2OPS !
    ['] 2OP_JG                   3  OPS_2OPS !
    ['] 2OP_DEC_CHK              4  OPS_2OPS !
    ['] 2OP_INC_CHK              5  OPS_2OPS !
    ['] 2OP_JIN                  6  OPS_2OPS !
    ['] 2OP_TEST                 7  OPS_2OPS !
    ['] 2OP_OR                   8  OPS_2OPS !
    ['] 2OP_AND                  9  OPS_2OPS !
    ['] 2OP_TEST_ATTR            10 OPS_2OPS !
    ['] 2OP_SET_ATTR             11 OPS_2OPS !
    ['] 2OP_CLEAR_ATTR           12 OPS_2OPS !
    ['] 2OP_STORE                13 OPS_2OPS !
    ['] 2OP_INSERT_OBJ           14 OPS_2OPS !
    ['] 2OP_LOADW                15 OPS_2OPS !
    ['] 2OP_LOADB                16 OPS_2OPS !
    ['] 2OP_GET_PROP             17 OPS_2OPS !
    ['] 2OP_GET_PROP_ADDR        18 OPS_2OPS !
    ['] 2OP_GET_NEXT_PROP        19 OPS_2OPS !
    ['] 2OP_ADD                  20 OPS_2OPS !
    ['] 2OP_SUB                  21 OPS_2OPS !
    ['] 2OP_MUL                  22 OPS_2OPS !
    ['] 2OP_DIV                  23 OPS_2OPS !
    ['] 2OP_MOD                  24 OPS_2OPS !
; INIT_2OPS



: ZINTERP_2OP ( arg2 arg1 opcode -- )
    OPS_2OPS @ EXECUTE
;



\ VAR opcodes

: VAR_CALL ( args... routine n -- )
    1- SWAP ( args... argc routine )
    DUP 0= IF ( args... argc routine )
        DROP
        BEGIN
            DUP 0>
        WHILE
            NIP 1-
        REPEAT ( argc )
        DROP ( )
        0 STORE \ return false
    ELSE ( args... argc routine )
        PA ( ... routine_ra )
        DUP RB ( ... ra nLocals )
        SP @ ( ... ra nLocals sp )
        BEGIN
            OVER 0>
        WHILE ( ... ra nLocals sp )
            4- -ROT ( .. sp ra nLocals )
            1- 2DUP ( ... sp ra nl ra nl )
            2 * SWAP 1+ + ( ... sp ra_routine nl ra_local )
            RW ( ... sp ra_routine nl local )
            3 PICK ( ... sp ra nl local sp )
            ! ( ... sp ra nl )
            ROT ( ... ra nl sp )
        REPEAT
        \ Now all the locals are copied. The sp on the stack points at the last one.
        \ Now we store the various pointers.
        4- FP @ ( ... ra nl sp' fp )
        OVER ! ( ... ra nl sp' )
        4- SP @ ( ... ra nl sp'' sp )
        OVER ! ( ... ra nl sp'' )
        4- PC @ ( ... ra nl sp''' pc )
        OVER ! ( ... ra nl sp''' )
        DUP FP ! \ store the new sp as the new FP
        SP ! \ and write it to SP ( ... ra nl )

        \ At this point, the stack is fully set up for the call,
        \ except for writing the arguments into the stack.
        ( args... argc routine_ra nLocals )
        \ First, determine the minimum of nLocals and argc.
        ROT ( args... ra nl argc )
        MIN ( args... ra argc' )

        SWAP >R ( args... argc' )

        FP @ 12 + \ points at the first local ( args... argc local1_ra )
        SWAP  ( args... l1_ra argc )
        BEGIN
            DUP 0>
        WHILE
            1- >R ( args... l1_ra )
            2DUP ( args... l1_ra arg ra )
            ! ( leftovers... ra )
            4+ ( leftovers... ra' )
            R> ( leftovers... ra' argc' )
        REPEAT
        \ All arguments are now copied into the locals.
        ( ra' argc' )
        2DROP
        R> ( routine_ra )
        DUP RB ( ra nLocals )
        2 * 1+ + ( ra_first_code )
        PC ! ( )
        \ Return handles the storing, in this case.
    THEN
;





: ZINTERP_VAR ( args... n opcode -- )
    ." VAR: " .
    DUP . ." args: "
    BEGIN
        DUP 0>
    WHILE
        SWAP . 1-
    REPEAT
    CR
;


: ZINTERP_SHORT ( opcode -- )
    DUP 4 >> 3 AND DUP 3 = IF ( opcode type )
        DROP ( opcode )
        ZINTERP_0OP
    ELSE
        GETARG SWAP ( arg opcode )
        ZINTERP_1OP
    THEN
;


\ Retrieves arguments based on the long form types.
\ 0 becomes 1 for small constant, 1 becomes 2 for variable.
: LONGARG ( type -- arg ) 1+ GETARG ;

\ Operand count is always 2OP in long form.
\ Bit 6 gives the first type, bit 5 the second. 0 = small constant, 1 = variable.
: ZINTERP_LONG ( opcode -- )
    DUP 6 BIT ( opcode type1 )
    LONGARG   ( opcode arg1 )
    OVER 5 BIT ( opcode arg1 type2 )
    LONGARG    ( opcode arg1 arg2 )
    SWAP ROT   ( arg2 arg1 opcode )
    ZINTERP_2OP
;

\ Operand count is either VAR or 2OP for variable form.
\ We resolve this by computing it as though it were VAR, and then checking the opcode's
\ bit 5. If it is 0, we drop the arg count and call ZINTERP_2OP, otherwise ZINTERP_VAR.
: ZINTERP_VARIABLE ( opcode -- )
    >R 0 >R PC@ ( typebyte RR opcode argc )
    DUP 6 >> DUP 3 < IF ( typebyte type1 )
        R> 1+ >R \ increment argc
        GETARG ( typebyte arg1 )
        SWAP   ( arg1 typebyte )

        DUP 4 >> 3 AND DUP 3 < IF ( arg1 typebyte type2 )
            R> 1+ >R \ inc argc
            GETARG ( arg1 typebyte arg2 )
            -ROT   ( arg2 arg1 typebyte )

            DUP 2 >> 3 AND DUP 3 < IF ( arg2 arg1 typebyte type3 )
                R> 1+ >R \ inc argc
                GETARG ( arg2 arg1 typbyte arg3 )
                SWAP >R ( arg2 arg1 arg3 )
                -ROT    ( arg3 arg2 arg1 )
                R>      ( arg3 arg2 arg1 typebyte )

                3 AND DUP 3 < IF ( arg3 arg2 arg1 type4 )
                    R> 1+ >R \ inc argc
                    GETARG  ( arg3 arg2 arg1 arg4 )
                    SWAP >R ( arg3 arg2 arg4 )
                    -ROT    ( arg4 arg3 arg2 )
                    R>      ( arg4 arg3 arg2 arg1 )
                    0 0     \ dummy typebyte and type for the below ( a4 a3 a2 a1 dummy1 dummy2 )
                THEN
            THEN
        THEN
    THEN
    DROP DROP \ drop the typebyte: ( args... RR opcode argc )
    R> R> \ retrieve the other values: ( args.. argc opcode )

    DUP BIT 5 IF
        \ Bit 5 is set, this is a VAR instruction.
        ZINTERP_VAR
    ELSE
        \ This is a 2OP instruction. Drop the count and call out.
        NIP ( arg2 arg1 opcode )
        ZINTERP_2OP
    THEN
;



\ The master interpreter.
: ZINTERP ( -- )
    PC@
    DUP 6 >>
    DUP 3 = IF
        DROP ZINTERP_VARIABLE
    ELSE
        2 = IF
            ZINTERP_SHORT
        ELSE
            ZINTERP_LONG
        THEN
    THEN
;


\ Testing
S" zmachine/Zork1.z3" LOAD_STORY

8 CELLS ALLOT
DUP FP !
16 + 12 BITSWAPH SWAP ! \ store 18 in Local 1

20159 BA PC ! ZINTERP



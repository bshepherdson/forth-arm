\ 0 - Introduction
\ Z-MACHINE INTERPRETER
\ (c) 2012 Braden Shepherdson
\ Version 1
\ 
\ This is a Z-machine
\ interpreter that can handle
\ Version 3 game files.
\ It expects a game file loaded
\ on disk. It will not destroy
\ the disk; no writes are done.

\ 1 LOAD to launch.



\ 1 - Master Loader
117 LOAD \ Debugging utils
2 LOAD \ Constants
3 LOAD \ Global data
4 LOAD \ Stack
5 LOAD \ Memory Helpers
52 LOAD \ Misc helpers
23 LOAD \ Header helpers
28 LOAD \ PC@ and PC@W
10 LOAD \ Strings and print
29 LOAD \ Opcodes and instrs.
112 LOAD \ Game loading
115 LOAD \ Welcome message



\ 2 - Constants
16384 CONSTANT MEMBASE














\ 3 - Global interpreter data
VARIABLE PC  \ BA in block
VARIABLE PCBLOCK \ Disk block
: PC+ ( n -- )
  PC @ +
  DUP 1023 > IF ( pc' )
    1 PCBLOCK +!
    1024 - PC !
  ELSE ( pc' )
    DUP 0< IF
      -1 PCBLOCK +!
      1024 + PC !
    ELSE ( pc' ) PC ! THEN
  THEN 
;
: PC++ ( -- ) 1 PC+ ;
\ 4 - Stack
VARIABLE SP
MMR SP !

: PUSH ( val -- )
  SP @ 1- TUCK ( sp val sp -- )
  ! SP ! ;

: POP ( -- val )
  SP @ DUP @ SWAP 1+ SP ! ;

VARIABLE FP




\ 5 - Memory Helpers
6 LOAD \ Static
27 LOAD \ BA, WA, PA
7 LOAD \ RB WB
8 LOAD \ RWBA WWBA RWWA WWWA
9 LOAD \ Bit handlers










\ 6 - STATIC
\ Necessary to allow running
\ BA, WA, PA before header is
\ loaded.
VARIABLE STATIC_PTR

: FAKE_STATIC_IMPL -1 ;
: INIT_FAKE_STATIC
  ' FAKE_STATIC_IMPL
  STATIC_PTR !
;
INIT_FAKE_STATIC

: STATIC ( -- ba )
  STATIC_PTR @ EXECUTE ;

\ 7 - Memory helpers 2
: RB ( ba -- val )
  DUP BA @ SWAP ( ra ba )
  1 AND ( val odd? )
  IF 8 >> THEN
  255 AND
;
: RBBA RB ;

: WB ( val ba -- )
  DUP 1 XOR RB ( val ba other )
  ROT 255 AND ( ba other val )
  2 PICK 1 AND NOT IF SWAP THEN
  8 << OR SWAP BA !
;
: WBBA WB ;
\ 8 - Memory helpers 3
\ Swaps endianness
: ENDSWAP ( val -- val)
  DUP 8 <<
  SWAP 8 >> OR
;
: RWBA ( ba -- val )
  DUP RB 8 << SWAP 1+ RB OR ;
: RWWA ( wa -- val )
  WA @ ENDSWAP ;

: WWBA ( val ba -- )
  OVER 255 AND OVER !
  SWAP 8 >> SWAP ! ;
: WWWA ( val wa -- )
  SWAP ENDSWAP SWAP WA ! ;
\ 9 - Bit handlers
: BIT ( val bit -- ? )
  >> 1 AND ;
: SETBIT ( val bit -- val )
  1 SWAP << OR ;
: CLEARBIT ( val bit -- val )
  1 SWAP << XOR ;


\ v3 specific
: RBPA ( pa -- val )
  RWWA 8 >> 255 AND ;

: RWPA ( pa - val )
  RWWA ;

\ 10 - Strings and Printing
11 LOAD \ ENCODE and DECODE
12 LOAD \ Alphabets 1
13 LOAD \ Alphabets 2
14 LOAD \ Alphabets 3
15 LOAD \ Printing literals
16 LOAD \ Printing abbrevs
17 LOAD \ Printing Shift+Norm
18 LOAD \ PRINT_CHAR
19 LOAD \ PRINT
20 LOAD \ PRINT_WA
21 LOAD \ PRINT_PA
22 LOAD \ STRLEN



\ 11 - ENCODE and DECODE
: DECODE ( abc -> c b a )
  DUP 31 AND SWAP ( c abc )
  DUP 5 >> 31 AND SWAP
  10 >> 31 AND
;

: ENCODE ( a b c -> abc )
  SWAP 5  << OR ( a bc )
  SWAP 10 << OR ( abc )
;





\ 12 - Alphabets 1
25 ARRAY ALPHABET3_
NL 0 ALPHABET3_ ! \ newline
49 1 ALPHABET3_ ! \ 0
50 2 ALPHABET3_ ! \ 1
51 3 ALPHABET3_ ! \ 2
52 4 ALPHABET3_ ! \ 3
53 5 ALPHABET3_ ! \ 4
54 6 ALPHABET3_ ! \ 5
55 7 ALPHABET3_ ! \ 6
56 8 ALPHABET3_ ! \ 7
57 9 ALPHABET3_ ! \ 8
58 10 ALPHABET3_ ! \ 9
46 11 ALPHABET3_ ! \ .
44 12 ALPHABET3_ ! \ ,
33 13 ALPHABET3_ ! \ !
\ 13 - Alphabets 2
63 14 ALPHABET3_ ! \ ?
95 15 ALPHABET3_ ! \ _
35 16 ALPHABET3_ ! \ #
39 17 ALPHABET3_ ! \ '
34 18 ALPHABET3_ ! \ "
47 19 ALPHABET3_ ! \ /
92 20 ALPHABET3_ ! \ \
45 21 ALPHABET3_ ! \ -
58 22 ALPHABET3_ ! \ :
40 23 ALPHABET3_ ! \ (
41 24 ALPHABET3_ ! \ )
: ALPHABET3 ( zscii -- ascii )
  7 - ALPHABET3_ @ ;


\ 14 - Alphabets 3
VARIABLE ALPHABET 0 ALPHABET !

VARIABLE LIT_CHAR
VARIABLE LITERAL_MODE
0 LITERAL_MODE !










\ 15 - Printing literals
\ No-op when not a literal.
: PRINT_LITERAL ( chr --chr! ?)
  LITERAL_MODE @
  DUP 1 = IF DROP LIT_CHAR ! 1
    2 LITERAL_MODE !
  ELSE 2 = IF
    LIT_CHAR @ 5 <<
    OR EMIT 0 LITERAL_MODE ! 1
  ELSE
    DUP 6 = ALPHABET @ 2 = AND
    IF
      DROP 1 LITERAL_MODE ! 1
    ELSE 0 THEN
  THEN THEN
;
\ 16 - Printing abbreviations
VARIABLE ABBREVIATING
0 ABBREVIATING !
\ Hack for forward declaration
VARIABLE FAKE_PRINT_WA
: PRINT_ABBREVIATION
  ABBREVIATING @ DUP 0> IF
    0 ABBREVIATING !
    1- 32 * + 1 << ABBREVS +
    RWBA FAKE_PRINT_WA @
    EXECUTE 1
  ELSE
    DROP DUP 0 > OVER 4 < AND
    IF ABBREVIATING ! 1
    ELSE 0 THEN
  THEN ;
\ 17 - Printing Shift + Normal
: PRINT_SHIFT ( chr -- chr! ? )
  DUP 4 = OVER 5 = OR DUP 
  IF ( chr ? )
    SWAP 3 - ALPHABET !
  THEN
;

\ TODO: More elegant.
: PRINT_NORMAL ( chr -- )
  ALPHABET @ CASE
    0 OF 91 + ENDOF
    1 OF 59 + ENDOF
    2 OF ALPHABET3 ENDOF
  ENDCASE EMIT 0 ALPHABET ! ;

\ 18 - PRINT_CHAR
\ These helpers either handle
\ things and BAIL or leave the
\ chr on the stack.
: PRINT_CHAR ( chr -- )
  PRINT_LITERAL
  IF EXIT THEN
  PRINT_ABBREVIATION
  IF EXIT THEN
  PRINT_SHIFT
  IF EXIT THEN
  DUP 0 = IF
    32 EMIT DROP
  ELSE PRINT_NORMAL THEN
;

\ 19 - Print
: PRINT ( ba -- )
  BEGIN
    DUP RWBA DUP DECODE
    PRINT_CHAR PRINT_CHAR
    PRINT_CHAR ( ba word )
    15 BIT SWAP 2 + SWAP
  UNTIL
  0 ALPHABET ! 0 ABBREVIATING !
  0 LITERAL_MODE !
  DROP
;




\ 20 - PRINT_WA
\ D: duplication
: PRINT_WA ( wa -- )
  BEGIN
    DUP RWWA DUP DECODE
    PRINT_CHAR PRINT_CHAR
    PRINT_CHAR ( wa word )
    15 BIT SWAP 1+ SWAP
  UNTIL
  0 ALPHABET ! 0 ABBREVIATING !
  0 LITERAL_MODE !
  DROP ;

: INIT_FAKE_PRINT_WA ( -- )
  ' PRINT_WA FAKE_PRINT_WA ! ;
INIT_FAKE_PRINT_WA
\ 21 - PRINT_PA
: PRINT_PA
  PRINT_WA \ TODO: v3 specific
;












\ 22 - STRLEN
\ Returns the number of WORDS
\ a string is long.
: STRLEN ( ba -- len )
  0 BEGIN
    OVER RWBA 15 BIT ( ba n ? )
    ROT 2 + ( n ? ba' )
    ROT 1+  ( ? ba' n+1 )
    ROT ( ba' n+1 ? )
  UNTIL
  NIP
;




\ 23 - Header handlers
24 LOAD \ Header handlers
25 LOAD \ Header handlers 2













\ 24 - Header handlers 1
: VERSION ( -- Z-mach-version )
  0 RB
;
: FLAGS1 ( -- ba ) 1 ;
: HIMEM ( -- ba ) 4 RWBA ;
: PC0 ( -- pc_ba ) 6 RWBA ;
: DICT ( -- ba ) 8 RWBA ;
: OBJTABLE ( -- ba ) 10 RWBA ;
: GLOBALS ( -- ba ) 12 RWBA ;

: FLAGS2 ( -- ba ) 16 RWBA ;
: ABBREVS ( -- ba ) 24 RWBA ;
: LENGTH ( -- len ) 26 RWBA ;


\ 25 - Header handlers 2
VARIABLE STATIC_ADDR

: REAL_STATIC ( -- ba )
  STATIC_ADDR @ ;

: INIT_STATIC ( -- )
  14 RWBA STATIC_ADDR !
  ' REAL_STATIC STATIC_PTR !
;






\ 26 - Blank















\ 27 - BA, WA and PA
: BA ( ba -- ra )
  DUP STATIC <U IF
    1 >> MEMBASE
  ELSE
    DUP 10 >> READBLOCK
    1 >> 511 AND MMR
  THEN + ;
: WA ( wa -- ra )
  DUP STATIC 1 >> <U
  IF MEMBASE ELSE
    DUP 9 >> READBLOCK
    511 AND MMR
  THEN + ;
\ TODO: v3 specific
: PA ( pa -- ra ) WA ;
\ 28 - PC@ and PC@W
: PC@ ( -- byte )
  PCBLOCK @ READBLOCK
  MMR PC @ 1 >> + @
  PC @ 1 AND IF 8 >> THEN
  255 AND
;

: PC@W ( -- word )
  PC@ 8 << PC++ PC@ OR -1 PC+
;





\ 29 - Opcodes and instruction
30 LOAD \ Argument handling
31 LOAD \ Argument handling 2
33 LOAD \ Argtypes
49 LOAD \ Store
32 LOAD \ Return
36 LOAD \ Instructions loader
44 LOAD \ Interpreter loader








\ 30 - Argument handling
: LARGECONST ( -- val )
  PC@W 2 PC+ ;
: SMALLCONST ( -- val )
  PC@ PC++ ;

: LOCAL ( var -- loc )
  4 + FP @ + ;
: LOCAL@ ( var -- val )
  LOCAL @ ;
: LOCAL! ( val var -- )
  LOCAL ! ;
: GLOBAL ( var -- loc )
  16 - 2 * GLOBALS + ;
: GLOBAL@ ( var -- val )
  GLOBAL RWBA ;
\ 31 - Argument handling 2
: GLOBAL! ( var val -- )
  GLOBAL WWBA ;

: VAR ( var -- val )
  DUP 0= IF DROP POP ELSE
  DUP 16 < IF LOCAL@ ELSE
  GLOBAL@ THEN THEN ;
: VARARG ( --  val )
  PC@ PC++ VAR ;
  

: EMPTYARG ( -- 0 )
  0 ;


\ 32 - Return
\ Performs a return.
: RETURN ( val -- )
  65 EMIT
  FP @ 2 + @ ( val st )
  DUP -1 <>
  IF STORE ELSE 2DROP THEN ( )
  FP @ @ PC !
  FP @ 1 + @ PCBLOCK !
  FP @ 3 + @ SP !
  FP @ 4 + @ FP !
;




\ 33 - Arg types
4 ARRAY ARGTYPES
: INIT_ARGTYPES ( -- )
  ' LARGECONST 0 ARGTYPES !
  ' SMALLCONST 1 ARGTYPES !
  ' VARARG 2 ARGTYPES !
  ' EMPTYARG 3 ARGTYPES !
;
INIT_ARGTYPES

: GETARG ( argtype -- arg )
  3 AND ARGTYPES @ EXECUTE ;




\ 34 - Blank















\ 35 - Blank















\ 36 - Instructions loader
48 LOAD \ Insutruction helpers
53 LOAD \ Object helpers

37 LOAD \ 0OP loader
59 LOAD \ 1OP loader
69 LOAD \ 2OP loader
84 LOAD \ VAR loader








\ 37 - 0OP loader
38 LOAD \ 0OP 1
39 LOAD \ 0OP 2
40 LOAD \ 0OP 3
41 LOAD \ 0OP initializer
42 LOAD \ 0OP initializer 2










\ 38 - 0OP instructions 1
: 0OP_RTRUE ( ) 1 RETURN ;
: 0OP_RFALSE ( ) 0 RETURN ;

: 0OP_PRINT ( )
  PC @ PCBLOCK @ 10 << OR
  DUP PRINT
  STRLEN 2 * PC+ ;

: 0OP_PRINT_RET ( ) 0OP_PRINT
  CR 1 RETURN ;

: 0OP_NOP ( ) ;



\ 39 - 0OP instructions 2
: 0OP_SAVE ( ) ZBRANCH ;
: 0OP_RESTORE ( ) ZBRANCH ;

VARIABLE FAKE_RESTART
: 0OP_RESTART ( )
  FAKE_RESTART @ EXECUTE ;
: 0OP_RET_POPPED ( )
  POP RETURN ;
: 0OP_POP ( ) POP DROP ;

: 0OP_QUIT ( ) BREAK ;

: 0OP_NEW_LINE ( )
  CR ;

\ 40 - 0OP instructions 3
: 0OP_SHOW_STATUS ( ) BREAK ;


\ TODO: Actually check.
: 0OP_VERIFY ( ) ZBRANCH ;



16 ARRAY OPS_0OP






\ 41 - 0OP initializer
: INIT_0OPS ( -- )
' 0OP_RTRUE 0 OPS_0OP !
' 0OP_RFALSE 1 OPS_0OP !
' 0OP_PRINT 2 OPS_0OP !
' 0OP_PRINT_RET 3 OPS_0OP !
' 0OP_NOP 4 OPS_0OP !
' 0OP_SAVE 5 OPS_0OP !
' 0OP_RESTORE 6 OPS_0OP !
' 0OP_RESTART 7 OPS_0OP !
' 0OP_RET_POPPED 8 OPS_0OP !
' 0OP_POP 9 OPS_0OP !
' 0OP_QUIT 10 OPS_0OP !
' 0OP_NEW_LINE 11 OPS_0OP !
' 0OP_SHOW_STATUS 12 OPS_0OP !
' 0OP_VERIFY 13 OPS_0OP ! ;
\ 42 - 0OP interpreter
INIT_0OPS

: INTERP_0OP ( op -- )
  OPS_0OP @ EXECUTE ;
  










\ 43 - Blank















\ 44 - Interpreter loader
45 LOAD \ INTERP_SHORT, _LONG
46 LOAD \ INTERP_VAR
47 LOAD \ Master interpreter












\ 45 - INTERP_SHORT, _LONG
: INTERP_SHORT ( opcode -- )
  DUP 4 >> 3 AND ( op argtype )
  DUP 3 = IF
    DROP 15 AND INTERP_0OP 
  ELSE
    GETARG SWAP 15 AND ( a op )
    INTERP_1OP
  THEN ;
: LONGARG ( ? -- argtype )
  1 SWAP << .. ;
: INTERP_LONG ( opcode -- )
  HEX .. DECIMAL
  DUP 6 BIT .. LONGARG GETARG
  OVER 5 BIT .. LONGARG GETARG
  ROT 31 AND INTERP_2OP ;
\ 46 - INTERP_VAR
\ : INTERP_EXT ( opcode -- ) ;
: INTERP_VAR ( opcode -- )
  >R PC@ PC++
  DUP 6 >> GETARG ( types a1 )
  SWAP DUP 4 >> GETARG ( 1 2 t)
  SWAP DUP 2 >> GETARG ( 123t )
  SWAP DUP GETARG ( 1 2 3 t 4 )
  SWAP R> ( 1 2 3 4 t op )
  INTERP_VAROP
;





\ 47 - Master interpreter
: ZINTERP ( -- )
  PCBLOCK @ 10 << PC @ +
  CR 37 EMIT HEX . DECIMAL
  PC@
  PC++ DUP HEX . DECIMAL
  KEY 105 = IF QUIT THEN
  DUP 192 AND 192 = IF
    INTERP_VAR ELSE
  DUP 192 AND 128 = IF
    INTERP_SHORT ELSE
  INTERP_LONG THEN THEN
;



\ 48 - Instruction helpers
50 LOAD \ Branching
51 LOAD \ Conditional branching













\ 49 - Store
\ Stores the given value into
\ the Z-machine value specified
\ at PC.
: STORE_TO ( val var -- )
  DUP 0= IF DROP PUSH
  ELSE DUP 16 < IF LOCAL!
  ELSE GLOBAL! THEN THEN
;

: STORE ( val -- )
  PC@ PC++ ( val var )
  STORE_TO ;



\ 50 - Branching
\ Always branch
: ZBRANCH ( -- )
  PC@W
  DUP 14 BIT IF ( offset )
    2 PC+
    16383 AND DUP BIT 13
    IF 3 << 14 OR THEN
  ELSE PC++ 63 AND THEN
  DUP 0= OVER 1 = OR
  IF RETURN
  ELSE 2 - PC+ THEN
;



\ 51 - Conditional branching
\ Checks whether to branch.
: ZBRANCH? ( ? -- )
  0> \ Make sure its 0 or 1
  PC@ ( ? offset )
  TUCK BIT 7 = IF
    DROP ZBRANCH
  ELSE
    6 BIT 2 SWAP - PC+
  THEN
;





\ 52 - Misc helpers
\ Skips returning to the
\ immediately previous word.
: BAIL ( -- ) R> DROP ;

: MAX ( a b -- max )
  2DUP > IF DROP ELSE NIP THEN
;

: MIN ( a b -- min )
  2DUP < IF DROP ELSE NIP THEN
;




\ 53 - Object loader
54 LOAD \ Objects 1
55 LOAD \ Objects 2
56 LOAD \ Objects 3
57 LOAD \ Objects 4
58 LOAD \ Objects 5










\ 54 - Objects 1
\ Returns BA for a given obj.
: OBJECT ( n -- ba )
  1- 9 * 62 + OBJTABLE + ;
\ Returns the value and bit.
: GETATTR_ ( attr n -- ba v b )
  OBJECT OVER 3 >> + DUP RBBA
  ROT 7 AND 7 SWAP - ;
: PUTATTR_ ( ba v' -- )
  SWAP WBBA ;
: ATTRIBUTE ( attr n -- ?)
  GETATTR_ BIT NIP ;
: SETATTR ( attr n -- )
  GETATTR_ SETBIT PUTATTR_ ;
: CLEARATTR ( attr n -- )
  GETATTR_ CLEARBIT PUTATTR_ ;
\ 55 - Objects 2
\ *ADDR ( n -- ba), * ( n -- n)
: PARENTADDR OBJECT 4 + ;
: PARENT PARENTADDR RBBA ;
: SIBLINGADDR OBJECT 5 + ;
: SIBLING SIBLINGADDR RBBA ;
: CHILDADDR OBJECT 6 + ;
: CHILD CHILDADDR RBBA ;

: PROPTABLE ( n -- ba )
  OBJECT 7 + RWBA ;
\ Address of the first prop.
: PROPTOP ( n -- ba )
  PROPTABLE DUP RBBA + ;
: SHORT_NAME ( n -- ba )
  PROPTABLE 1+ ;
\ 56 - Objects 3
\ Returns the address of the
\ SIZE BYTE of the property.
\ 0 for not found.
: PROPADDR ( prop n -- ba )
  PROPTOP BEGIN ( prop ba )
    2DUP RBBA .. ( p a p s )
    TUCK 31 AND = ( p a s ? )
    OVER 0= OR NOT ( p a s ? )
  WHILE ( prop addr size )
    5 >> + 2 + ( p a' )
  REPEAT ( p a s )
  0= IF 2DROP 0 ELSE NIP THEN
;


\ 57 - Objects 4
\ 0 for nonexistent.
: PROPLEN ( prop n -- len )
  PROPADDR DUP 0> IF
    RBBA 5 >> 1+ THEN
;

\ Returns the default prop.
: PROPDEFAULT ( prop -- val )
  1- 2 * OBJTABLE + RWBA ;






\ 58 - Objects 5
\ Falls back to default.
\ Must be 1 or 2 bytes long.
: PROPVAL ( prop n -- val )
  OVER >R PROPADDR R> ( ba p )
  OVER 0= IF
    NIP PROPDEFAULT ( val )
  ELSE ( ba p )
    DROP DUP RBBA 5 >> ( ba s )
    DUP 1 = IF DROP 1+ RBBA
    ELSE DUP 2 = IF
      DROP 1+ RWBA
      ELSE
        ." Property too long."
  THEN THEN THEN
;
\ 59 - 1OP Loader
60 LOAD \ 1OP 1
61 LOAD \ 1OP 2
62 LOAD \ 1OP 3
63 LOAD \ 1OP 4
64 LOAD \ 1OP 5
65 LOAD \ 1OP Table 1
66 LOAD \ 1OP Table 2








\ 60 - 1OP 1
\ Jump if a is 0.
: 1OP_JZ ( a -- ) ZBRANCH? ;
: 1OP_GET_SIBLING ( a -- )
  SIBLING DUP STORE ZBRANCH? ;
: 1OP_GET_CHILD ( a -- )
  CHILD DUP STORE ZBRANCH? ;
: 1OP_GET_PARENT ( a -- )
  PARENT STORE ;







\ 61 - 1OP 2
: 1OP_GET_PROP_LEN ( a -- )
  RBBA 5 >> STORE ;
: INCDEC ( a amount -- )
  >R DUP 0= IF DROP R> SP +!
  ELSE DUP 16 < IF
    LOCAL R> SWAP +!
  ELSE GLOBAL R> SWAP +! 
  THEN THEN
;
: 1OP_INC ( a -- )  1 INCDEC ;
: 1OP_DEC ( a -- ) -1 INCDEC ;
: 1OP_PRINT_ADDR ( ba -- )
  PRINT ;
\ : 1OP_CALL_1S v4

\ 62 - 1OP 3
: 1OP_REMOVE_OBJ ( n -- )
  DUP PARENTADDR DUP RBBA
  SWAP 0 SWAP WBBA ( n p )
  DUP 0= IF 2DROP EXIT THEN
  2DUP ( n p n p )
  CHILD = IF
    DUP CHILD SIBLING ( n p s )
    SWAP CHILDADDR WBBA ( n )
    DROP
  ELSE CHILD BEGIN ( n c )
      2DUP SIBLING <> ( n c ? )
    WHILE SIBLING REPEAT ( n c)
    SWAP SIBLING SWAP
    SIBLINGADDR !
  THEN ;
\ 63 - 1OP 4
: 1OP_PRINT_OBJ ( n -- )
  SHORT_NAME PRINT ;

: 1OP_RET ( val -- )
  RETURN ;

: 1OP_JUMP ( offset -- )
  2 - PC+ ;

: 1OP_PRINT_PADDR ( pa -- )
  PRINT_PA ;




\ 64 - 1OP 5
: 1OP_LOAD ( var -- )
  VAR STORE ;

: 1OP_NOT ( val -- )
  -1 XOR STORE ;










\ 65 - 1OP Table 1
16 ARRAY OPS_1OP


: INIT_1OPS_1 ( -- )
  ' 1OP_JZ 0 OPS_1OP !
  ' 1OP_GET_SIBLING 1 OPS_1OP !
  ' 1OP_GET_CHILD 2 OPS_1OP !
  ' 1OP_GET_PARENT 3 OPS_1OP !
  ' 1OP_GET_PROP_LEN 4
    OPS_1OP !
  ' 1OP_INC 5 OPS_1OP !
  ' 1OP_DEC 6 OPS_1OP !
  ' 1OP_PRINT_ADDR 7 OPS_1OP !
;
INIT_1OPS_1
\ 66 - 1OP Table 2
: INIT_1OPS_2 ( -- )
  \ call_1s in v4 8 OPS_1OP !
  ' 1OP_REMOVE_OBJ 9 OPS_1OP !
  ' 1OP_PRINT_OBJ 10 OPS_1OP !
  ' 1OP_RET 11 OPS_1OP !
  ' 1OP_JUMP 12 OPS_1OP !
  ' 1OP_PRINT_PADDR 13
    OPS_1OP !
  ' 1OP_LOAD 14 OPS_1OP !
  ' 1OP_NOT 15 OPS_1OP !
;
INIT_1OPS_2

: INTERP_1OP ( a op -- )
  OPS_1OP @ EXECUTE ;
\ 67 - Blank















\ 68 - Blank















\ 69 - 2OP loader
70 LOAD \ 2OP 1
71 LOAD \ 2OP 2
72 LOAD \ 2OP 3
73 LOAD \ 2OP 4
74 LOAD \ 2OP 5
75 LOAD \ 2OP 6
76 LOAD \ 2OP 7
77 LOAD \ 2OP 8

79 LOAD \ 2OP Table 1
80 LOAD \ 2OP Table 2
81 LOAD \ 2OP Table 3

82 LOAD \ 2OP Interpreter

\ 70 - 2OPs 1
: 2OP_JE ( a b -- )
  \ TODO: can get only 1 arg?
  = ZBRANCH? ;

: 2OP_JL ( a b -- )
  < ZBRANCH? ;

: 2OP_JG ( a b -- )
  > ZBRANCH? ;






\ 71 - 2OPs 2
: INCDEC_CHK ( var val d -- )
  DUP >R >R SWAP DUP 0= IF
    DROP R> SP +! SP @
  ELSE DUP 16 < IF
    LOCAL DUP R> SWAP +! @
  ELSE
    GLOBAL DUP R> SWAP +! @
  THEN THEN
  R> ( val new d )
  0> IF SWAP > ELSE > THEN
  ZBRANCH? ;
: 2OP_DEC_CHK ( var val -- )
  -1 INCDEC_CHK ;
: 2OP_INC_CHK ( var val -- )
  1 INCDEC_CHK ;
\ 72 - 2OPs 3  
: 2OP_JIN ( c p -- )
  SWAP PARENT = ZBRANCH? ;
: 2OP_TEST ( bitmap flags -- )
  TUCK AND = ZBRANCH? ;
: 2OP_OR ( a b -- )
  OR STORE ;
: 2OP_AND ( a b -- )
  AND STORE ;







\ 73 - 2OPs 4
: 2OP_TEST_ATTR ( obj attr -- )
  SWAP ATTRIBUTE ZBRANCH? ;
: 2OP_SET_ATTR ( obj attr -- )
  SWAP SETATTR ;
: 2OP_CLEAR_ATTR ( obj attr --)
  SWAP CLEARATTR ;

: 2OP_STORE ( var val -- )
  SWAP STORE_TO ;






\ 74 - 2OPs 5
: 2OP_INSERT_OBJ ( obj dest --)
  OVER 1OP_REMOVE_OBJ
  DUP CHILD ( obj dest newsib )
  ROT DUP CHILDADDR ( s o d ca)
  -ROT OVER ! ( s d o )
  DUP PARENTADDR ( s d o pa)
  -ROT SWAP ! ( s o )
  SIBLINGADDR ! ( )
;






\ 75 - 2OPs 6
: 2OP_LOADW ( arr off -- )
  2 * + RWBA STORE ;
: 2OP_LOADB ( arr off -- )
  + RBBA STORE ;











\ 76 - 2OPs 7
: 2OP_GET_PROP ( obj prop -- )
  SWAP PROPVAL STORE ;
: 2OP_GET_PROP_ADDR ( obj prop)
  SWAP PROPADDR STORE ;
: 2OP_GET_NEXT_PROP ( obj prop)
  DUP 0= IF
    DROP PROPTOP RBBA 31 AND
  ELSE 
    SWAP PROPADDR DUP RBBA
    5 >> 1+ ( ba len )
    + RBBA 31 AND
  THEN
  STORE
;

\ 77 - 2OPs 8
: 2OP_ADD ( a b -- ) CR
  2DUP . . + 61 EMIT .. STORE ;
: 2OP_SUB ( a b -- )
  - STORE ;
: 2OP_MUL ( a b -- )
  * STORE ;
: 2OP_DIV ( a b -- )
  DUP 0= IF
    ." Div by 0" BREAK EXIT
  THEN / STORE ;
: 2OP_MOD ( a b -- )
  DUP 0= IF
    ." Mod by 0" BREAK EXIT
  THEN MOD STORE ;

\ 78 - 2OPs 9
\ 2OP_CALL_2S v4
\ 2OP_CALL_2N v5
\ 2OP_SET_COLOUR v5 and 6
\ 2OP_THROW v5/6
\ Unused
\ Unused
\ Unused








\ 79 - 2OPS Table 1
32 ARRAY OPS_2OP
: INIT_2OPS_1 ( -- )
  \ Unused 0
  ' 2OP_JE 1 OPS_2OP !
  ' 2OP_JL 2 OPS_2OP !
  ' 2OP_JG 3 OPS_2OP !
  ' 2OP_DEC_CHK 4 OPS_2OP !
  ' 2OP_INC_CHK 5 OPS_2OP !
  ' 2OP_JIN 6 OPS_2OP !
  ' 2OP_TEST 7 OPS_2OP !
  ' 2OP_OR 8 OPS_2OP !
  ' 2OP_AND 9 OPS_2OP !
  ' 2OP_TEST_ATTR 10 OPS_2OP !
;
INIT_2OPS_1
\ 80 - 2OPS Table 1
: INIT_2OPS_2 ( -- )
  ' 2OP_SET_ATTR 11 OPS_2OP !
  ' 2OP_CLEAR_ATTR 12 OPS_2OP !
  ' 2OP_STORE 13 OPS_2OP !
  ' 2OP_INSERT_OBJ 14 OPS_2OP !
  ' 2OP_LOADW 15 OPS_2OP !
  ' 2OP_LOADB 16 OPS_2OP !
  ' 2OP_GET_PROP 17 OPS_2OP !
  ' 2OP_GET_PROP_ADDR
    18 OPS_2OP !
  ' 2OP_GET_NEXT_PROP
    19 OPS_2OP !
  ' 2OP_ADD 20 OPS_2OP !
;
INIT_2OPS_2
\ 81 - 2OPS Table 3
: INIT_2OPS_3 ( -- )
  ' 2OP_SUB 21 OPS_2OP !
  ' 2OP_MUL 22 OPS_2OP !
  ' 2OP_DIV 23 OPS_2OP !
  ' 2OP_MOD 24 OPS_2OP !
  \ 2OP_CALL_2S v4
  \ 2OP_CALL_2N v5
  \ 2OP_SET_COLOUR v5 and 6
  \ 2OP_THROW v5/6
  \ Unused
  \ Unused
  \ Unused
;
INIT_2OPS_3

\ 82 - 2OPS Interpreter
: INTERP_2OP ( a b op -- )
  OPS_2OP @ EXECUTE ;













\ 83 - Blank















\ 84 - VAR ops Loader
85 LOAD \ VAR 1
86 LOAD \ VAR 2
87 LOAD \ VAR 3
88 LOAD \ VAR 4
89 LOAD \ VAR 5
90 LOAD \ VAR 6
91 LOAD \ VAR 7
92 LOAD \ VAR 8
93 LOAD \ VAR 9
94 LOAD \ VAR 10
95 LOAD \ VAR 11
96 LOAD \ VAR 12
106 LOAD \ Secondary loader


\ 85 - VAR ops 1
\ Used by PREPARE_LOCALS to
\ move PC to the first default
\ value.
\ v3-specific
: MOVE_PC ( f -- )
  DUP 9 >> PCBLOCK !
  511 AND 1 << 1+ PC !
;







\ 86 - VAR ops 2
: PREPARE_LOCALS ( f locals --)
  PC@ PC++ ( f ls ret_target )
  OVER 5 + SP @ SWAP -
  ( f ls r fp' )
  FP @ OVER 4 + ! \ old FP
  SP @ OVER 3 + ! \ old SP
  PCBLOCK @ OVER 1 + ! \ block
  PC @ OVER ! \ return addr
  TUCK 2 + ! \ return target
  ( f ls fp' )
  2 PICK MOVE_PC ( f ls fp' )
  SWAP 0 DO ( f fp' )
    DUP I 5 + + PC@W SWAP !
    2 PC+
  LOOP FP ! FP @ SP ! DROP ;
\ 87 - VAR ops 3
: PREPARE_ARGUMENTS
  ( a b c n ls -- )
  2DUP >R >R
  MIN 0 DO ( a b c )
    I 5 + FP @ + !
  LOOP
  R> R> ( ls n )
  SWAP - 0 DO DROP LOOP
  \ Drop any extra args.
;





\ 88 - Var ops 4
: VAR_CALL ( f a b c n -- )
  DUP PICK ( f a 2 2 -> f )
  DUP 0= IF 0 RETURN EXIT THEN
  ( f a b c n f )
  DUP RBPA ( fabc n f locals )
  2DUP PREPARE_LOCALS
  ( fabc n f ls )
  NIP PREPARE_ARGUMENTS ( f )
;






\ 89 - VAR ops 4
: VAR_STOREW ( arr wi val n --)
  DROP -ROT 1 << + WWBA ;

: VAR_STOREB ( arr bi val n --)
  DROP -ROT + WBBA ;










\ 90 - VAR ops 5
: VAR_PUT_PROP ( o p val n -- )
  DROP -ROT SWAP PROPADDR
  DUP 0= IF
    ." put_prop prop DNE"
    BREAK EXIT THEN
  DUP RBBA 5 >> ( val ba size )
  DUP 1 > IF
    ." ILLEGAL" EXIT THEN

  0= IF SWAP 255 AND SWAP THEN
  WWBA
;



\ 91 - VAR ops 6
\ TODO: Implement status lines.
: DRAW_STATUS_LINE ( ) ;

: TO_LOWERCASE ( key -- key )
  DUP 65 91 WITHIN
  IF 32 + THEN
;








\ 92 - VAR ops 7
\ Expects length in byte 0.
: READ_LINE ( text-buffer -- )
  DUP RBBA ( buf len )
  1+ 1 DO \ indexed from 1
    KEY
    2DUP SWAP I + WBBA ( b k )
    TO_LOWERCASE
    10 = IF
      0 OVER WBBA 10000
    ELSE 1 THEN
  +LOOP
  \ Write final 0.
  DUP RBBA + 0 SWAP WBBA ;


\ 93 - VAR ops 8
6 ARRAY DICT_BUF
VARIABLE DICT_BUF_LEN

: PUT_DICT_CHAR ( c -- )
  DICT_BUF_LEN @ DUP 6 < IF
    DICT_BUF !
    1 DICT_BUF_LEN +!
  ELSE 2DROP THEN
;






\ 94 - VAR ops 9
\ Writes next char (or 5) to
\ DICT_BUF
: NEXT_CHAR
  ( addr len -- addr' len' )
  DUP 0= IF 5 PUT_DICT_CHAR
  ELSE
    1- SWAP DUP RBBA \ l' a c
    SWAP 1+ \ l' c a'
    -ROT
    DUP 97 123 WITHIN IF
      91 - PUT_DICT_CHAR
    ELSE BREAK THEN
  THEN \ a' l'
;

\ 95 - VAR ops 10
\ TODO: v3 specific
2 ARRAY DICT_TARGET

: PARSE_STRING ( addr len -- )
  NEXT_CHAR NEXT_CHAR NEXT_CHAR
  NEXT_CHAR NEXT_CHAR NEXT_CHAR
  2DROP
  0 DICT_BUF @ 1 DICT_BUF @
  2 DICT_BUF @
  ENCODE 0 DICT_TARGET !
  3 DICT_BUF @ 4 DICT_BUF @
  5 DICT_BUF @
  ENCODE 1 DICT_TARGET !
;

\ 96 - VAR ops 11
\ Address of the first word.
: DICT_TOP ( -- ba )
  DICT DUP RBBA 4 + + ;
: DICT_ENTRY_SIZE ( -- n )
  DICT DUP RBBA 1+ + RBBA ;
: DICT_ENTRY_COUNT ( -- n )
  DICT DUP RBBA 2 + + RWBA ;

\ Address after the last word.
: DICT_BOTTOM ( -- ba )
  DICT_ENTRY_SIZE
  DICT_ENTRY_COUNT * DICT_TOP +
;


\ 97 - DICT_WRITE_ENTRY
\ Writes the entry's data to 
\ a parse table.
: DICT_WRITE_ENTRY
  ( p l i d --)
  3 PICK ( p l i d p )
  WWBA ( p l i )
  -ROT OVER 2 + ( i p l p )
  WBBA 3 + ( i p )
  WBBA ( )
;





\ 98 - DICT_LOOKUP
\ Looks up DICT_TARGET
: DICT_LOOKUP ( parse len i --)
  DICT_BOTTOM DICT_TOP DO
    I RWBA 0 DICT_TARGET @ =
    I 2 + RWBA 1 DICT_TARGET @
    = AND IF ( p l i )
      I DICT_WRITE_ENTRY ( )
      20000
    ELSE DICT_ENTRY_SIZE THEN
  +LOOP
;




\ 99 - PARSE_WORD helpers
: IS_SEPARATOR ( c -- ? )
  DUP 32 = OVER 44 = OR ( c ? )
  SWAP 46 = OR
;

: PARSE_FIND_SEPARATOR
  ( t i -- t i' )
  BEGIN
    2DUP + RBBA ( t i c )
    IS_SEPARATOR NOT
  WHILE ( t i' ) 1+ REPEAT
;

VARIABLE WORDS_PARSED

\ 100 - PARSE_WORD
: PARSE_WORD ( p t i )
  DUP >R \ Set aside start pos
  PARSE_FIND_SEPARATOR ( pti')
  R> DUP >R \ p t i' i
  2DUP - \ p t i' i len
  3 PICK \ p t i' i len t
  ROT + \ p t i' len addr
  SWAP DUP >R \ p t i' a l
  PARSE_STRING \ p t i'
  ROT DUP R> R> \ t i' p p l i
  DICT_LOOKUP \ t i' p
  4 + -ROT \ p' t i' 
  2DUP + RBBA
  0= IF DROP 0 THEN
;
\ 101 - PARSE
\ Parses the whole text buffer
\ into the parse buffer.
\ Writes the number of words
\ parsed into byte 1 of parse.
: PARSE ( parse text -- )
  OVER >R \ Set aside parse.
  0 WORDS_PARSED !
  SWAP 2 + SWAP 1+ ( p t )
  1 BEGIN ( p t i )
    PARSE_WORD ( p' t i' )
    DUP 0>
  UNTIL ( p t i )
  2DROP DROP R> ( p_orig )
  1+ WORDS_PARSED @ SWAP WBBA
;
\ 102 - VAR ops - SREAD
: VAR_SREAD ( text parse n -- )
  DROP SWAP ( parse text )
  DRAW_STATUS_LINE
  DUP READ_LINE ( parse text )
  OVER 0>
  IF PARSE ( )
  ELSE 2DROP THEN
;







\ 103 - VAR ops
: VAR_PRINT_CHAR ( char n -- )
  DROP EMIT ;

: VAR_PRINT_NUM ( num n -- )
  DROP . ;










\ 104 - VAR ops
: VAR_RANDOM ( range n -- )
  DROP DUP 0> IF
    RANDOM SWAP MOD 1+
  ELSE
    SETSEED 0
  THEN
  STORE
;
  
: VAR_PUSH ( value n -- )
  DROP PUSH ;
: VAR_PULL ( var n -- )
  DROP POP ( var val )
  SWAP STORE_TO
;
\ 105 - VAR ops
\ TODO: Implement these, v3.
: VAR_SPLIT_WINDOW ( lines n )
  2DROP BREAK ;

: VAR_SET_WINDOW ( window n )
  2DROP BREAK ;

: VAR_OUTPUT_STREAM ( num n --)
  2DROP BREAK ;
: VAR_INPUT_STREAM ( num n -- )
  2DROP BREAK ;
: VAR_SOUND_EFFECT ( n e v r n)
  2DROP 2DROP DROP BREAK ;


\ 106 - Secondary VAR loader
97 LOAD \ Var ops
98 LOAD \ Var ops
99 LOAD \ Var ops
100 LOAD \ Var ops
101 LOAD \ Var ops
102 LOAD \ Var ops
103 LOAD \ Var ops
104 LOAD \ Var ops
105 LOAD \ Var ops

107 LOAD \ Var ops table 1
108 LOAD \ Var ops table 2
109 LOAD \ Var ops interpreter


\ 107 - Var ops table 1
22 ARRAY OPS_VAR

: INIT_VAR_OPS_1 ( -- )
  ' VAR_CALL 0 OPS_VAR !
  ' VAR_STOREW 1 OPS_VAR !
  ' VAR_STOREB 2 OPS_VAR !
  ' VAR_PUT_PROP 3 OPS_VAR !
  ' VAR_SREAD 4 OPS_VAR !
  ' VAR_PRINT_CHAR 5 OPS_VAR !
  ' VAR_PRINT_NUM 6 OPS_VAR !
  ' VAR_RANDOM 7 OPS_VAR !
  ' VAR_PUSH 8 OPS_VAR !
;
INIT_VAR_OPS_1

\ 108 - Var ops table 2
: INIT_VAR_OPS_2 ( -- )
  ' VAR_PULL 9 OPS_VAR !
  ' VAR_SPLIT_WINDOW 10
    OPS_VAR !
  ' VAR_SET_WINDOW 11 OPS_VAR !
  ' VAR_OUTPUT_STREAM 19
    OPS_VAR !
  ' VAR_INPUT_STREAM 20
    OPS_VAR !
  ' VAR_SOUND_EFFECT 21
    OPS_VAR !
;
INIT_VAR_OPS_2


\ 109 - Var ops interpreter
\ 4 args, type byte, op byte
: INTERP_VAROP ( 1234to -- )
  >R 0 ( 1 2 3 4 t 0 )
  OVER 3 AND 3 = + ( 1234 tn)
  OVER 12 AND 12 = + ( 1234tn )
  OVER 48 AND 48 = + ( 1234tn )
  SWAP 192 AND 192 = + ( 1234n)
  DUP 4 SWAP - >R ( 1234n//on)
  BEGIN DUP 0> WHILE ( args..n)
    SWAP DROP 1-
  REPEAT DROP ( args.. )
  R> R> ( args.. n op )
  224 - OPS_VAR @ EXECUTE
;

\ 110 - Blank















\ 111 - Blank















\ 112 - Game loading
113 LOAD \ INIT_HEADER
116 LOAD \ Copying to memory
114 LOAD \ RESTART












\ 113 - INIT_HEADER
: INIT_HEADER ( -- )
  INIT_STATIC

  FLAGS1 RBBA ..
  6 CLEARBIT
  5 CLEARBIT
  4 SETBIT
  FLAGS1 WBBA

  FLAGS2 RWBA
  0 CLEARBIT
  FLAGS2 WWBA
; 


\ 114 - RESTART
: RESTART
  COPY_DYNAMIC_MEMORY
  INIT_HEADER
  PC0
  DUP 10 >> PCBLOCK !
  1023 AND PC !
  BEGIN
    ZINTERP
  AGAIN
;

: INIT_RESTART
  ' RESTART FAKE_RESTART !
;
INIT_RESTART
\ 115 - Welcome message
: WELCOME
  ." 0x10^Z" CR
  ." Z-machine for DCPU-16" CR
  ." by Braden Shepherdson" CR
  ." version 0.1" CR
;
WELCOME








\ 116 - Copying to memory
\ Raw copying, no RWBA et al.
: GET_STATIC ( -- static_ba )
  0 READBLOCK 7 MMR + @ ;

: COPY_DYNAMIC_MEMORY ( -- )
  GET_STATIC 0 DO
    I 10 >> READBLOCK
    I 1 >> 511 AND MMR + @ ( v)
    I 1 >> MEMBASE + ! ( )
  2 +LOOP
;




\ 117 - Debugging utils
: .. ( a -- a ) DUP . ;















\ 0 - Introduction
\ DCPU-16 Forth
\ (c) 2012 Braden Shepherdson
\ Version 3

\ This is a Forth system
\ designed to run on DCPU-16
\ systems. It consists of this
\ floppy disk image and a
\ binary to be loaded directly.






\ 1 - Master Loader
2 LOAD \ Core
6 LOAD \ Control Structures
12 LOAD \ ( comments
13 LOAD \ Extra stack words
14 LOAD \ Spaces and bases
15 LOAD \ Output
22 LOAD \ WITHIN and DEPTH
23 LOAD \ Strings loader
29 LOAD \ Defining Words Loader
36 LOAD \ Welcome message
37 LOAD \ Compiler Loader




\ 2 - Core Loader
3 LOAD \ Math and constants
4 LOAD \ Literals and chars
5 LOAD \ [COMPILE] and RECURSE












\ 3 - Core - Math and constants
: /MOD 2DUP MOD -ROT / ;

: NL 10 ;
: BL 32 ;
: CR NL EMIT ;
: SPACE BL EMIT ;

: NEGATE 0 SWAP - ;

\ Standard words for booleans
: TRUE 1 ;
: FALSE 0 ;
: NOT 0= ;


\ 4 - Core - Literals and chars
\ LITERAL compiles LIT <foo>
: LITERAL IMMEDIATE
    ' LIT , \ compile LIT
    ,       \ compile literal
  ;

\ Idiom: [ ] and LITERAL to
\ compute at compile time.
\ Me: This seems dubious. The
\ dict is getting longer.
: ';' [ CHAR ; ] LITERAL ;
: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;


\ 5 - Core - [COMPILE] RECURSE
\ Compiles IMMEDIATE words.
: [COMPILE] IMMEDIATE
    WORD  \ get the next word
    FIND  \ find it in the dict
    >CFA  \ get its codeword
    ,     \ and compile it.
  ;


: RECURSE IMMEDIATE
    LATEST @  \ This word
    >CFA      \ get codeword
    ,         \ compile it
  ;

\ 6 - Control Structures Loader
\ Compiled code only!
\ cond IF true-part THEN rest
\ cond IF t ELSE f THEN
\ CASE c1 OF ... ENDOF
\   default ENDCASE

7 LOAD \ IF
8 LOAD \ THEN ELSE
9 LOAD \ BEGIN UNTIL
10 LOAD \ AGAIN UNLESS
11 LOAD \ WHILE REPEAT
33 LOAD \ CASE Statements
40 LOAD \ DO LOOP


\ 7 - IF
\ cond IF t THEN rest
\ -> cond 0BRANCH OFFSET t rest
\ cond IF t ELSE f THEN rest
\ -> cond 0BRANCH OFFSET t
\      BRANCH OFFSET2 f rest
: IF IMMEDIATE
    ' 0BRANCH ,
    HERE @      \ save location
    0 ,         \ dummy offset
  ;





\ 8 - THEN ELSE
: THEN IMMEDIATE
    DUP
    HERE @ SWAP - \ calc offset
    SWAP !        \ store it
  ;

: ELSE IMMEDIATE
    ' BRANCH , \ branch to end
    HERE @     \ save location
    0 ,        \ dummy offset
    SWAP       \ orig IF offset
    DUP        \ like THEN
    HERE @ SWAP -
    SWAP !
  ;
\ 9 - BEGIN UNTIL
\ BEGIN loop condition UNTIL ->
\ loop cond 0BRANCH OFFSET
: BEGIN IMMEDIATE
    HERE @
;

: UNTIL IMMEDIATE
    ' 0BRANCH ,
    HERE @ -
    ,
;




\ 10 - AGAIN and UNLESS
\ BEGIN loop AGAIN, infinitely.
: AGAIN IMMEDIATE
    ' BRANCH ,
    HERE @ -
    ,
  ;

\ UNLESS is IF reversed
: UNLESS IMMEDIATE
    ' NOT ,
    [COMPILE] IF
  ;



\ 11 - WHILE REPEAT
\ BEGIN cond WHILE loop REPEAT
: WHILE IMMEDIATE
    ' 0BRANCH ,
    HERE @
    0 , \ dummy offset
  ;

: REPEAT IMMEDIATE
    ' BRANCH ,
    SWAP
    HERE @ - ,
    DUP
    HERE @ SWAP -
    SWAP !
  ;
\ 12 - ( comments
: ( IMMEDIATE
    1 \ tracking depth
    BEGIN
        KEY \ read next char
        DUP 40 = IF \ open (
            DROP \ drop it
            1+ \ bump the depth
        ELSE
            41 = IF \ close )
               1- \ dec depth
            THEN
        THEN
    DUP 0= UNTIL \ depth == 0
    DROP \ drop the depth
  ;
\ 13 - Extra stack words
: NIP ( x y -- y ) SWAP DROP ;
: TUCK ( x y -- y x y )
    SWAP OVER ;
: PICK ( x_u ... x_1 x_0 u --
    x_u ... x_1 x_0 x_u )
    1+     \ skip over u
    DSP@ + \ add to DSP
    @      \ fetch
  ;






\ 14 - Spaces and Bases
\ writes n spaces to stdout
: SPACES ( n -- )
    BEGIN
        DUP 0> \ while n > 0
    WHILE
        SPACE 1-
    REPEAT
    DROP
;

\ Standard base changers.
: DECIMAL ( -- ) 10 BASE ! ;
: HEX ( -- ) 16 BASE ! ;


\ 15 - Output LOADER
16 LOAD \ U.
17 LOAD \ .S stack printer
18 LOAD \ UWIDTH
19 LOAD \ U.R unsigned padded
20 LOAD \ .R signed padded
21 LOAD \ . U. ?









\ 16 - U.
: U.  ( u -- )
    BASE @ /MOD \ ( width r q )
    ?DUP IF \ if q <> 0 then
        RECURSE \ print quot
    THEN
    \ print the remainder
    DUP 10 < IF
        48 \ dec digits 0..9
    ELSE
        10 -
        65 \ hex and other A..Z
    THEN
    + EMIT
;

\ 17 - .S prints the stack
\ Debugging utility.
: .S ( -- )
    DSP@ \ get stack pointer
    BEGIN
        DUP S0 @ <
    WHILE
        DUP @ U. \ print
        SPACE
        1+       \ move up
    REPEAT
    DROP
;



\ 18 - UWIDTH
: UWIDTH ( u -- width )
    BASE @ / \ rem quot
    ?DUP IF   \ if quot <> 0
        RECURSE 1+
    ELSE
        1 \ return 1
    THEN
;







\ 19 - U.R
: U.R ( u width -- )
    SWAP   \ ( width u )
    DUP    \ ( width u u )
    UWIDTH \ ( width u uwidth )
    ROT    \ ( u uwidth width )
    SWAP - \ ( u width-uwdith )
    SPACES \ no-op on negative
    U.
;






\ 20 - .R
\ Print padded, signed number
: .R ( n width -- )
    SWAP DUP 0< IF
        NEGATE \ width u
        1 SWAP \ width 1 u
        ROT 1- \ 1 u width-1
    ELSE
        0 SWAP ROT \ 0 u width
    THEN
    SWAP DUP \ flag width u u
    UWIDTH \ flag width u uw
    ROT SWAP - \ ? u w-uw
    SPACES SWAP \ u ?
    IF 45 EMIT THEN \ print -
    U. ;
\ 21 - dot, master print, U.
: . 0 .R SPACE ;
\ Replace U.
: U. U. SPACE ;
\ ? fetches an addr and prints
: ? ( addr -- ) @ . ;










\ 22 - WITHIN and DEPTH
\ c a b WITHIN ->
\   a <= c & c < b
: WITHIN ( c a b -- ? )
    >R \ c a
    2DUP < IF
        2DROP FALSE EXIT
    THEN
    DROP R> \ c b
    < ;

: DEPTH ( -- n )
    S0 @ DSP@ -
    1- \ adjust for S0 on stack
;

\ 23 - Strings Loader
\ S" string" defines a string.
\     ( -- addr len )
\ C, appends a (machine) word
\ to the current compiled word.
24 LOAD \ C,
25 LOAD \ String compiling
26 LOAD \ String interpreting
27 LOAD \ String master
28 LOAD \ print string






\ 24 - C,
: C,
    HERE @ !
    1 HERE +! \ Increment HERE
;











\ 25 - String compiling
: .S_COMP
    ' LITSTRING ,
    HERE @ \ address
    0 ,    \ dummy length
    BEGIN
        KEY        \ next char
        DUP 34 <>  \ ASCII "
    WHILE
        C, \ copy character
    REPEAT
    DROP \ drop the "
    DUP HERE @ SWAP - \ length
    1- SWAP ! \ set length
  ;

\ 26 - String interpreted
: .S_INTERP
    HERE @ \ temp space
    BEGIN
        KEY
        DUP 34 <>  \ ASCII "
    WHILE
        OVER ! \ save character
        1+     \ bump address
    REPEAT
    DROP     \ drop the "
    HERE @ - \ calculate length
    HERE @   \ push start addr
    SWAP     \ addr len
  ;

\ 27 - String master
: S" IMMEDIATE ( -- addr len )
    STATE @ IF \ compiling?
        .S_COMP
    ELSE \ immediate mode
        .S_INTERP
    THEN
  ;








\ 28 - print string
: ." IMMEDIATE ( -- )
    STATE @ IF \ compiling?
        [COMPILE] S"
        ' TELL ,
    ELSE
        \ Just read and print
        BEGIN
            KEY
            DUP 34 = IF \ "
                DROP EXIT
            THEN
            EMIT
        AGAIN
    THEN
  ;
\ 29 - Defining Words Loader
30 LOAD \ Constants and Values
31 LOAD \ ALLOT and Variables
32 LOAD \ FORGET
34 LOAD \ Execution tokens
35 LOAD \ Arrays










\ 30 - Constants and Values
: CONSTANT
    WORD CREATE
    DOCOL , \ codeword
    ' LIT , \ append LIT
    ,       \ input value
    ' EXIT , \ and append EXIT
  ;
: VALUE ( n -- )
    WORD CREATE
    DOCOL ,
    ' LIT ,
    ,
    ' EXIT ,
  ;

\ 31 - ALLOT and Variables
\ Allocates n words of memory
: ALLOT ( n -- addr )
    HERE @ SWAP \ here n
    HERE +!     \ add n to HERE
  ;

\ Finally VARIABLE itself.
: VARIABLE
    1 ALLOT \ allocate 1 cell
    WORD CREATE
    DOCOL ,
    ' LIT ,
    , \ pointer from ALLOT
    ' EXIT ,
  ;
\ 32 - FORGET
\ FORGET is a horrible hack to
\ deallocate memory.
\ Sets HERE to the beginning of
\ the given word and resets
\ LATEST. FORGETing built-ins
\ will cause suffering.
: FORGET
    WORD FIND \ dict address
    DUP @ LATEST !
    HERE !
  ;




\ 33 - CASE Statements
: CASE IMMEDIATE 0 ;
: OF IMMEDIATE
    ' OVER ,
    ' = ,
    [COMPILE] IF
    ' DROP ,
;
: ENDOF IMMEDIATE
    [COMPILE] ELSE ;
: ENDCASE IMMEDIATE
    ' DROP ,
    BEGIN ?DUP WHILE
    [COMPILE] THEN REPEAT
;

\ 34 - Execution Tokens
: :NONAME
    0 0 CREATE \ nameless entry
    HERE @     \ current HERE
    \ value is the address of
    \ the codeword, ie. the xt
    DOCOL ,
    ] \ compile the definition.
  ;

\ compiles in a LIT
: ['] IMMEDIATE ' LIT , ;




\ 35 - Arrays
: ARRAY ( n -- )
  ALLOT >R
  WORD CREATE \ define the word
  DOCOL ,    \ compile DOCOL
  ' LIT ,    \ compile LIT
  R> ,       \ compile address
  ' + ,      \ add index
  ' EXIT ,   \ compile EXIT
;






\ 36 - Welcome message
: WELCOME
    ." DCPU-16 FORTH" CR
    ." by Braden Shepherdson"
    CR
    ." version " VERSION . CR
;

WELCOME
\ HIDE WELCOME






\ 37 - Compiler loading
\ Words for writing the
\ compiled code to disk.
38 LOAD \ Disk writers
39 LOAD \ Main compiler











\ 38 - Disk writers
VARIABLE DISK_BLOCK

: WRITEWORD ( n -- )
    DUP 511 AND MMR + ( n da )
    SWAP @ 
    DUP 8 << SWAP 8 >> OR
    OVER ! ( da )
    511 AND 511 = IF
        DISK_BLOCK @
        WRITEBLOCK
        1 DISK_BLOCK +!
    THEN
  ;


\ 39 - Main compiler
\ Takes the top address
: COMPILER ( n -- )
    0 DISK_BLOCK !
    0 ( top 0 )
    BEGIN
        DUP WRITEWORD
        1+
        2DUP = ( here == 0 )
    UNTIL
    . DROP
    DISK_BLOCK @ WRITEBLOCK
  ;



\ 40 - DO LOOP
41 LOAD
42 LOAD

: DO IMMEDIATE \ lim start --
  HERE @
  ' 2DUP ,
  ' SWAP , ' >R , ' >R ,
  ' > ,
  ' 0BRANCH ,
  HERE @ \ location of offset
  0 , \ dummy exit offset
;



\ 41 - +LOOP, LOOP
: +LOOP IMMEDIATE \ inc --
  ' R> , ' R> , \ i s l
  ' SWAP , \ ils
  ' ROT , ' + , \ l s'
  ' BRANCH , \ ( top branch )
  SWAP HERE @ \ ( br top here )
  - , \ top ( br )
  HERE @ OVER -
  SWAP ! \ end
  ' R> , ' R> , ' 2DROP ,
;

: LOOP IMMEDIATE \ --
  ' LIT , 1 , [COMPILE] +LOOP ;

\ 42 - I, J
: I \  -- i
  R> R> \ ret i
  DUP -ROT >R >R ;

: J \ -- j
  R> R> R> R> DUP \ ( riljj )
  -ROT \ ( r i j l j )
  >R >R \ ( r i j )
  -ROT >R >R \ ( j )
;

\ Drops the values from RS.
: UNLOOP \ ( -- )
  R> R> R> 2DROP >R ;


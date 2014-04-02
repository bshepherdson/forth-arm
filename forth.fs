\ Forth ARM
\ Copyright 2014 Braden Shepherdson
\ Version 1

\ This is a Forth system
\ designed to run on ARMv6
\ systems. It consists of this
\ source code file and a binary
\ executable.

: / /MOD SWAP DROP ;

: MOD /MOD DROP ;

: NL 10 ;
: BL 32 ;
: CR NL EMIT ;
: SPACE BL EMIT ;

: NEGATE 0 SWAP - ;

\ Standard words for booleans
: TRUE -1 ;
: FALSE 0 ;
: NOT 0= ;


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


\ Compiles IMMEDIATE words.
: [COMPILE] IMMEDIATE
    WORD  \ get the next word - yes, word. FIND expects the counted string.
    FIND  \ find it in the dict -- xt flag
    DROP  \ XXX: Dangerous, we're assuming it's found successfully.
    >BODY \  get its codeword
    ,     \ and compile it.
  ;


: RECURSE IMMEDIATE
    LATEST @  \ This word
    >BODY     \ get codeword
    ,         \ compile it
  ;

\ Control structures - ONLY SAFE IN COMPILED CODE!
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


: ( IMMEDIATE
    ')' PARSE 2DROP ;

: NIP ( x y -- y ) SWAP DROP ;
: TUCK ( x y -- y x y )
    SWAP OVER ;
: PICK ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u )
    1+     \ skip over u
    4 *    \ word size
    DSP@ + \ add to DSP
    @      \ fetch
  ;


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


\ Strings and numbers
: U.  ( u -- )
    BASE @ /MOD \ ( r q )
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

\ Debugging utility.
: .S ( -- )
    DSP@ \ get stack pointer
    BEGIN
        DUP S0 @ <
    WHILE
        DUP @ U. \ print
        SPACE
        4+       \ move up
    REPEAT
    DROP
;


: UWIDTH ( u -- width )
    BASE @ / \ rem quot
    ?DUP IF   \ if quot <> 0
        RECURSE 1+
    ELSE
        1 \ return 1
    THEN
;



: U.R ( u width -- )
    SWAP   \ ( width u )
    DUP    \ ( width u u )
    UWIDTH \ ( width u uwidth )
    ROT    \ ( u uwidth width )
    SWAP - \ ( u width-uwdith )
    SPACES \ no-op on negative
    U.
;


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

: . 0 .R SPACE ;
\ Replace U.
: U. U. SPACE ;
\ ? fetches an addr and prints
: ? ( addr -- ) @ . ;



\ c a b WITHIN ->
\   a <= c & c < b
: WITHIN ( c a b -- ? )
    -ROT    ( b c a )
    OVER    ( b c a c )
    <= IF
        > IF   ( b c -- )
            TRUE
        ELSE
            FALSE
        THEN
    ELSE
        2DROP
        FALSE
    THEN
;

: DEPTH ( -- n )
    S0 @ DSP@ -
    4- \ adjust for S0 on stack
;

: ALIGNED ( addr -- addr )
    3 + 3 INVERT AND
;

: ALIGN HERE @ ALIGNED HERE ! ;

: C,
    HERE @ C!
    1 HERE +! \ Increment HERE
;



: CONSTANT
    CREATE
    DOCOL , \ codeword
    ' LIT , \ append LIT
    ,       \ input value
    ' EXIT , \ and append EXIT
  ;
: VALUE ( n -- )
    CREATE
    DOCOL ,
    ' LIT ,
    ,
    ' EXIT ,
  ;

\ Allocates n bytes of memory
: ALLOT ( n -- addr )
    HERE @ SWAP \ here n
    HERE +!     \ add n to HERE
  ;

\ Converts a number of cells into a number of bytes
: CELLS ( n -- n ) 4 * ;

\ Finally VARIABLE itself.
: VARIABLE
    1 CELLS ALLOT \ allocate 1 cell
    CREATE
    DOCOL ,
    ' LIT ,
    , \ pointer from ALLOT
    ' EXIT ,
  ;


: DUMP ( addr len -- )
    BASE @ -ROT \ save the current BASE at the bottom of the stack
    HEX         \ and switch to hex mode

    BEGIN
        ?DUP \ while len > 0
    WHILE
        OVER 8 U.R  \ print the address
        SPACE

        \ print up to 16 words on this line
        2DUP     \ addr len addr len
        1- 15 AND 1+  \ addr len addr linelen
        BEGIN
            ?DUP  \ while linelen > 0
        WHILE
            SWAP       \ addr len linelen addr
            DUP C@     \ addr len linelen addr byte
            2 .R SPACE \ print the byte
            1+ SWAP 1- \ addr len linelen addr -- addr len addr+1 linelen-1
        REPEAT
        DROP ( addr len )

        \ print the ASCII equivalents
        2DUP 1- 15 AND 1+ \ addr len addr linelen
        BEGIN
            ?DUP   \ while linelen > 0
        WHILE
            SWAP DUP C@   \ addr len linelen addr byte
            DUP 32 123 WITHIN IF    \ 32 <= c < 128
                EMIT
            ELSE
                DROP 46 EMIT \ emit a period
            THEN
            1+ SWAP 1-   \ addr len linelen addr -- addr len addr+1 linelen-1
        REPEAT
        DROP \ addr len
        CR

        DUP 1- 15 AND 1+ \ addr len linelen
        TUCK             \ addr linelen len linelen
        -                \ addr linelen len-linelen
        >R + R>          \ addr+linelen len-linelen
    REPEAT

    DROP   \ restore stack
    BASE ! \ restore saved BASE
;


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

\ Creates a dictionary entry with no name.
: :NONAME ( -- xt)
    0 0 (CREATE) \ nameless entry
    LATEST @     \ LATEST holds the address of the link pointer, which is the xt.
    \ value is the address of
    \ the codeword, ie. the xt
    DOCOL ,
    ] \ compile the definition.
;

\ compiles in a LIT
: ['] IMMEDIATE ' LIT , ;


\ Expects the user to specify the number of bytes, not cells.
: ARRAY ( n -- )
  ALLOT >R
  CREATE \ define the word
  DOCOL ,    \ compile DOCOL
  ' CELLS ,     \ multiply the index into cells
  ' LIT ,    \ compile LIT
  R> ,       \ compile address
  ' + ,      \ add index
  ' EXIT ,   \ compile EXIT
;




: DO IMMEDIATE \ lim start --
  HERE @
  ' 2DUP ,
  ' SWAP , ' >R , ' >R ,
  ' > ,
  ' 0BRANCH ,
  HERE @ \ location of offset
  0 , \ dummy exit offset
;


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


: S" IMMEDIATE ( -- addr len )
    STATE @ IF \ compiling?
        ' LITSTRING ,
        34 PARSE \ addr len
        DUP ,    \ addr len (and the length compiled in)
        HERE @ \ src len dst
        SWAP   \ src dst len
        DUP HERE +! \ src dst len - move HERE to make room

        0 DO \ src dst
            OVER I + C@ \ src dst val
            OVER I + C! \ src dst
        LOOP
        2DROP
        ALIGN
    THEN
    \ TODO: Write the interpretation version, that copies to HERE without moving the HERE-pointer.
    \ Or can it get away with using the keyboard input buffer?
;

: ." IMMEDIATE ( -- )
    STATE @ IF \ compiling?
        [COMPILE] S"
        ' TELL ,
    ELSE
        \ Just read and print
        34 PARSE \ addr len
        TELL
    THEN
;

: WELCOME
    ." FORTH ARM" CR
    ." by Braden Shepherdson" CR
    ." version " VERSION . CR
;
WELCOME



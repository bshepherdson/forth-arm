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

: [CHAR] IMMEDIATE PARSE-NAME DROP C@ ' LIT , , ;

\ Compiles IMMEDIATE words.
: [COMPILE] IMMEDIATE
    WORD  \ get the next word - yes, word. FIND expects the counted string.
    FIND  \ find it in the dict -- xt flag
    DROP  \ XXX: Dangerous, we're assuming it's found successfully.
    >BODY \  get its codeword
    ,     \ and compile it.
  ;

\ POSTPONE is a smart COMPILE/[COMPILE]. It is immediate.
\ On non-immediate words, it is the same as , .
\ On immediate words, it is the same as [COMPILE].
: POSTPONE IMMEDIATE
    WORD FIND DROP \ Now we have the xt.
    >BODY ,
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



\ This is tricky. The LHS before DOES> should have called CREATE. That new definition willl
\ look like: DOCOL, LIT, body pointer, EXIT, EXIT. (Yes, two EXITs.)
\ DOES> replaces the first EXIT with the address of the DOCOL it's about to insert.

\ There are three distinct phases here:
\ 1. A defining word (eg. ARRAY) whose definition contains DOES>
\ 2. That defining word is used to create another word.
\ 3. That new word is executed.
\ DOES> runs during compilation of 1., and it needs to set up the second two phases.
: DOES> IMMEDIATE
    \ First compute the address of where we're going to put the DOCOL for the code after DOES>.
    \ Code needs to be compiled into phase 2 that will overwrite the first EXIT in phase 3.

    HERE @ 44 +

    ' LATEST ,
    ' @ ,
    ' >BODY ,
    ' LIT ,
    12 ,
    ' + ,
    ' LIT ,
    , \ Write the address we computed above.
    ' SWAP ,
    ' ! ,
    ' EXIT ,

    \ Now this should be where the HERE pointer above is aimed. Compile DOCOL now.
    \ We're now one-layer compiling instead of ridiculous double-compiling.
    DOCOL ,
    \ And now we return to compiling mode for the DOES> RHS.
;


\ Allocates n bytes of memory. If n is negative, frees the memory instead.
: ALLOT ( n -- )
    HERE +!     \ add n to HERE
  ;

\ Cell and character conversion functions.
: CELLS ( n -- n ) 4 * ;
: CELL+ ( a-addr1 -- a-addr2 ) 4 + ;
: CHAR+ ( c-addr1 -- c-addr2 ) 1+ ;
: CHARS ( n1 -- n2 ) ;

\ The three core defining words, which make use of DOES>.
: CONSTANT  ( x -- ) ( -- x ) CREATE , DOES> @ ;

: VARIABLE ( -- ) ( -- a-addr )
    CREATE 0 , ; \ No need for a DOES> here, returning the address is already the right thing.

: ARRAY ( n -- ) ( index -- a-addr )
    CREATE CELLS ALLOT DOES> SWAP CELLS + ;

\ Turns an address for a counted string into an address/length pair.
: COUNT ( c-addr1 -- c-addr2 u ) DUP C@ SWAP 1+ SWAP ;

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
    DUP >R \ Set aside the xt.
    >BODY HERE ! \ And adjust HERE to point at the top of the body again.
    DOCOL ,
    R> \ Restore the xt to TOS.
    ] \ compile the definition.
;

\ Parse the next word, find its xt, and compile a literal for it.
: ['] IMMEDIATE WORD FIND DROP [COMPILE] LITERAL ;

\ Create a small region of loop metadata.
HERE @ 8 CELLS ALLOT
VARIABLE (LOOP-SP)
(LOOP-SP) ! \ (LOOP-SP) now holds the address of the lowest entry in the loop stack.

: DO IMMEDIATE \ lim start --
  HERE @
  ' 2DUP ,
  ' SWAP , ' >R , ' >R ,
  ' > ,
  ' 0BRANCH ,
  HERE @ \ location of offset
  DUP (LOOP-SP) @ ! \ Store that branch offset into (LOOP-SP).
  1 CELLS (LOOP-SP) +! \ And bump the pointer.
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
  1 CELLS NEGATE (LOOP-SP) +! \ Drop this entry from the (LOOP-SP) stack.
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

\ LEAVE jumps out of a DO LOOP.
\ We implement it by capturing the offset of the top branch above, and jumping to it.
\ That instruction is a 0-branch, so we compile code that will push a 0 and jump to it.
: LEAVE IMMEDIATE \ Runtime: -- , Immediate: (top jump -- top jump)
    ' DEBUG ,
    ' LIT , 0 , ' BRANCH , \ Add a 0 and then the branch.
    \ top jump
    (LOOP-SP) @ 4 - \ top jump loop-entry
    @ 4 - \ top jump branch-address
    HERE @ - \ top jump offset
    , \ Compute and compile the (negative) offset. ( top jump )
;


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
    ELSE
        34 PARSE \ addr len
        2DUP \ addr len addr len
        HERE @ SWAP \ addr len src dst len
        CMOVE \ addr len
        NIP HERE @ \ len addr
        SWAP
    THEN
;

: ." IMMEDIATE ( -- )
    STATE @ IF \ compiling?
        [COMPILE] S"
        ' TYPE ,
    ELSE
        \ Just read and print
        34 PARSE \ addr len
        TYPE
    THEN
;

\ Lame duplication. Oh well.
: .( IMMEDIATE ( -- addr len )
    STATE @ IF \ compiling?
        ' LITSTRING ,
        41 PARSE \ addr len
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
        ' TYPE ,
    ELSE
        41 PARSE TYPE
    THEN
;


\ Empties both stacks and returns to a pristine state.
: ABORT ( ... -- ) S0 @ DSP!   QUIT ;

\ Parses a string at compile-time. At run-time prints it and aborts if the test value is nonzero.
\ This is one of the most meta things I've ever written.
: ABORT" IMMEDIATE ( ... "ccc<quote>" -- )
    [COMPILE] IF
        [COMPILE] S"
        ' TYPE ,
        ' CR ,
        ' ABORT ,
    [COMPILE] THEN
;


: ABS ( n -- u ) DUP 0< IF NEGATE THEN ;

\ Reads up to n1 characters into c-addr. Echoes them as they come in.
\ Returns the number of characters actually read. Stops on line termination.
\ XXX: This is slightly busted: it won't return until you press Enter, even if you
\ overrun the buffer.
: ACCEPT ( c-addr +n1 - +n2 )
    DUP >R \ Set aside the original length for later.
    BEGIN DUP 0 > WHILE \ addr remaining
        KEY     \ addr rem key
        DUP 10 = IF
            \ Exit early.
            DROP NIP \ rem
            R> SWAP - \ diff
            EXIT
        THEN
        ROT \ rem key addr
        2DUP C! \ rem key addr
        1+ ROT \ key addr' rem
        1- ROT DROP \ addr' rem'
        DUP .
    REPEAT
    \ If we get here, we ran out of space, so return  the original length.
    2DROP R> \ n1
;

\ Compares two strings for (case sensitive) equality.
: STR= ( c-addr2 u2 c-addr1 u1 -- ? )
    ROT  \ a2 a1 u1 u2
    OVER <> IF DROP 2DROP FALSE EXIT THEN \ Bail if the lengths are different.
    \ Need to actually compare the strings.
    0 DO \ a2 a1
        2DUP I + C@ \ a2 a1 a2 c1
        SWAP I + C@ \ a2 a1 c1 c2
        <> IF 2DROP FALSE UNLOOP EXIT THEN
    LOOP
    2DROP TRUE
;


: ENVIRONMENT? ( c-addr u -- false | i.x true )
    2DUP S" /COUNTED-STRING" STR= IF 2DROP 2000000000 TRUE EXIT THEN
    2DUP S" /HOLD" STR= IF 2DROP FALSE EXIT THEN
    2DUP S" /PAD" STR= IF 2DROP 10000 TRUE EXIT THEN
    2DUP S" ADDRESS-UNIT-BITS" STR= IF 2DROP 32 TRUE EXIT THEN
    2DUP S" FLOORED" STR= IF 2DROP FALSE TRUE EXIT THEN \ TODO Check which division I do.
    2DUP S" MAX-CHAR" STR= IF 2DROP 127 TRUE EXIT THEN
    2DUP S" MAX-D" STR= IF 2DROP -1 2147483647 TRUE EXIT THEN
    2DUP S" MAX-N" STR= IF 2DROP 2147483647 TRUE EXIT THEN
    2DUP S" MAX-U" STR= IF 2DROP -1 TRUE EXIT THEN
    2DUP S" MAX-UD" STR= IF 2DROP -1 -1 TRUE EXIT THEN
    2DUP S" RETURN-STACK-CELLS" STR= IF 2DROP 1024 TRUE EXIT THEN
    2DUP S" STACK-CELLS" STR= IF 2DROP 256 1024 * TRUE EXIT THEN
    2DROP FALSE
;

: FILL ( c-addr u char -- )
    -ROT \ char c-addr u
    OVER + SWAP DO \ char
        DUP I C!
    LOOP
    DROP
;

\ MOVE works in address units, and CMOVE in characters. But that's the same size on ARM, so
\ MOVE is just an alias.
: MOVE CMOVE ;

\ Converts a single-cell value to a double-cell value with same value.
: S>D DUP 0> IF 0 ELSE -1 THEN ;


: 2>R SWAP >R  >R ;
: 2R> R> R> SWAP ;
: 2R@ R> R> 2DUP >R >R SWAP ;


\ A deferred word is basically a variable that executes when called.
\ Its cell holds the xt to be executed, so it must be read, then executed.
: DEFER CREATE 0 , DOES> @ EXECUTE ;
: DEFER@ ( xt1 -- xt2 ) >DATA @ ;
: DEFER! ( xt2 xt1 -- ) >DATA ! ;

\ Gets the name of the next word, looks it up, and returns its target xt.
\ TODO: This is busted, it needs to be smart about the current state. it should return the value in immediate mode, and compile it in compilation mode.
: ACTION-OF IMMEDIATE ( "<spaces>name" -- xt )
    WORD FIND DROP >DATA @ \ xt
    STATE @ IF ' LIT , , THEN \ compile it if we're compiling.
;

: IS IMMEDIATE ( xt "<spaces>name" -- )
    WORD FIND DROP >DATA
    STATE @ IF ' LIT , , ' ! , \ If we're compiling, compile the literal address and a store.
    ELSE ! THEN \ And if interpreting, store it now.
;


\ Creates a region of the given size; does not need to do anything on name execution.
: BUFFER: CREATE ALLOT ALIGN ;


: C" IMMEDIATE ( -- addr )
    STATE @ IF \ compiling?
        ' LITCSTRING ,
        34 PARSE \ addr len
        HERE @ 1+  \ src len dst
        OVER C,    \ src len dst - now with the length compiled in.
        SWAP       \ src dst len
        DUP HERE +! \ src dst len - move HERE to make room

        0 DO \ src dst
            OVER I + C@ \ src dst val
            OVER I + C! \ src dst
        LOOP
        2DROP
        ALIGN
    THEN
;

\ Convert the xt into a codeword pointer, then compile it.
: COMPILE, ( xt -- ) >BODY , ;

: ERASE ( addr u -- ) DUP 0> IF OVER + SWAP DO 0 I C! LOOP ;


\ Creates a new word with the given name, which when executed deletes itself and everything
\ after it.
\ The hacky bit is that it has to work backwards from its data pointer to its link pointer.
: MARKER ( "<spaces>name" -- )
    CREATE
    LATEST @      \ The link pointer of the new word. Same as its xt.
    DUP >BODY 8 + \ link body' - points at the literal value, which is current the data field.
    !             \ -- now this definition pushes its link pointer instead of its data pointer.
    DOES>
    DUP @ LATEST ! \ Read the link pointer of the marker, and set LATEST to that value.
    HERE ! \ Also set HERE equal to my link pointer.
;

512 BUFFER: PAD

\ A value returns its payload.
: VALUE ( x -- ) CREATE , DOES> @ ;
: TO IMMEDIATE
    WORD FIND DROP >DATA \ data pointer on the stack.
    STATE @ IF \ compiling
        ' LIT , \ compile a literal for the data area.
        ,
        ' ! ,   \ and compile a store.
    ELSE
        !
    THEN
;



\ FILE ACCESS WORDS
\ There's no difference between text and binary modes on Linux, apparently?
: BIN ;

: CLOSE-FILE ( fileid -- ior )
    6 SYSCALL1 ; \ Call close(2) with the fileid. Returns 0 on success, -1 otherwise.

\ A helper method that copies a two-cell string to a C string. Uses HERE.
: C-STR ( c-addr u -- addr )
    DUP >R \ Stash the length for later.
    HERE @ SWAP \ src dst len
    MOVE
    0   HERE @ R> +   C! \ Write the 0 terminator into the C string.
    HERE @ \ And return the address of the C-string.
;

: CREATE-FILE ( c-addr u access -- fileid ior )
    >R C-STR \ c-addr
    R> 64 OR 512 OR \ Mix in O_CREATE and O_TRUNC. ( c-addr access )
    SWAP \ access c-addr
    \ Create the mode: 0644 by default. = 110 100 100 = 0x1a4 = 420
    420 -ROT \ mode access c-addr
    5 SYSCALL3
    DUP 0< IF 0 SWAP ( fileid ior )
    ELSE 0 ( fileid ior ) THEN
;

: DELETE-FILE ( c-addr u -- ior )
    C-STR 10 SYSCALL1 ; \ unlink(2) returns 0 on success, -1 on failure.

\ Calls lseek(2) with whence = CUR, offset 0. That returns the current offset.
\ NB: The returned size is double-cell.
: FILE-POSITION ( fileid -- ud ior )
    >R
    1  \ SEEK_CUR
    0  \ offset of 0
    R> \ fileid
    19 \ lseek(2)
    SYSCALL3
    DUP 0< IF DROP 0 0 -1 ( ud ior )
    ELSE 0 0 ( ud ior ) THEN
;

\ To do an fstat(2) we need a stat buffer. I'll use HERE@ for that.
: FILE-SIZE ( fileid -- ud ior )
    HERE @ SWAP \ buf fileid
    108 SYSCALL2 \ fstat(2)  ( ior )   XXX The stat syscalls are a bit of a mess; see manual.
    \ By experimentation, it's the 6th word in that has the length.
    HERE @ 5 CELLS + @ 0 \ ( ior size_lo size_hi )
    ROT \ size_d ior
;

\ TODO: Implement INCLUDE-FILE, INCLUDED, REQUIRE

: OPEN-FILE ( c-addr u access -- fileid ior )
    >R C-STR R> SWAP \ access c-str
    5 SYSCALL2 \ ret
    DUP 0< IF DROP 0 -1
    ELSE 0 ( fileid ior ) THEN
;

: R/O ( -- access ) 0 ;
: R/W ( -- access ) 2 ;
: W/O ( -- access ) 1 ;


: READ-FILE ( c-addr u1 fileid -- u2 ior )
    >R SWAP R> \ u1 c-addr fileid
    3 SYSCALL3 \ len
    DUP 0< IF -1 ( u2 ior )
    ELSE 0 ( u2 ior ) THEN
;

\ TODO: Implement READ-LINE.

: REPOSITION-FILE ( ud fileid -- ior )
    NIP \ u fileid
    0 -ROT     \ 0 u fileid \ -- bury the whence value SEEK_SET.
    19 SYSCALL3 \ ret
    -1 = IF -1 ELSE 0 THEN \ Return 0 on success, not the file position.
;

: RESIZE-FILE ( ud fileid -- ior )
    NIP \ u fileid
    93 SYSCALL2 \ ftruncate(2)
    \ Returns 0 on success, -1 on error.
;

: WRITE-FILE ( c-addr u fileid -- ior )
    >R SWAP R> \ u c-addr fileid
    4 SYSCALL3 \ write(2)
    -1 = IF -1 ELSE 0 THEN \ Return 0 on success, -1 on failure.
;



\ Turns a codeword/body pointer (ie. from ') into an xt, by walking the link list and
\ returning the first address in it lower than the named value.
: >XT ( addr -- xt ) LATEST @ BEGIN 2DUP < WHILE @ REPEAT NIP ;


: DIE ( -- ) 0 1 SYSCALL1 ; \ exit(2)

: WELCOME
    ." FORTH ARM" CR
    ." by Braden Shepherdson" CR
    ." version " VERSION . CR
;
WELCOME



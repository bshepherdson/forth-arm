.text

/*
Register outline
sp/r13 is used as the data stack pointer.
r10 is used for the return stack pointer. (J in DCPU-16)
r11 is used for the next Forth word. (I in DCPU-16)

Some macros for common parts of the Forth system.
*/

/* NEXT macro Completes this Forth word and moves on to the next one */
.macro NEXT
ldr r0, [r11]
add r11, #4
ldr pc, [r0]
.endm

/* PUSHRSP macro: Needs to store the current Forth word (R11) to the position pointed to by RSP (r10), and decrement RSP. */
/* This is used to push the execution state onto the return stack. */
.macro PUSHRSP
stmdb r10!, {r11}
.endm

/* POPRSP macro: Retrieves the value from the top of the return stack into r11 (next Forth word). */
.macro POPRSP
ldmia r10!, {r11}
.endm


.set F_IMMED, 0x80
.set F_HIDDEN, 0x20
.set F_LENMASK, 0x1f
.set F_HIDDEN_AND_LENMASK, 0x3f


/* Actual entry point */
b main



/* DOCOL. The interpreter code for running functions written in Forth. Expects r0 to be the codeword address! */
DOCOL:
PUSHRSP
add r0, #4
mov r11, r0
NEXT


/*
Forth word definitions.
The link pointer is the first word after the name_FOO label.
It points at the name_BAR of the wod before it, or 0 for the last in the chain.

The structure of a Forth word is as follows:
0 - link pointer, points to previous word, or 0.
4 - size byte - specifies the size of the word's name in bytes
5 - start of word... (no 0 terminator)
N - code pointer. word-aligned.
*/

name_DROP:
.word 0
.byte 4
.ascii "DROP"
.align
DROP:
.word code_DROP
code_DROP:
add sp, sp, #4
NEXT

name_SWAP:
.word name_DROP
.byte 4
.ascii "SWAP"
.align
SWAP:
.word code_SWAP
code_SWAP:
pop {r0,r1}
push {r0}
push {r1}
NEXT

name_DUP:
.word name_SWAP
.byte 3
.ascii "DUP"
.align
DUP:
.word code_DUP
code_DUP:
ldr r0, [sp]
push {r0}
NEXT

name_OVER:
.word name_DUP
.byte 4
.ascii "OVER"
.align
OVER:
.word code_OVER
code_OVER:
ldr r0, [sp,#4]
push {r0}
NEXT


name_ROT:
.word name_OVER
.byte 3
.ascii "ROT"
.align
ROT:
.word code_ROT
code_ROT:
pop {r0,r1,r2} /* c b a */
push {r0,r1} /* b a c (grab) */
push {r2}
NEXT

name_NEGROT:
.word name_ROT
.byte 4
.ascii "-ROT"
.align
NEGROT:
.word code_NEGROT
code_NEGROT:
pop {r0,r1,r2} /* c b a */
push {r0}
push {r1,r2} /* a c b (bury) */
NEXT


name_TWODROP:
.word name_NEGROT
.byte 5
.ascii "2DROP"
.align
TWODROP:
.word code_TWODROP
code_TWODROP:
add sp, sp, #8
NEXT

name_TWODUP:
.word name_TWODROP
.byte 4
.ascii "2DUP"
.align
TWODUP:
.word code_TWODUP
code_TWODUP:
ldr r0, [sp]
ldr r1, [sp,#4]
push {r0,r1}
NEXT

name_TWOSWAP:
.word name_TWODUP
.byte 5
.ascii "2SWAP"
.align
TWOSWAP:
.word code_TWOSWAP
code_TWOSWAP:
pop {r0,r1,r2,r3}
push {r0,r1}
push {r2,r3}
NEXT

name_TWOOVER:
.word name_TWOSWAP
.byte 5
.ascii "2OVER"
.align
TWOOVER:
.word code_TWOOVER
code_TWOOVER:
ldr r0, [sp,#8]
ldr r1, [sp,#12]
push {r0,r1}
NEXT


name_QDUP:
.word name_TWOOVER
.byte 4
.ascii "?DUP"
.align
QDUP:
.word code_QDUP
code_QDUP:
ldr r0, [sp]
cmp r0, #0
pushne {r0}
NEXT



name_INCR:
.word name_QDUP
.byte 2
.ascii "1+"
.align
INCR:
.word code_INCR
code_INCR:
pop {r0}
add r0, r0, #1
push {r0}
NEXT

name_DECR:
.word name_INCR
.byte 2
.ascii "1-"
.align
DECR:
.word code_DECR
code_DECR:
pop {r0}
sub r0, r0, #1
push {r0}
NEXT

name_INCR4:
.word name_DECR
.byte 2
.ascii "4+"
.align
INCR4:
.word code_INCR4
code_INCR4:
pop {r0}
add r0, r0, #4
push {r0}
NEXT

name_DECR4:
.word name_INCR4
.byte 2
.ascii "4-"
.align
DECR4:
.word code_DECR4
code_DECR4:
pop {r0}
sub r0, r0, #4
push {r0}
NEXT


name_ADD:
.word name_DECR4
.byte 1
.ascii "+"
.align
ADD:
.word code_ADD
code_ADD:
pop {r0,r1}
add r0, r0, r1
push {r0}
NEXT

name_SUB:
.word name_ADD
.byte 1
.ascii "-"
.align
SUB:
.word code_SUB
code_SUB:
pop {r0,r1}
sub r0, r1, r0 /* deeper on the stack minus shallower */
push {r0}
NEXT

name_MUL:
.word name_SUB
.byte 1
.ascii "*"
.align
MUL:
.word code_MUL
code_MUL:
pop {r0,r1}
mul r2, r0, r1
push {r2}
NEXT

/* No integer division instructions on the ARM. Using code taken from Stack Overflow. */

name_DIVMOD:
.word name_MUL
.byte 4
.ascii "/MOD"
.align
DIVMOD:
.word code_DIVMOD
code_DIVMOD:
pop {r2} /* denominator */
pop {r0} /* numerator */
/* This algorithm expects positive numerator, negative, nonzero denominator. */
/* I will adjust accordingly, and negate afterwards if necessary. */
/* Division by 0 results in a punch in the face, and syscall to exit with code 8 */
cmp r2, #0
  beq _div_by_zero

/* Now there are four cases: */
/* - p/p -> negate denom, clean return. */
/* - p/n -> clean call, negate return */
/* - n/p -> negate both, negate return */
/* - n/n -> negate num, clean return */
/* I'm not terribly confident in these, tests required. */

cmp r0, #0
  bge _div_p

_div_n:
cmp r2, #0
  ble _div_np

_div_nn:
/* negate numerator, clean return. */
rsb r0, r0, #0 /* subtract from 0 --> negate */
b _div_trampoline

_div_np:
/* negate both, negate return */
rsb r0, r0, #0
rsb r2, r2, #0
bl _div_main
rsb r0, r0, #0
b _div_return

_div_p:
cmp r2, #0
  ble _div_pn

_div_pp:
/* negate denom, clean return */
rsb r2, r2, #0
b _div_trampoline

_div_pn:
/* clean call, negate return */
bl _div_main
rsb r0, r0, #0
b _div_return


_div_main:
/* r0 = num, r2 = denom. r0 +ve, r2 -ve. quot in r0, rem in r1. */

mov r1, #0
adds r0, r0, r0
.rept 32
  adcs r1, r2, r1, lsl #1
  subcc r1, r1, r2
  adcs r0, r0, r0
.endr
bx lr

/* A jumping off point that allows easy branching and returning to the _div_return code. */
_div_trampoline:
bl _div_main
_div_return:
/* push the quotient and remainder in the right order. */
/* quotient on the top, mod underneath */
push {r0,r1}
NEXT



_div_by_zero:
mov r0, #8
mov r7, #__NR_exit
swi #0



name_SHL:
.word name_DIVMOD
.byte 6
.ascii "LSHIFT"
.align
SHL:
.word code_SHL
code_SHL:
pop {r0,r1}
lsl r0, r1, r0
push {r0}
NEXT

/* LOGICAL shift right */
name_SHR:
.word name_SHL
.byte 6
.ascii "RSHIFT"
.align
SHR:
.word code_SHR
code_SHR:
pop {r0,r1}
lsr r0, r1, r0
push {r0}
NEXT

/* ARITHMETIC shift right */
name_ASR:
.word name_SHR
.byte 7
.ascii "ARSHIFT"
.align
ASR:
.word code_ASR
code_ASR:
pop {r0, r1}
asr r0, r1, r0
push {r0}
NEXT


name_EQU:
.word name_ASR
.byte 1
.ascii "="
.align
EQU:
.word code_EQU
code_EQU:
pop {r0,r1}
mov r2, #0
cmp r0, r1
subeq r2, r2, #1
push {r2}
NEXT


name_NEQU:
.word name_EQU
.byte 2
.ascii "<>"
.align
NEQU:
.word code_NEQU
code_NEQU:
pop {r0,r1}
mov r2, #0
cmp r0, r1
subne r2, r2, #1
push {r2}
NEXT


name_GT:
.word name_NEQU
.byte 1
.ascii ">"
.align
GT:
.word code_GT
code_GT:
pop {r0,r1}
mov r2, #0
cmp r1, r0
subgt r2, r2, #1
push {r2}
NEXT

name_GTU:
.word name_GT
.byte 2
.ascii ">U"
.align
GTU:
.word code_GTU
code_GTU:
pop {r0,r1}
mov r2, #0
cmp r1, r0
subhi r2, r2, #1
push {r2}
NEXT


name_LT:
.word name_GTU
.byte 1
.ascii "<"
.align
LT:
.word code_LT
code_LT:
pop {r0,r1}
mov r2, #0
cmp r1, r0
sublt r2, r2, #1
push {r2}
NEXT

name_LTU:
.word name_LT
.byte 2
.ascii "<U"
.align
LTU:
.word code_LTU
code_LTU:
pop {r0,r1}
mov r2, #0
cmp r0, r1
subhi r2, r2, #1
push {r2}
NEXT


name_GE:
.word name_LTU
.byte 2
.ascii ">="
.align
GE:
.word code_GE
code_GE:
pop {r0,r1}
mov r2, #0
cmp r1, r0
subge r2, r2, #1
push {r2}
NEXT


name_LE:
.word name_GE
.byte 2
.ascii "<="
.align
LE:
.word code_LE
code_LE:
pop {r0,r1}
mov r2, #0
cmp r1, r0
suble r2, r2, #1
push {r2}
NEXT



name_ZEQU:
.word name_LE
.byte 2
.ascii "0="
.align
ZEQU:
.word code_ZEQU
code_ZEQU:
pop {r0}
mov r2, #0
cmp r0, #0
subeq r2, r2, #1
push {r2}
NEXT


name_ZNEQU:
.word name_ZEQU
.byte 3
.ascii "0<>"
.align
ZNEQU:
.word code_ZNEQU
code_ZNEQU:
pop {r0}
mov r2, #0
cmp r0, #0
subne r2, r2, #1
push {r2}
NEXT


name_ZLT:
.word name_ZNEQU
.byte 2
.ascii "0<"
.align
ZLT:
.word code_ZLT
code_ZLT:
pop {r0}
mov r2, #0
cmp r0, #0
submi r2, r2, #1
push {r2}
NEXT


name_ZGT:
.word name_ZLT
.byte 2
.ascii "0>"
.align
ZGT:
.word code_ZGT
code_ZGT:
pop {r0}
mov r2, #0
cmp r0, #0
subgt r2, r2, #1
push {r2}
NEXT


name_ZLE:
.word name_ZGT
.byte 3
.ascii "0<="
.align
ZLE:
.word code_ZLE
code_ZLE:
pop {r0}
mov r2, #0
rsbs r0, r0, #0
subpl r2, r2, #1
push {r2}
NEXT


name_ZGE:
.word name_ZLE
.byte 3
.ascii "0>="
.align
ZGE:
.word code_ZGE
code_ZGE:
pop {r0}
mov r2, #0
cmp r0, #0
subge r2, r2, #1
push {r2}
NEXT



name_AND:
.word name_ZGE
.byte 3
.ascii "AND"
.align
AND:
.word code_AND
code_AND:
pop {r0,r1}
and r0, r0, r1
push {r0}
NEXT

name_OR:
.word name_AND
.byte 2
.ascii "OR"
.align
OR:
.word code_OR
code_OR:
pop {r0,r1}
orr r0, r0, r1
push {r0}
NEXT

name_XOR:
.word name_OR
.byte 3
.ascii "XOR"
.align
XOR:
.word code_XOR
code_XOR:
pop {r0,r1}
eor r0, r0, r1
push {r0}
NEXT

name_INVERT:
.word name_XOR
.byte 6
.ascii "INVERT"
.align
INVERT:
.word code_INVERT
code_INVERT:
pop {r0}
mvn r1, #0
eor r0, r0, r1
push {r0}
NEXT




name_EXIT:
.word name_INVERT
.byte 4
.ascii "EXIT"
.align
EXIT:
.word code_EXIT
code_EXIT:
POPRSP
NEXT


name_LIT:
.word name_EXIT
.byte 3
.ascii "LIT"
.align
LIT:
.word code_LIT
code_LIT:
ldr r0, [r11]
push {r0}
add r11, r11, #4
NEXT


name_STORE:
.word name_LIT
.byte 1
.ascii "!"
.align
STORE:
.word code_STORE
code_STORE:
pop {r0,r1} /* storage address, value to store */
str r1, [r0]
NEXT

name_FETCH:
.word name_STORE
.byte 1
.ascii "@"
.align
FETCH:
.word code_FETCH
code_FETCH:
pop {r0}
ldr r0, [r0]
push {r0}
NEXT

name_ADDSTORE:
.word name_FETCH
.byte 2
.ascii "+!"
.align
ADDSTORE:
.word code_ADDSTORE
code_ADDSTORE:
pop {r0,r1} /* storage address, adjustment value */
ldr r2, [r0]
add r2, r2, r1
str r2, [r0]
NEXT

name_SUBSTORE:
.word name_ADDSTORE
.byte 2
.ascii "-!"
.align
SUBSTORE:
.word code_SUBSTORE
code_SUBSTORE:
pop {r0,r1} /* storage address, adjustment value */
ldr r2, [r0]
sub r2, r2, r1
str r2, [r0]
NEXT

name_STOREBYTE:
.word name_SUBSTORE
.byte 2
.ascii "C!"
.align
STOREBYTE:
.word code_STOREBYTE
code_STOREBYTE:
pop {r0,r1}
strb r1, [r0]
NEXT

name_FETCHBYTE:
.word name_STOREBYTE
.byte 2
.ascii "C@"
.align
FETCHBYTE:
.word code_FETCHBYTE
code_FETCHBYTE:
pop {r0}
mov r1, #0
ldrb r1, [r0]
push {r1}
NEXT

name_CCOPY:
.word name_FETCHBYTE
.byte 4
.ascii "C@C!"
.align
CCOPY:
.word code_CCOPY
code_CCOPY:
pop {r0,r1} /* source addr, destination addr */
ldrb r2, [r0]
strb r2, [r1]
add r0, r0, #1
add r1, r1, #1
push {r0,r1}
NEXT

name_CMOVE:
.word name_CCOPY
.byte 5
.ascii "CMOVE"
.align
CMOVE:
.word code_CMOVE
code_CMOVE:
pop {r4} /* length */
pop {r1} /* destination address */
pop {r0} /* source address */
_cmove_loop:
ldrb r2, [r0]
strb r2, [r1]
add r0, r0, #1
add r1, r1, #1
subs r4, r4, #1
  bgt _cmove_loop
NEXT


/* Some halfword read/write functions */
name_FETCHHALFWORD:
.word name_CMOVE
.byte 2
.ascii "H@"
.align
FETCHHALFWORD:
.word code_FETCHHALFWORD
code_FETCHHALFWORD:
pop {r0} /* the address */
ldrh r0, [r0]
push {r0}
NEXT

name_FETCHHALFWORD_SIGNED:
.word name_FETCHHALFWORD
.byte 3
.ascii "H@S"
.align
FETCHHALFWORD_SIGNED:
.word code_FETCHHALFWORD_SIGNED
code_FETCHHALFWORD_SIGNED:
pop {r0}
ldrsh r0, [r0]
push {r0}
NEXT

name_STOREHALFWORD:
.word name_FETCHHALFWORD_SIGNED
.byte 2
.ascii "H!"
.align
STOREHALFWORD:
.word code_STOREHALFWORD
code_STOREHALFWORD:
pop {r0,r1} /* the address and the value */
strh r1, [r0]
NEXT


name_BITSWAPHALFWORD:
.word name_STOREHALFWORD
.byte 8
.ascii "BITSWAPH"
.align
BITSWAPH:
.word code_BITSWAPH
code_BITSWAPH:
pop {r0}
rev16 r0, r0
push {r0}
NEXT

name_BITSWAPHALFWORD_SIGNED:
.word name_BITSWAPHALFWORD
.byte 9
.ascii "BITSWAPHS"
.align
BITSWAPHS:
.word code_BITSWAPHS
code_BITSWAPHS:
pop {r0}
revsh r0, r0
push {r0}
NEXT


name_EXTENDH:
.word name_BITSWAPHALFWORD_SIGNED
.byte 7
.ascii "EXTENDH"
.align
EXTENDH:
.word code_EXTENDH
code_EXTENDH:
pop {r0}
sxth r0, r0
push {r0}
NEXT


name_STATE:
.word name_EXTENDH
.byte 5
.ascii "STATE"
.align
STATE:
.word code_STATE
code_STATE:
ldr r0, =var_THREAD
add r0, r0, #THREAD_STATE
push {r0}
NEXT


name_LATEST:
.word name_STATE
.byte 6
.ascii "LATEST"
.align
LATEST:
.word code_LATEST
code_LATEST:
ldr r0, =var_THREAD
add r0, r0, #THREAD_LATEST
push {r0}
NEXT


name_HERE:
.word name_LATEST
.byte 4
.ascii "HERE"
.align
HERE:
.word code_HERE
code_HERE:
ldr r0, =var_THREAD
add r0, r0, #THREAD_HERE
push {r0}
NEXT

name_UNUSED:
.word name_HERE
.byte 6
.ascii "UNUSED"
.align
UNUSED:
.word code_UNUSED
code_UNUSED:
ldr r0, =var_THREAD
add r0, r0, #THREAD_HERE
ldr r0, [r0]             /* Read the HERE value proper. */
sub r0, sp, r0           /* Subtract it from SP. */
push {r0}
NEXT


name_S0:
.word name_UNUSED
.byte 2
.ascii "S0"
.align
S0:
.word code_S0
code_S0:
ldr r0, =var_THREAD
add r0, r0, #THREAD_TOP
ldr r0, [r0]
sub r0, r0, #RETURN_STACK_SIZE
push {r0}
NEXT


name_BASE:
.word name_S0
.byte 4
.ascii "BASE"
.align
BASE:
.word code_BASE
code_BASE:
ldr r0, =var_THREAD
add r0, r0, #THREAD_BASE
push {r0}
NEXT



name_VERSION:
.word name_BASE
.byte 7
.ascii "VERSION"
.align
VERSION:
.word code_VERSION
code_VERSION:
mov r0, #2
push {r0}
NEXT

name_R0:
.word name_VERSION
.byte 2
.ascii "R0"
.align
_R0:
.word code_R0
code_R0:
ldr r0, =var_THREAD
add r0, r0, #THREAD_TOP
ldr r0, [r0]
push {r0}
NEXT


name_DOCOL:
.word name_R0
.byte 5
.ascii "DOCOL"
.align
__DOCOL:
.word code_DOCOL
code_DOCOL:
ldr r0, =DOCOL
push {r0}
NEXT


name_F_IMMED:
.word name_DOCOL
.byte 7
.ascii "F_IMMED"
.align
__F_IMMED:
.word code_F_IMMED
code_F_IMMED:
mov r0, #F_IMMED
push {r0}
NEXT


name_F_HIDDEN:
.word name_F_IMMED
.byte 8
.ascii "F_HIDDEN"
.align
__F_HIDDEN:
.word code_F_HIDDEN
code_F_HIDDEN:
mov r0, #F_HIDDEN
push {r0}
NEXT

name_F_LENMASK:
.word name_F_HIDDEN
.byte 9
.ascii "F_LENMASK"
.align
__F_LENMASK:
.word code_F_LENMASK
code_F_LENMASK:
mov r0, #F_LENMASK
push {r0}
NEXT



/* Return stack words */

name_TOR:
.word name_F_LENMASK
.byte 2
.ascii ">R"
.align
TOR:
.word code_TOR
code_TOR:
mov r0, r11
pop {r11}
PUSHRSP
mov r11, r0
NEXT


name_FROMR:
.word name_TOR
.byte 2
.ascii "R>"
.align
FROMR:
.word code_FROMR
code_FROMR:
mov r0, r11
POPRSP
push {r11}
mov r11, r0
NEXT


name_RSPFETCH:
.word name_FROMR
.byte 4
.ascii "RSP@"
.align
RSPFETCH:
.word code_RSPFETCH
code_RSPFETCH:
push {r10}
NEXT


name_RSPSTORE:
.word name_RSPFETCH
.byte 4
.ascii "RSP!"
.align
RSPSTORE:
.word code_RSPSTORE
code_RSPSTORE:
pop {r10} /* I hope you know what you're doing. */
NEXT


name_RDROP:
.word name_RSPSTORE
.byte 5
.ascii "RDROP"
.align
RDROP:
.word code_RDROP
code_RDROP:
add r10, r10, #4
NEXT


name_DSPFETCH:
.word name_RDROP
.byte 4
.ascii "DSP@"
.align
DSPFETCH:
.word code_DSPFETCH
code_DSPFETCH:
mov r0, sp
push {r0}
NEXT

name_DSPSTORE:
.word name_DSPFETCH
.byte 4
.ascii "DSP!"
.align
DSPSTORE:
.word code_DSPSTORE
code_DSPSTORE:
pop {r0}
mov sp, r0
NEXT




/* Syscalls and other constants */
.set __NR_open, 5
.set __NR_close, 6
.set __NR_read, 3
.set __NR_write, 4
.set __NR_exit, 1

.set stdin, 0
.set stdout, 1
.set stderr, 2

.set O_RDONLY, 0
.set O_WRONLY, 1
.set O_RDWR, 2
.set O_CREAT, 64
.set O_TRUNC, 512


/* Input and output */


/* Raw Parser flow: */
/* 1. Read characters until the delimiter or end of parse area. */
/* 2. Adjust >IN (input_offset) */
/* 3. Return address and length of parsed input. */

/* Expects the delimiter in r0. */
/* Clobbers r0-r4 */
/* Returns the length of the found string in r0, and its address in r1. */
_parse_delimiter:
ldr r2, =input_source     /* The cell holding the input source address. */
ldr r2, [r2]              /* The input source address itself. */
add r1, r2, #SRC_POS      /* The offset of the parse buffer. */
ldr r1, [r1]              /* The actual parse buffer offset. */
add r1, r1, r2
add r1, r1, #SRC_START    /* Add the input_source position and offset to the buffer. r1 is now the parse buffer pointer. */
add r2, r2, #SRC_TOP       /* The address of the parse buffer top pointer. */
ldr r2, [r2]              /* The actual parse buffer top. */
mov r3, r1

/* Now r0 holds the delimiter, r1 the adress of the start (permanent), r2 the address of the end, */
/* and r3 the address of the start, used as the loop counter. */
_parse_delimiter_loop:
cmp r3, r2
  bge _parse_delimiter_end
/* Read the character at the current position */
ldrb r4, [r3]
cmp r0, r4
  beq _parse_delimiter_found

/* Now some special-case handling for spaces. */
/* If the delimiter is 32 (space), then accept space, or tab, or NL or CR */
cmp r0, #32
  bne _parse_delimiter_loop_bump

/* If we come to here, it is a space. So we check r4 against 10, 13, and 9 (tab). */
cmp r4, #10
  beq _parse_delimiter_found
cmp r4, #13
  beq _parse_delimiter_found
cmp r4, #9
  beq _parse_delimiter_found

/* Not found, so move on one. */
_parse_delimiter_loop_bump:
add r3, r3, #1
b _parse_delimiter_loop

/* We found the delimiter. Adjust >IN and return the string. */
_parse_delimiter_found:
sub r0, r3, r1 /* Put the difference into r0: this is the length of the string */
ldr r4, =input_source
ldr r4, [r4]   /* The base of the input source. */
add r2, r4, #SRC_POS
add r3, r3, #1  /* Bump the running pointer by one so it's after the delimiter. */
sub r3, r3, r4
sub r3, r3, #SRC_START /* Turn the pointer back into an offset. */
str r3, [r2]    /* And store the new parse offset into the input source. */
bx lr /* and return */


/* If we reached the end, it's similar to finding the delimiter, but not the same. */
/* We put the length into >IN and return the address and length. */
/* At entry here, we have r0 the delimiter, r1 the start address, r2 the end address, r3 counter (end) */
_parse_delimiter_end:
sub r0, r2, r1   /* The difference is the length. */
ldr r2, =input_source
ldr r2, [r2]     /* The input source base pointer. */
sub r3, r3, r2
sub r3, r3, #SRC_START  /* Turn the end pointer back into an offset. */
add r2, r2, #SRC_POS
str r3, [r2]     /* Store the end-pointer into the buffer field. That is, this source needs refilling. */
/* Now the length is in r0 and the string address in r1: so return */
bx lr



/* PARSE-WORD: Skip leading delimiters, and then parse a delimited name. */
/* Expects the delimiter in r0. */
/* Clobbers r0-r6 */
/* Returns the address in r1 and length in r0, like _parse_delimiter. */
/* NB: DOES NOT refill the buffer. That's QUIT's job. */

_parse_word:
ldr r6, =input_source
ldr r6, [r6]           /* r6 holds the input source pointer. */
add r2, r6, #SRC_POS
ldr r2, [r2]           /* r2 is the offset into the input buffer. */
add r2, r2, r6
add r2, r2, #SRC_START /* Turn the offset into a pointer: r2 is the start of the parse buffer. */
mov r3, r2             /* And r3 is the loop counter. */
add r4, r6, #SRC_TOP
ldr r4, [r4]           /* and r4 is the address above the buffer */

/* Now we loop over whitespace characters until a non-whitespace character or end-of-buffer. */
_parse_word_loop:
cmp r3, r4
  bge _parse_word_end

ldrb r5, [r3] /* Read the character */
cmp r5, r0
  bne _parse_word_nondelim

add r3, r3, #1
b _parse_word_loop

_parse_word_nondelim:
/* If we get here, we found a non-delimiter character. Update input_offset and call parse_delimiter. */
sub r3, r3, r6
sub r3, r3, #SRC_START /* Turn our r3 loop counter back into an offset. */
add r2, r6, #SRC_POS
str r3, [r2]        /* Write the new source position (r3) into the input source. */
/* Now tail-call parse-delimiter. The delimiter is still in r0. */
b _parse_delimiter


/* And if we reached the end of the buffer, we return a length of 0 and the address of the end of the buffer. */
/* Also needs to update the buffer pointer in the input source. */
_parse_word_end:
sub r4, r4, r6
sub r4, r4, #SRC_START /* Turn the top pointer (r4) into an offset. */
add r2, r6, #SRC_POS
str r4, [r2]        /* And store it into the position field. */
mov r0, #0
mov r1, r6    /* Return a length of 0. The pointer is undefined; I use the input source (r6) arbitrarily. */
bx lr



/* Now we define the Forth words for the above two operations. */
name_PARSE:
.word name_DSPSTORE
.byte 5
.ascii "PARSE"
.align
PARSE:
.word code_PARSE
code_PARSE:
/* Get the delimiter from the stack into r0. */
pop {r0}
_parse_inner:
bl _parse_delimiter
/* Now r0 is the length and r1 the address. */
push {r1}
push {r0}
NEXT

name_PARSE_NAME:
.word name_PARSE
.byte 10
.ascii "PARSE-NAME"
.align
PARSE_NAME:
.word code_PARSE_NAME
code_PARSE_NAME:
mov r0, #32
bl _parse_word
push {r1}
push {r0}
NEXT


name_WORD:
.word name_PARSE_NAME
.byte 4
.ascii "WORD"
.align
WORD:
.word code_WORD
code_WORD:
mov r0, #32
bl _parse_word /* r0 = length, r1 = address */

ldr r2, =var_THREAD
add r2, r2, #THREAD_HERE
ldr r2, [r2]      /* Using HERE area as temporary space. */
push {r2}         /* That's always the return value from WORD, so push it now. */
strb r0, [r2]     /* Store the length byte into the beginning of the space. */
add r2, r2, #1

_word_loop:
cmp r0, #0
  beq _word_done

ldrb r3, [r1], #1  /* Post-incrementing will move r1 and r2. */
strb r3, [r2], #1
sub r0, r0, #1
b _word_loop

_word_done:
NEXT


name_KEY:
.word name_WORD
.byte 3
.ascii "KEY"
.align
KEY:
.word code_KEY
code_KEY:
/* Read a key directly from the keyboard. */
/* Make the system call to read 1 byte from stdin. */
mov r7, #__NR_read
mov r0, #stdin
ldr r1, =_key_buffer
mov r2, #1
swi #0
/* Now r0 holds the number of characters read. */
/* TODO: Handle this better? It's just being assumed that I got my character. */
ldr r1, =_key_buffer
ldrb r1, [r1]
push {r1}
NEXT



name_NOBUF:
.word name_KEY
.byte 7
.ascii "(NOBUF)"
.align
NOBUF:
.word code_NOBUF
code_NOBUF:
/* Calls tcsetattr() to disable ICANON and ECHO modes. */
/* TODO: Capture SIGKILL and reset the terminal. */
ldr r7, =var_THREAD
add r7, r7, #THREAD_HERE
ldr r7, [r7]
mov r1, r7
mov r0, #stdin
bl tcgetattr /* Returns nothing, but sets the structure. */
/* Alternative implementation: cfmakeraw() */
mov r0, r7
bl cfmakeraw
/* add r0, r7, #12  /* The value we want is 12 bytes in. */
/* mvn r1, #10      /* This mask disables ICANON and ECHO. */

/* ldr r2, [r0]
/* and r2, r2, r1 /* Mask out the bits. */
/* str r2, [r0]   /* And write it back. */

mov r0, #stdin
mov r1, #0      /* This is TCSANOW. */
mov r2, r7      /* And the pointer to the structure. */
bl tcsetattr /* Also returns nothing. */
NEXT


name_EMIT:
.word name_NOBUF
.byte 4
.ascii "EMIT"
.align
EMIT:
.word code_EMIT
code_EMIT:
pop {r0}
bl _emit
NEXT


/* Clobbers r0-r2, and r7 */
_emit:
push {lr}
ldr r1, =_key_buffer
strb r0, [r1]
mov r0, #stdout
mov r2, #1
mov r7, #__NR_write
swi $0
pop {pc}



name_NUMBER:
.word name_EMIT
.byte 6
.ascii "NUMBER"
.align
NUMBER:
.word code_NUMBER
code_NUMBER:
pop {r2,r3} /* length of string, start address */
bl _number
push {r0}
push {r2} /* unparsed chars, number */
NEXT


/* Expects r2 to be the length and r3 the start address of the string. */
/* Clobbers r0-r3 and returns the number in r0 and the number of unparsed characters in r3. */
_number:
mov r0, #0
mov r1, r0

cmp r2, #0 /* length 0 is an error. returns 0, I guess. */
  bxeq lr

ldr r4, =var_THREAD
add r4, r4, #THREAD_BASE
ldr r4, [r4]

/* Check if the first character is '-' */
ldrb r1, [r3]
add r3, r3, #1
push {r0} /* Push a 0 to signal positive. */
cmp r1, #0x2d
  bne _number_check_digit /* Not -, so it's just the first number. */

/* If it is negative: */
pop {r0} /* Pop the 0 */
push {r1} /* Push the 0x2d, nonzero signals negative. */
sub r2, r2, #1 /* Decrement the length */

cmp r2, #0
  bne _number_loop /* If C is nonzero, jump over error handler. */

/* length is 0, the string was just '-' */
pop {r1} /* Remove the negation flag from the stack. */
mov r2, #1 /* Unparsed characters = 1 */
bx lr


/* Loop, reading digits */
_number_loop:
mov r9, r0
mul r0, r9, r4 /* r0 *= BASE */
ldrb r1, [r3] /* Get next character */
add r3, r3, #1

_number_check_digit:
cmp r1, #'0'
  blt _number_end
cmp r1, #'9'
  ble _number_found_digit
cmp r1, #'A'
  blt _number_end

/* If we made it here, it's a letter-digit */
sub r1, r1, #7 /* 65-58 = 7, turns 'A' into '9' + 1 */

_number_found_digit:
/* Adjust to be the actual number */
sub r1, r1, #0x30

cmp r1, r4 /* Check that it's < BASE */
  blt _number_good
b _number_end


_number_good:
add r0, r0, r1
sub r2, r2, #1
cmp r2, #0
  bgt _number_loop /* loop if there's still bytes to be had */

_number_end:
pop {r1} /* Grab the negativity flag we pushed earlier. */
cmp r1, #0
  beq _number_return /* positive, just return. */

/* Negate the number (2's complement) */
mvn r3, #0
eor r0, r0, r3
add r0, r0, #1

_number_return:
bx lr


/* Dictionary lookup */

name_FIND:
.word name_NUMBER
.byte 4
.ascii "FIND"
.align
FIND:
.word code_FIND
code_FIND:
pop {r3} /* counted string */
push {r3} /* Immediately save it. */
/* Convert the counted string to a length and address. */
ldrb r2, [r3]
add r3, r3, #1
bl _find

/* Returns either the address of the dictionary entry (that is, its xt), or 0. */
/* If it was 0, then push 0 and return. */
cmp r0, #0
  beq find_nope

/* It was found, so check its immediacy. */
add r1, r0, #4
ldrb r1, [r1]
mov r2, #1
ands r1, r1, #F_IMMED
  subeq r2, r2, #2    /* They're "equal" if F_IMMED is not set. Therefore -1 if F_IMMED is clear, and 1 if set. */
pop {r6}  /* Pop the junk c-addr copy. */
push {r0}
push {r2}
NEXT

find_nope:
mov r0, #0
push {r0} /* c-addr is already on the stack; this pushes a 0 on top to signal not-found. */
NEXT


/*
Expects the length in r2, address in r3.
Returns the address of the dictionary entry in r0.
Clobbers r0-r5
*/
_find:

ldr r4, =var_THREAD
add r4, r4, #THREAD_LATEST
ldr r4, [r4]

_find_loop:
cmp r4, #0
  beq _find_failed /* NULL pointer, reached the end. */

/*
Check the length of the word.
Note that if the F_HIDDEN flag is set on the word, then by a bit of trickery this
won't find the word, since the length appears to be wrong.
*/
mov r0, #0
add r4, r4, #4 /* Advance to the flags/length field. */
ldrb r1, [r4] /* Grab that field. */
add r4, r4, #1 /* Advance to the first letter. */
and r1, r1, #F_HIDDEN_AND_LENMASK
cmp r1, r2 /* Check length against target length */
  bne _find_follow_link

/*
Same length, so now compare in detail.
r2 holds the length, use it to loop.
Use r0 and r1 for the characters.
r5 is the index we're comparing.
*/

mov r5, #0
_find_compare_loop:
cmp r2, r5
  bgt _find_continue_comparing
b _find_found /* If c == 0 then we've run out of letters. */

_find_continue_comparing:
add r0, r3, r5 /* r0 is now the address of the next letter. */
ldrb r0, [r0] /* And now the actual letter itself. */

add r1, r4, r5 /* r1 is not the address of the dictionary letter */
ldrb r1, [r1] /* and the letter itself */

add r5, r5, #1

cmp r0, r1
  bne _find_follow_link /* Not equal, follow the link pointer */
b _find_compare_loop

_find_follow_link:
ldr r4, [r4,#-5] /* Reach back to the pointer, and follow it. */
b _find_loop

_find_found:
sub r0, r4, #5 /* Put the beginning of the block into r0 */
bx lr

_find_failed:
mov r0, #0 /* Return 0 on failure. */
bx lr



name_BACKSLASH:
.word name_FIND
.byte 0x81   /* IMMED + 1 */
.ascii "\\"
.align
BACKSLASH:
.word code_BACKSLASH
code_BACKSLASH:
/* Skip everything up to end-of-line. Simple: just REFILL */
/* XXX: Might not work when input source is a block. */
bl _refill
NEXT


name_TBODY:
.word name_BACKSLASH
.byte 5
.ascii ">BODY"
.align
TBODY:
.word code_TBODY
code_TBODY:
pop {r3} /* dictionary address */
bl _tbody
push {r3}
NEXT


/*
Expects a dictionary block address in r3.
Returns address of codeword in r3.
Clobbers r0, r3
*/
_tbody:
mov r0, #0
add r3, r3, #4 /* skip over link pointer, now points to length */
ldrb r0, [r3] /* Retrieve the length+flags */
and r0, #F_LENMASK
add r3, r3, r0 /* Move it ahead to the last letter */
add r3, r3, #4 /* One more to the codeword-ish */
mvn r0, #3
and r3, r3, r0 /* Mask off the last two bits, to align. */
/* r3 is now the codeword */
bx lr

name_TDATA:
.word name_TBODY
.byte 5
.ascii ">DATA"
.align
TDATA:
.word code_TDATA
code_TDATA:
pop {r3}   /* This is an xt. That is, it points at the link pointer of a word. */
bl _tbody /* Now its the address of the codeword. */
add r3, r3, #20 /* CREATEd words look like: DOCOL, LIT, ADDR, EXIT, EXIT, data area... */
push {r3}       /* So adding 20 should move the pointer to the data area. */
NEXT


/* Compilation and defining */

name_CREATE_INT:
.word name_TDATA
.byte 8
.ascii "(CREATE)"
.align
CREATE_INT:
.word code_CREATE_INT
code_CREATE_INT:
pop {r0, r1} /* Length in r0, address in r1. */

/* Store the link pointer. */
ldr r3, =var_THREAD
add r3, r3, #THREAD_HERE
ldr r3, [r3]
ldr r4, =var_THREAD
add r4, r4, #THREAD_LATEST
ldr r4, [r4]

str r4, [r3] /* Write LATEST into the new link pointer at HERE */
ldr r4, =var_THREAD
add r4, r4, #THREAD_LATEST
str r3, [r4] /* And write the new HERE into LATEST */
add r3, r3, #4 /* Jump over the link pointer. */

/* Length byte and the word itself need storing. */
strb r0, [r3] /* store the length byte */
add r3, r3, #1 /* Move to the start of the string. */

_create_name_loop:
cmp r0, #0
  beq _create_name_loop_done
ldrb r5, [r1], #1 /* Load the next letter of the name into r5 */
strb r5, [r3], #1 /* And write it into the new word block */

sub r0, r0, #1
b _create_name_loop

/*
Move HERE to point to after the block
New value of HERE is in r3, but not aligned.
Align it:
*/
_create_name_loop_done:
add r3, r3, #3
mvn r5, #3
and r3, r3, r5

/* Now r3 is the correct HERE value where the body begins. */
/* We create a body thus: DOCOL, LIT, HERE+20, EXIT, EXIT. (See below for why two EXITs.) */
/* That means the definition's default behavior is to return its own data space address. */
ldr r5, =DOCOL
str r5, [r3], #4
ldr r5, =LIT
str r5, [r3], #4
add r5, r3, #12   /* This is the pointer after where EXIT is about to go. */
str r5, [r3], #4
ldr r5, =EXIT
str r5, [r3], #4
str r5, [r3], #4 /* We write EXIT twice, so that it can be overwritten by DOES>.  */
/* This only wastes space for CREATEd words without a DOES>. That doesn't apply to words */
/* defined using : and :NONAME, since those overwrite the entire body. */

/* And then store this new HERE pointer. */
ldr r1, =var_THREAD
add r1, r1, #THREAD_HERE
str r3, [r1]
NEXT


name_CREATE:
.word name_CREATE_INT
.byte 6
.ascii "CREATE"
.align
CREATE:
.word code_CREATE
code_CREATE:
mov r0, #32
bl _parse_word /* Length in r0, address in r1. */
push {r0, r1}
b code_CREATE_INT
/* Intentionally no NEXT */



name_COMMA:
.word name_CREATE
.byte 1
.ascii ","
.align
COMMA:
.word code_COMMA
code_COMMA:
pop {r0} /* value to store */
bl _comma
NEXT

_comma:
ldr r3, =var_THREAD
add r3, r3, #THREAD_HERE
ldr r1, [r3] /* HERE value is in r1, address in r3 */
str r0, [r1] /* Store the specified value at HERE */
add r1, r1, #4 /* Update the HERE value */
str r1, [r3] /* And store it back */
bx lr


name_LBRAC:
.word name_COMMA
.byte 0x81 /* F_IMMED | 1 */
.ascii "["
.align
LBRAC:
.word code_LBRAC
code_LBRAC:
ldr r0, =var_THREAD
add r0, r0, #THREAD_STATE
mov r1, #0
str r1, [r0] /* Update the STATE to 0 */
NEXT

name_RBRAC:
.word name_LBRAC
.byte 1
.ascii "]"
.align
RBRAC:
.word code_RBRAC
code_RBRAC:
ldr r0, =var_THREAD
add r0, r0, #THREAD_STATE
mov r1, #1
str r1, [r0] /* Update the STATE to 1 */
NEXT


name_COLON:
.word name_RBRAC
.byte 1
.ascii ":"
.align
COLON:
.word DOCOL
/* Create the header */
.word CREATE
/* Adjust the HERE value back to the body of the latest definition. */
/* This will allow it to overwrite the unneeded default CREATE implementation. */
.word LATEST
.word FETCH
.word TBODY
.word HERE
.word STORE
/* Append the codeword, DOCOL */
.word LIT
.word DOCOL
.word COMMA
/* Make the word hidden */
.word LATEST
.word FETCH
.word HIDDEN
/* Back to compile mode */
.word RBRAC
/* Return */
.word EXIT


name_SEMICOLON:
.word name_COLON
.byte 0x81 /* F_IMMED | 1 */
.ascii ";"
.align
SEMICOLON:
.word DOCOL
/* Append EXIT so the word will return. */
.word LIT
.word EXIT
.word COMMA
/* Toggle hidden flag to unhide the word. */
.word LATEST
.word FETCH
.word HIDDEN
/* Return the immediate mode */
.word LBRAC
.word EXIT


/* Put the literals table here, so it should be in reach from everywhere. */
/* This is necessary because the file is long enough to keep it out of */
/* reach from some places when it's in the default location (end of the assembly). */
.ltorg


name_IMMEDIATE:
.word name_SEMICOLON
.byte 0x89 /* F_IMMED | 9 */
.ascii "IMMEDIATE"
.align
IMMEDIATE:
.word code_IMMEDIATE
code_IMMEDIATE:
ldr r3, =var_THREAD
add r3, r3, #THREAD_LATEST
ldr r3, [r3]
add r3, r3, #4 /* Aim at length byte */
ldrb r4, [r3]  /* Get that byte */
eor r4, r4, #F_IMMED /* Toggle the IMMEDIATE bit */
strb r4, [r3]  /* And write it back */
NEXT


name_HIDDEN:
.word name_IMMEDIATE
.byte 6
.ascii "HIDDEN"
.align
HIDDEN:
.word code_HIDDEN
code_HIDDEN:
pop {r3} /* Address of the dictionary entry. */
add r3, r3, #4 /* Point at the length byte */
ldrb r4, [r3]  /* Load it */
eor r4, r4, #F_HIDDEN /* Toggle the HIDDEN bit */
strb r4, [r3]
NEXT


name_TICK:
.word name_HIDDEN
.byte 1
.ascii "'"
.align
TICK:
.word code_TICK
code_TICK:
ldr r0, [r11] /* Get the address of the next word. */
add r11, r11, #4 /* Skip it. */
push {r0} /* Push the address */
NEXT
/*
TODO - This definition only works in compiled code.
According to the jonesforth commentary, TICK can be written
with WORD, FIND and >BODY so that it'll run in immediate mode too.
*/

/* System call primitives. Expect parameters on the stack in reverse order (ie. a3 a2 a1 syscallnumber ). Pushes r0, the return value, even for void syscalls */
name_SYSCALL1:
.word name_TICK
.byte 8
.ascii "SYSCALL1"
.align
SYSCALL1:
.word code_SYSCALL1
code_SYSCALL1:
pop {r7}
pop {r0}
swi #0
push {r0}
NEXT

name_SYSCALL2:
.word name_SYSCALL1
.byte 8
.ascii "SYSCALL2"
.align
SYSCALL2:
.word code_SYSCALL2
code_SYSCALL2:
pop {r7}
pop {r0, r1}
swi #0
push {r0}
NEXT

name_SYSCALL3:
.word name_SYSCALL2
.byte 8
.ascii "SYSCALL3"
.align
SYSCALL3:
.word code_SYSCALL3
code_SYSCALL3:
pop {r7}
pop {r0, r1, r2}
swi #0
push {r0}
NEXT


name_SETSEED:
.word name_SYSCALL3
.byte 7
.ascii "SETSEED"
.align
SETSEED:
.word code_SETSEED
code_SETSEED:
pop {r0}
bl srandom
NEXT


name_RANDOM:
.word name_SETSEED
.byte 6
.ascii "RANDOM"
.align
RANDOM:
.word code_RANDOM
code_RANDOM:
bl random /* r0 holds a random integer from 0 to MAXINT */
push {r0}
NEXT


/* Branching primitives */

name_BRANCH:
.word name_RANDOM
.byte 6
.ascii "BRANCH"
.align
BRANCH:
.word code_BRANCH
code_BRANCH:
_branch_inner:
ldr r0, [r11] /* Get the value at I */
add r11, r11, r0 /* And offset by it, since it's the branch amount. */
/* Beautiful and cunning. */
NEXT


name_ZBRANCH:
.word name_BRANCH
.byte 7
.ascii "0BRANCH"
.align
ZBRANCH:
.word code_ZBRANCH
code_ZBRANCH:
pop {r0} /* Get the flag */
cmp r0, #0
  beq code_BRANCH

/* Otherwise skip over the offset */
add r11, r11, #4
NEXT



/* Literal strings */
name_LITSTRING:
.word name_ZBRANCH
.byte 9
.ascii "LITSTRING"
.align
LITSTRING:
.word code_LITSTRING
code_LITSTRING:
ldr r0, [r11] /* Get the length of the string from the next word */
add r11, r11, #4 /* And skip over it */
push {r0,r11} /* Push the length (r0) and the address of the start of the string (r11) */
add r11, r11, r0 /* Skip past the string */
/* and align */
add r11, r11, #3
mvn r0, #3
and r11, r11, r0
NEXT

name_LITCSTRING:
.word name_LITSTRING
.byte 10
.ascii "LITCSTRING"
.align
LITCSTRING:
.word code_LITCSTRING
code_LITCSTRING:
mov r0, r11
ldrb r1, [r0] /* Get the length of the string from the next byte. */
add r11, r11, #1
add r11, r11, r1 /* Get past the string. */
push {r0}   /* Push the length of the string. */
/* and align */
add r11, r11, #3
mvn r2, #3
and r11, r11, r2
NEXT


name_TYPE:
.word name_LITCSTRING
.byte 4
.ascii "TYPE"
.align
TYPE:
.word code_TYPE
code_TYPE:
pop {r8,r9} /* Length = r8, address = r9 */

cmp r8, #0
  beq _type_done

bl _type
_type_done:
NEXT

_type:
push {lr}
_type_inner:
ldrb r0, [r9] /* Get the next character */
bl _emit /* Clobbers r0-r2, r7 */
add r9, r9, #1
subs r8, r8, #1
  bgt _type_inner
pop {pc}


name_SOURCE_ID:
.word name_TYPE
.byte 9
.ascii "SOURCE-ID"
.align
SOURCE_ID:
.word code_SOURCE_ID
code_SOURCE_ID:
ldr r0, =input_source
ldr r0, [r0]  /* Returns -1 for EVALUATE, 0 for keyboard, or the fileid. */
add r1, r0, #SRC_TYPE
ldr r1, [r1]  /* r1 is the type */

cmp r1, #1
  bgt _source_id_file

/* Otherwise negate and return that. */
rsb r1, r1, #0 /* subtract from 0 --> negate */
push {r1}  /* Now 0 is still 0, but 1 turned into -1 for evaluate. */
b _source_id_done

_source_id_file:
add r2, r0, #SRC_DATA
ldr r2, [r2]
push {r2}

_source_id_done:
NEXT


name_QUIT:
.word name_SOURCE_ID
.byte 4
.ascii "QUIT"
.align
QUIT:
.word DOCOL
.word _R0
.word RSPSTORE
_quit_inner:
.word INTERPRET
.word BRANCH
.word -8
/* Deliberately no EXIT call here */



name_INTERPRET:
.word name_QUIT
.byte 9
.ascii "INTERPRET"
.align
INTERPRET:
.word code_INTERPRET
code_INTERPRET:

/* Main interpreter loop. Refills the buffer to get a line of input, then parses and executes each word. */
/* Check if there's anything more to parse. */
ldr r0, =input_source
ldr r0, [r0]      /* r0 is the input source pointer. */
add r1, r0, #SRC_POS
ldr r1, [r1]      /* r1 is the parse buffer offset. */
add r1, r1, r0
add r1, r1, #SRC_START /* r1 is now a pointer to the start of the parse area. */
add r2, r0, #SRC_TOP
ldr r2, [r2]      /* r2 is the parse buffer top. */
cmp r1, r2
  blge _interpret_refill_needed

/* Now, either way, there's more to be read. */
/* Note that the r0 and r1 from above are now invalidated. */
/* Now we begin a loop of parsing names and executing them until we run out of buffer and get 0 back. */
mov r0, #32
bl _parse_word

/* Now r0 is the length and r1 the address. */
cmp r0, #0
  beq code_INTERPRET

/* If not, we've got a valid word here to be parsed. */
/* It's not a literal number (at least not yet) */
ldr r2, =var_THREAD
add r2, r2, #THREAD_interpret_is_lit
mov r3, #0
str r3, [r2]

/* Find:
Expects the length in r2, address in r3.
Returns the address of the dictionary entry in r0.
Clobbers r0-r5
*/
mov r2, r0
mov r3, r1
mov r8, r0 /* Save r0, the length */
mov r9, r1 /* Save r1, the address */
bl _find

cmp r0, #0
  beq _interpret_not_in_dict

/* In the dictionary. Check if it's an IMMEDIATE codeword */
add r2, r0, #4 /* r2 is the address of the dictionary header's length byte */
ldrb r9, [r2] /* Set aside the actual value of that word */

/* _TBODY expects the address in r3 */
mov r3, r0
bl _tbody
mov r0, r3 /* Move the address of the codeword into r0 */

/* Now r3 points at the codeword */
and r2, r9, #F_IMMED /* r2 holds the and result */
cmp r2, #0
  bgt _interpret_execute /* Jump straight to executing */
b _interpret_compile_check

_interpret_not_in_dict:
/* Not in the dictionary, so assume it's a literal number. */
/* At this point, the length is in r8 and the address in r9. */
ldr r7, =var_THREAD
add r7, r7, #THREAD_interpret_is_lit
mov r6, #1
str r6, [r7]

/* _NUMBER expects the length in r2 and the address in r3. */
/* It returns the number in r0 and the number unparsed in r2. */
mov r2, r8
mov r3, r9
bl _number

cmp r2, #0
  bgt _interpret_illegal_number

mov r6, r0
ldr r0, =LIT /* Set the word to LIT */

_interpret_compile_check:
/* Are we compiling or executing? */

ldr r4, =var_THREAD
add r4, r4, #THREAD_STATE
ldr r4, [r4]
cmp r4, #0
  beq _interpret_execute /* Executing, so jump there */

/* Compiling. Append the word to the current dictionary definition. */
bl _comma /* The word lives in r0, which is what _comma wants. */

ldr r9, =var_THREAD
add r9, r9, #THREAD_interpret_is_lit
ldr r9, [r9]
cmp r9, #0
  beq _interpret_end /* Not a literal, so done. */

/* Literal number in play, so push it too. */
mov r0, r6 /* Move the literal from r1 to r0 */
bl _comma  /* And push it too. */
b _interpret_end

_interpret_execute:
/* Executing, run the word. */

ldr r9, =var_THREAD
add r9, r9, #THREAD_interpret_is_lit
ldr r9, [r9]
cmp r9, #0
  bgt _interpret_push_literal

/*
Not a literal, execute it now.
This never returns, but the codeword will eventually call NEXT,
which will reenter the loop in QUIT
*/

ldr pc, [r0]


_interpret_push_literal:
/* Executing a literal means pushing it onto the stack. */
push {r6}
b _interpret_end

_interpret_illegal_number:
/* We couldn't find the word in the dictionary, and it isn't */
/* a number either. Show an error message. */

push {r8, r9}
ldr r9, =errmsg
ldr r8, =errmsglen
ldr r8, [r8]
bl _type

pop {r8, r9}
bl _type

mov r0, #0x0a /*  newline */
bl _emit

/* Now output the input line */
ldr r9, =input_source
ldr r9, [r9]
add r8, r9, #SRC_TOP
ldr r8, [r8]
add r9, r9, #SRC_START
sub r8, r8, r9  /* Length goes in r8, address in r9 */
bl _type

mov r0, #10
bl _emit

ldr r9, =input_source
ldr r9, [r9]
add r8, r9, #SRC_POS
ldr r8, [r8]    /* r8 is the position in the input buffer. */
/* Output r8-many spaces, then a caret ^ (ASCII 94) */

_interpret_illegal_number_loop:
cmp r8, #0
  beq _interpret_illegal_number_end
mov r0, #32
bl _emit
sub r8, r8, #1
b _interpret_illegal_number_loop

_interpret_illegal_number_end:
mov r0, #94  /* ^ */
bl _emit
mov r0, #10
bl _emit

_interpret_end:
NEXT


/* Called as part of interpret, handles calling REFILL and popping empty input sources. */
/* Clobbers lots of things, including lr. */
_interpret_refill_needed:
push {lr}
/* First calls refill. */
_interpret_refill_needed_loop:
bl _refill
/* r0 now holds the flag value: 0 for failure, -1 for success. */
cmp r0, #0
  popne {pc} /* If it's nonzero, it succeeded, so return. */

/* If we get here, the refill failed. Therefore we pop the input source and try again. */
_interpret_refill_needed_pop:
ldr r0, =input_source /* r0 holds the cell for the input source */
ldr r1, [r0]          /* r1 holds the base pointer for the current input source. */
sub r1, r1, #INPUT_SOURCE_SIZE
str r1, [r0]          /* And write that into the variable. */

ldr r0, =input_source_top
ldr r0, [r0]          /* Load the input source top value. This is the pointer to the keyboard; if we've gone below that, quit. */
cmp r1, r0
  blt _interpret_refill_needed_fail
/* We still have a valid source. Return and let INTERPRET try to read what's already in it. */
/* One special-case here, though. If the freshly-popped input source is an EVALUATE string, POPRSP and continue executing. */
add r1, r1, #INPUT_SOURCE_SIZE
add r1, r1, #SRC_TYPE
ldr r1, [r1] /* Load the type of the OLD input source. */
cmp r1, #1
  popne {pc}  /* If it wasn't 1 (EVALUATE), return. */

/* It was EVALUATE, so do the special case magic. */
POPRSP
NEXT

/* At this point, we have run out of valid input sources. */
/* Even the keyboard has failed, which suggests a Ctrl-D or similar end-of-input. Therefore we exit with success. */
_interpret_refill_needed_fail:
mov r7, #__NR_exit
mov r0, #0
swi #0
/* Never returns */



name_REFILL:
.word name_INTERPRET
.byte 6
.ascii "REFILL"
.align
REFILL:
.word code_REFILL
code_REFILL:
bl _refill
push {r0}
NEXT

/* REFILL re-loads the parse buffer from the input source and returns a flag. */
/* If the input source is the keyboard, read a line. */
/* If the input source is an EVALUATEd string, do nothing and return false. */
/* If the input source is a file, read a line from it. */
/* If the input source is a block, bump BLK to the next block and load it. */
/* If any given input source is empty, we will fail to load from it. INTERPRET calls REFILL, and when */
/* REFILL fails, it pops the input source and refills from there. */

/* Returns a flag in r0. Clobbers a bunch of things. */
_refill:
push {lr}
ldr r0, =input_source
ldr r0, [r0]   /* r0 holds the input source base pointer. */
/* Now load the type into r1 and dispatch on it. */
add r1, r0, #SRC_TYPE
ldr r1, [r1]

/* TODO: Handle blocks. */
cmp r1, #0
  beq _refill_keyboard
cmp r1, #2
  beq _refill_file

/* Refilling from EVALUATE: do nothing and return false. */
mov r0, #0
pop {pc}

/* Refilling from the keyboard: Call getchar repeatedly, storing into the input source's buffer. */
_refill_keyboard:
/* Use to-be-saved registers here so that the getchar calls will preserve them. */
ldr r7, =input_source
ldr r7, [r7]
add r8, r7, #SRC_START  /* r8 holds the current pointer where new characters should go. */
add r9, r8, #PARSE_BUFFER_LEN /* r9 holds the top, don't write past here. */

_refill_keyboard_loop:
bl getchar    /* r0 now holds the next character. */
mvn r1, #0
cmp r0, r1
  beq _refill_keyboard_done /* EOF found. */

cmp r0, #13
  beq _refill_keyboard_done /* CR found. */
cmp r0, #10
  beq _refill_keyboard_done /* NL found. */

/* Otherwise, write this value into the buffer and loop. */
strb r0, [r8]
add r8, r8, #1

cmp r8, r9
  blt _refill_keyboard_loop

/* When we come down here, either falling through or jumping, we have r8 pointing after the last character. */
_refill_keyboard_done:
add r0, r7, #SRC_TOP
str r8, [r0]        /* Store the r8 running pointer into the top field. */

mov r9, #0
add r0, r7, #SRC_POS
str r9, [r0]        /* And 0 into the offset. */

mvn r0, #0 /* Load -1, a true flag, and return. */
pop {pc}


_refill_file:
ldr r8, =input_source
ldr r8, [r8]   /* r8 is the input source pointer. */
add r9, r8, #SRC_DATA
ldr r9, [r9]  /* Load the fd into r9. */
add r6, r8, #SRC_START /* And the starting pointer into r6. */

_refill_file_loop:
mov r7, #__NR_read
mov r0, r9  /* The fd. */
mov r1, r6  /* The pointer. */
mov r2, #1  /* Length 1 */
swi #0

cmp r0, #0  /* 0 indicates an error or EOF, so return an error. */
  beq _refill_file_empty

ldrb r1, [r6] /* Load the character we just read. */
cmp r1, #10  /* NL */
  beq _refill_file_done
cmp r1, #13  /* CR */
  beq _refill_file_done

/* If we got down here, this is a regular character, and we should loop. */
add r6, r6, #1 /* Bump the pointer */
b _refill_file_loop

_refill_file_done:
add r7, r8, #SRC_TOP
str r6, [r7] /* Store the top pointer. */
add r7, r8, #SRC_POS
mov r6, #0
str r6, [r7] /* And 0 to the offset. */

mvn r0, #0 /* Return true. */
pop {pc}

_refill_file_empty:
mov r0, #0  /* Return false. */
pop {pc}



name_EVALUATE:
.word name_REFILL
.byte 8
.ascii "EVALUATE"
.align
EVALUATE:
.word code_EVALUATE
code_EVALUATE:
pop {r0, r1}  /* r0 = length, r1 = address */
/* Create a new input source above the current one, and copy in the evaluated string. */
/* The copying is unfortunate but necessary, since it keeps the interface common for the input sources. */

ldr r3, =input_source
ldr r4, [r3]
add r4, r4, #INPUT_SOURCE_SIZE
str r4, [r3]   /* Write the new input source's location into the variable. */

add r5, r4, #SRC_TYPE
mov r2, #1  /* Load the type for an EVALUATE string. */
str r2, [r5] /* And write the type into the field. */

add r5, r4, #SRC_START
cmp r0, #0   /* Short-circuit and skip over the loop when the length is 0. */
  beq _evaluate_done
_evaluate_loop:
ldrb r2, [r1], #1  /* Load the byte, post-index by 1. */
strb r2, [r5], #1  /* Store the byte, post-index by 1. */
subs r0, r0, #1
  bne _evaluate_loop

/* When we get down here, r5 is the top pointer, and the copy is complete. */
_evaluate_done:
add r1, r4, #SRC_TOP
str r5, [r1]          /* Store r5, the top pointer. */
add r1, r4, #SRC_POS
mov r5, #0
str r5, [r1]

/* Now here's the execution flow for EVALUATE. */
/* First we PUSHRSP, to put the code after the EVALUATE call into the return stack. */
/* Then we load the address of the middle of QUIT into r11 and NEXT. */
/* This will begin an INTERPRET loop, targeting the EVALUATE string as the input buffer. */
/* When we try to _refill the EVALUATE string's buffer, it fails. */
/* Special-case code in _interpret_refill_needed will detect that we just popped an EVALUATEd string. */
/* And it will POPRSP and carry on executing the previous code, rather than returning to INTERPRET. */
PUSHRSP
ldr r11, =_quit_inner
NEXT


name_IN:
.word name_EVALUATE
.byte 3
.ascii ">IN"
.align
IN:
.word code_IN
code_IN:
ldr r0, =input_source
ldr r0, [r0]
add r0, r0, #SRC_POS
push {r0}
NEXT


name_SOURCE:
.word name_IN
.byte 6
.ascii "SOURCE"
.align
SOURCE:
.word code_SOURCE
code_SOURCE:
/* Returns the address of, and number of characters in, the input buffer. */
ldr r0, =input_source
ldr r0, [r0]
add r2, r0, #SRC_START
add r1, r0, #SRC_TOP
ldr r1, [r1]   /* Actual top pointer */
sub r1, r1, r2 /* Now the length */
push {r2}
push {r1}
NEXT


/* Odds and ends */

name_CHAR:
.word name_SOURCE
.byte 4
.ascii "CHAR"
.align
CHAR:
.word code_CHAR
code_CHAR:
mov r0, #32 /* Delimit by spaces. */
bl _parse_word /* Address in r1, length in r0 */
ldrb r1, [r1] /* Get the letter */
push {r1} /* push it */
NEXT


name_DEBUG:
.word name_CHAR
.byte 5
.ascii "DEBUG"
.align
DEBUG:
.word code_DEBUG
code_DEBUG:
_debug:
mov r0, r1
NEXT

name_LOOP_SP:
.word name_DEBUG
.byte 9
.ascii "(LOOP-SP)"
.align
LOOP_SP:
.word code_LOOP_SP
code_LOOP_SP:
ldr r0, =var_THREAD
add r1, r0, #THREAD_LOOP_SP
push {r1}
NEXT


name_CURRENT_THREAD:
.word name_LOOP_SP
.byte 16
.ascii "(CURRENT-THREAD)"
.align
CURRENT_THREAD:
.word code_CURRENT_THREAD
code_CURRENT_THREAD:
ldr r0, =var_THREAD
push {r0}
NEXT


name_ALLOCATE:
.word name_CURRENT_THREAD
.byte 8
.ascii "ALLOCATE"
.align
ALLOCATE:
.word code_ALLOCATE
code_ALLOCATE:
pop {r0}
bl malloc
/* We return an error if malloc returned NULL. */
mov r1, #0
cmp r0, r1
  moveq r1, #37

push {r0} /* address under */
push {r1} /* ior on top. */
NEXT

name_FREE:
.word name_ALLOCATE
.byte 4
.ascii "FREE"
.align
FREE:
.word code_FREE
code_FREE:
pop {r0}
bl free
mov r0, #0 /* free(3) never fails, at least not detectably */
push {r0}
NEXT


name_EXECUTE:
.word name_DEBUG
.byte 7
.ascii "EXECUTE"
.align
EXECUTE:
.word code_EXECUTE
code_EXECUTE:
pop {r3} /* Get the execution token (a pointer) off the stack */
bl _tbody /* Convert from the xt (pointer to the dictionary block) to the codeword. */
ldr r2, [r3] /* Load the value stored there */
mov r0, r3   /* DOCOL expects the codeword address in r0. */
bx r2 /* And jump there */
NEXT

/* EXECUTE needs to be the last word, or the initial value of var_LATEST needs updating. */


/* Loads every filename from argv into the input sources, but in reverse order. */
_load_files:
push {lr}
ldr r8, =argc
ldr r8, [r8]
ldr r9, =argv
ldr r9, [r9]

_load_files_loop:
cmp r8, #0
  beq _load_files_done

sub r2, r8, #1  /* Knock off one so it's now the index of the highest arg. */
mov r1, #4      /* Can't multiply with a literal, I guess. */
mul r0, r2, r1  /* Convert to an offset from argv to the next file */
add r0, r0, r9  /* Address of the pointer to the next file name. */
ldr r0, [r0]    /* Load the address of the file name itself. */

mov r1, #O_RDONLY
mov r7, #__NR_open
swi #0  /* Now r0 contains the fileid */
mvn r2, #0
cmp r2, r0
  beq _load_files_error

/* Put the fileid into a new entry in the input source list. */
ldr r2, =input_source
ldr r1, [r2]
add r1, r1, #INPUT_SOURCE_SIZE
str r1, [r2]

add r3, r1, #SRC_TYPE
mov r4, #2  /* 2 is the type for file */
str r4, [r3] /* store the type */

add r4, r1, #SRC_START
add r3, r1, #SRC_TOP
str r4, [r3] /* Store the start address in the top field (therefore buffer is empty). */
mov r4, #0
add r3, r1, #SRC_POS
str r4, [r3] /* And store 0 into the parse offset field. */

add r3, r1, #SRC_DATA
str r0, [r3]   /* Finally, store the fileid into the data field. */

/* Now I'm done loading the file entry, so now it's time to loop. */
sub r8, r8, #1  /* Remove one from argc. */
b _load_files_loop

_load_files_done:
pop {pc}

_load_files_error:
ldr r9, =_load_files_error_message
ldr r8, =_load_files_error_message_len
ldr r8, [r8]
bl _type
mov r0, #0x0a /*  newline */
bl _emit

/* And exit with code 1. */
mov r0, #1
mov r7, #__NR_exit
swi #0


.globl main
main:
/* Check for the command-line args, and set aside their values */
ldr r3, =argc
sub r0, r0, #1 /* jump over the command name */
str r0, [r3]
ldr r3, =argv
add r1, r1, #4 /* jump over the command name */
str r1, [r3]

/* Call malloc to request space for HERE. Currently 1M */
mov r0, #1
lsl r0, r0, #20
mov r7, r0
bl malloc
/* r0 now contains the pointer to the memory */
ldr r1, =var_THREAD  /* Store it in the thread pointer. */
str r0, [r1]
add r1, r0, #THREAD_HERE  /* Offset the thread pointer by HERE's location */
add r2, r0, #THREAD_SIZE  /* The actual HERE value is after the thread struct. */
str r2, [r1]
add r1, r0, #THREAD_TOP
add r2, r0, r7 /* Add the pointer and length to get a top pointer. */
str r2, [r1]   /* And store that in HERE_TOP. */

/* Set up the stacks */
mov r10, r2    /* Return stack is at the top of the thread memory. */
sub sp, r10, #RETURN_STACK_SIZE  /* The data stack is below it, by 1K words. */

/* Set up the other variables. */
ldr r1, =var_THREAD
add r2, r1, #THREAD_LATEST
ldr r3, =name_EXECUTE
str r3, [r2]

mov r3, #0
add r2, r1, #THREAD_STATE
str r3, [r2]

mov r3, #10
add r2, r1, #THREAD_BASE
str r3, [r2]

add r3, r1, #THREAD_LOOP_STACK
add r2, r1, #THREAD_LOOP_SP
str r3, [r2]

mov r3, #0
add r2, r1, #THREAD_interpret_is_lit
str r3, [r2]

/* Load the input files, if applicable */
bl _load_files

/* And launch the interpreter */
ldr r0, =QUIT
ldr r11, =cold_start /* Initialize the interpreter */
NEXT

cold_start:
.word QUIT



.data

errmsg:
.ascii "Interpreter error: Unknown word or bad number: "
.align
errmsglen:
.word 47

_load_files_error_message:
.ascii "Could not open input file."
.align
_load_files_error_message_len:
.word 26

debug_caret:
.ascii "> "
.align

_key_buffer:
.word 0

/* On input buffers:
- There are 16 input sources here. Each is 512 bytes long, and has the following form:
  - 4 bytes: input source type (0 = keyboard, 1 = EVALUATE string, 2 = file, 3 = block)
  - 4 bytes: parse buffer offset (offset into the data area)
  - 4 bytes: parse buffer top (points to after the current parse field)
  - 4 bytes: data value (keyboard: empty, EVALUATE: pointer to string, file: fileid, block: blockid)
  - 496 bytes: input buffer itself. Not used for blocks or EVALUATE strings, but still present.
- input_source_top points at the first entry (the keyboard)
- input_source points at the current entry
*/
input_spec_space:
.space 8192, 0  /* 8K = 16 * 512-byte entries, allowing 16 layers of nesting. */
input_source_top:
.word input_spec_space
input_source:
.word input_spec_space

.equ SRC_TYPE, 0
.equ SRC_POS, 4
.equ SRC_TOP, 8
.equ SRC_DATA, 12
.equ SRC_START, 16

.equ PARSE_BUFFER_LEN, 496
.equ INPUT_SOURCE_SIZE, 512

/* These need to stay in sync with thread.fs */
.equ THREAD_LINK, 0
.equ THREAD_HERE, 4
.equ THREAD_LATEST, 8
.equ THREAD_STATE, 12
.equ THREAD_BASE, 16
.equ THREAD_LOOP_SP, 20
.equ THREAD_LOOP_STACK, 24 /* Current 4 cells of loop stack. */
.equ THREAD_interpret_is_lit, 40
.equ THREAD_TOP, 44

.equ THREAD_SIZE, 64

.equ RETURN_STACK_SIZE, 4096

current_fd:
.word stdin

argc:
.word 0
argv:
.word 0

/* XXX Thread-safety: This assumes only one thread is running at once, */
/* ie. single core. */
var_THREAD:
.word 0

.end

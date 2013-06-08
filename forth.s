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
.byte 2
.ascii "<<"
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
.byte 2
.ascii ">>"
.align
SHR:
.word code_SHR
code_SHR:
pop {r0,r1}
lsr r0, r1, r0
push {r0}
NEXT



/* Following Jonesforth and breaking the ANS standard by returning 0 and 1, not 0 and 0xffffffff */
name_EQU:
.word name_SHR
.byte 1
.ascii "="
.align
EQU:
.word code_EQU
code_EQU:
pop {r0,r1}
mov r2, #0
cmp r0, r1
addeq r2, r2, #1
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
addne r2, r2, #1
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
addgt r2, r2, #1
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
addhi r2, r2, #1
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
addlt r2, r2, #1
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
addhi r2, r2, #1
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
addge r2, r2, #1
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
addle r2, r2, #1
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
addeq r2, r2, #1
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
addne r2, r2, #1
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
addmi r2, r2, #1
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
addgt r2, r2, #1
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
addpl r2, r2, #1
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
addge r2, r2, #1
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




name_STATE:
.word name_CMOVE
.byte 5
.ascii "STATE"
.align
STATE:
.word code_STATE
code_STATE:
ldr r0, =var_STATE
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
ldr r0, =var_LATEST
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
ldr r0, =var_HERE
push {r0}
NEXT



name_S0:
.word name_HERE
.byte 2
.ascii "S0"
.align
S0:
.word code_S0
code_S0:
ldr r0, =var_S0
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
ldr r0, =var_BASE
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
mov r0, #1
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
ldr r0, =return_stack_top
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
.set __NR_read, 3
.set __NR_write, 4
.set __NR_brk, 45
.set __NR_exit, 93

.set stdin, 1
.set stdout, 2

/* Input and output */

name_KEY:
.word name_DSPSTORE
.byte 3
.ascii "KEY"
.align
KEY:
.word code_KEY
code_KEY:
bl _key
push {r0}
NEXT
/* TODO - Local echo? */


/* No input, returns a character in r0 */
/* Clobbers r0-r2 + r7 + lr */
_key:
push {lr}
_key_inner:
ldr r0, =current_fd
ldr r0, [r0]
ldr r1, =key_buffer
mov r2, #1
bl read

/* Check for a read of zero */
cmp r0, #0
  beq _key_eof

ldr r0, =key_buffer
ldrb r0, [r0]
pop {pc}

_key_eof:
/* Load a new file, or stdin */
bl _load_files
b _key_inner


name_EMIT:
.word name_KEY
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
ldr r1, =key_buffer
strb r0, [r1]
mov r0, #stdout
mov r2, #1
mov r7, #__NR_write
swi $0
pop {pc}


name_WORD:
.word name_EMIT
.byte 4
.ascii "WORD"
.align
WORD:
.word code_WORD
code_WORD:
bl _word /* returns with r0 = address, r1 = length */
push {r0}
push {r1}
NEXT


/* Returns the buffer address in r0 and the length in r1 */
/* Clobbers r0-r2, r6, r7, and lr. */
_word:
push {lr}
_word_top:
bl _key
cmp r0, #0x5c /* backslash, the start of a line comment */
beq _word_comment
cmp r0, #0x20 /* space, keep searching for real letters */
beq _word_top
cmp r0, #0x0d /* carriage return, keep searching */
beq _word_top
cmp r0, #0x0a /* newline, keep searching */
beq _word_top
cmp r0, #0x09 /* tap, keep searching */
beq _word_top

/* If we got down here, found a real letter. */
ldr r6, =_word_buffer
_word_main:
strb r0, [r6]
add r6, r6, #1
bl _key

cmp r0, #0x10 /* backspace */
  beq _word_backspace
cmp r0, #0x20 /* space, so jump down */
  beq _word_complete
cmp r0, #0x0d /* carriage return, jump down */
  beq _word_complete
cmp r0, #0x0a /* newline, jump down */
  beq _word_complete

b _word_main /* loop */

_word_complete:
ldr r0, =_word_buffer
sub r1, r6, r0 /* r1 now holds the length. */
pop {pc}


/*
Handle backspace
Bump the pointer back by two so it'll overwrite the previous good letter and then be set to overwrite the bad one.
Kind of a hack, but it works.
*/
_word_backspace:
sub r6, r6, #2
ldr r0, [r6]
b _word_main

/* And the code to skip past a comment: */
_word_comment:
bl _key
cmp r0, #0x0a /* newline, end of comment */
  beq _word_top
b _word_comment



name_NUMBER:
.word name_WORD
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

ldr r4, =var_BASE
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
sub r1, r1, #8 /* 65-57 = 8, turns 'A' into '9' + 1 */

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
pop {r2,r3} /* length on top of the stack -> r2, address beneath -> r3 */
bl _find
push {r0} /* address of dictionary entry, or 0. */
NEXT


/*
Expects the length in r2, address in r3.
Returns the address of the dictionary entry in r0.
Clobbers r0-r3
*/
_find:

ldr r4, =var_LATEST
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



name_TCFA:
.word name_FIND
.byte 4
.ascii ">CFA"
.align
TCFA:
.word code_TCFA
code_TCFA:
pop {r3} /* dictionary address */
bl _tcfa
push {r3}
NEXT


/*
Expects a dictionary block address in r3.
Returns address of codeword in r3.
Clobbers r0, r3
*/
_tcfa:
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


name_TDFA:
.word name_TCFA
.byte 4
.ascii ">DFA"
.align
TDFA:
.word code_TDFA
code_TDFA:
pop {r3}
bl _tcfa
add r3, r3, #4 /* Jump over to the code. */
push {r3}
NEXT



/* Compilation and defining */

name_CREATE:
.word name_TDFA
.byte 6
.ascii "CREATE"
.align
CREATE:
.word code_CREATE
code_CREATE:
pop {r1} /* length from the top, address of the name underneath */
pop {r2}

/* Store the link pointer. */
ldr r3, =var_HERE
ldr r3, [r3]
ldr r0, =var_LATEST
ldr r0, [r0]

str r0, [r3] /* Write LATEST into the new link pointer at HERE */
ldr r4, =var_LATEST
str r3, [r4] /* And write the new HERE into LATEST */
add r3, r3, #4 /* Jump over the link pointer. */

/* Length byte and the word itself need storing. */
strb r1, [r3] /* store the length byte */
add r3, r3, #1 /* Move to the start of the string. */

_create_name_loop:
ldrb r5, [r2] /* Load the next letter of the name into r5 */
strb r5, [r3] /* And write it into the new word block */
add r2, r2, #1
add r3, r3, #1

sub r1, r1, #1
cmp r1, #0
  bgt _create_name_loop

/*
Move HERE to point to after the block
New value of HERE is in r3, but not aligned.
Align it:
*/
add r3, r3, #3
mvn r5, #3
and r3, r3, r5

/* And then store it */
ldr r1, =var_HERE
str r3, [r1]
NEXT


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
ldr r3, =var_HERE
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
ldr r0, =var_STATE
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
ldr r0, =var_STATE
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
/* Get the name of the new word. */
.word WORD
/* Create the header */
.word CREATE
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



name_IMMEDIATE:
.word name_SEMICOLON
.byte 0x89 /* F_IMMED | 9 */
.ascii "IMMEDIATE"
.align
IMMEDIATE:
.word code_IMMEDIATE
code_IMMEDIATE:
ldr r3, =var_LATEST
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


name_HIDE:
.word name_HIDDEN
.byte 4
.ascii "HIDE"
.align
HIDE:
.word DOCOL
/* Get the word after HIDE */
.word WORD
/* Look it up in the dictionary */
.word FIND
/* Set the flag */
.word HIDDEN
.word EXIT


name_TICK:
.word name_HIDE
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
with WORD, FIND and >CFA so that it'll run in immediate mode too.
*/




/* Branching primitives */

name_BRANCH:
.word name_TICK
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



name_TELL:
.word name_LITSTRING
.byte 4
.ascii "TELL"
.align
TELL:
.word code_TELL
code_TELL:
pop {r8,r9} /* Length = r8, address = r9 */

cmp r8, #0
  beq _tell_done

bl _tell
_tell_done:
NEXT

_tell:
push {lr}
_tell_inner:
ldrb r0, [r9] /* Get the next character */
bl _emit /* Clobbers r0-r2, r7 */
add r9, r9, #1
subs r8, r8, #1
  bgt _tell_inner
pop {pc}


name_QUIT:
.word name_TELL
.byte 4
.ascii "QUIT"
.align
QUIT:
.word DOCOL
.word _R0
.word RSPSTORE
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

bl _word /* r0 has the address, r1 the length */

/* Not a literal number (at least not yet) */
ldr r2, =interpret_is_lit
mov r3, #0
str r3, [r2]

/* Adjust for the differences between my FIND and WORD. TODO: Clean this up. */
mov r2, r1
mov r3, r0
mov r9, r1 /* Save r1 */
bl _find
mov r1, r9 /* And restore */

cmp r0, #0
  beq _interpret_not_in_dict

/* In the dictionary. Check if it's an IMMEDIATE codeword */
add r2, r0, #4 /* r2 is the address of the dictionary header's length byte */
ldrb r9, [r2] /* Set aside the actual value of that word */

/* _TCFA expects the address in r3 */
mov r3, r0
bl _tcfa
mov r0, r3 /* Move the address of the codeword into r0 */

/* Now r3 points at the codeword */
and r2, r9, #F_IMMED /* r2 holds the and result */
cmp r2, #0
  bgt _interpret_execute /* Jump straight to executing */
b _interpret_compile_check

_interpret_not_in_dict:
/* Not in the dictionary, so assume it's a literal number. */
ldr r9, =interpret_is_lit
mov r8, #1
str r8, [r9]

/* _NUMBER expects the length in r2 and the address in r3. */
/* It returns the number in r0 and the number unparsed in r2. */
mov r2, r1 /* The length was in r1, now in r2 */
bl _number

cmp r2, #0
  bgt _interpret_illegal_number

mov r6, r0
ldr r0, =LIT /* Set the word to LIT */

_interpret_compile_check:
/* Are we compiling or executing? */

ldr r4, =var_STATE
ldr r4, [r4]
cmp r4, #0
  beq _interpret_execute /* Executing, so jump there */

/* Compiling. Append the word to the current dictionary definition. */
bl _comma /* The word lives in r0, which is what _comma wants. */

ldr r9, =interpret_is_lit
ldr r9, [r9]
cmp r9, #0
  beq _interpret_end /* Not a literal, so done. */

/* Literal number in play, so push it too. */
mov r0, r6 /* Move the literal from r1 to r0 */
bl _comma  /* And push it too. */
b _interpret_end

_interpret_execute:
/* Executing, run the word. */

ldr r9, =interpret_is_lit
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
/* TODO - Improve the amount of detail in the message. */

ldr r9, =errmsg
ldr r8, =errmsglen
ldr r8, [r8]
bl _tell
mov r0, #0x0a /*  newline */
bl _emit

_interpret_end:
NEXT


/* Odds and ends */

name_CHAR:
.word name_INTERPRET
.byte 4
.ascii "CHAR"
.align
CHAR:
.word code_CHAR
code_CHAR:
bl _word /* Address in r0 */
ldrb r0, [r0] /* Get the letter */
push {r0} /* push it */
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

name_EXECUTE:
.word name_DEBUG
.byte 7
.ascii "EXECUTE"
.align
EXECUTE:
.word code_EXECUTE
code_EXECUTE:
pop {r0} /* Get the execution token (a pointer) off the stack */
ldr r0, [r0] /* Load the value stored there */
bx r0 /* And jump there */
NEXT

/* EXECUTE needs to be the last word, or the initial value of var_LATEST needs updating. */



/* Reads argc and argv, and queues up the next file if there are more */
_load_files:
push {r0,r1,r7,r8,r9,lr}
ldr r8, =argc
ldr r7, [r8]
cmp r7, #0
  bgt _load_files_load

/* Otherwise, we're just setting the file to stdin */
ldr r0, =current_fd
mov r1, #stdin
str r1, [r0]
b _load_files_done

_load_files_load:
ldr r9, =argv
ldr r0, [r9] /* r0 holds the argv pointer */
ldr r0, [r0] /* r0 holds the char* */
mov r1, #0   /* 0 is O_RDONLY */
bl open

ldr r1, =current_fd
str r0, [r1]

ldr r0, [r9]
add r0, r0, #4 /* advance the argv pointer */
str r0, [r9]

sub r7, r7, #1
str r7, [r8] /* write back argc */

_load_files_done:
pop {r0,r1,r7,r8,r9,pc}



.globl main
main:
/* Check for the command-line args, and set aside their values */
ldr r3, =argc
sub r0, r0, #1 /* jump over the command name */
str r0, [r3]
ldr r3, =argv
add r1, r1, #4 /* jump over the command name */
str r1, [r3]

/* Call malloc to request space for HERE. Currently 256K */
mov r0, #1
lsl r0, r0, #18
bl malloc
/* r0 now contains the pointer to the memory */
ldr r1, =var_HERE
str r0, [r1]

/* Set up the stacks */
ldr r0, =return_stack_top
str sp, [r0]
mov r10, sp
sub sp, sp, #4096 /* Leave 1K words for the return stack */

ldr r0, =var_S0
str sp, [r0]

/* Load the first file, if applicable */
bl _load_files

/* And launch the interpreter */
ldr r0, =QUIT
ldr r11, =cold_start /* Initialize the interpreter */
NEXT

cold_start:
.word QUIT


.data

errmsg:
.ascii "Interpreter error: Unknown word or bad number."
errmsglen:
.word 46

var_STATE:
.word 0

var_LATEST:
.word name_EXECUTE

var_S0:
.word 0

var_BASE:
.word 10

key_buffer:
.word 0

_word_buffer:
.space 32


interpret_is_lit:
.word 0


return_stack_top:
.word 0

current_fd:
.word stdin

argc:
.word 0
argv:
.word 0

/* var_HERE must be the last entry in the data segment */
var_HERE:
.word 0

.end

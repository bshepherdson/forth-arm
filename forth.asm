; DCPU-16 assembly code for a Forth system.
; The start point of execution on DCPU-16 is unspecified and assumed to begin at 0
; Therefore I assemble an instruction at that position that jumps to the actual Forth engine startup code.
;
; REGISTER SPEC
; =============
; SP is used for the data stack. The top of the Forth return stack is kept in J.
; It's modified a lot, so it's worth dedicating the register.
; The next Forth address to run is stored in I.
; NB: This means that using the unfortunately named DCPU opcodes STI and STD is verboten.
;
;
; BINARY LAYOUT
; =============
; First comes an instruction to jump to the start point farther along in the code.
; Then we have some global variables with various uses, like the dictionary head pointer.
; Then comes the builtin word definitions.
; Then space for the compiled definitions.
; Then the main routine of the user program.
;
; Then we have three non-fixed-size areas: user-allocatable data space, return stack, and real stack.
; The easiest one to limit in size is the return stack, to say 1KB. Maybe put that starting at 0xffff and the data stack below it?
; And then the allocatable data space goes after the code. If the data stack and allocation area grow over each other, oh well.


; Adjustable Parameters

.set return_stack_top, 0xffff
.set data_stack_top, 0xfbff
.set mmr, 0xe000
.set load_stack_top, 0xe800

.set disk_interrupt_msg, 8


; NEXT pseudomacro
; NEXT
; SET A, [I]
; ADD I, 1
; SET PC, [A]
; .sym NEXT 0x3801 0x8462 0x21c1

; PUSHRSP pseudomacro. Needs to store the current I (the address of the next Forth instruction to run) to the location pointed to by J, and decrement J.
; This is pushing the execution state onto the return stack.
; PUSHRSP
; SET [J], I
; SUB J, 1
; END PUSHRSP
; .sym PUSHRSP 0x18f1 0x8473

; POPRSP pseudomacro. Increment J, and restore the value at [J] to I.
; POPRSP
; ADD J, 1
; SET I, [J]
; END POPRSP
; .sym POPRSP 0x8472 0x3c61

.set F_IMMED, 0x80
.set F_HIDDEN, 0x20
.set F_LENMASK, 0x1f
.set F_HIDDEN_AND_LENMASK, 0x3f


; ENTRY POINT

SET PC, startup ; jump to the Forth startup code.


; DOCOL. The interpreter code for running functions written in Forth. Expects A to contain the codeword address!
:DOCOL
; PUSHRSP
SET [J], I
SUB J, 1
; END PUSHRSP
ADD A, 1 ; now A points at the first data word.
SET I, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


; WORD DEFINITIONS
; The link pointer is the first word after the :name_FOO label.
; It points at the name_BAR of the word before it, or 0 for the last in the chain.
; This needs maintaing as new words are implemented.

:name_DROP
DAT 0
DAT 4
DAT "DROP"
:DROP
DAT code_DROP
:code_DROP
ADD SP, 1
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]

:name_SWAP
DAT name_DROP
DAT 4
DAT "SWAP"
:SWAP
DAT code_SWAP
:code_SWAP
SET A, POP
SET B, POP
SET PUSH, A
SET PUSH, B
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_DUP
DAT name_SWAP
DAT 3
DAT "DUP"
:DUP
DAT code_DUP
:code_DUP
SET A, PEEK ; TODO: SET PUSH, PEEK ?
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_OVER
DAT name_DUP
DAT 4
DAT "OVER"
:OVER
DAT code_OVER
:code_OVER
SET A, SP
ADD A, 1
SET PUSH, [A]
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_ROT
DAT name_OVER
DAT 3
DAT "ROT"
:ROT
DAT code_ROT
:code_ROT
SET A, POP ; ( c b a )
SET B, POP
SET C, POP
SET PUSH, B
SET PUSH, A
SET PUSH, C ; ( b a c ) grab
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_NEGROT
DAT name_ROT
DAT 4
DAT "-ROT"
:NEGROT
DAT code_NEGROT
:code_NEGROT
SET A, POP ; ( c b a )
SET B, POP
SET C, POP
SET PUSH, A
SET PUSH, C
SET PUSH, B ; ( a c b ) bury
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_TWODROP
DAT name_NEGROT
DAT 5
DAT "2DROP"
:TWODROP
DAT code_TWODROP
:code_TWODROP
ADD SP, 2
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_TWODUP
DAT name_TWODROP
DAT 4
DAT "2DUP"
:TWODUP
DAT code_TWODUP
:code_TWODUP
SET A, POP
SET B, POP
SET PUSH, B
SET PUSH, A
SET PUSH, B
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_TWOSWAP
DAT name_TWODUP
DAT 5
DAT "2SWAP"
:TWOSWAP
DAT code_TWOSWAP
:code_TWOSWAP
SET A, POP
SET B, POP
SET C, POP
SET X, POP
SET PUSH, B
SET PUSH, A
SET PUSH, X
SET PUSH, C
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_TWOOVER
DAT name_TWOSWAP
DAT 5
DAT "2OVER"
:TWOOVER
DAT code_TWOOVER
:code_TWOOVER
SET A, POP
SET B, POP
SET C, POP
SET X, POP

SET PUSH, X
SET PUSH, C
SET PUSH, B
SET PUSH, A
SET PUSH, X
SET PUSH, C  ; This is inefficient but straightforward. Oh well. TODO
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_QDUP
DAT name_TWOOVER
DAT 4
DAT "?DUP"
:QDUP
DAT code_QDUP
:code_QDUP
SET A, PEEK
IFG A, 0
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_INCR
DAT name_QDUP
DAT 2
DAT "1+"
:INCR
DAT code_INCR
:code_INCR
SET A, POP ; TODO ADD PEEK, 1
ADD A, 1
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_DECR
DAT name_INCR
DAT 2
DAT "1-"
:DECR
DAT code_DECR
:code_DECR
SET A, POP ; TODO SUB PEEK, 1
SUB A, 1
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_ADD
DAT name_DECR
DAT 1
DAT "+"
:ADD
DAT code_ADD
:code_ADD
SET A, POP
SET B, POP
ADD A, B
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]

:name_ADDEX
DAT name_ADD
DAT 3
DAT "+EX"
:ADDEX
DAT code_ADDEX
:code_ADDEX
SET A, POP
SET B, POP
ADD A, B
SET PUSH, A
SET PUSH, EX
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_SUB
DAT name_ADDEX
DAT 1
DAT "-"
:SUB
DAT code_SUB
:code_SUB
SET A, POP
SET B, POP
SUB B, A
SET PUSH, B
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_MUL
DAT name_SUB
DAT 1
DAT "*"
:MUL
DAT code_MUL
:code_MUL
SET A, POP
SET B, POP
MLI A, B
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_MULU
DAT name_MUL
DAT 2
DAT "*U"
:MULU
DAT code_MULU
:code_MULU
SET A, POP
SET B, POP
MUL A, B
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


; There's no unified division instruction, so DIV and MOD are implemented as primitives
:name_DIV
DAT name_MULU
DAT 1
DAT "/"
:DIV
DAT code_DIV
:code_DIV
SET A, POP
SET B, POP
DVI B, A
SET PUSH, B
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]

:name_DIVU
DAT name_DIV
DAT 2
DAT "/U"
:DIVU
DAT code_DIVU
:code_DIVU
SET A, POP
SET B, POP
DIV B, A
SET PUSH, B
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_MOD
DAT name_DIVU
DAT 3
DAT "MOD"
:MOD
DAT code_MOD
:code_MOD
SET A, POP
SET B, POP
MDI B, A
SET PUSH, B
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_MODU
DAT name_MOD
DAT 4
DAT "MODU"
:MODU
DAT code_MODU
:code_MODU
SET A, POP
SET B, POP
MOD B, A
SET PUSH, B
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_SHL
DAT name_MODU
DAT 2
DAT "<<"
:SHL
DAT code_SHL
:code_SHL
SET A, POP
SET B, POP
SHL B, A
SET PUSH, B
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_SHR
DAT name_SHL
DAT 2
DAT ">>"
:SHR
DAT code_SHR
:code_SHR
SET A, POP
SET B, POP
SHR B, A
SET PUSH, B
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]



; Following jonesforth and breaking the ANS standard by returning 0 and 1, not 0 and 0xffff.

:name_EQU
DAT name_SHR
DAT 1
DAT "="
:EQU
DAT code_EQU
:code_EQU
SET A, POP
SET B, POP
SET C, 0
IFE A, B
SET C, 1 ; run only when they're equal
SET PUSH, C
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_NEQU
DAT name_EQU
DAT 2
DAT "<>"
:NEQU
DAT code_NEQU
:code_NEQU
SET A, POP
SET B, POP
SET C, 0
IFN A, B
SET C, 1 ; run only when not equal
SET PUSH, C
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


; Expects A to contain a number. Sets A to 1 if it's positive and to 0 otherwise.
:_IS_POSITIVE
IFG A, 0x7fff
SET PC, _RET_FALSE
SET PC, _RET_TRUE

; Expects A to contain a number. Sets A to 0 if it's positive and 1 otherwise.
:_IS_NEGATIVE
IFG A, 0x7fff
SET PC, _RET_TRUE
SET PC, _RET_FALSE

:_RET_TRUE
SET A, 1
SET PC, POP

:_RET_FALSE
SET A, 0
SET PC, POP



:name_GT
DAT name_NEQU
DAT 1
DAT ">"
:GT
DAT code_GT
:code_GT
SET B, POP
SET A, POP
SET C, 0
IFA A, B
  SET C, 1
SET PUSH, C
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_GTU
DAT name_GT
DAT 2
DAT ">U"
:GTU
DAT code_GTU
:code_GTU
SET B, POP
SET A, POP
SET C, 0
IFG A, B
  SET C, 1
SET PUSH, C
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_LT
DAT name_GTU
DAT 1
DAT "<"
:LT
DAT code_LT
:code_LT
SET B, POP
SET A, POP
SET C, 0
IFU A, B
  SET C, 1
SET PUSH, C
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_LTU
DAT name_LT
DAT 2
DAT "<U"
:LTU
DAT code_LTU
:code_LTU
SET B, POP
SET A, POP
SET C, 0
IFL A, B
  SET C, 1
SET PUSH, C
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_GE
DAT name_LTU
DAT 2
DAT ">="
:GE
DAT code_GE
:code_GE
SET B, POP
SET A, POP
IFE A, B
SET PC, true_GE

; Not equal, so check > 
SUB A, B
JSR _IS_POSITIVE
SET PC, done_GE

:true_GE
SET A, 1
:done_GE
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_LE
DAT name_GE
DAT 2
DAT "<="
:LE
DAT code_LE
:code_LE
SET B, POP
SET A, POP
IFE A, B
SET PC, true_LE

; Not equal, so check <
SUB A, B
JSR _IS_NEGATIVE
SET PC, done_LE

:true_LE
SET A, 1
:done_LE
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_ZEQU
DAT name_LE
DAT 2
DAT "0="
:ZEQU
DAT code_ZEQU
:code_ZEQU
SET A, POP
SET C, 0
IFE A, 0
SET C, 1 ; only run when =0
SET PUSH, C
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_ZNEQU
DAT name_ZEQU
DAT 3
DAT "0<>"
:ZNEQU
DAT code_ZNEQU
:code_ZNEQU
SET A, POP
SET C, 0
IFN A, 0
SET C, 1
SET PUSH, C
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_ZLT
DAT name_ZNEQU
DAT 2
DAT "0<"
:ZLT
DAT code_ZLT
:code_ZLT
SET A, POP
JSR _IS_NEGATIVE
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_ZGT
DAT name_ZLT
DAT 2
DAT "0>"
:ZGT
DAT code_ZGT
:code_ZGT
SET A, POP
IFE A, 0
SET PC, false_ZGT

; Not 0, check for positive.
JSR _IS_POSITIVE
SET PC, done_ZGT

:false_ZGT
SET A, 0
:done_ZGT
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_ZLE
DAT name_ZGT
DAT 3
DAT "0<="
:ZLE
DAT code_ZLE
:code_ZLE
SET A, POP
IFE A, 0
SET PC, true_ZLE

; Not 0, check negative
JSR _IS_NEGATIVE
SET PC, done_ZLE

:true_ZLE
SET A, 1
:done_ZLE
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_ZGE
DAT name_ZLE
DAT 3
DAT "0>="
:ZGE
DAT code_ZGE
:code_ZGE
SET A, POP
JSR _IS_POSITIVE
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_AND
DAT name_ZGE
DAT 3
DAT "AND"
:AND
DAT code_AND
:code_AND
SET A, POP
SET B, POP
AND A, B
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_OR
DAT name_AND
DAT 2
DAT "OR"
:OR
DAT code_OR
:code_OR
SET A, POP
SET B, POP
BOR A, B
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_XOR
DAT name_OR
DAT 3
DAT "XOR"
:XOR
DAT code_XOR
:code_XOR
SET A, POP
SET B, POP
XOR A, B
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_INVERT
DAT name_XOR
DAT 6
DAT "INVERT"
:INVERT
DAT code_INVERT
:code_INVERT
SET A, POP
XOR A, 0xffff
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_EXIT
DAT name_INVERT
DAT 4
DAT "EXIT"
:EXIT
DAT code_EXIT
:code_EXIT
; POPRSP
ADD J, 1
SET I, [J]
; END POPRSP
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_LIT
DAT name_EXIT
DAT 3
DAT "LIT"
:LIT
DAT code_LIT
:code_LIT
SET PUSH, [I] ; grab the value of the next item and push it onto the stack
ADD I, 1 ; and move I past the value.
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_STORE
DAT name_LIT
DAT 1
DAT "!"
:STORE
DAT code_STORE
:code_STORE
SET A, POP ; address to store at
SET B, POP ; value to store there
SET [A], B
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_FETCH
DAT name_STORE
DAT 1
DAT "@"
:FETCH
DAT code_FETCH
:code_FETCH
SET A, POP ; The address to fetch.
SET PUSH, [A] ; Fetch onto the stack.
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_ADDSTORE
DAT name_FETCH
DAT 2
DAT "+!"
:ADDSTORE
DAT code_ADDSTORE
:code_ADDSTORE
SET A, POP ; Address
SET B, POP ; Amount to add
ADD [A], B ; Add to it
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_SUBSTORE
DAT name_ADDSTORE
DAT 2
DAT "-!"
:SUBSTORE
DAT code_SUBSTORE
:code_SUBSTORE
SET A, POP ; Address
SET B, POP ; Amount to subtract
SUB [A], B
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


; Byte-aligned operations are skipped because DCPU-16 doesn't do byte access.



; Built-in variables

:name_STATE
DAT name_SUBSTORE
DAT 5
DAT "STATE"
:STATE
DAT code_STATE
:code_STATE
SET PUSH, var_STATE
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]

:var_STATE
DAT 0


:name_LATEST
DAT name_STATE
DAT 6
DAT "LATEST"
:LATEST
DAT code_LATEST
:code_LATEST
SET PUSH, var_LATEST
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]

:var_LATEST
DAT name_EXECUTE ; This is the last word in the built-in dictionary.


:name_HERE
DAT name_LATEST
DAT 4
DAT "HERE"
:HERE
DAT code_HERE
:code_HERE
SET PUSH, var_HERE
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]

:var_HERE
DAT init_HERE


:name_S0
DAT name_HERE
DAT 2
DAT "S0"
:S0
DAT code_S0
:code_S0
SET PUSH, var_S0
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]

:var_S0
DAT 0 ; Set properly in startup.


:name_BASE
DAT name_S0
DAT 4
DAT "BASE"
:BASE
DAT code_BASE
:code_BASE
SET PUSH, var_BASE
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]

:var_BASE
DAT 10


; Built-in constants

:name_VERSION
DAT name_BASE
DAT 7
DAT "VERSION"
:VERSION
DAT code_VERSION
:code_VERSION
SET PUSH, 2 ; Version number
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_R0
DAT name_VERSION
DAT 2
DAT "R0"
:R0
DAT code_R0
:code_R0
SET PUSH, return_stack_top
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_DOCOL
DAT name_R0
DAT 5
DAT "DOCOL"
:__DOCOL
DAT code_DOCOL
:code_DOCOL
SET PUSH, DOCOL
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_F_IMMED
DAT name_DOCOL
DAT 7
DAT "F_IMMED"
:__F_IMMED
DAT code_F_IMMED
:code_F_IMMED
SET PUSH, F_IMMED
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_F_HIDDEN
DAT name_F_IMMED
DAT 8
DAT "F_HIDDEN"
:__F_HIDDEN
DAT code_F_HIDDEN
:code_F_HIDDEN
SET PUSH, F_HIDDEN
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]



:name_F_LENMASK
DAT name_F_HIDDEN
DAT 9
DAT "F_LENMASK"
:__F_LENMASK
DAT code_F_LENMASK
:code_F_LENMASK
SET PUSH, F_LENMASK
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]



; Return stack access words.

:name_TOR
DAT name_F_LENMASK
DAT 2
DAT ">R"
:TOR
DAT code_TOR
:code_TOR
SET A, I ; Back up the value of I, the Forth word pointer.
SET I, POP ; Pop the value to store into I.
; PUSHRSP
SET [J], I
SUB J, 1
; END PUSHRSP
SET I, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_FROMR
DAT name_TOR
DAT 2
DAT "R>"
:FROMR
DAT code_FROMR
:code_FROMR
SET A, I ; Back up the value of I, the Forth word pointer.
; POPRSP
ADD J, 1
SET I, [J]
; END POPRSP
SET PUSH, I ; Store the value from the return stack.
SET I, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_RSPFETCH
DAT name_FROMR
DAT 4
DAT "RSP@"
:RSPFETCH
DAT code_RSPFETCH
:code_RSPFETCH
SET PUSH, J ; This doesn't actually point at the value.
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_RSPSTORE
DAT name_RSPFETCH
DAT 4
DAT "RSP!"
:RSPSTORE
DAT code_RSPSTORE
:code_RSPSTORE
SET J, POP ; Pop the value from the stack into J and hope the user knows what he's doing.
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_RDROP
DAT name_RSPSTORE
DAT 5
DAT "RDROP"
:RDROP
DAT code_RDROP
:code_RDROP
ADD J, 1
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_DSPFETCH
DAT name_RDROP
DAT 4
DAT "DSP@"
:DSPFETCH
DAT code_DSPFETCH
:code_DSPFETCH
SET A, SP
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_DSPSTORE
DAT name_DSPFETCH
DAT 4
DAT "DSP!"
:DSPSTORE
DAT code_DSPSTORE
:code_DSPSTORE
SET A, POP
SET SP, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]



; Input and output

:name_KEY
DAT name_DSPSTORE
DAT 3
DAT "KEY"
:KEY
DAT code_KEY
:code_KEY
JSR [key]
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]

:key
DAT _KEY ; Points to _KEY while interpreting and _KEY_DISK while LOADing a block.
; Default is to wait for input from the keyboard.

; WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
; Do not call _KEY_FILE or _KEY directly. JSR [key] instead.

:_KEY_DISK
; Called when we're reading from the disk. Expects disk_block and disk_pos to be set properly.
; Reads a whole block, skipping 0s. When it runs out of block, calls _LOAD_BLOCK_FINISHED.
; _LOAD_BLOCK_FINISHED will either return to _KEY user input or continue the reading of an interrupted block.
; Eventually the caller of KEY will get a keystoke back, one way or another.

; Retrieve the next value, which is at disk_pos.
:_KEY_DISK_LOOP
SET A, mmr
ADD A, [disk_pos]

ADD [disk_pos], 1

IFG [A], 0
  SET PC, _KEY_DISK_LOOP_DONE

IFE [disk_pos], 512
  SET PC, _LOAD_BLOCK_FINISHED
SET PC, _KEY_DISK_LOOP

:_KEY_DISK_LOOP_DONE
; Found a nonzero word to return.
SET A, [A]
SET PC, POP



:_KEY
; Loop until we get a character (ie. nonzero) from the keyboard hardware.
SET PUSH, C
:_KEY_INNER
SET A, 1
HWI [hw_keyboard]
IFE C, 0
  SET PC, _KEY_INNER

SET A, C
JSR _EMIT
SET A, C
SET C, POP
SET PC, POP ; return


; Output

:name_EMIT
DAT name_KEY
DAT 4
DAT "EMIT"
:EMIT
DAT code_EMIT
:code_EMIT
SET A, POP ; Retrieve the byte to write.
JSR _EMIT
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]



:name_WORD
DAT name_EMIT
DAT 4
DAT "WORD"
:WORD
DAT code_WORD
:code_WORD
JSR _WORD
SET PUSH, A ; address
SET PUSH, B ; length
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:_WORD
JSR [key] ; Loads the next byte into A.
IFE A, 0x5c ; backslash, the start of a line comment.
SET PC, _WORD_COMMENT
IFE A, 0x20 ; space, keep searching for real letters
SET PC, _WORD
IFE A, 0x11 ; \n, keep searching for real letters (DCPU-16 keyboard newline)
SET PC, _WORD
IFE A, 0x0a ; \n, keep searching for real letters (ASCII newline)
SET PC, _WORD
IFE A, 0x0d ; \r, keep searching for real letters
SET PC, _WORD
IFE A, 0x09 ; \t, keep searching for real letters
SET PC, _WORD

; Found a word.
SET B, _WORD_BUFFER
:_WORD_MAIN
SET [B], A
ADD B, 1
JSR [key]
IFE A, 0x10 ; backspace
  SET PC, _WORD_BACKSPACE
IFE A, 0x20 ; space, so jump down
SET PC, _WORD_COMPLETE
IFE A, 0x11 ; DCPU newline, jump down
SET PC, _WORD_COMPLETE
IFE A, 0x0a ; ASCII newline, jump down
SET PC, _WORD_COMPLETE
SET PC, _WORD_MAIN ; Non-whitespace, keep collecting letters.

; Complete word, so return it.
:_WORD_COMPLETE
SUB B, _WORD_BUFFER ; B now stores the length
SET A, _WORD_BUFFER ; and A the initial address
SET PC, POP ; return

; Handle backspace
; Bump the pointer back by two so it'll overwrite the previous good letter and then be set to overwrite the bad one.
; Kind of a hack, but it works.
:_WORD_BACKSPACE
SUB B, 2
SET A, [B]
SET PC, _WORD_MAIN

; And the code to skip past a comment.
:_WORD_COMMENT
JSR [key]
IFE A, 0x11 ; DCPU newline, end of line comment
  SET PC, _WORD ; Found a newline, jump to the top and keep reading words.
IFE A, 0x0a ; ASCII newline, end of line comment
  SET PC, _WORD ; Found a newline, jump to the top and keep reading words.
SET PC, _WORD_COMMENT ; Keep looping through the comment.

:_WORD_BUFFER
DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0



:name_NUMBER
DAT name_WORD
DAT 6
DAT "NUMBER"
:NUMBER
DAT code_NUMBER
:code_NUMBER
SET C, POP ; length of string
SET X, POP ; start address of string
JSR _NUMBER
SET PUSH, A ; parsed number
SET PUSH, C ; number of unparsed characters
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:_NUMBER
SET A, 0
SET B, 0

IFE C, 0 ; length 0 is an error, but we return 0 anyway.
SET PC, POP ; return

SET Y, [var_BASE]

; Check if the first character is '-'.
SET B, [X] ; Get the first character.
ADD X, 1
SET PUSH, 0 ; Put a 0 on the stack to signal positive.
IFN 0x2D, B
SET PC, _NUMBER_CHECK_DIGIT ; Not -, so it's just the first number.

SET A, POP ; Pop off that 0
SET PUSH, B ; Push the 0x2d, nonzero signals negative.
SUB C, 1 ; Decrement the length.
IFN C, 0
SET PC, _NUMBER_LOOP ; If C is nonzero, jump over the error handler.

; C is 0, the string was just '-'
SET B, POP ; Remove the flag we added to the stack.
SET C, 1 ; Set the number of unparsed characters to 1
SET PC, POP ; Return


; Loop reading digits
:_NUMBER_LOOP
MUL A, Y ; A *= BASE
SET B, [X] ; Get the next character
ADD X, 1

:_NUMBER_CHECK_DIGIT
IFG 0x30, B ; < ASCII 0
SET PC, _NUMBER_END
IFG 0x3A, B ; <= ASCII 9
SET PC, _NUMBER_FOUND_DIGIT
IFG 0x41, B ; < ASCII 'A'
SET PC, _NUMBER_END

SUB B, 8 ; 65-57 = 8, turns 'A' into '9' + 1

:_NUMBER_FOUND_DIGIT
; Adjust to be the actual number
SUB B, 0x30 ; ASCII '0'
IFG Y, B ; Make sure it's < BASE
SET PC, _NUMBER_GOOD
SET PC, _NUMBER_END

:_NUMBER_GOOD
ADD A, B
SUB C, 1 ; decrement the number of unparsed characters
IFG C, 0
SET PC, _NUMBER_LOOP ; loop if there's still bytes to be had

:_NUMBER_END
SET B, POP ; Grab the flag we pushed earlier.
IFE B, 0
SET PC, _NUMBER_RETURN ; Positive, so return.

; Negate the number.
XOR A, 0xffff ; Invert the bits.
ADD A, 1 ; And add one.

:_NUMBER_RETURN
SET PC, POP ; return



; DICTIONARY LOOKUP

:name_FIND
DAT name_NUMBER
DAT 4
DAT "FIND"
:FIND
DAT code_FIND
:code_FIND
SET C, POP ; length
SET X, POP ; address
JSR _FIND
SET PUSH, A ; address of dictonary entry (or 0)
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:_FIND
SET Y, [var_LATEST]
:_FIND_LOOP

; DEBUG: print the dictionary word here.
; HEX Y
; OUT 32
; 
; SET B, Y
; ADD B, 1
; SET Z, [B]
; AND Z, F_LENMASK
; 
; ADD B, 1
; :_FIND_DEBUG_LOOP
; OUT [B]
; SUB Z, 1
; ADD B, 1
; IFG Z, 0
; SET PC, _FIND_DEBUG_LOOP
; 
; OUT 10
; END DEBUG

IFE Y, 0
SET PC, _FIND_FAILED ; NULL pointer, reached the end.

; Check the lengths of the words.
; Note that if the F_HIDDEN flag is set on the word, then by a bit of trickery this
; won't pick the word (the length will appear to be wrong).
SET A, 0
ADD Y, 1 ; advance to the flags/length field.
SET B, [Y] ; grab that field
ADD Y, 1 ; advance Y to the first letter.
AND B, F_HIDDEN_AND_LENMASK
IFN C, B
SET PC, _FIND_FOLLOW_LINK

; Same length, so now compare in detail.
; C holds the length, use it to loop. Use A and B for the characters.
; Z is the index we're comparing.

SET Z, 0
:_FIND_COMPARE_LOOP
IFG C, Z
SET PC, _FIND_CONTINUE_COMPARING
SET PC, _FIND_FOUND ; If C == 0 then we've run out of letters, they match.

:_FIND_CONTINUE_COMPARING
SET A, Z ; A is now the index
ADD A, X ; A is now the actual address of the next letter.
SET A, [A] ; And now A is the letter itself.

SET B, Z
ADD B, Y ; the dictionary word
SET B, [B]

ADD Z, 1

IFN A, B
SET PC, _FIND_FOLLOW_LINK ; Not equal, follow the link pointer to the next word.
SET PC, _FIND_COMPARE_LOOP ; Equal, so keep looping.


:_FIND_FOLLOW_LINK
SUB Y, 2 ; move back to the pointer.
SET Y, [Y] ; back through link
SET PC, _FIND_LOOP

:_FIND_FOUND
SUB Y, 2 ; Reset Y to the beginning of the dictionary header.
SET A, Y
SET PC, POP

:_FIND_FAILED
SET A, 0 ; return 0 when failed.
SET PC, POP


:name_TCFA
DAT name_FIND
DAT 4
DAT ">CFA"
:TCFA
DAT code_TCFA
:code_TCFA
SET X, POP ; dictionary address.
JSR _TCFA
SET PUSH, X
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]

:_TCFA
SET A, 0
ADD X, 1 ; skip over link pointer, X now points to length.
SET A, [X] ; retrieve the length+flags
AND A, F_LENMASK ; mask out the length
ADD X, A ; move the pointer to the last letter
ADD X, 1 ; move the pointer to the codeword.
SET PC, POP ; return



:name_TDFA
DAT name_TCFA
DAT 4
DAT ">DFA"
:TDFA
DAT code_TDFA
:code_TDFA
SET X, POP
JSR _TCFA
ADD X, 1
SET PUSH, X
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]




; Compilation and defining

:name_CREATE
DAT name_TDFA
DAT 6
DAT "CREATE"
:CREATE
DAT code_CREATE
:code_CREATE
SET C, POP ; length
SET B, POP ; address of name

; Store the link pointer.
SET X, [var_HERE] ; address where we're going to put the header
SET A, [var_LATEST]
SET [X], A ; write LATEST to the link pointer.
SET [var_LATEST], X ; Move LATEST to point here.
ADD X, 1

; Length byte and the word itself.
SET [X], C ; store length
ADD X, 1

:_CREATE_NAME_LOOP
SET [X], [B] ; Store the next letter of the name.
ADD X, 1
ADD B, 1
SUB C, 1
IFG C, 0
SET PC, _CREATE_NAME_LOOP

; Move HERE.
SET [var_HERE], X ; X points after the last word of the name.
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]



:name_COMMA
DAT name_CREATE
DAT 1
DAT ","
:COMMA
DAT code_COMMA
:code_COMMA
SET A, POP ; the value to store
JSR _COMMA
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]

:_COMMA
SET X, [var_HERE]
SET [X], A
ADD X, 1
SET [var_HERE], X
SET PC, POP ; return



:name_LBRAC
DAT name_COMMA
DAT 0x81 ; F_IMMED | 1
DAT "["
:LBRAC
DAT code_LBRAC
:code_LBRAC
SET [var_STATE], 0
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_RBRAC
DAT name_LBRAC
DAT 1
DAT "]"
:RBRAC
DAT code_RBRAC
:code_RBRAC
SET [var_STATE], 1
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]



:name_COLON
DAT name_RBRAC
DAT 1
DAT ":"
:COLON
DAT DOCOL
; Get the name of the new word.
DAT WORD
; Create the header
DAT CREATE
; Append the codeword DOCOL.
DAT LIT
DAT DOCOL
DAT COMMA
; Make the word hidden.
DAT LATEST
DAT FETCH
DAT HIDDEN
; Back to compile mode.
DAT RBRAC
; Return
DAT EXIT


:name_SEMICOLON
DAT name_COLON
DAT 0x81 ; F_IMMED | 1
DAT ";"
:SEMICOLON
DAT DOCOL
; Append EXIT so the word will return.
DAT LIT
DAT EXIT
DAT COMMA
; Toggle hidden flag to unhide the word.
DAT LATEST
DAT FETCH
DAT HIDDEN
; Return to immediate mode.
DAT LBRAC
DAT EXIT



:name_IMMEDIATE
DAT name_SEMICOLON
DAT 0x89 ; F_IMMED | 9
DAT "IMMEDIATE"
:IMMEDIATE
DAT code_IMMEDIATE
:code_IMMEDIATE
SET X, [var_LATEST]
ADD X, 1
XOR [X], F_IMMED ; toggle the IMMEDIATE bit.
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]



:name_HIDDEN
DAT name_IMMEDIATE
DAT 6
DAT "HIDDEN"
:HIDDEN
DAT code_HIDDEN
:code_HIDDEN
SET X, POP ; Address of the dictionary entry.
ADD X, 1 ; Point at flags+length word.
XOR X, F_HIDDEN ; Toggle the HIDDEN bit.
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_HIDE
DAT name_HIDDEN
DAT 4
DAT "HIDE"
:HIDE
DAT DOCOL
; Get the word after HIDE.
DAT WORD
; Look it up in the dictionary.
DAT FIND
; Set the flag.
DAT HIDDEN
DAT EXIT


:name_TICK
DAT name_HIDE
DAT 1
DAT "'"
:TICK
DAT code_TICK
:code_TICK
SET A, [I] ; Get the address of the next word.
ADD I, 1 ; Skip that word.
SET PUSH, A ; Push the address.
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]
; TODO: That definition only works in compiled code. According to the jonesforth
;       commentary, TICK can be written with WORD, FIND and >CFA so that it'll
;       run in immediate mode too.



; BRANCHING PRIMITIVES

:name_BRANCH
DAT name_TICK
DAT 6
DAT "BRANCH"
:BRANCH
DAT code_BRANCH
:code_BRANCH
; IFG 0x0750, I
; SET PC, _BRANCH_INNER
; HEX I
; OUT 32
; HEX [I]
; OUT 10
:_BRANCH_INNER
ADD I, [I] ; Add to I the value at I, which is the offset for the branch.
; Beautiful and cunning.
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_ZBRANCH
DAT name_BRANCH
DAT 7
DAT "0BRANCH"
:ZBRANCH
DAT code_ZBRANCH
:code_ZBRANCH
SET A, POP
IFE A, 0
SET PC, code_BRANCH

; Otherwise, skip over the offset.
ADD I, 1
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]



; LITERAL STRINGS

:name_LITSTRING
DAT name_ZBRANCH
DAT 9
DAT "LITSTRING"
:LITSTRING
DAT code_LITSTRING
:code_LITSTRING
SET A, [I] ; Get the length of the string from the next word.
ADD I, 1 ; And skip over it.
SET PUSH, I ; Push the address of the start of the string (I).
SET PUSH, A ; Push the length.
ADD I, A ; Skip past the string.
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_TELL
DAT name_LITSTRING
DAT 4
DAT "TELL"
:TELL
DAT code_TELL
:code_TELL
SET C, POP ; Length of the string.
SET X, POP ; Address of the start of the string.

IFE C, 0
SET PC, _TELL_DONE

JSR _TELL
:_TELL_DONE
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:_TELL
SET A, [X] ; Get the next character.
JSR _EMIT
ADD X, 1
SUB C, 1
IFG C, 0
  SET PC, _TELL
SET PC, POP


; QUIT and INTERPRET

:name_QUIT
DAT name_TELL
DAT 4
DAT "QUIT"
:QUIT
DAT DOCOL
DAT R0
DAT RSPSTORE
DAT INTERPRET
DAT BRANCH
DAT 0xfffe ; Branch by -2, pointing back at INTERPRET.
; Deliberately no EXIT call here.



:name_INTERPRET
DAT name_QUIT
DAT 9
DAT "INTERPRET"
:INTERPRET
DAT code_INTERPRET
:code_INTERPRET
JSR _WORD ; A stores the address, B the length.

; Store the pointer to the word and the length of it, so it can be output for debugging later if necessary.
SET [errdata], A
SET [errdatalen], B

; DEBUG
; SET PUSH, A
; SET PUSH, B
; SET X, A
; SET C, B
; JSR _TELL
; SET B, POP
; SET A, POP
; OUT 32
; END DEBUG

; Check the dictionary.
SET [interpret_is_lit], 0 ; Not a literal number (not yet anyway...)

; Adjust for the differences between my FIND and WORD. TODO: Clean this up.
SET C, B
SET X, A
SET PUSH, B ; save B
JSR _FIND
SET B, POP ; and restore.
IFE A, 0
SET PC, _INTERPRET_NOT_IN_DICT

; In the dictionary. Check if it's an IMMEDIATE codeword.
SET C, A ; C is the address of the dictionary header.
ADD C, 1 ; Now of the length+flags word.
SET C, [C] ; Retrieve the actual value.
SET PUSH, C ; Just stash that value for now.

; _TCFA expects the address in X.
SET X, A
JSR _TCFA
SET A, X ; Move the address of the codeword into A as well.

; Now X points at the codeword.
SET C, POP ; Retrieve the length+flags word.
IFB C, F_IMMED ; Check the IMMEDIATE flag.
SET PC, _INTERPRET_EXECUTE ; Jump straight to executing.
SET PC, _INTERPRET_COMPILE_CHECK ; Jump to the interpret/compile check.

:_INTERPRET_NOT_IN_DICT
; Not in the dictionary, so assume it's a literal number.
SET [interpret_is_lit], 1

; _NUMBER expects the length in C and address in X.
; It returns the number in A and number unparsed in C.
SET C, B ; The length was in B, now it's in C.
JSR _NUMBER

IFG C, 0
SET PC, _INTERPRET_ILLEGAL_NUMBER

SET B, A
SET A, LIT ; Set the word to LIT.


:_INTERPRET_COMPILE_CHECK
; Are we compiling or executing?

SET Y, [var_STATE]
IFE Y, 0
SET PC, _INTERPRET_EXECUTE ; Executing, so jump to there.

; Compiling. Append the word to the current dictionary definition.
JSR _COMMA ; The word lives in A, which is what _COMMA wants.

IFE [interpret_is_lit], 0
SET PC, _INTERPRET_END ; Not a literal, so done.

; Literal number in play, so push it too.
SET A, B ; Move the literal from B to A and put that into the code too.
JSR _COMMA
SET PC, _INTERPRET_END


:_INTERPRET_EXECUTE
; Executing, run the word.

IFG [interpret_is_lit], 0
SET PC, _INTERPRET_PUSH_LITERAL

; Not a literal, execute it now.
; This never returns, but the codeword will eventually call NEXT, which will
; reenter the loop in QUIT.

SET PC, [A]


:_INTERPRET_PUSH_LITERAL
; Executing a literal means pushing it onto the stack.
SET PUSH, B
SET PC, _INTERPRET_END


:_INTERPRET_ILLEGAL_NUMBER
; We couldn't find the word in the dictionary, and it isn't a number either.
; Show an error message.
; TODO: improve the amount of information.

SET X, errmsg
SET C, [errmsglen]
JSR _TELL
SET A, 0x0a ; newline
JSR _EMIT

SET X, [errdata]
SET C, [errdatalen]
JSR _TELL
SET A, 0x0a ; newline
JSR _EMIT

:_INTERPRET_END
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]

:errmsg
DAT "Interpreter error: Unknown word or bad number."
:errmsglen
DAT 46
:interpret_is_lit
DAT 0

:errdata
DAT 0
:errdatalen
DAT 0



; ODDS AND ENDS

:name_CHAR
DAT name_INTERPRET
DAT 4
DAT "CHAR"
:CHAR
DAT code_CHAR
:code_CHAR
JSR _WORD ; Address in A, length in B.
SET PUSH, [A]
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_CROAK
DAT name_CHAR
DAT 5
DAT "CROAK"
:CROAK
DAT code_CROAK
:code_CROAK
HCF 25
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_BREAK
DAT name_CROAK
DAT 5
DAT "BREAK"
:BREAK
DAT code_BREAK
:code_BREAK
HCF 0
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_BLINK
DAT name_BREAK
DAT 5
DAT "BLINK"
:BLINK
DAT code_BLINK
:code_BLINK
SET A, POP ; Get the boolean
AND [cursor_color_mask], 0xff7f
IFG A, 0
  BOR [cursor_color_mask], 0x0080
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_FG
DAT name_BLINK
DAT 2
DAT "FG"
:FG
DAT code_FG
:code_FG
SET A, POP
SHL A, 12 ; Shift it into a foreground color
AND [cursor_color_mask], 0x0fff
BOR [cursor_color_mask], A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_BG
DAT name_FG
DAT 2
DAT "BG"
:BG
DAT code_BG
:code_BG
SET A, POP
AND A, 0x000f
SHL A, 8
AND [cursor_color_mask], 0xf0ff
BOR [cursor_color_mask], A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_COLORS
DAT name_BG
DAT 6
DAT "COLORS"
:COLORS
DAT code_COLORS
:code_COLORS
SET A, [cursor_color_mask]
SET B, A
SHR B, 8
AND B, 0x000f
SET PUSH, B ; bg goes on first

SHR A, 12
SET PUSH, A ; and fg on top
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]



:name_CLEARSCREEN
DAT name_COLORS
DAT 11
DAT "CLEARSCREEN"
:CLEARSCREEN
DAT code_CLEARSCREEN
:code_CLEARSCREEN
JSR _CLEARSCREEN
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]

; insert new words here


:name_LOAD
DAT name_CLEARSCREEN
DAT 4
DAT "LOAD"
:LOAD
DAT code_LOAD
:code_LOAD

SET B, [load_stack]
SUB B, 3
SET [B], [disk_pos]
SET [B+1], [disk_block]
SET [B+2], [key]
SET [load_stack], B

SET B, POP ; The block to load.
SET [disk_pos], 0
SET [disk_block], B
SET [key], _KEY_DISK

SET A, B
JSR _READ_BLOCK

; Now finished reading the block.
; Return and let INTERPRET process the new block.
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:load_stack
DAT load_stack_top

; Called when a block is finished processing. Pops the previous state and carries on by re-calling KEY.
:_LOAD_BLOCK_FINISHED
SET PUSH, A
SET PUSH, B
SET B, [load_stack]

; If we've run out of LOADs to perform, force a return to keyboard interpreting.
IFE B, load_stack_top
  SET PC, _LOAD_BLOCK_FINAL
SET PC, _LOAD_BLOCK_MAIN

:_LOAD_BLOCK_FINAL
SET [key], _KEY
SET PC, _LOAD_BLOCK_DONE

:_LOAD_BLOCK_MAIN
SET [disk_pos], [B]
SET [disk_block], [B+1]
SET [key], [B+2]
ADD B, 3
SET [load_stack], B

SET A, [disk_block]
JSR _READ_BLOCK

:_LOAD_BLOCK_DONE
SET B, POP
SET A, POP

SET PC, [key] ; Call KEY again and let it carry on.



:name_READBLOCK
DAT name_LOAD
DAT 9
DAT "READBLOCK"
:READBLOCK
DAT code_READBLOCK
:code_READBLOCK
SET A, POP ; Pop the block number to read.
JSR _READ_BLOCK ; Blocks until the block is ready.
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:name_WRITEBLOCK
DAT name_READBLOCK
DAT 10
DAT "WRITEBLOCK"
:WRITEBLOCK
DAT code_WRITEBLOCK
:code_WRITEBLOCK
SET A, POP ; Pop the block number to write.
JSR _WRITE_BLOCK
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


; Expects the block to read in A.
; Blocks until the read is complete.
; Married to _WRITE_BLOCK and the other handlers below.
:_READ_BLOCK
SET PUSH, X
SET PUSH, Y

SET X, A
SET A, 2 ; Read
SET Y, mmr

SET PC, _DISK_GO

; Expects the block to write in A.
; Blocks until the read is complete.
; Married to _READ_BLOCK, and the other handlers below.
:_WRITE_BLOCK
SET PUSH, X
SET PUSH, Y

SET X, A
SET A, 3 ; Write
SET Y, mmr

;SET PC, _DISK_GO

:_DISK_GO
; Actually fires the HWI to begin the disk read/write.
HWI [hw_disk]
; B is now set to 1 on successfully starting, 0 otherwise. HCF if it's not 1.
; This is justified because my code is supposed to block until the read/write is done.
IFN B, 1
  HCF 0

:_DISK_WAIT
SET PC, _DISK_WAIT ; infinite loop

; The interrupt handler will move PC here when the interrupt fires.
; It returns to the originally caller of _READ_BLOCK/_WRITE_BLOCK.
:_DISK_WAIT_DONE
SET Y, POP
SET X, POP
SET PC, POP



:name_MMR
DAT name_WRITEBLOCK
DAT 3
DAT "MMR"
:MMR
DAT code_MMR
:code_MMR
SET PUSH, mmr
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]



:name_DEBUG
DAT name_MMR
DAT 5
DAT "DEBUG"
:DEBUG
DAT code_DEBUG
:code_DEBUG
HWI [hw_debug]
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


:randseed
DAT 0x5678
DAT 0x1234

; ( -- random_word )
:name_RANDOM
DAT name_DEBUG
DAT 6
DAT "RANDOM"
:RANDOM
DAT code_RANDOM
:code_RANDOM

SET B, [randseed]
SET A, [randseed + 1]
MUL A, 0x660D
MUL B, 0x0019
MUL [randseed], 0x660D
ADX A, B
ADD [randseed], 1
ADD A, EX
SET [randseed + 1], A
SET PUSH, A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]



; ( w -- )
; Sets the random seed to
; w + 0x89d2, w
:name_SETSEED
DAT name_RANDOM
DAT 7
DAT "SETSEED"
:SETSEED
DAT code_SETSEED
:code_SETSEED
SET A, POP
SET B, A
ADD B, 0x89d2
SET [randseed], B
SET [randseed+1], A
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]



:name_EXECUTE
DAT name_SETSEED
DAT 7
DAT "EXECUTE"
:EXECUTE
DAT code_EXECUTE
:code_EXECUTE
SET A, POP ; grab the execution token off the stack and jump to it.
SET PC, [A]
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]


; EXECUTE needs to be the last word, or the initial value of var_LATEST needs updating.




:startup
SET J, return_stack_top ; Initializing the return stack pointer.
SET SP, data_stack_top ; Move SP down 1KB to make room for the return stack. This is the data stack.
SET [var_S0], SP ; Set S0 to the top of the stack.

; Explore hardware, initialize keyboard and display.

HWN Z ; The number of devices.

:hwloop
IFE Z, 0
  SET PC, launch

SUB Z, 1
HWQ Z

IFE A, 0x7406
  IFE B, 0x30cf
    SET PC, _init_keyboard

IFE A, 0xf615
  IFE B, 0x7349
    SET PC, _init_monitor

IFE A, 0x24c5
  IFE B, 0x4fd5
    SET PC, _init_disk

IFE A, 0x923d
  IFE B, 0x581f
    SET [hw_debug], Z

SET PC, hwloop


; Z holds the hardware number
; keyboard initializer
:_init_keyboard
SET [hw_keyboard], Z
SET A, 3 ; Interrupt config
SET B, 0 ; Disable interrupts (we'll poll it, it queues keys)
HWI Z
SET PC, hwloop

; monitor initializer
:_init_monitor
SET [hw_monitor], Z

SET A, 0 ; Memory map VRAM
SET B, vram
HWI Z

; TODO Do I need to set these or will it assume default?
SET A, 1 ; Memory map font
SET B, 0 ; default
HWI Z

SET A, 2 ; Memory map palette
SET B, 0 ; default
HWI Z

SET PC, hwloop

; disk initialization
:_init_disk
SET [hw_disk], Z

SET [disk_block], 1
SET [disk_pos], 0

; Set up the disk interrupt.
IAS interrupt_handler

SET A, 1 ; Configure interrupts
SET X, disk_interrupt_msg
HWI Z


; Read block 0 so it's ready for the code.
SET A, 1
JSR _READ_BLOCK

SET PC, hwloop

:disk_block
DAT 0
:disk_pos
DAT 0


; Launch the interpreter
:launch
SET A, QUIT ; TODO This can be removed, I think?
SET I, cold_start ; Initialize the interpreter.
; NEXT
SET A, [I]
ADD I, 1
SET PC, [A]

:cold_start
DAT QUIT


:hw_keyboard
DAT 0
:hw_monitor
DAT 0
:hw_disk
DAT 0
:hw_debug
DAT 0

; 384 words, 32x12, of space for VRAM
; using default fonts and such for now.
:vram
DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

:cursor_row
DAT 0
:cursor_col
DAT 0
:cursor_color_mask
DAT 0xf000 ; Yellow on black for the default.


; VIDEO CODE

; Adds a single character, stored in A, to the display, wrapping appropriately.
; Destroys A. Doesn't clobber anything else.
; JSR to this
:_EMIT
SET PUSH, X

; Check for special characters.
IFE A, 0x0a ; ASCII newline
  SET PC, _EMIT_NEWLINE
IFE A, 0x11 ; DCPU newline
  SET PC, _EMIT_NEWLINE
IFE A, 0x10 ; DCPU backspace
  SET PC, _EMIT_BACKSPACE

; Standard printable character.
SET X, [cursor_row]
MUL X, 32
ADD X, [cursor_col]
ADD X, vram

BOR A, [cursor_color_mask]
SET [X], A

ADD [cursor_col], 1
IFL [cursor_col], 32
  SET PC, _EMIT_DONE

:_EMIT_NEWLINE
SET [cursor_col], 0
ADD [cursor_row], 1
IFL [cursor_row], 12
  SET PC, _EMIT_DONE

; Otherwise, the screen needs to be cleared and wrapped to the top.
JSR _CLEARSCREEN

:_EMIT_DONE
SET X, POP
SET PC, POP


:_EMIT_BACKSPACE
IFE [cursor_col], 0
  SET PC, _EMIT_DONE

SUB [cursor_col], 1
SET A, 0x20 ; Space
JSR _EMIT
SUB [cursor_col], 1
SET PC, _EMIT_DONE


; Clears the screen and positions the cursor at 0,0.
:_CLEARSCREEN
SET PUSH, A
SET PUSH, B
SET A, vram
SET B, A
ADD B, 384

:_CLEARSCREEN_LOOP
SET [A], 0xf020 ; space, black background
ADD A, 1
IFL A, B
  SET PC, _CLEARSCREEN_LOOP

SET [cursor_col], 0
SET [cursor_row], 0

SET B, POP
SET A, POP
SET PC, POP


; Interrupt handler
:interrupt_handler
; Currently the only interrupt we're getting is the disk interrupt, no need to check messages.
; We're also not in a terrible hurry in this handler, since disk interrupts can't come closer together than 1668 cycles.
; When a disk interrupt comes in, change the enstacked PC (two deep on the stack) to _DISK_WAIT_DONE.
SET [SP+1], _DISK_WAIT_DONE
RFI 0

; This comes last.
:init_HERE
DAT 0


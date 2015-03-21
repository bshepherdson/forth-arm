\ Queue implementation for Forth ARM.
\ These queues treat the pointers as the values, and expect the link
\ pointer to be stored at the top of the structure.
\ The queue object itself it represented as a pair of pointers, allocated
\ in HERE-space of the running thread. Their address is returned from
\ QUEUE-MAKE, and is passed to other QUEUE-* words.
\ The first pointer is the head of the queue, the second the tail.
\ If the queue is empty, both are 0.

\ QUEUE is a defining word that takes a name and produces a word for a
\ QUEUE cell.

: QUEUE-MAKE ( -- queue )
    HERE @ 2 CELLS ALLOT ( queue )
    0 OVER !
    0 OVER CELL+ !
  ;

\ Creates a new definition and compiles two 0s into its data space.
\ When executed it returns the data-space pointer, which is correct.
: QUEUE ( -- ) ( -- a-addr )
    CREATE 0 , 0 , ;

\ Returns the head of the queue WITHOUT popping it.
: QUEUE-HEAD ( queue -- a-addr ) @ ;
\ Likewise
: QUEUE-TAIL ( queue -- a-addr ) CELL+ @ ;

\ Actually performs a pop and returns the popped value.
: QUEUE-POP ( queue -- a-addr )
    DUP QUEUE-HEAD ( queue head )
    DUP @          ( queue head link )
    ROT            ( head link queue )
    2DUP !         ( head link queue ) \ Write the new head pointer.
    OVER 0= IF CELL+ ! ELSE 2DROP THEN \ 0 tail pointer if head is now null.
    ( head )
  ;

: QUEUE-PUSH ( a-addr queue -- )
    DUP QUEUE-TAIL ( new queue tail )
    DUP 0= IF      ( new queue tail )
        DROP
        2DUP !     ( new queue )
        2DUP CELL+ ! ( new queue )
        DROP 0 !   ( ) \ Write 0 into the link pointer.
    ELSE           ( new queue tail )
        ROT        ( queue tail new )
        2DUP SWAP ! ( queue tail new ) \ Write new value into the tail's link
        0 OVER !   ( queue tail new ) \ Write 0 into new's link.
        NIP SWAP CELL+ ! \ Write new into queue[1] as the new tail.
    THEN
  ;

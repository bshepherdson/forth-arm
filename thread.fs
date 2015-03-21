\ Thread implementation for Forth ARM.
\ Requires queue.fs

\ This is based on the native word (CURRENT-THREAD), which returns a cell
\ holding a pointer to the currently running thread. The beginning of each
\ thread is the following structure:
\ All of these are ( a-addr -- a-addr ) offset adjusters.
: THREAD-LINK ;
: THREAD-HERE       1  CELLS + ;
: THREAD-LATEST     2  CELLS + ;
: THREAD-STATE      3  CELLS + ;
: THREAD-BASE       4  CELLS + ;
: THREAD-LOOP-SP    5  CELLS + ;
: THREAD-LOOP-STACK 6  CELLS + ;
: THREAD-INTERP     10 CELLS + ;
: THREAD-TOP        11 CELLS + ;
: THREAD-RSP        12 CELLS + ;
: THREAD-DSP        13 CELLS + ;

: THREAD-HEADER-SIZE ( -- u ) 16 CELLS ;

65535 CONSTANT THREAD-SIZE

\ The global queue of threads waiting to run.
\ Global since it's created during the single-threaded load process.
\ Starts empty, since the only thread that exists is currently running.
QUEUE THREAD-QUEUE

\ Constructs a new thread with the given size.
\ This size is not checked, but it should be at least 16K or so, minimum.
\ Threads inherit their parent's LATEST, but BASE and STATE are reset to 0.
\ Return stacks are set to be 1K WORDS, data stack below that.
: (MAKE-THREAD) ( u -- a-addr )
    DUP ALLOCATE     ( size thread ior )
    DROP             ( size thread ) \ XXX Dangerous, ignoring errors.
    DUP THREAD-HEADER-SIZE +    ( size thread HERE-value )
    OVER THREAD-HERE !          ( size thread )
    2DUP + OVER THREAD-TOP !    ( size thread )
    LATEST OVER THREAD-LATEST ! ( size thread )
    10 OVER THREAD-BASE !       ( size thread )
    DUP THREAD-LOOP-STACK OVER THREAD-LOOP-SP ! ( size thread )
    0 OVER THREAD-INTERP !      ( size thread )

    2DUP + OVER THREAD-RSP !    ( size thread )
    2DUP + 1024 CELLS - OVER THREAD-DSP ! ( size thread )
    NIP
  ;


\ Advances to the next thread without saving this one.
\ Slightly shaky because we're using free()d memory if the previous
\ thread has died.
: NEXT-THREAD ( -- )
    THREAD-QUEUE QUEUE-POP ( new-thread )
    \ Load that thread's DSP into the real one. That will magically put its
    \ RSP and current thread there too.
    \ This very gently abuses the calling thread's data stack, but it
    \ should be harmless.
    DUP 0= IF DIE THEN    \ Bail if the thread is NULL.
    THREAD-DSP @ DSP!     ( new-thread rsp )
    RSP!                  ( new-thread )
    (CURRENT-THREAD) !    ( )
  ; \ This returns to whatever the newly loaded thread was doing before.

\ Abandon the current thread, by popping it and not pushing it.
\ Slightly shaky because it's free()ing itself before calling the next.
\ TODO No free() call for now, try with it later on.
\ Alternatively to free(), just make a pool of threads waiting to be used.
: THREAD-DIE ( -- ) NEXT-THREAD ;


\ Spawns a thread with a given size and sets it running the given XT.
\ That requires calling (MAKE-THREAD) to get a base thread, and then setting its
\ top few return-stack and data-stack values.
\ Return stack: THREAD-DIE xt's-body
\ Data stack:   thread RSP
\ That will make it run correctly when popped by PAUSE.
\ TODO Consider copying arguments to the thread from this stack?
\ They're already in the right order for copying, it wouldn't be hard to copy
\ deeper than the two values currently present.
: MAKE-THREAD ( xt -- )
    >BODY  >R                 ( R: xt-body )
    THREAD-SIZE (MAKE-THREAD) ( thread )
    \ Now load that thread's RSP and store the xt's body.
    DUP THREAD-RSP            ( thread *rsp )
    DUP @                     ( thread *rsp rsp )
    1 CELLS -                 ( thread *rsp rsp' )
    ' THREAD-DIE >BODY OVER ! ( thread *rsp rsp' )
    1 CELLS -                 ( thread *rsp rsp'' )
    R> OVER !                 ( thread *rsp rsp'' )
    2DUP SWAP !               ( thread *rsp rsp'' )
    NIP                       ( thread rsp'' )
    \ Now the thread's return stack is ready.
    \ Its data stack must contain its RSP value and the thread value.
    \ Similar to the above, load the thread's DSP pointer, and decrement it
    \ while storing the values into it.
    OVER THREAD-DSP           ( thread rsp'' *dsp )
    ROT DUP >R                ( rsp'' *dsp thread   R: thread )
    OVER @ 1 CELLS -          ( rsp'' *dsp thread dsp' )
    2DUP ! NIP                ( rsp'' *dsp dsp' )
    1 CELLS -                 ( rsp'' *dsp dsp'' )
    ROT OVER !                ( *dsp dsp'' )
    SWAP !                    ( ) ( R: thread )
    R>                        ( thread )
    THREAD-QUEUE QUEUE-PUSH  ( )
  ;


\ Suspends the current thread, for co-operative multi-tasking.
\ It might run again immediately if it's the only one.
\ The process is: fetch the current thread, and store it into the thread queue.
\ Pop the frontmost thread off the queue, by loading its stack pointers.
\ Return from PAUSE to whatever that thread was doing before.
\ New threads, therefore, simply need to put the codeword for their task at the
\ top of their return stacks, and be enqueued.
: PAUSE ( -- )
    (CURRENT-THREAD) @ RSP@ DSP@ ( thread rsp dsp )
    \ dsp here points at where rsp is on the stack.
    ROT              ( rsp dsp thread )
    2DUP THREAD-DSP ! ( rsp dsp thread )
    NIP 2DUP THREAD-RSP ! ( rsp thread )
    NIP               ( thread )
    THREAD-QUEUE QUEUE-PUSH ( )
    NEXT-THREAD
  ;


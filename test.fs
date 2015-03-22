\ Optimistic thread testing. Fully expect this to burst into flames.

: TEST-THREAD ( -- )
    ." Thread running 1" CR
    PAUSE
    ." Thread running 2" CR
    PAUSE
    ." Thread running 3" CR
  ;

: TEST ( -- )
    ." Before creating thread" CR
    ' TEST-THREAD MAKE-THREAD
    ." After creating thread" CR
    PAUSE
    ." After PAUSE" CR
    PAUSE
    ." After PAUSE 2" CR
    PAUSE
    ." After PAUSE 3" CR
    PAUSE
    ." Just me now" CR
    DIE
  ;

: TEST-DETAILS
    ' TEST-THREAD ." test-thread " .H CR
    ' TEST ." test: " .H CR
;
TEST-DETAILS

TEST


512 BUFFER: buf

: TEST
    S" test2" W/O CREATE-FILE ABORT" Couldn't open file!" \ fileid
    >R
    S" Short string." \ c-addr u
    R> WRITE-FILE ABORT" Failed to write" \ (empty)
;

TEST


\ Unix system calls implemented using the underlying Forth words

\ File-related calls
\ int open(string, mode)
: OPEN ( mode str -- fd ) 5 SYSCALL2 ;

\ File modes
0 CONSTANT O_RDONLY

\ int read(fd, buf, len)
: READ ( len buf fd -- bytes_read ) 3 SYSCALL3 ;
\ int close(fd) -- returns 1 on success and 0 on failure
: CLOSE ( fd -- ? ) 6 SYSCALL1 NOT ;


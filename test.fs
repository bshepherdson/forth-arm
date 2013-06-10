: foo 65 EMIT ;
VARIABLE v
: bar ['] foo ;
bar v !
v @ EXECUTE

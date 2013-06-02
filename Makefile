
default: forth

forth.o: forth.s
	as -o forth.o forth.s

forth: forth.o
	gcc -o forth forth.o -lc

clean:
	rm -f *.o forth


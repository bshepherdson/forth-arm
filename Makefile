
default: forth

forth.o: forth.s
	as -o forth.o forth.s

forth: forth.o
	ld -o forth forth.o

clean:
	rm *.o forth


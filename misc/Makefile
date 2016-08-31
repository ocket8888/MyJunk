pokesim: pokesim.o
	ld -o pokesim pokesim.o

pokesim.o:
	nasm -f elf64 -o pokesim.o -w+orphan-labels pokesim.asm

all: clean pokesim

clean:
	rm -f pokesim pokesim.o

.PHONY: clean

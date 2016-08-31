;;macros
%define stdin 0
%define stdout 1
%define stderr 2
%define sys_exit 1
%define sys_write 4
%define sys_read 3
%define sys_open 5
%define sys_close 4
%define rdonly 0
%define wonly 1
%define rdw 2
%define syscall int 80h

%macro println 2
	mov eax,sys_write
	mov ebx,%1
	mov ecx,%2
	mov edx,%2Len
	syscall
	mov eax,sys_write
	mov ebx,%1
	mov ecx,endl
	mov edx,endlLen
	syscall
%endmacro

%macro print 2
	mov eax,sys_write
	mov ebx,%1
	mov ecx,%2
	mov edx,%2Len
	syscall
%endmacro

section .data
	selectprompt:	dw	'Select a Pokemon',0xA
	selectpromptLen:	equ	 $-selectprompt
	Squirtle:	dw	'Squirtle'
	SquirtleLen:	equ	 $-Squirtle
	Bulbasaur:	dw	'Bulbasaur'
	BulbasaurLen:	equ	 $-Bulbasaur
	Charmander:	dw	'Charmander'
	CharmanderLen:	equ	 $-Charmander
	endl:		db	0xA
	endlLen:	equ	 $-endl
	colon		db	':'
	colonLen:	equ	$-colon
	statsfile:	db	'datafile.dat'
	selection1:	db	'1) '
	selection1Len:	equ	$-selection1
	selection2:	db	'2) '
	selection2Len:	equ	$-selection2
	selection3:	db	'3) '
	selection3Len:	equ	$-selection3
	selerr:		db	'Please enter 1,2 or 3'
	selerrLen:	equ	$-selerr

section .text
	global _start
	
	_start:
		
		;select a pokemon
		pokeSelect:
		
			;display available pokemon (If this is ever more than the three starters, kill me)
			call dispAvail
		
			;read user response into ibuffer
			mov eax,sys_read
			mov ebx,stdin
			mov ecx,ibuffer
			mov edx,2
			syscall
	
			;put the result into a register, so we can do stuffs with it
			movx byte eax,[ibuffer]

			cmp eax,1
			jne pkmnsel2
			jmp pokeSelectDone
			pkmnsel2:
				cmp eax,2
				jne pkmnsel3
				jmp pokeSelectDone
			pkmnsel3:
				cmp eax,3
				jne selectagain
				jmp pokeSelectDone
			selectagain
				println stdout,selerr
				jmp pokeSelect
		pokeSelectDone:

		mov eax,sys_write
		mov ebx,stdout
		syscall

		mov eax,sys_open
		mov ebx,statsfile
		mov ecx,rdw
		syscall
		
		mov eax,sys_read
		mov ebx,statsfile
		mov ecx,fbuffer
		mov edx,1024
		syscall

		mov eax,sys_close
		xor ebx,ebx
		syscall

		;write file output to stdout
		mov eax,sys_write
		mov ebx,stdout
		mov ecx,fbuffer
		syscall

		;exit program. Getting here is the dream
		mov eax,sys_exit
		mov ebx,0
		syscall





	dispAvail:

		;write prompt
		print stdout,selectprompt

		;write "1) Bulbasaur"
		print stdout,selection1
		println stdout,Bulbasaur

		;write "2) Charmander"
		print stdout,selection2
		println stdout,Charmander

		;write "3) Squirtle"
		print stdout,selection3
		println stdout,Squirtle

		;write colon to indicate user input required
		mov eax,sys_write
		mov ecx,colon
		mov edx,colonLen
		syscall
		ret





section .bss
	ibuffer:	resw	1024; reserve a kilobyte for the cli input buffer
	sbuffer:	resb	2; reserve a byte for number-based selection
	fbuffer:	resw	1024; reserve a kilobyte for the file input buffer

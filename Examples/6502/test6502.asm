/**************************************************************************
* 18-Jan-22 (JR)
* Small sample 6502 program for use with RetroAssembler
* from command line:
* Assemble command (with symbol listing file):
*	retroassembler -x -O=BIN test6502.asm > test6502.sym
*	load resultant test6502.bin file into Sim6502 with a load address of $0000
*
* Dissasemble command (with .LST listing file):	
*	retroassembler -d -D=$0000 test6502.bin > test6502.lst 
**************************************************************************/
	.target "6502"				; set target processor for RetroAssembler

	.org $0030					; data segment starting at $0015
Num1	.byte $00,$12,$13,$14,$15,$16		; dataset to work thru backwards so $00 in front
Num2	.byte $12,$13,$14,$15,$16,$00		; dataset to work thru forwards so $00 in back

      .org $0000					; prog segment starting at $0000

Start:	jsr Begin1				; subroutine jump to label Begin1 (backwards)
		jsr Begin2				; subroutine jump to label Begin2 (forwards)
		brk					; end program on return from subroutine				

			
/**************************************************************************
* small routine to sequentially load the accumulator with our data
* stored at location Num1 (working thru Num1 values back to front)
**************************************************************************/
Begin1:	clc					; clear carry flag
		cld					; clear decimal flag
		ldx #$05				; x register = 5 (end of dataset)
cont1:	lda Num1,x				; accumulater = Num,x
	      cmp #$00				; accumulater == 0 ??
		beq fini1				; yes - jump to fini1
		dex					; no - decrement x register
		jmp cont1				; jump to cont1
fini1:	rts					; return from subroutine


/**************************************************************************
* alternate 
* small routine to sequentially load the accumulator with our data
* stored at location Num1 (working thru Num1 values front to back)
* NOTE:  Move $00 to back of Num1 data set first
*	   then change jsr Begin1 to jsr Begin2
**************************************************************************/
Begin2:	clc					; clear carry flag
		cld					; clear decimal flag
		ldx #$00				; x register = 0 (start of dataset)
cont2:	lda Num2,x				; accumulater = Num,x
	      cmp #$00				; at end of dataset if accumulator == 0 ??
		beq fini2				; yes - jump to fini2
		inx					; no - decrement x register
		jmp cont2				; jump to cont2
fini2:	rts					; return from subroutine
		
;/**************************************************************************
;* 27-Jan-22 (JR)
;* 12-Aug-22 (JR)
;*	Tested with Retroassembler & VSC
;*	Modified program to be an entirely self-contained sub-routine
;*				
;*
;* Assemble command (with symbol listing file):
;*   retroassembler -x -O=BIN prime.asm > prime.sym
;*   load resultant prime.bin file into Sim6502 with a load address of $00000
;*   Dissasemble command (with .LST listing file): 
;*   retroassembler -d -D=$0000 prime.bin > prime.lst 
;*
;*   tasm -t65 -b prime.asm
;*   tasm assembler creates a much better .lst file, automatically
;*
;* To use with AppleWin Simulator:
;*   tasm -t65 -b prime.asm prime.bin
;*   convertx prime 0x0800 0x0800
;*
;*
;**************************************************************************/


;**************************************************************************/
; RetroAssembler Specific directives
    .target "6502"              ; set target processor for RetroAssembler
	// the following .format and .setting's allow the generated output
	// file to be directly pasted into the AppleWin emulator
	.format "txt"										; set output file format as ASCII TXT file 
	.setting "OutputTxtAddressFormatFirst","{0:x04}: "	; add colon & space after address
	.setting "OutputTxtAddressFormatNext","{0:x04}: "	; add colon & space after address
;**************************************************************************/


;/**************************************************************************'
;*	Subroutine:	_sub16	
;*			*(call with JSR _sub16)
;*
;*	Purpose:	Subtract 2 16 bit signed/unsigned words and return
;*			a 16 bit signed/unsigned difference
;*
;*	On Entry:	TOP OF STACK
;*			RETURN ADDRESS LSB
;*			RESTUR ADDRESS MSB
;*			SUBTRAHEND LSB
;*			SUBTRAHEND MSB
;*			MINUEND LSB
;*			MINUEND MSB
;*	
;*
;*	On Return:
;*			TOP OF STACK
;*			LSB of difference
;*			MSB of difference
;*			
;**************************************************************************/




				

	.org $0F00			
					; set the .org at whatever the program counter is currently at with .org $
					; this "jams" the subroutine right up against the bottom of the calling pgm
					; alternative -->program starting at .org $0900
					; 6502SIM doesn't like .org $
					; TASM doesn't like .org $ unless it's in an "INCLUDED" .asm file
					; so only use .org $ if using assembled program in AppleWin Sim

	  
	; entry into sub16 subroutine
_sub16:	; save return address
	PLA			
	STA _retadrs16			; save lsb byte of return address
	PLA
	STA _retadrs16+1		; save msb byte of return address

	; gbet subtrahend
	PLA				; get subtrahend LSB 
	STA _subtra
	PLA				; get subtrahend MSB
	sta _subtra+1
	
	; subtrace subtrahend from minuend
	PLA				; get minuend LSB
	SEC
	SBC _subtra
	tay				; save low byte of the difference
	pla				; get minuend MSB
	sbc _subtra+1

	; push the difference
	pha				; push MSB of difference
	TYA
	pha				; push LSB of difference

	; push return address onto stack
	lda _retadrs16+1
	pha
	lda _retadrs16
	pha

_fini:	rts
	  
; data
_subtra:		.word	$0000
_retadrs16:		.word	$0000

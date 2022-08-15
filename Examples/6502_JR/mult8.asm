;/**************************************************************************
;* 27-Jan-22 (JR)
;*	Modified program to be an entirely self-contained sub-routine
;*	Input Parameters:
;* 12-Aug-22 (JR)
;*	Tested with Retroassembler & VSC
;*				
;*
;* Assemble command (with symbol listing file):
;*   retroassembler -x -O=BIN add16.asm > add16.sym
;*   load resultant add16.bin file into Sim6502 with a load address of $00000
;*   Dissasemble command (with .LST listing file): 
;*   retroassembler -d -D=$0000 add1.bin > add1.lst 
;*
;*   tasm -t65 -b add16.asm
;*   tasm assembler creates a much better .lst file, automatically
;*
;* To use with AppleWin Simulator:
;*   tasm -t65 -b add16.asm add16.bin
;*   convertx add16 0x0800 0x0800
;*
;*
;**************************************************************************/


;**************************************************************************/
; RetroAssembler Specific directives
    .target "6502"              ; set target processor for RetroAssembler
	// the following .format and .setting's allow the generated output
	// file to be directly pasted into the AppleWin/MicroM8 emulators
	.format "txt"										; set output file format as ASCII TXT file 
	.setting "OutputTxtAddressFormatFirst","{0:x04}: "	; add colon & space after address
	.setting "OutputTxtAddressFormatNext","{0:x04}: "	; add colon & space after address
;**************************************************************************/


;/**************************************************************************'
;*	Subroutine:	_mult8	
;*			*(call with JSR _mult8)
;*	
;*	Input:		Place the two 8bit numbers to be multiplied on the
;*			6502's stack before calling _mult8
;*
;*	Required:	2 8bit numbers at top of stack prior to call
;*
;*	Output:		16 bit result will be on the top of the 6502 stack
;*			MSB of result at stack pointer
;*			LSB of result at stack pointer - 1
;*			
;**************************************************************************/
   	
				

	.org $0900			
					; set the .org at whatever the program counter is currently at
					; this "jams" the subroutine right up against the bottom of the calling pgm
					; alternative -->program starting at .org $0900
					; 6502SIM doesn't like .org $
					; TASM doesn't like .org $ unless it's in an "INCLUDED" .asm file

	  
	; entry into mult8 subroutine
_mult8:	PLA			
	STA _retadrm8			; save lo byte of return address
	PLA
	STA _retadrm8+1			; save hi byte of return address

	PLA				; get multiplicand 
	STA _factor1
	PLA				; get multiplier
	sta _factor2
	

	LDA #0
	LDX  #$8
	LSR  _factor1
_mult:  BCC  _no_add
	CLC
	ADC  _factor2
_no_add: ROR 
	ROR  _factor1
	DEX
	BNE  _mult
	STA  _factor2
	; done, high result in factor2, low result in factor1	
	  
	; prepare for return from subroutine by placing the following on the stack:
	; STACK -->  	MSB of return address
	;		LSB of return address
	;		MSB of result
	;		LSB of result
	;
	;		after the RTS is executed in this subroutine 
	;		the calling program will be able to immediately
	;		pull the MSB of the result off the stack 1st
	;		pull the LSB of the result off the stack 2nd
	;		once these two PLA's are accomplished the stack
	;		is now restored to the condition it was in before
	;		the call to MULT8
	LDA _factor1
	PHA
	LDA _factor2
	PHA
	LDA _retadrm8+1
	PHA
	LDA _retadrm8
	PHA

_finim8:	
	rts
	  
_factor1: 	.byte $00
_factor2: 	.byte $00
_retadrm8:	.byte $00,$00


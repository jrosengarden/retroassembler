;/**************************************************************************
;* 27-Jan-22 (JR)
;*	Modified program to be an entirely self-contained sub-routine
;* 12-Aug-22 (JR)
;*	Tested with Retroassembler & VSC
;*				
;*
;* Assemble command (with symbol listing file):
;*   retroassembler -x -O=BIN mult16.asm > mult16.sym
;*   load resultant mult16.bin file into Sim6502 with a load address of $00000
;*   Dissasemble command (with .LST listing file): 
;*   retroassembler -d -D=$0000 mult16.bin > mult16.lst 
;*
;*   tasm -t65 -b mult16.asm
;*   tasm assembler creates a much better .lst file, automatically
;*
;* To use with AppleWin Simulator:
;*   tasm -t65 -b mult16.asm mult16.bin
;*   convertx mult16 0x0800 0x0800
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
;*	Subroutine:	_mult16	
;*			*(call with JSR _mult16)
;*
;*	Purpose:	Multiply 2 16bit unsigned numbers
;*			Return 32 bit unsigned result
;*
;*	On Entry:	TOP OF STACK
;*			Lo-Byte of Return Add   (RETADRM16)
;*			Hi-Byte of Return Add   (RETADRM16+1)
;*			Lo-Byte of Multiplier   (MLIER)
;*			Hi-Byte of Multiplier   (MLIER+1)
;*			Lo-Byte of Multiplicand (MCAND)
;*			Hi-Byte of Multiplicand (MCAND+1)
;*	
;*
;*	Output:		TOP OF STACK
;*			LSWord Lo-Byte of result  (MLIER)
;*			LSWord Hi-Byte of result  (MLIER+1)
;*			MSWord Lo-Byte of result  (HIPROD)
;*			MSWord Hi-Byte of result  (HIPROD+1)
;*			
;**************************************************************************/
   	
				

	.org $0E00			
					; set the .org at whatever the program counter is currently at with .org $
					; this "jams" the subroutine right up against the bottom of the calling pgm
					; alternative -->program starting at .org $0900
					; 6502SIM doesn't like .org $
					; TASM doesn't like .org $ unless it's in an "INCLUDED" .asm file
					; so only use .org $ if using assembled program in AppleWin Sim

	  
	; entry into mult16 subroutine
_mult16:	PLA			
	STA _retadrm16			; save lo byte of return address
	PLA
	STA _retadrm16+1		; save hi byte of return address

	
	PLA				; get multiplier LSB (MLIER)
	STA _mlier
	PLA				; get multiplier MSB (MLIER+1)
	sta _mlier+1
	
	PLA				; get multiplicand LSB (MCAND)
	STA _mcand
	PLA				; get multiplicand MSB (MCAND+1)
	STA _mcand+1
	

	LDA #0
	STA _hiprod			; zero hi-word of product
	STA _hiprod+1
	LDX  #17			; number of bits in multiplier plus 1
					; extra loop is to move the last (potential) carry
					; into the product 
	CLC

_mult:	; if next bit = 1 then
	; _hiprod:= _hiprod + _mcand
	ROR _hiprod+1
	ROR _hiprod
	ROR _mlier+1
	ROR _mlier
	BCC  _no_add			; skip around the add routine if next bit of
					; multiplier is 0

	CLC				; next bit is 1 so add multiplicand to product
	LDA _mcand
	ADC _hiprod
	STA _hiprod
	LDA _mcand+1
	ADC _hiprod+1
	STA _hiprod+1			; carry = overflow from add
_no_add:DEX
	BNE  _mult

	; push lo word of product onto stack
	LDA _mlier+1
	PHA
	LDA _mlier
	PHA

	; push hi word of product onto stack
	LDA _hiprod+1
	PHA
	LDA _hiprod
	PHA

	; push return address onto stack
	lda _retadrm16+1
	pha
	lda _retadrm16
	pha

_finim16:	
	rts
	  
_mcand  	 .word $0000			; multiplicand
_mlier  	 .word $0000			; multiplier
_hiprod 	 .word $0000			; MSWord of product
_retadrm16	 .word $0000

	.end
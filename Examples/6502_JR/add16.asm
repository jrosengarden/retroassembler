;/**************************************************************************
;* 26-Jan-22 (JR)
;*	Modified program to be an entirely self-contained sub-routine
;* 10-Aug-22 (JR)
;*	Tested with Retroassembler & VSC
;*  Testing git integration
;*
;*	Input Parameters:
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
;*	Subroutine:	_add16	
;*			*(call with JSR _add16)
;*	
;*	Input:		Address of 1st set of numbers placed on stack
;*				push MSB 1st, push LSB 2nd
;*
;*	Required:	up to 20 16bit numbers at 1st location terminated with $0000
;*			up to 20 16bit numbers at 2nd location terminated with $0000
;*			the 2 sets of 16bit numbers must be contiguous 
;*				example:  set 1 starting at $0850 thru $087A  (20 16bit #'s with final $0000)
;*					  set 2 starting at $087A thru $08A3  (20 16bit #'s with final $0000)
;*			this is due to subroutine calculating starting address of set 2 from set 1's address
;*
;*			must fill any unused positions, in both sets,  with $0000
;*			can add a single set of 20 16bit numbers (terminated with $0000)
;*			with the 2nd set of 20 16bit numbers all set to $0000 (terminated with final $0000)
;*
;*	Output:		32 bit number placed on stack
;*				LSB of Hi-Word pushed 1st
;*				MSB of Lo-Word pushed 2nd
;*				LSB of Lo-Word pushed 3rd
;*				This results in the reverse when popping off the stack
;*					LSB of Lo-Word will get popped 1st - this should be stored 1st at the calling pgm's result+2 
;*					MSB of Lo-Word will get popped 2nde - this should be stored 2nd at the calling pgm's result+1
;*					LSB of Lo-Word will get popped 3rd - this should be stored 3rd at the calling pgm's result
;				This insures the sub-routine follows the little-endian encoding of the 6502
;**************************************************************************/
   
_NUMADD1 .equ $00FB		; location where calling program places start address of first string of 16 bit numbers
_NUMADD2 .equ $00FD		; calculated location of second string's start address (calculated by subroutine)	
				

	.org $0B00			; set the .org at whatever the program counter is currently at
					; this "jams" the subroutine right up against the bottom of the calling pgm
					; alternative -->program starting at .org $0900
					; 6502SIM doesn't like .org $
					; TASM doesn't like .org $ unless it's in an "INCLUDED" .asm file

           
;/**************************************************************************
;*   main 16bit addition subroutine     
;**************************************************************************/
_add16: clc				; clear carry & clear decimal at start of routine
	cld
	pla				; pull 1st value off stack: it will be the LSB of the calling pgm's return address
	sta _retadra16			; store it in _retadr
	pla				; pull 2nd value off stack: it will be the MSB of the calling pgm's return address
	sta _retadra16+1		; store it in _retadr+1
					; above insures following 6502's little endian encoding of addresses

	pla				; pull LSB of NUMADD1 from stack (placed there by calling pgm)
	sta $FB				; store in _NUMADD1
	adc #$2A			; Add #$2A (42D) calculating LSB of NUMADD2
	STA $FD				; store in _NUMADD2
	pla				; pull MSB of NUMADD1 from stack (placed there by calling pgm)
	sta $FC				; store in _NUMADD1
	sta $FE				; store in NUMADD2

	clc				; clear carry
	LDY #$ff			; set yreg to $ff
	LDA #$00
	STA _RSLT			; zero out result bytes
	STA _RSLT+1
	STA _RSLT+2
	STA _RSLT+3
	
_add1:	clc
	LDX #$00			; set/reset xreg to $0 (point to lo byte of temp storage)
	STX _TMP			; clear temp memory location(s)
	STX _TMP+1
	INY				; inc yreg (point to lo bytes of next word)
	JSR _test			; test to see if both of the next words == $0000
	LDA _TMP			
	CMP #$FF			; does lo byte of temp location (LOC3) == $FF ??
	BEQ _finia16			; yes = done

	
	lda (_NUMADD1),y
	adc (_NUMADD2),y
	STA _TMP,x			; store sum of LSBs
	
	inx				; inc xreg (point to hi byte of temp storage)		
	INY				; inc yreg (point to hi byte of current pair being added)
	
	lda (_NUMADD1),y
	adc (_NUMADD2),y			; add the MSBs using carry from the previous calculation
	sta _TMP,x			; store sum of MSBs

	
	LDA _RSLT+2			; add any carry from the MSB addition
	ADC #$00			; to the LSB of the result's hi word
	STA _RSLT+2
	
	dex				; dec xreg (point to lo byte of temp storage)
	CLC				; clear the carry in case it was set
	
	lda _TMP,x			; add LSBs of current result
	adc _RSLT,x			; to LSB of final result
	STA _RSLT,x			; store result in LSB of the result's lo word
	
	INX				; inc xreg (point to hi byte of temp storage)
	
	lda _TMP,x			; add MSB of current result
	adc _RSLT,x			; to MSB of final result
	sta _RSLT,x			; store result in MSB of the result's hi word

	lda _RSLT+2			; add any carry from the result's MSB Addition			
	adc #00				; to the LSB of the result's hi word
	STA _RSLT+2
	
	clc				; clear carry before jumping back for next addition
	jmp _add1	


_finia16:	
	lda _RSLT+2			; get 32 bit results LSB of Hi-Word
	pha				; push it onto the stack
	lda _RSLT+1			; get 32 bit results MSB of Lo-Word
	pha				; push it onto the stack
	lda _RSLT			; get 32 bit results LSB of Lo-Word
	pha				; push it onto the stack
	lda _retadra16+1		; restore the MSB of the callling program's return address
	pha				; onto the stack
	lda _retadra16			; restore the LSB of the calling program's return address
	pha				; onto the stack
					; note: above will insure adhering to 6502's little endian encoding
	RTS                     	; return from subroutine


_test:	LDA (_NUMADD1),y		; test to see if both of the next words == $0000
	CMP #$00			; which indicates the end of the string of numbers
	BNE _test2
	LDA (_NUMADD2),y
	CMP #$00
	BNE _test2
	iny
	LDA (_NUMADD1),y
	CMP #$00
	BNE _setx
	LDA (_NUMADD2),y
	CMP #$00
	BNE _setx
	LDA #$FF			; if they do store $FF in the temp location (LOC3)
	STA _TMP			; for the calling program to test for
_setx:	dey
_test2:	RTS

;data segment starts here
_TMP:		.word $0000,$0000	; temp storage for addition of current pair (32 bit)
_RSLT:		.word $0000,$0000	; final result storage (32 bit)
_retadra16:	.word $0000		; save return address from stack
	
	.END
.END
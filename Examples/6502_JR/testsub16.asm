;/**************************************************************************
;* 26-Jan-22 (JR)
;* 12-Aug-22 (JR)
;*	Tested with Retroassembler & VSC
;*
;*	Sample program to test SUB16.ASM as a self-contained utility program
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


   
	.org $0800		; program starting at $0800


START:  
	; subtract oprnd2 from oprnd1
	lda oprnd1+1
	pha
	lda oprnd1
	pha
	lda oprnd2+1
	pha
	lda oprnd2
	PHA


	jsr _sub16		; subroutine to subtract 1 16bit number from another 16bit number

	; retrieve result from stack
	pla
	tay			
	pla

	brk			; accum = MSB of difference
				; yreg  = LSB of difference

;data
oprnd1	.word	10000
oprnd2	.word	257		; 10000 - 257 = 9743 = $260f

	.include "Jeff_Lib.asm"

	.end
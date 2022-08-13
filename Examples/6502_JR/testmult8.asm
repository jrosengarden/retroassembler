;/**************************************************************************
;* 26-Jan-22 (JR)
;* 12-Aug-22 (JR)
;*	Tested with Retroassembler & VSC
;*
;*	Sample program to test MULT8.ASM as a self-contained utility program
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


   
	.org $0800		; program starting at $0800


START:  clc
	cld
	lda #$10		; 1st 8bit number to multiply (multiplier)
	pha
	lda #$20		; 2nd 8bit number to multiply (multiplicand)
	pha

	jsr _mult8              ; subroutine jump to multiply 2 8bit numbers

	; retrieve result from stack
	pla
	sta result+1		; MSB of result
	pla
	sta result		; LSB of result
	brk

result .word $0000

	.include "Jeff_Lib.asm"

	.end
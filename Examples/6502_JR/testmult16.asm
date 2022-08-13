;/**************************************************************************
;* 26-Jan-22 (JR)
;* 12-Aug-22 (JR)
;*	Tested with Retroassembler & VSC
;*
;*	Sample program to test MULT16.ASM as a self-contained utility program
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
	LDA op1+1
	PHA
	LDA op1
	PHA
	LDA op2+1
	PHA
	LDA op2
	pha

	jsr _mult16              ; subroutine jump to multiply 2 8bit numbers

	; retrieve result from stack
	pla
	sta prod+2		; MSWord LSB
	pla
	sta prod+3		; MSWord MSB
	pla
	sta prod		; LSWord LSB
	pla
	sta prod+1		; LSWord MSB

	brk

prod 	.WORD $0000,$0000
op1	.WORD 14289
op2	.word 41029

	.include "Jeff_Lib.asm"

	.end
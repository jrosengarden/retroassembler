;/**************************************************************************
;* 06-Feb-22 (JR)
;* 10-Aug-22 (JR)
;*	Tested with Retroassembler & VSC
;*
;*	Sample program to test CMP16.ASM as a self-contained utility program
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
	lda oprnd1+1
	pha
	lda oprnd1
	pha
	lda oprnd2+1
	pha
	lda oprnd2
	pha

	jsr _cmp16

	brk

oprnd1:		.word	$A45D	; minuend
oprnd2:		.word	$77E1	; subtrahend

	.include "cmp16.asm"


	.end
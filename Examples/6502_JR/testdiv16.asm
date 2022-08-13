;/**************************************************************************
;* 26-Jan-22 (JR)
;* 12-Aug-22 (JR)
;*	Tested with Retroassembler & VSC
;*
;*	Sample program to test div16.ASM as a self-contained utility program
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


START:
	; signed divide, oprnd1 / oprnd2, store the quotient at quot
	lda oprnd1+1
	pha
	lda oprnd1
	pha
	lda oprnd2+1
	pha
	lda oprnd2
	pha

	jsr _sdiv16		; signed divide

	pla
	sta quot
	pla
	sta quot+1 

	; break here in order to check signed divide results
	nop

	; unsigned divide, oprnd1 / oprnd2, store the quotient at quot
	lda oprnd1+1
	pha
	lda oprnd1
	pha
	lda oprnd2+1
	pha
	lda oprnd2
	pha

	jsr _udiv16		; unsigned divide

	pla
	sta quot
	pla
	sta quot+1 

	; break here in order to check unsigned divide results
	nop

	; signed remainder, oprnd1 / oprnd2, store the remainder at rem
	lda oprnd1+1
	pha
	lda oprnd1
	pha
	lda oprnd2+1
	pha
	lda oprnd2
	pha

	jsr _srem16		; signed division returning remainder

	pla
	sta rem
	pla
	STA rem+1 
	
	; break here in order to check signed remainder results
	nop

	; unsigned remainder, oprnd1 / oprnd2, store the remainder at rem
	lda oprnd1+1
	pha
	lda oprnd1
	pha
	lda oprnd2+1
	pha
	lda oprnd2
	pha

	jsr _urem16		; unsigned division returning remainder

	pla
	sta rem
	pla
	STA rem+1 
	
	; break here in order to check unsigned remainder results
	nop

	brk
	

;data
oprnd1:	.word	256		; dividend (64,513 unsigned)
oprnd2:	.word	2		; divisor
quot:	.byte	0,0		; quotient
rem:	.byte	0,0		; remainder

	.include "div16.asm"

	.end
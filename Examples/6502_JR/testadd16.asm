;/**************************************************************************
;* 26-Jan-22 (JR)
;* 10-Aug-22 (JR)
;*	Tested with Retroassembler & VSC
;*
;*	Sample program to test ADD16.ASM as a self-contained utility program
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
	;lda #$08		; 1st string of 16bit numbers; MSB of address (HARD CODED)
	lda #>STR1		; 1st string of 16sbit numbers; MSB of address (SOFT CODED)
	pha				; push MSB onto stack in prep for call to _ADD16
	;lda #$50		; 1st string of 16bit numbers; LSB of address (HARD CODED)
	LDA #<STR1		; 1st string of 16bit numbers; LSB of address (SOFT CODED)
					; NOTE:  Much better form to use soft coding of variable
					; 		 addresses, instead of hard coding.  This allows
					;		 where the variable resides in memory to be a problem
					; 		 for the assembler to deal with instead of the programmer
	pha				; push LSB onto stack in prep for call to _ADD16

	jsr _add16    	; subroutine jump to add

	pla
	sta ANSR
	pla
	sta ANSR+1
	pla
	STA ANSR+2

        brk        	; end program on return from subroutine

	.org $0850
STR1:    .word $0000,$2222,$9876,$ABCD,$0200,$0300,$1000,$2000,$3000,$0005,$0001,$0000,$0000,$000,$0000,$0000,$0000,$0000,$0000,$000,$0000
STR2:    .word $1234,$3333,$6789,$DCBA,$2000,$3000,$0100,$AAAA,$BBBB,$0006,$0001,$0000,$0000,$000,$0000,$0000,$0000,$0000,$0000,$000,$0000
ANSR:	 .word $0000,$0000

; NOTE:  The answer for all STR1 added to all STR2 = $050C81


;	Comment out one or the other of the following lines.  Including only add16.asm will result in a
;	smaller executable but the preference is to use the entire library.
;	.include "add16.asm"	; $0B00				Include ONLY the sub-routine needed
	.include "Jeff_Lib.asm"	; $0800				Include the entier Jeff_Lib.asm (which includes add16.asm)
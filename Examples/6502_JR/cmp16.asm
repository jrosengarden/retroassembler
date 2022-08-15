;/**************************************************************************
;* 06-Feb-22 (JR)
;* 10-Aug-22 (JR)
;*	Tested with Retroassembler & VSC
;*
;*	Subroutine:	_cmp16	
;*			*(call with JSR _cmp16)
;*
;*	Purpose:	compare 2 16bit signed/unsigned words and
;*			return the C, Z, N flags set or cleared
;*
;*	On Entry:	TOP OF STACK
;*			RETURN ADDRESS LSB
;*			RESTUR ADDRESS MSB
;*			LSB of subtrahend
;*			MSB of subtrahend
;*			LSB of minuend
;*			MSB of minuend
;*	
;*
;*	On Return:			            (WORD1)   (WORD2)
;*			Flags returned are based on minuend - subtrahend
;*			Zero flag 	=1 if subtrahend and minuend are equal
;*			Carry flag	=0 if subtrahend > minuend (unsigned)
;*			Carry flag	=1 if subtrahend < minuend (unsigned)
;*			Negative flag	=1 if subtrahend > minuend (signed)
;*			Negative flag 	=0 if subtrahend < minuend (signed)
;*			NOTE:  	The negative flag is corrected if 2's complement
;*				overflow occurs
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

  	
	.org $0B00			
					; set the .org at whatever the program counter is currently at with .org $
					; this "jams" the subroutine right up against the bottom of the calling pgm
					; alternative -->program starting at .org $0900
					; 6502SIM doesn't like .org $
					; TASM doesn't like .org $ unless it's in an "INCLUDED" .asm file
					; so only use .org $ if using assembled program in AppleWin Sim

	  
	; entry into cmp16 subroutine
_cmp16:
	PLA			
	STA _retadrcmp			; save lsb byte of return address
	PLA
	STA _retadrcmp+1		; save msb byte of return address

	; get subtrahend
	pla
	sta _subtrah
	pla
	sta _subtrah+1

	; get minuend
	pla
	sta _minuend
	pla
	sta _minuend+1

	; restore return address
	lda _retadrcmp+1
	pha
	lda _retadrcmp
	pha

	; main cmp16 routine starts here
	lda _minuend
	cmp _subtrah			; compare low bytes
	beq _equal			; branch if they are equal

	; low bytes are not equal - compare high bytes
	lda _minuend+1
	sbc _subtrah+1			; compare high bytes
	ora #$01			; make z = 0, since low bytes are not equal
	bvs _ovflow			; must handle overflow for signed arithmetic
	rts				; exit

	; low bytes are equal - compare high bytes
_equal:
	lda _minuend+1			
	sbc _subtrah+1			; upper bytes
	bvs _ovflow			; must handle overflow for signed arithmetic
	rts				; return with flags set

	; overflow with signed arithmetic so complement the negative flag
	; do not change the carry flag and make the zero flag equal 0
	; complement negative flag by exclusive-oring 80H and accumulator
_ovflow:
	eor #$80			; complemnt negative flag
	ora #$01			; if overflow then the words are not equal (z=0)
					; carry unchanged
	rts

;data
_minuend:	.word	$0000
_subtrah:	.word	$0000
_retadrcmp:	.word 	$0000

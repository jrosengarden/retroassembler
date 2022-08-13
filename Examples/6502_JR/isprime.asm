;/**************************************************************************
;* 06-Feb-22 (JR)
;* 07-Feb-22 (JR)
;* 08-Feb-22 (JR)
;* 10-Aug-22 (JR)
;*	Tested with Retroassembler & VSC
;*
;*	Subroutine:	_isprime	
;*			*(call with JSR _isprime)
;*
;*	Purpose:	test if number is prime
;*				Current limit is 0 thru 40,000 ($9C40)
;*
;*	On Entry:	TOP OF STACK
;*			RETURN ADDRESS LSB
;*			RESTUR ADDRESS MSB
;*			PRIME LSB (number to test for prime)
;*			PRIME MSB
;*	
;*
;*	On Return:
;*			Xreg = 1 --> # Is Prime
;*			Xreg = 0 --> # Is Not Prime
;*
;*			NOTE:  	calling program needs to save Xreg on entry and restore
;*				on return from _isprime
;*
;*	NOTE ON LIMIT:	This subroutine is limited to testing a number for prime
;*			as long as the number is between 0 - 40000
;*			The reason for this limit is that as iloop gets squared
;*			in the routine (cnt3:) the value of iloop can exceed
;*			a 32 bit result which, then, screws up the rest of
;*			the testing process.  Didn't want to have to deal
;*			with a 64 bit value hence the limit of 40,000.
;*			
;**************************************************************************/



;/**************************************************************************'
;*// pseudo-code for algorithim used
;* function isisprime(n) {
;*   if n ≤ 1 return FALSE
;*    else if n ≤ 3 return TRUE
;*    else if (n mod 2 = 0) or (n mod 3 = 0) return FALSE
;*   i = 5
;*    while (i*i ≤ n) {			while (minuend <= subtrahend)
;*      if (n mod i = 0) or (n mod (i + 2) = 0) return FALSE
;*      i = i + 1
;*    }
;*    return TRUE
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

	.org $0900			
					; set the .org at whatever the program counter is currently at with .org $
					; this "jams" the subroutine right up against the bottom of the calling pgm
					; alternative -->program starting at .org $0900
					; 6502SIM doesn't like .org $
					; TASM doesn't like .org $ unless it's in an "INCLUDED" .asm file
					; so only use .org $ if using assembled program in AppleWin Sim

	  
	; entry into isprime subroutine
_isprime:

		
	PLA			
	STA _retadr			; save lsb byte of return address
	PLA
	STA _retadr+1			; save msb byte of return address

	
	pla				; get _prime candidate LSB
	STA _prime
	pla				; get _prime candidate MSB
	sta _prime+1

	CLC
	ldx #$0				; set default return of NOT PRIME
	LDA #$02
	STA _tmp1			; variable used for divide by 2 and 3
	LDA #$05
	sta _iloop			; incrementer used for tstAA and tstBB

	;main isprime subroutine starts here

	; is MSB of _prime == 0 ??
	lda _prime+1
	cmp #$00
	bne cnt1			; MSB of _prime <> 0 so skip checking LSB for 0, 1, 2, 3

	; is LSB of _prime == 0 ??
tst00:	
	lda _prime
	cmp #$00
	bne tst01			; MSB + LSB == 0 so not prime
	jmp finix			; Xreg already set for not prime so simply exit

	; is LSB of _prime == 1 ??
tst01:
	cmp #$01
	bne tst02			; <> 1 so jump to next test
	jmp finix			; Xreg already set for not prime so simply exit

	; is LSB of _prime == 2 ??
tst02:
	cmp #$02
	bne tst03			; <> 2 so jump to next test
	ldx #$01			; == 2 so set Xreg to indicate prime
	jmp finix			; and exit

	; is LSB of _prime == 3 ??
tst03:
	cmp #$03
	bne cnt1			; <> 3 so jump to next test
	ldx #$01			; == 3 so set Xreg to indicate prime
	jmp finix			; and exit


	; _prime not 0, 1, 2, 3
cnt1:	; test for divison by 2 leaves no remainder		
	; _tmp1 preloaded with 2 at start of subroutine (above)

comm1:	; common code to test division by 2 and 3 (using _prime / _tmp)
	; push dividend onto stack
	lda _prime+1			; push _prime MSB onto stack
	pha
	lda _prime			; push _prime LSB onto stack
	PHA
	
	; push divisor onto stack
	lda #$00			; push MSB of _tmp1 onto stack
					; note: divisor is 8bit number 
					; so the MSB will always be $00
	pha
	lda _tmp1			; push _tmp1 LSB onto stack
	pha

	; save x/y registers
	stx _tmpx
	sty _tmpy

	jsr _urem16			; call _urem16 routine in _div16 module
					; which will return remainder on stack
	; restore x/y registers
	ldx _tmpx
	ldy _tmpy

	; retrieve remainder off stack and test
	pla				
	cmp #$00			; LSB of remainder == 0 ??
	BEQ cnt2a			; yes - so now go test the MSB for a remainder
	PLA				; no - means _prime is not divisible by 2 (or 3)
					; so pull last byte of remainder (LSB) off stack
	jmp cnt2			; and move onto next test since there is no need
					; to test the LSB now
	
cnt2a:
	PLA
	cmp #$00			; MSB of remainder == 0 ??
	bne cnt2			; no - remainder exists so not divisible by current _tmp1 (2 or 3)

	jmp finix			; yes: jump over remainder of tests since current number
					; is evenly divisible by current value of _tmp1 (2 or 3) meaining it's not prime
					; Xreg preset with 0 so just exit
	

cnt2:	; test for division by 3 leaves no remainder
	INC _tmp1			; inc _tmp1 from 2 to 3
	lda _tmp1			
	cmp #$04			; _tmp1 == 4 ??
	beq cnt3			; yes - done with divisible by 2 and 3 testing
	jmp comm1			; no - continue with next test (divisible by 3)

	; passed not divisible by 2 and 3 (both divisons left remainder) 
	; so continue with next test(s)
cnt3:
	; calc square of current iloop value
	; note:  this is the reason for limiting the routine to testing
	; 	 numbers <= 40,000 
	lda _iloop
	pha
	lda _iloop+1
	pha
	lda _iloop
	pha
	lda _iloop+1
	pha
	
	; save x/y registers
	stx _tmpx
	sty _tmpy

	; calculate _iloop squared
	jsr _mult16

	; restore x/y registers
	ldy _tmpy
	ldx _tmpx

	; load LSWord of result into _test1
	pla
	sta _test1			; lsb of multiplication result
	pla
	sta _test1+1			; msb of multiplication result

	;  pop the MSWord of result off stack
	;  not using MSWord of result as our subroutine
	;  can only (currently) find out if a number
	;  <= 40000 ($9C40)
	pla
	pla

	; prepare to compare _iloop squared and _prime
	; using _cmp16 subroutine which uses subtraction
	; as follows:  _iloop squared - _prime
	lda _test1+1			; MSB of minuend (_iloop squared)
	pha
	lda _test1			; LSB of minuend
	pha
	lda _prime+1			; MSB of subtrahend (_prime)
	pha
	lda _prime			; LSB of subtrahend
	pha

	; save x/y registers
	stx _tmpx
	sty _tmpy

	jsr _cmp16			; compare by doing subtraction 
					;    (minuend - subtrahend) --> (_iloop squared - _prime)
					; then _cmp16 subroutine will set the Z, N, C flags accordingly
					; NOTE:  Can't restore the x/y reg
					; upon returning from _cmp16 since doing so can
					; possibly change the z flag
					; so check the results of the _cmp16
					; subroutine 1st before restoring x/y regs

	; iloop * iloop <= _prime  --> minuend <= subtrahend
	; for the above to be true z=1 or c=0
	; NOTE:  see cmp16.asm for explanation of how Z,N,C flags are set
	beq cnt4			; z=1 first test 
	BCC cnt4			; c=0 second test

	; iloop * iloop > _prime --> minuend > subtrahend
	; for the above to be true z=0 and c=1
	; but if code gets here no need to test since
	; its given that z=0 and c=1 so the number is prime.
	; simply set Xreg and exit
	ldx #$01			; indicate _prime is a prime number
	jmp finix

cnt4:	; iloop squared is less then or equal to _prime 
	
	; now we can (and need to) restore the
	; x/y registers before moving on
	ldx _tmpx
	ldy _tmpy

tstAA:	; 1st test is: if (_prime mod _iloop = 0)
	; if no remainder then not prime so return
	; push dividend onto stack
	lda _prime+1			; push _prime MSB onto stack
	pha
	lda _prime			; push _prime LSB onto stack
	PHA
	
	; push divisor onto stack
	lda _iloop+1			; push _tmp1 MSB onto stack
	pha
	lda _iloop			; push _tmp1 LSB onto stack
	pha

	; save x/y registers
	stx _tmpx
	sty _tmpy

	jsr _urem16			; call _urem16 routine in _div16 module
					; which will return remainder on stack
	; restore x/y registers
	ldx _tmpx
	ldy _tmpy

	; retrieve remainder off stack and test
	pla				
	cmp #$00			; LSB of remainder == 0 ??
	beq tstBBA 			; yes: so go test MSB for remainder
	PLA				; no:  LSB has remainder so no need to test
					;      MSB so pull it off stack and then
	jmp tstBB			; move on to next test
	
tstBBA:
	PLA
	cmp #$00			; MSB of remainder == 0 ??
	bne tstBB			; no - remainder exists so not divisible by current test #
					; move on to next test

	; evenly divisible by current
	; value of _iloop so _prime is
	; not prime so exit
	jmp finix			; jump over remainder of tests since current number
					; is evenly divisible by current test # meaining it's not prime
					; Xreg preset with 0 so just exit
	

	; 2nd test (only if 1st test had remainder)
	; 2nd test is: if (_prime mod (_iloop + 2) = 0
	; if no remainder then not prime so return
	; need to adjust _iloop to _iloop + 2 for this test
	; so place adjusted value into _tmpiloop
tstBB:	clc
	; add 2 to _iloop and save in _tmpiloop
	LDA _iloop
	adc #$02
	sta _tmpiloop
	lda _iloop+1
	adc #$00
	sta _tmpiloop+1

	; push dividend onto stack
	lda _prime+1			; push _prime MSB onto stack
	pha
	lda _prime			; push _prime LSB onto stack
	PHA
	
	; push divisor onto stack
	lda _tmpiloop+1			; push _tmp1 MSB onto stack
	pha
	lda _tmpiloop			; push _tmp1 LSB onto stack
	pha

	; save x/y registers
	stx _tmpx
	sty _tmpy

	jsr _urem16			; call _urem16 routine in _div16 module
					; which will return remainder on stack
	; restore x/y registers
	ldx _tmpx
	ldy _tmpy

	; retrieve remainder off stack and test
	pla				
	cmp #$00			; LSB of remainder == 0 ??
	beq tstagna			; yes - remainder exists so not divisible by current test #
	PLA				; so no need to test MSB so pull it of stack and then
	jmp tstagn			; move on
tstagna:
	PLA
	cmp #$00			; MSB of remainder == 0 ??
	bne tstagn			; no - remainder exists so not divisible by current test #
					; move on

	; evenly divisible by current
	; value of _iloop + 2 so _prime is
	; not prime so exit
	jmp finix			; jump over remainder of tests since current number
					; is evenly divisible by current test # meaining it's not prime
					; Xreg preset with 0 so just exit


	
	; only arrived here if both above tests returned remainder
	; so increment _iloop and jmp back to cnt3 for next test sequence
tstagn:
	jsr incby1			; increment _iloop by 1 for next pass thru 
	jmp cnt3


finix:	
	; push return address onto stack and return
	lda _retadr+1
	pha
	lda _retadr
	pha
	rts


;/**************************************************************************'
;* tiny routine used as internal subroutine (JSR/RTS)
;* use to increment the 16bit variable _iloop
;* NOTE:  Too small to turn into external subroutine
;**************************************************************************/
incby1: ; inc 16bit _ploop value
	; internal subroutine
	inc _iloop
	lda #$0
	cmp _iloop
	beq incby1a			
	rts
incby1a:
	inc _iloop+1
	rts


;/**************************************************************************'
;* Data Segment
;**************************************************************************/
;data 
_prime:		.word	$0000					; 16bit number being tested for prime
_test1:		.word	$0000					; temp storage for 0,1,2,3 testing
											; and for mult16 result (cnt3 label)
_tmp1:		.byte	$00						; temp incrementer for div by 2 & 3
_tmpx:   	.byte 	$00						; temp cubbyhole for xreg
_tmpy:  	.byte 	$00						; temp cubbyhole for yreg
_iloop:		.word	$0000					; i variable for looping
_tmpiloop:	.word	$0000					; temp storage for _iloop + 2 during tstBB
_retadr:	.word 	$0000					; return address to calling program

;/**************************************************************************'
;* Code Includes Segment
;**************************************************************************/
	; includes go here and make sure their .org's are set as indicated.
	; which will insure no overlap amongst included subroutines
	.include "Jeff_Lib.asm"	;various modules & addresses

	.end
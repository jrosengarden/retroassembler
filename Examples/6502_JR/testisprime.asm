;/**************************************************************************
;* 06-Feb-22 (JR)
;* 10-Aug-22 (JR)
;*	Tested with Retroassembler & VSC
;*
;*	Sample program to test ISPRIME.ASM as a self-contained utility program
;**************************************************************************/

;**************************************************************************/
; RetroAssembler Specific directives
    .target "6502"              ; set target processor for RetroAssembler
	// the following .format and .setting's allow the generated output
	// file to be directly pasted into the AppleWin/MicroM8 emulators
	.format "bin"										; set output file format as ASCII TXT file 
	.setting "OutputTxtAddressFormatFirst","{0:x04}: "	; add colon & space after address
	.setting "OutputTxtAddressFormatNext","{0:x04}: "	; add colon & space after address
;**************************************************************************/
   
	.org $0800		; program starting at $0800

	LDX #$00
	LDY #$00
	STA IO_AREA+0		; clear in/out window on 6502 Sim
	
	; load next number to test for prime
start:  clc
	cld
	lda prime+1		
	pha
	lda prime
	pha

	; save x/y registers before jumping into _isprime
	STX tmpx
	STY tmpy

	JSR _isprime

	; Xreg will hold result of _isprime test.  X=1 means prime.  X=0 means not prime
	; check Xreg result 1st, before reloading x/y registers

	cpx #$00		; is Xreg = 0 ??				
	bne good1		; no: means this number WAS a prime

	; restore x/y registers
	ldx tmpx
	ldy tmpy

	jmp contx		; yes: means this number WAS NOT prime
				; so jump to contx to get next number to test for

good1:	; restore x/y registers
	LDX tmpx
	LDY tmpy
	
	; display current number on 6502 Sim In/Out window since it's prime
	LDA prime+1
	STA IO_AREA+3		; writıe MSB to in/out window (6502 Sim)
	LDA prime
	STA IO_AREA+3		; write LSB to in/out window (6502 Sim)
	LDA ' '
	STA IO_AREA+1		; write space between numbers (6502 Sim)
	
	; tracking quantity of prime #'s found so increment qty
	jsr jincby1		
	
	;INX
	;CPX #$00
	;BNE contx
	;iny
	

	
contx:
	; increment _prime to get next candidate for prime testing
	INC prime		; inc LSB 1st
	LDA #$00		; did LSB roll over to $00 ??
	CMP prime
	BNE fintst		; no so jump to fintst
	INC prime+1		; yes so now inc MSB
	LDA #$00		; did MSB roll over to $00 ??
	CMP prime+1
	BNE fintst		; no so jump to finst
	
fintst:
	; insure _prime is still <= 40000 (our subroutine limit)
	LDA prime+1
	CMP #$9C
	BNE start		; NO: MSB <= $9C so no need to test LSB
				; jump back to start for testing of
				; next candidate for prime
	LDA prime		; YES: 
	CMP #$40		; is LSB <= $40 ??
	BNE start		; YES
				; jump back to start for testing of
				; next candidate for prime
	
	; ending message to 6502 Sim's In/Out window
	LDA #10
	STA IO_AREA+1			; send line feed (Decimal 10)
	LDA #13				; send carriage return (Decimal 13)
	STA IO_AREA+1
	LDX #$1				; start msg offset by 1 for padding char of 30
	LDY #$0				; counter for string length
msgout:
	LDA msg,x			; put out each character of msg
	STA IO_AREA+2
	INX
	INY				; point to next char of msg				
	CPY #48				; are we at end of output msg ??
	beq numout			; yes - so now output # of primes found
	JMP msgout			; no - continue outputting msg
numout:
	LDA qty+1			; get MSB of count of prime #'s
	STA IO_AREA+3			; output to In/Out window
	LDA qty				; get LSB of count of prime #'s
	sta IO_AREA+3			; output to In/Out window
	
		

	
exit1:  
	BRK
	
	
;/**************************************************************************'
;* tiny routine used as internal subroutine (JSR/RTS)
;* use to increment the 16bit variable qty
;* NOTE:  Too small to turn into external subroutine
;**************************************************************************/
jincby1: 
	; inc 16bit _ploop value
	; internal subroutine
	inc qty
	lda #$0
	cmp qty
	beq jincby1a			
	rts
jincby1a:
	inc qty+1
	rts
	
	
;data segment
prime:		.word $0000			; next number to test for prime
tmpx:		.BYTE $00			; tmp variable to hold XReg
tmpy:		.BYTE $00			; tmp variable to hold YReg
qty:		.WORD $0000			; counter for # of primes found

; comment out the following if assembling with TASM32
;msg:		.string "Quantity of Prime Numbers between 0 and 40000: $"

; comment out the following if assembling with SIM6502
IO_AREA:	.equ $E000
msg:		.text "Quantity of Prime Numbers between 0 and 40000: $"
		


	.include "isprime.asm"	; $0900


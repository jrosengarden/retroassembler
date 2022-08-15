;/**************************************************************************
;* 27-Jan-22 (JR)
;*	Modified program to be an entirely self-contained sub-routine
;* 12-Aug-22 (JR)
;*	Tested with Retroassembler & VSC
;*				
;*
;* Assemble command (with symbol listing file):
;*   retroassembler -x -O=BIN div16.asm > div16.sym
;*   load resultant div16.bin file into Sim6502 with a load address of $00000
;*   Dissasemble command (with .LST listing file): 
;*   retroassembler -d -D=$0000 div16.bin > div16.lst 
;*
;*   tasm -t65 -b div16.asm
;*   tasm assembler creates a much better .lst file, automatically
;*
;* To use with AppleWin Simulator:
;*   tasm -t65 -b div16.asm div16.bin
;*   convertx div16 0x0800 0x0800
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
;*	Subroutine:	_div16	
;*			*(call with JSR _div16)
;*
;*	Purpose:	SDIV16
;*				Divided 2 signed 16 bit words and return
;*				a 16 bit signed quotient
;*			UDIV16
;*				Divide 2 unsigned 16 bit words and return
;*				a 16 bit unsigned quotient
;*			SREM16
;*				Divide 2 signed 16 bit words and return
;*				a 16 bit signed remainder
;*			UREM16
;*				Divde 2 unsiged 16 bit words and return
;*				a 16 bit unsigned remainder
;*
;*	On Entry:	TOP OF STACK
;*				LSB of return address
;*				MSB of return address
;*				LSB of divisor
;*				MSB of divisor
;*				LSB of dividend
;*				MSB of dividend
;*
;*	On Return:
;*			TOP OF STACK
;*				LSB of result
;*				MSB of result
;*			If no errors then 
;*				Carry := 0
;*			else
;*				divide by zero error
;*					    Carry := 1
;*					 quotient := 0
;*					remainder := 0
;*			
;*			
;**************************************************************************/
   	
				

	.org $0C00			
					; set the .org at whatever the program counter is currently at with .org $
					; this "jams" the subroutine right up against the bottom of the calling pgm
					; alternative -->program starting at .org $0900
					; 6502SIM doesn't like .org $
					; TASM doesn't like .org $ unless it's in an "INCLUDED" .asm file
					; so only use .org $ if using assembled program in AppleWin Sim

; unsigned division  
_udiv16:
	LDA #0				; result is 	quotient (index = 0)
	BEQ _udivmd

; unsigned remainder
_urem16:
	LDA #2				; result is remainder (index = 2)

_udivmd:
	sta _rsltix			; result index (0 for quotient)
					;              (2 for remainder)


	; save return address
	PLA			
	STA _retadrd16			; save lsb byte of return address
	PLA
	STA _retadrd16+1		; save msb byte of return address

	; get divisor
	PLA				; get LSB of divisor 
	STA _dvsor
	PLA				; get MSB of divisor				
	sta _dvsor+1
	
	; get dividend
	PLA				; get LSB of dividend
	STA _dvend
	PLA				; get MSB of divident
	STA _dvend+1
	
	; perform division
	jsr _udiv
	bcc _divok			; branch if no errors
_diver: jmp _erexit
_divok: jmp _okexit

;
; signed division
_sdiv16:
	lda #0				; result is quotient (index = 0)
	beq _sdivmd

; signed remainder
_srem16:
	lda #2				; result is remainder (index = 2)
	bne _sdivmd

_sdivmd:
	sta _rsltix			; result index (0 for quotient)
					;              (2 for remainder)

	; save return address
	PLA			
	STA _retadrd16			; save lsb byte of return address
	PLA
	STA _retadrd16+1		; save msb byte of return address

	; get divisor
	PLA				; get LSB of divisor 
	STA _dvsor
	PLA				; get MSB of divisor				
	sta _dvsor+1
	
	; get dividend
	PLA				; get LSB of dividend
	STA _dvend
	PLA				; get MSB of divident
	STA _dvend+1

	; determine sign of quotient by performing an exclusive OR of the
	; high bytes.  If the signs are the same then bit 7 will be 0 and the
	; quotient is positive.  If the signs are different then the quotient
	; is negative
	lda _dvend+1
	eor _dvsor+1
	sta _squot

	; sign of remainder is the sign of the divident
	lda _dvend+1
	sta _srem

	; take the absolute value of the divisor
	lda _dvsor+1
	bpl _chkde			; branch if already positive

	lda #0				; subtract divisor from zero
	sec
	sbc _dvsor
	sta _dvsor
	lda #0
	sbc _dvsor+1
	sta _dvsor+1

	; take the absolute value of the divident 
_chkde:
	lda _dvend+1
	bpl _dodiv			; branch if dividend is positive
	lda #0				; subtract divident from zero
	sec
	sbc _dvend
	sta _dvend
	lda #0
	sbc _dvend+1
	sta _dvend+1

	; divide absolute values
_dodiv: jsr _udiv
	bcs _erexit			; exit if divide by zero

	; negate quotient if it is negative
	lda _squot
	bpl _dorem			; branch if quotient is positive
	lda #0				; subtract quotient from zero
	sec
	sbc _dvend
	sta _dvend
	lda #0
	sbc _dvend+1
	sta _dvend+1

_dorem:
	; negate remainder if it is negative
	lda _srem
	bpl _okexit			; branch if remainder is positive
	lda #0
	sec
	sbc _dvend+2
	sta _dvend+2
	lda #0
	sbc _dvend+3
	sta _dvend+3
	jmp _okexit

	; error exit (carry = 1, results set to 0)
_erexit:
	lda #0
	sta _dvend
	sta _dvend+1			; quotient := 0
	sta _dvend+2
	sta _dvend+3
	sec				; remainder : =
					; carry = 1 if error

	bcs _dvexit

	; good exit (carry = 0
_okexit:
	clc				; carry = 0, no errors

_dvexit:
	; push result
	ldx _rsltix			; get index to result (0=quotient, 2=remainder)
	lda _dvend+1,x
	pha
	lda _dvend,x
	pha

	; restore return address
	lda _retadrd16+1
	pha
	lda _retadrd16
	pha
	
_finid16:	
	rts

;/**************************************************************************
;* Routine:	_udiv
;* Purpose:	Divide a 16 bit divident by a 16 bit divisor
;* Entry:	_dvend = divident
;*		_dvsor = divisor
;* Exit:	_dvend = quotient
;*		_dvend+2 = remainder
;* Registers Used: all
;*
;**************************************************************************/

_udiv:
	; Zero upper word of divident this will be dalled dividend[1] below
	lda #0
	sta _dvend+2
	sta _dvend+3

	; first check for division by zero
	lda _dvsor
	ora _dvsor+1
	bne _okudiv			; branch if divisor is not zero
	sec				; else error exit
	rts

	; perform the division by trial subtractions
_okudiv:
	ldx #16				; loop thru 16 bits
_divlp:
	rol _dvend			; shift the carry into bit 0 of divident
	rol _dvend+1			; which will be the quotient
	rol _dvend+2			; and shift divident at the same time
	rol _dvend+3

	;
	; check if dividend[1] is less than divisor
_chklt:
	sec
	lda _dvend+2
	sbc _dvsor
	tay				; save low byte in y reg
	lda _dvend+3
	sbc _dvsor+1			; subtract high bytes with result in accum
	bcc _deccnt			; branch if dividend < divisor and carry
	sty _dvend+2			; else
	sta _dvend+3			;  dividend[1] := dividend[1] - divisor

_deccnt:
	dex
	bne _divlp

	rol _dvend			; shift in the last carry for the quotient
	rol _dvend+1
	clc				; no errors, clear carry
	rts


	  
; data
_dvsor:	 	.byte 0,0			; divisor
_dvend:		.byte 0,0			; dividend[0] and quotient
	 	.byte 0,0			; dividend[1] and remainder
_retadrd16: 	.byte 0,0			; return address
_squot:	 	.byte 0			; sign of quotient
_srem:	 	.byte 0			; sign of remainder
_rsltix: 	.byte 0			; index to the result
					;	0 = quotient
					;	2 = remainder

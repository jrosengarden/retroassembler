;* 12-Aug-22 (JR)
;*	Tested with Retroassembler & VSC
;*
;/**************Jeff_Lib.asm**************
;* modules included in this library w/load address:
;* add16.asm	$0B00		add 2 strings of 16 bit nums
;* cmp16.asm	$0BB8		compare 2 16 bit numbers
;* div16.asm	$0C00		divide 2 16 bit numbers (signed/unsigned)
;* mult16.asm	$0E00		multiply 2 16 bit numbers
;* sub16.asm	$0F00		subtract 2 16 bit numbers
;* mult8.asm	$0F30		multiply 2 8 bit numbers
;
; NOTE:  Any of the 16 bit routines can be used with 8 bit numbers
;		 as long as they are extended to be 16 bit numbers
;		 (example:  instead of $18 use $0018)
;
; NOTE:	Full details of each function can be found at the top
;		of the function.
;
;*  All functions were tested (assembled & debugged) with test
;*	code specific to each function (last tested 08/12/22);
;*	add16.asm 		tested with 	testadd16.asm
;*	calcfib.asm		tested with		testcalcfib.asm
;*	cmp16.asm		tested wih		testcmp16.asm
;*	div16.asm		tested with		testdiv16.asm
;*	isprime.asm		tested with		testisprime.asm
;*	mult8.asm		tested with		testmult8.asm
;*	mult16.asm		tested with		testmult16.asm
;*	sub16.asm		tested with		testsub16.asm
;*
;*	NOTE:  The test programs show how to properly use/call
;*		   each function in the library
;
;
; NOTE:  Any programs using this library should load
; 		 at an address that will have an ending address
;		 above $0B00 after being assembled.
;		 Suggested Starting Addresses:
;		 $0100, $0200, $0300, $0400, $0500, $0600, $0700, $0800, $0900
;		
;		 Alternatively the calling program should load
;		 at $1000 or above.  Doing so will eliminate the need
;		 to worry about "bumping" into this libraries code
;		 (as long as your code doesn't stretch into ROM)
;
; NOTE:	Jeff_Library.asm runs from $0B00 to $0F70
;
; NOTE:	Zero page memory locations used in Jeff_Library.asm:
;		$FB, $FC, $FD, $FE, $FF
;
; NOTE:	All functions in this library make heavy use of the
;		stack for retrieval of calling program variables and
;		return addresses as well as placing results on the stack
;		to allow the calling program easy retrieval.
;		WARNING:  CAREFUL MANIPULATING THE STACK/STACK POINTER
;				  OR YOU WILL "BREAK" FUNCTIONS IN THIS LIBRARY




;**************************************************************************/
; RetroAssembler Specific directives
    .target "6502"              ; set target processor for RetroAssembler
	// the following .format and .setting's allow the generated output
	// file to be directly pasted into the AppleWin/MicroM8 emulators
	.format "txt"										; set output file format as ASCII TXT file 
	.setting "OutputTxtAddressFormatFirst","{0:x04}: "	; add colon & space after address
	.setting "OutputTxtAddressFormatNext","{0:x04}: "	; add colon & space after address
;**************************************************************************/


;/**************************************************************************
;* 27-Jan-22 (JR)
;*	Modified program to be an entirely self-contained sub-routine
;* 10-Aug-22 (JR)
;*	Moved all subroutines into a single library file since
;*  Retroassembler v2022.1 will only allow a single .include
;*	directive (any add'l .include's are ignored)
;*				
;*
;* Assemble command (with symbol listing file):
;*   retroassembler -x -O=BIN div16.asm > div16.sym (command line)
;*		CTRL-SHIFT-B (from inside VSC)
;*   load resultant bin file into Sim6502 with a load address of $00000
;*	 retroassembler -x -O=TXT div16.asm > div16.sym (command line)
;*	 copy/paste resultant txt file into MicroM8
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

;/**************************************************************************'
;*	Subroutine:	_add16	
;*			*(call with JSR _add16)
;*	
;*	Input:		Address of 1st set of numbers placed on stack
;*				push MSB 1st, push LSB 2nd
;*
;*	Required:	up to 20 16bit numbers at 1st location terminated with $0000
;*			up to 20 16bit numbers at 2nd location terminated with $0000
;*			the 2 sets of 16bit numbers must be contiguous 
;*				example:  set 1 starting at $0850 thru $087A  (20 16bit #'s with final $0000)
;*					  set 2 starting at $087A thru $08A3  (20 16bit #'s with final $0000)
;*			this is due to subroutine calculating starting address of set 2 from set 1's address
;*
;*			must fill any unused positions, in both sets,  with $0000
;*			can add a single set of 20 16bit numbers (terminated with $0000)
;*			with the 2nd set of 20 16bit numbers all set to $0000 (terminated with final $0000)
;*
;*	Output:		32 bit number placed on stack
;*				LSB of Hi-Word pushed 1st
;*				MSB of Lo-Word pushed 2nd
;*				LSB of Lo-Word pushed 3rd
;*				This results in the reverse when popping off the stack
;*					LSB of Lo-Word will get popped 1st - this should be stored 1st at the calling pgm's result+2 
;*					MSB of Lo-Word will get popped 2nde - this should be stored 2nd at the calling pgm's result+1
;*					LSB of Lo-Word will get popped 3rd - this should be stored 3rd at the calling pgm's result
;				This insures the sub-routine follows the little-endian encoding of the 6502
;**************************************************************************/
   
_NUMADD1 .equ $00FB		; location where calling program places start address of first string of 16 bit numbers
_NUMADD2 .equ $00FD		; calculated location of second string's start address (calculated by subroutine)	
				

	.org $0B00			; set the .org at whatever the program counter is currently at
					; this "jams" the subroutine right up against the bottom of the calling pgm
					; alternative -->program starting at .org $0900
					; 6502SIM doesn't like .org $
					; TASM doesn't like .org $ unless it's in an "INCLUDED" .asm file

           
;/**************************************************************************
;*   main 16bit addition subroutine     
;**************************************************************************/
_add16: clc				; clear carry & clear decimal at start of routine
	cld
	pla				; pull 1st value off stack: it will be the LSB of the calling pgm's return address
	sta _retadra16			; store it in _retadr
	pla				; pull 2nd value off stack: it will be the MSB of the calling pgm's return address
	sta _retadra16+1		; store it in _retadr+1
					; above insures following 6502's little endian encoding of addresses

	pla				; pull LSB of NUMADD1 from stack (placed there by calling pgm)
	sta $FB				; store in _NUMADD1
	adc #$2A			; Add #$2A (42D) calculating LSB of NUMADD2
	STA $FD				; store in _NUMADD2
	pla				; pull MSB of NUMADD1 from stack (placed there by calling pgm)
	sta $FC				; store in _NUMADD1
	sta $FE				; store in NUMADD2

	clc				; clear carry
	LDY #$ff			; set yreg to $ff
	LDA #$00
	STA _RSLT			; zero out result bytes
	STA _RSLT+1
	STA _RSLT+2
	STA _RSLT+3
	
_add1:	clc
	LDX #$00			; set/reset xreg to $0 (point to lo byte of temp storage)
	STX _TMP			; clear temp memory location(s)
	STX _TMP+1
	INY				; inc yreg (point to lo bytes of next word)
	JSR _test			; test to see if both of the next words == $0000
	LDA _TMP			
	CMP #$FF			; does lo byte of temp location (LOC3) == $FF ??
	BEQ _finia16			; yes = done

	
	lda (_NUMADD1),y
	adc (_NUMADD2),y
	STA _TMP,x			; store sum of LSBs
	
	inx				; inc xreg (point to hi byte of temp storage)		
	INY				; inc yreg (point to hi byte of current pair being added)
	
	lda (_NUMADD1),y
	adc (_NUMADD2),y			; add the MSBs using carry from the previous calculation
	sta _TMP,x			; store sum of MSBs

	
	LDA _RSLT+2			; add any carry from the MSB addition
	ADC #$00			; to the LSB of the result's hi word
	STA _RSLT+2
	
	dex				; dec xreg (point to lo byte of temp storage)
	CLC				; clear the carry in case it was set
	
	lda _TMP,x			; add LSBs of current result
	adc _RSLT,x			; to LSB of final result
	STA _RSLT,x			; store result in LSB of the result's lo word
	
	INX				; inc xreg (point to hi byte of temp storage)
	
	lda _TMP,x			; add MSB of current result
	adc _RSLT,x			; to MSB of final result
	sta _RSLT,x			; store result in MSB of the result's hi word

	lda _RSLT+2			; add any carry from the result's MSB Addition			
	adc #00				; to the LSB of the result's hi word
	STA _RSLT+2
	
	clc				; clear carry before jumping back for next addition
	jmp _add1	


_finia16:	
	lda _RSLT+2			; get 32 bit results LSB of Hi-Word
	pha				; push it onto the stack
	lda _RSLT+1			; get 32 bit results MSB of Lo-Word
	pha				; push it onto the stack
	lda _RSLT			; get 32 bit results LSB of Lo-Word
	pha				; push it onto the stack
	lda _retadra16+1		; restore the MSB of the callling program's return address
	pha				; onto the stack
	lda _retadra16			; restore the LSB of the calling program's return address
	pha				; onto the stack
					; note: above will insure adhering to 6502's little endian encoding
	RTS                     	; return from subroutine


_test:	LDA (_NUMADD1),y		; test to see if both of the next words == $0000
	CMP #$00			; which indicates the end of the string of numbers
	BNE _test2
	LDA (_NUMADD2),y
	CMP #$00
	BNE _test2
	iny
	LDA (_NUMADD1),y
	CMP #$00
	BNE _setx
	LDA (_NUMADD2),y
	CMP #$00
	BNE _setx
	LDA #$FF			; if they do store $FF in the temp location (LOC3)
	STA _TMP			; for the calling program to test for
_setx:	dey
_test2:	RTS

;data segment starts here
_TMP:		.word $0000,$0000	; temp storage for addition of current pair (32 bit)
_RSLT:		.word $0000,$0000	; final result storage (32 bit)
_retadra16:	.word $0000		; save return address from stack

;/**************************************************************************
;* 06-Feb-22 (JR)
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

  	
	.org $0BB8			
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



;/**************************************************************************'
;*	Subroutine:	_mult8	
;*			*(call with JSR _mult8)
;*	
;*	Input:		Place the two 8bit numbers to be multiplied on the
;*			6502's stack before calling _mult8
;*
;*	Required:	2 8bit numbers at top of stack prior to call
;*
;*	Output:		16 bit result will be on the top of the 6502 stack
;*			MSB of result at stack pointer
;*			LSB of result at stack pointer - 1
;*			
;**************************************************************************/				

	.org $0F30			
					; set the .org at whatever the program counter is currently at
					; this "jams" the subroutine right up against the bottom of the calling pgm
					; alternative -->program starting at .org $0900
					; 6502SIM doesn't like .org $
					; TASM doesn't like .org $ unless it's in an "INCLUDED" .asm file

	  
	; entry into mult8 subroutine
_mult8:	PLA			
	STA _retadrm8			; save lo byte of return address
	PLA
	STA _retadrm8+1			; save hi byte of return address

	PLA				; get multiplicand 
	STA _factor1
	PLA				; get multiplier
	sta _factor2
	

	LDA #0
	LDX  #$8
	LSR  _factor1
_mult:  BCC  _no_add
	CLC
	ADC  _factor2
_no_add: ROR 
	ROR  _factor1
	DEX
	BNE  _mult
	STA  _factor2
	; done, high result in factor2, low result in factor1	
	  
	; prepare for return from subroutine by placing the following on the stack:
	; STACK -->  	MSB of return address
	;		LSB of return address
	;		MSB of result
	;		LSB of result
	;
	;		after the RTS is executed in this subroutine 
	;		the calling program will be able to immediately
	;		pull the MSB of the result off the stack 1st
	;		pull the LSB of the result off the stack 2nd
	;		once these two PLA's are accomplished the stack
	;		is now restored to the condition it was in before
	;		the call to MULT8
	LDA _factor1
	PHA
	LDA _factor2
	PHA
	LDA _retadrm8+1
	PHA
	LDA _retadrm8
	PHA

_finim8:	
	rts
	  
_factor1: 	.byte $00
_factor2: 	.byte $00
_retadrm8:	.byte $00,$00

;/**************************************************************************'
;*	Subroutine:	_mult16	
;*			*(call with JSR _mult16)
;*
;*	Purpose:	Multiply 2 16bit unsigned numbers
;*			Return 32 bit unsigned result
;*
;*	On Entry:	TOP OF STACK
;*			Lo-Byte of Return Add   (RETADRM16)
;*			Hi-Byte of Return Add   (RETADRM16+1)
;*			Lo-Byte of Multiplier   (MLIER)
;*			Hi-Byte of Multiplier   (MLIER+1)
;*			Lo-Byte of Multiplicand (MCAND)
;*			Hi-Byte of Multiplicand (MCAND+1)
;*	
;*
;*	Output:		TOP OF STACK
;*			LSWord Lo-Byte of result  (MLIER)
;*			LSWord Hi-Byte of result  (MLIER+1)
;*			MSWord Lo-Byte of result  (HIPROD)
;*			MSWord Hi-Byte of result  (HIPROD+1)
;*			
;**************************************************************************/
   	
				

	.org $0E00			
					; set the .org at whatever the program counter is currently at with .org $
					; this "jams" the subroutine right up against the bottom of the calling pgm
					; alternative -->program starting at .org $0900
					; 6502SIM doesn't like .org $
					; TASM doesn't like .org $ unless it's in an "INCLUDED" .asm file
					; so only use .org $ if using assembled program in AppleWin Sim

	  
	; entry into mult16 subroutine
_mult16:	PLA			
	STA _retadrm16			; save lo byte of return address
	PLA
	STA _retadrm16+1		; save hi byte of return address

	
	PLA				; get multiplier LSB (MLIER)
	STA _mlier
	PLA				; get multiplier MSB (MLIER+1)
	sta _mlier+1
	
	PLA				; get multiplicand LSB (MCAND)
	STA _mcand
	PLA				; get multiplicand MSB (MCAND+1)
	STA _mcand+1
	

	LDA #0
	STA _hiprod			; zero hi-word of product
	STA _hiprod+1
	LDX  #17			; number of bits in multiplier plus 1
					; extra loop is to move the last (potential) carry
					; into the product 
	CLC

_mult16a:	; if next bit = 1 then
	; _hiprod:= _hiprod + _mcand
	ROR _hiprod+1
	ROR _hiprod
	ROR _mlier+1
	ROR _mlier
	BCC  _no_add1			; skip around the add routine if next bit of
					; multiplier is 0

	CLC				; next bit is 1 so add multiplicand to product
	LDA _mcand
	ADC _hiprod
	STA _hiprod
	LDA _mcand+1
	ADC _hiprod+1
	STA _hiprod+1			; carry = overflow from add
_no_add1:DEX
	BNE  _mult16a

	; push lo word of product onto stack
	LDA _mlier+1
	PHA
	LDA _mlier
	PHA

	; push hi word of product onto stack
	LDA _hiprod+1
	PHA
	LDA _hiprod
	PHA

	; push return address onto stack
	lda _retadrm16+1
	pha
	lda _retadrm16
	pha

_finim16:	
	rts
	  
_mcand  	 .word $0000			; multiplicand
_mlier  	 .word $0000			; multiplier
_hiprod 	 .word $0000			; MSWord of product
_retadrm16	 .word $0000


;/**************************************************************************'
;*	Subroutine:	_sub16	
;*			*(call with JSR _sub16)
;*
;*	Purpose:	Subtract 2 16 bit signed/unsigned words and return
;*			a 16 bit signed/unsigned difference
;*
;*	On Entry:	TOP OF STACK
;*			RETURN ADDRESS LSB
;*			RESTUR ADDRESS MSB
;*			SUBTRAHEND LSB
;*			SUBTRAHEND MSB
;*			MINUEND LSB
;*			MINUEND MSB
;*	
;*
;*	On Return:
;*			TOP OF STACK
;*			LSB of difference
;*			MSB of difference
;*			
;**************************************************************************/


	.org $0F00			
					; set the .org at whatever the program counter is currently at with .org $
					; this "jams" the subroutine right up against the bottom of the calling pgm
					; alternative -->program starting at .org $0900
					; 6502SIM doesn't like .org $
					; TASM doesn't like .org $ unless it's in an "INCLUDED" .asm file
					; so only use .org $ if using assembled program in AppleWin Sim

	  
	; entry into sub16 subroutine
_sub16:	; save return address
	PLA			
	STA _retadrs16			; save lsb byte of return address
	PLA
	STA _retadrs16+1		; save msb byte of return address

	; gbet subtrahend
	PLA				; get subtrahend LSB 
	STA _subtra
	PLA				; get subtrahend MSB
	sta _subtra+1
	
	; subtrace subtrahend from minuend
	PLA				; get minuend LSB
	SEC
	SBC _subtra
	tay				; save low byte of the difference
	pla				; get minuend MSB
	sbc _subtra+1

	; push the difference
	pha				; push MSB of difference
	TYA
	pha				; push LSB of difference

	; push return address onto stack
	lda _retadrs16+1
	pha
	lda _retadrs16
	pha

_fini:	rts
	  
; data
_subtra:		.word	$0000
_retadrs16:		.word	$0000

	.end
;/**************************************************************************
;* 09-Feb-22 - 11-FEB-22 (JR)
;* 10-Aug-22 (JR)
;*	Tested with Retroassembler & VSC
;*
;*	Subroutine:	_calcfib	
;*			*(call with JSR _calcfib
;*
;*	Purpose:	calculate a fibonacci sequence between 0 and given end number
;*			(all calculated numbers will be 32Bit numbers)
;*			current limit is 0 to 47
;*			(a Fibonacci number > 47 will exceed 32 bits)
;*			Formula to calculate a single Fibonacci #:
;*				Fn = ( (1 + √5)^n - (1 - √5)^n ) / (2^n × √5)
;*				where n > 47 will overflow 32 bits
;*			
;*
;*
;*	On Entry:	TOP OF STACK
;*			_retadrf 	Return Address LSB
;*			_retadrf+1 	Return Address MSB
;*			_startf 	LSB (number where to start fibonacci sequence)
;*			_startf+1   	MSB
;*			_endf 		LSB (number where to stop fibonacci sequence)
;*			_endf+1 	MSB
;*
;*			NOTE:	_startf is defaulted to $0000 regardless of what the
;*				calling program sent in.  This way the entire fibonacci
;*				sequence calculated will be from 0 thru _endf
;*		
;*	Requirement:	  	_endf <= 47
;*				Failure of the above test will cause the subroutine to
;*				return with no Fibonacci #'s calculated but the 1st
;*				number at the address returned to the calling program
;*				will be $FFFF (which indicates the end of the Fibonacci Sequence)
;*	
;*
;*	On Return:
;*			TOP OF STACK
;*			_resultadrf 	LSB (address where fibonacci sequence starts
;*			_resultadrf+1 	MSB
;*
;*			NOTE:  	String of calculated Fibonacci #'s will be terminated
;*				with $FFFFFFFF
;*
;*
;*			
;**************************************************************************/



;/**************************************************************************'
;*// pseudo-code for algorithim used
;* function calcfib(start,end)
;*	dim result[end - start]
;*	loop = 0
;*	while (loop <= end) {
;*	  if loop < 2 {
;*	    result[loop] = loop
;*	  } else {
;*	    result[loop] = result[loop - 1] + result[loop - 2]
;*	  }
;*	  loop++
;*	}
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
   	
	.org $1000			
					; set the .org at whatever the program counter is currently at with .org $
					; this "jams" the subroutine right up against the bottom of the calling pgm
					; alternative -->program starting at .org $0900
					; 6502SIM doesn't like .org $
					; TASM doesn't like .org $ unless it's in an "INCLUDED" .asm file
					; so only use .org $ if using assembled program in AppleWin Sim

	  
	; entry into calcfib subroutine
_calcfib:		
	PLA			
	STA _retadrf			; save lsb byte of return address
	PLA
	STA _retadrf+1			; save msb byte of return address

	; need to pull these off the stack but they're going to get dumped anyhow
	pla				; get _start LSB
	STA _startf
	pla				; get _start MSB
	STA _startf+1
	
	; override _startf values passed in and set to default of $0000
	LDA #$00
	STA _startf
	sta _startf+1

	pla				; get _end LSB
	STA _endf
	pla				; get _end MSB
	sta _endf+1

	; test for _endf > 47 and if so....exit
	lda #$FF			; set 1st 16bit number of _resultsf
	sta _resultsf+1			; to $FFFF
	sta _resultsf

	lda _endf+1
	cmp #$00			; if MSB of _endf <> 0 then _endf > 0 	
	beq stp1			; 
	JMP _finifb1			; exit
					; NOTE:  had to change bne 
					; 		       jmp _finifb1    to
					;		       ------------------
					;		       beq stp1
					;		       jmp _finifb1
					;	 due to original bne exceeded
					;        the 6502's range limit for a
					;	 branch instruction	
stp1:	CLC
	LDA _endf
	CMP #48				; if LSB of _endf is >= 48 then
	bcc stp2
	jmp _finifb1			; exit
					; NOTE:  had to change bcs 
					; 		       jmp _finifb1    to
					;		       ------------------
					;		       bcc stp2
					;		       jmp _finifb1
					;	 due to original bne exceeded
					;        the 6502's range limit for a
					;	 branch instruction
	

	; prep work before starting calcs
stp2:	CLC
	lda #$00			; start _loop at $FFFF
	sta _loop			; initial increment will result in $0000
	sta _loop+1
	ldx #$0				; zero out Xreg for use as index into _resultsf

_bgnf:	; start of fibonacci calc
	
_cmp1:	; compare _endf to _loop using cmp16 subroutine
	; which will perform a subtraction of _endf - _loop
	; and then will set the Z,N,C flags accordingly
	; NOTE:  See cmp16.asm 

	lda _endf+1			; minuend MSB
	pha
	lda _endf			; minuend LSB
	pha
	lda _loop+1			; subtrahend MSB
	pha
	lda _loop			; subtrahend LSB
	PHA
	

	; save x/y registers
	stx _tmpx
	sty _tmpy
	jsr _cmp16

	; check results of _cmp16
	BNE cmp1a
	jmp _finifb
cmp1a:	
	BCS cmp1b			; carry is clear which indicates _loop is > _endf
	jmp _finifb

	; restore x/y registers
cmp1b:
	LDX _tmpx
	ldy _tmpx

	; _loop <= _endf so continue calculating
	lda #$00
	CMP _loop+1			; MSB of _loop == 0 ??
	bne _cnt1			; no so jmp to cnt1
	lda #$02
	CMP _loop			; LSB of _loop >= 2 ??
	BNE _cnt
	
	; special case of 2nd "1" in Fibonacci sequence
	LDA #$01
	STA _resultsf,x
	inc _fibcount
	
	; replaces INX \\ INX \\ INX \\ INX
	; pushes XReg forward 4 byte positions
	PHP
	LDA #$04
	STA _incamt
	JSR incxreg
	plp

	; normal continuation
_cnt:	bcc _cnt1			; YES: so jmp to cnt1
					; NO:  fall into common save routine

	; _loop < 2 (0,1)
	; common save routine
saveit:
	inc _fibcount
	LDA _loop
	STA _resultsf,x
	INX
	LDA _loop+1
	STA _resultsf,x
	
	; replaces INX \\ INX \\ INX
	; pushes XReg forward 3 byte positions
	PHP
	LDA #$03
	STA _incamt
	JSR incxreg
	plp
			
	JMP _cnt2			; jmp to _cnt2 to calc next fibonacci seq #

	; _loop >= 2
	; _resultsf[loop] = _resultsf[loop - 1] + _resultsf[loop - 2]
_cnt1:
	inc _fibcount
	CLC
	; save x/y registers
	STX _tmpx
	STY _tmpy

	;
	;TXA
	;TAY
	
	; set increment/decrement amount to #$04
	LDA #$04
	STA _incamt
	
	; move YReg back 4 byte positions
	jsr decyreg

	; add LSB's
	LDA _resultsf,y
	jsr decyreg
	ADC _resultsf,y
	STA _resultsf,x
	
	; move YReg fwd 4 byte positions
	jsr incyreg
	LDA _resultsf+1,y
	
	; add MSB's
	jsr decyreg
	ADC _resultsf+1,y
	INX
	STA _resultsf,x
	
	; move YReg fwd 4 byte positions
	jsr incyreg
	LDA _resultsf+2,y
	
	; add MSW-LSB's
	jsr decyreg
	ADC _resultsf+2,y
	INX
	STA _resultsf,x
	
	; move YReg fwd 4 byte positions
	jsr incyreg
	LDA _resultsf+3,y
	
	; move YReg bwd 4 byte positions
	JSR decyreg
	
	; add MSW-MSB
	ADC _resultsf+3,y
	INX
	STA _resultsf,x
	
	; restore x/y registers
	LDX _tmpx
	ldy _tmpy

	; move XReg fwd 4 byte positiions
	JSR incxreg
	
	JMP _cnt2		

	; increment all _loop values then go again
_cnt2:	; move _loop1 into _loop2 (_loop2 = _loop - 2)
	lda _loop1
	sta _loop2
	lda _loop1+1
	sta _loop2+1

	; move _loop into _loop1 (_loop1 = _loop - 1)
	lda _loop
	sta _loop1
	lda _loop+1
	sta _loop1+1

	; increment _loop
	jsr incloop

	jmp _bgnf


	; _loop > _endf so calculation is over
_finifb:
	; restore x/y registers
	ldx _tmpx
	ldy _tmpy

	; terminate fibonacci sequence with $FFFFFFFF
	lda #$FF			; set final 32bit number of _resultsf
					; to $FFFFFFFF
	sta _resultsf+3,x
	sta _resultsf+2,x
	STA _resultsf+1,x
	sta _resultsf,x
	
_finifb1:
	; push count of calculated fibonacci #'s
	LDA _fibcount
	pha
	; push start address of calculated Fibonacci #'s onto stack
	lda _addrresults+1		; MSB
	pha
	lda _addrresults		; LSB
	pha

	; push return address onto stack and return
	lda _retadrf+1			; MSB
	pha
	lda _retadrf			; LSB
	pha
	rts

;/**************************************************************************'
;* tiny routine used as internal subroutine (JSR/RTS)
;* use to increment the 16bit variable _loop
;* NOTE:  Too small to turn into external subroutine
;**************************************************************************/
incloop:
	; inc 16bit _loop value
	; internal subroutine
	inc _loop
	lda #$0
	cmp _loop
	beq incloopa			
	rts
incloopa:
	inc _loop+1
	RTS


;/**************************************************************************'
;* tiny routines used as internal subroutines (JSR/RTS)
;* used to increment/decrement X/Y Registers a specified amount (_incamt)
;**************************************************************************/	
incxreg:
	PHP
	sta _tmpacc
	CLC
	TXA
	ADC _incamt
	TAX
	lda _tmpacc
	PLP
	RTS
	
incyreg:
	PHP
	sta _tmpacc
	CLC
	TYA
	ADC _incamt
	TAY
	lda _tmpacc
	PLP
	RTS
	
decyreg:
	PHP
	sta _tmpacc
	sec
	TYA
	SBC _incamt
	TAY
	lda _tmpacc
	PLP
	rts	

	


;/**************************************************************************'
;* Data Segment
;**************************************************************************/
;data 
_startf:	.word	$0000			; starting number for the fibonacci sequence
_endf:		.word 	$0000			; ending number for the fibonacci sequence

_loop:		.word	$0000			; looping control during fibonacci seq calc
_loop1:		.word	$0000			; tracking _loop - 1
_loop2:		.word	$0000			; tracking _loop - 2
_looptmp:	.word	$0000			; temporary storage for _loop

; really only need $C4 (196) bytes to hold 48 32 bit fibonacci #'s plus a 32bit terminator
; but using $FF (255) leaves a comfortable cushion.
;_resultsf	.fill $FF,0			; storage for 255 Fibonacci #'s (for use w/TASM32)
;_resultsf	.rs $FF 			; storage for 255 Fibonacci #'s (for use w/Sim6502)
_resultsf	.ds $FF 			; storage for 255 Fibonacci #'s (for use w/Sim6502)

_addrresults	.word	_resultsf		; 16bit address where calculated Fibonacci #'s start
						; This is the address returned to the calling program.

_tmpx		.BYTE 	$00			; temp XReg storage
_tmpy		.BYTE	$00			; temp YReg storage
_tmpacc		.byte	$00			; temp Accumulator storage

_incamt		.BYTE	$00			; amount to inc/dec X/Y Register

_fibcount	.byte	$00			; count of fibonacci #'s calculated


_retadrf:	.word 	$0000			; return address to calling program


;/**************************************************************************'
;* Code Includes Segment
;**************************************************************************/
	; includes go here and make sure their .org's are set as indicated.
	//.include "cmp16.asm"	;$1000
	.include "Jeff_Lib.asm"	;various modules & addresses

	.end
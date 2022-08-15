;/**************************************************************************
;* 09-FEB-22 thru 11-FEB-22 (JR)
;* 10-Aug-22 (JR)
;*	Tested with Retroassembler & VSC
;*
;*	Sample program to test CALCFIB.ASM as a self-contained utility program
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


fresults .equ $00FD		; zero page location to save starting address of Fibonacci numbers
				; returned from calcfib.asm.  This will allow for indirect
				; accessing of the returned Fibonacci numbers using the YReg
  
	.org $0800		; program starting at $0800


START:  clc
	CLD
	
	LDA fend+1		; end MSB
	PHA
	LDA fend		; end LSB
	PHA
	LDA fstrt+1		; start MSB
	PHA
	LDA fstrt		; start LSB
	pha

	jsr _calcfib            ; subroutine jump to calc fibonacci sequence

	; retrieve result starting address from stack
	; and place in zp memory 
	pla
	sta fresults		; LSB
	pla
	STA fresults+1		; MSB
	PLA
	sta count

	; routine to display fibonacci sequence
	; to Sim6502 In/Out Window

	sta IO_AREA		; clear In/Out window
	
	; if calcfib.asm returned $FFFFFFFF as the 1st (and only)
	; fibonacci number then an error occured
	; (probably a request with an ending # > 47)
	; (or the start # and end # were the same)
	LDY #$00		; using YReg for indirect addressing
	LDX #$00		; using x for CR/LF every 8th fibonacci #

	jsr isdone		
	bcs error		; carry = 1 only if $FFFF was returned

	; there is a valid Fibonacci sequence so display it
noerror:
	LDA (fresults),y
	STA IO_AREA+3
	INY
	LDA (fresults),y
	STA IO_AREA+3
	INY
	LDA (fresults),y
	STA IO_AREA+3
	INY
	LDA (fresults),y
	STA IO_AREA+3
	iny
	LDA ' '
	STA IO_AREA+1
	INX
	CPX #$08
	BNE noerrcnt
	LDX #$00
	LDA #10
	STA IO_AREA+1
	LDA #13
	STA IO_AREA+1
		
noerrcnt:
	JSR isdone
	BCC noerror		; carry = 1 only if $FFFF is next
		
	; all Fibonacci #'s displayed - done!
	; display final message 
	LDA #10
	STA IO_AREA+1
	LDA #13
	STA IO_AREA+1
	LDX #$00
mmsg:	inx
	LDA msg2,x
	STA IO_AREA+1
	CPX #38
	BNE mmsg
	LDA count
	sta IO_AREA+3
	
	jmp finifb

error:
	LDX #$01		; using XReg for indexing thru msg
		
error1:
	lda msg1,x		; get next char of msg
	sta IO_AREA+2		; display it on In/Out window
	inx			; increment x
	cpx #43			; x == 43 ??
	bne error1		; NO: continue displaying msg

	
finifb:
	brk
	
	
; small subroutine to test if the next 32bit number returned
; is $FFFFFFFF indicating all Fibonacci numbers have been displayed
isdone:	
	DEY			; initial decrement of Yreg so the common
				; loop works for all 4 bytes
idonelp:
	INC idoneloop		; need to track which byte is being worked on
	LDA idoneloop
	CMP #$05		; has it been incremented to 5 ??
	beq idonelpout		; YES: jump to idonelpout
idonelp1:
	CLC			; NO:  CLC prior to any ADC functions
	INY			; increment YReg for indirect access of (fresults)
	LDA (fresults),y	; load byte into accumulator
	ADC #$01		; add 1 (with carry).  If was $FF now it'll be $00
	CMP #$00		; is it == $00 ??
	BNE goodout		; NO: so if any of the 4 bytes <> $FF time to exit
	
idonelpout:
	LDA #$00		; all 4 bytes have been processed so reset counter to 0
	sta idoneloop	
	DEY			; put YReg back to where it was
	dey
	dey
	SEC			; set carry flag to indicate we found 4 consecutive $FF's
				; which means the fibonacci sequence is at an end
	rts			; return
goodout:
	LDA #$00		; reset counter to 0 for next pass thru this subroutine
	sta idoneloop
	CLC			; clear the carry flag to indicate these 4 bytes are not the end
	rts			; return


; data segment
fstrt:		.word 	0	; # to start the fibonacci sequence with
fend:		.WORD 	47	; # to end the fibonacci sequence with (MUST BE <=47)
idoneloop:	.BYTE 	0	; loop tracker for internal isdone: subroutine
count:		.BYTE	0	; count of fibonacci #'s calculated by subroutine
				; sent back to calling program on the stack

; use .string if assembling with 6502SIM
; use .text is assembling with TASM32
; use .text if assembling with retroassembler
msg1:		.text		"ERROR - No Fibonacci numbers were returned"
msg2:		.text		"Qty of Fibonacci Numbers calculated: $"

; comment this next line out if assembling with 6502SIM
; otherwise leave it in
IO_AREA		.equ	$E000

;Includes go here along with what each include's ORG should be
	.include "calcfib.asm"	;$1000


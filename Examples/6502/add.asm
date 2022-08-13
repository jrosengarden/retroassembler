/**************************************************************************
* 18-Jan-22 (JR)
* 08-Aug-22 (JR) - retroassembler v2022.1 is on dropbox at following location:
*				   \\Mac\Dropbox\Jeff\Assembly Programming\retroassembler v2022.1
*				   (Both the Mac (OSx) and Windows (Win11) are pointed at the same location)
*
* Sample 6502 program
*	1) adds 2 8bit numbers with an 8bit result (or 16bit result if result > $FF)
*	2) adds 2 16bit numbers with a 16bit result (or 32bit result if result > $FFFF)
*
* Assemble command (with symbol listing file):
*	retroassembler -x -O=BIN add1.asm > add1.sym
*	load resultant add1.bin file into Sim6502 with a load address of $0000
*
* Dissasemble command (with .LST listing file):	
*	retroassembler -d -D=$0000 add1.bin > add1.lst 
*************************************ı*************************************/
	.target "6502"				; set target processor for RetroAssembler

	// the following .format and .setting's allow the generated output
	// file to be directly pasted into the AppleWin emulator
	.format "txt"										; set output file format as ASCII TXT file 
	.setting "OutputTxtAddressFormatFirst","{0:x04}: "	; add colon & space after address
	.setting "OutputTxtAddressFormatNext","{0:x04}: "	; add colon & space after address


	.org $1000					; data segment starting at $1000
LOC1	.byte 102					; first 8 bit number to be added (decimal format)
LOC2	.byte $e9					; second 8 bit number to be added (hex format)
LOC3	.byte $00,$00				; result: lo 8bits, add'l bit if result > $255
LOC4	.byte $ff,$ff				; 1st 16 bit number: $ffff
LOC5	.byte $ff,$ff				; 2nd 16 bit number: $ffff
LOC6	.byte $00,$00,$00				; Result: lo 8 bits, hi 8 bits, add'l bit if result > $FFFF

      .org $0300					; prog segment starting at $0300

Start:	jsr add1				; subroutine jump to add1
		jsr add2
		brk					; end program on return from subroutine	

			
/**************************************************************************
*	add1 subroutine
*		Will add 8 bit numbers at LOC1 and LOC2
*		Result will be stored at LOC3 for 8bit result and LOC3+1 if 16bit result		
**************************************************************************/
add1:		clc					; clear carry flag
		cld					; clear decimal flag
		lda LOC1				; accumulater = LOC1 value
		adc LOC2				; accumulater = LOC1 + LOC2
							; Carry will be set if addition overflows 8bit number
		sta LOC3				; LOC3 = LOC1 + LOC2
		BCC fini				; if Carry flag == 0 jump to label fini
		lda #$01				; else - Set accumulator to 1
		STA LOC3+1				; and save it to LOC3 + 1
							; This will result in the low 8 bits in LOC3
							; and the high 8 bits in LOC3+1
fini:		rts					; return from subroutine





/**************************************************************************
*	add2 subroutine
*		will add 16 bit numbers at LOC4 and LOC5
*		Result will be stored at LOC6 (lo byte),LOC6+1 (hi byte),LOC6+2
*		LOC6+2 (lo byte of hi word) will have $01 if the 2 16bit numbers > $FFFF	
*				
**************************************************************************/
add2:		clc					; clear carry flag
		cld					; clear decimal flag
lobyte:	lda LOC4				; lo byte of 1st number
		adc LOC5				; add lo byte of 2nd number (with carry)
		sta LOC6				; lo byte of result

hibyte:	lda LOC4+1				; hi byte of 1st number
		ADC LOC5+1				; hi byte of 2nd number (with carry)

		STA LOC6+1				; hi byte of result
		BCC fini2				; if carry clear move on otherwise
		LDA #$01				; put a 1 in the accumulator
		STA LOC6+2				; store accumulator in LOC6+1 (result hi word) location
fini2:	rts					; return from subroutine


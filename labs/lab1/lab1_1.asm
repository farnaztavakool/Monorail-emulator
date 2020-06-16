;
; lab1_1.asm
;
; Created: 6/7/2020 4:24:48 PM
; Author : Feddrick Aquino
;
; Implementation of atoi (ascii to integer) in AVR assembly language

.include	"m2560def.inc" ; ATMega2560 is used
.equ	size = 6  ; define a symbolic constant  
.def	i = r17 ; define a symbolic name on the register
.def	counter = r18
.def	n1 = r20
.def	n2 = r21
.def	n3 = r22		; 3 bytes in total registers allocated to store n
.def	temp_ll = r23		
.def	temp_lh = r24	
.def	temp_hl = r25	; temporary variable to store 2*n value and integer form of s[i]
.def	zero = r0
.dseg    ; data segment
.org	0x200    ; set the starting address of data segment to 0x200
n: .byte	3   ; allocate 3 bytes of data memory to store the integer 325658

.cseg
s:	.db		"325658"		; define the string 325658

	clr		i	; counter = i = 0

	clr		n1
	clr		n2
	clr		n3				; n = 0

	clr     temp_ll
	clr		temp_lh
	clr		temp_hl			; temp = 0

	clr		zero			; zero = r0 = 0
	ldi		counter, size	; r18 = size = 6
	ldi		zh, high(s << 1)
	ldi		zl, low(s << 1)		;makes z points to starting address of s[]

for_loop:
	cp		i,	counter
	brsh	end_loop		; if i >= size, go to end_loop

	lsl	n1
	rol	n2
	rol	n3				; n = 2n

	mov	temp_ll, n1
	mov	temp_lh, n2
	mov	temp_hl, n3		; temp = 2n

	lsl	n1
	rol	n2
	rol	n3				; n = 4n
	lsl	n1
	rol	n2
	rol	n3				; n = 8n

	add	n1, temp_ll
	adc	n2, temp_lh
	adc	n3, temp_hl		; n = 8n + 2n

	clr temp_lh
	clr	temp_hl

	lpm		temp_ll, z+ ; temp = s[i]

	subi	temp_ll, 48
	sbc		temp_lh, zero	
	sbc		temp_hl, zero	; temp = s[i] - '0'

	add		n1, temp_ll
	adc		n2, temp_lh
	adc		n3, temp_hl			; n = 10n + (s[i] - '0')

	inc i
	rjmp for_loop

end_loop:
	ldi		yl, low(n)
	ldi		yh, high(n)		; makes y points to n
	
	st		y+, n1
	st		y+, n2
	st		y+,	n3			
	st		y, zero			; n (in SRAM) =  10n + (s[i] - '0')

loop_infinitely:
	rjmp	loop_infinitely	;loop infinitely
	

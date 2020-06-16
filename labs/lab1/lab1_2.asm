;
; lab1_2.asm
;
; Created: 6/7/2020 4:24:48 PM
; Author : Feddrick Aquino
; Email : f.aquino@student.unsw.edu.au
;
; Summation of i * 200 for 0 <= i <= 9

.include	"m2560def.inc" ; ATMega2560 is used
.equ	twohundred = 200
.def	r_twohundred = r16;
.def	i = r17
.def	sum_l = r4
.def	sum_h = r5
.def	temp_l = r6
.def	temp_h = r7		; temp is used to store A[i] that is pulled from the SRAM (Data memory)

.dseg    ; data segment
.org	0x200    ; set the starting address of data segment to 0x200
A: .byte 20			; Allocate 20 bytes to store 10 2 bytes unsigned integer
sum: .byte 2		; Allocate 2 bytes for sum

.cseg
	clr		i			; i = 0
	clr		sum_l
	clr		sum_h		; sum = 0
	clr		temp_l
	clr		temp_h		; temp = 0
	ldi		r_twohundred, twohundred	; r_twohundred = 200

	ldi		yl, low(A)
	ldi		yh, high(A)			; make y points to starting address of A[]

for_start1:
	cpi		i, 10
	brsh	for_exit1		; if i>=10, goto for_exit1

	mul		i, r_twohundred		; r1:r0 = i * 200
	st		y+, r0
	st		y+, r1				; A[i] = i * 200

	inc		i				; i = i + 1
	rjmp	for_start1

for_exit1:
	ldi		yl, low(A)
	ldi		yh, high(A)			; make y points to starting address of A[] again

	clr		i					; i = 0
for_start2:
	cpi		i, 10
	brsh	for_exit2		; if i>=10, goto for_exit1

	ld		temp_l, y+
	ld		temp_h, y+		; temp = A[i] = i * 200

	add		sum_l, temp_l
	adc		sum_h, temp_h	; sum += A[i]

	inc		i				; i = i + 1
	rjmp	for_start2

for_exit2:
	ldi		yl, low(sum)
	ldi		yh, high(sum)		; make y points to sum (2 bytes memory in data)

	st		y+, sum_l
	st		y+, sum_h		; sum (in data memory) = sum_h:sum_l

loop_infinitely:  
	rjmp	loop_infinitely			; loop infinitely





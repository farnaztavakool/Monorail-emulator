;
; lab1_3.asm
;
; Created: 6/9/2020 
; Author : Feddrick Aquino
; Email : f.aquino@student.unsw.edu.au
;
; Implementation of matrix multiplication

.include	"m2560def.inc"	; ATMega2560 is used
.equ		size = 5
.def		i = r16
.def		j = r17
.def		k = r18
.def		counter = r19
.def		var1_l = r20
.def		var1_h = r21
.def		var2_l = r22
.def		var2_h = r23
.def		result_l = r24
.def		result_h = r25


.dseg
A:			.byte 25
B:			.byte 25
C:			.byte 50

.cseg
	clr		i
	clr		j
	clr		k

	ldi		yl, low(A)
	ldi		yh, high(A)
	ldi		xl, low(B)
	ldi		xh, high(B)

	ldi		counter, size
for_start_outer1:
	cp		i, counter
	brsh	for_exit_outer1		; if i >= size, goto for_exit1
	ldi		j, 0				; j = 0
for_start_inner1:
	cp		j, counter
	brsh	for_exit_inner1		; if j >= size, goto for_exi2

	mov		var1_l, i
	add		var1_l, j			;var1_l = i + j
	st		y+, var1_l			;A[i][j] = i + j

	mov		var1_l, i
	sub		var1_l, j
	st		x+, var1_l			;B[i][j] = i - j 

	inc		j
	jmp		for_start_inner1
for_exit_inner1:
	inc		i
	jmp		for_start_outer1
for_exit_outer1:
	clr		var1_l
	clr		i
	clr		j
	clr		k
	ldi		zl, low(C)
	ldi		zh, high(C)			;z pointing to starting address C

	ldi		yl, low(B)
	ldi		yh, high(B)			;z pointing to starting address B

	ldi		xl, low(A)
	ldi		xh, high(A)			;z pointing to starting address A

for_start_outer2:
	cp		i, counter
	brsh	for_exit_outer2		; if i>=size, goto for_exit_outer2
	ldi		j, 0
for_start_middle2:
	cp		j, counter
	brsh	for_exit_middle2
	ldi		k, 0
	
	clr		result_l
	clr		result_h			; result = 0
for_start_inner2:
	cp		k, counter
	brsh	for_exit_inner2

	ld		x+, var1_l

	ld		y, var2_l
	adiw	yl, 5				

	muls	var1_l, var2_l
	
	add		result_l, r0
	adc		result_h, r1		;result = result +  A[i][k] * B[k][j]

	inc		k
	jmp		for_start_inner2
for_exit_inner2:
	st		z+, result_l
	st		z+, result_h

	sbiw	xl, 5				; makes x points to the starting address of the current row in A
			
	sbiw	yl, 25				
	adiw	yl, 1				; makes y points to the starting address next column in B

	inc		j
	jmp		for_starting_middle2
for_exit_middle2:
	adiw	xl, 5				; makes x points to the starting address of the next row in A
	sbiw	yl, 5				; makes y points to the starting address of B
	inc		i
	jmp		for_starting_outer2
for_exit_outer2:
	
loop_infinitely:
	rjmp	loop_infinitely		;loop infinitely
	











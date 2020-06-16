
; lab1_4.asm
;
; Created: 6/9/2020 
; Author : Feddrick Aquino
; Email : f.aquino@student.unsw.edu.au
;
; Implementation of Horner's method for polynomial evaluation
; Let us evaluate 100x^5 +60x^4 + 120x^3 +100x^2 +50 x +70 for x = 5

.include	"m2560def.inc" ; ATMega2560 is used
.equ	size = 6				; size of the array
.equ	substitute = 5			; the value to substitute to x
.def	i = r2
.def	zero = r3
.def	result_l = r16
.def	result_h = r17			; result used to store the result of polynomial evaluation
.def	n_register = r18		; register for n
.def	x_register = r19		; register for x
.def	r_overflow = r20		; register to indicate overflow
.def	var1_l= r22				
.def	var1_h = r23			; general register to store value
.def	var2_l = r24
.def	var2_h = r25

// @1:@0 = result_h:result_l, @2 = x, @3 = poly[i], @5:@4 = register to store values in multiplication
// this macro assume x = 5
.macro horner_mul
	; result = result_h * 2^8 + result_l
	; result * x = 2^16*a + 2^8(b + c) + d
	; where  2^16*a + 2^8*b is from result_h * x
	; and 2^8*c + d is from result_l * x
	; NOTE: For the result to not overflow, the value of a should be zero
	mul		@0, @2
	movw	@4, r0

	mul		@1, @2

	cp		zero, r1
	brsh	else_not_overflow1		; if a <= 0, no overflow happens

if_overflow1: 
	ldi		r20, 1					; indicates that overflow have occured
else_not_overflow1:
	mov		@0, @4					; result_l = d
	add		r0, @5					; r1 = 2^8(b + c)
	brvc	else_not_overflow2		; if overflow bits is not set, overflow did not happen
if_overflow2:
	ldi		r20, 1					; indicates that overflow have occured
else_not_overflow2:
	mov		@1, r0					; result_h = 2^8(b + c)

	add		@0, @3
	adc		@1, zero				; result = result * x + poly[i]
	brvc	else_not_overflow3		; if overflow bits is not set, overflow did not happen
if_overflow3:
	ldi		r20, 1
else_not_overflow3:
	; we done
.endmacro

.dseg    ; data segment
.org	0x200    ; set the starting address of data segment to 0x200
poly:	.byte	6		; array poly[]
x_data:	.byte	1
n:		.byte	1
result_data: .byte 2

.cseg
poly_cseg: .db	100, 60, 120, 100, 50, 70
	clr		zero
	clr		result_l
	clr		result_h			; result = 0

	ldi		n_register, size
	subi	n_register, 1		; n_register = size of array - 1
	ldi		xl, low(n)
	ldi		xh, high(n)			; x points to starting address of n
	st		x, n_register		; data(n)  = size of array - 1

	ldi		x_register, substitute		; x_register = substitution value
	ldi		xl, low(x_data)
	ldi		xh, high(x_data)			; register(x) points to starting address of x_data
	st		x, x_register				; data(x) = subtitution value

	ldi		zl,	low(poly_cseg << 1)
	ldi		zh, high(poly_cseg << 1)	; z points to starting address of poly[] in code segment

	ldi		yl, low(poly)
	ldi		yh, high(poly)				; y points to starting address of poly[] in data segment

	clr		i							; i = 0
	ldi		n_register, size			; n_register = size of array
	
// The loop here moves poly[] in code segment to its counterpart in data segment
for_start1:
	cp		i, n_register
	brsh	for_exit1			; if i >= n, goto for_exit1

	lpm		var1_l, z+
	st		y+, var1_l			; data(poly[i]) = cseg(poly[i])

	inc		i
	rjmp	for_start1
for_exit1:
	 
	 subi	n_register, 1		; n_register = size of array - 1
	 clr	i
	 inc	i					; i = 1
	 ldi	yl, low(poly)
	 ldi	yh, high(poly)		; y points to starting address of poly[] in data segment

	 ld		result_l, y+
	 clr	result_h			; Assuming data(poly[i]) is only one byte
	 clr	r20

// The loop here computes result using horner's method
for_start2:
	cp		n_register, i
	brlt	for_exit2

	ld		var2_l, y+			; var2_l = poly[i]
	horner_mul	result_l, result_h, x_register, var2_l, var1_l, var1_h

	inc		i
	rjmp	for_start2
for_exit2:
	ldi		yl, low(result_data)
	ldi		yh, high(result_data)		; y points to result in data segment

	st		y+, result_l
	st		y+, result_h				; store result into data(result) in little endian order

loop_infinitely:
	rjmp	loop_infinitely				; Loop infinitely



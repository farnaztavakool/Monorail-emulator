;
; lab2_taskC.asm
;
; Note: LCD D0-7 is connected to PF0-7; LCD control pin BE-RS is connected to PA 4-7
;
; Created: 07/07/2020 12:05:41 PM
; Author : Feddrick Aquino
;
  
.include "m2560def.inc"

.equ		seconds_in_minute = 60
.equ		one_second_num_interrupts = 61
.equ		delay1ms = 1776
.equ		LCD_RS = 7
.equ		LCD_E = 6
.equ		LCD_RW = 5
.equ		LCD_BE = 4

.def		r_numInterrupts = r18
.def		r_seconds = r16
.def		r_minutes = r17
.def		temp1 = r19
.def		temp2 = r22


///////////////// MACROS //////////////////
; macro to set the DB7-DB0 in the lcd
.macro		do_set_lcd_D_bits
	ldi		r20, @0
	rcall	set_lcd_D_bits
	rcall	lcd_wait
.endmacro

; macro to set display an ascii character into lcd screen
.macro		do_display_a_character
	mov		r20, @0
	rcall	display_a_character
	rcall	lcd_wait
.endmacro

; macro	to clear the display of the screen
.macro		clear_lcd_display
	do_set_lcd_D_bits	0b00000001	
.endmacro

; macro to set the control @0'th bit in the lcd
.macro		lcd_ctrl_set
	sbi		PORTA, @0
.endmacro

; macro to clear the control @0'th bit in the lcd
.macro		lcd_ctrl_clr
	cbi		PORTA, @0
.endmacro

; macro to divide @0 by ten and stores the result in @0
; It keeps subtracting 10 from @0, and everytime it subtracts,
; it increment r21 by one
.macro		div_by_ten
	ldi		r20, 10						
	clr		r21							; r21 will store the result of @0/10
dbt_loop_start:
	cp		@0, r20
	brlo	dbt_loop_finishes
	sub		@0, r20						; @0 -= 10
	inc		r21							; r21++
	rjmp	dbt_loop_start				
dbt_loop_finishes:
	mov		@0, r21
.endmacro

; macro to convert a SINGLE digit character in @0 to ascii 
.macro		convert_digit_to_ascii
	ldi		r20, '0'
	add		@0, r20
.endmacro

; macro to modulus @0 by ten and stores the result in @0
; It keeps subtracting 10 from @0 until @0 is lower than 10
.macro		modulo_by_ten
	ldi		r20, 10						
mbt_loop_start:
	cp		@0, r20
	brlo	mbt_loop_finishes
	sub		@0, r20						; @0 -= 10
	rjmp	mbt_loop_start				
mbt_loop_finishes:
.endmacro
/////////////////END MACROS////////////////////

.dseg
.org		0x200

.cseg

.org		0x000
jmp			RESET

.org OVF0addr											; OVF0addr is the address of Timer0 Overflow Interrupt Vector
jmp			Timer0OVF

RESET:
	ldi		temp1, low(RAMEND)
	out		SPL, temp1
	ldi		temp1, high(RAMEND)
	out		SPH, temp1									; set up stack frame

lcd_initialization:
	ser		temp1
	out		DDRF, temp1								; set port F as all output pin
	out		DDRA, temp1								; set port A as all output pin

	clr		temp1
	out		PORTF, temp1
	out		PORTA, temp1				

	do_set_lcd_D_bits	0b00110000
	rcall	wait_5ms
	do_set_lcd_D_bits	0b00110000
	rcall	wait_1ms
	do_set_lcd_D_bits	0b00110000
	do_set_lcd_D_bits	0b00110000					; 8 bits transfer, 2 no of line, and big font
	do_set_lcd_D_bits	0b00001000					; display OFF
	do_set_lcd_D_bits	0b00000001					; clear display
	do_set_lcd_D_bits	0b00000110					; decremental mode with no screen shifting
	do_set_lcd_D_bits	0b00001100					; no cursor and blink
	
timer0_initialization:
	clr		r_seconds
	clr		r_minutes								; reset clock
	clr		r_numInterrupts

	rcall	display_minutes_seconds					; display the 00 00 on LCD

	clr		temp1
	out		TCCR0A, temp1
	ldi		temp1, 0b00000101						; NOTE: TCCRnA is the control register for Timern
													; pre-scaler value is 1024 (the cycle for timer0 runs 1024 slower)
	out		TCCR0B, temp1							; set pre-scaler value as 1024 for timer 0

	ldi		temp1, (1 << TOIE0)
	sts		TIMSK0, temp1							; enable Timer0 overflow interrupt

	sei												; enable global interrupt flag

loop_infinitely:
	rjmp	loop_infinitely							; loop infinitely


Timer0OVF:
	push	temp1
	in		temp1, SREG
	push	temp1
	push	temp2
	; prologue
	ldi		temp1, one_second_num_interrupts		; temp1 = the number of interrupts required for 1 second to pass
	inc		r_numInterrupts

	cp		r_numInterrupts, temp1
	brlo	if_not_one_second						; if one second hasn't passed, return from interrupt handler
	rcall	increment_one_second_lcd				; else increment one seconds and display it on lcd
	clr		r_numInterrupts
if_not_one_second:
	;epilogue
	pop		temp2
	pop		temp1
	out		SREG, temp1
	pop		temp1
	reti

; This function increments the seconds register and update the lcd display
; If 60 seconds have passed, increments the minutes register, and update the lcd display
; Arguments: r_seconds, r_minutes
; Returns: Nothing
increment_one_second_lcd:
	inc		r_seconds								; r_seconds++
	
	cpi		r_seconds, seconds_in_minute			; if r_seconds is not equal 60, branch to if_not_one minute		
	brne	if_not_one_minute
	clr		r_seconds								; set r_seconds to be 00
	inc		r_minutes								; increment one minutes
if_not_one_minute:
	clear_lcd_display
	rcall	display_minutes_seconds					; update the LCD to reflect changes
	ret

; This function first converts each digit in the seconds into 
; ascii character and send it to the lcd. Next, it does the same thing with the minutes counter.
; Arguments: r_seconds, r_minutes
; Return:	-
display_minutes_seconds:
	push	temp1
	push	temp2

	mov	temp1, r_minutes							; temp1 = r_minutes
	div_by_ten	temp1								; temp1 /= 10
	modulo_by_ten	temp1							; temp1 = temp1 %10

	convert_digit_to_ascii temp1
	do_display_a_character	temp1					; display the second digit in r_seconds into lcd (the left digit)
	
	mov		temp1, r_minutes						; temp1 = r_minutes
	modulo_by_ten temp1								; temp1 = temp1 % 10

	convert_digit_to_ascii	temp1						
	do_display_a_character temp1					; display the first digit of r_minutes into lcd
	
	ldi	temp1, ';'
	do_display_a_character temp1					; display a semicolon

	mov	temp1, r_seconds							; temp1 = r_seconds
	div_by_ten	temp1								; temp1 /= 10
	modulo_by_ten	temp1							; temp1 = temp1 %10

	convert_digit_to_ascii temp1
	do_display_a_character	temp1					; display the second digit in r_seconds into lcd (the left digit)

	mov		temp1, r_seconds						; temp1 = r_seconds
	modulo_by_ten temp1								; temp1 = temp1 % 10

	convert_digit_to_ascii	temp1						
	do_display_a_character temp1					; display the first digit of r_seconds into lcd

	pop		temp2
	pop		temp1
	ret

; Display the character in r20 to the lcd
; Arguments: r20
; Return: -
display_a_character:
	out		PORTF, r20
	lcd_ctrl_set	LCD_RS							; select the Data Register
	nop
	nop
	nop
	lcd_ctrl_set	LCD_E
	nop
	nop
	nop
	lcd_ctrl_clr	LCD_E
	nop
	nop
	nop
	lcd_ctrl_clr	LCD_RS
	ret
	

; set DB7-DB0 according to r20 with cycle delay considerations
; Arguments: r20
; Return: -
set_lcd_D_bits:
	out		PORTF, r20
	nop
	lcd_ctrl_set	LCD_E							; set the LCD enable bit
	nop
	nop
	nop
	lcd_ctrl_clr	LCD_E							; clear the LCD enable bit
	nop
	nop
	nop
	ret

; Loop until the busy flag is cleared (LCD is not busy)
; Argument: r20
; Return: -
lcd_wait:
	push	r20
	clr		r20
	out		DDRF, r20								; set all port F as input to read the busy flag
	out		PORTF, r20								; dismiss the pull-up resistor
	lcd_ctrl_set	LCD_RW							; set LCD to read mode
lcd_wait_loop:
	nop												
	lcd_ctrl_set	LCD_E							; set enable bit in lcd
	nop
	nop
	nop
	in		r20, PINF								; read the data bits with BF from port F
	lcd_ctrl_clr	LCD_E							; disable enable bit in lcd
	sbrc	r20, 7									; if busy flag is cleared (means not busy), skip next line
	rjmp	lcd_wait_loop

	lcd_ctrl_clr	LCD_RW							; set lcd into Instruction Register mode
	ser		r20
	out		DDRF, r20								; makes port F output again
	pop		r20
	ret			

; A function that call wait_1ms 5 times
; Registers: -
; Argument: -
; Return: -
wait_5ms:
	rcall	wait_1ms
	rcall	wait_1ms
	rcall	wait_1ms
	rcall	wait_1ms
	rcall	wait_1ms
	ret

; A function that waste approximately 1ms
; Registers: temp1, temp2, r25, r24, 23
; Argument: -
; Return: -
wait_1ms:
	push	temp1
	push	temp2
	
	clr		temp1
	clr		temp2
	ldi		r25, high(delay1ms)
	ldi		r24, low(delay1ms)
wait_1ms_loop_start:
	cp		temp1, r24
	cpc		temp2, r25
	brge	wait_1ms_loop_finish					; if temp2:temp1 >= 8887, break out of the loop

	ldi		r23, 1
	add		temp1, r23
	ldi		r23, 0
	adc		temp2, r23								; r25:r24++

	rjmp	wait_1ms_loop_start
wait_1ms_loop_finish:
	pop		temp2
	pop		temp1
	ret

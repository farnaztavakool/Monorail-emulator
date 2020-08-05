
; the code is built on feddrick's code for task c




.include "m2560def.inc"

.equ			MAXCHAR_DISPLAY = 16								; maximum character that can be contained on the lcd
.equ			delay1ms = 1776
.equ			LCD_RS = 7
.equ			LCD_E = 6
.equ			LCD_RW = 5
.equ			LCD_BE = 4
.equ			stopPeriod = 3											; constants for lcd stuff

.equ			one_second_num_interrupts = 60						; 6 number of interrupts means 100 ms has passed (with 1024 pre-scaler)

.equ			OC3B_PIN = 0b00010000								; constants to initialize the PE4 (OC3B) pin

.equ			waste_n = $FFFF										; constant to define the counter for switch_debouncing

.def			zero = r2
.def			macro_r1 = r16
.def			macro_r2 = r17
.def			temp1 = r18
.def			temp2 = r19
.def			temp3 = r20
.def			temp4 = r21
.def			location = r22




////////////////////////////MACRO START////////////////////////////////////

; macro to set x register points to @0
.macro		set_x
	ldi		xh, high(@0)
	ldi		xl, low(@0)
.endmacro

; macro to set y register points to @0
.macro		set_y
	ldi		yh, high(@0)
	ldi		yl, low(@0)
.endmacro

; macro to set z register to points to @0 (a program memory address)
.macro		set_z
	ldi		zh, high(@0 << 1)
	ldi		zl, low(@0 << 1)
.endmacro

; macro to set the DB7-DB0 in the lcd
.macro		do_set_lcd_D_bits
	ldi		macro_r1, @0
	rcall	set_lcd_D_bits
	rcall	lcd_wait
.endmacro

; macro to set display an ascii character into lcd screen
; It also checks if the current cursor pointing somewhere 
; out of display, shift the screen to the left
.macro		do_display_a_character
	push	yl
	push	yh
	ldi		yl, low(current_lcd_pointer_pos)
	ldi		yh, high(current_lcd_pointer_pos)
	ld		macro_r1, y
	cpi		macro_r1, MAXCHAR_DISPLAY									
	brlo	if_no_shift_lcd_left									; if the current lcd pointer points out of
	shift_lcd_left													; the screen, shift lcd screen to the left
if_no_shift_lcd_left:
	inc		macro_r1
	st		y, macro_r1
	mov		macro_r1, @0
	rcall	display_a_character
	rcall	lcd_wait
	pop		yh
	pop		yl
.endmacro

; macro to shift the lcd display to the left
.macro		shift_lcd_left
	do_set_lcd_D_bits 0b00011000
.endmacro

; macro	to clear the display of the screen
.macro		clear_lcd_display
	sts		current_lcd_pointer_pos, zero							; set the current_lcd_pointer_pos = 0
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
; it increment macro_r2 by one
.macro		div_by_ten
	ldi		macro_r1, 10						
	clr		macro_r2						; macro_r2 will store the result of @0/10
dbt_loop_start:
	cp		@0, macro_r1
	brlo	dbt_loop_finishes
	sub		@0, macro_r1					; @0 -= 10
	inc		macro_r2						; macro_r2++
	rjmp	dbt_loop_start				
dbt_loop_finishes:
	mov		@0, macro_r2
.endmacro

; macro to convert a SINGLE digit character in @0 to ascii 
.macro		convert_digit_to_ascii
	ldi		macro_r1, '0'
	add		@0, macro_r1
.endmacro

; macro to modulus @0 by ten and stores the result in @0
; It keeps subtracting 10 from @0 until @0 is lower than 10
.macro		modulo_by_ten
	ldi		macro_r1, 10						
mbt_loop_start:
	cp		@0, macro_r1
	brlo	mbt_loop_finishes
	sub		@0, macro_r1						; @0 -= 10
	rjmp	mbt_loop_start				
mbt_loop_finishes:
.endmacro
////////////////////////////MACROS END////////////////////////////////////

.dseg



		
.org					0x200
second_left: .byte 1
time:		.byte  1
stop:		.byte  1
start:		.byte  1



num_interrupts_timer0:		.byte	1					; a variable to keep track how many time timer0 overflow happens
num_motor_hole_counter:		.byte	1					; a variable to keep track how many time we have come accross a hole on the DC motor
current_lcd_pointer_pos:	.byte	1					; a variable to keep track the current position of the lcd cursor
station_array:				.byte	2					; a variable to store the current measured rps
;location:					.byte	1

.cseg

.org					0x000
jmp						RESET

.org					INT0addr						; INT0addr is the address of EXT_INT0 (External Interrupt 0)
jmp						EXT_INT0						; interrupt vector for External Interrupt 0

.org					INT1addr						; INT0addr is the address of EXT_INT1 (External Interrupt 1)
jmp						EXT_INT1						; interrupt vector for External Interrupt 1

.org					INT2addr						; INT2addr is the address of EXT_INT2 (External Interrupt 2)
jmp						EXT_INT2						; interrupt vector for External Interrupt 2	



.org					OVF1addr						; OVF0addr is the address of Timer0 Overflow Interrupt Vector
jmp						Timer1OVF			

RESET:
	ldi				temp1, high(RAMEND)
	out				SPH, temp1
	ldi				temp1, low(RAMEND)
	out				SPL, temp1							



	

	set_x			station_array	
	ldi				temp1, 3
	st				x+, temp1
	ldi				temp1, 4
	st				x, temp1

	

	clr temp1
	sts			start, temp1
lcd_initialization:
	set_x			current_lcd_pointer_pos
	st				x, zero								; current_lcd_pointer_pos = 0
	ser				temp1
	out				DDRF, temp1							; set port F as all output pin
	out				DDRA, temp1							; set port A as all output pin

	clr				temp1
	out				PORTF, temp1
	out				PORTA, temp1				

	do_set_lcd_D_bits	0b00110000
	rcall			wait_5ms
	do_set_lcd_D_bits	0b00110000
	rcall			wait_1ms
	do_set_lcd_D_bits	0b00110000
	do_set_lcd_D_bits	0b00111100						; 8 bits transfer, 2 no of line, and font '1'
	do_set_lcd_D_bits	0b00001000						; display OFF
	do_set_lcd_D_bits	0b00000001						; clear display
	do_set_lcd_D_bits	0b00000110						; incremental mode without screen shifting
	do_set_lcd_D_bits	0b00001110						; display cursor and no blink

external_interrupt_initialization:
	clr				temp1
	out				DDRD, temp1							; set PORTD as all input
	ser				temp1
	out				PORTD, temp1						; enable the pull-up resistor for port D

	ldi				temp1, (1<<ISC21|1<<ISC11|1<<ISC01) ; 
	sts				EICRA, temp1						; falling EDGE generate interrupt 2, 1 and 0
	in				temp1, EIMSK
	ori				temp1, (1<<INT2|1<<INT1|1<<INT0)	
	out				EIMSK, temp1						; enable INT2, 1 and 0

timer1_initialization:

	clr				temp1
	sts				TCCR1A, temp1
	ldi				temp1, 0b00000010					; change the precalser value
														; pre-scaler value is 1024 (the cycle for timer0 runs 1024 slower)
	sts				TCCR1B, temp1						; set pre-scaler value as 1024 for timer 0

	ldi				temp1, (1 << TOIE1)
	sts				TIMSK1, temp1
	

		
	clr				temp1					; enable Timer0 overflow interrupt
	sts				time, temp1
	set_x			station_array
	ld				temp1, x
	sts				second_left, temp1
	
pwm_timer3B_initialization:
	ldi				temp1, OC3B_PIN						; temp1 = 0b00010000
	out				DDRE, temp1							; set PE4 (OC3B) pint as output

	clr				temp1
	sts				OCR3BH, temp1
	;ldi				temp1, 0xee
	sts				OCR3BL, temp1						; intially OCR3B = 0x0000 to make the OC3 always low (for non-inverting phase correct)
														; NOTE: the order matters
	ldi				temp1, (1<<CS32|1<<CS30)			
	sts				TCCR3B, temp1						; set prescaler = 1024

	ldi				temp1,  (1<<COM3B1|1<<WGM30)
	sts				TCCR3A, temp1						; set OC3B to be phase correct (non-inverting mode), 8 bit top (0xFFFF), and
	sei	
													; enable global interrupt flag
loop_infinitely:
	rjmp			loop_infinitely						; loop infinitely


; passenger needs to get off
EXT_INT0:

	push temp1
	in temp1, SREG
	push temp1
	
	ldi temp1, 1
	sts	stop, temp1
	

	out DDRC, temp1
	out PORTC, temp1

	

	pop temp1
	out SREG, temp1
	pop temp1
	reti


;This interrupt will start the monorail
EXT_INT1:

	push			temp1
	in				temp1, SREG
	push			temp1
	out				PORTC, temp1
	ldi				temp1, 1
	sts				start, temp1

	pop				temp1
	out				SREG, temp1
	pop				temp1
	reti
	

; This interrupt handler will only increments num_motor_hole_counter by one and returns
EXT_INT2:
	push			temp1
	in				temp1, SREG
	push			temp1
	push			xl
	push			xh

	ld				temp1, x
	inc				temp1								
	st				x, temp1							; num_motor_hole_counter++

	pop				xh
	pop				xl
	pop				temp1
	out				SREG, temp1
	pop				temp1
	reti
	

; This interrupt handler will calculate current rps, and update the lcd monitor if 1 second has passed
; else, just increments num_interrupts_timer0
Timer1OVF:
	
	push			temp1
	in				temp1, SREG
	push			temp1
	push			temp2
	push			temp3

	

	lds				temp1, time
	inc				temp1
	
	;
	cpi				temp1, 50
	brne			not_second				; if num_interrupts_time0 != one_second_num_interrupts, goto if_a_hundred_ms_not_passed

	clr				temp1
	sts				time, temp1
	
	lds			temp1, start
	cpi			temp1, 0
	breq		waiting

	lds				temp1, second_left
	dec				temp1

	sts				second_left, temp1
	mov				temp2, temp1

	
	
	convert_digit_to_ascii temp2							
	do_display_a_character temp2

	

	cpi				temp1, 0	
	breq			new_station

	cpi				temp1, 1
	breq			check_stop

	lds temp3, start
	cpi				temp3, 1

	brne			finish_updating_pwm

	rcall			increase_duty_cycle_function
	
	rjmp			finish_updating_pwm

; the monorail is not operating
not_second:
	sts time, temp1	
	rjmp if_a_hundred_ms_not_passed

waiting:

	clear_lcd_display
	ldi temp1, 's'
	do_display_a_character temp1
	
	rjmp if_a_hundred_ms_not_passed

new_station:
	
	
	ld				temp1, x+						;dummy data 
	ld				temp1, x+
	sts				second_left, temp1
	rjmp 			finish_updating_pwm	


	
				

finish_updating_pwm:
	sts				num_motor_hole_counter, zero
	rjmp			if_a_hundred_ms_not_passed	
	

if_a_hundred_ms_not_passed:
	pop				temp3
	pop				temp2
	pop				temp1
	out				SREG, temp1
	pop				temp1
	reti


check_stop:

	ser temp1
	out	DDRC, temp1
	out PORTC, temp1

	lds temp1, stop
	cpi temp1, 1
	brne if_a_hundred_ms_not_passed	

	clr temp1
	out PORTC, temp1
	rcall lower_duty_cycle_function


	rjmp if_a_hundred_ms_not_passed

; A function that loads OCRN3B, lower  the value of it by 2, and store it back
; Registers: temp1, temp2, temp3
; Arguments: OCR3BH, OCR3BL
; Return: -
lower_duty_cycle_function:
	push			temp1
	push			temp2
	push			temp3

	clr				temp1
	sts				stop, temp1

	clr				temp1
	sts				OCR3BH, temp1								; OCR3B -= 2
	sts				OCR3BL, temp1

/**cycle:
	lds				temp1, OCR3BL
	lds				temp2, OCR3BH								; temp2:temp1 = OCR3B

	ldi				temp3, 5

	cp				temp1, temp3
	cpc				temp2, zero
	brlt			lower_duty_cycle_function_end				; if OCR3B < 2, goto lower_duty_cycle_function_end
	sub				temp1, temp3
	sbc				temp2, zero									; temp2:temp1 -= 2

	sts				OCR3BH, temp2								; OCR3B -= 2
	sts				OCR3BL, temp1
	rcall		wait_5ms
	
	rjmp			cycle**/

	

lower_duty_cycle_function_end:
	pop				temp3
	pop				temp2
	pop				temp1
	ret

; A function that loads OCRN3B, increase the value of it by 2, and store it back
; Registers: temp1, temp2, temp3
; Arguments: OCR3BH, OCR3BL
; Return: -
increase_duty_cycle_function:
	push			temp1
	push			temp2
	push			temp3


	
	lds				temp1, OCR3BL
	lds				temp2, OCR3BH								; temp2:temp1 = OCR3B

	ldi				temp3, $FD									; temp3 = 0xFD = 253

	cp				temp3, temp1
	cpc				zero,  temp2
	brlt			increase_duty_cycle_function_end			; if OCR3B > 253, goto increase_duty_cycle_function_end

	ldi				temp3, 2
	add				temp1, temp3
	adc				temp2, zero									; temp2:temp1 += 2

	ser				temp1
	sts				OCR3BH, temp2								
	sts				OCR3BL, temp1								; OCR3B -= 2
		
increase_duty_cycle_function_end:
	pop				temp3
	pop				temp2
	pop				temp1
	ret

; A function that clear the lcd, display the new rps number onto the screen, display "rps" onto lcd screen
; and return.
; Note: temp1 needs to contain the number that will be displayed
; Registers: 
; Arguments: temp1
; Return: -





	
display_rps:
	push			temp1
	push			temp2
	push			temp3
	push			temp4

	clear_lcd_display

	mov				temp3, temp1
	ldi				temp2, 1						; temp2 = number of digit = 1 
divide_rps_by_ten:
	div_by_ten		temp3								; temp3 /= 10
	cp				temp3, zero
	breq			if_know_how_many_digit			; if temp3 == 0, goto if_know_how_many_digit
	inc				temp2
	rjmp			divide_rps_by_ten				
if_know_how_many_digit:
display_rps_loop_outer_start:
	cp				zero, temp2
	brsh			display_rps_loop_outer_end		; if i <= 0, goto display_rps_loop_outer_end
	
	mov				temp3, temp2
	dec				temp3							; j = i - 1

	mov				temp4, temp1					; temp4 = rps
display_rps_loop_inner_start:
	cp				zero, temp3
	brsh			display_rps_loop_inner_end		; if j <= 0, goto display_rps_loop_outer_end

	div_by_ten		temp4							; rps /= 10

	dec				temp3
	rjmp			display_rps_loop_inner_start

display_rps_loop_inner_end:
	modulo_by_ten	temp4
	convert_digit_to_ascii temp4
	do_display_a_character temp4				

	dec				temp2
	rjmp			display_rps_loop_outer_start
display_rps_loop_outer_end:
	ldi				temp2, 'r'
	do_display_a_character temp2
	ldi				temp2, 'p'
	do_display_a_character temp2
	ldi				temp2, 's'
	do_display_a_character temp2

	pop				temp4
	pop				temp3
	pop				temp2
	pop				temp1
	ret
	
; Display the character in macro_r1 to the lcd
; Registers: macro_r1
; Arguments: macro_r1
; Return: -
display_a_character:
	out		PORTF, macro_r1
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
	

; set DB7-DB0 according to macro_r1 with cycle delay considerations
; Registers: macro_r1
; Arguments: macro_r1
; Return: -
set_lcd_D_bits:
	out		PORTF, macro_r1
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
; Registers: macro_r1
; Argument: macro_r1
; Return: -
lcd_wait:
	push	macro_r1
	clr		macro_r1
	out		DDRF, macro_r1							; set all port F as input to read the busy flag
	out		PORTF, macro_r1							; dismiss the pull-up resistor
	lcd_ctrl_set	LCD_RW							; set LCD to read mode
lcd_wait_loop:
	nop												
	lcd_ctrl_set	LCD_E							; set enable bit in lcd
	nop
	nop
	nop
	in		macro_r1, PINF							; read the data bits with BF from port F
	lcd_ctrl_clr	LCD_E							; disable enable bit in lcd
	sbrc	macro_r1, 7								; if busy flag is cleared (means not busy), skip next line
	rjmp	lcd_wait_loop

	lcd_ctrl_clr	LCD_RW							; set lcd into Instruction Register mode
	ser		macro_r1
	out		DDRF, macro_r1							; makes port F output again
	pop		macro_r1
	ret	

; A function that call wait_1ms 5 times
; Registers: -
; Argument: -
; Return: -
wait_5ms:
	rcall		wait_1ms
	rcall		wait_1ms
	rcall		wait_1ms
	rcall		wait_1ms
	rcall		wait_1ms
	ret

; A function that waste approximately 1ms
; Registers: temp1, temp2, r25, r24, r23
; Argument: -
; Return: -
wait_1ms:
	push		r25
	push		temp1
	push		temp2
	
	clr			temp1
	clr			temp2
	ldi			r25, high(delay1ms)
	ldi			r24, low(delay1ms)
wait_1ms_loop_start:
	cp			temp1, r24
	cpc			temp2, r25
	brge		wait_1ms_loop_finish					; if temp2:temp1 >= 8887, break out of the loop

	ldi			r23, 1
	add			temp1, r23
	ldi			r23, 0
	adc			temp2, r23								; r25:r24++

	rjmp		wait_1ms_loop_start
wait_1ms_loop_finish:
	pop			temp2
	pop			temp1
	pop			r25
	ret

; A function that waste approximately 120ms. Is right after someone presses a switch on the key pad
; Registers: temp1, temp2, r24, r25, r2
; Arguments: -
; Return: -
switch_delay:
; c = 65535 * 29 + 16 cycles
; total time wasted = c/f = 120ms
	push		r25
	push		temp1				; 2 cycles
	push		temp2				; 2 cycles

	clr			temp1				; 1 cycle
	clr			temp2				; 1 cycle

	ldi			r25, high(waste_n)	; 1 cycle
	ldi			r24, low(waste_n)	; 1 cycle

waste_loop_start:
	cp			temp1, r24			; waste_n cycle + 1
	cpc			temp2, r25			; waste_n cycle + 1
	breq		waste_loop_exit		; waste_n cycle + 2

	clr			r2					
	inc			r2					; r2 = 1
	add			temp1, r2			; waste_n cycle
	clr			r2
	adc			temp2, r2			; temp2:temp1 += 1 ; waste_n cycle

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop								; 20  waste_n cycle

	rjmp		waste_loop_start	; 2  waste_n cycle

	

waste_loop_exit:
	pop			temp2				; 2 cycles
	pop			temp1				; 2 cycles
	pop			r25
	ret								; 4 cycle ; total of 9n + 16 							; 4 cycle ; total of 9n + 16 


waste_one_second:

	push temp1
	clr temp1

loop:

	cpi temp1, 10
	breq exit
	rjmp switch_delay
	inc temp1
	rjmp loop

exit:
	
	pop temp1
	ret
				

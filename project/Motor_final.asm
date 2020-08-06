

; the code is built on feddrick's code for task c

; set the prescaler for 1024 doesnt work correctly
; when press the buttons it will restart



.include "m2560def.inc"

.equ			MAXCHAR_DISPLAY = 16								; maximum character that can be contained on the lcd
.equ			delay1ms = 1776
.equ			OC3B_PIN = 0b00010000								; constants to initialize the PE4 (OC3B) pin
.equ			waste_n = $FFFF										; constant to define the counter for switch_debouncing



.equ			KEYPAD_INIT =	0b11110000							; mask to set PORTL 
.equ			ROWMASK_INIT =	0b00000001
.equ			COLMASK_INIT =	0b11101111
.equ			ROWMASK	=	0b00001111							; mask to check whether one of the key is pressed in a column

.equ			KEYPAD_NOT_PRESSED = 10								; constant that describes the keypad hasn't been pressed yet
.equ			ASTERISK_PRESSED = 11 
.equ			ENTER_PRESSED = 12								; constant indicating ENTER key has been pressed
.equ			OTHER_CHARACTER_PRESSED = 13							; constant returned by display_pressed's function to indicates character other than
													; ENTER has been pressed
.equ			UNKNOWN_CHARACTER_PRESSED = 14							; constant to indicates an unknow character has been pressed
.equ			BACKSPACE_PRESSED = 15

.equ			STATION_NAME_READING = 0x1
.equ			TRAVEL_TIME_READING = 0x2
.equ			DWELL_TIME_READING = 0x4							
.equ			FINISH_READING = 0x8								; these constants indicate which stages the input reading is currently in 

.equ			STATION_NAME_READING_BIT = 0
.equ			TRAVEL_TIME_READING_BIT = 1		
.equ			DWELL_TIME_READING_BIT = 3							; these constants correspond to the bit position of the above constants

.equ			STATION_NAME_SIZE = 21								; constant for size of each station name (including null terminator)


.equ			LCD_RS = 7
.equ			LCD_E = 6
.equ			LCD_RW = 5
.equ			LCD_BE = 4									; constant for LCD
.equ			BF_POSITION = 7									; constant that contains the busy flag bit position 


.equ			one_second_num_interrupts = 977							; constant indicating how many time timer0 ovf 
													; should occur before 1s has passed

.def			zero = r2
.def			row = r3
.def			col = r4

.def			macro_r1 = r16
.def			macro_r2 = r17

.def			temp1 = r18
.def			temp2 = r19
.def			temp3 = r20
.def			temp4 = r21

.def			mask1 = r22
							
.def			timer = r23
	; registers used for keypad

.def			return_val_l = r24
.def			return_val_h = r25								; return__val_h:return_val_l as a return register


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

current_lcd_pointer_pos:		.byte	1					; a variable to keep track the current position of the lcd cursor
second_left: .byte 1
stop_flag:		.byte  1
start_flag:		.byte  1
middle_flag:	.byte	1
wait_time:		.byte	1
number_of_stations:	.byte 1
n_stations:			.byte	1



station_array:				.byte	3					; a variable to store the current measured rps
stop_time_array:			.byte	3
stop_station:				.byte	1
start_after_stop:			.byte	1
suspence:					.byte	1
.cseg

.org					0x000
jmp						RESET

	


.org					INT0addr						; INT0addr is the address of EXT_INT0 (External Interrupt 0)
jmp						EXT_INT0						; interrupt vector for External Interrupt 0

.org					INT1addr						; INT0addr is the address of EXT_INT1 (External Interrupt 1)
jmp						EXT_INT1						; interrupt vector for External Interrupt 1



.org					OVF2addr
jmp						Time2OVF	




RESET:


	ldi				temp1, high(RAMEND)
	out				SPH, temp1
	ldi				temp1, low(RAMEND)
	out				SPL, temp1							



	
	; setting the beginning of path
	

lcd_initialization:

	ldi			zl,	low(current_lcd_pointer_pos)
	ldi			zh, high(current_lcd_pointer_pos)

	st				z, zero								; current_lcd_pointer_pos = 0
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
	ori				temp1, (0<<INT2|1<<INT1|1<<INT0)	
	out				EIMSK, temp1						; enable INT2, 1 and 0


;enable this timer when we are in a station 
timer2_initilization:

	clr				temp1
	sts				TCCR2A, temp1
	ldi				temp1, (1<<CS22)|(1<<CS21)|(1<<CS20)					; change the precalser value
														; pre-scaler value is 1024 (the cycle for timer0 runs 1024 slower)
	sts				TCCR2B, temp1						; set pre-scaler value as 1024 for timer 0

	ldi				temp1, (0 << TOIE2)					;we enable it when we stop at a station 
	sts				TIMSK2, temp1
	 
pwm_timer3B_initialization:
	ldi				temp1, OC3B_PIN						; temp1 = 0b00010000
	out				DDRE, temp1							; set PE4 (OC3B) pint as output

	clr				temp1
	sts				OCR3BH, temp1
	sts				OCR3BL, temp1						; intially OCR3B = 0x0000 to make the OC3 always low (for non-inverting phase correct)
														; NOTE: the order matters
	ldi				temp1, (1<<CS32|1<<CS30)			
	sts				TCCR3B, temp1						; set prescaler = 1024

	ldi				temp1,  (1<<COM3B1|1<<WGM30)
	sts				TCCR3A, temp1						; set OC3B to be phase correct (non-inverting mode), 8 bit top (0xFFFF), and
	sei

	
											
												
rjmp keypad					; loop infinitely


; passenger needs to get off

; returned correctly or not for each function
;
EXT_INT0:

	;rcall switch_delay
	push temp1
	in temp1, SREG
	push temp1
	
	ldi temp1, 1
	sts	stop_flag, temp1
	
	
	ser temp1
	out DDRC, temp1

	ldi temp1, 0b00001111
	out PORTC, temp1

	

	pop temp1
	out SREG, temp1
	pop temp1
	reti


;This interrupt will start the monorail
EXT_INT1:

		;rcall switch_delay
		push temp1
		in temp1, SREG
		push temp1
	
		ldi temp1, 1
		sts	stop_flag, temp1
	
	
		ser temp1
		out DDRC, temp1

		ldi temp1, 0b11110000
		out PORTC, temp1

	

		pop temp1
		out SREG, temp1
		pop temp1
		reti
	


	

Time2OVF:
	
	push			temp1
	in				temp1, SREG
	push			temp1
	push			temp2
	push			temp3

	
	inc timer
	cpi timer , 61

	brne do_end_second
	clr timer 
	

	; are we in the middle?
	lds temp1, middle_flag
	cpi temp1, 1			; we are 
	breq decrease_second_left
		
	lds temp1, wait_time			; check if we are waiting at a station 
	cpi temp1, 0
	brne wait_at_station
	
	rjmp end_second

do_end_second: rjmp end_second

decrease_second_left:

	lds	temp1, second_left
	dec temp1
	sts second_left, temp1
	clear_lcd_display

	lds temp3, n_stations

	convert_digit_to_ascii temp1
	do_display_a_character temp1

	;convert_digit_to_ascii temp3
	;do_display_a_character temp3
	rjmp end_second
	
wait_at_station:

	lds temp1, wait_time
	dec temp1

	sts wait_time, temp1
	mov temp2, temp1

	convert_digit_to_ascii temp2
	do_display_a_character temp2

	ldi temp2, 1
	sts suspence, temp2

	cpi temp1, 0
	brne end_second

	ldi temp1, 1			;if wait time is over start 
	sts start_after_stop, temp1
	
	clr temp1
	sts suspence, temp1

end_second:

	pop temp3
	pop temp2
	pop temp1
	out SREG, temp1
	pop temp1
	reti

	





	

lower_duty_cycle_function:

	push			temp1
	push			temp2
	push			temp3

	clr				temp1
	;sts				stop, temp1

	clr				temp1
	sts				OCR3BH, temp1								; OCR3B -= 2
	sts				OCR3BL, temp1

	
	

lower_duty_cycle_function_end:
	pop				temp3
	pop				temp2
	pop				temp1
	ret

;  function that loads OCRN3B, increase the value of it by 2, and store it back
; Registers: temp1, temp2, temp3
; Arguments: OCR3BH, OCR3BL
; Return: -
increase_duty_cycle_function:
	push			temp1
	push			temp2
	push			temp3


	clr				temp2									; temp2:temp1 += 2
	;ser				temp1
	ldi				temp1, 100
	sts				OCR3BH, temp2								
	sts				OCR3BL, temp1								; OCR3B -= 2
		
increase_duty_cycle_function_end:
	pop				temp3
	pop				temp2
	pop				temp1
	ret








keypad:

	set_x			station_array	
	ldi				temp1, 3
	st				x+, temp1
	ldi				temp1, 4
	st				x+, temp1
	ldi				temp1, 5
	st				x, temp1


	

	ldi				temp1, 3
	sts				number_of_stations, temp1

	
	set_y			stop_time_array
	ldi				temp1, 3
	st				y+, temp1
	ldi				temp1, 4
	st				y+, temp1

	ldi				temp1, 4
	st				y, temp1

	clr				temp1
	sts				start_flag, temp1

	clr				temp1
	sts				stop_flag, temp1
	
		
	rjmp			logic_main

logic_main:
	
	set_x			station_array
	set_y			stop_time_array

	ld temp1, x+				;where we are
	sts second_left, temp1

	ldi temp1, 1
	sts middle_flag, temp1			; not in the middle
	sts n_stations, temp1

	ldi temp1, 0
	sts stop_flag, temp1
	sts	wait_at_station, temp1	
	sts suspence, temp1


	

	ldi temp1, 1
	sts start_flag, temp1		;will set the flag when we need to start moving

	jmp halt


halt:

	ldi temp1, (1 << TOIE2)
	sts TIMSK2, temp1

	ser temp1
	out DDRC, temp1
	
one_loop:

	lds temp1, number_of_stations
	inc temp1

	lds temp2, n_stations
	cp temp1, temp2
	breq end_loop

check_suspence:

	lds temp1, suspence
	cpi temp1, 1
	breq check_suspence

check_start:
	lds temp1, start_flag	; if we need to start moving
	cpi temp1, 1
	breq start_moving

check_start_after_stop:
	lds temp1, start_after_stop
	cpi temp1, 1 
	breq start_moving_again

check_second_left:
	lds temp1, second_left
	cpi temp1, 0
	breq do_we_stop


rjmp halt

end_loop: 

	rcall lower_duty_cycle_function
	ldi temp1, ( 0<< TOIE2)
	sts TIMSK2, temp1
	rjmp end_loop

start_moving:

	 
	rcall increase_duty_cycle_function
	rjmp check_second_left


start_moving_again:
	
	ldi temp1, 0
	sts stop_flag, temp1

	ld temp1, x+			;how long to the new station
	sts second_left, temp1

	ld temp1, y+
	rcall increase_duty_cycle_function

	ldi temp1, 1
	sts middle_flag, temp1

	ldi temp1, 0
	sts start_after_stop, temp1
	ldi temp1, 1
	sts	start_flag, temp1

	;inc n_stations
	rjmp halt

	

do_we_stop:

	lds temp1, stop_flag
	cpi temp1, 1
	brne new_station

			;stop the motor
	ldi temp1, 0
	sts middle_flag, temp1				;empty the middle flag
	sts start_flag, temp1			;empty the start flag

	rcall lower_duty_cycle_function

	ld	temp1, y
	sts wait_time, temp1			;how long to wait at the station 
	
	
	rjmp halt
	; how long to wait? the new station 				

new_station:

	ld temp1, x+			;how long to the new station
	sts second_left, temp1

	ld temp1, y+

	lds temp1, n_stations
	inc temp1
	sts n_stations, temp1
	rjmp halt

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


/**stop_at_station:

	clr macro_r1
	ld macro_r2, y
	

loop:

	cp macro_r1, macro_r2
	breq exit
	inc macro_r1
	ldi temp1, 10
loop1:
	cpi temp1, 10
	breq loop
	inc temp1
	rjmp switch_delay
	rjmp loop1

exit:
	ret
	**/			

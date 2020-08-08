
; the code is built on feddrick's code for task c

; set the prescaler for 1024 doesnt work correctly
; when press the buttons it will restart



.include "m2560def.inc"

.equ			MAXCHAR_DISPLAY = 16								; maximum character that can be contained on the lcd
.equ			delay1ms = 1776


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

.equ			FOSC = 16000000
.equ			BAUD = 9600
.equ			MYUBRR = (FOSC/16/BAUD-1)							; constants for the USART

.equ			waste_n = $FFFF										; constant to define the counter for switch_debouncing

.equ			one_second_num_interrupts = 977							; constant indicating how many time timer0 ovf 
													; should occur before 1s has passed
													
.equ			OC3B_PIN = 0b00010000								; constants to initialize the PE4 (OC3B) pin

.def			zero = r2
.def			temp1 = r16
.def			temp2 = r17
.def			temp3 = r18
.def			temp4 = r19									; registers for general use
		
.def			macro_r1 = r20
.def			macro_r2 = r21									; registers used by macros

.def			mask1 = r22
.def			row = r3
.def			col = r4									; registers used for keypad

.def			return_val_l = r24
.def			return_val_h = r25								; return__val_h:return_val_l as a return register


.def			timer = r23
/////////////////////////////MACRO START////////////////////////////////////

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
	call		set_lcd_D_bits
	call		lcd_wait
.endmacro

; macro to set display an ascii character into lcd screen
; It also checks if the current cursor pointing somewhere 
; out of display, shift the screen to the left
.macro		do_display_a_character
	push		yl
	push		yh
	ldi		yl, low(current_lcd_pointer_pos)
	ldi		yh, high(current_lcd_pointer_pos)
	ld		macro_r1, y
	cpi		macro_r1, MAXCHAR_DISPLAY									
	brlt		if_no_shift_lcd_left						; if the current lcd pointer points out of
	shift_lcd_left									; the screen, shift lcd screen to the left
if_no_shift_lcd_left:
	inc		macro_r1
	st		y, macro_r1
	mov		macro_r1, @0
	call		display_a_character
	call		lcd_wait
	pop		yh
	pop		yl
.endmacro

; macro to get the current address of the pointer in the lcd and store it onto @0
.macro do_get_lcd_pointer_address
	clr		macro_r1
	out		DDRF, macro_r1							; set all port F as input to read the address counter
	out		PORTF, macro_r1							; dismiss the pull-up resistor
	lcd_ctrl_set	LCD_RW								; set LCD to read mode
	nop								
	lcd_ctrl_set	LCD_E								; set enable bit in lcd
	nop
	nop
	nop
	in		macro_r1, PINF							; read the address counter from port F
	lcd_ctrl_clr	LCD_E								; disable enable bit in lcd
	nop	
	nop
	nop
	lcd_ctrl_clr	LCD_RW								; set the R/W register in the lcd to 0 again
	ser		macro_r2
	out		DDRF, macro_r2							; makes port F output again

	mov		@0, macro_r1							; @0 = LCD DD-RAM addresss counter (with addition of busy flag)

	rcall		lcd_wait							; wait for lcd to not be busy, then return
.endmacro

; macro to store @0 into the DD-RAM address counter of the LCD 
.macro do_store_lcd_pointer_address
	sbr		@0, 0x80 							; ensure that the BF is set
	out		PORTF, @0							; by defaulte LCD_RW, LCD_RS = 0, so we can immediately write @0 into the D0-7 register
	lcd_ctrl_set	LCD_E
	nop
	nop
	nop
	lcd_ctrl_clr	LCD_E
	nop
	nop
	nop

	rcall		lcd_wait							; wait for lcd to not be busy, then return
.endmacro

; macro to decrements the lcd DD-RAM address counter by one. Note: it requires one extra register @0 and it changes the value
.macro decrement_lcd_pointer_address
	
	do_get_lcd_pointer_address	@0						; @0 contain the current pointer address in the LCD
	dec				@0						; LCD pointer adddress--
	do_store_lcd_pointer_address	@0						; update the DD-RAM address counter in lcd
.endmacro

; macro to shift the lcd display to the left
.macro		shift_lcd_left
	do_set_lcd_D_bits 0b00011000
.endmacro

; macro to shift the lcd display to the right
.macro		shift_lcd_right
	do_set_lcd_D_bits 0b00011100
.endmacro

; macro	to clear the display of the screen
.macro		clear_lcd_display
	sts		current_lcd_pointer_pos, zero				; set the current_lcd_pointer_pos = 0
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

; macro that checks whether row == 0 && col == 0. If yes, @0 = 1 else @0 = 0
; Note: this macro assumes @0 to be r16-r31
.macro		check_row_col_zero
	ldi		@0, 1								; @0 = 1 (initially assumes row = 0 & col = 0)
check_row_zero:
	cp		row, zero
	breq		check_col_zero							; if row == 0, goto check_col_zero
	ldi		@0, 0								; else @0 = 0, and return from macro
	rjmp		check_row_col_zero_end
check_col_zero:
	cp		col, zero							 
	breq		check_row_col_zero_end						; if col == 0, return from macro
	ldi		@0, 0								; else @0 = 0, and return from macro
check_row_col_zero_end:
.endmacro
; macro to find the character corresponding to the key enterred in the keypad and the return value stored in @0
; Note: if (row, col) = (0, 0), this macro returns 'Y'
; Note: this function assumes @1 to be register between r16-r31.
; e.g.	1. if (row, col) = (0, 1), this macro returns 'A' in @0
;	2. if (row, col) = (2, 1), this macro returns 'P' in @0
.macro		find_character_according_row_col
	cp			row, zero
	brne			if_not_row_col_zero
	cp			col, zero
	brne			if_not_row_col_zero
	ldi			@0, 'Y'
	rjmp			find_character_according_row_col_end
if_not_row_col_zero:
	mov			@0, row							; @0 = row
	ldi			@1, 9							; @1 = 9
	mul			@0, @1							; r1:r0 = 9 * row					
	mov			@0, r0							; @0 = 9 * row

	add			@0, col
	add			@0, col
	add			@0, col							; @0 = 9 * row + 3 * col


	ldi			@1, 'A'
	subi			@1, 3							; @1 = 'A' - 3

	add			@0, @1							; @0 = ('A' - 3) +  (9 * row + 3 * col)
find_character_according_row_col_end:
.endmacro

; macro that start timer0 if input_reading_stage == STATION_NAME_READING, else stops the timer 0
.macro set_timer0_according_to_input_stage
	lds		macro_r1, input_reading_stage
	cpi		macro_r1, STATION_NAME_READING
	brne		stop_timer0
	call		timer0_initialization
	rjmp		set_timer0_according_to_input_stage_end
stop_timer0:
	call		timer0_stop
set_timer0_according_to_input_stage_end:
.endmacro

; macro to convert a SINGLE digit character in @0 to ascii
; and store the result into @0
.macro		convert_digit_to_ascii
	ldi		macro_r1, '0'
	add		@0, macro_r1
.endmacro

; macro to conver a single ASCII DIGIT character in @0 into a number
; ands store the result into @0
.macro		convert_ascii_to_digit
	ldi		macro_r1, '0'
	sub		@0, macro_r1
.endmacro

; macro that do @1 % @0, and store the value onto @1. Note: @0 is expected to be a constant
.macro		modulus_one_byte_by
	ldi			macro_r1, @0
modulus_one_byte_by_loop_start:
	cp			@1, macro_r1
	brlt			modulus_one_byte_by_loop_end
	sub			@1, macro_r1						; @1 -= @0
	rjmp			modulus_one_byte_by_loop_start
modulus_one_byte_by_loop_end:
.endmacro

; macro that clear the 2 bytes located in @0. Note: @0 should be a constant/label
.macro		clear_two_bytes
	sts			@0, zero
	sts			@0 + 1, zero		
.endmacro

; macro that remoes external interrupt flag
; Note: @0 should contain the bit position of the external interrupt flag in EIFR
.macro		remove_ext_int_flag
	in			macro_r1, EIFR
	ldi			macro_r2, (1 << @0)
	com			macro_r2
	and			macro_r1, macro_r2
	out			EIFR, macro_r1						; clear the INTF1  bit in the EIFR register
.endmacro

////////////////////////////////////////////////////////////////// USART macros//////////////////////////////////////////////////////////////////

.macro USART_Init
	ldi		macro_r1, high(MYUBRR)
	sts		UBRR0H, macro_r1
	ldi		macro_r1, low(MYUBRR)
	sts		UBRR0L, macro_r1							; set up the baud rate on the board

	ldi		macro_r1, (1<< TXEN0| 1 <<RXEN0)
	sts		UCSR0B, macro_r1							; enable transmitter and receiver

	ldi		macro_r1, (1<< USBS0|3 <<UCSZ00)					; set frame structure. 8 data bit and 2 stop bit
	sts		UCSR0C, macro_r1
.endmacro

.macro USART_Transmit
	;Wait for empty transmit buffer
USART_Transmit_loop:
	lds		macro_r1, UCSR0A
	sbrs		macro_r1, UDRE0
	rjmp		USART_Transmit_loop							; if USART0 data register is empty, put the data into the buffer

	sts		UDR0, @0

.endmacro

.macro USART_Receive
USART_Receiver_loop:
	lds		macro_r1, UCSR0A
	sbrs		macro_r1, RXC0
	rjmp		USART_Receiver_loop							; if USART0 data register is empty, put the data into the buffer

	lds		@0, UDR0
.endmacro

; A macro to check that correct character for station names is input
; Only 'A - Z', 'a - z', ' ', or '\n' is valid
.macro check_valid_station

check_station_name_character:
	cpi		@0, '\n'								; Check against ASCII code for '\n'
	breq		check_valid_station_valid_input
	cpi		@0, ' '									; Check against ASCII code for ' '
	breq		check_valid_station_valid_input
	cpi		@0, 'A'									; Anything else below 65 is not valid
	brlt		check_valid_station_invalid_input
	cpi		@0, 'Z'	+ 1								; Within 'A - Z' range
	brlt		check_valid_station_valid_input
	cpi		@0, 'a'									; Between 91 nad 97 is invalid
	brlt		check_valid_station_invalid_input
	cpi		@0, 'z' + 1								; Within 'a - z' range
	brlt		check_valid_station_valid_input

check_valid_station_invalid_input:
	ldi		return_val_l, UNKNOWN_CHARACTER_PRESSED
	rjmp		check_valid_station_end
check_valid_station_valid_input:
	mov		return_val_l, @0
check_valid_station_end:
.endmacro



; macro to check valid character for time input
; Only '0-9', '\n'
.macro check_valid_time
check_time_input_character:
	cpi		@0, '\n'								; Check for '\n'
	breq		check_valid_time_valid_input
	cpi		@0, '0'									; Below 48 is not valid
	brlt		check_valid_time_invalid_input
	cpi		@0, '9' + 1								; Within '0 - 9'
	brlt		check_valid_time_valid_input

check_valid_time_invalid_input:
	ldi return_val_l, UNKNOWN_CHARACTER_PRESSED
	rjmp		check_valid_time_end
check_valid_time_valid_input:
	mov		 return_val_l, @0
check_valid_time_end:
.endmacro

; macro to move string from @0 (a program memory address) to @1 (a data memory address)
; this assumes the data memory is big enough to store the string
.macro move_string_program_to_data
	set_z	@0
	set_x	@1
move_string_loop:
	lpm		macro_r1, z+
	cpi		macro_r1, 0
	breq	move_string_loop_end
	st		x+, macro_r1								; a loop that keep moving characters from @0 to @1 until
														; it reaches null terminator and break out of the loop
	rjmp	move_string_loop
move_string_loop_end:
	st		x+, macro_r1								; put the null terminator too

.endmacro
/////////////////////////////////MACRO END//////////////////////////////////////

.dseg



		
.org					0x200


station_prompt_data:			.byte	15
travel_time_prompt_data:		.byte	20
dwell_time_prompt_data:			.byte	20
wrong_input_data:			.byte	25
finish_string_data:			.byte	7					; am array that will be containing the variable "finish"

current_lcd_pointer_pos:		.byte	1

prev_row:				.byte	1
prev_col:				.byte	1					; prev row and col will store the previous key pressed onto the keypad
last_typed_character:			.byte	1					; a variable to store the last typed character on the LCD 

num_timer0_ovf:				.byte	2					; a variable to store how many time timer0 has overflowed

input_reading_stage:			.byte	1					; a variable to keep track in which stage of input reading are the program currently in

curr_input_time:			.byte	1					; a variable to store the time being input from the keypad

station_name_array:			.byte	21 * 21					; a 2d array that can store 20 string each of size 20 (excluding null terminator)
											; Note: in manual, it only store 20 station name. The extra slot is by USART to input
											; check. Refer to the function "USART_initiation" for more info

travel_time_array:			.byte	20					; an array of 20 bytes containing travel times
dwell_time_array:			.byte	20					; an array of 20 bytes containing dwell time
curr_num_parameters:			.byte	1					; a variable to keep track how many parameters we currently have 
curr_station_name_num_characters:	.byte	1					; a variable to contain the current number of character of the station  name being input


second_left:				.byte 1
stop_flag:				.byte  1
start_flag:				.byte  1
middle_flag:				.byte	1
wait_time:				.byte	1
number_of_stations:			.byte 1
n_stations:				.byte	1

emergency_stop:				.byte	1
start_after_emergency:			.byte 1


start_after_stop:			.byte	1
suspence:				.byte	1
blink:					.byte	1

.cseg

.org					0x000
jmp					RESET

	


.org					INT0addr						; INT0addr is the address of EXT_INT0 (External Interrupt 0)
jmp					EXT_INT0						; interrupt vector for External Interrupt 0

.org					INT1addr						; INT0addr is the address of EXT_INT1 (External Interrupt 1)
jmp					EXT_INT1						; interrupt vector for External Interrupt 1




.org					OVF2addr
jmp					Time2OVF	

.org					OVF0addr							; OVF0addr is the address of Timer0 Overflow Interrupt Vector
jmp					Timer0OVF


RESET:


	ldi			temp1, high(RAMEND)
	out			SPH, temp1
	ldi			temp1, low(RAMEND)
	out			SPL, temp1							


lcd_initialization:

	ldi			zl,	low(current_lcd_pointer_pos)
	ldi			zh, high(current_lcd_pointer_pos)

	st			z, zero								; current_lcd_pointer_pos = 0
	ser			temp1
	out			DDRF, temp1							; set port F as all output pin
	out			DDRA, temp1							; set port A as all output pin

	clr			temp1
	out			PORTF, temp1
	out			PORTA, temp1				

	do_set_lcd_D_bits	0b00110000
	call			wait_5ms
	do_set_lcd_D_bits	0b00110000
	call			wait_1ms
	do_set_lcd_D_bits	0b00110000
	do_set_lcd_D_bits	0b00111100						; 8 bits transfer, 2 no of line, and font '1'
	do_set_lcd_D_bits	0b00001000						; display OFF
	do_set_lcd_D_bits	0b00000001						; clear display
	do_set_lcd_D_bits	0b00000110						; incremental mode without screen shifting
	do_set_lcd_D_bits	0b00001110						; display cursor and no blink

																			
decide_input_platform:
	rcall			display_input_choice					; a function that display "PB0/PB1-USART/KEYPAD"

	clr			temp1
	out			DDRD, temp1						; set PIND as all input
	ser			temp2
	out			PORTD, temp2						; set the pull up resistor for all of PIND
decide_input_platform_loop_start:
	in			temp1, PIND						; temp1 = current PIND state
	ldi			temp2, 0x01						
	and			temp1, temp2						; temp1 = current PIND state & 0x01
	cpi			temp1, 0
	breq			if_USART_case						; if PB0 is pressed, goto if_USART_case

	in			temp1, PIND						; temp1 = current PIND state
	ldi			temp2, 0x02						
	and			temp1, temp2						; temp1 = current PIND state & 0x02
	cpi			temp1, 0
	breq			if_keypad_case						; if PB1 is pressed, goto if_keypad_case


	rjmp			decide_input_platform_loop_start			; go read PB0/PB1 until one of them is clicked

if_keypad_case:
	clear_lcd_display
	rcall			read_input_through_keypad

	rjmp			main_monorail

if_USART_case:
	clear_lcd_display
	rcall			USART_initiation
	rjmp			main_monorail
	
main_monorail:
	remove_ext_int_flag	INTF0
	remove_ext_int_flag	INTF1							; clear the external INT0&1 interrupt flag caused by PB0/1 being click
	
external_interrupt_initialization:
	clr			temp1
	out			DDRD, temp1							; set PORTD as all input
	ser			temp1
	out			PORTD, temp1						; enable the pull-up resistor for port D

	ldi			temp1, (1<<ISC21|1<<ISC11|1<<ISC01) ; 
	sts			EICRA, temp1						; falling EDGE generate interrupt 2, 1 and 0
	in			temp1, EIMSK
	ori			temp1, (0<<INT2|1<<INT1|1<<INT0)	
	out			EIMSK, temp1						; enable INT2, 1 and 0

timer2_initilization:

	clr			temp1
	sts			TCCR2A, temp1
	ldi			temp1, (1<<CS22)|(1<<CS21)|(1<<CS20)					; change the precalser value
													; pre-scaler value is 1024 (the cycle for timer0 runs 1024 slower)
	sts			TCCR2B, temp1						; set pre-scaler value as 1024 for timer 0

	ldi			temp1, (0 << TOIE2)					;we enable it when we stop at a station 
	sts			TIMSK2, temp1
	 
pwm_timer3B_initialization:

	ldi			temp1, OC3B_PIN						; temp1 = 0b00010000
	out			DDRE, temp1							; set PE4 (OC3B) pint as output

	clr			temp1
	sts			OCR3BH, temp1
	sts			OCR3BL, temp1						; intially OCR3B = 0x0000 to make the OC3 always low (for non-inverting phase correct)
														; NOTE: the order matters
	ldi			temp1, (1<<CS32|1<<CS30)			
	sts			TCCR3B, temp1						; set prescaler = 1024

	ldi			temp1,  (1<<COM3B1|1<<WGM30)
	sts			TCCR3A, temp1						; set OC3B to be phase correct (non-inverting mode), 8 bit top (0xFFFF), and
	sei				

;the function will set the paramethers for the motor
;if will implement the logics for the motor

logic_main:
	
	ldi			temp1, 1
	sts			middle_flag, temp1
	out			DDRC, temp1
	
	ldi			temp1, 0
	sts			n_stations, temp1

	rcall			find_station_travel_time
	rcall			display_station_name

	ldi			temp1, 0

	sts			stop_flag, temp1		;flag indicating if the train in moving
	sts			wait_at_station, temp1		;flag indicating when the train is waiting at a station
	sts			wait_time, temp1		;indicating the waiting time for the current station
	sts			suspence, temp1			;indicating when there is no need for any logic implementation 
	sts			blink, temp1			;timer for 3Hz blink
	sts			start_after_stop, temp1		;indicating the train is starting after stopping at a station 
	sts			emergency_stop, temp1		;indicating emergency stop at a station 
	sts			start_after_emergency, temp1	;indicating when the train is starting after emergency stop

	ldi			temp1, 1
	sts			start_flag, temp1		;will set the flag when we need to start moving
	
	clr			temp1
	out			PORTC, temp1
	jmp			halt


; logic implementation for the train 
halt:

	ldi			temp1, (1 << TOIE2)		;start timer2
	sts			TIMSK2, temp1

	ser			temp1
	out			DDRC, temp1
	
	
; checks if we are in the last station 	
one_loop:

	lds			temp1, curr_num_parameters	
	lds			temp2, n_stations
	cp			temp1, temp2
	breq			end_loop

; checks if there is an emergency
check_emeregency:

	lds			temp1, emergency_stop
	cpi			temp1, 1
	breq			do_stop_emergency

; checks if there is no need of any logic implementation 
check_suspence:

	lds			temp1, suspence
	cpi			temp1, 1
	breq			check_suspence

; checks if the train has to start moving
check_start:

	lds			temp1, start_flag	; if we need to start moving
	cpi			temp1, 1
	breq			start_moving

;checl if we are moving after we stopped in a station 
check_start_after_stop:	

	lds			temp1, start_after_stop
	cpi			temp1, 1 
	breq			start_moving_again



; checks how long is left to the next station 
check_second_left:				


	lds			temp1, second_left
	cpi			temp1, 3		;checks to display the name of the next station 
	breq			do_show_next_station

	lds			temp1, second_left
	cpi			temp1, 0
	breq			do_we_stop


; infinite loop  
	rjmp			halt

; does the logic part for showing the name of the next station 
do_show_next_station: 

	lds			temp1, curr_num_parameters	
	lds			temp2, n_stations

	inc			temp2
	cp			temp1, temp2
	breq			halt

	rcall			display_next_station_name
	rjmp			halt


do_stop_emergency:		rjmp stop_emergency

; will stop the emulator and reset the paramethers to loop 
end_loop: 

	rcall			lower_duty_cycle_function
	ldi			temp1, ( 0<< TOIE2)
	sts			TIMSK2, temp1


	
	rcall			last_stop

	call			switch_delay
	call			switch_delay
	call			switch_delay
	call			switch_delay
	call			switch_delay
	call			switch_delay
	call			switch_delay
	call			switch_delay

	clear_lcd_display
	rjmp			logic_main
	;rjmp end_loop

; start the motor 
start_moving:

	 
	rcall			increase_duty_cycle_function
	rjmp			check_second_left


; start moving after stopping at a station 
start_moving_again:
	

	ldi			temp1, 0
	sts			stop_flag, temp1
	sts			wait_time, temp1
	
	rcall			increase_station
	rcall			find_station_travel_time
	
	
	rcall			increase_duty_cycle_function

	ldi			temp1, 1
	sts			middle_flag, temp1

	ldi			temp1, 0
	sts			start_after_stop, temp1
	ldi			temp1, 1
	sts			start_flag, temp1	

	

	rjmp			halt

	
; checks if we need to stop at the next station 
do_we_stop:

	

	lds			temp1, stop_flag
	cpi			temp1, 1
	brne			new_station

			;stop the motor
	ldi			temp1, 0
	sts			middle_flag, temp1				;empty the middle flag
	sts			start_flag, temp1			;empty the start flag

	rcall			lower_duty_cycle_function
	rcall			find_stop_time

	clear_lcd_display
	rcall			increase_station
	rcall			display_station_name

	lds			temp1, n_stations
	dec			temp1
	sts			n_stations, temp1
	
	
		

	rjmp			halt
	 				
; sets the parametheres and will go to the next station 
new_station:

	rcall			increase_station
	rcall			find_station_travel_time
	clear_lcd_display

	rcall			display_station_name
	rjmp			halt

; incrase the numeber of the stations we have visited	
increase_station:

	lds			temp1, n_stations
	inc			temp1
	sts			n_stations, temp1
	ret

; will stop the motor in case of emergency
stop_emergency:

	ldi			temp1, ( 0<< TOIE2)			;stop timer2
	sts			TIMSK2, temp1

	clear_lcd_display

	ldi			temp1, 'E'
	do_display_a_character	temp1
	ldi			temp1, 'M'
	do_display_a_character	temp1
	ldi			temp1, 'E'
	do_display_a_character	temp1
	ldi			temp1, 'R'
	do_display_a_character	temp1
	ldi			temp1, 'G'
	do_display_a_character	temp1
	ldi			temp1, 'E'
	do_display_a_character	temp1
	ldi			temp1, 'N'
	do_display_a_character	temp1
	ldi			temp1, 'C'
	do_display_a_character	temp1
	ldi			temp1, 'Y'
	do_display_a_character	temp1
	

	rcall			lower_duty_cycle_function

;starts the motor after emergency	
continue_after_emergency:

	lds			temp1, start_after_emergency
	cpi			temp1, 1 
	brne			continue_after_emergency

	ldi			temp1, 0
	sts			stop_flag, temp1
	sts			emergency_stop, temp1
	sts			continue_after_emergency, temp1

	clear_lcd_display
	rcall			display_station_name
	ldi			temp1, ( 1<< TOIE2)
	sts			TIMSK2, temp1

	
	rjmp			halt



; A function that reads all the inputs for the monorail from the keypad
; Note: this function don't save any register (treat is as another main function).
read_input_through_keypad:

keypad_initialization:
	ldi			temp1, KEYPAD_INIT
	sts			DDRL, temp1						; set R0-R3 as input, C0-C3 as output

	ldi			temp1, KEYPAD_NOT_PRESSED 

	sts			prev_row, temp1
	sts			prev_col, temp1
	sts			last_typed_character, temp1				; set prev_row, prev_col and last_typed_character as the constant
											; KEYPAD_NOT_PRESSED to indicate that there are no key pressed yet

	;ser			temp1
	;out			PORTF, temp1						;  enable R0-R3 pull up resistor. Open all C0-C3 switch

	ldi			temp1, STATION_NAME_READING
	sts			input_reading_stage, temp1				; input_reading_stage = STATION_NAME_READING

	sts			curr_num_parameters, zero				; curr_num_parameters = 0
	sts			curr_station_name_num_characters, zero			; curr_station_name_num_characters = 0

	call			timer0_initialization					; turn on timer0 with pre-scaler of 64

	sei										; enable global interrupt (mainly for timer0OVF currently)
main_reading_input:
	clr			col
	ldi			mask1, COLMASK_INIT					; mask = 0b11101111
col_loop:
	sts			PORTL, mask1
	call			delay
	lds			temp1, PINL						; read the state of current column
	andi			temp1, ROWMASK						; temp1 &= 0b00001111
	cpi			temp1, ROWMASK
	breq			no_switch_pressed					; if temp1 &= 0b00001111 != ROW_MASK, goto no_switch_pressed
											; else, a key is pressed, search for it 
	clr			row							; row = 0
	ldi			mask1, ROWMASK_INIT					; mask = 0b00000001
row_loop:
	mov			temp2, temp1						; temp2 = the state of current column
	and			temp2, mask1						
	cpi			temp2, 0
	brne			continue_scanning_row					; if temp2 & mask != 0, goto continue_scanning_row
											; else we found the key pressed,

	lds			temp1, input_reading_stage

	cpi			temp1, STATION_NAME_READING
	brne			display_time_character					; if input_reading_stage != STATION_TIME_READING, goto display_time_character
											; else, display_station_name_character
display_station_name_character:
	cli										; clear global interrupt bit
	rcall			display_pressed_character_station_name			; display the pressed character onto LCD

	in			temp1, TIFR0
	cbr			temp1, 0x01
	out			TIFR0, temp1						; clear the timer0 ovf interrupt flag (not having this is okay, since it only affects
											; 1 counter increment in num_timer0_ovf)

	sei										; NOTE: it is important to make sure display_pressed_character_station_name won't be interrupted
											; by timer0 overflow
	
	rjmp			display_pressed_character_finished
display_time_character:
	rcall			display_pressed_digit_time			
	rjmp			display_pressed_character_finished
display_pressed_character_finished:
	call			switch_delay						; delay for approx 0.124sec
	cpi			return_val_l, ENTER_PRESSED
	breq			main_reading_input_end					; if the "ENTER" key is pressed, break out the current stage input reading and
											; move on to reading the next sgae

	cpi			return_val_l, BACKSPACE_PRESSED
	breq			jump_back_to_main_reading_input				; if the "BACKSPACE" key is pressed, continue scanning the next character

	cpi			return_val_l, ASTERISK_PRESSED
	breq			relative_branch_asterisk_resolve			; if the "ASTERISK" key is pressed, break out of the entire reading stage
											; and move on to the next stage of simulating the railway
	rjmp			relative_branch_asterisk_not_needed
relative_branch_asterisk_resolve:
	rjmp			input_reading_finish
relative_branch_asterisk_not_needed:
	lds			temp1, input_reading_stage
	cpi			temp1, STATION_NAME_READING
	breq			if_station_name_input_stage				; if input_reading_stage == STATION_NAME_READING , goto if_station_name_input_stage
											; (which means reads the next character being input for the current station name)
	; If we arrive at this point, result_val_l will contain the number 0-9 which we need to store onto curr_input_time
	mov			temp1, return_val_l					; temp1 = return value of display_pressed_digit_time (between 0-9)
	rcall			update_curr_input_time					; update the curr_input_time according to the current return_val_l value
	rjmp			jump_back_to_main_reading_input
if_station_name_input_stage:
	cpi			return_val_l, UNKNOWN_CHARACTER_PRESSED
	breq			jump_back_to_main_reading_input				; if return_val_l == UNKNOWN_PRESSED_CHARACTER, goto jump_back_to_main_reading_input

	mov			temp1, return_val_l					; return_val_l = last_typed_character
	rcall			store_curr_character_station_name			; otherwise, it is a valid character, store it onto the current station name array

	lds			temp1, curr_station_name_num_characters
	inc			temp1
	sts			curr_station_name_num_characters, temp1			; curr_station_name_num_characters++
	
jump_back_to_main_reading_input:
	rjmp			main_reading_input					; continue scanning new pressed key

continue_scanning_row:
	inc			row							; row++
	lsl			mask1							; mask1 << 1
	rjmp			row_loop						; goto row_loop	
	
	
no_switch_pressed:
	rol			mask1							; update mask1
	inc			col							; col++
	ldi			temp3, 4						; temp3 = 4
	cp			col, temp3
	breq			relative_branch_resolve_main_reading_input		; if col == 4, goto main
	rjmp			relative_branch_resolve_main_reading_input_not_needed
relative_branch_resolve_main_reading_input:
	rjmp			main_reading_input
relative_branch_resolve_main_reading_input_not_needed:
	rjmp			col_loop						; else, goto col_loop
main_reading_input_end:
	clear_lcd_display								; clear the display to read the next input
	
	lds			temp1, input_reading_stage
	cpi			temp1, STATION_NAME_READING
	breq			insert_null_terminator_station_name			; if input_reading_stage == NAME_STATION_READING, goto change_input_stage
	rcall			store_curr_input_time					; otherwise, store curr_input_time to either travel/dwell array
	rjmp			change_input_stage
insert_null_terminator_station_name:
	ldi			temp1, '\0'
	rcall			store_curr_character_station_name			; insert '\0' to the index right after the last character in the current statio name array
	sts			curr_station_name_num_characters, zero			; curr_station_name_num_characters = 0
change_input_stage:
	call			change_input_reading_stage				; call change_input_reading_stage to change input_reading_stage variable accordingly

	lds			temp1, input_reading_stage
	cpi			temp1, STATION_NAME_READING
	brne			check_if_curr_input_time_need_reset			; if the new input_reading_stage != STATION_NAME_READING, goto check_if_curr_input_time_need_reset

	lds			temp1, curr_num_parameters				; otherwise, curr_num_parameters++
	inc			temp1
	sts			curr_num_parameters, temp1

check_if_curr_input_time_need_reset:
	lds			temp1, input_reading_stage				
	cpi			temp1, STATION_NAME_READING				
	breq			update_timer0_state					; if the new input_reading_stage == STATION_NAME_READING, goto update_input_stage
	sts			curr_input_time, zero					; otherwise, clear curr_input_time
update_timer0_state:			
	cli
	set_timer0_according_to_input_stage						; macro that stops timer0 if input reading stage != STATION_NAME_READING, else it initialize it
	sei										; NOTE: this macro shouldn't be interrupted by timer0 (since it is changing the state of it)
	rjmp			main_reading_input
input_reading_finish:									; input reading finish at this point
	clear_lcd_display
	;rcall			display_all_arrays					; IF_DEBUG
	;call			delay_three_half_seconds				; allow timer0 to stabilize
	cli
	call			timer0_stop						; stops timer0 overflow interrupt
	sei	

	ret

; A function that display "PB0/1-USART/KEY"
; Registers: temp1
; Arguments: -
; Returns: -
display_input_choice:
	push			temp1
	ldi			temp1, 'P'
	do_display_a_character	temp1
	ldi			temp1, 'B'
	do_display_a_character	temp1
	ldi			temp1, '0'
	do_display_a_character	temp1
	ldi			temp1, '/'
	do_display_a_character	temp1
	ldi			temp1, '1'
	do_display_a_character	temp1
	ldi			temp1, '-'
	do_display_a_character	temp1
	ldi			temp1, 'U'
	do_display_a_character	temp1
	ldi			temp1, 'S'
	do_display_a_character	temp1
	ldi			temp1, 'A'
	do_display_a_character	temp1
	ldi			temp1, 'R'						; display "Time exceeded 255" onto the lcd
	do_display_a_character	temp1
	ldi			temp1, 'T'
	do_display_a_character  temp1
	ldi			temp1, '/'
	do_display_a_character  temp1
	ldi			temp1, 'K'
	do_display_a_character  temp1
	ldi			temp1, 'E'
	do_display_a_character  temp1
	ldi			temp1, 'Y'
	do_display_a_character  temp1

	pop			temp1
	ret	


; A function that reads all the input for the monorail from USART (local host)
; Note: this function don't save any register (treat is as another main function).
USART_initiation:
	
station_prompt:			.db	'E','n','t','e','r',' ','s','t','a','t','i','o','n',':','\0', '\0'
travel_time_prompt:		.db	'E','n','t','e','r',' ','t','r','a','v','e','l',' ','t','i','m','e',':','\0', '\0'
dwell_time_prompt:		.db	'E','n','t','e','r',' ','d','w','e','l','l',' ','t','i','m','e',':','\0'
wrong_input:			.db	'I','n','v','a','l','i','d',' ','i','n','p','u','t',' ','d','e','t','e','c','t','e','d','\0', '\0'
finish_string:			.db	'f','i','n','i','s','h','\0','\0'


	move_string_program_to_data	station_prompt, station_prompt_data
	move_string_program_to_data	travel_time_prompt, travel_time_prompt_data
	move_string_program_to_data	dwell_time_prompt, dwell_time_prompt_data
	move_string_program_to_data	wrong_input, wrong_input_data
	move_string_program_to_data	finish_string, finish_string_data

	USART_Init
	ldi			temp1, STATION_NAME_READING
	sts			input_reading_stage, temp1					; initially, start with input_reading_stage = STATION_NAME_READING
	
	sts			curr_num_parameters, zero					; curr_num_parameters = 0

check_input_parameter:
	lds			temp1, input_reading_stage

	cpi			temp1, STATION_NAME_READING
	breq			reading_STATION_NAME_input_start				; if input_reading_stage == STATION_NAME_READING, goto reading_STATION_NAME_input_start

	cpi			temp1, TRAVEL_TIME_READING
	breq			reading_TRAVEL_TIME_input_start					; if input_reading_stage == TRAVEL_TIME_READING, goto reading_TRAVEL_TIME_input_start

	cpi			temp1, DWELL_TIME_READING
	breq			reading_DWELL_TIME_input_start_relative_branch_resolve		; if input_reading_stage == DWELL_TIME_READING, goto reading_DWELL_TIME_input_start
	; The code won't reach here

reading_DWELL_TIME_input_start_relative_branch_resolve:
	rjmp			reading_DWELL_TIME_input_start

reading_STATION_NAME_input_start:
	; initialise to write to station_array
	set_y			station_prompt_data
	rcall			transmit_string		
	
	lds			temp1, curr_num_parameters
	ldi			temp2, STATION_NAME_SIZE
	mul			temp1, temp2
	
	set_x			station_name_array
	
	add			xl, r0
	adc			xh, r1								; x = &station_name_array[curr_num_parameters]				

reading_STATION_NAME_input_loop:
	USART_Receive		temp2								; temp2 = new character from USART
	check_valid_station	temp2								; check if the new character is valid or not
	cpi			return_val_l, '\n'				
	breq			reading_STATION_NAME_input_end					; if the new character == '\n', goto reading_STATION_NAME_input_end
	cpi			return_val_l, UNKNOWN_CHARACTER_PRESSED
	breq			handling_wrong_input_branch_resolve1				
	rjmp			handling_wrong_input_branch_resolve1_not_needed
handling_wrong_input_branch_resolve1:
	rjmp			handling_wrong_input
handling_wrong_input_branch_resolve1_not_needed:
	st			x+, temp2
	rjmp			reading_STATION_NAME_input_loop

reading_STATION_NAME_input_end:
	st			x+, zero

	lds			temp1, curr_num_parameters
	ldi			temp2, STATION_NAME_SIZE
	mul			temp1, temp2
	
	set_x			station_name_array
	
	add			xl, r0
	adc			xh, r1								; x = &station_name_array[curr_num_parameters]	

	set_y			finish_string_data
	call			strcmp								; compare the newly input string with "finish" string
	cpi			return_val_h, 0
	breq			USART_initiation_end_branch_resolve1				; if newly input string == "finish", goto USART_initiation_end
	rjmp			USART_initiation_end_branch_resolve1_not_needed
USART_initiation_end_branch_resolve1:
	rjmp			USART_initiation_end
USART_initiation_end_branch_resolve1_not_needed:
	; TODO: check if "finish" is entered		
	call			change_input_reading_stage
	rjmp			check_input_parameter

reading_TRAVEL_TIME_input_start:
	; initialise to write to travel_array
	set_y			travel_time_prompt_data
	rcall			transmit_string

	sts			curr_input_time, zero						; curr_input_time =  0
reading_TRAVEL_TIME_input_loop:
	USART_Receive		temp2
	check_valid_time	temp2
	cpi			return_val_l, '\n'
	breq			reading_TRAVEL_TIME_input_loop_end
	cpi			return_val_l, UNKNOWN_CHARACTER_PRESSED
	breq			handling_wrong_input_branch_resolve2			
	rjmp			handling_wrong_input_branch_resolve2_not_needed
handling_wrong_input_branch_resolve2:
	rjmp			handling_wrong_input
handling_wrong_input_branch_resolve2_not_needed:

	lds			temp1, curr_input_time
	convert_ascii_to_digit	temp2								; convert the new ascii into a digit
	ldi			temp3, 10

	mul			temp1, temp3
	mov			temp1, r0
	; TODO; check overflow
	add			temp1, temp2
	sts			curr_input_time, temp1						; curr_input_time = curr_input_time * 10 + new digit
	rjmp			reading_TRAVEL_TIME_input_loop

reading_TRAVEL_TIME_input_loop_end:
	lds			temp1, curr_num_parameters
	set_x			travel_time_array

	add			xl, temp1
	adc			xh, zero							; x = &travel_time_array[curr_num_parameters]

	lds			temp1, curr_input_time
	st			x, temp1							; travel_time_array[curr_num_parameters] = curr_input_time
	call			change_input_reading_stage
	rjmp			check_input_parameter

reading_DWELL_TIME_input_start:
	; initialise to write to dwell_array
	set_y			dwell_time_prompt_data
	rcall			transmit_string

	sts			curr_input_time, zero						; curr_input_time =  0

reading_DWELL_TIME_input_loop:
	USART_Receive		temp2
	check_valid_time	temp2
	cpi			return_val_l, '\n'

	breq			reading_DWELL_TIME_input_end
	cpi			return_val_l, UNKNOWN_CHARACTER_PRESSED
	breq			handling_wrong_input

	lds			temp1, curr_input_time
	convert_ascii_to_digit	temp2								; convert the new ascii into a digit
	ldi			temp3, 10

	mul			temp1, temp3
	mov			temp1, r0
	; TODO; check overflow
	add			temp1, temp2
	sts			curr_input_time, temp1						; curr_input_time = curr_input_time * 10 + new digit

	rjmp			reading_DWELL_TIME_input_loop

reading_DWELL_TIME_input_end:
	lds			temp1, curr_num_parameters
	set_x			dwell_time_array

	add			xl, temp1
	adc			xh, zero

	lds			temp1, curr_input_time
	st			x, temp1							; travel_time_array[curr_num_parameters] = curr_input_time
	call			change_input_reading_stage

	lds			temp1, curr_num_parameters
	inc			temp1
	sts			curr_num_parameters, temp1
	rjmp			reading_STATION_NAME_input_start

handling_wrong_input:
	set_y			wrong_input_data
	rcall			transmit_string
	rjmp			check_input_parameter
USART_initiation_end:
	ret


; A function to display string on USART
transmit_string:
	push			temp1

transmit_string_loop:
	ld			temp1, y+
	cpi			temp1, 0
	breq			transmit_string_loop_end
	USART_Transmit		temp1
	rjmp			transmit_string_loop
transmit_string_loop_end:
	ldi			temp1, '\n'
	USART_Transmit		temp1
	pop			temp1
	ret

; when passenger needs to get on/off or when want to start after emergency stop
EXT_INT0:
	push			temp1
	in			temp1, SREG
	push			temp1

	rcall			switch_delay

	
	
	lds			temp1, emergency_stop
	cpi			temp1 ,1 
	breq			start_after_emergency_stop

	lds			temp1, stop_flag
	cpi			temp1, 0 
	brne			end_interrupt

	ldi			temp1, 1
	sts			stop_flag, temp1
	
	
	ser			temp1
	out			DDRC, temp1
	ldi			temp1, 0b00001111
	out			PORTC, temp1

	rjmp			end_interrupt

start_after_emergency_stop:
	
	ldi			temp1, 1 
	sts			start_after_emergency, temp1
	
	clr			temp1
	out			PORTC, temp1

end_interrupt:
	
	ldi			temp1, 1
	out			EIFR, temp1

	pop			temp1
	out			SREG, temp1
	pop			temp1
	reti


;This interrupt is for emergency stop
EXT_INT1:
	push			temp1
	in			temp1, SREG
	push			temp1

	rcall			switch_delay
	rcall			switch_delay

	
	
	

	ldi			temp1, 1
	sts			emergency_stop, temp1
	
	
	ser			temp1
	out			DDRC, temp1

	ldi			temp1, 0b11111111
	out			PORTC, temp1
	rjmp			endinterrupt


	
endinterrupt:

	ldi			temp1, 1
	out			EIFR, temp1

	pop			temp1
	out			SREG, temp1
	pop			temp1
	reti
	


	

Time2OVF:
	
	push			temp1
	in			temp1, SREG
	push			temp1
	push			temp2
	push			temp3

	

	lds			temp1, wait_time			; check if we are waiting at a station 
	cpi			temp1, 0
	brne			check_blink				; will blink with 3HZ frequency 

second_time:

	inc			timer
	cpi			timer , 61

	brne			do_end_second
	clr			timer 
	

	
	lds			temp1, middle_flag
	cpi			temp1, 1			; checks if the train is in the middle
	breq			decrease_second_left
		
	lds			temp1, wait_time			;check if we are waiting at a station 
	cpi			temp1, 0
	brne			do_wait
	
	rjmp			end_second

do_wait:
	rjmp  wait_at_station

do_end_second: 
	rjmp end_second

check_blink:

	clr			temp1
	out			PORTC, temp1

	lds			temp1, blink
	inc			temp1
	sts			blink, temp1

	cpi			temp1, 20
	brne			second_time
	ldi			temp1, 0b00001100
	out			PORTC, temp1
	

	clr			temp1
	sts			blink, temp1
	rjmp			second_time

decrease_second_left:

	lds			temp1, second_left
	dec			temp1
	sts			second_left, temp1
	

	lds			temp3, n_stations
	rjmp			end_second
	
wait_at_station:

	lds			temp1, wait_time
	dec			temp1

	sts			wait_time, temp1
	mov			temp2, temp1

	

	ldi			temp2, 1
	sts			suspence, temp2

	lds			temp1, wait_time
	cpi			temp1, 0
	brne			end_second

	ldi			temp1, 1			;if wait time is over start 
	sts			start_after_stop, temp1
	
	clr			temp1
	sts			suspence, temp1

end_second:

	pop			temp3
	pop			temp2
	pop			temp1
	out			SREG, temp1
	pop			temp1
	reti

	





	
; stop the motor 
lower_duty_cycle_function:

	push			temp1
	push			temp2
	push			temp3

	clr			temp1
	
	sts			OCR3BH, temp1								
	sts			OCR3BL, temp1

	

lower_duty_cycle_function_end:
	pop			temp3
	pop			temp2
	pop			temp1
	ret

;star the motor agai

increase_duty_cycle_function:
	push			temp1
	push			temp2
	push			temp3


	clr			temp2									; temp2:temp1 += 2
	ldi			temp1, 100
	sts			OCR3BH, temp2								
	sts			OCR3BL, temp1								; OCR3B -= 2
		
increase_duty_cycle_function_end:
	pop			temp3
	pop			temp2
	pop			temp1
	ret

; find the travel time for the current station 

find_station_travel_time:

	push			temp1
	push			temp2
	push			xl
	push			xh

	lds			temp1, n_stations				
	
			
	set_x			travel_time_array			

	add			xl, temp1
	
	ld			temp1, x					; y = &station_name_array[next station index]
	sts			second_left, temp1


	pop			xh
	pop			xl
	pop			temp2
	pop			temp1
	ret

; finds the stop time for the station that we are going to stop in 

find_stop_time:

	push			temp1
	push			temp2
	push			xl
	push			xh

	lds			temp1, n_stations				
	
			
	set_x			dwell_time_array			

	add			xl, temp1
	
	ld			temp1, x					; y = &station_name_array[next station index]
	sts			wait_time, temp1

	
	

	pop			xh
	pop			xl
	pop			temp2
	pop			temp1
	ret

; displays the name of the current station 

display_station_name:

	push			temp1
	push			temp2
	push			temp3
	push			yl
	push			yh

	lds			temp1, n_stations				
							; temp1 = curr_station_index + 1



	
	ldi			yl, low(station_name_array)
	ldi			yh, high(station_name_array)				
	ldi			temp2, STATION_NAME_SIZE				; temp2 = STATION_NAME_SIZE

	mul			temp1, temp2						; r1:r0 = temp1 * STATION_NAME_SIZE
	mov			temp1, r0
	mov			temp2, r1
	
	add			yl, temp1
	adc			yh, temp2						; y = &station_name_array[next station index]

	rcall			display_string_onto_lcd					; display the next station's name

	pop			yh
	pop			yl
	pop			temp3
	pop			temp2
	pop			temp1
	ret

; displays the name of the next station 

display_next_station_name:
	
	push			temp1
	push			temp2
	push			temp3
	push			yl
	push			yh

	clear_lcd_display

	
	clr temp1

	ldi			temp1, 'N'
	do_display_a_character	temp1
	ldi			temp1, 'E'
	do_display_a_character	temp1
	ldi			temp1, 'X'
	do_display_a_character	temp1
	ldi			temp1, 'T'
	do_display_a_character	temp1
	ldi			temp1, ' '
	do_display_a_character	temp1


	lds			temp1, n_stations	
	inc			temp1			
							
	ldi			yl, low(station_name_array)
	ldi			yh, high(station_name_array)				
	ldi			temp2, STATION_NAME_SIZE				; temp2 = STATION_NAME_SIZE

	

	

	mul			temp1, temp2						; r1:r0 = temp1 * STATION_NAME_SIZE
	mov			temp1, r0
	mov			temp2, r1
	
	add			yl, temp1
	adc			yh, temp2						; y = &station_name_array[next station index]

	
	
	
	rcall			display_string_onto_lcd					; display the next station's name

	pop			yh
	pop			yl
	pop			temp3
	pop			temp2
	pop			temp1
	ret

; the function show 'loop' when start to loop to the first station 
last_stop:

	push temp1

	clear_lcd_display

	ldi			temp1, 'L'
	do_display_a_character	temp1
	ldi			temp1, 'o'
	do_display_a_character	temp1
	ldi			temp1, 'o'
	do_display_a_character	temp1
	ldi			temp1, 'p'
	do_display_a_character	temp1
	

	pop temp1
	ret

display_string:
	push				temp1
	

display_string_loop_start:
	ld					temp1, z+						; temp1 = string[i]
	cpi					temp1, 0
	breq				display_string_loop_end			; if temp1 == '\0', goto display_string_loop_end
	do_display_a_character temp1
	rjmp				display_string_loop_start
display_string_loop_end:

	
	pop					temp1
	ret

display_pressed_digit_time:
	push			temp1

	ldi			temp1, 3							; temp1 = 3
	cp			row, temp1
	breq			check_if_zero_character						; if row == 3, goto check_if_zero_character_pressed

	cp			col, temp1
	breq			check_if_enter_pressed_time					; if col == 3, goto check_if_enter_pressed_time

	mov			temp1, row							; temp1 = row
	lsl			temp1								; temp1 = 2 * row
	add			temp1, row							; temp1 = 3 * row
	add			temp1, col							; temp1 = 3 * row + col
	inc			temp1								; temp1 = 3 * row + (col + 1)
	mov			return_val_l, temp1						; return value = 0 ... 9
	convert_digit_to_ascii	temp1
	do_display_a_character	temp1								; display the digit into lcd display
	rjmp			display_pressed_digit_time_end					; return

check_if_enter_pressed_time:
	ldi			temp1, 0							; temp1 = 0
	cp			row, temp1
	brne			display_pressed_digit_time_end					; if row != 0, goto display_pressed_digit_time_end
	ldi			return_val_l, ENTER_PRESSED					; else, set return_val_l = ENTER_PRESSED
	rjmp			display_pressed_digit_time_end					; return
check_if_zero_character:
	ldi			temp1, 1							; temp1 = 1
	cp			col, temp1
	brne			display_pressed_digit_time_end					; if col != 1, it is not valid character, so we just ignore it
	ldi			temp1, '0'							; display '0'
	do_display_a_character	temp1	
	ldi			return_val_l, 0; set return value = 0
display_pressed_digit_time_end:
	pop			temp1
	ret

; A function that display the pressed character in the keypad to the screen. This function return ENTER_PRESSED if the 'A' key is pressed on the keypad, ASTERISK_PRESSED
; if the '*' key is pressed, 'A' to 'Z' or ' ' dependinging on which character currently being pressed, or otherwise UNKNOWN_CHARACTER_PRESSED
; Note: This function need to synchronize with timer0
; Registers: temp1, temp2
; Arguments: prev_row, prev_col, last_typed_character, row, col
; Returns: return_val_l (either contain 'A'-'Z' or ' ', ASTERISK_PRESSED, ENTER_PRESSED or UNKNOWN_CHARACTER_PRESSED)
display_pressed_character_station_name:
	push			temp1
	push			temp2
	push			temp3

	ldi			return_val_l, UNKNOWN_CHARACTER_PRESSED			; initially, just assume UNKNOWN_CHARACTER_PRESSED
	
	lds			temp1, prev_row
	cp			temp1, row
	brne			relative_branch_resolve1				; if prev_row != row, goto new_character_pressed
	rjmp			relative_branch_not_needed1
relative_branch_resolve1:
	rjmp			new_character_pressed
relative_branch_not_needed1:
	lds			temp1, prev_col
	cp			temp1, col
	brne			relative_branch_resolve2				; if prev_col != col, goto new_character_pressed
	rjmp			relative_branch_not_needed2
relative_branch_resolve2:
	rjmp			new_character_pressed
relative_branch_not_needed2:
											; else if prev_col == col  && prev_row == row, we need to overwrite the previous
											; typed character with a new character. e.g.replaces 'A' with 'B'


	decrement_lcd_pointer_address	temp1

	lds			temp1, current_lcd_pointer_pos				; temp1 = current_lcd_pointer_pos
	cpi			temp1, MAXCHAR_DISPLAY + 1			
	brlt			if_no_shift_lcd_right					; if current_lcd_pointer_pos < current_lcd_pointer_pos
											; goto if_no_shift_lcd_right
	shift_lcd_right									; else, shift the lcd to the right.
if_no_shift_lcd_right:
	lds			temp1, current_lcd_pointer_pos
	dec			temp1
	sts			current_lcd_pointer_pos, temp1				; pre-decrement current_lcd_pointer_pos, since we are only overwriting the previous
											; character

	find_character_according_row_col	temp1, temp2				; temp1 will contain the first character corresponding to which key pressed
											; e.g temp1 can contain A, D, G, J, M, P, T, W
	lds			temp2, last_typed_character				; temp2 = last_typed_character

	sub			temp2, temp1						; temp2 = last_typed_character - the first character in the key
	inc			temp2							; temp2++
	check_row_col_zero	temp3							; temp1 = 1 if row & col is 0, otherwise temp1 = 0
	cpi			temp3, 1
	brne			if_row_col_not_zero
	modulus_one_byte_by	2, temp2						; temp2 %= 2
	rjmp			done_modulus_character					; goto done_modulus_character
if_row_col_not_zero:
	modulus_one_byte_by	3, temp2						; temp2 %= 3
done_modulus_character:

	add			temp2, temp1						; temp2 +=  the first character in the key

	do_display_a_character	temp2
	 
	sts			last_typed_character, temp2				; last_typed_character = temp2
	clear_two_bytes		num_timer0_ovf

	mov			return_val_l, temp2					; return_val_l = last_typed_character

	lds			temp1, curr_station_name_num_characters
	dec			temp1
	sts			curr_station_name_num_characters, temp1			; curr_station_name_num_characters-- (since we are only overwriting a character, not introducing
											; new character)

	rjmp			display_pressed_character_station_name_end


new_character_pressed:
	ldi			temp1, 3						; temp1 = 3
	cp			col, temp1
	breq			check_third_column1					; if col == 3, goto check_third_column1

	cp			row, temp1
	breq			check_third_row1_relative_branch_resolve		; if row == 3, goto check_third_row1
											; else, row and col is not 3, find the new chararacter to display
	rjmp			search_for_new_character_to_display

check_third_row1_relative_branch_resolve:
	rjmp			check_third_row1						
	

search_for_new_character_to_display:
	lds			temp1, curr_station_name_num_characters
	cpi			temp1, 20
	brsh			search_for_new_character_to_display_end			; if curr_station_name_num_characters >= 20, goto search_for_new_character_to_display_en
											; (since we are not allowed to exceed 20 character for single station name).
	find_character_according_row_col	temp1, temp2				; temp1 will contain the first character corresponding to which key pressed
											; e.g temp1 can contain A, D, G, J, M, P, S, V, Y

	do_display_a_character	temp1							; display the new character onto the lcd
		
	sts			last_typed_character, temp1				; last_typed_character = temp1 
	sts			prev_row, row						; prev_row = row
	sts			prev_col, col						; prev_col = col

	clear_two_bytes		num_timer0_ovf

	mov			return_val_l, temp1					; return_val_l = last_typed_character
search_for_new_character_to_display_end:
	rjmp			display_pressed_character_station_name_end

check_third_column1:
	ldi			temp1, 0
	cp			row, temp1
	brne			check_if_backspace_pressed
	ldi			return_val_l, ENTER_PRESSED				; return_l = ENTER_PRESSED
	rjmp			check_third_column1_end
check_if_backspace_pressed:
	ldi			temp1, 1
	cp			row, temp1
	brne			check_third_column1_end_relative_branch_resolve1	; if row != 1, goto check_third_column1_end	
	rjmp			check_third_column1_end_relative_branch_not_needed1
check_third_column1_end_relative_branch_resolve1:
	rjmp			check_third_column1_end
check_third_column1_end_relative_branch_not_needed1:

	ldi			return_val_l, BACKSPACE_PRESSED				; return_val_l = BACKSPACE_PRESSED
	
	lds			temp1, curr_station_name_num_characters
	cpi			temp1, 0
	breq			check_third_column1_end_relative_branch_resolve2	; if curr_station_name_num_characters == 0, goto check_third_column1_end
	rjmp			check_third_column1_end_relative_branch_not_needed2
check_third_column1_end_relative_branch_resolve2:
	rjmp			check_third_column1_end
check_third_column1_end_relative_branch_not_needed2:
											; othwewise, delete the last character from station_name_array and also delete the last character
											; on the lcd 

	lds			temp1, current_lcd_pointer_pos
	cpi			temp1, MAXCHAR_DISPLAY + 1
	brlt			decrements_lcd_pointer_pos_variable1			; if current_lcd_pointer_pos < MAXCHAR_DISPLAY + 1, goto decrements_lcd_pointer_pos_variable
		
	shift_lcd_right									; otherwise, shift lcd to the right first
decrements_lcd_pointer_pos_variable1:
	dec			temp1
	sts			current_lcd_pointer_pos, temp1				 ; current_lcd_pointer_pos--

	lds			temp1, curr_station_name_num_characters
	dec			temp1
	sts			curr_station_name_num_characters, temp1			; curr_station_name_num_characters--
	decrement_lcd_pointer_address temp1						; Note: temp1 value will be changed

	ldi			temp1, ' '
	do_display_a_character	temp1							; ovewrite the previous character with ' '

	lds			temp1, current_lcd_pointer_pos
	cpi			temp1, MAXCHAR_DISPLAY + 1
	brlt			decrements_lcd_pointer_pos_variable2			; if current_lcd_pointer_pos < MAXCHAR_DISPLAY + 1, goto decrements_lcd_pointer_pos_variable

	shift_lcd_right
decrements_lcd_pointer_pos_variable2:
	dec			temp1
	sts			current_lcd_pointer_pos, temp1				; current_lcd_pointer_pos-- (since when we display ' ', it increases the current_lcd_pointer_pos)

	ldi			temp1, KEYPAD_NOT_PRESSED
	sts			prev_col, temp1
	sts			prev_row, temp1
	sts			last_typed_character, temp1

	clear_two_bytes		num_timer0_ovf

	decrement_lcd_pointer_address temp1
check_third_column1_end:
	rjmp			display_pressed_character_station_name_end	

check_third_row1:
	ldi			temp1, 0						; temp1 = 0
	cp			col, temp1
	brne			check_space_key_pressed					; if col != 0, goto check_space_key_pressed
											; otherwise, we know that the ASTERISK key has been pressed
	ldi			return_val_l, ASTERISK_PRESSED				; return_val_l = ASTERISK_PRESSED
	rjmp			check_third_row1_end

check_space_key_pressed:
	ldi			temp1, 1
	cp			col, temp1
	brne			check_third_row1_end
space_key_pressed:
	ldi			temp1, ' '						; temp1 = ' '
	do_display_a_character	temp1							

	ldi			temp1, KEYPAD_NOT_PRESSED
	sts			last_typed_character, temp1				
	sts			prev_row, temp1						
	sts			prev_col, temp1						; last_typed_character, prev_row, prev_col = KEYPAD_NOT_PRESSED
	
	clear_two_bytes		num_timer0_ovf						; reset the timer0 count
	ldi			return_val_l, ' '					; return_val_l = ' '
check_third_row1_end:
	rjmp			display_pressed_character_station_name_end
display_pressed_character_station_name_end:

	pop			temp3
	pop			temp2
	pop			temp1
	ret

; A function that stores the current character being input into the current station name array according to curr_num_parameters and curr_station_name_num_characters.
; temp1 will store the character needed to be stored.
; Registers: temp2, temp3, xl, xh
; Arguments: curr_station_name_num_characters, station_name_array, temp1
; Return: -
store_curr_character_station_name:
	push			temp1
	push			temp2
	push			temp3
	push			temp4
	push			xl
	push			xh

	lds			temp2, curr_num_parameters				; temp2 = curr_num_parameters
	ldi			temp3, STATION_NAME_SIZE				; temp3 = STATION_NAME_SIZE
	mul			temp2, temp3						; r1:r0 = STATION_NAME_SIZE * curr_num_parameters

	mov			temp2, r0	
	mov			temp3, r1						; temp3:temp2 = STATION_NAME_SIZE * curr_num_parameters

	set_x			station_name_array
	add			xl, temp2
	adc			xh, temp3						; x = &station_name_array[curr_num_parameters]

	lds			temp2, curr_station_name_num_characters
	add			xl, temp2
	adc			xh, zero						; x = &station_name_array[curr_num_parameters][curr_station_num_characters]

	st			x, temp1						; station_name_array[curr_num_parameters][curr_station_num_characters] = temp1


	pop			xh
	pop			xl
	pop			temp4
	pop			temp3
	pop			temp2
	pop			temp1
	ret

; A function that stores curr_input_time into either travel_time_array/dwell_time_array according to curr_num_parameters
; Registers: temp1, xl, xh
; Arguments: curr_input_time, input_reading_stage
; Return: -
store_curr_input_time:
	push			temp1
	push			temp2
	push			xl
	push			xh

	lds			temp1, curr_input_time
	lds			temp2, input_reading_stage

	cpi			temp2, TRAVEL_TIME_READING
	brne			store_curr_check_if_dwell_time_reading			; if input_reading_stage != TRAVEL_TIME_READING, goto check_if_dwell_time_reading
	set_x			travel_time_array					; otherwise, store curr_input_time into travel_time_array corresponding to index
	lds			temp2, curr_num_parameters				; in curr_num_parameters
	add			xl, temp2
	adc			xh, zero
	st			x, temp1
	rjmp			store_curr_input_time_end
store_curr_check_if_dwell_time_reading:
	cpi			temp2, DWELL_TIME_READING
	brne			store_curr_input_time_end				; if input_reading_stage != DWELL_TIME_READING, goto store_curr_input_time_end
	set_x			dwell_time_array					; otherwise, store curr_input_time into dwell_time_array corresponding to index
	lds			temp2, curr_num_parameters				; in curr_num_parameters
	add			xl, temp2
	adc			xh, zero
	st			x, temp1
	rjmp			store_curr_input_time_end

store_curr_input_time_end:
	pop			xh
	pop			xl
	pop			temp2
	pop			temp1
	ret
	
; A function to update the value of curr_input_time as a new character is being input
; Essentially what it does is curr_input_time = curr_input_time * 10 + temp1, where temp1 is a number between 0...9
; Registers: temp2, temp3
; Arguments: temp1
; Return: -
update_curr_input_time:
	push			temp1
	push			temp2
	push			temp3
	lds			temp2, curr_input_time					; temp2 = curr_input_time
	ldi			temp3, 10						; temp3 = 10
	mul			temp2, temp3						; r1:r0 = curr_input_time * 10
	cp			zero, r1
	brsh			overflow_did_not_occured1				; if r1 <= 0, goto overflow_did_not_occured
	rcall			time_overflow_handler					; otherwise call the time_overflow_handler
	rjmp			update_curr_input_time_end

overflow_did_not_occured1:
	mov			temp2, r0						; temp2 = r0

	add			temp2, temp1						; temp2 = curr_input_time * 10 + temp1
	brcc			overflow_did_not_occured2
	rcall			time_overflow_handler					; otherwise call the time_overflow_handler
	rjmp			update_curr_input_time_end
overflow_did_not_occured2:
	sts			curr_input_time, temp2					; curr_input_time = curr_input_time * 10 + temp1

	
update_curr_input_time_end:
	pop			temp3
	pop			temp2
	pop			temp1
	ret

; A function that handle the case when current input dwell/travel time overflow (or the user input something exceeding 255 for the time).
; it displays "Time exceeded 255" onto the lcd for 3.5s and clear curr_input_time 
; Registers: temp1
; Arguments: -
; Return: -
time_overflow_handler:
	push			temp1
	clear_lcd_display

	ldi			temp1, 'T'
	do_display_a_character	temp1
	ldi			temp1, 'i'
	do_display_a_character	temp1
	ldi			temp1, 'm'
	do_display_a_character	temp1
	ldi			temp1, 'e'
	do_display_a_character	temp1
	ldi			temp1, ' '
	do_display_a_character	temp1
	ldi			temp1, 'e'
	do_display_a_character	temp1
	ldi			temp1, 'x'
	do_display_a_character	temp1
	ldi			temp1, 'c'
	do_display_a_character	temp1
	ldi			temp1, 'e'
	do_display_a_character	temp1
	ldi			temp1, 'e'						; display "Time exceeded 255" onto the lcd
	do_display_a_character	temp1
	ldi			temp1, 'd'
	do_display_a_character  temp1
	ldi			temp1, 'e'
	do_display_a_character  temp1
	ldi			temp1, 'd'
	do_display_a_character  temp1
	ldi			temp1, ' '
	do_display_a_character  temp1
	ldi			temp1, '2'
	do_display_a_character  temp1
	ldi			temp1, '5'
	do_display_a_character  temp1
	ldi			temp1, '5'
	do_display_a_character  temp1

	sts			curr_input_time, zero

	rcall			delay_three_half_seconds
	clear_lcd_display	
	pop			temp1
	ret



; Interrupt handler for Timer0 overflow. This interrupt handler is mainly used by they keypad system to allow user to type multiple character
; from a single key (e.g. key "2" allows user to enter 'A','B' and 'C').
Timer0OVF:
	push			temp1
	in			temp1, SREG
	push			temp1
	push			temp2
	push			temp3
	push			temp4

	lds			temp1, num_timer0_ovf
	lds			temp2, num_timer0_ovf + 1				; temp2:temp1 = num_timer0_ovf

	ldi			temp3, 1						; temp3 = 1
	add			temp1, temp3				
	adc			temp2, zero						; temp2:temp1 =  num_timer0_ovf + 1
	
	ldi			temp3, low(one_second_num_interrupts)
	ldi			temp4, high(one_second_num_interrupts)			; temp4:temp3 = one_second_num_interrupts

	cp			temp1, temp3
	cpc			temp2, temp4
	brlt			Timer0OVF_end						; if temp2:temp1 < one_second_num_interrupts, goto Timer0OVF_end
											; else 400ms has passed, reset prev_col, prev_row, last_typed_character 
	ldi			temp1, KEYPAD_NOT_PRESSED
	sts			prev_col, temp1
	sts			prev_row, temp1
	sts			last_typed_character, temp1				; set prev_col, prev_row, last_typed_character = KEYPAD_NOT_PRESSED indicating
											; the next key pressed will be a new character 
	clr			temp1
	clr			temp2							; temp2:temp1 = 0

Timer0OVF_end:
	sts			num_timer0_ovf, temp1
	sts			num_timer0_ovf + 1, temp2				; num_timer0_ovf = temp2:temp1
	
	pop			temp4
	pop			temp3
	pop			temp2
	pop			temp1
	out			SREG, temp1
	pop			temp1
	reti

; A function that initialize the timer0. It have pre-scaler of 64.
; Registers: temp1
; Arguments: -
; Returns: -
timer0_initialization:
	push			temp1
	
	sts			num_timer0_ovf, zero
	sts			num_timer0_ovf + 1, zero				; num_timer0_ovf = 0

	clr			temp1
	out			TCCR0A, temp1						; NOTE: TCCRnA is the control register for Timern

	ldi			temp1, 0b00000011																
	out			TCCR0B, temp1						; pre-scaler value is 64 (the cycle for timer0 runs 64 slower)

	lds			temp1, TIMSK0
	ori			temp1, (1 << TOIE0)					; POSSIBLE PROBLEM
	sts			TIMSK0, temp1						; enable Timer0 overflow interrupt
	pop			temp1
	ret		

; A function that stops timer0
; Registers: temp1
; Arguments: -
; Returns: -
timer0_stop:
	push			temp1
	push			temp2		

	ldi			temp1, (1 << TOIE0)					
	com			temp1
	lds			temp2, TIMSK0
	and			temp2, temp1						
	sts			TIMSK0, temp2		
				
	pop			temp2
	pop			temp1
	ret		


; A function that changes input_reading_stage variable from one stage to the next stage, showcased below.
; STATION_NAME_READING ->  TRAVEL_TIME_READING -> DWELL_TIME_READING -> STATION_NAME_READING -> ...
; Registers: temp1
; Arguments: input_reading_stage
; Returns: - (only changes input_reading_stage value)
change_input_reading_stage:
	push			temp1
	lds			temp1, input_reading_stage				; temp1 = input_reading_stage

	cpi			temp1, STATION_NAME_READING
	brne			check_if_travel_time_reading				; if input_reading_stage != STATION_NAME_READING, goto check_if_travel_time_reading
	ldi			temp1, TRAVEL_TIME_READING				
	sts			input_reading_stage, temp1				; else, input_reading_stage = TRAVEL_TIME_READING
	rjmp			change_input_reading_stage_end				; return

check_if_travel_time_reading:
	cpi			temp1, TRAVEL_TIME_READING				
	brne			check_if_dwell_time_reading				;  if input_reading_stage != TRAVEL_TIME_READING, goto check_if_dwell_time_reading
	ldi			temp1, DWELL_TIME_READING
	sts			input_reading_stage, temp1				; else, input_reading_stage, = DWELL_TIME_READING
	rjmp			change_input_reading_stage_end				; return

check_if_dwell_time_reading:
	; If we arrive at this stage, input_reading_stage should be DWELL_TIME_READING (no need to check)
	ldi			temp1, STATION_NAME_READING
	sts			input_reading_stage, temp1				; input_readign_stage = STATION_NAME_READING
change_input_reading_stage_end:
	pop			temp1
	ret

; A function that display a string into the lcd. The argument y will need to points to
; a valid starting addresss of a string
; IMPORTANT NOTE: it changes the value of y register
; Registers: temp1
; Arguments: y
; Returns: -
display_string_onto_lcd:
	push					temp1
display_string_onto_lcd_loop_start:
	ld					temp1, y+						; temp1 = string[i]
	cpi					temp1, 0
	breq					display_string_onto_lcd_loop_end			; if temp1 == '\0', goto display_string_loop_end
	do_display_a_character temp1
	rjmp					display_string_onto_lcd_loop_start
display_string_onto_lcd_loop_end:
	pop					temp1
	ret

	
; A function that compares two string pointed by x and y register. This function assumes
; x and y are pointing to a valid starting address of a string. Return 0 if matches, otherwise 1
; Registers:
; Arguments: x, y
; Return: returl_val_h
strcmp:
	push				temp1
	push				temp2
	push				xl
	push				xh
	push				yl
	push				yh

	ldi				return_val_h, 0						; initially assumes the string matches
strcmp_loop_start:
	ld				temp1, x+
	ld				temp2, y+

	cpi				temp1, 0
	breq				strcmp_loop_end
	cpi				temp2, 0
	breq				strcmp_loop_end						; if(x[i] == '\0'|| y[i] == '\0'), goto strcmp_loop_end

	cp				temp1, temp2
	breq				if_character_equal
	ldi				return_val_h, 1						; return value = 1
	rjmp				strcmp_end						; return
if_character_equal:
	rjmp				strcmp_loop_start
strcmp_loop_end:
	cp				temp1, temp2
	breq				strcmp_end						; if temp1 == temp2, goto strcmp_end (since one of them is guaranteed
												; to be '\0', if they are equal, they must be matching)
	ldi				return_val_h, 1						; else, they don't match, set return value = 1
strcmp_end:
	pop				yh
	pop				yl
	pop				xh
	pop				xl
	pop				temp2
	pop				temp1
	ret

; A function that set temp as 0xFF and decrements it until 0
; Registers: temp1
; Argument: -
; Return: -
delay:
	push			temp1
	ser			temp1
delay_loop:
	dec			temp1
	brne			delay_loop
	pop			temp1
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
	push		macro_r1
	clr		macro_r1
	out		DDRF, macro_r1						; set all port F as input to read the busy flag
	out		PORTF, macro_r1						; dismiss the pull-up resistor
	lcd_ctrl_set	LCD_RW							; set LCD to read mode
lcd_wait_loop:
	nop												
	lcd_ctrl_set	LCD_E							; set enable bit in lcd
	nop
	nop
	nop
	in		macro_r1, PINF						; read the data bits with BF from port F
	lcd_ctrl_clr	LCD_E							; disable enable bit in lcd
	sbrc		macro_r1, 7							; if busy flag is cleared (means not busy), skip next line
	rjmp		lcd_wait_loop

	lcd_ctrl_clr	LCD_RW							; set lcd into Instruction Register mode
	ser		macro_r1
	out		DDRF, macro_r1						; makes port F output again
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
	push		r23
	push		r24
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
	adc			temp2, r23					; r25:r24++

	rjmp		wait_1ms_loop_start
wait_1ms_loop_finish:
	pop			temp2
	pop			temp1
	pop			r25
	pop			r24
	pop			r23
	ret

; A function that waste approximately 120ms. Is right after someone presses a switch on the key pad
; Registers: temp1, temp2, r24, r25, r2
; Arguments: -
; Return: -
switch_delay:
; c = 65535 * 29 + 16 cycles
; total time wasted = c/f = 120ms
	push			r25
	push			r24
	push			temp1				; 2 cycles
	push			temp2				; 2 cycles
	push			r5

	clr			temp1				; 1 cycle
	clr			temp2				; 1 cycle

	ldi			r25, high(waste_n)		; 1 cycle
	ldi			r24, low(waste_n)		; 1 cycle

waste_loop_start:
	cp			temp1, r24			; waste_n cycle + 1
	cpc			temp2, r25			; waste_n cycle + 1
	breq			waste_loop_exit			; waste_n cycle + 2

	clr			r5					
	inc			r5				; r5 = 1
	add			temp1, r5			; waste_n cycle
	clr			r5
	adc			temp2, r5			; temp2:temp1 += 1 ; waste_n cycle

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
	nop							; 20  waste_n cycle

	rjmp			waste_loop_start		; 2  waste_n cycle
waste_loop_exit:
	pop			r5
	pop			temp2				; 2 cycles
	pop			temp1				; 2 cycles
	pop			r24
	pop			r25
	ret							; 4 cycle ; total of 9n + 16 	


////////////////////////////////////////////////////////// DEBUGGING FUNCTION //////////////////////////////////////////////////////////
; A function for purpse of debugging that displays all the inside of station_name_array, travel_time_array and dwell_time_array
display_all_arrays:
	push			temp1
	push			temp2
	push			temp3
	push			temp4
	push			yl
	push			yh

	ldi			temp1, 0						; temp1 = i = 0
	lds			temp2, curr_num_parameters				; temp2 = curr_num_parameters
display_all_arrays_loop_start:
	cp			temp1, temp2
	brge			display_all_array_relative_branch_resolve		; if i >= curr_num_parameters
	rjmp			display_all_array_relative_branch_resolve_not_needed
display_all_array_relative_branch_resolve:
	rjmp			display_all_arrays_loop_start_end
display_all_array_relative_branch_resolve_not_needed:
	ldi			temp3, STATION_NAME_SIZE				; temp3 = STATION_NAME_SIZE
	mul			temp1, temp3						; r1:r0 = STATION_NAME_SIZE * curr_num_parameters

	mov			temp3, r0	
	mov			temp4, r1						; temp4:temp3 = STATION_NAME_SIZE * curr_num_parameters

	set_y			station_name_array
	add			yl, temp3
	adc			yh, temp4						; y = &station_name_array[curr_num_parameters]

	rcall			display_string_onto_lcd

	rcall			delay_three_half_seconds
	clear_lcd_display

	set_y			travel_time_array
	add			yl, temp1
	adc			yh, zero

	ld			temp3, y
	ldi			temp4, 'A'
	add			temp3, temp4
	do_display_a_character	temp3

	rcall			delay_three_half_seconds
	clear_lcd_display

	set_y			dwell_time_array
	add			yl, temp1
	adc			yh, zero

	ld			temp3, y
	ldi			temp4, 'A'
	add			temp3, temp4
	do_display_a_character	temp3

	rcall			delay_three_half_seconds

	clear_lcd_display

	inc			temp1
	rjmp			display_all_arrays_loop_start
display_all_arrays_loop_start_end:
	pop			yh
	pop			yl
	pop			temp4
	pop			temp3
	pop			temp2
	pop			temp1
	ret
	
; A function that delays for approx 3.5s 
delay_three_half_seconds:
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay
	rcall			switch_delay							; delay approx 3.2s
	ret



			

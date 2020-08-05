;
; keypad.asm
;
; For keypad: R3-R0 to PL4-PL7 and C3-C0 to PL0-PL3
; For LCD: Pin F0-7 for LCD's D0-7. Pin A4-7 for LCD's BE-RS
;
;
; Created: 03/08/2020 12:05:41 PM
; Author : Feddrick Aquino
;
.include		"m2560def.inc"

.equ			MAXCHAR_DISPLAY = 16								; maximum character that can be contained on the lcd
.equ			KEYPAD_INIT =	0b11110000							; mask to set PORTL 
.equ			ROWMASK_INIT =	0b00000001
.equ			COLMASK_INIT =	0b11101111
.equ			ROWMASK	=	0b00001111							; mask to check whether one of the key is pressed in a column
.equ			KEYPAD_NOT_PRESSED = 5								; constant that describes the keypad hasn't been pressed yet
.equ			BF_POSITION = 7									; constant that contains the busy flag bit position 

.equ			delay1ms = 1776									; constant for wait_1ms

.equ			LCD_RS = 7
.equ			LCD_E = 6
.equ			LCD_RW = 5
.equ			LCD_BE = 4									; constant for LCD

.equ			waste_n = $FFFF									; constant for switch_delay

.equ			four_hundred_ms_num_interrupts = 977						; constant indicating how many time timer0 ovf 
													; should occur before 200ms has passed

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
	rcall		set_lcd_D_bits
	rcall		lcd_wait
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
	rcall		display_a_character
	rcall		lcd_wait
	pop		yh
	pop		yl
.endmacro

; macro to get the current address of the pointer in the lcd and store it onto @0
.macro do_get_lcd_pointer_address
	clr		macro_r1
	out		DDRF, macro_r1						; set all port F as input to read the address counter
	out		PORTF, macro_r1						; dismiss the pull-up resistor
	lcd_ctrl_set	LCD_RW							; set LCD to read mode
	nop							
	lcd_ctrl_set	LCD_E							; set enable bit in lcd
	nop
	nop
	nop
	in		macro_r1, PINF						; read the address counter from port F
	lcd_ctrl_clr	LCD_E							; disable enable bit in lcd
	nop
	nop
	nop
	lcd_ctrl_clr	LCD_RW							; set the R/W register in the lcd to 0 again
	ser		macro_r2
	out		DDRF, macro_r2						; makes port F output again

	mov		@0, macro_r1						; @0 = LCD DD-RAM addresss counter (with addition of busy flag)

	rcall		lcd_wait						; wait for lcd to not be busy, then return
.endmacro

; macro to store @0 into the DD-RAM address counter of the LCD 
.macro do_store_lcd_pointer_address
	sbr		@0, 0x80 						; ensure that the BF is set
	out		PORTF, @0						; by defaulte LCD_RW, LCD_RS = 0, so we can immediately write @0 into the D0-7 register
	lcd_ctrl_set	LCD_E
	nop
	nop
	nop
	lcd_ctrl_clr	LCD_E
	nop
	nop
	nop

	rcall		lcd_wait						; wait for lcd to not be busy, then return
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
	sts		current_lcd_pointer_pos, zero					; set the current_lcd_pointer_pos = 0
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

; macro to find the character corresponding to the key enterred in the keypad and the return value stored in @0
; Note: this function assumes @1 to be register between r16-r31
; e.g.	1. if (row, col) = (0, 1), this macro returns 'A' in @0
;	2. if (row, col) = (2, 1), this macro returns 'P' in @0
.macro		find_character_according_row_col
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


.dseg
.org			0x200

current_lcd_pointer_pos:		.byte	1					; a variable to keep track the current position of the lcd cursor

prev_row:				.byte	1
prev_col:				.byte	1					; prev row and col will store the previous key pressed onto the keypad
last_typed_character:			.byte	1					; a variable to store the last typed character on the LCD 

num_timer0_ovf:				.byte	2					; a variable to store how many time timer0 has overflowed

.cseg	
		
.org			0x000
jmp			RESET

.org			OVF0addr							; OVF0addr is the address of Timer0 Overflow Interrupt Vector
jmp			Timer0OVF			

RESET:
	ldi			temp1, high(RAMEND)
	out			SPH, temp1
	ldi			temp1, low(RAMEND)
	out			SPL, temp1						; initialize the stack pointer
	clr			zero							; zero = 0

lcd_initialization:
	set_x			current_lcd_pointer_pos
	st			x, zero							; current_lcd_pointer_pos = 0
	ser			temp1
	out			DDRF, temp1						; set port F as all output pin
	out			DDRA, temp1						; set port A as all output pin

	clr			temp1
	out			PORTF, temp1
	out			PORTA, temp1				

	do_set_lcd_D_bits	0b00110000
	rcall		wait_5ms
	do_set_lcd_D_bits	0b00110000
	rcall		wait_1ms
	do_set_lcd_D_bits	0b00110000
	do_set_lcd_D_bits	0b00111100						; 8 bits transfer, 2 no of line, and font '1'
	do_set_lcd_D_bits	0b00001000						; display OFF
	do_set_lcd_D_bits	0b00000001						; clear display
	do_set_lcd_D_bits	0b00000110						; incremental mode without screen shifting
	do_set_lcd_D_bits	0b00001110						; display cursor and no blink

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

	rcall			timer0_initialization					; turn on timer0 with pre-scaler of 64

	sei										; enable global interrupt (mainly for timer0OVF currently)
main:
	clr			col
	ldi			mask1, COLMASK_INIT					; mask = 0b11101111
col_loop:
	sts			PORTL, mask1
	rcall			delay
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

	cli										; clear global interrupt bit
	rcall			display_pressed_character				; display the pressed character onto LCD

	in			temp1, TIFR0
	cbr			temp1, 0x01
	out			TIFR0, temp1						; clear the timer0 ovf interrupt flag (not having this is okay, since it only affects
											; 1 counter increment in num_timer0_ovf)

	sei										; NOTE: it is important to make sure display_pressed_character won't be interrupted
											; by timer0 overflow

	rcall			switch_delay						; delay for approx 0.124sec
	rjmp			main							; continue scanning new pressed key

continue_scanning_row:
	inc			row							; row++
	lsl			mask1							; mask1 << 1
	rjmp			row_loop						; goto row_loop	
	
	
no_switch_pressed:
	rol			mask1							; update mask1
	inc			col							; col++
	ldi			temp3, 4						; temp3 = 4
	cp			col, temp3
	breq			relative_branch_resolve_main				; if col == 4, goto main
	rjmp			relative_branch_resolve_main_not_needed
relative_branch_resolve_main:
	rjmp			main
relative_branch_resolve_main_not_needed:
	rjmp			col_loop						; else, goto col_loop



; A function that display the pressed character in the keypad to the screen. Note: This function need to synchronize with timer0
; Registers: temp1, temp2
; Arguments: prev_row, prev_col, last_typed_character
; Returns: -
display_pressed_character:
	push			temp1
	push			temp2
	
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


	do_get_lcd_pointer_address  temp1						; temp1 contain the current pointer address in the LCD
	dec			temp1							; LCD pointer adddress--
	do_store_lcd_pointer_address temp1						; update the DD-RAM address counter in lcd

	lds			temp1, current_lcd_pointer_pos
	dec			temp1
	sts			current_lcd_pointer_pos, temp1				; pre-decrement current_lcd_pointer_pos, since we are only overwriting the previous
											; character

	find_character_according_row_col	temp1, temp2				; temp1 will contain the first character corresponding to which key pressed
											; e.g temp1 can contain A, D, G, J, M, P, T, W
	lds			temp2, last_typed_character				; temp2 = last_typed_character

	sub			temp2, temp1						; temp2 = last_typed_character - the first character in the key
	inc			temp2							; temp2++
	modulus_one_byte_by	3, temp2						; temp2 %= 3

	add			temp2, temp1						; temp2 +=  the first character in the key

	do_display_a_character	temp2
	 
	sts			last_typed_character, temp2				; last_typed_character = temp2
	sts			num_timer0_ovf, zero
	sts			num_timer0_ovf + 1, zero
	rjmp			display_pressed_character_end


new_character_pressed:
	mov			temp1, row
	cpi			temp1, 3
	breq			display_pressed_character_end				; if row == 3, goto display_pressed_character_end

	mov			temp1, col
	cpi			temp1, 3
	breq			display_pressed_character_end				; if col == 3, goto display_pressed_character_end
check_one_is_pressed:
	cpi			temp1, 0
	brne			search_for_new_character_to_display			; if col != 0, goto search_for_new_character_to_display

	mov			temp1, row
	cpi			temp1, 0
	brne			search_for_new_character_to_display			; if row != 0, goto search_for_new_character_to_display

	rjmp			display_pressed_character_end				; else, row == 0 && col == 0, we ignore the key "1"
search_for_new_character_to_display:
	find_character_according_row_col	temp1, temp2				; temp1 will contain the first character corresponding to which key pressed
											; e.g temp1 can contain A, D, G, J, M, P, T, W

	do_display_a_character	temp1							; display the new character onto the lcd
		
	sts			last_typed_character, temp1				; last_typed_character = temp1 
	sts			prev_row, row						; prev_row = row
	sts			prev_col, col						; prev_col = col

	sts			num_timer0_ovf, zero
	sts			num_timer0_ovf + 1, zero

display_pressed_character_end:

	pop			temp2
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
	
	ldi			temp3, low(four_hundred_ms_num_interrupts)
	ldi			temp4, high(four_hundred_ms_num_interrupts)		; temp4:temp3 = four_hundred_ms_num_interrupts

	cp			temp1, temp3
	cpc			temp2, temp4
	brlt			Timer0OVF_end						; if temp2:temp1 < four_hundred_ms_num_interrupts, goto Timer0OVF_end
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
	sbrc	macro_r1, 7							; if busy flag is cleared (means not busy), skip next line
	rjmp	lcd_wait_loop

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
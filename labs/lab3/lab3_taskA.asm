;
; lab3_taskA.asm
;
; A program that get its input (in the form of string) from host machine through USART0, and
; checks whether the given string matches one of our group member's name
; If there is a match, display the name onto the lcd and send the matching person's zID through serial monitor
; else, clear lcd display and sent "XXX is not in our group\n", where XXX is the string send by the host machine
;
; Created: 7/18/2020 9:32:13 PM
; Author : Feddrick Aquino
;
; Tested input: "Feddrick Aquino", "Ivan Huang", "Tavakol Farnaz", "Eric123", "Farnaz no", "Feddrick Aquinooo", "Feddrick Aquin", "\n"
;
.include "m2560def.inc"


.equ			NO_MATCHING_NAME = 5								; constant to indicate that there are no matching name 
.equ			MAXCHAR_DISPLAY = 16								; maximum character that can be contained on the lcd
.equ			FOSC = 16000000
.equ			BAUD = 9600
.equ			MYUBRR = (FOSC/16/BAUD-1)	
.equ			num_member = 3										; number of group member is 3
.equ			size_name_array = 20								; each row can contain at most 20 character (including null terminator)

.equ			delay1ms = 1776
.equ			LCD_RS = 7
.equ			LCD_E = 6
.equ			LCD_RW = 5
.equ			LCD_BE = 4											; constants for lcd stuff
			
.def			zero = r2
.def			macro_r1 = r16
.def			macro_r2 = r17
.def			usart_rdata = r20
.def			temp1 = r18
.def			temp2 = r19
.def			temp3 = r21



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

;USART_Init
.macro USART_Init
	ldi		macro_r1, high(MYUBRR)
	sts		UBRR0H, macro_r1
	ldi		macro_r1, low(MYUBRR)
	sts		UBRR0L, macro_r1							; set up the baud rate on the board

	ldi		macro_r1, (1<< TXEN0| 1 <<RXEN0)
	sts		UCSR0B, macro_r1							; enable transmitter and receiver

	ldi		macro_r1, (1<< USBS0|3 <<UCSZ00)			; set frame structure. 8 data bit and 2 stop bit
	sts		UCSR0C, macro_r1
.endmacro

.macro USART_Transmit
	;Wait for empty transmit buffer
USART_Transmit_loop:
	lds		macro_r1, UCSR0A
	sbrs	macro_r1, UDRE0
	rjmp	USART_Transmit_loop							; if USART0 data register is empty, put the data into the buffer

	sts		UDR0, @0

.endmacro

.macro USART_Receive
USART_Receiver_loop:
	lds		macro_r1, UCSR0A
	sbrs	macro_r1, RXC0
	rjmp	USART_Receiver_loop							; if USART0 data register is empty, put the data into the buffer

	lds		@0, UDR0
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

////////////////////////////MACROS END////////////////////////////////////

.dseg
.org				0x200

names_array: .byte	20 * 3								; a 2D array storing the name of our group members
id_array:	 .byte	20 * 3
usart_rdata_array: .byte 100							; an array to store the string received from the USART
not_in_group_string: .byte 21							; a string that contains " is not in our group" including the null terminator

current_lcd_pointer_pos : .byte	1						; a variable to keep track the current position of cursor on lcd

.cseg
.org				0x0000
rjmp				RESET

RESET:
	ldi				temp1, high(RAMEND)
	out				SPH, temp1
	ldi				temp1, low(RAMEND)
	out				SPL, temp1							; set up the stack

	clr				zero								; zero  = 0
USART_initialization:		
	USART_Init											; initialize the baud rate and enable the transmitter and receiver
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

array_initialization:
name_1:						.db	'F', 'e', 'd', 'd', 'r', 'i', 'c', 'k', ' ', 'A', 'q', 'u', 'i', 'n', 'o', '\0'
zid_1 :						.db	'5', '2', '3', '8', '2', '3', '6', '\0'
name_2:						.db	'I', 'v', 'a', 'n', ' ', 'H', 'u', 'a', 'n', 'g', '\0', '\0'
zid_2 :						.db	'1', '2', '3', '4', '5', '6', '7', '\0'
name_3:						.db	'T', 'a', 'v', 'a', 'k', 'o', 'l', ' ', 'F', 'a', 'r', 'n', 'a', 'z', '\0', '\0'
zid_3 :						.db	'7', '6', '5', '4', '3', '2', '1', '\0'
not_in_group_string_progam: .db ' ', 'i', 's', ' ', 'n', 'o', 't', ' ', 'i', 'n', ' ', 'o', 'u', 'r', ' ', 'g', 'r', 'o', 'u', 'p', '\0', '\0'

	
	move_string_program_to_data name_1, names_array
	move_string_program_to_data	zid_1, id_array

	move_string_program_to_data name_2, names_array + size_name_array
	move_string_program_to_data	zid_2, id_array + size_name_array

	move_string_program_to_data name_3, names_array + (size_name_array * 2)
	move_string_program_to_data	zid_3, id_array + (size_name_array * 2)

	move_string_program_to_data not_in_group_string_progam, not_in_group_string

	set_x			usart_rdata_array
read_string_from_USART_loop_start:
	USART_Receive	usart_rdata
	cpi				usart_rdata, '\n'
	breq			read_string_from_USART_loop_end		; if usart_rdata == '\n', goto read_string_from_USART_loop_end
	st				x+, usart_rdata						; else, store the character into usart_rdata_array and keep receiving from USART
	rjmp			read_string_from_USART_loop_start
read_string_from_USART_loop_end:
	st				x+, zero							; add '\0' to the end of the string
	rcall			check_name_valid					; call check_name_valid
	cpi				r24, NO_MATCHING_NAME
	brne			if_valid_name						; if r24 != NO_MATCHING_NAME, goto if_valid_name
	rcall			invalid_name_handler				; else, call the invalid_name_handler
	rjmp			read_string_from_USART_loop_start	; and start reading from USART0 again for another input
if_valid_name:
	mov				temp1, r24							; temp1 = index of the valid name
	rcall			valid_name_handler 
	rjmp			read_string_from_USART_loop_start	; in the case of valid name, call valid_name_handler and start reading another input
	

; A function that handle valid name case. This funtion displays the name of the valid 
; name onto lcd screen and transmit the corresponding zID to the host machine
; Note: temp1 = index of the valid name
; Registers: temp2, temp3
; Arguments: usart_rdata_array, temp1, id_array
; Return: - 
valid_name_handler:
	push			temp2
	push			temp3
	clear_lcd_display									; clear the lcd display
	set_y			usart_rdata_array
	rcall			display_string						; Display the valid name into lcd
	
	ldi				temp3,  size_name_array
	mov				temp2, temp1
	mul				temp2, temp3
	mov				temp2, r0							; temp2 = i * 20
			
	set_y			id_array
	add				yl, temp2
	adc				yh, zero							; y = &id_array[i]
	rcall			transmit_string						; transmit the corresponding zid string to USART0
	ldi				temp2, '\n'
	USART_Transmit  temp2								; transmit a new line character

	pop				temp3
	pop				temp2
	ret
		

; A function that handle invalid name case. This function basically
; transmit to USART0 the string "XXX is not in our group\n", where XXX is a string
; coming from usart_rdata_array (or the invalid name that we received).
; Registers: temp1
; Arguments: usart_rdata_array
; Return: -
invalid_name_handler:
	push			temp1

	clear_lcd_display									; clear lcd display
	set_y			usart_rdata_array
	rcall			transmit_string						; transmit to USART0 the string inside usart_rdata_array
	set_y			not_in_group_string
	rcall			transmit_string						; transmit to USART0 the string " is not in our group"
	ldi				temp1, '\n'
	USART_Transmit  temp1								; transmit to USART0 the new line character

	pop				temp1
	ret

; A function that display a string into the lcd. The argument y will need to points to
; a valid starting addresss of a string
; IMPORTANT NOTE: it changes the value of y register
; Registers: temp1
; Arguments: y
; Returns: -
display_string:
	push				temp1
display_string_loop_start:
	ld					temp1, y+						; temp1 = string[i]
	cpi					temp1, 0
	breq				display_string_loop_end			; if temp1 == '\0', goto display_string_loop_end
	do_display_a_character temp1
	rjmp				display_string_loop_start
display_string_loop_end:
	pop					temp1
	ret

; A function to transmit a string through the USART. The argument y will need to points
; to a valid starting address of a string
; IMPORTANT NOTE: it changes the value of y register
; Registers: temp1
; Arguments: y
; Returns: -
transmit_string:
	push				temp1
transmit_string_loop_start:
	ld					temp1, y+						; temp1 = string[i]
	cpi					temp1, 0
	breq				transmit_string_loop_end		; if temp1 == '\0', goto transmit_string_loop_end
	USART_Transmit		temp1
	rjmp				transmit_string_loop_start
transmit_string_loop_end:
	pop					temp1
	ret


; A function that checks whether there is a matching name with usart_rdata_array.
; If there is a matching name, it returns the index in names_array of that string 
; else, it return NO_MATCHING_NAME.
; Registers: temp1, temp2, temp3, x
; Argument: usart_rdata_array, names_array
; Return : r24
check_name_valid:
	push			temp1
	push			temp2
	push			temp3

	clr				temp1								; i = 0
	ldi				temp3, size_name_array				; temp3 = size_name_array

	ldi				r24, NO_MATCHING_NAME				; initially assume there are no matching name
	
check_name_valid_loop_start:
	cpi				temp1, num_member
	brsh			check_name_valid_loop_end			; if i >= 3, goto check_first_character_loop_end

	set_y			names_array

	mov				temp2, temp1
	mul				temp2, temp3
	mov				temp2, r0							; temp2 = i * 20

	add				yl, temp2
	adc				yh, zero							; x = names_array[i]
	
	set_x			usart_rdata_array

	rcall			strcmp								; strcmp compares the string pointed by x and y. Return 0 if matches, 1 otherwise.
	cpi				r25, 0
	brne			if_usart_rdata_not_match			; if strcmp(usart_rdata_array, names_array[i]) !=  0, goto if_usart_rdata_not_match
	mov				r24, temp1							; else if it matches, set return value = i, and return
	rjmp			check_name_valid_loop_end			
if_usart_rdata_not_match:
	inc				temp1
	rjmp			check_name_valid_loop_start	
check_name_valid_loop_end:
	pop				temp3
	pop				temp2
	pop				temp1
	ret
	

; A function that compares two string pointed by x and y register. This function assumes
; x and y are pointing to a valid starting address of a string. Return 0 if matches, otherwise 1
; Registers:
; Arguments: x, y
; Return: r25
strcmp:
	push			temp1
	push			temp2
	push			xl
	push			xh
	push			yl
	push			yh

	ldi				r25, 0								; initially assumes the string matches
strcmp_loop_start:
	ld				temp1, x+
	ld				temp2, y+

	cpi				temp1, 0
	breq			strcmp_loop_end
	cpi				temp2, 0
	breq			strcmp_loop_end						; if(x[i] == '\0'|| y[i] == '\0'), goto strcmp_loop_end

	cp				temp1, temp2
	breq			if_character_equal
	ldi				r25, 1								; return value = 1
	rjmp			strcmp_end							; return
if_character_equal:
	rjmp			strcmp_loop_start
strcmp_loop_end:
	cp				temp1, temp2
	breq			strcmp_end							; if temp1 == temp2, goto strcmp_end (since one of them is guaranteed
														; to be '\0', if they are equal, they must be matching)
	ldi				r25, 1								; else, they don't match, set return value = 1
strcmp_end:
	pop				yh
	pop				yl
	pop				xh
	pop				xl
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


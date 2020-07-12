;
; lab2_taskD.asm
;
; Note: LCD D0-7 is connected to PF0-7; LCD control pin BE-RS is connected to PA4-7
; For keypad: R3-R0 to PL4-PL7 and C3-C0 to PL0-PL3
;
; Created: 07/07/2020 12:05:41 PM
; Author : Feddrick Aquino
;

.include		"m2560def.inc"

.equ			MAXCHAR_DISPLAY = 16								; maximum character that can be contained on the lcd
.equ			PLUS_SIGN = $0B
.equ			MINUS_SIGN = $0A
.equ			EQUAL_SIGN = $0C
.equ			KEYPAD_INIT =	0b11110000							; mask to set PORTL 
.equ			ROWMASK_INIT =	0b00000001
.equ			COLMASK_INIT =	0b11101111
.equ			ROWMASK	=		0b00001111							; mask to check whether one of the key is pressed in a column
.equ			delay1ms = 1776
.equ			LCD_RS = 7
.equ			LCD_E = 6
.equ			LCD_RW = 5
.equ			LCD_BE = 4
.equ			waste_n = $FFFF

.def			temp3 = r2
.def			temp4 = r3
.def			flag_number_exist = r4	
.def			zero = r5											; r6 and r7 reserved for macro usage
.def			temp5 = r8

.def			temp1 = r16
.def			temp2 = r17
.def			row = r18
.def			col = r19											; r20 and r21 reserved for macro usage
.def			mask = r22
.def			mask2 = r23

/////////////////////////////////////////
; macro to set x register points to @0
.macro		set_x
	ldi		xh, high(@0)
	ldi		xl, low(@0)
.endmacro

; macro to clear 2 byte on data memory
.macro		clear_two_bytes
	ldi		xh, high(@0)
	ldi		xl, low(@0)
	clr		r20
	st		x+, r20
	st		x+, r20
.endmacro

; macro to set the DB7-DB0 in the lcd
.macro		do_set_lcd_D_bits
	ldi		r20, @0
	rcall	set_lcd_D_bits
	rcall	lcd_wait
.endmacro

; macro to set display an ascii character into lcd screen
; It also checks if the current cursor pointing somewhere 
; out of display, shift the screen to the left
.macro		do_display_a_character
	ldi		yl, low(current_lcd_pointer_pos)
	ldi		yh, high(current_lcd_pointer_pos)
	ld		r20, y
	cpi		r20, MAXCHAR_DISPLAY									
	brlo	if_no_shift_lcd_left									; if the current lcd pointer points out of
	shift_lcd_left													; the screen, shift lcd screen to the left
if_no_shift_lcd_left:
	inc		r20
	st		y, r20
	mov		r20, @0
	rcall	display_a_character
	rcall	lcd_wait
.endmacro

; macro to shift the lcd display to the left
.macro		shift_lcd_left
	do_set_lcd_D_bits 0b00011000
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

; macro to divide @1:@0 by ten and stores the result in @1:@0
; It keeps subtracting 10 from @1:@0, and everytime it subtracts,
; it increment r7:r6 by one
.macro		divide_two_bytes_by_ten					
	ldi		r20, 10						; r20 = 10			
	clr		r6							
	clr		r7							; r7:r6 will store the result of (@1:@0)/10
dbt_two_bytes_loop_start:
	ldi		r20, 10
	cp		@0, r20	
	cpc		@1, zero					; sign extend r20 with zeros
	brlo	dbt_two_bytes_loop_finishes			; if @1:@0 <= 10, goto dbt_loop_finishes
	sub		@0, r20						
	sbci	@1, 0						; @1:@0 -= 10
	ldi		r20, 1						; r20 = 1
	add		r6, r20					
	adc		r7, zero					; r7:r6++
	rjmp	dbt_two_bytes_loop_start				
dbt_two_bytes_loop_finishes:
	mov		@0, r6
	mov		@1, r7
.endmacro

; macro to modulus @1:@0 by ten and stores the result in @1:@0
; It keeps subtracting 10 from @1:@0 until @1:@0 is lower than 10
.macro		modulo_two_bytes_by_ten
	ldi		r20, 10						; r20 = 10			
mbt_two_bytes_loop_start:
	cp		@0, r20	
	cpc		@1, zero					; sign-extend 10 with zeros
	brlo	mbt_two_bytes_loop_finishes	; if @1:@0 <= 10, goto dbt_loop_finishes
	sub		@0, r20						
	sbci	@1, 0						; @1:@0 -= 10
	rjmp	mbt_two_bytes_loop_start				
mbt_two_bytes_loop_finishes:
.endmacro

; macro to convert a SINGLE digit character in @0 to ascii
; and store the result into @0
.macro		convert_digit_to_ascii
	ldi		r20, '0'
	add		@0, r20
.endmacro

; macro to check whether overflow happens by checking
; whether carry flag in SREG is set/cleared. If it is set
; call the overflow function
.macro		check_overflow_carry
	in		r20, SREG
	sbrc	r20, SREG_C													; skip the next line if the Carry flag is not set
	rcall	overflow_handler											; if carry is set, overflow happened, call overflow_handler
.endmacro
/////////////////END MACROS////////////////////

.dseg
.org			0x200

result1:			.byte	2											; to store the result of expression
result2:			.byte	2
previous_operator:	.byte	1											; to store the value of previous operator
current_lcd_pointer_pos:		.byte	1								; to keep track of pointer position in lcd

.cseg
.org			0x000
jmp				RESET

RESET:
	ldi			temp1, high(RAMEND)
	out			SPH, temp1
	ldi			temp1, low(RAMEND)
	out			SPL, temp1								; initialize the stack pointer
	clr			zero									; zero = 0

lcd_initialization:
	set_x		current_lcd_pointer_pos
	st			x, zero									; current_lcd_pointer_pos = 0
	ser			temp1
	out			DDRF, temp1								; set port F as all output pin
	out			DDRA, temp1								; set port A as all output pin

	clr			temp1
	out			PORTF, temp1
	out			PORTA, temp1				

	do_set_lcd_D_bits	0b00110000
	rcall		wait_5ms
	do_set_lcd_D_bits	0b00110000
	rcall		wait_1ms
	do_set_lcd_D_bits	0b00110000
	do_set_lcd_D_bits	0b00111100					; 8 bits transfer, 2 no of line, and font '1'
	do_set_lcd_D_bits	0b00001000					; display OFF
	do_set_lcd_D_bits	0b00000001					; clear display
	do_set_lcd_D_bits	0b00000110					; incremental mode without screen shifting
	do_set_lcd_D_bits	0b00001110					; display cursor and no blink

keypad_initialization:
	ldi			temp1, KEYPAD_INIT
	sts			DDRL, temp1							; set R0-R3 as input, C0-C3 as output
	clear_two_bytes		result1
	clear_two_bytes		result2						; set result1 = result2 = 0
	clr			flag_number_exist					; intially there are no number BEING inputted

	;ser			temp1
	;sts			PORTF, temp1					;  enable R0-R3 pull up resistor. Open all C0-C3 switch
	ldi			temp1, PLUS_SIGN
	set_x		previous_operator
	st			x, temp1							; by default, set previous_operator to be '+'
main:
	ldi			mask, COLMASK_INIT
	clr			col									; col = 0
col_loop:
	sts			PORTL, mask							; close one of the column switch
	rcall		delay								; call delay function stabilize hardware
	lds			temp1, PINL							; read the current switch state of the column
	andi		temp1, ROWMASK						; temp1 = state of the column & 0x0F
	cpi			temp1, ROWMASK							
	breq		no_switch_pressed					; if temp1 == 0x0F, goto no_switch_pressed:
	clr			row									; row = 0
	ldi			mask2, ROWMASK_INIT					; mask2 = 0x01
search_which_row_pressed:
	mov			temp2, temp1						; restore original switch state of the column
	and			temp2, mask2						; temp1 &= mask2 (where mask2 contains 0 only on the current row)	
	cpi			temp2, 0					
	breq		found_switch_pressed				; if temp1 == 0, goto found_switch_pressed
	inc			row									; else if switch hasn't been found yet, increment row count
	lsl			mask2								; shift the rowmask to the left
	rjmp		search_which_row_pressed			; go check the next row
found_switch_pressed:
	rcall		check_key_pressed_valid				; return 0/1 into r25 if key pressed is invalid/valid respectively
	cpi			r25, 0
	breq		continue_scanning					; if key pressed is invalid, continue scanning character
	rcall		display_pressed_character			; call a function to display a character
	cpi			r24, MINUS_SIGN
	brge		check_whether_operate_on_result_needed				; if r24 >= MINUS_SIGN, goto check_wheter_operate_on_result_needed
	rcall		update_current_value				; else, we update the current value of result2 according to digit inputted
	rjmp		continue_scanning
check_whether_operate_on_result_needed:
	; first we check whether the syntax of the operation is valid
	cpi			r24, EQUAL_SIGN
	brne		do_subtraction_addition_operation	; if r24 != EQUAL_SIGN, goto do_subtraction_addition_operation
	clr			temp3								; else check whether flag_number_exist == 1
	inc			temp3								; if yes, syntax is valid, display the final result
	cp			flag_number_exist, temp3			; else syntax is not valid, call the incorrect expression handler
	breq		do_subtraction_addition_operation
	rcall		incorrect_expression_handler		; call incorrect expression handler
do_subtraction_addition_operation:
	clr			temp3
	inc			temp3								; temp3 = 1
	cp			flag_number_exist, temp3
	brne		operate_on_result_finish			; if flag_number_exist != 1, don't do any operation
	rcall		operate_on_result_function
	clr			flag_number_exist					; clear flag_number_exist to indicate that there are no current number
													; being inputted
operate_on_result_finish:
	cpi			r24, EQUAL_SIGN
	breq		go_display_final_result				; if equal sign is inputted, goto go_display_final_result
	clr			temp3
	inc			temp3								; temp3 = 1
	cp			flag_number_exist, temp3
	breq		overwrite_previous_operation		; if flag_number_exists == 1, we just overwrite the previous operators
	rcall		update_previous_operator_accordingly			; else, we need to consider how the - or + interracts i.e. --1 = 1 or 1+---1 = 0
	rjmp		continue_scanning
overwrite_previous_operation:
	set_x		previous_operator					 
	st			x+, r24								; after executing previous operator, store the current operator
	rjmp		continue_scanning					; if r24 != EQUAL_SIGN, we continue scanning
go_display_final_result:
	rcall		display_final_result				; else we display the result (since an '=' sign is pressed)
continue_scanning:
	rcall		switch_delay						
	rcall		switch_delay						; delay approximately 0.25s to avoid pressing too long problem
	rjmp		main	
no_switch_pressed:
	inc			col
	cpi			col, 4
	brlo		continue_col_loop					; if col <=3, goto cotinue_col_loop
	rjmp		main								; else, goto main to start the loop again
continue_col_loop:
	sec												; set the carry bit
	rol			mask								; shift the col mask to the left
	rjmp		col_loop							; jump to col_loop

; A function that updates previous_operator accordingly with operator rules
; i.e.	1. if previous_operator = '-' and r24 = '-', the new operator becomes '+'
;		2. if previous_operator = '-' and r24 = '+', the new operator is still '-' 
; Registers: temp1
; Arguments: previous_operator, r24
; Returns: previous_operator (new value stored back into data memory)
update_previous_operator_accordingly:
	push		temp1
	set_x		previous_operator
	ld			temp1, x
	cpi			temp1, MINUS_SIGN
	breq		if_previous_operator_minus			; if previous_operator == MINUS_SIGN, goto if_previous_operator_minus
													; else, we are now dealing with previous_operator = PLUS_SIGN
	cpi			r24, PLUS_SIGN
	breq		update_previous_operator_accordingly_end	; if r24 == PLUS_SIGN, goto update_previous_operator_accordingly_end (do nothing)
															; else, set previous_operator = MINUS_SIGN
	ldi			temp1, MINUS_SIGN							; previous_operator = MINUS_SIGN			
	rjmp		update_previous_operator_accordingly_end		
if_previous_operator_minus:
	cpi			r24, PLUS_SIGN
	breq		update_previous_operator_accordingly_end	; if r24 == PLUS_SIGN,  goto update_previous_operator_accordingly_end (do nothing)

	ldi			temp1, PLUS_SIGN							; else, set previous_operator = PLUS_SIGN
update_previous_operator_accordingly_end:
	st			x, temp1
	pop			temp1
	ret


; A function that display the final result onto the screen
; usually called after '=' is pressed
; Registers: r18, r19, temp1, temp2, temp3, temp4, temp5
; Arguments: result1
; Returns: -
display_final_result:
	push		r18
	push		r19
	push		temp1
	push		temp2
	push		temp3
	push		temp4
	push		temp5

	clr			temp3								; temp3 is used to store the number of digit result1 have
	inc			temp3								; temp3 = 1
	set_x		result1
	ld			temp1, x+
	ld			temp2, x+							; temp2:temp1 = result1

	mov			r18, temp1
	mov			r19, temp2							; r19:r18 = result1

	cp			temp1, zero
	cpc			temp2, zero
	brge		divide_result1_by_ten				; if temp2:temp1 >= 0, goto divide_result1_by_ten

	ldi			r18, '-'							; else, we need to display a '-' sign, and do two's complement on result1
	do_display_a_character r18						; display the '-' sign

	ldi			r18, 1
	com			temp1
	com			temp2
	add			temp1, r18
	adc			temp2, zero							; temp2:temp1 = two's complement(result1)

	mov			r18, temp1
	mov			r19, temp2							; r19:r18 = two's complement(result1)
												
divide_result1_by_ten:
	divide_two_bytes_by_ten temp1, temp2			; temp2:temp1 /= 10
	cp			temp1, zero
	cpc			temp2, zero
	breq		if_know_how_many_digit				; if temp2:temp1 == 0, goto if_know_how_many_digit
	inc			temp3
	rjmp		divide_result1_by_ten				
if_know_how_many_digit:
	clr			temp5
	inc			temp5								; temp5 = 1
	; In this case, temp3 = i and temp4 = j
display_digit_outer_loop_start:
	cp			temp3, temp5
	brlo		display_digit_outer_loop_end		; if i < 1, goto display_digit_outer_loop_end

	mov			temp4, temp3						
	dec			temp4								; j = i - 1

	mov			temp1, r18
	mov			temp2, r19							; temp2:temp1 = result1 or two's complement(result1)
													; temp2:temp1 contains the second result if only the original result1 is negative
display_digit_inner_loop_start:
	cp			temp4, temp5						
	brlo		display_digit_inner_loop_end		; if j < 1, goto display_digit_inner_loop_end

	divide_two_bytes_by_ten temp1, temp2			; temp2:temp1 /= 10
	dec			temp4								; j--
	rjmp		display_digit_inner_loop_start
display_digit_inner_loop_end:
	modulo_two_bytes_by_ten temp1, temp2			; temp2:temp1 %= 10
	convert_digit_to_ascii temp1					; convert temp1 into ascii
	do_display_a_character	temp1					; display the character onto the screen
	dec			temp3
	rjmp		display_digit_outer_loop_start
display_digit_outer_loop_end:
	pop			temp5
	pop			temp4
	pop			temp3
	pop			temp2
	pop			temp1
	pop			r21
	pop			r20
	ret

; A function to check whethet the key that is pressed is valid
; It returns 0/1 into r25 if the key is invalid/valid respectively
; Registers: r25
; Arguments: row, col
; Returns: r25
check_key_pressed_valid:
	; initially assume it is valid
	ldi			r25, 1
	cpi			row, 3
	brne		check_key_pressed_valid_end							; if key pressed is not on row 3, it is a valid key
	cpi			col, 1
	breq		check_key_pressed_valid_end							; if key pressed is on row 3  && col 1, it is a valid key
	ldi			r25, 0												; else, key is invalid, set r25 to 0
check_key_pressed_valid_end:
	ret

; A function that display the pressed character into lcd display
; It returns value between the number 0 - 12
; Registers: temp1
; Arguments: row, col
; Returns: r24
display_pressed_character:
	push		temp1

	cpi			row, 3
	breq		zero_character_midway
	cpi			col, 3
	breq		operator_character					; if col == 3, goto operator_character

	mov			temp1, row							; temp1 = row
	lsl			temp1								; temp1 = 2 * row
	add			temp1, row							; temp1 = 3 * row
	add			temp1, col							; temp1 = 3 * row + col
	inc			temp1								; temp1 = 3 * row + (col + 1)
	mov			r24, temp1							; return value = 0 ... 9
	convert_digit_to_ascii temp1
	do_display_a_character temp1					; display the digit into lcd display
	rjmp		display_pressed_character_finish
zero_character_midway:
	rjmp		zero_character						; added here because of relative branch out of reach 
operator_character:
	cpi			row, 0
	brne		add_character						; if row != 0, goto add_character
	ldi			temp1, '-'
	do_display_a_character temp1					; display '-' sign
	ldi			r24, MINUS_SIGN							; return value = 10 (indicates '-')
	rjmp		display_pressed_character_finish
add_character:
	cpi			row, 1
	brne		equal_character						; if row != 1, goto equal_character
	ldi			temp1, '+'
	do_display_a_character temp1
	ldi			r24, PLUS_SIGN						; return value = 11 (indicates '+')
	rjmp		display_pressed_character_finish
equal_character:
	cpi			row, 2
	brne		display_pressed_character_finish	; if row != 2, goto display_pressed_character_finish
	ldi			temp1, '='
	do_display_a_character temp1					; case for '='
	ldi			r24, EQUAL_SIGN						; return value = 12 (indicates '=')
	rjmp		display_pressed_character_finish	; display '='
zero_character:
	cpi			col, 1
	brne		display_pressed_character_finish	; if row = 3, but col != 1, it is not valid character, so we just ignore it
	ldi			temp1, '0'							; display '0'
	do_display_a_character temp1	
	ldi			r24, 0								; set return value = 0
display_pressed_character_finish:
	pop			temp1
	ret

; A function to update the value of result1 as a new character is being input
; Essentially what it does is result1 = result1 * 10 + r24, where r24 is a number between 0...9
; Also update the flag_number_exist to 1 to indicate there a currently a number being inputted
; Registers: temp1, temp2, temp3, temp4, xl, xh
; Arguments: r24, result2, flag_number_exist
; Return: -
update_current_value:
	push		temp1
	push		temp2
	push		temp3
	push		temp4

	ldi			temp1, 1
	cp			flag_number_exist, temp1
	breq		no_update_flag_number_exist			; if flag_number_exist == 1, don't need to update it 
	inc			flag_number_exist					; update flag to indicates a number exist
no_update_flag_number_exist:
	set_x		result2								; set x points to result2
	ld			temp3, x+
	ld			temp4, x+							; temp4:temp3 = current intermediate value
	ldi			temp1, 10							; temp1 = 10
													; temp4:temp3 * 10 = a*2^16 + (b+c)*2^8 + d 
	mul			temp3, temp1						; r1:r0 = c*2^8:d
	mov			temp2, r1							; temp2 = c*2^8
	mov			temp3, r0							; temp3 = d
	mul			temp4, temp1						; r1:r0 = a*2^16 + b*2^8
check_overflow1:
	cp			r1, zero
	breq		check_overflow2						; if r1 == 0, goto check_overflow2
	rcall		overflow_handler					; else, overflow occured. Call overflow_handler
check_overflow2:
	mov			temp4, r0							; temp4 = b * 2^8
	add			temp4, temp2						; temp4 = (b+c) * 2^8
	check_overflow_carry
check_overflow3:
	ldi			temp1, 0
	add			temp3, r24
	adc			temp4, temp1						; In the end: temp4:temp3 = temp4:temp3 * 10 + r24
	check_overflow_carry
	rcall		check_overflow_function
	set_x		result2
	st			x+, temp3
	st			x+, temp4

	pop			temp4
	pop			temp3
	pop			temp2
	pop			temp1
	ret
	
; A function that checks whether temp4:temp3 overflow-ed according to previous_operator.
; call the overflow handler function if an overflow happens.
; Registers: temp1, temp2, xl, xh
; Arguments: temp4, temp3
; Return: -
check_overflow_function:
	push		temp1
	push		temp2
	set_x		previous_operator
	ld			temp1, x												; temp1 = previous_operator
	cpi			temp1, MINUS_SIGN
	brne		set_overflow_positive_comparator						; if temp1 != MINUS_SIGN, goto set_overflow_positive_comparator
	ldi			temp2, $80												; else set the comparator value for negative overflow
	ldi			temp1, 1
	rjmp		check_overflow_occured									; temp2:temp1 = $8001 (for negative)
set_overflow_positive_comparator:
	ldi			temp2, $80												
	ldi			temp1, 0												; temp2:temp1 = $8000 (for positive)
check_overflow_occured:
	cp			temp3, temp1
	cpc			temp4, temp2		
	brlo		check_overflow_function_end								; if temp4:temp3 < $8001, goto check_overflow_function_end
	rcall		overflow_handler										; else, overflow happens, and we need to call overflow handler
check_overflow_function_end:
	pop			temp2
	pop			temp1
	ret

; A function that handles overflow. Writes "Overflow occurred" onto the lcd screen.
; User need to use the RESET button to use the calculator again
; Registers: temp1, xl, xh
; Argument: -
; Returns: Never (sit on a never ending loop)
overflow_handler:
	clear_lcd_display
	set_x		current_lcd_pointer_pos
	clr			temp1
	st			x, temp1												; current_lcd_pointer_pos = 0
	ldi			temp1, 'O'
	do_display_a_character	temp1
	ldi			temp1, 'v'
	do_display_a_character	temp1
	ldi			temp1, 'e'
	do_display_a_character	temp1
	ldi			temp1, 'r'
	do_display_a_character	temp1
	ldi			temp1, 'f'
	do_display_a_character	temp1
	ldi			temp1, 'l'
	do_display_a_character	temp1
	ldi			temp1, 'o'
	do_display_a_character	temp1
	ldi			temp1, 'w'
	do_display_a_character	temp1
	ldi			temp1, ' '
	do_display_a_character	temp1
	ldi			temp1, 'o'
	do_display_a_character	temp1
	ldi			temp1, 'c'
	do_display_a_character  temp1
	ldi			temp1, 'c'
	do_display_a_character  temp1
	ldi			temp1, 'u'
	do_display_a_character  temp1
	ldi			temp1, 'r'
	do_display_a_character  temp1
	ldi			temp1, 'e'
	do_display_a_character  temp1
	ldi			temp1, 'd'
	do_display_a_character  temp1
overflow_loop_infinitely:
	rjmp		overflow_loop_infinitely

; A function that prints out "Incorrect expression" and then sit on an infinite_loop
; Registers: temp1
; Arguments: -
; Returns:  Never (sit on a never ending loop)
incorrect_expression_handler:
	ldi			temp1, 'I'
	do_display_a_character	temp1
	ldi			temp1, 'n'
	do_display_a_character	temp1
	ldi			temp1, 'c'
	do_display_a_character	temp1
	ldi			temp1, 'o'
	do_display_a_character	temp1
	ldi			temp1, 'r'
	do_display_a_character	temp1
	ldi			temp1, 'r'
	do_display_a_character	temp1
	ldi			temp1, 'e'
	do_display_a_character	temp1
	ldi			temp1, 'c'
	do_display_a_character	temp1
	ldi			temp1, 't'
	do_display_a_character	temp1
	ldi			temp1, ' '
	do_display_a_character	temp1
	ldi			temp1, 'e'
	do_display_a_character  temp1
	ldi			temp1, 'x'
	do_display_a_character  temp1
	ldi			temp1, 'p'
	do_display_a_character  temp1
	ldi			temp1, 'r'
	do_display_a_character  temp1
	ldi			temp1, 'e'
	do_display_a_character  temp1
	ldi			temp1, 's'
	do_display_a_character  temp1
	ldi			temp1, 's'
	do_display_a_character  temp1
	ldi			temp1, 'i'
	do_display_a_character  temp1
	ldi			temp1, 'o'
	do_display_a_character  temp1
	ldi			temp1, 'n'
	do_display_a_character  temp1
incorrect_expression_loop_infinitely:
	rjmp		incorrect_expression_loop_infinitely
			
; A function that adds/subtract depending on argument given
; Registers: temp1, temp2, temp3, temp4, r25
; Arguments: previous_operator, result1, result2
; Return: -
operate_on_result_function:
	push		r25
	push		temp1
	push		temp2
	push		temp3
	push		temp4

	set_x		previous_operator
	ld			r25, x+										; set r25 to contain either subtraction or addition (10 or 11 respectively)
	set_x		result2
	ld			temp1, x+
	ld			temp2, x+									; temp2:temp1 = result2
	cpi			r25, MINUS_SIGN		
	brne		add_result1_result2							; if r25 != MINUS_SIGN, goto add_result1_result2
	com			temp1										; else, do one's complement for temp2:temp1, and add one to them
	com			temp2
	clr			temp3										; temp3 = 0
	inc			temp3										; temp3 += 1
	add			temp1, temp3
	adc			temp2, zero									; temp2:temp1 += 1
add_result1_result2:
	set_x		result1
	ld			temp3, x+
	ld			temp4, x+
	add			temp3, temp1
	adc			temp4, temp2								; temp4:temp3 = result1 + result2
	brvc		overflow_did_not_occured
	rcall		overflow_handler
overflow_did_not_occured:
	sbiw		xl, 2
	st			x+, temp3
	st			x+, temp4									; store the result1+result2 value back to result1
	clear_two_bytes	 result2								; result2 = 0

	pop			temp4
	pop			temp3
	pop			temp2
	pop			temp1
	pop			r25
	ret


	
	

; Display the character in r20 to the lcd
; Registers: r20
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
; Registers: r20
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
; Registers: r20
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
	rcall		wait_1ms
	rcall		wait_1ms
	rcall		wait_1ms
	rcall		wait_1ms
	rcall		wait_1ms
	ret

; A function that waste approximately 1ms
; Registers: temp1, temp2, r25, r24, 23
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
	
; A function that set temp as 0xFF and decrements it until 0
; Registers: temp1
; Argument: -
; Return: -
delay:
	push		temp1
	ser			temp1
delay_loop:
	dec			temp1
	brne		delay_loop
	pop			temp1
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
	ret								; 4 cycle ; total of 9n + 16 

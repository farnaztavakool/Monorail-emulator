;
; lab3_taskd.asm
;
; Created: 7/26/2020 6:56:11 PM
; Author : Farnaz Tavakol



.include "m2560def.inc"
.def temp1 = r16
.def temp2 = r17			;temp register for macros
.def temp3 = r18
.def one_revolution = r19
.def counter = r20
.def hundred_ms = r21


.macro do_lcd_command
	ldi temp2, @0
	rcall lcd_command
	rcall lcd_wait
.endmacro

.macro do_lcd_data
	ldi temp2, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro

.macro check
	mov temp2, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro

.macro clear_lcd
	do_lcd_command 0b00000001
.endmacro

.macro ascii
	clr temp2
	ldi temp2, '0'
	add @0, temp2
.endmacro

.macro module_by_ten
	clr temp2
	ldi temp2, 10
loop1:
	cp @0, temp2
	brlo end
	sub @0,temp2
	rjmp loop1
end:
	.endmacro

.macro		divide_by_ten
	ldi		temp2, 10						
	clr		temp3						; r21 will store the result of @0/10
dbt_loop_start:
	cp		@0, temp2
	brlo	dbt_loop_finishes
	sub		@0, temp2					; @0 -= 10
	inc		temp3						; r21++
	rjmp	dbt_loop_start				
dbt_loop_finishes:
	mov		@0, temp3
.endmacro


.macro clear
ldi YL, low(@0)     ; load the memory address to Y pointer
ldi YH, high(@0)
 clr temp2            ; set temp to 0 
 st Y+, temp2     ; clear the two bytes at @0 in SRAM
 st Y, temp2
 .endmacro

 ////////END MACRO\\\\\\\\\\\

 .dseg

	revolution:	  .byte 1
	turn:		  .byte 1

.cseg

.org 0x00
jmp RESET

.org INT2addr
jmp motor

.org OVF0addr 
jmp timer0


RESET:
	ldi temp1, low(RAMEND)
	out SPL, temp1
	ldi temp1, high(RAMEND)
	out SPH, temp1

	ser temp1			;testing 
	out DDRC, temp1
	out PORTC, temp1

LCD:
	ser temp1
	out DDRF, temp1
	out DDRA, temp1
	clr temp1
	out PORTF, temp1
	out PORTA, temp1
	ser temp1
	out DDRC, temp1

	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_5ms
	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_1ms
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00001000 ; display off?
	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001110 ; Cursor on, bar, no blink
	clear_lcd
	do_lcd_data 'H'
	do_lcd_data 'e'
	do_lcd_data 'l'
	do_lcd_data 'l'
	do_lcd_data 'o'
	
Timer0_init:
	ldi temp1, 0b00000000
	out TCCR0A, temp1
	ldi temp1, 0b00000101		;1024 prescaler 
	out TCCR0B, temp1         
	ldi temp1, 1<<TOIE0          
	sts TIMSK0, temp1          ; enable Timer0 Overflow Interrupt
	clr counter
	sts turn, counter			; turn = revolution = 0
	sts revolution, counter
	clr hundred_ms	
		
interrupts:
	clr temp1				;PORTD all input
	out DDRD, temp1
	ldi temp1, (1<< ISC21)	;falling edge configuration 
	sts EICRA, temp1
	in temp1, EIMSK
	ori temp1, (1<<INT2)	;enabling external interrupt 
	out EIMSK, temp1
	sei

halt: rjmp halt

Timer0:
	push temp1
	push temp2
	in temp1, SREG
	push temp1
	push r25 
	push r24
	inc counter
	cpi counter, 61
	;brlo not_second1	;check 100ms
	brlo ENDSECOND
	;check_operation
	lds temp1, turn
	sts revolution, temp1	;how many revolution per second
	clr counter
	;rjmp ENDSECOND
;not_second1: jmp not_second

display:	
	clear_lcd
	lds temp1, revolution 
	cpi temp1, 100			;check if big speed
	brsh big_display
	divide_by_ten temp1
	ascii temp1
	check temp1

	lds temp1, revolution 
	module_by_ten temp1
	ascii temp1
	check temp1

	clr temp2
	sts turn , temp2		;start the new speed
	clr hundred_ms
	rjmp ENDSECOND
	
not_second:
	inc hundred_ms
	cpi hundred_ms, 6
	breq display 
	
ENDSECOND:
	pop r24 
	pop r25
	pop temp1
	out SREG, temp1
	pop temp2
	pop temp1
	reti

big_display:

	divide_by_ten temp1
	mov r26, temp1
	divide_by_ten r26
	ascii r26
	check r26

	module_by_ten temp1
	ascii temp1
	check temp1

	lds temp1, revolution 
	module_by_ten temp1
	ascii temp1
	check temp1

	clr temp2 
	sts turn, temp2
	clr hundred_ms
	rjmp ENDSECOND

MOTOR: 
	push temp1
	in temp1, SREG
	push temp1
	cpi one_revolution, 4	;each 4 interrupt one revolution
	breq turn_happend
	inc one_revolution
	rjmp end_motor

turn_happend:
	clr one_revolution
	lds temp1, turn
	inc temp1
	sts turn, temp1

end_motor:
	pop temp1
	out SREG, temp1
	pop temp1
	reti

.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4

.macro lcd_set
	sbi PORTA, @0
.endmacro
.macro lcd_clr
	cbi PORTA, @0
.endmacro

;
; Send a command to the LCD (r16)
;

lcd_command:
	out PORTF, temp2
	nop
	lcd_set LCD_E
	nop
	nop
	nop
	lcd_clr LCD_E
	nop
	nop
	nop
	ret
;need to follow the specific time constraint
lcd_data:
	out PORTF, temp2	;load the instruciton to regsiter r16
	lcd_set LCD_RS
	nop
	nop
	nop
	lcd_set LCD_E
	nop
	nop
	nop
	lcd_clr LCD_E
	nop
	nop
	nop
	lcd_clr LCD_RS
	ret

lcd_wait:
	push temp2
	clr temp2
	out DDRF, temp2
	out PORTF, temp2
	lcd_set LCD_RW
lcd_wait_loop:
	nop
	lcd_set LCD_E
	nop
	nop
        nop
	in temp2, PINF
	lcd_clr LCD_E
	sbrc temp2, 7
	rjmp lcd_wait_loop
	lcd_clr LCD_RW
	ser temp2
	out DDRF, temp2
	pop temp2
	ret

.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4
; 4 cycles per iteration - setup/call-return overhead

sleep_1ms:
	push r24
	push r25
	ldi r25, high(DELAY_1MS)
	ldi r24, low(DELAY_1MS)
delayloop_1ms:
	sbiw r25:r24, 1
	brne delayloop_1ms
	pop r25
	pop r24
	ret

sleep_5ms:
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	ret
 
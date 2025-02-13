	output "standing.bin"
	org #6000

	;test code

begin

	ld hl,music_data
	call play
	ret
	
	
	
	;engine code

;squat by Shiru, 06'17
;Squeeker like, just without the output value table
;4 channels of tone with different duty cycle
;sample drums, non-interrupting
;customizeable noise percussion, interrupting


;music data is all 16-bit words, first control then a few optional ones

;control word is PSSSSSSS DDDN4321, where P=percussion,S=speed, D=drum, N=noise mode, 4321=channels
;D triggers non-interruping sample drum
;P trigger
;if 1, channel 1 freq follows
;if 2, channel 2 freq follows
;if 3, channel 3 freq follows
;if 4, channel 4 freq follows
;if N, channel 4 mode follows, it is either #0000 (normal) or #04cb (noise)
;if P, percussion follows, LSB=volume, MSB=pitch



RLC_H=#04cb			;to enable noise mode
NOP_2=#0000			;to disable noise mode
RLC_HL=#06cb		;to enable sample reading
ADD_IX_IX=#29dd		;to disable sample reading


play

	di
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld (pattern_ptr),de
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	
	ld (loop_ptr),de

	dec hl
	ld (sample_list),hl
	
	ld hl,ADD_IX_IX
	ld (sample_read),hl
	ld hl,NOP_2					;normal mode
	ld (noise_mode),hl
	
	ld ix,0						;needs to be 0 to skip sample reading

	ld c,0
	exx
	ld de,#0808					;sample bit counter and reload value

play_loop

pattern_ptr=$+1
	ld sp,0
	
return_loop

	pop bc						;control word
								;B=duration of the row (0=loop)
								;C=flags DDDN4321 (Drum, Noise, 1-4 channel update)
	ld a,b
	or a
	jp nz,no_loop
	
loop_ptr=$+1
	ld sp,0
	
	jp return_loop
	
no_loop

	ld a,c
	
	rra
	jr nc,skip_note_0
	
	pop hl
	ld (ch0_add),hl
	
skip_note_0

	rra
	jr nc,skip_note_1

	pop hl
	ld (ch1_add),hl
	
skip_note_1

	rra
	jr nc,skip_note_2
	
	pop hl
	ld (ch2_add),hl
	
skip_note_2

	rra
	jr nc,skip_note_3
	
	pop hl
	ld (ch3_add),hl
	
skip_note_3

	rra
	jr nc,skip_mode_change
	
	pop hl						;nop:nop or rlc h
	ld (noise_mode),hl

skip_mode_change

	and 7
	jp z,skip_drum
	
sample_list=$+1
	ld hl,0						;sample_list-2
	add a,a
	add a,l
	ld l,a
	ld a,(hl)
	inc l
	ld h,(hl)
	ld l,a
	ld (sample_ptr),hl
	ld hl,RLC_HL
	ld (sample_read),hl

skip_drum

	bit 7,b						;check percussion flag
	jp z,skip_percussion

	res 7,b						;clear percussion flag
	dec b						;compensate speed

	ld (noise_bc),bc
	ld (noise_de),de

	pop hl						;read percussion parameters

	ld a,l						;noise volume
	ld (noise_volume),a
	ld b,h						;noise pitch
	ld c,h
	ld de,#2174					;utz's rand seed			
	exx
	ld bc,811					;noise duration, takes as long as inner sound loop

noise_loop

	exx							;4
	dec c						;4
	jr nz,noise_skip			;7/12
	ld c,b						;4
	add hl,de					;11
	rlc h						;8		utz's noise generator idea
	inc d						;4		improves randomness
	jp noise_next				;10
	
noise_skip

	jr $+2						;12
	jr $+2						;12
	nop							;4
	nop							;4
	
noise_next

	ld a,h						;4
	
noise_volume=$+1
	cp #80						;7
	sbc a,a						;4
;	and 1
	out ($90),a					;11
	exx							;4

	dec bc						;6
	ld a,b						;4
	or c						;4
	jp nz,noise_loop			;10=106t

	exx

noise_bc=$+1
	ld bc,0
noise_de=$+1
	ld de,0



skip_percussion

	ld (pattern_ptr),sp

sample_ptr=$+1
	ld hl,0

	ld c,0						;internal loop runs 256 times

sound_loop

sample_read=$
	rlc (hl)					;15 	rotate sample bits in place, rl (hl) or add ix,ix (dummy operation)
	sbc a,a						;4		sbc a,a to make bit into 0 or 255, or xor a to keep it 0

	dec e						;4--+	count bits
	jp z,sample_cycle			;10 |
	jp sample_next				;10

sample_cycle

	ld e,d						;4	|	reload counter
	inc hl						;6--+	advance pointer --24t

sample_next

	exx							;4		squeeker type unrolled code
	ld b,a						;4		sample mask
	xor a						;4
	
	ld sp,sound_list			;10
		
	pop de						;10		ch0_acc
	pop hl						;10		ch0_add
	add hl,de					;11
	rla							;4
	ld (ch0_acc),hl				;16
						
	pop de						;10		ch1_acc
	pop hl						;10		ch1_add
	add hl,de					;11
	rla							;4
	ld (ch1_acc),hl				;16
	
	pop de						;10		ch2_acc
	pop hl						;10		ch2_add
	add hl,de					;11
	rla							;4
	ld (ch2_acc),hl				;16

	pop de						;10		ch3_acc
	pop hl						;10		ch3_add
	add hl,de					;11
	
noise_mode=$
	ds 2,0						;8		rlc h for noise effects

	rla							;4
	ld (ch3_acc),hl				;16

	add a,c						;4		no table like in Squeeker, channels summed as is, for uneven 'volume'
	add a,#ff					;7
	sbc a,#ff					;7
	ld c,a						;4
	sbc a,a						;4

	or b						;4		mix sample
;	and 1
	out ($90),a					;11
		
	exx							;4

	dec c						;4
	jp nz,sound_loop			;10=336t


	dec hl						;last byte of a 256/8 byte sample packet is #80 means it was the last packet
	ld a,(hl)
	inc hl
	cp #80
	jr nz,sample_no_stop

	ld hl,ADD_IX_IX
	ld (sample_read),hl			;disable sample reading

sample_no_stop

	djnz sound_loop

	ld (sample_ptr),hl
	
	jp play_loop
	
	
		
;variables in the sound_list can't be reordered because of stack-based fetching

sound_list

ch0_add		dw 0
ch0_acc		dw 0
ch1_add		dw 0
ch1_acc		dw 0
ch2_add		dw 0
ch2_acc		dw 0
ch3_add		dw 0
ch3_acc		dw 0


;compiled music data

	align 2

music_data
	dw .pattern
	dw .loop
;sample data

.sample_list
	dw .sample_1
	dw .sample_2
	dw .sample_3
	dw .sample_4
	dw .sample_5
	dw .sample_6
	dw .sample_7
	align 256/8

.sample_1
.sample_2
.sample_3
.sample_4
.sample_5
.sample_6
.sample_7


.pattern
	dw #8419,#15a,#819,NOP_2,#1180
	dw #400
	dw #8409,#0,#0,#110
	dw #8400,#110
	dw #8409,#15a,#819,#780
	dw #400
	dw #8409,#2b4,#0,#1180
	dw #8400,#110
	dw #8409,#134,#819,#110
	dw #400
	dw #8409,#268,#0,#1180
	dw #8400,#110
	dw #8409,#15a,#819,#780
	dw #400
	dw #8409,#2b4,#0,#1180
	dw #8400,#110
	dw #8405,#15a,#819,#1180
	dw #400
	dw #8405,#0,#0,#110
	dw #8400,#110
	dw #8405,#15a,#819,#780
	dw #400
	dw #8405,#2b4,#0,#1180
	dw #8400,#110
	dw #8405,#112,#895,#110
	dw #400
	dw #8405,#225,#0,#1180
	dw #8400,#110
	dw #8405,#134,#9a2,#780
	dw #400
	dw #8405,#268,#0,#780
	dw #8400,#780
	dw #8403,#112,#568,#1180
	dw #400
	dw #8403,#0,#0,#110
	dw #8400,#110
	dw #8403,#112,#568,#780
	dw #400
	dw #8403,#225,#0,#1180
	dw #8400,#110
	dw #8403,#103,#568,#110
	dw #400
	dw #8403,#206,#0,#1180
	dw #8400,#110
	dw #8403,#112,#568,#780
	dw #400
	dw #8403,#225,#0,#1180
	dw #8400,#110
	dw #8405,#112,#568,#1180
	dw #400
	dw #8405,#0,#0,#110
	dw #8400,#110
	dw #8405,#112,#568,#780
	dw #400
	dw #8405,#225,#0,#1180
	dw #8400,#110
	dw #8405,#112,#895,#110
	dw #400
	dw #8405,#225,#0,#1180
	dw #8400,#110
	dw #8405,#134,#9a2,#780
	dw #400
	dw #8405,#268,#0,#780
	dw #8400,#780
	dw #8409,#15a,#819,#1180
	dw #400
	dw #8409,#0,#0,#110
	dw #8400,#110
	dw #8409,#15a,#819,#780
	dw #400
	dw #8409,#2b4,#0,#1180
	dw #8400,#110
	dw #8409,#134,#819,#110
	dw #400
	dw #8409,#268,#0,#1180
	dw #8400,#110
	dw #8409,#15a,#819,#780
	dw #400
	dw #8409,#2b4,#0,#1180
	dw #8400,#110
	dw #8405,#15a,#819,#1180
	dw #400
	dw #8405,#0,#0,#110
	dw #8400,#110
	dw #8405,#15a,#819,#780
	dw #400
	dw #8405,#2b4,#0,#1180
	dw #8400,#110
	dw #8405,#112,#895,#110
	dw #400
	dw #8405,#225,#0,#1180
	dw #8400,#110
	dw #8405,#134,#9a2,#780
	dw #400
	dw #8405,#268,#0,#780
	dw #8400,#780
	dw #8403,#e6,#819,#1180
	dw #400
	dw #8403,#0,#0,#110
	dw #8400,#110
	dw #8403,#e6,#819,#780
	dw #400
	dw #8403,#1cd,#0,#1180
	dw #8400,#110
	dw #8403,#cd,#819,#110
	dw #400
	dw #8403,#19b,#0,#1180
	dw #8400,#110
	dw #8403,#e6,#819,#780
	dw #400
	dw #8403,#1cd,#0,#1180
	dw #8400,#110
	dw #8405,#103,#819,#1180
	dw #400
	dw #8405,#0,#0,#110
	dw #8400,#110
	dw #8405,#103,#819,#780
	dw #400
	dw #8405,#206,#0,#1180
	dw #8400,#110
	dw #8403,#112,#895,#110
	dw #400
	dw #8403,#225,#0,#1180
	dw #8400,#110
	dw #8403,#134,#9a2,#780
	dw #400
	dw #403,#268,#0
	dw #400
	dw #8407,#15a,#811,#819,#1180
	dw #401,#2b4
	dw #407,#0,#0,#737
	dw #401,#15a
	dw #840b,#2b4,#819,#819,#110
	dw #401,#0
	dw #840f,#15a,#0,#819,#737,#110
	dw #401,#2b4
	dw #8407,#0,#819,#737,#780
	dw #400
	dw #40a,#0,#819
	dw #400
	dw #840f,#15a,#811,#819,#737,#110
	dw #400
	dw #8407,#2b4,#0,#737,#110
	dw #400
	dw #40b,#15a,#811,#819
	dw #401,#2b4
	dw #40b,#0,#0,#737
	dw #401,#15a
	dw #8407,#2b4,#819,#0,#1180
	dw #401,#0
	dw #403,#15a,#0
	dw #401,#2b4
	dw #840b,#0,#895,#0,#780
	dw #400
	dw #403,#15a,#0
	dw #400
	dw #8403,#0,#9a2,#110
	dw #400
	dw #8403,#2b4,#0,#110
	dw #400
	dw #8407,#134,#568,#819,#1180
	dw #401,#268
	dw #407,#0,#0,#737
	dw #401,#134
	dw #840b,#268,#568,#819,#110
	dw #401,#0
	dw #840f,#134,#0,#819,#737,#110
	dw #401,#268
	dw #8407,#0,#568,#737,#780
	dw #400
	dw #40a,#0,#819
	dw #400
	dw #840f,#134,#568,#819,#737,#110
	dw #400
	dw #8407,#268,#0,#737,#110
	dw #400
	dw #40b,#134,#568,#819
	dw #401,#268
	dw #40b,#0,#0,#737
	dw #401,#134
	dw #8407,#268,#568,#0,#1180
	dw #401,#0
	dw #403,#134,#0
	dw #401,#268
	dw #840b,#0,#895,#0,#780
	dw #400
	dw #403,#134,#0
	dw #400
	dw #8407,#0,#9a2,#568,#110
	dw #400
	dw #8407,#268,#0,#611,#110
	dw #400
	dw #840f,#e6,#819,#66e,#568,#1180
	dw #401,#1cd
	dw #40b,#0,#0,#611
	dw #401,#e6
	dw #840b,#1cd,#819,#66e,#110
	dw #401,#0
	dw #8407,#e6,#0,#611,#110
	dw #401,#1cd
	dw #8403,#0,#819,#780
	dw #400
	dw #40a,#0,#611
	dw #400
	dw #8407,#e6,#819,#568,#110
	dw #400
	dw #8403,#1cd,#0,#110
	dw #400
	dw #40b,#e6,#819,#568
	dw #401,#1cd
	dw #403,#0,#0
	dw #401,#e6
	dw #8407,#1cd,#819,#0,#1180
	dw #401,#0
	dw #403,#e6,#0
	dw #401,#1cd
	dw #840b,#0,#895,#0,#780
	dw #400
	dw #403,#e6,#0
	dw #400
	dw #8407,#0,#9a2,#568,#110
	dw #400
	dw #8407,#1cd,#0,#611,#110
	dw #400
	dw #840f,#103,#819,#66e,#568,#1180
	dw #401,#206
	dw #40b,#0,#0,#611
	dw #401,#103
	dw #840b,#206,#819,#66e,#110
	dw #401,#0
	dw #8407,#103,#0,#737,#110
	dw #401,#206
	dw #8403,#0,#819,#780
	dw #400
	dw #40a,#0,#737
	dw #400
	dw #8407,#103,#819,#66e,#110
	dw #400
	dw #8403,#206,#0,#110
	dw #400
	dw #40b,#103,#819,#66e
	dw #401,#206
	dw #403,#0,#0
	dw #401,#103
	dw #8407,#206,#819,#611,#1180
	dw #401,#0
	dw #403,#103,#0
	dw #401,#206
	dw #840b,#0,#895,#611,#780
	dw #400
	dw #403,#103,#0
	dw #400
	dw #8407,#0,#9a2,#0,#110
	dw #400
	dw #8403,#206,#0,#110
	dw #400
	dw #840f,#15a,#811,#819,#0,#1180
	dw #401,#2b4
	dw #8407,#0,#0,#737,#110
	dw #8401,#15a,#110
	dw #840b,#2b4,#811,#819,#780
	dw #401,#0
	dw #840f,#15a,#0,#819,#737,#1180
	dw #8401,#2b4,#110
	dw #8407,#0,#819,#737,#110
	dw #400
	dw #840a,#0,#819,#1180
	dw #8400,#110
	dw #840f,#15a,#811,#819,#737,#780
	dw #400
	dw #8407,#2b4,#0,#737,#1180
	dw #8400,#110
	dw #840b,#15a,#811,#819,#1180
	dw #401,#2b4
	dw #840b,#0,#0,#737,#110
	dw #8401,#15a,#110
	dw #8407,#2b4,#819,#0,#780
	dw #401,#0
	dw #8403,#15a,#0,#1180
	dw #8401,#2b4,#110
	dw #840b,#0,#895,#0,#110
	dw #400
	dw #8403,#15a,#0,#1180
	dw #8400,#110
	dw #8403,#0,#9a2,#780
	dw #400
	dw #8403,#2b4,#0,#780
	dw #8400,#780
	dw #8407,#134,#568,#819,#1180
	dw #401,#268
	dw #8407,#0,#0,#737,#110
	dw #8401,#134,#110
	dw #840b,#268,#568,#819,#780
	dw #401,#0
	dw #840f,#134,#0,#819,#737,#1180
	dw #8401,#268,#110
	dw #8407,#0,#568,#737,#110
	dw #400
	dw #840a,#0,#819,#1180
	dw #8400,#110
	dw #840f,#134,#568,#819,#737,#780
	dw #400
	dw #8407,#268,#0,#737,#1180
	dw #8400,#110
	dw #840b,#134,#568,#819,#1180
	dw #401,#268
	dw #840b,#0,#0,#737,#110
	dw #8401,#134,#110
	dw #8407,#268,#568,#0,#780
	dw #401,#0
	dw #8403,#134,#0,#1180
	dw #8401,#268,#110
	dw #840b,#0,#895,#0,#110
	dw #400
	dw #8403,#134,#0,#1180
	dw #8400,#110
	dw #8407,#0,#9a2,#568,#780
	dw #400
	dw #8407,#268,#0,#611,#780
	dw #8400,#780
	dw #840f,#e6,#819,#66e,#568,#1180
	dw #401,#1cd
	dw #840b,#0,#0,#611,#110
	dw #8401,#e6,#110
	dw #840b,#1cd,#819,#66e,#780
	dw #401,#0
	dw #8407,#e6,#0,#611,#1180
	dw #8401,#1cd,#110
	dw #8403,#0,#819,#110
	dw #400
	dw #840a,#0,#611,#1180
	dw #8400,#110
	dw #8407,#e6,#819,#568,#780
	dw #400
	dw #8403,#1cd,#0,#1180
	dw #8400,#110
	dw #840b,#e6,#819,#568,#1180
	dw #401,#1cd
	dw #8403,#0,#0,#110
	dw #8401,#e6,#110
	dw #8407,#1cd,#819,#0,#780
	dw #401,#0
	dw #8403,#e6,#0,#1180
	dw #8401,#1cd,#110
	dw #840b,#0,#895,#0,#110
	dw #400
	dw #8403,#e6,#0,#1180
	dw #8400,#110
	dw #8407,#0,#9a2,#568,#780
	dw #400
	dw #8407,#1cd,#0,#611,#780
	dw #8400,#780
	dw #840f,#103,#819,#66e,#568,#1180
	dw #401,#206
	dw #840b,#0,#0,#611,#110
	dw #8401,#103,#110
	dw #840b,#206,#819,#66e,#780
	dw #401,#0
	dw #8407,#103,#0,#737,#1180
	dw #8401,#206,#110
	dw #8403,#0,#819,#110
	dw #400
	dw #840a,#0,#737,#1180
	dw #8400,#110
	dw #8407,#103,#819,#66e,#780
	dw #400
	dw #8403,#206,#0,#1180
	dw #8400,#110
	dw #840b,#103,#819,#66e,#1180
	dw #401,#206
	dw #8403,#0,#0,#110
	dw #8401,#103,#110
	dw #8407,#206,#819,#611,#780
	dw #401,#0
	dw #8403,#103,#0,#780
	dw #8401,#206,#780
	dw #840b,#0,#895,#611,#780
	dw #400
	dw #403,#103,#0
	dw #400
	dw #407,#0,#9a2,#0
	dw #400
	dw #403,#206,#0
	dw #400
	dw #840f,#15a,#819,#c23,#0,#1180
	dw #400
	dw #8407,#2b4,#0,#0,#110
	dw #8400,#110
	dw #840b,#15a,#819,#c23,#780
	dw #400
	dw #840f,#2b4,#0,#c23,#0,#1180
	dw #8400,#110
	dw #8407,#134,#819,#0,#110
	dw #400
	dw #840b,#268,#0,#c23,#1180
	dw #8400,#110
	dw #840f,#15a,#819,#cdc,#0,#780
	dw #400
	dw #8407,#2b4,#0,#0,#1180
	dw #8400,#110
	dw #840b,#15a,#819,#cdc,#1180
	dw #400
	dw #840f,#2b4,#0,#cdc,#0,#110
	dw #8400,#110
	dw #8407,#15a,#819,#0,#780
	dw #400
	dw #840b,#2b4,#0,#cdc,#1180
	dw #8400,#110
	dw #840f,#112,#895,#ad0,#0,#110
	dw #400
	dw #8407,#225,#0,#0,#1180
	dw #8400,#110
	dw #840b,#134,#9a2,#ad0,#780
	dw #400
	dw #840b,#268,#0,#0,#780
	dw #8400,#780
	dw #8407,#112,#568,#c23,#1180
	dw #400
	dw #8407,#225,#0,#0,#110
	dw #8400,#110
	dw #840b,#112,#568,#c23,#780
	dw #400
	dw #840f,#225,#0,#c23,#0,#1180
	dw #8400,#110
	dw #8407,#103,#568,#0,#110
	dw #400
	dw #840b,#206,#0,#c23,#1180
	dw #8400,#110
	dw #840f,#112,#568,#cdc,#0,#780
	dw #400
	dw #8407,#225,#0,#0,#1180
	dw #8400,#110
	dw #840b,#112,#568,#cdc,#1180
	dw #400
	dw #840f,#225,#0,#cdc,#0,#110
	dw #8400,#110
	dw #8407,#112,#568,#0,#780
	dw #400
	dw #840b,#225,#0,#cdc,#1180
	dw #8400,#110
	dw #840f,#112,#895,#ad0,#0,#110
	dw #400
	dw #8407,#225,#0,#0,#1180
	dw #8400,#110
	dw #840b,#134,#9a2,#ad0,#780
	dw #400
	dw #840b,#268,#0,#0,#780
	dw #8400,#780
	dw #8407,#15a,#819,#c23,#1180
	dw #400
	dw #8407,#2b4,#0,#0,#110
	dw #8400,#110
	dw #840b,#15a,#819,#c23,#780
	dw #400
	dw #840f,#2b4,#0,#c23,#0,#1180
	dw #8400,#110
	dw #8407,#134,#819,#0,#110
	dw #400
	dw #840b,#268,#0,#c23,#1180
	dw #8400,#110
	dw #840f,#15a,#819,#cdc,#0,#780
	dw #400
	dw #8407,#2b4,#0,#0,#1180
	dw #8400,#110
	dw #840b,#15a,#819,#cdc,#1180
	dw #400
	dw #840f,#2b4,#0,#cdc,#0,#110
	dw #8400,#110
	dw #8407,#15a,#819,#0,#780
	dw #400
	dw #840b,#2b4,#0,#cdc,#1180
	dw #8400,#110
	dw #840f,#112,#895,#ad0,#0,#110
	dw #400
	dw #8407,#225,#0,#0,#1180
	dw #8400,#110
	dw #840b,#134,#9a2,#ad0,#780
	dw #400
	dw #840b,#268,#0,#0,#780
	dw #8400,#780
	dw #8407,#e6,#819,#c23,#1180
	dw #400
	dw #8407,#1cd,#0,#0,#110
	dw #8400,#110
	dw #840b,#e6,#819,#c23,#780
	dw #400
	dw #840f,#1cd,#0,#c23,#0,#1180
	dw #8400,#110
	dw #8407,#cd,#819,#0,#110
	dw #400
	dw #840b,#19b,#0,#c23,#1180
	dw #8400,#110
	dw #840f,#e6,#819,#cdc,#0,#780
	dw #400
	dw #8407,#1cd,#0,#0,#1180
	dw #8400,#110
	dw #840b,#103,#819,#cdc,#1180
	dw #400
	dw #840f,#206,#0,#cdc,#0,#110
	dw #8400,#110
	dw #8407,#103,#819,#0,#780
	dw #400
	dw #840b,#206,#0,#cdc,#1180
	dw #8400,#110
	dw #840f,#112,#895,#ad0,#0,#180
	dw #8400,#380
	dw #8407,#225,#0,#0,#580
	dw #8400,#780
	dw #840b,#134,#9a2,#ad0,#980
	dw #8400,#b80
	dw #840b,#268,#0,#0,#d80
	dw #8400,#f80
	dw #8407,#15a,#819,#c23,#1180
	dw #8400,#110
	dw #8407,#2b4,#0,#0,#180
	dw #8400,#1180
	dw #840b,#15a,#819,#c23,#780
	dw #8400,#110
	dw #840f,#2b4,#0,#c23,#0,#1180
	dw #8400,#110
	dw #8407,#134,#819,#0,#180
	dw #8400,#110
	dw #840b,#268,#0,#c23,#1180
	dw #8400,#110
	dw #840f,#15a,#819,#cdc,#0,#780
	dw #8400,#110
	dw #8407,#2b4,#0,#0,#1180
	dw #8400,#110
	dw #840b,#15a,#819,#cdc,#1180
	dw #8400,#110
	dw #840f,#2b4,#0,#cdc,#0,#180
	dw #8400,#1180
	dw #8407,#15a,#819,#0,#780
	dw #8400,#110
	dw #840b,#2b4,#0,#cdc,#1180
	dw #8400,#110
	dw #840f,#112,#895,#ad0,#0,#180
	dw #8400,#110
	dw #8407,#225,#0,#0,#1180
	dw #8400,#110
	dw #840b,#134,#9a2,#ad0,#780
	dw #8400,#110
	dw #840b,#268,#0,#0,#780
	dw #8400,#780
	dw #8407,#112,#568,#c23,#1180
	dw #8400,#110
	dw #8407,#225,#0,#0,#180
	dw #8400,#1180
	dw #840b,#112,#568,#c23,#780
	dw #8400,#110
	dw #840f,#225,#0,#c23,#0,#1180
	dw #8400,#110
	dw #8407,#103,#568,#0,#180
	dw #8400,#110
	dw #840b,#206,#0,#c23,#1180
	dw #8400,#110
	dw #840f,#112,#568,#cdc,#0,#780
	dw #8400,#110
	dw #8407,#225,#0,#0,#1180
	dw #8400,#110
	dw #840b,#112,#568,#cdc,#1180
	dw #8400,#110
	dw #840f,#225,#0,#cdc,#0,#180
	dw #8400,#1180
	dw #8407,#112,#568,#0,#780
	dw #8400,#110
	dw #840b,#225,#0,#cdc,#1180
	dw #8400,#110
	dw #840f,#112,#895,#ad0,#0,#780
	dw #8400,#780
	dw #8407,#225,#0,#0,#710
	dw #8400,#710
	dw #840b,#134,#9a2,#ad0,#780
	dw #8400,#780
	dw #840b,#268,#0,#0,#710
	dw #8400,#780
	dw #8407,#15a,#819,#c23,#1180
	dw #8400,#110
	dw #8407,#2b4,#0,#0,#180
	dw #8400,#1180
	dw #840b,#15a,#819,#c23,#780
	dw #8400,#110
	dw #840f,#2b4,#0,#c23,#0,#1180
	dw #8400,#110
	dw #8407,#134,#819,#0,#180
	dw #8400,#110
	dw #840b,#268,#0,#c23,#1180
	dw #8400,#110
	dw #840f,#15a,#819,#cdc,#0,#780
	dw #8400,#110
	dw #8407,#2b4,#0,#0,#1180
	dw #8400,#110
	dw #840b,#15a,#819,#cdc,#1180
	dw #8400,#110
	dw #840f,#2b4,#0,#cdc,#0,#180
	dw #8400,#1180
	dw #8407,#15a,#819,#0,#780
	dw #8400,#110
	dw #840b,#2b4,#0,#cdc,#1180
	dw #8400,#110
	dw #840f,#112,#895,#ad0,#0,#180
	dw #8400,#110
	dw #8407,#225,#0,#0,#1180
	dw #8400,#110
	dw #840b,#134,#9a2,#ad0,#780
	dw #8400,#110
	dw #840b,#268,#0,#0,#780
	dw #8400,#780
	dw #8407,#e6,#819,#c23,#1180
	dw #8400,#110
	dw #8407,#1cd,#0,#0,#180
	dw #8400,#1180
	dw #840b,#e6,#819,#c23,#780
	dw #8400,#110
	dw #840f,#1cd,#0,#c23,#0,#1180
	dw #8400,#110
	dw #8407,#cd,#819,#0,#180
	dw #8400,#110
	dw #840b,#19b,#0,#c23,#1180
	dw #8400,#110
	dw #840f,#e6,#819,#cdc,#0,#780
	dw #8400,#110
	dw #8407,#1cd,#0,#0,#1180
	dw #8400,#110
	dw #840b,#206,#819,#cdc,#180
	dw #8400,#380
	dw #840e,#1fe,#cdc,#0,#180
	dw #8400,#380
	dw #8404,#0,#580
	dw #8400,#380
	dw #8408,#cdc,#580
	dw #8400,#780
	dw #840f,#225,#21d,#ad0,#0,#580
	dw #8400,#780
	dw #8404,#0,#980
	dw #8400,#780
	dw #840b,#268,#258,#ad0,#980
	dw #8400,#b80
	dw #8408,#0,#980
	dw #8400,#b80
	dw #8407,#15a,#0,#819,#1180
	dw #401,#2b4
	dw #405,#0,#737
	dw #401,#15a
	dw #8409,#2b4,#819,#110
	dw #401,#0
	dw #840d,#15a,#819,#737,#110
	dw #401,#2b4
	dw #8405,#0,#737,#780
	dw #400
	dw #408,#819
	dw #400
	dw #840d,#15a,#819,#737,#110
	dw #400
	dw #8405,#2b4,#737,#110
	dw #400
	dw #409,#15a,#819
	dw #401,#2b4
	dw #409,#0,#737
	dw #401,#15a
	dw #8405,#2b4,#0,#1180
	dw #401,#0
	dw #401,#15a
	dw #401,#2b4
	dw #8409,#0,#0,#780
	dw #400
	dw #401,#15a
	dw #400
	dw #8401,#0,#110
	dw #400
	dw #8401,#2b4,#110
	dw #400
	dw #8405,#134,#819,#1180
	dw #401,#268
	dw #405,#0,#737
	dw #401,#134
	dw #8409,#268,#819,#110
	dw #401,#0
	dw #840d,#134,#819,#737,#110
	dw #401,#268
	dw #8405,#0,#737,#780
	dw #400
	dw #408,#819
	dw #400
	dw #840d,#134,#819,#737,#110
	dw #400
	dw #8405,#268,#737,#110
	dw #400
	dw #409,#134,#819
	dw #401,#268
	dw #409,#0,#737
	dw #401,#134
	dw #8405,#268,#0,#1180
	dw #401,#0
	dw #401,#134
	dw #401,#268
	dw #8409,#0,#0,#780
	dw #400
	dw #401,#134
	dw #400
	dw #8405,#0,#568,#110
	dw #400
	dw #8405,#268,#611,#110
	dw #400
	dw #840f,#e6,#cdc,#66e,#568,#1180
	dw #403,#1cd,#0
	dw #40b,#0,#c23,#611
	dw #403,#e6,#0
	dw #840b,#1cd,#ad0,#66e,#110
	dw #403,#0,#0
	dw #8407,#e6,#9a2,#611,#110
	dw #403,#1cd,#0
	dw #8405,#0,#cdc,#780
	dw #404,#0
	dw #40c,#c23,#611
	dw #404,#0
	dw #8407,#e6,#ad0,#568,#110
	dw #402,#0
	dw #8403,#1cd,#9a2,#110
	dw #402,#0
	dw #40b,#e6,#cdc,#568
	dw #403,#1cd,#0
	dw #403,#0,#c23
	dw #403,#e6,#0
	dw #8405,#1cd,#ad0,#1180
	dw #405,#0,#0
	dw #405,#e6,#9a2
	dw #405,#1cd,#0
	dw #8409,#0,#cdc,#780
	dw #408,#0
	dw #409,#e6,#c23
	dw #408,#0
	dw #840d,#0,#568,#ad0,#110
	dw #408,#0
	dw #840d,#1cd,#611,#9a2,#110
	dw #408,#0
	dw #840f,#103,#cdc,#66e,#568,#1180
	dw #403,#206,#0
	dw #40b,#0,#cdc,#611
	dw #403,#103,#0
	dw #840b,#206,#cdc,#66e,#110
	dw #403,#0,#0
	dw #8407,#103,#e6e,#737,#110
	dw #403,#206,#0
	dw #8403,#0,#e6e,#780
	dw #402,#0
	dw #40a,#e6e,#737
	dw #402,#0
	dw #8407,#103,#1033,#66e,#110
	dw #402,#0
	dw #8403,#206,#1033,#110
	dw #402,#0
	dw #40b,#103,#1033,#66e
	dw #403,#206,#0
	dw #403,#0,#1033
	dw #403,#103,#0
	dw #8407,#206,#1033,#611,#1180
	dw #403,#0,#0
	dw #403,#103,#1033
	dw #403,#206,#0
	dw #840f,#103,#1033,#0,#611,#780
	dw #402,#0
	dw #402,#1033
	dw #402,#0
	dw #840a,#1033,#0,#110
	dw #402,#0
	dw #8402,#1033,#110
	dw #402,#0
	dw #840f,#15a,#811,#819,#0,#1180
	dw #401,#2b4
	dw #8407,#0,#0,#737,#110
	dw #8401,#15a,#110
	dw #840b,#2b4,#811,#819,#780
	dw #401,#0
	dw #840f,#15a,#0,#819,#737,#1180
	dw #8401,#2b4,#110
	dw #8407,#0,#819,#737,#110
	dw #400
	dw #840a,#0,#819,#1180
	dw #8400,#110
	dw #840f,#15a,#811,#819,#737,#780
	dw #400
	dw #8407,#2b4,#0,#737,#1180
	dw #8400,#110
	dw #840b,#15a,#811,#819,#1180
	dw #401,#2b4
	dw #840b,#0,#0,#737,#110
	dw #8401,#15a,#110
	dw #8407,#2b4,#819,#0,#780
	dw #401,#0
	dw #8403,#15a,#0,#1180
	dw #8401,#2b4,#110
	dw #840b,#0,#895,#0,#110
	dw #400
	dw #8403,#15a,#0,#1180
	dw #8400,#110
	dw #8403,#0,#9a2,#780
	dw #400
	dw #8403,#2b4,#0,#780
	dw #8400,#780
	dw #8407,#134,#568,#819,#1180
	dw #401,#268
	dw #8407,#0,#0,#737,#110
	dw #8401,#134,#110
	dw #840b,#268,#568,#819,#780
	dw #401,#0
	dw #840f,#134,#0,#819,#737,#1180
	dw #8401,#268,#110
	dw #8407,#0,#568,#737,#110
	dw #400
	dw #840a,#0,#819,#1180
	dw #8400,#110
	dw #840f,#134,#568,#819,#737,#780
	dw #400
	dw #8407,#268,#0,#737,#1180
	dw #8400,#110
	dw #840b,#134,#568,#819,#1180
	dw #401,#268
	dw #840b,#0,#0,#737,#110
	dw #8401,#134,#110
	dw #8407,#268,#568,#0,#780
	dw #401,#0
	dw #8403,#134,#0,#1180
	dw #8401,#268,#110
	dw #840b,#0,#895,#0,#110
	dw #400
	dw #8403,#134,#0,#1180
	dw #8400,#110
	dw #8407,#0,#9a2,#568,#780
	dw #400
	dw #8407,#268,#0,#611,#780
	dw #8400,#780
	dw #840f,#e6,#819,#66e,#568,#1180
	dw #401,#1cd
	dw #840b,#0,#0,#611,#110
	dw #8401,#e6,#110
	dw #840b,#1cd,#819,#66e,#780
	dw #401,#0
	dw #8407,#e6,#0,#611,#1180
	dw #8401,#1cd,#110
	dw #8403,#0,#819,#110
	dw #400
	dw #840a,#0,#611,#1180
	dw #8400,#110
	dw #8407,#e6,#819,#568,#780
	dw #400
	dw #8403,#1cd,#0,#1180
	dw #8400,#110
	dw #840b,#e6,#819,#568,#1180
	dw #401,#1cd
	dw #8403,#0,#0,#110
	dw #8401,#e6,#110
	dw #8407,#1cd,#819,#0,#780
	dw #401,#0
	dw #8403,#e6,#0,#1180
	dw #8401,#1cd,#110
	dw #840b,#0,#895,#0,#110
	dw #400
	dw #8403,#e6,#0,#1180
	dw #8400,#110
	dw #8407,#0,#9a2,#568,#780
	dw #400
	dw #8407,#1cd,#0,#611,#780
	dw #8400,#780
	dw #840f,#103,#819,#66e,#568,#1180
	dw #401,#206
	dw #840b,#0,#0,#611,#110
	dw #8401,#103,#110
	dw #840b,#206,#819,#66e,#780
	dw #401,#0
	dw #8407,#103,#0,#737,#1180
	dw #8401,#206,#110
	dw #8403,#0,#819,#110
	dw #400
	dw #840a,#0,#737,#1180
	dw #8400,#110
	dw #8407,#103,#819,#66e,#780
	dw #400
	dw #8403,#206,#0,#1180
	dw #8400,#110
	dw #840b,#103,#819,#66e,#1180
	dw #401,#206
	dw #8403,#0,#0,#110
	dw #8401,#103,#110
	dw #8407,#206,#819,#611,#780
	dw #401,#0
	dw #8403,#103,#0,#780
	dw #8401,#206,#780
	dw #840b,#0,#895,#611,#780
	dw #400
	dw #403,#103,#0
	dw #400
	dw #407,#0,#9a2,#0
	dw #400
	dw #403,#206,#0
	dw #400
	dw #840f,#15a,#819,#609,#611,#1180
	dw #400
	dw #8403,#2b4,#0,#110
	dw #8400,#110
	dw #8403,#15a,#819,#780
	dw #400
	dw #8403,#2b4,#0,#1180
	dw #8400,#110
	dw #8407,#134,#819,#609,#120
	dw #404,#0
	dw #8407,#268,#0,#609,#1180
	dw #8404,#0,#120
	dw #8407,#15a,#819,#609,#780
	dw #404,#0
	dw #8407,#2b4,#0,#609,#1180
	dw #8404,#0,#120
	dw #8407,#15a,#819,#609,#1180
	dw #400
	dw #8407,#2b4,#0,#0,#140
	dw #8400,#140
	dw #8407,#15a,#819,#609,#780
	dw #400
	dw #8407,#2b4,#0,#0,#1180
	dw #8400,#140
	dw #8407,#112,#895,#609,#140
	dw #400
	dw #8407,#225,#0,#0,#1180
	dw #8400,#140
	dw #8407,#134,#9a2,#609,#780
	dw #400
	dw #8407,#268,#0,#0,#780
	dw #8400,#780
	dw #840f,#112,#568,#601,#611,#1180
	dw #400
	dw #8403,#225,#0,#180
	dw #8400,#180
	dw #8403,#112,#568,#780
	dw #400
	dw #8403,#225,#0,#1180
	dw #8400,#180
	dw #8407,#103,#568,#601,#140
	dw #404,#0
	dw #8407,#206,#0,#601,#1180
	dw #8404,#0,#140
	dw #8407,#112,#568,#601,#780
	dw #404,#0
	dw #8407,#225,#0,#601,#1180
	dw #8404,#0,#140
	dw #8407,#112,#568,#601,#1180
	dw #400
	dw #8407,#225,#0,#0,#120
	dw #8400,#120
	dw #8407,#112,#568,#601,#780
	dw #400
	dw #8407,#225,#0,#0,#1180
	dw #8400,#120
	dw #8407,#112,#895,#601,#110
	dw #400
	dw #8407,#225,#0,#0,#1180
	dw #8400,#110
	dw #8407,#134,#9a2,#601,#780
	dw #400
	dw #8407,#268,#0,#0,#780
	dw #8400,#780
	dw #840f,#15a,#819,#5f9,#611,#1180
	dw #400
	dw #8403,#2b4,#0,#120
	dw #8400,#120
	dw #8403,#15a,#819,#780
	dw #400
	dw #8403,#2b4,#0,#1180
	dw #8400,#120
	dw #8407,#134,#819,#5f9,#320
	dw #404,#0
	dw #8407,#268,#0,#5f9,#1180
	dw #8404,#0,#320
	dw #8407,#15a,#819,#5f9,#780
	dw #404,#0
	dw #8407,#2b4,#0,#5f9,#1180
	dw #8404,#0,#320
	dw #8407,#15a,#819,#5f9,#1180
	dw #400
	dw #8407,#2b4,#0,#0,#520
	dw #8400,#520
	dw #8407,#15a,#819,#5f9,#780
	dw #400
	dw #8407,#2b4,#0,#0,#1180
	dw #8400,#520
	dw #8407,#112,#895,#5f9,#110
	dw #400
	dw #8407,#225,#0,#0,#1180
	dw #8400,#720
	dw #8407,#134,#9a2,#5f9,#780
	dw #400
	dw #8407,#268,#0,#0,#780
	dw #8400,#780
	dw #840f,#e6,#819,#5f1,#611,#1180
	dw #400
	dw #8403,#1cd,#0,#720
	dw #8400,#720
	dw #8403,#e6,#819,#780
	dw #400
	dw #8403,#1cd,#0,#1180
	dw #8400,#720
	dw #8407,#cd,#819,#5f1,#520
	dw #404,#0
	dw #8407,#19b,#0,#5f1,#1180
	dw #8404,#0,#520
	dw #8407,#e6,#819,#5f1,#780
	dw #404,#0
	dw #8407,#1cd,#0,#5f1,#1180
	dw #8404,#0,#520
	dw #8407,#103,#819,#5f1,#1180
	dw #400
	dw #8407,#206,#0,#0,#320
	dw #8400,#320
	dw #8407,#103,#819,#5f1,#780
	dw #400
	dw #8407,#206,#0,#0,#1180
	dw #8400,#110
	dw #8407,#112,#895,#5f1,#180
	dw #8400,#380
	dw #8407,#225,#0,#0,#580
	dw #8400,#780
	dw #8407,#134,#9a2,#5f1,#980
	dw #8400,#b80
	dw #8407,#268,#0,#0,#d80
	dw #8400,#f80
	dw #840f,#15a,#819,#c23,#611,#1180
	dw #8400,#110
	dw #8407,#2b4,#0,#609,#180
	dw #8400,#1180
	dw #8403,#15a,#819,#780
	dw #8400,#110
	dw #8407,#2b4,#0,#c23,#1180
	dw #8400,#110
	dw #8407,#134,#819,#609,#180
	dw #8404,#0,#110
	dw #8407,#268,#0,#609,#1180
	dw #8404,#0,#110
	dw #8407,#15a,#819,#cdc,#780
	dw #8400,#110
	dw #8407,#2b4,#0,#609,#1180
	dw #8404,#0,#110
	dw #8407,#15a,#819,#609,#1180
	dw #8404,#0,#110
	dw #8407,#2b4,#0,#cdc,#180
	dw #8400,#1180
	dw #8407,#15a,#819,#609,#780
	dw #8400,#110
	dw #8407,#2b4,#0,#0,#1180
	dw #8400,#110
	dw #8407,#112,#895,#ad0,#180
	dw #8400,#110
	dw #8407,#225,#0,#0,#1180
	dw #8400,#110
	dw #8407,#134,#9a2,#609,#780
	dw #8400,#110
	dw #8407,#268,#0,#0,#780
	dw #8400,#780
	dw #840f,#112,#568,#c23,#611,#1180
	dw #8400,#110
	dw #8407,#225,#0,#601,#180
	dw #8400,#1180
	dw #8403,#112,#568,#780
	dw #8400,#110
	dw #8407,#225,#0,#c23,#1180
	dw #8400,#110
	dw #8407,#103,#568,#601,#180
	dw #8404,#0,#110
	dw #8407,#206,#0,#611,#1180
	dw #8404,#0,#110
	dw #8407,#112,#568,#cdc,#780
	dw #8400,#110
	dw #8407,#225,#0,#601,#1180
	dw #8404,#0,#110
	dw #8407,#112,#568,#611,#1180
	dw #8404,#0,#110
	dw #8407,#225,#0,#cdc,#180
	dw #8400,#1180
	dw #8407,#112,#568,#601,#780
	dw #8400,#110
	dw #8407,#225,#0,#0,#1180
	dw #8400,#110
	dw #8407,#112,#895,#ad0,#780
	dw #8400,#780
	dw #8407,#225,#0,#0,#710
	dw #8400,#710
	dw #8407,#134,#9a2,#601,#780
	dw #8400,#780
	dw #8407,#268,#0,#0,#710
	dw #8400,#780
	dw #840f,#15a,#819,#c23,#611,#1180
	dw #8400,#110
	dw #8407,#2b4,#0,#5f9,#180
	dw #8400,#1180
	dw #8403,#15a,#819,#780
	dw #8400,#110
	dw #8407,#2b4,#0,#c23,#1180
	dw #8400,#110
	dw #8407,#134,#819,#5f9,#180
	dw #8404,#0,#110
	dw #8407,#268,#0,#5f9,#1180
	dw #8404,#0,#110
	dw #8407,#15a,#819,#cdc,#780
	dw #8400,#110
	dw #8407,#2b4,#0,#5f9,#1180
	dw #8404,#0,#110
	dw #8407,#15a,#819,#5f9,#1180
	dw #8404,#0,#110
	dw #8407,#2b4,#0,#cdc,#180
	dw #8400,#1180
	dw #8407,#15a,#819,#5f9,#780
	dw #8400,#110
	dw #8407,#2b4,#0,#0,#1180
	dw #8400,#110
	dw #8407,#112,#895,#ad0,#180
	dw #8400,#110
	dw #8407,#225,#0,#0,#1180
	dw #8400,#110
	dw #8407,#134,#9a2,#5f9,#780
	dw #8400,#110
	dw #8407,#268,#0,#0,#780
	dw #8400,#780
	dw #840f,#e6,#819,#c23,#611,#1180
	dw #8400,#110
	dw #8407,#1cd,#0,#5f1,#180
	dw #8400,#1180
	dw #8403,#e6,#819,#780
	dw #8400,#110
	dw #8407,#1cd,#0,#c23,#1180
	dw #8400,#110
	dw #8407,#cd,#819,#5f1,#180
	dw #8404,#0,#110
	dw #8407,#19b,#0,#5f1,#1180
	dw #8404,#0,#110
	dw #8407,#e6,#819,#cdc,#780
	dw #8400,#110
	dw #8407,#1cd,#0,#5f1,#1180
	dw #8404,#0,#110
	dw #8407,#103,#819,#5f1,#180
	dw #8404,#0,#380
	dw #8407,#206,#0,#cdc,#180
	dw #8400,#380
	dw #8405,#103,#5f1,#580
	dw #8400,#380
	dw #8405,#206,#0,#580
	dw #8400,#780
	dw #840d,#112,#ad0,#0,#580
	dw #8400,#780
	dw #8405,#225,#0,#980
	dw #8400,#780
	dw #8401,#134,#980
	dw #8400,#b80
	dw #8401,#268,#980
	dw #8400,#b80
	dw #8403,#15a,#819,#1180
	dw #400
	dw #8403,#0,#0,#110
	dw #8400,#110
	dw #8403,#15a,#819,#780
	dw #400
	dw #8403,#2b4,#0,#1180
	dw #8400,#110
	dw #8403,#134,#819,#110
	dw #400
	dw #8403,#268,#0,#1180
	dw #8400,#110
	dw #8403,#15a,#819,#780
	dw #400
	dw #8403,#2b4,#0,#1180
	dw #8400,#110
	dw #8403,#15a,#819,#1180
	dw #400
	dw #8403,#0,#0,#110
	dw #8400,#110
	dw #8403,#15a,#819,#780
	dw #400
	dw #8403,#2b4,#0,#1180
	dw #8400,#110
	dw #8403,#112,#895,#780
	dw #8400,#780
	dw #8403,#225,#0,#1180
	dw #8400,#1180
	dw #8403,#134,#9a2,#780
	dw #8400,#780
	dw #8403,#268,#0,#1180
	dw #8400,#1180
	dw #8403,#112,#568,#1180
	dw #400
	dw #8403,#0,#0,#110
	dw #8400,#110
	dw #8403,#112,#568,#780
	dw #400
	dw #8403,#225,#0,#1180
	dw #8400,#110
	dw #8403,#103,#568,#110
	dw #400
	dw #8403,#206,#0,#1180
	dw #8400,#110
	dw #8403,#112,#568,#780
	dw #400
	dw #8403,#225,#0,#1180
	dw #8400,#110
	dw #8403,#112,#568,#1180
	dw #400
	dw #8403,#0,#0,#110
	dw #8400,#1180
	dw #8403,#112,#568,#780
	dw #400
	dw #8403,#225,#0,#1180
	dw #8400,#1180
	dw #8403,#112,#895,#780
	dw #400
	dw #8403,#225,#0,#1180
	dw #8400,#1180
	dw #8403,#134,#9a2,#780
	dw #400
	dw #8403,#268,#0,#780
	dw #8400,#780
	dw #8403,#15a,#819,#1180
	dw #400
	dw #8403,#0,#0,#110
	dw #8400,#110
	dw #8403,#15a,#819,#780
	dw #400
	dw #8403,#2b4,#0,#1180
	dw #8400,#110
	dw #8403,#134,#819,#110
	dw #400
	dw #8403,#268,#0,#1180
	dw #8400,#110
	dw #8403,#15a,#819,#780
	dw #400
	dw #8403,#2b4,#0,#1180
	dw #8400,#110
	dw #8403,#15a,#819,#1180
	dw #400
	dw #8403,#0,#0,#110
	dw #8400,#110
	dw #8403,#15a,#819,#780
	dw #400
	dw #8403,#2b4,#0,#1180
	dw #8400,#110
	dw #8403,#112,#895,#780
	dw #8400,#780
	dw #8403,#225,#0,#1180
	dw #8400,#1180
	dw #8403,#134,#9a2,#780
	dw #8400,#780
	dw #8403,#268,#0,#1180
	dw #8400,#1180
	dw #8403,#e6,#819,#1180
	dw #400
	dw #8403,#0,#0,#110
	dw #8400,#110
	dw #8403,#e6,#819,#780
	dw #400
	dw #8403,#1cd,#0,#1180
	dw #8400,#110
	dw #8403,#cd,#819,#110
	dw #400
	dw #8403,#19b,#0,#1180
	dw #8400,#110
	dw #8403,#e6,#819,#780
	dw #400
	dw #8403,#1cd,#0,#1180
	dw #8400,#110
	dw #8403,#103,#819,#180
	dw #8400,#380
	dw #8403,#0,#0,#180
	dw #8400,#380
	dw #8403,#103,#819,#580
	dw #8400,#380
	dw #8403,#206,#0,#580
	dw #8400,#780
	dw #8403,#112,#895,#580
	dw #8400,#780
	dw #8403,#225,#0,#980
	dw #8400,#780
	dw #8403,#134,#9a2,#980
	dw #8400,#b80
	dw #8403,#268,#0,#980
	dw #8400,#b80
	dw #8403,#15a,#152,#780
	dw #400
	dw #8400,#740
	dw #400
	dw #8400,#720
	dw #400
	dw #8400,#710
	dw #400
	dw #402,#0
	dw #400
	dw #400
	dw #400
	dw #400
	dw #400
	dw #400
	dw #400
	dw #401,#0
	dw #400
	dw #400
	dw #400
	dw #400
	dw #400
	dw #400
	dw #400
	dw #400
	dw #400
	dw #400
	dw #400
	dw #400
	dw #400
	dw #400
	dw #400
.loop
	dw #400
	dw #400
	dw #400
	dw #400
	dw #400
	dw #400
	dw #400
	dw #400
	dw 0

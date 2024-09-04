	output "babka.bin"	; a strange game
	org #6000

	;test code

begin
        exx
        push hl
	ld hl,music_data
	call play
        pop hl
        exx
        ei
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

	;; ld (loop_ptr),de
	ld (loop_ptr),sp

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

	;; jp return_loop
        ret

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
	and 2
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
	and 2
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
	db 0,0,0,0,0,0,0,2,32,0,0,0,0,0,0,0
	db 0,0,0,0,8,193,239,231,255,255,255,255,14,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,238,255,255,255,255,255
	db 255,255,255,255,255,255,255,255,35,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128
.sample_2
	db 0,0,0,0,0,0,0,4,32,0,0,0,0,0,0,0
	db 252,255,255,3,0,0,0,0,0,0,254,255,255,255,255,9
	db 0,0,0,0,240,255,255,255,111,37,0,0,0,0,0,0
	db 0,220,255,255,255,15,0,0,0,0,0,128,207,255,255,255
	db 20,0,0,0,0,0,128,164,238,255,251,123,19,0,0,0
	db 0,0,0,128,254,207,127,182,1,0,0,0,0,0,0,128
.sample_3
	db 0,0,0,0,0,0,0,1,32,0,0,0,0,0,0,0
	db 21,32,85,4,9,0,0,0,2,0,0,0,132,0,0,0
	db 64,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128
.sample_4
.sample_5
.sample_6
.sample_7


.pattern
	dw $333,$15a,$568,NOP_2
	dw $302,$0
	dw $300
	dw $300
	dw $30b,$0,$568,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$19b,$337,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32b,$0,$337,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $32b,$15a,$568,$0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30b,$0,$568,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$206,$40c,$0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $32b,$0,$40c,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $32b,$15a,$568,$0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30b,$0,$568,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$19b,$337,$0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $32b,$0,$337,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $32b,$15a,$568,$0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30b,$0,$568,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$206,$40c,$0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $32b,$0,$40c,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $323,$15a,$ad0
	dw $302,$0
	dw $328,$0
	dw $300
	dw $32b,$0,$ad0,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30b,$19b,$66e,$0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30b,$0,$66e,$ad0
	dw $302,$0
	dw $300
	dw $300
	dw $30b,$184,$c23,$0
	dw $302,$0
	dw $300
	dw $300
	dw $303,$103,$819
	dw $302,$0
	dw $300
	dw $300
	dw $303,$122,$917
	dw $302,$0
	dw $300
	dw $300
	dw $303,$146,$a34
	dw $302,$0
	dw $300
	dw $300
	dw $323,$15a,$ad0
	dw $302,$0
	dw $320
	dw $300
	dw $32b,$0,$ad0,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30b,$19b,$66e,$0
	dw $302,$0
	dw $300
	dw $300
	dw $30b,$0,$66e,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30b,$184,$c23,$0
	dw $302,$0
	dw $300
	dw $300
	dw $343,$103,$819
	dw $302,$0
	dw $300
	dw $300
	dw $343,$122,$917
	dw $302,$0
	dw $340
	dw $300
	dw $343,$146,$a34
	dw $302,$0
	dw $300
	dw $300
	dw $327,$15a,$568,$102b
	dw $306,$0,$1023
	dw $304,$101b
	dw $304,$1013
	dw $36f,$0,$568,$1023,$ad0
	dw $306,$0,$1023
	dw $30c,$1033,$0
	dw $300
	dw $343,$19b,$337
	dw $302,$0
	dw $300
	dw $300
	dw $32b,$0,$337,$ad0
	dw $302,$0
	dw $328,$0
	dw $340
	dw $32f,$15a,$568,$cdc,$0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $32b,$0,$568,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$206,$40c,$0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $36b,$0,$40c,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $32f,$15a,$568,$819,$0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $36b,$0,$568,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$19b,$337,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32b,$0,$337,$ad0
	dw $302,$0
	dw $328,$0
	dw $340
	dw $32b,$15a,$568,$0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $32b,$0,$568,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$206,$40c,$0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $36f,$0,$40c,$0,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $327,$146,$51a,$f4a
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$51a,$a34
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$184,$308,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32b,$0,$308,$a34
	dw $302,$0
	dw $328,$0
	dw $340
	dw $32f,$146,$51a,$c23,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32b,$0,$51a,$a34
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$1e9,$3d2,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$3d2,$a34
	dw $302,$0
	dw $308,$0
	dw $300
	dw $32f,$146,$51a,$7a5,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$51a,$a34
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$184,$308,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32b,$0,$308,$a34
	dw $302,$0
	dw $328,$0
	dw $348,$0
	dw $32b,$146,$51a,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32f,$0,$51a,$0,$a34
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34f,$1e9,$3d2,$feb,$0
	dw $306,$0,$ff3
	dw $304,$ffb
	dw $304,$1003
	dw $36f,$0,$3d2,$100b,$a34
	dw $306,$0,$1013
	dw $30c,$101b,$0
	dw $304,$1023
	dw $327,$15a,$568,$1033
	dw $302,$0
	dw $300
	dw $300
	dw $36f,$0,$568,$1344,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34f,$19b,$337,$15a0,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32f,$0,$337,$1846,$ad0
	dw $302,$0
	dw $328,$0
	dw $340
	dw $32f,$15a,$568,$15a0,$0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $32f,$0,$568,$1344,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34f,$206,$40c,$1033,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$40c,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $32f,$15a,$568,$15a0,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$568,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$19b,$337,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32b,$0,$337,$ad0
	dw $302,$0
	dw $328,$0
	dw $340
	dw $32b,$15a,$568,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32b,$0,$568,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$206,$40c,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36f,$0,$40c,$0,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $327,$146,$51a,$f4a
	dw $302,$0
	dw $300
	dw $300
	dw $36f,$0,$51a,$122f,$a34
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34f,$184,$308,$1469,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32f,$0,$308,$1846,$a34
	dw $302,$0
	dw $328,$0
	dw $340
	dw $32f,$146,$51a,$1469,$0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $32f,$0,$51a,$122f,$a34
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34f,$1e9,$3d2,$f4a,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$3d2,$a34
	dw $302,$0
	dw $308,$0
	dw $300
	dw $32f,$146,$51a,$1469,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$51a,$a34
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$184,$308,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32b,$0,$308,$a34
	dw $302,$0
	dw $328,$0
	dw $340
	dw $32f,$146,$51a,$1461,$0
	dw $306,$0,$1459
	dw $304,$1451
	dw $304,$1449
	dw $32f,$0,$51a,$1441,$a34
	dw $306,$0,$1439
	dw $30c,$1431,$0
	dw $304,$1429
	dw $34f,$1e9,$3d2,$1421,$0
	dw $306,$0,$0
	dw $300
	dw $300
	dw $36b,$0,$3d2,$a34
	dw $302,$0
	dw $308,$0
	dw $300
	dw $303,$15a,$ad0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$ad0,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30b,$19b,$66e,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$66e,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $36b,$184,$c23,$0
	dw $302,$0
	dw $360
	dw $300
	dw $363,$103,$819
	dw $302,$0
	dw $300
	dw $300
	dw $363,$122,$917
	dw $302,$0
	dw $300
	dw $300
	dw $363,$146,$a34
	dw $302,$0
	dw $300
	dw $300
	dw $363,$15a,$ad0
	dw $302,$0
	dw $300
	dw $300
	dw $30b,$0,$ad0,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $36b,$19b,$66e,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$66e,$ad0
	dw $302,$0
	dw $368,$0
	dw $300
	dw $36b,$184,$c23,$0
	dw $302,$0
	dw $300
	dw $300
	dw $363,$103,$819
	dw $302,$0
	dw $300
	dw $300
	dw $343,$122,$917
	dw $342,$0
	dw $340
	dw $300
	dw $343,$146,$a34
	dw $302,$0
	dw $340
	dw $300
	dw $327,$19b,$66e,$9a2
	dw $306,$0,$9a2
	dw $304,$1344
	dw $304,$9a2
	dw $36f,$0,$66e,$9a2,$cdc
	dw $306,$0,$9a2
	dw $30c,$1344,$0
	dw $304,$9a2
	dw $34f,$1e9,$3d2,$9a2,$0
	dw $306,$0,$9a2
	dw $304,$1344
	dw $304,$9a2
	dw $32f,$0,$3d2,$9a2,$cdc
	dw $306,$0,$9a2
	dw $32c,$1344,$0
	dw $344,$9a2
	dw $32f,$19b,$66e,$b74,$0
	dw $306,$0,$b74
	dw $304,$16e9
	dw $304,$b74
	dw $32f,$0,$66e,$b74,$cdc
	dw $306,$0,$b74
	dw $30c,$16e9,$0
	dw $304,$b74
	dw $34f,$268,$4d1,$b74,$0
	dw $306,$0,$b74
	dw $304,$16e9
	dw $304,$b74
	dw $36f,$0,$4d1,$b74,$cdc
	dw $306,$0,$b74
	dw $30c,$16e9,$0
	dw $304,$b74
	dw $32f,$19b,$66e,$cdc,$0
	dw $306,$0,$cdc
	dw $304,$1846
	dw $304,$cdc
	dw $36f,$0,$66e,$cdc,$cdc
	dw $306,$0,$cdc
	dw $30c,$1846,$0
	dw $304,$cdc
	dw $34f,$1e9,$3d2,$cdc,$0
	dw $306,$0,$cdc
	dw $304,$1846
	dw $304,$cdc
	dw $32f,$0,$3d2,$cdc,$cdc
	dw $306,$0,$cdc
	dw $32c,$1846,$0
	dw $344,$cdc
	dw $32f,$19b,$66e,$b74,$0
	dw $306,$0,$b74
	dw $304,$16e9
	dw $304,$b74
	dw $32f,$0,$66e,$b74,$cdc
	dw $306,$0,$b74
	dw $30c,$16e9,$0
	dw $304,$b74
	dw $34f,$268,$4d1,$b74,$0
	dw $306,$0,$b74
	dw $304,$16e9
	dw $304,$b74
	dw $36f,$0,$4d1,$b74,$cdc
	dw $306,$0,$b74
	dw $30c,$16e9,$0
	dw $304,$b74
	dw $327,$16e,$5ba,$895
	dw $306,$0,$895
	dw $304,$112a
	dw $304,$895
	dw $36f,$0,$5ba,$895,$b74
	dw $306,$0,$895
	dw $30c,$112a,$0
	dw $304,$895
	dw $34f,$1b3,$367,$895,$0
	dw $306,$0,$895
	dw $304,$112a
	dw $304,$895
	dw $32f,$0,$367,$895,$b74
	dw $306,$0,$895
	dw $32c,$112a,$0
	dw $344,$895
	dw $32f,$16e,$5ba,$a34,$0
	dw $306,$0,$a34
	dw $304,$1469
	dw $304,$a34
	dw $32f,$0,$5ba,$a34,$b74
	dw $306,$0,$a34
	dw $30c,$1469,$0
	dw $304,$a34
	dw $34f,$225,$44a,$a34,$0
	dw $306,$0,$a34
	dw $304,$1469
	dw $304,$a34
	dw $36f,$0,$44a,$a34,$b74
	dw $306,$0,$a34
	dw $30c,$1469,$0
	dw $304,$a34
	dw $32f,$16e,$5ba,$b74,$0
	dw $306,$0,$b74
	dw $304,$16e9
	dw $304,$b74
	dw $36f,$0,$5ba,$b74,$b74
	dw $306,$0,$b74
	dw $30c,$16e9,$0
	dw $304,$b74
	dw $34f,$1b3,$367,$b74,$0
	dw $306,$0,$b74
	dw $304,$16e9
	dw $304,$b74
	dw $32f,$0,$367,$b74,$b74
	dw $306,$0,$b74
	dw $32c,$16e9,$0
	dw $344,$b74
	dw $32f,$16e,$5ba,$a34,$0
	dw $306,$0,$a34
	dw $30c,$1469,$0
	dw $304,$a34
	dw $32f,$0,$5ba,$a34,$b74
	dw $306,$0,$a34
	dw $30c,$1469,$0
	dw $304,$a34
	dw $34f,$225,$44a,$a34,$0
	dw $306,$0,$a34
	dw $304,$a34
	dw $304,$a34
	dw $36f,$0,$44a,$a34,$b74
	dw $306,$0,$a34
	dw $30c,$a34,$0
	dw $304,$a34
	dw $327,$146,$51a,$f4a
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$51a,$a34
	dw $302,$0
	dw $308,$0
	dw $308,$0
	dw $34b,$184,$308,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32b,$0,$308,$a34
	dw $302,$0
	dw $328,$0
	dw $340
	dw $32f,$146,$51a,$c23,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32b,$0,$51a,$a34
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$1e9,$3d2,$0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30a,$3d2,$a34
	dw $303,$0,$0
	dw $300
	dw $300
	dw $30f,$146,$51a,$7a5,$0
	dw $302,$0
	dw $300
	dw $300
	dw $30b,$0,$51a,$a34
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30b,$184,$308,$0
	dw $302,$0
	dw $300
	dw $300
	dw $30b,$0,$308,$a34
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30b,$146,$51a,$0
	dw $302,$0
	dw $300
	dw $300
	dw $30b,$0,$51a,$a34
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30f,$1e9,$3d2,$1033,$0
	dw $306,$0,$1033
	dw $304,$1033
	dw $304,$1033
	dw $30f,$0,$3d2,$1033,$a34
	dw $306,$0,$1033
	dw $30c,$1033,$0
	dw $304,$1033
	dw $327,$15a,$568,$1033
	dw $302,$0
	dw $300
	dw $300
	dw $36f,$0,$568,$1344,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34f,$19b,$337,$15a0,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32f,$0,$337,$1846,$ad0
	dw $302,$0
	dw $328,$0
	dw $340
	dw $32f,$15a,$568,$15a0,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32f,$0,$568,$1344,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34f,$206,$40c,$1033,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$40c,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $32f,$15a,$568,$15a0,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$568,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$19b,$337,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32b,$0,$337,$ad0
	dw $302,$0
	dw $328,$0
	dw $340
	dw $32b,$15a,$568,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32b,$0,$568,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$206,$40c,$0
	dw $302,$0
	dw $300
	dw $300
	dw $30b,$0,$40c,$ad0
	dw $302,$0
	dw $308,$0
	dw $308,$0
	dw $307,$134,$4d1,$e6e
	dw $302,$0
	dw $300
	dw $300
	dw $30b,$0,$4d1,$9a2
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30b,$16e,$2dd,$0
	dw $302,$0
	dw $300
	dw $300
	dw $30b,$0,$2dd,$9a2
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30f,$134,$4d1,$b74,$0
	dw $302,$0
	dw $300
	dw $300
	dw $30b,$0,$4d1,$9a2
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30b,$1cd,$39b,$0
	dw $302,$0
	dw $300
	dw $300
	dw $30b,$0,$39b,$9a2
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30f,$134,$4d1,$737,$0
	dw $302,$0
	dw $300
	dw $300
	dw $30b,$0,$4d1,$9a2
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30b,$16e,$2dd,$0
	dw $302,$0
	dw $300
	dw $300
	dw $30b,$0,$2dd,$9a2
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30b,$134,$4d1,$0
	dw $302,$0
	dw $300
	dw $300
	dw $30b,$0,$4d1,$9a2
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30f,$1cd,$39b,$f4a,$0
	dw $306,$0,$f4a
	dw $304,$f4a
	dw $304,$f4a
	dw $30f,$0,$39b,$f4a,$9a2
	dw $306,$0,$f4a
	dw $30c,$f4a,$0
	dw $304,$f4a
	dw $327,$146,$51a,$f4a
	dw $302,$0
	dw $300
	dw $300
	dw $36f,$0,$51a,$122f,$a34
	dw $302,$0
	dw $308,$0
	dw $308,$0
	dw $34f,$184,$308,$1469,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32f,$0,$308,$1846,$a34
	dw $302,$0
	dw $328,$0
	dw $340
	dw $32f,$146,$51a,$1469,$0
	dw $302,$0
	dw $308,$0
	dw $308,$0
	dw $32f,$0,$51a,$122f,$a34
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34f,$1e9,$3d2,$f4a,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$3d2,$a34
	dw $302,$0
	dw $308,$0
	dw $300
	dw $32f,$146,$51a,$1469,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$51a,$a34
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$184,$308,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32b,$0,$308,$a34
	dw $302,$0
	dw $328,$0
	dw $340
	dw $32b,$146,$51a,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32b,$0,$51a,$a34
	dw $302,$0
	dw $308,$0
	dw $308,$0
	dw $34b,$1e9,$3d2,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$3d2,$a34
	dw $302,$0
	dw $308,$0
	dw $300
	dw $327,$15a,$568,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$568,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$19b,$337,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$337,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $32b,$15a,$568,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32b,$0,$568,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$206,$40c,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$40c,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $32b,$15a,$568,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$568,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$19b,$337,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$337,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $32b,$15a,$568,$0
	dw $302,$0
	dw $300
	dw $300
	dw $32b,$0,$568,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $34b,$206,$40c,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$40c,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $323,$15a,$568
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$568,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30b,$19b,$337,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$337,$ad0
	dw $302,$0
	dw $368,$0
	dw $300
	dw $36b,$15a,$568,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$568,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $36b,$206,$40c,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$40c,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $36b,$15a,$568,$0
	dw $302,$0
	dw $360
	dw $300
	dw $36b,$0,$568,$ad0
	dw $302,$0
	dw $368,$0
	dw $300
	dw $36b,$19b,$337,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$337,$ad0
	dw $302,$0
	dw $308,$0
	dw $300
	dw $30b,$15a,$568,$0
	dw $302,$0
	dw $300
	dw $300
	dw $36b,$0,$568,$ad0
	dw $302,$0
	dw $368,$0
	dw $300
	dw $36b,$206,$40c,$0
	dw $302,$0
	dw $360
	dw $300
	dw $36b,$0,$40c,$ad0
	dw $302,$0
	dw $368,$0
	dw $300
	dw $34a,$ad0,$0
	dw $300
	dw $302,$0
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
	dw $300
.loop
	dw $7f1f,0,0,0,0,NOP_2
	dw 0

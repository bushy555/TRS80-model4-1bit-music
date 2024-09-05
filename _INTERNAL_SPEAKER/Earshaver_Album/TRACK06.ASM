	output	"TRACK06.BIN"

	org $6000
begin:


; 	EARSHAVER
; 	TRACK 6
;	TRS80 MODEL 4, internal speaker
;


	ld hl, music_data
	call play
	ret
	
hi_bit	equ	$01
port	equ	$90


;music data format
;two bytes absolute pointer to drumParam table,  then song data follows
;row length and flags byte
;%Frrrrrrr
; r equ row length 0..127,  F is special event flag
; 00 equ end of song data
;if F flag is set,  check the lowest bit,  it is engine change or drum pointer
; RD00EEE1 is engine change
;  R equ phase reset flag (always set for engines 0 and 5)
;  E equ engine number*2 (0, 2, 4, 6, 8, 10, 12, 14)
;  D equ drum flag,  if set,  the drum param pointer follows
; xxxxxxx0 is drum pointer,  this is LSB,  MSB follows
;two note fields follows after the speed byte and optional bytes:
;$00 empty field,  $01 rest note,  otherwise MSB/LSB word of the divider/duty/phase
;drum param table follows,  entries always aligned to 2 byte (lowest bit is zero):
; 2 byte pointer to the sample data,  complete with added offset in frames
; 1 byte frames to be played (may vary depending on the offset)
; 1 byte volume 0..3 *3
; 1 byte pitch 0..8 *3

OP_NOP equ $00
OP_XORA equ $af
OP_RLCD equ $02
OP_SBCAA equ $9f

play:


	di
	push 	iy
	exx
	push 	hl
	exx
	ld 	c, (hl)
	inc 	hl
	ld 	b, (hl)
	inc 	hl
	ld 	(drumParam), bc
	push 	hl					;put song ptr to stack

	;hl 	acc1
	;de 	add1
	;bc 	sample counter
	;hl' 	acc2
	;de' 	add2
	;b' 	squeeker output acc
	;c' 	always 16
	;a' 	phaser output bit
	
	ld 	ix, 0					;ModPhase lfo's,  ixh equ ch1 ixl equ ch2
	ld 	hl, 0					;acc2
	ld 	de, 0					;add2
	ld 	b, 0					;squeker output acc
	ld 	c, hi_bit				;output bit mask
	exx
	ld 	hl, 0					;acc1
	ld 	de, 0					;add1
	xor 	a
	ex 	af,af'						;phaser output bit

playRow:ex 	(sp), hl				;get song ptr,  store acc1
	
loopRow:ld 	a, (hl)					;row length and flags
	inc 	hl
	or 	a
	jp 	nz, setRowLen
	ld 	a, (hl)					;go loop
	inc 	hl
	ld 	h, (hl)
	ld 	l, a
	jp 	loopRow
	
setRowLen:push 	af						;row length
	jp 	p, readNotes
	ld 	a, (hl)
	inc	hl
	bit 	0, a
	jp 	nz, engineChange
	ld 	c, a
	jp 	drumCall
engineChange:
	push 	af
	push 	hl
	ld 	(phaseReset1), a			;> equ 128 means it uses phase reset
	ld 	(phaseReset2), a
	ld 	hl, engineList
	and 	$3e
	add 	a, l
	ld 	l, a
	jr 	nc, $+3
	inc 	h
	ld 	a, (hl)
	inc 	hl
	ld 	h, (hl)
	ld 	l, a
	ld 	(engineJump), hl
	pop 	hl
	pop 	af
	and 	$40
	jr 	z, readNotes	
	ld 	c, (hl)
	inc 	hl	
drumCall:
	call 	playDrum

readNotes:
	pop 	af
	and 	$7f
	ld 	b, a				;row time*256/4 for better time resolution
	ld 	c, 0
	srl 	b
	rr 	c
	srl 	b
	rr 	c
	ld 	a, (hl)				;ch1
	inc	hl
	or 	a
	jr 	z, skipCh1
	dec 	a
	jp 	nz, noteCh1				;mute ch1
	ld 	(tt_duty1), a			;reset duty1
	ld 	(ttev_duty1), a
	ld 	(ttqn_duty1), a
	ld 	(sq_duty1), a
	ld 	(mod_mute1), a		;nop
	ld 	(mod_alt1), a
	pop 	de					;get acc1 off the stack
	ld 	d, a					;reset add1
	ld 	e, a
	push 	de					;put acc1 back to the stack,  now it is zero	
	jp 	skipCh1
noteCh1:inc 	a
	ld 	d, a
	rra						;duty1 for squeeker
	rra
	rra
	and 	$0e
	add 	a, a
	inc 	a
	ld 	(sq_duty1), a
	ld 	a, d
	and 	$f0					;duty1 for tritone
	cp 	$80
	jr 	nz, $+5				;reset phase for ModPhase's non-zero W
	ld 	ixh, $80
	ld 	(tt_duty1), a
	ld 	(ttev_duty1), a
	ld 	(ttqn_duty1), a
	add 	a, a
	ld 	(mod_alt1), a
	ld 	a, OP_SBCAA
	ld 	(mod_mute1), a
	jp z, noPhase1			;phase reset

phaseReset1 equ $+1
	ld a, 0					;
	rla						;
	jr nc, noPhase1			;
	ld a, d					;
	and $f0					;
	sub $80					;to keep compatibility
	ex (sp), hl				;set phase
	ld h, a					;
	ld l, 0					;
	ex (sp), hl				;
	
noPhase1:

	ld a, d
	and $0f
	ld d, a					;add1 msb
	
	ld e, (hl)				;add1 lsb
	inc hl

skipCh1:
	
	ld a, (hl)				;ch2
	inc hl
	or a
	jr z, skipCh2
	dec a
	jp nz, noteCh2
	
							;mute ch2
	ld (tt_duty2), a			;reset duty2
	ld (ttev_duty2), a
	ld (ttln_duty2), a
	ld (sq_duty2), a
	ld (mod_mute2), a		;nop
	exx
	ld h, a					;reset acc2
	ld l, a
	ld d, a					;reset add2
	ld e, a
	exx
	add a, a
	ld (mod_alt2), a
	
	jp skipCh2
	
noteCh2:

	inc a
	exx
	ld d, a
	
	rra						;duty2 for squeeker
	rra
	rra
	and $0e
	add a, a
	inc a
	ld (sq_duty2), a
	
	ld a, d
	and $f0					;duty2 for tritone
	cp $80
	jp nz, $+5				;reset phase for ModPhase's non-zero W
	ld ixl, $80
	ld (tt_duty2), a
	ld (ttev_duty2), a
	ld (ttln_duty2), a
	ld (mod_alt2), a
	ld a, OP_SBCAA
	ld (mod_mute2), a
	
	jp z, noPhase2			;phase reset

phaseReset2 equ $+1
	ld 	a, 0					;
	rla						;
	jr 	nc, noPhase2			;
	ld 	a, d					;
	and 	$f0					;
	sub 	$80					;to keep compatibility
	ld 	h, a					;set phase
	ld 	l, 0					;
	
noPhase2:
	ld 	a, d
	and 	$0f
	ld 	d, a					;add2 msb
	exx
	ld 	a, (hl)
	inc 	hl
	exx
	ld 	e, a					;add2 lsb
	exx
	
skipCh2:ex (sp), hl				;get acc1,  store song ptr

engineJump equ $+1
	jp 0

	
	
;Engine 1: EarthShaker-alike

soundLoopES:

	add hl, de				;11
	
	jr nc, soundLoopES1S		;7/12-+
	xor a					;4    |
	out (port), a				;11   |
	jp soundLoopES1			;10---+-32t
	
soundLoopES1S:	

	jp $+3					;10   |
	jp $+3					;10---+-32t
	
soundLoopES1:
	exx						;4
	add hl, de				;11
	jr nc, soundLoopES2S		;7/12
	ld a, c					;4
	out (port), a				;11
	

	jp soundLoopES2			;10
	
soundLoopES2S:

	jp $+3					;10
	jp $+3					;10
	
soundLoopES2:

	exx						;4
	
	dec  bc					;6
	ld   a, b				;4
	or   c					;4
	jr	nz, soundLoopES		;12 equ 120t
	
;	in a, (port)				;check keyboard
;	cpl
;	and $1f
;	jp z, playRow



	jp  	playRow

	jp 	stopPlayer

	
	
;Engine 2: Tritone-alike with two tone channels of uneven volume (33/87t)

soundLoopTT:

	add hl, de				;11
	
	ld a, h					;4
	
tt_duty1 equ $+1
	cp $80					;7
	
	sbc a, a					;4
	and 1					;7
	
	
	exx						;4
	
	add hl, de				;11
	
	out (port), a				;11
	
	ld a, h					;4
	
tt_duty2 equ $+1
	cp $80					;7

	sbc a, a					;4
	and 1					;7
	out (port), a				;11



	exx						;4

	dec  bc					;6
	ld   a, b				;4
	or   c					;4
	jp	nz, soundLoopTT		;10 equ 120t

;	in a, (port)				;check keyboard
;	cpl
;	and $1f
;	jp z, playRow

	jp  playRow

	jp stopPlayer
	
	
	
;Engine 3: Tritone-alike with two tone channels with even volumes (mostly,  58/62t)

soundLoopTTEV:

	add hl, de				;11
	
	ld a, h					;4
	
ttev_duty1 equ $+1
	cp $80					;7
	
	sbc a, a					;4
	and 1					;7
	out (port), a				;11



	
	exx						;4
	add hl, de				;11
	ld a, h					;4
	
ttev_duty2 equ $+1
	cp $80					;7

	sbc a, a					;4
	and 1					;7
	
	
	exx						;4

	dec  bc					;6
	out (port), a				;11


	ld   a, b				;4
	or   c					;4
	jp	nz, soundLoopTTEV	;10 equ 120t

;	in a, (port)				;check keyboard
;	cpl
;	and $1f
;	jp z, playRow
	jp  playRow

	jp stopPlayer
	
	
	
;Engine 4: Tritone-alike quiet tone channel,  loud noise channel

soundLoopTTLN:

	add hl, de				;11
	
	rlc h					;8
	ld a, h					;4
	exx						;4
	and c					;4
	
	add hl, de				;11
	
	out (port), a				;11

	
	ld a, h					;4
	
ttln_duty2 equ $+1
	cp $80					;7

	sbc a, a					;4
	and c					;4
	out (port), a				;11


	exx						;4

	ld a, r					;9	to align to 120t
	dec  bc					;6
	ld   a, b				;4
	or   c					;4
	jp	nz, soundLoopTTLN	;10 equ 120t

;	in a, (port)				;check keyboard
;	cpl
;	and $1f
;	jp z, playRow

	jp	playRow

	jp stopPlayer
	
	
	
;Engine 5: Tritone-alike quiet noise channel,  loud tone channel

soundLoopTTQN:

	add hl, de				;11
	
	ld a, h					;4
	
ttqn_duty1 equ $+1
	cp $80					;7

	sbc a, a					;4
	exx						;4
	and c					;4
	
	add hl, de				;11
	
	out (port), a				;11


	rlc h					;8

	ld a, h					;4
	and c					;4
	out (port), a				;11


	exx						;4

	ld a, r					;9	to align to 120t
	dec  bc					;6
	ld   a, b				;4
	or   c					;4
	jp	nz, soundLoopTTQN	;10 equ 120t

;	in a, (port)				;check keyboard
;	cpl
;	and $1f
;	jp z, playRow
	jp	playRow

	jp stopPlayer
	

	
;Engine 6: Phaser-alike,  single channel,  two oscillators controlled directly

soundLoopPHA:

	ex af,af'						;4
	
    add hl, de      	 		;11
    jr c, $+4        		;7/12-+
    jr $+4          		;7/12 |
    xor 1         	 		;7   -+19t

	
	exx						;4
    add hl, de       		;11
    jr c, $+4       			;7/12-+
    jr $+4          		;7/12 |
    xor 1         	 		;7   -+19t
    out (port), a     		;11

	exx						;4
	
	ex af,af'						;4
	ld a, r					;9	to align to 120t
	
	dec  bc					;6
	ld   a, b				;4
	or   c					;4
	jp	nz, soundLoopPHA		;10 equ 120t

;	in a, (port)				;check keyboard
;	cpl
;	and $1f
;	jp z, playRow
	jp	playRow

	jp stopPlayer
	
	
	
;Engine 7: Squeeker-alike,  two tone channels with duty control

soundLoopSQ:

    ld a, c					;correct the loop counter for the double 8-bit counter
    dec bc
    inc b
	ld c, a
	
soundLoopSQ1:

	add hl, de				;11
	sbc a, a					;4
sq_duty1 equ $+1
	and 8*2					;7 (0..7 duty*2+1)

	exx						;4

	add a, b					;4
	ld b, a					;4
	
	add hl, de				;11
	sbc a, a					;4
sq_duty2 equ $+1
	and 8*2					;7
	add a, b					;4

	ld b, $ff				;7
	add a, b					;4
	sbc a, b					;4
	ld b, a					;4
	sbc a, a					;4

	and c					;4
	out (port), a				;11




	exx						;4
	nop						;4
	
	dec c					;4 double 8-bit loop counter
	jp nz, soundLoopSQ1		;10 equ 120t
	dec b					;Sqeeker-like engines are much forgiving for floating loop times, 
	jp nz, soundLoopSQ1		;so this is an acceptable compromise to fit the average loop time into 120t

;	in a, (port)				;check keyboard
;	cpl
;	and $1f
;	jp z, playRow

	jp	playRow
	jp stopPlayer
	
	
	
;Engine 8: CrossPhase,  another PWM modulation engine similar to Phaser1,  single channel,  two oscillators controlled directly

soundLoopCPA:

    add hl, de      	 		;11
	ld a, h					;4
	exx						;4
    add hl, de       		;11
	cp h					;4
	exx						;4
	sbc a, a					;4
	and 1					;7
	out (port), a				;11

	
	jr $+2					;12
	jr $+2					;12
	jr $+2					;12
	
	dec  bc					;6
	ld   a, b				;4
	or   c					;4
	jp	nz, soundLoopCPA		;10 equ 120t

;	in a, (port)				;check keyboard
;	cpl
;	and $1f
;	jp z, playRow
	jp	playRow
	jp stopPlayer
	


;Engine 9: ModPhase,  PWM modulation engine,  two tone channels of uneven volume with a mod alteration control

soundLoopMOD:

    	ld a, c					;correct the loop counter for the double 8-bit counter
    	dec bc
    	inc b
	ld c, a
	
soundLoopMOD1:

    add hl, de      	 		;11
	ld a, h					;4
mod_alt1 equ $+1
	xor 0					;7
	cp ixh					;8
mod_mute1 equ $
	sbc a, a					;4
	exx						;4
	
    add hl, de      	 		;11
	out (port), a				;11
	


	ld a, h					;4
mod_alt2 equ $+1
	xor 0					;7
	cp ixl					;8
mod_mute2 equ $
	sbc a, a					;4
	

	out (port), a				;11

	exx						;4

	nop						;4
	nop						;4
	
	dec c					;4 double 8-bit loop counter
	jp nz, soundLoopMOD1		;10 equ 120t
	inc ixh
	inc ixl
	dec b
	jp nz, soundLoopMOD1

;	in a, (port)				;check keyboard
;	cpl
;	and $1f
;	jp z, playRow
	jp	playRow
	jp stopPlayer
	
	
	
stopPlayer:

	pop hl					;song pointer/acc1 word,  not needed anymore
	pop hl					;restore HL'
	exx
	pop iy
	ei
	ret

	
	
engineList:

	;engines 1, 6, 8 use the W column/top bits for phase reset,  all others use it as duty cycle
	
	dw soundLoopES		;1 EarthShaker-alike
	dw soundLoopTT		;2 Tritone-alike with uneven volumes
	dw soundLoopTTEV	;3 Tritone-alike with equal volumes
	dw soundLoopTTLN	;4 Tritone-alike with quiet tone channel,  loud noise channel
	dw soundLoopTTQN	;5 Tritone-alike with quiet noise channel,  loud tone channel
	dw soundLoopPHA		;6 Phaser-alike (single channel)
	dw soundLoopSQ		;7 Squeeker-alike
	dw soundLoopCPA		;8 CrossPhase
	dw soundLoopMOD		;9 ModPhase
	
	
;C equ drum param number

playDrum:

	push de
	push hl

	ld b, 0
	ld h, b
	ld l, c
	srl c
	add hl, hl		;C already *2,  another *2
	add hl, bc		;+1 to have *5
drumParam equ $+1
	ld bc, 0
	add hl, bc
	
	ld a, (hl)		;drum sample pointer,  complete with precalculated offset
	ld (drumPtr+0), a
	inc hl
	ld a, (hl)
	ld (drumPtr+1), a
	inc hl
	ld a, (hl)		;frames to be played
	ld (drumFrames), a
	inc hl
	ld a, (hl)		;volume*3
	ld (drumVolume), a
	inc hl
	ld a, (hl)		;pitch*8
	ld (drumPitch), a

drumVolume equ $+1
	ld a, 0
	ld hl, volTable
	add a, l
	ld l, a
	jr nc, $+3
	inc h
	
	ld a, (hl)
	inc hl
	ld (drumVol01), a
	ld (drumVol11), a
	ld (drumVol21), a
	ld (drumVol31), a
	ld (drumVol41), a
	ld (drumVol51), a
	ld (drumVol61), a
	ld (drumVol71), a
	ld a, (hl)
	inc hl
	ld (drumVol02), a
	ld (drumVol12), a
	ld (drumVol22), a
	ld (drumVol32), a
	ld (drumVol42), a
	ld (drumVol52), a
	ld (drumVol62), a
	ld (drumVol72), a
	ld a, (hl)
	ld (drumVol03), a
	ld (drumVol13), a
	ld (drumVol23), a
	ld (drumVol33), a
	ld (drumVol43), a
	ld (drumVol53), a
	ld (drumVol63), a
	ld (drumVol73), a
		
drumPitch equ $+1
	ld a, 0
	ld hl, pitchTable
	add a, l
	ld l, a
	jr nc, $+3
	inc h
	
	ld a, (hl)
	inc hl
	ld (drumShift0), a
	ld a, (hl)
	inc hl
	ld (drumShift1), a
	ld a, (hl)
	inc hl
	ld (drumShift2), a
	ld a, (hl)
	inc hl
	ld (drumShift3), a
	ld a, (hl)
	inc hl
	ld (drumShift4), a
	ld a, (hl)
	inc hl
	ld (drumShift5), a
	ld a, (hl)
	inc hl
	ld (drumShift6), a
	ld a, (hl)
	ld (drumShift7), a
	
drumPtr equ $+1
	ld hl, 0
	
drumFrames equ $+1
	ld b, 0
	ld c, 0
	ld d, 1
	
drumLoop:

;bit 0

	ld a, (hl)				;7
	
	and d					;4
	jr nz, $+4				;7/12-+
	jr z, $+4				;7/12 |
	ld a, hi_bit				;7   -+19t

	out (port), a				;11



drumVol01 equ $
	nop						;4
	out (port), a				;11
	

	
drumVol02 equ $
	nop						;4
	out (port), a				;11
	


drumShift0 equ $+1
	rlc d					;8
	nop						;4

drumVol03 equ $
	nop						;4
	out (port), a				;11
	


	
	nop						;4
	nop						;4
	dec c					;4
	jp $+3					;10 equ 120t
	
;bit 1

	ld a, (hl)				;7
	
	and d					;4
	jr nz, $+4				;7/12-+
	jr z, $+4				;7/12 |
	ld a, hi_bit				;7   -+19t

	out (port), a				;11
	



drumVol11 equ $
	nop						;4
	out (port), a				;11
	



drumVol12 equ $
	nop						;4
	out (port), a				;11
	


drumShift1 equ $+1
	rlc d					;8
	nop						;4
	
drumVol13 equ $
	nop						;4
	out (port), a				;11
	

	
	nop						;4
	nop						;4
	dec c					;4
	jp $+3					;10 equ 120t
	
;bit 2

	ld a, (hl)				;7
	
	and d					;4
	jr nz, $+4				;7/12-+
	jr z, $+4				;7/12 |
	ld a, hi_bit				;7   -+19t

	out (port), a				;11
	


drumVol21 equ $
	nop						;4
	out (port), a				;11
	


drumVol22 equ $
	nop						;4
	out (port), a				;11
	


drumShift2 equ $+1
	rlc d					;8
	nop						;4
	
drumVol23 equ $
	nop						;4
	out (port), a				;11
	

	
	nop						;4
	nop						;4
	dec c					;4
	jp $+3					;10 equ 120t
	
;bit 3

	ld a, (hl)				;7
	
	and d					;4
	jr nz, $+4				;7/12-+
	jr z, $+4				;7/12 |
	ld a, hi_bit				;7   -+19t

	out (port), a				;11
	


drumVol31 equ $
	nop						;4
	
	 out (port), a				;11
	
drumVol32 equ $
	nop						;4
	
	 out (port), a				;11
drumShift3 equ $+1
	rlc d					;8
	nop						;4
	
drumVol33 equ $
	nop						;4
	
	 out (port), a				;11
	
	nop						;4
	nop						;4
	dec c					;4
	jp $+3					;10 equ 120t
	
;bit 4

	ld a, (hl)				;7
	
	and d					;4
	jr nz, $+4				;7/12-+
	jr z, $+4				;7/12 |
	ld a, hi_bit				;7   -+19t

	
	 out (port), a				;11

drumVol41 equ $
	nop						;4
	
	 out (port), a				;11
	
drumVol42 equ $
	nop						;4
	
	 out (port), a				;11
drumShift4 equ $+1
	rlc d					;8
	nop						;4
	
drumVol43 equ $
	nop						;4
	
	 out (port), a				;11
	
	nop						;4
	nop						;4
	dec c					;4
	jp $+3					;10 equ 120t
	
;bit 5

	ld a, (hl)				;7
	
	and d					;4
	jr nz, $+4				;7/12-+
	jr z, $+4				;7/12 |
	ld a, hi_bit				;7   -+19t

	
	 out (port), a				;11

drumVol51 equ $
	nop						;4
	
	 out (port), a				;11
	
drumVol52 equ $
	nop						;4
	
	 out (port), a				;11
drumShift5 equ $+1
	rlc d					;8
	nop						;4
	
drumVol53 equ $
	nop						;4
	
	 out (port), a				;11
	
	nop						;4
	nop						;4
	dec c					;4
	jp $+3					;10 equ 120t
	
;bit 6

	ld a, (hl)				;7
	
	and d					;4
	jr nz, $+4				;7/12-+
	jr z, $+4				;7/12 |
	ld a, hi_bit				;7   -+19t

	
	 out (port), a				;11

drumVol61 equ $
	nop						;4
	
	 out (port), a				;11
	
drumVol62 equ $
	nop						;4
	
	 out (port), a				;11
drumShift6 equ $+1
	rlc d					;8
	nop						;4

drumVol63 equ $
	nop						;4
	
	 out (port), a				;11
	
	nop						;4
	nop						;4
	dec c					;4
	jp $+3					;10 equ 120t
	
;bit 7

	ld a, (hl)				;7
	
	and d					;4
	jr nz, $+4				;7/12-+
	jr z, $+4				;7/12 |
	ld a, hi_bit				;7   -+19t

	
	 out (port), a				;11

drumVol71 equ $
	nop						;4
	
	 out (port), a				;11
	
drumVol72 equ $
	nop						;4
	
	 out (port), a				;11
drumShift7 equ $+1
	rlc d					;8
	nop						;4

drumVol73 equ $
	nop						;4
	
	 out (port), a				;11
	
	inc hl					;6
	jp $+3					;10
	dec c					;4
	jp nz, drumLoop			;10 equ 128t a bit longer iteration
	
	nop						;4 aligned to 8t just in case
	dec b					;4
	jp nz, drumLoop			;10

	pop hl
	pop de

	ret
	
	
	
volTable:

	db OP_XORA, OP_NOP , OP_NOP
	db OP_NOP , OP_XORA, OP_NOP
	db OP_NOP , OP_NOP , OP_XORA
	db OP_NOP , OP_NOP , OP_NOP
		
pitchTable:

	db OP_NOP , OP_NOP , OP_NOP , OP_NOP , OP_NOP , OP_NOP , OP_NOP , OP_NOP
	db OP_RLCD, OP_NOP , OP_NOP , OP_NOP , OP_NOP , OP_NOP , OP_NOP , OP_NOP
	db OP_RLCD, OP_NOP , OP_NOP , OP_NOP , OP_RLCD, OP_NOP , OP_NOP , OP_NOP
	db OP_RLCD, OP_NOP , OP_RLCD, OP_NOP , OP_RLCD, OP_NOP , OP_NOP , OP_NOP
	db OP_RLCD, OP_NOP , OP_RLCD, OP_NOP , OP_RLCD, OP_NOP , OP_RLCD, OP_NOP
	db OP_RLCD, OP_RLCD, OP_NOP , OP_RLCD, OP_RLCD, OP_NOP , OP_RLCD, OP_NOP
	db OP_RLCD, OP_RLCD, OP_RLCD, OP_NOP , OP_RLCD, OP_RLCD, OP_RLCD, OP_NOP
	db OP_RLCD, OP_RLCD, OP_RLCD, OP_RLCD, OP_RLCD, OP_RLCD, OP_RLCD, OP_NOP
	db OP_RLCD, OP_RLCD, OP_RLCD, OP_RLCD, OP_RLCD, OP_RLCD, OP_RLCD, OP_RLCD




; TRACK06

;compiled music data

music_data
 dw .drumpar
.song
 db $a0,$91,$f1,$05,$80,$80
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f0,$f7,$80,$79
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f0,$c4,$80,$60
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f0,$ae,$80,$55
 db $20,$00,$00
 db $20,$f0,$c4,$80,$60
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f1,$05,$80,$80
 db $20,$00,$00
 db $20,$f0,$f7,$80,$79
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f0,$c4,$80,$60
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f0,$ae,$80,$55
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f1,$05,$80,$80
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f0,$f7,$80,$79
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f0,$c4,$80,$60
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f0,$ae,$80,$55
 db $20,$00,$00
 db $20,$f0,$c4,$80,$60
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f1,$05,$80,$80
 db $20,$00,$00
 db $20,$f0,$f7,$80,$79
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f0,$c4,$80,$60
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f1,$05,$80,$80
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f0,$f7,$80,$79
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f0,$c4,$80,$60
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f0,$ae,$80,$55
 db $20,$00,$00
 db $20,$f0,$c4,$80,$60
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f1,$05,$80,$80
 db $20,$00,$00
 db $20,$f0,$f7,$80,$79
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f0,$c4,$80,$60
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f0,$ae,$80,$55
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f1,$05,$80,$80
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f0,$f7,$80,$79
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f0,$c4,$80,$60
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f0,$ae,$80,$55
 db $20,$00,$00
 db $20,$f0,$c4,$90,$60
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $98,$00,$f1,$05,$80,$80
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $9c,$02,$f0,$f7,$80,$79
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $94,$04,$f0,$c4,$80,$60
 db $20,$00,$00
 db $9c,$02,$00,$00
 db $20,$00,$00
 db $98,$00,$f0,$ae,$80,$55
 db $20,$00,$00
 db $98,$00,$f0,$c4,$80,$60
 db $20,$00,$00
 db $9c,$06,$00,$00
 db $20,$00,$00
 db $98,$08,$f1,$05,$80,$80
 db $20,$00,$00
 db $94,$0a,$f0,$f7,$80,$79
 db $20,$00,$00
 db $9c,$06,$00,$00
 db $20,$00,$00
 db $94,$0a,$f0,$c4,$80,$60
 db $20,$00,$00
 db $9c,$06,$00,$00
 db $20,$00,$00
 db $94,$0a,$f0,$ae,$80,$55
 db $20,$00,$00
 db $98,$08,$00,$00
 db $20,$00,$00
 db $98,$0c,$f1,$05,$80,$80
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $9c,$0e,$f0,$f7,$80,$79
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $94,$10,$f0,$c4,$80,$60
 db $20,$00,$00
 db $9c,$0e,$00,$00
 db $20,$00,$00
 db $98,$0c,$f0,$ae,$80,$55
 db $20,$00,$00
 db $98,$0c,$f0,$c4,$80,$60
 db $20,$00,$00
 db $9c,$12,$00,$00
 db $20,$00,$00
 db $98,$14,$f1,$05,$80,$80
 db $20,$00,$00
 db $94,$16,$f0,$f7,$80,$79
 db $20,$00,$00
 db $9c,$12,$00,$00
 db $20,$00,$00
 db $94,$16,$f0,$c4,$80,$60
 db $20,$00,$00
 db $9c,$12,$00,$00
 db $20,$00,$00
 db $94,$16,$00,$00
 db $20,$00,$00
 db $98,$14,$00,$00
 db $20,$00,$00
 db $98,$18,$f1,$05,$80,$80
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $9c,$1a,$f0,$f7,$80,$79
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $94,$1c,$f0,$c4,$80,$60
 db $20,$00,$00
 db $9c,$1a,$00,$00
 db $20,$00,$00
 db $98,$18,$f0,$ae,$80,$55
 db $20,$00,$00
 db $98,$18,$f0,$c4,$80,$60
 db $20,$00,$00
 db $9c,$1e,$00,$00
 db $20,$00,$00
 db $98,$20,$f1,$05,$80,$80
 db $20,$00,$00
 db $94,$22,$f0,$f7,$80,$79
 db $20,$00,$00
 db $9c,$1e,$00,$00
 db $20,$00,$00
 db $94,$22,$f0,$c4,$80,$60
 db $20,$00,$00
 db $9c,$1e,$00,$00
 db $20,$00,$00
 db $94,$22,$f0,$ae,$80,$55
 db $20,$00,$00
 db $98,$20,$00,$00
 db $20,$00,$00
 db $98,$24,$f0,$c4,$90,$60
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $9c,$26,$00,$01
 db $20,$00,$00
 db $20,$00,$90,$60
 db $20,$00,$00
 db $94,$28,$00,$00
 db $20,$00,$00
 db $9c,$26,$00,$01
 db $20,$00,$00
 db $98,$24,$00,$90,$60
 db $20,$00,$00
 db $98,$24,$00,$00
 db $20,$00,$00
 db $9c,$26,$00,$01
 db $20,$00,$00
 db $98,$2a,$00,$00
 db $20,$00,$00
 db $94,$2c,$00,$90,$60
 db $20,$00,$00
 db $20,$01,$01
 db $20,$00,$00
 db $94,$2c,$f0,$c4,$a0,$60
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$01
 db $20,$00,$00
 db $98,$00,$80,$62,$80,$60
 db $20,$00,$00
 db $20,$80,$6e,$80,$6c
 db $20,$00,$00
 db $9c,$02,$01,$82,$4b
 db $20,$00,$00
 db $20,$00,$82,$2a
 db $20,$00,$00
 db $94,$04,$a2,$4b,$00
 db $20,$00,$00
 db $9c,$02,$a2,$2a,$82,$0b
 db $20,$00,$00
 db $98,$00,$80,$62,$81,$ee
 db $20,$00,$00
 db $98,$00,$80,$6e,$80,$6c
 db $20,$00,$00
 db $9c,$06,$81,$ee,$01
 db $20,$00,$00
 db $98,$43,$08,$f2,$0b,$00
 db $20,$00,$00
 db $94,$0a,$f1,$ee,$00
 db $20,$00,$00
 db $9c,$06,$01,$00
 db $20,$00,$00
 db $94,$0a,$00,$00
 db $20,$00,$00
 db $9c,$06,$00,$00
 db $20,$00,$00
 db $94,$0a,$00,$00
 db $20,$00,$00
 db $98,$08,$00,$00
 db $20,$00,$00
 db $98,$d1,$0c,$80,$62,$80,$60
 db $20,$00,$00
 db $20,$80,$6e,$80,$6c
 db $20,$00,$00
 db $9c,$0e,$01,$a1,$5d
 db $20,$00,$00
 db $20,$00,$a1,$72
 db $20,$00,$00
 db $94,$10,$a1,$5d,$01
 db $20,$00,$00
 db $9c,$0e,$a1,$72,$00
 db $20,$00,$00
 db $98,$0c,$80,$62,$a1,$88
 db $20,$00,$00
 db $98,$0c,$80,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$12,$81,$88,$01
 db $20,$00,$00
 db $98,$14,$81,$b8,$00
 db $20,$00,$00
 db $94,$43,$16,$f1,$88,$00
 db $20,$00,$00
 db $9c,$12,$f1,$b8,$00
 db $20,$00,$00
 db $94,$16,$01,$00
 db $20,$00,$00
 db $9c,$12,$00,$00
 db $20,$00,$00
 db $94,$16,$00,$00
 db $20,$00,$00
 db $98,$14,$00,$00
 db $20,$00,$00
 db $98,$d1,$18,$80,$62,$80,$60
 db $20,$00,$00
 db $20,$80,$6e,$80,$6c
 db $20,$00,$00
 db $9c,$1a,$01,$82,$2a
 db $20,$00,$82,$4b
 db $20,$00,$82,$2a
 db $20,$00,$00
 db $94,$1c,$a2,$2a,$00
 db $20,$a2,$4b,$00
 db $9c,$1a,$a2,$2a,$00
 db $20,$00,$00
 db $98,$18,$80,$62,$82,$0b
 db $20,$00,$00
 db $98,$18,$80,$6e,$81,$d2
 db $20,$00,$81,$ee
 db $9c,$1e,$82,$0b,$01
 db $20,$00,$00
 db $98,$20,$81,$d2,$00
 db $20,$81,$ee,$00
 db $81,$43,$2e,$f2,$0b,$00
 db $17,$00,$00
 db $9c,$1e,$f1,$d2,$00
 db $20,$f1,$ee,$00
 db $94,$22,$01,$00
 db $20,$00,$00
 db $81,$30,$00,$00
 db $17,$00,$00
 db $94,$22,$00,$00
 db $20,$00,$00
 db $98,$20,$00,$00
 db $20,$00,$00
 db $98,$d1,$24,$80,$62,$81,$ee
 db $20,$00,$00
 db $20,$80,$6e,$80,$6c
 db $20,$00,$00
 db $9c,$26,$81,$ee,$00
 db $20,$00,$00
 db $20,$01,$82,$0b
 db $20,$00,$00
 db $94,$28,$a1,$ee,$01
 db $20,$00,$00
 db $9c,$26,$82,$0b,$00
 db $20,$00,$00
 db $98,$24,$80,$62,$80,$60
 db $20,$00,$00
 db $98,$24,$80,$6e,$80,$6c
 db $20,$00,$00
 db $9c,$26,$a1,$ee,$01
 db $20,$01,$00
 db $98,$2a,$00,$00
 db $20,$00,$00
 db $81,$32,$00,$00
 db $17,$00,$00
 db $20,$a2,$0b,$00
 db $20,$01,$00
 db $81,$34,$00,$00
 db $17,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $81,$36,$00,$00
 db $17,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $98,$00,$80,$62,$80,$60
 db $20,$00,$00
 db $20,$80,$6e,$80,$6c
 db $20,$00,$00
 db $9c,$02,$01,$82,$4b
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $94,$04,$82,$4b,$00
 db $20,$00,$00
 db $9c,$02,$00,$82,$2a
 db $20,$00,$00
 db $98,$00,$80,$62,$82,$0b
 db $20,$00,$81,$ee
 db $98,$00,$80,$6e,$82,$0b
 db $20,$00,$00
 db $9c,$06,$82,$0b,$01
 db $20,$81,$ee,$00
 db $98,$08,$82,$0b,$00
 db $20,$00,$00
 db $94,$0a,$93,$dc,$00
 db $20,$01,$00
 db $9c,$06,$a3,$dc,$00
 db $20,$01,$00
 db $94,$0a,$b3,$dc,$00
 db $20,$01,$00
 db $9c,$06,$c3,$dc,$00
 db $20,$01,$00
 db $94,$0a,$00,$00
 db $20,$00,$00
 db $98,$08,$00,$00
 db $20,$00,$00
 db $98,$0c,$80,$62,$80,$60
 db $20,$00,$00
 db $20,$80,$6e,$80,$6c
 db $20,$00,$00
 db $9c,$0e,$01,$a1,$5d
 db $20,$00,$00
 db $20,$00,$a1,$72
 db $20,$00,$00
 db $94,$10,$81,$5d,$01
 db $20,$00,$00
 db $9c,$0e,$81,$72,$00
 db $20,$00,$00
 db $98,$0c,$80,$62,$a1,$88
 db $20,$00,$00
 db $98,$0c,$80,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$12,$81,$88,$01
 db $20,$00,$00
 db $98,$14,$81,$b8,$00
 db $20,$00,$00
 db $94,$16,$93,$dc,$00
 db $20,$01,$00
 db $9c,$12,$a3,$dc,$00
 db $20,$01,$00
 db $94,$16,$b3,$dc,$00
 db $20,$01,$00
 db $9c,$12,$c3,$dc,$00
 db $20,$01,$00
 db $94,$16,$00,$00
 db $20,$00,$00
 db $98,$14,$00,$00
 db $20,$00,$00
 db $98,$18,$80,$62,$80,$60
 db $20,$00,$00
 db $20,$80,$6e,$80,$6c
 db $20,$00,$00
 db $9c,$1a,$01,$c1,$ee
 db $20,$00,$c1,$b8
 db $20,$00,$00
 db $20,$00,$00
 db $94,$1c,$81,$b8,$00
 db $20,$00,$00
 db $9c,$1a,$00,$c2,$2a
 db $20,$00,$00
 db $98,$18,$80,$62,$c2,$0b
 db $20,$00,$00
 db $98,$18,$80,$6e,$c1,$ee
 db $20,$00,$00
 db $9c,$1e,$82,$0b,$01
 db $20,$00,$00
 db $98,$20,$81,$ee,$00
 db $20,$00,$00
 db $81,$2e,$93,$dc,$00
 db $17,$01,$00
 db $9c,$1e,$a3,$dc,$00
 db $20,$01,$00
 db $94,$22,$b3,$dc,$00
 db $20,$01,$00
 db $81,$30,$c3,$dc,$00
 db $17,$01,$00
 db $94,$22,$00,$00
 db $20,$00,$00
 db $98,$20,$00,$00
 db $20,$00,$00
 db $98,$24,$80,$62,$d1,$88
 db $20,$00,$00
 db $20,$80,$6e,$d1,$b8
 db $20,$00,$00
 db $9c,$26,$81,$88,$01
 db $20,$00,$00
 db $20,$81,$b8,$00
 db $20,$00,$00
 db $94,$28,$a1,$88,$00
 db $20,$00,$00
 db $9c,$26,$a1,$b8,$00
 db $20,$00,$00
 db $98,$24,$80,$62,$83,$10
 db $20,$00,$00
 db $98,$24,$80,$6e,$93,$70
 db $20,$00,$00
 db $9c,$26,$83,$10,$01
 db $20,$00,$00
 db $98,$2a,$83,$70,$00
 db $20,$00,$00
 db $94,$2c,$a3,$10,$00
 db $20,$00,$00
 db $20,$a3,$70,$00
 db $20,$00,$00
 db $94,$2c,$01,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$01
 db $20,$00,$00
 db $9c,$4d,$3a,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$01
 db $20,$00,$00
 db $a0,$91,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$c1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$d1,$b8
 db $20,$00,$00
 db $a0,$91,$e1,$88,$01
 db $20,$00,$00
 db $94,$3e,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$91,$03,$92,$0b
 db $20,$00,$00
 db $9c,$d1,$3a,$91,$b8,$01
 db $20,$00,$00
 db $88,$4d,$3c,$a0,$f5,$a1,$ee
 db $20,$00,$00
 db $a0,$91,$a2,$0b,$01
 db $20,$00,$00
 db $94,$4d,$3e,$b0,$da,$b1,$b8
 db $20,$00,$00
 db $a0,$91,$b1,$ee,$01
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$91,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$01
 db $20,$00,$00
 db $9c,$4d,$3a,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$01
 db $20,$00,$00
 db $a0,$91,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$b1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $a0,$91,$01,$a1,$88
 db $20,$00,$00
 db $94,$3e,$00,$a1,$b8
 db $20,$00,$00
 db $98,$38,$81,$88,$01
 db $20,$00,$00
 db $9c,$3a,$81,$b8,$00
 db $20,$00,$00
 db $88,$3c,$a1,$88,$00
 db $20,$00,$00
 db $20,$a1,$b8,$00
 db $20,$00,$00
 db $94,$3e,$c1,$88,$00
 db $20,$00,$00
 db $20,$c1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$01
 db $20,$00,$00
 db $9c,$4d,$3a,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$01
 db $20,$00,$00
 db $a0,$91,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$c1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$d1,$b8
 db $20,$00,$00
 db $a0,$91,$e1,$88,$01
 db $20,$00,$00
 db $94,$3e,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$91,$03,$92,$0b
 db $20,$00,$00
 db $9c,$3a,$a0,$f5,$81,$ee
 db $20,$00,$00
 db $88,$d1,$3c,$82,$0b,$01
 db $20,$00,$00
 db $20,$81,$ee,$00
 db $20,$00,$00
 db $94,$4d,$3e,$b0,$da,$b1,$b8
 db $20,$00,$00
 db $a0,$91,$b1,$ee,$01
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$01
 db $20,$00,$00
 db $9c,$4d,$3a,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$01
 db $20,$00,$00
 db $a0,$91,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$c1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$d1,$b8
 db $20,$00,$00
 db $88,$d1,$3c,$01,$d1,$88
 db $20,$00,$00
 db $98,$38,$c1,$88,$a0,$62
 db $20,$c2,$2a,$01
 db $94,$3e,$c2,$4b,$a0,$62
 db $20,$b1,$88,$01
 db $88,$3c,$b2,$2a,$a0,$62
 db $20,$b2,$4b,$01
 db $98,$38,$a1,$88,$a0,$62
 db $20,$a2,$2a,$01
 db $94,$3e,$a2,$4b,$a0,$62
 db $20,$91,$88,$01
 db $88,$3c,$92,$2a,$a0,$62
 db $20,$92,$4b,$01
 db $94,$3e,$81,$88,$a0,$62
 db $20,$82,$2a,$01
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$01
 db $20,$00,$00
 db $9c,$4d,$3a,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$01
 db $20,$00,$00
 db $a0,$91,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$c1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$d1,$b8
 db $20,$00,$00
 db $a0,$91,$e1,$88,$01
 db $20,$00,$00
 db $94,$3e,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$91,$03,$92,$0b
 db $20,$00,$00
 db $9c,$d1,$3a,$91,$b8,$01
 db $20,$00,$00
 db $88,$4d,$3c,$a0,$f5,$a1,$ee
 db $20,$00,$00
 db $a0,$91,$a2,$0b,$01
 db $20,$00,$00
 db $94,$4d,$3e,$b0,$da,$b1,$b8
 db $20,$00,$00
 db $a0,$91,$b1,$ee,$01
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$91,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$01
 db $20,$00,$00
 db $9c,$4d,$3a,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$01
 db $20,$00,$00
 db $a0,$91,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$b1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $a0,$91,$01,$a1,$88
 db $20,$00,$00
 db $94,$3e,$00,$a1,$b8
 db $20,$00,$00
 db $98,$38,$81,$88,$01
 db $20,$00,$00
 db $9c,$3a,$81,$b8,$00
 db $20,$00,$00
 db $88,$3c,$a1,$88,$00
 db $20,$00,$00
 db $20,$a1,$b8,$00
 db $20,$00,$00
 db $94,$3e,$c1,$88,$00
 db $20,$00,$00
 db $20,$c1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$01
 db $20,$00,$00
 db $9c,$4d,$3a,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$01
 db $20,$00,$00
 db $a0,$91,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$c1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$d1,$b8
 db $20,$00,$00
 db $a0,$91,$e1,$88,$01
 db $20,$00,$00
 db $94,$3e,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$91,$03,$92,$0b
 db $20,$00,$00
 db $9c,$3a,$a0,$f5,$81,$ee
 db $20,$00,$00
 db $88,$d1,$3c,$82,$0b,$01
 db $20,$00,$00
 db $20,$81,$ee,$00
 db $20,$00,$00
 db $94,$4d,$3e,$b0,$da,$b1,$b8
 db $20,$00,$00
 db $a0,$91,$b1,$ee,$01
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$01
 db $20,$00,$00
 db $9c,$4d,$3a,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$01
 db $20,$00,$00
 db $a0,$91,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$c1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$d1,$b8
 db $20,$00,$00
 db $88,$d1,$3c,$01,$d1,$88
 db $20,$00,$00
 db $20,$f0,$c4,$a0,$60
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$f0,$b9,$a0,$5c
 db $20,$f0,$ae,$a0,$57
 db $20,$f0,$a4,$a0,$52
 db $20,$f0,$9b,$a0,$4d
 db $98,$00,$80,$60,$80,$62
 db $20,$00,$00
 db $20,$80,$6c,$80,$6e
 db $20,$00,$00
 db $9c,$02,$01,$a2,$4b
 db $20,$00,$00
 db $20,$00,$a2,$2a
 db $20,$00,$00
 db $94,$04,$82,$4b,$00
 db $20,$00,$00
 db $9c,$02,$82,$2a,$a2,$0b
 db $20,$00,$00
 db $98,$00,$80,$60,$a1,$ee
 db $20,$00,$00
 db $98,$00,$80,$6c,$80,$6e
 db $20,$00,$00
 db $9c,$06,$81,$ee,$01
 db $20,$00,$00
 db $98,$08,$a2,$0b,$00
 db $20,$00,$00
 db $94,$0a,$a1,$ee,$00
 db $20,$00,$00
 db $9c,$43,$06,$f2,$0b,$00
 db $20,$00,$00
 db $94,$0a,$f1,$ee,$00
 db $20,$00,$00
 db $9c,$06,$01,$00
 db $20,$00,$00
 db $94,$d1,$0a,$00,$a1,$49
 db $20,$00,$00
 db $98,$08,$00,$a1,$5d
 db $20,$00,$00
 db $98,$0c,$80,$60,$00
 db $20,$00,$00
 db $20,$80,$6c,$00
 db $20,$00,$00
 db $9c,$0e,$81,$5d,$01
 db $20,$00,$00
 db $20,$00,$a1,$72
 db $20,$00,$00
 db $94,$10,$00,$00
 db $20,$00,$00
 db $9c,$0e,$81,$72,$00
 db $20,$00,$00
 db $98,$0c,$80,$62,$81,$88
 db $20,$00,$82,$2a
 db $98,$0c,$80,$6e,$82,$4b
 db $20,$00,$82,$2a
 db $9c,$12,$81,$88,$01
 db $20,$82,$2a,$00
 db $98,$14,$82,$4b,$00
 db $20,$82,$2a,$00
 db $94,$16,$a1,$88,$00
 db $20,$a2,$2a,$00
 db $9c,$12,$a2,$4b,$00
 db $20,$a2,$2a,$00
 db $94,$43,$16,$f1,$88,$00
 db $20,$f2,$2a,$00
 db $9c,$12,$f2,$4b,$00
 db $20,$f2,$2a,$00
 db $94,$16,$01,$00
 db $20,$00,$00
 db $98,$14,$00,$00
 db $20,$00,$00
 db $98,$d1,$18,$80,$60,$80,$62
 db $20,$00,$00
 db $20,$80,$6c,$80,$6e
 db $20,$00,$00
 db $9c,$1a,$01,$a2,$4b
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $94,$1c,$82,$4b,$00
 db $20,$00,$00
 db $9c,$1a,$00,$00
 db $20,$00,$00
 db $98,$18,$80,$60,$a2,$2a
 db $20,$00,$a2,$0b
 db $98,$18,$80,$6c,$a1,$ee
 db $20,$00,$00
 db $9c,$1e,$82,$2a,$01
 db $20,$82,$0b,$00
 db $98,$20,$81,$ee,$00
 db $20,$00,$00
 db $81,$2e,$a2,$2a,$00
 db $17,$a2,$0b,$00
 db $9c,$1e,$a1,$ee,$00
 db $20,$00,$00
 db $94,$43,$22,$f2,$2a,$00
 db $20,$f2,$0b,$00
 db $81,$30,$f1,$ee,$00
 db $17,$00,$00
 db $94,$d1,$22,$01,$91,$ee
 db $20,$00,$92,$0b
 db $98,$20,$00,$91,$ee
 db $20,$00,$00
 db $98,$24,$80,$60,$00
 db $20,$00,$00
 db $20,$80,$6c,$00
 db $20,$00,$00
 db $9c,$26,$a1,$ee,$91,$88
 db $20,$00,$00
 db $20,$00,$01
 db $20,$00,$00
 db $94,$28,$a1,$88,$00
 db $20,$00,$00
 db $9c,$26,$01,$00
 db $20,$00,$00
 db $98,$24,$80,$60,$c2,$2a
 db $20,$00,$00
 db $98,$24,$80,$6c,$00
 db $20,$00,$00
 db $9c,$26,$82,$2a,$00
 db $20,$00,$00
 db $98,$2a,$00,$c1,$b8
 db $20,$00,$00
 db $81,$32,$00,$01
 db $17,$00,$00
 db $20,$81,$b8,$00
 db $20,$00,$00
 db $20,$81,$b4,$81,$b8
 db $20,$00,$00
 db $20,$81,$84,$81,$88
 db $20,$00,$00
 db $81,$36,$a1,$b8,$01
 db $17,$00,$00
 db $20,$a1,$88,$00
 db $20,$00,$00
 db $98,$00,$80,$60,$80,$62
 db $20,$00,$00
 db $20,$80,$6c,$80,$6e
 db $20,$00,$00
 db $9c,$02,$01,$a2,$4b
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $94,$04,$a2,$4b,$00
 db $20,$00,$00
 db $9c,$02,$00,$82,$2a
 db $20,$00,$00
 db $98,$00,$80,$60,$82,$0b
 db $20,$00,$00
 db $98,$00,$80,$6c,$81,$ee
 db $20,$00,$00
 db $9c,$06,$82,$0b,$01
 db $20,$00,$00
 db $98,$08,$81,$ee,$00
 db $20,$00,$00
 db $94,$0a,$a2,$0b,$00
 db $20,$00,$00
 db $9c,$06,$a1,$ee,$00
 db $20,$00,$00
 db $94,$43,$0a,$f2,$0b,$00
 db $20,$00,$00
 db $9c,$06,$f1,$ee,$a1,$88
 db $20,$00,$00
 db $94,$0a,$00,$01
 db $20,$00,$00
 db $98,$d1,$08,$f1,$88,$a1,$9f
 db $20,$00,$a1,$88
 db $98,$0c,$80,$60,$a1,$b8
 db $20,$00,$00
 db $20,$80,$6c,$80,$6e
 db $20,$00,$00
 db $9c,$0e,$01,$01
 db $20,$00,$00
 db $20,$00,$b1,$9f
 db $20,$00,$00
 db $94,$10,$00,$01
 db $20,$00,$00
 db $9c,$0e,$b1,$9f,$00
 db $20,$00,$00
 db $98,$0c,$80,$60,$c1,$88
 db $20,$00,$00
 db $98,$0c,$80,$6c,$80,$6e
 db $20,$00,$00
 db $9c,$12,$81,$88,$01
 db $20,$00,$00
 db $98,$14,$01,$00
 db $20,$00,$00
 db $94,$16,$a1,$88,$00
 db $20,$00,$00
 db $9c,$12,$01,$00
 db $20,$00,$00
 db $94,$43,$16,$f1,$88,$00
 db $20,$00,$00
 db $9c,$12,$01,$00
 db $20,$00,$00
 db $94,$16,$00,$00
 db $20,$00,$00
 db $98,$14,$00,$00
 db $20,$00,$00
 db $98,$d1,$18,$80,$60,$80,$62
 db $20,$00,$00
 db $20,$80,$6c,$80,$6e
 db $20,$00,$00
 db $9c,$43,$1a,$f3,$10,$00
 db $20,$f3,$70,$00
 db $20,$f4,$55,$00
 db $20,$f3,$10,$00
 db $94,$1c,$e3,$70,$f3,$0e
 db $20,$e4,$55,$f3,$6e
 db $9c,$1a,$e3,$10,$f4,$53
 db $20,$e3,$70,$e3,$0e
 db $98,$18,$b0,$62,$e3,$6e
 db $20,$00,$e4,$53
 db $98,$18,$b0,$6e,$d3,$0e
 db $20,$00,$d3,$6e
 db $9c,$1e,$d3,$10,$d4,$53
 db $20,$c3,$70,$c3,$0e
 db $98,$20,$c4,$55,$c3,$6e
 db $20,$c3,$10,$c4,$53
 db $81,$2e,$b3,$70,$b3,$0e
 db $17,$b4,$55,$b3,$6e
 db $9c,$1e,$b3,$10,$b4,$53
 db $20,$a3,$70,$a3,$0e
 db $94,$22,$a4,$55,$a3,$6e
 db $20,$f3,$10,$01
 db $81,$30,$f3,$70,$00
 db $17,$f4,$55,$00
 db $94,$22,$f3,$10,$00
 db $20,$f3,$70,$00
 db $98,$20,$f4,$55,$00
 db $20,$f3,$10,$00
 db $98,$c1,$24,$81,$86,$81,$88
 db $20,$00,$00
 db $20,$81,$b6,$81,$b8
 db $20,$00,$00
 db $9c,$d1,$26,$81,$88,$01
 db $20,$00,$00
 db $20,$81,$b8,$00
 db $20,$00,$00
 db $94,$28,$a1,$88,$00
 db $20,$00,$00
 db $9c,$26,$a1,$b8,$00
 db $20,$00,$00
 db $98,$24,$80,$c2,$91,$88
 db $20,$00,$00
 db $98,$24,$80,$da,$91,$b8
 db $20,$00,$00
 db $9c,$26,$81,$88,$01
 db $20,$00,$00
 db $98,$2a,$81,$b8,$00
 db $20,$00,$00
 db $94,$2c,$a1,$88,$00
 db $20,$00,$00
 db $20,$a1,$b8,$00
 db $20,$00,$00
 db $94,$43,$2c,$f1,$88,$00
 db $20,$00,$00
 db $20,$f1,$b8,$00
 db $20,$00,$00
 db $20,$01,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $98,$d1,$38,$80,$79,$80,$7b
 db $20,$00,$00
 db $20,$80,$80,$80,$82
 db $20,$00,$00
 db $9c,$3a,$a4,$17,$01
 db $20,$01,$00
 db $20,$00,$00
 db $20,$00,$00
 db $88,$3c,$a3,$dc,$00
 db $20,$01,$00
 db $9c,$3a,$00,$00
 db $20,$00,$00
 db $98,$38,$80,$79,$80,$7b
 db $20,$00,$00
 db $98,$38,$80,$80,$80,$82
 db $20,$00,$00
 db $9c,$3a,$a3,$dc,$a1,$ee
 db $20,$01,$a2,$0b
 db $98,$38,$00,$00
 db $20,$a1,$ee,$00
 db $94,$3e,$93,$dc,$00
 db $20,$92,$0b,$00
 db $9c,$3a,$a3,$dc,$00
 db $20,$a2,$0b,$00
 db $88,$3c,$b3,$dc,$00
 db $20,$b2,$0b,$00
 db $9c,$3a,$c3,$dc,$00
 db $20,$c2,$0b,$00
 db $94,$3e,$00,$a2,$4b
 db $20,$00,$00
 db $98,$38,$00,$a2,$93
 db $20,$00,$00
 db $98,$38,$80,$7b,$a2,$bb
 db $20,$00,$00
 db $20,$80,$82,$00
 db $20,$00,$00
 db $9c,$3a,$a4,$17,$01
 db $20,$01,$00
 db $20,$a2,$bb,$00
 db $20,$00,$00
 db $88,$3c,$a3,$dc,$a2,$93
 db $20,$01,$00
 db $9c,$3a,$a2,$bb,$00
 db $20,$00,$00
 db $98,$38,$80,$79,$80,$7b
 db $20,$00,$00
 db $98,$38,$80,$80,$80,$82
 db $20,$00,$00
 db $9c,$3a,$a3,$dc,$a2,$4b
 db $20,$01,$00
 db $98,$38,$a2,$bb,$00
 db $20,$00,$00
 db $94,$3e,$93,$dc,$01
 db $20,$92,$4b,$00
 db $9c,$3a,$a3,$dc,$00
 db $20,$a2,$4b,$00
 db $88,$3c,$b3,$dc,$a1,$ee
 db $20,$b2,$4b,$00
 db $9c,$3a,$c3,$dc,$01
 db $20,$01,$00
 db $94,$3e,$c1,$ec,$a1,$ee
 db $20,$00,$00
 db $98,$38,$01,$00
 db $20,$00,$00
 db $98,$38,$80,$57,$00
 db $20,$00,$00
 db $20,$80,$62,$00
 db $20,$00,$00
 db $9c,$3a,$a2,$bb,$a2,$0b
 db $20,$01,$00
 db $20,$00,$00
 db $20,$00,$00
 db $88,$3c,$a3,$10,$01
 db $20,$01,$00
 db $9c,$3a,$a2,$0b,$00
 db $20,$00,$00
 db $98,$38,$80,$55,$80,$57
 db $20,$00,$00
 db $98,$38,$80,$60,$80,$62
 db $20,$00,$00
 db $9c,$3a,$a3,$10,$01
 db $20,$01,$00
 db $98,$38,$00,$00
 db $20,$00,$00
 db $94,$3e,$92,$bb,$00
 db $20,$01,$00
 db $9c,$3a,$a2,$bb,$00
 db $20,$01,$00
 db $88,$3c,$b2,$bb,$00
 db $20,$01,$00
 db $9c,$3a,$c2,$bb,$00
 db $20,$01,$00
 db $94,$3e,$00,$a2,$4b
 db $20,$00,$00
 db $98,$38,$00,$a2,$93
 db $20,$00,$00
 db $98,$38,$80,$57,$a2,$bb
 db $20,$00,$00
 db $20,$80,$62,$00
 db $20,$00,$00
 db $9c,$3a,$a3,$10,$01
 db $20,$01,$00
 db $20,$a2,$bb,$00
 db $20,$00,$00
 db $88,$3c,$00,$a2,$93
 db $20,$01,$00
 db $9c,$3a,$00,$00
 db $20,$00,$00
 db $98,$38,$80,$55,$80,$57
 db $20,$00,$00
 db $98,$38,$80,$60,$80,$62
 db $20,$00,$00
 db $9c,$3a,$a2,$bb,$a2,$4b
 db $20,$01,$00
 db $98,$38,$00,$00
 db $20,$00,$00
 db $94,$3e,$92,$bb,$01
 db $20,$92,$4b,$00
 db $20,$a2,$bb,$00
 db $20,$a2,$4b,$00
 db $88,$3c,$b2,$bb,$a1,$ee
 db $20,$b2,$4b,$00
 db $20,$c2,$bb,$01
 db $20,$01,$00
 db $20,$c1,$ec,$a1,$ee
 db $20,$00,$00
 db $20,$01,$00
 db $20,$00,$00
 db $98,$38,$80,$7b,$00
 db $20,$00,$00
 db $20,$80,$82,$00
 db $20,$00,$00
 db $9c,$3a,$a4,$17,$a2,$0b
 db $20,$01,$00
 db $20,$00,$00
 db $20,$00,$00
 db $88,$3c,$a3,$dc,$01
 db $20,$01,$00
 db $9c,$3a,$a2,$0b,$00
 db $20,$00,$00
 db $98,$38,$80,$79,$80,$7b
 db $20,$00,$00
 db $98,$38,$80,$80,$80,$82
 db $20,$00,$00
 db $9c,$3a,$a3,$dc,$01
 db $20,$01,$00
 db $98,$38,$00,$00
 db $20,$00,$00
 db $94,$3e,$93,$dc,$00
 db $20,$01,$00
 db $9c,$3a,$a3,$dc,$00
 db $20,$01,$00
 db $88,$3c,$b3,$dc,$00
 db $20,$01,$00
 db $9c,$3a,$c3,$dc,$00
 db $20,$01,$00
 db $94,$3e,$00,$a2,$4b
 db $20,$00,$00
 db $98,$38,$00,$a2,$93
 db $20,$00,$00
 db $98,$38,$80,$7b,$a2,$bb
 db $20,$00,$00
 db $20,$80,$82,$00
 db $20,$00,$00
 db $9c,$3a,$a4,$17,$01
 db $20,$01,$00
 db $20,$a2,$bb,$00
 db $20,$00,$00
 db $88,$3c,$a3,$dc,$a2,$93
 db $20,$01,$00
 db $9c,$3a,$00,$00
 db $20,$00,$00
 db $98,$38,$80,$79,$80,$7b
 db $20,$00,$00
 db $98,$38,$80,$80,$80,$82
 db $20,$00,$00
 db $9c,$3a,$a3,$dc,$a2,$4b
 db $20,$01,$00
 db $98,$38,$00,$00
 db $20,$00,$00
 db $94,$3e,$93,$dc,$01
 db $20,$92,$4b,$00
 db $9c,$3a,$a3,$dc,$00
 db $20,$a2,$4b,$00
 db $88,$3c,$b3,$dc,$a1,$ee
 db $20,$b2,$4b,$00
 db $9c,$3a,$c3,$dc,$01
 db $20,$01,$00
 db $94,$3e,$c1,$ec,$a1,$ee
 db $20,$00,$00
 db $98,$38,$01,$00
 db $20,$00,$00
 db $98,$38,$80,$a4,$00
 db $20,$00,$00
 db $20,$80,$ae,$00
 db $20,$00,$00
 db $9c,$3a,$a2,$bb,$00
 db $20,$01,$00
 db $20,$a1,$ee,$00
 db $20,$00,$00
 db $88,$3c,$a2,$93,$a2,$0b
 db $20,$01,$00
 db $9c,$3a,$a1,$ee,$00
 db $20,$00,$00
 db $98,$38,$80,$a4,$a1,$b8
 db $20,$00,$00
 db $98,$38,$80,$ae,$00
 db $20,$00,$00
 db $9c,$3a,$a2,$93,$00
 db $20,$01,$00
 db $98,$38,$a1,$b8,$00
 db $20,$00,$00
 db $94,$3e,$92,$93,$00
 db $20,$91,$b8,$00
 db $9c,$3a,$a2,$93,$00
 db $20,$a1,$b8,$00
 db $88,$3c,$b2,$93,$00
 db $20,$b1,$b8,$00
 db $9c,$3a,$c2,$93,$00
 db $20,$01,$00
 db $94,$3e,$00,$a2,$4b
 db $20,$00,$00
 db $98,$38,$00,$a2,$93
 db $20,$00,$00
 db $98,$38,$80,$57,$a2,$bb
 db $20,$00,$00
 db $20,$80,$62,$00
 db $20,$00,$00
 db $9c,$3a,$a3,$10,$01
 db $20,$01,$00
 db $20,$a2,$bb,$00
 db $20,$00,$00
 db $88,$3c,$00,$a2,$93
 db $20,$01,$00
 db $9c,$3a,$00,$00
 db $20,$00,$00
 db $98,$38,$80,$55,$80,$57
 db $20,$00,$00
 db $98,$38,$80,$60,$80,$62
 db $20,$00,$00
 db $9c,$3a,$a2,$bb,$a2,$4b
 db $20,$01,$00
 db $98,$38,$00,$00
 db $20,$00,$00
 db $88,$3c,$92,$bb,$01
 db $20,$92,$4b,$00
 db $98,$38,$a2,$bb,$00
 db $20,$a2,$4b,$00
 db $88,$3c,$b2,$bb,$a1,$ee
 db $20,$b2,$4b,$00
 db $98,$38,$c2,$bb,$00
 db $20,$01,$00
 db $88,$3c,$00,$00
 db $20,$00,$00
 db $88,$3c,$00,$00
 db $20,$00,$00
 db $98,$38,$80,$79,$82,$bb
 db $20,$00,$82,$93
 db $20,$80,$80,$01
 db $20,$00,$00
 db $9c,$3a,$82,$b9,$00
 db $20,$82,$91,$00
 db $20,$01,$00
 db $20,$00,$00
 db $88,$3c,$a2,$bb,$00
 db $20,$a2,$93,$00
 db $9c,$3a,$01,$00
 db $20,$00,$00
 db $98,$38,$80,$79,$80,$7b
 db $20,$00,$00
 db $98,$38,$80,$80,$80,$82
 db $20,$00,$00
 db $9c,$3a,$a3,$dc,$a1,$ee
 db $20,$01,$a2,$0b
 db $98,$38,$00,$00
 db $20,$a1,$ee,$00
 db $94,$3e,$93,$dc,$00
 db $20,$92,$0b,$00
 db $9c,$3a,$a3,$dc,$00
 db $20,$a2,$0b,$00
 db $88,$3c,$b3,$dc,$00
 db $20,$b2,$0b,$00
 db $98,$38,$c3,$dc,$00
 db $20,$c2,$0b,$00
 db $88,$3c,$00,$a2,$4b
 db $20,$00,$00
 db $94,$40,$00,$a2,$93
 db $20,$00,$00
 db $98,$38,$80,$7b,$a2,$bb
 db $20,$00,$00
 db $20,$80,$82,$00
 db $20,$00,$00
 db $9c,$3a,$a4,$17,$01
 db $20,$01,$00
 db $20,$a2,$bb,$00
 db $20,$00,$00
 db $88,$3c,$a3,$dc,$a2,$93
 db $20,$01,$00
 db $9c,$3a,$a2,$bb,$00
 db $20,$00,$00
 db $98,$38,$80,$79,$80,$7b
 db $20,$00,$00
 db $98,$38,$80,$80,$80,$82
 db $20,$00,$00
 db $9c,$3a,$a3,$dc,$a2,$4b
 db $20,$01,$00
 db $98,$38,$a2,$bb,$00
 db $20,$00,$00
 db $94,$3e,$93,$dc,$01
 db $20,$92,$4b,$00
 db $9c,$3a,$a3,$dc,$00
 db $20,$a2,$4b,$00
 db $88,$3c,$b3,$dc,$a1,$ee
 db $20,$b2,$4b,$00
 db $9c,$3a,$c3,$dc,$01
 db $20,$01,$00
 db $94,$3e,$c1,$ec,$a1,$ee
 db $20,$00,$00
 db $98,$38,$01,$00
 db $20,$00,$00
 db $98,$38,$80,$57,$00
 db $20,$00,$00
 db $20,$80,$62,$00
 db $20,$00,$00
 db $9c,$3a,$a2,$bb,$a2,$0b
 db $20,$01,$00
 db $20,$00,$00
 db $20,$00,$00
 db $88,$3c,$a3,$10,$00
 db $20,$01,$00
 db $9c,$3a,$a2,$0b,$00
 db $20,$00,$00
 db $98,$38,$80,$55,$80,$57
 db $20,$00,$00
 db $98,$38,$80,$60,$80,$62
 db $20,$00,$00
 db $9c,$3a,$a3,$10,$82,$bb
 db $20,$01,$82,$93
 db $98,$38,$00,$01
 db $20,$00,$00
 db $94,$3e,$92,$bb,$a2,$bb
 db $20,$01,$a2,$93
 db $9c,$3a,$a2,$bb,$01
 db $20,$01,$00
 db $88,$3c,$b2,$bb,$00
 db $20,$b2,$93,$00
 db $98,$38,$01,$00
 db $20,$00,$00
 db $88,$3c,$00,$a2,$4b
 db $20,$00,$00
 db $94,$40,$00,$a2,$93
 db $20,$00,$00
 db $98,$38,$80,$57,$a2,$bb
 db $20,$00,$00
 db $20,$80,$62,$00
 db $20,$00,$00
 db $9c,$3a,$a3,$10,$01
 db $20,$01,$00
 db $20,$a2,$bb,$00
 db $20,$00,$00
 db $88,$3c,$00,$a2,$93
 db $20,$01,$00
 db $9c,$3a,$00,$00
 db $20,$00,$00
 db $98,$38,$80,$55,$80,$57
 db $20,$00,$00
 db $98,$38,$80,$60,$80,$62
 db $20,$00,$00
 db $9c,$3a,$a2,$bb,$a2,$4b
 db $20,$01,$00
 db $98,$38,$00,$00
 db $20,$00,$00
 db $94,$3e,$92,$bb,$01
 db $20,$92,$4b,$00
 db $20,$a2,$bb,$00
 db $20,$a2,$4b,$00
 db $88,$3c,$b2,$bb,$a1,$ee
 db $20,$b2,$4b,$00
 db $20,$c2,$bb,$01
 db $20,$01,$00
 db $20,$c1,$ec,$a1,$ee
 db $20,$00,$00
 db $20,$01,$00
 db $20,$00,$00
 db $98,$38,$80,$7b,$00
 db $20,$00,$00
 db $20,$80,$82,$00
 db $20,$00,$00
 db $9c,$3a,$a4,$17,$a2,$0b
 db $20,$01,$00
 db $20,$00,$00
 db $20,$00,$00
 db $88,$3c,$a3,$dc,$01
 db $20,$01,$00
 db $9c,$3a,$a2,$0b,$00
 db $20,$00,$00
 db $98,$38,$80,$79,$80,$7b
 db $20,$00,$00
 db $98,$38,$80,$80,$80,$82
 db $20,$00,$00
 db $9c,$3a,$a3,$dc,$82,$bb
 db $20,$01,$82,$93
 db $98,$38,$00,$01
 db $20,$00,$00
 db $94,$3e,$93,$dc,$a2,$bb
 db $20,$01,$a2,$93
 db $9c,$3a,$a3,$dc,$01
 db $20,$01,$00
 db $88,$3c,$b2,$bb,$00
 db $20,$b2,$93,$00
 db $98,$38,$01,$00
 db $20,$00,$00
 db $88,$3c,$00,$a2,$4b
 db $20,$00,$00
 db $94,$40,$00,$a2,$93
 db $20,$00,$00
 db $98,$38,$80,$7b,$a2,$bb
 db $20,$00,$00
 db $20,$80,$82,$00
 db $20,$00,$00
 db $9c,$3a,$a4,$17,$01
 db $20,$01,$00
 db $20,$a2,$bb,$00
 db $20,$00,$00
 db $88,$3c,$a3,$dc,$a2,$93
 db $20,$01,$00
 db $9c,$3a,$00,$00
 db $20,$00,$00
 db $98,$38,$80,$79,$80,$7b
 db $20,$00,$00
 db $98,$38,$80,$80,$80,$82
 db $20,$00,$00
 db $9c,$3a,$a3,$dc,$a2,$4b
 db $20,$01,$00
 db $98,$38,$00,$00
 db $20,$00,$00
 db $94,$3e,$93,$dc,$01
 db $20,$92,$4b,$00
 db $9c,$3a,$a3,$dc,$00
 db $20,$a2,$4b,$00
 db $88,$3c,$b3,$dc,$a1,$ee
 db $20,$b2,$4b,$00
 db $9c,$3a,$c3,$dc,$01
 db $20,$01,$00
 db $94,$3e,$c1,$ec,$a1,$ee
 db $20,$00,$00
 db $98,$38,$01,$00
 db $20,$00,$00
 db $98,$38,$80,$a4,$00
 db $20,$00,$00
 db $20,$80,$ae,$00
 db $20,$00,$00
 db $9c,$3a,$a2,$bb,$00
 db $20,$01,$00
 db $20,$a1,$ee,$00
 db $20,$00,$00
 db $88,$3c,$a2,$93,$a2,$0b
 db $20,$01,$00
 db $9c,$3a,$a1,$ee,$00
 db $20,$00,$00
 db $98,$38,$80,$a4,$a1,$b8
 db $20,$00,$00
 db $98,$38,$80,$ae,$00
 db $20,$00,$00
 db $9c,$3a,$a2,$93,$00
 db $20,$01,$00
 db $98,$38,$a1,$b8,$00
 db $20,$00,$00
 db $94,$3e,$92,$93,$00
 db $20,$91,$b8,$00
 db $9c,$3a,$a2,$93,$00
 db $20,$a1,$b8,$00
 db $88,$3c,$b2,$93,$00
 db $20,$b1,$b8,$00
 db $98,$38,$c2,$93,$00
 db $20,$01,$00
 db $88,$3c,$00,$a2,$4b
 db $20,$00,$00
 db $94,$40,$00,$a2,$93
 db $20,$00,$00
 db $98,$38,$80,$57,$a2,$bb
 db $20,$00,$00
 db $20,$80,$62,$00
 db $20,$00,$00
 db $20,$a3,$10,$01
 db $20,$01,$00
 db $20,$a2,$bb,$00
 db $20,$00,$00
 db $20,$00,$a2,$93
 db $20,$01,$00
 db $20,$00,$00
 db $20,$00,$00
 db $88,$3c,$80,$55,$80,$57
 db $20,$00,$00
 db $98,$38,$80,$60,$80,$62
 db $20,$00,$00
 db $20,$a2,$bb,$82,$93
 db $20,$01,$82,$4b
 db $88,$3c,$00,$00
 db $20,$00,$01
 db $98,$38,$00,$a2,$93
 db $20,$00,$a2,$4b
 db $20,$00,$00
 db $20,$00,$01
 db $81,$2e,$82,$93,$a0,$c4
 db $17,$82,$4b,$00
 db $20,$00,$00
 db $20,$01,$00
 db $20,$a2,$93,$00
 db $20,$a2,$4b,$00
 db $81,$30,$00,$00
 db $17,$00,$00
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$01
 db $20,$00,$00
 db $9c,$4d,$3a,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$01
 db $20,$00,$00
 db $a0,$91,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$c1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$d1,$b8
 db $20,$00,$00
 db $a0,$91,$e1,$88,$01
 db $20,$00,$00
 db $94,$3e,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$91,$03,$92,$0b
 db $20,$00,$00
 db $9c,$d1,$3a,$91,$b8,$01
 db $20,$00,$00
 db $88,$4d,$3c,$a0,$f5,$a1,$ee
 db $20,$00,$00
 db $a0,$91,$a2,$0b,$01
 db $20,$00,$00
 db $94,$4d,$3e,$b0,$da,$b1,$b8
 db $20,$00,$00
 db $a0,$91,$b1,$ee,$01
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$91,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$01
 db $20,$00,$00
 db $9c,$4d,$3a,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$01
 db $20,$00,$00
 db $a0,$91,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$b1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $a0,$91,$01,$a1,$88
 db $20,$00,$00
 db $94,$3e,$00,$a1,$b8
 db $20,$00,$00
 db $98,$38,$81,$88,$01
 db $20,$00,$00
 db $9c,$3a,$81,$b8,$00
 db $20,$00,$00
 db $88,$3c,$a1,$88,$00
 db $20,$00,$00
 db $20,$a1,$b8,$00
 db $20,$00,$00
 db $94,$3e,$c1,$88,$00
 db $20,$00,$00
 db $20,$c1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$01
 db $20,$00,$00
 db $9c,$4d,$3a,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$01
 db $20,$00,$00
 db $a0,$91,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$c1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$d1,$b8
 db $20,$00,$00
 db $a0,$91,$e1,$88,$01
 db $20,$00,$00
 db $94,$3e,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$92,$09,$92,$0b
 db $20,$00,$00
 db $9c,$3a,$a1,$ec,$81,$ee
 db $20,$00,$00
 db $88,$d1,$3c,$82,$0b,$01
 db $20,$00,$00
 db $20,$81,$ee,$00
 db $20,$00,$00
 db $94,$4d,$3e,$93,$6e,$b1,$b8
 db $20,$00,$00
 db $a0,$91,$91,$ee,$01
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$01
 db $20,$00,$00
 db $9c,$4d,$3a,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$01
 db $20,$00,$00
 db $a0,$91,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$c1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$d1,$b8
 db $20,$00,$00
 db $88,$d1,$3c,$01,$d1,$88
 db $20,$00,$00
 db $98,$38,$c1,$88,$a0,$62
 db $20,$c2,$2a,$01
 db $94,$3e,$c2,$4b,$a0,$62
 db $20,$b1,$88,$01
 db $88,$3c,$b2,$2a,$a0,$62
 db $20,$b2,$4b,$01
 db $98,$38,$a1,$88,$a0,$62
 db $20,$a2,$2a,$01
 db $94,$3e,$a2,$4b,$a0,$62
 db $20,$91,$88,$01
 db $88,$3c,$92,$2a,$a0,$62
 db $20,$92,$4b,$01
 db $94,$3e,$81,$88,$a0,$62
 db $20,$82,$2a,$01
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$01
 db $20,$00,$00
 db $9c,$4d,$3a,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$01
 db $20,$00,$00
 db $a0,$91,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$c1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$d1,$b8
 db $20,$00,$00
 db $a0,$91,$e1,$88,$01
 db $20,$00,$00
 db $94,$3e,$e1,$b8,$00
 db $20,$00,$00
 db $98,$c1,$38,$94,$13,$92,$0b
 db $20,$00,$00
 db $9c,$d1,$3a,$91,$b8,$01
 db $20,$00,$00
 db $88,$c1,$3c,$a3,$d8,$a1,$ee
 db $20,$00,$00
 db $a0,$91,$a4,$17,$01
 db $20,$00,$00
 db $94,$c1,$3e,$b3,$6c,$b1,$b8
 db $20,$00,$00
 db $a0,$91,$b3,$dc,$01
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$91,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$01
 db $20,$00,$00
 db $9c,$4d,$3a,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$01
 db $20,$00,$00
 db $a0,$91,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$b1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $a0,$91,$01,$a1,$88
 db $20,$00,$00
 db $94,$3e,$00,$a1,$b8
 db $20,$00,$00
 db $98,$38,$81,$88,$01
 db $20,$00,$00
 db $9c,$3a,$81,$b8,$00
 db $20,$00,$00
 db $88,$3c,$a1,$88,$00
 db $20,$00,$00
 db $20,$a1,$b8,$00
 db $20,$00,$00
 db $94,$3e,$c1,$88,$00
 db $20,$00,$00
 db $20,$c1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$01
 db $20,$00,$00
 db $9c,$4d,$3a,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$01
 db $20,$00,$00
 db $a0,$91,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$c1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$d1,$b8
 db $20,$00,$00
 db $a0,$91,$e1,$88,$01
 db $20,$00,$00
 db $94,$3e,$e1,$b8,$00
 db $20,$00,$00
 db $98,$cb,$38,$94,$13,$a4,$17
 db $20,$00,$00
 db $9c,$3a,$93,$d8,$93,$dc
 db $20,$00,$00
 db $88,$d1,$3c,$94,$15,$01
 db $20,$00,$00
 db $20,$93,$dc,$00
 db $20,$00,$00
 db $94,$cb,$3e,$b3,$6c,$b3,$70
 db $20,$00,$00
 db $a0,$91,$b3,$dc,$01
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$01
 db $20,$00,$00
 db $9c,$4d,$3a,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$01
 db $20,$00,$00
 db $a0,$91,$e1,$b8,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$c1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$d1,$b8
 db $20,$00,$00
 db $a0,$91,$01,$d1,$88
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $88,$cb,$3c,$f0,$62,$a0,$60
 db $88,$42,$00,$00
 db $88,$44,$00,$00
 db $20,$00,$00
 db $88,$3c,$00,$00
 db $20,$00,$00
 db $88,$44,$00,$00
 db $20,$00,$00
 db $88,$3c,$00,$00
 db $20,$00,$00
 db $88,$44,$00,$00
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$a0,$dc
 db $20,$00,$01
 db $9c,$4d,$3a,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$b0,$dc
 db $20,$00,$01
 db $94,$d1,$40,$e1,$b8,$b0,$dc
 db $20,$00,$01
 db $98,$4d,$38,$e0,$62,$c1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$d1,$b8
 db $20,$00,$00
 db $94,$d1,$40,$e1,$88,$d0,$dc
 db $20,$00,$01
 db $94,$3e,$e1,$b8,$d0,$dc
 db $20,$00,$01
 db $98,$cb,$38,$92,$09,$92,$0b
 db $20,$00,$00
 db $9c,$d1,$3a,$91,$b8,$91,$05
 db $20,$00,$01
 db $88,$cb,$3c,$a1,$ec,$a1,$ee
 db $20,$00,$00
 db $94,$d1,$40,$a2,$0b,$a0,$f7
 db $20,$00,$01
 db $94,$cb,$3e,$b1,$b6,$b1,$b8
 db $20,$00,$00
 db $94,$d1,$40,$b1,$ee,$b0,$dc
 db $20,$00,$01
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$91,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$90,$dc
 db $20,$00,$01
 db $9c,$4d,$3a,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$a0,$dc
 db $20,$00,$01
 db $94,$d1,$40,$e1,$b8,$a0,$dc
 db $20,$00,$01
 db $98,$4d,$38,$e0,$62,$b1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $94,$d1,$40,$01,$a1,$88
 db $20,$00,$00
 db $94,$3e,$00,$a1,$b8
 db $20,$00,$00
 db $98,$38,$81,$88,$a0,$dc
 db $20,$00,$01
 db $9c,$3a,$81,$b8,$a0,$dc
 db $20,$00,$01
 db $88,$3c,$a1,$88,$a0,$dc
 db $20,$00,$01
 db $94,$40,$a1,$b8,$a0,$dc
 db $20,$00,$01
 db $94,$3e,$c1,$88,$a0,$dc
 db $20,$00,$01
 db $94,$40,$c1,$b8,$a0,$dc
 db $20,$00,$01
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$a0,$dc
 db $20,$00,$01
 db $9c,$4d,$3a,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$b0,$dc
 db $20,$00,$01
 db $94,$d1,$40,$e1,$b8,$b0,$dc
 db $20,$00,$01
 db $98,$4d,$38,$e0,$62,$c1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$d1,$b8
 db $20,$00,$00
 db $94,$d1,$40,$e1,$88,$d0,$dc
 db $20,$00,$01
 db $94,$3e,$e1,$b8,$d0,$dc
 db $20,$00,$01
 db $98,$cb,$38,$b2,$09,$92,$0b
 db $20,$00,$00
 db $9c,$d1,$3a,$b1,$b8,$91,$05
 db $20,$00,$01
 db $88,$cb,$3c,$c1,$ec,$a1,$ee
 db $20,$00,$00
 db $94,$d1,$40,$c2,$0b,$a0,$f7
 db $20,$00,$01
 db $94,$cb,$3e,$d1,$b6,$b1,$b8
 db $20,$00,$00
 db $94,$d1,$40,$d1,$ee,$b0,$dc
 db $20,$00,$01
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$a0,$dc
 db $20,$00,$01
 db $9c,$4d,$3a,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$b0,$dc
 db $20,$00,$01
 db $94,$d1,$40,$e1,$b8,$b0,$dc
 db $20,$00,$01
 db $98,$4d,$38,$e0,$62,$c1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$d1,$b8
 db $20,$00,$00
 db $88,$d1,$3c,$01,$d1,$88
 db $20,$00,$00
 db $98,$38,$00,$c1,$88
 db $20,$00,$c2,$2a
 db $98,$38,$c1,$88,$c2,$4b
 db $20,$c2,$2a,$b1,$88
 db $88,$3c,$c2,$4b,$b2,$2a
 db $88,$3c,$b1,$88,$b2,$4b
 db $98,$38,$b2,$2a,$a1,$88
 db $20,$b2,$4b,$a2,$2a
 db $98,$38,$a1,$88,$a2,$4b
 db $20,$a2,$2a,$91,$88
 db $88,$3c,$a2,$4b,$92,$2a
 db $20,$91,$88,$92,$4b
 db $98,$38,$92,$2a,$81,$88
 db $20,$92,$4b,$82,$2a
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$a0,$dc
 db $20,$00,$01
 db $9c,$4d,$3a,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$b0,$dc
 db $20,$00,$01
 db $94,$d1,$40,$e1,$b8,$b0,$dc
 db $20,$00,$01
 db $98,$4d,$38,$e0,$62,$c1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$d1,$b8
 db $20,$00,$00
 db $94,$d1,$40,$e1,$88,$d0,$dc
 db $20,$00,$01
 db $94,$3e,$e1,$b8,$d0,$dc
 db $20,$00,$01
 db $98,$cb,$38,$d4,$13,$94,$17
 db $20,$00,$00
 db $9c,$d1,$3a,$d1,$b8,$91,$05
 db $20,$00,$01
 db $88,$cb,$3c,$e3,$d8,$a3,$dc
 db $20,$00,$00
 db $94,$d1,$40,$e4,$17,$a0,$f7
 db $20,$00,$01
 db $94,$cb,$3e,$f3,$6c,$b3,$70
 db $20,$00,$00
 db $94,$d1,$40,$f3,$dc,$b0,$dc
 db $20,$00,$01
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$91,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$90,$dc
 db $20,$00,$01
 db $9c,$4d,$3a,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$a0,$dc
 db $20,$00,$01
 db $94,$d1,$40,$e1,$b8,$a0,$dc
 db $20,$00,$01
 db $98,$4d,$38,$e0,$62,$b1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $94,$d1,$40,$01,$a1,$88
 db $20,$00,$00
 db $94,$3e,$00,$a1,$b8
 db $20,$00,$00
 db $98,$38,$00,$81,$88
 db $20,$00,$00
 db $9c,$3a,$81,$86,$81,$b8
 db $20,$00,$00
 db $88,$3c,$81,$b6,$a1,$88
 db $20,$00,$00
 db $94,$40,$81,$86,$a1,$b8
 db $20,$00,$00
 db $94,$3e,$81,$b6,$c1,$88
 db $20,$00,$00
 db $94,$40,$81,$86,$c1,$b8
 db $20,$00,$00
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$a0,$dc
 db $20,$00,$01
 db $9c,$4d,$3a,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$b0,$dc
 db $20,$00,$01
 db $94,$d1,$40,$e1,$b8,$b0,$dc
 db $20,$00,$01
 db $98,$4d,$38,$e0,$62,$c1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$d1,$b8
 db $20,$00,$00
 db $94,$d1,$40,$e1,$88,$d0,$dc
 db $20,$00,$01
 db $94,$3e,$e1,$b8,$d0,$dc
 db $20,$00,$01
 db $98,$cb,$38,$d4,$13,$b4,$17
 db $20,$00,$00
 db $9c,$d1,$3a,$d1,$b8,$b1,$05
 db $20,$00,$01
 db $88,$cb,$3c,$e3,$d8,$c3,$dc
 db $20,$00,$00
 db $94,$d1,$40,$e4,$17,$c0,$f7
 db $20,$00,$01
 db $94,$cb,$3e,$f3,$6c,$e3,$70
 db $20,$00,$00
 db $94,$d1,$40,$f3,$dc,$e0,$dc
 db $20,$00,$01
 db $98,$4d,$38,$e0,$62,$91,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$a1,$b8
 db $20,$00,$00
 db $9c,$d1,$3a,$e1,$88,$a0,$dc
 db $20,$00,$01
 db $9c,$4d,$3a,$e0,$6e,$b1,$b8
 db $20,$00,$00
 db $88,$3c,$01,$b0,$dc
 db $20,$00,$01
 db $94,$d1,$40,$e1,$b8,$b0,$dc
 db $20,$00,$01
 db $98,$4d,$38,$e0,$62,$c1,$88
 db $20,$00,$00
 db $98,$38,$e0,$6e,$d1,$b8
 db $20,$00,$00
 db $88,$d1,$3c,$01,$d1,$88
 db $20,$00,$00
 db $81,$2e,$f0,$c4,$a0,$60
 db $17,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $81,$30,$00,$00
 db $17,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $81,$46,$00,$00
 db $17,$00,$00
 db $20,$e0,$dc,$80,$6c
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$e0,$cf,$80,$67
 db $20,$e0,$c4,$80,$62
 db $20,$e0,$b9,$80,$5c
 db $20,$e0,$a4,$80,$52
 db $20,$e0,$7b,$80,$3d
 db $20,$e0,$6e,$80,$37
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
.loop
 db $20,$01,$01
 db $20,$00,$00
 db $20,$00,$00
 db $20,$00,$00
 db $00
 dw .loop
 align 2
.drumpar
.dp0
 dw .dsmp0+0
 db $02,$09,$00
.dp1
 dw .dsmp2+0
 db $01,$09,$00
.dp2
 dw .dsmp1+0
 db $03,$09,$00
.dp3
 dw .dsmp2+0
 db $01,$09,$08
.dp4
 dw .dsmp0+0
 db $02,$09,$08
.dp5
 dw .dsmp1+0
 db $03,$09,$08
.dp6
 dw .dsmp0+0
 db $02,$09,$10
.dp7
 dw .dsmp2+0
 db $01,$09,$10
.dp8
 dw .dsmp1+0
 db $03,$09,$10
.dp9
 dw .dsmp2+0
 db $01,$09,$18
.dp10
 dw .dsmp0+0
 db $02,$09,$18
.dp11
 dw .dsmp1+0
 db $03,$09,$18
.dp12
 dw .dsmp0+0
 db $02,$09,$20
.dp13
 dw .dsmp2+0
 db $01,$09,$20
.dp14
 dw .dsmp1+0
 db $03,$09,$20
.dp15
 dw .dsmp2+0
 db $01,$09,$28
.dp16
 dw .dsmp0+0
 db $02,$09,$28
.dp17
 dw .dsmp1+0
 db $03,$09,$28
.dp18
 dw .dsmp0+0
 db $02,$09,$30
.dp19
 dw .dsmp2+0
 db $01,$09,$30
.dp20
 dw .dsmp1+0
 db $03,$09,$30
.dp21
 dw .dsmp0+0
 db $02,$09,$38
.dp22
 dw .dsmp1+0
 db $03,$09,$38
.dp23
 dw .dsmp4+0
 db $0a,$09,$40
.dp24
 dw .dsmp4+0
 db $0a,$03,$40
.dp25
 dw .dsmp5+0
 db $0a,$09,$38
.dp26
 dw .dsmp5+0
 db $0a,$03,$40
.dp27
 dw .dsmp5+0
 db $0a,$00,$40
.dp28
 dw .dsmp0+0
 db $02,$09,$40
.dp29
 dw .dsmp2+0
 db $01,$09,$40
.dp30
 dw .dsmp3+0
 db $06,$09,$40
.dp31
 dw .dsmp1+0
 db $03,$09,$40
.dp32
 dw .dsmp6+0
 db $03,$09,$40
.dp33
 dw .dsmp3+0
 db $06,$09,$38
.dp34
 dw .dsmp3+0
 db $06,$09,$30
.dp35
 dw .dsmp4+0
 db $0a,$00,$40
.dsmp0
 db $00,$00,$00,$00,$00,$00,$00,$00,$01,$07,$f3,$fc,$ff,$ff,$ff,$ff
 db $ff,$e7,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$f3,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
 db $ff,$ff,$ff,$ff,$f8,$c0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp1
 db $00,$78,$00,$f0,$07,$00,$0e,$00,$0f,$80,$38,$00,$00,$70,$00,$7f
 db $00,$02,$1c,$00,$0e,$00,$00,$c0,$01,$80,$00,$00,$00,$00,$00,$00
 db $3e,$00,$00,$00,$00,$07,$00,$00,$30,$00,$60,$00,$00,$c0,$00,$00
 db $00,$1f,$00,$03,$e0,$00,$3c,$00,$00,$0e,$00,$1e,$00,$03,$c0,$00
 db $00,$00,$00,$00,$03,$80,$00,$07,$80,$03,$80,$00,$00,$00,$18,$00
 db $00,$3c,$00,$78,$00,$00,$00,$00,$40,$00,$00,$00,$00,$00,$00,$00
.dsmp2
 db $00,$01,$02,$02,$03,$02,$02,$01,$00,$01,$02,$02,$03,$02,$02,$01
 db $00,$01,$02,$02,$03,$02,$02,$01,$00,$01,$02,$02,$03,$02,$02,$01
.dsmp3
 db $3d,$ff,$0e,$38,$00,$00,$00,$00,$01,$01,$0f,$ff,$ef,$ff,$ff,$ff
 db $ff,$ff,$ff,$fe,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$ff,$ff
 db $ff,$ff,$ff,$ff,$ff,$ff,$00,$00,$00,$00,$00,$00,$00,$00,$00,$19
 db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$fe,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$0f,$7f,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$fb,$70,$80,$00
 db $00,$00,$00,$00,$00,$00,$00,$07,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
 db $df,$18,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$09,$ff,$ff,$ff
 db $ff,$ff,$ff,$ff,$ff,$cc,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $08,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$dc,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$09,$79,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$fe,$f0,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp4
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$ff,$00,$00,$21
 db $e0,$00,$00,$ff,$f0,$00,$00,$7e,$00,$00,$0f,$ff,$00,$00,$07,$e0
 db $00,$00,$ff,$f0,$00,$00,$3f,$00,$00,$07,$ff,$80,$00,$00,$fc,$00
 db $00,$3f,$fe,$00,$00,$83,$e0,$00,$00,$ff,$f0,$00,$03,$1f,$00,$00
 db $01,$ff,$e0,$00,$06,$1f,$00,$00,$07,$fb,$80,$00,$0c,$3c,$00,$00
 db $0f,$d7,$00,$00,$1c,$38,$00,$00,$0f,$d6,$00,$00,$18,$1c,$00,$00
 db $0f,$c4,$00,$00,$1c,$0c,$00,$00,$3f,$c0,$00,$00,$0e,$01,$00,$00
 db $13,$e0,$00,$00,$07,$01,$00,$00,$00,$f8,$00,$00,$01,$80,$00,$00
 db $07,$3f,$00,$00,$00,$70,$00,$00,$00,$87,$c0,$00,$00,$06,$00,$00
 db $00,$10,$fc,$34,$00,$00,$c0,$00,$00,$01,$07,$c0,$80,$00,$07,$00
 db $00,$00,$1c,$7f,$0d,$00,$00,$38,$00,$00,$00,$70,$f8,$78,$00,$00
 db $60,$00,$00,$00,$83,$e0,$f0,$00,$00,$c0,$00,$00,$00,$07,$e0,$e0
 db $00,$00,$c0,$00,$00,$00,$07,$e0,$e0,$00,$00,$60,$00,$00,$00,$0d
 db $f8,$78,$00,$00,$18,$00,$00,$00,$00,$ff,$0f,$00,$00,$01,$00,$00
 db $00,$00,$3f,$f0,$e0,$00,$00,$10,$00,$00,$00,$01,$ff,$0f,$00,$00
 db $00,$c0,$00,$00,$00,$07,$fe,$78,$80,$00,$00,$00,$00,$00,$04,$1f
 db $f8,$f3,$00,$00,$00,$00,$00,$00,$1c,$1f,$f8,$f1,$00,$00,$00,$00
 db $00,$00,$0e,$1f,$f8,$f0,$80,$00,$00,$00,$00,$00,$01,$06,$ff,$3c
 db $e0,$00,$00,$00,$00,$00,$00,$01,$9f,$c3,$98,$00,$00,$00,$00,$00
.dsmp5
 db $00,$00,$00,$00,$00,$00,$00,$01,$fe,$00,$00,$07,$ff,$e0,$00,$00
 db $07,$ff,$c0,$00,$0f,$ff,$c0,$00,$00,$ff,$f0,$00,$00,$01,$ff,$c0
 db $00,$03,$ff,$f8,$00,$00,$ff,$f8,$00,$00,$0f,$ff,$80,$00,$0f,$ff
 db $c0,$00,$01,$ff,$80,$00,$00,$07,$ff,$00,$00,$ff,$f8,$00,$00,$7f
 db $f8,$00,$00,$1f,$ff,$00,$00,$7f,$fe,$00,$00,$7f,$e0,$00,$00,$07
 db $ff,$00,$00,$7f,$00,$00,$00,$00,$00,$00,$00,$00,$03,$e0,$00,$01
 db $ff,$80,$00,$00,$ff,$80,$00,$03,$ff,$e0,$00,$00,$ff,$e0,$00,$00
 db $ff,$c0,$00,$00,$01,$fe,$00,$00,$0f,$f0,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$1f,$e0,$00,$00,$00,$7f,$e0,$00,$00
 db $07,$e0,$00,$00,$00,$00,$3f,$ff,$00,$00,$00,$00,$3f,$f8,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$7f,$fc,$00,$00,$00,$00,$00
 db $00,$7f,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$ff,$f0,$00,$00,$00,$00
 db $00,$00,$3f,$ff,$c0,$00,$00,$00,$03,$ff,$e0,$00,$00,$00,$00,$00
 db $00,$ff,$ff,$f0,$00,$00,$00,$07,$ff,$ff,$f8,$00,$00,$00,$7f,$ff
 db $c0,$00,$00,$00,$00,$3f,$ff,$f8,$00,$00,$0f,$ff,$ff,$f0,$00,$00
.dsmp6
 db $00,$00,$c0,$00,$01,$03,$0f,$1f,$3f,$7e,$7c,$00,$00,$00,$00,$00
 db $00,$00,$07,$9f,$ff,$ff,$fe,$00,$00,$00,$00,$00,$00,$00,$00,$0f
 db $ff,$ff,$ff,$00,$00,$00,$00,$00,$00,$00,$00,$01,$ff,$ff,$ff,$80
 db $00,$00,$00,$00,$00,$00,$00,$00,$0f,$ff,$ff,$c0,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$7f,$ff,$c0,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$7f,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3f,$ff,$ff





	output	"TRACK07.BIN"

	org $6000
begin:


; 	EARSHAVER
; 	TRACK 7
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




; TRACK07

;compiled music data

music_data
 dw .drumpar
.song
 db $a4,$c1,$00,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $28,$00,$b0,$f7
 db $28,$01,$01
 db $28,$f1,$ee,$b2,$4b
 db $28,$01,$01
 db $a4,$00,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $98,$02,$00,$b0,$f7
 db $a8,$03,$f2,$4b,$01
 db $a8,$81,$f1,$b8,$b2,$2a
 db $28,$01,$01
 db $28,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $28,$00,$b0,$f7
 db $a8,$03,$f2,$2a,$01
 db $a8,$81,$f1,$88,$b1,$ee
 db $28,$00,$00
 db $28,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$00,$00,$b0,$f7
 db $a8,$03,$f1,$ee,$01
 db $a4,$c1,$00,$f3,$10,$b3,$a5
 db $28,$00,$00
 db $98,$02,$f0,$a4,$b0,$f7
 db $28,$01,$01
 db $28,$f2,$e4,$b3,$70
 db $a8,$03,$f3,$a5,$01
 db $a8,$81,$f0,$a4,$b0,$f7
 db $28,$01,$01
 db $28,$f2,$93,$b3,$10
 db $28,$00,$00
 db $a4,$00,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $28,$00,$b0,$f7
 db $a8,$03,$f3,$10,$01
 db $a4,$c1,$00,$f1,$ee,$b2,$4b
 db $28,$01,$01
 db $28,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $98,$02,$00,$b0,$f7
 db $a8,$03,$f2,$4b,$01
 db $a8,$81,$f1,$b8,$b2,$2a
 db $28,$01,$01
 db $28,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $28,$00,$b0,$f7
 db $a8,$03,$f2,$2a,$01
 db $a4,$c1,$00,$f1,$88,$b1,$ee
 db $28,$00,$00
 db $a4,$00,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$00,$00,$b0,$f7
 db $a8,$03,$f1,$ee,$01
 db $a4,$c1,$00,$f1,$88,$b3,$0c
 db $28,$00,$b3,$3b
 db $98,$02,$00,$b3,$6c
 db $28,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $98,$02,$f2,$e4,$b1,$b4
 db $28,$00,$00
 db $98,$02,$f2,$93,$b1,$84
 db $28,$00,$00
 db $a4,$00,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $28,$00,$b0,$f7
 db $a8,$03,$f1,$b8,$01
 db $a8,$81,$f1,$ee,$b2,$4b
 db $a8,$03,$f1,$88,$01
 db $a4,$c1,$00,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $98,$02,$00,$b0,$f7
 db $a8,$03,$f2,$4b,$01
 db $a8,$81,$f1,$b8,$b2,$2a
 db $28,$01,$01
 db $28,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $28,$00,$b0,$f7
 db $a8,$03,$f2,$2a,$01
 db $a8,$81,$f1,$88,$b1,$ee
 db $28,$00,$00
 db $28,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$00,$00,$b0,$f7
 db $a8,$03,$f1,$ee,$01
 db $a4,$c1,$00,$f3,$10,$b3,$a5
 db $28,$00,$00
 db $98,$02,$f0,$a4,$b0,$f7
 db $28,$01,$01
 db $28,$f2,$e4,$b3,$70
 db $a8,$03,$f3,$a5,$01
 db $a8,$81,$f0,$a4,$b0,$f7
 db $a8,$03,$f3,$70,$01
 db $a8,$81,$f2,$93,$b3,$10
 db $28,$00,$00
 db $a4,$00,$f0,$92,$f0,$dc
 db $28,$00,$00
 db $28,$00,$b0,$dc
 db $a8,$03,$f3,$10,$01
 db $a4,$c1,$00,$f1,$ee,$b2,$4b
 db $28,$01,$01
 db $28,$f0,$92,$f0,$dc
 db $28,$00,$00
 db $98,$02,$00,$b0,$dc
 db $a8,$03,$f2,$4b,$01
 db $a8,$81,$f1,$b8,$b2,$2a
 db $28,$01,$01
 db $28,$f0,$92,$f0,$dc
 db $28,$00,$00
 db $28,$00,$00
 db $a8,$03,$f2,$2a,$01
 db $a4,$c1,$00,$f1,$88,$f1,$ee
 db $28,$00,$00
 db $a4,$00,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$00,$01,$01
 db $28,$00,$00
 db $a4,$00,$f3,$10,$b3,$0c
 db $28,$00,$b3,$3b
 db $98,$02,$00,$b3,$6c
 db $28,$00,$00
 db $28,$f2,$e4,$b3,$70
 db $28,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $90,$04,$f2,$93,$00
 db $28,$00,$00
 db $a0,$06,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$00,$00
 db $90,$04,$f1,$ee,$b2,$4b
 db $28,$00,$00
 db $a0,$0a,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$00,$00
 db $a0,$06,$f1,$b8,$b2,$2a
 db $28,$00,$00
 db $90,$04,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$00,$00
 db $a0,$06,$f1,$88,$b1,$ee
 db $28,$00,$00
 db $a0,$06,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $90,$04,$00,$b0,$f7
 db $28,$00,$00
 db $a0,$0a,$f3,$10,$b3,$a5
 db $28,$00,$00
 db $a4,$08,$f0,$a4,$b0,$f7
 db $28,$00,$00
 db $a0,$06,$f2,$e4,$b3,$70
 db $28,$00,$00
 db $90,$04,$f0,$a4,$b0,$f7
 db $28,$00,$00
 db $a4,$08,$f2,$93,$b3,$10
 db $28,$00,$00
 db $a0,$06,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$00,$00
 db $90,$04,$f1,$ee,$b2,$4b
 db $28,$00,$00
 db $a0,$0a,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$00,$00
 db $a0,$06,$f1,$b8,$b2,$2a
 db $28,$00,$00
 db $90,$04,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$00,$00
 db $a0,$06,$f1,$88,$b1,$ee
 db $28,$00,$00
 db $a0,$06,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $90,$04,$00,$b0,$f7
 db $28,$00,$00
 db $a0,$0a,$f1,$88,$b3,$0c
 db $28,$00,$b3,$3b
 db $a4,$08,$00,$b3,$6c
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$f2,$e4,$b1,$b4
 db $28,$00,$00
 db $a0,$0a,$f2,$93,$b1,$84
 db $28,$00,$00
 db $a0,$06,$f0,$a2,$f1,$ee
 db $28,$00,$00
 db $28,$f0,$a0,$b1,$ee
 db $28,$00,$00
 db $28,$f1,$ee,$b2,$4b
 db $28,$00,$00
 db $a0,$06,$f0,$a2,$f1,$ee
 db $28,$00,$00
 db $90,$04,$f0,$a0,$b1,$ee
 db $28,$00,$00
 db $a0,$06,$f1,$b8,$b2,$2a
 db $28,$00,$00
 db $a0,$0a,$f0,$a2,$f1,$ee
 db $28,$00,$00
 db $28,$f0,$a0,$b1,$ee
 db $28,$00,$00
 db $a0,$06,$f1,$88,$00
 db $28,$00,$00
 db $a0,$06,$f0,$a2,$f1,$ee
 db $28,$00,$00
 db $a0,$06,$f0,$a0,$b1,$ee
 db $28,$00,$00
 db $28,$f3,$10,$b3,$a5
 db $28,$00,$00
 db $90,$04,$f0,$a0,$b1,$ee
 db $28,$00,$00
 db $a4,$08,$f2,$e4,$b3,$70
 db $28,$00,$00
 db $a0,$06,$f0,$a0,$b1,$ee
 db $28,$00,$00
 db $a0,$0a,$f2,$93,$b3,$10
 db $28,$00,$00
 db $a0,$06,$f0,$92,$f0,$dc
 db $28,$00,$00
 db $a4,$08,$00,$b0,$dc
 db $28,$00,$00
 db $90,$04,$f1,$ee,$b2,$4b
 db $28,$00,$00
 db $a0,$0a,$f0,$92,$f0,$dc
 db $28,$00,$00
 db $a4,$08,$00,$b0,$dc
 db $28,$00,$00
 db $a0,$06,$f1,$b8,$b2,$2a
 db $28,$00,$00
 db $90,$04,$f0,$92,$f0,$dc
 db $28,$00,$00
 db $a4,$08,$00,$00
 db $28,$00,$00
 db $a0,$06,$f1,$88,$f1,$ee
 db $28,$00,$00
 db $a0,$06,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $90,$43,$04,$f1,$ee,$01
 db $28,$01,$00
 db $90,$c1,$04,$f3,$10,$f3,$0c
 db $28,$00,$f3,$3b
 db $28,$00,$f3,$6c
 db $28,$00,$00
 db $90,$04,$f2,$e4,$f3,$70
 db $28,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $90,$04,$f2,$93,$00
 db $28,$00,$00
 db $a0,$06,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $28,$00,$b0,$f7
 db $28,$01,$01
 db $98,$43,$02,$f0,$f7,$00
 db $28,$01,$00
 db $a8,$81,$f1,$49,$f0,$f7
 db $28,$00,$00
 db $28,$00,$b0,$f7
 db $28,$01,$01
 db $a4,$43,$00,$f1,$49,$00
 db $28,$01,$00
 db $98,$c1,$02,$f1,$49,$f1,$ee
 db $28,$00,$00
 db $28,$00,$b1,$ee
 db $28,$00,$00
 db $a0,$43,$06,$f1,$ee,$01
 db $28,$00,$00
 db $a4,$c1,$00,$f2,$4b,$f1,$ee
 db $28,$00,$00
 db $98,$02,$00,$b1,$ee
 db $28,$01,$01
 db $a8,$03,$f2,$4b,$00
 db $28,$01,$00
 db $a8,$81,$f1,$49,$f2,$4b
 db $28,$00,$00
 db $a0,$06,$00,$b2,$4b
 db $28,$00,$00
 db $98,$02,$00,$f2,$2a
 db $28,$00,$00
 db $28,$00,$b1,$ee
 db $28,$00,$00
 db $a0,$06,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a0,$06,$00,$b0,$f7
 db $28,$01,$01
 db $98,$43,$02,$f0,$f7,$00
 db $28,$01,$00
 db $a8,$81,$f1,$49,$f0,$f7
 db $28,$00,$00
 db $28,$00,$b0,$f7
 db $28,$01,$01
 db $a4,$43,$00,$f1,$49,$00
 db $28,$01,$00
 db $98,$c1,$02,$f1,$49,$f1,$ee
 db $28,$00,$00
 db $28,$00,$b1,$ee
 db $28,$00,$00
 db $a0,$43,$06,$f1,$ee,$01
 db $28,$00,$00
 db $a4,$4d,$00,$90,$a4,$91,$ee
 db $28,$00,$00
 db $98,$02,$91,$49,$00
 db $28,$00,$90,$a4
 db $a0,$06,$00,$91,$ee
 db $28,$00,$90,$a4
 db $a0,$06,$a2,$93,$a1,$ee
 db $28,$00,$90,$a4
 db $a0,$06,$00,$a1,$ee
 db $28,$00,$91,$49
 db $98,$02,$b5,$27,$b1,$ee
 db $28,$00,$a1,$49
 db $98,$02,$00,$b1,$ee
 db $28,$00,$a2,$93
 db $a0,$c1,$06,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$01,$01
 db $98,$43,$02,$f0,$f7,$00
 db $28,$01,$00
 db $a4,$c1,$08,$f1,$49,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$01,$01
 db $a4,$43,$00,$f1,$49,$00
 db $28,$01,$00
 db $98,$c1,$02,$f1,$49,$f1,$ee
 db $28,$00,$00
 db $a4,$08,$00,$b1,$ee
 db $28,$00,$00
 db $a0,$43,$06,$f1,$ee,$01
 db $28,$00,$00
 db $a4,$c1,$00,$f2,$93,$f1,$ee
 db $28,$00,$00
 db $98,$02,$00,$b1,$ee
 db $28,$01,$01
 db $a4,$43,$08,$f2,$93,$00
 db $28,$01,$00
 db $a4,$c1,$08,$f5,$27,$f2,$4b
 db $28,$00,$00
 db $a0,$06,$00,$b2,$4b
 db $28,$00,$00
 db $98,$02,$00,$f2,$2a
 db $28,$00,$00
 db $a4,$08,$00,$b1,$ee
 db $28,$00,$00
 db $a0,$06,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$01,$01
 db $98,$43,$02,$f0,$f7,$00
 db $28,$01,$00
 db $a4,$c1,$08,$f1,$49,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$01,$01
 db $a4,$43,$00,$f1,$49,$00
 db $28,$01,$00
 db $98,$c1,$02,$f1,$49,$f1,$ee
 db $28,$00,$00
 db $a4,$08,$00,$b1,$ee
 db $28,$00,$00
 db $a0,$43,$06,$f1,$ee,$01
 db $28,$00,$00
 db $a0,$c1,$06,$f1,$49,$f1,$ee
 db $28,$00,$00
 db $a0,$06,$00,$b1,$ee
 db $28,$00,$00
 db $a0,$06,$00,$f1,$ee
 db $28,$00,$00
 db $90,$04,$f2,$93,$b1,$ee
 db $28,$00,$00
 db $90,$04,$00,$f1,$ee
 db $28,$00,$00
 db $90,$cf,$04,$00,$b3,$dc
 db $28,$00,$00
 db $90,$04,$00,$f3,$dc
 db $28,$00,$00
 db $a0,$c1,$06,$f0,$f5,$f1,$47
 db $28,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $28,$00,$f1,$25
 db $28,$00,$00
 db $a0,$06,$00,$f1,$49
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$f1,$25
 db $28,$00,$00
 db $28,$00,$f1,$49
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $28,$00,$f1,$ee
 db $28,$00,$00
 db $a0,$06,$f1,$25,$f1,$b8
 db $28,$00,$00
 db $a0,$06,$00,$f2,$4b
 db $28,$00,$00
 db $90,$04,$f1,$05,$f1,$88
 db $28,$00,$00
 db $a0,$06,$00,$f2,$07
 db $28,$00,$00
 db $a0,$06,$00,$f1,$88
 db $28,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $28,$00,$f3,$10
 db $28,$00,$00
 db $a0,$06,$00,$f1,$72
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$f2,$e4
 db $28,$00,$00
 db $28,$f1,$25,$f1,$88
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $28,$00,$f3,$10
 db $28,$00,$00
 db $a0,$06,$00,$f1,$b8
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$f3,$70
 db $28,$00,$00
 db $a0,$06,$f0,$f7,$f1,$25
 db $28,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $28,$00,$f2,$2a
 db $28,$00,$00
 db $a0,$06,$00,$f1,$25
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$f2,$2a
 db $28,$00,$00
 db $28,$00,$f1,$25
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $28,$00,$f1,$ee
 db $28,$00,$00
 db $a0,$06,$f1,$25,$f1,$b8
 db $28,$00,$00
 db $a0,$06,$00,$f2,$4b
 db $28,$00,$00
 db $90,$04,$f1,$05,$f1,$88
 db $28,$00,$00
 db $a0,$06,$00,$f2,$07
 db $28,$00,$00
 db $a0,$06,$00,$f1,$88
 db $28,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $28,$00,$f3,$10
 db $28,$00,$00
 db $a0,$06,$00,$f1,$72
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$f2,$e4
 db $28,$00,$00
 db $28,$f1,$25,$f1,$88
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $28,$00,$f3,$10
 db $28,$00,$00
 db $a0,$06,$00,$f1,$b8
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$f3,$70
 db $28,$00,$00
 db $a0,$06,$f0,$f5,$f1,$47
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $a0,$06,$00,$00
 db $a4,$08,$00,$f1,$25
 db $a0,$06,$00,$00
 db $a0,$06,$00,$f1,$49
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$f1,$25
 db $28,$00,$00
 db $a0,$06,$00,$f1,$49
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $a0,$06,$00,$00
 db $a4,$08,$00,$f1,$ee
 db $a0,$06,$00,$00
 db $a0,$06,$f1,$25,$f1,$b8
 db $28,$00,$00
 db $a0,$06,$00,$f2,$4b
 db $a0,$06,$00,$00
 db $90,$04,$f1,$05,$f1,$88
 db $28,$00,$00
 db $a0,$06,$00,$f2,$07
 db $28,$00,$00
 db $a0,$06,$00,$f1,$88
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $a0,$06,$00,$00
 db $a4,$08,$00,$f3,$10
 db $a0,$06,$00,$00
 db $a0,$06,$00,$f1,$72
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$f2,$e4
 db $28,$00,$00
 db $a0,$06,$f1,$25,$f1,$88
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $a0,$06,$00,$00
 db $a4,$08,$00,$f3,$10
 db $a0,$06,$00,$00
 db $a0,$06,$00,$f1,$b8
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$f3,$6c
 db $28,$00,$00
 db $a0,$06,$f0,$f7,$f1,$25
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $a0,$06,$00,$00
 db $a4,$08,$00,$f2,$2a
 db $a0,$06,$00,$00
 db $a0,$06,$00,$f1,$25
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$f2,$2a
 db $28,$00,$00
 db $a0,$06,$00,$f1,$25
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $a0,$06,$00,$00
 db $a4,$08,$00,$f1,$ee
 db $a0,$06,$00,$00
 db $90,$04,$f1,$25,$f1,$b8
 db $28,$00,$00
 db $a0,$06,$00,$f2,$4b
 db $28,$00,$00
 db $90,$04,$f1,$05,$f1,$88
 db $28,$00,$00
 db $a0,$06,$00,$f2,$07
 db $28,$00,$00
 db $a0,$06,$00,$f1,$88
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$f3,$10
 db $28,$00,$00
 db $a0,$06,$00,$f1,$72
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$f2,$e4
 db $28,$00,$00
 db $a0,$06,$f1,$25,$f1,$88
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $a0,$06,$00,$00
 db $a4,$08,$00,$f3,$10
 db $a0,$06,$00,$00
 db $90,$04,$00,$f1,$b8
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$f3,$70
 db $28,$00,$00
 db $a0,$06,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $28,$00,$b0,$f7
 db $28,$01,$01
 db $98,$43,$02,$f0,$f7,$00
 db $28,$01,$00
 db $a8,$81,$f1,$49,$f0,$f7
 db $28,$00,$00
 db $28,$00,$b0,$f7
 db $28,$01,$01
 db $a4,$43,$00,$f1,$49,$00
 db $28,$01,$00
 db $98,$c1,$02,$f1,$49,$f1,$ee
 db $28,$00,$00
 db $28,$00,$b1,$ee
 db $28,$00,$00
 db $a0,$43,$06,$f1,$ee,$01
 db $28,$00,$00
 db $a4,$00,$01,$00
 db $28,$00,$00
 db $98,$02,$00,$f1,$49
 db $28,$00,$f1,$ee
 db $28,$00,$e2,$4b
 db $28,$00,$e1,$49
 db $28,$f1,$49,$d1,$ee
 db $28,$f1,$ee,$d2,$4b
 db $a0,$06,$e2,$4b,$c1,$49
 db $28,$e1,$49,$c1,$ee
 db $98,$02,$d1,$ee,$b2,$4b
 db $28,$d2,$4b,$b1,$49
 db $28,$c1,$49,$a1,$ee
 db $28,$c1,$ee,$a2,$4b
 db $a0,$c1,$06,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a0,$06,$00,$b0,$f7
 db $28,$01,$01
 db $98,$43,$02,$f0,$f7,$00
 db $28,$01,$00
 db $a8,$81,$f1,$49,$f0,$f7
 db $28,$00,$00
 db $28,$00,$b0,$f7
 db $28,$01,$01
 db $a4,$43,$00,$f1,$49,$00
 db $28,$01,$00
 db $98,$c1,$02,$f1,$49,$f1,$ee
 db $28,$00,$00
 db $28,$00,$b1,$ee
 db $28,$00,$00
 db $a0,$43,$06,$f1,$ee,$01
 db $28,$00,$00
 db $a4,$d1,$00,$80,$52,$d0,$52
 db $28,$00,$00
 db $98,$02,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $98,$02,$00,$00
 db $28,$00,$00
 db $98,$02,$00,$00
 db $28,$00,$00
 db $a0,$c1,$06,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$01,$01
 db $98,$43,$02,$f0,$f7,$00
 db $28,$01,$00
 db $a4,$c1,$08,$f1,$49,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$01,$01
 db $a4,$43,$00,$f1,$49,$00
 db $28,$01,$00
 db $98,$c1,$02,$f1,$49,$f1,$ee
 db $28,$00,$00
 db $a4,$08,$00,$b1,$ee
 db $28,$00,$00
 db $a0,$43,$06,$f1,$ee,$01
 db $28,$00,$00
 db $a4,$d1,$00,$f2,$93,$f1,$ee
 db $28,$00,$00
 db $98,$02,$00,$b1,$ee
 db $28,$01,$01
 db $a4,$08,$f2,$93,$b1,$ee
 db $28,$f1,$ee,$00
 db $a4,$08,$f2,$93,$f2,$4b
 db $28,$f1,$ee,$00
 db $a0,$06,$f2,$93,$b2,$4b
 db $28,$f2,$4b,$00
 db $98,$02,$f2,$93,$f2,$2a
 db $28,$f2,$4b,$00
 db $a4,$08,$f2,$93,$b1,$ee
 db $28,$f2,$2a,$00
 db $a0,$c1,$06,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$01,$01
 db $98,$43,$02,$f0,$f7,$00
 db $28,$01,$00
 db $a4,$c1,$08,$f1,$49,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$01,$01
 db $a4,$43,$00,$f1,$49,$00
 db $28,$01,$00
 db $98,$c1,$02,$f1,$49,$f1,$ee
 db $28,$00,$00
 db $a4,$08,$00,$b1,$ee
 db $28,$00,$00
 db $a0,$43,$06,$f1,$ee,$01
 db $28,$00,$00
 db $a0,$c1,$06,$f1,$49,$f1,$ee
 db $28,$00,$00
 db $a0,$06,$00,$b1,$ee
 db $28,$00,$00
 db $a0,$06,$00,$f1,$ee
 db $28,$00,$00
 db $90,$04,$f2,$93,$b1,$ee
 db $28,$00,$00
 db $90,$04,$00,$f1,$ee
 db $28,$00,$00
 db $90,$cf,$04,$00,$b3,$dc
 db $28,$00,$00
 db $90,$04,$00,$f3,$dc
 db $28,$00,$00
 db $a0,$06,$f0,$f5,$f1,$47
 db $28,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $28,$00,$f1,$25
 db $28,$00,$00
 db $a0,$06,$00,$f1,$49
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$f1,$25
 db $28,$00,$00
 db $28,$00,$f1,$49
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a8,$81,$00,$f1,$ee
 db $28,$00,$00
 db $a0,$cf,$06,$f1,$25,$f1,$b8
 db $28,$00,$00
 db $a0,$c1,$06,$00,$f2,$4b
 db $28,$00,$00
 db $90,$cf,$04,$f1,$05,$f1,$88
 db $28,$00,$00
 db $a0,$c1,$06,$00,$f2,$07
 db $28,$00,$00
 db $a0,$cf,$06,$00,$f1,$88
 db $28,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a8,$81,$00,$f3,$10
 db $28,$00,$00
 db $a0,$cf,$06,$00,$f1,$72
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$c1,$06,$00,$f2,$e4
 db $28,$00,$00
 db $a8,$8f,$f1,$25,$f1,$88
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a8,$81,$00,$f3,$10
 db $28,$00,$00
 db $a0,$cf,$06,$00,$f1,$b8
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$c1,$06,$00,$f3,$70
 db $28,$00,$00
 db $a0,$cf,$06,$f0,$f7,$f1,$25
 db $28,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a8,$81,$00,$f2,$2a
 db $28,$00,$00
 db $a0,$cf,$06,$00,$f1,$25
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$c1,$06,$00,$f2,$2a
 db $28,$00,$00
 db $a8,$8f,$00,$f1,$25
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a8,$81,$00,$f1,$ee
 db $28,$00,$00
 db $a0,$cf,$06,$f1,$25,$f1,$b8
 db $28,$00,$00
 db $a0,$c1,$06,$00,$f2,$4b
 db $28,$00,$00
 db $90,$cf,$04,$f1,$05,$f1,$88
 db $28,$00,$00
 db $a0,$c1,$06,$00,$f2,$07
 db $28,$00,$00
 db $a0,$cf,$06,$00,$f1,$88
 db $28,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a8,$81,$00,$f3,$10
 db $28,$00,$00
 db $a0,$cf,$06,$00,$f1,$72
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$c1,$06,$00,$f2,$e4
 db $28,$00,$00
 db $a8,$8f,$f1,$25,$f1,$88
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a8,$81,$00,$f3,$10
 db $28,$00,$00
 db $a0,$cf,$06,$00,$f1,$b8
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$c1,$06,$00,$f3,$70
 db $28,$00,$00
 db $a0,$cf,$06,$f0,$f5,$f1,$47
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $a0,$06,$00,$00
 db $a4,$08,$00,$f1,$25
 db $a0,$06,$00,$00
 db $a0,$06,$00,$f1,$49
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$f1,$25
 db $28,$00,$00
 db $a0,$06,$00,$f1,$49
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $a0,$06,$00,$00
 db $a4,$c1,$08,$00,$f1,$ee
 db $a0,$06,$00,$00
 db $a0,$cf,$06,$f1,$25,$f1,$b8
 db $28,$00,$00
 db $a0,$c1,$06,$00,$f2,$4b
 db $a0,$06,$00,$00
 db $90,$cf,$04,$f1,$05,$f1,$88
 db $28,$00,$00
 db $a0,$c1,$06,$00,$f2,$07
 db $28,$00,$00
 db $a0,$cf,$06,$00,$f1,$88
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $a0,$06,$00,$00
 db $a4,$c1,$08,$00,$f3,$10
 db $a0,$06,$00,$00
 db $a0,$cf,$06,$00,$f1,$72
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$c1,$06,$00,$f2,$e4
 db $28,$00,$00
 db $a0,$cf,$06,$f1,$25,$f1,$88
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $a0,$06,$00,$00
 db $a4,$c1,$08,$00,$f3,$10
 db $a0,$06,$00,$00
 db $a0,$cf,$06,$00,$f1,$b8
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$c1,$06,$00,$f3,$6c
 db $28,$00,$00
 db $a0,$cf,$06,$f0,$f7,$f1,$25
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $a0,$06,$00,$00
 db $a4,$c1,$08,$00,$f2,$2a
 db $a0,$06,$00,$00
 db $a0,$cf,$06,$00,$f1,$25
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a0,$c1,$06,$00,$f2,$2a
 db $28,$00,$00
 db $a0,$cf,$06,$00,$f1,$25
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $a0,$06,$00,$00
 db $90,$04,$00,$00
 db $a0,$06,$00,$00
 db $a4,$c1,$08,$00,$f1,$ee
 db $a0,$06,$00,$00
 db $90,$cf,$04,$f1,$25,$f1,$b8
 db $28,$00,$00
 db $a8,$81,$00,$f2,$4b
 db $28,$00,$00
 db $90,$cf,$04,$f0,$e9,$f1,$88
 db $28,$f0,$f7,$00
 db $a8,$81,$f1,$05,$f2,$07
 db $28,$00,$00
 db $90,$cf,$04,$00,$f1,$88
 db $28,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $90,$c1,$04,$00,$f3,$10
 db $28,$00,$00
 db $a8,$8f,$00,$f1,$72
 db $90,$04,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $a8,$81,$00,$f2,$e4
 db $90,$04,$00,$00
 db $a8,$8f,$f1,$25,$f1,$88
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $90,$04,$00,$00
 db $a8,$81,$00,$f3,$10
 db $28,$00,$00
 db $90,$cf,$04,$00,$f1,$b8
 db $28,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $28,$f1,$05,$f0,$c4
 db $28,$f0,$f7,$f0,$b9
 db $a8,$81,$f0,$dc,$f0,$a4
 db $28,$f0,$b9,$f0,$92
 db $a0,$06,$f0,$a4,$e0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $a4,$08,$00,$00
 db $90,$04,$f2,$47,$b4,$97
 db $28,$00,$00
 db $a4,$08,$f0,$a4,$e0,$f7
 db $a4,$08,$00,$00
 db $28,$00,$b0,$f7
 db $a8,$03,$f2,$4b,$b0,$a4
 db $a0,$c1,$06,$f2,$07,$b4,$17
 db $28,$00,$00
 db $90,$04,$f0,$a4,$e0,$f7
 db $28,$00,$00
 db $a0,$0a,$00,$b0,$f7
 db $a0,$43,$0a,$f2,$0b,$b0,$a4
 db $a0,$c1,$06,$f1,$ea,$b3,$dc
 db $28,$00,$00
 db $a0,$06,$f0,$a4,$e0,$f7
 db $28,$00,$00
 db $90,$04,$f1,$b4,$e3,$70
 db $28,$00,$00
 db $a4,$08,$f0,$a4,$b0,$f7
 db $a4,$43,$08,$f1,$ee,$b0,$a4
 db $a8,$81,$f1,$84,$b3,$10
 db $28,$00,$00
 db $a0,$06,$f1,$b4,$b3,$70
 db $28,$00,$00
 db $90,$04,$f0,$a4,$e0,$f7
 db $a8,$03,$f1,$88,$e0,$a4
 db $a4,$c1,$08,$f1,$45,$e2,$93
 db $28,$00,$00
 db $a0,$06,$f0,$a4,$e0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $a4,$43,$08,$f1,$49,$b0,$a4
 db $90,$c1,$04,$f2,$47,$b4,$97
 db $28,$00,$00
 db $a4,$08,$f0,$a4,$e0,$f7
 db $a4,$08,$00,$00
 db $28,$00,$b0,$f7
 db $a8,$03,$f2,$4b,$b0,$a4
 db $a0,$c1,$06,$f2,$07,$b4,$17
 db $28,$00,$00
 db $90,$04,$f0,$a4,$e0,$f7
 db $28,$00,$00
 db $a0,$0a,$00,$b0,$f7
 db $a0,$43,$0a,$f2,$0b,$b0,$a4
 db $a0,$c1,$06,$f1,$ea,$b3,$dc
 db $28,$00,$00
 db $a0,$06,$f0,$a4,$e0,$f7
 db $28,$00,$00
 db $90,$04,$f1,$b4,$e3,$70
 db $28,$00,$00
 db $a4,$08,$f0,$a4,$b0,$f7
 db $a4,$43,$08,$f1,$ee,$b0,$a4
 db $a8,$81,$f1,$84,$b3,$10
 db $28,$00,$00
 db $a0,$06,$f1,$b4,$b3,$70
 db $28,$00,$00
 db $90,$04,$f0,$a4,$e0,$f7
 db $a8,$03,$f1,$88,$e0,$a4
 db $a4,$c1,$08,$f2,$8f,$e5,$27
 db $28,$00,$00
 db $a0,$06,$f0,$82,$e0,$c4
 db $28,$00,$00
 db $a4,$08,$00,$b0,$c4
 db $a4,$43,$08,$f2,$93,$b0,$82
 db $90,$c1,$04,$f2,$e0,$b5,$c9
 db $28,$00,$00
 db $a4,$08,$f0,$82,$e0,$c4
 db $a4,$08,$00,$00
 db $28,$00,$b0,$c4
 db $28,$00,$00
 db $a0,$43,$06,$f2,$e4,$b0,$82
 db $28,$00,$00
 db $90,$c1,$04,$f0,$82,$e0,$c4
 db $28,$00,$00
 db $a0,$0a,$00,$b0,$c4
 db $a0,$0a,$00,$00
 db $a0,$06,$f2,$e0,$e5,$c9
 db $28,$00,$00
 db $a0,$06,$f2,$8f,$d5,$27
 db $28,$00,$00
 db $90,$04,$f0,$82,$e0,$c4
 db $a8,$03,$f2,$e4,$e0,$82
 db $a4,$c1,$08,$f2,$47,$c4,$97
 db $28,$00,$00
 db $28,$f0,$82,$b0,$c4
 db $a8,$03,$f2,$93,$b0,$82
 db $a0,$c1,$06,$f2,$8f,$b5,$27
 db $28,$00,$00
 db $90,$04,$f0,$82,$e0,$c4
 db $a8,$03,$f2,$4b,$e0,$82
 db $a4,$c1,$08,$f2,$e0,$a5,$c9
 db $28,$00,$00
 db $a0,$06,$f0,$92,$e0,$dc
 db $28,$00,$00
 db $a4,$08,$00,$b0,$dc
 db $a4,$43,$08,$f2,$e4,$b0,$92
 db $90,$c1,$04,$f3,$0c,$b6,$21
 db $28,$00,$00
 db $a4,$08,$f0,$92,$e0,$dc
 db $a4,$08,$00,$00
 db $28,$00,$b0,$dc
 db $28,$00,$00
 db $a0,$43,$06,$f3,$10,$b0,$92
 db $28,$00,$00
 db $90,$c1,$04,$f0,$92,$e0,$dc
 db $28,$00,$00
 db $a0,$0a,$00,$b0,$dc
 db $a0,$0a,$00,$00
 db $a0,$06,$83,$0e,$f6,$21
 db $28,$00,$00
 db $a0,$06,$82,$e2,$f5,$c9
 db $28,$00,$00
 db $90,$04,$80,$92,$e0,$dc
 db $90,$43,$04,$83,$10,$e0,$92
 db $a4,$c1,$08,$82,$91,$f5,$27
 db $28,$00,$00
 db $90,$04,$80,$92,$b0,$dc
 db $90,$43,$04,$82,$e4,$b0,$92
 db $a0,$c1,$06,$82,$e2,$f5,$c9
 db $28,$00,$00
 db $90,$04,$80,$92,$e0,$dc
 db $90,$43,$04,$82,$93,$e0,$92
 db $90,$c1,$04,$83,$0e,$f6,$21
 db $28,$00,$00
 db $a0,$06,$80,$a4,$e0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $a4,$43,$08,$83,$10,$b0,$a4
 db $90,$cf,$04,$82,$47,$b4,$97
 db $28,$00,$00
 db $a4,$c1,$08,$80,$a4,$e0,$f7
 db $a4,$08,$00,$00
 db $28,$00,$b0,$f7
 db $a8,$03,$82,$4b,$b0,$a4
 db $a0,$cf,$06,$82,$07,$b4,$17
 db $28,$00,$00
 db $90,$c1,$04,$80,$a4,$e0,$f7
 db $28,$00,$00
 db $a0,$0a,$00,$b0,$f7
 db $a0,$43,$0a,$82,$0b,$b0,$a4
 db $a0,$cf,$06,$81,$ea,$b3,$dc
 db $28,$00,$00
 db $a0,$c1,$06,$80,$a4,$e0,$f7
 db $28,$00,$00
 db $90,$cf,$04,$81,$b4,$e3,$70
 db $28,$00,$00
 db $a4,$c1,$08,$80,$a4,$b0,$f7
 db $a4,$43,$08,$81,$ee,$b0,$a4
 db $a8,$8f,$81,$84,$b3,$10
 db $28,$00,$00
 db $a0,$06,$81,$b4,$b3,$70
 db $28,$00,$00
 db $90,$c1,$04,$80,$a4,$e0,$f7
 db $a8,$03,$81,$88,$e0,$a4
 db $a4,$cf,$08,$81,$ea,$e3,$dc
 db $28,$00,$00
 db $a0,$43,$06,$83,$dc,$e0,$a4
 db $28,$93,$70,$00
 db $a4,$08,$a2,$93,$b0,$a4
 db $a4,$08,$b3,$dc,$00
 db $90,$04,$c3,$70,$01
 db $28,$d2,$93,$00
 db $a4,$08,$e3,$dc,$e0,$a4
 db $a4,$08,$f3,$70,$00
 db $28,$f3,$dc,$b0,$a4
 db $28,$f3,$70,$00
 db $a0,$cf,$06,$f2,$47,$b4,$97
 db $28,$00,$00
 db $90,$04,$f2,$07,$b4,$17
 db $28,$00,$00
 db $a0,$c1,$0a,$f0,$a4,$e0,$f7
 db $a0,$43,$0a,$f2,$4b,$e0,$a4
 db $a0,$cf,$06,$f1,$ea,$e3,$dc
 db $28,$00,$00
 db $a0,$c1,$06,$f0,$a4,$b0,$f7
 db $a8,$03,$f2,$0b,$b0,$a4
 db $90,$cf,$04,$f1,$b4,$b3,$70
 db $28,$00,$00
 db $a4,$c1,$08,$f0,$a4,$e0,$f7
 db $a4,$43,$08,$f1,$ee,$e0,$a4
 db $a8,$8f,$f1,$84,$e3,$10
 db $28,$00,$00
 db $a0,$06,$f1,$b4,$e3,$70
 db $28,$00,$00
 db $90,$c1,$04,$f0,$a4,$b0,$f7
 db $a8,$03,$f1,$88,$b0,$a4
 db $a4,$cf,$08,$f1,$ea,$b3,$dc
 db $28,$00,$00
 db $a0,$c1,$06,$f0,$82,$e0,$c4
 db $28,$00,$00
 db $a4,$08,$00,$b0,$c4
 db $a4,$43,$08,$f1,$ee,$b0,$82
 db $90,$cf,$04,$f2,$07,$b4,$17
 db $28,$00,$00
 db $a4,$c1,$08,$f0,$82,$e0,$c4
 db $a4,$08,$00,$00
 db $28,$00,$b0,$c4
 db $28,$00,$00
 db $a0,$43,$06,$f2,$0b,$b0,$82
 db $28,$00,$00
 db $90,$c1,$04,$f0,$82,$e0,$c4
 db $28,$00,$00
 db $a0,$0a,$00,$b0,$c4
 db $a0,$0a,$00,$00
 db $a0,$cf,$06,$f2,$47,$b4,$97
 db $28,$00,$00
 db $a0,$06,$f2,$07,$b4,$17
 db $28,$00,$00
 db $90,$c1,$04,$f0,$82,$e0,$c4
 db $a8,$03,$f2,$4b,$e0,$82
 db $a4,$cf,$08,$f1,$ea,$e3,$dc
 db $28,$00,$00
 db $a8,$81,$f0,$82,$b0,$c4
 db $a8,$03,$f2,$0b,$b0,$82
 db $a0,$cf,$06,$f1,$b4,$b3,$70
 db $28,$00,$00
 db $90,$c1,$04,$f0,$82,$e0,$c4
 db $a8,$03,$f1,$ee,$e0,$82
 db $a4,$cf,$08,$f1,$ea,$e3,$dc
 db $28,$00,$00
 db $a0,$c1,$06,$f0,$92,$e0,$dc
 db $28,$00,$00
 db $a4,$08,$00,$b0,$dc
 db $a4,$43,$08,$f1,$ee,$b0,$92
 db $90,$cf,$04,$f2,$07,$b4,$17
 db $28,$00,$00
 db $a4,$c1,$08,$f0,$92,$e0,$dc
 db $a4,$08,$00,$00
 db $28,$00,$b0,$dc
 db $28,$00,$00
 db $a0,$43,$06,$f2,$0b,$b0,$92
 db $28,$00,$00
 db $90,$c1,$04,$f0,$92,$e0,$dc
 db $28,$00,$00
 db $a0,$0a,$00,$b0,$dc
 db $a0,$0a,$00,$00
 db $90,$cf,$04,$93,$0e,$86,$21
 db $28,$00,$00
 db $a8,$03,$93,$10,$01
 db $28,$00,$00
 db $a8,$8f,$a2,$e2,$85,$c9
 db $28,$00,$00
 db $28,$b2,$91,$85,$27
 db $28,$00,$00
 db $a8,$03,$b2,$e4,$01
 db $28,$00,$00
 db $a8,$8f,$c2,$e2,$85,$c9
 db $28,$00,$00
 db $a8,$03,$c2,$93,$01
 db $28,$00,$00
 db $a8,$8f,$d3,$0e,$86,$21
 db $28,$00,$00
 db $a0,$c1,$06,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$00,$00
 db $90,$04,$f1,$ee,$b2,$4b
 db $28,$00,$00
 db $a0,$0a,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$00,$00
 db $a0,$06,$f1,$b8,$b2,$2a
 db $28,$00,$00
 db $90,$04,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$00,$00
 db $a0,$06,$f1,$88,$b1,$ee
 db $28,$00,$00
 db $a0,$06,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $90,$04,$00,$b0,$f7
 db $28,$00,$00
 db $a0,$0a,$f3,$10,$b3,$a5
 db $28,$00,$00
 db $a4,$08,$f0,$a4,$b0,$f7
 db $28,$00,$00
 db $a0,$06,$f2,$e4,$b3,$70
 db $28,$00,$00
 db $90,$04,$f0,$a4,$b0,$f7
 db $28,$00,$00
 db $a4,$08,$f2,$93,$b3,$10
 db $28,$00,$00
 db $a0,$06,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$00,$00
 db $90,$04,$f1,$ee,$b2,$4b
 db $28,$00,$00
 db $a0,$0a,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$00,$00
 db $a0,$06,$f1,$b8,$b2,$2a
 db $28,$00,$00
 db $90,$04,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $a4,$08,$00,$b0,$f7
 db $28,$00,$00
 db $a0,$06,$f1,$88,$b1,$ee
 db $28,$00,$00
 db $a0,$06,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $90,$04,$00,$b0,$f7
 db $28,$00,$00
 db $90,$04,$f1,$88,$b3,$0c
 db $28,$00,$b3,$3b
 db $28,$00,$b3,$6c
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$f2,$e4,$b1,$b4
 db $28,$00,$00
 db $90,$04,$f2,$93,$b1,$84
 db $28,$00,$00
 db $a0,$06,$f0,$a2,$f1,$ee
 db $28,$00,$00
 db $28,$f0,$a0,$b1,$ee
 db $28,$00,$00
 db $28,$f1,$ee,$b2,$4b
 db $28,$00,$00
 db $a0,$06,$f0,$a2,$f1,$ee
 db $28,$00,$00
 db $90,$04,$f0,$a0,$b1,$ee
 db $28,$00,$00
 db $a0,$06,$f1,$b8,$b2,$2a
 db $28,$00,$00
 db $a0,$0a,$f0,$a2,$f1,$ee
 db $28,$00,$00
 db $28,$f0,$a0,$b1,$ee
 db $28,$00,$00
 db $a0,$06,$f1,$88,$00
 db $28,$00,$00
 db $a0,$06,$f0,$a2,$f1,$ee
 db $28,$00,$00
 db $a0,$06,$f0,$a0,$b1,$ee
 db $28,$00,$00
 db $28,$f3,$10,$b3,$a5
 db $28,$00,$00
 db $90,$04,$f0,$a0,$b1,$ee
 db $28,$00,$00
 db $a4,$08,$f2,$e4,$b3,$70
 db $28,$00,$00
 db $a0,$06,$f0,$a0,$b1,$ee
 db $28,$00,$00
 db $a0,$0a,$f2,$93,$b3,$10
 db $28,$00,$00
 db $a0,$06,$f0,$90,$f1,$b8
 db $28,$00,$00
 db $a4,$08,$f0,$8e,$b1,$b8
 db $28,$00,$00
 db $28,$f1,$ee,$b2,$4b
 db $28,$00,$00
 db $a0,$0a,$f0,$90,$f1,$b8
 db $28,$00,$00
 db $90,$04,$f0,$8e,$b1,$b8
 db $28,$00,$00
 db $a0,$06,$f1,$b8,$b2,$2a
 db $28,$00,$00
 db $a0,$06,$f0,$90,$f1,$b8
 db $28,$00,$00
 db $a4,$08,$f0,$8e,$00
 db $28,$00,$00
 db $a0,$06,$f1,$88,$f1,$ee
 db $28,$00,$00
 db $a0,$06,$f0,$a4,$f0,$f7
 db $28,$00,$00
 db $90,$43,$04,$f1,$ee,$01
 db $28,$01,$00
 db $a0,$c1,$06,$f3,$10,$f3,$0c
 db $28,$00,$f3,$3b
 db $28,$00,$f3,$6c
 db $28,$00,$00
 db $90,$04,$f2,$e4,$f3,$70
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$f2,$93,$00
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$f1,$9f
 db $28,$00,$f1,$88
 db $a0,$06,$00,$f1,$72
 db $28,$00,$f1,$5d
 db $a0,$06,$00,$f1,$47
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $a0,$06,$00,$00
 db $28,$00,$00
 db $90,$04,$f1,$49,$f0,$a2
 db $28,$00,$00
 db $90,$04,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $90,$0c,$00,$00
 db $28,$00,$00
 db $90,$0c,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $90,$0e,$00,$00
 db $28,$00,$00
 db $90,$0e,$00,$00
 db $28,$00,$00
.loop
 db $28,$01,$01
 db $28,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $28,$00,$00
 db $00
 dw .loop
 align 2
.drumpar
.dp0
 dw .dsmp3+0
 db $01,$09,$40
.dp1
 dw .dsmp2+0
 db $04,$09,$40
.dp2
 dw .dsmp1+0
 db $06,$09,$40
.dp3
 dw .dsmp0+0
 db $02,$09,$40
.dp4
 dw .dsmp5+0
 db $01,$09,$40
.dp5
 dw .dsmp4+0
 db $02,$09,$40
.dp6
 dw .dsmp1+0
 db $06,$06,$40
.dp7
 dw .dsmp1+0
 db $06,$03,$40
.dsmp0
 db $00,$00,$00,$00,$00,$00,$00,$00,$83,$07,$ff,$fd,$ff,$ff,$ff,$ff
 db $ff,$ff,$a2,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$1f,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
 db $ff,$ff,$ff,$ff,$ff,$ff,$1e,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp1
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
.dsmp2
 db $18,$e2,$1c,$70,$c2,$78,$61,$e7,$00,$f9,$00,$39,$c0,$00,$fc,$e0
 db $06,$78,$00,$00,$00,$00,$00,$00,$80,$1f,$e1,$03,$ff,$87,$03,$00
 db $60,$01,$c0,$f0,$07,$00,$03,$00,$00,$70,$00,$08,$c0,$e0,$80,$06
 db $02,$00,$00,$00,$00,$30,$e0,$03,$e1,$18,$18,$07,$00,$00,$00,$00
 db $03,$01,$30,$00,$e0,$04,$04,$00,$00,$00,$00,$00,$00,$e2,$00,$00
 db $00,$00,$00,$00,$04,$18,$00,$00,$c0,$04,$18,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$80,$00,$00,$00,$00,$00,$04,$00,$01,$0c,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp3
 db $00,$08,$00,$00,$00,$00,$45,$80,$83,$07,$ff,$ff,$ff,$ff,$ff,$ff
 db $ff,$ff,$f7,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp4
 db $00,$00,$00,$00,$01,$ff,$ff,$ff,$ff,$00,$c0,$00,$00,$00,$00,$00
 db $00,$fc,$7f,$ff,$ff,$c0,$00,$00,$00,$00,$00,$00,$00,$3e,$03,$ff
 db $ff,$f8,$00,$00,$00,$00,$00,$00,$00,$03,$80,$ff,$ff,$ff,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$0f,$ff,$ff,$00,$00,$00,$00,$00,$00
.dsmp5
 db $50,$90,$0c,$62,$04,$30,$00,$28,$01,$80,$10,$40,$40,$40,$00,$0a
 db $80,$21,$00,$00,$00,$00,$00,$10,$00,$40,$00,$90,$00,$00,$24,$00







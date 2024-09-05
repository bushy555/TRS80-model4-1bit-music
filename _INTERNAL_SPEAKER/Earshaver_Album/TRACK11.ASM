	output	"TRACK11.BIN"

	org $6000
begin:


; 	EARSHAVER
; 	TRACK 11
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




;compiled music data

music_data




; TRACK11
;compiled music data

music_data
 dw .drumpar
.song
 db $9c,$c1,$00,$e1,$72,$83,$3f
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$e0,$b9,$82,$e4
 db $24,$00,$00
 db $24,$b1,$72,$83,$3f
 db $24,$00,$00
 db $98,$43,$02,$b3,$3f,$82,$e4
 db $24,$01,$01
 db $24,$b2,$e4,$00
 db $24,$01,$00
 db $a4,$81,$e0,$b9,$84,$55
 db $24,$00,$00
 db $24,$b1,$72,$00
 db $24,$00,$00
 db $a4,$03,$01,$80,$b9
 db $24,$00,$01
 db $24,$b0,$b9,$81,$72
 db $24,$01,$01
 db $9c,$c1,$00,$e0,$b9,$84,$55
 db $24,$00,$00
 db $24,$b1,$72,$00
 db $24,$00,$00
 db $98,$43,$02,$00,$80,$b9
 db $24,$00,$01
 db $24,$b0,$b9,$81,$72
 db $24,$01,$01
 db $a4,$81,$b0,$b9,$81,$6e
 db $24,$00,$00
 db $24,$b0,$a4,$81,$45
 db $24,$00,$00
 db $9c,$00,$e1,$72,$83,$3f
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$e0,$b9,$82,$e4
 db $24,$00,$00
 db $24,$b1,$72,$83,$3f
 db $24,$00,$00
 db $98,$43,$02,$b3,$3f,$82,$e4
 db $24,$01,$01
 db $24,$b2,$e4,$00
 db $24,$01,$00
 db $a4,$81,$e0,$b9,$84,$55
 db $24,$00,$00
 db $24,$b1,$72,$00
 db $24,$00,$00
 db $a4,$03,$01,$80,$b9
 db $24,$00,$01
 db $24,$b0,$b9,$81,$72
 db $24,$01,$01
 db $9c,$c1,$00,$e0,$b9,$84,$55
 db $24,$00,$00
 db $24,$b1,$72,$00
 db $24,$00,$00
 db $98,$43,$02,$00,$80,$b9
 db $24,$00,$01
 db $24,$b0,$b9,$81,$72
 db $24,$01,$01
 db $a4,$81,$b0,$b9,$81,$6e
 db $24,$00,$00
 db $24,$b0,$a4,$81,$45
 db $24,$00,$00
 db $9c,$00,$e1,$49,$82,$e4
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$e0,$a4,$82,$93
 db $24,$00,$00
 db $24,$b1,$49,$82,$e4
 db $24,$00,$00
 db $98,$43,$02,$b2,$e4,$82,$93
 db $24,$01,$01
 db $24,$b2,$93,$00
 db $24,$01,$00
 db $a4,$81,$e0,$a4,$83,$dc
 db $24,$00,$00
 db $24,$b1,$49,$00
 db $24,$00,$00
 db $a4,$03,$01,$80,$a4
 db $24,$00,$01
 db $24,$b0,$a4,$81,$49
 db $24,$01,$01
 db $9c,$c1,$00,$e0,$a4,$83,$dc
 db $24,$00,$00
 db $24,$b1,$49,$00
 db $24,$00,$00
 db $98,$43,$02,$00,$80,$a4
 db $24,$00,$01
 db $24,$b0,$a4,$81,$49
 db $24,$01,$01
 db $a4,$81,$b0,$a4,$81,$45
 db $24,$00,$00
 db $24,$b0,$92,$81,$21
 db $24,$00,$00
 db $9c,$00,$e1,$49,$82,$e4
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$e0,$a4,$82,$93
 db $24,$00,$00
 db $24,$b1,$49,$82,$e4
 db $24,$00,$00
 db $98,$43,$02,$b2,$e4,$82,$93
 db $24,$01,$01
 db $24,$b2,$93,$00
 db $24,$01,$00
 db $a4,$81,$e0,$a4,$83,$dc
 db $24,$00,$00
 db $24,$b1,$49,$00
 db $24,$00,$00
 db $a4,$03,$01,$80,$a4
 db $24,$00,$01
 db $24,$b0,$a4,$81,$49
 db $24,$01,$01
 db $9c,$c1,$00,$e0,$a4,$83,$dc
 db $24,$00,$00
 db $24,$b1,$49,$00
 db $24,$00,$00
 db $98,$43,$02,$00,$80,$a4
 db $24,$00,$01
 db $24,$b0,$a4,$81,$49
 db $24,$01,$01
 db $98,$c1,$02,$b0,$a4,$81,$45
 db $24,$00,$00
 db $98,$02,$b0,$92,$81,$21
 db $24,$00,$00
 db $9c,$00,$e1,$25,$81,$72
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$e0,$92,$82,$4b
 db $24,$00,$00
 db $24,$b1,$25,$81,$72
 db $24,$00,$00
 db $98,$43,$02,$b2,$93,$82,$4b
 db $24,$01,$01
 db $24,$b2,$4b,$00
 db $24,$01,$00
 db $a4,$81,$e0,$92,$83,$70
 db $24,$00,$00
 db $24,$b1,$25,$00
 db $24,$00,$00
 db $a4,$03,$01,$80,$92
 db $24,$00,$01
 db $24,$b0,$92,$81,$25
 db $24,$01,$01
 db $9c,$c1,$00,$e0,$92,$83,$70
 db $24,$00,$00
 db $24,$b1,$25,$00
 db $24,$00,$00
 db $98,$43,$02,$00,$80,$92
 db $24,$00,$01
 db $24,$b0,$92,$81,$25
 db $24,$01,$01
 db $a4,$81,$b0,$92,$81,$21
 db $24,$00,$00
 db $24,$b0,$82,$81,$01
 db $24,$00,$00
 db $9c,$00,$e1,$25,$81,$72
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$e0,$92,$82,$4b
 db $24,$00,$00
 db $24,$b1,$25,$81,$72
 db $24,$00,$00
 db $98,$43,$02,$b2,$93,$82,$4b
 db $24,$01,$01
 db $24,$b2,$4b,$00
 db $24,$01,$00
 db $a4,$81,$e0,$92,$83,$70
 db $24,$00,$00
 db $24,$b1,$25,$00
 db $24,$00,$00
 db $a4,$03,$01,$80,$92
 db $24,$00,$01
 db $24,$b0,$92,$81,$25
 db $24,$01,$01
 db $9c,$c1,$00,$e0,$92,$83,$70
 db $24,$00,$00
 db $24,$b1,$25,$00
 db $24,$00,$00
 db $98,$43,$02,$00,$80,$92
 db $24,$00,$01
 db $24,$b0,$92,$81,$25
 db $24,$01,$01
 db $a4,$81,$b0,$92,$81,$21
 db $24,$00,$00
 db $24,$b0,$82,$81,$01
 db $24,$00,$00
 db $9c,$00,$e0,$f7,$82,$2a
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$e0,$7b,$81,$ee
 db $24,$00,$00
 db $24,$b0,$f7,$82,$2a
 db $24,$00,$00
 db $98,$43,$02,$b2,$2a,$81,$ee
 db $24,$01,$01
 db $24,$b1,$ee,$00
 db $24,$01,$00
 db $a4,$81,$e0,$7b,$82,$e4
 db $24,$00,$00
 db $24,$b0,$f7,$00
 db $24,$00,$00
 db $a4,$03,$01,$80,$7b
 db $24,$00,$01
 db $24,$b0,$7b,$80,$f7
 db $24,$01,$01
 db $9c,$c1,$00,$e0,$7b,$82,$e4
 db $24,$00,$00
 db $24,$b0,$f7,$00
 db $24,$00,$00
 db $98,$43,$02,$00,$80,$7b
 db $24,$00,$01
 db $24,$b0,$7b,$80,$f7
 db $24,$01,$01
 db $a4,$81,$b0,$7b,$80,$f3
 db $24,$00,$00
 db $24,$b0,$6e,$80,$d8
 db $24,$00,$00
 db $9c,$00,$e1,$15,$81,$9f
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$e0,$8a,$82,$2a
 db $24,$00,$00
 db $24,$b1,$15,$81,$9f
 db $24,$00,$00
 db $98,$43,$02,$b2,$6e,$82,$2a
 db $24,$01,$01
 db $24,$b2,$2a,$00
 db $24,$01,$00
 db $9c,$c1,$00,$e1,$15,$81,$9f
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $a4,$03,$e0,$8a,$81,$15
 db $24,$01,$01
 db $9c,$c1,$00,$82,$26,$81,$9f
 db $24,$00,$00
 db $24,$81,$0f,$00
 db $24,$00,$00
 db $98,$02,$81,$0d,$00
 db $24,$00,$00
 db $24,$81,$0b,$00
 db $24,$00,$00
 db $98,$02,$80,$8a,$81,$11
 db $24,$00,$00
 db $98,$02,$80,$7b,$80,$f3
 db $24,$00,$00
 db $9c,$43,$00,$b4,$51,$a0,$b9
 db $24,$01,$01
 db $24,$b2,$e4,$a0,$b9
 db $24,$01,$00
 db $a0,$cb,$04,$a3,$3d,$a3,$3f
 db $24,$00,$00
 db $24,$a3,$6e,$a3,$70
 db $24,$00,$00
 db $8c,$43,$06,$93,$3f,$b0,$b9
 db $24,$00,$00
 db $a4,$8b,$a3,$3d,$a3,$3f
 db $24,$00,$00
 db $9c,$43,$08,$83,$70,$01
 db $24,$00,$00
 db $9c,$cb,$08,$82,$91,$92,$93
 db $24,$00,$00
 db $a4,$03,$94,$55,$c0,$b9
 db $24,$01,$01
 db $24,$92,$93,$c0,$b9
 db $24,$00,$00
 db $9c,$00,$a3,$3f,$01
 db $24,$01,$00
 db $24,$a2,$e4,$c0,$b9
 db $24,$01,$01
 db $8c,$06,$b4,$55,$d0,$b9
 db $24,$01,$00
 db $24,$b2,$e4,$01
 db $24,$01,$00
 db $a0,$04,$c3,$3f,$d0,$b9
 db $24,$01,$00
 db $24,$c2,$e4,$01
 db $24,$01,$00
 db $9c,$00,$b4,$55,$a0,$b9
 db $24,$01,$01
 db $24,$b2,$e4,$a0,$b9
 db $24,$01,$00
 db $a0,$04,$a3,$3f,$01
 db $24,$01,$00
 db $24,$a2,$e4,$00
 db $24,$01,$00
 db $8c,$06,$94,$55,$b0,$b9
 db $24,$01,$00
 db $24,$92,$e4,$01
 db $24,$01,$00
 db $9c,$08,$83,$3f,$00
 db $24,$01,$00
 db $9c,$08,$82,$e4,$00
 db $24,$01,$00
 db $24,$94,$55,$c0,$b9
 db $24,$01,$01
 db $9c,$08,$92,$e4,$c0,$b9
 db $24,$01,$00
 db $9c,$00,$a3,$3f,$01
 db $24,$01,$00
 db $24,$a2,$e4,$c0,$b9
 db $24,$01,$01
 db $8c,$06,$b4,$55,$d0,$b9
 db $24,$01,$00
 db $24,$b2,$e4,$01
 db $24,$01,$00
 db $94,$0a,$c3,$3f,$d0,$b9
 db $24,$01,$00
 db $94,$0a,$c2,$e4,$01
 db $24,$01,$00
 db $9c,$00,$b4,$55,$a0,$b9
 db $24,$01,$01
 db $24,$b2,$e4,$a0,$b9
 db $24,$01,$00
 db $94,$cb,$0c,$83,$3d,$83,$3f
 db $24,$00,$00
 db $94,$0c,$83,$6e,$83,$70
 db $24,$00,$00
 db $8c,$43,$06,$93,$3f,$b0,$b9
 db $24,$00,$00
 db $a4,$8b,$83,$3d,$83,$3f
 db $24,$00,$00
 db $9c,$43,$08,$83,$70,$01
 db $24,$00,$00
 db $9c,$cb,$08,$82,$91,$92,$93
 db $24,$00,$00
 db $a4,$03,$93,$3f,$c0,$b9
 db $24,$00,$01
 db $24,$92,$93,$c0,$b9
 db $24,$00,$00
 db $9c,$cb,$00,$82,$28,$82,$2a
 db $24,$00,$00
 db $a4,$03,$82,$e4,$80,$b9
 db $24,$01,$01
 db $8c,$cb,$06,$83,$3d,$93,$3f
 db $24,$00,$00
 db $a4,$03,$82,$2a,$01
 db $24,$00,$00
 db $a0,$cb,$04,$82,$91,$a2,$93
 db $24,$00,$00
 db $a4,$03,$83,$3f,$01
 db $24,$00,$00
 db $9c,$cb,$00,$82,$e2,$a2,$e4
 db $24,$00,$00
 db $a4,$03,$82,$93,$a0,$b9
 db $24,$00,$00
 db $a0,$04,$a3,$3f,$01
 db $24,$01,$00
 db $24,$a2,$e4,$00
 db $24,$00,$00
 db $8c,$06,$94,$55,$b0,$b9
 db $24,$01,$00
 db $24,$92,$e4,$01
 db $24,$01,$00
 db $9c,$08,$83,$3f,$00
 db $24,$01,$00
 db $9c,$08,$82,$e4,$00
 db $24,$01,$00
 db $24,$94,$55,$c0,$b9
 db $24,$01,$01
 db $9c,$08,$92,$e4,$c0,$b9
 db $24,$01,$00
 db $9c,$00,$a3,$3f,$01
 db $24,$01,$00
 db $24,$a2,$e4,$c0,$b9
 db $24,$01,$01
 db $8c,$06,$b4,$55,$d0,$b9
 db $24,$01,$00
 db $24,$b2,$e4,$01
 db $24,$01,$00
 db $94,$0a,$c3,$3f,$d0,$b9
 db $24,$01,$00
 db $94,$0a,$c2,$e4,$01
 db $24,$01,$00
 db $9c,$00,$c4,$55,$d0,$92
 db $24,$01,$01
 db $24,$c2,$e4,$d0,$92
 db $24,$01,$00
 db $94,$cb,$0c,$a3,$3d,$a3,$3b
 db $24,$00,$00
 db $94,$0c,$a3,$70,$a3,$6a
 db $24,$00,$00
 db $8c,$43,$06,$a3,$3f,$a0,$92
 db $24,$00,$00
 db $a4,$8b,$a3,$3d,$a3,$3b
 db $24,$00,$00
 db $9c,$43,$08,$a3,$70,$01
 db $24,$00,$00
 db $9c,$cb,$08,$a2,$91,$a2,$8d
 db $24,$00,$00
 db $a4,$03,$a3,$3f,$a0,$92
 db $24,$00,$01
 db $24,$a2,$93,$a0,$92
 db $24,$00,$00
 db $9c,$00,$a3,$3f,$01
 db $24,$01,$00
 db $24,$a2,$e4,$a0,$92
 db $24,$01,$01
 db $8c,$06,$a4,$55,$a0,$92
 db $24,$01,$00
 db $24,$a2,$e4,$01
 db $24,$01,$00
 db $a0,$cb,$04,$a2,$28,$a2,$26
 db $24,$00,$00
 db $a4,$03,$a2,$e4,$01
 db $24,$01,$00
 db $9c,$cb,$00,$a2,$e2,$a2,$de
 db $24,$00,$00
 db $a4,$03,$a2,$2a,$a0,$92
 db $24,$00,$00
 db $a0,$04,$a2,$e4,$01
 db $24,$00,$00
 db $24,$00,$00
 db $24,$01,$00
 db $8c,$cb,$06,$a2,$49,$a2,$47
 db $24,$00,$00
 db $a4,$03,$a2,$e4,$01
 db $24,$01,$00
 db $9c,$cb,$08,$a3,$3d,$a3,$39
 db $24,$00,$00
 db $9c,$43,$08,$a2,$4b,$01
 db $24,$00,$00
 db $24,$a3,$3f,$a0,$92
 db $24,$00,$01
 db $9c,$08,$a2,$e4,$a0,$92
 db $24,$01,$00
 db $9c,$cb,$00,$a3,$6e,$a3,$6c
 db $24,$00,$00
 db $a4,$03,$a2,$e4,$a0,$92
 db $24,$01,$01
 db $8c,$cb,$06,$a3,$3d,$a3,$39
 db $24,$00,$00
 db $a4,$03,$a3,$70,$01
 db $24,$00,$00
 db $94,$cb,$0a,$a2,$91,$b2,$8b
 db $24,$00,$00
 db $94,$43,$0a,$a3,$3f,$01
 db $24,$00,$00
 db $9c,$00,$a2,$93,$b0,$7b
 db $24,$00,$01
 db $24,$a2,$e4,$b0,$7b
 db $24,$01,$00
 db $94,$cb,$0c,$a3,$3d,$a3,$3b
 db $24,$00,$00
 db $94,$0c,$a3,$6e,$a3,$6a
 db $24,$00,$00
 db $8c,$43,$06,$a3,$3f,$a0,$7b
 db $24,$00,$00
 db $a4,$8b,$a3,$3d,$a3,$3b
 db $24,$00,$00
 db $9c,$43,$08,$a3,$70,$01
 db $24,$00,$00
 db $9c,$cb,$08,$a4,$53,$a4,$4f
 db $24,$00,$00
 db $a4,$03,$a3,$3f,$a0,$7b
 db $24,$00,$01
 db $24,$a2,$e4,$a0,$7b
 db $24,$01,$00
 db $9c,$cb,$00,$a2,$28,$a2,$26
 db $24,$00,$00
 db $a4,$03,$a2,$e4,$a0,$7b
 db $24,$01,$01
 db $8c,$cb,$06,$a4,$53,$a4,$4f
 db $24,$00,$00
 db $a4,$03,$a2,$2a,$01
 db $24,$00,$00
 db $a0,$cb,$04,$a3,$3d,$a3,$37
 db $24,$00,$00
 db $a4,$03,$a4,$55,$01
 db $24,$00,$00
 db $9c,$cb,$00,$a3,$6e,$a3,$6a
 db $24,$00,$00
 db $a4,$03,$a3,$3f,$a0,$8a
 db $24,$00,$00
 db $a0,$04,$a3,$70,$01
 db $24,$00,$00
 db $24,$a2,$e4,$00
 db $24,$01,$00
 db $8c,$cb,$06,$a3,$3d,$a3,$6e
 db $24,$a3,$6e,$00
 db $24,$00,$00
 db $a4,$03,$a3,$3f,$01
 db $9c,$cb,$00,$a3,$3d,$a3,$39
 db $24,$00,$00
 db $8c,$43,$06,$a3,$70,$01
 db $24,$00,$00
 db $24,$a3,$3f,$a0,$8a
 db $24,$00,$01
 db $9c,$00,$a2,$e4,$a0,$8a
 db $24,$01,$00
 db $8c,$cb,$06,$a2,$28,$a2,$26
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $9c,$43,$00,$a2,$2a,$a0,$8a
 db $24,$00,$00
 db $a0,$0e,$a2,$e4,$01
 db $24,$01,$00
 db $8c,$06,$a3,$3f,$a0,$8a
 db $24,$01,$00
 db $8c,$06,$a2,$e4,$01
 db $24,$01,$00
 db $9c,$c1,$00,$e1,$72,$83,$3f
 db $24,$00,$00
 db $a0,$0e,$00,$00
 db $24,$00,$00
 db $a0,$04,$e0,$b9,$82,$e4
 db $24,$00,$00
 db $a0,$0e,$b1,$72,$83,$3f
 db $24,$00,$00
 db $98,$43,$02,$b3,$3f,$82,$e4
 db $24,$01,$01
 db $a0,$0e,$b2,$e4,$00
 db $24,$01,$00
 db $a0,$c1,$04,$e0,$b9,$84,$55
 db $24,$00,$00
 db $a0,$0e,$b1,$72,$00
 db $24,$00,$00
 db $a0,$43,$04,$01,$80,$b9
 db $24,$00,$01
 db $a0,$0e,$b0,$b9,$81,$72
 db $24,$01,$01
 db $9c,$c1,$00,$e0,$b9,$84,$55
 db $24,$00,$00
 db $a0,$0e,$b1,$72,$00
 db $24,$00,$00
 db $98,$43,$02,$00,$80,$b9
 db $24,$00,$01
 db $a0,$0e,$b0,$b9,$81,$72
 db $24,$01,$01
 db $a0,$c1,$04,$b0,$b9,$81,$6e
 db $24,$00,$00
 db $a0,$0e,$b0,$a4,$81,$45
 db $24,$00,$00
 db $9c,$00,$e1,$72,$83,$3f
 db $24,$00,$00
 db $a0,$0e,$00,$00
 db $24,$00,$00
 db $a0,$04,$e0,$b9,$82,$e4
 db $24,$00,$00
 db $a0,$0e,$b1,$72,$83,$3f
 db $24,$00,$00
 db $98,$43,$02,$b3,$3f,$82,$e4
 db $24,$01,$01
 db $a0,$0e,$b2,$e4,$00
 db $24,$01,$00
 db $a0,$c1,$04,$e0,$b9,$84,$55
 db $24,$00,$00
 db $a0,$0e,$b1,$72,$00
 db $24,$00,$00
 db $a0,$43,$04,$01,$80,$b9
 db $24,$00,$01
 db $a0,$0e,$b0,$b9,$81,$72
 db $24,$01,$01
 db $9c,$c1,$00,$e0,$b9,$84,$55
 db $24,$00,$00
 db $a0,$0e,$b1,$72,$00
 db $24,$00,$00
 db $98,$43,$02,$00,$80,$b9
 db $24,$00,$01
 db $a0,$0e,$b0,$b9,$81,$72
 db $24,$01,$01
 db $a0,$c1,$04,$b0,$b9,$81,$6e
 db $24,$00,$00
 db $a0,$0e,$b0,$a4,$81,$45
 db $24,$00,$00
 db $9c,$00,$e1,$49,$82,$e4
 db $24,$00,$00
 db $a0,$0e,$00,$00
 db $24,$00,$00
 db $a0,$04,$e0,$a4,$82,$93
 db $24,$00,$00
 db $a0,$0e,$b1,$49,$82,$e4
 db $24,$00,$00
 db $98,$43,$02,$b2,$e4,$82,$93
 db $24,$01,$01
 db $a0,$0e,$b2,$93,$00
 db $24,$01,$00
 db $a0,$c1,$04,$e0,$a4,$83,$dc
 db $24,$00,$00
 db $a0,$0e,$b1,$49,$00
 db $24,$00,$00
 db $a0,$43,$04,$01,$80,$a4
 db $24,$00,$01
 db $a0,$0e,$b0,$a4,$81,$49
 db $24,$01,$01
 db $9c,$c1,$00,$e0,$a4,$83,$dc
 db $24,$00,$00
 db $a0,$0e,$b1,$49,$00
 db $24,$00,$00
 db $98,$43,$02,$00,$80,$a4
 db $24,$00,$01
 db $a0,$0e,$b0,$a4,$81,$49
 db $24,$01,$01
 db $a0,$c1,$04,$b0,$a4,$81,$45
 db $24,$00,$00
 db $a0,$0e,$b0,$92,$81,$21
 db $24,$00,$00
 db $9c,$00,$e1,$49,$82,$e4
 db $24,$00,$00
 db $a0,$0e,$00,$00
 db $24,$00,$00
 db $a0,$04,$e0,$a4,$82,$93
 db $24,$00,$00
 db $a0,$0e,$b1,$49,$82,$e4
 db $24,$00,$00
 db $98,$43,$02,$b2,$e4,$82,$93
 db $24,$01,$01
 db $a0,$0e,$b2,$93,$00
 db $24,$01,$00
 db $a0,$c1,$04,$e0,$a4,$83,$dc
 db $24,$00,$00
 db $a0,$0e,$b1,$49,$00
 db $24,$00,$00
 db $a0,$43,$04,$01,$80,$a4
 db $24,$00,$01
 db $a0,$0e,$b0,$a4,$81,$49
 db $24,$01,$01
 db $9c,$c1,$00,$e0,$a4,$83,$dc
 db $24,$00,$00
 db $a0,$0e,$b1,$49,$00
 db $24,$00,$00
 db $98,$43,$02,$00,$80,$a4
 db $24,$00,$01
 db $a0,$0e,$b0,$a4,$81,$49
 db $24,$01,$01
 db $98,$c1,$02,$b0,$a4,$81,$45
 db $24,$00,$00
 db $98,$02,$b0,$92,$81,$21
 db $24,$00,$00
 db $9c,$00,$e1,$25,$81,$72
 db $24,$00,$00
 db $a0,$0e,$00,$00
 db $24,$00,$00
 db $a0,$04,$e0,$92,$82,$4b
 db $24,$00,$00
 db $a0,$0e,$b1,$25,$81,$72
 db $24,$00,$00
 db $98,$43,$02,$b2,$93,$82,$4b
 db $24,$01,$01
 db $a0,$0e,$b2,$4b,$00
 db $24,$01,$00
 db $a0,$c1,$04,$e0,$92,$83,$70
 db $24,$00,$00
 db $a0,$0e,$b1,$25,$00
 db $24,$00,$00
 db $a0,$43,$04,$01,$80,$92
 db $24,$00,$01
 db $a0,$0e,$b0,$92,$81,$25
 db $24,$01,$01
 db $9c,$c1,$00,$e0,$92,$83,$70
 db $24,$00,$00
 db $a0,$0e,$b1,$25,$00
 db $24,$00,$00
 db $98,$43,$02,$00,$80,$92
 db $24,$00,$01
 db $a0,$0e,$b0,$92,$81,$25
 db $24,$01,$01
 db $a0,$c1,$04,$b0,$92,$81,$21
 db $24,$00,$00
 db $a0,$0e,$b0,$82,$81,$01
 db $24,$00,$00
 db $9c,$00,$e1,$25,$81,$72
 db $24,$00,$00
 db $a0,$0e,$00,$00
 db $24,$00,$00
 db $a0,$04,$e0,$92,$82,$4b
 db $24,$00,$00
 db $a0,$0e,$b1,$25,$81,$72
 db $24,$00,$00
 db $98,$43,$02,$b2,$93,$82,$4b
 db $24,$01,$01
 db $a0,$0e,$b2,$4b,$00
 db $24,$01,$00
 db $a0,$c1,$04,$e0,$92,$83,$70
 db $24,$00,$00
 db $a0,$0e,$b1,$25,$00
 db $24,$00,$00
 db $a0,$43,$04,$01,$80,$92
 db $24,$00,$01
 db $a0,$0e,$b0,$92,$81,$25
 db $24,$01,$01
 db $9c,$c1,$00,$e0,$92,$83,$70
 db $24,$00,$00
 db $a0,$0e,$b1,$25,$00
 db $24,$00,$00
 db $98,$43,$02,$00,$80,$92
 db $24,$00,$01
 db $a0,$0e,$b0,$92,$81,$25
 db $24,$01,$01
 db $a0,$c1,$04,$b0,$92,$81,$21
 db $24,$00,$00
 db $a0,$0e,$b0,$82,$81,$01
 db $24,$00,$00
 db $9c,$00,$e0,$f7,$82,$2a
 db $24,$00,$00
 db $a0,$0e,$00,$00
 db $24,$00,$00
 db $a0,$04,$e0,$7b,$81,$ee
 db $24,$00,$00
 db $a0,$0e,$b0,$f7,$82,$2a
 db $24,$00,$00
 db $98,$43,$02,$b2,$2a,$81,$ee
 db $24,$01,$01
 db $a0,$0e,$b1,$ee,$00
 db $24,$01,$00
 db $a0,$c1,$04,$e0,$7b,$82,$e4
 db $24,$00,$00
 db $a0,$0e,$b0,$f7,$00
 db $24,$00,$00
 db $a0,$43,$04,$01,$80,$7b
 db $24,$00,$01
 db $a0,$0e,$b0,$7b,$80,$f7
 db $24,$01,$01
 db $9c,$c1,$00,$e0,$7b,$82,$e4
 db $24,$00,$00
 db $a0,$0e,$b0,$f7,$00
 db $24,$00,$00
 db $98,$43,$02,$00,$80,$7b
 db $24,$00,$01
 db $a0,$0e,$b0,$7b,$80,$f7
 db $24,$01,$01
 db $a0,$c1,$04,$b0,$7b,$80,$f3
 db $24,$00,$00
 db $a0,$0e,$b0,$6e,$80,$d8
 db $24,$00,$00
 db $9c,$00,$e1,$15,$81,$9f
 db $24,$00,$00
 db $a0,$0e,$00,$00
 db $24,$00,$00
 db $98,$02,$e0,$8a,$82,$2a
 db $24,$00,$00
 db $9c,$00,$b1,$15,$81,$9f
 db $24,$00,$00
 db $a0,$43,$0e,$b2,$6e,$82,$2a
 db $24,$01,$01
 db $9c,$00,$b2,$2a,$00
 db $24,$01,$00
 db $98,$c1,$02,$e1,$15,$81,$9f
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $a4,$03,$e0,$8a,$81,$15
 db $24,$01,$01
 db $a4,$81,$82,$26,$81,$9f
 db $24,$82,$22,$00
 db $24,$82,$1e,$00
 db $98,$02,$82,$1a,$00
 db $8c,$06,$82,$0b,$00
 db $24,$81,$ee,$00
 db $24,$81,$d2,$00
 db $24,$81,$b8,$00
 db $8c,$06,$80,$8a,$81,$11
 db $24,$00,$00
 db $8c,$06,$80,$7b,$80,$f3
 db $24,$00,$00
 db $9c,$43,$00,$b4,$51,$a0,$b9
 db $24,$01,$01
 db $24,$b2,$e4,$a0,$b9
 db $24,$01,$00
 db $a0,$cb,$04,$a3,$3d,$a3,$3f
 db $24,$00,$00
 db $24,$a3,$6e,$a3,$70
 db $24,$00,$00
 db $8c,$43,$06,$93,$3f,$b0,$b9
 db $24,$00,$00
 db $a4,$8b,$a3,$3d,$a3,$3f
 db $24,$00,$00
 db $9c,$43,$08,$83,$70,$01
 db $24,$00,$00
 db $9c,$cb,$08,$82,$91,$92,$93
 db $24,$00,$00
 db $a4,$03,$94,$55,$c0,$b9
 db $24,$01,$01
 db $24,$92,$93,$c0,$b9
 db $24,$00,$00
 db $9c,$00,$b5,$c9,$01
 db $24,$01,$00
 db $24,$c5,$c9,$c0,$b9
 db $24,$01,$01
 db $8c,$06,$d5,$c9,$d0,$b9
 db $24,$01,$00
 db $24,$e5,$c9,$01
 db $24,$01,$00
 db $a0,$04,$f5,$c9,$d0,$b9
 db $24,$01,$00
 db $24,$f2,$e4,$01
 db $24,$01,$00
 db $9c,$00,$b4,$55,$a0,$b9
 db $24,$01,$01
 db $24,$b2,$e4,$a0,$b9
 db $24,$01,$00
 db $a0,$04,$a3,$3f,$f1,$49
 db $24,$01,$00
 db $24,$a2,$e4,$f1,$72
 db $24,$01,$00
 db $8c,$06,$f1,$49,$b0,$b9
 db $24,$00,$00
 db $24,$f1,$72,$f1,$49
 db $24,$00,$00
 db $9c,$08,$83,$3f,$01
 db $24,$01,$00
 db $9c,$08,$f1,$49,$f1,$72
 db $24,$00,$00
 db $24,$94,$55,$c0,$b9
 db $24,$01,$01
 db $9c,$08,$f1,$72,$c0,$b9
 db $24,$00,$00
 db $9c,$00,$b5,$c9,$01
 db $24,$01,$00
 db $24,$c5,$c9,$c0,$b9
 db $24,$01,$01
 db $8c,$06,$d5,$c9,$d0,$b9
 db $24,$01,$00
 db $24,$e5,$c9,$01
 db $24,$01,$00
 db $94,$0a,$f5,$c9,$d0,$b9
 db $24,$01,$00
 db $94,$0a,$f2,$e4,$01
 db $24,$01,$00
 db $9c,$00,$b4,$55,$a0,$b9
 db $24,$01,$01
 db $24,$b2,$e4,$a0,$b9
 db $24,$01,$00
 db $94,$cb,$0c,$83,$3d,$83,$3f
 db $24,$00,$00
 db $94,$0c,$83,$6e,$83,$70
 db $24,$00,$00
 db $8c,$43,$06,$93,$3f,$b0,$b9
 db $24,$00,$00
 db $a4,$8b,$83,$3d,$83,$3f
 db $24,$00,$00
 db $9c,$43,$08,$83,$70,$01
 db $24,$00,$00
 db $9c,$cb,$08,$82,$91,$92,$93
 db $24,$00,$00
 db $a4,$03,$93,$3f,$c0,$b9
 db $24,$00,$01
 db $24,$92,$93,$c0,$b9
 db $24,$00,$00
 db $9c,$cb,$00,$82,$28,$82,$2a
 db $24,$00,$00
 db $a4,$03,$c5,$c9,$80,$b9
 db $24,$01,$01
 db $8c,$cb,$06,$83,$3d,$93,$3f
 db $24,$00,$00
 db $a4,$03,$e5,$c9,$01
 db $24,$01,$00
 db $a0,$cb,$04,$82,$91,$a2,$93
 db $24,$00,$00
 db $a4,$03,$83,$3f,$01
 db $24,$00,$00
 db $9c,$cb,$00,$82,$e2,$a2,$e4
 db $24,$00,$00
 db $a4,$03,$82,$93,$a0,$b9
 db $24,$00,$00
 db $a0,$04,$a3,$3f,$e1,$49
 db $24,$01,$00
 db $24,$a2,$e4,$e1,$72
 db $24,$00,$00
 db $8c,$06,$e1,$49,$b0,$b9
 db $24,$00,$00
 db $24,$e1,$72,$e1,$49
 db $24,$00,$00
 db $9c,$08,$83,$3f,$01
 db $24,$01,$00
 db $9c,$08,$e1,$49,$e1,$72
 db $24,$00,$00
 db $24,$94,$55,$c0,$b9
 db $24,$01,$01
 db $9c,$08,$e1,$72,$c0,$b9
 db $24,$00,$00
 db $9c,$00,$b5,$c9,$01
 db $24,$01,$00
 db $24,$c5,$c9,$c0,$b9
 db $24,$01,$01
 db $8c,$06,$d5,$c9,$d0,$b9
 db $24,$01,$00
 db $24,$e5,$c9,$01
 db $24,$01,$00
 db $94,$0a,$f5,$c9,$d0,$b9
 db $24,$01,$00
 db $94,$0a,$f2,$e4,$01
 db $24,$01,$00
 db $9c,$00,$f4,$55,$d0,$92
 db $24,$01,$01
 db $24,$f2,$e4,$d0,$92
 db $24,$01,$00
 db $94,$cb,$0c,$a3,$3d,$a3,$3b
 db $24,$00,$00
 db $94,$0c,$a3,$70,$a3,$6a
 db $24,$00,$00
 db $8c,$43,$06,$a3,$3f,$a0,$92
 db $24,$00,$00
 db $a4,$8b,$a3,$3d,$a3,$3b
 db $24,$00,$00
 db $9c,$43,$08,$a3,$70,$01
 db $24,$00,$00
 db $9c,$cb,$08,$a2,$91,$a2,$8d
 db $24,$00,$00
 db $a4,$03,$a3,$3f,$a0,$92
 db $24,$00,$01
 db $24,$a2,$93,$a0,$92
 db $24,$00,$00
 db $9c,$00,$b5,$c9,$01
 db $24,$01,$00
 db $24,$c5,$c9,$a0,$92
 db $24,$01,$01
 db $8c,$06,$d5,$c9,$a0,$92
 db $24,$01,$00
 db $24,$e5,$c9,$01
 db $24,$01,$00
 db $a0,$cb,$04,$a2,$28,$a2,$26
 db $24,$00,$00
 db $a4,$03,$a2,$e4,$01
 db $24,$01,$00
 db $9c,$cb,$00,$a2,$e2,$a2,$de
 db $24,$00,$00
 db $a4,$03,$a2,$2a,$a0,$92
 db $24,$00,$00
 db $a0,$04,$a2,$e4,$01
 db $24,$00,$00
 db $24,$00,$00
 db $24,$01,$00
 db $8c,$cb,$06,$a2,$49,$a2,$47
 db $24,$00,$00
 db $a4,$03,$a2,$e4,$01
 db $24,$01,$00
 db $9c,$cb,$08,$a3,$3d,$a3,$39
 db $24,$00,$00
 db $9c,$43,$08,$a2,$4b,$01
 db $24,$00,$00
 db $24,$a3,$3f,$a0,$92
 db $24,$00,$01
 db $9c,$08,$a2,$e4,$a0,$92
 db $24,$01,$00
 db $9c,$cb,$00,$a3,$6e,$a3,$6c
 db $24,$00,$00
 db $a4,$03,$c5,$c9,$a0,$92
 db $24,$01,$01
 db $8c,$cb,$06,$a3,$3d,$a3,$39
 db $24,$00,$00
 db $a4,$03,$e5,$c9,$01
 db $24,$01,$00
 db $94,$cb,$0a,$a2,$91,$b2,$8b
 db $24,$00,$00
 db $94,$43,$0a,$a3,$3f,$01
 db $24,$00,$00
 db $9c,$00,$a2,$93,$b0,$7b
 db $24,$00,$01
 db $24,$a2,$e4,$b0,$7b
 db $24,$01,$00
 db $94,$cb,$0c,$a3,$3d,$a3,$3b
 db $24,$00,$00
 db $94,$0c,$a3,$6e,$a3,$6a
 db $24,$00,$00
 db $8c,$43,$06,$a3,$3f,$a0,$7b
 db $24,$00,$00
 db $a4,$8b,$a3,$3d,$a3,$3b
 db $24,$00,$00
 db $9c,$43,$08,$a3,$70,$01
 db $24,$00,$00
 db $9c,$cb,$08,$a4,$53,$a4,$4f
 db $24,$00,$00
 db $a4,$03,$a3,$3f,$a0,$7b
 db $24,$00,$01
 db $24,$a2,$e4,$a0,$7b
 db $24,$01,$00
 db $9c,$cb,$00,$a2,$28,$a2,$26
 db $24,$00,$00
 db $a4,$03,$a5,$bf,$a0,$7b
 db $24,$01,$01
 db $8c,$cb,$06,$a4,$53,$a4,$4f
 db $24,$00,$00
 db $a4,$03,$a5,$bb,$01
 db $24,$01,$00
 db $a0,$cb,$04,$a3,$3d,$a3,$37
 db $24,$00,$00
 db $a4,$03,$a4,$55,$01
 db $24,$00,$00
 db $9c,$cb,$00,$a3,$6e,$a3,$6a
 db $24,$00,$00
 db $a4,$03,$a3,$3f,$a0,$8a
 db $24,$00,$00
 db $a0,$04,$a3,$70,$01
 db $24,$00,$00
 db $24,$a2,$e4,$00
 db $24,$01,$00
 db $8c,$cb,$06,$a3,$3d,$a3,$6e
 db $24,$a3,$6e,$00
 db $24,$00,$00
 db $a4,$03,$a3,$3f,$01
 db $9c,$cb,$00,$a3,$3d,$a3,$39
 db $24,$00,$00
 db $8c,$43,$06,$a3,$70,$01
 db $24,$00,$00
 db $24,$a3,$3f,$a0,$8a
 db $24,$00,$01
 db $9c,$00,$a2,$e4,$a0,$8a
 db $24,$01,$00
 db $8c,$cb,$06,$a2,$28,$a2,$26
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $9c,$43,$00,$a2,$2a,$a0,$8a
 db $24,$00,$00
 db $a0,$0e,$a2,$e4,$01
 db $24,$01,$00
 db $8c,$06,$a3,$3f,$a0,$8a
 db $24,$01,$00
 db $8c,$06,$a2,$e4,$01
 db $24,$01,$00
 db $9c,$00,$f3,$70,$a0,$92
 db $24,$01,$01
 db $24,$f2,$4b,$a0,$92
 db $24,$01,$01
 db $a0,$04,$f2,$e4,$00
 db $24,$01,$00
 db $24,$f2,$4b,$00
 db $24,$01,$00
 db $8c,$c1,$06,$d2,$4b,$b2,$6c
 db $24,$00,$b2,$93
 db $24,$00,$00
 db $24,$00,$00
 db $9c,$43,$08,$d2,$93,$01
 db $24,$00,$00
 db $9c,$08,$d2,$4b,$00
 db $24,$01,$00
 db $a4,$81,$c2,$4b,$c2,$e2
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $9c,$43,$00,$c2,$e4,$01
 db $24,$00,$00
 db $24,$c2,$4b,$c0,$92
 db $24,$01,$01
 db $8c,$c1,$06,$82,$4b,$d4,$53
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $a0,$04,$00,$d3,$da
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $9c,$43,$00,$b3,$dc,$a0,$a4
 db $24,$00,$01
 db $24,$b2,$93,$a0,$a4
 db $24,$01,$00
 db $a0,$04,$b3,$3f,$d1,$49
 db $24,$01,$00
 db $24,$b2,$93,$d1,$72
 db $24,$01,$00
 db $8c,$06,$d1,$49,$b0,$a4
 db $24,$00,$00
 db $24,$d1,$72,$d1,$49
 db $24,$00,$00
 db $9c,$08,$a3,$3f,$01
 db $24,$01,$00
 db $9c,$08,$a2,$93,$c1,$72
 db $24,$01,$00
 db $24,$a3,$dc,$c0,$a4
 db $24,$01,$01
 db $9c,$08,$d1,$72,$c0,$a4
 db $24,$00,$00
 db $9c,$00,$93,$3f,$d1,$49
 db $24,$01,$00
 db $24,$92,$93,$d1,$72
 db $24,$01,$00
 db $8c,$06,$d1,$49,$c0,$a4
 db $24,$00,$00
 db $24,$d1,$72,$d1,$49
 db $24,$00,$00
 db $a0,$04,$83,$3f,$c0,$a4
 db $24,$01,$00
 db $24,$d1,$49,$01
 db $24,$00,$00
 db $9c,$00,$83,$3f,$a0,$8a
 db $24,$01,$01
 db $24,$82,$2a,$a0,$8a
 db $24,$01,$00
 db $a0,$04,$82,$93,$01
 db $24,$01,$00
 db $24,$82,$2a,$00
 db $24,$01,$00
 db $8c,$c1,$06,$c2,$2a,$c2,$91
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $9c,$43,$08,$c2,$93,$01
 db $24,$00,$00
 db $9c,$08,$c2,$2a,$00
 db $24,$01,$00
 db $a4,$81,$c2,$2a,$c2,$e2
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $9c,$43,$00,$c2,$e4,$01
 db $24,$00,$00
 db $24,$c2,$2a,$c0,$8a
 db $24,$01,$01
 db $8c,$c1,$06,$82,$2a,$d3,$6e
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $a0,$04,$00,$d3,$3d
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $9c,$45,$00,$f8,$a7,$a0,$b9
 db $24,$01,$e6,$e1
 db $24,$d6,$7e,$a0,$b9
 db $24,$01,$c5,$27
 db $a0,$43,$04,$f8,$ab,$b4,$55
 db $24,$e6,$e1,$a3,$70
 db $24,$d6,$7e,$93,$3f
 db $24,$c5,$27,$82,$93
 db $8c,$06,$88,$ab,$b0,$b9
 db $24,$96,$e1,$00
 db $24,$a6,$7e,$01
 db $24,$b5,$27,$00
 db $9c,$08,$c4,$55,$00
 db $24,$d3,$70,$00
 db $98,$02,$e3,$3f,$00
 db $24,$f2,$93,$00
 db $9c,$45,$00,$00,$c0,$b9
 db $24,$e3,$3f,$01
 db $9c,$08,$d3,$70,$c0,$b9
 db $24,$c4,$55,$00
 db $9c,$00,$b5,$27,$01
 db $24,$a6,$7e,$00
 db $24,$96,$e1,$c0,$b9
 db $24,$88,$ab,$01
 db $8c,$43,$06,$82,$93,$d0,$b9
 db $24,$93,$3f,$00
 db $98,$02,$a3,$70,$01
 db $24,$b4,$55,$00
 db $94,$0a,$c5,$27,$d0,$b9
 db $24,$d6,$7e,$00
 db $94,$0a,$e6,$e1,$01
 db $24,$f8,$ab,$00
 db $9c,$00,$f3,$70,$d0,$92
 db $24,$01,$01
 db $24,$f2,$4b,$d0,$92
 db $24,$01,$00
 db $94,$0c,$f2,$e4,$01
 db $24,$01,$00
 db $94,$0c,$f2,$4b,$00
 db $24,$01,$00
 db $8c,$c1,$06,$d2,$4b,$b2,$91
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $9c,$43,$08,$d2,$93,$01
 db $24,$00,$00
 db $9c,$08,$d2,$4b,$00
 db $24,$01,$00
 db $24,$d3,$70,$b0,$92
 db $24,$01,$01
 db $24,$d2,$4b,$b0,$92
 db $24,$01,$00
 db $9c,$c1,$00,$d2,$4b,$b2,$e4
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $8c,$06,$c2,$4b,$b4,$55
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $a0,$04,$00,$b3,$dc
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $9c,$43,$00,$b3,$dc,$a0,$a4
 db $24,$00,$01
 db $24,$b2,$93,$a0,$a4
 db $24,$01,$00
 db $a0,$04,$b3,$3f,$c1,$9d
 db $24,$01,$00
 db $24,$b2,$93,$01
 db $24,$01,$00
 db $8c,$06,$c1,$9f,$b1,$b6
 db $24,$00,$00
 db $24,$a2,$93,$a1,$9d
 db $24,$01,$00
 db $9c,$08,$b1,$b8,$01
 db $24,$00,$00
 db $9c,$08,$a1,$9f,$91,$b6
 db $24,$00,$00
 db $24,$93,$dc,$b0,$a4
 db $24,$01,$01
 db $9c,$08,$91,$b8,$b0,$a4
 db $24,$00,$00
 db $9c,$00,$93,$3f,$a1,$9d
 db $24,$01,$00
 db $24,$92,$93,$90,$a4
 db $24,$01,$01
 db $8c,$06,$a1,$9f,$b1,$b6
 db $24,$00,$00
 db $24,$82,$93,$c1,$9d
 db $24,$01,$00
 db $a0,$04,$b1,$b8,$a0,$a4
 db $24,$00,$00
 db $24,$c1,$9f,$01
 db $24,$00,$00
 db $9c,$00,$82,$26,$a0,$b9
 db $24,$01,$01
 db $24,$82,$e4,$a0,$b9
 db $24,$01,$00
 db $a0,$04,$83,$3f,$01
 db $24,$01,$00
 db $24,$82,$e4,$00
 db $24,$01,$00
 db $8c,$c1,$06,$94,$55,$a2,$4b
 db $24,$00,$a2,$6e
 db $24,$00,$a2,$93
 db $24,$00,$00
 db $9c,$08,$00,$a2,$e4
 db $24,$00,$00
 db $9c,$08,$00,$00
 db $24,$00,$00
 db $a4,$03,$a2,$e4,$a0,$b9
 db $24,$00,$01
 db $24,$00,$a0,$b9
 db $24,$01,$00
 db $9c,$00,$a3,$3f,$01
 db $24,$01,$00
 db $24,$a2,$e4,$a0,$b9
 db $24,$01,$01
 db $8c,$c1,$06,$b4,$55,$a3,$70
 db $24,$00,$00
 db $a4,$03,$b2,$e4,$a0,$b9
 db $24,$01,$00
 db $a0,$c1,$04,$b4,$55,$a3,$10
 db $24,$00,$a3,$3f
 db $24,$00,$00
 db $24,$00,$00
 db $9c,$43,$00,$f1,$6e,$a0,$b9
 db $24,$f1,$9f,$01
 db $24,$f2,$2a,$a0,$b9
 db $24,$f1,$72,$00
 db $94,$10,$e1,$9f,$01
 db $24,$e2,$2a,$00
 db $94,$12,$d1,$72,$00
 db $24,$d1,$9f,$00
 db $8c,$45,$06,$c2,$2a,$a0,$b9
 db $a4,$03,$c1,$72,$00
 db $94,$45,$14,$b1,$9f,$01
 db $a4,$03,$b2,$2a,$00
 db $9c,$45,$08,$a1,$72,$00
 db $24,$a1,$9f,$00
 db $8c,$43,$06,$92,$2a,$00
 db $a4,$05,$91,$72,$00
 db $9c,$43,$16,$81,$9f,$a0,$b9
 db $24,$82,$2a,$01
 db $8c,$06,$91,$72,$a0,$b9
 db $24,$91,$9f,$00
 db $94,$18,$a2,$2a,$01
 db $24,$a1,$72,$00
 db $94,$18,$b1,$9f,$a0,$b9
 db $24,$b2,$2a,$01
 db $8c,$45,$06,$c1,$72,$a0,$b9
 db $a4,$03,$c1,$9f,$00
 db $94,$45,$14,$d2,$2a,$01
 db $a4,$03,$d1,$72,$00
 db $8c,$45,$06,$e1,$9f,$a0,$b9
 db $24,$e2,$2a,$00
 db $8c,$43,$06,$f1,$72,$01
 db $a4,$05,$f1,$9f,$00
 db $9c,$43,$00,$f3,$70,$a0,$92
 db $24,$01,$01
 db $a0,$0e,$f2,$4b,$a0,$92
 db $24,$01,$00
 db $a0,$4d,$04,$f2,$26,$82,$2a
 db $24,$00,$00
 db $a0,$43,$0e,$f2,$4b,$01
 db $24,$01,$00
 db $8c,$4d,$06,$e2,$8f,$92,$93
 db $24,$00,$00
 db $a0,$43,$0e,$e2,$4b,$01
 db $24,$01,$00
 db $9c,$4d,$08,$e2,$26,$a2,$2a
 db $24,$00,$00
 db $9c,$43,$08,$e2,$4b,$01
 db $24,$01,$00
 db $a0,$4d,$04,$d1,$ea,$b1,$ee
 db $24,$00,$00
 db $a0,$43,$0e,$d2,$4b,$c0,$92
 db $24,$01,$00
 db $9c,$4d,$00,$d1,$b4,$c1,$b8
 db $24,$00,$00
 db $a0,$43,$0e,$d2,$4b,$c0,$92
 db $24,$01,$01
 db $8c,$4d,$06,$c1,$9b,$d1,$9f
 db $24,$00,$00
 db $a0,$43,$0e,$c2,$4b,$01
 db $24,$01,$00
 db $a0,$4d,$04,$c1,$b4,$e1,$b8
 db $24,$00,$00
 db $a0,$0e,$c1,$9b,$f1,$9f
 db $24,$00,$00
 db $9c,$43,$00,$b1,$b8,$a0,$a4
 db $24,$00,$01
 db $a0,$0e,$b1,$9f,$a0,$a4
 db $24,$00,$00
 db $a0,$c1,$04,$84,$4d,$84,$55
 db $24,$00,$00
 db $a0,$43,$0e,$b2,$93,$01
 db $24,$01,$00
 db $8c,$c1,$06,$85,$1f,$95,$27
 db $24,$00,$00
 db $a0,$43,$0e,$a4,$55,$01
 db $24,$00,$00
 db $9c,$c1,$08,$84,$4d,$a4,$55
 db $24,$00,$00
 db $9c,$43,$08,$a2,$93,$01
 db $24,$00,$00
 db $a0,$c1,$04,$83,$d4,$b3,$dc
 db $24,$00,$00
 db $9c,$43,$08,$94,$55,$c0,$a4
 db $24,$00,$00
 db $9c,$c1,$00,$83,$68,$c3,$70
 db $24,$00,$00
 db $a0,$43,$0e,$92,$93,$c0,$a4
 db $24,$01,$01
 db $8c,$c1,$06,$83,$37,$d3,$3f
 db $24,$00,$00
 db $a0,$0e,$83,$68,$a3,$70
 db $24,$00,$00
 db $a0,$43,$04,$83,$3f,$e0,$a4
 db $24,$00,$00
 db $a0,$c1,$0e,$83,$37,$f3,$3f
 db $24,$00,$00
 db $9c,$43,$00,$83,$70,$a0,$8a
 db $24,$00,$01
 db $a0,$0e,$83,$3f,$a0,$8a
 db $24,$00,$00
 db $a0,$cb,$04,$83,$3b,$a6,$7e
 db $24,$00,$00
 db $a0,$0e,$83,$6c,$a6,$e1
 db $24,$00,$00
 db $8c,$43,$06,$93,$3f,$b0,$8a
 db $24,$00,$00
 db $a0,$cb,$0e,$93,$3b,$b6,$7e
 db $24,$00,$00
 db $9c,$43,$08,$93,$70,$01
 db $24,$00,$00
 db $9c,$cb,$08,$92,$8f,$b5,$27
 db $24,$00,$00
 db $a0,$43,$04,$a3,$3f,$c0,$8a
 db $24,$01,$01
 db $a0,$0e,$a2,$93,$c0,$8a
 db $24,$00,$00
 db $9c,$cb,$00,$a3,$37,$c3,$3f
 db $24,$00,$00
 db $a0,$0e,$a3,$68,$c3,$70
 db $24,$00,$00
 db $8c,$43,$06,$b3,$3f,$d0,$8a
 db $24,$00,$00
 db $a0,$cb,$0e,$b3,$37,$d3,$3f
 db $24,$00,$00
 db $a0,$43,$04,$b3,$70,$d0,$8a
 db $24,$00,$00
 db $a0,$cb,$0e,$b2,$8b,$d2,$93
 db $24,$00,$00
 db $9c,$43,$00,$f2,$e4,$a0,$b9
 db $24,$e3,$3f,$01
 db $a0,$0e,$d4,$55,$a0,$b9
 db $24,$c2,$e4,$00
 db $a0,$04,$b3,$3f,$01
 db $24,$a4,$55,$00
 db $a0,$0e,$92,$e4,$00
 db $24,$83,$3f,$00
 db $8c,$c1,$06,$d2,$e0,$b2,$4b
 db $24,$00,$b2,$93
 db $24,$00,$00
 db $24,$00,$00
 db $9c,$43,$08,$d3,$3f,$b0,$b9
 db $24,$01,$00
 db $9c,$1a,$d2,$93,$01
 db $24,$00,$00
 db $a0,$c1,$04,$93,$6c,$c2,$e4
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $9c,$43,$08,$93,$3f,$01
 db $24,$01,$00
 db $9c,$1a,$92,$e4,$c0,$b9
 db $24,$00,$01
 db $8c,$c1,$06,$f2,$e4,$d4,$55
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $94,$0a,$00,$d3,$dc
 db $24,$00,$00
 db $94,$0a,$00,$00
 db $24,$00,$00
 db $9c,$43,$00,$f3,$dc,$b0,$92
 db $24,$00,$01
 db $a0,$0e,$f2,$4b,$b0,$92
 db $24,$01,$00
 db $94,$0c,$f2,$e4,$d2,$e4
 db $24,$01,$00
 db $94,$0c,$f2,$4b,$01
 db $24,$01,$00
 db $8c,$06,$e2,$e4,$d2,$93
 db $24,$00,$00
 db $a0,$0e,$e2,$4b,$01
 db $24,$01,$00
 db $9c,$08,$e2,$93,$c2,$e4
 db $24,$00,$00
 db $9c,$08,$e2,$4b,$c2,$93
 db $24,$01,$00
 db $a0,$04,$d2,$e4,$b0,$92
 db $24,$00,$01
 db $a0,$0e,$d2,$93,$b0,$92
 db $24,$00,$00
 db $9c,$00,$d2,$e4,$b3,$70
 db $24,$01,$00
 db $a0,$0e,$d2,$4b,$b0,$92
 db $24,$01,$01
 db $8c,$06,$c3,$70,$b3,$3f
 db $24,$00,$00
 db $a0,$0e,$c2,$4b,$01
 db $24,$01,$00
 db $a0,$04,$c3,$3f,$a3,$70
 db $24,$00,$00
 db $a0,$0e,$c2,$4b,$a3,$3f
 db $24,$01,$00
 db $9c,$00,$b3,$70,$b0,$a4
 db $24,$00,$01
 db $a0,$0e,$b3,$3f,$b0,$a4
 db $24,$00,$00
 db $a0,$04,$00,$b2,$e4
 db $24,$01,$00
 db $a0,$0e,$b2,$93,$01
 db $24,$01,$00
 db $8c,$06,$a2,$e4,$b2,$93
 db $24,$00,$00
 db $a0,$0e,$a2,$93,$01
 db $24,$01,$00
 db $9c,$08,$a2,$93,$c2,$e4
 db $24,$00,$00
 db $9c,$08,$00,$01
 db $24,$01,$00
 db $a0,$04,$92,$e4,$c3,$70
 db $24,$00,$00
 db $9c,$08,$92,$93,$d3,$3f
 db $24,$01,$00
 db $9c,$00,$93,$70,$01
 db $24,$00,$00
 db $a0,$0e,$93,$3f,$d2,$e4
 db $24,$00,$00
 db $8c,$06,$83,$dc,$b0,$a4
 db $24,$01,$00
 db $a0,$0e,$82,$e4,$e2,$93
 db $24,$00,$00
 db $a0,$04,$83,$3f,$b0,$a4
 db $24,$01,$00
 db $a0,$0e,$82,$93,$e2,$e4
 db $24,$00,$00
 db $9c,$00,$82,$26,$b0,$b9
 db $24,$01,$01
 db $a0,$0e,$82,$e4,$b0,$b9
 db $24,$00,$00
 db $a0,$4d,$04,$83,$3f,$a2,$e4
 db $24,$00,$00
 db $a0,$43,$0e,$82,$e4,$01
 db $24,$01,$00
 db $8c,$4d,$06,$94,$55,$b2,$e4
 db $24,$00,$00
 db $a0,$43,$0e,$92,$e4,$00
 db $24,$00,$00
 db $9c,$4d,$08,$93,$3f,$c2,$93
 db $24,$00,$01
 db $9c,$43,$08,$92,$e4,$00
 db $24,$00,$00
 db $a0,$4d,$04,$a2,$2a,$d2,$e4
 db $24,$00,$00
 db $a0,$43,$0e,$a2,$93,$00
 db $24,$00,$00
 db $9c,$4d,$00,$a3,$3f,$e2,$93
 db $24,$00,$01
 db $a0,$43,$0e,$a2,$e4,$e0,$b9
 db $24,$00,$01
 db $8c,$4d,$06,$b4,$55,$f2,$e4
 db $24,$00,$00
 db $a0,$43,$0e,$b2,$93,$00
 db $24,$00,$00
 db $a0,$4d,$04,$b3,$3f,$f2,$93
 db $24,$00,$00
 db $a0,$0e,$b2,$e4,$f2,$e4
 db $24,$00,$00
 db $9c,$43,$00,$c4,$51,$b0,$b9
 db $24,$01,$01
 db $a0,$0e,$c2,$e4,$b0,$b9
 db $24,$01,$00
 db $a0,$cb,$04,$c6,$7a,$b3,$3f
 db $24,$00,$00
 db $a0,$0e,$c6,$dd,$b3,$70
 db $24,$00,$00
 db $8c,$43,$06,$d3,$3f,$b0,$b9
 db $24,$00,$00
 db $a0,$cb,$0e,$d6,$7a,$b3,$3f
 db $24,$00,$00
 db $9c,$43,$08,$d3,$3f,$01
 db $24,$01,$00
 db $8c,$cb,$06,$d5,$23,$b2,$93
 db $24,$00,$00
 db $a0,$43,$04,$e4,$55,$b0,$b9
 db $24,$01,$01
 db $8c,$06,$e2,$93,$b0,$b9
 db $24,$00,$00
 db $9c,$cb,$00,$e6,$7a,$b3,$3f
 db $24,$00,$00
 db $a0,$0e,$e6,$dd,$b3,$70
 db $24,$00,$00
 db $8c,$43,$06,$f3,$3f,$b0,$b9
 db $24,$00,$00
 db $a0,$cb,$0e,$f6,$7a,$b3,$3f
 db $24,$00,$00
 db $9c,$43,$00,$f3,$3f,$b0,$b9
 db $24,$01,$00
 db $9c,$00,$f2,$e4,$01
 db $24,$01,$00
 db $8c,$c1,$06,$80,$b9,$c2,$e0
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $a4,$0d,$a0,$b9,$01
 db $24,$01,$00
 db $24,$a0,$b9,$00
 db $24,$01,$00
 db $9c,$1c,$a0,$b9,$00
 db $24,$01,$00
 db $9c,$1c,$a0,$b9,$00
 db $24,$01,$00
 db $8c,$c1,$1e,$80,$b9,$b2,$e2
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $a4,$0d,$90,$b9,$01
 db $24,$01,$00
 db $24,$90,$b9,$00
 db $24,$01,$00
 db $9c,$20,$90,$b9,$00
 db $24,$01,$00
 db $9c,$1c,$90,$b9,$00
 db $24,$01,$00
 db $8c,$c1,$22,$80,$b9,$92,$e2
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $a4,$0d,$00,$01
 db $24,$01,$00
 db $24,$80,$b9,$00
 db $24,$01,$00
 db $24,$80,$b9,$00
 db $24,$01,$00
 db $24,$80,$b9,$00
 db $24,$01,$00
 db $8c,$24,$80,$b9,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
.loop
 db $24,$01,$00
 db $24,$00,$00
 db $24,$00,$00
 db $24,$00,$00
 db $00
 dw .loop
 align 2
.drumpar
.dp0
 dw .dsmp0+0
 db $02,$09,$40
.dp1
 dw .dsmp1+0
 db $03,$09,$40
.dp2
 dw .dsmp3+0
 db $01,$09,$40
.dp3
 dw .dsmp2+0
 db $06,$09,$40
.dp4
 dw .dsmp4+0
 db $02,$09,$40
.dp5
 dw .dsmp5+0
 db $04,$09,$40
.dp6
 dw .dsmp5+0
 db $04,$03,$40
.dp7
 dw .dsmp3+0
 db $01,$06,$40
.dp8
 dw .dsmp5+0
 db $04,$09,$00
.dp9
 dw .dsmp5+0
 db $04,$09,$08
.dp10
 dw .dsmp5+0
 db $04,$03,$00
.dp11
 dw .dsmp4+0
 db $02,$03,$40
.dp12
 dw .dsmp5+0
 db $04,$09,$10
.dp13
 dw .dsmp4+0
 db $02,$06,$40
.dp14
 dw .dsmp0+0
 db $02,$03,$40
.dp15
 dw .dsmp2+0
 db $06,$06,$40
.dp16
 dw .dsmp0+0
 db $02,$00,$40
.dp17
 dw .dsmp2+0
 db $06,$03,$40
.dp18
 dw .dsmp2+0
 db $06,$00,$40
.dsmp0
 db $00,$00,$00,$00,$00,$00,$00,$00,$01,$07,$f3,$fc,$ff,$ff,$ff,$ff
 db $ff,$e7,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$f3,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
 db $ff,$ff,$ff,$ff,$f8,$c0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp1
 db $3c,$ff,$0e,$20,$00,$00,$00,$00,$01,$01,$0f,$7f,$cf,$bf,$ff,$ff
 db $ff,$ff,$ff,$f8,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$ff,$ff
 db $ff,$ff,$ff,$ff,$ff,$f9,$00,$00,$00,$00,$00,$00,$00,$00,$00,$09
 db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$cc,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$06,$7f,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$f9,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$03,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ef
.dsmp2
 db $00,$00,$00,$00,$00,$dc,$ff,$ff,$ff,$ff,$cf,$98,$00,$00,$00,$00
 db $00,$00,$02,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$00,$00,$00,$00,$00
 db $00,$00,$00,$3f,$ff,$ff,$ff,$ff,$ff,$ff,$98,$00,$00,$00,$00,$00
 db $00,$00,$0f,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$e0,$00,$00,$00,$00
 db $00,$00,$00,$02,$7f,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$cc,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$0b,$ff,$ff,$ff,$ff,$ff,$f7,$e0,$00,$00
 db $00,$00,$00,$00,$00,$3f,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$88,$00
 db $00,$00,$00,$00,$00,$04,$7f,$ff,$ff,$ff,$ff,$ff,$fe,$c0,$80,$00
 db $00,$00,$00,$00,$00,$00,$79,$ff,$ff,$ff,$ff,$ff,$ff,$f1,$00,$00
 db $00,$00,$00,$00,$00,$00,$c0,$ff,$ff,$ff,$ff,$ff,$fe,$c0,$60,$00
 db $00,$00,$00,$00,$00,$00,$3f,$ff,$ff,$ff,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp3
 db $00,$90,$0c,$40,$04,$20,$00,$28,$00,$00,$00,$40,$40,$40,$00,$00
 db $80,$01,$00,$00,$00,$00,$00,$10,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp4
 db $00,$e0,$1f,$01,$fc,$7f,$81,$f0,$00,$00,$00,$1f,$c3,$fc,$0c,$00
 db $00,$00,$00,$f8,$3f,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp5
 db $00,$00,$1c,$01,$fe,$c0,$00,$00,$00,$ff,$c0,$00,$00,$00,$7f,$cf
 db $ff,$fc,$00,$00,$00,$7f,$ff,$82,$00,$00,$00,$ff,$ff,$ff,$e0,$00
 db $00,$03,$ff,$ff,$f8,$00,$00,$07,$e7,$ff,$f8,$00,$00,$00,$0f,$ff
 db $c0,$e0,$00,$00,$ff,$0f,$c7,$00,$00,$e0,$00,$ff,$c0,$00,$00,$00
 db $ff,$f0,$80,$00,$00,$33,$f1,$ff,$fc,$00,$00,$00,$0f,$ff,$c0,$00
 db $00,$00,$7f,$ff,$ff,$80,$00,$00,$03,$ff,$f8,$00,$00,$00,$1f,$ff
 db $ff,$c0,$00,$00,$00,$7f,$fc,$00,$00,$00,$03,$ff,$f8,$e0,$00,$00
 db $0c,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00



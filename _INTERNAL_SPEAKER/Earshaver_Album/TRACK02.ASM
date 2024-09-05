	output	"TRACK02.BIN"

	org $6000
begin:


; 	EARSHAVER
; 	TRACK 2
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





; TRACK02

;compiled music data

music_data
 dw .drumpar
.song
 db $9d,$81,$e1,$ee,$c1,$ee
 db $1d,$00,$00
 db $1d,$b0,$7b,$c0,$7b
 db $1d,$00,$00
 db $1d,$e1,$ee,$c1,$ee
 db $1d,$00,$00
 db $1d,$b1,$ee,$00
 db $1d,$00,$00
 db $1d,$e0,$7b,$c0,$7b
 db $1d,$00,$00
 db $1d,$b1,$ee,$c1,$ee
 db $1d,$00,$00
 db $1d,$e1,$ee,$00
 db $1d,$00,$00
 db $1d,$b0,$7b,$c0,$7b
 db $1d,$00,$00
 db $1d,$e0,$7b,$00
 db $1d,$00,$00
 db $1d,$b1,$ee,$c1,$ee
 db $1d,$00,$00
 db $1d,$e1,$ee,$00
 db $1d,$00,$00
 db $1d,$b1,$ee,$00
 db $1d,$00,$00
 db $1d,$e0,$7b,$c0,$7b
 db $1d,$00,$00
 db $1d,$b1,$ee,$c1,$ee
 db $1d,$00,$00
 db $1d,$e1,$ee,$00
 db $1d,$00,$00
 db $1d,$b0,$7b,$c0,$7b
 db $1d,$00,$00
 db $1d,$e1,$ee,$a1,$ee
 db $1d,$00,$00
 db $1d,$b0,$7b,$a0,$7b
 db $1d,$00,$00
 db $1d,$e1,$ee,$a1,$ee
 db $1d,$00,$00
 db $1d,$b1,$ee,$00
 db $1d,$00,$00
 db $1d,$e0,$7b,$a0,$7b
 db $1d,$00,$00
 db $1d,$b1,$ee,$a1,$ee
 db $1d,$00,$00
 db $1d,$e1,$ee,$00
 db $1d,$00,$00
 db $1d,$b0,$7b,$a0,$7b
 db $1d,$00,$00
 db $1d,$e0,$7b,$00
 db $1d,$00,$00
 db $1d,$b1,$ee,$a1,$ee
 db $1d,$00,$00
 db $1d,$e1,$ee,$00
 db $1d,$00,$00
 db $1d,$b1,$ee,$00
 db $1d,$00,$00
 db $1d,$e0,$7b,$a0,$7b
 db $1d,$00,$00
 db $1d,$b1,$ee,$a1,$ee
 db $1d,$00,$00
 db $1d,$e1,$ee,$00
 db $1d,$00,$00
 db $1d,$b0,$7b,$a0,$7b
 db $1d,$00,$00
 db $1d,$e1,$88,$91,$88
 db $1d,$00,$00
 db $1d,$b0,$62,$90,$62
 db $1d,$00,$00
 db $1d,$e1,$88,$91,$88
 db $1d,$00,$00
 db $1d,$b1,$88,$00
 db $1d,$00,$00
 db $1d,$e0,$62,$90,$62
 db $1d,$00,$00
 db $1d,$b1,$88,$91,$88
 db $1d,$00,$00
 db $1d,$e1,$88,$00
 db $1d,$00,$00
 db $1d,$b0,$62,$90,$62
 db $1d,$00,$00
 db $1d,$e0,$62,$00
 db $1d,$00,$00
 db $1d,$b1,$88,$91,$88
 db $1d,$00,$00
 db $1d,$e1,$88,$00
 db $1d,$00,$00
 db $1d,$b1,$88,$00
 db $1d,$00,$00
 db $1d,$e0,$62,$90,$62
 db $1d,$00,$00
 db $1d,$b1,$88,$91,$88
 db $1d,$00,$00
 db $1d,$e1,$88,$00
 db $1d,$00,$00
 db $1d,$b0,$62,$90,$62
 db $1d,$00,$00
 db $1d,$e1,$49,$81,$49
 db $1d,$00,$00
 db $1d,$b0,$52,$80,$52
 db $1d,$00,$00
 db $1d,$e1,$49,$81,$49
 db $1d,$00,$00
 db $1d,$b1,$49,$00
 db $1d,$00,$00
 db $1d,$e0,$52,$80,$52
 db $1d,$00,$00
 db $1d,$b1,$49,$81,$49
 db $1d,$00,$00
 db $1d,$e1,$49,$91,$49
 db $1d,$00,$00
 db $1d,$b0,$52,$80,$52
 db $1d,$00,$00
 db $1d,$e0,$52,$00
 db $1d,$00,$00
 db $1d,$b1,$49,$81,$49
 db $1d,$00,$00
 db $1d,$e1,$49,$00
 db $1d,$00,$00
 db $1d,$b1,$49,$91,$49
 db $1d,$00,$00
 db $1d,$e0,$52,$80,$52
 db $1d,$00,$00
 db $1d,$b1,$49,$81,$49
 db $1d,$00,$00
 db $1d,$e1,$49,$00
 db $1d,$00,$00
 db $1d,$b0,$52,$80,$52
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$c1,$ee
 db $1d,$00,$00
 db $1d,$b0,$7b,$c0,$7b
 db $1d,$00,$00
 db $1d,$e1,$ee,$c1,$ee
 db $1d,$00,$00
 db $1d,$b1,$ee,$00
 db $1d,$00,$00
 db $85,$02,$e0,$7b,$c0,$7b
 db $1d,$00,$00
 db $1d,$b1,$ee,$c1,$ee
 db $1d,$00,$00
 db $1d,$e1,$ee,$00
 db $1d,$00,$00
 db $95,$00,$b0,$7b,$c0,$7b
 db $1d,$00,$00
 db $95,$00,$e0,$7b,$00
 db $1d,$00,$00
 db $1d,$b1,$ee,$c1,$ee
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$00
 db $1d,$00,$00
 db $1d,$b1,$ee,$00
 db $1d,$00,$00
 db $85,$02,$e0,$7b,$c0,$7b
 db $1d,$00,$00
 db $1d,$b1,$ee,$c1,$ee
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$00
 db $1d,$00,$00
 db $1d,$b0,$7b,$c0,$7b
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$a1,$ee
 db $1d,$00,$00
 db $1d,$b0,$7b,$a0,$7b
 db $1d,$00,$00
 db $1d,$e1,$ee,$a1,$ee
 db $1d,$00,$00
 db $1d,$b1,$ee,$00
 db $1d,$00,$00
 db $85,$02,$e0,$7b,$a0,$7b
 db $1d,$00,$00
 db $1d,$b1,$ee,$a1,$ee
 db $1d,$00,$00
 db $1d,$e1,$ee,$00
 db $1d,$00,$00
 db $95,$00,$b0,$7b,$a0,$7b
 db $1d,$00,$00
 db $95,$00,$e0,$7b,$00
 db $1d,$00,$00
 db $1d,$b1,$ee,$a1,$ee
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$00
 db $1d,$00,$00
 db $1d,$b1,$ee,$00
 db $1d,$00,$00
 db $85,$02,$e0,$7b,$a0,$7b
 db $1d,$00,$00
 db $1d,$b1,$ee,$a1,$ee
 db $1d,$00,$00
 db $1d,$e1,$ee,$00
 db $1d,$00,$00
 db $95,$00,$b0,$7b,$a0,$7b
 db $1d,$00,$00
 db $95,$00,$e1,$88,$91,$88
 db $1d,$00,$00
 db $1d,$b0,$62,$90,$62
 db $1d,$00,$00
 db $1d,$e1,$88,$91,$88
 db $1d,$00,$00
 db $1d,$b1,$88,$00
 db $1d,$00,$00
 db $85,$02,$e0,$62,$90,$62
 db $1d,$00,$00
 db $1d,$b1,$88,$91,$88
 db $1d,$00,$00
 db $1d,$e1,$88,$00
 db $1d,$00,$00
 db $85,$02,$b0,$62,$90,$62
 db $1d,$00,$00
 db $95,$00,$e0,$62,$00
 db $1d,$00,$00
 db $1d,$b1,$88,$91,$88
 db $1d,$00,$00
 db $95,$00,$e1,$88,$00
 db $1d,$00,$00
 db $1d,$b1,$88,$00
 db $1d,$00,$00
 db $85,$02,$e0,$62,$90,$62
 db $1d,$00,$00
 db $1d,$b1,$88,$91,$88
 db $1d,$00,$00
 db $95,$00,$e1,$88,$00
 db $1d,$00,$00
 db $95,$00,$b0,$62,$90,$62
 db $1d,$00,$00
 db $95,$00,$e1,$49,$81,$49
 db $1d,$00,$00
 db $1d,$b0,$52,$80,$52
 db $1d,$00,$00
 db $1d,$e1,$49,$81,$49
 db $1d,$00,$00
 db $1d,$b1,$49,$91,$49
 db $1d,$00,$00
 db $85,$02,$e0,$52,$80,$52
 db $1d,$00,$00
 db $1d,$b1,$49,$81,$49
 db $1d,$00,$00
 db $1d,$e1,$49,$00
 db $1d,$00,$00
 db $85,$02,$b0,$52,$80,$52
 db $1d,$00,$00
 db $95,$00,$e0,$52,$00
 db $1d,$00,$00
 db $1d,$b1,$49,$81,$49
 db $1d,$00,$00
 db $95,$00,$e1,$49,$00
 db $1d,$00,$00
 db $1d,$b1,$49,$00
 db $1d,$00,$00
 db $85,$02,$e1,$49,$b1,$ee
 db $1d,$00,$00
 db $85,$02,$00,$00
 db $1d,$00,$00
 db $85,$02,$00,$00
 db $1d,$00,$00
 db $85,$02,$00,$00
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $1d,$b0,$7b,$b0,$f3
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $1d,$b1,$ee,$b4,$55
 db $1d,$00,$00
 db $85,$02,$e0,$7b,$e0,$f3
 db $1d,$00,$00
 db $1d,$b1,$ee,$b4,$55
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$e4,$97
 db $1d,$00,$00
 db $95,$00,$b0,$7b,$b0,$f3
 db $1d,$00,$00
 db $95,$00,$e0,$7b,$e0,$f7
 db $1d,$00,$00
 db $1d,$b1,$ee,$b4,$97
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e4,$97
 db $1d,$00,$00
 db $1d,$b1,$ee,$b4,$55
 db $1d,$00,$00
 db $85,$02,$e0,$7b,$e0,$f3
 db $1d,$00,$00
 db $1d,$b1,$ee,$b4,$55
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e4,$97
 db $1d,$00,$00
 db $1d,$b0,$7b,$b0,$f3
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e4,$97
 db $1d,$00,$00
 db $1d,$b0,$7b,$b0,$f3
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$e4,$97
 db $1d,$00,$00
 db $1d,$b1,$ee,$b4,$55
 db $1d,$00,$00
 db $85,$02,$e0,$7b,$e0,$f3
 db $1d,$00,$00
 db $1d,$b1,$ee,$b4,$55
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$e4,$97
 db $1d,$00,$00
 db $95,$00,$b0,$7b,$b0,$f3
 db $1d,$00,$00
 db $95,$00,$e0,$7b,$e0,$f7
 db $1d,$00,$00
 db $1d,$b1,$ee,$b4,$97
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e4,$97
 db $1d,$00,$00
 db $1d,$b1,$ee,$b4,$55
 db $1d,$00,$00
 db $85,$02,$e0,$7b,$e0,$f3
 db $1d,$00,$00
 db $1d,$b1,$ee,$b4,$55
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$e4,$97
 db $1d,$00,$00
 db $95,$00,$b0,$7b,$b0,$f3
 db $1d,$00,$00
 db $95,$00,$e1,$88,$e1,$ee
 db $1d,$00,$00
 db $1d,$b0,$62,$b0,$c0
 db $1d,$00,$00
 db $95,$04,$e1,$88,$e1,$ee
 db $1d,$00,$00
 db $1d,$b1,$88,$b1,$b8
 db $1d,$00,$00
 db $85,$02,$e0,$62,$e0,$c0
 db $1d,$00,$00
 db $1d,$b1,$88,$b1,$b8
 db $1d,$00,$00
 db $95,$04,$e1,$88,$e1,$ee
 db $1d,$00,$00
 db $85,$02,$b0,$62,$b0,$c0
 db $1d,$00,$00
 db $95,$00,$e0,$62,$e0,$c4
 db $1d,$00,$00
 db $1d,$b1,$88,$b1,$ee
 db $1d,$00,$00
 db $95,$00,$e1,$88,$e1,$ee
 db $1d,$00,$00
 db $1d,$b1,$88,$b1,$b8
 db $1d,$00,$00
 db $85,$02,$e0,$62,$e0,$c0
 db $1d,$00,$00
 db $1d,$b1,$88,$b1,$b8
 db $1d,$00,$00
 db $95,$00,$e1,$88,$e1,$ee
 db $1d,$00,$00
 db $95,$00,$b0,$62,$b0,$c0
 db $1d,$00,$00
 db $95,$00,$e1,$49,$e1,$88
 db $1d,$00,$00
 db $1d,$b0,$52,$b0,$a0
 db $1d,$00,$00
 db $95,$04,$e1,$49,$e1,$88
 db $1d,$00,$00
 db $1d,$b1,$49,$b1,$72
 db $1d,$00,$00
 db $85,$02,$e0,$52,$e0,$a0
 db $1d,$00,$00
 db $1d,$b1,$49,$b1,$72
 db $1d,$00,$00
 db $95,$04,$e1,$49,$e1,$88
 db $1d,$00,$00
 db $85,$02,$b0,$52,$b0,$a0
 db $1d,$00,$00
 db $95,$00,$e0,$52,$e0,$a4
 db $1d,$00,$00
 db $85,$02,$b1,$49,$b1,$88
 db $1d,$00,$00
 db $85,$02,$e1,$49,$e1,$88
 db $1d,$00,$00
 db $85,$02,$b1,$49,$b1,$72
 db $1d,$00,$00
 db $95,$00,$e0,$52,$e0,$a0
 db $1d,$00,$00
 db $85,$02,$b1,$49,$b1,$72
 db $1d,$00,$00
 db $85,$02,$e1,$49,$e1,$88
 db $1d,$00,$00
 db $95,$00,$b0,$52,$b0,$a0
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $1d,$b0,$7b,$b0,$f3
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $1d,$b1,$ee,$b2,$2a
 db $1d,$00,$00
 db $85,$02,$e0,$7b,$e0,$f3
 db $1d,$00,$00
 db $1d,$b1,$ee,$b2,$2a
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $95,$00,$b0,$7b,$b0,$f3
 db $1d,$00,$00
 db $95,$00,$e0,$7b,$e0,$f7
 db $1d,$00,$00
 db $1d,$b1,$ee,$b2,$4b
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $1d,$b1,$ee,$b2,$2a
 db $1d,$00,$00
 db $85,$02,$e0,$7b,$e0,$f3
 db $1d,$00,$00
 db $1d,$b1,$ee,$b2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $1d,$b0,$7b,$b0,$f3
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $1d,$b0,$7b,$b0,$f3
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $1d,$b1,$ee,$b2,$2a
 db $1d,$00,$00
 db $85,$02,$e0,$7b,$e0,$f3
 db $1d,$00,$00
 db $1d,$b1,$ee,$b2,$2a
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $95,$00,$b0,$7b,$b0,$f3
 db $1d,$00,$00
 db $95,$00,$e0,$7b,$e0,$f7
 db $1d,$00,$00
 db $1d,$b1,$ee,$b2,$4b
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $1d,$b1,$ee,$b2,$2a
 db $1d,$00,$00
 db $85,$02,$e0,$7b,$e0,$f3
 db $1d,$00,$00
 db $1d,$b1,$ee,$b2,$2a
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $95,$00,$b0,$7b,$b0,$f3
 db $1d,$00,$00
 db $95,$00,$e1,$88,$e1,$ee
 db $1d,$00,$00
 db $1d,$b0,$62,$b0,$c0
 db $1d,$00,$00
 db $95,$04,$e1,$88,$e1,$ee
 db $1d,$00,$00
 db $1d,$b1,$88,$b1,$b8
 db $1d,$00,$00
 db $85,$02,$e0,$62,$e0,$c0
 db $1d,$00,$00
 db $1d,$b1,$88,$b1,$b8
 db $1d,$00,$00
 db $95,$04,$e1,$88,$e1,$ee
 db $1d,$00,$00
 db $85,$02,$b0,$62,$b0,$c0
 db $1d,$00,$00
 db $95,$00,$e0,$62,$e0,$c4
 db $1d,$00,$00
 db $1d,$b1,$88,$b1,$ee
 db $1d,$00,$00
 db $95,$00,$e1,$88,$e1,$ee
 db $1d,$00,$00
 db $1d,$b1,$88,$b1,$b8
 db $1d,$00,$00
 db $85,$02,$e0,$62,$e0,$c0
 db $1d,$00,$00
 db $95,$04,$b1,$88,$b1,$b8
 db $1d,$00,$00
 db $95,$00,$e1,$88,$e1,$ee
 db $1d,$00,$00
 db $95,$00,$b0,$62,$b0,$c0
 db $1d,$00,$00
 db $95,$00,$e1,$49,$e1,$88
 db $1d,$00,$00
 db $1d,$b0,$52,$b0,$a0
 db $1d,$00,$00
 db $95,$04,$e1,$49,$e1,$88
 db $1d,$00,$00
 db $1d,$b1,$49,$b1,$72
 db $1d,$00,$00
 db $85,$02,$e0,$52,$e0,$a0
 db $1d,$00,$00
 db $95,$00,$b1,$49,$b1,$72
 db $1d,$00,$00
 db $1d,$e1,$49,$e1,$88
 db $1d,$00,$00
 db $85,$02,$b0,$52,$b0,$a0
 db $1d,$00,$00
 db $85,$02,$e1,$49,$90,$a4
 db $1d,$00,$00
 db $9d,$03,$e3,$10,$e1,$88
 db $1d,$00,$00
 db $1d,$d3,$10,$00
 db $1d,$d1,$88,$00
 db $1d,$c2,$e4,$e1,$72
 db $1d,$c1,$88,$00
 db $9d,$81,$e1,$49,$90,$a4
 db $1d,$00,$00
 db $9d,$03,$82,$e4,$e1,$72
 db $1d,$81,$49,$00
 db $1d,$93,$10,$e1,$88
 db $1d,$91,$72,$00
 db $9d,$81,$b1,$49,$90,$a4
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e1,$72
 db $1d,$00,$00
 db $1d,$b1,$ee,$e2,$2a
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$e1,$72
 db $1d,$00,$00
 db $8d,$06,$e0,$7b,$e0,$f7
 db $1d,$00,$00
 db $1d,$b0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$e0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $1d,$b1,$ee,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e1,$72
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$e0,$f7
 db $1d,$00,$00
 db $8d,$06,$e0,$7b,$00
 db $1d,$00,$00
 db $1d,$b0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$e0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$00
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e2,$e4
 db $1d,$00,$00
 db $1d,$b0,$7b,$e0,$f7
 db $1d,$00,$00
 db $95,$04,$e0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$e2,$e4
 db $1d,$00,$00
 db $8d,$06,$e0,$7b,$e0,$f7
 db $1d,$00,$00
 db $1d,$b0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$e2,$e4
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$e0,$f7
 db $1d,$00,$00
 db $95,$00,$e0,$7b,$00
 db $1d,$00,$00
 db $1d,$b1,$ee,$e2,$e4
 db $1d,$00,$00
 db $95,$00,$e0,$7b,$e0,$f7
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$00
 db $1d,$00,$00
 db $8d,$06,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $95,$04,$e0,$7b,$e0,$f7
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$00
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e1,$72
 db $1d,$00,$00
 db $1d,$b1,$ee,$e2,$2a
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$e1,$72
 db $1d,$00,$00
 db $8d,$06,$e0,$7b,$e0,$f7
 db $1d,$00,$00
 db $1d,$b0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$e0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $1d,$b1,$ee,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e1,$72
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$e0,$f7
 db $1d,$00,$00
 db $8d,$06,$e0,$7b,$00
 db $1d,$00,$00
 db $1d,$b0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$e0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$00
 db $1d,$00,$00
 db $95,$00,$e2,$4b,$e2,$e4
 db $1d,$00,$00
 db $1d,$b1,$ee,$00
 db $1d,$00,$00
 db $95,$04,$e0,$7b,$e0,$f7
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$00
 db $1d,$00,$00
 db $8d,$06,$e2,$4b,$e2,$e4
 db $1d,$00,$00
 db $1d,$b0,$7b,$e0,$f7
 db $1d,$00,$00
 db $95,$04,$e2,$4b,$e2,$e4
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$00
 db $1d,$00,$00
 db $95,$00,$e0,$7b,$e0,$f7
 db $1d,$00,$00
 db $1d,$b2,$e4,$e1,$ee
 db $1d,$00,$00
 db $95,$00,$e2,$e4,$00
 db $1d,$00,$00
 db $95,$04,$b2,$e4,$00
 db $1d,$00,$00
 db $8d,$06,$e2,$e4,$e2,$2a
 db $1d,$00,$00
 db $1d,$b2,$e4,$00
 db $1d,$00,$00
 db $95,$04,$e2,$e4,$e2,$4b
 db $1d,$00,$00
 db $95,$04,$b2,$e4,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$88,$e1,$ee
 db $1d,$00,$00
 db $1d,$b1,$88,$e2,$2a
 db $1d,$00,$00
 db $95,$04,$e1,$88,$e2,$4b
 db $1d,$00,$00
 db $95,$04,$b1,$88,$e1,$ee
 db $1d,$00,$00
 db $8d,$06,$e0,$62,$e0,$c4
 db $1d,$00,$00
 db $1d,$b0,$62,$00
 db $1d,$00,$00
 db $95,$04,$e0,$62,$00
 db $1d,$00,$00
 db $95,$04,$b1,$88,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$88,$e2,$4b
 db $1d,$00,$00
 db $1d,$b1,$88,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$88,$e1,$ee
 db $1d,$00,$00
 db $95,$04,$90,$62,$e0,$c4
 db $1d,$00,$00
 db $8d,$06,$e0,$62,$00
 db $1d,$00,$00
 db $1d,$b0,$62,$00
 db $1d,$00,$00
 db $95,$04,$e0,$62,$00
 db $1d,$00,$00
 db $95,$04,$b0,$62,$00
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e2,$e4
 db $1d,$00,$00
 db $1d,$b0,$62,$e0,$c0
 db $1d,$00,$00
 db $95,$04,$e0,$62,$00
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$e2,$e4
 db $1d,$00,$00
 db $8d,$06,$e0,$62,$e0,$c0
 db $1d,$00,$00
 db $1d,$b0,$62,$00
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$e2,$e4
 db $1d,$00,$00
 db $95,$04,$b0,$62,$e0,$c0
 db $1d,$00,$00
 db $95,$00,$e0,$62,$00
 db $1d,$00,$00
 db $1d,$b1,$ee,$e2,$e4
 db $1d,$00,$00
 db $95,$00,$e0,$62,$e0,$c0
 db $1d,$00,$00
 db $95,$04,$b0,$62,$00
 db $1d,$00,$00
 db $8d,$06,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $95,$04,$e0,$62,$e0,$c0
 db $1d,$00,$00
 db $95,$04,$b0,$62,$00
 db $1d,$00,$00
 db $95,$00,$e2,$93,$e2,$e4
 db $1d,$00,$00
 db $1d,$b1,$ee,$00
 db $1d,$00,$00
 db $95,$04,$e0,$52,$e0,$a0
 db $1d,$00,$00
 db $95,$04,$b0,$52,$00
 db $1d,$00,$00
 db $8d,$06,$e2,$93,$e2,$e4
 db $1d,$00,$00
 db $1d,$b0,$52,$e0,$a0
 db $1d,$00,$00
 db $95,$04,$e2,$93,$e2,$e4
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$00
 db $1d,$00,$00
 db $95,$00,$e0,$52,$e0,$a0
 db $1d,$00,$00
 db $1d,$b2,$93,$e1,$ee
 db $1d,$00,$00
 db $95,$00,$e2,$93,$00
 db $1d,$00,$00
 db $95,$04,$b2,$93,$00
 db $1d,$00,$00
 db $8d,$06,$e2,$93,$e2,$2a
 db $1d,$00,$00
 db $1d,$b2,$93,$00
 db $1d,$00,$00
 db $95,$04,$e2,$93,$e2,$4b
 db $1d,$00,$00
 db $95,$04,$b2,$93,$e2,$2a
 db $1d,$00,$00
 db $8d,$06,$e2,$e4,$e3,$70
 db $1d,$00,$00
 db $8d,$06,$b2,$e4,$e1,$ee
 db $1d,$00,$00
 db $95,$04,$e0,$5c,$e0,$b5
 db $1d,$00,$00
 db $95,$04,$b0,$5c,$00
 db $1d,$00,$00
 db $8d,$06,$e2,$e4,$e3,$70
 db $1d,$00,$00
 db $1d,$b0,$5c,$e0,$b5
 db $1d,$00,$00
 db $8d,$06,$e2,$e4,$e3,$70
 db $1d,$00,$00
 db $8d,$06,$b2,$e4,$e1,$ee
 db $1d,$00,$00
 db $95,$00,$e0,$5c,$e0,$b5
 db $1d,$00,$00
 db $85,$08,$b2,$e4,$e3,$dc
 db $1d,$00,$00
 db $85,$0a,$e2,$e4,$00
 db $1d,$00,$00
 db $85,$08,$b2,$e4,$00
 db $1d,$00,$00
 db $85,$0a,$e2,$e4,$e4,$55
 db $1d,$00,$00
 db $85,$08,$b2,$e4,$00
 db $1d,$00,$00
 db $85,$0a,$e2,$e4,$e4,$97
 db $1d,$00,$00
 db $85,$0a,$b2,$e4,$e4,$55
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e1,$72
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$e2,$2a
 db $1d,$00,$00
 db $95,$0c,$e1,$ee,$e2,$4b
 db $1d,$ef,$6e,$00
 db $95,$04,$b1,$ee,$e1,$72
 db $1d,$00,$00
 db $85,$0a,$e0,$7b,$e0,$f7
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$00
 db $1d,$00,$00
 db $99,$0e,$e0,$f3,$00
 db $1d,$ef,$6e,$00
 db $95,$0c,$b1,$ee,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $95,$0c,$b1,$ee,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e1,$72
 db $1d,$ef,$6e,$00
 db $95,$04,$b0,$f3,$e0,$f7
 db $1d,$00,$00
 db $85,$0a,$e0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$00
 db $1d,$00,$00
 db $99,$0e,$e0,$f3,$00
 db $1d,$ef,$6e,$00
 db $95,$04,$b0,$f3,$00
 db $1d,$00,$00
 db $95,$00,$e2,$4b,$e2,$e4
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$e0,$f7
 db $1d,$00,$00
 db $95,$0c,$e0,$f3,$00
 db $1d,$ef,$6e,$00
 db $95,$04,$b2,$4b,$e2,$e4
 db $1d,$00,$00
 db $85,$0a,$e0,$7b,$e0,$f7
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$00
 db $1d,$00,$00
 db $99,$0e,$e2,$4b,$e2,$e4
 db $1d,$ef,$6e,$00
 db $95,$0c,$b0,$f3,$e0,$f7
 db $1d,$00,$00
 db $95,$00,$e0,$7b,$00
 db $1d,$00,$00
 db $95,$0c,$b2,$4b,$e2,$e4
 db $1d,$00,$00
 db $95,$00,$e0,$f3,$e0,$f7
 db $1d,$ef,$6e,$00
 db $95,$04,$b0,$f3,$00
 db $1d,$00,$00
 db $85,$0a,$e3,$dc,$e2,$4b
 db $1d,$00,$00
 db $95,$04,$00,$00
 db $1d,$00,$00
 db $99,$0e,$e0,$f3,$e0,$f7
 db $1d,$ef,$6e,$00
 db $95,$04,$b0,$f3,$00
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e1,$72
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$e2,$2a
 db $1d,$00,$00
 db $95,$0c,$e1,$ee,$e2,$4b
 db $1d,$ef,$6e,$00
 db $95,$04,$b1,$ee,$e1,$72
 db $1d,$00,$00
 db $85,$0a,$e0,$7b,$e0,$f7
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$00
 db $1d,$00,$00
 db $99,$0e,$e0,$f3,$00
 db $1d,$ef,$6e,$00
 db $95,$0c,$b1,$ee,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $95,$0c,$b1,$ee,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e1,$72
 db $1d,$ef,$6e,$00
 db $95,$04,$b0,$f3,$e0,$f7
 db $1d,$00,$00
 db $85,$0a,$e0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$00
 db $1d,$00,$00
 db $99,$0e,$e0,$f3,$00
 db $1d,$ef,$6e,$00
 db $95,$04,$b0,$f3,$00
 db $1d,$00,$00
 db $95,$00,$e2,$4b,$e1,$b8
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$e3,$70
 db $1d,$00,$00
 db $95,$0c,$e0,$f3,$e0,$f7
 db $1d,$ef,$6e,$00
 db $95,$04,$b0,$f3,$00
 db $1d,$00,$00
 db $85,$0a,$e2,$4b,$e3,$70
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$e0,$f7
 db $1d,$00,$00
 db $99,$0e,$e2,$4b,$e1,$b8
 db $1d,$ef,$6e,$00
 db $95,$0c,$b1,$ee,$e3,$70
 db $1d,$00,$00
 db $95,$00,$e0,$7b,$e0,$f7
 db $1d,$00,$00
 db $95,$0c,$b2,$e4,$e1,$b6
 db $1d,$00,$00
 db $95,$00,$e2,$e4,$00
 db $1d,$00,$00
 db $95,$04,$b2,$e4,$00
 db $1d,$00,$00
 db $85,$0a,$e2,$e4,$e1,$ec
 db $1d,$00,$00
 db $95,$04,$b2,$e4,$00
 db $1d,$00,$00
 db $99,$0e,$e2,$e4,$e2,$26
 db $1d,$ef,$6e,$00
 db $95,$04,$b2,$e4,$e1,$ea
 db $1d,$00,$00
 db $95,$00,$e1,$88,$e1,$ee
 db $1d,$00,$00
 db $95,$04,$b1,$88,$e2,$2a
 db $1d,$00,$00
 db $95,$0c,$e1,$88,$e2,$4b
 db $1d,$ec,$3e,$00
 db $95,$04,$b1,$88,$e1,$ee
 db $1d,$00,$00
 db $85,$0a,$e0,$62,$e0,$c4
 db $1d,$00,$00
 db $95,$04,$b0,$62,$00
 db $1d,$00,$00
 db $99,$0e,$e0,$c0,$00
 db $1d,$ec,$3e,$00
 db $95,$0c,$b1,$88,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$88,$e2,$4b
 db $1d,$00,$00
 db $95,$0c,$b1,$88,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$88,$e1,$ee
 db $1d,$ec,$3e,$00
 db $95,$04,$90,$c0,$e0,$c4
 db $1d,$00,$00
 db $85,$0a,$e0,$62,$00
 db $1d,$00,$00
 db $95,$04,$b0,$62,$00
 db $1d,$00,$00
 db $99,$0e,$e0,$c0,$00
 db $1d,$ec,$3e,$00
 db $95,$04,$b0,$c0,$00
 db $1d,$00,$00
 db $95,$00,$e2,$4b,$e2,$e4
 db $1d,$00,$00
 db $95,$04,$b0,$62,$e0,$c0
 db $1d,$00,$00
 db $95,$0c,$e0,$c4,$00
 db $1d,$ec,$3e,$00
 db $95,$04,$b2,$4b,$e2,$e4
 db $1d,$00,$00
 db $85,$0a,$e0,$62,$e0,$c0
 db $1d,$00,$00
 db $95,$04,$b0,$62,$00
 db $1d,$00,$00
 db $99,$0e,$e2,$4b,$e2,$e4
 db $1d,$ec,$3e,$00
 db $95,$0c,$b0,$c4,$e0,$c0
 db $1d,$00,$00
 db $95,$00,$e0,$62,$00
 db $1d,$00,$00
 db $95,$0c,$b2,$4b,$e2,$e4
 db $1d,$00,$00
 db $95,$00,$e0,$c4,$e0,$c0
 db $1d,$ec,$3e,$00
 db $95,$04,$b0,$c4,$00
 db $1d,$00,$00
 db $85,$0a,$e3,$0c,$e2,$4b
 db $1d,$00,$00
 db $95,$04,$00,$00
 db $1d,$00,$00
 db $99,$0e,$e0,$c4,$e0,$c0
 db $1d,$ec,$3e,$00
 db $95,$04,$b0,$c4,$00
 db $1d,$00,$00
 db $95,$00,$e2,$93,$e2,$e4
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$00
 db $1d,$00,$00
 db $95,$0c,$e0,$a4,$e0,$a0
 db $1d,$ea,$4b,$00
 db $95,$04,$b0,$a4,$00
 db $1d,$00,$00
 db $85,$0a,$e2,$93,$e2,$e4
 db $1d,$00,$00
 db $95,$04,$b0,$52,$e0,$a0
 db $1d,$00,$00
 db $99,$0e,$e2,$93,$e2,$e4
 db $1d,$ea,$4b,$00
 db $95,$0c,$b1,$ee,$00
 db $1d,$00,$00
 db $95,$00,$e0,$52,$e0,$a0
 db $1d,$00,$00
 db $95,$0c,$b2,$93,$e1,$ee
 db $1d,$00,$00
 db $95,$00,$e2,$93,$00
 db $1d,$ea,$4b,$00
 db $95,$04,$b2,$93,$00
 db $1d,$00,$00
 db $85,$0a,$e2,$93,$e2,$2a
 db $1d,$00,$00
 db $95,$04,$b2,$93,$00
 db $1d,$00,$00
 db $85,$0a,$e2,$93,$e2,$4b
 db $1d,$ea,$4b,$00
 db $85,$0a,$b2,$93,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e2,$e4,$e3,$70
 db $1d,$00,$00
 db $95,$10,$b2,$e4,$e1,$ee
 db $1d,$00,$00
 db $99,$12,$e0,$b9,$e0,$b5
 db $1d,$00,$00
 db $95,$14,$b0,$b9,$00
 db $1d,$00,$00
 db $99,$16,$e2,$e4,$e3,$70
 db $1d,$00,$00
 db $95,$14,$b0,$5c,$e0,$b5
 db $1d,$00,$00
 db $99,$18,$e2,$e4,$e3,$70
 db $1d,$00,$00
 db $95,$1a,$b2,$e4,$e1,$ee
 db $1d,$00,$00
 db $99,$1c,$e0,$5c,$e0,$b5
 db $1d,$00,$00
 db $85,$0a,$b2,$e4,$e1,$ee
 db $1d,$00,$00
 db $85,$0a,$e2,$e4,$00
 db $1d,$00,$00
 db $85,$0a,$b2,$e4,$00
 db $1d,$00,$00
 db $85,$0a,$e2,$e4,$e2,$2a
 db $1d,$00,$00
 db $85,$0a,$b2,$e4,$00
 db $1d,$00,$00
 db $85,$0a,$e2,$e4,$e2,$4b
 db $1d,$00,$00
 db $85,$0a,$b2,$e4,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$b1,$72,$e1,$ee
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$e1,$d2
 db $1d,$00,$e1,$b8
 db $1d,$00,$e1,$9f
 db $1d,$00,$e1,$88
 db $1d,$00,$e1,$72
 db $1d,$00,$e1,$5d
 db $1d,$00,$e1,$49
 db $95,$1e,$00,$e1,$25
 db $1d,$01,$01
 db $95,$20,$b1,$72,$e1,$49
 db $1d,$01,$01
 db $95,$1a,$b1,$72,$e1,$5d
 db $1d,$01,$01
 db $95,$22,$b1,$72,$e1,$88
 db $1d,$01,$01
 db $99,$24,$b1,$72,$e1,$9f
 db $1d,$01,$01
 db $99,$12,$b1,$72,$e1,$b8
 db $1d,$01,$01
 db $99,$26,$b1,$72,$e1,$ee
 db $1d,$01,$01
 db $99,$28,$b1,$72,$e1,$ee
 db $1d,$01,$01
 db $95,$00,$e1,$ee,$c3,$70
 db $1d,$00,$00
 db $1d,$b0,$7b,$c0,$7b
 db $1d,$00,$00
 db $9d,$03,$e3,$70,$c1,$ee
 db $1d,$00,$00
 db $9d,$81,$b1,$ee,$c2,$4b
 db $1d,$00,$00
 db $1d,$e0,$7b,$c0,$7b
 db $1d,$00,$00
 db $9d,$03,$b2,$4b,$c1,$ee
 db $1d,$00,$00
 db $9d,$81,$e1,$ee,$c2,$4b
 db $1d,$00,$00
 db $1d,$b0,$7b,$c0,$7b
 db $1d,$00,$00
 db $9d,$03,$e2,$4b,$00
 db $1d,$00,$00
 db $9d,$81,$b1,$ee,$c1,$ee
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$00
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$00
 db $1d,$00,$00
 db $99,$0e,$e0,$7b,$c0,$7b
 db $1d,$00,$00
 db $1d,$b1,$ee,$c1,$ee
 db $1d,$00,$00
 db $1d,$e1,$ee,$00
 db $1d,$00,$00
 db $1d,$b0,$7b,$c0,$7b
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$a3,$70
 db $1d,$00,$00
 db $1d,$b0,$7b,$a0,$7b
 db $1d,$00,$00
 db $95,$43,$00,$e3,$70,$a1,$ee
 db $1d,$00,$00
 db $9d,$81,$b1,$ee,$a2,$4b
 db $1d,$00,$00
 db $95,$04,$e0,$7b,$a0,$7b
 db $1d,$00,$00
 db $9d,$03,$b2,$4b,$a1,$ee
 db $1d,$00,$00
 db $9d,$81,$e1,$ee,$a2,$4b
 db $1d,$00,$00
 db $1d,$b0,$7b,$a0,$7b
 db $1d,$00,$00
 db $9d,$03,$e2,$4b,$00
 db $1d,$00,$00
 db $9d,$81,$b1,$ee,$a1,$ee
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$a2,$93
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$a1,$ee
 db $1d,$00,$00
 db $99,$43,$0e,$e2,$93,$a0,$7b
 db $1d,$00,$00
 db $9d,$81,$b1,$ee,$a1,$ee
 db $1d,$00,$00
 db $1d,$e1,$ee,$a2,$93
 db $1d,$00,$00
 db $99,$0e,$b1,$ee,$a2,$e4
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$c3,$70
 db $1d,$00,$00
 db $1d,$b2,$e4,$c0,$7b
 db $1d,$00,$00
 db $9d,$03,$e3,$70,$c1,$ee
 db $1d,$00,$00
 db $9d,$81,$b1,$ee,$c2,$4b
 db $1d,$00,$00
 db $95,$04,$e0,$7b,$c0,$7b
 db $1d,$00,$00
 db $9d,$03,$b2,$4b,$c1,$ee
 db $1d,$00,$00
 db $9d,$81,$e1,$ee,$c2,$4b
 db $1d,$00,$00
 db $1d,$b0,$7b,$c0,$7b
 db $1d,$00,$00
 db $9d,$03,$e2,$4b,$00
 db $1d,$00,$00
 db $9d,$81,$b1,$ee,$c1,$ee
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$00
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$00
 db $1d,$00,$00
 db $99,$0e,$e0,$7b,$c0,$7b
 db $1d,$00,$00
 db $1d,$b1,$ee,$c1,$ee
 db $1d,$00,$00
 db $1d,$e1,$ee,$00
 db $1d,$00,$00
 db $1d,$b0,$7b,$c0,$7b
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$a3,$70
 db $1d,$00,$00
 db $95,$00,$b0,$7b,$a0,$7b
 db $1d,$00,$00
 db $95,$43,$00,$e3,$70,$a1,$ee
 db $1d,$00,$00
 db $95,$c1,$00,$b1,$ee,$a2,$4b
 db $1d,$00,$00
 db $95,$04,$e0,$7b,$a0,$7b
 db $1d,$00,$00
 db $9d,$03,$b2,$4b,$a1,$ee
 db $1d,$00,$00
 db $9d,$81,$e1,$ee,$a2,$4b
 db $1d,$00,$00
 db $1d,$b0,$7b,$a0,$7b
 db $1d,$00,$00
 db $9d,$03,$e2,$4b,$00
 db $1d,$00,$00
 db $9d,$81,$b1,$ee,$a2,$93
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$a1,$ee
 db $1d,$00,$00
 db $95,$43,$04,$b2,$93,$00
 db $1d,$00,$00
 db $99,$c1,$0e,$e0,$7b,$a0,$7b
 db $1d,$00,$00
 db $1d,$b1,$ee,$a1,$ee
 db $1d,$00,$00
 db $99,$0e,$e1,$ee,$a2,$93
 db $1d,$00,$00
 db $85,$02,$b1,$ee,$a2,$e4
 db $1d,$00,$00
 db $95,$00,$e3,$10,$93,$70
 db $1d,$00,$00
 db $1d,$b2,$e4,$90,$62
 db $1d,$00,$00
 db $95,$43,$04,$e3,$70,$91,$88
 db $1d,$00,$00
 db $9d,$81,$b3,$10,$92,$4b
 db $1d,$00,$00
 db $85,$02,$e0,$62,$90,$62
 db $1d,$00,$00
 db $9d,$03,$b2,$4b,$91,$88
 db $1d,$00,$00
 db $95,$c1,$04,$e3,$10,$92,$4b
 db $1d,$00,$00
 db $1d,$b0,$62,$90,$62
 db $1d,$00,$00
 db $95,$43,$00,$e2,$4b,$00
 db $1d,$00,$00
 db $9d,$81,$b1,$88,$91,$88
 db $1d,$00,$00
 db $95,$04,$e1,$88,$00
 db $1d,$00,$00
 db $95,$04,$b1,$88,$00
 db $1d,$00,$00
 db $85,$02,$e0,$62,$90,$62
 db $1d,$00,$00
 db $1d,$b1,$88,$91,$88
 db $1d,$00,$00
 db $95,$04,$e1,$88,$00
 db $1d,$00,$00
 db $1d,$b0,$62,$90,$62
 db $1d,$00,$00
 db $95,$00,$e3,$10,$93,$70
 db $1d,$00,$00
 db $1d,$b0,$62,$90,$62
 db $1d,$00,$00
 db $95,$43,$04,$e3,$70,$91,$88
 db $1d,$00,$00
 db $9d,$81,$b3,$10,$92,$4b
 db $1d,$00,$00
 db $85,$02,$e0,$62,$90,$62
 db $1d,$00,$00
 db $9d,$03,$b2,$4b,$91,$88
 db $1d,$00,$00
 db $95,$c1,$04,$e3,$10,$92,$4b
 db $1d,$00,$00
 db $1d,$b0,$62,$90,$62
 db $1d,$00,$00
 db $95,$43,$00,$e2,$4b,$00
 db $1d,$00,$00
 db $9d,$81,$b3,$10,$92,$93
 db $1d,$00,$00
 db $95,$04,$e1,$88,$91,$88
 db $1d,$00,$00
 db $95,$43,$04,$b2,$93,$00
 db $1d,$00,$00
 db $85,$c1,$02,$e0,$62,$90,$62
 db $1d,$00,$00
 db $1d,$b1,$88,$91,$88
 db $1d,$00,$00
 db $95,$04,$e3,$10,$92,$93
 db $1d,$00,$00
 db $1d,$b3,$10,$92,$e4
 db $1d,$00,$00
 db $95,$00,$e2,$93,$83,$6c
 db $1d,$00,$00
 db $95,$00,$b2,$e4,$80,$52
 db $1d,$00,$00
 db $95,$43,$00,$e3,$70,$81,$49
 db $1d,$00,$00
 db $95,$c1,$00,$b1,$ee,$82,$8f
 db $1d,$00,$00
 db $85,$02,$e0,$52,$80,$52
 db $1d,$00,$00
 db $9d,$03,$b2,$93,$81,$49
 db $1d,$00,$00
 db $95,$c1,$04,$e1,$ee,$82,$8f
 db $1d,$00,$00
 db $1d,$b0,$52,$80,$52
 db $1d,$00,$00
 db $95,$43,$00,$e2,$93,$00
 db $1d,$00,$00
 db $9d,$81,$b1,$49,$81,$49
 db $1d,$00,$00
 db $95,$04,$e1,$49,$00
 db $1d,$00,$00
 db $95,$04,$b1,$49,$00
 db $1d,$00,$00
 db $85,$02,$e0,$52,$80,$52
 db $1d,$00,$00
 db $1d,$b1,$49,$81,$49
 db $1d,$00,$00
 db $95,$04,$e2,$2a,$82,$8f
 db $1d,$00,$00
 db $1d,$b2,$2a,$82,$e0
 db $1d,$00,$00
 db $95,$00,$e2,$93,$83,$6c
 db $1d,$00,$00
 db $1d,$b2,$e4,$80,$52
 db $1d,$00,$00
 db $95,$43,$04,$e3,$70,$81,$49
 db $1d,$00,$00
 db $9d,$81,$b1,$ee,$82,$8f
 db $1d,$00,$00
 db $85,$02,$e0,$52,$80,$52
 db $1d,$00,$00
 db $95,$43,$00,$b2,$93,$81,$49
 db $1d,$00,$00
 db $9d,$81,$e1,$ee,$82,$8f
 db $1d,$00,$00
 db $85,$02,$b0,$52,$80,$52
 db $1d,$00,$00
 db $95,$00,$92,$93,$81,$47
 db $1d,$00,$00
 db $1d,$00,$82,$93
 db $1d,$00,$82,$bb
 db $1d,$00,$82,$e4
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$81,$47,$f1,$72
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e1,$72
 db $1d,$00,$00
 db $1d,$b1,$ee,$e2,$2a
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$e1,$72
 db $1d,$00,$00
 db $8d,$06,$e0,$7b,$e0,$f7
 db $1d,$00,$00
 db $1d,$b0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$e0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $1d,$b1,$ee,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e1,$72
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$e0,$f7
 db $1d,$00,$00
 db $8d,$06,$e0,$7b,$00
 db $1d,$00,$00
 db $1d,$b0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$e0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$00
 db $1d,$00,$00
 db $95,$cb,$00,$e3,$70,$e3,$6e
 db $1d,$00,$00
 db $9d,$81,$b0,$7b,$e0,$f7
 db $1d,$00,$00
 db $95,$43,$04,$e3,$70,$00
 db $1d,$00,$00
 db $95,$cb,$04,$b2,$4b,$b2,$47
 db $1d,$00,$00
 db $8d,$c1,$06,$e0,$7b,$b0,$f7
 db $1d,$00,$00
 db $9d,$03,$b2,$4b,$00
 db $1d,$00,$00
 db $95,$cb,$04,$e2,$4b,$d2,$49
 db $1d,$00,$00
 db $95,$c1,$04,$b0,$7b,$d0,$f7
 db $1d,$00,$00
 db $95,$43,$00,$e2,$4b,$00
 db $1d,$00,$00
 db $9d,$81,$b1,$ee,$c1,$ee
 db $1d,$00,$00
 db $95,$cb,$00,$e2,$93,$a2,$91
 db $1d,$00,$00
 db $95,$c1,$04,$b1,$ee,$e1,$ee
 db $1d,$00,$00
 db $8d,$43,$06,$e2,$93,$c0,$7b
 db $1d,$00,$00
 db $9d,$81,$b1,$ee,$e1,$ee
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$91,$ee
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$f1,$ee
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e1,$72
 db $1d,$00,$00
 db $1d,$b1,$ee,$e2,$2a
 db $1d,$00,$00
 db $95,$04,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$e1,$72
 db $1d,$00,$00
 db $8d,$06,$e0,$7b,$e0,$f7
 db $1d,$00,$00
 db $1d,$b0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$e0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e2,$4b
 db $1d,$00,$00
 db $1d,$b1,$ee,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$ee,$e1,$72
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$e0,$f7
 db $1d,$00,$00
 db $8d,$06,$e0,$7b,$00
 db $1d,$00,$00
 db $1d,$b0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$e0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$00
 db $1d,$00,$00
 db $95,$cb,$00,$e3,$70,$e3,$6e
 db $1d,$00,$00
 db $9d,$81,$b0,$7b,$e0,$f7
 db $1d,$00,$00
 db $95,$43,$04,$e3,$70,$00
 db $1d,$00,$00
 db $95,$cb,$04,$b2,$4b,$b2,$47
 db $1d,$00,$00
 db $8d,$c1,$06,$e0,$7b,$b0,$f7
 db $1d,$00,$00
 db $9d,$03,$b2,$4b,$00
 db $1d,$00,$00
 db $95,$cb,$04,$e2,$4b,$a2,$49
 db $1d,$00,$00
 db $95,$c1,$04,$b0,$7b,$a0,$f7
 db $1d,$00,$00
 db $95,$43,$00,$e2,$4b,$00
 db $1d,$00,$00
 db $8d,$c1,$06,$b0,$7b,$00
 db $1d,$00,$00
 db $95,$cb,$00,$b2,$93,$a2,$91
 db $1d,$00,$00
 db $95,$c1,$04,$b0,$7b,$a0,$f7
 db $1d,$00,$00
 db $8d,$cb,$06,$e2,$e4,$a2,$e4
 db $1d,$00,$00
 db $9d,$03,$b2,$93,$a0,$f7
 db $1d,$00,$00
 db $95,$cb,$04,$e2,$93,$a2,$91
 db $1d,$00,$00
 db $95,$43,$04,$b2,$e4,$a0,$f7
 db $1d,$00,$00
 db $95,$c1,$00,$e3,$10,$e1,$ee
 db $1d,$00,$00
 db $1d,$b3,$10,$e2,$2a
 db $1d,$00,$00
 db $95,$cb,$04,$e2,$4b,$b2,$49
 db $1d,$00,$00
 db $95,$c1,$04,$b3,$10,$b1,$ee
 db $1d,$00,$00
 db $8d,$43,$06,$e2,$4b,$b0,$c4
 db $1d,$00,$00
 db $9d,$81,$b0,$62,$00
 db $1d,$00,$00
 db $95,$04,$e0,$62,$00
 db $1d,$00,$00
 db $95,$04,$b1,$88,$b2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$88,$b2,$4b
 db $1d,$00,$00
 db $1d,$b1,$88,$b2,$2a
 db $1d,$00,$00
 db $95,$00,$e1,$88,$b1,$ee
 db $1d,$00,$00
 db $95,$04,$90,$62,$b0,$c4
 db $1d,$00,$00
 db $8d,$06,$e0,$62,$00
 db $1d,$00,$00
 db $1d,$b0,$62,$00
 db $1d,$00,$00
 db $95,$04,$e0,$62,$00
 db $1d,$00,$00
 db $95,$04,$b0,$62,$00
 db $1d,$00,$00
 db $95,$cb,$00,$e3,$70,$e3,$6e
 db $1d,$00,$00
 db $9d,$81,$b0,$62,$e0,$c0
 db $1d,$00,$00
 db $95,$43,$04,$e3,$70,$00
 db $1d,$00,$00
 db $95,$cb,$04,$b2,$4b,$b2,$47
 db $1d,$00,$00
 db $8d,$c1,$06,$e0,$62,$b0,$c0
 db $1d,$00,$00
 db $9d,$03,$b2,$4b,$00
 db $1d,$00,$00
 db $95,$cb,$04,$e2,$4b,$d2,$49
 db $1d,$00,$00
 db $95,$c1,$04,$b0,$62,$d0,$c0
 db $1d,$00,$00
 db $95,$43,$00,$e2,$4b,$00
 db $1d,$00,$00
 db $8d,$c1,$06,$b1,$88,$91,$88
 db $1d,$00,$00
 db $95,$cb,$00,$e2,$93,$92,$91
 db $1d,$00,$00
 db $95,$c1,$04,$b1,$88,$91,$84
 db $1d,$00,$00
 db $8d,$cb,$06,$e2,$e4,$92,$e4
 db $1d,$00,$00
 db $9d,$03,$b2,$93,$91,$88
 db $1d,$00,$00
 db $95,$cb,$04,$e2,$93,$92,$91
 db $1d,$00,$00
 db $95,$c1,$04,$b1,$88,$91,$84
 db $1d,$00,$00
 db $95,$43,$00,$e2,$93,$e0,$a2
 db $1d,$00,$00
 db $9d,$81,$b0,$52,$00
 db $1d,$00,$00
 db $95,$cb,$04,$e2,$4b,$e2,$49
 db $1d,$00,$00
 db $95,$c1,$04,$b0,$52,$e0,$a2
 db $1d,$00,$00
 db $8d,$43,$06,$e2,$4b,$00
 db $1d,$00,$00
 db $9d,$81,$b0,$52,$00
 db $1d,$00,$00
 db $95,$cb,$04,$e3,$10,$e3,$0a
 db $1d,$00,$00
 db $95,$04,$b3,$70,$e3,$6e
 db $1d,$00,$00
 db $95,$00,$e3,$10,$e3,$0e
 db $1d,$00,$00
 db $9d,$03,$b3,$70,$e0,$a2
 db $1d,$00,$00
 db $95,$cb,$00,$e2,$e4,$e2,$e2
 db $1d,$00,$00
 db $95,$43,$04,$b3,$10,$e0,$a2
 db $1d,$00,$00
 db $8d,$cb,$06,$e2,$93,$e2,$91
 db $1d,$00,$00
 db $9d,$03,$b2,$e4,$e0,$a2
 db $1d,$00,$00
 db $95,$cb,$04,$e2,$4b,$e2,$49
 db $1d,$00,$00
 db $95,$43,$04,$b2,$93,$e0,$a2
 db $1d,$00,$00
 db $8d,$c1,$06,$e0,$5c,$e0,$b7
 db $1d,$00,$00
 db $8d,$43,$06,$b2,$4b,$00
 db $1d,$00,$00
 db $95,$cb,$04,$e2,$93,$e2,$8d
 db $1d,$00,$00
 db $95,$c1,$04,$b0,$5c,$e0,$b7
 db $1d,$00,$00
 db $8d,$cb,$06,$e2,$e4,$e2,$de
 db $1d,$00,$00
 db $9d,$03,$b2,$93,$e0,$b7
 db $1d,$00,$00
 db $8d,$cb,$06,$e2,$2a,$e2,$24
 db $1d,$00,$00
 db $8d,$43,$06,$b2,$e4,$e0,$b7
 db $1d,$00,$00
 db $95,$c1,$00,$e0,$5c,$00
 db $1d,$00,$00
 db $85,$43,$08,$b2,$2a,$00
 db $1d,$00,$00
 db $85,$cb,$0a,$e2,$4b,$c2,$45
 db $1d,$00,$00
 db $85,$c1,$08,$b0,$5c,$c0,$b7
 db $1d,$00,$00
 db $85,$cb,$0a,$e2,$93,$c2,$8d
 db $1d,$00,$00
 db $85,$43,$08,$b2,$4b,$c0,$b7
 db $1d,$00,$00
 db $85,$c1,$0a,$e0,$5c,$00
 db $1d,$00,$00
 db $85,$43,$0a,$b2,$93,$00
 db $1d,$00,$00
 db $95,$c1,$00,$e3,$dc,$e1,$72
 db $1d,$00,$00
 db $95,$04,$b3,$dc,$e2,$2a
 db $1d,$00,$00
 db $95,$0c,$e3,$dc,$e2,$4b
 db $1d,$ef,$72,$00
 db $95,$04,$b3,$dc,$e1,$72
 db $1d,$00,$00
 db $85,$0a,$e0,$7b,$e0,$f7
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$00
 db $1d,$00,$00
 db $99,$0e,$e0,$f3,$00
 db $1d,$ef,$72,$00
 db $95,$0c,$b3,$dc,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e3,$dc,$e2,$4b
 db $1d,$00,$00
 db $95,$0c,$b3,$dc,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e3,$dc,$e1,$72
 db $1d,$ef,$72,$00
 db $95,$04,$b0,$f3,$e0,$f7
 db $1d,$00,$00
 db $85,$0a,$e0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$00
 db $1d,$00,$00
 db $99,$0e,$e0,$f3,$00
 db $1d,$ef,$72,$00
 db $95,$04,$b0,$f3,$00
 db $1d,$00,$00
 db $95,$cb,$00,$e3,$70,$e3,$6e
 db $1d,$00,$00
 db $95,$c1,$04,$b0,$7b,$e0,$f7
 db $1d,$00,$00
 db $95,$43,$0c,$e3,$70,$00
 db $1d,$00,$00
 db $95,$cb,$04,$b2,$4b,$b2,$47
 db $1d,$00,$00
 db $85,$c1,$0a,$e0,$7b,$b0,$f7
 db $1d,$00,$00
 db $95,$43,$04,$b2,$4b,$00
 db $1d,$00,$00
 db $99,$cb,$0e,$e2,$4b,$d2,$49
 db $1d,$00,$00
 db $95,$c1,$0c,$b0,$f3,$d0,$f7
 db $1d,$bf,$72,$00
 db $95,$43,$00,$e2,$4b,$00
 db $1d,$00,$00
 db $95,$c1,$0c,$b1,$ee,$c1,$ee
 db $1d,$00,$00
 db $95,$cb,$00,$e2,$93,$a2,$91
 db $1d,$00,$00
 db $95,$c1,$04,$b1,$ee,$e1,$ee
 db $1d,$00,$00
 db $85,$43,$0a,$e2,$93,$c0,$7b
 db $1d,$00,$00
 db $95,$c1,$04,$b1,$ee,$e1,$ee
 db $1d,$00,$00
 db $99,$0e,$e1,$ee,$91,$ee
 db $1d,$00,$00
 db $95,$04,$b1,$ee,$f1,$ee
 db $1d,$00,$00
 db $95,$00,$e3,$dc,$e1,$72
 db $1d,$00,$00
 db $95,$04,$b3,$dc,$e2,$2a
 db $1d,$00,$00
 db $95,$0c,$e3,$dc,$e2,$4b
 db $1d,$ef,$72,$00
 db $95,$04,$b3,$dc,$e1,$72
 db $1d,$00,$00
 db $85,$0a,$e0,$7b,$e0,$f7
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$00
 db $1d,$00,$00
 db $99,$0e,$e0,$f3,$00
 db $1d,$ef,$72,$00
 db $95,$0c,$b3,$dc,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e3,$dc,$e2,$4b
 db $1d,$00,$00
 db $95,$0c,$b3,$dc,$e2,$2a
 db $1d,$00,$00
 db $95,$00,$e3,$dc,$e1,$72
 db $1d,$ef,$72,$00
 db $95,$04,$b0,$f3,$e0,$f7
 db $1d,$00,$00
 db $85,$0a,$e0,$7b,$00
 db $1d,$00,$00
 db $95,$04,$b0,$7b,$00
 db $1d,$00,$00
 db $99,$0e,$e0,$f3,$00
 db $1d,$ef,$72,$00
 db $95,$04,$b0,$f3,$00
 db $1d,$00,$00
 db $95,$cb,$00,$e3,$70,$e3,$6e
 db $1d,$00,$00
 db $95,$c1,$04,$b0,$7b,$e0,$f7
 db $1d,$00,$00
 db $95,$43,$0c,$e3,$70,$00
 db $1d,$00,$00
 db $95,$cb,$04,$b2,$4b,$b2,$47
 db $1d,$00,$00
 db $85,$c1,$0a,$e0,$7b,$b0,$f7
 db $1d,$00,$00
 db $95,$43,$04,$b2,$4b,$00
 db $1d,$00,$00
 db $99,$cb,$0e,$e2,$4b,$a2,$49
 db $1d,$00,$00
 db $95,$c1,$0c,$b0,$f7,$a0,$f7
 db $1d,$bf,$72,$00
 db $95,$43,$00,$e2,$4b,$00
 db $1d,$00,$00
 db $95,$c1,$0c,$b0,$7b,$00
 db $1d,$00,$00
 db $95,$cb,$00,$b2,$93,$a2,$91
 db $1d,$00,$00
 db $95,$c1,$04,$b0,$f7,$a0,$f7
 db $1d,$bf,$72,$00
 db $85,$cb,$0a,$e2,$e4,$a2,$e4
 db $1d,$00,$00
 db $95,$43,$04,$b2,$93,$a0,$f7
 db $1d,$00,$00
 db $99,$cb,$0e,$e2,$93,$a2,$91
 db $1d,$00,$00
 db $95,$43,$04,$b2,$e4,$a0,$f7
 db $1d,$00,$00
 db $95,$00,$e3,$10,$e1,$ee
 db $1d,$00,$00
 db $95,$04,$b3,$10,$e2,$2a
 db $1d,$00,$00
 db $95,$cb,$0c,$e2,$4b,$c2,$47
 db $1d,$00,$00
 db $95,$c1,$04,$b3,$10,$c1,$ee
 db $1d,$00,$00
 db $85,$43,$0a,$e2,$4b,$c0,$c4
 db $1d,$00,$00
 db $95,$c1,$04,$b0,$62,$00
 db $1d,$00,$00
 db $99,$0e,$e0,$c0,$00
 db $1d,$ec,$42,$00
 db $95,$0c,$b3,$10,$c2,$2a
 db $1d,$00,$00
 db $95,$00,$e3,$10,$c2,$4b
 db $1d,$00,$00
 db $95,$0c,$b3,$10,$c2,$2a
 db $1d,$00,$00
 db $95,$00,$e3,$10,$c1,$ee
 db $1d,$ec,$42,$00
 db $95,$04,$90,$c0,$c0,$c4
 db $1d,$00,$00
 db $85,$0a,$e0,$62,$00
 db $1d,$00,$00
 db $95,$04,$b0,$62,$00
 db $1d,$00,$00
 db $99,$0e,$e0,$c0,$00
 db $1d,$ec,$42,$00
 db $95,$04,$b0,$c0,$00
 db $1d,$00,$00
 db $95,$cb,$00,$e3,$70,$e3,$6e
 db $1d,$00,$00
 db $95,$c1,$04,$b0,$62,$e0,$c0
 db $1d,$00,$00
 db $95,$43,$0c,$e3,$70,$00
 db $1d,$00,$00
 db $95,$cb,$04,$b2,$4b,$b2,$47
 db $1d,$00,$00
 db $85,$c1,$0a,$e0,$62,$b0,$c0
 db $1d,$00,$00
 db $95,$43,$04,$b2,$4b,$00
 db $1d,$00,$00
 db $99,$cb,$0e,$e2,$4b,$d2,$49
 db $1d,$00,$00
 db $95,$c1,$0c,$b0,$c4,$d0,$c0
 db $1d,$bc,$42,$00
 db $95,$43,$00,$e2,$4b,$00
 db $1d,$00,$00
 db $95,$c1,$0c,$b1,$88,$91,$88
 db $1d,$00,$00
 db $95,$cb,$00,$e2,$93,$92,$91
 db $1d,$00,$00
 db $95,$c1,$04,$b1,$88,$91,$84
 db $1d,$00,$00
 db $85,$cb,$0a,$e2,$e4,$92,$e4
 db $1d,$00,$00
 db $95,$43,$04,$b2,$93,$91,$88
 db $1d,$00,$00
 db $99,$cb,$0e,$e2,$93,$92,$91
 db $1d,$00,$00
 db $95,$c1,$04,$b1,$88,$91,$84
 db $1d,$00,$00
 db $95,$43,$00,$e2,$93,$e0,$a2
 db $1d,$00,$00
 db $95,$c1,$04,$b0,$52,$00
 db $1d,$00,$00
 db $95,$cb,$0c,$e2,$4b,$e2,$49
 db $1d,$00,$00
 db $95,$c1,$04,$b0,$a0,$e0,$a2
 db $1d,$ba,$4f,$00
 db $85,$43,$0a,$e2,$4b,$00
 db $1d,$00,$00
 db $95,$c1,$04,$b0,$52,$00
 db $1d,$00,$00
 db $99,$cb,$0e,$e3,$10,$e3,$0a
 db $1d,$00,$00
 db $95,$0c,$b3,$70,$e3,$6e
 db $1d,$00,$00
 db $95,$00,$e3,$10,$e3,$0e
 db $1d,$00,$00
 db $95,$43,$0c,$b3,$70,$e0,$a2
 db $1d,$00,$00
 db $95,$cb,$00,$e2,$e4,$e2,$e2
 db $1d,$00,$00
 db $95,$43,$04,$b3,$10,$e0,$a2
 db $1d,$00,$00
 db $85,$cb,$0a,$e2,$93,$e2,$91
 db $1d,$00,$00
 db $95,$43,$04,$b2,$e4,$e0,$a2
 db $1d,$00,$00
 db $85,$cb,$0a,$e2,$4b,$e2,$49
 db $1d,$00,$00
 db $85,$43,$0a,$b2,$93,$e0,$a2
 db $1d,$00,$00
 db $95,$c1,$00,$e0,$5c,$e0,$b7
 db $1d,$00,$00
 db $95,$43,$04,$b2,$4b,$00
 db $1d,$00,$00
 db $99,$cb,$0e,$e2,$93,$e2,$8d
 db $1d,$00,$00
 db $95,$c1,$04,$b0,$b3,$e0,$b7
 db $1d,$bb,$92,$00
 db $9d,$8b,$e2,$e4,$e2,$de
 db $1d,$00,$00
 db $95,$43,$04,$b2,$93,$e0,$b7
 db $1d,$00,$00
 db $99,$cb,$0e,$e2,$2a,$e2,$24
 db $1d,$00,$00
 db $95,$43,$04,$b2,$e4,$e0,$b7
 db $1d,$00,$00
 db $9d,$81,$e0,$5c,$00
 db $1d,$00,$00
 db $95,$43,$00,$b2,$2a,$00
 db $1d,$00,$00
 db $95,$cb,$00,$e2,$4b,$c2,$45
 db $1d,$00,$00
 db $95,$c1,$00,$b0,$b3,$c0,$b7
 db $1d,$bb,$92,$00
 db $95,$cb,$00,$e2,$93,$c2,$8d
 db $1d,$00,$00
 db $95,$43,$00,$b2,$4b,$c0,$b7
 db $1d,$00,$00
 db $95,$c1,$00,$e0,$b3,$00
 db $1d,$eb,$92,$00
 db $95,$43,$00,$b2,$93,$00
 db $1d,$00,$00
 db $95,$cb,$00,$b1,$ec,$e1,$ee
 db $1d,$00,$00
 db $9d,$81,$80,$7b,$e0,$79
 db $1d,$00,$00
 db $9d,$03,$81,$ee,$e0,$7b
 db $1d,$00,$00
 db $9d,$81,$90,$7b,$90,$77
 db $1d,$00,$00
 db $1d,$a0,$7b,$a0,$77
 db $1d,$00,$00
 db $1d,$b0,$7b,$b0,$77
 db $1d,$00,$00
 db $1d,$c0,$7b,$c0,$77
 db $1d,$00,$00
 db $1d,$d0,$7b,$d0,$77
 db $1d,$00,$00
 db $1d,$e0,$7b,$c0,$79
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
.loop
 db $1d,$01,$01
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $1d,$00,$00
 db $00
 dw .loop
 align 2
.drumpar
.dp0
 dw .dsmp0+0
 db $02,$09,$40
.dp1
 dw .dsmp1+0
 db $06,$09,$40
.dp2
 dw .dsmp3+0
 db $02,$09,$40
.dp3
 dw .dsmp6+0
 db $04,$09,$40
.dp4
 dw .dsmp2+0
 db $06,$06,$40
.dp5
 dw .dsmp2+0
 db $06,$09,$40
.dp6
 dw .dsmp5+0
 db $02,$06,$40
.dp7
 dw .dsmp4+0
 db $01,$09,$40
.dp8
 dw .dsmp3+0
 db $02,$09,$30
.dp9
 dw .dsmp4+0
 db $01,$09,$28
.dp10
 dw .dsmp3+0
 db $02,$09,$20
.dp11
 dw .dsmp4+0
 db $01,$03,$28
.dp12
 dw .dsmp4+0
 db $01,$09,$18
.dp13
 dw .dsmp3+0
 db $02,$09,$10
.dp14
 dw .dsmp4+0
 db $01,$03,$10
.dp15
 dw .dsmp3+0
 db $02,$09,$00
.dp16
 dw .dsmp3+0
 db $02,$09,$08
.dp17
 dw .dsmp3+0
 db $02,$09,$18
.dp18
 dw .dsmp4+0
 db $01,$09,$20
.dp19
 dw .dsmp4+0
 db $01,$09,$30
.dp20
 dw .dsmp4+0
 db $01,$09,$38
.dsmp0
 db $00,$00,$00,$00,$00,$00,$00,$00,$01,$07,$f3,$fc,$ff,$ff,$ff,$ff
 db $ff,$e7,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$f3,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
 db $ff,$ff,$ff,$ff,$f8,$c0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp1
 db $00,$1f,$ff,$ff,$fd,$fc,$1f,$c0,$fc,$00,$00,$00,$00,$00,$00,$00
 db $00,$0f,$03,$e0,$3f,$01,$00,$00,$00,$00,$00,$00,$f8,$1f,$80,$fe
 db $0f,$c0,$00,$03,$e0,$7e,$00,$00,$00,$00,$00,$00,$00,$00,$1f,$01
 db $fc,$1f,$80,$fc,$00,$00,$00,$00,$00,$00,$03,$e0,$40,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$08,$0f,$80,$fc,$0f,$c0,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$03,$e0,$00,$00,$00,$00,$00,$00
 db $00,$00,$f8,$0f,$80,$fc,$07,$c0,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$f8,$1f,$80,$fc,$08,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$3f,$01,$f0,$3f,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$7e,$07,$e0,$7f,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp2
 db $00,$00,$00,$ff,$ff,$ff,$f8,$00,$00,$00,$00,$00,$00,$00,$00,$ff
 db $ff,$ff,$ff,$ff,$ff,$e2,$00,$00,$00,$00,$00,$00,$ff,$ff,$ff,$ff
 db $ff,$70,$91,$00,$00,$00,$00,$00,$00,$00,$00,$01,$ef,$ff,$ff,$ff
 db $ff,$a0,$00,$00,$00,$00,$00,$00,$20,$ff,$1f,$ff,$fb,$ff,$cd,$00
 db $00,$00,$00,$00,$00,$00,$01,$22,$9e,$fd,$ff,$7f,$df,$79,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$bf,$ff,$3f,$fc,$ce,$c0,$00,$00,$00
 db $00,$00,$00,$00,$00,$01,$13,$73,$ec,$8f,$a0,$00,$00,$00,$0c,$00
 db $00,$00,$00,$00,$00,$05,$99,$f0,$e3,$6c,$26,$40,$00,$00,$00,$00
 db $00,$00,$00,$04,$00,$8c,$19,$80,$fa,$60,$80,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$20,$00,$69,$40,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$04,$8c,$00,$40,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp3
 db $00,$90,$0c,$40,$04,$20,$00,$28,$00,$00,$00,$40,$40,$40,$00,$00
 db $80,$01,$00,$00,$00,$00,$00,$10,$00,$00,$00,$00,$00,$00,$20,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$40,$00,$00,$00,$00,$00,$00,$48
 db $01,$00,$00,$00,$00,$00,$40,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp4
 db $04,$20,$00,$00,$04,$10,$10,$01,$00,$04,$08,$00,$23,$80,$07,$40
 db $08,$90,$10,$00,$03,$00,$c0,$00,$64,$08,$08,$20,$03,$60,$31,$12
.dsmp5
 db $03,$ff,$fc,$00,$03,$ff,$f8,$00,$07,$ff,$f0,$00,$0f,$ff,$e0,$00
 db $1f,$ff,$c0,$00,$3f,$ff,$c0,$00,$3f,$ff,$80,$00,$7f,$ff,$00,$00
 db $ff,$ff,$00,$00,$ff,$c2,$00,$00,$fc,$04,$00,$01,$c0,$04,$00,$02
 db $00,$08,$00,$00,$00,$10,$00,$00,$00,$00,$00,$08,$00,$00,$00,$10
.dsmp6
 db $3c,$ff,$0e,$20,$00,$00,$00,$00,$01,$01,$0f,$7f,$cf,$bf,$ff,$ff
 db $ff,$ff,$ff,$f8,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$ff,$ff
 db $ff,$ff,$ff,$ff,$ff,$f9,$00,$00,$00,$00,$00,$00,$00,$00,$00,$09
 db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$cc,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$06,$7f,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$f9,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$03,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ef
 db $1e,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$ff,$ff,$ff
 db $ff,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

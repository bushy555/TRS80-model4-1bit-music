	output	"TRACK10.BIN"

	org $6000
begin:


; 	EARSHAVER
; 	TRACK 10
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




; TRACK10

;compiled music data


music_data
 dw .drumpar
.song
 db $88,$43,$00,$80,$b5,$b0,$b9
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $10,$80,$b7,$b0,$b9
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $90,$81,$81,$6e,$b1,$b8
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $90,$03,$80,$b7,$b0,$b9
 db $10,$00,$00
 db $10,$f1,$b8,$01
 db $10,$00,$00
 db $81,$c1,$02,$81,$6e,$b1,$49
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$81,$70,$b1,$b8
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $90,$03,$80,$b5,$b0,$b9
 db $10,$00,$00
 db $10,$f1,$b8,$01
 db $10,$00,$00
 db $10,$80,$b7,$b0,$b9
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $10,$80,$b5,$c1,$b8
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $10,$80,$b7,$c1,$72
 db $10,$00,$00
 db $10,$81,$b8,$01
 db $10,$00,$00
 db $88,$00,$80,$b5,$d2,$93
 db $10,$00,$00
 db $10,$91,$72,$01
 db $10,$00,$00
 db $10,$80,$b7,$d1,$72
 db $10,$00,$00
 db $10,$a2,$93,$01
 db $10,$00,$00
 db $81,$02,$80,$b5,$e1,$b8
 db $07,$00,$00
 db $10,$b1,$72,$01
 db $10,$00,$00
 db $10,$80,$b7,$e1,$72
 db $10,$00,$00
 db $10,$c1,$b8,$01
 db $10,$00,$00
 db $10,$80,$b5,$f2,$93
 db $10,$00,$00
 db $10,$d1,$72,$01
 db $10,$00,$00
 db $10,$80,$b7,$f1,$72
 db $10,$00,$00
 db $10,$e2,$93,$01
 db $10,$00,$00
 db $88,$00,$80,$b5,$b0,$b9
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $10,$80,$b7,$b0,$b9
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $90,$81,$81,$6e,$b1,$b8
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$b1,$49
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$43,$02,$80,$b7,$b0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $90,$81,$81,$70,$b1,$b8
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $90,$03,$80,$b5,$b0,$b9
 db $10,$00,$00
 db $10,$f1,$b8,$01
 db $10,$00,$00
 db $10,$80,$b7,$b0,$b9
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $10,$80,$b5,$f1,$b8
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $10,$80,$b7,$f1,$72
 db $10,$00,$00
 db $10,$81,$b8,$01
 db $10,$00,$00
 db $88,$00,$80,$b5,$e2,$93
 db $10,$00,$00
 db $10,$91,$72,$01
 db $10,$00,$00
 db $10,$80,$b7,$e1,$72
 db $10,$00,$00
 db $10,$a2,$93,$01
 db $10,$00,$00
 db $81,$02,$80,$b5,$d1,$b8
 db $07,$00,$00
 db $10,$b1,$72,$01
 db $10,$00,$00
 db $10,$80,$b7,$d1,$72
 db $10,$00,$00
 db $10,$c1,$b8,$01
 db $10,$00,$00
 db $10,$80,$b5,$c2,$93
 db $10,$00,$00
 db $10,$d1,$72,$01
 db $10,$00,$00
 db $10,$80,$b7,$c1,$72
 db $10,$00,$00
 db $10,$e2,$93,$01
 db $10,$00,$00
 db $88,$00,$80,$b5,$b0,$b9
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $10,$80,$b7,$b0,$b9
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $90,$81,$81,$6e,$b1,$b8
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $90,$03,$80,$b7,$b0,$b9
 db $10,$00,$00
 db $10,$f1,$b8,$01
 db $10,$00,$00
 db $81,$c1,$02,$81,$6e,$b1,$49
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$81,$70,$b1,$b8
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $90,$03,$80,$b5,$b0,$b9
 db $10,$00,$00
 db $10,$f1,$b8,$01
 db $10,$00,$00
 db $10,$80,$b7,$b0,$b9
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $10,$80,$b5,$c1,$b8
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $10,$80,$b7,$c1,$72
 db $10,$00,$00
 db $10,$81,$b8,$01
 db $10,$00,$00
 db $88,$00,$80,$b5,$d2,$93
 db $10,$00,$00
 db $10,$91,$72,$01
 db $10,$00,$00
 db $10,$80,$b7,$d1,$72
 db $10,$00,$00
 db $10,$a2,$93,$01
 db $10,$00,$00
 db $81,$02,$80,$b5,$e1,$b8
 db $07,$00,$00
 db $10,$b1,$72,$01
 db $10,$00,$00
 db $10,$80,$b7,$e1,$72
 db $10,$00,$00
 db $10,$c1,$b8,$01
 db $10,$00,$00
 db $10,$80,$b5,$f2,$93
 db $10,$00,$00
 db $10,$d1,$72,$01
 db $10,$00,$00
 db $10,$80,$b7,$f1,$72
 db $10,$00,$00
 db $10,$e2,$93,$01
 db $10,$00,$00
 db $88,$c1,$00,$81,$25,$b1,$b8
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $88,$00,$81,$25,$b1,$b8
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$b0,$8c
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $88,$00,$81,$25,$b0,$8c
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $81,$02,$81,$25,$b1,$b8
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$b0,$8c
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $88,$00,$81,$25,$b1,$b8
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$81,$49,$b1,$ee
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$b0,$9e
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $81,$02,$81,$49,$b1,$ee
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b0,$9e
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $10,$81,$49,$b1,$ee
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$00,$b0,$9e
 db $07,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $88,$00,$81,$49,$b1,$ee
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$00,$b0,$9e
 db $07,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $81,$02,$81,$49,$b0,$9e
 db $07,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $88,$43,$00,$80,$b5,$b0,$b9
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $88,$04,$80,$b7,$b0,$b9
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $81,$c1,$06,$81,$6e,$b1,$b8
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$43,$04,$80,$b7,$b0,$b9
 db $10,$00,$00
 db $10,$f1,$b8,$01
 db $10,$00,$00
 db $88,$c1,$00,$81,$6e,$b1,$49
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$81,$70,$b1,$b8
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$43,$06,$80,$b5,$b0,$b9
 db $07,$00,$00
 db $10,$f1,$b8,$01
 db $10,$00,$00
 db $88,$04,$80,$b7,$b0,$b9
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $88,$00,$80,$b5,$c1,$b8
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $88,$00,$80,$b7,$c1,$72
 db $10,$00,$00
 db $10,$81,$b8,$01
 db $10,$00,$00
 db $81,$06,$80,$b5,$d2,$93
 db $07,$00,$00
 db $10,$91,$72,$01
 db $10,$00,$00
 db $88,$04,$80,$b7,$d1,$72
 db $10,$00,$00
 db $10,$a2,$93,$01
 db $10,$00,$00
 db $88,$00,$80,$b5,$e1,$b8
 db $10,$00,$00
 db $10,$b1,$72,$01
 db $10,$00,$00
 db $88,$00,$80,$b7,$e1,$72
 db $10,$00,$00
 db $10,$c1,$b8,$01
 db $10,$00,$00
 db $81,$06,$80,$b5,$f2,$93
 db $07,$00,$00
 db $10,$d1,$72,$01
 db $10,$00,$00
 db $88,$04,$80,$b7,$f1,$72
 db $10,$00,$00
 db $10,$e2,$93,$01
 db $10,$00,$00
 db $88,$00,$80,$b5,$b0,$b9
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $88,$04,$80,$b7,$b0,$b9
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $81,$c1,$06,$81,$6e,$b1,$b8
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$49
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$43,$00,$80,$b7,$b0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$c1,$04,$81,$70,$b1,$b8
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$43,$06,$80,$b5,$b0,$b9
 db $07,$00,$00
 db $10,$f1,$b8,$01
 db $10,$00,$00
 db $88,$04,$80,$b7,$b0,$b9
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $88,$00,$80,$b5,$f1,$b8
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $88,$00,$80,$b7,$f1,$72
 db $10,$00,$00
 db $10,$81,$b8,$01
 db $10,$00,$00
 db $81,$06,$80,$b5,$e2,$93
 db $07,$00,$00
 db $10,$91,$72,$01
 db $10,$00,$00
 db $88,$04,$80,$b7,$e1,$72
 db $10,$00,$00
 db $10,$a2,$93,$01
 db $10,$00,$00
 db $88,$00,$80,$b5,$d1,$b8
 db $10,$00,$00
 db $10,$b1,$72,$01
 db $10,$00,$00
 db $88,$00,$80,$b7,$d1,$72
 db $10,$00,$00
 db $10,$c1,$b8,$01
 db $10,$00,$00
 db $81,$06,$80,$b5,$c2,$93
 db $07,$00,$00
 db $10,$d1,$72,$01
 db $10,$00,$00
 db $88,$04,$80,$b7,$c1,$72
 db $10,$00,$00
 db $10,$e2,$93,$01
 db $10,$00,$00
 db $88,$00,$80,$b5,$b0,$b9
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $88,$04,$80,$b7,$b0,$b9
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $81,$c1,$06,$81,$6e,$b1,$b8
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$43,$04,$80,$b7,$b0,$b9
 db $10,$00,$00
 db $10,$f1,$b8,$01
 db $10,$00,$00
 db $88,$c1,$00,$81,$6e,$b1,$49
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$81,$70,$b1,$b8
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$43,$06,$80,$b5,$b0,$b9
 db $07,$00,$00
 db $10,$f1,$b8,$01
 db $10,$00,$00
 db $88,$04,$80,$b7,$b0,$b9
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $88,$00,$80,$b5,$c1,$b8
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $88,$00,$80,$b7,$c1,$72
 db $10,$00,$00
 db $10,$81,$b8,$01
 db $10,$00,$00
 db $81,$06,$80,$b5,$d2,$93
 db $07,$00,$00
 db $10,$91,$72,$01
 db $10,$00,$00
 db $88,$04,$80,$b7,$d1,$72
 db $10,$00,$00
 db $10,$a2,$93,$01
 db $10,$00,$00
 db $88,$00,$80,$b5,$e1,$b8
 db $10,$00,$00
 db $10,$b1,$72,$01
 db $10,$00,$00
 db $88,$00,$80,$b7,$e1,$72
 db $10,$00,$00
 db $10,$c1,$b8,$01
 db $10,$00,$00
 db $81,$06,$80,$b5,$f2,$93
 db $07,$00,$00
 db $10,$d1,$72,$01
 db $10,$00,$00
 db $88,$04,$80,$b7,$f1,$72
 db $10,$00,$00
 db $10,$e2,$93,$01
 db $10,$00,$00
 db $88,$c1,$00,$82,$4b,$81,$b8
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $88,$00,$82,$4b,$81,$b8
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$81,$25,$80,$8c
 db $07,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $88,$00,$81,$25,$80,$8c
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $81,$06,$82,$4b,$81,$b8
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$81,$25,$80,$8c
 db $10,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $81,$06,$82,$4b,$81,$b8
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$82,$93,$81,$ee
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$81,$49,$80,$9e
 db $07,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $81,$06,$82,$93,$81,$ee
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$81,$49,$80,$9e
 db $07,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $81,$06,$82,$93,$81,$ee
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$81,$d2
 db $07,$00,$81,$b8
 db $10,$00,$81,$9f
 db $10,$00,$81,$88
 db $10,$00,$81,$72
 db $10,$00,$81,$5d
 db $10,$00,$81,$49
 db $10,$00,$81,$37
 db $90,$03,$a1,$d2,$01
 db $10,$b1,$b8,$00
 db $10,$c1,$9f,$00
 db $10,$d1,$88,$00
 db $10,$e1,$72,$00
 db $10,$f1,$5d,$00
 db $10,$f1,$49,$00
 db $10,$f1,$37,$00
 db $88,$4d,$00,$84,$55,$f0,$b9
 db $10,$83,$70,$00
 db $10,$82,$93,$00
 db $10,$01,$00
 db $88,$04,$94,$55,$f1,$b8
 db $10,$93,$70,$01
 db $10,$92,$93,$00
 db $10,$01,$00
 db $81,$02,$a4,$55,$f0,$dc
 db $07,$a3,$70,$00
 db $10,$a2,$93,$01
 db $10,$01,$00
 db $10,$b4,$55,$f1,$72
 db $10,$b3,$70,$00
 db $10,$b2,$93,$00
 db $10,$01,$00
 db $10,$b2,$93,$01
 db $10,$00,$00
 db $10,$b2,$e4,$00
 db $10,$00,$00
 db $88,$00,$b2,$93,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$92,$93,$f0,$b9
 db $07,$00,$00
 db $10,$92,$e4,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$92,$93,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$92,$e4,$f1,$b8
 db $10,$00,$01
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$b3,$3f,$f0,$dc
 db $07,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $10,$92,$e4,$f1,$72
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$b2,$e4,$01
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$93,$3f,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$b2,$93,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$92,$93,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b4,$55,$f1,$72
 db $10,$b3,$70,$01
 db $10,$b2,$93,$00
 db $10,$01,$00
 db $81,$02,$a4,$55,$f0,$b9
 db $07,$a3,$70,$00
 db $10,$a2,$93,$01
 db $10,$01,$00
 db $10,$94,$55,$f1,$25
 db $10,$93,$70,$00
 db $10,$92,$93,$00
 db $10,$01,$00
 db $10,$b3,$a5,$01
 db $10,$b3,$70,$00
 db $10,$b3,$3f,$00
 db $10,$00,$00
 db $88,$00,$00,$f0,$8a
 db $10,$00,$00
 db $10,$00,$93,$70
 db $10,$00,$93,$3f
 db $81,$02,$b3,$70,$f0,$92
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$8a
 db $10,$00,$00
 db $10,$00,$93,$70
 db $10,$00,$00
 db $88,$00,$94,$55,$f0,$a4
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$04,$93,$70,$f1,$9f
 db $10,$00,$01
 db $10,$01,$84,$55
 db $10,$00,$00
 db $81,$02,$92,$93,$f0,$cf
 db $07,$00,$00
 db $10,$01,$83,$70
 db $10,$00,$00
 db $10,$94,$55,$f1,$49
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $10,$b3,$dc,$82,$2a
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$b2,$93,$f0,$a4
 db $07,$b2,$e4,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$84,$55,$f0,$b9
 db $10,$83,$70,$00
 db $10,$82,$93,$00
 db $10,$01,$00
 db $88,$04,$94,$55,$f1,$b8
 db $10,$93,$70,$01
 db $10,$92,$93,$00
 db $10,$01,$00
 db $81,$02,$a4,$55,$f0,$dc
 db $07,$a3,$70,$00
 db $10,$a2,$93,$01
 db $10,$01,$00
 db $10,$b4,$55,$f1,$72
 db $10,$b3,$70,$00
 db $10,$b2,$93,$00
 db $10,$01,$00
 db $10,$a4,$55,$01
 db $10,$a3,$70,$00
 db $10,$a2,$93,$00
 db $10,$01,$00
 db $88,$00,$94,$55,$f0,$a4
 db $10,$93,$70,$00
 db $10,$92,$93,$00
 db $10,$01,$00
 db $81,$02,$b3,$70,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$b2,$2a,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f1,$b8
 db $10,$00,$92,$2a
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$92,$e4,$f0,$dc
 db $07,$00,$00
 db $10,$01,$82,$2a
 db $10,$00,$00
 db $10,$94,$55,$f1,$72
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $10,$b3,$dc,$82,$e4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$f0,$a4
 db $10,$00,$00
 db $10,$b3,$70,$00
 db $10,$b3,$3f,$00
 db $81,$02,$b2,$e4,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$a4
 db $10,$00,$00
 db $10,$00,$92,$e4
 db $10,$00,$00
 db $88,$00,$94,$97,$f0,$92
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$04,$93,$dc,$f1,$72
 db $10,$00,$01
 db $10,$01,$84,$97
 db $10,$00,$00
 db $81,$02,$a2,$e4,$f0,$b9
 db $07,$00,$00
 db $10,$01,$83,$dc
 db $10,$00,$00
 db $10,$a4,$97,$e1,$25
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $10,$b3,$dc,$84,$97
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$00,$b2,$e4,$f0,$8a
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $81,$02,$b3,$dc,$f0,$92
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$8a
 db $10,$00,$00
 db $10,$00,$00
 db $10,$b4,$17,$00
 db $88,$00,$b4,$55,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f1,$9f
 db $10,$00,$01
 db $10,$00,$94,$55
 db $10,$00,$00
 db $81,$02,$a1,$49,$f0,$cf
 db $07,$00,$00
 db $10,$00,$94,$55
 db $10,$00,$00
 db $10,$b3,$70,$f1,$49
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$70
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$a1,$49,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$b2,$e4,$f0,$a4
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$84,$55,$f0,$b9
 db $10,$83,$70,$00
 db $10,$82,$93,$00
 db $10,$01,$00
 db $88,$04,$94,$55,$f1,$b8
 db $10,$93,$70,$01
 db $10,$92,$93,$00
 db $10,$01,$00
 db $81,$02,$a4,$55,$f0,$dc
 db $07,$a3,$70,$00
 db $10,$a2,$93,$01
 db $10,$01,$00
 db $88,$04,$b4,$55,$f1,$72
 db $10,$b3,$70,$00
 db $10,$b2,$93,$00
 db $10,$01,$00
 db $88,$04,$b2,$93,$01
 db $10,$00,$00
 db $10,$b2,$e4,$00
 db $10,$00,$00
 db $88,$00,$b2,$93,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$92,$93,$f0,$b9
 db $07,$00,$00
 db $10,$92,$e4,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$92,$93,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$92,$e4,$f1,$b8
 db $10,$00,$01
 db $10,$00,$82,$93
 db $10,$00,$00
 db $81,$02,$b3,$70,$f0,$dc
 db $07,$00,$00
 db $10,$00,$82,$e4
 db $10,$00,$00
 db $88,$04,$92,$e4,$f1,$72
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b3,$3f,$93,$70
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$93,$70,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$b2,$93,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$92,$93,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b4,$55,$f1,$72
 db $10,$b3,$70,$01
 db $10,$b2,$93,$00
 db $10,$01,$00
 db $81,$02,$a4,$55,$f0,$b9
 db $07,$a3,$70,$00
 db $10,$a2,$93,$01
 db $10,$01,$00
 db $88,$04,$94,$55,$f1,$25
 db $10,$93,$70,$00
 db $10,$92,$93,$00
 db $10,$01,$00
 db $88,$04,$94,$97,$01
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$00,$93,$70,$f0,$8a
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $81,$02,$b3,$3f,$f0,$92
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$8a
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$b3,$70,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f1,$9f
 db $10,$00,$93,$70
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$94,$55,$f0,$cf
 db $07,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $88,$04,$93,$70,$f1,$49
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$04,$b3,$3f,$84,$55
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$00,$00,$f0,$92
 db $10,$00,$00
 db $10,$b3,$10,$00
 db $10,$b2,$e4,$00
 db $81,$02,$b2,$93,$f0,$a4
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$92
 db $10,$00,$00
 db $88,$04,$00,$92,$93
 db $10,$00,$00
 db $88,$00,$84,$55,$f0,$b9
 db $10,$83,$70,$00
 db $10,$82,$93,$00
 db $10,$01,$00
 db $88,$04,$94,$55,$f1,$b8
 db $10,$93,$70,$01
 db $10,$92,$93,$00
 db $10,$01,$00
 db $81,$02,$a4,$55,$f0,$dc
 db $07,$a3,$70,$00
 db $10,$a2,$93,$01
 db $10,$01,$00
 db $88,$04,$b4,$55,$f1,$72
 db $10,$b3,$70,$00
 db $10,$b2,$93,$00
 db $10,$01,$00
 db $88,$04,$a4,$55,$01
 db $10,$a3,$70,$00
 db $10,$a2,$93,$00
 db $10,$01,$00
 db $88,$00,$94,$55,$f0,$a4
 db $10,$93,$70,$00
 db $10,$92,$93,$00
 db $10,$01,$00
 db $81,$02,$b3,$70,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$b2,$2a,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f1,$b8
 db $10,$00,$92,$2a
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$92,$93,$f0,$dc
 db $07,$00,$00
 db $10,$01,$92,$2a
 db $10,$00,$00
 db $88,$04,$94,$55,$f1,$72
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$04,$b3,$dc,$82,$93
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$00,$00,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$b4,$55,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$a4
 db $10,$00,$00
 db $10,$00,$94,$55
 db $10,$00,$00
 db $88,$00,$94,$97,$f0,$92
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$04,$93,$70,$f1,$72
 db $10,$00,$84,$97
 db $10,$01,$00
 db $10,$00,$00
 db $81,$02,$a2,$e4,$f0,$b9
 db $07,$00,$00
 db $10,$01,$83,$70
 db $10,$00,$00
 db $88,$04,$a4,$97,$f1,$25
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$04,$b3,$70,$82,$e4
 db $10,$00,$00
 db $10,$01,$84,$97
 db $10,$00,$00
 db $88,$00,$b2,$e4,$f0,$8a
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $81,$02,$b2,$e4,$f0,$92
 db $07,$b3,$3f,$00
 db $10,$b3,$70,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$8a
 db $10,$00,$00
 db $10,$b3,$a5,$00
 db $10,$b3,$dc,$00
 db $88,$00,$b4,$55,$f0,$a4
 db $10,$b4,$97,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f1,$9f
 db $10,$00,$93,$70
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$b4,$55,$f0,$a4
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$00,$94,$97
 db $07,$00,$00
 db $81,$02,$00,$00
 db $07,$00,$00
 db $81,$02,$c4,$55,$f0,$a4
 db $07,$00,$00
 db $10,$c3,$70,$00
 db $10,$00,$00
 db $10,$d2,$93,$91,$15
 db $10,$00,$00
 db $10,$d4,$55,$00
 db $10,$00,$00
 db $10,$e3,$70,$01
 db $10,$00,$00
 db $10,$e2,$93,$00
 db $10,$00,$00
 db $81,$06,$f4,$55,$00
 db $07,$00,$00
 db $81,$06,$f3,$70,$00
 db $07,$00,$00
 db $88,$c1,$00,$b1,$72,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e1,$15
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$2a
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$cf,$00,$b1,$72,$91,$15
 db $10,$01,$01
 db $88,$00,$b1,$72,$b1,$15
 db $10,$01,$01
 db $88,$c1,$00,$b1,$72,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e1,$15
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$4b
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$e2,$93
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$00
 db $88,$00,$b1,$72,$e1,$25
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$25
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e1,$25
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$25
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$e1,$25
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$25
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$4b
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$25
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$00
 db $88,$45,$00,$f4,$97,$c0,$92
 db $10,$00,$00
 db $90,$03,$00,$00
 db $10,$00,$01
 db $88,$45,$04,$e4,$55,$c0,$92
 db $10,$00,$00
 db $90,$03,$f4,$97,$00
 db $10,$00,$01
 db $81,$45,$06,$d2,$e4,$b0,$92
 db $07,$00,$00
 db $90,$03,$e4,$55,$00
 db $10,$00,$01
 db $88,$45,$04,$c4,$97,$b0,$92
 db $10,$00,$00
 db $90,$03,$d2,$e4,$00
 db $10,$00,$01
 db $88,$45,$04,$b4,$55,$a0,$92
 db $10,$00,$00
 db $90,$03,$c4,$97,$00
 db $10,$00,$01
 db $81,$45,$06,$a2,$e4,$a0,$92
 db $07,$00,$00
 db $81,$43,$06,$b4,$55,$00
 db $07,$00,$01
 db $81,$45,$06,$94,$97,$90,$92
 db $07,$00,$00
 db $90,$03,$a2,$e4,$00
 db $10,$00,$01
 db $88,$45,$00,$84,$55,$90,$92
 db $10,$00,$00
 db $88,$43,$00,$94,$97,$00
 db $10,$00,$01
 db $88,$c1,$00,$b1,$72,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e1,$15
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$2a
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$cf,$00,$b1,$72,$e1,$15
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $88,$c1,$00,$00,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e1,$15
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$4b
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$e2,$93
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$00
 db $88,$00,$b1,$72,$e0,$f7
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b0,$f7
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e0,$f7
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b0,$f5
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$e0,$f5
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b0,$f5
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e1,$ea
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$ea
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$00
 db $81,$4d,$06,$90,$92,$b0,$92
 db $07,$00,$00
 db $10,$90,$f7,$00
 db $10,$00,$01
 db $88,$00,$a1,$15,$b0,$92
 db $10,$00,$00
 db $10,$a1,$25,$00
 db $10,$00,$90,$92
 db $81,$06,$b1,$ee,$b0,$92
 db $07,$00,$00
 db $10,$b2,$2a,$00
 db $10,$00,$91,$15
 db $88,$00,$c2,$4b,$b0,$92
 db $10,$00,$00
 db $10,$c3,$dc,$00
 db $10,$00,$91,$ee
 db $81,$06,$d4,$97,$b0,$92
 db $07,$00,$00
 db $10,$d4,$55,$00
 db $10,$00,$92,$4b
 db $88,$00,$e3,$dc,$b0,$92
 db $10,$00,$00
 db $10,$e2,$4b,$00
 db $10,$00,$94,$97
 db $81,$06,$f2,$2a,$b0,$92
 db $07,$00,$00
 db $81,$06,$f1,$ee,$00
 db $07,$00,$93,$dc
 db $81,$06,$f1,$25,$b0,$92
 db $07,$00,$00
 db $81,$06,$f1,$15,$00
 db $07,$00,$01
 db $88,$c1,$00,$b1,$72,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$2a
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b2,$2a
 db $10,$00,$00
 db $88,$00,$00,$00
 db $10,$00,$00
 db $88,$00,$b2,$e4,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$2a
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$cf,$00,$b1,$72,$92,$2a
 db $10,$00,$00
 db $88,$00,$00,$b2,$2a
 db $10,$00,$00
 db $88,$c1,$00,$00,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$2a
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b2,$2a
 db $10,$00,$00
 db $88,$00,$00,$00
 db $10,$00,$00
 db $88,$00,$b2,$e4,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$4b
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$e2,$93
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$00
 db $88,$00,$b1,$72,$e1,$25
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$25
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$4b
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b2,$4b
 db $10,$00,$00
 db $88,$00,$00,$00
 db $10,$00,$00
 db $88,$00,$b2,$e4,$e1,$25
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$25
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$4b
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b2,$4b
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$00
 db $88,$45,$00,$f4,$97,$c0,$92
 db $10,$00,$00
 db $90,$03,$00,$00
 db $10,$00,$01
 db $88,$45,$04,$e4,$55,$c0,$92
 db $10,$00,$00
 db $90,$03,$f4,$97,$00
 db $10,$00,$01
 db $81,$45,$06,$d2,$e4,$b0,$92
 db $07,$00,$00
 db $90,$03,$e4,$55,$00
 db $10,$00,$01
 db $88,$45,$04,$c4,$97,$b0,$92
 db $10,$00,$00
 db $90,$03,$d2,$e4,$00
 db $10,$00,$01
 db $88,$45,$04,$b4,$55,$a0,$92
 db $10,$00,$00
 db $90,$03,$c4,$97,$00
 db $10,$00,$01
 db $81,$45,$06,$a2,$e4,$a0,$92
 db $07,$00,$00
 db $81,$43,$06,$b4,$55,$00
 db $07,$00,$01
 db $81,$45,$06,$94,$97,$90,$92
 db $07,$00,$00
 db $90,$03,$a2,$e4,$00
 db $10,$00,$01
 db $88,$45,$00,$84,$55,$90,$92
 db $10,$00,$00
 db $88,$43,$00,$94,$97,$00
 db $10,$00,$01
 db $88,$c1,$00,$b1,$72,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$2a
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b2,$2a
 db $10,$00,$00
 db $88,$00,$00,$00
 db $10,$00,$00
 db $88,$00,$b2,$e4,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e4,$55
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$cf,$00,$00,$e2,$2a
 db $10,$01,$01
 db $88,$00,$b2,$e4,$e2,$2a
 db $10,$01,$01
 db $88,$c1,$00,$b1,$72,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$2a
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b2,$2a
 db $10,$00,$00
 db $88,$00,$00,$00
 db $10,$00,$00
 db $88,$00,$b2,$e4,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$4b
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$e2,$93
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$e1,$ee
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$ee
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e1,$ee
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$ec
 db $10,$00,$00
 db $81,$06,$00,$00
 db $07,$00,$00
 db $88,$00,$00,$e1,$ec
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$ec
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e3,$d8
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$b3,$d8
 db $07,$00,$00
 db $81,$06,$00,$00
 db $07,$00,$00
 db $81,$4d,$06,$90,$92,$b0,$92
 db $07,$00,$00
 db $10,$90,$f7,$00
 db $10,$00,$01
 db $10,$a1,$15,$b0,$92
 db $10,$00,$00
 db $10,$a1,$25,$00
 db $10,$00,$90,$92
 db $10,$b1,$ee,$b0,$92
 db $10,$00,$00
 db $10,$b2,$2a,$00
 db $10,$00,$91,$15
 db $10,$c2,$4b,$b0,$92
 db $10,$00,$00
 db $10,$c3,$dc,$00
 db $10,$00,$91,$ee
 db $10,$d4,$97,$b0,$92
 db $10,$00,$00
 db $10,$d4,$55,$00
 db $10,$00,$92,$4b
 db $10,$e3,$dc,$b0,$92
 db $10,$00,$00
 db $10,$e2,$4b,$00
 db $10,$00,$94,$97
 db $10,$f2,$2a,$b0,$92
 db $10,$00,$00
 db $10,$f1,$ee,$00
 db $10,$00,$93,$dc
 db $81,$02,$f1,$25,$b0,$92
 db $07,$00,$00
 db $81,$02,$f1,$15,$00
 db $07,$00,$01
 db $88,$00,$84,$55,$f0,$b9
 db $10,$83,$70,$00
 db $10,$82,$93,$00
 db $10,$01,$00
 db $88,$04,$94,$55,$f1,$b8
 db $10,$93,$70,$01
 db $10,$92,$93,$00
 db $10,$01,$00
 db $81,$02,$a4,$55,$f0,$dc
 db $07,$a3,$70,$00
 db $10,$a2,$93,$01
 db $10,$01,$00
 db $10,$b4,$55,$f1,$72
 db $10,$b3,$70,$00
 db $8c,$08,$b2,$93,$00
 db $10,$01,$00
 db $8c,$08,$b2,$93,$01
 db $10,$00,$00
 db $10,$b2,$e4,$00
 db $10,$00,$00
 db $88,$00,$b2,$93,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$92,$93,$f0,$b9
 db $07,$00,$00
 db $10,$92,$e4,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$92,$93,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$92,$e4,$f1,$b8
 db $10,$00,$01
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$b3,$3f,$f0,$dc
 db $07,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $8c,$08,$92,$e4,$f1,$72
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$b2,$e4,$01
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$93,$3f,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$b2,$93,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$92,$93,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$a1,$49,$f1,$72
 db $10,$00,$01
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$a1,$72,$f0,$b9
 db $07,$00,$00
 db $10,$00,$91,$49
 db $10,$00,$00
 db $10,$94,$55,$f1,$25
 db $10,$93,$70,$00
 db $8c,$08,$92,$93,$00
 db $10,$01,$00
 db $8c,$08,$b3,$a5,$91,$72
 db $10,$b3,$70,$00
 db $10,$b3,$3f,$00
 db $10,$00,$00
 db $88,$00,$00,$f0,$8a
 db $10,$00,$00
 db $10,$00,$93,$70
 db $10,$00,$93,$3f
 db $81,$02,$b3,$70,$f0,$92
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$8a
 db $10,$00,$00
 db $10,$00,$93,$70
 db $10,$00,$00
 db $88,$00,$94,$55,$f0,$a4
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$04,$93,$70,$f1,$9f
 db $10,$00,$01
 db $10,$01,$84,$55
 db $10,$00,$00
 db $81,$02,$92,$93,$f0,$cf
 db $07,$00,$00
 db $10,$01,$83,$70
 db $10,$00,$00
 db $10,$94,$55,$f1,$49
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $8c,$08,$b3,$dc,$82,$2a
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$b2,$93,$f0,$a4
 db $07,$b2,$e4,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$a1,$ee,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$94,$55,$f1,$b8
 db $10,$93,$70,$91,$ee
 db $10,$92,$93,$00
 db $10,$01,$00
 db $81,$02,$a1,$49,$f0,$dc
 db $07,$a1,$72,$00
 db $10,$00,$91,$ee
 db $10,$00,$00
 db $10,$b4,$55,$f1,$72
 db $10,$b3,$70,$00
 db $8c,$08,$b2,$93,$00
 db $10,$01,$00
 db $8c,$08,$a4,$55,$91,$49
 db $10,$a3,$70,$91,$72
 db $10,$a2,$93,$00
 db $10,$01,$00
 db $88,$00,$94,$55,$f0,$a4
 db $10,$93,$70,$00
 db $10,$92,$93,$00
 db $10,$01,$00
 db $81,$02,$b3,$70,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$b2,$2a,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f1,$b8
 db $10,$00,$92,$2a
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$92,$e4,$f0,$dc
 db $07,$00,$00
 db $10,$01,$82,$2a
 db $10,$00,$00
 db $10,$94,$55,$f1,$72
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $8c,$08,$b3,$dc,$82,$e4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$f0,$a4
 db $10,$00,$00
 db $10,$b3,$70,$00
 db $10,$b3,$3f,$00
 db $81,$02,$b2,$e4,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$a4
 db $10,$00,$00
 db $10,$00,$92,$e4
 db $10,$00,$00
 db $88,$00,$a1,$ee,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$93,$dc,$f1,$72
 db $10,$00,$01
 db $10,$01,$91,$ee
 db $10,$00,$00
 db $81,$02,$a1,$b8,$f0,$b9
 db $07,$a1,$d2,$00
 db $10,$a1,$72,$91,$ee
 db $10,$00,$00
 db $10,$a4,$97,$e1,$25
 db $10,$00,$00
 db $8c,$08,$01,$00
 db $10,$00,$00
 db $8c,$08,$b3,$dc,$91,$b8
 db $10,$00,$91,$9f
 db $10,$01,$91,$72
 db $10,$00,$00
 db $88,$00,$b2,$e4,$f0,$8a
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $81,$02,$b3,$dc,$f0,$92
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$8a
 db $10,$00,$00
 db $10,$00,$00
 db $10,$b4,$17,$00
 db $88,$00,$b4,$55,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f1,$9f
 db $10,$00,$01
 db $10,$00,$94,$55
 db $10,$00,$00
 db $81,$02,$a1,$ee,$f0,$cf
 db $07,$00,$00
 db $10,$00,$94,$55
 db $10,$00,$00
 db $10,$b3,$70,$f1,$49
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $8c,$08,$00,$93,$70
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$a2,$2a,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$b2,$e4,$f0,$a4
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$a1,$b8,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$94,$55,$f1,$b8
 db $10,$93,$70,$91,$b8
 db $10,$92,$93,$00
 db $10,$01,$00
 db $81,$02,$a1,$72,$f0,$dc
 db $07,$00,$00
 db $10,$00,$91,$b8
 db $10,$00,$00
 db $88,$04,$b4,$55,$f1,$72
 db $10,$b3,$70,$00
 db $8c,$08,$b2,$93,$00
 db $10,$01,$00
 db $88,$04,$b2,$93,$91,$72
 db $10,$00,$00
 db $10,$b2,$e4,$00
 db $10,$00,$00
 db $88,$00,$b2,$93,$90,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$92,$93,$90,$b9
 db $07,$00,$00
 db $10,$92,$e4,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$92,$93,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$92,$e4,$f1,$b8
 db $10,$00,$01
 db $10,$00,$82,$93
 db $10,$00,$00
 db $81,$02,$b3,$70,$f0,$dc
 db $07,$00,$00
 db $10,$00,$82,$e4
 db $10,$00,$00
 db $88,$04,$92,$e4,$f1,$72
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b3,$3f,$93,$70
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$93,$70,$f0,$a4
 db $10,$00,$00
 db $8c,$08,$00,$00
 db $10,$00,$00
 db $81,$02,$b2,$93,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$92,$93,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$a1,$49,$f1,$72
 db $10,$00,$01
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$a1,$72,$f0,$b9
 db $07,$00,$00
 db $10,$00,$91,$49
 db $10,$00,$00
 db $88,$04,$94,$55,$f1,$25
 db $10,$93,$70,$00
 db $8c,$08,$92,$93,$00
 db $10,$01,$00
 db $88,$04,$94,$97,$91,$72
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$00,$93,$70,$f0,$8a
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $81,$02,$b3,$3f,$f0,$92
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$8a
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$b3,$70,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f1,$9f
 db $10,$00,$93,$70
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$94,$55,$f0,$cf
 db $07,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $88,$04,$93,$70,$f1,$49
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$04,$b3,$3f,$84,$55
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$00,$00,$f0,$92
 db $10,$00,$00
 db $8c,$08,$b3,$10,$00
 db $10,$b2,$e4,$00
 db $81,$02,$b2,$93,$f0,$a4
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$92
 db $10,$00,$00
 db $88,$04,$00,$92,$93
 db $10,$00,$00
 db $88,$00,$a1,$9f,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$94,$55,$f1,$b8
 db $10,$93,$70,$91,$9f
 db $10,$92,$93,$00
 db $10,$01,$00
 db $81,$02,$a1,$88,$f0,$dc
 db $07,$a1,$72,$00
 db $10,$a1,$49,$91,$9f
 db $10,$00,$00
 db $88,$04,$b4,$55,$f1,$72
 db $10,$b3,$70,$00
 db $8c,$08,$b2,$93,$00
 db $10,$01,$00
 db $88,$04,$a4,$55,$91,$88
 db $10,$a3,$70,$91,$72
 db $10,$a2,$93,$91,$49
 db $10,$01,$00
 db $88,$00,$94,$55,$f0,$a4
 db $10,$93,$70,$00
 db $10,$92,$93,$00
 db $10,$01,$00
 db $81,$02,$b3,$70,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$b2,$2a,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f1,$b8
 db $10,$00,$92,$2a
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$92,$93,$f0,$dc
 db $07,$00,$00
 db $10,$01,$92,$2a
 db $10,$00,$00
 db $88,$04,$94,$55,$f1,$72
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$04,$b3,$dc,$82,$93
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$00,$00,$f0,$a4
 db $10,$00,$00
 db $8c,$08,$00,$00
 db $10,$00,$00
 db $81,$02,$b4,$55,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$a4
 db $10,$00,$00
 db $10,$00,$94,$55
 db $10,$00,$00
 db $88,$00,$a1,$ee,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$a3,$70,$f1,$72
 db $10,$00,$91,$ee
 db $10,$01,$00
 db $10,$00,$00
 db $81,$02,$a2,$2a,$f0,$b9
 db $07,$00,$00
 db $10,$00,$91,$ee
 db $10,$00,$00
 db $88,$04,$a4,$97,$f1,$25
 db $10,$00,$00
 db $8c,$08,$01,$00
 db $10,$00,$00
 db $88,$04,$b3,$70,$92,$2a
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$00,$b2,$e4,$f0,$8a
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $81,$02,$b2,$e4,$f0,$92
 db $07,$b3,$3f,$00
 db $10,$b3,$70,$00
 db $10,$00,$00
 db $88,$04,$00,$f0,$8a
 db $10,$00,$00
 db $10,$b3,$a5,$00
 db $10,$b3,$dc,$00
 db $88,$00,$b4,$55,$92,$e4
 db $10,$b4,$97,$93,$3f
 db $10,$00,$93,$70
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$a5
 db $10,$00,$93,$dc
 db $81,$02,$b4,$55,$94,$55
 db $07,$00,$94,$97
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$02,$c4,$55,$94,$55
 db $07,$00,$00
 db $10,$c3,$70,$00
 db $10,$00,$00
 db $10,$d2,$93,$00
 db $10,$00,$00
 db $10,$d4,$55,$00
 db $10,$00,$00
 db $81,$02,$e3,$70,$01
 db $07,$00,$00
 db $10,$e2,$93,$00
 db $10,$00,$00
 db $81,$06,$f4,$55,$00
 db $07,$00,$00
 db $81,$06,$f3,$70,$00
 db $07,$00,$00
 db $88,$d1,$00,$94,$97,$90,$92
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$04,$93,$dc,$90,$92
 db $10,$00,$00
 db $8c,$08,$00,$01
 db $10,$00,$00
 db $81,$02,$90,$92,$83,$70
 db $07,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$04,$94,$97,$90,$92
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $8c,$08,$90,$92,$83,$3f
 db $10,$00,$00
 db $10,$83,$70,$00
 db $10,$00,$00
 db $88,$00,$00,$90,$92
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $81,$02,$90,$92,$82,$e4
 db $07,$00,$00
 db $10,$83,$3f,$00
 db $10,$00,$00
 db $88,$04,$90,$92,$82,$93
 db $10,$00,$00
 db $88,$04,$83,$3f,$00
 db $10,$00,$00
 db $88,$00,$90,$92,$00
 db $10,$00,$00
 db $10,$82,$e4,$00
 db $10,$00,$00
 db $88,$04,$90,$92,$82,$e4
 db $10,$00,$00
 db $8c,$08,$82,$93,$00
 db $10,$00,$00
 db $81,$02,$00,$90,$92
 db $07,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$04,$92,$e4,$90,$92
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $8c,$08,$94,$97,$90,$92
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$00,$93,$dc,$90,$92
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $81,$02,$92,$e4,$90,$92
 db $07,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$04,$90,$92,$83,$70
 db $10,$00,$00
 db $88,$04,$01,$83,$dc
 db $10,$00,$00
 db $88,$00,$90,$a4,$84,$55
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$04,$90,$a4,$00
 db $10,$00,$00
 db $8c,$08,$83,$70,$00
 db $10,$00,$00
 db $81,$02,$83,$dc,$90,$a4
 db $07,$00,$00
 db $10,$84,$55,$01
 db $10,$00,$00
 db $88,$04,$00,$90,$a4
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $8c,$08,$90,$a4,$83,$70
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$00,$90,$a4,$00
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $81,$02,$90,$a4,$83,$3f
 db $07,$00,$00
 db $10,$83,$70,$00
 db $10,$00,$00
 db $88,$04,$90,$a4,$00
 db $10,$00,$00
 db $88,$04,$83,$70,$00
 db $10,$00,$00
 db $88,$00,$83,$3f,$90,$a4
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$04,$00,$90,$a4
 db $10,$00,$00
 db $8c,$08,$00,$01
 db $10,$00,$00
 db $81,$02,$93,$70,$90,$a4
 db $07,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$04,$95,$27,$90,$a4
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $8c,$08,$94,$55,$90,$a4
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$00,$93,$70,$90,$a4
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $81,$02,$95,$27,$90,$a4
 db $07,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$04,$94,$55,$90,$a4
 db $10,$00,$00
 db $88,$04,$00,$01
 db $10,$00,$00
 db $88,$00,$00,$90,$8a
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$04,$93,$70,$90,$8a
 db $10,$00,$00
 db $8c,$08,$00,$01
 db $10,$00,$00
 db $81,$02,$90,$8a,$83,$70
 db $07,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$04,$90,$8a,$83,$3f
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $8c,$08,$83,$70,$90,$8a
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$00,$83,$3f,$90,$8a
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $81,$02,$90,$8a,$82,$e4
 db $07,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$04,$90,$8a,$82,$93
 db $10,$00,$00
 db $88,$04,$01,$00
 db $10,$00,$00
 db $88,$00,$82,$e4,$90,$8a
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$04,$90,$8a,$82,$e4
 db $10,$00,$00
 db $8c,$08,$82,$93,$00
 db $10,$00,$00
 db $81,$02,$92,$e4,$90,$8a
 db $07,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$04,$82,$e4,$90,$8a
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $8c,$08,$90,$8a,$82,$2a
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$00,$92,$e4,$90,$8a
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $81,$02,$a0,$8a,$82,$93
 db $07,$00,$00
 db $10,$82,$2a,$00
 db $10,$00,$00
 db $88,$04,$00,$80,$8a
 db $10,$00,$00
 db $88,$04,$82,$93,$01
 db $10,$00,$00
 db $88,$00,$90,$8a,$82,$e4
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$04,$93,$70,$90,$b9
 db $10,$00,$00
 db $8c,$08,$00,$01
 db $10,$00,$00
 db $81,$02,$82,$e4,$90,$b9
 db $07,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$04,$94,$55,$90,$b9
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $8c,$08,$90,$b9,$83,$70
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$00,$92,$93,$90,$b9
 db $10,$00,$00
 db $10,$83,$70,$01
 db $10,$00,$00
 db $81,$02,$90,$b9,$83,$3f
 db $07,$00,$00
 db $10,$83,$70,$00
 db $10,$00,$00
 db $88,$04,$93,$70,$90,$b9
 db $10,$00,$00
 db $88,$04,$00,$01
 db $10,$00,$00
 db $88,$00,$83,$3f,$90,$b9
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$04,$93,$70,$90,$b9
 db $10,$00,$00
 db $8c,$08,$00,$01
 db $10,$00,$00
 db $81,$02,$90,$b9,$83,$70
 db $07,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$04,$94,$55,$90,$b9
 db $10,$00,$00
 db $10,$83,$70,$00
 db $10,$00,$00
 db $8c,$08,$90,$b9,$83,$dc
 db $10,$00,$00
 db $10,$83,$70,$00
 db $10,$00,$00
 db $88,$00,$90,$b9,$00
 db $10,$00,$00
 db $10,$83,$dc,$00
 db $10,$00,$00
 db $81,$02,$90,$b9,$84,$55
 db $07,$00,$00
 db $10,$83,$dc,$00
 db $10,$00,$00
 db $88,$04,$00,$90,$b9
 db $10,$00,$00
 db $88,$04,$00,$01
 db $10,$00,$00
 db $88,$00,$84,$55,$90,$92
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$04,$93,$dc,$90,$92
 db $10,$00,$00
 db $8c,$08,$00,$01
 db $10,$00,$00
 db $81,$02,$90,$92,$83,$70
 db $07,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$04,$90,$92,$83,$3f
 db $10,$00,$00
 db $10,$83,$70,$00
 db $10,$00,$00
 db $8c,$08,$93,$dc,$90,$92
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$00,$83,$3f,$90,$92
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $81,$02,$90,$92,$82,$e4
 db $07,$00,$00
 db $10,$01,$01
 db $10,$00,$00
 db $88,$04,$90,$92,$82,$93
 db $10,$00,$00
 db $88,$04,$82,$e4,$00
 db $10,$00,$00
 db $88,$00,$90,$92,$00
 db $10,$00,$00
 db $10,$82,$93,$00
 db $10,$00,$00
 db $88,$04,$90,$92,$82,$e4
 db $10,$00,$00
 db $8c,$08,$82,$93,$00
 db $10,$00,$00
 db $81,$02,$90,$92,$00
 db $07,$00,$00
 db $10,$82,$e4,$00
 db $10,$00,$00
 db $88,$04,$90,$92,$00
 db $10,$00,$00
 db $10,$82,$e4,$00
 db $10,$00,$00
 db $8c,$08,$93,$dc,$90,$92
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$00,$92,$e4,$90,$92
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $81,$02,$94,$97,$90,$92
 db $07,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$04,$90,$92,$83,$70
 db $10,$00,$00
 db $88,$04,$01,$84,$55
 db $10,$00,$00
 db $88,$00,$90,$a4,$85,$27
 db $10,$00,$00
 db $10,$83,$70,$00
 db $10,$00,$00
 db $88,$04,$90,$a4,$00
 db $10,$00,$00
 db $8c,$08,$84,$55,$00
 db $10,$00,$00
 db $81,$02,$85,$27,$90,$a4
 db $07,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$04,$90,$a4,$83,$70
 db $10,$00,$00
 db $10,$85,$27,$00
 db $10,$00,$00
 db $8c,$08,$90,$a4,$00
 db $10,$00,$00
 db $10,$85,$27,$00
 db $10,$00,$00
 db $88,$00,$83,$70,$90,$a4
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $81,$02,$90,$a4,$83,$3f
 db $07,$00,$00
 db $10,$83,$70,$00
 db $10,$00,$00
 db $88,$04,$90,$a4,$00
 db $10,$00,$00
 db $88,$04,$83,$70,$00
 db $10,$00,$00
 db $88,$00,$83,$3f,$90,$a4
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$04,$00,$90,$a4
 db $10,$00,$00
 db $8c,$08,$00,$01
 db $10,$00,$00
 db $81,$02,$93,$70,$90,$a4
 db $07,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$04,$95,$27,$90,$a4
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $8c,$08,$94,$55,$90,$a4
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$00,$93,$70,$90,$a4
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $81,$02,$95,$27,$90,$a4
 db $07,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$04,$93,$70,$90,$a4
 db $10,$00,$00
 db $88,$04,$00,$01
 db $10,$00,$00
 db $88,$00,$90,$b9,$83,$70
 db $10,$00,$00
 db $10,$01,$00
 db $10,$00,$00
 db $88,$04,$93,$70,$90,$b9
 db $10,$00,$00
 db $8c,$08,$00,$01
 db $10,$00,$00
 db $81,$02,$90,$b9,$83,$3f
 db $07,$00,$00
 db $10,$83,$70,$00
 db $10,$00,$00
 db $88,$04,$94,$55,$90,$b9
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $8c,$08,$90,$b9,$83,$70
 db $10,$00,$00
 db $10,$83,$3f,$00
 db $10,$00,$00
 db $88,$00,$92,$93,$90,$b9
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $81,$02,$90,$b9,$83,$3f
 db $07,$00,$00
 db $10,$83,$70,$00
 db $10,$00,$00
 db $88,$04,$90,$b9,$83,$70
 db $10,$00,$00
 db $88,$04,$83,$70,$00
 db $10,$00,$00
 db $88,$00,$83,$3f,$90,$b9
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $88,$04,$90,$b9,$83,$3f
 db $10,$00,$00
 db $8c,$08,$83,$70,$00
 db $10,$00,$00
 db $81,$02,$90,$b9,$83,$70
 db $07,$00,$00
 db $10,$83,$70,$00
 db $10,$00,$00
 db $88,$04,$83,$3f,$90,$b9
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$00
 db $8c,$08,$90,$b9,$83,$3f
 db $10,$00,$00
 db $10,$83,$70,$00
 db $10,$00,$00
 db $88,$00,$90,$b9,$00
 db $10,$00,$00
 db $10,$83,$3f,$00
 db $10,$00,$00
 db $81,$02,$90,$b9,$82,$e4
 db $07,$00,$00
 db $10,$83,$3f,$00
 db $10,$00,$00
 db $88,$04,$93,$70,$90,$b9
 db $10,$00,$00
 db $88,$04,$00,$01
 db $10,$00,$00
 db $88,$00,$90,$b9,$f3,$70
 db $10,$00,$00
 db $10,$82,$e4,$00
 db $10,$00,$01
 db $88,$04,$90,$b9,$f3,$3f
 db $10,$00,$00
 db $8c,$08,$82,$e4,$00
 db $10,$00,$01
 db $81,$02,$90,$b9,$e2,$e4
 db $07,$00,$00
 db $10,$83,$70,$00
 db $10,$00,$01
 db $88,$04,$90,$b9,$e3,$70
 db $10,$00,$00
 db $10,$83,$3f,$00
 db $10,$00,$01
 db $8c,$08,$90,$b9,$d3,$3f
 db $10,$00,$00
 db $10,$82,$e4,$00
 db $10,$00,$01
 db $88,$00,$90,$b9,$c2,$e4
 db $10,$00,$00
 db $10,$83,$70,$00
 db $10,$00,$01
 db $81,$02,$90,$b9,$b3,$70
 db $07,$00,$00
 db $10,$83,$3f,$00
 db $10,$00,$01
 db $88,$04,$90,$b9,$a3,$3f
 db $10,$00,$00
 db $88,$04,$82,$e4,$92,$93
 db $10,$00,$92,$bb
 db $88,$00,$90,$b9,$82,$e4
 db $10,$00,$00
 db $10,$83,$70,$00
 db $10,$00,$00
 db $88,$04,$90,$b9,$00
 db $10,$00,$00
 db $8c,$08,$83,$3f,$00
 db $10,$00,$00
 db $81,$02,$90,$b9,$00
 db $07,$00,$00
 db $10,$82,$e4,$00
 db $10,$00,$00
 db $88,$04,$90,$b9,$00
 db $10,$00,$00
 db $10,$92,$e4,$00
 db $10,$00,$00
 db $88,$00,$00,$b0,$b9
 db $10,$00,$00
 db $10,$00,$b0,$c4
 db $10,$00,$00
 db $81,$06,$00,$b0,$cf
 db $07,$00,$00
 db $81,$06,$00,$b0,$dc
 db $07,$00,$00
 db $81,$06,$00,$b0,$e9
 db $07,$00,$00
 db $10,$00,$b0,$f7
 db $10,$00,$00
 db $81,$06,$00,$b1,$05
 db $07,$00,$00
 db $81,$06,$00,$b1,$15
 db $07,$00,$00
 db $88,$4d,$00,$b0,$dc,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$01
 db $88,$04,$b0,$dc,$f0,$92
 db $10,$00,$00
 db $8c,$08,$00,$00
 db $10,$01,$01
 db $81,$06,$b3,$70,$f0,$92
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$01
 db $88,$04,$b0,$dc,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$93,$70
 db $8c,$08,$b3,$3f,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$70
 db $88,$00,$b0,$dc,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$93,$3f
 db $81,$06,$b2,$e4,$f0,$92
 db $07,$00,$00
 db $10,$00,$00
 db $10,$b2,$bb,$93,$3f
 db $88,$04,$b2,$93,$f0,$92
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$92,$e4
 db $88,$00,$00,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$92,$93
 db $88,$04,$b2,$e4,$f0,$92
 db $10,$00,$00
 db $8c,$08,$00,$00
 db $10,$00,$92,$93
 db $81,$06,$b0,$dc,$f0,$92
 db $07,$00,$00
 db $10,$00,$00
 db $10,$01,$92,$e4
 db $88,$04,$b0,$dc,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$92,$e4
 db $8c,$08,$b0,$dc,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$82,$e4
 db $88,$00,$b0,$dc,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$82,$e4
 db $81,$06,$b0,$dc,$f0,$92
 db $07,$00,$00
 db $10,$00,$00
 db $10,$01,$01
 db $88,$04,$b3,$70,$f0,$92
 db $10,$b3,$a5,$00
 db $88,$04,$b3,$dc,$00
 db $10,$b4,$17,$01
 db $88,$00,$b4,$55,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$70
 db $88,$04,$00,$f0,$a4
 db $10,$00,$00
 db $8c,$08,$00,$00
 db $10,$00,$94,$55
 db $81,$06,$b0,$f7,$f0,$a4
 db $07,$00,$00
 db $10,$00,$00
 db $10,$01,$94,$55
 db $88,$04,$b0,$f7,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$84,$55
 db $8c,$08,$b3,$70,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$01
 db $88,$00,$00,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$70
 db $81,$06,$b3,$3f,$f0,$a4
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$70
 db $88,$04,$00,$f0,$a4
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$93,$3f
 db $88,$00,$00,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$3f
 db $88,$04,$00,$f0,$a4
 db $10,$00,$00
 db $8c,$08,$00,$00
 db $10,$00,$93,$3f
 db $81,$06,$b0,$f7,$f0,$a4
 db $07,$00,$00
 db $10,$00,$00
 db $10,$01,$93,$3f
 db $88,$04,$b0,$f7,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$83,$3f
 db $8c,$08,$b0,$f7,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$83,$3f
 db $88,$00,$b0,$f7,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$01
 db $81,$06,$b0,$f7,$f0,$a4
 db $07,$00,$00
 db $10,$00,$00
 db $10,$01,$01
 db $88,$04,$b0,$f7,$f0,$a4
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$01,$01
 db $88,$00,$b0,$cf,$f0,$8a
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$01
 db $88,$04,$b0,$cf,$f0,$8a
 db $10,$00,$00
 db $8c,$08,$00,$00
 db $10,$01,$01
 db $81,$06,$b3,$70,$f0,$8a
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$01
 db $88,$04,$b3,$3f,$f0,$8a
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$70
 db $8c,$08,$b0,$cf,$f0,$8a
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$93,$3f
 db $88,$00,$b0,$cf,$f0,$8a
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$83,$3f
 db $81,$06,$b2,$e4,$f0,$8a
 db $07,$00,$00
 db $10,$00,$00
 db $10,$b2,$bb,$01
 db $88,$04,$b2,$93,$f0,$8a
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$92,$e4
 db $88,$00,$00,$f0,$8a
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$92,$93
 db $88,$04,$b2,$e4,$f0,$8a
 db $10,$00,$00
 db $8c,$08,$00,$00
 db $10,$00,$92,$93
 db $81,$06,$00,$f0,$8a
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$92,$e4
 db $88,$04,$b0,$cf,$f0,$8a
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$92,$e4
 db $8c,$08,$b2,$2a,$f0,$8a
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$92,$e4
 db $88,$00,$b0,$cf,$f0,$8a
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$92,$2a
 db $81,$06,$b2,$93,$f0,$8a
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$82,$2a
 db $88,$04,$b0,$cf,$f0,$8a
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$01,$92,$93
 db $88,$00,$b2,$e4,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$82,$93
 db $88,$04,$00,$f0,$b9
 db $10,$00,$00
 db $8c,$08,$00,$00
 db $10,$00,$92,$e4
 db $81,$06,$00,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$92,$e4
 db $88,$04,$00,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$92,$e4
 db $8c,$08,$b3,$70,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$82,$e4
 db $88,$00,$b1,$15,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$93,$70
 db $81,$06,$b3,$3f,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$83,$70
 db $88,$04,$00,$f0,$b9
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$93,$3f
 db $88,$00,$00,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$3f
 db $88,$04,$00,$f0,$b9
 db $10,$00,$00
 db $8c,$08,$00,$00
 db $10,$00,$93,$3f
 db $81,$06,$b3,$70,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$83,$3f
 db $88,$04,$b1,$15,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$93,$70
 db $8c,$08,$b3,$dc,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$83,$70
 db $88,$00,$00,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$dc
 db $81,$06,$b4,$55,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$dc
 db $88,$04,$b1,$15,$f0,$b9
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$01,$94,$55
 db $88,$00,$b1,$b8,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$84,$55
 db $88,$04,$b1,$b8,$f0,$92
 db $10,$00,$00
 db $8c,$08,$00,$00
 db $10,$01,$01
 db $81,$06,$b3,$70,$f0,$92
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$01
 db $88,$04,$b3,$3f,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$70
 db $8c,$08,$b1,$b8,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$93,$3f
 db $88,$00,$b1,$b8,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$83,$3f
 db $81,$06,$b2,$e4,$f0,$92
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$01
 db $88,$04,$b2,$93,$f0,$92
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$92,$e4
 db $88,$00,$b1,$b8,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$92,$93
 db $88,$04,$b1,$b8,$f0,$92
 db $10,$00,$00
 db $8c,$08,$00,$00
 db $10,$01,$82,$93
 db $81,$06,$b2,$e4,$f0,$92
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$01
 db $88,$04,$00,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$92,$e4
 db $8c,$08,$00,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$92,$e4
 db $88,$00,$b1,$b8,$f0,$92
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$92,$e4
 db $81,$06,$b3,$70,$f0,$92
 db $07,$00,$00
 db $10,$b3,$dc,$00
 db $10,$00,$93,$70
 db $88,$04,$b4,$55,$f0,$92
 db $10,$00,$00
 db $88,$04,$b4,$97,$00
 db $10,$00,$93,$70
 db $88,$00,$b5,$27,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$94,$55
 db $88,$04,$00,$f0,$a4
 db $10,$00,$00
 db $8c,$08,$00,$00
 db $10,$00,$95,$27
 db $81,$06,$00,$f0,$a4
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$95,$27
 db $88,$04,$b3,$70,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$95,$27
 db $8c,$08,$00,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$70
 db $88,$00,$00,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$70
 db $81,$06,$b3,$3f,$f0,$a4
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$70
 db $88,$04,$00,$f0,$a4
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$93,$3f
 db $88,$00,$00,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$3f
 db $88,$04,$b1,$ee,$f0,$a4
 db $10,$00,$00
 db $8c,$08,$00,$00
 db $10,$01,$93,$3f
 db $81,$06,$b1,$ee,$f0,$a4
 db $07,$00,$00
 db $10,$00,$00
 db $10,$01,$83,$3f
 db $88,$04,$b1,$ee,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$83,$3f
 db $8c,$08,$b1,$ee,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$01
 db $88,$00,$b1,$ee,$f0,$a4
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$01
 db $81,$06,$b1,$ee,$f0,$a4
 db $07,$00,$00
 db $10,$00,$00
 db $10,$01,$01
 db $88,$04,$b1,$ee,$f0,$a4
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$01,$01
 db $88,$00,$b3,$70,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$01
 db $88,$04,$b2,$2a,$f0,$b9
 db $10,$00,$00
 db $8c,$08,$00,$00
 db $10,$01,$93,$70
 db $81,$06,$b3,$3f,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$83,$70
 db $88,$04,$b2,$2a,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$93,$3f
 db $8c,$08,$b3,$70,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$83,$3f
 db $88,$00,$b2,$2a,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$93,$70
 db $81,$06,$b3,$3f,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$83,$70
 db $88,$04,$b3,$70,$f0,$b9
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$93,$3f
 db $88,$00,$b2,$2a,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$93,$70
 db $88,$04,$b3,$3f,$f0,$b9
 db $10,$00,$00
 db $8c,$08,$00,$00
 db $10,$00,$83,$70
 db $81,$06,$b3,$70,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$3f
 db $88,$04,$b2,$2a,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$93,$70
 db $8c,$08,$b3,$3f,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$83,$70
 db $88,$00,$00,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$3f
 db $81,$06,$b2,$93,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$3f
 db $88,$04,$b3,$70,$f0,$b9
 db $10,$b3,$dc,$00
 db $88,$04,$b4,$55,$00
 db $10,$b4,$97,$92,$93
 db $88,$00,$b5,$27,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$93,$70
 db $88,$04,$b2,$2a,$f0,$b9
 db $10,$00,$00
 db $8c,$08,$00,$00
 db $10,$01,$95,$27
 db $81,$06,$b3,$dc,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$85,$27
 db $88,$04,$b2,$2a,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$93,$dc
 db $8c,$08,$b4,$55,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$83,$dc
 db $88,$00,$b2,$2a,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$01,$94,$55
 db $81,$06,$b3,$dc,$f0,$b9
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$94,$55
 db $88,$04,$b2,$e4,$f0,$b9
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$93,$dc
 db $88,$00,$00,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$92,$e4
 db $10,$00,$f0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$92,$e4
 db $10,$a2,$e4,$e0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$82,$e4
 db $10,$00,$d0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$82,$e4
 db $10,$92,$e4,$c0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$b0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$01
 db $10,$82,$e4,$a0,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$01
 db $10,$00,$90,$b9
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$01
 db $88,$c1,$00,$b1,$72,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e1,$15
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$2a
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$cf,$00,$b1,$72,$91,$15
 db $10,$01,$01
 db $88,$00,$b1,$72,$b1,$15
 db $10,$01,$01
 db $88,$c1,$00,$b1,$72,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e1,$15
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$4b
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$e2,$93
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$00
 db $88,$00,$b1,$72,$e1,$25
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$25
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e1,$25
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$25
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$e1,$25
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$25
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$4b
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$25
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$00
 db $81,$45,$06,$f4,$97,$c0,$92
 db $07,$00,$00
 db $90,$03,$00,$00
 db $10,$00,$01
 db $88,$45,$00,$e4,$55,$c0,$92
 db $10,$00,$00
 db $90,$03,$f4,$97,$00
 db $10,$00,$01
 db $81,$45,$06,$d2,$e4,$b0,$92
 db $07,$00,$00
 db $90,$03,$e4,$55,$00
 db $10,$00,$01
 db $88,$45,$00,$c4,$97,$b0,$92
 db $10,$00,$00
 db $90,$03,$d2,$e4,$00
 db $10,$00,$01
 db $81,$45,$06,$b4,$55,$a0,$92
 db $07,$00,$00
 db $90,$03,$c4,$97,$00
 db $10,$00,$01
 db $88,$45,$00,$a2,$e4,$a0,$92
 db $10,$00,$00
 db $81,$43,$06,$b4,$55,$00
 db $07,$00,$01
 db $81,$45,$06,$94,$97,$90,$92
 db $07,$00,$00
 db $90,$03,$a2,$e4,$00
 db $10,$00,$01
 db $88,$45,$00,$84,$55,$90,$92
 db $10,$00,$00
 db $88,$43,$00,$94,$97,$00
 db $10,$00,$01
 db $88,$c1,$00,$b1,$72,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e1,$15
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$2a
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$cf,$00,$b1,$72,$e1,$15
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $88,$c1,$00,$00,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e1,$15
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$4b
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$e2,$93
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$00
 db $88,$00,$b1,$72,$e0,$f7
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b0,$f7
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e0,$f7
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b0,$f5
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$b2,$e4,$e0,$f5
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b0,$f5
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e1,$ea
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$ea
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$00
 db $81,$4d,$06,$90,$92,$b0,$92
 db $07,$00,$00
 db $10,$90,$f7,$00
 db $10,$00,$01
 db $88,$00,$a1,$15,$b0,$92
 db $10,$00,$00
 db $10,$a1,$25,$00
 db $10,$00,$90,$92
 db $81,$06,$b1,$ee,$b0,$92
 db $07,$00,$00
 db $10,$b2,$2a,$00
 db $10,$00,$91,$15
 db $88,$00,$c2,$4b,$b0,$92
 db $10,$00,$00
 db $10,$c3,$dc,$00
 db $10,$00,$91,$ee
 db $81,$06,$d4,$97,$b0,$92
 db $07,$00,$00
 db $10,$d4,$55,$00
 db $10,$00,$92,$4b
 db $88,$00,$e3,$dc,$b0,$92
 db $10,$00,$00
 db $10,$e2,$4b,$00
 db $10,$00,$94,$97
 db $81,$06,$f2,$2a,$b0,$92
 db $07,$00,$00
 db $81,$06,$f1,$ee,$00
 db $07,$00,$93,$dc
 db $81,$06,$f1,$25,$b0,$92
 db $07,$00,$00
 db $81,$06,$f1,$15,$00
 db $07,$00,$01
 db $88,$c1,$00,$b1,$72,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$2a
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b2,$2a
 db $10,$00,$00
 db $88,$00,$00,$00
 db $10,$00,$00
 db $88,$00,$b2,$e4,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$2a
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$cf,$00,$b1,$72,$92,$2a
 db $10,$00,$00
 db $88,$00,$00,$b2,$2a
 db $10,$00,$00
 db $88,$c1,$00,$00,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$2a
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b2,$2a
 db $10,$00,$00
 db $88,$00,$00,$00
 db $10,$00,$00
 db $88,$00,$b2,$e4,$e1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$4b
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$e2,$93
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$00
 db $88,$00,$b1,$72,$e1,$25
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b1,$25
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$4b
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b2,$4b
 db $10,$00,$00
 db $88,$00,$00,$00
 db $10,$00,$00
 db $88,$00,$b2,$e4,$e1,$25
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$25
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$4b
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$04,$00,$b2,$4b
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$00
 db $81,$45,$06,$f4,$97,$c0,$92
 db $07,$00,$00
 db $90,$03,$00,$00
 db $10,$00,$01
 db $88,$45,$00,$e4,$55,$c0,$92
 db $10,$00,$00
 db $90,$03,$f4,$97,$00
 db $10,$00,$01
 db $81,$45,$06,$d2,$e4,$b0,$92
 db $07,$00,$00
 db $90,$03,$e4,$55,$00
 db $10,$00,$01
 db $88,$45,$00,$c4,$97,$b0,$92
 db $10,$00,$00
 db $90,$03,$d2,$e4,$00
 db $10,$00,$01
 db $81,$45,$06,$b4,$55,$a0,$92
 db $07,$00,$00
 db $90,$03,$c4,$97,$00
 db $10,$00,$01
 db $88,$45,$00,$a2,$e4,$a0,$92
 db $10,$00,$00
 db $81,$43,$06,$b4,$55,$00
 db $07,$00,$01
 db $81,$45,$06,$94,$97,$90,$92
 db $07,$00,$00
 db $90,$03,$a2,$e4,$00
 db $10,$00,$01
 db $88,$45,$00,$84,$55,$90,$92
 db $10,$00,$00
 db $88,$43,$00,$94,$97,$00
 db $10,$00,$01
 db $81,$c1,$06,$b1,$72,$e1,$15
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$2a
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b2,$2a
 db $10,$00,$00
 db $88,$00,$00,$00
 db $10,$00,$00
 db $81,$06,$b2,$e4,$e1,$15
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e4,$55
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$cf,$00,$00,$e2,$2a
 db $10,$01,$01
 db $88,$00,$b2,$e4,$e2,$2a
 db $10,$01,$01
 db $81,$c1,$06,$b1,$72,$e1,$15
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$2a
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b2,$2a
 db $10,$00,$00
 db $88,$00,$00,$00
 db $10,$00,$00
 db $81,$06,$b2,$e4,$e1,$15
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$15
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e2,$4b
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$e2,$93
 db $10,$00,$00
 db $88,$04,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e1,$ee
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$ee
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e3,$dc
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b3,$da
 db $10,$00,$00
 db $81,$06,$00,$00
 db $07,$00,$00
 db $81,$06,$00,$e1,$ec
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $88,$00,$00,$b1,$ec
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $81,$06,$00,$e3,$d8
 db $07,$00,$00
 db $81,$06,$00,$00
 db $07,$00,$00
 db $81,$06,$00,$b3,$d8
 db $07,$00,$00
 db $81,$06,$00,$00
 db $07,$00,$00
 db $88,$4d,$0a,$90,$92,$b0,$92
 db $10,$00,$00
 db $88,$0a,$90,$f7,$00
 db $10,$00,$01
 db $88,$0a,$a1,$15,$b0,$92
 db $10,$00,$00
 db $10,$a1,$25,$00
 db $10,$00,$90,$92
 db $88,$0c,$b1,$ee,$b0,$92
 db $10,$00,$00
 db $10,$b2,$2a,$00
 db $10,$00,$91,$15
 db $88,$0e,$c2,$4b,$b0,$92
 db $10,$00,$00
 db $10,$c3,$dc,$00
 db $10,$00,$91,$ee
 db $88,$10,$d4,$97,$b0,$92
 db $10,$00,$00
 db $10,$d4,$55,$00
 db $10,$00,$92,$4b
 db $88,$12,$e3,$dc,$b0,$92
 db $10,$00,$00
 db $10,$e2,$4b,$00
 db $10,$00,$94,$97
 db $88,$14,$f2,$2a,$b0,$92
 db $10,$00,$00
 db $10,$f1,$ee,$00
 db $10,$00,$93,$dc
 db $88,$16,$f1,$25,$b0,$92
 db $10,$00,$00
 db $10,$f1,$15,$00
 db $10,$00,$01
 db $81,$c1,$06,$b1,$15,$f2,$e4
 db $07,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$f2,$0b
 db $10,$00,$f1,$ee
 db $10,$00,$f1,$d2
 db $10,$00,$f1,$b8
 db $10,$00,$f1,$88
 db $10,$00,$f1,$72
 db $10,$00,$f1,$5d
 db $10,$00,$f1,$49
 db $90,$05,$a1,$ee,$01
 db $10,$b1,$d2,$00
 db $10,$c1,$b8,$00
 db $10,$d1,$9f,$00
 db $10,$e1,$88,$00
 db $10,$f1,$72,$00
 db $10,$f1,$5d,$00
 db $10,$f1,$49,$00
 db $90,$03,$a1,$ee,$00
 db $10,$b1,$d2,$00
 db $10,$c1,$b8,$00
 db $10,$d1,$9f,$00
 db $10,$e1,$88,$00
 db $10,$f1,$72,$00
 db $10,$f1,$5d,$00
 db $10,$f1,$49,$00
.loop
 db $10,$01,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
 db $10,$00,$00
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
 dw .dsmp2+0
 db $06,$09,$40
.dp4
 dw .dsmp4+0
 db $01,$09,$40
.dp5
 dw .dsmp0+0
 db $02,$09,$00
.dp6
 dw .dsmp0+0
 db $02,$09,$08
.dp7
 dw .dsmp0+0
 db $02,$09,$10
.dp8
 dw .dsmp0+0
 db $02,$09,$18
.dp9
 dw .dsmp0+0
 db $02,$09,$20
.dp10
 dw .dsmp0+0
 db $02,$09,$28
.dp11
 dw .dsmp0+0
 db $02,$09,$30
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
.dsmp3
 db $50,$90,$0c,$6a,$04,$34,$21,$2c,$21,$90,$50,$40,$50,$48,$10,$0a
 db $80,$21,$40,$00,$00,$00,$00,$10,$00,$61,$10,$92,$a4,$00,$a4,$02
 db $04,$04,$24,$00,$02,$00,$40,$00,$40,$01,$01,$00,$48,$00,$21,$48
 db $21,$00,$00,$00,$00,$00,$40,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp4
 db $00,$01,$02,$02,$03,$02,$02,$01,$00,$01,$02,$02,$03,$02,$02,$01
 db $00,$01,$02,$02,$03,$02,$02,$01,$00,$01,$02,$02,$03,$02,$02,$01











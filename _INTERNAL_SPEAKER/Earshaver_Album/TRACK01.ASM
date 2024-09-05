	output	"TRACK01.BIN"

	org $6000
begin:


; 	EARSHAVER
; 	TRACK 1
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
 dw .drumpar
.song
 db $90, $cf, $00, $80, $5a, $f0, $58
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $80, $b9, $f0, $b7
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $43, $00, $f0, $b9, $01
 db $18, $01, $00
 db $18, $f0, $b9, $00
 db $18, $01, $00
 db $98, $8f, $f0, $b9, $f0, $b5
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $43, $00, $01, $f0, $b9
 db $18, $00, $01
 db $18, $00, $f0, $b9
 db $18, $00, $01
 db $98, $8f, $f0, $b9, $f0, $b7
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $43, $00, $f2, $e4, $f0, $b9
 db $18, $01, $01
 db $18, $f2, $e4, $f0, $b9
 db $18, $01, $01
 db $98, $8f, $f0, $b9, $f0, $b5
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f0, $5c, $e0, $58
 db $18, $00, $00
 db $18, $00, $b0, $5c
 db $18, $00, $00
 db $18, $f0, $b9, $b0, $b7
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f1, $72, $e1, $15
 db $18, $01, $01
 db $18, $f1, $72, $b1, $15
 db $18, $01, $01
 db $18, $f0, $b9, $b0, $b5
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f0, $5c, $b0, $58
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $f0, $b9, $b0, $b7
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f1, $72, $e2, $2a
 db $18, $01, $01
 db $18, $f1, $72, $b2, $2a
 db $18, $01, $01
 db $18, $f0, $b9, $b0, $b5
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f0, $5c, $b0, $58
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $f0, $b9, $b0, $b7
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $43, $00, $00, $01
 db $18, $01, $00
 db $18, $f0, $b9, $00
 db $18, $01, $00
 db $98, $8f, $f0, $b9, $b0, $b5
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $43, $00, $01, $f0, $b9
 db $18, $00, $01
 db $18, $00, $f0, $b9
 db $18, $00, $01
 db $98, $8f, $f0, $b9, $f0, $b7
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $43, $00, $f2, $e4, $f0, $b9
 db $18, $01, $01
 db $18, $f2, $e4, $f0, $b9
 db $18, $01, $01
 db $98, $8f, $f0, $b9, $f0, $b5
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f0, $5c, $e0, $58
 db $18, $00, $00
 db $18, $00, $b0, $5c
 db $18, $00, $00
 db $18, $f0, $b9, $b0, $b7
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f1, $72, $e1, $15
 db $18, $01, $01
 db $18, $f1, $72, $b1, $15
 db $18, $01, $01
 db $18, $f0, $b9, $b0, $b5
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f0, $5c, $b0, $58
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $f0, $b9, $b0, $b7
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f1, $72, $e2, $2a
 db $18, $01, $01
 db $18, $f1, $72, $b2, $2a
 db $18, $01, $01
 db $18, $f0, $b9, $b0, $b5
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f0, $6e, $b0, $6a
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $f0, $dc, $b0, $da
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $43, $00, $00, $01
 db $18, $01, $00
 db $18, $f0, $dc, $00
 db $18, $01, $00
 db $98, $8f, $f0, $dc, $b0, $d8
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $43, $00, $01, $f0, $dc
 db $18, $00, $01
 db $18, $00, $f0, $dc
 db $18, $00, $01
 db $98, $8f, $f0, $dc, $f0, $da
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $43, $00, $f3, $70, $f0, $dc
 db $18, $01, $01
 db $18, $f3, $70, $f0, $dc
 db $18, $01, $01
 db $98, $8f, $f0, $dc, $f0, $d8
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f0, $6e, $e0, $6a
 db $18, $00, $00
 db $18, $00, $b0, $6e
 db $18, $00, $00
 db $18, $f0, $dc, $b0, $da
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f1, $b8, $e1, $49
 db $18, $01, $01
 db $18, $f1, $b8, $b1, $49
 db $18, $01, $01
 db $18, $f0, $dc, $b0, $d8
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f0, $6e, $b0, $6a
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $f0, $dc, $b0, $da
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f1, $b8, $e2, $93
 db $18, $01, $01
 db $18, $f1, $b8, $b2, $93
 db $18, $01, $01
 db $18, $f0, $dc, $b0, $d8
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f0, $52, $b0, $4e
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $f0, $a4, $b0, $a2
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $43, $00, $00, $01
 db $18, $01, $00
 db $18, $f0, $a4, $00
 db $18, $01, $00
 db $98, $8f, $f0, $a4, $b0, $a0
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $43, $00, $01, $f0, $a4
 db $18, $00, $01
 db $18, $00, $f0, $a4
 db $18, $00, $01
 db $98, $8f, $f0, $a4, $f0, $a2
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $43, $00, $f2, $93, $f0, $a4
 db $18, $01, $01
 db $18, $f2, $93, $f0, $a4
 db $18, $01, $01
 db $98, $8f, $f0, $a4, $f0, $a0
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f0, $52, $e0, $4e
 db $18, $00, $00
 db $18, $00, $b0, $52
 db $18, $00, $00
 db $18, $f0, $a4, $b0, $a2
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f1, $49, $e0, $f7
 db $18, $01, $01
 db $18, $f1, $49, $b0, $f7
 db $18, $01, $01
 db $18, $f0, $a4, $b0, $a0
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f0, $52, $b0, $4e
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $f0, $a4, $b0, $a2
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f1, $49, $e1, $ee
 db $18, $01, $01
 db $18, $f1, $49, $b1, $ee
 db $18, $01, $01
 db $81, $02, $f0, $a4, $b0, $a0
 db $01, $00, $00
 db $16, $00, $00
 db $18, $00, $00
 db $90, $c1, $00, $80, $5a, $f0, $58
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $94, $04, $80, $b9, $f0, $b7
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $8c, $d1, $06, $92, $e2, $91, $72
 db $18, $01, $01
 db $18, $92, $e2, $91, $72
 db $18, $01, $01
 db $94, $c1, $04, $90, $b9, $80, $b5
 db $18, $00, $00
 db $90, $00, $00, $00
 db $18, $00, $00
 db $90, $43, $00, $f2, $e0, $80, $b9
 db $18, $01, $01
 db $18, $f2, $e0, $80, $b9
 db $18, $01, $01
 db $90, $c1, $00, $80, $b9, $80, $b7
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $8c, $d1, $06, $92, $e0, $81, $72
 db $18, $01, $01
 db $18, $92, $e0, $81, $72
 db $18, $01, $01
 db $94, $c1, $04, $90, $b9, $80, $b5
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $94, $04, $90, $5c, $e0, $58
 db $18, $00, $00
 db $18, $00, $b0, $5c
 db $18, $00, $00
 db $90, $00, $80, $b9, $e0, $b7
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $8c, $06, $81, $72, $e1, $15
 db $18, $01, $01
 db $90, $00, $81, $72, $b1, $15
 db $18, $01, $01
 db $94, $04, $80, $b9, $e0, $b5
 db $18, $00, $00
 db $90, $00, $00, $00
 db $18, $00, $00
 db $90, $00, $80, $5c, $f0, $58
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $80, $b9, $f0, $b7
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $8c, $06, $81, $72, $e2, $2a
 db $18, $01, $01
 db $18, $81, $72, $b2, $2a
 db $18, $01, $01
 db $90, $00, $80, $b9, $f0, $b5
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $80, $5c, $f0, $58
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $94, $04, $80, $b9, $f0, $b7
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $8c, $d1, $06, $92, $e2, $91, $72
 db $18, $01, $01
 db $18, $92, $e2, $91, $72
 db $18, $01, $01
 db $94, $c1, $04, $90, $b9, $90, $b5
 db $18, $00, $00
 db $90, $00, $00, $00
 db $18, $00, $00
 db $90, $43, $00, $f2, $e0, $80, $b9
 db $18, $01, $01
 db $18, $f2, $e0, $80, $b9
 db $18, $01, $01
 db $90, $c1, $00, $f0, $b9, $80, $b7
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $8c, $d1, $06, $92, $e0, $81, $72
 db $18, $01, $01
 db $18, $92, $e0, $81, $72
 db $18, $01, $01
 db $94, $c1, $04, $90, $b9, $80, $b5
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $94, $04, $90, $5c, $e0, $58
 db $18, $00, $00
 db $18, $00, $b0, $5c
 db $18, $00, $00
 db $90, $00, $90, $b9, $b0, $b7
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $8c, $06, $91, $72, $e1, $15
 db $18, $01, $01
 db $90, $00, $91, $72, $b1, $15
 db $18, $01, $01
 db $94, $04, $90, $b9, $b0, $b5
 db $18, $00, $00
 db $90, $00, $00, $00
 db $18, $00, $00
 db $90, $00, $90, $5c, $b0, $58
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $90, $b9, $b0, $b7
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $8c, $06, $91, $72, $e2, $2a
 db $18, $01, $01
 db $18, $91, $72, $b2, $2a
 db $18, $01, $01
 db $90, $00, $90, $b9, $b0, $b5
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $80, $6e, $b0, $6a
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $94, $04, $80, $dc, $b0, $da
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $8c, $d1, $06, $93, $6e, $91, $b8
 db $18, $01, $01
 db $18, $93, $6e, $91, $b8
 db $18, $01, $01
 db $94, $c1, $04, $90, $dc, $90, $d8
 db $18, $00, $00
 db $90, $00, $00, $00
 db $18, $00, $00
 db $90, $43, $00, $f3, $6c, $80, $dc
 db $18, $01, $01
 db $18, $f3, $6c, $80, $dc
 db $18, $01, $01
 db $90, $c1, $00, $f0, $dc, $80, $da
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $8c, $d1, $06, $f3, $6c, $81, $b8
 db $18, $01, $01
 db $18, $f3, $6c, $81, $b8
 db $18, $01, $01
 db $94, $c1, $04, $f0, $dc, $80, $d8
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $94, $04, $f0, $6e, $e0, $6a
 db $18, $00, $00
 db $18, $00, $b0, $6e
 db $18, $00, $00
 db $90, $00, $f0, $dc, $b0, $da
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $8c, $06, $f1, $b8, $e1, $49
 db $18, $01, $01
 db $90, $00, $f1, $b8, $b1, $49
 db $18, $01, $01
 db $94, $04, $f0, $dc, $b0, $d8
 db $18, $00, $00
 db $90, $00, $00, $00
 db $18, $00, $00
 db $90, $00, $f0, $6e, $b0, $6a
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f0, $dc, $b0, $da
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $8c, $06, $f1, $b8, $e2, $93
 db $18, $01, $01
 db $18, $f1, $b8, $b2, $93
 db $18, $01, $01
 db $90, $00, $f0, $dc, $b0, $d8
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $80, $52, $b0, $4e
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $94, $04, $80, $a4, $b0, $a2
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $8c, $d1, $06, $92, $91, $91, $49
 db $18, $01, $01
 db $18, $92, $91, $91, $49
 db $18, $01, $01
 db $94, $c1, $04, $90, $a4, $90, $a0
 db $18, $00, $00
 db $90, $00, $00, $00
 db $18, $00, $00
 db $90, $43, $00, $f2, $8f, $80, $a4
 db $18, $01, $01
 db $18, $f2, $8f, $80, $a4
 db $18, $01, $01
 db $90, $c1, $00, $f0, $a4, $80, $a2
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $8c, $d1, $06, $f2, $8f, $81, $49
 db $18, $01, $01
 db $18, $f2, $8f, $81, $49
 db $18, $01, $01
 db $94, $c1, $04, $f0, $a4, $80, $a0
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $94, $04, $f0, $52, $e0, $4e
 db $18, $00, $00
 db $18, $00, $b0, $52
 db $18, $00, $00
 db $90, $00, $f0, $a4, $b0, $a2
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $8c, $06, $f1, $49, $e0, $f7
 db $18, $01, $01
 db $90, $00, $f1, $49, $b0, $f7
 db $18, $01, $01
 db $94, $04, $f0, $a4, $b0, $a0
 db $18, $00, $00
 db $90, $00, $00, $00
 db $18, $00, $00
 db $90, $00, $f0, $52, $b0, $4e
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $90, $00, $f0, $a4, $b0, $a2
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $8c, $06, $f1, $49, $e1, $ee
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $e1, $d2
 db $18, $00, $e1, $b8
 db $18, $00, $e1, $9f
 db $18, $00, $e1, $88
 db $90, $d1, $00, $85, $c9, $c0, $b9
 db $18, $00, $01
 db $18, $84, $55, $e0, $b9
 db $18, $00, $01
 db $90, $00, $83, $70, $c0, $b9
 db $18, $00, $00
 db $94, $08, $82, $e4, $01
 db $18, $00, $00
 db $8c, $0a, $95, $c9, $c0, $b9
 db $18, $00, $01
 db $94, $08, $94, $55, $e0, $b9
 db $18, $00, $01
 db $90, $0c, $93, $70, $c1, $72
 db $18, $00, $00
 db $90, $0c, $92, $e4, $c1, $49
 db $18, $00, $00
 db $18, $a1, $72, $d0, $b9
 db $18, $00, $01
 db $90, $0c, $a1, $49, $b0, $b9
 db $18, $00, $01
 db $90, $0c, $a3, $70, $d1, $72
 db $18, $00, $00
 db $94, $08, $a2, $e4, $01
 db $18, $00, $00
 db $8c, $0a, $b1, $72, $d1, $49
 db $18, $00, $00
 db $94, $08, $b4, $55, $b0, $b9
 db $18, $00, $01
 db $18, $b1, $49, $d1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $e4, $01
 db $18, $00, $00
 db $90, $00, $c1, $72, $c0, $b9
 db $18, $00, $01
 db $18, $c4, $55, $a0, $b9
 db $18, $00, $01
 db $90, $00, $c3, $70, $c0, $b9
 db $18, $00, $00
 db $94, $0e, $c2, $e4, $01
 db $18, $00, $00
 db $8c, $10, $d5, $c9, $c0, $b9
 db $18, $00, $01
 db $94, $0e, $d4, $55, $a0, $b9
 db $18, $00, $01
 db $90, $0c, $d3, $70, $c1, $72
 db $18, $00, $00
 db $90, $0c, $d2, $e4, $c1, $49
 db $18, $00, $00
 db $18, $e1, $72, $d0, $b9
 db $18, $00, $01
 db $90, $0c, $e1, $49, $c0, $b9
 db $18, $00, $01
 db $90, $0c, $e3, $70, $d1, $72
 db $18, $00, $00
 db $94, $0e, $e2, $e4, $01
 db $18, $00, $00
 db $8c, $10, $f1, $72, $d1, $49
 db $18, $00, $00
 db $94, $0e, $f4, $55, $c0, $b9
 db $18, $00, $01
 db $8c, $10, $f1, $49, $d0, $b9
 db $18, $00, $00
 db $18, $f2, $e4, $01
 db $18, $00, $00
 db $90, $00, $85, $c9, $c0, $b9
 db $18, $00, $01
 db $18, $84, $55, $e0, $b9
 db $18, $00, $01
 db $90, $00, $83, $70, $c0, $b9
 db $18, $00, $00
 db $94, $12, $82, $e4, $01
 db $18, $00, $00
 db $8c, $14, $95, $c9, $c0, $b9
 db $18, $00, $01
 db $94, $12, $94, $55, $e0, $b9
 db $18, $00, $01
 db $90, $0c, $93, $70, $c1, $72
 db $18, $00, $00
 db $90, $0c, $92, $e4, $c1, $49
 db $18, $00, $00
 db $18, $a1, $72, $d0, $b9
 db $18, $00, $01
 db $90, $0c, $a1, $49, $b0, $b9
 db $18, $00, $01
 db $90, $0c, $a3, $70, $d1, $72
 db $18, $00, $00
 db $94, $12, $a2, $e4, $01
 db $18, $00, $00
 db $8c, $14, $b1, $72, $d1, $49
 db $18, $00, $00
 db $94, $12, $b4, $55, $b0, $b9
 db $18, $00, $01
 db $18, $b1, $49, $d1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $e4, $01
 db $18, $00, $00
 db $90, $00, $c1, $72, $c0, $b9
 db $18, $00, $01
 db $18, $c4, $55, $a0, $b9
 db $18, $00, $01
 db $90, $00, $c3, $70, $c0, $b9
 db $18, $00, $00
 db $94, $16, $c2, $e4, $01
 db $18, $00, $00
 db $8c, $18, $d5, $c9, $c0, $b9
 db $18, $00, $01
 db $94, $16, $d4, $55, $a0, $b9
 db $18, $00, $01
 db $90, $0c, $d3, $70, $c1, $72
 db $18, $00, $00
 db $90, $0c, $d2, $e4, $c1, $b8
 db $18, $00, $00
 db $18, $e1, $72, $d1, $b8
 db $18, $00, $00
 db $90, $0c, $e1, $b8, $c0, $b9
 db $18, $00, $01
 db $90, $0c, $e3, $70, $d1, $9f
 db $18, $00, $00
 db $94, $16, $e2, $e4, $01
 db $18, $00, $00
 db $8c, $18, $f1, $9f, $d1, $72
 db $18, $00, $00
 db $94, $16, $f4, $55, $c1, $72
 db $18, $00, $00
 db $8c, $18, $f1, $72, $d0, $b9
 db $18, $00, $00
 db $18, $f2, $e4, $01
 db $18, $00, $00
 db $90, $00, $85, $c9, $c0, $92
 db $18, $00, $01
 db $18, $84, $55, $e0, $92
 db $18, $00, $01
 db $90, $00, $83, $70, $c0, $92
 db $18, $00, $00
 db $94, $1a, $82, $e4, $01
 db $18, $00, $00
 db $8c, $1c, $95, $c9, $c0, $92
 db $18, $00, $01
 db $94, $1a, $94, $55, $e0, $92
 db $18, $00, $01
 db $90, $0c, $92, $e4, $c1, $72
 db $18, $00, $00
 db $90, $0c, $92, $93, $c1, $49
 db $18, $00, $00
 db $18, $a1, $72, $d0, $92
 db $18, $00, $01
 db $90, $0c, $a1, $49, $b0, $92
 db $18, $00, $01
 db $90, $0c, $a2, $e4, $d1, $72
 db $18, $00, $00
 db $94, $1a, $00, $01
 db $18, $00, $00
 db $8c, $1c, $b2, $93, $d1, $49
 db $18, $00, $00
 db $94, $1a, $b4, $55, $b0, $92
 db $18, $00, $01
 db $18, $b2, $e4, $d1, $72
 db $18, $00, $00
 db $90, $0c, $00, $01
 db $18, $00, $00
 db $90, $00, $c1, $72, $c0, $92
 db $18, $00, $01
 db $18, $c4, $55, $a0, $92
 db $18, $00, $01
 db $90, $00, $c3, $70, $c0, $92
 db $18, $00, $00
 db $94, $1e, $c2, $e4, $01
 db $18, $00, $00
 db $8c, $20, $d5, $c9, $c0, $92
 db $18, $00, $01
 db $94, $1e, $d4, $55, $a0, $92
 db $18, $00, $01
 db $90, $0c, $d2, $e4, $c1, $72
 db $18, $00, $00
 db $90, $0c, $d2, $93, $c1, $49
 db $18, $00, $00
 db $18, $e1, $72, $d0, $92
 db $18, $00, $01
 db $90, $0c, $e1, $49, $c0, $92
 db $18, $00, $01
 db $90, $0c, $e2, $e4, $d1, $72
 db $18, $00, $00
 db $94, $1e, $00, $01
 db $18, $00, $00
 db $8c, $20, $f2, $93, $d1, $49
 db $18, $00, $00
 db $94, $1e, $f4, $55, $c0, $92
 db $18, $00, $01
 db $8c, $20, $f1, $49, $d0, $92
 db $18, $00, $00
 db $18, $f2, $e4, $01
 db $18, $00, $00
 db $90, $00, $84, $55, $c0, $8a
 db $18, $00, $01
 db $18, $83, $3f, $e0, $8a
 db $18, $00, $01
 db $90, $00, $82, $93, $c0, $8a
 db $18, $00, $00
 db $94, $22, $82, $2a, $01
 db $18, $00, $00
 db $8c, $24, $94, $55, $c0, $8a
 db $18, $00, $01
 db $94, $22, $93, $3f, $e0, $8a
 db $18, $00, $01
 db $90, $0c, $92, $e4, $c1, $72
 db $18, $00, $00
 db $90, $0c, $92, $93, $c1, $49
 db $18, $00, $00
 db $18, $a1, $72, $d0, $8a
 db $18, $00, $01
 db $90, $0c, $a1, $49, $b0, $8a
 db $18, $00, $01
 db $90, $0c, $a2, $e4, $d1, $72
 db $18, $00, $00
 db $94, $22, $a2, $2a, $01
 db $18, $00, $00
 db $8c, $24, $b3, $70, $d1, $b8
 db $18, $00, $00
 db $94, $22, $b2, $e4, $b0, $8a
 db $18, $00, $01
 db $18, $b1, $49, $d1, $49
 db $18, $00, $00
 db $90, $0c, $b3, $70, $01
 db $18, $00, $00
 db $90, $00, $c1, $49, $c0, $8a
 db $18, $00, $01
 db $18, $c3, $3f, $a0, $8a
 db $18, $00, $01
 db $90, $00, $c2, $93, $c0, $8a
 db $18, $00, $00
 db $94, $26, $c2, $2a, $01
 db $18, $00, $00
 db $8c, $28, $d4, $55, $c0, $8a
 db $18, $00, $01
 db $94, $26, $d3, $3f, $a0, $8a
 db $18, $00, $01
 db $90, $0c, $d3, $70, $c1, $b8
 db $18, $00, $01
 db $90, $0c, $d4, $17, $c2, $0b
 db $18, $d4, $55, $c2, $2a
 db $18, $e1, $b8, $d2, $2a
 db $18, $00, $00
 db $90, $0c, $e2, $0b, $c0, $8a
 db $18, $e2, $2a, $01
 db $90, $0c, $e4, $97, $d2, $4b
 db $18, $00, $00
 db $94, $26, $e2, $2a, $00
 db $18, $00, $00
 db $8c, $28, $f4, $55, $d2, $2a
 db $18, $00, $00
 db $94, $26, $f3, $3f, $c2, $2a
 db $18, $00, $00
 db $84, $2a, $f2, $2a, $d0, $8a
 db $18, $00, $00
 db $84, $2a, $00, $01
 db $18, $00, $00
 db $90, $00, $c0, $b9, $f5, $c9
 db $98, $03, $01, $f0, $b9
 db $98, $91, $e0, $b9, $f4, $55
 db $98, $03, $e5, $c9, $f0, $b9
 db $94, $d1, $04, $c0, $b9, $f3, $70
 db $98, $03, $00, $f0, $b9
 db $94, $d1, $04, $c4, $55, $f2, $e4
 db $18, $00, $00
 db $84, $2a, $c0, $b9, $e5, $c9
 db $98, $03, $01, $e0, $b9
 db $94, $d1, $04, $e0, $b9, $e4, $55
 db $98, $03, $e5, $c9, $e0, $b9
 db $8c, $cb, $18, $82, $e2, $82, $e4
 db $18, $00, $00
 db $8c, $18, $82, $91, $82, $93
 db $18, $00, $00
 db $90, $d1, $00, $d2, $e4, $d5, $c9
 db $98, $03, $00, $d0, $b9
 db $90, $d1, $0c, $b2, $93, $d4, $55
 db $98, $03, $00, $d0, $b9
 db $90, $cb, $0c, $82, $e2, $82, $e4
 db $18, $00, $00
 db $94, $d1, $04, $84, $55, $00
 db $18, $00, $00
 db $84, $cb, $2a, $82, $91, $82, $93
 db $18, $00, $00
 db $94, $d1, $04, $b2, $e4, $84, $55
 db $98, $03, $00, $80, $b9
 db $90, $cb, $0c, $82, $e2, $82, $e4
 db $18, $00, $00
 db $90, $d1, $0c, $82, $93, $00
 db $18, $00, $00
 db $90, $00, $c2, $e4, $c5, $c9
 db $98, $03, $00, $c0, $b9
 db $98, $91, $a0, $b9, $c4, $55
 db $98, $03, $a5, $c9, $c0, $b9
 db $94, $d1, $04, $c0, $b9, $c3, $70
 db $98, $03, $00, $c0, $b9
 db $94, $d1, $04, $c4, $55, $c2, $e4
 db $18, $00, $00
 db $84, $2a, $c0, $b9, $b5, $c9
 db $98, $03, $01, $b0, $b9
 db $94, $d1, $04, $a0, $b9, $b4, $55
 db $98, $03, $a5, $c9, $b0, $b9
 db $8c, $cb, $20, $82, $e2, $82, $e4
 db $18, $00, $00
 db $8c, $20, $82, $91, $82, $93
 db $18, $00, $00
 db $90, $d1, $00, $d2, $e4, $a5, $c9
 db $98, $03, $00, $a0, $b9
 db $90, $d1, $0c, $c2, $93, $a4, $55
 db $98, $03, $00, $a0, $b9
 db $90, $cb, $0c, $82, $e2, $82, $e4
 db $18, $00, $00
 db $94, $d1, $04, $84, $55, $00
 db $18, $00, $00
 db $84, $cb, $2a, $82, $91, $82, $93
 db $18, $00, $00
 db $94, $d1, $04, $c2, $e4, $94, $55
 db $98, $03, $00, $90, $b9
 db $8c, $d1, $06, $d2, $93, $93, $70
 db $98, $03, $00, $90, $b9
 db $8c, $d1, $06, $d4, $55, $92, $e4
 db $18, $00, $00
 db $90, $00, $c0, $b9, $85, $c9
 db $98, $03, $01, $80, $b9
 db $98, $91, $e0, $b9, $84, $55
 db $98, $03, $e5, $c9, $80, $b9
 db $94, $d1, $04, $c0, $b9, $83, $70
 db $98, $03, $00, $80, $b9
 db $94, $d1, $04, $c4, $55, $82, $e4
 db $18, $00, $00
 db $84, $2a, $c0, $b9, $95, $c9
 db $98, $03, $01, $90, $b9
 db $94, $d1, $04, $e0, $b9, $94, $55
 db $98, $03, $e5, $c9, $90, $b9
 db $8c, $cb, $18, $82, $e2, $82, $e4
 db $18, $00, $00
 db $8c, $18, $82, $91, $82, $93
 db $18, $00, $00
 db $90, $d1, $00, $d2, $e4, $a5, $c9
 db $98, $03, $00, $a0, $b9
 db $90, $d1, $0c, $b2, $93, $a4, $55
 db $98, $03, $00, $a0, $b9
 db $90, $cb, $0c, $82, $e2, $82, $e4
 db $18, $00, $00
 db $94, $d1, $04, $84, $55, $00
 db $18, $00, $00
 db $84, $cb, $2a, $82, $91, $82, $93
 db $18, $00, $00
 db $94, $d1, $04, $b2, $e4, $84, $55
 db $98, $03, $00, $80, $b9
 db $90, $cb, $0c, $82, $e2, $82, $e4
 db $18, $00, $00
 db $90, $d1, $0c, $82, $93, $00
 db $18, $00, $00
 db $90, $00, $c2, $e4, $c5, $c9
 db $98, $03, $00, $c0, $b9
 db $98, $91, $a0, $b9, $c4, $55
 db $98, $03, $a5, $c9, $c0, $b9
 db $94, $d1, $04, $c0, $b9, $c3, $70
 db $98, $03, $00, $c0, $b9
 db $94, $d1, $04, $c4, $55, $c2, $e4
 db $18, $00, $00
 db $84, $2a, $c0, $b9, $d5, $c9
 db $98, $03, $01, $d0, $b9
 db $94, $d1, $04, $a0, $b9, $d4, $55
 db $98, $03, $a5, $c9, $d0, $b9
 db $8c, $cb, $20, $82, $e2, $82, $e4
 db $18, $00, $00
 db $8c, $20, $83, $70, $83, $70
 db $18, $00, $00
 db $90, $d1, $00, $d2, $e4, $e5, $c9
 db $98, $03, $00, $e0, $b9
 db $90, $d1, $0c, $c3, $70, $e4, $55
 db $98, $03, $00, $e0, $b9
 db $90, $cb, $0c, $83, $3d, $83, $3f
 db $18, $00, $00
 db $94, $d1, $04, $84, $55, $82, $e4
 db $18, $00, $00
 db $84, $cb, $2a, $82, $e2, $00
 db $18, $00, $00
 db $94, $d1, $04, $c3, $3f, $84, $55
 db $18, $00, $80, $b9
 db $8c, $cb, $06, $82, $91, $82, $93
 db $98, $03, $00, $00
 db $8c, $d1, $06, $82, $49, $82, $4b
 db $18, $00, $00
 db $90, $00, $c2, $93, $85, $c9
 db $98, $03, $00, $80, $92
 db $98, $91, $e2, $4b, $84, $55
 db $98, $03, $00, $80, $92
 db $94, $d1, $04, $c0, $92, $83, $70
 db $98, $03, $00, $80, $92
 db $94, $d1, $04, $c4, $55, $82, $e4
 db $18, $00, $00
 db $84, $2a, $c0, $92, $95, $c9
 db $98, $03, $01, $90, $92
 db $94, $d1, $04, $e0, $92, $94, $55
 db $98, $03, $e5, $c9, $90, $92
 db $8c, $cb, $18, $82, $e2, $82, $e4
 db $18, $00, $00
 db $8c, $18, $82, $91, $82, $93
 db $18, $00, $00
 db $90, $d1, $00, $d2, $e4, $a5, $c9
 db $98, $03, $00, $a0, $92
 db $90, $d1, $0c, $b2, $93, $a4, $55
 db $98, $03, $00, $a0, $92
 db $90, $cb, $0c, $82, $e2, $82, $e4
 db $18, $00, $00
 db $94, $d1, $04, $84, $55, $00
 db $18, $00, $00
 db $84, $cb, $2a, $82, $91, $82, $93
 db $18, $00, $00
 db $94, $d1, $04, $b2, $e4, $84, $55
 db $18, $00, $80, $92
 db $90, $cb, $0c, $82, $e2, $82, $e4
 db $18, $00, $00
 db $90, $d1, $0c, $82, $93, $00
 db $18, $00, $00
 db $90, $00, $c2, $e4, $c5, $c9
 db $18, $00, $c0, $92
 db $18, $a0, $92, $c4, $55
 db $18, $a5, $c9, $c0, $92
 db $94, $04, $c0, $92, $c3, $70
 db $18, $00, $c0, $92
 db $94, $04, $c4, $55, $c2, $e4
 db $18, $00, $00
 db $84, $2a, $c0, $92, $d5, $c9
 db $18, $01, $d0, $92
 db $94, $04, $a0, $92, $d4, $55
 db $18, $a5, $c9, $d0, $92
 db $8c, $cb, $20, $82, $e2, $82, $e4
 db $18, $00, $00
 db $8c, $20, $82, $91, $82, $93
 db $18, $00, $00
 db $90, $d1, $00, $d2, $e4, $e5, $c9
 db $18, $00, $e0, $92
 db $90, $0c, $c2, $93, $e4, $55
 db $18, $00, $e0, $92
 db $90, $cb, $0c, $d2, $e2, $82, $e4
 db $18, $00, $00
 db $94, $d1, $04, $d2, $e4, $00
 db $18, $00, $00
 db $84, $cb, $2a, $d2, $91, $82, $93
 db $18, $00, $00
 db $94, $d1, $04, $c2, $e4, $84, $55
 db $18, $00, $80, $92
 db $8c, $06, $d2, $93, $83, $70
 db $18, $00, $80, $92
 db $8c, $06, $d4, $55, $82, $e4
 db $18, $00, $00
 db $90, $00, $c0, $b9, $84, $55
 db $18, $01, $80, $8a
 db $18, $e0, $b9, $83, $3f
 db $18, $e5, $c9, $80, $8a
 db $94, $04, $c0, $b9, $82, $93
 db $18, $00, $80, $8a
 db $94, $04, $c4, $55, $82, $2a
 db $18, $00, $00
 db $84, $2a, $c0, $b9, $94, $55
 db $18, $01, $90, $8a
 db $94, $04, $e0, $b9, $93, $3f
 db $18, $e5, $c9, $90, $8a
 db $8c, $cb, $18, $82, $e2, $82, $e4
 db $18, $00, $00
 db $8c, $18, $82, $91, $82, $93
 db $18, $00, $00
 db $90, $d1, $00, $d2, $e4, $a4, $55
 db $18, $00, $a0, $8a
 db $90, $0c, $b2, $93, $a3, $3f
 db $18, $00, $a0, $8a
 db $90, $cb, $0c, $82, $e2, $82, $e4
 db $18, $00, $00
 db $94, $d1, $04, $84, $55, $82, $2a
 db $18, $00, $00
 db $84, $cb, $2a, $83, $6e, $83, $70
 db $18, $00, $00
 db $94, $d1, $04, $b2, $e4, $83, $3f
 db $18, $00, $80, $8a
 db $90, $cb, $0c, $81, $47, $81, $49
 db $18, $00, $00
 db $90, $d1, $0c, $83, $70, $82, $2a
 db $18, $00, $00
 db $90, $00, $c1, $49, $c4, $55
 db $98, $03, $00, $c0, $8a
 db $98, $91, $a0, $b9, $c3, $3f
 db $98, $03, $a5, $c9, $c0, $8a
 db $98, $91, $c0, $b9, $c2, $93
 db $98, $03, $00, $c0, $8a
 db $98, $91, $c4, $55, $c2, $2a
 db $18, $00, $00
 db $90, $00, $c0, $b9, $d4, $55
 db $98, $03, $01, $d0, $8a
 db $98, $91, $a0, $b9, $d3, $3f
 db $98, $03, $a5, $c9, $d0, $8a
 db $98, $8b, $83, $6e, $83, $70
 db $18, $00, $01
 db $18, $84, $15, $83, $a5
 db $18, $84, $53, $83, $dc
 db $90, $00, $00, $e4, $17
 db $18, $00, $e4, $55
 db $18, $00, $00
 db $18, $00, $00
 db $18, $84, $95, $e4, $97
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $81, $02, $84, $53, $f4, $55
 db $01, $00, $00
 db $16, $00, $00
 db $18, $00, $00
 db $98, $91, $d4, $97, $01
 db $18, $00, $00
 db $18, $d4, $55, $00
 db $18, $00, $00
 db $90, $c1, $00, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $18, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $18, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $18, $e1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $18, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $18, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $18, $e1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e2, $4b
 db $18, $00, $00
 db $18, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e2, $4b
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $92
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $92
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e2, $4b
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $18, $b1, $ee, $e2, $4b
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $92
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $92
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e2, $4b
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b2, $4b
 db $18, $00, $00
 db $18, $e1, $ee, $e2, $4b
 db $18, $00, $00
 db $90, $0c, $e2, $2a, $b2, $4b
 db $18, $00, $00
 db $90, $00, $b1, $ea, $e2, $2a
 db $18, $b2, $26, $00
 db $18, $00, $b2, $2a
 db $18, $00, $00
 db $90, $00, $00, $e2, $2a
 db $18, $00, $00
 db $94, $04, $b1, $ee, $b2, $2a
 db $18, $00, $00
 db $84, $2a, $b1, $13, $e1, $15
 db $18, $00, $00
 db $94, $04, $00, $b1, $15
 db $18, $00, $00
 db $90, $0c, $b1, $b8, $e2, $2a
 db $18, $00, $00
 db $90, $0c, $00, $b2, $2a
 db $18, $00, $00
 db $18, $b1, $13, $e1, $15
 db $18, $00, $00
 db $90, $0c, $b1, $9f, $b2, $2a
 db $18, $00, $00
 db $90, $0c, $00, $e2, $2a
 db $18, $00, $00
 db $94, $04, $b1, $13, $b1, $15
 db $18, $00, $00
 db $84, $2a, $b1, $72, $e2, $2a
 db $18, $00, $00
 db $90, $00, $00, $b2, $2a
 db $18, $00, $00
 db $84, $2a, $b1, $49, $e2, $2a
 db $18, $00, $00
 db $84, $2a, $00, $b2, $2a
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $18, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $18, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $18, $e1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $18, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $18, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $18, $e1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e2, $4b
 db $18, $00, $00
 db $18, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e2, $4b
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $92
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $92
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e2, $4b
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $18, $b1, $ee, $e2, $4b
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $92
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $92
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e2, $4b
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b2, $4b
 db $18, $00, $00
 db $18, $e1, $ee, $e2, $4b
 db $18, $00, $00
 db $90, $0c, $e2, $2a, $b2, $4b
 db $18, $00, $00
 db $90, $00, $b1, $ea, $e2, $93
 db $18, $b2, $26, $00
 db $18, $00, $b2, $93
 db $18, $00, $00
 db $90, $00, $00, $e2, $93
 db $18, $00, $00
 db $94, $04, $b1, $ee, $b2, $93
 db $18, $00, $00
 db $84, $2a, $b1, $13, $e1, $49
 db $18, $00, $00
 db $94, $04, $00, $b1, $49
 db $18, $00, $00
 db $90, $0c, $b1, $b8, $e2, $93
 db $18, $00, $00
 db $90, $0c, $00, $b2, $93
 db $18, $00, $00
 db $18, $b1, $13, $e1, $49
 db $18, $00, $00
 db $84, $2a, $b1, $9f, $b2, $93
 db $18, $00, $00
 db $90, $00, $00, $e2, $93
 db $18, $00, $00
 db $94, $04, $b1, $13, $b1, $49
 db $18, $00, $00
 db $84, $2a, $b1, $72, $e2, $93
 db $18, $00, $00
 db $84, $2a, $00, $b2, $93
 db $18, $00, $00
 db $84, $2a, $b1, $49, $e2, $93
 db $18, $00, $00
 db $84, $2a, $00, $b2, $93
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $94, $04, $e1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $84, $2a, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $e1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e2, $4b
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e2, $4b
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $92
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $92
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e2, $4b
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e2, $4b
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $92
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $92
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e2, $4b
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b2, $4b
 db $18, $00, $00
 db $94, $04, $e1, $47, $e1, $25
 db $18, $00, $00
 db $90, $0c, $e1, $70, $b1, $25
 db $18, $00, $00
 db $90, $00, $e2, $28, $e2, $2a
 db $18, $00, $00
 db $94, $04, $00, $b2, $2a
 db $18, $00, $00
 db $90, $00, $00, $e1, $15
 db $18, $00, $00
 db $94, $04, $e1, $47, $b1, $15
 db $18, $00, $00
 db $84, $2a, $e1, $ec, $e1, $ee
 db $18, $00, $00
 db $94, $04, $00, $b1, $ee
 db $18, $00, $00
 db $90, $0c, $e1, $b6, $e1, $b8
 db $18, $00, $00
 db $84, $2a, $00, $b1, $b8
 db $18, $00, $00
 db $90, $00, $00, $e2, $2a
 db $18, $00, $00
 db $90, $0c, $e1, $47, $b1, $15
 db $18, $00, $00
 db $90, $0c, $00, $e1, $49
 db $18, $00, $00
 db $94, $04, $00, $b2, $2a
 db $18, $00, $00
 db $84, $2a, $e1, $70, $e1, $72
 db $18, $00, $00
 db $94, $04, $00, $b2, $2a
 db $18, $00, $00
 db $84, $2a, $e1, $9d, $e1, $9f
 db $18, $00, $00
 db $84, $2a, $00, $b2, $2a
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $94, $04, $e1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $84, $2a, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $e1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e2, $4b
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e2, $4b
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $92
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $92
 db $18, $00, $00
 db $90, $c1, $0c, $b1, $b6, $e2, $4b
 db $18, $00, $00
 db $90, $0c, $00, $b2, $4b
 db $18, $00, $00
 db $90, $00, $00, $e1, $25
 db $18, $00, $00
 db $90, $0c, $00, $b1, $25
 db $18, $00, $00
 db $90, $0c, $00, $e2, $4b
 db $18, $00, $00
 db $94, $04, $00, $b2, $4b
 db $18, $00, $00
 db $84, $2a, $00, $e1, $25
 db $18, $00, $00
 db $94, $04, $00, $b1, $25
 db $18, $00, $00
 db $94, $04, $00, $e2, $4b
 db $18, $00, $00
 db $90, $0c, $b1, $ec, $b2, $4b
 db $18, $00, $00
 db $90, $00, $b2, $28, $e2, $93
 db $18, $00, $00
 db $94, $04, $00, $b2, $93
 db $18, $00, $00
 db $90, $00, $00, $e2, $93
 db $18, $00, $00
 db $94, $04, $00, $b2, $93
 db $18, $00, $00
 db $84, $2a, $00, $e2, $93
 db $18, $00, $00
 db $94, $04, $00, $b2, $93
 db $18, $00, $00
 db $90, $0c, $00, $e2, $93
 db $18, $00, $00
 db $84, $2a, $00, $b2, $93
 db $18, $00, $00
 db $90, $00, $b2, $4b, $e2, $93
 db $18, $b2, $e4, $00
 db $18, $b3, $3f, $b2, $93
 db $18, $00, $00
 db $18, $00, $e2, $93
 db $18, $00, $00
 db $18, $00, $b2, $93
 db $18, $00, $00
 db $18, $b2, $e0, $e2, $93
 db $18, $00, $e2, $e0
 db $18, $b2, $8f, $b2, $93
 db $18, $00, $b2, $8f
 db $18, $b2, $26, $e2, $93
 db $18, $00, $e2, $26
 db $84, $2a, $b1, $ea, $b2, $93
 db $18, $00, $00
 db $90, $d1, $00, $b2, $2a, $b0, $b7
 db $18, $01, $01
 db $18, $b2, $2a, $b0, $b9
 db $18, $01, $01
 db $18, $b2, $2a, $b0, $b9
 db $18, $01, $00
 db $18, $b2, $2a, $01
 db $18, $01, $00
 db $90, $00, $b2, $2a, $b0, $b9
 db $18, $01, $01
 db $18, $b2, $2a, $b0, $b9
 db $18, $01, $01
 db $98, $0d, $f1, $72, $b0, $b9
 db $18, $00, $00
 db $18, $f1, $49, $01
 db $18, $00, $00
 db $90, $d1, $00, $f1, $72, $b0, $b9
 db $18, $00, $01
 db $18, $f1, $49, $b0, $b9
 db $18, $00, $01
 db $98, $0d, $f1, $72, $b0, $b9
 db $18, $00, $00
 db $98, $91, $f2, $2a, $01
 db $18, $01, $00
 db $90, $4d, $00, $f1, $49, $b0, $b9
 db $18, $00, $01
 db $98, $91, $f1, $72, $b0, $b9
 db $18, $00, $01
 db $98, $0d, $00, $b0, $b9
 db $18, $00, $00
 db $98, $91, $f1, $49, $01
 db $18, $00, $00
 db $90, $00, $f1, $72, $b0, $b9
 db $18, $00, $01
 db $18, $f2, $2a, $b0, $b9
 db $18, $01, $01
 db $18, $f2, $2a, $b0, $b9
 db $18, $01, $00
 db $18, $f2, $2a, $01
 db $18, $01, $00
 db $90, $00, $f2, $2a, $b0, $b9
 db $18, $01, $01
 db $18, $f2, $2a, $b0, $b9
 db $18, $01, $01
 db $98, $0d, $e1, $72, $b0, $b9
 db $18, $00, $00
 db $18, $e1, $49, $01
 db $18, $00, $00
 db $90, $d1, $00, $e1, $72, $b0, $b9
 db $18, $00, $01
 db $18, $e1, $49, $b0, $b9
 db $18, $00, $01
 db $98, $0d, $e1, $72, $b0, $b9
 db $18, $00, $00
 db $98, $91, $e2, $2a, $01
 db $18, $01, $00
 db $90, $4d, $00, $e1, $49, $b0, $b9
 db $18, $00, $01
 db $98, $91, $e1, $72, $b0, $b9
 db $18, $00, $01
 db $18, $e1, $49, $b0, $b9
 db $18, $00, $00
 db $18, $e2, $2a, $01
 db $18, $01, $00
 db $90, $00, $e2, $2a, $b0, $da
 db $18, $01, $01
 db $18, $e2, $2a, $b0, $dc
 db $18, $01, $01
 db $18, $e2, $2a, $b0, $dc
 db $18, $01, $00
 db $18, $e2, $2a, $01
 db $18, $01, $00
 db $90, $00, $e2, $2a, $b0, $dc
 db $18, $01, $01
 db $18, $e2, $2a, $b0, $dc
 db $18, $01, $01
 db $98, $0d, $d1, $72, $b0, $dc
 db $18, $00, $00
 db $18, $d1, $49, $01
 db $18, $00, $00
 db $90, $d1, $00, $d1, $72, $b0, $dc
 db $18, $00, $01
 db $18, $d1, $49, $b0, $dc
 db $18, $00, $01
 db $98, $0d, $d1, $72, $b0, $dc
 db $18, $00, $00
 db $98, $91, $d2, $2a, $01
 db $18, $01, $00
 db $90, $4d, $00, $d1, $49, $b0, $dc
 db $18, $00, $01
 db $98, $91, $d1, $72, $b0, $dc
 db $18, $00, $01
 db $98, $0d, $00, $b0, $dc
 db $18, $00, $00
 db $98, $91, $d1, $49, $01
 db $18, $00, $00
 db $90, $4d, $00, $c1, $9f, $b0, $a4
 db $18, $00, $01
 db $98, $91, $c0, $a4, $f1, $9f
 db $18, $01, $00
 db $18, $c1, $9f, $f0, $a4
 db $18, $00, $00
 db $18, $c2, $2a, $01
 db $18, $01, $00
 db $90, $4d, $00, $c1, $b8, $f0, $a4
 db $18, $00, $01
 db $98, $91, $c0, $a4, $f1, $b8
 db $18, $01, $00
 db $18, $c1, $9f, $f0, $a4
 db $18, $00, $00
 db $18, $c2, $2a, $01
 db $18, $01, $00
 db $90, $4d, $00, $b1, $ee, $f0, $a4
 db $18, $00, $01
 db $98, $91, $b0, $a4, $f1, $ee
 db $18, $01, $00
 db $18, $b1, $b8, $f0, $a4
 db $18, $00, $00
 db $18, $b2, $2a, $01
 db $18, $01, $00
 db $90, $4d, $00, $a1, $9f, $d0, $cf
 db $18, $00, $00
 db $98, $91, $a1, $ee, $d0, $a4
 db $18, $00, $01
 db $98, $0d, $a1, $72, $d0, $b9
 db $18, $00, $00
 db $98, $91, $a1, $9f, $01
 db $18, $00, $00
 db $90, $00, $a2, $2a, $d0, $b7
 db $18, $01, $01
 db $18, $a2, $2a, $d0, $b9
 db $18, $01, $01
 db $18, $a2, $2a, $d0, $b9
 db $18, $01, $00
 db $18, $a2, $2a, $01
 db $18, $01, $00
 db $90, $cf, $00, $a2, $28, $d1, $15
 db $18, $00, $00
 db $98, $91, $a2, $2a, $d0, $b9
 db $18, $01, $01
 db $98, $8f, $a2, $91, $d1, $49
 db $18, $00, $00
 db $98, $91, $a1, $15, $01
 db $18, $00, $00
 db $90, $00, $a1, $49, $d0, $b9
 db $18, $00, $01
 db $18, $a2, $2a, $d0, $b9
 db $18, $01, $01
 db $18, $a2, $2a, $d0, $b9
 db $18, $01, $00
 db $18, $a2, $2a, $01
 db $18, $01, $00
 db $90, $cf, $00, $83, $6e, $d1, $b8
 db $18, $00, $00
 db $98, $91, $82, $2a, $d0, $b9
 db $18, $01, $01
 db $98, $8f, $83, $3d, $d1, $9f
 db $18, $00, $00
 db $98, $91, $81, $b8, $01
 db $18, $00, $00
 db $90, $00, $81, $9f, $d0, $b9
 db $18, $00, $01
 db $18, $82, $2a, $d0, $b9
 db $18, $01, $01
 db $18, $82, $2a, $d0, $b9
 db $18, $01, $00
 db $18, $82, $2a, $01
 db $18, $01, $00
 db $90, $cf, $00, $85, $25, $d2, $93
 db $18, $00, $00
 db $98, $91, $82, $2a, $d0, $b9
 db $18, $01, $01
 db $98, $8f, $84, $53, $d2, $2a
 db $18, $00, $00
 db $98, $91, $82, $93, $01
 db $18, $00, $00
 db $90, $00, $82, $2a, $d0, $b9
 db $18, $00, $01
 db $18, $00, $d0, $b9
 db $18, $01, $01
 db $18, $82, $2a, $d0, $b9
 db $18, $01, $00
 db $18, $82, $2a, $01
 db $18, $01, $00
 db $90, $00, $82, $2a, $d0, $b9
 db $18, $01, $01
 db $18, $82, $2a, $d0, $b9
 db $18, $01, $01
 db $98, $8b, $81, $b4, $91, $b8
 db $18, $00, $00
 db $18, $81, $9b, $91, $9f
 db $18, $00, $00
 db $90, $00, $81, $b4, $91, $b8
 db $18, $00, $00
 db $98, $91, $81, $9f, $90, $dc
 db $18, $00, $01
 db $98, $03, $81, $b8, $90, $dc
 db $18, $00, $00
 db $18, $82, $2a, $01
 db $18, $01, $00
 db $90, $cb, $00, $81, $9b, $91, $9f
 db $18, $00, $00
 db $98, $91, $82, $2a, $90, $dc
 db $18, $01, $01
 db $98, $03, $81, $9f, $90, $dc
 db $18, $00, $00
 db $98, $91, $82, $2a, $01
 db $18, $01, $00
 db $90, $cb, $00, $81, $6e, $91, $72
 db $18, $00, $00
 db $98, $03, $81, $9f, $90, $dc
 db $18, $00, $01
 db $18, $81, $72, $90, $dc
 db $18, $00, $00
 db $98, $91, $82, $2a, $01
 db $18, $01, $00
 db $90, $cb, $00, $81, $45, $91, $49
 db $18, $00, $00
 db $98, $91, $82, $2a, $90, $dc
 db $18, $01, $01
 db $98, $8b, $81, $b4, $91, $b8
 db $18, $00, $00
 db $84, $43, $2c, $81, $49, $01
 db $84, $2c, $00, $00
 db $90, $2e, $81, $b8, $90, $a4
 db $18, $00, $01
 db $94, $d1, $08, $82, $2a, $90, $a4
 db $18, $01, $01
 db $94, $08, $82, $2a, $90, $a4
 db $18, $01, $00
 db $90, $2e, $82, $2a, $01
 db $18, $01, $00
 db $84, $cb, $30, $81, $9b, $91, $9f
 db $18, $00, $00
 db $90, $d1, $32, $82, $2a, $90, $a4
 db $18, $01, $01
 db $94, $43, $0e, $81, $9f, $90, $a4
 db $18, $00, $00
 db $84, $d1, $30, $82, $2a, $01
 db $18, $01, $00
 db $90, $cb, $34, $81, $6e, $91, $72
 db $18, $00, $00
 db $84, $d1, $36, $82, $2a, $90, $a4
 db $18, $01, $01
 db $90, $43, $34, $81, $72, $90, $a4
 db $18, $00, $00
 db $94, $d1, $12, $82, $2a, $01
 db $18, $01, $00
 db $84, $cb, $38, $81, $47, $91, $49
 db $18, $00, $00
 db $94, $d1, $16, $82, $2a, $90, $a4
 db $18, $01, $01
 db $84, $c1, $38, $84, $53, $f2, $2a
 db $18, $00, $00
 db $84, $d1, $38, $82, $2a, $01
 db $18, $01, $00
 db $90, $00, $82, $2a, $f0, $90
 db $18, $00, $01
 db $90, $00, $00, $f0, $92
 db $18, $00, $01
 db $94, $04, $82, $e4, $f0, $92
 db $18, $01, $00
 db $90, $00, $82, $e4, $01
 db $18, $01, $00
 db $84, $c1, $2a, $83, $da, $f1, $ee
 db $18, $00, $00
 db $90, $d1, $00, $82, $e4, $f0, $92
 db $18, $01, $01
 db $94, $04, $81, $ee, $f0, $92
 db $18, $00, $00
 db $90, $00, $00, $01
 db $18, $00, $00
 db $90, $c1, $00, $83, $6e, $f1, $b8
 db $18, $00, $00
 db $94, $d1, $04, $82, $e4, $f0, $92
 db $18, $01, $01
 db $90, $00, $81, $b8, $f0, $92
 db $18, $00, $01
 db $90, $00, $00, $f0, $92
 db $18, $00, $01
 db $84, $c1, $2a, $83, $3d, $f1, $9f
 db $18, $00, $00
 db $90, $d1, $00, $82, $e4, $f0, $92
 db $18, $01, $01
 db $94, $c1, $04, $84, $53, $f2, $2a
 db $18, $00, $00
 db $94, $d1, $04, $81, $9f, $01
 db $18, $00, $00
 db $90, $00, $82, $2a, $f0, $a4
 db $18, $00, $01
 db $90, $00, $00, $f0, $a4
 db $18, $00, $01
 db $94, $04, $83, $3f, $f0, $a4
 db $18, $01, $00
 db $90, $00, $83, $3f, $01
 db $18, $01, $00
 db $84, $c1, $2a, $83, $da, $f1, $ee
 db $18, $00, $00
 db $90, $d1, $00, $83, $3f, $f0, $a4
 db $18, $01, $01
 db $94, $04, $81, $ee, $f0, $a4
 db $18, $00, $00
 db $90, $00, $00, $01
 db $18, $00, $00
 db $90, $c1, $00, $83, $6e, $f1, $b8
 db $18, $00, $00
 db $94, $d1, $04, $83, $3f, $f0, $a4
 db $18, $01, $01
 db $90, $00, $81, $b8, $f0, $a4
 db $18, $00, $01
 db $90, $00, $00, $f0, $a4
 db $18, $00, $01
 db $84, $c1, $2a, $83, $3d, $f1, $9f
 db $18, $00, $00
 db $90, $d1, $00, $83, $3f, $f0, $a4
 db $18, $01, $01
 db $94, $c1, $04, $85, $25, $f2, $93
 db $18, $00, $00
 db $94, $d1, $04, $81, $9f, $01
 db $18, $00, $00
 db $90, $00, $82, $93, $f0, $b7
 db $18, $00, $01
 db $90, $00, $00, $f0, $b9
 db $18, $00, $01
 db $94, $04, $83, $70, $f0, $b9
 db $18, $01, $00
 db $90, $00, $83, $70, $01
 db $18, $01, $00
 db $84, $c1, $2a, $84, $95, $f2, $4b
 db $18, $00, $00
 db $90, $d1, $00, $83, $70, $f0, $b9
 db $18, $01, $01
 db $94, $04, $82, $4b, $f0, $b9
 db $18, $00, $00
 db $90, $00, $00, $01
 db $18, $00, $00
 db $90, $00, $83, $70, $f0, $b9
 db $18, $01, $01
 db $94, $04, $83, $70, $f0, $b9
 db $18, $01, $01
 db $90, $c1, $00, $84, $53, $f2, $2a
 db $18, $00, $00
 db $90, $00, $83, $70, $f0, $b9
 db $18, $01, $01
 db $84, $2a, $83, $da, $f1, $ee
 db $18, $00, $00
 db $90, $d1, $00, $82, $2a, $f0, $b9
 db $18, $00, $01
 db $94, $c1, $04, $84, $53, $f2, $2a
 db $18, $00, $00
 db $94, $d1, $04, $81, $ee, $01
 db $18, $00, $00
 db $90, $00, $82, $2a, $f0, $b9
 db $18, $00, $01
 db $90, $00, $00, $f0, $b9
 db $18, $00, $01
 db $94, $04, $84, $53, $84, $55
 db $18, $01, $01
 db $90, $00, $84, $53, $84, $55
 db $18, $01, $01
 db $84, $2a, $84, $53, $83, $dc
 db $18, $00, $01
 db $90, $00, $94, $53, $90, $b9
 db $18, $01, $01
 db $94, $04, $83, $da, $83, $70
 db $18, $00, $01
 db $90, $00, $94, $53, $83, $70
 db $18, $01, $01
 db $90, $00, $83, $6e, $83, $3f
 db $18, $00, $00
 db $94, $04, $94, $53, $90, $b9
 db $18, $01, $01
 db $90, $00, $83, $3d, $82, $e4
 db $18, $00, $00
 db $90, $00, $94, $53, $90, $b9
 db $18, $01, $01
 db $84, $2a, $82, $e2, $82, $93
 db $18, $00, $00
 db $90, $00, $94, $53, $82, $2a
 db $18, $01, $00
 db $94, $cb, $04, $82, $e2, $82, $e4
 db $18, $00, $00
 db $94, $04, $82, $91, $82, $93
 db $18, $00, $00
 db $90, $00, $82, $e2, $82, $e4
 db $18, $00, $00
 db $90, $d1, $00, $82, $93, $80, $92
 db $18, $00, $01
 db $94, $04, $82, $e4, $80, $92
 db $18, $00, $00
 db $90, $00, $00, $01
 db $18, $00, $00
 db $84, $cb, $2a, $82, $91, $82, $93
 db $18, $00, $00
 db $90, $d1, $00, $82, $e4, $80, $92
 db $18, $01, $01
 db $94, $cb, $04, $82, $49, $82, $4b
 db $18, $00, $00
 db $90, $d1, $00, $82, $93, $01
 db $18, $00, $00
 db $90, $00, $82, $4b, $80, $92
 db $18, $00, $01
 db $94, $04, $00, $80, $92
 db $18, $00, $01
 db $90, $cb, $00, $82, $e2, $82, $e4
 db $18, $00, $00
 db $90, $d1, $00, $82, $e4, $80, $92
 db $18, $01, $01
 db $84, $cb, $2a, $82, $91, $82, $93
 db $18, $00, $00
 db $90, $d1, $00, $82, $e4, $80, $92
 db $18, $00, $01
 db $94, $cb, $04, $82, $e2, $82, $e4
 db $18, $00, $00
 db $94, $d1, $04, $82, $93, $01
 db $18, $00, $00
 db $90, $cb, $00, $83, $3d, $83, $3f
 db $18, $00, $00
 db $90, $d1, $00, $82, $e4, $80, $a4
 db $18, $00, $01
 db $94, $04, $83, $3f, $80, $a4
 db $18, $00, $00
 db $90, $00, $00, $01
 db $18, $00, $00
 db $84, $cb, $2a, $82, $e2, $82, $e4
 db $18, $00, $00
 db $90, $d1, $00, $83, $3f, $80, $a4
 db $18, $01, $01
 db $94, $cb, $04, $82, $91, $82, $93
 db $18, $00, $00
 db $90, $d1, $00, $82, $e4, $01
 db $18, $00, $00
 db $90, $00, $82, $93, $80, $a4
 db $18, $00, $01
 db $94, $04, $00, $80, $a4
 db $18, $00, $01
 db $90, $00, $83, $3f, $80, $a4
 db $18, $01, $01
 db $90, $cb, $00, $83, $3d, $83, $3f
 db $18, $00, $00
 db $84, $2a, $83, $6e, $83, $70
 db $18, $00, $00
 db $90, $d1, $00, $83, $3f, $80, $a4
 db $18, $00, $01
 db $94, $cb, $04, $83, $3d, $83, $3f
 db $18, $00, $00
 db $94, $d1, $04, $83, $3f, $01
 db $18, $01, $00
 db $90, $00, $83, $3f, $80, $b7
 db $18, $00, $01
 db $90, $00, $00, $80, $b9
 db $18, $00, $01
 db $94, $cb, $04, $82, $e2, $82, $e4
 db $18, $00, $00
 db $90, $d1, $00, $82, $e4, $01
 db $18, $01, $00
 db $84, $cb, $2a, $82, $91, $82, $93
 db $18, $00, $00
 db $90, $d1, $00, $82, $e4, $80, $b9
 db $18, $00, $01
 db $94, $cb, $04, $82, $e2, $82, $e4
 db $18, $00, $00
 db $90, $d1, $00, $82, $93, $01
 db $18, $00, $00
 db $90, $00, $82, $e4, $80, $b9
 db $18, $00, $01
 db $94, $04, $00, $80, $b9
 db $18, $00, $01
 db $90, $cb, $00, $83, $3d, $83, $3f
 db $18, $00, $00
 db $90, $d1, $00, $82, $e4, $80, $b9
 db $18, $01, $01
 db $84, $cb, $2a, $82, $e2, $82, $e4
 db $18, $00, $00
 db $90, $d1, $00, $83, $3f, $80, $b9
 db $18, $00, $01
 db $94, $cb, $04, $83, $3d, $83, $3f
 db $18, $00, $00
 db $94, $d1, $04, $82, $e4, $01
 db $18, $00, $00
 db $90, $00, $83, $3f, $80, $b9
 db $18, $00, $01
 db $90, $00, $00, $80, $b9
 db $18, $00, $01
 db $94, $cb, $04, $83, $6e, $83, $70
 db $18, $00, $00
 db $90, $d1, $00, $82, $e4, $01
 db $18, $01, $00
 db $84, $cb, $2a, $83, $3d, $83, $3f
 db $18, $00, $00
 db $90, $d1, $00, $83, $70, $80, $b9
 db $18, $00, $01
 db $94, $cb, $04, $83, $6e, $83, $70
 db $18, $00, $00
 db $90, $d1, $00, $83, $3f, $01
 db $18, $00, $00
 db $90, $00, $83, $70, $80, $b9
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $01, $93, $dc
 db $18, $00, $94, $17
 db $18, $00, $94, $55
 db $18, $00, $00
 db $81, $02, $83, $da, $00
 db $01, $84, $15, $00
 db $16, $84, $53, $00
 db $18, $00, $00
 db $18, $00, $93, $70
 db $18, $00, $92, $2a
 db $18, $00, $91, $b8
 db $18, $00, $91, $15
 db $90, $c1, $00, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $18, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $18, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $18, $e1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $18, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $18, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $18, $e1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e2, $4b
 db $18, $00, $00
 db $18, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e2, $4b
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $92
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $92
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e2, $4b
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $18, $b1, $ee, $e2, $4b
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $92
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $92
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e2, $4b
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b2, $4b
 db $18, $00, $00
 db $18, $e1, $ee, $e2, $4b
 db $18, $00, $00
 db $90, $0c, $e2, $2a, $b2, $4b
 db $18, $00, $00
 db $90, $00, $b1, $ea, $e2, $2a
 db $18, $b2, $26, $00
 db $18, $00, $b2, $2a
 db $18, $00, $00
 db $90, $00, $00, $e2, $2a
 db $18, $00, $00
 db $94, $04, $b1, $ee, $b2, $2a
 db $18, $00, $00
 db $84, $2a, $b1, $13, $e1, $15
 db $18, $00, $00
 db $94, $04, $00, $b1, $15
 db $18, $00, $00
 db $90, $0c, $b1, $b8, $e2, $2a
 db $18, $00, $00
 db $90, $0c, $00, $b2, $2a
 db $18, $00, $00
 db $18, $b1, $13, $e1, $15
 db $18, $00, $00
 db $90, $0c, $b1, $9f, $b2, $2a
 db $18, $00, $00
 db $90, $0c, $00, $e2, $2a
 db $18, $00, $00
 db $94, $04, $b1, $13, $b1, $15
 db $18, $00, $00
 db $84, $2a, $b1, $72, $e2, $2a
 db $18, $00, $00
 db $90, $00, $00, $b2, $2a
 db $18, $00, $00
 db $84, $2a, $b1, $49, $e2, $2a
 db $18, $00, $00
 db $84, $2a, $00, $b2, $2a
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $18, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $18, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $18, $e1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $18, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $18, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $18, $e1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e2, $4b
 db $18, $00, $00
 db $18, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e2, $4b
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $92
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $92
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e2, $4b
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $18, $b1, $ee, $e2, $4b
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $92
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $92
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e2, $4b
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b2, $4b
 db $18, $00, $00
 db $90, $00, $e1, $ee, $e2, $4b
 db $18, $00, $00
 db $90, $00, $e2, $2a, $b2, $4b
 db $18, $00, $00
 db $84, $2a, $b1, $ea, $e2, $93
 db $18, $b2, $26, $00
 db $94, $04, $00, $b2, $93
 db $18, $00, $00
 db $90, $00, $00, $e2, $93
 db $18, $00, $00
 db $84, $2a, $b1, $ee, $b2, $93
 db $18, $00, $00
 db $90, $00, $b1, $13, $e1, $49
 db $18, $00, $00
 db $90, $00, $00, $b1, $49
 db $18, $00, $00
 db $84, $2a, $b1, $b8, $e2, $93
 db $18, $00, $00
 db $90, $00, $00, $b2, $93
 db $18, $00, $00
 db $94, $04, $b1, $13, $e1, $49
 db $18, $00, $00
 db $84, $2a, $b1, $9f, $b2, $93
 db $18, $00, $00
 db $90, $00, $00, $e2, $93
 db $18, $00, $00
 db $94, $04, $b1, $13, $b1, $49
 db $18, $00, $00
 db $84, $2a, $b1, $72, $e2, $93
 db $18, $00, $00
 db $84, $2a, $00, $b2, $93
 db $18, $00, $00
 db $84, $2a, $b1, $49, $e2, $93
 db $18, $00, $00
 db $84, $2a, $00, $b2, $93
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $94, $04, $e1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $84, $2a, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $e1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e2, $4b
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e2, $4b
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $92
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $92
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e2, $4b
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e2, $4b
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b2, $4b
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $92
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $92
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e2, $4b
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b2, $4b
 db $18, $00, $00
 db $94, $04, $e1, $47, $e1, $25
 db $18, $00, $00
 db $90, $0c, $e1, $70, $b1, $25
 db $18, $00, $00
 db $90, $00, $e2, $28, $e2, $2a
 db $18, $00, $00
 db $94, $04, $00, $b2, $2a
 db $18, $00, $00
 db $90, $00, $00, $e1, $15
 db $18, $00, $00
 db $94, $04, $e1, $47, $b1, $15
 db $18, $00, $00
 db $84, $2a, $e1, $ec, $e1, $ee
 db $18, $00, $00
 db $94, $04, $00, $b1, $ee
 db $18, $00, $00
 db $90, $0c, $e1, $b6, $e1, $b8
 db $18, $00, $00
 db $84, $2a, $00, $b1, $b8
 db $18, $00, $00
 db $90, $00, $00, $e2, $2a
 db $18, $00, $00
 db $90, $0c, $e1, $47, $b1, $15
 db $18, $00, $00
 db $90, $0c, $00, $e1, $49
 db $18, $00, $00
 db $94, $04, $00, $b2, $2a
 db $18, $00, $00
 db $84, $2a, $e1, $70, $e1, $72
 db $18, $00, $00
 db $94, $04, $00, $b2, $2a
 db $18, $00, $00
 db $84, $2a, $e1, $9d, $e1, $9f
 db $18, $00, $00
 db $84, $2a, $00, $b2, $2a
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $94, $04, $e1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $94, $04, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $84, $43, $2a, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $90, $c1, $0c, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $84, $2a, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $b1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $b2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $43, $0c, $e1, $ee, $e0, $b9
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b0, $b9
 db $18, $00, $00
 db $84, $c1, $2a, $e2, $e0, $e1, $72
 db $18, $00, $00
 db $94, $04, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $e1, $ee, $e1, $72
 db $18, $00, $00
 db $90, $0c, $e2, $2a, $b1, $72
 db $18, $00, $00
 db $90, $00, $e2, $28, $e2, $4b
 db $18, $00, $00
 db $94, $04, $00, $b2, $4b
 db $18, $00, $00
 db $90, $00, $00, $e1, $25
 db $18, $00, $00
 db $94, $04, $e1, $23, $b1, $25
 db $18, $00, $00
 db $84, $2a, $e1, $ec, $e1, $ee
 db $18, $00, $00
 db $94, $04, $00, $b1, $ee
 db $18, $00, $00
 db $90, $0c, $e1, $b6, $e1, $b8
 db $18, $00, $00
 db $90, $0c, $00, $b1, $b8
 db $18, $00, $00
 db $90, $00, $00, $e2, $4b
 db $18, $00, $00
 db $90, $0c, $e1, $23, $b1, $25
 db $18, $00, $00
 db $90, $0c, $e1, $47, $e1, $49
 db $18, $00, $00
 db $94, $04, $00, $b2, $4b
 db $18, $00, $00
 db $84, $2a, $e1, $70, $e1, $72
 db $18, $00, $00
 db $94, $04, $00, $b2, $2a
 db $18, $00, $00
 db $94, $04, $e1, $9d, $e1, $9f
 db $18, $00, $00
 db $90, $0c, $00, $b2, $4b
 db $18, $00, $00
 db $90, $00, $e2, $28, $e2, $93
 db $18, $00, $00
 db $94, $04, $00, $b2, $93
 db $18, $00, $00
 db $90, $00, $00, $e2, $93
 db $18, $00, $00
 db $94, $04, $00, $b2, $93
 db $18, $00, $00
 db $84, $2a, $00, $e2, $93
 db $18, $00, $00
 db $94, $04, $00, $b2, $93
 db $18, $00, $00
 db $90, $0c, $00, $e2, $93
 db $18, $00, $00
 db $84, $2a, $00, $b2, $93
 db $18, $00, $00
 db $90, $00, $e2, $4b, $e2, $93
 db $18, $e2, $e4, $00
 db $18, $e3, $3f, $b2, $93
 db $18, $00, $00
 db $18, $00, $e2, $93
 db $18, $00, $00
 db $18, $00, $b2, $93
 db $18, $00, $00
 db $18, $e2, $e0, $e2, $93
 db $18, $00, $e2, $e0
 db $18, $e2, $8f, $b2, $93
 db $18, $00, $b2, $8f
 db $18, $e2, $26, $e2, $93
 db $18, $00, $e2, $26
 db $84, $2a, $e1, $ea, $b2, $93
 db $18, $00, $00
 db $90, $00, $b2, $e0, $e1, $72
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $b2, $bb, $00
 db $18, $b2, $93, $00
 db $18, $b2, $6e, $00
 db $18, $b2, $4b, $00
 db $18, $b2, $2a, $00
 db $18, $b2, $0b, $00
 db $18, $b1, $ee, $00
 db $18, $b1, $d2, $00
 db $84, $2a, $b1, $6e, $b0, $b9
 db $18, $00, $00
 db $84, $2a, $00, $e0, $b9
 db $18, $00, $00
 db $84, $2a, $b0, $b7, $b0, $b9
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
.loop
 db $18, $01, $01
 db $18, $00, $00
 db $18, $00, $00
 db $18, $00, $00
 db $00
 dw .loop
 align 2
.drumpar
.dp0
 dw .dsmp0+0
 db $02, $09, $40
.dp1
 dw .dsmp3+0
 db $0c, $09, $40
.dp2
 dw .dsmp2+0
 db $01, $09, $40
.dp3
 dw .dsmp1+0
 db $03, $09, $40
.dp4
 dw .dsmp2+0
 db $01, $09, $00
.dp5
 dw .dsmp5+0
 db $03, $09, $00
.dp6
 dw .dsmp4+0
 db $02, $09, $40
.dp7
 dw .dsmp2+0
 db $01, $09, $08
.dp8
 dw .dsmp5+0
 db $03, $09, $08
.dp9
 dw .dsmp2+0
 db $01, $09, $10
.dp10
 dw .dsmp5+0
 db $03, $09, $10
.dp11
 dw .dsmp2+0
 db $01, $09, $18
.dp12
 dw .dsmp5+0
 db $03, $09, $18
.dp13
 dw .dsmp2+0
 db $01, $09, $20
.dp14
 dw .dsmp5+0
 db $03, $09, $20
.dp15
 dw .dsmp2+0
 db $01, $09, $28
.dp16
 dw .dsmp5+0
 db $03, $09, $28
.dp17
 dw .dsmp2+0
 db $01, $09, $30
.dp18
 dw .dsmp5+0
 db $03, $09, $30
.dp19
 dw .dsmp2+0
 db $01, $09, $38
.dp20
 dw .dsmp5+0
 db $03, $09, $38
.dp21
 dw .dsmp6+0
 db $05, $09, $40
.dp22
 dw .dsmp6+0
 db $05, $09, $00
.dp23
 dw .dsmp0+0
 db $02, $09, $00
.dp24
 dw .dsmp6+0
 db $05, $09, $08
.dp25
 dw .dsmp0+0
 db $02, $09, $08
.dp26
 dw .dsmp0+0
 db $02, $09, $10
.dp27
 dw .dsmp6+0
 db $05, $09, $10
.dp28
 dw .dsmp6+0
 db $05, $09, $18
.dsmp0
 db $00, $00, $00, $00, $00, $00, $00, $00, $01, $07, $f3, $fc, $ff, $ff, $ff, $ff
 db $ff, $e7, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
 db $00, $00, $00, $f3, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
 db $ff, $ff, $ff, $ff, $f8, $c0, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.dsmp1
 db $00, $78, $00, $f0, $07, $00, $0e, $00, $0f, $80, $38, $00, $00, $70, $00, $7f
 db $00, $02, $1c, $00, $0e, $00, $00, $c0, $01, $80, $00, $00, $00, $00, $00, $00
 db $3e, $00, $00, $00, $00, $07, $00, $00, $30, $00, $60, $00, $00, $c0, $00, $00
 db $00, $1f, $00, $03, $e0, $00, $3c, $00, $00, $0e, $00, $1e, $00, $03, $c0, $00
 db $00, $00, $00, $00, $03, $80, $00, $07, $80, $03, $80, $00, $00, $00, $18, $00
 db $00, $3c, $00, $78, $00, $00, $00, $00, $40, $00, $00, $00, $00, $00, $00, $00
.dsmp2
 db $00, $90, $0c, $40, $04, $20, $00, $28, $00, $00, $00, $40, $40, $40, $00, $00
 db $80, $01, $00, $00, $00, $00, $00, $10, $00, $00, $00, $00, $00, $00, $00, $00
.dsmp3
 db $00, $e0, $00, $df, $c0, $00, $00, $00, $00, $00, $00, $1e, $fc, $10, $39, $00
 db $00, $00, $38, $00, $2c, $00, $07, $ff, $80, $00, $00, $00, $00, $00, $00, $f8
 db $00, $ff, $fc, $3f, $ff, $80, $01, $ff, $e7, $e0, $00, $00, $00, $00, $01, $00
 db $30, $fe, $8f, $ff, $ff, $ff, $88, $5f, $ff, $f8, $00, $00, $00, $00, $00, $00
 db $0c, $00, $08, $ff, $ff, $ff, $ff, $ff, $ff, $00, $00, $00, $00, $00, $00, $40
 db $00, $03, $ff, $ff, $ff, $f8, $ff, $ff, $f8, $00, $00, $00, $00, $00, $00, $00
 db $01, $80, $3f, $ff, $ff, $ff, $ff, $fb, $00, $00, $00, $00, $00, $00, $00, $00
 db $00, $02, $ff, $ff, $ff, $ff, $f9, $00, $00, $00, $00, $f0, $00, $40, $00, $00
 db $00, $07, $ff, $ff, $ff, $ff, $00, $00, $00, $00, $7f, $fc, $00, $00, $00, $00
 db $07, $ff, $ff, $ff, $f3, $20, $00, $00, $00, $3f, $ff, $80, $00, $00, $00, $00
 db $ff, $ff, $ff, $ff, $40, $00, $00, $00, $0f, $ff, $02, $80, $00, $00, $00, $7f
 db $ff, $ff, $ff, $80, $00, $00, $00, $03, $ff, $ff, $f8, $00, $00, $00, $07, $ff
 db $ff, $ff, $fa, $00, $00, $00, $00, $ff, $ff, $ff, $c0, $00, $00, $00, $0f, $ff
 db $ff, $ff, $f8, $00, $00, $00, $07, $ff, $ff, $ff, $c0, $00, $00, $00, $07, $ff
 db $ff, $ff, $f8, $00, $00, $00, $00, $07, $ff, $ff, $ff, $80, $00, $00, $00, $3f
 db $ff, $ff, $ff, $f0, $00, $00, $00, $00, $07, $ff, $ff, $ff, $00, $00, $00, $00
 db $07, $ff, $ff, $ff, $fc, $00, $00, $00, $00, $01, $ff, $ff, $ff, $ff, $80, $00
 db $00, $00, $07, $ff, $ff, $ff, $fc, $00, $00, $00, $00, $00, $1f, $ff, $ff, $ff
 db $ff, $c0, $00, $00, $00, $00, $ff, $ff, $ff, $fe, $f0, $00, $00, $00, $00, $07
 db $ff, $ff, $ff, $fe, $80, $00, $00, $00, $00, $ff, $ff, $ff, $fe, $00, $00, $00
 db $00, $00, $00, $ff, $ff, $ff, $ff, $fc, $00, $00, $00, $00, $00, $00, $ff, $ff
 db $ff, $f3, $e0, $00, $00, $00, $00, $00, $07, $ff, $ff, $ff, $fc, $38, $00, $00
 db $00, $00, $07, $ff, $ff, $ff, $e0, $00, $00, $00, $00, $00, $00, $00, $ff, $ff
 db $ff, $ff, $f8, $00, $00, $00, $00, $00, $00, $00, $07, $81, $ff, $07, $00, $00
.dsmp4
 db $3f, $c0, $7f, $80, $7f, $80, $ff, $00, $ff, $00, $ff, $00, $ff, $01, $fe, $03
 db $fe, $03, $fc, $07, $fc, $07, $f8, $0f, $f8, $0f, $f0, $0f, $e0, $1f, $e0, $1f
 db $c0, $3f, $c0, $3f, $80, $7f, $00, $7f, $00, $ff, $00, $ff, $00, $ff, $00, $fe
 db $01, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $00, $01, $02, $02, $03, $02, $02, $01
.dsmp5
 db $00, $00, $00, $08, $20, $10, $66, $cf, $e6, $80, $00, $83, $00, $df, $fe, $40
 db $00, $00, $00, $3f, $ff, $ff, $c0, $00, $00, $00, $0f, $ff, $e0, $00, $00, $00
 db $00, $ff, $ff, $fe, $00, $00, $00, $00, $3f, $ff, $80, $00, $00, $00, $00, $63
 db $ff, $f0, $00, $00, $00, $00, $7f, $fe, $00, $00, $00, $00, $00, $01, $ff, $e0
 db $00, $00, $00, $00, $0c, $04, $c0, $00, $00, $00, $00, $03, $fc, $00, $00, $00
 db $00, $00, $00, $00, $e0, $00, $00, $00, $00, $00, $00, $00, $00, $3f, $ff, $ff
.dsmp6
 db $3d, $ff, $0e, $38, $00, $00, $00, $00, $01, $01, $0f, $ff, $ef, $ff, $ff, $ff
 db $ff, $ff, $ff, $fe, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $ff, $ff
 db $ff, $ff, $ff, $ff, $ff, $ff, $00, $00, $00, $00, $00, $00, $00, $00, $00, $19
 db $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $fe, $00, $00, $00, $00, $00, $00
 db $00, $00, $00, $0f, $7f, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $fb, $70, $80, $00
 db $00, $00, $00, $00, $00, $00, $00, $07, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
 db $df, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $09, $ff, $ff, $ff
 db $ff, $ff, $ff, $ff, $ff, $cc, $80, $00, $00, $00, $00, $00, $00, $00, $00, $00
 db $08, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $dc, $00, $00, $00, $00, $00, $00
 db $00, $00, $00, $00, $09, $79, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $fe, $f0, $00



end


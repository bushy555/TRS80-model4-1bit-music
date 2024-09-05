	output	"TRACK05.BIN"

	org $6000
begin:


; 	EARSHAVER
; 	TRACK 5
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




; TRACK05

;compiled music data

music_data
 dw .drumpar
.song
 db $a1,$cb,$00,$81,$72,$91,$70
 db $29,$00,$01
 db $a1,$02,$00,$91,$6e
 db $a1,$02,$00,$01
 db $a1,$00,$00,$91,$6c
 db $29,$00,$01
 db $a1,$04,$00,$91,$6a
 db $a1,$04,$00,$01
 db $a1,$00,$00,$91,$68
 db $29,$00,$01
 db $a1,$06,$00,$91,$66
 db $a1,$06,$00,$01
 db $a1,$c1,$00,$e2,$e4,$91,$6e
 db $29,$00,$00
 db $a1,$08,$b2,$93,$91,$ee
 db $a1,$08,$00,$00
 db $a1,$cb,$00,$d1,$72,$e1,$70
 db $29,$00,$01
 db $a1,$0a,$00,$b1,$6e
 db $a1,$0a,$00,$01
 db $a1,$00,$00,$e1,$6c
 db $29,$00,$01
 db $a1,$0c,$00,$b1,$6a
 db $a1,$0c,$00,$01
 db $a1,$00,$00,$e1,$68
 db $29,$00,$01
 db $a1,$0e,$00,$b1,$66
 db $a1,$0e,$00,$01
 db $a1,$c1,$00,$e2,$e4,$e1,$6e
 db $29,$00,$00
 db $a1,$10,$b2,$93,$e1,$ee
 db $a1,$10,$00,$00
 db $a1,$00,$e1,$72,$b2,$2a
 db $29,$00,$00
 db $a1,$12,$b1,$72,$b1,$ee
 db $a1,$12,$00,$00
 db $99,$14,$e0,$b7,$b1,$72
 db $29,$00,$00
 db $a1,$00,$b1,$72,$b1,$ee
 db $29,$00,$00
 db $a1,$16,$e0,$b7,$b1,$72
 db $29,$00,$00
 db $a1,$18,$b1,$72,$b1,$b8
 db $29,$00,$00
 db $99,$14,$e1,$72,$b1,$ee
 db $29,$00,$00
 db $a1,$00,$b1,$72,$b1,$b8
 db $29,$00,$00
 db $a1,$00,$e1,$72,$b2,$2a
 db $29,$00,$00
 db $a1,$1a,$b1,$72,$b1,$ee
 db $a1,$1a,$00,$00
 db $99,$14,$e0,$b7,$b1,$72
 db $29,$00,$00
 db $a1,$1c,$b1,$72,$b1,$ee
 db $29,$00,$00
 db $a1,$1e,$e0,$b7,$b1,$72
 db $29,$00,$00
 db $a1,$00,$b1,$72,$b1,$b8
 db $29,$00,$00
 db $99,$14,$e1,$72,$b1,$ee
 db $29,$00,$00
 db $a1,$20,$b1,$72,$b1,$b6
 db $29,$00,$00
 db $a1,$00,$e0,$a2,$b1,$ee
 db $29,$00,$b0,$a2
 db $a1,$0c,$b0,$a2,$b1,$ee
 db $a1,$0c,$00,$b0,$a2
 db $91,$22,$e1,$47,$b1,$9f
 db $29,$00,$b1,$47
 db $9d,$24,$b1,$47,$b1,$9f
 db $29,$00,$b1,$47
 db $a1,$26,$e0,$a2,$b1,$ee
 db $29,$00,$b0,$a2
 db $a1,$0e,$b0,$a2,$b1,$ee
 db $a1,$0e,$00,$b0,$a2
 db $91,$22,$e1,$47,$b1,$9f
 db $29,$00,$b1,$47
 db $a1,$00,$b1,$47,$b1,$9f
 db $29,$00,$b1,$47
 db $a1,$00,$e0,$d8,$b2,$2a
 db $29,$00,$b0,$d8
 db $a1,$10,$b0,$da,$b2,$2a
 db $a1,$10,$00,$b0,$da
 db $91,$22,$e1,$b6,$b2,$2a
 db $29,$00,$b1,$b6
 db $9d,$24,$b1,$b6,$b2,$2a
 db $29,$00,$b1,$b6
 db $a1,$28,$e0,$da,$b2,$2a
 db $29,$00,$b0,$da
 db $a1,$2a,$b0,$da,$b2,$2a
 db $a1,$2a,$00,$b0,$da
 db $91,$22,$e1,$b6,$b2,$2a
 db $29,$00,$b1,$b6
 db $a1,$00,$b1,$b6,$b2,$2a
 db $99,$14,$00,$b1,$b6
 db $a1,$00,$e1,$72,$b2,$2a
 db $29,$00,$00
 db $a1,$2a,$b1,$72,$b1,$ee
 db $a1,$2a,$00,$00
 db $99,$14,$e0,$b7,$b1,$72
 db $29,$00,$00
 db $a1,$00,$b1,$72,$b1,$ee
 db $29,$00,$00
 db $a1,$2c,$e0,$b7,$b1,$72
 db $29,$00,$00
 db $a1,$10,$b1,$72,$b1,$b8
 db $29,$00,$00
 db $99,$14,$e1,$72,$b1,$ee
 db $29,$00,$00
 db $a1,$00,$b1,$72,$b1,$b8
 db $29,$00,$00
 db $a1,$00,$e1,$72,$b2,$2a
 db $29,$00,$00
 db $a1,$0e,$b1,$72,$b1,$ee
 db $a1,$0e,$00,$00
 db $99,$14,$e0,$b7,$b1,$72
 db $29,$00,$00
 db $a1,$0c,$b1,$72,$b1,$ee
 db $29,$00,$00
 db $a1,$2e,$e0,$b7,$b1,$72
 db $29,$00,$00
 db $a1,$00,$b1,$72,$b1,$b8
 db $29,$00,$00
 db $99,$14,$e1,$72,$b1,$ee
 db $29,$00,$00
 db $a1,$20,$b1,$72,$b1,$b6
 db $29,$00,$00
 db $a1,$00,$e0,$a2,$b1,$ee
 db $29,$00,$b0,$a2
 db $a1,$20,$b0,$a2,$b1,$ee
 db $a1,$20,$00,$b0,$a2
 db $91,$22,$e1,$47,$b1,$9f
 db $29,$00,$b1,$47
 db $9d,$24,$b1,$47,$b1,$9f
 db $29,$00,$b1,$47
 db $a1,$1e,$e0,$a2,$b1,$ee
 db $29,$00,$b0,$a2
 db $a1,$1c,$b0,$a2,$b1,$ee
 db $a1,$1c,$00,$b0,$a2
 db $91,$22,$e1,$47,$b1,$9f
 db $29,$00,$b1,$47
 db $a1,$00,$b1,$47,$b1,$9f
 db $29,$00,$b1,$47
 db $a1,$00,$e0,$d8,$b2,$2a
 db $29,$00,$b0,$d8
 db $a1,$1a,$b0,$da,$b2,$2a
 db $a1,$1a,$00,$b0,$da
 db $91,$22,$e1,$b6,$b2,$2a
 db $29,$00,$b1,$b6
 db $9d,$24,$b1,$b6,$b2,$2a
 db $29,$00,$b1,$b6
 db $a1,$16,$e0,$da,$b2,$2a
 db $29,$00,$b0,$da
 db $a1,$12,$b0,$da,$b2,$2a
 db $a1,$12,$00,$b0,$da
 db $91,$22,$e1,$b6,$b2,$2a
 db $29,$00,$b1,$b6
 db $99,$14,$b1,$b6,$b2,$2a
 db $99,$14,$00,$b1,$b6
 db $a1,$00,$e1,$15,$b0,$b9
 db $29,$00,$00
 db $a1,$00,$b1,$15,$00
 db $29,$00,$00
 db $99,$30,$e2,$2a,$b2,$93
 db $29,$00,$00
 db $99,$30,$b2,$2a,$b2,$e4
 db $a1,$00,$00,$00
 db $29,$e1,$15,$b0,$b9
 db $99,$43,$30,$b2,$93,$f0,$b9
 db $a1,$c1,$00,$b1,$15,$b0,$b9
 db $a9,$03,$b2,$e4,$f0,$b9
 db $a1,$c1,$00,$e2,$2a,$b2,$93
 db $29,$00,$00
 db $29,$b2,$2a,$b2,$e4
 db $29,$00,$00
 db $a1,$00,$e1,$15,$b0,$b9
 db $a9,$03,$b2,$93,$f0,$b9
 db $a1,$c1,$00,$b1,$15,$b0,$b9
 db $a9,$03,$b2,$e4,$f0,$b9
 db $99,$c1,$30,$e1,$15,$b1,$49
 db $29,$00,$00
 db $a1,$00,$b1,$15,$b1,$72
 db $29,$00,$00
 db $29,$e1,$15,$b0,$b9
 db $99,$30,$00,$00
 db $a1,$00,$b1,$15,$00
 db $29,$00,$00
 db $99,$30,$e1,$15,$b1,$49
 db $29,$00,$00
 db $a1,$00,$b1,$15,$b1,$72
 db $29,$00,$00
 db $a1,$00,$e1,$25,$b0,$b9
 db $a9,$03,$b1,$49,$f0,$b9
 db $a1,$c1,$00,$b1,$25,$b0,$b9
 db $a9,$03,$b1,$72,$f0,$b9
 db $99,$c1,$30,$e1,$25,$b1,$49
 db $29,$00,$00
 db $99,$30,$b1,$25,$b1,$72
 db $a1,$00,$00,$00
 db $29,$e1,$25,$b0,$b9
 db $99,$30,$00,$00
 db $a1,$00,$b1,$25,$00
 db $29,$00,$00
 db $a1,$00,$e1,$25,$b1,$49
 db $29,$00,$00
 db $29,$b1,$25,$b1,$72
 db $29,$00,$00
 db $a1,$00,$e1,$49,$b0,$b9
 db $a9,$03,$b1,$49,$f0,$b9
 db $a1,$c1,$00,$00,$b0,$b9
 db $a9,$03,$b1,$72,$f0,$b9
 db $99,$c1,$30,$e1,$47,$b1,$49
 db $29,$00,$00
 db $a1,$00,$b1,$49,$b1,$72
 db $29,$00,$00
 db $29,$e1,$49,$b0,$b9
 db $99,$30,$00,$00
 db $a1,$00,$b1,$49,$00
 db $29,$00,$00
 db $99,$30,$e1,$47,$b1,$49
 db $29,$00,$00
 db $a1,$00,$b1,$49,$b1,$72
 db $29,$00,$00
 db $a1,$00,$e1,$15,$b0,$b9
 db $a9,$03,$b1,$49,$f0,$b9
 db $a1,$c1,$00,$b1,$15,$b0,$b9
 db $a9,$03,$b1,$72,$f0,$b9
 db $99,$c1,$30,$e2,$2a,$b2,$93
 db $29,$00,$00
 db $99,$30,$b2,$2a,$b2,$e4
 db $a1,$00,$00,$00
 db $29,$e1,$15,$b0,$b9
 db $99,$43,$30,$b2,$93,$f0,$b9
 db $a1,$c1,$00,$b1,$15,$b0,$b9
 db $a9,$03,$b2,$e4,$f0,$b9
 db $a1,$c1,$00,$e2,$2a,$b2,$93
 db $29,$00,$00
 db $29,$b2,$2a,$b2,$e4
 db $29,$00,$00
 db $a1,$00,$e1,$15,$b0,$b9
 db $a9,$03,$b2,$93,$f0,$b9
 db $a1,$c1,$00,$b1,$15,$b0,$b9
 db $a9,$03,$b2,$e4,$f0,$b9
 db $99,$c1,$30,$e2,$2a,$b1,$49
 db $29,$00,$00
 db $a1,$00,$b2,$2a,$b1,$72
 db $29,$00,$00
 db $29,$e1,$15,$b0,$b9
 db $99,$30,$00,$00
 db $a1,$00,$b1,$15,$00
 db $29,$00,$00
 db $99,$30,$e2,$2a,$b1,$49
 db $29,$00,$00
 db $a1,$00,$b2,$2a,$b1,$72
 db $29,$00,$00
 db $a1,$00,$e1,$25,$b0,$b9
 db $a9,$03,$b1,$49,$f0,$b9
 db $a1,$c1,$00,$b1,$25,$b0,$b9
 db $a9,$03,$b1,$72,$f0,$b9
 db $99,$c1,$30,$e2,$4b,$b1,$49
 db $29,$00,$00
 db $99,$30,$b2,$4b,$b1,$72
 db $a1,$00,$00,$00
 db $29,$e1,$25,$b0,$b9
 db $99,$43,$30,$b1,$49,$f0,$b9
 db $a1,$c1,$00,$b1,$25,$b0,$b9
 db $a9,$03,$b1,$72,$f0,$b9
 db $a1,$c1,$00,$e2,$4b,$b1,$49
 db $29,$00,$00
 db $a1,$00,$b2,$4b,$b1,$72
 db $a1,$00,$00,$00
 db $99,$30,$e1,$49,$e1,$70
 db $29,$00,$00
 db $29,$b1,$49,$b1,$70
 db $29,$00,$00
 db $29,$e2,$93,$e2,$91
 db $29,$00,$00
 db $29,$b2,$93,$b2,$e2
 db $29,$00,$00
 db $29,$e1,$49,$e1,$70
 db $29,$00,$00
 db $29,$b1,$49,$b1,$70
 db $29,$00,$00
 db $29,$e2,$93,$e2,$91
 db $29,$00,$00
 db $29,$b2,$93,$b2,$e2
 db $29,$00,$00
 db $a6,$00,$b0,$8a,$e1,$9f
 db $2e,$b1,$15,$b1,$9f
 db $9c,$00,$b2,$2a,$e3,$3f
 db $9c,$00,$00,$00
 db $9e,$30,$b1,$ee,$b3,$3f
 db $2e,$00,$00
 db $24,$b2,$2a,$e3,$3f
 db $94,$14,$00,$b3,$3f
 db $a6,$00,$b0,$8a,$e1,$9f
 db $2e,$b1,$15,$b1,$9f
 db $9c,$00,$b2,$2a,$e1,$9f
 db $9c,$00,$00,$00
 db $9e,$30,$b1,$ee,$b1,$9f
 db $2e,$00,$00
 db $24,$b2,$2a,$e1,$9f
 db $24,$00,$00
 db $a6,$00,$b0,$8a,$b1,$9f
 db $2e,$b1,$15,$e1,$9f
 db $9c,$00,$b2,$2a,$b1,$9f
 db $9c,$00,$00,$00
 db $9e,$30,$b1,$ee,$e3,$3f
 db $2e,$00,$00
 db $24,$b2,$2a,$b1,$9f
 db $94,$30,$00,$e1,$9f
 db $a6,$00,$b2,$4b,$b3,$3f
 db $a6,$00,$00,$e3,$3f
 db $94,$30,$b2,$2a,$b1,$9f
 db $24,$00,$00
 db $a6,$00,$b1,$ee,$e3,$3f
 db $a6,$00,$00,$00
 db $94,$30,$b2,$2a,$b1,$9f
 db $24,$00,$00
 db $a6,$00,$e2,$49,$b1,$72
 db $2e,$00,$00
 db $9c,$00,$b0,$90,$b0,$92
 db $9c,$00,$00,$00
 db $9e,$30,$e2,$49,$b1,$72
 db $2e,$00,$00
 db $9c,$00,$b2,$49,$b1,$49
 db $9c,$00,$00,$00
 db $a6,$00,$e0,$90,$b0,$92
 db $2e,$00,$00
 db $9c,$00,$b0,$90,$00
 db $9c,$00,$00,$00
 db $9e,$30,$e2,$49,$b1,$72
 db $2e,$00,$00
 db $9c,$00,$b0,$90,$b0,$92
 db $9c,$00,$00,$00
 db $a6,$00,$e2,$91,$b1,$b8
 db $2e,$00,$00
 db $9c,$00,$b2,$91,$b1,$9f
 db $24,$00,$00
 db $9e,$14,$e0,$a2,$b0,$a4
 db $2e,$00,$00
 db $9c,$00,$b2,$91,$b1,$72
 db $24,$00,$00
 db $a6,$00,$e0,$a2,$b0,$a4
 db $2e,$00,$00
 db $94,$14,$b2,$8f,$b1,$49
 db $24,$00,$00
 db $a6,$00,$e0,$a0,$b0,$a4
 db $2e,$00,$00
 db $94,$14,$b2,$8f,$b1,$72
 db $24,$00,$00
 db $a6,$00,$b0,$8a,$e1,$9f
 db $2e,$b1,$15,$b1,$9f
 db $9c,$00,$b2,$2a,$e3,$3f
 db $9c,$00,$00,$00
 db $9e,$30,$b1,$ee,$b3,$3f
 db $2e,$00,$00
 db $24,$b2,$2a,$e3,$3f
 db $94,$14,$00,$b3,$3f
 db $a6,$00,$b0,$8a,$e1,$9f
 db $2e,$b1,$15,$b1,$9f
 db $9c,$00,$b2,$2a,$e1,$9f
 db $9c,$00,$00,$00
 db $9e,$30,$b1,$ee,$b1,$9f
 db $2e,$00,$00
 db $24,$b2,$2a,$e1,$9f
 db $24,$00,$00
 db $a6,$00,$b0,$8a,$b1,$9f
 db $2e,$b1,$15,$e1,$9f
 db $9c,$00,$b2,$2a,$b3,$3f
 db $9c,$00,$00,$00
 db $9e,$30,$b1,$ee,$e3,$3f
 db $2e,$00,$00
 db $24,$b2,$2a,$b3,$3f
 db $94,$30,$00,$e3,$3f
 db $a6,$00,$b2,$4b,$b3,$3f
 db $a6,$00,$00,$e3,$3f
 db $94,$30,$b2,$2a,$b3,$3f
 db $24,$00,$00
 db $a6,$00,$b1,$ee,$e3,$3f
 db $a6,$00,$00,$00
 db $94,$30,$b2,$2a,$b3,$3f
 db $24,$00,$00
 db $a6,$00,$e2,$49,$b2,$e4
 db $2e,$00,$00
 db $9c,$00,$b1,$23,$b0,$92
 db $9c,$00,$00,$00
 db $9e,$30,$e2,$49,$b2,$e4
 db $2e,$00,$00
 db $9c,$00,$b2,$49,$b2,$93
 db $9c,$00,$00,$00
 db $a6,$00,$e1,$23,$b0,$92
 db $2e,$00,$00
 db $9c,$00,$b1,$23,$00
 db $9c,$00,$00,$00
 db $9e,$30,$e2,$49,$b2,$e4
 db $2e,$00,$00
 db $9c,$00,$b1,$23,$b0,$92
 db $24,$00,$00
 db $9e,$14,$e2,$91,$b3,$70
 db $2e,$00,$00
 db $9c,$00,$b2,$91,$b3,$3f
 db $24,$00,$00
 db $9e,$14,$e1,$47,$b0,$f7
 db $2e,$00,$00
 db $9c,$00,$b2,$91,$b2,$e4
 db $24,$00,$00
 db $9e,$14,$e1,$47,$b0,$f7
 db $2e,$00,$00
 db $9c,$00,$b2,$8f,$b2,$93
 db $24,$00,$00
 db $9e,$14,$e1,$45,$b0,$f7
 db $2e,$00,$00
 db $94,$14,$b2,$8f,$b2,$e4
 db $24,$00,$00
 db $a1,$43,$00,$e0,$90,$90,$92
 db $29,$01,$01
 db $29,$b0,$90,$90,$92
 db $29,$01,$01
 db $91,$cb,$22,$e2,$49,$82,$4b
 db $29,$00,$00
 db $a9,$03,$b0,$90,$90,$92
 db $29,$01,$01
 db $a9,$8b,$e3,$3d,$83,$3f
 db $29,$00,$00
 db $a1,$00,$b3,$6e,$83,$70
 db $a1,$00,$00,$00
 db $91,$43,$22,$e3,$3d,$90,$92
 db $29,$00,$01
 db $a9,$8b,$b3,$da,$83,$dc
 db $29,$00,$00
 db $a1,$43,$00,$e3,$6e,$90,$92
 db $29,$00,$01
 db $a9,$8b,$b3,$6e,$83,$70
 db $29,$00,$00
 db $91,$43,$22,$e3,$da,$90,$92
 db $29,$00,$01
 db $a9,$8b,$b3,$3d,$83,$3f
 db $29,$00,$00
 db $a9,$03,$e3,$6e,$90,$92
 db $29,$00,$01
 db $a1,$00,$b3,$3d,$90,$92
 db $a1,$00,$00,$01
 db $91,$cb,$22,$e2,$49,$82,$4b
 db $29,$00,$00
 db $29,$b2,$91,$82,$93
 db $29,$00,$00
 db $a1,$45,$00,$e0,$88,$f2,$2a
 db $29,$01,$e2,$93
 db $29,$b0,$88,$d3,$3f
 db $a9,$03,$e2,$93,$c2,$2a
 db $91,$45,$22,$e0,$88,$b2,$93
 db $a9,$03,$c2,$2a,$a3,$3f
 db $a9,$05,$b0,$88,$90,$8a
 db $a9,$03,$a3,$3f,$01
 db $a9,$05,$e0,$88,$90,$8a
 db $29,$01,$01
 db $a1,$cb,$00,$b3,$3d,$83,$3f
 db $a1,$00,$00,$00
 db $91,$22,$e3,$6e,$83,$70
 db $29,$00,$00
 db $29,$b3,$da,$83,$dc
 db $29,$00,$00
 db $a1,$00,$e4,$53,$84,$55
 db $29,$00,$00
 db $a9,$03,$b3,$da,$90,$8a
 db $29,$00,$01
 db $91,$cb,$22,$e3,$da,$83,$dc
 db $29,$00,$00
 db $a9,$03,$b0,$88,$90,$8a
 db $29,$00,$01
 db $a9,$8b,$e3,$6e,$83,$70
 db $29,$00,$00
 db $a1,$43,$00,$b3,$da,$90,$8a
 db $a1,$00,$00,$01
 db $91,$cb,$22,$e2,$28,$82,$2a
 db $29,$00,$00
 db $29,$b2,$91,$b2,$93
 db $29,$00,$00
 db $a1,$00,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $91,$22,$00,$00
 db $29,$00,$00
 db $29,$e2,$e2,$b2,$e4
 db $29,$00,$00
 db $a9,$03,$00,$90,$7b
 db $29,$00,$01
 db $a1,$00,$b0,$79,$90,$7b
 db $a1,$00,$01,$01
 db $91,$cb,$22,$e2,$e2,$82,$e4
 db $29,$00,$00
 db $29,$b3,$6e,$b3,$70
 db $29,$00,$00
 db $a1,$43,$00,$e2,$e2,$90,$8a
 db $29,$00,$01
 db $a9,$8b,$b3,$3d,$b3,$3f
 db $29,$00,$00
 db $91,$43,$22,$e3,$6e,$90,$8a
 db $29,$00,$01
 db $a9,$8b,$b2,$e2,$b2,$e4
 db $29,$00,$00
 db $a9,$03,$e3,$3d,$90,$8a
 db $29,$00,$01
 db $a1,$cb,$00,$b2,$28,$b2,$2a
 db $a1,$00,$00,$00
 db $91,$43,$22,$e2,$e2,$90,$8a
 db $29,$00,$01
 db $a9,$8b,$b2,$49,$82,$4b
 db $29,$00,$00
 db $a1,$45,$00,$e0,$90,$90,$92
 db $a9,$03,$e2,$2a,$01
 db $a9,$05,$b0,$90,$90,$92
 db $a9,$03,$b2,$4b,$01
 db $91,$45,$22,$e0,$90,$90,$92
 db $a9,$03,$e2,$4b,$01
 db $a9,$05,$b0,$90,$90,$92
 db $29,$01,$01
 db $29,$e0,$90,$90,$92
 db $29,$01,$01
 db $a1,$00,$b0,$90,$90,$92
 db $a1,$00,$01,$01
 db $91,$cb,$22,$e2,$e0,$92,$e4
 db $29,$00,$00
 db $29,$e3,$6c,$93,$70
 db $29,$00,$00
 db $a1,$43,$00,$e2,$e2,$90,$a4
 db $29,$00,$01
 db $a9,$8b,$b3,$3b,$93,$3f
 db $29,$00,$00
 db $91,$43,$22,$e3,$6e,$90,$a4
 db $29,$00,$01
 db $a9,$8b,$b2,$e0,$92,$e4
 db $29,$00,$00
 db $a9,$03,$e3,$3d,$90,$a4
 db $29,$00,$01
 db $a1,$cb,$00,$b3,$3b,$93,$3f
 db $a1,$00,$00,$00
 db $91,$43,$22,$e2,$e2,$90,$a4
 db $29,$00,$01
 db $a9,$8b,$b3,$6c,$93,$70
 db $29,$00,$00
 db $a1,$45,$00,$e0,$90,$f2,$4b
 db $29,$01,$e2,$e4
 db $a1,$32,$b0,$90,$d3,$70
 db $a9,$03,$e2,$e4,$c2,$4b
 db $91,$45,$22,$e0,$90,$b2,$e4
 db $a9,$03,$c2,$4b,$a3,$70
 db $a1,$45,$32,$b0,$90,$90,$92
 db $a1,$43,$32,$a3,$70,$01
 db $a9,$05,$e0,$90,$90,$92
 db $29,$01,$01
 db $a1,$cb,$00,$b4,$93,$92,$4b
 db $a1,$00,$00,$00
 db $91,$22,$e6,$7a,$93,$3f
 db $29,$00,$00
 db $a1,$32,$b6,$dd,$93,$70
 db $29,$00,$00
 db $a1,$00,$e7,$b5,$93,$dc
 db $29,$00,$00
 db $a1,$43,$32,$b1,$b6,$90,$92
 db $29,$00,$01
 db $91,$cb,$22,$e6,$dd,$93,$70
 db $29,$00,$00
 db $a1,$32,$b6,$7a,$93,$3f
 db $a1,$32,$00,$00
 db $a9,$03,$b1,$b6,$90,$92
 db $29,$00,$01
 db $a1,$00,$b1,$9d,$90,$92
 db $a1,$00,$00,$01
 db $91,$cb,$22,$e4,$93,$92,$4b
 db $29,$00,$00
 db $a1,$32,$b5,$23,$92,$93
 db $29,$00,$00
 db $a1,$45,$00,$e0,$88,$f2,$2a
 db $29,$01,$e2,$93
 db $a1,$32,$b0,$88,$d3,$3f
 db $a9,$03,$e2,$93,$c2,$2a
 db $91,$45,$22,$e0,$88,$b2,$93
 db $a9,$03,$c2,$2a,$a3,$3f
 db $a1,$45,$32,$b0,$88,$90,$8a
 db $a1,$43,$32,$a3,$3f,$01
 db $a9,$05,$e0,$88,$90,$8a
 db $29,$01,$01
 db $a1,$cb,$00,$b6,$7a,$83,$3f
 db $a1,$00,$00,$00
 db $91,$22,$e6,$dd,$83,$70
 db $29,$00,$00
 db $a1,$32,$b7,$b5,$83,$dc
 db $29,$00,$00
 db $a1,$00,$e8,$a7,$84,$55
 db $29,$00,$00
 db $a1,$43,$32,$b1,$ec,$90,$8a
 db $29,$00,$01
 db $91,$cb,$22,$e7,$b5,$83,$dc
 db $29,$00,$00
 db $a1,$43,$32,$b0,$88,$90,$8a
 db $a1,$32,$00,$01
 db $a9,$8b,$e6,$dd,$83,$70
 db $29,$00,$00
 db $a1,$43,$00,$b1,$ec,$90,$8a
 db $a1,$00,$00,$01
 db $91,$cb,$22,$e4,$51,$82,$2a
 db $29,$00,$00
 db $a1,$32,$b5,$23,$b2,$93
 db $29,$00,$00
 db $a1,$00,$00,$00
 db $29,$00,$00
 db $a1,$32,$00,$00
 db $29,$00,$00
 db $91,$22,$00,$00
 db $29,$00,$00
 db $a1,$32,$e5,$c5,$b2,$e4
 db $a1,$32,$00,$00
 db $a9,$03,$e1,$70,$90,$7b
 db $29,$00,$01
 db $a1,$00,$b0,$79,$90,$7b
 db $a1,$00,$01,$01
 db $91,$cb,$22,$e5,$c5,$82,$e4
 db $29,$00,$00
 db $a1,$32,$b6,$df,$b3,$70
 db $29,$00,$00
 db $a1,$43,$00,$e1,$70,$90,$8a
 db $29,$00,$01
 db $a1,$cb,$32,$b6,$7a,$b3,$3f
 db $29,$00,$00
 db $91,$43,$22,$e1,$b6,$90,$8a
 db $29,$00,$01
 db $a1,$cb,$32,$b5,$c5,$b2,$e4
 db $a1,$32,$00,$00
 db $a9,$03,$e1,$9d,$90,$8a
 db $29,$00,$01
 db $a1,$cb,$00,$b4,$51,$b2,$2a
 db $a1,$00,$00,$00
 db $91,$43,$22,$e1,$70,$90,$8a
 db $29,$00,$01
 db $a1,$cb,$32,$b4,$93,$82,$4b
 db $29,$00,$00
 db $a1,$45,$00,$e0,$90,$f2,$4b
 db $29,$01,$e2,$e4
 db $a1,$32,$b0,$90,$d3,$70
 db $a9,$03,$e2,$e4,$c2,$4b
 db $91,$45,$22,$e0,$90,$b2,$e4
 db $a9,$03,$c2,$4b,$a3,$70
 db $a1,$45,$32,$b0,$90,$90,$92
 db $a1,$43,$32,$a3,$70,$01
 db $a9,$05,$e0,$90,$90,$92
 db $29,$01,$01
 db $a1,$00,$b0,$90,$90,$92
 db $a1,$00,$01,$01
 db $91,$c1,$22,$e2,$4b,$92,$e4
 db $29,$00,$00
 db $91,$22,$b2,$e4,$93,$70
 db $29,$00,$00
 db $a1,$43,$00,$e2,$e4,$90,$a4
 db $29,$00,$00
 db $91,$c1,$22,$b2,$93,$93,$3f
 db $29,$00,$00
 db $a1,$43,$00,$e3,$70,$90,$a4
 db $29,$00,$00
 db $91,$c1,$22,$b2,$4b,$92,$e4
 db $29,$00,$00
 db $a1,$43,$00,$e3,$3f,$90,$a4
 db $29,$00,$00
 db $91,$c1,$22,$b2,$93,$93,$3f
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $a1,$00,$e1,$72,$b2,$2a
 db $29,$00,$00
 db $a1,$12,$b1,$72,$b1,$ee
 db $a1,$12,$00,$00
 db $99,$14,$e0,$b7,$b1,$72
 db $29,$00,$00
 db $a1,$00,$b1,$72,$b1,$ee
 db $29,$00,$00
 db $a1,$16,$e0,$b7,$b1,$72
 db $29,$00,$00
 db $a1,$18,$b1,$72,$b1,$b8
 db $29,$00,$00
 db $99,$14,$e1,$72,$b1,$ee
 db $29,$00,$00
 db $a1,$00,$b1,$72,$b1,$b8
 db $29,$00,$00
 db $a1,$00,$e1,$72,$b2,$2a
 db $29,$00,$00
 db $a1,$1a,$b1,$72,$b1,$ee
 db $a1,$1a,$00,$00
 db $99,$14,$e0,$b7,$b1,$72
 db $29,$00,$00
 db $a1,$1c,$b1,$72,$b1,$ee
 db $29,$00,$00
 db $a1,$1e,$e0,$b7,$b1,$72
 db $29,$00,$00
 db $a1,$00,$b1,$72,$b1,$b8
 db $29,$00,$00
 db $99,$14,$e1,$72,$b1,$ee
 db $29,$00,$00
 db $a1,$20,$b1,$72,$b1,$b6
 db $29,$00,$00
 db $a1,$00,$e0,$a2,$b1,$ee
 db $29,$00,$b0,$a2
 db $a1,$0c,$b0,$a2,$b1,$ee
 db $a1,$0c,$00,$b0,$a2
 db $91,$22,$e1,$47,$b1,$9f
 db $29,$00,$b1,$47
 db $9d,$24,$b1,$47,$b1,$9f
 db $29,$00,$b1,$47
 db $a1,$26,$e0,$a2,$b1,$ee
 db $29,$00,$b0,$a2
 db $a1,$0e,$b0,$a2,$b1,$ee
 db $a1,$0e,$00,$b0,$a2
 db $91,$22,$e1,$47,$b1,$9f
 db $29,$00,$b1,$47
 db $a1,$00,$b1,$47,$b1,$9f
 db $29,$00,$b1,$47
 db $a1,$00,$e0,$d8,$b2,$2a
 db $29,$00,$b0,$d8
 db $a1,$10,$b0,$da,$b2,$2a
 db $a1,$10,$00,$b0,$da
 db $91,$22,$e1,$b6,$b2,$2a
 db $29,$00,$b1,$b6
 db $9d,$24,$b1,$b6,$b2,$2a
 db $29,$00,$b1,$b6
 db $a1,$28,$e0,$da,$b2,$2a
 db $29,$00,$b0,$da
 db $a1,$2a,$b0,$da,$b2,$2a
 db $a1,$2a,$00,$b0,$da
 db $91,$22,$e1,$b6,$b2,$2a
 db $29,$00,$b1,$b6
 db $a1,$00,$b1,$b6,$b2,$2a
 db $99,$14,$00,$b1,$b6
 db $a1,$00,$e1,$72,$b2,$2a
 db $29,$00,$00
 db $a1,$2a,$b1,$72,$b1,$ee
 db $a1,$2a,$00,$00
 db $99,$14,$e0,$b7,$b1,$72
 db $29,$00,$00
 db $a1,$00,$b1,$72,$b1,$ee
 db $29,$00,$00
 db $a1,$2c,$e0,$b7,$b1,$72
 db $29,$00,$00
 db $a1,$10,$b1,$72,$b1,$b8
 db $29,$00,$00
 db $99,$14,$e1,$72,$b1,$ee
 db $29,$00,$00
 db $a1,$00,$b1,$72,$b1,$b8
 db $29,$00,$00
 db $a1,$00,$e1,$72,$b2,$2a
 db $29,$00,$00
 db $a1,$0e,$b1,$72,$b1,$ee
 db $a1,$0e,$00,$00
 db $99,$14,$e0,$b7,$b1,$72
 db $29,$00,$00
 db $a1,$0c,$b1,$72,$b1,$ee
 db $29,$00,$00
 db $a1,$2e,$e0,$b7,$b1,$72
 db $29,$00,$00
 db $a1,$00,$b1,$72,$b1,$b8
 db $29,$00,$00
 db $99,$14,$e1,$72,$b1,$ee
 db $29,$00,$00
 db $a1,$20,$b1,$72,$b1,$b6
 db $29,$00,$00
 db $a1,$00,$e0,$a2,$b1,$ee
 db $29,$00,$b0,$a2
 db $a1,$20,$b0,$a2,$b1,$ee
 db $a1,$20,$00,$b0,$a2
 db $91,$22,$e1,$47,$b1,$9f
 db $29,$00,$b1,$47
 db $9d,$24,$b1,$47,$b1,$9f
 db $29,$00,$b1,$47
 db $a1,$1e,$e0,$a2,$b1,$ee
 db $29,$00,$b0,$a2
 db $a1,$1c,$b0,$a2,$b1,$ee
 db $a1,$1c,$00,$b0,$a2
 db $91,$22,$e1,$47,$b1,$9f
 db $29,$00,$b1,$47
 db $a1,$00,$b1,$47,$b1,$9f
 db $29,$00,$b1,$47
 db $a1,$00,$e0,$d8,$b2,$2a
 db $29,$00,$b0,$d8
 db $a1,$1a,$b0,$da,$b2,$2a
 db $a1,$1a,$00,$b0,$da
 db $91,$22,$e1,$b6,$b2,$2a
 db $29,$00,$b1,$b6
 db $9d,$24,$b1,$b6,$b2,$2a
 db $29,$00,$b1,$b6
 db $a1,$16,$e0,$da,$b2,$2a
 db $29,$00,$b0,$da
 db $a1,$12,$b0,$da,$b2,$2a
 db $a1,$12,$00,$b0,$da
 db $91,$22,$e1,$b6,$b2,$2a
 db $29,$00,$b1,$b6
 db $99,$14,$b1,$b6,$b2,$2a
 db $99,$14,$00,$b1,$b6
 db $a1,$00,$e1,$15,$b0,$b9
 db $29,$00,$00
 db $a1,$00,$b1,$15,$00
 db $29,$00,$00
 db $99,$30,$e2,$2a,$b2,$93
 db $29,$00,$00
 db $99,$30,$b2,$2a,$b2,$e4
 db $a1,$00,$00,$00
 db $29,$e1,$15,$b0,$b9
 db $99,$30,$00,$00
 db $a1,$00,$b1,$15,$00
 db $29,$00,$00
 db $a1,$00,$e2,$2a,$b2,$93
 db $29,$00,$00
 db $29,$b2,$2a,$b2,$e4
 db $29,$00,$00
 db $a1,$00,$e1,$15,$b0,$b9
 db $29,$00,$00
 db $a1,$00,$b1,$15,$00
 db $29,$00,$00
 db $99,$30,$e1,$15,$b1,$49
 db $29,$00,$00
 db $a1,$00,$b1,$15,$b1,$72
 db $29,$00,$00
 db $29,$e1,$15,$b0,$b9
 db $99,$30,$00,$00
 db $a1,$00,$b1,$15,$00
 db $29,$00,$00
 db $99,$30,$e1,$15,$b1,$49
 db $29,$00,$00
 db $a1,$00,$b1,$15,$b1,$72
 db $29,$00,$00
 db $a1,$00,$e1,$25,$b0,$b9
 db $29,$00,$00
 db $a1,$00,$b1,$25,$00
 db $29,$00,$00
 db $99,$30,$e1,$25,$b1,$49
 db $29,$00,$00
 db $99,$30,$b1,$25,$b1,$72
 db $a1,$00,$00,$00
 db $29,$e1,$25,$b0,$b9
 db $99,$30,$00,$00
 db $a1,$00,$b1,$25,$00
 db $29,$00,$00
 db $a1,$00,$e1,$25,$b1,$49
 db $29,$00,$00
 db $29,$b1,$25,$b1,$72
 db $29,$00,$00
 db $a1,$00,$e1,$49,$b0,$b9
 db $29,$00,$00
 db $a1,$00,$b1,$49,$00
 db $29,$00,$00
 db $99,$30,$e1,$47,$b1,$49
 db $29,$00,$00
 db $a1,$00,$b1,$49,$b1,$72
 db $29,$00,$00
 db $29,$e1,$49,$b0,$b9
 db $99,$30,$00,$00
 db $a1,$00,$b1,$49,$00
 db $29,$00,$00
 db $99,$30,$e1,$47,$b1,$49
 db $29,$00,$00
 db $a1,$00,$b1,$49,$b1,$72
 db $29,$00,$00
 db $a1,$00,$e1,$15,$b0,$b9
 db $29,$00,$00
 db $a1,$00,$b1,$15,$00
 db $29,$00,$00
 db $99,$30,$e2,$2a,$b2,$93
 db $29,$00,$00
 db $99,$30,$b2,$2a,$b2,$e4
 db $a1,$00,$00,$00
 db $29,$e1,$15,$b0,$b9
 db $99,$30,$00,$00
 db $a1,$00,$b1,$15,$00
 db $29,$00,$00
 db $a1,$00,$e2,$2a,$b2,$93
 db $29,$00,$00
 db $29,$b2,$2a,$b2,$e4
 db $29,$00,$00
 db $a1,$00,$e1,$15,$b0,$b9
 db $29,$00,$00
 db $a1,$00,$b1,$15,$00
 db $29,$00,$00
 db $99,$30,$e2,$2a,$b1,$49
 db $29,$00,$00
 db $a1,$00,$b2,$2a,$b1,$72
 db $29,$00,$00
 db $29,$e1,$15,$b0,$b9
 db $99,$30,$00,$00
 db $a1,$00,$b1,$15,$00
 db $29,$00,$00
 db $99,$30,$e2,$2a,$b1,$49
 db $29,$00,$00
 db $a1,$00,$b2,$2a,$b1,$72
 db $29,$00,$00
 db $a1,$00,$e1,$25,$b0,$b9
 db $29,$00,$00
 db $a1,$00,$b1,$25,$00
 db $29,$00,$00
 db $99,$30,$e2,$4b,$b1,$49
 db $29,$00,$00
 db $99,$30,$b2,$4b,$b1,$72
 db $a1,$00,$00,$00
 db $29,$e1,$25,$b0,$b9
 db $99,$30,$00,$00
 db $a1,$00,$b1,$25,$00
 db $29,$00,$00
 db $a1,$00,$e2,$4b,$b1,$49
 db $29,$00,$00
 db $a1,$00,$b2,$4b,$b1,$72
 db $a1,$00,$00,$00
 db $99,$30,$e1,$49,$e1,$70
 db $29,$00,$00
 db $29,$b1,$49,$b1,$70
 db $29,$00,$00
 db $29,$e2,$93,$e2,$91
 db $29,$00,$00
 db $29,$b2,$93,$b2,$e2
 db $29,$00,$00
 db $29,$e2,$93,$b2,$91
 db $29,$00,$00
 db $29,$e2,$e4,$b2,$e2
 db $29,$00,$00
 db $a9,$03,$e2,$93,$01
 db $29,$00,$00
 db $29,$b2,$e4,$00
 db $29,$00,$00
 db $a6,$c1,$00,$b0,$8a,$e1,$9f
 db $2e,$b1,$15,$b1,$9f
 db $9c,$00,$b2,$2a,$e3,$3f
 db $9c,$00,$00,$00
 db $9e,$30,$b1,$ee,$b3,$3f
 db $2e,$00,$00
 db $24,$b2,$2a,$e3,$3f
 db $94,$14,$00,$b3,$3f
 db $a6,$00,$b0,$8a,$e1,$9f
 db $2e,$b1,$15,$b1,$9f
 db $9c,$00,$b2,$2a,$e1,$9f
 db $9c,$00,$00,$00
 db $9e,$30,$b1,$ee,$b1,$9f
 db $2e,$00,$00
 db $24,$b2,$2a,$e1,$9f
 db $24,$00,$00
 db $a6,$00,$b0,$8a,$b1,$9f
 db $2e,$b1,$15,$e1,$9f
 db $9c,$00,$b2,$2a,$b1,$9f
 db $9c,$00,$00,$00
 db $9e,$30,$b1,$ee,$e3,$3f
 db $2e,$00,$00
 db $24,$b2,$2a,$b1,$9f
 db $94,$30,$00,$e1,$9f
 db $a6,$00,$b2,$4b,$b3,$3f
 db $a6,$00,$00,$e3,$3f
 db $94,$30,$b2,$2a,$b1,$9f
 db $24,$00,$00
 db $a6,$00,$b1,$ee,$e3,$3f
 db $a6,$00,$00,$00
 db $94,$30,$b2,$2a,$b1,$9f
 db $24,$00,$00
 db $a6,$00,$e2,$49,$b1,$72
 db $2e,$00,$00
 db $9c,$00,$b1,$23,$b1,$b8
 db $9c,$00,$00,$00
 db $9e,$30,$e2,$49,$b1,$72
 db $2e,$00,$00
 db $9c,$00,$b2,$49,$b1,$49
 db $9c,$00,$00,$00
 db $a6,$00,$e1,$23,$b1,$b8
 db $2e,$00,$00
 db $9c,$00,$b1,$23,$00
 db $9c,$00,$00,$00
 db $9e,$30,$e2,$49,$b1,$72
 db $2e,$00,$00
 db $9c,$00,$b1,$23,$b1,$b8
 db $9c,$00,$00,$00
 db $a6,$00,$e2,$91,$00
 db $2e,$00,$00
 db $9c,$00,$b2,$91,$b1,$9f
 db $24,$00,$00
 db $9e,$14,$e1,$47,$00
 db $2e,$00,$00
 db $9c,$00,$b2,$91,$b1,$72
 db $24,$00,$00
 db $a6,$00,$e1,$47,$b1,$9f
 db $2e,$00,$00
 db $94,$14,$b2,$8f,$b1,$49
 db $24,$00,$00
 db $a6,$00,$e1,$45,$b1,$9f
 db $2e,$00,$00
 db $94,$14,$b2,$8f,$b1,$72
 db $24,$00,$00
 db $a6,$00,$b0,$8a,$e1,$9f
 db $2e,$b1,$15,$b1,$9f
 db $9c,$00,$b2,$2a,$e3,$3f
 db $9c,$00,$00,$00
 db $9e,$30,$b1,$ee,$b3,$3f
 db $2e,$00,$00
 db $24,$b2,$2a,$e3,$3f
 db $94,$14,$00,$b3,$3f
 db $a6,$00,$b0,$8a,$e1,$9f
 db $2e,$b1,$15,$b1,$9f
 db $9c,$00,$b2,$2a,$e1,$9f
 db $9c,$00,$00,$00
 db $9e,$30,$b1,$ee,$b1,$9f
 db $2e,$00,$00
 db $24,$b2,$2a,$e1,$9f
 db $24,$00,$00
 db $a6,$00,$b0,$8a,$b1,$9f
 db $2e,$b1,$15,$e1,$9f
 db $9c,$00,$b2,$2a,$b3,$3f
 db $9c,$00,$00,$00
 db $9e,$30,$b1,$ee,$e3,$3f
 db $2e,$00,$00
 db $24,$b2,$2a,$b3,$3f
 db $94,$30,$00,$e3,$3f
 db $a6,$00,$b2,$4b,$b3,$3f
 db $a6,$00,$00,$e3,$3f
 db $94,$30,$b2,$2a,$b3,$3f
 db $24,$00,$00
 db $a6,$00,$b1,$ee,$e3,$3f
 db $a6,$00,$00,$00
 db $94,$30,$b2,$2a,$b3,$3f
 db $24,$00,$00
 db $a6,$00,$e2,$49,$b2,$e4
 db $2e,$00,$00
 db $9c,$00,$b1,$23,$b1,$b8
 db $9c,$00,$00,$00
 db $9e,$30,$e2,$49,$b2,$e4
 db $2e,$00,$00
 db $9c,$00,$b2,$49,$b2,$93
 db $9c,$00,$00,$00
 db $a6,$00,$e1,$23,$b1,$b8
 db $2e,$00,$00
 db $9c,$00,$b1,$23,$00
 db $9c,$00,$00,$00
 db $9e,$30,$e2,$49,$b2,$e4
 db $2e,$00,$00
 db $9c,$00,$b1,$23,$b1,$b8
 db $24,$00,$00
 db $9e,$14,$e2,$91,$b3,$70
 db $2e,$00,$00
 db $9c,$00,$b2,$91,$b3,$3f
 db $24,$00,$00
 db $9e,$14,$e1,$47,$b1,$9f
 db $2e,$00,$00
 db $9c,$00,$b2,$91,$b2,$e4
 db $24,$00,$00
 db $9e,$14,$e1,$47,$b1,$9f
 db $2e,$00,$00
 db $9c,$00,$b2,$8f,$b2,$93
 db $24,$00,$00
 db $9e,$14,$e1,$45,$b1,$9f
 db $2e,$00,$00
 db $94,$14,$b2,$8f,$b2,$e4
 db $24,$00,$00
 db $a1,$43,$00,$e0,$90,$90,$92
 db $29,$01,$01
 db $29,$b0,$90,$90,$92
 db $29,$01,$01
 db $91,$cb,$22,$e3,$70,$81,$25
 db $29,$00,$00
 db $a9,$03,$b0,$90,$90,$92
 db $29,$01,$01
 db $a9,$8b,$e4,$dd,$81,$9f
 db $29,$00,$00
 db $a1,$00,$b5,$27,$81,$b8
 db $a1,$00,$00,$00
 db $91,$43,$22,$e3,$3d,$90,$92
 db $29,$00,$01
 db $a9,$8b,$b5,$c9,$81,$ee
 db $29,$00,$00
 db $a1,$43,$00,$e3,$6e,$90,$92
 db $29,$00,$01
 db $a9,$8b,$b5,$27,$81,$b8
 db $29,$00,$00
 db $91,$43,$22,$e3,$da,$90,$92
 db $29,$00,$01
 db $a9,$8b,$b4,$dd,$81,$9f
 db $29,$00,$00
 db $a9,$03,$e3,$6e,$90,$92
 db $29,$00,$01
 db $a1,$00,$b3,$3d,$90,$92
 db $a1,$00,$00,$01
 db $91,$cb,$22,$e3,$70,$81,$25
 db $29,$00,$00
 db $a1,$2c,$b3,$dc,$81,$49
 db $a1,$2c,$00,$00
 db $a1,$45,$00,$e0,$88,$f2,$2a
 db $29,$01,$e2,$93
 db $29,$b0,$88,$d3,$3f
 db $a9,$03,$e2,$93,$c2,$2a
 db $91,$45,$22,$e0,$88,$b2,$93
 db $a9,$03,$c2,$2a,$a3,$3f
 db $a9,$05,$b0,$88,$90,$8a
 db $a9,$03,$a3,$3f,$01
 db $a9,$05,$e0,$88,$90,$8a
 db $29,$01,$01
 db $a1,$cb,$00,$b4,$dd,$81,$9f
 db $a1,$00,$00,$00
 db $91,$22,$e5,$27,$81,$b8
 db $29,$00,$00
 db $29,$b5,$c9,$81,$ee
 db $29,$00,$00
 db $a1,$00,$e6,$7e,$82,$2a
 db $29,$00,$00
 db $a9,$03,$b3,$da,$90,$8a
 db $29,$00,$01
 db $91,$cb,$22,$e5,$c9,$81,$ee
 db $29,$00,$00
 db $a9,$03,$b0,$88,$90,$8a
 db $29,$00,$01
 db $a9,$8b,$e5,$27,$81,$b8
 db $29,$00,$00
 db $a1,$43,$00,$b3,$da,$90,$8a
 db $a1,$00,$00,$01
 db $91,$cb,$22,$e3,$3f,$81,$15
 db $29,$00,$00
 db $a1,$2c,$b3,$dc,$b1,$49
 db $a1,$2c,$00,$00
 db $a1,$00,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $91,$22,$00,$00
 db $29,$00,$00
 db $29,$e4,$55,$b1,$72
 db $29,$00,$00
 db $a9,$03,$e2,$e2,$90,$7b
 db $29,$00,$01
 db $a1,$00,$b0,$79,$90,$7b
 db $a1,$00,$01,$01
 db $91,$cb,$22,$e4,$55,$81,$72
 db $29,$00,$00
 db $29,$b5,$27,$b1,$b8
 db $29,$00,$00
 db $a1,$43,$00,$e2,$e2,$90,$8a
 db $29,$00,$01
 db $a9,$8b,$b4,$dd,$b1,$9f
 db $29,$00,$00
 db $91,$43,$22,$e3,$6e,$90,$8a
 db $29,$00,$01
 db $a9,$8b,$b4,$55,$b1,$72
 db $29,$00,$00
 db $a9,$03,$e3,$3d,$90,$8a
 db $29,$00,$01
 db $a1,$cb,$00,$b3,$3f,$b1,$15
 db $a1,$00,$00,$00
 db $91,$43,$22,$e2,$e2,$90,$8a
 db $29,$00,$01
 db $a1,$cb,$2c,$b3,$70,$81,$25
 db $a1,$2c,$00,$00
 db $a1,$45,$00,$e0,$90,$90,$92
 db $a9,$03,$e2,$2a,$01
 db $a9,$05,$b0,$90,$90,$92
 db $a9,$03,$b2,$4b,$01
 db $91,$45,$22,$e0,$90,$90,$92
 db $a9,$03,$e2,$4b,$01
 db $a9,$05,$b0,$90,$90,$92
 db $29,$01,$01
 db $29,$e0,$90,$90,$92
 db $29,$01,$01
 db $a1,$00,$b0,$90,$90,$92
 db $a1,$00,$01,$01
 db $91,$4d,$22,$92,$e0,$92,$e4
 db $29,$00,$00
 db $a1,$00,$93,$6c,$93,$70
 db $29,$00,$00
 db $91,$43,$22,$e2,$e2,$90,$a4
 db $29,$00,$01
 db $a1,$4d,$00,$93,$3b,$93,$3f
 db $29,$00,$00
 db $91,$43,$22,$e3,$6e,$90,$a4
 db $29,$00,$01
 db $a1,$4d,$00,$92,$e0,$92,$e4
 db $29,$00,$00
 db $91,$43,$22,$e3,$3d,$90,$a4
 db $29,$00,$01
 db $a1,$4d,$00,$93,$3b,$93,$3f
 db $29,$00,$00
 db $91,$43,$22,$e2,$e2,$90,$a4
 db $29,$00,$01
 db $91,$4d,$22,$93,$6c,$93,$70
 db $29,$00,$00
 db $a1,$45,$00,$e0,$90,$f2,$4b
 db $29,$01,$e2,$e4
 db $a1,$32,$b0,$90,$d3,$70
 db $a9,$03,$e2,$e4,$c2,$4b
 db $91,$45,$22,$e0,$90,$b2,$e4
 db $a9,$03,$c2,$4b,$a3,$70
 db $a1,$45,$32,$b0,$90,$90,$92
 db $a1,$43,$32,$a3,$70,$01
 db $a9,$05,$e0,$90,$90,$92
 db $29,$01,$01
 db $a1,$c1,$00,$b1,$6e,$92,$4b
 db $a1,$00,$00,$00
 db $91,$22,$e1,$ea,$93,$3f
 db $29,$00,$00
 db $a1,$32,$b2,$26,$93,$70
 db $29,$00,$00
 db $a1,$00,$e2,$47,$93,$dc
 db $29,$00,$00
 db $a1,$43,$32,$b1,$b6,$90,$92
 db $29,$00,$01
 db $91,$c1,$22,$e2,$26,$93,$70
 db $29,$00,$00
 db $a1,$32,$b1,$ea,$93,$3f
 db $a1,$32,$00,$00
 db $a9,$03,$b1,$b6,$90,$92
 db $29,$00,$01
 db $a1,$00,$b1,$9d,$90,$92
 db $a1,$00,$00,$01
 db $91,$c1,$22,$e1,$6e,$92,$4b
 db $29,$00,$00
 db $a1,$32,$b1,$9b,$92,$93
 db $29,$00,$00
 db $a1,$45,$00,$e0,$88,$f2,$2a
 db $29,$01,$e2,$93
 db $a1,$32,$b0,$88,$d3,$3f
 db $a9,$03,$e2,$93,$c2,$2a
 db $91,$45,$22,$e0,$88,$b2,$93
 db $a9,$03,$c2,$2a,$a3,$3f
 db $a1,$45,$32,$b0,$88,$90,$8a
 db $a1,$43,$32,$a3,$3f,$01
 db $a9,$05,$e0,$88,$90,$8a
 db $29,$01,$01
 db $a1,$c1,$00,$b1,$ea,$83,$3f
 db $a1,$00,$00,$00
 db $91,$22,$e2,$26,$83,$70
 db $29,$00,$00
 db $a1,$32,$b2,$47,$83,$dc
 db $29,$00,$00
 db $a1,$00,$e2,$8f,$84,$55
 db $29,$00,$00
 db $a1,$43,$32,$b1,$ec,$90,$8a
 db $29,$00,$01
 db $91,$c1,$22,$e2,$47,$83,$dc
 db $29,$00,$00
 db $a1,$43,$32,$b0,$88,$90,$8a
 db $a1,$32,$00,$01
 db $a9,$81,$e2,$26,$83,$70
 db $29,$00,$00
 db $a1,$43,$00,$b1,$ec,$90,$8a
 db $a1,$00,$00,$01
 db $91,$c1,$22,$e1,$45,$82,$2a
 db $29,$00,$00
 db $a1,$32,$b1,$ea,$b2,$93
 db $29,$00,$00
 db $a1,$00,$00,$00
 db $29,$00,$00
 db $a1,$32,$00,$00
 db $29,$00,$00
 db $91,$22,$00,$00
 db $29,$00,$00
 db $a1,$32,$e2,$26,$b2,$e4
 db $a1,$32,$00,$00
 db $a9,$03,$e1,$70,$90,$7b
 db $29,$00,$01
 db $a1,$00,$b0,$79,$90,$7b
 db $a1,$00,$01,$01
 db $91,$4d,$22,$e1,$6c,$92,$e4
 db $29,$00,$00
 db $a1,$32,$b1,$b2,$a3,$70
 db $29,$00,$00
 db $a1,$43,$00,$e1,$70,$90,$8a
 db $29,$00,$01
 db $a1,$4d,$32,$b1,$99,$b3,$3f
 db $29,$00,$00
 db $91,$43,$22,$e1,$b6,$90,$8a
 db $29,$00,$01
 db $a1,$4d,$32,$b1,$6c,$c2,$e4
 db $a1,$32,$00,$00
 db $a9,$03,$e1,$9d,$90,$8a
 db $29,$00,$01
 db $a1,$4d,$00,$b1,$0f,$d2,$2a
 db $a1,$00,$00,$00
 db $91,$43,$22,$e1,$70,$90,$8a
 db $29,$00,$01
 db $a1,$4d,$32,$b1,$1f,$e2,$4b
 db $29,$00,$00
 db $a1,$45,$00,$e0,$90,$f2,$4b
 db $29,$01,$e2,$e4
 db $a1,$32,$b0,$90,$d3,$70
 db $a9,$03,$e2,$e4,$c2,$4b
 db $91,$45,$22,$e0,$90,$b2,$e4
 db $a9,$03,$c2,$4b,$a3,$70
 db $a1,$45,$32,$b0,$90,$90,$92
 db $a1,$43,$32,$a3,$70,$01
 db $a9,$05,$e0,$90,$90,$92
 db $29,$01,$01
 db $a1,$00,$b0,$90,$90,$92
 db $a1,$00,$01,$01
 db $91,$c1,$22,$e2,$4b,$92,$e4
 db $29,$00,$00
 db $a1,$32,$b2,$e4,$93,$70
 db $29,$b2,$e2,$00
 db $a1,$32,$b2,$e0,$00
 db $29,$b2,$de,$00
 db $a1,$32,$b2,$e0,$00
 db $29,$b2,$e2,$00
 db $a1,$32,$b2,$e0,$00
 db $29,$b2,$de,$00
 db $a1,$32,$b2,$e0,$00
 db $29,$b2,$e2,$00
 db $a1,$32,$b2,$93,$93,$3f
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $91,$22,$00,$00
 db $91,$22,$00,$00
 db $a1,$cb,$00,$81,$72,$91,$70
 db $29,$00,$01
 db $a1,$10,$00,$91,$6e
 db $a1,$10,$00,$01
 db $a1,$00,$00,$91,$6c
 db $29,$00,$01
 db $a1,$0e,$00,$91,$6a
 db $a1,$0e,$00,$01
 db $a1,$00,$00,$91,$68
 db $29,$00,$01
 db $a1,$0c,$00,$91,$66
 db $a1,$0c,$00,$01
 db $a1,$c1,$00,$e2,$e4,$91,$6e
 db $29,$00,$00
 db $a1,$0a,$b2,$93,$91,$ee
 db $a1,$0a,$00,$00
 db $a1,$cb,$00,$81,$72,$91,$70
 db $29,$00,$01
 db $a1,$08,$00,$91,$6e
 db $a1,$08,$00,$01
 db $a1,$00,$00,$91,$6c
 db $29,$00,$01
 db $a1,$34,$00,$91,$6a
 db $a1,$34,$00,$01
 db $a1,$00,$00,$91,$68
 db $29,$00,$01
 db $a1,$04,$00,$91,$66
 db $a1,$04,$00,$01
 db $a1,$c1,$00,$e2,$e4,$91,$6e
 db $29,$00,$00
 db $a1,$02,$b2,$93,$91,$ee
 db $a1,$02,$00,$00
 db $a1,$00,$80,$b9,$90,$b7
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $a1,$00,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $a1,$00,$00,$90,$f5
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $a1,$00,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $a1,$00,$80,$5c,$90,$b7
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
.loop
 db $29,$01,$01
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $29,$00,$00
 db $00
 dw .loop
 align 2
.drumpar
.dp0
 dw .dsmp0+0
 db $02,$09,$40
.dp1
 dw .dsmp2+0
 db $02,$03,$00
.dp2
 dw .dsmp2+0
 db $02,$03,$08
.dp3
 dw .dsmp2+0
 db $02,$03,$10
.dp4
 dw .dsmp2+0
 db $02,$06,$18
.dp5
 dw .dsmp2+0
 db $02,$06,$20
.dp6
 dw .dsmp2+0
 db $02,$09,$28
.dp7
 dw .dsmp2+0
 db $02,$09,$30
.dp8
 dw .dsmp2+0
 db $02,$09,$38
.dp9
 dw .dsmp2+0
 db $02,$09,$00
.dp10
 dw .dsmp1+0
 db $04,$09,$40
.dp11
 dw .dsmp3+0
 db $02,$09,$08
.dp12
 dw .dsmp2+0
 db $02,$09,$08
.dp13
 dw .dsmp2+0
 db $02,$09,$10
.dp14
 dw .dsmp2+0
 db $02,$09,$18
.dp15
 dw .dsmp3+0
 db $02,$09,$18
.dp16
 dw .dsmp2+0
 db $02,$09,$20
.dp17
 dw .dsmp7+0
 db $06,$09,$40
.dp18
 dw .dsmp7+48
 db $03,$09,$40
.dp19
 dw .dsmp3+0
 db $02,$09,$30
.dp20
 dw .dsmp3+0
 db $02,$09,$38
.dp21
 dw .dsmp2+0
 db $02,$09,$40
.dp22
 dw .dsmp3+0
 db $02,$09,$40
.dp23
 dw .dsmp3+0
 db $02,$09,$28
.dp24
 dw .dsmp5+0
 db $04,$09,$40
.dp25
 dw .dsmp6+0
 db $02,$09,$40
.dp26
 dw .dsmp2+0
 db $02,$06,$10
.dsmp0
 db $00,$00,$00,$00,$00,$00,$00,$00,$01,$07,$f3,$fc,$ff,$ff,$ff,$ff
 db $ff,$e7,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$f3,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
 db $ff,$ff,$ff,$ff,$f8,$c0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp1
 db $3d,$ff,$0e,$38,$00,$00,$00,$00,$01,$01,$0f,$ff,$ef,$ff,$ff,$ff
 db $ff,$ff,$ff,$fe,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$ff,$ff
 db $ff,$ff,$ff,$ff,$ff,$ff,$00,$00,$00,$00,$00,$00,$00,$00,$00,$19
 db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$fe,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$0f,$7f,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$fb,$70,$80,$00
 db $00,$00,$00,$00,$00,$00,$00,$07,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
 db $df,$18,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$09,$ff,$ff,$ff
 db $ff,$ff,$ff,$ff,$ff,$cc,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp2
 db $7c,$1f,$07,$c0,$fc,$3f,$07,$f0,$7f,$07,$f0,$7f,$83,$fe,$3f,$f0
 db $ff,$83,$ff,$1f,$fc,$7f,$f8,$ff,$f0,$ff,$e1,$ff,$e1,$ff,$f1,$ff
 db $f1,$ff,$f8,$ff,$fe,$7f,$ff,$1f,$ff,$c7,$ff,$f1,$ff,$ff,$7f,$ff
 db $cf,$ff,$fc,$ff,$ff,$8f,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.dsmp3
 db $f0,$c3,$87,$8f,$0f,$1f,$9f,$cf,$e7,$f9,$fc,$ff,$3f,$e7,$fc,$ff
 db $9f,$f9,$ff,$bf,$fd,$ff,$df,$fe,$ff,$f3,$ff,$df,$ff,$7f,$fe,$ff
 db $fb,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
 db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp5
 db $00,$00,$00,$00,$00,$8c,$ff,$ff,$ff,$fe,$07,$00,$00,$00,$00,$00
 db $00,$00,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$fe,$00,$00,$00,$00,$00
 db $00,$00,$00,$1f,$ff,$ff,$ff,$ff,$ff,$ff,$00,$00,$00,$00,$00,$00
 db $00,$00,$0f,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$80,$00,$00,$00,$00
 db $00,$00,$00,$00,$79,$ff,$ff,$ff,$ff,$fc,$fc,$ff,$84,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$ff,$f1,$00,$00,$00
 db $00,$00,$00,$00,$00,$32,$f7,$ff,$ff,$ff,$ff,$ff,$fe,$f1,$00,$00
 db $00,$00,$00,$00,$00,$fc,$ff,$ff,$00,$f8,$c3,$00,$00,$00,$00,$00
.dsmp6
 db $50,$90,$0c,$6a,$04,$34,$21,$2c,$21,$90,$50,$40,$50,$48,$10,$0a
 db $80,$21,$40,$00,$00,$00,$00,$10,$00,$61,$10,$92,$a4,$00,$a4,$02
 db $04,$04,$24,$00,$02,$00,$40,$00,$40,$01,$01,$00,$48,$00,$21,$48
 db $21,$00,$00,$00,$00,$00,$40,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp7
 db $03,$e7,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$c0,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$e0,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$07,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
 db $ff,$fa,$00,$00,$00,$00,$00,$00,$00,$00,$00,$c4,$e7,$77,$ff,$ff
 db $ff,$ff,$ff,$ff,$ff,$ff,$ef,$a0,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$03,$7f,$ff,$ff,$ff,$ff,$ff,$ff,$fa,$e0,$40,$20,$00,$00,$00
 db $00,$00,$00,$00,$00,$01,$fa,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
 db $f8,$c0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$08,$c2,$ff,$ff,$ff
 db $ff,$ff,$ff,$ff,$ff,$ff,$00,$40,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$08,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$de,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$1f,$ff,$ff,$ff,$ff,$ff,$ff,$ff
 db $ff,$ff,$ff,$e6,$20,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3f



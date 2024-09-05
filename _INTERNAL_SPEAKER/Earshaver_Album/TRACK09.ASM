	output	"TRACK09.BIN"

	org $6000
begin:


; 	EARSHAVER
; 	TRACK 9
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




; TRACK09

;compiled music data

music_data
 dw .drumpar
.song
 db $98,$0d,$87,$b9,$01
 db $18,$87,$4a,$00
 db $18,$86,$e1,$00
 db $18,$86,$7e,$00
 db $18,$86,$21,$00
 db $18,$85,$c9,$00
 db $18,$85,$76,$00
 db $18,$85,$27,$00
 db $18,$84,$dd,$00
 db $18,$84,$97,$00
 db $18,$84,$55,$00
 db $18,$84,$17,$00
 db $18,$83,$dc,$00
 db $18,$00,$00
 db $18,$01,$00
 db $18,$00,$00
 db $18,$83,$dc,$00
 db $18,$00,$00
 db $18,$01,$00
 db $18,$00,$00
 db $18,$87,$b9,$00
 db $18,$00,$00
 db $18,$83,$dc,$00
 db $18,$00,$00
 db $18,$01,$00
 db $18,$00,$00
 db $18,$83,$dc,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$01,$00
 db $18,$00,$00
 db $18,$83,$dc,$00
 db $18,$00,$00
 db $18,$01,$00
 db $18,$00,$00
 db $18,$87,$b9,$00
 db $18,$00,$00
 db $18,$83,$dc,$00
 db $18,$00,$00
 db $18,$01,$00
 db $18,$00,$00
 db $18,$83,$dc,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$01,$00
 db $18,$00,$00
 db $18,$83,$dc,$00
 db $18,$00,$00
 db $18,$01,$00
 db $18,$00,$00
 db $18,$87,$b9,$00
 db $18,$00,$00
 db $18,$83,$dc,$00
 db $18,$00,$00
 db $18,$01,$00
 db $18,$00,$00
 db $18,$83,$dc,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$01,$00
 db $18,$00,$00
 db $18,$83,$dc,$00
 db $18,$00,$00
 db $18,$01,$00
 db $18,$00,$00
 db $18,$87,$b9,$00
 db $18,$00,$00
 db $18,$83,$dc,$00
 db $18,$00,$00
 db $18,$86,$e1,$00
 db $18,$00,$00
 db $18,$87,$b9,$00
 db $18,$00,$00
 db $94,$00,$83,$dc,$83,$da
 db $18,$00,$00
 db $18,$01,$00
 db $18,$00,$01
 db $18,$83,$dc,$83,$da
 db $18,$00,$00
 db $18,$01,$00
 db $18,$00,$01
 db $94,$02,$87,$b9,$87,$b7
 db $18,$00,$00
 db $18,$83,$dc,$83,$da
 db $18,$00,$00
 db $18,$01,$87,$b7
 db $18,$00,$01
 db $18,$83,$dc,$83,$da
 db $18,$00,$00
 db $94,$00,$00,$00
 db $18,$00,$00
 db $18,$01,$00
 db $18,$00,$01
 db $18,$83,$dc,$83,$da
 db $18,$00,$00
 db $18,$01,$00
 db $18,$00,$01
 db $94,$02,$87,$b9,$87,$b7
 db $18,$00,$00
 db $18,$83,$dc,$83,$da
 db $18,$00,$00
 db $18,$01,$01
 db $18,$00,$00
 db $18,$83,$dc,$87,$b7
 db $18,$00,$00
 db $94,$00,$00,$83,$da
 db $18,$00,$00
 db $18,$01,$00
 db $18,$00,$01
 db $18,$83,$dc,$83,$da
 db $18,$00,$00
 db $18,$01,$00
 db $18,$00,$01
 db $94,$02,$87,$b9,$87,$b7
 db $18,$00,$00
 db $18,$83,$dc,$83,$da
 db $18,$00,$00
 db $18,$01,$87,$b7
 db $18,$00,$01
 db $18,$83,$dc,$83,$da
 db $18,$00,$00
 db $94,$00,$00,$00
 db $18,$00,$00
 db $18,$01,$00
 db $18,$00,$01
 db $18,$83,$dc,$83,$da
 db $18,$00,$00
 db $18,$01,$00
 db $18,$00,$01
 db $94,$02,$87,$b9,$87,$b7
 db $18,$00,$00
 db $18,$83,$dc,$83,$da
 db $18,$00,$00
 db $94,$00,$86,$e1,$86,$df
 db $18,$00,$00
 db $94,$00,$87,$b9,$87,$b7
 db $18,$00,$00
 db $90,$cb,$04,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $98,$03,$e0,$f7,$80,$7b
 db $18,$00,$00
 db $98,$8b,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $90,$43,$04,$e3,$dc,$80,$7b
 db $18,$01,$00
 db $90,$cb,$06,$e3,$dc,$83,$d8
 db $18,$00,$00
 db $90,$04,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $98,$03,$e3,$dc,$80,$7b
 db $18,$01,$00
 db $98,$8b,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $18,$00,$b1,$ea
 db $18,$00,$00
 db $98,$03,$e3,$dc,$90,$7b
 db $18,$01,$00
 db $90,$cb,$04,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $90,$43,$04,$e3,$dc,$90,$7b
 db $18,$01,$00
 db $90,$cb,$06,$e3,$dc,$83,$d8
 db $18,$00,$00
 db $90,$04,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $98,$03,$e3,$dc,$90,$7b
 db $18,$01,$00
 db $98,$8b,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $90,$04,$00,$00
 db $18,$00,$00
 db $98,$03,$e0,$f7,$a0,$7b
 db $18,$00,$00
 db $98,$8b,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $90,$43,$04,$e3,$dc,$a0,$7b
 db $18,$01,$00
 db $90,$cb,$06,$e3,$dc,$83,$d8
 db $18,$00,$00
 db $90,$04,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $98,$03,$e3,$dc,$a0,$7b
 db $18,$01,$00
 db $98,$8b,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $18,$00,$b1,$ea
 db $18,$00,$00
 db $98,$03,$e3,$dc,$b0,$7b
 db $18,$01,$00
 db $90,$cb,$04,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $90,$43,$04,$e3,$dc,$b0,$7b
 db $18,$01,$00
 db $90,$cb,$06,$e3,$dc,$83,$d4
 db $18,$00,$00
 db $90,$04,$e1,$ee,$81,$e6
 db $18,$00,$00
 db $18,$e3,$70,$83,$68
 db $18,$00,$00
 db $18,$e3,$dc,$83,$d4
 db $18,$00,$00
 db $90,$04,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $98,$03,$e0,$f7,$c0,$7b
 db $18,$00,$00
 db $98,$8b,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $90,$43,$04,$e3,$dc,$c0,$7b
 db $18,$01,$00
 db $90,$cb,$06,$e3,$dc,$83,$d8
 db $18,$00,$00
 db $90,$04,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $98,$03,$e3,$dc,$c0,$7b
 db $18,$01,$00
 db $98,$8b,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $18,$00,$b1,$ea
 db $18,$00,$00
 db $98,$03,$e3,$dc,$d0,$7b
 db $18,$01,$00
 db $90,$cb,$04,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $90,$43,$04,$e3,$dc,$d0,$7b
 db $18,$01,$00
 db $90,$cb,$06,$e3,$dc,$83,$d8
 db $18,$00,$00
 db $90,$04,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $98,$03,$e3,$dc,$d0,$7b
 db $18,$01,$00
 db $98,$8b,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $90,$04,$00,$00
 db $18,$00,$00
 db $98,$03,$e0,$f7,$e0,$7b
 db $18,$00,$00
 db $98,$8b,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $90,$43,$04,$e3,$dc,$e0,$7b
 db $18,$01,$00
 db $90,$cb,$06,$e3,$dc,$83,$d8
 db $18,$00,$00
 db $90,$04,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $98,$03,$e3,$dc,$e0,$7b
 db $18,$01,$00
 db $98,$8b,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $18,$00,$b1,$ea
 db $18,$00,$00
 db $98,$03,$e3,$dc,$f0,$7b
 db $18,$01,$00
 db $90,$cb,$04,$e1,$ee,$81,$ea
 db $18,$00,$00
 db $90,$43,$04,$e3,$dc,$f0,$7b
 db $18,$01,$00
 db $90,$cb,$06,$e3,$dc,$83,$d4
 db $18,$00,$00
 db $90,$04,$e1,$ee,$81,$e6
 db $18,$00,$00
 db $84,$08,$e3,$70,$83,$68
 db $18,$00,$00
 db $84,$08,$e3,$dc,$83,$d4
 db $18,$00,$00
 db $90,$0a,$e1,$88,$81,$84
 db $18,$00,$00
 db $98,$03,$e0,$c4,$f0,$62
 db $18,$00,$00
 db $98,$8b,$e1,$88,$81,$84
 db $18,$00,$00
 db $90,$43,$04,$e3,$10,$f0,$62
 db $18,$01,$00
 db $90,$cb,$06,$e3,$10,$83,$0c
 db $18,$00,$00
 db $90,$04,$e1,$88,$81,$84
 db $18,$00,$00
 db $90,$43,$0a,$e3,$10,$f0,$62
 db $18,$01,$00
 db $98,$8b,$e1,$88,$81,$84
 db $18,$00,$00
 db $84,$08,$00,$b1,$84
 db $18,$00,$00
 db $98,$03,$e3,$10,$e0,$62
 db $18,$01,$00
 db $90,$cb,$0a,$e1,$88,$81,$84
 db $18,$00,$00
 db $90,$43,$04,$e3,$10,$e0,$62
 db $18,$01,$00
 db $90,$cb,$06,$e3,$10,$83,$0c
 db $18,$00,$00
 db $90,$04,$e1,$88,$81,$84
 db $18,$00,$00
 db $90,$43,$0a,$e3,$10,$e0,$62
 db $18,$01,$00
 db $98,$8b,$e1,$88,$81,$84
 db $18,$00,$00
 db $90,$0a,$00,$00
 db $18,$00,$00
 db $98,$03,$e0,$c4,$d0,$62
 db $18,$00,$00
 db $98,$8b,$e1,$88,$81,$84
 db $18,$00,$00
 db $90,$43,$04,$e3,$10,$d0,$62
 db $18,$01,$00
 db $90,$cb,$06,$e3,$10,$83,$0c
 db $18,$00,$00
 db $90,$04,$e1,$88,$81,$84
 db $18,$00,$00
 db $90,$43,$0a,$e3,$10,$d0,$62
 db $18,$01,$00
 db $98,$8b,$e1,$88,$81,$84
 db $18,$00,$00
 db $84,$08,$00,$b1,$84
 db $18,$00,$00
 db $98,$03,$e3,$10,$c0,$62
 db $18,$01,$00
 db $90,$cb,$0a,$e1,$88,$81,$84
 db $18,$00,$00
 db $90,$43,$04,$e3,$10,$c0,$62
 db $18,$01,$00
 db $90,$cb,$06,$e3,$10,$83,$08
 db $18,$00,$00
 db $90,$04,$e1,$88,$81,$80
 db $18,$00,$00
 db $84,$08,$e2,$bb,$82,$b3
 db $18,$00,$00
 db $84,$08,$e3,$10,$83,$08
 db $18,$00,$00
 db $90,$0a,$e1,$b8,$81,$b4
 db $18,$00,$00
 db $98,$03,$e0,$dc,$b0,$6e
 db $18,$00,$00
 db $98,$8b,$e1,$b8,$81,$b4
 db $18,$00,$00
 db $90,$43,$04,$e3,$70,$b0,$6e
 db $18,$01,$00
 db $84,$cb,$08,$e3,$70,$83,$6c
 db $18,$00,$00
 db $90,$04,$e1,$b8,$81,$b4
 db $18,$00,$00
 db $98,$03,$e3,$70,$b0,$6e
 db $18,$01,$00
 db $98,$8b,$e1,$b8,$81,$b4
 db $18,$00,$00
 db $90,$0a,$00,$b1,$b4
 db $18,$00,$00
 db $98,$03,$e3,$70,$a0,$6e
 db $18,$01,$00
 db $90,$cb,$04,$e1,$b8,$81,$b4
 db $18,$00,$00
 db $90,$43,$04,$e3,$70,$a0,$6e
 db $18,$01,$00
 db $84,$cb,$08,$e3,$70,$83,$6c
 db $18,$00,$00
 db $90,$04,$e1,$b8,$81,$b4
 db $18,$00,$00
 db $98,$03,$e3,$70,$a0,$6e
 db $18,$01,$00
 db $98,$8b,$e1,$b8,$81,$b4
 db $18,$00,$00
 db $90,$0a,$00,$00
 db $18,$00,$00
 db $98,$03,$e0,$dc,$90,$6e
 db $18,$00,$00
 db $98,$8b,$e1,$b8,$81,$b4
 db $18,$00,$00
 db $90,$43,$04,$e3,$70,$90,$6e
 db $18,$01,$00
 db $84,$cb,$08,$e3,$70,$83,$6c
 db $18,$00,$00
 db $90,$04,$e1,$b8,$81,$b4
 db $18,$00,$00
 db $90,$0a,$83,$70,$a3,$6c
 db $18,$00,$00
 db $90,$0a,$81,$b8,$a1,$b4
 db $18,$00,$00
 db $84,$c1,$08,$00,$b0,$d8
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$81,$88,$00
 db $18,$81,$72,$00
 db $18,$81,$5d,$00
 db $18,$81,$49,$00
 db $84,$08,$81,$37,$b1,$37
 db $18,$81,$25,$b1,$25
 db $84,$08,$81,$15,$b1,$15
 db $18,$81,$05,$b1,$05
 db $84,$08,$80,$dc,$b0,$dc
 db $18,$00,$00
 db $84,$08,$00,$00
 db $18,$00,$00
 db $90,$d1,$0a,$e0,$f7,$91,$25
 db $18,$00,$91,$37
 db $18,$b0,$f7,$91,$49
 db $18,$00,$91,$5d
 db $18,$e0,$f7,$91,$70
 db $18,$00,$00
 db $94,$0c,$b0,$f7,$00
 db $18,$01,$00
 db $84,$08,$e0,$f7,$00
 db $18,$e1,$72,$00
 db $18,$b0,$f7,$00
 db $18,$b1,$72,$00
 db $94,$0e,$e0,$f7,$00
 db $18,$e1,$72,$00
 db $84,$08,$b0,$f7,$00
 db $18,$b1,$72,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$e1,$72,$00
 db $94,$0e,$b0,$f7,$00
 db $18,$b1,$72,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$01,$00
 db $90,$0a,$b0,$f7,$00
 db $18,$01,$00
 db $84,$08,$e0,$f7,$00
 db $18,$01,$00
 db $94,$0c,$b0,$f7,$00
 db $18,$01,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$01,$00
 db $18,$b0,$f7,$00
 db $18,$01,$00
 db $90,$0a,$e0,$f7,$a2,$91
 db $18,$01,$00
 db $18,$b0,$f7,$00
 db $18,$01,$00
 db $18,$e0,$f7,$00
 db $18,$e2,$93,$00
 db $94,$0c,$b0,$f7,$00
 db $18,$b2,$93,$00
 db $84,$08,$e0,$f7,$00
 db $18,$e2,$93,$00
 db $18,$b0,$f7,$00
 db $18,$b2,$93,$00
 db $94,$0e,$e0,$f7,$00
 db $18,$e2,$93,$00
 db $84,$08,$b0,$f7,$00
 db $18,$b2,$93,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$01,$00
 db $94,$0e,$b0,$f7,$00
 db $18,$01,$00
 db $90,$0a,$e0,$f7,$b2,$e2
 db $18,$01,$00
 db $90,$0a,$b0,$f7,$00
 db $18,$01,$00
 db $84,$08,$e0,$f7,$00
 db $18,$e2,$e4,$00
 db $94,$0c,$b0,$f7,$00
 db $18,$b2,$e4,$00
 db $90,$0a,$e0,$f7,$b2,$28
 db $18,$e2,$e4,$00
 db $18,$b0,$f7,$00
 db $18,$b2,$e4,$00
 db $90,$0a,$e0,$dc,$00
 db $18,$e2,$2a,$00
 db $18,$b0,$dc,$00
 db $18,$b2,$2a,$00
 db $18,$e0,$dc,$00
 db $18,$e2,$2a,$00
 db $94,$0c,$b0,$dc,$00
 db $18,$b2,$2a,$00
 db $84,$08,$e0,$dc,$00
 db $18,$01,$00
 db $18,$b0,$dc,$00
 db $18,$01,$00
 db $94,$0e,$e1,$b8,$92,$49
 db $18,$00,$92,$6c
 db $84,$08,$b1,$b8,$92,$91
 db $18,$00,$00
 db $90,$0a,$e1,$b8,$00
 db $18,$01,$00
 db $94,$0e,$b1,$b8,$00
 db $18,$b2,$93,$00
 db $90,$0a,$e1,$b8,$00
 db $18,$e2,$93,$00
 db $90,$0a,$b1,$b8,$00
 db $18,$b2,$93,$00
 db $84,$08,$e1,$b8,$92,$b9
 db $18,$00,$92,$e2
 db $94,$0c,$b1,$b8,$00
 db $18,$00,$00
 db $90,$0a,$e1,$b8,$00
 db $18,$e2,$e4,$00
 db $18,$b1,$b8,$00
 db $18,$b2,$e4,$00
 db $90,$0a,$e1,$b8,$92,$91
 db $18,$00,$92,$49
 db $18,$b1,$b8,$92,$28
 db $18,$00,$00
 db $18,$e1,$b8,$00
 db $18,$01,$00
 db $94,$0c,$b1,$b8,$00
 db $18,$b2,$2a,$00
 db $84,$08,$e1,$b8,$00
 db $18,$e2,$2a,$00
 db $18,$b1,$b8,$00
 db $18,$b2,$2a,$00
 db $94,$0e,$e0,$da,$00
 db $18,$00,$00
 db $94,$0c,$b0,$da,$00
 db $18,$00,$00
 db $84,$cf,$08,$e1,$b8,$91,$ee
 db $18,$00,$00
 db $90,$0a,$b1,$b8,$00
 db $18,$00,$00
 db $90,$43,$0a,$e1,$ee,$b0,$dc
 db $18,$00,$00
 db $84,$cf,$08,$b1,$b8,$b1,$ee
 db $18,$00,$00
 db $90,$0a,$e1,$b8,$00
 db $18,$00,$00
 db $90,$43,$0a,$b1,$ee,$b0,$dc
 db $18,$00,$00
 db $84,$cf,$08,$e1,$b8,$b2,$4b
 db $18,$00,$00
 db $90,$43,$0a,$b1,$ee,$b0,$dc
 db $18,$00,$00
 db $90,$d1,$0a,$e0,$f7,$b1,$23
 db $18,$e2,$4b,$00
 db $18,$b0,$f7,$00
 db $18,$b2,$4b,$00
 db $18,$e0,$f7,$00
 db $18,$e1,$25,$00
 db $94,$0c,$b0,$f7,$00
 db $18,$b1,$25,$00
 db $84,$08,$e0,$f7,$00
 db $18,$e1,$25,$00
 db $18,$b0,$f7,$00
 db $18,$b1,$25,$00
 db $94,$0e,$e0,$f7,$b1,$70
 db $18,$00,$00
 db $84,$08,$b0,$f7,$00
 db $18,$00,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$e1,$72,$00
 db $94,$0e,$b0,$f7,$00
 db $18,$b1,$72,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$e1,$72,$00
 db $90,$0a,$b0,$f7,$00
 db $18,$b1,$72,$00
 db $84,$08,$e1,$ee,$b1,$b6
 db $18,$00,$00
 db $94,$0c,$b1,$ee,$00
 db $18,$00,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$e1,$b8,$00
 db $18,$b0,$f7,$00
 db $18,$b1,$b8,$00
 db $90,$0a,$e1,$ee,$92,$e2
 db $18,$00,$00
 db $18,$b1,$ee,$00
 db $18,$00,$00
 db $18,$e0,$f7,$00
 db $18,$e2,$e4,$00
 db $94,$0c,$b0,$f7,$00
 db $18,$b2,$e4,$00
 db $84,$08,$e0,$f7,$00
 db $18,$e2,$e4,$00
 db $18,$b0,$f7,$00
 db $18,$b2,$e4,$00
 db $94,$0e,$e0,$f7,$00
 db $18,$e2,$e4,$00
 db $84,$08,$b0,$f7,$00
 db $18,$b2,$e4,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$01,$00
 db $94,$0e,$b0,$f7,$00
 db $18,$01,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$01,$00
 db $90,$0a,$b0,$f7,$00
 db $18,$01,$00
 db $84,$08,$e0,$f7,$92,$49
 db $18,$01,$00
 db $94,$0c,$b0,$f7,$00
 db $18,$01,$00
 db $90,$0a,$e0,$f7,$92,$28
 db $18,$e2,$4b,$00
 db $18,$b0,$f7,$00
 db $18,$b2,$4b,$00
 db $90,$0a,$e0,$a4,$92,$49
 db $18,$e2,$2a,$00
 db $18,$b0,$a4,$00
 db $18,$b2,$2a,$00
 db $18,$e0,$a4,$01
 db $18,$e2,$4b,$00
 db $94,$0c,$b0,$a4,$00
 db $18,$b2,$4b,$00
 db $84,$08,$e0,$a4,$92,$28
 db $18,$01,$00
 db $18,$b0,$a4,$00
 db $18,$01,$00
 db $94,$0e,$e0,$a4,$91,$ec
 db $18,$e2,$2a,$00
 db $84,$08,$b0,$a4,$00
 db $18,$b2,$2a,$00
 db $90,$0a,$e0,$a4,$00
 db $18,$e1,$ee,$00
 db $94,$0e,$b0,$a4,$00
 db $18,$b1,$ee,$00
 db $90,$0a,$e0,$a4,$00
 db $18,$e1,$ee,$00
 db $90,$0a,$b0,$a4,$00
 db $18,$b1,$ee,$00
 db $84,$08,$e0,$a4,$00
 db $18,$01,$00
 db $94,$0c,$b0,$a4,$00
 db $18,$01,$00
 db $90,$0a,$e0,$a4,$00
 db $18,$01,$00
 db $18,$b0,$a4,$00
 db $18,$01,$00
 db $90,$0a,$e1,$72,$c2,$91
 db $18,$00,$00
 db $18,$b1,$72,$00
 db $18,$00,$00
 db $18,$e0,$b9,$01
 db $18,$e2,$93,$00
 db $94,$0c,$b0,$b9,$00
 db $18,$b2,$93,$00
 db $84,$08,$e1,$72,$c2,$49
 db $18,$00,$00
 db $18,$b1,$72,$00
 db $18,$00,$00
 db $94,$0c,$e1,$72,$c2,$28
 db $18,$e2,$4b,$00
 db $94,$0e,$b1,$72,$00
 db $18,$b2,$4b,$00
 db $84,$cf,$08,$e1,$72,$c1,$ee
 db $18,$00,$00
 db $90,$0a,$b1,$72,$00
 db $18,$00,$00
 db $90,$43,$0a,$e1,$ee,$c0,$b9
 db $18,$00,$00
 db $84,$cf,$08,$b1,$72,$c2,$2a
 db $18,$00,$00
 db $90,$0a,$e1,$72,$00
 db $18,$00,$00
 db $90,$43,$0a,$b1,$ee,$c0,$b9
 db $18,$00,$00
 db $84,$cf,$08,$e1,$72,$c2,$4b
 db $18,$00,$00
 db $90,$43,$0a,$b2,$2a,$c0,$b9
 db $18,$00,$00
 db $90,$d1,$0a,$e1,$88,$b1,$ee
 db $18,$e2,$4b,$00
 db $18,$b1,$88,$00
 db $18,$b2,$4b,$00
 db $18,$e1,$88,$00
 db $18,$e2,$4b,$00
 db $94,$10,$b0,$c4,$00
 db $18,$b3,$dc,$00
 db $84,$08,$e0,$c4,$00
 db $18,$e3,$dc,$00
 db $18,$b0,$c4,$00
 db $18,$b3,$dc,$00
 db $94,$12,$e0,$c4,$00
 db $18,$e3,$dc,$00
 db $84,$08,$b0,$c4,$00
 db $18,$b3,$dc,$00
 db $90,$0a,$e0,$c4,$00
 db $18,$e3,$dc,$00
 db $94,$12,$b0,$c4,$00
 db $18,$b3,$dc,$00
 db $90,$0a,$e0,$c4,$00
 db $18,$e3,$dc,$00
 db $90,$0a,$b0,$c4,$00
 db $18,$b3,$dc,$00
 db $84,$08,$e0,$c4,$00
 db $18,$01,$00
 db $94,$10,$b0,$c4,$00
 db $18,$01,$00
 db $90,$0a,$e0,$c4,$b2,$93
 db $18,$01,$00
 db $18,$b0,$c4,$b2,$e4
 db $18,$01,$00
 db $90,$0a,$e0,$c4,$b3,$70
 db $18,$e2,$93,$00
 db $18,$b0,$c4,$00
 db $18,$b2,$e4,$00
 db $18,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $94,$10,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $84,$08,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $18,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $94,$12,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $84,$08,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $90,$0a,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $94,$12,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $90,$0a,$e0,$c4,$00
 db $18,$01,$00
 db $90,$0a,$b0,$c4,$00
 db $18,$01,$00
 db $84,$08,$e0,$c4,$00
 db $18,$01,$00
 db $94,$10,$b0,$c4,$00
 db $18,$01,$00
 db $90,$0a,$e1,$ee,$b3,$10
 db $18,$01,$00
 db $18,$b1,$ee,$b2,$e4
 db $18,$01,$00
 db $90,$0a,$e1,$49,$b3,$10
 db $18,$e3,$10,$00
 db $18,$b1,$49,$00
 db $18,$b2,$e4,$00
 db $18,$e1,$49,$00
 db $18,$e3,$10,$00
 db $94,$10,$b0,$a4,$00
 db $18,$b3,$10,$00
 db $84,$08,$e1,$49,$b2,$e4
 db $18,$01,$00
 db $18,$b1,$49,$00
 db $18,$01,$00
 db $94,$12,$e1,$49,$b2,$4b
 db $18,$e2,$e4,$00
 db $84,$08,$b1,$49,$00
 db $18,$b2,$e4,$00
 db $90,$0a,$e1,$49,$00
 db $18,$e2,$4b,$00
 db $94,$12,$b1,$49,$00
 db $18,$b2,$4b,$00
 db $90,$0a,$e1,$49,$00
 db $18,$e2,$4b,$00
 db $90,$0a,$b1,$49,$00
 db $18,$b2,$4b,$00
 db $84,$08,$e1,$49,$00
 db $18,$e2,$4b,$00
 db $94,$10,$b1,$49,$00
 db $18,$b2,$4b,$00
 db $90,$0a,$e1,$49,$00
 db $18,$01,$00
 db $18,$b1,$49,$00
 db $18,$01,$00
 db $90,$0a,$e2,$93,$b3,$10
 db $18,$01,$00
 db $18,$b2,$93,$00
 db $18,$01,$00
 db $18,$e2,$93,$b2,$e4
 db $18,$e3,$10,$00
 db $94,$10,$b2,$93,$00
 db $18,$b3,$10,$00
 db $84,$08,$e1,$49,$01
 db $18,$e2,$e4,$00
 db $18,$b1,$49,$00
 db $18,$b2,$e4,$00
 db $94,$12,$e2,$91,$b2,$4b
 db $18,$01,$00
 db $94,$10,$b2,$91,$00
 db $18,$01,$00
 db $84,$cf,$08,$e1,$49,$b1,$ee
 db $18,$00,$00
 db $90,$0a,$b1,$49,$00
 db $18,$00,$00
 db $90,$43,$0a,$e1,$ee,$b0,$a4
 db $18,$00,$00
 db $84,$cf,$08,$b1,$49,$b2,$2a
 db $18,$00,$00
 db $90,$0a,$e1,$49,$00
 db $18,$00,$00
 db $90,$43,$0a,$b1,$ee,$b0,$a4
 db $18,$00,$00
 db $84,$cf,$08,$e1,$49,$b2,$4b
 db $18,$00,$00
 db $90,$0a,$00,$00
 db $18,$00,$00
 db $90,$d1,$0a,$e0,$c4,$b1,$ee
 db $18,$e2,$4b,$00
 db $18,$b0,$c4,$00
 db $18,$b2,$4b,$00
 db $18,$e0,$c4,$00
 db $18,$e1,$ee,$00
 db $94,$10,$b0,$c4,$00
 db $18,$b1,$ee,$00
 db $84,$08,$e0,$c4,$00
 db $18,$e1,$ee,$00
 db $18,$b0,$c4,$00
 db $18,$b1,$ee,$00
 db $94,$12,$e0,$c4,$00
 db $18,$01,$00
 db $84,$08,$b0,$c4,$00
 db $18,$01,$00
 db $90,$c1,$0a,$e3,$10,$00
 db $18,$00,$00
 db $94,$12,$b3,$10,$00
 db $18,$00,$00
 db $90,$43,$0a,$e1,$ee,$b0,$c4
 db $18,$01,$00
 db $90,$c1,$0a,$b3,$10,$b2,$2a
 db $18,$00,$00
 db $84,$08,$e3,$10,$00
 db $18,$00,$00
 db $94,$43,$10,$b1,$ee,$b0,$c4
 db $18,$01,$00
 db $90,$c1,$0a,$e3,$10,$b2,$4b
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $90,$d1,$0a,$e0,$c4,$93,$70
 db $18,$e2,$4b,$00
 db $18,$b0,$c4,$00
 db $18,$b2,$4b,$00
 db $18,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $94,$10,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $84,$08,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $18,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $94,$12,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $84,$08,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $90,$0a,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $94,$12,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $90,$0a,$e0,$c4,$00
 db $18,$01,$00
 db $90,$0a,$b0,$c4,$00
 db $18,$01,$00
 db $84,$08,$e0,$c4,$93,$10
 db $18,$01,$00
 db $94,$10,$b0,$c4,$00
 db $18,$01,$00
 db $90,$0a,$e0,$c4,$92,$e4
 db $18,$e3,$10,$00
 db $18,$b0,$c4,$00
 db $18,$b3,$10,$00
 db $90,$0a,$e0,$dc,$93,$10
 db $18,$e2,$e4,$00
 db $18,$b0,$dc,$00
 db $18,$b2,$e4,$00
 db $18,$e0,$dc,$01
 db $18,$e3,$10,$00
 db $94,$10,$b0,$dc,$00
 db $18,$b3,$10,$00
 db $84,$08,$e0,$dc,$92,$e4
 db $18,$01,$00
 db $18,$b0,$dc,$00
 db $18,$01,$00
 db $94,$12,$e0,$dc,$92,$93
 db $18,$e2,$e4,$00
 db $84,$08,$b0,$dc,$00
 db $18,$b2,$e4,$00
 db $90,$0a,$e0,$dc,$00
 db $18,$e2,$93,$00
 db $94,$12,$b0,$dc,$00
 db $18,$b2,$93,$00
 db $90,$0a,$e0,$dc,$00
 db $18,$e2,$93,$00
 db $90,$0a,$b0,$dc,$00
 db $18,$b2,$93,$00
 db $84,$08,$e0,$dc,$00
 db $18,$01,$00
 db $94,$10,$b0,$dc,$00
 db $18,$01,$00
 db $90,$0a,$e0,$dc,$00
 db $18,$00,$00
 db $18,$b0,$dc,$00
 db $18,$00,$00
 db $90,$0a,$e0,$dc,$00
 db $18,$00,$00
 db $18,$b0,$dc,$00
 db $18,$00,$00
 db $98,$8f,$e1,$b8,$92,$4b
 db $18,$00,$00
 db $94,$10,$b1,$b8,$00
 db $18,$00,$00
 db $84,$43,$08,$e2,$4b,$90,$dc
 db $18,$00,$00
 db $98,$8f,$b1,$b8,$92,$4b
 db $18,$00,$00
 db $94,$12,$e1,$b8,$00
 db $18,$00,$00
 db $94,$43,$10,$b2,$4b,$90,$dc
 db $18,$00,$00
 db $84,$cf,$08,$e1,$b8,$91,$ee
 db $18,$00,$00
 db $90,$0a,$b1,$b8,$00
 db $18,$00,$00
 db $90,$43,$0a,$e2,$4b,$90,$dc
 db $18,$00,$00
 db $84,$cf,$08,$b1,$b8,$91,$ee
 db $18,$00,$00
 db $90,$0a,$e1,$b8,$00
 db $18,$00,$00
 db $90,$43,$0a,$b1,$ee,$90,$dc
 db $18,$00,$00
 db $84,$cf,$08,$e1,$b8,$92,$4b
 db $18,$00,$00
 db $90,$43,$0a,$b1,$ee,$00
 db $18,$00,$00
 db $90,$47,$0a,$b0,$24,$e0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$24,$f0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$26,$e0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$26,$f0,$f7
 db $18,$01,$01
 db $94,$02,$b0,$29,$e0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$29,$f0,$f7
 db $18,$01,$01
 db $18,$b0,$2b,$e0,$f7
 db $18,$01,$01
 db $94,$02,$b0,$2b,$f0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$2e,$d0,$f7
 db $18,$01,$01
 db $94,$02,$b0,$2e,$e0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$31,$d0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$31,$e0,$f7
 db $18,$01,$01
 db $94,$02,$b0,$33,$d0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$33,$e0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$37,$d0,$f7
 db $18,$01,$01
 db $18,$b0,$37,$e0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$3a,$c0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$3a,$d0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$3d,$c0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$3d,$d0,$f7
 db $18,$01,$01
 db $94,$02,$b0,$41,$c0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$41,$d0,$f7
 db $18,$01,$01
 db $18,$b0,$45,$c0,$f7
 db $18,$01,$01
 db $94,$02,$b0,$45,$d0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$49,$b0,$f7
 db $18,$01,$01
 db $94,$02,$b0,$49,$c0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$4d,$b0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$4d,$c0,$f7
 db $18,$01,$01
 db $94,$02,$b0,$52,$b0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$52,$c0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$57,$b0,$f7
 db $18,$01,$01
 db $18,$b0,$57,$c0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$5c,$a0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$5c,$b0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$62,$a0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$62,$b0,$f7
 db $18,$01,$01
 db $94,$02,$b0,$67,$a0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$67,$b0,$f7
 db $18,$01,$01
 db $18,$b0,$6e,$a0,$f7
 db $18,$01,$01
 db $94,$02,$b0,$6e,$b0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$74,$90,$f7
 db $18,$01,$01
 db $94,$02,$b0,$74,$a0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$7b,$90,$f7
 db $18,$01,$01
 db $94,$00,$b0,$7b,$a0,$f7
 db $18,$01,$01
 db $94,$02,$b0,$82,$90,$f7
 db $18,$01,$01
 db $94,$00,$b0,$82,$a0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$8a,$90,$f7
 db $18,$01,$01
 db $18,$b0,$8a,$a0,$f7
 db $18,$01,$01
 db $94,$00,$b0,$92,$80,$f7
 db $18,$01,$01
 db $94,$00,$b0,$92,$90,$f7
 db $18,$01,$01
 db $94,$00,$b0,$9b,$80,$f7
 db $18,$01,$01
 db $94,$00,$b0,$9b,$90,$f7
 db $18,$01,$01
 db $94,$02,$b0,$a4,$80,$f7
 db $18,$01,$01
 db $94,$00,$b0,$a4,$90,$f7
 db $18,$01,$01
 db $18,$b0,$ae,$80,$f7
 db $18,$01,$01
 db $94,$02,$b0,$ae,$90,$f7
 db $18,$01,$01
 db $94,$00,$b0,$b9,$80,$f5
 db $18,$01,$01
 db $94,$02,$b0,$b9,$90,$f3
 db $18,$01,$01
 db $94,$00,$b0,$c4,$a0,$f1
 db $18,$01,$01
 db $94,$00,$b0,$c4,$b0,$ef
 db $18,$01,$01
 db $94,$02,$b0,$cf,$c0,$ed
 db $18,$01,$01
 db $94,$00,$b0,$cf,$d0,$eb
 db $18,$01,$01
 db $94,$00,$b0,$dc,$e0,$e9
 db $18,$01,$01
 db $18,$b0,$dc,$f0,$e7
 db $18,$01,$01
 db $90,$43,$04,$f1,$ee,$f0,$f7
 db $18,$f2,$4b,$01
 db $90,$04,$f2,$e4,$f0,$f7
 db $18,$f1,$ee,$01
 db $90,$04,$e2,$4b,$e0,$f7
 db $18,$e2,$e4,$01
 db $90,$04,$e3,$dc,$e0,$f7
 db $18,$e4,$97,$01
 db $90,$06,$d5,$c9,$d0,$f7
 db $18,$d3,$dc,$01
 db $90,$04,$d4,$97,$d0,$f7
 db $18,$d5,$c9,$01
 db $18,$c7,$b9,$c0,$f7
 db $18,$c9,$2f,$01
 db $90,$06,$cb,$92,$c0,$f7
 db $18,$c7,$b9,$01
 db $90,$04,$b1,$ee,$b0,$f7
 db $18,$b2,$4b,$01
 db $90,$06,$b2,$e4,$b0,$f7
 db $18,$b1,$ee,$01
 db $90,$04,$a2,$4b,$a0,$f7
 db $18,$a2,$e4,$01
 db $90,$04,$a3,$dc,$a0,$f7
 db $18,$a4,$97,$01
 db $90,$06,$95,$c9,$90,$f7
 db $18,$93,$dc,$01
 db $90,$04,$94,$97,$90,$f7
 db $18,$95,$c9,$01
 db $90,$04,$87,$b9,$80,$f7
 db $18,$89,$2f,$01
 db $18,$8b,$92,$80,$f7
 db $18,$87,$b9,$01
 db $90,$45,$04,$f0,$f7,$f0,$f7
 db $98,$03,$f1,$25,$01
 db $90,$04,$f1,$72,$f0,$f7
 db $18,$f1,$ee,$01
 db $90,$45,$04,$e2,$4b,$e0,$f7
 db $98,$03,$e2,$e4,$01
 db $90,$04,$e3,$dc,$e0,$f7
 db $18,$e4,$97,$01
 db $90,$45,$06,$d5,$c9,$d0,$f7
 db $98,$03,$d7,$b9,$01
 db $90,$04,$d9,$2f,$d0,$f7
 db $18,$db,$92,$01
 db $98,$05,$c0,$f7,$c0,$f7
 db $98,$03,$c1,$25,$01
 db $90,$06,$c1,$72,$c0,$f7
 db $18,$c1,$ee,$01
 db $90,$45,$04,$b2,$4b,$b0,$f7
 db $98,$03,$b2,$e4,$01
 db $90,$06,$b3,$dc,$b0,$f7
 db $18,$b4,$97,$01
 db $90,$45,$04,$a5,$c9,$a0,$f7
 db $98,$03,$a7,$b9,$01
 db $90,$04,$a9,$2f,$a0,$f7
 db $18,$ab,$92,$01
 db $90,$45,$06,$90,$f7,$90,$f7
 db $98,$03,$91,$25,$01
 db $90,$04,$91,$72,$90,$f7
 db $18,$91,$ee,$01
 db $90,$45,$04,$82,$4b,$80,$f7
 db $98,$03,$82,$e4,$01
 db $18,$83,$dc,$80,$f7
 db $18,$84,$97,$01
 db $90,$45,$04,$f1,$ee,$f0,$f7
 db $98,$03,$f2,$4b,$01
 db $90,$45,$04,$f2,$e4,$f0,$f7
 db $98,$03,$f3,$dc,$01
 db $90,$45,$04,$e4,$97,$e0,$f7
 db $18,$e5,$c9,$01
 db $90,$04,$e1,$ee,$e0,$f7
 db $98,$03,$e2,$4b,$01
 db $90,$45,$06,$d2,$e4,$d0,$f7
 db $98,$03,$d3,$dc,$01
 db $90,$45,$04,$d4,$97,$d0,$f7
 db $98,$03,$d5,$c9,$01
 db $98,$05,$c1,$ee,$c0,$f7
 db $18,$c2,$4b,$01
 db $90,$06,$c2,$e4,$c0,$f7
 db $98,$03,$c3,$dc,$01
 db $90,$45,$04,$b4,$97,$b0,$f7
 db $18,$b5,$c9,$01
 db $90,$06,$b1,$ee,$b0,$f7
 db $18,$b2,$4b,$01
 db $90,$04,$a2,$e4,$a0,$f7
 db $18,$a3,$dc,$01
 db $90,$04,$a4,$97,$a0,$f7
 db $18,$a5,$c9,$01
 db $90,$06,$91,$ee,$90,$f7
 db $18,$92,$4b,$01
 db $90,$04,$92,$e4,$90,$f7
 db $18,$93,$dc,$01
 db $90,$04,$84,$97,$80,$f7
 db $18,$85,$c9,$01
 db $18,$81,$ee,$80,$f7
 db $18,$82,$4b,$01
 db $90,$04,$f0,$f7,$f0,$f7
 db $18,$01,$f1,$25
 db $90,$04,$f0,$f7,$f1,$72
 db $18,$01,$f1,$b8
 db $90,$04,$e0,$f7,$e1,$ee
 db $18,$01,$e2,$4b
 db $90,$04,$e0,$f7,$e2,$e4
 db $18,$01,$e3,$70
 db $90,$06,$d0,$f7,$d3,$dc
 db $98,$03,$f1,$25,$d4,$97
 db $90,$45,$04,$d0,$f7,$d5,$c9
 db $98,$03,$f1,$b8,$d6,$e1
 db $98,$05,$c0,$f7,$c7,$b9
 db $98,$03,$e2,$4b,$c9,$2f
 db $90,$45,$06,$c0,$f7,$cb,$92
 db $98,$03,$e3,$70,$cd,$c3
 db $90,$45,$04,$b0,$f7,$bf,$72
 db $98,$03,$d4,$97,$bd,$c3
 db $90,$45,$06,$b0,$f7,$bb,$92
 db $98,$03,$d6,$e1,$b9,$2f
 db $90,$45,$04,$a0,$f7,$a7,$b9
 db $98,$03,$c9,$2f,$a6,$e1
 db $90,$45,$04,$a0,$f7,$a5,$c9
 db $98,$03,$cd,$c3,$a4,$97
 db $84,$45,$08,$90,$f7,$93,$dc
 db $98,$03,$bd,$c3,$93,$70
 db $84,$45,$08,$90,$f7,$92,$e4
 db $98,$03,$b9,$2f,$92,$4b
 db $84,$45,$08,$80,$f7,$81,$ee
 db $98,$03,$a6,$e1,$81,$b8
 db $84,$45,$08,$80,$f7,$81,$72
 db $98,$03,$a4,$97,$81,$25
 db $90,$cb,$0a,$e1,$ee,$f0,$f7
 db $18,$00,$01
 db $90,$43,$0a,$e0,$f7,$f0,$f7
 db $18,$00,$01
 db $90,$cb,$0a,$e1,$ee,$e0,$f7
 db $18,$00,$01
 db $90,$43,$0a,$e3,$dc,$e0,$f7
 db $18,$01,$01
 db $84,$cb,$08,$e3,$dc,$d0,$f7
 db $18,$00,$01
 db $90,$0a,$e1,$ee,$d0,$f7
 db $18,$00,$01
 db $98,$03,$e3,$dc,$c0,$f7
 db $18,$01,$01
 db $84,$cb,$08,$e1,$ee,$c0,$f7
 db $18,$00,$01
 db $90,$0a,$00,$b0,$f7
 db $18,$00,$01
 db $84,$43,$08,$e3,$dc,$b0,$f7
 db $18,$01,$01
 db $90,$cb,$0a,$e1,$ee,$a0,$f7
 db $18,$00,$01
 db $90,$43,$0a,$e3,$dc,$a0,$f7
 db $18,$01,$01
 db $84,$cb,$08,$e3,$dc,$90,$f7
 db $18,$00,$01
 db $90,$0a,$e1,$ee,$90,$f7
 db $18,$00,$01
 db $90,$43,$0a,$e3,$dc,$80,$f7
 db $18,$01,$01
 db $98,$8b,$e1,$ee,$80,$f7
 db $18,$00,$01
 db $90,$0a,$00,$f0,$f7
 db $18,$00,$01
 db $90,$43,$0a,$e0,$f7,$f0,$f7
 db $18,$00,$01
 db $90,$cb,$0a,$e1,$ee,$e0,$f7
 db $18,$00,$01
 db $90,$43,$0a,$e3,$dc,$e0,$f7
 db $18,$01,$01
 db $84,$cb,$08,$e3,$dc,$d0,$f7
 db $18,$00,$01
 db $90,$0a,$e1,$ee,$d0,$f7
 db $18,$00,$01
 db $98,$03,$e3,$dc,$c0,$f7
 db $18,$01,$01
 db $84,$cb,$08,$e1,$ee,$c0,$f7
 db $18,$00,$01
 db $90,$0a,$00,$b0,$f7
 db $18,$00,$01
 db $84,$43,$08,$e3,$dc,$b0,$f7
 db $18,$01,$01
 db $90,$cb,$0a,$e1,$ee,$a0,$f7
 db $18,$00,$01
 db $90,$43,$0a,$e3,$dc,$a0,$f7
 db $18,$01,$01
 db $84,$cb,$08,$e3,$dc,$90,$f7
 db $18,$00,$01
 db $90,$0a,$e1,$ee,$90,$f7
 db $18,$00,$01
 db $90,$0a,$e3,$70,$80,$f7
 db $18,$00,$01
 db $18,$e3,$dc,$80,$f7
 db $18,$00,$01
 db $90,$0a,$e1,$ee,$f0,$f7
 db $18,$00,$01
 db $90,$43,$0a,$e0,$f7,$f0,$f7
 db $18,$00,$01
 db $90,$cb,$0a,$e1,$ee,$e0,$f7
 db $18,$00,$01
 db $90,$43,$0a,$e3,$dc,$e0,$f7
 db $18,$01,$01
 db $84,$cb,$08,$e3,$dc,$d0,$f7
 db $18,$00,$01
 db $90,$0a,$e1,$ee,$d0,$f7
 db $18,$00,$01
 db $98,$03,$e3,$dc,$c0,$f7
 db $18,$01,$01
 db $84,$cb,$08,$e1,$ee,$c0,$f7
 db $18,$00,$01
 db $90,$0a,$00,$b0,$f7
 db $18,$00,$01
 db $84,$43,$08,$e3,$dc,$b0,$f7
 db $18,$01,$01
 db $90,$cb,$0a,$e1,$ee,$a0,$f7
 db $18,$00,$01
 db $90,$43,$0a,$e3,$dc,$a0,$f7
 db $18,$01,$01
 db $84,$cb,$08,$e3,$dc,$90,$f7
 db $18,$00,$01
 db $90,$0a,$e1,$ee,$90,$f7
 db $18,$00,$01
 db $90,$43,$0a,$e3,$dc,$80,$f7
 db $18,$01,$01
 db $98,$8b,$e1,$ee,$80,$f7
 db $18,$00,$01
 db $90,$0a,$00,$f0,$f7
 db $18,$00,$01
 db $90,$43,$0a,$e0,$f7,$f0,$f7
 db $18,$00,$01
 db $90,$cb,$0a,$e1,$ee,$e0,$f7
 db $18,$00,$01
 db $90,$43,$0a,$e3,$dc,$e0,$f7
 db $18,$01,$01
 db $84,$cb,$08,$e3,$dc,$d0,$f7
 db $18,$00,$01
 db $90,$0a,$e1,$ee,$d0,$f7
 db $18,$00,$01
 db $98,$03,$e3,$dc,$c0,$f7
 db $18,$01,$01
 db $84,$cb,$08,$e1,$ee,$c0,$f7
 db $18,$00,$01
 db $90,$0a,$00,$b0,$f7
 db $18,$00,$01
 db $84,$43,$08,$e3,$dc,$b0,$f7
 db $18,$01,$01
 db $90,$cb,$0a,$e1,$ee,$a0,$f7
 db $18,$00,$01
 db $90,$43,$0a,$e3,$dc,$a0,$f7
 db $18,$01,$01
 db $84,$cb,$08,$e1,$ee,$a1,$ec
 db $18,$e1,$b8,$a1,$b6
 db $90,$0a,$e1,$88,$a1,$86
 db $18,$e1,$72,$a1,$70
 db $90,$0a,$e0,$f7,$a0,$f5
 db $18,$e0,$dc,$a0,$da
 db $18,$e0,$c4,$a0,$c2
 db $18,$e0,$b9,$a0,$b7
 db $90,$0a,$e1,$88,$81,$84
 db $18,$00,$00
 db $90,$c1,$0a,$e0,$c4,$f1,$25
 db $18,$00,$00
 db $90,$cb,$0a,$e1,$88,$81,$84
 db $18,$00,$00
 db $90,$c1,$0a,$e0,$c4,$f1,$25
 db $18,$00,$00
 db $84,$cb,$08,$e3,$10,$83,$0c
 db $18,$00,$00
 db $90,$0a,$e1,$88,$81,$84
 db $18,$00,$00
 db $98,$81,$e0,$c4,$f1,$25
 db $18,$00,$00
 db $84,$cb,$08,$e1,$88,$81,$84
 db $18,$00,$00
 db $90,$0a,$00,$b1,$84
 db $18,$00,$00
 db $84,$c1,$08,$e0,$c4,$e1,$25
 db $18,$00,$00
 db $90,$cb,$0a,$e1,$88,$81,$84
 db $18,$00,$00
 db $90,$c1,$0a,$e0,$c4,$e1,$25
 db $18,$00,$00
 db $84,$cb,$08,$e3,$10,$83,$0c
 db $18,$00,$00
 db $90,$0a,$e1,$88,$81,$84
 db $18,$00,$00
 db $90,$c1,$0a,$e0,$c4,$e1,$25
 db $18,$00,$00
 db $98,$8b,$e1,$88,$81,$84
 db $18,$00,$00
 db $90,$0a,$00,$00
 db $18,$00,$00
 db $90,$c1,$0a,$e0,$c4,$d1,$25
 db $18,$00,$00
 db $90,$cb,$0a,$e1,$88,$81,$84
 db $18,$00,$00
 db $90,$c1,$0a,$e0,$c4,$d1,$25
 db $18,$00,$00
 db $84,$cb,$08,$e3,$10,$83,$0c
 db $18,$00,$00
 db $90,$0a,$e1,$88,$81,$84
 db $18,$00,$00
 db $98,$81,$e0,$c4,$d1,$25
 db $18,$00,$00
 db $84,$cb,$08,$e1,$88,$81,$84
 db $18,$00,$00
 db $90,$0a,$00,$b1,$84
 db $18,$00,$00
 db $84,$c1,$08,$e0,$c4,$c1,$25
 db $18,$00,$00
 db $90,$cb,$0a,$e1,$88,$81,$84
 db $18,$00,$00
 db $90,$c1,$0a,$e0,$c4,$c1,$25
 db $18,$00,$00
 db $84,$cb,$08,$e3,$10,$83,$08
 db $18,$00,$00
 db $90,$0a,$e1,$88,$81,$80
 db $18,$00,$00
 db $90,$0a,$e2,$bb,$82,$b3
 db $18,$00,$00
 db $18,$e3,$10,$83,$08
 db $18,$00,$00
 db $90,$0a,$e1,$b8,$81,$b4
 db $18,$00,$00
 db $90,$c1,$0a,$e0,$dc,$b1,$49
 db $18,$00,$00
 db $90,$cb,$0a,$e1,$b8,$81,$b4
 db $18,$00,$00
 db $90,$c1,$0a,$e3,$70,$b1,$49
 db $18,$00,$00
 db $84,$cb,$08,$00,$83,$6c
 db $18,$00,$00
 db $90,$0a,$e1,$b8,$81,$b4
 db $18,$00,$00
 db $98,$81,$e3,$70,$b1,$49
 db $18,$00,$00
 db $84,$cb,$08,$e1,$b8,$81,$b4
 db $18,$00,$00
 db $90,$0a,$00,$b1,$b4
 db $18,$00,$00
 db $84,$c1,$08,$e3,$70,$a1,$49
 db $18,$00,$00
 db $90,$cb,$0a,$e1,$b8,$81,$b4
 db $18,$00,$00
 db $90,$c1,$0a,$e3,$70,$a1,$49
 db $18,$00,$00
 db $84,$cb,$08,$00,$83,$6c
 db $18,$00,$00
 db $90,$0a,$e1,$b8,$81,$b4
 db $18,$00,$00
 db $90,$c1,$0a,$e3,$70,$a1,$49
 db $18,$00,$00
 db $98,$8b,$e1,$b8,$81,$b4
 db $18,$00,$00
 db $90,$0a,$00,$00
 db $18,$00,$00
 db $90,$c1,$0a,$e0,$dc,$91,$49
 db $18,$00,$00
 db $90,$cb,$0a,$e1,$b8,$81,$b4
 db $18,$00,$00
 db $90,$c1,$0a,$e3,$70,$91,$49
 db $18,$00,$00
 db $84,$cb,$08,$00,$83,$6c
 db $18,$00,$00
 db $90,$0a,$e1,$b8,$81,$b4
 db $18,$00,$00
 db $90,$0a,$83,$70,$a3,$6c
 db $18,$00,$00
 db $90,$0a,$81,$b8,$a1,$b4
 db $18,$00,$00
 db $84,$cf,$08,$00,$b0,$6c
 db $18,$00,$00
 db $84,$08,$00,$00
 db $18,$00,$00
 db $84,$08,$81,$88,$00
 db $18,$81,$72,$00
 db $84,$08,$81,$5d,$00
 db $18,$81,$49,$00
 db $84,$08,$81,$37,$00
 db $18,$81,$25,$00
 db $84,$08,$81,$15,$00
 db $18,$81,$05,$00
 db $84,$08,$80,$dc,$00
 db $18,$00,$00
 db $84,$08,$00,$00
 db $18,$00,$00
 db $90,$d1,$0a,$e0,$f7,$91,$25
 db $18,$00,$91,$37
 db $18,$b0,$f7,$91,$49
 db $18,$00,$91,$5d
 db $18,$e0,$f7,$91,$70
 db $18,$00,$00
 db $90,$04,$b0,$f7,$00
 db $18,$01,$00
 db $84,$08,$e0,$f7,$00
 db $18,$e1,$72,$00
 db $18,$b0,$f7,$00
 db $18,$b1,$72,$00
 db $90,$06,$e0,$f7,$00
 db $18,$e1,$72,$00
 db $84,$08,$b0,$f7,$00
 db $18,$b1,$72,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$e1,$72,$00
 db $90,$06,$b0,$f7,$00
 db $18,$b1,$72,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$01,$00
 db $90,$0a,$b0,$f7,$00
 db $18,$01,$00
 db $84,$08,$e0,$f7,$00
 db $18,$01,$00
 db $90,$04,$b0,$f7,$00
 db $18,$01,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$01,$00
 db $18,$b0,$f7,$00
 db $18,$01,$00
 db $90,$0a,$e0,$f7,$a2,$91
 db $18,$01,$00
 db $18,$b0,$f7,$00
 db $18,$01,$00
 db $18,$e0,$f7,$00
 db $18,$e2,$93,$00
 db $90,$04,$b0,$f7,$00
 db $18,$b2,$93,$00
 db $84,$08,$e0,$f7,$00
 db $18,$e2,$93,$00
 db $18,$b0,$f7,$00
 db $18,$b2,$93,$00
 db $90,$06,$e0,$f7,$00
 db $18,$e2,$93,$00
 db $84,$08,$b0,$f7,$00
 db $18,$b2,$93,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$01,$00
 db $90,$06,$b0,$f7,$00
 db $18,$01,$00
 db $90,$0a,$e0,$f7,$b2,$e2
 db $18,$01,$00
 db $90,$0a,$b0,$f7,$00
 db $18,$01,$00
 db $84,$08,$e0,$f7,$00
 db $18,$e2,$e4,$00
 db $90,$04,$b0,$f7,$00
 db $18,$b2,$e4,$00
 db $90,$0a,$e0,$f7,$b2,$28
 db $18,$e2,$e4,$00
 db $18,$b0,$f7,$00
 db $18,$b2,$e4,$00
 db $90,$0a,$e0,$dc,$00
 db $18,$e2,$2a,$00
 db $18,$b0,$dc,$00
 db $18,$b2,$2a,$00
 db $18,$e0,$dc,$00
 db $18,$e2,$2a,$00
 db $90,$04,$b0,$dc,$00
 db $18,$b2,$2a,$00
 db $84,$08,$e0,$dc,$00
 db $18,$01,$00
 db $18,$b0,$dc,$00
 db $18,$01,$00
 db $90,$06,$e1,$b8,$92,$49
 db $18,$00,$92,$6c
 db $84,$08,$b1,$b8,$92,$91
 db $18,$00,$00
 db $90,$0a,$e1,$b8,$00
 db $18,$01,$00
 db $90,$06,$b1,$b8,$00
 db $18,$b2,$93,$00
 db $90,$0a,$e1,$b8,$00
 db $18,$e2,$93,$00
 db $90,$0a,$b1,$b8,$00
 db $18,$b2,$93,$00
 db $84,$08,$e1,$b8,$92,$b9
 db $18,$00,$92,$e2
 db $90,$04,$b1,$b8,$00
 db $18,$00,$00
 db $90,$0a,$e1,$b8,$00
 db $18,$e2,$e4,$00
 db $18,$b1,$b8,$00
 db $18,$b2,$e4,$00
 db $90,$0a,$e1,$b8,$92,$91
 db $18,$00,$92,$49
 db $18,$b1,$b8,$92,$28
 db $18,$00,$00
 db $18,$e1,$b8,$00
 db $18,$01,$00
 db $90,$04,$b1,$b8,$00
 db $18,$b2,$2a,$00
 db $84,$08,$e1,$b8,$00
 db $18,$e2,$2a,$00
 db $18,$b1,$b8,$00
 db $18,$b2,$2a,$00
 db $90,$06,$e0,$da,$00
 db $18,$00,$00
 db $90,$04,$b0,$da,$00
 db $18,$00,$00
 db $84,$cf,$08,$e1,$b8,$91,$ee
 db $18,$00,$00
 db $90,$0a,$b1,$b8,$00
 db $18,$00,$00
 db $90,$43,$0a,$e1,$ee,$b0,$dc
 db $18,$00,$00
 db $84,$cf,$08,$b1,$b8,$b1,$ee
 db $18,$00,$00
 db $90,$0a,$e1,$b8,$00
 db $18,$00,$00
 db $90,$43,$0a,$b1,$ee,$b0,$dc
 db $18,$00,$00
 db $84,$cf,$08,$e1,$b8,$b2,$4b
 db $18,$00,$00
 db $90,$43,$0a,$b1,$ee,$b0,$dc
 db $18,$00,$00
 db $90,$d1,$0a,$e0,$f7,$b1,$23
 db $18,$e2,$4b,$00
 db $18,$b0,$f7,$00
 db $18,$b2,$4b,$00
 db $18,$e0,$f7,$00
 db $18,$e1,$25,$00
 db $90,$04,$b0,$f7,$00
 db $18,$b1,$25,$00
 db $84,$08,$e0,$f7,$00
 db $18,$e1,$25,$00
 db $18,$b0,$f7,$00
 db $18,$b1,$25,$00
 db $90,$06,$e0,$f7,$b1,$70
 db $18,$00,$00
 db $84,$08,$b0,$f7,$00
 db $18,$00,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$e1,$72,$00
 db $90,$06,$b0,$f7,$00
 db $18,$b1,$72,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$e1,$72,$00
 db $90,$0a,$b0,$f7,$00
 db $18,$b1,$72,$00
 db $84,$08,$e1,$ee,$b1,$b6
 db $18,$00,$00
 db $90,$04,$b1,$ee,$00
 db $18,$00,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$e1,$b8,$00
 db $18,$b0,$f7,$00
 db $18,$b1,$b8,$00
 db $90,$0a,$e1,$ee,$92,$e2
 db $18,$00,$00
 db $18,$b1,$ee,$00
 db $18,$00,$00
 db $18,$e0,$f7,$00
 db $18,$e2,$e4,$00
 db $90,$04,$b0,$f7,$00
 db $18,$b2,$e4,$00
 db $84,$08,$e0,$f7,$00
 db $18,$e2,$e4,$00
 db $18,$b0,$f7,$00
 db $18,$b2,$e4,$00
 db $90,$06,$e0,$f7,$00
 db $18,$e2,$e4,$00
 db $84,$08,$b0,$f7,$00
 db $18,$b2,$e4,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$01,$00
 db $90,$06,$b0,$f7,$00
 db $18,$01,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$01,$00
 db $90,$0a,$b0,$f7,$00
 db $18,$01,$00
 db $84,$08,$e0,$f7,$92,$49
 db $18,$01,$00
 db $90,$04,$b0,$f7,$00
 db $18,$01,$00
 db $90,$0a,$e0,$f7,$92,$28
 db $18,$e2,$4b,$00
 db $18,$b0,$f7,$00
 db $18,$b2,$4b,$00
 db $90,$0a,$e0,$a4,$92,$49
 db $18,$e2,$2a,$00
 db $18,$b0,$a4,$00
 db $18,$b2,$2a,$00
 db $18,$e0,$a4,$91,$49
 db $18,$e2,$4b,$01
 db $90,$04,$b0,$a4,$91,$49
 db $18,$b2,$4b,$01
 db $84,$08,$e0,$a4,$92,$28
 db $18,$01,$00
 db $18,$b0,$a4,$00
 db $18,$01,$00
 db $90,$06,$e0,$a4,$91,$ec
 db $18,$e2,$2a,$00
 db $84,$08,$b0,$a4,$00
 db $18,$b2,$2a,$00
 db $90,$0a,$e0,$a4,$00
 db $18,$e1,$ee,$00
 db $90,$06,$b0,$a4,$00
 db $18,$b1,$ee,$00
 db $90,$0a,$e0,$a4,$00
 db $18,$e1,$ee,$00
 db $90,$0a,$b0,$a4,$00
 db $18,$b1,$ee,$00
 db $84,$08,$e0,$a4,$00
 db $18,$01,$00
 db $90,$04,$b0,$a4,$00
 db $18,$01,$00
 db $90,$0a,$e0,$a4,$00
 db $18,$01,$00
 db $18,$b0,$a4,$00
 db $18,$01,$00
 db $90,$0a,$e1,$72,$c2,$91
 db $18,$00,$00
 db $18,$b1,$72,$00
 db $18,$00,$00
 db $18,$e0,$b9,$c1,$72
 db $18,$e2,$93,$01
 db $90,$04,$b0,$b9,$c1,$72
 db $18,$b2,$93,$01
 db $84,$08,$e1,$72,$c2,$49
 db $18,$00,$00
 db $18,$b1,$72,$00
 db $18,$00,$00
 db $90,$06,$e1,$72,$c2,$28
 db $18,$e2,$4b,$00
 db $90,$04,$b1,$72,$00
 db $18,$b2,$4b,$00
 db $84,$cf,$08,$e1,$72,$c1,$ee
 db $18,$00,$00
 db $90,$0a,$b1,$72,$00
 db $18,$00,$00
 db $90,$43,$0a,$e1,$ee,$c0,$b9
 db $18,$00,$00
 db $84,$cf,$08,$b1,$72,$c2,$2a
 db $18,$00,$00
 db $90,$0a,$e1,$72,$00
 db $18,$00,$00
 db $90,$43,$0a,$b1,$ee,$c0,$b9
 db $18,$00,$00
 db $84,$cf,$08,$e1,$72,$c2,$4b
 db $18,$00,$00
 db $90,$43,$0a,$b2,$2a,$c0,$b9
 db $18,$00,$00
 db $90,$c1,$0a,$e0,$f7,$b1,$25
 db $18,$00,$b1,$37
 db $18,$b0,$f7,$b1,$49
 db $18,$00,$b1,$5d
 db $18,$e0,$f7,$b1,$72
 db $18,$00,$00
 db $90,$04,$b0,$f7,$00
 db $18,$b1,$72,$00
 db $84,$08,$e0,$f7,$00
 db $18,$e1,$72,$00
 db $18,$b0,$f7,$00
 db $18,$b1,$72,$00
 db $90,$06,$e0,$f7,$00
 db $18,$e1,$72,$00
 db $84,$08,$b0,$f7,$00
 db $18,$b1,$72,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$e1,$72,$00
 db $90,$06,$b0,$f7,$00
 db $18,$b1,$72,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$e1,$72,$00
 db $90,$0a,$b0,$f7,$00
 db $18,$b1,$72,$00
 db $84,$08,$e0,$f7,$00
 db $18,$e1,$72,$00
 db $90,$04,$b0,$f7,$00
 db $18,$b1,$72,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$e1,$72,$00
 db $18,$b0,$f7,$00
 db $18,$b1,$72,$00
 db $90,$0a,$e0,$f7,$b2,$93
 db $18,$e2,$93,$00
 db $18,$b0,$f7,$00
 db $18,$b2,$93,$00
 db $18,$e0,$f7,$00
 db $18,$e2,$93,$00
 db $90,$04,$b0,$f7,$00
 db $18,$b2,$93,$00
 db $84,$08,$e0,$f7,$00
 db $18,$e2,$93,$00
 db $18,$b0,$f7,$00
 db $18,$b2,$93,$00
 db $90,$06,$e0,$f7,$00
 db $18,$e2,$93,$00
 db $84,$08,$b0,$f7,$00
 db $18,$b2,$93,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$e2,$93,$00
 db $90,$06,$b0,$f7,$00
 db $18,$b2,$93,$00
 db $90,$0a,$e0,$f7,$b2,$e4
 db $18,$e2,$e4,$00
 db $90,$0a,$b0,$f7,$00
 db $18,$b2,$e4,$00
 db $84,$08,$e0,$f7,$00
 db $18,$e2,$e4,$00
 db $90,$04,$b0,$f7,$00
 db $18,$b2,$e4,$00
 db $90,$0a,$e0,$f7,$b2,$2a
 db $18,$e2,$2a,$00
 db $18,$b0,$f7,$00
 db $18,$b2,$2a,$00
 db $90,$0a,$e0,$dc,$00
 db $18,$e2,$2a,$00
 db $18,$b0,$dc,$00
 db $18,$b2,$2a,$00
 db $18,$e0,$dc,$00
 db $18,$e2,$2a,$00
 db $90,$04,$b0,$dc,$00
 db $18,$b2,$2a,$00
 db $84,$08,$e0,$dc,$00
 db $18,$e2,$2a,$00
 db $18,$b0,$dc,$00
 db $18,$b2,$2a,$00
 db $90,$06,$e1,$b8,$b2,$4b
 db $18,$00,$b2,$6e
 db $84,$08,$b1,$b8,$b2,$93
 db $18,$00,$00
 db $90,$0a,$e1,$b8,$00
 db $18,$e2,$93,$00
 db $90,$06,$b1,$b8,$00
 db $18,$b2,$93,$00
 db $90,$0a,$e1,$b8,$00
 db $18,$e2,$93,$00
 db $90,$0a,$b1,$b8,$00
 db $18,$b2,$93,$00
 db $84,$08,$e1,$b8,$b2,$bb
 db $18,$00,$b2,$e4
 db $90,$04,$b1,$b8,$00
 db $18,$00,$00
 db $90,$0a,$e1,$b8,$00
 db $18,$e2,$e4,$00
 db $18,$b1,$b8,$00
 db $18,$b2,$e4,$00
 db $90,$0a,$e1,$b8,$b2,$93
 db $18,$00,$b2,$4b
 db $18,$b1,$b8,$b2,$2a
 db $18,$00,$00
 db $18,$e1,$b8,$00
 db $18,$e2,$2a,$00
 db $90,$04,$b1,$b8,$00
 db $18,$b2,$2a,$00
 db $84,$08,$e1,$b8,$00
 db $18,$e2,$2a,$00
 db $18,$b1,$b8,$00
 db $18,$b2,$2a,$00
 db $90,$06,$e0,$da,$b0,$dc
 db $18,$00,$00
 db $90,$04,$b0,$da,$00
 db $18,$00,$00
 db $84,$cf,$08,$e1,$b8,$b1,$ee
 db $18,$00,$00
 db $90,$0a,$b1,$b8,$00
 db $18,$00,$00
 db $90,$43,$0a,$e1,$ee,$b0,$dc
 db $18,$00,$00
 db $84,$cf,$08,$b1,$b8,$b1,$ee
 db $18,$00,$00
 db $90,$0a,$e1,$b8,$00
 db $18,$00,$00
 db $90,$43,$0a,$b1,$ee,$b0,$dc
 db $18,$00,$00
 db $84,$cf,$08,$e1,$b8,$b2,$4b
 db $18,$00,$00
 db $90,$43,$0a,$b1,$ee,$b0,$dc
 db $18,$00,$00
 db $90,$c1,$0a,$e0,$f7,$b1,$25
 db $18,$00,$00
 db $18,$b0,$f7,$00
 db $18,$00,$00
 db $18,$e0,$f7,$00
 db $18,$e1,$25,$00
 db $90,$04,$b0,$f7,$00
 db $18,$b1,$25,$00
 db $84,$08,$e0,$f7,$00
 db $18,$e1,$25,$00
 db $18,$b0,$f7,$00
 db $18,$b1,$25,$00
 db $90,$06,$e0,$f7,$b1,$72
 db $18,$00,$00
 db $84,$08,$b0,$f7,$00
 db $18,$00,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$e1,$72,$00
 db $90,$06,$b0,$f7,$00
 db $18,$b1,$72,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$e1,$72,$00
 db $90,$0a,$b0,$f7,$00
 db $18,$b1,$72,$00
 db $84,$08,$e1,$ee,$b1,$b8
 db $18,$00,$00
 db $90,$04,$b1,$ee,$00
 db $18,$00,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$e1,$b8,$00
 db $18,$b0,$f7,$00
 db $18,$b1,$b8,$00
 db $90,$0a,$e1,$ee,$b2,$e4
 db $18,$00,$00
 db $18,$b1,$ee,$00
 db $18,$00,$00
 db $18,$e0,$f7,$00
 db $18,$e2,$e4,$00
 db $90,$04,$b0,$f7,$00
 db $18,$b2,$e4,$00
 db $84,$08,$e0,$f7,$00
 db $18,$e2,$e4,$00
 db $18,$b0,$f7,$00
 db $18,$b2,$e4,$00
 db $90,$06,$e0,$f7,$00
 db $18,$e2,$e4,$00
 db $84,$08,$b0,$f7,$00
 db $18,$b2,$e4,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$e2,$e4,$00
 db $90,$06,$b0,$f7,$00
 db $18,$b2,$e4,$00
 db $90,$0a,$e0,$f7,$00
 db $18,$e2,$e4,$00
 db $90,$0a,$b0,$f7,$00
 db $18,$b2,$e4,$00
 db $84,$08,$e0,$f7,$b2,$4b
 db $18,$e2,$4b,$00
 db $90,$04,$b0,$f7,$00
 db $18,$b2,$4b,$00
 db $90,$0a,$e0,$f7,$b2,$2a
 db $18,$e2,$2a,$00
 db $18,$b0,$f7,$00
 db $18,$b2,$2a,$00
 db $90,$0a,$e0,$a4,$b2,$4b
 db $18,$e2,$4b,$00
 db $18,$b0,$a4,$00
 db $18,$b2,$4b,$00
 db $18,$e0,$a4,$b1,$45
 db $18,$01,$01
 db $90,$04,$b0,$a4,$b1,$45
 db $18,$01,$01
 db $84,$08,$e0,$a4,$b2,$2a
 db $18,$e2,$2a,$00
 db $18,$b0,$a4,$00
 db $18,$b2,$2a,$00
 db $90,$06,$e0,$a4,$b1,$ee
 db $18,$e1,$ee,$00
 db $84,$08,$b0,$a4,$00
 db $18,$b1,$ee,$00
 db $90,$0a,$e0,$a4,$00
 db $18,$e1,$ee,$00
 db $90,$06,$b0,$a4,$00
 db $18,$b1,$ee,$00
 db $90,$0a,$e0,$a4,$00
 db $18,$e1,$ee,$00
 db $90,$0a,$b0,$a4,$00
 db $18,$b1,$ee,$00
 db $84,$08,$e0,$a4,$00
 db $18,$e1,$ee,$00
 db $90,$04,$b0,$a4,$00
 db $18,$b1,$ee,$00
 db $90,$0a,$e0,$a4,$00
 db $18,$e1,$ee,$00
 db $18,$b0,$a4,$00
 db $18,$b1,$ee,$00
 db $90,$0a,$e1,$72,$b2,$93
 db $18,$00,$00
 db $18,$b1,$72,$00
 db $18,$00,$00
 db $18,$e0,$b9,$b1,$6e
 db $18,$01,$01
 db $90,$04,$b0,$b9,$b1,$6e
 db $18,$01,$01
 db $84,$08,$e1,$72,$b2,$4b
 db $18,$00,$00
 db $18,$b1,$72,$00
 db $18,$00,$00
 db $90,$06,$e1,$72,$b2,$2a
 db $18,$00,$00
 db $90,$04,$b1,$72,$00
 db $18,$00,$00
 db $84,$cf,$14,$e1,$72,$b1,$ee
 db $18,$00,$00
 db $90,$16,$b1,$72,$00
 db $18,$00,$00
 db $98,$03,$e1,$ee,$b0,$b9
 db $18,$00,$00
 db $84,$cf,$18,$b1,$72,$b2,$2a
 db $18,$00,$00
 db $90,$1a,$e1,$72,$00
 db $18,$00,$00
 db $98,$03,$b1,$ee,$b0,$b9
 db $18,$00,$00
 db $84,$cf,$1c,$e1,$72,$b2,$4b
 db $18,$00,$00
 db $90,$43,$1e,$b2,$2a,$b0,$b9
 db $18,$00,$00
 db $90,$d1,$0a,$e1,$88,$b1,$ee
 db $18,$e2,$4b,$00
 db $18,$b1,$88,$00
 db $18,$b2,$4b,$00
 db $18,$e1,$88,$00
 db $18,$e2,$4b,$00
 db $90,$04,$b0,$c4,$00
 db $18,$b3,$dc,$00
 db $84,$08,$e0,$c4,$00
 db $18,$e3,$dc,$00
 db $18,$b0,$c4,$00
 db $18,$b3,$dc,$00
 db $90,$06,$e0,$c4,$00
 db $18,$e3,$dc,$00
 db $84,$08,$b0,$c4,$00
 db $18,$b3,$dc,$00
 db $90,$0a,$e0,$c4,$00
 db $18,$e3,$dc,$00
 db $90,$06,$b0,$c4,$00
 db $18,$b3,$dc,$00
 db $90,$0a,$e0,$c4,$00
 db $18,$e3,$dc,$00
 db $90,$0a,$b0,$c4,$00
 db $18,$b3,$dc,$00
 db $84,$08,$e0,$c4,$00
 db $18,$01,$00
 db $90,$04,$b0,$c4,$00
 db $18,$01,$00
 db $90,$0a,$e0,$c4,$b2,$93
 db $18,$01,$00
 db $18,$b0,$c4,$b2,$e4
 db $18,$01,$00
 db $90,$0a,$e0,$c4,$b3,$70
 db $18,$e2,$93,$00
 db $18,$b0,$c4,$00
 db $18,$b2,$e4,$00
 db $18,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $90,$04,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $84,$08,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $18,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $90,$06,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $84,$08,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $90,$0a,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $90,$06,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $90,$0a,$e0,$c4,$00
 db $18,$01,$00
 db $90,$0a,$b0,$c4,$00
 db $18,$01,$00
 db $84,$08,$e0,$c4,$00
 db $18,$01,$00
 db $90,$04,$b0,$c4,$00
 db $18,$01,$00
 db $90,$0a,$e1,$ee,$b3,$10
 db $18,$01,$00
 db $18,$b1,$ee,$b2,$e4
 db $18,$01,$00
 db $90,$0a,$e1,$49,$b3,$10
 db $18,$e3,$10,$00
 db $18,$b1,$49,$00
 db $18,$b2,$e4,$00
 db $18,$e1,$49,$b1,$49
 db $18,$e3,$10,$01
 db $90,$04,$b0,$a4,$b1,$49
 db $18,$b3,$10,$01
 db $84,$08,$e1,$49,$b2,$e4
 db $18,$01,$00
 db $18,$b1,$49,$00
 db $18,$01,$00
 db $90,$06,$e1,$49,$b2,$4b
 db $18,$e2,$e4,$00
 db $84,$08,$b1,$49,$00
 db $18,$b2,$e4,$00
 db $90,$0a,$e1,$49,$00
 db $18,$e2,$4b,$00
 db $90,$06,$b1,$49,$00
 db $18,$b2,$4b,$00
 db $90,$0a,$e1,$49,$00
 db $18,$e2,$4b,$00
 db $90,$0a,$b1,$49,$00
 db $18,$b2,$4b,$00
 db $84,$08,$e1,$49,$00
 db $18,$e2,$4b,$00
 db $90,$04,$b1,$49,$00
 db $18,$b2,$4b,$00
 db $90,$0a,$e1,$49,$00
 db $18,$01,$00
 db $18,$b1,$49,$00
 db $18,$01,$00
 db $90,$0a,$e2,$93,$b3,$10
 db $18,$01,$00
 db $18,$b2,$93,$00
 db $18,$01,$00
 db $18,$e2,$93,$b2,$e4
 db $18,$e3,$10,$00
 db $90,$04,$b2,$93,$00
 db $18,$b3,$10,$00
 db $84,$08,$e1,$49,$b1,$49
 db $18,$e2,$e4,$01
 db $18,$b1,$49,$b1,$49
 db $18,$b2,$e4,$01
 db $90,$06,$e2,$91,$b2,$4b
 db $18,$01,$00
 db $90,$04,$b2,$91,$00
 db $18,$01,$00
 db $84,$cf,$08,$e1,$49,$b1,$ee
 db $18,$00,$00
 db $90,$0a,$b1,$49,$00
 db $18,$00,$00
 db $90,$43,$0a,$e1,$ee,$b0,$a4
 db $18,$00,$00
 db $84,$cf,$08,$b1,$49,$b2,$2a
 db $18,$00,$00
 db $90,$0a,$e1,$49,$00
 db $18,$00,$00
 db $90,$43,$0a,$b1,$ee,$b0,$a4
 db $18,$00,$00
 db $84,$cf,$08,$e1,$49,$b2,$4b
 db $18,$00,$00
 db $90,$0a,$00,$00
 db $18,$00,$00
 db $90,$d1,$0a,$e1,$88,$b1,$ee
 db $18,$e2,$4b,$00
 db $18,$b1,$88,$00
 db $18,$b2,$4b,$00
 db $18,$e1,$88,$00
 db $18,$e2,$4b,$01
 db $90,$04,$b0,$c4,$b1,$ee
 db $18,$b3,$dc,$01
 db $84,$08,$e0,$c4,$b1,$ee
 db $18,$e3,$dc,$00
 db $18,$b0,$c4,$00
 db $18,$b3,$dc,$00
 db $90,$06,$e0,$c4,$00
 db $18,$e3,$dc,$01
 db $84,$08,$b0,$c4,$b1,$ee
 db $18,$b3,$dc,$01
 db $90,$0a,$e0,$c4,$b1,$ee
 db $18,$e3,$dc,$00
 db $90,$06,$b0,$c4,$00
 db $18,$b3,$dc,$00
 db $90,$0a,$e0,$c4,$00
 db $18,$e3,$dc,$01
 db $90,$0a,$b0,$c4,$b1,$ee
 db $18,$b3,$dc,$01
 db $84,$08,$e0,$c4,$b1,$ee
 db $18,$01,$00
 db $90,$04,$b0,$c4,$00
 db $18,$01,$00
 db $90,$0a,$e0,$c4,$b2,$93
 db $18,$01,$00
 db $18,$b0,$c4,$b2,$e4
 db $18,$01,$00
 db $90,$0a,$e0,$c4,$b3,$70
 db $18,$e2,$93,$00
 db $18,$b0,$c4,$00
 db $18,$b2,$e4,$00
 db $18,$e0,$c4,$00
 db $18,$e3,$70,$01
 db $90,$04,$b0,$c4,$b3,$70
 db $18,$b3,$70,$01
 db $84,$08,$e0,$c4,$b3,$70
 db $18,$e3,$70,$00
 db $18,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $90,$06,$e0,$c4,$00
 db $18,$e3,$70,$01
 db $84,$08,$b0,$c4,$b3,$70
 db $18,$b3,$70,$01
 db $90,$0a,$e0,$c4,$b3,$70
 db $18,$e3,$70,$00
 db $90,$06,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $90,$0a,$e0,$c4,$00
 db $18,$01,$01
 db $90,$0a,$b0,$c4,$b3,$70
 db $18,$01,$01
 db $84,$08,$e0,$c4,$b3,$70
 db $18,$01,$00
 db $90,$04,$b0,$c4,$00
 db $18,$01,$00
 db $90,$0a,$e1,$ee,$b3,$10
 db $18,$01,$00
 db $18,$b1,$ee,$b2,$e4
 db $18,$01,$00
 db $90,$0a,$e1,$49,$b3,$10
 db $18,$e3,$10,$00
 db $18,$b1,$49,$00
 db $18,$b2,$e4,$00
 db $18,$e1,$49,$b1,$49
 db $18,$e3,$10,$01
 db $90,$04,$b0,$a4,$b1,$49
 db $18,$b3,$10,$01
 db $84,$08,$e1,$49,$b2,$e4
 db $18,$01,$00
 db $18,$b1,$49,$00
 db $18,$01,$00
 db $90,$06,$e1,$49,$b2,$4b
 db $18,$e2,$e4,$00
 db $84,$08,$b1,$49,$00
 db $18,$b2,$e4,$00
 db $90,$0a,$e1,$49,$00
 db $18,$e2,$4b,$01
 db $90,$06,$b1,$49,$b2,$4b
 db $18,$b2,$4b,$01
 db $90,$0a,$e1,$49,$b2,$4b
 db $18,$e2,$4b,$00
 db $90,$0a,$b1,$49,$00
 db $18,$b2,$4b,$00
 db $84,$08,$e1,$49,$00
 db $18,$e2,$4b,$01
 db $90,$04,$b1,$49,$b2,$4b
 db $18,$b2,$4b,$01
 db $90,$0a,$e1,$49,$b2,$4b
 db $18,$01,$00
 db $18,$b1,$49,$00
 db $18,$01,$00
 db $90,$0a,$e2,$93,$b3,$10
 db $18,$01,$00
 db $18,$b2,$93,$00
 db $18,$01,$00
 db $18,$e2,$93,$b2,$e4
 db $18,$e3,$10,$00
 db $90,$04,$b2,$93,$00
 db $18,$b3,$10,$00
 db $84,$08,$e1,$49,$b1,$49
 db $18,$e2,$e4,$01
 db $18,$b1,$49,$b1,$49
 db $18,$b2,$e4,$01
 db $90,$06,$e2,$91,$b2,$4b
 db $18,$01,$00
 db $90,$04,$b2,$91,$00
 db $18,$01,$00
 db $84,$cf,$08,$e1,$49,$b1,$ee
 db $18,$00,$00
 db $90,$0a,$b1,$49,$00
 db $18,$00,$00
 db $90,$43,$0a,$e1,$ee,$b0,$a4
 db $18,$00,$00
 db $84,$cf,$08,$b1,$49,$b2,$2a
 db $18,$00,$00
 db $90,$0a,$e1,$49,$00
 db $18,$00,$00
 db $90,$43,$0a,$b1,$ee,$b0,$a4
 db $18,$00,$00
 db $84,$cf,$08,$e1,$49,$b2,$4b
 db $18,$00,$00
 db $90,$0a,$00,$00
 db $18,$00,$00
 db $90,$c1,$0a,$e1,$88,$b1,$ee
 db $18,$00,$00
 db $18,$b1,$88,$00
 db $18,$00,$00
 db $18,$e1,$88,$00
 db $18,$00,$00
 db $90,$04,$b0,$c4,$00
 db $18,$b1,$ee,$00
 db $84,$08,$e0,$c4,$00
 db $18,$e1,$ee,$00
 db $18,$b0,$c4,$00
 db $18,$b1,$ee,$00
 db $90,$06,$e0,$c4,$00
 db $18,$e1,$ee,$00
 db $84,$08,$b0,$c4,$00
 db $18,$b1,$ee,$00
 db $90,$0a,$e0,$c4,$00
 db $18,$e1,$ee,$00
 db $90,$06,$b0,$c4,$00
 db $18,$b1,$ee,$00
 db $90,$0a,$e0,$c4,$00
 db $18,$e1,$ee,$00
 db $90,$0a,$b0,$c4,$00
 db $18,$b1,$ee,$00
 db $84,$08,$e0,$c4,$00
 db $18,$e1,$ee,$00
 db $90,$04,$b0,$c4,$00
 db $18,$b1,$ee,$00
 db $90,$0a,$e0,$c4,$b2,$93
 db $18,$e2,$93,$00
 db $18,$b0,$c4,$b2,$e4
 db $18,$b2,$e4,$00
 db $90,$0a,$e0,$c4,$b3,$70
 db $18,$e3,$70,$00
 db $18,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $18,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $90,$04,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $84,$08,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $18,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $90,$06,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $84,$08,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $90,$0a,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $90,$06,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $90,$0a,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $90,$0a,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $84,$08,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $90,$04,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $90,$0a,$e1,$ee,$b3,$10
 db $18,$e3,$10,$00
 db $18,$b1,$ee,$b2,$e4
 db $18,$b2,$e4,$00
 db $90,$0a,$e1,$49,$b3,$10
 db $18,$e3,$10,$00
 db $18,$b1,$49,$00
 db $18,$b3,$10,$00
 db $18,$e1,$49,$b2,$8f
 db $18,$01,$01
 db $90,$04,$b0,$a4,$b2,$8f
 db $18,$01,$01
 db $84,$08,$e1,$49,$b2,$e4
 db $18,$e2,$e4,$00
 db $18,$b1,$49,$00
 db $18,$b2,$e4,$00
 db $90,$06,$e1,$49,$b2,$4b
 db $18,$e2,$4b,$00
 db $84,$08,$b1,$49,$00
 db $18,$b2,$4b,$00
 db $90,$0a,$e1,$49,$00
 db $18,$e2,$4b,$00
 db $90,$06,$b1,$49,$00
 db $18,$b2,$4b,$00
 db $90,$0a,$e1,$49,$00
 db $18,$e2,$4b,$00
 db $90,$0a,$b1,$49,$00
 db $18,$b2,$4b,$00
 db $84,$08,$e1,$49,$00
 db $18,$e2,$4b,$00
 db $90,$04,$b1,$49,$00
 db $18,$b2,$4b,$00
 db $90,$0a,$e1,$49,$00
 db $18,$e2,$4b,$00
 db $18,$b1,$49,$00
 db $18,$b2,$4b,$00
 db $90,$0a,$e2,$93,$b3,$10
 db $18,$e3,$10,$00
 db $18,$b2,$93,$00
 db $18,$b3,$10,$00
 db $18,$e2,$93,$b2,$e4
 db $18,$e2,$e4,$00
 db $90,$04,$b2,$93,$00
 db $18,$b2,$e4,$00
 db $84,$08,$e1,$49,$b1,$b6
 db $18,$01,$01
 db $18,$b1,$49,$b1,$b6
 db $18,$01,$01
 db $90,$06,$e2,$91,$b2,$4b
 db $18,$e2,$4b,$00
 db $90,$04,$b2,$91,$00
 db $18,$b2,$4b,$00
 db $84,$cf,$08,$e1,$49,$b1,$ee
 db $18,$00,$00
 db $90,$0a,$b1,$49,$00
 db $18,$00,$00
 db $90,$43,$0a,$e1,$ee,$b0,$a4
 db $18,$00,$00
 db $84,$cf,$08,$b1,$49,$b2,$2a
 db $18,$00,$00
 db $90,$0a,$e1,$49,$00
 db $18,$00,$00
 db $90,$43,$0a,$b1,$ee,$b0,$a4
 db $18,$00,$00
 db $84,$cf,$08,$e1,$49,$b2,$4b
 db $18,$00,$00
 db $90,$0a,$00,$00
 db $18,$00,$00
 db $90,$c1,$0a,$e0,$c4,$b1,$ee
 db $18,$00,$00
 db $18,$b0,$c4,$00
 db $18,$00,$00
 db $18,$e0,$c4,$00
 db $18,$e1,$ee,$00
 db $90,$04,$b0,$c4,$00
 db $18,$b1,$ee,$00
 db $84,$08,$e0,$c4,$00
 db $18,$e1,$ee,$00
 db $18,$b0,$c4,$00
 db $18,$b1,$ee,$00
 db $90,$06,$e0,$c4,$00
 db $18,$e1,$ee,$00
 db $84,$08,$b0,$c4,$00
 db $18,$b1,$ee,$00
 db $90,$0a,$e3,$10,$00
 db $18,$00,$00
 db $90,$06,$b3,$10,$00
 db $18,$00,$00
 db $90,$43,$0a,$e1,$ee,$b0,$c4
 db $18,$01,$00
 db $90,$c1,$0a,$b3,$10,$b2,$2a
 db $18,$00,$00
 db $84,$08,$e3,$10,$00
 db $18,$00,$00
 db $90,$43,$04,$b1,$ee,$b0,$c4
 db $18,$01,$00
 db $90,$c1,$0a,$e3,$10,$b2,$4b
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $90,$0a,$e0,$c4,$b3,$70
 db $18,$e3,$70,$00
 db $18,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $18,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $90,$04,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $84,$08,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $18,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $90,$06,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $84,$08,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $90,$0a,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $90,$06,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $90,$0a,$e0,$c4,$00
 db $18,$e3,$70,$00
 db $90,$0a,$b0,$c4,$00
 db $18,$b3,$70,$00
 db $84,$08,$e0,$c4,$b3,$10
 db $18,$e3,$10,$00
 db $90,$04,$b0,$c4,$00
 db $18,$b3,$10,$00
 db $90,$0a,$e0,$c4,$b2,$e4
 db $18,$e2,$e4,$00
 db $18,$b0,$c4,$00
 db $18,$b2,$e4,$00
 db $90,$0a,$e0,$dc,$b3,$10
 db $18,$e3,$10,$00
 db $18,$b0,$dc,$00
 db $18,$b3,$10,$00
 db $18,$e0,$dc,$b0,$dc
 db $18,$01,$01
 db $90,$04,$b0,$dc,$b0,$dc
 db $18,$01,$01
 db $84,$08,$e0,$dc,$b2,$e4
 db $18,$e2,$e4,$00
 db $18,$b0,$dc,$00
 db $18,$b2,$e4,$00
 db $90,$06,$e0,$dc,$b2,$93
 db $18,$e2,$93,$00
 db $84,$08,$b0,$dc,$00
 db $18,$b2,$93,$00
 db $90,$0a,$e0,$dc,$00
 db $18,$e2,$93,$00
 db $90,$06,$b0,$dc,$00
 db $18,$b2,$93,$00
 db $90,$0a,$e0,$dc,$00
 db $18,$e2,$93,$00
 db $90,$0a,$b0,$dc,$00
 db $18,$b2,$93,$00
 db $84,$08,$e0,$dc,$00
 db $18,$e2,$93,$00
 db $90,$04,$b0,$dc,$00
 db $18,$b2,$93,$00
 db $90,$0a,$e0,$dc,$00
 db $18,$00,$00
 db $18,$b0,$dc,$00
 db $18,$00,$00
 db $90,$0a,$e0,$dc,$00
 db $18,$00,$00
 db $18,$b0,$dc,$00
 db $18,$00,$00
 db $98,$8f,$e1,$b8,$b2,$4b
 db $18,$00,$00
 db $90,$04,$b1,$b8,$00
 db $18,$00,$00
 db $84,$43,$08,$e2,$4b,$b0,$dc
 db $18,$00,$00
 db $98,$8f,$b1,$b8,$b2,$4b
 db $18,$00,$00
 db $90,$06,$e1,$b8,$00
 db $18,$00,$00
 db $90,$43,$04,$b2,$4b,$b0,$dc
 db $18,$00,$00
 db $84,$cf,$08,$e1,$b8,$b1,$ee
 db $18,$00,$00
 db $90,$0a,$b1,$b8,$00
 db $18,$00,$00
 db $90,$43,$0a,$e2,$4b,$b0,$dc
 db $18,$00,$00
 db $84,$cf,$08,$b1,$b8,$b1,$ee
 db $18,$00,$00
 db $90,$0a,$e1,$b8,$00
 db $18,$00,$00
 db $90,$43,$0a,$b1,$ee,$b0,$dc
 db $18,$00,$00
 db $84,$cf,$08,$e1,$b8,$b2,$4b
 db $18,$00,$00
 db $90,$43,$0a,$b1,$ee,$00
 db $18,$00,$00
 db $84,$47,$08,$00,$f0,$f7
 db $18,$01,$01
 db $94,$00,$b1,$ee,$e0,$f7
 db $98,$03,$f0,$f7,$01
 db $94,$47,$00,$f1,$b8,$f0,$f7
 db $98,$03,$e0,$f7,$01
 db $94,$47,$00,$e1,$b8,$e0,$f7
 db $98,$03,$f0,$f7,$01
 db $94,$47,$02,$f1,$88,$d0,$f7
 db $98,$03,$e0,$f7,$01
 db $94,$47,$00,$e1,$88,$e0,$f7
 db $98,$03,$d0,$f7,$01
 db $98,$07,$d1,$72,$d0,$f7
 db $98,$03,$e0,$f7,$01
 db $94,$47,$02,$e1,$72,$e0,$f7
 db $98,$03,$d0,$f7,$01
 db $94,$47,$00,$d1,$49,$d0,$f7
 db $98,$03,$e0,$f7,$01
 db $94,$47,$02,$e1,$49,$c0,$f7
 db $98,$03,$d0,$f7,$01
 db $94,$47,$00,$d1,$25,$d0,$f7
 db $98,$03,$c0,$f7,$01
 db $94,$47,$00,$c1,$25,$c0,$f7
 db $98,$03,$d0,$f7,$01
 db $94,$47,$02,$d1,$15,$d0,$f7
 db $98,$03,$c0,$f7,$01
 db $94,$47,$00,$c1,$15,$e0,$f7
 db $98,$03,$d0,$f7,$01
 db $94,$47,$00,$00,$f0,$f7
 db $98,$03,$e0,$f7,$01
 db $98,$07,$00,$f0,$f7
 db $98,$03,$f0,$f7,$01
 db $94,$47,$00,$00,$f0,$f5
 db $18,$01,$01
 db $94,$00,$f0,$f7,$f0,$f5
 db $98,$03,$00,$01
 db $94,$47,$00,$f1,$15,$e0,$f3
 db $98,$03,$e0,$f7,$01
 db $94,$47,$00,$e1,$15,$f0,$f3
 db $98,$03,$f0,$f7,$01
 db $94,$47,$02,$f1,$25,$d0,$f1
 db $98,$03,$e0,$f7,$01
 db $94,$47,$00,$e1,$25,$e0,$f1
 db $98,$03,$d0,$f7,$01
 db $98,$07,$d1,$49,$c0,$ef
 db $98,$03,$e0,$f7,$01
 db $94,$47,$02,$e1,$49,$d0,$ef
 db $98,$03,$d0,$f7,$01
 db $94,$47,$00,$d1,$72,$b0,$ed
 db $98,$03,$e0,$f7,$01
 db $94,$47,$02,$e1,$72,$c0,$ed
 db $98,$03,$d0,$f7,$01
 db $94,$47,$00,$d1,$88,$a0,$eb
 db $98,$03,$c0,$f7,$01
 db $94,$47,$00,$c1,$88,$b0,$eb
 db $98,$03,$d0,$f7,$01
 db $94,$47,$02,$d1,$b8,$90,$e9
 db $98,$03,$c0,$f7,$01
 db $94,$47,$00,$c1,$b8,$a0,$e9
 db $98,$03,$d0,$f7,$01
 db $94,$47,$00,$d1,$ee,$80,$e7
 db $98,$03,$e0,$f7,$01
 db $90,$47,$0a,$e1,$ee,$90,$e7
 db $98,$03,$f0,$f7,$01
 db $90,$d1,$0a,$f0,$7b,$90,$7b
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$01
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
.loop
 db $18,$01,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $00
 dw .loop
 align 2
.drumpar
.dp0
 dw .dsmp4+0
 db $01,$09,$40
.dp1
 dw .dsmp5+0
 db $01,$09,$40
.dp2
 dw .dsmp0+0
 db $02,$09,$40
.dp3
 dw .dsmp1+0
 db $02,$09,$40
.dp4
 dw .dsmp3+0
 db $05,$09,$40
.dp5
 dw .dsmp2+0
 db $02,$09,$40
.dp6
 dw .dsmp4+0
 db $01,$03,$40
.dp7
 dw .dsmp5+0
 db $01,$03,$40
.dp8
 dw .dsmp4+0
 db $01,$06,$40
.dp9
 dw .dsmp5+0
 db $01,$06,$40
.dp10
 dw .dsmp3+0
 db $05,$09,$08
.dp11
 dw .dsmp2+0
 db $02,$09,$08
.dp12
 dw .dsmp3+0
 db $05,$09,$18
.dp13
 dw .dsmp2+0
 db $02,$09,$18
.dp14
 dw .dsmp3+0
 db $05,$09,$28
.dp15
 dw .dsmp2+0
 db $02,$09,$28
.dsmp0
 db $00,$78,$00,$f0,$07,$00,$0e,$00,$0f,$80,$38,$00,$00,$70,$00,$7f
 db $00,$02,$1c,$00,$0e,$00,$00,$c0,$01,$80,$00,$00,$00,$00,$00,$00
 db $3e,$00,$00,$00,$00,$07,$00,$00,$30,$00,$60,$00,$00,$c0,$00,$00
 db $00,$1f,$00,$03,$e0,$00,$3c,$00,$00,$0e,$00,$1e,$00,$03,$c0,$00
.dsmp1
 db $00,$00,$c0,$c1,$21,$01,$c3,$00,$b8,$1f,$03,$e0,$70,$0e,$0c,$c0
 db $00,$0c,$1e,$00,$f0,$38,$06,$07,$00,$e0,$70,$0f,$03,$80,$f0,$01
 db $03,$00,$e0,$38,$01,$83,$02,$3c,$1c,$00,$81,$00,$1c,$1c,$40,$00
 db $80,$0c,$00,$00,$c0,$cc,$0f,$00,$80,$e0,$00,$01,$01,$c0,$60,$00
.dsmp2
 db $00,$00,$00,$00,$00,$00,$00,$00,$01,$07,$f3,$fc,$ff,$ff,$ff,$ff
 db $ff,$e7,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$f3,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
 db $ff,$ff,$ff,$ff,$f8,$c0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
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
.dsmp4
 db $03,$ff,$fc,$00,$03,$ff,$f8,$00,$07,$ff,$f0,$00,$0f,$ff,$e0,$00
 db $1f,$ff,$e0,$00,$3f,$ff,$c0,$00,$7f,$ff,$80,$00,$00,$00,$00,$00
.dsmp5
 db $3f,$c0,$7f,$80,$7f,$00,$ff,$00,$ff,$00,$ff,$00,$ff,$01,$fe,$01
 db $fe,$03,$fc,$03,$f8,$07,$f8,$07,$f0,$0f,$f0,$0f,$00,$00,$00,$00







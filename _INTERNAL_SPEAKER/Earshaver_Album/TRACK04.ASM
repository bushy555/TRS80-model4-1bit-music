	output	"TRACK04.BIN"

	org $6000
begin:


; 	EARSHAVER
; 	TRACK 4
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




; TRACK04

;compiled music data

music_data
 dw .drumpar
.song
 db $90,$c1,$00,$80,$b9,$b1,$15
 db $18,$00,$00
 db $18,$00,$91,$15
 db $18,$00,$00
 db $90,$02,$00,$00
 db $18,$00,$00
 db $90,$00,$81,$72,$92,$2a
 db $18,$00,$00
 db $81,$04,$01,$01
 db $17,$00,$00
 db $90,$00,$81,$72,$92,$2a
 db $18,$00,$00
 db $18,$01,$01
 db $18,$00,$00
 db $18,$81,$72,$90,$dc
 db $18,$00,$00
 db $90,$02,$00,$91,$15
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $90,$00,$81,$15,$f1,$72
 db $18,$01,$01
 db $90,$00,$81,$15,$e1,$72
 db $18,$01,$01
 db $81,$04,$81,$15,$d1,$72
 db $17,$01,$01
 db $90,$00,$81,$15,$c1,$72
 db $18,$01,$01
 db $90,$00,$81,$15,$b1,$72
 db $18,$01,$01
 db $90,$00,$81,$15,$a1,$72
 db $18,$01,$01
 db $90,$00,$80,$b9,$b1,$15
 db $18,$00,$00
 db $18,$00,$91,$15
 db $18,$00,$00
 db $90,$02,$00,$00
 db $18,$00,$00
 db $90,$00,$81,$72,$92,$2a
 db $18,$00,$00
 db $81,$04,$01,$01
 db $17,$00,$00
 db $90,$00,$81,$72,$92,$2a
 db $18,$00,$00
 db $18,$01,$01
 db $18,$00,$00
 db $90,$00,$81,$72,$91,$15
 db $18,$00,$00
 db $90,$02,$81,$49,$90,$f7
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $90,$00,$80,$f7,$b1,$45
 db $18,$01,$01
 db $90,$00,$80,$f7,$a1,$45
 db $18,$01,$01
 db $81,$04,$80,$f7,$b1,$45
 db $17,$01,$01
 db $90,$02,$80,$f7,$a1,$45
 db $18,$01,$01
 db $90,$02,$80,$f7,$b1,$45
 db $18,$01,$01
 db $90,$02,$80,$f7,$c1,$45
 db $18,$01,$01
 db $90,$00,$80,$b9,$b1,$15
 db $18,$00,$00
 db $18,$00,$91,$15
 db $18,$00,$00
 db $90,$02,$00,$00
 db $18,$00,$00
 db $90,$00,$81,$72,$92,$2a
 db $18,$00,$00
 db $81,$04,$01,$01
 db $17,$00,$00
 db $90,$00,$81,$72,$92,$2a
 db $18,$00,$00
 db $18,$01,$01
 db $18,$00,$00
 db $18,$81,$72,$90,$dc
 db $18,$00,$00
 db $90,$02,$00,$91,$15
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $90,$00,$81,$15,$f1,$72
 db $18,$01,$01
 db $90,$00,$81,$15,$e1,$72
 db $18,$01,$01
 db $81,$04,$81,$15,$d1,$72
 db $17,$01,$01
 db $90,$00,$81,$15,$c1,$72
 db $18,$01,$01
 db $90,$00,$81,$15,$b1,$72
 db $18,$01,$01
 db $90,$00,$81,$15,$a1,$72
 db $18,$01,$01
 db $90,$00,$80,$b9,$b1,$15
 db $18,$00,$00
 db $18,$00,$91,$15
 db $18,$00,$00
 db $90,$02,$00,$00
 db $18,$00,$00
 db $90,$00,$81,$72,$92,$2a
 db $18,$00,$00
 db $81,$04,$01,$01
 db $17,$00,$00
 db $90,$00,$81,$72,$92,$2a
 db $18,$00,$00
 db $18,$01,$01
 db $18,$00,$00
 db $90,$00,$81,$72,$91,$15
 db $18,$00,$00
 db $90,$02,$81,$49,$90,$f7
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $90,$00,$e1,$49,$e1,$9b
 db $18,$00,$00
 db $90,$00,$b1,$49,$b1,$9b
 db $18,$00,$00
 db $81,$04,$e1,$49,$e1,$9b
 db $17,$00,$00
 db $90,$00,$b1,$49,$b1,$9b
 db $18,$00,$00
 db $81,$04,$e1,$49,$e1,$9b
 db $17,$00,$00
 db $90,$00,$b1,$49,$b1,$9b
 db $18,$00,$00
 db $90,$00,$e1,$ee,$b1,$72
 db $18,$00,$00
 db $18,$b1,$ee,$00
 db $18,$00,$00
 db $94,$06,$b0,$f3,$b0,$7b
 db $18,$00,$00
 db $90,$08,$b1,$ee,$b1,$72
 db $18,$00,$00
 db $81,$0a,$b0,$f3,$b0,$7b
 db $13,$00,$00
 db $90,$00,$e0,$f7,$b1,$72
 db $18,$01,$01
 db $94,$06,$e0,$f7,$b1,$72
 db $18,$01,$01
 db $18,$b0,$f7,$b1,$72
 db $18,$01,$01
 db $94,$06,$e1,$ee,$b1,$72
 db $18,$00,$00
 db $90,$08,$b1,$ee,$00
 db $18,$00,$00
 db $90,$08,$b0,$f3,$b0,$7b
 db $18,$00,$00
 db $90,$08,$b1,$ee,$b1,$72
 db $18,$00,$00
 db $81,$0a,$b0,$f3,$b0,$7b
 db $13,$00,$00
 db $18,$b0,$f7,$80,$f5
 db $18,$01,$01
 db $90,$00,$e0,$f7,$80,$f7
 db $18,$01,$01
 db $18,$b0,$f7,$80,$f7
 db $18,$01,$01
 db $90,$00,$e2,$2a,$b1,$9f
 db $18,$00,$00
 db $18,$b2,$2a,$00
 db $18,$00,$00
 db $94,$06,$b1,$11,$b0,$8a
 db $18,$00,$00
 db $90,$08,$b2,$2a,$b1,$9f
 db $18,$00,$00
 db $81,$0a,$b1,$11,$b0,$8a
 db $13,$00,$00
 db $90,$00,$e1,$15,$b1,$9f
 db $18,$01,$01
 db $94,$06,$e1,$15,$b1,$9f
 db $18,$01,$01
 db $18,$b1,$15,$b1,$9f
 db $18,$01,$01
 db $94,$06,$e2,$2a,$b1,$9f
 db $18,$00,$00
 db $90,$08,$b2,$2a,$00
 db $18,$00,$00
 db $90,$08,$b1,$11,$b0,$8a
 db $18,$00,$00
 db $90,$08,$b2,$2a,$b1,$9f
 db $18,$00,$00
 db $81,$0a,$b1,$11,$b0,$8a
 db $13,$00,$00
 db $18,$b1,$15,$81,$13
 db $18,$01,$01
 db $90,$00,$e1,$15,$81,$15
 db $18,$01,$01
 db $18,$b1,$15,$81,$15
 db $18,$01,$01
 db $90,$00,$e1,$ee,$b1,$72
 db $18,$00,$00
 db $18,$b1,$ee,$00
 db $18,$00,$00
 db $94,$06,$b0,$f3,$b0,$7b
 db $18,$00,$00
 db $90,$08,$b1,$ee,$b1,$72
 db $18,$00,$00
 db $81,$0a,$b0,$f3,$b0,$7b
 db $13,$00,$00
 db $90,$00,$e0,$f7,$b1,$72
 db $18,$01,$01
 db $94,$06,$e0,$f7,$b1,$72
 db $18,$01,$01
 db $18,$b0,$f7,$b1,$72
 db $18,$01,$01
 db $94,$06,$e1,$ee,$b1,$72
 db $18,$00,$00
 db $90,$08,$b1,$ee,$00
 db $18,$00,$00
 db $90,$08,$b0,$f3,$b0,$7b
 db $18,$00,$00
 db $90,$08,$b1,$ee,$b1,$72
 db $18,$00,$00
 db $81,$0a,$b0,$f3,$b0,$7b
 db $13,$00,$00
 db $18,$b0,$f7,$80,$f5
 db $18,$01,$01
 db $90,$00,$e0,$f7,$80,$f7
 db $18,$01,$01
 db $18,$b0,$f7,$80,$f7
 db $18,$01,$01
 db $90,$00,$e1,$25,$91,$b6
 db $18,$00,$00
 db $18,$b1,$25,$00
 db $18,$00,$00
 db $94,$06,$e1,$25,$00
 db $18,$00,$00
 db $90,$08,$b1,$25,$00
 db $18,$00,$00
 db $81,$0a,$e1,$25,$00
 db $13,$00,$00
 db $90,$00,$b1,$25,$00
 db $18,$00,$00
 db $94,$06,$e1,$25,$00
 db $18,$00,$00
 db $18,$b1,$25,$00
 db $18,$00,$00
 db $81,$0c,$e2,$2a,$e3,$3f
 db $17,$00,$00
 db $81,$0e,$b2,$2a,$b3,$3f
 db $17,$00,$00
 db $81,$0c,$e2,$2a,$e3,$3f
 db $17,$00,$00
 db $81,$0e,$b2,$2a,$b3,$3f
 db $17,$00,$00
 db $81,$10,$e2,$2a,$e3,$3f
 db $17,$00,$00
 db $81,$12,$b2,$2a,$b3,$3f
 db $17,$00,$00
 db $81,$10,$e2,$2a,$e3,$3f
 db $17,$00,$00
 db $81,$12,$b2,$2a,$b3,$3f
 db $17,$00,$00
 db $90,$00,$b0,$b9,$b1,$15
 db $18,$00,$00
 db $90,$14,$00,$91,$15
 db $18,$00,$00
 db $90,$16,$00,$00
 db $18,$00,$00
 db $90,$00,$b1,$72,$92,$2a
 db $18,$00,$00
 db $81,$04,$01,$01
 db $17,$00,$00
 db $90,$00,$b1,$72,$92,$2a
 db $18,$00,$00
 db $90,$16,$01,$01
 db $18,$00,$00
 db $90,$14,$b1,$72,$90,$dc
 db $18,$00,$00
 db $90,$16,$00,$91,$15
 db $18,$00,$00
 db $90,$14,$00,$00
 db $18,$00,$00
 db $90,$cb,$00,$e4,$55,$82,$28
 db $98,$03,$00,$01
 db $90,$cb,$00,$00,$92,$26
 db $98,$03,$e3,$3f,$01
 db $81,$cb,$04,$e8,$ab,$a2,$8d
 db $97,$03,$e2,$93,$01
 db $90,$cb,$00,$e8,$ab,$b2,$8b
 db $98,$03,$e4,$55,$01
 db $90,$cb,$00,$00,$c8,$a1
 db $98,$03,$e3,$3f,$01
 db $90,$cb,$00,$e4,$55,$d4,$49
 db $98,$03,$e2,$93,$01
 db $90,$c1,$00,$e0,$b9,$b1,$15
 db $18,$00,$00
 db $90,$14,$00,$91,$15
 db $18,$00,$00
 db $90,$16,$00,$00
 db $18,$00,$00
 db $90,$00,$e1,$72,$92,$2a
 db $18,$00,$00
 db $81,$04,$01,$01
 db $17,$00,$00
 db $90,$00,$e1,$72,$92,$2a
 db $18,$00,$00
 db $90,$16,$01,$01
 db $18,$00,$00
 db $90,$00,$e1,$72,$91,$15
 db $18,$00,$00
 db $90,$16,$e1,$49,$90,$f7
 db $18,$00,$00
 db $90,$14,$00,$00
 db $18,$00,$00
 db $90,$00,$e1,$ee,$b1,$45
 db $18,$00,$00
 db $90,$00,$00,$a1,$45
 db $18,$00,$00
 db $81,$04,$00,$b1,$45
 db $17,$00,$00
 db $90,$14,$00,$a1,$45
 db $18,$00,$00
 db $90,$16,$00,$b1,$45
 db $18,$00,$00
 db $90,$14,$00,$c1,$45
 db $18,$00,$00
 db $90,$00,$e0,$b9,$b1,$15
 db $18,$00,$00
 db $90,$14,$00,$91,$15
 db $18,$00,$00
 db $90,$16,$00,$00
 db $18,$00,$00
 db $90,$00,$e1,$72,$92,$2a
 db $18,$00,$00
 db $81,$04,$01,$01
 db $17,$00,$00
 db $90,$00,$e1,$72,$92,$2a
 db $18,$00,$00
 db $90,$16,$01,$01
 db $18,$00,$00
 db $90,$14,$e1,$72,$90,$dc
 db $18,$00,$00
 db $90,$16,$00,$91,$15
 db $18,$00,$00
 db $90,$14,$00,$00
 db $18,$00,$00
 db $90,$cb,$00,$e4,$55,$82,$28
 db $98,$03,$00,$01
 db $90,$cb,$00,$00,$92,$26
 db $98,$03,$e3,$3f,$01
 db $81,$cb,$04,$e8,$ab,$a2,$8d
 db $97,$03,$e2,$93,$01
 db $90,$cb,$00,$e8,$ab,$b2,$8b
 db $98,$03,$e4,$55,$01
 db $90,$cb,$00,$00,$c8,$a1
 db $98,$03,$e3,$3f,$01
 db $90,$cb,$00,$e4,$55,$d4,$49
 db $98,$03,$e2,$93,$01
 db $90,$c1,$00,$e0,$b9,$b1,$15
 db $18,$00,$00
 db $90,$14,$00,$91,$15
 db $18,$00,$00
 db $90,$16,$00,$00
 db $18,$00,$00
 db $90,$00,$e1,$72,$92,$2a
 db $18,$00,$00
 db $81,$04,$01,$01
 db $17,$00,$00
 db $90,$00,$e1,$72,$92,$2a
 db $18,$00,$00
 db $90,$16,$01,$01
 db $18,$00,$00
 db $90,$00,$e1,$72,$91,$15
 db $18,$00,$00
 db $90,$16,$e1,$49,$90,$f7
 db $18,$00,$00
 db $90,$14,$00,$00
 db $18,$00,$00
 db $90,$00,$e2,$93,$83,$3d
 db $18,$00,$00
 db $90,$00,$b2,$93,$00
 db $18,$00,$00
 db $81,$04,$e2,$93,$93,$3b
 db $17,$00,$00
 db $90,$00,$b2,$93,$00
 db $18,$00,$00
 db $81,$04,$e2,$93,$a3,$39
 db $17,$00,$00
 db $90,$00,$b2,$93,$00
 db $18,$00,$00
 db $90,$00,$e1,$ee,$b2,$e4
 db $18,$00,$00
 db $90,$14,$b1,$ee,$00
 db $18,$00,$00
 db $94,$06,$b0,$f3,$b0,$7b
 db $18,$00,$00
 db $90,$08,$b1,$ee,$b2,$e4
 db $18,$00,$00
 db $81,$0a,$b0,$f3,$b0,$7b
 db $13,$00,$00
 db $90,$00,$e0,$f7,$b1,$72
 db $18,$01,$01
 db $94,$06,$e0,$f7,$b1,$72
 db $18,$01,$01
 db $90,$14,$b0,$f7,$b1,$72
 db $18,$01,$01
 db $94,$06,$e1,$ee,$b2,$e4
 db $18,$00,$00
 db $90,$08,$b1,$ee,$00
 db $18,$00,$00
 db $90,$08,$b0,$f3,$b0,$7b
 db $18,$00,$00
 db $90,$08,$b1,$ee,$b2,$e4
 db $18,$00,$00
 db $81,$0a,$b0,$f3,$b0,$7b
 db $13,$00,$00
 db $90,$14,$b0,$f7,$80,$f5
 db $18,$01,$01
 db $90,$00,$e0,$f7,$80,$f7
 db $18,$01,$01
 db $90,$14,$b0,$f7,$80,$f7
 db $18,$01,$01
 db $90,$00,$e2,$2a,$b3,$3f
 db $18,$00,$00
 db $90,$14,$b2,$2a,$00
 db $18,$00,$00
 db $94,$06,$b1,$11,$b0,$8a
 db $18,$00,$00
 db $90,$08,$b2,$2a,$b3,$3f
 db $18,$00,$00
 db $81,$0a,$b1,$11,$b0,$8a
 db $13,$00,$00
 db $90,$00,$e1,$15,$b1,$9f
 db $18,$01,$01
 db $94,$06,$e1,$15,$b1,$9f
 db $18,$01,$01
 db $90,$14,$b1,$15,$b1,$9f
 db $18,$01,$01
 db $94,$06,$e2,$2a,$b3,$3f
 db $18,$00,$00
 db $90,$08,$b2,$2a,$00
 db $18,$00,$00
 db $90,$08,$b1,$11,$b0,$8a
 db $18,$00,$00
 db $90,$08,$b2,$2a,$b3,$3f
 db $18,$00,$00
 db $81,$0a,$b1,$11,$b0,$8a
 db $13,$00,$00
 db $90,$14,$b1,$15,$81,$13
 db $18,$01,$01
 db $90,$00,$e1,$15,$81,$15
 db $18,$01,$01
 db $90,$14,$b1,$15,$81,$15
 db $18,$01,$01
 db $90,$00,$e1,$ee,$b2,$e4
 db $18,$00,$00
 db $90,$14,$b1,$ee,$00
 db $18,$00,$00
 db $94,$06,$b0,$f3,$b0,$7b
 db $18,$00,$00
 db $90,$08,$b1,$ee,$b2,$e4
 db $18,$00,$00
 db $81,$0a,$b0,$f3,$b0,$7b
 db $13,$00,$00
 db $90,$00,$e0,$f7,$b1,$72
 db $18,$01,$01
 db $94,$06,$e0,$f7,$b1,$72
 db $18,$01,$01
 db $90,$14,$b0,$f7,$b1,$72
 db $18,$01,$01
 db $94,$06,$e1,$ee,$b2,$e4
 db $18,$00,$00
 db $90,$08,$b1,$ee,$00
 db $18,$00,$00
 db $90,$08,$b0,$f3,$b0,$7b
 db $18,$00,$00
 db $90,$08,$b1,$ee,$b2,$e4
 db $18,$00,$00
 db $81,$0a,$b0,$f3,$b0,$7b
 db $13,$00,$00
 db $90,$14,$b0,$f7,$80,$f5
 db $18,$01,$01
 db $90,$00,$e0,$f7,$80,$f7
 db $18,$01,$01
 db $90,$14,$b0,$f7,$80,$f7
 db $18,$01,$01
 db $81,$0a,$e1,$25,$91,$b6
 db $13,$00,$00
 db $90,$14,$b1,$25,$00
 db $18,$00,$00
 db $81,$0a,$e1,$25,$00
 db $13,$00,$00
 db $90,$08,$b1,$25,$00
 db $18,$00,$00
 db $81,$0a,$e1,$25,$00
 db $13,$00,$00
 db $90,$00,$b1,$25,$00
 db $18,$00,$00
 db $81,$0a,$e1,$25,$00
 db $13,$00,$00
 db $90,$14,$b1,$25,$00
 db $18,$00,$00
 db $81,$04,$e2,$2a,$e3,$3f
 db $97,$03,$f8,$ab,$f8,$a9
 db $98,$81,$b2,$2a,$b3,$3d
 db $98,$03,$e6,$7e,$e6,$7c
 db $98,$81,$e2,$2a,$e3,$3b
 db $98,$03,$d5,$27,$d5,$25
 db $98,$81,$b2,$2a,$b3,$39
 db $98,$03,$01,$c8,$ab
 db $98,$81,$e2,$2a,$e3,$37
 db $98,$03,$01,$b6,$7e
 db $98,$81,$b2,$2a,$b3,$35
 db $98,$03,$01,$a5,$27
 db $98,$81,$e2,$2a,$e3,$33
 db $98,$03,$98,$ab,$01
 db $98,$81,$b2,$2a,$b3,$31
 db $98,$03,$86,$7e,$01
 db $90,$c1,$00,$e2,$e0,$b2,$e2
 db $18,$00,$00
 db $18,$e0,$b9,$b0,$b9
 db $18,$01,$01
 db $18,$e0,$b9,$b0,$b9
 db $98,$03,$f2,$e4,$01
 db $98,$81,$f0,$b9,$b0,$b9
 db $98,$03,$f2,$e4,$01
 db $81,$c1,$04,$00,$b2,$bb
 db $17,$00,$00
 db $18,$f0,$b9,$b0,$b9
 db $18,$01,$01
 db $18,$f0,$b9,$b0,$b9
 db $98,$03,$f2,$bb,$01
 db $98,$81,$f0,$b9,$b0,$b9
 db $98,$03,$f2,$bb,$01
 db $98,$81,$f2,$e4,$b2,$4b
 db $18,$00,$00
 db $18,$f0,$b9,$b0,$b9
 db $18,$01,$01
 db $90,$00,$f0,$b9,$b0,$b9
 db $98,$03,$f2,$4b,$01
 db $90,$c1,$00,$f0,$b9,$b0,$b9
 db $98,$03,$f2,$4b,$01
 db $81,$c1,$04,$f2,$e4,$b2,$2a
 db $97,$03,$00,$00
 db $18,$f0,$b9,$01
 db $18,$01,$00
 db $18,$f0,$b9,$00
 db $18,$f2,$2a,$00
 db $98,$81,$f0,$b9,$b0,$b9
 db $98,$03,$f2,$2a,$01
 db $98,$81,$e0,$b5,$b0,$b5
 db $18,$01,$01
 db $18,$e0,$b9,$b0,$b9
 db $18,$01,$01
 db $90,$00,$e2,$e4,$82,$d6
 db $18,$00,$00
 db $18,$e0,$b9,$80,$b9
 db $18,$01,$01
 db $81,$04,$e2,$e4,$82,$bb
 db $17,$00,$00
 db $18,$e0,$b9,$80,$b9
 db $98,$03,$e2,$e4,$01
 db $98,$81,$00,$82,$4b
 db $18,$00,$00
 db $18,$e0,$b9,$80,$b9
 db $98,$03,$e2,$bb,$01
 db $98,$81,$e0,$b9,$80,$b9
 db $98,$03,$e2,$4b,$01
 db $98,$81,$e0,$b9,$80,$b9
 db $98,$03,$e2,$4b,$01
 db $90,$c1,$00,$e2,$e4,$82,$2a
 db $98,$03,$00,$00
 db $90,$00,$e0,$b9,$01
 db $18,$01,$00
 db $81,$04,$e0,$b9,$00
 db $17,$e2,$2a,$00
 db $98,$81,$e0,$b9,$80,$b9
 db $98,$03,$e2,$2a,$01
 db $81,$c1,$04,$e0,$b9,$80,$b9
 db $17,$01,$01
 db $81,$04,$e0,$b9,$80,$b9
 db $17,$01,$01
 db $90,$00,$e4,$51,$82,$26
 db $18,$00,$00
 db $18,$e1,$15,$81,$15
 db $18,$01,$01
 db $18,$e1,$15,$81,$15
 db $98,$03,$d2,$2a,$01
 db $98,$81,$d4,$55,$82,$bb
 db $18,$00,$82,$4b
 db $81,$04,$00,$82,$bb
 db $17,$00,$00
 db $18,$d1,$15,$81,$15
 db $98,$03,$d2,$4b,$01
 db $98,$81,$d4,$55,$82,$e4
 db $18,$00,$00
 db $18,$d1,$15,$81,$15
 db $98,$03,$d2,$bb,$01
 db $98,$81,$d4,$55,$82,$bb
 db $18,$00,$00
 db $18,$d1,$15,$81,$15
 db $98,$03,$d2,$e4,$01
 db $90,$c1,$00,$d4,$55,$82,$4b
 db $18,$00,$00
 db $90,$00,$d1,$15,$81,$15
 db $98,$03,$d2,$bb,$01
 db $81,$c1,$04,$d4,$55,$82,$26
 db $17,$00,$00
 db $18,$d1,$15,$81,$15
 db $98,$03,$d2,$4b,$01
 db $98,$81,$d4,$55,$81,$ee
 db $18,$00,$00
 db $18,$d1,$15,$81,$15
 db $98,$03,$d2,$2a,$01
 db $98,$81,$d3,$dc,$82,$4b
 db $18,$00,$00
 db $18,$d0,$f7,$80,$f7
 db $98,$03,$c3,$dc,$01
 db $90,$c1,$00,$c0,$f7,$80,$f7
 db $98,$03,$c2,$4b,$01
 db $98,$81,$c0,$f7,$80,$f7
 db $98,$03,$c2,$4b,$01
 db $81,$c1,$04,$c3,$dc,$81,$9f
 db $17,$00,$00
 db $18,$c0,$f7,$80,$f7
 db $18,$01,$01
 db $18,$c3,$dc,$81,$ea
 db $18,$00,$00
 db $18,$c0,$f7,$80,$f7
 db $98,$03,$c1,$9f,$01
 db $18,$f2,$4b,$80,$f7
 db $18,$f2,$e4,$01
 db $18,$e3,$3f,$80,$f7
 db $18,$e2,$4b,$01
 db $90,$00,$d2,$e4,$80,$f7
 db $18,$d3,$3f,$01
 db $90,$00,$c2,$4b,$80,$f7
 db $18,$c2,$e4,$01
 db $81,$04,$b3,$3f,$80,$f7
 db $17,$b2,$4b,$01
 db $18,$a2,$e4,$80,$f7
 db $18,$a3,$3f,$01
 db $81,$04,$92,$4b,$80,$f7
 db $17,$92,$e4,$01
 db $81,$04,$83,$3f,$80,$f7
 db $17,$82,$4b,$01
 db $90,$c1,$00,$e0,$b5,$80,$b5
 db $18,$01,$01
 db $18,$e0,$b9,$80,$b9
 db $18,$01,$01
 db $18,$e0,$b9,$80,$b9
 db $18,$01,$01
 db $18,$e0,$b9,$80,$b9
 db $18,$01,$01
 db $81,$04,$e2,$e4,$82,$bb
 db $17,$00,$00
 db $18,$00,$82,$e0
 db $18,$00,$00
 db $18,$00,$82,$bb
 db $18,$00,$00
 db $18,$e0,$b9,$80,$b9
 db $98,$03,$b2,$e4,$01
 db $98,$81,$00,$82,$4b
 db $18,$00,$00
 db $18,$b0,$b9,$80,$b9
 db $18,$01,$01
 db $90,$00,$b0,$b9,$80,$b9
 db $98,$03,$b2,$4b,$01
 db $90,$c1,$00,$b0,$b9,$80,$b9
 db $98,$03,$b2,$4b,$01
 db $81,$c1,$04,$b0,$b9,$80,$b9
 db $17,$01,$01
 db $18,$b0,$b9,$80,$b9
 db $18,$01,$01
 db $18,$b2,$e4,$82,$2a
 db $18,$00,$00
 db $18,$b0,$b9,$80,$b9
 db $18,$01,$01
 db $18,$b2,$e4,$82,$e0
 db $18,$00,$00
 db $18,$b0,$b9,$80,$b9
 db $98,$03,$a2,$2a,$01
 db $90,$c1,$00,$a0,$b9,$80,$b9
 db $98,$03,$a2,$e4,$01
 db $98,$81,$a0,$b9,$80,$b9
 db $98,$03,$a2,$e4,$01
 db $81,$c1,$04,$00,$82,$bb
 db $17,$00,$00
 db $18,$a0,$b9,$80,$b9
 db $18,$01,$01
 db $18,$a2,$e4,$82,$4b
 db $18,$00,$00
 db $18,$a0,$b9,$80,$b9
 db $98,$03,$a2,$bb,$01
 db $98,$81,$a0,$b9,$80,$b9
 db $98,$03,$a2,$4b,$01
 db $98,$81,$a0,$b9,$80,$b9
 db $98,$03,$a2,$4b,$01
 db $90,$c1,$00,$a2,$e4,$82,$2a
 db $18,$00,$00
 db $90,$00,$a0,$b9,$80,$b9
 db $18,$01,$01
 db $81,$04,$a0,$b9,$80,$b9
 db $97,$03,$a2,$2a,$01
 db $98,$81,$a0,$b9,$80,$b9
 db $98,$03,$a2,$2a,$01
 db $81,$c1,$04,$a0,$b9,$80,$b9
 db $17,$01,$01
 db $81,$04,$a0,$b9,$80,$b9
 db $17,$01,$01
 db $90,$00,$e1,$11,$81,$11
 db $18,$01,$01
 db $18,$e1,$15,$81,$15
 db $18,$01,$01
 db $18,$e1,$15,$81,$15
 db $18,$01,$01
 db $18,$e1,$15,$81,$15
 db $18,$01,$01
 db $81,$04,$e2,$2a,$81,$9f
 db $17,$00,$00
 db $18,$e1,$15,$81,$15
 db $18,$01,$01
 db $18,$e2,$2a,$81,$b8
 db $18,$00,$00
 db $18,$e1,$15,$81,$15
 db $98,$03,$b1,$9f,$01
 db $98,$81,$b2,$2a,$81,$ee
 db $18,$00,$00
 db $18,$b1,$15,$81,$15
 db $98,$03,$b1,$b8,$01
 db $90,$c1,$00,$b2,$2a,$82,$26
 db $18,$00,$00
 db $90,$00,$b1,$15,$81,$15
 db $98,$03,$b1,$ee,$01
 db $81,$c1,$04,$b2,$2a,$82,$4b
 db $17,$00,$00
 db $18,$b1,$15,$81,$15
 db $98,$03,$b2,$2a,$01
 db $98,$81,$00,$82,$4b
 db $18,$00,$00
 db $18,$00,$82,$bb
 db $18,$00,$00
 db $18,$b1,$ee,$82,$4b
 db $18,$00,$00
 db $18,$b0,$f7,$80,$f7
 db $98,$03,$c2,$bb,$01
 db $90,$c1,$00,$c0,$f7,$80,$f7
 db $98,$03,$c2,$4b,$01
 db $98,$81,$c0,$f7,$80,$f7
 db $98,$03,$c2,$4b,$01
 db $81,$c1,$04,$c1,$ee,$82,$bb
 db $17,$00,$00
 db $18,$c0,$f7,$80,$f7
 db $18,$01,$01
 db $18,$c1,$ee,$82,$e4
 db $18,$00,$00
 db $18,$c0,$f7,$80,$f7
 db $98,$03,$c2,$bb,$01
 db $18,$f2,$4b,$80,$f7
 db $18,$f2,$e4,$01
 db $18,$e3,$3f,$80,$f7
 db $18,$e2,$4b,$01
 db $90,$00,$d2,$e4,$80,$f7
 db $18,$d3,$3f,$01
 db $90,$00,$c2,$4b,$80,$f7
 db $18,$c2,$e4,$01
 db $81,$c1,$04,$c1,$ee,$82,$e4
 db $17,$00,$00
 db $18,$00,$82,$bb
 db $18,$00,$00
 db $81,$04,$00,$82,$4b
 db $17,$00,$00
 db $81,$04,$c0,$f7,$80,$f7
 db $97,$03,$c2,$bb,$01
 db $90,$c1,$00,$c1,$b8,$82,$2a
 db $18,$00,$00
 db $90,$14,$c0,$dc,$80,$dc
 db $98,$03,$d2,$4b,$01
 db $94,$c1,$06,$d1,$b8,$81,$b8
 db $98,$03,$d2,$2a,$01
 db $90,$c1,$00,$d1,$b8,$81,$b8
 db $98,$03,$d2,$2a,$01
 db $81,$c1,$0a,$d1,$b8,$81,$9f
 db $13,$00,$00
 db $94,$06,$d0,$dc,$80,$dc
 db $18,$01,$01
 db $90,$16,$d1,$b8,$81,$b4
 db $18,$00,$00
 db $90,$00,$00,$81,$b8
 db $98,$03,$d1,$9f,$01
 db $90,$c1,$00,$d0,$dc,$80,$dc
 db $98,$03,$d1,$b8,$01
 db $90,$c1,$14,$d0,$dc,$80,$dc
 db $98,$03,$d1,$b8,$01
 db $94,$c1,$06,$00,$81,$b8
 db $18,$01,$01
 db $90,$00,$d1,$b8,$81,$b8
 db $18,$01,$01
 db $81,$04,$d0,$dc,$80,$dc
 db $17,$01,$01
 db $90,$00,$d0,$dc,$80,$dc
 db $18,$01,$01
 db $90,$16,$d1,$b8,$82,$e4
 db $18,$00,$00
 db $90,$14,$00,$82,$bb
 db $18,$00,$00
 db $90,$00,$d1,$ee,$82,$4b
 db $18,$00,$00
 db $90,$14,$d0,$f7,$80,$f7
 db $98,$03,$e2,$bb,$01
 db $94,$c1,$06,$e1,$ee,$81,$ee
 db $98,$03,$e2,$4b,$01
 db $90,$c1,$00,$e1,$ee,$81,$ee
 db $98,$03,$e2,$4b,$01
 db $81,$c1,$0a,$e1,$ee,$81,$72
 db $13,$00,$00
 db $94,$06,$00,$00
 db $18,$00,$00
 db $90,$16,$00,$82,$4b
 db $18,$00,$00
 db $90,$00,$00,$81,$ee
 db $98,$03,$e1,$72,$01
 db $90,$c1,$00,$e0,$f7,$80,$f7
 db $98,$03,$e2,$4b,$01
 db $90,$c1,$14,$e0,$f7,$80,$f7
 db $98,$03,$e2,$4b,$01
 db $94,$c1,$06,$e1,$ee,$82,$2a
 db $18,$00,$00
 db $90,$00,$00,$81,$ee
 db $18,$01,$01
 db $81,$04,$e0,$f7,$80,$f7
 db $97,$03,$e2,$2a,$01
 db $90,$c1,$00,$e0,$f7,$80,$f7
 db $98,$03,$e2,$2a,$01
 db $90,$c1,$16,$e1,$ee,$81,$ee
 db $18,$01,$01
 db $90,$14,$e1,$ee,$81,$ee
 db $18,$01,$01
 db $90,$00,$e2,$e4,$81,$6e
 db $18,$00,$00
 db $90,$14,$e1,$72,$81,$72
 db $18,$01,$01
 db $94,$06,$e2,$e4,$81,$5d
 db $18,$00,$00
 db $90,$00,$00,$82,$e4
 db $98,$03,$f1,$72,$01
 db $81,$c1,$0a,$00,$81,$72
 db $93,$03,$f1,$5d,$01
 db $94,$c1,$06,$f1,$72,$81,$72
 db $98,$03,$f1,$5d,$01
 db $90,$c1,$16,$f2,$e4,$81,$6e
 db $18,$00,$00
 db $90,$00,$00,$82,$e4
 db $18,$01,$01
 db $90,$00,$f2,$e4,$81,$5d
 db $18,$00,$00
 db $90,$14,$f1,$72,$81,$72
 db $98,$03,$e1,$72,$01
 db $94,$c1,$06,$00,$81,$72
 db $98,$03,$e1,$5d,$01
 db $90,$c1,$00,$e2,$e4,$82,$e4
 db $98,$03,$e1,$5d,$01
 db $81,$c1,$04,$e2,$e4,$81,$6e
 db $17,$00,$00
 db $90,$00,$e1,$72,$81,$72
 db $18,$01,$01
 db $90,$16,$e2,$e4,$81,$5d
 db $18,$00,$00
 db $90,$14,$00,$82,$e4
 db $98,$03,$e1,$72,$01
 db $90,$c1,$00,$e2,$2a,$81,$b8
 db $18,$00,$00
 db $90,$14,$e1,$15,$81,$15
 db $98,$03,$d1,$5d,$01
 db $94,$c1,$06,$d2,$2a,$81,$9f
 db $18,$00,$00
 db $90,$00,$00,$82,$2a
 db $98,$03,$d1,$b8,$01
 db $81,$c1,$0a,$d2,$2a,$81,$b8
 db $13,$00,$00
 db $94,$06,$00,$00
 db $18,$00,$00
 db $90,$16,$00,$81,$ee
 db $18,$00,$00
 db $90,$00,$00,$82,$2a
 db $98,$03,$d1,$b8,$01
 db $90,$c1,$00,$d2,$2a,$82,$26
 db $18,$00,$00
 db $90,$14,$00,$00
 db $18,$00,$00
 db $94,$06,$00,$82,$2a
 db $98,$03,$00,$01
 db $90,$c1,$00,$00,$82,$2a
 db $98,$03,$00,$01
 db $81,$c1,$04,$00,$82,$e4
 db $17,$00,$00
 db $90,$00,$00,$82,$bb
 db $18,$00,$00
 db $90,$16,$00,$82,$4b
 db $18,$00,$00
 db $90,$14,$00,$82,$2a
 db $98,$03,$d2,$bb,$01
 db $90,$c1,$00,$d1,$b8,$82,$2a
 db $18,$00,$00
 db $90,$14,$d0,$dc,$80,$dc
 db $98,$03,$c2,$4b,$01
 db $94,$c1,$06,$c1,$b8,$81,$b8
 db $98,$03,$c2,$2a,$01
 db $90,$c1,$00,$c1,$b8,$81,$b8
 db $98,$03,$c2,$2a,$01
 db $81,$c1,$0a,$c1,$b8,$81,$9f
 db $13,$00,$00
 db $94,$06,$00,$00
 db $18,$00,$00
 db $90,$16,$00,$81,$b4
 db $18,$00,$00
 db $90,$00,$00,$81,$b8
 db $98,$03,$c1,$9f,$01
 db $90,$c1,$00,$c0,$dc,$80,$dc
 db $98,$03,$c1,$b8,$01
 db $90,$c1,$14,$c0,$dc,$80,$dc
 db $98,$03,$c1,$b8,$01
 db $94,$c1,$06,$00,$81,$9f
 db $18,$00,$00
 db $90,$00,$00,$00
 db $18,$00,$00
 db $81,$04,$00,$81,$b4
 db $17,$00,$00
 db $90,$00,$c0,$dc,$80,$dc
 db $98,$03,$c1,$9f,$01
 db $90,$c1,$16,$c1,$b8,$82,$e4
 db $18,$00,$00
 db $90,$14,$00,$82,$bb
 db $18,$00,$00
 db $90,$00,$c1,$ee,$82,$4b
 db $18,$00,$00
 db $90,$14,$c0,$f7,$80,$f7
 db $98,$03,$b2,$bb,$01
 db $94,$c1,$06,$b1,$ee,$81,$ee
 db $98,$03,$b2,$4b,$01
 db $90,$c1,$00,$b1,$ee,$81,$ee
 db $98,$03,$b2,$4b,$01
 db $81,$c1,$0a,$b1,$ee,$81,$72
 db $13,$00,$00
 db $94,$06,$00,$00
 db $18,$00,$00
 db $90,$16,$00,$82,$4b
 db $18,$00,$00
 db $90,$00,$00,$00
 db $18,$00,$00
 db $90,$00,$b0,$f7,$80,$f7
 db $98,$03,$b2,$4b,$01
 db $90,$c1,$14,$b0,$f7,$80,$f7
 db $98,$03,$b2,$4b,$01
 db $94,$c1,$06,$b1,$ee,$82,$2a
 db $18,$00,$00
 db $90,$00,$00,$00
 db $18,$00,$00
 db $81,$04,$b0,$f7,$80,$f7
 db $97,$03,$b2,$2a,$01
 db $90,$c1,$00,$b0,$f7,$80,$f7
 db $98,$03,$b2,$2a,$01
 db $90,$c1,$16,$b1,$ee,$81,$ee
 db $18,$01,$01
 db $90,$14,$b1,$ee,$81,$ee
 db $18,$01,$01
 db $90,$00,$b2,$2a,$82,$4b
 db $18,$00,$00
 db $90,$14,$b1,$15,$81,$15
 db $18,$01,$01
 db $94,$06,$b2,$2a,$82,$26
 db $18,$00,$00
 db $90,$00,$00,$82,$2a
 db $98,$03,$a2,$4b,$01
 db $81,$c1,$0a,$a2,$2a,$82,$4b
 db $13,$00,$00
 db $94,$06,$a1,$15,$81,$15
 db $98,$03,$a2,$2a,$01
 db $90,$c1,$16,$00,$82,$26
 db $18,$00,$00
 db $90,$00,$00,$82,$2a
 db $98,$03,$a2,$4b,$01
 db $90,$c1,$00,$a2,$2a,$82,$e4
 db $18,$00,$00
 db $90,$14,$a1,$15,$81,$15
 db $98,$03,$a2,$2a,$01
 db $94,$c1,$06,$00,$82,$bb
 db $18,$00,$00
 db $90,$00,$00,$82,$2a
 db $98,$03,$a2,$e4,$01
 db $81,$c1,$04,$a2,$2a,$82,$e4
 db $17,$00,$00
 db $90,$00,$00,$00
 db $18,$00,$00
 db $90,$16,$00,$82,$bb
 db $18,$00,$00
 db $90,$14,$00,$82,$e4
 db $18,$00,$00
 db $90,$00,$00,$83,$70
 db $18,$00,$00
 db $90,$14,$a1,$15,$81,$15
 db $98,$03,$92,$e4,$01
 db $94,$c1,$06,$92,$2a,$83,$3f
 db $18,$00,$00
 db $90,$00,$00,$82,$2a
 db $98,$03,$83,$70,$01
 db $81,$c1,$0a,$82,$2a,$83,$70
 db $13,$00,$00
 db $94,$06,$81,$15,$81,$15
 db $98,$03,$b3,$3f,$01
 db $90,$c1,$16,$b2,$2a,$83,$dc
 db $18,$00,$00
 db $90,$00,$00,$82,$2a
 db $98,$03,$d3,$70,$01
 db $90,$c1,$00,$e2,$2a,$e4,$53
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$01,$01
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $90,$00,$e0,$b9,$b1,$15
 db $18,$00,$00
 db $18,$00,$91,$15
 db $18,$00,$00
 db $90,$02,$00,$00
 db $18,$00,$00
 db $90,$00,$e1,$72,$92,$2a
 db $18,$00,$00
 db $81,$04,$01,$01
 db $17,$00,$00
 db $90,$00,$e1,$72,$92,$2a
 db $18,$00,$00
 db $18,$01,$01
 db $18,$00,$00
 db $18,$e1,$72,$90,$dc
 db $18,$00,$00
 db $90,$02,$00,$91,$15
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $88,$cb,$18,$e4,$55,$82,$28
 db $98,$03,$00,$01
 db $88,$cb,$18,$00,$92,$26
 db $98,$03,$e3,$3f,$01
 db $81,$cb,$04,$e8,$ab,$a2,$8d
 db $97,$03,$e2,$93,$01
 db $90,$cb,$00,$e8,$ab,$b2,$8b
 db $98,$03,$e4,$55,$01
 db $88,$cb,$18,$00,$c8,$a1
 db $98,$03,$e3,$3f,$01
 db $90,$cb,$00,$e4,$55,$d4,$49
 db $98,$03,$e2,$93,$01
 db $90,$c1,$00,$e0,$b9,$b1,$15
 db $18,$00,$00
 db $18,$00,$91,$15
 db $18,$00,$00
 db $90,$02,$00,$00
 db $18,$00,$00
 db $90,$00,$e1,$72,$92,$2a
 db $18,$00,$00
 db $81,$04,$01,$01
 db $17,$00,$00
 db $90,$00,$e1,$72,$92,$2a
 db $18,$00,$00
 db $18,$01,$01
 db $18,$00,$00
 db $90,$00,$e1,$72,$91,$15
 db $18,$00,$00
 db $90,$02,$e1,$49,$90,$f7
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $88,$18,$e1,$ee,$b1,$45
 db $18,$00,$00
 db $90,$00,$00,$a1,$45
 db $18,$00,$00
 db $88,$18,$00,$b1,$45
 db $18,$00,$00
 db $90,$02,$00,$a1,$45
 db $18,$00,$00
 db $90,$02,$00,$b1,$45
 db $18,$00,$00
 db $90,$02,$00,$c1,$45
 db $18,$00,$00
 db $90,$00,$e0,$b9,$b1,$15
 db $18,$00,$00
 db $18,$00,$91,$15
 db $18,$00,$00
 db $90,$02,$00,$00
 db $18,$00,$00
 db $90,$00,$e1,$72,$92,$2a
 db $18,$00,$00
 db $81,$04,$01,$01
 db $17,$00,$00
 db $90,$00,$e1,$72,$92,$2a
 db $18,$00,$00
 db $18,$01,$01
 db $18,$00,$00
 db $18,$e1,$72,$90,$dc
 db $18,$00,$00
 db $90,$02,$00,$91,$15
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $88,$cb,$18,$e4,$55,$82,$28
 db $98,$03,$00,$01
 db $88,$cb,$18,$00,$92,$26
 db $98,$03,$e3,$3f,$01
 db $81,$cb,$04,$e8,$ab,$a2,$8d
 db $97,$03,$e2,$93,$01
 db $90,$cb,$00,$e8,$ab,$b2,$8b
 db $98,$03,$e4,$55,$01
 db $88,$cb,$18,$00,$c8,$a1
 db $98,$03,$e3,$3f,$01
 db $90,$cb,$00,$e4,$55,$d4,$49
 db $98,$03,$e2,$93,$01
 db $90,$c1,$00,$e0,$b9,$b1,$15
 db $18,$00,$00
 db $18,$00,$91,$15
 db $18,$00,$00
 db $90,$02,$00,$00
 db $18,$00,$00
 db $90,$00,$e1,$72,$92,$2a
 db $18,$00,$00
 db $81,$04,$01,$01
 db $17,$00,$00
 db $90,$00,$e1,$72,$92,$2a
 db $18,$00,$00
 db $18,$01,$01
 db $18,$00,$00
 db $90,$00,$e1,$72,$91,$15
 db $18,$00,$00
 db $90,$02,$e1,$49,$90,$f7
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $90,$00,$e0,$a4,$83,$3d
 db $18,$00,$00
 db $90,$00,$b0,$cf,$00
 db $18,$00,$00
 db $81,$04,$e1,$49,$93,$3b
 db $17,$00,$00
 db $90,$00,$b1,$9f,$00
 db $18,$00,$00
 db $81,$04,$e2,$93,$a3,$39
 db $17,$00,$00
 db $90,$00,$b3,$3f,$00
 db $18,$00,$00
 db $90,$00,$e1,$ee,$b2,$e4
 db $18,$00,$00
 db $18,$b1,$ee,$00
 db $18,$00,$00
 db $94,$06,$b0,$f3,$b0,$7b
 db $18,$00,$00
 db $90,$08,$b1,$ee,$b2,$e4
 db $18,$00,$00
 db $81,$0a,$b0,$f3,$b0,$7b
 db $13,$00,$00
 db $90,$00,$e0,$f7,$b1,$72
 db $18,$01,$01
 db $88,$18,$e0,$f7,$b1,$72
 db $18,$01,$01
 db $88,$18,$b0,$f7,$b1,$72
 db $18,$01,$01
 db $94,$06,$e1,$ee,$b2,$e4
 db $18,$00,$00
 db $90,$08,$b1,$ee,$00
 db $18,$00,$00
 db $88,$18,$b0,$f3,$b0,$7b
 db $18,$00,$00
 db $90,$08,$b1,$ee,$b2,$e4
 db $18,$00,$00
 db $81,$0a,$b0,$f3,$b0,$7b
 db $13,$00,$00
 db $18,$b0,$f7,$80,$f5
 db $18,$01,$01
 db $90,$00,$e0,$f7,$80,$f7
 db $18,$01,$01
 db $18,$b0,$f7,$80,$f7
 db $18,$01,$01
 db $90,$00,$e2,$2a,$b3,$3f
 db $18,$00,$00
 db $18,$b2,$2a,$00
 db $18,$00,$00
 db $94,$06,$b1,$11,$b0,$8a
 db $18,$00,$00
 db $90,$08,$b2,$2a,$b3,$3f
 db $18,$00,$00
 db $81,$0a,$b1,$11,$b0,$8a
 db $13,$00,$00
 db $90,$00,$e1,$15,$b1,$9f
 db $18,$01,$01
 db $88,$18,$e1,$15,$b1,$9f
 db $18,$01,$01
 db $88,$18,$b1,$15,$b1,$9f
 db $18,$01,$01
 db $94,$06,$e2,$2a,$b3,$3f
 db $18,$00,$00
 db $88,$18,$b2,$2a,$00
 db $18,$00,$00
 db $90,$08,$b1,$11,$b0,$8a
 db $18,$00,$00
 db $90,$08,$b2,$2a,$b3,$3f
 db $18,$00,$00
 db $81,$0a,$b1,$11,$b0,$8a
 db $13,$00,$00
 db $18,$b1,$15,$81,$13
 db $18,$01,$01
 db $90,$00,$e1,$15,$81,$15
 db $18,$01,$01
 db $81,$04,$b1,$15,$81,$15
 db $17,$01,$01
 db $90,$00,$e1,$ee,$b2,$e4
 db $18,$00,$00
 db $18,$b1,$ee,$00
 db $18,$00,$00
 db $94,$06,$b0,$f3,$b0,$7b
 db $18,$00,$00
 db $90,$08,$b1,$ee,$b2,$e4
 db $18,$00,$00
 db $81,$0a,$b0,$f3,$b0,$7b
 db $13,$00,$00
 db $90,$00,$e0,$f7,$b1,$72
 db $18,$01,$01
 db $88,$18,$e0,$f7,$b1,$72
 db $18,$01,$01
 db $88,$18,$b0,$f7,$b1,$72
 db $18,$01,$01
 db $94,$06,$e1,$ee,$b2,$e4
 db $18,$00,$00
 db $90,$08,$b1,$ee,$00
 db $18,$00,$00
 db $88,$18,$b0,$f3,$b0,$7b
 db $18,$00,$00
 db $90,$08,$b1,$ee,$b2,$e4
 db $18,$00,$00
 db $81,$0a,$b0,$f3,$b0,$7b
 db $13,$00,$00
 db $18,$b0,$f7,$80,$f5
 db $18,$01,$01
 db $90,$00,$e0,$f7,$80,$f7
 db $18,$01,$01
 db $18,$b0,$f7,$80,$f7
 db $18,$01,$01
 db $81,$0a,$e1,$25,$91,$b6
 db $13,$00,$00
 db $90,$14,$b1,$25,$00
 db $18,$00,$00
 db $81,$0a,$e1,$25,$00
 db $13,$00,$00
 db $90,$08,$b1,$25,$00
 db $18,$00,$00
 db $81,$0a,$e1,$25,$00
 db $13,$00,$00
 db $90,$00,$b1,$25,$00
 db $18,$00,$00
 db $81,$0a,$e1,$25,$00
 db $13,$00,$00
 db $90,$14,$b1,$25,$00
 db $18,$00,$00
 db $88,$18,$e2,$2a,$e3,$3f
 db $98,$03,$88,$ab,$00
 db $88,$c1,$1a,$b2,$2a,$b3,$3f
 db $18,$86,$7e,$00
 db $88,$43,$18,$e2,$2a,$e3,$3f
 db $98,$81,$85,$27,$00
 db $88,$43,$1a,$b2,$2a,$b3,$3f
 db $98,$81,$88,$ab,$00
 db $88,$43,$1c,$e2,$2a,$e3,$3f
 db $98,$81,$86,$7e,$00
 db $88,$43,$1e,$b2,$2a,$b3,$3f
 db $98,$81,$85,$27,$00
 db $88,$43,$1c,$e2,$2a,$e3,$3f
 db $98,$81,$88,$ab,$00
 db $88,$43,$1e,$b2,$2a,$b3,$3f
 db $98,$81,$86,$7e,$00
 db $90,$4d,$20,$90,$b9,$01
 db $18,$01,$00
 db $90,$20,$90,$b9,$00
 db $18,$01,$00
 db $18,$80,$b9,$00
 db $18,$01,$00
 db $18,$90,$b9,$00
 db $18,$01,$00
 db $88,$22,$90,$b9,$94,$97
 db $18,$01,$00
 db $18,$80,$b9,$00
 db $18,$01,$01
 db $18,$90,$b9,$94,$55
 db $18,$84,$97,$00
 db $18,$80,$b9,$00
 db $18,$84,$97,$01
 db $18,$91,$15,$93,$dc
 db $18,$84,$55,$00
 db $18,$91,$15,$00
 db $18,$84,$55,$00
 db $90,$20,$81,$15,$01
 db $18,$83,$dc,$00
 db $90,$20,$91,$15,$00
 db $18,$83,$dc,$00
 db $88,$22,$91,$15,$93,$70
 db $18,$01,$00
 db $18,$81,$15,$00
 db $18,$01,$00
 db $88,$22,$91,$15,$01
 db $18,$83,$70,$00
 db $88,$22,$81,$15,$00
 db $18,$83,$70,$00
 db $90,$24,$a0,$b9,$a3,$3f
 db $18,$01,$00
 db $90,$24,$a0,$b9,$00
 db $18,$01,$00
 db $18,$90,$b9,$01
 db $18,$83,$3f,$00
 db $18,$a0,$b9,$00
 db $18,$83,$3f,$00
 db $88,$26,$a0,$b9,$a2,$e4
 db $18,$01,$00
 db $18,$90,$b9,$01
 db $18,$01,$00
 db $18,$a0,$b9,$a2,$bb
 db $18,$82,$e4,$00
 db $18,$90,$b9,$a2,$e4
 db $18,$82,$e4,$00
 db $18,$a0,$f7,$a2,$bb
 db $18,$82,$bb,$00
 db $18,$a0,$f7,$00
 db $18,$82,$e4,$00
 db $90,$24,$90,$f7,$01
 db $18,$82,$bb,$00
 db $90,$24,$a0,$f7,$00
 db $18,$82,$bb,$00
 db $88,$26,$a0,$f7,$a2,$4b
 db $18,$01,$00
 db $18,$90,$f7,$00
 db $18,$01,$00
 db $88,$26,$a0,$f7,$01
 db $18,$82,$4b,$00
 db $88,$26,$90,$f7,$00
 db $18,$82,$4b,$00
 db $90,$28,$b0,$b9,$b2,$2a
 db $18,$01,$00
 db $90,$28,$b0,$b9,$00
 db $18,$01,$00
 db $18,$a0,$b9,$01
 db $18,$82,$2a,$00
 db $18,$b0,$b9,$00
 db $18,$82,$2a,$00
 db $88,$1e,$b0,$b9,$00
 db $18,$01,$00
 db $18,$a0,$b9,$00
 db $18,$01,$00
 db $18,$b0,$b9,$b2,$4b
 db $18,$01,$00
 db $18,$a0,$b9,$01
 db $18,$01,$00
 db $18,$b1,$15,$b2,$bb
 db $18,$82,$4b,$00
 db $18,$b1,$15,$00
 db $18,$82,$4b,$00
 db $90,$28,$a1,$15,$01
 db $18,$82,$bb,$00
 db $90,$28,$b1,$15,$00
 db $18,$82,$bb,$00
 db $88,$1e,$b1,$15,$b2,$e4
 db $18,$01,$00
 db $18,$a1,$15,$00
 db $18,$01,$00
 db $88,$1e,$b1,$15,$01
 db $18,$82,$e4,$00
 db $88,$1e,$a1,$15,$00
 db $18,$82,$e4,$00
 db $90,$2a,$c0,$b9,$c3,$3f
 db $18,$01,$00
 db $90,$2a,$c0,$b9,$c3,$70
 db $18,$01,$00
 db $18,$b0,$b9,$c3,$3f
 db $18,$83,$3f,$c3,$70
 db $18,$c0,$b9,$c3,$3f
 db $18,$83,$70,$c3,$70
 db $88,$2c,$c0,$b9,$c3,$dc
 db $18,$83,$3f,$00
 db $18,$b0,$b9,$01
 db $18,$83,$3f,$00
 db $18,$c0,$b9,$c4,$55
 db $18,$83,$dc,$00
 db $18,$b0,$b9,$01
 db $18,$83,$dc,$00
 db $18,$c0,$a4,$c4,$97
 db $18,$84,$55,$00
 db $18,$c0,$a4,$00
 db $18,$84,$55,$00
 db $90,$2a,$b0,$a4,$01
 db $18,$84,$97,$00
 db $90,$2a,$c0,$a4,$00
 db $18,$84,$97,$00
 db $88,$2c,$c0,$a4,$00
 db $18,$01,$00
 db $18,$b0,$a4,$00
 db $18,$01,$00
 db $88,$2c,$c0,$a4,$00
 db $18,$01,$00
 db $88,$2c,$b0,$a4,$00
 db $18,$01,$00
 db $90,$2e,$d0,$f7,$00
 db $18,$01,$00
 db $90,$2e,$d0,$f7,$00
 db $18,$01,$00
 db $90,$30,$c0,$f7,$00
 db $18,$01,$00
 db $90,$32,$d0,$f7,$00
 db $18,$01,$00
 db $88,$1c,$d0,$f7,$d4,$97
 db $18,$01,$00
 db $90,$32,$c0,$f7,$00
 db $18,$00,$01
 db $90,$30,$d0,$f7,$d4,$55
 db $18,$84,$97,$00
 db $90,$32,$c0,$f7,$00
 db $18,$84,$97,$01
 db $90,$30,$d0,$f7,$d3,$dc
 db $18,$84,$55,$00
 db $90,$32,$d0,$f7,$00
 db $18,$84,$55,$00
 db $90,$2e,$c0,$f7,$01
 db $18,$83,$dc,$00
 db $90,$2e,$d0,$f7,$00
 db $18,$83,$dc,$00
 db $88,$1c,$d0,$f7,$d3,$70
 db $18,$01,$00
 db $90,$32,$c0,$f7,$00
 db $18,$01,$00
 db $88,$1c,$d0,$f7,$01
 db $18,$83,$70,$00
 db $88,$1c,$c0,$f7,$00
 db $18,$83,$70,$00
 db $90,$34,$e1,$15,$e3,$3f
 db $18,$01,$00
 db $90,$34,$e1,$15,$01
 db $18,$01,$00
 db $90,$36,$d1,$15,$e2,$e4
 db $18,$83,$3f,$00
 db $90,$38,$e1,$15,$01
 db $18,$83,$3f,$00
 db $88,$3a,$e1,$15,$00
 db $18,$82,$e4,$00
 db $90,$38,$d1,$15,$00
 db $18,$82,$e4,$00
 db $90,$36,$e1,$15,$e2,$bb
 db $18,$01,$00
 db $90,$38,$d1,$15,$00
 db $18,$01,$00
 db $90,$36,$e1,$15,$01
 db $18,$82,$bb,$00
 db $90,$38,$e1,$15,$00
 db $18,$82,$bb,$00
 db $90,$34,$d1,$15,$e2,$4b
 db $18,$01,$00
 db $90,$34,$e1,$15,$00
 db $18,$01,$00
 db $88,$3a,$e1,$15,$01
 db $18,$82,$4b,$00
 db $90,$38,$d1,$15,$00
 db $18,$82,$4b,$00
 db $88,$3a,$e1,$15,$00
 db $18,$01,$00
 db $88,$3a,$d1,$15,$00
 db $18,$01,$00
 db $90,$3c,$f0,$f7,$00
 db $18,$01,$00
 db $90,$3c,$f0,$f7,$00
 db $18,$01,$00
 db $90,$3e,$e0,$f7,$00
 db $18,$01,$00
 db $90,$40,$f0,$f7,$00
 db $18,$01,$00
 db $88,$1a,$f0,$f7,$f2,$4b
 db $18,$01,$00
 db $90,$40,$e0,$f7,$f2,$2a
 db $18,$01,$00
 db $90,$3e,$f0,$f7,$f2,$4b
 db $18,$82,$4b,$00
 db $90,$40,$e0,$f7,$01
 db $18,$82,$4b,$00
 db $90,$3e,$f0,$f7,$f2,$bb
 db $18,$82,$4b,$00
 db $90,$40,$f0,$f7,$00
 db $18,$82,$4b,$00
 db $90,$3c,$e0,$f7,$01
 db $18,$82,$bb,$00
 db $90,$3c,$f0,$f7,$00
 db $18,$82,$bb,$00
 db $88,$1a,$f0,$f7,$f2,$e4
 db $18,$01,$00
 db $90,$40,$e0,$f7,$00
 db $18,$01,$00
 db $88,$1a,$f0,$f7,$f3,$3f
 db $18,$82,$e4,$f3,$70
 db $88,$1a,$e0,$f7,$f3,$dc
 db $18,$82,$e4,$f4,$55
 db $81,$42,$f1,$25,$00
 db $17,$83,$3f,$00
 db $90,$44,$f1,$25,$00
 db $18,$83,$dc,$00
 db $81,$42,$e1,$25,$00
 db $17,$84,$55,$00
 db $90,$46,$f1,$25,$00
 db $18,$84,$55,$00
 db $81,$42,$f1,$25,$f3,$dc
 db $17,$01,$00
 db $90,$46,$e1,$25,$00
 db $18,$01,$00
 db $81,$42,$f1,$25,$f4,$55
 db $17,$83,$dc,$00
 db $81,$42,$e1,$25,$00
 db $17,$83,$dc,$00
 db $81,$c1,$42,$e2,$2a,$f8,$ab
 db $97,$03,$88,$ab,$01
 db $98,$81,$b2,$2a,$f8,$ab
 db $18,$86,$7e,$f6,$7e
 db $98,$03,$e2,$2a,$f8,$ab
 db $98,$81,$85,$27,$f5,$27
 db $98,$03,$b2,$2a,$f8,$ab
 db $98,$81,$88,$ab,$00
 db $98,$03,$e2,$2a,$00
 db $98,$81,$86,$7e,$f6,$7e
 db $98,$03,$b2,$2a,$f8,$ab
 db $98,$81,$85,$27,$f5,$27
 db $98,$03,$e2,$2a,$f8,$ab
 db $98,$81,$88,$ab,$00
 db $98,$03,$b2,$2a,$00
 db $98,$81,$86,$7e,$f6,$7e
 db $90,$00,$e2,$e0,$b2,$e2
 db $18,$00,$00
 db $18,$e0,$b9,$b0,$b9
 db $18,$01,$01
 db $98,$03,$e8,$ab,$b0,$b9
 db $18,$f2,$e4,$01
 db $18,$f6,$7e,$b0,$b9
 db $18,$f2,$e4,$01
 db $81,$c1,$04,$00,$b2,$bb
 db $17,$00,$00
 db $18,$f0,$b9,$b0,$b9
 db $18,$01,$01
 db $98,$03,$f8,$ab,$b0,$b9
 db $18,$f2,$bb,$01
 db $18,$f6,$7e,$b0,$b9
 db $18,$f2,$bb,$01
 db $98,$81,$f2,$e4,$b2,$4b
 db $18,$00,$00
 db $18,$f0,$b9,$b0,$b9
 db $18,$01,$01
 db $90,$43,$00,$f8,$ab,$b0,$b9
 db $18,$f2,$4b,$01
 db $90,$00,$f6,$7e,$b0,$b9
 db $18,$f2,$4b,$01
 db $81,$c1,$04,$f2,$e4,$b2,$2a
 db $17,$00,$00
 db $98,$03,$f0,$b9,$01
 db $18,$01,$00
 db $18,$f8,$ab,$b0,$b9
 db $18,$f2,$2a,$01
 db $18,$f6,$7e,$b0,$b9
 db $18,$f2,$2a,$01
 db $98,$81,$e0,$b5,$b0,$b5
 db $18,$01,$01
 db $18,$e0,$b9,$b0,$b9
 db $18,$01,$01
 db $90,$00,$e2,$e4,$82,$d6
 db $18,$00,$00
 db $98,$03,$e6,$7e,$80,$b9
 db $18,$01,$01
 db $81,$c1,$04,$e2,$e4,$82,$bb
 db $17,$00,$00
 db $18,$e0,$b9,$80,$b9
 db $98,$03,$e2,$e4,$01
 db $98,$81,$00,$82,$4b
 db $18,$00,$00
 db $98,$03,$e6,$7e,$80,$b9
 db $18,$e2,$bb,$01
 db $98,$81,$e0,$b9,$80,$b9
 db $98,$03,$e2,$4b,$01
 db $98,$81,$e0,$b9,$80,$b9
 db $98,$03,$e2,$4b,$01
 db $90,$c1,$00,$e2,$e4,$82,$2a
 db $98,$03,$00,$00
 db $90,$00,$e6,$7e,$80,$b9
 db $18,$01,$01
 db $81,$c1,$04,$e0,$b9,$80,$b9
 db $97,$03,$e2,$2a,$01
 db $98,$81,$e0,$b9,$80,$b9
 db $98,$03,$e2,$2a,$01
 db $81,$04,$e8,$ab,$80,$b9
 db $17,$01,$01
 db $81,$04,$e6,$7e,$80,$b9
 db $17,$01,$01
 db $90,$c1,$00,$e4,$51,$82,$26
 db $18,$00,$00
 db $18,$e1,$15,$81,$15
 db $18,$01,$01
 db $98,$03,$e6,$7e,$81,$15
 db $18,$d2,$2a,$01
 db $98,$81,$d4,$55,$82,$bb
 db $18,$00,$82,$4b
 db $81,$04,$00,$82,$bb
 db $17,$00,$00
 db $18,$d1,$15,$81,$15
 db $98,$03,$d2,$4b,$01
 db $98,$81,$d4,$55,$82,$e4
 db $18,$00,$00
 db $98,$03,$d5,$76,$81,$15
 db $18,$d2,$bb,$01
 db $98,$81,$d4,$55,$82,$bb
 db $18,$00,$00
 db $18,$d1,$15,$81,$15
 db $98,$03,$d2,$e4,$01
 db $90,$c1,$00,$d4,$55,$82,$4b
 db $18,$00,$00
 db $90,$43,$00,$d5,$76,$81,$15
 db $18,$d2,$bb,$01
 db $81,$c1,$04,$d4,$55,$82,$26
 db $17,$00,$00
 db $18,$d1,$15,$81,$15
 db $98,$03,$d2,$4b,$01
 db $98,$81,$d4,$55,$81,$ee
 db $18,$00,$00
 db $18,$d5,$76,$81,$15
 db $98,$03,$d2,$2a,$01
 db $98,$81,$d3,$dc,$82,$4b
 db $18,$00,$00
 db $18,$d0,$f7,$80,$f7
 db $98,$03,$c3,$dc,$01
 db $90,$00,$c5,$76,$80,$f7
 db $18,$c2,$4b,$01
 db $18,$c4,$97,$80,$f7
 db $18,$c2,$4b,$01
 db $81,$c1,$04,$c3,$dc,$81,$9f
 db $17,$00,$00
 db $18,$c0,$f7,$80,$f7
 db $18,$01,$01
 db $18,$c3,$dc,$81,$ea
 db $18,$00,$00
 db $98,$03,$c4,$97,$80,$f7
 db $18,$c1,$9f,$01
 db $18,$f2,$4b,$80,$f7
 db $18,$f2,$e4,$01
 db $18,$e3,$3f,$80,$f7
 db $18,$e2,$4b,$01
 db $90,$00,$d2,$e4,$80,$f7
 db $18,$d3,$3f,$01
 db $90,$00,$c2,$4b,$80,$f7
 db $18,$c2,$e4,$01
 db $81,$04,$b3,$3f,$80,$f7
 db $17,$b2,$4b,$01
 db $18,$a2,$e4,$80,$f7
 db $18,$a3,$3f,$01
 db $81,$04,$92,$4b,$80,$f7
 db $17,$92,$e4,$01
 db $81,$04,$83,$3f,$80,$f7
 db $17,$82,$4b,$01
 db $90,$c1,$00,$e0,$b5,$80,$b5
 db $18,$01,$01
 db $18,$e0,$b9,$80,$b9
 db $18,$01,$01
 db $98,$03,$e8,$ab,$80,$b9
 db $18,$01,$01
 db $18,$e6,$7e,$80,$b9
 db $18,$01,$01
 db $81,$c1,$04,$e2,$e4,$82,$bb
 db $17,$00,$00
 db $18,$00,$82,$e0
 db $18,$00,$00
 db $18,$00,$82,$bb
 db $18,$00,$00
 db $98,$03,$e6,$7e,$80,$b9
 db $18,$b2,$e4,$01
 db $98,$81,$00,$82,$4b
 db $18,$00,$00
 db $18,$b0,$b9,$80,$b9
 db $18,$01,$01
 db $90,$43,$00,$b8,$ab,$80,$b9
 db $18,$b2,$4b,$01
 db $90,$c1,$00,$b6,$7e,$80,$b9
 db $98,$03,$b2,$4b,$01
 db $81,$c1,$04,$b0,$b9,$80,$b9
 db $17,$01,$01
 db $18,$b0,$b9,$80,$b9
 db $18,$01,$01
 db $18,$b2,$e4,$82,$2a
 db $18,$00,$00
 db $98,$03,$b6,$7e,$80,$b9
 db $18,$01,$01
 db $98,$81,$b2,$e4,$82,$e0
 db $18,$00,$00
 db $18,$b0,$b9,$80,$b9
 db $98,$03,$a2,$2a,$01
 db $90,$00,$a8,$ab,$80,$b9
 db $18,$a2,$e4,$01
 db $18,$a6,$7e,$80,$b9
 db $18,$a2,$e4,$01
 db $81,$c1,$04,$00,$82,$bb
 db $17,$00,$00
 db $18,$a0,$b9,$80,$b9
 db $18,$01,$01
 db $18,$a2,$e4,$82,$4b
 db $18,$00,$00
 db $98,$03,$a6,$7e,$80,$b9
 db $18,$a2,$bb,$01
 db $98,$81,$a0,$b9,$80,$b9
 db $98,$03,$a2,$4b,$01
 db $98,$81,$a0,$b9,$80,$b9
 db $98,$03,$a2,$4b,$01
 db $90,$c1,$00,$a2,$e4,$82,$2a
 db $18,$00,$00
 db $90,$43,$00,$a6,$7e,$80,$b9
 db $18,$01,$01
 db $81,$c1,$04,$a0,$b9,$80,$b9
 db $97,$03,$a2,$2a,$01
 db $98,$81,$a0,$b9,$80,$b9
 db $98,$03,$a2,$2a,$01
 db $81,$04,$a8,$ab,$80,$b9
 db $17,$01,$01
 db $81,$04,$a6,$7e,$80,$b9
 db $17,$01,$01
 db $90,$c1,$00,$e1,$11,$81,$11
 db $18,$01,$01
 db $18,$e1,$15,$81,$15
 db $18,$01,$01
 db $98,$03,$e6,$7e,$81,$15
 db $18,$01,$01
 db $18,$e5,$76,$81,$15
 db $18,$01,$01
 db $81,$c1,$04,$e2,$2a,$81,$9f
 db $17,$00,$00
 db $18,$e1,$15,$81,$15
 db $18,$01,$01
 db $18,$e2,$2a,$81,$b8
 db $18,$00,$00
 db $98,$03,$e5,$76,$81,$15
 db $18,$b1,$9f,$01
 db $98,$81,$b2,$2a,$81,$ee
 db $18,$00,$00
 db $18,$b1,$15,$81,$15
 db $98,$03,$b1,$b8,$01
 db $90,$c1,$00,$b2,$2a,$82,$26
 db $18,$00,$00
 db $90,$43,$00,$b5,$76,$81,$15
 db $18,$b1,$ee,$01
 db $81,$c1,$04,$b2,$2a,$82,$4b
 db $17,$00,$00
 db $18,$b1,$15,$81,$15
 db $98,$03,$b2,$2a,$01
 db $98,$81,$00,$82,$4b
 db $18,$00,$00
 db $18,$00,$82,$bb
 db $18,$00,$00
 db $18,$b1,$ee,$82,$4b
 db $18,$00,$00
 db $18,$b0,$f7,$80,$f7
 db $98,$03,$c2,$bb,$01
 db $90,$00,$c5,$76,$80,$f7
 db $18,$c2,$4b,$01
 db $18,$c4,$97,$80,$f7
 db $18,$c2,$4b,$01
 db $81,$c1,$04,$c1,$ee,$82,$bb
 db $17,$00,$00
 db $18,$c0,$f7,$80,$f7
 db $18,$01,$01
 db $18,$c1,$ee,$82,$e4
 db $18,$00,$00
 db $98,$03,$c4,$97,$80,$f7
 db $18,$c2,$bb,$01
 db $18,$f2,$4b,$80,$f7
 db $18,$f2,$e4,$01
 db $18,$e3,$3f,$80,$f7
 db $18,$e2,$4b,$01
 db $90,$00,$d2,$e4,$80,$f7
 db $18,$d3,$3f,$01
 db $90,$00,$c2,$4b,$80,$f7
 db $18,$c2,$e4,$01
 db $81,$c1,$04,$c1,$ee,$82,$e4
 db $17,$00,$00
 db $81,$48,$00,$82,$bb
 db $17,$00,$00
 db $81,$04,$00,$82,$4b
 db $17,$00,$00
 db $81,$48,$c0,$f7,$80,$f7
 db $97,$03,$c2,$bb,$01
 db $90,$c1,$00,$c1,$b8,$82,$2a
 db $18,$00,$00
 db $90,$14,$c0,$dc,$80,$dc
 db $98,$03,$d2,$4b,$01
 db $88,$c1,$18,$d1,$b8,$88,$ab
 db $98,$03,$d2,$2a,$01
 db $90,$c1,$00,$d1,$b8,$88,$ab
 db $98,$03,$d2,$2a,$01
 db $81,$c1,$0a,$d1,$b8,$81,$9f
 db $13,$00,$00
 db $88,$18,$d0,$dc,$80,$dc
 db $18,$01,$01
 db $90,$16,$d1,$b8,$81,$b4
 db $18,$00,$00
 db $90,$00,$00,$88,$ab
 db $98,$03,$d1,$9f,$01
 db $90,$c1,$00,$d0,$dc,$80,$dc
 db $98,$03,$d1,$b8,$01
 db $90,$c1,$14,$d0,$dc,$80,$dc
 db $98,$03,$d1,$b8,$01
 db $88,$c1,$18,$00,$88,$ab
 db $18,$01,$01
 db $90,$00,$d1,$b8,$88,$ab
 db $18,$01,$01
 db $81,$04,$d0,$dc,$80,$dc
 db $17,$01,$01
 db $90,$00,$d0,$dc,$80,$dc
 db $18,$01,$01
 db $90,$16,$d1,$b8,$82,$e4
 db $18,$00,$00
 db $90,$14,$00,$82,$bb
 db $18,$00,$00
 db $90,$00,$d1,$ee,$82,$4b
 db $18,$00,$00
 db $90,$14,$d0,$f7,$80,$f7
 db $98,$03,$e2,$bb,$01
 db $88,$c1,$18,$e1,$ee,$84,$97
 db $98,$03,$e2,$4b,$01
 db $90,$c1,$00,$e1,$ee,$84,$97
 db $98,$03,$e2,$4b,$01
 db $81,$c1,$0a,$e1,$ee,$81,$72
 db $13,$00,$00
 db $88,$18,$00,$00
 db $18,$00,$00
 db $90,$16,$00,$82,$4b
 db $18,$00,$00
 db $90,$00,$00,$84,$97
 db $98,$03,$e1,$72,$01
 db $90,$c1,$00,$e0,$f7,$80,$f7
 db $98,$03,$e2,$4b,$01
 db $90,$c1,$14,$e0,$f7,$80,$f7
 db $98,$03,$e2,$4b,$01
 db $94,$c1,$06,$e1,$ee,$82,$2a
 db $18,$00,$00
 db $90,$00,$00,$84,$97
 db $18,$01,$01
 db $81,$04,$e0,$f7,$80,$f7
 db $97,$03,$e2,$2a,$01
 db $90,$c1,$00,$e0,$f7,$80,$f7
 db $98,$03,$e2,$2a,$01
 db $88,$c1,$18,$e1,$ee,$84,$97
 db $18,$01,$01
 db $88,$18,$e1,$ee,$84,$97
 db $18,$01,$01
 db $90,$00,$e2,$e4,$81,$6e
 db $18,$00,$00
 db $90,$14,$e1,$72,$81,$72
 db $18,$01,$01
 db $88,$18,$e2,$e4,$81,$5d
 db $18,$00,$00
 db $90,$00,$00,$86,$e1
 db $98,$03,$f1,$72,$01
 db $81,$c1,$0a,$00,$81,$72
 db $93,$03,$f1,$5d,$01
 db $88,$c1,$18,$f1,$72,$81,$72
 db $98,$03,$f1,$5d,$01
 db $90,$c1,$16,$f2,$e4,$81,$6e
 db $18,$00,$00
 db $90,$00,$00,$86,$e1
 db $18,$01,$01
 db $90,$00,$f2,$e4,$81,$5d
 db $18,$00,$00
 db $90,$14,$f1,$72,$81,$72
 db $98,$03,$e1,$72,$01
 db $88,$c1,$18,$00,$86,$e1
 db $98,$03,$e1,$5d,$01
 db $90,$c1,$00,$e2,$e4,$86,$e1
 db $98,$03,$e1,$5d,$01
 db $81,$c1,$04,$e2,$e4,$81,$6e
 db $17,$00,$00
 db $90,$00,$e1,$72,$81,$72
 db $18,$01,$01
 db $90,$16,$e2,$e4,$81,$5d
 db $18,$00,$00
 db $90,$14,$00,$86,$e1
 db $98,$03,$e1,$72,$01
 db $90,$c1,$00,$e2,$2a,$81,$b8
 db $18,$00,$00
 db $90,$14,$e1,$15,$81,$15
 db $98,$03,$d1,$5d,$01
 db $88,$c1,$18,$d2,$2a,$81,$9f
 db $18,$00,$00
 db $90,$00,$00,$85,$27
 db $98,$03,$d1,$b8,$01
 db $81,$c1,$0a,$d2,$2a,$81,$b8
 db $13,$00,$00
 db $88,$18,$00,$00
 db $18,$00,$00
 db $90,$16,$00,$81,$ee
 db $18,$00,$00
 db $90,$00,$00,$85,$27
 db $98,$03,$d1,$b8,$01
 db $90,$c1,$00,$d2,$2a,$82,$26
 db $18,$00,$00
 db $90,$14,$00,$00
 db $18,$00,$00
 db $94,$06,$00,$85,$27
 db $98,$03,$00,$01
 db $90,$c1,$00,$00,$85,$27
 db $98,$03,$00,$01
 db $81,$c1,$04,$00,$82,$e4
 db $17,$00,$00
 db $90,$00,$00,$82,$bb
 db $18,$00,$00
 db $88,$18,$00,$82,$4b
 db $18,$00,$00
 db $88,$18,$00,$85,$27
 db $98,$03,$d2,$bb,$01
 db $90,$c1,$00,$d1,$b8,$82,$2a
 db $18,$00,$00
 db $90,$14,$d0,$dc,$80,$dc
 db $98,$03,$c2,$4b,$01
 db $88,$c1,$18,$c1,$b8,$84,$55
 db $98,$03,$c2,$2a,$01
 db $90,$c1,$00,$c1,$b8,$84,$55
 db $98,$03,$c2,$2a,$01
 db $81,$c1,$0a,$c1,$b8,$81,$9f
 db $13,$00,$00
 db $88,$18,$00,$00
 db $18,$00,$00
 db $90,$16,$00,$81,$b4
 db $18,$00,$00
 db $90,$00,$00,$84,$55
 db $98,$03,$c1,$9f,$01
 db $90,$c1,$00,$c0,$dc,$80,$dc
 db $98,$03,$c1,$b8,$01
 db $90,$c1,$14,$c0,$dc,$80,$dc
 db $98,$03,$c1,$b8,$01
 db $88,$c1,$18,$00,$81,$9f
 db $18,$00,$00
 db $90,$00,$00,$00
 db $18,$00,$00
 db $81,$04,$00,$81,$b4
 db $17,$00,$00
 db $90,$00,$c0,$dc,$80,$dc
 db $98,$03,$c1,$9f,$01
 db $90,$c1,$16,$c1,$b8,$82,$e4
 db $18,$00,$00
 db $90,$14,$00,$82,$bb
 db $18,$00,$00
 db $90,$00,$c1,$ee,$82,$4b
 db $18,$00,$00
 db $90,$14,$c0,$f7,$80,$f7
 db $98,$03,$b2,$bb,$01
 db $88,$c1,$18,$b1,$ee,$84,$97
 db $98,$03,$b2,$4b,$01
 db $90,$c1,$00,$b1,$ee,$84,$97
 db $98,$03,$b2,$4b,$01
 db $81,$c1,$0a,$b1,$ee,$81,$72
 db $13,$00,$00
 db $88,$18,$00,$00
 db $18,$00,$00
 db $90,$16,$00,$82,$4b
 db $18,$00,$00
 db $90,$00,$00,$00
 db $18,$00,$00
 db $90,$00,$b0,$f7,$80,$f7
 db $98,$03,$b2,$4b,$01
 db $90,$c1,$14,$b0,$f7,$80,$f7
 db $98,$03,$b2,$4b,$01
 db $94,$c1,$06,$b1,$ee,$82,$2a
 db $18,$00,$00
 db $90,$00,$00,$00
 db $18,$00,$00
 db $81,$04,$b0,$f7,$80,$f7
 db $97,$03,$b2,$2a,$01
 db $90,$c1,$00,$b0,$f7,$80,$f7
 db $98,$03,$b2,$2a,$01
 db $88,$c1,$18,$b1,$ee,$84,$97
 db $18,$01,$01
 db $88,$18,$b1,$ee,$84,$97
 db $18,$01,$01
 db $90,$00,$b2,$2a,$82,$4b
 db $18,$00,$00
 db $90,$14,$b1,$15,$81,$15
 db $18,$01,$01
 db $88,$18,$b2,$2a,$82,$26
 db $18,$00,$00
 db $90,$00,$00,$82,$2a
 db $98,$03,$a2,$4b,$01
 db $81,$c1,$0a,$a2,$2a,$82,$4b
 db $13,$00,$00
 db $88,$18,$a1,$15,$81,$15
 db $98,$03,$a2,$2a,$01
 db $90,$c1,$16,$00,$82,$26
 db $18,$00,$00
 db $90,$00,$00,$82,$2a
 db $98,$03,$a2,$4b,$01
 db $90,$c1,$00,$a2,$2a,$82,$e4
 db $18,$00,$00
 db $90,$14,$a1,$15,$81,$15
 db $98,$03,$a2,$2a,$01
 db $88,$c1,$18,$00,$82,$bb
 db $18,$00,$00
 db $90,$00,$00,$82,$2a
 db $98,$03,$a2,$e4,$01
 db $81,$c1,$04,$a2,$2a,$82,$e4
 db $17,$00,$00
 db $90,$00,$00,$00
 db $18,$00,$00
 db $90,$16,$00,$82,$bb
 db $18,$00,$00
 db $90,$14,$00,$82,$e4
 db $18,$00,$00
 db $90,$00,$00,$83,$70
 db $18,$00,$00
 db $90,$14,$a1,$15,$81,$15
 db $98,$03,$92,$e4,$01
 db $88,$c1,$18,$92,$2a,$83,$3f
 db $18,$00,$00
 db $90,$00,$00,$82,$2a
 db $98,$03,$83,$70,$01
 db $81,$c1,$0a,$82,$2a,$83,$70
 db $13,$00,$00
 db $88,$18,$81,$15,$81,$15
 db $98,$03,$b3,$3f,$01
 db $90,$c1,$16,$b2,$2a,$83,$dc
 db $18,$00,$00
 db $90,$00,$00,$82,$2a
 db $98,$03,$d3,$70,$01
 db $90,$c1,$00,$d2,$2a,$84,$51
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $18,$00,$00
 db $81,$43,$4a,$f4,$55,$01
 db $17,$f3,$3f,$00
 db $81,$4c,$e2,$93,$00
 db $17,$e4,$55,$00
 db $81,$4e,$d3,$3f,$00
 db $17,$d2,$93,$00
 db $81,$50,$c4,$55,$00
 db $17,$c3,$3f,$00
 db $90,$c1,$44,$c1,$72,$b2,$2a
 db $18,$00,$00
 db $90,$00,$00,$92,$2a
 db $18,$00,$00
 db $90,$02,$00,$00
 db $18,$00,$00
 db $90,$00,$c2,$e4,$94,$55
 db $18,$00,$00
 db $81,$04,$01,$01
 db $17,$00,$00
 db $90,$00,$c2,$e4,$94,$55
 db $18,$00,$00
 db $90,$02,$01,$01
 db $18,$00,$00
 db $90,$00,$c1,$72,$90,$dc
 db $18,$00,$00
 db $90,$00,$00,$91,$13
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
.loop
 db $18,$01,$01
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
 db $00
 dw .loop
 align 2
.drumpar
.dp0
 dw .dsmp0+0
 db $02,$09,$40
.dp1
 dw .dsmp2+0
 db $02,$09,$40
.dp2
 dw .dsmp1+0
 db $06,$09,$40
.dp3
 dw .dsmp5+0
 db $01,$09,$40
.dp4
 dw .dsmp3+0
 db $02,$09,$40
.dp5
 dw .dsmp4+0
 db $07,$09,$40
.dp6
 dw .dsmp6+0
 db $06,$09,$40
.dp7
 dw .dsmp6+0
 db $06,$09,$30
.dp8
 dw .dsmp6+0
 db $06,$09,$20
.dp9
 dw .dsmp6+0
 db $06,$09,$10
.dp10
 dw .dsmp2+0
 db $02,$03,$40
.dp11
 dw .dsmp2+0
 db $02,$06,$40
.dp12
 dw .dsmp7+0
 db $04,$09,$40
.dp13
 dw .dsmp7+0
 db $04,$09,$30
.dp14
 dw .dsmp7+0
 db $04,$09,$20
.dp15
 dw .dsmp7+0
 db $04,$09,$10
.dp16
 dw .dsmp0+0
 db $02,$09,$00
.dp17
 dw .dsmp7+0
 db $04,$09,$00
.dp18
 dw .dsmp0+0
 db $02,$09,$08
.dp19
 dw .dsmp7+0
 db $04,$09,$08
.dp20
 dw .dsmp0+0
 db $02,$09,$10
.dp21
 dw .dsmp0+0
 db $02,$09,$18
.dp22
 dw .dsmp7+0
 db $04,$09,$18
.dp23
 dw .dsmp0+0
 db $02,$09,$20
.dp24
 dw .dsmp2+0
 db $02,$06,$20
.dp25
 dw .dsmp2+0
 db $02,$03,$20
.dp26
 dw .dsmp0+0
 db $02,$09,$28
.dp27
 dw .dsmp2+0
 db $02,$06,$28
.dp28
 dw .dsmp2+0
 db $02,$03,$28
.dp29
 dw .dsmp7+0
 db $04,$09,$28
.dp30
 dw .dsmp0+0
 db $02,$09,$30
.dp31
 dw .dsmp2+0
 db $02,$06,$30
.dp32
 dw .dsmp2+0
 db $02,$03,$30
.dp33
 dw .dsmp1+0
 db $06,$09,$38
.dp34
 dw .dsmp0+0
 db $02,$09,$38
.dp35
 dw .dsmp2+0
 db $02,$03,$38
.dp36
 dw .dsmp1+0
 db $06,$09,$28
.dp37
 dw .dsmp1+0
 db $06,$09,$00
.dp38
 dw .dsmp1+0
 db $06,$09,$10
.dp39
 dw .dsmp1+0
 db $06,$09,$20
.dp40
 dw .dsmp1+0
 db $06,$09,$30
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
 db $08,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$dc,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$09,$79,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$fe,$f0,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp2
 db $50,$90,$0c,$6a,$04,$34,$21,$2c,$21,$90,$50,$40,$50,$48,$10,$0a
 db $80,$21,$40,$00,$00,$00,$00,$10,$00,$61,$10,$92,$a4,$00,$a4,$02
 db $04,$04,$24,$00,$02,$00,$40,$00,$40,$01,$01,$00,$48,$00,$21,$48
 db $21,$00,$00,$00,$00,$00,$40,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp3
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$e2,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$7f,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$fc
 db $fc,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp4
 db $00,$00,$00,$00,$00,$9c,$ff,$ff,$ff,$ff,$0f,$00,$00,$00,$00,$00
 db $00,$00,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$00,$00,$00,$00,$00
 db $00,$00,$00,$1f,$ff,$ff,$ff,$ff,$ff,$ff,$80,$00,$00,$00,$00,$00
 db $00,$00,$0f,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$c0,$00,$00,$00,$00
 db $00,$00,$00,$00,$7f,$ff,$ff,$ff,$ff,$ff,$fd,$ff,$8c,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$01,$ff,$ff,$ff,$ff,$ff,$f3,$c0,$00,$00
 db $00,$00,$00,$00,$00,$3a,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$f9,$80,$00
 db $00,$00,$00,$00,$00,$04,$31,$ff,$ff,$ff,$ff,$ff,$fe,$c0,$00,$00
 db $00,$00,$00,$00,$00,$00,$09,$ff,$ff,$ff,$ff,$ff,$fb,$f1,$00,$00
 db $00,$00,$00,$00,$00,$00,$c0,$fe,$ff,$ff,$ff,$ff,$fe,$80,$00,$00
 db $00,$00,$00,$00,$00,$00,$12,$ff,$ff,$ff,$ff,$ff,$80,$03,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$0b,$ff,$f9,$80,$00,$00,$00,$01
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$10,$0f,$0f,$01,$80,$80,$40
 db $30,$18,$00,$00,$00,$e0,$40,$08,$0c,$00,$00,$00,$e0,$10,$3c,$0e
.dsmp5
 db $00,$00,$00,$01,$f0,$00,$f8,$00,$ff,$00,$00,$00,$00,$00,$00,$01
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.dsmp6
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1f,$ff
 db $ff,$ff,$ff,$00,$00,$00,$00,$00,$0f,$ff,$ff,$f8,$00,$00,$00,$00
 db $3f,$ff,$e0,$1f,$00,$0e,$00,$00,$00,$00,$7c,$ff,$ff,$80,$00,$00
 db $07,$c0,$00,$00,$00,$00,$00,$00,$3f,$f8,$70,$00,$00,$00,$00,$00
 db $00,$07,$00,$00,$00,$00,$00,$03,$03,$ff,$f0,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$03,$07,$00,$00,$00,$00,$00,$00,$00,$00,$c0,$fe
 db $00,$00,$c0,$30,$00,$03,$3f,$00,$00,$00,$c7,$00,$00,$00,$e7,$00
 db $00,$00,$00,$00,$00,$03,$00,$00,$8e,$0f,$e0,$00,$0e,$70,$00,$00
 db $00,$00,$00,$00,$ff,$80,$00,$1f,$00,$01,$80,$00,$07,$00,$1c,$e3
 db $00,$70,$00,$00,$38,$00,$73,$e0,$00,$0e,$0e,$e0,$00,$06,$70,$00
 db $00,$07,$f0,$00,$00,$00,$00,$63,$00,$07,$00,$00,$03,$00,$01,$80
.dsmp7
 db $00,$00,$5f,$60,$00,$03,$f1,$00,$00,$7e,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$18,$00,$20,$04,$00,$00,$03,$70,$00,$32,$80,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$00,$00,$00,$30,$c0,$00,$fa,$00,$00,$1f
 db $40,$03,$18,$40,$00,$80,$00,$00,$00,$10,$00,$00,$00,$00,$00,$00
 db $06,$c0,$00,$7e,$00,$01,$80,$00,$00,$04,$00,$03,$f8,$00,$00,$80
 db $01,$c8,$00,$30,$01,$0c,$80,$00,$80,$80,$00,$23,$b8,$00,$21,$01


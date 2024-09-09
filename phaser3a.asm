
 output "Phaser3a.bin"
	org $6000

	;test code

begin

	ld hl,musicData
	call play
	ret



	;engine code

OP_NOP		equ $00
OP_INCBC	equ $03
OP_DECBC	equ $0b
OP_INCSP	equ $33
OP_DECSP	equ $3b
OP_INCDE	equ $13
OP_DECDE	equ $1b
OP_DECHL	equ $2b
OP_ANDH		equ $a4
OP_XORH		equ $ac
OP_ORC		equ $b1
OP_ORH		equ $b4



play

	di
	exx
	push hl
	push iy
	ld (stopPlayer.oldSP),sp
	exx
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld (readRow.pos),de
	ld (readRow.loop),de
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld (readRow.insList0),de
	ld (readRow.insList1),de
	ld (playDrumSample.drumList),hl

	ld hl,0
	ld (soundInit.ch0cnt0),hl
	ld ix,0						;ch0cnt1
	ld (readRow.ch1cnt0),hl
	ld iy,0						;ch1cnt1
	ld (soundInit.ch0add0),hl
	ld sp,hl					;ch0add1
	ld (readRow.ch1add0),hl
	ld (readRow.ch1add1),hl
	xor a
	ld (readRow.ch0int),a
	ld (readRow.ch1int),a
	ld (readRow.ch0det),a
	ld (readRow.ch1det),a
	ld (soundInit.skip),a
	ld (soundLoop.phaseSlideDivA),a
	ld (soundLoop.phaseSlideDivB),a
	ld a,2						; djm
	ld (readRow.ch0pha),a
	ld (readRow.ch1pha),a
	ld a,8
	ld (soundInit.len),a
	exx
	ld hl,0						;ch1cnt0
	ld de,0						;ch1add1
	ld bc,0						;ch1add0
	exx



readRow

.pos=$+1
	ld hl,0

.read

	ld a,(hl)
	inc hl
	cp 245
	jp c,.ch0
	jr z,.setSpeed
	cp 254
	jr z,.setLoop
	cp 255
	jr z,.readLoop
	ld (.pos),hl
	sub 246
	jp playDrumSample

.setSpeed

	ld a,(hl)
	ld (soundInit.len),a
	inc hl
	jp .read

.setLoop

	ld (.loop),hl
	jp .read

.readLoop

.loop=$+1
	ld hl,0
	jp .read


.ch0mute

	ld sp,0
	ld ix,0
	ld (soundInit.ch0cnt0),sp
	ld (soundInit.ch0add0),sp
	jp .ch1

.ch1mute

	exx
	ld hl,0
	ld iy,0
	ld d,h
	ld e,h
	ld b,h
	ld c,h
	exx
	jp .ch1skip

.ch0

	add a,a
	jp nc,.note0			;bit 7 is not set, it is a note
	ex de,hl				;set instrument of channel 0

	ld h,0
	ld l,a
	add hl,hl
	add hl,hl
.insList0=$+1
	ld bc,0
	add hl,bc

	ld a,(hl)
	inc hl
	ld (.ch0int),a
	ld a,(hl)
	inc hl
	ld (.ch0pha),a
	inc a
	jr z,$+4
	ld a,$0a
	xor $0a
	ld (.phasereset0),a

	ld a,(hl)
	ld (.ch0det),a
	inc hl
	ld a,(hl)				;mix mode
	inc hl
	ld (soundLoop.opA0),a
	ld (soundLoop.opA1),a
	ld (soundLoop.opA2),a
	ld (soundLoop.opA3),a

	ld c,(hl)				;volume
	inc hl
	ld b,2					;out mask
	ld a,c
	and b
	ld (soundLoop.volA0),a
	rr c
	ld a,c
	and b
	ld (soundLoop.volA1),a
	rr c
	ld a,c
	and b
	ld (soundLoop.volA2),a
	rr c
	ld a,c
	and b
	ld (soundLoop.volA3),a

	ld a,(hl)				;phase slide speed
	inc hl
	ld (soundLoop.phaseSlideSpeedA),a

	or a
	jr z,$+4
	ld a,soundLoop.phaseSlideA1-soundLoop.phaseSlideA0
	xor soundLoop.phaseSlideA1-soundLoop.phaseSlideA0
	ld (soundLoop.phaseSlideEnableA),a

	ld bc,0
	ld (soundLoop.cntA0Slide+0),bc
	ld (soundLoop.cntA0Slide+2),bc
	ld (soundLoop.cntA1Slide+0),bc
	ld (soundLoop.cntA1Slide+2),bc

	ld a,(hl)				;generator 1 slide
	inc hl
	ld c,a
	or a
	jr z,.ch0noslide0
	and $0f
	or OP_INCBC&$f0
	rl c
	jr nc,$+5
	ld (soundLoop.cntA0Slide+0),a
	rl c
	jr nc,$+5
	ld (soundLoop.cntA0Slide+1),a
	rl c
	jr nc,$+5
	ld (soundLoop.cntA0Slide+2),a
	rl c
	jr nc,$+5
	ld (soundLoop.cntA0Slide+3),a

.ch0noslide0

	ld a,(hl)				;generator 2 slide
	ld c,a
	or a
	jr z,.ch0noslide1
	and $0f
	or OP_INCSP&$f0
	rl c
	jr nc,$+5
	ld (soundLoop.cntA1Slide+0),a
	rl c
	jr nc,$+5
	ld (soundLoop.cntA1Slide+1),a
	rl c
	jr nc,$+5
	ld (soundLoop.cntA1Slide+2),a
	rl c
	jr nc,$+5
	ld (soundLoop.cntA1Slide+3),a

.ch0noslide1

	ex de,hl
	ld a,(hl)				;instrument always followed by a note
	inc hl
	add a,a

.note0

	jp z,.ch1				;empty row, skip to the second channel
	cp 2
	jp z,.ch0mute
	ex de,hl
	ld l,a

.phasereset0=$+1
	jr $+2					;$+2 to reset, $+12 to skip

	ld ix,(soundInit.ch0cnt0)
.ch0pha=$+1
	ld a,0
	add a,ixh
	ld ixh,a

.ch0phaseresetskip

	ld h,noteTable/256
	ld c,(hl)
	inc l
	ld b,(hl)
	ld (soundInit.ch0add0),bc
.ch0int=$+1
	ld a,0
	add a,l
	ld l,a
	ld b,(hl)
	dec l
	ld c,(hl)
.ch0det=$+1
	ld hl,0
	add hl,bc
	ld sp,hl				;ch0add1
	ex de,hl

.ch1

	ld a,(hl)
	inc hl
	add a,a
	jp nc,.note1

	ex de,hl				;set instrument of channel 1

	ld h,0
	ld l,a
	add hl,hl
	add hl,hl
.insList1=$+1
	ld bc,0
	add hl,bc

	ld a,(hl)
	inc hl
	ld (.ch1int),a
	ld a,(hl)
	inc hl
	ld (.ch1pha),a
	inc a
	jr z,$+4
	ld a,$0a
	xor $0a
	ld (.phasereset1),a

	ld a,(hl)
	ld (.ch1det),a
	inc hl
	ld a,(hl)				;mix mode
	inc hl
	ld (soundLoop.opB0),a
	ld (soundLoop.opB1),a
	ld (soundLoop.opB2),a
	ld (soundLoop.opB3),a

	ld c,(hl)				;volume
	inc hl
	ld b,2					;out mask
	ld a,c
	and b
	ld (soundLoop.volB0),a
	rr c
	ld a,c
	and b
	ld (soundLoop.volB1),a
	rr c
	ld a,c
	and b
	ld (soundLoop.volB2),a
	rr c
	ld a,c
	and b
	ld (soundLoop.volB3),a

	ld a,(hl)				;phase slide speed
	inc hl
	ld (soundLoop.phaseSlideSpeedB),a

	or a
	jr z,$+4
	ld a,soundLoop.phaseSlideB2-soundLoop.phaseSlideB0
	xor soundLoop.phaseSlideB2-soundLoop.phaseSlideB0
	ld (soundLoop.phaseSlideEnableB),a

	ld bc,0
	ld (soundLoop.cntB0Slide+0),bc
	ld (soundLoop.cntB0Slide+2),bc
	ld (soundLoop.cntB1Slide+0),bc
	ld (soundLoop.cntB1Slide+2),bc

	ld a,(hl)				;generator 1 slide
	inc hl
	ld c,a
	or a
	jr z,.ch1noslide0
	and $0f
	or OP_INCBC&$f0
	rl c
	jr nc,$+5
	ld (soundLoop.cntB0Slide+0),a
	rl c
	jr nc,$+5
	ld (soundLoop.cntB0Slide+1),a
	rl c
	jr nc,$+5
	ld (soundLoop.cntB0Slide+2),a
	rl c
	jr nc,$+5
	ld (soundLoop.cntB0Slide+3),a

.ch1noslide0

	ld a,(hl)				;generator 2 slide
	ld c,a
	or a
	jr z,.ch1noslide1
	and $0f
	or OP_INCDE&$f0
	rl c
	jr nc,$+5
	ld (soundLoop.cntB1Slide+0),a
	rl c
	jr nc,$+5
	ld (soundLoop.cntB1Slide+1),a
	rl c
	jr nc,$+5
	ld (soundLoop.cntB1Slide+2),a
	rl c
	jr nc,$+5
	ld (soundLoop.cntB1Slide+3),a

.ch1noslide1

	ex de,hl
	ld a,(hl)
	inc hl
	add a,a

.note1

	jp z,.ch1skip
	cp 2
	jp z,.ch1mute
	ex de,hl
	ld l,a

.phasereset1=$+1
	jr $+2					;$+2 to reset, $+12 to skip

	ld iy,(.ch1cnt0)
.ch1pha=$+1
	ld a,0
	add a,iyh
	ld iyh,a

.ch1phaseresetskip

	ld h,noteTable/256
	ld c,(hl)
	inc l
	ld b,(hl)
	ld (.ch1add0),bc
.ch1int=$+1
	ld a,0
	add a,l
	ld l,a
	ld b,(hl)
	dec l
	ld c,(hl)
.ch1det=$+1
	ld hl,0
	add hl,bc
	ld (.ch1add1),hl
	ex de,hl
	exx
.ch1cnt0=$+1
	ld hl,0
.ch1add1=$+1
	ld de,0
.ch1add0=$+1
	ld bc,0
	exx

.ch1skip

	ld (.pos),hl

    xor a
    jp soundInit

stopPlayer

.oldSP=$+1
	ld sp,0
	pop hl
	exx
	pop iy
	ei
	ret



soundInit

.skip=$+1
	ld d,0			;drum length to compensate
.len=$+1
	ld a,100		;speed
	sub d
	jp nc,$+5
	ld a,1
	or a
	jp nz,$+5
	ld a,1
	ld d,a
	xor a
	ld (.skip),a

.ch0cnt0=$+1
	ld hl,0
.ch0add0=$+1
	ld bc,200

soundLoopH

	ld e,64			;4*64 loop repeats, ~85 hz

soundLoop

	add hl,bc		;11
	ld a,h			;4
	add ix,sp		;15
.opA0=$+1
	xor ixh			;8
	rla				;4
	sbc a,a			;4
.volA0=$+1
	and 2			;7
	out ($ff),a		;11


	exx				;4
	add hl,bc		;11
	ld a,h			;4
	add iy,de		;15
.opB0=$+1
	xor iyh			;8
	rla				;4
	sbc a,a			;4
.volB0=$+1
	and 2			;7
	out ($ff),a		;11
	exx				;4

;	nop				;4
	jr $+2			;12=152

	add hl,bc		;11
	ld a,h			;4
	add ix,sp		;15
.opA1=$+1
	xor ixh			;8
	rla				;4
	sbc a,a			;4
.volA1=$+1
	 and 2			;7
	out ($ff),a		;11
	exx				;4
	add hl,bc		;11
	ld a,h			;4
	add iy,de		;15
.opB1=$+1
	xor iyh			;8
	rla				;4
	sbc a,a			;4
.volB1=$+1
 	and 2			;7
	out ($ff),a		;11
	exx				;4

;	nop				;4
	jr $+2			;12=152

	add hl,bc		;11
	ld a,h			;4
	add ix,sp		;15
.opA2=$+1
	xor ixh			;8
	rla				;4
	sbc a,a			;4
.volA2=$+1
 	and 2			;7
	out ($ff),a		;11
	exx				;4
	add hl,bc		;11
	ld a,h			;4
	add iy,de		;15
.opB2=$+1
	xor iyh			;8
	rla				;4
	sbc a,a			;4
.volB2=$+1
 	and 2			;7
	out ($ff),a		;11
	exx				;4

;	nop				;4
	jr $+2			;12=152

	add hl,bc		;11
	ld a,h			;4
	add ix,sp		;15
.opA3=$+1
	xor ixh			;8
	rla				;4
	sbc a,a			;4
.volA3=$+1
 	and 2			;7
	out ($ff),a		;11
	exx				;4
	add hl,bc		;11
	ld a,h			;4
	add iy,de		;15
.opB3=$+1
	xor iyh			;8
	rla				;4
	sbc a,a			;4
.volB3=$+1
 	and 2			;7
	out ($ff),a		;11
	exx				;4

	dec e			;4
	jr nz,soundLoop	;12=152

.phaseSlideEnableA=$+1
	jr $+2

.phaseSlideA0
.phaseSlideDivA=$+1
	ld a,0
	inc a
.phaseSlideSpeedA=$+1
	cp 1
	jr c,$+3
	xor a
	ld (.phaseSlideDivA),a
	jr c,.phaseSlideA1

	ld a,ixh
	cp h
	jr z,.phaseSlideA1
	inc h
	ld a,ixl
	ld l,a
.phaseSlideA1

.phaseSlideEnableB=$+1
	jr $+2

.phaseSlideB0
.phaseSlideDivB=$+1
	ld a,0
	inc a
.phaseSlideSpeedB=$+1
	cp 1
	jr c,$+3
	xor a
	ld (.phaseSlideDivB),a
	jr c,.phaseSlideB2

	exx
	ld a,iyh
	cp h
	jr z,.phaseSlideB1
	inc h
	ld a,iyl
	ld l,a
.phaseSlideB1
	exx
.phaseSlideB2

.cntA0Slide=$
	nop			;bc
	nop
	nop
	nop

.cntA1Slide=$
	nop			;sp
	nop
	nop
	nop

	exx

.cntB0Slide=$
	nop			;bc
	nop
	nop
	nop

.cntB1Slide=$
	nop			;de
	nop
	nop
	nop

	exx

	dec d
	jp nz,soundLoopH

	ld (soundInit.ch0cnt0),hl

	jp readRow



playDrumSample

	ld l,a
	ld h,0
	add hl,hl
	add hl,hl
.drumList=$+1
	ld bc,0
	add hl,bc
	
	ld a,(hl)				;length in 256-sample blocks
	ld b,a
	inc hl
	inc hl
	ld (soundInit.skip),a
	ld a,(hl)
	inc hl
	ld h,(hl)				;sample data
	ld l,a

	ld a,1
	ld (.mask),a
	
	ld c,0
.loop0
	ld a,(hl)			;7
.mask=$+1
	and 0				;7
	sub 1				;7
	sbc a,a				;4
 	and 2				;7
	out ($ff),a			;11
	ld a,(.mask)		;13
	rlc a				;8
	ld (.mask),a		;13
	jr nc,$+3			;7/12
	inc hl				;6
	
	dec hl				;6
	inc hl				;6
	dec hl				;6
	inc hl				;6
	dec hl				;6
	inc hl				;6
	dec hl				;6
	inc hl				;6
	ld a,0				;7
	
	dec c				;4
	jr nz,.loop0		;7/12=105t
	dec b
	jp nz,.loop0
	
	jp readRow



	align 256

noteTable

	dw $0000,$0000
	dw $0030,$0033,$0036,$003a,$003d,$0041,$0045,$0049,$004d,$0052,$0057,$005c
	dw $0061,$0067,$006d,$0074,$007b,$0082,$008a,$0092,$009b,$00a4,$00ae,$00b8
	dw $00c3,$00cf,$00db,$00e9,$00f6,$0105,$0115,$0125,$0137,$0149,$015d,$0171
	dw $0187,$019f,$01b7,$01d2,$01ed,$020b,$022a,$024b,$026e,$0293,$02ba,$02e3
	dw $030f,$033e,$036f,$03a4,$03db,$0416,$0454,$0496,$04dc,$0526,$0574,$05c7
	dw $061f,$067c,$06df,$0748,$07b7,$082c,$08a8,$092c,$09b8,$0a4c,$0ae9,$0b8f
	dw $0c3f,$0cf9,$0dbf,$0e90,$0f6e,$1059,$1151,$1259,$1370,$1498,$15d2,$171e
	dw $187e,$19f3,$1b7e,$1d20,$1edc,$20b2,$22a3,$24b3,$26e1,$2931,$2ba4,$2e3c
	dw $30fc,$33e6,$36fc,$3a41,$3db8,$4164,$4547,$4966,$4dc3,$5263,$5748,$5c79
	dw $61f9,$67cc,$6df8,$7483,$7b71,$82c8,$8a8f,$92cc,$9b86,$a4c6,$ae91,$b8f3



 align 256
musicData
 dw .sequence
 dw .insList
.drums
 dw 4,.drum0
 dw 4,.drum1
 dw 4,.drum2
 dw 4,.drum3
 dw 4,.drum4
 dw 4,.drum5
 dw 4,.drum6
 dw 4,.drum7
.drum0
.drum1
.drum2
.drum3
.drum4
.drum5
.drum6
.drum7
.insList
 db 0,255,4,172,240,0,0,0
.sequence
 db $f5,7
 db 128,35,128,1
 db 0,0
 db 54,0
 db 0,0
 db 35,0
 db 0,0
 db 0,0
 db 0,0
 db 50,0
 db 0,0
 db 0,0
 db 0,0
 db 35,0
 db 0,0
 db 50,0
 db 0,0
 db 0,0
 db 0,0
 db 35,0
 db 0,0
 db 50,0
 db 0,0
 db 35,0
 db 0,0
 db 49,0
 db 0,0
 db 35,0
 db 0,0
 db 50,0
 db 0,0
 db 0,0
 db 0,0
 db 31,0
 db 0,0
 db 54,0
 db 0,0
 db 31,0
 db 0,0
 db 0,0
 db 0,0
 db 50,0
 db 0,0
 db 0,0
 db 0,0
 db 31,0
 db 0,0
 db 50,0
 db 0,0
 db 0,0
 db 0,0
 db 31,0
 db 0,0
 db 49,0
 db 0,0
 db 50,0
 db 0,0
 db 0,0
 db 0,0
 db 31,0
 db 0,0
 db 0,0
 db 0,0
 db 0,0
 db 0,0
 db 28,0
 db 0,0
 db 54,0
 db 0,0
 db 28,0
 db 0,0
 db 0,0
 db 0,0
 db 50,0
 db 0,0
 db 0,0
 db 0,0
 db 28,0
 db 0,0
 db 50,0
 db 0,0
 db 0,0
 db 0,0
 db 28,0
 db 0,0
 db 50,0
 db 0,0
 db 28,0
 db 0,0
 db 49,0
 db 0,0
 db 28,0
 db 0,0
 db 50,0
 db 0,0
 db 0,0
 db 0,0
 db 31,0
 db 0,0
 db 50,0
 db 0,0
 db 31,0
 db 0,0
 db 0,0
 db 0,0
 db 49,0
 db 0,0
 db 0,0
 db 0,0
 db 50,0
 db 0,0
 db 0,0
 db 0,0
 db 33,0
 db 0,0
 db 50,0
 db 0,0
 db 33,0
 db 0,0
 db 0,0
 db 0,0
 db 49,45
 db 0,40
 db 0,37
 db 0,35
 db 50,33
 db 0,28
 db 0,25
 db 0,23
 db 35,35
 db 0,1
 db 54,35
 db 0,1
 db 35,35
 db 0,1
 db 0,35
 db 0,1
 db 50,35
 db 0,1
 db 0,35
 db 0,1
 db 35,35
 db 0,1
 db 50,35
 db 0,1
 db 0,35
 db 0,1
 db 35,35
 db 0,1
 db 50,35
 db 0,1
 db 35,35
 db 0,1
 db 49,35
 db 0,1
 db 35,35
 db 0,1
 db 50,35
 db 0,1
 db 0,35
 db 0,1
 db 31,31
 db 0,1
 db 54,31
 db 0,1
 db 31,31
 db 0,1
 db 0,31
 db 0,1
 db 50,31
 db 0,1
 db 0,31
 db 0,1
 db 31,31
 db 0,1
 db 50,31
 db 0,1
 db 0,31
 db 0,1
 db 31,31
 db 0,1
 db 49,31
 db 0,1
 db 50,31
 db 0,1
 db 0,43
 db 0,38
 db 31,35
 db 0,33
 db 0,31
 db 0,26
 db 0,23
 db 0,21
 db 28,28
 db 0,1
 db 54,28
 db 0,1
 db 28,28
 db 0,1
 db 0,28
 db 0,1
 db 50,28
 db 0,1
 db 0,28
 db 0,1
 db 28,28
 db 0,1
 db 50,28
 db 0,1
 db 0,28
 db 0,1
 db 28,28
 db 0,1
 db 50,28
 db 0,1
 db 28,28
 db 0,1
 db 49,28
 db 0,1
 db 28,28
 db 0,1
 db 50,28
 db 0,1
 db 0,28
 db 0,1
 db 31,31
 db 0,1
 db 50,31
 db 0,1
 db 31,31
 db 0,1
 db 0,31
 db 0,1
 db 49,31
 db 0,1
 db 0,31
 db 0,1
 db 50,31
 db 0,1
 db 0,31
 db 0,1
 db 33,33
 db 0,1
 db 0,33
 db 0,1
 db 49,33
 db 0,1
 db 50,33
 db 0,1
 db 0,33
 db 0,1
 db 49,33
 db 47,1
 db 45,33
 db 42,1
 db 1,33
 db 0,1
 db 50,35
 db 1,1
 db 50,35
 db 1,1
 db 0,47
 db 0,1
 db 50,35
 db 1,1
 db 50,35
 db 1,1
 db 0,47
 db 0,1
 db 50,35
 db 1,1
 db 50,47
 db 1,1
 db 0,35
 db 0,1
 db 54,35
 db 1,1
 db 54,47
 db 1,1
 db 0,35
 db 0,1
 db 54,35
 db 1,1
 db 54,47
 db 1,1
 db 52,35
 db 1,1
 db 0,47
 db 0,1
 db 50,35
 db 1,1
 db 50,35
 db 1,1
 db 0,47
 db 0,1
 db 50,35
 db 1,1
 db 50,35
 db 1,1
 db 0,47
 db 0,1
 db 50,35
 db 1,1
 db 50,47
 db 1,1
 db 0,35
 db 0,1
 db 54,35
 db 1,1
 db 54,47
 db 1,1
 db 0,35
 db 0,1
 db 54,35
 db 1,1
 db 54,47
 db 1,1
 db 52,35
 db 1,1
 db 0,47
 db 0,1
 db 50,31
 db 1,0
 db 50,31
 db 1,0
 db 0,43
 db 0,0
 db 50,31
 db 1,0
 db 50,31
 db 1,0
 db 0,43
 db 0,0
 db 50,31
 db 1,0
 db 50,43
 db 1,0
 db 0,31
 db 0,0
 db 54,31
 db 1,0
 db 54,43
 db 1,0
 db 0,31
 db 0,0
 db 54,31
 db 1,0
 db 54,43
 db 1,0
 db 52,31
 db 1,0
 db 0,43
 db 0,0
 db 50,31
 db 1,0
 db 50,31
 db 1,0
 db 0,43
 db 0,0
 db 50,31
 db 1,0
 db 50,31
 db 1,0
 db 0,43
 db 0,0
 db 50,31
 db 1,0
 db 50,43
 db 1,0
 db 0,31
 db 0,0
 db 54,31
 db 1,0
 db 54,43
 db 1,0
 db 0,31
 db 0,0
 db 54,31
 db 1,0
 db 54,43
 db 1,0
 db 52,31
 db 1,0
 db 0,43
 db 0,0
 db 50,28
 db 1,0
 db 50,28
 db 1,0
 db 0,40
 db 0,0
 db 50,28
 db 1,0
 db 50,28
 db 1,0
 db 0,40
 db 0,0
 db 50,28
 db 1,0
 db 50,40
 db 1,0
 db 0,28
 db 0,0
 db 54,28
 db 1,0
 db 54,40
 db 1,0
 db 0,28
 db 0,0
 db 54,28
 db 1,0
 db 54,40
 db 1,0
 db 52,28
 db 1,0
 db 0,40
 db 0,0
 db 50,28
 db 1,0
 db 50,28
 db 1,0
 db 0,40
 db 0,0
 db 50,28
 db 1,0
 db 50,28
 db 1,0
 db 0,40
 db 0,0
 db 50,28
 db 1,0
 db 50,40
 db 1,0
 db 0,28
 db 0,0
 db 54,28
 db 1,0
 db 54,40
 db 1,0
 db 0,28
 db 0,0
 db 54,28
 db 1,0
 db 54,40
 db 1,0
 db 52,28
 db 1,0
 db 0,40
 db 0,0
 db 50,31
 db 1,0
 db 50,31
 db 1,0
 db 0,43
 db 0,0
 db 50,31
 db 1,0
 db 50,31
 db 1,0
 db 0,43
 db 0,0
 db 50,31
 db 1,0
 db 50,43
 db 1,0
 db 0,31
 db 0,0
 db 54,31
 db 1,0
 db 54,43
 db 1,0
 db 0,31
 db 0,0
 db 54,31
 db 1,0
 db 54,43
 db 1,0
 db 52,31
 db 1,0
 db 0,43
 db 0,0
 db 50,33
 db 1,0
 db 50,33
 db 1,0
 db 0,45
 db 0,0
 db 50,33
 db 1,0
 db 50,33
 db 1,0
 db 0,45
 db 0,0
 db 50,33
 db 1,0
 db 50,45
 db 1,0
 db 0,33
 db 0,0
 db 54,33
 db 1,0
 db 54,45
 db 1,0
 db 0,33
 db 0,0
 db 54,33
 db 1,0
 db 54,45
 db 1,0
 db 52,33
 db 1,0
 db 0,45
 db 0,0
 db 47,35
 db 1,0
 db 47,35
 db 1,0
 db 0,1
 db 0,0
 db 47,35
 db 1,0
 db 47,35
 db 1,0
 db 0,1
 db 0,0
 db 47,35
 db 1,0
 db 47,35
 db 1,0
 db 0,62
 db 0,1
 db 0,61
 db 0,1
 db 62,59
 db 1,1
 db 61,54
 db 1,1
 db 59,62
 db 1,1
 db 54,61
 db 1,1
 db 62,59
 db 1,1
 db 61,54
 db 1,1
 db 47,35
 db 1,0
 db 47,35
 db 1,0
 db 0,1
 db 0,0
 db 47,35
 db 1,0
 db 47,35
 db 1,0
 db 0,1
 db 0,0
 db 47,35
 db 1,0
 db 47,35
 db 1,0
 db 0,1
 db 0,0
 db 0,0
 db 0,0
 db 0,0
 db 0,0
 db 0,0
 db 0,0
 db 0,0
 db 0,0
 db 0,0
 db 0,0
 db 0,0
 db 0,0
 db 0,0
 db 0,0
 db 35,35
 db 0,1
 db 54,35
 db 0,1
 db 35,35
 db 0,1
 db 0,35
 db 0,1
 db 50,35
 db 0,1
 db 0,35
 db 0,1
 db 35,35
 db 0,1
 db 50,35
 db 0,1
 db 0,35
 db 0,1
 db 35,35
 db 0,1
 db 50,35
 db 0,1
 db 35,35
 db 0,1
 db 49,35
 db 0,1
 db 35,35
 db 0,1
 db 50,35
 db 0,1
 db 0,35
 db 0,1
 db 31,31
 db 0,1
 db 54,31
 db 0,1
 db 31,31
 db 0,1
 db 0,31
 db 0,1
 db 50,31
 db 0,1
 db 0,31
 db 0,1
 db 31,31
 db 0,1
 db 50,31
 db 0,1
 db 0,31
 db 0,1
 db 31,31
 db 0,1
 db 49,31
 db 0,1
 db 50,31
 db 0,1
 db 0,31
 db 0,1
 db 31,31
 db 0,1
 db 0,31
 db 0,1
 db 0,31
 db 0,1
 db 28,28
 db 0,1
 db 54,28
 db 0,1
 db 28,28
 db 0,1
 db 0,28
 db 0,1
 db 50,28
 db 0,1
 db 0,28
 db 0,1
 db 28,28
 db 0,1
 db 50,28
 db 0,1
 db 0,28
 db 0,1
 db 28,28
 db 0,1
 db 50,28
 db 0,1
 db 28,28
 db 0,1
 db 49,28
 db 0,1
 db 28,28
 db 0,1
 db 50,28
 db 0,1
 db 0,28
 db 0,1
 db 31,31
 db 0,1
 db 50,31
 db 0,1
 db 31,31
 db 0,1
 db 0,31
 db 0,1
 db 49,31
 db 0,1
 db 0,31
 db 0,1
 db 50,31
 db 0,1
 db 0,31
 db 0,1
 db 33,33
 db 0,1
 db 50,33
 db 0,1
 db 33,33
 db 0,1
 db 0,33
 db 0,1
 db 49,45
 db 0,40
 db 0,37
 db 0,35
 db 50,33
 db 0,28
 db 0,25
 db 0,23
 db 35,35
 db 0,1
 db 54,35
 db 0,1
 db 35,47
 db 0,1
 db 0,35
 db 0,1
 db 50,35
 db 0,1
 db 0,47
 db 0,1
 db 35,35
 db 0,1
 db 50,47
 db 0,1
 db 0,35
 db 0,1
 db 35,35
 db 0,1
 db 50,47
 db 0,1
 db 35,35
 db 0,1
 db 49,35
 db 0,1
 db 35,47
 db 0,1
 db 50,35
 db 0,1
 db 0,47
 db 0,1
 db 31,31
 db 0,1
 db 54,31
 db 0,1
 db 31,43
 db 0,1
 db 0,31
 db 0,1
 db 50,31
 db 0,1
 db 0,43
 db 0,1
 db 31,31
 db 0,1
 db 50,43
 db 0,1
 db 0,31
 db 0,1
 db 31,31
 db 0,1
 db 49,43
 db 0,1
 db 50,31
 db 0,1
 db 0,43
 db 0,38
 db 31,35
 db 0,33
 db 0,31
 db 0,26
 db 0,23
 db 0,21
 db 28,28
 db 0,1
 db 54,28
 db 0,1
 db 28,40
 db 0,1
 db 0,28
 db 0,1
 db 50,28
 db 0,1
 db 0,40
 db 0,1
 db 28,28
 db 0,1
 db 50,40
 db 0,1
 db 0,28
 db 0,1
 db 28,28
 db 0,1
 db 50,40
 db 0,1
 db 28,28
 db 0,1
 db 49,28
 db 0,1
 db 28,40
 db 0,1
 db 50,28
 db 0,1
 db 0,40
 db 0,1
 db 31,31
 db 0,1
 db 50,31
 db 0,1
 db 31,43
 db 0,1
 db 0,31
 db 0,1
 db 49,31
 db 0,1
 db 0,43
 db 0,1
 db 50,31
 db 0,1
 db 0,43
 db 0,1
 db 33,33
 db 0,1
 db 50,33
 db 0,1
 db 1,33
 db 0,1
 db 0,33
 db 0,1
 db 49,45
 db 0,42
 db 0,40
 db 0,37
 db 50,33
 db 0,30
 db 0,28
 db 0,25
 db 50,35
 db 1,1
 db 50,35
 db 1,1
 db 0,47
 db 0,1
 db 50,35
 db 1,1
 db 50,35
 db 1,1
 db 0,47
 db 0,1
 db 50,35
 db 1,1
 db 50,47
 db 1,1
 db 0,35
 db 0,1
 db 54,35
 db 1,1
 db 54,47
 db 1,1
 db 0,35
 db 0,1
 db 54,35
 db 1,1
 db 54,47
 db 1,1
 db 52,35
 db 1,1
 db 0,47
 db 0,1
 db 50,35
 db 1,1
 db 50,35
 db 1,1
 db 0,47
 db 0,1
 db 50,35
 db 1,1
 db 50,35
 db 1,1
 db 0,47
 db 0,1
 db 50,35
 db 1,1
 db 50,47
 db 1,1
 db 0,35
 db 0,1
 db 54,35
 db 1,1
 db 54,47
 db 1,1
 db 0,35
 db 0,1
 db 54,35
 db 1,1
 db 54,47
 db 1,1
 db 52,35
 db 1,1
 db 0,47
 db 0,1
 db 50,31
 db 1,0
 db 50,31
 db 1,0
 db 0,43
 db 0,0
 db 50,31
 db 1,0
 db 50,31
 db 1,0
 db 0,43
 db 0,0
 db 50,31
 db 1,0
 db 50,43
 db 1,0
 db 0,31
 db 0,0
 db 54,31
 db 1,0
 db 54,43
 db 1,0
 db 0,31
 db 0,0
 db 54,31
 db 1,0
 db 54,43
 db 1,0
 db 52,31
 db 1,0
 db 0,43
 db 0,0
 db 50,31
 db 1,0
 db 50,31
 db 1,0
 db 0,43
 db 0,0
 db 50,31
 db 1,0
 db 50,31
 db 1,0
 db 0,43
 db 0,0
 db 50,31
 db 1,0
 db 50,43
 db 1,0
 db 0,31
 db 0,0
 db 54,31
 db 1,0
 db 54,43
 db 1,0
 db 0,31
 db 0,0
 db 54,31
 db 1,0
 db 54,43
 db 1,0
 db 52,31
 db 1,0
 db 0,43
 db 0,0
 db 50,28
 db 1,0
 db 50,28
 db 1,0
 db 0,40
 db 0,0
 db 50,28
 db 1,0
 db 50,28
 db 1,0
 db 0,40
 db 0,0
 db 50,28
 db 1,0
 db 50,40
 db 1,0
 db 0,28
 db 0,0
 db 54,28
 db 1,0
 db 54,40
 db 1,0
 db 0,28
 db 0,0
 db 54,28
 db 1,0
 db 54,40
 db 1,0
 db 52,28
 db 1,0
 db 0,40
 db 0,0
 db 50,28
 db 1,0
 db 50,28
 db 1,0
 db 0,40
 db 0,0
 db 50,28
 db 1,0
 db 50,28
 db 1,0
 db 0,40
 db 0,0
 db 50,28
 db 1,0
 db 50,40
 db 1,0
 db 0,28
 db 0,0
 db 54,28
 db 1,0
 db 54,40
 db 1,0
 db 0,28
 db 0,0
 db 54,28
 db 1,0
 db 54,40
 db 1,0
 db 52,28
 db 1,0
 db 0,40
 db 0,0
 db 50,31
 db 1,0
 db 50,31
 db 1,0
 db 0,43
 db 0,0
 db 50,31
 db 1,0
 db 50,31
 db 1,0
 db 0,43
 db 0,0
 db 50,31
 db 1,0
 db 50,43
 db 1,0
 db 0,31
 db 0,0
 db 54,31
 db 1,0
 db 54,43
 db 1,0
 db 0,31
 db 0,0
 db 54,31
 db 1,0
 db 54,43
 db 1,0
 db 52,31
 db 1,0
 db 0,43
 db 0,0
 db 50,33
 db 1,0
 db 50,33
 db 1,0
 db 0,45
 db 0,0
 db 50,33
 db 1,0
 db 50,33
 db 1,0
 db 0,45
 db 0,0
 db 50,33
 db 1,0
 db 50,45
 db 1,0
 db 0,33
 db 0,0
 db 54,33
 db 1,0
 db 54,45
 db 1,0
 db 0,33
 db 0,0
 db 54,33
 db 1,0
 db 54,45
 db 1,0
 db 52,33
 db 1,0
 db 0,45
 db 0,0
 db 55,31
 db 1,0
 db 50,31
 db 1,0
 db 59,43
 db 55,0
 db 50,31
 db 50,0
 db 55,31
 db 59,0
 db 50,43
 db 50,0
 db 59,31
 db 55,0
 db 50,43
 db 50,0
 db 55,31
 db 59,0
 db 50,31
 db 50,0
 db 59,43
 db 55,0
 db 50,31
 db 50,0
 db 55,31
 db 59,0
 db 50,43
 db 50,0
 db 59,31
 db 55,0
 db 50,43
 db 50,0
 db 54,33
 db 1,0
 db 50,33
 db 1,0
 db 57,45
 db 54,0
 db 50,33
 db 50,0
 db 54,33
 db 57,0
 db 50,45
 db 50,0
 db 57,33
 db 54,0
 db 50,45
 db 50,0
 db 54,33
 db 57,0
 db 50,33
 db 50,0
 db 57,45
 db 54,0
 db 50,33
 db 50,0
 db 54,33
 db 57,0
 db 50,45
 db 50,0
 db 57,33
 db 54,0
 db 50,45
 db 50,0
 db 67,31
 db 1,0
 db 50,31
 db 1,0
 db 59,43
 db 67,0
 db 50,31
 db 50,0
 db 67,31
 db 59,0
 db 50,43
 db 50,0
 db 59,31
 db 67,0
 db 50,43
 db 50,0
 db 67,31
 db 59,0
 db 50,31
 db 50,0
 db 59,43
 db 67,0
 db 50,31
 db 50,0
 db 67,31
 db 59,0
 db 50,43
 db 50,0
 db 59,31
 db 67,0
 db 50,43
 db 50,0
 db 66,28
 db 1,0
 db 50,28
 db 1,0
 db 57,40
 db 66,0
 db 50,28
 db 50,0
 db 66,28
 db 57,0
 db 50,40
 db 50,0
 db 57,28
 db 66,0
 db 50,40
 db 50,0
 db 66,30
 db 57,0
 db 50,30
 db 50,0
 db 57,42
 db 66,0
 db 50,30
 db 50,0
 db 66,30
 db 57,0
 db 50,42
 db 50,0
 db 57,30
 db 66,0
 db 50,42
 db 50,0
 db 67,31
 db 1,0
 db 50,31
 db 1,0
 db 71,43
 db 67,0
 db 50,31
 db 50,0
 db 67,31
 db 71,0
 db 50,43
 db 50,0
 db 71,31
 db 67,0
 db 50,43
 db 50,0
 db 67,31
 db 71,0
 db 50,31
 db 50,0
 db 71,43
 db 67,0
 db 50,31
 db 50,0
 db 67,31
 db 71,0
 db 50,43
 db 50,0
 db 71,31
 db 67,0
 db 50,43
 db 50,0
 db 66,33
 db 1,0
 db 50,33
 db 1,0
 db 69,45
 db 66,0
 db 50,33
 db 50,0
 db 66,33
 db 69,0
 db 50,45
 db 50,0
 db 69,33
 db 66,0
 db 50,45
 db 50,0
 db 66,33
 db 69,0
 db 50,33
 db 50,0
 db 69,45
 db 66,0
 db 50,33
 db 50,0
 db 66,33
 db 69,0
 db 50,45
 db 50,0
 db 69,33
 db 66,0
 db 50,45
 db 50,0
 db 71,35
 db 1,0
 db 71,35
 db 1,0
 db 0,1
 db 0,0
 db 71,35
 db 1,0
 db 71,35
 db 1,0
 db 0,1
 db 0,0
 db 71,35
 db 1,0
 db 71,35
 db 1,0
 db 0,1
 db 0,0
 db 71,0
 db 1,0
 db 71,0
 db 1,0
 db 0,0
 db 0,0
 db 71,0
 db 1,0
 db 0,0
 db 0,0
 db 71,0
 db 1,0
 db 71,0
 db 1,0
 db 47,35
 db 1,0
 db 47,35
 db 1,0
 db 0,1
 db 0,0
 db 47,35
 db 1,0
 db 47,35
 db 1,0
 db 0,1
 db 0,0
 db 47,35
 db 1,0
 db 47,35
 db 1,0
 db 0,1
 db 0,0
 db 0,0
 db 0,0
 db 0,0
 db 0,0
 db 0,0
 db 0,0
 db 47,35
 db 47,0
 db 47,0
 db 47,0
 db 47,0
 db 47,0
 db 47,0
 db 0,0
 db 1,1
 db $fe,$01,$01,$ff
 db $ff



end




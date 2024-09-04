	output "repeat.bin"

	org #6000

	;test code

begin

	ld hl,musicData
	call play
	ret



	;engine code

play

	di

	ld (readRow.drumList),hl

	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a

	xor a
	ld (songSpeedComp),a
	ld (soundLoop.ch1out),a
	ld (soundLoop.ch2out),a

	ld a,128
	ld (soundLoop.ch1freq),a
	ld (soundLoop.ch2freq),a
	ld a,1
	ld (soundLoop.ch1delay1),a
	ld (soundLoop.ch2delay1),a
	ld a,16
	ld (soundLoop.ch1delay2),a
	ld (soundLoop.ch2delay2),a

	exx
	ld d,a
	ld e,a
	ld b,a
	ld c,a
	push hl
	exx

readRow

	ld c,(hl)
	inc hl

	bit 7,c
	jr z,.noSpeed

	ld a,(hl)
	inc hl
	or a
	jr nz,.noLoop

	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	jr readRow

.noLoop

	ld (songSpeed),a

.noSpeed

	bit 6,c
	jr z,.noSustain1

	ld a,(hl)
	inc hl
	exx
	ld d,a
	ld e,a
	exx

.noSustain1

	bit 5,c
	jr z,.noSustain2

	ld a,(hl)
	inc hl
	exx
	ld b,a
	ld c,a
	exx

.noSustain2

	bit 4,c
	jr z,.noNote1

	ld a,(hl)
	ld d,a
	inc hl
	or a
	jr z,$+4
	ld a,#2
	ld (soundLoop.ch1out),a
	jr z,.noNote1

	ld a,d
	ld (soundLoop.ch1freq),a
	srl a
	srl a
	ld (soundLoop.ch1delay2),a
	ld a,1
	ld (soundLoop.ch1delay1),a

	exx
	ld e,d
	exx

.noNote1

	bit 3,c
	jr z,.noNote2

	ld a,(hl)
	ld e,a
	inc hl
	or a
	jr z,$+4
	ld a,#2
	ld (soundLoop.ch2out),a
	jr z,.noNote2

	ld a,e
	ld (soundLoop.ch2freq),a
	srl a
	srl a
	srl a
	ld (soundLoop.ch2delay2),a
	ld a,1
	ld (soundLoop.ch2delay1),a

	exx
	ld c,b
	exx

.noNote2

	ld a,c
	and 7
	jr z,.noDrum

.playDrum

	push hl

	add a,a
	add a,a
	ld c,a
	ld b,0
.drumList=$+1
	ld hl,0
	add hl,bc

	ld a,(hl)				;length in 256-sample blocks
	ld b,a
	inc hl
	inc hl

	add a,a
	add a,a
	ld (songSpeedComp),a

	ld a,(hl)
	inc hl
	ld h,(hl)				;sample data
	ld l,a

	ld a,1
	ld (.mask),a

	ld c,0
.loop0
	ld a,(hl)				;7
.mask=$+1
	and 0					;7
	sub 1					;7
	sbc a,a					;4
	and #2					;7
	out (#ff),a				;11
	ld a,(.mask)			;13
	rlc a					;8
	ld (.mask),a			;13
	jr nc,$+3				;7/12
	inc hl					;6

	jr $+2					;12
	jr $+2					;12
	jr $+2					;12
	jr $+2					;12
	nop						;4
	nop						;4
	ld a,0					;7

	dec c					;4
	jr nz,.loop0			;7/12=168t
	djnz .loop0

	pop hl

.noDrum

songSpeed=$+1
	ld a,0
	ld b,a
songSpeedComp=$+1
	sub 0
	jr nc,$+3
	xor a
	ld c,a

	ld a,(songSpeedComp)
	sub b
	jr nc,$+3
	xor a
	ld (songSpeedComp),a

	ld a,c
	or a
	jp z,readRow

	ld c,a
	ld b,64

soundLoop

	ld a,3				;7
	dec a				;4
	jr nz,$-1			;7/12=50t
	jr $+2				;12

	dec d				;4
	jp nz,.ch2			;10

.ch1freq=$+1
	ld d,0				;7

.ch1delay1=$+1
	ld a,0				;7
	dec a				;4
	jr nz,$-1			;7/12

.ch1out=$+1
	ld a,0				;7
	out (#ff),a			;11

.ch1delay2=$+1
	ld a,0				;7
	dec a				;4
	jr nz,$-1			;7/12

	and 2
	out (#ff),a			;11

.ch2

	ld a,3				;7
	dec a				;4
	jr nz,$-1			;7/12=50t
	jr $+2				;12

	dec e				;4
	jp nz,.loop			;10

.ch2freq=$+1
	ld e,0				;7

.ch2delay1=$+1
	ld a,0				;7
	dec a				;4
	jr nz,$-1			;7/12

.ch2out=$+1
	ld a,0				;7
	out (#ff),a			;11

.ch2delay2=$+1
	ld a,0				;7
	dec a				;4
	jr nz,$-1			;7/12
	and 2
	out (#ff),a			;11

.loop

	dec b				;4
	jr nz,soundLoop		;7/12=168t

	ld b,64

envelopeDown

	exx

	dec e
	jp nz,.noEnv1
	ld e,d

	ld hl,soundLoop.ch1delay2
	dec (hl)
	jr z,$+5
	ld hl,soundLoop.ch1delay1
	inc (hl)

.noEnv1

	dec c
	jp nz,.noEnv2
	ld c,b

	ld hl,soundLoop.ch2delay2
	dec (hl)
	jr z,$+5
	ld hl,soundLoop.ch2delay1
	inc (hl)

.noEnv2

	exx

	dec c
	jp nz,soundLoop

;	xor a
;	in a,(#fe)
;	cpl
;	and #1f
;	jp z,readRow
	jp readRow

	pop hl
	exx
	ei
	ret



musicData:
 dw .song,0
.drums:
 dw 3,.drum0
 dw 1,.drum1
 dw 1,.drum2
 dw 1,.drum3
 dw 1,.drum4
 dw 4,.drum5
 dw 4,.drum6
.drum0:
 db #00,#00,#fc,#ff,#ff,#00,#00,#00,#40,#00,#00,#e0,#df,#ff,#ff,#1f
 db #07,#00,#00,#00,#00,#f8,#ff,#ff,#ff,#1b,#01,#00,#00,#00,#00,#00
 db #c0,#f0,#fe,#bf,#ff,#3f,#00,#00,#00,#00,#00,#c2,#c7,#ff,#ff,#bf
 db #00,#00,#00,#00,#00,#00,#84,#b6,#f7,#fe,#7f,#06,#00,#00,#00,#00
 db #00,#00,#50,#ff,#f9,#4f,#11,#00,#00,#00,#00,#00,#02,#40,#69,#6b
 db #0b,#03,#00,#00,#20,#00,#02,#00,#00,#00,#28,#01,#48,#80,#00,#10
.drum1:
 db #00,#00,#00,#00,#00,#04,#f0,#f7,#ff,#ff,#ff,#ff,#ff,#ff,#ff,#ff
 db #ff,#ff,#ff,#ff,#0f,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00
.drum2:
 db #00,#00,#00,#00,#00,#00,#c0,#ff,#3f,#00,#00,#f0,#ff,#07,#00,#00
 db #fe,#7f,#00,#00,#e0,#ff,#0f,#00,#00,#f8,#ff,#01,#00,#00,#ff,#1f
.drum3:
 db #00,#01,#a0,#25,#a2,#12,#00,#00,#00,#80,#00,#00,#02,#20,#00,#00
 db #02,#00,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00
.drum4:
 db #00,#1f,#f0,#00,#ff,#00,#07,#f0,#00,#7f,#00,#03,#f0,#00,#3f,#00
 db #00,#e0,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#07,#f0,#00,#00
.drum5:
.drum6:
.song:
 db 250,25,10,99,31,189
 db 0
 db 27,189,126
 db 0
 db 89,1,189,31
 db 0
 db 90,10,31,189
 db 0
 db 27,189,126
 db 0
 db 90,1,189,31
 db 0
 db 89,10,31,189
 db 0
 db 26,189,126
 db 0
 db 26,189,31
 db 0
 db 27,189,126
 db 0
 db 89,1,189,126
 db 0
 db 90,10,189,126
 db 0
 db 27,189,126
 db 0
 db 90,1,189,126
 db 0
 db 89,10,189,126
 db 0
 db 26,189,126
 db 0
 db 26,42,252
 db 0
 db 27,252,168
 db 0
 db 89,1,252,42
 db 0
 db 90,10,42,252
 db 0
 db 27,252,168
 db 0
 db 90,1,252,42
 db 0
 db 89,10,42,252
 db 0
 db 26,252,168
 db 0
 db 26,39,238
 db 0
 db 27,238,159
 db 0
 db 89,1,238,39
 db 0
 db 91,10,39,238
 db 0
 db 26,212,141
 db 0
 db 90,1,212,39
 db 0
 db 89,10,35,212
 db 0
 db 25,212,141
 db 0
 db 26,31,189
 db 0
 db 27,189,126
 db 0
 db 89,1,189,31
 db 0
 db 90,10,31,189
 db 0
 db 27,189,126
 db 0
 db 90,1,189,31
 db 0
 db 89,10,31,189
 db 0
 db 26,189,126
 db 0
 db 26,189,31
 db 0
 db 27,189,126
 db 0
 db 89,1,189,126
 db 0
 db 90,10,189,126
 db 0
 db 27,189,126
 db 0
 db 90,1,189,126
 db 0
 db 89,10,189,126
 db 0
 db 26,189,126
 db 0
 db 26,42,252
 db 0
 db 27,252,168
 db 0
 db 89,1,252,42
 db 0
 db 90,10,42,252
 db 0
 db 27,252,168
 db 0
 db 90,1,252,42
 db 0
 db 89,10,42,252
 db 0
 db 26,252,168
 db 0
 db 26,39,238
 db 0
 db 27,238,159
 db 0
 db 89,1,238,239
 db 0
 db 91,10,39,238
 db 0
 db 25,212,39
 db 0
 db 90,1,212,213
 db 0
 db 89,10,35,212
 db 0
 db 25,212,35
 db 1
 db 26,31,189
 db 0
 db 27,189,126
 db 0
 db 89,1,189,31
 db 0
 db 90,10,31,189
 db 0
 db 27,189,126
 db 0
 db 90,1,189,31
 db 0
 db 89,10,31,189
 db 0
 db 26,189,126
 db 0
 db 26,189,31
 db 0
 db 27,189,126
 db 0
 db 89,1,189,126
 db 0
 db 90,10,189,126
 db 0
 db 27,189,126
 db 0
 db 90,1,189,126
 db 0
 db 89,10,189,126
 db 0
 db 26,189,126
 db 0
 db 26,42,252
 db 0
 db 27,252,168
 db 0
 db 89,1,252,42
 db 0
 db 90,10,42,252
 db 0
 db 27,252,168
 db 0
 db 90,1,252,42
 db 0
 db 89,10,42,252
 db 0
 db 26,252,168
 db 0
 db 26,39,238
 db 0
 db 27,238,159
 db 0
 db 89,1,238,39
 db 0
 db 91,10,39,238
 db 0
 db 26,212,141
 db 0
 db 90,1,212,39
 db 0
 db 89,10,35,212
 db 0
 db 25,212,141
 db 0
 db 26,31,189
 db 0
 db 27,189,126
 db 0
 db 89,1,189,31
 db 0
 db 90,10,31,189
 db 0
 db 27,189,126
 db 0
 db 90,1,189,31
 db 0
 db 89,10,31,189
 db 0
 db 26,189,126
 db 0
 db 26,189,31
 db 0
 db 27,189,126
 db 0
 db 89,1,189,126
 db 0
 db 90,10,189,126
 db 0
 db 27,189,126
 db 0
 db 90,1,189,126
 db 0
 db 89,10,189,126
 db 0
 db 26,189,126
 db 0
 db 26,42,252
 db 0
 db 27,252,168
 db 0
 db 89,1,252,42
 db 0
 db 90,10,42,252
 db 0
 db 27,252,168
 db 0
 db 90,1,252,42
 db 0
 db 89,10,42,252
 db 0
 db 26,252,168
 db 0
 db 25,39,238
 db 0
 db 89,99,159,238
 db 0
 db 25,159,238
 db 0
 db 89,10,39,79
 db 0
 db 89,99,141,212
 db 1
 db 26,141,212
 db 2
 db 89,10,35,70
 db 1
 db 90,99,141,212
 db 2
 db 122,10,10,39,189
 db 0
 db 24,39,189
 db 8,39
 db 57,1,42,189
 db 40,10,39
 db 24,47,189
 db 8,42
 db 24,0,189
 db 8,47
 db 58,1,53,189
 db 0
 db 57,10,0,189
 db 8,53
 db 24,53,189
 db 0
 db 26,47,189
 db 8,53
 db 26,47,189
 db 8,47
 db 57,1,53,189
 db 40,10,47
 db 24,47,189
 db 8,53
 db 8,189
 db 8,47
 db 58,1,0,189
 db 0
 db 41,10,189
 db 0
 db 8,189
 db 0
 db 26,39,212
 db 0
 db 24,39,212
 db 8,39
 db 57,1,42,212
 db 40,10,39
 db 24,47,212
 db 8,42
 db 24,0,212
 db 8,47
 db 58,1,53,212
 db 0
 db 57,10,63,212
 db 8,53
 db 24,0,212
 db 8,63
 db 26,70,212
 db 0
 db 10,212
 db 8,70
 db 41,1,212
 db 0
 db 56,10,63,212
 db 0
 db 8,212
 db 8,63
 db 42,1,212
 db 0
 db 57,10,59,212
 db 0
 db 8,212
 db 8,59
 db 90,50,39,238
 db 0
 db 24,39,238
 db 8,39
 db 57,1,42,238
 db 40,10,39
 db 24,47,238
 db 8,42
 db 24,0,238
 db 8,47
 db 58,1,53,238
 db 0
 db 57,10,0,238
 db 8,53
 db 24,53,238
 db 0
 db 26,47,238
 db 8,53
 db 26,47,238
 db 8,47
 db 57,1,53,238
 db 40,10,47
 db 24,47,238
 db 8,53
 db 8,238
 db 8,47
 db 58,1,0,238
 db 0
 db 41,10,238
 db 0
 db 8,238
 db 0
 db 26,70,252
 db 0
 db 8,252
 db 8,70
 db 41,1,252
 db 40,10,70
 db 24,63,252
 db 0
 db 8,252
 db 8,63
 db 42,1,252
 db 40,10,63
 db 25,70,252
 db 0
 db 24,63,252
 db 8,70
 db 10,238
 db 8,63
 db 10,238
 db 0
 db 57,1,126,238
 db 0
 db 40,10,238
 db 0
 db 9,212
 db 0
 db 58,1,119,212
 db 0
 db 41,10,212
 db 0
 db 9,212
 db 1
 db 122,99,99,39,189
 db 0
 db 28,39,189
 db 40,10,39
 db 57,99,42,94
 db 40,10,39
 db 60,99,47,189
 db 40,10,42
 db 56,99,0,189
 db 40,10,47
 db 58,99,53,94
 db 0
 db 25,0,189
 db 40,10,53
 db 59,99,53,94
 db 0
 db 26,47,189
 db 40,10,53
 db 58,99,47,189
 db 40,10,47
 db 57,99,42,94
 db 40,10,47
 db 59,99,39,189
 db 40,10,42
 db 40,99,189
 db 40,10,39
 db 58,99,0,94
 db 8,39
 db 9,189
 db 0
 db 11,94
 db 0
 db 58,90,39,212
 db 0
 db 28,39,212
 db 40,10,39
 db 57,90,42,106
 db 40,10,39
 db 60,90,47,106
 db 40,10,42
 db 56,90,0,212
 db 40,10,47
 db 58,90,53,212
 db 0
 db 25,63,106
 db 40,10,53
 db 59,90,0,106
 db 40,10,63
 db 58,90,70,212
 db 0
 db 10,212
 db 40,10,70
 db 41,90,106
 db 0
 db 59,10,53,106
 db 0
 db 8,212
 db 8,53
 db 42,90,212
 db 0
 db 25,47,106
 db 0
 db 11,106
 db 40,10,47
 db 58,90,39,238
 db 0
 db 28,39,238
 db 40,10,39
 db 57,90,42,119
 db 40,10,39
 db 60,90,47,119
 db 40,10,42
 db 56,90,0,238
 db 40,10,47
 db 58,90,53,238
 db 0
 db 25,0,119
 db 40,10,53
 db 59,90,53,119
 db 0
 db 26,47,238
 db 40,10,53
 db 58,90,47,238
 db 40,10,47
 db 57,90,42,119
 db 40,10,47
 db 59,90,39,119
 db 40,10,42
 db 40,90,238
 db 40,10,39
 db 58,90,0,238
 db 8,39
 db 9,119
 db 0
 db 11,119
 db 0
 db 26,35,252
 db 0
 db 12,252
 db 40,10,35
 db 41,90,126
 db 40,10,35
 db 60,90,31,126
 db 0
 db 8,252
 db 40,10,31
 db 42,90,252
 db 40,10,31
 db 121,1,90,252,126
 db 0
 db 27,252,126
 db 32,10
 db 121,99,90,31,238
 db 32,10
 db 41,90,31
 db 0
 db 41,10,119
 db 0
 db 25,29,119
 db 0
 db 9,29
 db 0
 db 10,238
 db 2
 db 10,119
 db 0
 db 10,119
 db 0
 db 122,10,99,31,189
 db 0
 db 27,189,190
 db 0
 db 89,1,189,31
 db 0
 db 90,10,31,189
 db 0
 db 27,189,190
 db 0
 db 90,1,189,31
 db 0
 db 89,10,31,189
 db 0
 db 26,189,190
 db 0
 db 26,189,31
 db 0
 db 27,189,126
 db 0
 db 89,1,189,126
 db 0
 db 90,10,189,126
 db 0
 db 27,189,126
 db 0
 db 90,1,189,126
 db 0
 db 89,10,126,189
 db 0
 db 26,126,189
 db 0
 db 26,42,252
 db 0
 db 27,252,253
 db 0
 db 89,1,252,42
 db 0
 db 90,10,42,252
 db 0
 db 27,252,253
 db 0
 db 90,1,252,42
 db 0
 db 89,10,42,252
 db 0
 db 26,252,253
 db 0
 db 26,39,238
 db 0
 db 27,238,239
 db 0
 db 89,1,238,39
 db 0
 db 91,10,39,238
 db 0
 db 26,212,213
 db 0
 db 90,1,212,39
 db 0
 db 89,10,35,212
 db 0
 db 25,212,141
 db 0
 db 26,31,189
 db 0
 db 27,189,190
 db 0
 db 89,1,189,31
 db 0
 db 90,10,31,189
 db 0
 db 27,189,190
 db 0
 db 90,1,189,31
 db 0
 db 89,10,31,189
 db 0
 db 26,189,190
 db 0
 db 26,189,31
 db 0
 db 27,189,126
 db 0
 db 89,1,189,126
 db 0
 db 90,10,189,126
 db 0
 db 27,189,126
 db 0
 db 90,1,189,126
 db 0
 db 89,10,126,189
 db 0
 db 26,126,189
 db 0
 db 26,42,252
 db 0
 db 27,252,253
 db 0
 db 89,1,252,42
 db 0
 db 90,10,42,252
 db 0
 db 27,252,253
 db 0
 db 90,1,252,42
 db 0
 db 89,10,42,252
 db 0
 db 26,252,253
 db 0
 db 26,39,238
 db 0
 db 27,238,239
 db 0
 db 89,1,238,239
 db 0
 db 91,10,39,238
 db 0
 db 25,212,213
 db 0
 db 90,1,212,213
 db 0
 db 89,10,35,212
 db 0
 db 25,212,35
 db 1
 db 26,79,189
 db 0
 db 27,189,190
 db 8,79
 db 89,1,126,31
 db 8,79
 db 90,10,31,189
 db 0
 db 27,84,190
 db 0
 db 90,1,189,31
 db 8,84
 db 89,10,126,189
 db 8,84
 db 26,189,190
 db 0
 db 26,79,31
 db 0
 db 27,189,126
 db 8,79
 db 89,1,126,126
 db 8,79
 db 90,10,189,126
 db 0
 db 27,84,126
 db 0
 db 90,1,189,126
 db 8,84
 db 89,10,126,126
 db 8,84
 db 26,189,126
 db 0
 db 26,42,252
 db 0
 db 27,252,253
 db 0
 db 89,1,252,42
 db 0
 db 90,10,42,252
 db 0
 db 27,252,253
 db 0
 db 90,1,252,42
 db 0
 db 89,10,42,252
 db 0
 db 26,252,253
 db 0
 db 26,39,238
 db 0
 db 27,238,239
 db 0
 db 89,1,238,39
 db 0
 db 91,10,39,238
 db 0
 db 26,212,213
 db 0
 db 90,1,212,39
 db 0
 db 89,10,35,212
 db 0
 db 25,212,141
 db 0
 db 26,79,189
 db 0
 db 27,189,190
 db 8,79
 db 89,1,126,31
 db 8,79
 db 90,10,31,189
 db 0
 db 27,84,190
 db 0
 db 90,1,189,31
 db 8,84
 db 89,10,126,189
 db 8,84
 db 26,189,190
 db 0
 db 26,79,31
 db 0
 db 27,189,126
 db 8,79
 db 89,1,126,126
 db 8,79
 db 90,10,189,126
 db 0
 db 27,84,126
 db 0
 db 90,1,189,126
 db 8,84
 db 89,10,79,126
 db 8,84
 db 26,189,126
 db 8,79
 db 26,70,252
 db 0
 db 27,252,253
 db 8,70
 db 89,1,252,42
 db 8,70
 db 90,10,42,252
 db 0
 db 27,252,253
 db 0
 db 90,1,252,42
 db 0
 db 89,10,42,252
 db 0
 db 26,252,253
 db 0
 db 25,39,239
 db 0
 db 25,238,239
 db 0
 db 89,1,238,239
 db 0
 db 89,10,39,79
 db 0
 db 25,212,213
 db 1
 db 89,1,212,213
 db 1
 db 89,10,35,70
 db 1
 db 25,212,213
 db 1
 db 26,39,189
 db 0
 db 28,39,189
 db 8,39
 db 25,42,189
 db 8,39
 db 26,47,189
 db 8,42
 db 24,0,189
 db 8,47
 db 26,53,189
 db 0
 db 25,0,189
 db 8,53
 db 28,53,189
 db 0
 db 26,47,189
 db 8,53
 db 26,47,189
 db 8,47
 db 25,53,189
 db 8,47
 db 26,47,189
 db 8,53
 db 8,189
 db 8,47
 db 26,0,189
 db 0
 db 9,189
 db 0
 db 12,189
 db 0
 db 26,39,212
 db 0
 db 28,39,212
 db 8,39
 db 25,42,212
 db 8,39
 db 26,47,212
 db 8,42
 db 24,0,212
 db 8,47
 db 26,53,212
 db 0
 db 25,63,212
 db 8,53
 db 28,0,212
 db 8,63
 db 26,70,212
 db 0
 db 10,212
 db 8,70
 db 9,212
 db 8,70
 db 26,63,212
 db 0
 db 8,212
 db 8,63
 db 10,212
 db 8,63
 db 25,59,212
 db 0
 db 12,212
 db 8,59
 db 90,50,39,238
 db 0
 db 28,39,238
 db 8,39
 db 25,42,238
 db 8,39
 db 26,47,238
 db 8,42
 db 24,0,238
 db 8,47
 db 26,53,238
 db 0
 db 25,0,238
 db 8,53
 db 28,53,238
 db 0
 db 26,47,238
 db 8,53
 db 26,47,238
 db 8,47
 db 25,53,238
 db 8,47
 db 26,47,238
 db 8,53
 db 8,238
 db 8,47
 db 26,0,238
 db 0
 db 9,238
 db 0
 db 12,238
 db 0
 db 26,75,252
 db 16,70
 db 12,252
 db 8,70
 db 9,252
 db 8,70
 db 26,63,252
 db 0
 db 8,252
 db 8,63
 db 10,252
 db 8,63
 db 25,70,252
 db 0
 db 28,66,252
 db 24,63,70
 db 10,238
 db 0
 db 10,238
 db 8,63
 db 89,10,31,238
 db 80,20,63
 db 88,30,31,238
 db 88,40,63,31
 db 89,50,31,212
 db 88,60,63,31
 db 90,70,29,212
 db 80,80,59
 db 89,90,29,212
 db 24,59,29
 db 25,29,212
 db 25,59,29
 db 90,99,39,189
 db 0
 db 28,39,189
 db 8,39
 db 25,42,94
 db 8,39
 db 29,47,189
 db 13,42
 db 28,63,189
 db 24,0,47
 db 26,53,94
 db 0
 db 25,63,189
 db 24,0,53
 db 27,53,94
 db 0
 db 26,47,189
 db 8,53
 db 26,47,189
 db 8,47
 db 25,42,94
 db 8,47
 db 29,39,189
 db 13,42
 db 11,189
 db 8,39
 db 29,63,94
 db 5
 db 25,63,189
 db 16,0
 db 27,63,94
 db 0
 db 26,39,212
 db 0
 db 28,39,212
 db 8,39
 db 25,42,106
 db 8,39
 db 29,47,106
 db 13,42
 db 28,70,212
 db 24,0,47
 db 26,53,212
 db 0
 db 25,70,106
 db 24,0,53
 db 27,141,106
 db 8,63
 db 26,70,212
 db 0
 db 10,212
 db 8,70
 db 25,70,106
 db 8,70
 db 27,53,106
 db 0
 db 13,212
 db 8,53
 db 29,70,212
 db 8,53
 db 25,47,106
 db 0
 db 10,106
 db 8,47
 db 26,39,238
 db 0
 db 28,39,238
 db 8,39
 db 25,42,119
 db 8,39
 db 29,47,119
 db 13,42
 db 28,79,238
 db 24,0,47
 db 26,53,238
 db 0
 db 25,79,119
 db 24,0,53
 db 27,53,119
 db 0
 db 26,47,238
 db 8,53
 db 26,47,238
 db 8,47
 db 25,42,119
 db 8,47
 db 29,39,119
 db 13,42
 db 11,238
 db 8,39
 db 29,79,238
 db 5
 db 25,79,119
 db 16,0
 db 27,79,119
 db 0
 db 26,35,252
 db 0
 db 12,252
 db 8,35
 db 25,84,126
 db 8,35
 db 29,31,126
 db 5
 db 12,252
 db 8,31
 db 26,84,252
 db 8,31
 db 89,1,252,126
 db 0
 db 27,252,126
 db 0
 db 89,99,31,238
 db 0
 db 9,31
 db 0
 db 25,79,119
 db 0
 db 25,29,119
 db 0
 db 9,29
 db 0
 db 26,79,238
 db 2
 db 26,29,119
 db 0
 db 10,119
 db 0
 db 90,10,79,189
 db 0
 db 27,189,190
 db 8,79
 db 89,1,126,31
 db 8,79
 db 90,10,31,189
 db 0
 db 27,84,190
 db 0
 db 90,1,189,31
 db 8,84
 db 89,10,126,189
 db 8,84
 db 26,189,190
 db 0
 db 26,79,31
 db 0
 db 27,189,126
 db 8,79
 db 89,1,126,126
 db 8,79
 db 90,10,189,126
 db 0
 db 27,84,126
 db 0
 db 90,1,189,126
 db 8,84
 db 89,10,126,126
 db 8,84
 db 26,189,126
 db 0
 db 26,42,252
 db 0
 db 27,252,253
 db 0
 db 89,1,252,42
 db 0
 db 90,10,42,252
 db 0
 db 27,252,253
 db 0
 db 90,1,252,42
 db 0
 db 89,10,42,252
 db 0
 db 26,252,253
 db 0
 db 26,39,238
 db 0
 db 27,238,239
 db 0
 db 89,1,238,39
 db 0
 db 91,10,39,238
 db 0
 db 26,212,213
 db 0
 db 93,1,212,39
 db 5
 db 89,10,35,212
 db 0
 db 29,212,141
 db 5
 db 26,79,189
 db 0
 db 27,189,190
 db 8,79
 db 89,1,126,31
 db 8,79
 db 90,10,31,189
 db 0
 db 27,84,190
 db 0
 db 90,1,189,31
 db 8,84
 db 89,10,126,189
 db 8,84
 db 26,189,190
 db 0
 db 26,79,31
 db 0
 db 27,189,126
 db 8,79
 db 89,1,126,126
 db 8,79
 db 90,10,189,126
 db 0
 db 27,84,126
 db 0
 db 90,1,189,126
 db 8,84
 db 89,10,79,126
 db 8,84
 db 26,189,126
 db 8,79
 db 26,84,252
 db 8,79
 db 27,252,253
 db 8,84
 db 89,1,252,42
 db 8,84
 db 90,10,42,252
 db 0
 db 27,252,253
 db 0
 db 90,1,252,42
 db 0
 db 89,10,42,252
 db 0
 db 26,252,253
 db 0
 db 26,39,238
 db 0
 db 29,238,239
 db 5
 db 89,1,238,239
 db 0
 db 93,10,39,238
 db 5
 db 25,212,213
 db 0
 db 93,1,212,213
 db 5
 db 89,10,35,212
 db 0
 db 29,212,35
 db 0
 db 26,79,189
 db 0
 db 27,189,190
 db 8,39
 db 89,1,126,31
 db 8,39
 db 90,10,31,189
 db 0
 db 27,84,190
 db 0
 db 90,1,189,31
 db 8,42
 db 89,10,126,189
 db 8,42
 db 26,189,190
 db 0
 db 26,79,31
 db 0
 db 27,189,126
 db 8,39
 db 89,1,126,126
 db 8,39
 db 90,10,189,126
 db 0
 db 27,84,126
 db 0
 db 90,1,189,126
 db 8,42
 db 89,10,126,126
 db 8,42
 db 26,189,126
 db 0
 db 26,42,252
 db 0
 db 27,252,253
 db 0
 db 89,1,252,42
 db 0
 db 90,10,42,252
 db 0
 db 27,252,253
 db 0
 db 90,1,252,42
 db 0
 db 89,10,42,252
 db 0
 db 26,252,253
 db 0
 db 26,39,238
 db 0
 db 27,238,239
 db 0
 db 89,1,238,39
 db 0
 db 91,10,39,238
 db 0
 db 26,212,213
 db 0
 db 93,1,212,39
 db 5
 db 89,10,35,212
 db 0
 db 29,212,141
 db 5
 db 26,79,189
 db 0
 db 27,189,190
 db 8,39
 db 89,1,126,31
 db 8,39
 db 90,10,31,189
 db 0
 db 27,84,190
 db 0
 db 90,1,189,31
 db 8,42
 db 89,10,126,189
 db 8,42
 db 26,189,190
 db 0
 db 26,79,31
 db 0
 db 27,189,126
 db 8,39
 db 89,1,126,126
 db 8,39
 db 90,10,189,126
 db 0
 db 27,84,126
 db 0
 db 90,1,189,126
 db 8,42
 db 89,10,79,126
 db 8,42
 db 26,189,126
 db 8,39
 db 26,70,252
 db 8,39
 db 11,253
 db 8,79
 db 89,1,252,42
 db 8,79
 db 90,10,42,252
 db 8,79
 db 27,252,253
 db 0
 db 90,1,252,42
 db 0
 db 89,10,42,252
 db 0
 db 26,252,253
 db 0
 db 25,39,80
 db 0
 db 90,99,159,239
 db 0
 db 25,159,239
 db 0
 db 90,10,39,79
 db 0
 db 89,99,141,213
 db 1
 db 26,141,213
 db 2
 db 89,10,35,70
 db 1
 db 90,99,141,213
 db 2
 db 121,20,20,48,189
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 24,0,0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
 db 0
.loop:
 db 0
 db 0
 db 0
 db 0
 dw #0080,.loop

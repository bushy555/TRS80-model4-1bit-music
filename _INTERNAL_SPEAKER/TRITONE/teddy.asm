
;Tritone v2 beeper music engine by Shiru (shiru@mail.ru) 03'11
;Three channels of tone, per-pattern tempo
;One channel of interrupting drums
;Feel free to do whatever you want with the code, it is PD
;
;
; TRITONE Engine template.
; Assemble with PASMO
;
; 	pasmo --alocal %1.bin
;	trs80-util %1.bin %1.cmd
;	trs80-util %1.bin %1.cas
;	trs80-util %1.bin %1.wav



OP_NOP	equ $00
OP_SCF	equ $37
OP_ORC	equ $b1


	org $6000

begin


	ld hl,musicData
	call play
	jp begin


NO_VOLUME equ 0			;define this if you want to have the same volume for all the channels

play
	di
	ld (.nppos),hl
	ld c,1
	push iy
	exx
	push hl
	ld (.prevSP),sp
	xor a
	ld h,a
	ld l,h
	ld (.cnt0),hl
	ld (.cnt1),hl
	ld (.cnt2),hl
	ld (.duty0),a
	ld (.duty1),a
	ld (.duty2),a
	ld (.skipDrum),a
;	in a,($1f)
;;	and $1f
;	ld a,OP_NOP
;	jr nz,$+4
;	ld a,OP_ORC
;	ld (.checkKempston),a
	jp nextPos

nextRow
.nrpos equ $+1
	ld hl,0
	ld a,(hl)
	inc hl
	cp 2
	jr c,.ch0
	cp 128
	jr c,drumSound
	cp 255
	jp z,nextPos

.ch0
	ld d,1
	cp d
	jr z,.ch1
	or a
	jr nz,.ch0note
	ld b,a
	ld c,a
	jr .ch0set
.ch0note
	ld e,a
	and $0f
	ld b,a
	ld c,(hl)
	inc hl
	ld a,e
	and $f0
.ch0set
	ld (.duty0),a
	ld (.cnt0),bc
.ch1
	ld a,(hl)
	inc hl
	cp d
	jr z,.ch2
	or a
	jr nz,.ch1note
	ld b,a
	ld c,a
	jr .ch1set
.ch1note
	ld e,a
	and $0f
	ld b,a
	ld c,(hl)
	inc hl
	ld a,e
	and $f0
.ch1set
	ld (.duty1),a
	ld (.cnt1),bc
.ch2
	ld a,(hl)
	inc hl
	cp d
	jr z,.skip
	or a
	jr nz,.ch2note
	ld b,a
	ld c,a
	jr .ch2set
.ch2note
	ld e,a
	and $0f
	ld b,a
	ld c,(hl)
	inc hl
	ld a,e
	and $f0
.ch2set
	ld (.duty2),a
	ld (.cnt2),bc

.skip
	ld (.nrpos),hl
.skipDrum equ $
	scf
	jp nc,playRow
	ld a,OP_NOP
	ld (.skipDrum),a

	ld hl,(.speed)
	ld de,-150
	add hl,de
	ex de,hl
	jr c,$+5
	ld de,257
	ld a,d
	or a
	jr nz,$+3
	inc d
	ld a,e
	or a
	jr nz,$+3
	inc e
	jP .drum

drumSound
	ld (.nrpos),hl

	add a,a
	ld ixl,a
	ld ixh,0
	ld bc,drumSettings-4
	add ix,bc
	cp 14*2
	ld a,OP_SCF
	ld (.skipDrum),a
	jr nc,drumNoise

drumTone
	ld bc,2
	ld a,b
	ld de,$0100	; DJM
	ld l,(ix)
.l01
	bit 0,b
	jr z,.l11
	dec e
	jr nz,.l11
	ld e,l
	exa
	ld a,l
	add a,(ix+1)
	ld l,a
;	exa
	ex af,af'
	xor d
.l11

;	and	1
	out	($90), a
	djnz .l01
	dec c
	jr nz,.l01

	jp nextRow

drumNoise
	ld b,0
	ld h,b
	ld l,h
	ld de,$0100	; DJM
.l02
	ld a,(hl)
	and d
	out	($90), a
	and (ix)
	dec e
;	and	1
	out	($90), a
	jr nz,.l12
	ld e,(ix+1)
	inc hl
.l12
	djnz .l02

	jp nextRow

nextPos
.nppos equ $+1
	ld hl,0
.read
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld a,d
	or e
	jr z,orderLoop
	ld (.nppos),hl
	ex de,hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	ld (.nrpos),hl
	ld (.speed),bc
	jp nextRow

orderLoop
	ld e,(hl)
	inc hl
	ld d,(hl)
	ex de,hl
	jr .read

playRow
.speed equ $+1
	ld de,0
.drum
.cnt0 equ $+1
	ld bc,0
.prevHL equ $+1
	ld hl,0
	exx
.cnt1 equ $+1
	ld de,0
.cnt2 equ $+1
	ld sp,0
	exx


soundLoop
	if NO_VOLUME = 1		;all the channels has the same volume
	
	add hl,bc	;11
	ld a,h		;4
.duty0 equ $+1
	cp 128		;7
	sbc a,a		;4
	exx			;4
	and c		;4
	out	($90), a
	add ix,de	;15
	ld a,ixh	;8
.duty1 equ $+1
	cp 128		;7
	sbc a,a		;4
	and c		;4
	out	($90), a
	add hl,sp	;11
	ld a,h		;4
.duty2 equ $+1
	cp 128		;7
	sbc a,a		;4
	and c		;4
	exx			;4
	dec e		;4
;	and	1
	out	($90), a
	jr nz,soundLoop	;10=153t
	dec d		;4
	jr nz,soundLoop	;10
	
	else				; all the channels has different volume

	add hl,bc	;11
	ld a,h		;4
	exx			;4
.duty0 equ $+1
	cp 128		;7
	sbc a,a		;4
	and c		;4
	add ix,de	;15
	out	($90), a
	ld a,ixh	;8
.duty1 equ $+1
	cp 128		;7
	sbc a,a		;4
	and c		;4
	out	($90), a
	add hl,sp	;11
	ld a,h		;4
.duty2 equ $+1
	cp 128		;7
	sbc a,a		;4
	and c		;4
	exx			;4
	dec e		;4
;	and	1
	out	($90), a
	jr nz,soundLoop	;10=153t
	dec d		;4
	jr nz,soundLoop	;10
	
	endif
	

;	xor a
;	ld (26624), a

;	and	1
	out	($90), a

	ld (.prevHL),hl

;	in a,($1f)
;	and $1f
;	ld c,a
;	in a,($fe)
;	cpl
;.checkKempston equ $
;	or c
;	and $1f
;	jp z,nextRow
	jp nextRow

stopPlayer
.prevSP equ $+1
	ld sp,0
	pop hl
	exx
	pop iy
	ei
	ret

drumSettings
	db $01,$01	;tone,highest
	db $01,$02
	db $01,$04
	db $01,$08
	db $01,$20
	db $20,$04
	db $40,$04
	db $40,$08	;lowest
	db $04,$80	;special
	db $08,$80
	db $10,$80
	db $10,$02
	db $20,$02
	db $40,$02
	db $16,$01	;noise,highest
	db $16,$02
	db $16,$04
	db $16,$08
	db $16,$10
	db $00,$01
	db $00,$02
	db $00,$04
	db $00,$08
	db $00,$10





musicData:


; *** Song layout ***
LOOPSTART:            DEFW      PAT0
                      DEFW      PAT1
                      DEFW      PAT0
                      DEFW      PAT2
                      DEFW      PAT3
                      DEFW      PAT4
                      DEFW      PAT5
                      DEFW      PAT6
                      DEFW      PAT7
                      DEFW      PAT8
                      DEFW      PAT9
                      DEFW      PAT8
                      DEFW      PAT10
                      DEFW      PAT9
                      DEFW      PAT11
                      DEFW      PAT8
                      DEFW      PAT12
                      DEFW      PAT13
                      DEFW      PAT14
                      DEFW      PAT15
                      DEFW      PAT16
                      DEFW      PAT0
                      DEFW      PAT2
                      DEFW      PAT3
                      DEFW      PAT4
                      DEFW      PAT3
                      DEFW      PAT0
                      DEFW      PAT4
                      DEFW      PAT15
                      DEFW      PAT16
                      DEFW      $0000
                      DEFW      LOOPSTART

; *** Patterns ***
PAT0:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $03,$B0,$EC,$80,$76,$A1,$D8
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$82,$52,$01
                DEFB      $00    ,$82,$C3,$01
                DEFB      $01    ,$83,$B0,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $B1,$76,$80,$9D,$A2,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $04,$B0,$EC,$80,$76,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$84,$A5,$01
                DEFB      $00    ,$85,$86,$01
                DEFB      $01    ,$87,$60,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$B0,$EC,$80,$76,$A3,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$B1,$76,$80,$9D,$82,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$B0,$C6,$80,$76,$82,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT1:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $03,$B0,$C6,$80,$63,$A3,$1A
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$81,$8D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$80,$63,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$81,$8D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $04,$B1,$3B,$80,$76,$A1,$D8
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$B1,$3B,$80,$76,$A1,$D8
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$B1,$18,$80,$69,$A1,$A4
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$B0,$D2,$80,$69,$A1,$3B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$B0,$84,$01    ,$A1,$08
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT2:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $03,$B1,$29,$80,$6F,$A1,$BD
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$80,$76,$A1,$D8
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $04,$01    ,$80,$6F,$A1,$BD
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$80,$63,$A1,$8D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$93,$B0,$01
                DEFB  $03,$B0,$BB,$94,$E7,$A1,$76
                DEFB      $01    ,$95,$DB,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$01    ,$80,$69,$A1,$3B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$01    ,$80,$63,$A1,$29
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$01    ,$80,$69,$A1,$3B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT3:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $03,$B0,$C6,$80,$63,$81,$8D
                DEFB      $B0,$FA,$01    ,$01
                DEFB      $B1,$29,$01    ,$01
                DEFB      $B0,$FA,$01    ,$01
                DEFB      $B0,$DE,$01    ,$01
                DEFB      $B0,$FA,$01    ,$01
                DEFB      $B1,$29,$80,$69,$A2,$76
                DEFB      $B0,$FA,$01    ,$01
                DEFB      $B0,$C6,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $04,$01    ,$80,$63,$A1,$29
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$80,$69,$A1,$3B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$B1,$76,$01    ,$B1,$76
                DEFB      $B1,$D8,$01    ,$01
                DEFB      $B2,$31,$01    ,$01
                DEFB      $B1,$D8,$01    ,$01
                DEFB      $B1,$76,$01    ,$01
                DEFB      $B1,$D8,$01    ,$01
                DEFB  $05,$B2,$31,$80,$69,$81,$3B
                DEFB      $B1,$D8,$01    ,$01
                DEFB      $B1,$76,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$01    ,$80,$63,$81,$29
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$01    ,$80,$69,$81,$3B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT4:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $03,$B0,$DE,$80,$6F,$81,$BD
                DEFB      $B1,$18,$01    ,$01
                DEFB      $B0,$DE,$01    ,$01
                DEFB      $B1,$4D,$01    ,$01
                DEFB      $B0,$DE,$01    ,$01
                DEFB      $B1,$18,$01    ,$01
                DEFB      $B0,$B0,$80,$76,$81,$D8
                DEFB      $B1,$29,$01    ,$01
                DEFB      $B0,$B0,$01    ,$01
                DEFB      $B0,$EC,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $04,$01    ,$80,$6F,$81,$BD
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$80,$63,$81,$8D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A2,$ED,$01
                DEFB  $03,$B1,$3B,$B3,$B0,$91,$76
                DEFB      $B0,$C6,$C4,$63,$01
                DEFB      $B0,$EC,$01    ,$01
                DEFB      $B1,$3B,$01    ,$01
                DEFB      $B0,$EC,$01    ,$01
                DEFB      $B0,$C6,$01    ,$01
                DEFB  $05,$B0,$9D,$C0,$69,$91,$3B
                DEFB      $B0,$C6,$01    ,$01
                DEFB      $B0,$EC,$01    ,$01
                DEFB      $B1,$3B,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$01    ,$C0,$63,$91,$29
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$01    ,$C0,$69,$91,$3B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT5:
                DEFW  3078     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $03,$B0,$A6,$91,$4D,$91,$4D
                DEFB      $01    ,$00    ,$00
                DEFB      $01    ,$91,$4D,$92,$9B
                DEFB      $B1,$76,$00    ,$00
                DEFB  $04,$01    ,$90,$63,$93,$1A
                DEFB      $01    ,$01    ,$93,$7B
                DEFB      $B2,$76,$90,$69,$93,$B0
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$01    ,$90,$5D,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$01    ,$90,$69,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$B0,$9D,$90,$63,$93,$7B
                DEFB      $B0,$D2,$01    ,$01
                DEFB  $05,$B0,$FA,$90,$63,$93,$1A
                DEFB      $01    ,$90,$DE,$01
                DEFB  $FF  ; End of Pattern

PAT6:
                DEFW  3078     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $03,$80,$A6,$91,$4D,$91,$4D
                DEFB      $01    ,$00    ,$00
                DEFB      $01    ,$91,$4D,$92,$9B
                DEFB      $81,$76,$00    ,$00
                DEFB  $04,$01    ,$90,$63,$93,$1A
                DEFB      $01    ,$01    ,$93,$7B
                DEFB      $82,$76,$90,$69,$93,$B0
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$01    ,$90,$5D,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$01    ,$90,$69,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$80,$9D,$90,$63,$93,$7B
                DEFB      $80,$D2,$01    ,$01
                DEFB  $05,$80,$FA,$90,$63,$93,$1A
                DEFB      $01    ,$90,$DE,$01
                DEFB  $FF  ; End of Pattern

PAT7:
                DEFW  3078     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $03,$80,$A6,$81,$BD,$82,$9B
                DEFB      $01    ,$80,$DE,$82,$11
                DEFB      $01    ,$80,$6F,$81,$BD
                DEFB      $01    ,$01    ,$82,$9B
                DEFB  $04,$01    ,$80,$63,$83,$1A
                DEFB      $01    ,$01    ,$83,$7B
                DEFB      $82,$76,$91,$D8,$83,$B0
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$01    ,$90,$5D,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$01    ,$90,$69,$83,$7B
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$80,$9D,$90,$63,$83,$7B
                DEFB      $80,$D2,$01    ,$01
                DEFB  $05,$80,$FA,$90,$63,$83,$1A
                DEFB      $01    ,$90,$DE,$01
                DEFB  $FF  ; End of Pattern

PAT8:
                DEFW  3078     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$B2,$31,$90,$8C,$94,$63
                DEFB      $B1,$A4,$81,$18,$01
                DEFB      $B1,$61,$90,$8C,$01
                DEFB      $B1,$A4,$81,$18,$01
                DEFB  $0D,$B2,$31,$90,$8C,$01
                DEFB      $B1,$A4,$81,$18,$01
                DEFB      $B1,$61,$90,$8C,$01
                DEFB      $B1,$A4,$81,$18,$92,$31
                DEFB  $09,$B2,$31,$90,$8C,$01
                DEFB      $B1,$A4,$81,$18,$93,$49
                DEFB      $B1,$61,$92,$31,$01
                DEFB      $B1,$A4,$81,$18,$94,$63
                DEFB  $0D,$B1,$18,$90,$8C,$01
                DEFB      $00    ,$81,$18,$83,$49
                DEFB  $04,$B1,$A4,$90,$8C,$01
                DEFB  $05,$B1,$F4,$80,$9D,$01
                DEFB  $FF  ; End of Pattern

PAT9:
                DEFW  3078     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $0A,$81,$18,$80,$8C,$84,$63
                DEFB      $01    ,$81,$18,$01
                DEFB      $01    ,$80,$8C,$00
                DEFB      $01    ,$81,$18,$85,$37
                DEFB  $07,$01    ,$80,$84,$85,$DB
                DEFB      $01    ,$81,$08,$01
                DEFB      $01    ,$80,$84,$01
                DEFB      $01    ,$81,$08,$85,$37
                DEFB  $09,$01    ,$80,$8C,$01
                DEFB  $07,$01    ,$80,$D2,$96,$92
                DEFB  $07,$01    ,$00    ,$00
                DEFB  $07,$01    ,$80,$D2,$86,$92
                DEFB  $08,$81,$18,$80,$8C,$01
                DEFB  $08,$00    ,$80,$D2,$82,$31
                DEFB  $04,$81,$A4,$80,$8C,$83,$49
                DEFB  $05,$81,$F4,$80,$FA,$83,$E8
                DEFB  $FF  ; End of Pattern

PAT10:
                DEFW  3078     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $0A,$81,$18,$80,$8C,$84,$63
                DEFB      $01    ,$81,$18,$01
                DEFB      $01    ,$80,$8C,$00
                DEFB      $01    ,$81,$18,$85,$37
                DEFB  $07,$01    ,$80,$84,$85,$DB
                DEFB      $01    ,$81,$08,$01
                DEFB      $01    ,$80,$84,$01
                DEFB      $01    ,$81,$08,$85,$37
                DEFB  $09,$01    ,$80,$8C,$01
                DEFB  $07,$01    ,$80,$D2,$C6,$92
                DEFB      $00    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $08,$01    ,$01    ,$01
                DEFB  $04,$01    ,$01    ,$01
                DEFB  $05,$01    ,$01    ,$01
                DEFB      $00    ,$80,$D2,$C2,$31
                DEFB      $81,$A4,$80,$8C,$C3,$49
                DEFB      $81,$F4,$80,$FA,$C3,$E8
                DEFB  $FF  ; End of Pattern

PAT11:
                DEFW  3078     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $0A,$81,$18,$80,$8C,$84,$63
                DEFB      $01    ,$81,$18,$01
                DEFB      $01    ,$80,$8C,$00
                DEFB      $01    ,$81,$18,$85,$37
                DEFB  $07,$01    ,$80,$84,$85,$DB
                DEFB      $01    ,$81,$08,$01
                DEFB      $01    ,$80,$84,$01
                DEFB      $01    ,$81,$08,$85,$37
                DEFB  $09,$01    ,$80,$8C,$01
                DEFB  $07,$01    ,$80,$D2,$86,$92
                DEFB  $07,$01    ,$80,$8C,$00
                DEFB  $07,$01    ,$80,$D2,$86,$92
                DEFB  $08,$81,$18,$80,$8C,$01
                DEFB  $08,$00    ,$80,$D2,$86,$92
                DEFB  $04,$81,$A4,$80,$8C,$01
                DEFB  $05,$81,$F4,$80,$FA,$86,$92
                DEFB  $FF  ; End of Pattern

PAT12:
                DEFW  3078     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $03,$B0,$C6,$80,$63,$C3,$1A
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $04,$B0,$EC,$80,$76,$C3,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$B1,$3B,$80,$76,$C1,$D8
                DEFB      $00    ,$01    ,$01
                DEFB  $05,$B1,$18,$80,$69,$C1,$A4
                DEFB      $00    ,$01    ,$01
                DEFB  $03,$B0,$D2,$80,$69,$C1,$3B
                DEFB      $01    ,$01    ,$01
                DEFB      $B0,$84,$01    ,$C1,$08
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT13:
                DEFW  3078     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $03,$B0,$C6,$80,$63,$C3,$1A
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $04,$B0,$EC,$80,$76,$C3,$B0
                DEFB      $00    ,$00    ,$01
                DEFB      $01    ,$01    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$B1,$3B,$80,$76,$C1,$D8
                DEFB      $00    ,$01    ,$00
                DEFB  $05,$B1,$18,$80,$76,$C3,$B0
                DEFB      $00    ,$01    ,$00
                DEFB  $03,$B0,$D2,$80,$69,$C1,$3B
                DEFB      $01    ,$01    ,$01
                DEFB      $B0,$84,$80,$84,$C1,$08
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT14:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $03,$81,$3B,$80,$76,$83,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$00
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$18,$80,$69,$83,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$00
                DEFB  $04,$00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $07,$80,$D2,$80,$69,$83,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$00
                DEFB  $07,$00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $04,$81,$18,$80,$8C,$83,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT15:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $03,$81,$3B,$80,$76,$83,$B0
                DEFB      $01    ,$01    ,$85,$86
                DEFB      $01    ,$01    ,$84,$A5
                DEFB      $00    ,$01    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$81,$3B,$80,$76,$83,$B0
                DEFB      $01    ,$01    ,$85,$86
                DEFB      $01    ,$01    ,$84,$A5
                DEFB      $00    ,$01    ,$83,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$80,$D2,$80,$69,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$80,$76,$82,$76,$83,$B0
                DEFB      $01    ,$01    ,$85,$86
                DEFB      $01    ,$01    ,$84,$A5
                DEFB      $01    ,$01    ,$83,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT16:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$00    ,$82,$76,$00
                DEFB      $01    ,$81,$A4,$01
                DEFB      $01    ,$81,$3B,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern


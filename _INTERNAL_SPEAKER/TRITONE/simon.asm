
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

; ************************************************************************
; * Song data...
; ************************************************************************
BORDER_CLR:          EQU $0

; *** DATA ***

; *** Song layout ***
                      DEFW      PAT0
LOOPSTART:            DEFW      PAT1
                      DEFW      PAT1
                      DEFW      PAT1
                      DEFW      PAT1
                      DEFW      PAT1
                      DEFW      PAT1
                      DEFW      PAT1
                      DEFW      PAT2
                      DEFW      PAT1
                      DEFW      PAT1
                      DEFW      PAT1
                      DEFW      PAT1
                      DEFW      PAT1
                      DEFW      PAT1
                      DEFW      PAT2
                      DEFW      PAT3
                      DEFW      PAT4
                      DEFW      PAT5
                      DEFW      PAT6
                      DEFW      PAT7
                      DEFW      PAT8
                      DEFW      PAT9
                      DEFW      PAT10
                      DEFW      PAT11
                      DEFW      PAT4
                      DEFW      PAT5
                      DEFW      PAT6
                      DEFW      PAT7
                      DEFW      PAT8
                      DEFW      PAT9
                      DEFW      PAT10
                      DEFW      PAT11
                      DEFW      PAT12
                      DEFW      PAT13
                      DEFW      PAT14
                      DEFW      PAT15
                      DEFW      PAT16
                      DEFW      PAT17
                      DEFW      PAT18
                      DEFW      PAT19
                      DEFW      PAT12
                      DEFW      PAT13
                      DEFW      PAT14
                      DEFW      PAT15
                      DEFW      PAT20
                      DEFW      PAT21
                      DEFW      PAT22
                      DEFW      PAT23
                      DEFW      PAT24
                      DEFW      PAT25
                      DEFW      PAT26
                      DEFW      PAT25
                      DEFW      PAT24
                      DEFW      PAT25
                      DEFW      PAT26
                      DEFW      PAT27
                      DEFW      PAT28
                      DEFW      PAT27
                      DEFW      PAT29
                      DEFW      PAT30
                      DEFW      PAT31
                      DEFW      PAT30
                      DEFW      PAT32
                      DEFW      PAT33
                      DEFW      PAT34
                      DEFW      PAT35
                      DEFW      PAT36
                      DEFW      PAT35
                      DEFW      PAT34
                      DEFW      PAT35
                      DEFW      PAT1
                      DEFW      PAT37
                      DEFW      PAT1
                      DEFW      PAT38
                      DEFW      $0000
                      DEFW      LOOPSTART

; *** Patterns ***
PAT0:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$B4,$EC,$82,$ED,$86,$92
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $B0,$D2,$90,$A6,$00
                DEFB  $04,$B4,$EC,$82,$ED,$86,$92
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $B0,$D2,$90,$A6,$00
                DEFB      $86,$92,$B0,$D2,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $04,$B4,$63,$82,$ED,$85,$DB
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $01    ,$90,$A6,$01
                DEFB  $10,$B1,$76,$84,$A5,$00
                DEFB      $01    ,$83,$7B,$01
                DEFB      $85,$DB,$82,$76,$01
                DEFB      $01    ,$82,$31,$01
                DEFB  $04,$01    ,$82,$ED,$01
                DEFB      $01    ,$82,$31,$01
                DEFB  $04,$B0,$BB,$82,$ED,$01
                DEFB      $01    ,$82,$31,$01
                DEFB  $10,$B4,$63,$82,$ED,$85,$86
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $01    ,$90,$A6,$01
                DEFB      $01    ,$B0,$B0,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$B0,$A6,$01
                DEFB      $01    ,$B0,$9D,$01
                DEFB      $01    ,$B0,$94,$01
                DEFB      $01    ,$B0,$8C,$01
                DEFB      $01    ,$B0,$84,$01
                DEFB      $01    ,$B0,$7D,$01
                DEFB      $01    ,$B0,$76,$01
                DEFB      $01    ,$B0,$6F,$01
                DEFB      $01    ,$B0,$69,$01
                DEFB      $01    ,$B0,$63,$01
                DEFB      $01    ,$B0,$5D,$01
                DEFB      $01    ,$B0,$58,$01
                DEFB      $B4,$23,$B0,$53,$85,$37
                DEFB      $B4,$63,$B0,$4E,$85,$86
                DEFB      $B4,$A5,$B0,$4A,$85,$DB
                DEFB      $B4,$63,$B0,$46,$85,$86
                DEFB      $B4,$23,$B0,$42,$85,$37
                DEFB      $B4,$63,$B0,$3E,$85,$86
                DEFB      $B4,$A5,$B0,$3B,$85,$DB
                DEFB      $B4,$63,$B0,$37,$85,$86
                DEFB      $B4,$23,$B0,$34,$85,$37
                DEFB      $B4,$63,$B0,$31,$85,$86
                DEFB      $B4,$A5,$B0,$2E,$85,$DB
                DEFB      $B4,$63,$00    ,$85,$86
                DEFB      $B4,$23,$01    ,$85,$37
                DEFB      $B4,$63,$01    ,$85,$86
                DEFB      $B4,$A5,$01    ,$85,$DB
                DEFB      $B4,$63,$01    ,$85,$86
                DEFB  $FF  ; End of Pattern

PAT1:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$80,$BB,$82,$ED,$F1,$76
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $80,$B0,$91,$4D,$F1,$61
                DEFB  $10,$80,$A6,$B0,$BB,$F1,$4D
                DEFB      $80,$9D,$01    ,$F1,$3B
                DEFB  $10,$80,$94,$01    ,$F1,$29
                DEFB      $80,$8C,$01    ,$F1,$18
                DEFB  $FF  ; End of Pattern

PAT2:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$80,$BB,$82,$ED,$F1,$76
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $80,$B0,$91,$4D,$F1,$61
                DEFB  $10,$80,$A6,$B0,$BB,$F1,$4D
                DEFB      $80,$9D,$01    ,$F1,$3B
                DEFB  $04,$80,$BB,$82,$ED,$F1,$76
                DEFB      $01    ,$82,$31,$01
                DEFB  $FF  ; End of Pattern

PAT3:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$80,$BB,$82,$ED,$F1,$76
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $80,$B0,$91,$4D,$F1,$61
                DEFB  $04,$80,$BB,$82,$ED,$F1,$76
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $80,$B0,$91,$4D,$F1,$61
                DEFB  $FF  ; End of Pattern

PAT4:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$A2,$31,$82,$ED,$84,$63
                DEFB      $F1,$76,$82,$31,$00
                DEFB      $A3,$49,$81,$8D,$86,$92
                DEFB      $F1,$61,$91,$4D,$00
                DEFB  $10,$A4,$63,$B0,$BB,$8A,$6E
                DEFB      $F1,$3B,$B0,$A6,$00
                DEFB  $10,$A3,$49,$B0,$94,$86,$92
                DEFB      $F1,$18,$B0,$84,$00
                DEFB  $FF  ; End of Pattern

PAT5:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$A3,$E8,$82,$ED,$89,$D9
                DEFB      $F1,$76,$82,$31,$01
                DEFB      $A3,$49,$81,$8D,$86,$92
                DEFB      $F1,$61,$91,$4D,$00
                DEFB  $10,$A3,$B0,$B0,$BB,$88,$C6
                DEFB      $F1,$3B,$B0,$A6,$01
                DEFB  $10,$A3,$49,$B0,$94,$86,$92
                DEFB      $F1,$18,$B0,$84,$00
                DEFB  $FF  ; End of Pattern

PAT6:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$A3,$49,$82,$ED,$84,$63
                DEFB      $F1,$76,$82,$31,$00
                DEFB      $A3,$49,$81,$8D,$86,$92
                DEFB      $F1,$61,$91,$4D,$00
                DEFB  $10,$A3,$E8,$B0,$BB,$89,$D9
                DEFB      $F1,$3B,$B0,$A6,$00
                DEFB  $10,$A3,$49,$B0,$94,$86,$92
                DEFB      $F1,$18,$B0,$84,$00
                DEFB  $FF  ; End of Pattern

PAT7:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$A3,$B0,$82,$ED,$88,$C6
                DEFB      $F1,$76,$82,$31,$01
                DEFB      $A3,$49,$81,$8D,$86,$92
                DEFB      $F1,$61,$91,$4D,$00
                DEFB  $10,$A3,$49,$B0,$BB,$87,$D0
                DEFB      $F1,$3B,$B0,$A6,$01
                DEFB  $10,$A3,$49,$B0,$94,$86,$92
                DEFB      $F1,$18,$B0,$84,$00
                DEFB  $FF  ; End of Pattern

PAT8:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$A3,$1A,$82,$ED,$87,$60
                DEFB      $F1,$76,$82,$31,$00
                DEFB      $A2,$76,$81,$8D,$84,$EC
                DEFB      $F1,$61,$91,$4D,$00
                DEFB  $10,$A3,$B0,$B0,$BB,$84,$63
                DEFB      $F1,$3B,$B0,$A6,$00
                DEFB  $10,$A2,$9B,$B0,$94,$84,$EC
                DEFB      $F1,$18,$B0,$84,$00
                DEFB  $FF  ; End of Pattern

PAT9:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$A3,$49,$82,$ED,$87,$D0
                DEFB      $F1,$76,$82,$31,$01
                DEFB      $A3,$49,$81,$8D,$84,$EC
                DEFB      $F1,$61,$91,$4D,$00
                DEFB  $10,$A3,$1A,$B0,$BB,$87,$60
                DEFB      $F1,$3B,$B0,$A6,$01
                DEFB  $10,$A3,$49,$B0,$94,$84,$EC
                DEFB      $F1,$18,$B0,$84,$00
                DEFB  $FF  ; End of Pattern

PAT10:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$A3,$49,$82,$ED,$86,$92
                DEFB      $F1,$76,$82,$31,$00
                DEFB      $A2,$76,$81,$8D,$84,$EC
                DEFB      $F1,$61,$91,$4D,$00
                DEFB  $10,$A5,$86,$B0,$BB,$85,$86
                DEFB      $F1,$3B,$B0,$A6,$00
                DEFB  $10,$A3,$1A,$B0,$94,$86,$34
                DEFB      $F1,$18,$B0,$84,$00
                DEFB  $FF  ; End of Pattern

PAT11:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$A3,$49,$82,$ED,$86,$92
                DEFB      $F1,$76,$82,$31,$00
                DEFB      $A3,$B0,$81,$8D,$87,$60
                DEFB      $F1,$61,$91,$4D,$00
                DEFB  $10,$A3,$E8,$B0,$BB,$87,$D0
                DEFB      $F1,$3B,$B0,$A6,$00
                DEFB  $04,$A4,$EC,$82,$ED,$89,$D9
                DEFB      $F1,$76,$82,$31,$00
                DEFB  $FF  ; End of Pattern

PAT12:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $A2,$31,$A2,$31,$84,$63
                DEFB      $01    ,$00    ,$00
                DEFB      $A3,$49,$A3,$49,$86,$92
                DEFB      $01    ,$00    ,$00
                DEFB      $A4,$63,$A4,$63,$8A,$6E
                DEFB      $01    ,$00    ,$00
                DEFB      $A3,$49,$A3,$49,$86,$92
                DEFB      $01    ,$00    ,$00
                DEFB  $FF  ; End of Pattern

PAT13:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $A3,$E8,$A3,$E8,$89,$D9
                DEFB      $01    ,$00    ,$01
                DEFB      $A3,$49,$A3,$49,$86,$92
                DEFB      $01    ,$00    ,$00
                DEFB      $A3,$B0,$A3,$B0,$88,$C6
                DEFB      $01    ,$00    ,$01
                DEFB      $A3,$49,$A3,$49,$86,$92
                DEFB      $01    ,$00    ,$00
                DEFB  $FF  ; End of Pattern

PAT14:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $A3,$49,$A3,$49,$84,$63
                DEFB      $01    ,$00    ,$00
                DEFB      $A3,$49,$A3,$49,$86,$92
                DEFB      $01    ,$00    ,$00
                DEFB      $A3,$E8,$A3,$E8,$89,$D9
                DEFB      $01    ,$00    ,$00
                DEFB      $A3,$49,$A3,$49,$86,$92
                DEFB      $01    ,$00    ,$00
                DEFB  $FF  ; End of Pattern

PAT15:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $A3,$B0,$A3,$B0,$88,$C6
                DEFB      $01    ,$00    ,$01
                DEFB      $A3,$49,$A3,$49,$86,$92
                DEFB      $01    ,$00    ,$00
                DEFB      $A3,$49,$A3,$49,$87,$D0
                DEFB      $01    ,$00    ,$01
                DEFB      $A3,$49,$A3,$49,$86,$92
                DEFB      $01    ,$00    ,$00
                DEFB  $FF  ; End of Pattern

PAT16:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $A3,$1A,$A3,$1A,$87,$60
                DEFB      $01    ,$00    ,$00
                DEFB      $A2,$76,$A2,$9B,$84,$EC
                DEFB      $01    ,$00    ,$00
                DEFB      $A3,$B0,$A3,$B0,$84,$63
                DEFB      $01    ,$00    ,$00
                DEFB      $A2,$9B,$A2,$9B,$84,$EC
                DEFB      $01    ,$00    ,$00
                DEFB  $FF  ; End of Pattern

PAT17:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $A3,$49,$A3,$49,$87,$D0
                DEFB      $01    ,$00    ,$01
                DEFB      $A3,$49,$A3,$49,$84,$EC
                DEFB      $01    ,$00    ,$00
                DEFB      $A3,$1A,$A3,$1A,$87,$60
                DEFB      $01    ,$00    ,$01
                DEFB      $A3,$49,$A3,$49,$84,$EC
                DEFB      $01    ,$00    ,$00
                DEFB  $FF  ; End of Pattern

PAT18:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $A3,$49,$A3,$49,$86,$92
                DEFB      $01    ,$00    ,$00
                DEFB      $A2,$76,$A2,$76,$84,$EC
                DEFB      $01    ,$00    ,$00
                DEFB      $A5,$86,$A5,$86,$85,$86
                DEFB      $01    ,$00    ,$00
                DEFB      $A3,$1A,$A3,$1A,$86,$34
                DEFB      $01    ,$00    ,$00
                DEFB  $FF  ; End of Pattern

PAT19:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $A3,$49,$A3,$49,$86,$92
                DEFB      $01    ,$00    ,$00
                DEFB      $A3,$B0,$A3,$B0,$87,$60
                DEFB      $01    ,$00    ,$00
                DEFB      $A3,$E8,$A3,$E8,$87,$D0
                DEFB      $01    ,$00    ,$00
                DEFB      $A4,$EC,$A4,$EC,$89,$D9
                DEFB      $01    ,$00    ,$00
                DEFB  $FF  ; End of Pattern

PAT20:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $A3,$B0,$A3,$B0,$87,$60
                DEFB      $01    ,$00    ,$00
                DEFB      $A3,$7B,$A3,$7B,$86,$F6
                DEFB      $01    ,$00    ,$00
                DEFB      $A3,$B0,$A3,$B0,$87,$60
                DEFB      $01    ,$00    ,$00
                DEFB      $A3,$E8,$A3,$E8,$87,$D0
                DEFB      $01    ,$00    ,$00
                DEFB  $FF  ; End of Pattern

PAT21:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $A4,$63,$A4,$63,$88,$C6
                DEFB      $01    ,$00    ,$00
                DEFB      $A4,$EC,$A4,$EC,$89,$D9
                DEFB      $01    ,$00    ,$00
                DEFB      $A5,$86,$A5,$86,$8B,$0D
                DEFB      $01    ,$00    ,$00
                DEFB      $A6,$34,$A6,$34,$8C,$68
                DEFB      $01    ,$00    ,$00
                DEFB  $FF  ; End of Pattern

PAT22:
                DEFW  2464     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $A6,$92,$A6,$92,$8D,$25
                DEFB      $00    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F0,$D2,$F1,$A4,$F3,$49
                DEFB      $00    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT23:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$82,$ED,$85,$DB,$86,$92
                DEFB  $05,$82,$31,$84,$63,$00
                DEFB  $07,$81,$8D,$83,$1A,$A6,$92
                DEFB      $91,$4D,$92,$9B,$00
                DEFB  $10,$81,$A4,$B0,$D2,$01
                DEFB      $91,$A4,$01    ,$01
                DEFB  $10,$A1,$A4,$01    ,$E5,$DB
                DEFB      $00    ,$01    ,$F5,$DB
                DEFB  $04,$81,$A4,$84,$63,$00
                DEFB      $91,$A4,$83,$49,$01
                DEFB      $A1,$A4,$82,$52,$D5,$37
                DEFB      $00    ,$91,$4D,$C5,$86
                DEFB  $10,$81,$D8,$B0,$EC,$B5,$86
                DEFB      $91,$D8,$01    ,$A5,$86
                DEFB  $10,$A1,$D8,$01    ,$95,$86
                DEFB      $00    ,$01    ,$85,$86
                DEFB  $04,$81,$F4,$82,$ED,$85,$86
                DEFB      $91,$F4,$82,$31,$95,$86
                DEFB      $A1,$F4,$81,$8D,$A5,$86
                DEFB      $00    ,$91,$4D,$B5,$37
                DEFB  $10,$81,$F4,$B0,$FA,$C4,$EC
                DEFB      $91,$F4,$01    ,$D4,$A5
                DEFB  $10,$A1,$F4,$01    ,$E3,$49
                DEFB      $00    ,$01    ,$F3,$49
                DEFB  $04,$81,$F4,$84,$63,$F3,$49
                DEFB      $91,$F4,$83,$49,$00
                DEFB      $A1,$F4,$82,$52,$D3,$B0
                DEFB      $00    ,$91,$4D,$00
                DEFB  $10,$82,$31,$B0,$BB,$B3,$E8
                DEFB      $92,$31,$01    ,$A3,$E8
                DEFB  $10,$A2,$31,$01    ,$94,$63
                DEFB      $00    ,$01    ,$00
                DEFB  $FF  ; End of Pattern

PAT24:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$82,$76,$82,$ED,$84,$A5
                DEFB      $92,$76,$82,$31,$94,$EC
                DEFB      $A2,$76,$81,$8D,$A4,$EC
                DEFB      $00    ,$91,$4D,$B4,$EC
                DEFB  $10,$82,$76,$B0,$D2,$C4,$63
                DEFB      $92,$76,$01    ,$D3,$E8
                DEFB  $10,$A2,$76,$01    ,$E3,$7B
                DEFB      $00    ,$01    ,$00
                DEFB  $04,$82,$76,$84,$63,$F3,$49
                DEFB      $92,$76,$83,$49,$E3,$49
                DEFB      $A2,$76,$82,$52,$D3,$49
                DEFB      $00    ,$91,$4D,$C3,$49
                DEFB  $10,$83,$49,$B0,$D2,$B3,$49
                DEFB      $93,$49,$01    ,$A3,$49
                DEFB  $10,$A3,$49,$01    ,$94,$EC
                DEFB      $00    ,$01    ,$00
                DEFB  $04,$82,$76,$82,$ED,$84,$63
                DEFB      $92,$76,$82,$31,$01
                DEFB      $A2,$76,$81,$8D,$A5,$DB
                DEFB      $00    ,$91,$4D,$B5,$DB
                DEFB  $10,$82,$52,$B0,$BB,$00
                DEFB      $92,$52,$01    ,$01
                DEFB  $10,$A2,$52,$01    ,$E5,$86
                DEFB      $00    ,$01    ,$F5,$DB
                DEFB  $04,$82,$31,$84,$63,$F5,$DB
                DEFB      $92,$31,$83,$49,$E5,$DB
                DEFB      $A2,$31,$82,$52,$D5,$DB
                DEFB      $00    ,$91,$4D,$C5,$DB
                DEFB  $10,$81,$F4,$B1,$18,$B6,$34
                DEFB      $91,$F4,$01    ,$A5,$DB
                DEFB  $10,$A1,$F4,$01    ,$95,$86
                DEFB      $00    ,$01    ,$85,$DB
                DEFB  $FF  ; End of Pattern

PAT25:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$81,$A4,$82,$ED,$86,$92
                DEFB      $91,$A4,$82,$31,$00
                DEFB      $A1,$A4,$81,$8D,$A6,$92
                DEFB      $00    ,$91,$4D,$00
                DEFB  $10,$81,$A4,$B0,$D2,$01
                DEFB      $91,$A4,$01    ,$01
                DEFB  $10,$A1,$A4,$01    ,$E5,$DB
                DEFB      $00    ,$01    ,$F5,$DB
                DEFB  $04,$81,$A4,$84,$63,$00
                DEFB      $91,$A4,$83,$49,$01
                DEFB      $A1,$A4,$82,$52,$D5,$37
                DEFB      $00    ,$91,$4D,$C5,$86
                DEFB  $10,$81,$D8,$B0,$EC,$B5,$86
                DEFB      $91,$D8,$01    ,$A5,$86
                DEFB  $10,$A1,$D8,$01    ,$95,$86
                DEFB      $00    ,$01    ,$85,$86
                DEFB  $04,$81,$F4,$82,$ED,$85,$86
                DEFB      $91,$F4,$82,$31,$95,$86
                DEFB      $A1,$F4,$81,$8D,$A5,$86
                DEFB      $00    ,$91,$4D,$B5,$37
                DEFB  $10,$81,$F4,$B0,$FA,$C4,$EC
                DEFB      $91,$F4,$01    ,$D4,$A5
                DEFB  $10,$A1,$F4,$01    ,$E3,$49
                DEFB      $00    ,$01    ,$F3,$49
                DEFB  $04,$81,$F4,$84,$63,$F3,$49
                DEFB      $91,$F4,$83,$49,$00
                DEFB      $A1,$F4,$82,$52,$D3,$B0
                DEFB      $00    ,$91,$4D,$00
                DEFB  $10,$82,$31,$B0,$BB,$B3,$E8
                DEFB      $92,$31,$01    ,$A3,$E8
                DEFB  $10,$A2,$31,$01    ,$94,$63
                DEFB      $00    ,$01    ,$00
                DEFB  $FF  ; End of Pattern

PAT26:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$82,$76,$82,$ED,$84,$A5
                DEFB      $92,$76,$82,$31,$94,$EC
                DEFB      $A2,$76,$81,$8D,$A4,$EC
                DEFB      $00    ,$91,$4D,$B4,$EC
                DEFB  $10,$82,$76,$B0,$D2,$C4,$63
                DEFB      $92,$76,$01    ,$D3,$E8
                DEFB  $10,$A2,$76,$01    ,$E3,$7B
                DEFB      $00    ,$01    ,$00
                DEFB  $04,$82,$76,$84,$63,$F3,$49
                DEFB      $92,$76,$83,$49,$E3,$49
                DEFB      $A2,$76,$82,$52,$D3,$49
                DEFB      $00    ,$91,$4D,$C3,$49
                DEFB  $10,$83,$49,$B0,$D2,$B3,$49
                DEFB      $93,$49,$01    ,$A3,$49
                DEFB  $10,$A3,$49,$01    ,$94,$EC
                DEFB      $00    ,$01    ,$00
                DEFB  $04,$82,$76,$82,$ED,$84,$63
                DEFB      $92,$76,$82,$31,$94,$63
                DEFB      $A2,$76,$81,$8D,$A4,$63
                DEFB      $00    ,$91,$4D,$B4,$63
                DEFB  $04,$82,$52,$82,$ED,$C4,$23
                DEFB      $92,$52,$82,$31,$D3,$E8
                DEFB      $A2,$52,$81,$8D,$E3,$B0
                DEFB      $00    ,$91,$4D,$F3,$7B
                DEFB  $04,$82,$31,$84,$63,$F3,$49
                DEFB      $92,$31,$83,$49,$E3,$1A
                DEFB      $A2,$31,$82,$52,$D2,$ED
                DEFB      $00    ,$91,$4D,$C2,$C3
                DEFB  $04,$81,$F4,$82,$ED,$B2,$9B
                DEFB      $91,$F4,$82,$31,$A2,$76
                DEFB      $A1,$F4,$81,$8D,$92,$52
                DEFB      $00    ,$91,$4D,$82,$31
                DEFB  $FF  ; End of Pattern

PAT27:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$81,$A4,$82,$ED,$00
                DEFB      $91,$A4,$82,$31,$01
                DEFB      $A1,$A4,$81,$8D,$01
                DEFB      $00    ,$91,$4D,$01
                DEFB  $10,$81,$A4,$B0,$D2,$85,$DB
                DEFB      $91,$A4,$01    ,$86,$34
                DEFB  $10,$A1,$A4,$01    ,$86,$92
                DEFB      $00    ,$01    ,$01
                DEFB  $04,$81,$A4,$84,$63,$01
                DEFB      $91,$A4,$83,$49,$01
                DEFB      $A1,$A4,$82,$52,$01
                DEFB      $00    ,$91,$4D,$01
                DEFB  $10,$81,$D8,$B0,$EC,$89,$D9
                DEFB      $91,$D8,$01    ,$01
                DEFB  $10,$A1,$D8,$01    ,$00
                DEFB      $00    ,$01    ,$01
                DEFB  $04,$81,$F4,$82,$ED,$89,$4B
                DEFB      $91,$F4,$82,$31,$01
                DEFB      $A1,$F4,$81,$8D,$89,$D9
                DEFB      $00    ,$91,$4D,$01
                DEFB  $10,$81,$F4,$B0,$FA,$89,$4B
                DEFB      $91,$F4,$01    ,$01
                DEFB  $10,$A1,$F4,$01    ,$87,$D0
                DEFB      $00    ,$01    ,$01
                DEFB  $04,$81,$F4,$84,$63,$00
                DEFB      $91,$F4,$83,$49,$01
                DEFB      $A1,$F4,$82,$52,$01
                DEFB      $00    ,$91,$4D,$01
                DEFB  $10,$82,$31,$B0,$BB,$01
                DEFB      $92,$31,$01    ,$01
                DEFB  $10,$A2,$31,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT28:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$82,$76,$82,$ED,$89,$D9
                DEFB      $92,$76,$82,$31,$01
                DEFB      $A2,$76,$81,$8D,$00
                DEFB      $00    ,$91,$4D,$01
                DEFB  $10,$82,$76,$B0,$D2,$89,$D9
                DEFB      $92,$76,$01    ,$01
                DEFB  $10,$A2,$76,$01    ,$89,$4B
                DEFB      $00    ,$01    ,$01
                DEFB  $04,$82,$76,$84,$63,$00
                DEFB      $92,$76,$83,$49,$01
                DEFB      $A2,$76,$82,$52,$87,$D0
                DEFB      $00    ,$91,$4D,$01
                DEFB  $10,$83,$49,$B0,$D2,$86,$92
                DEFB      $93,$49,$01    ,$01
                DEFB  $10,$A3,$49,$01    ,$89,$D9
                DEFB      $00    ,$01    ,$01
                DEFB  $04,$82,$76,$82,$ED,$89,$4B
                DEFB      $92,$76,$82,$31,$01
                DEFB      $A2,$76,$81,$8D,$87,$D0
                DEFB      $00    ,$91,$4D,$01
                DEFB  $10,$82,$52,$B0,$BB,$89,$D9
                DEFB      $92,$52,$01    ,$01
                DEFB  $10,$A2,$52,$01    ,$89,$4B
                DEFB      $00    ,$01    ,$01
                DEFB  $04,$82,$31,$84,$63,$00
                DEFB      $92,$31,$83,$49,$01
                DEFB      $A2,$31,$82,$52,$87,$D0
                DEFB      $00    ,$91,$4D,$01
                DEFB  $10,$81,$F4,$B1,$18,$86,$92
                DEFB      $91,$F4,$01    ,$01
                DEFB  $10,$A1,$F4,$01    ,$00
                DEFB      $00    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT29:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$82,$76,$82,$ED,$89,$D9
                DEFB      $92,$76,$82,$31,$01
                DEFB      $A2,$76,$81,$8D,$00
                DEFB      $00    ,$91,$4D,$01
                DEFB  $10,$82,$76,$B0,$D2,$89,$D9
                DEFB      $92,$76,$01    ,$01
                DEFB  $10,$A2,$76,$01    ,$89,$4B
                DEFB      $00    ,$01    ,$01
                DEFB  $04,$82,$76,$84,$63,$00
                DEFB      $92,$76,$83,$49,$01
                DEFB      $A2,$76,$82,$52,$87,$D0
                DEFB      $00    ,$91,$4D,$01
                DEFB  $10,$83,$49,$B0,$D2,$86,$92
                DEFB      $93,$49,$01    ,$01
                DEFB  $10,$A3,$49,$01    ,$89,$D9
                DEFB      $00    ,$01    ,$01
                DEFB  $04,$82,$76,$82,$ED,$89,$4B
                DEFB      $92,$76,$82,$31,$01
                DEFB      $A2,$76,$81,$8D,$87,$D0
                DEFB      $00    ,$91,$4D,$01
                DEFB  $10,$82,$52,$B0,$BB,$89,$D9
                DEFB      $92,$52,$01    ,$01
                DEFB  $10,$A2,$52,$01    ,$89,$4B
                DEFB      $00    ,$01    ,$01
                DEFB  $04,$82,$31,$84,$63,$88,$C6
                DEFB      $92,$31,$83,$49,$01
                DEFB      $A2,$31,$82,$52,$87,$D0
                DEFB      $00    ,$91,$4D,$01
                DEFB  $04,$81,$F4,$84,$63,$86,$92
                DEFB      $91,$F4,$83,$49,$01
                DEFB      $A1,$F4,$82,$52,$85,$DB
                DEFB      $00    ,$91,$4D,$01
                DEFB  $FF  ; End of Pattern

PAT30:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $F2,$31,$F2,$31,$85,$86
                DEFB      $01    ,$00    ,$85,$DB
                DEFB      $F3,$49,$F3,$49,$86,$34
                DEFB      $01    ,$00    ,$01
                DEFB  $10,$F5,$37,$F5,$37,$01
                DEFB  $0F,$01    ,$00    ,$01
                DEFB  $0E,$F3,$49,$F3,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $F4,$EC,$F4,$EC,$87,$60
                DEFB      $01    ,$00    ,$01
                DEFB      $F3,$49,$F3,$49,$01
                DEFB      $01    ,$00    ,$00
                DEFB  $10,$F4,$63,$F4,$63,$01
                DEFB  $0F,$01    ,$00    ,$01
                DEFB  $0E,$F3,$49,$F3,$49,$8A,$6E
                DEFB      $01    ,$00    ,$01
                DEFB      $F2,$31,$F2,$31,$89,$D9
                DEFB      $01    ,$00    ,$01
                DEFB      $F3,$49,$F3,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB  $10,$F4,$EC,$F4,$EC,$87,$D0
                DEFB  $0F,$01    ,$00    ,$01
                DEFB  $0E,$F3,$49,$F3,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $F4,$63,$F4,$63,$86,$92
                DEFB      $01    ,$00    ,$01
                DEFB      $F3,$49,$F3,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB  $10,$F3,$E8,$F3,$E8,$84,$EC
                DEFB  $0F,$01    ,$00    ,$01
                DEFB  $0E,$F3,$49,$F3,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB  $FF  ; End of Pattern

PAT31:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $F3,$B0,$F3,$B0,$85,$86
                DEFB      $01    ,$00    ,$85,$DB
                DEFB      $F2,$76,$F2,$76,$86,$34
                DEFB      $01    ,$00    ,$01
                DEFB  $10,$F2,$31,$F2,$31,$01
                DEFB  $0F,$01    ,$00    ,$01
                DEFB  $0E,$F2,$76,$F2,$76,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $F3,$E8,$F3,$E8,$87,$60
                DEFB      $01    ,$00    ,$01
                DEFB      $F2,$76,$F2,$76,$01
                DEFB      $01    ,$00    ,$00
                DEFB  $10,$F3,$B0,$F3,$B0,$01
                DEFB  $0F,$01    ,$00    ,$01
                DEFB  $0E,$F2,$76,$F2,$76,$8A,$6E
                DEFB      $01    ,$00    ,$01
                DEFB      $F3,$49,$F3,$49,$89,$D9
                DEFB      $01    ,$00    ,$01
                DEFB      $F2,$76,$F2,$76,$01
                DEFB      $01    ,$00    ,$01
                DEFB  $10,$F2,$C3,$F2,$C3,$01
                DEFB  $0F,$01    ,$00    ,$01
                DEFB  $0E,$F3,$1A,$F3,$1A,$89,$4B
                DEFB      $01    ,$00    ,$88,$C6
                DEFB  $10,$F3,$49,$F3,$49,$88,$47
                DEFB      $01    ,$00    ,$87,$D0
                DEFB      $F3,$B0,$F3,$B0,$87,$60
                DEFB      $01    ,$00    ,$86,$F6
                DEFB  $10,$F3,$E8,$F3,$E8,$86,$92
                DEFB      $01    ,$00    ,$85,$DB
                DEFB      $F4,$EC,$F4,$EC,$85,$37
                DEFB      $01    ,$00    ,$84,$A5
                DEFB  $FF  ; End of Pattern

PAT32:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $F3,$1A,$F3,$1A,$85,$86
                DEFB      $01    ,$00    ,$85,$DB
                DEFB      $F2,$76,$F2,$76,$86,$34
                DEFB      $01    ,$00    ,$01
                DEFB  $10,$F2,$9B,$F2,$9B,$01
                DEFB  $0F,$01    ,$00    ,$01
                DEFB  $0E,$F2,$76,$F2,$76,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $F3,$1A,$F3,$1A,$87,$60
                DEFB      $01    ,$00    ,$F3,$B0
                DEFB      $F2,$76,$F2,$76,$87,$60
                DEFB      $01    ,$00    ,$00
                DEFB  $10,$F2,$9B,$F2,$9B,$F3,$B0
                DEFB  $0F,$01    ,$00    ,$01
                DEFB  $0E,$F2,$76,$F2,$76,$F5,$37
                DEFB      $01    ,$00    ,$01
                DEFB      $F3,$1A,$F3,$1A,$F4,$EC
                DEFB      $01    ,$00    ,$01
                DEFB      $F3,$49,$F3,$49,$01
                DEFB      $01    ,$00    ,$00
                DEFB  $10,$F3,$B0,$F3,$B0,$F5,$37
                DEFB      $01    ,$00    ,$01
                DEFB      $F3,$E8,$80,$BB,$01
                DEFB      $01    ,$80,$D2,$00
                DEFB  $10,$F3,$B0,$80,$EC,$F5,$86
                DEFB      $01    ,$81,$08,$01
                DEFB  $10,$F3,$E8,$81,$29,$01
                DEFB      $01    ,$81,$4D,$00
                DEFB  $16,$F4,$63,$81,$76,$F6,$34
                DEFB      $01    ,$81,$A4,$01
                DEFB  $10,$F4,$EC,$81,$D8,$01
                DEFB      $01    ,$81,$F4,$01
                DEFB  $FF  ; End of Pattern

PAT33:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$82,$ED,$85,$DB,$86,$34
                DEFB  $05,$82,$31,$84,$63,$96,$92
                DEFB  $07,$81,$8D,$83,$1A,$A6,$92
                DEFB      $91,$4D,$92,$9B,$B6,$92
                DEFB  $10,$81,$A4,$B0,$D2,$C6,$92
                DEFB      $91,$A4,$01    ,$D6,$92
                DEFB  $10,$A1,$A4,$01    ,$E6,$92
                DEFB      $00    ,$01    ,$F6,$92
                DEFB  $04,$81,$A4,$84,$63,$00
                DEFB      $91,$A4,$83,$49,$01
                DEFB      $A1,$A4,$82,$52,$01
                DEFB      $00    ,$91,$4D,$01
                DEFB  $10,$81,$D8,$B0,$EC,$B3,$49
                DEFB      $91,$D8,$01    ,$00
                DEFB  $10,$A1,$D8,$01    ,$93,$49
                DEFB      $00    ,$01    ,$00
                DEFB  $04,$81,$F4,$82,$ED,$83,$49
                DEFB      $91,$F4,$82,$31,$93,$49
                DEFB      $A1,$F4,$81,$8D,$00
                DEFB      $00    ,$91,$4D,$01
                DEFB  $10,$81,$F4,$B0,$FA,$C3,$49
                DEFB      $91,$F4,$01    ,$D3,$49
                DEFB  $10,$A1,$F4,$01    ,$E3,$49
                DEFB      $00    ,$01    ,$00
                DEFB  $04,$82,$31,$84,$63,$F3,$49
                DEFB      $92,$31,$83,$49,$E3,$49
                DEFB      $A2,$31,$82,$52,$D3,$49
                DEFB      $00    ,$91,$4D,$C3,$49
                DEFB  $10,$82,$76,$B1,$3B,$B2,$ED
                DEFB      $92,$76,$01    ,$A2,$ED
                DEFB  $10,$A2,$76,$01    ,$92,$ED
                DEFB      $00    ,$01    ,$82,$ED
                DEFB  $FF  ; End of Pattern

PAT34:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$81,$A4,$82,$ED,$82,$9B
                DEFB      $91,$A4,$82,$31,$92,$9B
                DEFB      $A1,$A4,$81,$8D,$00
                DEFB      $00    ,$91,$4D,$01
                DEFB  $10,$81,$A4,$B0,$D2,$C2,$9B
                DEFB      $91,$A4,$01    ,$D2,$9B
                DEFB  $10,$A1,$A4,$01    ,$E2,$9B
                DEFB      $00    ,$01    ,$E2,$9B
                DEFB  $04,$81,$A4,$84,$63,$F3,$49
                DEFB      $91,$A4,$83,$49,$E3,$49
                DEFB      $A1,$A4,$82,$52,$D3,$49
                DEFB      $00    ,$91,$4D,$C3,$49
                DEFB  $10,$81,$D8,$B0,$EC,$B3,$E8
                DEFB      $91,$D8,$01    ,$A3,$E8
                DEFB  $10,$A1,$D8,$01    ,$93,$E8
                DEFB      $00    ,$01    ,$00
                DEFB  $04,$81,$F4,$82,$ED,$82,$ED
                DEFB      $91,$F4,$82,$31,$92,$ED
                DEFB      $A1,$F4,$81,$8D,$00
                DEFB      $00    ,$91,$4D,$01
                DEFB  $10,$81,$F4,$B0,$FA,$C2,$ED
                DEFB      $91,$F4,$01    ,$D2,$ED
                DEFB  $10,$A1,$F4,$01    ,$E2,$ED
                DEFB      $00    ,$01    ,$E2,$ED
                DEFB  $04,$82,$31,$84,$63,$F3,$B0
                DEFB      $92,$31,$83,$49,$E3,$B0
                DEFB      $A2,$31,$82,$52,$D3,$B0
                DEFB      $00    ,$91,$4D,$C3,$B0
                DEFB  $10,$82,$76,$B1,$3B,$B4,$63
                DEFB      $92,$76,$01    ,$A4,$63
                DEFB  $10,$A2,$76,$01    ,$94,$63
                DEFB      $00    ,$01    ,$00
                DEFB  $FF  ; End of Pattern

PAT35:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$81,$A4,$82,$ED,$86,$34
                DEFB      $91,$A4,$82,$31,$96,$92
                DEFB      $A1,$A4,$81,$8D,$A6,$92
                DEFB      $00    ,$91,$4D,$B6,$92
                DEFB  $10,$81,$A4,$B0,$D2,$C6,$92
                DEFB      $91,$A4,$01    ,$D6,$92
                DEFB  $10,$A1,$A4,$01    ,$E6,$92
                DEFB      $00    ,$01    ,$F6,$92
                DEFB  $04,$81,$A4,$84,$63,$00
                DEFB      $91,$A4,$83,$49,$01
                DEFB      $A1,$A4,$82,$52,$01
                DEFB      $00    ,$91,$4D,$01
                DEFB  $10,$81,$D8,$B0,$EC,$B3,$49
                DEFB      $91,$D8,$01    ,$00
                DEFB  $10,$A1,$D8,$01    ,$93,$49
                DEFB      $00    ,$01    ,$00
                DEFB  $04,$81,$F4,$82,$ED,$83,$49
                DEFB      $91,$F4,$82,$31,$93,$49
                DEFB      $A1,$F4,$81,$8D,$00
                DEFB      $00    ,$91,$4D,$01
                DEFB  $10,$81,$F4,$B0,$FA,$C3,$49
                DEFB      $91,$F4,$01    ,$D3,$49
                DEFB  $10,$A1,$F4,$01    ,$E3,$49
                DEFB      $00    ,$01    ,$00
                DEFB  $04,$82,$31,$84,$63,$F3,$49
                DEFB      $92,$31,$83,$49,$E3,$49
                DEFB      $A2,$31,$82,$52,$D3,$49
                DEFB      $00    ,$91,$4D,$C3,$49
                DEFB  $10,$82,$76,$B1,$3B,$B2,$ED
                DEFB      $92,$76,$01    ,$A2,$ED
                DEFB  $10,$A2,$76,$01    ,$92,$ED
                DEFB      $00    ,$01    ,$82,$ED
                DEFB  $FF  ; End of Pattern

PAT36:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$81,$A4,$82,$ED,$82,$9B
                DEFB      $91,$A4,$82,$31,$92,$9B
                DEFB      $A1,$A4,$81,$8D,$00
                DEFB      $00    ,$91,$4D,$01
                DEFB  $10,$81,$A4,$B0,$D2,$C2,$9B
                DEFB      $91,$A4,$01    ,$D2,$9B
                DEFB  $10,$A1,$A4,$01    ,$E2,$9B
                DEFB      $00    ,$01    ,$E2,$9B
                DEFB  $04,$81,$A4,$84,$63,$F3,$49
                DEFB      $91,$A4,$83,$49,$E3,$49
                DEFB      $A1,$A4,$82,$52,$D3,$49
                DEFB      $00    ,$91,$4D,$C3,$49
                DEFB  $10,$81,$D8,$B0,$EC,$B3,$E8
                DEFB      $91,$D8,$01    ,$A3,$E8
                DEFB  $10,$A1,$D8,$01    ,$93,$E8
                DEFB      $00    ,$01    ,$00
                DEFB  $04,$81,$F4,$82,$ED,$82,$ED
                DEFB      $91,$F4,$82,$31,$92,$ED
                DEFB      $A1,$F4,$81,$8D,$00
                DEFB      $00    ,$91,$4D,$01
                DEFB  $10,$81,$F4,$B0,$FA,$C2,$ED
                DEFB      $91,$F4,$01    ,$D2,$ED
                DEFB  $10,$A1,$F4,$01    ,$E2,$ED
                DEFB      $00    ,$01    ,$E2,$ED
                DEFB  $04,$82,$31,$84,$63,$F3,$B0
                DEFB      $92,$31,$83,$49,$E3,$B0
                DEFB      $A2,$31,$82,$52,$D3,$B0
                DEFB      $00    ,$91,$4D,$C3,$B0
                DEFB  $04,$82,$76,$84,$63,$B4,$63
                DEFB      $92,$76,$83,$49,$A4,$63
                DEFB      $A2,$76,$82,$52,$94,$63
                DEFB      $00    ,$91,$4D,$00
                DEFB  $FF  ; End of Pattern

PAT37:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$80,$BB,$84,$63,$F1,$76
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$82,$52,$01
                DEFB      $80,$B0,$91,$4D,$F1,$61
                DEFB  $10,$80,$A6,$B0,$BB,$F1,$4D
                DEFB      $80,$9D,$01    ,$F1,$3B
                DEFB  $10,$80,$94,$01    ,$F1,$29
                DEFB      $80,$8C,$01    ,$F1,$18
                DEFB  $FF  ; End of Pattern

PAT38:
                DEFW  1236     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$80,$BB,$84,$63,$F1,$76
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$82,$52,$01
                DEFB      $80,$B0,$91,$4D,$F1,$61
                DEFB  $10,$80,$A6,$B0,$BB,$F1,$4D
                DEFB      $80,$9D,$01    ,$F1,$3B
                DEFB  $04,$80,$BB,$82,$ED,$F1,$76
                DEFB      $01    ,$82,$31,$01
                DEFB  $FF  ; End of Pattern


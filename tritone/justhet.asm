
;Tritone v2 beeper music engine by Shiru (shiru@mail.ru) 03'11
;Three channels of tone, per-pattern tempo
;One channel of interrupting drums
;Feel free to do whatever you want with the code, it is PD
;
;
; TRITONE Engine
;
; Assemble with PASMO
;
; 	pasmo --alocal %1.asm
; 	rbinary %1.obj %1.vz


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
	ld c,2
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
	ld de,$0200	; DJM
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

;	and	2
	out	(255), a
	djnz .l01
	dec c
	jr nz,.l01

	jp nextRow

drumNoise
	ld b,0
	ld h,b
	ld l,h
	ld de,$0200	; DJM
.l02
	ld a,(hl)
	and d
	out	(255), a
	and (ix)
	dec e
	and	2
	out	(255), a
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
	out	(255), a
	add ix,de	;15
	ld a,ixh	;8
.duty1 equ $+1
	cp 128		;7
	sbc a,a		;4
	and c		;4
	out	(255), a
	add hl,sp	;11
	ld a,h		;4
.duty2 equ $+1
	cp 128		;7
	sbc a,a		;4
	and c		;4
	exx			;4
	dec e		;4
;	and	2
	out	(255), a
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
	out	(255), a
	ld a,ixh	;8
.duty1 equ $+1
	cp 128		;7
	sbc a,a		;4
	and c		;4
	out	(255), a
	add hl,sp	;11
	ld a,h		;4
.duty2 equ $+1
	cp 128		;7
	sbc a,a		;4
	and c		;4
	exx			;4
	dec e		;4
;	and	2
	out	(255), a
	jr nz,soundLoop	;10=153t
	dec d		;4
	jr nz,soundLoop	;10
	
	endif
	

;	xor a
;	ld (26624), a

;	and	2
	out	(255), a

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



MUSICDATA:

; *** Song layout ***
                      DEFW      PAT0
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
                      DEFW      PAT8
                      DEFW      PAT9
                      DEFW      PAT10
                      DEFW      PAT11
                      DEFW      PAT8
                      DEFW      PAT9
                      DEFW      PAT10
                      DEFW      PAT11
                      DEFW      PAT8
                      DEFW      PAT9
                      DEFW      PAT10
                      DEFW      PAT12
LOOPSTART:            DEFW      PAT13
                      DEFW      PAT14
                      DEFW      PAT15
                      DEFW      PAT16
                      DEFW      PAT13
                      DEFW      PAT17
                      DEFW      PAT18
                      DEFW      PAT19
                      DEFW      PAT20
                      DEFW      PAT21
                      DEFW      PAT22
                      DEFW      PAT23
                      DEFW      PAT20
                      DEFW      PAT21
                      DEFW      PAT22
                      DEFW      PAT23
                      DEFW      PAT24
                      DEFW      PAT25
                      DEFW      PAT26
                      DEFW      PAT27
                      DEFW      PAT24
                      DEFW      PAT25
                      DEFW      PAT28
                      DEFW      PAT27
                      DEFW      PAT24
                      DEFW      PAT25
                      DEFW      PAT26
                      DEFW      PAT27
                      DEFW      PAT24
                      DEFW      PAT25
                      DEFW      PAT28
                      DEFW      PAT27
                      DEFW      PAT13
                      DEFW      PAT14
                      DEFW      PAT15
                      DEFW      PAT16
                      DEFW      PAT13
                      DEFW      PAT17
                      DEFW      PAT18
                      DEFW      PAT19
                      DEFW      PAT20
                      DEFW      PAT21
                      DEFW      PAT22
                      DEFW      PAT23
                      DEFW      PAT20
                      DEFW      PAT21
                      DEFW      PAT22
                      DEFW      PAT23
                      DEFW      PAT29
                      DEFW      PAT33
                      DEFW      PAT30
                      DEFW      PAT34
                      DEFW      PAT31
                      DEFW      PAT35
                      DEFW      PAT32
                      DEFW      PAT36
                      DEFW      PAT29
                      DEFW      PAT33
                      DEFW      PAT30
                      DEFW      PAT34
                      DEFW      PAT31
                      DEFW      PAT35
                      DEFW      PAT32
                      DEFW      PAT36
                      DEFW      PAT4
                      DEFW      PAT1
                      DEFW      PAT2
                      DEFW      PAT3
                      DEFW      PAT4
                      DEFW      PAT5
                      DEFW      PAT6
                      DEFW      PAT7
                      DEFW      $0000
                      DEFW      LOOPSTART

; *** Patterns ***
PAT0:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB  $FF  ; End of Pattern

PAT1:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT2:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB  $FF  ; End of Pattern

PAT3:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$82,$ED,$80,$7D
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT4:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $00    ,$83,$49,$90,$8C
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB  $FF  ; End of Pattern

PAT5:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$80,$8C
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$83,$49,$80,$A6
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$00
                DEFB  $FF  ; End of Pattern

PAT6:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$82,$ED,$90,$BB
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$82,$ED,$00
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$82,$ED,$90,$BB
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB  $FF  ; End of Pattern

PAT7:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$01    ,$83,$E8,$91,$A4
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $08,$01    ,$82,$ED,$91,$76
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $07,$01    ,$82,$ED,$91,$3B
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB  $06,$01    ,$82,$ED,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB  $09,$01    ,$83,$E8,$90,$FA
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $08,$01    ,$82,$ED,$90,$D2
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB  $07,$01    ,$83,$E8,$90,$BB
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT8:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$01    ,$83,$49,$90,$8C
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$83,$49,$90,$7D
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB  $09,$01    ,$83,$49,$90,$8C
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB  $03,$01    ,$83,$49,$84,$63
                DEFB      $01    ,$85,$86,$82,$31
                DEFB      $01    ,$84,$63,$81,$76
                DEFB      $01    ,$83,$49,$81,$18
                DEFB      $01    ,$01    ,$00
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$90,$8C
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$83,$49,$90,$D2
                DEFB      $01    ,$85,$86,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB  $09,$01    ,$83,$49,$90,$FA
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB  $FF  ; End of Pattern

PAT9:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$84,$63,$90,$8C
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $09,$01    ,$83,$49,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$91,$18
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$01    ,$83,$49,$90,$8C
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB  $03,$01    ,$83,$49,$84,$63
                DEFB      $01    ,$85,$DB,$82,$31
                DEFB      $01    ,$84,$63,$81,$76
                DEFB      $01    ,$83,$49,$81,$18
                DEFB      $01    ,$01    ,$00
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$90,$8C
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$83,$49,$90,$A6
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB  $09,$01    ,$84,$63,$90,$B0
                DEFB      $01    ,$83,$49,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT10:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$01    ,$82,$ED,$90,$BB
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB  $09,$01    ,$82,$ED,$90,$A6
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB  $09,$01    ,$82,$ED,$90,$BB
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB  $03,$01    ,$82,$ED,$84,$63
                DEFB      $01    ,$85,$DB,$82,$31
                DEFB      $01    ,$83,$E8,$81,$76
                DEFB      $01    ,$82,$ED,$81,$18
                DEFB      $01    ,$01    ,$00
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$90,$BB
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$82,$ED,$91,$18
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB  $09,$01    ,$82,$ED,$91,$4D
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB  $FF  ; End of Pattern

PAT11:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$83,$E8,$90,$7D
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $09,$01    ,$82,$ED,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$90,$FA
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $09,$01    ,$82,$ED,$90,$7D
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB  $03,$01    ,$82,$ED,$84,$63
                DEFB      $01    ,$84,$E7,$82,$31
                DEFB      $01    ,$83,$E8,$81,$76
                DEFB      $01    ,$82,$ED,$81,$18
                DEFB      $01    ,$01    ,$00
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$90,$7D
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$00
                DEFB      $01    ,$82,$ED,$90,$7D
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB  $09,$01    ,$83,$E8,$90,$84
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT12:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$01    ,$83,$E8,$90,$8C
                DEFB      $01    ,$82,$ED,$90,$8C
                DEFB      $01    ,$00    ,$90,$8C
                DEFB      $01    ,$01    ,$90,$8C
                DEFB      $01    ,$01    ,$90,$8C
                DEFB      $01    ,$01    ,$90,$8C
                DEFB  $08,$01    ,$82,$ED,$90,$8C
                DEFB      $01    ,$84,$E7,$90,$8C
                DEFB      $01    ,$83,$E8,$90,$8C
                DEFB      $01    ,$82,$ED,$90,$8C
                DEFB      $01    ,$82,$ED,$90,$8C
                DEFB      $01    ,$84,$E7,$90,$8C
                DEFB      $01    ,$83,$E8,$90,$7D
                DEFB      $01    ,$82,$ED,$90,$7D
                DEFB      $01    ,$00    ,$90,$7D
                DEFB      $01    ,$01    ,$90,$7D
                DEFB      $01    ,$01    ,$90,$7D
                DEFB      $01    ,$01    ,$90,$7D
                DEFB  $07,$01    ,$82,$ED,$90,$8C
                DEFB      $01    ,$84,$E7,$90,$8C
                DEFB      $01    ,$83,$E8,$90,$8C
                DEFB      $01    ,$82,$ED,$90,$8C
                DEFB      $01    ,$82,$ED,$90,$8C
                DEFB      $01    ,$00    ,$90,$8C
                DEFB  $03,$01    ,$82,$ED,$84,$63
                DEFB      $01    ,$84,$E7,$82,$31
                DEFB      $01    ,$83,$E8,$81,$76
                DEFB      $01    ,$82,$ED,$81,$18
                DEFB      $01    ,$82,$ED,$00
                DEFB      $01    ,$84,$E7,$01
                DEFB  $09,$89,$D9,$83,$E8,$90,$8C
                DEFB      $84,$E7,$82,$ED,$90,$8C
                DEFB      $89,$D9,$00    ,$90,$8C
                DEFB      $84,$E7,$01    ,$90,$8C
                DEFB      $89,$D9,$01    ,$90,$8C
                DEFB      $84,$E7,$01    ,$90,$8C
                DEFB  $08,$8B,$0D,$82,$ED,$90,$D2
                DEFB      $85,$86,$84,$E7,$90,$D2
                DEFB      $8B,$0D,$83,$E8,$90,$D2
                DEFB      $85,$86,$82,$ED,$90,$D2
                DEFB      $8B,$0D,$82,$ED,$90,$D2
                DEFB      $85,$86,$84,$E7,$90,$D2
                DEFB  $07,$8B,$B6,$83,$E8,$90,$FA
                DEFB      $85,$DB,$82,$ED,$90,$FA
                DEFB      $8B,$B6,$00    ,$90,$FA
                DEFB      $85,$DB,$01    ,$90,$FA
                DEFB      $8B,$B6,$01    ,$90,$FA
                DEFB      $85,$DB,$01    ,$90,$FA
                DEFB  $FF  ; End of Pattern

PAT13:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$8B,$0D,$83,$49,$90,$8C
                DEFB      $85,$86,$85,$86,$90,$8C
                DEFB      $8B,$0D,$84,$63,$90,$8C
                DEFB      $85,$86,$83,$49,$90,$8C
                DEFB      $8B,$0D,$83,$49,$90,$8C
                DEFB      $00    ,$85,$86,$90,$8C
                DEFB      $9B,$0D,$84,$63,$90,$8C
                DEFB      $95,$86,$83,$49,$90,$8C
                DEFB      $9B,$0D,$00    ,$90,$8C
                DEFB      $95,$86,$01    ,$90,$8C
                DEFB      $9B,$0D,$01    ,$90,$8C
                DEFB      $95,$86,$01    ,$00
                DEFB      $00    ,$83,$49,$90,$7D
                DEFB      $00    ,$85,$86,$90,$7D
                DEFB      $00    ,$84,$63,$90,$7D
                DEFB      $00    ,$83,$49,$90,$7D
                DEFB      $00    ,$83,$49,$90,$7D
                DEFB      $00    ,$00    ,$00
                DEFB  $09,$BB,$0D,$83,$49,$90,$8C
                DEFB      $B5,$86,$85,$86,$90,$8C
                DEFB      $BB,$0D,$84,$63,$90,$8C
                DEFB      $B5,$86,$83,$49,$90,$8C
                DEFB      $BB,$0D,$83,$49,$90,$8C
                DEFB      $00    ,$00    ,$00
                DEFB  $03,$C9,$D9,$83,$49,$84,$63
                DEFB      $C4,$E7,$85,$86,$82,$31
                DEFB      $C9,$D9,$84,$63,$81,$76
                DEFB      $C4,$E7,$83,$49,$81,$18
                DEFB      $C9,$D9,$83,$49,$00
                DEFB      $C4,$E7,$85,$86,$01
                DEFB      $00    ,$84,$63,$90,$8C
                DEFB      $00    ,$83,$49,$90,$8C
                DEFB      $00    ,$00    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB      $C8,$C6,$83,$49,$90,$D2
                DEFB      $E4,$63,$85,$86,$90,$D2
                DEFB      $E8,$C6,$84,$63,$90,$D2
                DEFB      $E4,$63,$83,$49,$90,$D2
                DEFB      $E8,$C6,$83,$49,$90,$D2
                DEFB      $E4,$63,$00    ,$90,$D2
                DEFB  $09,$FB,$B6,$83,$49,$90,$FA
                DEFB      $F5,$DB,$85,$DB,$90,$FA
                DEFB      $FB,$B6,$84,$63,$90,$FA
                DEFB      $F5,$DB,$83,$49,$90,$FA
                DEFB      $FB,$B6,$83,$49,$90,$FA
                DEFB      $F5,$DB,$85,$DB,$90,$FA
                DEFB  $FF  ; End of Pattern

PAT14:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $EB,$B6,$84,$63,$90,$8C
                DEFB      $E5,$DB,$83,$49,$90,$8C
                DEFB      $EB,$B6,$00    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB  $09,$D6,$92,$83,$49,$90,$8C
                DEFB      $D8,$C6,$85,$DB,$90,$8C
                DEFB      $D6,$92,$84,$63,$90,$8C
                DEFB      $D8,$C6,$83,$49,$90,$8C
                DEFB      $D6,$92,$83,$49,$90,$8C
                DEFB      $00    ,$85,$DB,$90,$8C
                DEFB      $C6,$92,$84,$63,$91,$18
                DEFB      $C8,$C6,$83,$49,$91,$18
                DEFB      $C6,$92,$00    ,$91,$18
                DEFB      $C8,$C6,$01    ,$91,$18
                DEFB      $C6,$92,$01    ,$91,$18
                DEFB      $00    ,$01    ,$91,$18
                DEFB  $09,$B6,$92,$83,$49,$90,$8C
                DEFB      $B8,$C6,$85,$DB,$90,$8C
                DEFB      $B6,$92,$84,$63,$90,$8C
                DEFB      $B8,$C6,$83,$49,$90,$8C
                DEFB      $B6,$92,$83,$49,$90,$8C
                DEFB      $00    ,$00    ,$90,$8C
                DEFB  $03,$A6,$92,$83,$49,$84,$63
                DEFB      $A8,$C6,$85,$DB,$82,$31
                DEFB      $A6,$92,$84,$63,$81,$76
                DEFB      $A8,$C6,$83,$49,$81,$18
                DEFB      $A6,$92,$83,$49,$00
                DEFB      $A8,$C6,$85,$DB,$01
                DEFB      $00    ,$84,$63,$90,$8C
                DEFB      $01    ,$83,$49,$90,$8C
                DEFB      $01    ,$00    ,$90,$8C
                DEFB      $01    ,$01    ,$90,$8C
                DEFB      $01    ,$01    ,$90,$8C
                DEFB      $01    ,$01    ,$90,$8C
                DEFB      $86,$92,$83,$49,$90,$A6
                DEFB      $88,$C6,$85,$DB,$90,$A6
                DEFB      $86,$92,$84,$63,$90,$A6
                DEFB      $88,$C6,$83,$49,$90,$A6
                DEFB      $86,$92,$83,$49,$90,$A6
                DEFB      $88,$C6,$85,$DB,$90,$A6
                DEFB  $09,$00    ,$84,$63,$90,$B0
                DEFB      $01    ,$83,$49,$90,$B0
                DEFB      $01    ,$00    ,$90,$B0
                DEFB      $01    ,$01    ,$90,$B0
                DEFB      $01    ,$01    ,$90,$B0
                DEFB      $01    ,$01    ,$90,$B0
                DEFB  $FF  ; End of Pattern

PAT15:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$89,$D9,$82,$ED,$90,$BB
                DEFB      $84,$E7,$85,$DB,$90,$BB
                DEFB      $89,$D9,$83,$E8,$90,$BB
                DEFB      $84,$E7,$82,$ED,$90,$BB
                DEFB      $89,$D9,$82,$ED,$90,$BB
                DEFB      $84,$E7,$00    ,$90,$BB
                DEFB      $00    ,$82,$ED,$90,$BB
                DEFB      $00    ,$85,$DB,$90,$BB
                DEFB      $00    ,$83,$E8,$90,$BB
                DEFB      $00    ,$82,$ED,$90,$BB
                DEFB      $00    ,$82,$ED,$90,$BB
                DEFB      $00    ,$00    ,$90,$BB
                DEFB  $09,$A9,$D9,$82,$ED,$90,$A6
                DEFB      $A4,$E7,$85,$DB,$90,$A6
                DEFB      $A9,$D9,$83,$E8,$90,$A6
                DEFB      $A4,$E7,$82,$ED,$90,$A6
                DEFB      $A9,$D9,$82,$ED,$90,$A6
                DEFB      $A4,$E7,$00    ,$90,$A6
                DEFB  $09,$00    ,$82,$ED,$90,$BB
                DEFB      $00    ,$85,$DB,$90,$BB
                DEFB      $00    ,$83,$E8,$90,$BB
                DEFB      $00    ,$82,$ED,$90,$BB
                DEFB      $00    ,$82,$ED,$90,$BB
                DEFB      $00    ,$00    ,$90,$BB
                DEFB  $03,$CB,$B6,$82,$ED,$84,$63
                DEFB      $C5,$DB,$85,$DB,$82,$31
                DEFB      $CB,$B6,$83,$E8,$81,$76
                DEFB      $C5,$DB,$82,$ED,$81,$18
                DEFB      $CB,$B6,$82,$ED,$00
                DEFB      $C5,$DB,$85,$DB,$01
                DEFB      $00    ,$83,$E8,$90,$BB
                DEFB      $00    ,$82,$ED,$90,$BB
                DEFB      $00    ,$00    ,$90,$BB
                DEFB      $00    ,$01    ,$90,$BB
                DEFB      $00    ,$01    ,$90,$BB
                DEFB      $00    ,$01    ,$90,$BB
                DEFB      $EB,$0D,$82,$ED,$91,$18
                DEFB      $E5,$86,$85,$DB,$91,$18
                DEFB      $EB,$0D,$83,$E8,$91,$18
                DEFB      $E5,$86,$82,$ED,$91,$18
                DEFB      $EB,$0D,$82,$ED,$91,$18
                DEFB      $00    ,$00    ,$91,$18
                DEFB  $09,$F8,$C6,$82,$ED,$91,$4D
                DEFB      $F4,$63,$84,$E7,$91,$4D
                DEFB      $F8,$C6,$83,$E8,$91,$4D
                DEFB      $F4,$63,$82,$ED,$91,$4D
                DEFB      $F8,$C6,$82,$ED,$91,$4D
                DEFB      $00    ,$84,$E7,$91,$4D
                DEFB  $FF  ; End of Pattern

PAT16:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $E4,$63,$83,$E8,$90,$8C
                DEFB      $E4,$63,$82,$ED,$90,$8C
                DEFB      $E4,$63,$00    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB  $09,$D6,$92,$82,$ED,$90,$8C
                DEFB      $D8,$C6,$84,$E7,$90,$8C
                DEFB      $D6,$92,$83,$E8,$90,$8C
                DEFB      $D8,$C6,$82,$ED,$90,$8C
                DEFB      $D6,$92,$82,$ED,$90,$8C
                DEFB      $00    ,$84,$E7,$90,$8C
                DEFB      $C6,$92,$83,$E8,$90,$7D
                DEFB      $C8,$C6,$82,$ED,$90,$7D
                DEFB      $C6,$92,$00    ,$90,$7D
                DEFB      $C8,$C6,$01    ,$90,$7D
                DEFB      $C6,$92,$01    ,$90,$7D
                DEFB      $00    ,$01    ,$90,$7D
                DEFB  $09,$B6,$92,$82,$ED,$90,$8C
                DEFB      $B8,$C6,$84,$E7,$90,$8C
                DEFB      $B6,$92,$83,$E8,$90,$8C
                DEFB      $B8,$C6,$82,$ED,$90,$8C
                DEFB      $B6,$92,$82,$ED,$90,$8C
                DEFB      $00    ,$00    ,$90,$8C
                DEFB  $03,$A6,$92,$82,$ED,$84,$63
                DEFB      $A8,$C6,$84,$E7,$82,$31
                DEFB      $A6,$92,$83,$E8,$81,$76
                DEFB      $A8,$C6,$82,$ED,$81,$18
                DEFB      $A6,$92,$82,$ED,$00
                DEFB      $A8,$C6,$84,$E7,$01
                DEFB      $00    ,$83,$E8,$90,$8C
                DEFB      $00    ,$82,$ED,$90,$8C
                DEFB      $00    ,$00    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB      $87,$60,$82,$ED,$90,$D2
                DEFB      $88,$C6,$84,$E7,$90,$D2
                DEFB      $87,$60,$83,$E8,$90,$D2
                DEFB      $88,$C6,$82,$ED,$90,$D2
                DEFB      $87,$60,$82,$ED,$90,$D2
                DEFB      $88,$C6,$84,$E7,$90,$D2
                DEFB  $09,$00    ,$83,$E8,$90,$FA
                DEFB      $00    ,$82,$ED,$90,$FA
                DEFB      $00    ,$00    ,$90,$FA
                DEFB      $00    ,$01    ,$90,$FA
                DEFB      $00    ,$01    ,$90,$FA
                DEFB      $00    ,$01    ,$90,$FA
                DEFB  $FF  ; End of Pattern

PAT17:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $E5,$DB,$84,$63,$90,$8C
                DEFB      $E5,$DB,$83,$49,$90,$8C
                DEFB      $E5,$DB,$00    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB  $09,$DB,$0D,$83,$49,$90,$8C
                DEFB      $D5,$86,$85,$DB,$90,$8C
                DEFB      $DB,$0D,$84,$63,$90,$8C
                DEFB      $D5,$86,$83,$49,$90,$8C
                DEFB      $D5,$86,$83,$49,$90,$8C
                DEFB      $00    ,$85,$DB,$90,$8C
                DEFB      $CB,$B6,$84,$63,$91,$18
                DEFB      $C5,$DB,$83,$49,$91,$18
                DEFB      $CB,$B6,$00    ,$91,$18
                DEFB      $C5,$DB,$01    ,$91,$18
                DEFB      $C5,$DB,$01    ,$91,$18
                DEFB      $00    ,$01    ,$91,$18
                DEFB  $09,$BD,$25,$83,$49,$90,$8C
                DEFB      $B6,$92,$85,$DB,$90,$8C
                DEFB      $BD,$25,$84,$63,$90,$8C
                DEFB      $B6,$92,$83,$49,$90,$8C
                DEFB      $B6,$92,$83,$49,$90,$8C
                DEFB      $00    ,$00    ,$90,$8C
                DEFB  $03,$AB,$B6,$83,$49,$84,$63
                DEFB      $A5,$DB,$85,$DB,$82,$31
                DEFB      $AB,$B6,$84,$63,$81,$76
                DEFB      $A5,$DB,$83,$49,$81,$18
                DEFB      $A5,$DB,$83,$49,$00
                DEFB      $00    ,$85,$DB,$01
                DEFB      $9B,$0D,$84,$63,$90,$8C
                DEFB      $95,$86,$83,$49,$90,$8C
                DEFB      $9B,$0D,$00    ,$90,$8C
                DEFB      $95,$86,$01    ,$90,$8C
                DEFB      $95,$86,$01    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB      $89,$D9,$83,$49,$90,$A6
                DEFB      $84,$E7,$85,$DB,$90,$A6
                DEFB      $89,$D9,$84,$63,$90,$A6
                DEFB      $84,$E7,$83,$49,$90,$A6
                DEFB      $84,$E7,$83,$49,$90,$A6
                DEFB      $00    ,$85,$DB,$90,$A6
                DEFB  $09,$88,$C6,$84,$63,$90,$B0
                DEFB      $84,$63,$83,$49,$90,$B0
                DEFB      $88,$C6,$00    ,$90,$B0
                DEFB      $84,$63,$01    ,$90,$B0
                DEFB      $84,$63,$01    ,$90,$B0
                DEFB      $00    ,$01    ,$90,$B0
                DEFB  $FF  ; End of Pattern

PAT18:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$87,$60,$82,$ED,$90,$BB
                DEFB      $83,$B0,$85,$DB,$90,$BB
                DEFB      $87,$60,$83,$E8,$90,$BB
                DEFB      $83,$B0,$82,$ED,$90,$BB
                DEFB      $87,$60,$82,$ED,$90,$BB
                DEFB      $83,$B0,$00    ,$90,$BB
                DEFB      $98,$C6,$82,$ED,$90,$BB
                DEFB      $94,$63,$85,$DB,$90,$BB
                DEFB      $98,$C6,$83,$E8,$90,$BB
                DEFB      $94,$63,$82,$ED,$90,$BB
                DEFB      $98,$C6,$82,$ED,$90,$BB
                DEFB      $94,$63,$00    ,$90,$BB
                DEFB  $09,$A9,$D9,$82,$ED,$90,$A6
                DEFB      $A4,$E7,$85,$DB,$90,$A6
                DEFB      $A9,$D9,$83,$E8,$90,$A6
                DEFB      $A4,$E7,$82,$ED,$90,$A6
                DEFB      $A9,$D9,$82,$ED,$90,$A6
                DEFB      $A4,$E7,$00    ,$90,$A6
                DEFB  $09,$B8,$C6,$82,$ED,$90,$BB
                DEFB      $B4,$63,$85,$DB,$90,$BB
                DEFB      $B8,$C6,$83,$E8,$90,$BB
                DEFB      $B4,$63,$82,$ED,$90,$BB
                DEFB      $B8,$C6,$82,$ED,$90,$BB
                DEFB      $B4,$63,$00    ,$90,$BB
                DEFB  $03,$C7,$60,$82,$ED,$84,$63
                DEFB      $C3,$B0,$85,$DB,$82,$31
                DEFB      $C7,$60,$83,$E8,$81,$76
                DEFB      $C3,$B0,$82,$ED,$81,$18
                DEFB      $C7,$60,$82,$ED,$00
                DEFB      $C3,$B0,$85,$DB,$01
                DEFB      $D6,$92,$83,$E8,$90,$BB
                DEFB      $D3,$49,$82,$ED,$90,$BB
                DEFB      $D6,$92,$00    ,$90,$BB
                DEFB      $D3,$49,$01    ,$90,$BB
                DEFB      $D6,$92,$01    ,$90,$BB
                DEFB      $D3,$49,$01    ,$90,$BB
                DEFB      $00    ,$82,$ED,$91,$18
                DEFB      $01    ,$85,$DB,$91,$18
                DEFB      $01    ,$83,$E8,$91,$18
                DEFB      $01    ,$82,$ED,$91,$18
                DEFB      $01    ,$82,$ED,$91,$18
                DEFB      $01    ,$00    ,$91,$18
                DEFB  $09,$F5,$DB,$82,$ED,$91,$4D
                DEFB      $F2,$ED,$84,$E7,$91,$4D
                DEFB      $F5,$DB,$83,$E8,$91,$4D
                DEFB      $F2,$ED,$82,$ED,$91,$4D
                DEFB      $F5,$DB,$82,$ED,$91,$4D
                DEFB      $F2,$ED,$84,$E7,$91,$4D
                DEFB  $FF  ; End of Pattern

PAT19:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $00    ,$83,$E8,$90,$8C
                DEFB      $01    ,$82,$ED,$90,$8C
                DEFB      $01    ,$00    ,$90,$8C
                DEFB      $01    ,$01    ,$90,$8C
                DEFB      $01    ,$01    ,$90,$8C
                DEFB      $01    ,$01    ,$90,$8C
                DEFB  $09,$D5,$DB,$82,$ED,$90,$8C
                DEFB      $D2,$ED,$84,$E7,$90,$8C
                DEFB      $D5,$DB,$83,$E8,$90,$8C
                DEFB      $D2,$ED,$82,$ED,$90,$8C
                DEFB      $D5,$DB,$82,$ED,$90,$8C
                DEFB      $D2,$ED,$84,$E7,$90,$8C
                DEFB      $00    ,$83,$E8,$90,$7D
                DEFB      $01    ,$82,$ED,$90,$7D
                DEFB      $01    ,$00    ,$90,$7D
                DEFB      $01    ,$01    ,$90,$7D
                DEFB      $01    ,$01    ,$90,$7D
                DEFB      $01    ,$01    ,$90,$7D
                DEFB  $09,$B5,$86,$82,$ED,$90,$8C
                DEFB      $B2,$C3,$84,$E7,$90,$8C
                DEFB      $B5,$86,$83,$E8,$90,$8C
                DEFB      $B2,$C3,$82,$ED,$90,$8C
                DEFB      $B5,$86,$82,$ED,$90,$8C
                DEFB      $B2,$C3,$00    ,$90,$8C
                DEFB  $03,$A4,$E7,$82,$ED,$84,$63
                DEFB      $A2,$76,$84,$E7,$82,$31
                DEFB      $A4,$E7,$83,$E8,$81,$76
                DEFB      $A2,$76,$82,$ED,$81,$18
                DEFB      $A4,$E7,$82,$ED,$00
                DEFB      $A2,$76,$84,$E7,$01
                DEFB  $03,$84,$63,$83,$E8,$90,$8C
                DEFB      $82,$31,$82,$ED,$90,$8C
                DEFB      $81,$76,$00    ,$90,$8C
                DEFB      $81,$18,$01    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB      $01    ,$01    ,$90,$8C
                DEFB  $03,$84,$63,$82,$ED,$90,$D2
                DEFB      $82,$31,$84,$E7,$90,$D2
                DEFB      $84,$63,$83,$E8,$90,$D2
                DEFB      $82,$31,$82,$ED,$90,$D2
                DEFB      $84,$63,$82,$ED,$90,$D2
                DEFB      $82,$31,$84,$E7,$90,$D2
                DEFB  $03,$84,$63,$83,$E8,$90,$FA
                DEFB      $82,$31,$82,$ED,$90,$FA
                DEFB      $81,$76,$00    ,$90,$FA
                DEFB      $81,$18,$01    ,$90,$FA
                DEFB      $00    ,$01    ,$90,$FA
                DEFB      $01    ,$01    ,$90,$FA
                DEFB  $FF  ; End of Pattern

PAT20:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$84,$63,$83,$49,$90,$8C
                DEFB      $88,$C6,$85,$86,$90,$8C
                DEFB      $84,$63,$84,$63,$90,$8C
                DEFB      $88,$C6,$83,$49,$90,$8C
                DEFB      $84,$63,$83,$49,$90,$8C
                DEFB      $88,$C6,$85,$86,$90,$8C
                DEFB      $95,$86,$84,$63,$90,$8C
                DEFB      $9B,$0D,$83,$49,$90,$8C
                DEFB      $95,$86,$00    ,$90,$8C
                DEFB      $9B,$0D,$01    ,$90,$8C
                DEFB      $95,$86,$01    ,$90,$8C
                DEFB      $9B,$0D,$01    ,$00
                DEFB      $A5,$DB,$83,$49,$90,$7D
                DEFB      $AB,$B6,$85,$86,$90,$7D
                DEFB      $A5,$DB,$84,$63,$90,$7D
                DEFB      $AB,$B6,$83,$49,$90,$7D
                DEFB      $A5,$DB,$83,$49,$90,$7D
                DEFB      $AB,$B6,$00    ,$00
                DEFB  $09,$B4,$63,$83,$49,$90,$8C
                DEFB      $B8,$C6,$85,$86,$90,$8C
                DEFB      $B4,$63,$84,$63,$90,$8C
                DEFB      $B8,$C6,$83,$49,$90,$8C
                DEFB      $B4,$63,$83,$49,$90,$8C
                DEFB      $B8,$C6,$00    ,$00
                DEFB  $03,$C5,$86,$83,$49,$84,$63
                DEFB      $CB,$0D,$85,$86,$82,$31
                DEFB      $C5,$86,$84,$63,$81,$76
                DEFB      $CB,$0D,$83,$49,$81,$18
                DEFB      $C5,$86,$83,$49,$00
                DEFB      $CB,$0D,$85,$86,$01
                DEFB      $D5,$DB,$84,$63,$90,$8C
                DEFB      $DB,$B6,$83,$49,$90,$8C
                DEFB      $D5,$DB,$00    ,$90,$8C
                DEFB      $DB,$B6,$01    ,$90,$8C
                DEFB      $D5,$DB,$01    ,$90,$8C
                DEFB      $DB,$B6,$01    ,$90,$8C
                DEFB      $E4,$63,$83,$49,$90,$D2
                DEFB      $E8,$C6,$85,$86,$90,$D2
                DEFB      $E4,$63,$84,$63,$90,$D2
                DEFB      $E8,$C6,$83,$49,$90,$D2
                DEFB      $E4,$63,$83,$49,$90,$D2
                DEFB      $E8,$C6,$00    ,$90,$D2
                DEFB  $09,$F5,$86,$83,$49,$90,$FA
                DEFB      $FB,$0D,$85,$DB,$90,$FA
                DEFB      $F5,$86,$84,$63,$90,$FA
                DEFB      $FB,$0D,$83,$49,$90,$FA
                DEFB      $F5,$86,$83,$49,$90,$FA
                DEFB      $FB,$0D,$85,$DB,$90,$FA
                DEFB  $FF  ; End of Pattern

PAT21:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $E5,$DB,$84,$63,$90,$8C
                DEFB      $EB,$B6,$83,$49,$90,$8C
                DEFB      $E5,$DB,$00    ,$90,$8C
                DEFB      $EB,$B6,$01    ,$90,$8C
                DEFB      $E5,$DB,$01    ,$90,$8C
                DEFB      $EB,$B6,$01    ,$90,$8C
                DEFB  $09,$D4,$63,$83,$49,$90,$8C
                DEFB      $D8,$C6,$85,$DB,$90,$8C
                DEFB      $D4,$63,$84,$63,$90,$8C
                DEFB      $D8,$C6,$83,$49,$90,$8C
                DEFB      $D4,$63,$83,$49,$90,$8C
                DEFB      $D8,$C6,$85,$DB,$90,$8C
                DEFB      $C5,$86,$84,$63,$91,$18
                DEFB      $CB,$0D,$83,$49,$91,$18
                DEFB      $C5,$86,$00    ,$91,$18
                DEFB      $CB,$0D,$01    ,$91,$18
                DEFB      $C5,$86,$01    ,$91,$18
                DEFB      $CB,$0D,$01    ,$91,$18
                DEFB  $09,$B5,$DB,$83,$49,$90,$8C
                DEFB      $BB,$B6,$85,$DB,$90,$8C
                DEFB      $B5,$DB,$84,$63,$90,$8C
                DEFB      $BB,$B6,$83,$49,$90,$8C
                DEFB      $B5,$DB,$83,$49,$90,$8C
                DEFB      $BB,$B6,$00    ,$90,$8C
                DEFB  $03,$B5,$DB,$83,$49,$84,$63
                DEFB      $BB,$B6,$85,$DB,$82,$31
                DEFB      $B5,$DB,$84,$63,$81,$76
                DEFB      $BB,$B6,$83,$49,$81,$18
                DEFB      $B5,$DB,$83,$49,$00
                DEFB      $BB,$B6,$85,$DB,$01
                DEFB      $A4,$63,$84,$63,$90,$8C
                DEFB      $A8,$C6,$83,$49,$90,$8C
                DEFB      $A4,$63,$00    ,$90,$8C
                DEFB      $A8,$C6,$01    ,$90,$8C
                DEFB      $A4,$63,$01    ,$90,$8C
                DEFB      $A8,$C6,$01    ,$90,$8C
                DEFB      $95,$86,$83,$49,$90,$A6
                DEFB      $9B,$0D,$85,$DB,$90,$A6
                DEFB      $95,$86,$84,$63,$90,$A6
                DEFB      $9B,$0D,$83,$49,$90,$A6
                DEFB      $95,$86,$83,$49,$90,$A6
                DEFB      $9B,$0D,$85,$DB,$90,$A6
                DEFB  $09,$85,$DB,$84,$63,$90,$B0
                DEFB      $8B,$B6,$83,$49,$90,$B0
                DEFB      $85,$DB,$00    ,$90,$B0
                DEFB      $8B,$B6,$01    ,$90,$B0
                DEFB      $85,$DB,$01    ,$90,$B0
                DEFB      $8B,$B6,$01    ,$90,$B0
                DEFB  $FF  ; End of Pattern

PAT22:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$84,$63,$82,$ED,$90,$BB
                DEFB      $88,$C6,$85,$DB,$90,$BB
                DEFB      $84,$63,$83,$E8,$90,$BB
                DEFB      $88,$C6,$82,$ED,$90,$BB
                DEFB      $84,$63,$82,$ED,$90,$BB
                DEFB      $88,$C6,$00    ,$90,$BB
                DEFB      $95,$86,$82,$ED,$90,$BB
                DEFB      $9B,$0D,$85,$DB,$90,$BB
                DEFB      $95,$86,$83,$E8,$90,$BB
                DEFB      $9B,$0D,$82,$ED,$90,$BB
                DEFB      $95,$86,$82,$ED,$90,$BB
                DEFB      $9B,$0D,$00    ,$90,$BB
                DEFB  $09,$A5,$DB,$82,$ED,$90,$A6
                DEFB      $AB,$B6,$85,$DB,$90,$A6
                DEFB      $A5,$DB,$83,$E8,$90,$A6
                DEFB      $AB,$B6,$82,$ED,$90,$A6
                DEFB      $A5,$DB,$82,$ED,$90,$A6
                DEFB      $AB,$B6,$00    ,$90,$A6
                DEFB  $09,$B4,$63,$82,$ED,$90,$BB
                DEFB      $B8,$C6,$85,$DB,$90,$BB
                DEFB      $B4,$63,$83,$E8,$90,$BB
                DEFB      $B8,$C6,$82,$ED,$90,$BB
                DEFB      $B4,$63,$82,$ED,$90,$BB
                DEFB      $B8,$C6,$00    ,$90,$BB
                DEFB  $03,$C5,$86,$82,$ED,$84,$63
                DEFB      $CB,$0D,$85,$DB,$82,$31
                DEFB      $C5,$86,$83,$E8,$81,$76
                DEFB      $CB,$0D,$82,$ED,$81,$18
                DEFB      $C5,$86,$82,$ED,$00
                DEFB      $CB,$0D,$85,$DB,$01
                DEFB      $D5,$DB,$83,$E8,$90,$BB
                DEFB      $DB,$B6,$82,$ED,$90,$BB
                DEFB      $D5,$DB,$00    ,$90,$BB
                DEFB      $DB,$B6,$01    ,$90,$BB
                DEFB      $D5,$DB,$01    ,$90,$BB
                DEFB      $DB,$B6,$01    ,$90,$BB
                DEFB      $E4,$63,$82,$ED,$91,$18
                DEFB      $E8,$C6,$85,$DB,$91,$18
                DEFB      $E4,$63,$83,$E8,$91,$18
                DEFB      $E8,$C6,$82,$ED,$91,$18
                DEFB      $E4,$63,$82,$ED,$91,$18
                DEFB      $E8,$C6,$00    ,$91,$18
                DEFB  $09,$F5,$86,$82,$ED,$91,$4D
                DEFB      $FB,$0D,$84,$E7,$91,$4D
                DEFB      $F5,$86,$83,$E8,$91,$4D
                DEFB      $FB,$0D,$82,$ED,$91,$4D
                DEFB      $F5,$86,$82,$ED,$91,$4D
                DEFB      $FB,$0D,$84,$E7,$91,$4D
                DEFB  $FF  ; End of Pattern

PAT23:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $E5,$DB,$83,$E8,$90,$8C
                DEFB      $EB,$B6,$82,$ED,$90,$8C
                DEFB      $E5,$DB,$00    ,$90,$8C
                DEFB      $EB,$B6,$01    ,$90,$8C
                DEFB      $E5,$DB,$01    ,$90,$8C
                DEFB      $EB,$B6,$01    ,$90,$8C
                DEFB  $09,$D5,$86,$82,$ED,$90,$8C
                DEFB      $DB,$0D,$84,$E7,$90,$8C
                DEFB      $D5,$86,$83,$E8,$90,$8C
                DEFB      $DB,$0D,$82,$ED,$90,$8C
                DEFB      $D5,$86,$82,$ED,$90,$8C
                DEFB      $DB,$0D,$84,$E7,$90,$8C
                DEFB      $C4,$E7,$83,$E8,$90,$7D
                DEFB      $C9,$D9,$82,$ED,$90,$7D
                DEFB      $C4,$E7,$00    ,$90,$7D
                DEFB      $C9,$D9,$01    ,$90,$7D
                DEFB      $C4,$E7,$01    ,$90,$7D
                DEFB      $C9,$D9,$01    ,$90,$7D
                DEFB  $09,$B4,$63,$82,$ED,$90,$8C
                DEFB      $B8,$C6,$84,$E7,$90,$8C
                DEFB      $B4,$63,$83,$E8,$90,$8C
                DEFB      $B8,$C6,$82,$ED,$90,$8C
                DEFB      $B4,$63,$82,$ED,$90,$8C
                DEFB      $B8,$C6,$00    ,$90,$8C
                DEFB  $03,$A3,$B0,$82,$ED,$84,$63
                DEFB      $A7,$60,$84,$E7,$82,$31
                DEFB      $A3,$B0,$83,$E8,$81,$76
                DEFB      $A7,$60,$82,$ED,$81,$18
                DEFB      $A3,$B0,$82,$ED,$00
                DEFB      $A7,$60,$84,$E7,$01
                DEFB      $93,$49,$83,$E8,$90,$8C
                DEFB      $96,$92,$82,$ED,$90,$8C
                DEFB      $93,$49,$00    ,$90,$8C
                DEFB      $96,$92,$01    ,$90,$8C
                DEFB      $93,$49,$01    ,$90,$8C
                DEFB      $96,$92,$01    ,$90,$8C
                DEFB      $83,$B0,$82,$ED,$90,$D2
                DEFB      $87,$60,$84,$E7,$90,$D2
                DEFB      $83,$B0,$83,$E8,$90,$D2
                DEFB      $87,$60,$82,$ED,$90,$D2
                DEFB      $83,$B0,$82,$ED,$90,$D2
                DEFB      $87,$60,$84,$E7,$90,$D2
                DEFB  $09,$84,$63,$83,$E8,$90,$FA
                DEFB      $88,$C6,$82,$ED,$90,$FA
                DEFB      $84,$63,$00    ,$90,$FA
                DEFB      $88,$C6,$01    ,$90,$FA
                DEFB      $84,$63,$01    ,$90,$FA
                DEFB      $88,$C6,$01    ,$90,$FA
                DEFB  $FF  ; End of Pattern

PAT24:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$89,$D9,$83,$49,$90,$8C
                DEFB      $84,$E7,$85,$86,$90,$8C
                DEFB      $89,$D9,$84,$63,$90,$8C
                DEFB      $84,$E7,$83,$49,$90,$8C
                DEFB      $89,$D9,$83,$49,$90,$8C
                DEFB      $84,$E7,$85,$86,$90,$8C
                DEFB      $99,$D9,$84,$63,$90,$8C
                DEFB      $94,$E7,$83,$49,$90,$8C
                DEFB      $99,$D9,$00    ,$90,$8C
                DEFB      $94,$E7,$01    ,$90,$8C
                DEFB      $99,$D9,$01    ,$90,$8C
                DEFB      $94,$E7,$01    ,$00
                DEFB      $A9,$D9,$83,$49,$90,$7D
                DEFB      $A4,$E7,$85,$86,$90,$7D
                DEFB      $A9,$D9,$84,$63,$90,$7D
                DEFB      $A4,$E7,$83,$49,$90,$7D
                DEFB      $A9,$D9,$83,$49,$90,$7D
                DEFB      $00    ,$00    ,$00
                DEFB  $09,$B9,$D9,$83,$49,$90,$8C
                DEFB      $B4,$E7,$85,$86,$90,$8C
                DEFB      $B9,$D9,$84,$63,$90,$8C
                DEFB      $B4,$E7,$83,$49,$90,$8C
                DEFB      $B9,$D9,$83,$49,$90,$8C
                DEFB      $B4,$E7,$00    ,$00
                DEFB  $03,$C9,$D9,$83,$49,$84,$63
                DEFB      $C4,$E7,$85,$86,$82,$31
                DEFB      $C9,$D9,$84,$63,$81,$76
                DEFB      $C4,$E7,$83,$49,$81,$18
                DEFB      $C9,$D9,$83,$49,$00
                DEFB      $C4,$E7,$85,$86,$01
                DEFB      $D9,$D9,$84,$63,$90,$8C
                DEFB      $D4,$E7,$83,$49,$90,$8C
                DEFB      $D9,$D9,$00    ,$90,$8C
                DEFB      $D4,$E7,$01    ,$90,$8C
                DEFB      $D9,$D9,$01    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB      $E9,$D9,$83,$49,$90,$D2
                DEFB      $E4,$E7,$85,$86,$90,$D2
                DEFB      $E9,$D9,$84,$63,$90,$D2
                DEFB      $E4,$E7,$83,$49,$90,$D2
                DEFB      $E9,$D9,$83,$49,$90,$D2
                DEFB      $E4,$E7,$00    ,$90,$D2
                DEFB  $09,$F8,$C6,$83,$49,$90,$FA
                DEFB      $F4,$63,$85,$DB,$90,$FA
                DEFB      $F8,$C6,$84,$63,$90,$FA
                DEFB      $F4,$63,$83,$49,$90,$FA
                DEFB      $F8,$C6,$83,$49,$90,$FA
                DEFB      $F4,$63,$85,$DB,$90,$FA
                DEFB  $FF  ; End of Pattern

PAT25:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $E8,$C6,$84,$63,$90,$8C
                DEFB      $E4,$63,$83,$49,$90,$8C
                DEFB      $E8,$C6,$00    ,$90,$8C
                DEFB      $E4,$63,$01    ,$90,$8C
                DEFB      $E8,$C6,$01    ,$90,$8C
                DEFB      $E4,$63,$01    ,$90,$8C
                DEFB  $09,$D8,$C6,$83,$49,$90,$8C
                DEFB      $D4,$63,$85,$DB,$90,$8C
                DEFB      $D8,$C6,$84,$63,$90,$8C
                DEFB      $D4,$63,$83,$49,$90,$8C
                DEFB      $D8,$C6,$83,$49,$90,$8C
                DEFB      $00    ,$85,$DB,$90,$8C
                DEFB      $C8,$C6,$84,$63,$91,$18
                DEFB      $C4,$63,$83,$49,$91,$18
                DEFB      $C8,$C6,$00    ,$91,$18
                DEFB      $C4,$63,$01    ,$91,$18
                DEFB      $C8,$C6,$01    ,$91,$18
                DEFB      $C4,$63,$01    ,$91,$18
                DEFB  $09,$B8,$C6,$83,$49,$90,$8C
                DEFB      $B4,$63,$85,$DB,$90,$8C
                DEFB      $B8,$C6,$84,$63,$90,$8C
                DEFB      $B4,$63,$83,$49,$90,$8C
                DEFB      $B8,$C6,$83,$49,$90,$8C
                DEFB      $00    ,$00    ,$90,$8C
                DEFB  $03,$A8,$C6,$83,$49,$84,$63
                DEFB      $A4,$63,$85,$DB,$82,$31
                DEFB      $A8,$C6,$84,$63,$81,$76
                DEFB      $A4,$63,$83,$49,$81,$18
                DEFB      $A8,$C6,$83,$49,$00
                DEFB      $A4,$63,$85,$DB,$01
                DEFB      $98,$C6,$84,$63,$90,$8C
                DEFB      $94,$63,$83,$49,$90,$8C
                DEFB      $98,$C6,$00    ,$90,$8C
                DEFB      $94,$63,$01    ,$90,$8C
                DEFB      $98,$C6,$01    ,$90,$8C
                DEFB      $94,$63,$01    ,$90,$8C
                DEFB      $88,$C6,$83,$49,$90,$A6
                DEFB      $84,$63,$85,$DB,$90,$A6
                DEFB      $88,$C6,$84,$63,$90,$A6
                DEFB      $84,$63,$83,$49,$90,$A6
                DEFB      $88,$C6,$83,$49,$90,$A6
                DEFB      $00    ,$85,$DB,$90,$A6
                DEFB  $09,$88,$C6,$84,$63,$90,$B0
                DEFB      $84,$63,$83,$49,$90,$B0
                DEFB      $88,$C6,$00    ,$90,$B0
                DEFB      $84,$63,$01    ,$90,$B0
                DEFB      $88,$C6,$01    ,$90,$B0
                DEFB      $84,$63,$01    ,$90,$B0
                DEFB  $FF  ; End of Pattern

PAT26:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$86,$92,$82,$ED,$90,$BB
                DEFB      $83,$49,$85,$DB,$90,$BB
                DEFB      $86,$92,$83,$E8,$90,$BB
                DEFB      $83,$49,$82,$ED,$90,$BB
                DEFB      $86,$92,$82,$ED,$90,$BB
                DEFB      $83,$49,$00    ,$90,$BB
                DEFB      $96,$92,$82,$ED,$90,$BB
                DEFB      $93,$49,$85,$DB,$90,$BB
                DEFB      $96,$92,$83,$E8,$90,$BB
                DEFB      $93,$49,$82,$ED,$90,$BB
                DEFB      $96,$92,$82,$ED,$90,$BB
                DEFB      $93,$49,$00    ,$90,$BB
                DEFB  $09,$A7,$60,$82,$ED,$90,$A6
                DEFB      $A3,$B0,$85,$DB,$90,$A6
                DEFB      $A7,$60,$83,$E8,$90,$A6
                DEFB      $A3,$B0,$82,$ED,$90,$A6
                DEFB      $A7,$60,$82,$ED,$90,$A6
                DEFB      $A3,$B0,$00    ,$90,$A6
                DEFB  $09,$B7,$60,$82,$ED,$90,$BB
                DEFB      $B3,$B0,$85,$DB,$90,$BB
                DEFB      $B7,$60,$83,$E8,$90,$BB
                DEFB      $B3,$B0,$82,$ED,$90,$BB
                DEFB      $B7,$60,$82,$ED,$90,$BB
                DEFB      $B3,$B0,$00    ,$90,$BB
                DEFB  $03,$C8,$C6,$82,$ED,$84,$63
                DEFB      $C4,$63,$85,$DB,$82,$31
                DEFB      $C8,$C6,$83,$E8,$81,$76
                DEFB      $C4,$63,$82,$ED,$81,$18
                DEFB      $C8,$C6,$82,$ED,$00
                DEFB      $C4,$63,$85,$DB,$01
                DEFB      $D8,$C6,$83,$E8,$90,$BB
                DEFB      $D4,$63,$82,$ED,$90,$BB
                DEFB      $D8,$C6,$00    ,$90,$BB
                DEFB      $D4,$63,$01    ,$90,$BB
                DEFB      $D8,$C6,$01    ,$90,$BB
                DEFB      $00    ,$01    ,$90,$BB
                DEFB      $E8,$C6,$82,$ED,$91,$18
                DEFB      $E4,$63,$85,$DB,$91,$18
                DEFB      $E8,$C6,$83,$E8,$91,$18
                DEFB      $E4,$63,$82,$ED,$91,$18
                DEFB      $E8,$C6,$82,$ED,$91,$18
                DEFB      $E4,$63,$00    ,$91,$18
                DEFB  $09,$F6,$92,$82,$ED,$91,$4D
                DEFB      $F3,$49,$84,$E7,$91,$4D
                DEFB      $F6,$92,$83,$E8,$91,$4D
                DEFB      $F3,$49,$82,$ED,$91,$4D
                DEFB      $F6,$92,$82,$ED,$91,$4D
                DEFB      $F3,$49,$84,$E7,$91,$4D
                DEFB  $FF  ; End of Pattern

PAT27:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $E3,$49,$83,$E8,$90,$8C
                DEFB      $E6,$92,$82,$ED,$90,$8C
                DEFB      $E3,$49,$00    ,$90,$8C
                DEFB      $E6,$92,$01    ,$90,$8C
                DEFB      $E3,$49,$01    ,$90,$8C
                DEFB      $E6,$92,$01    ,$90,$8C
                DEFB  $09,$D3,$49,$82,$ED,$90,$8C
                DEFB      $D6,$92,$84,$E7,$90,$8C
                DEFB      $D3,$49,$83,$E8,$90,$8C
                DEFB      $D6,$92,$82,$ED,$90,$8C
                DEFB      $D3,$49,$82,$ED,$90,$8C
                DEFB      $D6,$92,$84,$E7,$90,$8C
                DEFB      $C3,$B0,$83,$E8,$90,$7D
                DEFB      $C7,$60,$82,$ED,$90,$7D
                DEFB      $C3,$B0,$00    ,$90,$7D
                DEFB      $C7,$60,$01    ,$90,$7D
                DEFB      $C3,$B0,$01    ,$90,$7D
                DEFB      $C7,$60,$01    ,$90,$7D
                DEFB  $09,$B3,$B0,$82,$ED,$90,$8C
                DEFB      $B7,$60,$84,$E7,$90,$8C
                DEFB      $B3,$B0,$83,$E8,$90,$8C
                DEFB      $B7,$60,$82,$ED,$90,$8C
                DEFB      $B3,$B0,$82,$ED,$90,$8C
                DEFB      $B7,$60,$00    ,$90,$8C
                DEFB  $03,$A4,$63,$82,$ED,$84,$63
                DEFB      $A8,$C6,$84,$E7,$82,$31
                DEFB      $A4,$63,$83,$E8,$81,$76
                DEFB      $A8,$C6,$82,$ED,$81,$18
                DEFB      $A4,$63,$82,$ED,$00
                DEFB      $A8,$C6,$84,$E7,$01
                DEFB      $94,$63,$83,$E8,$90,$8C
                DEFB      $98,$C6,$82,$ED,$90,$8C
                DEFB      $94,$63,$00    ,$90,$8C
                DEFB      $98,$C6,$01    ,$90,$8C
                DEFB      $94,$63,$01    ,$90,$8C
                DEFB      $98,$C6,$01    ,$90,$8C
                DEFB      $84,$E7,$82,$ED,$90,$D2
                DEFB      $89,$D9,$84,$E7,$90,$D2
                DEFB      $84,$E7,$83,$E8,$90,$D2
                DEFB      $89,$D9,$82,$ED,$90,$D2
                DEFB      $84,$E7,$82,$ED,$90,$D2
                DEFB      $89,$D9,$84,$E7,$90,$D2
                DEFB  $09,$84,$E7,$83,$E8,$90,$FA
                DEFB      $89,$D9,$82,$ED,$90,$FA
                DEFB      $84,$E7,$00    ,$90,$FA
                DEFB      $89,$D9,$01    ,$90,$FA
                DEFB      $84,$E7,$01    ,$90,$FA
                DEFB      $00    ,$01    ,$90,$FA
                DEFB  $FF  ; End of Pattern

PAT28:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$8B,$B6,$82,$ED,$90,$BB
                DEFB      $85,$DB,$85,$DB,$90,$BB
                DEFB      $8B,$B6,$83,$E8,$90,$BB
                DEFB      $85,$DB,$82,$ED,$90,$BB
                DEFB      $8B,$B6,$82,$ED,$90,$BB
                DEFB      $00    ,$00    ,$90,$BB
                DEFB      $9B,$B6,$82,$ED,$90,$BB
                DEFB      $95,$DB,$85,$DB,$90,$BB
                DEFB      $9B,$B6,$83,$E8,$90,$BB
                DEFB      $95,$DB,$82,$ED,$90,$BB
                DEFB      $9B,$B6,$82,$ED,$90,$BB
                DEFB      $95,$DB,$00    ,$90,$BB
                DEFB  $09,$AB,$0D,$82,$ED,$90,$A6
                DEFB      $A5,$86,$85,$DB,$90,$A6
                DEFB      $AB,$0D,$83,$E8,$90,$A6
                DEFB      $A5,$86,$82,$ED,$90,$A6
                DEFB      $AB,$0D,$82,$ED,$90,$A6
                DEFB      $A5,$86,$00    ,$90,$A6
                DEFB  $09,$BB,$0D,$82,$ED,$90,$BB
                DEFB      $B5,$86,$85,$DB,$90,$BB
                DEFB      $BB,$0D,$83,$E8,$90,$BB
                DEFB      $B5,$86,$82,$ED,$90,$BB
                DEFB      $BB,$0D,$82,$ED,$90,$BB
                DEFB      $B5,$86,$00    ,$90,$BB
                DEFB  $03,$C9,$D9,$82,$ED,$84,$63
                DEFB      $C4,$E7,$85,$DB,$82,$31
                DEFB      $C9,$D9,$83,$E8,$81,$76
                DEFB      $C4,$E7,$82,$ED,$81,$18
                DEFB      $C9,$D9,$82,$ED,$00
                DEFB      $C4,$E7,$85,$DB,$01
                DEFB      $D9,$D9,$83,$E8,$90,$BB
                DEFB      $D4,$E7,$82,$ED,$90,$BB
                DEFB      $D9,$D9,$00    ,$90,$BB
                DEFB      $D4,$E7,$01    ,$90,$BB
                DEFB      $D9,$D9,$01    ,$90,$BB
                DEFB      $D4,$E7,$01    ,$90,$BB
                DEFB      $E8,$C6,$82,$ED,$91,$18
                DEFB      $E4,$63,$85,$DB,$91,$18
                DEFB      $E8,$C6,$83,$E8,$91,$18
                DEFB      $E4,$63,$82,$ED,$91,$18
                DEFB      $E8,$C6,$82,$ED,$91,$18
                DEFB      $E4,$63,$00    ,$91,$18
                DEFB  $09,$F6,$92,$82,$ED,$91,$4D
                DEFB      $F3,$49,$84,$E7,$91,$4D
                DEFB      $F6,$92,$83,$E8,$91,$4D
                DEFB      $F3,$49,$82,$ED,$91,$4D
                DEFB      $F6,$92,$82,$ED,$91,$4D
                DEFB      $F3,$49,$84,$E7,$91,$4D
                DEFB  $FF  ; End of Pattern

PAT29:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$83,$B0,$82,$ED,$90,$9D
                DEFB      $87,$60,$84,$E7,$90,$9D
                DEFB      $83,$B0,$83,$B0,$90,$9D
                DEFB      $87,$60,$82,$ED,$90,$9D
                DEFB      $83,$B0,$82,$ED,$90,$9D
                DEFB      $87,$60,$84,$E7,$90,$9D
                DEFB      $83,$B0,$83,$B0,$90,$9D
                DEFB      $87,$60,$82,$ED,$90,$9D
                DEFB      $83,$B0,$00    ,$90,$9D
                DEFB      $87,$60,$01    ,$90,$9D
                DEFB      $83,$B0,$01    ,$90,$9D
                DEFB      $00    ,$01    ,$90,$9D
                DEFB      $94,$E7,$82,$ED,$90,$8C
                DEFB      $99,$D9,$84,$E7,$90,$8C
                DEFB      $94,$E7,$83,$B0,$90,$8C
                DEFB      $99,$D9,$82,$ED,$90,$8C
                DEFB      $94,$E7,$82,$ED,$90,$8C
                DEFB      $99,$D9,$00    ,$90,$8C
                DEFB      $00    ,$82,$ED,$90,$9D
                DEFB      $01    ,$84,$E7,$90,$9D
                DEFB      $01    ,$83,$B0,$90,$9D
                DEFB      $01    ,$82,$ED,$90,$9D
                DEFB      $01    ,$82,$ED,$90,$9D
                DEFB      $01    ,$00    ,$90,$9D
                DEFB  $03,$A3,$B0,$82,$ED,$84,$63
                DEFB      $A7,$60,$84,$E7,$82,$31
                DEFB      $A3,$B0,$83,$B0,$81,$76
                DEFB      $A7,$60,$82,$ED,$81,$18
                DEFB      $A3,$B0,$82,$ED,$00
                DEFB      $A7,$60,$84,$E7,$01
                DEFB      $00    ,$83,$B0,$90,$8C
                DEFB      $01    ,$82,$ED,$90,$8C
                DEFB      $01    ,$00    ,$90,$8C
                DEFB      $01    ,$01    ,$90,$8C
                DEFB      $01    ,$01    ,$90,$8C
                DEFB      $01    ,$01    ,$90,$8C
                DEFB      $B3,$B0,$82,$ED,$90,$9D
                DEFB      $B7,$60,$84,$E7,$90,$9D
                DEFB      $B3,$B0,$83,$B0,$90,$9D
                DEFB      $B7,$60,$82,$ED,$90,$9D
                DEFB      $B3,$B0,$82,$ED,$90,$9D
                DEFB      $00    ,$00    ,$90,$9D
                DEFB  $09,$C3,$B0,$82,$ED,$90,$8C
                DEFB      $C7,$60,$84,$E7,$90,$8C
                DEFB      $C3,$B0,$83,$B0,$90,$8C
                DEFB      $C7,$60,$82,$ED,$90,$8C
                DEFB      $C3,$B0,$82,$ED,$90,$8C
                DEFB      $C7,$60,$84,$E7,$90,$8C
                DEFB      $C3,$B0,$83,$B0,$90,$9D
                DEFB      $C7,$60,$82,$ED,$90,$9D
                DEFB      $C3,$B0,$00    ,$90,$9D
                DEFB      $C7,$60,$01    ,$90,$9D
                DEFB      $C3,$B0,$01    ,$90,$9D
                DEFB      $00    ,$01    ,$90,$9D
                DEFB  $09,$D3,$B0,$82,$ED,$90,$9D
                DEFB      $D7,$60,$84,$E7,$90,$9D
                DEFB      $D3,$B0,$83,$B0,$90,$9D
                DEFB      $D7,$60,$82,$ED,$90,$9D
                DEFB      $D3,$B0,$82,$ED,$90,$9D
                DEFB      $D7,$60,$84,$E7,$90,$9D
                DEFB      $00    ,$83,$B0,$90,$8C
                DEFB      $01    ,$82,$ED,$90,$8C
                DEFB      $01    ,$00    ,$90,$8C
                DEFB      $01    ,$01    ,$90,$8C
                DEFB  $FF  ; End of Pattern

PAT30:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$D3,$B0,$82,$ED,$90,$BB
                DEFB      $D7,$60,$83,$B0,$90,$BB
                DEFB      $D3,$B0,$84,$63,$90,$BB
                DEFB      $D7,$60,$82,$ED,$90,$BB
                DEFB      $D3,$B0,$82,$ED,$90,$BB
                DEFB      $D7,$60,$00    ,$90,$BB
                DEFB      $D3,$B0,$82,$ED,$90,$BB
                DEFB      $D7,$60,$83,$B0,$90,$BB
                DEFB      $D3,$B0,$84,$63,$90,$BB
                DEFB      $D7,$60,$82,$ED,$90,$BB
                DEFB      $D3,$B0,$82,$ED,$90,$BB
                DEFB      $00    ,$00    ,$90,$BB
                DEFB      $C4,$63,$82,$ED,$90,$A6
                DEFB      $C8,$C6,$83,$B0,$90,$A6
                DEFB      $C4,$63,$84,$63,$90,$A6
                DEFB      $C8,$C6,$82,$ED,$90,$A6
                DEFB      $C4,$63,$82,$ED,$90,$A6
                DEFB      $C8,$C6,$00    ,$90,$A6
                DEFB      $00    ,$82,$ED,$90,$BB
                DEFB      $01    ,$83,$B0,$90,$BB
                DEFB      $01    ,$84,$63,$90,$BB
                DEFB      $01    ,$82,$ED,$90,$BB
                DEFB      $01    ,$82,$ED,$90,$BB
                DEFB      $01    ,$00    ,$90,$BB
                DEFB  $03,$B3,$B0,$82,$ED,$84,$63
                DEFB      $B7,$60,$83,$B0,$82,$31
                DEFB      $B3,$B0,$84,$63,$81,$76
                DEFB      $B7,$60,$82,$ED,$81,$18
                DEFB      $B3,$B0,$82,$ED,$00
                DEFB      $B7,$60,$83,$B0,$01
                DEFB      $00    ,$84,$63,$90,$BB
                DEFB      $01    ,$82,$ED,$90,$BB
                DEFB      $01    ,$00    ,$90,$BB
                DEFB      $01    ,$01    ,$90,$BB
                DEFB      $01    ,$01    ,$90,$BB
                DEFB      $01    ,$01    ,$90,$BB
                DEFB      $A3,$B0,$82,$ED,$90,$FA
                DEFB      $A7,$60,$83,$B0,$90,$FA
                DEFB      $A3,$B0,$84,$63,$90,$FA
                DEFB      $A7,$60,$82,$ED,$90,$FA
                DEFB      $A3,$B0,$82,$ED,$90,$FA
                DEFB      $00    ,$00    ,$90,$FA
                DEFB  $09,$93,$B0,$82,$ED,$90,$BB
                DEFB      $97,$60,$83,$B0,$90,$BB
                DEFB      $93,$B0,$84,$63,$90,$BB
                DEFB      $97,$60,$82,$ED,$90,$BB
                DEFB      $93,$B0,$82,$ED,$90,$BB
                DEFB      $97,$60,$83,$B0,$90,$BB
                DEFB      $93,$B0,$84,$63,$91,$08
                DEFB      $97,$60,$82,$ED,$91,$08
                DEFB      $93,$B0,$00    ,$91,$08
                DEFB      $97,$60,$01    ,$91,$08
                DEFB      $93,$B0,$01    ,$91,$08
                DEFB      $00    ,$01    ,$91,$08
                DEFB  $09,$83,$B0,$82,$ED,$90,$BB
                DEFB      $87,$60,$83,$B0,$90,$BB
                DEFB      $83,$B0,$84,$63,$90,$BB
                DEFB      $87,$60,$82,$ED,$90,$BB
                DEFB      $83,$B0,$82,$ED,$90,$BB
                DEFB      $87,$60,$83,$B0,$90,$BB
                DEFB      $00    ,$84,$63,$90,$BB
                DEFB      $01    ,$82,$ED,$90,$BB
                DEFB      $01    ,$00    ,$90,$BB
                DEFB      $01    ,$01    ,$90,$BB
                DEFB  $FF  ; End of Pattern

PAT31:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$C3,$49,$83,$49,$90,$8C
                DEFB      $C6,$92,$85,$86,$90,$8C
                DEFB      $C3,$49,$84,$63,$90,$8C
                DEFB      $C6,$92,$83,$49,$90,$8C
                DEFB      $C3,$49,$83,$49,$90,$8C
                DEFB      $C6,$92,$85,$86,$90,$8C
                DEFB      $C3,$49,$84,$63,$90,$8C
                DEFB      $C6,$92,$83,$49,$90,$8C
                DEFB      $C3,$49,$00    ,$90,$8C
                DEFB      $C6,$92,$01    ,$90,$8C
                DEFB      $C3,$49,$01    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB      $D4,$63,$83,$49,$90,$7D
                DEFB      $D8,$C6,$85,$86,$90,$7D
                DEFB      $D4,$63,$84,$63,$90,$7D
                DEFB      $D8,$C6,$83,$49,$90,$7D
                DEFB      $D4,$63,$83,$49,$90,$7D
                DEFB      $D8,$C6,$00    ,$90,$7D
                DEFB      $00    ,$83,$49,$90,$8C
                DEFB      $01    ,$85,$86,$90,$8C
                DEFB      $01    ,$84,$63,$90,$8C
                DEFB      $01    ,$83,$49,$90,$8C
                DEFB      $01    ,$83,$49,$90,$8C
                DEFB      $01    ,$00    ,$90,$8C
                DEFB  $03,$E3,$49,$83,$49,$84,$63
                DEFB      $E6,$92,$85,$86,$82,$31
                DEFB      $E3,$49,$84,$63,$81,$76
                DEFB      $E6,$92,$83,$49,$81,$18
                DEFB      $E3,$49,$83,$49,$00
                DEFB      $E6,$92,$85,$86,$01
                DEFB      $00    ,$84,$63,$90,$7D
                DEFB      $01    ,$83,$49,$90,$7D
                DEFB      $01    ,$00    ,$90,$7D
                DEFB      $01    ,$01    ,$90,$7D
                DEFB      $01    ,$01    ,$90,$7D
                DEFB      $01    ,$01    ,$90,$7D
                DEFB      $F3,$49,$83,$49,$90,$8C
                DEFB      $F6,$92,$85,$86,$90,$8C
                DEFB      $F3,$49,$84,$63,$90,$8C
                DEFB      $F6,$92,$83,$49,$90,$8C
                DEFB      $F3,$49,$83,$49,$90,$8C
                DEFB      $00    ,$00    ,$90,$8C
                DEFB  $09,$E3,$49,$83,$49,$90,$7D
                DEFB      $E6,$92,$85,$86,$90,$7D
                DEFB      $E3,$49,$84,$63,$90,$7D
                DEFB      $E6,$92,$83,$49,$90,$7D
                DEFB      $E3,$49,$83,$49,$90,$7D
                DEFB      $E6,$92,$85,$86,$90,$7D
                DEFB      $E3,$49,$84,$63,$90,$8C
                DEFB      $E6,$92,$83,$49,$90,$8C
                DEFB      $E3,$49,$00    ,$90,$8C
                DEFB      $E6,$92,$01    ,$90,$8C
                DEFB      $E3,$49,$01    ,$90,$8C
                DEFB      $00    ,$01    ,$90,$8C
                DEFB  $09,$D3,$49,$83,$49,$90,$8C
                DEFB      $D6,$92,$85,$86,$90,$8C
                DEFB      $D3,$49,$84,$63,$90,$8C
                DEFB      $D6,$92,$83,$49,$90,$8C
                DEFB      $D3,$49,$83,$49,$90,$8C
                DEFB      $D6,$92,$85,$86,$90,$8C
                DEFB      $00    ,$84,$63,$90,$7D
                DEFB      $01    ,$83,$49,$90,$7D
                DEFB      $01    ,$00    ,$90,$7D
                DEFB      $01    ,$01    ,$90,$7D
                DEFB  $FF  ; End of Pattern

PAT32:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$93,$49,$83,$49,$90,$D2
                DEFB      $96,$92,$84,$E7,$90,$D2
                DEFB      $93,$49,$83,$E8,$90,$D2
                DEFB      $96,$92,$83,$49,$90,$D2
                DEFB      $93,$49,$83,$49,$90,$D2
                DEFB      $00    ,$00    ,$90,$D2
                DEFB      $83,$49,$83,$49,$90,$D2
                DEFB      $86,$92,$84,$E7,$90,$D2
                DEFB      $83,$49,$83,$E8,$90,$D2
                DEFB      $86,$92,$83,$49,$90,$D2
                DEFB      $83,$49,$83,$49,$90,$D2
                DEFB      $00    ,$00    ,$90,$D2
                DEFB  $09,$93,$E8,$83,$49,$90,$BB
                DEFB      $97,$D0,$84,$E7,$90,$BB
                DEFB      $93,$E8,$83,$E8,$90,$BB
                DEFB      $97,$D0,$83,$49,$90,$BB
                DEFB      $93,$E8,$83,$49,$90,$BB
                DEFB      $00    ,$00    ,$90,$BB
                DEFB      $A3,$49,$83,$49,$90,$D2
                DEFB      $A6,$92,$84,$E7,$90,$D2
                DEFB      $A3,$49,$83,$E8,$90,$D2
                DEFB      $A6,$92,$83,$49,$90,$D2
                DEFB      $A3,$49,$83,$49,$90,$D2
                DEFB      $00    ,$00    ,$90,$D2
                DEFB  $03,$B3,$49,$83,$49,$84,$63
                DEFB      $B6,$92,$84,$E7,$82,$31
                DEFB      $B3,$49,$83,$E8,$81,$76
                DEFB      $B6,$92,$83,$49,$81,$18
                DEFB      $B3,$49,$83,$49,$00
                DEFB      $B6,$92,$84,$E7,$01
                DEFB      $00    ,$83,$E8,$90,$BB
                DEFB      $01    ,$83,$49,$90,$BB
                DEFB      $01    ,$00    ,$90,$BB
                DEFB      $01    ,$01    ,$90,$BB
                DEFB      $01    ,$01    ,$90,$BB
                DEFB      $01    ,$01    ,$90,$BB
                DEFB  $09,$C3,$49,$83,$49,$90,$FA
                DEFB      $C6,$92,$84,$E7,$90,$FA
                DEFB      $C3,$49,$83,$E8,$90,$FA
                DEFB      $C6,$92,$83,$49,$90,$FA
                DEFB      $C3,$49,$83,$49,$90,$FA
                DEFB      $00    ,$00    ,$90,$FA
                DEFB  $09,$D3,$E8,$83,$49,$90,$D2
                DEFB      $D7,$D0,$84,$E7,$90,$D2
                DEFB      $D3,$E8,$83,$E8,$90,$D2
                DEFB      $D7,$D0,$83,$49,$90,$D2
                DEFB      $D3,$E8,$83,$49,$90,$D2
                DEFB      $D7,$D0,$84,$E7,$90,$D2
                DEFB      $00    ,$83,$E8,$90,$BB
                DEFB      $01    ,$83,$49,$90,$BB
                DEFB      $01    ,$00    ,$90,$BB
                DEFB      $01    ,$01    ,$90,$BB
                DEFB      $01    ,$01    ,$90,$BB
                DEFB      $01    ,$01    ,$90,$BB
                DEFB  $09,$E3,$E8,$83,$49,$90,$9D
                DEFB      $E7,$D0,$84,$E7,$90,$9D
                DEFB      $E3,$E8,$83,$E8,$90,$9D
                DEFB      $E7,$D0,$83,$49,$90,$9D
                DEFB      $E3,$E8,$83,$49,$90,$9D
                DEFB      $E7,$D0,$84,$E7,$90,$9D
                DEFB      $00    ,$83,$E8,$90,$8C
                DEFB      $01    ,$83,$49,$90,$8C
                DEFB      $01    ,$00    ,$90,$8C
                DEFB      $01    ,$01    ,$90,$8C
                DEFB  $FF  ; End of Pattern

PAT33:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$01    ,$90,$8C
                DEFB      $01    ,$01    ,$90,$8C
                DEFB  $09,$E3,$B0,$82,$ED,$90,$9D
                DEFB      $E7,$60,$84,$E7,$90,$9D
                DEFB      $E3,$B0,$83,$B0,$90,$9D
                DEFB      $E7,$60,$82,$ED,$90,$9D
                DEFB      $E3,$B0,$82,$ED,$90,$9D
                DEFB      $00    ,$00    ,$90,$9D
                DEFB  $03,$F4,$E7,$82,$ED,$84,$63
                DEFB      $F9,$D9,$84,$E7,$82,$31
                DEFB      $F4,$E7,$83,$B0,$81,$76
                DEFB      $F9,$D9,$82,$ED,$80,$8C
                DEFB      $F4,$E7,$82,$ED,$00
                DEFB      $F9,$D9,$84,$E7,$01
                DEFB      $00    ,$83,$B0,$B0,$9D
                DEFB      $01    ,$82,$ED,$B0,$9D
                DEFB      $01    ,$00    ,$B0,$9D
                DEFB      $01    ,$01    ,$B0,$9D
                DEFB      $01    ,$01    ,$B0,$9D
                DEFB      $01    ,$01    ,$B0,$9D
                DEFB  $09,$E3,$B0,$82,$ED,$B0,$A6
                DEFB      $E7,$60,$84,$E7,$B0,$A6
                DEFB      $E3,$B0,$83,$B0,$B0,$A6
                DEFB      $E7,$60,$82,$ED,$B0,$A6
                DEFB      $E3,$B0,$82,$ED,$B0,$A6
                DEFB      $E7,$60,$84,$E7,$B0,$A6
                DEFB  $09,$00    ,$83,$B0,$B0,$B0
                DEFB      $01    ,$82,$ED,$B0,$B0
                DEFB      $01    ,$00    ,$B0,$B0
                DEFB      $01    ,$01    ,$B0,$B0
                DEFB      $01    ,$01    ,$B0,$B0
                DEFB      $01    ,$01    ,$B0,$B0
                DEFB  $FF  ; End of Pattern

PAT34:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$01    ,$90,$BB
                DEFB      $01    ,$01    ,$90,$BB
                DEFB  $09,$93,$B0,$82,$ED,$91,$18
                DEFB      $97,$60,$83,$B0,$91,$18
                DEFB      $93,$B0,$84,$63,$91,$18
                DEFB      $97,$60,$82,$ED,$91,$18
                DEFB      $93,$B0,$82,$ED,$91,$18
                DEFB      $00    ,$00    ,$91,$18
                DEFB  $03,$A4,$63,$82,$ED,$84,$63
                DEFB      $A8,$C6,$83,$B0,$82,$31
                DEFB      $A4,$63,$84,$63,$81,$76
                DEFB      $A8,$C6,$82,$ED,$81,$18
                DEFB      $A4,$63,$82,$ED,$00
                DEFB      $A8,$C6,$83,$B0,$01
                DEFB      $00    ,$84,$63,$B0,$BB
                DEFB      $01    ,$82,$ED,$B0,$BB
                DEFB      $01    ,$00    ,$B0,$BB
                DEFB      $01    ,$01    ,$B0,$BB
                DEFB      $01    ,$01    ,$B0,$BB
                DEFB      $01    ,$01    ,$B0,$BB
                DEFB  $09,$B3,$B0,$82,$ED,$B0,$A6
                DEFB      $B7,$60,$83,$B0,$B0,$A6
                DEFB      $B3,$B0,$84,$63,$B0,$A6
                DEFB      $B7,$60,$82,$ED,$B0,$A6
                DEFB      $B3,$B0,$82,$ED,$B0,$A6
                DEFB      $B7,$60,$83,$B0,$B0,$A6
                DEFB  $09,$00    ,$84,$63,$B0,$8C
                DEFB      $01    ,$82,$ED,$B0,$8C
                DEFB      $01    ,$00    ,$B0,$8C
                DEFB      $01    ,$01    ,$B0,$8C
                DEFB      $01    ,$01    ,$B0,$8C
                DEFB      $01    ,$01    ,$B0,$8C
                DEFB  $FF  ; End of Pattern

PAT35:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$01    ,$90,$7D
                DEFB      $01    ,$01    ,$90,$7D
                DEFB  $09,$C3,$49,$83,$49,$90,$8C
                DEFB      $C6,$92,$85,$86,$90,$8C
                DEFB      $C3,$49,$84,$63,$90,$8C
                DEFB      $C6,$92,$83,$49,$90,$8C
                DEFB      $C3,$49,$83,$49,$90,$8C
                DEFB      $00    ,$00    ,$90,$8C
                DEFB  $03,$B4,$63,$83,$49,$84,$63
                DEFB      $B8,$C6,$85,$86,$82,$31
                DEFB      $B4,$63,$84,$63,$81,$76
                DEFB      $B8,$C6,$83,$49,$81,$18
                DEFB      $B4,$63,$83,$49,$00
                DEFB      $B8,$C6,$85,$86,$01
                DEFB      $00    ,$84,$63,$B0,$8C
                DEFB      $01    ,$83,$49,$B0,$8C
                DEFB      $01    ,$00    ,$B0,$8C
                DEFB      $01    ,$01    ,$B0,$8C
                DEFB      $01    ,$01    ,$B0,$8C
                DEFB      $01    ,$01    ,$B0,$8C
                DEFB  $09,$A3,$49,$83,$49,$B0,$BB
                DEFB      $A6,$92,$85,$86,$B0,$BB
                DEFB      $A3,$49,$84,$63,$B0,$BB
                DEFB      $A6,$92,$83,$49,$B0,$BB
                DEFB      $A3,$49,$83,$49,$B0,$BB
                DEFB      $A6,$92,$85,$86,$B0,$BB
                DEFB  $09,$00    ,$84,$63,$B0,$C6
                DEFB      $01    ,$83,$49,$B0,$C6
                DEFB      $01    ,$00    ,$B0,$C6
                DEFB      $01    ,$01    ,$B0,$C6
                DEFB      $01    ,$01    ,$B0,$C6
                DEFB      $01    ,$01    ,$B0,$C6
                DEFB  $FF  ; End of Pattern

PAT36:
                DEFW  713     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$01    ,$90,$8C
                DEFB      $01    ,$01    ,$90,$8C
                DEFB  $09,$F3,$E8,$83,$49,$90,$BB
                DEFB      $F7,$D0,$84,$E7,$90,$BB
                DEFB      $F3,$E8,$83,$E8,$90,$BB
                DEFB      $F7,$D0,$83,$49,$90,$BB
                DEFB      $F3,$E8,$83,$49,$90,$BB
                DEFB      $00    ,$00    ,$90,$BB
                DEFB  $03,$E3,$E8,$83,$49,$84,$63
                DEFB      $E7,$D0,$84,$E7,$82,$31
                DEFB      $E3,$B0,$83,$E8,$81,$76
                DEFB      $E7,$60,$83,$49,$81,$18
                DEFB      $D3,$49,$83,$49,$00
                DEFB      $D6,$92,$84,$E7,$01
                DEFB      $D2,$ED,$83,$E8,$B0,$7D
                DEFB      $D5,$DB,$83,$49,$B0,$7D
                DEFB      $C2,$C3,$00    ,$B0,$7D
                DEFB      $C5,$86,$01    ,$B0,$7D
                DEFB      $C2,$76,$01    ,$B0,$7D
                DEFB      $C4,$E7,$01    ,$B0,$7D
                DEFB      $B2,$31,$83,$49,$B0,$8C
                DEFB      $B4,$63,$84,$E7,$B0,$8C
                DEFB      $B1,$F4,$83,$E8,$B0,$8C
                DEFB      $B3,$E8,$83,$49,$B0,$8C
                DEFB      $A1,$D8,$83,$49,$B0,$8C
                DEFB      $A3,$B0,$84,$E7,$B0,$8C
                DEFB      $A1,$A4,$83,$E8,$B0,$94
                DEFB      $A3,$49,$83,$49,$B0,$94
                DEFB      $91,$76,$00    ,$B0,$94
                DEFB      $92,$ED,$01    ,$B0,$94
                DEFB      $91,$61,$01    ,$B0,$94
                DEFB      $92,$C3,$01    ,$B0,$94
                DEFB  $FF  ; End of Pattern



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

; ************************************************************************
; * Song data...
; ************************************************************************
BORDER_CLR:          EQU $0

; *** DATA ***
MUSICDATA:

; *** Song layout ***
LOOPSTART:            DEFW      PAT0
                      DEFW      PAT1
                      DEFW      PAT2
                      DEFW      PAT3
                      DEFW      $0000
                      DEFW      LOOPSTART

; *** Patterns ***
PAT0:
                DEFW  2464     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $B7,$60,$B1,$3B,$A4,$E7
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$D0,$B1,$3B,$A5,$DB
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$60,$B1,$3B,$A5,$86
                DEFB      $01    ,$00    ,$01
                DEFB      $B4,$E7,$B1,$3B,$A4,$E7
                DEFB      $01    ,$00    ,$01
                DEFB      $B5,$DB,$B1,$3B,$A5,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$A7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $B6,$92,$B1,$3B,$A4,$E7
                DEFB      $01    ,$00    ,$01
                DEFB      $B5,$DB,$B1,$3B,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B6,$92,$B1,$3B,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B5,$DB,$B1,$3B,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$60,$B1,$3B,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $B7,$60,$B1,$76,$A5,$DB
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$D0,$B1,$76,$A5,$86
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$60,$B1,$76,$A5,$DB
                DEFB      $01    ,$00    ,$01
                DEFB      $B5,$DB,$B1,$76,$A6,$92
                DEFB      $01    ,$00    ,$01
                DEFB      $B6,$92,$B1,$76,$A7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$A8,$C6
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $B5,$DB,$B0,$EC,$A6,$92
                DEFB      $01    ,$00    ,$01
                DEFB      $B6,$92,$B0,$EC,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$60,$B0,$EC,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B8,$C6,$B0,$EC,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B6,$92,$B0,$EC,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT1:
                DEFW  2464     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $B6,$92,$B1,$3B,$A4,$E7
                DEFB      $01    ,$00    ,$01
                DEFB      $B5,$DB,$B1,$3B,$A5,$DB
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$60,$B1,$3B,$A6,$92
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$D0,$B1,$3B,$A5,$DB
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$60,$B1,$3B,$A7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$A7,$D0
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $B7,$D0,$B1,$3B,$A7,$60
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$60,$B1,$3B,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B6,$92,$B1,$3B,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B5,$DB,$B1,$3B,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$60,$B1,$3B,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $B5,$DB,$B1,$76,$A5,$DB
                DEFB      $01    ,$00    ,$01
                DEFB      $B6,$92,$B1,$76,$A5,$86
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$60,$B1,$76,$A7,$60
                DEFB      $01    ,$00    ,$01
                DEFB      $B6,$92,$B1,$76,$A6,$92
                DEFB      $01    ,$00    ,$01
                DEFB      $B5,$86,$B1,$76,$A5,$86
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$A5,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $B5,$DB,$B1,$A4,$A5,$86
                DEFB      $01    ,$00    ,$01
                DEFB      $B6,$92,$B1,$A4,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$D0,$B1,$A4,$A4,$63
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$60,$B1,$A4,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B4,$E7,$B1,$A4,$A3,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT2:
                DEFW  2464     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $B4,$E7,$B1,$D8,$A4,$E7
                DEFB      $01    ,$00    ,$01
                DEFB      $B5,$DB,$B1,$D8,$A5,$DB
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$60,$B1,$D8,$A6,$92
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$D0,$B1,$D8,$A4,$E7
                DEFB      $01    ,$00    ,$01
                DEFB      $B6,$92,$B1,$D8,$A7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$A7,$D0
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $B5,$DB,$B1,$76,$A7,$60
                DEFB      $01    ,$00    ,$01
                DEFB      $B8,$C6,$B1,$76,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$D0,$B1,$76,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B6,$92,$B1,$76,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$60,$B1,$76,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $B6,$92,$B2,$31,$A5,$DB
                DEFB      $01    ,$00    ,$01
                DEFB      $B5,$DB,$B2,$31,$A5,$86
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$D0,$B2,$31,$A5,$DB
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$60,$B2,$31,$A6,$92
                DEFB      $01    ,$00    ,$01
                DEFB      $B6,$92,$B2,$31,$A7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$A6,$92
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $B4,$E7,$B1,$A4,$A5,$86
                DEFB      $01    ,$00    ,$01
                DEFB      $B5,$86,$B1,$A4,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B5,$DB,$B1,$A4,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B6,$92,$B1,$A4,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B5,$86,$B1,$A4,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT3:
                DEFW  2464     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $B5,$DB,$B1,$D8,$A5,$DB
                DEFB      $01    ,$00    ,$01
                DEFB      $B6,$92,$B1,$D8,$A5,$86
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$60,$B1,$D8,$A5,$DB
                DEFB      $01    ,$00    ,$01
                DEFB      $B5,$86,$B1,$D8,$A6,$92
                DEFB      $01    ,$00    ,$01
                DEFB      $B5,$DB,$B1,$D8,$A5,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$A5,$86
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $B6,$92,$B1,$76,$A5,$DB
                DEFB      $01    ,$00    ,$01
                DEFB      $B5,$DB,$B1,$76,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B8,$C6,$B1,$76,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B6,$92,$B1,$76,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$60,$B1,$76,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $B8,$C6,$B2,$31,$A8,$C6
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$D0,$B2,$31,$A7,$60
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$60,$B2,$31,$A7,$D0
                DEFB      $01    ,$00    ,$01
                DEFB      $B5,$DB,$B2,$31,$A5,$DB
                DEFB      $01    ,$00    ,$01
                DEFB      $B6,$92,$B2,$31,$A7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$A6,$92
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $B5,$DB,$B1,$61,$A5,$86
                DEFB      $01    ,$00    ,$01
                DEFB      $B6,$92,$B1,$61,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$D0,$B1,$61,$A5,$DB
                DEFB      $01    ,$00    ,$01
                DEFB      $B7,$60,$B1,$61,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $B9,$D9,$B1,$61,$A4,$E7
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern


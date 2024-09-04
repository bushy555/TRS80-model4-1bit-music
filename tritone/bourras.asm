
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



BOURLOOPSTART:            DEFW      BOURPAT0
                      DEFW      BOURPAT0
                      DEFW      BOURPAT1
                      DEFW      BOURPAT0
                      DEFW      BOURPAT0
                      DEFW      BOURPAT1
                      DEFW      BOURPAT2
                      DEFW      BOURPAT2
                      DEFW      BOURPAT3
                      DEFW      BOURPAT2
                      DEFW      BOURPAT2
                      DEFW      BOURPAT3
                      DEFW      BOURPAT4
                      DEFW      BOURPAT5
                      DEFW      BOURPAT6
                      DEFW      BOURPAT7
                      DEFW      BOURPAT8
                      DEFW      BOURPAT9
                      DEFW      BOURPAT10
                      DEFW      BOURPAT10
                      DEFW      BOURPAT11
                      DEFW      BOURPAT10
                      DEFW      BOURPAT10
                      DEFW      BOURPAT11
                      DEFW      BOURPAT12
                      DEFW      BOURPAT13
                      DEFW      BOURPAT16
                      DEFW      BOURPAT17
                      DEFW      BOURPAT12
                      DEFW      BOURPAT13
                      DEFW      BOURPAT16
                      DEFW      BOURPAT17
                      DEFW      BOURPAT19
                      DEFW      BOURPAT20
                      DEFW      BOURPAT14
                      DEFW      BOURPAT18
                      DEFW      BOURPAT0
                      DEFW      BOURPAT0
                      DEFW      BOURPAT1
                      DEFW      BOURPAT0
                      DEFW      BOURPAT0
                      DEFW      BOURPAT1
                      DEFW      BOURPAT2
                      DEFW      BOURPAT2
                      DEFW      BOURPAT3
                      DEFW      BOURPAT2
                      DEFW      BOURPAT2
                      DEFW      BOURPAT3
                      DEFW      BOURPAT21
                      DEFW      BOURPAT22
                      DEFW      BOURPAT23
                      DEFW      BOURPAT24
                      DEFW      BOURPAT25
                      DEFW      BOURPAT26
                      DEFW      BOURPAT27
                      DEFW      BOURPAT27
                      DEFW      BOURPAT28
                      DEFW      BOURPAT27
                      DEFW      BOURPAT27
                      DEFW      BOURPAT28
                      DEFW      BOURPAT21
                      DEFW      BOURPAT22
                      DEFW      BOURPAT23
                      DEFW      BOURPAT24
                      DEFW      BOURPAT25
                      DEFW      BOURPAT26
                      DEFW      BOURPAT27
                      DEFW      BOURPAT27
                      DEFW      BOURPAT28
                      DEFW      BOURPAT27
                      DEFW      BOURPAT27
                      DEFW      BOURPAT28
                      DEFW      BOURPAT33
                      DEFW      BOURPAT34
                      DEFW      BOURPAT12
                      DEFW      BOURPAT13
                      DEFW      BOURPAT31
                      DEFW      BOURPAT32
                      DEFW      BOURPAT29
                      DEFW      BOURPAT30
                      DEFW      BOURPAT31
                      DEFW      BOURPAT32
                      DEFW      BOURPAT12
                      DEFW      BOURPAT13
                      DEFW      BOURPAT16
                      DEFW      BOURPAT17
                      DEFW      BOURPAT0
                      DEFW      BOURPAT0
                      DEFW      BOURPAT1
                      DEFW      BOURPAT0
                      DEFW      BOURPAT0
                      DEFW      BOURPAT1
                      DEFW      BOURPAT2
                      DEFW      BOURPAT2
                      DEFW      BOURPAT3
                      DEFW      BOURPAT2
                      DEFW      BOURPAT2
                      DEFW      BOURPAT3
                      DEFW      BOURPAT35
                      DEFW      BOURPAT36
                      DEFW      BOURPAT37
                      DEFW      BOURPAT38
                      DEFW      BOURPAT39
                      DEFW      BOURPAT40
                      DEFW      BOURPAT41
                      DEFW      BOURPAT42
                      DEFW      BOURPAT43
                      DEFW      BOURPAT43
                      DEFW      BOURPAT44
                      DEFW      BOURPAT44
                      DEFW      BOURPAT43
                      DEFW      BOURPAT43
                      DEFW      BOURPAT44
                      DEFW      BOURPAT44
                      DEFW      BOURPAT35
                      DEFW      BOURPAT36
                      DEFW      BOURPAT37
                      DEFW      BOURPAT38
                      DEFW      BOURPAT35
                      DEFW      BOURPAT36
                      DEFW      BOURPAT37
                      DEFW      BOURPAT38
                      DEFW      BOURPAT43
                      DEFW      BOURPAT43
                      DEFW      BOURPAT44
                      DEFW      BOURPAT46
                      DEFW      BOURPAT47
                      DEFW      BOURPAT47
                      DEFW      BOURPAT48
                      DEFW      BOURPAT48
                      DEFW      BOURPAT49
                      DEFW      BOURPAT51
                      DEFW      BOURPAT50
                      DEFW      BOURPAT50
                      DEFW      BOURPAT52
                      DEFW      $0000
                      DEFW      BOURLOOPSTART

; *** BOURPATterns ***
BOURPAT0:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$83,$49,$84,$E7,$80,$D2
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$85,$DB,$81,$A4
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$83,$E8,$86,$92,$80,$D2
                DEFB      $01    ,$87,$D0,$01
                DEFB      $01    ,$86,$92,$01
                DEFB      $01    ,$87,$D0,$01
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT1:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$83,$49,$84,$E7,$80,$D2
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$85,$DB,$81,$A4
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$81,$F4,$84,$E7,$81,$A4
                DEFB      $01    ,$83,$E8,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT2:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$82,$76,$83,$B0,$80,$9D
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$B0,$01
                DEFB      $01    ,$84,$63,$81,$3B
                DEFB      $01    ,$83,$B0,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$82,$ED,$84,$E7,$80,$9D
                DEFB      $01    ,$85,$DB,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$85,$DB,$01
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT3:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$82,$76,$83,$B0,$80,$9D
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$83,$B0,$01
                DEFB      $01    ,$84,$63,$81,$3B
                DEFB      $01    ,$83,$B0,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$81,$76,$83,$B0,$81,$3B
                DEFB      $01    ,$82,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT4:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$87,$D0,$84,$E7,$80,$D2
                DEFB      $87,$D0,$85,$DB,$01
                DEFB      $8B,$B6,$84,$E7,$01
                DEFB      $8B,$B6,$85,$DB,$81,$A4
                DEFB      $89,$D9,$84,$E7,$01
                DEFB      $89,$D9,$85,$DB,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB  $06,$89,$D9,$86,$92,$80,$D2
                DEFB      $84,$E7,$87,$D0,$01
                DEFB      $84,$E7,$86,$92,$01
                DEFB      $89,$D9,$87,$D0,$01
                DEFB  $06,$84,$E7,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT5:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$88,$C6,$84,$E7,$80,$D2
                DEFB      $88,$C6,$85,$DB,$01
                DEFB      $88,$C6,$84,$E7,$01
                DEFB      $88,$C6,$85,$DB,$81,$A4
                DEFB      $87,$D0,$84,$E7,$01
                DEFB      $87,$D0,$85,$DB,$01
                DEFB      $87,$D0,$01    ,$01
                DEFB      $83,$49,$01    ,$01
                DEFB  $06,$83,$49,$86,$92,$80,$D2
                DEFB      $83,$49,$87,$D0,$01
                DEFB      $82,$ED,$86,$92,$01
                DEFB      $83,$49,$87,$D0,$01
                DEFB  $06,$83,$49,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $83,$49,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT6:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$D3,$49,$B4,$E7,$A0,$69
                DEFB      $01    ,$C5,$DB,$01
                DEFB      $01    ,$D4,$E7,$01
                DEFB      $01    ,$E5,$DB,$B0,$D2
                DEFB      $01    ,$F4,$E7,$01
                DEFB      $01    ,$F5,$DB,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$D1,$F4,$F4,$E7,$C0,$D2
                DEFB      $01    ,$F3,$E8,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT7:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$87,$D0,$84,$E7,$A0,$69
                DEFB      $87,$D0,$85,$DB,$01
                DEFB      $8B,$B6,$84,$E7,$01
                DEFB      $8B,$B6,$85,$DB,$B0,$D2
                DEFB      $89,$D9,$84,$E7,$01
                DEFB      $89,$D9,$85,$DB,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB  $06,$89,$D9,$86,$92,$E0,$69
                DEFB      $84,$E7,$87,$D0,$01
                DEFB      $84,$E7,$86,$92,$01
                DEFB      $89,$D9,$87,$D0,$01
                DEFB  $06,$84,$E7,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT8:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$88,$C6,$84,$E7,$A0,$69
                DEFB      $88,$C6,$85,$DB,$01
                DEFB      $88,$C6,$84,$E7,$01
                DEFB      $88,$C6,$85,$DB,$B0,$D2
                DEFB      $87,$D0,$84,$E7,$01
                DEFB      $87,$D0,$85,$DB,$01
                DEFB      $87,$D0,$01    ,$01
                DEFB      $83,$49,$01    ,$01
                DEFB  $06,$83,$49,$86,$92,$F0,$69
                DEFB      $83,$49,$87,$D0,$01
                DEFB      $82,$ED,$86,$92,$01
                DEFB      $83,$49,$87,$D0,$01
                DEFB  $06,$83,$49,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $83,$49,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT9:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$D3,$49,$B4,$E7,$A0,$69
                DEFB      $D3,$49,$C5,$DB,$01
                DEFB      $D6,$92,$D4,$E7,$01
                DEFB      $D6,$92,$E5,$DB,$B0,$D2
                DEFB      $D3,$49,$F4,$E7,$01
                DEFB      $D3,$49,$F5,$DB,$01
                DEFB      $D6,$92,$01    ,$01
                DEFB      $D6,$92,$01    ,$01
                DEFB  $06,$D1,$F4,$F4,$E7,$C0,$D2
                DEFB      $D1,$F4,$F3,$E8,$01
                DEFB      $D3,$E8,$01    ,$01
                DEFB      $D3,$E8,$01    ,$01
                DEFB  $06,$D3,$E8,$01    ,$01
                DEFB      $D7,$D0,$01    ,$01
                DEFB      $D7,$D0,$01    ,$01
                DEFB      $D7,$D0,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT10:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$82,$76,$93,$B0,$80,$9D
                DEFB      $82,$76,$94,$63,$01
                DEFB      $82,$76,$93,$B0,$01
                DEFB      $82,$76,$94,$63,$81,$3B
                DEFB      $84,$E7,$93,$B0,$01
                DEFB      $84,$E7,$94,$63,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB      $82,$ED,$01    ,$01
                DEFB  $06,$82,$ED,$A4,$E7,$80,$9D
                DEFB      $82,$ED,$A5,$DB,$01
                DEFB      $82,$ED,$A4,$E7,$01
                DEFB      $82,$ED,$A5,$DB,$01
                DEFB  $06,$85,$DB,$01    ,$81,$3B
                DEFB      $85,$DB,$01    ,$01
                DEFB      $82,$ED,$01    ,$80,$BB
                DEFB      $85,$DB,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT11:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$82,$76,$93,$B0,$80,$9D
                DEFB      $01    ,$94,$63,$01
                DEFB      $01    ,$93,$B0,$01
                DEFB      $01    ,$94,$63,$81,$3B
                DEFB      $01    ,$93,$B0,$01
                DEFB      $01    ,$94,$63,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$81,$76,$A3,$B0,$81,$3B
                DEFB      $01    ,$A2,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$01    ,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$80,$BB
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT12:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$89,$D9,$84,$E7,$80,$69
                DEFB      $89,$D9,$85,$DB,$01
                DEFB      $89,$D9,$84,$E7,$01
                DEFB      $89,$D9,$85,$DB,$80,$D2
                DEFB      $89,$D9,$84,$E7,$01
                DEFB      $89,$D9,$85,$DB,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB  $06,$89,$D9,$86,$92,$80,$69
                DEFB      $89,$D9,$87,$D0,$01
                DEFB      $89,$D9,$86,$92,$01
                DEFB      $89,$D9,$87,$D0,$81,$3B
                DEFB  $06,$84,$E7,$01    ,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB  $02,$88,$C6,$84,$E7,$80,$69
                DEFB      $88,$C6,$85,$DB,$01
                DEFB      $88,$C6,$84,$E7,$01
                DEFB      $88,$C6,$85,$DB,$80,$D2
                DEFB      $88,$C6,$84,$E7,$01
                DEFB      $87,$D0,$85,$DB,$01
                DEFB      $87,$D0,$01    ,$01
                DEFB      $87,$D0,$01    ,$00
                DEFB  $06,$87,$D0,$86,$92,$80,$D2
                DEFB      $87,$D0,$87,$D0,$01
                DEFB      $88,$C6,$86,$92,$01
                DEFB      $88,$C6,$87,$D0,$80,$FA
                DEFB  $06,$86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB  $02,$86,$92,$84,$E7,$80,$69
                DEFB      $86,$92,$85,$DB,$01
                DEFB      $86,$92,$84,$E7,$01
                DEFB      $86,$92,$85,$DB,$80,$D2
                DEFB      $86,$92,$84,$E7,$01
                DEFB      $89,$D9,$85,$DB,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB  $06,$89,$D9,$84,$E7,$80,$FA
                DEFB      $89,$D9,$83,$E8,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$80,$9D
                DEFB  $06,$86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT13:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$89,$D9,$84,$E7,$80,$69
                DEFB      $89,$D9,$85,$DB,$01
                DEFB      $89,$D9,$84,$E7,$01
                DEFB      $89,$D9,$85,$DB,$80,$D2
                DEFB      $89,$D9,$84,$E7,$01
                DEFB      $89,$D9,$85,$DB,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB  $06,$89,$D9,$86,$92,$80,$69
                DEFB      $89,$D9,$87,$D0,$01
                DEFB      $89,$D9,$86,$92,$01
                DEFB      $89,$D9,$87,$D0,$81,$3B
                DEFB  $06,$89,$D9,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB  $02,$88,$C6,$84,$E7,$80,$69
                DEFB      $88,$C6,$85,$DB,$01
                DEFB      $88,$C6,$84,$E7,$01
                DEFB      $88,$C6,$85,$DB,$80,$D2
                DEFB      $88,$C6,$84,$E7,$01
                DEFB      $87,$D0,$85,$DB,$01
                DEFB      $87,$D0,$01    ,$01
                DEFB      $87,$D0,$01    ,$00
                DEFB  $06,$87,$D0,$86,$92,$80,$D2
                DEFB      $87,$D0,$87,$D0,$01
                DEFB      $86,$92,$86,$92,$01
                DEFB      $86,$92,$87,$D0,$80,$FA
                DEFB  $06,$86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB  $02,$86,$92,$84,$E7,$80,$69
                DEFB      $86,$92,$85,$DB,$01
                DEFB      $86,$92,$84,$E7,$01
                DEFB      $86,$92,$85,$DB,$80,$D2
                DEFB      $86,$92,$84,$E7,$01
                DEFB      $86,$92,$85,$DB,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB  $06,$86,$92,$84,$E7,$80,$FA
                DEFB      $86,$92,$83,$E8,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$80,$9D
                DEFB  $06,$86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT14:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$89,$D9,$84,$E7,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB  $07,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$8A,$6E,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $07,$89,$D9,$83,$49,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$88,$C6,$84,$63,$80,$5D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$88,$C6,$83,$49,$80,$8C
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$84,$63,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT16:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$87,$60,$83,$B0,$80,$9D
                DEFB      $87,$60,$84,$63,$01
                DEFB      $87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$81,$3B
                DEFB      $87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB  $06,$87,$60,$84,$E7,$01
                DEFB      $87,$60,$85,$DB,$01
                DEFB      $87,$60,$84,$E7,$01
                DEFB      $87,$60,$85,$DB,$80,$EC
                DEFB  $06,$87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB  $02,$86,$92,$83,$B0,$01
                DEFB      $86,$92,$84,$63,$01
                DEFB      $86,$92,$83,$B0,$01
                DEFB      $86,$92,$84,$63,$80,$9D
                DEFB      $86,$92,$83,$B0,$01
                DEFB      $85,$DB,$84,$63,$01
                DEFB      $85,$DB,$01    ,$01
                DEFB      $85,$DB,$01    ,$00
                DEFB  $06,$85,$DB,$84,$E7,$80,$9D
                DEFB      $85,$DB,$85,$DB,$01
                DEFB      $84,$E7,$84,$E7,$01
                DEFB      $84,$E7,$85,$DB,$80,$BB
                DEFB  $06,$84,$E7,$01    ,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB  $02,$84,$E7,$83,$B0,$01
                DEFB      $84,$E7,$84,$63,$01
                DEFB      $84,$E7,$83,$B0,$01
                DEFB      $84,$E7,$84,$63,$80,$9D
                DEFB      $84,$E7,$83,$B0,$01
                DEFB      $84,$E7,$84,$63,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB  $06,$84,$E7,$83,$B0,$80,$BB
                DEFB      $84,$E7,$82,$ED,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB      $84,$E7,$01    ,$80,$76
                DEFB  $06,$84,$E7,$01    ,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT17:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$87,$60,$83,$B0,$80,$9D
                DEFB      $87,$60,$84,$63,$01
                DEFB      $87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$81,$3B
                DEFB      $87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB  $06,$87,$60,$84,$E7,$01
                DEFB      $87,$60,$85,$DB,$01
                DEFB      $87,$60,$84,$E7,$01
                DEFB      $87,$60,$85,$DB,$80,$EC
                DEFB  $06,$83,$B0,$01    ,$01
                DEFB      $83,$B0,$01    ,$01
                DEFB      $83,$B0,$01    ,$01
                DEFB      $83,$B0,$01    ,$01
                DEFB  $02,$86,$92,$83,$B0,$01
                DEFB      $86,$92,$84,$63,$01
                DEFB      $86,$92,$83,$B0,$01
                DEFB      $86,$92,$84,$63,$80,$9D
                DEFB      $86,$92,$83,$B0,$01
                DEFB      $85,$DB,$84,$63,$01
                DEFB      $85,$DB,$01    ,$01
                DEFB      $85,$DB,$01    ,$00
                DEFB  $06,$85,$DB,$84,$E7,$80,$9D
                DEFB      $85,$DB,$85,$DB,$01
                DEFB      $87,$60,$84,$E7,$01
                DEFB      $87,$60,$85,$DB,$80,$BB
                DEFB  $06,$87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB  $02,$87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$01
                DEFB      $87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$80,$9D
                DEFB      $87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB  $06,$87,$60,$83,$B0,$80,$BB
                DEFB      $87,$60,$82,$ED,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$80,$76
                DEFB  $06,$87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT18:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$87,$D0,$84,$E7,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB  $07,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$88,$C6,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $07,$89,$D9,$83,$49,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$01    ,$84,$63,$80,$5D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$87,$D0,$83,$49,$80,$8C
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $87,$60,$84,$63,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT19:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$83,$E8,$84,$E7,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$01    ,$01    ,$80,$BB
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$84,$63,$81,$18
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT20:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$83,$B0,$84,$E7,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$01    ,$01    ,$80,$5D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$83,$49,$80,$8C
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT21:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$80,$EC,$84,$E7,$80,$D2
                DEFB      $80,$EC,$85,$DB,$01
                DEFB      $81,$D8,$84,$E7,$01
                DEFB      $81,$D8,$85,$DB,$81,$A4
                DEFB      $80,$EC,$84,$E7,$01
                DEFB      $80,$EC,$85,$DB,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$80,$D2,$86,$92,$80,$69
                DEFB      $80,$D2,$87,$D0,$01
                DEFB      $81,$A4,$86,$92,$01
                DEFB      $81,$A4,$87,$D0,$01
                DEFB  $06,$80,$D2,$01    ,$01
                DEFB      $80,$D2,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT22:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$88,$C6,$84,$E7,$80,$D2
                DEFB      $88,$C6,$85,$DB,$01
                DEFB      $88,$C6,$84,$E7,$01
                DEFB      $88,$C6,$85,$DB,$81,$A4
                DEFB      $87,$D0,$84,$E7,$01
                DEFB      $87,$D0,$85,$DB,$01
                DEFB      $87,$D0,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB  $06,$89,$D9,$89,$D9,$80,$69
                DEFB      $89,$D9,$87,$D0,$01
                DEFB      $89,$D9,$89,$D9,$01
                DEFB      $89,$D9,$87,$D0,$01
                DEFB  $06,$84,$E7,$89,$D9,$01
                DEFB      $84,$E7,$89,$D9,$01
                DEFB      $84,$E7,$89,$D9,$01
                DEFB      $84,$E7,$89,$D9,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT23:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$D7,$D0,$B3,$E8,$C0,$69
                DEFB      $01    ,$C3,$E8,$01
                DEFB      $01    ,$D3,$E8,$01
                DEFB      $01    ,$E3,$B0,$C0,$D2
                DEFB      $01    ,$F3,$B0,$01
                DEFB      $01    ,$F3,$B0,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$D1,$F4,$F4,$63,$D0,$D2
                DEFB      $01    ,$F4,$63,$01
                DEFB      $01    ,$F3,$E8,$01
                DEFB      $01    ,$F3,$E8,$01
                DEFB  $06,$01    ,$F3,$E8,$01
                DEFB      $01    ,$F3,$E8,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT24:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$B0,$EC,$B4,$E7,$80,$D2
                DEFB      $B0,$EC,$C5,$DB,$01
                DEFB      $B1,$D8,$D4,$E7,$01
                DEFB      $B1,$D8,$E5,$DB,$81,$A4
                DEFB      $B0,$EC,$F4,$E7,$01
                DEFB      $B0,$EC,$F5,$DB,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$B0,$D2,$F6,$92,$80,$69
                DEFB      $B0,$D2,$F7,$D0,$01
                DEFB      $B1,$A4,$F6,$92,$01
                DEFB      $B1,$A4,$F7,$D0,$01
                DEFB  $06,$B0,$D2,$01    ,$01
                DEFB      $B0,$D2,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT25:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$C8,$C6,$B4,$E7,$80,$D2
                DEFB      $C8,$C6,$C5,$DB,$01
                DEFB      $C8,$C6,$D4,$E7,$01
                DEFB      $C8,$C6,$E5,$DB,$81,$A4
                DEFB      $C7,$D0,$F4,$E7,$01
                DEFB      $C7,$D0,$F5,$DB,$01
                DEFB      $C7,$D0,$01    ,$01
                DEFB      $C9,$D9,$01    ,$01
                DEFB  $06,$C9,$D9,$F9,$D9,$80,$69
                DEFB      $C9,$D9,$F7,$D0,$01
                DEFB      $C9,$D9,$F9,$D9,$01
                DEFB      $C9,$D9,$F7,$D0,$01
                DEFB  $06,$C4,$E7,$F9,$D9,$01
                DEFB      $C4,$E7,$F9,$D9,$01
                DEFB      $C4,$E7,$F9,$D9,$01
                DEFB      $C4,$E7,$F9,$D9,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT26:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$D7,$D0,$83,$E8,$C0,$69
                DEFB      $01    ,$93,$E8,$01
                DEFB      $01    ,$A3,$E8,$01
                DEFB      $01    ,$B3,$B0,$C0,$D2
                DEFB      $01    ,$C3,$B0,$01
                DEFB      $01    ,$D3,$B0,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$D1,$F4,$D4,$63,$D0,$D2
                DEFB      $01    ,$D4,$63,$01
                DEFB      $01    ,$D3,$E8,$01
                DEFB      $01    ,$D3,$E8,$01
                DEFB  $06,$01    ,$D3,$E8,$01
                DEFB      $01    ,$D3,$E8,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT27:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$C2,$76,$C3,$B0,$B0,$9D
                DEFB      $B2,$76,$B4,$63,$01
                DEFB      $A2,$76,$C3,$B0,$01
                DEFB      $92,$76,$D4,$63,$A1,$3B
                DEFB      $84,$E7,$E3,$B0,$01
                DEFB      $84,$E7,$F4,$63,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB      $82,$ED,$01    ,$01
                DEFB  $06,$82,$ED,$A4,$E7,$90,$9D
                DEFB      $82,$ED,$A5,$DB,$01
                DEFB      $82,$ED,$A4,$E7,$01
                DEFB      $82,$ED,$A5,$DB,$01
                DEFB  $06,$85,$DB,$01    ,$C1,$3B
                DEFB      $85,$DB,$01    ,$01
                DEFB      $82,$ED,$01    ,$D0,$BB
                DEFB      $85,$DB,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT28:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$B2,$76,$D3,$B0,$B0,$9D
                DEFB      $01    ,$B4,$63,$01
                DEFB      $01    ,$C3,$B0,$01
                DEFB      $01    ,$D4,$63,$A1,$3B
                DEFB      $01    ,$E3,$B0,$01
                DEFB      $01    ,$F4,$63,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$D1,$76,$C3,$B0,$91,$3B
                DEFB      $01    ,$D2,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$01    ,$01    ,$C0,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$D0,$BB
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT29:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$89,$D9,$84,$E7,$80,$69
                DEFB      $89,$D9,$85,$DB,$01
                DEFB      $89,$D9,$84,$E7,$01
                DEFB      $89,$D9,$85,$DB,$80,$D2
                DEFB      $89,$D9,$84,$E7,$01
                DEFB      $88,$C6,$85,$DB,$01
                DEFB      $88,$C6,$01    ,$01
                DEFB      $88,$C6,$01    ,$01
                DEFB  $06,$87,$D0,$86,$92,$80,$69
                DEFB      $87,$D0,$87,$D0,$01
                DEFB      $87,$D0,$86,$92,$01
                DEFB      $87,$D0,$87,$D0,$81,$3B
                DEFB  $06,$88,$C6,$01    ,$01
                DEFB      $88,$C6,$01    ,$01
                DEFB      $88,$C6,$01    ,$01
                DEFB      $88,$C6,$01    ,$01
                DEFB  $02,$89,$D9,$84,$E7,$80,$69
                DEFB      $89,$D9,$85,$DB,$01
                DEFB      $88,$C6,$84,$E7,$01
                DEFB      $88,$C6,$85,$DB,$80,$D2
                DEFB      $87,$D0,$84,$E7,$01
                DEFB      $87,$D0,$85,$DB,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$00
                DEFB  $06,$86,$92,$86,$92,$80,$D2
                DEFB      $86,$92,$87,$D0,$01
                DEFB      $86,$92,$86,$92,$01
                DEFB      $86,$92,$87,$D0,$80,$FA
                DEFB  $06,$86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB  $02,$86,$92,$84,$E7,$80,$69
                DEFB      $86,$92,$85,$DB,$01
                DEFB      $86,$92,$84,$E7,$01
                DEFB      $86,$92,$85,$DB,$80,$D2
                DEFB      $86,$92,$84,$E7,$01
                DEFB      $89,$D9,$85,$DB,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB  $06,$89,$D9,$84,$E7,$80,$FA
                DEFB      $89,$D9,$83,$E8,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$80,$9D
                DEFB  $06,$86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT30:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$89,$D9,$84,$E7,$80,$69
                DEFB      $89,$D9,$85,$DB,$01
                DEFB      $89,$D9,$84,$E7,$01
                DEFB      $89,$D9,$85,$DB,$80,$D2
                DEFB      $89,$D9,$84,$E7,$01
                DEFB      $89,$D9,$85,$DB,$01
                DEFB      $88,$C6,$01    ,$01
                DEFB      $88,$C6,$01    ,$01
                DEFB  $06,$88,$C6,$86,$92,$80,$69
                DEFB      $88,$C6,$87,$D0,$01
                DEFB      $88,$C6,$86,$92,$01
                DEFB      $88,$C6,$87,$D0,$81,$3B
                DEFB  $06,$88,$C6,$01    ,$01
                DEFB      $88,$C6,$01    ,$01
                DEFB      $88,$C6,$01    ,$01
                DEFB      $88,$C6,$01    ,$01
                DEFB  $02,$89,$D9,$84,$E7,$80,$69
                DEFB      $89,$D9,$85,$DB,$01
                DEFB      $89,$D9,$84,$E7,$01
                DEFB      $89,$D9,$85,$DB,$80,$D2
                DEFB      $89,$D9,$84,$E7,$01
                DEFB      $89,$D9,$85,$DB,$01
                DEFB      $8B,$B6,$01    ,$01
                DEFB      $8B,$B6,$01    ,$00
                DEFB  $06,$8B,$B6,$86,$92,$80,$D2
                DEFB      $8B,$B6,$87,$D0,$01
                DEFB      $8B,$B6,$86,$92,$01
                DEFB      $8B,$B6,$87,$D0,$80,$FA
                DEFB  $06,$89,$D9,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB      $89,$D9,$01    ,$01
                DEFB  $02,$89,$D9,$84,$E7,$80,$69
                DEFB      $86,$92,$85,$DB,$01
                DEFB      $86,$92,$84,$E7,$01
                DEFB      $86,$92,$85,$DB,$80,$D2
                DEFB      $86,$92,$84,$E7,$01
                DEFB      $86,$92,$85,$DB,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB  $06,$86,$92,$84,$E7,$80,$FA
                DEFB      $86,$92,$83,$E8,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$80,$9D
                DEFB  $06,$86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB      $86,$92,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT31:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$87,$60,$83,$B0,$80,$9D
                DEFB      $87,$60,$84,$63,$01
                DEFB      $87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$81,$3B
                DEFB      $87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB  $06,$87,$D0,$84,$E7,$01
                DEFB      $87,$D0,$82,$ED,$01
                DEFB      $87,$D0,$84,$E7,$01
                DEFB      $87,$D0,$82,$ED,$80,$EC
                DEFB  $06,$87,$D0,$01    ,$01
                DEFB      $87,$D0,$01    ,$01
                DEFB      $87,$D0,$01    ,$01
                DEFB      $87,$D0,$01    ,$01
                DEFB  $02,$87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$01
                DEFB      $87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$80,$9D
                DEFB      $87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $85,$DB,$01    ,$00
                DEFB  $06,$85,$DB,$84,$E7,$80,$9D
                DEFB      $85,$DB,$82,$ED,$01
                DEFB      $84,$E7,$84,$E7,$01
                DEFB      $84,$E7,$82,$ED,$80,$BB
                DEFB  $06,$84,$E7,$01    ,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB  $02,$84,$E7,$83,$B0,$01
                DEFB      $84,$E7,$84,$63,$01
                DEFB      $84,$E7,$83,$B0,$01
                DEFB      $84,$E7,$84,$63,$80,$9D
                DEFB      $84,$E7,$83,$B0,$01
                DEFB      $84,$E7,$84,$63,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB  $06,$84,$E7,$83,$B0,$80,$BB
                DEFB      $84,$E7,$82,$ED,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB      $84,$E7,$01    ,$80,$76
                DEFB  $06,$84,$E7,$01    ,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB      $84,$E7,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT32:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$87,$60,$83,$B0,$80,$9D
                DEFB      $87,$60,$84,$63,$01
                DEFB      $87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$81,$3B
                DEFB      $87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB  $06,$87,$D0,$84,$E7,$01
                DEFB      $87,$D0,$85,$DB,$01
                DEFB      $87,$D0,$84,$E7,$01
                DEFB      $87,$D0,$85,$DB,$80,$EC
                DEFB  $06,$87,$D0,$01    ,$01
                DEFB      $87,$D0,$01    ,$01
                DEFB      $87,$D0,$01    ,$01
                DEFB      $87,$D0,$01    ,$01
                DEFB  $02,$87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$01
                DEFB      $87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$80,$9D
                DEFB      $87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$00
                DEFB  $06,$85,$DB,$84,$E7,$80,$9D
                DEFB      $85,$DB,$85,$DB,$01
                DEFB      $87,$60,$84,$E7,$01
                DEFB      $87,$60,$85,$DB,$80,$BB
                DEFB  $06,$87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB  $02,$87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$01
                DEFB      $87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$80,$9D
                DEFB      $87,$60,$83,$B0,$01
                DEFB      $87,$60,$84,$63,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB  $06,$87,$60,$83,$B0,$80,$BB
                DEFB      $87,$60,$82,$ED,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$80,$76
                DEFB  $06,$87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB      $87,$60,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT33:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$A3,$E8,$B4,$E7,$90,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$01    ,$01    ,$90,$BB
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$B4,$63,$91,$18
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT34:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$A3,$B0,$B4,$E7,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$01    ,$01    ,$80,$5D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$B3,$49,$80,$8C
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT35:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$90,$D2,$81,$F4,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $91,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$90,$D2,$82,$31,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$81,$3B
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$90,$D2,$82,$76,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $91,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$00
                DEFB  $06,$91,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $91,$F4,$01    ,$80,$FA
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$90,$D2,$01    ,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $91,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$91,$F4,$01    ,$80,$FA
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $91,$3B,$01    ,$80,$9D
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT36:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$80,$D2,$82,$31,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$80,$D2,$81,$F4,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$76,$01    ,$81,$3B
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$80,$D2,$01    ,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$00
                DEFB  $06,$81,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$F4,$01    ,$80,$FA
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$81,$A4,$01    ,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$81,$F4,$01    ,$80,$FA
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$3B,$01    ,$80,$9D
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT37:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$3B,$81,$D8,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$76,$01    ,$81,$3B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$D8,$01    ,$80,$EC
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$3B,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$00
                DEFB  $06,$82,$76,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$76,$01    ,$80,$BB
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$76,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$81,$76,$01    ,$80,$BB
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$D8,$01    ,$80,$76
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT38:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$3B,$82,$76,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$76,$01    ,$81,$3B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$D8,$01    ,$80,$EC
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$3B,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$81,$3B
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$82,$76,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$76,$01    ,$80,$BB
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$3B,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$81,$76,$01    ,$80,$BB
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $80,$EC,$01    ,$80,$76
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT39:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$90,$D2,$84,$E7,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $91,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$90,$D2,$83,$49,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$81,$3B
                DEFB  $06,$01    ,$81,$A4,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$90,$D2,$84,$E7,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $91,$A4,$01    ,$80,$D2
                DEFB      $01    ,$82,$76,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$00
                DEFB  $06,$91,$A4,$86,$92,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $91,$F4,$83,$49,$80,$FA
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$84,$E7,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$90,$D2,$89,$D9,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $91,$A4,$84,$E7,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$91,$F4,$83,$E8,$80,$FA
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $91,$3B,$88,$C6,$80,$9D
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT40:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$80,$D2,$88,$C6,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$80,$D2,$01    ,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$76,$01    ,$81,$3B
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$80,$D2,$94,$63,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$00
                DEFB  $06,$81,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$F4,$01    ,$80,$FA
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$81,$A4,$01    ,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $81,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$81,$F4,$01    ,$80,$FA
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$3B,$01    ,$80,$9D
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT41:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$3B,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$76,$01    ,$81,$3B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$D8,$84,$63,$80,$EC
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$3B,$84,$E7,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$00
                DEFB  $06,$82,$76,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$76,$01    ,$80,$BB
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$76,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$81,$76,$01    ,$80,$BB
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$D8,$01    ,$80,$76
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT42:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$3B,$82,$ED,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$76,$01    ,$81,$3B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$D8,$01    ,$80,$EC
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$3B,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$00
                DEFB  $06,$82,$76,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$76,$01    ,$80,$BB
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$3B,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$81,$76,$01    ,$80,$BB
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $80,$EC,$01    ,$80,$76
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT43:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$90,$D2,$00    ,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $0A,$91,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $90,$D2,$01    ,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$81,$3B
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$90,$D2,$01    ,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $91,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$00
                DEFB  $06,$91,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $91,$F4,$01    ,$80,$FA
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$90,$D2,$01    ,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $91,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$91,$F4,$01    ,$80,$FA
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $91,$3B,$01    ,$80,$9D
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT44:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$3B,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$76,$01    ,$81,$3B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$D8,$01    ,$80,$EC
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$3B,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$82,$76,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$76,$01    ,$80,$BB
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$76,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $06,$81,$76,$01    ,$80,$BB
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$D8,$01    ,$80,$76
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT46:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $81,$3B,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$76,$01    ,$81,$3B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$D8,$01    ,$80,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$3B,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$76,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$76,$01    ,$80,$BB
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$76,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$76,$01    ,$80,$BB
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$D8,$01    ,$80,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT47:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $D1,$A4,$00    ,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $E1,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F0,$D2,$01    ,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$81,$3B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F0,$D2,$01    ,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$F4,$01    ,$80,$FA
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F0,$D2,$01    ,$80,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$A4,$01    ,$80,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$F4,$01    ,$80,$FA
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$3B,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT48:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $D1,$3B,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $E2,$76,$01    ,$81,$3B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$D8,$01    ,$80,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$3B,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$81,$3B
                DEFB      $01    ,$01    ,$01
                DEFB      $F2,$76,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$76,$01    ,$80,$BB
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F2,$76,$01    ,$80,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$76,$01    ,$80,$BB
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$D8,$01    ,$80,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT49:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $D1,$A4,$00    ,$F0,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $E1,$A4,$01    ,$F0,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F0,$D2,$01    ,$F0,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$F1,$3B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$A4,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F0,$D2,$01    ,$F0,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$A4,$01    ,$F0,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$A4,$01    ,$F0,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$F4,$01    ,$F0,$7D
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F0,$D2,$01    ,$F0,$69
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$A4,$01    ,$F0,$D2
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$F4,$01    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$3B,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT50:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $D1,$3B,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $E2,$76,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$D8,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$3B,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F2,$76,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$76,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F2,$76,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$76,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$D8,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT51:
                DEFW  900     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $D1,$A4,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $E1,$A4,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F0,$D2,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F0,$D2,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$A4,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$00
                DEFB      $F1,$A4,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$F4,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F0,$D2,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$A4,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$F4,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$3B,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern

BOURPAT52:
                DEFW  3692     ; BOURPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $F1,$A4,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of BOURPATtern


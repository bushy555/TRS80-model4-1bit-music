
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

; *** Song layout ***
LOOPSTART:            DEFW      PAT0
                      DEFW      PAT0
                      DEFW      PAT1
                      DEFW      PAT2
                      DEFW      PAT3
                      DEFW      PAT4
                      DEFW      PAT3
                      DEFW      PAT4
                      DEFW      PAT5
                      DEFW      PAT6
                      DEFW      PAT5
                      DEFW      PAT6
                      DEFW      PAT7
                      DEFW      PAT8
                      DEFW      PAT7
                      DEFW      PAT9
                      DEFW      PAT10
                      DEFW      PAT11
                      DEFW      PAT12
                      DEFW      PAT13
                      DEFW      PAT10
                      DEFW      PAT11
                      DEFW      PAT12
                      DEFW      PAT13
                      DEFW      PAT1
                      DEFW      PAT2
                      DEFW      PAT14
                      DEFW      PAT14
                      DEFW      PAT14
                      DEFW      PAT14
                      DEFW      PAT15
                      DEFW      PAT16
                      DEFW      PAT17
                      DEFW      PAT18
                      DEFW      PAT17
                      DEFW      PAT18
                      DEFW      PAT5
                      DEFW      PAT6
                      DEFW      PAT5
                      DEFW      PAT19
                      DEFW      PAT0
                      DEFW      PAT0
                      DEFW      PAT1
                      DEFW      PAT2
                      DEFW      PAT20
                      DEFW      $0000
                      DEFW      LOOPSTART

; *** Patterns ***
PAT0:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $E0,$EC,$01    ,$01
                DEFB      $E0,$EC,$01    ,$01
                DEFB      $E1,$D8,$01    ,$01
                DEFB      $E0,$EC,$01    ,$01
                DEFB      $E0,$EC,$01    ,$01
                DEFB      $E0,$EC,$01    ,$01
                DEFB      $E1,$D8,$01    ,$01
                DEFB      $E0,$EC,$01    ,$01
                DEFB      $E0,$EC,$01    ,$01
                DEFB      $E0,$EC,$01    ,$01
                DEFB      $E1,$D8,$01    ,$01
                DEFB      $E0,$EC,$01    ,$01
                DEFB      $E0,$D2,$01    ,$01
                DEFB      $E0,$D2,$01    ,$01
                DEFB      $E1,$A4,$01    ,$01
                DEFB      $E1,$A4,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT1:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$E0,$EC,$01    ,$F1,$D8
                DEFB      $E0,$EC,$01    ,$00
                DEFB      $E1,$D8,$01    ,$F3,$B0
                DEFB      $E0,$EC,$01    ,$00
                DEFB  $04,$E0,$EC,$01    ,$F3,$B0
                DEFB      $E0,$EC,$01    ,$00
                DEFB      $E1,$D8,$01    ,$F2,$31
                DEFB      $E0,$EC,$01    ,$00
                DEFB      $E0,$EC,$01    ,$F1,$D8
                DEFB      $E0,$EC,$01    ,$00
                DEFB  $06,$E1,$D8,$01    ,$F3,$B0
                DEFB      $E0,$EC,$01    ,$00
                DEFB  $04,$E0,$D2,$01    ,$F3,$B0
                DEFB      $E0,$D2,$01    ,$00
                DEFB      $E1,$A4,$01    ,$F3,$B0
                DEFB      $E1,$A4,$01    ,$00
                DEFB  $FF  ; End of Pattern

PAT2:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$E0,$EC,$00    ,$F1,$D8
                DEFB      $E0,$EC,$01    ,$00
                DEFB      $E1,$D8,$01    ,$F3,$B0
                DEFB  $09,$E0,$EC,$01    ,$00
                DEFB  $04,$E0,$EC,$01    ,$F1,$D8
                DEFB      $E0,$EC,$01    ,$F0,$EC
                DEFB      $E1,$D8,$01    ,$F0,$76
                DEFB  $02,$E0,$76,$01    ,$01
                DEFB  $02,$E0,$76,$01    ,$01
                DEFB  $02,$01    ,$01    ,$00
                DEFB  $06,$01    ,$01    ,$01
                DEFB  $06,$01    ,$01    ,$01
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $00    ,$01    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT3:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$90,$76,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB      $90,$8C,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$F4
                DEFB  $04,$90,$9D,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB      $90,$8C,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$A4
                DEFB  $06,$90,$8C,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB  $04,$90,$9D,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$F4
                DEFB      $90,$8C,$01    ,$F1,$D8
                DEFB      $90,$8C,$01    ,$F1,$D8
                DEFB  $FF  ; End of Pattern

PAT4:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$90,$76,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB      $90,$8C,$01    ,$F1,$D8
                DEFB  $09,$90,$76,$01    ,$F1,$F4
                DEFB  $04,$90,$9D,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB      $90,$8C,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$A4
                DEFB  $06,$90,$8C,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB  $04,$90,$9D,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$F4
                DEFB  $04,$90,$8C,$01    ,$F1,$D8
                DEFB  $04,$90,$8C,$01    ,$F1,$D8
                DEFB  $FF  ; End of Pattern

PAT5:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$90,$76,$F2,$76,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB      $90,$8C,$01    ,$F1,$D8
                DEFB      $90,$76,$F2,$36,$F1,$F4
                DEFB  $04,$90,$9D,$01    ,$F1,$D8
                DEFB      $90,$76,$F3,$B4,$F1,$D8
                DEFB      $90,$8C,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$A4
                DEFB  $06,$90,$8C,$F5,$86,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB  $04,$90,$9D,$F5,$86,$F1,$D8
                DEFB      $90,$76,$F4,$E7,$F1,$F4
                DEFB      $90,$8C,$F5,$86,$F1,$D8
                DEFB      $90,$8C,$F4,$E7,$F1,$D8
                DEFB  $FF  ; End of Pattern

PAT6:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$90,$76,$F5,$DB,$F1,$D8
                DEFB      $90,$76,$F5,$DB,$F1,$D8
                DEFB      $90,$8C,$F5,$86,$F1,$D8
                DEFB  $09,$90,$76,$F4,$E7,$F1,$F4
                DEFB  $04,$90,$9D,$F5,$86,$F1,$D8
                DEFB      $90,$76,$F5,$DB,$F1,$D8
                DEFB      $90,$8C,$F5,$96,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$A4
                DEFB  $06,$90,$8C,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB  $04,$90,$9D,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$F4
                DEFB  $04,$90,$8C,$F4,$E7,$F1,$D8
                DEFB  $04,$90,$8C,$F4,$E7,$F1,$D8
                DEFB  $FF  ; End of Pattern

PAT7:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$90,$76,$F1,$3B,$F1,$D8
                DEFB      $90,$76,$F1,$3B,$F1,$D8
                DEFB      $90,$8C,$F2,$76,$F1,$D8
                DEFB      $90,$76,$F1,$40,$F1,$F4
                DEFB  $04,$90,$9D,$F2,$31,$F1,$D8
                DEFB      $90,$76,$F2,$7A,$F1,$D8
                DEFB      $90,$8C,$F1,$3B,$F1,$D8
                DEFB      $90,$76,$F1,$3B,$F1,$D8
                DEFB      $90,$76,$F2,$76,$F1,$D8
                DEFB      $90,$76,$F1,$3B,$F1,$A4
                DEFB  $06,$90,$8C,$F2,$31,$F1,$D8
                DEFB      $90,$76,$F2,$76,$F1,$D8
                DEFB  $04,$90,$9D,$F1,$3B,$F1,$D8
                DEFB      $90,$76,$F1,$3B,$F1,$F4
                DEFB      $90,$8C,$F2,$31,$F1,$D8
                DEFB      $90,$8C,$F2,$76,$F1,$D8
                DEFB  $FF  ; End of Pattern

PAT8:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$90,$76,$F0,$EC,$F1,$D8
                DEFB      $90,$76,$F0,$EC,$F1,$D8
                DEFB      $90,$8C,$F1,$D8,$F1,$D8
                DEFB  $09,$90,$76,$F0,$F1,$F1,$F4
                DEFB  $04,$90,$9D,$F1,$D8,$F1,$D8
                DEFB      $90,$76,$F1,$DC,$F1,$D8
                DEFB      $90,$8C,$F3,$B0,$F1,$D8
                DEFB      $90,$76,$F1,$D8,$F1,$D8
                DEFB      $90,$76,$F3,$B0,$F1,$D8
                DEFB      $90,$76,$F3,$B0,$F1,$A4
                DEFB  $06,$90,$8C,$F7,$60,$F1,$D8
                DEFB      $90,$76,$F3,$B0,$F1,$D8
                DEFB  $04,$90,$9D,$F7,$60,$F1,$D8
                DEFB      $90,$76,$F7,$60,$F1,$F4
                DEFB  $04,$90,$8C,$FE,$C1,$F1,$D8
                DEFB  $04,$90,$8C,$F7,$60,$F1,$D8
                DEFB  $FF  ; End of Pattern

PAT9:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$90,$76,$F0,$EC,$F1,$D8
                DEFB      $90,$76,$F0,$EC,$F1,$D8
                DEFB      $90,$8C,$F1,$D8,$F1,$D8
                DEFB  $09,$90,$76,$F0,$F1,$F1,$F4
                DEFB  $04,$90,$9D,$F1,$D8,$F1,$D8
                DEFB      $90,$76,$F1,$DC,$F1,$D8
                DEFB      $90,$8C,$F3,$B0,$F1,$D8
                DEFB      $90,$76,$F1,$D8,$F1,$D8
                DEFB      $90,$76,$F3,$B0,$F1,$D8
                DEFB      $90,$76,$F3,$B0,$F1,$A4
                DEFB  $06,$90,$8C,$F7,$60,$F1,$D8
                DEFB      $90,$76,$F3,$B0,$F1,$D8
                DEFB  $04,$00    ,$F6,$92,$00
                DEFB      $01    ,$F7,$60,$01
                DEFB      $01    ,$F7,$D0,$01
                DEFB      $01    ,$F6,$92,$01
                DEFB  $FF  ; End of Pattern

PAT10:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$90,$76,$F7,$60,$F0,$EC
                DEFB      $90,$76,$F7,$60,$F0,$EC
                DEFB      $90,$8C,$F6,$F6,$F1,$D8
                DEFB      $90,$76,$F6,$9E,$F0,$EC
                DEFB  $04,$90,$9D,$01    ,$F1,$A4
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB      $90,$8C,$01    ,$F0,$EC
                DEFB      $90,$76,$F7,$60,$F0,$EC
                DEFB      $90,$76,$F7,$60,$F0,$EC
                DEFB      $90,$76,$FB,$0D,$F0,$EC
                DEFB  $06,$90,$8C,$F7,$60,$F1,$D8
                DEFB      $90,$76,$FB,$B6,$F0,$EC
                DEFB  $04,$90,$9D,$F7,$60,$F1,$A4
                DEFB      $90,$76,$FD,$25,$F1,$D8
                DEFB      $90,$8C,$F7,$60,$F1,$F4
                DEFB      $90,$8C,$01    ,$F1,$D8
                DEFB  $FF  ; End of Pattern

PAT11:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$90,$76,$F3,$B0,$F0,$EC
                DEFB      $90,$76,$F7,$60,$F0,$EC
                DEFB      $90,$8C,$F9,$D9,$F1,$D8
                DEFB  $09,$90,$76,$FB,$0D,$F0,$EC
                DEFB  $04,$90,$9D,$FD,$25,$F1,$A4
                DEFB      $90,$76,$FB,$B6,$F1,$D8
                DEFB      $90,$8C,$FB,$0D,$F0,$EC
                DEFB      $90,$76,$F9,$D9,$F0,$EC
                DEFB      $90,$76,$F4,$63,$F0,$EC
                DEFB      $90,$76,$F7,$D0,$F0,$EC
                DEFB  $06,$90,$8C,$F3,$E8,$F1,$3B
                DEFB      $90,$76,$F7,$60,$F2,$76
                DEFB  $04,$90,$9D,$F7,$D0,$F2,$76
                DEFB      $90,$76,$F8,$C6,$F1,$3B
                DEFB      $90,$8C,$F7,$60,$F2,$31
                DEFB      $90,$8C,$F7,$D0,$F2,$76
                DEFB  $FF  ; End of Pattern

PAT12:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$90,$76,$F7,$60,$F0,$EC
                DEFB      $90,$76,$FB,$0D,$F0,$EC
                DEFB      $90,$8C,$FB,$B6,$F1,$D8
                DEFB      $90,$76,$F7,$60,$F0,$EC
                DEFB  $04,$90,$9D,$FB,$0D,$F1,$A4
                DEFB      $90,$76,$FB,$B6,$F1,$D8
                DEFB      $90,$8C,$F7,$60,$F0,$EC
                DEFB      $90,$76,$FB,$0D,$F0,$EC
                DEFB      $90,$76,$FB,$B6,$F0,$EC
                DEFB      $90,$76,$FE,$C1,$F0,$EC
                DEFB  $06,$90,$8C,$F9,$D9,$F1,$D8
                DEFB      $90,$76,$F9,$D9,$F0,$EC
                DEFB  $04,$90,$9D,$FE,$C1,$F1,$A4
                DEFB      $90,$76,$F8,$C6,$F1,$D8
                DEFB      $90,$8C,$F8,$C6,$F1,$F4
                DEFB      $90,$8C,$F7,$D0,$F1,$D8
                DEFB  $FF  ; End of Pattern

PAT13:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$90,$76,$FF,$E2,$F0,$EC
                DEFB      $90,$76,$FD,$ED,$F0,$EC
                DEFB      $90,$8C,$FE,$C1,$F1,$D8
                DEFB  $09,$90,$76,$01    ,$F0,$EC
                DEFB  $04,$90,$9D,$01    ,$F1,$A4
                DEFB      $90,$76,$FB,$B6,$F1,$D8
                DEFB      $90,$8C,$FB,$0D,$F0,$EC
                DEFB      $90,$76,$FB,$B6,$F0,$EC
                DEFB      $90,$76,$FB,$0D,$F0,$EC
                DEFB      $90,$76,$FB,$B6,$F0,$EC
                DEFB  $06,$90,$8C,$FB,$0D,$F0,$EC
                DEFB      $90,$76,$FB,$B6,$F1,$D8
                DEFB  $04,$90,$9D,$FB,$0D,$F0,$D2
                DEFB      $90,$76,$FB,$B6,$F1,$A4
                DEFB      $90,$8C,$FB,$0D,$F0,$BB
                DEFB      $90,$8C,$01    ,$F1,$76
                DEFB  $FF  ; End of Pattern

PAT14:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$91,$08,$B2,$11,$F2,$11
                DEFB      $91,$08,$B2,$11,$F2,$11
                DEFB      $91,$08,$B2,$76,$F2,$11
                DEFB      $91,$08,$B2,$11,$F2,$76
                DEFB  $04,$91,$3B,$B2,$11,$F2,$11
                DEFB      $91,$08,$B2,$C3,$F2,$11
                DEFB  $04,$91,$08,$B2,$11,$F2,$C3
                DEFB  $04,$91,$08,$B2,$11,$F2,$11
                DEFB      $91,$61,$B2,$76,$F2,$11
                DEFB      $91,$08,$B2,$11,$F2,$76
                DEFB  $06,$91,$08,$B2,$11,$F2,$11
                DEFB      $91,$08,$B2,$C3,$F2,$11
                DEFB  $04,$91,$8D,$B2,$11,$F2,$C3
                DEFB      $91,$08,$B2,$11,$F2,$11
                DEFB  $04,$91,$61,$B2,$76,$F2,$11
                DEFB  $04,$91,$08,$B2,$11,$F2,$76
                DEFB  $FF  ; End of Pattern

PAT15:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$90,$EC,$00    ,$F1,$D8
                DEFB      $90,$EC,$01    ,$01
                DEFB      $90,$EC,$01    ,$01
                DEFB      $90,$EC,$01    ,$01
                DEFB  $04,$91,$18,$01    ,$01
                DEFB      $90,$EC,$01    ,$F1,$E4
                DEFB      $90,$EC,$01    ,$F1,$E6
                DEFB      $90,$EC,$01    ,$F1,$E8
                DEFB      $91,$3B,$01    ,$F1,$EA
                DEFB      $90,$EC,$01    ,$F1,$EB
                DEFB  $06,$90,$EC,$01    ,$F1,$EC
                DEFB      $90,$EC,$01    ,$F1,$ED
                DEFB  $04,$91,$61,$01    ,$F1,$EE
                DEFB      $90,$EC,$01    ,$F1,$EF
                DEFB      $91,$3B,$01    ,$F1,$F0
                DEFB      $90,$EC,$01    ,$F1,$F1
                DEFB  $FF  ; End of Pattern

PAT16:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$90,$EC,$00    ,$F1,$F2
                DEFB      $90,$EC,$01    ,$F1,$F3
                DEFB      $90,$EC,$01    ,$F1,$F4
                DEFB  $09,$90,$EC,$01    ,$F1,$F5
                DEFB  $04,$91,$18,$01    ,$F1,$F6
                DEFB      $90,$EC,$01    ,$F1,$F8
                DEFB      $90,$EC,$01    ,$F2,$00
                DEFB      $90,$EC,$01    ,$F2,$05
                DEFB  $04,$91,$3B,$01    ,$F2,$0A
                DEFB  $04,$90,$EC,$01    ,$F2,$0F
                DEFB  $04,$90,$EC,$01    ,$F2,$14
                DEFB  $04,$90,$EC,$01    ,$F2,$19
                DEFB  $04,$91,$61,$01    ,$F2,$1E
                DEFB  $04,$90,$EC,$01    ,$F2,$23
                DEFB      $00    ,$01    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT17:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$90,$84,$F2,$C3,$F2,$11
                DEFB      $90,$84,$01    ,$F2,$11
                DEFB      $90,$9D,$01    ,$F2,$11
                DEFB      $90,$84,$F2,$7B,$F2,$31
                DEFB  $04,$90,$B0,$01    ,$F2,$11
                DEFB      $90,$84,$F4,$27,$F2,$11
                DEFB      $90,$9D,$01    ,$F2,$11
                DEFB      $90,$84,$01    ,$F2,$11
                DEFB      $90,$84,$01    ,$F2,$11
                DEFB      $90,$84,$01    ,$F1,$D8
                DEFB  $06,$90,$9D,$F6,$34,$F2,$11
                DEFB      $90,$84,$01    ,$F2,$11
                DEFB  $04,$90,$B0,$F6,$34,$F2,$11
                DEFB      $90,$84,$F5,$86,$F2,$31
                DEFB      $90,$9D,$F6,$34,$F2,$11
                DEFB      $90,$9D,$F5,$86,$F2,$11
                DEFB  $FF  ; End of Pattern

PAT18:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$90,$84,$F6,$92,$F2,$11
                DEFB      $90,$84,$F6,$92,$F2,$11
                DEFB      $90,$9D,$F6,$34,$F2,$11
                DEFB      $90,$84,$F5,$86,$F2,$31
                DEFB  $04,$90,$B0,$F6,$34,$F2,$11
                DEFB      $90,$84,$F6,$92,$F2,$11
                DEFB      $90,$9D,$F6,$44,$F2,$11
                DEFB      $90,$84,$01    ,$F2,$11
                DEFB      $90,$84,$01    ,$F2,$11
                DEFB      $90,$84,$01    ,$F1,$D8
                DEFB  $06,$90,$9D,$01    ,$F2,$11
                DEFB      $90,$84,$01    ,$F2,$11
                DEFB  $04,$90,$B0,$01    ,$F2,$11
                DEFB      $90,$84,$01    ,$F2,$31
                DEFB  $04,$90,$9D,$F5,$86,$F2,$11
                DEFB  $04,$90,$9D,$F5,$86,$F2,$11
                DEFB  $FF  ; End of Pattern

PAT19:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$90,$76,$F5,$DB,$F1,$D8
                DEFB      $90,$76,$F5,$DB,$F1,$D8
                DEFB      $90,$8C,$F5,$86,$F1,$D8
                DEFB      $90,$76,$F4,$E7,$F1,$F4
                DEFB  $04,$90,$9D,$F5,$86,$F1,$D8
                DEFB      $90,$76,$F5,$DB,$F1,$D8
                DEFB      $90,$8C,$F5,$96,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB  $04,$90,$76,$01    ,$F1,$D8
                DEFB  $04,$90,$76,$01    ,$F1,$A4
                DEFB  $04,$90,$8C,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$D8
                DEFB  $04,$90,$9D,$01    ,$F1,$D8
                DEFB      $90,$76,$01    ,$F1,$F4
                DEFB      $01    ,$F4,$E7,$F1,$D8
                DEFB      $01    ,$F3,$BC,$F1,$D8
                DEFB  $FF  ; End of Pattern

PAT20:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $06,$F7,$60,$00    ,$F1,$D8
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$01    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern


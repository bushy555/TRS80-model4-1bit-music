
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
LOOPSTART:            DEFW      PAT0
                      DEFW      PAT0
                      DEFW      PAT1
                      DEFW      PAT2
                      DEFW      PAT3
                      DEFW      PAT3
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
                      DEFW      PAT3
                      DEFW      PAT3
                      DEFW      PAT10
                      DEFW      PAT11
                      DEFW      PAT12
                      DEFW      PAT13
                      DEFW      PAT12
                      DEFW      PAT13
                      DEFW      PAT14
                      DEFW      PAT14
                      DEFW      PAT14
                      DEFW      PAT15
                      DEFW      PAT16
                      DEFW      PAT17
                      DEFW      PAT17
                      DEFW      PAT18
                      DEFW      $0000
                      DEFW      LOOPSTART

; *** Patterns ***
PAT0:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $00    ,$F2,$76,$00
                DEFB      $01    ,$F2,$ED,$01
                DEFB      $01    ,$F3,$49,$01
                DEFB      $01    ,$F2,$ED,$01
                DEFB      $01    ,$F1,$F4,$01
                DEFB      $01    ,$F2,$76,$01
                DEFB      $01    ,$F2,$C3,$01
                DEFB      $01    ,$F2,$76,$01
                DEFB      $01    ,$F2,$31,$01
                DEFB      $01    ,$F2,$C3,$01
                DEFB      $01    ,$F2,$ED,$01
                DEFB      $01    ,$F2,$C3,$01
                DEFB      $01    ,$F2,$31,$01
                DEFB      $01    ,$F2,$C3,$01
                DEFB      $01    ,$F2,$ED,$01
                DEFB      $01    ,$F2,$C3,$01
                DEFB  $FF  ; End of Pattern

PAT1:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $08,$80,$9D,$F2,$76,$00
                DEFB  $08,$80,$9D,$F2,$ED,$01
                DEFB      $81,$3B,$F3,$49,$01
                DEFB      $80,$9D,$F2,$ED,$01
                DEFB      $80,$7D,$F1,$F4,$01
                DEFB      $80,$7D,$F2,$76,$01
                DEFB      $80,$FA,$F2,$C3,$01
                DEFB      $80,$7D,$F2,$76,$01
                DEFB  $08,$80,$BB,$F2,$31,$01
                DEFB  $08,$80,$BB,$F2,$C3,$01
                DEFB      $81,$76,$F2,$ED,$01
                DEFB      $80,$BB,$F2,$C3,$01
                DEFB      $80,$EC,$F2,$31,$01
                DEFB      $80,$EC,$F2,$C3,$01
                DEFB      $81,$D8,$F2,$ED,$01
                DEFB      $80,$EC,$F2,$C3,$01
                DEFB  $FF  ; End of Pattern

PAT2:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $08,$80,$9D,$F2,$76,$00
                DEFB  $08,$80,$9D,$F2,$ED,$01
                DEFB      $81,$3B,$F3,$49,$01
                DEFB      $80,$9D,$F2,$ED,$01
                DEFB      $80,$7D,$F1,$F4,$01
                DEFB      $80,$7D,$F2,$76,$01
                DEFB      $80,$FA,$F2,$C3,$01
                DEFB      $80,$7D,$F2,$76,$01
                DEFB  $08,$80,$BB,$F2,$31,$01
                DEFB  $08,$80,$BB,$F2,$C3,$01
                DEFB      $81,$76,$F2,$ED,$01
                DEFB      $80,$BB,$F2,$C3,$01
                DEFB  $04,$80,$EC,$F2,$31,$01
                DEFB      $00    ,$00    ,$01
                DEFB  $08,$01    ,$F1,$D8,$01
                DEFB  $08,$80,$EC,$F2,$C3,$01
                DEFB  $FF  ; End of Pattern

PAT3:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $08,$80,$9D,$F2,$76,$F2,$76
                DEFB  $08,$80,$9D,$F2,$ED,$01
                DEFB      $81,$3B,$F3,$49,$01
                DEFB      $80,$9D,$F2,$ED,$F2,$ED
                DEFB  $04,$80,$7D,$F1,$F4,$01
                DEFB      $80,$7D,$F2,$76,$01
                DEFB      $80,$FA,$F2,$C3,$F2,$31
                DEFB  $08,$80,$7D,$F2,$76,$01
                DEFB  $08,$80,$BB,$F2,$31,$01
                DEFB  $06,$80,$BB,$F2,$C3,$F3,$49
                DEFB      $81,$76,$F2,$ED,$01
                DEFB      $80,$BB,$F2,$C3,$01
                DEFB  $04,$80,$EC,$F2,$31,$F2,$C3
                DEFB  $08,$80,$EC,$F2,$C3,$01
                DEFB      $81,$D8,$F2,$ED,$F2,$ED
                DEFB      $80,$EC,$F2,$C3,$01
                DEFB  $FF  ; End of Pattern

PAT4:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $08,$80,$9D,$F2,$76,$F2,$76
                DEFB  $08,$80,$9D,$F2,$ED,$01
                DEFB      $81,$3B,$F3,$49,$01
                DEFB      $80,$9D,$F2,$ED,$F2,$ED
                DEFB  $04,$80,$7D,$F1,$F4,$01
                DEFB      $80,$7D,$F2,$76,$01
                DEFB      $80,$FA,$F2,$C3,$F2,$31
                DEFB  $08,$80,$7D,$F2,$76,$01
                DEFB  $08,$80,$BB,$F2,$31,$01
                DEFB  $06,$80,$BB,$F2,$C3,$F3,$49
                DEFB      $81,$76,$F2,$ED,$01
                DEFB      $80,$BB,$F2,$C3,$01
                DEFB  $04,$80,$EC,$F2,$31,$E7,$60
                DEFB  $08,$80,$EC,$F2,$C3,$D7,$60
                DEFB      $81,$D8,$F2,$ED,$C7,$60
                DEFB      $80,$EC,$F2,$C3,$B7,$60
                DEFB  $FF  ; End of Pattern

PAT5:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $08,$80,$9D,$F2,$76,$C7,$60
                DEFB  $08,$80,$9D,$F2,$ED,$00
                DEFB      $81,$3B,$F3,$49,$C7,$60
                DEFB      $80,$9D,$F2,$ED,$00
                DEFB  $04,$80,$7D,$F1,$F4,$C7,$60
                DEFB      $80,$7D,$F2,$76,$00
                DEFB      $80,$FA,$F2,$C3,$C7,$60
                DEFB  $08,$80,$7D,$F2,$76,$00
                DEFB  $08,$80,$BB,$F2,$31,$C7,$60
                DEFB  $06,$80,$BB,$F2,$C3,$C6,$92
                DEFB      $81,$76,$F2,$ED,$C5,$DB
                DEFB      $80,$BB,$F2,$C3,$C4,$63
                DEFB  $04,$80,$EC,$F2,$31,$01
                DEFB  $08,$80,$EC,$F2,$C3,$01
                DEFB      $81,$D8,$F2,$ED,$01
                DEFB      $80,$EC,$F2,$C3,$01
                DEFB  $FF  ; End of Pattern

PAT6:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $08,$80,$9D,$F2,$76,$C3,$B0
                DEFB  $08,$80,$9D,$F2,$ED,$01
                DEFB      $81,$3B,$F3,$49,$00
                DEFB      $80,$9D,$F2,$ED,$C3,$B0
                DEFB  $04,$80,$7D,$F1,$F4,$C6,$92
                DEFB      $80,$7D,$F2,$76,$C3,$B0
                DEFB      $80,$FA,$F2,$C3,$C3,$B0
                DEFB  $08,$80,$7D,$F2,$76,$C7,$60
                DEFB  $08,$80,$BB,$F2,$31,$C3,$B0
                DEFB  $06,$80,$BB,$F2,$C3,$00
                DEFB      $81,$76,$F2,$ED,$C3,$B0
                DEFB      $80,$BB,$F2,$C3,$C4,$63
                DEFB  $04,$80,$EC,$F2,$31,$C4,$63
                DEFB  $08,$80,$EC,$F2,$C3,$C4,$E7
                DEFB      $81,$D8,$F2,$ED,$C4,$63
                DEFB      $80,$EC,$F2,$C3,$C4,$E7
                DEFB  $FF  ; End of Pattern

PAT7:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $08,$80,$9D,$F2,$76,$C4,$E7
                DEFB  $08,$80,$9D,$F2,$ED,$C9,$D9
                DEFB      $81,$3B,$F3,$49,$C4,$E7
                DEFB      $80,$9D,$F2,$ED,$C8,$C6
                DEFB  $04,$80,$7D,$F1,$F4,$C9,$D9
                DEFB      $80,$7D,$F2,$76,$01
                DEFB      $80,$FA,$F2,$C3,$01
                DEFB  $08,$80,$7D,$F2,$76,$01
                DEFB  $08,$80,$BB,$F2,$31,$01
                DEFB  $06,$80,$BB,$F2,$C3,$CB,$0D
                DEFB      $81,$76,$F2,$ED,$00
                DEFB      $80,$BB,$F2,$C3,$C7,$60
                DEFB  $04,$80,$EC,$F2,$31,$00
                DEFB  $08,$80,$EC,$F2,$C3,$01
                DEFB      $81,$D8,$F2,$ED,$C3,$B0
                DEFB      $80,$EC,$F2,$C3,$01
                DEFB  $FF  ; End of Pattern

PAT8:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $08,$80,$9D,$F2,$76,$C3,$B0
                DEFB  $08,$80,$9D,$F2,$ED,$00
                DEFB      $81,$3B,$F3,$49,$C3,$B0
                DEFB      $80,$9D,$F2,$ED,$00
                DEFB  $04,$80,$7D,$F1,$F4,$C3,$B0
                DEFB      $80,$7D,$F2,$76,$00
                DEFB      $80,$FA,$F2,$C3,$C7,$60
                DEFB  $08,$80,$7D,$F2,$76,$C3,$B0
                DEFB  $08,$80,$BB,$F2,$31,$00
                DEFB  $06,$80,$BB,$F2,$C3,$C4,$63
                DEFB      $81,$76,$F2,$ED,$00
                DEFB      $80,$BB,$F2,$C3,$C4,$63
                DEFB  $04,$80,$EC,$F2,$31,$C4,$E7
                DEFB  $08,$80,$EC,$F2,$C3,$00
                DEFB      $81,$D8,$F2,$ED,$C5,$86
                DEFB      $80,$EC,$F2,$C3,$C5,$86
                DEFB  $FF  ; End of Pattern

PAT9:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $08,$80,$9D,$F2,$76,$C3,$B0
                DEFB  $08,$80,$9D,$F2,$ED,$00
                DEFB      $81,$3B,$F3,$49,$C3,$B0
                DEFB      $80,$9D,$F2,$ED,$00
                DEFB  $04,$80,$7D,$F1,$F4,$C3,$B0
                DEFB      $80,$7D,$F2,$76,$00
                DEFB      $80,$FA,$F2,$C3,$C7,$60
                DEFB  $08,$80,$7D,$F2,$76,$C3,$B0
                DEFB  $08,$80,$BB,$F2,$31,$00
                DEFB  $06,$80,$BB,$F2,$C3,$C4,$63
                DEFB      $81,$76,$F2,$ED,$00
                DEFB      $80,$BB,$F2,$C3,$C4,$63
                DEFB  $04,$80,$EC,$F2,$31,$F4,$E7
                DEFB  $04,$80,$EC,$F2,$C3,$F3,$49
                DEFB  $04,$81,$D8,$F2,$ED,$F4,$63
                DEFB  $04,$80,$EC,$F2,$C3,$F4,$E7
                DEFB  $FF  ; End of Pattern

PAT10:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $08,$80,$9D,$F2,$76,$F2,$76
                DEFB  $08,$80,$9D,$F2,$ED,$01
                DEFB  $04,$81,$3B,$F3,$49,$01
                DEFB  $04,$80,$9D,$F2,$ED,$D2,$ED
                DEFB  $04,$80,$7D,$F1,$F4,$01
                DEFB  $04,$80,$7D,$F2,$76,$C2,$ED
                DEFB  $04,$80,$FA,$00    ,$B2,$ED
                DEFB  $08,$00    ,$00    ,$F2,$ED
                DEFB  $08,$00    ,$F2,$31,$01
                DEFB  $06,$00    ,$F2,$C3,$F3,$49
                DEFB      $00    ,$F2,$ED,$01
                DEFB      $80,$BB,$F2,$C3,$01
                DEFB  $04,$80,$EC,$F2,$31,$F2,$C3
                DEFB  $08,$80,$EC,$F2,$C3,$01
                DEFB      $81,$D8,$F2,$ED,$F2,$ED
                DEFB      $80,$EC,$F2,$C3,$01
                DEFB  $FF  ; End of Pattern

PAT11:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $08,$80,$9D,$F2,$76,$F2,$76
                DEFB  $08,$80,$9D,$F2,$ED,$01
                DEFB      $81,$3B,$F3,$49,$01
                DEFB      $80,$9D,$F2,$ED,$F2,$ED
                DEFB  $04,$80,$7D,$F1,$F4,$01
                DEFB      $80,$7D,$F2,$76,$01
                DEFB      $80,$FA,$F2,$C3,$F2,$31
                DEFB  $08,$80,$7D,$F2,$76,$01
                DEFB  $08,$80,$BB,$F2,$31,$01
                DEFB  $06,$80,$BB,$F2,$C3,$F3,$49
                DEFB  $08,$81,$76,$F2,$ED,$01
                DEFB  $08,$80,$BB,$F2,$C3,$01
                DEFB  $04,$80,$EC,$F2,$31,$F2,$C3
                DEFB  $04,$00    ,$00    ,$00
                DEFB  $04,$81,$D8,$F2,$ED,$F2,$ED
                DEFB  $04,$00    ,$00    ,$00
                DEFB  $FF  ; End of Pattern

PAT12:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $08,$80,$B0,$F2,$C3,$D8,$47
                DEFB  $08,$80,$B0,$F3,$49,$D4,$23
                DEFB      $81,$61,$F3,$B0,$D8,$47
                DEFB      $80,$B0,$F3,$49,$D4,$23
                DEFB  $04,$80,$8C,$F2,$31,$D8,$47
                DEFB      $80,$8C,$F2,$C3,$D8,$47
                DEFB      $81,$18,$F3,$1A,$D8,$47
                DEFB  $08,$80,$8C,$F2,$C3,$D4,$23
                DEFB  $08,$80,$D2,$F2,$76,$D8,$47
                DEFB  $06,$80,$D2,$F3,$1A,$D7,$60
                DEFB      $81,$A4,$F3,$49,$D6,$92
                DEFB      $80,$D2,$F3,$1A,$D8,$47
                DEFB  $04,$81,$08,$F2,$76,$01
                DEFB  $08,$81,$08,$F3,$1A,$D4,$23
                DEFB      $82,$11,$F3,$49,$01
                DEFB      $81,$08,$F3,$1A,$D7,$60
                DEFB  $FF  ; End of Pattern

PAT13:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $08,$80,$B0,$F2,$C3,$D4,$23
                DEFB  $08,$80,$B0,$F3,$49,$D4,$23
                DEFB      $81,$61,$F3,$B0,$00
                DEFB      $80,$B0,$F3,$49,$D8,$47
                DEFB  $04,$80,$8C,$F2,$31,$D7,$60
                DEFB      $80,$8C,$F2,$C3,$D4,$23
                DEFB      $81,$18,$F3,$1A,$D4,$23
                DEFB  $08,$80,$8C,$F2,$C3,$D4,$23
                DEFB  $08,$80,$D2,$F2,$76,$D4,$23
                DEFB  $06,$80,$D2,$F3,$1A,$D8,$47
                DEFB      $81,$A4,$F3,$49,$D4,$23
                DEFB      $80,$D2,$F3,$1A,$D4,$23
                DEFB  $04,$81,$08,$F2,$76,$D4,$E7
                DEFB  $08,$81,$08,$F3,$1A,$D8,$47
                DEFB      $82,$11,$F3,$49,$D4,$E7
                DEFB      $81,$08,$F3,$1A,$D4,$23
                DEFB  $FF  ; End of Pattern

PAT14:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $08,$80,$B0,$F2,$C3,$D8,$47
                DEFB  $08,$80,$B0,$F3,$49,$01
                DEFB      $81,$61,$F3,$B0,$01
                DEFB      $80,$B0,$F3,$49,$01
                DEFB  $04,$80,$8C,$F2,$31,$01
                DEFB      $80,$8C,$F2,$C3,$01
                DEFB      $81,$18,$F3,$1A,$01
                DEFB  $08,$80,$8C,$F2,$C3,$01
                DEFB  $08,$80,$D2,$F2,$76,$01
                DEFB  $06,$80,$D2,$F3,$1A,$01
                DEFB      $81,$A4,$F3,$49,$01
                DEFB      $80,$D2,$F3,$1A,$01
                DEFB  $04,$81,$08,$F2,$76,$01
                DEFB  $08,$81,$08,$F3,$1A,$D4,$23
                DEFB      $82,$11,$F3,$49,$01
                DEFB      $81,$08,$F3,$1A,$D7,$60
                DEFB  $FF  ; End of Pattern

PAT15:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $08,$80,$B0,$F2,$C3,$D8,$47
                DEFB  $08,$81,$18,$F3,$49,$01
                DEFB      $01    ,$F3,$B0,$01
                DEFB      $01    ,$F3,$49,$01
                DEFB  $04,$01    ,$F2,$31,$01
                DEFB      $01    ,$F2,$C3,$01
                DEFB      $01    ,$F3,$1A,$01
                DEFB  $08,$01    ,$F2,$C3,$01
                DEFB  $08,$01    ,$F0,$EC,$01
                DEFB  $06,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $04,$01    ,$01    ,$01
                DEFB  $08,$01    ,$01    ,$D4,$23
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$D7,$60
                DEFB  $FF  ; End of Pattern

PAT16:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $08,$80,$EC,$F2,$76,$D9,$D9
                DEFB  $08,$00    ,$F2,$ED,$01
                DEFB      $01    ,$F3,$49,$00
                DEFB      $01    ,$F2,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$F2,$76,$01
                DEFB      $01    ,$F2,$C3,$01
                DEFB      $01    ,$F2,$76,$01
                DEFB      $01    ,$F2,$31,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$F2,$ED,$01
                DEFB      $01    ,$F2,$C3,$01
                DEFB      $01    ,$F2,$31,$01
                DEFB      $01    ,$F2,$C3,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$F2,$C3,$01
                DEFB  $FF  ; End of Pattern

PAT17:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $00    ,$F2,$76,$00
                DEFB      $01    ,$F2,$ED,$01
                DEFB      $01    ,$F3,$49,$01
                DEFB      $01    ,$F2,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$F2,$76,$01
                DEFB      $01    ,$F2,$C3,$01
                DEFB      $01    ,$F2,$76,$01
                DEFB      $01    ,$F2,$31,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$F2,$ED,$01
                DEFB      $01    ,$F2,$C3,$01
                DEFB      $01    ,$F2,$31,$01
                DEFB      $01    ,$F2,$C3,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$F2,$C3,$01
                DEFB  $FF  ; End of Pattern

PAT18:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $00    ,$F2,$76,$00
                DEFB      $01    ,$F2,$ED,$01
                DEFB      $01    ,$F3,$49,$01
                DEFB      $01    ,$F2,$ED,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$F2,$76,$01
                DEFB      $01    ,$F2,$C3,$01
                DEFB      $01    ,$F2,$76,$01
                DEFB      $01    ,$F2,$31,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$F2,$ED,$01
                DEFB      $01    ,$F2,$C3,$01
                DEFB  $08,$01    ,$F2,$31,$01
                DEFB  $08,$01    ,$F2,$C3,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern


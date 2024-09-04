
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
                      DEFW      PAT8
LOOPSTART:            DEFW      PAT9
                      DEFW      PAT8
                      DEFW      PAT9
                      DEFW      PAT0
                      DEFW      PAT0
                      DEFW      PAT1
                      DEFW      PAT1
                      DEFW      PAT2
                      DEFW      PAT2
                      DEFW      PAT3
                      DEFW      PAT4
                      DEFW      PAT5
                      DEFW      PAT6
                      DEFW      PAT7
                      DEFW      PAT10
                      DEFW      PAT11
                      DEFW      PAT12
                      DEFW      PAT13
                      DEFW      PAT5
                      DEFW      PAT6
                      DEFW      PAT7
                      DEFW      PAT14
                      DEFW      PAT16
                      DEFW      PAT17
                      DEFW      PAT15
                      DEFW      PAT18
                      DEFW      $0000
                      DEFW      LOOPSTART

; *** Patterns ***
PAT0:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$FA,$6E,$81,$4D,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $E9,$4B,$91,$4D,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $D8,$47,$A1,$4D,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $0A,$C7,$D0,$B1,$4D,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $04,$B8,$47,$C1,$4D,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $A7,$D0,$B1,$4D,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $04,$96,$F6,$A1,$4D,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $03,$86,$34,$91,$4D,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT1:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$8A,$6E,$81,$08,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $99,$4B,$91,$08,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $A8,$47,$A1,$08,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $0A,$B7,$D0,$B1,$08,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $08,$C8,$47,$C1,$08,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $D7,$D0,$B1,$08,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $05,$E6,$F6,$A1,$08,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $F6,$34,$91,$08,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT2:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $07,$FA,$6E,$80,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $E9,$4B,$90,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$D8,$47,$A0,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $C7,$D0,$B0,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $B8,$47,$C0,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $0A,$A7,$D0,$B0,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $07,$96,$F6,$A0,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $86,$34,$90,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT3:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $07,$84,$A5,$80,$FA,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $07,$95,$37,$90,$FA,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$A6,$92,$A0,$FA,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $0A,$B7,$D0,$B0,$FA,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $C9,$4B,$C0,$FA,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $0A,$DA,$6E,$B0,$FA,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$E9,$4B,$A0,$FA,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F8,$47,$90,$FA,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT4:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$87,$D0,$80,$D2,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $96,$92,$90,$D2,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $09,$A9,$4B,$A0,$D2,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $0A,$B6,$92,$B0,$D2,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $09,$C6,$F6,$C0,$DE,$01
                DEFB  $08,$D6,$F6,$01    ,$01
                DEFB  $07,$E6,$F6,$B0,$DE,$01
                DEFB  $06,$F6,$F6,$01    ,$01
                DEFB  $05,$E6,$F6,$A0,$DE,$01
                DEFB  $04,$D6,$F6,$01    ,$01
                DEFB  $03,$C6,$F6,$90,$DE,$01
                DEFB  $02,$B6,$F6,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT5:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$FA,$6E,$80,$A6,$D1,$BD
                DEFB      $00    ,$01    ,$01
                DEFB      $E9,$4B,$C1,$4D,$E3,$7B
                DEFB      $00    ,$01    ,$01
                DEFB      $D8,$47,$00    ,$00
                DEFB      $00    ,$01    ,$F2,$76
                DEFB  $0A,$C7,$D0,$F1,$4D,$E2,$9B
                DEFB      $00    ,$01    ,$01
                DEFB  $04,$B8,$47,$80,$A6,$00
                DEFB      $00    ,$01    ,$B2,$9B
                DEFB      $A7,$D0,$C1,$4D,$01
                DEFB      $00    ,$01    ,$00
                DEFB  $04,$96,$F6,$00    ,$92,$9B
                DEFB      $00    ,$01    ,$82,$9B
                DEFB  $03,$86,$34,$01    ,$92,$9B
                DEFB      $00    ,$01    ,$A2,$9B
                DEFB  $FF  ; End of Pattern

PAT6:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$8A,$6E,$80,$A6,$B2,$9B
                DEFB      $00    ,$01    ,$C2,$9B
                DEFB      $99,$4B,$C1,$4D,$00
                DEFB      $00    ,$01    ,$01
                DEFB      $A8,$47,$00    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $0A,$B7,$D0,$F1,$4D,$92,$11
                DEFB      $00    ,$01    ,$01
                DEFB  $04,$C8,$47,$80,$A6,$A2,$52
                DEFB      $00    ,$01    ,$01
                DEFB      $D7,$D0,$C1,$4D,$D2,$9B
                DEFB      $00    ,$01    ,$01
                DEFB  $04,$E6,$F6,$00    ,$00
                DEFB      $00    ,$01    ,$B2,$76
                DEFB  $03,$F6,$34,$01    ,$A2,$9B
                DEFB      $00    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT7:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$FA,$6E,$80,$84,$95,$37
                DEFB      $00    ,$01    ,$01
                DEFB      $E9,$4B,$C1,$08,$A4,$A5
                DEFB      $00    ,$01    ,$01
                DEFB      $D8,$47,$00    ,$00
                DEFB      $00    ,$01    ,$B4,$E7
                DEFB  $0A,$C7,$D0,$F1,$08,$C5,$37
                DEFB      $00    ,$01    ,$D5,$37
                DEFB  $04,$B8,$47,$80,$84,$E5,$37
                DEFB      $00    ,$01    ,$F5,$37
                DEFB      $A7,$D0,$C1,$08,$E5,$37
                DEFB      $00    ,$01    ,$D5,$37
                DEFB  $04,$96,$F6,$00    ,$C5,$37
                DEFB      $00    ,$01    ,$B5,$37
                DEFB  $03,$86,$34,$01    ,$C5,$37
                DEFB      $00    ,$01    ,$D5,$37
                DEFB  $FF  ; End of Pattern

PAT8:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $07,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $07,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $07,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $07,$01    ,$01    ,$01
                DEFB  $0A,$01    ,$01    ,$01
                DEFB  $02,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $07,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT9:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $04,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $09,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $09,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $0A,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT10:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$FA,$6E,$80,$84,$94,$A5
                DEFB      $00    ,$01    ,$01
                DEFB      $E9,$4B,$C1,$08,$A4,$23
                DEFB      $00    ,$01    ,$01
                DEFB      $D8,$47,$00    ,$00
                DEFB      $00    ,$01    ,$B4,$63
                DEFB  $0A,$C7,$D0,$F1,$08,$C4,$A5
                DEFB      $00    ,$01    ,$D4,$A5
                DEFB  $04,$B8,$47,$80,$84,$E4,$A5
                DEFB      $00    ,$01    ,$F4,$A5
                DEFB      $A7,$D0,$C1,$08,$E4,$A5
                DEFB      $00    ,$01    ,$D4,$A5
                DEFB  $04,$96,$F6,$00    ,$C4,$A5
                DEFB      $00    ,$01    ,$B4,$A5
                DEFB  $03,$86,$34,$01    ,$C4,$A5
                DEFB      $00    ,$01    ,$D4,$A5
                DEFB  $FF  ; End of Pattern

PAT11:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$FA,$6E,$80,$D2,$A4,$63
                DEFB      $00    ,$90,$D2,$B4,$63
                DEFB      $EA,$6E,$A0,$D2,$C4,$63
                DEFB      $00    ,$90,$D2,$D4,$63
                DEFB      $DA,$6E,$80,$D2,$C4,$63
                DEFB      $00    ,$90,$D2,$B4,$63
                DEFB  $09,$EA,$6E,$A0,$D2,$A4,$63
                DEFB      $00    ,$90,$D2,$B4,$63
                DEFB  $04,$F9,$D9,$80,$EC,$C3,$E8
                DEFB      $00    ,$90,$EC,$01
                DEFB      $E9,$D9,$A0,$EC,$D4,$63
                DEFB      $00    ,$90,$EC,$01
                DEFB  $09,$D9,$D9,$80,$EC,$C4,$E7
                DEFB      $00    ,$90,$EC,$01
                DEFB      $E9,$D9,$A0,$EC,$B4,$63
                DEFB      $00    ,$90,$EC,$01
                DEFB  $FF  ; End of Pattern

PAT12:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$F6,$92,$F1,$61,$83,$E8
                DEFB      $01    ,$E1,$61,$93,$E8
                DEFB      $00    ,$D1,$61,$A3,$E8
                DEFB      $01    ,$C1,$61,$B3,$E8
                DEFB      $F6,$92,$B1,$61,$C3,$E8
                DEFB      $00    ,$A1,$61,$D3,$E8
                DEFB  $09,$E6,$92,$91,$61,$E3,$E8
                DEFB      $01    ,$81,$61,$F3,$E8
                DEFB  $04,$D9,$D9,$91,$61,$E3,$E8
                DEFB      $01    ,$A1,$61,$D3,$E8
                DEFB      $00    ,$B1,$61,$C3,$E8
                DEFB      $01    ,$C1,$61,$B3,$E8
                DEFB  $09,$E8,$C6,$D1,$61,$A3,$E8
                DEFB      $01    ,$E1,$61,$93,$E8
                DEFB      $01    ,$F1,$61,$83,$E8
                DEFB      $01    ,$E1,$61,$93,$E8
                DEFB  $FF  ; End of Pattern

PAT13:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $04,$F6,$F6,$F1,$61,$84,$23
                DEFB      $01    ,$E1,$61,$94,$23
                DEFB  $04,$00    ,$D1,$61,$A4,$23
                DEFB      $01    ,$C1,$61,$B4,$23
                DEFB  $04,$F6,$F6,$B1,$61,$C4,$23
                DEFB  $0A,$00    ,$A1,$61,$D4,$23
                DEFB  $09,$E6,$F6,$91,$61,$E3,$E8
                DEFB      $01    ,$81,$61,$F3,$B0
                DEFB      $DA,$6E,$91,$61,$E3,$7B
                DEFB      $01    ,$A1,$61,$D3,$49
                DEFB  $08,$00    ,$00    ,$C3,$1A
                DEFB  $07,$01    ,$00    ,$B2,$ED
                DEFB  $06,$E8,$47,$D1,$61,$A2,$C3
                DEFB  $05,$01    ,$E1,$61,$92,$9B
                DEFB  $04,$01    ,$00    ,$82,$52
                DEFB  $03,$01    ,$00    ,$92,$11
                DEFB  $FF  ; End of Pattern

PAT14:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$FA,$6E,$80,$84,$94,$23
                DEFB      $00    ,$01    ,$01
                DEFB      $E9,$4B,$C1,$08,$A4,$A5
                DEFB      $00    ,$01    ,$01
                DEFB      $D8,$47,$00    ,$00
                DEFB      $00    ,$01    ,$B4,$E7
                DEFB  $0A,$C7,$D0,$F1,$08,$C5,$37
                DEFB      $00    ,$01    ,$D5,$37
                DEFB  $04,$B8,$47,$80,$84,$E5,$37
                DEFB      $00    ,$01    ,$F5,$37
                DEFB      $A7,$D0,$C1,$08,$E5,$37
                DEFB      $00    ,$01    ,$D5,$37
                DEFB  $04,$96,$F6,$00    ,$C5,$37
                DEFB      $00    ,$01    ,$B5,$37
                DEFB  $03,$86,$34,$01    ,$C5,$37
                DEFB      $00    ,$01    ,$D5,$37
                DEFB  $FF  ; End of Pattern

PAT15:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$FA,$6E,$80,$84,$98,$47
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$C1,$08,$A7,$D0
                DEFB      $01    ,$01    ,$01
                DEFB      $D7,$D0,$00    ,$A8,$47
                DEFB      $00    ,$01    ,$01
                DEFB  $0A,$01    ,$F0,$FA,$C6,$F6
                DEFB      $01    ,$01    ,$01
                DEFB  $04,$B6,$92,$80,$69,$E7,$D0
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$C0,$D2,$00
                DEFB      $01    ,$01    ,$00
                DEFB  $04,$96,$F6,$00    ,$C6,$92
                DEFB      $00    ,$01    ,$01
                DEFB  $03,$01    ,$01    ,$00
                DEFB      $01    ,$01    ,$00
                DEFB  $FF  ; End of Pattern

PAT16:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$FA,$6E,$80,$FA,$A5,$DB
                DEFB      $00    ,$90,$FA,$B6,$34
                DEFB      $EA,$6E,$A0,$FA,$C6,$34
                DEFB      $00    ,$90,$FA,$D6,$34
                DEFB      $DA,$6E,$80,$FA,$C6,$34
                DEFB      $00    ,$90,$FA,$B6,$34
                DEFB  $09,$EA,$6E,$A0,$FA,$A6,$34
                DEFB      $00    ,$90,$FA,$B6,$34
                DEFB  $04,$F9,$D9,$81,$08,$C6,$F6
                DEFB      $00    ,$91,$08,$B6,$F6
                DEFB      $E9,$D9,$A1,$08,$A6,$F6
                DEFB      $00    ,$91,$08,$96,$F6
                DEFB  $09,$D9,$D9,$81,$08,$A6,$F6
                DEFB      $00    ,$91,$08,$B6,$F6
                DEFB      $E9,$D9,$A1,$08,$C6,$F6
                DEFB      $00    ,$91,$08,$D6,$F6
                DEFB  $FF  ; End of Pattern

PAT17:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $09,$01    ,$80,$C6,$87,$D0
                DEFB      $01    ,$01    ,$98,$47
                DEFB      $01    ,$00    ,$A8,$47
                DEFB      $01    ,$01    ,$B8,$47
                DEFB      $01    ,$A0,$C6,$C8,$47
                DEFB      $01    ,$01    ,$D8,$47
                DEFB  $09,$01    ,$90,$DE,$E8,$47
                DEFB  $0A,$01    ,$01    ,$D8,$47
                DEFB  $04,$01    ,$81,$08,$C8,$47
                DEFB      $01    ,$91,$08,$B8,$47
                DEFB      $01    ,$A1,$08,$A8,$47
                DEFB      $01    ,$B1,$08,$98,$47
                DEFB  $09,$01    ,$C1,$08,$88,$47
                DEFB      $01    ,$B1,$08,$98,$47
                DEFB      $01    ,$A1,$08,$A8,$47
                DEFB      $01    ,$91,$08,$98,$47
                DEFB  $FF  ; End of Pattern

PAT18:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $07,$01    ,$01    ,$B6,$F6
                DEFB      $01    ,$01    ,$C6,$F6
                DEFB  $07,$01    ,$01    ,$D6,$F6
                DEFB      $01    ,$01    ,$E6,$F6
                DEFB  $07,$01    ,$01    ,$D6,$F6
                DEFB      $01    ,$01    ,$C6,$F6
                DEFB  $07,$01    ,$01    ,$B6,$F6
                DEFB  $0A,$01    ,$01    ,$A6,$F6
                DEFB  $02,$01    ,$01    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $07,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern


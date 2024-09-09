
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

; ===========================================
THEINVASION:			; The Invasion
SONG18:
; ===========================================

INVASIONLOOPSTART:            DEFW      INVASIONPAT0
                      DEFW      INVASIONPAT58
                      DEFW      INVASIONPAT1
                      DEFW      INVASIONPAT2
                      DEFW      INVASIONPAT2
                      DEFW      INVASIONPAT2
                      DEFW      INVASIONPAT2
                      DEFW      INVASIONPAT3
                      DEFW      INVASIONPAT4
                      DEFW      INVASIONPAT5
                      DEFW      INVASIONPAT5
                      DEFW      INVASIONPAT5
                      DEFW      INVASIONPAT5
                      DEFW      INVASIONPAT6
                      DEFW      INVASIONPAT7
                      DEFW      INVASIONPAT8
                      DEFW      INVASIONPAT8
                      DEFW      INVASIONPAT8
                      DEFW      INVASIONPAT8
                      DEFW      INVASIONPAT9
                      DEFW      INVASIONPAT10
                      DEFW      INVASIONPAT11
                      DEFW      INVASIONPAT10
                      DEFW      INVASIONPAT11
                      DEFW      INVASIONPAT10
                      DEFW      INVASIONPAT9
                      DEFW      INVASIONPAT12
                      DEFW      INVASIONPAT13
                      DEFW      INVASIONPAT13
                      DEFW      INVASIONPAT13
                      DEFW      INVASIONPAT12
                      DEFW      INVASIONPAT14
                      DEFW      INVASIONPAT7
                      DEFW      INVASIONPAT8
                      DEFW      INVASIONPAT7
                      DEFW      INVASIONPAT8
                      DEFW      INVASIONPAT7
                      DEFW      INVASIONPAT9
                      DEFW      INVASIONPAT15
                      DEFW      INVASIONPAT16
                      DEFW      INVASIONPAT16
                      DEFW      INVASIONPAT16
                      DEFW      INVASIONPAT15
                      DEFW      INVASIONPAT17
                      DEFW      INVASIONPAT7
                      DEFW      INVASIONPAT8
                      DEFW      INVASIONPAT7
                      DEFW      INVASIONPAT8
                      DEFW      INVASIONPAT7
                      DEFW      INVASIONPAT9
                      DEFW      INVASIONPAT15
                      DEFW      INVASIONPAT18
                      DEFW      INVASIONPAT18
                      DEFW      INVASIONPAT18
                      DEFW      INVASIONPAT18
                      DEFW      INVASIONPAT18
                      DEFW      INVASIONPAT18
                      DEFW      INVASIONPAT19
                      DEFW      INVASIONPAT20
                      DEFW      INVASIONPAT21
                      DEFW      INVASIONPAT22
                      DEFW      INVASIONPAT23
                      DEFW      INVASIONPAT22
                      DEFW      INVASIONPAT24
                      DEFW      INVASIONPAT25
                      DEFW      INVASIONPAT26
                      DEFW      INVASIONPAT27
                      DEFW      INVASIONPAT28
                      DEFW      INVASIONPAT29
                      DEFW      INVASIONPAT30
                      DEFW      INVASIONPAT30
                      DEFW      INVASIONPAT30
                      DEFW      INVASIONPAT31
                      DEFW      INVASIONPAT31
                      DEFW      INVASIONPAT31
                      DEFW      INVASIONPAT32
                      DEFW      INVASIONPAT33
                      DEFW      INVASIONPAT34
                      DEFW      INVASIONPAT34
                      DEFW      INVASIONPAT34
                      DEFW      INVASIONPAT35
                      DEFW      INVASIONPAT35
                      DEFW      INVASIONPAT35
                      DEFW      INVASIONPAT36
                      DEFW      INVASIONPAT37
                      DEFW      INVASIONPAT38
                      DEFW      INVASIONPAT39
                      DEFW      INVASIONPAT38
                      DEFW      INVASIONPAT37
                      DEFW      INVASIONPAT38
                      DEFW      INVASIONPAT39
                      DEFW      INVASIONPAT40
                      DEFW      INVASIONPAT41
                      DEFW      INVASIONPAT42
                      DEFW      INVASIONPAT41
                      DEFW      INVASIONPAT42
                      DEFW      INVASIONPAT41
                      DEFW      INVASIONPAT42
                      DEFW      INVASIONPAT41
                      DEFW      INVASIONPAT42
                      DEFW      INVASIONPAT43
                      DEFW      INVASIONPAT44
                      DEFW      INVASIONPAT45
                      DEFW      INVASIONPAT44
                      DEFW      INVASIONPAT43
                      DEFW      INVASIONPAT44
                      DEFW      INVASIONPAT45
                      DEFW      INVASIONPAT46
                      DEFW      INVASIONPAT47
                      DEFW      INVASIONPAT48
                      DEFW      INVASIONPAT47
                      DEFW      INVASIONPAT48
                      DEFW      INVASIONPAT41
                      DEFW      INVASIONPAT42
                      DEFW      INVASIONPAT41
                      DEFW      INVASIONPAT49
                      DEFW      INVASIONPAT50
                      DEFW      INVASIONPAT51
                      DEFW      INVASIONPAT52
                      DEFW      INVASIONPAT53
                      DEFW      INVASIONPAT54
                      DEFW      INVASIONPAT55
                      DEFW      INVASIONPAT56
                      DEFW      INVASIONPAT57
                      DEFW      INVASIONPAT50
                      DEFW      INVASIONPAT51
                      DEFW      INVASIONPAT52
                      DEFW      INVASIONPAT53
                      DEFW      $0000
                      DEFW      INVASIONLOOPSTART

; *** INVASIONPATterns ***
INVASIONPAT0:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $08,$F5,$86,$D5,$86,$B5,$86
                DEFB      $F5,$37,$D5,$37,$85,$37
                DEFB      $E4,$EC,$C4,$EC,$A4,$EC
                DEFB      $E4,$A5,$C4,$A5,$A4,$A5
                DEFB      $D4,$63,$B4,$63,$00
                DEFB      $D4,$23,$B4,$23,$01
                DEFB      $C3,$E8,$00    ,$01
                DEFB      $C3,$B0,$01    ,$01
                DEFB      $F5,$37,$01    ,$01
                DEFB      $F4,$EC,$8B,$B6,$01
                DEFB      $E4,$A5,$00    ,$01
                DEFB      $E4,$63,$01    ,$01
                DEFB      $D4,$23,$01    ,$01
                DEFB      $D3,$E8,$01    ,$01
                DEFB      $C3,$B0,$01    ,$01
                DEFB      $C3,$7B,$01    ,$01
                DEFB  $07,$F4,$EC,$F4,$EC,$F4,$EC
                DEFB      $F4,$A5,$F4,$A5,$F4,$A5
                DEFB      $E4,$63,$E4,$63,$E4,$63
                DEFB      $E4,$23,$E4,$23,$E4,$23
                DEFB      $D3,$E8,$D3,$E8,$00
                DEFB      $D3,$B0,$D3,$B0,$01
                DEFB      $C3,$7B,$00    ,$01
                DEFB      $C3,$49,$01    ,$01
                DEFB      $F4,$A5,$01    ,$8B,$B6
                DEFB      $F4,$63,$01    ,$00
                DEFB      $E4,$23,$01    ,$01
                DEFB      $E3,$E8,$01    ,$01
                DEFB      $D3,$B0,$01    ,$01
                DEFB      $D3,$7B,$01    ,$01
                DEFB      $C3,$49,$01    ,$01
                DEFB      $C3,$1A,$01    ,$01
                DEFB  $06,$F4,$63,$F4,$63,$F4,$63
                DEFB      $F4,$23,$F4,$23,$B4,$23
                DEFB      $E3,$E8,$E3,$E8,$83,$E8
                DEFB      $E3,$B0,$E3,$B0,$E3,$B0
                DEFB      $D3,$7B,$D3,$7B,$00
                DEFB      $D3,$49,$D3,$49,$01
                DEFB      $C3,$1A,$00    ,$01
                DEFB      $C2,$ED,$01    ,$01
                DEFB      $F4,$23,$85,$DB,$8B,$B6
                DEFB      $F3,$E8,$00    ,$00
                DEFB      $E3,$B0,$01    ,$01
                DEFB      $E3,$7B,$01    ,$01
                DEFB      $D3,$49,$01    ,$01
                DEFB      $D3,$1A,$01    ,$01
                DEFB      $C2,$ED,$01    ,$01
                DEFB      $C2,$C3,$01    ,$01
                DEFB  $05,$F3,$E8,$D3,$E8,$B3,$E8
                DEFB      $F3,$B0,$D3,$B0,$F3,$B0
                DEFB      $E3,$7B,$C3,$7B,$A3,$7B
                DEFB      $E3,$49,$C3,$49,$A3,$49
                DEFB      $D3,$1A,$B3,$1A,$00
                DEFB      $D2,$ED,$B2,$ED,$01
                DEFB      $C2,$C3,$00    ,$01
                DEFB      $C2,$9B,$01    ,$01
                DEFB      $84,$63,$88,$C6,$81,$8C
                DEFB      $F3,$7B,$00    ,$00
                DEFB      $E3,$49,$01    ,$01
                DEFB      $E3,$1A,$01    ,$01
                DEFB      $D2,$ED,$01    ,$01
                DEFB      $D2,$C3,$01    ,$01
                DEFB      $C2,$9B,$01    ,$01
                DEFB      $C2,$76,$01    ,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT1:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$82,$52,$82,$ED,$81,$61
                DEFB      $81,$61,$82,$31,$91,$61
                DEFB      $80,$FA,$81,$8D,$A1,$61
                DEFB      $80,$94,$90,$A6,$00
                DEFB      $F1,$29,$D1,$8D,$81,$8D
                DEFB      $01    ,$01    ,$91,$8D
                DEFB      $01    ,$A0,$C6,$A1,$8D
                DEFB      $01    ,$01    ,$00
                DEFB      $01    ,$01    ,$81,$8D
                DEFB      $01    ,$01    ,$91,$8D
                DEFB      $01    ,$01    ,$A1,$8D
                DEFB      $01    ,$01    ,$00
                DEFB      $01    ,$01    ,$81,$8D
                DEFB      $01    ,$01    ,$91,$8D
                DEFB      $01    ,$01    ,$A1,$8D
                DEFB      $01    ,$01    ,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT2:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $F1,$29,$A0,$C6,$81,$61
                DEFB      $01    ,$01    ,$91,$61
                DEFB      $01    ,$01    ,$A1,$61
                DEFB      $01    ,$01    ,$00
                DEFB      $01    ,$01    ,$81,$8D
                DEFB      $01    ,$01    ,$91,$8D
                DEFB      $01    ,$01    ,$A1,$8D
                DEFB      $01    ,$01    ,$00
                DEFB      $01    ,$01    ,$81,$8D
                DEFB      $01    ,$01    ,$91,$8D
                DEFB      $01    ,$01    ,$A1,$8D
                DEFB      $01    ,$01    ,$00
                DEFB      $01    ,$01    ,$81,$8D
                DEFB      $01    ,$01    ,$91,$8D
                DEFB      $01    ,$01    ,$A1,$8D
                DEFB      $01    ,$01    ,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT3:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $F1,$29,$A0,$C6,$81,$61
                DEFB      $01    ,$01    ,$91,$61
                DEFB      $01    ,$01    ,$A1,$61
                DEFB      $01    ,$01    ,$00
                DEFB      $01    ,$01    ,$81,$8D
                DEFB      $01    ,$01    ,$91,$8D
                DEFB      $01    ,$01    ,$A1,$8D
                DEFB      $01    ,$01    ,$00
                DEFB  $13,$A0,$C6,$84,$A5,$81,$D8
                DEFB      $01    ,$83,$7B,$91,$D8
                DEFB      $01    ,$82,$76,$A1,$D8
                DEFB      $01    ,$82,$52,$00
                DEFB  $13,$01    ,$84,$A5,$81,$61
                DEFB      $01    ,$83,$7B,$91,$61
                DEFB      $01    ,$82,$76,$A1,$61
                DEFB      $01    ,$82,$52,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT4:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$82,$52,$82,$ED,$81,$61
                DEFB      $81,$61,$82,$31,$91,$61
                DEFB      $80,$FA,$81,$8D,$A1,$61
                DEFB      $80,$94,$90,$A6,$00
                DEFB      $B2,$52,$D1,$8D,$81,$8D
                DEFB      $C2,$52,$01    ,$91,$8D
                DEFB      $D2,$52,$A0,$C6,$A1,$8D
                DEFB      $00    ,$01    ,$00
                DEFB      $B2,$52,$01    ,$81,$8D
                DEFB      $C2,$52,$01    ,$91,$8D
                DEFB      $D2,$52,$01    ,$A1,$8D
                DEFB      $00    ,$01    ,$00
                DEFB      $B2,$52,$01    ,$81,$8D
                DEFB      $C2,$52,$01    ,$91,$8D
                DEFB      $D2,$52,$01    ,$A1,$8D
                DEFB      $00    ,$01    ,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT5:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $B2,$11,$A0,$C6,$81,$61
                DEFB      $C2,$11,$01    ,$91,$61
                DEFB      $D2,$11,$01    ,$A1,$61
                DEFB      $00    ,$01    ,$00
                DEFB      $B2,$52,$01    ,$81,$8D
                DEFB      $C2,$52,$01    ,$91,$8D
                DEFB      $D2,$52,$01    ,$A1,$8D
                DEFB      $00    ,$01    ,$00
                DEFB      $B2,$52,$01    ,$81,$8D
                DEFB      $C2,$52,$01    ,$91,$8D
                DEFB      $D2,$52,$01    ,$A1,$8D
                DEFB      $00    ,$01    ,$00
                DEFB      $B2,$52,$01    ,$81,$8D
                DEFB      $C2,$52,$01    ,$91,$8D
                DEFB      $D2,$52,$01    ,$A1,$8D
                DEFB      $00    ,$01    ,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT6:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $87,$60,$A0,$C6,$81,$61
                DEFB      $F7,$60,$01    ,$91,$61
                DEFB      $86,$34,$01    ,$A1,$61
                DEFB      $F6,$34,$01    ,$00
                DEFB      $85,$86,$01    ,$81,$8D
                DEFB      $F5,$86,$01    ,$91,$8D
                DEFB      $86,$34,$01    ,$A1,$8D
                DEFB      $F6,$34,$01    ,$00
                DEFB  $13,$87,$60,$84,$A5,$81,$D8
                DEFB      $F7,$60,$83,$7B,$91,$D8
                DEFB      $86,$34,$82,$76,$A1,$D8
                DEFB      $F6,$34,$82,$52,$00
                DEFB  $13,$87,$60,$84,$A5,$82,$11
                DEFB      $F7,$60,$83,$7B,$92,$11
                DEFB      $88,$47,$82,$76,$A2,$11
                DEFB      $F8,$47,$82,$52,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT7:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$87,$60,$82,$ED,$81,$61
                DEFB      $F7,$D0,$82,$31,$91,$61
                DEFB      $88,$47,$81,$8D,$A1,$61
                DEFB      $F8,$C6,$90,$A6,$00
                DEFB      $89,$4B,$D1,$8D,$81,$8D
                DEFB      $F9,$4B,$01    ,$91,$8D
                DEFB      $89,$4B,$A0,$C6,$A1,$8D
                DEFB      $F9,$4B,$01    ,$00
                DEFB      $89,$4B,$01    ,$81,$8D
                DEFB      $F9,$4B,$01    ,$91,$8D
                DEFB      $89,$4B,$01    ,$A1,$8D
                DEFB      $F9,$4B,$01    ,$00
                DEFB      $89,$4B,$01    ,$81,$8D
                DEFB      $F9,$4B,$01    ,$91,$8D
                DEFB      $89,$4B,$01    ,$A1,$8D
                DEFB      $F9,$4B,$01    ,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT8:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$89,$4B,$82,$ED,$81,$61
                DEFB      $F9,$4B,$82,$31,$91,$61
                DEFB      $89,$4B,$81,$8D,$A1,$61
                DEFB      $F9,$4B,$90,$A6,$00
                DEFB      $89,$4B,$D1,$8D,$81,$8D
                DEFB      $F9,$4B,$01    ,$91,$8D
                DEFB      $89,$4B,$A0,$C6,$A1,$8D
                DEFB      $F9,$4B,$01    ,$00
                DEFB      $89,$4B,$01    ,$81,$8D
                DEFB      $F9,$4B,$01    ,$91,$8D
                DEFB      $89,$4B,$01    ,$A1,$8D
                DEFB      $F9,$4B,$01    ,$00
                DEFB      $89,$4B,$01    ,$81,$8D
                DEFB      $F9,$4B,$01    ,$91,$8D
                DEFB      $89,$4B,$01    ,$A1,$8D
                DEFB      $F9,$4B,$01    ,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT9:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$87,$60,$82,$ED,$81,$61
                DEFB      $F7,$60,$82,$31,$91,$61
                DEFB      $86,$34,$81,$8D,$A1,$61
                DEFB      $F6,$34,$90,$A6,$00
                DEFB      $85,$86,$D1,$8D,$81,$8D
                DEFB      $F5,$86,$01    ,$91,$8D
                DEFB      $86,$34,$A0,$C6,$A1,$8D
                DEFB      $F6,$34,$01    ,$00
                DEFB  $13,$87,$60,$84,$A5,$81,$D8
                DEFB      $F7,$60,$83,$7B,$91,$D8
                DEFB      $86,$34,$82,$76,$A1,$D8
                DEFB      $F6,$34,$82,$52,$00
                DEFB  $13,$87,$60,$84,$A5,$82,$11
                DEFB      $F7,$60,$83,$7B,$92,$11
                DEFB      $88,$47,$82,$76,$A2,$11
                DEFB      $F8,$47,$82,$52,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT10:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$89,$D9,$82,$ED,$81,$61
                DEFB      $FA,$6E,$82,$31,$91,$61
                DEFB      $8B,$0D,$81,$8D,$A1,$61
                DEFB      $FB,$B6,$90,$A6,$00
                DEFB      $8C,$68,$D1,$8D,$81,$8D
                DEFB      $FC,$68,$01    ,$91,$8D
                DEFB      $8C,$68,$A0,$C6,$A1,$8D
                DEFB      $FC,$68,$01    ,$00
                DEFB      $8C,$68,$01    ,$81,$8D
                DEFB      $FC,$68,$01    ,$91,$8D
                DEFB      $8C,$68,$01    ,$A1,$8D
                DEFB      $FC,$68,$01    ,$00
                DEFB      $8C,$68,$01    ,$81,$8D
                DEFB      $FC,$68,$01    ,$91,$8D
                DEFB      $8C,$68,$01    ,$A1,$8D
                DEFB      $FC,$68,$01    ,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT11:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$8C,$68,$82,$ED,$81,$61
                DEFB      $FC,$68,$82,$31,$91,$61
                DEFB      $8C,$68,$81,$8D,$A1,$61
                DEFB      $FC,$68,$90,$A6,$00
                DEFB      $8C,$68,$D1,$8D,$81,$8D
                DEFB      $FC,$68,$01    ,$91,$8D
                DEFB      $8C,$68,$A0,$C6,$A1,$8D
                DEFB      $FC,$68,$01    ,$00
                DEFB      $8C,$68,$01    ,$81,$8D
                DEFB      $FC,$68,$01    ,$91,$8D
                DEFB      $8C,$68,$01    ,$A1,$8D
                DEFB      $FC,$68,$01    ,$00
                DEFB      $8C,$68,$01    ,$81,$8D
                DEFB      $FC,$68,$01    ,$91,$8D
                DEFB      $8C,$68,$01    ,$A1,$8D
                DEFB      $FC,$68,$01    ,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT12:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$86,$92,$82,$ED,$81,$D8
                DEFB      $F6,$F6,$82,$31,$91,$D8
                DEFB      $87,$60,$81,$8D,$A1,$D8
                DEFB      $F7,$D0,$90,$A6,$00
                DEFB      $88,$47,$D2,$11,$82,$11
                DEFB      $F8,$47,$01    ,$92,$11
                DEFB      $88,$47,$A1,$08,$A2,$11
                DEFB      $F8,$47,$01    ,$00
                DEFB      $88,$47,$01    ,$82,$11
                DEFB      $F8,$47,$01    ,$92,$11
                DEFB      $88,$47,$01    ,$A2,$11
                DEFB      $F8,$47,$01    ,$00
                DEFB      $88,$47,$01    ,$82,$11
                DEFB      $F8,$47,$01    ,$92,$11
                DEFB      $88,$47,$01    ,$A2,$11
                DEFB      $F8,$47,$01    ,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT13:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$88,$47,$82,$ED,$81,$D8
                DEFB      $F8,$47,$82,$31,$91,$D8
                DEFB      $88,$47,$81,$8D,$A1,$D8
                DEFB      $F8,$47,$90,$A6,$00
                DEFB      $88,$47,$D2,$11,$82,$11
                DEFB      $F8,$47,$01    ,$92,$11
                DEFB      $88,$47,$A1,$08,$A2,$11
                DEFB      $F8,$47,$01    ,$00
                DEFB      $88,$47,$01    ,$82,$11
                DEFB      $F8,$47,$01    ,$92,$11
                DEFB      $88,$47,$01    ,$A2,$11
                DEFB      $F8,$47,$01    ,$00
                DEFB      $88,$47,$01    ,$82,$11
                DEFB      $F8,$47,$01    ,$92,$11
                DEFB      $88,$47,$01    ,$A2,$11
                DEFB      $F8,$47,$01    ,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT14:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$87,$60,$82,$ED,$81,$D8
                DEFB      $F7,$60,$82,$31,$91,$D8
                DEFB      $86,$34,$81,$8D,$A1,$D8
                DEFB      $F6,$34,$90,$A6,$00
                DEFB      $85,$86,$D2,$11,$82,$11
                DEFB      $F5,$86,$01    ,$92,$11
                DEFB      $86,$34,$A1,$08,$A2,$11
                DEFB      $F6,$34,$01    ,$00
                DEFB  $13,$87,$60,$84,$A5,$82,$76
                DEFB      $F7,$60,$83,$7B,$92,$76
                DEFB      $86,$34,$82,$76,$A2,$76
                DEFB      $F6,$34,$82,$52,$00
                DEFB  $13,$87,$60,$84,$A5,$82,$C3
                DEFB      $F7,$60,$83,$7B,$92,$C3
                DEFB      $88,$47,$82,$76,$A2,$C3
                DEFB      $F8,$47,$82,$52,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT15:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$88,$47,$82,$ED,$81,$8D
                DEFB      $F8,$C6,$82,$31,$91,$8D
                DEFB      $89,$4B,$81,$8D,$A1,$8D
                DEFB      $F9,$D9,$90,$A6,$00
                DEFB      $8A,$6E,$D1,$BD,$81,$BD
                DEFB      $FA,$6E,$01    ,$91,$BD
                DEFB      $8A,$6E,$A0,$DE,$A1,$BD
                DEFB      $FA,$6E,$01    ,$00
                DEFB      $8A,$6E,$01    ,$81,$BD
                DEFB      $FA,$6E,$01    ,$91,$BD
                DEFB      $8A,$6E,$01    ,$A1,$BD
                DEFB      $FA,$6E,$01    ,$00
                DEFB      $8A,$6E,$01    ,$81,$BD
                DEFB      $FA,$6E,$01    ,$91,$BD
                DEFB      $8A,$6E,$01    ,$A1,$BD
                DEFB      $FA,$6E,$01    ,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT16:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$8A,$6E,$82,$ED,$81,$8D
                DEFB      $FA,$6E,$82,$31,$91,$8D
                DEFB      $8A,$6E,$81,$8D,$A1,$8D
                DEFB      $FA,$6E,$90,$A6,$00
                DEFB      $8A,$6E,$D1,$BD,$81,$BD
                DEFB      $FA,$6E,$01    ,$91,$BD
                DEFB      $8A,$6E,$A0,$DE,$A1,$BD
                DEFB      $FA,$6E,$01    ,$00
                DEFB      $8A,$6E,$01    ,$81,$BD
                DEFB      $FA,$6E,$01    ,$91,$BD
                DEFB      $8A,$6E,$01    ,$A1,$BD
                DEFB      $FA,$6E,$01    ,$00
                DEFB      $8A,$6E,$01    ,$81,$BD
                DEFB      $FA,$6E,$01    ,$91,$BD
                DEFB      $8A,$6E,$01    ,$A1,$BD
                DEFB      $FA,$6E,$01    ,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT17:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$8A,$6E,$82,$ED,$81,$8D
                DEFB      $FA,$6E,$82,$31,$91,$8D
                DEFB      $8A,$6E,$81,$8D,$A1,$8D
                DEFB      $F9,$D9,$90,$A6,$00
                DEFB      $89,$4B,$D1,$BD,$81,$BD
                DEFB      $F8,$C6,$01    ,$91,$BD
                DEFB      $88,$47,$A0,$DE,$A1,$BD
                DEFB      $F7,$D0,$01    ,$00
                DEFB  $13,$87,$60,$84,$A5,$82,$11
                DEFB      $F6,$F6,$83,$7B,$92,$11
                DEFB      $86,$92,$82,$76,$A2,$11
                DEFB      $F6,$34,$82,$52,$00
                DEFB  $13,$85,$DB,$84,$A5,$82,$52
                DEFB      $F5,$86,$83,$7B,$92,$52
                DEFB      $85,$37,$82,$76,$A2,$52
                DEFB      $F4,$EC,$82,$52,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT18:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $8A,$6E,$A0,$DE,$81,$8D
                DEFB      $FA,$6E,$01    ,$91,$8D
                DEFB      $8A,$6E,$01    ,$A1,$8D
                DEFB      $FA,$6E,$01    ,$00
                DEFB      $8A,$6E,$01    ,$81,$BD
                DEFB      $FA,$6E,$01    ,$91,$BD
                DEFB      $8A,$6E,$01    ,$A1,$BD
                DEFB      $FA,$6E,$01    ,$00
                DEFB      $8A,$6E,$01    ,$81,$BD
                DEFB      $FA,$6E,$01    ,$91,$BD
                DEFB      $8A,$6E,$01    ,$A1,$BD
                DEFB      $FA,$6E,$01    ,$00
                DEFB      $8A,$6E,$01    ,$81,$BD
                DEFB      $FA,$6E,$01    ,$91,$BD
                DEFB      $8A,$6E,$01    ,$A1,$BD
                DEFB      $FA,$6E,$01    ,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT19:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $8A,$6E,$A0,$DE,$81,$8D
                DEFB      $FA,$6E,$01    ,$91,$8D
                DEFB      $8A,$6E,$01    ,$A1,$8D
                DEFB      $FA,$6E,$01    ,$00
                DEFB      $8A,$6E,$01    ,$81,$BD
                DEFB      $FA,$6E,$01    ,$91,$BD
                DEFB      $8A,$6E,$01    ,$A1,$BD
                DEFB      $FA,$6E,$01    ,$00
                DEFB  $13,$8A,$6E,$83,$B0,$81,$BD
                DEFB      $00    ,$00    ,$00
                DEFB      $8A,$6E,$81,$F4,$91,$BD
                DEFB      $00    ,$00    ,$00
                DEFB  $13,$88,$47,$83,$B0,$A1,$BD
                DEFB      $00    ,$00    ,$00
                DEFB  $13,$86,$92,$83,$B0,$B1,$BD
                DEFB      $00    ,$00    ,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT20:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$82,$9B,$82,$ED,$81,$8D
                DEFB      $81,$8D,$82,$31,$91,$8D
                DEFB      $81,$18,$81,$8D,$A1,$8D
                DEFB      $80,$A6,$90,$A6,$00
                DEFB  $02,$C1,$BD,$82,$ED,$81,$BD
                DEFB      $01    ,$82,$31,$91,$BD
                DEFB      $01    ,$81,$8D,$A1,$BD
                DEFB      $00    ,$90,$A6,$00
                DEFB  $13,$C1,$BD,$D0,$DE,$81,$BD
                DEFB  $14,$01    ,$01    ,$91,$BD
                DEFB  $15,$01    ,$A0,$DE,$A1,$BD
                DEFB      $00    ,$01    ,$00
                DEFB  $02,$C1,$BD,$82,$ED,$81,$BD
                DEFB      $01    ,$82,$31,$91,$BD
                DEFB      $01    ,$81,$8D,$A1,$BD
                DEFB      $00    ,$90,$A6,$00
                DEFB  $13,$C1,$8D,$84,$A5,$81,$8D
                DEFB      $01    ,$83,$7B,$91,$8D
                DEFB      $01    ,$82,$76,$A1,$8D
                DEFB      $00    ,$82,$52,$00
                DEFB      $C1,$BD,$D0,$DE,$81,$BD
                DEFB      $01    ,$01    ,$91,$BD
                DEFB      $01    ,$A0,$DE,$A1,$BD
                DEFB      $00    ,$01    ,$00
                DEFB  $02,$C1,$BD,$82,$ED,$81,$BD
                DEFB      $01    ,$82,$31,$91,$BD
                DEFB  $03,$01    ,$82,$ED,$A1,$BD
                DEFB      $00    ,$82,$31,$00
                DEFB  $13,$C1,$BD,$84,$A5,$81,$BD
                DEFB      $01    ,$83,$7B,$91,$BD
                DEFB      $01    ,$82,$76,$A1,$BD
                DEFB      $00    ,$82,$52,$00
                DEFB      $C1,$8D,$D0,$DE,$81,$8D
                DEFB      $01    ,$01    ,$91,$8D
                DEFB      $01    ,$A0,$DE,$A1,$8D
                DEFB      $00    ,$01    ,$00
                DEFB  $02,$C1,$BD,$82,$ED,$81,$BD
                DEFB      $01    ,$82,$31,$91,$BD
                DEFB      $01    ,$81,$8D,$A1,$BD
                DEFB      $00    ,$90,$A6,$00
                DEFB  $02,$C1,$BD,$82,$ED,$81,$BD
                DEFB      $01    ,$82,$31,$91,$BD
                DEFB      $01    ,$81,$8D,$A1,$BD
                DEFB      $00    ,$90,$A6,$00
                DEFB  $13,$C1,$BD,$D0,$DE,$81,$BD
                DEFB  $14,$01    ,$01    ,$91,$BD
                DEFB  $15,$01    ,$A0,$DE,$A1,$BD
                DEFB      $00    ,$01    ,$00
                DEFB  $13,$C1,$8D,$84,$A5,$81,$8D
                DEFB      $01    ,$83,$7B,$91,$8D
                DEFB      $01    ,$82,$76,$A1,$8D
                DEFB      $00    ,$82,$52,$00
                DEFB  $02,$C1,$BD,$82,$ED,$81,$BD
                DEFB      $01    ,$82,$31,$91,$BD
                DEFB      $01    ,$81,$8D,$A1,$BD
                DEFB      $00    ,$90,$A6,$00
                DEFB  $02,$C2,$11,$82,$ED,$82,$11
                DEFB      $01    ,$82,$31,$92,$11
                DEFB      $01    ,$81,$8D,$A2,$11
                DEFB      $00    ,$90,$A6,$00
                DEFB  $13,$C1,$BD,$84,$A5,$81,$BD
                DEFB      $01    ,$83,$7B,$91,$BD
                DEFB      $01    ,$82,$76,$A1,$BD
                DEFB      $00    ,$82,$52,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT21:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$C1,$8D,$82,$ED,$81,$8D
                DEFB      $01    ,$82,$31,$91,$8D
                DEFB      $01    ,$81,$8D,$A1,$8D
                DEFB      $00    ,$90,$A6,$00
                DEFB  $02,$C1,$BD,$82,$ED,$81,$BD
                DEFB      $01    ,$82,$31,$91,$BD
                DEFB      $01    ,$81,$8D,$A1,$BD
                DEFB      $00    ,$90,$A6,$00
                DEFB  $13,$C1,$BD,$D0,$DE,$81,$BD
                DEFB  $14,$01    ,$01    ,$91,$BD
                DEFB  $15,$01    ,$A0,$DE,$A1,$BD
                DEFB      $00    ,$01    ,$00
                DEFB  $02,$C1,$BD,$82,$ED,$81,$BD
                DEFB      $01    ,$82,$31,$91,$BD
                DEFB      $01    ,$81,$8D,$A1,$BD
                DEFB      $00    ,$90,$A6,$00
                DEFB  $13,$C1,$8D,$84,$A5,$81,$8D
                DEFB      $01    ,$83,$7B,$91,$8D
                DEFB      $01    ,$82,$76,$A1,$8D
                DEFB      $00    ,$82,$52,$00
                DEFB      $C1,$BD,$D0,$DE,$81,$BD
                DEFB      $01    ,$01    ,$91,$BD
                DEFB      $01    ,$A0,$DE,$A1,$BD
                DEFB      $00    ,$01    ,$00
                DEFB  $02,$C1,$BD,$82,$ED,$81,$BD
                DEFB      $01    ,$82,$31,$91,$BD
                DEFB  $03,$01    ,$82,$ED,$A1,$BD
                DEFB      $00    ,$82,$31,$00
                DEFB  $13,$C1,$BD,$84,$A5,$81,$BD
                DEFB      $01    ,$83,$7B,$91,$BD
                DEFB      $01    ,$82,$76,$A1,$BD
                DEFB      $00    ,$82,$52,$00
                DEFB      $C1,$8D,$D0,$DE,$81,$8D
                DEFB      $01    ,$01    ,$00
                DEFB      $01    ,$A0,$DE,$A1,$8D
                DEFB      $00    ,$01    ,$00
                DEFB  $02,$C1,$BD,$82,$ED,$81,$BD
                DEFB      $01    ,$82,$31,$00
                DEFB      $01    ,$81,$8D,$A1,$BD
                DEFB      $00    ,$90,$A6,$00
                DEFB  $02,$C1,$BD,$82,$ED,$82,$11
                DEFB      $01    ,$82,$31,$00
                DEFB      $01    ,$81,$8D,$A2,$11
                DEFB      $00    ,$90,$A6,$00
                DEFB  $13,$C1,$BD,$D0,$DE,$81,$BD
                DEFB  $14,$01    ,$01    ,$00
                DEFB  $15,$01    ,$A0,$DE,$A1,$BD
                DEFB      $00    ,$01    ,$00
                DEFB  $13,$C1,$8D,$84,$A5,$80,$BB
                DEFB      $01    ,$83,$7B,$90,$D2
                DEFB      $01    ,$82,$76,$A0,$EC
                DEFB      $00    ,$82,$52,$B1,$08
                DEFB  $13,$C1,$BD,$84,$A5,$C1,$29
                DEFB      $01    ,$83,$7B,$D1,$4D
                DEFB      $01    ,$82,$76,$E1,$76
                DEFB      $00    ,$82,$52,$F1,$A4
                DEFB  $13,$C1,$BD,$84,$A5,$F1,$D8
                DEFB      $01    ,$83,$7B,$E2,$11
                DEFB  $13,$01    ,$84,$A5,$D2,$52
                DEFB      $00    ,$83,$7B,$C2,$9B
                DEFB  $13,$C1,$BD,$84,$A5,$B2,$ED
                DEFB      $01    ,$83,$7B,$A3,$49
                DEFB  $13,$01    ,$84,$A5,$93,$B0
                DEFB      $00    ,$83,$7B,$84,$23
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT22:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$C1,$8D,$82,$ED,$84,$A5
                DEFB      $01    ,$82,$31,$84,$EC
                DEFB      $01    ,$81,$8D,$85,$37
                DEFB      $00    ,$90,$A6,$01
                DEFB  $02,$C1,$BD,$82,$ED,$01
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$00
                DEFB      $C1,$BD,$D0,$DE,$83,$7B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$DE,$00
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$C1,$BD,$82,$ED,$83,$7B
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$00
                DEFB      $00    ,$90,$A6,$01
                DEFB  $13,$C1,$8D,$84,$A5,$01
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB      $C1,$BD,$D0,$DE,$83,$7B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $13,$C1,$BD,$84,$A5,$83,$1A
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $13,$C1,$BD,$84,$A5,$83,$7B
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$00
                DEFB      $00    ,$82,$52,$01
                DEFB      $C1,$8D,$D0,$DE,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$C1,$BD,$82,$ED,$84,$A5
                DEFB      $01    ,$82,$31,$84,$EC
                DEFB  $02,$01    ,$82,$ED,$85,$37
                DEFB      $00    ,$82,$31,$01
                DEFB  $02,$C1,$BD,$82,$ED,$01
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $02,$C1,$BD,$82,$ED,$86,$34
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $13,$C1,$8D,$84,$A5,$86,$F6
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $02,$C1,$BD,$82,$ED,$86,$92
                DEFB      $01    ,$82,$31,$86,$F6
                DEFB      $01    ,$81,$8D,$87,$60
                DEFB      $00    ,$90,$A6,$86,$F6
                DEFB  $02,$C1,$BD,$82,$ED,$88,$47
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $13,$C1,$BD,$84,$A5,$89,$4B
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT23:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$C1,$8D,$82,$ED,$8A,$6E
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$00
                DEFB      $00    ,$90,$A6,$01
                DEFB  $02,$C1,$BD,$82,$ED,$8A,$6E
                DEFB      $01    ,$82,$31,$00
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $02,$C1,$BD,$82,$ED,$F2,$C3
                DEFB      $01    ,$82,$31,$F2,$ED
                DEFB      $01    ,$81,$8D,$F3,$1A
                DEFB      $00    ,$90,$A6,$01
                DEFB  $02,$C1,$BD,$82,$ED,$01
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $13,$C1,$8D,$84,$A5,$F3,$7B
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB      $C1,$BD,$D0,$DE,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$DE,$01
                DEFB      $00    ,$01    ,$F3,$B0
                DEFB  $13,$C1,$BD,$84,$A5,$F4,$A5
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $13,$C1,$BD,$84,$A5,$F5,$37
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$00
                DEFB      $00    ,$82,$52,$01
                DEFB      $C1,$8D,$D0,$DE,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$C1,$BD,$82,$ED,$F5,$37
                DEFB      $01    ,$82,$31,$01
                DEFB  $02,$01    ,$82,$ED,$01
                DEFB      $00    ,$82,$31,$01
                DEFB  $02,$C1,$BD,$82,$ED,$F6,$34
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $02,$C1,$BD,$82,$ED,$F4,$EC
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $13,$C1,$8D,$84,$A5,$F5,$37
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $02,$C1,$BD,$82,$ED,$F5,$86
                DEFB      $01    ,$82,$31,$F5,$37
                DEFB      $01    ,$81,$8D,$F4,$EC
                DEFB      $00    ,$90,$A6,$F5,$37
                DEFB  $02,$C2,$11,$82,$ED,$83,$E8
                DEFB      $01    ,$82,$31,$84,$23
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $13,$C1,$BD,$84,$A5,$83,$1A
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT24:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$C1,$8D,$82,$ED,$86,$F6
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$00
                DEFB      $00    ,$90,$A6,$01
                DEFB  $02,$C1,$BD,$82,$ED,$86,$F6
                DEFB      $01    ,$82,$31,$00
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $02,$C1,$BD,$82,$ED,$F2,$C3
                DEFB      $01    ,$82,$31,$F2,$ED
                DEFB      $01    ,$81,$8D,$F3,$1A
                DEFB      $00    ,$90,$A6,$01
                DEFB  $02,$C1,$BD,$82,$ED,$F4,$23
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $13,$C1,$8D,$84,$A5,$01
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB      $C1,$BD,$D0,$DE,$F2,$C3
                DEFB      $01    ,$01    ,$F2,$ED
                DEFB      $01    ,$A0,$DE,$F3,$1A
                DEFB      $00    ,$01    ,$01
                DEFB  $13,$C1,$BD,$84,$A5,$F3,$7B
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $13,$C1,$BD,$84,$A5,$00
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB      $83,$7B,$D0,$DE,$83,$7B
                DEFB      $C1,$8D,$01    ,$01
                DEFB      $83,$1A,$A0,$DE,$83,$1A
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$82,$9B,$82,$ED,$82,$9B
                DEFB      $C1,$BD,$82,$31,$01
                DEFB  $02,$83,$7B,$82,$ED,$83,$7B
                DEFB      $00    ,$82,$31,$01
                DEFB  $02,$83,$1A,$82,$ED,$83,$1A
                DEFB      $C1,$BD,$82,$31,$01
                DEFB      $82,$9B,$81,$8D,$82,$9B
                DEFB      $00    ,$90,$A6,$01
                DEFB  $02,$83,$7B,$82,$ED,$83,$7B
                DEFB      $C2,$11,$82,$31,$01
                DEFB      $83,$1A,$81,$8D,$83,$1A
                DEFB      $00    ,$90,$A6,$01
                DEFB  $13,$82,$9B,$84,$A5,$82,$9B
                DEFB      $C2,$11,$83,$7B,$01
                DEFB      $83,$7B,$82,$76,$83,$7B
                DEFB      $00    ,$82,$52,$01
                DEFB  $02,$83,$1A,$82,$ED,$83,$1A
                DEFB      $C2,$11,$82,$31,$01
                DEFB      $82,$9B,$81,$8D,$82,$9B
                DEFB      $00    ,$90,$A6,$01
                DEFB  $02,$81,$4D,$82,$ED,$85,$37
                DEFB      $C2,$76,$82,$31,$01
                DEFB  $02,$84,$A5,$82,$ED,$84,$EC
                DEFB      $00    ,$82,$31,$01
                DEFB  $13,$81,$4D,$84,$A5,$85,$37
                DEFB      $C2,$76,$83,$7B,$01
                DEFB      $86,$34,$82,$76,$86,$34
                DEFB      $00    ,$82,$52,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT25:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $13,$C1,$8D,$84,$A5,$86,$34
                DEFB      $01    ,$83,$7B,$86,$92
                DEFB      $01    ,$82,$76,$86,$F6
                DEFB      $00    ,$82,$52,$01
                DEFB  $13,$C1,$BD,$84,$A5,$01
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $12,$C1,$BD,$D0,$DE,$87,$60
                DEFB  $13,$01    ,$01    ,$86,$F6
                DEFB  $14,$01    ,$A0,$DE,$86,$92
                DEFB      $00    ,$01    ,$86,$F6
                DEFB  $02,$C1,$BD,$82,$ED,$87,$60
                DEFB      $01    ,$82,$31,$86,$F6
                DEFB      $01    ,$81,$8D,$86,$92
                DEFB      $00    ,$90,$A6,$86,$F6
                DEFB  $13,$C1,$8D,$84,$A5,$88,$47
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $13,$C1,$BD,$84,$A5,$01
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $02,$C1,$BD,$82,$ED,$89,$4B
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB      $C1,$BD,$D0,$DE,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $13,$C1,$8D,$84,$A5,$01
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$88,$C6
                DEFB  $13,$C1,$BD,$84,$A5,$88,$47
                DEFB      $01    ,$83,$7B,$87,$D0
                DEFB      $01    ,$82,$76,$87,$60
                DEFB      $00    ,$82,$52,$86,$F6
                DEFB  $12,$C1,$BD,$D0,$DE,$8A,$6E
                DEFB  $13,$01    ,$01    ,$01
                DEFB  $14,$01    ,$A0,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$C1,$BD,$82,$ED,$01
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $13,$C1,$8D,$84,$A5,$8B,$0D
                DEFB      $01    ,$83,$7B,$8A,$6E
                DEFB      $01    ,$82,$76,$89,$D9
                DEFB      $00    ,$82,$52,$8A,$6E
                DEFB  $13,$C1,$BD,$84,$A5,$8B,$0D
                DEFB      $01    ,$83,$7B,$8A,$6E
                DEFB      $01    ,$82,$76,$89,$D9
                DEFB      $00    ,$82,$52,$8A,$6E
                DEFB  $13,$C1,$BD,$84,$A5,$8B,$0D
                DEFB      $01    ,$83,$7B,$8A,$6E
                DEFB  $13,$01    ,$84,$A5,$89,$D9
                DEFB      $00    ,$83,$7B,$8A,$6E
                DEFB  $13,$C1,$BD,$84,$A5,$8B,$0D
                DEFB      $01    ,$83,$7B,$8A,$6E
                DEFB  $13,$01    ,$84,$A5,$89,$D9
                DEFB      $00    ,$83,$7B,$8A,$6E
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT26:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$C1,$8D,$84,$A5,$8C,$68
                DEFB      $01    ,$83,$7B,$8D,$25
                DEFB      $01    ,$82,$76,$8D,$ED
                DEFB      $00    ,$82,$52,$00
                DEFB  $02,$C1,$BD,$84,$A5,$8D,$ED
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$00
                DEFB      $00    ,$82,$52,$01
                DEFB  $02,$C1,$BD,$D0,$DE,$F3,$1A
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$C1,$BD,$82,$ED,$F3,$7B
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $13,$C1,$8D,$84,$A5,$F4,$23
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB      $C1,$BD,$84,$A5,$F4,$63
                DEFB      $01    ,$83,$7B,$F4,$23
                DEFB      $01    ,$82,$76,$F3,$E8
                DEFB      $00    ,$82,$52,$F4,$23
                DEFB  $13,$C1,$BD,$82,$ED,$F3,$7B
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $13,$C1,$BD,$D0,$DE,$F3,$1A
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $C1,$8D,$84,$A5,$F4,$A5
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $02,$C1,$BD,$84,$A5,$F4,$EC
                DEFB      $01    ,$83,$7B,$F4,$A5
                DEFB  $02,$01    ,$82,$76,$F4,$63
                DEFB      $00    ,$82,$52,$F4,$A5
                DEFB  $02,$C1,$BD,$D0,$DE,$F3,$7B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$C1,$BD,$82,$ED,$F3,$1A
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $13,$C1,$8D,$84,$A5,$F5,$37
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $02,$C1,$BD,$84,$A5,$F5,$86
                DEFB      $01    ,$83,$7B,$F5,$37
                DEFB      $01    ,$82,$76,$F4,$EC
                DEFB      $00    ,$82,$52,$F5,$37
                DEFB  $02,$C1,$BD,$84,$A5,$F4,$23
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$84,$A5,$01
                DEFB      $00    ,$83,$7B,$01
                DEFB  $13,$C1,$BD,$84,$A5,$F4,$A5
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$84,$A5,$01
                DEFB      $00    ,$83,$7B,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT27:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $13,$C1,$8D,$84,$A5,$F3,$1A
                DEFB      $01    ,$83,$7B,$F3,$49
                DEFB      $01    ,$82,$76,$F3,$7B
                DEFB      $00    ,$82,$52,$01
                DEFB  $13,$C1,$BD,$84,$A5,$01
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $12,$C1,$BD,$D0,$DE,$01
                DEFB  $13,$01    ,$01    ,$01
                DEFB  $14,$01    ,$A0,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$C1,$BD,$82,$ED,$F3,$B0
                DEFB      $01    ,$82,$31,$F3,$7B
                DEFB      $01    ,$81,$8D,$F3,$49
                DEFB      $00    ,$90,$A6,$F3,$7B
                DEFB  $13,$C1,$8D,$84,$A5,$F3,$1A
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $13,$C1,$BD,$84,$A5,$01
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $02,$C1,$BD,$82,$ED,$F2,$9B
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB      $01    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $13,$C1,$8D,$84,$A5,$F2,$9B
                DEFB      $01    ,$83,$7B,$01
                DEFB      $00    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB  $13,$C1,$BD,$84,$A5,$F2,$9B
                DEFB      $01    ,$83,$7B,$01
                DEFB      $00    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB  $12,$C1,$BD,$D0,$DE,$F2,$52
                DEFB  $13,$01    ,$01    ,$01
                DEFB  $14,$01    ,$A0,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$C1,$BD,$82,$ED,$F2,$76
                DEFB      $01    ,$82,$31,$F2,$52
                DEFB      $01    ,$81,$8D,$F2,$31
                DEFB      $00    ,$90,$A6,$F2,$52
                DEFB  $13,$C1,$8D,$84,$A5,$F2,$11
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $13,$C1,$BD,$84,$A5,$00
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $13,$C1,$BD,$84,$A5,$F1,$8D
                DEFB      $01    ,$83,$7B,$01
                DEFB  $13,$01    ,$84,$A5,$01
                DEFB      $00    ,$83,$7B,$01
                DEFB  $13,$C1,$BD,$84,$A5,$01
                DEFB      $01    ,$83,$7B,$01
                DEFB  $13,$01    ,$84,$A5,$01
                DEFB      $00    ,$83,$7B,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT28:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$C1,$8D,$84,$A5,$F1,$A4
                DEFB      $01    ,$83,$7B,$F1,$BD
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $02,$C1,$BD,$84,$A5,$01
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $02,$C1,$BD,$D0,$DE,$F1,$A4
                DEFB      $01    ,$01    ,$F1,$8D
                DEFB      $01    ,$A0,$DE,$F1,$76
                DEFB      $00    ,$01    ,$F1,$61
                DEFB  $02,$C1,$BD,$82,$ED,$F1,$A4
                DEFB      $01    ,$82,$31,$F1,$BD
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $13,$C1,$8D,$84,$A5,$01
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB      $C1,$BD,$84,$A5,$F1,$A4
                DEFB      $01    ,$83,$7B,$F1,$8D
                DEFB      $01    ,$82,$76,$F1,$76
                DEFB      $00    ,$82,$52,$F1,$61
                DEFB  $13,$C1,$BD,$82,$ED,$F1,$A4
                DEFB      $01    ,$82,$31,$F1,$BD
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $13,$C1,$BD,$D0,$DE,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $C1,$8D,$84,$A5,$F1,$8D
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $02,$C1,$BD,$84,$A5,$F1,$BD
                DEFB      $01    ,$83,$7B,$01
                DEFB  $02,$01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $02,$C1,$BD,$D0,$DE,$F2,$11
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$DE,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$C1,$BD,$82,$ED,$F2,$52
                DEFB      $01    ,$82,$31,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $13,$C1,$8D,$84,$A5,$F1,$8D
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $02,$C1,$BD,$84,$A5,$F1,$BD
                DEFB      $01    ,$83,$7B,$01
                DEFB      $01    ,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $13,$C1,$BD,$84,$A5,$F2,$52
                DEFB      $01    ,$83,$7B,$01
                DEFB  $13,$01    ,$84,$A5,$01
                DEFB      $00    ,$83,$7B,$01
                DEFB  $13,$C1,$BD,$84,$A5,$F2,$9B
                DEFB      $01    ,$83,$7B,$01
                DEFB  $13,$01    ,$84,$A5,$01
                DEFB      $00    ,$83,$7B,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT29:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$8D,$82,$ED,$F3,$1A
                DEFB      $01    ,$82,$31,$F3,$49
                DEFB      $F1,$BD,$81,$8D,$F3,$7B
                DEFB      $01    ,$90,$A6,$01
                DEFB      $81,$BD,$D1,$BD,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$BD,$A0,$DE,$F3,$49
                DEFB      $01    ,$01    ,$F3,$1A
                DEFB      $81,$BD,$01    ,$F2,$ED
                DEFB      $01    ,$01    ,$F2,$C3
                DEFB      $F1,$BD,$01    ,$F2,$9B
                DEFB      $01    ,$01    ,$F2,$76
                DEFB      $81,$BD,$01    ,$F2,$52
                DEFB      $01    ,$01    ,$F2,$31
                DEFB      $F1,$BD,$01    ,$F2,$11
                DEFB      $01    ,$01    ,$F1,$F4
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT30:
                DEFW  2464     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $F3,$7B,$A0,$DE,$F3,$7B
                DEFB      $F1,$BD,$01    ,$00
                DEFB      $81,$BD,$01    ,$01
                DEFB      $F1,$BD,$01    ,$01
                DEFB      $F3,$7B,$01    ,$01
                DEFB      $F1,$BD,$01    ,$01
                DEFB      $81,$BD,$01    ,$01
                DEFB      $F1,$BD,$01    ,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT31:
                DEFW  2464     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $F1,$BD,$A0,$DE,$F1,$BD
                DEFB      $F1,$BD,$01    ,$00
                DEFB      $81,$BD,$01    ,$01
                DEFB      $F1,$BD,$01    ,$01
                DEFB      $F1,$BD,$01    ,$01
                DEFB      $F1,$BD,$01    ,$01
                DEFB      $81,$BD,$01    ,$01
                DEFB      $F1,$BD,$01    ,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT32:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$F1,$BD,$84,$A5,$F1,$BD
                DEFB      $01    ,$83,$7B,$01
                DEFB      $F1,$BD,$82,$76,$00
                DEFB      $01    ,$82,$52,$01
                DEFB  $02,$81,$BD,$84,$A5,$01
                DEFB      $01    ,$83,$7B,$01
                DEFB      $F1,$BD,$82,$76,$01
                DEFB      $01    ,$82,$52,$01
                DEFB  $13,$F1,$BD,$82,$ED,$01
                DEFB      $01    ,$82,$31,$01
                DEFB  $13,$F1,$BD,$82,$ED,$01
                DEFB      $01    ,$82,$31,$01
                DEFB  $13,$81,$BD,$82,$ED,$01
                DEFB      $01    ,$82,$31,$01
                DEFB  $13,$F1,$BD,$82,$ED,$01
                DEFB      $01    ,$82,$31,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT33:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$F1,$8D,$82,$ED,$F1,$8D
                DEFB      $01    ,$82,$31,$01
                DEFB      $F1,$8D,$81,$8D,$00
                DEFB      $01    ,$90,$A6,$01
                DEFB      $81,$8D,$D0,$C6,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$8D,$A0,$C6,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$8D,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$8D,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $81,$8D,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F1,$8D,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT34:
                DEFW  2464     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $F1,$8D,$A0,$C6,$F1,$8D
                DEFB      $F1,$8D,$01    ,$00
                DEFB      $81,$8D,$01    ,$01
                DEFB      $F1,$8D,$01    ,$01
                DEFB      $F1,$8D,$01    ,$01
                DEFB      $F1,$8D,$01    ,$01
                DEFB      $81,$8D,$01    ,$01
                DEFB      $F1,$8D,$01    ,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT35:
                DEFW  2464     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $F3,$1A,$A0,$C6,$F3,$1A
                DEFB      $F1,$8D,$01    ,$00
                DEFB      $81,$8D,$01    ,$01
                DEFB      $F1,$8D,$01    ,$01
                DEFB      $F3,$1A,$01    ,$01
                DEFB      $F1,$8D,$01    ,$01
                DEFB      $81,$8D,$01    ,$01
                DEFB      $F1,$8D,$01    ,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT36:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$F3,$1A,$82,$ED,$F3,$1A
                DEFB      $01    ,$82,$31,$01
                DEFB      $F1,$8D,$81,$8D,$80,$BB
                DEFB      $01    ,$90,$A6,$90,$D2
                DEFB      $81,$8D,$D0,$C6,$A0,$EC
                DEFB      $01    ,$01    ,$B1,$08
                DEFB      $F1,$8D,$A0,$C6,$C1,$29
                DEFB      $01    ,$01    ,$D1,$4D
                DEFB  $02,$F3,$1A,$82,$ED,$E1,$76
                DEFB      $01    ,$82,$31,$F1,$A4
                DEFB      $F1,$8D,$81,$8D,$F1,$D8
                DEFB      $01    ,$90,$A6,$E2,$11
                DEFB  $02,$81,$8D,$82,$ED,$D2,$52
                DEFB      $01    ,$82,$31,$C2,$9B
                DEFB  $13,$F1,$8D,$84,$A5,$B2,$ED
                DEFB      $01    ,$83,$7B,$A3,$49
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT37:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$F4,$82,$ED,$C2,$C3
                DEFB      $81,$76,$82,$31,$C2,$ED
                DEFB      $81,$08,$81,$8D,$C3,$1A
                DEFB      $90,$6F,$90,$A6,$01
                DEFB      $81,$8D,$D0,$C6,$01
                DEFB      $91,$8D,$01    ,$01
                DEFB      $A1,$8D,$A0,$C6,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$81,$8D,$82,$ED,$C2,$ED
                DEFB      $91,$8D,$82,$31,$C3,$1A
                DEFB      $A1,$8D,$81,$8D,$C3,$49
                DEFB      $00    ,$90,$A6,$C3,$1A
                DEFB  $02,$81,$8D,$82,$ED,$C2,$ED
                DEFB      $91,$8D,$82,$31,$C3,$1A
                DEFB      $A1,$8D,$81,$8D,$C3,$49
                DEFB      $00    ,$90,$A6,$C3,$1A
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT38:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $13,$81,$61,$84,$A5,$C3,$49
                DEFB      $91,$61,$83,$7B,$C3,$7B
                DEFB      $A1,$61,$82,$76,$C3,$B0
                DEFB      $00    ,$82,$52,$01
                DEFB      $81,$8D,$D0,$C6,$C4,$23
                DEFB      $91,$8D,$01    ,$01
                DEFB      $A1,$8D,$A0,$C6,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$81,$8D,$82,$ED,$01
                DEFB      $91,$8D,$82,$31,$01
                DEFB      $A1,$8D,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB      $81,$8D,$D0,$C6,$C4,$63
                DEFB      $91,$8D,$01    ,$C4,$A5
                DEFB      $A1,$8D,$A0,$C6,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT39:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$61,$82,$ED,$C4,$A5
                DEFB      $91,$61,$82,$31,$01
                DEFB      $A1,$61,$81,$8D,$00
                DEFB      $00    ,$90,$A6,$01
                DEFB  $02,$81,$8D,$82,$ED,$C3,$1A
                DEFB      $91,$8D,$82,$31,$01
                DEFB      $A1,$8D,$81,$8D,$00
                DEFB      $00    ,$90,$A6,$01
                DEFB      $81,$8D,$D0,$C6,$C3,$1A
                DEFB      $91,$8D,$01    ,$01
                DEFB      $A1,$8D,$A0,$C6,$00
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$81,$8D,$82,$ED,$C2,$C3
                DEFB      $91,$8D,$82,$31,$01
                DEFB      $A1,$8D,$81,$8D,$00
                DEFB      $00    ,$90,$A6,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT40:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $13,$81,$61,$84,$A5,$C3,$49
                DEFB      $91,$61,$83,$7B,$C3,$7B
                DEFB      $A1,$61,$82,$76,$C3,$B0
                DEFB      $00    ,$82,$52,$00
                DEFB      $81,$8D,$D0,$C6,$C3,$B0
                DEFB      $91,$8D,$01    ,$01
                DEFB      $A1,$8D,$A0,$C6,$00
                DEFB      $00    ,$01    ,$01
                DEFB  $13,$81,$8D,$84,$A5,$C2,$C3
                DEFB      $91,$8D,$83,$7B,$01
                DEFB  $13,$A1,$8D,$84,$A5,$00
                DEFB      $00    ,$83,$7B,$01
                DEFB  $13,$81,$8D,$84,$A5,$C2,$9B
                DEFB      $91,$8D,$83,$7B,$C2,$C3
                DEFB  $13,$A1,$8D,$84,$A5,$01
                DEFB      $00    ,$83,$7B,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT41:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$F4,$82,$ED,$F3,$1A
                DEFB      $81,$76,$82,$31,$F3,$B0
                DEFB      $81,$08,$81,$8D,$E4,$A5
                DEFB      $90,$6F,$90,$A6,$E6,$34
                DEFB      $81,$8D,$D0,$C6,$D3,$1A
                DEFB      $91,$8D,$01    ,$D3,$7B
                DEFB      $A1,$8D,$A0,$C6,$C4,$A5
                DEFB      $00    ,$01    ,$C6,$34
                DEFB  $02,$81,$8D,$82,$ED,$B3,$1A
                DEFB      $91,$8D,$82,$31,$B3,$B0
                DEFB      $A1,$8D,$81,$8D,$A4,$A5
                DEFB      $00    ,$90,$A6,$A6,$34
                DEFB  $02,$81,$8D,$82,$ED,$93,$1A
                DEFB      $91,$8D,$82,$31,$93,$7B
                DEFB  $02,$A1,$8D,$82,$ED,$84,$A5
                DEFB      $00    ,$82,$31,$86,$34
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT42:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $13,$81,$61,$84,$A5,$83,$1A
                DEFB      $91,$61,$83,$7B,$83,$B0
                DEFB      $A1,$61,$82,$76,$94,$A5
                DEFB      $00    ,$82,$52,$96,$34
                DEFB      $81,$8D,$D0,$C6,$A3,$1A
                DEFB      $91,$8D,$01    ,$A3,$7B
                DEFB      $A1,$8D,$A0,$C6,$B4,$A5
                DEFB      $00    ,$01    ,$B6,$34
                DEFB  $02,$81,$8D,$82,$ED,$C3,$1A
                DEFB      $91,$8D,$82,$31,$C3,$B0
                DEFB  $02,$A1,$8D,$82,$ED,$D4,$A5
                DEFB      $00    ,$82,$31,$D6,$34
                DEFB  $13,$81,$8D,$84,$A5,$E3,$1A
                DEFB      $91,$8D,$83,$7B,$E3,$7B
                DEFB      $A1,$8D,$82,$76,$F4,$A5
                DEFB      $00    ,$82,$52,$F6,$34
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT43:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$F4,$82,$ED,$C3,$B0
                DEFB      $81,$76,$82,$31,$C3,$E8
                DEFB      $81,$08,$81,$8D,$C4,$23
                DEFB      $90,$6F,$90,$A6,$01
                DEFB      $82,$11,$D1,$08,$01
                DEFB      $92,$11,$01    ,$01
                DEFB      $A2,$11,$A1,$08,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$82,$11,$82,$ED,$C3,$E8
                DEFB      $92,$11,$82,$31,$C4,$23
                DEFB      $A2,$11,$81,$8D,$C4,$63
                DEFB      $00    ,$90,$A6,$C4,$23
                DEFB  $02,$82,$11,$82,$ED,$C3,$E8
                DEFB      $92,$11,$82,$31,$C4,$23
                DEFB      $A2,$11,$81,$8D,$C4,$63
                DEFB      $00    ,$90,$A6,$C4,$23
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT44:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $13,$81,$D8,$84,$A5,$C4,$63
                DEFB      $91,$D8,$83,$7B,$C4,$A5
                DEFB      $A1,$D8,$82,$76,$C4,$EC
                DEFB      $00    ,$82,$52,$01
                DEFB      $82,$11,$D1,$08,$C5,$86
                DEFB      $92,$11,$01    ,$01
                DEFB      $A2,$11,$A1,$08,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$82,$11,$82,$ED,$01
                DEFB      $92,$11,$82,$31,$01
                DEFB      $A2,$11,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB      $82,$11,$D1,$08,$C5,$DB
                DEFB      $92,$11,$01    ,$C6,$34
                DEFB      $A2,$11,$A1,$08,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT45:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$D8,$82,$ED,$C6,$34
                DEFB      $91,$D8,$82,$31,$01
                DEFB      $A1,$D8,$81,$8D,$00
                DEFB      $00    ,$90,$A6,$01
                DEFB  $02,$82,$11,$82,$ED,$C4,$23
                DEFB      $92,$11,$82,$31,$01
                DEFB      $A2,$11,$81,$8D,$00
                DEFB      $00    ,$90,$A6,$01
                DEFB      $82,$11,$D1,$08,$C4,$23
                DEFB      $92,$11,$01    ,$01
                DEFB      $A2,$11,$A1,$08,$00
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$82,$11,$82,$ED,$C3,$B0
                DEFB      $92,$11,$82,$31,$01
                DEFB      $A2,$11,$81,$8D,$00
                DEFB      $00    ,$90,$A6,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT46:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $13,$81,$D8,$84,$A5,$C4,$63
                DEFB      $91,$D8,$83,$7B,$C4,$A5
                DEFB      $A1,$D8,$82,$76,$C4,$EC
                DEFB      $00    ,$82,$52,$00
                DEFB      $82,$11,$D1,$08,$C4,$EC
                DEFB      $92,$11,$01    ,$01
                DEFB      $A2,$11,$A1,$08,$00
                DEFB      $00    ,$01    ,$01
                DEFB  $13,$82,$11,$84,$A5,$C3,$B0
                DEFB      $92,$11,$83,$7B,$01
                DEFB  $13,$A2,$11,$84,$A5,$00
                DEFB      $00    ,$83,$7B,$01
                DEFB  $13,$82,$11,$84,$A5,$C3,$7B
                DEFB      $92,$11,$83,$7B,$C3,$B0
                DEFB  $13,$A2,$11,$84,$A5,$01
                DEFB      $00    ,$83,$7B,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT47:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$F4,$82,$ED,$F6,$34
                DEFB      $81,$76,$82,$31,$F7,$60
                DEFB      $81,$08,$81,$8D,$E9,$4B
                DEFB      $90,$6F,$90,$A6,$EC,$68
                DEFB      $81,$8D,$D0,$C6,$D6,$34
                DEFB      $91,$8D,$01    ,$D6,$F6
                DEFB      $A1,$8D,$A0,$C6,$C9,$4B
                DEFB      $00    ,$01    ,$CC,$68
                DEFB  $02,$81,$8D,$82,$ED,$B6,$34
                DEFB      $91,$8D,$82,$31,$B7,$60
                DEFB      $A1,$8D,$81,$8D,$A9,$4B
                DEFB      $00    ,$90,$A6,$AC,$68
                DEFB  $02,$81,$8D,$82,$ED,$96,$34
                DEFB      $91,$8D,$82,$31,$96,$F6
                DEFB  $02,$A1,$8D,$82,$ED,$89,$4B
                DEFB      $00    ,$82,$31,$8C,$68
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT48:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $13,$81,$61,$84,$A5,$86,$34
                DEFB      $91,$61,$83,$7B,$87,$60
                DEFB      $A1,$61,$82,$76,$99,$4B
                DEFB      $00    ,$82,$52,$9C,$68
                DEFB      $81,$8D,$D0,$C6,$A6,$34
                DEFB      $91,$8D,$01    ,$A6,$F6
                DEFB      $A1,$8D,$A0,$C6,$B9,$4B
                DEFB      $00    ,$01    ,$BC,$68
                DEFB  $02,$81,$8D,$82,$ED,$C6,$34
                DEFB      $91,$8D,$82,$31,$C7,$60
                DEFB  $02,$A1,$8D,$82,$ED,$D9,$4B
                DEFB      $00    ,$82,$31,$DC,$68
                DEFB  $13,$81,$8D,$84,$A5,$E6,$34
                DEFB      $91,$8D,$83,$7B,$E6,$F6
                DEFB      $A1,$8D,$82,$76,$F9,$4B
                DEFB      $00    ,$82,$52,$FC,$68
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT49:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $13,$81,$61,$84,$A5,$83,$1A
                DEFB      $91,$61,$83,$7B,$83,$B0
                DEFB      $A1,$61,$82,$76,$94,$A5
                DEFB      $00    ,$82,$52,$96,$34
                DEFB      $81,$8D,$D0,$C6,$A3,$1A
                DEFB      $91,$8D,$01    ,$A3,$7B
                DEFB      $A1,$8D,$A0,$C6,$B4,$A5
                DEFB      $00    ,$01    ,$B6,$34
                DEFB  $02,$81,$8D,$82,$ED,$C3,$1A
                DEFB      $00    ,$82,$31,$00
                DEFB  $02,$A1,$8D,$82,$ED,$D4,$A5
                DEFB      $00    ,$82,$31,$00
                DEFB  $02,$81,$8D,$82,$ED,$E3,$1A
                DEFB      $00    ,$82,$31,$00
                DEFB  $02,$A1,$8D,$82,$ED,$F4,$A5
                DEFB      $00    ,$82,$31,$00
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT50:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$82,$11,$82,$ED,$84,$23
                DEFB      $92,$11,$82,$31,$84,$63
                DEFB      $A2,$11,$81,$8D,$84,$A5
                DEFB      $00    ,$90,$A6,$01
                DEFB  $02,$82,$52,$82,$ED,$01
                DEFB      $92,$52,$82,$31,$01
                DEFB      $A2,$52,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB      $82,$52,$D1,$29,$84,$EC
                DEFB      $92,$52,$01    ,$84,$A5
                DEFB      $A2,$52,$A1,$29,$84,$63
                DEFB      $00    ,$01    ,$84,$A5
                DEFB  $02,$82,$52,$82,$ED,$84,$EC
                DEFB      $92,$52,$82,$31,$84,$A5
                DEFB      $A2,$52,$81,$8D,$84,$63
                DEFB      $00    ,$90,$A6,$84,$A5
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT51:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $13,$82,$11,$84,$A5,$85,$86
                DEFB      $92,$11,$83,$7B,$01
                DEFB      $A2,$11,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB      $82,$52,$D1,$29,$01
                DEFB      $92,$52,$01    ,$01
                DEFB      $A2,$52,$A1,$29,$01
                DEFB      $00    ,$01    ,$85,$DB
                DEFB  $02,$82,$52,$82,$ED,$86,$34
                DEFB      $92,$52,$82,$31,$01
                DEFB      $A2,$52,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB      $82,$52,$D1,$29,$01
                DEFB      $92,$52,$01    ,$01
                DEFB      $A2,$52,$A1,$29,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT52:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$82,$11,$82,$ED,$86,$34
                DEFB      $92,$11,$82,$31,$01
                DEFB      $A2,$11,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $02,$82,$52,$82,$ED,$85,$DB
                DEFB      $92,$52,$82,$31,$85,$86
                DEFB  $02,$A2,$52,$82,$ED,$85,$37
                DEFB      $00    ,$82,$31,$84,$EC
                DEFB      $82,$52,$D1,$29,$84,$A5
                DEFB      $92,$52,$01    ,$01
                DEFB      $A2,$52,$A1,$29,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$82,$52,$82,$ED,$01
                DEFB      $92,$52,$82,$31,$01
                DEFB      $A2,$52,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT53:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $13,$82,$11,$84,$A5,$85,$86
                DEFB      $92,$11,$83,$7B,$01
                DEFB      $A2,$11,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB      $82,$52,$D1,$29,$01
                DEFB      $92,$52,$01    ,$01
                DEFB      $A2,$52,$A1,$29,$01
                DEFB      $00    ,$01    ,$85,$DB
                DEFB  $02,$82,$52,$82,$ED,$86,$34
                DEFB      $92,$52,$82,$31,$01
                DEFB      $A2,$52,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $13,$82,$52,$84,$A5,$01
                DEFB      $92,$52,$83,$7B,$01
                DEFB      $A2,$52,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT54:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$D8,$82,$ED,$83,$B0
                DEFB      $91,$D8,$82,$31,$83,$E8
                DEFB      $A1,$D8,$81,$8D,$84,$23
                DEFB      $00    ,$90,$A6,$01
                DEFB  $02,$82,$11,$82,$ED,$01
                DEFB      $92,$11,$82,$31,$01
                DEFB      $A2,$11,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB      $82,$11,$D1,$08,$84,$63
                DEFB      $92,$11,$01    ,$84,$23
                DEFB      $A2,$11,$A1,$08,$83,$E8
                DEFB      $00    ,$01    ,$84,$23
                DEFB  $02,$82,$11,$82,$ED,$84,$63
                DEFB      $92,$11,$82,$31,$84,$23
                DEFB      $A2,$11,$81,$8D,$83,$E8
                DEFB      $00    ,$90,$A6,$84,$23
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT55:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $13,$81,$D8,$84,$A5,$84,$EC
                DEFB      $91,$D8,$83,$7B,$01
                DEFB      $A1,$D8,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB      $82,$11,$D1,$08,$01
                DEFB      $92,$11,$01    ,$01
                DEFB      $A2,$11,$A1,$08,$01
                DEFB      $00    ,$01    ,$85,$37
                DEFB  $02,$82,$11,$82,$ED,$85,$86
                DEFB      $92,$11,$82,$31,$01
                DEFB  $02,$A2,$11,$81,$8D,$01
                DEFB  $03,$00    ,$90,$A6,$01
                DEFB  $04,$82,$11,$D1,$08,$01
                DEFB  $05,$92,$11,$01    ,$01
                DEFB      $A2,$11,$A1,$08,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT56:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$D8,$82,$ED,$85,$86
                DEFB      $91,$D8,$82,$31,$01
                DEFB      $A1,$D8,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $02,$82,$11,$82,$ED,$85,$37
                DEFB      $92,$11,$82,$31,$84,$EC
                DEFB  $02,$A2,$11,$82,$ED,$84,$A5
                DEFB      $00    ,$82,$31,$84,$63
                DEFB      $82,$11,$D1,$08,$84,$23
                DEFB      $92,$11,$01    ,$01
                DEFB      $A2,$11,$A1,$08,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $02,$82,$11,$82,$ED,$01
                DEFB      $92,$11,$82,$31,$01
                DEFB      $A2,$11,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT57:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $13,$81,$D8,$84,$A5,$84,$EC
                DEFB      $91,$D8,$83,$7B,$01
                DEFB      $A1,$D8,$82,$76,$01
                DEFB      $00    ,$82,$52,$01
                DEFB      $82,$11,$D1,$08,$01
                DEFB      $92,$11,$01    ,$01
                DEFB      $A2,$11,$A1,$08,$01
                DEFB      $00    ,$01    ,$85,$37
                DEFB  $02,$82,$11,$82,$ED,$85,$86
                DEFB      $92,$11,$82,$31,$01
                DEFB      $A2,$11,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $02,$82,$11,$82,$ED,$01
                DEFB      $92,$11,$82,$31,$01
                DEFB      $A2,$11,$81,$8D,$01
                DEFB      $00    ,$90,$A6,$01
                DEFB  $FF  ; End of INVASIONPATtern

INVASIONPAT58:
                DEFW  1236     ; INVASIONPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$F3,$7B,$F3,$7B,$01
                DEFB      $F3,$49,$F3,$49,$01
                DEFB      $E3,$1A,$E3,$1A,$01
                DEFB      $E2,$ED,$E2,$ED,$01
                DEFB      $D2,$C3,$D2,$C3,$01
                DEFB      $D2,$9B,$D2,$9B,$01
                DEFB      $C2,$76,$00    ,$01
                DEFB      $C2,$52,$01    ,$01
                DEFB      $F3,$49,$01    ,$01
                DEFB      $F3,$1A,$01    ,$01
                DEFB      $E2,$ED,$01    ,$01
                DEFB      $E2,$C3,$01    ,$01
                DEFB      $D2,$9B,$01    ,$01
                DEFB      $D2,$76,$01    ,$01
                DEFB      $C2,$52,$01    ,$01
                DEFB      $C2,$31,$01    ,$01
                DEFB      $F3,$1A,$F3,$1A,$01
                DEFB      $F2,$ED,$F2,$ED,$01
                DEFB      $E2,$C3,$E2,$C3,$01
                DEFB      $E2,$9B,$E2,$9B,$01
                DEFB      $D2,$76,$D2,$76,$01
                DEFB      $D2,$52,$D2,$52,$01
                DEFB      $C2,$31,$00    ,$01
                DEFB      $C2,$11,$01    ,$01
                DEFB      $F2,$ED,$01    ,$01
                DEFB      $F2,$C3,$01    ,$01
                DEFB      $E2,$9B,$01    ,$01
                DEFB      $E2,$76,$01    ,$01
                DEFB      $D2,$52,$01    ,$01
                DEFB      $D2,$31,$01    ,$01
                DEFB      $C2,$11,$01    ,$01
                DEFB      $C1,$F4,$01    ,$01
                DEFB      $F2,$C3,$01    ,$01
                DEFB      $F2,$9B,$01    ,$01
                DEFB      $E2,$76,$01    ,$01
                DEFB      $E2,$52,$01    ,$01
                DEFB      $D2,$31,$01    ,$01
                DEFB      $D2,$11,$01    ,$01
                DEFB      $C1,$F4,$01    ,$01
                DEFB      $C1,$D8,$01    ,$01
                DEFB      $F2,$9B,$01    ,$01
                DEFB      $F2,$76,$01    ,$01
                DEFB      $E2,$52,$01    ,$01
                DEFB      $E2,$31,$01    ,$01
                DEFB      $D2,$11,$01    ,$01
                DEFB      $D1,$F4,$01    ,$01
                DEFB      $C1,$D8,$01    ,$01
                DEFB      $C1,$BD,$01    ,$01
                DEFB      $F2,$76,$F0,$BB,$01
                DEFB      $F2,$52,$F0,$C6,$01
                DEFB      $E2,$31,$E0,$D2,$01
                DEFB      $E2,$11,$E0,$DE,$01
                DEFB      $D1,$F4,$D0,$EC,$01
                DEFB      $D1,$D8,$D0,$FA,$F0,$5D
                DEFB      $C1,$BD,$C1,$08,$F0,$63
                DEFB      $C1,$A4,$C1,$18,$E0,$69
                DEFB      $F2,$52,$B1,$29,$E0,$6F
                DEFB      $F2,$31,$B1,$3B,$D0,$76
                DEFB      $E2,$11,$A1,$4D,$D0,$7D
                DEFB      $E1,$F4,$A1,$61,$C0,$84
                DEFB      $D1,$D8,$91,$76,$C0,$8C
                DEFB      $D1,$BD,$91,$8D,$B0,$94
                DEFB      $C1,$A4,$81,$A4,$B0,$9D
                DEFB      $C1,$8D,$81,$BD,$A0,$A6
                DEFB  $FF  ; End of INVASIONPATtern

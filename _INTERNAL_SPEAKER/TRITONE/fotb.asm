
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

; =======================================
FOTB:			; flight of the bumblebee
SONG4:
; =======================================


; *** Song layout ***
FOTBLOOPSTART:            DEFW      FOTBPAT0
                      DEFW      FOTBPAT1
                      DEFW      FOTBPAT2
                      DEFW      FOTBPAT3
                      DEFW      FOTBPAT4
                      DEFW      FOTBPAT5
                      DEFW      FOTBPAT6
                      DEFW      FOTBPAT7
                      DEFW      FOTBPAT6
                      DEFW      FOTBPAT7
                      DEFW      FOTBPAT8
                      DEFW      FOTBPAT9
                      DEFW      FOTBPAT10
                      DEFW      FOTBPAT11
                      DEFW      FOTBPAT12
                      DEFW      FOTBPAT13
                      DEFW      FOTBPAT14
                      DEFW      FOTBPAT15
                      DEFW      FOTBPAT16
                      DEFW      FOTBPAT17
                      DEFW      FOTBPAT16
                      DEFW      FOTBPAT17
                      DEFW      FOTBPAT18
                      DEFW      FOTBPAT19
                      DEFW      FOTBPAT20
                      DEFW      FOTBPAT19
                      DEFW      FOTBPAT21
                      DEFW      FOTBPAT22
                      DEFW      FOTBPAT23
                      DEFW      FOTBPAT24
                      DEFW      FOTBPAT25
                      DEFW      FOTBPAT26
                      DEFW      FOTBPAT27
                      DEFW      FOTBPAT28
                      DEFW      FOTBPAT29
                      DEFW      FOTBPAT30
                      DEFW      FOTBPAT31
                      DEFW      FOTBPAT32
                      DEFW      FOTBPAT33
                      DEFW      FOTBPAT34
                      DEFW      FOTBPAT35
                      DEFW      FOTBPAT36
                      DEFW      FOTBPAT37
                      DEFW      FOTBPAT38
                      DEFW      FOTBPAT39
                      DEFW      FOTBPAT40
                      DEFW      FOTBPAT41
                      DEFW      FOTBPAT42
                      DEFW      FOTBPAT43
                      DEFW      FOTBPAT44
                      DEFW      FOTBPAT45
                      DEFW      FOTBPAT46
                      DEFW      FOTBPAT47
                      DEFW      FOTBPAT48
                      DEFW      FOTBPAT49
                      DEFW      FOTBPAT50
                      DEFW      FOTBPAT51
                      DEFW      FOTBPAT52
                      DEFW      FOTBPAT53
                      DEFW      FOTBPAT54
                      DEFW      FOTBPAT53
                      DEFW      FOTBPAT54
                      DEFW      FOTBPAT55
                      DEFW      FOTBPAT56
                      DEFW      FOTBPAT57
                      DEFW      FOTBPAT58
                      DEFW      FOTBPAT59
                      DEFW      FOTBPAT60
                      DEFW      FOTBPAT61
                      DEFW      FOTBPAT60
                      DEFW      FOTBPAT62
                      DEFW      FOTBPAT63
                      DEFW      FOTBPAT64
                      DEFW      FOTBPAT65
                      DEFW      FOTBPAT66
                      DEFW      FOTBPAT67
                      DEFW      FOTBPAT66
                      DEFW      FOTBPAT68
                      DEFW      FOTBPAT69
                      DEFW      FOTBPAT70
                      DEFW      FOTBPAT71
                      DEFW      FOTBPAT72
                      DEFW      FOTBPAT73
                      DEFW      FOTBPAT74
                      DEFW      FOTBPAT16
                      DEFW      FOTBPAT75
                      DEFW      FOTBPAT76
                      DEFW      FOTBPAT78
                      DEFW      FOTBPAT77
                      DEFW      $0000
                      DEFW      FOTBLOOPSTART

; *** FOTBPATterns ***
FOTBPAT0:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$D8,$92,$C3,$8E,$C1
                DEFB      $01    ,$01    ,$9D,$ED
                DEFB      $00    ,$00    ,$AD,$25
                DEFB      $01    ,$01    ,$BC,$68
                DEFB      $01    ,$01    ,$CD,$25
                DEFB      $01    ,$01    ,$DC,$68
                DEFB      $01    ,$01    ,$EB,$B6
                DEFB      $01    ,$01    ,$FB,$0D
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT1:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $00    ,$00    ,$FB,$B6
                DEFB      $01    ,$01    ,$EB,$0D
                DEFB      $01    ,$01    ,$DA,$6E
                DEFB      $01    ,$01    ,$C9,$D9
                DEFB      $01    ,$01    ,$B9,$4B
                DEFB      $01    ,$01    ,$A8,$C6
                DEFB      $01    ,$01    ,$98,$47
                DEFB      $01    ,$01    ,$87,$D0
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT2:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $90,$EC,$B1,$61,$87,$60
                DEFB      $01    ,$01    ,$96,$F6
                DEFB      $00    ,$00    ,$A6,$92
                DEFB      $01    ,$01    ,$B6,$34
                DEFB      $01    ,$01    ,$C6,$92
                DEFB      $01    ,$01    ,$D6,$34
                DEFB      $01    ,$01    ,$E5,$DB
                DEFB      $01    ,$01    ,$F5,$86
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT3:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$01    ,$F5,$DB
                DEFB      $01    ,$01    ,$E5,$86
                DEFB      $01    ,$01    ,$D5,$37
                DEFB      $01    ,$01    ,$C4,$E7
                DEFB      $01    ,$01    ,$B4,$A5
                DEFB      $01    ,$01    ,$A4,$63
                DEFB      $01    ,$01    ,$94,$23
                DEFB      $01    ,$01    ,$83,$E8
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT4:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$01    ,$83,$B0
                DEFB      $01    ,$01    ,$93,$7B
                DEFB      $01    ,$01    ,$A3,$49
                DEFB      $01    ,$01    ,$B3,$1A
                DEFB      $01    ,$01    ,$C3,$49
                DEFB      $01    ,$01    ,$D3,$1A
                DEFB      $01    ,$01    ,$E2,$ED
                DEFB      $01    ,$01    ,$F2,$C3
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT5:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$01    ,$F3,$B0
                DEFB      $01    ,$01    ,$E3,$7B
                DEFB      $01    ,$01    ,$D3,$49
                DEFB      $01    ,$01    ,$C3,$1A
                DEFB      $01    ,$01    ,$B3,$49
                DEFB      $01    ,$01    ,$A3,$1A
                DEFB      $01    ,$01    ,$92,$ED
                DEFB      $01    ,$01    ,$82,$C3
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT6:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$3B,$B1,$D8,$83,$B0
                DEFB      $01    ,$01    ,$93,$7B
                DEFB      $00    ,$00    ,$A3,$49
                DEFB      $01    ,$01    ,$B3,$1A
                DEFB      $01    ,$01    ,$C2,$ED
                DEFB      $01    ,$01    ,$D3,$E8
                DEFB      $01    ,$01    ,$E3,$B0
                DEFB      $01    ,$01    ,$F3,$7B
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT7:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$B1,$76,$F3,$B0
                DEFB      $01    ,$01    ,$E3,$7B
                DEFB      $01    ,$00    ,$D3,$49
                DEFB      $01    ,$01    ,$C3,$1A
                DEFB      $01    ,$B1,$3B,$B2,$ED
                DEFB      $01    ,$01    ,$A3,$1A
                DEFB      $01    ,$00    ,$93,$49
                DEFB      $01    ,$01    ,$83,$7B
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT8:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $92,$76,$B2,$76,$83,$B0
                DEFB      $01    ,$01    ,$93,$7B
                DEFB      $00    ,$00    ,$A3,$49
                DEFB      $01    ,$01    ,$B3,$1A
                DEFB      $91,$F4,$B2,$76,$C3,$49
                DEFB      $01    ,$01    ,$D3,$1A
                DEFB      $00    ,$00    ,$E2,$ED
                DEFB      $01    ,$01    ,$F2,$C3
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT9:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$D8,$B2,$76,$F2,$ED
                DEFB      $01    ,$01    ,$E3,$1A
                DEFB      $00    ,$00    ,$D3,$49
                DEFB      $01    ,$01    ,$C3,$7B
                DEFB      $91,$A4,$B1,$D8,$B3,$B0
                DEFB      $01    ,$01    ,$A3,$E8
                DEFB      $00    ,$00    ,$93,$B0
                DEFB      $01    ,$01    ,$83,$7B
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT10:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$76,$01    ,$83,$B0
                DEFB      $01    ,$01    ,$93,$7B
                DEFB      $00    ,$01    ,$A3,$49
                DEFB      $01    ,$01    ,$B3,$1A
                DEFB      $91,$F4,$B2,$76,$C3,$49
                DEFB      $01    ,$01    ,$D3,$1A
                DEFB      $00    ,$00    ,$E2,$ED
                DEFB      $01    ,$01    ,$F2,$C3
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT11:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$D8,$B2,$76,$F2,$ED
                DEFB      $01    ,$01    ,$E3,$1A
                DEFB      $00    ,$00    ,$D3,$49
                DEFB      $01    ,$01    ,$C3,$7B
                DEFB      $91,$A4,$B1,$D8,$B3,$B0
                DEFB      $01    ,$01    ,$A4,$23
                DEFB      $00    ,$00    ,$94,$63
                DEFB      $01    ,$01    ,$84,$A5
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT12:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$8D,$B2,$76,$84,$E7
                DEFB      $01    ,$01    ,$94,$A5
                DEFB      $00    ,$00    ,$A4,$63
                DEFB      $01    ,$01    ,$B4,$23
                DEFB      $91,$A4,$B2,$76,$C3,$E8
                DEFB      $01    ,$01    ,$D5,$37
                DEFB      $00    ,$00    ,$E4,$E7
                DEFB      $01    ,$01    ,$F4,$A5
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT13:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $90,$FA,$B1,$A4,$F4,$E7
                DEFB      $01    ,$01    ,$E4,$A5
                DEFB      $00    ,$00    ,$D4,$63
                DEFB      $01    ,$01    ,$C4,$23
                DEFB      $90,$D2,$B1,$61,$B3,$E8
                DEFB      $01    ,$01    ,$A4,$23
                DEFB      $00    ,$00    ,$94,$63
                DEFB      $01    ,$01    ,$84,$A5
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT14:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $90,$FA,$B1,$A4,$84,$E7
                DEFB      $01    ,$01    ,$94,$A5
                DEFB      $00    ,$00    ,$A4,$63
                DEFB      $01    ,$01    ,$B4,$23
                DEFB      $01    ,$01    ,$C3,$E8
                DEFB      $01    ,$01    ,$D5,$37
                DEFB      $01    ,$01    ,$E4,$E7
                DEFB      $01    ,$01    ,$F4,$A5
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT15:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$01    ,$F4,$E7
                DEFB      $01    ,$01    ,$E4,$A5
                DEFB      $01    ,$01    ,$D4,$63
                DEFB      $01    ,$01    ,$C4,$23
                DEFB      $01    ,$01    ,$B3,$E8
                DEFB      $01    ,$01    ,$A4,$23
                DEFB      $01    ,$01    ,$94,$63
                DEFB      $01    ,$01    ,$84,$A5
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT16:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$01    ,$84,$E7
                DEFB      $01    ,$01    ,$94,$A5
                DEFB      $01    ,$01    ,$A4,$63
                DEFB      $01    ,$01    ,$B4,$23
                DEFB      $01    ,$01    ,$C4,$63
                DEFB      $01    ,$01    ,$D4,$23
                DEFB      $01    ,$01    ,$E3,$E8
                DEFB      $01    ,$01    ,$F3,$B0
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT17:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$01    ,$F3,$E8
                DEFB      $01    ,$01    ,$E4,$23
                DEFB      $01    ,$01    ,$D4,$63
                DEFB      $01    ,$01    ,$C4,$A5
                DEFB      $01    ,$01    ,$B4,$E7
                DEFB      $01    ,$01    ,$A5,$37
                DEFB      $01    ,$01    ,$94,$E7
                DEFB      $01    ,$01    ,$84,$A5
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT18:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$A4,$81,$F4,$B4,$E7
                DEFB      $01    ,$92,$76,$01
                DEFB      $01    ,$A2,$C3,$00
                DEFB      $01    ,$B2,$76,$01
                DEFB      $01    ,$C2,$C3,$B4,$E7
                DEFB      $01    ,$D2,$76,$01
                DEFB      $01    ,$E2,$C3,$01
                DEFB      $01    ,$F2,$76,$01
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT19:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $00    ,$F2,$9B,$B5,$37
                DEFB      $01    ,$E2,$52,$01
                DEFB      $01    ,$D2,$9B,$01
                DEFB      $01    ,$C2,$52,$01
                DEFB      $01    ,$B2,$9B,$01
                DEFB      $01    ,$A2,$52,$01
                DEFB      $01    ,$92,$9B,$01
                DEFB      $01    ,$82,$52,$01
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT20:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $00    ,$00    ,$B4,$E7
                DEFB      $01    ,$92,$76,$01
                DEFB      $01    ,$A2,$C3,$00
                DEFB      $01    ,$B2,$76,$01
                DEFB      $01    ,$C2,$C3,$B4,$E7
                DEFB      $01    ,$D2,$76,$01
                DEFB      $01    ,$E2,$C3,$01
                DEFB      $01    ,$F2,$76,$01
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT21:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $92,$76,$B1,$3B,$84,$E7
                DEFB      $01    ,$01    ,$95,$37
                DEFB      $00    ,$00    ,$A4,$E7
                DEFB      $01    ,$01    ,$B4,$A5
                DEFB      $92,$52,$B2,$9B,$C4,$E7
                DEFB      $01    ,$01    ,$D5,$37
                DEFB      $00    ,$00    ,$E4,$E7
                DEFB      $01    ,$01    ,$F4,$A5
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT22:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $92,$76,$B1,$3B,$F4,$E7
                DEFB      $01    ,$01    ,$E5,$37
                DEFB      $92,$52,$B2,$9B,$D4,$E7
                DEFB      $01    ,$01    ,$C4,$A5
                DEFB      $92,$31,$B2,$C3,$B4,$E7
                DEFB      $01    ,$01    ,$A5,$37
                DEFB      $92,$11,$B2,$ED,$94,$E7
                DEFB      $01    ,$01    ,$84,$A5
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT23:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $81,$F4,$F3,$1A,$84,$E7
                DEFB      $91,$F4,$E3,$1A,$95,$37
                DEFB      $A1,$F4,$D3,$1A,$A5,$86
                DEFB      $B1,$F4,$C3,$1A,$B5,$DB
                DEFB      $C1,$F4,$B3,$1A,$C6,$34
                DEFB      $D1,$F4,$A3,$1A,$D5,$DB
                DEFB      $E1,$F4,$93,$1A,$E5,$86
                DEFB      $F1,$F4,$83,$1A,$F5,$37
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT24:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$01    ,$F4,$E7
                DEFB      $01    ,$01    ,$E5,$37
                DEFB      $01    ,$01    ,$D5,$86
                DEFB      $01    ,$01    ,$C5,$DB
                DEFB      $01    ,$01    ,$B6,$34
                DEFB      $01    ,$01    ,$A5,$DB
                DEFB      $01    ,$01    ,$95,$86
                DEFB      $01    ,$01    ,$85,$37
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT25:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $00    ,$81,$08,$B4,$E7
                DEFB      $01    ,$91,$A4,$01
                DEFB      $01    ,$A1,$D8,$00
                DEFB      $01    ,$B1,$A4,$01
                DEFB      $01    ,$C1,$D8,$B6,$92
                DEFB      $01    ,$D1,$A4,$01
                DEFB      $01    ,$E1,$D8,$01
                DEFB      $01    ,$F1,$A4,$01
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT26:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $B6,$34,$F1,$BD,$96,$F6
                DEFB      $01    ,$E1,$8D,$01
                DEFB      $01    ,$D1,$BD,$01
                DEFB      $01    ,$C1,$8D,$01
                DEFB      $01    ,$B1,$BD,$01
                DEFB      $01    ,$A1,$8D,$01
                DEFB      $01    ,$91,$BD,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT27:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$00    ,$B6,$92
                DEFB      $01    ,$91,$A4,$01
                DEFB      $01    ,$A1,$D8,$00
                DEFB      $01    ,$B1,$A4,$01
                DEFB      $01    ,$C1,$D8,$B6,$92
                DEFB      $01    ,$D1,$A4,$01
                DEFB      $01    ,$E1,$D8,$01
                DEFB      $01    ,$F1,$A4,$01
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT28:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $96,$34,$F1,$BD,$B6,$F6
                DEFB      $01    ,$E1,$8D,$01
                DEFB      $01    ,$D1,$BD,$01
                DEFB      $01    ,$C1,$8D,$01
                DEFB      $01    ,$B1,$BD,$01
                DEFB      $01    ,$A1,$8D,$01
                DEFB      $01    ,$91,$BD,$01
                DEFB      $01    ,$81,$8D,$01
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT29:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $00    ,$B1,$A4,$86,$92
                DEFB      $01    ,$01    ,$96,$F6
                DEFB      $01    ,$00    ,$A6,$92
                DEFB      $01    ,$01    ,$B6,$34
                DEFB      $91,$8D,$B1,$BD,$C6,$92
                DEFB      $01    ,$01    ,$D6,$F6
                DEFB      $00    ,$00    ,$E6,$92
                DEFB      $01    ,$01    ,$F6,$34
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT30:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $00    ,$B1,$A4,$F6,$92
                DEFB      $01    ,$01    ,$E6,$F6
                DEFB      $91,$8D,$B1,$BD,$D6,$92
                DEFB      $01    ,$01    ,$C6,$34
                DEFB      $91,$76,$B1,$D8,$B6,$92
                DEFB      $01    ,$01    ,$A6,$F6
                DEFB      $91,$61,$B1,$F4,$96,$92
                DEFB      $01    ,$01    ,$86,$34
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT31:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$4D,$B2,$11,$86,$92
                DEFB      $01    ,$01    ,$96,$F6
                DEFB      $01    ,$00    ,$A7,$60
                DEFB      $01    ,$01    ,$B7,$D0
                DEFB      $00    ,$01    ,$C8,$47
                DEFB      $01    ,$01    ,$D7,$D0
                DEFB      $01    ,$01    ,$E7,$60
                DEFB      $01    ,$01    ,$F6,$F6
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT32:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$01    ,$F6,$92
                DEFB      $01    ,$01    ,$E6,$F6
                DEFB      $01    ,$01    ,$D7,$60
                DEFB      $01    ,$01    ,$C7,$D0
                DEFB      $01    ,$01    ,$B8,$47
                DEFB      $01    ,$01    ,$A7,$D0
                DEFB      $01    ,$01    ,$97,$60
                DEFB      $01    ,$01    ,$86,$F6
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT33:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$4D,$B2,$31,$86,$92
                DEFB      $01    ,$01    ,$96,$34
                DEFB      $00    ,$00    ,$A5,$DB
                DEFB      $01    ,$01    ,$B5,$86
                DEFB      $00    ,$01    ,$C5,$37
                DEFB      $01    ,$01    ,$D6,$F6
                DEFB      $01    ,$01    ,$E6,$92
                DEFB      $01    ,$01    ,$F6,$34
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT34:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$4D,$B2,$31,$F6,$92
                DEFB      $01    ,$01    ,$E6,$34
                DEFB      $00    ,$00    ,$D5,$DB
                DEFB      $01    ,$01    ,$C5,$86
                DEFB      $91,$76,$B2,$31,$B5,$37
                DEFB      $01    ,$01    ,$A5,$86
                DEFB      $00    ,$00    ,$95,$DB
                DEFB      $01    ,$01    ,$86,$34
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT35:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$4D,$B2,$31,$86,$92
                DEFB      $01    ,$01    ,$96,$34
                DEFB      $00    ,$00    ,$A5,$DB
                DEFB      $01    ,$01    ,$B5,$86
                DEFB      $91,$08,$B1,$BD,$C5,$DB
                DEFB      $01    ,$01    ,$D5,$86
                DEFB      $00    ,$00    ,$E5,$37
                DEFB      $01    ,$01    ,$F4,$E7
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT36:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$18,$B1,$A4,$F5,$37
                DEFB      $01    ,$01    ,$E5,$86
                DEFB      $00    ,$00    ,$D5,$DB
                DEFB      $01    ,$01    ,$C6,$34
                DEFB      $90,$FA,$B1,$A4,$B5,$DB
                DEFB      $01    ,$01    ,$A6,$34
                DEFB      $00    ,$00    ,$96,$92
                DEFB      $01    ,$01    ,$86,$F6
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT37:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $90,$EC,$B1,$D8,$8E,$C1
                DEFB      $01    ,$01    ,$9D,$ED
                DEFB      $01    ,$01    ,$AD,$25
                DEFB      $01    ,$01    ,$BC,$68
                DEFB      $01    ,$01    ,$CD,$25
                DEFB      $01    ,$01    ,$DC,$68
                DEFB      $01    ,$01    ,$EB,$B6
                DEFB      $01    ,$01    ,$FB,$0D
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT38:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$01    ,$FB,$B6
                DEFB      $01    ,$01    ,$EB,$0D
                DEFB      $01    ,$01    ,$DA,$6E
                DEFB      $01    ,$01    ,$C9,$D9
                DEFB      $01    ,$01    ,$B9,$4B
                DEFB      $01    ,$01    ,$A8,$C6
                DEFB      $01    ,$01    ,$98,$47
                DEFB      $01    ,$01    ,$87,$D0
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT39:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $00    ,$00    ,$87,$60
                DEFB      $01    ,$01    ,$97,$D0
                DEFB      $01    ,$01    ,$A7,$60
                DEFB      $01    ,$01    ,$B6,$F6
                DEFB      $90,$EC,$B1,$D8,$C7,$60
                DEFB      $01    ,$01    ,$D7,$D0
                DEFB      $90,$BB,$B1,$76,$E7,$60
                DEFB      $01    ,$01    ,$F6,$F6
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT40:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $90,$9D,$B1,$3B,$F7,$60
                DEFB      $01    ,$01    ,$E7,$D0
                DEFB      $90,$7D,$B0,$FA,$D7,$60
                DEFB      $01    ,$01    ,$C6,$F6
                DEFB      $90,$9D,$B1,$3B,$B7,$60
                DEFB      $01    ,$01    ,$A7,$D0
                DEFB      $90,$BB,$B1,$76,$97,$60
                DEFB      $01    ,$01    ,$86,$F6
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT41:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $90,$EC,$B1,$D8,$87,$60
                DEFB      $01    ,$01    ,$97,$D0
                DEFB      $01    ,$01    ,$A7,$60
                DEFB      $01    ,$01    ,$B6,$F6
                DEFB      $B0,$EC,$F1,$D8,$C7,$60
                DEFB      $01    ,$01    ,$D7,$D0
                DEFB      $B0,$BB,$F1,$76,$E7,$60
                DEFB      $01    ,$01    ,$F6,$F6
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT42:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $B0,$9D,$B1,$3B,$F7,$60
                DEFB      $01    ,$01    ,$E7,$D0
                DEFB      $B0,$7D,$B0,$FA,$D7,$60
                DEFB      $01    ,$01    ,$C6,$F6
                DEFB      $B0,$9D,$B1,$3B,$B7,$60
                DEFB      $01    ,$01    ,$A7,$D0
                DEFB      $B0,$BB,$B1,$76,$97,$60
                DEFB      $01    ,$01    ,$86,$F6
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT43:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $00    ,$81,$D8,$B7,$60
                DEFB      $01    ,$91,$BD,$01
                DEFB      $01    ,$A1,$A4,$00
                DEFB      $01    ,$B1,$8D,$01
                DEFB      $01    ,$C1,$A4,$01
                DEFB      $01    ,$D1,$8D,$01
                DEFB      $01    ,$E1,$76,$01
                DEFB      $01    ,$F1,$61,$01
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT44:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$F1,$76,$01
                DEFB      $01    ,$E1,$61,$01
                DEFB      $01    ,$D1,$4D,$01
                DEFB      $01    ,$C1,$3B,$01
                DEFB      $01    ,$B1,$29,$01
                DEFB      $01    ,$A1,$18,$01
                DEFB      $01    ,$91,$08,$01
                DEFB      $01    ,$80,$FA,$01
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT45:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$80,$EC,$01
                DEFB      $01    ,$90,$FA,$01
                DEFB      $01    ,$A0,$EC,$01
                DEFB      $01    ,$B0,$DE,$01
                DEFB      $01    ,$C0,$EC,$B7,$60
                DEFB      $01    ,$D0,$FA,$01
                DEFB      $01    ,$E0,$EC,$B5,$DB
                DEFB      $01    ,$F0,$DE,$01
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT46:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$F0,$EC,$B4,$E7
                DEFB      $01    ,$E0,$FA,$01
                DEFB      $01    ,$D0,$EC,$B3,$E8
                DEFB      $01    ,$C0,$DE,$01
                DEFB      $01    ,$B0,$EC,$B4,$E7
                DEFB      $01    ,$A0,$FA,$01
                DEFB      $01    ,$90,$EC,$B5,$DB
                DEFB      $01    ,$80,$DE,$01
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT47:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$80,$EC,$97,$60
                DEFB      $01    ,$90,$FA,$01
                DEFB      $01    ,$A0,$EC,$01
                DEFB      $01    ,$B0,$DE,$01
                DEFB      $01    ,$C0,$EC,$F7,$60
                DEFB      $01    ,$D0,$FA,$01
                DEFB      $01    ,$E0,$EC,$F5,$DB
                DEFB      $01    ,$F0,$DE,$01
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT48:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$F0,$EC,$E4,$E7
                DEFB      $01    ,$E0,$FA,$01
                DEFB      $01    ,$D0,$EC,$D3,$E8
                DEFB      $01    ,$C0,$DE,$01
                DEFB      $01    ,$B0,$EC,$C4,$E7
                DEFB      $01    ,$A0,$FA,$01
                DEFB      $01    ,$90,$EC,$B5,$DB
                DEFB      $01    ,$80,$DE,$01
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT49:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$80,$EC,$A7,$60
                DEFB      $01    ,$90,$FA,$01
                DEFB      $01    ,$A1,$08,$00
                DEFB      $01    ,$B1,$18,$01
                DEFB      $01    ,$C1,$29,$01
                DEFB      $01    ,$D1,$3B,$01
                DEFB      $01    ,$E1,$4D,$01
                DEFB      $01    ,$F1,$61,$01
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT50:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$00    ,$F1,$76
                DEFB      $01    ,$01    ,$E1,$8D
                DEFB      $01    ,$01    ,$D1,$A4
                DEFB      $01    ,$01    ,$C1,$BD
                DEFB      $01    ,$01    ,$B1,$D8
                DEFB      $01    ,$01    ,$A1,$F4
                DEFB      $01    ,$01    ,$92,$11
                DEFB      $01    ,$01    ,$82,$31
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT51:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$01    ,$82,$52
                DEFB      $01    ,$01    ,$92,$76
                DEFB      $01    ,$01    ,$A2,$9B
                DEFB      $01    ,$01    ,$B2,$C3
                DEFB      $01    ,$01    ,$C2,$ED
                DEFB      $01    ,$01    ,$D3,$1A
                DEFB      $01    ,$01    ,$E3,$49
                DEFB      $01    ,$01    ,$F3,$7B
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT52:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$01    ,$F3,$B0
                DEFB      $01    ,$01    ,$E3,$E8
                DEFB      $01    ,$01    ,$D3,$B0
                DEFB      $01    ,$01    ,$C3,$7B
                DEFB      $01    ,$01    ,$B3,$B0
                DEFB      $01    ,$01    ,$A3,$E8
                DEFB      $01    ,$01    ,$93,$B0
                DEFB      $01    ,$01    ,$83,$7B
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT53:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $92,$76,$B2,$ED,$83,$B0
                DEFB      $01    ,$01    ,$93,$7B
                DEFB      $00    ,$00    ,$A3,$49
                DEFB      $01    ,$01    ,$B3,$1A
                DEFB      $01    ,$01    ,$C2,$ED
                DEFB      $01    ,$01    ,$D3,$E8
                DEFB      $01    ,$01    ,$E3,$B0
                DEFB      $01    ,$01    ,$F3,$7B
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT54:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$B2,$ED,$F3,$B0
                DEFB      $01    ,$01    ,$E3,$7B
                DEFB      $01    ,$00    ,$D3,$49
                DEFB      $01    ,$01    ,$C3,$1A
                DEFB      $01    ,$B2,$76,$B2,$ED
                DEFB      $01    ,$01    ,$A3,$1A
                DEFB      $01    ,$00    ,$93,$49
                DEFB      $01    ,$01    ,$83,$7B
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT55:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $92,$76,$B2,$ED,$83,$B0
                DEFB      $01    ,$01    ,$93,$7B
                DEFB      $00    ,$00    ,$A3,$49
                DEFB      $01    ,$01    ,$B3,$1A
                DEFB      $91,$F4,$B2,$76,$C3,$49
                DEFB      $01    ,$01    ,$D3,$1A
                DEFB      $00    ,$00    ,$E2,$ED
                DEFB      $01    ,$01    ,$F2,$C3
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT56:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$D8,$B2,$76,$F2,$ED
                DEFB      $01    ,$01    ,$E3,$1A
                DEFB      $00    ,$00    ,$D3,$49
                DEFB      $01    ,$01    ,$C3,$7B
                DEFB      $91,$D8,$B1,$A4,$B3,$B0
                DEFB      $01    ,$01    ,$A3,$E8
                DEFB      $00    ,$00    ,$93,$B0
                DEFB      $01    ,$01    ,$83,$7B
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT57:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$76,$B2,$76,$83,$B0
                DEFB      $01    ,$01    ,$93,$7B
                DEFB      $00    ,$00    ,$A3,$49
                DEFB      $01    ,$01    ,$B3,$1A
                DEFB      $91,$F4,$B2,$76,$C3,$49
                DEFB      $01    ,$01    ,$D3,$1A
                DEFB      $00    ,$00    ,$E2,$ED
                DEFB      $01    ,$01    ,$F2,$C3
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT58:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$D8,$B2,$76,$F2,$ED
                DEFB      $01    ,$01    ,$E3,$1A
                DEFB      $00    ,$00    ,$D3,$49
                DEFB      $01    ,$01    ,$C3,$7B
                DEFB      $91,$D8,$B1,$A4,$B3,$B0
                DEFB      $01    ,$01    ,$A4,$23
                DEFB      $00    ,$00    ,$94,$63
                DEFB      $01    ,$01    ,$84,$A5
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT59:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$8D,$B2,$31,$84,$E7
                DEFB      $01    ,$01    ,$94,$A5
                DEFB      $00    ,$00    ,$A4,$63
                DEFB      $01    ,$01    ,$B4,$23
                DEFB      $91,$A4,$B1,$F4,$C3,$E8
                DEFB      $01    ,$01    ,$D5,$37
                DEFB      $00    ,$00    ,$E4,$E7
                DEFB      $01    ,$01    ,$F4,$A5
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT60:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$F4,$B3,$49,$F4,$E7
                DEFB      $01    ,$01    ,$E4,$A5
                DEFB      $00    ,$00    ,$D4,$63
                DEFB      $01    ,$01    ,$C4,$23
                DEFB      $91,$A4,$B2,$C3,$B3,$E8
                DEFB      $01    ,$01    ,$A4,$23
                DEFB      $00    ,$00    ,$94,$63
                DEFB      $01    ,$01    ,$84,$A5
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT61:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$F4,$B3,$49,$84,$E7
                DEFB      $01    ,$01    ,$94,$A5
                DEFB      $00    ,$00    ,$A4,$63
                DEFB      $01    ,$01    ,$B4,$23
                DEFB      $01    ,$01    ,$C3,$E8
                DEFB      $01    ,$01    ,$D5,$37
                DEFB      $01    ,$01    ,$E4,$E7
                DEFB      $01    ,$01    ,$F4,$A5
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT62:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$F4,$B3,$49,$84,$E7
                DEFB      $01    ,$01    ,$94,$A5
                DEFB      $00    ,$00    ,$A4,$63
                DEFB      $01    ,$01    ,$B4,$23
                DEFB      $92,$31,$B2,$9B,$C4,$63
                DEFB      $01    ,$01    ,$D4,$23
                DEFB      $00    ,$00    ,$E3,$E8
                DEFB      $01    ,$01    ,$F3,$B0
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT63:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$F4,$B3,$49,$F3,$E8
                DEFB      $01    ,$01    ,$E4,$23
                DEFB      $00    ,$00    ,$D4,$63
                DEFB      $01    ,$01    ,$C4,$A5
                DEFB      $92,$76,$B3,$1A,$B4,$E7
                DEFB      $01    ,$01    ,$A5,$37
                DEFB      $00    ,$00    ,$94,$E7
                DEFB      $01    ,$01    ,$84,$A5
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT64:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$F4,$B3,$49,$84,$E7
                DEFB      $01    ,$01    ,$94,$A5
                DEFB      $00    ,$00    ,$A4,$63
                DEFB      $01    ,$01    ,$B4,$23
                DEFB      $91,$F4,$B3,$49,$C3,$E8
                DEFB      $01    ,$01    ,$D4,$23
                DEFB      $00    ,$00    ,$E4,$63
                DEFB      $01    ,$01    ,$F4,$A5
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT65:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$D8,$B2,$ED,$F4,$E7
                DEFB      $01    ,$01    ,$E5,$86
                DEFB      $00    ,$00    ,$D5,$DB
                DEFB      $01    ,$01    ,$C6,$92
                DEFB      $91,$D8,$B2,$C3,$B7,$60
                DEFB      $01    ,$01    ,$A7,$D0
                DEFB      $00    ,$00    ,$97,$60
                DEFB      $01    ,$01    ,$86,$F6
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT66:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$3B,$B1,$D8,$87,$60
                DEFB      $01    ,$01    ,$96,$F6
                DEFB      $91,$D8,$B2,$ED,$A6,$92
                DEFB      $01    ,$01    ,$B6,$34
                DEFB      $00    ,$00    ,$C5,$DB
                DEFB      $01    ,$01    ,$D7,$D0
                DEFB      $01    ,$01    ,$E7,$60
                DEFB      $01    ,$01    ,$F6,$F6
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT67:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $B2,$ED,$00    ,$F7,$60
                DEFB      $01    ,$01    ,$E6,$F6
                DEFB      $00    ,$01    ,$D6,$92
                DEFB      $01    ,$01    ,$C6,$34
                DEFB      $B2,$76,$01    ,$B5,$DB
                DEFB      $01    ,$01    ,$A6,$34
                DEFB      $00    ,$01    ,$96,$92
                DEFB      $01    ,$01    ,$86,$F6
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT68:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $92,$76,$B2,$ED,$F7,$60
                DEFB      $01    ,$01    ,$E6,$F6
                DEFB      $00    ,$00    ,$D6,$92
                DEFB      $01    ,$01    ,$C6,$34
                DEFB      $92,$11,$B3,$49,$B5,$DB
                DEFB      $01    ,$01    ,$A6,$34
                DEFB      $00    ,$00    ,$96,$92
                DEFB      $01    ,$01    ,$86,$F6
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT69:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $92,$76,$B2,$ED,$87,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$00    ,$A4,$A5
                DEFB      $01    ,$01    ,$B4,$E7
                DEFB      $92,$76,$B2,$77,$C5,$37
                DEFB      $01    ,$01    ,$D5,$86
                DEFB      $91,$F4,$B1,$F5,$E5,$DB
                DEFB      $01    ,$01    ,$F6,$34
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT70:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$A4,$B1,$A5,$F6,$92
                DEFB      $01    ,$01    ,$E6,$34
                DEFB      $91,$61,$B1,$62,$D5,$DB
                DEFB      $01    ,$01    ,$C5,$86
                DEFB      $91,$A4,$B1,$A5,$B5,$DB
                DEFB      $01    ,$01    ,$A5,$86
                DEFB      $91,$F4,$B1,$F5,$95,$37
                DEFB      $01    ,$01    ,$84,$E7
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT71:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$76,$B2,$76,$84,$A5
                DEFB      $01    ,$01    ,$94,$E7
                DEFB      $00    ,$00    ,$A5,$37
                DEFB      $01    ,$01    ,$B5,$86
                DEFB      $01    ,$01    ,$C5,$DB
                DEFB      $01    ,$01    ,$D6,$34
                DEFB      $01    ,$01    ,$E6,$92
                DEFB      $01    ,$01    ,$F6,$F6
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT72:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $91,$D8,$B3,$B0,$F7,$60
                DEFB      $01    ,$01    ,$E7,$D0
                DEFB      $00    ,$00    ,$D7,$60
                DEFB      $01    ,$01    ,$C6,$F6
                DEFB      $01    ,$01    ,$B7,$60
                DEFB      $01    ,$01    ,$A8,$47
                DEFB      $01    ,$01    ,$98,$C6
                DEFB      $01    ,$01    ,$89,$4B
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT73:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $92,$76,$B4,$E7,$89,$D9
                DEFB      $01    ,$01    ,$99,$4B
                DEFB      $00    ,$00    ,$A8,$C6
                DEFB      $01    ,$01    ,$B8,$47
                DEFB      $92,$76,$B4,$63,$C8,$C6
                DEFB      $01    ,$01    ,$D8,$47
                DEFB      $00    ,$00    ,$E7,$D0
                DEFB      $01    ,$01    ,$F7,$60
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT74:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $92,$76,$B3,$E8,$F7,$D0
                DEFB      $01    ,$01    ,$E7,$60
                DEFB      $00    ,$00    ,$D6,$F6
                DEFB      $01    ,$01    ,$C6,$92
                DEFB      $01    ,$01    ,$B6,$34
                DEFB      $01    ,$01    ,$A5,$DB
                DEFB      $01    ,$01    ,$95,$86
                DEFB      $01    ,$01    ,$85,$37
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT75:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$01    ,$F3,$E8
                DEFB      $01    ,$01    ,$E3,$B0
                DEFB      $01    ,$01    ,$D3,$7B
                DEFB      $01    ,$01    ,$C3,$49
                DEFB      $01    ,$01    ,$B3,$1A
                DEFB      $01    ,$01    ,$A2,$ED
                DEFB      $01    ,$01    ,$92,$C3
                DEFB      $01    ,$01    ,$82,$9B
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT76:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$01    ,$82,$76
                DEFB      $01    ,$01    ,$92,$9B
                DEFB      $01    ,$01    ,$A2,$76
                DEFB      $01    ,$01    ,$B2,$52
                DEFB      $01    ,$01    ,$C2,$9B
                DEFB      $01    ,$01    ,$D2,$52
                DEFB      $01    ,$01    ,$E2,$9B
                DEFB      $01    ,$01    ,$F2,$52
                DEFB      $01    ,$F0,$9D,$F2,$76
                DEFB      $01    ,$E1,$3B,$01
                DEFB      $01    ,$D1,$29,$D2,$C3
                DEFB      $01    ,$C1,$3B,$01
                DEFB      $01    ,$F1,$18,$B2,$ED
                DEFB      $01    ,$E1,$3B,$01
                DEFB      $01    ,$D0,$FA,$93,$49
                DEFB      $01    ,$C1,$3B,$01
                DEFB      $01    ,$00    ,$83,$B0
                DEFB      $01    ,$01    ,$93,$E8
                DEFB      $01    ,$01    ,$A3,$B0
                DEFB      $01    ,$01    ,$B3,$7B
                DEFB      $01    ,$01    ,$C3,$E8
                DEFB      $01    ,$01    ,$D3,$7B
                DEFB      $01    ,$01    ,$E3,$E8
                DEFB      $01    ,$01    ,$F3,$7B
                DEFB      $01    ,$01    ,$F3,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$B3,$B0,$D4,$23
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$B3,$B0,$B4,$63
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$B3,$49,$94,$A5
                DEFB      $01    ,$01    ,$01
                DEFB      $91,$3B,$B2,$ED,$84,$E7
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$C1,$D8
                DEFB      $01    ,$01    ,$D1,$F4
                DEFB      $01    ,$01    ,$E2,$11
                DEFB      $01    ,$01    ,$F2,$31
                DEFB      $01    ,$01    ,$F2,$52
                DEFB      $01    ,$01    ,$E2,$76
                DEFB      $01    ,$01    ,$D2,$9B
                DEFB      $01    ,$01    ,$C2,$C3
                DEFB      $01    ,$01    ,$B2,$ED
                DEFB      $01    ,$01    ,$A3,$1A
                DEFB      $01    ,$01    ,$93,$49
                DEFB      $01    ,$01    ,$83,$7B
                DEFB      $01    ,$01    ,$83,$B0
                DEFB      $01    ,$01    ,$93,$E8
                DEFB      $01    ,$01    ,$A4,$23
                DEFB      $01    ,$01    ,$B4,$63
                DEFB      $01    ,$01    ,$C4,$A5
                DEFB      $01    ,$01    ,$D4,$E7
                DEFB      $01    ,$01    ,$E5,$37
                DEFB      $01    ,$01    ,$F5,$86
                DEFB      $01    ,$01    ,$F5,$DB
                DEFB      $01    ,$01    ,$E6,$34
                DEFB      $01    ,$01    ,$D6,$92
                DEFB      $01    ,$01    ,$C6,$F6
                DEFB      $01    ,$01    ,$B7,$60
                DEFB      $01    ,$01    ,$A8,$47
                DEFB      $01    ,$01    ,$98,$C6
                DEFB      $01    ,$01    ,$89,$4B
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT77:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $00    ,$00    ,$00
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
                DEFB  $FF  ; End of FOTBPATtern

FOTBPAT78:
                DEFW  1451   ; was 1850     ; FOTBPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB      $01    ,$01    ,$89,$D9
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $91,$3B,$B2,$76,$F4,$E7
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $F0,$9D,$B1,$3C,$81,$3B
                DEFB      $E0,$9D,$01    ,$91,$3B
                DEFB      $D0,$9D,$01    ,$A1,$3B
                DEFB      $C0,$9D,$01    ,$B1,$3B
                DEFB      $B0,$9D,$01    ,$C1,$3B
                DEFB      $A0,$9D,$01    ,$D1,$3B
                DEFB      $90,$9D,$01    ,$E1,$3B
                DEFB      $80,$9D,$01    ,$F1,$3B
                DEFB  $FF  ; End of FOTBPATtern





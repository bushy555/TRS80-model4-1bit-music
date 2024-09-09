
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
                      DEFW      PAT3
                      DEFW      PAT4
                      DEFW      PAT5
                      DEFW      PAT6
                      DEFW      PAT5
                      DEFW      PAT6
                      DEFW      PAT7
                      DEFW      PAT8
                      DEFW      PAT7
                      DEFW      PAT8
                      DEFW      PAT5
                      DEFW      PAT6
                      DEFW      PAT5
                      DEFW      PAT9
                      DEFW      PAT10
                      DEFW      PAT11
                      DEFW      PAT12
                      DEFW      PAT13
                      DEFW      PAT12
                      DEFW      PAT14
                      DEFW      PAT15
                      DEFW      PAT13
                      DEFW      PAT15
                      DEFW      PAT16
                      DEFW      PAT0
                      DEFW      PAT1
                      DEFW      PAT0
                      DEFW      PAT2
                      DEFW      PAT3
                      DEFW      PAT4
                      DEFW      PAT7
                      DEFW      PAT8
                      DEFW      PAT5
                      DEFW      PAT6
                      DEFW      PAT5
                      DEFW      PAT9
                      DEFW      PAT10
                      DEFW      PAT11
                      DEFW      PAT12
                      DEFW      PAT13
                      DEFW      PAT12
                      DEFW      PAT14
                      DEFW      PAT12
                      DEFW      PAT13
                      DEFW      PAT15
                      DEFW      PAT13
                      DEFW      PAT15
                      DEFW      PAT16
                      DEFW      PAT17
                      DEFW      PAT18
                      DEFW      $0000
                      DEFW      LOOPSTART

; *** Patterns ***
PAT0:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$80,$BB,$80,$BC,$90,$BB
                DEFB      $80,$BB,$01    ,$90,$BB
                DEFB      $00    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$80,$BB,$A0,$BD,$90,$B9
                DEFB      $80,$BB,$01    ,$01
                DEFB      $00    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$B2,$31,$C2,$2F,$93,$7D
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $A1,$76,$D2,$EB,$93,$7D
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$00    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$92,$ED,$E3,$79,$92,$9D
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$31,$F2,$2F,$92,$33
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of Pattern

PAT1:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$80,$BB,$80,$BC,$90,$BB
                DEFB      $80,$BB,$01    ,$90,$BB
                DEFB      $00    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$98,$C6,$9A,$6E
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$01    ,$97,$D0,$99,$4B
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$80,$BB,$A0,$BD,$98,$C4
                DEFB      $80,$BB,$01    ,$01
                DEFB      $00    ,$A1,$BD,$01
                DEFB      $01    ,$A2,$31,$01
                DEFB      $01    ,$B2,$9B,$01
                DEFB      $01    ,$B2,$ED,$01
                DEFB      $01    ,$B3,$7B,$01
                DEFB      $01    ,$B4,$63,$01
                DEFB  $02,$01    ,$C5,$35,$96,$94
                DEFB      $01    ,$00    ,$00
                DEFB      $01    ,$C3,$E8,$95,$37
                DEFB      $01    ,$00    ,$00
                DEFB      $01    ,$D3,$47,$94,$65
                DEFB      $01    ,$00    ,$00
                DEFB  $03,$01    ,$D3,$49,$93,$E8
                DEFB      $01    ,$00    ,$00
                DEFB  $02,$01    ,$E2,$EB,$93,$7D
                DEFB      $01    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $C0,$BB,$A0,$BC,$B0,$BA
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$00    ,$00
                DEFB  $FF  ; End of Pattern

PAT2:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$80,$BB,$80,$BC,$90,$BB
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$98,$C6,$9A,$6E
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$01    ,$97,$D0,$99,$4B
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$80,$BB,$A0,$BD,$98,$C4
                DEFB      $80,$BB,$01    ,$01
                DEFB      $00    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$C1,$72,$C5,$D9,$90,$E0
                DEFB      $C1,$6E,$00    ,$00
                DEFB  $04,$C1,$6A,$C5,$DB,$01
                DEFB      $C1,$66,$00    ,$01
                DEFB  $05,$D1,$5E,$D5,$D9,$91,$1A
                DEFB      $D1,$56,$00    ,$00
                DEFB  $03,$D1,$52,$D5,$DB,$01
                DEFB      $D1,$4E,$00    ,$01
                DEFB  $02,$E1,$44,$E5,$D9,$90,$E0
                DEFB      $E1,$3A,$00    ,$00
                DEFB  $05,$E1,$30,$E5,$DB,$01
                DEFB  $05,$E1,$26,$00    ,$01
                DEFB  $05,$F1,$1C,$A5,$DC,$B1,$17
                DEFB  $05,$F1,$12,$00    ,$00
                DEFB  $05,$F1,$76,$A5,$DB,$01
                DEFB  $05,$00    ,$00    ,$01
                DEFB  $FF  ; End of Pattern

PAT3:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$80,$BB,$80,$BB,$92,$31
                DEFB      $80,$BB,$81,$BD,$01
                DEFB      $F2,$31,$82,$ED,$01
                DEFB      $F1,$76,$81,$BD,$01
                DEFB      $E1,$BD,$92,$31,$01
                DEFB      $E2,$31,$92,$ED,$01
                DEFB  $03,$E1,$76,$91,$BD,$01
                DEFB      $E1,$BD,$92,$31,$01
                DEFB  $02,$80,$BB,$A1,$76,$01
                DEFB      $80,$BB,$A1,$BD,$01
                DEFB      $D2,$31,$A2,$31,$01
                DEFB      $D1,$76,$A2,$ED,$01
                DEFB      $C1,$BD,$B1,$BD,$01
                DEFB      $C2,$31,$B2,$31,$01
                DEFB      $C1,$76,$B2,$ED,$01
                DEFB      $C1,$BD,$B1,$BD,$01
                DEFB  $02,$B2,$31,$C2,$31,$92,$35
                DEFB      $B1,$76,$C2,$ED,$92,$39
                DEFB      $B1,$BD,$C1,$BD,$92,$3D
                DEFB      $B2,$31,$C2,$31,$92,$41
                DEFB      $A1,$76,$D2,$ED,$92,$45
                DEFB      $A1,$BD,$D1,$BD,$92,$49
                DEFB  $03,$A2,$31,$D2,$31,$92,$4D
                DEFB      $A1,$76,$D2,$ED,$92,$51
                DEFB  $02,$91,$BD,$E1,$BD,$92,$55
                DEFB      $92,$31,$E2,$31,$92,$59
                DEFB      $91,$76,$E2,$ED,$92,$5D
                DEFB      $91,$BD,$E1,$BD,$92,$63
                DEFB      $82,$31,$F2,$31,$92,$6D
                DEFB      $81,$76,$F2,$ED,$92,$81
                DEFB  $03,$81,$BD,$F1,$BD,$92,$8B
                DEFB      $82,$31,$F2,$31,$92,$95
                DEFB  $FF  ; End of Pattern

PAT4:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$80,$BB,$82,$ED,$95,$37
                DEFB      $80,$BB,$81,$BD,$92,$9B
                DEFB      $F2,$31,$82,$ED,$94,$E7
                DEFB      $F1,$76,$81,$BD,$92,$76
                DEFB      $E1,$BD,$92,$31,$94,$A5
                DEFB      $E2,$31,$92,$ED,$92,$52
                DEFB  $03,$E1,$76,$91,$BD,$94,$63
                DEFB      $E1,$BD,$92,$31,$01
                DEFB  $02,$80,$BB,$A2,$ED,$94,$63
                DEFB      $80,$BB,$A1,$BD,$01
                DEFB      $D2,$31,$A2,$31,$A4,$63
                DEFB      $D1,$76,$A2,$ED,$01
                DEFB      $C1,$BD,$B1,$BD,$01
                DEFB      $C2,$31,$B2,$31,$01
                DEFB      $C1,$76,$B2,$ED,$B4,$63
                DEFB      $C1,$BD,$B1,$BD,$01
                DEFB  $02,$B2,$31,$C2,$31,$01
                DEFB      $B1,$76,$C2,$ED,$01
                DEFB      $B1,$BD,$C1,$BD,$C4,$63
                DEFB      $B2,$31,$C2,$31,$01
                DEFB      $A1,$76,$D2,$ED,$01
                DEFB      $A1,$BD,$D1,$BD,$01
                DEFB  $03,$A2,$31,$D2,$31,$D4,$63
                DEFB      $A1,$76,$D2,$ED,$01
                DEFB  $02,$91,$BD,$E1,$BD,$01
                DEFB      $92,$31,$E2,$31,$01
                DEFB      $91,$76,$E2,$ED,$E4,$63
                DEFB      $91,$BD,$E1,$BD,$01
                DEFB      $82,$31,$F2,$31,$01
                DEFB      $81,$76,$F2,$ED,$01
                DEFB  $03,$80,$A6,$81,$4D,$F4,$63
                DEFB      $81,$4D,$80,$A6,$01
                DEFB  $FF  ; End of Pattern

PAT5:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$29,$81,$BD,$92,$ED
                DEFB      $81,$29,$82,$52,$01
                DEFB      $F2,$52,$82,$ED,$01
                DEFB      $F1,$BD,$81,$BD,$01
                DEFB      $E1,$76,$92,$52,$01
                DEFB      $E1,$BD,$92,$ED,$01
                DEFB  $03,$E2,$52,$91,$BD,$01
                DEFB      $E1,$76,$92,$52,$01
                DEFB  $02,$81,$29,$A2,$ED,$01
                DEFB      $81,$29,$A1,$BD,$01
                DEFB      $D1,$76,$A2,$52,$01
                DEFB      $D1,$BD,$A2,$ED,$01
                DEFB      $C2,$52,$B1,$BD,$01
                DEFB      $C1,$76,$B2,$52,$01
                DEFB      $C1,$BD,$B2,$ED,$01
                DEFB      $C2,$52,$B1,$BD,$01
                DEFB  $02,$B1,$76,$C2,$52,$92,$F1
                DEFB      $B1,$BD,$C2,$ED,$92,$F5
                DEFB      $B2,$52,$C1,$BD,$92,$F9
                DEFB      $B1,$76,$C2,$52,$92,$FD
                DEFB      $A1,$BD,$D2,$ED,$93,$01
                DEFB      $A2,$52,$D1,$BD,$93,$05
                DEFB  $03,$A1,$76,$D2,$52,$93,$09
                DEFB      $A1,$BD,$D2,$ED,$93,$0D
                DEFB  $02,$92,$52,$E1,$BD,$93,$11
                DEFB      $91,$76,$E2,$52,$93,$15
                DEFB      $91,$BD,$E2,$ED,$93,$19
                DEFB      $92,$52,$E1,$BD,$93,$1F
                DEFB      $81,$76,$F2,$52,$93,$29
                DEFB      $81,$BD,$F2,$ED,$93,$3D
                DEFB  $03,$82,$52,$F1,$BD,$93,$47
                DEFB      $81,$76,$F2,$52,$93,$51
                DEFB  $FF  ; End of Pattern

PAT6:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$80,$FA,$80,$FA,$93,$7B
                DEFB      $80,$FA,$80,$FA,$91,$BD
                DEFB      $F2,$9B,$81,$76,$93,$49
                DEFB      $F2,$ED,$81,$F4,$91,$A4
                DEFB      $E1,$F4,$92,$9B,$93,$1A
                DEFB      $E2,$9B,$91,$76,$91,$8D
                DEFB  $03,$E2,$ED,$90,$FA,$92,$ED
                DEFB      $E1,$F4,$91,$4D,$91,$76
                DEFB  $02,$80,$FA,$A0,$FA,$01
                DEFB      $80,$FA,$A0,$FA,$01
                DEFB      $D2,$9B,$A1,$76,$A2,$ED
                DEFB      $D2,$ED,$B0,$FA,$01
                DEFB      $C1,$F4,$B1,$4D,$01
                DEFB      $C2,$9B,$B1,$76,$01
                DEFB      $C2,$ED,$B0,$FA,$B2,$ED
                DEFB      $C1,$F4,$C1,$4D,$01
                DEFB  $02,$B2,$9B,$C1,$76,$01
                DEFB      $B2,$ED,$C0,$FA,$01
                DEFB      $B1,$F4,$C1,$4D,$C2,$ED
                DEFB      $B2,$9B,$D1,$76,$01
                DEFB      $A2,$ED,$D0,$FA,$01
                DEFB      $A1,$F4,$D1,$4D,$01
                DEFB  $03,$A2,$9B,$D1,$76,$D2,$ED
                DEFB      $A2,$ED,$E0,$FA,$01
                DEFB  $02,$91,$F4,$E1,$4D,$01
                DEFB      $92,$9B,$E1,$76,$01
                DEFB      $92,$ED,$E0,$FA,$E2,$ED
                DEFB      $91,$F4,$F1,$4D,$01
                DEFB      $82,$9B,$F1,$76,$01
                DEFB      $82,$ED,$F0,$FA,$01
                DEFB  $09,$81,$F4,$F1,$4D,$F2,$ED
                DEFB      $82,$9B,$F1,$76,$01
                DEFB  $FF  ; End of Pattern

PAT7:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$80,$BB,$81,$BD,$92,$31
                DEFB      $80,$BB,$81,$BD,$01
                DEFB      $F2,$31,$82,$ED,$01
                DEFB      $F1,$76,$81,$BD,$01
                DEFB      $E1,$BD,$92,$31,$01
                DEFB      $E2,$31,$92,$ED,$01
                DEFB  $03,$E1,$76,$91,$BD,$01
                DEFB      $E1,$BD,$92,$31,$01
                DEFB  $02,$80,$BB,$A1,$76,$01
                DEFB      $80,$BB,$A1,$BD,$01
                DEFB      $D2,$31,$A2,$31,$01
                DEFB      $D1,$76,$A2,$ED,$01
                DEFB      $C1,$BD,$B1,$BD,$01
                DEFB      $C2,$31,$B2,$31,$01
                DEFB      $C1,$76,$B2,$ED,$01
                DEFB      $C1,$BD,$B1,$BD,$01
                DEFB  $02,$B2,$31,$C2,$31,$92,$35
                DEFB      $B1,$76,$C2,$ED,$92,$39
                DEFB      $B1,$BD,$C1,$BD,$92,$3D
                DEFB      $B2,$31,$C2,$31,$92,$41
                DEFB      $A1,$76,$D2,$ED,$92,$45
                DEFB      $A1,$BD,$D1,$BD,$92,$49
                DEFB  $03,$A2,$31,$D2,$31,$92,$4D
                DEFB      $A1,$76,$D2,$ED,$92,$51
                DEFB  $02,$91,$BD,$E1,$BD,$92,$55
                DEFB      $92,$31,$E2,$31,$92,$59
                DEFB      $91,$76,$E2,$ED,$92,$5D
                DEFB      $91,$BD,$E1,$BD,$92,$63
                DEFB      $82,$31,$F2,$31,$92,$6D
                DEFB      $81,$76,$F2,$ED,$92,$81
                DEFB  $03,$81,$BD,$F1,$BD,$92,$8B
                DEFB      $82,$31,$F2,$31,$92,$95
                DEFB  $FF  ; End of Pattern

PAT8:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$80,$BB,$83,$7B,$95,$37
                DEFB      $80,$BB,$81,$BD,$92,$9B
                DEFB      $F2,$31,$83,$49,$94,$E7
                DEFB      $F1,$76,$81,$A4,$92,$76
                DEFB      $E1,$BD,$93,$1A,$94,$A5
                DEFB      $E2,$31,$91,$8D,$92,$52
                DEFB  $03,$E1,$76,$92,$ED,$94,$63
                DEFB      $E1,$BD,$91,$76,$01
                DEFB  $02,$80,$BB,$A2,$ED,$94,$63
                DEFB      $80,$BB,$A2,$ED,$01
                DEFB      $D2,$31,$A5,$DB,$A4,$63
                DEFB      $D1,$76,$A2,$ED,$A4,$63
                DEFB      $C1,$BD,$B5,$DB,$A1,$4D
                DEFB      $C2,$31,$B1,$4D,$A4,$63
                DEFB      $C1,$76,$B5,$DB,$B4,$63
                DEFB      $C1,$BD,$B1,$4D,$B4,$63
                DEFB  $02,$B2,$31,$C5,$DB,$B1,$4D
                DEFB      $B1,$76,$C1,$3B,$B4,$63
                DEFB      $B1,$BD,$C5,$DB,$C4,$63
                DEFB      $B2,$31,$C1,$3B,$C4,$63
                DEFB      $A1,$76,$D5,$DB,$C1,$3B
                DEFB      $A1,$BD,$D1,$18,$C4,$63
                DEFB  $03,$A2,$31,$D5,$DB,$D4,$63
                DEFB      $A1,$76,$D1,$18,$D4,$63
                DEFB  $02,$91,$BD,$E5,$DB,$D0,$FA
                DEFB      $92,$31,$E0,$FA,$D4,$63
                DEFB      $91,$76,$E5,$DB,$E4,$63
                DEFB      $91,$BD,$E0,$FA,$E4,$63
                DEFB      $82,$31,$F5,$DB,$E0,$DE
                DEFB      $81,$76,$F0,$DE,$E4,$63
                DEFB  $03,$80,$A6,$81,$4D,$F4,$63
                DEFB      $81,$4D,$80,$A6,$F4,$63
                DEFB  $FF  ; End of Pattern

PAT9:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$80,$FA,$80,$FA,$93,$7B
                DEFB      $80,$FA,$80,$FA,$91,$BD
                DEFB      $F2,$9B,$81,$76,$93,$49
                DEFB      $F2,$ED,$81,$F4,$91,$A4
                DEFB      $E1,$F4,$92,$9B,$93,$1A
                DEFB      $E2,$9B,$91,$76,$91,$8D
                DEFB  $03,$E2,$ED,$90,$FA,$92,$ED
                DEFB      $E1,$F4,$91,$4D,$91,$76
                DEFB  $02,$80,$FA,$A0,$FA,$01
                DEFB      $80,$FA,$A0,$FA,$01
                DEFB      $D2,$9B,$A1,$76,$A2,$ED
                DEFB      $D2,$ED,$B0,$FA,$01
                DEFB      $C1,$F4,$B1,$4D,$A2,$9B
                DEFB      $C2,$9B,$B2,$ED,$A2,$C3
                DEFB      $C2,$ED,$B0,$FA,$B2,$ED
                DEFB      $C1,$F4,$C1,$4D,$01
                DEFB  $02,$B2,$9B,$C1,$76,$B3,$7B
                DEFB      $B2,$ED,$C0,$FA,$B3,$49
                DEFB      $B1,$F4,$C1,$4D,$C2,$ED
                DEFB      $B2,$9B,$D2,$ED,$01
                DEFB      $A2,$ED,$D0,$FA,$C2,$9B
                DEFB      $A1,$F4,$D1,$4D,$C2,$C3
                DEFB  $03,$A2,$9B,$D2,$ED,$D2,$ED
                DEFB      $A2,$ED,$E0,$FA,$01
                DEFB  $02,$91,$F4,$E1,$4D,$D3,$7B
                DEFB      $92,$9B,$E2,$ED,$D3,$49
                DEFB      $92,$ED,$E0,$FA,$E2,$ED
                DEFB      $91,$F4,$F1,$4D,$01
                DEFB      $82,$9B,$F2,$ED,$E2,$9B
                DEFB      $82,$ED,$F0,$FA,$E2,$C3
                DEFB  $03,$81,$F4,$F1,$4D,$F2,$ED
                DEFB      $82,$9B,$F2,$ED,$01
                DEFB  $FF  ; End of Pattern

PAT10:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$18,$81,$18,$90,$FA
                DEFB      $81,$18,$81,$18,$91,$18
                DEFB      $F1,$A4,$82,$C3,$90,$FA
                DEFB      $F1,$F4,$83,$49,$91,$18
                DEFB      $E1,$61,$92,$C3,$90,$FA
                DEFB      $E1,$A4,$93,$49,$91,$18
                DEFB  $03,$E1,$F4,$92,$C3,$90,$FA
                DEFB      $E2,$31,$93,$49,$91,$18
                DEFB  $02,$81,$18,$81,$18,$91,$18
                DEFB      $81,$18,$81,$18,$91,$61
                DEFB      $D2,$31,$A2,$C3,$A1,$18
                DEFB      $D2,$C3,$B3,$49,$A1,$61
                DEFB      $C1,$F4,$B2,$C3,$A1,$18
                DEFB      $C2,$31,$B3,$49,$A1,$61
                DEFB      $C2,$C3,$B2,$C3,$B1,$18
                DEFB      $C3,$49,$C3,$49,$B1,$61
                DEFB  $02,$B2,$31,$C2,$C3,$B1,$61
                DEFB      $B2,$C3,$C3,$49,$B1,$A4
                DEFB      $B3,$49,$C2,$C3,$C1,$61
                DEFB      $B3,$E8,$D3,$49,$C1,$A4
                DEFB      $A3,$49,$D2,$C3,$C1,$61
                DEFB      $A3,$E8,$D3,$49,$C1,$A4
                DEFB  $03,$A4,$63,$D2,$C3,$D1,$61
                DEFB      $A5,$86,$E3,$49,$D1,$A4
                DEFB  $02,$93,$E8,$E2,$C3,$D1,$A4
                DEFB      $94,$63,$E3,$49,$D1,$F4
                DEFB      $95,$86,$E2,$C3,$E1,$A4
                DEFB      $96,$92,$F3,$49,$E1,$F4
                DEFB      $84,$63,$F2,$C3,$E1,$A4
                DEFB      $85,$86,$F3,$49,$E1,$F4
                DEFB  $03,$86,$92,$F2,$C3,$F1,$A4
                DEFB      $87,$D0,$F3,$49,$F1,$F4
                DEFB  $FF  ; End of Pattern

PAT11:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$29,$82,$C3,$91,$29
                DEFB      $81,$29,$83,$49,$92,$52
                DEFB      $F1,$A4,$82,$C3,$91,$F4
                DEFB      $F1,$F4,$83,$49,$92,$52
                DEFB      $E1,$61,$92,$C3,$91,$F4
                DEFB      $E1,$A4,$93,$49,$92,$52
                DEFB  $03,$E1,$F4,$92,$C3,$91,$F4
                DEFB      $E2,$52,$93,$49,$92,$52
                DEFB  $02,$81,$A4,$81,$61,$91,$29
                DEFB      $81,$29,$81,$18,$91,$29
                DEFB      $D2,$52,$A2,$C3,$A2,$52
                DEFB      $D3,$49,$B3,$49,$A2,$C3
                DEFB      $C1,$F4,$B2,$C3,$A2,$52
                DEFB      $C2,$52,$B3,$49,$A2,$C3
                DEFB      $C3,$49,$B2,$C3,$B2,$52
                DEFB      $C3,$E8,$C3,$49,$B2,$C3
                DEFB  $02,$B2,$52,$C2,$C3,$B2,$C3
                DEFB      $B2,$C3,$C3,$49,$B3,$49
                DEFB      $B3,$49,$C2,$C3,$C2,$C3
                DEFB      $B3,$E8,$D3,$49,$C3,$49
                DEFB      $A3,$49,$D2,$C3,$C2,$C3
                DEFB      $A3,$E8,$D3,$49,$C3,$49
                DEFB  $03,$A4,$A5,$D2,$C3,$D2,$C3
                DEFB      $A5,$86,$E3,$49,$D3,$49
                DEFB  $02,$93,$E8,$E2,$C3,$D3,$49
                DEFB      $94,$A5,$E3,$49,$D3,$E8
                DEFB      $95,$86,$E2,$C3,$E3,$49
                DEFB      $96,$92,$F3,$49,$E3,$E8
                DEFB      $84,$A5,$F2,$C3,$E3,$49
                DEFB      $85,$86,$F3,$49,$E3,$E8
                DEFB  $03,$86,$92,$F2,$C3,$F3,$49
                DEFB      $83,$E8,$F3,$49,$F3,$E8
                DEFB  $FF  ; End of Pattern

PAT12:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$3B,$83,$B0,$94,$E7
                DEFB      $81,$3B,$83,$7B,$94,$A5
                DEFB      $F1,$76,$83,$49,$94,$63
                DEFB      $F1,$D8,$83,$1A,$93,$E8
                DEFB      $E1,$76,$92,$ED,$94,$23
                DEFB      $E1,$D8,$92,$ED,$93,$B0
                DEFB  $03,$E1,$76,$92,$ED,$93,$B0
                DEFB      $E1,$D8,$92,$ED,$93,$B0
                DEFB  $02,$81,$3B,$83,$B0,$95,$DB
                DEFB      $81,$3B,$83,$7B,$95,$86
                DEFB      $D1,$D8,$A3,$49,$A5,$37
                DEFB      $D2,$ED,$B3,$1A,$A4,$E7
                DEFB      $C1,$D8,$B2,$ED,$A4,$E7
                DEFB      $C2,$ED,$B2,$ED,$A4,$E7
                DEFB      $C1,$D8,$B2,$ED,$B4,$E7
                DEFB      $C2,$ED,$C2,$ED,$B4,$E7
                DEFB  $02,$B1,$D8,$C3,$B0,$B4,$E7
                DEFB      $B2,$ED,$C3,$7B,$B4,$A5
                DEFB      $B1,$D8,$C3,$49,$C4,$63
                DEFB      $B2,$ED,$D3,$1A,$C4,$23
                DEFB      $A1,$D8,$D2,$ED,$C3,$B0
                DEFB      $A2,$ED,$D2,$ED,$C3,$B0
                DEFB  $03,$A1,$D8,$D2,$ED,$D3,$B0
                DEFB      $A2,$ED,$E2,$ED,$D3,$B0
                DEFB  $02,$91,$D8,$E3,$B0,$D2,$76
                DEFB      $92,$ED,$E3,$7B,$D1,$3B
                DEFB      $91,$D8,$E3,$49,$E2,$76
                DEFB      $92,$ED,$F3,$1A,$E1,$3B
                DEFB      $81,$D8,$F2,$ED,$E2,$76
                DEFB      $82,$ED,$F2,$ED,$E1,$3B
                DEFB  $03,$81,$D8,$F2,$ED,$F2,$76
                DEFB      $82,$ED,$F2,$ED,$F1,$3B
                DEFB  $FF  ; End of Pattern

PAT13:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$29,$81,$BD,$92,$ED
                DEFB      $81,$29,$82,$52,$01
                DEFB      $F2,$52,$82,$ED,$01
                DEFB      $F1,$BD,$81,$BD,$01
                DEFB      $E1,$76,$92,$52,$01
                DEFB      $E1,$BD,$92,$ED,$01
                DEFB  $03,$E2,$52,$91,$BD,$01
                DEFB      $E1,$76,$92,$52,$01
                DEFB  $02,$81,$29,$A2,$ED,$01
                DEFB      $81,$29,$A1,$BD,$01
                DEFB      $D1,$76,$A2,$52,$01
                DEFB      $D1,$BD,$A2,$ED,$01
                DEFB      $C2,$52,$B1,$BD,$01
                DEFB      $C1,$76,$B2,$52,$01
                DEFB      $C1,$BD,$B2,$ED,$01
                DEFB      $C2,$52,$B1,$BD,$01
                DEFB  $02,$B1,$76,$C2,$52,$85,$DB
                DEFB      $B1,$BD,$C2,$ED,$84,$A5
                DEFB      $B2,$52,$C1,$BD,$93,$E8
                DEFB      $B1,$76,$C2,$52,$94,$A5
                DEFB      $A1,$BD,$D2,$ED,$A3,$E8
                DEFB      $A2,$52,$D1,$BD,$A2,$ED
                DEFB  $03,$A1,$76,$D2,$52,$B5,$DB
                DEFB      $A1,$BD,$D2,$ED,$B4,$A5
                DEFB  $02,$92,$52,$E1,$BD,$C3,$E8
                DEFB      $91,$76,$E2,$52,$C7,$D0
                DEFB      $91,$BD,$E2,$ED,$D5,$DB
                DEFB      $92,$52,$E1,$BD,$D4,$A5
                DEFB      $81,$76,$F2,$52,$E4,$A5
                DEFB      $81,$BD,$F2,$ED,$E3,$E8
                DEFB  $03,$82,$52,$F1,$BD,$F2,$ED
                DEFB      $81,$76,$F2,$52,$F5,$DB
                DEFB  $FF  ; End of Pattern

PAT14:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$29,$81,$BD,$97,$D0
                DEFB      $81,$29,$82,$52,$A3,$E8
                DEFB      $F2,$52,$82,$ED,$A5,$DB
                DEFB      $F1,$BD,$81,$BD,$B2,$ED
                DEFB      $E1,$76,$92,$52,$B4,$A5
                DEFB      $E1,$BD,$92,$ED,$C2,$52
                DEFB  $03,$E2,$52,$91,$BD,$C3,$E8
                DEFB      $E1,$76,$92,$52,$D1,$F4
                DEFB  $02,$81,$29,$A2,$ED,$D9,$4B
                DEFB      $81,$29,$A1,$BD,$E4,$A5
                DEFB      $D1,$76,$A2,$52,$E7,$D0
                DEFB      $D1,$BD,$A2,$ED,$F3,$E8
                DEFB      $C2,$52,$B1,$BD,$F5,$DB
                DEFB      $C1,$76,$B2,$52,$E2,$ED
                DEFB      $C1,$BD,$B2,$ED,$E4,$A5
                DEFB      $C2,$52,$B1,$BD,$D2,$52
                DEFB  $02,$B1,$76,$C2,$52,$8B,$B6
                DEFB      $B1,$BD,$C2,$ED,$85,$DB
                DEFB      $B2,$52,$C1,$BD,$99,$4B
                DEFB      $B1,$76,$C2,$52,$94,$A5
                DEFB      $A1,$BD,$D2,$ED,$A7,$D0
                DEFB      $A2,$52,$D1,$BD,$A3,$E8
                DEFB  $03,$A1,$76,$D2,$52,$B5,$DB
                DEFB      $A1,$BD,$D2,$ED,$B2,$ED
                DEFB  $02,$92,$52,$E1,$BD,$C4,$A5
                DEFB      $91,$76,$E2,$52,$C5,$DB
                DEFB      $91,$BD,$E2,$ED,$D7,$D0
                DEFB      $92,$52,$E1,$BD,$D9,$4B
                DEFB      $81,$76,$F2,$52,$EB,$B6
                DEFB      $81,$BD,$F2,$ED,$E9,$4B
                DEFB  $03,$82,$52,$F1,$BD,$F7,$D0
                DEFB      $81,$76,$F2,$52,$F5,$DB
                DEFB  $FF  ; End of Pattern

PAT15:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$3B,$83,$B0,$94,$E7
                DEFB      $81,$3B,$83,$7B,$94,$A5
                DEFB      $00    ,$00    ,$94,$63
                DEFB      $81,$D8,$83,$1A,$93,$E8
                DEFB  $02,$E1,$76,$92,$ED,$94,$23
                DEFB      $00    ,$00    ,$93,$B0
                DEFB      $E1,$76,$92,$ED,$93,$B0
                DEFB      $E1,$D8,$92,$ED,$93,$B0
                DEFB  $02,$81,$3B,$83,$B0,$95,$DB
                DEFB      $81,$3B,$83,$7B,$95,$86
                DEFB      $00    ,$00    ,$A5,$37
                DEFB      $82,$ED,$B3,$1A,$A4,$E7
                DEFB  $02,$C1,$D8,$B2,$ED,$A4,$E7
                DEFB      $00    ,$00    ,$A4,$E7
                DEFB      $C1,$D8,$B2,$ED,$B4,$E7
                DEFB      $C2,$ED,$C2,$ED,$B4,$E7
                DEFB  $02,$00    ,$00    ,$B4,$E7
                DEFB      $C2,$ED,$C3,$7B,$B4,$A5
                DEFB      $C1,$D8,$C3,$49,$C4,$63
                DEFB      $00    ,$00    ,$C4,$23
                DEFB  $02,$A1,$D8,$C2,$ED,$C3,$B0
                DEFB      $A2,$ED,$C2,$ED,$C3,$B0
                DEFB      $00    ,$00    ,$D3,$B0
                DEFB      $A2,$ED,$E2,$ED,$D3,$B0
                DEFB  $02,$91,$D8,$E3,$B0,$D2,$76
                DEFB      $00    ,$00    ,$D1,$3B
                DEFB      $91,$D8,$E3,$49,$E2,$76
                DEFB      $92,$ED,$F3,$1A,$E1,$3B
                DEFB      $00    ,$00    ,$E2,$76
                DEFB  $02,$92,$ED,$F2,$ED,$E1,$3B
                DEFB  $03,$91,$D8,$F2,$ED,$F2,$76
                DEFB      $92,$ED,$F2,$ED,$F1,$3B
                DEFB  $FF  ; End of Pattern

PAT16:
                DEFW  1850     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$81,$29,$81,$BD,$97,$D0
                DEFB      $01    ,$01    ,$01
                DEFB      $F2,$52,$82,$ED,$95,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $E1,$76,$92,$52,$94,$A5
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$E2,$52,$91,$BD,$93,$E8
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$81,$29,$A2,$ED,$99,$4B
                DEFB      $01    ,$01    ,$01
                DEFB      $D1,$76,$A2,$52,$97,$D0
                DEFB      $01    ,$01    ,$01
                DEFB  $07,$C2,$52,$B1,$BD,$95,$DB
                DEFB  $08,$01    ,$01    ,$01
                DEFB  $09,$C1,$BD,$B2,$ED,$94,$A5
                DEFB  $0A,$01    ,$01    ,$01
                DEFB  $02,$90,$7D,$E0,$FA,$C0,$FA
                DEFB      $01    ,$D0,$F6,$C0,$F7
                DEFB      $01    ,$D0,$F2,$D0,$F3
                DEFB      $01    ,$C0,$EE,$D0,$EF
                DEFB      $01    ,$C0,$EA,$E0,$EB
                DEFB      $01    ,$B0,$E6,$E0,$E7
                DEFB      $01    ,$B0,$E2,$F0,$E3
                DEFB      $01    ,$B0,$DE,$F0,$DF
                DEFB      $01    ,$B0,$D2,$F0,$D4
                DEFB      $01    ,$B0,$C8,$F0,$CA
                DEFB      $01    ,$B0,$BE,$F0,$C0
                DEFB      $01    ,$B0,$B4,$F0,$B6
                DEFB      $01    ,$B0,$AA,$F0,$AC
                DEFB      $01    ,$B0,$A0,$F0,$A2
                DEFB      $01    ,$B0,$96,$F0,$97
                DEFB      $00    ,$B0,$FA,$00
                DEFB  $FF  ; End of Pattern

PAT17:
                DEFW  2464     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$80,$BB,$F1,$74,$D2,$EF
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
                DEFB      $01    ,$01    ,$01
                DEFB  $03,$00    ,$00    ,$00
                DEFB      $E2,$ED,$F2,$31,$C3,$7B
                DEFB  $04,$00    ,$00    ,$00
                DEFB      $E2,$31,$F2,$ED,$C1,$BD
                DEFB  $05,$00    ,$00    ,$00
                DEFB      $E1,$BD,$F1,$18,$C0,$BB
                DEFB  $07,$00    ,$00    ,$00
                DEFB  $02,$E0,$B7,$81,$BA,$A1,$1C
                DEFB      $E0,$AB,$81,$AE,$A1,$28
                DEFB      $D0,$A7,$81,$AA,$A1,$2C
                DEFB      $D0,$9D,$91,$A0,$91,$36
                DEFB      $C0,$93,$91,$96,$91,$40
                DEFB      $C0,$89,$91,$8C,$91,$4A
                DEFB      $B0,$7F,$A1,$82,$81,$54
                DEFB      $B0,$75,$A1,$78,$81,$5E
                DEFB      $A0,$6B,$A1,$6E,$81,$68
                DEFB      $A0,$61,$B1,$64,$91,$72
                DEFB      $90,$57,$B1,$5A,$91,$7C
                DEFB      $90,$4D,$B1,$50,$91,$86
                DEFB  $FF  ; End of Pattern

PAT18:
                DEFW  3692     ; Pattern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$F0,$BB,$E3,$7D,$D2,$2F
                DEFB      $01    ,$01    ,$01
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
                DEFB      $00    ,$00    ,$01
                DEFB  $FF  ; End of Pattern



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


; =========================================
TFS:			; 247
SONG20:
; =========================================
                      DEFW      TFSPAT0
TFSLOOPSTART:            DEFW      TFSPAT19
                      DEFW      TFSPAT1
                      DEFW      TFSPAT20
                      DEFW      TFSPAT0
                      DEFW      TFSPAT19
                      DEFW      TFSPAT1
                      DEFW      TFSPAT20
                      DEFW      TFSPAT2
                      DEFW      TFSPAT19
                      DEFW      TFSPAT3
                      DEFW      TFSPAT22
                      DEFW      TFSPAT4
                      DEFW      TFSPAT23
                      DEFW      TFSPAT5
                      DEFW      TFSPAT24
                      DEFW      TFSPAT6
                      DEFW      TFSPAT25
                      DEFW      TFSPAT7
                      DEFW      TFSPAT26
                      DEFW      TFSPAT8
                      DEFW      TFSPAT27
                      DEFW      TFSPAT9
                      DEFW      TFSPAT28
                      DEFW      TFSPAT10
                      DEFW      TFSPAT29
                      DEFW      TFSPAT11
                      DEFW      TFSPAT30
                      DEFW      TFSPAT12
                      DEFW      TFSPAT31
                      DEFW      TFSPAT13
                      DEFW      TFSPAT32
                      DEFW      TFSPAT14
                      DEFW      TFSPAT33
                      DEFW      TFSPAT15
                      DEFW      TFSPAT34
                      DEFW      TFSPAT16
                      DEFW      TFSPAT35
                      DEFW      TFSPAT17
                      DEFW      TFSPAT36
                      DEFW      TFSPAT18
                      DEFW      TFSPAT19
                      DEFW      $0000
                      DEFW      TFSLOOPSTART

; *** TFSPATterns ***
TFSPAT0:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$00    ,$81,$76,$00
                DEFB      $01    ,$81,$3B,$01
                DEFB      $01    ,$81,$08,$01
                DEFB      $01    ,$80,$DE,$01
                DEFB      $01    ,$90,$9D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$9D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$01    ,$80,$9D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$9D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$01    ,$A0,$9D,$82,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$82,$ED
                DEFB      $01    ,$01    ,$83,$B0
                DEFB      $01    ,$B0,$9D,$92,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$92,$ED
                DEFB      $01    ,$01    ,$93,$B0
                DEFB      $A2,$76,$80,$BB,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $A2,$ED,$01    ,$01
                DEFB      $A3,$B0,$01    ,$01
                DEFB      $00    ,$90,$BB,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$01    ,$81,$76,$82,$76
                DEFB      $01    ,$81,$3B,$01
                DEFB      $01    ,$81,$08,$82,$ED
                DEFB      $01    ,$80,$DE,$83,$B0
                DEFB      $92,$76,$90,$D2,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $92,$ED,$01    ,$01
                DEFB      $93,$B0,$01    ,$01
                DEFB  $02,$00    ,$82,$ED,$01
                DEFB      $01    ,$82,$9B,$01
                DEFB      $01    ,$82,$52,$01
                DEFB      $01    ,$82,$11,$01
                DEFB      $01    ,$90,$DE,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$80,$D2,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$D2,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT1:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$01    ,$81,$76,$01
                DEFB      $01    ,$81,$3B,$01
                DEFB      $01    ,$81,$08,$01
                DEFB      $01    ,$80,$DE,$01
                DEFB      $01    ,$90,$9D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$9D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$01    ,$80,$9D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$9D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$01    ,$A0,$9D,$82,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$82,$ED
                DEFB      $01    ,$01    ,$83,$B0
                DEFB      $01    ,$B0,$9D,$92,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$92,$ED
                DEFB      $01    ,$01    ,$93,$B0
                DEFB      $A2,$76,$81,$3B,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $A2,$ED,$01    ,$01
                DEFB      $A3,$B0,$01    ,$01
                DEFB      $00    ,$91,$3B,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$01    ,$81,$76,$82,$76
                DEFB      $01    ,$81,$3B,$01
                DEFB      $01    ,$81,$08,$82,$ED
                DEFB      $01    ,$80,$DE,$83,$B0
                DEFB      $92,$76,$91,$18,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $92,$ED,$01    ,$01
                DEFB      $93,$B0,$01    ,$01
                DEFB  $02,$00    ,$82,$ED,$01
                DEFB      $01    ,$82,$9B,$01
                DEFB      $01    ,$82,$52,$01
                DEFB      $01    ,$82,$11,$01
                DEFB      $01    ,$90,$9D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$81,$18,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$91,$18,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT2:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$E2,$ED,$81,$76,$E3,$49
                DEFB      $01    ,$81,$3B,$E3,$7B
                DEFB      $01    ,$81,$08,$E3,$B0
                DEFB      $01    ,$80,$DE,$01
                DEFB      $D2,$ED,$90,$9D,$D3,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $C2,$ED,$A0,$9D,$C3,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$E2,$ED,$80,$9D,$E3,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $D2,$ED,$90,$9D,$D3,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$82,$76,$A0,$9D,$C3,$E8
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$ED,$01    ,$C3,$B0
                DEFB      $83,$B0,$01    ,$01
                DEFB      $92,$76,$B0,$9D,$B3,$7B
                DEFB      $01    ,$01    ,$01
                DEFB      $92,$ED,$01    ,$B3,$B0
                DEFB      $93,$B0,$01    ,$01
                DEFB      $A3,$1A,$80,$BB,$A3,$E8
                DEFB      $01    ,$01    ,$01
                DEFB      $A2,$ED,$01    ,$A3,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $92,$C3,$90,$BB,$93,$7B
                DEFB      $01    ,$01    ,$01
                DEFB      $92,$ED,$01    ,$93,$B0
                DEFB      $92,$C3,$01    ,$93,$7B
                DEFB  $05,$82,$76,$81,$76,$83,$49
                DEFB      $01    ,$81,$3B,$83,$1A
                DEFB      $82,$ED,$81,$08,$82,$ED
                DEFB      $83,$B0,$80,$DE,$82,$C3
                DEFB      $92,$11,$90,$D2,$92,$9B
                DEFB      $91,$F4,$01    ,$92,$76
                DEFB      $91,$D8,$01    ,$92,$52
                DEFB      $91,$BD,$01    ,$92,$31
                DEFB  $02,$A2,$11,$82,$ED,$00
                DEFB      $A1,$F4,$82,$9B,$01
                DEFB      $A1,$D8,$82,$52,$01
                DEFB      $A1,$BD,$82,$11,$01
                DEFB      $B1,$A4,$90,$DE,$01
                DEFB      $B1,$8D,$01    ,$01
                DEFB      $B1,$76,$01    ,$01
                DEFB      $B1,$61,$01    ,$01
                DEFB      $00    ,$80,$D2,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$D2,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT3:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$01    ,$81,$76,$01
                DEFB      $01    ,$81,$3B,$01
                DEFB      $01    ,$81,$08,$01
                DEFB      $01    ,$80,$DE,$01
                DEFB      $01    ,$90,$9D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$9D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$01    ,$80,$9D,$E4,$63
                DEFB      $01    ,$01    ,$E4,$A5
                DEFB      $01    ,$01    ,$E4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$9D,$D4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $E4,$63,$01    ,$01
                DEFB      $E4,$A5,$01    ,$01
                DEFB  $10,$82,$76,$A0,$9D,$C5,$37
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$ED,$01    ,$C4,$EC
                DEFB      $83,$B0,$01    ,$01
                DEFB      $92,$76,$B0,$9D,$B4,$A5
                DEFB      $01    ,$01    ,$01
                DEFB      $92,$ED,$01    ,$B4,$EC
                DEFB      $93,$B0,$01    ,$01
                DEFB      $C5,$37,$81,$3B,$E4,$63
                DEFB      $01    ,$01    ,$01
                DEFB      $B4,$A5,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $B4,$EC,$91,$3B,$D4,$63
                DEFB      $01    ,$01    ,$01
                DEFB      $E4,$63,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$82,$76,$81,$76,$E3,$B0
                DEFB      $01    ,$81,$3B,$01
                DEFB      $82,$ED,$81,$08,$01
                DEFB      $83,$B0,$80,$DE,$01
                DEFB      $D4,$63,$91,$18,$D3,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $E3,$B0,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$82,$ED,$E5,$86
                DEFB      $01    ,$82,$9B,$01
                DEFB      $D3,$B0,$82,$52,$E5,$DB
                DEFB      $01    ,$82,$11,$01
                DEFB      $01    ,$90,$9D,$D5,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $E5,$86,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $E5,$DB,$81,$18,$E4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $D5,$DB,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$91,$18,$D4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $E4,$EC,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT4:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$01    ,$81,$76,$E3,$49
                DEFB      $01    ,$81,$3B,$E3,$1A
                DEFB      $01    ,$81,$08,$E2,$ED
                DEFB      $01    ,$80,$DE,$01
                DEFB      $01    ,$90,$9D,$D2,$ED
                DEFB      $01    ,$01    ,$01
                DEFB      $E3,$49,$01    ,$01
                DEFB      $E3,$1A,$01    ,$01
                DEFB      $E2,$ED,$A0,$9D,$C3,$1A
                DEFB      $01    ,$01    ,$C2,$ED
                DEFB      $D2,$ED,$01    ,$C2,$C3
                DEFB      $01    ,$01    ,$C2,$ED
                DEFB      $01    ,$00    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $C3,$1A,$01    ,$01
                DEFB      $C2,$ED,$01    ,$01
                DEFB  $15,$C2,$C3,$80,$9D,$E3,$B0
                DEFB      $C2,$ED,$01    ,$E3,$7B
                DEFB      $00    ,$01    ,$E3,$49
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$9D,$D3,$49
                DEFB      $01    ,$01    ,$01
                DEFB      $E3,$B0,$01    ,$01
                DEFB      $E3,$7B,$01    ,$01
                DEFB  $10,$82,$76,$A0,$9D,$C3,$49
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$ED,$01    ,$01
                DEFB      $83,$B0,$01    ,$01
                DEFB      $92,$76,$B0,$9D,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $92,$ED,$01    ,$01
                DEFB      $93,$B0,$01    ,$01
                DEFB      $C3,$49,$80,$BB,$E3,$E8
                DEFB      $01    ,$01    ,$E3,$B0
                DEFB      $00    ,$01    ,$E3,$7B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$BB,$D3,$7B
                DEFB      $01    ,$01    ,$01
                DEFB      $E3,$E8,$01    ,$01
                DEFB      $E3,$B0,$01    ,$01
                DEFB  $05,$82,$76,$81,$76,$C3,$7B
                DEFB      $01    ,$81,$3B,$01
                DEFB      $82,$ED,$81,$08,$01
                DEFB      $83,$B0,$80,$DE,$01
                DEFB      $D3,$7B,$90,$D2,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $C3,$7B,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$82,$ED,$E3,$49
                DEFB      $01    ,$82,$9B,$E3,$7B
                DEFB      $00    ,$82,$52,$E3,$B0
                DEFB      $01    ,$82,$11,$01
                DEFB      $01    ,$90,$DE,$D3,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $E3,$49,$01    ,$01
                DEFB      $E3,$7B,$01    ,$01
                DEFB      $E3,$B0,$80,$D2,$C3,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $D3,$B0,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$D2,$B3,$E8
                DEFB      $01    ,$01    ,$01
                DEFB      $C3,$B0,$01    ,$B3,$B0
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT5:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$01    ,$81,$76,$01
                DEFB      $01    ,$81,$3B,$01
                DEFB      $01    ,$81,$08,$01
                DEFB      $01    ,$80,$DE,$01
                DEFB      $01    ,$90,$9D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$9D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$01    ,$80,$9D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$E3,$49
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$9D,$01
                DEFB      $01    ,$01    ,$E3,$7B
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$82,$76,$A0,$9D,$E3,$49
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$ED,$01    ,$01
                DEFB      $83,$B0,$01    ,$01
                DEFB      $92,$76,$B0,$9D,$D3,$49
                DEFB      $01    ,$01    ,$01
                DEFB      $92,$ED,$01    ,$01
                DEFB      $93,$B0,$01    ,$01
                DEFB      $D3,$49,$81,$3B,$C3,$49
                DEFB      $01    ,$01    ,$01
                DEFB      $C3,$49,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$91,$3B,$B3,$49
                DEFB      $01    ,$01    ,$01
                DEFB      $B3,$49,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$82,$76,$81,$76,$A3,$7B
                DEFB      $01    ,$81,$3B,$01
                DEFB      $82,$ED,$81,$08,$A3,$49
                DEFB      $83,$B0,$80,$DE,$01
                DEFB      $B3,$49,$91,$18,$93,$1A
                DEFB      $01    ,$01    ,$01
                DEFB      $A3,$7B,$01    ,$93,$49
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$A3,$49,$82,$ED,$83,$7B
                DEFB      $01    ,$82,$9B,$01
                DEFB      $93,$1A,$82,$52,$83,$49
                DEFB      $01    ,$82,$11,$01
                DEFB      $93,$49,$90,$9D,$93,$1A
                DEFB      $01    ,$01    ,$01
                DEFB      $83,$7B,$01    ,$93,$49
                DEFB      $01    ,$01    ,$01
                DEFB      $83,$49,$81,$18,$A3,$7B
                DEFB      $01    ,$01    ,$A3,$1A
                DEFB      $93,$1A,$01    ,$A2,$C3
                DEFB      $01    ,$01    ,$A2,$76
                DEFB      $93,$49,$91,$18,$B2,$31
                DEFB      $01    ,$01    ,$B1,$F4
                DEFB      $A3,$7B,$01    ,$B1,$BD
                DEFB      $A3,$1A,$01    ,$B1,$8D
                DEFB  $FF  ; End of TFSPATtern

TFSPAT6:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$01    ,$81,$76,$E2,$76
                DEFB      $01    ,$81,$3B,$01
                DEFB      $D2,$C3,$81,$08,$01
                DEFB      $01    ,$80,$DE,$01
                DEFB      $01    ,$90,$D2,$D2,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $E2,$76,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$D2,$C2,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $D2,$76,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$B2,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $C2,$76,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$01    ,$80,$D2,$A2,$9B
                DEFB      $01    ,$01    ,$01
                DEFB      $B2,$76,$01    ,$A2,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$D2,$92,$52
                DEFB      $01    ,$01    ,$01
                DEFB      $A2,$9B,$01    ,$92,$76
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$82,$76,$A0,$D2,$82,$9B
                DEFB      $01    ,$01    ,$01
                DEFB      $83,$49,$01    ,$82,$76
                DEFB      $83,$E8,$01    ,$01
                DEFB      $92,$76,$B0,$D2,$92,$52
                DEFB      $01    ,$01    ,$01
                DEFB      $93,$49,$01    ,$92,$76
                DEFB      $93,$E8,$01    ,$01
                DEFB      $82,$76,$80,$FA,$A2,$31
                DEFB      $01    ,$01    ,$A2,$11
                DEFB      $92,$52,$01    ,$A1,$F4
                DEFB      $01    ,$01    ,$A1,$D8
                DEFB      $92,$76,$90,$FA,$B1,$BD
                DEFB      $01    ,$01    ,$B1,$A4
                DEFB      $A2,$31,$01    ,$B1,$8D
                DEFB      $01    ,$01    ,$B1,$76
                DEFB  $05,$82,$76,$81,$76,$00
                DEFB      $01    ,$81,$3B,$01
                DEFB      $83,$49,$81,$08,$01
                DEFB      $83,$E8,$80,$DE,$01
                DEFB      $B1,$8D,$91,$18,$01
                DEFB      $B1,$76,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$82,$ED,$01
                DEFB      $01    ,$82,$9B,$01
                DEFB      $01    ,$82,$52,$01
                DEFB      $01    ,$82,$11,$01
                DEFB      $01    ,$91,$29,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$81,$18,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$91,$18,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT7:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$01    ,$81,$76,$E2,$ED
                DEFB      $01    ,$81,$3B,$E3,$1A
                DEFB      $01    ,$81,$08,$E3,$49
                DEFB      $01    ,$80,$DE,$01
                DEFB      $01    ,$90,$D2,$D3,$49
                DEFB      $01    ,$01    ,$01
                DEFB      $E2,$ED,$01    ,$01
                DEFB      $E3,$1A,$01    ,$01
                DEFB      $E3,$49,$A0,$D2,$C2,$ED
                DEFB      $01    ,$01    ,$C2,$C3
                DEFB      $D3,$49,$01    ,$C2,$9B
                DEFB      $01    ,$01    ,$C2,$76
                DEFB      $01    ,$00    ,$B2,$52
                DEFB      $01    ,$01    ,$B2,$31
                DEFB      $C2,$ED,$01    ,$B2,$11
                DEFB      $C2,$C3,$01    ,$B1,$F4
                DEFB  $15,$C2,$9B,$80,$D2,$E2,$ED
                DEFB      $C2,$76,$01    ,$01
                DEFB      $B2,$52,$01    ,$01
                DEFB      $B2,$31,$01    ,$01
                DEFB      $B2,$11,$90,$D2,$D2,$ED
                DEFB      $B1,$F4,$01    ,$01
                DEFB      $A2,$ED,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$82,$76,$A0,$D2,$E3,$49
                DEFB      $01    ,$01    ,$01
                DEFB      $83,$49,$01    ,$01
                DEFB      $83,$E8,$01    ,$01
                DEFB      $92,$76,$B0,$D2,$D3,$49
                DEFB      $01    ,$01    ,$01
                DEFB      $93,$49,$01    ,$01
                DEFB      $93,$E8,$01    ,$01
                DEFB      $E3,$49,$81,$A4,$E3,$E8
                DEFB      $01    ,$01    ,$01
                DEFB      $D3,$49,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$91,$A4,$D3,$E8
                DEFB      $01    ,$01    ,$01
                DEFB      $E3,$E8,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$82,$76,$81,$76,$E3,$49
                DEFB      $01    ,$81,$3B,$01
                DEFB      $83,$49,$81,$08,$01
                DEFB      $83,$E8,$80,$DE,$01
                DEFB      $D3,$E8,$91,$76,$D3,$49
                DEFB      $01    ,$01    ,$01
                DEFB      $E3,$49,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$82,$ED,$E3,$E8
                DEFB      $01    ,$82,$9B,$E4,$23
                DEFB      $D3,$49,$82,$52,$E4,$63
                DEFB      $01    ,$82,$11,$01
                DEFB      $01    ,$90,$D2,$D4,$63
                DEFB      $01    ,$01    ,$01
                DEFB      $E3,$E8,$01    ,$01
                DEFB      $E4,$23,$01    ,$01
                DEFB      $E4,$63,$81,$76,$C3,$E8
                DEFB      $01    ,$01    ,$C3,$B0
                DEFB      $D4,$63,$01    ,$C3,$7B
                DEFB      $01    ,$01    ,$C3,$49
                DEFB      $01    ,$91,$76,$B3,$1A
                DEFB      $01    ,$01    ,$B2,$ED
                DEFB      $C3,$E8,$01    ,$B2,$C3
                DEFB      $C3,$B0,$01    ,$B2,$9B
                DEFB  $FF  ; End of TFSPATtern

TFSPAT8:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$01    ,$81,$76,$E4,$63
                DEFB      $01    ,$81,$3B,$E4,$A5
                DEFB      $D3,$E8,$81,$08,$E4,$EC
                DEFB      $01    ,$80,$DE,$01
                DEFB      $01    ,$90,$D2,$D4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $E4,$63,$01    ,$01
                DEFB      $E4,$A5,$01    ,$00
                DEFB      $E4,$EC,$A0,$D2,$E4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $D4,$EC,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$D4,$EC
                DEFB      $00    ,$01    ,$01
                DEFB      $E4,$EC,$01    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$D4,$EC,$80,$D2,$E4,$63
                DEFB      $01    ,$01    ,$E4,$A5
                DEFB      $01    ,$01    ,$E4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$90,$D2,$D4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $E4,$63,$01    ,$01
                DEFB      $E4,$A5,$01    ,$00
                DEFB  $10,$82,$76,$A0,$D2,$E4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $83,$49,$01    ,$01
                DEFB      $83,$E8,$01    ,$01
                DEFB      $92,$76,$B0,$D2,$D4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $93,$49,$01    ,$00
                DEFB      $93,$E8,$01    ,$01
                DEFB      $E4,$EC,$80,$FA,$E4,$63
                DEFB      $01    ,$01    ,$E4,$A5
                DEFB      $D4,$EC,$01    ,$E4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$90,$FA,$D4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $E4,$63,$01    ,$01
                DEFB      $E4,$A5,$01    ,$00
                DEFB  $05,$82,$76,$81,$76,$E4,$EC
                DEFB      $01    ,$81,$3B,$01
                DEFB      $83,$49,$81,$08,$01
                DEFB      $83,$E8,$80,$DE,$01
                DEFB      $E4,$EC,$91,$18,$D4,$EC
                DEFB      $00    ,$01    ,$01
                DEFB      $E4,$EC,$01    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$82,$ED,$E4,$63
                DEFB      $01    ,$82,$9B,$E4,$A5
                DEFB      $D4,$EC,$82,$52,$E4,$EC
                DEFB      $01    ,$82,$11,$01
                DEFB      $00    ,$91,$29,$D4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $E4,$63,$01    ,$01
                DEFB      $E4,$A5,$01    ,$00
                DEFB      $E4,$EC,$81,$18,$E4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $D4,$EC,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$91,$18,$D4,$EC
                DEFB      $00    ,$01    ,$01
                DEFB      $E4,$EC,$01    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT9:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$E4,$EC,$81,$76,$E5,$DB
                DEFB      $01    ,$81,$3B,$01
                DEFB      $01    ,$81,$08,$01
                DEFB      $01    ,$80,$DE,$E4,$EC
                DEFB      $D4,$EC,$90,$D2,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $E5,$DB,$01    ,$E4,$63
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$D2,$E5,$DB
                DEFB      $E4,$EC,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$E4,$EC
                DEFB      $E4,$63,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $E5,$DB,$01    ,$E4,$63
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$01    ,$80,$D2,$E5,$DB
                DEFB      $E4,$EC,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$E4,$EC
                DEFB      $E4,$63,$90,$D2,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $E5,$DB,$01    ,$E4,$63
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$82,$76,$A0,$D2,$E5,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $83,$49,$01    ,$01
                DEFB      $83,$E8,$01    ,$E4,$EC
                DEFB      $92,$76,$B0,$D2,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $93,$49,$01    ,$E4,$63
                DEFB      $93,$E8,$01    ,$01
                DEFB      $E5,$DB,$81,$A4,$E5,$DB
                DEFB      $E4,$EC,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$E4,$EC
                DEFB      $E4,$63,$91,$A4,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $E5,$DB,$01    ,$E4,$63
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$82,$76,$81,$76,$E5,$DB
                DEFB      $01    ,$81,$3B,$01
                DEFB      $83,$49,$81,$08,$01
                DEFB      $83,$E8,$80,$DE,$E4,$EC
                DEFB      $E4,$63,$91,$76,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $E5,$DB,$01    ,$E4,$63
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$82,$ED,$E5,$DB
                DEFB      $E4,$EC,$82,$9B,$01
                DEFB      $01    ,$82,$52,$01
                DEFB      $01    ,$82,$11,$E4,$EC
                DEFB      $E4,$63,$90,$D2,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $E5,$DB,$01    ,$E4,$63
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$81,$76,$E5,$DB
                DEFB      $E4,$EC,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$E4,$EC
                DEFB      $E4,$63,$91,$76,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $E5,$DB,$01    ,$E4,$63
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT10:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$01    ,$81,$76,$E6,$92
                DEFB      $01    ,$81,$3B,$E6,$F6
                DEFB      $01    ,$81,$08,$E7,$60
                DEFB      $01    ,$80,$DE,$01
                DEFB      $01    ,$90,$9D,$D7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $E6,$92,$01    ,$01
                DEFB      $E6,$F6,$01    ,$01
                DEFB      $E7,$60,$A0,$9D,$C7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $D7,$60,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$B7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $C7,$60,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$01    ,$80,$9D,$A7,$D0
                DEFB      $01    ,$01    ,$01
                DEFB      $B7,$60,$01    ,$A7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$9D,$96,$F6
                DEFB      $01    ,$01    ,$01
                DEFB      $A7,$D0,$01    ,$97,$60
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$82,$76,$A0,$9D,$87,$D0
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$ED,$01    ,$87,$60
                DEFB      $83,$B0,$01    ,$01
                DEFB      $92,$76,$B0,$9D,$96,$F6
                DEFB      $01    ,$01    ,$01
                DEFB      $92,$ED,$01    ,$97,$60
                DEFB      $93,$B0,$01    ,$01
                DEFB      $87,$60,$80,$BB,$A7,$D0
                DEFB      $01    ,$01    ,$01
                DEFB      $96,$F6,$01    ,$A7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $97,$60,$90,$BB,$B6,$F6
                DEFB      $01    ,$01    ,$01
                DEFB      $A7,$D0,$01    ,$B7,$60
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$82,$76,$81,$76,$C6,$92
                DEFB      $01    ,$81,$3B,$C6,$34
                DEFB      $82,$ED,$81,$08,$C5,$DB
                DEFB      $83,$B0,$80,$DE,$C5,$86
                DEFB      $B7,$60,$90,$D2,$D5,$37
                DEFB      $B6,$F6,$01    ,$D4,$EC
                DEFB      $C6,$92,$01    ,$D4,$A5
                DEFB      $C6,$34,$01    ,$D4,$63
                DEFB  $02,$E4,$23,$82,$ED,$00
                DEFB      $E3,$E8,$82,$9B,$01
                DEFB      $E3,$B0,$82,$52,$01
                DEFB      $E3,$7B,$82,$11,$01
                DEFB      $D3,$49,$90,$DE,$01
                DEFB      $D3,$1A,$01    ,$01
                DEFB      $D2,$ED,$01    ,$01
                DEFB      $D2,$C3,$01    ,$01
                DEFB      $C2,$9B,$80,$D2,$01
                DEFB      $C2,$76,$01    ,$01
                DEFB      $C2,$52,$01    ,$01
                DEFB      $C2,$31,$01    ,$01
                DEFB      $B2,$11,$90,$D2,$01
                DEFB      $B1,$F4,$01    ,$01
                DEFB      $B1,$D8,$01    ,$01
                DEFB      $B1,$BD,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT11:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$E7,$60,$81,$76,$E7,$D0
                DEFB      $01    ,$81,$3B,$E8,$47
                DEFB      $D7,$60,$81,$08,$E8,$C6
                DEFB      $01    ,$80,$DE,$01
                DEFB      $01    ,$90,$9D,$D8,$C6
                DEFB      $01    ,$01    ,$01
                DEFB      $E7,$D0,$01    ,$01
                DEFB      $E8,$47,$01    ,$01
                DEFB      $E8,$C6,$A0,$9D,$E8,$C6
                DEFB      $01    ,$01    ,$E8,$47
                DEFB      $D8,$C6,$01    ,$E7,$D0
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$D7,$D0
                DEFB      $01    ,$01    ,$01
                DEFB      $E8,$C6,$01    ,$01
                DEFB      $E8,$47,$01    ,$01
                DEFB  $15,$E7,$D0,$80,$9D,$E6,$92
                DEFB      $01    ,$01    ,$E5,$DB
                DEFB      $D7,$D0,$01    ,$E4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$9D,$D4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $E6,$92,$01    ,$01
                DEFB      $E5,$DB,$01    ,$01
                DEFB  $10,$82,$76,$A0,$9D,$E5,$DB
                DEFB      $01    ,$01    ,$E6,$92
                DEFB      $82,$ED,$01    ,$E7,$D0
                DEFB      $83,$B0,$01    ,$01
                DEFB      $92,$76,$B0,$9D,$D7,$D0
                DEFB      $01    ,$01    ,$01
                DEFB      $92,$ED,$01    ,$01
                DEFB      $93,$B0,$01    ,$01
                DEFB      $E7,$D0,$81,$3B,$E7,$D0
                DEFB      $01    ,$01    ,$E7,$60
                DEFB      $D7,$D0,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$91,$3B,$D7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $E7,$D0,$01    ,$01
                DEFB      $E7,$60,$01    ,$01
                DEFB  $05,$82,$76,$81,$76,$E5,$DB
                DEFB      $01    ,$81,$3B,$E4,$EC
                DEFB      $82,$ED,$81,$08,$01
                DEFB      $83,$B0,$80,$DE,$01
                DEFB      $E7,$60,$91,$18,$D4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $E5,$DB,$01    ,$01
                DEFB      $E4,$EC,$01    ,$01
                DEFB  $02,$01    ,$82,$ED,$E4,$EC
                DEFB      $01    ,$82,$9B,$E5,$DB
                DEFB      $D4,$EC,$82,$52,$E6,$92
                DEFB      $01    ,$82,$11,$E6,$F6
                DEFB      $01    ,$90,$9D,$D7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $E4,$EC,$01    ,$01
                DEFB      $E5,$DB,$01    ,$01
                DEFB      $E6,$92,$81,$18,$C7,$D0
                DEFB      $E6,$F6,$01    ,$01
                DEFB      $D7,$60,$01    ,$C7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$91,$18,$B6,$F6
                DEFB      $01    ,$01    ,$01
                DEFB      $C7,$D0,$01    ,$B7,$60
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT12:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$01    ,$81,$76,$E5,$86
                DEFB      $01    ,$81,$3B,$01
                DEFB      $D5,$DB,$81,$08,$01
                DEFB      $01    ,$80,$DE,$01
                DEFB      $01    ,$90,$9D,$D5,$86
                DEFB      $01    ,$01    ,$01
                DEFB      $E5,$86,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$9D,$C5,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $D5,$86,$01    ,$C5,$86
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$B5,$37
                DEFB      $01    ,$01    ,$01
                DEFB      $C5,$DB,$01    ,$B5,$86
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$C5,$86,$80,$9D,$A5,$DB
                DEFB      $01    ,$01    ,$A5,$86
                DEFB      $B5,$37,$01    ,$A5,$37
                DEFB      $01    ,$01    ,$A4,$EC
                DEFB      $B5,$86,$90,$9D,$94,$A5
                DEFB      $01    ,$01    ,$94,$63
                DEFB      $A5,$DB,$01    ,$94,$23
                DEFB      $A5,$86,$01    ,$93,$E8
                DEFB  $10,$82,$76,$A0,$9D,$E5,$86
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$ED,$01    ,$01
                DEFB      $83,$B0,$01    ,$01
                DEFB      $92,$76,$B0,$9D,$D5,$86
                DEFB      $01    ,$01    ,$01
                DEFB      $92,$ED,$01    ,$01
                DEFB      $93,$B0,$01    ,$01
                DEFB      $E5,$86,$80,$BB,$E5,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $D5,$86,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$BB,$D5,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $E5,$DB,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$82,$76,$81,$76,$E6,$92
                DEFB      $01    ,$81,$3B,$01
                DEFB      $82,$ED,$81,$08,$01
                DEFB      $83,$B0,$80,$DE,$01
                DEFB      $D5,$DB,$90,$D2,$D6,$92
                DEFB      $01    ,$01    ,$01
                DEFB      $E6,$92,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$82,$ED,$E6,$92
                DEFB      $01    ,$82,$9B,$E6,$F6
                DEFB      $D6,$92,$82,$52,$E7,$60
                DEFB      $01    ,$82,$11,$01
                DEFB      $01    ,$90,$DE,$D7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $E6,$92,$01    ,$01
                DEFB      $E6,$F6,$01    ,$01
                DEFB      $E7,$60,$80,$D2,$E5,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $D7,$60,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$D2,$D5,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $E5,$DB,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT13:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$01    ,$81,$76,$E7,$D0
                DEFB      $01    ,$81,$3B,$E8,$47
                DEFB      $01    ,$81,$08,$E8,$C6
                DEFB      $01    ,$80,$DE,$01
                DEFB      $01    ,$90,$9D,$D8,$C6
                DEFB      $01    ,$01    ,$01
                DEFB      $E7,$D0,$01    ,$01
                DEFB      $E8,$47,$01    ,$01
                DEFB      $E8,$C6,$A0,$9D,$C8,$C6
                DEFB      $01    ,$01    ,$01
                DEFB      $D8,$C6,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$B8,$C6
                DEFB      $01    ,$01    ,$01
                DEFB      $C8,$C6,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$01    ,$80,$9D,$A9,$4B
                DEFB      $01    ,$01    ,$01
                DEFB      $B8,$C6,$01    ,$A8,$C6
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$9D,$98,$47
                DEFB      $01    ,$01    ,$01
                DEFB      $A9,$4B,$01    ,$98,$C6
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$82,$76,$A0,$9D,$89,$4B
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$ED,$01    ,$88,$C6
                DEFB      $83,$B0,$01    ,$01
                DEFB      $92,$76,$B0,$9D,$98,$47
                DEFB      $01    ,$01    ,$01
                DEFB      $92,$ED,$01    ,$98,$C6
                DEFB      $93,$B0,$01    ,$01
                DEFB      $88,$C6,$81,$3B,$A9,$4B
                DEFB      $01    ,$01    ,$01
                DEFB      $98,$47,$01    ,$A8,$C6
                DEFB      $01    ,$01    ,$01
                DEFB      $98,$C6,$91,$3B,$B8,$47
                DEFB      $01    ,$01    ,$01
                DEFB      $A9,$4B,$01    ,$B8,$C6
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$82,$76,$81,$76,$C9,$4B
                DEFB      $01    ,$81,$3B,$01
                DEFB      $82,$ED,$81,$08,$C8,$C6
                DEFB      $83,$B0,$80,$DE,$01
                DEFB      $B8,$C6,$91,$18,$D8,$47
                DEFB      $01    ,$01    ,$01
                DEFB      $C9,$4B,$01    ,$D8,$C6
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$C8,$C6,$82,$ED,$E9,$4B
                DEFB      $01    ,$82,$9B,$01
                DEFB      $D8,$47,$82,$52,$E8,$C6
                DEFB      $01    ,$82,$11,$01
                DEFB      $D8,$C6,$90,$9D,$F8,$47
                DEFB      $01    ,$01    ,$01
                DEFB      $E9,$4B,$01    ,$F8,$C6
                DEFB      $01    ,$01    ,$01
                DEFB      $E8,$C6,$81,$18,$E9,$4B
                DEFB      $01    ,$01    ,$01
                DEFB      $F8,$47,$01    ,$E8,$C6
                DEFB      $01    ,$01    ,$01
                DEFB      $F8,$C6,$91,$18,$D8,$47
                DEFB      $01    ,$01    ,$01
                DEFB      $E9,$4B,$01    ,$D8,$C6
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT14:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$A6,$F6,$81,$76,$E9,$D9
                DEFB      $A7,$60,$81,$3B,$01
                DEFB      $B7,$D0,$81,$08,$01
                DEFB      $B8,$47,$80,$DE,$01
                DEFB      $B8,$C6,$90,$D2,$D9,$D9
                DEFB      $B9,$4B,$01    ,$01
                DEFB      $E9,$D9,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$D2,$C9,$D9
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$B9,$D9
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$01    ,$80,$D2,$AA,$6E
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$A9,$D9
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$D2,$99,$4B
                DEFB      $01    ,$01    ,$01
                DEFB      $EA,$6E,$01    ,$99,$D9
                DEFB      $01    ,$01    ,$99,$4B
                DEFB  $10,$82,$76,$A0,$D2,$88,$C6
                DEFB      $01    ,$01    ,$88,$47
                DEFB      $83,$49,$01    ,$87,$D0
                DEFB      $83,$E8,$01    ,$87,$60
                DEFB      $92,$76,$B0,$D2,$96,$F6
                DEFB      $01    ,$01    ,$96,$92
                DEFB      $93,$49,$01    ,$96,$34
                DEFB      $93,$E8,$01    ,$95,$DB
                DEFB      $E9,$D9,$80,$FA,$E8,$C6
                DEFB      $01    ,$01    ,$01
                DEFB      $E9,$4B,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $E9,$D9,$90,$FA,$D8,$C6
                DEFB      $E9,$4B,$01    ,$01
                DEFB      $E8,$C6,$01    ,$01
                DEFB      $E8,$47,$01    ,$01
                DEFB  $05,$82,$76,$81,$76,$E9,$D9
                DEFB      $01    ,$81,$3B,$01
                DEFB      $83,$49,$81,$08,$01
                DEFB      $83,$E8,$80,$DE,$01
                DEFB      $E6,$34,$91,$18,$D9,$D9
                DEFB      $E5,$DB,$01    ,$01
                DEFB      $E9,$D9,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$82,$ED,$EB,$0D
                DEFB      $01    ,$82,$9B,$01
                DEFB      $D9,$D9,$82,$52,$EB,$B6
                DEFB      $01    ,$82,$11,$01
                DEFB      $01    ,$91,$29,$DB,$B6
                DEFB      $01    ,$01    ,$01
                DEFB      $EB,$0D,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $EB,$B6,$81,$18,$EB,$0D
                DEFB      $01    ,$01    ,$01
                DEFB      $DB,$B6,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$91,$18,$DB,$0D
                DEFB      $01    ,$01    ,$01
                DEFB      $EB,$0D,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT15:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$01    ,$81,$76,$E6,$92
                DEFB      $01    ,$81,$3B,$01
                DEFB      $01    ,$81,$08,$01
                DEFB      $01    ,$80,$DE,$01
                DEFB      $01    ,$90,$D2,$D6,$92
                DEFB      $01    ,$01    ,$01
                DEFB      $E6,$92,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$D2,$E7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $D6,$92,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$D7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $E7,$60,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$01    ,$80,$D2,$E4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $D7,$60,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$D2,$D4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $E4,$EC,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$82,$76,$A0,$D2,$E5,$86
                DEFB      $01    ,$01    ,$01
                DEFB      $83,$49,$01    ,$E5,$DB
                DEFB      $83,$E8,$01    ,$01
                DEFB      $92,$76,$B0,$D2,$D5,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $93,$49,$01    ,$01
                DEFB      $93,$E8,$01    ,$01
                DEFB      $E5,$DB,$81,$A4,$E5,$86
                DEFB      $01    ,$01    ,$01
                DEFB      $D5,$DB,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$91,$A4,$D5,$86
                DEFB      $01    ,$01    ,$01
                DEFB      $E5,$86,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$82,$76,$81,$76,$E4,$63
                DEFB      $01    ,$81,$3B,$01
                DEFB      $83,$49,$81,$08,$01
                DEFB      $83,$E8,$80,$DE,$01
                DEFB      $D5,$86,$91,$76,$D4,$63
                DEFB      $01    ,$01    ,$01
                DEFB      $E4,$63,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$82,$ED,$E4,$EC
                DEFB      $01    ,$82,$9B,$01
                DEFB      $D4,$63,$82,$52,$01
                DEFB      $01    ,$82,$11,$01
                DEFB      $01    ,$90,$D2,$D4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $E4,$EC,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$81,$76,$C5,$37
                DEFB      $01    ,$01    ,$01
                DEFB      $D4,$EC,$01    ,$C4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$91,$76,$B4,$A5
                DEFB      $01    ,$01    ,$01
                DEFB      $C5,$37,$01    ,$B4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT16:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$E3,$49,$81,$76,$E5,$DB
                DEFB      $01    ,$81,$3B,$EB,$B6
                DEFB      $E3,$E8,$81,$08,$E5,$DB
                DEFB      $01    ,$80,$DE,$01
                DEFB      $E4,$EC,$90,$D2,$E4,$EC
                DEFB      $01    ,$01    ,$E9,$D9
                DEFB      $E5,$DB,$01    ,$E4,$EC
                DEFB      $EB,$B6,$01    ,$01
                DEFB      $E5,$DB,$A0,$D2,$E5,$DB
                DEFB      $01    ,$01    ,$EB,$B6
                DEFB      $E4,$EC,$01    ,$E5,$DB
                DEFB      $E9,$D9,$01    ,$01
                DEFB      $E4,$EC,$00    ,$E4,$EC
                DEFB      $01    ,$01    ,$E9,$D9
                DEFB      $E5,$DB,$01    ,$E4,$EC
                DEFB      $EB,$B6,$01    ,$01
                DEFB  $15,$E5,$DB,$80,$D2,$E5,$DB
                DEFB      $01    ,$01    ,$EB,$B6
                DEFB      $E4,$EC,$01    ,$E5,$DB
                DEFB      $E9,$D9,$01    ,$01
                DEFB      $E4,$EC,$90,$D2,$E4,$EC
                DEFB      $01    ,$01    ,$E9,$D9
                DEFB      $E5,$DB,$01    ,$E4,$EC
                DEFB      $EB,$B6,$01    ,$01
                DEFB  $10,$82,$76,$A0,$D2,$E5,$DB
                DEFB      $01    ,$01    ,$EB,$B6
                DEFB      $83,$49,$01    ,$E5,$DB
                DEFB      $83,$E8,$01    ,$01
                DEFB      $92,$76,$B0,$D2,$E4,$EC
                DEFB      $01    ,$01    ,$E9,$D9
                DEFB      $93,$49,$01    ,$E4,$EC
                DEFB      $93,$E8,$01    ,$01
                DEFB      $E5,$DB,$80,$FA,$E5,$DB
                DEFB      $01    ,$01    ,$EB,$B6
                DEFB      $E4,$EC,$01    ,$E5,$DB
                DEFB      $E9,$D9,$01    ,$01
                DEFB      $E4,$EC,$90,$FA,$E4,$EC
                DEFB      $01    ,$01    ,$E9,$D9
                DEFB      $E5,$DB,$01    ,$E4,$EC
                DEFB      $EB,$B6,$01    ,$01
                DEFB  $05,$82,$76,$81,$76,$E5,$DB
                DEFB      $01    ,$81,$3B,$EB,$B6
                DEFB      $83,$49,$81,$08,$E5,$DB
                DEFB      $83,$E8,$80,$DE,$01
                DEFB      $E4,$EC,$91,$18,$E4,$EC
                DEFB      $01    ,$01    ,$E9,$D9
                DEFB      $E5,$DB,$01    ,$E4,$EC
                DEFB      $EB,$B6,$01    ,$01
                DEFB  $02,$E5,$DB,$82,$ED,$E6,$92
                DEFB      $01    ,$82,$9B,$ED,$25
                DEFB      $E4,$EC,$82,$52,$E6,$92
                DEFB      $E9,$D9,$82,$11,$01
                DEFB      $E4,$EC,$91,$29,$E5,$DB
                DEFB      $01    ,$01    ,$EB,$B6
                DEFB      $E6,$92,$01    ,$E5,$DB
                DEFB      $ED,$25,$01    ,$01
                DEFB      $E6,$92,$81,$18,$E4,$EC
                DEFB      $01    ,$01    ,$E9,$D9
                DEFB      $E5,$DB,$01    ,$E4,$EC
                DEFB      $EB,$B6,$01    ,$01
                DEFB      $E5,$DB,$91,$18,$E6,$92
                DEFB      $01    ,$01    ,$ED,$25
                DEFB      $E4,$EC,$01    ,$E6,$92
                DEFB      $E9,$D9,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT17:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$01    ,$81,$76,$E5,$86
                DEFB      $01    ,$81,$3B,$01
                DEFB      $D6,$92,$81,$08,$01
                DEFB      $01    ,$80,$DE,$01
                DEFB      $01    ,$90,$8C,$D5,$86
                DEFB      $01    ,$01    ,$01
                DEFB      $E5,$86,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$8C,$C5,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $D5,$86,$01    ,$C5,$86
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$B5,$37
                DEFB      $01    ,$01    ,$01
                DEFB      $C5,$DB,$01    ,$B5,$86
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$C5,$86,$80,$8C,$A5,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $B5,$37,$01    ,$A5,$86
                DEFB      $01    ,$01    ,$01
                DEFB      $B5,$86,$90,$8C,$95,$37
                DEFB      $01    ,$01    ,$01
                DEFB      $A5,$DB,$01    ,$95,$86
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$82,$C3,$80,$8C,$85,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $83,$49,$01    ,$85,$86
                DEFB      $84,$63,$01    ,$01
                DEFB      $92,$C3,$90,$8C,$95,$37
                DEFB      $01    ,$01    ,$01
                DEFB      $93,$49,$01    ,$95,$86
                DEFB      $94,$63,$01    ,$01
                DEFB      $85,$86,$80,$D2,$A5,$DB
                DEFB      $01    ,$01    ,$A5,$86
                DEFB      $95,$37,$01    ,$B5,$37
                DEFB      $01    ,$01    ,$B4,$EC
                DEFB      $95,$86,$90,$D2,$C4,$A5
                DEFB      $01    ,$01    ,$C4,$63
                DEFB      $A5,$DB,$01    ,$D4,$23
                DEFB      $A5,$86,$01    ,$D3,$E8
                DEFB  $05,$82,$C3,$81,$76,$E5,$DB
                DEFB      $01    ,$81,$3B,$01
                DEFB      $83,$49,$81,$08,$01
                DEFB      $84,$63,$80,$DE,$01
                DEFB      $D4,$23,$91,$18,$D5,$DB
                DEFB      $D3,$E8,$01    ,$01
                DEFB      $E5,$DB,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $02,$01    ,$82,$ED,$E5,$DB
                DEFB      $01    ,$82,$9B,$E6,$34
                DEFB      $D5,$DB,$82,$52,$E6,$92
                DEFB      $01    ,$82,$11,$01
                DEFB      $01    ,$90,$8C,$D6,$92
                DEFB      $01    ,$01    ,$01
                DEFB      $E5,$DB,$01    ,$01
                DEFB      $E6,$34,$01    ,$01
                DEFB      $E6,$92,$80,$D2,$C6,$92
                DEFB      $01    ,$01    ,$01
                DEFB      $D6,$92,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$D2,$B6,$92
                DEFB      $01    ,$01    ,$01
                DEFB      $C6,$92,$01    ,$B6,$F6
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT18:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $05,$C3,$B0,$81,$76,$00
                DEFB      $C3,$7B,$81,$3B,$01
                DEFB      $D3,$49,$81,$08,$01
                DEFB      $D3,$1A,$80,$DE,$01
                DEFB      $D2,$ED,$90,$9D,$01
                DEFB      $D1,$61,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$A0,$9D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$00    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$01    ,$80,$9D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$9D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$01    ,$A0,$9D,$82,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$82,$ED
                DEFB      $01    ,$01    ,$83,$B0
                DEFB      $01    ,$B0,$9D,$92,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$92,$ED
                DEFB      $01    ,$01    ,$93,$B0
                DEFB      $A2,$76,$80,$BB,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $A2,$ED,$01    ,$01
                DEFB      $A3,$B0,$01    ,$01
                DEFB      $00    ,$90,$BB,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $05,$01    ,$81,$76,$82,$76
                DEFB      $01    ,$81,$3B,$01
                DEFB      $01    ,$81,$08,$82,$ED
                DEFB      $01    ,$80,$DE,$83,$B0
                DEFB      $92,$76,$90,$D2,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $92,$ED,$01    ,$01
                DEFB      $93,$B0,$01    ,$01
                DEFB  $02,$00    ,$82,$ED,$01
                DEFB      $01    ,$82,$9B,$01
                DEFB      $01    ,$82,$52,$01
                DEFB      $01    ,$82,$11,$01
                DEFB      $01    ,$90,$DE,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$80,$D2,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$D2,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT19:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $15,$01    ,$80,$BB,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$BB,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$01    ,$80,$D2,$82,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$82,$ED
                DEFB      $01    ,$01    ,$83,$B0
                DEFB      $01    ,$90,$D2,$92,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$92,$ED
                DEFB      $01    ,$01    ,$93,$B0
                DEFB      $A2,$76,$80,$8C,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $A2,$ED,$01    ,$01
                DEFB      $A3,$B0,$01    ,$01
                DEFB      $B2,$76,$90,$8C,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $B2,$ED,$01    ,$01
                DEFB      $B3,$B0,$01    ,$01
                DEFB  $15,$00    ,$80,$94,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$94,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT20:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $15,$01    ,$80,$FA,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$FA,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$01    ,$80,$9D,$82,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$82,$ED
                DEFB      $01    ,$01    ,$83,$B0
                DEFB      $01    ,$90,$9D,$92,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$92,$ED
                DEFB      $01    ,$01    ,$93,$B0
                DEFB      $A2,$76,$80,$FA,$00
                DEFB      $01    ,$01    ,$01
                DEFB      $A2,$ED,$01    ,$01
                DEFB      $A3,$B0,$01    ,$01
                DEFB      $B2,$76,$90,$FA,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $B2,$ED,$01    ,$01
                DEFB      $B3,$B0,$01    ,$01
                DEFB  $15,$00    ,$80,$EC,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$EC,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT22:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $15,$01    ,$80,$FA,$E3,$7B
                DEFB      $01    ,$01    ,$01
                DEFB      $D4,$EC,$01    ,$E3,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$FA,$D3,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $E3,$7B,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$82,$76,$80,$9D,$E3,$7B
                DEFB      $01    ,$01    ,$E3,$B0
                DEFB      $82,$ED,$01    ,$E3,$7B
                DEFB      $83,$B0,$01    ,$01
                DEFB      $92,$76,$90,$9D,$D3,$7B
                DEFB      $01    ,$01    ,$01
                DEFB      $92,$ED,$01    ,$01
                DEFB      $93,$B0,$01    ,$01
                DEFB      $E3,$7B,$80,$FA,$E3,$49
                DEFB      $01    ,$01    ,$01
                DEFB      $D3,$7B,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$FA,$D3,$49
                DEFB      $01    ,$01    ,$01
                DEFB      $E3,$49,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$01    ,$80,$EC,$E2,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $D3,$49,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$EC,$D2,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $E2,$76,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT23:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $15,$01    ,$80,$BB,$A3,$7B
                DEFB      $01    ,$01    ,$01
                DEFB      $B3,$E8,$01    ,$A3,$B0
                DEFB      $01    ,$01    ,$01
                DEFB      $B3,$B0,$90,$BB,$93,$E8
                DEFB      $01    ,$01    ,$01
                DEFB      $A3,$7B,$01    ,$93,$B0
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$82,$76,$80,$D2,$83,$49
                DEFB      $01    ,$01    ,$83,$1A
                DEFB      $82,$ED,$01    ,$82,$ED
                DEFB      $83,$B0,$01    ,$82,$C3
                DEFB      $92,$76,$90,$D2,$92,$9B
                DEFB      $01    ,$01    ,$92,$76
                DEFB      $92,$ED,$01    ,$92,$52
                DEFB      $93,$B0,$01    ,$92,$31
                DEFB      $82,$ED,$80,$8C,$A2,$11
                DEFB      $82,$C3,$01    ,$A1,$F4
                DEFB      $92,$9B,$01    ,$00
                DEFB      $92,$76,$01    ,$01
                DEFB      $92,$52,$90,$8C,$01
                DEFB      $92,$31,$01    ,$01
                DEFB      $A2,$11,$01    ,$01
                DEFB      $A1,$F4,$01    ,$01
                DEFB  $15,$00    ,$80,$94,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$94,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT24:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $15,$A2,$C3,$80,$FA,$E2,$76
                DEFB      $A2,$76,$01    ,$01
                DEFB      $B2,$31,$01    ,$01
                DEFB      $B1,$F4,$01    ,$01
                DEFB      $B1,$BD,$90,$FA,$D2,$76
                DEFB      $B1,$8D,$01    ,$01
                DEFB      $E2,$76,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$82,$76,$80,$9D,$E2,$C3
                DEFB      $01    ,$01    ,$01
                DEFB      $82,$ED,$01    ,$01
                DEFB      $83,$B0,$01    ,$01
                DEFB      $92,$76,$90,$9D,$D2,$C3
                DEFB      $01    ,$01    ,$01
                DEFB      $92,$ED,$01    ,$01
                DEFB      $93,$B0,$01    ,$01
                DEFB      $E2,$C3,$80,$FA,$E2,$ED
                DEFB      $01    ,$01    ,$01
                DEFB      $D2,$C3,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$FA,$D2,$ED
                DEFB      $01    ,$01    ,$01
                DEFB      $E2,$ED,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$01    ,$80,$EC,$E2,$C3
                DEFB      $01    ,$01    ,$01
                DEFB      $D2,$ED,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$EC,$D2,$C3
                DEFB      $01    ,$01    ,$01
                DEFB      $E2,$C3,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT25:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $15,$01    ,$80,$FA,$E1,$F4
                DEFB      $01    ,$01    ,$E2,$11
                DEFB      $01    ,$01    ,$E2,$31
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$FA,$D2,$31
                DEFB      $01    ,$01    ,$01
                DEFB      $E1,$F4,$01    ,$01
                DEFB      $E2,$11,$01    ,$01
                DEFB  $10,$82,$76,$81,$18,$E2,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $83,$49,$01    ,$01
                DEFB      $83,$E8,$01    ,$01
                DEFB      $92,$76,$91,$18,$D2,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $93,$49,$01    ,$01
                DEFB      $93,$E8,$01    ,$01
                DEFB      $E2,$76,$80,$BB,$E2,$ED
                DEFB      $01    ,$01    ,$01
                DEFB      $D2,$76,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$BB,$D2,$ED
                DEFB      $01    ,$01    ,$01
                DEFB      $E2,$ED,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$01    ,$80,$C6,$E2,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $D2,$ED,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$C6,$D2,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $E2,$76,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT26:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $15,$C3,$7B,$81,$4D,$E2,$ED
                DEFB      $C3,$49,$01    ,$E3,$1A
                DEFB      $B3,$1A,$01    ,$E3,$49
                DEFB      $B2,$ED,$01    ,$01
                DEFB      $B2,$C3,$91,$4D,$D3,$49
                DEFB      $B2,$9B,$01    ,$01
                DEFB      $E2,$ED,$01    ,$01
                DEFB      $E3,$1A,$01    ,$01
                DEFB  $10,$82,$76,$80,$D2,$E3,$E8
                DEFB      $01    ,$01    ,$01
                DEFB      $83,$49,$01    ,$01
                DEFB      $83,$E8,$01    ,$01
                DEFB      $92,$76,$90,$D2,$D3,$E8
                DEFB      $01    ,$01    ,$01
                DEFB      $93,$49,$01    ,$01
                DEFB      $93,$E8,$01    ,$01
                DEFB      $E3,$E8,$81,$4D,$E4,$63
                DEFB      $01    ,$01    ,$01
                DEFB      $D3,$E8,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$91,$4D,$D4,$63
                DEFB      $01    ,$01    ,$01
                DEFB      $E4,$63,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$01    ,$81,$3B,$E3,$E8
                DEFB      $01    ,$01    ,$01
                DEFB      $D4,$63,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$91,$3B,$D3,$E8
                DEFB      $01    ,$01    ,$01
                DEFB      $E3,$E8,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT27:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $15,$01    ,$80,$FA,$E4,$63
                DEFB      $01    ,$01    ,$E4,$A5
                DEFB      $D4,$EC,$01    ,$E4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$90,$FA,$D4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $E4,$63,$01    ,$01
                DEFB      $E4,$A5,$01    ,$00
                DEFB  $10,$82,$76,$81,$18,$E4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $83,$49,$01    ,$01
                DEFB      $83,$E8,$01    ,$01
                DEFB      $92,$76,$91,$18,$D4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $93,$49,$01    ,$00
                DEFB      $93,$E8,$01    ,$01
                DEFB      $E4,$EC,$80,$BB,$E4,$EC
                DEFB      $01    ,$01    ,$E4,$A5
                DEFB      $D4,$EC,$01    ,$E4,$63
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$90,$BB,$D4,$63
                DEFB      $01    ,$01    ,$01
                DEFB      $E4,$EC,$01    ,$01
                DEFB      $E4,$A5,$01    ,$01
                DEFB  $15,$E4,$63,$80,$C6,$E4,$63
                DEFB      $01    ,$01    ,$E4,$A5
                DEFB      $01    ,$01    ,$E4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $D4,$63,$90,$C6,$D4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $E4,$63,$01    ,$01
                DEFB      $E4,$A5,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT28:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $15,$01    ,$81,$4D,$E5,$DB
                DEFB      $E4,$EC,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$E4,$EC
                DEFB      $E4,$63,$91,$4D,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $E5,$DB,$01    ,$E4,$63
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$82,$76,$80,$D2,$E6,$92
                DEFB      $01    ,$01    ,$E6,$F6
                DEFB      $83,$49,$01    ,$E7,$60
                DEFB      $83,$E8,$01    ,$01
                DEFB      $92,$76,$90,$D2,$D7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $93,$49,$01    ,$01
                DEFB      $93,$E8,$01    ,$01
                DEFB      $E7,$60,$81,$4D,$C7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $D7,$60,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$91,$4D,$B7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $C7,$60,$01    ,$00
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$01    ,$81,$3B,$E7,$60
                DEFB      $01    ,$01    ,$00
                DEFB      $B7,$60,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $00    ,$91,$3B,$E7,$60
                DEFB      $01    ,$01    ,$00
                DEFB      $E7,$60,$01    ,$01
                DEFB      $00    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT29:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $15,$00    ,$80,$BB,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$BB,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$82,$76,$80,$D2,$E6,$92
                DEFB      $01    ,$01    ,$E6,$F6
                DEFB      $82,$ED,$01    ,$E7,$60
                DEFB      $83,$B0,$01    ,$01
                DEFB      $92,$76,$90,$D2,$D7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $92,$ED,$01    ,$01
                DEFB      $93,$B0,$01    ,$01
                DEFB      $E7,$60,$80,$8C,$E7,$60
                DEFB      $01    ,$01    ,$E6,$F6
                DEFB      $D7,$60,$01    ,$E6,$92
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$8C,$D6,$92
                DEFB      $01    ,$01    ,$01
                DEFB      $E7,$60,$01    ,$01
                DEFB      $E6,$F6,$01    ,$01
                DEFB  $15,$E6,$92,$80,$94,$E6,$92
                DEFB      $01    ,$01    ,$E6,$F6
                DEFB      $D6,$92,$01    ,$E7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$94,$D7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $E6,$92,$01    ,$01
                DEFB      $E6,$F6,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT30:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $15,$C7,$60,$80,$FA,$A7,$D0
                DEFB      $01    ,$01    ,$A7,$60
                DEFB      $B6,$F6,$01    ,$A6,$F6
                DEFB      $01    ,$01    ,$A6,$92
                DEFB      $B7,$60,$90,$FA,$96,$34
                DEFB      $01    ,$01    ,$95,$DB
                DEFB      $A7,$D0,$01    ,$95,$86
                DEFB      $A7,$60,$01    ,$95,$37
                DEFB  $10,$82,$76,$80,$9D,$E6,$92
                DEFB      $01    ,$01    ,$E6,$F6
                DEFB      $82,$ED,$01    ,$E7,$60
                DEFB      $83,$B0,$01    ,$01
                DEFB      $92,$76,$90,$9D,$D7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $92,$ED,$01    ,$01
                DEFB      $93,$B0,$01    ,$01
                DEFB      $E7,$60,$80,$FA,$E7,$60
                DEFB      $01    ,$01    ,$E6,$F6
                DEFB      $D7,$60,$01    ,$E6,$92
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$FA,$D6,$92
                DEFB      $01    ,$01    ,$01
                DEFB      $E7,$60,$01    ,$01
                DEFB      $E6,$F6,$01    ,$01
                DEFB  $15,$E6,$92,$80,$EC,$E5,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $D6,$92,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$EC,$D5,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $E5,$DB,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT31:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $15,$01    ,$80,$BB,$E4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $D5,$DB,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$BB,$D4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB      $E4,$EC,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$82,$76,$80,$D2,$E8,$C6
                DEFB      $01    ,$01    ,$E9,$4B
                DEFB      $82,$ED,$01    ,$E9,$D9
                DEFB      $83,$B0,$01    ,$01
                DEFB      $92,$76,$90,$D2,$D9,$D9
                DEFB      $01    ,$01    ,$01
                DEFB      $92,$ED,$01    ,$01
                DEFB      $93,$B0,$01    ,$01
                DEFB      $E9,$D9,$80,$8C,$CA,$6E
                DEFB      $01    ,$01    ,$C9,$D9
                DEFB      $D9,$D9,$01    ,$C9,$4B
                DEFB      $01    ,$01    ,$C9,$D9
                DEFB      $01    ,$90,$8C,$B9,$4B
                DEFB      $01    ,$01    ,$B8,$C6
                DEFB      $CA,$6E,$01    ,$B8,$47
                DEFB      $C9,$D9,$01    ,$B7,$D0
                DEFB  $15,$C9,$4B,$80,$94,$E7,$60
                DEFB      $C9,$D9,$01    ,$01
                DEFB      $B9,$4B,$01    ,$01
                DEFB      $B8,$C6,$01    ,$01
                DEFB      $B8,$47,$90,$94,$D7,$60
                DEFB      $B7,$D0,$01    ,$01
                DEFB      $E7,$60,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT32:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $15,$E8,$C6,$80,$FA,$C9,$4B
                DEFB      $01    ,$01    ,$01
                DEFB      $D8,$47,$01    ,$C8,$47
                DEFB      $01    ,$01    ,$01
                DEFB      $D8,$C6,$90,$FA,$B7,$60
                DEFB      $01    ,$01    ,$B6,$F6
                DEFB      $C9,$4B,$01    ,$B6,$92
                DEFB      $01    ,$01    ,$B6,$34
                DEFB  $10,$82,$76,$80,$9D,$A5,$DB
                DEFB      $01    ,$01    ,$A5,$86
                DEFB      $82,$ED,$01    ,$A5,$37
                DEFB      $83,$B0,$01    ,$A4,$EC
                DEFB      $82,$76,$90,$9D,$94,$A5
                DEFB      $01    ,$01    ,$94,$63
                DEFB      $82,$ED,$01    ,$94,$23
                DEFB      $83,$B0,$01    ,$93,$E8
                DEFB      $A5,$37,$80,$FA,$83,$E8
                DEFB      $A4,$EC,$01    ,$84,$23
                DEFB      $94,$A5,$01    ,$84,$63
                DEFB      $94,$63,$01    ,$84,$A5
                DEFB      $94,$23,$90,$FA,$94,$EC
                DEFB      $93,$E8,$01    ,$95,$37
                DEFB      $83,$E8,$01    ,$95,$86
                DEFB      $84,$23,$01    ,$95,$DB
                DEFB  $15,$84,$63,$80,$EC,$A6,$34
                DEFB      $84,$A5,$01    ,$A6,$92
                DEFB      $94,$EC,$01    ,$A6,$F6
                DEFB      $95,$37,$01    ,$A7,$60
                DEFB      $95,$86,$90,$EC,$B7,$D0
                DEFB      $95,$DB,$01    ,$B8,$47
                DEFB      $A6,$34,$01    ,$B8,$C6
                DEFB      $A6,$92,$01    ,$B9,$4B
                DEFB  $FF  ; End of TFSPATtern

TFSPAT33:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $15,$01    ,$80,$FA,$E8,$C6
                DEFB      $01    ,$01    ,$01
                DEFB      $DB,$0D,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$FA,$D8,$C6
                DEFB      $01    ,$01    ,$01
                DEFB      $E8,$C6,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $10,$82,$76,$81,$18,$E8,$C6
                DEFB      $01    ,$01    ,$E9,$4B
                DEFB      $83,$49,$01    ,$E9,$D9
                DEFB      $83,$E8,$01    ,$01
                DEFB      $92,$76,$91,$18,$D9,$D9
                DEFB      $01    ,$01    ,$01
                DEFB      $93,$49,$01    ,$01
                DEFB      $93,$E8,$01    ,$01
                DEFB      $E9,$D9,$80,$BB,$E7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $D9,$D9,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$BB,$D7,$60
                DEFB      $01    ,$01    ,$01
                DEFB      $E7,$60,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$01    ,$80,$C6,$E5,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $D7,$60,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB      $01    ,$90,$C6,$D5,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $E5,$DB,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT34:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $15,$C4,$EC,$81,$4D,$A3,$E8
                DEFB      $01    ,$01    ,$A3,$1A
                DEFB      $B4,$A5,$01    ,$A2,$76
                DEFB      $01    ,$01    ,$A2,$31
                DEFB      $B4,$EC,$91,$4D,$91,$F4
                DEFB      $01    ,$01    ,$91,$8D
                DEFB      $A4,$A5,$01    ,$91,$3B
                DEFB      $A4,$63,$01    ,$90,$FA
                DEFB  $10,$82,$76,$80,$D2,$81,$D8
                DEFB      $01    ,$01    ,$81,$BD
                DEFB      $83,$49,$01    ,$E0,$D2
                DEFB      $83,$E8,$01    ,$01
                DEFB      $92,$76,$90,$D2,$E0,$FA
                DEFB      $01    ,$01    ,$01
                DEFB      $93,$49,$01    ,$E1,$3B
                DEFB      $93,$E8,$01    ,$01
                DEFB      $E0,$D2,$81,$4D,$E1,$76
                DEFB      $01    ,$01    ,$01
                DEFB      $E0,$FA,$01    ,$E1,$A4
                DEFB      $01    ,$01    ,$01
                DEFB      $E1,$3B,$91,$4D,$E1,$F4
                DEFB      $01    ,$01    ,$01
                DEFB      $E1,$76,$01    ,$E2,$76
                DEFB      $01    ,$01    ,$01
                DEFB  $15,$E1,$A4,$81,$3B,$E2,$ED
                DEFB      $01    ,$01    ,$01
                DEFB      $E1,$F4,$01    ,$E3,$49
                DEFB      $01    ,$01    ,$01
                DEFB      $E2,$76,$91,$3B,$E3,$E8
                DEFB      $01    ,$01    ,$01
                DEFB      $E2,$ED,$01    ,$E4,$EC
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT35:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $15,$E4,$EC,$80,$FA,$E5,$DB
                DEFB      $01    ,$01    ,$EB,$B6
                DEFB      $E6,$92,$01    ,$E5,$DB
                DEFB      $ED,$25,$01    ,$01
                DEFB      $E6,$92,$90,$FA,$E4,$EC
                DEFB      $01    ,$01    ,$E9,$D9
                DEFB      $E5,$DB,$01    ,$E4,$EC
                DEFB      $EB,$B6,$01    ,$01
                DEFB  $10,$82,$76,$81,$18,$E5,$86
                DEFB      $01    ,$01    ,$E5,$DB
                DEFB      $83,$49,$01    ,$01
                DEFB      $83,$E8,$01    ,$01
                DEFB      $92,$76,$91,$18,$D5,$DB
                DEFB      $01    ,$01    ,$01
                DEFB      $93,$49,$01    ,$01
                DEFB      $93,$E8,$01    ,$01
                DEFB      $E5,$DB,$80,$BB,$E6,$34
                DEFB      $01    ,$01    ,$E5,$DB
                DEFB      $D5,$DB,$01    ,$E5,$86
                DEFB      $01    ,$01    ,$E5,$DB
                DEFB      $01    ,$90,$BB,$E5,$86
                DEFB      $01    ,$01    ,$E5,$37
                DEFB      $E6,$34,$01    ,$E4,$EC
                DEFB      $E5,$DB,$01    ,$E4,$A5
                DEFB  $15,$E5,$86,$80,$B0,$E6,$92
                DEFB      $E5,$DB,$01    ,$01
                DEFB      $E5,$86,$01    ,$01
                DEFB      $E5,$37,$01    ,$01
                DEFB      $E4,$EC,$90,$B0,$D6,$92
                DEFB      $E4,$A5,$01    ,$01
                DEFB      $E6,$92,$01    ,$01
                DEFB      $01    ,$01    ,$01
                DEFB  $FF  ; End of TFSPATtern

TFSPAT36:
                DEFW  713     ; TFSPATtern tempo
                ;    Drum,Chan.1 ,Chan.2 ,Chan.3
                DEFB  $02,$82,$ED,$81,$18,$A6,$92
                DEFB      $82,$9B,$01    ,$01
                DEFB      $82,$52,$01    ,$A6,$34
                DEFB      $82,$11,$01    ,$01
                DEFB  $02,$82,$ED,$91,$18,$96,$92
                DEFB      $82,$9B,$01    ,$01
                DEFB      $82,$52,$01    ,$96,$F6
                DEFB      $82,$11,$01    ,$01
                DEFB  $02,$82,$C3,$82,$ED,$86,$92
                DEFB      $01    ,$82,$9B,$01
                DEFB      $83,$49,$82,$52,$86,$34
                DEFB      $84,$63,$82,$11,$01
                DEFB      $92,$C3,$90,$8C,$96,$92
                DEFB      $01    ,$01    ,$01
                DEFB      $93,$49,$01    ,$96,$F6
                DEFB      $94,$63,$01    ,$01
                DEFB  $02,$86,$34,$82,$ED,$A6,$92
                DEFB      $01    ,$82,$9B,$A6,$34
                DEFB      $96,$92,$82,$52,$A5,$DB
                DEFB      $01    ,$82,$11,$A5,$86
                DEFB      $96,$F6,$90,$D2,$B5,$37
                DEFB      $01    ,$01    ,$B4,$EC
                DEFB      $A6,$92,$01    ,$B4,$A5
                DEFB      $A6,$34,$01    ,$B4,$63
                DEFB  $02,$A5,$DB,$82,$ED,$C4,$23
                DEFB      $A5,$86,$82,$9B,$C3,$E8
                DEFB      $B5,$37,$82,$52,$C3,$B0
                DEFB      $B4,$EC,$82,$11,$C3,$7B
                DEFB      $B4,$A5,$91,$61,$D3,$49
                DEFB      $B4,$63,$01    ,$D3,$1A
                DEFB      $C4,$23,$01    ,$D2,$ED
                DEFB      $C3,$E8,$01    ,$D1,$61
                DEFB  $FF  ; End of TFSPATtern



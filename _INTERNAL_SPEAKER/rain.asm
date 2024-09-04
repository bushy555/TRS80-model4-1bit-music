	output "rain.bin"
;QChan beeper music engine by Shiru (shiru@mail.ru) 03'11
;Four channels of tone with global volumes, per-pattern tempo and decays
;One channel of interrupting drums, no ROM data required
;Feel free to do whatever you want with the code, it is PD

	org	6000h

begin
	ld hl,musicData
	call play
	jp begin



OP_NOP	equ 000h
OP_ORC	equ 0b1h


play
	di
	ld a,(hl)
	inc hl
	ld (ch0vol),a
	ld a,(hl)
	inc hl
	ld (ch1vol),a
	ld a,(hl)
	inc hl
	ld (ch2vol),a
	ld a,(hl)
	inc hl
	ld (ch3vol),a
	ld (orderPtr),hl
	ld hl,0
	ld (frq0),hl
	ld (frq1),hl
	ld (frq2),hl
	ld (frq3),hl
	ld a,l
	ld (vol0),a
	ld (vol1),a
	ld (vol2),a
	ld (vol3),a
	ld (frameCnt),a
	ld (oldSP),sp
;	in a,(1f)
;	and 01fh
;	ld a,OP_NOP
;	jr nz,$+4
;	ld a,OP_ORC
;	ld (checkKempston),a
	exx
	push hl
	push iy
	jr nextPosition

readLoop
musicPtr=$+1
	ld de,0
readRow
	ld a,(de)
	cp 128
	jp nz,readNotes
nextPosition
orderPtr=$+1
	ld hl,0
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld a,d
	or e
	jr z,orderLoop
	ld (orderPtr),hl
	ld a,(de)
	ld (frameMax),a
	inc de
	ld a,(de)
	ld (ch0decay),a
	inc de
	ld a,(de)
	ld (ch1decay),a
	inc de
	ld a,(de)
	ld (ch2decay),a
	inc de
	ld a,(de)
	ld (ch3decay),a
	inc de
	jp readRow
orderLoop
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld (orderPtr),de
	jp nextPosition
readNotes
	ld h,freqTable/256
	inc de
	or a
	jr z,noNote0
	ld l,a
	ld c,(hl)
	inc l
	ld b,(hl)
	ld (frq0),bc
ch0vol=$+1
	ld a,16
	ld (vol0),a
noNote0
	ld a,(de)
	inc de
	or a
	jr z,noNote1
	ld l,a
	ld c,(hl)
	inc l
	ld b,(hl)
	ld (frq1),bc
frq1=$+1
	ld sp,0
ch1vol=$+1
	ld a,16
	ld (vol1),a
noNote1
	ld a,(de)
	inc de
	or a
	jr z,noNote2
	ld l,a
	ld c,(hl)
	inc l
	ld b,(hl)
	ld (frq2),bc
	exx
frq2=$+1
	ld de,0
	exx
ch2vol=$+1
	ld a,16
	ld (vol2),a
noNote2
	ld a,(de)
	inc de
	or a
	jr z,noNote3
	ld l,a
	ld c,(hl)
	inc l
	ld b,(hl)
	ld (frq3),bc
	exx
frq3=$+1
	ld bc,0
	exx
ch3vol=$+1
	ld a,16
	ld (vol3),a
noNote3
	ld (musicPtr),de
	ld a,(de)
	cp 129
	jr c,noDrum
	inc de
	ld (musicPtr),de

	ld b,128
	sub b
	add a,a
	ld c,a
	ld e,a
	ld l,a
	ld h,a
drum1
	dec c
	jr nz,drum2
	ld c,e
	and 2
	out ($90),a
drum2
	ld a,l
	add a,11
	xor h
	ld l,a
	ld a,h
	add a,12
	xor l
	ld h,a
	djnz drum1

noDrum
	xor a

frq0=$+1
	ld de,0

soundLoopRepeat
	exa

prevCnt1=$+1
	ld hl,0
	ld c,64

soundLoop
	add ix,de		;15
	sbc a,a			;4
vol0=$+1
	and 0			;7
	ld b,a			;4
	add hl,sp		;11
	sbc a,a			;4
vol1=$+1
	and 0			;7
	or b			;4
	ld b,a			;4
	exx				;4
	add iy,de		;15
	sbc a,a			;4
vol2=$+1
	and 0			;7
	exx				;4
	or b			;4
	ld b,a			;4
	exx				;4
	add hl,bc		;11
	sbc a,a			;4
vol3=$+1
	and 0			;7
	exx				;4
	or b			;4
	jr z,noOut		;7/12
	ld b,a			;4
	ld a,2			;7
	out ($90),a		;11

	ld a,b			;4
	djnz $			;~
	cpl				;4
noOut
	add a,32		;7
	ld b,a			;4
	xor a			;4
	out ($90),a		;11

	djnz $			;~
	dec c			;4
	jp nz,soundLoop	;10=~404t

	ld (prevCnt1),hl

frameCnt=$+1
	ld a,0
	ld c,a
ch0decay=$+1
	and 0
	jr nz,ch0dskip
	ld hl,vol0
	or (hl)
	jr z,$+3
	dec (hl)
ch0dskip
	ld a,c
ch1decay=$+1
	and 0
	jr nz,ch1dskip
	ld hl,vol1
	or (hl)
	jr z,$+3
	dec (hl)
ch1dskip
	ld a,c
ch2decay=$+1
	and 0
	jr nz,ch2dskip
	ld hl,vol2
	or (hl)
	jr z,$+3
	dec (hl)
ch2dskip
	ld a,c
ch3decay=$+1
	and 0
	jr nz,ch3dskip
	ld hl,vol3
	or (hl)
	jr z,$+3
	dec (hl)
ch3dskip
	ld hl,frameCnt
	inc (hl)

	exa
	inc a
frameMax=$+1
	cp 20
	jp c,soundLoopRepeat

	jp readLoop

stopPlayer
oldSP=$+1
	ld sp,0
	pop iy
	pop hl
	exx
	ei
	ret


	align 256
freqTable
	dw 0
	dw 247,262,277,294,311,330,349,370,392,416,440,467
	dw 494,524,555,588,623,660,699,741,785,832,881,934
	dw 989,1048,1110,1176,1246,1320,1399,1482,1570,1664,1763,1868
	dw 1979,2096,2221,2353,2493,2641,2798,2965,3141,3328,3526,3736
	dw 3958,4193,4442,4707,4987,5283,5597,5930,6283,6656,7052,7472
	dw 0


; ************************************************************************
; * Song data...
; ************************************************************************
BORDER_CLR:          EQU $0

; *** DATA ***
; *** DATA ***
musicData:

; *** Volumes ***
                     DEFB  0Fh,0Fh,0Fh,0Fh
; *** Song layout ***
                      DEFW      PAT0
                      DEFW      PAT0
                      DEFW      PAT1
                      DEFW      PAT1
                      DEFW      PAT2
                      DEFW      PAT1
                      DEFW      PAT0
                      DEFW      PAT3
                      DEFW      PAT4
                      DEFW      PAT5
                      DEFW      PAT6
                      DEFW      PAT7
                      DEFW      PAT4
                      DEFW      PAT5
                      DEFW      PAT6
                      DEFW      PAT8
                      DEFW      PAT9
                      DEFW      PAT10
                      DEFW      PAT11
                      DEFW      PAT12
                      DEFW      PAT13
                      DEFW      PAT14
                      DEFW      PAT15
                      DEFW      PAT16
                      DEFW      PAT17
                      DEFW      PAT17
                      DEFW      PAT18
                      DEFW      PAT18
                      DEFW      PAT19
                      DEFW      PAT19
                      DEFW      PAT18
                      DEFW      PAT20
                      DEFW      PAT4
                      DEFW      PAT5
                      DEFW      PAT6
                      DEFW      PAT7
                      DEFW      PAT4
                      DEFW      PAT5
                      DEFW      PAT6
                      DEFW      PAT21
                      DEFW      PAT22
                      DEFW      PAT22
                      DEFW      PAT23
                      DEFW      PAT23
                      DEFW      PAT24
                      DEFW      PAT23
                      DEFW      PAT22
                      DEFW      PAT25
LOOPSTART:            DEFW      PAT26
                      DEFW      $0000
                      DEFW      LOOPSTART

; *** Patterns ***
PAT0:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 0,44,0,0
                     DEFB 0,122,0,0
                     DEFB 0,58,0,0
                     DEFB 44,122,0,0
                     DEFB 0,68,0,0
                     DEFB 58,122,0,0
                     DEFB 0,74,0,0
                     DEFB 68,122,0,0
                     DEFB 0,82,0,0
                     DEFB 74,122,0,0
                     DEFB 0,74,0,0
                     DEFB 82,122,0,0
                     DEFB 0,68,0,0
                     DEFB 74,122,0,0
                     DEFB 0,58,0,0
                     DEFB 68,122,0,0
                     DEFB 80h             ; Pattern end

PAT1:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 0,40,0,0
                     DEFB 0,122,0,0
                     DEFB 0,54,0,0
                     DEFB 40,122,0,0
                     DEFB 0,64,0,0
                     DEFB 54,122,0,0
                     DEFB 0,72,0,0
                     DEFB 64,122,0,0
                     DEFB 0,78,0,0
                     DEFB 72,122,0,0
                     DEFB 0,72,0,0
                     DEFB 78,122,0,0
                     DEFB 0,64,0,0
                     DEFB 72,122,0,0
                     DEFB 0,54,0,0
                     DEFB 64,122,0,0
                     DEFB 80h             ; Pattern end

PAT2:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 0,36,0,0
                     DEFB 0,122,0,0
                     DEFB 0,50,0,0
                     DEFB 36,122,0,0
                     DEFB 0,60,0,0
                     DEFB 50,122,0,0
                     DEFB 0,68,0,0
                     DEFB 60,122,0,0
                     DEFB 0,74,0,0
                     DEFB 68,122,0,0
                     DEFB 0,68,0,0
                     DEFB 74,122,0,0
                     DEFB 0,60,0,0
                     DEFB 68,122,0,0
                     DEFB 0,36,0,0
                     DEFB 60,122,0,0
                     DEFB 80h             ; Pattern end

PAT3:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 0,44,0,0
                     DEFB 0,0,0,0
                     DEFB 44,122,0,0
                     DEFB 0,0,0,0
                     DEFB 0,0,0,0
                     DEFB 0,0,0,0
                     DEFB 0,0,0,0
                     DEFB 0,0,0,0
                     DEFB 0,44,0,0
                     DEFB 0,0,0,0
                     DEFB 44,122,0,0
                     DEFB 0,0,0,0
                     DEFB 0,40,0,0
                     DEFB 0,0,0,0
                     DEFB 40,122,0,0
                     DEFB 0,72,0,0
                     DEFB 80h             ; Pattern end

PAT4:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 44,74,0,0,129
                     DEFB 0,0,0,0
                     DEFB 64,0,0,0
                     DEFB 68,58,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0
                     DEFB 40,58,0,0,129
                     DEFB 0,0,0,0
                     DEFB 44,0,0,0,129
                     DEFB 0,0,0,0
                     DEFB 64,0,0,0
                     DEFB 68,0,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0
                     DEFB 40,0,0,0,129
                     DEFB 0,0,0,0
                     DEFB 44,74,0,0,129
                     DEFB 0,0,0,0
                     DEFB 64,0,0,0
                     DEFB 68,58,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0
                     DEFB 40,58,0,0,129
                     DEFB 0,0,0,0
                     DEFB 44,74,0,0,129
                     DEFB 0,0,0,0
                     DEFB 64,0,0,0
                     DEFB 68,72,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0
                     DEFB 40,68,0,0,129
                     DEFB 0,0,0,0
                     DEFB 80h             ; Pattern end

PAT5:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 40,72,0,0,129
                     DEFB 0,0,0,0
                     DEFB 60,0,0,0
                     DEFB 64,54,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0
                     DEFB 36,54,0,0,129
                     DEFB 0,0,0,0
                     DEFB 40,0,0,0,129
                     DEFB 0,0,0,0
                     DEFB 60,0,0,0
                     DEFB 64,0,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0
                     DEFB 36,0,0,0,129
                     DEFB 0,0,0,0
                     DEFB 40,68,0,0,129
                     DEFB 0,72,0,0
                     DEFB 60,0,0,0
                     DEFB 64,0,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0
                     DEFB 36,60,0,0,129
                     DEFB 0,64,0,0
                     DEFB 40,68,0,0,129
                     DEFB 0,0,0,0
                     DEFB 60,0,0,0
                     DEFB 64,64,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0
                     DEFB 36,54,0,0,129
                     DEFB 0,0,0,0
                     DEFB 80h             ; Pattern end

PAT6:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 36,68,0,0,129
                     DEFB 0,0,0,0
                     DEFB 58,0,0,0
                     DEFB 60,50,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0
                     DEFB 34,50,0,0,129
                     DEFB 0,0,0,0
                     DEFB 36,0,0,0,129
                     DEFB 0,0,0,0
                     DEFB 58,0,0,0
                     DEFB 60,0,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0
                     DEFB 34,0,0,0,129
                     DEFB 0,0,0,0
                     DEFB 36,68,0,0,129
                     DEFB 0,0,0,0
                     DEFB 58,0,0,0
                     DEFB 60,50,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0
                     DEFB 34,50,0,0,129
                     DEFB 0,72,0,0
                     DEFB 36,74,0,0,129
                     DEFB 0,0,0,0
                     DEFB 58,0,0,0
                     DEFB 60,0,0,0,129
                     DEFB 122,44,0,0,130
                     DEFB 0,0,0,0
                     DEFB 34,54,0,0,129
                     DEFB 0,0,0,0
                     DEFB 80h             ; Pattern end

PAT7:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 40,58,0,0,129
                     DEFB 0,0,0,0
                     DEFB 60,0,0,0
                     DEFB 64,54,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0
                     DEFB 36,50,0,0,129
                     DEFB 0,0,0,0
                     DEFB 40,50,0,0,129
                     DEFB 0,0,0,0
                     DEFB 60,0,0,0
                     DEFB 64,54,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0
                     DEFB 36,58,0,0,129
                     DEFB 0,0,0,0
                     DEFB 40,54,0,0,129
                     DEFB 0,0,0,0
                     DEFB 60,0,0,0,130
                     DEFB 64,0,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0,130
                     DEFB 36,0,0,0,129
                     DEFB 0,0,0,0
                     DEFB 40,0,0,0,130
                     DEFB 0,0,0,0,130
                     DEFB 60,0,0,0,129
                     DEFB 64,0,0,0
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0,130
                     DEFB 36,0,0,0,129
                     DEFB 0,72,0,0
                     DEFB 80h             ; Pattern end

PAT8:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 40,58,0,0,129
                     DEFB 0,0,0,0
                     DEFB 60,0,0,0
                     DEFB 64,54,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0
                     DEFB 36,50,0,0,129
                     DEFB 0,0,0,0
                     DEFB 40,50,0,0,129
                     DEFB 0,0,0,0
                     DEFB 60,0,0,0
                     DEFB 64,54,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0
                     DEFB 36,58,0,0,129
                     DEFB 0,0,0,0
                     DEFB 40,64,0,0,129
                     DEFB 0,0,0,0
                     DEFB 60,0,0,0,130
                     DEFB 64,0,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0,130
                     DEFB 36,0,0,0,129
                     DEFB 0,0,0,0
                     DEFB 40,0,0,0,130
                     DEFB 0,0,0,0,130
                     DEFB 60,0,0,0,129
                     DEFB 64,0,0,0
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0,130
                     DEFB 36,0,0,0,129
                     DEFB 0,0,0,0
                     DEFB 80h             ; Pattern end

PAT9:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 50,44,0,0,129
                     DEFB 50,44,0,0,129
                     DEFB 50,122,0,0
                     DEFB 122,0,0,0
                     DEFB 50,0,0,0
                     DEFB 122,0,0,0
                     DEFB 50,44,0,0
                     DEFB 50,44,0,0
                     DEFB 50,122,0,0
                     DEFB 122,0,0,0
                     DEFB 50,0,0,0,129
                     DEFB 122,0,0,0
                     DEFB 50,0,0,0,130
                     DEFB 122,0,0,0
                     DEFB 50,0,0,0
                     DEFB 122,0,0,0
                     DEFB 80h             ; Pattern end

PAT10:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 50,44,0,0,129
                     DEFB 50,44,0,0,129
                     DEFB 50,122,0,0
                     DEFB 122,0,0,0
                     DEFB 50,0,0,0
                     DEFB 122,0,0,0
                     DEFB 50,44,0,0
                     DEFB 50,44,0,0
                     DEFB 50,122,0,0
                     DEFB 122,68,0,0
                     DEFB 50,72,0,0,129
                     DEFB 122,0,0,0
                     DEFB 50,74,0,0,130
                     DEFB 122,0,0,0
                     DEFB 50,72,0,0
                     DEFB 122,0,0,0
                     DEFB 80h             ; Pattern end

PAT11:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 64,40,0,0,129
                     DEFB 0,40,0,0,129
                     DEFB 0,122,0,0
                     DEFB 0,0,0,0
                     DEFB 72,0,0,0
                     DEFB 122,0,0,0
                     DEFB 72,40,0,0
                     DEFB 72,40,0,0
                     DEFB 72,122,0,0
                     DEFB 122,0,0,0
                     DEFB 72,0,0,0,129
                     DEFB 122,0,0,0
                     DEFB 72,0,0,0,130
                     DEFB 122,0,0,0
                     DEFB 72,0,0,0
                     DEFB 122,0,0,0
                     DEFB 80h             ; Pattern end

PAT12:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 72,40,0,0,129
                     DEFB 72,40,0,0,129
                     DEFB 72,122,0,0
                     DEFB 122,0,0,0
                     DEFB 72,0,0,0
                     DEFB 122,0,0,0
                     DEFB 72,40,0,0
                     DEFB 72,40,0,0
                     DEFB 72,122,0,0
                     DEFB 122,64,0,0,130
                     DEFB 72,68,0,0,129
                     DEFB 122,122,0,0
                     DEFB 72,68,0,0,130
                     DEFB 122,0,0,0
                     DEFB 72,64,0,0
                     DEFB 122,0,0,0
                     DEFB 80h             ; Pattern end

PAT13:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 68,36,0,0,129
                     DEFB 0,36,0,0,129
                     DEFB 50,122,0,0
                     DEFB 122,0,0,0
                     DEFB 50,0,0,0
                     DEFB 122,0,0,0
                     DEFB 50,36,0,0
                     DEFB 50,36,0,0
                     DEFB 50,122,0,0
                     DEFB 122,0,0,0
                     DEFB 50,0,0,0,129
                     DEFB 122,0,0,0
                     DEFB 50,0,0,0,130
                     DEFB 122,0,0,0
                     DEFB 50,0,0,0
                     DEFB 122,0,0,0
                     DEFB 80h             ; Pattern end

PAT14:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 50,36,0,0,129
                     DEFB 50,36,0,0,129
                     DEFB 50,122,0,0
                     DEFB 122,0,0,0
                     DEFB 50,0,0,0
                     DEFB 122,0,0,0
                     DEFB 50,36,0,0
                     DEFB 50,36,0,0
                     DEFB 50,122,0,0
                     DEFB 122,0,0,0,130
                     DEFB 50,60,0,0,129
                     DEFB 122,0,0,0
                     DEFB 50,64,0,0,130
                     DEFB 122,0,0,0
                     DEFB 50,68,0,0
                     DEFB 122,0,0,0
                     DEFB 80h             ; Pattern end

PAT15:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 64,40,0,0,129
                     DEFB 0,40,0,0,129
                     DEFB 64,64,0,0
                     DEFB 122,0,0,0
                     DEFB 64,0,0,0
                     DEFB 122,0,0,0
                     DEFB 64,40,0,0
                     DEFB 0,40,0,0
                     DEFB 64,64,0,0
                     DEFB 122,0,0,0
                     DEFB 64,0,0,0,129
                     DEFB 122,0,0,0
                     DEFB 64,0,0,0,130
                     DEFB 122,0,0,0
                     DEFB 64,0,0,0
                     DEFB 122,0,0,0
                     DEFB 80h             ; Pattern end

PAT16:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 40,64,0,0,129
                     DEFB 40,0,0,0,129
                     DEFB 64,0,0,0
                     DEFB 122,0,0,0
                     DEFB 64,0,0,0
                     DEFB 122,0,0,0
                     DEFB 40,0,0,0
                     DEFB 40,0,0,0
                     DEFB 64,0,0,0
                     DEFB 122,0,0,0,130
                     DEFB 64,40,0,0,129
                     DEFB 122,0,0,0
                     DEFB 64,40,0,0,131
                     DEFB 122,0,0,0
                     DEFB 64,40,0,0,131
                     DEFB 122,0,0,0
                     DEFB 80h             ; Pattern end

PAT17:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 68,44,0,0,129
                     DEFB 68,44,0,0,129
                     DEFB 64,122,0,0,130
                     DEFB 50,68,0,0
                     DEFB 0,44,0,0,132
                     DEFB 0,0,0,0
                     DEFB 44,68,0,0,129
                     DEFB 44,68,0,0,129
                     DEFB 44,122,0,0,130
                     DEFB 0,0,0,0,132
                     DEFB 40,48,0,0,129
                     DEFB 0,68,0,0
                     DEFB 50,44,0,0,131
                     DEFB 0,0,0,0
                     DEFB 40,48,0,0,129
                     DEFB 0,0,0,0
                     DEFB 80h             ; Pattern end

PAT18:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 64,40,0,0,129
                     DEFB 64,40,0,0,129
                     DEFB 60,122,0,0,130
                     DEFB 72,64,0,0
                     DEFB 0,40,0,0,132
                     DEFB 0,0,0,0
                     DEFB 40,64,0,0,129
                     DEFB 40,64,0,0,129
                     DEFB 40,122,0,0,130
                     DEFB 0,0,0,0,132
                     DEFB 36,68,0,0,129
                     DEFB 0,64,0,0
                     DEFB 72,40,0,0,131
                     DEFB 0,0,0,0
                     DEFB 36,68,0,0,129
                     DEFB 0,0,0,0
                     DEFB 80h             ; Pattern end

PAT19:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 60,36,0,0,129
                     DEFB 60,36,0,0,129
                     DEFB 58,122,0,0,130
                     DEFB 68,60,0,0
                     DEFB 0,36,0,0,132
                     DEFB 0,0,0,0
                     DEFB 36,60,0,0,129
                     DEFB 36,60,0,0,129
                     DEFB 36,122,0,0,130
                     DEFB 0,0,0,0,132
                     DEFB 34,64,0,0,129
                     DEFB 0,60,0,0
                     DEFB 68,36,0,0,131
                     DEFB 0,0,0,0
                     DEFB 34,64,0,0,129
                     DEFB 0,0,0,0
                     DEFB 80h             ; Pattern end

PAT20:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 64,40,0,0,129
                     DEFB 64,40,0,0,129
                     DEFB 64,122,0,0,130
                     DEFB 60,64,0,0
                     DEFB 40,40,0,0,132
                     DEFB 0,0,0,0
                     DEFB 40,68,0,0,129
                     DEFB 40,72,0,0,129
                     DEFB 0,0,0,0
                     DEFB 0,0,0,0
                     DEFB 34,0,0,0,131
                     DEFB 0,0,0,0
                     DEFB 40,0,0,0,131
                     DEFB 0,0,0,0,130
                     DEFB 48,0,0,0,129
                     DEFB 0,72,0,0
                     DEFB 80h             ; Pattern end

PAT21:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 40,58,0,0,129
                     DEFB 0,0,0,0
                     DEFB 60,0,0,0
                     DEFB 64,54,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0
                     DEFB 36,50,0,0,129
                     DEFB 0,0,0,0
                     DEFB 40,50,0,0,129
                     DEFB 0,0,0,0
                     DEFB 60,0,0,0
                     DEFB 64,54,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0
                     DEFB 36,58,0,0,129
                     DEFB 0,0,0,0
                     DEFB 40,64,0,0,129
                     DEFB 0,0,0,0
                     DEFB 60,0,0,0,130
                     DEFB 64,0,0,0,129
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0,130
                     DEFB 36,0,0,0,129
                     DEFB 0,0,0,0
                     DEFB 40,0,0,0,130
                     DEFB 0,0,0,0,130
                     DEFB 60,0,0,0,129
                     DEFB 64,0,0,0
                     DEFB 122,0,0,0,130
                     DEFB 0,0,0,0,130
                     DEFB 0,0,0,0,129
                     DEFB 0,0,0,0
                     DEFB 80h             ; Pattern end

PAT22:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 0,44,0,0,129
                     DEFB 0,122,0,0
                     DEFB 0,58,0,0
                     DEFB 44,122,0,0
                     DEFB 0,68,0,0
                     DEFB 58,122,0,0
                     DEFB 0,74,0,0,129
                     DEFB 68,122,0,0
                     DEFB 0,82,0,0,130
                     DEFB 74,122,0,0
                     DEFB 0,74,0,0
                     DEFB 82,122,0,0
                     DEFB 0,68,0,0
                     DEFB 74,122,0,0
                     DEFB 0,58,0,0,129
                     DEFB 68,122,0,0
                     DEFB 80h             ; Pattern end

PAT23:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 0,40,0,0,129
                     DEFB 0,122,0,0
                     DEFB 0,54,0,0
                     DEFB 40,122,0,0
                     DEFB 0,64,0,0
                     DEFB 54,122,0,0
                     DEFB 0,72,0,0,129
                     DEFB 64,122,0,0
                     DEFB 0,78,0,0,130
                     DEFB 72,122,0,0
                     DEFB 0,72,0,0
                     DEFB 78,122,0,0
                     DEFB 0,64,0,0
                     DEFB 72,122,0,0
                     DEFB 0,54,0,0,129
                     DEFB 64,122,0,0
                     DEFB 80h             ; Pattern end

PAT24:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 0,36,0,0,129
                     DEFB 0,122,0,0
                     DEFB 0,50,0,0
                     DEFB 36,122,0,0
                     DEFB 0,60,0,0
                     DEFB 50,122,0,0
                     DEFB 0,68,0,0,129
                     DEFB 60,122,0,0
                     DEFB 0,74,0,0,130
                     DEFB 68,122,0,0
                     DEFB 0,68,0,0
                     DEFB 74,122,0,0
                     DEFB 0,60,0,0
                     DEFB 68,122,0,0
                     DEFB 0,36,0,0,129
                     DEFB 60,122,0,0
                     DEFB 80h             ; Pattern end

PAT25:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 0,44,0,0,129
                     DEFB 0,0,0,0
                     DEFB 44,0,0,0
                     DEFB 0,0,0,0
                     DEFB 0,0,0,0
                     DEFB 0,0,0,0
                     DEFB 0,0,0,0
                     DEFB 0,0,0,0
                     DEFB 0,0,0,0
                     DEFB 0,0,0,0
                     DEFB 80h             ; Pattern end

PAT26:             DEFB 21             ; Pattern tempo
                     DEFB 15,15,15,15        ; Decays
                     DEFB 122,122,0,0
                     DEFB 0,0,0,0
                     DEFB 0,0,0,0
                     DEFB 0,0,0,0
                     DEFB 80h             ; Pattern end


	end begin

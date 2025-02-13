	output "faseuno.bin"

	org #6000

	;test code

begin

	ld hl,musicData
	call play
	ret



	;engine code

play

    di
    push iy

    ld e,(hl)
    inc hl
    ld d,(hl)
    inc hl
    ld (insTable),hl
    ld (curInsOff),hl

	ld a,(#5c48)
	rra
	rra
	rra
	and 7
	ld (borderCol),a

    ld h,0
    ld l,a
    ld (cnt1a),hl
    ld (cnt1b),hl
    ld (div1a),hl
    ld (div1b),hl
    ld (cnt2),hl
    ld (div2),hl
borderCol=$+1
    ld a,0
    ld (out1),a
    ld (out2),a

    ex de,hl
    ld (seqPtr),hl

mainLoop
    ld iyl,0
readLoop
seqPtr=$+1
    ld hl,0
    ld a,(hl)
    inc hl
    ld (seqPtr),hl
    or a

    jp z,restart		;end of song, loop
    ;jp z,exitPlayer 	;end of song, stop

    bit 7,a
    jp z,render     	;wait
    ld iyh,a
    and 63
    cp 60
    jp nc,other     	;other parameters
    add a,a         	;note
    ld b,0
    ld c,a
    ld hl,noteTable
    add hl,bc
    ld e,(hl)
    inc hl
    ld d,(hl)
    ld a,iyl
    or a
    jr nz,setNote2  	;second channel
setNote1
    ld (div1a),de
    ex de,hl
curInsOff=$+2
    ld ix,0
    ld a,(ix)
    or a
    jr z,$+5
    ld b,a
    add hl,hl
    djnz $-1
    ld e,(ix+1)
    ld d,(ix+2)
    add hl,de
    ld (div1b),hl
    ld iyl,1
    ld a,iyh
    and 64
    jr z,readLoop   	;no phase reset
    ld hl,out1
    res 4,(hl)
    ld hl,0
    ld (cnt1a),hl
    ld h,(ix+3)
    ld (cnt1b),hl
    jr readLoop
setNote2
    ld (div2),de
    ld a,iyh
    ld hl,out2
    res 4,(hl)
    ld hl,0
    ld (cnt2),hl
    jr readLoop

setStop
    ld hl,0
    ld a,iyl
    or a
    jr nz,setStop2
setStop1
    ld (div1a),hl
    ld (div1b),hl
    ld hl,out1
    res 4,(hl)
    ld iyl,1
    jp readLoop
setStop2
    ld (div2),hl
    ld hl,out2
    res 4,(hl)
    jp readLoop

other
    cp 60
    jr z,setStop    	;stop note
    cp 62
    jr z,skipChn1   	;no changes for ch1
    cp 63
    jr z,setLoop    	;loop start
    ld hl,(seqPtr)  	;instrument change
    ld a,(hl)
    inc hl
    ld (seqPtr),hl
    ld h,0
    ld l,a
    add hl,hl
insTable=$+1
    ld bc,0
    add hl,bc
    ld (curInsOff),hl
    jp readLoop

skipChn1
    ld iyl,1
    jp readLoop

setLoop
    ld hl,(seqPtr)
    ld (seqStart),hl
    jp readLoop

restart
seqStart=$+1
    ld hl,0
    ld (seqPtr),hl
    jp readLoop

exitPlayer

	ld hl,10072
	exx
    pop iy
    ei
    ret

render

    and 127
    cp 118

	jp nc,playDrum

    ld d,a
    exx

cnt1a=$+1
    ld hl,0
cnt1b=$+2
    ld ix,0
div1a=$+1
    ld bc,0
div1b=$+1
    ld de,0
out1=$+1
    ld a,0
    exx
    exa
cnt2=$+1
    ld hl,0
div2=$+1
    ld bc,0
out2=$+1
    ld a,0

playNote

    ld e,a          ;4
    xor a           ;4
;    in a,(#fe)      ;11
;    or #e0          ;7
;   inc a           ;4

;    jr nz,exitPlayer	;7/12 z=hold key, nz=wait key

    ld a,e          ;4
    ld e,0          ;7=48t

soundLoop

    exx             	;4
    exa             	;4
    add hl,bc      	 	;11
    out ($90),a     	;11
    jr c,$+4        	;7/12
    jr $+4          	;7/12
    xor 1         	 	;7
    add ix,de       	;15
    jr c,$+4       		;7/12
    jr $+4          	;7/12
    xor 1         	 	;7

    exa             	;4
    out ($90),a     	;11
    exx             	;4
    add hl,bc       	;11
    jr c,$+4        	;7/12
    jr $+4				;7/12
    xor 1       	  	;7

    dec e           	;4
	nop					;4
    jr nz,soundLoop		;7/12=152, aligned to 8t

    dec d           	;4
    jp nz,playNote  	;10

    ld (cnt2),hl
    ld (out2),a
    exx
    exa
    ld (cnt1a),hl
    ld (cnt1b),ix
    ld (out1),a

    jp mainLoop



noteTable
	dw 186,197,208,221,234,248,263,278,295,313,331,351
	dw 372,394,417,442,469,496,526,557,590,626,663,702
	dw 744,788,835,885,938,993,1053,1115,1181,1252,1326,1405
	dw 1489,1577,1671,1771,1876,1987,2106,2231,2363,2504,2653,2811
	dw 2978,3155,3343,3542,3752,3975,4212,4462,4727,5009,5306,5622
	
	
	


playDrum

	sub 116
	ld b,a
	ld a,128
	rla
	djnz $-1

	ld (.smpn),a
	ld a,(borderCol)
	ld d,a
	ld hl,smpData
	ld bc,1024
.l0
	ld a,(hl)		;7
.smpn=$+1
	and 0			;7
	ld a,d			;4
	jr nz,$+4		;7/12
	jr z,$+4		;7/12
	or 1			;7
	out ($90),a		;11
	ld e,4			;7
	dec e			;4
	jr nz,$-1		;7/12=56
	inc hl			;6
	dec bc			;6
	ld a,b			;4
	or c			;4
	jr nz,.l0		;7/12=83t
	jp mainLoop



smpData
	db #02,#02,#02,#02,#00,#00,#02,#00,#0e,#00,#00,#00,#00,#00,#00,#00,#0c,#00,#00,#00,#08,#10,#10,#10
	db #18,#70,#70,#70,#70,#70,#70,#70,#74,#70,#50,#50,#d8,#d0,#d0,#d0,#d4,#d0,#d0,#d0,#d0,#d0,#d0,#d0
	db #d4,#d1,#d1,#d1,#5d,#41,#41,#41,#41,#41,#41,#41,#4d,#41,#41,#41,#49,#41,#41,#41,#47,#41,#41,#60
	db #6c,#22,#20,#20,#2e,#22,#20,#20,#22,#22,#22,#22,#26,#32,#32,#32,#36,#32,#32,#32,#36,#b2,#b2,#b2
	db #b2,#b2,#32,#32,#3e,#32,#32,#b2,#b2,#b2,#b2,#b2,#be,#12,#12,#11,#17,#13,#93,#93,#97,#83,#83,#c3
	db #cb,#c3,#c3,#c3,#cb,#c3,#c1,#c1,#c9,#c1,#c1,#c1,#c5,#c1,#c1,#c1,#41,#41,#41,#41,#4d,#41,#41,#41
	db #41,#40,#40,#40,#6c,#60,#70,#70,#78,#70,#70,#70,#7c,#70,#70,#70,#74,#70,#70,#70,#70,#70,#70,#70
	db #34,#30,#30,#30,#38,#30,#30,#30,#38,#30,#30,#b0,#b8,#b0,#b0,#b0,#a4,#a0,#a0,#a0,#84,#80,#80,#80
	db #8c,#80,#80,#80,#8c,#82,#82,#82,#8c,#80,#82,#82,#8e,#83,#81,#83,#8f,#83,#83,#83,#87,#83,#83,#83
	db #87,#d3,#d3,#d3,#d7,#d3,#d3,#53,#53,#53,#73,#73,#77,#73,#73,#73,#7b,#73,#73,#73,#73,#73,#73,#73
	db #73,#73,#73,#73,#77,#73,#73,#73,#7d,#73,#73,#71,#6d,#61,#60,#60,#6c,#62,#62,#62,#62,#60,#60,#60
	db #64,#60,#60,#20,#00,#00,#00,#00,#08,#00,#00,#00,#00,#00,#00,#00,#04,#00,#00,#00,#04,#00,#90,#90
	db #1c,#10,#10,#10,#90,#90,#90,#90,#18,#10,#10,#10,#90,#90,#90,#10,#1c,#10,#10,#10,#1c,#30,#30,#30
	db #34,#b0,#b0,#b0,#34,#30,#f0,#f0,#f4,#f0,#e0,#e0,#e4,#e0,#e0,#60,#68,#e0,#e0,#e0,#e0,#e2,#e2,#e2
	db #e6,#e0,#e2,#e0,#e6,#e3,#e3,#e3,#e3,#61,#63,#e3,#e7,#e3,#43,#43,#4f,#41,#41,#41,#43,#53,#53,#53
	db #57,#53,#53,#53,#57,#53,#53,#53,#13,#13,#13,#13,#17,#13,#13,#13,#1b,#13,#13,#11,#19,#13,#13,#11
	db #11,#11,#13,#11,#11,#11,#11,#13,#1b,#33,#33,#a1,#a3,#a1,#a1,#a1,#21,#21,#a1,#a1,#a9,#a0,#a0,#a0
	db #a0,#a0,#20,#20,#28,#20,#a0,#a0,#a0,#20,#20,#a0,#e0,#e0,#60,#e0,#e0,#e0,#e0,#e0,#e8,#60,#60,#70
	db #78,#f0,#f0,#f0,#d8,#50,#50,#50,#58,#50,#50,#50,#50,#50,#50,#50,#50,#50,#50,#50,#58,#50,#50,#50
	db #50,#50,#50,#50,#50,#50,#50,#50,#5c,#50,#50,#50,#54,#40,#42,#40,#00,#02,#00,#00,#00,#00,#20,#20
	db #20,#22,#22,#22,#22,#22,#20,#22,#2a,#20,#22,#22,#22,#20,#22,#22,#26,#22,#20,#22,#26,#22,#20,#22
	db #22,#22,#a2,#b2,#b8,#b0,#b0,#b2,#b2,#b2,#b2,#32,#32,#b2,#b0,#b2,#b0,#b0,#32,#90,#90,#92,#52,#51
	db #51,#51,#d1,#d1,#d9,#d1,#51,#51,#51,#51,#53,#d1,#d9,#51,#51,#d3,#d3,#51,#41,#c1,#c1,#c1,#c1,#c1
	db #c9,#c1,#c1,#c1,#41,#41,#c1,#c1,#c9,#c1,#c1,#41,#c9,#c1,#c1,#61,#69,#e1,#e1,#e1,#61,#61,#61,#61
	db #61,#61,#61,#61,#29,#21,#21,#21,#25,#31,#31,#31,#35,#31,#31,#31,#39,#b1,#b1,#31,#31,#31,#31,#31
	db #31,#b1,#32,#30,#b0,#b0,#30,#30,#34,#30,#32,#32,#14,#10,#10,#10,#10,#10,#10,#10,#10,#90,#92,#12
	db #10,#92,#82,#02,#00,#00,#00,#02,#8a,#02,#02,#02,#ce,#40,#40,#40,#40,#42,#40,#42,#4a,#c2,#c2,#42
	db #40,#40,#40,#40,#48,#c2,#42,#42,#48,#40,#60,#60,#ea,#62,#62,#e2,#ea,#60,#70,#70,#70,#70,#f0,#f0
	db #70,#70,#72,#70,#7a,#72,#70,#70,#f0,#f0,#70,#f0,#f0,#70,#70,#f0,#f2,#70,#f0,#b0,#3c,#30,#30,#30
	db #34,#30,#30,#30,#30,#b0,#b0,#30,#b0,#90,#10,#00,#00,#00,#00,#80,#82,#02,#00,#00,#00,#00,#00,#00
	db #00,#00,#00,#00,#04,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00
	db #00,#00,#00,#00,#00,#50,#50,#50,#58,#72,#70,#70,#70,#70,#70,#70,#70,#70,#70,#72,#7a,#72,#72,#70
	db #f8,#f0,#f0,#f2,#7a,#70,#70,#70,#f0,#f0,#f0,#70,#70,#70,#72,#f2,#f0,#70,#70,#f2,#f2,#f0,#f0,#e0
	db #e8,#e2,#60,#60,#e0,#e0,#e2,#e2,#ea,#c0,#c2,#c2,#c2,#42,#00,#82,#82,#82,#80,#82,#80,#80,#80,#80
	db #80,#80,#00,#02,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#08,#00,#10,#10,#10,#10,#10,#10
	db #18,#10,#10,#10,#12,#10,#10,#10,#10,#30,#30,#30,#30,#30,#30,#30,#30,#30,#30,#30,#30,#30,#30,#70
	db #70,#70,#70,#70,#70,#70,#70,#70,#70,#70,#70,#70,#70,#70,#60,#60,#60,#60,#60,#60,#60,#60,#60,#60
	db #68,#60,#60,#60,#60,#60,#60,#60,#68,#60,#60,#c0,#c8,#c0,#c0,#c0,#c0,#40,#40,#40,#40,#c0,#c0,#40
	db #40,#42,#40,#c2,#c8,#c0,#40,#42,#c2,#c2,#10,#10,#98,#90,#92,#92,#12,#12,#10,#10,#10,#10,#10,#10
	db #9a,#92,#90,#90,#98,#90,#90,#90,#12,#12,#92,#90,#b0,#b0,#30,#30,#38,#30,#30,#30,#38,#30,#30,#30
	db #3a,#30,#32,#30,#30,#30,#20,#22,#20,#a0,#a0,#20,#28,#a0,#a2,#a2,#a0,#20,#20,#20,#20,#60,#60,#60
	db #60,#60,#62,#60,#68,#e0,#e0,#e0,#e0,#e0,#60,#60,#60,#60,#e0,#e0,#40,#40,#40,#40,#40,#40,#40,#40
	db #48,#40,#40,#40,#58,#50,#50,#50,#58,#50,#d0,#d0,#50,#50,#50,#50
	
	

musicData
 dw .sequence
 db 1
 dw 2
 db 0
 db 0
 dw 1
 db 128
 db 0
 dw 2
 db 128
 db 0
 dw 1
 db 0
 db 0
 dw 0
 db 2
 db 1
 dw 2
 db 0
 db 0
 dw 1
 db 8
 db 0
 dw 0
 db 8
.sequence
 db #fd,0
 db 137
 db 20
 db 149
 db 20
 db 137
 db 20
 db 149
 db 20
 db 137
 db 20
 db 149
 db 20
 db 137
 db 20
 db 149
 db 20
 db 133
 db 20
 db 145
 db 20
 db 133
 db 20
 db 145
 db 20
 db 135
 db 20
 db 147
 db 20
 db 135
 db 20
 db 147
 db 20
 db 137
 db 20
 db 149
 db 20
 db 137
 db 20
 db 149
 db 20
 db 137
 db 20
 db 149
 db 20
 db 137
 db 20
 db 149
 db 20
 db 133
 db 20
 db 145
 db 20
 db 133
 db 20
 db 145
 db 20
 db 135
 db 20
 db 147
 db 20
 db 135
 db 20
 db 147
 db 16
 db 118
 db #fe
 db 1
 db 118
 db 137
 db 16
 db 120
 db 149
 db 16
 db 118
 db 137
 db 16
 db 120
 db 149
 db 16
 db 118
 db 137
 db 16
 db 120
 db 149
 db 16
 db 118
 db 137
 db 16
 db 120
 db 149
 db 16
 db 118
 db 133
 db 16
 db 120
 db 145
 db 16
 db 118
 db 133
 db 16
 db 120
 db 145
 db 16
 db 118
 db 135
 db 16
 db 120
 db 147
 db 16
 db 118
 db 135
 db 16
 db 120
 db 147
 db 16
 db 118
 db 137
 db 16
 db 120
 db 149
 db 16
 db 118
 db 137
 db 16
 db 120
 db 149
 db 16
 db 118
 db 137
 db 16
 db 120
 db 149
 db 16
 db 118
 db 137
 db 16
 db 120
 db 149
 db 16
 db 118
 db 133
 db 16
 db 120
 db 145
 db 16
 db 118
 db 133
 db 16
 db 120
 db 145
 db 16
 db 118
 db 135
 db 12
 db 122
 db #fe
 db 1
 db 122
 db 147
 db 16
 db 123
 db 135
 db 16
 db 124
 db 147
 db 16
 db 118
 db #fd,2
 db 216
 db 201
 db 16
 db 121
 db 156
 db 213
 db 16
 db 125
 db #fc
 db 201
 db 16
 db 121
 db 156
 db 213
 db 16
 db 118
 db #fc
 db 201
 db 16
 db 121
 db 156
 db 213
 db 16
 db 125
 db 154
 db 201
 db 16
 db 121
 db 152
 db 213
 db 16
 db 118
 db #fc
 db 197
 db 16
 db 121
 db #fe
 db 209
 db 16
 db 125
 db #fe
 db 197
 db 16
 db 121
 db #fe
 db 209
 db 16
 db 118
 db #fd,0
 db 156
 db 199
 db 16
 db 121
 db 154
 db 211
 db 16
 db 125
 db #fd,8
 db 144
 db 199
 db 16
 db 121
 db 142
 db 211
 db 16
 db 118
 db #fd,4
 db 228
 db 201
 db 16
 db 121
 db 168
 db 213
 db 16
 db 125
 db #fc
 db 201
 db 16
 db 121
 db 168
 db 213
 db 16
 db 118
 db #fc
 db 201
 db 16
 db 121
 db 168
 db 213
 db 16
 db 125
 db 166
 db 201
 db 16
 db 121
 db 164
 db 213
 db 16
 db 118
 db #fc
 db 197
 db 16
 db 121
 db #fe
 db 209
 db 16
 db 125
 db #fe
 db 197
 db 16
 db 121
 db #fe
 db 209
 db 16
 db 118
 db #fd,6
 db 220
 db 199
 db 16
 db 119
 db #fd,8
 db 144
 db 211
 db 16
 db 125
 db #fd,6
 db 159
 db 199
 db 16
 db 119
 db #fd,8
 db 147
 db 211
 db 16
 db 118
 db #fd,2
 db 216
 db 201
 db 16
 db 121
 db 156
 db 213
 db 16
 db 125
 db #fc
 db 201
 db 16
 db 121
 db 156
 db 213
 db 16
 db 118
 db #fc
 db 201
 db 16
 db 121
 db 156
 db 213
 db 16
 db 125
 db 154
 db 201
 db 16
 db 121
 db 152
 db 213
 db 16
 db 118
 db #fc
 db 197
 db 16
 db 121
 db #fe
 db 209
 db 16
 db 125
 db #fe
 db 197
 db 16
 db 121
 db #fe
 db 209
 db 16
 db 118
 db #fd,0
 db 156
 db 199
 db 16
 db 121
 db 154
 db 211
 db 16
 db 125
 db #fd,8
 db 144
 db 199
 db 16
 db 121
 db 142
 db 211
 db 16
 db 118
 db #fd,4
 db 228
 db 201
 db 16
 db 121
 db 168
 db 213
 db 16
 db 125
 db #fc
 db 201
 db 16
 db 121
 db 168
 db 213
 db 16
 db 118
 db #fc
 db 201
 db 16
 db 121
 db 168
 db 213
 db 16
 db 125
 db 166
 db 201
 db 16
 db 121
 db 164
 db 213
 db 16
 db 118
 db #fc
 db 197
 db 16
 db 121
 db #fd,14
 db 220
 db 209
 db 16
 db 125
 db 154
 db 197
 db 16
 db 121
 db 152
 db 209
 db 12
 db 119
 db #fe
 db 1
 db 119
 db #fd,12
 db 208
 db 211
 db 16
 db 120
 db #fe
 db 16
 db 125
 db 147
 db 215
 db 16
 db 120
 db #fe
 db 16
 db 118
 db 152
 db 201
 db 16
 db 120
 db #fe
 db 213
 db 16
 db 118
 db #fe
 db 201
 db 16
 db 120
 db #fe
 db 213
 db 16
 db 118
 db #fe
 db 201
 db 16
 db 120
 db #fe
 db 213
 db 16
 db 118
 db #fe
 db 201
 db 16
 db 120
 db #fe
 db 213
 db 16
 db 118
 db #fe
 db 201
 db 16
 db 120
 db #fe
 db 213
 db 16
 db 118
 db #fe
 db 201
 db 16
 db 120
 db #fe
 db 213
 db 16
 db 118
 db #fe
 db 201
 db 16
 db 120
 db #fe
 db 213
 db 16
 db 118
 db 154
 db 201
 db 16
 db 120
 db 151
 db 213
 db 16
 db 118
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 118
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 118
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 118
 db 147
 db 199
 db 16
 db 120
 db 151
 db 211
 db 16
 db 118
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 118
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 118
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 118
 db 215
 db 199
 db 16
 db 118
 db #fe
 db 211
 db 16
 db 118
 db 149
 db 197
 db 16
 db 120
 db #fe
 db 209
 db 16
 db 118
 db #fe
 db 197
 db 16
 db 120
 db #fe
 db 209
 db 16
 db 118
 db #fe
 db 197
 db 16
 db 120
 db #fe
 db 209
 db 16
 db 118
 db 147
 db 197
 db 16
 db 120
 db 149
 db 209
 db 16
 db 118
 db #fe
 db 197
 db 16
 db 120
 db #fe
 db 209
 db 16
 db 118
 db #fe
 db 197
 db 16
 db 120
 db #fe
 db 209
 db 16
 db 118
 db #fe
 db 197
 db 16
 db 120
 db #fe
 db 209
 db 16
 db 118
 db 152
 db 197
 db 16
 db 120
 db #fe
 db 209
 db 16
 db 118
 db 154
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 118
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 118
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 118
 db #fe
 db 199
 db 16
 db 120
 db 152
 db 211
 db 16
 db 118
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 118
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 118
 db #fe
 db 199
 db 12
 db 119
 db #fe
 db 1
 db 119
 db #fe
 db 211
 db 16
 db 119
 db #fe
 db 199
 db 16
 db 119
 db #fe
 db 211
 db 12
 db #fc
 db 4
 db 118
 db #fd,10
 db 215
 db 201
 db 4
 db 152
 db 12
 db 120
 db #fe
 db 213
 db 16
 db 119
 db #fe
 db 201
 db 16
 db 120
 db #fe
 db 213
 db 16
 db 118
 db #fe
 db 201
 db 16
 db 120
 db #fe
 db 213
 db 16
 db 119
 db #fe
 db 201
 db 16
 db 120
 db #fe
 db 213
 db 16
 db 118
 db #fe
 db 201
 db 16
 db 120
 db #fe
 db 213
 db 16
 db 119
 db #fe
 db 201
 db 16
 db 120
 db #fe
 db 213
 db 16
 db 118
 db #fe
 db 201
 db 16
 db 120
 db #fe
 db 213
 db 16
 db 119
 db 154
 db 201
 db 16
 db 120
 db 151
 db 213
 db 16
 db 118
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 119
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 118
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 119
 db 144
 db 199
 db 4
 db 147
 db 12
 db 120
 db 151
 db 211
 db 16
 db 118
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 119
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 118
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 119
 db 215
 db 199
 db 16
 db 118
 db #fe
 db 211
 db 16
 db 118
 db 149
 db 197
 db 16
 db 120
 db #fe
 db 209
 db 16
 db 119
 db #fe
 db 197
 db 16
 db 120
 db #fe
 db 209
 db 16
 db 118
 db #fe
 db 197
 db 16
 db 120
 db #fe
 db 209
 db 16
 db 119
 db 147
 db 197
 db 16
 db 120
 db 149
 db 209
 db 16
 db 118
 db #fe
 db 197
 db 16
 db 120
 db #fe
 db 209
 db 16
 db 119
 db #fe
 db 197
 db 16
 db 120
 db #fe
 db 209
 db 16
 db 118
 db #fe
 db 197
 db 16
 db 120
 db #fe
 db 209
 db 16
 db 119
 db 151
 db 197
 db 4
 db 152
 db 12
 db 118
 db #fe
 db 209
 db 16
 db 118
 db 154
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 119
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 118
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 119
 db #fe
 db 199
 db 16
 db 120
 db 152
 db 211
 db 16
 db 118
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 119
 db #fe
 db 199
 db 16
 db 120
 db #fe
 db 211
 db 16
 db 118
 db #fe
 db 199
 db 12
 db 122
 db #fe
 db 1
 db 122
 db #fe
 db 211
 db 12
 db 124
 db #fe
 db 1
 db 123
 db #fe
 db 199
 db 12
 db 124
 db #fe
 db 1
 db 124
 db #fe
 db 211
 db 16
 db 118
 db #fd,0
 db 137
 db #fc
 db #fe
 db #fe
 db #fe
 db #fe
 db #fe
 db #fe
 db #fe
 db 117
 db 39
 db #fc
 db 20
 db #fc,#fc,#ff,117
 db 0

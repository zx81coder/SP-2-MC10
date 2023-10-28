; TRS80 MC10 emulator for the 48K ZX Spectrum

; v1.12
; added CLOADM
; noticed code can't add extra routines, cause unknown.

; v1.06 to v1.11
; multiple translated routines

; v1.05
; #52 = swap emulate to translate routine
; not in ROM but in translated code when a part must be emulated

; v1.04
; #F9C9 translated

; v1.03
; #cd to start multiple translated routines
; #F03F translated
; #F04B translated
; #F060 translated
; #E200 translated
; #E51B translated
; #E549 translated

; v1.02
; COM fix, always set carry

; v1.01
; fix for PSHA and PSHB 
; #5e at $F160	= translated routine
; #61 at $F29F  = translated routine

; v1.00
; first release, only load of BASIC, no speeding of ROM
; #15 at $FFAB	= sound routine
; #18 at #FD6a 	= trigger LOAD a file

; 5ccc 5ff5	BASIC code
; 5ff6 7fff	virtual tape memory		8K+
; 8000 80ff	zero page			256 bytes
; 8100 afff	emulator translated ROM		16K- 8K emulator, 8K- available translated ROM
; b000 b7ff	characterset 
; b800 b8ff	attributetable
; b900 baff	copy screen for changes
; bc00 bffe	unused memory			free
; bfff		keyboard key storage		1 byte
; c000 dfff	8K RAM		
; e000 ffff	8K ROM


; registers:
; X = hl'  
; A = c	
; B = b
; D = cb 
; so PUSH bc is PUSH AB also in memory big endian (cb=AB)
; A B D S X
; S = SP + #8001
; B' = 0 (fixed value)

; known error
; TCSR not emulated NEEDED for SOUND, but SOUND is done with translated code
; STS, STD and STX do NOT check on zero, assumption all will never be 0
; STD no test on reading keyboard

brkp	equ	#f8d4

cpscr	equ	#b900
attrtab	equ	#b8


macro	main
;		jp	brktst
		ld	a,(de)
		ld	l,a
		ld	h,hbyte/256
		ld	h,(hl)
		jp	(hl)
endmacro



macro xplusn
		inc	de			
		ld	a,(de)			; get n
		exx
		ld	d,h
		ld	e,l			; save HL'
		ld	c,a
		ex	af,af'			; save flags
		add	hl,bc			; add n to x, B always 0
		set	7,h
		ex	af,af'			; get flags
endmacro


macro 	b1main
		inc	de
		main
endmacro


		org	32768


zp		equ	zptab/256

zptab		block	256,0

hbyte		db	c00 / 256, c01 / 256, c02 / 256, c03 / 256
		db	c04 / 256, c05 / 256, c06 / 256, c07 / 256
		db	c08 / 256, c09 / 256, c0a / 256, c0b / 256
		db	c0c / 256, c0d / 256, c0e / 256, c0f / 256

		db	c10 / 256, c11 / 256, c12 / 256, c13 / 256
		db	c14 / 256, c15 / 256, c16 / 256, c17 / 256
		db	c18 / 256, c19 / 256, c1a / 256, c1b / 256
		db	c1c / 256, c1d / 256, c1e / 256, c1f / 256

		db	c20 / 256, c21 / 256, c22 / 256, c23 / 256
		db	c24 / 256, c25 / 256, c26 / 256, c27 / 256
		db	c28 / 256, c29 / 256, c2a / 256, c2b / 256
		db	c2c / 256, c2d / 256, c2e / 256, c2f / 256

		db	c30 / 256, c31 / 256, c32 / 256, c33 / 256
		db	c34 / 256, c35 / 256, c36 / 256, c37 / 256
		db	c38 / 256, c39 / 256, c3a / 256, c3b / 256
		db	c3c / 256, c3d / 256, c3e / 256, c3f / 256

		db	c40 / 256, c41 / 256, c42 / 256, c43 / 256
		db	c44 / 256, c45 / 256, c46 / 256, c47 / 256
		db	c48 / 256, c49 / 256, c4a / 256, c4b / 256
		db	c4c / 256, c4d / 256, c4e / 256, c4f / 256

		db	c50 / 256, c51 / 256, c52 / 256, c53 / 256
		db	c54 / 256, c55 / 256, c56 / 256, c57 / 256
		db	c58 / 256, c59 / 256, c5a / 256, c5b / 256
		db	c5c / 256, c5d / 256, c5e / 256, c5f / 256

		db	c60 / 256, c61 / 256, c62 / 256, c63 / 256
		db	c64 / 256, c65 / 256, c66 / 256, c67 / 256
		db	c68 / 256, c69 / 256, c6a / 256, c6b / 256
		db	c6c / 256, c6d / 256, c6e / 256, c6f / 256

		db	c70 / 256, c71 / 256, c72 / 256, c73 / 256
		db	c74 / 256, c75 / 256, c76 / 256, c77 / 256
		db	c78 / 256, c79 / 256, c7a / 256, c7b / 256
		db	c7c / 256, c7d / 256, c7e / 256, c7f / 256

		db	c80 / 256, c81 / 256, c82 / 256, c83 / 256
		db	c84 / 256, c85 / 256, c86 / 256, c87 / 256
		db	c88 / 256, c89 / 256, c8a / 256, c8b / 256
		db	c8c / 256, c8d / 256, c8e / 256, c8f / 256

		db	c90 / 256, c91 / 256, c92 / 256, c93 / 256
		db	c94 / 256, c95 / 256, c96 / 256, c97 / 256
		db	c98 / 256, c99 / 256, c9a / 256, c9b / 256
		db	c9c / 256, c9d / 256, c9e / 256, c9f / 256

		db	ca0 / 256, ca1 / 256, ca2 / 256, ca3 / 256
		db	ca4 / 256, ca5 / 256, ca6 / 256, ca7 / 256
		db	ca8 / 256, ca9 / 256, caa / 256, cab / 256
		db	cac / 256, cad / 256, cae / 256, caf / 256

		db	cb0 / 256, cb1 / 256, cb2 / 256, cb3 / 256
		db	cb4 / 256, cb5 / 256, cb6 / 256, cb7 / 256
		db	cb8 / 256, cb9 / 256, cba / 256, cbb / 256
		db	cbc / 256, cbd / 256, cbe / 256, cbf / 256

		db	cc0 / 256, cc1 / 256, cc2 / 256, cc3 / 256
		db	cc4 / 256, cc5 / 256, cc6 / 256, cc7 / 256
		db	cc8 / 256, cc9 / 256, cca / 256, ccb / 256
		db	ccc / 256, ccd / 256, cce / 256, ccf / 256

		db	cd0 / 256, cd1 / 256, cd2 / 256, cd3 / 256
		db	cd4 / 256, cd5 / 256, cd6 / 256, cd7 / 256
		db	cd8 / 256, cd9 / 256, cda / 256, cdb / 256
		db	cdc / 256, cdd / 256, cde / 256, cdf / 256

		db	ce0 / 256, ce1 / 256, ce2 / 256, ce3 / 256
		db	ce4 / 256, ce5 / 256, ce6 / 256, ce7 / 256
		db	ce8 / 256, ce9 / 256, cea / 256, ceb / 256
		db	cec / 256, ced / 256, cee / 256, cef / 256

		db	cf0 / 256, cf1 / 256, cf2 / 256, cf3 / 256
		db	cf4 / 256, cf5 / 256, cf6 / 256, cf7 / 256
		db	cf8 / 256, cf9 / 256, cfa / 256, cfb / 256
		db	cfc / 256, cfd / 256, cfe / 256, cff / 256

c00		nop			; v illegal opcode

c01		b1main			; v NOP 

c08		exx			; v INX 
		ex	af,af'		; keep flags
		inc	hl		; INX
		ld	a,h		
		or	l		; test zero reached
		exx
		jp	z,setz		; if so set zero flag		
		ex	af,af'		; get original flags
		jp	c01		; no change but Z had to be reset

		block 	$/256*256+#15-$

c15		jp	LFFAB1		; opcode #15 becomes play sound

c18		jp	load		; opcode #18 activates load in BASIC

c1b		ld	a,c		; v ABA	Add B to A
		add	a,b
		ld	c,a
		b1main

c25		jp	c,c20		; v BCS   branch carry set
		inc	de
		b1main

c30		exx			; v TSX   transfer S to X
		ex	af,af'		; save flags
		ld	hl,#8000	; SP is #8000 off
		add	hl,sp		; HL now "real" S
		ex	af,af'		; get flags
c08ok		exx			; HL again HL'
		b1main

c3f		inc	de		; SWI  software intrupt
		push	de		; stack RET
		exx
		ld	d,l
		ld	e,h
		push	de		; stack X
		exx
		push	bc		; stack D
		push	af		; stack CCR (flag)
		ld	hl,(#fffa)
		ld	d,l
		ld	e,h		; get intruptroutine
		main				
 
c53		ld	a,b		; v COMB
		cpl		
		or	a		; zero and sign test
		scf			; COM always C
		ld	b,a
		jp	c01

		block	$/256*256+#5d-$,0
c5d		xor	a		; v TSTB
		or	b
		b1main

		block	$/256*256+#66-$,0
c66		xplusn			; v ROR n,x
		rr	(hl)			
		inc	(hl)
		dec	(hl)
		ex	de,hl		; get original HL'
		exx
		b1main

c7e		ex	de,hl		; v JMP nnnn 
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	e,(hl)
		set	7,d
		main

c8b		inc	de		; v ADD A,#n
		ld	a,(de)
		add	a,c
		ld	c,a
c8b01		b1main

c96		inc	de		; v LDAA zp
		ld	a,(de)
		ld	l,a
		ld	h,zp
		ld	c,(hl)
		ld	a,c
		inc	a
		dec	a		; test zero		
		jp	c01

; set B'=0
		block	$/256*256+#a3-$,0
ca3		ld	(ca3bc+1),bc
		xplusn			; SUBD n,x   D=AB = cb   D:=D-m:m+1
		ld	b,(hl)
		inc	hl
		ld	c,(hl)
ca3bc		ld	hl,0
		ld	a,l
		ld	l,h
		ld	h,a
		xor	a
		sbc	hl,bc
		ld	b,a
		ex	de,hl
		ld	a,d
 		exx
 		ld	c,a
 		exx 
		ld 	a,e
		exx
		ld	b,a
		jp	c01

		block	$/256*256+#ca-$,0
cca		inc	de		; v ORAB #n
		ld	a,(de)
		jp	c,orbc
		or	b
		ld	b,a
		jp	c01

		block	$/256*256+#d5-$,0
cd5		inc	de		; v BITB zpnn
		ld	a,(de)
		ld	h,zp
		ld	l,a
		ld	a,b
		jr	nc,cd5nc
		and	(hl)
		scf			; keep CARRY
		db	62		; LD A,N skip AND (HL)
cd5nc		and	(hl)
		jr	c09ok+1		; B1MAIN in rech

		block	$/256*256+#e3-$,0

; set B' =0
ce3		ld	(ce3bc+1),bc
		xplusn			; v ADDD n,x   D=AB = cb   D:=D-m:m+1
		ld	b,(hl)
		inc	hl
		ld	c,(hl)
ce3bc		ld	hl,0
		ld	a,l
		ld	l,h
		ld	h,a
		add	hl,bc
		ld	b,0
		ex	de,hl
		ld	a,d
 		exx
 		ld	c,a
 		exx 
		ld 	a,e
		exx
		ld	b,a
		jp	c01

		block 	$/256*256+#09-$,0
c09		exx			; v DEX
		dec	hl
		ld	a,h
		or	l
c09ok		exx
		b1main
		db	1

c16		ld	b,c		; TAB
		inc	b	
		dec	b
		b1main

		block 	$/256*256+#21-$,0

c21		jp	skc21		; v BRN branch never (skip)		

c24		jp	nc,c20		; v BCC branch carry clear
skc21		inc	de
		b1main

c2f		jp	m,c20		; v BLE BRANCH LESS EQUAL zero
		jp	z,c20
		inc	de
		b1main
c3d		ld	h,b		; v MUL A*B>D
		ld	l,0
		ld	b,l
		ld	a,8		; HL=H*C
m81m		add	hl,hl
		jr	nc,m8sk
		add	hl,bc
m8sk		dec	a
		jr	nz,m81m
		ld	c,h
		ld	b,l
		b1main
		nop
c54		srl	b		; v LSRB
		b1main
		nop
c5e		jp	LF160s
c61		jp	LF29Fs
c64		xplusn			; v LSR n,x
		srl	(hl)			
		ex	de,hl		; get original HL'
		exx
		b1main
		db	0,0
c7c		inc	de		; v INC nnnn
		ld	a,(de)
		ld	h,a
		set	7,h
		inc	de
		ld	a,(de)
		ld	l,a
		inc	(hl)
c8cjp		b1main
; set B'
c8c		inc	de		; v CPX #NN
		ex	de,hl
		ld	d,(hl)
		inc	hl
		ld	e,(hl)
		ld	(c8cde+1),de
		ex	de,hl
		exx
c8cde		ld	bc,0
		ld	d,h
		ld	e,l

		xor	a
		sbc	hl,bc
		ld	b,a
		ex	de,hl

		exx
		jr	c8cjp

		block	$/256*256+#a4-$,0
ca4		xplusn			; v ANDA n,x
		ld	a,(hl)
		ex	de,hl		; get original HL'
		exx
		jp	c,andcc
		and	c
		ld	c,a
		jp	c01

emulst		jp	start

		block $/256*256+#bd-$,0

cbd		inc	de		; v JSR nnnn
		ex	de,hl
		ld	d,(hl)
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	a,h
		ld	h,l
		ld	l,a
		push	hl
		set	7,d
		main

refpeek		ld	bc,ifrq+1
		ret


		block	$/256*256+#d6-$,0

cd6		inc	de		; v LDAB zpnn
		ld	a,(de)
		ld	h,zp
		ld	l,a
		ld	b,(hl)
		ld	a,b
		inc	a
		dec	a
		jp	c01

		block	$/256*256+#e4-$,0
ce4		xplusn			; v ANDB n,x
		ld	a,(hl)
		ex	de,hl		; get original HL'
		exx
		jp	c,andbc
		and	b
		ld	b,a
		jp	c01

		block	$/256*256+#fd-$,0
cfd		inc	de		; v STD nnnn
		ld	a,(de)
		ld	h,a
		set	7,h
		inc	de
		ld	a,(de)
		ld	l,a
		ld	(hl),c
		inc	hl
		ld	(hl),b
		b1main

c0f		di			; v SEI Stop intrupt on ZX Spectrum
		b1main
		db	0,0,0

c1a		jp	L00F3S
c1d		jp	error
c20		inc	de		; v BRA n
		ex	af,af'
		ld	a,(de)
		ld	l,a
		add	a,a
		sbc	a,a
		ld	h,a
		add	hl,de
		ex	af,af'
		ex	de,hl
		b1main
		block 1,0
c32		dec	sp
		pop	hl		; PULA
		ld	c,h
		b1main
c3c		exx			; v PSHX
		ld	d,l
		ld	e,h
		push	de
		exx
		b1main				
c48		sla	c		; v ASLA
		b1main
c51		jp	error
		nop	
c55		jp	error
c58		sla	b		; v ASL B
		b1main
		nop
c62		jp	error
c65		jp	error
c68		xplusn			; v ASL n,x
		sla	(hl)
		ex	de,hl
		exx
		b1main
		db	0

c7f		ex	de,hl		; v CLR nnnn
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	e,(hl)
		set	7,d
		ex	de,hl
		xor	a
		ld	(hl),a
		b1main
c90		inc	de		; v SUBA zp
		ld	a,(de)
		ld	l,a
		ld	h,zp
		ld	a,c
		sub	(hl)
		ld	c,a
		b1main

c9f		ex	af,af'		; v STS zp ,save flags
		ld	hl,#8000-1	; SP is #8001 off
		add	hl,sp		; HL now "real" S
		ld	a,h
		ex	af,af'
		inc	de
		ld	a,l		; lowbyte to A
		ex	de,hl
		ld	e,(hl)		; get location
		ld	d,zp
		inc	e		; first set second
		ld	(de),a
		dec	e		
		ex	af,af'		; get highbyte in A
		ld	(de),a		; write highbyte
		ex	de,hl		; swap to PC
		ex	af,af'		; get flags
		jp	c01
		
		block	$/256*256+#b9-$,0
cb9		inc	de		; v ADCA nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	a,c
		adc	a,(hl)
		ld	c,a
		b1main
		nop
ccc		inc	de		; v LDD #nnnn
		ld	a,(de)
		ld	c,a
		inc	de
		ld	a,(de)
		ld	b,a
		jp	c,lddc
		or	c		; test zero only!
		jp	c01

;		b1main
cd9		inc	de		; v ADCB zpnn
		ld	a,(de)
		ld	h,zp
		ld	l,a
		ld	a,b
		adc	a,(hl)
		ld	b,a
		b1main
ce8		xplusn			; v EORB n,x
		ld	a,(hl)
		ex	de,hl		; get original HL'
		exx
		jp	c,xorbc
		xor	b
		ld	b,a
		jp	c01
		nop
isrvect		dw	isr		; #84ff 
		nop
c02		jp	error
c05		sla	b		; ASLD
		rl	c
		b1main
c10		ld	a,c		; v SBA	Subtract B from A
		sub	b
		ld	c,a
		b1main
		block 2,0
c1c		jp	error
c1f		jp	error

c22		inc	de		; v BHI BRANCH HIGHER (C=0 Z=0)
		jr	c,c221
		jp	nz,c20+1
c221		b1main
		block 2,0
c31		inc	sp		; v INS
		b1main
c39		pop	de		; v RTS
		ld	a,d
		ld	d,e
		ld	e,a
		set	7,d
		main
c45		jp	error
		nop

c49		jp	c49ov

		block	$/256*256+#52-$,0
c52		ex	de,hl		; swap to HL
		jp	(hl)		; RUN from next byte
		nop
		nop
c56		rr	b		; v RORB
		inc	b
		dec	b
		jp	c01
		db	1,1

c5f		xor	a
		ld	b,a		; v CLRB
		b1main
		nop
c69		xplusn			; v ROL n,x
		rl	(hl)		
		inc	(hl)
		dec	(hl)
		ex	de,hl
		exx
		b1main

c81		ex	de,hl		; v CMPA #n
		inc	hl			
		ld	a,c
		cp	(hl)
		ex	de,hl
		b1main
c8d		inc	de		; v BSR n
		ex	af,af'
		ld	a,(de)
		inc	de
		ld	h,e
		ld	l,d
		push	hl
		ld	l,a
		add	a,a
		sbc	a,a
		ld	h,a
		add	hl,de
		ex	de,hl
		jp	afmain
		block   $/256*256+#9d-$,0
c9d		inc	de		; v JSR zp
		ld	a,(de)
		inc	de
		ld	l,d
		ld	h,e
		push	hl
		ld	d,zp
		ld	e,a
		jp	c01+1
		block	$/256*256+#ab-$,0
cab		xplusn			; v ADDA n,x
		ld	a,(hl)
		ex	de,hl		; get original HL'
		exx
		add	a,c
		ld	c,a
		b1main
		db	0,0
cc4		inc	de		; v ANDB #n
		ld	a,(de)
		jp	c,andbc
c17nc		and	b
		ld	b,a
		jp	c01

		block	$/256*256+#cf-$,0
ccf		jp	error		; illegal opcode
cd2		inc	de		; v SBCB	zpnn
		ld	a,(de)
		ld	h,zp
		ld	l,a
		ld	a,b
		sbc	a,(hl)
		ld	b,a
		b1main

ce1		xplusn			; v CMPB n,x
		ld	a,(hl)
		ex	de,hl		; get original HL'
		exx
		ld	h,a
		ld	a,b
		cp	h
		b1main
		db	0,0

cfb		inc	de		; v ADDB nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	a,b
		add	a,(hl)
		ld	b,a
		b1main

c0d		scf			; v SEC
		b1main
		block 	2,0

c17		ld	c,b		; v TBA
		inc	c
		dec	c
		b1main

		block	$/256*256+#23-$,0

c23		inc	de		; v BLS lower or same		
		jp	c,c20+1		; C=1
		jp	z,c20+1		; Z=1
		b1main
		block 	2,0
c33		dec	sp
		pop	hl		; v PULB
		ld	b,h
		b1main
		nop

c3e		jp error		; v WAI wait for intrupt
; handle this when opcode is called
c41		jp	error
c44		srl	c		; v LSRA
		b1main
c4d		xor	a		; v TSTA
		or	c
		b1main
		nop
c57		sra	b		; v ASRB
		b1main
c60		xplusn			; v NEG n,x
		ld	a,b		; B=0
		sub	(hl)
		ld	(hl),a			
		ex	de,hl		; get original HL'
		exx
		b1main
		db	0,0
c79		ex	de,hl		; v ROL nnnn
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	e,(hl)
		set	7,d
		ex	de,hl
		rl	(hl)
		inc	(hl)
		dec	(hl)
		jp	c01
		db	1,1

c8a		inc	de		; v ORAA #n
		ld	a,(de)
		jp	c,orcc
		or	c
		ld	c,a
		jp	c01

		block	$/256*256+#95-$,0
c95		inc	de		; v BITA zp
		ld	a,(de)
		ld	l,a
		ld	h,zp
		ld	a,c
		jr	nc,c95nc
		and	(hl)
		scf
		db	62
c95nc		and	(hl)
		jp	c01

		block	$/256*256+#a5-$,0
ca5		xplusn			; v BITA n,x
		ld	a,(hl)
		ex	de,hl		; get original HL'
		exx
		jr	nc,ca5nc
		and	c
		scf
		db	62
ca5nc		and	c
		jp	c01

		block	$/256*256+#be-$,0
cbe		ex	de,hl		; v LDS nnnn
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	e,(hl)
		ex	de,hl
		set	7,h
		ld	sp,hl
		inc	sp
		jp	c01
		db	0,0,0
;		b1main
cce		inc	de		; v LDX #nnnn
		ld	a,(de)
		exx
		ld	h,a	
		exx
		inc	de
		ld	a,(de)
		exx
		ld	l,a
		exx
		b1main
cdf		inc	de		; v STX zp
		ld	a,(de)
		exx
		ld	e,a
		ld	d,zp
		ex	de,hl
		ld	(hl),d
		inc	hl
		ld	(hl),e
		ex	de,hl
		exx
		jp	c01

		block	$/256*256+#f0-$,0
cf0		jp	cf0a
		nop
cf4		inc	de		; v ANDB nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	a,(hl)
		jp	c,andbc
		and	b
		ld	b,a
		jp	c01

		block	$/256*256+#06-$,0

; not perfect, but if MC10 mostly use this to stack flags, this will work
c06		push	bc		; TAP transfer A to flags
		pop	af		; C holds A, so now C in F
		ei			; allways allow intrupts again
		b1main

		block   #11-#10,0
c11		ld	a,c		; v CBA compare B to A
		cp	b
		b1main

; the routine to built a new screen after (re)start from BASIC
killscr		ld	hl,cpscr
kill1		ld	(hl),40			; impossible character
		inc	hl
		ld	a,h
		cp	cpscr /256 +2
		jr	nz,kill1
		ret

c26		jp	nz,c20		; v BNE not equal zero	
		inc	de
		b1main
		block 	#34-#31,0
c34		dec	sp		; v DECS
		b1main

		block	#40-#3c,0
c40		ld	a,0		; v NEGA
		sub	c
		ld	c,a
		b1main
c4b		jp	error
c4e		jp	error
afmain		ex	af,af'
		main
		block	$/256*256+#59-$,0
c59		jp	c59ov		; v ROLB

		block	$/256*256+#63-$,0
c63		xplusn			; v COM n,X
		ld	a,(hl)
		cpl
		or	a
		scf
		ld	(hl),a
		ex	de,hl		; get original HL'
		exx
		b1main

		block	$/256*256+#7d-$,0
c7d		ex	de,hl		; v TST nnnn
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	e,(hl)
		set	7,d
		ex	de,hl
		xor	a
		or	(hl)
		b1main
c8e		inc	de		; v LDS #nnnn
		ld	a,(de)
		ld	h,a
		set 	7,h
		inc	de
		ld	a,(de)
		ld	l,a
		ld	sp,hl
		inc	sp
		jp	c01
		db	0,0,0
;		b1main
c9e		inc	de		; v LDS zpnn
		ld	a,(de)
		ld	l,a
		ld	h,zp
		ld	a,(hl)
		inc	hl
		ld	l,(hl)
		ld	h,a
		set	7,h
		ld	sp,hl
		inc	sp
		jp	c01
		db	0,0,0
;		b1main
cb1		jp	cb1a
		nop
		nop
cb6		jp	c,cb6c
		jp	cb6a

		block	$/256*256+#c2-$,0
cc2		ex	de,hl		; v SBCB #n
		inc	hl			
		ld	a,b
		sbc	a,(hl)
		ld	b,a
		ex	de,hl
		b1main

		nop
cd0		inc	de		; v SUBB zpnn
		ld	a,(de)
		ld	l,a
		ld	h,zp
		ld	a,b
		sub	(hl)
		ld	b,a
		b1main
		nop

ce0		xplusn			; v SUBB n,x
		ld	a,(hl)
		ex	de,hl		; get original HL'
		exx
		ld	h,a
		ld	a,b
		sub	h
		ld	b,a
		b1main
		dw	0
		nop
cfc		inc	de		; v LDD nnnn
		ld	a,(de)
		ld	h,a
		set	7,h
		inc	de
		ld	a,(de)
		ld	l,a
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
		ld	a,b
		jp	c,lddc
		or	c		
		db	62

c0e		ei			; v CLI
		b1main


		block	$/256*256+#27-$,0

c27		jp	z,c20		; v BEQ branch equal
		inc	de
		b1main

		block	#35-#32,0
c35		exx			; v TXS transfer X to S
		ld	a,h		; save high
		set	7,h
		ld 	sp,hl		; TXS => S=X-1
		ld	h,a		; undo change
		exx
		b1main

c43		ld	a,c		; v COMA
		cpl		
		or	a		; zero and sign test
		scf			; COM always C
		ld	c,a
		b1main

		block	$/256*256+#4f-$,0
c4f		xor	a
		ld	c,a		; v CLRA
		b1main
		block	#5a-#58,0
c5a		dec	b		; v DECB
		b1main
		nop
		block	#67-#63,0
c67		xplusn			; v ASR n,x
		sra	(hl)
		ex	de,hl
		exx
		b1main
		dw	0
		nop
c80		ex	de,hl		; v SUBA #n
		inc	hl			
		ld	a,c
		sub	(hl)
		ld	c,a
		ex	de,hl
		b1main
		block	#8f-#8d,0
c8f		jp	error
c92		inc	de		; v SBCA zpnn
		ld	a,(de)
		ld	l,a
		ld	h,zp
		ld	a,c
		sbc	a,(hl)
		ld	c,a
		b1main
ca1		xplusn			; v CMPA n,x
		ld	a,(hl)
		ex	de,hl		; get original HL'
		exx
		ld	h,a
		ld	a,c
		cp	h
		b1main
		dw	0
cbb		inc	de		; v ADDA NNNN
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	a,c
		add	a,(hl)
		ld	c,a
		b1main
ccd		ex	de,hl		; PC as startaddres
		jp	(hl)		; make the call
		db 	0,0

cd1		inc	de		; v CMPB zpnn
		ld	a,(de)
		ld	h,zp
		ld	l,a
		ld	a,b
		cp	(hl)
		b1main
		block	#e2-#df,0
ce2		xplusn			; SBCB n,x
		ld	a,(hl)
		ex	de,hl		; get original HL'
		exx
		ld	h,a
		ld	a,b
		sbc	a,h
		ld	b,a
		b1main
		dw	0
		nop

cfe		inc	de		; LDX nnnn
		ld	a,(de)
		ld	h,a
		set	7,h
		inc	de
		ld	a,(de)
		ld	l,a
		ld	(cfehl+1),hl
		exx
cfehl		ld	hl,0
		ld	a,(hl)
		inc	hl
		ld	l,(hl)
		ld	h,a
		exx
		b1main

		block $/256*256+#19-$,0
c19		ld	a,c		; v DAA
		daa
		ld	c,a
		b1main


		block	#28-#23,0

c28		inc	de		; BVC
		jp	m,c28m
		jp	nc,c20+1
		b1main



		block	$/256*256+$36-$,0
; v1.01 fix, old method did not work when S was used to copy over itself
c36		dec	sp		; PSHA
		pop	hl		; get right H
		ld	l,c		; set L to A
		push	hl		; again write H and PUSH A
		b1main

		block	$/256*256+$42-$,0
c42		jp	error
		nop
c46		rr	c		; v RORA
		inc	c
		dec	c
		jp	c01

		block	$/256*256+$50-$,0
c50		ld	a,0		; v NEGB
		sub	b
		ld	b,a
		b1main
c5b		jp	error

; 12 free bytes can be used for fixed address return from BASIC
emulwrm		call	killscr		; built a new sceen after return
emulsp		ld	sp,0		; get SP-emulator
		im	2		; allow screenupdates
		pop	de		; get PC
		pop	bc		; get registers
		exx			; go to needed registers
		ret			; continue where left

c6a		xplusn			; v DEC n,x
		dec	(hl)
		ex	de,hl
		exx
		b1main
		dw	0
		nop
c82		ex	de,hl		; v SBCA #n
		inc	hl			
		ld	a,c
		sbc	a,(hl)
		ld	c,a
		ex	de,hl
		b1main

		block	#91-#8f,0
c91		inc	de		; v CMPA zpnn
		ld	a,(de)
		ld	l,a
		ld	h,zp
		ld	a,c
		cp	(hl)
		b1main
		block	#a0-#9f,0
ca0		xplusn			; v SUBA n,x
		ld	a,(hl)
		ex	de,hl		; get original HL'
		exx
		ld	h,a
		ld	a,c
		sub	h
		ld	c,a
		b1main
		dw	0
		nop

cbc		inc	de		; v CPX nnnn
		ex	de,hl
		ld	d,(hl)
		inc	hl
		ld	e,(hl)
		ex	de,hl
		set	7,h
		ld	a,(hl)
		exx
		ld	b,a
		exx
		inc	hl
		ld	a,(hl)
		exx
		ld	c,a
		ld	d,h
		ld	e,l
		xor	a		; reset possible carry
		sbc	hl,bc
		ld	b,a		; fixed value of 0 in B'
		ex	de,hl
		exx
		jp	c01
		
		block	$/256*256+#d8-$,0
cd8		inc	de		; v EORB zpnn
		ld	a,(de)
		ld	h,zp
		ld	l,a
		ld	a,(hl)
		jp	c,xorbc
		xor	b
		ld	b,a
		jp	c01

		nop
ce7		inc	de		; STAB n,x		
		ld	a,(de)		; get n
		exx
		ld	d,h
		ld	e,l		; save HL'
		ld	c,a
		ex	af,af'		; save flags
		add	hl,bc		; add n to x, B always 0
		set	7,h
					
		exx
		ld	a,b
		exx
		ld	(hl),a
		jp	keybtst1

; old code fills to #nn03
		ex	de,hl		; get original HL'
		exx
		b1main
		block	2,0

c03		jp	error
		nop
; not perfect, but if MC10 mostly use this to stack flags, this will work
c07		ld	a,b		; v TPA keep B unchanged
		push	af		; TPA transfer flags to A to 
		pop	bc		; C holds A, so now C in F
		b1main
		block	#12-#11,0
c12		nop			; also error
c13		nop			; also error
c14		jp	error

c73a		ex	de,hl		; v COM nnnn
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	e,(hl)
		set	7,d
		ex	de,hl
		ld	a,(hl)
		cpl
		ld	(hl),a
		or	a
		scf
		jp	c01

		block	$/256*256+#29-$
c29		inc	de		; v BVS
		jp	p,c28m		; 0 + 1 > V
		jp	nc,c20+1	; 1 + 0 > V
		b1main

		block	$/256*256+#37-$
c37		dec	sp		; PSHB
		pop	hl		; get H 
		ld	l,b
		push	hl		; save H and PSHB
		b1main


		block	$/256*256+#47-$
c47		sra	c		; v ASRA
		b1main
		block	#5c-#50,0
c5c		inc	b		; v INCB
		b1main
		block	#6b-#64,0
c6b		jp	error
c6e		xplusn			; v JMP n,x
		ld	a,h
		exx
		ld	d,a
		exx
		ld	a,l
		ex	de,hl
		exx
		ld	e,a
		main

		block	$/256*256+#87-$,0
c87		jp	error

c01c		or	a		; zero test
		scf			; keep carry
c0c1m		b1main

		block	$/256*256+#93-$,0
c93		ld	(c93bc+1),bc			; SUBD zpnn
		ex	de,hl
		inc	hl
		ld	e,(hl)
		ld	d,zp
		ex	de,hl
		ld	b,(hl)
		inc	l
		ld	c,(hl)
c93bc		ld	hl,0
		ld	a,l
		ld	l,h
		ld	h,a
		and	a
		sbc	hl,bc
		ld	c,h
		ld	b,l
		jr	c0c1m	
; B'
		block	$/256*256+#ad-$,0
cad		inc	de		; v JSR n,x
		ld	a,(de)
		inc	de
		ld	h,e
		ld	l,d
		push	hl
		exx
		ld	d,h
		ld	e,l		; save HL'
		ld	c,a
		ex	af,af'		; save flags
		add	hl,bc		; add n to x
		set	7,h
		ex	af,af'		; get flags
		ex	de,hl
		push	de
		exx
		pop	de
		main

		block	$/256*256+#c6-$,0
cc6		inc	de		; v LDAB #n
		ld	a,(de)
		ld	b,a
		inc	a
		dec	a		; sign test	
		b1main

		block	#d3-#d2,0
cd3		inc	de		; v ADDD zpnn
		ld	a,(de)
		ld	l,a
		ld	h,zp
		ld	a,(hl)
		inc	hl
		ld	l,(hl)
		ld	h,a
		ld	a,b
		ld	b,c
		ld	c,a
		add	hl,bc		; ADDD zpnn
		ld	c,h		; result in registers
		ld	b,l
		b1main			
ce9		xplusn			; v ADCB n,x
		ld	a,(hl)
		ex	de,hl		; get original HL'
		exx
		adc	a,b
		ld	b,a
		b1main
		dw	0
		block	#04-#02,0
c04		srl	c		; v LSRD
		rr	b
		b1main

cb1a		inc	de		; v CMPA nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	a,c
		cp	(hl)
		b1main


		block	10,0
c2a		jp	z,c20		; zero is also positive
		jp	p,c20		; v BPL
		inc	de
		b1main
;		block	#38-#37,0
c38		exx			; v PULX
		pop	de					
		ld	l,d
		ld	h,e
		exx
		b1main
error		inc	a
		ld	(16384),a
		jr	error
c4a		dec	c		; v DECA
		b1main

cf3a		ld	(cf3bc+1),bc		; v ADDD nnnn
		ex	de,hl
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	e,(hl)
		set	7,d
		ex	de,hl
		ld	b,(hl)
		inc	hl
		ld	c,(hl)
cf3bc		ld	hl,0
		ld	a,l
		ld	l,h
		ld	h,a
		add	hl,bc
		ld	c,h
		ld	b,l
		jr	c4a+1

		block	$/256*256+#6c-$,0
c6c		xplusn			; v INC n,x
		inc	(hl)
		ex	de,hl
		exx
		b1main
		dw	0
c83		ex 	de,hl		; v SUBD #nn
		inc	hl
		ld	d,c		; D=A
		ld	e,b		; E=B
		ld	b,(hl)
		inc	hl
		ld	c,(hl)
		ex	de,hl
		and	a
		sbc	hl,bc		; SUBD #nn
		ld	c,h		; result in registers
		ld	b,l
		b1main			
c97		inc	de		; v STAA zpnn
		ld	a,(de)
		ld	l,a
		ld	h,zp
		ld	(hl),c
		ld	a,c
		jp	keybtst

		block	#a6-#a1,0
ca6		xplusn			; v LDAA n,x
		ld	a,(hl)
		ex	de,hl
		exx
		ld	c,a
		inc	a
		dec	a
		b1main

		block	$/256*256+#c0-$,0
cc0		ex	de,hl		; v SUBB #n
		inc	hl			
		ld	a,b
		sub	(hl)
		ld	b,a
		ex	de,hl
		b1main

cbfa		inc	de		; v STS nnnn
		ex	de,hl
		ld	d,(hl)			
		inc	hl
		ld	e,(hl)
		set	7,d
		ex	af,af'
		push	hl
		ld	hl,#8000-1
		add	hl,sp
		ex	de,hl
		ld	(hl),d
		inc	hl
		ld	(hl),e
		ex	af,af'
		pop	de
		b1main
					

		block	$/256*256+#ea-$,0
cea		xplusn			; v ORAB n,x
		ld	a,(hl)
		ex	de,hl		; get original HL'
		exx
		jp	c,orbc
		or	b
		ld	b,a
		b1main

		block	$/256*256+#0a-$,0
c0a		push	af		; v CLV
		pop	hl
		res	2,l
		push	hl		
		pop	af
		b1main



		block	$/256*256+#2b-$,0
c2b		jp	m,c20		; v BMI
		inc	de
		b1main
		block	#3a-#36,0
c3a		ld	a,b		; v ABX
		exx
		ld	c,a
		ex	af,af'		; ABX does not effect flags
		add	hl,bc
		ex	af,af'		
		exx
		b1main

		block	$/256*256+#4c-$,0
c4c		inc	c		; v INCA
		b1main

cbaa		inc	de			; ORAA nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	a,(hl)
		jp	c,orcc
		or	c
		ld	c,a
		b1main


		block	$/256*256+#6d-$,0
c6d		xplusn			; v TST n,x	
		ld	a,(hl)
		cp	0
		ex	de,hl
		exx
		b1main
		db	0
c85		inc	de		; v BITA #n
		ld	a,(de)
		jr	nc,c85nc
		and	c
		scf
		db	62
c85nc		and	c
		jp	c01

		block	$/256*256+#94-$,0
c94		inc	de		; v ANDA zpnn
		ld	a,(de)
		ld	l,a
		ld	h,zp
		ld	a,(hl)
		jp	c,andcc
		and	c
		ld	c,a
		b1main

		block	#a7-#a6,0
ca7		inc	de		; v STAA n,x
		ld	a,(de)			; get n
		exx
		ld	d,h
		ld	e,l			; save HL'
		ld	c,a
		ex	af,af'			; save flags
		add	hl,bc			; add n to x, B always 0
		set	7,h

		exx
		ld	a,c
		exx
		ld	(hl),a
		jp	keybtst1
		ex	de,hl		; get original HL'
		exx
		b1main
;		dw	0
cc1		ex	de,hl		; v CMPB #n
		inc	hl			
		ld	a,b
		cp	(hl)
		ex	de,hl
		b1main
		block	#d4-#cd,0
cd4		inc	de		; v ANDB zpnn
		ld	a,(de)
		ld	h,zp
		ld	l,a
		ld	a,(hl)
		jp	c,andbc
		and	b
		ld	b,a
		jp	c01

		block	$/256*256+#e5-$,0
ce5		xplusn			; v BITB n,x
		ld	a,(hl)
		ex	de,hl		; get original HL'
		exx
		jr	nc,ce5nc
		and	b
		scf
		db	62
ce5nc		and	b
		jp	c01

		block	$/256*256+#ff-$,0
cff		inc	de		; v STX nnnn
		ld	a,(de)
		ld	h,a
		set	7,a
		exx
		ld	d,a
		exx
		inc	de
		ld	a,(de)
		exx
		ld	e,a
		ld	a,h
		ld	(de),a
		inc	de
		ld	a,l
		ld	(de),a
		exx
		b1main

c7aa		ex	de,hl		; v DEC nnnn
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	e,(hl)
		set	7,d
		ex	de,hl
		dec	(hl)
		b1main

		block	$/256*256+#2c-$,0
c2c		jp	z,c20		; v BGE
		jp	po,c20
		inc	de
		b1main				
		nop

c3b		pop	af		; v RTI
		pop	bc
		exx
		pop	de
		ld	h,e
		ld	l,d
		exx
		pop	de		
		set	7,d
		main

cb7a		inc	de			; STAA nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	(hl),c
		ld	a,c
		jp	keybtst

cb8a		inc	de		; v EORA nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	a,(hl)
		jp	c,xorcc
		xor	c
		ld	c,a
		b1main

		block	$/256*256+#6f-$,0
c6f		xplusn			; v CLR n,X
		xor	a
		ld	(hl),a
		ex	de,hl
		exx
		b1main
		dw	0
		nop

c88		inc 	de		; v EORA #n
		ld	a,(de)
		jp	c,xorcc
		xor	c
		ld	c,a
		b1main
		block	#98-#96,0
c98		inc	de		; v EORA zp
		ld	a,(de)
		ld	l,a
		ld	h,zp
		ld	a,(hl)
		jp	c,xorcc
		xor	c
		ld	c,a
		jp	c01
		block	$/256*256+#a8-$,0		
ca8		xplusn			; v EORA n,x
		ld	a,(hl)
		ex	de,hl		; get original HL'
		exx
		jp	c,xorcc
		xor	c
		ld	c,a
		b1main

		block	$/256*256+#c3-$,0		

cc3		ex 	de,hl		; v ADDD #nn
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	e,(hl)
		ex	de,hl
		ld	a,b
		ld	b,c
		ld	c,a
		add	hl,bc		; ADDD #nn
		ld	c,h		; result in registers
		ld	b,l
		b1main			

		block	$/256*256+#d7-$,0		
cd7		inc	de		; v STAB zpnn
		ld	a,(de)
		ld	l,a
		ld	h,zp
		ld	(hl),b
		ld	a,b
		jp	keybtst

		block	#e6-#e1,0

ce6		xplusn			; v LDAB n,x
		ld	a,(hl)
		ex	de,hl
		exx
		ld	b,a
		inc	a
		dec	a
		b1main
		db	0,0

		block	$/256*256+#0b-$,0

c0b		push	af		; v SEV
		pop	hl
		set	2,l
		push	hl
		pop	af
		b1main

		block	$/256*256+#1e-$,0
c1e		jp	error

		block	$/256*256+#2d-$,0
c2d		jp	m,c20		; v BLT
		inc	de
		b1main

; B'
cafa		inc	de			; STS n,x
		ld	a,(de)			; get n
		exx
		ld	b,h
		ld	c,l			; save HL'
		ld	d,0
		ld	e,a
		ex	af,af'			; save flags
		add	hl,de			; add n to x
		ex	de,hl
;		set	7,h
		ld	hl,#8000-1
		add	hl,sp
		ex	de,hl
;		res	7,d
		ld	(hl),d
		inc	hl
		ld	(hl),e
		ld	h,b
		ld	l,c
		ld	b,0
		exx	
		ex	af,af'			; get flags
		b1main






cdea		inc	de		; LDX	zpnn
		ld	a,(de)
		ld	h,zp
		ld	l,a
		ld	a,(hl)
		exx
		ld	h,a
		exx		
		inc	hl
		ld	a,(hl)
		exx
		ld	l,a
		jr	nc,cdenc	; opcode de without carry
		or	h
		scf
		exx		
		b1main


		block	$/256*256+#71-$,0

c71		nop			; v error
c72		jp	error		; v 
c75		jp	error		; v
c78		ex de,hl		; v ASL nnnn
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	e,(hl)
		set	7,d
		ex	de,hl
		sla	(hl)
		b1main
c89		inc 	de		; v ADCA #n
		ld	a,(de)
		adc	a,c
		ld	c,a
		b1main
		block	#99-#94,0
c99		inc	de		; v ADCA zpnn
		ld	a,(de)
		ld	l,a
		ld	h,zp
		ld	a,c
		adc	a,(hl)
		ld	c,a
		b1main
		nop
ca9		xplusn			; v ADCA n,x
		ld	a,(hl)
		ex	de,hl		; get original HL'
		exx
		adc	a,c
		ld	c,a
		b1main
		dw	0
		block	#c5-#c2,0
cc5		inc	de		; v BITB #n
		ld	a,(de)	
		jr	nc,cc5nc		
		and	b
		scf
		db	62
cc5nc		and	b
		jp	c01

cdenc		or	h
		exx
		b1main


		block	$/256*256+#da-$,0
cda		inc	de		; v ORAB zpnn
		ld	a,(de)
		ld	h,zp
		ld	l,a
		ld	a,(hl)
		jp	c,orbc
		or	b
		ld	b,a
		jp	c01

		block	$/256*256+#eb-$,0
ceb		xplusn			; v ADDB n,x
		ld	a,(hl)
		ex	de,hl		; get original HL'
		exx
		add	a,b
		ld	b,a
		b1main

		dw	0
		block	#0c-#04,0
c0c		scf			; v CLC
		ccf
		b1main

c70a		ex	de,hl		; v NEG nnnn
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	e,(hl)
		set	7,d
		ex	de,hl
		ld	a,(hl)
		neg
		ld	(hl),a
		b1main


		block	#2e-#28,0
c2e		jp	z,c2efls	; v BGT
		jp	p,c20
c2efls		inc	de
		b1main


keybtst		ex	af,af'
		dec	l
		dec	l		; test possible write to P1DATA
		jr	z,zptest	; if so: continue test2



		ex	af,af'
		jr	c,keybcf
		or	a		
		b1main



keybtst1 	ex	af,af'
		dec	l
		dec	l
		ld	a,h
		ex	de,hl		; get original HL'
		exx
		jr	z,zptest+1
		ex	af,af'
		jr	c,keybcf
		or	a
		b1main

zptest		ld	a,h
		add	a,a		; test highbyte P1DATA zeropage = #80
		jp	z,readkb	; if so: read keyboard
		ex	af,af'
		jr	c,keybcf
		or	a
		b1main


		block	$/256*256+#70-$,0
c70		jp	c70a		; less open blocks in the emulator
c73		jp	c73a
c76		jp	c76a
		nop
c7a		jp	c7aa
		block	#84-#7d,0
c84		inc	de		; v ANDA #n
		ld	a,(de)	
		jp	c,andcc		
		and	c
		ld	c,a
		jp	c01

keybcf		or	a
		scf		
		b1main


		block	$/256*256+#9a-$,0
c9a		inc	de		; v ORAA zpnn
		ld	a,(de)
		ld	l,a
		ld	h,zp
		ld	a,(hl)
		jp	c,orcc
		or	c
		ld	c,a
		jp	c01

		block	$/256*256+#aa-$,0
caa		xplusn			; v ORAA n,x
		ld	a,(hl)
		ex	de,hl		; get original HL'
		exx
		jp	c,orcc
		or	c
		ld	c,a
		b1main

jperr		jp	error	
cc7		jr	jperr
cc9		inc	de		; v ADCB #n
		ld	a,(de)
		adc	a,b
		ld	b,a
		b1main
		block	#db-#d4,0
cdb		inc	de		; v ADDB zpnn
		ld	a,(de)
		ld	h,zp
		ld	l,a
		ld	a,b
		add	a,(hl)
		ld	b,a
		b1main
		block	#ec-#ea,0
cec		xplusn			; v LDD n,x 	X<->M:M+1
		ld	a,(hl)
		exx
		ld	c,a
		exx
		inc	hl
		ld	a,(hl)
		ex	de,hl
		exx
		ld	b,a
		jp	c,lddc
		or	c
		b1main

c74a		ex	de,hl		; v LSR nnnn
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	e,(hl)
		set	7,d
		ex	de,hl
		srl	(hl)
		b1main

c77a		ex de,hl		; v ASR nnnn
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	e,(hl)
		set	7,d
		ex	de,hl
		sra	(hl)
		b1main

			
		block	$/256*256+#74-$,0


c74		jp	c74a
c77		jp	c77a
		nop
c7b		jp	error
		block	#86-#7e,0

c86		inc 	de		; v LDAA #n 
		ld	a,(de)
		ld	c,a
		inc	a
		dec	a
		b1main

		block	$/256*256+#9b-$,0
c9b		inc	de		; v ADDA zpnn
		ld	a,(de)
		ld	l,a
		ld	h,zp
		ld	a,c
		add	a,(hl)
		ld	c,a
		b1main

		block 2,0

cac		xplusn			; v CPX n,x 	X<->M:M+1
		ld	a,(hl)
		inc	hl
		ld	l,(hl)
		ld	h,a
		ex	de,hl
		and	a
		sbc	hl,de		; HL=X, DE=(n,x)
		ex	de,hl
		exx
		b1main
		dw	0
		nop
ccb		inc	de		; v ADDB #n
		ld	a,(de)
		add	a,b
		ld	b,a
		b1main


		block	$/256*256+#dd-$,0		

cdd		inc	de		; v STD zpnn
		ld	a,(de)
		ld	h,zp
		ld	l,a
		ld	(hl),c
		inc	hl
		ld	(hl),b
		b1main

		block	#ee-#ec,0
cee		jp	ceea	
		nop	
cf2		jp	cf2a
cf5		jr	cf5a
cf7		jr	cf7a
cf9		jp	cf9a

cf5a		inc	de			; BITB nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	a,b
		jr	nc,cf5anc
		and	(hl)
		scf
		db	62
cf5anc		and	(hl)
		b1main

cf7a		inc	de			; STAB nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	(hl),b
		ld	a,b
		jp	keybtst

ca2a		xplusn				; SBCA n,x
		ld	a,(hl)
		ex	de,hl			; get original HL'
		exx
		ld	a,c
		sbc	a,h
		ld	c,a
		b1main

cb0a		inc	de			; SUBA nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	a,c
		sub	(hl)
		ld	c,a
		b1main


cb5a		inc	de			; BITA nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	a,c
		jr	nc,cf5anc		; same test there
		and	(hl)
		scf
		b1main



cb3a		push	bc		; SUBD nnnn
		ex	de,hl
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	e,(hl)
		set	7,d
		ex	de,hl
		ld	b,(hl)
		inc	hl
		ld	c,(hl)
		pop	hl
		ld	a,l
		ld	l,h
		ld	h,a
		and	a
		sbc	hl,bc
		ld	c,h
		ld	b,l
		b1main


caea		xplusn				; LDS n,x  ld sp,(nn)
		ld	a,(hl)
		inc	hl
		ld	l,(hl)
		ld	h,a
		set 	7,h
		ld	sp,hl
		inc	sp
		ex	de,hl
		exx
		b1main

		block	$/256*256+#9c-$,0

c9c		jp	c9ca
		block	3,0
ca2		jp	ca2a
		block #ae-#a5,0
cae		jr	caea
cb0		jp	cb0a
cb3		jr	cb3a
cb5		jp	cb5a
cb8		jp	cb8a
		block	#bf-#bb,0
cbf		jp	cbfa
		
		block	#c8-#c2,0

cc8		inc	de		; v EORB #n
		ld	a,(de)
		jp	c,xorbc
		xor	b
		ld	b,a
		b1main

icnt		db	4		

		block	$/256*256+#dc-$,0

; flag effected!!!
cdc		inc	de		; v LDD zpnn
		ld	a,(de)
		ld	h,zp
		ld	l,a
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
		ld	a,b
		jp	c,lddc
		or	c
		jp	c01

		block	$/256*256+#ed-$,0
ced		xplusn			; v STD n,x
		exx
		ld	a,c
		exx
		ld	(hl),a
		exx
		ld	a,b
		exx
		inc	hl
		ld	(hl),a
		ex	de,hl
		exx
		b1main

cb6a		inc	de			; v LDAA nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	c,(hl)
		inc	c
		dec	c
		b1main


cf0a		inc	de			; SUBB nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	a,b
		sub	(hl)
		ld	b,a
		b1main

; B'
ceea		inc	de		; v LDX n,x
		ld	a,(de)		; get n
		exx
		ld	c,a
		ex	af,af'		; save flags
		add	hl,bc		; add n to x
		set	7,h
		ex	af,af'			; get flags

		ld	a,(hl)
		inc	hl
		ld	l,(hl)
		ld	h,a
		exx
		b1main

cf2a		inc	de			; SBCB nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	a,b
		sbc	a,(hl)
		ld	b,a
		b1main




cf9a		inc	de		; v ADCB nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	a,b
		adc	a,(hl)
		ld	b,a
		b1main

cb4a		inc	de			; ANDA nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	a,(hl)
		jp	c,andcc
		and	c
		ld	c,a
		jp	c01

cb2a		inc	de			; SBCA nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	a,c
		sbc	a,(hl)
		ld	c,a
		b1main

;x
cf1a		inc	de			; CMPB nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	a,b
		cp	(hl)
		b1main



;x
cf6a		inc	de			; LDAB nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	b,(hl)
		ld	a,b
		inc	a
		dec	a
		b1main

		block	$/256*256+#af-$,0

caf		jp	cafa
cb2		jr	cb2a
cb4		jp	cb4a
cb7		jp	cb7a
cba		jp	cbaa

;x
cefa		xplusn				; STX n,x
		ld	a,(hl)
		inc	hl
		ld	l,(hl)
		ld	h,a
		ld	(hl),d
		inc	hl
		ld	(hl),e
		ex	de,hl
		exx
		b1main

		block	$/256*256+#de-$,0
cde		jp	cdea
		block	#ef-#e1,0
cef		jr	cefa
cf1		jr	cf1a
cf3		jp	cf3a
cf6		jr	cf6a
cf8		jr	cf8a
cfa		inc	de			; ORAB nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	a,(hl)
		jp	c,orbc
		or	b
		ld	b,a
		b1main



;x
cf8a		inc	de			; EORB nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		ld	a,(hl)
		jp	c,xorbc
		xor	b
		ld	b,a
		b1main

; keep table here within same highbyte
; bit reset is reading vertical keys
; bit is reset when a key is pressed

;      sh                ct
; bit  7  6  5  4  3  2  1  0 portvalue in #0002
;  5   /  .  -  ,  ;  :  9  8
;  4   7  6  5  4  3  2  1  0
;  3   SP EN          Z  Y  X
;  2   W  V  U  T  S  R  Q  P
;  1   O  N  M  L  K  J  I  H
;  0   G  F  E  D  C  B  A  @



keytab
	db #80,#04,#01,#08,#40	  	; ^ZXCV
	db #02,#08,#10,#40,#80		; ASDFG
	db #02,#80,#20,#04,#10		; QWERT
	db #02,#04,#08,#10,#20		; 12345
	db #01,#02,#01,#80,#40		; 09876
	db #01,#80,#02,#20,#02		; POIUY
	db #40,#10,#08,#04,#01		; nLKJH
	db #80,#02,#20,#40,#04		;  eMNB

shtab	
; symbolled table for 7 keys not with own key on ZX Spectrum
	db #80,#04,#01,#80,#80		; e:ee/ ^zxcv
	db #02,#08,#10,#40,#80		; eeeee asdfg
	db #02,#80,#20,#10,#40		; eeeee qwert
	db #02,#01,#08,#10,#20		; e@eee 12345
	db #01,#02,#01,#80,#40		; eeeee 09876
	db #01,#08,#02,#20,#02		; e;eee poiuy
	db #40,#20,#08,#20,#01		; eee-e nLKJH
	db #80,#02,#40,#10,#04		; eee.,  $mnb 
		
;debugging
brktst	ex af,af'
	and a
	ld hl,brkp
	sbc hl,de
	jr nz,cont
bpnt	jr 	cont		; debug breakpoint in spectrum emulator

cont 	ex af,af'
	ld a,(de)
	ld l,a
	ld h,hbyte/256
	ld h,(hl)
	jp (hl)		
	
keyrel	equ	keytab mod 256
	block $/256*256+keyrel+128-$	

; the keydatablock
	db #BF,#FF-08,#FF-08,255-01,255-04  	; ^ZXCV
	db 255-01,255-04,255-01,255-01,255-01	; ASDFG
	db 255-04,255-04,255-01,255-04,255-04	; QWERT
	db #FF-16,#FF-16,#FF-16,#FF-16,#FF-16	; 12345
	db #FF-16,#FF-32,#FF-32,#FF-16,#FF-16	; 09876
	db 255-04,255-02,255-02,255-04,255-08	; POIUY
	db 255-08,255-02,255-02,255-02,255-02	; nLKJH
	db #FF-08,#7f,255-02,255-02,255-01	;  eMNB
 
; the symbolled keydatablock for 7 keys data 
	db #BF-00,#FF-32,#7F-08,#3F-32,#FF-32	; e:KK/
	db #7F-01,#7F-04,#7F-01,#7F-01,#7F-01	; KKKKK
	db #7F-04,#7F-04,#7F-01,#3F-32,#3F-32	; KKK<>
	db #7F-16,#FF-01,#7F-16,#7F-16,#7F-16	; K@KKK
	db #7F-16,#7F-32,#7F-32,#7F-16,#7F-16	; KKKKK
	db #7F-04,#FF-32,#7F-02,#7F-04,#7F-08	; K;KKK
	db #7F-08,#3F-32,#3F-32,#FF-32,#7F-02	; nK+-K	nlkjh
	db #7F-00,#7F-00,#FF-32,#FF-32,#3F-32	;  e.,*


start		ld	(bassp+1),sp	; save return SP
		ld	hl,zptab	; clear zeropage, also set zeropage
clzp		ld	(hl),l		; to force
		inc	l		; hard
		jr	nz,clzp		; reset
		call	killscr		; delete backupscreen

		call	fastopc

		exx

		ld	hl,(#fffe)	; relocated startvector
		ld	e,h		; save lowbyte
		ld	d,l		; DE now relocated PC
		jp	c01+1

c9ca		inc	de			; CPX zp
		ld	a,(de)
		ld	l,a
		ld	h,zp
		ld	a,(hl)
		inc	hl
		ld	l,(hl)
		ld	h,a
		ld	(c9hl+1),hl
		exx
c9hl		ld	bc,0
		ld	d,h
		ld	e,l
		xor	a
		sbc	hl,bc
		ld	b,a
		ex	de,hl	
		exx
		b1main

cb6c		inc	de			; v LDAA nnnn
		ld	a,(de)
		set	7,a
		ld	h,a
		inc	de
		ld	a,(de)
		ld	l,a
		xor	a
		add	a,(hl)
		ld	c,a
		scf
		b1main

lddc		or	c		; test zero only!
		scf
		jp	c01

xorbc		xor	b
		ld	b,a
		scf
		b1main

xorcc		xor	c
		ld	c,a
		scf
		b1main

orcc		or	c
		ld	c,a
		scf
		b1main

orbc		or	b
		ld	b,a
		scf
		b1main

andcc		and	c
		ld	c,a
		scf
		b1main

andbc		and	b
		ld	b,a
		scf
		b1main


; fix v65
c49ov		ld	a,c
		rla
		ld	c,a
		inc	c
		dec	c
		b1main

c59ov		ld	a,b
		rla
		ld	b,a
		inc	a
		dec	a
		b1main

		nop
		nop
		nop
		nop


isr		push	af
		push	hl

		ld	hl,#800a
		inc	(hl)
		jp	nz,timeok
		dec	l
		inc	(hl)
timeok		ld	hl,icnt
		dec	(hl)
		jr	nz,exit		; no screen update
ifrq		ld	(hl),4
		push	de
		
		ld	h,cpscr/256	; shadowscreen
		ld	de,#c000	; TRS80 MC10 screen
		exx
		push	hl
		push	de
		push	bc
		ld	de,#4000	; preset start zx spectrum screen
		ld	b,#58		; preset start zx spectrum attribute 
nchar		exx
		ld	a,(de)		; get current char
		ld	l,e
		cp	(hl)		; test a change
		jp	z,noch		; same = no change = no write to screen

		ld	(hl),a		; save change
		exx			; prepare for write to screen
; set attribute	
		ld	c,e		; BC=attribute location
		ld	h,attrtab	; attribute table for chars 
		ld	l,a		; current character
		ld	a,(hl)		; get attribute for this character
		ld	(bc),a		; write attribute
		ld	c,d		; save current screenhighbyte

; copy character
		ld	h,22		; start of characterdata
		add	hl,hl
		add	hl,hl
		add	hl,hl		; *8, 8 data = 1 character

		ld	a,(hl)		; get data
		ld	(de),a		; write to screen
		inc	l		; next data
		inc	d		; next screenline
		ld	a,(hl)
		ld	(de),a
		inc	l
		inc	d
		ld	a,(hl)
		ld	(de),a
		inc	l
		inc	d
		ld	a,(hl)
		ld	(de),a
		inc	l
		inc	d
		ld	a,(hl)
		ld	(de),a
		inc	l
		inc	d
		ld	a,(hl)
		ld	(de),a
		inc	l
		inc	d
		ld	a,(hl)
		ld	(de),a
		inc	l
		inc	d
		ld	a,(hl)
		ld	(de),a
		ld	d,c		; undo changed D
		exx			; character written to screen

noch		inc	e		; next TRS screenposition
		exx
		inc	e		; next screenposition on ZX Spectrum
		jp	nz,nchar	; test end screenpart 
npart		ld	a,b		; get current screenpart
		ld	d,#48		; set D to next part
		inc	b		; set B to next attributes
		exx
		inc	h
		inc	d
		rra			; test bit screenpart
		jr	nc,nchar+1	; if not set then screen not ready
		exx
		pop	bc
		pop	de
		pop	hl
		exx
		pop	de
exit		pop	hl
		pop	af
		ei			; allow intrupts
		ret			; back to program


	block	#9513-$,0

prmenu	ld	a,(ifrq+1)
	ld	hl,less10
	cp	10
	jr	c,aless
	ld	(hl),47
pk10	inc	(hl)
	sub	10
	jr	nc,pk10
	inc	hl
	add	a,10
	

aless	add	a,48
	ld	(hl),a
	ex	de,hl
	inc	de
	ld	hl,dtxt
	ldi
	ldi
	ldi

	ld	hl,menutxt
prhl	ld	a,(hl)		
	rst	16
	inc	hl
	ld	a,(hl)
	inc	a
	jr	nz,prhl
	ld	a,8
	ld	(23658),a
	ret

dtxt	defm	" D "

menutxt	db	22,16,8
	defm	"SP-2-MC10 v1.12"
	db	22,17,5
	defm	"R reset emulator"
	db	22,18,5
	defm	"K show MC10 keyboard"
	db	22,19,5
	defm	"Q quit emulator"
	db	22,20,5
	defm	"U screenrefresh 1/"
less10	db	32,32,32,32,32	
	db	22,21,5	
	defm	"B back to emulator"
	db	255

frqdwn	ld	hl,ifrq+1
	dec	(hl)
	ret	nz
frqup	ld	hl,ifrq+1
	ld	a,(hl)
	cp	20
	ret	z
	inc	(hl)
	ret


readkb	ex	af,af'
	push	af
	exx
	push	hl

	ld	a,(#8002)	; get ports to be read
	cpl			; we need bits set
	ld	d,a		; save in D

	xor	a
	in	a,(254)
	or	224
	ld	c,a
	inc	a
	jr	z,keysup		; no keys pressed


	ld	hl,keytab	; point to default keytable
	ld	a,127		; port BNM-symbol-space
	in	a,(254)		; read port
	bit	1,a		; check symbolshift pressed
	jr	nz,tabok	; if not, read normal keys

	ld	a,254
	in	a,(254)		; read SHIFT-V
	rrca
	call	nc,menu
	ld	hl,shtab 	; point to symboltable

tabok	ld	e,254		; start port to check keypress
	ld	c,255		; default no key pressed
portlp	ld	a,e		; get next port on ZX Spectrum to read
	in	a,(254)		; read the port
	ld	b,5		; check 5 keys
keylp	rra			; rotate keypress into carry
	jr	c,nokey		; carry=no key pressed

	ex	af,af'		; keep port-data
	ld	a,d		; get MC10 ports 
	and	(hl)		; check in table matching port
	jr	z,noread	; if not, not a key now read by MC10

	set	7,l		; point to keypressdata MC10
	ld	a,(hl)		; get keyvalue
	and	c		; update status
	ld	c,a		; save new status
	res	7,l		; undo tablechange

noread	ex	af,af'		; back to port-data
nokey	inc	l		; point to next key in table (allways<256)
	djnz	keylp		; test full port
	rlc	e		; calculate next port
	jr	c,portlp	; stop when 8 ports checked

keysup	ld	a,c
	ld	(#bfff),a	; write keyresult for MC10

; now we need to set correct shiftmode	
nshft	or	128		; take off possible CONTROL-pressed
	inc	a		; test no key pressed
	jr	z,keych		; when shifted, skip storing key

	ld	a,c
	ld	(key1+1),a
	ld	(key2+1),a

keych	ld	a,d
	add	a,a
	jr	c,shft		; check shiftport

	cp	8
	jr	nz,nobrk

	ld	a,(key1+1)	; we need key value, not C
	sub	#b7		; test BREAK key
	jr	z,chs		; set BREAK

nobrk	ld	a,c		; undo change
	cp	#7f		; CONTROL only pressed?
	jr	z,key1		; CONTROL must be taken from keyvalue
	
	bit	7,a
	jr	z,newkey	; Control AND a key pressed

key1	ld	c,#ff		; get oldkey, needed for CONTROL
	ld	a,#ff		; we have done control, erase 
	ld	(key1+1),a	; old pressed key to prevent ghostkeys
newkey	ld	a,c		; get needed key
	and	192
	cp	%01000000
	jr	settest		

; 11 = no ctrl
; 01 = ctrl
; 10 = no ctrl shift from pressed
; 00 = no ctrl key shift always
skipct	or	2		; signal no ctrl and reset C
ctrl	ld	(#8003),a	; set ctrl key data
	jr	keydone

shft	ld	a,c
	sub	#b0
	jr	z,chs		; only shift makes #b0, set shift
	
key2	ld	c,0		; we need the pressed key for shift too
	ld	a,255
	ld	(key2+1),a	

	bit	6,c		
settest	ld	a,0
	jr	z,chs	
	ld	a,2
chs	ld	(#8003),a	; set status data

keydone	pop	hl
	exx
	pop	af
	b1main			; do next opcode

menu	ld	a,1		; GOTO BASIC line 256-511 where menu is
bk2bas	ld	hl,10072	; fixed value HL' in BASIC
	exx
	push	bc		; save registers
	push	de		; save PC
	ld	b,a		; set BASIC line
	ld	c,0
	ld	(emulsp+1),sp	; save SP for return
bassp	ld	sp,0		; get SP from BASIC
	im	1		; disable intruptroutine
	ret


; compressed keyboardscreen
ckb db  6,32,255,240,0,0,240,0,0,240
   db  0,0,240,0,0,240,0,0,240,0
   db  0,240,0,0,240,0,0,240,0,0
   db  240,0,0,255,255,248,0,1,248,0
   db  1,248,0,1,248,0,1,248,0,1
   db  248,0,1,248,0,1,248,0,1,248
   db  0,1,248,0,1,6,3,255,243,89
   db  218,240,0,56,240,25,200,240,0,32
   db  240,0,128,245,210,87,240,16,148,240
   db  174,164,240,0,24,245,87,38,255,255
   db  240,1,252,240,1,252,240,1,252,240
   db  1,252,240,1,252,240,1,252,240,0
   db  0,240,0,0,240,0,0,240,0,0
   db  6,3,255,240,0,184,240,0,232,245
   db  82,170,245,74,173,240,2,128,241,42
   db  40,240,0,60,240,0,62,240,0,0
   db  240,0,1,255,255,244,34,60,247,129
   db  4,247,129,252,244,1,228,243,129,60
   db  244,64,0,243,128,0,244,64,0,247
   db  192,0,240,0,1,192,0,0,240,0
   db  40,240,0,0,240,0,8,240,0,16
   db  240,0,28,240,0,8,240,0,0,240
   db  0,0,60,0,51,51,179,6,32,255
   db  241,0,64,243,129,32,243,128,136,241
   db  128,32,247,192,196,243,192,48,247,192
   db  24,243,128,16,243,128,32,243,128,0
   db  6,35,255,240,0,0,240,0,0,240
   db  0,0,240,0,0,240,0,0,240,0
   db  0,240,0,0,240,0,0,240,0,8
   db  240,0,2,255,255,248,0,1,248,0
   db  1,248,0,1,248,0,1,248,0,1
   db  248,0,1,248,0,1,248,0,1,248
   db  0,1,248,0,1,6,3,255,240,0
   db  64,240,0,16,243,36,108,245,105,173
   db  240,2,128,241,43,168,240,0,0,240
   db  0,8,240,0,60,240,0,1,255,255
   db  240,3,252,240,1,252,240,1,252,240
   db  1,252,240,1,252,240,0,0,240,0
   db  0,240,0,0,240,0,0,240,0,1
   db  198,76,236,240,24,68,240,0,48,240
   db  0,0,240,0,32,240,0,42,240,0
   db  16,240,0,0,243,85,220,60,0,42
   db  162,171,248,0,0,248,0,0,248,0
   db  0,248,0,0,248,0,0,248,0,0
   db  248,0,0,248,0,0,248,0,0,248
   db  0,0,255,255,243,0,64,244,65,32
   db  244,65,252,242,128,120,244,0,200,244
   db  0,72,240,64,8,244,64,32,244,64
   db  16,244,203,40,6,35,255,243,129,252
   db  244,65,252,247,129,252,247,129,252,247
   db  193,252,244,65,252,244,64,0,247,192
   db  0,243,128,16,247,128,0,6,35,255
   db  240,0,0,240,0,0,240,0,0,240
   db  0,0,240,0,0,240,0,0,240,0
   db  0,240,0,8,240,0,0,246,206,217
   db  255,255,248,0,1,248,0,1,248,0
   db  1,248,0,1,248,0,1,248,0,1
   db  248,0,1,248,0,1,248,0,1,248
   db  0,3,200,170,74,240,24,40,240,0
   db  16,240,0,8,240,0,64,240,0,8
   db  240,0,0,240,0,0,244,85,8,60
   db  0,50,186,171,240,101,96,240,1,192
   db  243,100,174,243,68,76,240,51,170,240
   db  68,220,243,78,76,240,13,12,243,50
   db  206,243,36,76,255,255,245,0,64,240
   db  130,64,241,128,136,244,128,160,247,128
   db  16,247,128,48,240,128,16,243,128,32
   db  244,64,16,245,74,168,6,3,255,248
   db  0,1,248,0,17,248,0,1,248,0
   db  1,248,0,1,248,0,1,248,0,1
   db  248,0,1,248,0,1,248,0,1,255
   db  255,244,65,252,244,65,252,244,1,252
   db  244,65,252,241,1,228,242,129,228,244
   db  64,0,241,0,0,244,64,0,244,64
   db  0,6,35,255,243,195,252,243,193,252
   db  247,129,252,247,192,0,243,129,252,244
   db  64,0,240,64,0,244,128,0,244,0
   db  0,244,164,149,6,32,255,200,170,76
   db  240,0,16,240,0,32,240,0,0,240
   db  0,0,240,0,0,240,0,0,240,0
   db  0,242,117,136,60,6,3,0,3,240
   db  85,80,240,2,32,244,138,168,244,74
   db  170,240,42,42,240,69,8,244,72,170
   db  240,17,16,242,170,164,244,84,170,255
   db  255,241,0,64,243,0,0,240,64,136
   db  247,224,112,240,64,32,244,64,84,241
   db  0,0,244,64,32,243,192,16,245,74
   db  168,6,3,255,242,25,208,240,0,40
   db  224,13,220,240,0,32,240,0,128,246
   db  207,91,240,28,152,240,206,174,240,0
   db  24,245,151,83,255,255,244,65,252,245
   db  65,252,247,129,252,244,65,252,241,1
   db  228,241,1,228,244,64,0,241,0,0
   db  244,64,0,244,64,0,6,3,255,248
   db  0,1,248,0,1,248,0,1,248,0
   db  1,248,0,1,248,0,1,248,0,1
   db  248,0,1,248,0,1,248,0,3,255
   db  255,244,35,196,244,1,228,244,65,28
   db  244,1,252,244,65,60,244,64,0,240
   db  64,0,245,0,0,244,0,0,246,164
   db  217,6,32,255,200,170,74,247,193,252
   db  244,65,252,243,129,252,244,65,252,247
   db  129,252,244,64,0,244,64,0,241,85
   db  8,60,6,3,0,3,240,101,80,240
   db  2,224,244,78,172,244,74,234,240,43
   db  42,240,68,136,244,76,236,240,17,8
   db  243,50,164,244,84,172,255,255,241,0
   db  0,244,0,0,244,64,136,240,128,40
   db  244,64,76,244,64,72,241,0,0,244
   db  64,32,240,64,16,246,74,168,6,3
   db  255,242,21,16,240,0,68,240,17,8
   db  240,0,64,240,0,64,245,146,182,240
   db  17,84,240,168,164,240,0,24,245,86
   db  84,255,255,245,65,252,245,65,196,244
   db  1,28,247,129,4,241,1,228,241,1
   db  228,244,64,0,241,0,0,244,64,0
   db  247,128,0,6,3,255,240,0,64,240
   db  0,16,243,34,172,246,126,182,240,2
   db  224,243,171,176,240,0,0,240,0,0
   db  240,0,0,240,0,1,255,255,244,35
   db  196,243,129,228,244,65,28,247,129,28
   db  244,1,60,247,192,0,240,64,0,246
   db  0,0,244,0,0,244,164,149,224,0
   db  1,248,0,1,248,0,1,248,0,1
   db  248,0,1,248,0,1,248,0,1,248
   db  0,1,248,0,0,126,6,3,0,7
   db  198,74,74,240,129,60,242,129,4,244
   db  65,4,244,65,4,244,65,4,246,64
   db  0,246,192,0,246,85,8,60,6,3
   db  0,3,240,85,80,240,2,160,244,42
   db  168,244,74,170,240,42,42,240,68,72
   db  244,72,170,240,17,4,242,42,164,244
   db  84,170,255,255,247,192,64,247,192,0
   db  243,129,252,240,128,240,243,128,140,243
   db  128,52,241,0,0,243,128,16,247,128
   db  32,243,138,144,6,3,255,242,21,144
   db  240,0,236,240,9,136,240,0,128,240
   db  0,32,246,210,187,240,25,88,240,172
   db  68,240,0,0,245,91,114,255,255,244
   db  193,252,245,65,196,244,1,28,244,129
   db  4,241,1,252,241,1,228,244,64,0
   db  241,0,0,244,64,0,244,0,0,6
   db  3,255,240,0,184,240,0,232,244,84
   db  170,245,74,173,240,2,128,241,42,40
   db  240,0,0,240,0,8,240,0,0,240
   db  0,1,255,255,247,226,60,240,65,228
   db  244,65,28,244,1,28,244,193,60,244
   db  64,0,240,64,0,245,0,0,244,0
   db  0,246,164,213,192,0,0,240,24,56
   db  240,0,48,240,0,24,240,0,4,240
   db  0,8,240,0,24,240,0,24,240,0
   db  0,60,0,51,57,43,192,0,0,241
   db  1,60,241,1,4,244,1,4,244,65
   db  4,247,129,4,245,64,0,245,64,0
   db  240,0,0,60,27,17,184,3,240,82
   db  80,240,2,96,243,202,78,243,100,172
   db  240,43,148,240,117,136,243,110,170,240
   db  13,216,242,42,164,243,38,74,255,255
   db  240,0,0,240,0,0,240,0,136,240
   db  0,0,240,0,0,240,0,0,240,0
   db  0,240,0,0,240,0,0,240,0,0
   db  6,3,255,242,21,16,240,0,40,240
   db  5,8,240,0,64,240,0,64,245,138
   db  182,240,17,84,240,168,164,240,0,24
   db  245,86,33,255,255,243,129,252,242,129
   db  196,247,129,28,244,65,4,241,1,252
   db  241,1,228,243,128,0,247,192,0,243
   db  128,0,244,0,0,6,3,255,240,1
   db  8,240,0,132,245,86,172,246,106,181
   db  240,2,192,241,59,40,240,0,0,240
   db  0,8,240,0,60,240,0,1,255,255
   db  244,34,60,240,65,4,244,65,252,244
   db  1,228,244,65,60,244,64,0,244,64
   db  0,244,128,0,244,0,0,240,0,1
   db  192,0,0,240,24,40,240,0,48,240
   db  0,4,240,0,8,240,0,42,240,0
   db  24,240,0,24,240,0,0,60,0,42
   db  162,171,192,0,0,241,1,60,241,1
   db  252,244,1,228,244,65,60,244,65,4
   db  244,192,0,245,64,0,240,0,0,60
   db  34,170,32,3,192,0,0,242,1,4
   db  242,129,252,244,65,228,242,129,60,244
   db  65,4,244,64,0,244,64,0,240,0
   db  0,60,19,58,48,3,169,87,108,206
   db  127,6,219,255,192,0,0,247,193,4
   db  244,65,252,243,129,228,241,1,60,247
   db  129,4,244,64,0,244,64,0,240,0
   db  0,60,10,42,32,3,171,6,3,85
   db  223,6,27,255,205,152,249,141,159,153
   db  159,205,141,252,136,159,202,127,55,63
   db  103,62,99,34,63,6,11,255,166,63
   db  54,166,126,98,43,242,54,127,50,126
   db  118,163,238,231,141,159,137,159,6,12
   db  255,173,168,231,124,173,152,202,143,42
   db  103,102,63,6,116,255,192,0,0,240
   db  1,252,240,1,252,240,1,252,240,1
   db  252,240,1,252,240,0,0,240,0,0
   db  240,0,0,60,50,41,184,3,153,21
   db  84,214,6,28,255,186,173,250,186,175
   db  170,175,186,218,251,219,175,186,190,234
   db  254,170,254,174,239,127,6,11,255,171
   db  126,234,170,190,174,235,239,106,190,234
   db  190,170,175,237,95,218,175,186,175,6
   db  12,255,170,171,234,187,170,173,186,222
   db  234,170,171,127,6,116,255,224,0,1
   db  248,0,1,240,0,1,248,0,1,248
   db  0,1,248,0,1,248,0,1,248,0
   db  1,248,0,0,126,6,3,0,7,171
   db  213,85,87,95,6,27,255,186,173,249
   db  152,175,154,175,170,218,253,217,159,218
   db  190,235,126,39,126,103,103,127,6,11
   db  255,171,127,106,170,190,102,103,247,106
   db  127,114,126,106,103,237,87,216,175,154
   db  175,6,12,255,136,169,234,189,138,157
   db  186,223,106,102,167,127,6,148,255,169
   db  26,109,76,6,28,255,186,173,154,186
   db  169,170,169,170,218,158,219,185,234,166
   db  235,166,171,166,175,175,127,6,11,255
   db  171,103,170,170,166,238,234,123,106,231
   db  186,166,234,174,109,84,218,169,186,175
   db  6,12,255,170,171,234,190,170,173,186
   db  223,170,238,171,79,6,180,255,205,173
   db  218,138,157,170,157,205,221,217,216,189
   db  154,183,54,118,166,118,162,99,127,6
   db  11,255,171,118,119,42,118,226,43,103
   db  118,246,122,182,246,163,102,230,218,173
   db  138,159,6,12,255,170,216,235,121,173
   db  173,204,222,114,239,107,79,6,182,255
   db  191,255,251,255,251,255,255,191,255,251
   db  255,239,255,239,255,239,6,15,255,239
   db  255,255,239,255,254,255,255,239,255,239
   db  255,254,255,253,255,251,6,142,255,6
   db  0,255,0


kbdisp	ld	hl,ckb	 	; compressed screen
	ld	de,16384	; spectrum screen
kbsh	ld	a,(hl)		; get current byte
	ld	b,1		; preset B for 1 copy
	cp	6		; test compresscode
	jr	nz,kbsh1	; if not display 1 character
	inc	hl		; skip compresscode
	ld	b,(hl)		; get repeater
	inc	hl		; goto data pointer
	ld	a,(hl)		; get byte
kbsh1	ld	(de),a		; display byte on screen
	inc	de		; goto next screenposition
	djnz	kbsh1		; repeat B times
	inc	hl		; goto next compressed byte
	ld	a,d		; test end of display
	cp	#50		; on spectrum screen 2/3
	jr	nz,kbsh		; continue until reached
	ld	hl,#5800	; attribute area
	ld	de,#5801	; next attribute
	ld	(hl),#39	; set attribute
	ld	bc,511		; copy 2 blocks
	ldir
	ret	

setz		ex	af,af'
		jr	nc,notc
		cp	a
		scf
		db	62
notc		cp	a
		b1main			

c76a		ex	de,hl		; v ROR nnnn
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	e,(hl)
		set	7,d
		ex	de,hl
		ld	a,(hl)
		rra		
c76c		inc	a
		dec	a		; test A=0 and keep carry
		ld	(hl),a
		b1main


c28m		jp	c,c20+1
		b1main

LF29Fs	call	LF29F
	jp	c39

LFFAB1	call	LFFAB		; call translated routine
	jp	c39		; perform RTS

; trigger #5e
LF160s	call	LF160		; call translated routine
	jp	c39		; perform RTS


playt	equ	8000		; the playtime of the note

LFFAB	di
	ld	a,b		; save duration
	ld	b,0		; subtract with C
	and	a		; clear carry
	ld	hl,800		; freq
	sbc	hl,bc		; freq of current note
	sbc	hl,bc		; freq of current note
	sbc	hl,bc		; freq of current note
	ld	c,b		; C now 0
	ld	b,a		; get duration
snd	ld	de,playt	; fixed playtime	
snd0	push	hl		; save freq
	ld	a,c
	xor	8
	ld	c,a		; toggle sound bit
	out	(254),a
snd1	inc	d
	dec	de
	dec	d
	jp	z,snd2		; end of duration?
	ld	a,r		;  9 same time
	jr	snd3		; 21
snd2	dec	b		;  4 test end
	jr	z,snd4		; 11
	ld	de,playt	; 21 get next duration 
snd3	dec	hl		; remaining freq
	ld	a,h
	or	l
	jr	nz,snd1
	pop	hl		; get next loop freq
	jr	snd0
snd4	pop	hl		; drop freq
	ei
	ret


fastopc	ld	a,#18
	ld	(#fe09),a		; CLOADM

	ld	a,#1a
	ld	(#f7d8),a

	ld	a,#cd			; CD opcode in ROM as extra start
	ld	hl,LF03FS
	ld	(#f03f),a
	ld	(#f040),hl

	ld	hl,LF04BS
	ld	(#f04b),a
	ld	(#f04c),hl

	ld	hl,LF060S
	ld	(#f060),a
	ld	(#f061),hl

	ld	hl,LF30BS
	ld	(#f30b),a
	ld	(#f30c),hl

	ld	hl,LE200S
	ld	(#e200),a
	ld	(#e201),hl

	ld	hl,LE51BS
	ld	(#e51b),a
	ld	(#e51c),hl

	ld	hl,LE549S
	ld	(#e549),a
	ld	(#e54a),hl

	ld	hl,LF9C9S
	ld	(#f9c9),a
	ld	(#f9ca),hl

;106
	ld	hl,LF251
	ld	(#F251),a
	ld	(#F252),hl

;107
	ld	hl,LEB1FS
	ld	(#EB1F),a
	ld	(#EB20),hl

	ld	hl,L00EBS
	ld	(#F7D0),a
	ld	(#F7D1),hl

;108
	ld	hl,LE1D8S
	ld	(#E1D8),a
	ld	(#E1D9),hl

;109
	ld	hl,LF886S
	ld	(#F886),a
	ld	(#F887),hl

	ld	hl,LF879S
	ld	(#F879),a
	ld	(#F87A),hl

	ld	hl,LF2D9S
	ld	(#F2D9),a
	ld	(#F2DA),hl

	ld	hl,LE21AS
	ld	(#E21A),a
	ld	(#E21B),hl

	ld	hl,LE9A4S
	ld	(#E9A4),a
	ld	(#E9A5),hl

	
		ld	a,isrvect/256
		ld	i,a
		im	2		; set im mode for screenupdate
		ld	b,0		; fixed value for B'=0
	ret

; code can not move anymore. why???

; FD6A breakpunt CLOAD
load	
	exx
	ld	a,2
	call	bk2bas
	exx
	jp	extra

	block 	#a10d-$,0
		
LF160	exx				; F160	ldd	1,x
	ld	d,h
	ld	e,l
	inc	hl
	set	7,h
	ld	a,(hl)
	exx
	ld	c,a
	exx
	inc	hl
	ld	a,(hl)
	exx
	ld	b,a

	ld	a,c			; F162	staa	FP1SGN
	ld	(#80db),a

	or	#80			; F164	oraa	#$80

	ld	h,b			; F166	std	FPA1
	ld	l,a
	ld	(#80d7),hl

	ld	a,(#80db)		; F168	ldab	FP1SGN

	ld	hl,#80ce		; F16A	eorb	FP0SGN
	xor	(hl)

	ld	(#80dc),a		; F16C	stab	RESSGN

	exx				; F16E	ldd	3,x
	inc	hl			
	ld	a,(hl)
	exx
	ld	c,a
	exx
	inc	hl
	ld	a,(hl)
	ld	h,d
	ld	l,e			; repair X
	exx

	ld	h,a			; F170	std	FPA1+2
	ld	l,c
	ld	(#80d9),hl

	exx				; F172	ldaa	,x
	set	7,h
	ld	a,(hl)
	ex	de,hl			; repair X
	exx
	ld	c,a

	ld	(#80d6),a		; F174	staa	FP1EXP

	ld	a,(#80c9)		; F176	ldab	FP0EXP
	ld	b,a
	or	a

	ret				; F178	rts

LF29F	ld	hl,(#80c9)
	ld	(#80d6),hl
	ld	c,l
	ld	b,h

	ld	hl,(#80cb)
	ld	(#80d8),hl

	exx
	ld	hl,(#80cd)
	ld	(#80da),hl
	ld	a,h
	ld	h,l
	ld	l,a
	exx

	xor	a
	or	c
	ret

LF03FS	pop	hl			; drop RETURN  
	call	LF03F
	jp	c39

LF04BS	pop	hl			; drop RETURN  
	call	LF04B
	jp	c39
 

LF03F	ld	hl,#80ca
	ld	a,(hl)		; COM FPA0
	cpl
	ld	(hl),a

	inc	hl
	ld	a,(hl)		; COM FPA0+1
	cpl
	ld	(hl),a

	inc	hl
	ld	a,(hl)		; COM FPA0+2
	cpl
	ld	(hl),a

	inc	hl
	xor	a		; COM FPA0+3
	inc	(hl)
	sub	(hl)
	ld	(hl),a

LF04B	exx
	ld	hl,(#80cc)
	inc	h
	jr	nz,LF04E
	inc	l	
LF04E	ld	(#80cc),hl
	ld	a,h
	ld	h,l
	ld	l,a

	jr	nz,LF057

	ld	hl,(#80ca)
	inc	h
	jr	nz,LF055
	inc	l	
LF055	ld	(#80ca),hl	; F055
	ld	a,h
	ld	h,l
	ld	l,a
LF057	exx
	ret			; F057

LE200S	pop	hl			; drop RETURN  
	call	LE200
	jp	c39


LF060S	pop	hl			; drop RETURN  
	call	LF060
	jp	c39

LF060	exx
	ld	d,h
	ld	e,l
	ld	c,4
	add	hl,bc
	ld	b,d
	ld	c,e
	set	7,h	
	ld	a,(hl)		; LDAA	4,X
	ld	(#80dd),a	; STAA 	FPSBYT
	ld	d,h
	ld	e,l
	dec	de
	ld	a,(de)		; LDAA	3,X
	ld	(hl),a		; STAA	4,X
	dec	hl
	dec	de
	ld	a,(de)		; LDAA	2,X
	ld	(hl),a		; STAA	3,X
	dec	hl
	dec	de
	ld	a,(de)		; LDAA	1,X
	ld	(hl),a		; STAA	2,X
	ld	h,b
	ld	l,c
	ld	a,(#80d5)	; LDAA SFTSGN
	ld	(de),a		; STAA 1,X
	ld	b,0
	exx
LF074	ld	a,b
	add	a,8
	ld	b,a
	
	jp	m,LF060
	jp	z,LF060

	ld	a,(#80dd)
	ld	c,a

	ld	a,b
	sub	8
	ld	b,a
	ret	z

LF07E	exx
	inc	hl
	set	7,h
	sra	(hl)
	dec	hl
	res	7,h
	exx
LF080	exx
	ld	d,h
	ld	e,l
	inc	hl
	inc	hl
	set	7,h
	rr	(hl)		; F080 ror 2,x			
	inc	hl
	rr	(hl)		; F082 ror 3,x
	inc	hl
	rr	(hl)		; F084 ror 4,x
	ld	h,d
	ld	l,e
	exx
	rr	c		; F086 rora
	inc	b		; F087 incb
	jr	nz,LF07E	; F088	bne	LF07E
	ret			; F08A	rts

LF30BS	pop	hl			; drop RETURN  
	call	LF30B
	jp	c39

LF30B	ld	a,(#80c9)	; F30B
	ld	b,a
	inc	a
	dec	a
	jr	z,LF350		; F30D
	sub	#a0		; F30F
	ld	b,a
	ld	a,(#80ce)	; F311
	bit	7,a		; F313
	jr	z,LF31B	
	ld	a,(#80d5)	; F315
	cpl	
	ld	(#80d5),a
	call	LF03F
LF31B	exx
	ld	hl,#00c9
	exx
	ld	a,b
	cp	256-8
	jp	nc,LF329
	call	LF074
	ld	a,0
	ld	(#80d5),a
	ret
LF329	ld	a,0
	ld	(#80d5),a
	ld	a,(#80ce)
	rl	a
	ld	c,a
	ld	hl,#80ca
	rr	(hl)	
	jp	LF080


LF350	ld	hl,#80ca
	ld	(hl),b
	inc	hl
	ld	(hl),b
	inc	hl
	ld	(hl),b
	inc	hl
	ld	(hl),b
	ret

LE200	ld	de,(#80bb)	; dstend
	ld	a,d
	ld	d,e
	ld	e,a
	set	7,d
	ld	hl,(#80bd)	; srcend
	ld	a,h
	ld	h,l
	ld	l,a
	inc	hl
LE20A	dec	hl
	set	7,h
	ld	a,(hl)
	ld	(de),a
	dec	de
	push	hl
	exx
	pop	hl
	res	7,h
	push	hl	
	ld	de,(#80c1)	
	and	a
	ld	a,d
	ld	d,e
	ld	e,a
	sbc 	hl,de
	pop	hl
	exx
	jr	nz,LE20A
	inc	de
	res	7,d
	ld	a,d
	ld	d,e
	ld	e,a
	ld	(#80bf),de
	ret

LE51BS	pop	hl	

LE51B	ld	hl,(#80f4)
	ld	(#80a9),hl
	ld	a,h
	ld	h,l
	ld	l,a
	set	7,h
	ld	c,(hl)
	ld	a,c
	or	a
	jr	z,LE52A
	cp	#3a
	push	hl	
	jr	z,LE53D
	pop	hl
	ld	de,#ea3c	; error
	main

LE52A	inc	hl
	ld	a,(hl)
	inc	hl
	or	(hl)
	ld	(#c283),a
	jr	nz,LE535
	ld	de,#E589
	main

LE535	inc	hl
	ld	c,(hl)
	inc	hl
	ld	b,(hl)	
	ld	(#80e2),bc
	res	7,h
	push	hl
	ld	a,h
	ld	h,l
	ld	l,a
	ld	(#80f4),hl
LE53D	exx
	pop	hl
	exx
	ld	de,#e53d
	main	

LE549S	pop	hl

LE549	bit	7,c
	ld	a,c
	jr	nz,LE54F
	ld	de,#e6d3
	jp	c01+1
LE54F	cp	#a0+1
	jr	nc,LE527
	ld	hl,#E148
	add	a,a
	ld	b,a
	ld	d,0
	ld	e,a
	add	hl,de
	ld	a,(hl)
	inc	hl
	ld	l,(hl)
	ld	h,a
	push	hl
	exx
	pop	hl
	exx
	ld	de,#e55b
	main

LE527	ld	de,#E527
	jp	c01+1		; Error, no need for speed with MAIN

LF9C9S	pop	hl
	call	LF9C9
	jp	c39

LF9C9	ld	a,(#80e8)
	or	a
	ret	nz		; no output support to other devices, do nothing

	push	bc	
LFA1B	ld	hl,(#c280)
	ld	a,h
	ld	h,l
	ld	l,a
	set 	7,h
	ld	a,c
	cp	8
	jr	nz,LFA2E
	and	a
	ld	de,#4000
	push	hl
	sbc	hl,de
	pop	hl
	jr	z,LFA77
	ld	a,#60
	dec	hl
	ld	(hl),a
	jr	LFA5C

LFA2E	cp	13
	jr	nz,LFA46
LFA35	ld	a,#60
	ld	(hl),a
	inc	hl
	ld	d,l
	ld	e,h
	res	7,e
	ld	(#c280),de
	ld	a,l
	and	31
	jr	nz,LFA35
	jr	LFA5C

LFA46	cp	32
	jr	c,LFA77
	bit	7,a
	jr	nz,LFA59
	cp	64
	jr	c,LFA57
	cp	96
	jr	c,LFA59
	and	#df
LFA57	xor	64
LFA59	ld	(hl),a
	inc	hl
LFA5C	ld	d,l
	ld	e,h
	res	7,e
	ld	(#c280),de

	ld	a,h
	cp	#c2

	jr	nz,LFA77

	ld	hl,#c020
	ld	de,#c000
	ld	bc,#200-32
	ldir
	ld	b,e
	ld	c,d
	res	7,c
	ld	(#c280),bc
	ld	a,96
clsf	ld	(de),a
	inc	e
	jr	nz,clsf

LFA77	pop	bc
	ret	

; aanpassing 106
LF19FS	pop	hl
	call	LF19F
	jp	c39

LF19F	call	LF29F	; MOVFP1
	ret	z
	ld	a,c
	add	a,2
	ld	c,a
	jp	c,OVRFLW
	ld	hl,#80dc
	ld	(hl),0
	ld	de,emEF89
	main
emEF89	db	#bd,#ef,#89	; jsr	LEF89
	db	#52
	ld	hl,#80c9
	inc	(hl)
	jp	z,OVRFLW
	ret

OVRFLW		ld	de,#f058
		jp	c01+1		; error

LF251	pop	hl
	call	LDFPA0
	jp	c39

LDFPA0	ld	h,c	; F251
	push	hl
	inc	sp
	exx
	set	7,h
	ld	c,(hl)
	inc	hl
	ld	a,(hl)
	exx
	ld	c,a
	exx
	inc	hl
	ld	a,(hl)
	exx
	ld	b,a
	ld	a,c
	ld	(#80ce),a
	or	128
	ld	c,a
	ld	(#80ca),bc
	ld	hl,#80dd
	ld	(hl),0
	exx
	ld	a,c
	exx
	ld	b,a
	exx
	inc	hl
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	ld	h,e
	ld	l,d
	ld	(#80cc),de
	exx	
	ld	a,b
	ld	(#80c9),a
	dec	sp
	pop	hl
	ld	c,h
	ret		

LEB1FS	pop	hl
	call	LEB1F
	jp	c39

LEB1F	ld	a,b
	ld	(#8083),a
	ld	a,c
	ld	(#80b1),a
	call	L00F3
	call	LEB76
	jr	nc,LEB2D

	ld	de,#ea3c
	jp	c01+1

LEB2D	ld	b,0
	ld	a,b
	ld	(#8084),a
	call	L00EB
	jr	c,LEB39
	call	LEB76
	jr	c,LEB43
LEB39	ld	b,c
LEB3A	call	L00EB	
	jr	c,LEB3A
	call	LEB76
	jr	nc,LEB3A

LEB43	ld	a,c
	cp	#24
	jr	nz,LEB4F
	ld	hl,#8084
	ld	a,(hl)
	cpl
	ld	(hl),a
	ld	a,b
	add	a,h
	ld	b,a
	call	L00EB
LEB4F	ld	a,b
	ld	(#80b2),a

	ld	a,(#8086)
	ld	b,a
	dec	b
	jr	nz,LEB59	

LEB56	ld	de,#EC07
	main			; continue emulated

LEB59	ld	a,(#8086)
	add	a,c
	sub	#28
	ld	c,a
	jr	nz,LEB62

LEB5F	ld	de,#EBDE
	jp	LEB56+3

LEB62	ld	hl,#8086
	ld	(hl),0

	exx
	ld	de,(#8095)
	ld	h,e
	ld	l,d
LEB67	ld	de,(#8097)
	ld	a,d
	ld	d,e
	ld	e,a
	push	hl
	and	a
	sbc	hl,de
	pop	hl
	exx
	jr	z,LEB7F
	ld	bc,(#80b1)
	exx
	ld	d,h
	set	7,h
	ld	a,(hl)
	exx
	ld	l,a
	exx
	inc	hl
	ld	a,(hl)
	dec	hl
	ld	h,d
	exx
	ld	h,a
	and	a
	sbc	hl,bc
	ld	c,h
	ld	b,h
	jr	z,LEBB0
	exx
	ld	c,7
	add	hl,bc
	jr	LEB67

LEB76	ld	a,c
	cp	#41
	ret	c
	sub	#5B
	sub	#A5
	ld	c,a
	ret

LEB7F	exx
	pop	de
	push	de

	and	a
	ld	hl,#4CEA
	sbc	hl,de
	ld	h,e
	ld	l,d
	exx
	jr	nz,LEB8A
	exx
	ld	hl,#ebb7
	exx
	ret
	
LEB8A	pop	hl
	ld	de,#EB8A
	main

LEBB0	exx
	inc	hl
	inc	hl
	ld	e,h
	ld	d,l
	ld	(#80b3),de
	exx
	ret

L00EBS	pop	hl
	call	L00EB
	jp	c39

L00F3S	call 	L00F3
	jp	c39

L00EB	ld	hl,#80f5
	inc	(hl)
	jr	nz,L00F3
	dec	hl
	inc	(hl)
L00F3	ld	hl,(#80f4)
	ld	a,h
	ld	h,l
	ld	l,a
	set	7,h
	ld	c,(hl)
LE1C8	ld	a,c
	cp	#3a
	ret	nc
	cp	32
	jr	z,L00EB
	sub	#30
	sub	#d0
	ld	c,a
	ret

LE1D8S	pop	hl
	call	LE1D8
	jp	c39


LE1D8	exx
	ld	hl,#8006	; 8000+4+2
	add	hl,sp
	exx
LE1DC	ld	b,18
	exx
	ld	d,l
	ld	e,h
	ld	(#8089),de
	set	7,h
	ld	a,(hl)
	sub	#80
	exx
	ld	c,a
	exx
	jr	nz,LE1FA
	inc	hl
	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
	ld	(#808b),hl
	ld	l,h
	ld	h,a
	ld	hl,#80b5
	ld	a,(hl)
	inc	hl
	ld	l,(hl)
	ld	h,a
	or	l
	jr	z,LE1F6
	ld	de,(#808b)
	ld	a,d
	cp	l
	jr	nz,LE1F2
	ld	a,e
	cp	h
	jr	z,LE1FA
LE1F2	exx
	call	LE22F
	jr	LE1DC

LE1F6	ld	hl,(#808b)
	ld	(#80b5),hl
	
LE1FA	ld	de,(#8089)
	ld	h,e
	ld	l,d
	exx
	ld	a,c
	cp	0
	ret

LE22F	ld	hl,(#8089)
	ld	a,b
	add	a,h
	ld	b,a
	ld	a,c
	adc	a,l
	ld	c,a
	ld	(#808b),bc
	exx
	ld	h,a
	exx
	ld	a,b
	exx
	ld	l,a
	exx
	ret

LF886S	pop	hl
	call	LF886
	jp	c39


LF879S	pop	hl
	call	LF879
	jp	c39
	
LF879	xor	a
	ld	(#8002),a	; clear old port read
	in	a,(254)		; no key = 1.111111 always 6 bits on
	ld	(#bfff),a	; clear any key pressed
	cpl
	and	31
	ret	z

LF886	exx
	push	hl
	exx
	push	bc
	inc	sp
	ld	c,#fb
	call	LF8D0	
	ld	b,c
	ld	a,b
	or	a
	jr	z,LF892
	ld	a,(#c23b)
	xor	b
	ld	b,a
LF892	ld	a,c
	ld	(#c23b),a
	ld	a,b
	or	a
	jr	z,LF89E

; debounce delay?????
	jr	LF8C8
LF89E	exx
	ld	hl,#4230
	exx
	ld	b,0
	dec	b
	ld	a,b
	ld	(#c239),a
LF8A6	rl	b
	jr	nc,LF8C6	
	ld	hl,#c239
	inc	(hl)
	call	LF8E2
	push	bc
	inc	sp
	ld	b,c
	ld	a,c
	exx
	ld	d,h
	inc	hl
	set	7,h
	xor	(hl)
	and	(hl)
	exx
	ld	c,a
	ld	a,b
	exx
	ld	(hl),a
	ld	h,d
	exx
	dec	sp
	pop	hl
	ld	b,h
	ld	a,c
	or	a
	scf
	jr	z,LF8A6


LF8BD	jp	LF8EA


LF8C6	ld	c,0
	db	33
LF8C8	ld	c,3
LF8CA	ld	a,c
	cp	0
	dec	sp
	pop	hl
	ld	b,h
	exx
	pop	hl
	exx
	ret	

LF8EA	ld	b,#f8
LF8EC	ld	a,b
	add	a,8
	ld	b,a
	ld	a,c
	srl	a
	ld	c,a
	jr	nc,LF8EC
	ld	hl,#c239
	ld	a,b
	add	a,(hl)
	ld	b,a
	ld	c,#fe
	call	LF8D0
	ld	a,c
	ld	(#c23a),a
	or	a
	jr	z,LF911
	exx
	ld	hl,#f97c
	exx
	ld	a,b
	cp	32
	jr	nz,LF92E
	ld	a,(#c282)
	add	a,16
	ld	c,a
	ld	(#c282),a
	jp	LF8C6

LF8CE	ld	c,#7f
LF8D0	ld	a,c
	ld	(#8002),a
	ld	de,endemul-1
	jp	readkb+1
endemul	db	#52
LF8D2	ld	a,(#8003)
	cpl
	and	2
	ld	c,a
	ret	z
	ld	c,255
	ret

LF8E2	ld	a,b
	ld	(#8002),a
	ld	de,noemul-1
	jp	readkb+1
noemul	db	#52
LF8E4	ld	a,(#bfff)
	or	192
	ld	c,a
	ret



	
LF911	ld	c,b
	ld	a,c
	or	a
	jr	z,LF919
	ld	a,b
	cp	27
	jr	c,LF933
	db	33
LF919	ld	b,29
	exx
	ld	hl,#f939
	exx
	call	LF8CE
	jr	z,LF92E
	exx
	ld	hl,#f94c
	exx
	ld	a,b
	cp	32
	jr	nz,LF92E
	ld	hl,#c21c
	ld	a,(hl)
	cpl
	ld	(hl),a
	jp	LF8C6

LF92E	ld	a,b
	exx
	ld	c,a
	add	hl,bc
	ld	a,(hl)
	exx
	ld	c,a
	jp	LF8CA

LF933	call	LF8CE
	ld	hl,#c21c
	ld	a,c
	xor	(hl)
	ld	c,a
	jr	nz,LF941
	ld	a,(hl)
	ld	c,a
	or	a
	jr	nz,LF946

	ld	a,b
	or	32
	ld	b,a
LF941	ld	c,b
	ld	a,c
	or	64
	ld	c,a
LF944	jp	LF8CA

LF946	ld	a,b
	exx
	ld	hl,#f9ab
	ld	c,a
	add	hl,bc
	ld	a,(hl)
	exx
	inc	a
	dec	a
	ld	c,a
	jp	p,LF8CA
	ld	a,(#c282)
	and	#70
	add	a,b
	ld	c,a
	jp	LF8CA

LF2D9S	pop	hl
	call	LF2D9
	jp	c39



LF2D9	exx
	ld	d,h
	set	7,h
	ld	a,(hl)
	or	a
	exx
	jr	z,LF2AD
	exx
	inc	hl
	ld	a,(hl)
	dec	l
	ld	h,d
	exx
	ld	hl,#80ce
	xor	(hl)
	ld	b,a
	jp	m,LF2B1
LF2E3	ld	a,(#80c9)
	exx
	ld	d,h
	ld	e,l
	set	7,h
	cp	(hl)
	jr	nz,LF306
	inc	hl		
	ld	a,(hl)
	or	#7F
	exx
	ld	hl,#80ca
	and	(hl)
	exx
	cp	(hl)
	jr	nz,LF306
	ld	a,(#80cb)
	inc	hl
	cp	(hl)
	jr	nz,LF306
	ld	a,(#80cc)
	inc	hl
	cp	(hl)
	jr	nz,LF306
	ld	a,(#80cd)
	inc	hl
	sub	(hl)
LF306	ld	h,d
	ld	l,e
	exx
	ld	b,a
	ret	z
	rra
	ld	hl,#80ce
	xor	(hl)
	rla
	ld	b,255	
	ret	c
	ld	b,1
	ret

LF2AD	ld	a,(#80c9)
	ld	b,a
	or	a
	ret	z
LF2B1	ld	a,(#80ce)
	ld	b,a
LF283	ld	a,b
	rla
	sbc	a,a
	ld	b,a	; C>  B=255
	ret	c
	inc	b	; NC> B=0+1
	ret

LE21AS	pop	hl
	call	LE21A
	jp	c39

LE21A	xor	a
	ld	c,a
	rl	b
	ld	hl,(#8099)
	ld	a,h
	add	a,b
	ld	b,a
	ld	a,c
	adc	a,l
	ld	c,a
LE21E	ld	a,b
	add	a,#3a
	ld	b,a
	ld	a,c
	adc	a,0
	ld	c,a
	jr	c,LE236
	ld	hl,#7fff
	add	hl,sp
	ld	a,b
	sub	l
	ld	a,c
	sbc	a,h
	ret	c
LE236	pop	hl
	ld	de,#E236
	jp	c01+1	

LE9A4S	ld	hl,(#80cc)	
	ex	(sp),hl		; drop RET, PUSH HL
	ld	hl,(#80ca)
	push	hl
	ld	hl,(#80c8)
	push	hl
	inc	sp
	exx
	ld	a,h
	exx
	set	7,a
	ld	d,a
	exx
	ld	a,l
	exx
	ld	e,a
	main



LE90E	db	#E6	; AND nn, reset carry
LE90F	scf
	ld	hl,#8084
	rr	(hl)
	jp	m,BVCC
	ccf
BVCC	ret	c
	ld	b,#18
	ld	de,#E238
	jp	c01+1


extra
	ld	de,(24574)		; possible machinecode address
	ld	bc,(24572)		; size of the program
	ld	h,e
	ld	l,d
	ld	(#c21f),hl		; set EXEC address
	ld	hl,#6000		; start of loaded game
	ld	a,d
	cp	64
	jr	nc,mload		; DE already loadaddress
	ld	de,(32915)		; BASIC must load here
	ld	a,d
	ld	d,e
	ld	e,a
mload	set	7,d
	ldir				; load into memory, B at end 0
	ex	de,hl			; HL now X
	res	7,h
	exx
	ld	de,#fd84		; continue with OK cursor
	jp	c,c01+1			; BASIC-exit
	jp	c39			; but RET when CLOADM

; CALL (CD) is errorroutine


; ex de,hl jp (hl) 
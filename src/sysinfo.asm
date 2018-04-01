;-------------------------------------------------
;MEMCHK
; Sysinfo and Memory check program by Kees van Oss 2012
;-------------------------------------------------
; Memory map Phill Harvey-Smith Nov/Dec 2015.
;-------------------------------------------------

if (ISROM=1)
	base = $E000
else
	base = $2900
endif

; If assembling for loading from MMC or emulator add ATM header
; If assembling for ROM then no header.
if ATM = 1
	hbase = base - 22			; offset for headder.
	org	hbase

	equs	"SYSINFO",0			; name zero padded to 16

	start	= P%
	for n, start, (hbase+15)
		equb 0
	next
			
	equw prog_start				; start address
	equw prog_start				; entry address
	equw prog_end-prog_start	; length
else
	hbase = base
	org base
endif

		include "..\src\atomdefs.asm"

;-----------------------------------------------------------------
; MEMORY CHECK:
;-----------------------------------------------------------------
;                           status
; USER RAM : 0000-0400  1kb   x0
;            0400-2800  9kb   x1
;            2800-2C00  1kb   x2
;            2C00-3C00  4kb   x3
;            3C00-4000  1kb   x4
;            4000-8000 16kb   x5
;
; VIDEO RAM: 8000-9800  6kb   x6
;            9800-A000  2kb   x7
;
;-----------------------------------------------------------------
; HARDWARE CHECK:
;-----------------------------------------------------------------
;           status
; 6522 VIA    y0
; AtoMMC      y1
; Joystick    y2
; SID         y3
; Turbo       y4
; AtomDOS     y5
; -           y6
; -           y7
;-----------------------------------------------------------------
verbose		= 1

.prog_start
	jmp		sysinfo1
	jmp		ramtest1
	
addr		= $80
userram		= $82
videoram	= $83
len			= $84
tmp			= $85
tmptab		= $86
status		= $87
total		= $88

stack1		= $16
stack2		= $25
stack3		= $34
stack4		= $43

.sysinfo1
if (verbose=1)
	lda 	$321
	sta 	tmptab
	lda 	#0
	sta 	$321
endif
	lda 	#0
	sta 	status

;-----------------------------------------------------------------
; Check USER RAM
;-----------------------------------------------------------------

if (verbose=1)
	lda 	#CLS					; clear screen
	jsr 	OSWRCH
	
	jsr		dashline
	
	jsr 	SYS_INLINE_PRINT
	EQUB 	"SYSTEM INFORMATION          V1.2"
	
	NOP
	jsr		dashline

	jsr 	OSCRLF
endif

.check_user_ram:
	lda 	#$4   ;+4		; Set initial USER RAM capacity 4 pages or 1K.
	sta 	total

	ldx 	#<$400		; Test RAM from $400-$8000
	ldy 	#>$400
	lda 	#($80-$4)
	jsr 	check_ram
	sta 	userram

if (verbose=1)
	jsr 	SYS_INLINE_PRINT
	EQUB 	"- USER  RAM: "
	
	NOP
	lda 	userram
	sta 	stack1
	lda 	#0
	sta 	stack2
	sta 	stack3
	sta 	stack4
	jsr 	prtdec
	
	jsr 	SYS_INLINE_PRINT
	EQUB 	" KB"
	
	NOP
	jsr 	OSCRLF
endif

;-----------------------------------------------------------------
; Check VIDEO RAM
;-----------------------------------------------------------------

.check_video_ram:
	lda 	#0			; Set initial video RAM capacity
	sta 	total
	ldx 	#<$8000		; Test video RAM from $8000-$A000
	ldy 	#>$8000
	lda 	#($A0-$80)
	jsr 	check_ram
	sta 	videoram

if (verbose=1)
	jsr 	SYS_INLINE_PRINT
	EQUB 	"- VIDEO RAM: "
	
	NOP
	lda 	videoram
	sta 	stack1
	lda 	#0
	sta 	stack2
	sta 	stack3
	sta 	stack4
	jsr 	prtdec
	
	jsr 	SYS_INLINE_PRINT
	EQUB 	" KB"
	
	NOP
	jsr 	OSCRLF
endif

;-----------------------------------------------------------------
; Check VIA 6522 present
;-----------------------------------------------------------------

timer	= $b809
delay	= $fe66

.chk_via:
if (verbose=1)
	jsr 	SYS_INLINE_PRINT		; MMC found
	EQUB 	"- VIA 6522 : "
	
	NOP
endif

	lda 	timer
	jsr 	delay
	cmp 	timer
	bne 	via_found
.via_not_found:
	clc
	jmp 	end_chk_via
.via_found:
	lda 	status		; Set bit0
	ora 	#%00000001
	sta 	status
	sec
.end_chk_via:
if (verbose=1)
	jsr 	print_yes_no
endif
;-----------------------------------------------------------------
; Check AtoMMC present
;-----------------------------------------------------------------

mmc_exxx	= $efd0
mmc_axxx	= $afd0

.chk_efd0:
if (verbose=1)
	jsr 	SYS_INLINE_PRINT
	EQUB 	"- ATOMMC   : "
	
	NOP
endif

	lda 	#<mmc_exxx		; Check if MMC at $Exxx
	sta 	addr
	lda 	#>mmc_exxx
	sta 	addr+1
	jsr 	check_mmc
	bcc 	chk_afd0
	jmp 	mmc_yes
	
.chk_afd0:
	lda 	#>mmc_axxx		; Check if MMC at $Exxx
	sta 	addr+1
	jsr 	check_mmc
	bcc 	mmc_no
	
.mmc_yes:
if (verbose=1)
	ldy 	#0				; Print MMC version
.mmc_loop:
	lda 	(addr),y
	cmp 	#$0d
	beq 	mmc_version
	jsr 	OSWRCH
	iny
	jmp 	mmc_loop
	
.mmc_version:
	jsr 	OSCRLF
endif

	lda 	status			; Set bit1
	ora 	#%00000010
	sta 	status
	jmp 	end_mmc_chk
	
.mmc_no:
if (verbose=1)
	clc
	jsr 	print_no		; No MMC found
endif
.end_mmc_chk:

;-----------------------------------------------------------------
; Check JOYSTICK
;-----------------------------------------------------------------

.chk_joystick:
if (verbose=1)
	jsr 	SYS_INLINE_PRINT
	EQUB 	"- JOYSTICK : "
	
	NOP
endif

	lda 	status		; Set bit2
	ora 	#%00000100
	sta 	status
if (verbose=1)
	sec
	jsr 	print_yes_no
endif

;-----------------------------------------------------------------
; Check SID
;-----------------------------------------------------------------

sid	= $bdc0		; SID base address
ws	= $90

.chk_sid:
if (verbose=1)
	jsr 	SYS_INLINE_PRINT
	EQUB 	"- SID      : "
	
	NOP
endif

	LDA 	#<sid
	LDX 	#>sid
	STA 	ws 					;any old zero-page address for pointer
	STX 	ws+1

	LDA 	#0 					;reset value
	LDY 	#7*2 				;start of voice 3 (frequency registers)
	STA 	(ws),Y 				; voice 3 freq. low
	INY
	STA 	(ws),Y 				; voice 3 freq. high
	INY
	STA 	(ws),Y 				; voice 3 pulse-width low
	INY
	STA 	(ws),Y				; voice 3 pulse-width high
	INY
	LDA 	#$41 				;gate voice, pulse waveform
	STA 	(ws),Y 				; gate voice 3
 
	LDY 	#$1B 				;voice 3 oscillator read-only register
	LDA 	#$FF 				;inversion of $00
	CMP 	(ws),Y
	BNE 	no_sid
 
	LDY 	#7*2+3 				;voice 3 pulse-width high
	STA 	(ws),Y 				;set to $FF
	LDY 	#$1B 				;voice 3 oscillator read-only register
	LDA 	(ws),Y 				;test inversion of $FF (test $00)
	BNE 	no_sid
 
;SID present!
	lda 	status		; Set bit3
	ora 	#%00001000
	sta 	status

    LDX 	#24
    LDA 	#0

.sid_loop:
	STA 	sid,x
    DEX
    BPL 	sid_loop

; main routine
    LDA 	#$02
	STA 	sid+$F
	LDA 	#$30
	STA 	sid+$12
	LDY 	#$00
	LDX 	#$00
	
.sl3:
	LDA 	sid+$1B
	BMI 	sl2
	DEX
	BNE 	sl3
	
	DEY
	BNE 	sl3
	BEQ 	sl4

.sl2:
	LDX 	#$01 				; 1 = 8580
.sl4:			
	bne 	d8580				; 0 = 6581

; we have 6581 found
if (verbose=1)
	jsr 	SYS_INLINE_PRINT
	EQUB 	"6581"
	NOP
endif
	jmp 	end_sid_found

; we have 8580 found
.d8580: 	
if (verbose=1)
	jsr 	SYS_INLINE_PRINT
	EQUB 	"8580"
	NOP
endif
.end_sid_found:
if (verbose=1)
	jsr		OSCRLF
endif
	jmp 	chk_turbo
	
.no_sid:
	clc
.end_chk_sid:
if (verbose=1)
	jsr 	print_yes_no
endif

;-----------------------------------------------------------------
; Check TURBO
;-----------------------------------------------------------------

flyback		= $b002

.chk_turbo:
if (verbose=1)
	jsr 	SYS_INLINE_PRINT
	EQUB 	"- TURBO    : "
	NOP
endif

	lda 	#0
	sta 	addr

	jsr 	delay
.inc_timer:
	inc 	addr
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	bit 	flyback
	bpl 	inc_timer

	lda 	addr
	cmp 	#145
	bcc 	no_turbo

	lda 	status				; Set bit4
	ora 	#%00010000
	sta 	status
	sec							; TURBO found 1,79 Mhz
	jmp 	end_chk_turbo
	
.no_turbo:
	clc							; No TURBO 1 Mhz
.end_chk_turbo:
if (verbose=1)
	jsr 	print_yes_no
endif

;-----------------------------------------------------------------
; Check AtomDOS
;-----------------------------------------------------------------

dosstring_8271	= $e010
dosstring_1770	= $e008

.chk_atomdos:
if (verbose=1)
	jsr 	SYS_INLINE_PRINT
	EQUB 	"- ATOMDOS  : "
	NOP
endif

	ldx 	#4
.dos_loop_8271:
	lda 	dosstring_8271,x
	cmp 	dos_text,x
	bne 	no_dos_8271
	dex
	bpl 	dos_loop_8271

	jsr		SYS_INLINE_PRINT
	EQUB	"YES: 8271"
	NOP
	jmp		dos_present
			
.no_dos_8271	
	ldx		#9
.dos_loop_1770:
	lda 	dosstring_1770,x
	cmp 	dos_text_1770,x
	bne 	no_dos
	dex
	bpl 	dos_loop_1770

	jsr		SYS_INLINE_PRINT
	EQUB	"YES: 1770"
	NOP
	
.dos_present
	lda 	status		; Set bit5
	ora 	#%00100000
	sta 	status
	sec					; ATOMDOS
	jsr		OSCRLF
	jmp 	end_chk_dos2
	
.no_dos:
	clc					; No ATOMDOS
.end_chk_dos:
if (verbose=1)
	jsr 	print_yes_no
endif
.end_chk_dos2:

;------------------

	jsr 	OSCRLF
	jsr 	SYS_INLINE_PRINT
	EQUB 	"USER: "
	NOP
	
	lda 	userram
	jsr		PRINT_HEXA

	jsr 	SYS_INLINE_PRINT
	EQUB 	" VIDEO: "
	NOP
	
	lda 	videoram
	jsr		PRINT_HEXA

	jsr 	SYS_INLINE_PRINT
	EQUB 	" PERP: "
	NOP
	
	lda 	status
	jsr		PRINT_HEXA

	jsr 	OSCRLF

if (verbose=1)
	lda 	tmptab
	sta 	$321
endif

	rts

;-----------------------------------------------------------------
.check_ram:
	stx 	addr		; Save parameters
	sty 	addr+1
	sta 	len

	ldy 	#0
.loop:
	lda 	(addr),y		; Save data
	sta 	tmp

	lda 	#$aa		; Set odd bits
	sta 	(addr),y
	lda 	(addr),y
	cmp 	#$aa
	bne 	no_ram

	lda 	#$55		; Set even bits
	sta 	(addr),y
	lda 	(addr),y
	cmp 	#$55
	bne 	no_ram
	
	inc 	total		; Was RAM so increment userram
.no_ram:
	lda 	tmp			; Restore data
	sta 	(addr),y

	inc 	addr+1
	dec 	len
	bne 	loop

	lsr 	total		; Divide by 4 to get KB
	lsr 	total
	lda 	total

	rts
;-----------------------------------------------------------------
.check_mmc:
	ldy 	#0
.chk_loop:
	lda 	(addr),y
	cmp 	mmc_text,y
	bne 	not_found
	
	iny
	cpy 	#6
	bne 	chk_loop
.mmc_found:
	sec
	rts
.not_found:
	clc
	rts

;-----------------------------------------------------------------
.print_yes_no:
	bcc 	print_no
	jsr 	SYS_INLINE_PRINT
	EQUB 	"YES"
	
	NOP
	jmp 	end_print

.print_no:
	jsr 	SYS_INLINE_PRINT
	EQUB 	"NO"
	
	NOP
.end_print:
	jmp 	OSCRLF

;-----------------------------------------------------------------
.mmc_text:
	EQUB 	"ATOMMC"
.dos_text:
	EQUB 	"DISK "
.dos_text_1770
	EQUB	"ADOS-1770 "                  


.dashline
	jsr 	SYS_INLINE_PRINT
	EQUB 	"--------------------------------"
	
	NOP
	rts

.ramtest1
	lda 	#CLS				; Clear screen
	jsr 	OSWRCH

	jsr		dashline
	jsr 	SYS_INLINE_PRINT
	EQUB 	"RAM MAP"
	NOP
	jsr 	OSCRLF
	jsr		dashline
	
	jsr 	SYS_INLINE_PRINT
	EQUB	"START     0123456789ABCDEF",CR,LF
	
	NOP
	lda		#$00				; start at $0000
	sta		addr
	sta		addr+1
	
.new_addr_line
	jsr		out_addr_line		; display address
	
.next_page
	ldy		#0					; check byte 0 of each page
	lda		(addr),y			; get byte at mem
	sta		tmp					; save it
	
	lda		#$55				; test patten 1
	sta		(addr),y			; save it
	lda 	(addr),y			; read it back
	cmp		#$55				; same ?
	
	bne		not_ram				; nope : not ram
	
	lda		#$aa				; test patten 2
	sta		(addr),y			; save it
	lda 	(addr),y			; read it back
	cmp		#$aa				; same ?
	
	bne		not_ram				; nope : not ram
	
	lda		#'1'				; flag ram there
	jmp		display_flag
	
.not_ram
	lda		#'0'				; flag no ram
	
.display_flag	
	jsr		OSWRCH				; display it
	
	lda		tmp					; restore saved value
	sta		(addr),y
	
	inc 	addr+1				; do next page
	lda		addr+1				; get MSB of address
	
	cmp		#$A0				; done all RAM?
	beq		test_done			; yes : exit
	
	sta		addr+1				; save address
	and		#$0f				; mask out top 4 bits
	beq		new_addr_line		; display next line
	
	jmp		next_page			; do next page
	
.test_done
	jsr		OSCRLF
	
	rts

.out_addr_line
	jsr		OSCRLF
	lda		#'#'					; print hex prefix
	jsr		OSWRCH
	
	lda		addr+1				; get MSB of addr
	jsr		display_page		; display it
	
	lda		addr				; get LSB
	jsr		hex_high_nibble		; high nibble
	
	lda		addr				; get LSB
	jsr		hex_nibble			; display it

	lda		addr+1				; get address being tested
	cmp		#$80				; in video RAM?
	
	bcc		prspace				; no : just print spaces
	
	jsr		SYS_INLINE_PRINT
	EQUB	" VID "
	NOP
	rts
	
.prspace
	jsr		SYS_INLINE_PRINT
	EQUB	"     "
	NOP
	rts
	
.display_page
	pha							; save page
	jsr		hex_high_nibble		; display high nibble
	lda		#'X'				; display X
	jsr		OSWRCH
	pla							; restore page
	rts
	
.hex_high_nibble
	lsr		a					; shift high nibble to low
	lsr		a
	lsr		a
	lsr		a
.hex_nibble
	and		#$0F				; mask off msb
	clc
	adc		#'0'				; convert to ASCII
	cmp		#'9'+1				; > 9
	bcc		hex_digit			; no display it
	
	adc		#('A'-'9')-2		; convert to letter
	
.hex_digit
	jmp		OSWRCH				; display it

; Pad rom to 4K.
if (ISROM=1)
	start = P%
	for n, start, (base+$FFE)
		equb $FF
	next 
endif

.prog_end


if (ATM = 1)
  save "sysinfo.atm", hbase, prog_end+1, prog_start
else
  if (ISROM)
    save "sysinfoe0.rom", prog_start, prog_end+1, prog_start
  else
    save "SYSINFO.DFS", prog_start, prog_end+1, prog_start
  endif
endif	
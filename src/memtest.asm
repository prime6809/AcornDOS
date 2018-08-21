;
; Memory test for System computers.
;
; Being unsatisfied witht the memory test in the Acorn supplied SYSTEST.
; I wrote this : 2018-09-12 PHS.
;
; Probes for memory at 8K boundries, if found tests and stops when a 
; failure is detected.
;
		include "../src/platinclude.asm"
		include "../src/wdfdc.asm"
		include "../src/intelfdc.asm"

if(ATOM=1)
		BASE	= $2900
else
		BASE	= $2800
endif
		org     BASE

VARSZP	= 		1

MemPtr	=		VTEMP				; Safe to use this as only used during format!
CkSum	=		VTEMP-2

if (VARSZP = 1)

if(ATOM=1)
WORK			= TMPWORK
endif

; Vars in Zero page
SaveByte		= 	TEMP1
WroteByte		= 	TEMP1+1
ReadByte		=	TEMP2
OffsetPtr		=	TEMP2+1
TablePtr		=	WORK
Cont			= 	WORK+1
endif

.BeebDisStartAddr
		jsr		INLINE_PRINT		; prompt for continuious test
if(ATOM=1)
		equs	"CONTINUOUS TEST? "
else
		equs	"Continuous test? "
endif
		NOP
		
		lda		#0
		sta		Cont				; Zero continue flag
		
		JSR     OSECHO				; get a char with echo
		cmp		#'Y'				; Y=yes
		beq		FlagCont			; nope : not continuous
		cmp		#'y'
		bne		TestAgain			; nope : not continuous

.FlagCont		
		inc		Cont				; flag continuous

.TestAgain
		jsr		DoCKSum				; Calculate and print checksum
		jsr		ProbeMem			; Probe for memory blocks available
		jsr		TestMem				; Test memory

		lda		Cont				; Should we repeat?
		bne		TestAgain
		
        RTS

.DoCKSum	
		lda		#HI(BeebDisStartAddr)	; Point to beginning of checksum range
		sta		MemPtr+1
		lda		#LO(BeebDisStartAddr)
		sta		MemPtr
		lda		#0						; zero checksum
		sta		CkSum				
		sta		CkSum+1	
		
		ldy		#0					; Zero pointer
.CKSumLoop
		lda		(MemPtr),y			; get a byte
		clc							; make sure carry clear
		adc		CkSum				; add to checksum
		sta		CkSum				; save lsb 
		bcc		NoCarry				; no carry, skip to next

		inc		CkSum+1				; Carry, deal with it
.NoCarry
		lda		MemPtr+1			; check MSB of end address
		cmp		#HI(LastCkSumAddr)	
		bne		CSumNext	

		cpy		#LO(LastCkSumAddr)	; Check LSB of address
		beq		CKSumDone
.CSumNext
		iny							; next byte
		bne		CKSumLoop			; end of page, increment page pointer
		inc		MemPtr+1
		jmp		CKSumLoop			; loop  again
		
.CKSumDone		
		jsr		INLINE_PRINT
if(ATOM=1)
		equs	"CHECKSUM: "
else		
		equs	"Checksum: "
endif
		NOP
		
		lda		CkSum				; Print checksum in hex
		jsr		PRINT_HEXA			
		lda		CkSum+1				; Print checksum in hex
		jsr		PRINT_HEXA			
		
		jsr		OSCRLF				; EOL
		
		rts

.ProbeMem
		ldy		#0
		ldx		#0					; Start of mem table
		lda		#0					; Zero offset
		sta		MemPtr			
.ProbeNext		
		lda		TestBlockTable,X	; get start of memory to test
		cmp		#$ff				; end of table?
		beq		ProbeExit			; yep exit
		
		sta		MemPtr+1
		
		lda		(MemPtr),y			; get byte from block to probe
		sta		SaveByte			; save it
		
		lda		#$55				; test val 1
		sta		(MemPtr),y			; try to save it
		cmp		(MemPtr),y			; is it the same?
		bne		NoMem				; no: no memory here 
		
		lda		#$AA				; test val 2
		sta		(MemPtr),y			; try to save it
		cmp		(MemPtr),y			; is it the same?
		bne		NoMem				; no: no memory here  
		
		lda		#1					; flag memory found
		sta		TestBlockTable+2,X	
.NoMem		
		lda		SaveByte			; restore original contents
		sta		(MemPtr),y

		lda		TestBlockTable+2,X	; get found flag
		beq		ProbeNextBlock		; not found don't print anything
		
		jsr		INLINE_PRINT		; display memory found
if (ATOM=1)
		equs	"FOUND MEMORY AT : $"
else
		equs	"Found memory at : $"
endif
		nop
		
		lda		TestBlockTable,X	; get start address
		jsr		PRINT_HEXA			; print address
		lda		#0
		jsr		PRINT_HEXA			; print address
		
		jsr		OSCRLF				; print EOL

.ProbeNextBlock		
		inx							; point to next entry
		inx	
		inx				
		
		jmp		ProbeNext			; go do next
		
.ProbeExit
		rts

.TestMemExit
		rts

;
; Test the identified memory blocks, several tests are done :
;	Read and write every bit combination $00..$FF
;	Walking 1 test $01..$80
; 	Walking 0 test $FE..$7F
;
; If a location fails then the program stops and displays the 
; location, byte written and byte read.
;
.TestMem
		ldy		#0
		ldx		#0					; Start of mem table
		lda		#0					; Zero offset
		sta		MemPtr			

.TestNextBlock
		lda		TestBlockTable,X	; get start of memory to test
		cmp		#$ff				; end of table?
		beq		TestMemExit			; yep exit
		sta		MemPtr+1

		lda		TestBlockTable+2,x	; Memory found in this block?
		bne		TestStart			; yes test it

.IncTestNextBlock
		inx							; move to next block in table
		inx
		inx	
		jmp		TestNextBlock

.TestStart
		jsr		INLINE_PRINT
if (ATOM=1)
		equs	"TESTING $"
else
		equs	"Testing $"
endif
		NOP
		
		lda		TestBlockTable,X	; get start of memory to test
		jsr		PRINT_HEXA
		lda		#$00
		jsr		PRINT_HEXA
		
		jsr		INLINE_PRINT
if (ATOM=1)
		equs	" TO $"
else
		equs	" to $"
endif
		NOP
		
		lda		TestBlockTable+1,X
		sec	
		sbc		#1
		jsr		PRINT_HEXA
		lda		#$FF
		jsr		PRINT_HEXA
		
; Check to see if we are currently trying to test the block of memory we 
; occupy, if so don't test it or bad things will happen!
; Since we are loaded at $2800, and most of the memory to be tested is higher
; than our address we test for the end address first as this will lead to
; a faster overall test.	

.TestNextByte	
		lda		MemPtr+1			; get MSB of pointer
		
		cmp		#HI(BeebDisEndAddr)+1
		bcs		DoTestNextByte		; higher do next byte
		bne		TestStartAddr		; not same page, so compare start addr
		
		tya							; get LSB of pointer
		cmp		#LO(BeebDisEndAddr)+1
		bcs		DoTestNextByte

; Pointer is lower than End of program so check to see if it is also lower
; than start of program, if so test may continue, otherwise pointer points
; to our code so skip test.		
.TestStartAddr		
		lda		MemPtr+1			; get MSB of pointer
		
		cmp		#HI(BeebDisStartAddr)	
		bcc		DoTestNextByte		; lower than beginning do next byte
		bne		SkipAddr			; , not same page, but lower than end so skip
		
		tya							; LSB is in y, as pointer points to start of page

		cmp		#LO(BeebDisStartAddr)
		bcs		SkipAddr			; if we get here then address is in range so skip
						
.DoTestNextByte
		lda		(MemPtr),y			; get byte from block to probe
		sta		SaveByte			; save it

; All byte values test
		lda		#$00				; initialize a
.ByteTestLoop
		jsr		DoTestMem			; test it and handle error
		clc							; clear carry
		adc		#1					; increment a
		bne		ByteTestLoop		

; Walking 1 loop
		lda		#$01				; initialize a
.Walking1TestLoop
		jsr		DoTestMem			; test it and handle error
		asl		a					; shift a
		bcc		Walking1TestLoop

; Walking 0 loop
		lda		#$FE				; initialize a
.Walking0TestLoop
		jsr		DoTestMem			; test it and handle error
		asl		a					; shift a
		ora		#$01				; make bottom bit 1
		cmp		#$FF
		bne		Walking0TestLoop
				
		lda		SaveByte			; restore original contents
		sta		(MemPtr),y
.SkipAddr		
		iny							; move to next byte to test
		bne		TestNextByte		; not zero on same page, loop again
		
		lda		#'.'				; Print a . for each page tested
		jsr		OSWRCH
		
		inc		MemPtr+1			; increment to next page
		lda		MemPtr+1			; check for end of block
		cmp		TestBlockTable+1,x	
		
		bne		TestNextByte		; nope do next

		jsr		INLINE_PRINT
		equs	" OK"
		NOP
	
		jsr		OSCRLF
		jmp		IncTestNextBlock	
		
.DoTestMem
		sta		(MemPtr),y			; write it
		sta		WroteByte			; save in our store
		lda		(MemPtr),y			
		cmp		WroteByte			; same ?
		bne		ByteError			; nope error!
		lda		WroteByte
		rts
		
.ByteError
		sta		ReadByte			; save byte read
		sty		OffsetPtr			; save offset and table pointers
		stx		TablePtr
		
		jsr		OSCRLF				; print EOL
		
		jsr		INLINE_PRINT
if (ATOM=1)
		equs	"ERROR AT LOCATION $"
else
		equs	"Error at location $"
endif
		nop
		
		lda		MemPtr+1			; print address MSB
		jsr		PRINT_HEXA
		lda		OffsetPtr			; print address LSB
		jsr		PRINT_HEXA
		
		jsr		INLINE_PRINT
if (ATOM=1)
		equs	" WROTE $"
else
		equs	" Wrote $"
endif
		nop
		
		lda		WroteByte			; print byte written
		jsr		PRINT_HEXA
		
		jsr		INLINE_PRINT
if (ATOM=1)
		equs	" READ $"
else
		equs	" Read $"
endif
		nop
		
		lda		ReadByte			; print byte read
		jsr		PRINT_HEXA
		
		jsr		OSCRLF				; print EOL
		
;		JSR     OSECHO				; read a character
;        CMP     #$51				; check for 'Q'
;		bne		Continue			; not Q continue test
		
;		tsx							; drop return address
;		inx
;		inx
;		txs
		
.Continue		
		ldx		TablePtr			; Restore offset and table pointers
		ldy		OffsetPtr
		lda		WroteByte
		rts
.LastCkSumAddr
		
.TestBlockTable
;				Start	End Page+1	
if (ATOM=1)
		equb	$04,	$20,	$00		; Extra ram e.g. on RAMROM baord
		
		equb	$20,	$28,	$00		; Extra RAM on DOS or RAMROM

		equb	$28,	$2C,	$00		; 1K base memory
		equb	$2C,	$30,	$00		; 2K base memory
		equb	$30,	$34,	$00		; 3K base memory
		equb	$34,	$38,	$00		; 4K base memory
		equb	$38,	$3C,	$00		; 5K base memory

		equb	$3C,	$40,	$00		; Extra RAM on DOS or RAMROM
	
		equb	$40,	$60,	$00		; Extra 8K e.g. on RAMROM
		equb	$60,	$80,	$00		; Extra 8K e.g. on RAMROM
		
		equb	$80,	$84,	$00		; 1K Video RAM
		equb	$84,	$88,	$00		; 2K Video RAM
		equb	$88,	$8C,	$00		; 3K Video RAM
		equb	$8C,	$90,	$00		; 4K Video RAM
		equb	$90,	$94,	$00		; 5K Video RAM
		equb	$94,	$98,	$00		; 6K Video RAM
		equb	$98,	$A0,	$00		; Extra 'video' RAM
		equb	$FF,	$FF,	$FF
else
;		equb	$00,	$04,	$00		; All systems and Atoms have 1K at bottom of map
		equb	$20,	$40,	$00		
		equb	$40,	$60,	$00
		equb	$60,	$80,	$00
		equb	$80,	$A0,	$00
		equb	$A0,	$C0,	$00
		equb	$C0,	$E0,	$00
		equb	$FF,	$FF,	$FF
endif

if (VARSZP = 0)
; saved byte from location being tested
.SaveByte		
		equb	$00
.WroteByte
		equb	$00
.ReadByte
		equb	$00
.OffsetPtr
		equb	$00
.TablePtr
		equb	$00
endif
.BeebDisEndAddr

SAVE "MEMTEST.DFS",BeebDisStartAddr,BeebDisEndAddr

;
; Here we include and assemble the system rom, this way we can access it's symbols
; however the assembled copy is not saved (as ISROM=0)
;
         include "../src/rominclude.asm"

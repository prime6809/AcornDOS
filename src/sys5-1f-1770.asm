; SYS5-1F/SRC
; Source for System 5 DOS and Kernel Issue 1F
; Source recreation by J.G.Harston
;
; Ported to BeebASM P.Harvey-Smith.
;
; Large parts of this are identical to AtomDOS (and to some extent DFS), so 
; as AtomDOS has been disassembled / commented, where appropreate I have 
; copied the code / labels / comments from the AtomDOS source, checking to 
; see that it still assembles to a binary identical copy to the dumped ROMs.
;
; For 80 column display this was checked against the the system 5 rom dump 
; available from :
;
; http://mdfs.net/System/ROMs/AcornMOS/System/System5-1F
;
; For 40 column display this was checked against a dump of the System 3 
; unit owned by Dave Moore, available from :
;
; http://mdfs.net/System/ROMs/AcornMOS/System/TOSDOS-S3
;

; The value of SYS40 determines what system / screen we are building for.
; SYS40 = 0 if building for a system with a 80 column mono display
; SYS40 = 1 if building for a system with a 40 column Teletext display.
 
;SYS40	= 1	

	ORG		&F000
	GUARD	&0000

if(ISROM)	
	include "..\src\sysdefs.asm"
	include "..\src\wdfdc.asm"
endif


if (SYS40 = 0)
print "80 Column!"
; 80x25 Monochrome display
SCREENBASE	= &1000						; Screen RAM base
SCREENLINES	= 24						; number of lines on screen
SCREENCOLS	= 80						; number of columnns on screen
SCREENIO	= &1840						; Screen IO, address of 6845
SCRADDRREG	= SCREENIO + 0
SCRSTATUSREG= SCREENIO + 0
SCRDATAREG	= SCREENIO + 1
else
print "40 Column!"
; 40x25 Colour Teletext display.
SCREENBASE	= &0400						; Screen RAM base
SCREENLINES	= 24						; number of lines on screen
SCREENCOLS	= 40						; number of columnns on screen
SCREENIO	= &0800						; Screen IO, address of 6845
SCRADDRREG	= SCREENIO + 0
SCRSTATUSREG= SCREENIO + 0
SCRDATAREG	= SCREENIO + 1
endif

print "Screen base:",~SCREENBASE
print "6845 at    :",~SCREENIO
print "Geometry   :",SCREENCOLS,"x",SCREENLINES 

; 6845 CRTC registers, accessed by writing following to the address reg
; then reading/writing data from the data reg.

HTOTAL		= 0							; Horizontal total
HDISP		= 1							; Horizontal displayed
HSYNC		= 2							; Horizontal sync pos
SYNCWIDTH	= 3							; Horizontal & Vertical sync widths
VTOTAL		= 4							; Vertical total
VADJUST		= 5							; Vertical total adjust
VDISP		= 6							; Vertical displayed
VSYNC		= 7							; Vertical sync pos
MODECTRL	= 8							; Mode control
SCANLINE	= 9							; Scanlines / character row
CSTART		= 10						; Cursor start
CEND		= 11						; Cursor end
DSTARTH		= 12						; Display start address high
DSTARTL		= 13						; Display start address low
CPOSH		= 14						; Cursor position high
CPOSL		= 15						; Cursor position low
LPENH		= 16						; light pen pos high
LPENL		= 17						; light pen pos low

;=======================================
; Westewrn digital WD177x Floppy controller
; WD177x regsiter base
;=======================================

IOBASE		= $0A80						; keep out of way of Intel FDC.

; Set this to 1 if using the WD1770 or WD1772, zero otherwise
ISWD177X	= 1

if (ISWD177X) 
FDC1797 	= 0 		              
else
; Set this to 0 if using 1793, 177x, 2793
; Set this to 1 if using the 1797 or 2797.
FDC1797 	= 0 		              
endif

STEPRATE	= WCF_RATE12                           
WRESTC		= WCMD_RESTORE 	or WCF_NO_SPINUP or STEPRATE                     
WSEEKC		= WCMD_SEEK 	or WCF_NO_SPINUP or WCF_VERIFY or STEPRATE                     
WRSCTC		= WCMD_READ_SEC or ((FDC1797)*8)                
WWSCTC		= WCMD_WRITE_SEC or ((FDC1797)*8)                
;WRTRCC		= WCMD_READ_TRACK or WCF_DELAY                              
WWTRCC		= WCMD_WRITE_TRACK or WCF_DELAY                              
WRDRSC		= WCMD_READ_ADDR						;$C0                              
WSTPIC		= WCMD_STEPIN or WCF_UPDATE or STEPRATE                             

; I/O locations WD177x 

WDBASE		= IOBASE
WSTATUS		= WDBASE + WDSTATUS			; Status / command register
WCMD   		= WDBASE + WDCMD		
WTRACK 		= WDBASE + WDTRACK	 		; Track register                         
WSECTOR		= WDBASE + WDSECTOR			; Sector register                          
WDATA 		= WDBASE + WDDATA			; Read / Write Data register

PCTRL		= IOBASE + 4				; Peripheral control register

; Peripheral control lines.
; When writing bits 6 and 7 are ignored (and are unconnected).
; When reading the bits 0..5, will return the currently written values.
; outputs 
PSIDE		= $01						; Side select mask
PDS0		= $02						; Drive select 0
PDS1		= $04						; Drive select 1
PDDEN		= $08						; Double density enable (low)
PRESET		= $10						; Reset FDC
P4080		= $20						; 40(low)/80(high) track switch
;inputs
PINDEX		= $40  						; Index pulse????
PINTRQ      = $40                       ; INTRQ from WD177x
PDRQ		= $80						; DRQ from WD177x

DRVSID_MASK	= PDDEN + PRESET + P4080	; Mask to zero drive & side bits

; *********************
; SYSTEM DOS AND KERNEL
; *********************

; Print "Disk " followed by inline text
; =====================================

.LF000 
		JSR INLINE_PRINT
		EQUS "Disk "
		NOP

; Print inline text
; =================

.INLINE_PRINT
		pla                             ; Get textpointer from return addresss
        sta     TEXTPTR                     
        pla                             
        sta     TEXTPTR+1                 
		
		LDY 	#&00					; zero offset
.LF011
		inc     TEXTPTR					; increment LSB of ptr                    
        bne     LF017                   ; no overflow, skip on
        inc     TEXTPTR+1				; increment MSB of ptr
.LF017
		lda     (TEXTPTR),Y				; get character to print                 
        bmi     LF023                   ; end of string, yep exit
        beq     LF023                   ; null byte (BRK) : exit
        jsr     OSWRCH               	; print it
		JMP 	LF011 					; Print text and loop for next

.LF023
		jmp     (TEXTPTR)               ; Return from call

; Read a line
; ===========
.LF026
		JSR 	OSCRLF					; Move to next line
.LF029
		JSR 	INLINE_PRINT
		EQUS 	"*"  					; Print '*' prompt
.READ_LINE		
.LF02D
		LDX 	#&FF           			; Initial line length = zero-1
.LF02F
		INX                 			; Inc. line length
		CPX 	#&40
		BCC 	LF037  					; If <64, read more characters
.LF034
		DEX
		BMI 	LF02D       			; Dec. line length, restart if at start
.LF037
		JSR 	OSECHO			        ; Wait for keypress and echo it
		CMP 	#CTRLX
		BEQ 	LF026  					; Ctrl-X - abandon line
		
		CMP 	#DELETE
		BEQ 	LF034  					; Delete - delete one character
		
		STA 	&0100,X         		; Enter into text buffer
		CMP 	#CR
		BNE 	LF02F  					; Loop until <cr>
		RTS

; Print A in hex
; ============== 
.PRINT_HEXA
		PHA                 			; Save A
		JSR 	diva16           		; Get MS nibble
		JSR 	PRINT_HEXA_LOWN         ; Print it
		PLA                 			; Restore it
.PRINT_HEXA_LOWN
		AND 	#&0F           			; Make sure A in range  
		CMP 	#&0A           			; Less than 10 ? 
		BCC 	LF05A          			; Yes : print it
		ADC 	#&06           			; No convert to letter A..F  
.LF05A
		ADC 	#&30           			; Convert to ASCII
		JMP 	OSWRCH         			; Print it
 
;============================================
; Read filename into $100 and check end of command
; Zeropage $D7 = command pointer
;--------------------------------------------
 
.LF05F	sty     INBUFIDX                ; Save commandpointer
        jsr     READFNAME               ; Read filename
        jsr     CHECK_FNPARAM           ; Check end of command
        ldy     INBUFIDX                ; Load commandpointer
        ldx     #$00                    ; Store filename at $100
		BEQ 	LF06F           		; Jump always?  

.READFNAME
		ldx     #$40                    ; Pointer at $140
.LF06F
		JSR 	SKIPSPACE           	; Skip spaces 
		STX 	FILENAMEPTR             ; F072 86 DD     .]  
        cmp     #DUBQUOTE               ; Is character double quote?
		BEQ 	LF0BB           		; Yep : read filename between ""

.LF078
		cmp     #CR                     ; Check end of line
		BEQ 	LF088          			; Yep !
		
		sta     STACKPAGE,X				; copy sting from STACKPAGE,y -> STACKPAGE,x                  
        inx                             
        iny                             
        lda     STACKPAGE,Y                  
        cmp     #SPACE                  ; Space, end reading
        BNE 	LF078           		  

.LF088
		lda     #CR                     ; End filename with $0D
        sta     STACKPAGE,X                  
        lda     #>STACKPAGE             ; Filenamepointer=$140
        sta     FILENAMEPTR+1                     
		LDX 	#FILENAMEPTR            
.LF093
		RTS                 

;============================================
; Copy filedata to $9A-$A3
; X = pointer to filedata ($DD=DOS, $??=COS)
;--------------------------------------------

.copy_check
		ldy     #$00					; Zero count / dest ptr     
.LF096
		lda     $00,X      				; get a byte from source             
        sta     FILENAMEPTR,Y          	; move to dest         
        inx                    			; advance pointers / count         
        iny                             
        cpy     #$0A    				; done all ?   
		BCC 	LF096           		; nope loop again.

;============================================
; Fill temp filename $E8-$EE with spaces
;--------------------------------------------

.LF0A1
		lda     #SPACE        			; Char to fill : space            
        ldy     #$06         			; count        
.LF0A5
		sta     FILENAME,Y      		; put it in             
        dey                    			; decrement count  
		BPL 	LF0A5          			; loop if more  

;============================================
; Check if len filename<8 copy to $EE
;--------------------------------------------

.LF0AB
		iny                           	; increment pointer  
        lda     (FILENAMEPTR),Y 		; get a char from name                
        cmp     #$0D                    ; EOL?
		BEQ 	LF093        			; Name is legal

		cpy     #$07                    ; More than 7 chars?
		BCS 	LF0D7           		; NAME? error  

		sta     FILENAME,Y         		; copy it
		BNE 	LF0AB           		; loop again

;============================================
; Store filename between "" at $100
;--------------------------------------------

.LF0BB
		iny                             ; increment pointer
        lda     STACKPAGE,Y             ; get character
        cmp     #CR                   	; EOL ? 
		BEQ 	LF0D7           		; yep : error  

        sta     STACKPAGE,X             ; save in dest
        inx                             ; increment dest pointer
        cmp     #DUBQUOTE               ; is it a double quote ?
		BNE 	LF0BB           
		
        dex                   			; change pointers          
        iny                             
        lda     STACKPAGE,Y                  
        cmp     #DUBQUOTE               ; is it a double quote ?
		BNE 	LF088           

		inx                             ; inc ptr 
		BCS 	LF0BB                   ; loop again

.LF0D7
		JSR INLINE_PRINT           		; NAME? error
		EQUS "Name?"
		BRK

;
; Skip spaces in passed string
;
.LF0E0
		INY                 			; Increment pointer  
.SKIPSPACE
		LDA 	STACKPAGE,Y         	; Get a byte from string 
		CMP 	#SPACE            		; Is it a space?  
		BEQ 	LF0E0        			; Yes : loop again
		RTS                  

;============================================
; Read loadadres from $100,Y into $DF,$E0
;--------------------------------------------
 
.get_loadadr
		ldx     #LOADADDR         		; Loadadres 
.addr_to_x
		lda     #$00                    
        sta     $00,X                   ; Clear address
        sta     $01,X                   
        sta     $02,X                   ; Clear address given flag
		JSR 	SKIPSPACE           	; Skip spaces

.LF0F6
		lda     STACKPAGE,Y             ; Check if hex digit
        cmp     #'0'                    ; Digit between 0-9?
		BCC 	LF11E           

		cmp     #':'   
		BCC 	LF109           

		sbc     #$07                    ; Digit between $A-$F?
		BCC 	LF11E           

		cmp     #$40  
		BCS 	LF11E

.LF109
		asl     A                       ; Move it to high nibble
        asl     A                       
        asl     A                       
        asl     A  
		
		sty     $02,X                   ; Save address position

        ldy     #$04                    ; shift nibble into X and X+1 
.LF111
		asl     A                       
        rol     $00,X                   ; 16 bit shift through x and x+1
        rol     $01,X                   
        dey               
		BNE 	LF111             

		ldy     $02,X                   ; 4 Chars read?
        iny 
		BNE 	LF0F6           
		
.LF11E
		LDA 	&02,X           		; A=address position
		RTS                 
 
; Print a space
; -------------
.PRSPACE
		lda     #SPACE                    
        jmp     OSWRCH               
 
;============================================
; Print 6 spaces (entry PRSPACE6)
; Print Y speces (entry PRSPACEY)
;--------------------------------------------
.PRSPACE6
		LDY #&06             
.PRSPACEY
		jsr     PRSPACE                   ; Print space
        dey                             
        bne     PRSPACEY                   
        rts                             
 
;============================================
; Divide A by 32
; Divide A by 16
;--------------------------------------------
.diva32
		LSR A               ; F12F 4A        J   
.diva16
		LSR A               ; F130 4A        J   
		LSR A               ; F131 4A        J   
		LSR A               ; F132 4A        J   
		LSR A               ; F133 4A        J   
		RTS                 ; F134 60        `   
 
;============================================
; Increment Y x8
; Increment Y x7
; Increment Y x4
;--------------------------------------------
 
.incy8
		INY
.incy7
		INY
		INY
		INY
.incy4
		INY
		INY
		INY
		INY
		RTS
 
 
;============================================
; Decrement Y x8
;--------------------------------------------

.decy8
		DEY
		DEY
		DEY
		DEY
		DEY
		DEY
		DEY
		DEY
		RTS
 
;============================================
; Calculate startaddres, nr of tracks & sectors
;--------------------------------------------

.calc_start_addr
		lda     LOADADDR				; Get load address                     
;        sec                             ; add 1 with carry
;        sbc     #$01                    
        sta     STARTADDR            	; save in start addr         
        lda     LOADADDR+1              ; do MSB       
;        sbc     #$00                    
        sta     STARTADDR+1             

        lda     #$FF 					; Init trackno                   
        sta     TRACKNO                     
        clc                             

        adc     FILESIZE                ; Caclulate the number of sectors
        lda     FILESIZE+1                     
        adc     #$00                    

        sta     SSECCOUNT              	; save it       
        lda     STARTSEC                ; Startsector high byte
        jsr     diva16                  ; Divide A by 16
        sta     SECC256FLAG                     

        lda     STARTSEC                ; Get MSB of start sector     
        and     #$0F                    ; 
        tax                             
        lda     STARTSEC+1                     
.LF16F
		SEC                 
.LF170
		inc     TRACKNO                 ; Add one to track no    
        sbc     #SECTRACK				; Decrement sector count by sectors / track                    
        BCS 	LF170                   ; still more keep going
		
        dex                             ; if x > 0 then we have more than 256 sectors
		BPL 	LF16F                   ; so loop again 

        adc     #SECTRACK				; compensate for over loop, calculate sector on track  
        sta     SECTORNO                ; save it
.LF17D
		RTS                    
 
 ;============================================
; Check end of command
;--------------------------------------------

.LF17E
		JSR 	CHECK_FNPARAM           
	
;============================================
; Check if filenaam legal and in cat
;--------------------------------------------
	
.LF181
		jsr     copy_check           	; Filename legal?
        jsr     FILEINCAT               ; Filename in cat?
		BCS 	LF17D           

;============================================
; Print FILE? error
;--------------------------------------------

.LF189
		JSR 	INLINE_PRINT 
		EQUS 	"File?"
		BRK

;============================================
; Check if filenaam in cat, carry set is yes
;--------------------------------------------

.FILEINCAT
		JSR 	READ_CAT           		; Check disk busy

        ldy     #$F8                    ; Init nameptr
.LF197
		jsr     incy8                   ; INY 8x, move to next entry
        cpy     WKBASE + $0105          ; FilePointer
        BCS 	LF1BF           
   
        lda     WKBASE + $000F,Y        ; Qualifier from catalog
        and     #$7F                    ; Mask out protectionbit
        cmp     USEQUAL                 ; Same qual as we are looking for ?
		BNE 	LF197           		; nope : skip to next file
   
		JSR incy7           			; Advance pointer 
        ldx     #$06                    ; Check filename in cat
.LF1AD
		lda     WKBASE + $0007,Y                 
        cmp     FILENAME,X                   
        BNE 	LF1B9           
		DEY                 
		DEX                 
		BPL 	LF1AD           
		RTS                    
 
.LF1B9
		dey                             ; Next file in cat
        dex                             
		BPL 	LF1B9            
		BMI 	LF197           

.LF1BF
		clc                             ; Filename not found
        rts 

;
; Remove catalog entry at y, shuffle all following entries back to  overwrite it
; exits with y pointing to free entry
;
		
.rem_cat_entry
		lda     WKBASE + $000F,Y        ; Check file protection
        BMI 	LF1DF                   ; Protected  
.LF1C6
		lda     WKBASE + $0010,Y        ; Remove filename from cat
        sta     WKBASE + $0008,Y                 
        lda     WKBASE + $0110,Y        ; Remove filedata
        sta     WKBASE + $0108,Y                 
        iny                             
        cpy     WKBASE + $0105          ; last file?
		BCC 	LF1C6           
		
        tya                             ; Decrement FilePointer
        sbc     #$08                    
        sta     WKBASE + $0105   
.LF1DE                
		RTS                    
 
;============================================
; Print PROT error
;--------------------------------------------

.LF1DF
		JSR 	INLINE_PRINT           
		EQUS 	"Prot"
		BRK

; *INFO <file> - display file information
; =======================================
.infocom
		JSR 	LF17E					; Check filename in cat 
		JSR 	print_info				; Print filenaam, start,link,lengte,sector
		JMP 	set_qual_to_use         ; Set qual back if changed by USE
 
.print_info_ifmon
		lda     MONFLAG                 ; Check MON flag
		BNE 	LF1DE   				; NOMON, exit without displaying info
.print_info
		lda     WKBASE + $000F,Y        ; Load Qual
        and     #$7F                    ; Mask protectionbit
        jsr     OSWRCH                  ; Print Qual
        jsr     PRSPACE                 ; Print space
		
        ldx     WKBASE + $000F,Y        ; Load protectionbit
		BPL 	LF206                   ; Not protected, print ' '  
		LDA 	#'#'                    ; Protected, print #
.LF206
		JSR 	OSWRCH          		
		ldx     #$07                    ; Print filenaam 
.LF20B
		lda     WKBASE + $0008,Y                 
        jsr     OSWRCH               
        iny                             
        dex                     
		BNE 	LF20B            
.LF215
		jsr     PRSPACE                 ; Print space

		lda     WKBASE + $0102,Y        ; Print hex adres
        jsr     PRINT_HEXA                   
        lda     WKBASE + $0101,Y                 
        jsr     PRINT_HEXA                   
        iny                             
        inx                             
        iny                             
        cpx     #$02                    ; Repeat 3 times
        BCC 	LF215          

        jsr     PRSPACE                 ; Print space
        jsr     PRSPACE                 ; Print space

        lda     WKBASE + $0103,Y        ; Check file>64Kb
        jsr     diva16                  ; Divide by 16
        jsr     PRINT_HEXA_LOWN         ; Print hex filelen
        lda     WKBASE + $0102,Y                 
        jsr     PRINT_HEXA                   
        lda     WKBASE + $0101,Y                 
        jsr     PRINT_HEXA                   

        jsr     PRSPACE              	; Print space

        lda     WKBASE + $0103,Y        ; Print hex sector
        jsr     PRINT_HEXA_LOWN                   
        lda     WKBASE + $0104,Y                 
        jsr     PRINT_HEXA                   

        jsr     OSCRLF                 

; Read catalogue from current drive
; =================================
.READ_CAT
		JMP 	load_cat

; *DIR <num> - select drive and load catalogue
; ============================================
.dircom
		JSR 	drivecom           		; Do *DRIVE <num>
		JMP 	READ_CAT           		; Load catalogue

; *CAT (<drive>) - display disk catalogue
; =======================================
.catcom 
		jsr     dircom                  ; DIR
.catcom2		
		ldx     #$00                    
        stx     &B6  		
.LF273
		lda     WKBASE + $0000,X        ; Print title
        cpx     #$08 
		BCC 	LF27D             

        lda     WKBASE + $00F8,X                 
.LF27D
		jsr     OSWRCH               	; Print character
        inx                             
        cpx     #$0D                    ; End of title?
        BNE 	LF273           

		JSR 	INLINE_PRINT           	; Print " DRIVE"
		EQUS 	" drive "
		
        lda     DRIVENO              	; Print drivenr
		JSR 	PRINT_HEXA_LOWN         ; Print drive no 
		
        jsr     INLINE_PRINT            ; Print " QUAL "
		EQUS 	" qual "

        lda     USEQUAL                 ; Print qual
        jsr     OSWRCH               
		
		JSR 	INLINE_PRINT           	; print "opt"
		EQUS 	" opt "

		LDA 	WKBASE + &0106          ; Get options
		JSR 	diva16           		; Shift into low nibble
		JSR 	PRINT_HEXA_LOWN         ; And print them

.LF2B3
		ldy     #$00                    
		JSR 	LF2CE					; Check last filename
		BCC 	LF304             

.LF2BA
		jsr     decy8                   ; DEY 8x
        lda     WKBASE + $0008,Y        ; Mask protectionbit
        and     #$7F                    
        sta     WKBASE + $0008,Y                 
        tya                             
        BNE 	LF2BA           
 		JMP 	OSCRLF

.LF2CB
		jsr     incy8                   ; INY 8x
.LF2CE
		cpy     WKBASE + $0105          ; Check last filename?
        BCS 	LF2D8         
		lda     WKBASE + $0008,Y        ; Check protectionbit
        BMI 	LF2CB           

.LF2D8

; WAIT_NOT_BUSY is defined here so that external programs such as compact that are linked
; into this rom have a place to JSR to, in the 1770 implementation, in the 8271 this 
; actually does a disk wait, for the 1770 it's just an RTS
.WAIT_NOT_BUSY
		RTS                    
 
.LF2D9
		ldy     CATEOLFLAG              ; Check EOL flag
		BEQ 	LF2E2           		; no EOL yet, skip  

        jsr     OSCRLF                  ; print EOL
		ldy     #$FF                    ; RE-init EOL flag  
.LF2E2
		iny                             ; 
        sty     CATEOLFLAG              ; save EOL flag       
        jsr     PRSPACE6                ; Print 6 spaces
.LF2E8
		lda     #'#'                    ; Assume file locked 
        ldy     CATINDEX                ; Get index
        ldx     WKBASE + $000F,Y        ; Check if file locked
        BMI 	LF2F3           		; Locked
		lda     #' '                    ; Show file not locked
.LF2F3
		jsr     OSWRCH                  ; Print locked character

        ldx     #$00                    ; Reset filenamepointer
.LF2F8
		lda     CATFILENAME,X           ; Get character filename
        jsr     OSWRCH                  ; Print character
        inx                             
        cpx     #$07                    ; Check end of filename
		BNE 	LF2F8					; Nope: look for next char
		BEQ 	LF2B3                   ; Last character print 

;--------------------------------------------
; Sort the directory alphabetically.
;--------------------------------------------

.LF304
		sty     CATINDEX                ; Save catalog index
		LDX 	#&00            ; F306 A2 00     ".  
.LF308
		lda     WKBASE + $0008,Y        ; get first character of pointed to filename
        and     #$7F                    ; make sure it's 7 bit ascii
        sta     CATFILENAME,X           ; copy to saved name
        iny                             ; increment pointers
        inx                             ; 
        cpx     #$08                    ; all characters done ?
		BNE 	LF308                   ; no : continue copying
		
.LF315
		JSR LF2CE           ; F315 20 CE F2   Nr 
		BCS LF337           ; F318 B0 1D     0.  
		LDX #&06            ; F31A A2 06     ".  
		SEC                 ; F31C 38        8   

.LF31D
		LDA WKBASE + &000E,Y         ; F31D B9 0E 20  9.  
		SBC &AE,X           ; F320 F5 AE     u.  
		DEY                 ; F322 88        .   
		DEX                 ; F323 CA        J   
		BPL LF31D           ; F324 10 F7     .w  
		JSR incy7           ; F326 20 36 F1   6q 

		LDA WKBASE + &000F,Y         ; F329 B9 0F 20  9.  
		AND #&7F            ; F32C 29 7F     ).  
		SBC &B5             ; F32E E5 B5     e5  
		BCC LF304           ; F330 90 D2     .R  
		JSR incy8           ; F332 20 35 F1   5q 
		BCS LF315           ; F335 B0 DE     0^  

.LF337
		LDY &B7             ; F337 A4 B7     $7  
		LDA WKBASE + &0008,Y         ; F339 B9 08 20  9.  
		ORA #&80            ; F33C 09 80     ..  
		STA WKBASE + &0008,Y         ; F33E 99 08 20  ..  
		LDA &B5             ; F341 A5 B5     %5  
		CMP &B6             ; F343 C5 B6     E6  
		BEQ LF2D9           ; F345 F0 92     p.  

		STA &B6             ; F347 85 B6     .6  
		JSR OSCRLF

		LDA &B5             ; F34C A5 B5     %5  
		JSR OSWRCH          ; F34E 20 F4 FF   t. 

		LDA #&3A            ; F351 A9 3A     ):  
		JSR OSWRCH          ; F353 20 F4 FF   t. 

		LDY #&04            ; F356 A0 04      .  
		JSR PRSPACEY           ; F358 20 28 F1   (q 

		STY &B8             ; F35B 84 B8     .8  
		BEQ LF2E8           ; F35D F0 89     p.  

.LF35F
		LDA WKBASE + &010E,Y         ; F35F B9 0E 21  9.! 
		JSR diva16           ; F362 20 30 F1   0q 

		STA &E5             ; F365 85 E5     .e  
		CLC                 ; F367 18        .   
		LDA #&FF            ; F368 A9 FF     ).  
		ADC WKBASE + &010C,Y         ; F36A 79 0C 21  y.! 
		LDA WKBASE + &010F,Y         ; F36D B9 0F 21  9.! 
		ADC WKBASE + &010D,Y         ; F370 79 0D 21  y.! 
		STA &E6             ; F373 85 E6     .f  
		LDA WKBASE + &010E,Y         ; F375 B9 0E 21  9.! 
		AND #&0F            ; F378 29 0F     ).  
		ADC &E5             ; F37A 65 E5     ee  
		STA &E5             ; F37C 85 E5     .e  
.LF37E
		SEC                 ; F37E 38        8   
		LDA WKBASE + &0107,Y         ; F37F B9 07 21  9.! 
		SBC &E6             ; F382 E5 E6     ef  
		PHA                 ; F384 48        H   
		LDA WKBASE + &0106,Y         ; F385 B9 06 21  9.! 
		AND #&0F            ; F388 29 0F     ).  
		SBC &E5             ; F38A E5 E5     ee  
		TAX                 ; F38C AA        *   
		LDA #&00            ; F38D A9 00     ).  
		CMP &E3             ; F38F C5 E3     Ec  
		PLA                 ; F391 68        h   
		SBC &E4             ; F392 E5 E4     ed  
		TXA                 ; F394 8A        .   
		SBC #&00            ; F395 E9 00     i.  
		RTS                 ; F397 60        `   
 
.LF398
		LDX #&02            ; F398 A2 02     ".  
		BNE LF39E           ; F39A D0 02     P.  

.LF39C
		LDX #&00            ; F39C A2 00     ".  
.LF39E
		STY &BF             ; F39E 84 BF     .?  
.LF3A0
		LDY &0208,X         ; F3A0 BC 08 02  <.. 
		LDA &BB,X           ; F3A3 B5 BB     5;  
		STY &BB,X           ; F3A5 94 BB     .;  
		STA &0208,X         ; F3A7 9D 08 02  ... 
		INX                 ; F3AA E8        h   
		TXA                 ; F3AB 8A        .   
		LSR A               ; F3AC 4A        J   
		BCS LF3A0           ; F3AD B0 F1     0q  
		LDY &BF             ; F3AF A4 BF     $?  
		RTS                 ; F3B1 60        `   

; Command table
; ============= 
.COMMAND_TABLE
		EQUB "CAT",		>catcom, 	<catcom
		EQUB "DIR",		>dircom, 	<dircom
		EQUB "INFO",	>infocom, 	<infocom
		EQUB "LOAD",	>loadcom, 	<loadcom
		EQUB "SAVE",	>savecom, 	<savecom
		EQUB "DELETE",	>deletecom, <deletecom		
		EQUB "GO",		>gocom, 	<gocom
		EQUB "RUN",		>runcom, 	<runcom
		EQUB "LOCK",	>lockcom, 	<lockcom
		EQUB "UNLOCK",	>unlockcom, <unlockcom
		EQUB "MON",		>moncom, 	<moncom
		EQUB "NOMON",	>nomoncom, 	<nomoncom
		EQUB "SET",		>setcom, 	<setcom
		EQUB "DRIVE",	>drivecom, 	<drivecom
		EQUB "TITLE",	>titlecom, 	<titlecom
		EQUB "USE",		>usecom, 	<usecom
		EQUB "OPTION",	>optioncom, <optioncom
		EQUB "EXEC",  	>execcom, 	<execcom
		EQUB "SHUT", 	>shutcom, 	<shutcom
		EQUB "SPOOL", 	>spoolcom, 	<spoolcom
		EQUB "",    	>exec, 		<exec

; =============
; OSCLI handler
; =============
; On entry, &100=command string
;
.LF42E
		ldx     #$FF                    ; Point to before start of command table
        cld                             ; Binary mode
.LF431
		ldy     #$00                    ; Init pointer to commandline
        jsr     SKIPSPACE               ; Skip spaces
        dey                             ; as beggining of loop does iny.....
.LF437
		iny                             ; Move to next char
        inx                             ; Move to next keyword char
.LF439
		lda     COMMAND_TABLE,X         ; get next command word character
		BMI 	LF456					; end of command, yep exec it.

        cmp     STACKPAGE,Y             ; Character the same as in buffer
		BEQ 	LF437					; yep : keep scanning
        dex                             ; nope
.LF444
		inx                             ; get next character from command
        lda     COMMAND_TABLE,X         ; is it <$80, so ascii?
		BPL 	LF444                   ; yes, not end of command string
        inx                             ; move to msb od exec
		
        lda     STACKPAGE,Y             ; Get next character from input buffer
        cmp     #'.'                    ; Is it a period?
		BNE 	LF431                   ; Nope, start again with next keyword
		
        iny                             ; move past found word in buffer
        dex                             ; point to exec address
		BCS 	LF439

.LF456
		sta     FILENAMEPTR+1           ; Save exec address in indirect
        lda     COMMAND_TABLE+1,X               
        sta     FILENAMEPTR             
        clc                             ; enter routine with x=0, carry clear
        ldx     #$00                    
        jmp     (FILENAMEPTR)           ; jump to exec routine

; *DELETE <file>
; ==============
.deletecom
		JSR 	LF17E                   ; Filename in cat
		sty     FILENAMEPTR             ; Save pointer to name
.LF468
		jsr     print_info_ifmon        ; Print info
        ldy     FILENAMEPTR             ; Restore name ptr
        jsr     rem_cat_entry           ; Delete name in cat
        jmp     write_cat_resqual       ; Save cat & restore qualifier

; *DRIVE <num>
; ============
.drivecom
		jsr     SKIPSPACE               ; Skip spaces
        cmp     #CR                   	; EOL?
        BEQ 	LF497                   ; Yep : No drivenr, do default drive
        iny                             ; Move to next char
        pha                             ; Save Drivenr
        jsr     CHECK_PARAM             ; Check end of command
        pla                             ; Restore drivenr
        cmp     #'0'                    ; Drivenr >=1?
		BCC 	LF498  					; <'0' - Bad drive
		
        cmp     #'4'                    ; Drivenr <=4?
		BCS 	LF498  					; >'3' - Bad drive

.readcat
		and     #$03                    ; Convert ascii no to binary
        eor     DRIVENO                 ; Mask against current drive, will be $80 or $00 if so
        cmp     #$80                    ; Catalog read ?
        BEQ 	LF497  					; If current drive b7=1, 

        eor     DRIVENO                 ; Store new drivenr (will undo above eor!)
        sta     DRIVENO                 
.LF497
		RTS
 
.LF498
		JSR 	INLINE_PRINT           ; Bad drive
		EQUS 	"Drive?"
		BRK

; *NOMON
; ======
.nomoncom
		LDX 	#&FF            		; Prepare to do NOMON

; *MON
; ====
.moncom
		jsr     CHECK_PARAM             ; Check end of command
        stx     MONFLAG                 ; Set mon flag

.set_qual_to_use
		lda     SETQUAL                 ; Get SET qualifier
        sta     USEQUAL                 ; Set USE qualifier
        rts                             

; *LOAD <file> (<addr>)
; =====================
.loadcom
		jsr     READFNAME               ; Read filename
        jsr     get_loadadr             ; Read adres into LOADADDR
		BEQ 	LF4BA					; none given skip

		lda     #$FF                    ; flag LOADADDR valid
.LF4B8
		sta     EXECADDR                ; 
.LF4BA
		LDX 	#FILENAMEPTR            		
        clc                             
        jmp     (loadv)                ; LODVEC

;============================================
;LODVEC SUB
;--------------------------------------------

.NEW_LODVEC 
		php                             ; Save flags.   
		JSR 	LF4CD           		; F4C1 20 CD F4   Mt 
.LF4C4
		plp                             ; Restore flags   
		BCC 	LF4CA           		; F4C5 90 03     ..  
.LF4CA
		jmp     set_qual_to_use         ; Reset qual
 
.LF4CD
		JSR 	LF181                   ; Check if filename legal and in cat
.LF4D0
		sty     FILENAMEPTR             ; Save filename ptr
        ldx     #$00                    ; 
        lda     EXECADDR                ; Get LOADADDR vald flag
		BPL 	LF4DC                   ; Skip no LOADADDR given

        ldx     #$02                    ; Skip past load address in catalog
        iny                             
        iny                             

.LF4DC
		lda     WKBASE + $0108,Y        ; Move catalog file data into buffer
        sta     LOADADDR,X              ; at LOADADDR
        iny                             
        inx                             
        cpx     #$08                    ; Done all ?
		BNE 	LF4DC                   ; nope : loop again
		
        ldy     FILENAMEPTR             ; recover filename ptr
        jsr     print_info_ifmon        ; Print file info (if MON).
.LF4EC
		lda		#WCMD_READ_SEC			; Flag reading

; Code between LF4F1 - F50D shared by LODVEC and SAVVEC	
.common_rw
		sta     FDCSAVCMD               ; Save command executing

        jsr     calc_start_addr         ; Calculate start address, number of sectors etc
.LF4F6
		JSR 	RECALC_TRACK_SEC        ; Recalculate track and sector
		BEQ 	LF50D 		            ; All done : exit
.LF4FB
		jsr     set_memptr              ; Set MEMPTR from STARTADDR, place to load at
		jsr		SEEK					; Seek to track

        lda     FDCSAVCMD               ; Get FDC command to execute
        cmp		#WCMD_WRITE_SEC			; Write sectors ?
		
		bne		do_read					; nope : do read
		
		jsr		WRITESEC				; Write sectors
		jmp		do_done
		
.do_read
		jsr		READSEC					; Read sectors
.do_done		
		bne     LF4FB                   ; no error : do next block
		
        jsr     set_startaddr           ; Update STARTADDR from current MEMPTR, inc TRACKNO
        BNE 	LF4F6  					; More to do loop again
.LF50D
		RTS                    

; Unmatched *command
; ==================
; Look for *command on drive 0, prefix <SPC>
; Effectively, :0.<SPC> is the library directory
;
.exec
		JSR 	LF05F                   ; Read filename
		lda     DRIVENO                 ; Save current drive
        sta     SAVDRIVENO              
        lda     USEQUAL                 ; Save current qualifier
        sta     SAVQUAL                  
        lda     #SPACE                  ; Set current qualifier to space
        sta     USEQUAL                   
        lda     #$00                    ; Drive = 0
        sta     EXECADDR                 
        JSR 	readcat           		; Select drive 0

		ldx     #FILENAMEPTR                    
        jsr     copy_check              ; Filename legal?
        jsr     FILEINCAT               ; Filename in cat?
        BCS 	LF53D 					; Look for file, if found then execute it

		JSR 	LF549           		; Restore drive & qualifier
.LF531
		JSR 	INLINE_PRINT            ; Bad command
		EQUS 	"Command?"
		BRK

; Load and execute file
; ---------------------
.LF53D
		JSR 	LF4D0           		; F53D 20 D0 F4   Pt 
		JSR 	LF549           		; Restore drive & qualifier
        jmp     (EXECADDR)              ; Link file

; Restore drive and SET/USE prefix
; -------------------------------- 
.LF549
		lda     SAVDRIVENO              ; Get saved drive no
        jsr     readcat                 ; Re-read it's catalog
        lda     SAVQUAL                 ; Restore saved qualifier
        sta     USEQUAL                   
        rts                             

 
; *RUN <file> (<params>)
; ======================
; Look for file on current drive, current prefix
;
.runcom
		JSR 	LF05F                   ; Check filename + string
		JSR 	LF4B8                   ; Read addresses
.LF559
		jmp     (EXECADDR)              ; Execute code
 
.LF55F
		JMP 	LF189                   ; Print "FILE?" error

; *EXEC <file>
; ============
.execcom
		jsr     CHECK_FNPARAM           ; Check end of command
        jsr     OSFIND                  ; Find / open the file for input
        tay                             ; 
        BEQ 	LF55F       			; No file, jump to File? error
		
        sta     EXECHANDLE              ; Save filehandle
		JSR 	LF398           		; Save current RDCHV
		LDA 	#<LF57C  				; Redirect rdchv to fetch from Exec channel
		LDY 	#>LF57C 
.LF574
		sta     cliv,X                	; Change command vector
        tya                             
        sta     cliv+1,X              
        rts  

; Fetch byte from EXEC channel
; ----------------------------
.LF57C 
		sty     INBUFIDX                ; Save input buffer pointer
        ldy     EXECHANDLE              ; Get filehandle
        jsr     OSBGET                  ; Get a byte from exec channel
		BCC 	LF58D           		; Not EOF, return

		jsr     OSSHUT                  ; Shut the file
        ldy     INBUFIDX                ; Restore input buffer pointer
        jmp     (rdchv)                 ; Exit to readvector

.LF58D
		ldy     INBUFIDX                ; Restore input buffer pointer
        rts              				; Restore Y and return

; *SPOOL <file>
; =============
.spoolcom

        jsr     CHECK_FNPARAM           ; Check end of command
        clc                             
        jsr     OSFIND                  ; Find / open the file for output
		sta     SPOOLHANDLE             ; Save filehandle
        JSR 	LF39C           		; Save current WRCHV
		LDA 	#<LF5A2  				; Redirect WRCH to send to SPOOL channel
		LDY 	#>LF5A2
		BNE		LF574

; Send byte to SPOOL channel
; --------------------------
.LF5A2
		sty     INBUFIDX                ; Save input buffer index
        ldy     SPOOLHANDLE             ; Get handle for spool file
        jsr     OSBPUT                  ; Put the byte to the SPOOL channel
		ldy     INBUFIDX                ; Restore buffer index
        jmp     (SAVWRCVEC)             ; Jump to saved WRCH vector

; *GO <addr> - enter machine code
; ===============================
.gocom
		jsr     get_loadadr             ; Read hex adres in #9C/9D
        php                             ; 
        jsr     CHECK_PARAM             ; Check end of command
        plp                             ; 
        BEQ 	LF559                   ; No address given, use last exec address.
        jmp     (LOADADDR)              ; Jump adres
		
; *SET <char>
; ===========
.setcom
		JSR 	LF5FC                   ; Read Qual
        sta     SETQUAL                 ; Store Qual
        rts                             

; *TITLE <title>
; ==============
.titlecom
		jsr     READFNAME               ; Read text
        jsr     dircom                  ; *DIR command
        ldx     #TITLELEN               ; Length of title
        lda     #SPACE                  ; Blank out old title 

.LF5CB
		jsr     put_title_cat           ; Write char in cat
        dex                             ; decrement count
		BPL 	LF5CB                   ; keep looing if not all done

.LF5D1
		inx                             ; Move to next char of specified name
        lda     $0140,X                 ; Get a char from name
        cmp     #CR                     ; EOL?
        beq     write_cat_resqual       ; Yep : write back to disk
		
        jsr     put_title_cat           ; Put next char in cat
        cpx     #TITLELEN               ; Done all?
		BCC 	LF5D1                   ; Nope keep going
        bcs     write_cat_resqual       ; Yep : write back to disk

; *LOCK <file>
; ============
.lockcom
		sec                             ; Flag lock

; *UNLOCK <file>
; ==============
.unlockcom
		php                             ; Save flags
		JSR 	LF17E					; Check filename in cat 
        lda     USEQUAL                 ; Get qualifier 
        rol     A                       ; Shift top bit to carry
        plp                             ; restore flags (lock falg in carry)
        ror     A                       ; Shift carry back to qual byte
        sta     WKBASE + $000F,Y        ; put back in catalog
        jsr     print_info_ifmon        ; print file info

; Write catalog and restore qualifier.
.write_cat_resqual
		jsr     write_cat               ; Write catalog back to disk
        jmp     set_qual_to_use         ; use qual=set qual

; *USE <char>
; ===========
.usecom
		lda     USEQUAL                 ; Get current use qualifier
        sta     SETQUAL                 ; Set SET qualifer to it
.LF5FC
		INY
		JSR CHECK_PARAM       			; Check only one character

        lda     $00FF,Y                 ; Get qualifier from command line
        sta     USEQUAL                 ; Set USE qualifier to it.
        rts                             

; *SAVE <file> <start> <end> (<reload>)
; =====================================
.savecom
		jsr 	READFNAME               ; Read filename
        jsr     get_loadadr             ; Read startadres
        BEQ 	LF636                   ; No startadres, error
		
		ldx     #STARTSEC               ; Get Endaddress
        jsr     addr_to_x               
        BEQ 	LF636           		; No endadrres, error  
		
        ldx     #EXECADDR               ; Get EXEC address
        jsr     addr_to_x               
        php                             ; Save flags
        lda     LOADADDR                ; Get LOADADDR to x:a
        ldx     LOADADDR+1              ; 
        plp                             ; Restore flags
		BNE 	LF669					; No exec address given

        sta     EXECADDR                ; So set EXEC address to load address.
        stx     EXECADDR+1              
.LF669
		sta     FILESIZE                ; Loadaddress here
        stx     FILESIZE+1              
        jsr     CHECK_PARAM             ; Check end of command
		
		ldx     #FILENAMEPTR            ; Flag Disk.
        clc                             
        jmp     OSSAVE                  ; OSSAVE
;============================================
;Part of TITLE COMMAND
;--------------------------------------------
 
.put_title_cat
		cpx     #$08                    ; First 8 chars of title at beginning of sector
        BCC 	LF628            		; Yep chars 0..7 write at beginning 
        sta     WKBASE + $00F8,X        ; Else write at end
        rts                             

.LF628
		sta     WKBASE + $0000,X        ; 
.LF62B
		rts

skipto $f62c
; Check for <filename> and no more parameters
; -------------------------------------------
.CHECK_FNPARAM
		JSR 	READFNAME           	; Parse filename

; Check for no more parameters
; ----------------------------
.CHECK_PARAM
		jsr     SKIPSPACE               ; Skip spaces
        cmp     #$0D                    ; End of command?
		BEQ 	LF62B  					; End of line - exit
.LF636
		JSR 	INLINE_PRINT           	; Syntax error
		EQUS 	"Syntax?"
		BRK
		
.LF641
		JSR 	INLINE_PRINT           	; Disk full
		EQUS 	"Full"
		BRK
		
; *OPTION <num>
; =============
.optioncom
		JSR 	get_loadadr           	; Get option into LOADADDR
		JSR 	dircom           		; Check and load catalog if needed
		LDA 	LOADADDR                ; Get option value
		ASLA               				; Shift it to upper nibble
		ASLA                  
		ASLA               
		ASLA               
		EOR 	WKBASE + &0106          ; Combine it with catalog
		AND 	#&30            		   
		EOR 	WKBASE + &0106           	
		STA 	WKBASE + &0106          ; Save back in catalog
		JMP 	write_cat           	; write catalog back to disk

;============================================
;SAVVEC SUB
;--------------------------------------------

.NEW_SAVVEC 
		php                             ; Save flags
		JSR 	LF710           		; Create catalog entry, write file.
		JMP 	LF4C4           		; Wait for disk, restore Qual etc.
 
.LF67D
		jsr     copy_check              ; Copy filename and check legal
        jsr     FILEINCAT               ; Filename in cat?
        BCC 	LF688					; 
        jsr     rem_cat_entry           ; Yes, remove file from cat

.LF688
		lda     FILESIZE              	; Save startadres
        pha                             
        lda     FILESIZE+1              
        pha                             
		
        sec                             ; Calculate length in FILESIZE
        lda     STARTSEC                
        sbc     FILESIZE                
        sta     FILESIZE                
        lda     STARTSEC+1              
        sbc     FILESIZE+1              
        sta     FILESIZE+1              

        lda     #$00                    ; Set STARTSEC = $002
        sta     STARTSEC                   
        lda     #$02                    
        sta     STARTSEC+1              

        ldy     WKBASE + $0105          ; 
        BEQ 	LF6E2           		; Disk empty?
		CPY 	#&F8            		; Disk full?  
		BCS 	LF641         
		
		JSR 	LF37E           		; Calculate ??
		JMP 	LF6B8           		
 
.LF6B2
		JSR 	decy8           
		JSR 	LF35F           
.LF6B8
		TYA                 
		BEQ 	LF6BD             
		BCC 	LF6B2             

.LF6BD
		BCS 	LF6CA           
.LF6BF		
		JSR 	INLINE_PRINT          
		EQUS 	"No room"
		BRK

.LF6CA
		STY 	TEXTPTR					; Move filedata 8 bytes up
		LDY 	WKBASE + &0105          ; F6CC AC 05 21  ,.! 
.LF6CF
		cpy     TEXTPTR                 ; Done all ?
		BEQ 	LF6E2                   ; Yes : exit

        lda     WKBASE + $0007,Y        ; Move bytes
        sta     WKBASE + $000F,Y        
        lda     WKBASE + $0107,Y        
        sta     WKBASE + $010F,Y        
        dey                             ; decrement pointer
  		BCS 	LF6CF           		; loop for next
.LF6E2
		ldx     #$00                    ; Copy new filename in cat
.LF6E4
		lda     FILENAME,X              ; Get a byte from name
        sta     WKBASE + $0008,Y        ; put in cat
        iny                             ; increment pointers
        inx                             
        cpx     #$08                    ; Done all ?
		BNE 	LF6E4                   ; nope : loop again

.LF6EF
		LDA 	FILENAMEPTR+1,X         
		DEY                 
		STA 	WKBASE + &0108,Y         
		DEX                 
		BNE 	LF6EF       
		
        jsr     print_info_ifmon         ; Print fileinfo

        pla                             
        sta     LOADADDR+1              
        pla                            
        sta     LOADADDR                  

        ldy     WKBASE + $0105          ; Update filecounter (+8)
        jsr     incy8                   
        sty     WKBASE + $0105          

        jsr     write_cat               ; Write catalog
        
.LF710
		JSR 	LF67D           
.LF713
        LDA		#WCMD_WRITE_SEC			; Flag we are writing
		jmp     common_rw               ; Go do it

;--------------------------------------------
; Initialise drive for loading catalog
;--------------------------------------------
.init_fdc_lcat
		lda     #$00                    ; Zero track and sector numbers
        sta     TRACKNO                  
        sta     SECTORNO                 
        lda     #$02                    ; Catalog is 2 sectors
        sta     SECCOUNT                

		jsr		RESTORE0 				; Seek to track 0
		
;		jmp		MEMPTR_WKBASE			; Point at workspace
.MEMPTR_WKBASE
        LDY     #WKLOW
        STY     MEMPTR                     
        LDA     #WKHIGH
        STA     MEMPTR+1                   
.LF765
		rts

;--------------------------------------------
; Load catalog
;--------------------------------------------
.load_cat
		jsr     TESTREADY               ; Check drive ready
		BNE 	LF765                   ; ready, return, don't need to re-read.

.load_cat_always		
        jsr     START_MOTOR_SELECT      ; Start drive motor and select drive
        jsr     init_fdc_lcat           ; Init FDC for cat load
		
		jmp		READSEC					; Read the sectors
 
; Write catalog
; Assumes drive is started and selected.
.write_cat
		jsr     START_MOTOR_SELECT      ; Start drive motor and select drive
        jsr     init_fdc_lcat           ; Init FDC for cat write

		jmp		WRITESEC				; Write the sectors

;--------------------------------------------
; Select drive / side                HARDWARE
;--------------------------------------------
.START_MOTOR_SELECT: 
		JSR		GET_DRV_Y				; Get driveno in Y
        ora     #MOTOR_ON               ; Flag drive running
        sta     DRIVENO                 ; update driveno

		lda		PCTRL					; Get the control register
		and 	#DRVSID_MASK			; MAsk out drive / side bytes
		ora		drvtab,y				; Mask in drive select / side select
		sta		PCTRL
        
		lda		SAVTRK0,y				; get current track for this drive
		sta		WTRACK					
		
        rts

.GET_DRV_Y
		lda     DRIVENO                 ; Get drive no
        and     #$03                    ; Mask it
        tay                             ; Get driveno into pointer
		rts
                             
.drvtab	EQUB	$02						; DS1=0,DS1=1,SS=0
		EQUB	$04						; DS1=1,DS1=0,SS=0
		EQUB	$03						; DS1=0,DS1=1,SS=1
		EQUB	$05						; DS1=1,DS0=1,SS=1
	
;--------------------------------------------
; Controler drive status
;--------------------------------------------
	
.TESTREADY
		lda     DRIVENO                 ; Check if catalog must be read
		BPL 	LF7A7                   ; If ms bit set, then drive cat in memory

		lda		WSTATUS					; Read status reg

.LF7A7	and 	#WST_MOTOR_ON			; motor on ?
		rts   
 
;--------------------------------------------
; Recalculate track and interrupt routine
; Recalculate track and sector interrupt routine
;--------------------------------------------

.RECALC_TRACK_SEC
		lda     #RETRIES                ; Reset retry count
        sta     RETRYCOUNT              
        sec                             
        lda     #SECTRACK               ; Sectors per track
        sbc     SECTORNO                
		
        ldy     SECC256FLAG             ; Get sector > 256 flag
        BNE 	LF845                   ; sector is > 256   

		cmp     SSECCOUNT               ; Saved Sector count 0 ?
        BCC 	LF845

		lda     SSECCOUNT               ; Get saved sector count
.LF845
		sta     SECCOUNT                ; Restore it
        sec                             ; set carry
        lda     SSECCOUNT               ; Get Saved sector count
        sbc     SECCOUNT                ; Decrement by sector count sectors
        sta     SSECCOUNT               ; Re-save it
		BCS 	LF852  

		dec     SECC256FLAG             ; Decrement > 256
.LF852
		lda     SECCOUNT                ; Get sector count
        rts                             

;--------------------------------------------
; Restore address interrupt routine
;--------------------------------------------

.set_startaddr
		lda		#0						; Zero sector no for next track
		sta     SECTORNO                
        lda     MEMPTR                  ; Get pointer to next mem location to be loaded / saved
        sta     STARTADDR               ; Update start adddress 
        lda     MEMPTR+1                
        sta     STARTADDR+1             
        inc     TRACKNO                 ; Increment track number
        rts                             
 
;--------------------------------------------
; Set address interrrupt routine
;--------------------------------------------

.set_memptr: 
		lda     STARTADDR              	; Copy MEMPTR from startaddr
        sta     MEMPTR                  
        lda     STARTADDR+1             
        sta     MEMPTR+1                
        rts                             

;
; Main NMI handler.
;

.NMIROUT
        JMP     (nmiv)

;*****************************************************
; The following code was borrowed / adapted from GDOS
; 2015-07-03 PHS.
;*****************************************************
;*********************
; 1770 code begin 
;*********************

;
; Step in a track.
;

.STEPIN                                 
        LDA     #WSTPIC  			; step in command            
		INC     TRACKNO             ; update current track
		JMP		STEP_RESTORE		; go do it
;
; Restore to track 0
;
.RESTORE0
		LDA		#0					; Zero current track no
		STA		TRACKNO
		
        LDA     #WRESTC          	; restore command
        AND     #WCF_RATE30         ; spin up and don't verify, leave rate bits    

; WD command contained in A
.STEP_RESTORE                                 
        STA		WDCOMM				; get WD command (step in or restore)
        STA     WCMD               	; send it 
        JMP     WAIT_INTRQ          ; Wait for terminating INTRQ
		
;.NULNMI                                   
;        RTS                             

;
; Read SCTLEFT sectors into memory at LDADDR, starting at sector SECTORNO. 
;

.READSEC  
		LDA     #WRSCTC             ; Read sector command    
		jmp		READ_WRITE_SEC		; go read / write sectors

;
; write SCTLEFT sectors from memory at LDADDR, starting at sector SECTORNO. 
;
.WRITESEC
        JSR     WRPROT      		; check for write protect            
.WRSECT                                 
        LDA     #WWSCTC             ; Write sector command

.READ_WRITE_SEC                                    
        sta		WDCOMM				; save command
                    
.READ_WRITE_NEXT					
        LDA     SECTORNO           	; get start sector     
        STA     WSECTOR             ; save in WD sector register 
        LDY     #0                  ; offset 0
        LDA		WDCOMM				; get WD command (read or write sec)
		tax
		STA     WCMD                ; send to WD

.RW_SEC_LOOP                                    
        LDA     PCTRL               ; read drive control register
        BPL     RW_SEC_LOOP       	; DRQ low : yes keep waiting 

		cpx		#WWSCTC
		beq		W_DATA

        LDA     WDATA               ; read data from disk
        STA     (MEMPTR),Y          ; save in memory	
		jmp		RW_NEXT
.W_DATA
        LDA     (MEMPTR),Y          ; get a byte from meory
        STA     WDATA               ; send it to the WD		
.RW_NEXT        
		INY                         ; increment pointer 
        BNE     RW_SEC_LOOP         ; continue waiting if y <> 0
	

.WAIT_INTRQ 
        BIT     PCTRL               ; N=b7, V=b6 of PCTRL
        BVC     WAIT_INTRQ          ; Loop until INTRQ

if(0)                                         
        LDA     PCTRL               ; Get control register
        AND     #PINTRQ             ; INTRQ yet?
        BEQ     WAIT_INTRQ          ; Keep waiting
endif

		LDA     WSTATUS        		; Get status from WD
        STA     DSTATUS           	; save it for error handlers      

		LDA		WDCOMM				; retrieve saved WD command.
		CMP		#WSEEKC				; Seek command ?
		BNE		CHECK_RW			; No check for sector read/write

		BEQ		HANDLE_SEEK			; Do SEEK actions
        
.CHECK_RW
		CMP		#WRSCTC				; read sec?
		BEQ		HANDLE_RW			; yep handle it

		CMP		#WWSCTC				; write sec?
		BNE		SAVE_CTRACK  		; no : exit (restore)
		
.HANDLE_RW		
        JSR     ERRTYPE2			; check for type II comand error               
        BCS     READ_WRITE_NEXT     ; error : retry
		
        INC     MEMPTR+1 			; move to the next block of memory                
        INC     SECTORNO            ; move to next sector
        DEC     SECCOUNT            ; decrement sector count
        BNE     READ_WRITE_NEXT     ; not zero : read next
.NMIEXIT
        RTS                             

;
; Seek to a track
;
.SEEK                                   	
        LDA     TRACKNO       		; get track to seek to 
        STA     WDATA               ; send to WD    
        LDA     #WSEEKC            	; Seek command      
		JMP		STEP_RESTORE
if(0)
        STA     WDCOMM              ; Save command
        STA     WCMD                ; send to WD     
        JMP     WAIT_INTRQ          ; Wait for terminating INTRQ
endif
		
.HANDLE_SEEK                                 
        JSR     ERRTYPE1          	; Check type 1 error
        BCS     SEEK                ; if error, try again    
.SAVE_CTRACK
		JSR		GET_DRV_Y			; get driveno in y
		LDA		WTRACK				; get track from WD
		STA		SAVTRK0,y			; save it
        RTS            
        
;
; Handle type 1 command errors
; restore / seek / step / step in / step out
;

.ERRTYPE1                              
        LDA     WSTATUS            	; get status register from WD  	   
        AND     #$18                ; mask out bits we are interested in RNF / CRC
   		BEQ     ERR5                ; no error : exit 
		
		PHA							; save status
		LDA     RETRYCOUNT          ; Any retries left ?
        BNE     ERR13               ; yep : flag error and try again
		PLA							; restore status
		
        AND     #$10                ; RNF ?
        BEQ     ERR_CRC             ; yep, do it
                
		JSR		RESTORE_ERR			; Restore to track 0 + error
		EQUB	"SK ERR"

.ERR_TRACKNO
		JSR     INLINE_PRINT        ; Display message
		EQUB	" TR:"      
        
		LDA     TRACKNO             ; get track number
        JSR     PRINT_HEXA          ; display it ?
        BRK                         ; back to command loop?
		                 
.ERR_CRC
        JSR     INLINE_PRINT        ; display message
		EQUB	"CRC"                                 
		BRK                             
		
.ERR13                                  
		PLA							; drop saved status
        DEC     RETRYCOUNT          ; decrement retry count
        SEC                         ; flag error
        RTS                             
		
.ERR5                                   
        LDA     #4                  ; reset retry count
        STA     RETRYCOUNT                 
        CLC							; flag no error                             
        RTS                             

;
; Check for write protected disk
;

.WRPROT                                 
        LDA     WSTATUS             ; get status register from WD
        ASL     A                   ; get wp bit into bit 7
        BPL     WRP1                ; not write protected, exit
		
		JSR		RESTORE0            ; unload head
		JSR		LF000           	; Print 'DISK '
		EQUB	"prot"                 
		BRK
.WRP1                                   
        RTS                             

;
; Handle type 2 command errors
; read sector / write sector
;

.ERRTYPE2                              
        LDA     DSTATUS 			; get status from WD                
        AND     #$3C        		; mask out bits we want            
        BEQ     ERR5                ; no error : exit
		
		PHA							; save status
        LDA     RETRYCOUNT          ; get retry count
        BNE     ERR13               ; still reties left, skip

		PLA							; restore status
        ASL     A 					; get record type flag in b7                      
        ASL     A                       
        BPL     ERR6                ; Data mark, skip
				
        JSR     INLINE_PRINT        ; write message
		EQUB	"WrErr"                    
.ERRC                               ; get track no 
        JSR     INLINE_PRINT        ; write sector message
		EQUB	" SCT:"                           
        LDA     SECTORNO            ; get sector no
        JSR     PRINT_HEXA          ; write it
		JMP		ERR_TRACKNO			; Write trackno and exit
		
.ERR6                                   
        ASL     A                   ; get RNF flag in b7
        BPL     ERR12               ; no error : skip
				
        JSR     INLINE_PRINT        ; print record not found message
		EQUB	"RNF"              
        NOP                           	  
        JSR     RESTORE0            ; unload head
        JMP     ERRC				; Write track and sector numbers                   
		
.ERR12                                  
        ASL     A                   ; get CRC error in b7
        BPL     ERR7                ; no CRC error, skip
				
		JMP		ERR_CRC				; crc error message
		
.ERR7                                   		
		JSR		RESTORE_ERR			; Restore to track 0 + error
		EQUB	"BadDt"                           
		BRK                                            

.RESTORE_ERR
		JSR     RESTORE0            ; unload head
        JMP     INLINE_PRINT        ; bad data message

;*********************
; 1770 code end 
;*********************

;============================================
; START RANDOM ACCESS ROUTINES
;============================================

; *SHUT
; =====
.shutcom
		LDY #&00             ; Do CLOSE#0 - close all channels

; OSSHUT handler
; ==============
.LF8C5
		PHA
		CLD
		TYA
		BNE LF8D9           ; Not CLOSE#0 - close specified channel

.LF8CA
		CLC                 ; F8CA 18        .   
		ADC #&20            ; F8CB 69 20     i   
		BEQ LF8D7           ; F8CD F0 08     p.  

		TAY                 ; F8CF A8        (   
		JSR LF8C5           ; Close this channel
		BNE LF8CA           ; F8D3 D0 F5     Pu  
.LF8D5
		LDX &C6             ; F8D5 A6 C6     &F  
.LF8D7
		PLA                 ; F8D7 68        h   
		RTS                 ; F8D8 60        `   

; Close channel in Y
; ------------------ 
.LF8D9
		JSR LFAA3           ; F8D9 20 A3 FA   #z 
		BCS LF8D5           ; F8DC B0 F7     0w  

		CPY EXECHANDLE             ; F8DE C4 B9     D9  
		BNE LF8E8           ; F8E0 D0 06     P.  
		
		JSR LF398           ; F8E2 20 98 F3   .s 
		LSR A               ; F8E5 4A        J   
		STA EXECHANDLE             ; F8E6 85 B9     .9  
.LF8E8
		CPY SPOOLHANDLE             ; F8E8 C4 BA     D:  
		BNE LF8F1           ; F8EA D0 05     P.  
		
		JSR LF39C           ; F8EC 20 9C F3   .s 
		STA SPOOLHANDLE             ; F8EF 85 BA     .:  
.LF8F1
		LDA WKBASE + &0217,Y         ; F8F1 B9 17 22  9." 
		AND #&60            ; F8F4 29 60     )`  
		BEQ LF92D           ; F8F6 F0 35     p5  

		JSR LF939           ; F8F8 20 39 F9   9y 
		LDA WKBASE + &0217,Y         ; F8FB B9 17 22  9." 
		AND #&20            ; F8FE 29 20     )   
		BEQ LF927           ; F900 F0 25     p%  

		LDX &C4             ; F902 A6 C4     &D  
		LDA WKBASE + &0214,Y         ; F904 B9 14 22  9." 
		STA WKBASE + &010C,X         ; F907 9D 0C 21  ..! 
		LDA WKBASE + &0215,Y         ; F90A B9 15 22  9." 
		STA WKBASE + &010D,X         ; F90D 9D 0D 21  ..! 
		LDA WKBASE + &0216,Y         ; F910 B9 16 22  9." 
		ASL A               ; F913 0A        .   
		ASL A               ; F914 0A        .   
		ASL A               ; F915 0A        .   
		ASL A               ; F916 0A        .   
		EOR WKBASE + &010E,X         ; F917 5D 0E 21  ].! 
		AND #&F0            ; F91A 29 F0     )p  
		EOR WKBASE + &010E,X         ; F91C 5D 0E 21  ].! 
		STA WKBASE + &010E,X         ; F91F 9D 0E 21  ..! 
		JSR write_cat           ; F922 20 66 F7   fw 

		LDY &C2             ; F925 A4 C2     $B  
.LF927
		JSR LFB9D           ; F927 20 9D FB   .{ 
		JSR LF549           ; F92A 20 49 F5   Iu 
.LF92D
		LDA WKBASE + &021B,Y         ; F92D B9 1B 22  9." 
		EOR #&FF            ; F930 49 FF     I.  
		AND &C0             ; F932 25 C0     %@  
		STA &C0             ; F934 85 C0     .@  
		JMP LF8D5           ; F936 4C D5 F8  LUx 
 
.LF939
		JSR LF965           ; F939 20 65 F9   ey 
.LF93C
		LDX #&07            ; F93C A2 07     ".  
.LF93E
		LDA WKBASE + &020C,Y         ; F93E B9 0C 22  9." 
		STA &E7,X           ; F941 95 E7     .g  
		DEY                 ; F943 88        .   
		DEY                 ; F944 88        .   
		DEX                 ; F945 CA        J   
		BNE LF93E           ; F946 D0 F6     Pv  

		JSR FILEINCAT           ; F948 20 92 F1   .q 
		BCC LF97F+2         ; F94B 90 34     .4  

		STY &C4             ; F94D 84 C4     .D  
		LDA WKBASE + &010E,Y         ; F94F B9 0E 21  9.! 
		LDX WKBASE + &010F,Y         ; F952 BE 0F 21  >.! 
		LDY &C2             ; F955 A4 C2     $B  
		EOR WKBASE + &020D,Y         ; F957 59 0D 22  Y." 
		AND #&0F            ; F95A 29 0F     ).  
		BNE LF97F+2         ; F95C D0 23     P#  
		TXA                 ; F95E 8A        .   
		CMP WKBASE + &020F,Y         ; F95F D9 0F 22  Y." 
		BNE LF97F+2         ; F962 D0 1D     P.  

		RTS                 ; F964 60        `   
 
.LF965
		LDA DRIVENO             ; F965 A5 DC     %\  
		STA &C7             ; F967 85 C7     .G  
		LDA &EF             ; F969 A5 EF     %o  
		STA &C8             ; F96B 85 C8     .H  
		LDA WKBASE + &020E,Y         ; F96D B9 0E 22  9." 
		AND #&7F            ; F970 29 7F     ).  
		STA &EF             ; F972 85 EF     .o  
		LDA WKBASE + &0217,Y         ; F974 B9 17 22  9." 
		JMP readcat           ; F977 4C 88 F4  L.t 

.LF97A 
		CLD                 ; F97A D8        X   
		TYA                 ; F97B 98        .   
		PHA                 ; F97C 48        H   
		STX &C6             ; F97D 86 C6     .F  
.LF97F
		PHP                 ; F97F 08        .   
		LDA &00,X           ; F980 B5 00     5.  
		STA FILENAMEPTR             ; F982 85 DD     .]  
		LDA &01,X           ; F984 B5 01     5.  
		STA FILENAMEPTR+1             ; F986 85 DE     .^  
		JSR LF0A1           ; F988 20 A1 F0   !p 
		JSR FILEINCAT           ; F98B 20 92 F1   .q 
		BCS LF9AF           ; F98E B0 1F     0.  

		PLP                 ; F990 28        (   
		BCC LF998           ; F991 90 05     ..  

		LDY #&00            ; F993 A0 00      .  
		JMP LFA78           ; F995 4C 78 FA  Lxz 
 
.LF998
		LDA #&00            ; F998 A9 00     ).  
		LDX #&08            ; F99A A2 08     ".  
.LF99C
		STA FILENAMEPTR+1,X           ; F99C 95 DE     .^  
		DEX                 ; F99E CA        J   
		BNE LF99C           ; F99F D0 FB     P{  
		LDA #&40            ; F9A1 A9 40     )@  
		STA &E6             ; F9A3 85 E6     .f  
		LDX #&DD            ; F9A5 A2 DD     "]  
		JSR LF67D           ; F9A7 20 7D F6   }v 

		LDX &C6             ; F9AA A6 C6     &F  
		CLC                 ; F9AC 18        .   
		BCC LF97F           ; F9AD 90 D0     .P  
.LF9AF
		STY &C3             ; F9AF 84 C3     .C  
.LF9B1
		LDA #&00            ; F9B1 A9 00     ).
		STA &C2             ; F9B3 85 C2     .B  
		LDY #&A0            ; F9B5 A0 A0         
		LDA #&08            ; F9B7 A9 08     ).  
.LF9B9
		BIT &C0             ; F9B9 24 C0     $@  
		BEQ LF9E5           ; F9BB F0 28     p(  

		PHA                 ; F9BD 48        H   
		STY &C4             ; F9BE 84 C4     .D  
		LDX &C3             ; F9C0 A6 C3     &C  
		LDA #&08            ; F9C2 A9 08     ).  
		STA &C5             ; F9C4 85 C5     .E  
.LF9C6
		LDA WKBASE + &0200,Y         ; F9C6 B9 00 22  9." 
		CMP WKBASE + &0008,X         ; F9C9 DD 08 20  ].  
		BNE LF9EC           ; F9CC D0 1E     P.  

		INY                 ; F9CE C8        H   
		LDA WKBASE + &0200,Y         ; F9CF B9 00 22  9." 
		CMP WKBASE + &0108,X         ; F9D2 DD 08 21  ].! 
		BNE LF9EC           ; F9D5 D0 15     P.  

		INY                 ; F9D7 C8        H   
		INX                 ; F9D8 E8        h   
		DEC &C5             ; F9D9 C6 C5     FE  
		BNE LF9C6           ; F9DB D0 E9     Pi  

		LDY &C4             ; F9DD A4 C4     $D  
		LDX &C6             ; F9DF A6 C6     &F  
		JSR LF8C5           ; F9E1 20 C5 F8   Ex 

		PLA                 ; F9E4 68        h   
.LF9E5
		STY &C2             ; F9E5 84 C2     .B  
		STA &C1             ; F9E7 85 C1     .A  
		JMP LF9EF           ; F9E9 4C EF F9  Loy 
 
.LF9EC
		LDY &C4             ; F9EC A4 C4     $D  
		PLA                 ; F9EE 68        h   
.LF9EF
		PHA                 ; F9EF 48        H   
		TYA                 ; F9F0 98        .   
		SEC                 ; F9F1 38        8   
		SBC #&20            ; F9F2 E9 20     i   
		TAY                 ; F9F4 A8        (   
		PLA                 ; F9F5 68        h   
		ASL A               ; F9F6 0A        .   
		BNE LF9B9           ; F9F7 D0 C0     P@  

		LDY &C2             ; F9F9 A4 C2     $B  
		BEQ LF9B1+1         ; F9FB F0 B5     p5  

		LDX &C3             ; F9FD A6 C3     &C  
		LDA #&08            ; F9FF A9 08     ).  
		STA &C5             ; FA01 85 C5     .E  
.LFA03
		LDA WKBASE + &0008,X         ; FA03 BD 08 20  =.  
		STA WKBASE + &0200,Y         ; FA06 99 00 22  .." 
		INY                 ; FA09 C8        H   
		LDA WKBASE + &0108,X         ; FA0A BD 08 21  =.! 
		STA WKBASE + &0200,Y         ; FA0D 99 00 22  .." 
		INY                 ; FA10 C8        H   
		INX                 ; FA11 E8        h   
		DEC &C5             ; FA12 C6 C5     FE  
		BNE LFA03           ; FA14 D0 ED     Pm  

		LDX #&10            ; FA16 A2 10     ".  
		LDA #&00            ; FA18 A9 00     ).  
.LFA1A
		STA WKBASE + &0200,Y         ; FA1A 99 00 22  .." 
		INY                 ; FA1D C8        H   
		DEX                 ; FA1E CA        J   
		BNE LFA1A           ; FA1F D0 F9     Py  

		LDA &C2             ; FA21 A5 C2     %B  
		TAY                 ; FA23 A8        (   
		JSR diva32           ; FA24 20 2F F1   /q 

		ADC #&22            ; FA27 69 22     i"  
		STA WKBASE + &0213,Y         ; FA29 99 13 22  .." 
		LDA &C1             ; FA2C A5 C1     %A  
		STA WKBASE + &021B,Y         ; FA2E 99 1B 22  .." 
		ORA &C0             ; FA31 05 C0     .@  
		STA &C0             ; FA33 85 C0     .@  
		LDA WKBASE + &0209,Y         ; FA35 B9 09 22  9." 
		ADC #&FF            ; FA38 69 FF     i.  
		LDA WKBASE + &020B,Y         ; FA3A B9 0B 22  9." 
		ADC #&00            ; FA3D 69 00     i.  
		STA WKBASE + &0219,Y         ; FA3F 99 19 22  .." 
		LDA WKBASE + &020D,Y         ; FA42 B9 0D 22  9." 
		ORA #&0F            ; FA45 09 0F     ..  
		ADC #&00            ; FA47 69 00     i.  
		JSR diva16           ; FA49 20 30 F1   0q 

		STA WKBASE + &021A,Y         ; FA4C 99 1A 22  .." 
		PLP                 ; FA4F 28        (   
		BCC LFA81           ; FA50 90 2F     ./  

		LDA WKBASE + &0209,Y         ; FA52 B9 09 22  9." 
		STA WKBASE + &0214,Y         ; FA55 99 14 22  .." 
		LDA WKBASE + &020B,Y         ; FA58 B9 0B 22  9." 
		STA WKBASE + &0215,Y         ; FA5B 99 15 22  .." 
		LDA WKBASE + &020D,Y         ; FA5E B9 0D 22  9." 
		JSR diva16           ; FA61 20 30 F1   0q 

		STA WKBASE + &0216,Y         ; FA64 99 16 22  .." 
.LFA67
		LDA DRIVENO             ; FA67 A5 DC     %\  
		AND #&0F            ; FA69 29 0F     ).  
		ORA WKBASE + &0217,Y         ; FA6B 19 17 22  .." 
		STA WKBASE + &0217,Y         ; FA6E 99 17 22  .." 
		JSR LFAF8           ; FA71 20 F8 FA   xz 

		LDA &CD             ; FA74 A5 CD     %M  
		STA &EF             ; FA76 85 EF     .o  
.LFA78
		STY &C3             ; FA78 84 C3     .C  
		LDX &C6             ; FA7A A6 C6     &F  
		PLA                 ; FA7C 68        h   
		TAY                 ; FA7D A8        (   
		LDA &C3             ; FA7E A5 C3     %C  
		RTS                 ; FA80 60        `   
 
.LFA81
		LDA #&20            ; FA81 A9 20     )   
		STA WKBASE + &0217,Y         ; FA83 99 17 22  .." 
		BNE LFA67           ; FA86 D0 DF     P_
.LFA88  
		PHA                 ; FA88 48        H   
		STY &C2             ; FA89 84 C2     .B  
		ASL A               ; FA8B 0A        .   
		ASL A               ; FA8C 0A        .   
		ADC &C2             ; FA8D 65 C2     eB  
		TAY                 ; FA8F A8        (   
		LDA WKBASE + &0210,Y         ; FA90 B9 10 22  9." 
		STA &00,X           ; FA93 95 00     ..  
		LDA WKBASE + &0211,Y         ; FA95 B9 11 22  9." 
		STA &01,X           ; FA98 95 01     ..  
		LDA WKBASE + &0212,Y         ; FA9A B9 12 22  9." 
		STA &02,X           ; FA9D 95 02     ..  
		LDY &C2             ; FA9F A4 C2     $B  
		PLA                 ; FAA1 68        h   
		RTS                 ; FAA2 60        `   
 
.LFAA3
		PHA                 ; FAA3 48        H   
		STX &C6             ; FAA4 86 C6     .F  
		TYA                 ; FAA6 98        .   
		AND #&E0            ; FAA7 29 E0     )`  
		STA &C2             ; FAA9 85 C2     .B  
		BEQ LFABE           ; FAAB F0 11     p.  

		JSR diva32           ; FAAD 20 2F F1   /q 
		TAY                 ; FAB0 A8        (   
		LDA #&00            ; FAB1 A9 00     ).  
		SEC                 ; FAB3 38        8   
.LFAB4
		ROR A               ; FAB4 6A        j   
		DEY                 ; FAB5 88        .   
		BNE LFAB4           ; FAB6 D0 FC     P|  

		LDY &C2             ; FAB8 A4 C2     $B  
		BIT &C0             ; FABA 24 C0     $@  
		BNE LFAC1           ; FABC D0 03     P.  
.LFABE
		PLA                 ; FABE 68        h   
		SEC                 ; FABF 38        8   
		RTS                 ; FAC0 60        `   
 
.LFAC1
		PLA                 ; FAC1 68        h   
		CLC                 ; FAC2 18        .   
.LFAC3
		RTS                 ; FAC3 60        `   
 
.LFAC4
		LDX #&20            ; FAC4 A2 20     "   
		TXA                 ; FAC6 8A        .   
		CLC                 ; FAC7 18        .   
.LFAC8
		ADC WKBASE + &0200,Y         ; FAC8 79 00 22  y." 
		INY                 ; FACB C8        H   
		DEX                 ; FACC CA        J   
		BNE LFAC8           ; FACD D0 F9     Py  

		ADC #&00            ; FACF 69 00     i.  
		LDY &C2             ; FAD1 A4 C2     $B  
		RTS                 ; FAD3 60        `   
 
.LFAD4
		JSR LFAC4           ; FAD4 20 C4 FA   Dz 
		CMP #&FF            ; FAD7 C9 FF     I.  
		BEQ LFAC3           ; FAD9 F0 E8     ph  
		BRK                 ; FADB 00        .   
.LFADC
		CLC                 ; FADC 18        .   
		LDA WKBASE + &020F,Y         ; FADD B9 0F 22  9." 
		ADC WKBASE + &0211,Y         ; FAE0 79 11 22  y." 
		STA &E6             ; FAE3 85 E6     .f  
		STA WKBASE + &021C,Y         ; FAE5 99 1C 22  .." 
		LDA WKBASE + &020D,Y         ; FAE8 B9 0D 22  9." 
		AND #&0F            ; FAEB 29 0F     ).  
		ADC WKBASE + &0212,Y         ; FAED 79 12 22  y." 
		STA &E5             ; FAF0 85 E5     .e  
		STA WKBASE + &021D,Y         ; FAF2 99 1D 22  .." 
		JSR LFB8C           ; FAF5 20 8C FB   .{ 
.LFAF8
		LDA #&FF            ; FAF8 A9 FF     ).  
		STA WKBASE + &021F,Y         ; FAFA 99 1F 22  .." 
		JSR LFAC4           ; FAFD 20 C4 FA   Dz 

		EOR #&FF            ; FB00 49 FF     I.  
.LFB02
		STA WKBASE + &021F,Y         ; FB02 99 1F 22  .." 
		RTS                 ; FB05 60        `   
 
.LFB06
		CLC                 ; FB06 18        .   
		ADC WKBASE + &021F,Y         ; FB07 79 1F 22  y." 
		ADC #&00            ; FB0A 69 00     i.  
		BNE LFB02           ; FB0C D0 F4     Pt  
.LFB0E
		JSR OSECHO          ; FB0E 20 E6 FF   f. 
		CMP #&04            ; FB11 C9 04     I.  
		BEQ LFB37           ; FB13 F0 22     p"  
		
		CLC                 ; FB15 18        .   
		RTS                 ; FB16 60        `   

.LFB17 
		CLD                 ; FB17 D8        X   
		TYA                 ; FB18 98        .   
		BEQ LFB0E           ; FB19 F0 F3     ps  

		JSR LFAA3           ; FB1B 20 A3 FA   #z 
		BCS LFB5A+1         ; FB1E B0 3B     0;  

		TYA                 ; FB20 98        .   
		JSR LFD21           ; FB21 20 21 FD   !} 
		BNE LFB3B           ; FB24 D0 15     P.  

		LDA WKBASE + &0217,Y         ; FB26 B9 17 22  9." 
		AND #&10            ; FB29 29 10     ).  
		BNE LFB7B+1         ; FB2B D0 4F     PO

		LDA #&10            ; FB2D A9 10     ).  
		JSR LFB8E           ; FB2F 20 8E FB   .{ 
		JSR LFAF8           ; FB32 20 F8 FA   xz 

		LDX &C6             ; FB35 A6 C6     &F  
.LFB37
		LDA #&FF            ; FB37 A9 FF     ).  
		SEC                 ; FB39 38        8   
		RTS                 ; FB3A 60        `   
 
.LFB3B
		LDA WKBASE + &0217,Y         ; FB3B B9 17 22  9." 
		BMI LFB50           ; FB3E 30 10     0.  

		JSR LFAD4           ; FB40 20 D4 FA   Tz 
		JSR LF965           ; FB43 20 65 F9   ey 
		JSR LFB9D           ; FB46 20 9D FB   .{ 
		SEC                 ; FB49 38        8   
		JSR LFBA5           ; FB4A 20 A5 FB   %{ 
		JSR LF549           ; FB4D 20 49 F5   Iu 
.LFB50
		LDA WKBASE + &0210,Y         ; FB50 B9 10 22  9." 
		STA FILENAMEPTR             ; FB53 85 DD     .]  
		LDA WKBASE + &0213,Y         ; FB55 B9 13 22  9." 
		STA FILENAMEPTR+1             ; FB58 85 DE     .^  
.LFB5A
		LDY #&00            ; FB5A A0 00      .
		LDA (FILENAMEPTR),Y         ; FB5C B1 DD     1]  
		PHA                 ; FB5E 48        H   
		LDY &C2             ; FB5F A4 C2     $B  
		LDA #&FE            ; FB61 A9 FE     )~  
		JSR LFB06           ; FB63 20 06 FB   .{ 

		LDX FILENAMEPTR             ; FB66 A6 DD     &]  
		INX                 ; FB68 E8        h   
		TXA                 ; FB69 8A        .   
		STA WKBASE + &0210,Y         ; FB6A 99 10 22  .." 
		BNE LFB88           ; FB6D D0 19     P.  

		CLC                 ; FB6F 18        .   
		LDA WKBASE + &0211,Y         ; FB70 B9 11 22  9." 
		ADC #&01            ; FB73 69 01     i.  
		STA WKBASE + &0211,Y         ; FB75 99 11 22  .." 
		LDA WKBASE + &0212,Y         ; FB78 B9 12 22  9." 
.LFB7B
		ADC #&00            ; FB7B 69 00     i.
		STA WKBASE + &0212,Y         ; FB7D 99 12 22  .." 
		JSR LFB93           ; FB80 20 93 FB   .{ 

		LDA #&80            ; FB83 A9 80     ).  
		JSR LFB06           ; FB85 20 06 FB   .{ 
.LFB88
		CLC                 ; FB88 18        .   
		JMP LF8D5           ; FB89 4C D5 F8  LUx 
 
.LFB8C
		LDA #&80            ; FB8C A9 80     ).  
.LFB8E
		ORA WKBASE + &0217,Y         ; FB8E 19 17 22  .." 
		BNE LFB98           ; FB91 D0 05     P.  
.LFB93
		LDA #&7F            ; FB93 A9 7F     ).  
.LFB95
		AND WKBASE + &0217,Y         ; FB95 39 17 22  9." 
.LFB98
		STA WKBASE + &0217,Y         ; FB98 99 17 22  .." 
		CLC                 ; FB9B 18        .   
		RTS                 ; FB9C 60        `   
 
.LFB9D
		LDA WKBASE + &0217,Y         ; FB9D B9 17 22  9." 
		AND #&40            ; FBA0 29 40     )@  
		BEQ LFBDE           ; FBA2 F0 3A     p:  
		CLC                 ; FBA4 18        .   
.LFBA5
		PHP                 ; FBA5 08        .   
		JSR 	START_MOTOR_SELECT           ; FBA6 20 77 F7   ww 

		LDY &C2             ; FBA9 A4 C2     $B  
		LDA WKBASE + &0213,Y         ; FBAB B9 13 22  9." 
		STA &E0             ; FBAE 85 E0     .`  
.LFBB0
		LDA #&00            ; FBB0 A9 00     ).
		STA &DF             ; FBB2 85 DF     ._  
		STA &E3             ; FBB4 85 E3     .c  
		LDA #&01            ; FBB6 A9 01     ).  
		STA &E4             ; FBB8 85 E4     .d  
		PLP                 ; FBBA 28        (   
		BCS LFBD3           ; FBBB B0 16     0.  

		LDA WKBASE + &021C,Y         ; FBBD B9 1C 22  9." 
		STA &E6             ; FBC0 85 E6     .f  
		LDA WKBASE + &021D,Y         ; FBC2 B9 1D 22  9." 
		STA &E5             ; FBC5 85 E5     .e  
		JSR LF713           ; FBC7 20 13 F7   .w 

		LDY &C2             ; FBCA A4 C2     $B  
		LDA #&BF            ; FBCC A9 BF     )?  
		JSR LFB95           ; FBCE 20 95 FB   .{ 
		BCC LFBD9           ; FBD1 90 06     ..  
.LFBD3
		JSR LFADC           ; FBD3 20 DC FA   \z 
		JSR LF4EC           ; FBD6 20 EC F4   lt 
.LFBD9
		LDY &C2             ; FBDC A4 C2     $B  
.LFBDE
		RTS                 ; FBDE 60        `   
 
.LFBDF
		PLA                 ; FBDF 68        h   
		JMP OSASCI           ; FBE0 4C E9 FF  Li. 
 
.LFBE3
		CLD                 ; FBE3 D8        X   
		PHA                 ; FBE4 48        H   
		TYA                 ; FBE5 98        .   
		BEQ LFBDF           ; FBE6 F0 F7     pw  

		JSR LFAA3           ; FBE8 20 A3 FA   #z 
		BCS LFC45+1         ; FBEB B0 59     0Y

		JSR LFAD4           ; FBED 20 D4 FA   Tz 
		LDA WKBASE + &020E,Y         ; FBF0 B9 0E 22  9." 
		BMI LFBB0+1         ; FBF3 30 BC     0<

		JSR LF965           ; FBF5 20 65 F9   ey 
		TYA                 ; FBF8 98        .   
		CLC                 ; FBF9 18        .   
		ADC #&04            ; FBFA 69 04     i.  
		JSR LFD21           ; FBFC 20 21 FD   !} 
		BNE LFC52           ; FBFF D0 51     PQ  

		JSR LF93C           ; FC01 20 3C F9   <y 
		LDX &C4             ; FC04 A6 C4     &D  
		SEC                 ; FC06 38        8   
		LDA WKBASE + &0107,X         ; FC07 BD 07 21  =.! 
		SBC WKBASE + &010F,X         ; FC0A FD 0F 21  }.! 
		PHA                 ; FC0D 48        H   
		LDA WKBASE + &0106,X         ; FC0E BD 06 21  =.! 
		SBC WKBASE + &010E,X         ; FC11 FD 0E 21  }.! 
		AND #&0F            ; FC14 29 0F     ).  
		STA &C3             ; FC16 85 C3     .C  
		ASL A               ; FC18 0A        .   
		ASL A               ; FC19 0A        .   
		ASL A               ; FC1A 0A        .   
		ASL A               ; FC1B 0A        .   
		EOR WKBASE + &010E,X         ; FC1C 5D 0E 21  ].! 
		AND #&F0            ; FC1F 29 F0     )p  
		EOR WKBASE + &010E,X         ; FC21 5D 0E 21  ].! 
		CMP WKBASE + &010E,X         ; FC24 DD 0E 21  ].! 
		STA WKBASE + &010E,X         ; FC27 9D 0E 21  ..! 
		BNE LFC39           ; FC2A D0 0D     P.  

		PLA                 ; FC2C 68        h   
		CMP WKBASE + &010D,X         ; FC2D DD 0D 21  ].! 
		BNE LFC3A           ; FC30 D0 08     P.  

		JSR LF549           ; FC32 20 49 F5   Iu 
		JSR LF8C5           ; FC35 20 C5 F8   Ex 
		BRK                 ; FC38 00        .   
.LFC39
		PLA                 ; FC39 68        h   
.LFC3A
		STA WKBASE + &010D,X         ; FC3A 9D 0D 21  ..! 
		STA WKBASE + &0219,Y         ; FC3D 99 19 22  .." 
		LDA &C3             ; FC40 A5 C3     %C  
		STA WKBASE + &021A,Y         ; FC42 99 1A 22  .." 
.LFC45
		LDA #&00            ; FC45 A9 00     ).
		STA WKBASE + &010C,X         ; FC47 9D 0C 21  ..! 
		JSR write_cat           ; FC4A 20 66 F7   fw 
		LDY &C2             ; FC50 A4 C2     $B  
.LFC52
		LDA WKBASE + &0217,Y         ; FC52 B9 17 22  9." 
		BMI LFC6E           ; FC55 30 17     0.  

		JSR LFB9D           ; FC57 20 9D FB   .{ 
		LDA WKBASE + &0214,Y         ; FC5A B9 14 22  9." 
		BNE LFC6A           ; FC5D D0 0B     P.  

		TYA                 ; FC5F 98        .   
		JSR LFD21           ; FC60 20 21 FD   !} 
		BNE LFC6A           ; FC63 D0 05     P.  

		JSR LFADC           ; FC65 20 DC FA   \z 
		BNE LFC6E           ; FC68 D0 04     P.  
.LFC6A
		SEC                 ; FC6A 38        8   
		JSR LFBA5           ; FC6B 20 A5 FB   %{ 
.LFC6E
		LDA WKBASE + &0210,Y         ; FC6E B9 10 22  9." 
		STA FILENAMEPTR             ; FC71 85 DD     .]  
		LDA WKBASE + &0213,Y         ; FC73 B9 13 22  9." 
		STA FILENAMEPTR+1             ; FC76 85 DE     .^  
		PLA                 ; FC78 68        h   
		LDY #&00            ; FC79 A0 00      .  
		STA (FILENAMEPTR),Y         ; FC7B 91 DD     .]  
		PHA                 ; FC7D 48        H   
		LDY &C2             ; FC7E A4 C2     $B  
		LDA #&40            ; FC80 A9 40     )@  
		JSR LFB8E           ; FC82 20 8E FB   .{ 

		INC FILENAMEPTR             ; FC85 E6 DD     f]  
		LDA FILENAMEPTR             ; FC87 A5 DD     %]  
		STA WKBASE + &0210,Y         ; FC89 99 10 22  .." 
		BNE LFCA1           ; FC8C D0 13     P.  

		JSR LFB93           ; FC8E 20 93 FB   .{ 
		LDA WKBASE + &0211,Y         ; FC91 B9 11 22  9." 
		ADC #&01            ; FC94 69 01     i.  
		STA WKBASE + &0211,Y         ; FC96 99 11 22  .." 
		LDA WKBASE + &0212,Y         ; FC99 B9 12 22  9." 
.LFC9C
		ADC #&00            ; FC9C 69 00     i.
		STA WKBASE + &0212,Y         ; FC9E 99 12 22  .." 
.LFCA1
		TYA                 ; FCA1 98        .   
		JSR LFD21           ; FCA2 20 21 FD   !} 
		BCC LFCBE           ; FCA5 90 17     ..  

		LDA #&20            ; FCA7 A9 20     )   
		JSR LFB8E           ; FCA9 20 8E FB   .{ 

		LDA WKBASE + &0210,Y         ; FCAC B9 10 22  9." 
		STA WKBASE + &0214,Y         ; FCAF 99 14 22  .." 
		LDA WKBASE + &0211,Y         ; FCB2 B9 11 22  9." 
		STA WKBASE + &0215,Y         ; FCB5 99 15 22  .." 
		LDA WKBASE + &0212,Y         ; FCB8 B9 12 22  9." 
		STA WKBASE + &0216,Y         ; FCBB 99 16 22  .." 
.LFCBE
		JSR LFAF8           ; FCBE 20 F8 FA   xz 
		JSR LF549           ; FCC1 20 49 F5   Iu 
		JMP LFB88           ; FCC4 4C 88 FB  L.{ 

.LFCC7 
		PHA                 ; FCC7 48        H   
		JSR LFAA3           ; FCC8 20 A3 FA   #z 
		BCS LFC9C+1         ; FCCB B0 D0     0P  

		JSR LFAD4           ; FCCD 20 D4 FA   Tz 
		LDX &C6             ; FCD0 A6 C6     &F  
		JSR incy4           ; FCD2 20 39 F1   9q 
		JSR LFD39           ; FCD5 20 39 FD   9} 
		BCC LFD3C+1         ; FCD8 90 63     .c

		LDY &C2             ; FCDA A4 C2     $B  
.LFCDC
		JSR LFD39           ; FCDC 20 39 FD   9} 
		BCS LFCE8           ; FCDF B0 07     0.  

		LDA #&FF            ; FCE1 A9 FF     ).  
		JSR LFBE3           ; FCE3 20 E3 FB   c{ 
		BNE LFCDC           ; FCE6 D0 F4     Pt  
.LFCE8
		LDA &00,X           ; FCE8 B5 00     5.  
		STA WKBASE + &0210,Y         ; FCEA 99 10 22  .." 
		LDA &01,X           ; FCED B5 01     5.  
		STA WKBASE + &0211,Y         ; FCEF 99 11 22  .." 
		LDA &02,X           ; FCF2 B5 02     5.  
		STA WKBASE + &0212,Y         ; FCF4 99 12 22  .." 
		LDA #&6F            ; FCF7 A9 6F     )o  
		JSR LFB95           ; FCF9 20 95 FB   .{ 

		LDA WKBASE + &020F,Y         ; FCFC B9 0F 22  9." 
		ADC WKBASE + &0211,Y         ; FCFF 79 11 22  y." 
		STA &C5             ; FD02 85 C5     .E  
		LDA WKBASE + &020D,Y         ; FD04 B9 0D 22  9." 
		AND #&0F            ; FD07 29 0F     ).  
		ADC WKBASE + &0212,Y         ; FD09 79 12 22  y." 
		CMP WKBASE + &021D,Y         ; FD0C D9 1D 22  Y." 
		BNE LFD1B           ; FD0F D0 0A     P.  

		LDA &C5             ; FD11 A5 C5     %E  
		CMP WKBASE + &021C,Y         ; FD13 D9 1C 22  Y." 
		BNE LFD1B           ; FD16 D0 03     P.  

		JSR LFB8C           ; FD18 20 8C FB   .{ 
.LFD1B
		JSR LFAF8           ; FD1B 20 F8 FA   xz 
		JMP LFB88           ; FD1E 4C 88 FB  L.{ 
 
.LFD21
		TAX                 ; FD21 AA        *   
		LDA WKBASE + &0212,Y         ; FD22 B9 12 22  9." 
		CMP WKBASE + &0216,X         ; FD25 DD 16 22  ]." 
		BNE LFD38           ; FD28 D0 0E     P.  

		LDA WKBASE + &0211,Y         ; FD2A B9 11 22  9." 
		CMP WKBASE + &0215,X         ; FD2D DD 15 22  ]." 
		BNE LFD38           ; FD30 D0 06     P.  

		LDA WKBASE + &0210,Y         ; FD32 B9 10 22  9." 
		CMP WKBASE + &0214,X         ; FD35 DD 14 22  ]." 
.LFD38
		RTS                 ; FD38 60        `   
 
.LFD39
		LDA WKBASE + &0214,Y         ; FD39 B9 14 22  9." 
.LFD3C
		CMP &00,X           ; FD3C D5 00     U.
		LDA WKBASE + &0215,Y         ; FD3E B9 15 22  9." 
		SBC &01,X           ; FD41 F5 01     u.  
		LDA WKBASE + &0216,Y         ; FD43 B9 16 22  9." 
		SBC &02,X           ; FD46 F5 02     u.  
		RTS                 ; FD48 60        `   

.LFD49
		PHA                 ; FD49 48        H   


; ******************************************************
; * SYSTEM KERNEL - Keyboard input, VDU/Printer output *
; ******************************************************
skipto $FD4A

; Convert Ctrl-Key to cursor movement, wait for another keypress
; --------------------------------------------------------------
.LFD4A
		ORA 	#&08
		AND 	#&0A
		ASLA       						; Convert to cursor movement
.LFD4F
		LSRA
		JSR 	LFD7E      				; Send character to screen

; =====================================
; OSRDCH - Read character from keyboard
; =====================================
; On exit, A=character read
;          P=corrupted
;          X,Y preserved
;
.LFD53
		BIT 	SVIARA					; Test keybaord
		BPL 	LFD53  					; Wait until key not pressed
.LFD58
		LDA 	SVIARA					
		BMI 	LFD58  					; Wait until key pressed

; Interpret Ctrl+QWASZ as cursor copying
; --------------------------------------
		CMP 	#&11
		BEQ 	LFD72   				; Ctrl-Q - Copy

		CMP 	#&17
		BEQ 	LFD4F   				; Ctrl-W - Do VDU 11 then get another key

		CMP 	#&01
		BEQ 	LFD4A   				; Ctrl-A - Do VDU 8 then get another key

		CMP 	#&13
		BEQ 	LFD4F   				; Ctrl-S - Do VDU 9 then get another key

		CMP 	#&1A
		BEQ 	LFD4A   				; Ctrl-Z - Do VDU 10 then get another key
		RTS

; Ctrl-Q - copy character
; -----------------------
.LFD72
		STY 	TEMPY              		; Save Y
		LDY 	#&00
		LDA 	(SCREENADDR),Y			; Get character from current cursor position
		LDY 	TEMPY
		RTS          					; Restore Y and return


; ========================================
; OSWRCH - Send character to output stream
; ========================================
; On entry, A=character to output
; On exit,  all registers preserved
;
.LFD7B
		JSR 	LFD8B            		; Send character to printer
.LFD7E
		PHP
		PHA              				; Save flags and A
		CLD                  			; Ensure Binary mode
		STY 	TEMPY              		; Save Y register
		JSR 	LFE4A            		; Send character to VDU
		PLA
		LDY 	TEMPY
		PLP      						; Restore registers
		RTS

; Send Character to printer via 6522 VIA
; --------------------------------------
; On entry, A=character
; On exit,  A,X,Y preserved
;  - Waits for the busy line VIA Port A bit 7 to go low, then dumps 7 bit
;    data to the 7 LSBs of Port A, and then strobes CA2 low for ~20uS.
;  - Enter with CA2 output set high.
;  - Preserves A,X,Y registers.

.LFD8B
		PHA                  			; Save character
		CMP 	#&02
		BEQ 	LFDB7   				; <STX> - start printing

		CMP 	#&03
		BEQ 	LFDC8   				; <ETX> - end printing

		CMP 	PRINTIGNORE
		BEQ 	LFDC6    				; If ignore-char, exit without printing

		LDA 	PVIAPCR            		; Read Peripheral Control Register
		AND 	#&0E
		BEQ 	LFDC6   				; Not set up, exit without printing
		
		PLA                  			; Get character back

; Wait for printer not busy
.LFDA0
		BIT 	PVIARA
		BMI 	LFDA0  					; Read BUSY in Port A b7, loop until not busy

; Write charcter to printer
		STA 	PVIARA            		; Write character to Port A
		PHA                  			; Save character to restore on exit

; Strobe printer
		LDA		PVIAPCR            		; Read Peripheral Control Register
		AND 	#&F0             		; Keep CB1 CB2 PortB control
		ORA 	#&0C             		; Set CA2 low to strobe printer
		STA 	PVIAPCR            		; Write to PCR to lower CA2/STROBE
		ORA 	#&02
		BNE 	LFDC3   				; Jump to set CA2 high and exit

; <STX> - start printing
; ----------------------
.LFDB7
		LDA 	#&7F
		STA 	PVIADDRA   				; Set PortA to b0-b6=output, b7=input
		LDA 	PVIAPCR            		; Get Peripheral Control Register
		AND 	#&F0
		ORA 	#&0E    				; Keep CB1/CB2/PortB, set CA2/STROBE high

.LFDC3
		STA 	PVIAPCR            		; Write to Peripheral Control Register
.LFDC6
		PLA
		RTS              				; Restore and exit

; <EXT> - end printing
; --------------------
.LFDC8
		LDA 	PVIAPCR            		; Get Peripheral Control Register
		AND 	#&F0
		BCS 	LFDC3   				; Keep CB1/CB2/PortB, clear PortA

.LFDCF
		PLA                 			; Restore & exit
		PLA                 
		RTS                 

; Move cursor position back one
; -----------------------------
.LFDD2
		DEY
		BPL 	LFDE9            		; Dec. X position, return if still positive

		LDY 	#SCREENCOLS-1          	; Set X position to last column of previous line
.LFDD7
		LDA 	LINESCROLL             	; Get lines left before scrolling
		CMP 	#SCREENLINES       		; Screen height  
		BCS 	LFDCF           		; 

		INC 	LINESCROLL              ; Increment lines before scroll
.LFDDF
		LDA 	&CF             		
		SBC 	#SCREENCOLS-1           
		STA 	&CF             
		BCS 	LFDE9           

		DEC 	SCREENTOP             		
.LFDE9
		RTS
 
.LFDEA
		DEC 	LINESCROLL
		RTS

; Do linefeed and scroll if needed
; --------------------------------
.LFDED
		LDY 	#SCREENCOLS
		JSR 	LFE21       			; Calculate address of character past end of line
										; This will be column zero of the next line
		LDA 	SCREENADDR
		STA 	&CF
		LDA 	SCROFFSET
		STA 	SCREENTOP
		LDA 	LINESCROLL
		BNE 	LFDEA        

		LDY 	#DSTARTL				; Select R13 Display start low
		STY 	SCRADDRREG       
		LDA 	&CF                  	 
		SEC                      
if (SYS40 = 0)
		SBC 	#&80
else
		SBC 	#&C0
endif
		STA 	SCRDATAREG              ; Sel low byte 
		DEY
		STY 	SCRADDRREG            	; Select R12 Display start high
		LDA 	SCREENTOP                  
if (SYS40 = 0)
		SBC 	#&07                 
else
		SBC 	#&03
endif
		STA 	SCRDATAREG           	; set it 
		LDY 	#SCREENCOLS-1          	; Length of line to fill
		LDA 	#' '              		; Fill next line with spaces
.LFE1A
		JSR 	LFE36                	; Store space character
		DEY
		BPL 	LFE1A            		; Loop for whole line
		RTS

; Calculate screen output address
; -------------------------------
; On entry, Y     = X position (from &D1)
;           &CF   = ?
; On exit,  &D2/D3=>output address
;           &D2/D4= offset into screen memory
;
.LFE21
		PHA
		CLC
		TYA								; Get Xpos in A
		ADC 	&CF
		STA 	SCREENADDR				

		LDA 	SCREENTOP
		ADC 	#&00
		STA 	SCROFFSET

		AND 	#&07					; mask out all but bottom 3 bits
if (SYS40 = 0)
		ORA 	#&10					; make sure value is 00001xxx
else
		ORA		#&04
endif
		STA 	SCREENADDR+1

		PLA
		RTS

; Store character in screen memory
; --------------------------------
.LFE36
		JSR 	LFE21               ; Calculate output address
		STY 	SCROFFSET           ; Save Y
		LDY 	#&00
		STA 	(SCREENADDR),Y     	; Store character in screen memory
		LDY 	SCROFFSET
		RTS              			; Restore Y and return

.LFE42
		CLC                 		; Clear carry
.LFE43
		PHP                 		; save flags   
		ASL 	SCREENXPOS          ; 
		PLP                 		; 
		ROR 	SCREENXPOS          ; set b7=0
		RTS                 		   

; Send Character to Screen
; ------------------------
; On entry, A=character
; On exit,  registers allowed to be corrupted
;
;  - Prints non-control codes (#20 to #FF) at the current cursor position on
;    the screen.
;  - Executes the following control codes:
;  
;    <ACK><BS><HT><LF><VT><FF><CR><NAK><RS><DEL>
;      6   8   9   10  11  12  13  21   30  127

.LFE4A
		CMP 	#&06
		BEQ 	LFE42       			; <ACK> - turn on VDU

		CMP 	#&15
		BEQ 	LFE43       			; <NAK> - turn off VDU

		LDY 	SCREENXPOS
		BMI 	LFE83        			; Get X position, b7=VDU turned off, exit without printing

		CMP 	#&20
		BCC 	LFE8E       			; less than <SPC> - control charater

		CMP 	#&7F
		BEQ 	LFE84       			; <DEL> - delete character

; Print a printable character on screen
; -------------------------------------
.LFE5E
		JSR 	LFE36                	; Store character in screen memory

; VDU 9 - Cursor right
; --------------------
.LFE61
		INY
		CPY 	#SCREENCOLS
		BCC 	LFE6B   				; Incrment X, skip if not reached end of line
		
		JSR 	LFDED                	; Do a linefeed and scroll if needed

; VDU 13 - Carriage Return
; ------------------------
.LFE69
		LDY 	#&00                 	; Make X position = 0
.LFE6B
		JSR 	LFE21                	; Calculate new character address
		STY 	SCREENXPOS              ; Store updated X position
		LDY 	#CPOSL
		STY 	SCRADDRREG       		; Select CTRC Reg 15 - cursor address low
		LDA 	SCREENADDR
		STA 	SCRDATAREG        		; set it
		DEY
		STY 	SCRADDRREG            	; Select CRTC Reg 14 - cursor address high
		LDY 	SCROFFSET
		STY 	SCRDATAREG        		;
.LFE83
		RTS

; VDU 127 - Delete
; ----------------
.LFE84
		JSR 	LFDD2                	; Move cursor back one
		LDA 	#&20
		JSR 	LFE36       			; Store <SPC> in current position
		BPL 	LFE6B                	; Jump to update cursor position

; Control characters
; ------------------
.LFE8E
		CMP 	#&0D
		BEQ 	LFE69       			; <CR>

		CMP 	#&0A
		BEQ 	LFEB2       			; <DOWN>

		CMP 	#&0C
		BEQ 	LFED1       			; <CLS>

		CMP 	#&08
		BEQ 	LFEAC       			; <LEFT>

		CMP 	#&1E
		BEQ 	LFEBA       			; <HOME>

		CMP 	#&0B
		BEQ 	LFECB       			; <UP>
		
		CMP 	#&09
		BNE 	LFE5E       			; Not <RIGHT> - store in screen memory
		BCS 	LFE61                	; <RIGHT>

; VDU 8 - Cursor left
; -------------------
.LFEAC
		JSR 	LFDD2                	; Move cursor back one
		JMP 	LFE6B                	; Jump to update cursor position

; VDU 10 - Cursor down
; --------------------
.LFEB2
		JSR 	LFDED                	; Do a linefeed and scroll if needed
		LDY 	SCREENXPOS              ; Get X position
		JMP 	LFE6B                	; Jump to update cursor position

; VDU 30 - Cursor home
; --------------------
.LFEBA
		LDA 	#SCREENLINES			; Setup lines till scroll
		LDY 	LINESCROLL    			; get current value         	
		STA 	LINESCROLL             	; reset it
.LFEC0
		CPY 	#SCREENLINES            ; Where we currently on the top line ?
		BCS 	LFE69           		; yes : just do a carrage return  

		INY                 			;  
		JSR 	LFDDF           		; cursor back one
		JMP 	LFEC0

; VDU 11 - Cursor up
; ------------------
.LFECB
		JSR 	LFDD7
		JMP 	LFE6B                	; Jump to update cursor position

; VDU 12 - CLS
; ------------
.LFED1
		LDA 	#' '              		; Fill screen with space characters
		LDY 	#&00                 	; Loop through each page of screen
.LFED5
		STA 	SCREENBASE + &0000,Y	; Fill screen memory with spaces
		STA 	SCREENBASE + &0100,Y  				
		STA 	SCREENBASE + &0200,Y
		STA 	SCREENBASE + &0300,Y
if (SYS40 = 0)
		STA 	SCREENBASE + &0400,Y
		STA 	SCREENBASE + &0500,Y
		STA 	SCREENBASE + &0600,Y
		STA 	SCREENBASE + &0700,Y
		INY
		BNE 	LFED5            		; Loop for all 256 bytes in each page
else
		INY
		BNE 	LFED5            		; Loop for all 256 bytes in each page
		
		STY 	&CF
		STY 	&CF
		STY 	&CF
		STY 	&CF
		STY 	&CF
		STY 	&CF
endif
		STY 	&CF
		STY 	SCREENXPOS
		
		LDY 	#&0D                 	; Set up CRTC registers
.LFEF6
		STY 	SCRADDRREG              ; Select CRTC register
		LDA 	CRTCREGS,Y
		STA 	SCRDATAREG    			; Write CRTC data from CRTC table
		DEY
		BPL 	LFEF6            		; Loop for all CRTC registers

		LDA 	#SCREENLINES			; Setup lines till scroll
		STA 	LINESCROLL
if (SYS40 = 0)
		LDA 	#&10
else
		LDA 	#&04
endif
		STA 	SCREENTOP      			; &D0=&10xx - start of screen
		JMP 	LFE69                	; Set CRTC cursor position


; ========================================================
; RESET - Set up vectors, reset hardware, enter supervisor
; ========================================================
.RESET
		LDX 	#&19
.LFF0F
		LDA 	DEF_VECTORS,X
		STA 	brkv,X  				; Reset all vectors except NMIV
		DEX
		BPL 	LFF0F

		TXS                      		; Reset stack
		CLD                      		; Set to Binary
		LDA 	#' '
		STA 	SETQUAL
		STA 	USEQUAL 				; Set current and saved prefix to <SPC>
		LDA 	#&0A
		STA 	PRINTIGNORE         	; Set printer ignore character to CHR$10
		LDY 	#&00
		STY 	DRIVENO
		STY 	&C0 					; Set drive 0
		STY 	EXECHANDLE
		STY 	SPOOLHANDLE          	; Set EXEC and SPOOL handles to 0

;        LDA     #0						; FDC reselt line low           
        STY     PCTRL                     

		JSR 	LFED1                	; Do CLS to set up screen

;        LDX     #3        				; Delay a short while for reset              
;        JSR     DELAY                   
        LDA     #PRESET	+ PDDEN			; Reset high, single density              
        STA     PCTRL

		BIT 	SVIARA
		BVS 	Supervisor      		; If key<'@' pressed, don't skip past

		LDX 	#&04              		; Noormally SPACE-BREAK to boot
.LFF40
		LDA 	LFF89,X
		STA 	STACKPAGE,X  			; Copy "BOOT" to command buffer
		DEX
		BPL 	LFF40

		JSR 	READ_CAT                ; Get catalogue

		LDY 	#&00
		LDX 	#&00

		LDA 	WKBASE + &0106
		JSR 	diva16      			; Get boot option, WKBASE + &0016 divided by 16
		BEQ 	Supervisor                	; Option=0, enter supervisor

		JSR 	LFF5E                	; Check other options
		JMP 	Supervisor                	; Continue to supervisor

; Check disk boot option
; ----------------------
.LFF5E
		CMP 	#&02
		BCC 	LFF6A       			; Option = 1 - jump to do *LOAD BOOT
		BEQ 	LFF67                	; Option = 2 - jump to do *RUN BOOT

		JMP 	execcom                	; Option > 2 - jump to do *EXEC BOOT

.LFF67	
		JMP 	runcom         			; Boot option 2
.LFF6A	
		JMP 	loadcom         		; Boot option 1


; Enter supervisor - *command prompt
; ==================================
.Supervisor
		JSR 	INLINE_PRINT       		; Print inline text
		EQUB 	"Acorn Dos",10

; Default BRKV handler
; --------------------
.LFF7A
		LDX 	#&FF
		TXS             				; Reset stack
		JSR 	OSCRLF
.LFF80
		JSR 	LF029                	; Read a line of text
		JSR 	OS_CLI               	; Pass to OSCLI to execute
		JMP 	LFF80                	; Loop back for another line
 
.LFF89
		EQUB 	"BOOT", 13


if (SYS40 = 0)
; 6845 CRTC default register values
; =================================
; This is functionally identical to BBC MODE 3
.CRTCREGS
		EQUB 	&7F ;  0 Horizontal Total         =128
		EQUB 	&50 ;  1 Horizontal Displayed     =80
		EQUB 	&66 ;  2 Horizontal Sync          =&66
		EQUB 	&62 ;  3 HSync Width+VSync        =&62
		EQUB 	&1E ;  4 Vertical Total           =30
		EQUB 	&02 ;  5 Vertical Adjust          =2
		EQUB 	&19 ;  6 Vertical Displayed       =25
		EQUB 	&1B ;  7 VSync Position           =&1B
		EQUB 	&40 ;  8 Interlace+Cursor         =&40 Cursor=1, Display=0, Interlace=Normal
		EQUB 	&09 ;  9 Scan Lines/Character     =10
		EQUB 	&68 ; 10 Cursor start line        =8   Blink=On, Speed=1/32, Line=8
		EQUB 	&09 ; 11 Cursor end scan line     =9
		EQUB 	&10 ; 12 Screen Start Address High=&10xx
		EQUB 	&00 ; 13 Screen Start Address Low =&xx00
					; 14 Cursor Address High
					; 15 Cursor Address Low
else
; 6845 CRTC default register values
; =================================
; I suspect that this is functionally identical to BBC MODE 7
.CRTCREGS
		EQUB 	&3F ;  0 Horizontal Total         =63
		EQUB 	&28 ;  1 Horizontal Displayed     =40
		EQUB 	&33 ;  2 Horizontal Sync          =&33
		EQUB 	&44 ;  3 HSync Width+VSync        =&44
		EQUB 	&1E ;  4 Vertical Total           =30
		EQUB 	&02 ;  5 Vertical Adjust          =2
		EQUB 	&19 ;  6 Vertical Displayed       =25
		EQUB 	&1B ;  7 VSync Position           =&1B
		EQUB 	&03 ;  8 Interlace+Cursor         =&40 Cursor=1, Display=0, Interlace=Normal
		EQUB 	&12 ;  9 Scan Lines/Character     =10
		EQUB 	&72 ; 10 Cursor start line        =8   Blink=On, Speed=1/32, Line=8
		EQUB 	&13 ; 11 Cursor end scan line     =9
		EQUB 	&04 ; 12 Screen Start Address High=&10xx
		EQUB 	&00 ; 13 Screen Start Address Low =&xx00
					; 14 Cursor Address High
					; 15 Cursor Address Low
endif

; Default vectors
; ===============
.DEF_VECTORS
							; &0200 - NMIV
		EQUW 	LFF7A 		; &0202 - BRKV
		EQUW 	LF531 		; &0204 - IRQV
.LFFA0
		EQUW 	LF42E 		; &0206 - CLIV
		EQUW 	LFD7B 		; &0208 - WRCHV
		EQUW 	LFD53 		; &020A - RDCHV
		EQUW 	NEW_LODVEC 	; &020C - LOADV
		EQUW 	NEW_SAVVEC 	; &020E - SAVEV
		EQUW 	LFA88 		; &0210 - RDARV
		EQUW 	LFCC7 		; &0212 - STARV
		EQUW 	LFB17 		; &0214 - BGETV
		EQUW	LFBE3 		; &0216 - BPUTV
		EQUW 	LF97A 		; &0218 - FINDV
		EQUW 	LF8C5 		; &021A - SHUTV
           ; &021C
           ; &021E

;SKIPTO $FFB6
; IRQ Handler
; ===========
;.LFFB6
.IRQBRK
		STA 	TEMPA             		; Save A in TEMPA
		PLA
		PHA             				; Get stacked flags

		AND 	#&10
		BNE 	LFFC4  					; Not IRQ, pass on to BRK handler

		LDA 	TEMPA
		PHA         					; Get A back, stack it
		JMP 	(irqv)         			; Pass on to IRQV

.LFFC4
		LDA 	TEMPA             		; Get A back from TEMPA
		PLP
		PHP             				; Restore flags

.OSBRK	JMP 	(brkv)           		; FFC8 - Pass on to BRKV
.OSSHUT JMP 	(shutv)          		; FFCB - Close file
.OSFIND JMP 	(findv)          		; FFCE - Open file
.OSBPUT JMP 	(bputv)          		; FFD1 - Put byte to file
.OSBGET JMP 	(bgetv)          		; FFD4 - Get byte from file
.OSSTAR JMP 	(starv)          		; FFD7 - Set file arguments
.OSRDAR JMP 	(rdarv)          		; FFDA - Read file arguments
.OSSAVE JMP 	(savev)          		; FFDD - Save file
.OSLOAD JMP 	(loadv)          		; FFE0 - Load file
.OSRDCH JMP 	(rdchv)          		; FFE3 - Read character from input
.OSECHO JSR 	OSRDCH           		; FFE6 - Read character and echo to output
.OSASCI CMP 	#&0D
		BNE 	OSWRCH  		 		; FFE9 - Write ASCII character
.OSCRLF LDA 	#&0A
		JSR 	OSWRCH  		 		; FFED - Write newline sequence
.OSWRCR LDA 	#&0D             		; FFF2 - Write carriage return
.OSWRCH JMP 	(wrchv)          		; FFF4 - Write character to output
.OS_CLI JMP 	(cliv)           		; FFF7 - Command line interpreter

.NMIV	EQUW 	NMIROUT           		; FFFA - NMI vector
.RSTV	EQUW 	RESET           		; FFFC - RESET vector
.IRQV	EQUW 	IRQBRK           		; FFFE - IRQ/BRK vector
.end

if (ISROM)
  if(SYS40)
    save "sys5-1770-40.rom",start,end,entry
  else
    save "sys5-1770-80.rom",start,end,entry
  endif
endif

IF INLINE_PRINT<>&F009: print "***WARNING: LF009 (INLINE_PRINT) fixed point moved now=",~INLINE_PRINT,(INLINE_PRINT-&F009) :endif
IF SKIPSPACE<>&F0E1:Print "***WARNING: LF0E1 (SKIPSPACE) fixed point moved now=",~SKIPSPACE,(SKIPSPACE-&F0E1) :endif
IF LF636<>&F636:print "***WARNING: LF636 fixed point moved now=",~LF636,(LF636-&F636) :endif
IF LFD53<>&FD53:print "***WARNING: LFD53 fixed point moved now=",~LFD53,(LFD53-&FD53) :endif
IF LFD7E<>&FD7E:print "***WARNING: LFD7E fixed point moved now=",~LFD7E,(LFD7E-&FD7E) :endif


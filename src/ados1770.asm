; Ported to BeebASM 2015-06-22 Phill Harvey-Smith.
; Ported to use WD1770, June-July 2015, Phill Harvey-Smith.
;
; Added *#40, *#80, *FORMAT and *VERIFY adapted from from GDOS
; Added *INFALL adapted from SDDOS
;
; 2018-02-06, Phill Harvey-Smith.
;	removed commented dead code.
;	Replaced all JSR followed immediately with RTS with a JMP to the subroutine, to let
;	it's RTS return to caller.
;	Combined common parts of READSEC and WRITESEC, saving 19 bytes.
;	Combined STEP_IN and RESTORE0 saving a further 7 bytes
; 	Streamlined error handling saving a further 16 bytes.
;

if(ISROM)
	include "..\src\atomdefs.asm"
	include "..\src\wdfdc.asm"
endif

print "IOBASE=",~IOBASE

USEAUTO		= 0

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
PDRQ		= $80						; DRQ

DRVSID_MASK	= PDDEN + PRESET + P4080	; Mask to zero drive & side bits

; MAIN ENTRY point

        ORG     $E000                   
        GUARD   $F000                   

; Start of code
;

;============================================
; Initialise DOS controller
;--------------------------------------------

		LDA		#PRESET					; Switch off motors, deselect drives, MR high
		STA		PCTRL

        JSR     MESSAGE             	; display signon message    

		EQUB	"ADOS-1770 "                  
        NOP     
		
		LDA		#>IOBASE				; Print IOBASE address
		JSR		PRINT_HEXA
		LDA		#<IOBASE
		JSR		PRINT_HEXA
		
		LDA		#':'					; Seperator
        JSR     OSWRCH

		LDA		#>WKBASE				; Print Workspace address
		JSR		PRINT_HEXA
		LDA		#<WKBASE
		JSR		PRINT_HEXA
	
        JSR     MESSAGE             	; display signon message    
		EQUB	CR,LF,LF 
		NOP
		
        LDA     #0						; FDC reselt line low           
        STA     PCTRL                     
        LDX     #3        				; Delay a short while for reset              
        JSR     DELAY                   
        LDA     #PRESET	+ PDDEN			; Reset high, single density              
        STA     PCTRL

        jmp     LEEE2                   ; Interpreter entry

;============================================
; Print DISK on screen
;--------------------------------------------

;.LE00D: jsr     INLINE_PRINT             ; Print "DISK "
;        EQUB    "DISK "                 
;        nop                             

;============================================
; Print text routine, end with char code >$7F or $00
; Zeropage $EA,$EB = txtpointer
;--------------------------------------------

.MESSAGE
.INLINE_PRINT  
;		jmp		SYS_INLINE_PRINT		; Use sysrom version
		pla                             ; Get textpointer from return addresss
        sta     TEXTPTR                     
        pla                             
        sta     TEXTPTR+1                 

        ldy     #$00                    ; zero offset
.LE01E: inc     TEXTPTR					; increment LSB of ptr                    
        bne     LE024                   ; no overflow, skip on
        inc     TEXTPTR+1				; increment MSB of ptr
                     
.LE024: lda     (TEXTPTR),Y				; get character to print                 
        bmi     LE030                   ; end of string, yep exit
        beq     LE030                   ; null byte : exit
        jsr     OSWRCH               	; print it
        jmp     LE01E                   ; loop for next
		
.LE030: jmp     (TEXTPTR)               ; Return from call

;============================================
; Read filename into $100 and check end of command
; Zeropage $E9 = command pointer
;--------------------------------------------

.LE033: sty     INBUFIDX                ; Save commandpointer
        jsr     READFNAME               ; Read filename
        jsr     CHECK_FNPARAM           ; Check end of command
        ldy     INBUFIDX                ; Load commandpointer
        ldx     #$00                    ; Store filename at $100
        beq     LE043                   ; Jump always

;============================================
; Read filename between "" or until space
; Zeropage $9A,$9B = filenamepointer
;--------------------------------------------

.READFNAME
.LE041: ldx     #$40                    ; Pointer at $140

.LE043: jsr     SKIPSPACE               ; Skip spaces
        stx     FILENAMEPTR                     
        cmp     #DUBQUOTE               ; Is character double quote?
        beq     LE08F                   ; Yep : read filename between ""
		
.LE04C: cmp     #CR                     ; Check end of line
        beq     LE05C 					; yep :
                  
        sta     STACKPAGE,X				; copy sting from STACKPAGE,y -> STACKPAGE,x                  
        inx                             
        iny                             
        lda     STACKPAGE,Y                  
        cmp     #SPACE                  ; Space, end reading
        bne     LE04C                   

.LE05C: lda     #CR                     ; End filename with $0D
        sta     STACKPAGE,X                  

        lda     #>STACKPAGE             ; Filenamepointer=$140
        sta     FILENAMEPTR+1                     
        ldx     #$9A                    
.LE067: rts                             

;============================================
; Copy filedata to $9A-$A3
; X = pointer to filedata ($9A=DOS, $52=COS)
;--------------------------------------------

.copy_check: 
		ldy     #$00					; Zero count / dest ptr                    
.LE06A: lda     $00,X      				; get a byte from source             
        sta     FILENAMEPTR,Y          	; move to dest         
        inx                    			; advance pointers / count         
        iny                             
        cpy     #$0A    				; done all ?                
        bcc     LE06A        			; nope loop again.           

;============================================
; Fill temp filename $A5-$AB with spaces
;--------------------------------------------

.LE075: lda     #SPACE        			; Char to fill : space            
        ldy     #$06         			; count           
.LE079: sta     FILENAME,Y      		; put it in             
        dey                    			; decrement count         
        bpl     LE079          			; loop if more         

;============================================
; Check if len filename<8 copy to $A5
;--------------------------------------------
.LE07F: iny                             
        lda     (FILENAMEPTR),Y                 
        cmp     #$0D                    
        beq     LE067                   ; Name is legal
        cpy     #$07                    
        bcs     LE0AB                   ; NAME? error
        sta     FILENAME,Y                   
        bne     LE07F                   

;============================================
; Store filename between "" at $100
;--------------------------------------------

.LE08F: iny                             ; increment pointer
        lda     STACKPAGE,Y             ; get character
        cmp     #CR                   	; EOL ? 
        beq     LE0AB                   ; yep : error
		
        sta     STACKPAGE,X             ; save in dest
        inx                             ; increment dest pointer
        cmp     #DUBQUOTE               ; is it a double quote ?
        bne     LE08F                   
        dex                   			; change pointers          
        iny                             
        lda     STACKPAGE,Y                  
        cmp     #DUBQUOTE               ; is it a double quote ?
        bne     LE05C                   
		
        inx                             ; inc ptr
        bcs     LE08F                   ; loop again

;============================================
; Print NAME? error
;--------------------------------------------

.LE0AB: jsr     INLINE_PRINT            ; NAME? error
        EQUB    "NAME?"                 
		brk                             

;============================================
; Read loadadres from $100,Y into $9C,$9D
;--------------------------------------------

.get_loadadr: ldx     #LOADADDR         ; Loadadres

;============================================
; Convert adres from $100,Y to bytes at X
;--------------------------------------------
; Exits with a=0 if no address given

.addr_to_x: 
		lda     #$00                    
        sta     $00,X                   ; Clear adres
        sta     $01,X                   
        sta     $02,X                   ; Clear adres given flag

        jsr     SKIPSPACE               ; Skip spaces

.LE0C1: lda     STACKPAGE,Y             ; Check if hex digit

        cmp     #'0'                    ; Digit between 0-9?
        bcc     LE0E9                   
        cmp     #':'                    
        bcc     LE0D4                   

        sbc     #$07                    ; Digit between $A-$F?
        bcc     LE0E9                   
        cmp     #$40                    
        bcs     LE0E9                   

.LE0D4: asl     A                       ; Move it to high nibble
        asl     A                       
        asl     A                       
        asl     A                       

        sty     $02,X                   ; Save address position

        ldy     #$04                    ; shift nibble into X and X+1
.LE0DC: asl     A                       
        rol     $00,X                   ; 16 bit shift through x and x+1
        rol     $01,X                   
        dey                             
        bne     LE0DC                   

        ldy     $02,X                   ; 4 Chars read?
        iny                             
        bne     LE0C1                   

.LE0E9: lda 	$02,X             		; A=address position
        rts                             

;============================================
; Print space
;--------------------------------------------

.PRINTSPACE: 
		lda     #SPACE                    
        jmp     OSWRCH               

;============================================
; Print 6 spaces (entry PRINTSPACE6)
; Print Y speces (entry PRINTSPACEY)
;--------------------------------------------

.PRINTSPACE6: 
		ldy     #$06                    

.PRINTSPACEY: 
		jsr     PRINTSPACE                   ; Print space
        dey                             
        bne     PRINTSPACEY                   
        rts                             

;============================================
; Shift right routine
;  divide by 32 (entry LE0FA)
;  divide by 16 (entry LE0FB)
;--------------------------------------------

.diva32: 
		lsr     A                       

.diva16:
		lsr     A                       
        lsr     A                       
        lsr     A                       
        lsr     A                       
        rts                             

;============================================
; INY routine
;  8x iny (entry LE100)
;  7x iny (entry LE101)
;  4x iny (entry LE104)
;--------------------------------------------

.incy8: iny                             

.incy7: iny                             
        iny                             
        iny                             

.incy4: iny                             
        iny                             
        iny                             
        iny                             
        rts                             

;============================================
; DEY routine
;  8x dey (entry decy8)
;--------------------------------------------

.decy8: dey                             
        dey                             
        dey                             
        dey                             
        dey                             
        dey                             
        dey                             
        dey                             
        rts                             

;============================================
; Calculate startaddres, nr of tracks & sectors
;--------------------------------------------

.calc_start_addr: 
		lda     LOADADDR				; Get load address                     
        sta     STARTADDR            	; save in start addr         
        lda     LOADADDR+1              ; do MSB       
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

.LE13A: sec                             ; Set carry
.LE13B: inc     TRACKNO                 ; Add one to track no    
        sbc     #SECTRACK				; Decrement sector count by sectors / track                    
        bcs     LE13B                   ; still more keep going

        dex                             ; if x > 0 then we have more than 256 sectors
        bpl     LE13A                   ; so loop again
        adc     #SECTRACK				; compensate for over loop, calculate sector on track  
        sta     SECTORNO                ; save it
.LE148: rts                             

;============================================
; Check end of command
;--------------------------------------------

.GET_CHK_FNAME: jsr     CHECK_FNPARAM                   

;============================================
; Check if filenaam legal and in cat
;--------------------------------------------

.LE14C: jsr     copy_check              ; Filename legal?
        jsr     FILEINCAT               ; Filename in cat?
        bcs     LE148                   

		jmp		FILE_ERROR				; File not found : error
		
;============================================
; Check if filenaam in cat, carry set is yes
;--------------------------------------------

.FILEINCAT
.LE15D: 
		jsr		load_cat				; load catalog, if needed
		
        ldy     #$F8                    ; Init nameptr
.LE162: jsr     incy8                   ; INY 8x, move to next entry
        cpy     WKFileCount         	; FilePointer
        bcs     LE18A         
          
        lda     WKFileDir1,Y        	; Qualifier from catalog
        and     #$7F                    ; Mask out protectionbit
        cmp     USEQUAL                   ; Same qual as we are looking for ?
        bne     LE162                   ; nope : skip to next file

        jsr     incy7                   ; INY 7x

        ldx     #$06                    ; Check filename in cat
.LE178: lda     WKTitle + $0007,Y   	; scan title              
        cmp     FILENAME,X                   
        bne     LE184                   
        dey                             
        dex                             
        bpl     LE178                   
        rts                             

.LE184: dey                             ; Next file in cat
        dex                             
        bpl     LE184                   
        bmi     LE162                   

.LE18A: clc                             ; Filename not found
        rts                             

;
; Remove catalog entry at y, shuffle all following entries back to  overwrite it
; exits with y pointing to free entry
;

.rem_cat_entry: 
		lda     WKFileDir1,Y        	; Check file protection
        bmi     LE1AA                   ; Protected

.LE191: lda     WKFileName2,Y        	; Remove filename from cat
        sta     WKFileName1,Y                 
        lda     WKLoadAddr2,Y        	; Remove filedata
        sta     WKLoadAddr1,Y                 
        iny                             
        cpy     WKFileCount	         	; last file?
        bcc     LE191                   

        tya                             ; Decrement FilePointer
        sbc     #$08                    
        sta     WKFileCount				; update filecount                   
        rts                             

;============================================
; Print PROT error
;--------------------------------------------

.LE1AA: jsr     INLINE_PRINT                   
        EQUB    "PROT"                  
		brk                             

;============================================
;INFO COMMAND
;
;   Q #FILENAA 3000 3000 01000 01A
;--------------------------------------------
.infocom:                               

.LE1B2  jsr     GET_CHK_FNAME           ; Check filename in cat
        jsr     print_info              ; Print filenaam, start,link,lengte,sector
        jmp     set_qual_to_use         ; Set qual back if changed by USE

;
; Print file info if mon is true.
; On entry Y points to the catalog entry to print
;
.print_info_ifmon
		lda     MONFLAG                 ; Check MON flag
        bne     LE230  					; nomon: exit                 

;
; Print info, ignore mon
; On entry Y points to the catalog entry to print
;
.print_info: 
		lda     WKFileDir1,Y        	; Load Qual
        and     #$7F                    ; Mask protectionbit
        jsr     OSWRCH                  ; Print Qual
        jsr     PRINTSPACE              ; Print space

        ldx     WKFileDir1,Y        	; Load protectionbit
        bpl     LE1D1                   ; Not protected, print ' '
        lda     #'#'                    ; Protected, print #
.LE1D1: jsr     OSWRCH               	; Print karakter

        ldx     #$07                    ; Print filenaam
.LE1D6: lda     WKFileName1,Y                 
        jsr     OSWRCH               
        iny                             
        dex                             
        bne     LE1D6                   

.LE1E0: jsr     PRINTSPACE              ; Print space

        lda     WKLoadAddr1 - 6,Y        ; Print hex adres
        jsr     PRINT_HEXA                   
        lda     WKLoadAddr1 - 7,Y                 
        jsr     PRINT_HEXA                   
        iny                             
        inx                             
        iny                             
        cpx     #$02                    ; Repeat 3 times
        bcc     LE1E0                   

        jsr     PRINTSPACE              ; Print space
        jsr     PRINTSPACE              ; Print space

        lda     WKBASE + $0103,Y        ; Check file>64Kb
        jsr     diva16                  ; Divide by 16
        jsr     PRINT_HEXA_LOWN              ; Print hex filelen
        lda     WKBASE + $0102,Y                 
        jsr     PRINT_HEXA                   
        lda     WKBASE + $0101,Y                 
        jsr     PRINT_HEXA                   

        jsr     PRINTSPACE              ; Print space

        lda     WKBASE + $0103,Y        ; Print hex sector
        jsr     PRINT_HEXA_LOWN                   
        lda     WKBASE + $0104,Y                 
        jsr     PRINT_HEXA                   

        jmp     OSCRLF                 

.infallcom 
		JSR 	load_cat          		; Read catalog into WKBASE
        LDY 	#0						; start at first file
        STY 	FILENAMEPTR
                
.infall1:
		JSR 	print_info          	; Print file info
        LDY 	FILENAMEPTR
        JSR 	incy8              		; Pointer to next file
        STY 	FILENAMEPTR
        CPY 	WKFileCount        		; Check last file?
        BNE 	infall1
.LE230

; WAIT_NOT_BUSY is defined here so that external programs such as compact that are linked
; into this rom have a place to JSR to, in the 1770 implementation, in the 8271 this 
; actually does a disk wait, for the 1770 it's just an RTS
.WAIT_NOT_BUSY
		RTS

;============================================
; DIR command
;--------------------------------------------
.dircom:                                

.LE231: jsr     drivecom             	; Check Drivenr given?
		jmp		load_cat
		
;============================================
; CAT command
;--------------------------------------------
.catcom:                                

.LE237  jsr     dircom                  ; DIR

.catcom2
	    ldx     #$00                    
        stx     $B6                     
.LE23E: lda     WKTitle,X        		; Print title
        cpx     #$08                    
        bcc     LE248                   

        lda     WKTitle2 - 8,X          ; -8 because x = 8,9,10,11 at this point...       

.LE248: jsr     OSWRCH               	; Print character
        inx                             
        cpx     #$0D                    ; End of title?
        bne     LE23E                   

        jsr     INLINE_PRINT            ; Print " DRIVE"
        EQUB    " DRIVE "               
        lda     DRIVENO                 ; Print drivenr
        jsr     PRINT_HEXA_LOWN              ; Convert drive no to ASCII & print 

        jsr     INLINE_PRINT            ; Print " QUAL "
        EQUB    " QUAL "                
        lda     USEQUAL                 ; Print qual
        jsr     OSWRCH               

.LE26D: ldy     #$00                    
        jsr     LE288                   ; Check last filename
        bcc     LE2BE                   

.LE274: jsr     decy8                   ; DEY 8x
        lda     WKFileName1,Y        	; Mask protectionbit
        and     #$7F                    
        sta     WKFileName1,Y                 
        tya                             
        bne     LE274                   
        jmp     OSCRLF                 

.LE285: jsr     incy8                   ; INY 8x

.LE288: cpy     WKFileCount          	; Check last filename?
        bcs     LE292                   
        lda     WKFileName1,Y        	; Check protectionbit
        bmi     LE285                   
.LE292: rts                             

;--------------------------------------------
.LE293: ldy     CATEOLFLAG              ; Check EOL flag
        beq     LE29C                   ; no EOL yet, skip
        jsr     OSCRLF                  ; print EOL

        ldy     #$FF                    ; RE-init EOL flag
.LE29C: iny                             ; 
        sty     CATEOLFLAG              ; save EOL flag       
        jsr     PRINTSPACE6                   ; Print 6 spaces

.LE2A2: lda     #'#'                    ; Assume file locked 
        ldy     CATINDEX                ; Get index
        ldx     WKFileDir1,Y        	; Check if file locked
        bmi     LE2AD                   ; Locked
        lda     #' '                    ; Show file not locked
.LE2AD: jsr     OSWRCH                  ; Print locked character

        ldx     #$00                    ; Reset filenamepointer
.LE2B2: lda     CATFILENAME,X           ; Get character filename
        jsr     OSWRCH                  ; Print character
        inx                             
        cpx     #$07                    ; Check end of filename
        bne     LE2B2                   ; Nope: look for next char
        beq     LE26D                   ; Last character print

;--------------------------------------------
.LE2BE: sty     CATINDEX                ; Save catalog index
        ldx     #$00                    ; A2 00
.LE2C2: lda     WKFileName1,Y        	; B9 08 20
        and     #$7F                    ; 29 7F
        sta     CATFILENAME,X           ; 95 AE
        iny                             ; C8
        inx                             ; E8
        cpx     #$08                    ; E0 08
        bne     LE2C2                   ; D0 F3
.LE2CF: jsr     LE288                   ; 20 88 E2
        bcs     LE2F1                   ; B0 1D
        ldx     #$06                    ; A2 06
        sec                             ; 38
.LE2D7: lda     WKBASE + $000E,Y        ; B9 0E 20
        sbc     CATFILENAME,X           ; F5 AE
        dey                             ; 88
        dex                             ; CA
        bpl     LE2D7                   ; 10 F7
        jsr     incy7                   ; 20 01 E1
        lda     WKBASE + $000F,Y        ; B9 0F 20
        and     #$7F                    ; 29 7F
        sbc     $B5                     ; E5 B5
        bcc     LE2BE                   ; 90 D2
        jsr     incy8                   ; 20 00 E1
        bcs     LE2CF                   ; B0 DE
.LE2F1: ldy     CATINDEX                   ; A4 B7
        lda     WKBASE + $0008,Y        ; B9 08 20
        ora     #$80                    ; 09 80
        sta     WKBASE + $0008,Y        ; 99 08 20
        lda     $B5                     ; A5 B5
        cmp     $B6                     ; C5 B6
        beq     LE293                   ; F0 92
        sta     $B6                     ; 85 B6
        jsr     OSCRLF                 ; 20 ED FF
        lda     $B5                     ; A5 B5
        jsr     OSWRCH               ; 20 F4 FF
        lda     #$3A                    ; A9 3A
        jsr     OSWRCH               ; 20 F4 FF
        ldy     #$04                    ; A0 04
        jsr     PRINTSPACEY                   ; 20 F3 E0
        sty     CATEOLFLAG                     ; 84 B8
        beq     LE2A2                   ; F0 89

.LE319: lda     WKBASE + $010E,Y        ; B9 0E 21
        jsr     diva16                   ; 20 FB E0
        sta     STARTSEC                     ; 85 A2
        clc                             ; 18
        lda     #$FF                    ; A9 FF
        adc     WKLength1,Y        		; 79 0C 21
        lda     WKStartSec1+1,Y        ; B9 0F 21
        adc     WKLength1+1,Y        ; 79 0D 21
        sta     STARTSEC+1                   ; 85 A3
        lda     WKStartSec1,Y        ; B9 0E 21
        and     #$0F                    ; 29 0F
        adc     STARTSEC                   ; 65 A2
        sta     STARTSEC                   ; 85 A2

.LE338: sec                             ; Entry save command
        lda     WKSecCOpt+1,Y        ; B9 07 21
        sbc     STARTSEC+1                   ; E5 A3
        pha                             ; 48
        lda     WKSecCOpt,Y        ; B9 06 21
        and     #$0F                    ; 29 0F
        sbc     STARTSEC                   ; E5 A2
        tax                             ; AA

        lda     #$00                    ; A9 00
        cmp     FILESIZE                   ; C5 A0
        pla                             ; 68
        sbc     FILESIZE+1                   ; E5 A1
        txa                             ; 8A
        sbc     #$00                    ; E9 00
        rts                             ; 60

;============================================
; Used by *EXEC and *SPOOL
;--------------------------------------------

.LE352: ldx     #$02                    ; *EXEC so modify read character vector
        bne     LE358                   ; D0 02
		
.LE356: ldx     #$00                    ; *SPOOL so modify write character vector

.LE358: sty     EXESPOOLIDX             ; Save index

.LE35A: ldy     WRCVEC,X                ; Swap WRCVEC/RDCVEC and saved vector
        lda     SAVWRCVEC,X             
        sty     SAVWRCVEC,X             
        sta     WRCVEC,X                
        inx                             ; Increment counter
        txa                             ; Get it in A
        lsr     A                       ; Shift bit 0 into carry
        bcs     LE35A                   ; Set, still a byte to go
        ldy     EXESPOOLIDX             ; Restore index
        rts                             

;============================================
; Command table
;--------------------------------------------

.COMMAND_TABLE: 
		EQUB    "CAT",    >catcom,<catcom
        EQUB    "DIR",    >dircom,<dircom
        EQUB    "INFO",   >infocom,<infocom
        EQUB    "LOAD",   >loadcom,<loadcom
        EQUB    "SAVE",   >savecom,<savecom
        EQUB    "DELETE", >deletecom,<deletecom
        EQUB    "RUN",    >runcom,<runcom
        EQUB    "LOCK",   >lockcom,<lockcom
        EQUB    "UNLOCK", >unlockcom,<unlockcom
        EQUB    "MON",    >moncom,<moncom
        EQUB    "NOMON",  >nomoncom,<nomoncom
        EQUB    "SET",    >setcom,<setcom
        EQUB    "DRIVE",  >drivecom,<drivecom
        EQUB    "TITLE",  >titlecom,<titlecom
        EQUB    "USE",    >usecom,<usecom
        EQUB    "EXEC",   >execcom,<execcom
        EQUB    "SHUT",   >shutcom,<shutcom
        EQUB    "GO",     >gocom,<gocom 
        EQUB    "SPOOL",  >spoolcom,<spoolcom
		EQUB	"FORMAT", >formatcom,<formatcom
		EQUB	"#40",	  >fortycom, <fortycom
		EQUB	"#80",	  >eightycom, <eightycom
		EQUB	"VERIFY", >verifycom, <verifycom
		EQUB	"INFALL", >infallcom, <infallcom
		
		EQUB    >exec,<exec             ; *filename entry

;============================================
; Command interpreter
;--------------------------------------------
;
; Command table consists of a series of entries, which are :
; n ASCII characters of command name followed by
; MSB of exec address, LSB of exec address.
; This allows the interpreter to detect the end of the command
; string when scanning by looking at the negative flag.
; NOTE this only works if the handler is at an address > $8000.
;
; String to scan for commands is held at the bottom of the stack
; page : $100 onwards.
;
.NEWCOMVEC
.LE3E5  ldx     #$FF                    ; 
        cld                             ; Binary mode
.LE3E8: ldy     #$00                    ; Init pointer to commandline
        jsr     SKIPSPACE               ; Skip spaces
        dey                             ; as beggining of loop does iny.....
		
.LE3EE: iny                             ; Move to next char
        inx                             ; Move to next keyword char
		
.LE3F0: lda     COMMAND_TABLE,X         ; get next command word character
        bmi     LE40D                   ; end of command, yep exec it.
		
        cmp     STACKPAGE,Y             ; Character the same as in buffer
        beq     LE3EE                   ; yep : keep scanning
        dex                             ; nope
		
.LE3FB: inx                             ; get next character from command
        lda     COMMAND_TABLE,X                 ; is it <$80, so ascii?
        bpl     LE3FB                   ; yes, not end of command string
        inx                             ; move to msb od exec
		
        lda     STACKPAGE,Y             ; Get next character from input buffer
        cmp     #'.'                    ; Is it a period?
        bne     LE3E8                   ; Nope, start again with next keyword
		
        iny                             ; move past found word in buffer
        dex                             ; point to exec address
        bcs     LE3F0                   ; B0 E3
		
.LE40D: sta     FILENAMEPTR+1           ; Save exec address in indirect
        lda     COMMAND_TABLE+1,X               
        sta     FILENAMEPTR             
        clc                             ; enter routine with x=0, carry clear
        ldx     #$00                    
        jmp     (FILENAMEPTR)           ; jump to exec routine

;============================================
; DELETE command
;--------------------------------------------
.deletecom:                             

.LE41A  jsr     GET_CHK_FNAME           ; Filename in cat
        sty     FILENAMEPTR             ; Save pointer to name
        jsr     print_info_ifmon        ; Print info
        ldy     FILENAMEPTR             ; Restore name ptr
        jsr     rem_cat_entry           ; Delete name in cat
        jmp     write_cat_resqual       ; Save cat & restore qualifier

;============================================
; DRIVE command
;--------------------------------------------
.drivecom:                              

.LE42A: jsr     SKIPSPACE               ; Skip spaces
        cmp     #CR                   	; EOL?
        beq     LE44E                   ; Yep : No drivenr, do default drive
        iny                             ; Move to next char
        pha                             ; Save Drivenr
        jsr     CHECK_PARAM             ; Check end of command
        pla                             ; Restore drivenr
        cmp     #'0'                    ; Drivenr >=1?
        bcc     LE44F                   ; No = Error
        cmp     #'4'                    ; Drivenr <=4?
        bcs     LE44F                   ; No = Error

.readcat
.LE43F: and     #$03                    ; Convert ascii no to binary
        eor     DRIVENO                 ; Mask against current drive, will be $80 or $00 if so
        cmp     #$80                    ; Catalog read ?
        beq     LE44E                   ; Current drive = drivenr & cat in memory
		
        eor     DRIVENO                 ; Store new drivenr (will undo above eor!)
        sta     DRIVENO                 
.LE44E: rts                             

.LE44F: jsr     INLINE_PRINT            ; Print error : exit
        EQUB    "DRIVE?"                
		brk                             

;============================================
;NOMON COMMAND
;--------------------------------------------
.nomoncom:                              

.LE459  ldx     #$FF                    ; Flag mon off

;============================================
;MON COMMAND
;--------------------------------------------
.moncom:                                

.LE45B  jsr     CHECK_PARAM             ; Check end of command
        stx     MONFLAG                 ; Set mon flag

.set_qual_to_use: 
		lda     SETQUAL                 ; Get SET qualifier
        sta     USEQUAL                 ; Set USE qualifier
        rts                             

;============================================
;LOAD COMMAND
;--------------------------------------------
.loadcom:                               

.LE465: jsr     READFNAME               ; Read filename
        jsr     get_loadadr             ; Read adres into LOADADDR
        beq     LE471                   ; none given skip
        lda     #$FF                    ; flag LOADADDR valid

.LE46F: sta     EXECADDR                ; 
.LE471: ldx     #FILENAMEPTR            ; 
        clc                             ; 18
        jmp     (LODVEC)                ; LODVEC

;============================================
;LODVEC SUB
;--------------------------------------------

.NEW_LODVEC  
		php                        		; Save flags
        jsr     LE484                   ; 20 84 E4
.LE47B: plp                             ; Restore flags
        bcc     LE481                   ; 
.LE481: jmp     set_qual_to_use         ; Reset qual

;============================================

.LE484: jsr     LE14C                   ; Check if filename legal and in cat
.LE487: sty     FILENAMEPTR             ; Save filename ptr
        ldx     #$00                    ; 
        lda     EXECADDR                ; Get LOADADDR vald flag
        bpl     LE493                   ; Skip no LOADADDR given

        ldx     #$02                    ; Skip past load address in catalog
        iny                             
        iny                             

.LE493: lda     WKBASE + $0108,Y        ; Move catalog file data into buffer
        sta     LOADADDR,X              ; at LOADADDR
        iny                             
        inx                             
        cpx     #$08                    ; Done all ?
        bne     LE493                   ; nope : loop again
		
        ldy     FILENAMEPTR             ; recover filename ptr
        jsr     print_info_ifmon        ; Print file info (if MON).

.ROM_READ_SECTORS: 
		lda		#WCMD_READ_SEC			; Flag reading
		
; Code between common_rw - E4C4 shared by LODVEC and SAVVEC	
.common_rw: 
		sta     FDCSAVCMD               ; Save command executing

        jsr     calc_start_addr         ; Calculate start address, number of sectors etc
.LE4AD: jsr     RECALC_TRACK_SEC                   ; Recalculate track and sector
        beq     LE4C4		            ; All done : exit
		
.LE4B2: jsr     set_memptr              ; Set MEMPTR from STARTADDR, place to load at

		jsr		SEEK					; Seek to track

        lda     FDCSAVCMD               ; Get FDC command to execute
        cmp		#WCMD_WRITE_SEC			; Write sectors ?
		
		bne		do_read					; nope : do read
		
		jsr		WRITESEC				; Write sectors
		jmp		do_done
		
.do_read
		jsr		READSEC					; Read sectors
.do_done		
		bne     LE4B2                   ; no error : do next block
		
        jsr     set_startaddr           ; Update STARTADDR from current MEMPTR, inc TRACKNO
        bne     LE4AD                   ; More to do loop again
		
.LE4C4: rts                             

;============================================
;CHECK IF FILE EXISTS, IF SO LOAD AND RUN
;--------------------------------------------
; End of command
.exec:                                  

.LE4C5: jsr     LE033                   ; Read filename

        lda     DRIVENO                 ; Save current drive
        sta     SAVDRIVENO              
        lda     USEQUAL                 ; Save current qualifier
        sta     SAVQUAL                  
        lda     #SPACE                  ; Set current qualifier to space
        sta     USEQUAL                   
        lda     #$00                    ; 
        sta     EXECADDR                 
        jsr     readcat                 ; Read catalog
        ldx     #$9A                     
        jsr     copy_check                   ; Filename legal?
        jsr     FILEINCAT               ; Filename in cat?
        bcs     LE4F4                   ; Yes : load and exec
		
        jsr     LE500                   ; Restore drive & qualifier
        jsr     INLINE_PRINT            ; Print error and exit
        EQUB    "COMMAND?"              
		brk                             

.LE4F4: jsr     LE487                   ; 
        jsr     LE500                   ; Restore drive & qualifier
        jmp     (EXECADDR)              ; Link file

;--------------------------------------------

.LE500: lda     SAVDRIVENO              ; Get saved drive no
        jsr     readcat                 ; Re-read it's catalog
        lda     SAVQUAL                 ; Restore saved qualifier
        sta     USEQUAL                   
        rts                             

;============================================
;RUN COMMAND
;--------------------------------------------
.runcom:                                

.LE50A: jsr     LE033                   ; Check filename + string
        jsr     LE46F                   ; Read addresses
.LE510: 
        jmp     (EXECADDR)              ; Execute code

;============================================
; Print FILE? error
;--------------------------------------------

.FILE_ERROR: 
		jsr     INLINE_PRINT                   
        EQUB    "FILE?"                 
		brk                             

;============================================
;EXEC COMMAND
;--------------------------------------------
.execcom:                               

.LE519: jsr     CHECK_FNPARAM           ; Check end of command
        jsr     OSFIND                  ; Find / open the file
        tay                             ; 
        beq     FILE_ERROR              ; Not found : exit
		
        sta     EXECHANDLE              ; Save filehandle
        jsr     LE352                   ; Save current RDCHV
        lda     #<LE533                 ; Redirect rdchv to fetch from Exec channel
        ldy     #>LE533                 

.LE52B: sta     COMVEC,X                ; Change command vector
        tya                             
        sta     COMVEC+1,X              
        rts                             

.LE533  sty     INBUFIDX                ; Save input buffer pointer
        ldy     EXECHANDLE              ; Get filehandle
        jsr     OSBGET                  ; Get a byte
		bcc     LE544                   ; Not EOF, return
        
		jsr     OSSHUT                  ; Shut the file
        ldy     INBUFIDX                ; Restore input buffer pointer
        jmp     (RDCVEC)                ; Exit to readvector

.LE544: ldy     INBUFIDX                ; Restore input buffer pointer
        rts                             

;============================================
;SPOOL COMMAND
;--------------------------------------------
.spoolcom:                              

        jsr     CHECK_FNPARAM           ; Check end of command
        clc                             
        jsr     OSFIND                  ; Find / open the file

.LE54E  sta     SPOOLHANDLE             ; Save filehandle
        jsr     LE356                   ; 
        
		lda     #<LE559                 ; Point to handler
        ldy     #>LE559                 
        
		bne     LE52B                   ; no error : go do it
		
.LE559  sty     INBUFIDX                ; Save input buffer index
        ldy     SPOOLHANDLE             ; Get handle for spool file
        jsr     OSBPUT                  ; Put the byte to the file
.LE560  ldy     INBUFIDX                ; Restore buffer index
        jmp     (SAVWRCVEC)             ; Jump to saved vector

;============================================
;GO COMMAND
;--------------------------------------------
.gocom:                                 

.LE565  jsr     get_loadadr             ; Read hex adres in #9C/9D
        php                             ; 
        jsr     CHECK_PARAM             ; Check end of command
        plp                             ; 
        beq     LE510                   ; No address given, use last exec address.
        jmp     (LOADADDR)              ; Jump adres

;============================================
;SET COMMAND
;--------------------------------------------
.setcom:                                

.LE572  jsr     LE5B3                   ; Read Qual
        sta     SETQUAL                 ; Store Qual
        rts                             

;============================================
;TITLE COMMAND
;--------------------------------------------
.titlecom:                              

.LE578  jsr     READFNAME               ; Read text
        jsr     dircom                  ; *DIR command
        ldx     #TITLELEN               ; Length of title
        lda     #SPACE                  ; Blank out old title 

.LE582: jsr     put_title_cat           ; Write char in cat
        dex                             ; decrement count
        bpl     LE582                   ; keep looing if not all done
		
.LE588: inx                             ; Move to next char of specified name
        lda     $0140,X                 ; Get a char from name
        cmp     #CR                     ; EOL?
        beq     write_cat_resqual                   ; Yep : write back to disk
		
        jsr     put_title_cat           ; Put next char in cat
        cpx     #TITLELEN               ; Done all?
        
		bcc     LE588                   ; Nope keep going
        bcs     write_cat_resqual       ; Yep : write back to disk

;============================================
; LOCK command
;--------------------------------------------
.lockcom:                               

.LE599  sec                             ; Flag lock

;============================================
; UNLOCK command
;--------------------------------------------
.unlockcom:                             

.LE59A  php                             ; Save flags
        jsr     GET_CHK_FNAME           ; 
        lda     USEQUAL                 ; Get qualifier 
        rol     A                       ; Shift top bit to carry
        plp                             ; restore flags (lock falg in carry)
        ror     A                       ; Shift carry back to qual byte
        sta     WKBASE + $000F,Y        ; put back in catalog
        jsr     print_info_ifmon              ; print file info

; Write catalog and restore qualifier.
.write_cat_resqual: 
		jsr     write_cat               ; Write catalog back to disk
        jmp     set_qual_to_use         ; use qual=set qual

;============================================
;USE COMMAND
;--------------------------------------------
.usecom:                                

.LE5AF  lda     USEQUAL                 ; Get current use qualifier
        sta     SETQUAL                 ; Set SET qualifer to it
.LE5B3: iny                              
        jsr     CHECK_PARAM             ; Check end of command
        lda     $00FF,Y                 ; Get qualifier from command line
        sta     USEQUAL                 ; Set USE qualifier to it.
        rts                             

;============================================
;Part of TITLE COMMAND
;--------------------------------------------
.put_title_cat
.LE5BD: cpx     #$08                    ; First 8 chars of title at beginning of sector
        bcc     LE5C5                   ; Yep chars 0..7 write at beginning
        sta     WKBASE + $00F8,X        ; Else write at end
        rts                             

.LE5C5: sta     WKBASE + $0000,X        ; 
.LE5C8: rts                             

;============================================
; Check end of command
;--------------------------------------------

.CHECK_FNPARAM
.LE5C9: jsr     READFNAME               ; Read filename

.CHECK_PARAM
.LE5CC: jsr     SKIPSPACE               ; Skip spaces
        cmp     #$0D                    ; End of command?
        beq     LE5C8                   ; RTS

.LE5D3: jsr     INLINE_PRINT            ; Print SYNTAX? error
        EQUB    "SYNTAX?"               
		brk                             

.DISK_FULL: jsr     INLINE_PRINT            ; FULL error
        EQUB    "FULL"                  
		brk                             

;============================================
; SAVE COMMAND
;--------------------------------------------
.savecom:                               

.LE5E6: jsr 	READFNAME               ; Read filename
        jsr     get_loadadr             ; Read startadres
        beq     LE5D3                   ; No startadres, error
        
		ldx     #STARTSEC               ; Get Endaddress
        jsr     addr_to_x               
        beq     LE5D3                   ; No endadrres, error
		
        ldx     #EXECADDR               ; Get EXEC address
        jsr     addr_to_x               
        php                             ; Save flags
        lda     LOADADDR                ; Get LOADADDR to x:a
        ldx     LOADADDR+1              ; 
        plp                             ; Restore flags
		
        bne     LE606                   ; No exec address given
        sta     EXECADDR                ; So set EXEC address to load address.
        stx     EXECADDR+1              
		
.LE606: sta     FILESIZE                ; Loadaddress here
        stx     FILESIZE+1              
        jsr     CHECK_PARAM             ; Check end of command
        ldx     #$9A                    ; Flag Disk.
        clc                             
        jmp     OSSAVE                  ; OSSAVE

;============================================
;SAVVEC SUB
;--------------------------------------------

.NEW_SAVVEC: 
		php                             ; Save flags
        jsr     LE6AD                   ; 20 AD E6
        jmp     LE47B                   ; 4C 7B E4

;============================================

.LE61A: jsr     copy_check              ; Copy filename and check legal
        jsr     FILEINCAT               ; Filename in cat?
        bcc     LE625                   ; no 
        jsr     rem_cat_entry           ; Yes, remove file from cat

.LE625: lda     FILESIZE              	; Save startadres
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

        lda     #$00                    ; Set STARTSEC = $200
        sta     STARTSEC                   
        lda     #$02                    
        sta     STARTSEC+1              

        ldy     WKBASE + $0105          ; 
        beq     LE67F                   ; Disk empty?
        cpy     #$F8                    ; Disk full?
        bcs     DISK_FULL                   ; 

        jsr     LE338                   ; Calculate ??
        jmp     LE655                   ; 4C 55 E6

.LE64F: jsr     decy8                   ; DEY 8x
        jsr     LE319                   ; 20 19 E3

.LE655: tya                             ; 98
        beq     LE65A                   ; F0 02
        bcc     LE64F                   ; 90 F5
		
.LE65A: bcs     LE667                   ; B0 0B
.NO_ROOM
        jsr     INLINE_PRINT            ; 20 16 E0
        EQUB    "NO ROOM"               
		brk                             

.LE667  sty     $00EA                   ; Move filedata 8 bytes up
        ldy     WKBASE + $0105          ; AC 05 21
.LE66C: cpy     $00EA                   ; C4 EA
        beq     LE67F                   ; F0 0F
        lda     WKBASE + $0007,Y        ; B9 07 20
        sta     WKBASE + $000F,Y        ; 99 0F 20
        lda     WKBASE + $0107,Y        ; B9 07 21
        sta     WKBASE + $010F,Y        ; 99 0F 21
        dey                             ; 88
        bcs     LE66C                   ; B0 ED

.LE67F: ldx     #$00                    ; Copy new filename in cat
.LE681: lda     FILENAME,X              ; B5 A5
        sta     WKBASE + $0008,Y        ; 99 08 20
        iny                             ; C8
        inx                             ; E8
        cpx     #$08                    ; E0 08
        bne     LE681                   ; D0 F5

.LE68C: lda     FILENAMEPTR+1,X                 ; B5 9B
        dey                             ; 88
        sta     WKBASE + $0108,Y        ; 99 08 21
        dex                             ; CA
        bne     LE68C                   ; D0 F7

        jsr     print_info_ifmon              ; Print fileinfo

        pla                             ; 68
        sta     LOADADDR+1                   ; 85 9D
        pla                             ; 68
        sta     LOADADDR                   ; 85 9C

        ldy     WKBASE + $0105          ; Update filecounter (+8)
        jsr     incy8                   
        sty     WKBASE + $0105          

        jmp     write_cat               ; Write catalog
		
.LE6AD: jsr     LE61A                   ; 20 1A E6
.ROM_WRITE_SECTORS: 
        LDA		#WCMD_WRITE_SEC			; Flag we are writing
		jmp     common_rw               ; Go do it


;--------------------------------------------
; Initialise drive for loading catalog
;--------------------------------------------
.init_fdc_lcat
.LE6FF: lda     #$00                    ; Zero track and sector numbers
        sta     TRACKNO                  
        sta     SECTORNO                 
        lda     #$02                    ; Catalog is 2 sectors
        sta     SECCOUNT                

		jsr		RESTORE0 				; Seek to track 0
		
		jmp		MEMPTR_WKBASE			; Point at workspace
		
;--------------------------------------------
; Load catalog
;--------------------------------------------

.load_cat
.LE731: jsr     TESTREADY               ; Check drive ready
        bne     LE749                   ; ready, return, don't need to re-read.

.load_cat_always		
        jsr     START_MOTOR_SELECT            ; Start drive motor and select drive
        jsr     init_fdc_lcat           ; Init FDC for cat load
		
		jsr		READSEC					; Read the sectors
.LE749: rts                             

; Write catalog
; Assumes drive is started and selected.
.write_cat: 
		jsr     START_MOTOR_SELECT            ; Start drive motor and select drive
        jsr     init_fdc_lcat           ; Init FDC for cat write

		jmp		WRITESEC				; Write the sectors
                                     

;--------------------------------------------
; Select drive / side                HARDWARE
;--------------------------------------------
;
; since the mapping of the control register is as follows :
; Bit	76543210	Meaning
;		-------1	Side select
;		------1-  	Drive select bit 0
;   	-----1--	Drive select bit 1
;		----1---	Double density enable (SD=1, DD=0)
;		---1----	Reset WD177x
;		--1-----	40(0)/80(1)
;		-1------	IP (1.40), INTRQ(1.45 and later).
;		1-------	DRQ from WD177x.
;
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

.TESTREADY:
		lda     DRIVENO                 ; Check if catalog must be read
        bpl     LE78B                   ; If ms bit set, then drive cat in memory

		lda		WSTATUS					; Read status reg
.LE78B:	and 	#WST_MOTOR_ON			; motor on ?

        rts                             

;--------------------------------------------
; Recalculate track and interrupt routine
; Recalculate track and sector interrupt routine
;--------------------------------------------

.RECALC_TRACK_SEC: 
		lda     #RETRIES                ; Reset retry count
        sta     RETRYCOUNT              
        sec                             
        lda     #SECTRACK               ; Sectors per track
        sbc     SECTORNO                
		
        ldy     SECC256FLAG             ; Get sector > 256 flag
        bne     LE829                   ; sector is > 256 
		
        cmp     SSECCOUNT               ; Saved Sector count 0 ?
        bcc     LE829                   ; 
		
        lda     SSECCOUNT               ; Get saved sector count
.LE829: sta     SECCOUNT                ; Restore it
        sec                             ; set carry
        lda     SSECCOUNT               ; Get Saved sector count
        sbc     SECCOUNT                ; Decrement by sector count sectors
        sta     SSECCOUNT               ; Re-save it
        bcs     LE836                   ; 
		
        dec     SECC256FLAG             ; Decrement > 256 
.LE836: lda     SECCOUNT                ; Get sector count
        rts                             

;--------------------------------------------
; Restore address interrupt routine
;--------------------------------------------

.set_startaddr: 
		lda		#FIRSTSEC				; Reset sector no
		sta     SECTORNO                ; 85 ED
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


;============================================
; START RANDOM ACCESS ROUTINES
;============================================

;--------------------------------------------
; *SHUT command
;--------------------------------------------
.shutcom:                               

.LE89C: 
		ldy     #$00                    ; A0 00
.NEW_SHTVEC: 
		pha                             ; 48
        cld                             ; D8
        tya                             ; 98
        bne     LE8B2                   ; D0 0F
.LE8A3: clc                             ; 18
        adc     #$20                    ; 69 20
        beq     LE8B0                   ; F0 08
        tay                             ; A8
        jsr     NEW_SHTVEC                   ; 20 9E E8
        bne     LE8A3                   ; D0 F5
.LE8AE: ldx     $00C6                   ; A6 C6
.LE8B0: pla                             ; 68
        rts                             ; 60

.LE8B2: jsr     LEA7C                   ; 20 7C EA
        bcs     LE8AE                   ; B0 F7
        cpy     EXECHANDLE                   ; C4 B9
        bne     LE8C1                   ; D0 06
        jsr     LE352                   ; 20 52 E3
        lsr     A                       ; 4A
        sta     EXECHANDLE                   ; 85 B9
.LE8C1: cpy     SPOOLHANDLE                   ; C4 BA
        bne     LE8CA                   ; D0 05
        jsr     LE356                   ; 20 56 E3
        sta     SPOOLHANDLE                   ; 85 BA
.LE8CA: lda     WKBASE + $0217,Y        ; B9 17 22
        and     #$60                    ; 29 60
        beq     LE906                   ; F0 35
        jsr     LE912                   ; 20 12 E9
        lda     WKBASE + $0217,Y        ; B9 17 22
        and     #$20                    ; 29 20
        beq     LE900                   ; F0 25
        ldx     $00C4                   ; A6 C4
        lda     WKBASE + $0214,Y        ; B9 14 22
        sta     WKBASE + $010C,X        ; 9D 0C 21
        lda     WKBASE + $0215,Y        ; B9 15 22
        sta     WKBASE + $010D,X        ; 9D 0D 21
        lda     WKBASE + $0216,Y        ; B9 16 22
        asl     A                       ; 0A
        asl     A                       ; 0A
        asl     A                       ; 0A
        asl     A                       ; 0A
        eor     WKBASE + $010E,X        ; 5D 0E 21
        and     #$F0                    ; 29 F0
        eor     WKBASE + $010E,X        ; 5D 0E 21
        sta     WKBASE + $010E,X        ; 9D 0E 21
        jsr     write_cat                   ; 20 4A E7
        ldy     $00C2                   ; A4 C2
.LE900: jsr     LEB76                   ; 20 76 EB
        jsr     LE500                   ; 20 00 E5
.LE906: lda     WKBASE + $021B,Y        ; B9 1B 22
        eor     #$FF                    ; 49 FF
        and     $00C0                   ; 25 C0
        sta     $00C0                   ; 85 C0
        jmp     LE8AE                   ; 4C AE E8
.LE912: jsr     LE93E                   ; 20 3E E9
.LE915: ldx     #$07                    ; A2 07
.LE917: lda     WKBASE + $020C,Y        ; B9 0C 22
        sta     $00A4,X                 ; 95 A4
        dey                             ; 88
        dey                             ; 88
        dex                             ; CA
        bne     LE917                   ; D0 F6
        jsr     FILEINCAT                   ; 20 5D E1
        bcc     LE959+1                 ; 90 34
        sty     $00C4                   ; 84 C4
        lda     WKBASE + $010E,Y        ; B9 0E 21
        ldx     WKBASE + $010F,Y        ; BE 0F 21
        ldy     $00C2                   ; A4 C2
        eor     WKBASE + $020D,Y        ; 59 0D 22
        and     #$0F                    ; 29 0F
        bne     LE959+1                 ; D0 23
        txa                             ; 8A
        cmp     WKBASE + $020F,Y        ; D9 0F 22
        bne     LE959+1                 ; D0 1D
        rts                             ; 60
.LE93E: lda     DRIVENO                   ; A5 EE
        sta     SAVDRIVENO                   ; 85 C7
        lda     USEQUAL                   ; A5 AC
        sta     SAVQUAL                   ; 85 C8
        lda     WKBASE + $020E,Y        ; B9 0E 22
        and     #$7F                    ; 29 7F
        sta     USEQUAL                   ; 85 AC
        lda     WKBASE + $0217,Y        ; B9 17 22
        jmp     readcat                   ; 4C 3F E4

;============================================
;OSFIND entry
;--------------------------------------------
.NEW_FNDVEC  
		cld                             ; D8
        tya                             ; 98
        pha                             ; 48
        stx     $00C6                   ; 86 C6
.LE958: php                             ; 08
.LE959: lda     $00,X                   ; B5 00
        sta     FILENAMEPTR                   ; 85 9A
        lda     $0001,X                 ; B5 01
        sta     FILENAMEPTR+1                   ; 85 9B
        jsr     LE075                   ; 20 75 E0
        jsr     FILEINCAT                   ; 20 5D E1
        bcs     LE988                   ; B0 1F
        plp                             ; 28
        bcc     LE971                   ; 90 05
        ldy     #$00                    ; A0 00
        jmp     LEA51                   ; 4C 51 EA

.LE971: lda     #$00                    ; A9 00
        ldx     #$08                    ; A2 08
.LE975: sta     FILENAMEPTR+1,X                 ; 95 9B
        dex                             ; CA
        bne     LE975                   ; D0 FB
        lda     #$40                    ; A9 40
        sta     STARTSEC+1                   ; 85 A3
        ldx     #$9A                    ; A2 9A
        jsr     LE61A                   ; 20 1A E6
        ldx     $00C6                   ; A6 C6
        clc                             ; 18
        bcc     LE958                   ; 90 D0

.LE988: sty     $00C3                   ; 84 C3
.LE98A: lda     #$00                    ; A9 00
        sta     $00C2                   ; 85 C2
        ldy     #$A0                    ; A0 A0
        lda     #$08                    ; A9 08
.LE992: bit     $00C0                   ; 24 C0
        beq     LE9BE                   ; F0 28
        pha                             ; 48
        sty     $00C4                   ; 84 C4
        ldx     $00C3                   ; A6 C3
        lda     #$08                    ; A9 08
        sta     $00C5                   ; 85 C5
.LE99F: lda     WKBASE + $0200,Y        ; B9 00 22
        cmp     WKBASE + $0008,X        ; DD 08 20
        bne     LE9C5                   ; D0 1E
        iny                             ; C8
        lda     WKBASE + $0200,Y        ; B9 00 22
        cmp     WKBASE + $0108,X        ; DD 08 21
        bne     LE9C5                   ; D0 15
        iny                             ; C8
        inx                             ; E8
        dec     $00C5                   ; C6 C5
        bne     LE99F                   ; D0 E9
        ldy     $00C4                   ; A4 C4
        ldx     $00C6                   ; A6 C6
        jsr     NEW_SHTVEC                   ; 20 9E E8
        pla                             ; 68
.LE9BE: sty     $00C2                   ; 84 C2
        sta     $00C1                   ; 85 C1
        jmp     LE9C8                   ; 4C C8 E9
.LE9C5: ldy     $00C4                   ; A4 C4
        pla                             ; 68
.LE9C8: pha                             ; 48
        tya                             ; 98
        sec                             ; 38
        sbc     #$20                    ; E9 20
        tay                             ; A8
        pla                             ; 68
        asl     A                       ; 0A
        bne     LE992                   ; D0 C0
        ldy     $00C2                   ; A4 C2
        beq     LE98A+1                 ; F0 B5
        ldx     $00C3                   ; A6 C3
        lda     #$08                    ; A9 08
        sta     $00C5                   ; 85 C5
.LE9DC: lda     WKBASE + $0008,X        ; BD 08 20
        sta     WKBASE + $0200,Y        ; 99 00 22
        iny                             ; C8
        lda     WKBASE + $0108,X        ; BD 08 21
        sta     WKBASE + $0200,Y        ; 99 00 22
        iny                             ; C8
        inx                             ; E8
        dec     $00C5                   ; C6 C5
        bne     LE9DC                   ; D0 ED
        ldx     #$10                    ; A2 10
        lda     #$00                    ; A9 00
.LE9F3: sta     WKBASE + $0200,Y        ; 99 00 22
        iny                             ; C8
        dex                             ; CA
        bne     LE9F3                   ; D0 F9
        lda     $00C2                   ; A5 C2
        tay                             ; A8
        jsr     diva32                   ; 20 FA E0
        adc     #$22                    ; 69 22
        sta     WKBASE + $0213,Y        ; 99 13 22
        lda     $00C1                   ; A5 C1
        sta     WKBASE + $021B,Y        ; 99 1B 22
        ora     $00C0                   ; 05 C0
        sta     $00C0                   ; 85 C0
        lda     WKBASE + $0209,Y        ; B9 09 22
        adc     #$FF                    ; 69 FF
        lda     WKBASE + $020B,Y        ; B9 0B 22
        adc     #$00                    ; 69 00
        sta     WKBASE + $0219,Y        ; 99 19 22
        lda     WKBASE + $020D,Y        ; B9 0D 22
        ora     #$0F                    ; 09 0F
        adc     #$00                    ; 69 00
        jsr     diva16                   ; 20 FB E0
        sta     WKBASE + $021A,Y        ; 99 1A 22
        plp                             ; 28
        bcc     LEA5A                   ; 90 2F
        lda     WKBASE + $0209,Y        ; B9 09 22
        sta     WKBASE + $0214,Y        ; 99 14 22
        lda     WKBASE + $020B,Y        ; B9 0B 22
        sta     WKBASE + $0215,Y        ; 99 15 22
        lda     WKBASE + $020D,Y        ; B9 0D 22
        jsr     diva16                   ; 20 FB E0
        sta     WKBASE + $0216,Y        ; 99 16 22
.LEA40: lda     DRIVENO                   ; A5 EE
        and     #$0F                    ; 29 0F
        ora     WKBASE + $0217,Y        ; 19 17 22
        sta     WKBASE + $0217,Y        ; 99 17 22
        jsr     LEAD1                   ; 20 D1 EA
        lda     SETQUAL                   ; A5 CD
        sta     USEQUAL                   ; 85 AC
.LEA51: sty     $00C3                   ; 84 C3
        ldx     $00C6                   ; A6 C6
        pla                             ; 68
        tay                             ; A8
        lda     $00C3                   ; A5 C3
        rts                             ; 60
.LEA5A: lda     #$20                    ; A9 20
        sta     WKBASE + $0217,Y        ; 99 17 22
        bne     LEA40                   ; D0 DF
.NEW_RDRVEC: 
		pha                             ; 48
        sty     $00C2                   ; 84 C2
        asl     A                       ; 0A
        asl     A                       ; 0A
        adc     $00C2                   ; 65 C2
        tay                             ; A8
        lda     WKBASE + $0210,Y        ; B9 10 22
        sta     $00,X                   ; 95 00
        lda     WKBASE + $0211,Y        ; B9 11 22
        sta     $0001,X                 ; 95 01
        lda     WKBASE + $0212,Y        ; B9 12 22
        sta     $0002,X                 ; 95 02
        ldy     $00C2                   ; A4 C2
        pla                             ; 68
        rts                             ; 60
.LEA7C: pha                             ; 48
        stx     $00C6                   ; 86 C6
        tya                             ; 98
        and     #$E0                    ; 29 E0
        sta     $00C2                   ; 85 C2
        beq     LEA97                   ; F0 11
        jsr     diva32                   ; 20 FA E0
        tay                             ; A8
        lda     #$00                    ; A9 00
        sec                             ; 38
.LEA8D: ror     A                       ; 6A
        dey                             ; 88
        bne     LEA8D                   ; D0 FC
        ldy     $00C2                   ; A4 C2
        bit     $00C0                   ; 24 C0
        bne     LEA9A                   ; D0 03
.LEA97: pla                             ; 68
        sec                             ; 38
        rts                             ; 60
.LEA9A: pla                             ; 68
        clc                             ; 18
.LEA9C: rts                             ; 60
.LEA9D: ldx     #$20                    ; A2 20
        txa                             ; 8A
        clc                             ; 18
.LEAA1: adc     WKBASE + $0200,Y        ; 79 00 22
        iny                             ; C8
        dex                             ; CA
        bne     LEAA1                   ; D0 F9
        adc     #$00                    ; 69 00
        ldy     $00C2                   ; A4 C2
        rts                             ; 60
.LEAAD: jsr     LEA9D                   ; 20 9D EA
        cmp     #$FF                    ; C9 FF
        beq     LEA9C                   ; F0 E8
        EQUB    $00                     
.LEAB5: clc                             ; 18
        lda     WKBASE + $020F,Y        ; B9 0F 22
        adc     WKBASE + $0211,Y        ; 79 11 22
        sta     STARTSEC+1                   ; 85 A3
        sta     WKBASE + $021C,Y        ; 99 1C 22
        lda     WKBASE + $020D,Y        ; B9 0D 22
        and     #$0F                    ; 29 0F
        adc     WKBASE + $0212,Y        ; 79 12 22
        sta     STARTSEC                   ; 85 A2
        sta     WKBASE + $021D,Y        ; 99 1D 22
        jsr     LEB65                   ; 20 65 EB
.LEAD1: lda     #$FF                    ; A9 FF
        sta     WKBASE + $021F,Y        ; 99 1F 22
        jsr     LEA9D                   ; 20 9D EA
        eor     #$FF                    ; 49 FF
.LEADB: sta     WKBASE + $021F,Y        ; 99 1F 22
        rts                             ; 60
.LEADF: clc                             ; 18
        adc     WKBASE + $021F,Y        ; 79 1F 22
        adc     #$00                    ; 69 00
        bne     LEADB                   ; D0 F4
.LEAE7: jsr     OSECHO                  ; 20 E6 FF
        cmp     #$04                    ; C9 04
        beq     LEB10                   ; F0 22
        clc                             ; 18
        rts                             ; 60

.NEW_BGTVEC: 
		cld                             ; D8
        tya                             ; 98
        beq     LEAE7                   ; F0 F3
        jsr     LEA7C                   ; 20 7C EA
        bcs     LEB33+1                 ; B0 3B
        tya                             ; 98
        jsr     LECFA                   ; 20 FA EC
        bne     LEB14                   ; D0 15
        lda     WKBASE + $0217,Y        ; B9 17 22
        and     #$10                    ; 29 10
        bne     LEB54+1                 ; D0 4F
        lda     #$10                    ; A9 10
        jsr     LEB67                   ; 20 67 EB
        jsr     LEAD1                   ; 20 D1 EA
        ldx     $00C6                   ; A6 C6
.LEB10: lda     #$FF                    ; A9 FF
        sec                             ; 38
        rts                             ; 60
.LEB14: lda     WKBASE + $0217,Y        ; B9 17 22
        bmi     LEB29                   ; 30 10
        jsr     LEAAD                   ; 20 AD EA
        jsr     LE93E                   ; 20 3E E9
        jsr     LEB76                   ; 20 76 EB
        sec                             ; 38
        jsr     LEB7E                   ; 20 7E EB
        jsr     LE500                   ; 20 00 E5
.LEB29: lda     WKBASE + $0210,Y        ; B9 10 22
        sta     FILENAMEPTR                   ; 85 9A
        lda     WKBASE + $0213,Y        ; B9 13 22
        sta     FILENAMEPTR+1                   ; 85 9B
.LEB33: ldy     #$00                    ; A0 00
        lda     (FILENAMEPTR),Y               ; B1 9A
        pha                             ; 48
        ldy     $00C2                   ; A4 C2
        lda     #$FE                    ; A9 FE
        jsr     LEADF                   ; 20 DF EA
        ldx     FILENAMEPTR                   ; A6 9A
        inx                             ; E8
        txa                             ; 8A
        sta     WKBASE + $0210,Y        ; 99 10 22
        bne     LEB61                   ; D0 19
        clc                             ; 18
        lda     WKBASE + $0211,Y        ; B9 11 22
        adc     #$01                    ; 69 01
        sta     WKBASE + $0211,Y        ; 99 11 22
        lda     WKBASE + $0212,Y        ; B9 12 22
.LEB54: adc     #$00                    ; 69 00
        sta     WKBASE + $0212,Y        ; 99 12 22
        jsr     LEB6C                   ; 20 6C EB
        lda     #$80                    ; A9 80
        jsr     LEADF                   ; 20 DF EA
.LEB61: clc                             ; 18
        jmp     LE8AE                   ; 4C AE E8
.LEB65: lda     #$80                    ; A9 80
.LEB67: ora     WKBASE + $0217,Y        ; 19 17 22
        bne     LEB71                   ; D0 05
.LEB6C: lda     #$7F                    ; A9 7F
.LEB6E: and     WKBASE + $0217,Y        ; 39 17 22
.LEB71: sta     WKBASE + $0217,Y        ; 99 17 22
        clc                             ; 18
        rts                             ; 60
.LEB76: lda     WKBASE + $0217,Y        ; B9 17 22
        and     #$40                    ; 29 40
        beq     LEBB7                   ; F0 3A
        clc                             ; 18
.LEB7E: php                             ; 08
        jsr     START_MOTOR_SELECT                   ; 20 5B E7
        ldy     $00C2                   ; A4 C2
        lda     WKBASE + $0213,Y        ; B9 13 22
        sta     LOADADDR+1                   ; 85 9D
.LEB89: lda     #$00                    ; A9 00
        sta     LOADADDR                   ; 85 9C
        sta     FILESIZE                   ; 85 A0
        lda     #$01                    ; A9 01
        sta     FILESIZE+1                   ; 85 A1
        plp                             ; 28
        bcs     LEBAC                   ; B0 16
        lda     WKBASE + $021C,Y        ; B9 1C 22
        sta     STARTSEC+1                   ; 85 A3
        lda     WKBASE + $021D,Y        ; B9 1D 22
        sta     STARTSEC                   ; 85 A2
        jsr     ROM_WRITE_SECTORS                   ; 20 B0 E6
        ldy     $00C2                   ; A4 C2
        lda     #$BF                    ; A9 BF
        jsr     LEB6E                   ; 20 6E EB
        bcc     LEBB2                   ; 90 06
.LEBAC: jsr     LEAB5                   ; 20 B5 EA
        jsr     ROM_READ_SECTORS                   ; 20 A3 E4
.LEBB2: 
		ldy     $00C2                   ; A4 C2
.LEBB7: rts                             ; 60

.LEBB8: pla                             ; 68
        jmp     OSASCI                 ; 4C E9 FF

.NEW_BPTVEC: 
		cld                             ; D8
        pha                             ; 48
        tya                             ; 98
        beq     LEBB8                   ; F0 F7
        jsr     LEA7C                   ; 20 7C EA
        bcs     LEC1E+1                 ; B0 59
        jsr     LEAAD                   ; 20 AD EA
        lda     WKBASE + $020E,Y        ; B9 0E 22
        bmi     LEB89+1                 ; 30 BC
        jsr     LE93E                   ; 20 3E E9
        tya                             ; 98
        clc                             ; 18
        adc     #$04                    ; 69 04
        jsr     LECFA                   ; 20 FA EC
        bne     LEC2B                   ; D0 51
        jsr     LE915                   ; 20 15 E9
        ldx     $00C4                   ; A6 C4
        sec                             ; 38
        lda     WKBASE + $0107,X        ; BD 07 21
        sbc     WKBASE + $010F,X        ; FD 0F 21
        pha                             ; 48
        lda     WKBASE + $0106,X        ; BD 06 21
        sbc     WKBASE + $010E,X        ; FD 0E 21
        and     #$0F                    ; 29 0F
        sta     $00C3                   ; 85 C3
        asl     A                       ; 0A
        asl     A                       ; 0A
        asl     A                       ; 0A
        asl     A                       ; 0A
        eor     WKBASE + $010E,X        ; 5D 0E 21
        and     #$F0                    ; 29 F0
        eor     WKBASE + $010E,X        ; 5D 0E 21
        cmp     WKBASE + $010E,X        ; DD 0E 21
        sta     WKBASE + $010E,X        ; 9D 0E 21
        bne     LEC12                   ; D0 0D
        pla                             ; 68
        cmp     WKBASE + $010D,X        ; DD 0D 21
        bne     LEC13                   ; D0 08
        jsr     LE500                   ; 20 00 E5
        jsr     NEW_SHTVEC                   ; 20 9E E8
		brk                             
.LEC12: pla                             ; 68
.LEC13: sta     WKBASE + $010D,X        ; 9D 0D 21
        sta     WKBASE + $0219,Y        ; 99 19 22
        lda     $00C3                   ; A5 C3
        sta     WKBASE + $021A,Y        ; 99 1A 22
.LEC1E: lda     #$00                    ; A9 00
        sta     WKBASE + $010C,X        ; 9D 0C 21
        jsr     write_cat                   ; 20 4A E7
        ldy     $00C2                   ; A4 C2
.LEC2B: lda     WKBASE + $0217,Y        ; B9 17 22
        bmi     LEC47                   ; 30 17
        jsr     LEB76                   ; 20 76 EB
        lda     WKBASE + $0214,Y        ; B9 14 22
        bne     LEC43                   ; D0 0B
        tya                             ; 98
        jsr     LECFA                   ; 20 FA EC
        bne     LEC43                   ; D0 05
        jsr     LEAB5                   ; 20 B5 EA
        bne     LEC47                   ; D0 04
.LEC43: sec                             ; 38
        jsr     LEB7E                   ; 20 7E EB
.LEC47: lda     WKBASE + $0210,Y        ; B9 10 22
        sta     FILENAMEPTR                   ; 85 9A
        lda     WKBASE + $0213,Y        ; B9 13 22
        sta     FILENAMEPTR+1                   ; 85 9B
        pla                             ; 68
        ldy     #$00                    ; A0 00
        sta     (FILENAMEPTR),Y               ; 91 9A
        pha                             ; 48
        ldy     $00C2                   ; A4 C2
        lda     #$40                    ; A9 40
        jsr     LEB67                   ; 20 67 EB
        inc     FILENAMEPTR                   ; E6 9A
        lda     FILENAMEPTR                   ; A5 9A
        sta     WKBASE + $0210,Y        ; 99 10 22
        bne     LEC7A                   ; D0 13
        jsr     LEB6C                   ; 20 6C EB
        lda     WKBASE + $0211,Y        ; B9 11 22
        adc     #$01                    ; 69 01
        sta     WKBASE + $0211,Y        ; 99 11 22
        lda     WKBASE + $0212,Y        ; B9 12 22
.LEC75: adc     #$00                    ; 69 00
        sta     WKBASE + $0212,Y        ; 99 12 22
.LEC7A: tya                             ; 98
        jsr     LECFA                   ; 20 FA EC
        bcc     LEC97                   ; 90 17
        lda     #$20                    ; A9 20
        jsr     LEB67                   ; 20 67 EB
        lda     WKBASE + $0210,Y        ; B9 10 22
        sta     WKBASE + $0214,Y        ; 99 14 22
        lda     WKBASE + $0211,Y        ; B9 11 22
        sta     WKBASE + $0215,Y        ; 99 15 22
        lda     WKBASE + $0212,Y        ; B9 12 22
        sta     WKBASE + $0216,Y        ; 99 16 22
.LEC97: jsr     LEAD1                   ; 20 D1 EA
        jsr     LE500                   ; 20 00 E5
        jmp     LEB61                   ; 4C 61 EB
		
.NEW_STRVEC: pha                             ; 48
        jsr     LEA7C                   ; 20 7C EA
        bcs     LEC75+1                 ; B0 D0
        jsr     LEAAD                   ; 20 AD EA
        ldx     $00C6                   ; A6 C6
        jsr     incy4                   ; 20 04 E1
        jsr     LED12                   ; 20 12 ED
        bcc     LED15+1                 ; 90 63
        ldy     $00C2                   ; A4 C2
.LECB5: jsr     LED12                   ; 20 12 ED
        bcs     LECC1                   ; B0 07
        lda     #$FF                    ; A9 FF
        jsr     NEW_BPTVEC                   ; 20 BC EB
        bne     LECB5                   ; D0 F4
.LECC1: lda     $00,X                   ; B5 00
        sta     WKBASE + $0210,Y        ; 99 10 22
        lda     $0001,X                 ; B5 01
        sta     WKBASE + $0211,Y        ; 99 11 22
        lda     $0002,X                 ; B5 02
        sta     WKBASE + $0212,Y        ; 99 12 22
        lda     #$6F                    ; A9 6F
        jsr     LEB6E                   ; 20 6E EB
        lda     WKBASE + $020F,Y        ; B9 0F 22
        adc     WKBASE + $0211,Y        ; 79 11 22
        sta     $00C5                   ; 85 C5
        lda     WKBASE + $020D,Y        ; B9 0D 22
        and     #$0F                    ; 29 0F
        adc     WKBASE + $0212,Y        ; 79 12 22
        cmp     WKBASE + $021D,Y        ; D9 1D 22
        bne     LECF4                   ; D0 0A
        lda     $00C5                   ; A5 C5
        cmp     WKBASE + $021C,Y        ; D9 1C 22
        bne     LECF4                   ; D0 03
        jsr     LEB65                   ; 20 65 EB
.LECF4: jsr     LEAD1                   ; 20 D1 EA
        jmp     LEB61                   ; 4C 61 EB
.LECFA: tax                             ; AA
        lda     WKBASE + $0212,Y        ; B9 12 22
        cmp     WKBASE + $0216,X        ; DD 16 22
        bne     LED11                   ; D0 0E
        lda     WKBASE + $0211,Y        ; B9 11 22
        cmp     WKBASE + $0215,X        ; DD 15 22
        bne     LED11                   ; D0 06
        lda     WKBASE + $0210,Y        ; B9 10 22
        cmp     WKBASE + $0214,X        ; DD 14 22
.LED11: rts                             ; 60
.LED12: lda     WKBASE + $0214,Y        ; B9 14 22
.LED15: cmp     $00,X                   ; D5 00
        lda     WKBASE + $0215,Y        ; B9 15 22
        sbc     $0001,X                 ; F5 01
        lda     WKBASE + $0216,Y        ; B9 16 22
        sbc     $0002,X                 ; F5 02
        rts                             ; 60


;============================================
; DOS Interpreter entry
;--------------------------------------------

.LEEE2: ldx     #VECOFS                 ; point at LODVEC
.LEEE4: lda     VECTBL-VECOFS,X         ; get byte from ROM
        sta     VECBASE,X               ; put in vector table
        inx                              
        cpx     #VECTBLLEN+VECOFS       ; done all?
        bne     LEEE4                   ; no : keep going
		
        lda     #<NMIROUT             	; Setup new NMI vector
        sta     NMIVEC                  
        lda     #>NMIROUT                
        sta     NMIVEC+1                
		
        lda     #<NEWCOMVEC             ; setup new command vec
        sta     COMVEC 
        lda     #>NEWCOMVEC             
        sta     COMVEC+1                

        lda     #SPACE                  ; A9 20
		
        sta     SETQUAL                 ; 85 CD
        sta     USEQUAL                 ; Current QUAL = space
        ldy     #$00                    ; Zero low ram vars & cmd ptr
		
		sty     DRIVENO                 ; Drive num
        sty     $C0                     ; 84 C0
        sty     EXECHANDLE              ; 84 B9
        sty     SPOOLHANDLE             ; 84 BA

		lda		#40-1					; tracks are 00..39
		sta		NTRC   

if (USEAUTO)
		ldy		#0						; zero pointer
.autoloop
		lda		autoname,y				; get a byte
		sta		STACKPAGE,y				; save in buffer
		cmp		#$0d					; end ?
		beq		runit					; yes : exit
		iny								; increment pointer
		bne		autoloop
.runit
		ldy		#0						; point at name
		jmp		exec					; go run it

.autoname
		equb 	"MENU",$0d	
else
        rts                             
endif

;============================================
; DOS vectors $20C-$21B
;--------------------------------------------

.VECTBL EQUB    <NEW_LODVEC, >NEW_LODVEC	; $20C LODVEC
        EQUB    <NEW_SAVVEC, >NEW_SAVVEC    ; $20E SAVVEC
        EQUB    <NEW_RDRVEC, >NEW_RDRVEC	; $210 RDRVEC
        EQUB    <NEW_STRVEC, >NEW_STRVEC    ; $212 STRVEC
        EQUB    <NEW_BGTVEC, >NEW_BGTVEC    ; $214 BGTVEC
        EQUB    <NEW_BPTVEC, >NEW_BPTVEC    ; $216 BPTVEC
        EQUB    <NEW_FNDVEC, >NEW_FNDVEC    ; $218 FNDVEC
        EQUB    <NEW_SHTVEC, >NEW_SHTVEC    ; $21A SHTVEC
.VECTBLEND

VECTBLLEN	= VECTBLEND - VECTBL
VECOFS		= LODVEC - VECBASE

;*****************************************************
; The following code was borrowed / adapted from GDOS
; 2015-07-03 PHS.
;*****************************************************

;
; Main NMI handler for disk.
; This routine gets the status from the disk controller and saves it
; for the error handlers. It then modifies it's own return address to
; jump to the user handler for the specified command.
;

.NMIROUT                                
		LDA     WSTATUS        		; Get status from WD
        STA     DSTATUS           	; save it for error handlers      
        
		LDA     $B000             	; read keyboard ?
        AND     #$F0                
        STA     $B000               

		PLA							; restore saved A
		PLP							; restore flags
		PLA
		PLA							; drop saved return address
		
		JMP		NMIHANDLER			; go handle it

if (0)    
        TXA                      	; save x on stack
        PHA                             
        TSX                       	; get SP to X      
        
		LDA     NMI+1              	; get our NMI handler address
        STA     $105,X              ; modify our return address on stack    
        LDA     NMI                 ; with NMI routine address    
        STA     $104,X              
    
        PLA                         ; restore x from stack
        TAX                             
        PLA                         ; return to target NMI routine    
        RTI                             
endif

;
; Seek to a track
;
.SEEK                                   	
        LDA     TRACKNO       		; get track to seek to 
        STA     WDATA               ; send to WD    
        LDA     #WSEEKC            	; Seek command      
		JMP		STEP_RESTORE		; go do it          

.HANDLE_SEEK		                                
        JSR     ERRTYPE1          	; Check type 1 error
        BCS     SEEK                ; if error, try again    
.SAVE_CTRACK
		JSR		GET_DRV_Y			; get driveno in y
		LDA		WTRACK				; get track from WD
		STA		SAVTRK0,y			; save it
        RTS            

;
; Step in a track.
;

.STEPIN                                 
        INC     TRACKNO             ; update current track
		LDA     #WSTPIC  			; step in command            
		JMP		STEP_RESTORE		; go do it
;
; Restore to track 0
;
.RESTORE0
		LDA		#0					; Zero current track no
		STA		TRACKNO
		
        LDA     #WRESTC          	; restore command
        AND     #WCF_RATE30         ; spin up and don't verify, leave rate bits    

; WD command in A
.STEP_RESTORE                                        
        STA		WDCOMM				; get WD command (step in or restore)
        STA     WCMD               	; send it     
.HUP1                                   
        BNE     HUP1              	; wait, nmi breaks out      

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
.RS7                                ; Wait for NMI
        BEQ     RS7                     

; NMI handler
.NMIHANDLER
.RWSNMI                                    
		LDA		WDCOMM				; retrieve saved WD command.
		CMP		#WSEEKC				; Seek command ?
		BNE		CHECK_RW			; No check for sector read/write

		JMP		HANDLE_SEEK			; Do SEEK actions
	
.CHECK_RW
		CMP		#WRSCTC				; read sec?
		BEQ		HANDLE_RW			; yep handle it

		CMP		#WWSCTC				; write sec?
		BEQ		HANDLE_RW			; yep handle it
		
		CMP		#WRDRSC				; Read sector headers?
		BEQ		HANDLE_RDHD			; yep handle it
		CMP		#WWTRCC				; Write track
		BEQ		HANDLE_WTRACK		; yep handle it
;		BNE		NMIEXIT				; no : exit
		BNE		SAVE_CTRACK  		; no : exit (restore)

.HANDLE_RDHD		
		JMP		VERIFY_NMI			; verify sectors
.HANDLE_WTRACK		
		JMP		WTRACK_NMI
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
        BEQ     ERR_CRC                ; yep, do it
                
		JSR		RESTORE_ERR			; Restore to track 0 + error
		EQUB	"SEEK ERROR"

.ERR_TRACKNO
		JSR     MESSAGE             ; Display message
		EQUB	" TR:"      
        
		LDA     TRACKNO             ; get track number
        JSR     PRINT_HEXA             ; display it ?
        BRK                         ; back to command loop?
		                 
.ERR_CRC
        JSR     MESSAGE             ; display message
		EQUB	"CRC"                                 
		BRK                             
		
.ERR13                                  
		PLA							; drop saved status
        DEC     RETRYCOUNT          ; decrement retry count
        SEC                         ; flag error
        RTS                             
		
.ERR5                                   
        CLC							; flag no error                             
        LDA     #4                  ; reset retry count
        STA     RETRYCOUNT                 
        RTS                             

;
; Check for write protected disk
;

.WRPROT                                 
        LDA     WSTATUS             ; get status register from WD
        ASL     A                   ; get wp bit into bit 7
        BPL     WRP1                ; not write protected, exit
		
		JSR		RESTORE_ERR			; Restore to track 0 + error
		EQUB	"WRITE PROT"                 
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
				
        JSR     MESSAGE             ; write message
		EQUB	"WRITE FAULT"                    
.ERRC                               ; get track no 
        JSR     MESSAGE             ; write sector message
		EQUB	" SCT:"                           
        LDA     SECTORNO            ; get sector no
        JSR     PRINT_HEXA             ; write it
		JMP		ERR_TRACKNO			; Write trackno and exit
		
.ERR6                                   
        ASL     A                   ; get RNF flag in b7
        BPL     ERR12               ; no error : skip
				
        JSR     MESSAGE             ; print record not found message
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
		EQUB	"BAD DATA"                           
		BRK                                            

.RESTORE_ERR
		JSR     RESTORE0            ; unload head
        JMP     MESSAGE             ; bad data message


.fortycom 
		lda		#40-1				; tracks are 0..39
		bne		set_tracks			
.eightycom
		lda		#80-1				; tracks are 0..79
.set_tracks
		sta		NTRC
		rts
		
.formatcom         
.FM1                                    
		jsr		START_MOTOR_SELECT				; select drive, turn on motors
        JSR     RESTORE0                 	; seek back to track 0

        JSR     WRPROT                  	; check for write protect, errors if wp.
        LDA     #15                     	; write char
        JSR     OSWRCH                 
.FM2          
		JSR		MEMPTR_WKBASE				; point at format buffer

        JSR     FOLDOUT                 	; write formatted track to buffer
        LDA     TRACKNO                   	; print track no
        JSR     PRINT_HEXA                   
        LDA     #':'                  		; print ':'
        JSR     OSWRCH                    
.FM8                                    
		JSR		MEMPTR_WKBASE				; point at track buffer

        JSR     WRITE_TRACK                 ; write the track to disk
        BCS     FM8                     	; error, try writing again!
		
        LDA     TRACKNO                   	; get track no
        CMP     NTRC                    	; done all?
        BEQ     FM6                     	; yep : exit loop	
        JSR     STEPIN                  	; step heads to next track
        JMP     FM2                     	; loop again
.FM6                                    
        JSR     INLINE_PRINT               	; print formatted message  
		EQUB	CR,LF,"FORMATTED"
		EQUB	CR,LF,LF  
        NOP 
		jsr		blank_cat					; write a blank catalog
        JSR     verifycom                  	; go verify freshly formatted disk              

.MEMPTR_WKBASE
        LDY     #WKLOW
        STY     MEMPTR                     
        LDA     #WKHIGH
        STA     MEMPTR+1                   
		rts

;
; Blank the catalog, set total sectors
;
.blank_cat
		lda		#0							; 256 bytes 2 buffers
		tay									; init buffer pointer		
.blank_loop	
		lda		#SPACE						; fill fist cat sector with spaces
		sta		WKBASE,y		
		lda		#0							; fill second cat sector with null bytes
		sta		WKBASE+$100,y
		iny									; increment pointer
		bne		blank_loop					; do next
		
		lda		NTRC
		cmp		#80							; 80 tracks
		beq		set_80
		
		lda		#>TotalTrk40				; set no of tracks
		sta		WKSecCOpt
		lda		#<TotalTrk40
		sta		WKSecCLSB
		jmp     write_cat               	; Write catalog back to disk

.set_80
		lda		#>TotalTrk80				; set no of tracks
		sta		WKSecCOpt
		lda		#<TotalTrk80
		sta		WKSecCLSB		
		jmp     write_cat               	; Write catalog back to disk

;
; Generate a byte stream in memory that represents a raw track.
;

TrackPtr	= TEMP2
SecPtr		= TEMP1
SecIDPtr	= TMPWORK

SecMarker	= $CC							; Sector marker within table, replaced with sector id
TrkMarker	= $DD							; Track marker within table, replaced with track id
EndMarker	= $63							; end of table marker

.FOLDOUT                                
        LDA     #0                      	; initialize sector count
        STA     SECTORNO                   
.FD1                                    
        LDA     #<TRFORMS          			; initialize track pointer
        STA     TrackPtr                   
        LDA     #>TRFORMS          
        STA     TrackPtr+1              
   
        LDA     #<GAP1S            			; initialize sector format pointer
        STA     SecPtr                   
        LDA     #>GAP1S            
        STA     SecPtr+1                 
		
        LDA     #<INTERLS         			; initialize sector id pointer
        STA     SecIDPtr                    
        LDA     #>INTERLS         
        STA     SecIDPtr+1      
            
.FD2                                    
        LDY     #0  						; init table pointer					                    	
        LDA     (TrackPtr),Y               	; get byte count
        INY                             	; point to next
        TAX                             	; transfer count to x
        LDA     (TrackPtr),Y               	; get byte from table
		
        CMP     #EndMarker                 	; end of table?
        BEQ     FD6                     	; yep : branch ahead, do next sector
        CMP     #SecMarker                 	; sector no marker?
        BNE     FD8                     	; no : skip
        
		STY     VTEMP                   	; save pointer
        LDY     SECTORNO                   	; get offset into interleave ID table
        LDA     (SecIDPtr),Y                ; get ID for this sector
        LDY     VTEMP                   	; restore pointer
.FD8                                    
        CMP     #TrkMarker  				; track marker?                  	
        BNE     FD9                     	; nope : skip
        LDA     TRACKNO                   	; get track number
.FD9                                    
        DEY                             

; write X copiues of byte in A to buffer
		
.FD3                                    
        STA     (MEMPTR),Y                 	; save byte in format buffer
        INY                             	; inc buffer pointer
        DEX                             	; decrement count
        BNE     FD3                     	; more loop again
		
        LDX     #(TrackPtr-$5A)   			; point at track pointer         
        JSR     INC5AX                   	; TrkPointer = TrkPointer + 2
        JSR     INC5AX                   
.FD4                                    
        TYA                             	; end of buffer page?
        BNE     FD5                     	; nope : skip
        INC     MEMPTR+1                   	; move to next page
        BNE     FD2                     	; and loop again

; add number of bytes written to format buffer pointer?
		
.FD5                                    
        CLC                             	; clear carry for addition
        ADC     MEMPTR                     	; add pointer LSB
        STA     MEMPTR                     	; update it
        LDA     MEMPTR+1                   	; carry to MSB
        ADC     #0                      
        STA     MEMPTR+1                   	; update that
        BNE     FD2                     	; loop again
		
.FD6                                    
        INC     SECTORNO                   	; move to next sector
        LDA     #SECTRACK-1					; get sectors per track                  
        CMP     SECTORNO                   	; done all?
        BCC     FD7                     	; yep : skip
		
        LDA     SecPtr                   	; point track pointer at beginning of sector definition
        STA     TrackPtr                   
        LDA     SecPtr+1                 
        STA     TrackPtr+1                 
        BNE     FD2                     
		
.FD7                                    
        LDX     #0                      	; 256 bytes at end of track
        DEY                             
        LDA     (TrackPtr),Y               	; get byte to write
;        STA     VTEMP                   
;        LDA     DENSITY                 
;        BNE     FD11                    
;        JSR     FD11                    
;        INC     MEMPTR+1                   
;.FD11                                   
;        LDA     VTEMP                   	
.FD10    					                               
        STA     (MEMPTR),Y  				; write bytes to buffer               
        INY                             
        DEX                             
        BNE     FD10                    
        RTS     							; return to caller

; FM track layout data, each pair of bytes is a count plus byte to be written to the 
; in-memory formatted track.
;
;				Count	Byte
.TRFORMS                
		EQUB	$20,	$FF					; track preamble	
.GAP1S                                  
		EQUB	$06,	$00
		EQUB	$01,	$FE					; ID address mark
		EQUB	$01,	TrkMarker			; track no written here
		EQUB	$01,	$00					; head number written here
		EQUB	$01,	SecMarker			; sector (record) number written here
		EQUB	$01,	$01					; sector length, 00=128, 01=256, 02=512, 03=1024 bytes.
		EQUB	$01,	$F7					; CRC written here
		EQUB	$0B,	$FF					; gap 
		EQUB	$06,	$00					; gap
		EQUB	$01,	$FB					; data address mark
		EQUB	$00,	$E5					; 256 bytes of sector data, filled with E5	
		EQUB	$01,	$F7					; sector CRC written here
		EQUB	$12,	$FF					; sector runoff
		EQUB	$FF,	EndMarker			; track runnof / end marker

; sector ID's for correct interleave.		
.INTERLS                               
;		EQUB	      0,1,8,5,2,9,6,3,10,7,4          
		EQUB	0,4,7,1,5,8,2,6,9,3		; 1:3 interleave
.FOLDEND
    
;                       
; Verify command, also called after format.
;

.verifycom                                 
		jsr		START_MOTOR_SELECT			; select drive and turn it on
        JSR     RESTORE0             	; Restore to track 0
		JSR		MEMPTR_WKBASE			; Setup memory pointer

.VX3                                    
        LDA     #SECTRACK				; get sectors / track ;HSTSPT                  
        STA     TEMP+1                  
        JSR     PRINTSPACE              ; Print a space
        LDA     TRACKNO                 ; Get track number
        STA     WTRACK               	; send to WD 
        JSR     PRINT_HEXA                 ; print it in hex
        LDY     #0                      
        STY     TEMP                    

;        LDA     #(VERIFY_NMI)/256       ; setup NMI handler
;        STA     NMI+1                   
;        LDA     #(VERIFY_NMI)%256              
;        STA     NMI                     
        LDA     #5  					; 5 retries                    
        STA     VTEMP                   
		
.VX2                                    
        LDA     #WRDRSC       			; Read sector addresses           
		STA		WDCOMM					; Save command for NMI handler
        STA     WCMD                    ; Send to WD
.VX5                                    
        LDA     PCTRL					; Get control register
        BPL     VX5                     ; loop if no DRQ
        LDA     WDATA                   ; Get data from WD
        STA     (MEMPTR),Y              ; Save in memory
        INY                            	; increment pointer 
        BNE     VX5                     
		
.VERIFY_NMI                                    
        LDY     TEMP                    ; NMI 
        INY                             
        INY                             
        INY                             
        STY     TEMP                    
        LDA     DSTATUS                 ; get WD status
        AND     #$1C                    ; check for error
        BEQ     VX21                    ; none : skip
		
        DEC     VTEMP                   ; dec retry count
        BEQ     VX22                    ; All retries exhausted, skip on
        BNE     VX2                     ; try again

.VX21                                   
        DEC     TEMP+1                  ; decrement sector header count
        BNE     VX2                     ; still more : loop again
		
        LDY     #0                      ; Zero count / pointer
        TYA                             
.VX10                                   
        STA     VerifyBuf,Y             ; Zero out buffer  
        INY                             
        CPY     #SECTRACK		                  
        BNE     VX10                    
		
        LDY     #2              		; Start at offset 2 within sector address table        
.VX11                                   
        LDA     (MEMPTR),Y             	; get sector number from address    
        TAX                             
        STA     VerifyBuf,X				; put it in buffer
        INY                             
        INY                             
        INY                             
        INC     TEMP+1          		; increment sector count        
        LDA     #SECTRACK		     	; check done all?             
        CMP     TEMP+1                  
        BNE     VX11                    ; nope : loop again
		
        LDX     #0						; zero sector count                      
        STX     TEMP+1                  
.VX7                                    
        LDA     VerifyBuf+1,X     		; get sector id from buffer         
        BEQ     VX1                     ; zero 
        INC     TEMP+1                  ; increment sector count
.VX1                                    
        INX                             ; increment pointer
        CPX     #SECTRACK				; loop if more                  
        BNE     VX7                     
		
        LDA     #12                     
        STA     TEMP                    
        LSR     TEMP                    
.VX13                                   
        LDA     TEMP+1                  
        CMP     TEMP                    
        BCS     VX8                     
.VX22                                   
        LDA     #'?'    				; Error, print ?              
        JSR     OSWRCH                 	; write it
        JMP     VX83                   	; skip on
		
.VX8    JSR     PRINTSPACE              ; print space
.VX83   LDA     TRACKNO                 ; Get track number
        CMP     NTRC                    ; done all?
        BEQ     VX4                     ; yep : exit
        JSR     STEPIN                  ; step to next 
        JMP     VX3                     ; loop again
.VX4                                    
        JSR     INLINE_PRINT          	; Print success message
		EQUB	CR,LF,"VERIFIED",CR,LF      
        NOP                             
        JMP     RESTORE0              	; Restore to track 0 and exit

;
; Write track to disk 
;

.WRITE_TRACK                                
;        LDA     #(SWT4)/256				; Setup NMI handler             
;        STA     NMI+1                   
;        LDA     #(SWT4)%256             
;        STA     NMI                     
        LDA     #WWTRCC              	; Write track command 
        STA		WDCOMM					; Save command for NMI handler
		STA     WCMD                  	; Send it to WD
.SWT2                                   
        LDA     PCTRL 					; Get control register                   
        BPL     SWT2                    ; Loop if no DRQ
        LDA     (MEMPTR),Y            	; DRQ: send a byte to WD     
        STA     WDATA                   
        INY                             ; increment pointer
        BNE     SWT2                    
        INC     MEMPTR+1                   
        BNE     SWT2          
          
.WTRACK_NMI                                   
        JSR     ERRTYPE2               	; NMI breaks out here, handle error
        BCS     SWT7                    
.SWT6                                   
        STY     MEMPTR                 	; Display address of last byte written    
        LDA     MEMPTR+1                   
        JSR     PRINT_HEXA                   
        LDA     MEMPTR                     
        JSR     PRINT_HEXA                   
        JSR     PRINTSPACE              
        CLC                           	; Flag no error  
.SWT7                                   
        RTS                             

INDEX	= $8C
SAVEX	= $8D
SAVEWRC	= $8E

if (0)
.oscli
		lda		WRCVEC+1				; Save old WRCVEC
		sta		SAVEWRC+1
		lda		WRCVEC
		sta		SAVEWRC
		
		lda		#>NEWWRC				; Set new WRCVEC
		sta		WRCVEC+1
		lda		#<NEWWRC			
		sta		WRCVEC

		lda		#0						; Clear index
		sta		INDEX
		pla								; discard return
		pla
		jmp		$C334					; jump into interpreter print.
		
.NEWWRC		
endif 
.end_of_code

;Pad rest of rom with $FF, leaving last bytes for registers.
{
	start = P%
	for n, start, $EFEF
		equb $FF
	next 	
}

	print (start_of_regs - end_of_code), "bytes spare"

.start_of_regs
		EQUB	"PHILL WAS HERE!"

if (ISROM=1)
  if(IOBASE=$EFF0)
    SAVE"ados1770-EFF0.rom",$E000,$F000,$E000           
  else
    SAVE"ados1770-BC00.rom",$E000,$F000,$E000           
  endif
endif


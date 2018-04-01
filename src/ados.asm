; Ported to BeebASM 2015-06-22 Phill Harvey-Smith.

if(ISROM)
	include "../src/atomdefs.asm"
	include "../src/intelfdc.asm"
endif

;INCLUDEVDG	= 1
;SYS40 = 1

if (INCLUDEVDG <> 0)
if (SYS40 = 0)
; 80x25 Monochrome display
SCREENBASE	= &1000						; Screen RAM base
SCREENLINES	= 24						; number of lines on screen
SCREENCOLS	= 80						; number of columnns on screen
SCREENIO	= &1840						; Screen IO, address of 6845
SCRADDRREG	= SCREENIO + 0
SCRSTATUSREG= SCREENIO + 0
SCRDATAREG	= SCREENIO + 1
else
; 40x25 Colour Teletext display.
SCREENBASE	= &0400						; Screen RAM base
SCREENLINES	= 24						; number of lines on screen
SCREENCOLS	= 40						; number of columnns on screen
SCREENIO	= &0800						; Screen IO, address of 6845
SCRADDRREG	= SCREENIO + 0
SCRSTATUSREG= SCREENIO + 0
SCRDATAREG	= SCREENIO + 1
endif
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
endif

; i8271 regsiter base
IFDC8271	= $0a00
; read registers
IFDCSTATUS	= IFDC8271 + 0				; Status register
IFDCRESULT	= IFDC8271 + 1				; Result register
IFDCDATA	= IFDC8271 + 4				; Read/Write data register
; write registers
IFDCCMD		= IFDC8271 + 0				; Command register
IFDCPARAM	= IFDC8271 + 1				; Parameter register
IFDCRESET	= IFDC8271 + 2				; Reset register

; MAIN ENTRY point

        ORG     $E000                   
        GUARD   $F000                   

; Start of code
;

;============================================
; Initialise DOS controller
;--------------------------------------------

        lda     #$01                    ; HARDWARE Reset controller
        sta     IFDCRESET                ; HARDWARE
        lda     #$00                    ; Clear reset bit
        sta     IFDCRESET                ; HARDWARE

        jmp     LEEE2                   ; Interpreter entry

;============================================
; Print DISK on screen
;--------------------------------------------

.LE00D: jsr     INLINE_PRINT             ; Print "DISK "
        EQUB    "DISK "                 
        nop                             

;============================================
; Print text routine, end with char code >$7F or $00
; Zeropage $EA,$EB = txtpointer
;--------------------------------------------

.INLINE_PRINT  
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
        ldx     #FILENAMEPTR                  
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
.LE07F: iny                         	; increment pointer    
        lda     (FILENAMEPTR),Y         ; get a char from name        
        cmp     #$0D                    ; EOL?
        beq     LE067                   ; Name is legal
        
		cpy     #$07                    ; More than 7 chars?
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
        sta     $00,X                   ; Clear address
        sta     $01,X                   
        sta     $02,X                   ; Clear address given flag

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
        bne     LE0C1					; no : loop again                   

.LE0E9: lda 	$02,X             		; A=address position
        rts                             

;============================================
; Print space
;--------------------------------------------

.PRSPACE: 
		lda     #SPACE                    
        jmp     OSWRCH               

;============================================
; Print 6 spaces (entry PRSPACE6)
; Print Y speces (entry PRSPACEY)
;--------------------------------------------

.PRSPACE6: 
		ldy     #$06                    

.PRSPACEY: 
		jsr     PRSPACE                   ; Print space
        dey                             
        bne     PRSPACEY                   
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
        sec                             ; add 1 with carry
        sbc     #$01                    
        sta     STARTADDR            	; save in start addr         
        lda     LOADADDR+1              ; do MSB       
        sbc     #$00                    
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

.LE14C: jsr     copy_check                   ; Filename legal?
        jsr     FILEINCAT               ; Filename in cat?
        bcs     LE148                   

;============================================
; Print FILE? error
;--------------------------------------------

.LE154: jsr     INLINE_PRINT                   
        EQUB    "FILE?"                 
        brk                             

;============================================
; Check if filenaam in cat, carry set is yes
;--------------------------------------------

.FILEINCAT
.LE15D: jsr     LE223                   ; Check disk busy

        ldy     #$F8                    ; Init nameptr
.LE162: jsr     incy8                   ; INY 8x, move to next entry
        cpy     WKBASE + $0105          ; FilePointer
        bcs     LE18A         
          
        lda     WKBASE + $000F,Y        ; Qualifier from catalog
        and     #$7F                    ; Mask out protectionbit
        cmp     USEQUAL                   ; Same qual as we are looking for ?
        bne     LE162                   ; nope : skip to next file

        jsr     incy7                   ; INY 7x

        ldx     #$06                    ; Check filename in cat
.LE178: lda     WKBASE + $0007,Y                 
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
		lda     WKBASE + $000F,Y        ; Check file protection
        bmi     LE1AA                   ; Protected

.LE191: lda     WKBASE + $0010,Y        ; Remove filename from cat
        sta     WKBASE + $0008,Y                 
        lda     WKBASE + $0110,Y        ; Remove filedata
        sta     WKBASE + $0108,Y                 
        iny                             
        cpy     WKBASE + $0105          ; last file?
        bcc     LE191                   

        tya                             ; Decrement FilePointer
        sbc     #$08                    
        sta     WKBASE + $0105                   
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

.LE1B2  jsr     GET_CHK_FNAME                ; Check filename in cat
        jsr     print_info                   ; Print filenaam, start,link,lengte,sector
        jmp     set_qual_to_use         ; Set qual back if changed by USE

.print_info_ifmon
		lda     MONFLAG                 ; Check MON flag
        bne     LE230  					; nomon: exit                 

.print_info: 
		lda     WKBASE + $000F,Y        ; Load Qual
        and     #$7F                    ; Mask protectionbit
        jsr     OSWRCH                  ; Print Qual
        jsr     PRSPACE                 ; Print space

        ldx     WKBASE + $000F,Y        ; Load protectionbit
        bpl     LE1D1                   ; Not protected, print ' '
        lda     #'#'                    ; Protected, print #
.LE1D1: jsr     OSWRCH               	; Print karakter

        ldx     #$07                    ; Print filenaam
.LE1D6: lda     WKBASE + $0008,Y                 
        jsr     OSWRCH               
        iny                             
        dex                             
        bne     LE1D6                   

.LE1E0: jsr     PRSPACE                 ; Print space

        lda     WKBASE + $0102,Y        ; Print hex adres
        jsr     $F802                   
        lda     WKBASE + $0101,Y                 
        jsr     $F802                   
        iny                             
        inx                             
        iny                             
        cpx     #$02                    ; Repeat 3 times
        bcc     LE1E0                   

        jsr     PRSPACE                 ; Print space
        jsr     PRSPACE                 ; Print space

        lda     WKBASE + $0103,Y        ; Check file>64Kb
        jsr     diva16                  ; Divide by 16
        jsr     $F80B                   ; Print hex filelen
        lda     WKBASE + $0102,Y                 
        jsr     $F802                   
        lda     WKBASE + $0101,Y                 
        jsr     $F802                   

        jsr     PRSPACE              	; Print space

        lda     WKBASE + $0103,Y        ; Print hex sector
        jsr     $F80B                   
        lda     WKBASE + $0104,Y                 
        jsr     $F802                   

        jsr     OSCRLF                 


;============================================
; Read catalog if drive changed
;--------------------------------------------

.LE223: jsr     load_cat                ; Load catalog

; Loop until 8271 is ready
.WAIT_NOT_BUSY
.LE226: bit     IFDCSTATUS               ; Test status if busy
        bmi     WAIT_NOT_BUSY           ; 
        bit     IFDCSTATUS               ; Test status if busy
        bmi     WAIT_NOT_BUSY           ; 
.LE230: rts                              

;============================================
; DIR command
;--------------------------------------------
.dircom:                                

.LE231: jsr     drivecom             	; Check Drivenr given?
        jmp     LE223                   

;============================================
; CAT command
;--------------------------------------------
.catcom:                                

.LE237  jsr     dircom                  ; DIR

.catcom2  
		ldx     #$00                    
        stx     $B6                     
.LE23E: lda     WKBASE + $0000,X        ; Print title
        cpx     #$08                    
        bcc     LE248                   

        lda     WKBASE + $00F8,X                 

.LE248: jsr     OSWRCH               	; Print character
        inx                             
        cpx     #$0D                    ; End of title?
        bne     LE23E                   

        jsr     INLINE_PRINT            ; Print " DRIVE"
        EQUB    " DRIVE "               
        lda     DRIVENO                 ; Print drivenr
        jsr     $F80B                   ; Convert drive no to ASCII & print 

        jsr     INLINE_PRINT            ; Print " QUAL "
        EQUB    " QUAL "                
        lda     USEQUAL                   ; Print qual
        jsr     OSWRCH               

.LE26D: ldy     #$00                    
        jsr     LE288                   ; Check last filename
        bcc     LE2BE                   

.LE274: jsr     decy8                   ; DEY 8x
        lda     WKBASE + $0008,Y        ; Mask protectionbit
        and     #$7F                    
        sta     WKBASE + $0008,Y                 
        tya                             
        bne     LE274                   
        jmp     OSCRLF                 

.LE285: jsr     incy8                   ; INY 8x

.LE288: cpy     WKBASE + $0105          ; Check last filename?
        bcs     LE292                   
        lda     WKBASE + $0008,Y        ; Check protectionbit
        bmi     LE285                   
.LE292: rts                             

;--------------------------------------------
.LE293: ldy     CATEOLFLAG              ; Check EOL flag
        beq     LE29C                   ; no EOL yet, skip
        jsr     OSCRLF                  ; print EOL

        ldy     #$FF                    ; RE-init EOL flag
.LE29C: iny                             ; 
        sty     CATEOLFLAG              ; save EOL flag       
        jsr     PRSPACE6                ; Print 6 spaces

.LE2A2: lda     #'#'                    ; Assume file locked 
        ldy     CATINDEX                ; Get index
        ldx     WKBASE + $000F,Y        ; Check if file locked
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
; Sort the directory alphabetically.
;--------------------------------------------

.LE2BE: sty     CATINDEX                ; Save catalog index
        ldx     #$00                    ; A2 00
		
.LE2C2: lda     WKBASE + $0008,Y        ; get first character of pointed to filename
        and     #$7F                    ; make sure it's 7 bit ascii
        sta     CATFILENAME,X           ; copy to saved name
        iny                             ; increment pointers
        inx                             ; 
        cpx     #$08                    ; all characters done ?
        bne     LE2C2                   ; no : continue copying
		
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
        jsr     PRSPACEY                   ; 20 F3 E0
        sty     CATEOLFLAG                     ; 84 B8
        beq     LE2A2                   ; F0 89

.LE319: lda     WKBASE + $010E,Y        ; B9 0E 21
        jsr     diva16                   ; 20 FB E0
        sta     STARTSEC                     ; 85 A2
        clc                             ; 18
        lda     #$FF                    ; A9 FF
        adc     WKBASE + $010C,Y        ; 79 0C 21
        lda     WKBASE + $010F,Y        ; B9 0F 21
        adc     WKBASE + $010D,Y        ; 79 0D 21
        sta     STARTSEC+1                   ; 85 A3
        lda     WKBASE + $010E,Y        ; B9 0E 21
        and     #$0F                    ; 29 0F
        adc     STARTSEC                   ; 65 A2
        sta     STARTSEC                   ; 85 A2

.LE338: sec                             ; Entry save command
        lda     WKBASE + $0107,Y        ; B9 07 21
        sbc     STARTSEC+1                   ; E5 A3
        pha                             ; 48
        lda     WKBASE + $0106,Y        ; B9 06 21
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

.COMMAND_TABLE: EQUB    "CAT",    >catcom,<catcom
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
if (INCLUDEVDG <> 0) 
        EQUB    "VDU",    >vducom,<vducom
else
		EQUB	"VDU",	  >LE067,<LE067		; just a convenient RTS
endif
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
.LE3E5  ldx     #$FF                    ; Point to before start of command table
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
        lda     COMMAND_TABLE,X         ; is it <$80, so ascii?
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
        jsr     WAIT_NOT_BUSY           ; Wait until FDC not busy
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
		php                             ; Save flags
        jsr     LE484                   ; 20 84 E4
.LE47B: plp                             ; Restore flags
        bcc     LE481                   ; 
        jsr     WAIT_NOT_BUSY           ; Wait for controller
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
		jsr     copy_for_read           ; Copy reader into low ram
        lda     #ICMD_READ_MULTI+ICMD_DRIVE0	; Multi sector read $53
		
; Code between LE4A8 - E4C4 shared by LODVEC and SAVVEC	
.LE4A8: sta     FDCSAVCMD               ; Save command executing

        jsr     calc_start_addr         ; Calculate start address, number of sectors etc
.LE4AD: jsr     LE816                   ; Recalculate track and sector
        beq     LE4C4		            ; All done : exit
		
.LE4B2: jsr     set_memptr              ; Set MEMPTR from STARTADDR, place to load at

        lda     FDCSAVCMD               ; Get FDC command to execute
        jsr     read_write_sectors     ; Send it to the FDC
        jsr     HANDLE_ERROR            ; Process any errors
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
        lda     #$00                    ; Drive = 0
        sta     EXECADDR                 
        jsr     readcat                 ; Read catalog
        
		ldx     #FILENAMEPTR                    
        jsr     copy_check              ; Filename legal?
        jsr     FILEINCAT               ; Filename in cat?
        bcs     LE4F4                   ; Yes : load and exec
		
        jsr     LE500                   ; Restore drive & qualifier
        jsr     INLINE_PRINT            ; Print error and exit
        EQUB    "COMMAND?"              
        brk                             

.LE4F4: jsr     LE487                   ; 
        jsr     WAIT_NOT_BUSY           ; Wait for FDC
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
.LE510: jsr     WAIT_NOT_BUSY           ; Wait for disk
        jmp     (EXECADDR)              ; Execute code

.LE516: jmp     LE154                   ; Print "FILE?" error

;============================================
;EXEC COMMAND
;--------------------------------------------
.execcom:                               

.LE519: jsr     CHECK_FNPARAM           ; Check end of command
        jsr     OSFIND                  ; Find / open the file
        tay                             ; 
        beq     LE516                   ; Not found : exit
		
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
        
		lda     #<LE559                 ; Redirect WRCH to send to SPOOL channel
        ldy     #>LE559                 
        
		bne     LE52B                   ; no error : go do it
		
.LE559  sty     INBUFIDX                ; Save input buffer index
        ldy     SPOOLHANDLE             ; Get handle for spool file
        jsr     OSBPUT                  ; Put the byte to the SPOOL channel
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
        beq     write_cat_resqual       ; Yep : write back to disk
		
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
        jsr     GET_CHK_FNAME           ; Check filename in cat  
        lda     USEQUAL                 ; Get qualifier 
        rol     A                       ; Shift top bit to carry
        plp                             ; restore flags (lock falg in carry)
        ror     A                       ; Shift carry back to qual byte
        sta     WKBASE + $000F,Y        ; put back in catalog
        jsr     print_info_ifmon        ; print file info

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

; Check for no more parameters
; ----------------------------
.CHECK_PARAM
.LE5CC: jsr     SKIPSPACE               ; Skip spaces
        cmp     #$0D                    ; End of command?
        beq     LE5C8                   ; RTS

.LE5D3: jsr     INLINE_PRINT            ; Print SYNTAX? error
        EQUB    "SYNTAX?"               
        brk                             

.DISK_FULL: 
		jsr     INLINE_PRINT            ; FULL error
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
        ldx     #FILENAMEPTR            ; Flag Disk.
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

.LE667  sty     TEXTPTR                 ; Move filedata 8 bytes up
        ldy     WKBASE + $0105          ; AC 05 21
.LE66C: cpy     TEXTPTR                 ; Done all ?
        beq     LE67F                   ; Yes : exit
		
        lda     WKBASE + $0007,Y        ; Move bytes
        sta     WKBASE + $000F,Y        
        lda     WKBASE + $0107,Y        
        sta     WKBASE + $010F,Y        
        dey                             ; decrement pointer
        bcs     LE66C                   ; loop for next

.LE67F: ldx     #$00                    ; Copy new filename in cat
.LE681: lda     FILENAME,X              ; Get a byte from name
        sta     WKBASE + $0008,Y        ; put in cat
        iny                             ; increment pointers
        inx                             
        cpx     #$08                    ; Done all ?
        bne     LE681                   ; nope : loop again

.LE68C: lda     FILENAMEPTR+1,X                 ; B5 9B
        dey                             ; 88
        sta     WKBASE + $0108,Y        ; 99 08 21
        dex                             ; CA
        bne     LE68C                   ; D0 F7

        jsr     print_info_ifmon         ; Print fileinfo

        pla                             
        sta     LOADADDR+1              
        pla                            
        sta     LOADADDR                  

        ldy     WKBASE + $0105          ; Update filecounter (+8)
        jsr     incy8                   
        sty     WKBASE + $0105          

        jsr     write_cat               ; Write catalog
        jmp     WAIT_NOT_BUSY           ; Wait for it to finish

.LE6AD: jsr     LE61A                   ; 20 1A E6
.ROM_WRITE_SECTORS: 
		jsr     copy_for_write          ; Copy write routine into RAM
        lda     #ICMD_WRITE_MULTI + ICMD_DRIVE0	; Write multiple drive 0 
        jmp     LE4A8                   

if (INCLUDEVDG <> 0) 
;============================================
; VDU COMMAND
; Free for usage without VDU-expansion board
; $E6B8 - $E6FE
;--------------------------------------------
.vducom:                                

.LE6B8  jsr     get_loadadr             ; Read adres
        php                             
        jsr     CHECK_PARAM             ; Check end of command
        ldx     #$00                    ; Reset vectors to normal VDG values 
        plp                             
        beq     LE6CA                   ; Set vectors
		
        lda     LOADADDR                ; Get supplied parameter
        beq     LE6CA                   ; Zero, yes reset
        ldx     #$04                    ; Otherwise new vectors
		
.LE6CA: lda     VDUVECS,X               ; Update WRCVEC
        sta     WRCVEC                  
        lda     VDUVECS+1,X             
        sta     WRCVEC+1                
		
        lda     VDUVECS+2,X             ; Update RDCVEC
        sta     RDCVEC                  
        lda     VDUVECS+3,X             
        sta     RDCVEC+1                
      
		jsr     INLINE_PRINT            ; print signon message
        EQUB    $06,$0F,$0C             
        EQUB    "ACORN ATOM"            
        EQUB    $0A,$0A,$0D             
        nop                             ; And return.
        rts                             

.VDUVECS 
		EQUB    <$FE52, >$FE52                     
.LE6F9: EQUB    <$FE94, >$FE94                     
		EQUB    <VDU_WRCVEC, >VDU_WRCVEC
		EQUB	<VDU_RDCVEC, >VDU_RDCVEC
endif

;--------------------------------------------
; Initialise drive for loading catalog
;--------------------------------------------
.init_fdc_lcat
.LE6FF: lda     #$00                    ; Zero track and sector numbers
        sta     TRACKNO                  
        sta     SECTORNO                 
        lda     #$02                    ; Catalog is 2 sectors
        sta     SECCOUNT                
        ldy     #(LE878-LE862)          ; Offset of seek command
		
        lda     #<LE729                 ; Setup INT handler
        sta     INTJMP                  
        lda     #>LE729					
        sta     INTJMP+1                
		
.LE713: lda     LE862,Y                 ; Get command byte
        jsr     fdc_send_cmd_drv        ; send it
		
.LE719: iny                             ; Increment pointer
        lda     LE862,Y                 ; get next cmd / param
        cmp     #$EA                    ; End ?
        beq     LE727                   ; Yep : skip out
		
        jsr     SEND_PARAM_BYTE         ; Send drive param
        jmp     LE719                   ; do next
.LE727: iny                             ; increment pointer & exit
        rts                             

;--------------------------------------------
.LE729
        jsr     get_fdc_result          ; Get result
        lda     #RETRIES                ; Reset retry count
        sta     RETRYCOUNT              
        rts                             

;--------------------------------------------
; Load catalog
;--------------------------------------------

.load_cat
.LE731: jsr     TESTREADY               ; Check drive ready
        bne     LE749                   ; ready, return, don't need to re-read.

.load_cat_always		
        jsr     START_MOTOR_SELECT             ; Start drive motor and select drive
        jsr     init_fdc_lcat           ; Init FDC for cat load
		
.LE73C: jsr     copy_for_read           ; Copy read routine into RAM
        lda     #ICMD_READ_MULTI+ICMD_DRIVE0
										; Read multiple sectors drive 0
        jsr     read_write_sectors     ; Go read them
        jsr     HANDLE_ERROR            ; Handle any errors.
        bne     LE73C                   ; no errors, loop until all read.
.LE749: rts                             

; Write catalog
; Assumes drive is started and selected.
.write_cat: 
		jsr     init_fdc_lcat           ; Init FDC for cat load
.LE74D: jsr     copy_for_write          ; copy send bytes to disk into lowram
        lda     #ICMD_WRITE_MULTI+ICMD_DRIVE0
										; Write multiple to drive 0
        jsr     read_write_sectors     ; Send the commnand
        jsr     HANDLE_ERROR            ; Handle any errors.
        bne     LE74D                   ; no errors, loop until all written
        rts                             

;--------------------------------------------
; Start motor                       HARDWARE
;--------------------------------------------

.START_MOTOR_SELECT: 
		lda     DRIVENO                 ; Get drive no
        and     #$03                    ; Mask out all but driveno
        tay                             
        ora     #MOTOR_ON               ; Flag drive running
        sta     DRIVENO                 ; update driveno

        lda     #ICMD_WRITE_SPEC         ; Write special reg command
        jsr     fdc_send_cmd            
        lda     #ISR_DRV_CTL_OUT         ; Drive control output register
        jsr     SEND_PARAM_BYTE          
        lda     LE78E,Y                 ; Get parameter byte
        jsr     SEND_PARAM_BYTE         ; Send it
.LE774: jsr     TESTREADY               ; Wait for drive ready
        beq     LE774                   
        rts                             

;--------------------------------------------
; Controler drive status
;--------------------------------------------

.TESTREADY: 
		lda     DRIVENO                 ; Check if catalog must be read
        bpl     LE78B                   ; If ms bit set, then drive cat in memory

        lda     #ICMD_READ_STATUS+ICMD_DRIVE0                    
										; Read drive status
        jsr     fdc_send_cmd_drv        ; Send it to the FDC
        jsr     get_fdc_result          ; Get result
        bcc     LE78B                   ; drive 0....
        jsr     diva16                  ; a=a/16,
.LE78B: and     #$04                    ; Test drive ready

        rts                             

; parameter bytes for starting drive motors.
.LE78E:
		EQUB	$48						; Select drive 0 ($40), side 0 ($00), Head load ($08)
		EQUB	$88						; Select drive 1 ($80), side 0 ($00), Head load ($08)
		EQUB	$68						; Select drive 0 ($40), side 1 ($20), Head load ($08)
		EQUB	$A8						; Select drive 1 ($80), side 1 ($20), Head load ($08)
		
;--------------------------------------------
; Copy interrupt load- save routine
;--------------------------------------------

.copy_for_read
.LE792: ldy     #$0A                    ; Offset of end of read handler
        bne     LE798                   ; Skip if z<>0

.copy_for_write
.LE796: ldy     #$12                    ; Offset of end of write handler

.LE798: ldx     #$0B                    ; Byte count
.LE79A: lda     NMIREAD,Y               ; Get byte
        sta     TRANSFER-1,X            ; Save in low ram
        dey                             ; decrement counts
        dex                             ; 
        bne     LE79A                   ; Loop if more
.LE7A3: rts                             ; 

;--------------------------------------------
; Controller error handler
;--------------------------------------------

.HANDLE_ERROR: 
		jsr     get_fdc_result          ; Get command result from FDC
        beq     LE7A3                   ; No error : return

        cmp     #$12                    ; Write protect
        bne     LE7B5                   ; Nope : skip
        jsr     LE00D                   ; Print 'DISK '
        EQUB    "PROT"                  
        brk                             

.LE7B5: cmp     #$16                    ; Write fault
        bne     LE7BE                   ; Nope : skip
        jsr     LE00D                   ; Print "DISK "
        EQUB    "?",$00                 

.LE7BE: dec     RETRYCOUNT              ; Decrement retry count
        bne     LE7A3                   ; more retries left, return
        pha                             ; Save error code 
        jsr     LE00D                   ; Print "DISK "
        EQUB    "ERROR "                
        nop                             

        pla                             ; Recover error code
        jsr     PRINT_HEXA              ; print it
        brk                             

;--------------------------------------------
; Send command to controler         HARDWARE
;--------------------------------------------
; Commands should enter this routine with the drive set to 0
; this routine checks to see which drive should be accessed
; and changes the drive select bits as needed.
; Carry set on exit indicates drive 1, clear indicates drive 0.

.fdc_send_cmd_drv
.LE7D2: pha                             ; Save command
        lda     DRIVENO                 ; Get driveno
        ror     A                       ; Move drive number into carry
        pla                             ; Restore command
        bcc     fdc_send_cmd            ; Carry is clear, then we are accessing drive 0, so skip
        eor     #ICMD_DRIVE0+ICMD_DRIVE1  ; Carry set, flip to drive 1

; Send FDC command in A register.

.fdc_send_cmd: 
		bit     IFDCSTATUS               ; Check id 8271 busy
        bmi     fdc_send_cmd             ; Keep waiting if busy
        sta     IFDCCMD                  ; Send command to FDC
        rts                             

;--------------------------------------------
; Read result register               HARDWARE
;--------------------------------------------

.get_fdc_result
.LE7E4: lda     IFDCSTATUS               ; Wait until 8271 not busy
        bmi     get_fdc_result           
        lda     IFDCRESULT               ; Read result from 8271
        rts                             

;--------------------------------------------
; Command and parameter routine
;--------------------------------------------

.read_write_sectors
.LE7ED: jsr     fdc_send_cmd_drv        ; Send command in a to FDC
        
		clc                             ; Make sure carry flag clear before arith
        pla                             ; get LSB of return address
        adc     #$01                    ; add 1 as return address on stack is -1
        sta     INTJMP                  ; Save in indirect vector
        pla                             ; get MSB of return address
        adc     #$00                    ; take care of any carry
        sta     INTJMP+1                ; Save in indirect vector
		
        lda     TRACKNO                 ; Get track number, send it 
        jsr     SEND_PARAM_BYTE         
        lda     SECTORNO                ; Get sectornumber, send it
        jsr     SEND_PARAM_BYTE         
        lda     SECCOUNT                ; Get sector count, send it
        ora     #$20                    

.SEND_PARAM_BYTE
.LE809: pha                             ; Save param byte on stack
.LE80A: lda     IFDCSTATUS               ; Get 8271 status
        and     #$20                    ; Mask out bits
        bne     LE80A                   ; Loop if not ready for param
        pla                             ; Recover param byte from stack
        sta     IFDCPARAM                ; Send parameter to 8271
        rts                             

;--------------------------------------------
; Recalculate track and interrupt routine
; Recalculate track and sector interrupt routine
;--------------------------------------------

.LE816: lda     #$0A                    ; Reset retry count
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

;============================================
; NMIVEC LOAD SUBOUTINE copied into low ram
;--------------------------------------------

.NMIREAD
.LE84F: lda     IFDCDATA                ; Get data from FDC
        sta     WKBASE-1                ; Save in RAM
        pla                             
        rti                             
.NMIREADEND

NMIREADLEN	= NMIREADEND - NMIREAD

;============================================
; NMIVEC SAVE SUBOUTINE copied into low ram
;--------------------------------------------

.NMIWRITE
        jmp     $00F5                   ; 4C F5 00
        lda     WKBASE-1                ; Get data from RAM
        sta     IFDCDATA                 ; Send to controller
        pla                             
        rti                             
.NMIWRITEEND
NMIWRITELEN	= NMIWRITEEND-NMIWRITE


;--------------------------------------------
; Table for initialising controler  HARDWARE
;--------------------------------------------

.LE862: EQUB    ICMD_INIT_8271			,$0D				; Init 8271, $0d, 
		EQUB	$14											; step time (ms)				
		EQUB	$05											; settle time (ms)
		EQUB	$CA											; index count before unload (msn) head load time (lsn)
		EQUB	$EA  
        
		EQUB    ICMD_INIT_8271			,$10,$FF,$FF,$00,$EA ; Load bad tracks drive 0
        EQUB    ICMD_INIT_8271			,$18,$FF,$FF,$00,$EA ; load bad tracks drive 1
        EQUB    ICMD_WRITE_SPEC			,$17,$C1,$EA         ; Write special reg 17, DMA mode.
.LE878  EQUB    ICMD_SEEK + ICMD_DRIVE0	,$00,$EA             ; Seek to track 0

;============================================
; NMIVEC SUB
;--------------------------------------------

.NEWNMIVEC
.LE87B  lda     IFDCSTATUS              ; Read status regsiter
        and     #$04                    ; Check data ready?
        beq     LE88B                   ; Yep skip
		
        inc     MEMPTR                  ; Increment src/dest ptr LSB
        bne     LE888                   ; No overflow skip
		
        inc     MEMPTR+1                ; Increment src/dest ptr MSB
.LE888: jmp     TRANSFER                ; Call transfer routine

.LE88B: txa                             ; Save regs on stack
        pha                              
        tya                             
        pha                             
        cld                             
        jsr     LE899                   ; Call int handler
        pla                             ; Restore regs from stack
        tay                             
        pla                             
        tax                             
        pla                             
        rti                             

.LE899: jmp     (INTJMP)                 ; Jump to int handler

;============================================
; START RANDOM ACCESS ROUTINES
;============================================

;--------------------------------------------
; *SHUT command
;--------------------------------------------
.shutcom:                               

.LE89C: ldy     #$00                    ; A0 00
.NEW_SHTVEC: pha                             ; 48
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
.NEW_FNDVEC  cld                             ; D8
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
.NEW_RDRVEC: pha                             ; 48
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

.NEW_BGTVEC: cld                             ; D8
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
.LEBB2: jsr     WAIT_NOT_BUSY                   ; 20 26 E2
        ldy     $00C2                   ; A4 C2
.LEBB7: rts                             ; 60
.LEBB8: pla                             ; 68
        jmp     OSASCI                 ; 4C E9 FF
.NEW_BPTVEC: cld                             ; D8
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
        jsr     WAIT_NOT_BUSY                   ; 20 26 E2
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

if (INCLUDEVDG <> 0) 
;============================================
; VDU ROUTINES
;--------------------------------------------

.VDU_RDCVEC: 
		php                             ; 08
        cld                             ; D8
        stx     $00E4                   ; 86 E4
        sty     $00E5                   ; 84 E5
.LED28: bit     $b002                   ; 2C 02 B0
        bvc     LED32                   ; 50 05
        jsr     GETKEY                  ; 20 71 FE
        bcc     LED28                   ; 90 F6
.LED32: jsr     $FB8A                   ; 20 8A FB
.LED35: jsr     GETKEY                  ; 20 71 FE
        bcs     LED35                   ; B0 FB
        jsr     GETKEY                  ; 20 71 FE
        bcs     LED35                   ; B0 F6
        tya                             ; 98
        ldx     #$17                    ; A2 17
        jsr     $FEC5                   ; 20 C5 FE
        lda     LED48,X                 ; BD 48 ED
.LED48: sta     $00E2                   ; 85 E2
        lda     LED55,X                 ; BD 55 ED
        sta     $00E3                   ; 85 E3
        tya                             ; 98
        jmp     ($00E2)                 ; 6C E2 00
		
        EQUB    $DF,$D2                 
.LED55: txs                             ; 9A
        dey                             ; 88
        EQUB    $E2                     
        sta     ($00C0,X)               ; 81 C0
        EQUB    $DF                     
        cld                             ; D8
        dec     $00C8,X                 ; D6 C8
        dec     $00C2                   ; C6 C2
        sbc     $FDFD,X                 ; FD FD FD
        sbc     $EDFD                   ; ED FD ED
        sbc     $FDFD,X                 ; FD FD FD
        sbc     $FDFD,X                 ; FD FD FD
        sbc     $0BA9,X                 ; FD A9 0B
        bne     LED7B                   ; D0 0A
        lda     #$08                    ; A9 08
        bne     LED7B                   ; D0 06
        lda     #$09                    ; A9 09
        bne     LED7B                   ; D0 02
        lda     #$0A                    ; A9 0A
.LED7B: jsr     LED97                   ; 20 97 ED
        jmp     VDU_RDCVEC                   ; 4C 22 ED
        ldy     #$00                    ; A0 00
        lda     ($00D2),Y               ; B1 D2
        jmp     $FE60                   ; 4C 60 FE
        and     #$05                    ; 29 05
        rol     $b001                   ; 2E 01 B0
        rol     A                       ; 2A
        jsr     LEE1F                   ; 20 1F EE
        jmp     LED28                   ; 4C 28 ED

.VDU_WRCVEC
		jsr     $FEFB                   ; 20 FB FE
.LED97: php                             ; 08
        pha                             ; 48
        cld                             ; D8
        sty     INBUFIDX                   ; 84 E9
        jsr     LEE1F                   ; 20 1F EE
        pla                             ; 68
        ldy     INBUFIDX                   ; A4 E9
        plp                             ; 28
        rts                             ; 60

.LEDA4: pla                             ; Restore & exit
        pla                             
        rts                             
		
; Move cursor position back one
; -----------------------------
.LEDA7: dey                             
        bpl     LEDBE                   ; Dec. X position, return if still positive
		
		LDY 	#SCREENCOLS-1          	; Set X position to last column of previous line
.LEDAC: 
		LDA 	LINESCROLL             	; Get lines left before scrolling
		CMP 	#SCREENLINES       		; Screen height  
        bcs     LEDA4                   ; B0 F2

		INC 	LINESCROLL              ; Increment lines before scroll
.LEDB4: 		LDA 	&CF             		
		SBC 	#SCREENCOLS-1           
		STA 	&CF             
		bcs     LEDBE                   
        
		dec     SCREENTOP               
.LEDBE: rts                             
.LEDBF: dec     LINESCROLL              
        rts                             

; Do linefeed and scroll if needed
; --------------------------------
.LEDC2: LDY 	#SCREENCOLS
		jsr     LEDF6                   ; Calculate address of character past end of line
										; This will be column zero of the next line
										
		LDA 	SCREENADDR
		STA 	&CF
		LDA 	SCROFFSET

		STA 	SCREENTOP
		LDA 	LINESCROLL
        bne     LEDBF                   
		
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
.LEDEF: 
		jsr     LEE0B                   ; Store space character
        dey                             
        bpl     LEDEF                   ; Loop for whole line
        rts                             

; Calculate screen output address
; -------------------------------
; On entry, Y     = X position (from &D1)
;           &CF   = ?
; On exit,  &D2/D3=>output address
;           &D2/D4= offset into screen memory
;
.LEDF6: PHA
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
.LEE0B: 
		jsr     LEDF6                   ; Calculate output address
		STY 	SCROFFSET           	; Save Y
		LDY 	#&00
		STA 	(SCREENADDR),Y     		; Store character in screen memory
		LDY 	SCROFFSET
		RTS              				; Restore Y and return
		
.LEE17: 
		clc                             ; Clear carry
.LEE18: 
		php                             ; save flags  
        asl     SCREENXPOS              ;
        plp                             ;
        ror     SCREENXPOS              ; set b7=0
        rts                             

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
.LEE1F: cmp     #$06                    
        beq     LEE17                  	; <ACK> - turn on VDU
		
        cmp     #$15                    
        beq     LEE18                   ; <NAK> - turn off VDU
		
        ldy     SCREENXPOS              
        bmi     LEE58                   ; Get X position, b7=VDU turned off, exit without printing
		
        cmp     #$20                    
        bcc     LEE63                   ; less than <SPC> - control charater
		
        cmp     #$7F                    
        beq     LEE59                   ; <DEL> - delete character

; Print a printable character on screen
; -------------------------------------
.LEE33: jsr     LEE0B                   ; Store character in screen memory

; VDU 9 - Cursor right
; --------------------
.LEE36: INY
		CPY 	#SCREENCOLS
		bcc     LEE40                   ; Incrment X, skip if not reached end of line
		
        jsr     LEDC2                   ; Do a linefeed and scroll if needed

; VDU 13 - Carriage Return
; ------------------------
.LEE3E: 
		ldy     #$00                    ; Make X position = 0
.LEE40: jsr     LEDF6                   ; Calculate new character address
		STY 	SCREENXPOS              ; Store updated X position
		LDY 	#CPOSL
		STY 	SCRADDRREG       		; Select CTRC Reg 15 - cursor address low
		LDA 	SCREENADDR
		STA 	SCRDATAREG        		; set it
		DEY
		STY 	SCRADDRREG            	; Select CRTC Reg 14 - cursor address high
		LDY 	SCROFFSET
		STY 	SCRDATAREG        		;
.LEE58: rts                             ; 60

; VDU 127 - Delete
; ----------------
.LEE59: jsr     LEDA7                   ; Move cursor back one
        lda     #$20                    
        jsr     LEE0B                   ; Store <SPC> in current position
        bpl     LEE40                   ; Jump to update cursor position

; Control characters
; ------------------
.LEE63: cmp     #$0D                    
        beq     LEE3E                   ; <CR>

        cmp     #$0A                    
        beq     LEE8B                   ; <DOWN>

        cmp     #$0C                    
        beq     LEEAA                   ; <CLS>

        cmp     #$08                    
        beq     LEE85                   ; <LEFT>

        cmp     #$1E                    
        beq     LEE93                   ; <HOME>

        cmp     #$0B                    
        beq     LEEA4                   ; <UP>

        cmp     #$07                    ; C9 07
        beq     LEEDA                   ; F0 5B

        cmp     #$09                    
        bne     LEE33                   ; Not <RIGHT> - store in screen memory
        bcs     LEE36                   ; <RIGHT>
		
; VDU 8 - Cursor left
; -------------------	
.LEE85: jsr     LEDA7                   ; Move cursor back one
        jmp     LEE40                   ; Jump to update cursor position

; VDU 10 - Cursor down
; --------------------
.LEE8B: jsr     LEDC2                   ; Do a linefeed and scroll if needed
        ldy     SCREENXPOS              ; Get X position
        jmp     LEE40                   ; Jump to update cursor position

; VDU 30 - Cursor home
; --------------------

.LEE93: 
		LDA 	#SCREENLINES			; Setup lines till scroll
		LDY 	LINESCROLL    			; get current value         	
		STA 	LINESCROLL             	; reset it
.LEE99: 
		CPY 	#SCREENLINES            ; Where we currently on the top line ?
        bcs     LEE3E                   ; yes : just do a carrage return  
        
		iny                             
        jsr     LEDB4                   ; cursor back one
        jmp     LEE99                   

; VDU 11 - Cursor up
; ------------------
.LEEA4: jsr     LEDAC                   ; Jump to update cursor position
        jmp     LEE40                   
		
	
; VDU 12 - CLS
; ------------
.LEEAA: 
		LDA 	#' '              		; Fill screen with space characters
		LDY 	#&00                 	; Loop through each page of screen

.LEEAE: 
		STA 	SCREENBASE + &0000,Y	; Fill screen memory with spaces
		STA 	SCREENBASE + &0100,Y  				
		STA 	SCREENBASE + &0200,Y
		STA 	SCREENBASE + &0300,Y
if (SYS40 = 0)
		STA 	SCREENBASE + &0400,Y
		STA 	SCREENBASE + &0500,Y
		STA 	SCREENBASE + &0600,Y
		STA 	SCREENBASE + &0700,Y
endif
        iny                             
        bne     LEEAE                   
		
		STY 	&CF
		STY 	SCREENXPOS

		LDY 	#&0D                 	; Set up CRTC registers
.LEEC3: 
		STY 	SCRADDRREG              ; Select CRTC register
		LDA 	CRTCREGS,Y
		STA 	SCRDATAREG    			; Write CRTC data from CRTC table
		DEY
        bpl     LEEC3                   ; Loop for all CRTC registers
        
		LDA 	#SCREENLINES			; Setup lines till scroll
		STA 	LINESCROLL
if (SYS40 = 0)
		LDA 	#&10
else
		LDA 	#&04
endif
		STA 	SCREENTOP      			; &D0=&10xx - start of screen
        jmp     LEE3E                   ; 4C 3E EE
		
.LEEDA: stx     $00D2                   ; 86 D2
        jsr     $FD1A                   ; 20 1A FD
        ldx     $00D2                   ; A6 D2
        rts                             ; 60
endif

;============================================
; DOS Interpreter entry
;--------------------------------------------

.LEEE2: ldx     #VECOFS                 ; point at LODVEC
.LEEE4: lda     VECTBL-VECOFS,X         ; get byte from ROM
        sta     VECBASE,X               ; put in vector table
        inx                              
        cpx     #VECTBLLEN+VECOFS       ; done all?
        bne     LEEE4                   ; no : keep going
		
        lda     #<NEWNMIVEC             ; Setup new NMI vector
        sta     NMIVEC                  
        lda     #>NEWNMIVEC                 
        sta     NMIVEC+1                
		
        lda     #<NEWCOMVEC             ; setup new command vec
        sta     COMVEC 
        lda     #>NEWCOMVEC             
        sta     COMVEC+1                

        lda     #SPACE                  ; A9 20
		
        sta     SETQUAL                     ; 85 CD
        sta     USEQUAL                   ; Current QUAL = space
        ldy     #$00                    ; Zero low ram vars & cmd ptr
		
		sty     DRIVENO                 ; Drive num
        sty     $C0                     ; 84 C0
        sty     EXECHANDLE                     ; 84 B9
        sty     SPOOLHANDLE                     ; 84 BA

        ldx     #$04                    ; Execute 4 FDC commands (y=ptr=0)
.LEF15: jsr     LE713                   ; Execute command
        dex                             ; Done all ?
        bne     LEF15                   ; Nope, loop again
        rts                             

if (INCLUDEVDG <> 0) 
;============================================
; VDU initialize
;--------------------------------------------
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
		EQUB 	&3F ;  0 Horizontal Total         =128
		EQUB 	&28 ;  1 Horizontal Displayed     =80
		EQUB 	&33 ;  2 Horizontal Sync          =&66
		EQUB 	&44 ;  3 HSync Width+VSync        =&62
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

endif 

;============================================
; DOS vectors $20C-$21B
;--------------------------------------------

.VECTBL EQUB    <NEW_LODVEC, >NEW_LODVEC      ; $20C LODVEC
        EQUB    <NEW_SAVVEC, >NEW_SAVVEC      ; $20E SAVVEC
        EQUB    <NEW_RDRVEC, >NEW_RDRVEC		; $210 RDRVEC
        EQUB    <NEW_STRVEC, >NEW_STRVEC      ; $212 STRVEC
        EQUB    <NEW_BGTVEC, >NEW_BGTVEC      ; $214 BGTVEC
        EQUB    <NEW_BPTVEC, >NEW_BPTVEC      ; $216 BPTVEC
        EQUB    <NEW_FNDVEC, >NEW_FNDVEC      ; $218 FNDVEC
        EQUB    <NEW_SHTVEC, >NEW_SHTVEC      ; $21A SHTVEC
.VECTBLEND

VECTBLLEN	= VECTBLEND - VECTBL
VECOFS		= LODVEC - VECBASE

;============================================
; Empty
;--------------------------------------------
.LEF3A  EQUB    $49,$43,$45,$53,$2e,$0d 

;.LEF40: EQUB    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;        EQUB    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;        EQUB    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;        EQUB    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;        EQUB    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;        EQUB    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;        EQUB    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;        EQUB    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;        EQUB    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;        EQUB    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;        EQUB    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;        EQUB    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

;Pad rest of rom with $FF
{
	start = P%
	for n, start, $EFFF
		equb $FF
	next 
	
}

if (ISROM=1)
  if (INCLUDEVDG=1)
    if (SYS40=1)
      SAVE"ados-40.rom",$E000,$F000,$E000           
    else
      SAVE"ados-40.rom",$E000,$F000,$E000           
    endif
  else
      SAVE"ados-novdg.rom",$E000,$F000,$E000           
  endif
endif 

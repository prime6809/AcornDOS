; GDOS 1.5 V.010286       
; GERRIT HILLEBRAND       
;
;        ASM-V                           
;        P."ASSEMBLINGGDOS 1.5"'         
;.OPTION.01010000                        
;.TABLE  $8200,$9000                     
;.CODE   $E000                           
;.RAM    $9000                           

; Converted to BeebASM 2015-06-11 Phill Harvey-Smith.
; Produces binary equivilent to SALFAA.

if(ISROM)
	include "../src/atomdefs.asm"
	include "../src/wdfdc.asm"
endif

; Set SIMPLEIO = 1 if using a smple IO port, set to 0 for PIA
SIMPLEIO	= 1

; Low RAM locations

NAME   		= $9A                            
LDADR  		= $9C                            
PEXEADR		= $9E                            
FILELEN		= $A0                            
MSB    		= $A2                            
STSCT  		= $A3                            
ENBIND 		= $A4                            
FLNM   		= $A5                            
CQUAL 		= $B4					; Current qualifier                            
             
WORK   		= $BB                            
SCTLEFT		= $BD                            
BCUAL 		= $BF                            
OSTEMP 		= $D7                            

MSGPNT 		= $EA                            
DTRCK  		= $EC                            
DSECT  		= $ED                            
DRNMB  		= $EE                                                      

DCOM   		= $F4         			; Currently executin command?
HSTSPT 		= $F5                   ; Current sectors / track
MEM    		= $F6                   ; memory pointer      

; Set this to 1 if using the WD1770 or WD1772, zero otherwise
ISWD177X	= 1

if (ISWD177X) 
FDC1797 	= 0 		              
else
; Set this to 0 if using 1793, 177x, 2793
; Set this to 1 if using the 1797 or 2797.
FDC1797 	= 0 		              
endif

STEPRATE	= $01                           
RESTC		= $08 or STEPRATE                     
SEEKC		= $1D or STEPRATE                     
RSCTC		= $80 or ((FDC1797)*8)                
WSCTC		= $A0 or ((FDC1797)*8)                
RTRCC		= $E4                              
WTRCC		= $F4                              
RDRSC		= $C0                              
STPIC		= $5B                              

; I/O locations WD179x / 177x / 279x

WDBASE		= IOBASE
STATUS 		= WDBASE+0		; Status / command register
CMD    		= WDBASE+0
TRACK  		= WDBASE+1 		; Track register                         
SECTOR 		= WDBASE+2		; Sector register                          
RDATA  		= WDBASE+3		; Read / Write Data register

; I/O locations 6821 PIA
; 6821 PIA is used for controling drives and FDC chip.
; It could be replaced with a simple IO port as no special 
; features of the chip are used.

PIABASE		= IOBASE+4                         
RDDA   		= PIABASE+0		; Data Direction A                         
RDA    		= PIABASE+0     ; Data A                     
RA     		= PIABASE+1     ; Control A                     
RDDB   		= PIABASE+2     ; Data Direction B                     
RDB    		= PIABASE+2     ; Data B                     
RB     		= PIABASE+3		; Control B
  
; PIA Data lines all on port A
PSIDE		= $01			; Side select mask
PDS0		= $02			; Drive select 0
PDS1		= $04			; Drive select 1
PDDEN		= $08			; Double density enable (low)
PRESET		= $10			; Reset FDC
P4080		= $20			; 40(low)/80(high) track switch
PINDEX		= $40  			; Index pulse????
PDRQ		= $80			; DRQ

PIAOUT		= PSIDE or PDS0 or PDS1 or PDDEN or PRESET or P4080

NMLNG		= 15 			; characters / filename ?                            
MAX_FILEN	= 14			; Maximum filename length	
MAX_TITLE	= 14			; Maximum title length
BEGINSCT	= 9                             
NMBFILES	= 78                            

; MAIN ENTRY point

		ORG		$E000
		GUARD	$F000
.DOS   
IF (SIMPLEIO = 1)                    
		LDA		#PRESET				; Switch off motors, deselect drives, MR high
		STA		RDA
ELSE
        LDA     RA					; Get control register A                      
        AND     #$FB				; Select DDRA             
        STA     RA   
        LDX     #PIAOUT				; set direction of lines             
        STX     RDDA                    
        ORA     #$04				; select Data register             
        STA     RA             
ENDIF
        JSR     MESSAGE             ; display signon message    

		EQUB	"GDOS-1.5",CR,LF,LF                   
        NOP     
                        
        LDA     #SPACE     	  		; set default qualifier to space
        STA     CQUAL                  
        STA     BCUAL                  
        STA     ENBIND                  
        
		JSR     PDD3                   
        LDA     STATUS                  
        LDA     #0   				; zero drive number                   
        STA     DRNMB                   
        STA     MONFLAG  			; and MON flag               
        TAX                             
.DS1                                    
        LDA     VECTAB,X			; change load and save vectors to ours                
        STA     LODVEC,X                  
        INX                             
        CPX     #4                      
        BNE     DS1       
              
        LDA     #(COMVCT)/256		; command line vector           
        STA     COMVEC+1                    
        LDA     #(COMVCT)%256           
        STA     COMVEC                    

; WD177x controllers *DON'T* seem to perform a Restore on reset, and so 
; don't generate an INTRQ, so we can setup the main NMI handler staight 
; away.
if (ISWD177X = 1)
        LDA     #(NMIROUT)/256    	; Change to main NMI routine      
        STA     NMIVEC+1                    
        LDA     #(NMIROUT)%256          
        STA     NMIVEC               
else		
        LDA     #(NMINI)/256		; NMI vector
        STA     NMIVEC+1                    
        LDA     #(NMINI)%256            
        STA     NMIVEC                    
endif		

        LDA     #PDS0				; Select drive 0, FDC reselt line low           
        STA     RDA                     
        LDX     #3        			; Delay a short while for reset              
        JSR     DELAY                   
        LDA     #PRESET OR PDS0		; Select drive 0, and reset high              
        STA     RDA                     
        JSR     FOUR       			; SETUP FOR 40 TRACK?             
        RTS         
                    
.VECTAB                                 
		EQUW	      LODVCT        ; new load vector                  
		EQUW	      SAVVCT    	; new save vector                      

; Display an inline message on the screen.

.MESSAGE                                
        PLA              			; get return address from STACKPAGE               
        STA     MSGPNT             	; and save in message pointer    
        PLA                             
        STA     MSGPNT+1                
		
        LDY     #0					; zero character count                      
        STY     RETRYCOUNT                 
.MX3                                    
        INC     MSGPNT            	; increment message pointer      
        BNE     MX1                     
        INC     MSGPNT+1                
.MX1                                    
        LDA     (MSGPNT),Y         	; get a character to print     
        PHA                         ; save it    
        EOR     RETRYCOUNT                 
        STA     RETRYCOUNT                 
        PLA                        	; restore char to print     
        BMI     MX2                	; > 128 or zero exit     
        BEQ     MX2                     
		
        JSR     OSWRCH            	; print char
        JMP     MX3                 ; do next    
.MX2                                    
        JMP     (MSGPNT)            ; return to caller    

.READNAME                               
        STY     TEMP                    
        JSR     STRBUFF		;FILL            
        JSR     STR			;DELMT               
        LDY     TEMP                    
        LDX     #0                      
        BEQ     GNX3                    

; moves keyboard input to buffer at $140, skiping leading and trailing spaces.

.STRBUFF					;FILL                           
        LDX     #(CMDLINE-STACKPAGE)	; offset of buffer within STACKPAGE                    
.GNX3                                   
        JSR     SKIPSPACE  			; skip leading spaces               
        STX     NAME                ; save name pointer    
        CMP     #DUBQUOTE          	; was last character a "     
        BEQ     QUOTEPART        	; yep : deal with it
       
.GNX2                                   
        CMP     #CR                	; EOL?
        BEQ     GNX1                ; yep : skip
    
        STA     STACKPAGE,X				; save in our buffer at $140                  
        INX                         ; increment pointers    
        INY                             
        LDA     STACKPAGE,Y            	; get next character      
        CMP     #SPACE              ; space ?    
        BNE     GNX2                ; nope continue getting chars    
.GNX1                                   
        LDA     #CR                 ; put EOL into buffer    
        STA     STACKPAGE,X                  
        LDA     #1                      
        STA     NAME+1                  
        LDX     #NAME                   
.GNX4                                   
        RTS                             

.STORELOCAL                             
        LDY     #0                      
.STX1                                   
        LDA     0,X                     
        STA     NAME,Y                  
        INX                             
        INY                             
        CPY     #10                     
        BCC     STX1                    
        LDY     #14                     
        LDA     #SPACE                  
.STX2                                   
        STA     FLNM,Y                  
        DEY                             
        BPL     STX2                    
.STX3                                   
        INY                             
        LDA     (NAME),Y                
        CMP     #CR                     
        BEQ     GNX4                    
        CPY     #NMLNG                  
        BCS     QX3                     
        CMP     #':'                  
        BNE     STX4                    
        CPY     #1                      
        BCC     STX5                    
        BNE     STX4                    
.STX5   LDA     FLNM                    
        STA     CQUAL                  
        LDA     #SPACE                  
        STA     FLNM                    
.STX6   INC     NAME                    
        BNE     STX7                    
        INC     NAME+1                  
.STX7   DEY                             
        BPL     STX6                    
        BNE     STX8                    
.STX4   STA     FLNM,Y                  
.STX8   BNE     STX3                    

.QUOTEPART                              
        INY                             
        LDA     $100,Y                  
        CMP     #CR                     
        BEQ     QX3                     
        STA     $100,X                  
        INX                             
        CMP     #DUBQUOTE               
        BNE     QUOTEPART               
        DEX                             
        INY                             
        LDA     $100,Y                  
        CMP     #DUBQUOTE               
        BNE     GNX1                    
        INX                             
        BCS     QUOTEPART               
.QX3                                    
        JSR     MESSAGE                 
		EQUB	"NAME OF FILE?"                  
        BRK                             

.STR						;DELMT                              
        JSR     STRBUFF		;FILL            

.DELMT                                  
        JSR     SKIPSPACE                 ; Skip spaces
        CMP     #CR                     ; EOL ?
		BEQ     LCHX1                   ; Yep : return
        
		CMP     #';'                    ; Next statement ?
        BEQ     LCHX1                   ; Yep : return
.SVX1                                   
        JSR     MESSAGE                 ; Print syntax error 
		EQUB	"SYNTAX?"                             
        BRK     
                        
.LCHX1                                  
        RTS                             

.PSV                                    
        JSR     STRBUFF		;FILL            
        JSR     EXLD                   
        BEQ     SVX1                    
        LDX     #MSB                    
        JSR     EXLD2                 
        BEQ     SVX1                    
        LDX     #PEXEADR                
        JSR     EXLD2                 
        PHP                             
        LDA     LDADR                   
        LDX     LDADR+1                 
        PLP                             
        BNE     SVX2                    
        STA     PEXEADR                 
        STX     PEXEADR+1               
.SVX2                                   
        STA     FILELEN                 
        STX     FILELEN+1               
        JSR     DELMT                   
        LDX     #NAME                   
        JMP     $FFDD                   

.NOMON                                  
        LDX     #$FF                    
.MON                                    
        JSR     DELMT                   
        STX     MONFLAG                 
.MONX1                                  
        LDA     BCUAL                  
        STA     CQUAL                  
        RTS                             

;
; *DRIVE command, also called by several other routines, to get & set driveno.
;
.DRIVE                                  
        JSR     SKIPSPACE                 ; Skip spaces
        CMP     #CR                     ; Current character EOL ?
        BEQ     GDX1                    ; Yep : exit
		
        INY								; increment command line ptr                             
        PHA                             ; save current char
        JSR     DELMT                   ; Check for EOL or next satement
        PLA                             ; retrieve current
		
        CMP     #'0'                  	; Validate drive no 0 - 3
        BCC     DRMESS                  ; error and exit if not
        CMP     #'4'                  
        BCS     DRMESS                  
.GDX2                                   
        AND     #7                      ; Mask out all but bottom bits (should this be 3?)
        STA     DRNMB                   ; save drive number
        LDA     RDA                     ; Get harware ctl register
        AND     #$F8                    ; mask out drive select & side
        ORA     DRNMB                   ; mask in drive number
        CLC                             ; clear carry
        ADC     #2                      ; convert drive no into physical drive and side.
        STA     RDA                     ; save back in hardware
.GDX1                                   
        RTS                             
.DRMESS                                 
        JSR     MESSAGE                 ; Print error and exit
		EQUB	"UNKNOWN DRIVE NUMBER"                 
        BRK                             

.LODVCT                                 
        PHP                             
        JSR     REALLD                  
        PLP                             
.LFR3                                   
        JSR     MONX1                   
        JMP     RESTORE0                  

.REALLD                                 
        JSR     LOOKUPNAME3            
.LFR1                                   
        LDY     #0                      
        LDA     PEXEADR                 
        BPL     LFR4                    
        INY                             
        INY                             
.LFR4                                   
        LDA     (TEMP2),Y               
        STA     LDADR,Y                 
        INY                             
        CPY     #8                      
        BNE     LFR4                    
        JSR     REPORT                  
        JSR     RESTORE                 
        JSR     TRSC                   
        JSR     RDSECT                 
.LFR7                                   
        RTS                             

;
; Set the disk title.
; 

.TITLE                                  
        JSR     STRBUFF		        ; get command line into buff    
        JSR     READTRCK0   		; Read track 0 to workspace            
        LDX     #0                  ; character pointer to 0
.TX2                                    
        LDA     CMDLINE,X         	; get a character from command line     
        CMP     #CR                	; EOL?     
        BEQ     TX3                	; yep : replace with space
    
        STA     WKBASE,X          	; save character in name       
        INX                         ; increment character count    
        CPX     #MAX_TITLE        	; max title len ?     
        BNE     TX2               	; no : do next
    
        JMP     LOX1               	; set title
.TX3                                    
        LDA     #SPACE            	; set next char to space
.TX4                                    
        STA     WKBASE,X            ; save in name     
        INX                         ; increment character count    
        CPX     #MAX_TITLE        	; max title len ?     
        BNE     TX4                 ; no keep padding
        JMP     LOX1               	; set title

.DELETE                                 
        JSR     LOOKUPNAME              
        JSR     REPORT                  
        JSR     REMDIR                  
        JMP     LOX1                    

.REMDIR                                 
        LDY     #NMLNG                  
        LDA     (TEMP1),Y               
        BMI     RDX1                    
.RDX2                                   
        LDY     #16                     
        LDA     (TEMP1),Y               
        LDY     #0                      
        STA     (TEMP1),Y               
        INC     TEMP1                   
        BNE     RDX2                    
        INC     TEMP1+1                 
        LDA     TEMP1+1                 
        CMP     #(WKHIGH + $05)		; $25                    
        BNE     RDX2                    
.RDX3                                   
        LDY     #8                      
        LDA     (TEMP2),Y               
        LDY     #0                      
        STA     (TEMP2),Y               
        INC     TEMP2                   
        BNE     RDX4                    
        INC     TEMP2+1                 
        BNE     RDX3                    
.RDX4                                   
        LDA     TEMP2                   
        CMP     #(WKLOW + $80)		; $80                    
        BNE     RDX3                    
        LDA     TEMP2+1                 
        CMP     #(WKHIGH + $07)		; $27                    
        BNE     RDX3                    
        DEC     WKBASE + $000F                   
        RTS                             
.RDX1                                   
        JSR     RESTORE0                  
        JSR     MESSAGE                 
		EQUB	"FILE PROTECTED"                      
        BRK                             

;
; Display disk catalog
;
.CAT                                    
        JSR     DIR   				; load the catalog into the workspace                  
.LIB                                    
        JSR     MESSAGE    			; display title message             
		EQUB	LF,"TITLE:"                     
		
        LDX     #0 					; title starts at offset 0                     
.CX1                                    
        LDA     WKBASE,X        	; get character from title         
        JSR     OSWRCH         	; print it        
        INX                     	; increment count        
        CPX     #$E             	; done all ?        
        BNE     CX1             	; nope loop again
        
        JSR     MESSAGE        		; print filecount message
		EQUB	"   FILES:"                       
  
        LDA     $321                ; calculate and print filecount    
        PHA                             
        LDA     WKBASE + $000F                   
        STA     TEMP                    
        STA     $16                     
        LDA     #0                      
        STA     $25                     
        STA     $34                     
        STA     $43                     
        STA     $321                    
        JSR     $C589   
                
        JSR     MESSAGE   			; print drive message              
		EQUB	CR,LF,"DRIVE "
                   
        LDA     DRNMB       		; get drive number            
        JSR     $F80B           	; print it        
        JSR     MESSAGE        		; print qualifier message         
		EQUB	CR,LF,"QUALIFIER "               

        LDA     CQUAL       		; get and print qualifier           
        JSR     OSWRCH                 
        LDA     WKBASE + $04FE                   
        AND     #$F                     
        STA     $25                     
        LDA     WKBASE + $04FF                   
        STA     $16                     
        LDY     WKBASE + $000F                   
        BEQ     CX30                    
        SEC                             
        SBC     WKBASE + $0507                   
        BCS     CX31                    
        DEC     $25                     
		
.CX31   SEC                             
        SBC     WKBASE + $0505                   
        BCS     CX32                    
        DEC     $25                     

.CX32   LDY     WKBASE + $0504                   
        BEQ     CX33                    
        SEC                             
        SBC     #1                      
        BCS     CX33                    
        DEC     $25                     

.CX33   STA     $16                     
        SEC                             
        LDA     $25                     
        SBC     WKBASE + $0506                   
        STA     $25                     

.CX30   JSR     MESSAGE     		; print sectors free message     		
		EQUB	CR,LF,"SECTORS FREE:"            
        
		LDX     #0                	  	    
        JSR     $C589                   
        LDA     WKBASE + $000E 		; get density flag                  
        BNE     CX2                	; nonzero : singl density     
        JSR     MESSAGE            	; otherwise print double density mess     
		EQUB	CR,LF,"DOUBLE"                  
        NOP                             
        JMP     CX3					; skip on
                     
.CX2                                    
        JSR     MESSAGE            	; print single density message     
		EQUB	CR,LF,"SINGLE"                  
.CX3                                    
        NOP     					
                        
        JSR     MESSAGE                 
		EQUB	" DENSITY",CR,LF,LF       
        NOP
                             
        PLA                             
        STA     $321                    
        JSR     POINTSETUP      	; setup pointers        
.CX9                                    
        LDA     TEMP       			; done all files ?             
        BEQ     CX10                ; yep exit
    
        LDY     #NMLNG				; point past filename                  
        LDA     (TEMP1),Y    		; get file's locked flag           
        BPL     CX9A               	; +ve : not locked      
        LDA     #'#'                ; show that it's locked  
.CX9A   JSR     OSWRCH                 
        JSR     PCHFILE				; print filename        		         
        DEC     TEMP                    
        JSR     PNTUP				; increment pointer to next filename                   
        JSR     $C504                   
        JMP     CX9                     
.CX10                                   
        JMP     OSCRLF             ; print EOL    

.PCHFILE                                
        LDY     #0        			; start at first character of name              
.PCHF1                                  
        LDA     (TEMP1),Y         	; get character from name      
        JSR     OSWRCH             ; print it    
        INY                       	; increment count      
        CPY     #NMLNG              ; done all?    
        BNE     PCHF1               ; nope loop again    
        RTS                             

.PRINTSPACE                       	; print a space      
        LDA     #SPACE                  
        JSR     OSWRCH                 
.LUX2                                   
        RTS                             

.REPORT                                 
        LDA     MONFLAG                 
        BNE     LUX2
.REPORT4                                            
        LDY     #NMLNG                  
        LDA     (TEMP1),Y               
        PHP                             
        AND     #$7F                    
        JSR     OSWRCH                 
        LDA     #'#'                  
        PLP                             
        BMI     RP4A                     
        LDA     #' '                  
.RP4A   JSR     OSWRCH                 
        JSR     PCHFILE                 
        LDY     #16                     
.RPX5                                   
        JSR     PRINTSPACE              
        DEY                             
        BNE     RPX5                    
        LDX     #0                      
        LDY     #1                      
.RPX4                                   
        JSR     PRINTSPACE              
        LDA     (TEMP2),Y               
        JSR     PRINT_HEXA                   
        DEY                             
        LDA     (TEMP2),Y               
        JSR     PRINT_HEXA                   
        INY                             
        INY                             
        INY                             
        INX                             
        CPX     #2                      
        BCC     RPX4                    
        JSR     PRINTSPACE              
        LDY     #4                      
        LDA     (TEMP2),Y               
        LDY     #0                      
        CLC                             
        ADC     (TEMP2),Y               
        STA     VTEMP                   
        LDY     #5                      
        LDA     (TEMP2),Y               
        LDY     #1                      
        ADC     (TEMP2),Y               
        JSR     PRINT_HEXA                   
        LDA     VTEMP                   
        JSR     PRINT_HEXA                   
        JSR     PRINTSPACE              
        LDY     #5                      
        LDA     (TEMP2),Y               
        JSR     PRINT_HEXA                   
        DEY                             
        LDA     (TEMP2),Y               
        JSR     PRINT_HEXA                   
        JSR     PRINTSPACE              
        INY                             
        INY                             
        LDA     (TEMP2),Y               
        JSR     $F80B                   
        INY                             
        LDA     (TEMP2),Y               
        JSR     PRINT_HEXA                   
        JMP     OSCRLF                 

.LOOKUPNAME                             
        JSR     STR			;DELMT               
.LOOKUPNAME3
        JSR     STORELOCAL              
        JSR     PCHPRESENT              
        BCS     LUX1                    
        JSR     RESTORE0                  
        JSR     MESSAGE                 
		EQUB	"FILE NOT FOUND"                  
        BRK                             
.LUX1                                   
        RTS                             

.TRSC                                  
        LDA     #$FF                    
        STA     DTRCK                   
        CLC                             
        ADC     FILELEN                 
        LDA     FILELEN+1               
        ADC     #0                      
        STA     SCTLEFT                 
        LDA     MSB                     
        JSR     SHIFTRIGHT4             
        STA     SCTLEFT+1               
        LDA     MSB                     
        AND     #$F                     
        TAX                             
        LDA     STSCT                   
.CXA2                                   
        SEC                             
.CXA1                                   
        INC     DTRCK                   
        SBC     HSTSPT                  
        BCS     CXA1                    
        DEX                             
        BPL     CXA2                    
        ADC     HSTSPT                  
        STA     DSECT                   
        BNE     CXA4                    
        DEC     DTRCK                   
        LDA     HSTSPT                  
        STA     DSECT                   
.CXA4                                   
        LDA     SCTLEFT                 
        BNE     CXA3                    
        INC     SCTLEFT                 
.CXA3                                   
        JSR     SEEK                    
        RTS                             

.SHIFTRIGHT4                            
        LSR     A                       
        LSR     A                       
        LSR     A                       
        LSR     A                       
        RTS                             

.LOCK                          		         
        SEC                             
.UNLOCK                                 
        PHP                             
        JSR     LOOKUPNAME              
        LDA     CQUAL                  
        ROL     A                       
        PLP                             
        ROR     A                       
        LDY     #NMLNG                  
        STA     (TEMP1),Y               
        JSR     REPORT                  

.LOX1   JSR     SAVETRACK0 			; Write track 0 back to disk             
        JMP     MONX1                   

.EXLD                                  
        LDX     #LDADR                  
.EXLD2  LDA     #0                      
        STA     0,X                     
        STA     1,X                     
        STA     2,X                     
        JSR     SKIPSPACE                 
.CEL4                                   
        LDA     $100,Y                  
        CMP     #'0'                  
        BCC     CEL1                    
        CMP     #':'                  
        BCC     CEL2                    
        SBC     #7                      
        BCC     CEL1                    
        CMP     #'@'                  
        BCS     CEL1                    
.CEL2                                   
        ASL     A                       
        ASL     A                       
        ASL     A                       
        ASL     A                       
        STY     2,X                     
        LDY     #4                      
.CEL3                                   
        ASL     A                       
        ROL     0,X                     
        ROL     1,X                     
        DEY                             
        BNE     CEL3                    
        LDY     2,X                     
        INY                             
        BNE     CEL4                    
.CEL1                                   
        LDA     2,X                     
        RTS                             

.AUTOSTART                              
        JSR     READNAME                
        LDA     DRNMB                   
        STA     VTEMP                   
        LDA     CQUAL                  
        STA     VTEMP+1                 
        LDA     #SPACE                  
        STA     CQUAL                  
        LDA     #0                      
        STA     PEXEADR                 
        JSR     GDX2                    
        LDX     #NAME                   
        JSR     STORELOCAL              
        JSR     PCHPRESENT              
        BCS     XAS1                    
        JSR     XAS2                    
        JSR     RESTORE0                  
        JSR     MESSAGE                 
		EQUB	"COMMAND NOT YET SUPPORTED"          
        BRK     
                        
.XAS1                                   
        JSR     LFR1                    
        JSR     XAS2                    
        JMP     (PEXEADR)               
.XAS2                                   
        LDA     VTEMP                   
        JSR     GDX2                    
        LDA     VTEMP+1                 
        STA     CQUAL                  
        RTS                             

.RUN                                    
        JSR     READNAME                
        JSR     LFX1                    
.RX1                                    
        JMP     (PEXEADR)               

.PLD                                    
        JSR     STRBUFF		;FILL            
        JSR     EXLD                   
        BEQ     LFX2                    
        LDA     #$FF                    
.LFX1                                   
        STA     PEXEADR                 
.LFX2                                   
        LDX     #NAME                   
        CLC                             
        JMP     (LODVEC)                  

.USE                                    
        LDA     CQUAL                  
        STA     BCUAL                  
.US1                                    
        INY                             
        JSR     DELMT                   
        LDA     $FF,Y                   
        STA     CQUAL                  
        RTS                             

.SET                                    
        JSR     US1                     
        STA     BCUAL                  
        RTS                             
;
; Go at an address
;

.GO                                     
        LDX     #NAME+1                 
        JSR     EXLD2                 
        PHP                             
        JSR     DELMT                   
        PLP                             
        BEQ     RX1                     
        JMP     (NAME+1)                

.HELP                                   
        JSR     DELMT					; Check for EOL or next satatement, error if not
  
        LDA     #(TABLE)/256            ; Get pointer to command table
        STA     TEMP1+1                 
        LDA     #(TABLE)%256            
        STA     TEMP1                   
        LDY     #0                      ; Y points to table
.HP2                                    
        LDX     #0                      ; Zero screen column number
.HP3                                    
        LDA     (TEMP1),Y   			; Get char from table		            
        BMI     HP1                     ; If > $7F then it's the address, go print that
        JSR     OSWRCH                 ; print the character
        INX                             ; increment pointers
        INY                             
        BNE     HP3                     ; reached end of page no : loop again?
        INC     TEMP+1                  ; increment MSB of pointer
        BNE     HP3                     ; Keep going if not zero
.HP1                                    
        LDA     #SPACE                  ; Print spaces after name
.HP4                                    
        JSR     OSWRCH                 
        INX                             ; increment column number
        CPX     #NMLNG                  ; past lenth of filename ?
        BNE     HP4                     ; nope keep going
        
		LDA     (TEMP1),Y               ; Get MSB of exec address
        JSR     PRINT_HEXA                 ; output it
        INY                             ; move to next 
        BNE     HP5                     ; end of page 
        INC     TEMP1+1                 ; yes increment MSB of pointer
.HP5                                    
        LDA     (TEMP1),Y               ; get LSB of exec addresss
        JSR     PRINT_HEXA                 ; output it
        INY                             ; move to next byte of table
        BNE     HP6                     ; end of page ?
        INC     TEMP1+1                 ; yes increment MSB of pointer
.HP6                                    
        JSR     OSCRLF                 ; move to new line
        JSR     $C504                   ; ???
        LDA     (TEMP1),Y               ; get next byte of table
        BPL     HP2         			; if it's > $80, loop again

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
            
        JMP     OSCRLF                 ; move to new line & exit

.COMVCT                                 
        LDX     #$FF                    
        CLD                             
.CMX4                                   
        LDY     #0                      
        JSR     SKIPSPACE                 
        DEY                             
.CMX2                                   
        INY                             
        INX                             
.CMX5                                   
        LDA     TABLE,X                 
        BMI     CMX1                    
        CMP     $100,Y                  
        BEQ     CMX2                    
        DEX                             
.CMX3                                   
        INX                             
        LDA     TABLE,X                 
        BPL     CMX3                    
        INX                             
        LDA     $100,Y                  
        CMP     #'.'                  
        BNE     CMX4                    
        INY                             
        DEX                             
        BCS     CMX5                    
.CMX1                                   
        STA     MEM+1                   
        LDA     TABLE+1,X               
        STA     MEM                     
        CLC                             
        LDX     #0                      
        LDA     #4                      
        STA     RETRYCOUNT                 
        JMP     (MEM)                   

.TABLE                                  
		EQUB	"CAT"                                 
		EQUB	>CAT, <CAT
                        
		EQUB	"TITLE"                               
		EQUB	>TITLE, <TITLE                           

		EQUB	"GO"
		EQUB	>GO, <GO
                        
		EQUB	"SAVE"                                
		EQUB	>PSV, <PSV

		EQUB	"LOAD"                                
		EQUB	>PLD, <PLD
                  
		EQUB	"LIB"                                 
		EQUB	>LIB, <LIB
                        
		EQUB	"SET"                                 
		EQUB	>SET, <SET

		EQUB	"USE"                                 
		EQUB	>USE, <USE
                             
		EQUB	"DOS"                                 
		EQUB	>DOS, <DOS

		EQUB	"DIR"                                 
		EQUB	>DIR, <DIR

		EQUB	"DELETE"                              
		EQUB	>DELETE, <DELETE
                        
		EQUB	"RUN"                                 
		EQUB	>RUN, <RUN
                            
		EQUB	"LOCK"                                
		EQUB	>LOCK, <LOCK
		
		EQUB	"UNLOCK"                              
		EQUB	>UNLOCK, <UNLOCK

		EQUB	"OSCLI"                               
		EQUB	>GDOSCLI, <GDOSCLI

		EQUB	"RENAME"                              
		EQUB	>RENAME, <RENAME

		EQUB	"WIPE"                                
		EQUB	>WIPE, <WIPE
          
		EQUB	"KEEP"                                
		EQUB	>KEEP, <KEEP
		
		EQUB	"WTRACK"                              
		EQUB	>WTRACK, <WTRACK

		EQUB	"FOLDOUT"                             
		EQUB	>PFLD, <PFLD

		EQUB	"RSECT"                               
		EQUB	>RSECT, <RSECT

		EQUB	"WSECT"                               
		EQUB	>WSECT, <WSECT

		EQUB	"MON"                                 
		EQUB	>MON, <MON
	
		EQUB	"NOMON"                               
		EQUB	>NOMON, <NOMON

		EQUB	"DRIVE"                               
		EQUB	>DRIVE, <DRIVE

		EQUB	"FORMAT"                              
		EQUB	>FORMAT, <FORMAT

		EQUB	"RECOVER"                             
		EQUB	>RECOVER, <RECOVER

		EQUB	"#40"                                 
		EQUB	>FOUR, <FOUR

		EQUB	"#80"                                 
		EQUB	>EIGHT, <EIGHT

		EQUB	"HELP"                                
		EQUB	>HELP, <HELP
		
		EQUB	"COS"                                 
		EQUB	>PCOS, <PCOS
                        
		EQUB	"INFALL"                              
		EQUB	>INFALL, <INFALL

		EQUB	"INFO"                                
		EQUB	>INFO, <INFO

		EQUB	"ENABLE"                              
		EQUB	>ENABLE, <ENABLE

		EQUB	"VERIFY"                              
		EQUB	>VERIFY, <VERIFY

		EQUB	"RTRACK"                              
		EQUB	>RTRACK, <RTRACK

		EQUB	"SD"                                  
		EQUB	>PSD, <PSD

		EQUB	"DD"                                  
		EQUB	>PDD, <PDD

		EQUB	>AUTOSTART, <AUTOSTART

.PCHPRESENT                             
        JSR     READTRCK0               
        JSR     POINTSETUP              
        LDA     WKBASE + $000F                   
        BNE     CIP4                    
        CLC                             
        RTS                             
.CIP4                                   
        LDA     #0                      
        STA     TEMP                    
.CIP2                                   
        LDY     #NMLNG                  
        LDA     (TEMP1),Y               
        AND     #$7F                    
        CMP     CQUAL                  
        BNE     CIP1                    
        DEY                             
.CIP3                                   
        LDA     (TEMP1),Y               
        CMP     FLNM,Y                  
        BNE     CIP1                    
        DEY                             
        BPL     CIP3                    
        SEC                             
        RTS                             
.CIP1                                   
        JSR     PNTUP                   
        INC     TEMP                    
        LDA     TEMP                    
        CMP     WKBASE + $000F                   
        BNE     CIP2                    
        CLC                             
        RTS                             

.PNTUP                                  
        PHP                             
        CLC                             
        LDA     TEMP1                   
        ADC     #16                     
        STA     TEMP1                   
        LDA     TEMP1+1                 
        ADC     #0                      
        STA     TEMP1+1                 
        CLC                             
        LDA     TEMP2                   
        ADC     #8                      
        STA     TEMP2                   
        LDA     TEMP2+1                 
        ADC     #0                      
        STA     TEMP2+1                 
        PLP                             
        RTS                             

.PNTDOWN                                
        PHP                             
        SEC                             
        LDA     TEMP1                   
        SBC     #16                     

        STA     TEMP1                   
        LDA     TEMP1+1                 
        SBC     #0                      
        STA     TEMP1+1                 
        SEC                             
        LDA     TEMP2                   
        SBC     #8                      
        STA     TEMP2                   
        LDA     TEMP2+1                 
        SBC     #0                      
        STA     TEMP2+1                 
        PLP                             
        RTS                             

.POINTSETUP                             
        PHA                             
        LDA     #(WKLOW + $10)
        STA     TEMP1                   
        LDA     #WKHIGH                    
        STA     TEMP1+1                 
        LDA     #0                      
        STA     TEMP2                   
        LDA     #(WKHIGH + 5)                     
        STA     TEMP2+1                 
        PLA                             
        RTS                             

;
; Initial NMI handler, to handle NMI generated when disk controller is reset.
;
if (ISWD177X = 0)
.NMINI    
        LDA     #(NMIROUT)/256    	; Change to main NMI routine      
        STA     NMIVEC+1                    
        LDA     #(NMIROUT)%256          
        STA     NMIVEC               
  
        LDA     STATUS 				; RESET INTRQ     
        PLA                             
        RTI                             
endif

;
; Main NMI handler for disk.
; This routine gets the status from the disk controller and saves it
; for the error handlers. It then modifies it's own return address to
; jump to the user handler for the specified command.
;

.NMIROUT                                
		LDA     STATUS         		; Get status from WD
        STA     DSTATUS           	; save it for error handlers      
        
		LDA     $B000             	; read keyboard ?
        AND     #$F0                
        STA     $B000               
    
        TXA                      	; save x on STACKPAGE
        PHA                             
        TSX                       	; get SP to X      
        
		LDA     NMI+1              	; get our NMI handler address
        STA     $105,X              ; modify our return address on STACKPAGE    
        LDA     NMI                 ; with NMI routine address    
        STA     $104,X              
    
        PLA                         ; restore x from STACKPAGE
        TAX                             
        PLA                         ; return to target NMI routine    
        RTI                             

;
; Restore to track 0
;

.RESTORE0                                 
        LDA     #(HUP2)/256 		; point to nmi handler            
        STA     NMI+1               ; update pointer    
        LDA     #(HUP2)%256             
        STA     NMI
                     
        LDA     #RESTC          	; restore command
        AND     #3                  ; spin up and don't verify    
        STA     CMD                	; send it     
.HUP1                                   
        BNE     HUP1              	; wait, nmi breaks out      
.HUP2                                   
        RTS                             

;
; Step in a track.
;

.STEPIN                                 
        LDA     #(STPL1)/256        ; Setup NMI 
        STA     NMI+1                   
        LDA     #(STPL1)%256            
        STA     NMI                     
		
        LDA     #STPIC  			; step in command            
        STA     DCOM               	; save in currently executing    
        STA     CMD                	; give command to wd     
.STPL2                                  
        BNE     STPL2              	; wait for NMI     
		
.STPL1                             	; NMI exit     
        INC     DTRCK               ; update current track
        RTS                             

;
; Restore to track 0
;

.RESTORE                                
        LDA     #(RST2)/256       	; Setup NMI       
        STA     NMI+1                   
        LDA     #(RST2)%256             
        STA     NMI                     

        LDA     #0             		; Set current track to 0         
        STA     DTRCK                   
        LDA     #RESTC              ; Send restore cmd to WD    
        STA     CMD                     
.RST1                         		; wait for NMI          
        BNE     RST1                    

.RST2                                   
        LDA     RDA   				; Get disk control register                  
        ASL     A                   ; check for index pulse    
        BPL     RST2                ; nope, keep waiting    
        LDX     #5                  ; dhort delay    
        JSR     DELAY                   
        RTS                             

;
; Seek to a track
;
.SEEK                                   
        LDA     #(SKL1)/256			; Setup NMI                    
        STA     NMI+1                   
        LDA     #(SKL1)%256             
        STA     NMI                     
		
        LDA     DTRCK       		; get track to seek to 
        STA     RDATA               ; send to WD    
        LDA     #SEEKC            	; Seek command      
        STA     DCOM              	; send to WD      
        STA     CMD                     
.SKL2                              	; loop waiting for NMI     
        BNE     SKL2                    
		
.SKL1                                   
        JSR     ERRTYPE1          	; Check type 1 error
        BCS     SEEK                ; if error, try again    
        RTS                             

;
; Read SCTLEFT sectors into memory at LDADDR, starting at sector DSECT. 
;

.RDSECT                                
        LDA     HSTSPT              ; compare starting sector to sec / track
        CMP     DSECT               ; less than : skip on
        BCS     RS2 
                    
        JSR     STEPIN              ; step drive in
        LDA     #1                  ; reset start sector to 1
        STA     DSECT                   
.RS2                                    
        LDA     #(RS6)/256          ; setup NMI handler  
        STA     NMI+1                   
        LDA     #(RS6)%256              
        STA     NMI 
                    
        LDA     DSECT              	; get start sector     
        STA     SECTOR              ; save in WD sector register 
        LDY     #0                  ; offset 0
        LDA     #RSCTC              ; Read sector command    
        STA     DCOM                ; save in drive command  
        STA     CMD                 ; send to WD
		
; Loop waiting for each DRQ, then saving read data to memory. NMI breaks out.
.RS8                                    
        LDA     RDA                 ; read drive control register
        BPL     RS8                 ; DRQ low : yes keep waiting 
        LDA     RDATA               ; read data from disk
        STA     (LDADR),Y           ; save in memory	
        INY                         ; increment pointer 
        BNE     RS8                 ; continue waiting if y <> 0
.RS7                                ; Wait for NMI
        BEQ     RS7                     

; NMI handler
.RS6                                    
        JSR     ERRTYPE2			; check for type II comand error               
        BCS     RS2                 ; error : retry
		
        INC     LDADR+1 			; move to the next block of memory                
        INC     DSECT               ; move to next sector
        DEC     SCTLEFT             ; decrement sector count
        BNE     RDSECT              ; not zero : read next
        RTS                             

;
; write SCTLEFT sectors from memory at LDADDR, starting at sector DSECT. 
;
.WRSECTWP
        JSR     WRPROT      		; check for write protect            
.WRSECT                                 
        LDA     HSTSPT              ; compare starting sector to sec / track
        CMP     DSECT               ; less than : skip on
        BCS     WS2                     
        
        JSR     STEPIN              ; step drive in
        LDA     #1                  ; reset start sector to 1
        STA     DSECT                   

.WS2                                    
        LDA     #(WS6)/256          ; setup NMI handler
        STA     NMI+1                   
        LDA     #(WS6)%256              
        STA     NMI                     
		
        LDA     DSECT              	; get start sector     
        STA     SECTOR              ; save in WD sector register 
        LDY     #0                  ; offset 0
        LDA     #WSCTC              ; Write sector command
        STA     DCOM                ; set current command in progress
        STA     CMD                 ; send to WD

.WS8                                    
        LDA     RDA                 ; get drive control register for DRQ
        BPL     WS8                 ; no DRQ, continue waiting
		
        LDA     (LDADR),Y           ; get a byte from meory
        STA     RDATA               ; send it to the WD
        INY                         ; increment count / pointer
        BNE     WS8                 ; loop again
.WS7                                    
        BEQ     WS7                 ; all done, wait for NMI
		
.WS6                                    
        JSR     ERRTYPE2            ; check for type 2 error
        BCS     WS2                 ; error : try again
		
        INC     LDADR+1             ; move to next page of memory to write
        INC     DSECT               ; move to next sector
        DEC     SCTLEFT             ; decrement sector count
        BNE     WRSECT              ; more left to do, loop again
        RTS                             

;
; Handle type 1 command errors
; restore / seek / step / step in / step out
;

.ERRTYPE1                              
        LDA     STATUS             	; get status register from WD  	   
        AND     #$18                ; mask out bits we are interested in RNF / CRC
        BEQ     ERR5                ; no error : exit 
		
        AND     #$10                ; RNF ?
        BEQ     ERR4                ; yep, do it
        
		LDA     RETRYCOUNT          ; Any retries left ?
        BNE     ERR13               ; yep : flag error and try again
        
		JSR     RESTORE0            ; lift head up
        JSR     MESSAGE             ; Display message
		EQUB	"FATAL SEEK ERROR TRACK:"      
        
		LDA     DTRCK               ; get track number
        JSR     PRINT_HEXA             ; display it ?
        BRK                         ; back to command loop?
		
.ERR4                                   
        LDA     RETRYCOUNT          ; Any retries left ?
        BNE     ERR13               ; yep : flag error and try again
        JSR     MESSAGE             ; display message
		EQUB	"CRC"                                 
        BRK                             
		
.ERR13                                  
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
        LDA     STATUS              ; get status register from WD
        ASL     A                   ; get wp bit into bit 7
        BPL     WRP1                ; not write protected, exit
		
        JSR     RESTORE0              ; unload head
        JSR     MESSAGE             ; display WP message & exit
		EQUB	"WRITE PROTECTED DISC"                 
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
		
        ASL     A 					; get record type flag in b7                      
        ASL     A                       
        BPL     ERR6                ; Data mark, skip
		
        LDA     RETRYCOUNT             ; get retry count
        BNE     ERR3                ; still reties left, skip
		
        JSR     MESSAGE             ; write message
		EQUB	"WRITE FAULT TR:"                    
.ERRC                               ; get track no 
        LDA     DTRCK               ; write it
        JSR     PRINT_HEXA                   
		
        JSR     MESSAGE             ; write sector message
		EQUB	" SCT:"                           
        LDA     DSECT               ; get sector no
        JSR     PRINT_HEXA             ; write it
        BRK                             
		
.ERR6                                   
        ASL     A                   ; get RNF flag in b7
        BPL     ERR12               ; no error : skip
		
        LDA     RETRYCOUNT             ; get retry count
        BNE     ERR3                ; still retries, try again
		
        JSR     MESSAGE             ; print record not found message
		EQUB	"RECORD NOT FOUND TR:"              
        NOP                           	  
        JSR     RESTORE0              ; unload head
        JMP     ERRC				; Write track and sector numbers                   
		
.ERR12                                  
        ASL     A                   ; get CRC error in b7
        BPL     ERR7                ; no CRC error, skip
		
        LDA     RETRYCOUNT             ; get retry count
        BNE     ERR3                ; still retries, try again
		
		JSR     MESSAGE             ; crc error message
		EQUB	"CRC"                                 
        BRK                             
		
.ERR7                                   
        LDA     RETRYCOUNT             ; get retry count
        BNE     ERR3                ; still retries, try again
		
		JSR     RESTORE0              ; unload head
        JSR     MESSAGE             ; bad data message
		EQUB	"BAD DATA"                           
        BRK                             
		
.ERR3                                   
        DEC     RETRYCOUNT             ; decrement retry count
        SEC                         ; flag error to caller
        RTS                             

.FORMAT                                 
        LDA     ENBIND                  
        CMP     #$4E                    
        BEQ     FM1                     
.FORMAT6
        JSR     MESSAGE                 
		EQUB	"FACILITY NOT ENABLED"                
        BRK                             
.FM1                                    
        JSR     DRIVE                   
        JSR     RESTORE                 
        JSR     WRPROT                  
        LDA     #15                     
        JSR     OSWRCH                 
.FM2                                    
        LDY     #WKLOW
        STY     MEM                     
        LDA     #WKHIGH
        STA     MEM+1                   

        JSR     FOLDOUT                 
        LDA     DTRCK                   
        JSR     PRINT_HEXA                   
        LDA     #':'                  
        JSR     OSWRCH                 
.FM8                                    
        LDY     #WKLOW
        STY     MEM                     
        LDA     #WKHIGH                    
        STA     MEM+1                   

        JSR     SWRTTRC                 
        BCS     FM8                     
        LDA     DTRCK                   
        CMP     NTRC                    
        BEQ     FM6                     
        JSR     STEPIN                  
        JMP     FM2                     
.FM6                                    
        JSR     MESSAGE                 
		EQUB	CR,LF,"DISC FORMATTED",CR,LF,LF  
        NOP                             
        JSR     VERIFY                  
.FM4                                    
        LDA     DENSITY                 
        STA     WKBASE + $000E                   
        LDA     #0                      
        STA     WKBASE + $000F                   
        STA     ENBIND                  
        TAY                             
        LDA     #SPACE                  
.FM3                                    
        STA     WKBASE,Y                 
        INY                             
        CPY     #$E                     
        BNE     FM3                     
        JMP     SAVETRACK0              
        NOP                             
        RTS                             
.FORMATEND

	print ~(FORMATEND-FORMAT)
.WIPE                                   
        JSR     DRIVE                   
        LDA     ENBIND                  
        CMP     #$4E                    
        BEQ     FM4                     
        JMP     FORMAT6                

.TRINP                                  
        JSR     RESTORE                 
        JSR     TRPAR                   
        BEQ     TR5                     
.TR3                                    
        JSR     STEPIN                  
        DEY                             
        BNE     TR3                     
.TR5                                    
        RTS                             

.TRPAR                                  
        LDX     #FLNM                   
        JSR     EXLD2                 
        LDX     #FLNM+1                 
        JSR     EXLD2                 
        LDA     FLNM+2                  
        BNE     TR1                     
        
		LDA     #WKHIGH                    
        STA     MEM+1                   
        LDA     #0                      
        BEQ     TR2                     
.TR1                                    
        STA     MEM+1                   
        LDA     FLNM+1                  
.TR2                                    
        STA     MEM                     
        LDY     FLNM                    
        STY     DTRCK                   
        RTS                             

.WTRACK                                 
        JSR     WRPROT                  
        JSR     TRINP                   

.SWRTTRC                                
        LDA     #(SWT4)/256             
        STA     NMI+1                   
        LDA     #(SWT4)%256             
        STA     NMI                     
        LDA     #WTRCC                  
        STA     DCOM                    
        STA     CMD                     
.SWT2                                   
        LDA     RDA                     
        BPL     SWT2                    
        LDA     (MEM),Y                 
        STA     RDATA                   
        INY                             
        BNE     SWT2                    
        INC     MEM+1                   
        BNE     SWT2                    
.SWT4                                   
        JSR     ERRTYPE2               
        BCS     SWT7                    
.SWT6                                   
        STY     MEM                     
        LDA     MEM+1                   
        JSR     PRINT_HEXA                   
        LDA     MEM                     
        JSR     PRINT_HEXA                   
        JSR     PRINTSPACE              
        CLC                             
.SWT7                                   
        RTS                             

.RTRACK                                 
        JSR     TRINP                   
.RDT4                                   
        LDA     #(RDT1)/256             
        STA     NMI+1                   
        LDA     #(RDT1)%256             
        STA     NMI                     
        LDA     #RTRCC                  
        STA     DCOM                    
        STA     CMD                     
.RDT2   LDA     RDA                     
        BPL     RDT2                    
        LDA     RDATA                   
        STA     (MEM),Y                 
        INY                             
        BNE     RDT2                    
        INC     MEM+1                   
        BNE     RDT2                    
.RDT1                                   
        JMP     SWT6                    

.PFLD                                   
        JSR     TRPAR                   

.FOLDOUT                                
        LDA     #1                      
        STA     DSECT                   
        LDA     DENSITY                 
        BNE     FD1                     
        LDA     #(TRFORMD)%256          
        STA     TEMP2                   
        LDA     #(TRFORMD)/256          
        STA     TEMP2+1                 
        LDA     #(GAP1D)%256            
        STA     TEMP1                   
        LDA     #(GAP1D)/256            
        STA     TEMP1+1                 
        LDA     #(INTERLD)%256         
        STA     WORK                    
        LDA     #(INTERLD)/256         
        STA     WORK+1                  
        BNE     FD2                     
.FD1                                    
        LDA     #(TRFORMS)%256          
        STA     TEMP2                   
        LDA     #(TRFORMS)/256          
        STA     TEMP2+1                 
        LDA     #(GAP1S)%256            
        STA     TEMP1                   
        LDA     #(GAP1S)/256            
        STA     TEMP1+1                 
        LDA     #(INTERLS)%256         
        STA     WORK                    
        LDA     #(INTERLS)/256         
        STA     WORK+1                  
.FD2                                    
        LDY     #0                      
        LDA     (TEMP2),Y               
        INY                             
        TAX                             
        LDA     (TEMP2),Y               
        CMP     #99                     
        BEQ     FD6                     
        CMP     #$CC                    
        BNE     FD8                     
        STY     VTEMP                   
        LDY     DSECT                   
        LDA     (WORK),Y                
        LDY     VTEMP                   
.FD8                                    
        CMP     #$DD                    
        BNE     FD9                     
        LDA     DTRCK                   
.FD9                                    
        DEY                             
.FD3                                    
        STA     (MEM),Y                 
        INY                             
        DEX                             
        BNE     FD3                     
        LDX     #(TEMP2-$5A)            
        JSR     $F671                   
        JSR     $F671                   
.FD4                                    
        TYA                             
        BNE     FD5                     
        INC     MEM+1                   
        BNE     FD2                     
.FD5                                    
        CLC                             
        ADC     MEM                     
        STA     MEM                     
        LDA     MEM+1                   
        ADC     #0                      
        STA     MEM+1                   
        BNE     FD2                     
.FD6                                    
        INC     DSECT                   
        LDA     HSTSPT                  
        CMP     DSECT                   
        BCC     FD7                     
        LDA     TEMP1                   
        STA     TEMP2                   
        LDA     TEMP1+1                 
        STA     TEMP2+1                 
        BNE     FD2                     
.FD7                                    
        LDX     #0                      
        DEY                             
        LDA     (TEMP2),Y               
        STA     VTEMP                   
        LDA     DENSITY                 
        BNE     FD11                    
        JSR     FD11                    
        INC     MEM+1                   
.FD11                                   
        LDA     VTEMP                   
.FD10                                   
        STA     (MEM),Y                 
        INY                             
        DEX                             
        BNE     FD10                    
        RTS                             

.TRFORMS                                
		EQUW	$FF20                                 
.GAP1S                                  
		EQUW	$0006                                 
		EQUW	$FE01                                 
		EQUW	$DD01                                 
		EQUW	$0001                                 
		EQUW	$CC01                                 
		EQUW	$0101                                 
		EQUW	$F701                                 
		EQUW	$FF0B                                 
		EQUW	$0006                                 
		EQUW	$FB01                                 
		EQUW	$E500                                 
		EQUW	$F701                                 
		EQUW	$FF12                                 
		EQUW	$63FF                                 

.TRFORMD                                
		EQUW	$4E50                                 
		EQUW	$000C                                 
		EQUW	$F603                                 
		EQUW	$FC01                                 
		EQUW	$4E32                                 
.GAP1D                                  
		EQUW	$000C                                 
		EQUW	$F503                                 
		EQUW	$FE01                                 
		EQUW	$DD01                                 
		EQUW	$0001                                 
		EQUW	$CC01                                 
		EQUW	$0101                                 
		EQUW	$F701                                 
		EQUW	$4E16                                 
		EQUW	$000C                                 
		EQUW	$F503                                 
		EQUW	$FB01                                 
		EQUW	$E500                                 
		EQUW	$F701                                 
		EQUW	$4E30                                 
		EQUW	$634E                                 

.INTERLS                               
		EQUB	      0,1,8,5,2,9,6,3,10,7,4          
.INTERLD                               
		EQUB	      0,1,12,7,2,13,8,3,14,9,4,15,10,5,16,11,6
.FOLDEND

	print ~(FOLDEND-FOLDOUT)
	
.VERIFY                                 
        JSR     RESTORE                 
        LDY     #WKLOW
        STY     MEM                     
        LDA     #WKHIGH
        STA     MEM+1                   

        LDA     CQUAL                  
        STA     BCUAL                  
.VX3                                    
        LDA     HSTSPT                  
        STA     TEMP+1                  
        JSR     PRINTSPACE              
        LDA     DTRCK                   
        JSR     PRINT_HEXA                   
        LDA     DTRCK                   
        STA     TRACK                   
        LDY     #0                      
        STY     TEMP                    
        LDA     #(VX6)/256              
        STA     NMI+1                   
        LDA     #(VX6)%256              
        STA     NMI                     
        LDA     #5                      
        STA     VTEMP                   
.VX2                                    
        LDA     #RDRSC                  
        STA     CMD                     
.VX5                                    
        LDA     RDA                     
        BPL     VX5                     
        LDA     RDATA                   
        STA     (MEM),Y                 
        INY                             
        BNE     VX5                     
.VX6                                    
        LDY     TEMP                    
        INY                             
        INY                             
        INY                             
        STY     TEMP                    
        LDA     DSTATUS                 
        AND     #$1C                    
        BEQ     VX21                    
        DEC     VTEMP                   
        BEQ     VX22                    
        BNE     VX2                     
.VX21                                   
        DEC     TEMP+1                  
        BNE     VX2                     
        LDY     #0                      
        TYA                             
.VX10                                   
        STA     FLNM+1,Y                
        INY                             
        CPY     HSTSPT                  
        BNE     VX10                    
        LDY     #2                      
.VX11                                   
        LDA     (MEM),Y                 
        TAX                             
        STA     FLNM,X                  
        INY                             
        INY                             
        INY                             
        INC     TEMP+1                  
        LDA     HSTSPT                  
        CMP     TEMP+1                  
        BNE     VX11                    
        LDX     #0                      
        STX     TEMP+1                  
.VX7                                    
        LDA     FLNM+1,X                
        BEQ     VX1                     
        INC     TEMP+1                  
.VX1                                    
        INX                             
        CPX     HSTSPT                  
        BNE     VX7                     
        LDA     #12                     
        STA     TEMP                    
        LDA     DENSITY                 
        BEQ     VX13                    
        LSR     TEMP                    
.VX13                                   
        LDA     TEMP+1                  
        CMP     TEMP                    
        BCS     VX8                     
.VX22                                   
        LDA     #'?'                  
        JSR     OSWRCH                 
        JMP     VX83                   
.VX8    JSR     PRINTSPACE              
.VX83   LDA     DTRCK                   
        CMP     NTRC                    
        BEQ     VX4                     
        JSR     STEPIN                  
        JMP     VX3                     
.VX4                                    
        LDA     BCUAL                  
        STA     CQUAL                  
        JSR     MESSAGE                 
		EQUB	      CR,LF,"DISC VERIFIED",CR,LF      
        NOP                             
        JMP     RESTORE                 

.PARAM                                  
        LDX     #FLNM                   
        JSR     EXLD2 				;TRACK          
        LDX     #FLNM+1                 
        JSR     EXLD2 				;SECTOR         
        LDX     #FLNM+2                 
        JSR     EXLD2 				;# SECTORS      
        LDX     #FLNM+3                 
        JSR     EXLD2 				;MEMORY         
        LDA     FLNM+2                  
        BEQ     NOSECT                  
        STA     SCTLEFT                 
        LDA     FLNM+3                  
        STA     LDADR                   
        LDA     FLNM+4                  
        STA     LDADR+1                 
        LDA     FLNM+1                  
        STA     DSECT                   
        LDA     FLNM                    
        STA     DTRCK                   
        JSR     SEEK                    
        RTS                             

.NOSECT                                 
        JSR     MESSAGE                 
		EQUB	"NO SECTORS?"                  
        BRK                             

.RSECT                                  
        JSR     PARAM                   
        JSR     RS2                     
        RTS                             

.WSECT                                  
        JSR     PARAM                   
        JSR     WS2                     
        RTS                             

.INFO                                   
        JSR     LOOKUPNAME              
        JSR     REPORT4                
        JMP     MONX1                   

.ENABLE                                 
        JSR     DELMT                   
        LDA     #$4E                    
        STA     ENBIND                  
        RTS                             

.MERRFULL                               
        JSR     RESTORE0                  
        JSR     MESSAGE                 
		EQUB	"MAXIMUM ACCOUNT ON DISC"              
        BRK                             

.PADAPTCAT                              
        JSR     STORELOCAL              
        JSR     PCHPRESENT              
        BCC     CRX1                    
        JSR     REMDIR                  
.CRX1                                   
        LDA     FILELEN                 
        PHA                             
        LDA     FILELEN+1               
        PHA                             
        SEC                             
        LDA     MSB                     
        SBC     FILELEN                 
        STA     FILELEN                 
        LDA     MSB+1                   
        SBC     FILELEN+1               
        STA     FILELEN+1               
        LDA     #0                      
        STA     MSB                     
        LDA     #BEGINSCT               
        STA     STSCT                   
        JSR     POINTSETUP              
        LDX     WKBASE + $000F                   
        STX     TEMP                    
        BEQ     CRX9                    
        CPX     #NMBFILES               
        BCS     MERRFULL                
.CRX14                                  
        JSR     PNTUP                   
        DEX                             
        BNE     CRX14                   
        JSR     EXAMROOM                
        JMP     CRX2                    
.CRX9                                   
        JMP     CRX5                    
.CRX4                                   
        DEC     TEMP                    
        JSR     PNTDOWN                 
        LDY     #6                      
        LDA     (TEMP2),Y               
        JSR     SHIFTRIGHT4             
        STA     MSB                     
        CLC                             
        DEY                             
        DEY                             
        LDA     (TEMP2),Y               
        ADC     #$FF                    
        INY                             
        LDA     (TEMP2),Y               
        INY                             
        INY                             
        ADC     (TEMP2),Y               
        STA     STSCT                   
        DEY                             
        LDA     (TEMP2),Y               
        AND     #$F                     
        ADC     MSB                     
        STA     MSB                     
        JSR     EXAMROOM                
.CRX2                                   
        LDA     TEMP                    
        BEQ     CRX3                    
        BCC     CRX4                    
.CRX3                                   
        BCS     CRX6                    
        JSR     RESTORE0                  
        JSR     MESSAGE                 
		EQUB	"NO ROOM ON DISC"                
        BRK                             
.CRX6                                   
						;blockmove                      
        LDA     #(WKHIGH + $04)                    
        STA     WORK+1                  
        LDA     #(WKLOW + $E0)
        STA     WORK                    
.CRX12                                  
        DEC     WORK                    
        LDA     WORK                    
        CMP     #$FF                    
        BNE     CRX11                   
        DEC     WORK+1                  
.CRX11                                  
        LDY     #0                      
        LDA     (WORK),Y                
        LDY     #16                     
        STA     (WORK),Y                
        LDA     WORK                    
        CMP     TEMP1                   
        BNE     CRX12                   
        LDA     WORK+1                  
        CMP     TEMP1+1                 
        BNE     CRX12 
                  
        LDA     #(WKHIGH + $07)                    
        STA     WORK+1                  
        LDA     #(WKLOW + $70)                    
        STA     WORK                    
.CRX22                                  
        DEC     WORK                    
        LDA     WORK                    
        CMP     #$FF                    
        BNE     CRX21                   
        DEC     WORK+1                  
.CRX21                                  
        LDY     #0                      
        LDA     (WORK),Y                
        LDY     #8                      
        STA     (WORK),Y                
        LDA     WORK                    
        CMP     TEMP2                   
        BNE     CRX22                   
        LDA     WORK+1                  
        CMP     TEMP2+1                 
        BNE     CRX22                   
.CRX5                                   
        LDY     #0                      
.CRX7                                   
        LDA     FLNM,Y                  
        STA     (TEMP1),Y               
        INY                             
        CPY     #NMLNG+1                
        BNE     CRX7                    
        LDY     #7                      
.CRX8                                   
        LDA     LDADR,Y                 
        STA     (TEMP2),Y               
        DEY                             
        BPL     CRX8                    
        JSR     REPORT                  
        PLA                             
        STA     LDADR+1                 
        PLA                             
        STA     LDADR                   
        INC     WKBASE + $000F                   
        RTS                             

.SAVVCT                                 
        JSR     PADAPTCAT               
        JSR     TRSC                   
        JSR     WRSECTWP                 
.FSA2                                   
        JSR     LOX1                    
        JMP     RESTORE0                  

.EXAMROOM                               
        JSR     PNTDOWN                 
        SEC                             
        LDY     #7                      
        LDA     (TEMP2),Y               
        SBC     STSCT                   
        PHA                             
        DEY                             
        LDA     (TEMP2),Y               
        AND     #15                     
        SBC     MSB                     
        TAX                             
        LDA     #0                      
        CMP     FILELEN                 
        PLA                             
        SBC     FILELEN+1               
        TXA                             
        SBC     #0                      
        PHP                             
        JSR     PNTUP                   
        PLP                             
        RTS                             

.INFALL                                 
        JSR     DRIVE                   
        JSR     DIR                     
        JSR     POINTSETUP              
        LDY     WKBASE + $000F                   
.IA1                                    
        TYA                             
        BEQ     IA2                     
        PHA                             
        JSR     REPORT4                
        JSR     PNTUP                   
        PLA                             
        TAY                             
        DEY                             
        JSR     $C504                   
        TYA                             
        BNE     IA1                     
.IA2                                    
        RTS                             

;
; Load disk catalog into memory at base of workspace.
;

.DIR                                    
        JSR     DRIVE   			; get drive number if specified                
        JSR     READTRCK0          	; read track 0     
        JSR     RESTORE0				; restore to track 0                  
        RTS                             

;
; Read track 0 into workspace
;

.READTRCK0                              
        LDA     LDADR				; get load address & save on STACKPAGE                   
        PHA                             
        LDA     LDADR+1                 
        PHA                             
		
        JSR     TR0SETUP           	; setup params
        JSR     RDSECT         		; read the sectors      
  
        PLA                         ; restore load address from the STACKPAGE
        STA     LDADR+1                 
        PLA                             
        STA     LDADR                   
        RTS                             

;
; Save track 0 from the workspace
;

.SAVETRACK0                             
        LDA     LDADR            	; get load address & save on STACKPAGE                   
        PHA                             
        LDA     LDADR+1                 
        PHA                             
		
        JSR     TR0SETUP            ; setup params    
        LDX     TYPE                   
        LDA     DENSITY             ; get density    
        BNE     ST02             	 
      
        LDA     HOD,X               ; set something dependent on density    
        STA     WKBASE + $04FE                   
        LDA     LOD,X                   
        STA     WKBASE + $04FF                   
        BNE     ST03                    
.ST02                                   
        LDA     HOS,X                   
        STA     WKBASE + $04FE                   
        LDA     LOS,X                   
        STA     WKBASE + $04FF                   
.ST03                                   
        LDA     #0                      
        STA     WKBASE + $04FD                   
        JSR     WRSECTWP         	; write the sectors        
.ST01                                   
        PLA                      	; restore load address from STACKPAGE       
        STA     LDADR+1                 
        PLA                             
        STA     LDADR                   
        JSR     RESTORE0             	; restore     
        RTS                             

.HOS                                    
		EQUB	      1,3                             
.LOS                                    
		EQUB	      $88,$18                         
.HOD                                    
		EQUB	      2,4                             
.LOD                                    
		EQUB	      $78,$F8                         

;
; Setup params for Single density.
;
.PSD                                    
        JSR     DRIVE				; Get drive no                   
        LDA     RDA                 ; get drive control reg     
        ORA     #PDDEN				; set DDEN high                      
        STA     RDA                 ; set control reg    
        LDA     #10					; 10 sectors per track                     
        STA     HSTSPT                  
        LDA     #1                	; set density to 1      
        STA     DENSITY                 
        RTS                             

.PDD                                    
        JSR     DRIVE				; Get drive no                   
.PDD3   LDA     RDA                 ; get drive control reg     
        AND     #PDDEN EOR $FF		; Set dden low                    
        STA     RDA                 ; set control reg    
        LDA     #16                 ; 16 sectors per track    
        STA     HSTSPT                  
        STA     HSTSPT                  
        LDA     #0                  ; set density flag    
        STA     DENSITY                 
        RTS                             

;
; Keep, make a backup of the diorectory track on the finat track of the disk.
;

.KEEP                                   
        JSR     DRIVE    			; Get drive no               
        JSR     READTRCK0			; Read track 0               
        JSR     TR0SETUP7           ; Set sector count and workspace ptr   
        LDA     #9            		; setting dsector to this causes backup to be on maxtrack +1          
        STA     DSECT              	     
        LDA     NTRC                ; get maximum track no for current disks    
        STA     DTRCK               ; set track to write to max track    
        JSR     SEEK              	; seek heads to track      
        JSR     WRSECTWP    		; Write the sectors to the track             
        JSR     RESTORE0             	; unmount head     
        RTS                             
		
;
; Recover, inverse of keep, copy backup directory to main directory.
;

.RECOVER                                
        LDA     NTRC          		; Get max track no
        STA     DTRCK               ; set track to read to max    
        JSR     SEEK              	; seek to the track      
        JSR     TR0SETUP7     		; Set sector count and workspace ptr            
        LDA     #9                  ; setting dsector to this causes backup to be on maxtrack +1    
        STA     DSECT                   
        JSR     RDSECT         		; Read sectors from backup        
        JSR     SAVETRACK0         	; write them to track 0     
        RTS                             

;
; Setup for track 0 read / write
;

.TR0SETUP                               
        LDA     #1        			; set start sector to 1              
        STA     DSECT                   
        JSR     RESTORE         	; restore drive to track 0        
.TR0SETUP7        
		LDA     #8           		; set sector count
        STA     SCTLEFT                 
		
        LDA     #WKLOW				; set load addr = workspace base                     
        STA     LDADR                   
        LDA     #WKHIGH
        STA     LDADR+1                 
        RTS                             

.FOUR                                   
        LDA     RDA                     
        AND     #P4080 EOR $FF		; Turn off 40/80 bit                    
        STA     RDA                     
        LDA     #39             	; max track no        
        STA     NTRC                    
        LDA     #0                      
        STA     TYPE                   
        RTS                             

.EIGHT                                  
        LDA     RDA                     
        ORA     #P4080 				; turn on 40//80 bit                   
        STA     RDA                     
        LDA     #79 				; max track no                    
        STA     NTRC                    
        LDA     #1                      
        STA     TYPE                   
        RTS                             

.RENAME                                 
        LDA     CQUAL                  
        PHA                             
        JSR     READNAME                
        JSR     LOOKUPNAME3            
        PLA                             
        STA     CQUAL                  
        JSR     REPORT                  
        LDX     #$40                    
        STX     NAME                    
        LDX     #NAME                   
        JSR     STORELOCAL              
        LDA     TEMP1                   
        PHA                             
        LDA     TEMP1+1                 
        PHA                             
        LDA     TEMP2                   
        PHA                             
        LDA     TEMP2+1                 
        PHA                             
        JSR     PCHPRESENT              
        BCC     RNM2                    
        JSR     RESTORE0                  
        JSR     MESSAGE                 
		EQUB	"NAME NOT UNIQUE"                 
        BRK                             
.RNM2                                   
        PLA                             
        STA     TEMP2+1                 
        PLA                             
        STA     TEMP2                   
        PLA                             
        STA     TEMP1+1                 
        PLA                             
        STA     TEMP1                   
        LDY     #NMLNG                  
        LDA     (TEMP1),Y               
        AND     #$80                    
        ORA     CQUAL                  
        STA     (TEMP1),Y               
        LDY     #0                      
.RNM1                                   
        LDA     FLNM,Y                  
        STA     (TEMP1),Y               
        INY                             
        CPY     #NMLNG                  
        BNE     RNM1                    
        JMP     LOX1                    

.GDOSCLI                                  
        STY     3                       
        LDY     #0                      
.OS1                                    
        LDX     #0                      
.OS2                                    
        LDA     (5),Y                   
        INY                             
        CMP     $100,X                  
        BNE     OS1                     
        INX                             
        CPX     3                       
        BNE     OS2                     
        STY     3                       
        CLC                             
        LDX     #0                      
        LDA     WRCVEC+1                    
        STA     OSTEMP+1                
        LDA     WRCVEC                    
        STA     OSTEMP                  
        LDA     #(NEWWRC)/256           
        STA     WRCVEC+1                    
        LDA     #(NEWWRC)%256           
        STA     WRCVEC                
        LDA     #0                      
        STA     OSTEMP+3                
        PLA                             
        PLA                             
        JMP     $C334                   
.NEWWRC                                 
        PHP                             
        STX     OSTEMP+2                
        LDX     OSTEMP+3                
        STA     $100,X                  
        CPX     #60                     
        BCS     NEWWRC1
        INX                             
.NEWWRC1
		STX     OSTEMP+3                
        LDX     OSTEMP+2                
        CMP     #CR                     
        BEQ     EXEC                    
        PLP                             
        RTS                             
.EXEC                                   
        PLA                             
        LDA     OSTEMP+1                
        STA     WRCVEC+1                    
        LDA     OSTEMP                  
        STA     WRCVEC                
        JSR     COMVCT                  
        RTS                             

.PCOS                                   
        JSR     DELMT                   
        LDX     #3                      
.PC1                                    
        LDA     $FFA2,X                 
        STA     LODVEC,X                  
        DEX                             
        BPL     PC1   
                  
        LDA     #$EF                    
        STA     COMVEC                    
        LDA     #$F8                    
        STA     COMVEC+1                    
        RTS                             

.END

;Pad rest of rom with $FF
{
	start = P%
	for n, start, $EFEF
		equb $FF
	next 
	
}

		EQUS	"PHILL WAS HERE!"

if (ISROM=1)
  if(IOBASE=$EFF0)
    SAVE"gdos1770-eff0.rom",$E000,$F000,$E000           
  else
    SAVE"gdos1770-bc00.rom",$E000,$F000,$E000           
  endif
endif

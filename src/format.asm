;
; Format command for AcornDOS 1770.
;
; Adapted from format command from ADOS-1770 for the Atom 2018-02-27 
; Phill Harvey-Smith.
; Adapted for ADOS-1770 from GDOS for the Atom, 2015-07
; Phill Harvey-Smith.
;

OURBASE =       $2800
		ORG		OURBASE
		
		include "../src/sysdefs.asm"
       	include "../src/wdfdc.asm"

VerifyBuf	= WKBASE + $0100	; Buffer used during verify.
FormatBuf   = OURBASE + $1000   ; Use this as format buffer, so we don't overwrite ourselves! 

.codestart
.formatcom         
.FM1                                    
;        JSR     DRIVE                   
;		jsr		drivecom
if(FORMAT)
        JSR     INLINE_PRINT				; Print confirmation message

        EQUB    $0D
        EQUS    "Do you really want to format drive "

        LDA     DRIVENO						; print drive number
        JSR     PRINT_HEXA_LOWN

        JSR     INLINE_PRINT				; Print " ?"
		
        EQUS    " ?"

		NOP

        JSR     OSECHO						; read a character
        CMP     #$59						; check for 'Y'
        BNE     L2847						; no : exit

        JSR     OSECHO						; read a character
        CMP     #$45						; check for 'E'
        BNE     L2847						; no : exit

        JSR     OSECHO						; read a character
        CMP     #$53						; check for 'S'
        BEQ     DO_FORMAT					; yep : go do it

.L2847
        JSR     OSCRLF						; print EOL and exit to dos
        RTS

.DO_FORMAT
		jsr		START_MOTOR_SELECT			; select drive, turn on motors
        JSR     RESTORE0                 	; seek back to track 0

        JSR     WRPROT                  	; check for write protect, errors if wp.
        LDA     #15                     	; write char
        JSR     OSWRCH                 
.FM2          
		JSR		MEMPTR_FormatBuf			; point at format buffer

        JSR     FOLDOUT                 	; write formatted track to buffer
        LDA     TRACKNO                   	; print track no
        JSR     PRINT_HEXA                   
        LDA     #':'                  		; print ':'
        JSR     OSWRCH                 
.FM8                                    
		JSR		MEMPTR_FormatBuf			; point at track buffer

        JSR     WRITE_TRACK                 ; write the track to disk
        BCS     FM8                     	; error, try writing again!
		
        LDA     TRACKNO                   	; get track no
        CMP     #TRACKS                    	; done all?
        BCS     FM6                     	; yep : exit loop	
        JSR     STEPIN                  	; step heads to next track
        JMP     FM2                     	; loop again
.FM6                                    
        JSR     INLINE_PRINT               	; print formatted message  
		EQUB	CR,LF,"FORMATTED"
		EQUB	CR,LF,LF  
        NOP 
		jsr		blank_cat					; write a blank catalog
        JSR     verifycom                  	; go verify freshly formatted disk
        JMP     MEMPTR_FormatBuf

;
; Format buffer. In the Atom ROM version of this code, we use the normal workspace at $2000
; however as this code is loaded at $2800, leaving the format buffer at $2800 means that 
; filling it will over-write our code!
; We therefore need to set the format buffer to some space beyond our code, so we have 
; sufficient space.
;

.MEMPTR_FormatBuf
        LDY     #<FormatBuf
        STY     MEMPTR                     
        LDA     #>FormatBuf
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

if(TRACKS=40)
		lda		#>TotalTrk40				; set no of tracks
		sta		WKSecCOpt
		lda		#<TotalTrk40
		sta		WKSecCLSB
else
		lda		#>TotalTrk80				; set no of tracks
		sta		WKSecCOpt
		lda		#<TotalTrk80
		sta		WKSecCLSB
endif		
		jmp     write_cat               	; Write catalog back to disk
        
;
; Generate a byte stream in memory that represents a raw track.
;

TrackPtr	= TEMP2
SecPtr		= TEMP1
SecIDPtr	= WORK

SecMarker	= $CC							; Sector marker within table, replaced with sector id
TrkMarker	= $DD							; Track marker within table, replaced with track id
EndMarker	= $63							; end of table marker

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

; sector byte count	= $151 (337) 
; track byte count	= $D2A (3370) + runoff......


; sector ID's for correct interleave.		
.INTERLS                               
		EQUB	0,4,7,1,5,8,2,6,9,3		; 1:3 interleave
.FOLDEND


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

; write X copies of byte in A to buffer
		
.FD3                                    
        STA     (MEMPTR),Y                 	; save byte in format buffer
        INY                             	; inc buffer pointer
        DEX                             	; decrement count
        BNE     FD3                     	; more loop again
		
        JSR     INCTrkPtr                  	; TrkPointer = TrkPointer + 2
        JSR     INCTrkPtr                   
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
		STA		VTEMP
		LDA		VTEMP
.FD10    					                               
        STA     (MEMPTR),Y  				; write bytes to buffer               
        INY                             
        DEX                             
        BNE     FD10                    
        RTS     							; return to caller

;
; Write track to disk 
;

.WRITE_TRACK                                
        LDA     #WWTRCC              	; Write track command 
        STA     WDCOMM					; Save command for error handler
        STA     WCMD                  	; Send it to WD
.SWT2                                   
        BIT     PCTRL                   ; N=b7, V=b6 of PCTRL
        BVS     END_WRITE               ; INTRQ set, end writr
        BPL     SWT2                    ; Loop if no DRQ

        LDA     (MEMPTR),Y            	; DRQ: send a byte to WD     
        STA     WDATA                   
        INY                             ; increment pointer
        BNE     SWT2                    
        INC     MEMPTR+1                   

        JMP     SWT2                    ; Go do next byte
        
;        LDA     PCTRL 					; Get control register
;        AND     #PINTRQ                 ; INTRQ yet?
;        BEQ     SWT2                    

.END_WRITE        
        LDA     WSTATUS        		    ; Get status from WD
        STA     DSTATUS           	    ; save it for error handlers      

.SWT4                                   
        JSR     ERRTYPE2               	; NMI breaks out here, handle error
        BCS     SWT7                    
.SWT6                                   
        STY     MEMPTR                 	; Display address of last byte written    
        LDA     MEMPTR+1                   
        JSR     PRINT_HEXA                   
        LDA     MEMPTR                     
        JSR     PRINT_HEXA                   
        JSR     LPRSPACE              
        CLC                           	; Flag no error  
.SWT7                                   
        RTS                             
        
.INCTrkPtr
		inc		TrackPtr				; increment track pointer
		bne		INCTrkPtrExit
		inc		TrackPtr+1
.INCTrkPtrExit
		rts
	
; Copied from Atom ROM
;    Increment the Vector (#5A),X subroutine
;    ---------------------------------------

;.INC5AX
;        inc     $5A, x		;
;        bne     LF670		;
;        inc     $5B, x		;
;.LF670
;        rts			        
endif

;                       
; Verify command, also called after format.
;

.verifycom                                 
		jsr		START_MOTOR_SELECT		; select drive and turn it on
        JSR     RESTORE0             	; Restore to track 0
		JSR		MEMPTR_WKBASE			; Setup memory pointer

.VX3                                    
        LDA     #SECTRACK				; get sectors / track                  
        STA     TEMP1+1                  
        JSR     LPRSPACE                ; Print a space
        LDA     TRACKNO                 ; Get track number
        STA     WTRACK               	; send to WD 
        JSR     PRINT_HEXA              ; print it in hex
        LDY     #0                      
        STY     TEMP1                    

        LDA     #5  					; 5 retries                    
        STA     VTEMP                   
		
.VX2                                    
        LDA     #WRDRSC       			; Read sector addresses           
        STA		WDCOMM					; Save command for error handler
        STA     WCMD                    ; Send to WD
.VX5    
        BIT     PCTRL                   ; N=b7, V=b6 of PCTRL
        BVS     VX_INTRQ                ; INTRQ set, end read
        BPL     VX5                     ; loop if no DRQ
        
        LDA     WDATA                   ; Get data from WD
        STA     (MEMPTR),Y              ; Save in memory
        INY                            	; increment pointer 
        BNE     VX5                     

.VX_WAIT_INTRQ                                    
        LDA     PCTRL                   ; Get control register
        AND     #PINTRQ                 ; INTRQ yet?
        BEQ     VX_WAIT_INTRQ           ; Keep waiting

.VX_INTRQ
		LDA     WSTATUS        		    ; Get status from WD
        STA     DSTATUS           	    ; save it for error handlers      
		
.VERIFY_NMI                                    
        LDY     TEMP1                   ; Command completed
        INY                             
        INY                             
        INY                             
        STY     TEMP1                    
        LDA     DSTATUS                 ; get WD status
        AND     #$1C                    ; check for error
        BEQ     VX21                    ; none : skip
		
        DEC     VTEMP                   ; dec retry count
        BEQ     VX22                    ; All retries exhausted, skip on
        BNE     VX2                     ; try again

.VX21                                   
        DEC     TEMP1+1                  ; decrement sector header count
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
        INC     TEMP1+1          		; increment sector count        
        LDA     #SECTRACK		     	; check done all?             
        CMP     TEMP1+1                  
        BNE     VX11                    ; nope : loop again
		
        LDX     #0						; zero sector count                      
        STX     TEMP1+1                  
.VX7                                    
        LDA     VerifyBuf+1,X     		; get sector id from buffer         
        BEQ     VX1                     ; zero 
        INC     TEMP1+1                  ; increment sector count
.VX1                                    
        INX                             ; increment pointer
        CPX     #SECTRACK				; loop if more                  
        BNE     VX7                     
		
        LDA     #12                     
        STA     TEMP1                    
        LSR     TEMP1                    
.VX13                                   
        LDA     TEMP1+1                  
        CMP     TEMP1                    
        BCS     VX8                     
.VX22                                   
        LDA     #'?'    				; Error, print ?              
        JSR     OSWRCH                 	; write it
        JMP     VX83                   	; skip on
		
.VX8    JSR     LPRSPACE                ; print space
.VX83   LDA     TRACKNO                 ; Get track number
        CMP     #TRACKS                 ; done all?
        BCS     VX4                     ; yep : exit
        JSR     STEPIN                  ; step to next 
        JMP     VX3                     ; loop again
.VX4                                    
        JSR     INLINE_PRINT          	; Print success INLINE_PRINT
		EQUB	CR,LF,"VERIFIED",CR,LF      
        NOP                             
        JMP     RESTORE0              	; Restore to track 0 and exit


.LPRSPACE 
		lda     #SPACE                    
        jmp     OSWRCH               
        

.codeend

print "TRACKS=",TRACKS
print "FORMAT=",FORMAT

if(FORMAT)
  if(TRACKS=40)
    save "FORM40.DFS",codestart,codeend,codestart
  else
    save "FORM80.DFS",codestart,codeend,codestart
  endif
else
  if(TRACKS=40)
    save "VERIFY4.DFS",codestart,codeend,codestart
  else
    save "VERIFY8.DFS",codestart,codeend,codestart
  endif    
endif  

;
; Here we include and assemble the system rom, this way we can access it's symbols
; however the assembled copy is not saved (as ISROM=0)
;
        SYS40   = 1
        include "../src/sys5-1f-1770.asm"
		
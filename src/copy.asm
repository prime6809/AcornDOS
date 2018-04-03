;
; copy a logical disk from one drive to another.
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

BufBASE		= BASE+$0400		; buffer used for copying sectors
BufMSB		= >BufBASE			; High byte (page)
BufLSB		= <BufBASE			; Low byte offset

;
; With the maximum RAM available, and the above buffer base the maximum buffer size will be
; $E000 - $2C00 = $B400 bytes.
; Since sectors are $100 bytes long this gives us a maximum of $B4 (180) sectors, or 18 tracks.
; 
; Defined in platinclude.asm
; 

.BeebDisStartAddr
        LDA     CMDLINE+2					; Get 3rd character from command line
        CMP     #$0D						; EOL?
        BNE     ErrorExit					; nope, too many characters, error: exit

        LDA     CMDLINE						; Get first drive no as ASCII
        JSR     DriveNoConvert				; Convert it to binary

        STA     FromDrive					; save it					
        LDA     CMDLINE+1					; Get second drive no as ASCII
        JSR     DriveNoConvert				; Convert it to binary

        STA     ToDrive						; save it
        CMP     FromDrive					; Check that source <> destination
        BEQ     ErrorExit					; if so error!

        JSR     INLINE_PRINT                ; print from message
if(ATOM=1)
		EQUS    "COPYING FROM DRIVE "
else
        EQUS    "Copying from drive "
endif
        NOP
        LDA     FromDrive                 	; get source drive
        JSR     PRINT_HEXA_LOWN             ; print it

        JSR     INLINE_PRINT                ; print to message
if(ATOM=1)
        EQUS    " TO DRIVE "
else
        EQUS    " to drive "
endif
        NOP
        LDA     ToDrive                   	; get dest drive 
        JSR     PRINT_HEXA_LOWN             ; print it

        JSR     OSCRLF                      ; EOL

        JSR     OSRDCH                      ; read keyboard

        CMP     #$1B                        ; escape?
        BNE     DoCopy                      ; nope : go do copy

        RTS

;
; Validate supplied ASCII drive number in A
; Exits program with an error if invalid
; Returns binary drive number if valid.
;
.DriveNoConvert
        CMP     #$30						; Supplied code smaller than '0'?
        BCC     ErrorExit					; Yep : error

        CMP     #$34						; Supplied code greater than '3'?
        BCS     ErrorExit					; Yep : error

        AND     #$03						; convert to binary
        RTS


.ErrorExit
        JSR     INLINE_PRINT
if(ATOM=1)
        EQUS    "COPY PARAMETERS"
else
        EQUS    "COPY parameters"
endif
		BRK
		
.QuitTooSmall
        JSR		INLINE_PRINT
if(ATOM=1)
        EQUS    "DESTINATION TOO SMALL"
else
        EQUS    "Destination too small"
endif
		BRK

.DoCopy
        LDA     #$00                        ; Zero load address, filesize and start sector

        STA     LOADADDR
        STA     FILESIZE
        STA     STARTSEC
        STA     STARTSEC+1
		
        LDA     #SectorsToCopy              ; MSB of filesize, read $32 (50) sectors, 5 track's worth
        STA     FILESIZE+1
        LDA     FromDrive                   ; select drive to copy from
        STA     DRIVENO

		JSR     load_cat_always             ; Unconditionally load catalog
        JSR     WAIT_NOT_BUSY               ; wait for drive

        LDA     WKSecCLSB                   ; Get sector count of source drive MSB
        STA     SecCountSrcLSB
        LDA     WKSecCOpt                   ; LSB
        AND     #$0F                        ; Mask out opt bits
        STA     SecCountSrcMSB
        LDA     WKSecCOpt                   ; Get opt of source
        AND     #$F0                        ; mask out sector count bits
        STA     OptSrc                      ; save it

        LDA     ToDrive                     ; Select destination drive
        STA     DRIVENO

        JSR     load_cat                    ; load it's catalog
        JSR     WAIT_NOT_BUSY               ; wait for drive

        LDA     WKSecCOpt                   ; Get sector count of destination drive MSB
        AND     #$0F                        ; Mask out opt bits
        CMP     SecCountSrcMSB              ; Compare to source count MSB
        BCC     QuitTooSmall

        BNE     L28DF						; Dest is bigger, no need to check LSB

        LDA     WKSecCLSB                   ; get LSB of destination sector count
        CMP     SecCountSrcLSB              ; smaller than source
        BCC     QuitTooSmall                ; yep: smaller so not enough room, quit

.L28DF
        LDA     FromDrive                   ; Get source drive id
        STA     DRIVENO

        LDA     #BufMSB                     ; Point at sector buffer
        STA     LOADADDR+1

        JSR     START_MOTOR_SELECT          ; start drive motor
        JSR     ROM_READ_SECTORS            ; Read sectors
        JSR     WAIT_NOT_BUSY               ; wait for drive

        LDA     #BufMSB                     ; Reset buffer pointer
        STA     LOADADDR+1

        LDA     ToDrive                     ; get destination drive
        STA     DRIVENO

        JSR     START_MOTOR_SELECT          ; start drive motor
        JSR     ROM_WRITE_SECTORS           ; write sectors	
        JSR     WAIT_NOT_BUSY               ; wait for drive

        LDA     STARTSEC+1                  ; get LSB of start sector
        CLC                                 ; setup carry
        ADC     #SectorsToCopy              ; add no of sectors copied
        STA     STARTSEC+1                  ; resave
        BCC     L290E                       ; no carry skip

        INC     STARTSEC                    ; increment MSB

; check to see if we have processed all sectors on source disk
.L290E
        LDA     STARTSEC                    ; Check MSB first
        CMP     SecCountSrcMSB
        BCC     L28DF					 	; still more to go loop again

        LDA     STARTSEC+1                  ; check LSB
        CMP     SecCountSrcLSB				
        BNE     L28DF					    ; still more to go loop again

        LDA     WKSecCOpt                   ; Get sec count MSB, save on stack
        PHA
        LDA     WKSecCLSB                   ; and LSB
        PHA
        JSR     load_cat_always             ; load dest catalog

        JSR     WAIT_NOT_BUSY               ; wait for disk

        PLA									; restore sec count LSB
        STA     WKSecCLSB

        PLA
        AND     #$0F                        ; mask out opt bits
        ORA     OptSrc                      ; combine with cat opt bits
        STA     WKSecCOpt
        JSR     write_cat                   ; write catalog back to disk

        JSR     WAIT_NOT_BUSY               ; wait for disk

        JSR     catcom2						; display catalog

        RTS

.FromDrive
        EQUB    $00
.ToDrive
        EQUB    $00
.SecCountSrcLSB
        EQUB    $00
.SecCountSrcMSB
        EQUB    $00
.OptSrc        
		EQUB	$00

.BeebDisEndAddr
SAVE "COPY.DFS",BeebDisStartAddr,BeebDisEndAddr

;
; Here we include and assemble the system rom, this way we can access it's symbols
; however the assembled copy is not saved (as ISROM=0)
;
        include "..\src\rominclude.asm"

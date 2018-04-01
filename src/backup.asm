;
; Backup a disk from one drive to another.
; Copies both sides of a physical disk.
;

		include "../src/platinclude.asm"
		include "../src/wdfdc.asm"
		include "../src/intelfdc.asm"
	
L214F   = $214F

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

TracksToCopy    = 5
SectorsToCopy   = (TracksToCopy*10)        ; Copy blocks of 50 sectors at once (5 tracks worth).

.BeebDisStartAddr
        NOP
        JSR     OSCRLF                      ; Print EOL

        JSR     INLINE_PRINT                ; Signon message
if(ATOM=1)
        EQUS    "SOURCE DRIVE ?"
else
        EQUS    "Source drive ?"
endif
        NOP
        JSR     OSRDCH                      ; Read source drive

        PHA                                 ; save it
        JSR     OSCRLF                      ; print EOL

        PLA                                 ; restore source drive
        CMP     #$30                        ; drive '0' ?
        BEQ     L2827                       ; yep valid : continue

        CMP     #$31                        ; Drive '1' ?
        BEQ     L285C                       ; yep valid : continue

        RTS

.L2827
        LDA     #$00                        ; Set drive 0 as source
        STA     SourceDrive
        LDA     #$01                        ; Set drive 1 as destination
        STA     DestDrive
        JSR     ConfirmMessage              ; Print confirmation message

        JSR     OSRDCH                      ; read a character

        CMP     #$1B                        ; Escape?
        BEQ     L285B                       ; yep : abort to OS

        JSR     OSCRLF                      ; print EOL

        LDA     #$00                        ; backup Drive 0 to 1 (side 0 of physical drive)
        STA     FromDrive
        LDA     #$01
        STA     ToDrive
        JSR     DoBackup

        LDA     #$02                        ; backup Drive 2 to 3 (side 1 of physical drive)
        STA     FromDrive
        LDA     #$03
        STA     ToDrive
        JSR     DoBackup

        CLC
.L2859
        BCC     BeebDisStartAddr

.L285B
        RTS

.L285C
        LDA     #$01                        ; Set drive 1 as source
        STA     SourceDrive
        LDA     #$00
        STA     DestDrive                   ; Set drive 0 as destination
        JSR     ConfirmMessage              ; Print confirmation message

        JSR     OSRDCH                      ; read a character

        CMP     #$1B                        ; Escape?
        BEQ     L285B                       ; yep : abort to OS

        JSR     OSCRLF                      ; print EOL

        LDA     #$01                        ; backup Drive 1 to 0 (side 0 of physical drive)
        STA     FromDrive
        LDA     #$00
        STA     ToDrive
        JSR     DoBackup

        LDA     #$03                        ; backup Drive 3 to 2 (side 1 of physical drive)      
        STA     FromDrive
        LDA     #$02
        STA     ToDrive
        JSR     DoBackup

        CLC                                 ; Force branch to beginnig, go again
        BCC     L2859

; Print confirmation message including physical drove numbers
.ConfirmMessage
        JSR     INLINE_PRINT                ; print message
if(ATOM=1)
        EQUS    "BACKING UP FROM DRIVE "
else
        EQUS    "Backing up from drive "
endif
        NOP                                 ; print source drive
        LDA     SourceDrive
        JSR     PRINT_HEXA_LOWN

        JSR     INLINE_PRINT                ; print message
if(ATOM=1)
        EQUS    " TO DRIVE "                
else
        EQUS    " to drive "                
endif
        NOP                                 ; print destination drive
        LDA     DestDrive
        JSR     PRINT_HEXA_LOWN

        JSR     INLINE_PRINT                ; print ok? message

        EQUS    " OK ? "
        NOP
        RTS


.QuitTooSmall
        JSR     INLINE_PRINT                ; Error : destination too small.
if(ATOM=1)
        EQUS    "DESTINATION TOO SMALL"
else
        EQUS    "Destination too small"
endif
		BRK

; Do the actual backup operation between the two logical drives specified in FromDrive and ToDrive

.DoBackup
        LDA     #$00                        ; Zero load address, filesize and start sector
.L28EA
        STA     LOADADDR
.L28EC
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
        BCC     QuitTooSmall                ; Smaller : quit not enough room

        BNE     L2936                       ; Dest is bigger, no need to check LSB

        LDA     WKSecCLSB                   ; get LSB of destination sector count
        CMP     SecCountSrcLSB              ; smaller than source
        BCC     QuitTooSmall                ; yep: smaller so not enough room, quit

.L2936
        LDA     FromDrive                   ; Get source drive id
        STA     DRIVENO
        
        LDA     #BufMSB                     ; Pint at sector buffer
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
        BCC     L2965                       ; no carry skip

        INC     STARTSEC                    ; increment MSB

; check to see if we have processed all sectors on source disk
.L2965
        LDA     STARTSEC                    ; Check MSB first
        CMP     SecCountSrcMSB
        BCC     L2936                       ; still more to go loop again

        LDA     STARTSEC+1                  ; check LSB
        CMP     SecCountSrcLSB
        BNE     L2936                       ; still more to go loop again

        LDA     WKSecCOpt                   ; Get sec count MSB, save on stack
        PHA
        LDA     WKSecCLSB                   ; and LSB
        PHA
        JSR     load_cat_always             ; load dest catalog

        JSR     WAIT_NOT_BUSY               ; wait for disk

        PLA                                 ; restore sec count LSB
; not sure about next line, this will be in the middle of the file entry for file 11, I think it should
; be WKSecCLSB, as it is in the otherwise identical section of source in the COPY command.        
        STA     L214F
        PLA
        AND     #$0F                        ; mask out opt bits
        ORA     OptSrc                      ; combine with cat opt bits
        STA     WKSecCOpt
        JSR     write_cat                   ; write catalog back to disk

        JSR     WAIT_NOT_BUSY               ; wait for disk

        RTS                                 ; done

.FromDrive
        NOP
.ToDrive
        NOP
.SecCountSrcLSB
        NOP
.SecCountSrcMSB
        NOP
.OptSrc
        NOP
.SourceDrive
        NOP
.DestDrive
        NOP
if (WD1770)		
.DestTrack
        EQUB    $00                         ; saved track for writing
endif
        
.BeebDisEndAddr

SAVE "BACKUP.DFS",BeebDisStartAddr,BeebDisEndAddr

;
; Here we include and assemble the system rom, this way we can access it's symbols
; however the assembled copy is not saved (as ISROM=0)
;
        include "..\src\rominclude.asm"

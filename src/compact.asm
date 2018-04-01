;
; AcornDOS compact command.
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


FileSecCountLSB   	= STARTSEC+2
FileSecCountMSB	  	= FILENAME

SourceStartSecLSB 	= FILENAME+1
SourceStartSecMSB 	= FILENAME+2

DestStartSecLSB		= FILENAME+3	; LSB and MSB of current move to sector ?
DestStartSecMSB   	= FILENAME+4

DirPtr   			= FILENAME+5	; Current dir pointer whilst scanning


.BeebDisStartAddr
        JSR     INLINE_PRINT                ; Print signon message
if(ATOM=1)
        EQUS    "COMPACTING DRIVE "
else
        EQUS    "Compacting drive "
endif
        NOP
        LDA     DRIVENO                     ; Get drive number
        JSR     PRINT_HEXA_LOWN             ; print it

        JSR     OSCRLF                      ; and eol

        JSR     OSRDCH                      ; read keyboad

        CMP     #$1B                        ; escape pressed?
        BNE     L2825                       ; nope, continue and compact

        RTS                                 ; exit

.L2825
        JSR     load_cat                    ; load catalog from disk
        JSR     WAIT_NOT_BUSY               ; wait for disk not busy

        LDY     WKFileCount                 ; Get no of files on disk 
        STY     DirPtr                      ; save it away

; In itialize the point that we will move files down to, this is effectively the last sector
; of the previous file + 1 (initialized to the sector after the catalog)
        
        LDA     #$02                        ; first data sector LBA?
        STA     DestStartSecLSB
        
        LDA     #$00                        
        STA     DestStartSecMSB

.CompactNext
        LDY     DirPtr                      ; get saved no files on disk
        JSR     decy8                       ; y=y-8

        CPY     #$F8                        ; done all files?
        BNE     L2844                       ; nope continue

        JMP     CompactDone                 ; quit, all files processsed

.L2844
        STY     DirPtr                      ; save current dir entry pointer
        JSR     print_info                  ; print it's info

        LDY     DirPtr                      ; Get dir entry pointer
        LDA     WKStartSec1,Y               ; get extra bits of cat entry

; Check to see if the file has any data by checking that at least one of the file length bits is set        
        
        AND     #$F0                        ; mask out all but length bits
        ORA     WKLength1+1,Y               ; combine with rest of file length bits
        ORA     WKLength1,Y
        BNE     L285B                       ; file has data (as some bits set).

        JMP     L28EE                       ; file has no data no need to move anything

.L285B
        LDA     #$00                        ; Zero totals
        STA     LOADADDR
        STA     FILESIZE
        LDA     #$FF                        ; setup carry for addition
        CLC

; since the DFS file length is 18 bits long, but DFS sectors are 256 bytes, we can treat the most
; significant 10 bits as a sector count. However if there are any bytes in the LSB we need to add 
; one to the count of sectors to account for the partially filled sector at the end of the file.
        
        ADC     WKLength1,Y                 ; Any carry from file len?  (bytes in last sector?)                
        LDA     #$00
        ADC     WKLength1+1,Y
        STA     FileSecCountLSB             ; save it
        
        LDA     WKStartSec1,Y               ; get file length MS bits
        PHP                                 ; save carry flag
        LSR     A                           ; shift MS bits of length into bits 0..3
        LSR     A
        LSR     A
        LSR     A
        PLP                                 ; restore carry
        ADC     #$00                        ; add carry
        
        STA     FileSecCountMSB             ; save it
        
        LDA     WKStartSec1+1,Y             ; get LSB of start sector
        STA     SourceStartSecLSB           ; save it
        
        LDA     WKStartSec1,Y               ; get MSB of start sector
        AND     #$0F                        ; mask out length bits
        STA     SourceStartSecMSB           ; save it
        
        
		LDA     DestStartSecLSB				; get saved previous first free sector
        STA     WKStartSec1+1,Y				; update our catalog entry
        LDA     WKStartSec1,Y				; merge in MS 4 bits
        AND     #$F0
        ORA     DestStartSecMSB
        STA     WKStartSec1,Y				; and update cat entry
	
; Work out how many sectors to copy, the file is moved in blocks of $14 (20) sectors, so 2 
; track's worth, until there are less than $14 sectors.
;
; while sectors to copy do
;  if sectors > $14 then
;    move $14 sectors; tocopy=tocopy-$14
;  else
;    move remainder sectors; tocopy=tocopy-remainder
; done
	
.L2896
        LDA     FileSecCountLSB				; get sector count LSB
        TAY									; save it for potential later use
        CMP     #$14						; more than 14 sectors?
        LDA     FileSecCountMSB				; get MSB
        SBC     #$00						; sub with carry, 
        BCC     L28A3						; less than $14, use count in Y

        LDY     #$14						; more than (or equal to) $14, reset count in y

; At this point we have the number of sectors to move in Y
		
.L28A3
        STY     FILESIZE+1					; set filesize to MSB to Y, as we are copying full sectors
        LDA     SourceStartSecLSB			; update start sec for read from saved start sec LSB
        STA     STARTSEC+1
        LDA     SourceStartSecMSB			; and MSB
        STA     STARTSEC

        LDA     #BufMSB						; Set buffer address for read
        STA     LOADADDR+1
        JSR     ROM_READ_SECTORS			; Read sectors

        JSR     WAIT_NOT_BUSY				; Wait for drive to be available

        LDA     DestStartSecLSB				; get destination start DestStartSec LSB
        STA     STARTSEC+1
        LDA     DestStartSecMSB				; and MSB
        STA     STARTSEC
		
        LDA     #BufMSB						; Set buffer address for write
        STA     LOADADDR+1
        JSR     ROM_WRITE_SECTORS			; Write sectors

        JSR     WAIT_NOT_BUSY				; Wait for drive to be available

        LDA     FILESIZE+1					; Increment destination sector pointer
        CLC
        ADC     DestStartSecLSB
        STA     DestStartSecLSB
        BCC     L28D4

        INC     DestStartSecMSB
.L28D4
        LDA     FILESIZE+1					; increment source sector pointer
        CLC
        ADC     SourceStartSecLSB
        STA     SourceStartSecLSB
        BCC     L28DF

        INC     SourceStartSecMSB
.L28DF
        SEC
        LDA     FileSecCountLSB				; Decrement filesize by ammount transferred
        SBC     FILESIZE+1
        STA     FileSecCountLSB
        BCS     L28EA

        DEC     FileSecCountMSB
.L28EA
        ORA     FileSecCountMSB				; more sectors to do ?
        BNE     L2896						; yep do them

.L28EE
        JSR     write_cat					; Save updated count

        JSR     WAIT_NOT_BUSY				; Wait for drive to be available

        LDY     DirPtr						; Print updated catalog info
        JSR     print_info

        JMP     CompactNext					; Go check the next one

.CompactDone
        JSR     INLINE_PRINT				; print finishing message
if(ATOM=1)
        EQUS    "DISK COMPACTED "
else
        EQUS    "Disk Compacted "
endif

.L290E
        NOP
        SEC									; setup carry for calculation
        LDA     WKSecCLSB					; get LSB of disk sector count
        SBC     DestStartSecLSB				; subtract last occupide sector
        PHA									; save low byte of count
		
        LDA     WKSecCOpt					; Do MSB
        AND     #$0F						; mask out non count bits
        SBC     DestStartSecMSB				; do subtract
        JSR     PRINT_HEXA_LOWN				; print MSB

        PLA									; restore LSB
        JSR     PRINT_HEXA					; and print it.

        JSR     INLINE_PRINT				; print message
if(ATOM=1)
        EQUS    " FREE SECTORS"
else
        EQUS    " free sectors"
endif
        NOP									; and EOL, rts will return to OS.		
        JMP     OSCRLF

.BeebDisEndAddr

SAVE "COMPACT.DFS",BeebDisStartAddr,BeebDisEndAddr

;
; Here we include and assemble the system rom, this way we can access it's symbols
; however the assembled copy is not saved (as ISROM=0)
;
        include "..\src\rominclude.asm"

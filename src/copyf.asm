;
; copy files from one logical disk to another.
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
        EQUS    "COPYING FILES FROM DRIVE "
else
        EQUS    "Copying files from drive "
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

        JMP     FileError

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
.L2865
        RTS

.ErrorExit
        JSR     INLINE_PRINT
if(ATOM=1)
        EQUS    "COPYF PARAMETERS"
else
        EQUS    "COPYF parameters"
endif
        BRK

.FileError
        JSR     INLINE_PRINT				; Prompt for filename
if(ATOM=1)
        EQUS    "?FILE?"
else
        EQUS    "?file?"
endif
        NOP
        JSR     READ_LINE					; read filename to $100-$140

        LDA     STACKPAGE					; point at beginning of name
        CMP     #$0D						; just enter : null filename
        BEQ     L2865						; yep: quit

        LDA     FromDrive					; set from drive as current
        STA     DRIVENO	
        LDY     #$00
        JSR     GET_CHK_FNAME  				; Check to see if filename is in catalog

        JSR     set_qual_to_use				; Make USE qualifier equal to SET qualifier						

        LDA     USEQUAL						; Make SET qualifier equal to USE!!!!
        STA     SETQUAL
		
        LDX     #$00						; Copy filename & qual
.L28A1
        LDA     WKFileName1,Y				; Copy filename
        STA     FILENAME,X
        LDA     WKLoadAddr1,Y				; copy file data
        STA     FILENAMEPTR+1,X
        STA     L2BF8,X
        INX									; increment pointers
        INY
        CPX     #$08						; done all bytes?
        BNE     L28A1						; nope loop again

        LDA     ToDrive						; make ToDrive current
        STA     DRIVENO
		
        LDA     USEQUAL						; set use qualifier
        PHA
        AND     #$7F
        STA     USEQUAL
        JSR     FILEINCAT					; Check to see if file is in catalog, already exists on dest?

        BCC     L28C8						; nope : skip

        JSR     rem_cat_entry				; exists, delete it!

.L28C8
        PLA									; restore use qualifier
        STA     USEQUAL
        LDA     FILESIZE+1					; MSB of filesize
        JSR     diva16						; divide by 16

        STA     STARTSEC+1					; set start sector
        LDA     #$02
        STA     STARTSEC
        LDA     #$00						
        STA     FILESIZE+1					

        LDY     WKFileCount					; get count of files on dest disk
        BEQ     L2914						; none : skip ahead

        CPY     #$F8						; Max number of files?
        BCC     L28E6						; no skip					

        JMP     DISK_FULL					; Yep: print message and quit

.L28E6
        JSR     L2A00

        JMP     L28F2

.L28EC
        JSR     decy8

        JSR     L29E1

.L28F2
        TYA
        BEQ     L28F7

        BCC     L28EC

.L28F7
        BCS     L28FC

        JMP     NO_ROOM					; Print "no room"

.L28FC
        STY     TEXTPTR
        LDY     WKFileCount
.L2901
        CPY     TEXTPTR
        BEQ     L2914

        LDA     WKTitle+7,Y
        STA     WKFileDir1,Y
        LDA     WKSecCLSB,Y
        STA     WKStartSec1+1,Y
        DEY
        BCS     L2901

.L2914
        LDX     #$00
        LDA     STARTSEC+1
        ASL     A
        ASL     A
        ASL     A
        ASL     A
        ORA     FILESIZE+1
        STA     FILESIZE+1
.L2920
        LDA     FILENAME,X				; copy filename to catalog, 8 bytes
        STA     WKFileName1,Y
        INY
        INX
        CPX     #$08
        BNE     L2920

.L292B
        LDA     FILENAMEPTR,X			; copy file data to catalog.
        DEY
        STA     WKLoadAddr1,Y
        DEX
        BNE     L292B

        JSR     print_info				; Print copied file info

        LDY     WKFileCount				; update disk filecount
        JSR     incy8

        STY     WKFileCount				
        JSR     write_cat				; write catalog back to dest disk

        JSR     WAIT_NOT_BUSY			; Wait for disk

        JSR     set_qual_to_use			; set qualifier

        LDA     EXECADDR+1
        CLC
        ADC     #$FF
        LDA     FILESIZE
        ADC     #$00
        STA     STARTSEC+2
        LDA     STARTSEC+1
        ADC     #$00
        STA     FILENAME
        LDA     L2BFF
        STA     FILENAME+1
        LDA     L2BFE
        AND     #$0F
        STA     FILENAME+2
        LDA     FILESIZE+1
        AND     #$0F
        STA     FILENAME+4
        LDA     STARTSEC
        STA     FILENAME+3

        LDA     #BufLSB						; Set LSB of buffer
        STA     LOADADDR
        STA     FILESIZE
		
.L2976
        LDA     STARTSEC+2					; Check sectors left to copy
        TAY
        CMP     #CopyBuffSize				; More than copy buffer?
        LDA     FILENAME	
        SBC     #$00
        BCC     L2983						; no set it

        LDY     #CopyBuffSize				; get CopyBuffSize worth of sectors
.L2983
        STY     FILESIZE+1
        LDA     FILENAME+1
        STA     STARTSEC+1
        LDA     FILENAME+2
        STA     STARTSEC
		
		LDA     #BufMSB                     ; Point at sector buffer
        STA     LOADADDR+1

        LDA     FromDrive                   ; Get source drive id
        STA     DRIVENO

        JSR     START_MOTOR_SELECT          ; start drive motor
        JSR     ROM_READ_SECTORS            ; Read sectors
        JSR     WAIT_NOT_BUSY               ; wait for drive

        LDA     FILENAME+3
        STA     STARTSEC+1
        LDA     FILENAME+4
        STA     STARTSEC
        LDA     #BufMSB                     ; Point at sector buffer
        STA     LOADADDR+1
        LDA     ToDrive                     ; get destination drive
        STA     DRIVENO

        JSR     START_MOTOR_SELECT          ; start drive motor
        JSR     ROM_WRITE_SECTORS           ; write sectors
        JSR     WAIT_NOT_BUSY               ; wait for drive

        LDA     FILESIZE+1
        CLC
        ADC     FILENAME+3
        STA     FILENAME+3
        BCC     L29C4

        INC     FILENAME+4
.L29C4
        LDA     FILESIZE+1
        CLC
        ADC     FILENAME+1
        STA     FILENAME+1
        BCC     L29CF

        INC     FILENAME+2
.L29CF
        SEC
        LDA     STARTSEC+2
        SBC     FILESIZE+1
        STA     STARTSEC+2
        BCS     L29DA

        DEC     FILENAME
.L29DA
        ORA     FILENAME
        BNE     L2976

        JMP     FileError

.L29E1
        LDA     WKStartSec1,Y
        JSR     diva16

        STA     FILESIZE+1
        CLC
        LDA     #$FF
        ADC     WKLength1,Y
        LDA     WKStartSec1+1,Y
        ADC     WKLength1+1,Y
        STA     STARTSEC
        LDA     WKStartSec1,Y
        AND     #$0F
        ADC     FILESIZE+1
        STA     FILESIZE+1

.L2A00
        SEC
        LDA     WKSecCLSB,Y					; get LSB of disk sector count
        SBC     STARTSEC					; take startsec LSB
        PHA			
		
        LDA     WKSecCOpt,Y					; Get MSB of sector count
        AND     #$0F						; Mask out opt bits
        SBC     FILESIZE+1					; take MSB of 
        TAX
		
        LDA     #$00						
        CMP     EXECADDR+1
        PLA
        SBC     FILESIZE
        TXA
        SBC     STARTSEC+1
        RTS

.FromDrive
        BRK
.ToDrive
        EQUB    $00

skipto $2BF8
.L2BF8	EQUB	0	
skipto $2BFE
.L2BFE	EQUB	0
.L2BFF	EQUB	0
   


.BeebDisEndAddr

SAVE "COPYF.DFS",BeebDisStartAddr,BeebDisEndAddr

;
; Here we include and assemble the system rom, this way we can access it's symbols
; however the assembled copy is not saved (as ISROM=0)
;
        include "../src/rominclude.asm"

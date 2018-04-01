;
; Defines for the Atom.
;

CR			= $0D                                  
LF			= $0A  
CLS			= $0C                               
SPACE		= $20                               
DUBQUOTE	= $22

TITLELEN	= $0C						; length of disk title
SECTRACK	= $0A						; Sectors per track
RETRIES		= $0A						; number of times to retry
FIRSTSEC	= $00						; Sector number of first sector

MOTOR_ON	= $80						; Motor is on flag in DRIVENO

TOTAL40		= 40 * SECTRACK				; Total sectors on 40 track disk
TOTAL80		= 80 * SECTRACK				; Total sectors on 80 track disk

;
; Low RAM vars.
;
SAVTRK0		= &90						; Saved current track for each drive.										
SAVTRK1		= &91										
SAVTRK2		= &92										
SAVTRK3		= &93										

FILENAMEPTR = $9A						; Pointer to filename
LOADADDR	= $9C						; Load address
EXECADDR	= $9E						; Execution address
FILESIZE	= $A0						; File size
STARTSEC	= $A2						; Location on disk high byte, low byte
FILENAME	= $A5						; Current filename
USEQUAL		= $AC						; Current USE qualifier
FDCSAVCMD	= $AD						; Saved FDC Command
CATFILENAME	= $AE						; File being cataloged

TEMP1  		= $B5                            
TEMP2  		= $B7                            
VTEMP  		= $B9

CATINDEX	= $B7						; Catalog index during *CAT scan / print.
CATEOLFLAG	= $B8						; Catalog EOL flag
EXECHANDLE	= $B9						; File handle for *EXEC
SPOOLHANDLE	= $BA						; File handle for *SPOOL
SAVWRCVEC	= $BB						; Saved write vector in *SPOOL
SAVRDCVEC	= $BD						; Saved read vector in *EXEC
EXESPOOLIDX	= $BF						; Saved exe / spool index
SAVDRIVENO	= $C7						; Saved drive number
SAVQUAL		= $C8						; Saved qualifier
STARTADDR	= $C9						; Start address for load/save
SSECCOUNT	= $CB						; Number of sectors to save / load
SECC256FLAG	= $CC						; Sector count > 256 (MSB of count?)
SETQUAL		= $CD						; Current SET qualifier
LINESCROLL	= &CE						; &CE     lines left before scrolling - 24-(text Y position)
SCREENTOP	= &D0						; &D0     top of displayed screen
SCREENXPOS	= &D1						; &D1     text X position, b7=1 if VDU disabled
SCREENADDR	= &D2						; &D2/&D3 screen output address
SCROFFSET	= &D4						; &D4     offset into screen memory
INTJMP		= $D5						; indirect jump to int routine (8271)
NMI    		= $D5      					; NMI routine to be jumped to on command compeltion.                      

INBUFIDX	= $E9						; Saved input buffer index
TEMP		= $E9

TEXTPTR		= $EA						; Pointer to text for inline print
TRACKNO		= $EC						; Disk track number
SECTORNO	= $ED						; Disk sector no
DRIVENO		= $EE						; Drive number in bits 0,1, catalog read in bit 7
MONFLAG		= $EF						; MON / NOMON flag, 0=mon, nonzero=nomon

RETRYCOUNT	= $F0						; Number of retries
SECCOUNT	= $F1						; Count of sectors to transfer? (ados, ados1770)
DENSITY		= $F1                       ; Single=1, Double=0 (gdos)        
TRANSFER	= $F2						; Disk transfer routine in low ram (8271) 
DSTATUS		= $F2      					; Saved status of last disk operation
NTRC   		= $F3                   	; Tracks per disk         
TMPWORK		= $F4
WDCOMM		= $F5						; saved wd command byte.
MEMPTR		= $F6						; Pointer to byte to load / save in TRANSFER area
TYPE  		= $F8						; ???? (gdos)                            

STACKPAGE	= $100						; Last (lowest) byte of stack page

CMDLINE		= $140						; command line pointer (on stack)

; Workspace 
; In the original Acorn DOS the workspace was set to $2000 as this is
; where the additional RAM on the Acorn floppy controller is mapped.
;
; I have hopefully found and changed all refferences to $2000-$2800
; to use the symbolic versions to alow easy re-configuration for 
; a different block of memory.
;

WKBASE		= $2000			; Base of workspace
WKHIGH		= >WKBASE		; High byte
WKLOW		= <WKBASE		; low byte

	print "wkhigh=",~WKHIGH
	print "wklow=",~WKLOW

if (lo(WKBASE) != 0)
	Error "Error WKBASE must be on page boundry!"
endif

;
; Standard offsets in workspace, taken from chapter 6 of "The Advanced Disk 
; Users Guide" for the BBC as AtomDOS and DFS are mostly the same.
;

WKTitle		= WKBASE + $0000	; Disk Title

; 31 filename entries
WKFileName1 = WKBASE + $0008	; Filename
WKFileDir1	= WKBASE + $000F	; Directory
WKFileName2 = WKBASE + $0010	; Filename
WKFileDir2	= WKBASE + $0017	; Directory
WKFileName3 = WKBASE + $0018	; Filename
WKFileDir3	= WKBASE + $001F	; Directory
WKFileName4 = WKBASE + $0020	; Filename
WKFileDir4	= WKBASE + $0027	; Directory
WKFileName5 = WKBASE + $0028	; Filename
WKFileDir5	= WKBASE + $002F	; Directory
WKFileName6 = WKBASE + $0030	; Filename
WKFileDir6	= WKBASE + $0037	; Directory
WKFileName7 = WKBASE + $0038	; Filename
WKFileDir7	= WKBASE + $003F	; Directory
WKFileName8 = WKBASE + $0040	; Filename
WKFileDir8	= WKBASE + $0047	; Directory
WKFileName9 = WKBASE + $0048	; Filename
WKFileDir9	= WKBASE + $004F	; Directory
WKFileName10= WKBASE + $0050	; Filename
WKFileDir10	= WKBASE + $0057	; Directory
WKFileName11= WKBASE + $0058	; Filename
WKFileDir11	= WKBASE + $005F	; Directory
WKFileName12= WKBASE + $0060	; Filename
WKFileDir12	= WKBASE + $0067	; Directory
WKFileName13= WKBASE + $0068	; Filename
WKFileDir13	= WKBASE + $006F	; Directory
WKFileName14= WKBASE + $0070	; Filename
WKFileDir14	= WKBASE + $0077	; Directory
WKFileName15= WKBASE + $0078	; Filename
WKFileDir15	= WKBASE + $007F	; Directory
WKFileName16= WKBASE + $0080	; Filename
WKFileDir16	= WKBASE + $0087	; Directory
WKFileName17= WKBASE + $0088	; Filename
WKFileDir17	= WKBASE + $008F	; Directory
WKFileName18= WKBASE + $0090	; Filename
WKFileDir18	= WKBASE + $0097	; Directory
WKFileName19= WKBASE + $0098	; Filename
WKFileDir19	= WKBASE + $009F	; Directory
WKFileName20= WKBASE + $00A0	; Filename
WKFileDir20	= WKBASE + $00A7	; Directory
WKFileName21= WKBASE + $00A8	; Filename
WKFileDir21	= WKBASE + $00AF	; Directory
WKFileName22= WKBASE + $00B0	; Filename
WKFileDir22	= WKBASE + $00B7	; Directory
WKFileName23= WKBASE + $00B8	; Filename
WKFileDir23	= WKBASE + $00BF	; Directory
WKFileName24= WKBASE + $00C0	; Filename
WKFileDir24	= WKBASE + $00C7	; Directory
WKFileName25= WKBASE + $00C8	; Filename
WKFileDir25	= WKBASE + $00CF	; Directory
WKFileName26= WKBASE + $00D0	; Filename
WKFileDir26	= WKBASE + $00D7	; Directory
WKFileName27= WKBASE + $00D8	; Filename
WKFileDir27	= WKBASE + $00DF	; Directory
WKFileName28= WKBASE + $00E0	; Filename
WKFileDir28	= WKBASE + $00E7	; Directory
WKFileName29= WKBASE + $00E8	; Filename
WKFileDir29	= WKBASE + $00EF	; Directory
WKFileName30= WKBASE + $00F0	; Filename
WKFileDir30	= WKBASE + $00F7	; Directory
WKFileName31= WKBASE + $00F8	; Filename
WKFileDir31	= WKBASE + $00FF	; Directory

WKTitle2	= WKBASE + $0100	; Last 4 bytes of title
WKCycle		= WKBASE + $0104	; Cycle count, number of updates since format
WKFileCount	= WKBASE + $0105	; Number of files on disk * 8	
WKSecCOpt	= WKBASE + $0106	; Sector count MSB bits 1,0, opt in bits 5,6
WKSecCLSB	= WKBASE + $0107	; Sector count LSB	

; 31 File data entries
; note byte 1 of the start sector is broken down as follows :
; bits	meaning
; 7,6	MS bits of Exec address
; 5,4	MS bits of file length
; 3,2 	MS bits of load address
; 1,0	MS bits of start sector
WKLoadAddr1	= WKBASE + $0108	; Load address
WKExecAddr1	= WKBASE + $010A	; Execution address
WKLength1	= WKBASE + $010C	; File length
WKStartSec1	= WKBASE + $010E	; Start sector, 10 bit

WKLoadAddr2	= WKBASE + $0110	; Load address
WKExecAddr2	= WKBASE + $0112	; Execution address
WKLength2	= WKBASE + $0114	; File length
WKStartSec2	= WKBASE + $0116	; Start sector, 10 bit
WKLoadAddr3	= WKBASE + $0118	; Load address
WKExecAddr3	= WKBASE + $011A	; Execution address
WKLength3	= WKBASE + $011C	; File length
WKStartSec3	= WKBASE + $011E	; Start sector, 10 bit

WKLoadAddr4	= WKBASE + $0120	; Load address
WKExecAddr4	= WKBASE + $0122	; Execution address
WKLength4	= WKBASE + $0124	; File length
WKStartSec4	= WKBASE + $0126	; Start sector, 10 bit
WKLoadAddr5	= WKBASE + $0128	; Load address
WKExecAddr5	= WKBASE + $012A	; Execution address
WKLength5	= WKBASE + $012C	; File length
WKStartSec5	= WKBASE + $012E	; Start sector, 10 bit

WKLoadAddr6	= WKBASE + $0130	; Load address
WKExecAddr6	= WKBASE + $0132	; Execution address
WKLength6	= WKBASE + $0134	; File length
WKStartSec6	= WKBASE + $0136	; Start sector, 10 bit
WKLoadAddr7	= WKBASE + $0138	; Load address
WKExecAddr7	= WKBASE + $013A	; Execution address
WKLength7	= WKBASE + $013C	; File length
WKStartSec7	= WKBASE + $013E	; Start sector, 10 bit

WKLoadAddr8	= WKBASE + $0140	; Load address
WKExecAddr8	= WKBASE + $0142	; Execution address
WKLength8	= WKBASE + $0144	; File length
WKStartSec8	= WKBASE + $0146	; Start sector, 10 bit
WKLoadAddr9	= WKBASE + $0148	; Load address
WKExecAddr9	= WKBASE + $014A	; Execution address
WKLength9	= WKBASE + $014C	; File length
WKStartSec9	= WKBASE + $014E	; Start sector, 10 bit

WKLoadAddr10= WKBASE + $0150	; Load address
WKExecAddr10= WKBASE + $0152	; Execution address
WKLength10	= WKBASE + $0154	; File length
WKStartSec10= WKBASE + $0156	; Start sector, 10 bit
WKLoadAddr11= WKBASE + $0158	; Load address
WKExecAddr11= WKBASE + $015A	; Execution address
WKLength11	= WKBASE + $015C	; File length
WKStartSec11= WKBASE + $015E	; Start sector, 10 bit

WKLoadAddr12= WKBASE + $0160	; Load address
WKExecAddr12= WKBASE + $0162	; Execution address
WKLength12	= WKBASE + $0164	; File length
WKStartSec12= WKBASE + $0166	; Start sector, 10 bit
WKLoadAddr13= WKBASE + $0168	; Load address
WKExecAddr13= WKBASE + $016A	; Execution address
WKLength13	= WKBASE + $016C	; File length
WKStartSec13= WKBASE + $016E	; Start sector, 10 bit

WKLoadAddr14= WKBASE + $0170	; Load address
WKExecAddr14= WKBASE + $0172	; Execution address
WKLength14	= WKBASE + $0174	; File length
WKStartSec14= WKBASE + $0176	; Start sector, 10 bit
WKLoadAddr15= WKBASE + $0178	; Load address
WKExecAddr15= WKBASE + $017A	; Execution address
WKLength15	= WKBASE + $017C	; File length
WKStartSec15= WKBASE + $017E	; Start sector, 10 bit

WKLoadAddr16= WKBASE + $0180	; Load address
WKExecAddr16= WKBASE + $0182	; Execution address
WKLength16	= WKBASE + $0184	; File length
WKStartSec16= WKBASE + $0186	; Start sector, 10 bit
WKLoadAddr17= WKBASE + $0188	; Load address
WKExecAddr17= WKBASE + $018A	; Execution address
WKLength17	= WKBASE + $018C	; File length
WKStartSec17= WKBASE + $018E	; Start sector, 10 bit

WKLoadAddr18= WKBASE + $0190	; Load address
WKExecAddr18= WKBASE + $0192	; Execution address
WKLength18	= WKBASE + $0194	; File length
WKStartSec18= WKBASE + $0196	; Start sector, 10 bit
WKLoadAddr19= WKBASE + $0198	; Load address
WKExecAddr19= WKBASE + $019A	; Execution address
WKLength19	= WKBASE + $019C	; File length
WKStartSec19= WKBASE + $019E	; Start sector, 10 bit

WKLoadAddr20= WKBASE + $01A0	; Load address
WKExecAddr20= WKBASE + $01A2	; Execution address
WKLength20	= WKBASE + $01A4	; File length
WKStartSec20= WKBASE + $01A6	; Start sector, 10 bit
WKLoadAddr21= WKBASE + $01A8	; Load address
WKExecAddr21= WKBASE + $01AA	; Execution address
WKLength21	= WKBASE + $01AC	; File length
WKStartSec21= WKBASE + $01AE	; Start sector, 10 bit

WKLoadAddr22= WKBASE + $01B0	; Load address
WKExecAddr22= WKBASE + $01B2	; Execution address
WKLength22	= WKBASE + $01B4	; File length
WKStartSec22= WKBASE + $01B6	; Start sector, 10 bit
WKLoadAddr23= WKBASE + $01B8	; Load address
WKExecAddr23= WKBASE + $01BA	; Execution address
WKLength23	= WKBASE + $01BC	; File length
WKStartSec23= WKBASE + $01BE	; Start sector, 10 bit

WKLoadAddr24= WKBASE + $01C0	; Load address
WKExecAddr24= WKBASE + $01C2	; Execution address
WKLength24	= WKBASE + $01C4	; File length
WKStartSec24= WKBASE + $01C6	; Start sector, 10 bit
WKLoadAddr25= WKBASE + $01C8	; Load address
WKExecAddr25= WKBASE + $01CA	; Execution address
WKLength25	= WKBASE + $01CC	; File length
WKStartSec25= WKBASE + $01CE	; Start sector, 10 bit

WKLoadAddr26= WKBASE + $01D0	; Load address
WKExecAddr26= WKBASE + $01D2	; Execution address
WKLength26	= WKBASE + $01D4	; File length
WKStartSec26= WKBASE + $01D6	; Start sector, 10 bit
WKLoadAddr27= WKBASE + $01D8	; Load address
WKExecAddr27= WKBASE + $01DA	; Execution address
WKLength27	= WKBASE + $01DC	; File length
WKStartSec27= WKBASE + $01DE	; Start sector, 10 bit

WKLoadAddr28= WKBASE + $01E0	; Load address
WKExecAddr28= WKBASE + $01E2	; Execution address
WKLength28	= WKBASE + $01E4	; File length
WKStartSec28= WKBASE + $01E6	; Start sector, 10 bit
WKLoadAddr29= WKBASE + $01E8	; Load address
WKExecAddr29= WKBASE + $01EA	; Execution address
WKLength29	= WKBASE + $01EC	; File length
WKStartSec29= WKBASE + $01EE	; Start sector, 10 bit

WKLoadAddr30= WKBASE + $01F0	; Load address
WKExecAddr30= WKBASE + $01F2	; Execution address
WKLength30	= WKBASE + $01F4	; File length
WKStartSec30= WKBASE + $01F6	; Start sector, 10 bit
WKLoadAddr31= WKBASE + $01F8	; Load address
WKExecAddr31= WKBASE + $01FA	; Execution address
WKLength31	= WKBASE + $01FC	; File length
WKStartSec31= WKBASE + $01FE	; Start sector, 10 bit

VerifyBuf	= WKBASE + $0100	; Buffer used during verify.

TotalTrk40  = (40*10)           ; Total sectors, 40 tracks
TotalTrk80  = (80*10)           ; Total sectors, 80 tracks

NMIVEC  = $0200 	; NMI service routine
BRKVEC  = $0202		; BRK service routine
IRQVEC	= $0204		; IRQ service routine
COMVEC  = $0206		; Command Line Interpreter
WRCVEC  = $0208 	; Write character
RDCVEC  = $020A 	; Read character
LODVEC  = $020C 	; Load file
SAVVEC  = $020E 	; Save file
RDRVEC  = $0210 	; Read open file info
STRVEC  = $0212 	; Write open file info
BGTVEC  = $0214 	; Get byte from open file
BPTVEC  = $0216 	; Put byte to open file
FNDVEC  = $0218 	; Open a file
SHTVEC  = $021A 	; Close a file
IN1VEC  = $021C 	; Used by Econet
IN2VEC  = $021E

VECBASE	= NMIVEC	; Base of vectors

INC5AX	= $F671		; increment pointer at ($5A),x

OSSHUT	= $FFCB		; Close a file
OSFIND  = $FFCE		; Open a file
OSBPUT	= $FFD1		; Put a byte to open file
OSBGET	= $FFD4   	; Get a byte from open file

OSSTAR  = $FFD7 	; Write open file info
OSRDAR  = $FFDA 	; Read open file info
OSSAVE  = $FFDD 	; Save a file
OSLOAD  = $FFE0 	; Load a file
OSRDCH  = $FFE3 	; Read character
OSECHO  = $FFE6 	; Read character and echo
OSASCI  = $FFE9 	; Write ASCII sequence
OSCRLF0 = $FFEB		; IF >0 then OSWRCH else OSCRLF
OSCRLF  = $FFED 	; Write LF-CR Newline sequence
OSWRCR  = $FFF2 	; Write CR
OSWRCH  = $FFF4 	; Write a character
OSCLI   = $FFF7 	; Command Line Interpreter
NMIV    = $FFFA 	; Hardware NMI vector
RESETV  = $FFFC 	; Hardware Reset vector
IRQV    = $FFFE 	; Hardware IRQ/BRK vector

;
; Atom ROM entry points (unofficial).
;

ENDCOM 			= $C558
prtdec			= $c589        
READ_LINE		= $CD0F          
PRINT_HEXA		= $F802			; Print contents of a as hex.
;HEXNIBOUTA		= $F80B  		; print lower nibble of a as hex
PRINT_HEXA_LOWN	= $F80B  		; print lower nibble of a as hex
SKIPSPACE 		= $F876                   
GETKEY 			= $FE71                   
PRTCHAR 		= $FE94   
DELAY  			= $FB83  
SYS_INLINE_PRINT= $F7D1			; System rom version of inline print 

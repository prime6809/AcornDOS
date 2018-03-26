;
; Acorn System defines
;
CR			= $0D                                  
LF			= $0A   
CTRLX		= $18                              
SPACE		= $20                               
DUBQUOTE	= $22
DELETE		= $7F

TITLELEN	= $0C						; length of disk title
SECTRACK	= $0A						; Sectors per track
RETRIES		= $0A						; number of times to retry

MOTOR_ON	= $80						; Motor is on flag in DRIVENO


.start
.entry
; MOS entry addresses
; -------------------
;OSSHUT=&FFCB
;OSFIND=&FFCE
;OSBPUT=&FFD1
;OSBGET=&FFD4
;OSSTAR=&FFD7
;OSRDAR=&FFDA
;OSSAVE=&FFDD
;OSLOAD=&FFE0
;OSRDCH=&FFE3
;OSECHO=&FFE6
;OSASCI=&FFE9
;OSNEWL=&FFED
;OSWRCR=&FFF2
;OSWRCH=&FFF4
;OS_CLI=&FFF7
:
; Vectors
; -------
nmiv 	= &0200
brkv 	= &0202
irqv 	= &0204
cliv 	= &0206
wrchv	= &0208
rdchv	= &020A
loadv	= &020C
savev	= &020E
rdarv	= &0210
starv	= &0212
bgetv	= &0214
bputv	= &0216
findv	= &0218
shutv	= &021A

										;
										; I/O addresses
										; -------------
										; &0E21 - keypress, b7=1 if nothing pressed
										;                   b6-b0=ASCII keypress
										;
										; &1000-&17FF Screen memory
										; &1840       CRTC register
										; &1841       CRTC data
										;
										; Memory map
										; ----------     
VTEMP       = &A0                       ; Temporary pointers used during format track generation.
TEMP1       = &A2
TEMP2       = &A4
WORK        = &A6

SAVTRK0		= &A8						; Saved current track for each drive.										
SAVTRK1		= &A9										
SAVTRK2		= &AA										
SAVTRK3		= &AB										
                                        
CATFILENAME	= &AE						; &AE-&B5 DOS: current filename being catalogued
FDCSAVCMD	= &AD						; &AD	  Saved FDC command
										; &B6     DOS, used in *DIR
CATINDEX	= $B7						;
CATEOLFLAG	= &B8						; &B8 Catalog EOL flag
EXECHANDLE	= &B9						; &B9     DOS: EXEC handle
SPOOLHANDLE	= &BA						; &BA     DOS: SPOOL handle
SAVWRCVEC	= &BB						; &BB-&BC DOS: saved RDCHV
										; &BD-&BE DOS: saved WRCHV
										; &BF
										; &C0
										; &C1
										; &C2     DOS
										; &C3
										; &C4     DOS
										;
SAVDRIVENO	= &C7						; &C7     DOS: Saved drive
SAVQUAL		= &C8						; &C8     Saved qualifier
STARTADDR	= &C9						; &C9/&CA Start address for load/save
SSECCOUNT	= &CB						; &CB 	  Saved sector count
SECC256FLAG	= &CC						; &CC	  Sector count > 256 (MSB of sector no?)
SETQUAL		= &CD						; &CD     DOS: Saved SET/USE prefix
LINESCROLL	= &CE						; &CE     lines left before scrolling - 24-(text Y position)
										; &CF     used by VDU
SCREENTOP	= &D0						; &D0     top of displayed screen
SCREENXPOS	= &D1						; &D1     text X position, b7=1 if VDU disabled
SCREENADDR	= &D2						; &D2/&D3 screen output address
SCROFFSET	= &D4						; &D4     offset into screen memory
INTJMP		= &D5						; &D5/&D6 Indirect int vector
										; &D6     written to by DOS
INBUFIDX	= &D7						; &D7     TEMPY temporary store
TEMPY		= INBUFIDX
TEXTPTR		= &D8						; &D8/&D9 inline text address
TRACKNO		= &DA						; &DA     DOS: Track number
SECTORNO	= &DB						; &DB     DOS: Sector no to start reading at (0..10)
DRIVENO		= &DC						; &DC     DOS: current drive
										;                                       OSFILE control block
FILENAMEPTR	= &DD						; &DD-&DE DOS: OSCLI dispatch address   Filename address
LOADADDR	= &DF						; &DF     DOS: scanned address b0-b7    load address b0-b7
										; &E0     DOS: scanned address b8-b15   load address b8-b15
EXECADDR	= &E1						; &E1     DOS: scanned address b16-b23  exec address b0-b7/load flag
										; &E2                                   exec address b8-b15
FILESIZE	= &E3						; &E3     DOS: Filesize b0-b7           start address b0-b7
										; &E4     DOS: Filesize b8-b15          start address b8-b15
STARTSEC	= &E5						; &E5     DOS: Start sector b15-b8      end address b0-b7
										; &E6     DOS: Start sector b7-b0       end address b8-b15
										; &E7
FILENAME	= &E8						; &E8-&EE DOS: filename
USEQUAL		= &EF						; &EF     DOS: current SET/USE prefix
MONFLAG		= &F0						; &F0     DOS: MON/NOMON flag
RETRYCOUNT 	= &F1						; &F1     DOS count of retries on error
SECCOUNT	= &F2						; &F2     DOS: sector count
TRANSFER	= &F3						; &F3-&FD NMI code space (8271 only)
WDCOMM		= $F5						; saved wd command byte.
MEMPTR		= &F7						; &F7/&F8 NMI data transfer address
DSTATUS		= $FD      					; Saved status of last disk operation
PRINTIGNORE	= &FE						; &FE     printer ignore character
TEMPA		= &FF						; &FF     interrupt TEMPA

STACKPAGE	= $100						; Last (lowest) byte of stack page

CMDLINE		= $140						; command line pointer

;=======================================
; 6522 VIA, on system board. 
;=======================================

SVIABASE	= &0E20
SVIARB		= SVIABASE + &0				; Data register port B
SVIARA		= SVIABASE + &1				; Data register port A
SVIADDRB	= SVIABASE + &2				; Data direction port B
SVIADDRA	= SVIABASE + &3				; Data direction port A
SVIAT1CL	= SVIABASE + &4				; Timer 1 low order latches / counter
SVIAT1CH	= SVIABASE + &5				; Timer 1 high order counter
SVIAT1LL	= SVIABASE + &6				; Timer 1 low order latches
SVIAT1LH	= SVIABASE + &7				; Timer 1 high order latches
SVIAT2CL	= SVIABASE + &8				; Timer 2 low order latches / counter
SVIAT2CH	= SVIABASE + &9				; Timer 2 high order counter
SVIASR		= SVIABASE + &A				; Shift register
SVIAACR		= SVIABASE + &B				; Aux control register
SVIAPCR		= SVIABASE + &C				; Peripheral control register
SVIAIFR		= SVIABASE + &D				; Interrupt flag register
SVIAIER		= SVIABASE + &E				; Interrupt enable register
SVIARAH		= SVIABASE + &F				; Same as Data A but without handshake

;=======================================
; 6522 VIA, on printer board 
;=======================================

PVIABASE	= &0C00
PVIARB		= PVIABASE + &0				; Data register port B
PVIARA		= PVIABASE + &1				; Data register port A
PVIADDRB	= PVIABASE + &2				; Data direction port B
PVIADDRA	= PVIABASE + &3				; Data direction port A
PVIAT1CL	= PVIABASE + &4				; Timer 1 low order latches / counter
PVIAT1CH	= PVIABASE + &5				; Timer 1 high order counter
PVIAT1LL	= PVIABASE + &6				; Timer 1 low order latches
PVIAT1LH	= PVIABASE + &7				; Timer 1 high order latches
PVIAT2CL	= PVIABASE + &8				; Timer 2 low order latches / counter
PVIAT2CH	= PVIABASE + &9				; Timer 2 high order counter
PVIASR		= PVIABASE + &A				; Shift register
PVIAACR		= PVIABASE + &B				; Aux control register
PVIAPCR		= PVIABASE + &C				; Peripheral control register
PVIAIFR		= PVIABASE + &D				; Interrupt flag register
PVIAIER		= PVIABASE + &E				; Interrupt enable register
PVIARAH		= PVIABASE + &F				; Same as Data A but without handshake

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

TotalTrk40  = (40*10)           ; Total sectors, 40 tracks
TotalTrk80  = (80*10)           ; Total sectors, 80 tracks

if(0)
if(ISROM=0)

;
; Only include these if not assembling rom
;

OSBRK  = $FFC8 ; Pass on to BRKV
OSSHUT = $FFCB ; Close file
OSFIND = $FFCE ; Open file
OSBPUT = $FFD1 ; Put byte to file
OSBGET = $FFD4 ; Get byte from file
OSSTAR = $FFD7 ; Set file arguments
OSRDAR = $FFDA ; Read file arguments
OSSAVE = $FFDD ; Save file
OSLOAD = $FFE0 ; Load file
OSRDCH = $FFE3 ; Read character from input
OSECHO = $FFE6 ; Read character and echo to output
OSASCI = $FFE9 ; Write ASCII character
OSCRLF = $FFED ; Write newline sequence
OSWRCR = $FFF2 ; Write carriage return
OSWRCH = $FFF4 ; Write character to output
OS_CLI = $FFF7 ; Command line interpreter

NMIV   = $FFFA ; NMI vector
RSTV   = $FFFC ; RESET vector
IRQV   = $FFFE ; IRQ/BRK vector

endif
endif
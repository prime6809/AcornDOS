;
; Machine includes
;

;
; TracksToCopy sets the number of tracks that the BACKUP and COPY
; commands copy in one go. This is limited by the amount of 
; available memory, since each track is 10*256 byte sectors, it
; takes up 2.5K, so you should set it it dependent on the ammount of
; memory your machine has.
; To calculate the amount of available memory use :
; RAMTOP - BASE / 2560, and round down to the nearest integer.
; BASE will be $2C00 for the System and $2D00 for the Atom.
;
;
; Simarly to TracksToCopy, CopyBufferTracks is used by COPYF  and DUTY 
; to size their buffers
;

if(ATOM=1)
	include "../src/atomdefs.asm"
; Set SMALL = 1 if assembling for the original Atom FDC *WITHOUT* extra RAM expansion
; Set SMALL to 0 if assembling for the original Atom FDC with extra ram between 
; $4000 and	$5F00
; Since the GDOS-2015 has 32K of onboard RAM, it always uses the default TracksToCopy.	
	SMALL	= 1

	if ((WD1770=0) and (SMALL=1))
		TracksToCopy 		= 1
		CopyBufferTracks	= 1
	else
		TracksToCopy 		= 5
		CopyBufferTracks	= 1
	endif
else
	include "../src/sysdefs.asm"

	TracksToCopy 		= 5
	CopyBufferTracks	= 1
endif


SectorsToCopy   = (TracksToCopy*SECTRACK)        ; Copy blocks of 50 sectors at once (5 tracks worth).
CopyBuffSize	= (CopyBufferTracks*SECTRACK)	 ; Size of buffer for copy (2 tracks worth)

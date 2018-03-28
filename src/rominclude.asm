;
; Here we include and assemble the system rom, this way we can access it's symbols
; however the assembled copy is not saved (as ISROM=0)
;
        SYS40   = 1
if (WD1770)		
        include "..\src\sys5-1f-1770.asm"
else
        include "..\src\sys5-1f.asm"
endif

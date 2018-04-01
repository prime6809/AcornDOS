#
# Makefile for System utility programs Intel 8271 FDC
#
# This requires a version of BeebASM that supports command line defines with -D 
# BeebASM is available from : 
# http://www.retrosoftware.co.uk/wiki/index.php?title=BeebAsm
#

include ../vars.mk

DEFS=-D ATOM=0 -D ISROM=0 -D WD1770=0

# Main target build everything
all: utils format 

# Utility commands are FDC independednt, but must be linked against
# the appropreate version of the rom so entry points are correct.
# Format and Verify commands ARE dependent on the FDC and the rom 
# version.
utils: backup compact copy copyf duty 
format: format8271 verify8271 

#
# Utility commands
#
backup: $(SRCPATH)/backup.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/backup.asm $(DEFS) -v > $(LIST)/backup-8271-sys.txt

compact: $(SRCPATH)/compact.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/compact.asm $(DEFS) -v > $(LIST)/compact-8271-sys.txt
	
copy: $(SRCPATH)/copy.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/copy.asm $(DEFS) -v > $(LIST)/copy-8271-sys.txt
	
copyf: $(SRCPATH)/copyf.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/copyf.asm $(DEFS) -v > $(LIST)/copyf-8271-sys.txt

duty: $(SRCPATH)/duty.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/duty.asm $(DEFS) -v > $(LIST)/duty-8271-sys.txt

infall: $(SRCPATH)/infall.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/infall.asm $(DEFS) -v > $(LIST)/infall-8271-sys.txt
	
format8271: $(SRCPATH)/format-8271.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/format-8271.asm $(DEFS) -D FORMAT=1 -D TRACKS=80 -v > $(LIST)/form80-8271-sys.txt
	$(ASM) -i $(SRCPATH)/format-8271.asm $(DEFS) -D FORMAT=1 -D TRACKS=40 -v > $(LIST)/form40-8271-sys.txt

verify8271: $(SRCPATH)/format-8271.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/format-8271.asm $(DEFS) -D FORMAT=0 -D TRACKS=80 -v > $(LIST)/verify80-8271-sys.txt
	$(ASM) -i $(SRCPATH)/format-8271.asm $(DEFS) -D FORMAT=0 -D TRACKS=40 -v > $(LIST)/verify40-8271-sys.txt


clean:
	rm -f *.DFS	
	

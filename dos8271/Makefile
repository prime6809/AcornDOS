#
# Makefile for utility programs Intel 8271 FDC
#
# This requires a version of BeebASM that supports command line defines with -D 
# BeebASM is available from : 
# http://www.retrosoftware.co.uk/wiki/index.php?title=BeebAsm
#

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
	$(ASM) -i $(SRCPATH)/backup.asm -D ISROM=0 -D WD1770=0 -v > $(LIST)/backup-8271.txt

compact: $(SRCPATH)/compact.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/compact.asm -D ISROM=0 -D WD1770=0 -v > $(LIST)/compact-8271.txt
	
copy: $(SRCPATH)/copy.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/copy.asm -D ISROM=0 -D WD1770=0 -v > $(LIST)/copy-8271.txt
	
copyf: $(SRCPATH)/copyf.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/copyf.asm -D ISROM=0 -D WD1770=0 -v > $(LIST)/copyf-8271.txt

duty: $(SRCPATH)/duty.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/duty.asm -D ISROM=0 -D WD1770=0 -v > $(LIST)/duty-8271.txt

infall: $(SRCPATH)/infall.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/infall.asm -D ISROM=0 -D WD1770=0 -v > $(LIST)/infall-8271.txt
	
format8271: $(SRCPATH)/format-8271.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/format-8271.asm -D FORMAT=1 -D TRACKS=80 -D ISROM=0 -v > $(LIST)/form80-8271.txt
	$(ASM) -i $(SRCPATH)/format-8271.asm -D FORMAT=1 -D TRACKS=40 -D ISROM=0 -v > $(LIST)/form40-8271.txt

verify8271: $(SRCPATH)/format-8271.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/format-8271.asm -D FORMAT=0 -D TRACKS=80 -D ISROM=0 -v > $(LIST)/verify80-8271.txt
	$(ASM) -i $(SRCPATH)/format-8271.asm -D FORMAT=0 -D TRACKS=40 -D ISROM=0 -v > $(LIST)/verify40-8271.txt


clean:
	rm -f *.DFS	
	

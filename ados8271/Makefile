#
# Makefile for Atom utility programs Intel 8271 FDC
#
# This requires a version of BeebASM that supports command line defines with -D 
# BeebASM is available from : 
# http://www.retrosoftware.co.uk/wiki/index.php?title=BeebAsm
#

include ../vars.mk

DEFS=-D ATOM=1 -D ISROM=0 -D WD1770=0

# Main target build everything
all: utils format 

# Utility commands are FDC independednt, but must be linked against
# the appropreate version of the rom so entry points are correct.
# Format and Verify commands ARE dependent on the FDC and the rom 
# version.
utils: backup compact copy copyf duty sysinfo
format: format8271 verify8271 

#
# Utility commands
#
backup: $(SRCPATH)/backup.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/backup.asm $(DEFS) -v > $(LIST)/backup-8271-atom.txt

compact: $(SRCPATH)/compact.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/compact.asm $(DEFS) -v > $(LIST)/compact-8271-atom.txt
	
copy: $(SRCPATH)/copy.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/copy.asm $(DEFS) -v > $(LIST)/copy-8271-atom.txt
	
copyf: $(SRCPATH)/copyf.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/copyf.asm $(DEFS) -v > $(LIST)/copyf-8271-atom.txt

duty: $(SRCPATH)/duty.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/duty.asm $(DEFS) -v > $(LIST)/duty-8271-atom.txt

infall: $(SRCPATH)/infall.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/infall.asm $(DEFS) -v > $(LIST)/infall-8271-atom.txt
	
format8271: $(SRCPATH)/format-8271.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/format-8271.asm $(DEFS) -D FORMAT=1 -D TRACKS=80 -v > $(LIST)/form80-8271-atom.txt
	$(ASM) -i $(SRCPATH)/format-8271.asm $(DEFS) -D FORMAT=1 -D TRACKS=40 -v > $(LIST)/form40-8271-atom.txt

verify8271: $(SRCPATH)/format-8271.asm $(INCLUDES) $(ROMSRC)
	$(ASM) -i $(SRCPATH)/format-8271.asm $(DEFS) -D FORMAT=0 -D TRACKS=80 -v > $(LIST)/verify80-8271-atom.txt
	$(ASM) -i $(SRCPATH)/format-8271.asm $(DEFS) -D FORMAT=0 -D TRACKS=40 -v > $(LIST)/verify40-8271-atom.txt

sysinfo: $(SRCPATH)/sysinfo.asm $(INCLUDES)
	$(ASM) -i $(SRCPATH)/sysinfo.asm $(DEFS) -D ATM=0 -v > $(LIST)/sysinfo-8271-atom.txt


clean:
	rm -f *.DFS	
	

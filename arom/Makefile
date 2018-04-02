#
# Makefile for Atom AcornDOS ROMS.
#
# This requires a version of BeebASM that supports command line defines with -D 
# BeebASM is available from : 
# http://www.retrosoftware.co.uk/wiki/index.php?title=BeebAsm
#

include ../vars.mk

# roms, Build 40 and 80 column variants of system roms with both 
# floppy controllers
all: ados40 ados80 ados-novdg ados1770-bc00 ados1770-eff0 gdos1770-bc00 gdos1770-eff0 sysinfo buildrom

# roms 

ados1770-bc00: $(ROMSRC) $(INCLUDES)
	$(ASM) -i $(SRCPATH)/ados1770.asm -D IOBASE=\$$BC10 -D ISROM=1 -o ados1770-bc00.rom -v > $(LIST)/ados1770-bc10.txt

ados1770-eff0: $(ROMSRC) $(INCLUDES)
	$(ASM) -i $(SRCPATH)/ados1770.asm -D IOBASE=\$$EFF0 -D ISROM=1 -o ados1770-eff0.rom -v > $(LIST)/ados1770-eff0.txt

ados40: $(ROMSRC) $(INCLUDES)
	$(ASM) -i $(SRCPATH)/ados.asm -D INCLUDEVDG=1 -D SYS40=1 -D ISROM=1 -o ados-40.rom -v > $(LIST)/ados-40.txt

ados80: $(ROMSRC) $(INCLUDES)
	$(ASM) -i $(SRCPATH)/ados.asm -D INCLUDEVDG=1 -D SYS40=0 -D ISROM=1 -o ados-40.rom -v > $(LIST)/ados-80.txt

ados-novdg: $(ROMSRC) $(INCLUDES)
	$(ASM) -i $(SRCPATH)/ados.asm -D INCLUDEVDG=0 -D SYS40=1 -D ISROM=1 -o ados-40.rom -v > $(LIST)/ados-novdg.txt

gdos1770-bc00: $(ROMSRC) $(INCLUDES)
	$(ASM) -i $(SRCPATH)/gdos150.asm -D IOBASE=\$$BC10 -D ISROM=1 -o gdos1770-bc00.rom -v > $(LIST)/gdos1770-bc10.txt

gdos1770-eff0: $(ROMSRC) $(INCLUDES)
	$(ASM) -i $(SRCPATH)/gdos150.asm -D IOBASE=\$$EFF0 -D ISROM=1 -o gdos1770-eff0.rom -v > $(LIST)/gdos1770-eff0.txt

sysinfo: $(ROMSRC) $(INCLUDES)
	$(ASM) -i $(SRCPATH)/sysinfo.asm -D ATM=0 -D ISROM=1 -o sysinfoe0.rom -v > $(LIST)/sysinfo-rom.txt


buildrom: ados1770-bc00 ados1770-eff0 gdos1770-bc00 gdos1770-eff0 sysinfo
	sh build-rom.sh

clean:
	rm -f ados*.rom gdos*.rom sysinfo*.rom fdc*.rom blank*.rom
	

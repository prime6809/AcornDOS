#
# Variables
#

# System
SDOS1770=sdos1770
SDOS8271=sdos8271
SROM=srom
SROMSRC=$(SRCPATH)/sys5-1f.asm $(SRCPATH)/sys5-1f-1770.asm 
SINCLUDES=$(SRCPATH)/sysdefs.asm $(SRCPATH)/wdfdc.asm $(SRCPATH)/intelfdc.asm

# Atom
ADOS1770=ados1770
ADOS8271=ados8271
AROM=arom
AROMSRC=$(SRCPATH)/ados.asm $(SRCPATH)/ados1770.asm 
AINCLUDES=$(SRCPATH)/sysdefs.asm $(SRCPATH)/wdfdc.asm $(SRCPATH)/intelfdc.asm

# Common
LISTPATH=list
LIST="../$(LISTPATH)"
SRCPATH=../src
DISKPATH=disks

ASM=beebasm


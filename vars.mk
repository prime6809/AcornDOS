#
# Variables
#

DOS1770=dos1770
DOS8271=dos8271
LISTPATH=list
LIST="../$(LISTPATH)"
ROM=rom
SRCPATH=../src
DISKPATH=disks

ASM=beebasm

ROMSRC=$(SRCPATH)/sys5-1f.asm $(SRCPATH)/sys5-1f-1770.asm 
INCLUDES=$(SRCPATH)/sysdefs.asm $(SRCPATH)/wdfdc.asm $(SRCPATH)/intelfdc.asm

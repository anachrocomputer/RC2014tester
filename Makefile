# Makefile for 'tester.hex'

AS=z80asm
APPMAKE=appmake
PPZSUM=ppzsum

all: tester.hex

tester.hex: tester_INIT.bin
	$(APPMAKE) +hex --org 0 -b tester_INIT.bin -o tester.hex
#	$(PPZSUM) -ii <tester.hex >/dev/null

tester_INIT.bin: tester.asm cksum.asm
	$(AS) -b -d -s -l tester.asm

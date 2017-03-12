all: tester.hex

tester.hex: tester_INIT.bin
	appmake +hex -b tester_INIT.bin -o tester.hex

tester_INIT.bin: tester.asm
	z80asm -b -d -s -l tester.asm

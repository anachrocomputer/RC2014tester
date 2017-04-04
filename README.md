# RC2014 Test and Diagnostics EPROM

This Z80 assembly-language program will run on an RC2014 system
(either Mini or full-size version) and perform several diagnostic tests
on it.
The coding is tricky because we cannot assume that the RAM chip is
working properly, or even that the RAM is installed in the system.
So all the serial terminal I/O, EPROM tests and RAM tests must be
done without using any RAM.
This precludes the use of the normal subroutine calling mechanism
(CALL or RST) because we cannot use the stack.

## Initialisation

The code disables interrupts, selects Interrupt Mode 1, and
initialises the stack pointer.
The code then jumps over the Z80 restart locations to address 3Ah.

After jumping, the code initialises the 6850 ACIA.
It first sends 03h to the control register of the chip (at I/O address
80h) to cause a Master Reset.
This is essential for proper start-up of the ACIA.
Then, it sets the control register for divide-by-64 mode.

## Commands

D - hex dump<br>
E - EPROM checksum test<br>
F - fill RAM, 2000h-0ffffh<br>
H - help<br>
I - input from a given I/O address<br>
J - joystick test (press key to exit)<br>
O - Output a byte to a given I/O address<br>
R - RAM test, 8000h-0ffffh<br>
S - RAM freerun test, 8000h-0ffffh, slow (RESET to exit)<br>
T - RAM freerun test, 8000h-0ffffh, fast (RESET to exit)<br>
Z - fill RAM 8000h-0ffffh by LDIR copying

## The EPROM Checksum

The EPROM programmer (an old Stag PP39) reports a checksum when
it accepts a hex file over the serial port.
The Z80 code regenerates this checksum and prints it on the terminal.

The checksum is simply the 16-bit sum of all the bytes contained in the
EPROM, expressed in hex.
It is a very weak checksum, suitable only for detection of transmission
or programming errors.
It is not in any way secure against malicious alteration.
However, it is sufficient to verify that the Z80 CPU can read the EPROM.
By careful coding of some data areas in the EPROM, we can also be
reasonably sure that the EPROM addressing is OK.

## The RAM Tests

The main function of the EPROM is to test the RAM in the RC2014.
We need to test two things: data storage and correct addressing.
Data storage tests simply verify that we can read back the same data that
was written to the RAM.
Addressing tests verify that we can write and read back all 32768 bytes
as separate memory locations.
Data bus shorts or bad connections can cause the data storage tests
to fail, while address bus problems will cause the addressing tests to
fail.

## Building from Source

The hex output file, in Intel hex format, is provided for users who
wish to simply burn an EPROM and run the code.
This file is 'tester.hex' and will occupy 8k of space in the EPROM.

To build the software from source, you will need 'z80asm',
which is a Z80 assembler that accepts a slightly non-standard syntax
for certain directives.
It's installed as part of the usual RC2014 development system,
from the 'z88dk' SDK (originally written for use with the Sinclair Z88).
There's a 'Makefile' that will invoke the assembler and linker in the
correct way to generate 'tester.hex'.

There's a small shell script, 'topp39',
which I use to transfer the hex file
to my Stag PP39 EPROM programmer, via a USB-to-serial converter.
I also have a utility called 'ppzsum' that can generate EPROM checksums
from the hex file.
This is a non-standard utility,
so the call is commented-out in the 'Makefile'.

I intend to add an automatic checksum generation and insertion routine,
coded in Python.
This will allow the EPROM to contain its own checksum, so that the
test code can verify itself automatically.

## Assembler Directives

The 'z80asm' assembler accepts directives that are different from those
usually found in Z80 assemblers.
It uses ORG as usual.
In place of EQU, it uses DEFC; and in place of DB it has DEFM.
Similarly, DW is replaced by DEFW.
There are SECTION and MODULE directives, whose function is not clear.

As mentioned below, I use a subroutine calling convention that places
the return address in a register, not on the stack.
This means that I frequently need to refer to the Z80's program counter,
or the current assembly address.
'z80asm' uses ASMPC for this, whereas most Z80 assemblers use the
symbol '$'.

## Subroutine Calling Convention

The subroutines in this code are called by loading a 16-bit return
address and then jumping.
The address is placed in HL, IX or IY.
To return, the subroutine executes JP (HL), JP (IX) or JP (IY),
according to which register was used to pass the return link.

Some subroutines are duplicated in the code,
with different registers used as the return link.
This allows some flexibility when choosing whether to use, say HL as a
parameter register or as the return link.

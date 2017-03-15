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
H - help<br>
R - RAM test<br>
S - RAM freerun test, slow (RESET to exit)<br>
T - RAM freerun test, fast (RESET to exit)<br>

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

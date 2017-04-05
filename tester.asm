; tester.asm --- memory test and diagnostics for RC2014 Mini   2017-03-08  
; Copyright (c) 2017 John Honniball
;
; http://rc2014.co.uk

            section INIT
            module Tester

            defc INITSTACK=0ffffh
            
            defc ROMBASE=0000h
            defc ROMSIZE=8192
            
            defc RAMBASE=8000h
            defc RAMSIZE=32768
            defc RAMTOP=0ffffh
            
            defc DIGIN=00h      ; Digital input port
            defc DIGOUT=00h     ; Digital output port
            defc JOY1=01h       ; Joystick ports
            defc JOY2=02h
            defc ACIAS=80h      ; 6850 ACIA status register 80h-0BEh
            defc ACIAD=81h      ; 6850 ACIA data register   81h-0BFh
            
            defc CR=0Dh
            defc LF=0Ah
            defc SPACE=20h
            defc EOS=0
            defc PROMPT='>'
            
            org 0000h           ; EPROM (27C512) starts at 0x0000

RST00:      di                  ; Disable interrupts
            im 1                ; Select interrupt mode 1, jump to 0038h
            ld sp,INITSTACK     ; Initialise stack
            jr mcinit

RST08:      jp (hl)
            nop
            nop
            nop
            nop
            nop
            nop
            nop

RST10:      jp (hl)
            nop
            nop
            nop
            nop
            nop
            nop
            nop

RST18:      jp (hl)
            nop
            nop
            nop
            nop
            nop
            nop
            nop

RST20:      jp (hl)
            nop
            nop
            nop
            nop
            nop
            nop
            nop

RST28:      jp (hl)
            nop
            nop
            nop
            nop
            nop
            nop
            nop

RST30:      jp (hl)
            nop
            nop
            nop
            nop
            nop
            nop
            nop

RST38:      reti                ; Interrupt mode 1 address. Ignore and return

mcinit:     ld a,$03            ; Reset 6850 ACIA
            out (ACIAS),a
            ld a,$96            ; Initialise ACIA to divide-by-64
            out (ACIAS),a

            ; TODO: initialise all the other RC2014 I/O devices
            
setup:      ld hl,signon        ; Print sign-on message
            ld iy,ASMPC+7
            jp puts_iy
            
loop:       ld a,PROMPT         ; Print the prompt character
            ld iy,ASMPC+7
            jp t1ou_iy
            
            ld hl,ASMPC+6       ; Get a character from the ACIA
            jp t1in_hl
            
            ld iy,ASMPC+7       ; Echo command character
            jp t1ou_iy
            
            and 05fh            ; Make upper-case
            
            cp 'A'              ; Range check A..Z
            jp m,notaz
            cp 'Z'+1
            jp p,notaz
            
            sub 'A'             ; Get into range 0-25
            sla a               ; Multiply by 2

            ld bc,jmptab        ; BC->jmptab
            ld l,a              ; HL=A
            ld h,0
            add hl,bc           ; HL->jmptab+A
            ld c,(hl)
            inc hl
            ld b,(hl)           ; BC=jump address
            ld l,c              ; HL=BC
            ld h,b
            ld sp,ASMPC+4       ; Load return link
            jp (hl)             ; Call command subroutine at HL

notaz:      jp loop
            
signon:     defm  CR,LF
            defm  "RC2014 Memory Test and Diagnostics ROM",CR,LF
            defm  "V1.00 2017-03-08",CR,LF,EOS

; Jump table containing pointers to command subroutines
jmptab:     defw nosuchcmd      ; A
            defw nosuchcmd      ; B
            defw nosuchcmd      ; c
            defw Dcmd           ; D
            defw Ecmd           ; E
            defw Fcmd           ; F
            defw nosuchcmd      ; G
            defw Hcmd           ; H
            defw Icmd           ; I
            defw Jcmd           ; J
            defw nosuchcmd      ; K
            defw nosuchcmd      ; L
            defw nosuchcmd      ; M
            defw nosuchcmd      ; N
            defw Ocmd           ; O
            defw nosuchcmd      ; P
            defw nosuchcmd      ; Q
            defw Rcmd           ; R
            defw Scmd           ; S
            defw Tcmd           ; T
            defw Ucmd           ; U
            defw Vcmd           ; V
            defw nosuchcmd      ; W
            defw nosuchcmd      ; X
            defw Ycmd           ; Y
            defw Zcmd           ; Z
            
; nosuchcmd
; Print error message and return
; Entry: return link in SP
; Exit: HL and IY modified
nosuchcmd:  ld hl,nosuchmsg
            ld iy,ASMPC+7
            jp puts_iy
            
            ld hl,0             ; Clear HL
            add hl,sp           ; Effectively ld hl,sp
            jp (hl)             ; Effectively jp (sp)

nosuchmsg:  defm CR,LF,"Command not recognised",CR,LF,EOS

            
; Dcmd
; Print hex dump
; Entry: return link in SP
; Exit: A, BC, D, HL, IX and IY modified
Dcmd:       ld a,SPACE          ; Print a SPACE
            ld iy,ASMPC+7
            jp t1ou_iy

            ld iy,ASMPC+7       ; Get four hex digits into HL
            jp hex4in_iy
            jp Derr             ; Error return

            ld a,CR             ; CR/LF
            ld iy,ASMPC+7
            jp t1ou_iy
            
            ld a,LF
            ld iy,ASMPC+7
            jp t1ou_iy
            
D16rows:    ld d,16             ; Row counter
            
Drow:       ld a,CR             ; Print CR in case we're looping
            ld iy,ASMPC+7       ; back from the pager
            jp t1ou_iy
            
            ld ix,ASMPC+7       ; Print HL in 4-digit hex
            jp hex4out_ix
            
            ld a,SPACE          ; Print a blank
            ld iy,ASMPC+7
            jp t1ou_iy
            
            ld b,16             ; Byte counter
Dhex2:      ld a,(hl)           ; Load one byte
            ld iy,ASMPC+7
            jp hex2out_iy       ; Print as two-digit hex

            inc hl

            ld a,SPACE          ; Print a blank
            ld iy,ASMPC+7
            jp t1ou_iy
            
            djnz Dhex2          ; Go back for next byte
            
            ld bc,16
            and a               ; Clear carry
            sbc hl,bc           ; HL=HL-16
            
            ld b,16             ; Byte counter
Dhex3:      ld a,(hl)           ; Load one byte again
            cp SPACE            ; Range check 20h..7eh
            jp m,noprnt
            cp 07fh
            jp p,noprnt
            jr prntok

noprnt:     ld a,'.'            ; Substitute for non-printables
prntok:     ld iy,ASMPC+7
            jp t1ou_iy
            
            inc hl

            djnz Dhex3          ; Go back for next byte
            
            ld a,CR             ; CR/LF
            ld iy,ASMPC+7
            jp t1ou_iy
            
            ld a,LF
            ld iy,ASMPC+7
            jp t1ou_iy
            
            dec d               ; Decrement row counter
            jr nz,Drow          ; Go back for another row

pager:      ld a,':'            ; Pager prompt
            ld iy,ASMPC+7
            jp t1ou_iy
            
            ld iy,ASMPC+7       ; Get pager response
            jp t1in_iy
            
            cp SPACE            ; SPACE prints 16 more rows
            jp z,D16rows
            
            cp CR               ; CR prints one more row
            jp nz,notcr
            
            ld d,1              ; D is row counter
            jp Drow
            
notcr:      cp '?'              ; '?' prints help
            jp nz,nothelp
            
            ex de,hl
            ld hl,pagermsg      ; Print pager help message
            ld iy,ASMPC+7
            jp puts_iy
            ex de,hl

            jp pager
            
nothelp:    ld a,CR             ; CR to clear pager prompt
            ld iy,ASMPC+7
            jp t1ou_iy

Dret:       ld hl,0             ; Clear HL
            add hl,sp           ; Effectively ld hl,sp
            jp (hl)             ; Effectively jp (sp)
            
Derr:       ld a,'?'            ; Print '?' in case of input error
            ld hl,ASMPC+6
            jp t1ou_hl

            ld a,CR             ; Print CR/LF
            ld hl,ASMPC+6
            jp t1ou_hl
            
            ld a,LF
            ld hl,ASMPC+6
            jp t1ou_hl

            jp Dret

pagermsg:   defm CR,"SPACE=next page, CR=next line, q=exit",EOS

; Ecmd

Ecmd:       ld hl,chkmsg        ; Print EPROM checksum message
            ld iy,ASMPC+7
            jp puts_iy
            
            ld ix,ROMBASE       ; Initialise EPROM pointer
            ld bc,ROMSIZE       ; Initialise loop counter
            ld hl,0             ; Checksum accumulates in HL
            ld de,0             ; Bytes will get loaded into E
romchk:     ld e,(ix)           ; Load a ROM byte
            add hl,de           ; Add to total in HL
            inc ix              ; Next byte
            dec bc              ; Byte counter
            ld a,c              ; Test BC for zero
            or b
            jr nz,romchk        ; Go back for another byte

            ld ix,ASMPC+7       ; We're done; checksum is in HL
            jp hex4out_ix

            ld a,CR             ; Print CR/LF
            ld hl,ASMPC+6
            jp t1ou_hl
            
            ld a,LF
            ld hl,ASMPC+6
            jp t1ou_hl

            ld hl,0             ; Clear HL
            add hl,sp           ; Effectively ld hl,sp
            jp (hl)             ; Effectively jp (sp)

chkmsg:     defm  CR,LF,"EPROM checksum is ",EOS

; Fcmd
; Fill all RAM space with a given byte
; Entry: return link in SP
; Exit: HL and IY modified
Fcmd:       ld a,SPACE          ; Print a space
            ld iy,ASMPC+7
            jp t1ou_iy

            ld iy,ASMPC+7       ; Get two hex digits into L
            jp hex2in_iy
            jp Derr             ; Error return (reuse 'D' error logic)
            ld d,l              ; Use D to hold byte we're going to fill with

            ld hl,2000h         ; Start of RAM space (just above EPROM)
            ld bc,(56 * 1024)   ; Fill 56k bytes
fillb:      ld (hl),d           ; Store byte from D into RAM
            inc hl              ; Next byte
            dec bc              ; Decrement byte counter
            ld a,c              ; Test BC for zero
            or b
            jp nz,fillb         ; If non-zero, go back for more
            
            ld a,CR             ; Print CR/LF
            ld hl,ASMPC+6
            jp t1ou_hl
            
            ld a,LF
            ld hl,ASMPC+6
            jp t1ou_hl

            ld hl,0             ; Clear HL
            add hl,sp           ; Effectively ld hl,sp
            jp (hl)             ; Effectively jp (sp)

; Hcmd
; Print help message and return
; Entry: return link in SP
; Exit: HL and IY modified
Hcmd:       ld hl,helpmsg
            ld iy,ASMPC+7
            jp puts_iy
            
            ld hl,0             ; Clear HL
            add hl,sp           ; Effectively ld hl,sp
            jp (hl)             ; Effectively jp (sp)

helpmsg:    defm CR,LF,"RC2014 Tester commands:",CR,LF
            defm "D - Dump",CR,LF
            defm "E - EPROM test",CR,LF
            defm "F - Fill RAM",CR,LF
            defm "H - Help",CR,LF
            defm "I - Input",CR,LF
            defm "J - Joystick test",CR,LF
            defm "O - Output",CR,LF
            defm "R - RAM test",CR,LF
            defm "S - Slow RAM freerun test",CR,LF
            defm "T - Fast RAM freerun test",CR,LF
            defm "U - Scope loop on OUT",CR,LF
            defm "V - Scope loop on IN",CR,LF
            defm "Z - Fast RAM fill using LDIR",CR,LF
            defm EOS

; Icmd
; Input port test
; Entry: return link in SP
; Exit: HL and IY modified
Icmd:       ld a,SPACE          ; Print a space
            ld iy,ASMPC+7
            jp t1ou_iy

            ld iy,ASMPC+7       ; Get two hex digits into L
            jp hex2in_iy
            jp Derr             ; Error return (reuse 'D' error logic)
            ld c,l              ; Use C to hold I/O address we'll read
            
            ld a,':'            ; Print a separator
            ld iy,ASMPC+7
            jp t1ou_iy

            in a,(c)            ; Do the actual input operation

            ld iy,ASMPC+7
            jp hex2out_iy       ; Print as two-digit hex

            ld a,CR             ; Print CR/LF
            ld hl,ASMPC+6
            jp t1ou_hl
            
            ld a,LF
            ld hl,ASMPC+6
            jp t1ou_hl

            ld hl,0             ; Clear HL
            add hl,sp           ; Effectively ld hl,sp
            jp (hl)             ; Effectively jp (sp)

; Jcmd
; Test joystick port
; Entry: return link in SP
; Exit: HL and IY modified
Jcmd:       ld hl,joystckmsg
            ld iy,ASMPC+7
            jp puts_iy
            
Jloop:      in a,(JOY1)         ; Read joystick port
            ld b,a              ; Copy result into B

            ld a,'.'
            bit 0,b             ; Check bit for UP
            jr z,notup1
            ld a,'U'
notup1:     ld iy,ASMPC+7
            jp t1ou_iy

            ld a,'.'
            bit 1,b             ; Check bit for DOWN
            jr z,notdown1
            ld a,'D'
notdown1:   ld iy,ASMPC+7
            jp t1ou_iy

            ld a,'.'
            bit 2,b             ; Check bit for LEFT
            jr z,notleft1
            ld a,'L'
notleft1:   ld iy,ASMPC+7
            jp t1ou_iy

            ld a,'.'
            bit 3,b             ; Check bit for RIGHT
            jr z,notright1
            ld a,'R'
notright1:  ld iy,ASMPC+7
            jp t1ou_iy

            ld a,'.'
            bit 4,b             ; Check bit for FIRE1
            jr z,notfire11
            ld a,'F'     
notfire11:  ld iy,ASMPC+7
            jp t1ou_iy

            ld a,'.'
            bit 5,b             ; Check bit for FIRE2
            jr z,notfire21
            ld a,'F'
notfire21:  ld iy,ASMPC+7
            jp t1ou_iy

            ld a,SPACE          ; Print space
            ld iy,ASMPC+7
            jp t1ou_iy
            
            in a,(JOY2)         ; Read joystick port
            ld b,a              ; Copy result into B

            ld a,'.'
            bit 0,b             ; Check bit for UP
            jr z,notup2
            ld a,'U'
notup2:     ld iy,ASMPC+7
            jp t1ou_iy

            ld a,'.'
            bit 1,b             ; Check bit for DOWN
            jr z,notdown2
            ld a,'D'
notdown2:   ld iy,ASMPC+7
            jp t1ou_iy

            ld a,'.'
            bit 2,b             ; Check bit for LEFT
            jr z,notleft2
            ld a,'L'
notleft2:   ld iy,ASMPC+7
            jp t1ou_iy

            ld a,'.'
            bit 3,b             ; Check bit for RIGHT
            jr z,notright2
            ld a,'R'
notright2:  ld iy,ASMPC+7
            jp t1ou_iy

            ld a,'.'
            bit 4,b             ; Check bit for FIRE1
            jr z,notfire12
            ld a,'F'     
notfire12:  ld iy,ASMPC+7
            jp t1ou_iy

            ld a,'.'
            bit 5,b             ; Check bit for FIRE2
            jr z,notfire22
            ld a,'F'
notfire22:  ld iy,ASMPC+7
            jp t1ou_iy

            ld b,255            ; Small time delay
Jdelay:     nop
            nop
            djnz Jdelay
            
            ld a,CR             ; Print just CR
            ld iy,ASMPC+7
            jp t1ou_iy
            
            in a,(ACIAS)        ; Read ACIA status register
            bit 0,a             ; Check Rx status bit
            jp z,Jloop          ; Loop if no key pressed

            in a,(ACIAD)        ; Get the character from the data reg

            ld a,LF             ; Print just LF
            ld iy,ASMPC+7
            jp t1ou_iy
            
            ld hl,0             ; Clear HL
            add hl,sp           ; Effectively ld hl,sp
            jp (hl)             ; Effectively jp (sp)

joystckmsg: defm CR,LF," JOY1   JOY2"
            defm CR,LF,"------ ------",CR,LF
            defm EOS

; Ocmd
; Output to an I/O port address
; Entry: return link in SP
; Exit: registers modified
Ocmd:       ld a,SPACE          ; Print a space
            ld iy,ASMPC+7
            jp t1ou_iy

            ld iy,ASMPC+7       ; Get two hex digits into L
            jp hex2in_iy
            jp Derr             ; Error return (reuse 'D' error logic)
            ld c,l              ; Use C to hold I/O address we'll write
            
            ld a,':'            ; Print a separator
            ld iy,ASMPC+7
            jp t1ou_iy

            ld iy,ASMPC+7       ; Get two hex digits into L
            jp hex2in_iy
            jp Derr             ; Error return (reuse 'D' error logic)
            ld a,l              ; Byte to output must be in A

            out (c),a           ; Do the actual output operation

            ld a,CR             ; Print CR/LF
            ld hl,ASMPC+6
            jp t1ou_hl
            
            ld a,LF
            ld hl,ASMPC+6
            jp t1ou_hl

            ld hl,0             ; Clear HL
            add hl,sp           ; Effectively ld hl,sp
            jp (hl)             ; Effectively jp (sp)

; Rcmd
; Test RAM by write/read and marching ones tests
; Entry: return link in SP
; Exit: registers modified
Rcmd:       ld hl,ramsz         ; Print RAM size message
            ld iy,ASMPC+7
            jp puts_iy
            
            ld ix,RAMBASE       ; Initialise RAM pointer
            ld bc,RAMSIZE       ; Initialise loop counter
            ld hl,0             ; HL counts good bytes
            ld de,0aa55h        ; Two test bytes
ramchk:     ld (ix),d           ; Store a byte in RAM
            ld (ix),e           ; Store a byte in RAM
            ld a,(ix)           ; Read it back
            cp a,e              ; Read OK?
            jr nz,notok
            inc hl              ; One more good byte
notok:      inc ix              ; Next byte
            dec bc              ; Byte counter
            ld a,c              ; Test BC for zero
            or b
            jr nz,ramchk

            ld ix,ASMPC+7       ; We're done; size is in HL
            jp hex4out_ix

            ld a,SPACE          ; Print a space
            ld iy,ASMPC+7
            jp t1ou_iy

            ld ix,RAMBASE       ; Initialise RAM pointer
            ld bc,RAMSIZE       ; Initialise loop counter
            ld hl,0             ; HL counts good bytes
            ld de,055aah        ; Two test bytes
ramchk2:    ld (ix),d           ; Store a byte in RAM
            ld (ix),e           ; Store a byte in RAM
            ld a,(ix)           ; Read it back
            cp a,e              ; Read OK?
            jr nz,notok2 
            inc hl              ; One more good byte
notok2:     inc ix              ; Next byte
            dec bc              ; Byte counter
            ld a,c              ; Test BC for zero
            or b
            jr nz,ramchk2

            ld ix,ASMPC+7       ; We're done; size is in HL
            jp hex4out_ix
            
            ld a,SPACE          ; Print a space
            ld iy,ASMPC+7
            jp t1ou_iy

; Clear RAM to all AA
            ld ix,RAMBASE       ; Initialise RAM pointer
            ld bc,RAMSIZE       ; Initialise loop counter
            ld hl,0             ; HL counts good bytes
            ld d,0aah           ; Fill all bytes with AA
ramfillaa:  ld (ix),d           ; Store a zero in RAM
            inc ix              ; Next byte
            dec bc              ; Decrement byte counter
            ld a,c              ; Test BC for zero
            or b
            jp nz,ramfillaa     ; If non-zero, go back for more

; Ascending marching ones test
            ld ix,RAMBASE       ; Initialise RAM pointer
            ld bc,RAMSIZE       ; Initialise loop counter
            ld hl,0             ; HL counts good bytes
            ld d,055h           ; Holds 55 to overwrite with
marchasc:   ld a,(ix)           ; Read byte from RAM
            cp 0aah             ; Check for AA
            jr nz,notaa         ; Non-zero means it got overwritten
            inc hl              ; One more good byte
notaa:      ld (ix),d           ; Store 55 into same byte
            inc ix              ; Next byte
            dec bc              ; Decrement byte counter
            ld a,c              ; Test BC for zero
            or b
            jp nz,marchasc      ; If non-zero, go back for more

            ld ix,ASMPC+7       ; We're done; size is in HL
            jp hex4out_ix

            ld a,SPACE          ; Print a space
            ld iy,ASMPC+7
            jp t1ou_iy

; Descending marching ones test
            ld ix,RAMTOP        ; Initialise RAM pointer
            ld bc,RAMSIZE       ; Initialise loop counter
            ld hl,0             ; HL counts good bytes
            ld d,0              ; Holds a zero to overwrite with
marchdsc:   ld a,(ix)           ; Read byte from RAM
            cp 055h             ; Check for 55
            jr nz,not55         ; Non-zero means it got overwritten
            inc hl              ; One more good byte
not55:      ld (ix),d           ; Store zero into same byte
            dec ix              ; Next byte, downwards in memory
            dec bc              ; Decrement byte counter
            ld a,c              ; Test BC for zero
            or b
            jp nz,marchdsc      ; If non-zero, go back for more

            ld ix,ASMPC+7       ; We're done; size is in HL
            jp hex4out_ix

            ld a,CR             ; CR/LF
            ld hl,ASMPC+6
            jp t1ou_hl
            
            ld a,LF
            ld hl,ASMPC+6
            jp t1ou_hl

            ld hl,0             ; Clear HL
            add hl,sp           ; Effectively ld hl,sp
            jp (hl)             ; Effectively jp (sp)

ramsz:      defm CR,LF
            defm "Test:     AA55 55AA  asc  dsc",CR,LF
            defm "RAM size: ",EOS

; Scmd
; Test RAM by filling with EX (SP),HL and executing
; Entry: return link in SP (unused)
; Exit: via RESET
Scmd:       ld d,0E3h           ; Fill all bytes with EX (SP),HL
            jp freerunram

; Tcmd
; Test RAM by filling with NOP and executing
; Entry: return link in SP (unused)
; Exit: via RESET
Tcmd:       ld d,0              ; Fill all bytes with NOP

freerunram: ld hl,exitmsg       ; Print freerun exit message
            ld iy,ASMPC+7
            jp puts_iy

; Store opcode in D into all locations in RAM
            ld ix,RAMBASE       ; Initialise RAM pointer
            ld bc,RAMSIZE       ; Initialise loop counter
            ld hl,0             ; HL counts good bytes
ramop:      ld (ix),d           ; Store opcode in RAM
            inc ix              ; Next byte
            dec bc              ; Decrement byte counter
            ld a,c              ; Test BC for zero
            or b
            jp nz,ramop         ; If non-zero, go back for more

; Copy small routine into last few bytes of RAM
            ld bc,backend-goback ; Byte counter
            ld de,RAMTOP-((backend-goback)-1)
            ld hl,goback        ; Source pointer
            ldir                ; Block copy
            
            ld a,'.'            ; ASCII character to print on each loop

            jp RAMBASE          ; Jump in and never return

goback:     out (ACIAD),a       ; Print a dot each time around
            jp RAMBASE          ; Jump back to beginning of RAM
backend:    

exitmsg:    defm CR,LF,"Use RESET button to exit freerun tests",CR,LF,EOS

; Ucmd
; Scope loop on OUT instruction
; Entry: return link in SP (unused)
; Exit: via RESET
Ucmd:       ld a,SPACE          ; Print a space
            ld iy,ASMPC+7
            jp t1ou_iy

            ld iy,ASMPC+7       ; Get two hex digits into L
            jp hex2in_iy
            jp Derr             ; Error return (reuse 'D' error logic)
            ld c,l              ; Use C to hold I/O address we'll write
            
            ld a,':'            ; Print a separator            
            ld iy,ASMPC+7
            jp t1ou_iy

            ld iy,ASMPC+7       ; Get two hex digits into L
            jp hex2in_iy
            jp Derr             ; Error return (reuse 'D' error logic)
            ld b,l              ; Save byte to output in B

            ld hl,Uexitmsg      ; Print scope loop exit message
            ld iy,ASMPC+7
            jp puts_iy

            ld a,b              ; Byte to output must be in A
Uloop:      out (c),a           ; Do the actual output operation
            jp Uloop

Uexitmsg:   defm CR,LF,"Use RESET button to exit scope loops",CR,LF,EOS

; Vcmd          
; Scope loop on IN instruction
; Entry: return link in SP (unused)
; Exit: via RESET
Vcmd:       ld a,SPACE          ; Print a space
            ld iy,ASMPC+7
            jp t1ou_iy

            ld iy,ASMPC+7       ; Get two hex digits into L
            jp hex2in_iy
            jp Derr             ; Error return (reuse 'D' error logic)
            ld c,l              ; Use C to hold I/O address we'll read
            
            ld hl,Uexitmsg      ; Print scope loop exit message
            ld iy,ASMPC+7
            jp puts_iy

Vloop:      in a,(c)            ; Do the actual input operation
            jp Vloop

Ycmd:       ld hl,feedface
            jp fill
            
feedface:   defb 0feh,0edh,0fah,0ceh

; Zcmd
; Fill RAM $8000-$FFFF with $DEADBEEF
; Entry: return link in SP
; Exit: HL and IY modified
Zcmd:       ld hl,deadbeef      ; Source pointer
fill:       ld de,RAMBASE       ; Destination pointer
            ld bc,4             ; Byte counter
            ldir                ; Block copy

            ld hl,RAMBASE       ; Source pointer
            ld de,RAMBASE+0004h ; Destination pointer
            ld bc,0004h         ; Byte counter
            ldir                ; Block copy

            ld hl,RAMBASE       ; Source pointer
            ld de,RAMBASE+0008h ; Destination pointer
            ld bc,0008h         ; Byte counter
            ldir                ; Block copy

            ld hl,RAMBASE       ; Source pointer
            ld de,RAMBASE+0010h ; Destination pointer
            ld bc,0010h         ; Byte counter
            ldir                ; Block copy

            ld hl,RAMBASE       ; Source pointer
            ld de,RAMBASE+0020h ; Destination pointer
            ld bc,0020h         ; Byte counter
            ldir                ; Block copy

            ld hl,RAMBASE       ; Source pointer
            ld de,RAMBASE+0040h ; Destination pointer
            ld bc,0040h         ; Byte counter
            ldir                ; Block copy

            ld hl,RAMBASE       ; Source pointer
            ld de,RAMBASE+0080h ; Destination pointer
            ld bc,0080h         ; Byte counter
            ldir                ; Block copy

            ld hl,RAMBASE       ; Source pointer
            ld de,RAMBASE+0100h ; Destination pointer
            ld bc,0100h         ; Byte counter
            ldir                ; Block copy

            ld hl,RAMBASE       ; Source pointer
            ld de,RAMBASE+0200h ; Destination pointer
            ld bc,0200h         ; Byte counter
            ldir                ; Block copy

            ld hl,RAMBASE       ; Source pointer
            ld de,RAMBASE+0400h ; Destination pointer
            ld bc,0400h         ; Byte counter
            ldir                ; Block copy

            ld hl,RAMBASE       ; Source pointer
            ld de,RAMBASE+0800h ; Destination pointer
            ld bc,0800h         ; Byte counter
            ldir                ; Block copy

            ld hl,RAMBASE       ; Source pointer
            ld de,RAMBASE+1000h ; Destination pointer
            ld bc,1000h         ; Byte counter
            ldir                ; Block copy

            ld hl,RAMBASE       ; Source pointer
            ld de,RAMBASE+2000h ; Destination pointer
            ld bc,2000h         ; Byte counter
            ldir                ; Block copy

            ld hl,RAMBASE       ; Source pointer
            ld de,RAMBASE+4000h ; Destination pointer
            ld bc,4000h         ; Byte counter
            ldir                ; Block copy

            ld a,CR             ; Print CR/LF
            ld hl,ASMPC+6
            jp t1ou_hl
            
            ld a,LF
            ld hl,ASMPC+6
            jp t1ou_hl

            ld hl,0             ; Clear HL
            add hl,sp           ; Effectively ld hl,sp
            jp (hl)             ; Effectively jp (sp)

deadbeef:   defb 0deh,0adh,0beh,0efh

; t1ou_hl
; Transmit one character via the 6850 ACIA, no stack
; Entry: character in B, return link in HL
; Exit: A' modified
t1ou_hl:    ex af,af'           ; Save char in A'
t1ou4poll:  in a,(ACIAS)        ; Read ACIA status register
            bit 1,a             ; Check status bit
            jr z,t1ou4poll      ; Loop and wait if busy
            ex af,af'           ; Move char back into A
            out (ACIAD),a       ; Send A to ACIA
            jp (hl)             ; Return via link in HL

; t1ou_ix
; Transmit one character via the 6850 ACIA, no stack
; Entry: character in A, return link in IX
; Exit: A' modified
t1ou_ix:    ex af,af'           ; Save char in A'
t1ou3poll:  in a,(ACIAS)        ; Read ACIA status register
            bit 1,a             ; Check status bit
            jr z,t1ou3poll      ; Loop and wait if busy
            ex af,af'           ; Move char back into A
            out (ACIAD),a       ; Send A to ACIA
            jp (ix)             ; Return via link in IX

; t1ou_iy
; Transmit one character via the 6850 ACIA, no stack
; Entry: character in A, return link in IY
; Exit: A' modified
t1ou_iy:    ex af,af'           ; Save char in A'
t1ou2poll:  in a,(ACIAS)        ; Read ACIA status register
            bit 1,a             ; Check status bit
            jr z,t1ou2poll      ; Loop and wait if busy
            ex af,af'           ; Move char back into A
            out (ACIAD),a       ; Send A to ACIA
            jp (iy)             ; Return via link in IY

; puts_hl
; Transmit a string of characters, terminated by zero, no stack
; Entry: IY points to string, return link in HL
; Exit: IY points to zero terminator, A and A' modified
puts_hl:    ld a,(iy)           ; Load char pointed to by IY
            cp 0
            jr z,p1done         ; Found zero, end of string
            inc iy
            ex af,af'           ; Save A in A'
p1txpoll:   in a,(ACIAS)        ; Read ACIA status register
            bit 1,a             ; Check status bit
            jr z,p1txpoll       ; Loop and wait if busy
            ex af,af'           ; Recover char into A
            out (ACIAD),a       ; Send A to ACIA
            jr puts_hl
p1done:     jp (hl)             ; Return via link in HL

; puts_iy
; Transmit a string of characters, terminated by zero, no stack
; Entry: HL points to string, return link in IY
; Exit: HL points to zero terminator, A and A' modified
puts_iy:    ld a,(hl)           ; Load char pointed to by HL
            cp 0
            jr z,p_done         ; Found zero, end of string
            inc hl
            ex af,af'           ; Save A in A'
p_txpoll:   in a,(ACIAS)        ; Read ACIA status register
            bit 1,a             ; Check status bit
            jr z,p_txpoll       ; Loop and wait if busy
            ex af,af'           ; Recover char into A
            out (ACIAD),a       ; Send A to ACIA
            jr puts_iy
p_done:     jp (iy)             ; Return via link in IY

; t1in_iy
; Read one character from the 6850 ACIA, no stack
; Entry: return link in IY
; Exit: character in A
t1in_iy:    in a,(ACIAS)        ; Read status reg
            bit 0,a
            jr z,t1in_iy
            in a,(ACIAD)        ; Get the character from the data reg
            jp (iy)             ; Return via link in IY

; t1in_hl
; Read one character from the 6850 ACIA, no stack
; Entry: return link in HL
; Exit: character in A
t1in_hl:    in a,(ACIAS)        ; Read status reg
            bit 0,a
            jr z,t1in_hl
            in a,(ACIAD)        ; Get the character from the data reg
            jp (hl)             ; Return via link in HL

; hex4in_iy
; Read 4-digit hex into HL
; Entry: return link in IY
; Exit: value in HL, A modified
hex4in_iy:  ld hl,0             ; Clear HL initially
getch1:     in a,(ACIAS)        ; Read status reg
            bit 0,a
            jr z,getch1
            in a,(ACIAD)        ; Get the character from the data reg
            out (ACIAD),a       ; Echo received character

            cp a,'0'
            jp m,not4hex
            
            cp a,'9'+1
            jp m,hex109
            
            and 05fh            ; Make upper-case

            cp 'A'
            jp m,not4hex
            
            cp 'F'+1
            jp m,hex1af
            jp not4hex

hex1af:     sub 'A'-10
            jr hex1dig

hex109:     sub '0'             ; Char was 0-9
hex1dig:    sla a
            sla a
            sla a
            sla a
            ld h,a              ; Save in H

getch2:     in a,(ACIAS)        ; Read status reg
            bit 0,a
            jr z,getch2
            in a,(ACIAD)        ; Get the character from the data reg
            out (ACIAD),a       ; Echo received character

            cp a,'0'
            jp m,not4hex
            
            cp a,'9'+1
            jp m,hex209
            
            and 05fh            ; Make upper-case

            cp 'A'
            jp m,not4hex
            
            cp 'F'+1
            jp m,hex2af
            jr not4hex

hex2af:     sub 'A'-10
            jr hex2dig

hex209:     sub '0'             ; Char was 0-9
hex2dig:    or h
            ld h,a              ; Save in H

hex2in_iy:                      ; Read just two hex digits, into L
getch3:     in a,(ACIAS)        ; Read status reg
            bit 0,a
            jr z,getch3
            in a,(ACIAD)        ; Get the character from the data reg
            out (ACIAD),a       ; Echo received character

            cp a,'0'
            jp m,not4hex
            
            cp a,'9'+1
            jp m,hex309
            
            and 05fh            ; Make upper-case

            cp 'A'
            jp m,not4hex
            
            cp 'F'+1
            jp m,hex3af
            jr not4hex

hex3af:     sub 'A'-10
            jr hex3dig

hex309:     sub '0'             ; Char was 0-9
hex3dig:    sla a 
            sla a
            sla a
            sla a
            ld l,a              ; Save in L

getch4:     in a,(ACIAS)        ; Read status reg
            bit 0,a
            jr z,getch4
            in a,(ACIAD)        ; Get the character from the data reg
            out (ACIAD),a       ; Echo received character

            cp a,'0'
            jp m,not4hex
            
            cp a,'9'+1
            jp m,hex409
            
            and 05fh            ; Make upper-case

            cp 'A'
            jp m,not4hex
            
            cp 'F'+1
            jp m,hex4af
            jr not4hex

hex4af:     sub 'A'-10
            jr hex4dig

hex409:     sub '0'             ; Char was 0-9
hex4dig:    or l
            ld l,a              ; Save in L

            inc iy              ; Non-error return 3 bytes later
            inc iy
            inc iy
not4hex:    jp (iy)             ; Return via link in IY

; hex2out_hl
; Print A as two-digit hex
; Entry: A contains number to be printed, return link in HL
; Exit: A, C, IY modified
hex2out_hl: ld c,a
            srl a
            srl a
            srl a
            srl a
            cp a,10
            jp m,h1digit
            add a,7
h1digit:    add a,30h
            ld iy,ASMPC+7
            jp t1ou_iy
            ld a,c
            and a,0fh
            cp a,10
            jp m,h2digit
            add a,7
h2digit:    add a,30h
            ld iy,ASMPC+7
            jp t1ou_iy          
            jp (hl)             ; Return via link in HL

; hex2out_iy
; Print A as two-digit hex
; Entry: A contains number to be printed, return link in HL
; Exit: A, C, IX modified
hex2out_iy: ld c,a
            srl a
            srl a
            srl a
            srl a
            cp a,10
            jp m,h7digit
            add a,7
h7digit:    add a,30h
            ld ix,ASMPC+7
            jp t1ou_ix
            ld a,c
            and a,0fh
            cp a,10
            jp m,h8digit
            add a,7
h8digit:    add a,30h
            ld ix,ASMPC+7
            jp t1ou_ix          
            jp (iy)             ; Return via link in iy

; hex4out_ix
; Print HL as four-digit hex
; Entry: HL contains number to be printed, return link in IX
; Exit: A, IY modified
hex4out_ix: ld a,h
            srl a
            srl a
            srl a
            srl a
            cp a,10
            jp m,h3digit
            add a,7
h3digit:    add a,30h
            ld iy,ASMPC+7
            jp t1ou_iy
            ld a,h
            and a,0fh
            cp a,10
            jp m,h4digit
            add a,7
h4digit:    add a,30h
            ld iy,ASMPC+7
            jp t1ou_iy
            ld a,l              ; And again with the low half
            srl a
            srl a
            srl a
            srl a
            cp a,10
            jp m,h5digit
            add a,7
h5digit:    add a,30h
            ld iy,ASMPC+7
            jp t1ou_iy
            ld a,l
            and a,0fh
            cp a,10
            jp m,h6digit
            add a,7
h6digit:    add a,30h
            ld iy,ASMPC+7
            jp t1ou_iy
            jp (ix)             ; Return via link in IX

; Fill empty EPROM space with $FF and test patterns
            defs  0A00h-ASMPC,0ffh
            defw  $0A00,$0A02,$0A04,$0A06,$0A08,$0A0A,$0A0C,$0A0E
            defw  $0A10,$0A12,$0A14,$0A16,$0A18,$0A1A,$0A1C,$0A1E
            defw  $0A20,$0A22,$0A24,$0A26,$0A28,$0A2A,$0A2C,$0A2E
            defw  $0A30,$0A32,$0A34,$0A36,$0A38,$0A3A,$0A3C,$0A3E
            defs  1000h-ASMPC,0ffh
            defw  $1000,$1002,$1004,$1006,$1008,$100A,$100C,$100E
            defw  $1010,$1012,$1014,$1016,$1018,$101A,$101C,$101E
            defw  $1020,$1022,$1024,$1026,$1028,$102A,$102C,$102E
            defw  $1030,$1032,$1034,$1036,$1038,$103A,$103C,$103E
            defs  2000h-ASMPC,0ffh

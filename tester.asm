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
            
            defc ACIAS=80h      ; 6850 ACIA status register
            defc ACIAD=81h      ; 6850 ACIA data register
            
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
            
signon:     defm  "RC2014 Memory Test and Diagnostics ROM",CR,LF
            defm  "V1.00 2017-03-08",CR,LF,EOS

; Jump table containing pointers to command subroutines
jmptab:     defw nosuchcmd      ; A
            defw nosuchcmd      ; B
            defw nosuchcmd      ; c
            defw Dcmd           ; D
            defw Ecmd           ; E
            defw nosuchcmd      ; F
            defw nosuchcmd      ; G
            defw Hcmd           ; H
            defw nosuchcmd      ; I
            defw nosuchcmd      ; J
            defw nosuchcmd      ; K
            defw nosuchcmd      ; L
            defw nosuchcmd      ; M
            defw nosuchcmd      ; N
            defw nosuchcmd      ; O
            defw nosuchcmd      ; P
            defw nosuchcmd      ; Q
            defw Rcmd           ; R
            defw nosuchcmd      ; S
            defw nosuchcmd      ; T
            defw nosuchcmd      ; U
            defw nosuchcmd      ; V
            defw nosuchcmd      ; W
            defw nosuchcmd      ; X
            defw nosuchcmd      ; Y
            defw nosuchcmd      ; Z
            
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
Dcmd:       ld hl,ROMBASE       ; Start address (TODO: accept from keyboard)

            ld a,CR             ; CR/LF
            ld iy,ASMPC+7
            jp t1ou_iy
            
            ld a,LF
            ld iy,ASMPC+7
            jp t1ou_iy
            
            ld d,16             ; Row counter
            
Drow:       ld ix,ASMPC+7       ; Print HL in 4-digit hex
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

            ld hl,0             ; Clear HL
            add hl,sp           ; Effectively ld hl,sp
            jp (hl)             ; Effectively jp (sp)

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
            dec c               ; Byte counter LO
            jr nz,romchk        ; Is C zero?
            djnz romchk         ; Byte counter HI

            ld ix,ASMPC+7       ; We're done; checksum is in HL
            jp hex4out_ix

            ld b,CR
            ld hl,ASMPC+6
            jp t1ou_hl
            
            ld b,LF
            ld hl,ASMPC+6
            jp t1ou_hl

            ld hl,0             ; Clear HL
            add hl,sp           ; Effectively ld hl,sp
            jp (hl)             ; Effectively jp (sp)

chkmsg:     defm  CR,LF,"EPROM checksum is ",EOS

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
            defm "H - Help",CR,LF
            defm "R - RAM test",CR, LF
            defm EOS

; Rcmd
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
            ld a,0              ; Zero in A for comparisons
            dec bc              ; Byte counter
            cp a,c              ; Is C zero?
            jr nz,ramchk
            cp a,b              ; Is B zero?
            jr nz,ramchk

            ld ix,ASMPC+7       ; We're done; size is in HL
            jp hex4out_ix

            ld b,CR             ; CR/LF
            ld hl,ASMPC+6
            jp t1ou_hl
            
            ld b,LF
            ld hl,ASMPC+6
            jp t1ou_hl

            ld hl,0             ; Clear HL
            add hl,sp           ; Effectively ld hl,sp
            jp (hl)             ; Effectively jp (sp)

ramsz:      defm  CR,LF,"RAM size is ",EOS

; t1ou_hl
; Transmit one character via the 6850 ACIA, no stack
; Entry: character in B, return link in HL
; Exit: A now holds character, B unchanged
t1ou_hl:    in a,(ACIAS)        ; Read ACIA status register
            bit 1,a             ; Check status bit
            jr z,t1ou_hl        ; Loop and wait if busy
            ld a,b              ; Move char into A
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
; Exit: IY points to zero terminator, A and B changed
puts_hl:    ld a,(iy)           ; Load char pointed to by IY
            cp 0
            jr z,p1done         ; Found zero, end of string
            inc iy
            ld b,a
p1txpoll:   in a,(ACIAS)        ; Read ACIA status register
            bit 1,a             ; Check status bit
            jr z,p1txpoll       ; Loop and wait if busy
            ld a,b              ; Move char into A
            out (ACIAD),a       ; Send A to ACIA
            jr puts_hl
p1done:     jp (hl)             ; Return via link in HL

; puts_iy
; Transmit a string of characters, terminated by zero, no stack
; Entry: HL points to string, return link in IY
; Exit: HL points to zero terminator, A and B changed
puts_iy:    ld a,(hl)           ; Load char pointed to by HL
            cp 0
            jr z,p_done         ; Found zero, end of string
            inc hl
            ld b,a
p_txpoll:   in a,(ACIAS)        ; Read ACIA status register
            bit 1,a             ; Check status bit
            jr z,p_txpoll       ; Loop and wait if busy
            ld a,b              ; Move char into A
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
            defs  0400h-ASMPC,0ffh
            defw  $0400,$0402,$0404,$0406,$0408,$040A,$040C,$040E
            defw  $0410,$0412,$0414,$0416,$0418,$041A,$041C,$041E
            defw  $0420,$0422,$0424,$0426,$0428,$042A,$042C,$042E
            defw  $0430,$0432,$0434,$0436,$0438,$043A,$043C,$043E
            defs  0800h-ASMPC,0ffh
            defw  $0800,$0802,$0804,$0806,$0808,$080A,$080C,$080E
            defw  $0810,$0812,$0814,$0816,$0818,$081A,$081C,$081E
            defw  $0820,$0822,$0824,$0826,$0828,$082A,$082C,$082E
            defw  $0830,$0832,$0834,$0836,$0838,$083A,$083C,$083E
            defs  1000h-ASMPC,0ffh
            defw  $1000,$1002,$1004,$1006,$1008,$100A,$100C,$100E
            defw  $1010,$1012,$1014,$1016,$1018,$101A,$101C,$101E
            defw  $1020,$1022,$1024,$1026,$1028,$102A,$102C,$102E
            defw  $1030,$1032,$1034,$1036,$1038,$103A,$103C,$103E
            defs  2000h-ASMPC,0ffh

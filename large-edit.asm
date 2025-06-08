; large-edit.asm - Commodore 64 Large Type Editor
; Copyright (c) 2025 by David Van Wagner ALL RIGHTS RESERVED
; MIT LICENSE
; github.com/davervw
; www.davevw.com

; Memory map
; 0000-00FF zero page: temps(saved):22,23,24,25,26,27,ff  and not saved: 02,fb,fc,fd,fe
; 0100-01FF stack
; 0200-03FF more lower RAM for OS/BASIC
; 0400-07FF logical screen codes memory (BASIC thinks screen is here)
; 0800-09FF BASIC RAM
; A000-BFFF BASIC ROM
; B400-B7FF color_next (banked RAM under ROM) = changes detected as if no viewports [TODO]
; BC00-BFFF color_copy (banked RAM under ROM) = what color memory should be as if no viewports [TODO]
; B800-BBFF color_last (banked RAM under ROM) = exact copy of color memory applied with viewports [TODO]
; C000-CBFF Large Type Editor machine code program, data, and misc. buffers
; CC00-CFFF VIC-II screen displayed
; D000-D7FF I/O (and banked chargen ROM, and banked RAM with copy of chargen ROM)
; D800-DFFF Color nybles in I/O space (and banked chargen ROM, and banked RAM with copy of chargen ROM)
; E000-FFFF BASIC(more) and KERNAL ROM (banked RAM is encoded 4x4 large type characters each using 16 byte PETSCII)

; Bank/Addreses/Functions
; $01  0000  A000  C000 D800  E000
; 0    RAM   RAM   RAM  RAM   RAM         64K RAM
; 1    RAM   RAM   RAM  CHAR  RAM
; 2    RAM   RAM   RAM  CHAR  KERNAL
; 3    RAM   BASIC RAM  CHAR  KERNAL
; 4    RAM   RAM   RAM  RAM   RAM
; 5    RAM   RAM   RAM  I/O   RAM
; 6    RAM   RAM   RAM  I/O   KERNAL
; 7    RAM   BASIC RAM  I/O   KERNAL      C64 NORMAL

CHROUT = $ffd2
IRQVECT = $0314
KEYVECT = $028F

; key info (TODO: intercept KEYVECT to scroll screen manually)
; $28D SHFLAG 1=shift, 2=commodore 4=control
; $C5 last key pressed, $40=none
; control up = 07 05
; control down = 07 04
; control left = 02 05
; control right = 02 04

*=$c000
    jmp init

newirq: ; TODO: verify IRQ source is 1/60 second timer
    lda $ff
    sta saveff
    lda $22
    sta save22
    lda $23
    sta save23
    lda $24
    sta save24
    lda $25
    sta save25
    lda $26
    sta save26
    lda $27
    sta save27
    lda $02
    sta save02
    lda $fb
    sta savefb
    lda $fc
    sta savefc
    lda $fd
    sta savefd
    lda $fe
    sta savefe

    ldy #0    
    lda 646
    cmp save_foreground ; detect color change
    bne +
    lda $d800
    and #$0f
    cmp save_foreground ; detect clear screen
    beq ++
; apply foreground color change to entire screen
+   sta save_foreground
-   sta $d800,y
    sta $d900,y
    sta $da00,y
    sta $db00,y    
    iny
    bne -
    lda #$80
    sta redraw

++      ; adjust left when cursor moved left/right out of viewport frame
    lda $d3 ; cursor column on current line (0..79)
-   cmp #40
    bcc +
    sbc #40
    bcc - ; one subtraction should be enough, but just in case
+   sec
    sbc left
    bpl + ; not out of frame, or went out of frame to right
    ; otherwise went out of frame to left
    adc left
    bmi ++ ; shouldn't happen
    sta left
    jmp ++
+   sec
    sbc #9
    bmi ++ ; not out of frame
    clc
    adc left
    sta left

    ; adjust viewport pointer when cursor moved up/down out of frame
++  sec
    lda $d6
    sbc top
    bpl +
    tax
-   sec
    lda viewport
    sbc #40
    sta viewport
    bcs ++
    dec viewport+1
++  dec top
    inx
    bne -
    beq ++
+   sbc #4
    bcc ++
    beq ++
+   tax
-   clc
    lda viewport
    adc #40
    sta viewport
    bcc +
    inc viewport+1
+   inc top
    dex
    bne -

++  sty $ff
    lda viewport
    ldx viewport+1
    sta $fb
    stx $fc
    lda #<($cc00 + 800)
    ldx #>($cc00 + 800)
    sta $fd
    stx $fe
-   lda ($fb),y
    bit redraw
    bmi +++
    cmp ($fd),y
    beq +
+++ sta ($fd),y
    inc $ff ; a change occurred, guaranteed not to wrap
+   iny
    cpy #200
    bcc -

    ; check case change
    lda $01
    sta $02
    ora #7 ; (normal) with I/O
    sta $01
    lda $d018
    and #2
    cmp lastcase
    beq +
    sta lastcase
    inc $ff ; a change occurred, guaranteed not to wrap
    lda #$80
    sta redraw
+   lda $02
    sta $01

    lda $ff
    bne +
    jmp +++

+   lda $01
    sta $02
    and #$f8 ; all RAM
    sta $01    

    ldy #0
    sty $22 ; row
    sty $23 ; col

    ; get pointer to viewport destination
++  lda viewport
    clc
    adc left
    ldx viewport+1
    sta $fb
    stx $fc

    sty $fd ; low byte dest screen (0)
    ldx #>$cc00 ; high byte dest screen
    stx $fe

-   lda ($fb),y
    bit redraw
    bmi +
    cmp text_buffer,y
    bne +
    jmp skip
+   sta text_buffer,y
    ldx #$e
    stx $27 ; will be high byte encoded screen codes after multiplication
    asl
    rol $27
    asl
    rol $27
    asl
    rol $27
    asl
    rol $27
    sta $26 ; low byte encoded screen codes

    ; check lowercase
    lda $02
    ora #7 ; normal w/ IO
    sta $01
    lda $d018
    and #2
    beq +
    lda $27
    ora #$10
    sta $27
+   lda $02
    and #$f8
    sta $01
   
    sty $ff ; save index

    ; copy the 16 characters (unrolled loop)
    ldy #0
    lda ($26),y
    sta ($fd),y

    iny ;1
    lda ($26),y
    sta ($fd),y

    iny ;2
    lda ($26),y
    sta ($fd),y

    iny ;3
    lda ($26),y
    sta ($fd),y

    iny ;4
    lda ($26),y
    ldy #40
    sta ($fd),y

    ldy #5
    lda ($26),y
    ldy #41
    sta ($fd),y

    ldy #6
    lda ($26),y
    ldy #42
    sta ($fd),y

    ldy #7
    lda ($26),y
    ldy #43
    sta ($fd),y

    ldy #8
    lda ($26),y
    ldy #80
    sta ($fd),y

    ldy #9
    lda ($26),y
    ldy #81
    sta ($fd),y

    ldy #10
    lda ($26),y
    ldy #82
    sta ($fd),y

    ldy #11
    lda ($26),y
    ldy #83
    sta ($fd),y

    ldy #12
    lda ($26),y
    ldy #120
    sta ($fd),y

    ldy #13
    lda ($26),y
    ldy #121
    sta ($fd),y

    ldy #14
    lda ($26),y
    ldy #122
    sta ($fd),y

    ldy #15
    lda ($26),y
    ldy #123
    sta ($fd),y

    ldy $ff ; restore index

skip
    iny
    clc
    lda $fd
    adc #4
    sta $fd
    bcc +
    inc $fe
+   inc $23
    lda $23
    cmp #10 ; done columns?
    bcs +
    jmp -
+   lda #0
    sta $23
    clc
    tya
    adc #30
    tay
    lda $fd
    adc #(160-40)
    sta $fd
    bcc +
    inc $fe
+   inc $22
    lda $22
    cmp #5 ; done rows?
    bcs +
    jmp -
+   lda #0
    sta redraw

restorebank
    lda $02
    sta $01

+++ lda save22
    sta $22
    lda save23
    sta $23
    lda save24
    sta $24
    lda save25
    sta $25
    lda save26
    sta $26
    lda save27
    sta $27
    lda saveff
    sta $ff
    lda save02
    sta $02
    lda savefb
    sta $fb
    lda savefc
    sta $fc
    lda savefd
    sta $fd
    lda savefe
    sta $fe

oldirq = *+1
    jmp $0000

init:
    jsr copy_charrom
    jsr switch_charram
    jsr switch_screen_cc00
    lda #<title
    ldx #>title
    jsr strout
    jsr encode_chars
    jsr swapirq
    jsr enqueue_keys
    rts

switch_charram:
    lda $d018
    and #$02
    ora #$34
    sta $d018
    rts

switch_screen_cc00:
    ldx #0
    lda #$20
-   sta $cc00, x
    sta $cd00, x
    sta $ce00, x
    sta $cf00, x
    inx
    bne -

    lda #$04
    sta $dd00

    lda #$80
    sta redraw

    lda #$ff
    sta save_foreground

    stx left ; 0
    stx top ; 0
    stx viewport ; 0
    lda #>$0400
    sta viewport+1

    rts

swapirq:
    lda #<newirq
    ldx #>newirq
    cpx IRQVECT+1
    beq +
    ldy IRQVECT
    sty oldirq
    ldy IRQVECT+1
    sty oldirq+1
    sta IRQVECT
    stx IRQVECT+1 
+   rts

; copy ROM D000-DFFF to RAM D000-DFFF
copy_charrom:
    sei
    ldy #0
    sty $fb
    lda #$d0
    sta $fc
    lda $01
    tax
    and #$fb ; bank 3 CHARGEN ROM
    sta $01
-   lda ($fb),y
    sta ($fb),y
    iny
    bne -
    inc $fc
    lda $fc
    cmp #$e0
    bcc -
    txa
    sta $01 ; restore to normal
    cli
    rts

strout:
    sta $fb
    stx $fc
    ldy #0
-   lda ($fb),y
    beq +
    jsr CHROUT
    iny
    bne -
+   rts

enqueue_keys:
    lda #19
    sta 631
    lda #17
    sta 632
    sta 633
    sta 634
    sta 635
    lda #5
    sta 198
    rts

encode_chars: ; encode each 8x8 pixel character (8 bytes bitmap) into local buffer (4x4 = 16 byte screencodes)
    lda #$00
    ldx #$d0
    sta $fb
    stx $fc
    lda #0
    ldx #$e0
    sta $24
    stx $25
-   lda #<bitmap_buffer
    ldx #>bitmap_buffer
    sta $fd
    stx $fe
    jsr copy_char
    jsr encode_char
    clc
    lda $fb
    adc #8
    sta $fb
    bcc +
    inc $fc
+   lda $fc
    cmp #$e0
    bcc -
    rts

copy_char: ; $fb/fc source (chargen ROM), to $fd/fe destination (normal RAM)
    sei
    lda $01
    tax
    and #$fb ; bank 3 CHARGEN ROM
    sta $01
    ldy #7
-   lda ($fb),y
    sta ($fd),y
    dey
    bpl -
    txa
    ora #7 ; normal ROM + I/O
    sta $01
    cli
    rts    

encode_char: ; given chargen bitmaps at bitmap_buffer+0 to +7, output 16 screen codes representing charcter to pointer $24/$25, pointer += 16, uses $22/$23/$ff
    ldx #0
    lda #4
    sta $22 ; #rows countdown
--  lda #4
    sta $23 ; #columns countdown
-   lda bitmap_buffer, x
    asl
    ror $ff
    asl
    ror $ff
    sta bitmap_buffer, x
    inx
    txa
    and #1
    bne -
    lda $ff ; bits from two bytes 01------/23------ are now in order 3210----
    lsr
    lsr
    lsr
    lsr ; bits now are ----3210 with high bits clear
    tay
    lda lores_codes, y
    ldy #0
    sta ($24), y
    inc $24
    bne +
    inc $25
+   dex
    dex
    dec $23
    bne -
    inx
    inx
    dec $22
    bne --
    rts

text_buffer: ; should only need 10x5, but current algorithm uses same offset as screen so 40x5
    !byte 32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32
    !byte 32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32
    !byte 32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32
    !byte 32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32
    !byte 32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32

bitmap_buffer:
    !byte 0,0,0,0
    !byte 0,0,0,0

save22: !byte 0
save23: !byte 0
save24: !byte 0
save25: !byte 0
save26: !byte 0
save27: !byte 0
saveff: !byte 0
save02: !byte 0
savefb: !byte 0
savefc: !byte 0
savefd: !byte 0
savefe: !byte 0

lastcase: !byte 0
redraw: !byte 0
save_foreground: !byte 0
top: !byte 0
left: !byte 0
viewport: !word 0 ; address to column 0

; 16 commodore graphics screen codes that make lo-res 2x2 pixels per character bits in NW,NE,SW,SE order low to high 
lores_codes:
        !byte $60 ; 00/00
        !byte $7e ; 10/00 NW
        !byte $7c ; 01/00 NE
        !byte $e2 ; 11/00
        !byte $7b ; 00/10 SW
        !byte $61 ; 10/10
        !byte $ff ; 01/10
        !byte $ec ; 11/10
        !byte $6c ; 00/01 SE
        !byte $7f ; 10/01
        !byte $e1 ; 01/01
        !byte $fb ; 11/01
        !byte $62 ; 00/11
        !byte $fc ; 10/11
        !byte $fe ; 01/11
        !byte $e0 ; 11/11

title: 
    !byte 147,18
    ;               1         2         3         4
    ;      1234567890123456789012345678901234567890
    !text "LARGE TYPE EDITOR                       ",146
    !text "(C) 2025 DAVID R. VAN WAGNER",13
    !text "GITHUB.COM/DAVERVW",13
    !text "DAVEVW.COM",13
    !byte 0

finish:

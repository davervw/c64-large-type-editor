; large-edit.asm - Commodore 64 Large Type Editor
; Copyright (c) 2025 by David Van Wagner ALL RIGHTS RESERVED
; MIT LICENSE
; github.com/davervw
; www.davevw.com

CHROUT = $ffd2
IRQVECT = $0314

*=$c000
    jmp init

newirq:
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

    ldx #0
    stx $ff
-   lda $0400,x
    cmp $cc00 + 800,x
    beq +
    sta $cc00 + 800,x
    inc $ff
+   clc
    inx
    cpx #200
    bcc -

    lda $ff
    bne +
    jmp ++

+   lda $01
    sta $02
    and #$f0
    sta $01    

    ldy #0
    sty $22 ; row
    sty $23 ; col
    sty $fb ; low byte source screen
    ldx #4
    stx $fc ; high byte source screen
    sty $fd ; low byte dest screen
    ldx #$cc ; high byte dest screen
    stx $fe
-   lda ($fb),y
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
    
    sty $ff

    ldy #0
    lda ($26),y
    sta ($fd),y

    ldy #1
    lda ($26),y
    sta ($fd),y

    ldy #2
    lda ($26),y
    sta ($fd),y

    ldy #3
    lda ($26),y
    sta ($fd),y

    ldy #4
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

    ldy $ff

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
    cmp #10
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
    cmp #5
    bcs +
    jmp -

restorebank
+   lda $02
    sta $01

++  lda save22
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
    ; lda #$cc
    ; sta $0288
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
    and #$fb
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
    sta $01
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
    and #$fb
    sta $01
    ldy #7
-   lda ($fb),y
    sta ($fd),y
    dey
    bpl -
    txa
    ora #7
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

text_buffer:
    !byte 32,32,32,32,32,32,32,32,32,32
    !byte 32,32,32,32,32,32,32,32,32,32
    !byte 32,32,32,32,32,32,32,32,32,32
    !byte 32,32,32,32,32,32,32,32,32,32
    !byte 32,32,32,32,32,32,32,32,32,32

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
    !text "LARGE TYPE EDITOR                       "
    !text "(C) 2025 DAVID R. VAN WAGNER            "
    !text "                                        "
    !text "GITHUB.COM/DAVERVW                      "
    !text "DAVEVW.COM                              "
    !byte 0

finish:

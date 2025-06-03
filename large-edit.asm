; large-edit.asm - Commodore 64 Large Type Editor
; Copyright (c) 2025 by David Van Wagner ALL RIGHTS RESERVED
; MIT LICENSE
; github.com/davervw
; www.davevw.com

CHROUT = $ffd2

*=$c000
    jmp init

init:
    jsr copy_charrom
    jsr switch_charram
    jsr switch_screen_cc00
    lda #<title
    ldx #>title
    jsr strout
    jsr encode_chars
    rts

switch_charram:
    lda $d018
    and #$02
    ora #$34
    sta $d018
    rts

switch_screen_cc00:
    lda #$04
    sta $dd00
    lda #$cc
    sta $0288
    rts

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
    ldx #$c4
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
    cmp #$d4
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

bitmap_buffer:
    !byte 0,0,0,0
    !byte 0,0,0,0

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

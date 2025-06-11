# Large Type Editor for C64 #

Large Type Editor is a utility to display a portion of the C64 screen enlarged, with a view of the currently displayed lines in normal size.  PETSCII graphic block characters are used to draw a character's 8x8 pixels in a 4x4 character grid.  Encoded 4x4 characters are pre-generated for performance so dealing with 16 characters instead of 64 pixels.  The VICII screen is now at $CC00 while the virtual screen is still at $0400.  The IRQ interrupt is captured to check for video changes and paint them at $CC00.  

Limitations:

* Requires Commodore Revision 3 ROM because currently does not properly update color memory (rev. 1 color mismatches, rev. 2 invisible text).  Biggest problem is with scrolling.
* Unfortunately multiple color support is not currently implemented (more complex challenge as source and destination addresses will both be $D800), so any foreground color change updates entire screen
* Code and buffers are using $C000-$CF00
* Chargen ROM had to be copied to its same address $D000-$DFFF RAM
* Encoded 4x4 characters are using $E000-$FFFF RAM 
* Tests show running with new IRQ processing at about 62-66% of the system's original processing power (could decrease fps to regain some performance)

Press [STOP]+[RESTORE] to return to normal 40x25 screen operation.   
```SYS 49152``` to return to large type.

![gif](demo_0.02.gif)
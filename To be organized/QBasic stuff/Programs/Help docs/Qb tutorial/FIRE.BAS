

' $DYNAMIC
'FAST FIRE BY DANNY BEARDSLEY
' COMPILE ME! 10 TIMES FASTER
' COMPILE ME! 10 TIMES FASTER
' COMPILE ME! 10 TIMES FASTER
'_______/----------------\______________
'This is the fastest fire routine that i have ever seen.
'If you COMPILE it will be !!!10 times faster!!!!
'This is not true fire it only spreads to the left
'If you want more code for other amazing feats then
'----------------------------
'Email:       dsb@cyberdude.com
'Homepage:    www.dnai.com/~beards
' FFFRRREEEEEEWWWAAARRREEE (just put my name in somewhere if you use it)

SCREEN 13
CLS
  FOR t% = 0 TO 63
    '
    ' Fade from black to red
    '
    OUT &H3C7, t%  ' Slots 0-15
    OUT &H3C9, t%
    OUT &H3C9, 0
    OUT &H3C9, 0
    '
    ' Fade from red to yellow
    '
    OUT &H3C7, t% + 63' Slots 16-31
    OUT &H3C9, 63
    OUT &H3C9, t%
    OUT &H3C9, 0
    '
    ' Fade from yellow to white
    '
    OUT &H3C7, t% + 126' Slots 32-47
    OUT &H3C9, 63
    OUT &H3C9, 63
    OUT &H3C9, t%
    '
  NEXT

tim = TIMER
DEF SEG = (&HA000 + &H7D0)
RANDOMIZE TIMER
DO
LINE (0, 198)-(320, 198), 0
FOR r% = 1 TO 320
  POKE (31360 + 320 * RND), 180
NEXT r%
cnt% = cnt% + 1
FOR y% = 31 TO 98
FOR x% = 2 TO 318
  v% = y% * 320 + x%
  c% = PEEK(v%)
  'IF c% < 0 THEN GOTO 1
  c% = c% + PEEK(v% + 1) + PEEK(v% + 320)
  IF c% < 10 THEN GOTO 1
  c% = c% \ 3 - 2
  POKE (v% - 320), c%
1
NEXT x%
NEXT y%
LOOP UNTIL INKEY$ <> ""
s = TIMER - tim
COLOR 180
PRINT cnt% / s
SLEEP
SCREEN 12
PRINT "FAST FIRE BY DANNY BEARDSLEY (made in QB4.5)"
PRINT "COMPILE ME! 10 TIMES FASTER"
PRINT "COMPILE ME! 10 TIMES FASTER"
PRINT "COMPILE ME! 10 TIMES FASTER"
PRINT "_______________________________________________________"
PRINT "This is the fastest fire routine that I have ever seen."
PRINT "   If you COMPILE it will be !!!10 times faster!!!!"
PRINT "   This is not true fire it only spreads to the left"
PRINT "  If you want more code for other amazing feats then"
PRINT "-------------------------------------------------------"
PRINT "Email:       dsb@cyberdude.com"
PRINT "Homepage:    www.dnai.com/~beards"
PRINT "FREEWARE (just put my name in somewhere if you use it {:-)"
SLEEP
END


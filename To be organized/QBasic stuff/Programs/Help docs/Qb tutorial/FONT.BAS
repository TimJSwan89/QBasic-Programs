
DECLARE SUB fontput (z1%, y1%, in$, c%)
DECLARE SUB fontput1 (z1%, y1%, in$, c%)
DECLARE SUB fontput2 (z1%, y1%, in$, c%)

RANDOMIZE TIMER
SCREEN 12
CLS
PAINT (1, 1), 1
fontput 0, 0, "The old algorithm...", 2
fontput 258, 8, "FontPut Demo", 0
fontput 260, 10, "FontPut Demo", 15
fontput 20, 30, "I dont expect you to use this procedure but it uses a
technique unknown", 15
fontput 20, 50, "to many programmers. It reads direct from the font area in
ROM, instead", 15
fontput 25, 70, "of using the method used by many programmers in which they
PRINT their", 15
fontput 30, 90, "text and then use the POINT command... so you can use this
method in", 15
fontput 20, 110, "place of that old method, look at the fontput procedure..
Cheers {:o)", 15
fontput 20, 150, "Peter Cooper", 14
A$ = INPUT$(1)
CLS
fontput 1, 1, "PRESS ANY KEY TO EXIT!", 12
A$ = INKEY$
DO
x% = INT((550 - 0 + 1) * RND + 0)
y% = INT((470 - 1 + 1) * RND + 1)
c% = INT((15 - 1 + 1) * RND + 1)
fontput x%, y%, "Hello there!", c%
LOOP UNTIL INKEY$ <> ""
SCREEN 13
CLS
FOR c% = 30 TO 16 STEP -1
fontput 130, 80, "Cheers!", c%
FOR d% = 1 TO 1000
FOR d2% = 1 TO 40
NEXT d2%
NEXT d%
WAIT &H3DA, 8
WAIT &H3DA, 8, 8
NEXT c%
LOCATE 25, 1: PRINT "Press any key to continue";
WHILE LEN(INKEY$) = 0: WEND

SCREEN 12
CLS
PAINT (1, 1), 1
fontput2 0, 0, "The ", 2
fontput2 32, 0, "NEW ", 10
fontput2 64, 0, "algorithm...", 2
fontput2 258, 8, "FontPut Demo", 0
fontput2 260, 10, "FontPut Demo", 15
fontput2 20, 30, "I dont expect you to use this procedure but it uses a
technique unknown", 15
fontput2 20, 50, "to many programmers. It reads direct from the font area in
ROM, instead", 15
fontput2 25, 70, "of using the method used by many programmers in which they
PRINT their", 15
fontput2 30, 90, "text and then use the POINT command... so you can use this
method in", 15
fontput2 20, 110, "place of that old method, look at the fontput procedure..
Cheers {:o)", 15
fontput2 20, 150, "Peter Cooper", 14
A$ = INPUT$(1)
CLS
fontput2 1, 1, "PRESS ANY KEY TO EXIT!", 12
A$ = INKEY$
DO
x% = INT((550 - 0 + 1) * RND + 0)
y% = INT((470 - 1 + 1) * RND + 1)
c% = INT((15 - 1 + 1) * RND + 1)
fontput2 x%, y%, "Hello there!", c%
LOOP UNTIL INKEY$ <> ""
SCREEN 13
CLS
FOR c% = 30 TO 16 STEP -1
fontput2 130, 80, "Cheers!", c%
FOR d% = 1 TO 1000
FOR d2% = 1 TO 40
NEXT d2%
NEXT d%
WAIT &H3DA, 8
WAIT &H3DA, 8, 8
NEXT c%

SUB fontput (z1%, y1%, in$, c%)
DEF SEG = &HFFA6
o1% = z1%
FOR l% = 1 TO LEN(in$)
        l$ = MID$(in$, l%, 1)
        FOR y% = y1% TO y1% + 7
                x% = PEEK(&HE + (ASC(l$) * 8) + (y% - y1%))
                FOR z% = 0 TO 7
                        IF x% AND (2 ^ (7 - z%)) THEN PSET (z1%, y%), c%
                        z1% = z1% + 1
                NEXT z%
                z1% = z1% - 8
        NEXT y%
        z1% = z1% + 8
NEXT l%
DEF SEG
END SUB

'Author: Byron Smith <unol@sat.net, http://www.sat.net/~unol>, 13-NOV-1996
SUB fontput1 (z1%, y1%, in$, c%)
  DEF SEG = -90
  FOR b% = 1 TO LEN(in$): FOR m% = 0 TO 7
    d% = 128 * PEEK(14 + 8 * ASC(MID$(in$, b%, 1)) + m%)
    LINE (z1% + 8 * b% - 9, y1% + m%)-(z1% + 8 * b% - 2, y1% + m%), c%, , d%
  NEXT m%, b%
END SUB

'Author: Byron Smith <unol@sat.net, http://www.sat.net/~unol>, 25-JAN-1997
'Update: Draws two characters at once using LINE.
SUB fontput2 (z1%, y1%, in$, c%)
  DEF SEG = -90
  tmp$ = in$ + " "
  FOR b% = 1 TO LEN(tmp$) - 1 STEP 2: FOR m% = 0 TO 7
  d& = 256& * PEEK(14 + 8 * ASC(MID$(tmp$, b%, 1)) + m%)  'join the next line
             + PEEK(14 + 8 * ASC(MID$(tmp$, b% + 1, 1)) + m%)
  d% = d& + (d& > 32767) * 65536
  LINE (z1% + 8 * b% - 8, y1% + m%)-(z1% + 8 * b% + 7, y1% + m%), c%, , d%
  NEXT m%, b%
END SUB

DECLARE SUB menutema ()
DECLARE SUB menukey ()
DECLARE SUB menuexam ()
DECLARE SUB menugraf ()
DECLARE SUB menuasm ()
DECLARE SUB text (a$)
DECLARE SUB menunagusia ()
DECLARE SUB menucolor ()
DECLARE SUB menuletra ()
DECLARE SUB loadfont (a$)

'******************** Interrupzioak erarbiltzeko **************
'$INCLUDE: 'c:/qbasic/QB.BI'
DIM SHARED inregsx AS RegTypeX
DIM SHARED outregsx AS RegTypeX
'**************************************************************

'*********** Aldagaiak **************
DIM SHARED colortestu1, colortestu2, colorfondo1, colorfondo2
colortestu1 = 8
colortestu2 = 13
colorfondo1 = 7
colorfondo2 = 5

'*************** Kodigo nagusia ******
SCREEN 0, 0, 0, 0
 VIEW PRINT 1 TO 25
 PALETTE 8, 14     '******* Blue koloreak ******
 PALETTE 13, 15
 PALETTE 7, 1
 PALETTE 5, 3
 loadfont "block.fnt"

menunagusia

SUB loadfont (a$)
PCOPY 0, 1
COLOR colortestu2, colorfondo2
LOCATE 12, 30: PRINT "ษออออออออออออออป"
LOCATE 13, 30: PRINT "บ LOADING FONT บ"
LOCATE 14, 30: PRINT "บ  ฑฑฑฑฑฑฑฑฑฑ  บ"
LOCATE 15, 30: PRINT "ศออออออออออออออผ"
OPEN a$ FOR BINARY AS #1
X = 1
LOCATE 14, 33
COLOR colortestu1
FOR i = 0 TO 256
 WAIT &H3DA, 8
chardata$ = ""
  co = (256 / 10) * X
  IF i > co THEN
  PRINT "Û";
  X = X + 1
  WAIT &H3DA, 8
  END IF
FOR ii = 1 TO 16
n = n + 1
SEEK #1, n
a$ = INPUT$(1, #1)
chardata$ = chardata$ + a$
NEXT ii



inregsx.ax = &H1100
inregsx.bx = &H1000
inregsx.cx = &H1
inregsx.dx = i
inregsx.es = SSEG(chardata$)
inregsx.bp = SADD(chardata$)

CALL INTERRUPTX(&H10, inregsx, outregsx)
NEXT i
PRINT "Û";
PCOPY 1, 0
CLOSE #1
END SUB

SUB menuasm
X = 1
PCOPY 0, 4
DO
DO
COLOR colortestu1, colorfondo1
LOCATE 7, 23: PRINT "ษออออออออออออออออออออออออออออออออออออป"
LOCATE 8, 23: PRINT "บ     -= ASM,Interrupts,Pokes =-     บ"
LOCATE 9, 23: PRINT "บออออออออออออออออออออออออออออออออออออบ"
LOCATE 10, 23: PRINT "บ ASMTUT.TXT  => Learn ASM for QB    บ"
LOCATE 11, 23: PRINT "บ ASM.TXT     => Add ASM to your codeบ"
LOCATE 12, 23: PRINT "บ BIOS.TXT    => Use BIOS interrupts บ"
LOCATE 13, 23: PRINT "บ HEX.TXT     => Hexadecimal numbers บ"
LOCATE 14, 23: PRINT "บ HEX-BIN.TXT => Hex and Bin numbers บ"
LOCATE 15, 23: PRINT "บ BITS.TXT    => Take bits from bytesบ"
LOCATE 16, 23: PRINT "บ MEM.TXT     => Segments and offsetsบ"
LOCATE 17, 23: PRINT "บ POKE.TXT    => Poke and Peek       บ"
LOCATE 18, 23: PRINT "บ DEBUG.TXT   => Usage of this tool  บ"
LOCATE 19, 23: PRINT "บ                                    บ"
LOCATE 20, 23: PRINT "ศออออออออออออออออออออออออออออออออออออผ"

COLOR colortestu2, colorfondo2

 SELECT CASE X
 CASE 1
 LOCATE 10, 25: PRINT "ASMTUT.TXT"
 CASE 2
 LOCATE 11, 25: PRINT "ASM.TXT"
 CASE 3
 LOCATE 12, 25: PRINT "BIOS.TXT"
 CASE 4
 LOCATE 13, 25: PRINT "HEX.TXT"
 CASE 5
 LOCATE 14, 25: PRINT "HEX-BIN.TXT"
 CASE 6
 LOCATE 15, 25: PRINT "BITS.TXT"
 CASE 7
 LOCATE 16, 25: PRINT "MEM.TXT "
 CASE 8
 LOCATE 17, 25: PRINT "POKE.TXT"
 CASE 9
 LOCATE 18, 25: PRINT "DEBUG.TXT"
 END SELECT

DO
a$ = INKEY$
LOOP WHILE a$ = ""
IF a$ = CHR$(0) + "H" THEN X = X - 1
IF a$ = CHR$(0) + "P" THEN X = X + 1
IF a$ = CHR$(27) THEN GOTO buk4
IF X > 9 THEN X = 1
IF X < 1 THEN X = 9
LOOP UNTIL a$ = CHR$(13)

 SELECT CASE X
 CASE 1
 text "ASMTUT.TXT"
 CASE 2
 text "ASM.TXT"
 CASE 3
 text "BIOS.TXT"
 CASE 4
 text "HEX.TXT"
 CASE 5
 text "HEX-BIN.TXT"
 CASE 6
 text "BITS.TXT"
 CASE 7
 text "MEM.TXT"
 CASE 8
 text "POKE.TXT"
 CASE 9
 text "debug.txt"
 END SELECT
LOOP
buk4:
PCOPY 4, 0
END SUB

SUB menucolor
PCOPY 0, 2
X = 1
DO

COLOR colortestu1, colorfondo1
LOCATE 10, 30: PRINT "ษออออออออออออออป"
LOCATE 11, 30: PRINT "บ What color?  บ"
LOCATE 12, 30: PRINT "บออออออออออออออบ"
LOCATE 13, 30: PRINT "บ ICE          บ"
LOCATE 14, 30: PRINT "บ FIRE         บ"
LOCATE 15, 30: PRINT "บ BLUE         บ"
LOCATE 16, 30: PRINT "บ BLACK&WHITE  บ"
LOCATE 17, 30: PRINT "ศออออออออออออออผ"

COLOR colortestu2, colorfondo2

 SELECT CASE X
 CASE 1
 LOCATE 13, 32: PRINT "ICE"
 CASE 2
 LOCATE 14, 32: PRINT "FIRE"
 CASE 3
 LOCATE 15, 32: PRINT "BLUE"
 CASE 4
 LOCATE 16, 32: PRINT "BLACK&WHITE"
 END SELECT

DO
a$ = INKEY$
LOOP WHILE a$ = ""
IF a$ = CHR$(0) + "H" THEN X = X - 1
IF a$ = CHR$(0) + "P" THEN X = X + 1
IF a$ = CHR$(27) THEN GOTO buk1
IF X > 4 THEN X = 1
IF X < 1 THEN X = 4
LOOP UNTIL a$ = CHR$(13)

 SELECT CASE X
 CASE 1
 PALETTE 8, 15
 PALETTE 13, 1
 PALETTE 7, 3
 PALETTE 5, 14
 CASE 2
 PALETTE 8, 14
 PALETTE 13, 6
 PALETTE 7, 4
 PALETTE 5, 0
 CASE 3
 PALETTE 8, 14
 PALETTE 13, 15
 PALETTE 7, 1
 PALETTE 5, 3
 CASE 4
 PALETTE 8, 15
 PALETTE 13, 0
 PALETTE 7, 0
 PALETTE 5, 15
 END SELECT
buk1:
PCOPY 2, 0
END SUB

SUB menuexam
X = 1
PCOPY 0, 4
DO
DO
COLOR colortestu1, colorfondo1
LOCATE 7, 23: PRINT "ษออออออออออออออออออออออออออออออออออออป"
LOCATE 8, 23: PRINT "บ    -= Examples & Miscelaneous =-   บ"
LOCATE 9, 23: PRINT "บออออออออออออออออออออออออออออออออออออบ"
LOCATE 10, 23: PRINT "บ BITS.BAS                           บ"
LOCATE 11, 23: PRINT "บ ANIMATE.BAS                        บ"
LOCATE 12, 23: PRINT "บ MODEX.BAS                          บ"
LOCATE 13, 23: PRINT "บ REDEF.BAS                          บ"
LOCATE 14, 23: PRINT "บ FONT.BAS                           บ"
LOCATE 15, 23: PRINT "บ SUBTUTOR.BAS                       บ"
LOCATE 16, 23: PRINT "บ 3SIMULAT.BAS                       บ"
LOCATE 17, 23: PRINT "บ KEYS.BAS                           บ"
LOCATE 18, 23: PRINT "บ WAV.BAS                            บ"
LOCATE 19, 23: PRINT "บ BIOS.BAS                           บ"
LOCATE 20, 23: PRINT "บ REBOOT.BAS                         บ"
LOCATE 21, 23: PRINT "บ STARS.BAS                          บ"
LOCATE 22, 23: PRINT "บ FIRE.BAS                           บ"
LOCATE 23, 23: PRINT "ศออออออออออออออออออออออออออออออออออออผ"

COLOR colortestu2, colorfondo2

 SELECT CASE X
 CASE 1
 LOCATE 10, 25: PRINT "BITS.BAS"
 CASE 2
 LOCATE 11, 25: PRINT "ANIMATE.BAS"
 CASE 3
 LOCATE 12, 25: PRINT "MODEX.BAS"
 CASE 4
 LOCATE 13, 25: PRINT "REDEF.BAS"
 CASE 5
 LOCATE 14, 25: PRINT "FONT.BAS"
 CASE 6
 LOCATE 15, 25: PRINT "SUBTUTOR.BAS"
 CASE 7
 LOCATE 16, 25: PRINT "3SIMULAT.BAS "
 CASE 8
 LOCATE 17, 25: PRINT "KEYS.BAS"
 CASE 9
 LOCATE 18, 25: PRINT "WAV.BAS"
 CASE 10
 LOCATE 19, 25: PRINT "BIOS.BAS"
 CASE 11
 LOCATE 20, 25: PRINT "REBOOT.BAS"
 CASE 12
 LOCATE 21, 25: PRINT "STARS.BAS"
 CASE 13
 LOCATE 22, 25: PRINT "FIRE.BAS"
 END SELECT

DO
a$ = INKEY$
LOOP WHILE a$ = ""
IF a$ = CHR$(0) + "H" THEN X = X - 1
IF a$ = CHR$(0) + "P" THEN X = X + 1
IF a$ = CHR$(27) THEN GOTO buk7
IF X > 13 THEN X = 1
IF X < 1 THEN X = 13
LOOP UNTIL a$ = CHR$(13)

 SELECT CASE X
 CASE 1
 text "BITS.BAS"
 CASE 2
 text "ANIMATE.BAS"
 CASE 3
 text "MODEX.BAS"
 CASE 4
 text "REDEF.BAS"
 CASE 5
 text "FONT.BAS"
 CASE 6
 text "SUBTUTOR.BAS"
 CASE 7
 text "3SIMULAT.BAS"
 CASE 8
 text "KEYS.BAS"
 CASE 9
 text "WAV.BAS"
 CASE 10
 text "BIOS.BAS"
 CASE 11
 text "REBOOT.BAS"
 CASE 12
 text "STARS.BAS"
 CASE 13
 text "FIRE.BAS"
 END SELECT
LOOP
buk7:
PCOPY 4, 0

END SUB

SUB menugraf
X = 1
PCOPY 0, 4
DO
DO
COLOR colortestu1, colorfondo1
LOCATE 7, 23: PRINT "ษออออออออออออออออออออออออออออออออออออป"
LOCATE 8, 23: PRINT "บ           -= Graphics =-           บ"
LOCATE 9, 23: PRINT "บออออออออออออออออออออออออออออออออออออบ"
LOCATE 10, 23: PRINT "บ FLICKER.TXT  => Avoid monitoring   บ"
LOCATE 11, 23: PRINT "บ GETPUT13.TXT=> Get and Put         บ"
LOCATE 12, 23: PRINT "บ OPTIMIZ5.TXT=> Sprites             บ"
LOCATE 13, 23: PRINT "บ SCROLL.TXT  => Pixel by pixel      บ"
LOCATE 14, 23: PRINT "บ STUFF2.TXT  => Bload,Bsave,Palette.บ"
LOCATE 15, 23: PRINT "บ 3D.TXT      => 3D graphics.        บ"
LOCATE 16, 23: PRINT "บ CLIPING.TXT => Scroll tecniques    บ"
LOCATE 17, 23: PRINT "บ SCROLL2.TXT => Tile by tile        บ"
LOCATE 18, 23: PRINT "บ COLORS.TXT  => Using colors        บ"
LOCATE 19, 23: PRINT "บ                                    บ"
LOCATE 20, 23: PRINT "ศออออออออออออออออออออออออออออออออออออผ"

COLOR colortestu2, colorfondo2

 SELECT CASE X
 CASE 1
 LOCATE 10, 25: PRINT "FLICKER.TXT"
 CASE 2
 LOCATE 11, 25: PRINT "GETPUT13.TXT"
 CASE 3
 LOCATE 12, 25: PRINT "OPTIMIZ5.TXT"
 CASE 4
 LOCATE 13, 25: PRINT "SCROLL.TXT"
 CASE 5
 LOCATE 14, 25: PRINT "STUFF2.TXT"
 CASE 6
 LOCATE 15, 25: PRINT "3D.TXT"
 CASE 7
 LOCATE 16, 25: PRINT "CLIPING.TXT "
 CASE 8
 LOCATE 17, 25: PRINT "SCROLL2.TXT"
 CASE 9
 LOCATE 18, 25: PRINT "COLOR2.TXT"
 END SELECT

DO
a$ = INKEY$
LOOP WHILE a$ = ""
IF a$ = CHR$(0) + "H" THEN X = X - 1
IF a$ = CHR$(0) + "P" THEN X = X + 1
IF a$ = CHR$(27) THEN GOTO buk5
IF X > 9 THEN X = 1
IF X < 1 THEN X = 9
LOOP UNTIL a$ = CHR$(13)

 SELECT CASE X
 CASE 1
 text "FLICKER.TXT"
 CASE 2
 text "GETPUT13.TXT"
 CASE 3
 text "OPTIMI~1.TXT"
 CASE 4
 text "SCROLL.TXT"
 CASE 5
 text "STUFF2.TXT"
 CASE 6
 text "3D.TXT"
 CASE 7
 text "CLIPING.TXT"
 CASE 8
 text "SCROLL2.TXT"
 CASE 9
 text "COLORS.txt"
 END SELECT
LOOP
buk5:
PCOPY 4, 0

END SUB

SUB menukey
X = 1
PCOPY 0, 4
DO
DO
COLOR colortestu1, colorfondo1
LOCATE 7, 23: PRINT "ษออออออออออออออออออออออออออออออออออออป"
LOCATE 8, 23: PRINT "บ         -= Keys & Mouse =-         บ"
LOCATE 9, 23: PRINT "บออออออออออออออออออออออออออออออออออออบ"
LOCATE 10, 23: PRINT "บ ARROWS.TXT   => Using the arrows   บ"
LOCATE 11, 23: PRINT "บ KEYS.TXT     => Many ways...       บ"
LOCATE 12, 23: PRINT "บ MOUSE.TXT    => Programing mouse   บ"
LOCATE 13, 23: PRINT "ศออออออออออออออออออออออออออออออออออออผ"

COLOR colortestu2, colorfondo2

 SELECT CASE X
 CASE 1
 LOCATE 10, 25: PRINT "ARROWS.TXT"
 CASE 2
 LOCATE 11, 25: PRINT "KEYS.TXT"
 CASE 3
 LOCATE 12, 25: PRINT "MOUSE.TXT"
 END SELECT

DO
a$ = INKEY$
LOOP WHILE a$ = ""
IF a$ = CHR$(0) + "H" THEN X = X - 1
IF a$ = CHR$(0) + "P" THEN X = X + 1
IF a$ = CHR$(27) THEN GOTO buk6
IF X > 3 THEN X = 1
IF X < 1 THEN X = 3
LOOP UNTIL a$ = CHR$(13)

 SELECT CASE X
 CASE 1
 text "ARROWS.TXT"
 CASE 2
 text "KEYS.TXT"
 CASE 3
 text "MOUSE.TXT"
 END SELECT
LOOP
buk6:
PCOPY 4, 0

END SUB

SUB menuletra
PCOPY 0, 2
X = 1
DO

COLOR colortestu1, colorfondo1
LOCATE 10, 30: PRINT "ษออออออออออออออป"
LOCATE 11, 30: PRINT "บ  Select font บ"
LOCATE 12, 30: PRINT "บออออออออออออออบ"
LOCATE 13, 30: PRINT "บ KIDS-2       บ"
LOCATE 14, 30: PRINT "บ BLOCK        บ"
LOCATE 15, 30: PRINT "บ ROMAN-1      บ"
LOCATE 16, 30: PRINT "บ ANTIQUE      บ"
LOCATE 17, 30: PRINT "บ COURIOR      บ"
LOCATE 18, 30: PRINT "บ COMPUTER     บ"
LOCATE 19, 30: PRINT "บ ASCII        บ"
LOCATE 20, 30: PRINT "ศออออออออออออออผ"

COLOR colortestu2, colorfondo2
 SELECT CASE X
 CASE 1
 LOCATE 13, 32: PRINT "KIDS-2"
 CASE 2
 LOCATE 14, 32: PRINT "BLOCK"
 CASE 3
 LOCATE 15, 32: PRINT "ROMAN-1"
 CASE 4
 LOCATE 16, 32: PRINT "ANTIQUE"
 CASE 5
 LOCATE 17, 32: PRINT "COURIOR"
 CASE 6
 LOCATE 18, 32: PRINT "COMPUTER"
 CASE 7
 LOCATE 19, 32: PRINT "ASCII"
 END SELECT

DO
a$ = INKEY$
LOOP WHILE a$ = ""
IF a$ = CHR$(0) + "H" THEN X = X - 1
IF a$ = CHR$(0) + "P" THEN X = X + 1
IF a$ = CHR$(27) THEN GOTO buk2
IF X > 7 THEN X = 1
IF X < 1 THEN X = 7
LOOP UNTIL a$ = CHR$(13)

 SELECT CASE X
 CASE 1
 loadfont "KIDS-2.FNT"
 CASE 2
 loadfont "BLOCK.FNT"
 CASE 3
 loadfont "ROMAN-1.FNT"
 CASE 4
 loadfont "ANTIQUE.FNT"
 CASE 5
 loadfont "COURIOR.fnt"
 CASE 6
 loadfont "COMPUTER.fnt"
 CASE 7
 loadfont "ASCII.fnt"
 END SELECT
buk2:
PCOPY 2, 0
END SUB

SUB menunagusia
LOCATE 24, 1: COLOR colortestu1, 0
PRINT "   Use the arrow keys to move up and down. [RETURN] to select. [ESC] key to exit"
COLOR colortestu1, colorfondo1
LOCATE 2, 10: PRINT "  The qbasic mini enciclopedy    "
LOCATE 3, 10: PRINT "     Hur => h20ur@yahoo.com      "
COLOR colortestu2, colorfondo2
LOCATE 4, 10: PRINT "http://www.geocities.com/siliconvalley/campus/9647     "
X = 1
DO
DO
COLOR colortestu1, colorfondo1
LOCATE 5, 18: PRINT "ษออออออออออออออออออออออออออออออออออออป"
LOCATE 6, 18: PRINT "บ        -= The main menu =-         บ"
LOCATE 7, 18: PRINT "บออออออออออออออออออออออออออออออออออออบ"
LOCATE 8, 18: PRINT "บ README.TXT                         บ"
LOCATE 9, 18: PRINT "บ     To know what is this.          บ"
LOCATE 10, 18: PRINT "บ                                    บ"
LOCATE 11, 18: PRINT "บ CHANGE COLORS                      บ"
LOCATE 12, 18: PRINT "บ CHANGE FONT                        บ"
LOCATE 13, 18: PRINT "บ                                    บ"
LOCATE 14, 18: PRINT "บ ENTER THE TUTORIAL                 บ"
LOCATE 15, 18: PRINT "บ      This is the most interesant   บ"
LOCATE 16, 18: PRINT "บ      part.                         บ"
LOCATE 17, 18: PRINT "บ                                    บ"
LOCATE 18, 18: PRINT "ศออออออออออออออออออออออออออออออออออออผ"

COLOR colortestu2, colorfondo2

 SELECT CASE X
 CASE 1
 LOCATE 8, 20: PRINT "README.TXT"
 CASE 2
 LOCATE 11, 20: PRINT "CHANGE COLORS"
 CASE 3
 LOCATE 12, 20: PRINT "CHANGE FONT"
 CASE 4
 LOCATE 14, 20: PRINT "ENTER THE TUTORIAL"
 END SELECT

DO
a$ = INKEY$
LOOP WHILE a$ = ""
IF a$ = CHR$(0) + "H" THEN X = X - 1
IF a$ = CHR$(0) + "P" THEN X = X + 1
IF a$ = CHR$(27) THEN GOTO buk3
IF X > 4 THEN X = 1
IF X < 1 THEN X = 4
LOOP UNTIL a$ = CHR$(13)

 SELECT CASE X
 CASE 1
 text "readme.txt"
 CASE 2
 menucolor
 CASE 3
 menuletra
 CASE 4
 menutema
 END SELECT
LOOP
buk3:
SYSTEM

END SUB

SUB menutema
X = 1
PCOPY 0, 1
DO
DO
COLOR colortestu1, colorfondo1
LOCATE 6, 20: PRINT "ษออออออออออออออออออออออออออออออออออออป"
LOCATE 7, 20: PRINT "บ     -= The Tutorial Themes =-      บ"
LOCATE 8, 20: PRINT "บออออออออออออออออออออออออออออออออออออบ"
LOCATE 9, 20: PRINT "บ ASM, INTERRUPTS,POKES...           บ"
LOCATE 10, 20: PRINT "บ     This is what you need to       บ"
LOCATE 11, 20: PRINT "บ     improve your code.             บ"
LOCATE 12, 20: PRINT "บ GRAPHICS                           บ"
LOCATE 13, 20: PRINT "บ     Advanced stuff.                บ"
LOCATE 14, 20: PRINT "บ KEYS & MOUSE                       บ"
LOCATE 15, 20: PRINT "บ     The key traping.               บ"
LOCATE 16, 20: PRINT "บ EXAMPLES & MISCELANEOUS            บ"
LOCATE 17, 20: PRINT "บ     Programs and tips.             บ"
LOCATE 18, 20: PRINT "บ                                    บ"
LOCATE 19, 20: PRINT "ศออออออออออออออออออออออออออออออออออออผ"

COLOR colortestu2, colorfondo2

 SELECT CASE X
 CASE 1
 LOCATE 9, 22: PRINT "ASM, INTERRUPTS,POKES... "
 CASE 2
 LOCATE 12, 22: PRINT "GRAPHICS"
 CASE 3
 LOCATE 14, 22: PRINT "KEYS & MOUSE"
 CASE 4
 LOCATE 16, 22: PRINT "EXAMPLES & MISCELANEOUS"
 END SELECT

DO
a$ = INKEY$
LOOP WHILE a$ = ""
IF a$ = CHR$(0) + "H" THEN X = X - 1
IF a$ = CHR$(0) + "P" THEN X = X + 1
IF a$ = CHR$(27) THEN GOTO buk8
IF X > 4 THEN X = 1
IF X < 1 THEN X = 4
LOOP UNTIL a$ = CHR$(13)

 SELECT CASE X
 CASE 1
 menuasm
 CASE 2
 menugraf
 CASE 3
 menukey
 CASE 4
 menuexam
 END SELECT
LOOP
buk8:
PCOPY 1, 0
END SUB

SUB text (a$)
PCOPY 0, 3
DIM lines(800) AS STRING * 80
DIM temp(25) AS STRING * 80
OPEN a$ FOR INPUT AS #1
  i = 0
  DO 'WHILE NOT EOF(1)
   LINE INPUT #1, lines(i)
   i = i + 1
 LOOP UNTIL EOF(1)
  en = i - 1
CLOSE #1
LOCATE 24, 1: COLOR colortestu2, colorfondo2
PRINT "                                                                                "

LOCATE 24, 1: COLOR colortestu2, colorfondo2
PRINT "Use arrows,(ESC) Exit | Time: "; TIME$; " | Line: "; lin; "/"; en; " | File: "; a$

FOR i = 1 TO 23
  LOCATE i, 1: COLOR colortestu1, colorfondo1: PRINT lines(i)
  temp(i) = lines(i)
NEXT i

DO

  SELECT CASE INKEY$
    CASE CHR$(0) + "H":
      IF lin >= 1 THEN lin = lin - 1
      FOR i = 1 TO 23
	IF LEN(lines(i + lin)) >= 81 THEN temp(i) = MID$(lines(i + lin), 1, 80)
	IF LEN(lines(i + lin)) <= 80 THEN temp(i) = lines(i + lin)
	LOCATE i, 1: COLOR colortestu1, colorfondo1: PRINT temp(i)
      NEXT i
LOCATE 24, 1: COLOR colortestu2, colorfondo2
PRINT "                                                                                "

      LOCATE 24, 1: COLOR colortestu2, colorfondo2: PRINT "Use arrows,(ESC) Exit | Time: "; TIME$; " | Line: "; lin; "/"; en; " | File: "; a$
    CASE CHR$(0) + "P":
      IF lin <= en - 21 THEN lin = lin + 1
     FOR i = 1 TO 23
	IF LEN(lines(i + lin)) >= 81 THEN temp(i) = MID$(lines(i + lin), 1, 80)
	IF LEN(lines(i + lin)) <= 80 THEN temp(i) = lines(i + lin)
	LOCATE i, 1: COLOR colortestu1, colorfondo1: PRINT temp(i)
      NEXT i
LOCATE 24, 1: COLOR colortestu2, colorfondo2
PRINT "                                                                                "

      LOCATE 24, 1: COLOR colortestu2, colorfondo2: PRINT "Use arrows,(ESC) Exit | Time: "; TIME$; " | Line: "; lin; "/"; en; " | File: "; a$
    CASE CHR$(27): EXIT DO
    CASE CHR$(0) + "Q":
      IF lin <= en - 21 THEN lin = lin + 20
       FOR i = 1 TO 23
	IF LEN(lines(i + lin)) >= 81 THEN temp(i) = MID$(lines(i + lin), 1, 80)
	IF LEN(lines(i + lin)) <= 80 THEN temp(i) = lines(i + lin)
	LOCATE i, 1: COLOR colortestu1, colorfondo1: PRINT temp(i)
      NEXT i
LOCATE 24, 1: COLOR colortestu2, colorfondo2
PRINT "                                                                                "

      LOCATE 24, 1: COLOR colortestu2, colorfondo2: PRINT "Use arrows,(ESC) Exit | Time: "; TIME$; " | Line: "; lin; "/"; en; " | File: "; a$
    CASE CHR$(0) + "I":
      IF lin >= 1 THEN lin = lin - 20
      IF lin < 0 THEN lin = 0
      FOR i = 1 TO 23
	IF LEN(lines(i + lin)) >= 81 THEN temp(i) = MID$(lines(i + lin), 1, 80)
	IF LEN(lines(i + lin)) <= 80 THEN temp(i) = lines(i + lin)
	LOCATE i, 1: COLOR colortestu1, colorfondo1: PRINT temp(i)
      NEXT i
LOCATE 24, 1: COLOR colortestu2, colorfondo2
PRINT "                                                                                "

      LOCATE 24, 1: COLOR colortestu2, colorfondo2: PRINT "Use arrows,(ESC) Exit | Time: "; TIME$; " | Line: "; lin; "/"; en; " | File: "; a$

  END SELECT

LOOP
PCOPY 3, 0
END SUB


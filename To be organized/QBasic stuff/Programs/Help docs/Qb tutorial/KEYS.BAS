
DEFINT A-Z

DECLARE SUB KeyOnn (InsKey%, CapKey%, NumKey%, ScrKey%, AltKey%, CtrKey%, LSfKey%, RSfKey%)

CLS
LOCATE , , 0

DO
 
  Hi$ = INKEY$
 
  CALL KeyOnn(InsKey, CapKey, NumKey, ScrKey, AltKey, CtrKey, LSfKey, RSfKey)
 
  LOCATE 1, 1: PRINT "Last Key:", , " "; Hi$
  LOCATE 2, 1: PRINT "Last Key's ASC:",
 
  FOR I = 1 TO LEN(Hi$)
    PRINT ASC(MID$(Hi$, I, 1)),
  NEXT I
 
  LOCATE 3, 1: PRINT "Insert:", , InsKey; SPACE$(80 - POS(0))
  LOCATE 4, 1: PRINT "Capitals Lock:", CapKey; SPACE$(80 - POS(0))
  LOCATE 5, 1: PRINT "Number Lock:", , NumKey; SPACE$(80 - POS(0))
  LOCATE 6, 1: PRINT "Scroll Lock:", , ScrKey; SPACE$(80 - POS(0))
  LOCATE 7, 1: PRINT "Alternate:", , AltKey; SPACE$(80 - POS(0))
  LOCATE 8, 1: PRINT "Control:", , CtrKey; SPACE$(80 - POS(0))
  LOCATE 9, 1: PRINT "Left Shift:", , LSfKey; SPACE$(80 - POS(0))
  LOCATE 10, 1: PRINT "Right Shift:", , RSfKey; SPACE$(80 - POS(0))

LOOP UNTIL Hi$ = CHR$(27)
END

SUB KeyOnn (InsKey%, CapKey%, NumKey%, ScrKey%, AltKey%, CtrKey%, LSfKey%, RSfKey%)

  DEF SEG = 0
  KeySts% = PEEK(1047)

  InsKey% = 0
  NumKey% = 0
  CapKey% = 0
  ScrKey% = 0
  RSfKey% = 0
  LSfKey% = 0
  CtrKey% = 0
  AltKey% = 0

  KeySts = KeySts + 1
  IF KeySts > 128 THEN KeySts = KeySts - 128: InsKey% = 1
  IF KeySts > 64 THEN KeySts = KeySts - 64: CapKey% = 1
  IF KeySts > 32 THEN KeySts = KeySts - 32: NumKey% = 1
  IF KeySts > 16 THEN KeySts = KeySts - 16: ScrKey% = 1
  IF KeySts > 8 THEN KeySts = KeySts - 8: AltKey% = 1
  IF KeySts > 4 THEN KeySts = KeySts - 4: CtrKey% = 1
  IF KeySts > 2 THEN KeySts = KeySts - 2: LSfKey% = 1
  IF KeySts > 1 THEN KeySts = KeySts - 1: RSfKey% = 1

END SUB


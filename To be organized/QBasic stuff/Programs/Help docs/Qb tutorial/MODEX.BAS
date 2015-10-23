
' It is recommended that you download this from the FTP site instead of
' the WWW site.

DECLARE SUB ModeX ()
DECLARE SUB OutPort (PortAddr&, vh%, vl%)
DECLARE SUB Standard ()
DECLARE SUB SetVOffset (Offset&)
'$DYNAMIC

DEFINT A-Z
SCREEN 13
DEF SEG = &HA000: CALL ModeX

FOR Offset& = 0 TO 15999
  rgbbase% = Offset& MOD 64
  POKE Offset&, rgbbase%
  POKE Offset& + 16000, rgbbase% + 64
  POKE Offset& + 32000, rgbbase% + 128
  POKE Offset& + 48000, rgbbase% + 192
NEXT Offset&

Offset& = 0: OffsetInc% = 80: Frame% = 0
DO
  Offset& = Offset& + OffsetInc%
  CALL SetVOffset(Offset&): WAIT &H3DA, 8
 
  Frame% = Frame% + 1
    IF Frame% = 99 THEN OffsetInc% = 81
    IF Frame% = 199 THEN OffsetInc% = 79
    IF Frame% = 299 THEN OffsetInc% = 80
    IF Frame% = 499 THEN OffsetInc% = -80
    IF Frame% = 599 THEN OffsetInc% = -79
    IF Frame% = 699 THEN OffsetInc% = -81
    IF Frame% = 799 THEN OffsetInc% = -80
    IF Frame% = 899 THEN
      CLS : PALETTE
      CALL SetVOffset(0): CALL Standard
      END
    END IF
  FOR Delay% = 1 TO 1000: NEXT
LOOP

REM $STATIC
DEFSNG A-Z
SUB ModeX
  CALL OutPort(&H3C4, 6, 4): CLS
  CALL OutPort(&H3D4, 227, 23)
  CALL OutPort(&H3D4, 0, 20)
  CALL OutPort(&H3C4, 15, 2)
END SUB

SUB OutPort (PortAddr&, vh%, vl%)
  OUT PortAddr&, vl%: OUT PortAddr& + 1, vh%
END SUB'<br>

SUB SetVOffset (Offset&)
  V& = Offset&
  CALL OutPort(&H3D4, V& \ 256, 12)
  CALL OutPort(&H3D4, V& AND 255, 13)
END SUB'<br>

SUB Standard
  CALL OutPort(&H3C4, 14, 4)
  CALL OutPort(&H3D4, 163, &H17)
  CALL OutPort(&H3D4, 64, &H14)
  CALL OutPort(&H3C4, 15, &H2)
END SUB


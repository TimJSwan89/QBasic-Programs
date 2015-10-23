
DECLARE FUNCTION VidMem% ()
 
DEFINT A-Z
FUNCTION VidMem%
 
' Returns the amount of Video Memory
' PC's BIOS only reports up to 256K, though.
'------------------------------------------------------------------------
 
DEF SEG = 0
vm = PEEK(&H487)
vm = Byte AND 96
vm = vm \ 32
vm = (vm + 1) * 64
DEF SEG
VidMem% = vm
 
END FUNCTION
 
 Next is a program which actually goes into all the details of different
 things in your computer. It's a good program. Run it!

' CMOS.BAS
' Reads the contents of the CMOS
'
DEFINT A-Z
 
DECLARE FUNCTION CMOSBattery% ()
DECLARE FUNCTION BitOn% (Which%, IntVal%)
DECLARE FUNCTION DriveType% (Drv%)
DECLARE FUNCTION Hex2Bin$ (Hcs$)
DECLARE FUNCTION TotalMem% ()
CLS
PRINT "CMOS Contents:"
FOR i = 0 TO &H7F
 OUT &H70, i
 PRINT USING "\   \"; HEX$(INP(&H71));
NEXT i
PRINT " "
PRINT "CMOS Battery State: ";
IF CMOSBattery% THEN PRINT "Good" ELSE PRINT "Battery Dead"
PRINT
k% = DriveType%(1)
PRINT "Drive A: ";
IF k% = 0 THEN PRINT "None"
IF k% = 1 THEN PRINT "5"; CHR$(172); " 360K"
IF k% = 2 THEN PRINT "5"; CHR$(172); " 1.2M"
IF k% = 3 THEN PRINT "3"; CHR$(171); " 720K"
IF k% = 4 THEN PRINT "3"; CHR$(171); " 1.44M"
k% = DriveType%(2)
PRINT "Drive B: ";
IF k% = 0 THEN PRINT "None"
IF k% = 1 THEN PRINT "5"; CHR$(172); " 360K"
IF k% = 2 THEN PRINT "5"; CHR$(172); " 1.2M"
IF k% = 3 THEN PRINT "3"; CHR$(171); " 720K"
IF k% = 4 THEN PRINT "3"; CHR$(171); " 1.44M"
PRINT
OUT &H70, &H19
b% = INP(&H71)
PRINT "Hard Disk 0 Type:";
IF b <> 0 THEN PRINT b% ELSE PRINT " Not Installed"
OUT &H70, &H1A
b% = INP(&H71)
PRINT "Hard Disk 1 Type:";
IF b <> 0 THEN PRINT b% ELSE PRINT " Not Installed" 
PRINT
 
OUT &H70, &H15
b% = INP(&H71)
OUT &H70, &H16
b1% = INP(&H71)
PRINT "Base Memory:"; RTRIM$(STR$(CVI(CHR$(b) + CHR$(b1%)))); "K"
 
OUT &H70, &H17
b% = INP(&H71)
OUT &H70, &H18
b1% = INP(&H71)
PRINT "Extended Memory:"; RTRIM$(STR$(CVI(CHR$(b) + CHR$(b1%)))); "K"
 
PRINT "Total System Memory:"; RTRIM$(STR$(TotalMem%)); "K"
 
FUNCTION BitOn (Which, IntVal)
   BitOn = 0
   SELECT CASE Which
      CASE 1: IF (IntVal AND 128) THEN BitOn = (-1)
      CASE 2: IF (IntVal AND 64) THEN BitOn = (-1)
      CASE 3: IF (IntVal AND 32) THEN BitOn = (-1)
      CASE 4: IF (IntVal AND 16) THEN BitOn = (-1)
      CASE 5: IF (IntVal AND 8) THEN BitOn = (-1)
      CASE 6: IF (IntVal AND 4) THEN BitOn = (-1)
      CASE 7: IF (IntVal AND 2) THEN BitOn = (-1)
      CASE 8: IF (IntVal AND 1) THEN BitOn = (-1)
      CASE 9: IF (IntVal AND (-32768)) THEN BitOn = (-1)
      CASE 10: IF (IntVal AND 16384) THEN BitOn = (-1)
      CASE 11: IF (IntVal AND 8192) THEN BitOn = (-1)
      CASE 12: IF (IntVal AND 4096) THEN BitOn = (-1)
      CASE 13: IF (IntVal AND 2048) THEN BitOn = (-1)
      CASE 14: IF (IntVal AND 1024) THEN BitOn = (-1)
      CASE 15: IF (IntVal AND 512) THEN BitOn = (-1)
      CASE 16: IF (IntVal AND 256) THEN BitOn = (-1)
   END SELECT
END FUNCTION
 
FUNCTION CMOSBattery%
	OUT &H70, &HD
	b% = INP(&H71)
	c = BitOn%(1, b%)
	CMOSBattery% = c
END FUNCTION

FUNCTION DriveType% (Drv%)
	OUT &H70, &H10
	b% = INP(&H71)
	IF Drv% = 1 THEN 
		t$ = LEFT$(Hex2Bin$(LTRIM$(RTRIM$(HEX$(b%)))), 4)
	ELSE
		t$ = MID$(Hex2Bin$(LTRIM$(RTRIM$(HEX$(b%)))), 5, 4)
	END IF
	IF t$ = "0001" THEN DriveType% = 1
	IF t$ = "0010" THEN DriveType% = 2
	IF t$ = "0011" THEN DriveType% = 3
	IF t$ = "0100" THEN DriveType% = 4
END FUNCTION
 
FUNCTION Hex2Bin$ (Hcs$)
   Hcs$ = UCASE$(Hcs$)
   lc = LEN(Hcs$)
   FOR x = 1 TO lc
      SELECT CASE MID$(Hcs$, x, 1)
	 CASE "0"
	    Out$ = Out$ + "0000"
	 CASE "1"
	    Out$ = Out$ + "0001"
	 CASE "2"
	    Out$ = Out$ + "0010"
	 CASE "3"
	    Out$ = Out$ + "0011"
	 CASE "4"
	    Out$ = Out$ + "0100"
	 CASE "5"
	    Out$ = Out$ + "0101"
	 CASE "6"
	    Out$ = Out$ + "0110"
	 CASE "7"
	    Out$ = Out$ + "0111"
	 CASE "8"
	    Out$ = Out$ + "1000"
	 CASE "9"
	    Out$ = Out$ + "1001"
	 CASE "A"
	    Out$ = Out$ + "1010"
	 CASE "B"
	    Out$ = Out$ + "1011"
	 CASE "C"
	    Out$ = Out$ + "1100"
	 CASE "D"
	    Out$ = Out$ + "1101"
	 CASE "E"
	    Out$ = Out$ + "1110"
	 CASE "F"
	    Out$ = Out$ + "1111"
      END SELECT
   NEXT
   Hex2Bin$ = Out$
END FUNCTION
 
FUNCTION TotalMem%
	OUT &H70, &H15
	b% = INP(&H71)
	OUT &H70, &H16
	b1% = INP(&H71)
	a1% = CVI(CHR$(b) + CHR$(b1%))
	OUT &H70, &H17
	b% = INP(&H71)
	OUT &H70, &H18
	b1% = INP(&H71)
	a2% = CVI(CHR$(b) + CHR$(b1%))
	TotalMem% = a1% + a2%
END FUNCTION

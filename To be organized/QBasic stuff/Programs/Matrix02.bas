WHILE 1 = 1
CLS
speednum = 10
lengthnum = 10
WHILE speednum = 10
INPUT "Would you like fast(1), medium(2), or slow(3)"; speed$
speednum = 10
IF speed$ = "fast" OR speed$ = "1" THEN
speednum = 75
ELSEIF speed$ = "medium" OR speed$ = "2" THEN
speednum = 500
ELSEIF speed$ = "slow" OR speed$ = "3" THEN
speednum = 1000
ELSE
PRINT "Please answer with: 1,2, or 3."
END IF
WEND
WHILE lengthnum = 10
PRINT "Would you like to run for a short(1), medium(2),"
INPUT "long(3), or infinite time(4)"; length$
lengthnum = 10
IF length$ = "short" OR length$ = "1" THEN
lengthnum = 15
ELSEIF length$ = "medium" OR length$ = "2" THEN
lengthnum = 40
ELSEIF length$ = "long" OR length$ = "3" THEN
lengthnum = 100
ELSEIF length$ = "infinite" OR length$ = "4" THEN
PRINT "In order to stop this program, you must press the windows key", "and terminate it from the Start Menu."
lengthnum = 50
ELSE
PRINT "Please answer with: 1 2 3, or 4."
END IF
WEND
PRINT "Press enter if you would like to run default,"
INPUT "or enter in characters that you would like to display.", character$
CLS
RANDOMIZE TIMER
DIM x(1 TO 23)
DIM c(1 TO 23)
FOR a = 1 TO 23
c(a) = 1
x(a) = 1
NEXT
FOR t = 1 TO lengthnum
FOR a = 1 TO 23
c(a) = x(a)
x(a) = INT(RND * 78) + 1
  FOR y = 1 TO 23
  FOR delayer = 1 TO speednum: NEXT
  z = a - y + 1
    IF z < 1 THEN
    z = z + 23
    END IF
  IF character$ = "" THEN
  char = INT(RND * 254) + 1
  WHILE char = 0 OR char = 7 OR char = 9 OR char = 10 OR char = 12 OR char = 13 OR char = 26 OR char = 31 OR char = 255
  char = INT(RND * 254) + 1
  WEND
  char$ = CHR$(char)
  ELSE
  char$ = MID$(character$, INT(RND * LEN(character$) + 1), 1)
  END IF
  LOCATE y, x(z)
  COLOR 10
  PRINT char$
  LOCATE y, c(z)
  PRINT " "
    IF y = 1 THEN
    LOCATE 23, c(z)
    ELSE
    LOCATE y - 1, x(z)
    END IF
  COLOR 2
  PRINT char$
  NEXT y
NEXT a
  IF length$ = "infinite" OR length$ = "4" THEN
  lengthnum = 1
  END IF
NEXT t
WEND
'x(y) = 1
'erasex(y) = 1
'NEXT

'FOR a = 1 TO 50

'FOR y = 1 TO 10
'  FOR delayer = 1 TO 20000
'  NEXT

'  char = INT(RND * 253) + 1
'  WHILE char = 0 OR char = 7 OR char = 9 OR char = 10 OR char = 13 OR char = 26 OR char = 255
'    char = INT(RND * 253) + 1
'  WEND
'   x(1) = INT(RND * 78) + 1
'   LOCATE y, x(y)
'   IF y = 1 THEN
'   COLOR 10
'   ELSE
'   COLOR 2
'   END IF
'   PRINT CHR$(char)
'NEXT
'    FOR y = 1 TO 10
'      IF y = 1 THEN
'        erasex(1) = x(10)
'      ELSE
'        erasex(y) = erasex(y - 1)
'      END IF
'    NEXT
'    FOR y = 1 TO 10
'    LOCATE y, erasex(y)
'    'PRINT " "
'    NEXT
'    FOR y = 1 TO 10
'       IF y <> 1 THEN
'      'IF y = 1 THEN
'        'varsave = x(10)
'      'ELSE
'        x(y) = x(y - 1)
'      END IF
'    NEXT
'NEXT


CLS
RANDOMIZE TIMER
FOR y = 1 TO 10
x(y) = 1
erasex(y) = 1
NEXT

FOR a = 1 TO 50

FOR y = 1 TO 10
  FOR delayer = 1 TO 20000
  NEXT

  char = INT(RND * 253) + 1
  WHILE char = 0 OR char = 7 OR char = 9 OR char = 10 OR char = 13 OR char = 26 OR char = 255
    char = INT(RND * 253) + 1
  WEND
   x(1) = INT(RND * 78) + 1
   LOCATE y, x(y)
   IF y = 1 THEN
   COLOR 10
   ELSE
   COLOR 2
   END IF
   PRINT CHR$(char)
NEXT
    FOR y = 1 TO 10
      IF y = 1 THEN
        erasex(1) = x(10)
      ELSE
        erasex(y) = erasex(y - 1)
      END IF
    NEXT
    FOR y = 1 TO 10
    LOCATE y, erasex(y)
    'PRINT " "
    NEXT
    FOR y = 1 TO 10
       IF y <> 1 THEN
      'IF y = 1 THEN
        'varsave = x(10)
      'ELSE
        x(y) = x(y - 1)
      END IF
    NEXT
NEXT


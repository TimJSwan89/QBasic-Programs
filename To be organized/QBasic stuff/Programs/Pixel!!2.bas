size = 100
numdots = 100
' StaggerBy = INT(size / numdots)
SCREEN 12
CLS
RANDOMIZE TIMER
x = 200
y = 200
DIM xspeed(100)
DIM yspeed(100)
DIM colour(100)
DIM dotx(100)
DIM doty(100)
DO
FOR a = 1 TO size
dotx(a) = x
doty(a) = y
xspeed(a) = RND * 2 - 1
yspeed(a) = RND * 2 - 1
colour(a) = INT(RND * 14) + 1
key$ = INKEY$
IF key$ = CHR$(0) + "P" THEN
y = y + 10
ELSEIF key$ = CHR$(0) + "M" THEN
x = x + 10
ELSEIF key$ = CHR$(0) + "H" THEN
y = y - 10
ELSEIF key$ = CHR$(0) + "K" THEN
x = x - 10
ELSE
END IF

' IF numdotsstep < numdots THEN
' numdotsstep = numdotsstep + 1: REM make smooth stream instead of explosion
' END IF
' FOR b = 1 TO numdotsstep

  FOR b = 1 TO numdots
    PSET (INT(dotx(b)), INT(doty(b))), 0
    dotx(b) = dotx(b) + xspeed(b)
    doty(b) = doty(b) + yspeed(b)
    PSET (INT(dotx(b)), INT(doty(b))), colour(b)
    FOR c = 1 TO 10: NEXT c
  NEXT b
NEXT a
LOOP


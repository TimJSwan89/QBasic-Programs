size = 60
numdots = 10
StaggerBy = INT(size / numdots)
SCREEN 12
CLS
RANDOMIZE TIMER
x = 200
y = 200
DIM xspeed(100)
DIM yspeed(100)
DIM xdist(100)
DIM ydist(100)
DIM xdistold(100)
DIM ydistold(100)
DIM colour(100)
DIM xold(100)
DIM yold(100)
DO
FOR a = 1 TO size
xdist(a) = 0
ydist(a) = 0
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
xold(a) = x
yold(a) = y

REM IF numdotsstep < numdots THEN
REM numdotsstep = numdotsstep + 1: REM make smooth stream instead of explosion
REM END IF
REM FOR b = 1 TO numdotsstep

FOR b = 1 TO numdots
PSET (INT(xold(b) + xdist(b)), INT(yold(b) + ydist(b))), colour(b)
IF INT(xdist(b)) <> INT(xdistold(b)) OR INT(ydist(b)) <> INT(ydistold(b)) THEN PSET (INT(xold(b) + xdistold(b)), INT(yold(b) + ydistold(b))), 0
xdistold(b) = xdist(b)
ydistold(b) = ydist(b)
xdist(b) = xdist(b) + xspeed(b)
ydist(b) = ydist(b) + yspeed(b)
FOR c = 1 TO 1000: NEXT c
NEXT b
NEXT a
LOOP


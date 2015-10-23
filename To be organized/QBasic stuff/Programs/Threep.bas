DEFINT A-Z

TYPE pnt                                        'type for each 3D point
    x AS INTEGER                                'x coord (horizontal)
    y AS INTEGER                                'y coord (vertical)
    Z AS INTEGER                                'z coord (into the screen)
    p AS INTEGER                                'dist from center of object
END TYPE

numLines = 12 - 1

DIM lO(numLines, 1) AS pnt                      'Original line coords
DIM lr(numLines, 1) AS pnt                      'Rotated coords
DIM scrX(numLines, 1)                           'screen x coord
DIM scrY(numLines, 1)                           'screen y coord
DIM oldX(numLines, 1)                           'old x coord for erasing
DIM oldY(numLines, 1)                           'old y coord for erasing

DIM s!(359)                                     'trig tables
DIM c!(359)

CONST PI = 3.141592

FOR i = 0 TO 359                                'create sine and cosine
   s!(i) = SIN(i * (PI / 180))                  'look up tables to speed up
   c!(i) = COS(i * (PI / 180))                  'the math
NEXT

' Read two points instead of one.
FOR i = 0 TO numLines
   READ lO(i, 0).x, lO(i, 0).y, lO(i, 0).Z, lO(i, 0).p
   READ lO(i, 1).x, lO(i, 1).y, lO(i, 1).Z, lO(i, 1).p
NEXT

SCREEN 13
CLS

xCenter = 160: yCenter = 100: zCenter = 256
theta = 0: phi = 0
thetaRot = 2: phiRot = 2

justStarted = 1
'FOR timtime = 1 TO 10000
DO
key$ = INKEY$
SELECT CASE key$
CASE CHR$(0) + "H"
yCenter = yCenter + 100
CASE CHR$(0) + "K"
yCenter = yCenter - 100
CASE CHR$(0) + "P"
xCenter = xCenter + 100
CASE CHR$(0) + "M"
xCenter = xCenter - 100
CASE "m"
zCenter = zCenter + 100
CASE "n"
zCenter = zCenter - 100
CASE ELSE
END SELECT
LOCATE 4, 4
  FOR i = 0 TO numLines
     ' Save the old values of x and y so we can erase the balls later.
     oldX(i, 0) = scrX(i, 0): oldY(i, 0) = scrY(i, 0)
     oldX(i, 1) = scrX(i, 1): oldY(i, 1) = scrY(i, 1)
    
     ' Rotate both points on each axis.
     lr(i, 0).x = -lO(i, 0).x * s!(theta) + lO(i, 0).y * c!(theta)
     lr(i, 0).y = -lO(i, 0).x * c!(theta) * s!(phi) - lO(i, 0).y * s!(theta) * s!(phi) - lO(i, 0).Z * c!(phi) + lO(i, 0).p
     lr(i, 0).Z = -lO(i, 0).x * c!(theta) * c!(phi) - lO(i, 0).y * s!(theta) * c!(phi) + lO(i, 0).Z * s!(phi)

     lr(i, 1).x = -lO(i, 1).x * s!(theta) + lO(i, 1).y * c!(theta)
     lr(i, 1).y = -lO(i, 1).x * c!(theta) * s!(phi) - lO(i, 1).y * s!(theta) * s!(phi) - lO(i, 1).Z * c!(phi) + lO(i, 1).p
     lr(i, 1).Z = -lO(i, 1).x * c!(theta) * c!(phi) - lO(i, 1).y * s!(theta) * c!(phi) + lO(i, 1).Z * s!(phi)
'LOCATE 4, 4
PRINT lr(i, 0).x
     ' Translate both points from 3D to 2D.
     IF (lr(i, 0).Z + zCenter) <> 0 THEN
       scrX(i, 0) = 256 * (lr(i, 0).x / (lr(i, 0).Z + zCenter)) + xCenter
       scrY(i, 0) = 256 * (lr(i, 0).y / (lr(i, 0).Z + zCenter)) + yCenter
     END IF

     IF (lr(i, 1).Z + zCenter) <> 0 THEN
       scrX(i, 1) = 256 * (lr(i, 1).x / (lr(i, 1).Z + zCenter)) + xCenter
       scrY(i, 1) = 256 * (lr(i, 1).y / (lr(i, 1).Z + zCenter)) + yCenter
     END IF
      '''  LINE (scrX(i, 0), scrY(i, 0))-(scrX(i, 1), scrY(i, 1)), 11
  NEXT i
LOCATE 5, 4
PRINT phi


  ' Erase the old lines.
  WAIT &H3DA, 8
  IF justStarted = 0 THEN
  FOR i = 0 TO numLines
       LINE (oldX(i, 0), oldY(i, 0))-(oldX(i, 1), oldY(i, 1)), 0
    NEXT i
  END IF
  ' Draw the new lines.
  FOR i = 0 TO numLines
     LINE (scrX(i, 0), scrY(i, 0))-(scrX(i, 1), scrY(i, 1)), 11
  NEXT i
 
  theta = (theta + thetaRot) MOD 360
  phi = (phi + phiRot) MOD 360

  justStarted = 0
 'DO
 'LOOP UNTIL INKEY$ <> ""
'  NEXT timtime
'LOOP UNTIL INKEY$ = CHR$(27)
LOOP UNTIL key$ = CHR$(27)

' Lines are stored in format (X1,Y1,Z1,p1)-(X2,Y2,Z2,p2)
DATA -50, 50, 50,1, 50, 50, 50,1
DATA  50,-50, 50,1, 50, 50, 50,1
DATA  50, 50,-50,1, 50, 50, 50,1
DATA -50,-50, 50,1,-50, 50, 50,1
DATA -50, 50,-50,1,-50, 50, 50,1
DATA -50,-50, 50,1, 50,-50, 50,1
DATA -50, 50,-50,1, 50, 50,-50,1
DATA -50,-50,-50,1, 50,-50,-50,1
DATA -50,-50,-50,1,-50, 50,-50,1
DATA  50,-50,-50,1, 50,-50, 50,1
DATA  50,-50,-50,1, 50, 50,-50,1
DATA -50,-50,-50,1,-50,-50, 50,1
DATA -50, 50, 50,1, 50, 50, 50,1
'   READ lO(i, 0).x, lO(i, 0).y, lO(i, 0).Z, lO(i, 0).p
'   READ lO(i, 1).x, lO(i, 1).y, lO(i, 1).Z, lO(i, 1).p
DATA -45,45,45,1,45,45,45,1
DATA 45,-45,45,1,45,45,45,1
DATA 45,45,-45,1,45,45,45,1
DATA -45,-45,45,1,-45,45,45,1
DATA -45,45,-45,1,-45,45,45,1
DATA -45,-45,45,1,45,-45,45,1
DATA -45,45,-45,1,45,45,-45,1
DATA -45,-45,-45,1,45,-45,-45,1
DATA -45,-45,-45,1,-45,45,-45,1
DATA 45,-45,-45,1,45,-45,45,1
DATA 45,-45,-45,1,45,45,-45,1
DATA -45,-45,-45,1,-45,-45,45,1

DATA -40,40,40,1,40,40,40,1
DATA 40,-40,40,1,40,40,40,1
DATA 40,40,-40,1,40,40,40,1
DATA -40,-40,40,1,-40,40,40,1
DATA -40,40,-40,1,-40,40,40,1
DATA -40,-40,40,1,40,-40,40,1
DATA -40,40,-40,1,40,40,-40,1
DATA -40,-40,-40,1,40,-40,-40,1
DATA -40,-40,-40,1,-40,40,-40,1
DATA 40,-40,-40,1,40,-40,40,1
DATA 40,-40,-40,1,40,40,-40,1
DATA -40,-40,-40,1,-40,-40,40,1

DATA -35,35,35,1,35,35,35,1
DATA 35,-35,35,1,35,35,35,1
DATA 35,35,-35,1,35,35,35,1
DATA -35,-35,35,1,-35,35,35,1
DATA -35,35,-35,1,-35,35,35,1
DATA -35,-35,35,1,35,-35,35,1
DATA -35,35,-35,1,35,35,-35,1
DATA -35,-35,-35,1,35,-35,-35,1
DATA -35,-35,-35,1,-35,35,-35,1
DATA 35,-35,-35,1,35,-35,35,1
DATA 35,-35,-35,1,35,35,-35,1
DATA -35,-35,-35,1,-35,-35,35,1


DATA -30,30,30,1,30,30,30,1
DATA 30,-30,30,1,30,30,30,1
DATA 30,30,-30,1,30,30,30,1
DATA -30,-30,30,1,-30,30,30,1
DATA -30,30,-30,1,-30,30,30,1
DATA -30,-30,30,1,30,-30,30,1
DATA -30,30,-30,1,30,30,-30,1
DATA -30,-30,-30,1,30,-30,-30,1
DATA -30,-30,-30,1,-30,30,-30,1
DATA 30,-30,-30,1,30,-30,30,1
DATA 30,-30,-30,1,30,30,-30,1
DATA -30,-30,-30,1,-30,-30,30,1

DATA -25,25,25,1,25,25,25,1
DATA 25,-25,25,1,25,25,25,1
DATA 25,25,-25,1,25,25,25,1
DATA -25,-25,25,1,-25,25,25,1
DATA -25,25,-25,1,-25,25,25,1
DATA -25,-25,25,1,25,-25,25,1
DATA -25,25,-25,1,25,25,-25,1
DATA -25,-25,-25,1,25,-25,-25,1
DATA -25,-25,-25,1,-25,25,-25,1
DATA 25,-25,-25,1,25,-25,25,1
DATA 25,-25,-25,1,25,25,-25,1
DATA -25,-25,-25,1,-25,-25,25,1

DATA -20,20,20,1,20,20,20,1
DATA 20,-20,20,1,20,20,20,1
DATA 20,20,-20,1,20,20,20,1
DATA -20,-20,20,1,-20,20,20,1
DATA -20,20,-20,1,-20,20,20,1
DATA -20,-20,20,1,20,-20,20,1
DATA -20,20,-20,1,20,20,-20,1
DATA -20,-20,-20,1,20,-20,-20,1
DATA -20,-20,-20,1,-20,20,-20,1
DATA 20,-20,-20,1,20,-20,20,1
DATA 20,-20,-20,1,20,20,-20,1
DATA -20,-20,-20,1,-20,-20,20,1

DATA -15,15,15,1,15,15,15,1
DATA 15,-15,15,1,15,15,15,1
DATA 15,15,-15,1,15,15,15,1
DATA -15,-15,15,1,-15,15,15,1
DATA -15,15,-15,1,-15,15,15,1
DATA -15,-15,15,1,15,-15,15,1
DATA -15,15,-15,1,15,15,-15,1
DATA -15,-15,-15,1,15,-15,-15,1
DATA -15,-15,-15,1,-15,15,-15,1
DATA 15,-15,-15,1,15,-15,15,1
DATA 15,-15,-15,1,15,15,-15,1
DATA -15,-15,-15,1,-15,-15,15,1


DATA -10,10,10,1,10,10,10,1
DATA 10,-10,10,1,10,10,10,1
DATA 10,10,-10,1,10,10,10,1
DATA -10,-10,10,1,-10,10,10,1
DATA -10,10,-10,1,-10,10,10,1
DATA -10,-10,10,1,10,-10,10,1
DATA -10,10,-10,1,10,10,-10,1
DATA -10,-10,-10,1,10,-10,-10,1
DATA -10,-10,-10,1,-10,10,-10,1
DATA 10,-10,-10,1,10,-10,10,1
DATA 10,-10,-10,1,10,10,-10,1
DATA -10,-10,-10,1,-10,-10,10,1

DATA -72,72,72,1,72,72,72,1
DATA 72,-72,72,1,72,72,72,1
DATA 72,72,-72,1,72,72,72,1
DATA -72,-72,72,1,-72,72,72,1
DATA -72,72,-72,1,-72,72,72,1
DATA -72,-72,72,1,72,-72,72,1
DATA -72,72,-72,1,72,72,-72,1
DATA -72,-72,-72,1,72,-72,-72,1
DATA -72,-72,-72,1,-72,72,-72,1
DATA 72,-72,-72,1,72,-72,72,1
DATA 72,-72,-72,1,72,72,-72,1
DATA -72,-72,-72,1,-72,-72,72,1
DATA -72,72,72,1,72,72,72,1

DATA -47,47,47,1,47,47,47,1
DATA 47,-47,47,1,47,47,47,1
DATA 47,47,-47,1,47,47,47,1
DATA -47,-47,47,1,-47,47,47,1
DATA -47,47,-47,1,-47,47,47,1
DATA -47,-47,47,1,47,-47,47,1
DATA -47,47,-47,1,47,47,-47,1
DATA -47,-47,-47,1,47,-47,-47,1
DATA -47,-47,-47,1,-47,47,-47,1
DATA 47,-47,-47,1,47,-47,47,1
DATA 47,-47,-47,1,47,47,-47,1
DATA -47,-47,-47,1,-47,-47,47,1

DATA -42,42,42,1,42,42,42,1
DATA 42,-42,42,1,42,42,42,1
DATA 42,42,-42,1,42,42,42,1
DATA -42,-42,42,1,-42,42,42,1
DATA -42,42,-42,1,-42,42,42,1
DATA -42,-42,42,1,42,-42,42,1
DATA -42,42,-42,1,42,42,-42,1
DATA -42,-42,-42,1,42,-42,-42,1
DATA -42,-42,-42,1,-42,42,-42,1
DATA 42,-42,-42,1,42,-42,42,1
DATA 42,-42,-42,1,42,42,-42,1
DATA -42,-42,-42,1,-42,-42,42,1

DATA -37,37,37,1,37,37,37,1
DATA 37,-37,37,1,37,37,37,1
DATA 37,37,-37,1,37,37,37,1
DATA -37,-37,37,1,-37,37,37,1
DATA -37,37,-37,1,-37,37,37,1
DATA -37,-37,37,1,37,-37,37,1
DATA -37,37,-37,1,37,37,-37,1
DATA -37,-37,-37,1,37,-37,-37,1
DATA -37,-37,-37,1,-37,37,-37,1
DATA 37,-37,-37,1,37,-37,37,1
DATA 37,-37,-37,1,37,37,-37,1
DATA -37,-37,-37,1,-37,-37,37,1


DATA -32,32,32,1,32,32,32,1
DATA 32,-32,32,1,32,32,32,1
DATA 32,32,-32,1,32,32,32,1
DATA -32,-32,32,1,-32,32,32,1
DATA -32,32,-32,1,-32,32,32,1
DATA -32,-32,32,1,32,-32,32,1
DATA -32,32,-32,1,32,32,-32,1
DATA -32,-32,-32,1,32,-32,-32,1
DATA -32,-32,-32,1,-32,32,-32,1
DATA 32,-32,-32,1,32,-32,32,1
DATA 32,-32,-32,1,32,32,-32,1
DATA -32,-32,-32,1,-32,-32,32,1

DATA -27,27,27,1,27,27,27,1
DATA 27,-27,27,1,27,27,27,1
DATA 27,27,-27,1,27,27,27,1
DATA -27,-27,27,1,-27,27,27,1
DATA -27,27,-27,1,-27,27,27,1
DATA -27,-27,27,1,27,-27,27,1
DATA -27,27,-27,1,27,27,-27,1
DATA -27,-27,-27,1,27,-27,-27,1
DATA -27,-27,-27,1,-27,27,-27,1
DATA 27,-27,-27,1,27,-27,27,1
DATA 27,-27,-27,1,27,27,-27,1
DATA -27,-27,-27,1,-27,-27,27,1

DATA -22,22,22,1,22,22,22,1
DATA 22,-22,22,1,22,22,22,1
DATA 22,22,-22,1,22,22,22,1
DATA -22,-22,22,1,-22,22,22,1
DATA -22,22,-22,1,-22,22,22,1
DATA -22,-22,22,1,22,-22,22,1
DATA -22,22,-22,1,22,22,-22,1
DATA -22,-22,-22,1,22,-22,-22,1
DATA -22,-22,-22,1,-22,22,-22,1
DATA 22,-22,-22,1,22,-22,22,1
DATA 22,-22,-22,1,22,22,-22,1
DATA -22,-22,-22,1,-22,-22,22,1

DATA -17,17,17,1,17,17,17,1
DATA 17,-17,17,1,17,17,17,1
DATA 17,17,-17,1,17,17,17,1
DATA -17,-17,17,1,-17,17,17,1
DATA -17,17,-17,1,-17,17,17,1
DATA -17,-17,17,1,17,-17,17,1
DATA -17,17,-17,1,17,17,-17,1
DATA -17,-17,-17,1,17,-17,-17,1
DATA -17,-17,-17,1,-17,17,-17,1
DATA 17,-17,-17,1,17,-17,17,1
DATA 17,-17,-17,1,17,17,-17,1
DATA -17,-17,-17,1,-17,-17,17,1


DATA -12,12,12,1,12,12,12,1
DATA 12,-12,12,1,12,12,12,1
DATA 12,12,-12,1,12,12,12,1
DATA -12,-12,12,1,-12,12,12,1
DATA -12,12,-12,1,-12,12,12,1
DATA -12,-12,12,1,12,-12,12,1
DATA -12,12,-12,1,12,12,-12,1
DATA -12,-12,-12,1,12,-12,-12,1
DATA -12,-12,-12,1,-12,12,-12,1
DATA 12,-12,-12,1,12,-12,12,1
DATA 12,-12,-12,1,12,12,-12,1
DATA -12,-12,-12,1,-12,-12,12,1


'DATA 7,-10,6,1,7,-10,2,1
'DATA 7,-10,6,1,7,-2,6,1
'DATA 7,-2,2,1,7,-2,6,1
'DATA 7,-2,2,1,7,-4,2,1
'DATA 7,-10,2,1,7,-8,2,1
'DATA 7,-8,2,1,7,-8,-6,1
'DATA 7,-8,-6,1,7,-4,-6,1
'DATA 7,-4,-6,1,7,-4,2,1

'DATA -7,-10,6,1,-7,-10,2,1
'DATA -7,-10,6,1,-7,-2,6,1
'DATA -7,-2,2,1,-7,-2,6,1
'DATA -7,-2,2,1,-7,-4,2,1
'DATA -7,-10,2,1,-7,-8,2,1
'DATA -7,-8,2,1,-7,-8,-6,1
'DATA -7,-8,-6,1,-7,-4,-6,1
'DATA -7,-4,-6,1,-7,-4,2,1
'
'DATA 7,-10,6,1,-7,-10,2,1
'DATA 7,-10,6,1,-7,-2,6,1
'DATA 7,-2,2,1,-7,-2,6,1
'DATA 7,-2,2,1,-7,-4,2,1
'DATA 7,-10,2,1,-7,-8,2,1
'DATA 7,-8,2,1,-7,-8,-6,1
'DATA 7,-8,-6,1,-7,-4,-6,1
'DATA 7,-4,-6,1,-7,-4,2,1

CLS
SCREEN 12
CLS
DIM picture(32001) AS INTEGER
picture(0) = 64
picture(1) = 16
FOR a = 2 TO 130
READ picture(a)
NEXT a
x% = 5
y% = 5
'DEF SEG = VARSEG(picture(0))
POKE VARPTR(picture(2)) + x% + 320& * y%, 15


'                BLOAD file$, VARPTR(picture(0))
                FOR tempx% = 0 TO picture(0) \ 8 - 1
                   FOR tempy% = 0 TO picture(1) - 1
                      temp% = PEEK(VARPTR(picture(2)) + tempx% + (picture(0) \ 8&) * tempy%)
                      IF temp% > 0 AND tempx% + x% >= 0 AND tempx% + x% < 320 AND tempy% + y% >= 0 AND tempy% + y% < 200 THEN
                         PSET (x% + tempx%, y% + tempy%), temp%
                      END IF
                   NEXT
                NEXT
                DEF SEG = &HA000
DATA 15,15,15,7,0,0,0,9
DATA 0,7,0,7,0,7,0,12
DATA 0,0,7,7,7,0,0,0
DATA 7,7,7,7,7,7,7,0
DATA 0,0,7,7,7,0,0,0
DATA 0,7,0,7,0,7,0,14
DATA 0,0,0,7,0,0,0,0
DATA 0,0,0,0,0,0,0,0,16

DATA 15,15,15,7,0,0,0,9
DATA 0,7,0,7,0,7,0,12
DATA 0,0,7,7,7,0,0,0
DATA 7,7,7,7,7,7,7,0
DATA 0,0,7,7,7,0,0,0
DATA 0,7,0,7,0,7,0,14
DATA 0,0,0,7,0,0,0,0
DATA 0,0,0,0,0,0,0,0,16



'ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
'Û            Code by Rioga   Internet e-mail: rioga@axis.org               Û
'Û                             Infovia e-mail: rioga@axis.inf               Û
'Û                                                                          Û
'Û Gargonscene n§1. Efecto 'Campo de estrellas horizontal'.                 Û
'Û MCMXCVII, Gargon Software                                                Û
'ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß

DEFINT I-J
CONST maxstars = 150

DECLARE SUB putpixel (x&, y&, c&)
DECLARE SUB cambiacolor (col%, r%, g%, b%)
DECLARE SUB wretrace ()

DIM SHARED x&(0 TO maxstars), y&(0 TO maxstars), z&(0 TO maxstars)

'Inicio del programa

SCREEN 13
RANDOMIZE TIMER

FOR i = 0 TO maxstars STEP 1
  x&(i) = INT(RND * 320)
  y&(i) = INT(RND * 200)
  z&(i) = INT(RND * 3) + 1
NEXT i

CALL cambiacolor(1, 25, 25, 25)
CALL cambiacolor(2, 40, 40, 40)
CALL cambiacolor(3, 63, 63, 63)

DO
  CALL wretrace
  FOR i = 0 TO maxstars STEP 1
    CALL putpixel(x&(i), y&(i), 0)
    x&(i) = x&(i) + z&(i)
    IF x&(i) > 320 THEN
      x&(i) = 0
      y&(i) = INT(RND * 200)
      z&(i) = INT(RND * 3) + 1
    END IF
    CALL putpixel(x&(i), y&(i), z&(i))
  NEXT i
LOOP WHILE INKEY$ = ""

SCREEN 0

'Cambiar paleta (M x. n£m. col. 256)
DEFSNG I-J
SUB cambiacolor (col%, r%, g%, b%)
OUT &H3C8, col%
OUT &H3C9, r%
OUT &H3C9, g%
OUT &H3C9, b%
END SUB

'Poner pixels en pantalla (Modo 13h)
SUB putpixel (x&, y&, c&)
DEF SEG = &HA000
POKE y& * 320 + x&, c&
END SUB

'Control del retrazado vertical (03DAh) bit 3
SUB wretrace
DO
LOOP UNTIL (INP(&H3DA) AND 8)
END SUB

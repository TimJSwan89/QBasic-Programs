
DIM Cube(1 TO 675)
SCREEN 1
' Draw a white box.
LINE (140, 25)-(140 + 100, 125), 3, B
' Draw the outline of a magenta cube inside the box.
'DRAW "C2 BM140,50 M+50,-25 M+50,25 M-50,25"
DRAW "M-50,-25 M+0,50 M+50,25 M+50,-25 M+0,-50 BM190,75 M+0,50"
' Save the drawing in the array Cube.
GET (140, 25)-(240, 125), Cube
' Set segment to the array Cube's segment and store the drawing
' in the file MAGCUBE.GRH. Note: 2700 is the number of bytes
' in Cube (4 bytes per array element * 675).
'DEF SEG = VARSEG(Cube(1))
'BSAVE "MAGCUBE.GRH", VARPTR(Cube(1)), 2700
'DEF SEG                 ' Restore default BASIC segment.
'*** Programming example using BLOAD ***
' You must create the file MAGCUBE.GRH using the BSAVE statement
' programming example before you can run this program. This program
' uses MAGCUBE.GRH.
'
'DIM Cube(1 TO 675)
' Set the screen mode. The mode should be the same as the
' mode used to create the original drawing.
'SCREEN 12
' Set segment to the array Cube's segment and load
' the graphic file into Cube.
DEF SEG = VARSEG(Cube(1))
BLOAD "MAGCUBE.GRH", VARPTR(Cube(1))
DEF SEG               ' Restore default BASIC segment.
' Put the drawing on the screen.
PUT (80, 10), Cube


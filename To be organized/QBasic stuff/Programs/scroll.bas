
DEFINT A-Z

'$DYNAMIC

DIM tile(1, 99)                         'Array for the two tiles
DIM map(399)                            'For the map

FOR j = 0 TO 1                          'Load the tiles
   FOR i = 0 TO 99
      READ tile(j, i)
   NEXT
NEXT

FOR i = 0 TO 399: READ map(i): NEXT     'Load the map

SCREEN 13
CLS

DEF SEG = &HA000                        'Point to the graphics buffer

hScroll = 0                             'These variables will keep track of
vScroll = 0                             'how far the screen has scrolled

PRINT "Use the number pad to scroll,"
PRINT "esc to quit."

DO
  verticalVal = vScroll

  tileX = hScroll \ 10                  'Calculate all original values
  tileY = verticalVal \ 10
  mapElem = tileY * 20 + tileX
 
  spriteX = hScroll MOD 10
  spriteY = vScroll MOD 10
  spriteElem = spriteY * 10 + spriteX
  
  WAIT &H3DA, 8                         'Wait for vertical retrace

  ' The two FOR..NEXT loops loop through the visible screen which is the box
  ' from (60, 110) to (139, 209).
  FOR screenY = 60 TO 139

     ' Inside the second loop, the offset will only be increased by 1 each
     ' time through, so it's pointless to keep on recalculating it.  If we
     ' calculate it once for each y loop and then just add 1 inside the x
     ' loop we can speed things up.
     offset& = screenY * 320& + 110

     FOR screenX = 110 TO 209

        ' Use the faster PSET.  The colour location of the pixel in the tile
        ' array has been precalculated, we get the tile number from the map
        ' and then take the specific pixel.  The offset is increased by 1
        ' each time through the loop to move horizontally across the screen.
        POKE offset&, tile(map(mapElem), spriteElem)
        offset& = offset& + 1

        ' Move over one pixel in the sprite, if we've moved on to the next
        ' tile then spriteElem will be a multiple of 10 (we move through
        ' pixels 0 to 9 and then when we hit 10, the width of the tile,
        ' we've moved on to the next tile).  So spriteElem is knocked back
        ' to the first pixel of the row and we move 1 forward along the map.
        spriteElem = spriteElem + 1
        IF spriteElem MOD 10 = 0 THEN
          spriteElem = spriteElem - 10
          mapElem = mapElem + 1
        END IF
     NEXT

     ' Recalculate the map element now that we've moved down one row in the
     ' sprite, and possibly down one row on the map.  VerticalVal keeps track
     ' of the vertical motion down the map.
     verticalVal = verticalVal + 1
     tileY = verticalVal \ 10
     mapElem = tileY * 20 + tileX
    
     ' Recalculate the sprite element now that we've moved down one row in
     ' the sprite.  If we've moved down to the next tile, knock the spriteY
     ' value back to the top of the tile.
     spriteY = spriteY + 1
     IF spriteY = 10 THEN spriteY = 0
     spriteElem = spriteY * 10 + spriteX
  NEXT

  ' Check for a key press and then clear the keyboard buffer.
  key$ = INKEY$
  DO: LOOP UNTIL INKEY$ = ""
  
  ' Adjust the hScroll and vScroll values to reflect how much the screen has
  ' been scrolled.
  SELECT CASE key$
         CASE "4"
             IF hScroll > 0 THEN hScroll = hScroll - 1
         CASE "6"
             IF hScroll < 100 THEN hScroll = hScroll + 1
         CASE "8"
             IF vScroll > 0 THEN vScroll = vScroll - 1
         CASE "2"
             IF vScroll < 120 THEN vScroll = vScroll + 1
         CASE "7"
             IF hScroll > 0 THEN hScroll = hScroll - 1
             IF vScroll > 0 THEN vScroll = vScroll - 1
         CASE "9"
             IF hScroll < 100 THEN hScroll = hScroll + 1
             IF vScroll > 0 THEN vScroll = vScroll - 1
         CASE "1"
             IF hScroll > 0 THEN hScroll = hScroll - 1
             IF vScroll < 120 THEN vScroll = vScroll + 1
         CASE "3"
             IF hScroll < 100 THEN hScroll = hScroll + 1
             IF vScroll < 120 THEN vScroll = vScroll + 1
  END SELECT
LOOP UNTIL key$ = CHR$(27)

' Tile 0
DATA 1,1,1,1,1,1,1,1,1,1
DATA 1,2,2,2,2,2,2,2,2,1
DATA 1,2,3,3,3,3,3,3,2,1
DATA 1,2,3,4,4,4,4,3,2,1
DATA 1,2,3,4,5,5,4,3,2,1
DATA 1,2,3,4,5,5,4,3,2,1
DATA 1,2,3,4,4,4,4,3,2,1
DATA 1,2,3,3,3,3,3,3,2,1
DATA 1,2,2,2,2,2,2,2,2,1
DATA 1,1,1,1,1,1,1,1,1,1

' Tile 1
DATA 1,1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,2,2,1,1,1,1
DATA 1,1,1,2,3,3,2,1,1,1
DATA 1,1,2,3,4,4,3,2,1,1
DATA 1,2,3,4,5,5,4,3,2,1
DATA 1,2,3,4,5,5,4,3,2,1
DATA 1,1,2,3,4,4,3,2,1,1
DATA 1,1,1,2,3,3,2,1,1,1
DATA 1,1,1,1,2,2,1,1,1,1
DATA 1,1,1,1,1,1,1,1,1,1

' Map
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0
DATA 0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,1,0,0,0,0
DATA 0,0,0,0,0,0,0,1,1,0,0,0,1,0,0,0,1,0,0,0
DATA 0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,1,0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0
DATA 0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0
DATA 0,0,0,0,1,0,1,0,0,0,1,0,0,0,1,0,0,1,0,0
DATA 0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1
DATA 0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,1,1,0,0,0
DATA 0,0,1,0,1,1,0,0,0,1,0,0,1,0,0,1,0,0,0,0
DATA 0,0,1,0,0,1,0,0,1,1,0,0,1,0,0,0,0,0,0,0
DATA 0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0
DATA 0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0
DATA 0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,1,0,0,0
DATA 0,0,0,1,0,0,1,0,0,0,1,0,1,0,1,1,0,0,1,0
DATA 0,0,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1
DATA 0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,1,0,1,0
DATA 0,1,0,0,0,1,1,0,1,0,0,1,0,0,0,1,1,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0

CLS
INPUT "Length of worms"; size
size = size + 1: 'has length of size after erase of last bubble
INPUT "Length of crawl"; crawl
INPUT "Number of worms"; numofworms
DIM xpast(numofworms, size)
DIM ypast(numofworms, size)
DIM lastdir(numofworms)
DIM dirlength(numofworms)
DIM head(numofworms)
DIM tail(numofworms)
DIM erasetail(numofworms)
DIM x(numofworms)
DIM y(numofworms)
CLS
RANDOMIZE TIMER
x = 320
y = 240
dir = 4    ' default direction
wall = 20
delayer = 3000 / numofworms ^ 2
SCREEN 12
'PRINT "Delay = "; INT(delayer)

FOR count = 1 TO crawl
FOR worm = 1 TO numofworms

  IF count = 1 THEN
    x(worm) = x
    y(worm) = y
  END IF
  CIRCLE (x(worm), y(worm)), 3, worm MOD 16
  xpast(worm, head(worm)) = x(worm)
  ypast(worm, head(worm)) = y(worm)
  head(worm) = head(worm) + 1
  IF head(worm) = size THEN
    head(worm) = 0
    erasetail(worm) = 1
  END IF
'FOR z = 1 TO delayer : NEXT

  IF erasetail(worm) = 1 THEN
    CIRCLE (xpast(worm, tail(worm)), ypast(worm, tail(worm))), 3, 0
    tail(worm) = tail(worm) + 1
    IF tail(worm) = size THEN
      tail(worm) = 0
    END IF
  END IF

'FOR z = 1 TO 50: temp = 7 / 3: NEXT

  'Calculate new direction - use if last bubble not past WALL
  IF dirlength(worm) = 0 THEN
    'diff = INT(RND * 3) - 1
    IF RND > .5 THEN
      diff = 1
    ELSE
      diff = -1
    END IF
    dir = lastdir(worm) + diff
    IF dir = -1 THEN
      dir = 7
    ELSEIF dir = 8 THEN
      dir = 0
    END IF
    dirlength(worm) = INT(RND * 3) + 1
  ELSE
    dirlength(worm) = dirlength(worm) - 1
    dir = lastdir(worm)
  END IF
  'If last bubble past WALL - force new DIR
  '-draws 1 bubble into wall, then will turn
 
  IF x(worm) < wall THEN
    IF lastdir(worm) = 3 THEN
      dir = 1
    ELSEIF lastdir(worm) = 5 THEN
      dir = 7
    ELSE
      dir = 0
    END IF
  ELSEIF x(worm) > 640 - wall THEN
    IF lastdir(worm) = 1 THEN
      dir = 3
    ELSEIF lastdir(worm) = 7 THEN
      dir = 5
    ELSE
      dir = 4
    END IF
  END IF
  IF y(worm) < wall THEN
    'Check upper left corner wall limit
    IF x(worm) < wall THEN
      dir = 7
    'Check upper right corner wall limit
    ELSEIF x(worm) > 640 - wall THEN
      dir = 5
    ELSE
      IF lastdir(worm) = 1 THEN
        dir = 7
      ELSEIF lastdir(worm) = 3 THEN
        dir = 5
      ELSE
        dir = 6
      END IF
    END IF
  ELSEIF y(worm) > 480 - wall THEN
    'Check lower left corner wall limit
    IF x(worm) < wall THEN
      dir = 1
    'Check lower right corner wall limit
    ELSEIF x(worm) > 640 - wall THEN
      dir = 3
    ELSE
      IF lastdir(worm) = 7 THEN
        dir = 1
      ELSEIF lastdir(worm) = 5 THEN
        dir = 3
      ELSE
        dir = 2
      END IF
    END IF
  END IF
 
  'Calculate new coordinate
  IF dir = 0 OR dir = 2 OR dir = 4 OR dir = 6 THEN
    dist = 6
  ELSE
    dist = 5
  END IF
  IF dir = 1 OR dir = 0 OR dir = 7 THEN
    xplus = dist
  ELSEIF dir = 3 OR dir = 4 OR dir = 5 THEN
    xplus = 0 - dist
  ELSE
    xplus = 0
  END IF
  IF dir = 1 OR dir = 2 OR dir = 3 THEN
    yplus = 0 - dist
  ELSEIF dir = 5 OR dir = 6 OR dir = 7 THEN
    yplus = dist
  ELSE
    yplus = 0
  END IF
  x(worm) = x(worm) + xplus
  y(worm) = y(worm) + yplus
  lastdir(worm) = dir
NEXT worm
NEXT count




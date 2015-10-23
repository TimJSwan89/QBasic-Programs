CLS
RANDOMIZE TIMER
x = 320
y = 240
dir = 4    ' default direction
wall = 50
dlength = 0
SCREEN 12
FOR count = 1 TO 1000
  CIRCLE (x, y), 3
  'Calculate new direction - use if last bubble not past WALL
  IF dlength = 0 THEN
    'diff = INT(RND * 3) - 1
    IF RND > .5 THEN
      diff = 1
    ELSE
      diff = -1
    END IF
    dir = dir + diff
    IF dir = -1 THEN
      dir = 7
    ELSEIF dir = 8 THEN
      dir = 0
    END IF
    dlength = INT(RND * 3) + 1
  ELSE
    dlength = dlength - 1
  END IF
  'If last bubble past WALL - force new DIR
  '-draws 1 bubble into wall, then will turn
 
  IF x < wall THEN
    IF lastdir = 3 THEN
      dir = 1
    ELSEIF lastdir = 5 THEN
      dir = 7
    ELSE
      dir = 0
    END IF
  ELSEIF x > 640 - wall THEN
    IF lastdir = 1 THEN
      dir = 3
    ELSEIF lastdir = 7 THEN
      dir = 5
    ELSE
      dir = 4
    END IF
  END IF
  IF y < wall THEN
    'Check upper left corner wall limit
    IF x < wall THEN
      dir = 7
    'Check upper right corner wall limit
    ELSEIF x > 640 - wall THEN
      dir = 5
    ELSE
      IF lastdir = 1 THEN
        dir = 7
      ELSEIF lastdir = 3 THEN
        dir = 5
      ELSE
        dir = 6
      END IF
    END IF
  ELSEIF y > 480 - wall THEN
    'Check lower left corner wall limit
    IF x < wall THEN
      dir = 1
    'Check lower right corner wall limit
    ELSEIF x > 640 - wall THEN
      dir = 3
    ELSE
      IF lastdir = 7 THEN
        dir = 1
      ELSEIF lastdir = 5 THEN
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
  x = x + xplus
  y = y + yplus
  lastdir = dir
NEXT count




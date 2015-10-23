DECLARE SUB DisplayScores (SammyPos%, JakePos%)
DECLARE SUB ScoreCenter (row%, text$)
DECLARE FUNCTION ScoreString$ (Score&)
' Nibbles   Copyright (C) Microsoft Corporation 1990
' v 2.8     This version by Robert Munster (no copyright would
'           be claimed for this voluntary work).

'Set default data type to integer for faster game play
DEFINT A-Z

'User-defined TYPEs
TYPE snakeBody
    row AS INTEGER
    col AS INTEGER
END TYPE

'This type defines the player's snake
TYPE snaketype
    head      AS INTEGER
    length    AS INTEGER
    row       AS INTEGER
    col       AS INTEGER
    direction AS INTEGER
    lives     AS INTEGER
    Score     AS LONG
    scolor    AS INTEGER
    alive     AS INTEGER
END TYPE

'This type is used to represent the playing screen in memory
'It is used to simulate graphics in text mode, and has some interesting,
'and slightly advanced methods to increasing the speed of operation.
'Instead of the normal 80x25 text graphics using chr$(219) "€", we will be
'using chr$(220)"‹" and chr$(223) "ﬂ" and chr$(219) "€" to mimic an 80x50
'pixel screen.
'Check out sub-programs SET and POINTISTHERE to see how this is implemented
'feel free to copy these (as well as arenaType and the DIM ARENA stmt and the
'initialization code in the DrawScreen subprogram) and use them in your own
'programs
TYPE arenaType
    realRow     AS INTEGER        'Maps the 80x50 point into the real 80x25
    acolor      AS INTEGER        'Stores the current color of the point
    sister      AS INTEGER        'Each char has 2 points in it.  .SISTER is
END TYPE                          '-1 if sister point is above, +1 if below

TYPE ScoreRecord
    Score AS LONG
    Player AS STRING * 24
END TYPE

'Sub Declarations
DECLARE SUB SpacePause (text$)
DECLARE SUB PrintScore (NumPlayers%, score1&, score2&, lives1%, lives2%)
DECLARE SUB Intro ()
DECLARE SUB GetInputs (NumPlayers, speed#, diff$, monitor$, SLevel, GameSpeed#, PlayMusic)
DECLARE SUB DrawScreen ()
DECLARE SUB PlayNibbles (NumPlayers, speed#, diff$, SLevel, GameSpeed#, CurLevel, sammyscore&, jakescore&, PlayMusic)
DECLARE SUB set (row, col, acolor)
DECLARE SUB Center (row, text$)
DECLARE SUB DoIntro ()
DECLARE SUB Initialize ()
DECLARE SUB SparklePause ()
DECLARE SUB Level (WhatToDO, Sammy() AS snaketype, SLevel)
DECLARE SUB InitColors ()
DECLARE SUB EraseSnake (snake() AS ANY, snakeBod() AS ANY, snakeNum%)
DECLARE FUNCTION StillWantsToPlay ()
DECLARE FUNCTION PointIsThere (row, col, backColor)
DECLARE FUNCTION GetPlayerName$ ()

'Constants
CONST TRUE = -1
CONST FALSE = NOT TRUE
CONST MAXSNAKELENGTH = 1000
CONST STARTOVER = 1             ' Parameters to 'Level' SUB
CONST SAMELEVEL = 2
CONST NEXTLEVEL = 3
CONST SCORECENTRE = 14
CONST SAMMYLINE = 5
CONST JAKELINE = 13
CONST HSTLEFT = 39
CONST HSTTOP = 1

'Global Variables
DIM SHARED arena(1 TO 50, 1 TO 80) AS arenaType
DIM SHARED CurLevel, colortable(10)
DIM SHARED HighScore AS ScoreRecord
                                 
'Local Variables
REM none

    OPEN "C:\NIBBLES.HST" FOR RANDOM AS #1 LEN = 28
    RANDOMIZE TIMER
    GOSUB ClearKeyLocks
    Intro
    GetInputs NumPlayers, speed#, diff$, monitor$, SLevel, GameSpeed#, PlayMusic
    GOSUB SetColors
    DrawScreen

    DO
        PlayNibbles NumPlayers, speed#, diff$, SLevel, GameSpeed#, CurLevel, sammyscore&, jakescore&, PlayMusic
        COLOR colortable(5), colortable(6)
        Center 10, "€ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ€"
        Center 11, "€       G A M E   O V E R       €"
        Center 12, "€                               €"
        Center 13, "€   Press any key to continue   €"
        Center 14, "€‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹€"
        WHILE INKEY$ <> "": WEND
        SammyPos = 1
        GET #1, 1, HighScore
        DO WHILE SammyPos <= LOF(1) \ 28 AND HighScore.Score > sammyscore&
            SammyPos = SammyPos + 1
            GET #1, SammyPos, HighScore
        LOOP
        SammyIns = SammyPos
        DO WHILE SammyIns <= LOF(1) \ 28 AND HighScore.Score = sammyscore&
            SammyIns = SammyIns + 1
            GET #1, SammyIns, HighScore
        LOOP
        Position = LOF(1) \ 28
        IF Position = 100 THEN Position = 99
        DO WHILE Position >= SammyIns
            GET #1, Position, HighScore
            PUT #1, Position + 1, HighScore
            Position = Position - 1
        LOOP
        IF SammyIns <= 100 THEN
            HighScore.Score = sammyscore&
            PUT #1, SammyIns, HighScore
        END IF
        IF NumPlayers = 2 THEN
            JakePos = 1
            GET #1, 1, HighScore
            DO WHILE JakePos <= LOF(1) \ 28 AND HighScore.Score > jakescore&
                JakePos = JakePos + 1
                GET #1, JakePos, HighScore
            LOOP
            JakeIns = JakePos
            DO WHILE JakeIns <= LOF(1) \ 28 AND HighScore.Score = jakescore&
                JakeIns = JakeIns + 1
                GET #1, JakeIns, HighScore
            LOOP
            Position = LOF(1) \ 28
            IF Position = 100 THEN Position = 99
            DO WHILE Position >= JakeIns
                GET #1, Position, HighScore
                PUT #1, Position + 1, HighScore
                Position = Position - 1
            LOOP
            IF SammyPos > JakePos THEN
                SammyPos = SammyPos + 1
                SammyIns = SammyIns + 1
            END IF
        END IF
        WHILE INKEY$ = "": WEND
        IF SammyIns <= 40 THEN
            COLOR colortable(1), colortable(9)
            CLS
            Center 10, "€ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ€"
            Center 11, "€  Sammy has achieved a high score!  €"
            Center 12, "€   Please enter his real name ...   €"
            Center 13, "€                                    €"
            Center 14, "€                                    €"
            Center 15, "€‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹€"
            HighScore.Player = GetPlayerName$
        ELSE
            HighScore.Player = ""
        END IF
        IF SammyIns <= 100 THEN
            HighScore.Score = sammyscore&
            PUT #1, SammyIns, HighScore
        END IF
        IF NumPlayers = 2 THEN
            IF JakeIns <= 40 THEN
                COLOR colortable(7), colortable(9)
                CLS
                Center 10, "€ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ€"
                Center 11, "€  Jake has achieved a high score!   €"
                Center 12, "€   Please enter his real name ...   €"
                Center 13, "€                                    €"
                Center 14, "€                                    €"
                Center 15, "€‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹€"
                HighScore.Player = GetPlayerName$
            ELSE
                HighScore.Player = ""
            END IF
            IF JakeIns <= 100 THEN
                HighScore.Score = jakescore&
                PUT #1, JakeIns, HighScore
            END IF
        END IF
        DisplayScores SammyIns, JakeIns
        COLOR colortable(5), colortable(9)
        LOCATE 25, 1
        PRINT "Press any key to continue";
        SLEEP
        CLS
    LOOP WHILE StillWantsToPlay

    GOSUB RestoreKeyLocks
    COLOR 15, 0
    CLS
    CLOSE #1
    SYSTEM

ClearKeyLocks:
    DEF SEG = 0                     ' Turn off CapLock, NumLock and ScrollLock
    KeyFlags = PEEK(1047)
    POKE 1047, &H20
    DEF SEG
    RETURN

RestoreKeyLocks:
    DEF SEG = 0                     ' Restore CapLock, NumLock and ScrollLock states
    POKE 1047, KeyFlags
    DEF SEG
    RETURN

SetColors:
    IF monitor$ = "M" THEN
        RESTORE mono
    ELSE
        RESTORE normal
    END IF

    FOR a = 1 TO 9
        READ colortable(a)
    NEXT a
    RETURN

       '                                Dialog    High Scores
       ' snake1 snake2 Walls Background Fore Back Jake Old Back
mono:   DATA 15,     7,    7,         0,  15,   0,  15,  7,   0
normal: DATA 14,    13,   12,         1,  15,   4,  13,  7,   0
END

'Center:
'  Centers text on given row
SUB Center (row, text$)
    LOCATE row, 41 - LEN(text$) / 2
    PRINT text$;
END SUB

SUB DisplayScores (SammyPos, JakePos)
    COLOR 15, 0
    CLS
    LOCATE 2, 1
    COLOR colortable(5), colortable(9)
    PRINT "High Scores"
    LOCATE 3, 1
    PRINT "---- ------"
    FOR Record% = 1 TO 20
        IF Record% <= LOF(1) \ 28 THEN
            IF Record% = SammyPos THEN
                COLOR colortable(1), colortable(9)
                LOCATE 4 + Record%, 1
                PRINT ">"
            ELSEIF Record% = JakePos THEN
                COLOR colortable(7), colortable(9)
                LOCATE 4 + Record%, 1
                PRINT ">"
            ELSE
                COLOR colortable(8), colortable(9)
            END IF
            GET #1, Record%, HighScore
            LOCATE 4 + Record%, 3
            PRINT HighScore.Player;
            LOCATE 4 + Record%, 29
            PRINT HighScore.Score;
        END IF
    NEXT Record%
    FOR Record% = 21 TO 40
        IF Record% <= LOF(1) \ 28 THEN
            IF Record% = SammyPos THEN
                COLOR colortable(1), colortable(9)
                LOCATE 4 + Record% - 20, 41
                PRINT ">"
            ELSEIF Record% = JakePos THEN
                COLOR colortable(7), colortable(9)
                LOCATE 4 + Record% - 20, 41
                PRINT ">"
            ELSE
                COLOR colortable(8), colortable(9)
            END IF
            GET #1, Record%, HighScore
            LOCATE 4 + Record% - 20, 43
            PRINT HighScore.Player;
            LOCATE 4 + Record% - 20, 69
            PRINT HighScore.Score;
        END IF
    NEXT Record%
END SUB

'DrawScreen:
'  Draws playing field
SUB DrawScreen

    'initialize screen
    VIEW PRINT
    COLOR colortable(1), colortable(4)
    CLS

    'Print title & message
    Center 1, "Nibbles!"
    Center 11, "Initializing Playing Field..."
    
    'Initialize arena array
    FOR row = 1 TO 50
        FOR col = 1 TO 80
            arena(row, col).realRow = INT((row + 1) / 2)
            arena(row, col).sister = (row MOD 2) * 2 - 1
        NEXT col
    NEXT row
END SUB

'EraseSnake:
'  Erases snake to facilitate moving through playing field
SUB EraseSnake (snake() AS snaketype, snakeBod() AS snakeBody, snakeNum)

    FOR c = 0 TO 9
        FOR b = snake(snakeNum).length - c TO 0 STEP -10
            tail = (snake(snakeNum).head + MAXSNAKELENGTH - b) MOD MAXSNAKELENGTH
            set snakeBod(tail, snakeNum).row, snakeBod(tail, snakeNum).col, colortable(4)
        NEXT b
    NEXT c
    
END SUB

'GetInputs:
'  Gets player inputs
SUB GetInputs (NumPlayers, speed#, diff$, monitor$, SLevel, GameSpeed#, PlayMusic)

    COLOR 7, 0
    CLS

    DO
        LOCATE 6, 47: PRINT SPACE$(34);
        LOCATE 6, 20
        INPUT "How many players (1 or 2)"; num$
    LOOP UNTIL VAL(num$) = 1 OR VAL(num$) = 2
    NumPlayers = VAL(num$)

    LOCATE 8, 21: PRINT "Skill level (1 to 10)"
    LOCATE 9, 22: PRINT "1  = Beginner"         '1
    LOCATE 10, 22: PRINT "4  = Novice"          '2
    LOCATE 11, 22: PRINT "7  = Expert"          '4
    LOCATE 12, 22: PRINT "10  = Twiddle Fingers"'6
    LOCATE 13, 15: PRINT "(Computer speed may affect your skill level)"
    DO
        LOCATE 8, 43: PRINT SPACE$(37);
        LOCATE 8, 42
        INPUT GameSpeed$
    LOOP UNTIL VAL(GameSpeed$) >= 1 AND VAL(GameSpeed$) <= 10
    IF VAL(GameSpeed$) = 1 THEN
        BEEP
        BEEP
        LOCATE 15, 10: PRINT "WARNING: The game speed you have chosen is VERY slow."
        LOCATE 16, 10: PRINT "Would you like to change your mind? Y or N                            "
        DO
            LOCATE 16, 53: PRINT SPACE$(28)
            LOCATE 16, 52
            INPUT changemind$
        LOOP UNTIL changemind$ = "y" OR changemind$ = "n" OR changemind$ = "Y" OR changemind$ = "N"
        LOCATE 15, 1: PRINT SPACE$(80)
        LOCATE 16, 1: PRINT SPACE$(80)
        IF changemind$ = "y" OR changemind$ = "Y" THEN
            DO
                LOCATE 8, 43: PRINT SPACE$(37);
                LOCATE 8, 42
                INPUT GameSpeed$
            LOOP UNTIL VAL(GameSpeed$) >= 1 AND VAL(GameSpeed$) <= 10
        END IF
    END IF
    GameSpeed# = VAL(GameSpeed$)
    speed# = 2 ^ (10 - (GameSpeed# - 1) / 2)

    DO
        LOCATE 15, 47: PRINT SPACE$(34);
        LOCATE 15, 20
        INPUT "Starting level (up to 41)"; SLevel$
    LOOP UNTIL VAL(SLevel$) >= 1 AND VAL(SLevel$) <= 50
    SLevel = VAL(SLevel$)

    DO
        LOCATE 17, 56: PRINT SPACE$(25);
        LOCATE 17, 15
        INPUT "Increase game speed during play (Y or N)"; diff$
        diff$ = UCASE$(diff$)
    LOOP UNTIL diff$ = "Y" OR diff$ = "N"

    DO
        LOCATE 19, 46: PRINT SPACE$(34);
        LOCATE 19, 17
        INPUT "Monochrome or color monitor (M or C)"; monitor$
        monitor$ = UCASE$(monitor$)
    LOOP UNTIL monitor$ = "M" OR monitor$ = "C"

    DO
        LOCATE 21, 49: PRINT SPACE$(31);
        LOCATE 21, 21
        INPUT "Play sound effects (Y or N)"; PlayMusic$
    LOOP UNTIL PlayMusic$ = "Y" OR PlayMusic$ = "y" OR PlayMusic$ = "N" OR PlayMusic$ = "n"
    IF PlayMusic$ = "Y" OR PlayMusic$ = "y" THEN PlayMusic = -1 ELSE PlayMusic = 0

    startTime# = TIMER                           ' Calculate speed of system
    FOR i# = 1 TO 100000: NEXT i#                ' and do some compensation
    stopTime# = TIMER
    speed# = speed# * 50 / (stopTime# - startTime#)

END SUB

FUNCTION GetPlayerName$
    LOCATE 14, 29
    TypedText$ = ""
    WHILE INKEY$ <> "": WEND
    DO
        DO
            NextKey$ = INKEY$
        LOOP WHILE NextKey$ = ""
        SELECT CASE NextKey$
        CASE CHR$(32) TO CHR$(255)
            IF LEN(TypedText$) < 24 THEN
                TypedText$ = TypedText$ + NextKey$
                PRINT NextKey$;
            ELSE
                BEEP
            END IF
        CASE CHR$(8)
            IF LEN(TypedText$) > 0 THEN
                TypedText$ = LEFT$(TypedText$, LEN(TypedText$) - 1)
                LOCATE , 29 + LEN(TypedText$)
                PRINT " ";
                LOCATE , 29 + LEN(TypedText$)
            ELSE
                BEEP
            END IF
        CASE CHR$(13)
            GetPlayerName$ = TypedText$
            EXIT FUNCTION
        CASE ELSE
            BEEP
        END SELECT
    LOOP
END FUNCTION

'InitColors:
'Initializes playing field colors
SUB InitColors
    
    FOR row = 1 TO 50
        FOR col = 1 TO 80
            arena(row, col).acolor = colortable(4)
        NEXT col
    NEXT row

    COLOR colortable(1), colortable(4)
    CLS
   
    'Set (turn on) pixels for screen border
    FOR col = 1 TO 80
        set 3, col, colortable(3)
        set 50, col, colortable(3)
    NEXT col

    FOR row = 4 TO 49
        set row, 1, colortable(3)
        set row, 80, colortable(3)
    NEXT row

END SUB

'Intro:
'  Displays game introduction
SUB Intro
    SCREEN 0
    WIDTH 80, 25
    COLOR 15, 0
    CLS

'                        Q B a s i c   N i b b l e s                 v. 2.7  `
'                                                                            `
' To run this game, press F5. To exit QBasic, press Alt, F, X.               `
'                                                                            `
' Version 2.0 and subsequent versions prepared by Robert Munster.            `
' To get help on a BASIC keyword, move the cursor to the keyword and press
' F1 or click the right mouse button.
'

    Center 3, "Q B a s i c   N i b b l e s"
    COLOR 7
    LOCATE 6, 8: PRINT "Nibbles is a  game for one  or two players.  Navigate your snakes"
    LOCATE 7, 8: PRINT "around the game  board trying  to eat up  numbers  while avoiding"
    LOCATE 8, 8: PRINT "running into walls or other snakes.  The more numbers you eat up,"
    LOCATE 9, 8: PRINT "the more points you gain and the longer your snake becomes.  Your"
    LOCATE 10, 8: PRINT "score also depends on your speed and the difficulty of the level."
    LOCATE 11, 8: PRINT "     Don't expect to be  able to  complete all  50  levels in one"
    LOCATE 12, 8: PRINT "session. But don't worry, you can choose which level to play when"
    LOCATE 13, 8: PRINT "you begin."
    LOCATE 14, 8: PRINT "     Note that at  higher levels,   play with two  snakes becomes"
    LOCATE 15, 8: PRINT "increasingly  unmanageable,  due to the  limited space available."
    LOCATE 17, 6: PRINT "Controls:-"
    LOCATE 18, 6: PRINT "Player  1  uses the main set of cursor keys,  while player 2 uses the"
    LOCATE 19, 6: PRINT "numeric keypad keys.  P pauses the game, and Esc stops the game there"
    LOCATE 20, 6: PRINT "and then (so don't press it by mistake!)."
    Center 24, "Press any key to continue"

    PLAY "MBT160O2L8CDEDCDL4ECC"
    SparklePause

END SUB

'Level:
'Sets game level
SUB Level (WhatToDO, Sammy() AS snaketype, SLevel) STATIC
    
    SELECT CASE (WhatToDO)

    CASE STARTOVER
        CurLevel = SLevel
    CASE NEXTLEVEL
        CurLevel = CurLevel + 1
    END SELECT

    Sammy(1).head = 1                       'Initialize Snakes
    Sammy(1).length = 2
    Sammy(1).alive = TRUE
    Sammy(2).head = 1
    Sammy(2).length = 2
    Sammy(2).alive = TRUE

    InitColors
    
    SELECT CASE CurLevel
   
    CASE 1
        Sammy(1).row = 25: Sammy(2).row = 25
        Sammy(1).col = 50: Sammy(2).col = 30
        Sammy(1).direction = 4: Sammy(2).direction = 3

    CASE 2
        FOR i = 20 TO 60
            set 25, i, colortable(3)
        NEXT i
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 60: Sammy(2).col = 20
        Sammy(1).direction = 3: Sammy(2).direction = 4

    CASE 3
        FOR i = 10 TO 40
            set i, 20, colortable(3)
            set i, 60, colortable(3)
        NEXT i
        Sammy(1).row = 25: Sammy(2).row = 25
        Sammy(1).col = 50: Sammy(2).col = 30
        Sammy(1).direction = 1: Sammy(2).direction = 2

    CASE 4
        FOR i = 14 TO 39
            set i, i + 15, colortable(3)
        NEXT i
        Sammy(1).row = 40: Sammy(2).row = 15
        Sammy(1).col = 75: Sammy(2).col = 5
        Sammy(1).direction = 1: Sammy(2).direction = 2

    CASE 5
        FOR i = 2 TO 36
            set 26, i, colortable(3)
        NEXT i
        FOR i = 44 TO 79
            set 26, i, colortable(3)
        NEXT i
        Sammy(1).row = 7: Sammy(2).row = 7
        Sammy(1).col = 70: Sammy(2).col = 10
        Sammy(1).direction = 3: Sammy(2).direction = 4

    CASE 6
        FOR i = 4 TO 30
            set i, 20, colortable(3)
            set 53 - i, 60, colortable(3)
        NEXT i
        FOR i = 2 TO 40
            set 38, i, colortable(3)
            set 15, 81 - i, colortable(3)
        NEXT i
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 60: Sammy(2).col = 20
        Sammy(1).direction = 3: Sammy(2).direction = 4

    CASE 7
        FOR i = 4 TO 30
            set i, 20, colortable(3)
            set 53 - i, 61, colortable(3)
        NEXT i
        FOR i = 2 TO 20
            set 34, i, colortable(3)
            set 19, 81 - i, colortable(3)
        NEXT i
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 60: Sammy(2).col = 20
        Sammy(1).direction = 3: Sammy(2).direction = 4
     
    CASE 8
        FOR i = 14 TO 38
            set i, 21, colortable(3)
            set i, 59, colortable(3)
        NEXT i
        FOR i = 24 TO 56
            set 11, i, colortable(3)
            set 41, i, colortable(3)
        NEXT i
        Sammy(1).row = 25: Sammy(2).row = 25
        Sammy(1).col = 50: Sammy(2).col = 30
        Sammy(1).direction = 1: Sammy(2).direction = 2
         
    CASE 9
        FOR i = 12 TO 40
            set i, 10, colortable(3)
            set i, 20, colortable(3)
            set i, 30, colortable(3)
            set i, 40, colortable(3)
            set i, 50, colortable(3)
            set i, 60, colortable(3)
            set i, 70, colortable(3)
        NEXT i
        Sammy(1).row = 10: Sammy(2).row = 10
        Sammy(1).col = 70: Sammy(2).col = 10
        Sammy(1).direction = 3: Sammy(2).direction = 4
      
    CASE 10
        FOR i = 4 TO 49
            IF i > 30 OR i < 23 THEN
                set i, 10, colortable(3)
                set i, 20, colortable(3)
                set i, 30, colortable(3)
                set i, 40, colortable(3)
                set i, 50, colortable(3)
                set i, 60, colortable(3)
                set i, 70, colortable(3)
            END IF
        NEXT i
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 65: Sammy(2).col = 15
        Sammy(1).direction = 2: Sammy(2).direction = 1

    CASE 11
        FOR i = 10 TO 70
            set 14, i, colortable(3)
            set 23, i, colortable(3)
            set 32, i, colortable(3)
            set 41, i, colortable(3)
        NEXT i
        FOR i = 1 TO 8
            set i + 14, 10, colortable(3)
            set i + 23, 70, colortable(3)
            set i + 32, 10, colortable(3)
            set i + 41, 70, colortable(3)
        NEXT i
        Sammy(1).row = 45: Sammy(2).row = 7
        Sammy(1).col = 75: Sammy(2).col = 5
        Sammy(1).direction = 1: Sammy(2).direction = 2

    CASE 12
        FOR i = 13 TO 39
            set i, 21, colortable(3)
            set i, 59, colortable(3)
        NEXT i
        FOR i = 23 TO 57
            set 11, i, colortable(3)
            set 41, i, colortable(3)
        NEXT i
        Sammy(1).row = 25: Sammy(2).row = 25
        Sammy(1).col = 50: Sammy(2).col = 30
        Sammy(1).direction = 1: Sammy(2).direction = 2

    CASE 13
        FOR i = 12 TO 40
            FOR j = 5 TO 77 STEP 4
                set i, j, colortable(3)
            NEXT j
        NEXT i
        Sammy(1).row = 5: Sammy(2).row = 5
        Sammy(1).col = 41: Sammy(2).col = 39
        Sammy(1).direction = 4: Sammy(2).direction = 4
     
    CASE 14
        FOR i = 4 TO 49 STEP 2
            set i, 40, colortable(3)
        NEXT i
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 65: Sammy(2).col = 15
        Sammy(1).direction = 2: Sammy(2).direction = 1

    CASE 15
        FOR i = 13 TO 39
            set i, 21, colortable(3)
            set i, 59, colortable(3)
        NEXT i
        FOR i = 20 TO 60
            set 11, i, colortable(3)
            set 41, i, colortable(3)
        NEXT i
        Sammy(1).row = 25: Sammy(2).row = 25
        Sammy(1).col = 50: Sammy(2).col = 30
        Sammy(1).direction = 1: Sammy(2).direction = 2

    CASE 16
        FOR i = 6 TO 47
            set i, i, colortable(3)
            set i, i + 28, colortable(3)
        NEXT i
        Sammy(1).row = 40: Sammy(2).row = 15
        Sammy(1).col = 75: Sammy(2).col = 5
        Sammy(1).direction = 1: Sammy(2).direction = 2

    CASE 17
        FOR i = 4 TO 49 STEP 2
            set i, 35, colortable(3)
            set i, 40, colortable(3)
            set i, 45, colortable(3)
        NEXT i
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 65: Sammy(2).col = 15
        Sammy(1).direction = 2: Sammy(2).direction = 1

    CASE 18
        FOR i = 4 TO 40
            set i, 10, colortable(3)
            set 53 - i, 20, colortable(3)
            set i, 30, colortable(3)
            set 53 - i, 40, colortable(3)
            set i, 50, colortable(3)
            set 53 - i, 60, colortable(3)
            set i, 70, colortable(3)
        NEXT i
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 65: Sammy(2).col = 15
        Sammy(1).direction = 2: Sammy(2).direction = 1
  
    CASE 19
        FOR i = 5 TO 49 STEP 2
            set i, 20, colortable(3)
            set i, 61, colortable(3)
        NEXT i
        FOR i = 4 TO 48 STEP 2
            FOR j = 32 TO 48
                set i, j, colortable(3)
            NEXT j
        NEXT i
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 65: Sammy(2).col = 15
        Sammy(1).direction = 2: Sammy(2).direction = 1
     
    CASE 20
        FOR i = 4 TO 9
            FOR j = 5 TO 37 STEP 4
                set i, j, colortable(3)
            NEXT j
            FOR j = 44 TO 76 STEP 4
                set i, j, colortable(3)
            NEXT j
        NEXT i
        FOR i = 12 TO 40
            FOR j = 5 TO 37 STEP 4
                set i, j, colortable(3)
            NEXT j
            FOR j = 44 TO 76 STEP 4
                set i, j, colortable(3)
            NEXT j
        NEXT i
        FOR i = 43 TO 49
            FOR j = 5 TO 37 STEP 4
                set i, j, colortable(3)
            NEXT j
            FOR j = 44 TO 76 STEP 4
                set i, j, colortable(3)
            NEXT j
        NEXT i
        Sammy(1).row = 4: Sammy(2).row = 4
        Sammy(1).col = 42: Sammy(2).col = 39
        Sammy(1).direction = 2: Sammy(2).direction = 2

    CASE 21
        FOR i = 1 TO 19
            set 12, i + 9, colortable(3)
            set 40, i + 9, colortable(3)
            set 12, 71 - i, colortable(3)
            set 40, 71 - i, colortable(3)
            set 16 + i, 10, colortable(3)
            set 16 + i, 70, colortable(3)
            set 16 + i, 15, colortable(3)
            set 16 + i, 65, colortable(3)
            set 17, 15 + i, colortable(3)
            set 35, 15 + i, colortable(3)
            set 17, 65 - i, colortable(3)
            set 35, 65 - i, colortable(3)
            set 22, 19 + i, colortable(3)
            set 22, 61 - i, colortable(3)
            set 26, 19 + i, colortable(3)
            set 26, 61 - i, colortable(3)
            set 30, 19 + i, colortable(3)
            set 30, 61 - i, colortable(3)
        NEXT i
        set 12, 29, colortable(3)
        set 40, 29, colortable(3)
        set 12, 51, colortable(3)
        set 40, 51, colortable(3)
        set 17, 35, colortable(3)
        set 35, 35, colortable(3)
        set 17, 45, colortable(3)
        set 35, 45, colortable(3)
        set 22, 39, colortable(3)
        set 22, 41, colortable(3)
        set 26, 39, colortable(3)
        set 26, 41, colortable(3)
        set 30, 39, colortable(3)
        set 30, 41, colortable(3)
        set 12, 30, colortable(3)
        set 40, 30, colortable(3)
        set 12, 50, colortable(3)
        set 40, 50, colortable(3)
        set 17, 36, colortable(3)
        set 35, 36, colortable(3)
        set 17, 44, colortable(3)
        set 35, 44, colortable(3)
        set 22, 40, colortable(3)
        set 26, 40, colortable(3)
        set 30, 40, colortable(3)
        set 12, 31, colortable(3)
        set 40, 31, colortable(3)
        set 12, 49, colortable(3)
        set 40, 49, colortable(3)
        set 17, 37, colortable(3)
        set 35, 37, colortable(3)
        set 17, 43, colortable(3)
        set 35, 43, colortable(3)
        FOR i = 23 TO 30
            set 12, i + 9, colortable(3)
            set 40, i + 9, colortable(3)
            set 12, 71 - i, colortable(3)
            set 40, 71 - i, colortable(3)
        NEXT i
        set 12, 40, colortable(3)
        set 40, 40, colortable(3)
        Sammy(1).row = 6: Sammy(2).row = 6
        Sammy(1).col = 77: Sammy(2).col = 4
        Sammy(1).direction = 3: Sammy(2).direction = 4

    CASE 22
        FOR i = 6 TO 47
            set i, i, colortable(3)
            set i, i + 14, colortable(3)
            set i, i + 28, colortable(3)
        NEXT i
        Sammy(1).row = 40: Sammy(2).row = 15
        Sammy(1).col = 75: Sammy(2).col = 5
        Sammy(1).direction = 1: Sammy(2).direction = 2

    CASE 23
        FOR i = 4 TO 48 STEP 2
            set i, 10, colortable(3)
            set i + 1, 20, colortable(3)
            set i, 30, colortable(3)
            set i + 1, 40, colortable(3)
            set i, 50, colortable(3)
            set i + 1, 60, colortable(3)
            set i, 70, colortable(3)
        NEXT i
        Sammy(1).row = 7: Sammy(2).row = 43
        Sammy(1).col = 65: Sammy(2).col = 15
        Sammy(1).direction = 2: Sammy(2).direction = 1

    CASE 24
        FOR i = 1 TO 13
            set 11, i + 11, colortable(3)
            set 29, i + 11, colortable(3)
            set 11, i + 33, colortable(3)
            set 29, i + 33, colortable(3)
            set 11, i + 55, colortable(3)
            set 29, i + 55, colortable(3)
            set i + 10, 26, colortable(3)
            set i + 28, 26, colortable(3)
            set i + 10, 48, colortable(3)
            set i + 28, 48, colortable(3)
            set i + 10, 70, colortable(3)
            set i + 28, 70, colortable(3)
            set 25, 27 - i, colortable(3)
            set 43, 27 - i, colortable(3)
            set 25, 49 - i, colortable(3)
            set 43, 49 - i, colortable(3)
            set 25, 71 - i, colortable(3)
            set 43, 71 - i, colortable(3)
            set 26 - i, 12, colortable(3)
            set 44 - i, 12, colortable(3)
            set 26 - i, 34, colortable(3)
            set 44 - i, 34, colortable(3)
            set 26 - i, 56, colortable(3)
            set 44 - i, 56, colortable(3)
            NEXT i
        Sammy(1).row = 6: Sammy(2).row = 47
        Sammy(1).col = 70: Sammy(2).col = 11
        Sammy(1).direction = 3: Sammy(2).direction = 4

    CASE 25
        FOR i = 1 TO 8
            set i + 6, i + 6, colortable(3)
            set i + 6, i + 13, colortable(3)
            set i + 6, i + 20, colortable(3)
            set i + 6, i + 27, colortable(3)
            set i + 6, i + 34, colortable(3)
            set i + 16, i + 16, colortable(3)
            set i + 16, i + 23, colortable(3)
            set i + 16, i + 30, colortable(3)
            set i + 16, i + 37, colortable(3)
            set i + 16, i + 44, colortable(3)
            set i + 26, i + 26, colortable(3)
            set i + 26, i + 33, colortable(3)
            set i + 26, i + 40, colortable(3)
            set i + 26, i + 47, colortable(3)
            set i + 26, i + 54, colortable(3)
            set i + 36, i + 36, colortable(3)
            set i + 36, i + 43, colortable(3)
            set i + 36, i + 50, colortable(3)
            set i + 36, i + 57, colortable(3)
            set i + 36, i + 64, colortable(3)
        NEXT i
        Sammy(1).row = 40: Sammy(2).row = 15
        Sammy(1).col = 75: Sammy(2).col = 5
        Sammy(1).direction = 1: Sammy(2).direction = 2

    CASE 26
        FOR j = 8 TO 68 STEP 10
            FOR i = 9 TO 41 STEP 4
                set i, j, colortable(3)
                set i + 1, j + 5, colortable(3)
            NEXT i
        NEXT j
        Sammy(1).row = 45: Sammy(2).row = 8
        Sammy(1).col = 76: Sammy(2).col = 5
        Sammy(1).direction = 1: Sammy(2).direction = 2

    CASE 27
        FOR i = 4 TO 44
            FOR j = 5 TO 77 STEP 4
                set i, j, colortable(3)
            NEXT j
        NEXT i
        Sammy(1).row = 47: Sammy(2).row = 47
        Sammy(1).col = 77: Sammy(2).col = 4
        Sammy(1).direction = 3: Sammy(2).direction = 4

    CASE 28
        FOR i = 1 TO 10
            FOR j = 11 TO 51 STEP 10
                set i + 3, j, colortable(3)
                set 50 - i, 81 - j, colortable(3)
            NEXT j
        NEXT i
        FOR i = 1 TO 10
            FOR j = 11 TO 51 STEP 10
                set i + 13, i + j, colortable(3)
                set 40 - i, 81 - i - j, colortable(3)
            NEXT j
        NEXT i
        Sammy(1).row = 10: Sammy(2).row = 42
        Sammy(1).col = 70: Sammy(2).col = 10
        Sammy(1).direction = 3: Sammy(2).direction = 4

    CASE 29
        FOR j = 5 TO 73 STEP 4
            FOR i = 9 TO 42 STEP 3
                set i, j, colortable(3)
                set i + 1, j + 2, colortable(3)
            NEXT i
        NEXT j
        Sammy(1).row = 48: Sammy(2).row = 5
        Sammy(1).col = 78: Sammy(2).col = 3
        Sammy(1).direction = 1: Sammy(2).direction = 2
   
    CASE 30
        FOR j = 19 TO 58 STEP 3
            FOR i = 1 TO j - 18
                set i + 3, j, colortable(3)
                set 50 - i, 77 - j, colortable(3)
            NEXT i
        NEXT j
        Sammy(1).row = 26: Sammy(2).row = 27
        Sammy(1).col = 70: Sammy(2).col = 10
        Sammy(1).direction = 2: Sammy(2).direction = 1

    CASE 31
        FOR j = 4 TO 76 STEP 4
            FOR i = 6 TO 46 STEP 4
                set i, j, colortable(3)
                set i + 1, j, colortable(3)
                set i, j + 1, colortable(3)
                set i + 1, j + 1, colortable(3)
            NEXT i
        NEXT j
        Sammy(1).row = 40: Sammy(2).row = 12
        Sammy(1).col = 75: Sammy(2).col = 6
        Sammy(1).direction = 1: Sammy(2).direction = 2

    CASE 32
        FOR j = 5 TO 15
            FOR i = 7 TO 47 STEP 8
                set i, j, colortable(3)
                set i - 2, j + 10, colortable(3)
                set i, j + 20, colortable(3)
                set i - 2, j + 30, colortable(3)
                set i, j + 40, colortable(3)
                set i - 2, j + 50, colortable(3)
                set i, j + 60, colortable(3)
            NEXT i
            set 49, j + 10, colortable(3)
            set 49, j + 30, colortable(3)
            set 49, j + 50, colortable(3)
        NEXT j
        FOR j = 2 TO 5
            FOR i = 5 TO 49 STEP 8
                set i, j, colortable(3)
            NEXT i
        NEXT j
        FOR j = 75 TO 79
            FOR i = 5 TO 49 STEP 8
                set i, j, colortable(3)
            NEXT i
        NEXT j
        Sammy(1).row = 4: Sammy(2).row = 4
        Sammy(1).col = 78: Sammy(2).col = 3
        Sammy(1).direction = 3: Sammy(2).direction = 4
   
    CASE 33
        FOR i = 1 TO 27
            set 50 - i, 57, colortable(3)
            set 43 - i, 37, colortable(3)
            set 33 - i, 49, colortable(3)
        NEXT i
        FOR i = 1 TO 6
            set i + 3, 13, colortable(3)
            set 3 + i, 53, colortable(3)
            set 50 - i, 13, colortable(3)
            set 10 - i, 33, colortable(3)
        NEXT i
        FOR i = 1 TO 14
            set i + 3, 5, colortable(3)
            set 47 - i, 69, colortable(3)
            set 50 - i, 17, colortable(3)
            set i + 21, 45, colortable(3)
            set i + 3, 41, colortable(3)
        NEXT i
        FOR i = 1 TO 24
            set 37 - i, 77, colortable(3)
        NEXT i
        FOR i = 1 TO 5
            set 44 + i, 37, colortable(3)
        NEXT i
        FOR i = 1 TO 2
            set i + 11, 13, colortable(3)
            set i + 25, 25, colortable(3)
        NEXT i
        FOR i = 1 TO 12
            set i + 37, 45, colortable(3)
            set 34 + i, 29, colortable(3)
            set 37 + i, 49, colortable(3)
        NEXT i
        FOR i = 1 TO 31
            set 37 - i, 73, colortable(3)
        NEXT i
        FOR i = 1 TO 10
            set i + 39, 9, colortable(3)
            set 14 - i, 37, colortable(3)
        NEXT i
        FOR i = 1 TO 30
            set i + 19, 5, colortable(3)
            set i + 3, 17, colortable(3)
        NEXT i
        FOR i = 1 TO 20
            set 29 + i, 53, colortable(3)
            set i + 3, 25, colortable(3)
            set 27 + i, 65, colortable(3)
        NEXT i
        set 49, 29, colortable(3)
        set 49, 69, colortable(3)
        set 35, 49, colortable(3)
        FOR i = 1 TO 7
            set 26 - i, 65, colortable(3)
            set 34 + i, 13, colortable(3)
            set 42 + i, 33, colortable(3)
            set 11 - i, 69, colortable(3)
            set i + 3, 77, colortable(3)
        NEXT i
        FOR i = 1 TO 16
            set 28 - i, 53, colortable(3)
            set i + 3, 45, colortable(3)
            set i + 3, 61, colortable(3)
        NEXT i
        FOR i = 1 TO 21
            set i + 3, 9, colortable(3)
        NEXT i
        FOR i = 1 TO 29
            set 33 - i, 29, colortable(3)
            set 11 + i, 33, colortable(3)
        NEXT i
        FOR i = 1 TO 17
            set i + 15, 13, colortable(3)
            set 21 - i, 57, colortable(3)
        NEXT i
        FOR i = 1 TO 28
            set 50 - i, 61, colortable(3)
            set i + 19, 41, colortable(3)
        NEXT i
        FOR i = 1 TO 18
            set 48 - i, 25, colortable(3)
            set 31 - i, 69, colortable(3)
        NEXT i
        FOR i = 1 TO 13
            set 17 - i, 65, colortable(3)
        NEXT i
        FOR i = 1 TO 22
            set i + 3, 21, colortable(3)
            set i + 27, 21, colortable(3)
        NEXT i
        FOR i = 1 TO 11
            set 50 - i, 77, colortable(3)
            set 38 - i, 9, colortable(3)
            set 38 + i, 73, colortable(3)
            set 38 + i, 73, colortable(3)
        NEXT i
        Sammy(1).row = 20: Sammy(2).row = 40
        Sammy(1).col = 66: Sammy(2).col = 16
        Sammy(1).direction = 2: Sammy(2).direction = 1

    CASE 34
        FOR j = 3 TO 75 STEP 3
            FOR i = 5 TO 47 STEP 3
                set i, j, colortable(3)
                set i + 1, j, colortable(3)
                set i, j + 1, colortable(3)
                set i + 1, j + 1, colortable(3)
            NEXT i
        NEXT j
        Sammy(1).row = 40: Sammy(2).row = 12
        Sammy(1).col = 74: Sammy(2).col = 5
        Sammy(1).direction = 1: Sammy(2).direction = 2

    CASE 35
        FOR i = 1 TO 17
            set 8, i + 6, colortable(3)
            set i + 7, 25, colortable(3)
            set 26, 26 - i, colortable(3)
            set 27 - i, 7, colortable(3)
        NEXT i
        FOR i = 8 TO 26 STEP 2
            FOR j = 31 TO 49 STEP 2
                set i, j, colortable(3)
            NEXT j
        NEXT i
        FOR i = 1 TO 19
            set 27 - i, 54 + i, colortable(3)
        NEXT i
        FOR i = 1 TO 13
            set 21 - i, 54 + i, colortable(3)
            set 27 - i, 60 + i, colortable(3)
        NEXT i
        FOR i = 1 TO 7
            set 15 - i, 54 + i, colortable(3)
            set 27 - i, 66 + i, colortable(3)
        NEXT i
        set 8, 55, colortable(3)
        set 26, 73, colortable(3)
        FOR i = 7 TO 25
            set 31, i, colortable(3)
            set 45, i, colortable(3)
        NEXT i
        FOR i = 34 TO 42
            set i, 7, colortable(3)
            set i, 25, colortable(3)
            set i, 10, colortable(3)
            set i, 22, colortable(3)
        NEXT i
        FOR i = 1 TO 4
            set 34, 10 + i, colortable(3)
            set 34, 22 - i, colortable(3)
            set 42, 10 + i, colortable(3)
            set 42, 22 - i, colortable(3)
        NEXT i
        FOR i = 13 TO 19
            set 37, i, colortable(3)
            set 39, i, colortable(3)
        NEXT i
        FOR i = 1 TO 43
            set 31, 30 + i, colortable(3)
            set 45, 74 - i, colortable(3)
        NEXT i
        FOR i = 1 TO 11
            set 31 + i, 73, colortable(3)
            set 45 - i, 31, colortable(3)
        NEXT i
        FOR i = 1 TO 39
            set 42, 73 - i, colortable(3)
            set 34, 31 + i, colortable(3)
        NEXT i
        FOR i = 1 TO 5
            set 42 - i, 34, colortable(3)
            set 34 + i, 70, colortable(3)
        NEXT i
        FOR i = 1 TO 31
            set 37, 34 + i, colortable(3)
            set 39, 70 - i, colortable(3)
        NEXT i
        Sammy(1).row = 5: Sammy(2).row = 5
        Sammy(1).col = 65: Sammy(2).col = 16
        Sammy(1).direction = 3: Sammy(2).direction = 4

    CASE 36
        FOR j = 6 TO 74 STEP 2
            FOR i = 8 TO 44 STEP 6
                set i, j, colortable(3)
                set i + 1, j, colortable(3)
            NEXT i
            FOR i = 5 TO 47 STEP 6
                set i, j + 1, colortable(3)
                set i + 1, j + 1, colortable(3)
            NEXT i
        NEXT j
        Sammy(1).row = 48: Sammy(2).row = 5
        Sammy(1).col = 78: Sammy(2).col = 3
        Sammy(1).direction = 1: Sammy(2).direction = 2

    CASE 37
        FOR i = 7 TO 19
            FOR j = 10 TO 70 STEP 10
                set i, j, colortable(3)
            NEXT j
        NEXT i
        FOR i = 21 TO 31
            FOR j = 10 TO 70 STEP 10
                set i, j, colortable(3)
            NEXT j
        NEXT i
        FOR i = 33 TO 45
            FOR j = 10 TO 70 STEP 10
                set i, j, colortable(3)
            NEXT j
        NEXT i
        FOR i = 1 TO 5
            FOR j = 7 TO 43 STEP 4
                FOR k = 10 TO 60 STEP 10
                    set j, i + k, colortable(3)
                    set j + 2, 80 - (i + k), colortable(3)
                NEXT k
            NEXT j
        NEXT i
        Sammy(1).row = 48: Sammy(2).row = 5
        Sammy(1).col = 70: Sammy(2).col = 11
        Sammy(1).direction = 3: Sammy(2).direction = 4

    CASE 38
        FOR j = 1 TO 38
            FOR i = 6 TO 45 STEP 3
                set i, j, colortable(3)
                set i, 80 - j, colortable(3)
            NEXT i
        NEXT j
        FOR i = 7 TO 43 STEP 3
            set i, 40, colortable(3)
        NEXT i
        FOR i = 8 TO 44 STEP 3
            set i, 40, colortable(3)
        NEXT i
        Sammy(1).row = 48: Sammy(2).row = 48
        Sammy(1).col = 50: Sammy(2).row = 30
        Sammy(1).direction = 3: Sammy(2).direction = 4

    CASE 39
        set 13, 10, colortable(3)
        set 40, 10, colortable(3)
        FOR i = 10 TO 71
            set 12, i, colortable(3)
            set 41, i, colortable(3)
        NEXT i
        set 13, 71, colortable(3)
        set 40, 71, colortable(3)
        FOR i = 14 TO 39
            set i, 12, colortable(3)
            set i, 69, colortable(3)
        NEXT i
        set 21, 20, colortable(3)
        set 32, 20, colortable(3)
        FOR i = 20 TO 61
            set 20, i, colortable(3)
            set 33, i, colortable(3)
        NEXT i
        set 21, 61, colortable(3)
        set 32, 61, colortable(3)
        FOR i = 22 TO 31
            set i, 22, colortable(3)
            set i, 59, colortable(3)
        NEXT i
        FOR i = 30 TO 51
            set 25, i, colortable(3)
            set 28, i, colortable(3)
        NEXT i
        set 27, 30, colortable(3)
        set 26, 39, colortable(3)
        set 27, 42, colortable(3)
        set 26, 51, colortable(3)
        Sammy(1).row = 26: Sammy(2).row = 27
        Sammy(1).col = 40: Sammy(2).col = 41
        Sammy(1).direction = 4: Sammy(2).direction = 3
   
    CASE 40
        FOR i = 5 TO 25
            FOR j = 3 TO 79 STEP 2
                set i, j, colortable(3)
            NEXT j
        NEXT i
        FOR i = 27 TO 48
            FOR j = 2 TO 78 STEP 2
                set i, j, colortable(3)
            NEXT j
        NEXT i
        set 4, 79, colortable(3)
        set 49, 2, colortable(3)
        Sammy(1).row = 4: Sammy(2).row = 49
        Sammy(1).col = 78: Sammy(2).col = 3
        Sammy(1).direction = 3: Sammy(2).direction = 4

    CASE 41
        FOR i = 12 TO 32 STEP 20
            FOR j = 9 TO 53 STEP 22
                set i + 14, j + 11, colortable(3)
                set i + 14, j + 10, colortable(3)
                set i + 14, j + 9, colortable(3)
                set i + 13, j + 9, colortable(3)
                set i + 12, j + 9, colortable(3)
                set i + 11, j + 9, colortable(3)
                FOR k = 9 TO 0 STEP -1
                    set i + 11, j + k, colortable(3)
                NEXT k
                FOR k = 10 TO -4 STEP -1
                    set i + k, j, colortable(3)
                NEXT k
                FOR k = 1 TO 19
                    set i - 4, j + k, colortable(3)
                NEXT k
                FOR k = -3 TO 11
                    set i + k, j + 19, colortable(3)
                NEXT k
                FOR k = 18 TO 11 STEP -1
                    set i + 11, j + k, colortable(3)
                NEXT k
                set i + 12, j + 11, colortable(3)
                set i + 12, j + 12, colortable(3)
                set i + 12, j + 13, colortable(3)
                set i + 13, j + 13, colortable(3)
                set i + 14, j + 13, colortable(3)
                set i + 4, j + 9, colortable(3)
                set i + 2, j + 9, colortable(3)
                set i + 2, j + 11, colortable(3)
                set i + 4, j + 11, colortable(3)
            NEXT j
        NEXT i
        Sammy(1).row = 5: Sammy(2).row = 5
        Sammy(1).col = 65: Sammy(2).col = 16
        Sammy(1).direction = 4: Sammy(2).direction = 3

    CASE 42
        Sammy(1).row = 25: Sammy(2).row = 25
        Sammy(1).col = 50: Sammy(2).col = 30
        Sammy(1).direction = 4: Sammy(2).direction = 3

    CASE ELSE
        COLOR 15, 0
        CLS
        END
   
    END SELECT
END SUB

'PlayNibbles:
'  Main routine that controls game play
SUB PlayNibbles (NumPlayers, speed#, diff$, SLevel, GameSpeed#, CurLevel, sammyscore&, jakescore&, PlayMusic)

    'Initialize Snakes
    DIM sammyBody(MAXSNAKELENGTH - 1, 1 TO 2) AS snakeBody
    DIM Sammy(1 TO 2) AS snaketype
    Sammy(1).lives = 10
    Sammy(1).Score = 0
    Sammy(1).scolor = colortable(1)
    Sammy(2).lives = 10
    Sammy(2).Score = 0
    Sammy(2).scolor = colortable(2)
                 
    Level STARTOVER, Sammy(), SLevel
    startRow1 = Sammy(1).row: startCol1 = Sammy(1).col
    startRow2 = Sammy(2).row: startCol2 = Sammy(2).col

    curspeed# = speed#

    'play Nibbles until finished

    SpacePause "     Level" + STR$(CurLevel) + ",  Push Space"
    gameOver = FALSE
    DO
        IF NumPlayers = 1 THEN
            Sammy(2).row = 0
        END IF

        number = 1          'Current number that snakes are trying to run into
        nonum = TRUE        'nonum = TRUE if a number is not on the screen

        PlayerDied = FALSE
        PrintScore NumPlayers, Sammy(1).Score, Sammy(2).Score, Sammy(1).lives, Sammy(2).lives
        IF PlayMusic THEN PLAY "T160O2L20CDEDCDL10ECC"

        DO
            'Print number if no number exists
            IF nonum = TRUE THEN
                DO
                    numberRow = INT(RND(1) * 47 + 3)
                    NumberCol = INT(RND(1) * 78 + 2)
                    sisterRow = numberRow + arena(numberRow, NumberCol).sister
                LOOP UNTIL NOT PointIsThere(numberRow, NumberCol, colortable(4)) AND NOT PointIsThere(sisterRow, NumberCol, colortable(4))
                numberRow = arena(numberRow, NumberCol).realRow
                nonum = FALSE
                COLOR colortable(1), colortable(4)
                LOCATE numberRow, NumberCol
                PRINT RIGHT$(STR$(number), 1);
                count = 0
            END IF

            'Delay game
            FOR a# = 1 TO curspeed#:    NEXT a#

            'Get keyboard input & change direction accordingly
            kbd1$ = NextKey$
            DO
                NextKey$ = INKEY$
            LOOP WHILE NextKey$ = kbd1$ AND NextKey$ <> ""
            kbd2$ = NextKey$
            DO
                NextKey$ = INKEY$
            LOOP WHILE NextKey$ = kbd2$ AND NextKey$ <> ""
            SELECT CASE kbd1$
                CASE "8": IF Sammy(2).direction <> 2 THEN Sammy(2).direction = 1
                CASE "5": IF Sammy(2).direction <> 1 THEN Sammy(2).direction = 2
                CASE "4": IF Sammy(2).direction <> 4 THEN Sammy(2).direction = 3
                CASE "6": IF Sammy(2).direction <> 3 THEN Sammy(2).direction = 4
                CASE CHR$(0) + "H": IF Sammy(1).direction <> 2 THEN Sammy(1).direction = 1
                CASE CHR$(0) + "P": IF Sammy(1).direction <> 1 THEN Sammy(1).direction = 2
                CASE CHR$(0) + "K": IF Sammy(1).direction <> 4 THEN Sammy(1).direction = 3
                CASE CHR$(0) + "M": IF Sammy(1).direction <> 3 THEN Sammy(1).direction = 4
                CASE "p", "P": SpacePause " Game Paused ... Push Space  "
                CASE CHR$(27)
                    COLOR 15, 0
                    CLS
                    SYSTEM
                CASE ELSE
            END SELECT
            SELECT CASE kbd2$
                CASE "8": IF Sammy(2).direction <> 2 THEN Sammy(2).direction = 1
                CASE "5": IF Sammy(2).direction <> 1 THEN Sammy(2).direction = 2
                CASE "4": IF Sammy(2).direction <> 4 THEN Sammy(2).direction = 3
                CASE "6": IF Sammy(2).direction <> 3 THEN Sammy(2).direction = 4
                CASE CHR$(0) + "H": IF Sammy(1).direction <> 2 THEN Sammy(1).direction = 1
                CASE CHR$(0) + "P": IF Sammy(1).direction <> 1 THEN Sammy(1).direction = 2
                CASE CHR$(0) + "K": IF Sammy(1).direction <> 4 THEN Sammy(1).direction = 3
                CASE CHR$(0) + "M": IF Sammy(1).direction <> 3 THEN Sammy(1).direction = 4
                CASE "p", "P": SpacePause " Game Paused ... Push Space  "
                CASE ELSE
            END SELECT

            FOR a = 1 TO NumPlayers
                'Move Snake
                SELECT CASE Sammy(a).direction
                    CASE 1: Sammy(a).row = Sammy(a).row - 1
                    CASE 2: Sammy(a).row = Sammy(a).row + 1
                    CASE 3: Sammy(a).col = Sammy(a).col - 1
                    CASE 4: Sammy(a).col = Sammy(a).col + 1
                END SELECT
                Sammy(a).Score = Sammy(a).Score - GameSpeed#
                PrintScore NumPlayers, Sammy(1).Score, Sammy(2).Score, Sammy(1).lives, Sammy(2).lives

                'If snake hits number, respond accordingly
                IF numberRow = INT((Sammy(a).row + 1) / 2) AND NumberCol = Sammy(a).col THEN
                    IF PlayMusic THEN PLAY "MBO2L16CCCE"
                    IF Sammy(a).length < (MAXSNAKELENGTH - 30) THEN
                        Sammy(a).length = Sammy(a).length + number * 4
                    END IF
                    Sammy(a).Score = Sammy(a).Score + (number + 5) * NumPlayers * GameSpeed# ^ 2 * CurLevel
                    PrintScore NumPlayers, Sammy(1).Score, Sammy(2).Score, Sammy(1).lives, Sammy(2).lives
                    number = number + 1
                    IF number = 10 THEN
                        EraseSnake Sammy(), sammyBody(), 1
                        EraseSnake Sammy(), sammyBody(), 2
                        LOCATE numberRow, NumberCol: PRINT " "
                        Level NEXTLEVEL, Sammy(), SLevel
                        PrintScore NumPlayers, Sammy(1).Score, Sammy(2).Score, Sammy(1).lives, Sammy(2).lives
                        SpacePause "     Level" + STR$(CurLevel) + ",  Push Space"
                        IF NumPlayers = 1 THEN Sammy(2).row = 0
                        number = 1
                        IF diff$ = "P" THEN speed# = speed# - 10: curspeed# = speed#
                    END IF
                    nonum = TRUE
                    IF curspeed# < 1 THEN curspeed# = 1
                END IF
            NEXT a

            FOR a = 1 TO NumPlayers
                'If player runs into any point, or the head of the other snake, it dies.
                IF PointIsThere(Sammy(a).row, Sammy(a).col, colortable(4)) OR (Sammy(1).row = Sammy(2).row AND Sammy(1).col = Sammy(2).col) THEN
                    IF PlayMusic THEN PLAY "mbo1l64EeFfGgEeFfDdCc"
                    COLOR , colortable(4)
                    LOCATE numberRow, NumberCol
                    PRINT " "
                   
                    PlayerDied = TRUE
                    Sammy(a).alive = FALSE
                    Sammy(a).lives = Sammy(a).lives - 1

                'Otherwise, move the snake, and erase the tail
                ELSE
                    Sammy(a).head = (Sammy(a).head + 1) MOD MAXSNAKELENGTH
                    sammyBody(Sammy(a).head, a).row = Sammy(a).row
                    sammyBody(Sammy(a).head, a).col = Sammy(a).col
                    tail = (Sammy(a).head + MAXSNAKELENGTH - Sammy(a).length) MOD MAXSNAKELENGTH
                    set sammyBody(tail, a).row, sammyBody(tail, a).col, colortable(4)
                    sammyBody(tail, a).row = 0
                    set Sammy(a).row, Sammy(a).col, Sammy(a).scolor
                END IF
            NEXT a

        LOOP UNTIL PlayerDied

        curspeed# = speed#                ' reset speed to initial value
       
        FOR a = 1 TO NumPlayers
            EraseSnake Sammy(), sammyBody(), a

            'If dead, then erase snake in really cool way
            IF Sammy(a).alive = FALSE THEN
                'Update score
                Sammy(a).Score = Sammy(a).Score - 100000 / GameSpeed#
                PrintScore NumPlayers, Sammy(1).Score, Sammy(2).Score, Sammy(1).lives, Sammy(2).lives
                
                IF a = 1 THEN
                    SpacePause " Sammy Dies! Push Space! --->"
                ELSE
                    SpacePause " <---- Jake Dies! Push Space "
                END IF
            END IF
        NEXT a

        Level SAMELEVEL, Sammy(), SLevel
        PrintScore NumPlayers, Sammy(1).Score, Sammy(2).Score, Sammy(1).lives, Sammy(2).lives
     
    'Play next round, until either of snake's lives have run out.
    LOOP UNTIL Sammy(1).lives = 0 OR Sammy(2).lives = 0

    sammyscore& = Sammy(1).Score
    jakescore& = Sammy(2).Score

END SUB

'PointIsThere:
'  Checks the global  arena array to see if the boolean flag is set
FUNCTION PointIsThere (row, col, acolor)
    IF row <> 0 THEN
        IF arena(row, col).acolor <> acolor THEN
            PointIsThere = TRUE
        ELSE
            PointIsThere = FALSE
        END IF
    END IF
END FUNCTION

'PrintScore:
'  Prints players scores and number of lives remaining
SUB PrintScore (NumPlayers, score1&, score2&, lives1, lives2)
    COLOR 15, colortable(4)

    IF NumPlayers = 2 THEN
        COLOR colortable(2), colortable(4)
        LOCATE 1, 1
        PRINT USING "#,###,###,###  Lives: ##  <--JAKE"; score2&; lives2
    END IF

    COLOR colortable(1), colortable(4)
    LOCATE 1, 46
    PRINT USING "SAMMY-->  Lives: ##  #,###,###,###"; lives1; score1&
END SUB

SUB ScoreCenter (row, text$)
    LOCATE row, SCORECENTRE + 1 - LEN(text$) \ 2
    PRINT text$;
END SUB

FUNCTION ScoreString$ (Score&)
    Negative = (Score& < 0)
    IF Negative THEN Score& = -Score&
    Digits$ = LTRIM$(STR$(Score&))
    Segments$ = ""
    FOR group = 0 TO (LEN(Digits$) - 1) \ 3 - 1
        Segments$ = "," + MID$(Digits$, LEN(Digits$) - 2 - group * 3, 3) + Segments$
    NEXT group
    Segments$ = LEFT$(Digits$, (LEN(Digits$) - 1) MOD 3 + 1) + Segments$
    IF Negative THEN Segments$ = "-" + Segments$
    ScoreString$ = Segments$
END FUNCTION

'Set:
'  Sets row and column on playing field to given color to facilitate moving
'  of snakes around the field.
SUB set (row, col, acolor)
FOR p% = 1 TO 2000
NEXT p%
    IF row <> 0 THEN
        arena(row, col).acolor = acolor             'assign color to arena
        realRow = arena(row, col).realRow           'Get real row of pixel
        topFlag = arena(row, col).sister + 1 / 2    'Deduce whether pixel
                                                    'is on topﬂ, or bottom‹
        sisterRow = row + arena(row, col).sister    'Get arena row of sister
        sisterColor = arena(sisterRow, col).acolor  'Determine sister's color

        LOCATE realRow, col

        IF acolor = sisterColor THEN                'If both points are same
            COLOR acolor, acolor                           'Print chr$(219) "€"
            PRINT CHR$(219);
        ELSE
            IF topFlag THEN                         'Since you cannot have
                IF acolor > 7 THEN                  'bright backgrounds
                    COLOR acolor, sisterColor       'determine best combo
                    PRINT CHR$(223);                'to use.
                ELSE
                    COLOR sisterColor, acolor
                    PRINT CHR$(220);
                END IF
            ELSE
                IF acolor > 7 THEN
                    COLOR acolor, sisterColor
                    PRINT CHR$(220);
                ELSE
                    COLOR sisterColor, acolor
                    PRINT CHR$(223);
                END IF
            END IF
        END IF
    END IF
END SUB

'SpacePause:
'  Pauses game play and waits for space bar to be pressed before continuing
SUB SpacePause (text$)

    COLOR colortable(5), colortable(6)
    Center 11, "€ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ€"
    Center 12, "€ " + LEFT$(text$ + SPACE$(29), 29) + " €"
    Center 13, "€‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹€"
    WHILE INKEY$ <> "": WEND
    WHILE INKEY$ <> " ": WEND
    COLOR 15, colortable(4)

    FOR i = 21 TO 26            ' Restore the screen background
        FOR j = 24 TO 56
            set i, j, arena(i, j).acolor
        NEXT j
    NEXT i

END SUB

'SparklePause:
'  Creates flashing border for intro screen
SUB SparklePause

    COLOR 4, 0
    a$ = "*    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    "
    WHILE INKEY$ <> "": WEND 'Clear keyboard buffer

    WHILE INKEY$ = ""
        FOR a = 1 TO 5
            LOCATE 1, 1                             'print horizontal sparkles
            PRINT MID$(a$, a, 80);
            LOCATE 22, 1
            PRINT MID$(a$, 6 - a, 80);

            FOR b = 2 TO 21                         'Print Vertical sparkles
                c = (a + b) MOD 5
                IF c = 1 THEN
                    LOCATE b, 80
                    PRINT "*";
                    LOCATE 23 - b, 1
                    PRINT "*";
                ELSE
                    LOCATE b, 80
                    PRINT " ";
                    LOCATE 23 - b, 1
                    PRINT " ";
                END IF
            NEXT b
        NEXT a
    WEND

END SUB

'StillWantsToPlay:
'  Determines if users want to play game again.
FUNCTION StillWantsToPlay

    COLOR colortable(5), colortable(6)
    Center 10, "€ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ€"
    Center 11, "€       G A M E   O V E R       €"
    Center 12, "€                               €"
    Center 13, "€      Play Again?   (Y/N)      €"
    Center 14, "€‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹€"

    WHILE INKEY$ <> "": WEND
    DO
        kbd$ = UCASE$(INKEY$)
    LOOP UNTIL kbd$ = "Y" OR kbd$ = "N"

    COLOR 15, colortable(4)
    Center 10, "                                 "
    Center 11, "                                 "
    Center 12, "                                 "
    Center 13, "                                 "
    Center 14, "                                 "

    IF kbd$ = "Y" THEN
        StillWantsToPlay = TRUE
    ELSE
        StillWantsToPlay = FALSE
        COLOR 7, 0
        CLS
    END IF

END FUNCTION


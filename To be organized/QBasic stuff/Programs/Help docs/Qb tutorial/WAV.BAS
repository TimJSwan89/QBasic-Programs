
'********************************************
'*                DMAWAV.BAS                *
'*              by Ken Rockot               *
'*  Insane7773@aol.com, Rockotman@juno.com  *
'********************************************
'NOTE: This program was tested with a Creative Labs Sound Blaster 16
'      on my 486 DX/2 running at 66MHz.  This is by far the FASTEST WAV
'      player for QBasic that I have seen.  Unlike other WAV players for
'      QBasic which can play WAVs too slow, because this uses DMA, it can
'      play WAVs too fast if you set the frequency too high (Most
'      commonly 11000, 22000, or 41000Hz).
'ANOTHER NOTE: You need to have an environment variable called BLASTER
'that
'              tells what DMA and Base Address your SB is on.  Most Sound
'              Blasters' drivers set this in the AUTOEXEC.BAT
'automatically.
'YET ANOTHER NOTE:  You may receive the 'DSP failed to reset' error.  If
'this
'                   happens, run the program again.  I don't know why,
'but
'                   it usually doesn't work the first time.

DEFINT A-Z   'Sets all variables to integers unless specified otherwise
DECLARE SUB LoadWAV (File$)
DECLARE FUNCTION ResetDSP ()
DECLARE SUB OutDSP (Byte)
DECLARE FUNCTION ReadDSP ()
DECLARE FUNCTION DSPVersion! ()
DECLARE SUB PlayWAV (Segment&, Offset&, Length&, Frequency&)
DECLARE FUNCTION PlayDone ()
DECLARE SUB ReadBLASTER ()
DIM SHARED BaseAdd, DMA, LenPort, Length&      ' Set vars to shared so we
'can
DIM SHARED WaveBuffer(0) AS STRING * 32767      ' use in different subs
CLS
ReadBLASTER         'Read the BLASTER environment variable
rd = ResetDSP
IF ResetDSP THEN    'Reset DSP (Success = -1, Failure = 0)
        PRINT "DSP reset successfully."
ELSE
        PRINT "DSP failed to reset"
        END
END IF
INPUT "Enter filename: ", File$                 'Get user input for a WAV
'file
File$ = "C:\USERS\TIM\QBASIC\HELPDO~1\QBTUTO~1\CHAT1.WAV"
INPUT "Enter frequency: ", Frequency&           'Get input for frequency
LoadWAV File$
OUT BaseAdd + 12, &HD1                         'Turn on speakers
DO
        PlayWAV VARSEG(WaveBuffer(0)), VARPTR(WaveBuffer(0)), Length&, Frequency&
        DO
        COLOR INT(RND * 15) + 1         'Print crap in colors
        PRINT "Looping WAV in the background while performing other functions."
        LOOP UNTIL PlayDone                     'Loop until WAV is
LOOP UNTIL INKEY$ = CHR$(27)                    'Play the WAV over and
OUT BaseAdd + 12, &HD3                         'Turn off speakers

FUNCTION DSPVersion!
OutDSP &HE1     'Function to get DSP Version
A = ReadDSP     'High byte
B = ReadDSP     'Low byte
DSPVersion! = VAL(STR$(A) + "." + STR$(B))  'Set DSPVersion!
END FUNCTION

SUB LoadWAV (File$)
WaveBuffer(0) = ""          'Clear the WAV buffer
OPEN File$ FOR BINARY AS #1 'Open the WAV file
IF LOF(1) = 0 THEN CLOSE #1: KILL File$: PRINT "Non-existant file!": END
GET #1, 44, WaveBuffer(0)                       'Skip 44 bytes (file
'header)
Length& = LOF(1) - 44                           'Store the length of the
'WAV
IF Length& > 32766 THEN Length& = 32766         'Truncate the WAV
CLOSE #1                    'Close the file
END SUB

SUB OutDSP (Byte)
DO
LOOP WHILE INP(BaseAdd + 12) AND &H80      'Loop until we get an OK byte
OUT BaseAdd + 12, Byte         'Set a byte to the SB input register
END SUB

FUNCTION PlayDone
A = INP(LenPort)        '\___ Read two bytes from the length port
B = INP(LenPort)        '/
Count& = CLNG(A + 1) * CLNG(B + 1)            '\____ Check to see if it's
'done
IF (Count& - 1) >= &HFFFF& THEN PlayDone = -1 '/
END FUNCTION

SUB PlayWAV (Segment&, Offset&, Length&, Frequency&)
MemLoc& = Segment& * 16 + Offset&   'Area in memory of WAV buffer
Page = 0                            'DMA page to use
SELECT CASE DMA                     'Depending on DMA set vars to values
        CASE 0
                PgPort = &H87       'Set port to send page data to
                AddPort = &H0       'Set port to send memory address to
                LenPort = &H1       'Set port to send length to
                ModeReg = &H48      'Set mode to play in
        CASE 1
                PgPort = &H83
                AddPort = &H2
                LenPort = &H3
                ModeReg = &H49
        CASE 2
                PgPort = &H81
                AddPort = &H4
                LenPort = &H5
                ModeReg = &H4A
        CASE 3
                PgPort = &H82
                AddPort = &H6
                LenPort = &H7
                ModeReg = &H4B
        CASE ELSE
                PRINT "This program only supports DMA Channels 0-3."
                END
END SELECT
OUT &HA, &H4 + DMA              ' Send all
OUT &HC, 0                      ' the crap to
OUT &HB, ModeReg                ' the ports.
OUT AddPort, MemLoc& AND &HFF   ' More crap sending
OUT AddPort, (MemLoc& AND &HFFFF&) \ &H100 ' Yet more
IF (MemLoc& AND 65536) THEN Page = Page + 1     ' Set
IF (MemLoc& AND 131072) THEN Page = Page + 2    ' the
IF (MemLoc& AND 262144) THEN Page = Page + 4    ' correct
IF (MemLoc& AND 524288) THEN Page = Page + 8    ' memory page.
OUT PgPort, Page    ' Send page data
OUT LenPort, Length& AND &HFF               ' Send length
OUT LenPort, (Length& AND &HFFFF&) \ &H100  ' data
OUT &HA, DMA    ' Send DMA port
IF Frequency& < 23000 THEN
        TimeConst = 256 - 1000000 \ Frequency&  ' Set the freqency
        OutDSP &H40                             ' Byte to send frequency
        OutDSP TimeConst                        ' Send frequency
        OutDSP &H14                             ' Byte to send length
        OutDSP (Length& AND &HFF)               ' Send
        OutDSP ((Length& AND &HFFFF&) \ &H100)  ' length
ELSE
        IF DSPVersion! >= 3 THEN        ' If they got the right DMA to
'fast
 TimeConst = ((65536 - 256000000 \ Frequency&) AND &HFFFF&) \ &H100
                OutDSP &H40
                OutDSP TimeConst
                OutDSP (Length& AND &HFF)
                OutDSP ((Length& AND &HFFFF&) \ &H100)
        ELSE
                PRINT "You must have a DSP Version of 3.00 or greater to "
PRINT "use a high frequency."
                END
        END IF
END IF
END SUB

SUB ReadBLASTER
B$ = ENVIRON$("BLASTER")            'Get the BLASTER variable
IF B$ = "" THEN PRINT "BLASTER environment variable not found!": END
'Darn!
FOR X = 1 TO LEN(B$)            ' Basically simple crap to read the
'variable
        T$ = MID$(B$, X, 1)
        Y = X + 1
        SELECT CASE T$
                CASE "A"
                DO
                        Tp$ = MID$(B$, Y, 1)
                        IF Tp$ = " " THEN EXIT DO
                        Addr$ = Addr$ + Tp$
                        Y = Y + 1
                LOOP
                BaseAdd = VAL("&H" + Addr$)
                Tp$ = ""
                CASE "D"
                DO
                        Tp$ = MID$(B$, Y, 1)
                        IF Tp$ = " " THEN EXIT DO
                        DMAC$ = DMAC$ + Tp$
                        Y = Y + 1
                LOOP
                DMA = VAL(DMAC$)
                Tp$ = ""
        END SELECT
NEXT X
END SUB

FUNCTION ReadDSP
DO
LOOP UNTIL INP(BaseAdd + 14) AND &H80 ' If it's okay...
ReadDSP = INP(BaseAdd + 10)           ' Read a byte from the SB port
END FUNCTION

FUNCTION ResetDSP
OUT BaseAdd + 6, 1         ' Send byte to reset
FOR I = 1 TO 4              ' Read crap
   Temp = INP(BaseAdd + 6)
NEXT I
OUT BaseAdd + 6, 0         ' Send 'Done' byte
A = INP(BaseAdd + 14)      ' Read prototype byte
IF INP(BaseAdd + 14) AND &H80 = &H80 AND INP(BaseAdd + 10) = &HAA THEN
'Reset
   ResetDSP = -1
ELSE
   ResetDSP = 0
END IF
END FUNCTION


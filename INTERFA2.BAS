CLS
WHILE Name$ = ""
PRINT "Welcome to Timothy's program. Please enter your first name below."
INPUT "", Name$
WEND
CLS
WHILE Entry$ <> "exit"
PRINT "Please enter a statement below."
INPUT "", Entry$
  FOR a = 1 TO LEN(Entry$)
    n$ = MID$(Entry$, a, 1)
      IF INSTR("abcdefghijklmnopqrstuvwxyz1234567890 ", n$) = 0 THEN
        PRINT "Please do not use capitals or punctuation."
        PRINT "Please enter a statement below."
        INPUT "", Entry$
        a = 1
    END IF
  NEXT
      wordnum = 1
      FOR charnum = 1 TO LEN(Entry$)
        c$ = MID$(Entry$, charnum, 1)
        IF c$ = " " THEN
          wordnum = wordnum + 1
        ELSE
          array$(wordnum) = array$(wordnum) + c$
        END IF
      NEXT
    IF array$(1) = "do" OR array$(1) = "are" OR array$(1) = "is" THEN
    SWAP array$(1), array$(2)
    END IF
    FOR a = 1 TO wordnum
      IF array$(a) = "i" OR array$(a) = "me" THEN
        array$(a) = "you"
      ELSEIF array$(a) = "am" THEN
        array$(a) = "are"
      ELSEIF array$(a) = "you" THEN
        IF array$(a + 1) =  THEN
      END IF
    NEXT a
    FOR a = 1 TO wordnum
    IF array$(a) <> "" THEN
    PRINT array$(a); : IF a < wordnum THEN PRINT " ";
    array$(a) = ""
    END IF
    NEXT
    PRINT "."
WEND


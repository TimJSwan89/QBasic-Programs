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
    FOR a = 1 TO wordnum
          'PRINT wordnum, a, Array$(a)
    IF array$(a) = "i" THEN
    array$(a) = "you"
    'ELSEIF array$(a - 3) = "how" AND array$(a + 3) = "you" THEN
    'swap$ = array$(a + 2)
    'array$(a + 2) = array$(a + 3)
    'array$(a + 3) = swap
    ELSEIF array$(a) = "do" THEN
    array$(a) = ""
    ELSEIF array$(a) = "are" AND array$(a + 1) = "you" THEN
    array$(a) = "am"
      IF array$(a - 1) = "how" THEN
      IF array$(a + 2) = "doing" THEN
      array$(a + 1) = "doing"
      array$(a + 2) = "well"
      ELSE
      array$(a + 1) = "good"
      END IF
      array$(a - 1) = "I"
      array$(a) = "am"
      END IF
      IF a > 2 THEN
      IF array$(a - 2) = "how" THEN
      array$(a - 2) = ""
      swap$ = array$(a + 1)
      array$(a + 1) = array$(a - 1)
      array$(a - 1) = "I"
      END IF
      END IF
    ELSEIF array$(a) = "am" THEN
    array$(a) = "are"
    ELSEIF array$(a) = "you" THEN
    array$(a) = "I"
      IF array$(a - 1) = "are" THEN
      array$(a) = "am"
      array$(a - 1) = "I"
      ELSEIF array$(a - 1) = "am" THEN
      array$(a) = "am"
      array$(a - 1) = "I"
      ELSEIF a = wordnum THEN
      array$(a) = "me"
      END IF
    END IF
    NEXT
    FOR a = 1 TO wordnum
    IF array$(a) <> "" THEN
    PRINT array$(a); " ";
    array$(a) = ""
    END IF
    NEXT
    PRINT ""
WEND


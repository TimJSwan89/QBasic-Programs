
'>   What is the difference between SUBs and FUNCTIONS?
'>.........
'_|_|_|   SUBTUTOR.BAS
'_|_|_|   A short tutorial on SUB and FUNCTION usage.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (10/6/96)
COLOR 15, 1: CLS
PRINT " This is a short tutorial on use of SUB and FUNCTION in QBasic"
PRINT " and Quick Basic. It explores some of the relationships of"
PRINT " variables between modules. By studying the code and following"
PRINT " with the text, one may grasp module concepts."
PRINT " Variables in a SUB or FUNCTION, unless declared as STATIC,"
PRINT " are AUTOMATIC variables. BASIC initializes them every time"
PRINT " the module containing them is called. They may have the same"
PRINT " name as variables in other modules without confusion."
PRINT " SHARED and PASSED variables will be examined. Ready?"
DO: LOOP WHILE INKEY$ = "": CLS
PRINT " We will begin with the SUB. Variables may be passed by SEG or"
PRINT " or by VAL. Variables enclosed in parentheses are passed by VALue."
PRINT " FUNCTION variables must be enclosed in a group parentheses, and"
PRINT " we may also use VAL parentheses. First, we will pass by VALue."
PRINT : MyStr$ = "This won't be changed.": MySub (MyStr$)
PRINT " MyStr$ = "; MyStr$: PRINT
PRINT " As you can see, MyStr$ is unchanged"
DO: LOOP WHILE INKEY$ = "": CLS
PRINT " Now we will pass by SEGment.": PRINT
MyStr$ = "This will be changed."
MySub MyStr$: PRINT " MyStr$ = "; MyStr$: PRINT
PRINT " Since the ADDRESS of MyStr$ was passed, the SUB was"
PRINT " able to change the contents of the variable."
DO: LOOP WHILE INKEY$ = "": CLS
PRINT " Now we will use a FUNCTION and pass by VALue.": PRINT
MyStr$ = "This won't be changed.": FuncStr$ = MyFunc((MyStr$)):
PRINT " MyFunc$ = "; FuncStr$: PRINT " MyStr$ = "; MyStr$: PRINT
PRINT " As a result, MyStr$ is unchanged, and a new string is created."
DO: LOOP WHILE INKEY$ = "": CLS
PRINT " Now we will pass MyStr$ by SEGment.": PRINT
MyStr$ = "This will be changed.": FuncStr$ = MyFunc(MyStr$):
PRINT " MyFunc$ = "; FuncStr$: PRINT " MyStr$ = "; MyStr$: PRINT
PRINT " We still got the new string, but now MyStr$ has changed!"
DO: LOOP WHILE INKEY$ = "": CLS
PRINT " Unlike subroutines using GOSUB, variables found in the"
PRINT " MAIN module are unknown in the SUB or FUNCTION routines."
PRINT " This is known as the SCOPE of a variable. In order for a"
PRINT " variable to be VISIBLE to a SUB or FUNCTION, it must be"
PRINT " declared as being SHARED, or be passed as an argument."
DO: LOOP WHILE INKEY$ = "": PRINT
MyStr$ = "This is MyStr$.": MyShare: PRINT " MyStr$ = "; MyStr$: PRINT
PRINT " This way, variables are available to your SUB or FUNCTION."
DO: LOOP WHILE INKEY$ = "": CLS
PRINT " In addition, we may declare variables SHARED in the main"
PRINT " module by use of the DIM statement.": PRINT
DIM OurStr AS STRING * 32: OurStr = "This is a shared string."
MySub OurStr: PRINT " OurStr = "; OurStr: PRINT
PRINT " So here we see that this string is shared in the SUB."
DO: LOOP WHILE INKEY$ = "": PRINT
PRINT " If we were to pass a fixed length string as an argument,"
PRINT " then the contents of the string would be passed and not the"
PRINT " string itself, although, if that string were passed by SEGment,"
PRINT " then whatever changes were made to the passed string would be"
PRINT " stored in the fixed string when we exit the SUB or FUNCTION."
DIM FixedStr AS STRING * 10: FixedStr = "A string"
PRINT " FixedStr = "; FixedStr: Fixed FixedStr
PRINT " FixedStr = "; FixedStr: PRINT
PRINT " The SUB string is a temporary dynamic string created by the SUB"
PRINT " and truncated to fit into the fixed length string."
DO: LOOP WHILE INKEY$ = "": PRINT
PRINT " I hope this answers most of your questions concerning"
PRINT " the use of SUBs and FUNCTIONs in QBasic and Quick Basic."
DO: LOOP WHILE INKEY$ = "": COLOR 2, 0: CLS
'_|_|_|   end   SUBTUTOR.BAS

SUB Fixed (p$)
   PRINT " (passed string)p$ = "; p$
   p$ = "This string is way too big for FixedStr."
   PRINT " (passed string)p$ = "; p$
END SUB

FUNCTION MyFunc$ (p$)
   PRINT " (passed variable)p$ = "; p$: p$ = "changed"
   MyFunc$ = "This is an entirely new string!"
END FUNCTION

SUB MyShare
   SHARED MyStr$: PRINT " SHARED variable MyStr$ = "; MyStr$
   MyStr$ = "This variable, being SHARED, may be changed."
END SUB

SUB MySub (p$)
   PRINT " (passed variable)p$ = "; p$: p$ = "changed"
END SUB



______________________________________________________________________________
|  Redefining Characters  |
~~~~~~~~~~~~~~~~~~~~~~~~~~~

 This program will change ascii character 65 (A) into a small stick figure
 So all of the As will be stick men. This program is fully tested but it may
 be incompatable with your BIOS. (I have a very popular AMI BIOS)
 Remember that QB4.5 or PDS7 should be loaded with the  /L  switch to have
 QBX.QLB or QB.QLB loaded.
 This program I am sure will interest you people out there that want to write
 games in text mode and have your own custom characters.

'$INCLUDE: 'qbx.bi'                             'Change to QB.BI for QB4.5
DIM SHARED inregsx AS RegTypeX
DIM SHARED outregsx AS RegTypeX
DIM chardata AS STRING
chardata = CHR$(60) + CHR$(66) + CHR$(66) + CHR$(129) + CHR$(129) + CHR$(66) + CHR$(24) + CHR$(24) + CHR$(126) + CHR$(24) + CHR$(24) + CHR$(24) + CHR$(36) + CHR$(36) + CHR$(195)
inregsx.ax = &H1100
inregsx.bx = &H1000
inregsx.cx = &H1
inregsx.dx = 65
inregsx.es = SSEG(chardata)
inregsx.bp = SADD(chardata)
CALL InterruptX(&H10, inregsx, outregsx)

 Let me step through the code and explain it...

'$INCLUDE: 'qbx.bi'                             'Change to QB.BI for QB4.5
DIM SHARED inregsx AS RegTypeX
DIM SHARED outregsx AS RegTypeX
DIM chardata AS STRING

 This loads up the include file for your MicroSoft basic compiler and also
 declares some variables for use by the program. 

chardata = CHR$(60) + CHR$(66) + CHR$(66) + CHR$(129) + CHR$(129) + CHR$(66) + CHR$(24) + CHR$(24) + CHR$(126) + CHR$(24) + CHR$(24) + CHR$(24) + CHR$(36) + CHR$(36) + CHR$(195)

 This is more interesting. This creates the shape that asc character 65 is 
 changed to. It is made up like so (only shown the first line):

  This is a grid of 8 bits for the first line. Each bit has a value doubling
  each time.

Value: 128  64    32   16   8     4   2     1
      _________________________________________
      |    |    |    |    |    |    |    |    |
      |    |    |    |    |    |    |    |    |
      |    |    |    |    |    |    |    |    |
      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
  For the value 60 (the first, top line) the following bits would be set:


Value: 128  64    32   16   8     4   2     1
      _________________________________________
      |    |    |XXXX|XXXX|XXXX|XXXX|    |    |
      |    |    |XXXX|XXXX|XXXX|XXXX|    |    |
      |    |    |XXXX|XXXX|XXXX|XXXX|    |    |
      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 That is because as you can see the  4,8,16,32 valued bits are set. These
 numbers add up to 60 and so that is what the top line of the character looks
 like. It's hard to grasp and understand, perhaps could be the subject of a
 tutorial some time? (binary)

inregsx.ax = &H1100
inregsx.bx = &H1000
inregsx.cx = &H1
inregsx.dx = 65

 This section of code sets some registers for calling the interrupt. You do
 not really need to worry about all of these but the inregsx.dx one is useful
 The dx one selects which ASCII character you want to adjust, 65 is A, 66 is
 B and so on, there is an ascii code for every character that you can see on
 your screen. 

inregsx.es = SSEG(chardata)
inregsx.bp = SADD(chardata)
CALL InterruptX(&H10, inregsx, outregsx)

 This sets some other special registers to the memory segment and offset that
 the character shape data is at. The CALL InterruptX then calls the hardware
 interrupt that changes the shape of the ascii character in DX (65).

 So now many of you can probably understand how to make any type of
 characters you want. You have a nice grid of 8x16 to use and in that you 
 can fit maybe a character of a house, man etc...


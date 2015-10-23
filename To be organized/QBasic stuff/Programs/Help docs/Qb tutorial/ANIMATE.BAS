
'>> Hello.  I've tried making simple animation in qbasic but with no
'>> success.  The command i use to simulate the movements is CLS, but it
'>> doesn't seem to work.  Is there another way or command i can use to
'>> make simple animation??  Thanks. =)

'>Use GET and PUT XOR.

'...And I'll bet that helped you a WHOLE lot.


Here is a preview of the newest addition to the code FAQ. It explains in
(I hope) enough detail to get a beginner started with animation in QB.
Take a look, ask a question if you don't understand.

8) HOW CAN I ANIMATE SPRITES IN QB?

   First thing, what's a sprite? Good question. Basically, it's a
   rectangle box that has a picture in it. In something like
   cartoon animation, they would be referred to as frames. So when
   you're animating in QB, you're essentially doing the same thing
   that you would do if you were to create a cartoon, just on a
   smaller scale.

   So how do you create sprites? QB has a variety of graphics
   functions (DRAW, LINE, CIRCLE, PSET etc.) that are at your
   disposal. They are quite cumbersome to draw with, so you might
   find it difficult to get started. You may want to keep your
   eyes open for a QB graphics editor, which there seem to be
   quite few of. So, here we go... plug for one of my own programs.
   Spectra.13 was designed specifically for drawing graphics in QB.
   It is totally mouse-controlled, has a handy pop-up menu,
   features many handy utilities (draw, erase, paint, line, circle,
   filled circle, box, filled box, text, Bitmap loading, palette
   manipulation, save & load pictures, save & load icons, on-line
   help, etc. etc. etc.) It's probably the most comprehensive QB
   drawing program available, but as of yet, is not widely
   available. It was just released in late November '96, so if you
   can't find it anywhere, get in touch with me, Dave Shea, and
   I'll see what I can do to hook you up with it.

   Back to animation...

   The first thing you have to do is set aside memory for your
   sprite using the DIM command. This can be done like this:

   DIM Array%(NumberOfBytes)

   Where Array% is the name of the array you're storing the sprite
   data in. For our purposes in this section of the FAQ, I won't
   explain what an array is. Just think of it as a really big
   variable that we will store our picture in. How do you determine
   the number of bytes you need to set aside? It depends on your
   screen mode. We will assume SCREEN 13 from now on, in which each
   pixel requires exactly one byte of memory. So, to determine the
   amount of memory needed, the formula would be:

   NumberOfBytes = ((XValue * YValue) / 2) + 12

   The extra 12 bytes are to store the (x,y) dimensions of the
   array. You must divide by two because each element of Array%
   is an integer value, 16 bytes, and we only need 8. If you don't
   understand, don't worry. Just follow the formula.

   So where do the XValue and YValue come from? Like I said, a
   sprite is a rectangle, so the XValue is the number of pixels
   across the sprite horizontally (Left to right), and Y is the
   number of pixels going up and down. Once you have your graphic
   on the screen, try drawing a box around it. Once you have a box
   that completely surrounds the graphic, use the X and Y values
   for the box as the same for your graphic. If you're having
   troubles with this, use the following formula for a box that
   completely surrounds your graphic:

   LINE (x1, y1) - (x2, y2), 31, B
   x = (x2 - x1) : y = (y2 - y1)

   Okay, so now I have everything set up, how do I store my picture
   in the array? Using the GET command. It's as easy as this:

   GET (x1, y1) - (x2, y2), Array%

   The x1, y1 etc. are the exact same as in the LINE command above,
   and the array Array% is the one we dimensioned above. So, now
   you have the graphic in memory, you can clear the screen so it's
   not still on there.

   Animation starts here. Once you have all the sprites stored in
   memory, you will most likely want to stick them back on the
   screen at a certain time in your program. This is done through
   the PUT command. Try this:

   PUT (50,50), Array%

   That will stick the graphic you have in Array% at position
   (50, 50) on the screen. Easy as that.

   So, now I have this going for me, how do I actually animate?

   It's quite simple from here. Basically, you stick your picture
   on the screen, delay for a bit, erase it and immediatly stick
   another picture on the screen at a different location, and so
   on. The one problem with this is that, for a very short time,
   there is no picture on the screen. This will cause the animation
   to flash a certain amount, no matter how fast your computer is.
   There is one way that slightly avoids this problem, but it's not
   a complete solution. This will work only if you are using small
   sprites, and only if you don't animate near the top of the
   screen. What you do is add a line AFTER you have placed your
   sprite, but BEFORE you erase it. (As it slows down the program a
   bit, it can also be used in place of a delay loop) This command
   is called the vertical retrace, and it goes like this:

   WAIT 986, 8    (or WAIT &H3DA, 8....)

   That will help, but only a little bit. The following example is
   a little animation of a guy running, and it uses the techniques
   described above.

[...........................Cut Here..............................]
   ' =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   '   RUN.BAS   by   Dave Shea
   '   Released to Public Domain
   '   on November 22nd, 1996. No
   '   Warantees expressed or
   '   implied.
   ' =-=-=-=-=-=-=-=-=-=-=-=-=-=-=

   CLS : SCREEN 13               'Set Screen 13, of course!


   PSET (25, 71), 31: DRAW "rdld2l2ngr3fnehld3rfrdngulhlg2lh"
   PSET (45, 71), 31: DRAW "rdld2lgnder2dnr2ld2rfdnguhld2lg"
   PSET (65, 71), 31: DRAW "rdld2ldrnednrddrfndhldlg"
   PSET (85, 71), 31: DRAW "rdld2lgnderdnr2d2rfdnguhlgdl2"

                                 'The above will draw four
                                 'different frames of a Stick man
                                 'running.

   DIM Guy1%(134), Guy2%(134), Guy3%(134), Guy4%(134)

                                 'Set aside the memory required for
                                 'each frame.

   GET (16, 69)-(34, 82), Guy1%  'Stick each frame into memory
   GET (36, 69)-(54, 82), Guy2%  '(X and Y values calculated by
   GET (56, 69)-(74, 82), Guy3%  ' pure trial and error. =u)
   GET (76, 69)-(94, 82), Guy4%

   CLS                           'Guess what THIS does!

   FOR a = 1 TO 300 STEP 2                'Begin Loop
    b = b + 1: IF b = 5 THEN b = 1        'Increase b until it
                                          'reaches a value of 5,
                                          'reset it to 1

    IF b = 1 THEN PUT (a, 100), Guy1%     'Determine which frame to
    IF b = 2 THEN PUT (a, 100), Guy2%     'PUT, then PUT it.
    IF b = 3 THEN PUT (a, 100), Guy3%
    IF b = 4 THEN PUT (a, 100), Guy4%

    WAIT 986, 8                           'Wait for Vertical
                                          'Retrace

    Start = TIMER                         'Small delay loop to
    WHILE TIMER < Start + .1: WEND        'slow it down a bit.

    IF b = 1 THEN PUT (a, 100), Guy1%     'Erase current frame by
    IF b = 2 THEN PUT (a, 100), Guy2%     'PUTting it again.
    IF b = 3 THEN PUT (a, 100), Guy3%
    IF b = 4 THEN PUT (a, 100), Guy4%

   NEXT                                   'Loop!

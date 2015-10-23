
'>        Does anyone know an easy way of modifying the bits in a byte? I
'>mean as in is it possible to assign a variable to each bit of a byte o
'>r
'>must you modify them by usind AND/OR/XOR?

'Here is a short program to "modify the bits in a byte"....

DEFINT A-Z                      'Define ALL numbers A thru Z as INTEGERs
                                'so processing SPEED is faster...


BYTE = 88                       'Define a value for BYTE

'************************* CONVERT BYTE to BITS ************************
A = (BYTE AND 1) \ 1            'Bit 0
B = (BYTE AND 2) \ 2            'Bit 1
C = (BYTE AND 4) \ 4            'Bit 2
D = (BYTE AND 8) \ 8            'Bit 3
E = (BYTE AND 16) \ 16          'Bit 4
F = (BYTE AND 32) \ 32          'Bit 5
G = (BYTE AND 64) \ 64          'Bit 6
H = (BYTE AND 128) \ 128        'Bit 7

'**************************** MODIFY BITS HERE *************************
'example
A = (NOT A) + 2                 'Toggles BIT "A" (0 or 1)
G = (NOT G) + 2                 'Toggles BIT "G" (0 or 1)

'********************** CONSTITUTE BITS into a BYTE *******************
BYTE = A + B * 2 + C * 4 + D * 8 + E * 16 + F * 32 + G * 64 + H * 128


PRINT BYTE                      'Display NEW byte value

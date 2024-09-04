TRS80-model4-1bit-music/settings


1-bit music for the TRS80 model 4 computer.
Converted to run on the model 4 from the ZX Specrtrum 1-bit music scene.  Use PASMO and SJASMPLUS assemblers where required.
Note: All audio is sent to the cassette port. Connect up an amplified speaker to the cassette port/jack/socket.
Credits to the original creators: Shiru, Utz, Tim Follin, and others.

.WAV, .CAS, .CMD files included. Tested in the 'TRS80GP' emulator. Not tested on real hardware.

Details for 1-bit music on TRS80 via cassette port:
  
  ORG $6000
  
  Cassette port hangs off Bits 0 and 1 on port 255.
  
  Output: Bits 0-1: 00=0.85 Volts, 10=0.0 Volts, 01 = 0.46 Volts (effectively could have three volume levels / 3 bit audio)
  
  So... spit out alternate values $00 and $02 to OUT ($FF)

Still to do: All tunes still need to be changed to port $90 for the internal speaker in the model 4.

DISK1.DSK contains the first 25 music tunes.

DISK2.DSK contains five octodeXl tunes.

TRITONE.DSK contains all ~30 tritone tunes.



                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;     Sorcerer Function/Chapter 7     ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;        load code to run SPEAC       ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

SPEAC
The SPEAC program begins by analyzing the surface detail of the chosen work. This 
means that each SPEAC identifier is selected based on metric, tension, beat, duration, and
other considerations as discussed in detail in Computer Models of Musical Intelligence. 
The program demonstrates this in its surface SPEAC window after users carefully adjust the
variables Interval Strength and Form Variables (particularly "Meter" and "Begin Beat").

The SPEAC program also analyzes form based on the setting of the remaining variables in 
Form Variables and demonstrated visually under Form Tree and Form Detail. Changing the 
variables will significantly vary the analysis. Settings to start with are given at the top
of each file for that database. Be sure not only to load a work but also to select it.  


VARIABLES
In order for the SPEAC program to operate effectively, one must set a number of
variables, variables which require experimentation to set correctly. Here are the
relevant variables for this program and their default settings:

*cadence-minimum* 9000 (tries to determine form by finding cadences - this is the minimum)

*pattern-size* 8 (size of pattern for pattern matching)

*threshold* 2 (threshold for pattern matching)

*intervals-off* 1 (intervals off for pattern matching)

*amount-off* 2 (amount off for pattern matching)

then GET-THE-LEVELS runs all of the basic approaches to finding form, primarily: 

density
composite-rhythm
cadences
find-all-pattern-occurances

and stores these in *output* and returns a four-deep speac tree as in

(((s1)) ((p2 e2 e2)) ((p3 e3) (s3 e3) (p3 e3))
 ((p4 e4 e4 e4 p4 e4 s4 e4 e4 e4 a4 p4 e4 s4 a4 s4)
  (p4 e4 e4 s4 e4 e4 e4 e4 e4 e4 e4 e4 p4 e4 s4 e4 e4 p4 e4 e4 e4 s4 e4 e4
   a4)
  (s4 e4 e4 a4 c4 s4 e4)
  (p4 e4 p4 e4 s4 e4 p4 e4 s4 e4 e4 a4 s4 e4 e4 e4 e4 e4 e4 e4 e4 p4 e4)
  (p4 e4 e4 e4 p4 e4 s4 e4 e4 e4 a4 p4 e4 e4 e4 s4)
  (p4 e4 e4 e4 p4 e4 s4 e4 a4 s4 a4 s4 p4 e4 s4 e4 e4)))

Then use form-the-levels on the output of GET-THE-LEVELS and then make-form-window.


or just run (analyze-speac-in-music chopin-33-3).


TEST WORKS FOR SPEAC. 
Each of the complete works here (names indicate their source) has been 
very carefully created foillowing rather severe constraints. First, the
events in each file follow 1000 equals a quarter note. This is very important
as the SPEAC program relies on the 1000-equals-beat model. Thus, none of 
these works is performed but rather follows its printed representations. The
music also does not contain any ornamentation.

Second, channel one has been relegated to the top voice only (i.e., does not
represent the right hand or treble clef music, but only the upper-most 
sounding voice at any given time with the other music found in channel 2). 

Third,
I have placed the pickup notes (when applicable) at the ends of the music rather
than at the beginning to create a complete metrical work.

Fourth, the music has been transposed to C major if in major and into C minor if in minor. 
This transposition includes sections in different keys than the original key.
 This process of transposition takes place during the loading stage.

The forms of these pieces by phrase are (arguably and based on cadence finding):

Here are examples of the effectiveness of cadence determination to find phrases:
? (setq *cadence-minimum* 9000)
9000
? (cadences chopin-33-3)
((c1 10000) (a1 21000) (c1 33000) (c1 45000) (a1 56000) (a1 67000) (c1 79000) (c1 90000)
 (c1 101000) (a1 117000) (c1 129000) (c1 141000))

desired (12):
(9 21 33 45 57 69 81 93 105 117 129 141)

***********

? (setq *cadence-minimum* 9000)
9000
? (cadences chopin-63-2)
((c1 10000) (c1 21000) (c1 33000) (c1 45000) (c1 57000) (c1 69000) (c1 81000) (c1 93000)
 (a1 104000) (c1 118000) (c1 129000) (c1 141000) (c1 153000) (c1 165000))

desired (14):
(9 21 33 45 57 69 81 96 105 117 129 141 153 165)

***********

? (setq *cadence-minimum* 16000)
16000
? (cadences chopin-67-4)
((a1 18000) (c1 36000) (c1 54000) (c1 72000) (c1 90000) (c1 108000) (a1 127000) (c1 145000)
 (c1 165000) (c1 184000) (c1 202000) (a1 223000) (c1 241000) (a1 259000) (c1 277000)
 (a1 295000) (c1 315000) (c1 333000))

desired (14):
(21 45 69 93 117 141 165 189 213 237 261 285 309 333)

HERE ARE EXAMPLES OF DENSITY FOR FINDING PHRASES 
as expected - not much use for chopin's mazurkas
(density chopin-33-3)
((58 1000) (24 58000) (7 82000) (55 89000))

(density chopin-63-2)
((37 1000) (1 37000) (3 38000) (7 41000) (1 48000) (2 49000) (3 51000) (1 54000) (2 55000)
 (1 57000) (2 58000) (1 60000) (2 61000) (1 63000) (3 64000) (11 67000) (1 78000) (2 79000)
 (1 81000) (2 82000) (1 84000) (2 85000) (4 87000) (6 91000) (6 97000) (5 103000) (3 108000)
 (1 111000) (2 112000) (3 114000) (1 117000) (2 118000) (37 120000) (1 157000) (3 158000)
 (97 161000))

(density chopin-67-4)
((1 1000) (2 1000) (1 3000) (2 4000) (1 6000) (2 7000) (1 9000) (2 10000) (1 12000)
 (1 13000) (11 14000) (2 25000) (1 27000) (2 28000) (1 30000) (2 31000) (12 33000) (4 45000)
 (6 49000) (6 55000) (1 61000) (5 62000) (4 67000) (2 71000) (5 73000) (1 78000) (4 79000)
 (3 83000) (11 86000) (6 97000) (6 103000) (1 109000) (5 110000) (4 115000) (2 119000)
 (5 121000) (1 126000) (4 127000) (3 131000) (11 134000) (3 145000) (2 148000) (1 150000)
 (9 151000) (2 160000) (1 162000) (5 163000) (4 168000) (2 172000) (1 174000) (7 175000)
 (3 182000) (8 185000) (3 193000) (2 196000) (1 198000) (9 199000) (2 208000) (1 210000)
 (5 211000) (4 216000) (2 220000) (1 222000) (7 223000) (3 230000) (8 233000) (2 241000)
 (1 243000) (2 244000) (1 246000) (2 247000) (1 249000) (2 250000) (1 252000) (1 253000)
 (11 254000) (2 265000) (1 267000) (2 268000) (1 270000) (2 271000) (12 273000) (4 285000)
 (6 289000) (6 295000) (1 301000) (5 302000) (4 307000) (2 311000) (5 313000) (1 318000)
 (4 319000) (3 323000) (10 326000))

HERE ARE EXAMPLESOF COMPOSITE RHYTHM FOR FINDING PHRASES
as expected - not much use for chopin's mazurkas
(composite-rhythm chopin-33-3)
((212 1000))

(composite-rhythm chopin-63-2)
((260 1000))

(composite-rhythm chopin-67-4)
((552 1000))

THE EFFECTIVENESS OF FINDING PHRASE AND FORM USING PATTERN MATCHING
with ? *pattern-size*
12
? *intervals-off*
2
? *amount-off*
1

? (simple-matcher chopin-33-3)
((4 0 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) (4 24000 (2 -2 -2 -1 1 1 1 1 1 -2 -2))
 (4 96000 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) (4 120000 (2 -2 -2 -1 1 1 1 1 1 -2 -2)))
? (simple-matcher chopin-63-2)
((4 1000 (-1 -2 -2 -1 -2 2 -7 12 -5 1 4)) (3 24000 (0 -1 -2 -2 -1 -2 2 -7 12 -5 2))
 (3 120000 (0 -1 -2 -2 -1 -2 2 -7 12 -5 1)) (3 144000 (0 -1 -2 -2 -1 -2 2 -7 12 -5 2)))
? (simple-matcher chopin-67-4)
((6 48000 (2 1 2 1 2 -2 -1 5 -5 1 2)) (3 56500 (1 2 -2 -1 5 0 -2 0 -2 0 -1))
 (3 64000 (-5 0 1 7 -2 0 -1 0 -2 0 -5)) (3 71000 (-1 -5 2 1 2 1 2 -2 -1 5 -5))
 (3 79500 (2 2 1 2 -2 -1 8 -1 -1 -10 9)) (3 86500 (0 -1 -1 -9 8 -1 0 -1 -1 -10 8))
 (6 96000 (2 1 2 1 2 -2 -1 5 -5 1 2)) (3 104500 (1 2 -2 -1 5 0 -2 0 -2 0 -1))
 (3 112000 (-5 0 1 7 -2 0 -1 0 -2 0 -5)) (3 119000 (-1 -5 2 1 2 1 2 -2 -1 5 -5))
 (3 127500 (2 2 1 2 -2 -1 8 -1 -1 -10 9)) (3 134500 (0 -1 -1 -9 8 -1 0 -1 -1 -10 8))
 (6 288000 (2 1 2 1 2 -2 -1 5 -5 1 2)) (3 296500 (1 2 -2 -1 5 0 -2 0 -2 0 -1))
 (3 304000 (-5 0 1 7 -2 0 -1 0 -2 0 -5)) (3 311000 (-1 -5 2 1 2 1 2 -2 -1 5 -5))
 (3 319500 (2 2 1 2 -2 -1 8 -1 -1 -10 9)) (3 326500 (0 -1 -1 -9 8 -1 0 -1 -1 -10 8)))


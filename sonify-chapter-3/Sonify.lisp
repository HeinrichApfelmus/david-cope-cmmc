
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;     Sonify Function/Chapter 3       ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;    simple code to run sonifying     ;;;;;
                   ;;;;;             function                ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

#|(sonify random-data)|#

;;;  In order to run the sonify function you must load this file, 
;;;  and then use the (sonify random-data) or (sonify cassiopeia-a) as shown in the 
;;;  example for the top-level below.
;;;  The last two functions make playable (with MIDI) events for QT on a Macintosh
;;;  See the play midi QT file for further details.

;;;;;
#|? (sonify random-data)
(37 56 31 25 60 42 73 73 60 24 72 108 107 102 25 68 63 25 93 50 83 31 97 63 33 86 65 56 . . .
|#
;;;;;

(defun SONIFY (data)
  "Returns a list of in-range midi-note-number representation of list arg."
  (let ((max (apply #'max data))
        (min (apply #'min data)))
    (normalize-numbers data min max 24 108)))

;;;;;
#|? (normalize-numbers '(157 375 90 20 418) 20 418 24 108)
(53 99 39 24 108)
|#
;;;;;

(defun NORMALIZE-NUMBERS (numbers min max low high)
  "Returns a list of normalized outputs of the numbers arg."
  (if (null numbers)()
      (cons (normalize min max (first numbers) low high)
            (normalize-numbers (rest numbers) min max low high))))

;;;;;
#|? (normalize 20 418 90 24 108)
39
|#
;;;;;

(defun NORMALIZE (low1 high1 number low2 high2)
  "This function normalizes number into low-high-1 range to the low-high-2 range."
  (round (+ (* (/ (- number low1)(- high1 low1))(- high2 low2)) low2)))

;;data for sonify
;;rounded radio telescope data from the VLA in New Mexico

(defVar CASSIOPEIA-A '(9 11 5 11 5 11 14 11 14 11 14 11 14 17 14 17 14 9 4 14 9 17 
14 17 9 21 9 4 14 4 9 4 9 21 4 9 21 4 9 14 17 14 17 14 17 9 17 14 17 0 17 0 17 0 14 
17 14 17 14 17 0 17 0 17 0 17 0 17 0 17 0 4 0 4 0 4 0 4 0 4 0 17 9 17 14 17 4 14 9 4 14
 17 14 9 14 17 14 21 4 17 0 17 14 9 14 17 0 4 0 4 0 4 0 4 0 4 0 4 17 14 17 0 17 0 14 9 
14 4 17 0 4 9 4 9 14 9 14 9 4 9 4 9 4 14 0 17 0 14 17 14 17 0 14 0 14 17 14 17 14 17 14 
17 14 17 14 17 0 17 4 17 14 17 0 17 14 4 0 14 17 14 17 14 17 14 17 0 17 0 4 0 4 0 4 9 
4 9 4 9 14 19 14 19 0 5 0 5 9 14 19 2 11 19 2 7 16 21 7 11 16 4 16 21 4 9 21 9 0 9 14 2
 5 11 14 2 11 19 7 21 7 4 12 21 4 17 0 4 14 5 2 19 2 11 7 11 21 7 12 17 9 14 9 11 5 2 19 
11 7 21 7 16 12 9 17 0 19 9 14 2 11 19 7 16 11 7 16 4 16 12 21 9 16 21 4 17 4 0 19 0 17 
9 4 0 9 14 19 14 19 9 14 9 0 17 14 9 21 17 12 9 21 16 12 7 21 16 11 19 11 2 11 14 2 5 19 
9 4 14 9 16 21 2 7 21 7 11 16 9 14 12 9 4 21 4 21 19 2 16 19 5 14 19 9 14 0 9 21 4
 12 7 21 2 21 7 2 11 2 14 5 19 9 0 17 9 4 21 17 9 21 12 7 4 21 11 7 2 7 21 16 11 7 2 19 14 
5 2 19 14 9 5 19 14 9 4 0 17 9 17 0 14 17 14 9 14 9 4 9 14 17 9 4 9 14 9 14 9 14 9 14 17 
14 9 4 14 9 14 9 14 17 14 9 17 14 4 9 4 9 14 17 0 17 0 9 14 17 0 17 0 17 9 14 17 14 17 
9 14 9 14 17 14 17 14 17 14 17 14 17 14 17 14 9 14 9 14 9 14 17 14 17 14 17 14 17
 14 17 14 17 14 17 14 17 14 17 14 17 14 17 14 17 14 17 0 17 0 17 0 17 0 17 0 17 0 17 0 17 
0 17 0 17 0 17 0 17 0 17 0 17 19 14 19 11 14 11 14 11 14))

;;random data

(defVar RANDOM-DATA '(157 375 90 20 418 217 568 576 425 9 564 973 966 900 19 518 458 16 
797 309 684 87 847 455 115 720 474 378 340 231 963 835 159 805 788 38 630 405 405 664 
230 731 769 782 790 124 697 839 165 864 881 56 669 85 505 958 98 388 506 724 441 830 951 
743 457 658 163 395 275 912 538 461 271 767 453 904 885 400 245 874 57 332 429 163 756 117 
557 114 917 142 502 352 346 757 837 492 238 649 271 282))

;;;;;
#|The following functions make events for performing the above sonify function
as in (make-events (sonify random-data))
   ((0 37 1000 1 90) (1000 56 1000 1 90) (2000 31 1000 1 90) (3000 25 1000 1 90) . . . )|#
;;;;;

(defVar *RS* (make-random-state t) "Variable for storing the current random state.")

(defun MAKE-EVENTS (pitch-groupings &optional (random nil)(ontime 0))
  "Makes consecutive events out of the pairs of pitches in its arg."
  (if (null pitch-groupings) ()
      (let ((duration (if random (+ 250 (random 1750 *rs*)) 1000)))
        (append (list (make-event ontime (first pitch-groupings) (if random (1+ (random 16 *rs*)) 1)))
                (make-events (rest pitch-groupings) random (+ ontime duration))))))

;;;;;
#|(make-event 0 60 1)
  (0 60 1000 1 90)|#
;;;;;

(defun MAKE-EVENT (ontime pitch channel)
  "Creates an event based on args."
  (list ontime
        (if (symbolp pitch) (eval pitch) pitch)
        1000
        channel
        90))
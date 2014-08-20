
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;      Cosine Function/Chapter 3      ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;      simple code to run cosine      ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

#|(make-events (cosine 20 24 108))|#

;;;  In order to run the cosine function you must load this file, 
;;;  and then use the (cosine 20 24 108) as shown in the 
;;;  example for the top-level below.
;;;  The last two functions make playable (with MIDI) events for QT
;;;  by loading the play midi QT file
;;;  see that file for further instructions.

;;;;;
#|? (cosine 20 24 108)
(104 94 95 95 101 75 108 106 102 102 38 102 102 107 106 24 101 95 95 93)
the repeats here are due to normalizing and not to equivalent outputs|#
;;;;;

(defun COSINE (n midi-low midi-high)
  "Returns a list of midi note numbers formed by the cosine-function (1/cos x2)."
  (let* ((test (run-cosine-function n 1))
         (max (apply #'max test))
         (min (apply #'min test)))
    (normalize-cosines test min max midi-low midi-high)))

;;;;;
#|? (run-cosine-function 10 1)
(1.8508157176809255 -1.5298856564663974 -1.097537906304962 -1.044212499898521 
1.0088752655170414 -7.814716838125932 3.3267624923286805 2.5519498489381194 
1.2875216279777653 1.1596638229046938)|#
;;;;;

(defun RUN-COSINE-FUNCTION (number x)
  "Returns a list of incremental increases of x within the cosine-function 1/cos x2." 
  (if (zerop number)()
      (cons (/ 1 (cos (expt x 2)))
            (run-cosine-function (1- number)(1+ x)))))
   
;;;;;
#|? (normalize-cosines (1.8508157176809255 -1.5298856564663974 -1.097537906304962 
-1.044212499898521 1.0088752655170414 -7.814716838125932 . . .
(104 94 95 95 101 75 108 106 102 102 38 . . .|#
;;;;;

(defun NORMALIZE-COSINES (cosines min max low high)
  "Returns a list of normalized outputs of the cosine-function 1/cos x2."
  (if (null cosines)()
      (cons (normalize min max (first cosines) low high)
            (normalize-cosines (rest cosines) min max low high))))

;;;;;
#|(normalize -25.13146272476594 3.3267624923286805 -1.9036873597561845 24 108)
  93|#
;;;;;

(defun NORMALIZE (low1 high1 number low2 high2)
  "This function normalizes number into low-high-1 range to the low-high-2 range."
  (round (+ (* (/ (- number low1)(- high1 low1))(- high2 low2)) low2)))

;;;;;
#|The following functions make events for performing the above cosine function
as in (make-events (cosine 20 24 108))
   ((0 104 1000 1 90) (1000 94 1000 1 90) (2000 95 1000 1 90) . . . )|#
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
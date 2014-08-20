

                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;     Markov Function/Chapter 3       ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

#|(compose-new-music-based-on-markovian-probabilities 
 60 5 
 '((0 60 1000 1 127) (0 60 1000 1 127) (1000 62 1000 1 127) (2000 64 1000 1 127) 
   (3000 65 1000 1 127) (4000 67 1000 1 127) (5000 65 1000 1 127) (6000 69 1000 1 127) 
   (7000 71 1000 1 127) (8000 60 1000 1 127)))
((0 62 250 1 127) (250 64 250 1 127) (500 65 250 1 127) (750 67 250 1 127) (1000 65 250 1 127)) |#

;;;;
#|The Markov composing code creates new music based on an Markovian analysis 
of music events. The analytical code for markov chains takes these events 
and produces a logical state transition matrix (*stm*) from the succession orders 
of their pitches. This version is quite simple and uses only the diatonic c-major
scale as data; however, with little modification, this code can analyze using any
diatonic or chromatic input (mainly creating an appropriate *stm*)|#
;;;;

;;this is the way events should look

(defVar *EVENTS* '((0 60 1000 1 127)(0 60 1000 1 127)(1000 62 1000 1 127)
                     (2000 64 1000 1 127)(3000 65 1000 1 127)(4000 67 1000 1 127)
                     (5000 65 1000 1 127)(6000 69 1000 1 127)(7000 71 1000 1 127)
                     (8000 72 1000 1 127)(9000 60 1000 1 127)) "Sample events.")

;;an empty stm with all of the notes of the above events present
(defVar *STM* '((60 ())
                (62 ())
                (64 ())
                (65 ())
                (67 ())
                (69 ())
                (71 ())
                (72 ())) "a blank stm")

;;;;;
#|(compose-new-music-based-on-markovian-probabilities 
 60 5 
 '((0 60 1000 1 127) (0 60 1000 1 127) (1000 62 1000 1 127) (2000 64 1000 1 127) 
   (3000 65 1000 1 127) (4000 67 1000 1 127) (5000 65 1000 1 127) (6000 69 1000 1 127) 
   (7000 71 1000 1 127) (8000 60 1000 1 127)))
((0 62 250 1 127) (250 64 250 1 127) (500 65 250 1 127) (750 67 250 1 127) (1000 65 250 1 127)) |#
;;;;;

(defun COMPOSE-NEW-MUSIC-BASED-ON-MARKOVIAN-PROBABILITIES (start length events)
  "Top-level composing function for Markov."
  (clear-matrix)
  (setq *stm* (put-probabilities-into-stm (get-pitches events) *stm*))    ;this creates the new set of probabilities
  (compose-markov start length))


;;;;;
#|(put-probabilities-into-stm (get-pitches *events*) *stm*)
   ((60 (62 60)) (62 (64)) (64 (65)) (65 nil) (67 nil) (69 nil) (71 nil) (72 nil))|#
;;;;;

(defun PUT-PROBABILITIES-INTO-STM (pitches stm)     ;all of the pitches (not events) and the raw stm
 "Runs through the pitches (created by above) and returns a revised stm."
 (if (null (rest pitches)) stm                      ;end of recursion
     (progn (setq stm (put-probabilities (firstn 2 pitches) stm)) ;collect revised stm based two pitches at a time
            (put-probabilities-into-stm (rest pitches) stm))))  ;recurse with rest of pitches

;;;;;
#|(put-probabilities (60 60) ((60 nil) (62 nil) (64 nil) (65 nil) (67 nil) (69 nil) (71 nil) (72 nil))) 
   ((60 (60)) (62 nil) (64 nil) (65 nil) (67 nil) (69 nil) (71 nil) (72 nil))|#
;;;;;

(defun PUT-PROBABILITIES (two-pitches stm)    ;only two pitches in first arg - see above
  "Puts the probs for one pair of notes in the stm."
 (cond ((null stm)())                         ;when to quit
       ((equal (first two-pitches) (first (first stm)))  ;is the first pitchhte current level of stm?
        (cons (list (first (first stm))                  ;if yes, then add second pitch to level
                    (cons (second two-pitches)(second (first stm))))
              (put-probabilities two-pitches (rest stm))))
       (t (cons (first stm)                              ;else collect it and go on
                (put-probabilities two-pitches (rest stm))))))

;;;;;use as top-level when the analysis code has not been loaded!!!
#|(compose-markov 62 5)
   ((0 64 250 1 127) (250 62 250 1 127) (500 64 250 1 127) (750 62 250 1 127) (1000 64 250 1 127))|#
;;;;;

(defun COMPOSE-MARKOV (start length)
  "Composing function when *stm* is filled - start must be a pitch available in the *stm*."
  (make-events (compose-m start length))) 

;;;;;
#|(compose-m 62 5)
   ((0 64 250 1 127) (250 62 250 1 127) (500 64 250 1 127) (750 62 250 1 127) (1000 64 250 1 127))|#
;;;;;

(defun COMPOSE-M (start length) 
  "Composing function when *stm* is filled - start must be a pitch available in the *stm*."
  (if (= length 0)()
      (let ((test (choose-one (second (assoc start *stm*)))))
        (cons test
              (compose-m test (- length 1))))))

;;;;;
#|(make-events '(60 62 64 65 67)) 
   ((0 60 250 1 127) (250 62 250 1 127) (500 64 250 1 127) (750 65 250 1 127) (1000 67 250 1 127))|#
;;;;;

(defun MAKE-EVENTS (list-of-pitches &optional (time 0)) 
  "Makes the events in temporal order for compose-markov."
  (if (null list-of-pitches)() 
      (cons (list time
                  (first list-of-pitches)
                  250
                  1
                  127)
            (make-events (rest list-of-pitches) (+ time 250))))) 

;;;;;
#|(choose-one '(1 2 3 4 5))
3
? (choose-one '(1 2 3 4 5))
1|#
;;;;;

(defVar *RS* (make-random-state t) "Variable for storing the current random state.") 

(defun CHOOSE-ONE (list)
  "Chooses a random member of its argument."
  (nth (random (length list) *rs*)
       list))

;;;;;
#|(firstn 3 '(1 2 3 4 5))
   (1 2 3)|#
;;;;;

(defun FIRSTN (number list)
  "Returns the first n number of the list arg."
  (if (< (length list) number)(firstn (1- number) list)
      (butlast list (- (length list) number))))

;;;;;
#|(get-pitches *events*)
   (60 60 62 64 65)|#
;;;;;

(defun GET-PITCHES (events)
  "Extracts pitches from events."
 (if (null events)()
     (cons (second (first events))
           (get-pitches (rest events)))))

;;;;;
#|(clear-matrix)
   ((60 nil) (62 nil) (64 nil) (65 nil) (67 nil) (69 nil) (71 nil) (72 nil))|#
;;;;;

(defun CLEAR-MATRIX ()
  "Returns an empty matrix for this simple program."
  (setq *stm* '((60 ())
                (62 ())
                (64 ())
                (65 ())
                (67 ())
                (69 ())
                (71 ())
                (72 ()))))
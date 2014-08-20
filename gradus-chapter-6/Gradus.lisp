
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;      Gradus Function/Chapter 6      ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;      simple code to run gradus      ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

#|(gradus)|#

;;;  a Simple Music Learning program with instructions following
;;;;
#|
This program should run on any complete implementation of Common Lisp.
Creator: David Cope 
Any problems encountered please contact David Cope at howell@ucsc.edu.

To run this program, boot the Common Lisp of your choice and either load from the Load 
menu, or copy this code into an appropriate interpreter window and evaluate.

Note that the default values given to *illegal-verticals*, *illegal-parallel-motions*,
*direct-fifths-and-octaves*, and *illegal-double-skips* below are just that - 
default values. When the program runs with *auto-goals* set to t then the program
creates the values for these variables automatically as specified in my article 
related to this file: "A Musical Learning Algorithm" in Computer Music Journal.
|#
;;;;

;;;settings for Macintosh Common Lisp only (to prepare preferred visual (cosmetic) environment).
(progn 
  (setq *save-doc-strings* t)
  (setq *arglist-on-space* t)
  (setq *save-local-symbols* t)
  (setq *print-case* :downcase)
  (setq *paste-with-styles* ())
  (setq *fasl-save-local-symbols* t)
  (setq *save-definitions* t)
  (setq *break-on-errors* ())
  (setq *trace-print-length* 100)
  (setq *trace-print-level* 20)
  (setq *print-pretty* t))

(defVar *MAJOR-SCALE* '(36 38 40 41 43 45 47 48 50 52 53 55 
                        57 59 60 62 64 65 67 69 71 72 74 76 
                        77 79 81 83 84 86 88 89 91 93 95 96) "Major scale")
(defVar *ILLEGAL-VERTICALS* '(0 1 2 5 6 10 11 13 14 17 18 22 23 25 26 29 30 34 35 -1 -2 -3 -4 -5 -6 -7 -8) "Illegal verticals")
(defVar *ILLEGAL-PARALLEL-MOTIONS* '((7 7)(12 12)(19 19)(24 24)) "Illegal parallel motions")
(defVar *ILLEGAL-DOUBLE-SKIPS* '((3 3)(3 4)(3 -3)(3 -4)(-3 -3)(-3 -4)(-3 3)(-3 4)
                                 (4 3)(4 4)(4 -3)(4 -4)(-4 -3)(-4 -4)(-4 3)(-4 4)) "Illegal double skips")
(defVar *DIRECT-FIFTHS-AND-OCTAVES* '((9 7)(8 7)(21 19)(20 19)) "Direct fifths and octaves")
(defVar *SOLUTION* () "Where the initial solution is stored")
(defVar *COUNTERPOINT* () "The resulting counterpoint is stored here.")
(defVar *SAVE-VOICES* () "Pitches only of the resultant counterpoint are stored here.")
(defVar *RULES* () "Where the program stores its rules")
(defVar *SEED-NOTE* 60 "The note which produces the possibilities for the opening note ")
(defVar *SEED-NOTES* '(64 62 59 57 55 60) "Must start with a reasonable seed note for the program to work")
(defVar *BACKTRACK* () "Sets the backtrack printout routine.")
(defVar *CANTUS-FIRMUS* '(69 71 72 76 74 72 74 72 71 69) "The cantus firmus")
(defVar *NEW-LINE* () "Shere the new line is stored")
(defVar *RS* (make-random-state t) "Variable for storing the current random state.")
(defVar *SAVE-RULES* () "Variable for storing rules during the inference process.")
(defVar *PRINT-STATE* t "Variable for setting whether or not the Listener window prints the various steps of composition.")
(defVar *AUTO-GOALS* () "Variable for having the program create its own goals.")
(defVar *SAVED-TEMPLATES* () "Where the templates for getting successful seeds are stored.")
(setq c1 36 d1 38 e1 40 f1 41 g1 43 a1 45 b1 47 c2 48 d2 50 e2 52 f2 53 g2 55 a2 57 b2 59 c3 60
 d3 62 e3 64 f3 65 g3 67 a3 69 b3 71 c4 72 d4 74 e4 76 f4 77 g4 79 a4 81 b4 83 c5 84 d5 86
 e5 88 f5 89 g5 91 a5 93 b5 95 c5 96)
(defVar *LIST-OF-NOTES* '(c1 d1 e1 f1 g1 a1 b1 c2 d2 e2 f2 g2 a2 b2 c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5 d5
 e5 f5 g5 a5 b5 c5) "Notenames and registers for translating MIDI numbers into notenames.")
(defVar *LOOK-AHEAD* () "Indicates whether the program has looked ahead or not.")
(defVar *TEMPORARY-RULES* ())
(defVar *LAST-CANTUS-FIRMUS* ())
(defVar *PAST-MODEL-LENGTH* ())
(defVar *MODELS* '
(((72 71 74 72 71 69 67 69) (64 67 65 64 62 65 64 60))
 ((72 71 74 72 71 69 67 69) (57 55 53 57 55 53 55 53))
 ((72 71 74 72 71 69 67 69) (57 55 53 52 50 53 52 48))
 ((72 71 74 72 71 69 67 69) (64 67 65 64 67 65 64 60))
 ((69 71 72 69 71 72 74 77 76 74 72) (57 55 52 53 55 57 55 57 55 59 57))
 ((69 71 72 69 71 72 74 77 76 74 72) (57 55 52 53 55 57 55 53 55 53 52))
 ((69 71 72 69 71 72 74 77 76 74 72) (57 55 52 53 55 57 55 53 55 59 57))
 ((69 71 72 69 71 72 74 77 76 74 72) (57 55 52 53 55 57 55 57 60 59 60))
 ((69 71 72 69 71 72 74 77 76 74 72) (57 55 52 53 55 57 55 57 60 59 57))
 ((72 71 69 67 69 72 71 72) (64 62 60 64 62 60 62 64))
 ((72 71 69 67 69 72 71 72) (64 62 65 64 65 64 67 65))
 ((72 71 69 67 69 72 71 72) (57 59 60 64 62 60 62 64))
 ((72 71 69 67 69 72 71 72) (57 55 53 55 53 52 50 48))
 ((72 71 69 67 69 72 71 72) (64 62 65 64 65 64 62 64))
 ((72 71 69 67 69 72 71 72) (64 67 65 64 62 60 62 64))
 ((72 71 69 67 69 72 71 72) (57 59 60 64 62 64 67 65))
 ((72 71 69 67 69 72 71 72) (57 55 53 55 53 52 55 53))
 ((72 71 69 67 69 72 71 72) (64 62 65 64 62 60 62 60))
 ((72 71 69 67 69 72 71 72) (64 62 60 64 62 64 67 65))
 ((72 71 69 67 69 72 71 72) (64 67 65 64 62 64 67 65))
 ((72 71 69 67 69 72 71 72) (57 55 53 55 53 52 50 52))
 ((72 71 69 67 69 72 71 72) (64 67 65 64 62 64 62 60))
 ((72 71 69 67 69 72 71 72) (64 67 65 64 62 60 62 60))
 ((72 71 69 67 69 72 71 72) (64 62 60 64 62 64 62 60))
 ((69 71 72 76 74 72 71 72 74 72) (57 55 57 55 53 57 55 57 55 57))
 ((69 71 72 76 74 72 71 72 74 72) (57 55 57 55 53 57 55 52 53 57))
 ((69 71 72 76 74 72 71 72 74 72) (57 55 57 55 53 57 55 53 50 52))
 ((69 71 72 76 74 72 71 72 74 72) (57 55 57 55 59 57 59 57 59 57))
 ((69 71 72 76 74 72 71 72 74 72) (57 55 57 55 59 57 59 57 55 52))
 ((69 71 72 76 74 72 71 72 74 72) (57 55 53 52 53 52 50 48 47 45))
 ((69 71 72 76 74 72 71 72 74 72) (57 55 57 55 53 52 55 53 50 52))
 ((69 71 69 72 71 74 72 71 69) (57 55 53 52 55 53 57 55 57))
 ((69 71 69 72 71 74 72 71 69) (57 55 53 52 55 53 57 55 53))
 ((69 71 69 72 71 74 72 71 69) (57 55 53 52 55 53 52 50 53))
 ((69 71 72 76 74 72 74 72 71 69) (57 55 53 55 53 57 55 57 59 60))
 ((69 71 72 76 74 72 74 72 71 69) (57 55 57 55 53 52 50 52 50 53))
 ((69 71 72 76 74 72 74 72 71 69) (57 55 53 55 53 57 55 57 59 62))
 ((69 71 72 76 74 72 74 72 71 69) (57 55 53 52 53 52 53 57 55 57))
 ((69 71 72 76 74 72 74 72 71 69) (57 55 53 55 59 57 55 57 59 62))
 ((69 71 72 76 74 72 74 72 71 69) (57 55 53 52 53 52 50 52 55 53))
 ((69 71 72 76 74 72 74 72 71 69) (57 55 57 55 53 52 50 52 55 53))
 ((69 71 72 76 74 72 74 72 71 69) (57 55 53 52 53 57 55 57 55 57))
 ((69 71 72 76 74 72 74 72 71 69) (57 55 53 55 53 57 55 57 55 53))
 ((69 71 72 76 74 72 74 72 71 69) (57 55 53 55 59 57 55 57 55 53))
 ((69 71 72 76 74 72 74 72 71 69) (57 55 57 55 59 57 59 60 62 65))
 ((69 71 72 76 74 72 74 72 71 69) (57 55 57 55 59 57 59 60 62 60))
 ((69 71 69 72 71 74 72 71 69) (57 55 53 52 55 53 52 55 53))
 ((69 71 72 76 74 72 71 72 74 72) (57 55 57 55 59 57 55 53 50 52))
 ((69 71 72 74 71 72 74 72) (57 55 57 53 55 53 50 52))
 ((69 71 72 69 71 72 74 77 76 74 72) (57 55 52 53 55 57 55 53 55 53 57)))
  "Where the models for the goals are stored.")

#| 
the default:
(setq *cantus-firmus* '(69 71 72 76 74 72 74 72 71 69))
some other logical cantus firmus possibilities
(setq *cantus-firmus* '(69 71 69 72 71 74 72 71 69))
(setq *cantus-firmus* '(69 71 72 76 74 72 71 72 74 72))
(setq *cantus-firmus* '(69 71 72 71 72 74 72))
(setq *cantus-firmus* '(72 71 69 71 69 72 71 72))
(setq *cantus-firmus* '(69 71 72 69 71 72 74 72 76 74 72))
(setq *cantus-firmus* '(72 71 74 72 71 69 71 69))
example of a problem cantus firmus
(setq *cantus-firmus* '(72 71 69 71 72 76 74 72))
|#

;;;;;
#|(gradus)
  working.....
  ((a3 b3 c4 e4 d4 c4 d4 c4 b3 a3) (a2))
  working.....|#
;;;;;

(defun GRADUS (&key (auto-goals *auto-goals*)                                ;;;decide autonomous goals
                     (print-state *print-state*)                              ;;;print out "working" + voices
                     (seed-note nil)                                          ;;;decide new seed note
                     (cantus-firmus *cantus-firmus*))                         ;;;new cantus firmus
  "Top-level function of the counterpoint program."
  (unless (equal *last-cantus-firmus* *cantus-firmus*)
    (progn
      (setq *temporary-rules* ())(setq *last-cantus-firmus* *cantus-firmus*)))
   (if seed-note (setq *seed-note* seed-note)                                 ;;;create relevant seed-note
       (let ((test (select-new-seed-note *cantus-firmus* *major-scale* *saved-templates*)))
         (if test (setq *seed-note* test))))
   (setq *auto-goals* auto-goals)                                             ;;;create new goals
   (setq *print-state* print-state)
   (setq *cantus-firmus* cantus-firmus)                                       
   (if (null *auto-goals*)(set-default-goals))
   (if *auto-goals* (progn (set-goals *models*)(setq *auto-goals* ())(setq *past-model-length* (length *models*))))
   (if (not (equal (length *models*) *past-model-length*)) (set-goals *models*))
   (setq *past-model-length* (length *models*))
   (setq *new-line* ())                                                        ;;;erase previous attempt
   (setq *solution* 
         (create-new-line                                                      ;;;compose the new line
          *cantus-firmus* 
          *major-scale* 
          (mix (create-choices *major-scale* *seed-note*)) nil))               ;;;mix the choices for first note
   (setq *save-voices* (list (firstn (length *solution*) *cantus-firmus*)
                             *solution*))                                      ;;;show voices as lines
   (setq *save-voices* (mapcar #'translate-into-pitchnames *save-voices*))     ;;;change to pitch names
   (setq *counterpoint* (make-events (pair *save-voices*)))                    ;;;create the events for playing
   (if (equal (length *cantus-firmus*)(length (second *save-voices*)))
     (push (analyze-for-template *seed-note* *cantus-firmus* *major-scale*) 
           *saved-templates*))                                                   ;;;add template if successful
   *counterpoint*)     
                                                        ;;;return cpt for playing
;;;;;
#|(create-new-line
           '(69 71 72 76 74 72 74 72 71 69)
           '(36 38 40 41 43 45 47 48 50 52 53 55 57 59 60 62 64 65 67 69 71 72
            74 76 77 79 81 83 84 86 88 89 91 93 95 96)
           '(64 57 62 59)
           nil) 
  (57 55 53 55 53 57 55 57 59 62)|#
;;;;;

(defun CREATE-NEW-LINE (cantus-firmus scale choices last-notes &optional (length (length cantus-firmus)))
  "Creates a new line with the cantus firmus."
  (if (stop-if-all-possibilities-are-nil *seed-note* *cantus-firmus* *rules*)   ;;;stop if all seed notes fail
    (format t "~A~&" "I can find no solution for this cantus firmus.")
    (if (<= length 0) *new-line*                                                ;;;if length is zero, end
        (let ((test (evaluate-choices cantus-firmus choices last-notes)))       ;;;evaluate the possibilities
          (if (null test)                                                       ;;;if none are possible
            (progn                                                              ;;;create a new rule to avoid in future
              (if (null *look-ahead*)
                (pushnew (create-rule cantus-firmus (append last-notes (list (first choices)))) *rules* :test #'equal)
                (pushnew (create-rule cantus-firmus (append last-notes (list (first choices)))) *temporary-rules* :test #'equal))
              (progn (setq *save-rules* *rules*)                                ;;;infer the strongest rule
                     (if (not (< (length *rules*)(length *save-rules*)))
                       (print-backtracking)))                                   ;;;backtrack
              (let ((new-last-notes (get-new-starting-point last-notes)))
                (setf *new-line* (butlast *new-line* (- (length last-notes)(length new-last-notes))))
                (create-new-line cantus-firmus 
                                 scale 
                             (remove (my-last last-notes)
                                 (mix (create-choices 
                                       *major-scale* 
                                       (if (null new-last-notes) *seed-note* (my-last new-last-notes)))))
                                 new-last-notes
                                 (+ length (- (length last-notes)(length new-last-notes))))))
            (progn (setf *new-line* (append *new-line* (list test)))            ;;;recurse if choice works
                   (if *print-state* (print-working cantus-firmus *new-line*))
                   (create-new-line cantus-firmus 
                                    scale 
                                    (mix (create-choices *major-scale* test)) 
                                    (append last-notes (list test))
                                    (1- length))))))))

;;;;;
#|(get-new-starting-point '(57 55 53 52 55)) 
   (57 55 53 52)|#
;;;;;

(defun GET-NEW-STARTING-POINT (last-notes)
  "For backtracking - starts 2 earlier or nil"
  (cond ((<= (length last-notes) 1) ())
        (t (butlast last-notes 1))))

;;;;;
#|(evaluate-choices
           '(69 71 72 76 74 72 74 72 71 69)
           '(53 57 52 59)
           '(57 55 57 55 53 57 55))
   57|#
;;;;;

(defun EVALUATE-CHOICES (cantus-firmus choices last-notes)
  "runs the evaluate and look-ahead functions through the various choices."
  (let ((correct-choices (evaluate cantus-firmus choices last-notes)))
    (if correct-choices (setq *look-ahead* t)(setq *look-ahead* ()))
    (if (> (length correct-choices) 0)
      (look-ahead-for-best-choice cantus-firmus last-notes correct-choices)
      (first correct-choices))))

;;;;;
#|(evaluate '(69 71 72 76 74 72 74 72 71 69)
            '(48 52 47)
            '(57 55 57 55 53 52 50))
   (52)|#
;;;;;

(defun EVALUATE (cantus-firmus choices last-notes)
  "Evaluates the various choices for a next note based on the goals and current rules"
  (let ((choice (first choices)))
    (cond ((null choices)())
          ((and (not (consult-rules (create-rule cantus-firmus (append last-notes (list choice)))))
                (not (test-for-vertical-dissonance (nth (length last-notes) cantus-firmus) choice))
                (not (test-for-parallel-octaves-and-fifths (firstn (1+ (length last-notes)) cantus-firmus)
                                                           choice last-notes))
                (not (test-for-leaps (append last-notes (list choice))))
                (not (test-for-simultaneous-leaps (firstn (1+ (length last-notes)) cantus-firmus)
                                                  choice last-notes))
                (not (test-for-direct-fifths (firstn (1+ (length last-notes)) cantus-firmus)
                                             choice last-notes))
                (not (test-for-consecutive-motions (firstn (1+ (length last-notes)) cantus-firmus)
                                                   choice last-notes)))
           (cons choice (evaluate cantus-firmus (rest choices) last-notes)))
          (t (evaluate cantus-firmus (rest choices) last-notes)))))

;;;;;
#|(create-choices
           '(36 38 40 41 43 45 47 48 50 52 53 55 57 59 60 62 64 65 67 69 71 72
            74 76 77 79 81 83 84 86 88 89 91 93 95 96)
           60) 
   (62 64 59 57)|#
;;;;;

(defun CREATE-CHOICES (scale last-choice)
  "Creates four possible choices - seconds and thirds - from a previous pitch choice."
  (list (choose-from-scale last-choice 1 scale)
        (choose-from-scale last-choice 3 scale)
        (choose-from-scale last-choice -1 scale)
        (choose-from-scale last-choice -3 scale)))

;;;;;
#|(choose-from-scale
           60
           -3
           '(36 38 40 41 43 45 47 48 50 52 53 55 57 59 60 62 64 65 67 69 71 72
            74 76 77 79 81 83 84 86 88 89 91 93 95 96))
     57|#
;;;;;

(defun CHOOSE-FROM-SCALE (current-note interval-class scale)
  "Gets the appropriate pitch from the current scale based on the interval class."
  (if (plusp interval-class)
      (nth (get-diatonic-interval interval-class) (member current-note scale))
      (nth (abs (get-diatonic-interval interval-class)) (member current-note (reverse scale)))))

;;;;;
#|(get-diatonic-interval 3) 
   2|#
;;;;;

(defun GET-DIATONIC-INTERVAL (interval-class)
  "Translates interval-classes into diatonic-interval classes."
  (cond ((equal interval-class 1) 1)
        ((equal interval-class 2) 1)
        ((equal interval-class 3) 2)
        ((equal interval-class 4) 2)
        ((equal interval-class -1) -1)
        ((equal interval-class -2) -1)
        ((equal interval-class -3) -2)
        ((equal interval-class -4) -2)
        (t 1)))

;;;;;
#|(consult-rules '(-9 (2 -1 -1) (-1 -1 -2))) 
   nil|#
;;;;;

(defun CONSULT-RULES (rule)
  "Calling (consult-rules (-9 (2 -1 -1) (-1 2 -2))) 
    consult-rules returned nil"
  (or (member rule *rules* :test #'equal)
      (member rule *temporary-rules* :test #'equal)))

;;;;;
#|(create-rule '(69 71 72 76 74 72 74 72 71 69)
               '(57 55 53 52 53 57 55 57 55 57))
  (-11 (-1 -1 -1) (1 -1 1))|#
;;;;;

(defun CREATE-RULE (cantus-firmus new-notes)
  "Creates rules for the *rules* variable"
  (let ((the-list (the-last 4 new-notes)))
    (create-interval-rule
     (list (the-last (length the-list) 
                     (butlast cantus-firmus (- (length cantus-firmus)(length new-notes)))) the-list))))

;;;;;
#|(test-for-vertical-dissonance 69 55)
   55
  (test-for-vertical-dissonance 69 60) 
   nil|#
;;;;;

(defun TEST-FOR-VERTICAL-DISSONANCE (cantus-firmus-note choice)
  "Tests to ensure vertical dissonance"
  (if (member (- cantus-firmus-note choice) *illegal-verticals*) choice)) 

;;;;;
#|(test-for-parallel-octaves-and-fifths
           '(69 71 72 76 74 72)
           52
           '(57 55 53 52 53))
    nil|#
;;;;;

(defun TEST-FOR-PARALLEL-OCTAVES-AND-FIFTHS (cantus-firmus choice last-notes)
  "Tests for parallel octaves and fifths."
  (let ((cantus-firmus-to-here (firstn (1+ (length last-notes)) cantus-firmus)))
  (cond ((or (not (>= (length cantus-firmus-to-here) 2))(not (>= (length last-notes) 1))) ())
        ((member (list (abs (- (second-to-last cantus-firmus-to-here)(my-last last-notes)))
                       (abs (- (my-last cantus-firmus-to-here) choice)))
                      *illegal-parallel-motions* :test #'equal) t)
        (t nil))))

;;;;;
#|(test-for-leaps '(57 55 57 55 53 57 53)) 
   t|#
;;;;;

(defun TEST-FOR-LEAPS (extended-last-notes)
  "Tests for leaps and avoids two in row and ensures that leaps are followed by contrary motion steps."
  (cond ((not (>= (length extended-last-notes) 3)) ())
        ((member (list (- (second-to-last extended-last-notes)(my-last extended-last-notes))
                       (- (third-to-last extended-last-notes)(second-to-last extended-last-notes)))
                 *illegal-double-skips* :test #'equal) t)
        ((and (> (abs (- (third-to-last extended-last-notes)(second-to-last extended-last-notes))) 2)
              (not (opposite-sign (list (- (second-to-last extended-last-notes)(my-last extended-last-notes))
                                        (- (third-to-last extended-last-notes)(second-to-last extended-last-notes))))))
         t)
        (t ())))

;;;;;
#|(test-for-simultaneous-leaps
           '(69 71 72 76 74 72 74 72 71 69)
           60
           '(57 55 57 55 59 57 55 57 59)) 
   nil|#
;;;;;

(defun TEST-FOR-SIMULTANEOUS-LEAPS (cantus-firmus choice last-notes)
  "Tests for the presence of simultaneous leaps."
  (let ((cantus-firmus-to-here  (firstn (1+ (length last-notes)) cantus-firmus)))
  (cond ((or (not (>= (length cantus-firmus-to-here) 2))(not (>= (length last-notes) 1))) ())
        ((and (skipp (the-last 2 cantus-firmus-to-here))(skipp (the-last 2 (append last-notes (list choice))))) t)
        (t ()))))

;;;;;
#|(test-for-direct-fifths
           '(69 71 72 76 74 72 74 72 71 69)
           60
           '(57 55 57 55 53 57 55 57 59)) 
  nil|#
;;;;;

(defun TEST-FOR-DIRECT-FIFTHS (cantus-firmus choice last-notes)
  "Tests for direct fifths between the two lines."
  (let ((cantus-firmus-to-here  (firstn (1+ (length last-notes)) cantus-firmus)))
    (cond ((or (not (>= (length cantus-firmus-to-here) 2))(not (>= (length last-notes) 1))) ())
          ((member (get-verticals (the-last 2 cantus-firmus-to-here)(the-last 2 (append last-notes (list choice))))
                   *direct-fifths-and-octaves* :test #'equal) t)
          (t ()))))

;;;;;
#|(test-for-consecutive-motions
           '(69 71 72 76 74 72 74 72 71 69)
           65
           '(57 55 53 55 59 57 59 60 62)) 
  nil|#
;;;;;

(defun TEST-FOR-CONSECUTIVE-MOTIONS (cantus-firmus choice last-notes)
  "Tests to see if there are more than two consecutive save-direction motions."
  (let ((cantus-firmus-to-here  (firstn (1+ (length last-notes)) cantus-firmus)))
  (cond ((or (not (> (length cantus-firmus-to-here) 3))(not (> (length last-notes) 2))) ())
        ((let ((last-four-cf (the-last 4 cantus-firmus-to-here))
               (last-four-newline (the-last 4 (append last-notes (list choice)))))
           (not (or (opposite-sign (list (first (get-intervals (firstn 2 last-four-cf)))
                                         (first (get-intervals (firstn 2 last-four-newline)))))
                    (opposite-sign (list (first (get-intervals (firstn 2 (rest last-four-cf))))
                                         (first (get-intervals (firstn 2 (rest last-four-newline))))))
                    (opposite-sign (list (first (get-intervals (the-last 2 last-four-cf)))
                                         (first (get-intervals (the-last 2 last-four-newline)))))))) t)
        (t ()))))

;;;;;
#|(create-interval-rule '((74 72 71 69) (55 57 55 53))) 
  (-11 (-1 -1 -1) (1 -1 -1))|#
;;;;;

(defun CREATE-INTERVAL-RULE (rule)
  "Creates the interval rule as in (-7 (2 2 2)(-1 1 2))."
  (list (first (find-scale-intervals (list (first (first rule))
                                            (first (second rule)))
                                      *major-scale*))
        (find-scale-intervals (first rule) *major-scale*)
        (find-scale-intervals (second rule)  *major-scale*)))

;;;;;
#|(reduce-to-within-octave -17)
   -3|#
;;;;;

(defun REDUCE-TO-WITHIN-OCTAVE (interval)
  "Reduces diatonic intervals to within the octave."
  (cond ((and (> (abs interval) 7)(minusp interval))
         (reduce-to-within-octave (+ interval 7)))
        ((> (abs interval) 7)(- interval 7))
        ((zerop interval) -7)
        (t interval)))

;;;;;
#|(find-scale-intervals
           '(69 76)
           '(36 38 40 41 43 45 47 48 50 52 53 55 57 59 60 62 64 65 67 69 71 72
             74 76 77 79 81 83 84 86 88 89 91 93 95 96)) 
   (4)|#
;;;;;

(defun FIND-SCALE-INTERVALS (notes scale)
  "Returns the diatonic intervals between the notes according to the scale."
  (cond ((null (rest notes))())
        ((null (second notes)) 
         (cons nil (find-scale-intervals (rest notes) scale)))
        (t (cons (let ((first-note-test (member (first notes) scale :test #'equal))
                       (second-note-test (member (second notes) scale :test #'equal)))
                   (if (< (first notes)(second notes))
                     (length (butlast first-note-test (length second-note-test)))
                     (- (length (butlast second-note-test (length first-note-test))))))
                 (find-scale-intervals (rest notes) scale)))))

;;;;;
#|(find-scale-intervals
           '(69 76)
           '(36 38 40 41 43 45 47 48 50 52 53 55 57 59 60 62 64 65 67 69 71 72
             74 76 77 79 81 83 84 86 88 89 91 93 95 96)) 
   (4)|#
;;;;;

(defun LOOK-AHEAD-FOR-BEST-CHOICE (cantus-firmus last-notes correct-choices)
  "Looks ahead for the best choice"
  (cond ((null correct-choices) ())
        ((not (look-ahead 1 
                          cantus-firmus 
                          (append last-notes (list (first correct-choices)))
                          (create-rule cantus-firmus (append last-notes (list (first correct-choices))))
                          *rules*))
         (first correct-choices))
        (t (look-ahead-for-best-choice cantus-firmus last-notes (rest correct-choices)))))

;;;;;
#|(look-ahead 1
              '(69 71 72 76 74 72 74 72 71 69)
              '(62)
              '(-4 nil nil)
              '((-9 (-1 1 -1) (-1 -2 2)) (-9 (-1 -1 -1) (1 2 -1))
                (-12 (1 -1 -1) (-1 2 2)) (-11 (2 -1 -1) (-1 2 1))
                (-4 (1) (2)) (-4 (1 1) (-2 -1))
                (-9 (1 -1 -1) (-1 -2 -1)) (-7 (1 1 2) (-1 -2 -2))))
   t|#
;;;;;

(defun LOOK-AHEAD (amount cantus-firmus last-notes rule rules)
  "The top-level function for looking ahead."
  (match-rules-freely 
   (reduce-rule (make-freer-rule amount (find-scale-intervals (create-relevant-cf-notes last-notes cantus-firmus) *major-scale*) rule))
   rules))

;;;;;
#|(create-relevant-cf-notes
           '(57 55 57 55 53 57 55 57)
           '(69 71 72 76 74 72 74 72 71 69)) 
   (72 71)|#
;;;;;

(defun CREATE-RELEVANT-CF-NOTES (last-notes cantus-firmus)
  "Creates the set of forward reaching cf notes."
  (firstn 2 (nthcdr (1- (length last-notes)) cantus-firmus)))

;;;;;
#|(reduce-rule '(-11 (2 -1 -1 1) (-1 1 -1 nil))) 
   (-14 (-1 -1 1) (1 -1 nil))|#
;;;;;

(defun REDUCE-RULE (rule)
  "Reduces the front-end of the look-ahead rule."
  (if (<= (length (second rule)) 3) rule
      (let ((amount (- (length (second rule)) 3)))
        (cons (+ (first rule)(- (first (second rule)))(first (third rule)))
              (mapcar #'(lambda (x)(nthcdr amount x)) (rest rule))))))
    
;;;;;
#|(make-freer-rule 1 '(-1) '(-12 (1 -1 -1) (-1 1 2)))
   (-12 (1 -1 -1 -1) (-1 1 2 nil))|#
;;;;;

(defun MAKE-FREER-RULE (amount cf-notes rule)
  "Adds the appropriate number of nils to the new line for look-ahead matching."
  (if (zerop amount) rule
      (make-freer-rule (1- amount)
                       (rest cf-notes) 
                       (list (first rule)
                             (append (second rule)(list (first cf-notes)))
                             (append (third rule)(list nil))))))

;;;;;
#|(match-rules-freely
           '(-9 (-1 -1 nil) (1 1 nil))
           '((-9 (-1 1 -1) (-1 -2 2)) (-9 (-1 -1 -1) (1 2 -1))
             (-12 (1 -1 -1) (-1 2 2)) (-11 (2 -1 -1) (-1 2 1)) (-4 (1) (2))
             (-4 (1 1) (-2 -1)) (-9 (1 -1 -1) (-1 -2 -1))
             (-7 (1 1 2) (-1 -2 -2)))) 
   nil|#
;;;;;

(defun MATCH-RULES-FREELY (rule rules)
  "Runs the match-rule function through the rules."
  (cond ((null rules)())
        ((and (equal (first rule)(first (first rules)))
              (match-interval-rule (rest rule)(rest (first rules)))) t)
        ((and (equal (first rule)(first (first rules)))
              (equal (length (second rule))(length (second (first rules))))
              (match-rule rule (first rules))) t)
        (t (match-rules-freely rule (rest rules)))))

;;;;;
#|(match-interval-rule
           '((-1 -1 nil) (-1 -1 nil))
           '((-1 1 -1) (-1 -2 2))) 
   nil|#
;;;;;

(defun MATCH-INTERVAL-RULE (rule-for-matching rule)
  "Matches the freer rule to the rule from *rules*."
  (cond ((and (null (first rule-for-matching))(null (first rule))) t)
        ((or (and (equal (very-first rule-for-matching)(very-first rule))
                  (equal (very-second rule-for-matching)(very-second rule)))
             (and (equal (very-first rule-for-matching)(very-first rule))
                  (null (very-second rule-for-matching))))
         (match-interval-rule (mapcar #'rest rule-for-matching) (mapcar #'rest rule)))
        (t nil)))

;;;;;
#|(match-rule '(-12 (-1 -1 nil) (-1 -1 nil)) '(-12 (1 -1 -1) (-1 2 2))) 
   nil|#
;;;;;

(defun MATCH-RULE (rule-for-matching rule)
  "Matches the freer rule to the rule from *rules*."
  (cond ((and (null (first (rest rule-for-matching)))(null (first (rest rule)))) t)
        ((or (and (equal (very-first (rest rule-for-matching))(very-first (rest rule)))
                  (equal (very-second (rest rule-for-matching))(very-second (rest rule))))
             (and (equal (very-first (rest rule-for-matching))(very-first (rest rule)))
                  (null (very-second (rest rule-for-matching)))))
         (match-rule (cons (first rule-for-matching)(mapcar #'rest (rest rule-for-matching)))
                     (cons (first rule)(mapcar #'rest (rest rule)))))
        (t nil)))

;;;;;
#|(replenish-seed-notes)
   (60 65 64 62 59 57 55 53)|#
;;;;;

(defun REPLENISH-SEED-NOTES ()
  "Replenishes the seednotes when when they have all been used."
  (setq *seed-notes* '(60 65 64 62 59 57 55 53)))

;;;;;
#|(set-goals *models*)
   ((3 3) (3 4) (3 -3) (3 -4) (4 3) (4 4) (4 -3) (4 -4) (-3 3) (-3 4) (-3 -3)
    (-3 -4) (-4 3) (-4 4) (-4 -3) (-4 -4))|#
;;;;;

(defun SET-GOALS (models)
  "Sets the goals for the gradus program."
  (setf *illegal-verticals* (get-illegal-verticals models))
  (setf *illegal-parallel-motions* (find-illegal-parallels models))
  (setf *direct-fifths-and-octaves* (find-illegal-parallels models))
  (setf *illegal-double-skips* (possible-combinations '(3 4 -3 -4))))

;;;;;
#|(get-illegal-verticals
           '(((72 71 74 72 71 69 67 69) (64 67 65 64 62 65 64 60))
             ((72 71 74 72 71 69 67 69) (57 55 53 57 55 53 55 53)) . . . .
    (0 1 2 5 6 10 11 13 14 17 18 22 23 25 26 29 30 34 35 37 38)|#
;;;;;

(defun GET-ILLEGAL-VERTICALS (models)
  "Returns all of the vertical intervals NOT in the models."
  (get-complement (get-the-verticals models)))

;;;;;
#|(get-complement
           '(3 4 7 8 9 12 15 16 19 20 21 24 27 28 31 32 33 36 39)) 
  (0 1 2 5 6 10 11 13 14 17 18 22 23 25 26 29 30 34 35 37 38)|#
;;;;;

(defun GET-COMPLEMENT (verticals &optional (number 0))
  "Incrementally returns all of the intervals not in the verticals arg."
  (cond ((null verticals)())
        ((member number verticals)
         (get-complement (rest verticals)(1+ number)))
        (t (cons number (get-complement verticals (1+ number))))))
        
;;;;;
#|(get-the-verticals *models*)
   (3 4 7 8 9 12 15 16 19 20 21 24 27 28 31 32 33 36 39)|#
;;;;;

(defun GET-THE-VERTICALS (models)
  "Collects the vertical intervals from the models used."
  (my-sort #'< 
           (remove-duplicates 
            (project 
             (let ((voiced-music (pair (make-voices models))))
               (loop for pair in voiced-music
                     collect (- (first pair) (second pair))))) :test #'equal)))

;;;;;
#|(make-voices *models*)
  ((72 71 74 72 71 69 67 69 72 71 74 72 71 69 67 69 72 71 74 72 71 69 67 69 72
    71 74 72 71 69 67 69 69 . . . |#
;;;;;

(defun MAKE-VOICES (models)
  "Makes lists of the cantus firmus and accompanying line pitches."
  (list (apply #'append (mapcar #'first models))(apply #'append (mapcar #'second models))))

;;;;;
#|(project '(7))
   (7 19 31)|#
;;;;;

(defun PROJECT (numbers)
  ""
  (if (null numbers)()
      (append (pro (first numbers))
              (project (Rest numbers)))))

;;;;;
#|(pro 7)
   (7 19 31)|#
;;;;;

(defun PRO (number)
  "Projects octaves out from number."
  (if (> number 12)
    (list (- number 12) number (+ number 12))
    (list number (+ number 12)(+ number 24))))

;;;;;
#|(my-sort #'< '(3 4 2 6 1 4))
   (1 2 3 4 4 6)|#
;;;;;

(defun MY-SORT (function lists)
  "Non-destructively sorts its arg by function."
  (loop for item in (sort (loop for x in lists
                                collect (list x))  function :key #'car)
        collect (first item)))

;;;;;
#|(find-illegal-parallels *models*)
  ((24 24) (24 19) (24 16) (24 15) (24 12) (24 9) (24 8) (24 7) (24 4) (24 3)
   (21 19) (21 12) (21 9) (21 8) (21 7) (21 4) (21 3) (20 20) (20 19) (20 15)
   (20 12) (20 9) . . .|#
;;;;;

(defun FIND-ILLEGAL-PARALLELS (models)
  "Returns the non-used parallels in the models which are assumed to be illegal."
  (let* ((illegal-verticals (get-illegal-verticals models)) ;;;good!
         (legal-verticals (remove-illegal-verticals illegal-verticals (find-all-possible-motions 24)))
         (model-verticals (find-legals models)))
    (remove-legal-motions model-verticals legal-verticals)))

;;;;;
#|(remove-legal-motions
           '((8 4) (4 9) (9 8) (8 9) (9 4) (4 3) (3 9) (15 16) (16 21) (21 15)
            (15 16) . . . 
    ((24 24) (24 19) (24 16) (24 15) (24 12) (24 9) . . .|#
;;;;;

(defun REMOVE-LEGAL-MOTIONS (legal-motions motions)
  "Removes the legal motions from the motions arg."
  (cond ((null legal-motions) motions)
        ((member (first legal-motions) motions :test #'equal)
         (progn (setf motions (remove (first legal-motions) motions :test #'equal))
                (remove-legal-motions (rest legal-motions) motions)))
        (t (remove-legal-motions (rest legal-motions) motions))))

;;;;;
#|(find-legals '(((72 71 74 72 71 69 67 69) (64 67 65 64 62 65 64 60))
                 ((72 71 74 72 71 69 67 69) . . . 
   ((8 4) (4 9) (9 8) (8 9) (9 4) (4 3) (3 9) (15 16)
                       (16 21) . . . |#
;;;;;

(defun FIND-LEGALS (models)
  "Collects the legal motions in its arg."
  (if (null models)()
      (append (find-the-legals (pair (first models)))
              (find-legals (rest models)))))

;;;;;
#|(find-the-legals
           '((69 57) (71 55) (72 57) (74 53) (71 55) (72 53) (74 50) (72 52))) 
  ((12 16) (16 15) (15 21) (21 16) (16 19) (19 24) (24 20))|#
;;;;;

(defun FIND-THE-LEGALS (paired-model)
  "Discovers the legal motions in its arg."
  (if (null (rest paired-model))()
      (cons (list (- (first (first paired-model))(second (first paired-model)))
                  (- (first (second paired-model))(second (second paired-model))))
            (find-the-legals (rest paired-model)))))

;;;;;
#|(remove-illegal-verticals
           '(0 1 2 5 6 10 11 13 14 17 18 22 23 25 26 29 30 34 35 37 38)
           '((24 24) (24 23) . . .
   ((24 24) (24 21) (24 20) (24 19) (24 16) (24 15) (24 12) . . .|#
;;;;;

(defun REMOVE-ILLEGAL-VERTICALS (illegal-verticals all-verticals)
  "Removes the illegal verticals in its second arg."
  (cond ((null all-verticals) ())
        ((anyp illegal-verticals (first all-verticals))
         (remove-illegal-verticals illegal-verticals (rest all-verticals)))
        (t (cons (first all-verticals)
                 (remove-illegal-verticals illegal-verticals (rest all-verticals))))))

;;;;;
#|(find-all-possible-motions 24) 
  ((24 24) (24 23) (24 22) (24 21) (24 20)
   (24 19) (24 18) (24 17) (24 16) (24 15)
   (24 14)|#
;;;;;

(defun FIND-ALL-POSSIBLE-MOTIONS (extent &optional (value 0)(save-extent extent))
  "Returns all possible motions to its extent arg."
  (if (zerop extent)()
      (append (find-motions extent save-extent)
              (find-all-possible-motions (1- extent) value save-extent))))

;;;;;
#|(find-motions 2 24) 
 ((2 24) (2 23) (2 22) (2 21) (2 20) (2 19) (2 18)
  (2 17) (2 16) (2 15) (2 14) (2 13) (2 12) (2 11)
  (2 10) (2 9) (2 8) (2 7) (2 6) (2 5) (2 4) (2 3)
  (2 2) (2 1))|#
;;;;;

(defun FIND-MOTIONS (extent value)
  "Sub-function of find-all-possible-motions."
  (if (zerop value)()
      (cons (list extent value)
            (find-motions extent (1- value)))))
  
;;;;;
#|(anyp '(a b) '(a b c d))
   A|#
;;;;;

(defun ANYP (find-list target-list)
 "Returns any of first arg in second arg."
  (loop for find in find-list
        when (member find target-list :test #'equal)
        return find))

;;;;;
#|(possible-combinations '(3 4 -3 -4)) 
  ((3 3) (3 4) (3 -3) (3 -4) (4 3) (4 4) (4 -3)
   (4 -4) (-3 3) (-3 4) (-3 -3) (-3 -4) (-4 3)
   (-4 4) (-4 -3) (-4 -4))|#
;;;;;

(defun POSSIBLE-COMBINATIONS (list &optional (save-list list))
  "Returns all possible combinations of its list arg."
  (if (null list)()
      (append (combinations (first list) save-list)
              (possible-combinations (rest list) save-list))))

;;;;;
#|(combinations 3 '(3 4 -3 -4)) 
  ((3 3) (3 4) (3 -3) (3 -4))|#
;;;;;

(defun COMBINATIONS (object list)
  "A sub-function of possible-combinations."
  (if (null list)()
      (cons (list object (first list))
            (combinations object (rest list)))))

;;;;;
#|(analyze-for-template
           60
           '(69 71 72 76 74 72 74 72 71 69)
           '(36 38 40 41 43 45 47 48 50 52 53 55 57 59 60 62 64 65 67 69 71 72
             74 76 77 79 81 83 84 86 88 89 91 93 95 96)) 
  (-5 (4 0))|#
;;;;;

(defun ANALYZE-FOR-TEMPLATE (seed-note cantus-firmus scale)
  "Returns the complete template (seed interval and map) for saving."
  (list (first (find-scale-intervals (list (first cantus-firmus) seed-note) scale))
        (get-map cantus-firmus scale)))

;;;;;
#|(get-map '(69 71 72 76 74 72 74 72 71 69)
           '(36 38 40 41 43 45 47 48 50 52 53 55 57 59 60 62 64 65 67
             69 71 72 74 76 77 79 81 83 84 86 88 89 91 93 95 96)) 
  (4 0)|#
;;;;;

(defun GET-MAP (cantus-firmus scale)
  "Returns the map part of the template."
  (list (get-tessitura cantus-firmus scale)
        (first (find-scale-intervals (list (first cantus-firmus)(my-last cantus-firmus)) scale))))

;;;;;
#|(get-tessitura
           '(69 71 72 76 74 72 74 72 71 69)
           '(36 38 40 41 43 45 47 48 50 52 53 55 57 59 60 62 64 65 67 69 71 72
             74 76 77 79 81 83 84 86 88 89 91 93 95 96)) 
  4|#
;;;;;

(defun GET-TESSITURA (cantus-firmus scale)
  "Gets the tessitura or highest/lowest interval of a note list."
  (let ((up
         (abs (first (find-scale-intervals (list (first cantus-firmus)(apply #'max cantus-firmus)) scale))))
        (down
         (abs (first (find-scale-intervals (list (first cantus-firmus)(apply #'min cantus-firmus)) scale)))))
    (if (> up down) up (- down))))

;;;;;
#|(select-new-seed-note
           '(69 71 72 76 74 72 74 72 71 69)
           '(36 38 40 41 43 45 47 48 50 52 53 55 57 59 60 62 64 65 67 69 71 72
             74 76 77 79 81 83 84 86 88 89 91 93 95 96)
           '((-5 (4 0)) (-5 (4 0)) . . .
  60|#
;;;;;

(defun SELECT-NEW-SEED-NOTE (cantus-firmus scale saved-templates)
  "Select a logical new seed note."
  (get-diatonic-note (first cantus-firmus)
                     (first 
                      (second 
                       (first 
                        (sortcar #'> 
                                 (return-counts (collect-all (get-map cantus-firmus scale) saved-templates))))))
                     scale))

;;;;;
#|(collect-all '(4 0)
               '((-5 (4 0)) . . .
  ((-5 (4 0)) . . .|#
;;;;;

(defun COLLECT-ALL (map saved-templates)
  "Collects all of the occurances of each member of its arg."
  (cond ((null saved-templates)())
        ((equal map (second (first saved-templates)))
         (cons (first saved-templates)
               (collect-all map (rest saved-templates))))
        (t (collect-all map (rest saved-templates)))))
  
;;;;;
#|(return-counts '((-5 (4 0))))
   ((1 (-5 (4 0))))|#
;;;;;

(defun RETURN-COUNTS (templates)
  "Simply adds the count of occurances to the beginning of each member of its arg."
  (if (null templates)()
      (cons (list (count (first templates) templates :test #'equal)(first templates))
            (return-counts (remove (first templates) templates :test #'equal)))))

;;;;;
#|(sortcar #'< '((1000 52 1000 1 90)(0 60 1000 1 90)(0 62 1000 2 90)(1000 54 1000 2 90)))
   ((0 60 1000 1 90) (0 62 1000 2 90) (1000 52 1000 1 90) (1000 54 1000 2 90))|#
;;;;;

(defun SORTCAR (function lists)
  "Sorts by the first element."
  (sort (copy-tree lists) function :key 'first))

;;;;;
#|(get-diatonic-note
           69
           -5
           '(36 38 40 41 43 45 47 48 50 52 53 55 57 59 60 62 64 65 67 69 71 72
             74 76 77 79 81 83 84 86 88 89 91 93 95 96)) 
   60|#
;;;;;

(defun GET-DIATONIC-NOTE (current-note interval scale)
  "A simple variant of choose-from-scale which uses a diatonic interval as its second arg."
  (cond ((null interval)())
        ((plusp interval)(nth interval (member current-note scale)))
        (t (nth (abs interval) (member current-note (reverse scale))))))

;;;;;
#|(make-events '((60 62 64)(52 54 55)))
   ((0 60 1000 1 90) (0 62 1000 2 90) (1000 52 1000 1 90) (1000 54 1000 2 90))|#
;;;;;

(defun MAKE-EVENTS (pitch-groupings &optional (ontime 0))
  "Makes consecutive events out of the pairs of pitches in its arg."
  (if (null pitch-groupings) ()
      (append (list (make-event ontime (first (first pitch-groupings)) 1)
                    (make-event ontime (second (first pitch-groupings)) 2))
              (make-events (rest pitch-groupings)(+ ontime 1000)))))

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

;;;;;
#|? (choose-one '(1 2 3))
   3
  ? (choose-one '(1 2 3))
   2|#
;;;;;

(defun CHOOSE-ONE (list)
  "Chooses one its arg randomly."
  (nth (random (length list) *rs*) list))

;;;;;
#|(mix '(1 2 3))
   (1 3 2)|#
;;;;;

(defun MIX (list)
  "Mixes its arg arbitrarily"
  (let ((choice ()))
    (loop until (null list)
          do (setf choice (choose-one list))
          collect choice 
          do (setf list (remove choice list :count 1)))))
;;;;;
#|(my-last '(1 2 3))
   3|#
;;;;;

(defun MY-LAST (list)
  "Returns th atom last of the list."
  (first (last list)))

;;;;;
#|(firstn 2 '(1 2 3 4 5))
   (1 2)|#
;;;;;

(defun FIRSTN (number list)
  "Returns the first n of is list arg."
 (if (< (length list) number)(firstn (1- number) list)
     (butlast list (- (length list) number))))
;;;;;
#|(skipp '(60 64))
  t|#
;;;;;

(defun SKIPP (notes)
  "Returns true if its two-number arg is a skip."
  (if (> (abs (- (second notes)(first notes))) 2) t))

;;;;;
#|(get-verticals '(60 62 64) '(52 54 55))
   (8 8 9)|#
;;;;;

(defun GET-VERTICALS (cantus-firmus new-line)
  "Returns the intervals between two lines of counterpoint."
  (if (null cantus-firmus)()
      (cons (- (first cantus-firmus)(first new-line))
            (get-verticals (rest cantus-firmus)(rest new-line)))))

;;;;;
#|(get-intervals '(60 62 64))
   (2 2)|#
;;;;;

(defun GET-INTERVALS (notes)
  "Returns a list of intervals one short of its pitch-list arg."
  (if (null (rest notes))()
      (cons (- (second notes)(first notes))
            (get-intervals (rest notes)))))

;;;;;
#|(opposite-sign '(4 -2))
  t|#
;;;;;

(defun OPPOSITE-SIGN (numbers)
  "returns t if the two numbers have opposite signs."
  (if (or (and (minusp (first numbers))(plusp (second numbers)))
          (and (plusp (first numbers))(minusp (second numbers)))) t))

;;;;;
#|(second-to-last '(1 2 3 4 5))
   4|#
;;;;;

(defun SECOND-TO-LAST (list)
  "Returns the second to last of the list arg."
  (my-last (butlast list)))

;;;;;
#|(third-to-last '(1 2 3 4 5))
   3|#
;;;;;

(defun THIRD-TO-LAST (list)
  "Returns the third to last of the list arg."
  (nth (- (length list) 3) (butlast list)))

;;;;;
#|(pair '((1 2 3)(4 5 6)))
   ((1 4) (2 5) (3 6))|#
;;;;;

(defun PAIR (voices)
  "Pairs the two lists."
  (if (null (first voices))()
      (cons (list (first (first voices))(first (second voices)))
            (pair (list (rest (first voices))(rest (second voices)))))))

;;;;;
#|(print-backtracking)
  backtracking.....there are now
  2
  rules.
   print-backtracking returned nil|#
;;;;;

(defun PRINT-BACKTRACKING ()
  "Simple printing function to show backtracking."
  (format t "~&~A~&~A~&~A~&" "backtracking.....there are now" (length *rules*) "rules."))

;;;;;
#|(print-working
           '(69 71 72 76 74 72 74 72 71 69)
           '(57 55 57 55 53 57 55 57 55)) 
  working.....
  ((a3 b3 c4 e4 d4 c4 d4 c4 b3 a3) (a2 g2 a2 g2 f2 a2 g2 a2 g2))
   print-working returned nil|#
;;;;;

(defun PRINT-WORKING (cantus-firmus last-notes)
  "simple printing function for continuing to compose"
  (format t "~&~A~&~A~&" "working....." (list (translate-into-pitchnames cantus-firmus)(translate-into-pitchnames last-notes))))

;;;;;
#|(the-last 3 '(1 2 3 4 5))
(3 4 5)|#
;;;;;

(defun THE-LAST (n list)
  "Returns the last n of list."
  (if (< (length list) n) list
      (nthcdr (- (length list) n) list)))

;;;;;
#|(very-first '((1 4) (2 5) (3 6)))
   1|#
;;;;;

(defun VERY-FIRST (list)
  "Returns the first of the first of list."
  (first (first list)))

;;;;;
#|(very-second '((1 4) (2 5) (3 6)))
    2|#
;;;;;

(defun VERY-SECOND (list)
  "Returns the first of the second of list."
  (first (second list)))

;;;;;
#|(set-default-goals) 
  ((9 7) (8 7) (21 19) (20 19))|#
;;;;;

(defun SET-DEFAULT-GOALS ()
  "Sets the default goals for the program."
  (setq *illegal-verticals* '(0 1 2 5 6 10 11 13 14 17 18 22 23 25 26 29 30 34 35 -1 -2 -3 -4 -5 -6 -7 -8))
  (setq *illegal-parallel-motions* '((7 7)(12 12)(19 19)(24 24)))
  (setq *illegal-double-skips* '((3 3)(3 4)(3 -3)(3 -4)(-3 -3)(-3 -4)(-3 3)(-3 4)
                                 (4 3)(4 4)(4 -3)(4 -4)(-4 -3)(-4 -4)(-4 3)(-4 4)))
  (setq *direct-fifths-and-octaves* '((9 7)(8 7)(21 19)(20 19))))

;;;;;
#|(stop-if-all-possibilities-are-nil
           60
           '(69 71 72 76 74 72 74 72 71 69)
           '((-7 (1 1 2) (-1 -2 1)) (-9 (1 -1 -1) (-1 -2 2)) (-4 (1) (-1))
             (-4 (1 1) (-2 2)))) 
   nil|#
;;;;;

(defun STOP-IF-ALL-POSSIBILITIES-ARE-NIL (seed-note cantus-firmus rules)
  "For stopping if no solution exists."
  (check-for-nils 
   (mapcar #'(lambda (x)
               (reduce-to-within-octave 
                (first (find-scale-intervals (list (first cantus-firmus) x) 
                                             *major-scale*))))
           (create-choices *major-scale* seed-note)) rules))

;;;;;
#|(check-for-nils
           '(-4 -3 -6 -7)
           '((-7 (1 1 2) (-1 -2 1)) (-9 (1 -1 -1) (-1 -2 2)) (-4 (1) (-1))
             (-4 (1 1) (-2 2)))) 
   nil|#
;;;;;

(defun CHECK-FOR-NILS (choices rules)
  "Checking to see if all possible first notes produce rule-conflicting problems."
  (cond ((null choices) t)
        ((member (list (first choices)
                       nil nil) rules :test #'equal)
         (check-for-nils (rest choices) rules))
        (t nil)))

;;;;;
#|(translate-into-pitchnames '(69 71 72 76 74 72 74 72 71 69)) 
   (a3 b3 c4 e4 d4 c4 d4 c4 b3 a3)|#
;;;;;

(defun TRANSLATE-INTO-PITCHNAMES (list-of-midi-note-numbers)
  "Used to translate MIDI note numbers into note names."
  (if (null list-of-midi-note-numbers)()
      (cons (nth (position (first list-of-midi-note-numbers) *major-scale*) *list-of-notes*)
            (translate-into-pitchnames (rest list-of-midi-note-numbers)))))

;;;;;
#|(translate-rule-into-pitches 60 '(-9 (1 -1 -1) (-1 -2 2)))
   ((c3 d3 c3 b2) (a1 g1 e1 g1))|#
;;;;;

(defun TRANSLATE-RULE-INTO-PITCHES (first-note rule)
  "Translates rules into more readable pitch names."
  (list (translate-notes first-note (second rule))
        (translate-notes (get-diatonic-note first-note (first rule) *major-scale*)(third rule))))

;;;;;
#|(translate-notes 60 '(1 -1 -1)) 
   (c3 d3 c3 b2)|#
;;;;;

(defun TRANSLATE-NOTES (first-note intervals)
  "Translates interval lists into note names for readability."
  (if (null intervals)(translate-into-pitchnames (list first-note))
      (let ((test (get-diatonic-note first-note (first intervals) *major-scale*)))
        (append (translate-into-pitchnames (list first-note))
              (translate-notes test (rest intervals))))))

;;;;;
#|(evaluate-pitch-names '((a3 b3)(f3 g3)))
   ((69 71) (65 67))|#
;;;;;

(defun EVALUATE-PITCH-NAMES (voices)
  "Evaluates the pitch names of its arg into midi note numbers."
    (mapcar #'(lambda (x)(mapcar #'eval x)) voices))

#|
A Canon Maker....

(setq *illegal-verticals* 
'(0 1 2 5 6 7 10 11 13 14 17 18 19 22 23 25 26 29 30 34 35 -1 -2 -3 -4 -5 -6 -7 -8))

(setq *cantus-firmus* '(69 71 72 76 74 72 71))

(defun CREATE-CANON ()
  "Creates a simple canon in two voices using gradus."
  (setq *seed-note* (- (my-last *cantus-firmus*) 12))
  (gradus)
  (setq *save-voices* (evaluate-pitch-names *save-voices*))
  (let* ((theme (append *cantus-firmus* (mapcar #'(lambda (x)(+ x 12))(second *save-voices*))))
         (lower-voice (mapcar #'(lambda (x)(- x 12)) theme)))
    (make-events 
     (pair (list (append theme theme theme (make-list (length  *cantus-firmus*) :initial-element 0))
                 (append (make-list (length  *cantus-firmus*) :initial-element 0) lower-voice lower-voice lower-voice))))))

A more contemporary example

(setq *models* '
      (((72 71 74 72 71 69 67 69) 
        (71 69 67 65 69 67 65 62))
       ((72 71 74 72 71 69 67 69) 
        (65 64 60 62 64 67 65 67))
       ((69 71 69 72 71 74 72 71 69)
        (62 64 67 65 64 60 62 64 62))
       ))

(setq *seed-notes* '(67 64 59 57 55 62))
(setq *cantus-firmus* '(72 71 69 67 69 72 71 72))
(setq *auto-goals* nil)
(setq *illegal-double-skips* '())
(setq *illegal-parallel-motions* '())
(setq *illegal-verticals* '())
(setq *direct-fifths-and-octaves* '())
(setq *auto-goals* t)
|#




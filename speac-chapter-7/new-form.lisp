


                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;       SPEAC Function/Chapter 7      ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;    form define code to run SPEAC    ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;


;;;this follows the books directions excactly!!!!
;;;(GET-FORMS-FROM-ALL-DATABASES)
;;;then loads the music for the following analysis

#| the final function

? (make-form-connections *events*)
((2 3))
? (density *events*)
(25 2 4 37)
? (composite-rhythm *events*)
(82 39)
? (cadences *events*)
(10 18 30 41 51 60 68)

;;;important!!!!
(return-best-cadences (mapcar #'get-function (mapcar #'get-pitches (capture-beats 
(break-into-beats *events*) *beat*))))
((10000 C1) (18000 C1) (30000 A1) (41000 C1) (51000 A1) (60000 A1) (68000 C1))

(defun BREAK-INTO-PHRASES (events)
  (let ((form (make-form-connections events))           ;;;provides the overall form
        (phrases (compare-all events
                              (density events)          ;;;phrases
                              (composite-rhythm events) ;;;phrases
                              (cadences events))))      ;;;phrases
    (if phrases (list form phrases)(list form (approximation events)))))
          
(defun compare-all (events density composite-rhythm cadences)
  (let ((approximate-number-of-phrases (get-approximate-number-of-phrases events)))

(defun get-approximate-number-of-phrases (events)
  (let* ((last-event (my-last (sortcar #'< events)))
         (overall-duration (+ (first last-event)(third last-event))))
    (list (round (/ overall-duration (* *meter* 1000 8)))
          (round (/ overall-duration (* *meter* 1000 4))))))
|#


(defVar *LETTERS-USED* ())

#|(eval-combine-and-integrate-forms chopin-33-3)
((a 0 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) (a 24000 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) (b 59000)
 (a 96000 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) (a 120000 (2 -2 -2 -1 1 1 1 1 1 -2 -2)))|#

(defun EVAL-COMBINE-AND-INTEGRATE-FORMS (events)
  (setq *letters-used* ())
  (let ((cadences (cadences events))
        (density (density events))
        (composite-rhythm (composite-rhythm events))
        (patterns (almost-the-same-lists (simple-matcher events)))
        (minimum (round (/  (+ (first (my-last events))
                               (third (my-last events))) (* *meter* 1000 8))))
        (maximum (round (/  (+ (first (my-last events))
                               (third (my-last events))) (* *meter* 1000 4)))))
    (combine (append (evaluate-forms maximum minimum (list cadences density composite-rhythm))(list patterns)))))

;;;they have got to come out looking the same for the following to work
;;;they do (something TIME)
#|
these three survive:
(eval-combine-and-integrate-forms chopin-33-3)
(((a1 21000) (a1 39000) (c1 59000) (c1 77000) (c1 95000) (a1 117000) (a1 135000))

 ((58 1000) (24 58000) (7 82000) (55 89000))

 ((a 0 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) (a 24000 (2 -2 -2 -1 1 1 1 1 1 -2 -2))
  (a 96000 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) (a 120000 (2 -2 -2 -1 1 1 1 1 1 -2 -2))))
|#

(setq *meter* 3)

(defVar *LETTERS* '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(defun COMBINE (forms &optional (letters (remove-all (reverse (find-letters-used (my-last forms))) '(a b c d e f g h i j k l m n o p q r s t u v w x y z))))
  (sortcadr #'< (name-them (apply #'append (cons (my-last forms)
                                             (reduce-out-close-calls (my-last forms)(butlast forms)))) letters)))

(defun NAME-THEM (forms letters)
  (cond ((null forms)())
        ((member (very-first forms) *letters* :test #'equal)
         (cons (first forms)
               (name-them (rest forms) letters)))
        (t (cons (list (first letters) (second (first forms)))
                 (name-them (rest forms) letters)))))

(defun REDUCE-OUT-CLOSE-CALLS (pattern-discovery forms)
  (if (null forms) ()
      (cons (return-within-range pattern-discovery (first forms))
            (reduce-out-close-calls pattern-discovery (rest forms)))))
      
(defun FIND-LETTERS-USED (form)
  "Finds and returns the letters for form already used."
  (cond ((null form) *letters-used*)
        ((member (very-first form) '(a b c d e f g) :test #'equal)
         (progn (pushnew (very-first form) *letters-used* :test #'equal)
                (find-letters-used (rest form))))
        (t (find-letters-used (rest form)))))

#|(return-within-range '((a 0 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) (a 24000 (2 -2 -2 -1 1 1 1 1 1 -2 -2))
  (a 96000 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) (a 120000 (2 -2 -2 -1 1 1 1 1 1 -2 -2))) '((a1 21000) (a1 39000) (c1 59000) (c1 77000) (c1 95000) (a1 117000) (a1 135000)))
  ((c1 59000))|#

(defVar *MEASURES* 8)

(defun RETURN-WITHIN-RANGE (original other &optional (amount (* *meter* *beat* *measures*)))
  "Returns those elements not in the ranges covered by the existing forms."
  (if (null original) other
      (return-within-range (rest original) (within-range (- (second (first original)) amount)
                                                  (+ (second (first original)) amount)
                                                  other))))

#|(within-range 3 5 '((1 2) (2 3) (3 4) (4 5) (5 6) (6 7)))
((1 2) (5 6) (6 7))|#

(defun WITHIN-RANGE (min max lists)
  "Returns those not within min max."
  (cond ((null lists)())
        ((and (>= (second (first lists)) min)
              (<= (second (first lists)) max))
         (within-range min max (rest lists)))
        (t (cons (first lists)
                 (within-range min max (rest lists))))))

#|(evaluate-forms
           12
           6
           '(((c1 10000) (a1 21000) (c1 33000) (c1 45000) (a1 56000) (a1 67000)
             (c1 79000) (c1 90000) (c1 101000) (a1 117000) (c1 129000)
             (c1 141000))
           '((58 1000) (24 58000) (7 82000) (55 89000)) ((212 1000)))) 
  (((c1 10000) (a1 21000) (c1 33000) (c1 45000) (a1 56000)
    (a1 67000) (c1 79000) (c1 90000) (c1 101000)(a1 117000) (c1 129000) (c1 141000)))|#

(defun EVALUATE-FORMS (maximum minimum forms)
  "Evaluates the forms to only include relevant elements."
  (cond ((null forms)())
        ((and (<= (length (first forms)) maximum)
              (>= (length (first forms)) minimum))
         (cons (first forms)
               (evaluate-forms maximum minimum (rest forms))))
        (t (evaluate-forms maximum minimum (rest forms)))))

#|
(2)
(almost-the-same-lists (simple-matcher chopin-33-3))
((a 0 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) (a 24000 (2 -2 -2 -1 1 1 1 1 1 -2 -2))
 (a 96000 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) (a 120000 (2 -2 -2 -1 1 1 1 1 1 -2 -2)))|#

(defun ALMOST-THE-SAME-LISTS (lists &optional (types '(a b c d e f g h i j k)))
  "Combines lists of nearly the same content."
  (cond ((null lists)())
        ((not (numberp (first (first lists))))
         (cons (first lists)
               (almost-the-same-lists (rest lists) types)))
         (t (cons (cons (first types)(rest (first lists)))
               (almost-the-same-lists (collect-patterns (first lists)(rest lists)(first types))(rest types))))))


#|(collect-patterns
           '(4 0 (2 -2 -2 -1 1 1 1 1 1 -2 -2))
           '((4 24000 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) (4 96000 (2 -2 -2 -1 1 1 1 1 1 -2 -2))
             (4 120000 (2 -2 -2 -1 1 1 1 1 1 -2 -2)))
            'a)
((a 0 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) (a 24000 (2 -2 -2 -1 1 1 1 1 1 -2 -2))
 (a 96000 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) (a 120000 (2 -2 -2 -1 1 1 1 1 1 -2 -2)))|#

(defun COLLECT-PATTERNS (list lists type)
  "Pattern matches the patterns."
  (cond ((null lists)())
        ((pattern-match (third list) (third (first lists)) *intervals-off*)
         (cons (cons type (rest (first lists)))
               (collect-patterns list (rest lists) type)))
        (t (cons (first lists)
                 (collect-patterns list (rest lists) type)))))

;;;;;
#|
(composite-rhythm *events*)
(82 39)|#
;;;;;

(defun COMPOSITE-RHYTHM (events)
  "Runs the composite rhythm portion of the form program."
  (collect-differential (make-composite-rhythm events)))

;;;;;
#|
(make-composite-rhythm *events*)
(0 1000 1500 2000 2500 3000|#
;;;;;

(defun MAKE-COMPOSITE-RHYTHM (events)
  "Discovers the relevant ontimes."
  (remove-duplicates 
   (loop for event in events
         collect (first event))))
  
;;;;;
#|
(collect-differential (composite-rhythm *events*))
(82 39)|#
;;;;;

(defun COLLECT-DIFFERENTIAL (ontime-map &optional (previous ())(count 0)(time (first ontime-map)))
  "Collects the aggregates separated by large changes in ontimes."
  (cond ((null ontime-map) (list (list count time)))
        ((null previous)
         (collect-differential (rest ontime-map) (first ontime-map) (1+ count)))
        ((> (abs (- (first ontime-map) previous)) 1001)
         (cons (list count time) 
               (collect-differential ontime-map (first ontime-map) 0 (first ontime-map))))
        (t (collect-differential (rest ontime-map) (first ontime-map) (1+ count) time))))

;;;;;
#|
(density *events*)
(25 2 4 37)
|#
;;;;;

(defun DENSITY (events)
  "Returns phrase beat lengths determined by density or texture."
  (let ((beats (capture-beats (break-into-beats events) *beat*)))
    (collect-by-differences (map-density beats))))

;;;;;
#|
(map-density (((0 60 1000 4 96) (0 55 1000 3 96) (0 64 1000 2 96) (0 76 1000 1 96)) ((1000 53 1000 4 96) . . .
(4 5 5 4 5 4 4 . . . |#
;;;;;

(defun MAP-DENSITY (beats)
  "Maps the density for each beat."
  (loop for beat in beats
        collect 
(list (length beat) (very-first beat))))

;;;;;
#|
(collect-by-differences '(4 5 8 2 5 6 7))
(2 1 1 3)|#
;;;;;

(defun COLLECT-BY-DIFFERENCES (density-map &optional (previous ())(count 0)(time (second (first density-map))))
  "Collects the aggregates separated by large changes in density."
  (cond ((null density-map) (list (list count time)))
        ((null previous)
         (collect-by-differences (rest density-map) (first density-map) (1+ count)))
        ((> (abs (- (very-first density-map) (first previous))) 2)
         (cons (list count time)
               (collect-by-differences density-map (first density-map) 0)))
        (t (collect-by-differences (rest density-map) (first density-map) (1+ count) time))))
  
;;;the cadence sequence

(defVar *ANALYSIS-LEXICON* '(((24 36 48 60 72 84 96 108 27 28 39 40 51 52 63 64 75 76 87 88 99 100 31 43 55 67 79 91
   103)
  c1)
 ((29 41 53 65 77 89 101 33 93 105 81 45 57 69 24 36 48 60 72 84 96 108) p1)
 ((31 43 55 67 79 91 103 35 47 59 71 83 95 107 26 38 50 62 74 86 98 29 41 53 65 77 89 101)
  a1)
 ((35 47 59 71 83 95 107 26 38 50 62 74 86 98 29 41 53 65 77 89 101) a2)
 ((28 40 52 64 76 88 100 31 43 55 67 79 91 103 35 47 59 71 83 95 107) c4)
 ((26 38 50 62 74 86 98 89 101 77 65 29 41 53 93 105 81 69 33 45 57) p2)
 ((33 45 57 69 81 93 105 24 36 48 60 72 84 96 108 28 40 52 64 76 88 100) c2)
 ((26 38 50 62 74 86 98 30 33 42 54 66 78 90 102 45 57 69 81 93 105) s1)
 ((28 40 52 64 76 88 100 32 35 44 56 68 80 92 104 47 59 71 83 95 107) s3)
 ((33 45 57 69 81 93 105 25 37 49 61 73 85 97 28 40 52 64 76 88 100) e1)
 ((35 47 59 71 83 95 107 27 39 51 63 75 87 99 30 42 54 66 78 90 102) e3)
 ((24 36 48 60 72 84 96 108 28 31 34 40 52 64 76 88 100 43 55 67 79 91 103 46 58 70 82 94
   106)
  c3)
 ((25 37 49 61 73 85 97 28 31 34 40 52 64 76 88 100 43 55 67 79 91 103 46 58 70 82 94 106)
  e2)
 ((27 39 51 63 75 87 99 24 30 33 36 48 60 72 84 96 108 42 54 66 78 90 102 45 57 69 81 93
   105)
  e4)
 ((32 44 56 68 80 92 104 26 29 35 38 50 62 74 86 98 41 53 65 77 89 101 47 59 71 83 95 107)
  a3)
 ((32 44 56 68 80 92 104 24 30 36 48 60 72 84 96 108 42 54 66 78 90 102) p3)
 ((25 37 49 61 73 85 97 29 32 41 53 65 77 89 101 44 56 68 80 92 104) p4)
 ((30 42 54 66 78 90 102 25 37 49 61 73 85 97 34 46 58 70 82 94 106) s4)
 ((27 39 51 63 75 87 99 31 43 55 67 79 91 103 34 46 58 70 82 94 106) a4)
 ((34 46 58 70 82 94 106 26 38 50 62 74 86 98 29 41 53 65 77 89 101) s2))
 "this is the chord analysis list. Note that it is arranged with the root and its
octaves listed first so that the analysis program will analyze single notes and intervals
only correctly.")

;;;;;
#|? (setq *cadence-minimum* 10000)
10000
? (cadences chopin-33-3)
(11 23 35 47 59 71 83 95 107 119 131 143)
with (setq *cadence-minimum* 10000)
|#
;;;;;

(defun CADENCES (events)
  "Provides a cadence mapping  of the events."
  (mapcar #'reverse
          (return-best-cadences (mapcar #'get-function (mapcar #'get-pitches 
(capture-beats 
events *beat*)))))
)

;;;;;
#|(return-best-cadences '((0 C1) (1000 P2) (2000 C2) . . .
   ((10000 C1) (18000 C1) (30000 A1) (41000 C1) (51000 A1) (60000 A1) (68000 C1))|#
;;;;;

(defun SET-MINOR-FLAG (list flag)
  (if (member (second list)
              '(s3 a3 p4) :test #'equal) 4 
      (if (> flag 0)(1- flag) 0)))

(defVar *CADENCE-MINIMUM* 9000)

(defun RETURN-BEST-CADENCES (function-timing-lists &optional (distance 0)(previous nil)(minor-flag 0))
  "Returns the timing of the principal cadences."
  (cond ((null function-timing-lists)())
        ((and (> minor-flag 0)(equal (second (first function-timing-lists)) 'c2)(> distance *cadence-minimum*))
         (cons (list (first (first function-timing-lists)) 'c1)
               (return-best-cadences (rest function-timing-lists) 0 (first function-timing-lists) (set-minor-flag (first function-timing-lists) minor-flag))))
        ((and (> minor-flag 0)(equal (second (first function-timing-lists)) 'c4)(> distance *cadence-minimum*))
         (cons (list (first (first function-timing-lists)) 'a1)
               (return-best-cadences (rest function-timing-lists) 0 (first function-timing-lists) (set-minor-flag (first function-timing-lists) minor-flag))))
        ((and (equal previous 'a1)(equal (second (first function-timing-lists)) 'c1)(> distance *cadence-minimum*))
         (cons (first function-timing-lists)
               (return-best-cadences (rest function-timing-lists) 0 (first function-timing-lists)(set-minor-flag (first function-timing-lists) minor-flag))))
        ((and (> distance *cadence-minimum*) (equal (second (first function-timing-lists)) 'a1)
              (not (equal (second (second function-timing-lists)) 'c1)))
         (cons (first function-timing-lists)
               (return-best-cadences (rest function-timing-lists) 0 (first function-timing-lists)(set-minor-flag (first function-timing-lists) minor-flag))))
        ((and (or (equal (second (first function-timing-lists)) 'c1)
                  )
              (> distance *cadence-minimum*))
         (cons (first function-timing-lists)
               (return-best-cadences (rest function-timing-lists) 0 (first function-timing-lists)(set-minor-flag (first function-timing-lists) minor-flag))))
        ((and (equal (second (first function-timing-lists)) 'a1)
              (not (next4 (rest function-timing-lists)))
              (> distance *cadence-minimum*))
         (cons (first function-timing-lists)
               (return-best-cadences (rest function-timing-lists) 0 (first function-timing-lists)(set-minor-flag (first function-timing-lists) minor-flag))))
        (t (return-best-cadences (rest function-timing-lists) (+ distance 1000) (first function-timing-lists)(set-minor-flag (first function-timing-lists) minor-flag)))))

(defun NEXT4 (lists)
  (if (or (equal 'c1 (second (first lists)))
          (equal 'c1 (second (second lists)))
          (equal 'c1 (second (third lists)))
          (equal 'c1 (second (fourth lists)))) t))
#|
(defun RETURN-BEST-CADENCES (function-timing-lists &optional (distance 0)(previous nil))
  "Returns the timing of the principal cadences."
  (cond ((null function-timing-lists)())
        ((and (equal previous 'a1)(equal (second (first function-timing-lists)) 'c1)(> distance 10000))
         (cons (first function-timing-lists)
               (return-best-cadences (rest function-timing-lists) 0)))
        ((and (> distance 10000) (equal (second (first function-timing-lists)) 'a1)
              (not (equal (second (second function-timing-lists)) 'c1)))
         (cons (first function-timing-lists)
               (return-best-cadences (rest function-timing-lists) 0)))
        ((and (or (equal (second (first function-timing-lists)) 'c1)
                  (equal (second (first function-timing-lists)) 'a1))
              (> distance 10000))
         (cons (first function-timing-lists)
               (return-best-cadences (rest function-timing-lists) 0)))
        (t (return-best-cadences (rest function-timing-lists) (+ distance 1000)))))
|#

;;;;;
#|(get-pitches '((61000 60 1000 4 96) (61000 76 1000 1 96) (61500 69 250 3 96) 
    (61500 72 500 2 96) (61750 70 250 3 96))) 
 (61000 (60 76 69 72 70))|#
;;;;;

(defun GET-PITCHES (events)
  "Returns a list of the initial ontime and the pitches."
  (list (very-first events)(mapcar #'second events)))

;;;;;
#|(get-function '(79 64 48 48 65 65 50 50))
   a1|#
;;;;;

(defun GET-FUNCTION (chord-notes)
  "Returns a function name from a set of pitches."
(if (< (length (second chord-notes)) 2) (list (first chord-notes) 'e4)
   (list (first chord-notes)
         (second (compare-them (second chord-notes) *analysis-lexicon*)))))

;;;;;
#|(compare-them '(79 64 48 48 65 65 50 50) *analysis-lexicon*)
((31 43 55 67 79 91 103 35 47 59 71 83 95 107 26 38 50 62 74 86 98 29 41 53 65
  77 89 101) a1)|#
;;;;;

(defun COMPARE-THEM (harmonic-notes harmonic-functions)
  "Returns an appropriate analysis from the *analysis-lexicon*."
  (let* ((counts (count-harmonic-notes harmonic-notes
                                       harmonic-functions))
         (highest (first (my-sort '> counts))))
    ;(if (count highest counts)
     ; (find-nearest-the-front (first (my-sort '< harmonic-notes))
     ;                         (get-relevant-analyses counts highest *analysis-lexicon*))
     ; (nth highest harmonic-functions)
(nth (position highest counts) harmonic-functions)))
;)

;;;;;
#|(get-relevant-analyses
           '(2 2 2 2 2 0 2 0 2 0 2 2 0 2 2 2 0 0 0 0)
           2
           '(((24 36 48 60 72 84 96 108 28 40 52 64 76 88 100 31 43 55 67 79 91 103) c1)
            ((29 41 53 65 77 89 101 33 93 105 81 45 57 69 24 36 48 60 72 84 96 108) p1) . . .
 (((31 43 55 67 79 91 103 35 47 59 71 83 95 107 26
    38 50 62 74 86 98 29 41 53 65 77 89 101) . . .|#
;;;;;

(defun GET-RELEVANT-ANALYSES (counts highest lexicon)
  "Returns an appropriate list of possible analyses."
  (if (null (member highest counts))()
      (cons (nth (position highest counts) lexicon)
            (get-relevant-analyses (rest (member highest counts)) highest
                                   (rest (nthcdr (position highest counts) lexicon))))))

;;;;;
#|(find-nearest-the-front
           64
           '(((24 36 48 60 72 84 96 108 28 40 52 64 76 88 100 31 43 55 67 79 91 103) c1)
           '((29 41 53 65 77 89 101 33 93 105 81 45 57 69 24 36 48 60 72 84 96 108) p1) . . . 
 ((28 40 52 64 76 88 100 31 43 55 67 79 91 103 35 47 59 71 83 95 107) c4)|#
;;;;;

(defun FIND-NEAREST-THE-FRONT (number lexicon)
  "Returns an analysis-lexicon list most appropriate for its first argument."
  (let ((test (mapcar #'(lambda (x)(let ((trial (position number (first x))))
                                     (if trial trial 100))) lexicon)))
    (nth (position (first (my-sort '< test)) test) lexicon)))

;;;;;
#|(find-position '(4 4 5 4 2 4 3 2 1 1 0 4 2 2 4 2 2 0 1 4))
    2|#
;;;;;

(defun FIND-POSITION (counts)
  "Returns the position of the highest value of its argument from base 0."
  (position (apply 'max counts) counts))

;;;;;
#|(count-harmonic-notes
            '(79 64 48 48 65 65 50 50) *analysis-lexicon*)
(4 4 5 4 2 4 3 2 1 1 0 4 2 2 4 2 2 0 1 4)|#
;;;;;

(defun COUNT-HARMONIC-NOTES (harmonic-notes harmonic-functions)
  "Counts its first argument in its second argument."
  (if (null harmonic-functions) nil
      (cons (my-count harmonic-notes (very-first harmonic-functions))
            (count-harmonic-notes harmonic-notes (rest harmonic-functions)))))

;;;;;
#|(my-count '(79 64 48 48 65 65 50 50)
           '(38 50 62 74 86 41 53 65 77 89 46 58 70 82 94))
   4|#
;;;;;

(defun MY-COUNT (list-1 list-2)
  "Counts its first argument in its second argument."
  (if (null list-1) 0
      (+ (count (first list-1) list-2)
         (my-count (rest list-1) list-2))))


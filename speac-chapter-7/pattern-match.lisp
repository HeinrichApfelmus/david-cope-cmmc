



                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;       SPEAC Function/Chapter 7      ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;   pattern-match code to run SPEAC   ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;


#|A lot depends on the size of the *pattern-size* variable. With proper setting it can 
reveal the form; with the wrong setting it will reveal nothing. Every work requires a separate 
setting for the best revelation of form.|#

;;;;the important variables here:
(setq *pattern-size* 8)
(setq *threshold* 2)
(setq *intervals-off* 1)
(setq *amount-off* 2)
#|
ultimately this code should produce 
(defVar *LEVELS* '(((c1)) 
                     ((a2 c1) (e2 c1)) 
                     ((p3 a2) (a3 a2) (e3 e2))
                     ((p4 p3 12) (s4 p3 24) (p4 a3 36) (a4 a3 48) (s4 e3 60) (e4 e3 72))))
(defVar *FORM-CONNECTIONS* '((1 5)(1 2)(5 6)(3 4)))

|#


;;;(top-level-matcher (list *events* *events*))
#|how it works
divide the data into two pieces and pattern match them together
select the two or three highest patterns and then use theit first appearances to
demark the basic thematic divisions of the music.
|#


;;;;;
#|(make-form-connections *events*)
((1 2))|#
;;;;;

(defun MAKE-FORM-CONNECTIONS (events)
  "The top-level form-analyzer."
  (setq *form-connections* 
        (form-connections 
         (collect-all-form-type-elements 
          (map-numbers 
           (insert-introduction
            (identify-and-eliminate-repeats-in-form 
             (find-all-pattern-occurances 
              (return-themes events) events))))))))

;;;;;
#|(divide-events-into-two-sections '((0 60 1000 4 96) (0 55 1000 3 96) . . .
(((0 60 1000 4 96) (0 55 1000 3 96) (0 64 1000 2 96) (0 76 1000 1 96) (1000 53 1000 4 96) (1000 57 500 3 96) . . .|#
;;;;;

(defun DIVIDE-EVENTS-INTO-TWO-SECTIONS (events)
  "Divides its arg into two roughly equal pieces."
  (let ((sorted-events (sortcar #'< events)))
    ;(divide-into-two (round (/ (get-end-time sorted-events) 2)) sorted-events)
(list sorted-events sorted-events)
))

;;;;;
#|(get-end-time '((0 60 1000 4 96) (0 55 1000 3 96) . . .
70000|#
;;;;;

(defun GET-END-TIME (sorted-events)
  "Returns the end-time of events."
  (let ((last (my-last sorted-events)))
    (+ (first last)(third last))))

;;;;;
#|(divide-into-two 35000 '((0 60 1000 4 96) . . .
(((0 60 1000 4 96) (0 55 1000 3 96) (0 64 1000 2 96) . . .|#
;;;;;

(defun DIVIDE-INTO-TWO (time sorted-events)
  "Returns the two halves of a work."
  (list (divide-it time sorted-events)
        (after-check (+ 1000 time) sorted-events)))

;;;;;
#|(divide-it 35000 '((0 60 1000 4 96) (0 55 1000 3 96) . . .
((0 60 1000 4 96) (0 55 1000 3 96) (0 64 1000 2 96) . . .|#
;;;;;

(defun DIVIDE-IT (time sorted-events)
  "Returns the first chunk of a work before time."
  (if (> (very-first sorted-events) time)
    ()
    (cons (first sorted-events)
          (divide-it time (rest sorted-events)))))

;;;;;
#|(after-check 36000 ((0 60 1000 4 96) (0 55 1000 3 96) . . .
((36000 50 500 4 96) (36000 65 500 3 96) (36000 69 500 2 96) . . .|#
;;;;;

(defun AFTER-CHECK (time sorted-events)
  "Returns the end chunk of the events after time."
  (cond ((null sorted-events) ())
        ((< (very-first sorted-events) time)
         (after-check time (rest sorted-events)))
        (t (cons (first sorted-events)
                 (after-check time (rest sorted-events))))))

;;;;;
#|(find-matches '((39000 70 1000 1 96) (39500 48 500 4 96) (39500 64 500 3 96) (39500 67 500 2 96) 
   (40000 53 1000 4 96) (40000 60 1000 3 96) . . .
  14|#
;;;;;

(defun FIND-MATCHES (pattern patterns)
  "Returns the number of the matches it finds."
  (run-pattern-match (interval-translator 
                      (mapcar (if (equal *duration* "yes") 'third 'second) 
                              (get-channel 1 pattern)))
                     (interval-translator 
                      (mapcar (if (equal *duration* "yes") 'third 'second) 
                              (get-channel 1 patterns)))))



(setq *pattern-size* 12)
(setq *threshold* 2)
(setq *intervals-off* 2)
(setq *amount-off* 1)

#| for 33.3 perfect!!!! missed B as it should and identified the basic as exact!!!!
the above set to 12 2 2 1 in that order
(simple-matcher *events*)
((4 0 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) (4 24000 (2 -2 -2 -1 1 1 1 1 1 -2 -2))
 (4 96000 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) (4 120000 (2 -2 -2 -1 1 1 1 1 1 -2 -2)))

this does a fairly good job of analyzing 63.2 with 11 1 2 1
((4 1000 (-1 -2 -2 -1 -2 2 -7 12 -5 1)) (2 10000 (0 -2 -2 -1 -2 -2 -1 -1 4 -5))
 (2 20000 (-2 -1 -1 -4 13 0 -1 -2 -2 -1)) (2 29500 (2 -7 12 -5 2 3 5 -1 -1 -1))
 (2 37000 (-4 1 1 2 2 -2 -1 -5 1 3)) (2 48000 (2 -2 -1 1 9 -2 2 -4 -1 1))
 (2 72000 (2 -2 -1 1 9 -2 2 -4 -1 1)) (3 119000 (13 0 -1 -2 -2 -1 -2 2 -7 12))
 (2 133000 (0 -2 -2 -1 -2 -2 -1 -1 4 -5)) (2 140000 (-2 -1 -1 -4 13 0 -1 -2 -2 -1))
 (2 149500 (2 -7 12 -5 2 3 5 -1 -1 -1)) (2 157000 (-4 1 1 2 2 -2 -1 -5 1 3)))

this does a fairly good job of analyzing 67.4 with 9 3 2 1
((4 1500 (-1 -1 1 -5 1 2 -2 2)) (9 48000 (2 1 2 1 2 -2 -1 5))
 (9 54000 (1 2 2 1 2 -2 -1 5)) (9 71500 (-5 2 1 2 1 2 -2 -1))
 (9 77000 (-5 1 2 2 1 2 -2 -1)) (6 83000 (-1 -1 -10 9 -1 0 -1 -1))
 (6 88500 (8 -1 0 -1 -1 -10 8 -1)) (9 96000 (2 1 2 1 2 -2 -1 5))
 (9 102000 (1 2 2 1 2 -2 -1 5)) (9 119500 (-5 2 1 2 1 2 -2 -1))
 (9 125000 (-5 1 2 2 1 2 -2 -1)) (6 131000 (-1 -1 -10 9 -1 0 -1 -1))
 (6 136500 (8 -1 0 -1 -1 -10 8 -1)) (4 241500 (-1 -1 1 -5 1 2 -2 2))
 (9 288000 (2 1 2 1 2 -2 -1 5)) (9 294000 (1 2 2 1 2 -2 -1 5))
 (9 311500 (-5 2 1 2 1 2 -2 -1)) (9 317000 (-5 1 2 2 1 2 -2 -1))
 (6 323000 (-1 -1 -10 9 -1 0 -1 -1)) (6 328500 (8 -1 0 -1 -1 -10 8 -1)))
|#

;;;;;
#|(simple-matcher
           '((0 55 1000 2 64) (0 65 1000 2 64) (0 71 1000 2 64) (0 74 1000 1 64)
             (1000 65 1000 2 64) (1000 67 1000 2 64) (1000 71 1000 2 64)
             (1000 76 1500 1 64) . . .
((4 0 (2 -2 -2 -1 1 1 1 1 1 -2 -2))
 (4 24000 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) . . .|#
;;;;;

(defun SIMPLE-MATCHER (events)
  "A contiguous matcher for the form program."
  (let ((ordered-and-channeled-events (get-ontimes-and-pitches (get-channel *matching-line* (sortcar #'< events)))))
    (find-the-matches ordered-and-channeled-events ordered-and-channeled-events)))

;;;;;
#|(get-ontimes-and-pitches
           ((0 74 1000 1 64) (1000 76 1500 1 64) (2500 74 500 1 64)
            (3000 72 750 1 64) . . .
((0 74) (1000 76) (2500 74) (3000 72) (3750 71)
                                   (4000 72) (5500 73) . . .|#
;;;;;

(defun GET-ONTIMES-AND-PITCHES (events)
  "Returns just the ontimes and pitches in that order in sublists."
  (if (null events)()
      (cons (firstn 2 (first events))
            (get-ontimes-and-pitches (rest events)))))

;;;;;
#|(find-the-matches
           '((0 74) (1000 76) (2500 74) (3000 72) (3750 71) (4000 72) (5500 73)
             (6000 74) (6750 75) . . .
           '((0 74) (1000 76) (2500 74) (3000 72) (3750 71) (4000 72) (5500 73)
             (6000 74) (6750 75) . . .
     ((4 0 (2 -2 -2 -1 1 1 1 1 1 -2 -2))
      (4 24000 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) . . .|#
;;;;;

(defun FIND-THE-MATCHES (work-1 work-2) 
  "Returns lists of matched patterns headed by their number of matches."
  (if (< (length work-1) *pattern-size*) nil
      (let* ((patterns (firstn *pattern-size* work-1))
             (test (find-matchings patterns work-2)))
        (if (> test *threshold*)
          (cons (list test (very-first patterns)(interval-translator (mapcar #'second patterns)))
                (find-the-matches (nthcdr *pattern-size* work-1) work-2))
          (find-the-matches (rest work-1) work-2)))))

;;;;;
#|(find-matches '((21750 74) (22000 79) (24000 74) (25000 76) (26500 74)
                  (27000 72) (27750 71) (28000 72) (29500 73) (30000 74)
                  (30750 75) (31000 76))
                '((0 74) (1000 76) (2500 74) (3000 72) (3750 71) (4000 72)
                  (5500 73) . . . 
   2|#
;;;;;

(defun FIND-MATCHINGS (pattern patterns)
  "Returns the number of the matches it finds."
  (run-pattern-match (interval-translator 
                      (mapcar (if (equal *duration* "yes") 'third 'second) 
                              pattern))
                     (interval-translator 
                      (mapcar (if (equal *duration* "yes") 'third 'second) 
                              patterns))))


;;;;;
#|(run-pattern-match '(-2 -2) '(-2 -2 -3 2 1 -1 1 0 0 0 2 3 2 2 -2 -2 -1 -2 2 3 0 -3 0 
-2 2 -7 2 1 2 -3 -2 2 1 2 2 1 2 -2 -1)) 
 12|#
;;;;;

(defun RUN-PATTERN-MATCH (pattern patterns &optional (matches 0))
  "Runs the pattern-matching program."
  (if (< (length patterns) (length pattern)) matches
      (if (pattern-match pattern (firstn (length pattern) patterns) *intervals-off*)
          (run-pattern-match pattern (rest patterns)(1+ matches))
          (run-pattern-match pattern (rest patterns) matches))))

;;;;;
#|(pattern-match '(2 3) '(1 2) 1) 
   t|#
;;;;;

(defun PATTERN-MATCH (pattern-1 pattern-2 number-wrong-possible)
  "Returns t if patterns match within the allowances."
  (cond ((and (null pattern-1)(null pattern-2)) t)
        ((or (equal number-wrong-possible -1)
             (> (abs (- (first pattern-1)(first pattern-2))) *amount-off*)) nil)
        ((neq (first pattern-1)(first pattern-2))
         (pattern-match (rest pattern-1)(rest pattern-2)(1- number-wrong-possible)))
        (t (pattern-match (rest pattern-1)(rest pattern-2) number-wrong-possible))))

;;;;;
#|(group-channel-pattern 3 '((0 60 1000 4 96) (0 55 1000 3 96) . . .
((0 60 1000 4 96) (0 55 1000 3 96) (0 64 1000 2 96) (0 76 1000 1 96) (1000 53 1000 4 96) 
(1000 57 500 3 96) (1000 65 1000 2 96) . . .|#
;;;;;

(defun GROUP-CHANNEL-PATTERN (number stuff)
  "Matches the music in *matching-line* channel."
  (cond ((or (null stuff)(zerop number))())
        ((equal (fourth (first stuff)) *matching-line*)
         (cons (first stuff)(group-channel-pattern (1- number)(rest stuff))))
        (t (cons (first stuff)(group-channel-pattern number (rest stuff))))))

;;;;;
#|(next-match-note '((65500 71 500 2 96) (66000 52 500 4 96) (66000 67 1000 3 96) (66000 72 1000 2 96) (66000 76 1000 1 96) . . .
((66000 76 1000 1 96) (66500 53 500 4 96) (67000 55 1000 4 96) (67000 67 500 3 96) (67000 71 1000 2 96) (67000 74 1000 1 96) . . .|#
;;;;;

(defun NEXT-MATCH-NOTE (stuff)
  "Returns the next note for matching."
  (if (equal (fourth (first stuff)) *matching-line*) stuff
      (next-match-note (rest stuff))))

;;;;;
#|(interval-translator '(60 62))
(2)|#
;;;;;

(defun INTERVAL-TRANSLATOR (midi-list)
  "Translates its arg into intervals."
  (if (null (rest midi-list)) nil
      (cons (- (second midi-list)(first midi-list))
            (interval-translator (rest midi-list)))))

;;;;;
#|(next 1) 
  0|#
;;;;;

(defun NEXT (n)
  "Returns its arg minus one."
  (1- n))

;;;;; 
#|(find-best-themes ((3 ((13000 79 1000 1 96) . . .
1|#
;;;;; 

(defun FIND-BEST-THEMES (patterns)
  "Finds the most important themes."
  (let* ((numbers (mapcar #'first patterns))
         (sorted-numbers (mapcar #'abs (interval-translator numbers)))
         (biggest-gap (1+ (position (first (my-sort #'> sorted-numbers)) sorted-numbers))))
    (if (< biggest-gap 4) biggest-gap 3)))
        
;;;;; 
#|(return-themes *events*)
((531 ((3000 69 1000 1 96) (4000 47 1000 4 96) (4000 62 1000 3 96) (4000 66 500 2 96) (4000 71 1000 1 96) 
       (4500 68 500 2 96) (5000 45 1000 4 96) (5000 64 1000 3 96) (5000 69 1000 2 96) (5000 72 1000 1 96))) 
 (334 ((0 60 1000 4 96) (0 55 1000 3 96) (0 64 1000 2 96) (0 76 1000 1 96) (1000 53 1000 4 96) 
       (1000 57 500 3 96) (1000 65 1000 2 96) (1000 74 1000 1 96) (1500 59 500 3 96) (2000 45 500 4 96) 
       (2000 60 1000 3 96) (2000 64 1000 2 96) (2000 72 1000 1 96))))|#
;;;;;
  
(defun RETURN-THEMES (events)
  "Returns all of the found themes."
  (let ((patterns (top-level-matcher (divide-events-into-two-sections events))))
     (firstn (find-best-themes patterns) patterns)))


;;;;;
#|(find-all-pattern-occurances (return-themes *events*) 
                             *events*)
((13000 36000))|#
;;;;;

(defun FIND-ALL-PATTERN-OCCURANCES (collected-patterns events)
  "Top-level pattern finder."
  (if (null collected-patterns)()
      (cons (find-pattern-occurances (first collected-patterns) events)
            (find-all-pattern-occurances (rest collected-patterns) events))))

;;;;;
#|(find-pattern-occurances (3 ((13000 79 1000 1 96) (13500 48 500 4 96). . .
(13000 36000))|#
;;;;;

(defun FIND-PATTERN-OCCURANCES (counted-pattern-events events &optional (time 0))
  "Finds the occrances of its first arg in second arg."
  (let ((test (find-pattern-entry (get-channel 1 (second counted-pattern-events))
                                  (get-channel 1 (sculpt-entry-point time events)))))
    (if (null test)()
        (cons test
              (find-pattern-occurances counted-pattern-events events (+ test (* *pattern-size* 1000)))))))
    
;;;;;
#|(find-pattern-entry ((13000 79 1000 1 96) (14000 81 1000 1 96) (15000 79 500 1 96) (15500 77 500 1 96) . . .
13000|#
;;;;;

(defun FIND-PATTERN-ENTRY (pattern patterns)
  "Takes channel-1 events as args."
  (cond ((< (length patterns)(length pattern))())
        ((pattern-match (interval-translator (mapcar 'second pattern))
                        (interval-translator (mapcar 'second (firstn (length pattern) patterns))) *intervals-off*)
         (very-first patterns))
        (t (find-pattern-entry pattern (rest patterns)))))

;;;;;
#|(sculpt-entry-point 21000 ((0 60 1000 4 96) (0 55 1000 3 96) (0 64 1000 2 96) (0 76 1000 1 96) (1000 53 1000 4 96)
((21000 60 500 4 96) (21000 67 1000 3 96)  . . .|#
;;;;;

(defun SCULPT-ENTRY-POINT (time events)
  "Simply returns all of the events from time and beyond."
  (cond ((null events)())
        ((< (very-first events) time)
         (sculpt-entry-point time (cdr events)))
        (t (cons (first events)
                 (sculpt-entry-point time (cdr events))))))

;;;;;
#|(map-numbers '((0 a)(5000 b)(10000 a)(15000 b)(20000 a)))
((0 a 1) (5000 b 2) (10000 a 3) (15000 b 4) (20000 a 5))|#
;;;;;

(defun MAP-NUMBERS (lists &optional (number 1))
  "Maps incremental numbers onto the rear of its arg."
  (if (null lists)()
      (cons (append (first lists)(list number))
            (map-numbers (rest lists)(1+ number)))))

;;;;;
#|(collect-all-form-type-elements '((13000 a 1) (36000 a 2)))
(((13000 a 1) (36000 a 2)))|#
;;;;;

(defun COLLECT-ALL-FORM-TYPE-ELEMENTS (form)
  "Collects all relevant related elements of form."
  (if (null form)()
      (let ((test (cons (first form)(collect-all (first form)(rest form)))))
        (cons test
              (collect-all-form-type-elements (remove-all test form))))))

;;;;;
#|(collect-all '(13000 a 1) '((36000 a 2)))
((36000 a 2))|#
;;;;;

(defun COLLECT-ALL (element form)
  "Collects all of its element-related iterations in form."
  (cond ((null form)())
        ((equal (second element)(second (first form)))
         (cons (first form)
               (collect-all element (rest form))))
        (t (collect-all element (rest form)))))

;;;;;
#|(form-connections '(((0 a 1) (10000 a 3) (20000 a 5)) ((5000 b 2) (15000 b 4))))
((1 3) (1 5) (3 5) (2 4))|#
;;;;;

(defun FORM-CONNECTIONS (form-counted)
  "The top-level form deriver for the SPEAC program."
  (apply #'append (derive-form-connections form-counted)))

;;;;;
#|(derive-form-connections '(((0 a 1) (10000 a 3) (20000 a 5)) ((5000 b 2) (15000 b 4))))
(((1 3) (1 5)) ((3 5)) ((2 4)))|#
;;;;;

(defun DERIVE-FORM-CONNECTIONS (form-counted)
  "Runs the derive-form program."
  (if (null form-counted)()
      (append (derive-form (first form-counted))
              (derive-form-connections (rest form-counted)))))

;;;;;
#|(derive-form '((0 a 1) (10000 a 3) (20000 a 5)))
(((2 4)))|#
;;;;;

(defun DERIVE-FORM (element-and-repeats)
  "The main function for deriving form from retrieved elements."
  (if (null (rest element-and-repeats))()
      (cons (derive-them (first element-and-repeats) (rest element-and-repeats))
            (derive-form (rest element-and-repeats)))))

;;;;;
#|(derive '(0 a 1) '((10000 a 3) (20000 a 5)))
((1 3) (1 5))|#
;;;;;

(defun DERIVE-THEM (element repeats)
  "Derives repeated form of its second arg."
  (if (null repeats)()
      (cons (list (third element)(third (first repeats)))
            (derive-them element (rest repeats)))))


;;;;;
#|(identify-and-eliminate-repeats-in-form '((0 22000)(13000 36000)))
((0 A) (13000 B) (22000 A) (36000 B))|#
;;;;;

(defun IDENTIFY-AND-ELIMINATE-REPEATS-IN-FORM (list-of-elements)
  "Uses the output of find-all-pattern-occrances to show a full form or a work."
  (sortcar #'< (apply #'append (identify list-of-elements))))

;;;;;
#|(identify ((0 22000) (13000 36000)))
(((0 A) (22000 A)) ((13000 B) (36000 B)))|#
;;;;;

(defun IDENTIFY (element-lists &optional (alpha '(a b c d e f g h i j k l m n)))
  "Identifies the formal elements of a work."
  (if (null element-lists)()
      (cons (loop for element in (first element-lists)
                  collect (list element (first alpha)))
            (identify (rest element-lists)(rest alpha)))))

;;;;;
#|(insert-introduction (identify-and-eliminate-repeats-in-form 
      (find-all-pattern-occurances 
       (return-themes *events*) *events*)))
((0 i) (13000 a) (36000 a))|#
;;;;;

(defun INSERT-INTRODUCTION (identified-forms)
  "Inserts introduction if the form does not begin on 0."
  (if (not (zerop (very-first identified-forms)))
    (cons '(0 i) identified-forms)))

;;;;;
#|(top-level-matcher (divide-events-into-two-sections *events*))
((531 ((3000 69 1000 1 96) (4000 47 1000 4 96) (4000 62 1000 3 96) (4000 66 500 2 96) (4000 71 1000 1 96) . . .|#
;;;;;


(defun TOP-LEVEL-MATCHER (event-lists)
  "Thge very top-level of the pattern-matcher."
  (rank-the-matches 
   (challenge-the-matches 
    (add-the-matches 
     (match-the-database-music event-lists)))))

;;;;;
#|(rank-the-matches ((334 ((0 60 1000 4 96). . . 
((531 ((3000 69 1000 1 96) (4000 47 1000 4 96) . . .|#
;;;;;

(defun RANK-THE-MATCHES (finds)
  "Ranks the matches in hierarchical order."
  (sortcar '> finds))

;;;;;
#|(challenge-the-matches '((334 ((0 60 1000 4 96) (0 55 1000 3 96)  . . .
((334 ((0 60 1000 4 96) (0 55 1000 3 96) (0 64 1000 2 96) (0 76 1000 1 96) (1000 53 1000 4 96) 
 (1000 57 500 3 96) . . .|#
;;;;;

(defun CHALLENGE-THE-MATCHES (finds)
  "Returns only those finds that exceed the *threshold*."
  (cond ((null finds) nil)
        ((>= (very-first finds) *threshold*)
         (cons (first finds)
               (challenge-the-matches (rest finds))))
        (t (challenge-the-matches (rest finds)))))

;;;;;
#|(add-the-matches '((10 ((0 60 1000 4 96) . . .
((334 ((0 60 1000 4 96) (0 55 1000 3 96) (0 64 1000 2 96) (0 76 1000 1 96) (1000 53 1000 4 96) 
 (1000 57 500 3 96) . . .|#
;;;;;here's the PROBLEM!!!!!

(defun ADD-THE-MATCHES (matched-list &optional (length (length matched-list)))
  "Adds the matches together."
  (if (zerop length) nil
      (let ((test (meta-matcher (first matched-list)(rest matched-list))))
        (if test 
            (cons (cons (apply '+ (mapcar #'first (cons (first matched-list) test)))
                        (rest (first matched-list)))
                  (add-the-matches (my-remove test (rest matched-list))
                                   (- length (1+ (length test)))))
            (cons (first matched-list)
                  (add-the-matches (rest matched-list)(1- length)))))))

;;;;;
#|(my-remove '(1) '(1 2 3))
(2 3)|#
;;;;;

(defun MY-REMOVE (objects-to-be-removed list-of-objects)
  "Removes all of its first arg from second arg."
  (if (null objects-to-be-removed) list-of-objects
      (my-remove (rest objects-to-be-removed)
                 (remove (first objects-to-be-removed)
                         list-of-objects))))

;;;;;
#|(meta-matcher '(1 ((24000 76 1000 1 96) (24500 62 500 4 96) (25000 60 500 4 96) (25000 64 500 3 96) . . .
 ((1 ((46000 76 2000 1 96) (48000 53 1000 4 96) (48000 69 2000 2 96) (48000 69 2000 1 96) 
(48500 60 500 3 96) (49000 52 1000 4 96) (49000 62 500 3 96) (49500 64 500 3 96) (50000 50 1000 4 96) 
(50000 65 500 3 96) . . .|#
;;;;; 

(defun META-MATCHER (first-matched-list second-matched-list)
  "Tabulates the matches."
  (cond ((null second-matched-list) nil)
        ((pattern-match (interval-translator (mapcar 'second (get-channel 1 (second first-matched-list))))
                        (interval-translator (mapcar 'second (get-channel 1 (second (first second-matched-list))))) *intervals-off*)
         (cons (first second-matched-list) (meta-matcher first-matched-list (rest second-matched-list))))
        (t (meta-matcher first-matched-list (rest second-matched-list)))))

;;;;;
#|(match-the-database-music '(((0 60 1000 4 96) (0 55 1000 3 96) (0 64 1000 2 96) (0 76 1000 1 96) (1000 53 1000 4 96) . . 
((10 ((0 60 1000 4 96) (0 55 1000 3 96) (0 64 1000 2 96) (0 76 1000 1 96) (1000 53 1000 4 96) . . .|#
;;;;;

(defun MATCH-THE-DATABASE-MUSIC (database-music &optional (length (length database-music)))
  "Returns lists of matched patterns headed by their number of matches."
  (if (zerop length) nil
      (append (match-the-databases (first database-music)
                                   (rest database-music))
              (match-the-database-music (append (rest database-music)
                                                (list (first database-music)))
                                        (next length)))))

;;;;;
#|(match-the-databases '((0 60 1000 4 96) (0 55 1000 3 96) (0 64 1000 2 96) (0 76 1000 1 96) 
(1000 53 1000 4 96) . . .
((10 ((0 60 1000 4 96) (0 55 1000 3 96) (0 64 1000 2 96) (0 76 1000 1 96) (1000 53 1000 4 96) 
(1000 57 500 3 96) (1000 65 1000 2 96) (1000 74 1000 1 96) (1500 59 500 3 96) (2000 45 500 4 96) . . |#
;;;;;

(defun MATCH-THE-DATABASES (music-from-db music-from-dbs)
  "Returns lists of matched patterns headed by their number of matches."
  (if (null music-from-dbs) nil
      (append (find-the-matches music-from-db (first music-from-dbs))
              (match-the-databases music-from-db (rest music-from-dbs)))))

;;;;;
#|(find-the-matches '((0 60 1000 4 96) (0 55 1000 3 96) . . .
((10 ((0 60 1000 4 96) (0 55 1000 3 96) (0 64 1000 2 96) (0 76 1000 1 96) (1000 53 1000 4 96) 
(1000 57 500 3 96) . . . 
;;;;;

(defun FIND-THE-MATCHES (work-1 work-2) 
  "Returns lists of matched patterns headed by their number of matches."
  (if (< (length (get-channel 1 work-1)) *pattern-size*) nil
      (let* ((patterns (group-channel-pattern *pattern-size* work-1))
             (test (find-matches patterns work-2)))
        (if (> test 0)
          (cons (list test patterns)
                (find-the-matches (next-match-note (rest work-1)) work-2))
          (find-the-matches (next-match-note (rest work-1)) work-2)))))|#

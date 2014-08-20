

                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;       Infer Function/Chapter 6      ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;       simple code to run infer      ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

;;;  In order to run Analogy you must load this file and follow the example shown in red.
;;;  Note that analogy requires the infer functions which have been included here
;;;  for convenience (bottom of file).

;;;;;
#|
the analogy program is like the infer program except it uses an 
IS LIKE A model instead
For example, circle-within-circle is to circle-alone as 
           square-within-square is to square-alone
(circle-circle:circle::square-square:square)
missing-circle - missing-square
an analogy!
OR
3 is to 6 as 6 is to 12 
(3:6::6:12)
2*x
OR
hands are to arms as feet are to legs
(hands:arms::feet:legs)
OR
In Musical Terms
minorseconds are to majorseconds as minorthirds are to majorthirds
(m2:M2::m3:M3)
m to M
OR
i-v is to i-v-i as c-g is to c-g-c
i-v:1-v-1::c-g:c-g-c
incomplete is to complete as incomplete is to complete
fairly easy using intervals since proportions of differences between intervals should have powerful 
analogies in a wide range of circumstances as in
2 is to 4 as 4 is to X (8)
in other words it can all be mathematical in music.

and different keys!! is transposition and modulation a good example of analogy in music (yes????) 
if so then it can all be done pretty simply with a "+" unification process as in
A+B+C is like D+E+F, etc.
but with numbers, other (more important?) analogies can become important 
especially when the numbers represent
intervals which can be (for example) added to create an underlying direction (ambitus) as in
(1 2 -1 -2) is to (3 2 -1 -2) as (0 2 -2) is to (4 -2)
as in 0 is to 2 as 0 is to 2!!!!!! (summed intervals of each set.....

1) infer the given pointer to make sure that it has complete lineage.
2) take the last item from 1
3) collect all other predecessors of 2
4) choose one of the predecessors
5) return one of these as an analog to the given pointer

Example:
(setq *database* '((cat*mouse are strong*weak)
                   (strong*weak are opposite*sides)
                   (oil*water are non*mixable)
                   (non*mixable are opposite*sides)))
(analogy 'cat*mouse)
(cat*mouse is like oil*water)
and a musical example of the above
(setq *database* '((g-b-d*c-e-g are dominant*tonic)
                   (dominant*tonic are resolutions)
                   (f-a-c*c-e-g are subdominant*tonic)
                   (subdominant*tonic are resolutions)))
(analogy 'g-b-d*c-e-g)
(g-b-d*c-e-g is like f-a-c*c-e-g)
|#
;;;;;

(defVar *RS* (make-random-state t) "Variable for storing the current random state.")
(defVar *DATABASE* () "Where the basic data is kept.")

;;;;;
#| ? (analogy 'cat*mouse)
   (cat*mouse is like oil*water)|#
;;;;;

(defun ANALOGY (pointer)
  "Toplevel function for Analogy - pretties up the results."
  (let* ((test (infer pointer))
         (predecessors (derive-all-logical-predecessors (my-last test)))
         (choice (choose-one (remove pointer predecessors :test #'equal))))
    (append (list pointer) '(is like) (list choice))))

;;;;;
#| ? (derive-all-logical-predecessors opposite*sides) 
   (cat*mouse oil*water)|#
;;;;;

(defun DERIVE-ALL-LOGICAL-PREDECESSORS (end-pointer &optional (database (reverse-the-database *database*)))
  "Connects the various pointers for Analogy."
  (let ((test (infer-for-analogy end-pointer database)))
    (if (null test)()
        (append (last (my-last test))
                (derive-all-logical-predecessors end-pointer (remove-all test database))))))

;;;;;
#| ? (reverse-the-database
           ((cat*mouse are strong*weak) (strong*weak are opposite*sides)
            (oil*water are non*mixable) (non*mixable are opposite*sides))) 
    ((strong*weak are cat*mouse)
     (opposite*sides are strong*weak)
     (non*mixable are oil*water)
     (opposite*sides are non*mixable))|#
;;;;;

(defun REVERSE-THE-DATABASE (database)
  "Reverses each element of the database."
  (loop for element in database
        collect (reverse element)))

;;;;;
#| ? (infer-for-analogy
           opposite*sides
           ((strong*weak are cat*mouse) (opposite*sides are strong*weak)
            (non*mixable are oil*water) (opposite*sides are non*mixable))) 
   ((opposite*sides are strong*weak)
    (strong*weak are cat*mouse))|#
;;;;;

(defun INFER-FOR-ANALOGY (pointer database)
  "Infers pointer with each successive pointer."
  (let ((test (lookup-data pointer database)))
    (if (null test)()
        (cons test
              (infer-for-analogy (my-last test) database)))))

;;;;;
#| ? (lookup-data opposite*sides
                      ((non*mixable are oil*water)
                       (opposite*sides are non*mixable))) 
   (opposite*sides are non*mixable)|#
;;;;;

(defun LOOKUP-DATA (pointer database)
  "Associates pointer with database."
  (assoc pointer database :test #'equal))

;;;;;
#| ? (remove-all '((a)(b)) '((a)(b)(a)(c)))
   ((c))|#
;;;;;

(defun REMOVE-ALL (lists other-lists)
  "Removes all of the first arg from the second arg."
  (if (null lists) other-lists
      (remove-all (rest lists)(remove (first lists) other-lists  :test #'equal))))

;;;;;
#| ? (choose-one '(1 2 3))
      2|#
;;;;;

(defun CHOOSE-ONE (list)
  "Chooses one its arg randomly."
  (nth (random (length list) *rs*) list))

;;;the Infer program here for convenience.

;;;;;
#| ? (lookup 'man)
   (man is a human)|#
;;;;;

(defun LOOKUP (pointer)
  "The basic lookup function - returns only what the pointer points towards."
  (assoc pointer *database* :test #'equal))

;;;;;
#| ? (infer 'man)
   (man is a human is a mammal)|#
;;;;;

(defun INFER (pointer)
  "Returns all of the pointers of what's pointed towards - implicit information."
  (let ((test (lookup pointer)))
    (if (null test)()
        (append test
                (rest (infer (my-last test)))))))

;;;;;
#| ? (add-to-database '(woman is a human))
     ((woman is a human) (man is a human) (human is a mammal))
   (infer 'woman)
     (woman is a human is a mammal)|#
;;;;;

(defun ADD-TO-DATABASE (statement)
  "Adds statement to the database as explicit fact."
  (push statement *database*))

;;;;;
#| ? (reset-the-database)
           nil|#
;;;;;

(defun RESET-THE-DATABASE ()
  "Resets the database to nil."
  (setq *database* ()))

;;;;;
#| ? (my-last '(1 2 3))
          3|#
;;;;;

(defun MY-LAST (list)
  "Returns the last element of list as an atom."
  (first (last list)))
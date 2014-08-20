

                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;       SPEAC Function/Chapter 7      ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;     utilities code to run SPEAC     ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;


;;;  Basic environment variables
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
(setq *print-pretty* t)
(setq *warn-if-redefine* nil)

(defVar *RS* (make-random-state t) "Variable for storing the current random state.")

;;;;;
#|(anyp '(a b) '(a b c d))
     A|#
;;;;;

(defun ANYP (find-list target-list)
  "Returns any of its first arg in the second arg."
  (loop for find in find-list
        when (member find target-list :test #'equal)
        return find))

;;;;;
#|(choose-one '(1 2 3 4))
    4|#
;;;;;

(defun CHOOSE-ONE (list)
  "Chooses a random member of its argument."
  (nth (random (length list) *rs*)
       list))

;;;;;
#|(concat 'a 'b 'c)
       ABC|#
;;;;;

(defun CONCAT (&rest list)
  "Makes a symbol of its atom args."
  (implode list))

;;;;;
#|(double-list 'a)
(a (a))|#
;;;;;

(defun DOUBLE-LIST (thing)
  "Double lists its arg."
  (list thing (list thing)))

;;;;;
#|(explode hello!)
   (h e l l o !)|#
;;;;;

(defun EXPLODE (atom)
  "Explodes its atom arg. into a list."
  (if (null atom) nil
      (let ((test (format nil "~a" (coerce (write-to-string atom)
                                           'list))))
        (if (not (find-period test))
          (read-from-string test)))))

;;;;;
#|(find-closest 6 '(1 2 3 5 7 8))
      7|#
;;;;;

(defun FIND-CLOSEST (number list)
  "Finds the closes number in list."
  (let ((test (loop for item in list
                    collect (abs (- number item)))))
    (nth (choose-one (positions (first (my-sort '< test)) test)) list)))

;;;;;
#|(every-other '(a b c d e f g))
      (A C E G)|#
;;;;;

(defun EVERY-OTHER (list)
  "Returns every other things in its arg."
  (let ((test ()))
    (loop for element in list
          do (if test (setf test ())(setf test t))
          when test
          collect element)))

;;;;;
#|(find-furthest 6 '(1 2 3 5 7 8))
       1|#
;;;;;

(defun FIND-FURTHEST (number list)
  "Finds the furthest number in list."
  (let ((test (loop for item in list
                    collect (abs (- number item)))))
    (nth (choose-one (positions (first (my-sort '> test)) test)) list)))

;;;;;
#|(get-filename 1PMacintosh HD:applications:MCL 4.0:Alice:databases:bach:bach-chorale-64-a-1) 
      bach-chorale-64-a-1|#
;;;;;

(defun GET-FILENAME (pathname)
  "Returns the filename from a pathname."
  (read-from-string 
   (mac-namestring 
    (file-namestring pathname))))

;;;;;
#|(positions 1 '(4 5 3 1 2 1 4 5))
       (3 5)|#
;;;;;

(defun POSITIONS (number list)
  "Returns the positions of its first arg in second arg."
  (let ((position ()))
    (loop until (null (member number list))
          do (setf position (position number list))
          collect position
          do (setf list  (substitute 'x number list 
                                     :end (1+ position))))))

;;;;;
#|(find-period "a b c d e")
       nil|#
;;;;;

(defun FIND-PERIOD (exploded-name)
  "Returns t is a period is present."
  (loop until (string= exploded-name "")
        when (string= (char exploded-name 0) ".")
        return t
        else do (setf exploded-name 
                      (string-left-trim (list (char exploded-name 0)) exploded-name))))

;;;;;
#|(firstn 2 '(a b c))
     (a b)|#
;;;;;

(defun FIRSTN (number list)
  "Returns the first number of its list."
  (if (< (length list) number)(firstn (1- number) list)
      (butlast list (- (length list) number))))

;;;;;
#|(get-channel-number
           1
           '((0 48 1000 4 64) (0 63 1000 3 64) (0 69 1000 2 64) (0 77 1000 1 64)...
    ((0 77 1000 1 64) (1000 78 1000 1 64)...|#
;;;;;

(defun GET-CHANNEL-NUMBER (channel-number music)
  "Collects the events with channel-number."
  (loop for event in music
        when (eq (fourth event) channel-number)
        collect event))

;;;;;
#|(implode '(sentence- 3)) 
     sentence-3|#
;;;;;

(defun IMPLODE (list)
  "Implodes its arglist."
  (if (null (cdr list))(first list)
      (implode (cons (read-from-string 
                      (format nil "~a~a" (first list)(second list))) 
                     (nthcdr 2 list)))))

;;;;;
#|(setq x 12)
      12
(incr 'x)
14
? x
14|#
;;;;;

(defun INCR (var &optional (upper-limit 16))
  "My incf."
  (if (>= (eval var) upper-limit)(set var upper-limit)(set var (1+ (eval var)))))

;;;;;
#|(setq x 12)
      12
      (increv 'x)
      11
      x
      11|#
;;;;;

(defun INCREV (var &optional (lower-limit 0))
  "Reverse incf."
  (if (<= (eval var) lower-limit)(set var lower-limit)(set var (1- (eval var)))))

;;;;;
#|(make-intervals '(60 64 67))
      (4 3)|#
;;;;;

(defun MAKE-INTERVALS (midi-list)
  "(make-intervals '(60 64 67))
      >> (4 3)"
  (loop until (null (cdr midi-list))
        collect (- (second midi-list)(first midi-list))
        do (setf midi-list (cdr midi-list))))

;;;;;
#|(mix '(4 2 5 3 6))
    (5 3 6 4 2)|#
;;;;;

(defun MIX (list)
  "Arbitrarily mixes its arg."
  (let ((choice ()))
    (loop until (null list)
          do (setf choice (choose-one list))
          collect choice 
          do (setf list (remove choice list :count 1)))))

;;;;;
#| (my-sort #'> '(3 6)) 
      (6 3)|#
;;;;;

(defun MY-SORT (function lists)
  "Non-destructive sort."
  (loop for item in (sort (loop for x in lists
                                collect (list x))  function :key #'car)
        collect (first item)))

;;;;;
#|(reduce-number 93000) 
    1000|#
;;;;;

(defun REDUCE-NUMBER (number)
  "A kind of mod 1000."
  (loop while (and number (>= number 0))
        if (> number 1000)
        do (setf number (- number 1000))
        else return number))

;;;;;
#|(remove-all '(1 2 3) '(3 4 2 1  5 6 3))
    (4 5 6)|#
;;;;;

(defun REMOVE-ALL (stuff other-stuff)
  "Removes all stuff from other-stuff."
  (loop when (null stuff) return other-stuff
        do (setf other-stuff (remove (first stuff) other-stuff :test 'equal))
        do (setf stuff (cdr stuff))))

;;;;;
#|(remove-chords
           '((0 48 1000 4 64) (0 63 1000 3 64) (0 69 1000 2 64) (0 77 1000 1 64)...
     ((0 48 1000 4 64) (1000 50 1000 4 64) (2000 47 1000 4 64)
      (3000 48 1000 4 64) (4000 50 1000 4 64)...|#
;;;;;

(defun REMOVE-CHORDS (music)
  "Removes all of the related chords to even in music."
  (loop until (null music)
        collect (first music)
        do (setf music (remove-all-chords (first music) music))))

;;;;;
#|(remove-all-chords
           '(2000 47 1000 4 64)
           '((2000 47 1000 4 64) (2000 63 1000 3 64) (2000 69 1000 2 64)...
   ((3000 48 1000 4 64) (3000 62 1000 3 64)
    (3000 67 1000 2 64) (3000 76 1000 1 64)...|#
;;;;;

(defun REMOVE-ALL-CHORDS (event music)
  "Removes all of the related chords to even in music."
  (loop for item in music
        when (not (and (equal (first event)(first item))
                       (equal (third event)(third item))))
        collect item))

;;;;;
#|(remove-nils '(a nil b)) 
   (a b|#
;;;;;

(defun REMOVE-NILS (item)
  "Removes the nils from its arg."
  (cond ((null item)())
        ((null (first item))
         (remove-nils (cdr item)))
        (t (cons (first item)
                 (remove-nils (cdr item))))))

;;;;;
#|(sortcar '< '((2 (a b c))(3 (d e f))(1 (g h i))))
   ((1 (G H I)) (2 (A B C)) (3 (D E F)))|#
;;;;;

(defun SORTCAR (function lists)
  "Sorts by the first element."
  (sort (copy-tree lists) function :key 'first))

#|(sortcadr #'< '((0 3)(0 2)))
((0 2) (0 3))|#

(defun sortcadr (function things)
  (mapcar #'(lambda (x)(append (list (second x))(list (first x))(nthcdr 2 x))) 
          (sortcar function 
                   (mapcar #'(lambda (x)(append (list (second x))(list (first x))(nthcdr 2 x))) things))))

;;;;;
#| (very-first '((a)))
     a|#
;;;;;

(defun VERY-FIRST (list)
  "Equivalent to (first (first."
  (caar list))

;;;;;
#|(very-second '((a b)(c d)))
   b|#
;;;;;

(defun VERY-SECOND (list)
  "Returns the second element of the first list in the arg."
  (cadar list))

;;;;;
#|(my-last '(hello! 0.1)) 
   0.1|#
;;;;;

(defun MY-LAST (list)
  "Returns the first of the last."
  (first (last list)))

;;;;;
#|(allp '(1 2 3 4 5) '(3 4 1 2 5))
t|#
;;;;;

(defun ALLP (first-list second-list)
  "Checks to see if the two args are equivalent but in different order."
  (cond ((and (null first-list)(null second-list)) t)
        ((member (first first-list) second-list)
         (allp (cdr first-list)(remove (first first-list) second-list :count 1)))
        (t ())))

;;;;;
#|(setq a '(1 2))
(1 2)
(PUSH-REV 3 a)
(1 2 3)|#
;;;;;

(defMacro PUSH-REV (thing target)
  "Pushes thing into rear-end of target."
  `(setq ,target (reverse (cons ,thing (reverse ,target)))))


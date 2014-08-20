
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;     Chorale Function/Chapter 4      ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;           platform dependent        ;;;;;
                   ;;;;;          code to run chorale        ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;


;;;;;
#|(sortcar '< '((2 (a b c))(3 (d e f))(1 (g h i))))
   ((1 (G H I)) (2 (A B C)) (3 (D E F)))|#
;;;;;

(defun SORTCAR (function lists)
  "Sorts by the first element."
  (sort (copy-tree lists) function :key 'first))

(defVar *RS* (make-random-state t) "Variable for storing the current random state.")

;;;;;
#|Calling (explode-list (hi there)) 
    explode-list returned (h i t h e r e)|#
;;;;;

(defun EXPLODE-LIST (list)
  "Explodes a list into single elements."
  (apply #'append (loop for item in list
                        collect (explode item))))

;;;;;
#|(explode hello!)
   (h e l l o !)
   13|#
;;;;;

(defun EXPLODE (atom)
  "Explodes its atom arg. into a list."
  (if (null atom) nil
      (let ((test (format nil "~a" (coerce (write-to-string atom)
                                           'list))))
        (read-from-string test))))

;;;;;
#|(implode '(sentence- 3)) 
     sentence-3|#
;;;;;

(defun IMPLODE (list)
  "Implodes its arglist."
  (if (null (rest list))(first list)
      (implode (cons (read-from-string 
                      (format nil "~a~a" (first list)(second list))) 
                     (nthcdr 2 list)))))

;;;;;
#|(my-last '(hello! 0.1)) 
   0.1|#
;;;;;

(defun MY-LAST (list)
  "Returns the first of the last."
  (first (last list)))

;;;;;
#| (my-sort #'> '(3 6)) 
      (6 3)|#
;;;;;

(defun MY-SORT (function lists)
  "Non-destructive sort."
  (loop for sorted-item in 
        (sort (loop for item in lists
                    collect (list item)) function :key #'car)
        collect (first sorted-item)))

;;;;;
#|(choose-one '(1 2 3 4))
    4|#
;;;;;

(defun CHOOSE-ONE (list)
  "Chooses a random member of its argument."
  (nth (random (length list) *rs*)
       list))

;;;;;
#|Calling (make-list-into-string (is your what hello!)) 
     make-list-into-string returned is your what hello!|#
;;;;;

(defun MAKE-LIST-INTO-STRING (list)
  "Turns the list arg into a string."
  (let ((storage ""))
    (loop for element in (butlast list)
          do (setf storage (format () "~A~A~A" storage (write-to-string element) " ")))
    (format () "~A~A" storage (write-to-string (my-last list)))))

;;;;;
#|Calling (make-string-into-list hello!) 
    make-string-into-list returned 2 values :
      (hello!)|#
;;;;;

(defun MAKE-STRING-INTO-LIST (string)
  "Makes the string arg into a simple list."
  (read-from-string (remove-punctuation
                     (format () "~A~A~A" "(" string ")"))))

;;;;;
#|(REMOVE-PUNCTUATION abc. efg# a'c)
        abc efg ac|#
;;;;;

(defun REMOVE-PUNCTUATION (string)
  "Removes all punctuation from list arg."
  (remove "," 
          (remove "'" 
                  (remove "#" 
                          (remove "." string 
                                  :test #'string=) 
                          :test #'string=)
                  :test #'string=) 
          :test #'string=))

;;;;;
#|(firstn 2 '(a b c))
     (a b)|#
;;;;;

(defun FIRSTN (number list)
  "Returns the first number of its list."
  (if (< (length list) number)(firstn (1- number) list)
      (butlast list (- (length list) number))))

;;;;;
#| (very-first '((a)))
     a|#
;;;;;

(defun VERY-FIRST (list)
  "Equivalent to (first (first."
  (caar list))

;;;;;
#|Calling (my-remove (what) (name? is)) 
     my-remove returned (name? is)|#
;;;;;

(defun MY-REMOVE (objects-to-be-removed list-of-objects)
  "Removes each element of first arg from second arg."
  (if (null objects-to-be-removed) list-of-objects
      (my-remove (rest objects-to-be-removed)
                 (remove (first objects-to-be-removed)
                         list-of-objects))))

;;;;;
#|Calling (sortcdr > ((bad 1.0) (you 1.0) (too 5.0) (cant 1.0))) 
      sortcdr returned ((too 5.0) (bad 1.0) (you 1.0) (cant 1.0))|#
;;;;;

(defun SORTCDR (function lists)
  "Sorts by cdr instead of car."
  (sort (copy-tree lists) function :key 'first))

;;;;;
#|(concat 'a 'b 'c)
      >> ABC|#
;;;;;

(defun CONCAT (&rest list)
  "Takes its individual args and makes them into an atom."
  (implode list))

;;;;;
#|(mix '(4 2 5 3 6))
      >> (5 3 6 4 2)|#
;;;;;

(defun MIX (list)
  "Mixes its arg pseudo-randomly."
  (let ((choice ()))
    (loop until (null list)
          do (setf choice (choose-one list))
          collect choice 
          do (setf list (remove choice list :count 1)))))
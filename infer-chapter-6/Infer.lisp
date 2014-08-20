

                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;       Infer Function/Chapter 6      ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;       simple code to run infer      ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

;;;  In order to run Infer you must load this file and follow the example shown

(defVar *DATABASE* () "Where the basic data is kept.")

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

;;;;;
#|this will take the form of a simple matcher and of an IS A process
boldface indicate user action - semicolon indicates what the program should return
(setq *database* '((man is a human)(human is a mammal)))
(lookup 'man)
;(man is a human)
(infer 'man)
;(man is a human is a mammal)
(add-to-database '(woman is a human))
;((woman is a human) (man is a human) (human is a mammal))
(infer 'woman)
;(woman is a human is a mammal)
we could obviously fill up these lists with musical items
(setq *database* '((c-d-e-c is a motive)(motive is a melody-fragment)))
(lookup 'c-d-e-c)
;(c-d-e-c is a motive)
(infer 'c-d-e-c)
;(c-d-e-c is a motive is a melody-fragment)
(add-to-database '(c-b-a-c is a motive))
;((c-b-a-c is a motive) (c-d-e-c is a motive) (motive is a melody-fragment))
(infer 'c-b-a-c)
;(c-b-a-c is a motive is a melody-fragment)
|#
;;;;;

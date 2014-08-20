


                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;       SPEAC Function/Chapter 7      ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;  toplevel define code to run SPEAC  ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

#|
? (run-the-program chopin-33-3 3)
((("statement") 1.27) 
(("preparation" "extension" "extension") 1.27)
 ((("preparation" "extension") 1.27) (("statement" "extension") 1.32)
  (("preparation" "extension") 1.23))
 ((("preparation" "extension" "extension" "extension" "preparation" "extension" "statement"
    "extension" "extension" "extension" "antecedent" "preparation" "extension" "statement"
    "antecedent" "statement")
   1.22)
  (("preparation" "extension" "extension" "statement" "extension" "extension" "extension"
    "extension" "extension" "extension" "extension" "extension" "preparation" "extension"
    "statement" "extension" "extension" "preparation" "extension" "extension" "extension"
    "statement" "extension" "extension" "antecedent")
   1.32)
  (("statement" "extension" "extension" "antecedent" "consequent" "statement" "extension")
   1.15)
  (("preparation" "extension" "preparation" "extension" "statement" "extension"
    "preparation" "extension" "statement" "extension" "extension" "antecedent" "statement"
    "extension" "extension" "extension" "extension" "extension" "extension" "extension"
    "extension" "preparation" "extension")
   1.5)
  (("preparation" "extension" "extension" "extension" "preparation" "extension" "statement"
    "extension" "extension" "extension" "antecedent" "preparation" "extension" "extension"
    "extension" "statement")
   1.22)
  (("preparation" "extension" "extension" "extension" "preparation" "extension" "statement"
    "extension" "antecedent" "statement" "antecedent" "statement" "preparation" "extension"
    "statement" "extension" "extension")
   1.24))
 ((a 0 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) (a 24000 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) (b 56000)
  (b 67000) (a 96000 (2 -2 -2 -1 1 1 1 1 1 -2 -2)) (a 120000 (2 -2 -2 -1 1 1 1 1 1 -2 -2))))
|#

;;;;;
#|(run-the-program
           '((0 55 1000 2 64) (0 65 1000 2 64) . . .
   ((("statement") 1.24)
   ((("preparation" . . . |#
;;;;;

(defun RUN-THE-PROGRAM (events meter)
"Top level for the form functions."
  (let* ((form (eval-combine-and-integrate-forms events))
         (phrased-events (break-into-phrases events (rest (mapcar #'second form)))) ;;these are the beginnings of each phrase
         (speak-phrase-lists (do-speac-on-phrases phrased-events meter))
         (speac-middleground (get-speac-middleground speak-phrase-lists (group-form form)))
         (speac-background (get-speac-background speac-middleground))
         (ursatz (get-speac-background (list speac-background))))
    (list ursatz speac-background speac-middleground speak-phrase-lists form)))
         
;;;;;
#|(get-speac-background
            '((("preparation" "extension" "extension" "extension" "extension"
               "extension" "antecedent" "preparation" "extension" "extension"
               "extension" "statement")
              1.24))) 
  (("statement") 1.24)|#
;;;;;

(defun GET-SPEAC-BACKGROUND (speac-middleground)
  "Returns the background of the SPEAC analysis."
    (let ((test (mapcar #'my-last speac-middleground)))
      (list (run-speac test (average test)) (average test))))

;;;;;
#|(get-speac-middleground
            '((("preparation" "extension" "extension" "extension" "preparation"
               "extension" "statement" "extension" "extension" "extension"
               "antecedent" "preparation" "extension" "statement")
              1.19) . . . 
    ((("preparation" "extension" "extension" "extension"  . . .|#
;;;;;

(defun GET-SPEAC-MIDDLEGROUND (speac-lists grouped-form)
  "Returns the middleground of the SPEAC analysis."
  (let ((grouped-speac-lists (group-speac-lists speac-lists grouped-form)))
    (loop for phrase in grouped-speac-lists 
          collect (let ((test (mapcar #'my-last phrase)))
                    (list (run-speac test (average test))(average test))))))

;;;;;
#|(group-speac-lists
             '((("preparation" "extension" "extension" "extension" "preparation"
                "extension" "statement" "extension" "extension" "extension"
                "antecedent" "preparation" "extension" "statement")
               1.19)
               (("preparation"  . . .
   (((("preparation" "extension" "extension" "extension" "preparation" "extension" . . .|#
;;;;;

(defun GROUP-SPEAC-LISTS (speac-lists grouped-form)
  "Groups the speac into appropriate lists."
  (if (null grouped-form)()
      (cons (firstn (length (first grouped-form)) speac-lists)
            (group-speac-lists (nthcdr (length (first grouped-form)) speac-lists)(rest grouped-form)))))

;;;;;
#|(group-form '((a 10000) (a 21000) (a 33000) (a 45000) (a 56000)
                (a 67000) (a 79000) (a 90000) (a 101000) (a 117000)
                (a 129000) (a 141000))) 
    (((a 10000) (a 21000) (a 33000) (a 45000) (a 56000)
     (a 67000) (a 79000) (a 90000) (a 101000) (a 117000)(a 129000) (a 141000)))|#
;;;;;

(defun GROUP-FORM (form &optional (element (very-first form)))
  "Groups the form elements."
  (if (null form)()
      (let ((test (group-them form element)))
        (cons test
              (group-form (nthcdr (length test) form))))))

;;;;;
#|(group-them '((a 10000) (a 21000) (a 33000) (a 45000) (a 56000)
                (a 67000) (a 79000) (a 90000) (a 101000) (a 117000)
                (a 129000) (a 141000))
                       a) 
   ((a 10000) (a 21000) (a 33000) (a 45000) (a 56000)
    (a 67000) (a 79000) (a 90000) (a 101000) (a 117000)(a 129000) (a 141000))|#
;;;;;

(defun GROUP-THEM (form &optional (element (very-first form)))
  "Sub function of group-form."
  (cond ((null form)())
        ((equal element (very-first form))
         (cons (first form)
               (group-them (rest form) element)))
        (t ())))
  
;;;;;
#|(do-speac-on-phrases
           '(((0 55 1000 2 64) (0 65 1000 2 64). . .
    ((("preparation" "extension" "extension" "extension" "preparation" . . .|#
;;;;;

(defun DO-SPEAC-ON-PHRASES (phrases meter)
  "Returns the SPEAC for each of the phrases in the arg."
  (if (null phrases)()
      (let ((weights (run-the-speac-weightings (first phrases) (get-the-start-beat-number (first phrases) meter) (round (/ (get-length (first phrases)) 1000)) meter)))
        (cons (list (run-speac weights (average weights))(average weights)) ;;;imp to include average here for higher functions!!!
              (do-speac-on-phrases (rest phrases) meter)))))

;;;;;
#|(break-into-phrases
           '((0 55 1000 2 64) (0 65 1000 2 64) (0 71 1000 2 64) (0 74 1000 1 64)
             (1000 65 1000 2 64). . .
     (((0 55 1000 2 64) (0 65 1000 2 64) (0 71 1000 2 64)
                               (0 74 1000 1 64) . . . |#
;;;;;

(defun BREAK-INTO-PHRASES (events timings)
  "Breaks its arg into logical phrases."
  (if (null timings)(list events)
      (cons (get-events-to (first timings) events)
            (break-into-phrases (get-events-from (first timings) events)(rest timings)))))

;;;;;
#|(get-events-to
            21000
            '((0 55 1000 2 64) . . .
((0 55 1000 2 64) (0 65 1000 2 64) (0 71 1000 2 64) (0 74 1000 1 64) . . .|#
;;;;;

(defun GET-EVENTS-TO (time events)
  "Returns all those events whose ontime is before time."
  (cond ((null events)())
        ((< (very-first events) time)
         (cons (first events)
               (get-events-to time (rest events))))
        (t (get-events-to time (rest events)))))

;;;;;
#|(get-events-from
            21000
            '((0 55 1000 2 64) . . .
   ((21000 55 1000 2 64) (21000 65 1000 2 64) (21000 71 1000 2 64)  . . .|#
;;;;;

(defun GET-EVENTS-FROM (time events)
  "Returns all those events whose ontime is after time."
  (cond ((null events)())
        ((>= (very-first events) time)
         (cons (first events)
               (get-events-from time (rest events))))
        (t (get-events-from time (rest events)))))

;;;;;
#|(get-length '((0 55 1000 2 64) (0 65 1000 2 64) (0 71 1000 2 64)
                (0 74 1000 1 64) . . 
21000|#
;;;;;

(defun GET-LENGTH (events &optional (begin (very-first events)))
  "Retruns the timed length of events."
  (- (+ (first (my-last events))(third (my-last events))) begin))

;;;;;
#|(get-the-start-beat-number
            '((21000 55 1000 2 64) . . . 3))
  1|#
;;;;;

(defun GET-THE-START-BEAT-NUMBER (events meter)
  "This assumes that everything starts on 0, even if the pieces first note is on 3000 for a pickup!!!"
  (let ((onbeat (round (/ (very-first events) 1000))))
    (1+ (mod onbeat meter))))



(defVar *OUTPUT* ())

#|(get-the-levels chopin-33-3)
(((s1)) ((p2 e2 e2)) ((p3 e3) (s3 e3) (p3 e3))
 ((p4 e4 e4 e4 p4 e4 s4 e4 e4 e4 a4 p4 e4 s4 a4 s4)
  (p4 e4 e4 s4 e4 e4 e4 e4 e4 e4 e4 e4 p4 e4 s4 e4 e4 p4 e4 e4 e4 s4 e4 e4 a4)
  (s4 e4 e4 a4 c4 s4 e4)
  (p4 e4 p4 e4 s4 e4 p4 e4 s4 e4 e4 a4 s4 e4 e4 e4 e4 e4 e4 e4 e4 p4 e4)
  (p4 e4 e4 e4 p4 e4 s4 e4 e4 e4 a4 p4 e4 e4 e4 s4)
  (p4 e4 e4 e4 p4 e4 s4 e4 a4 s4 a4 s4 p4 e4 s4 e4 e4)))|#

(defun GET-THE-LEVELS (events)
  (setq *output* (run-the-program events *meter*))
  (number-the-elements (create-the-window-levels *output*)))

#|(create-the-window-levels
           '((("statement") 1.27) . . 
(((s)) ((p e e)) ((p e) (s e) (p e))((p e e e  . . .|#

(defun CREATE-THE-WINDOW-LEVELS (levels-from-the-program)
  "Creates the levels for the form window."
  (let ((levels (wrap-first-two (firstn 4 levels-from-the-program))))
    (loop for phrase in levels
          collect (loop for element in phrase
                        collect (loop for speac in (first element)
                                      collect (DERIVE-SPEAC-SYMBOL speac))))))
#|(wrap-first-two
            ((("statement") 1.27) . . .
    (((("statement") 1.27))
     ((("preparation" . . |#

(defun WRAP-FIRST-TWO (lists)
  "A simple kludge to get two types of data to communicate."
  (list (list (first lists))(list (second lists))(third lists)(fourth lists)))
    
#|(derive-speac-symbol "statement") 
  s|#

(defun DERIVE-SPEAC-SYMBOL (speac-name)
  "Returns abbreviation from full speac-name."
  (cond ((equal speac-name "consequent") 'C)
        ((equal speac-name "antecedent") 'A)
        ((equal speac-name "statement") 'S)
        ((equal speac-name "extension") 'E)
        ((equal speac-name "preparation") 'P)
        ((equal speac-name "") "")))

#|(number-the-elements '(((c)) ((a c) (e c)) ((p a) (a a) (e e))
                   ((p p) (s p) (p a) (a a) (s e) (e e))))
(((c1)) ((a2 c2) (e2 c2)) ((p3 a3) (a3 a3) (e3 e3))
 ((p4 p4) (s4 p4) (p4 a4) (a4 a4) (s4 e4) (e4 e4)))|#

(defun NUMBER-THE-ELEMENTS (levels &optional (level-numbers '(1 2 3 4 5 6 7 8 9)))
  "Numbers the level elements in *levels* for final chart output."
     (loop for level in levels
           collect (loop for phrase in level
                         collect (loop for element in phrase
                                       collect (concat element (first level-numbers))))
           do (setf level-numbers (cdr level-numbers))))
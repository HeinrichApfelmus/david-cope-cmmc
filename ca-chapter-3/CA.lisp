

                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;        CA Function/Chapter 3        ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;        simple code to run CA        ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

#|(create-cellular-automata 262 186)|#

;;;  In order to run the CA function you must load this file on a 
;;;  Macintosh computer and then use Macintosh Common Lisp (MCL)
;;;  and the code (create-cellular-automata 262 186) as shown in the 
;;;  example for the top-level below. To change the rules, 
;;;  you must reset the variable *RULES* as shown in the examples
;;;  opening is the horizontal size (create-cellular-automata 262 186)
;;;  = fits the screen nicely and runs program
;;;  The last two functions make playable (with MIDI) events for QT.

(require 'quickdraw)   ;;;requires macintosh drawing primitives
(require :scrollers)   ;;;requires macintosh scrolling file

(defVar OPENING '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 * 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
  "this default determines the horizontal size of the output with odd numbers 
    working best and the * signifying the blackened square")
(defVar *ITERATIONS* 400 "number of iterations")
(defVar *BOX-SIZE* 4 "size of cell in pixels")
(defVar *START-POINT* 0 "start point at left")
(defVar STORE-RULES ())
(defVar *RULES* 
   '(((* * *) 0)((* * 0) 0)((* 0 *) 0)((* 0 0) *)
     ((0 * *) *)((0 * 0) *)((0 0 *) *)((0 0 0) 0))
   "this is rule 30, the default")
(defvar *events* ())

;;;;;
#| ? (create-cellular-automata 262 186)
(0 0 0 0 0 0 0 0 . . .
#<SCROLLING-WINDOW "Cellular Automata" #xFE95E7E>|#
;;;;;

(defun CREATE-CELLULAR-AUTOMATA (horizontal vertical)
  "Top-level function with the two args referring to size of visual image output -
      good size being 262 186 for standard monitor. the larger the size the longer it takes to create
      see below for good variations of the rules variable."
  (setq opening (make-opening horizontal))       ;;;defines the opening 
  (setq *iterations* vertical)                   ;;;changes the iterations to vertical size
  (make-instance 'scrolling-window               ;;;creates the window
    :scroller-class 'scrolling-window-scroller1
    :window-type :document
    :view-size (make-point (- *screen-width* 26) (- *screen-height* 46))
    :window-title "Cellular Automata" 
    :track-thumb-p t))

;;;;;
#| ? (make-opening 262)
(0 0 0 0 0 0 0 0 . . .|#
;;;;;

(defun MAKE-OPENING (n)
  "Creates the opening number of properly defined cells."
  (let ((half (make-list (round (/ (1- n) 2)) :initial-element 0)))
    (append half '(*) half)))

;;;;;
#| ? (CREATE-MATRIX #<SCROLLING-WINDOW-SCROLLER1 #x1008DF56> (0 0 0 0 0 0 0 0 0 0 0 0 * 
          0 0 0 0 0 0 0 0 0 0 0 0) 
          (((* * *) 0) ((* * 0) 0) ((* 0 *) 0) ((* 0 0) *) 
           ((0 * *) *) ((0 * 0) *) ((0 0 *) *) ((0 0 0) 0)))
NIL|#
;;;;;

(defun CREATE-MATRIX (window opening rules)
"Creates the full matrix of * and 0 and places it in the window - returns nil."
  (set-pen-pattern window *gray-pattern*)
  (let ((info (create *iterations* opening rules))
        (current-position (list *box-size* *box-size*)))
    (loop for line in info
          do (loop for data in line
                   do (if (equal data 0)
                        (frame-rect window 
                                    (first current-position)
                                    (second current-position)
                                    (+ (first current-position) *box-size*)
                                    (+ (second current-position) *box-size*))
                        (paint-rect window 
                                    (first current-position)
                                    (second current-position)
                                    (+ (first current-position) *box-size*)
                                    (+ (second current-position) *box-size*)))
                   do (setf current-position (list (+ (first current-position) (1- *box-size*))
                                                   (second current-position))))
          do (setf current-position (list *box-size* (+ (second current-position) (1- *box-size*)))))))

;;;;;
#| ? (CREATE 25 (0 0 0 0 0 0 0 0 0 0 0 0 * 0 0 0 0 0 0 0 0 0 0 0 0) 
   (((* * *) 0) ((* * 0) 0) ((* 0 *) 0) ((* 0 0) *) 
    ((0 * *) *) ((0 * 0) *) ((0 0 *) *) ((0 0 0) 0)))
((0 0 0 0 0 0 0 0 0 0 0 0 * 0  . . .|#
;;;;;

(defun CREATE (number start rules)
  "Simple upper level, runs create-lists."
  (setq store-rules ())
  (create-lists number start rules))

;;;;;
#| ? (CREATE-LISTS 25 (0 0 0 0 0 0 0 0 0 0 0 0 * 0 0 0 0 0 0 0 0 0 0 0 0) 
      (((* * *) 0) ((* * 0) 0) ((* 0 *) 0) ((* 0 0) *) ((0 * *) *) 
       ((0 * 0) *) ((0 0 *) *) ((0 0 0) 0)))
((0 0 0 0 0 0 0 0 0 0 0 0 * . . . |#
;;;;;

(defun CREATE-LISTS (number start rules)
  "Creates all of the rows of the automata in lists."
  (if (zerop number) (reverse store-rules)
      (progn (push start store-rules)
             (create-lists (1- number) 
                           (cons '0 (butlast (create-row start rules))) rules))))

;;;;;
#| ? (CREATE 25 (0 0 0 0 0 0 0 0 0 0 0 0 * 0 0 0 0 0 0 0 0 0 0 0 0) 
        (((* * *) 0) ((* * 0) 0) ((* 0 *) 0) ((* 0 0) *) ((0 * *) *) 
         ((0 * 0) *) ((0 0 *) *) ((0 0 0) 0)))
((0 0 0 0 0 0 0 0 0 0 0 0 * 0 0 . . . |#
;;;;;

(defun CREATE-ROW (old-row rules)
  "Creates all of the rows of the automata in lists."
  (if (null old-row) ()
      (cons (apply-rule (firstn 3 old-row) rules)
            (create-row (rest old-row) rules))))

;;;;;
#| ? (APPLY-RULE (0 NIL NIL) (((* * *) 0) ((* * 0) 0) ((* 0 *) 0) ((* 0 0) *)
         ((0 * *) *) ((0 * 0) *) ((0 0 *) *) ((0 0 0) 0)))
0|#
;;;;;

(defun APPLY-RULE (group rules)
  "Applies the rule to successive a cell of each line of the graphic."
  (let ((test (second (assoc group rules :test #'equal))))
    (cond (test)
          (t '0))))
;;;;;
#| ? (firstn 3 '(1 2 3 4 5))
(1 2 3)|#
;;;;;

(defun FIRSTN (n list)
  "Utility, returns n number of elements of list."
  (if (zerop n) ()
      (cons (first list)
            (firstn (1- n)(cdr list)))))

;;;;;
#|all of the necessary 
window classes|#
;;;;;

(defClass SCROLLING-WINDOW (window) ((my-scroller :accessor my-scroller)))

(defMethod INITIALIZE-INSTANCE ((self SCROLLING-WINDOW) &rest rest &key
                                    (scroller-class 'scroller)
                                    scroll-bar-class h-scroll-class v-scroll-class
                                    track-thumb-p field-size)
  (declare (dynamic-extent rest))
  ; We use the values of these keywords by modifying the rest parameter
  (declare (ignore scroll-bar-class h-scroll-class v-scroll-class
                   track-thumb-p field-size))
  (call-next-method)
  (let* ((handle (cons nil rest)))
    (declare (dynamic-extent handle) (type cons handle))
    (do ((tail handle))
        ((null (cdr tail)) (setq rest (cdr handle)))
      (declare (type cons tail))
      (if (memq (cadr tail) 
                '(:scroll-bar-class :h-scroll-class :v-scroll-class
                  :track-thumb-p :field-size))
        (setq tail (cddr tail))
        (setf (cdr tail) (cdr (cddr tail))))))
  (setf (my-scroller self) (apply #'make-instance
                                  scroller-class
                                  :view-container self
                                  :view-size (subtract-points
                                              (view-size self) #@(15 15))
                                  :view-position #@(0 0)
                                  :draw-scroller-outline nil
                                  rest)))

(defMethod SET-VIEW-SIZE ((self SCROLLING-WINDOW) h &optional v)
  (declare (ignore h v))
  (without-interrupts
   (call-next-method)
   (let* ((new-size (subtract-points (view-size self) #@(15 15))))    
     (set-view-size (my-scroller self) new-size))))

(defMethod WINDOW-ZOOM-EVENT-HANDLER ((self SCROLLING-WINDOW) message)
  (declare (ignore message))
  (without-interrupts
   (call-next-method)
   (let* ((new-size (subtract-points (view-size self) #@(15 15))))
     (set-view-size (my-scroller self) new-size))))

(defClass SCROLLING-WINDOW2 (window) ((my-scroller :accessor my-scroller)))

(defMethod INITIALIZE-INSTANCE ((self SCROLLING-WINDOW2) &rest rest &key
                                    (scroller-class 'scroller)
                                    scroll-bar-class h-scroll-class v-scroll-class
                                    track-thumb-p field-size)
  (declare (dynamic-extent rest))
  (declare (ignore scroll-bar-class h-scroll-class v-scroll-class
                   track-thumb-p field-size))
  (call-next-method)
  (let* ((handle (cons nil rest)))
    (declare (dynamic-extent handle) (type cons handle))
    (do ((tail handle))
        ((null (cdr tail)) (setq rest (cdr handle)))
      (declare (type cons tail))
      (if (memq (cadr tail) 
                '(:scroll-bar-class :h-scroll-class :v-scroll-class
                  :track-thumb-p :field-size))
        (setq tail (cddr tail))
        (setf (cdr tail) (cdr (cddr tail))))))
  (setf (my-scroller self) (apply #'make-instance
                                  scroller-class
                                  :view-container self
                                  :view-size (subtract-points
                                              (view-size self) #@(15 15))
                                  :view-position #@(0 0)
                                  :draw-scroller-outline nil
                                  rest)))

(defClass SCROLLING-WINDOW-SCROLLER1 (scroller)  
((my-rules :initarg :my-rules :initform *rules* :accessor my-rules))
  (:default-initargs
    :field-size (make-point 
                 (* *box-size* (length opening))
                 (* *box-size* *iterations*))))

(defMethod VIEW-DRAW-CONTENTS ((window SCROLLING-WINDOW-SCROLLER1))
  (create-matrix window opening (my-rules window)))

#|some sample rules 

;;;this is rule 22
(setq *RULES* 
   '(((* * *) 0)((* * 0) 0)((* 0 *) 0)((* 0 0) *)
     ((0 * *) 0)((0 * 0) *)((0 0 *) *)((0 0 0) 0)))

;;;this is rule 57
(setq *RULES* 
   '(((* * *) 0)((* * 0) 0)((* 0 *) *)((* 0 0) *)
     ((0 * *) *)((0 * 0) 0)((0 0 *) 0)((0 0 0) *)))

;;;this is rule 73
(setq *RULES* 
   '(((* * *) 0)((* * 0) *)((* 0 *) 0)((* 0 0) 0)
     ((0 * *) *)((0 * 0) 0)((0 0 *) 0)((0 0 0) *)))

;;;this is rule 75
(setq *RULES* 
   '(((* * *) 0)((* * 0) *)((* 0 *) 0)((* 0 0) 0)
     ((0 * *) *)((0 * 0) 0)((0 0 *) *)((0 0 0) *)))

;;;this is rule 121
(setq *RULES* 
   '(((* * *) 0)((* * 0) *)((* 0 *) *)((* 0 0) *)
     ((0 * *) *)((0 * 0) 0)((0 0 *) 0)((0 0 0) *)))
|#

#|
;;;  musical version of CA.
;;;Do not try to run this with the visual version up above
;;;this code is only for making events as shown below.
;;;If you wish to try this and then return to the visual version make sure and 
;;;evaluate create-lists above again before you do!
(defVar *EVENTS* ())

#|(create-lists 20 
              '(0 0 0 0 0 0 0 0 0 0 0 0 * 0 0 0 0 0 0 0 0 0 0 0 0) 
              '(((* * *) 0)((* * 0) *)((* 0 *) 0)((* 0 0) *)
                ((0 * *) *)((0 * 0) *)((0 0 *) 0)((0 0 0) 0)))
((4750 41 250 1 100) (4750 41 250 1 100) (4750 41 250 1 100) (4750 40 250 1 100) (4750 40 250 1 100) (4750 39 . . .
|#

(defun CREATE-LISTS (number start rules &optional (up-number 1))
  "The musical version of create-lists."
  (if (equal number up-number) (reverse store-rules)
      (progn (push start store-rules)
             (create-lists number
                           (cons '0 (butlast (create-the-row start rules up-number))) 
                           rules 
                           (1+ up-number))
            (reverse *events*))))

;;;;;
#|(CREATE-THE-ROW '(0 0 0 0 0 0 0 0 0 0 0 0 * * 0 0 0 0 0 0 0 0 0 0 0) '(((* * *) 0) ((* * 0) *) ((* 0 *) 0) ((* 0 0) *) ((0 * *) *) ((0 * 0) *) ((0 0 *) 0) ((0 0 0) 0)) 2)
   (0 0 0 0 0 0 0 0 0 0 0 * * * 0 0 0 0 0 0 0 0 0 0 0)|#
;;;;;

(defun CREATE-THE-ROW (old-row rules timing &optional (position 1))
  "Creates a new row from the previous row using rules."
  (if (null old-row) ()
      (cons (apply-the-rule (firstn 3 old-row) rules position timing)
            (create-the-row (rest old-row) rules timing (1+ position)))))

;;;;;
#| (APPLY-THE-RULE '(0 0 *) 
    '(((* * *) 0) ((* * 0) *) ((* 0 *) 0) ((* 0 0) *) ((0 * *) *) 
      ((0 * 0) *) ((0 0 *) 0) ((0 0 0) 0)) 11 1) 
   0|#
;;;;;

(defun APPLY-THE-RULE (group rules position timing)
  "Applies the rule for individual cases."
  (let ((test (second (assoc group rules :test #'equal))))
    (progn (if (equal test '*)(push (create-event position timing) *events*))
           (cond (test)
                 (t '0)))))

;;;;;
#|(CREATE-EVENT 12 1) 
    (250 39 250 1 100)|#
;;;;;

(defun CREATE-EVENT (position timing)
  "Creates a single event from timing."
  (list (* timing 250) (round (+ (* position (/ 60 261)) 36)) 250 1 100))
|#


                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;       SPEAC Function/Chapter 7      ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;     analysis code to run SPEAC      ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

(require 'quickdraw)

;;;;;
#|(average '(0.52 0.52 0.42 0.42 0.42 2.1 1.3 0.5 0.95 2.08 0.52 1.6 1.35
                   0.43 0.15 0.15))
0.84|#
;;;;;

(defun AVERAGE (list-of-values)
  "Computes the average of its argument."
  (my-round (/ (apply #'+ list-of-values)(length list-of-values))))

;;;;;
#|(create-chart '(0.2 0.05 0.1 0.05 0.2 0.05 0.1 0.05) "dave")
#<my-analysis-window "SPEAC" #x1F93F80E>|#
;;;;;

(defun CREATE-CHART (values name-of-db)
  "The top-level chart-maker - note that the second arg is a string."
  (setq *collapse* 1)
  (setq *values* values)
  (if (> (length *values*) 16)
    (setq *collapse* (round (/ (round (length *values*)) 12))))
  (setq *settings* (translate-values-into-rectangles values))
  (setq *settings* (dimension *settings*))
  (setq *settings* (mapcar #'(lambda (x)
                               (append (list (round (/ (first x) *collapse*)))
                                       (list (second x))
                                       (list (round (/ (third x) *collapse*)))
                                       (last x))) *settings*))
  (setq *name* name-of-db)
  (setq *window* (make-instance 'my-analysis-window 
                   :view-size (make-point (let ((test (+ (first (first (last *settings*))) 50)))
                                            (if (< test 200) 200 test)) 300)
                   :the-values *values* 
                   :settings *settings* 
                   :name *name* 
                   :beatsize *beat-size* 
                   :the-dimension *dimension* 
                   :hierarchical *hierarchical* 
                   :the-collapse *collapse*)))

;;;;;
#|(translate-values-into-rectangles '(0.52 0.52 0.42 0.42 0.42 2.1 1.3 0.5 0.95 2.08 0.52 1.6 1.35 0.43 0.15 0.15)) 
 ((10 230 45 270) (45 230 80 270) (80 237 115 270) (115 237 150 270) . . .|#
;;;;;

(defun TRANSLATE-VALUES-INTO-RECTANGLES (values &optional (left 10)(right 45))
  "Takes a series of ratings and turns them into chart positions."
  (if (null values)()
      (cons (list left
                  (- 269 (round (* (first values) 75)))
                  right
                  270)
            (translate-values-into-rectangles (cdr values)(+ left 35)(+ right 35)))))

(defClass MY-ANALYSIS-WINDOW (window)
  ((settings :initarg :settings :initform nil :accessor settings)
   (beatsize :initarg :beatsize :initform nil :accessor beatsize)
   (name :initarg :name :initform nil :accessor name)
   (the-values :initarg :the-values :initform nil :accessor the-values)
   (the-dimension :initarg :the-dimension :initform nil :accessor the-dimension)
   (hierarchical :initarg :hierarchical :initform nil :accessor hierarchical)
   (the-collapse :initarg :the-collapse :initform nil :accessor the-collapse))
  (:default-initargs :window-type :document
    :window-title "SPEAC"
    :view-size #@(580 320)
    :view-position #@(80 50)))

(defMethod VIEW-DRAW-CONTENTS ((window MY-ANALYSIS-WINDOW))
  (set-view-font window :plain)
  (set-view-font window 9)
  (set-view-font window "times")
  (mapcar #'(lambda (x)
              (invert-rect window (+ 3 (first x))
                           (+ 3 (second x))(+ 3 (third x))(+ 3 (fourth x)))) 
          (settings window))
  (setq *textures* '(*gray-pattern* *light-gray-pattern* *gray-pattern* *white-pattern*
                     *gray-pattern* *light-gray-pattern* *dark-gray-pattern* *white-pattern*
                     *black-pattern*))
  (mapcar #'(lambda (x)(fill-rect window (progn
                                           (if *hierarchical* 
                                             (eval (pop *textures*))
                                             *gray-pattern*)) (first x)(second x)(third x)(fourth x))) (settings window))
  (mapcar #'(lambda (x)(frame-rect window (first x)(second x)(third x)(fourth x))) (settings window))
  (setq *positions* (mapcar #'(lambda (y)
                                (list (+ 2 (first y))(- (second y) 2))) (settings window)))
  (mapcar #'(lambda (y)(let ((position (pop *positions*)))
                         (move-to window (+ 2 (first position)) (- (second position) 2))
                         (format window "~A" (write-to-string y)))) 
          (cond ((> *collapse* 2)
                 (mapcar #'(lambda (x)(round x))(the-values window)))
                ((> *collapse* 1)(mapcar #'(lambda (x)(round-it x))(the-values window)))
                (t (the-values window))))
  (set-view-font window :bold)
  (set-view-font window 14)
  (move-to window 10 15)
  (set-view-font window 12)
  (move-to window 10 35)
  (set-view-font window 12)
  (if (> *dimension* 1)
    (progn (move-to window 110 35)
           (format window "~A~A" "Dimension: " (the-dimension window))))
  #|(if (> *collapse* 1)
    (progn (move-to window 200 35)
           (format window "~A~A" "Accordian: " (the-collapse window))))|#
;(if (> *collapse* 1)
    (progn (move-to window 20 35)
           (format window "~A~A~A~A~A~A" "Beats " (write-to-string *beginning-beat*) " to " (write-to-string *ending-beat*)
" of " (write-to-string (first *target-item*))));)
  (set-view-font window "palatino")
  (setq *speac* (run-speac (the-values window) (average (the-values window))))
  (setq *positions* (mapcar #'(lambda (y)
                                (+ (round (/ 16 *collapse*))(first y))) (settings window)))
  (mapcar #'(lambda (y)(let ((position (pop *positions*)))
                         (move-to window position 286)
                         (format window "~A"  (speac-derive-symbol y)))) *speac*)
  (setq *hierarchical* ()))

;;;;;
#|(speac-derive-symbol "consequent") 
 "C"|#
;;;;;

(defun SPEAC-DERIVE-SYMBOL (speac-name)
  "Returns abbreviation from full speac-name."
  (cond ((equal speac-name "consequent") "C")
        ((equal speac-name "antecedent") "A")
        ((equal speac-name "statement") "S")
        ((equal speac-name "extension") "E")
        ((equal speac-name "preparation") "P")
        ((equal speac-name "") "")
        ((equal speac-name 'consequent) 'C)
        ((equal speac-name 'antecedent) 'A)
        ((equal speac-name 'statement) 'S)
        ((equal speac-name 'extension) 'E)
        ((equal speac-name 'preparation) 'P)))

;;;;;
#|(group-into 1000 '(0.52 0.42 2.1 0.5 5.1 2.88 2.73 0.15))
((0.52 0.42 2.1 0.5) (5.1 2.88 2.73 0.15))|#
;;;;;

(defun GROUP-INTO (n list)
  "Groups list by n."
  (if (null list)()
      (let ((number (* (/ n 1000) 4)))
        (cons (firstn number list)
              (group-into n (nthcdr number list))))))

;;;;;
#|(dimension '((10 230 45 270) (45 237 80 270) (80 237 115 270)
                     (115 261 150 270) (150 198 185 270) (185 230 220 270)
                     (220 168 255 270) (255 258 290 270)))
((10 230 45 270) (45 237 80 270) (80 237 115 270) (115 261 150 270) (150 198 185 270) (185 230 220 270) (220 168 255 270) (255 258 290 270))|#
;;;;;

(defun DIMENSION (settings)
  "Returns the dimensions of the settings."
  (setq *dimension* (get-dimension settings))
  (mapcar #'(lambda (x)(cons (first x)
                             (cons (round (if (minusp (second x))
                                            (+ (* (+ (abs (second x)) 269)(- 1 (/ 1 *dimension*)))(second x))
                                            (+ (* (- 269 (second x))(- 1 (/ 1 *dimension*))) (second x))))
                                   (nthcdr 2 x))))
          settings))

;;;;;
#|(get-dimension '((10 230 45 270) (45 237 80 270) (80 237 115 270) (115 261 150 270) (150 198 185 270) (185 230 220 270) (220 168 255 270) (255 258 290 270))) 
   1|#
;;;;;

(defun GET-DIMENSION (settings &optional (dim 1))
  "Gets the true dimension from the settings."
  (if (< (first (sort (mapcar #'second settings) '<)) 30)
    (get-dimension (mapcar #'(lambda (x)(cons (first x)(cons (+ (second x) 269)(nthcdr 2 x))))
                           settings)(+ dim 1))
    dim))

;;;;;
#|(almost-the-same 1.1 1.2)
T|#
;;;;;

(defun ALMOST-THE-SAME (first second)
  "Determines whether the numbers are within .1 of each other."
  (if (and (<= (abs (- first second)) .1)) t))

;;;;;
#|(round-it 15.234)
15|#
;;;;;

(defun ROUND-IT (number)
  "Returns a number to one (if none) decimal points (if an integer)."
  (cond ((> number 10)(round (/ (round (* number 10)) 10)))
        ((zerop number) 0)
        ((and (> number 1)(zerop (rem number (round number))))(round number))
        (t (let ((test (/ (round (* number 10)) 10)))
             (if (and (>= test 1)(zerop (rem test (round test))))
               (round test)
               (float (/ (round (* number 10)) 10)))))))

;;;;;
#|(capture-beats book-example 1000)
(((0 45 1000 4 55) (0 64 1000 3 55) (0 69 1000 2 55) . . .|#
;;;;;

(defun CAPTURE-BEATS (music beat)
  "Returns the music in beat-size chunks."
  (collect-beats (sortcar #'< (break-events-into-beats beat music)) beat))

;;;;;
#|(collect-beats '((0 45 1000 4 55) (0 64 1000 3 55) (0 69 1000 2 55) (0 73 1000 1 55) (1000 57 1000 4 55) . . .
(((0 45 1000 4 55) (0 64 1000 3 55) . . .|#
;;;;;

(defun COLLECT-BEATS (clarified-music beat)
  "Returns the music in beats."
  (let ((collected-beat (collect-beat beat clarified-music))
        (accumulated-beat beat))
    (loop for event in clarified-music by #'(lambda (y)(nthcdr (length collected-beat) y))
          when collected-beat
          collect collected-beat
          do (setf accumulated-beat (+ accumulated-beat beat))
          do (car event)
          do (setf clarified-music (nthcdr (length collected-beat) clarified-music))
          do (setf collected-beat (collect-beat accumulated-beat clarified-music)))))

;;;;;
#|(collect-beat 6000 '((5000 55 1000 4 55) (5000 62 1000 3 55) . . .
((5000 55 1000 4 55) (5000 62 1000 3 55) (5000 67 500 2 55) (5500 66 500 2 55) (5000 71 1000 1 55))|#
;;;;;

(defun COLLECT-BEAT (beat clarified-music)
  "Collects a single beat from music."
  (loop for event in clarified-music 
        until (>= (first event) beat) 
        collect event))

;;;;;
#|(break-events-into-beats 1000 ((0 45 1000 4 55) . . .
((0 45 1000 4 55) (0 64 1000 3 55) . . .|#
;;;;;

(defun BREAK-EVENTS-INTO-BEATS (beat music)
  "Breaks the event if longer than a beat."
  (let ((accumulated-beat beat))
    (loop for event in music
          when (< (first event) accumulated-beat) 
          append (break-event beat event)
          when (>= (first event) accumulated-beat)
          append (break-event beat event)
          and do (setf accumulated-beat (+ accumulated-beat beat)))))

;;;;;
#|(break-event 8000 '(7000 57 1000 3 55)) 
((7000 57 1000 3 55))|#
;;;;;

(defun BREAK-EVENT (beat event)
  "Breaks the event if longer than a beat."
  (let ((ontime (first event))
        (duration (third event)))
    (loop until (zerop duration)
          collect (list ontime 
                        (second event)
                        (if (> duration beat) beat duration)
                        (fourth event)
                        (fifth event))
          do (setf ontime (+ ontime beat))
          do (setf duration (- duration (if (> duration beat) beat duration))))))

;;;;;
#|(BREAK-INTO-BEATS chopin-33-3)
((0 74 1000 1 64) (1000 76 1000 1 64) (2000 76 500 1 64)
 (2500 74 500 1 64) (3000 72 750 1 64) (3750 71 250 1 64)|#
;;;;;

(defun BREAK-INTO-BEATS (events)
  "Breaks events into beat-sized chunks - returns events with all half-notes, etc. as quarters."
  (sortcar #'< (apply #'append (chop-into-bites (sortcar #'< events)))))

;;;;;
#|(chop-into-bites
           '((0 74 1000 1 64) (1000 76 1500 1 64) . . . 
((0 74 1000 1 64) (1000 76 1500 1 64) (2500 74 500 1 64) . . . |#
;;;;;

(defun CHOP-INTO-BITES (events)
  "Chops beats into beat-size pieces if necessary."
  (cond ((null events)())
        ((and (equal (third (first events)) 1000)
              (thousandp (very-first events)))
         (cons (list (first events))
               (chop-into-bites (rest events))))
        ((> (third (first events)) 1000)
         (cons (chop (first events))
               (chop-into-bites (append (remainder (first events))(rest events)))))
        (t (cons (get-full-beat (get-channel (fourth (first events)) events))
                 (chop-into-bites (append (remainders (get-channel (fourth (first events)) events))
                                          (append (remove-full-beat (get-channel (fourth (first events)) events))
                                                  (get-other-channels (fourth (first events)) events))))))))

;;;;;
#|(chop '(1000 76 1500 1 64)) 
 ((1000 76 1000 1 64))|#
;;;;;

(defun CHOP (event &optional (begin-time (first event))(duration (third event)))
  "Chops beat into beat-size pieces if necessary."
  (if (< duration 1000)()
      (cons (append (list begin-time)
                    (list (second event))
                    '(1000)
                    (nthcdr 3 event))
            (chop event (+ begin-time 1000)(- duration 1000)))))
  
;;;;;
#|(remainder '(1000 76 1500 1 64)) 
  ((2000 76 500 1 64))|#
;;;;;

(defun REMAINDER (event &optional (begin-time (first event))(duration (third event)))
  "Returns the remainder of a beat if necessary."
  (cond ((null events)())
        ((= duration 1000)())
        ((< duration 1000) (list (append (list begin-time)
                                         (list (second event))
                                         (list duration)
                                         (nthcdr 3 event))))
      (t (remainder event (+ begin-time 1000)(- duration 1000)))))

;;;;;
#|(get-full-beat
            '((2000 76 500 1 64) (2500 74 500 1 64) . . .
((2000 76 500 1 64) (2500 74 500 1 64))|#
;;;;;

(defun GET-FULL-BEAT (events &optional (begin-time (very-first events))(duration 0))
  "Returns the full beat in all voices without remainders."
  (cond ((null events)())
        ((= (+ duration (third (first events))) 1000)
         (list (first events)))
        ((> (+ duration (third (first events))) 1000)
         (list (append (firstn 2 (first events))
                       (list (- 1000 duration))
                       (nthcdr 3 (first events)))))
        (t (cons (first events)
                 (get-full-beat (rest events)
                                (+ begin-time (third (first events)))
                                (+ (third (first events)) duration))))))
             
;;;;;
#|(remainders '((2000 76 500 1 64) (2500 74 500 1 64)
                       (3000 72 750 1 64) (3750 71 250 1 64) . . .
nil|#
;;;;;

(defun REMAINDERS (events &optional (begin-time (very-first events))(duration 0))
  "Returns the remainders of the events."
  (cond ((null events)())
        ((= (+ duration (third (first events))) 1000)
         ())
        ((> (+ duration (third (first events))) 1000)
         (list (append (list (+ begin-time (- 1000 duration)))
                       (list (second (first events)))
                       (list (- (third (first events)) (- 1000 duration)))
                       (nthcdr 3 (first events)))))
        (t (remainders (rest events)
                       (+ begin-time (third (first events)))
                       (+ (third (first events)) duration)))))

;;;;;
#|(remove-full-beat
            '((2000 76 500 1 64) . . .
((3000 72 750 1 64) (3750 71 250 1 64)
 (4000 72 1500 1 64) (5500 73 500 1 64) . . .|#
;;;;;

(defun REMOVE-FULL-BEAT (events &optional (begin-time (very-first events))(duration 0))
  "Removes the first full beat of events."
  (cond ((null events)())
        ((>= (+ duration (third (first events))) 1000)
         (rest events))
        (t (remove-full-beat (rest events)
                             (+ begin-time (third (first events)))
                             (+ (third (first events)) duration)))))

;;;;;
#|(get-other-channels
            1
            '((2000 76 500 1 64) (2500 74 500 1 64) . . .
nil|#
;;;;;

(defun GET-OTHER-CHANNELS (channel-not-to-get events)
  "Returns the other channels beside channel-not-to-get, if any."
  (cond ((null events)())
        ((equal (fourth (first events)) channel-not-to-get)
         (get-other-channels channel-not-to-get (rest events)))
        (t (cons (first events)
                 (get-other-channels channel-not-to-get (rest events))))))

;;;;;
#|(get-the-speac
             '((0 60 1000 4 127) (0 67 1000 2 127) (0 64 1000 3 127)))
consequent|#
;;;;;

(defun GET-THE-SPEAC (loaded-phrase-name)
  "Returns the SPEAC analysis of phrase."
loaded-phrase-name
  (loop for item in (reverse (run-speac *weights* (average *weights*)))
        if (not (string= item ""))
        return (read-from-string item)))

;;;;;
#|(speac 0.56 0.41 0.78) 
 speac returned "consequent"|#
;;;;;

(defun SPEAC (previous current next)
  "Returns a stringed SPEAC symbol representing its argument based on
    various standards including the values of its preceding and following symbols."
  (cond ((and (null previous) next (almost-the-same current next))
         "statement")
        ((and (null previous) next (> current next))
         "antecedent")
        ((null previous) "preparation")
        ((and (null next)(> previous current))
         "consequent")
        ((and (null next)(< previous current))
         "antecedent")
        ((and (null next)(almost-the-same previous current))
         "")
        ((null next)
         "consequent")
        ((almost-the-same previous current) "")
        ((and (< previous current)(< current next)) "preparation")
        ((< previous current) "antecedent")
        (t "consequent")))

;;;;;
#|(run-speac '(0.56 0.41 0.78 0.51 1.33 0.51 1.26 0.51) 0.73)
("preparation" "extension" "statement" "extension" "antecedent" "consequent" "antecedent" "consequent")|#
;;;;;

(defun RUN-SPEAC (weights average)
  "Runs SPEAC using weights and the overall average."
  (develop-speac weights  average (first (my-sort #'> weights))(first (my-sort #'< weights))))

;;;;;
#|(develop-speac '(0.56 0.41 0.78 0.51 1.33 0.51 1.26 0.51) 0.73 1.33 0.41)
("preparation" "extension" "statement" "extension" "antecedent" "consequent" "antecedent" "consequent")|#
;;;;;

(defun DEVELOP-SPEAC (weights average largest smallest &optional (previous-weight nil)(previous-assignment nil))
  "Returns a stringed SPEAC symbol representing its argument based on
    various standards including the values of its preceding and following symbols."
  (cond ((null weights)())
        ((almost (first weights) previous-weight .2) (cons "extension" (develop-SPEAC (rest weights) average largest smallest (first weights) "extension")))
        ((almost (first weights) (second weights) .2) (cons (if (equal previous-assignment "preparation") "extension" "preparation") (develop-SPEAC (rest weights) average largest smallest (first weights) (if (equal previous-assignment "preparation") "extension" "preparation"))))
        ((almost (first weights) average .2) (cons (if (equal previous-assignment "statement") "extension" "statement") (develop-SPEAC (rest weights) average largest smallest (first weights) "statement")))
        ((almost (first weights) largest .2) 
         (cons (if (equal previous-assignment "antecedent") "extension" "antecedent") (develop-SPEAC (rest weights) average largest smallest (first weights) (if (equal previous-assignment "antecedent") "extension" "antecedent"))))
        ((and (equal previous-assignment "antecedent")(almost (first weights) smallest .2))
         (cons (if (equal previous-assignment "consequent") "extension" "consequent") (develop-SPEAC (rest weights) average largest smallest (first weights) "consequent")))
        (t (cons (if (equal previous-assignment "statement") "extension" "statement") (develop-SPEAC (rest weights) average largest smallest (first weights) "statement")))))

;;;;;
#|(almost 1.26 1.33 0.2)
t|#
;;;;;

(defun ALMOST (first second allowance)
  "Returns t is args fall within allowance of one another."
  (cond ((or (null first)(null second))())
        ((< (abs (- first second)) allowance) t)))

(require :scrollers)

(defClass SCROLLING-WINDOW (window) ((my-scroller :accessor my-scroller)))

(defMethod INITIALIZE-INSTANCE ((self SCROLLING-WINDOW) &rest rest &key
                                    (scroller-class 'scroller)
                                    scroll-bar-class h-scroll-class v-scroll-class
                                    track-thumb-p field-size)
  (declare (dynamic-extent rest))
  (declare (ignore scroll-bar-class h-scroll-class v-scroll-class
                   track-thumb-p field-size))
  (call-next-method)
  (setq *chosen-staff* 1)
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
  (declare (ignore v h))
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

(defClass MY-SCROLLING-WINDOW (scrolling-window) ()
  (:default-initargs
    :field-size #@(10220 220)))

(defClass SCORE-WINDOW (window) ((my-scroller :accessor my-scroller)))

(defMethod INITIALIZE-INSTANCE ((self SCORE-WINDOW) &rest rest &key
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

(defMethod SET-VIEW-SIZE ((self SCORE-WINDOW) h &optional v)
  (declare (ignore v h))
  (without-interrupts
   (call-next-method)
   (let* ((new-size (subtract-points (view-size self) #@(15 15))))    
     (set-view-size (my-scroller self) new-size))))

(defMethod WINDOW-ZOOM-EVENT-HANDLER ((self SCORE-WINDOW) message)
  (declare (ignore message))
  (without-interrupts
   (call-next-method)
   (let* ((new-size (subtract-points (view-size self) #@(15 15))))
     (set-view-size (my-scroller self) new-size))))

(defClass MY-SCROLLING-WINDOW (score-window) ()
  (:default-initargs
    :field-size #@(10220 220)))

;;;;;
#|(thousandp 1223)
nil|#
;;;;;

(defun THOUSANDP (number)
  "Returns t if number is some pultiple of one thousand."
  (if (zerop (mod number 1000)) t))

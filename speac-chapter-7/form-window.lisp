


                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;       SPEAC Function/Chapter 7      ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;    form-window code to run SPEAC    ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;


;;;;;
#|(check-data book-example)
((0 45 1000 4 55) (0 64 1000 3 55) . . .|#
;;;;;

(defun CHECK-DATA (data)
  "Checks to ensure correct type of data for events."
  (if (not (listp (first data)))(eval (first data)) data))

;;;;;
#|(make-form-window)
nil|#
;;;;;

(defun MAKE-FORM-WINDOW ()
  "Creates the form window for viewing."
  (set-back-color (setq *form-window* (make-instance 'scrolling-window
                                        :scroller-class 'scrolling-form-scroller
                                        :window-type :document-with-zoom
                                        :window-title (write-to-string (first *target-item*))
                                        :view-size #@(750 520)
                                        :view-position #@(60 40)
                                        :track-thumb-p t)) *black-color*))

(defClass SCROLLING-FORM-SCROLLER (scroller) 
  ((levels :initarg :levels :initform *levels* :accessor levels))
  (:default-initargs
    :field-size #@(2200 2220)))

(defMethod VIEW-DRAW-CONTENTS ((window SCROLLING-FORM-SCROLLER))
  (set-pen-pattern window *black-pattern*)
  (let* ((level-number 0)(levels (levels window)))
    (set-view-font window :bold)
    (set-view-font window 18)
    (set-view-font window "times")
    (loop for element in (unwrap *levels*) ;collection
          do (move-to window (first (second element)) (second (second element)))
          do 
          (if *color* 
            (with-fore-color *yellow-color* 
              (format window "~A" (first element)))
            (format window "~A" (first element)))
          do (setf level-number (1+ level-number)))
    (draw-the-lines window levels)
    (set-view-font window :plain)
    (set-view-font window 14)
    (let ((positions (apply #'append (loop for entry in (my-last levels) ;*revised-levels*)
                                           collect (mapcar #'second entry)))))
      (loop for arc in *form-connections*
            do (let ((left (first (nth (1- (first arc)) positions)))
                     (right (first (nth (1- (second arc)) positions))))
                 (if *color* 
                   (with-fore-color *light-blue-color* (frame-arc window 
                                                                  90
                                                                  180
                                                                  (+ 6 left)
                                                                  (- (second  (nth (1- (first arc)) positions))
                                                                     (round (/ (- right left) 2)))
                                                                  (+ 6 right)
                                                                  (+ (second (nth (1- (second arc)) positions))  
                                                                     (round (/ (- right left) 2)))))
                   (frame-arc window 
                              90
                              180
                              (+ 6 left)
                              (- (second (nth (1- (first arc)) positions))
                                 (round (/ (- right left) 2)))
                              (+ 6 right)
                              (+ (second (nth (1- (second arc)) positions))
                                 (round (/ (- right left) 2))))))))))


;;;;;
#|(create-the-points
           '((((s1))) (((p2) (e2) (e2)))
             (((p3) (e3)) ((p3) (e3)) ((p3) (e3))))
            3)
((((s1 (128 85)))) . . .|#
;;;;;

(defun CREATE-THE-POINTS (levels length &optional (h 0)(v 0))
  "Creates the points for drawing the window diagram."
  (let ((level-length))
    (loop for level in levels
          do (setf level-length (round (/ (* *size* length) (1+ (length (apply #'append level))))))
          do (setf h level-length)
          do (setf v (+ v *size*))
          collect (loop for thing in level
                        collect (loop for element in thing
                                      collect (list (first element) (list h v))
                                      do (setf h (+ h level-length)))))))

;;;;;
#|(get-appropriate-element e2 '(((a2 c1) (130 130)) ((e2 c1) (260 130)))) 
 (260 130)|#
;;;;;

(defun GET-APPROPRIATE-ELEMENT (element element-list)
  "Returns the hv of element."
  (cond ((null element-list)())
        ((equal element (first (first (first element-list))))
         (second (first element-list)))
        (t (get-appropriate-element element (nthcdr 1 element-list)))))

(defMethod VIEW-CLICK-EVENT-HANDLER ((self SCROLLING-FORM-SCROLLER) where)
  (let* ((h (point-h where))(v (point-v where))
         (letter-positions ;(apply #'append 
          (loop for element in (unwrap *levels*)
                ;collect (loop for entry in element 
                collect (second element)));))
         (text (loop for element in (unwrap *levels*)
                     collect (first element))))
    (loop for position in letter-positions
          if (and (> h (first position))(< h (+ 15 (first position)))
                  (< v (second position))(> v (- (second position) 15)))
          do (make-instance 'my-window :object (first text))
          else do (setf text (cdr text)))))

(defClass MY-WINDOW (window)
  ((object :initarg :object :initform nil :accessor object))
  (:default-initargs :window-type :document
    :window-title "SPEAC"
    :view-position #@(70 140)
    :view-size #@(436 164)
    :auto-position :centermainscreen))

(defMethod INITIALIZE-INSTANCE ((window MY-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs))

(defMethod VIEW-DRAW-CONTENTS ((window MY-WINDOW))
  (move-to window 5 20)
  (format window "Name: ~A~&"
          ;(eval 
          (object window))
  (format window "Speac: ~A~&"
          (first (explode (object window))))
  (format window "Level: ~A~&"
          (second (explode (object window))))
  #|(format window "Inheritance: ~A~&"
          (inheritance (eval (object window))))
  (format window "Offspring: ~A~&"
          (offspring (eval (object window))))
  (format window "Logic: ~A~&"
          (logic (eval (object window))))|#)

(defClass SPEAC-NODE ()
  ((name :initarg :name :initform nil :accessor name)
   (level :initarg :level :initform nil :accessor level)
   (inheritance :initarg :inheritance :initform nil :accessor inheritance)
   (offspring :initarg :offspring :initform nil :accessor offspring)
   (logic :initarg :logic :initform nil :accessor logic))
  (:documentation "this is the top-level object for sentences in a database file."))

(defClass BACK (view)())

(defMethod INITIALIZE-INSTANCE ((pie BACK) &rest initargs)
  (apply #'call-next-method pie initargs))

(defMethod VIEW-DRAW-CONTENTS ((pie BACK))
  (with-fore-color *gray-color*
    (paint-rect pie 0 0 670 610)))

(defClass PROGRAM-VARIABLES-WINDOW (window)
  nil
  (:default-initargs 
    :window-type :document
    :window-title "SPEAC Variables"
    :view-position #@(80 100)
    :view-size #@(475 270)))

(defMethod INITIALIZE-INSTANCE ((window PROGRAM-VARIABLES-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs) 
  (add-subviews window 
    (make-instance 'back    :view-position #@(0 0)
                   :view-size #@(613 417))
    (make-new-text-item #@(0 5) #@(100 22) "Minor 2nd")
    ;(make-new-text-item #@(160 3) #@(44 24) "1")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *minor-second*)
                   :view-size #@(28 10) 
                   :view-position #@(112 17)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *minor-second* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(0 25) #@(100 22) "Major 2nd")
    ;(make-new-text-item #@(160 23) #@(44 24) ".8")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *major-second*)
                   :view-size #@(28 10) 
                   :view-position #@(112 37)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *major-second* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(0 45) #@(100 22) "Minor 3rd")
    ;(make-new-text-item #@(160 43) #@(44 24) ".225")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *minor-third*)
                   :view-size #@(28 10) 
                   :view-position #@(112 57)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *minor-third* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(0 65) #@(100 22) "Major 3rd")
    ;(make-new-text-item #@(160 63) #@(44 24) ".2")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *major-third*)
                   :view-size #@(28 10) 
                   :view-position #@(112 77)
                   :view-font '("geneva" 9)                               
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *major-third* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(0 85) #@(100 22) "Perfect 4th")
    ;(make-new-text-item #@(160 83) #@(44 24) ".55")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *perfect-fourth*)
                   :view-size #@(28 10) 
                   :view-position #@(112 97)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *perfect-fourth* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(0 105) #@(100 22) "Aug. 4th")
    ;(make-new-text-item #@(160 103) #@(44 24) ".65")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *augmented-fourth*)
                   :view-size #@(28 10) 
                   :view-position #@(112 117)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *augmented-fourth* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(0 125) #@(100 22) "Perfect 5th")
    ;(make-new-text-item #@(160 123) #@(44 24) ".1")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *perfect-fifth*)
                   :view-size #@(28 10) 
                   :view-position #@(112 137)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *perfect-fifth* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(0 145) #@(100 22) "Minor 6th")
    ;(make-new-text-item #@(160 143) #@(44 24) ".275")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *minor-sixth*)
                   :view-size #@(28 10) 
                   :view-position #@(112 157)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *minor-sixth* (read-from-string (dialog-item-text item))))))                
    (make-new-text-item #@(0 165) #@(100 22) "Major 6th")
    ;(make-new-text-item #@(160 163) #@(44 24) ".25")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *major-sixth*)
                   :view-size #@(28 10) 
                   :view-position #@(112 177)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *major-sixth* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(0 185) #@(100 22) "Minor 7th")
    ;(make-new-text-item #@(160 183) #@(44 24) ".7")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *minor-seventh*)
                   :view-size #@(28 10) 
                   :view-position #@(112 197)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *minor-seventh* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(0 205) #@(100 22) "Major 7th")
    ;(make-new-text-item #@(160 203) #@(44 24) ".9")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *major-seventh*)
                   :view-size #@(28 10) 
                   :view-position #@(112 217)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *major-seventh* (read-from-string (dialog-item-text item))))))
    
    
    (make-new-text-item #@(160 5) #@(100 22) "Minor 9th")
    ;(make-new-text-item #@(160 3) #@(44 24) "1")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *minor-ninth*)
                   :view-size #@(28 10) 
                   :view-position #@(272 17)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *minor-ninth* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(160 25) #@(100 22) "Major 9th")
    ;(make-new-text-item #@(160 23) #@(44 24) ".8")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *major-ninth*)
                   :view-size #@(28 10) 
                   :view-position #@(272 37)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *major-ninth* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(160 45) #@(100 22) "Minor 10th")
    ;(make-new-text-item #@(160 43) #@(44 24) ".225")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *minor-tenth*)
                   :view-size #@(28 10) 
                   :view-position #@(272 57)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *minor-tenth* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(160 65) #@(100 22) "Major 10th")
    ;(make-new-text-item #@(160 63) #@(44 24) ".2")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *major-tenth*)
                   :view-size #@(28 10) 
                   :view-position #@(272 77)
                   :view-font '("geneva" 9)                               
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *major-tenth* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(160 85) #@(100 22) "Perfect 11th")
    ;(make-new-text-item #@(160 83) #@(44 24) ".55")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *perfect-eleventh*)
                   :view-size #@(28 10) 
                   :view-position #@(272 97)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *perfect-eleventh* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(160 105) #@(100 22) "Aug. 11th")
    ;(make-new-text-item #@(160 103) #@(44 24) ".65")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *augmented-eleventh*)
                   :view-size #@(28 10) 
                   :view-position #@(272 117)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *augmented-eleventh* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(160 125) #@(100 22) "Perfect 12th")
    ;(make-new-text-item #@(160 123) #@(44 24) ".1")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *perfect-twelveth*)
                   :view-size #@(28 10) 
                   :view-position #@(272 137)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *perfect-twelveth* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(160 145) #@(100 22) "Minor 13th")
    ;(make-new-text-item #@(160 143) #@(44 24) ".275")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *minor-thirteenth*)
                   :view-size #@(28 10) 
                   :view-position #@(272 157)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *minor-thirteenth* (read-from-string (dialog-item-text item))))))                
    (make-new-text-item #@(160 165) #@(100 22) "Major 13th")
    ;(make-new-text-item #@(160 163) #@(44 24) ".25")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *major-thirteenth*)
                   :view-size #@(28 10) 
                   :view-position #@(272 177)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *major-thirteenth* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(160 185) #@(100 22) "Minor 14th")
    ;(make-new-text-item #@(160 183) #@(44 24) ".7")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *minor-fourteenth*)
                   :view-size #@(28 10) 
                   :view-position #@(272 197)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *minor-fourteenth* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(160 205) #@(100 22) "Major 14th")
    ;(make-new-text-item #@(160 203) #@(44 24) ".9")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *major-fourteenth*)
                   :view-size #@(28 10) 
                   :view-position #@(272 217)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *major-fourteenth* (read-from-string (dialog-item-text item))))))
    
    
    (make-new-text-item #@(320 5) #@(100 22) "Minor 16th")
    ;(make-new-text-item #@(160 3) #@(44 24) "1")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *minor-sixteenth*)
                   :view-size #@(28 10) 
                   :view-position #@(432 17)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *minor-sixteenth* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(320 25) #@(100 22) "Major 16th")
    ;(make-new-text-item #@(160 23) #@(44 24) ".8")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *major-sixteenth*)
                   :view-size #@(28 10) 
                   :view-position #@(432 37)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *major-sixteenth* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(320 45) #@(100 22) "Minor 17th")
    ;(make-new-text-item #@(160 43) #@(44 24) ".225")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *minor-seventeenth*)
                   :view-size #@(28 10) 
                   :view-position #@(432 57)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *minor-seventeenth* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(320 65) #@(100 22) "Major 17th")
    ;(make-new-text-item #@(160 63) #@(44 24) ".2")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *major-seventeenth*)
                   :view-size #@(28 10) 
                   :view-position #@(432 77)
                   :view-font '("geneva" 9)                               
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *major-seventeenth* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(320 85) #@(100 22) "Perfect 18th")
    ;(make-new-text-item #@(160 83) #@(44 24) ".55")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *perfect-eighteenth*)
                   :view-size #@(28 10) 
                   :view-position #@(432 97)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *perfect-eighteenth* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(320 105) #@(100 22) "Aug. 18th")
    ;(make-new-text-item #@(160 103) #@(44 24) ".65")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *augmented-eighteenth*)
                   :view-size #@(28 10) 
                   :view-position #@(432 117)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *augmented-eighteenth* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(320 125) #@(100 22) "Perfect 19th")
    ;(make-new-text-item #@(160 123) #@(44 24) ".1")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *perfect-nineteenth*)
                   :view-size #@(28 10) 
                   :view-position #@(432 137)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *perfect-nineteenth* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(320 145) #@(100 22) "Minor 20th")
    ;(make-new-text-item #@(160 143) #@(44 24) ".275")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *minor-twentieth*)
                   :view-size #@(28 10) 
                   :view-position #@(432 157)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *minor-twentieth* (read-from-string (dialog-item-text item))))))                
    (make-new-text-item #@(320 165) #@(100 22) "Major 20th")
    ;(make-new-text-item #@(160 163) #@(44 24) ".25")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *major-twentieth*)
                   :view-size #@(28 10) 
                   :view-position #@(432 177)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *major-twentieth* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(320 185) #@(100 22) "Minor 21st")
    ;(make-new-text-item #@(160 183) #@(44 24) ".7")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *minor-twentyfirst*)
                   :view-size #@(28 10) 
                   :view-position #@(432 197)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *minor-twentyfirst* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(320 205) #@(100 22) "Major 21st")
    ;(make-new-text-item #@(160 203) #@(44 24) ".9")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *major-twentyfirst*)
                   :view-size #@(28 10) 
                   :view-position #@(432 217)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *major-twentyfirst* (read-from-string (dialog-item-text item))))))
    (make-new-text-item #@(150 235) #@(130 22) "Octave Separation")
    ;(make-new-text-item #@(160 203) #@(44 24) ".9")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *octave-separation*)
                   :view-size #@(28 10) 
                   :view-position #@(302 247)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *octave-separation* (read-from-string (dialog-item-text item))))))))
;;;;;
#|(make-new-text-item 12124480 1441892 "Minor 21st")
  #<new-text-item #xE53E496>|#
;;;;;

(defun MAKE-NEW-TEXT-ITEM (position size text)
  "(MAKE-new-TEXT-ITEM #@(40 40) #@(40 40) hello) 
     #<new-text-item #x17519FE>"
  (setq *text-item-text* text)
  (make-instance 
    'new-text-item
    :view-position position
    :view-size size))

;;;;;
#|(make-variables-window) 
 make-variables-window returned #<program-variables-window "SPEAC Variables" #xE7BC6DE>|#
;;;;;

(defun MAKE-VARIABLES-WINDOW ()
  (make-instance' PROGRAM-VARIABLES-WINDOW))

;;;;;
#|(make-analysis-window book-example)
#<my-analysis-window "SPEAC" #xE8D1DFE>|#
;;;;;

(defun MAKE-ANALYSIS-WINDOW (events)
  (setq *weights* (mapcar #'my-round (run-the-speac-weightings (get-passage events (* *beginning-beat* 1000)(* *ending-beat* 1000))
                                                               *begin-beat* (round (/ (first (my-last (sortcar #'< events))) 1000)) *meter*)))
   (create-chart *weights* 'dave))

(defClass NEW-TEXT-ITEM (view)
  ((text-item-text :initarg :text-item-text :initform *text-item-text* :accessor text-item-text)))

(defMethod INITIALIZE-INSTANCE ((pie NEW-TEXT-ITEM) &rest initargs)
  (apply #'call-next-method pie initargs))

(defMethod VIEW-DRAW-CONTENTS ((pie NEW-TEXT-ITEM))
  (set-pen-pattern pie *white-pattern*)
  (paint-rect pie 10 10 270 110)
  (move-to pie 14 20)
  (set-view-font pie "geneva")
  (set-view-font pie 9)
  (set-view-font pie :bold)
  (set-pen-pattern pie *black-pattern*)
  (frame-rect pie 10 10 270 110)
  (format pie "~A" (text-item-text pie)))

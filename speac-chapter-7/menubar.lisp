
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;     Sorcerer Function/Chapter 7     ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;        load code to run SPEAC       ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

;;;;;
#|(make-the-points
           '(((s1)) ((p2 e2 e2)) ((p3 e3) (p3 e3) (p3 e3))))
((((s1))) (((p2) (e2) (e2)))
 (((p3) (e3)) ((p3) (e3)) ((p3) (e3))))|#
;;;;;

(defun MAKE-THE-POINTS (levels)
  "Double recrusion necessary for listing each element for display."
  (cond ((null levels)())
        ((atom levels)(list levels))
        (t (cons (make-the-points (first levels))
                 (make-the-points (rest  levels))))))

;;;;;
#|(unwrap '((((s1 (128 85))))
            (((p2 (64 170)) (e2 (128 170)) (e2 (192 170))))
            (((p3 (36 255)) . . . .
((s1 (128 85)) (p2 (64 170)) (e2 (128 170))
 (e2 (192 170)) . . .|#
;;;;;

(defun UNWRAP (lists)
  "Reduces is arg by two lists."
  (apply #'append (apply #'append lists)))

;;;;;
#|(group-them-all '(a b c) '((d e f)(g h i)(j k l)))
(((a d) (a e) (a f)) ((b g) (b h) (b i)) ((c j) (c k) (c l)))|#
;;;;;

(defun GROUP-THEM-ALL (list stuff)
  "Groups list with matching stuff by placement."
  (if (or (null list)(null stuff))()
      (cons (group-it (first list)(first stuff))
            (group-them-all (rest list)(rest stuff)))))

;;;;;
#|(group-it 'a '(b c d))
((a b) (a c) (a d))|#
;;;;;

(defun GROUP-IT (first rest)
  "Groups its first arg with all the rest."
  (if (null rest)()
      (cons (list first (first rest))
            (group-it first (rest rest)))))

;;;;;
#|(arrange-levels-for-lines
            '((((s1 (128 85))))
              (((p2 (64 170)) . . .
(((s1 (128 85)) (p2 (64 170)))((s1 (128 85)) . . .|#
;;;;;

(defun ARRANGE-LEVELS-FOR-LINES (levels)
  (append (group-it (first (very-first levels))(first (second levels)))
          (apply #'append (group-them-all (first (second levels))(third levels)))))

;;;;;
#|(draw-the-lines
           #<scrolling-form-scroller #x35C1749E>
           '((((s1 (128 85))))
             (((p2 (64 170)) . . .
nil|#
;;;;;

(defun DRAW-THE-LINES (object levels)
  "Used in the form window to draw the diagonal lines."
  (let ((new-levels (arrange-levels-for-lines levels)))
    (loop for level in new-levels
          do (move-to object (+ (first (second (first level))) 8)(- (second (second (first level))) 4))
          do (with-fore-color *white-color* 
                            (line-to object (+ 8 (first (second (second level))))(second (second (second level))))))))

;;;;;
#|(form-the-levels
            '(((s1)) ((p2 e2 e2)) ((p3 e3) (p3 e3) (p3 e3))
              ((s4 p4 e4 e4 p4  . . .
((((s1 (128 85))))(((p2 (64 170))  . . .|#
;;;;;

(defun FORM-THE-LEVELS (speac-tree)
  "Runs get-the-levels of the speac form module."
  (create-the-points 
       (make-the-points (firstn 3 speac-tree))
       (length (third (firstn 3 speac-tree)))))

;;;;;
#|(analyze-speac-in-music
           '((0 74 1000 1 64) . . . 
nil|#
;;;;;

(defun ANALYZE-SPEAC-IN-MUSIC (events)
  "Toplevel for analyzing the form of a work with window resulting."
  (progn (setq *levels* (form-the-levels (get-the-levels events)))
         (create-form-connections)
         (make-form-window)))

;;;;;
#|(create-form-connections)
((1 2)(1 5) . . .|#
;;;;;

(defun CREATE-FORM-CONNECTIONS ()
  "Creates the formal connections between the sections for visual projection."
  (let ((form (number-them (mapcar #'first (my-last *output*)))))   ;((a 1) (b 2) (c 3) (c 4) (c 5) (c 6) (b 7) (b 8))
    (setq *form-connections* (link-them form))))

;;;;;
#|(link-them '((a 1) (a 2) (b 3) (b 4) (a 5) (a 6)))
((1 2) (1 5) (1 6) (2 5) (2 6) (3 4) (5 6))|#
;;;;;

(defun LINK-THEM (list)
  "Links the sections to one another."
  (if (null (rest list))()
      (append (link (first list)(rest list))
              (link-them (rest list)))))

;;;;;
#|(link '(a 1) '((a 2) (b 3) (b 4) (a 5) (a 6))) 
   ((1 2) (1 5) (1 6))|#
;;;;;

(defun LINK (element list)
  "Links a particular section to its cousins"
  (cond ((null list)())
        ((equal (first element)(very-first list))
         (cons (list (second element)(second (first list)))
               (link element (rest list))))
        (t (link element (rest list)))))

;;;;;
#|(number-them '(a a b b a a)) 
  ((a 1) (a 2) (b 3) (b 4) (a 5) (a 6))|#
;;;;;

(defun NUMBER-THEM (letters &optional (numbers '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30)))
  "Returns the letters with numbers in a list."
  (if (null letters)()
      (cons (list (first letters)(first numbers))
            (number-them (rest letters)(rest numbers)))))
  
;;;here's the menu for SPEAC

(defVar MY-MENU *default-menubar*)
(defVar *CHOICES* ())
(defvar *SORCERER-MENUBAR* ())

(setq *SORCERER-MENUBAR* (list (first my-menu)
                               (second my-menu)
                               (third my-menu)
                               (fourth my-menu)
                               (fifth my-menu)
                               (make-instance 'menu :menu-title "Load/Select"
                                              :menu-items 
                                              (list 
                                               (make-instance 
                                                 'menu-item :menu-item-title "Load"
                                                 :command-key #\1
                                                 :menu-item-action 
                                                 #'(lambda nil 
                                                     (let ((data (eval (setq *file-name* (get-filename (let ((pathname (choose-file-dialog)))
                                                                                                         (progn (load pathname)
                                                                                                                pathname)))))))
                                                       (if data (progn
                                                                  (pushnew *file-name* *choices*)
                                                                  (setq *score-name* *file-name*)
                                                                  (setq *select* *file-name*)
                                                                  (set *file-name* data)
                                                                  (setq *the-data* data))))))
                                               (make-instance 
                                                 'menu-item :menu-item-title "Select"
                                                 :command-key #\2
                                                 :menu-item-action 
                                                 #'(lambda nil (setq *target-item* (select-item-from-list *choices* :window-title "Select Target Work"))))))
                               (make-instance 'menu :menu-title "Variables"
                                              :menu-items 
                                              (list 
                                               (make-instance 
                                                 'menu-item :menu-item-title "Interval Strengths"
                                                 :command-key #\3
                                                 :menu-item-action 
                                                 #'(lambda nil 
                                                     (make-instance 'PROGRAM-VARIABLES-WINDOW)))
                                               (make-instance 
                                                 'menu-item :menu-item-title "Form Variables"
                                                 :command-key #\4
                                                 :menu-item-action 
                                                 #'(lambda nil 
                                                     (make-instance 'form-VARIABLES-WINDOW)))
                                               (make-instance 
                                                 'menu-item :menu-item-title "Window Variables"
                                                 :command-key #\5
                                                 :menu-item-action 
                                                 #'(lambda nil 
                                                     (make-instance 'window-VARIABLES-WINDOW)))))
                               (make-instance 'menu :menu-title "Analyze"
                                              :menu-items 
                                              (list 
                                               (make-instance 
                                                 'menu-item :menu-item-title "Surface SPEAC"
                                                 :command-key #\6
                                                 :menu-item-action 
                                                 #'(lambda nil 
                                                     (progn (if *window* (window-close *window*))
                                                            (unless (null *target-item*)
                                                              (make-instance 'range-WINDOW)))))
                                               (make-instance 
                                                 'menu-item :menu-item-title "Form Tree"
                                                 :command-key #\7
                                                 :menu-item-action 
                                                 #'(lambda nil 
                                                     (progn (if *form-window* (window-close *form-window*))
                                                            (unless (null *target-item*)(analyze-speac-in-music (eval (first *target-item*)))))))
                                               (make-instance 
                                                 'menu-item :menu-item-title "Form Detail"
                                                 :command-key #\8
                                                 :menu-item-action 
                                                 #'(lambda nil 
                                                     (progn (if *output-window* (window-close *output-window*))
                                                            (unless (null *output*) (setq *output-window* (make-instance 'output-WINDOW))))))))
                               (sixth my-menu)))

(set-menubar *sorcerer-menubar*)

(defClass FORM-VARIABLES-WINDOW (window)
  nil
  (:default-initargs 
    :window-type :document
    :window-title "Form Variables"
    :view-position #@(80 100)
    :view-size #@(200 160)))

(defMethod INITIALIZE-INSTANCE ((window FORM-VARIABLES-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs) 
  (add-subviews window (make-instance 'back    :view-position #@(0 0)
                                      :view-size #@(613 417))
                (make-new-text-item #@(0 5) #@(100 22) "Meter")
                ;(make-new-text-item #@(160 23) #@(44 24) ".8")
                (make-instance 'editable-text-dialog-item :dialog-item-text 
                               (write-to-string *meter*)
                               :view-size #@(28 10) 
                               :view-position #@(112 17)
                               :view-font '("geneva" 9)
                               :dialog-item-action #'(lambda (item) 
                                                       (ignore-errors (setq *meter* (read-from-string (dialog-item-text item))))))
                
                (make-new-text-item #@(0 25) #@(100 22) "Begin Beat")
                ;(make-new-text-item #@(160 23) #@(44 24) ".8")
                (make-instance 'editable-text-dialog-item :dialog-item-text 
                               (write-to-string *begin-beat*)
                               :view-size #@(28 10) 
                               :view-position #@(112 37)
                               :view-font '("geneva" 9)
                               :dialog-item-action #'(lambda (item) 
                                                       (ignore-errors (setq *begin-beat* (read-from-string (dialog-item-text item))))))
                
                (make-new-text-item #@(0 45) #@(100 22) "Cadence Min.")
                ;(make-new-text-item #@(160 23) #@(44 24) ".8")
                (make-instance 'editable-text-dialog-item :dialog-item-text 
                               (write-to-string *cadence-minimum*)
                               :view-size #@(28 10) 
                               :view-position #@(112 57)
                               :view-font '("geneva" 9)
                               :dialog-item-action #'(lambda (item) 
                                                       (ignore-errors (setq *cadence-minimum* (read-from-string (dialog-item-text item))))))
                (make-new-text-item #@(0 65) #@(100 22) "Pattern Size")
                ;(make-new-text-item #@(160 43) #@(44 24) ".225")
                (make-instance 'editable-text-dialog-item :dialog-item-text 
                               (write-to-string *pattern-size*)
                               :view-size #@(28 10) 
                               :view-position #@(112 77)
                               :view-font '("geneva" 9)
                               :dialog-item-action #'(lambda (item) 
                                                       (ignore-errors (setq *pattern-size* (read-from-string (dialog-item-text item))))))
                (make-new-text-item #@(0 85) #@(100 22) "Threshold")
                ;(make-new-text-item #@(160 63) #@(44 24) ".2")
                (make-instance 'editable-text-dialog-item :dialog-item-text 
                               (write-to-string *threshold*)
                               :view-size #@(28 10) 
                               :view-position #@(112 97)
                               :view-font '("geneva" 9)                               
                               :dialog-item-action #'(lambda (item) 
                                                       (ignore-errors (setq *threshold* (read-from-string (dialog-item-text item))))))
                (make-new-text-item #@(0 105) #@(100 22) "Intervals Off")
                ;(make-new-text-item #@(160 83) #@(44 24) ".55")
                (make-instance 'editable-text-dialog-item :dialog-item-text 
                               (write-to-string *intervals-off*)
                               :view-size #@(28 10) 
                               :view-position #@(112 117)
                               :view-font '("geneva" 9)
                               :dialog-item-action #'(lambda (item) 
                                                       (ignore-errors (setq *intervals-off* (read-from-string (dialog-item-text item))))))
                (make-new-text-item #@(0 125) #@(100 22) "Amount Off")
                ;(make-new-text-item #@(160 103) #@(44 24) ".65")
                (make-instance 'editable-text-dialog-item :dialog-item-text 
                               (write-to-string *amount-off*)
                               :view-size #@(28 10) 
                               :view-position #@(112 137)
                               :view-font '("geneva" 9)
                               :dialog-item-action #'(lambda (item) 
                                                       (ignore-errors (setq *amount-off* (read-from-string (dialog-item-text item))))))))

(defClass WINDOW-VARIABLES-WINDOW (window)
  nil
  (:default-initargs 
    :window-type :document
    :window-title "Window Variables"
    :view-position #@(80 100)
    :view-size #@(200 50)))

(defMethod INITIALIZE-INSTANCE ((window WINDOW-VARIABLES-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs) 
  (add-subviews window (make-instance 'back    :view-position #@(0 0)
                   :view-size #@(613 417))
    (make-new-text-item #@(0 5) #@(100 22) "Image Size")
    (make-instance 'editable-text-dialog-item :dialog-item-text 
                   (write-to-string *size*)
                   :view-size #@(28 10) 
                   :view-position #@(112 17)
                   :view-font '("geneva" 9)
                   :dialog-item-action #'(lambda (item) 
                                           (ignore-errors (setq *size* (read-from-string (dialog-item-text item))))))))

(defClass *OUTPUT-SEQUENCE* (sequence-dialog-item)
  (sequence :initarg :sequence :initform *output*))

(defMethod INITIALIZE-INSTANCE ((sequence *OUTPUT-SEQUENCE*) &rest initargs)
  (apply #'call-next-method sequence initargs)
  (set-table-sequence sequence (my-last *output*)))

(defClass OUTPUT-WINDOW (window)
  nil
  (:default-initargs :window-type :document
    :window-title (write-to-string (first *target-item*))
    :view-position #@(10 40)
    :view-size #@(360 380)))

(defMethod INITIALIZE-INSTANCE ((window OUTPUT-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs)
  (add-subviews window 
                
                (make-dialog-item 
                 '*output-sequence*
                 #@(2 2) #@(655 380)
                 ""
                 #'(lambda (item)
                     (declare (ignore item)) 
                     :selection-type :disjoint
                     :visible-dimensions #@(1 12)
                     :view-font '("times" 12 :bold)))))


(defVar *BEGIN-SCROLL* (make-instance
                            'scroll-bar-dialog-item
                            :view-position #@(102 28)
                            :direction :horizontal
                            :length 120
                            :setting *beginning-beat*
                            :max 180
                            :min 1
                            :dialog-item-action
                            #'(lambda (item &aux (setting (format nil 
                                                                  "~a"
                                                                  (scroll-bar-setting 
                                                                   item))))
                                (set-dialog-item-text
                                 (find-named-sibling item 'begin-display)
                                 setting)
                                (setq *beginning-beat* (read-from-string setting))
                                (window-update-event-handler (view-window item)))))

(defVar *END-SCROLL* (make-instance
                            'scroll-bar-dialog-item
                            :view-position #@(102 68)
                            :direction :horizontal
                            :length 120
                            :setting *ending-beat*
                            :max 180
                            :min 1
                            :dialog-item-action
                            #'(lambda (item &aux (setting (format nil 
                                                                  "~a"
                                                                  (scroll-bar-setting 
                                                                   item))))
                                (set-dialog-item-text
                                 (find-named-sibling item 'end-display)
                                 setting)
                                (setq *ending-beat* (read-from-string setting))
                                (window-update-event-handler (view-window item)))))

(defClass RANGE-WINDOW (window)
  nil
  (:default-initargs :window-type :document
    :window-title ""
    :view-position #@(10 400)
    :view-size #@(300 180)
    :auto-position :centermainscreen))

(defMethod INITIALIZE-INSTANCE ((window RANGE-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs)
  (set-scroll-bar-setting *begin-scroll* *beginning-beat*)
  (set-scroll-bar-setting *end-scroll* *total-beats*)
  (add-subviews window
    (make-instance 'static-text-dialog-item
      :dialog-item-text "You must choose a phrase-length amt. of music."
      :view-position #@(12 5)
      :view-size #@(432 15)
      :view-font '("palatino" 12 :bold))
    (make-instance 'static-text-dialog-item
      :dialog-item-text (write-to-string *beginning-beat*)
      :view-position #@(244 30)
      :view-size #@(32 15)
      :view-nick-name 'begin-display
      :view-font '("palatino" 12 :bold)) 
    (make-instance 'static-text-dialog-item
      :dialog-item-text "Set Begin Beat:"
      :view-position #@(14 30)
      :view-size #@(102 15)
      :view-nick-name 'begin-display
      :view-font '("palatino" 12 :bold))
    (make-instance 'static-text-dialog-item
      :dialog-item-text (write-to-string *ending-beat*)
      :view-position #@(244 70)
      :view-size #@(32 15)
      :view-nick-name 'end-display
      :view-font '("palatino" 12 :bold)) 
    (make-instance 'static-text-dialog-item
      :dialog-item-text "Set End Beat:"
      :view-position #@(14 70)
      :view-size #@(82 15)
      :view-nick-name 'end-display
      :view-font '("palatino" 12 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(106 105) #@(104 38)
     "Okay"
     #'(lambda (item) 
         item 
         (if (> *total-beats* *beginning-beat*)(progn (window-close window)
                                                      (MAKE-ANALYSIS-WINDOW (eval (first *target-item*))))
             (message-dialog "Your end time must be higher than your begin time!")))
     :view-font '("geneva" 10 :bold))
    *begin-scroll*
    *end-scroll*))


;;;;;
#|(get-passage '((0 74 1000 1 64) (1000 76 1500 1 64) (2500 74 500 1 64)
                 (3000 72 750 1 64) . . .
((12000 71 1000 1 64) (13000 72 1500 1 64)(14500 71 500 1 64) (15000 69 750 1 64)  . . .|#
;;;;;

(defun GET-PASSAGE (events begin-time end-time)
  "Simple returns those events in the given range."
  (cond ((null events)())
        ((< (very-first events) begin-time)
         (get-passage (rest events) begin-time end-time))
        ((> (very-first events) end-time)
         (get-passage (rest events) begin-time end-time))
        (t (cons (first events)
                 (get-passage (rest events) begin-time end-time)))))
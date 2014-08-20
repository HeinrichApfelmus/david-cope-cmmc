


                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;     Improvise Function/Chapter 4    ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;     simple code to run Improvise    ;;;;;
                   ;;;;;               windows               ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

;========================
;global declarations
;========================

(defVar *TEMPORARY-NAME-LIST* () "Temporary storage for database names.")
(defVar *A-DATABASE-SEQUENCE* () "The database sequence for the improvise window.")
(defVar *SELECTIONS* () "The chosen selections.")

;========================
;retriving the filename from a pathname
;========================

;;;;;
#| Calling (get-filename #1P"Macintosh HD:mcl 5.0:MCL 5.0:CMMC programs:improvise-chapter-4:stuff:test dbs:cope") 
    get-filename returned 2 values :
      cope
      4|#
;;;;;

(defun GET-FILENAME (pathname)
  "For retriving the filename from a pathname."
  (read-from-string 
   (mac-namestring 
    (file-namestring pathname))))

;========================
;the database sequence
;========================

(defClass DATABASE-SEQUENCE (sequence-dialog-item)
  (sequence :initarg :sequence :initform *temporary-name-list*))

(defMethod INITIALIZE-INSTANCE ((sequence DATABASE-SEQUENCE) &rest initargs)
  (apply #'call-next-method sequence initargs)
  (set-table-sequence sequence *temporary-name-list*))

;========================
;the improvisation window
;========================

(defClass IMPROVISE-WINDOW (window)
  nil
  (:default-initargs :window-type :document
    :window-title "Improvise"
    :view-position #@(50 160)
    :view-size #@(436 230)))

(defMethod INITIALIZE-INSTANCE ((window IMPROVISE-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs)
  ;(set-scroll-bar-setting *order-scroll* *look-ahead*)
  (add-subviews 
    window 
    (make-dialog-item 
     'button-dialog-item
     #@(16 25) #@(104 25)
     "Load"
     #'(lambda (item) 
         item 
         (let ((test (y-or-n-dialog "Choose a type:" :yes-text "Lisp" :no-text "Midi" :cancel-text ())))
           (if test (progn
                      (setq *name* (get-filename (load (choose-file-dialog))))
                      (push *name* *temporary-name-list*)
                      (setq *input-name* *temporary-name-list*)
                      (set-table-sequence *a-database-sequence* *temporary-name-list*)
                      (loop for cell in (selected-cells *a-database-sequence*)
                            do (cell-deselect *a-database-sequence* cell)))
               (progn (setq it (sortcar #'< (TRANSLATE-MIDI-FILE)))
                      (push *file-name* *temporary-name-list*)
                      (set *file-name* it)
                      (set-table-sequence *a-database-sequence* *temporary-name-list*)
                      (loop for cell in (selected-cells *a-database-sequence*)
                            do (cell-deselect *a-database-sequence* cell))))))
     #| #'(lambda (item) 
         item 
         (setq *name* (get-filename (load (choose-file-dialog))))
         (push *name* *temporary-name-list*)
         (setq *input-name* *temporary-name-list*)
         (create-a-complete-database *input-name*)
         (set-table-sequence *a-database-sequence* *temporary-name-list*)
         (loop for cell in (selected-cells *a-database-sequence*)
               do (cell-deselect *a-database-sequence* cell)))|#
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 65) #@(104 28)
     "Play"
     #'(lambda (item) 
         item 
         (setq *process* (process-run-function "play" 'play-events (eval (first *selections*)))))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 105) #@(104 28)
     "Improvise"
     #'(lambda (item) 
         item 
         ;(interact)
         ;(unless (null *grouping-names*)
           (setq *process* (process-run-function "play" 'play-events (improvise *selections*))));)
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 145) #@(104 28)
     "Stop"
     #'(lambda (item) 
         item (if *process* (progn (process-kill *process*)(stop-the-music))))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 185) #@(104 28)
     "Close"
     #'(lambda (item) 
         item 
         (window-close window))
     :view-font '("geneva" 10 :bold))
    
    (make-dialog-item 
     'button-dialog-item
     #@(200 145) #@(104 28)
     "Clear"
     #'(lambda (item) 
         item 
         (setq *temporary-name-list* (remove-names *selections* *temporary-name-list*))
         (setq *selections* ())
         (remove-data)
         (set-table-sequence *a-database-sequence* *temporary-name-list*))
     :view-font '("geneva" 10 :bold))
    (setq *a-database-sequence* (make-dialog-item 
                                 'database-sequence
                                 #@(142 17) #@(245 290)
                                 ""
                                 #'(lambda (item) 
                                     (setq *selections* 
                                           (mapcar #'(lambda (x)(cell-contents item x))
                                                   (selected-cells item))))
                                 :selection-type :disjoint
                                 :visible-dimensions #@(1 10)
                                 :view-font '("times" 12 :bold)))))

;========================
;for clearing out databases
;========================

;;;;;
#| Calling (remove-names (cope) (cope)) 
   remove-names returned nil|#
;;;;;

(defun REMOVE-NAMES (remove-events events)
  "Allows the removal of individual names from the database namelist."
  (if (null remove-events) events
      (remove-names (rest remove-events)
                    (remove-name (first remove-events) events))))

;;;;;
#|Calling (remove-name cope (cope)) 
  remove-name returned nil|#
;;;;;

(defun REMOVE-NAME (event events)
  "Allows the removal of na individual name from the database namelist."
  (cond ((null events)())
        ((equal event (first events))
         (rest events))
        (t (cons (first events)(remove-name event (rest events))))))

;========================
;the about window
;========================

(defClass ABOUT-WINDOW (window)
  nil
  (:default-initargs :window-type :document
    :window-title ""
    :view-position #@(10 40)
    :view-size #@(300 180)
    :close-box-p t))

(defMethod INITIALIZE-INSTANCE ((window ABOUT-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs)
  (add-subviews window 
                (make-instance 'static-text-dialog-item
                  :dialog-item-text "David Cope"
                  :view-position #@(109 75)
                  :view-size #@(142 15)
                  :view-font '("times" 14 :bold))
                (make-instance 'static-text-dialog-item
                  :dialog-item-text "Improvise"
                  :view-position #@(80 28)
                  :view-size #@(142 45)
                  :view-font '("times" 32 :bold))
                (make-instance 'static-text-dialog-item
                  :dialog-item-text "A simple interactive improvisation program."
                  :view-position #@(20 98)
                  :view-size #@(342 15)
                  :view-font '("times" 14 :italic))
                (make-dialog-item 
                 'button-dialog-item
                 #@(96 125) #@(104 28)
                 "Done"
                 #'(lambda (item) 
                     item 
                     (window-close window))
                 :view-font '("geneva" 10 :bold))))


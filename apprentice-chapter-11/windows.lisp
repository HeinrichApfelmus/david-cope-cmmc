
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;        Apprentice/Chapter 11        ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;    simple code to run Apprentice    ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

;========================
;some global and general variable declarations
;========================

(defVar *TEMPORARY-NAME-LIST* () "Temporary storage for database names.")
(defVar *A-DATABASE-SEQUENCE* () "The database sequence for the Apprentice music window.")
(defVar *SELECTIONS* () "The chosen selections.")
(defVar *NAME* () "Name storage.")
(defVar *INPUT-NAME* () "Input name storage.")
(defVar *SAVE-GROUPINGS* () "Where groupings are stored for complete works.")

;;;;;
#| (remove-data)
     ()|#
;;;;;

(defun REMOVE-DATA ()
  "Cleans up databases for starting over."
  (setq *first-groupings* ())
  (mapcar #'makunbound *lexicons*)  
  (setq *lexicons* ())
  (mapcar #'makunbound *grouping-names*)
  (setq *grouping-names* ())
  (setq *groupings* ())
  (setq *save-groupings* ())
  (setq *database-names* ())
  (setq test ())
  (setq the-name ()))


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

(defVar *DIALOG-ITEM-TEXT* ())

(setq *dialog-item-text* (make-instance 'editable-text-dialog-item 
                           :dialog-item-text (if (stringp *name-list*) *name-list* 
                                                 (if (null *name-list*) "" (write-to-string *name-list*)))
                           :view-size #@(245 134) 
                           :view-position #@(142 225)
                           :allow-returns t
                           :view-font '("geneva" 10 :bold)
                           :view-nick-name 'music-objects-display
                           :dialog-item-action 
                           #'(lambda (item) 
                               item
                               (setq *name-list* (MAKE-STRING-INTO-LIST (dialog-item-text item))))))

(defClass MUSIC-WINDOW (window)
  nil
  (:default-initargs :window-type :document
    :window-title "Apprentice"
    :view-position #@(50 60)
    :close-box-p ()
    :view-size #@(436 420)))

(defMethod INITIALIZE-INSTANCE ((window MUSIC-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs)
  (setq *name-list* nil)
  (setq *close* ())
  (input-the-music-objects-into-the-editable-dialog-window *name-list*)
  (add-subviews 
    window 
    (make-instance 'static-text-dialog-item
      :dialog-item-text "Music"
      :view-position #@(209 2)
      :view-size #@(142 15)
      :view-font '("times" 14 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 25) #@(104 25)
     "Load"
     #'(lambda (item) 
         item 
         (let ((test (y-or-n-dialog "Choose a type:" :yes-text "Lisp" :no-text "Midi" :cancel-text ())))
           (if test (progn
                      (setq *name* (get-filename (load (choose-file-dialog))))
                      (CREATE-A-COMPLETE-DATABASE (list *name*))
                      (setq *temporary-name-list* (append *grouping-names* *temporary-name-list*))
                      
                      ;(push *name* *temporary-name-list*)
                      (setq *input-name* *temporary-name-list*)
                      (set-table-sequence *a-database-sequence* *temporary-name-list*)
                      (loop for cell in (selected-cells *a-database-sequence*)
                            do (cell-deselect *a-database-sequence* cell)))
               (progn (setq it (sortcar #'< (TRANSLATE-MIDI-FILE)))
                      ;(push *file-name* *temporary-name-list*)
                      (set *file-name* it)
                      (CREATE-A-COMPLETE-DATABASE (list *file-name*))
                      (setq *temporary-name-list* (append *grouping-names* *temporary-name-list*))
                      (set-table-sequence *a-database-sequence* *temporary-name-list*)
                      (loop for cell in (selected-cells *a-database-sequence*)
                            do (cell-deselect *a-database-sequence* cell))))))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 65) #@(104 28)
     "Play"
     #'(lambda (item) 
         item 
         (if *selections* (setq *process* (process-run-function "play" 'play-events (reset-timing (events (eval (first *selections*))))))))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 145) #@(104 28)
     "Enter"
     #'(lambda (item) 
         item 
         (setq *name-list* (append *name-list* (list (first *selections*))))
         (input-the-music-objects-into-the-editable-dialog-window *name-list*))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(160 145) #@(104 28)
     "Clear"
     #'(lambda (item) 
         item 
         ;(setq *temporary-name-list* (remove-names *selections* *temporary-name-list*))
         (setq *name-list* ())
         (input-the-music-objects-into-the-editable-dialog-window *name-list*)
         (remove-data)
         (setq *selections* ())
         (setq *temporary-name-list* ())
         (set-table-sequence *a-database-sequence* *name-list*))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 105) #@(104 28)
     "Stop"
     #'(lambda (item) 
         item (if *process* (progn (process-kill *process*)(stop-the-music))))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(160 375) #@(104 28)
     "Clear"
     #'(lambda (item) 
         item 
         (setq *name-list* ())
         (input-the-music-objects-into-the-editable-dialog-window *name-list*))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(309 375) #@(104 28)
     "Send"
     #'(lambda (item) 
         item (if *name-list*
                (return-from-modal-dialog window)
                (message-dialog "You have not entered anything into Apprentice!")))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 375) #@(104 28)
     "Close"
     #'(lambda (item) 
         item 
         ;(window-close window)
         (progn (setq *close* t)(return-from-modal-dialog window)))
     :view-font '("geneva" 10 :bold))
    (make-instance 'static-text-dialog-item
      :dialog-item-text "===================================================="
      :view-position #@(9 192)
      :view-size #@(642 15)
      :view-font '("times" 14 :bold))
    (make-instance 'static-text-dialog-item
      :dialog-item-text "Input"
      :view-position #@(209 205)
      :view-size #@(142 15)
      :view-font '("times" 14 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 234) #@(104 28)
     "Play"
     #'(lambda (item) 
         item 
         (setq *process* (process-run-function "play" 'play-events (apply #'append (make-timings (remove-nils (mapcar #'(lambda (x)(if (boundp x)(events (eval x)))) *name-list*)))))))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 274) #@(104 28)
     "Stop"
     #'(lambda (item) 
         item (if *process* (progn (process-kill *process*)(stop-the-music))))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 314) #@(104 28)
     "Full Work"
     #'(lambda (item) 
         item 
         (setq *process* (process-run-function "play" 'play-events (apply #'append (re-time (remove-nils (create-a-work *sentences*))))))
         (let ((test (y-or-n-dialog "Do you wish to save this as MIDI file?" :cancel-text ())))
           (cond ((equal test :cancel)())
                 ((null test)())
                 (t (progn (setq *events* (reveal-the-hidden-events (apply #'append (reverse (return-only-music-sentences *sentences*)))))
                           (SAVE-AS-MIDI-MENU-ITEM-ACTION))))))
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
                                 :view-font '("times" 12 :bold)))
    (setq *dialog-item-text* (make-instance 'editable-text-dialog-item 
                               :dialog-item-text (if (stringp *name-list*) *name-list* 
                                                     (if (null *name-list*) "" (write-to-string *name-list*)))
                               :view-size #@(245 134) 
                               :view-position #@(142 225)
                               :allow-returns t
                               :view-font '("geneva" 10 :bold)
                               :view-nick-name 'music-objects-display
                               :dialog-item-action 
                               #'(lambda (item) 
                                   item
                                   (setq *name-list* (MAKE-STRING-INTO-LIST (dialog-item-text item))))))))

;;;;;
#| Calling (make-timings (((4412 75 5 1 117 tie) (4412 66 5 1 77 tie)
                         (4412 70 5 1 86 tie))
                        ((4417 75 3 1 117 tie) (4417 66 3 1 77 tie) . . . .
 make-timings returned (((0 75 5 1 117 tie) (0 66 5 1 77 tie)
                         (0 70 5 1 86 tie))
                        ((5 75 3 1 117 tie) (5 66 3 1 77 tie) (5 70 3 1 86 tie) . . . .|#
;;;;;

(defun MAKE-TIMINGS (event-groupings &optional (time 0))
  "Resets an event group's timings to start at time."
  (if (null event-groupings) ()
      (cons (mapcar #'(lambda (x)(append (list time)(cdr x)))(first event-groupings))
            (make-timings (rest event-groupings)(+ time (find-longest-duration (first event-groupings)))))))

;;;;;
#|  Calling (find-longest-duration
            ((4417 75 3 1 117 tie) (4417 66 3 1 77 tie) (4417 70 3 1 86 tie)
             (4417 54 3 1 80 tie) (4417 44 3 1 77 tie))) 
  find-longest-duration returned 3|#
;;;;;

(defun FIND-LONGEST-DURATION (events)
"Returns the longest duration of the events."
  (first (my-sort #'> (mapcar #'third events))))

;;;;;
#| Calling (remove-nils (nil nil nil 1 nil)) 
 remove-nils returned (1)|#
;;;;;

(defun REMOVE-NILS (stuff)
  "Removes the nils from the stuff."
  (cond ((null stuff)())
        ((null (first stuff))
         (remove-nils (rest stuff)))
        (t (cons (first stuff)
                 (remove-nils (rest stuff))))))

;;;;the music objects for Apprentice are in *name-list*!!!!
;;;have to now find a way in enter text as well (important - 1 ? etc.)
;;;have to find a way to initialize window so when re-open still have the previous objects present!!!!

;;;;;
#| Calling (input-the-music-objects-into-the-editable-dialog-window
           (improv[7]-75-66-70-54-44-61)) 
 input-the-music-objects-into-the-editable-dialog-window returned "improv[7]-75-66-70-54-44-61"|#
;;;;;

(defun INPUT-THE-MUSIC-OBJECTS-INTO-THE-EDITABLE-DIALOG-WINDOW (objects)
  "Places the music objects into the editable-dialog-window."
  (if (null objects) (set-dialog-item-text *dialog-item-text* 
                        "")
  (set-dialog-item-text *dialog-item-text* 
                        (eval (return-function-ready-for-call objects)))))

;;;;;
#|Calling (return-function-ready-for-call (improv[7]-75-66-70-54-44-61)) 
  return-function-ready-for-call returned (format nil
                                                  "~a~&"
                                                  "improv[7]-75-66-70-54-44-61")|#
;;;;;

(defun RETURN-FUNCTION-READY-FOR-CALL (objects)
  "creates a function call to format ready for evaluation."
  (append '(format) '(())
          (list (create-appropriate-string-code objects))
          (mapcar #'write-to-string objects)))

;;;;;
#|Calling (create-appropriate-string-code (improv[7]-75-66-70-54-44-61)) 
   create-appropriate-string-code returned "~a~&"|#
;;;;;

(defun CREATE-APPROPRIATE-STRING-CODE (objects)
  "Creates the appropriate string code for use with format."
  (write-to-string (implode (apply #'append (loop for object in objects
                                                  do (write-to-string object)
                                                  collect '(~A~&))))))

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

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

(defVar *TRANSCRIPT-WINDOW* () "For the transcript window.")
(defVar *TRANSCRIPT* () "Where the transcript of a session is stored.")
(defVar *TEMPORARY-BUFFER* () "A simple temporary buffer to store for file writing.")
(defVar *NAME-OF-FILE* () "Where the name of the file to be written is stored.")
(defVar *SOUND* () "Now defunct but possibly useful in the future variable.")
(defVar *PLAY-WINDOW* () "The play-window stored here.")
(defVar *WEIGHT-WIND* () "The weight-window stored here.")

;;;;;
#| Calling (clear-memory) 
 clear-memory returned nil |#
;;;;;

(defun CLEAR-MEMORY ()
  "Clears the session from RAM."
  (loop for sentence in *sentences*
        do (makunbound sentence))
  (loop for word in *words*
      do (setf (associations (eval word)) ())
      do (setf (used-before? (eval word)) ()))
  (loop for word in *words*
        do (if (not (events (eval word)))(makunbound word)))
  (setq *question-incipient-lexicon* (make-instance 'incipient-lexicon))
  (setq *answer-incipient-lexicon* (make-instance 'incipient-lexicon))
  (setq *question-cadence-lexicon* (make-instance 'cadence-lexicon))
  (setq *answer-cadence-lexicon* (make-instance 'cadence-lexicon))  (setq *sentences* ())
  (setq *words* ())
  (setq *counter* 1)
  (setq *successor* ())
  (setq *keyword* ())
  (setq *no-sentences* ())
  (setq *output* ())
  (setq *storage* ())
  (setq *input* "")
  (setq *predecessor* ())
  (setq *successor* ())
  (setq *current-words* ())
  (setq *no* ())
  (setq *keywords* ())
  (setq *response* ())
  (setq *last-words* ())
  (setq *last-word* ())
  (setq *weight-list* ())
  (setq *yes-sentences* ())
  (setq *yes* ())
  (setq *all-words* ())
  (gc))

;;;;;
#| Calling (save-transcript) 
 save-transcript returned #1P"Macintosh HD:mcl 5.0:MCL 5.0:CMMC programs:Apprentice/Chapter 11:stuff:untitled"
#1P"Macintosh HD:mcl 5.0:MCL 5.0:CMMC programs:Apprentice/Chapter 11:stuff:untitled" |#
;;;;;

(defun SAVE-TRANSCRIPT ()
  "Saves the current session to a file for reloading at a later time."
  (setq *temporary-buffer* (make-buffer))
  (make-mark *temporary-buffer*)
  (loop for sentence in *sentences*
        do (buffer-insert *temporary-buffer* (show-sentence-slots sentence)))
  (loop for word in *words*
        do (buffer-insert *temporary-buffer* (show-word-slots word)))
  (buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *question-incipient-lexicon* (make-instance 'incipient-lexicon :incipients '" (write-to-string  (incipients *question-incipient-lexicon*)) "))"))
  (buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *answer-incipient-lexicon* (make-instance 'incipient-lexicon :incipients '" (write-to-string  (incipients *answer-incipient-lexicon*)) "))"))
  (buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *question-cadence-lexicon* (make-instance 'cadence-lexicon :cadences '" (write-to-string  (cadences *question-cadence-lexicon*)) "))"))
  (buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *answer-cadence-lexicon* (make-instance 'cadence-lexicon :cadences '" (write-to-string  (cadences *answer-cadence-lexicon*)) "))"))
  (buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *sentences* '" (write-to-string *sentences*) ")"))
  (buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *words* '" (write-to-string *words*) ")"))
  (buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *counter* '" (write-to-string *counter*) ")"))
  (buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *successor* '" (write-to-string *successor*) ")"))
  (buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *keyword* '" (write-to-string *keyword*) ")"))
  (buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *no-sentences* '" (write-to-string *no-sentences*) ")"))
  (buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *output* '" (write-to-string *output*) ")"))
  (buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *storage* '" (write-to-string *storage*) ")"))
  (buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *input* '" (write-to-string *input*) ")"))
  (buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *predecessor* '" (write-to-string *predecessor*) ")"))
  (buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *successor* '" (write-to-string *successor*) ")"))
  (buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *no* '" (write-to-string *no*) ")"))
  (buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *keywords* '" (write-to-string *keywords*) ")"))
  (buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *last-words* '" (write-to-string *last-words*) ")"))
(buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *weight-list* '" (write-to-string *weight-list*) ")"))
 
(buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *ALL-WORDS* '" (write-to-string *ALL-WORDS*) ")")) 
(buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *LAST-WORD* '" (write-to-string *LAST-WORD*) ")"))
(buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *YES-SENTENCES* '" (write-to-string *YES-SENTENCES*) ")"))
(buffer-insert *temporary-buffer* (format () "~A~A~A~%" "(setq *NEW-SENTENCES* '" (write-to-string *NEW-SENTENCES*) ")"))
(setq *name-of-file* (choose-new-file-dialog))
  (buffer-write-file 
   *temporary-buffer* 
   (make-pathname :directory (mac-directory-namestring *name-of-file*)
                  :name (mac-file-namestring *name-of-file*)
                  :type :unspecific)
   :if-exists :overwrite))

;;;;;
#|  Calling (show-sentence-slots #<sentence #x240F536>) 
 show-sentence-slots returned "(setq #<sentence #x240F536> (make-instance 'sentence 
    :name '(sentence-1) :sentence-type '(?) :sentence '((how are you?)) :length . . .|#
;;;;;

(defun SHOW-SENTENCE-SLOTS (object)
  "This function produces a readable and executable reinstatement of the argument's
      sentence status. The argument must be quoted."
  (format () "~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~%"
          "(setq " object
          " (make-instance 'sentence :name '"
          (write-to-string (name (eval object)))
          " :sentence-type '" (write-to-string (sentence-type (eval object)))
          " :sentence '" (write-to-string (sentence (eval object)))
          " :length-of-sentence '" (write-to-string (length-of-sentence (eval object)))
          " :parse-it '" (write-to-string (parse-it (eval object)))
          " :origination '" (write-to-string (origination (eval object)))
          "))"))

;;;;;
#|  Calling (show-word-slots #<word #x240F5AE>) 
 show-word-slots returned "(setq #<word #x240F5AE> (make-instance 'word :predecessors '(nil) 
    :successors '(are) :keywords '(you?) :word-type '(?) :positions-in- . . .|#
;;;;;

(defun SHOW-WORD-SLOTS (object)
  "This function produces a readable and executable reinstatement of the argument's
  object status. The argument must be quoted."
  (format () "~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~%"
          "(setq " object
          " (make-instance 'word :predecessors '" (write-to-string (predecessors (eval object)))
          " :successors '" (write-to-string (successors (eval object)))
          " :keywords '" (write-to-string (keywords (eval object)))
          " :word-type '" (write-to-string (word-type (eval object)))
          " :positions-in-sentence '" (write-to-string (positions-in-sentence (eval object)))
          " :usage '" (write-to-string (usage (eval object)))
          " :associations '" (write-to-string (associations (eval object)))
          " :events '" (write-to-string (events (eval object)))
          "))"))

;;;;;
#| Calling (show-transcript) 
 show-transcript returned #<transcript-window "Transcript" #x2479D16>
#<transcript-window "Transcript" #x2479D16> |#
;;;;;

(defun SHOW-TRANSCRIPT ()
  "Creates a transcript window for the menu which prints out the current conversation."
  (setq *transcript* (append '("Apprentice output preceded by >.""""")
                             (let ((sentences (reverse (loop for sentence in *sentences*
                                                             collect 
                                                             (if (equal (origination (eval sentence)) 'Apprentice)
                                                               (list (cons '> (first (sentence (eval sentence)))))
                                                               (sentence (eval sentence)))))))
                               (loop for sentence in sentences
                                     collect (make-list-into-string (first sentence))))))
  (setq *transcript-window* (make-instance 'transcript-window)))

(defClass *TRANSCRIPT-SEQUENCE* (sequence-dialog-item)
  (sequence :initarg :sequence :initform *transcript*))

(defMethod INITIALIZE-INSTANCE ((sequence *TRANSCRIPT-SEQUENCE*) &rest initargs)
  (apply #'call-next-method sequence initargs)
  (set-table-sequence sequence *transcript*))

(defClass TRANSCRIPT-WINDOW (window)
  nil
  (:default-initargs :window-type :document
    :window-title "Transcript"
    :view-position #@(80 40)
    :view-size #@(460 380)))

(defMethod INITIALIZE-INSTANCE ((window TRANSCRIPT-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs)
  (add-subviews window                 
    (make-dialog-item 
     '*transcript-sequence*
     #@(2 2) #@(455 360)
     ""
     #'(lambda (item)
         (declare (ignore item)) 
         :selection-type :disjoint
         :visible-dimensions #@(1 12)
         :view-font '("times" 12 :bold)))))

(set-menubar (list (first my-menu)
                   (second my-menu)
                   (third my-menu)
                   (fourth my-menu)
                   (fifth my-menu)
                   (make-instance 'menu :menu-title "Apprentice"
                                  :menu-items 
                                  (list 
                                   (make-instance 
                                     'menu-item :menu-item-title "Initiate Session"
                                     :command-key #\1
                                     :menu-item-action 
                                     #'(lambda nil 
                                         (progn (if *transcript-window* (window-close *transcript-window*))
                                         (clear-memory)
                                         (setq *close* ())
                                         (setq *initiate* t)
                                              (apprentice))))
                                    (make-instance 
                                     'menu-item :menu-item-title "Continue Session"
                                     :command-key #\2
                                     :menu-item-action 
                                     #'(lambda nil 
                                         (setq *initiate* nil)
                                         (if *transcript-window* (window-close *transcript-window*))
                                         ;(if *sound* (sound-apprentice-associate)
                                             (apprentice))) ;)
                                   (make-instance 
                                     'menu-item :menu-item-title "Transcript of Current Session"
                                     :command-key #\3
                                     :menu-item-action 
                                     #'(lambda nil 
                                         (show-transcript)))
                                   (make-instance 
                                     'menu-item :menu-item-title "Clear Session"
                                     :command-key #\4
                                     :menu-item-action 
                                     #'(lambda nil 
                                         (if *transcript-window* (window-close *transcript-window*))
                                         (progn (clear-memory)
                                                (if *weight-wind*
                                                  (set-table-sequence *dialog-text* nil)))))
                                   (make-instance 
                                     'menu-item :menu-item-title "Save Session"
                                     :command-key #\5
                                     :menu-item-action 
                                     #'(lambda nil 
                                         (save-transcript)))
                                   (make-instance 
                                     'menu-item :menu-item-title "Load Session"
                                     :command-key #\6
                                     :menu-item-action 
                                     #'(lambda nil 
                                         (with-cursor *watch-cursor* (load (choose-file-dialog)))))
                                   (make-instance 
                                     'menu-item :menu-item-title "Help"
                                     :command-key #\7
                                     :menu-item-action 
                                     #'(lambda nil 
                                         (make-instance 'help-manual-window)))
                                   (make-instance 
                                     'menu-item :menu-item-title "Weightings"
                                     :command-key #\8
                                     :menu-item-action 
                                     #'(lambda nil 
                                         (if *weight-wind* (window-close *weight-wind*))
                                         (setq *weight-wind* 
                                               (make-instance 'scrolling-window2
                                                 :scroller-class 'scrolling-window-scroller2
                                                 :window-type :document-with-zoom
                                                 :window-title "Weightings"
                                                 :view-size #@(630 400)
                                                 :view-position #@(60 50)
                                                 :track-thumb-p t))))
                                   (make-instance 
                                     'menu-item :menu-item-title "Associations"
                                     :command-key #\9
                                     :menu-item-action 
                                     #'(lambda nil 
                                         (if *graph-window* (window-close *graph-window*))
                                         (setq *new-sentences* (loop for sentence in (reverse *sentences*)
                                                                     collect (first (sentence (eval sentence)))))
                                         (let ((test (catch-cancel (y-or-n-dialog "
Do you wish to have a
reverse screen (i.e.,
white on black)?." :cancel-text nil))))
                                           (cond ((equal test t)(setq *reverse-mode* t))
                                                 (t (setq *reverse-mode* ())))
                                           (if *reverse-mode* 
                                             (set-back-color (setq *graph-window* (make-instance 'scrolling-window
                                                                                    :scroller-class 'scrolling-window-scroller1
                                                                                    :window-type :document
                                                                                    :view-size (make-point (- *screen-width* 26) (- *screen-height* 46))
                                                                                    :window-title "Associations" 
                                                                                    :track-thumb-p t))
                                                             *black-color*)
                                             (window-ensure-on-screen (setq *graph-window* (make-instance 'scrolling-window
                                                                                             :scroller-class 'scrolling-window-scroller1
                                                                                             :window-type :document
                                                                                             :view-size (make-point (- *screen-width* 26) (- *screen-height* 46))
                                                                                             :window-title "Associations" 
                                                                                             :track-thumb-p t))
                                                                      *window-default-position* (make-point (- *screen-width* 10)(- *screen-height* 50))))))
                                     )))
                   (sixth my-menu)))

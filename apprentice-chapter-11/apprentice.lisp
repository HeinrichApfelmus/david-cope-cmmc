
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;        Apprentice/Chapter 11        ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;    simple code to run Apprentice    ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

#|To load Apprentice, place the Apprentice folder alongside your MCL 5.0 application and then load 
the init.lisp file from within MCL.  Alternatively, you can pull the init.lisp from the Apprintice folder
and then simply start MCL 5.0 and it should load automatically (as long as you've placed the Apprentice folder 
alongside your MCL 5.0 application.
This form of Apprentice is capable of accepting music and or text input and a mixture of the two with 
weightings impacted by a combination of both. I have not implemented allusions into this model to avoid 
confusing first-time users with output that would not match expectations. Please contact me at 
howell@ucsc.edu if you would like to experiment with a more fully implemented version of Apprentice and
if enough readers of CMMC express an interest, I will place such a program on this site.|#

;========================
;some global and general variable declarations
;========================

(require 'quickdraw)
(require 'scrolling-windows)
(defVar *NAME-LIST* () "Primary location for names of word storage.")
(defVar *REVERSE-MODE* () "For reversing the color on the graphic window.")
(defVar TEST () "For testing.")
(defVar *RHYTHM* () "Probably defunct method for adding rhythm of pitch entry.")
(defVar *CURRENT-WORDS* () "The current words under analysis.")
(defVar *INPUT-WORK* () "An input work.")
(defVar *PLAY* () "Switch to determine whether to play or not (words versus music).")
(defVar NOTE-VALUES () "Probably defunct method for adding rhythm of pitch entry.")
(defVar *PREDECESSOR-WEIGHT* .5 "Default value.")
(defVar *KEYWORD-WEIGHT* .75 "Default value.")
(defVar *SUCCESSOR-WEIGHT* .5 "Default value.")
(defVar *KEYWORDS* () "Keyword storage.")
(defVar *LAST-WORDS* () "Last word storage.")
(defVar *BROAD-KEYWORD-WEIGHT* .1 "Default value.")
(defVar *BACKWARD-CHAIN-WEIGHT* .1 "Default value.")
(defVar MY-MENU *default-menubar* "Saving the default menubar for testing.")
(defVar MENU-ITEM-1 t "Menu item setting.")
(defVar MENU-ITEM-2 ())
(defVar *CREATOR* () "Menu item setting.")
(defVar *EMAIL* () "About window setting.")
(defVar *VISUAL* () "About window setting.")
(defVar *SOFTWARE* () "About window setting.")
(defVar *TITLE* () "About window setting.")
(defVar *COPYRIGHT* () "About window setting.")
(defVar *WEIGHT-LIST* () "Word weights stored here.")
(defVar *ALL-WORDS* () "Storage for all currently input words.")
(defVar *LAST-WORD-WEIGHT* 0.2 "Default value.")
(defVar *KEYWORD-WEIGHT* 0.15 "Default value.")
(defVar *LAST-WORD* () "Last word storage.")
(defVar *WEIGHT-DIVISOR* 2 "Default value.")
(defVar *YES-SENTENCES* () "Yes sentence storage.")
(defVar *YES* () "Word for yes")
(defVar *GRAPH-WINDOW* () "Graph window variable.")
(defVar *NEW-SENTENCES* () "New sentence storage.")
(defVar *INITIATE* t "Begin.")
(defVar *WORD-COLOR* *blue-color* "Word color.")
(defVar *LINE-COLOR* *pink-color* "Line color.")
(defVar *DIALOG-TEXT* (make-instance 
                           'sequence-dialog-item
                           :view-font '("monaco" 10)
                           :view-position #@(0 0)
                           :view-size #@(1390 1400)
                           :table-sequence (reverse *weight-list*)))
(defVar *CLOSE* () "Telling the system the Apprentice window is now closed.")
(defVar TEST-WORD () "Testing variable.")
(defVar TESTER-WORD () "Testing variable.")

;;;;;
#|Calling (compound-associations ((name? 0.75) (name? 0.2) (is 0.5))) 
    compound-associations returned ((name? 0.95) (is 0.5))|#
;;;;;

(defun COMPOUND-ASSOCIATIONS (associations)
  "Aggregates all of the same word weightings into single entries."
  (cond ((null associations)())
        ((assoc (very-first associations)(rest associations))
         (let ((compound-object (list (very-first associations)
                                      (round-it (+ (second (first associations))
                                                   (second (assoc (very-first associations)(rest associations))))))))
           (compound-associations 
            (cons compound-object (remove-object-twice (very-first associations) associations)))))
        (t (cons (first associations)
                 (compound-associations (rest associations))))))

;;;;;
#|Calling (remove-object-twice name? ((name? 0.75) (name? 0.2) (is 0.5))) 
     remove-object-twice returned ((is 0.5))|#
;;;;;

(defun REMOVE-OBJECT-TWICE (object associations &optional (times 2))
  "Removes the object and its target twice."
  (cond ((zerop times) associations)
        ((null associations)())
        ((equal object (very-first associations))
         (remove-object-twice object (cdr associations) (1- times)))
        (t (cons (first associations)
                 (remove-object-twice object (cdr associations) times)))))

;;;;;
#|Calling (compare-words a a) 
       compare-words returned t |#
;;;;;

(defun COMPARE-WORDS (first-word second-word)
  "Compares the first word with the second for similarities."
  (let ((test-1 (explode first-word))
        (test-2 (explode second-word)))
    (if (or (equal test-1 (firstn (length test-1)(explode second-word)))
            (equal test-2 (firstn (length test-1)(explode first-word)))) t)))

;;;;;
#|Calling (put-sentence-into-database (what is your name?)) 
  put-sentence-into-database returned "nil"  |#
;;;;;

(defun PUT-SENTENCE-INTO-DATABASE (sentence)
  "Puts the sentence into the database."
  (establish-keywords sentence)
  (let ((sentence-type (get-sentence-type sentence))
        (name (make-new-name)))
    (make-sentence-object sentence sentence-type name)
    (make-word-objects sentence sentence-type name)
    (parse-sentence sentence name)
    (define-incipients sentence sentence-type)
    (define-cadences sentence sentence-type)
    (new-text)
    (setq *response* (reply sentence-type sentence))
    (display *response*)))

;;;;;
#|Calling (reply ? (what is your name?)) 
   reply returned nil
|#
;;;;;

(defun REPLY (type sentence)
  "This function creates sentences by using the various associations in each
       word in the sentence argument."
  (cond ((recognize-no sentence)
         (progn (reduce-weighting (first (sentence (eval (third *sentences*))))
                                  (first (sentence (eval (second *sentences*)))))
                (list '*)))
        ((recognize-yes sentence)
         (progn (add-weighting (first (sentence (eval (third *sentences*))))
                               (first (sentence (eval (second *sentences*)))))
                (list '^)))
        
        ((events (eval (first sentence)))
         (let ((choices (compound-associations                              ;;;this is a pro-rated list of all of the associations of the sentence argument
                         (apply #'append 
                                (loop for word in sentence
                                      collect (get-music-associations (associations (eval word)))))))
               (incipients (if (equal type '?)     ;;;this is a just in case choices is nil listing of alternatives sentence incipients
                             (my-remove (list (eval *no*)) 
                                        (get-music-words (incipients *answer-incipient-lexicon*)))
                             (get-music-words (incipients *question-incipient-lexicon*)))))
           (setq *current-words* ())
           (if (or (null choices)(null incipients))
             ()
             (let ((current-word                                            ;;;here's where we get a current word - the highest rated word in choices
                    (let ((trial (choose-the-highest-rated-word 
                                  (remove-them 
                                   (get-music-words (cadences (if (equal type '?) 
                                                                *question-cadence-lexicon* 
                                                                *answer-cadence-lexicon*)) )
                                   choices))))
                      (if trial trial (get-music-words (choose-one incipients)))))            ;;;here is where we resort to incipients if necessary
                   (cadences (get-music-words (cadences (eval (other-lexicon-type type))))))  ;;;this variable will indicate when we must stop!
               (let ((new-sentence                                          ;;;here is where the new sentences is stored
                      (cons current-word                                    ;;;current word changes until the "or"
                            (loop until 
                              (or (null current-word)(member current-word cadences))
                                   do (setq test-word current-word)
                                   do (setq tester-word (member current-word cadences))
                                   do (pushnew current-word *current-words*) ;;;these must be subtracted from options to avoid repeats
                                   collect (setf current-word 
                                                 (let ((test 
                                                        (choose-the-highest-rated-word
                                                         (remove-them 
                                                          (append *current-words* 
                                                                  (get-music-words (cadences 
                                                                                    (if (equal type '?) *question-cadence-lexicon* *answer-cadence-lexicon*))))
                                                          (get-music-associations (associations (eval current-word)))))))
                                                   (if test test 
                                                       (choose-the-one 
                                                        (get-music-words (loop for association in (associations (eval current-word))
                                                                               collect (first association)))))))))))
                 new-sentence)))))
        (t (let ((choices (compound-associations                              ;;;this is a pro-rated list of all of the associations of the sentence argument
                           (apply #'append 
                                  (loop for word in sentence
                                        collect (get-word-associations (associations (eval word)))))))
                 (incipients (if (equal type '?)     ;;;this is a just in case choices is nil listing of alternatives sentence incipients
                               (my-remove (list (eval *no*)) 
                                          (get-word-words (incipients *answer-incipient-lexicon*)))
                               (get-word-words (incipients *question-incipient-lexicon*)))))
             (setq *current-words* ())
             (if (or (null choices)(null incipients))
               ()
               (let ((current-word                                            ;;;here's where we get a current word - the highest rated word in choices
                      (let ((trial (choose-the-highest-rated-word 
                                    (remove-them 
                                     (get-word-words (cadences (if (equal type '?) 
                                                                 *question-cadence-lexicon* 
                                                                 *answer-cadence-lexicon*)))
                                     choices))))
                        (if trial trial (choose-one (get-word-words incipients)))))           ;;;here is where we resort to incipients if necessary
                     (cadences (cadences (eval (other-lexicon-type type)))))  ;;;this variable will indicate when we must stop!
                 (let ((new-sentence                                          ;;;here is where the new sentences is stored
                        (cons current-word                                    ;;;current word changes until the "or"
                              (loop until 
                                    (or (null current-word)(member current-word cadences))
                                    do (pushnew current-word *current-words*) ;;;these must be subtracted from options to avoid repeats
                                    collect (setf current-word 
                                                  (let ((test 
                                                         (choose-the-highest-rated-word
                                                          (remove-them 
                                                           (append *current-words* 
                                                                   (get-word-words (cadences 
                                                                                    (if (equal type '?) *question-cadence-lexicon* *answer-cadence-lexicon*))))
                                                           (get-word-associations (associations (eval current-word)))))))
                                                    (if test test 
                                                        (choose-the-one 
                                                         (get-word-words (loop for association in (associations (eval current-word))
                                                                               collect (first association)))))))))))
                   new-sentence)))))))

;;;;;
#| Calling (choose-the-one (1 2 3)) 
 choose-the-one returned 2 |#
;;;;;

(defun CHOOSE-THE-ONE (stuff)
  "Simply chooses one object pseudo-randomly from its arg."
  (choose-one stuff))

;;;;;
#| Calling (reduce-weighting (what is your name?) (computer!)) 
    reduce-weighting returned ((what (computer! 0.81) (name? 5.04) (is 2.7)
                                (david! 1.62) (name 0.9) (my 0.5) (your 1.3))
                               (is (computer! 1.54) (name? 4.86) (your 2.4) . . .|#
;;;;;

(defun REDUCE-WEIGHTING (sentence-1 sentence-2)
  "sentence 1 here is the initiating sentence."
  (loop for word in sentence-1
        collect (cons word (reduce-weight word sentence-2))))

;;;;;
#| Calling (reduce-weight what (computer!)) 
     reduce-weight returned ((computer! 0.81) (name? 5.04) (is 2.7)
                             (david! 1.62) (name 0.9) (my 0.5) (your 1.3))|#
;;;;;

(defun REDUCE-WEIGHT (word sentence)
  "Reduces the weight of each entry  in word for all of the words in sentence."
  (setf (associations (eval word))
        (punish (associations (eval word)) sentence)))

;;;;;
#|   Calling (make-weight-list name? 0.75) 
    make-weight-list returned ((name? 0.75)) |#
;;;;;

(defun MAKE-WEIGHT-LIST (name weight)
  "A simple cover for double listing."
  (list (list name weight)))

;;;;;
#| Calling (add-word-to-word-weightlists what) 
    add-word-to-word-weightlists returned nil |#
;;;;;

(defun ADD-WORD-TO-WORD-WEIGHTLISTS (word)
  "Adds new words backchain style to all previous words in the database."
  (loop for item in *all-words*
        unless (equal item word)
        do (setf (associations (eval item))
                 (compound-associations 
                  (append (associations (eval item))
                          (list 
                           (cond ((equal word *keyword*)
                                  (list word (round-it (/ *KEYWORD-WEIGHT* 2))))
                                 ((equal word *last-word*)
                                  (list word (round-it (/ *LAST-WORD-WEIGHT* 2))))
                                 (t (list word *BACKWARD-CHAIN-WEIGHT*)))))))))

;;;;;
#| Calling (round-it 0.95) 
     round-it returned 0.95 |#
;;;;;

(defun ROUND-IT (n)
  "Simple utility to limit decimal places."
  (float (/ (round (* n 100)) 100)))

;;;;;
#|  Calling (establish-keywords (what is your name?)) 
 establish-keywords returned (name?)|#
;;;;;

(defun ESTABLISH-KEYWORDS (sentence)
  "Establishes all of the principal keywords."
  (let ((test (recognize-no sentence))
        (yes-test (RECOGNIZE-yes sentence)))
    (setq *rs* (make-random-state t))
    (setq *predecessor* ())
    (setq *successor* (second sentence))
    (setq *last-word* (my-last sentence))
    (unless (or yes-test test)
      (setq *keyword* (get-keyword sentence)))
    (unless (or yes-test test)
      (pushnew *keyword* *keywords*))
    (unless (or yes-test test)
      (pushnew (my-last sentence) *last-words*))))

;;;;;
#| Calling (get-sentence-type (what is your name?)) 
   get-sentence-type returned ? |#
;;;;;

(defun GET-SENTENCE-TYPE (sentence)
  "Returns the sentence type of question or statement."
  (my-last (explode (my-last sentence))))

;;;;;
#| Calling (make-new-name) 
   make-new-name returned sentence-1 |#
;;;;;

(defun MAKE-NEW-NAME ()
  "Returns a new sentence name."
  (implode (list 'sentence '- *counter*)))

;;;;;
#|  Calling (make-sentence-object (what is your name?) ? sentence-1) 
   make-sentence-object returned 2 |#
;;;;;

(defun MAKE-SENTENCE-OBJECT (sentence sentence-type name)
  "associations in this version are of four types:
  1) keyword found in *keyword*, weight being *KEYWORD-WEIGHT*
  2) last words found in *last-word*, weight being *LAST-WORD-WEIGHT*
  3) next words found in *successor*, *SUCCESSOR-WEIGHT*
  4) all remaining words found in *all-words*, weight being *BACKWARD-CHAIN-WEIGHT*
   the only exception being the word for no - this will not be in the vocabulary"
  (set name
       (make-instance 'sentence 
         :name (list name)
         :sentence-type (list sentence-type)
         :sentence (list sentence)
         :length-of-sentence (list (length sentence))
         :parse-it ()
         :origination 'user))
  (pushnew name *sentences*)
  (incf *counter*))

;;;;;
#|   Calling (make-word-objects (what is your name?) ? sentence-1) 
   make-word-objects returned nil |#
;;;;;

(defun MAKE-WORD-OBJECTS (sentence sentence-type name)
  "Makes the words objects for Colaborator."
  (loop for word in sentence
        do (cond ((and (not (member word *words*))(not (boundp word))) ;;;in other words not previously used or a music object
                  (progn
                    (set word 
                         (make-instance 'word 
                           :name (list name)
                           :sentence-type (list sentence-type)
                           :sentence (list sentence)
                           :length-of-sentence (list (length sentence))
                           :predecessors (list *predecessor*)
                           :successors (list *successor*)
                           :keywords (list *keyword*)
                           :word-type (list sentence-type)
                           :positions-in-sentence (list (1+ (position word sentence)))
                           :associations 
                           (compound-associations 
                            (append (if (and *keyword* (not (equal word *keyword*)))
                                      (make-weight-list *keyword* *KEYWORD-WEIGHT*))
                                    (if (and *last-word* (not (equal word *last-word*))) 
                                      (make-weight-list *last-word* *LAST-WORD-WEIGHT*))
                                    (if (and *successor* (not (equal word *successor*))) 
                                      (make-weight-list *successor* *SUCCESSOR-WEIGHT*))
                                    (loop for item in (my-remove (list word) *all-words*)
                                          collect (list item *BACKWARD-CHAIN-WEIGHT*))))
                           :usage 1
                           :used-before? t))
                    (if (not (equal sentence-type '*))
                      (push word *all-words*))
                    (setq *input-work* (rest *input-work*))))
                 ((and (boundp word)(not (used-before? (eval word)))) ;music word but not actually used yet!!!
                  (progn 
                    (setf (name (eval word))(cons name (name (eval word))))
                    (setf (sentence-type (eval word)) (list sentence-type))
                    (setf (sentence (eval word)) (list sentence))
                    (setf (length-of-sentence (eval word))
                          (list (length sentence)))
                    (setf (predecessors (eval word)) (list *predecessor*))
                    (setf (successors (eval word)) (list *successor*))
                    (setf (keywords (eval word)) (list *keyword*))
                    (setf (positions-in-sentence (eval word)) (list (1+ (position word sentence))))
                    (setf (word-type (eval word)) (list sentence-type))
                    (setf (associations (eval word))
                          (compound-associations 
                           (append (if (and *keyword* (not (equal word *keyword*)))
                                     (make-weight-list *keyword* *KEYWORD-WEIGHT*))
                                   (if (and *last-word* (not (equal word *last-word*))) 
                                     (make-weight-list *last-word* *LAST-WORD-WEIGHT*))
                                   (if (and *successor* (not (equal word *successor*))) 
                                     (make-weight-list *successor* *SUCCESSOR-WEIGHT*))
                                   (loop for item in (my-remove (list word) *all-words*) ;;;??????
                                         collect (list item *BACKWARD-CHAIN-WEIGHT*)))))
                    (setf (usage (eval word)) 1
                          )
                    (setf (used-before? (eval word)) t)
                    (if (not (equal sentence-type '*))
                      (push word *all-words*))
                    (setq *input-work* (rest *input-work*))))
                 (t (progn
                      (setf (name (eval word))(cons name (name (eval word))))
                      (setf (sentence-type (eval word))(cons sentence-type (sentence-type (eval word))))
                      (setf (sentence (eval word))(cons sentence (sentence (eval word))))
                      (setf (length-of-sentence (eval word))
                            (cons (length sentence) (length-of-sentence (eval word))))
                      (setf (predecessors (eval word))(cons *predecessor* (predecessors (eval word))))
                      (setf (successors (eval word))(cons *successor* (successors (eval word))))
                      (setf (keywords (eval word))(cons *keyword* (keywords (eval word))))
                      (setf (positions-in-sentence (eval word))(cons (1+ (position word sentence)) (positions-in-sentence (eval word))))
                      (setf (word-type (eval word))(cons sentence-type (word-type (eval word))))
                        (setf (associations (eval word))
                            (compound-associations 
                             (append (if (and *keyword* (not (equal word *keyword*)))
                                       (make-weight-list *keyword* *KEYWORD-WEIGHT*))
                                     (if (and *last-word* (not (equal word *last-word*))) 
                                       (make-weight-list *last-word* *LAST-WORD-WEIGHT*))
                                     (if (and *successor* (not (equal word *successor*))) 
                                       (make-weight-list *successor* *SUCCESSOR-WEIGHT*))
                                     (loop for item in (my-remove (list word) *all-words*) ;;;??????
                                           collect (list item *BACKWARD-CHAIN-WEIGHT*))
                                     (associations (eval word)))))
                      (setf (usage (eval word))
                            (1+ (usage (eval word))))
                      (setf (used-before? (eval word)) t))))
        do (setf *predecessor* word)
        do (setf *successor* (nth (+ (position word sentence) 2) sentence))
        do (pushnew word *words*)
        if (not (equal sentence-type '*))
        do (loop for item in sentence
                 do (add-word-to-word-weightlists item))))

;;;;;
#| Calling (parse-sentence (what is your name?) sentence-2) 
     parse-sentence returned (a a a a)|#
;;;;;

(defun PARSE-SENTENCE (sentence name)
  "Parses the sentence fully."
  (setf (parse-it (eval name))
        (loop for word in sentence
              collect (figure-speac word))))

;;;;;
#|Calling (define-incipients (my name is david!) !) 
    define-incipients returned (my) |#
;;;;;

(defun DEFINE-INCIPIENTS (sentence sentence-type)
  "Defines the incipients for the sentence."
  (if (equal sentence-type '?)
    (setf (incipients (eval *question-incipient-lexicon*))
          (cons (first sentence)(incipients (eval *question-incipient-lexicon*))))
    (setf (incipients (eval *answer-incipient-lexicon*))
          (cons (first sentence)(incipients (eval *answer-incipient-lexicon*))))))

;;;;;
#|Calling (define-cadences (my name is david!) !) 
    define-cadences returned (david!) |#
;;;;;

(defun DEFINE-CADENCES (sentence sentence-type)
  "Finds and returns its arg's cadences."
  (unless (equal sentence-type '*)
    (if (equal sentence-type '?)
      (setf (cadences (eval *question-cadence-lexicon*))
            (cons (my-last sentence)(cadences (eval *question-cadence-lexicon*))))
      (setf (cadences (eval *answer-cadence-lexicon*))
            (cons (my-last sentence)(cadences (eval *answer-cadence-lexicon*)))))))

;;;;;
#| Calling (remove-them (name?)
                      ((name? 7.15) (is 1.5) (david! 2.61) (name 0.7) (my 0.7)
                       (your 1.5) (what 1.0))) 
      remove-them returned ((is 1.5) (david! 2.61) (name 0.7) (my 0.7) (your 1.5)
                       (what 1.0))|#
;;;;;

(defun REMOVE-THEM (list things)
  "Removes its first arg from its second arg."
  (if (null list) things
      (remove-them (rest list)(remove-it (first list) things))))

;;;;;
#|Calling (remove-it name?
                     ((name? 7.15) (is 1.5) (david! 2.61) (name 0.7) (my 0.7)
                      (your 1.5) (what 1.0))) 
     remove-it returned ((is 1.5) (david! 2.61) (name 0.7) (my 0.7) (your 1.5)
                      (what 1.0))|#
;;;;;

(defun REMOVE-IT (thing things)
  "Removes its first arg from its second arg."
  (cond ((null things)())
        ((equal thing (very-first things))
         (remove-it thing (rest things)))
        (t (cons (first things)
                 (remove-it thing (rest things))))))

;;;;;
#|    Calling (choose-the-highest-rated-word
              ((name 1.6) (name? 2.49) (your 1.2) (is 1.6) (what 0.8) (my 1.1))) 
    choose-the-highest-rated-word returned name? |#
;;;;;

(defun CHOOSE-THE-HIGHEST-RATED-WORD (words)
  "Chooses the highest choice from among ties for the honor."
  (first 
   (choose-one (let* ((rated-words (sortcdr '> words))
                      (rating (second (first rated-words))))
                 (loop for word in rated-words
                       if (equal (second word) rating)
                       collect word)))))

;;;;;
#|Calling (display nil) 
     display returned "nil" |#
;;;;;

(defun DISPLAY (response)
  "Simple making of list into string."
  (make-list-into-string response))

;;;;;
#|Calling (get-keyword (who am i speaking to?)) 
       get-keyword returned speaking |#
;;;;;

(defun GET-KEYWORD (sentence)
  "Gets the keyword from its arg."
  (let ((test (loop for word in sentence
                    collect (length (explode word)))))
    (nth (position (first (my-sort '> test)) test) sentence)))

;;;;;
#|Calling (recognize-no (what is your name?)) 
   recognize-no returned nil
|#
;;;;;

(setq *question-incipient-lexicon* (make-instance 'incipient-lexicon))
(setq *answer-incipient-lexicon* (make-instance 'incipient-lexicon))
(setq *question-cadence-lexicon* (make-instance 'cadence-lexicon))
(setq *answer-cadence-lexicon* (make-instance 'cadence-lexicon))

(defun RECOGNIZE-NO (sentence)
  "This function finds the first ocurance of the no word (followed by a *) and
      places it in the *no-sentences* listing.
      Calling (recognize-no (too bad you cant answer!)) 
      recognize-no returned nil"
  (if (find-no sentence)
    (pushnew (first *sentences*) *no-sentences*)
    ()))

;;;;;
#| Calling (find-no (what is your name?)) 
    find-no returned nil |#
;;;;;

(defun FIND-NO (sentence)
  "Tests the sentence to see if it contains the no word."
  (cond ((or (member (first sentence) '(! ? ^))(null sentence))())
        ((or (member '* (explode (first sentence)) :test 'equal)
             (member *no* (list (first sentence)) :test 'equal)
             (if (null (rest sentence))
               (member *no* (list (implode (butlast (explode (first sentence))))) :test 'equal)))
         (let ((test (butlast (explode (first sentence)))))
           (if (equal (my-last test) '*)
             (setq *no* (butlast (implode test)))
             (setq *no* (implode (list (first sentence)))))))
        (t (find-no (rest sentence)))))

;;;;;
#| Calling (recognize-yes (too bad you cant answer!)) 
      recognize-no returned nil |#
;;;;;

(defun RECOGNIZE-YES (sentence)
  "This function finds the first ocurance of the yes word (followed by a ^) and
       places it in the *yes-sentences* listing.
     "
  (if (find-yes sentence)
    (pushnew (first *sentences*) *yes-sentences*)
    ()))

;;;;;
#|   Calling (find-yes (what is your name?)) 
   find-yes returned nil |#
;;;;;

(defun FIND-YES (sentence)
  "Tests the sentence to see if it contains the yes word."
  (cond ((or (member (first sentence) '(! ? ^))(null sentence))())
        ((or (member '^ (explode (first sentence)) :test 'equal)
             (member *yes* (list (first sentence)) :test 'equal)
             (if (null (rest sentence))
               (member *yes* (list (implode (butlast (explode (first sentence))))) :test 'equal)))
         (let ((test (butlast (explode (first sentence)))))
           (if (equal (my-last test) '*)
             (setq *yes* (butlast (implode test)))
             (setq *yes* (implode (list (first sentence)))))))
        (t (find-yes (rest sentence)))))

;;;;;
#|    Calling (add-weighting (no*) (*)) 
    add-weighting returned ((no* (name? 0.85) (computer! 0.1) (david! 0.1)
                             (name 0.1) (my 0.1) (your 0.1) (is 0.1) (what 0.1))) |#
;;;;;

(defun ADD-WEIGHTING (sentence-1 sentence-2)
  "Sentence 1 here is the initiating sentence."
  (loop for word in sentence-1
        collect (cons word (add-weight word sentence-2))))

;;;;;
#|      Calling (add-weight no* (*)) 
     add-weight returned ((name? 0.85) (computer! 0.1) (david! 0.1) (name 0.1)
                          (my 0.1) (your 0.1) (is 0.1) (what 0.1))|#
;;;;;

(defun ADD-WEIGHT (word sentence)
  "Increases the weight of each entry  in word for all of the words in sentence."
  (setf (associations (eval word))
        (reward (associations (eval word)) sentence)))

;;;;;
#|Calling (reward ((david! 2.47) (name 0.9) (up? 0.1) (is 0.5) (what 0.1)
                  (hello! 0.1) (yes^ 0.1))
                 (what is up?)) 
      reward returned ((up? 0.2) (is 1.0) (what 0.2) (david! 2.47) (name 0.9)
                  (hello! 0.1) (yes^ 0.1)) |#
;;;;;

(defun REWARD (associations words)
  "Rewards the weights with a * statement from user."
  (if (null words) associations
      (let ((test (assoc (first words) associations)))
        (if test (reward (cons (list (first test) (round-it (* (second test) *weight-divisor*)))
                               (remove test associations)) 
                         (rest words))
            (reward associations (rest words))))))

;;;;;
#| Calling (other-lexicon-type !) 
      other-lexicon-type returned #<cadence-lexicon #x164BF4E>|#
;;;;;

(defun OTHER-LEXICON-TYPE (type)
  "Returns words from the opposite of its arg sentence type."
  (if (equal type '?) *answer-cadence-lexicon* *question-cadence-lexicon*))

;;;;;
#| Calling (punish ((name? 2.47) (is 1.0) (david! 0.1) (name 0.1) (my 0.1)
                  (your 0.4))
                 (david!)) 
    punish returned ((david! 0.05) (name? 2.47) (is 1.0) (name 0.1) (my 0.1)
                  (your 0.4))|#
;;;;;

(defun PUNISH (associations words)
  "Punishes the weights with a * statement from user."
  (if (null words) associations
      (let ((test (assoc (first words) associations)))
        (if test (punish (cons (list (first test) (round-it (/ (second test) *weight-divisor*)))
                               (remove test associations)) 
                         (rest words))
            (punish associations (rest words))))))

;;;;;
#| Calling (figure-speac what) 
   figure-speac returned s|#
;;;;;

(defun FIGURE-SPEAC (word)
  "This function sets up parsing structure in sentences for future creation of sentences and
      ATN use. Important to note that word types are figured contextually based on their current usage
      and thus don't require a separate parse entry in their slots."
  (let ((count (count word *words*))
        (length (length *words*)))
    (cond ((< count (/ length 10))
           'c)
          ((< count (/ length 8))
           'e)
          ((< count (/ length 6))
           'a)
          ((< count (/ length 4))
           'p)
          (t 's))))

;;;;;
#| Calling (apprentice) 
 apprentice returned nil|#
;;;;;

(defun APPRENTICE ()
  "This function runs the program from the menu."
  (if *initiate* (setq *all-words* nil))
  (progn (setq *no* ())(setq *yes* ())
         (loop until (progn (modal-dialog (make-instance 'music-window))
                      *close*)
               do (setq *input* *name-list*)
               do (if (and (boundp (first *name-list*))(events (eval (first *name-list*))))
                    (progn (setq *name-list* (fix-end-of-music-sentences *name-list*))
                           (setq *input* *name-list*)))
               do (let* ((trial (put-sentence-into-database *input*)))                  ;;;this is where reply is!!
                    (if (not (null (read-from-string trial)))
                      (progn (let ((name (implode (list 'sentence- *counter*)))
                                   (sentence-type (my-last (explode (my-last *response*)))))
                               (set name (make-instance 'sentence 
                                           :name 'me
                                           :sentence-type sentence-type
                                           :sentence (list *response*)
                                           :length-of-sentence (length *response*)
                                           :origination 'apprentice))
                               (pushnew name *sentences*)
                               (incf *counter*)))))
               do (if (not (null *response*))
                    (progn (new-text)
                           (if (and (not (equal (first *response*) '*))(not (equal (first *response*) '^))
                                    (not (null (first *response*)))
                                    (events (eval (first *response*))))
                             (setq *process* (process-run-function "play" 'play-events (apply #'append (make-timings (mapcar #'(lambda (x)(events (eval x))) *response*))))))
                           (message-dialog (make-list-into-string *response*) :title "Apprentice" :position #@(80 160)))
                    (progn (new-text)
                           (message-dialog " ------- " :title "Apprentice" :position #@(80 160))))
               do (gc))))

;;;;;
#|Calling (new-text) 
     new-text returned ((my ((david! 2.47) (name 0.9) (is 0.4)))
                    (name ((david! 2.09) (is 0.8) (my 0.4)))
                    (is ((david! 2.21) (name 0.3) (my 0.3)))
                    (david! ((is 0.2) (name 0.2) (my 0.2))))|#
;;;;;

(defun NEW-TEXT ()
  "Gets the elements from words and sets the table sequence thusly."
  (setq *weight-list* 
        (let ((test (get-element-from-words 'associations)))
          (if test test nil)))
  (set-table-sequence *dialog-text* (reverse *weight-list*)))

;;;;;
#| Calling (parse-the-sentence (c c c c) (huh? name? to?)) 
      parse-the-sentence returned (pretty what stupid huh?)|#
;;;;;

(defun PARSE-THE-SENTENCE (parse cadence)
  "The argument here is the parse found in any sentence parse-it slot."
  (append (let ((parse-lists (remove-cadences (loop for item in (get-associations (collect-parsings *sentences*)) collect (reverse item)))))
            (loop for element in (butlast parse)
                  collect (second (assoc element (mix parse-lists)))))
          (list (choose-one cadence))))

;;;;;
#| Calling (collect-parsings (sentence-3 sentence-2 sentence-1)) 
       collect-parsings returned ((c too) (c bad) (c you) (c cant) (c answer!) (c what)
                            (c is) (c your) (c name?) (c who) (c am) (c i)
                            (c speaking) (c to?))|#
;;;;;

(defun COLLECT-PARSINGS (sentences)
  "Pairs the parsings with the words in the sentences."
  (pair (apply #'append (loop for sentence in sentences
                              collect (first (sentence (eval sentence)))))
        (apply #'append (loop for sentence in sentences
                              collect (parse-it (eval sentence))))))

;;;;;
#|Calling (pair (c c c c c c c c c c c c c c)
                 (too bad you cant answer! what is your name? who am i speaking to?)) 
   pair returned ((c too) (c bad) (c you) (c cant) (c answer!) (c what) (c is)
                  (c your) (c name?) (c who) (c am) (c i) (c speaking) (c to?)) |#
;;;;;

(defun PAIR (list-1 list-2)
  "Pairs the two list args."
  (loop for item in list-1
        collect (list item (first list-2))
        do (setf list-2 (rest list-2))))

;;;;;
#|Calling (mix ((c too) (c bad) (c you) (c cant) (c answer!) (c what) (c is) (c your)
                (c name?) (c who) (c am) (c i) (c speaking) (c to?))) 
       mix returned ((c is) (c bad) (c to?) (c too) (c i) (c answer!) (c speaking) (c who)
                (c am) (c your) (c what) (c name?) (c you) (c cant)) |#
;;;;;

(defun MIX (list)
  "Pseudo-randomly mixes the list arg."
  (if (null list) nil
      (let ((choice (choose-one list)))
        (cons choice (mix (remove choice list :count 1))))))

;;;;;
#|Calling (remove-cadences
            ((c too) (c bad) (c you) (c cant) (c answer!) (c what) (c is) (c your)
             (c name?) (c who) (c am) (c i) (c speaking) (c to?))) 
       remove-cadences returned ((c too) (c bad) (c you) (c cant) (c what) (c is) (c your)
                            (c who) (c am) (c i) (c speaking)) |#
;;;;;

(defun REMOVE-CADENCES (choices)
  "Removes the cadences from the choices."
  (cond ((null choices)())
        ((or (member (second (first choices)) (cadences *answer-cadence-lexicon*))
             (member (second (first choices)) (cadences *question-cadence-lexicon*)))
         (remove-cadences (rest choices)))
        (t (cons (first choices)(remove-cadences (rest choices))))))

;;;;;
#|Calling (get-associations
           ((pretty c) (what c) (stupid c) (huh? c) (that c) (what c) (i c) (said! c)
            (huh? c) (sounds c) (pretty c) (stupid c) (to c) (me! c) (too c)...
      get-associations returned ((c sounds) (c stupid) (c said!) (c that) (c name?) (c is)
                            (c pretty) (c to) (c me!) (c answer!) (nil to?)
                            (nil speaking))|#
;;;;;

(defun GET-ASSOCIATIONS (parsed-words)
  "Gets the associations for its arg."
  (let ((relevant-words 
         (compound-associations 
          (apply #'append 
                 (loop for word in (first (sentence (eval (first *sentences*))))
                       collect (associations (eval word)))))))
    (relate-words relevant-words parsed-words)))

;;;;;
#|Calling (relate-words ((sounds 0.8888888888888888) (stupid 0.5) (said! 7.5)
                         (that 0.5) (name? 10.0) (is 1.0) (pretty 0.5) (to 0.5)
                         (me! 0.1) (answer! 0.1) (to? 0.1) (speaking 0.1))
                        ((pretty c) (what c) (stupid c) (huh? c) (that c) (what c)....
    relate-words returned ((sounds c) (stupid c) (said! c) (that c) (name? c) (is c)
                          (pretty c) (to c) (me! c) (answer! c) (to? nil)
                          (speaking nil))|#
;;;;;

(defun RELATE-WORDS (words associations)
  "Relates the words with their SPEAC symbols to words with weightings alone."
  (cond ((null words)())
        ((let ((test (assoc (very-first words) associations)))
           (if test (cons test (relate-words (rest words) associations)))))
        (t (relate-words (rest words) associations))))

;;;;;
#| (get-parse-elements '(c c c c c))
     (c)
     (get-parse-elements '(c c s c c))
     (c s)|#
;;;;;

(defun GET-PARSE-ELEMENTS (parse)
  "Gets the SPEAC parase elements for the SPEAC sentence."
  (if (null parse)()
      (cons (first parse)
            (get-parse-elements (remove (first parse) parse :test #'equal)))))

;;;;;
#| Calling (get-element-from-words associations) 
 get-element-from-words returned ((here!
                                   ((am 0.2) (i 0.2) (you? 0.1) (are 0.1)
                                    (how 0.1) (improv[6]-75-66-70-54-44! 0.1)
                                    (improv[7]-75-66-70-54-44-61 0.1)
                                    (improv[7]-75-66-70-54-44-61? 0.1)
                                    (improv[8]-75-66-70-54-44-61-59 0.1)
                                    (improv[10]-75-68-59 0.1))) . . . |#
;;;;;

(defun GET-ELEMENT-FROM-WORDS (type)
  "This is a test function for getting infor from words. Type can be
       predecessors successors keywords word-type positions-in-sentence associations usage music.
       weight is stored in associations."
  (let ((words (remove-duplicates *words*)))
    (mapcar #'(lambda (x)(list x (funcall type (eval x)))) words)))

(require :scrollers)

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
  ; Leave, in rest, only the four keywords we want to pass to the
  ; make-instance for scroller-class. This allows them to default
  ; as desired by scroll-class.
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
  ; We use the values of these keywords by modifying the rest parameter
  (declare (ignore scroll-bar-class h-scroll-class v-scroll-class
                   track-thumb-p field-size))
  (call-next-method)
  ; Leave, in rest, only the four keywords we want to pass to the
  ; make-instance for scroller-class. This allows them to default
  ; as desired by scroll-class.
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

(defClass SCROLLING-WINDOW-SCROLLER2 (scroller) ()
  (:default-initargs
    :field-size #@(1320 1520)))

(defMethod VIEW-DRAW-CONTENTS ((self SCROLLING-WINDOW-SCROLLER2))
  (call-next-method))

(defMethod INITIALIZE-INSTANCE ((window SCROLLING-WINDOW-SCROLLER2) &rest initargs)
  (apply #'call-next-method window initargs)
  (add-subviews window 
    (setf *dialog-text* 
          (make-instance 
            'sequence-dialog-item
            :view-font '("times" 12)
            :view-position #@(0 0)
            :view-size #@(1320 1510)
            :table-sequence (reverse *weight-list*)))))

(defClass SCROLLING-WINDOW-SCROLLER1 (scroller)  ((sentences :initarg :sentences :initform *new-sentences* :accessor sentences))
  (:default-initargs
    :field-size (make-point ;330 
                 (round (+ (* (length *sentences*) 300) 830))
                 (round (* (length *sentences*) 60)))))

(defMethod VIEW-DRAW-CONTENTS ((window SCROLLING-WINDOW-SCROLLER1))
  (call-next-method)
  (set-pen-pattern window *black-pattern*)
  (set-view-font window :bold)
  (set-view-font window 10)
  (set-view-font window "geneva")
  (let ((points (create-the-coordinates (sentences window))))
    (loop until (null (rest points))
          do (let* ((point (first points))
                    (rest-points (rest points)))
               (with-fore-color *line-color* 
                 (loop until (null rest-points)
                       do (move-to window (make-point (first point))(second point))
                       do (line-to window (make-point (first (first rest-points))(second (first rest-points))))
                       do (setf rest-points (rest rest-points)))))
          do (setf points (rest points)))
    (let ((new-points (create-the-coordinates (sentences window)))
          (words (apply #'append (sentences window))))
      (loop for entry in new-points
            do (make-paint-oval window entry (first words))
            do (setf words (rest words))))))

(defun MAKE-PAINT-OVAL (window entry word)
  "Calling (make-paint-oval #<scrolling-window-scroller1 #x1FCDC06> (136 30) my) 
    make-paint-oval returned #<A Mac Non-zone Pointer #x1C76B7A>"
  (set-pen-pattern window *black-pattern*)
  (frame-oval window 
              (make-point (- (first entry) 10)(- (second entry) 10))
              (make-point (+ (first entry) 10)(+ (second entry) 10)))
  (set-pen-pattern window *white-pattern*)
  (if *reverse-mode* (invert-oval window
                                  (make-point (- (first entry) 9)(- (second entry) 9))
                                  (make-point (+ (first entry) 9)(+ (second entry) 9)))
      (paint-oval window
                  (make-point (- (first entry) 9)(- (second entry) 9))
                  (make-point (+ (first entry) 9)(+ (second entry) 9))))
  (move-to window (make-point (+ (first entry) 10)(- (second entry) 2)))
  (if *reverse-mode* (with-fore-color *white-color* (format window "~A" (write-to-string word)))
      (with-fore-color *word-color* (format window "~A" (write-to-string word))))
  (set-pen-pattern window *black-pattern*))

(defVar *HORI-CONS* 50)

;;;;;
#|Calling (create-the-coordinates
           ((my name is david!) (what is your name?) (david!) (no*) (*)
            (your name is computer!) (name?) (what is your name?) (computer!))) 
      create-the-coordinates returned ((136 30) (272 30) (408 30) (544 30) (136 90)....|#
;;;;;

(defun CREATE-THE-COORDINATES (sentences)
  "Creates the coordinates for the association graph."
  (apply #'append
         (let* ((horizontal-constant 0)
                (horizontal-cumulative 10) ;horizontal-constant)
                (vertical-constant 60)
                (vertical-cumulative 30))
           (loop for sentence in sentences
                 do (setf horizontal-constant (round (/ (+ (* (length *sentences*) 
                                                              (if (any-greater-than? 12 (mapcar #'(lambda (x)(length (explode (first x)))) sentences)) 400 *hori-cons*)) 130)
                                                        (1+ (length sentence)))))
                 do (setf horizontal-cumulative horizontal-constant)
                 collect (loop for word in sentence
                               collect (list horizontal-cumulative vertical-cumulative)
                               do (setf horizontal-cumulative (+ horizontal-cumulative horizontal-constant)))
                 do (setf vertical-cumulative (+ vertical-constant vertical-cumulative))))))

;;;;;
#|  Calling (any-greater-than?
           12
           (6 4 2 2 5 4 5 4 9 4 6 4 4 3 1 4 4 4 2 3 1 4 2 3 1 4 2))
 any-greater-than? returned nil|#
;;;;;

(defun ANY-GREATER-THAN? (n list-of-numbers)
  "Determines if any in second arg are greater than its first arg."
  (cond ((null list-of-numbers)())
        ((> (first list-of-numbers) n) t)
        (t (any-greater-than? n (rest list-of-numbers)))))

;;;;;
#|  Calling (get-music-associations
           ((improv[6]-75-66-70-54-44! 0.5) (improv[7]-75-66-70-54-44-61 0.8)
            (improv[7]-75-66-70-54-44-61? 1.0) (improv[10]-75-68-59 0.4))) 
 get-music-associations returned ((improv[6]-75-66-70-54-44! 0.5)
                                  (improv[7]-75-66-70-54-44-61 0.8)
                                  (improv[7]-75-66-70-54-44-61? 1.0)
                                  (improv[10]-75-68-59 0.4))|#
;;;;;

;;;separating the music out from the words
(defun GET-MUSIC-ASSOCIATIONS (associations)
  (cond ((null associations)())
        ((events (eval (very-first associations)))
         (cons (first associations)
               (get-music-associations (rest associations))))
        (t (get-music-associations (rest associations)))))

;;;;;
#| Calling (get-music-words (improv[10]-75-68-59)) 
 get-music-words returned (improv[10]-75-68-59) |#
;;;;;

(defun GET-MUSIC-WORDS (words)
  (cond ((null words)())
        ((events (eval (first words)))
         (cons (first words)
               (get-music-words (rest words))))
        (t (get-music-words (rest words)))))

;;;;;
#| Calling (get-word-words (improv[8]-75-66-70-54-44-61-59)) 
 get-word-words returned nil |#
;;;;;

(defun GET-WORD-WORDS (words)
  (cond ((null words)())
        ((events (eval (first (if (listp (first words))
                                (list (very-first words))
                                (list (first words))))))
               (get-word-words (rest words)))
        (t (cons (first words)
                 (get-word-words (rest words))))))

;;;;;
#| Calling (get-word-associations
           ((you? 2.09) (are 0.8) (improv[6]-75-66-70-54-44! 0.1)
            (improv[7]-75-66-70-54-44-61 0.1) (improv[7]-75-66-70-54-44-61? 0.1)
            (improv[8]-75-66-70-54-44-61-59 0.1) (improv[10]-75-68-59 0.1))) 
 get-word-associations returned ((you? 2.09) (are 0.8)) |#
;;;;;

(defun GET-WORD-ASSOCIATIONS (associations)
  (cond ((null associations)())
        ((events (eval (very-first associations)))
         (get-word-associations (rest associations)))
        (t (cons (first associations)
                 (get-word-associations (rest associations))))))

;;;;;
#| Calling (play-sentence
           (improv[10]-75-68-59 improv[8]-75-66-70-54-44-61-59
            improv[7]-75-66-70-54-44-61?)) 
 play-sentence returned nil|#
;;;;;

(defun PLAY-SENTENCE (sentence)
  "Plays a sentence of word objects."
  (play-events (apply #'append (re-time (loop for word in sentence
                                              if (events (eval word))
                                              collect (events (eval word)))))))

;;;;;
#|(re-time '(((1000 28 1000 4 127))))
   (((0 28 1000 4 127))) |#
;;;;;

(defun RE-TIME (event-lists &optional (current-time 0))
  "Retimes the events lists to begin one after the other."
  (if (null event-lists)()
      (cons (loop for event in (set-to-zero (first event-lists))
                  collect (cons (+ (first event) current-time) (rest event)))
            (re-time (rest event-lists) (+ current-time 
                                           (get-beat-length 
                                            (first event-lists)))))))

;;;;;
#| Calling (set-to-zero ((1000 28 1000 4 127))) 
    set-to-zero returned ((0 28 1000 4 127))|#
;;;;;

(defun SET-TO-ZERO (events &optional (subtract (very-first events)))
  "Sets the events to begin on zero."
  (if (null events)()
      (cons (cons (- (very-first events) subtract)
                  (rest (first events)))
            (set-to-zero (rest events) subtract))))

;;;;;
#| Calling (get-beat-length ((1000 28 1000 4 127))) 
    get-beat-length returned 1000|#
;;;;;

(defun GET-BEAT-LENGTH (events)
  "this is used in re-time for setting the new time!
   requires that the first in events be sorted to current time!"
  (let ((time (very-first events)))
    (first (my-sort #'> (loop for event in events
                              collect (get-note-timing event time))))))

;;;;;
#|Calling (get-note-timing (1000 28 1000 4 127) 1000) 
  get-note-timing returned 1000|#
;;;;;

(defun GET-NOTE-TIMING (event time)
  "grunt work for get-beat-length"
  (- (+ (first event)(third event)) time))

;;;;;
#| Calling (reveal-the-hidden-events
           (improv[10]-75-68-59 improv[8]-75-66-70-54-44-61-59
            improv[7]-75-66-70-54-44-61?)) 
 reveal-the-hidden-events returned (((4912 75 5 1 117 tie) (4912 68 5 1 89 tie)
                                     (4912 59 5 1 76 tie))
                                    ((4425 75 483 1 117 tie) (4425 66 150 1 77)
                                     (4425 70 141 1 86) (4425 54 183 1 80) . . .|#
;;;;;

(defun REVEAL-THE-HIDDEN-EVENTS (events)
  "Reveals the events in words."
  (mapcar #'(lambda (x)(events (eval x))) events))

;;;;;
#|Calling (tester-for-hidden-events
            (improv[10]-75-68-59 improv[8]-75-66-70-54-44-61-59
             improv[7]-75-66-70-54-44-61?)) 
  tester-for-hidden-events returned ((4912 75 5 1 117 tie) (4912 68 5 1 89 tie)
                                     (4912 59 5 1 76 tie))|#
;;;;;

(defun TESTER-FOR-HIDDEN-EVENTS (sentence)
  "Tests to see if arg is a music sentence."
  (and (boundp (first sentence))
       (events (eval (first sentence)))))

;;;;;
#| Calling (return-only-music-sentences (sentence-1)) 
 return-only-music-sentences returned ((improv[10]-75-68-59
                                        improv[8]-75-66-70-54-44-61-59
                                        improv[7]-75-66-70-54-44-61?))|#
;;;;;

(defun RETURN-ONLY-MUSIC-SENTENCES (sentence-objects)
  "The arg to this should be *sentences*."
  (let ((real-sentences (apply #'append (mapcar #'(lambda (x)(sentence (eval x))) sentence-objects))))
    (loop for sentence in real-sentences
          if (tester-for-hidden-events sentence)
          collect sentence)))

;;;;;
#| Calling (create-a-work (sentence-1)) 
 create-a-work returned (((4412 75 5 1 117 tie) (4412 66 5 1 77 tie)
                          (4412 70 5 1 86 tie)))|#
;;;;;

(defun CREATE-A-WORK (sentences)
 "The arg to this should be *sentences*"
  (reveal-the-hidden-events (apply #'append (reverse (return-only-music-sentences sentences)))))

;;;;;
#| Calling (fix-end-of-music-sentences (improv[5]-75-66-70 !)) 
 fix-end-of-music-sentences returned (improv[5]-75-66-70!)|#
;;;;;

(defun FIX-END-OF-MUSIC-SENTENCES (sentence)
  "Attaches the punctuation to the end of the music sentence."
 (let ((object (nth (- (length sentence) 2) sentence))
       (the-name (implode (nthcdr (- (length sentence) 2) sentence))))
   (progn (set the-name (make-instance 'word 
                          :name (name (eval object))
                          :timing (timing (eval object))
                          :destination (destination (eval object))
                          :events (events (eval object))
                          :usage (usage (eval object))))
          (append (butlast sentence 2) (list the-name)))))

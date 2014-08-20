

                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;          Associate/Chapter 9        ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;    simple code to run associate     ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

;;;
#|Since this version of Associate simply uses note names as musical input,
I have not included here any play midi functions for actual playback. One could 
easily adapt the standalone quicktime module for playback to accomplish this.
However, I would use the complete version of Apprentice for this purpose.
Otherwise, the version of Associate should return all of the responses given by CMMC
as shown in various examples and figures in chapter 9.|#
;;;

(require 'quickdraw)
(require 'scrolling-windows)
(require :scrollers)

;;;  simple environment variables 
(setq *save-doc-strings* t)
(setq *arglist-on-space* t)
(setq *save-local-symbols* t)
(setq *print-case* :downcase)
(setq *paste-with-styles* ())
(setq *fasl-save-local-symbols* t)
(setq *save-definitions* t)
(setq *break-on-errors* ())
(setq *trace-print-length* 100)
(setq *trace-print-level* 20)
(setq *print-pretty* t)

;;;  main variables
(defVar *REVERSE-MODE* () "Inversion of visual graphs.")
(defVar TEST () "For use in top-level to catch cancel in the send mode.")
(defVar *RHYTHM* () "Durations for musical I/O.")
(defVar *CURRENT-WORDS* () "The current words input.")
(defVar *INPUT-WORK* () "For use with input music.")
(defVar *PLAY* () "For playing or not.")
(defVar *PREDECESSOR-WEIGHT* .5 "Default predecessor weight.")
(defVar *KEYWORD-WEIGHT* .75 "Default keyword weight.")
(defVar *SUCCESSOR-WEIGHT* .5 "Default successor weight.")
(defVar *KEYWORDS* () "Where keywords are kept.")
(defVar *LAST-WORDS* () "Where last words are kept.")
(defVar *BROAD-KEYWORD-WEIGHT* .1 "Where keyword weight is kept.")
(defVar *BACKWARD-CHAIN-WEIGHT* .1 "Where backward chain weitht is kept.")
(defVar MY-MENU *default-menubar* "Storage of default menubar.")
(defVar *CREATOR* () "Simple variable to add color to creator name in window.")
(defVar *EMAIL* () "Simple variable to add color to email name in window.")
(defVar *VISUAL* () "Simple variable to add color to visual name in window.")
(defVar *SOFTWARE* () "Simple variable to add color to software name in window.")
(defVar *TITLE* () "Simple variable to add color to title name in window.")
(defVar *COPYRIGHT* () "Simple variable to add color to copyright name in window.")
(defVar *WEIGHT-LIST* () "Default weights list.")
(defVar *ALL-WORDS* () "All words weight list.")
(defVar *LAST-WORD-WEIGHT* 0.2 "Last word weight.")
(defVar *KEYWORD-WEIGHT* 0.15 "Keyword weight.")
(defVar *LAST-WORD* () "The last word.")
(defVar *WEIGHT-DIVISOR* 2 "The default weight divisor.")
(defVar *YES-SENTENCES* () "Yes sentence storage.")
(defVar *NO-SENTENCES* () "No sentence storage.")
(defVar *YES* () "A test for yes.")
(defVar *GRAPH-WINDOW* () "Window variable.")
(defVar *NEW-SENTENCES* () "New sentence storage.")
(defVar *INITIATE* t "For beginning a new session.")
(defVar *WORD-COLOR* *blue-color* "Word color variable.")
(defVar *LINE-COLOR* *pink-color* "Line color variable.")
(defVar *DIALOG-TEXT* (make-instance 
                           'sequence-dialog-item
                           :view-font '("monaco" 10)
                           :view-position #@(0 0)
                           :view-size #@(1390 1400)
                           :table-sequence (reverse *weight-list*)))
(defVar *SOUND* () "Whether sound is used.")
(defVar *PLAY-WINDOW* () "Play window variable.")
(defVar *WEIGHT-WIND* () "Weight window variable.")
(defVar *RS* (make-random-state t) "Variable for storing the current random state.")
(defVar *TRANSCRIPT-WINDOW* () "Transcript window variable.")
(defVar *OUTPUT* () "Output variable.")
(defVar *STORAGE* () "Main storage variable.")
(defVar *INPUT* "" "Main input variable.")
(defVar *PREDECESSOR* () "Predecessor storage.")
(defVar *SUCCESSOR* () "Successor storage.")
(defVar *KEYWORD* () "Keyword storage.")
(defVar *SENTENCES* () "Sentence storage.")
(defVar *WORDS* () "Where words are kept.")
(defVar *COUNTER* 1 "Simple counter.")
(defVar *QUESTION-INCIPIENT-LEXICON* () "Question lexicon.")
(defVar *ANSWER-INCIPIENT-LEXICON* () "Answer lexicon.")
(defVar *QUESTION-CADENCE-LEXICON* () "Question cadence lexicon.")
(defVar *ANSWER-CADENCE-LEXICON* () "Answer cadence lexicon.")
(defVar *RESPONSE* () "Response kept here.")
(defVar *NO* () "The word for no.")
(defVar WORD () "Basic word variable.")
(defVar *END* () "Signifies the end.")
(defVar *TRANSCRIPT* () "Transcript variable.")
(defVar *TEMPORARY-BUFFER* () "temporary buffer for saving sessions.")
(defVar *NAME-OF-FILE* () "Name of file variable.")
(defVar *HELP* '("""""                   Associate" " "
"GENERAL"
""
"Associate is a small program designed to accept language (any)"
"as input and slowly, through exchange with the user, develop a small "
"ability to converse in that language. The rules are simple. "
"All statements and questions should be concluded with an exclamation "
"point (!) or question mark (?) respectively followed by <cr>. "
"Do not use <cr> or any other special symbols except when you "
"use your word for no - end that word with a star (*),"
"or use your word for yes - end that word with a caret (^)."
""
"Associate uses a limited association net. Association nets "
"can provide useful connectivity for programs such "
"as Associate. Associate cannot respond until both "
"questions and statements have been entered. "
"As examples, Who and What will be connected as "
"initiating words for questions, name and David will connect "
"as (for example) last words of sentences, and Who, Am, and "
"I relate to Speaking as important and related words. One can "
"travel in any direction in an association net. Hence, a word "
"in sentence 1 can access a word in sentence 5 and vice versa."
""
"Associate attempts to define words by tracing connections to "
"various associations so that individual word meanings become "
"heightened. Many other aspects of the words or nodes can also "
"benefit from such associations. Parsing becomes clearer as words such "
"as Who and What can coalesce into a single representation. "
""
"Association nets can resemble neural nets. However, there are a "
"number of critically important differences between these two "
"approaches. First, neural nets are generally fixed in terms of "
"the nodal content while association nets have potential for an "
"infinite number of interconnected nodes. Association nets also "
"begin as empty containers while neural nets already possess nodes. "
"While association nets can chain backward as well as forward they "
"do not back propagate as do neural nets. Association nets do not, "
"therefore, require training. Finally, and possibly most importantly,"
"association nets do not have hidden units. The nodes in neural "
"nets can be accessed at any time and will reveal all of their "
"associations. "
""
"In Associate, the association net is a natural outgrowth of the "
"slots present in each word object. Examining these slots will show "
"how connections and access continues to expand as the association "
"net grows. As well, since word objects in Associate are a subclass "
"of sentence objects, word objects have access to the sentence objects "
"slots. Therefore, the nodes shown in the associations window "
"have more information than shown. For example, each word object has "
"access to the length and content of each preceding sentence. This "
"information can provide important contextualization for the various "
"instances of associated words and lead to further implicated "
"information relating to word meanings, parsing representations, and other" 
"critical factors in determining logical machine responses to user input."
""
"Associate also uses sound as input. The Mac keyboard represents, "
"when selecting the sound option, the following: "
"    c   =   C natural octave 1 "
"    1   =   C sharp octave 1 "
"    d   =   D natural octave 1 "
"    2   =   D sharp octave 1 "
"    e   =   E natural octave 1 "
"    f   =   F natural octave 1 "
"    3   =   F sharp octave 1 "
"    g   =   G natural octave 1 "
"    4   =   G sharp octave 1 "
"    a   =   A natural octave 1 "
"    5   =   A sharp octave 1 "
"    b   =   B natural octave 1 "
"    C   =   C natural octave 2 "
"    6   =   C sharp octave 2 "
"    D   =   D natural octave 2 "
"    7   =   D sharp octave 2 "
"    E   =   E natural octave 2 "
"    F   =   F natural octave 2 "
"    8   =   F sharp octave 2 "
"    G   =   G natural octave 2 "
"    9   =   G sharp octave 2 "
"    A   =   A natural octave 2 "
"    0   =   A sharp octave 2 "
"    B   =   B natural octave 2 " 
"Make sure to 1) leave spaces between musical words, and 2) end each "
"musical sentence with a ? or ! just as in the language version. "
""
"Send all questions and comments about Associate to:"
"David Cope"
"howell@cats.ucsc.edu"
""
"                                    - David Cope"
))

;;;  The basic classes.

(defClass SENTENCE nil
  ((name :initarg :name :initform nil :accessor name)
   (sentence-type :initarg :sentence-type :initform nil :accessor sentence-type)
   (sentence :initarg :sentence :initform nil :accessor sentence)
   (length-of-sentence :initarg :length-of-sentence :initform nil :accessor length-of-sentence)
   (parse-it :initarg :parse-it :initform nil :accessor parse-it)
   (origination :initarg :origination :initform nil :accessor origination))
  (:documentation "this is the top-level object for sentences in a database file."))

(defClass WORD (sentence)
  ((predecessors :initarg :predecessors :initform nil :accessor predecessors)
   (successors :initarg :successors :initform nil :accessor successors)
   (keywords :initarg :keywords :initform nil :accessor keywords)
   (word-type :initarg :word-type :initform nil :accessor word-type)
   (positions-in-sentence :initarg :positions-in-sentence :initform nil :accessor positions-in-sentence)
   (associations :initarg :associations :initform nil :accessor associations)
   (usage :initarg :usage :initform nil :accessor usage)
   (music :initarg :music :initform nil :accessor music))
  (:documentation "this is the top-level object for words in a database file."))

(defClass INCIPIENT-LEXICON (word)
  ((incipients :initarg :incipients :initform nil :accessor incipients))
  (:documentation "this is the top-level object for incipient lexicons in a database file."))

(defClass CADENCE-LEXICON (word)
  ((cadences :initarg :cadences :initform nil :accessor cadences))
  (:documentation "this is the top-level object for cadence lexicons in a database file."))

;;;;;
#|(compound-associations '((name? 0.75) (name? 0.2) (is 0.5) (hello! 0.1)))
    ((name? 0.95) (is 0.5) (hello! 0.1))|#
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
#|(remove-object-twice name? '((name? 0.75) (name? 0.2) (is 0.5) (hello! 0.1)))
    ((is 0.5) (hello! 0.1))|#
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
#|(compare-words 'hello 'hello)
   t|#
;;;;;

(defun COMPARE-WORDS (first-word second-word)
  "Compares the two word arguments for similarity."
  (let ((test-1 (explode first-word))
        (test-2 (explode second-word)))
    (if (or (equal test-1 (firstn (length test-1)(explode second-word)))
            (equal test-2 (firstn (length test-1)(explode first-word)))) t)))

;;;;;
#|(put-sentence-into-database '(hello!))
   "nil"|#
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
#|(reply '! '(hello!))
    nil|#
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
        (t (let ((choices (compound-associations                              ;;;this is a pro-rated list of all of the associations of the sentence argument
                           (apply #'append 
                                  (loop for word in sentence
                                        collect (associations (eval word))))))
                 (incipients (if (equal type '?)     ;;;this is a just in case choices is nil listing of alternatives sentence incipients
                               (my-remove (list (eval *no*)) (incipients *answer-incipient-lexicon*))
                               (incipients *question-incipient-lexicon*))))
             (setq *current-words* ())
             (if (or (null choices)(null incipients))
               ()
               (let ((current-word                                            ;;;here's where we get a current word - the highest rated word in choices
                      (let ((trial (choose-the-highest-rated-word 
                                    (remove-them (cadences (if (equal type '?) 
                                                             *question-cadence-lexicon* 
                                                             *answer-cadence-lexicon*)) 
                                                 choices))))
                        (if trial trial (choose-one incipients))))            ;;;here is where we resort to incipients if necessary
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
                                                                   (cadences 
                                                                    (if (equal type '?) *question-cadence-lexicon* *answer-cadence-lexicon*)))
                                                           (associations (eval current-word))))))
                                                    (if test test 
                                                        (choose-the-one 
                                                         (loop for association in (associations (eval current-word))
                                                               collect (first association))))))))))
                   new-sentence)))))))

;;;;;
#|(choose-the-one '(a b c))
   b|#
;;;;;

(defun CHOOSE-THE-ONE (stuff)
  "A simple cover for choose-one."
  (choose-one stuff))

;;;;;
#|(reduce-weighting '(what is my name?) '(david!))
   ((what (david! 0.86) (name? 7.61) (is 3.7) (computer! 1.72)
       (name 1.0) (my 1.0) (your 1.4) . . . |#
;;;;;

(defun REDUCE-WEIGHTING (sentence-1 sentence-2)
  "Sentence 1 here is the initiating sentence."
  (loop for word in sentence-1
        collect (cons word (reduce-weight word sentence-2))))

;;;;;
#|(reduce-weight 'what '(david!))
   ((david! 0.86) (name? 7.61) (is 3.7) (computer! 1.72) (name 1.0)
                            (my 1.0) (your 1.4) (hello! 0.3))|#
;;;;;

(defun REDUCE-WEIGHT (word sentence)
  "Reduces the weight of each entry  in word for all of the words in sentence."
  (setf (associations (eval word))
        (punish (associations (eval word)) sentence)))

;;;;;
#|(make-weight-list 'name? 0.75) 
    ((name? 0.75))|#
;;;;;

(defun MAKE-WEIGHT-LIST (name weight)
  "A simple cover for double listing."
  (list (list name weight)))

;;;;;
#|(add-word-to-word-weightlists 'hello!)
   nil|#
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
#|(round-it 0.95)
   0.95|#
;;;;;

(defun ROUND-IT (n)
  "Simple utility to limit decimal places."
  (float (/ (round (* n 100)) 100)))

;;;;;
#|(establish-keywords '(what is your name?))
   (name? hello!)|#
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
#|(get-sentence-type '(hello!))
    !|#
;;;;;

(defun GET-SENTENCE-TYPE (sentence)
  "Returns the sentence type of question or statement."
  (my-last (explode (my-last sentence))))

;;;;;
#|(make-new-name)
    sentence-1|#
;;;;;

(defun MAKE-NEW-NAME ()
  "Returns a new sentence name."
  (implode (list 'sentence '- *counter*)))

;;;;;
#|(make-sentence-object '(hello!) '! 'sentence-1)
   2|#
;;;;;

(defun MAKE-SENTENCE-OBJECT (sentence sentence-type name)
  "Associations in this version are of four types:
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
#|(make-word-objects '(hello!) '! 'sentence-1)
    nil|#
;;;;;

(defun MAKE-WORD-OBJECTS (sentence sentence-type name)
  "Makes the words objects for Colaborator."
  (loop for word in sentence
        do (if (not (member word *words*))
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
                      :music (first *input-work*)))
               (if (not (equal sentence-type '*))
                 (push word *all-words*))
               (setq *input-work* (rest *input-work*)))
             (progn
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
                              (loop for item in (my-remove (list word) *all-words*)
                                    collect (list item *BACKWARD-CHAIN-WEIGHT*))
                              (associations (eval word)))))
               (setf (usage (eval word))
                     (1+ (usage (eval word))))))
        do (setf *predecessor* word)
        do (setf *successor* (nth (+ (position word sentence) 2) sentence))
        do (pushnew word *words*)
        if (not (equal sentence-type '*))
        do (loop for item in sentence
                 do (add-word-to-word-weightlists item))))

;;;;;
#|(parse-sentence '(hello!) 'sentence-1)
   (s)|#
;;;;;

(defun PARSE-SENTENCE (sentence name)
  "Parses the SPEAC for a given sentence."
  (setf (parse-it (eval name))
        (loop for word in sentence
              collect (figure-speac word))))

;;;;;
#|(define-incipients '(hello!) '!)
   (hello!)|#
;;;;;

(defun DEFINE-INCIPIENTS (sentence sentence-type)
  "Defines the incipient word."
  (if (equal sentence-type '?)
    (setf (incipients (eval *question-incipient-lexicon*))
          (cons (first sentence)(incipients (eval *question-incipient-lexicon*))))
    (setf (incipients (eval *answer-incipient-lexicon*))
          (cons (first sentence)(incipients (eval *answer-incipient-lexicon*))))))

;;;;;
#|(define-cadences '(hello!) '!)
   (hello!)|#
;;;;;

(defun DEFINE-CADENCES (sentence sentence-type)
  "Defines the cadence type."
  (unless (equal sentence-type '*)
    (if (equal sentence-type '?)
      (setf (cadences (eval *question-cadence-lexicon*))
            (cons (my-last sentence)(cadences (eval *question-cadence-lexicon*))))
      (setf (cadences (eval *answer-cadence-lexicon*))
            (cons (my-last sentence)(cadences (eval *answer-cadence-lexicon*)))))))

;;;;;
#|(remove-them '(name?) '((name? 6.77) (is 1.4) (hello! 0.4) (your 1.4) (what 0.9)))
    ((is 1.4) (hello! 0.4) (your 1.4) (what 0.9))|#
;;;;;

(defun REMOVE-THEM (list things)
  "Removes all of the list from things non-destructively."
  (if (null list) things
      (remove-them (rest list)(remove-it (first list) things))))

;;;;;
#|(remove-it 'name? '((name? 6.77) (is 1.4) (hello! 0.4) (your 1.4) (what 0.9))) 
    ((is 1.4) (hello! 0.4) (your 1.4) (what 0.9))|#
;;;;;

(defun REMOVE-IT (thing things)
  "Removes the thing from thigs non-destructively."
  (cond ((null things)())
        ((equal thing (very-first things))
         (remove-it thing (rest things)))
        (t (cons (first things)
                 (remove-it thing (rest things))))))

;;;;;
#|(choose-the-highest-rated-word '((is 1.4) (hello! 0.4) (your 1.4) (what 0.9))) 
   your|#
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
#|(display '(your what is hello!)) 
   "your what is hello!"|#
;;;;;

(defun DISPLAY (response)
  "Displays its arg."
  ;(speak-text (make-list-into-string response))
  (make-list-into-string response))

;;;;;
#|(get-keyword '(hello!))
  hello!|#
;;;;;

(defun GET-KEYWORD (sentence)
  "Returns the keyword of sentence."
  (let ((test (loop for word in sentence
                    collect (length (explode word)))))
    (nth (position (first (my-sort '> test)) test) sentence)))

;;;;;
#|(recognize-no '(hello!))
  nil|#
;;;;;

(defun RECOGNIZE-NO (sentence)
  "This function finds the first ocurance of the no word (followed by a *) and
      places it in the *no-sentences* listing."
  (if (find-no sentence)
    (pushnew (first *sentences*) *no-sentences*)
    ()))

;;;;;
#|(find-no '(hello!))
   nil|#
;;;;;

(defun FIND-NO (sentence)
  "Tests the sentence to see if it contains the no word."
  (cond ((null sentence)())
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
#|(recognize-yes '(hello!))
   nil|#
;;;;;

(defun RECOGNIZE-YES (sentence)
  "This function finds the first ocurance of the yes word (followed by a ^) and
       places it in the *yes-sentences* listing."
  (if (find-yes sentence)
    (pushnew (first *sentences*) *yes-sentences*)
    ()))

;;;;;
#|(find-yes '(hello!))
   nil|#
;;;;;

(defun FIND-YES (sentence)
  "Tests the sentence to see if it contains the yes word."
  (cond ((null sentence)())
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
#|(add-weighting '(what is my name?) '(is computer!))
((what (computer! 3.64) (is 9.4) (name? 10.18) (david! 0.96)
 (name 1.1) (my 1.5) (your 1.5) (hello! 0.4) (yes^ 0.1)) . . .|#
;;;;;

(defun ADD-WEIGHTING (sentence-1 sentence-2)
  "Sentence 1 here is the initiating sentence."
  (loop for word in sentence-1
        collect (cons word (add-weight word sentence-2))))

;;;;;
#|(add-weight 'what '(is computer!))
((computer! 3.64) (is 9.4) (name? 10.18) (david! 0.96) (name 1.1)
                         (my 1.5) (your 1.5) (hello! 0.4) (yes^ 0.1))|#
;;;;;

(defun ADD-WEIGHT (word sentence)
  "Increases the weight of each entry  in word for all of the words in sentence."
  (setf (associations (eval word))
        (reward (associations (eval word)) sentence)))

;;;;;
#|(reward '((name? 10.18) (is 4.7) (computer! 1.82) (david! 0.96) (name 1.1)
                      (my 1.5) (your 1.5) (hello! 0.4) (yes^ 0.1))
          '(is computer!))
((computer! 3.64) (is 9.4) (name? 10.18) (david! 0.96) (name 1.1)
                      (my 1.5) (your 1.5) (hello! 0.4) (yes^ 0.1))|#
;;;;;

(defun REWARD (associations words)
  "Reqeards the given associations."
  (if (null words) associations
      (let ((test (assoc (first words) associations)))
        (if test (reward (cons (list (first test) (round-it (* (second test) *weight-divisor*)))
                               (remove test associations)) 
                         (rest words))
            (reward associations (rest words))))))

;;;;;
#|(other-lexicon-type ;?) 
    #<cadence-lexicon #xFB8F25E>|#
;;;;;

(defun OTHER-LEXICON-TYPE (type)
  "Returns the opposite lexicon type."
  (if (equal type '?) *answer-cadence-lexicon* *question-cadence-lexicon*))

;;;;;
#|(punish '((name? 7.61) (is 3.7) (computer! 1.72) (david! 1.72) (name 1.0)
                      (my 1.0) (your 1.4) (hello! 0.3))
          '(david!))
   ((david! 0.86) (name? 7.61) (is 3.7) (computer! 1.72) (name 1.0)
                      (my 1.0) (your 1.4) (hello! 0.3))|#
;;;;;

(defun PUNISH (associations words)
  "Punishes the weights for a given word."
  (if (null words) associations
      (let ((test (assoc (first words) associations)))
        (if test (punish (cons (list (first test) (round-it (/ (second test) *weight-divisor*)))
                               (remove test associations)) 
                         (rest words))
            (punish associations (rest words))))))

;;;;;
#|(figure-speac 'hello!) 
    s|#
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
#|(associate)
  nil|#
;;;;;

(defun ASSOCIATE ()
  "This function runs the program from the menu."
  (if *initiate* (setq *all-words* nil))
  (progn (setq *no* ())(setq *yes* ())
         (loop until (equal (setq test (catch-cancel (get-string-from-user "" :ok-text "Send" :position #@(20 50))))
                            :cancel)
               do (setq *input* (make-string-into-list test))
               do (let* ((trial (put-sentence-into-database *input*))) ;;;this is where reply is!!
                    (if (not (null (read-from-string trial)))
                      (progn (let ((name (implode (list 'sentence- *counter*)))
                                   (sentence-type (my-last (explode (my-last *response*)))))
                               (set name (make-instance 'sentence 
                                           :name 'me
                                           :sentence-type sentence-type
                                           :sentence (list *response*)
                                           :length-of-sentence (length *response*)
                                           :origination 'Associate))
                               (pushnew name *sentences*)
                               (incf *counter*)))))
               do (if (not (null *response*))
                    (progn (new-text)
                           (message-dialog (make-list-into-string *response*) :title "Associate" :position #@(20 160)))
                    (progn (new-text)
                           (message-dialog " ------- " :title "Associate" :position #@(20 160))))
               do (gc))))

;;;;;
#|(new-text)
   ((hello! nil)) |#
;;;;;

(defun NEW-TEXT ()
  "Returns input plus weight."
  (setq *weight-list* 
        (let ((test (get-element-from-words 'associations)))
          (if test test nil)))
  (set-table-sequence *dialog-text* (reverse *weight-list*)))

;;;;;
#|(parse-the-sentence '(c c c c) '(huh? name? to?)) 
   (pretty what stupid huh?)|#
;;;;;

(defun PARSE-THE-SENTENCE (parse cadence)
  "The argument here is the parse found in any sentence parse slot."
  (append (let ((parse-lists (remove-cadences (loop for item in (get-associations (collect-parsings *sentences*)) collect (reverse item)))))
            (loop for element in (butlast parse)
                  collect (second (assoc element (mix parse-lists)))))
          (list (choose-one cadence))))

;;;;;
#|(collect-parsings '(sentence-3 sentence-2 sentence-1)) 
    ((c too) (c bad) (c you) (c cant) (c answer!) (c what)
     (c is) (c your) (c name?) (c who) (c am) (c i)
     (c speaking) (c to?)|#
;;;;;

(defun COLLECT-PARSINGS (sentences)
  "Returns parsed sentences."
  (pair (apply #'append (loop for sentence in sentences
                              collect (first (sentence (eval sentence)))))
        (apply #'append (loop for sentence in sentences
                              collect (parse-it (eval sentence))))))

;;;;;
#|(pair '(c c c c c c c c c c c c c c)
        '(too bad you cant answer! what is your name? who am i speaking to?)) 
   ((c too) (c bad) (c you) (c cant) (c answer!) (c what) (c is)
    (c your) (c name?) (c who) (c am) (c i) (c speaking) (c to?))|#
;;;;;

(defun PAIR (list-1 list-2)
  "Pairs its two args."
  (loop for item in list-1
        collect (list item (first list-2))
        do (setf list-2 (rest list-2))))

;;;;;
#|(mix '((c too) (c bad) (c you) (c cant) (c answer!) (c what) (c is) (c your)
        (c name?) (c who) (c am) (c i) (c speaking) (c to?))) 
       ((c is) (c bad) (c to?) (c too) (c i) (c answer!) (c speaking) (c who)
                (c am) (c your) (c what) (c name?) (c you) (c cant))|#
;;;;;

(defun MIX (list)
  "Arbitrarily pairs its list arg."
  (if (null list) nil
      (let ((choice (choose-one list)))
        (cons choice (mix (remove choice list :count 1))))))

;;;;;
#|(remove-cadences
            '((c too) (c bad) (c you) (c cant) (c answer!) (c what) (c is) (c your)
             (c name?) (c who) (c am) (c i) (c speaking) (c to?))) 
    ((c too) (c bad) (c you) (c cant) (c what) (c is) (c your)
     (c who) (c am) (c i) (c speaking))|#
;;;;;

(defun REMOVE-CADENCES (choices)
  "Removes the last element of choices."
  (cond ((null choices)())
        ((or (member (second (first choices)) (cadences *answer-cadence-lexicon*))
             (member (second (first choices)) (cadences *question-cadence-lexicon*)))
         (remove-cadences (rest choices)))
        (t (cons (first choices)(remove-cadences (rest choices))))))

;;;;;
#|(get-associations
           '((pretty c) (what c) (stupid c) (huh? c) (that c) (what c) (i c) (said! c)
             (huh? c) (sounds c) (pretty c) (stupid c) (to c) (me! c) (too c)...
   ((c sounds) (c stupid) (c said!) (c that) (c name?) (c is)
    (c pretty) (c to) (c me!) (c answer!) (nil to?)
    (nil speaking))|#
;;;;;

(defun GET-ASSOCIATIONS (parsed-words)
  "Retrieves associated words."
  (let ((relevant-words 
         (compound-associations 
          (apply #'append 
                 (loop for word in (first (sentence (eval (first *sentences*))))
                       collect (associations (eval word)))))))
    (relate-words relevant-words parsed-words)))

;;;;;
#|(relate-words '((sounds 0.8888888888888888) (stupid 0.5) (said! 7.5)
                  (that 0.5) (name? 10.0) (is 1.0) (pretty 0.5) (to 0.5)
                  (me! 0.1) (answer! 0.1) (to? 0.1) (speaking 0.1))
               '((pretty c) (what c) (stupid c) (huh? c) (that c) (what c)....
   ((sounds c) (stupid c) (said! c) (that c) (name? c) (is c)
    (pretty c) (to c) (me! c) (answer! c) (to? nil)
    (speaking nil))|#
;;;;;

(defun RELATE-WORDS (words associations)
  "Relates associated words."
  (cond ((null words)())
        ((let ((test (assoc (very-first words) associations)))
           (if test (cons test (relate-words (rest words) associations)))))
        (t (relate-words (rest words) associations))))

;;;;;
#|(get-parse-elements '(c c c c c))
     (c)
   (get-parse-elements '(c c s c c))
     (c s)|#
;;;;;

(defun GET-PARSE-ELEMENTS (parse)
  "Returns the parsed elements of arg."
  (if (null parse)()
      (cons (first parse)
            (get-parse-elements (remove (first parse) parse :test #'equal)))))

;;;;;
#|(get-element-from-words associations) 
   ((hello! nil))|#
;;;;;

(defun GET-ELEMENT-FROM-WORDS (type)
  "This is a test function for getting infor from words. Type can be
       predecessors successors keywords word-type positions-in-sentence associations usage music.
       weight is stored in associations."
  (let ((words (remove-duplicates *words*)))
    (mapcar #'(lambda (x)(list x (funcall type (eval x)))) words)))

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
                 (round (+ (* (length *sentences*) 50) 230))
                 (round (* (length *sentences*) 60)))))

(defMethod VIEW-DRAW-CONTENTS ((window SCROLLING-WINDOW-SCROLLER1))
  (call-next-method)
  (set-pen-pattern window *black-pattern*)
  (set-view-font window :bold)
  (set-view-font window 12)
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

;;;;;
#|(make-paint-oval
           #<scrolling-window-scroller1 #xFC3226E>
           '(390 30)
           'hello!) 
  #<A Mac Non-zone Pointer #xEC2123A>|#
;;;;;

(defun MAKE-PAINT-OVAL (window entry word)
  "Returns a pointer to the location of the oval."
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

;;;;;
#|(create-the-coordinates
           '((hello!) (what is your name?) (is your what hello!)
            (my name is david!) (name?) (your name is computer!) (name?)
            (what is your name?) (computer!) (what is my name?) (david!))) 
 ((390 30) (156 90) (312 90) (468 90) (624 90) . . .|#
;;;;;

(defun CREATE-THE-COORDINATES (sentences)
  "CCreates the coordinates for the output graph."
  (apply #'append
         (let* ((horizontal-constant 0)
                (horizontal-cumulative 10) ;horizontal-constant)
                (vertical-constant 60)
                (vertical-cumulative 30))
           (loop for sentence in sentences
                 do (setf horizontal-constant (round (/ (+ (* (length *sentences*) 50) 230)
                                                        (1+ (length sentence)))))
                 do (setf horizontal-cumulative horizontal-constant)
                 collect (loop for word in sentence
                               collect (list horizontal-cumulative vertical-cumulative)
                               do (setf horizontal-cumulative (+ horizontal-cumulative horizontal-constant)))
                 do (setf vertical-cumulative (+ vertical-constant vertical-cumulative))))))

;;;;;
#| (explode-list '(hi there)) 
    (h i t h e r e)|#
;;;;;

(defun EXPLODE-LIST (list)
  "Explodes the elements of the list arg."
  (apply #'append (loop for item in list
                        collect (explode item))))

;;;;;
#|(explode hello!)
   (h e l l o !)|#
;;;;;

(defun EXPLODE (atom)
  "Explodes its atom arg. into a list."
  (if (null atom) nil
      (let ((test (format nil "~a" (coerce (write-to-string atom)
                                           'list))))
        (read-from-string test))))

;;;;;
#|(implode '(sentence- 3)) 
     sentence-3|#
;;;;;

(defun IMPLODE (list)
  "Implodes its arglist."
  (if (null (rest list))(first list)
      (implode (cons (read-from-string 
                      (format nil "~a~a" (first list)(second list))) 
                     (nthcdr 2 list)))))

;;;;;
#|(my-last '(hello! 0.1)) 
   0.1|#
;;;;;

(defun MY-LAST (list)
  "Returns the first of the last."
  (first (last list)))

;;;;;
#| (my-sort #'> '(3 6)) 
      (6 3)|#
;;;;;

(defun MY-SORT (function lists)
  "Non-destructive sort."
  (loop for sorted-item in 
        (sort (loop for item in lists
                    collect (list item)) function :key #'car)
        collect (first sorted-item)))

;;;;;
#|(choose-one '(1 2 3 4))
    4|#
;;;;;

(defun CHOOSE-ONE (list)
  "Chooses a random member of its argument."
  (nth (random (length list) *rs*)
       list))

;;;;;
#|(make-list-into-string '(is your what hello!)) 
    "is your what hello!"|#
;;;;;

(defun MAKE-LIST-INTO-STRING (list)
  "Makes its list arg into a string."
  (let ((storage ""))
    (loop for element in (butlast list)
          do (setf storage (format () "~A~A~A" storage (write-to-string element) " ")))
    (format () "~A~A" storage (write-to-string (my-last list)))))

;;;;;
#|(make-string-into-list "hello!") 
      (hello!)|#
;;;;;

(defun MAKE-STRING-INTO-LIST (string)
  "RFeturns a list from string input."
  (read-from-string (remove-punctuation
                     (format () "~A~A~A" "(" string ")"))))

;;;;;
#|(REMOVE-PUNCTUATION "abc. efg# a'c")
        abc efg ac|#
;;;;;

(defun REMOVE-PUNCTUATION (string)
  "Removes basic punctuation from string."
  (remove "," 
          (remove "'" 
                  (remove "#" 
                          (remove "." string 
                                  :test #'string=) 
                          :test #'string=)
                  :test #'string=) 
          :test #'string=))

;;;;;
#|(firstn 2 '(a b c))
     (a b)|#
;;;;;

(defun FIRSTN (number list)
  "Returns the first number of its list."
  (if (< (length list) number)(firstn (1- number) list)
      (butlast list (- (length list) number))))

;;;;;
#| (very-first '((a)))
     a|#
;;;;;

(defun VERY-FIRST (list)
  "Equivalent to (first (first."
  (caar list))

;;;;;
#|(my-remove '(what) '(name? is)) 
    (name? is)|#
;;;;;

(defun MY-REMOVE (objects-to-be-removed list-of-objects)
  "Removes all of first arg in second arg."
  (if (null objects-to-be-removed) list-of-objects
      (my-remove (rest objects-to-be-removed)
                 (remove (first objects-to-be-removed)
                         list-of-objects))))

;;;;;
#|(sortcdr #'> '((bad 1.0) (you 1.0) (too 5.0) (cant 1.0))) 
    ((too 5.0) (bad 1.0) (you 1.0) (cant 1.0)|#
;;;;;

(defun SORTCDR (function lists)
  "Sorts by second element of each sublist."
  (let ((first-lists (sort (loop for item in lists
                                 collect (my-last item)) function)))
    (loop for item in first-lists
          collect (rassoc (list item) lists :test #'equal)
          do (setf first-lists (rest first-lists))
          do (setf lists (remove (rassoc (list item) lists :test #'equal) lists :test 'equal :count 1)))))

(defClass *HELP-MANUAL-SEQUENCE* (sequence-dialog-item)
  (sequence :initarg :sequence :initform *help*))

(defMethod INITIALIZE-INSTANCE ((sequence *HELP-MANUAL-SEQUENCE*) &rest initargs)
  (apply #'call-next-method sequence initargs)
  (set-table-sequence sequence *help*))


(defClass HELP-MANUAL-WINDOW (window)
  nil
  (:default-initargs :window-type :document
    :window-title "Help"
    :view-position #@(10 40)
    :view-size #@(460 380)))

(defMethod INITIALIZE-INSTANCE ((window HELP-MANUAL-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs)
  (add-subviews window 
                
                (make-dialog-item 
                 '*help-manual-sequence*
                 #@(2 2) #@(455 380)
                 ""
                 #'(lambda (item)
                     (declare (ignore item)) 
                     :selection-type :disjoint
                     :visible-dimensions #@(1 12)
                     :view-font '("times" 12 :bold)))))

;;;;;
#|(clear-memory)
    nil|#
;;;;;

(defun CLEAR-MEMORY ()
  "Clears the session from RAM."
  (loop for sentence in *sentences*
        do (makunbound sentence))
  (loop for word in *words*
        do (makunbound word))
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
#|(save-transcript)
   #1P"Macintosh HD:Applications (Mac OS 9):mcl:MCL 4.3.1:test"|#
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
  (setq *name-of-file* (choose-new-file-dialog))
  (buffer-write-file 
   *temporary-buffer* 
   (make-pathname :directory (mac-directory-namestring *name-of-file*)
                  :name (mac-file-namestring *name-of-file*)
                  :type :unspecific)
   :if-exists :overwrite))

;;;;;
#|(show-sentence-slots 'sentence-11)
"(setq sentence-11 (make-instance 'sentence :name . . ."|#
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
#|(show-word-slots 'computer!)
"(setq computer! (make-instance 'word :predecessors . . ."|#
;;;;;

(defun SHOW-WORD-SLOTS (object)
  "This function produces a readable and executable reinstatement of the argument's
      word status. The argument must be quoted."
  "This function produces a readable and executable reinstatement of the argument's
  object status. The argument must be quoted."
  (format () "~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~%"
          "(setq " object
          " (make-instance 'word :predecessors '" (write-to-string (predecessors (eval object)))
          " :successors '" (write-to-string (successors (eval object)))
          " :keywords '" (write-to-string (keywords (eval object)))
          " :word-type '" (write-to-string (word-type (eval object)))
          " :positions-in-sentence '" (write-to-string (positions-in-sentence (eval object)))
          " :usage '" (write-to-string (usage (eval object)))
          " :associations '" (write-to-string (associations (eval object)))
          "))"))

;;;;;
#|(show-transcript)
   #<transcript-window "Transcript" #xFFC834E>|#
;;;;;

(defun SHOW-TRANSCRIPT ()
  "Creates a transcript window for the menu which prints out the current conversation."
  (setq *transcript* (append '("Associate output preceded by >.""""")
                             (let ((sentences (reverse (loop for sentence in *sentences*
                                                             collect 
                                                             (if (equal (origination (eval sentence)) 'Associate)
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
    :view-position #@(10 40)
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

(defClass ABOUT-WINDOW (window)
  nil
  (:default-initargs :window-type :document
    :window-title ""
    :view-position #@(10 40)
    :view-size #@(300 180)
    :close-box-p ()
    :grow-icon-p ()))

(defMethod INITIALIZE-INSTANCE ((window ABOUT-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs)
  (add-subviews window 
    (setq *title* (make-instance 
                    'static-text-dialog-item
                    :view-font '("times" 26 :bold)
                    :view-position #@(98 12)
                    :dialog-item-text
                    "Associate"))
    (setq *creator* (make-instance 
                      'static-text-dialog-item
                      :view-font '("athens" 10)
                      :view-position #@(92 44)
                      :dialog-item-text "Created by David Cope"))
    (setq *email* (make-instance 
                    'static-text-dialog-item
                    :view-font '("athens" 10)
                    :view-position #@(92 60)
                    :dialog-item-text "howell@cats.ucsc.edu"))
    (setq *visual* (make-instance 
                     'static-text-dialog-item
                     :view-font '("athens" 14)
                     :view-position #@(70 82)
                     :dialog-item-text
                     "Language Recognition"))
    (setq *software* (make-instance 
                       'static-text-dialog-item
                       :view-font '("athens" 14)
                       :view-position #@(115 104)
                       :dialog-item-text
                       "Software"))
    (setq *copyright* (make-instance 
                        'static-text-dialog-item
                        :view-font '("geneva" 10)
                        :view-position #@(102 124)
                        :dialog-item-text "©David Cope 1996"))
    (make-dialog-item 
     'button-dialog-item
     #@(75 138) #@(150 32)
     "Okay"
     #'(lambda (item) 
         item (window-close window))
     :view-font '("athens" 14))))

(apply #'remove-menu-items *apple-menu* 
       (list (first (menu-items *apple-menu*))(second (menu-items *apple-menu*))))

(add-menu-items *apple-menu* 
                (make-instance 'menu-item 
                  :menu-item-title "About Associate..." 
                  :menu-item-action 
                  #'(lambda nil(make-instance 'about-window)))
                (make-instance 'menu-item 
                  :menu-item-title "-" 
                  :menu-item-action 
                  #'(lambda nil)))

(setq *question-incipient-lexicon* (make-instance 'incipient-lexicon))
(setq *answer-incipient-lexicon* (make-instance 'incipient-lexicon))
(setq *question-cadence-lexicon* (make-instance 'cadence-lexicon))
(setq *answer-cadence-lexicon* (make-instance 'cadence-lexicon))

;;;;;
#|(assign-durations)
    ((49 125))|#
;;;;;

(defun ASSIGN-DURATIONS ()
  "Assigns random durations to notes in this simple form of sound Associate."
  (setq *rs* (make-random-state t))
  (loop for note in '(48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71)
        collect (list note (choose-one '(250 125 500 750 1000)))))

(set-menubar (list (first my-menu)
                   (second my-menu)
                   (third my-menu)
                   (fourth my-menu)
                   (fifth my-menu)
                   (make-instance 'menu :menu-title "Associate"
                                  :menu-items 
                                  (list 
                                   (make-instance 
                                     'menu-item :menu-item-title "Initiate Session"
                                     :command-key #\1
                                     :menu-item-action 
                                     #'(lambda nil 
                                         (if *transcript-window* (window-close *transcript-window*))
                                         (clear-memory)
                                         (setq *initiate* t)
                                         (setq *rhythm* (assign-durations))
                                         ;(if *sound* (sound-Associate)
                                         (Associate)))
                                   ;)
                                   (make-instance 
                                     'menu-item :menu-item-title "Continue Session"
                                     :command-key #\2
                                     :menu-item-action 
                                     #'(lambda nil 
                                         (setq *initiate* nil)
                                         (if *transcript-window* (window-close *transcript-window*))
                                         ;(if *sound* (sound-Associate)
                                         (Associate)))
                                   ;)
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
                                         (load (choose-file-dialog))))
                                   #|(make-instance 
                                     'menu-item :menu-item-title "Sound"
                                     :command-key #\7
                                     :menu-item-action 
                                     #'(lambda nil 
                                         (let ((test (catch-cancel (y-or-n-dialog "
Note that you will need to have QuickTime 
Instruments installed for the sound to work 
properly." :cancel-text "Select"))))
                                           (cond ((equal test t)(setq *sound* t))
                                                 ((equal test ':cancel)
                                                  (progn (if *play-window* (window-close *play-window*))
                                                         (setq *play-window* (make-instance 'play-window))))
                                                 (t (setq *sound* ()))))))|#
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
                                                 :view-position #@(10 50)
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


                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;        Apprentice/Chapter 11        ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;    simple code to run Apprentice    ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

#|
The standard objects for Apprentice., music, words or otherwise."
|#

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
   (name :initarg :name :initform nil :accessor name)
   (timing :initarg :timing :initform nil :accessor timing)
   (destination :initarg :destination :initform nil :accessor destination)
   (events :initarg :events :initform nil :accessor events)
   (lexicon :initarg :lexicon :initform nil :accessor lexicon)
   (used-before? :initarg :used-before? :initform nil :accessor used-before?))
  (:documentation "this is the top-level object for words in a database file."))

(defClass INCIPIENT-LEXICON (word)
  ((incipients :initarg :incipients :initform nil :accessor incipients))
  (:documentation "this is the top-level object for incipient lexicons in a database file."))

(defClass CADENCE-LEXICON (word)
  ((cadences :initarg :cadences :initform nil :accessor cadences))
  (:documentation "this is the top-level object for cadence lexicons in a database file."))


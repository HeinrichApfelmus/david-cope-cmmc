
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;        Apprentice/Chapter 11        ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;     simple code to run Improvise    ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;


;========================
;some global and general variable declarations
;========================

(defVar *OUTPUT* ())
(defVar *STORAGE* ())
(defVar *INPUT* "")
(defVar *PREDECESSOR* ())
(defVar *SUCCESSOR* ())
(defVar *KEYWORD* ())
(defVar *SENTENCES* ())
(defVar *WORDS* ())
(defVar *COUNTER* 1)
(defVar *QUESTION-INCIPIENT-LEXICON* ())
(defVar *ANSWER-INCIPIENT-LEXICON* ())
(defVar *QUESTION-CADENCE-LEXICON* ())
(defVar *ANSWER-CADENCE-LEXICON* ())
(defVar *RESPONSE* ())
(defVar *NO* ())
(defVar *NO-SENTENCES* ())
(defVar WORD ())
(defVar *END* ())
(defVar *HELP* '("""""                   Apprentice" " "
"GENERAL"
""
"Apprentice is designed to accept language (any) and music"
"as input and slowly, through exchange with users, develop an "
"ability to converse in that language. The rules are simple. "
"All statements and questions should be concluded with an exclamation "
"point (!) or question mark (?) respectively. "
"The word for no - to decrese the strength of nodes - ends with "
"a star (*). The word for no - to increase the strength of nodes - "
"ends with a caret (^)."
"Apprentice uses a limited association net. Association nets "
"can provide useful connectivity for programs such "
"as Apprentice. Apprentice cannot respond until both "
"questions and statements have been entered. "
""
"Apprentice attempts to define words by tracing connections to "
"various associations so that individual word meanings become "
"heightened. Many other aspects of the words or nodes can also "
"benefit from such associations. Parsing becomes clearer as words such "
"as Who and What can coalesce into a single representation. "
""
"Association nets resemble neural nets. However, there are a "
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
"In Apprentice, the association net is a natural outgrowth of the "
"slots present in each word object. Examining these slots will show "
"how connections and access continues to expand as the association "
"net grows. As well, since word objects in Apprentice are a subclass "
"of sentence objects, word objects have access to the sentence objects "
"slots. Therefore, the nodes shown in the associations window "
"have more information than shown. For example, each word object has "
"access to the length and content of each preceding sentence. This "
"information can provide important contextualization for the various "
"instances of associated words and lead to further implicated "
"information relating to word meanings, parsing representations, and other" 
"critical factors in determining logical machine responses to user input."
""
""
"Send all questions and comments about Apprentice to:"
"David Cope"
"howell@ucsc.edu"
""
"                                    - David Cope"
))

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
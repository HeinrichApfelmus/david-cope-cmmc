
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;     Chorale Function/Chapter 4      ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;           platform dependent        ;;;;;
                   ;;;;;          code to run chorale        ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

;;;this loads the data

(setq *minimum-stack-overflow-size* 3048576)
(defVar *COMPOSER* 'bach)
(defVar *RULES-STORAGE* ())
(defVar *MIX-NAMES* ())
(defVar *MIX* ())
(defVar *LEXICONS* ())
(defVar BACH-DOMINANTS-TONICS ())
(defVar BACH-START-BEATS ())
(defVar BACH-DOMINANTS ())
(defVar BACH-TONICS ())
(defVar BACH-COMPOSE-BEATS ())
(defVar BACH-RULES ())
(defVar *END* ())
(defVar *HISTORY* ())
(defVar *EVENTS* ())
(defVar *TONIC* 'major)
(defVar *EARLY-EXIT?* ())
(defVar *END* ())
(defVar *COMPOSE-NUMBER* 0)
(defVar *HISTORIES* ())
(defVar *PREVIOUS-BEAT* ())
(defVar *SAVE-EVENTS* ())
(defVar *BEAT-SIZE* 1000)
(defVar DAVIDCOPE ())
(defVar COPE-TEXT ())
(defVar DAVIDCOPE1 ())

(defClass ALL-ABOUT-WINDOW (window)
  nil
  (:default-initargs :window-type :double-edge-box 
    :window-title "Chorale"
    :view-position #@(40 40)
    :view-size #@(300 180)
    :close-box-p ()
    :auto-position :centermainscreen))

(defMethod INITIALIZE-INSTANCE ((window ALL-ABOUT-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs)
  (add-subviews window 
    (setq davidcope (make-instance 'static-text-dialog-item
                      :dialog-item-text "Chorale"
                      :view-position '#@(77 26)
                      :view-size #@(322 54)
                      :view-font '("times" 46)))
    (setq davidcope (make-instance 'static-text-dialog-item
                      :dialog-item-text "Please be patient."
                      :view-position '#@(97 76)
                      :view-size #@(322 54)
                      :view-font '("times" 16)))
    (setq davidcope1 (make-instance 'static-text-dialog-item
                       :dialog-item-text "Lots of data must be loaded."
                       :view-position '#@(62 96)
                       :view-size #@(322 54)
                       :view-font '("times" 16)))
    (setq Cope-text (make-instance 'static-text-dialog-item
                      :dialog-item-text "© David Cope 1992 -"
                      :view-position '#@(80 140)
                      :view-size #@(334 54)
                      :view-font '("times" 16 :bold))))
  (set-part-color davidcope :text *red-color*)
  (set-part-color davidcope1 :text *red-color*)
  (set-part-color Cope-text :text *dark-green-color*))

(defVar *ABOUT-WINDOW* (make-instance 'ALL-ABOUT-WINDOW))

(setq bach '(bach-compose-beats bach-start-beats bach-rules))

(setq *beats* 4)

(setq bach-form '(?))

(load (mac-namestring "ccl:chorale-chapter-4;data;jsb1.lisp"))
(load (mac-namestring "ccl:chorale-chapter-4;data;jsb2.lisp"))
(load (mac-namestring "ccl:chorale-chapter-4;data;jsb3.lisp"))
(load (mac-namestring "ccl:chorale-chapter-4;data;jsb4.lisp"))
(load (mac-namestring "ccl:chorale-chapter-4;data;jsb5.lisp"))
(load (mac-namestring "ccl:chorale-chapter-4;data;jsb6.lisp"))
(load (mac-namestring "ccl:chorale-chapter-4;data;jsb7.lisp"))
(load (mac-namestring "ccl:chorale-chapter-4;data;jsb8.lisp"))
(load (mac-namestring "ccl:chorale-chapter-4;data;jsb9.lisp"))
(load (mac-namestring "ccl:chorale-chapter-4;data;jsb10.lisp"))
(load (mac-namestring "ccl:chorale-chapter-4;data;jsb11.lisp"))
(load (mac-namestring "ccl:chorale-chapter-4;data;jsb12.lisp"))
(load (mac-namestring "ccl:chorale-chapter-4;data;jsb13.lisp"))

(defun CREATE-COMPLETE-DATABASE (db-names &optional (counter 1))
  "Loads and makes proper objects out of the db-names arg."
  (if (null db-names) t
      (progn (let ((beats (remove-nils (collect-beats (set-to-zero 
                                                       (sortcar #'< (eval (first db-names)))))))
                   (name ())
                   (start t))
               (loop until (null beats)
                     do (setq name (make-name (first db-names) counter))
                     do 
                     
                     (let ((start-notes (get-onset-notes (first beats)))
                           (destination-notes (get-onset-notes (second beats)))
                           (events (first beats)))
                       (set name
                            (make-instance 'beat-it :start-notes start-notes
                                           :destination-notes destination-notes
                                           :events events
                                           :voice-leading (first 
                                                           (my-push (cons (get-rules start-notes destination-notes name)
                                                                          (list name (very-first (sortcar #'< events))))
                                                                    (concat *composer* '- 'rules)))
                                           :speac ())))
                     do (setf counter (1+ counter))
                     do (setf beats (rest beats))
                     do (put-beat-into-lexicon name)
                     do (my-push name (concat *composer* '- 'compose-beats))
                     do (if start (my-push name (concat *composer* '- 'start-beats)))
                     do (setf start nil)))
             (create-complete-database (rest db-names)))))

;;;;;
#| Calling (set-to-zero ((31000 60 1000 4 96) (31000 67 1000 3 96) (31000 72 1000 2 96) (31000 76 1000 1 96))) 
 set-to-zero returned ((0 60 1000 4 96) (0 67 1000 3 96) (0 72 1000 2 96) (0 76 1000 1 96))|#
;;;;;

(defun SET-TO-ZERO (events &optional (subtract (very-first events)))
  "Sets the events to zero."
  (if (null events)()
      (cons (cons (- (very-first events) subtract)
                  (rest (first events)))
            (set-to-zero (rest events) subtract))))

;;;;;
#| Calling (thousandp 5000) 
 thousandp returned t|#
;;;;;

(defun THOUSANDP (number)
  "Returns the number under 1000."
  (if (zerop (mod number 1000)) t))

;;;;;
#| Calling (GET-RULES (57 60 69 76) (59 62 67 79) B206B-1) 
 GET-RULES returned ((3 2 2 B206B-1) (12 2 -2 B206B-1) (7 2 3 B206B-1) (9 2 -2 B206B-1) (4 2 3 B206B-1) (7 -2 3 B206B-1))|#
;;;;;

(defun GET-RULES (start-notes destination-notes name)
  "Gets the intervals between adjacent sets of the two args."
  (setq *rules-storage* ())
  (let ((test (make-lists-equal (list start-notes destination-notes))))
    (get-rules1 (first test)(second test) name)))

;;;;;
#|  Calling (GET-RULES1 (57 60 69 76) (59 62 67 79) B206B-1) 
  GET-RULES1 returned ((3 2 2 B206B-1) (12 2 -2 B206B-1) (7 2 3 B206B-1) (9 2 -2 B206B-1) (4 2 3 B206B-1) (7 -2 3 B206B-1))|#
;;;;;

(defun GET-RULES1 (start-notes destination-notes name)
  "Does the grunt work for get-rules."
  (if (or (null (rest start-notes))(null (rest destination-notes)))
    (reverse *rules-storage*)
    (progn
      (setq *rules-storage* (append (reverse (get-rule (- (first destination-notes) (first start-notes)) 
                                                       (first start-notes) start-notes destination-notes name)) *rules-storage*))
      (get-rules1 (rest start-notes) (rest destination-notes) name))))

;;;;;
#|   Calling (GET-RULE 2 57 (57 60 69 76) (59 62 67 79) B206B-1) 
   GET-RULE returned ((7 -2 3 B206B-1))|#
;;;;;

(defun GET-RULE (voice start-note start-notes destination-notes name)
  "Gets the rule between first two args."
  (if (or (null (rest start-notes))(null destination-notes))()
      (cons (list (reduce-interval (- (second start-notes) start-note))
                  voice
                  (- (second destination-notes) (second start-notes))
                  name)
            (get-rule voice start-note (rest start-notes)(rest destination-notes) name))))

;;;;;
#|Calling (REDUCE-INTERVAL 3) 
    REDUCE-INTERVAL returned 3|#
;;;;;

(defun REDUCE-INTERVAL (interval)
  "Reduces the interval mod 12."
  (cond ((<= (abs interval) 12) interval)
        ((minusp interval)(reduce-interval (+ interval 12)))
        (t (reduce-interval (- interval 12)))))

;;;;;
#|  Calling (MAKE-LISTS-EQUAL ((57 60 69 76) (59 62 67 79))) 
  MAKE-LISTS-EQUAL returned ((57 60 69 76) (59 62 67 79))|#
;;;;;

(defun MAKE-LISTS-EQUAL (lists)
  "Ensures the two lists are equal in length."
  (cond ((> (length (first lists))(length (second lists)))
         (list (firstn (length (second lists)) (first lists))(second lists)))
        ((> (length (second lists))(length (first lists)))
         (list (first lists)(firstn (length (first lists)) (second lists))))
        (t lists)))

;;;;;
#|  Calling (MAKE-LEXICON-NAME (57 60 69 76)) 
  MAKE-LEXICON-NAME returned BACH-57-60-69-76|#
;;;;;

(defun MAKE-LEXICON-NAME (note-numbers &optional (names *mix-names*))
  "Creates the appropriate lexicon name for the object."
  (cond ((null *mix*)(implode (cons *composer* (cons '- (hyphenate note-numbers)))))
        ((null names)(implode (cons *composer* (cons '- (hyphenate note-numbers)))))
        ((boundp (implode (cons (first names) (cons '- (hyphenate note-numbers)))))
         (implode (cons (first names) (cons '- (hyphenate note-numbers)))))
        (t (make-lexicon-name note-numbers (mix (rest names))))))

;;;;;
#| Calling (MAKE-NAME B206B 1) 
 MAKE-NAME returned B206B-1|#
;;;;;

(defun MAKE-NAME (db-name counter)
  "Simple synonym for imploding the dtabase name and number."
  (implode (cons db-name  (list '- counter))))

;;;;;
#| Calling (HYPHENATE (4 7 1)) 
 HYPHENATE returned (4 - 7 - 1)|#
;;;;;

(defun HYPHENATE (note-numbers)
  "Hyphenates the numbers in its arg."
  (if (null note-numbers)()
      (append (if (null (rest note-numbers))
                (list (first note-numbers))
                (list (first note-numbers) '-))
              (hyphenate (rest note-numbers)))))

;;;;;
#| Calling (GET-ONSET-NOTES ((0 57 1000 4 96) (0 60 1000 3 96) (0 69 1000 2 96) (0 76 1000 1 96))) 
 GET-ONSET-NOTES returned (57 60 69 76)|#
;;;;;

(defun GET-ONSET-NOTES (events)
  "Gets the onset pitches for its arg."
  (let ((onbeat (very-first events)))
    (loop for event in events
          if (equal (first event) onbeat)
          collect (second event))))

;;;;;
#| Calling (PUT-BEAT-INTO-LEXICON B206B-1) 
 PUT-BEAT-INTO-LEXICON returned (BACH-57-60-69-76)|#
;;;;;

(defun PUT-BEAT-INTO-LEXICON (beat-name)
  "Puts the beat arg into the appropriate lexicon."
  (let ((lexicon-name (make-lexicon-name (start-notes (eval beat-name)))))
    (if (and (exist-lexicon lexicon-name)
             (not (member beat-name (beats (eval lexicon-name)) :test #'equal)))
      (setf (beats (eval lexicon-name)) (cons beat-name (beats (eval lexicon-name))))
      (progn (set lexicon-name
                  (make-instance 'lexicon :beats (list beat-name)))
             (pushnew lexicon-name *lexicons*)))))

;;;;;
#|  Calling (MAKE-LEXICON-NAME (57 60 69 76)) 
  EXIST-LEXICON returned NIL|#
;;;;;

(defun EXIST-LEXICON (lexicon-name)
  "Sees if the lexicon exists."
  (boundp lexicon-name))


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

;;;;;;;
#|  Calling (COLLECT-BEATS ((0 57 1000 4 96) (0 60 1000 3 96) . . .
 COLLECT-BEATS returned (((0 57 1000 4 96) (0 60 1000 3 96)  . . .|#
;;;;;;;

(defun COLLECT-BEATS (events)
  "(collect-beats (sortcar #'< b4003))"
  (if (null events)()
      (let* ((test (collect-by-timing (first-place-where-all-together events) events))
             (reduced-test (nthcdr (length test) events)))
        (cons test 
              (collect-beats reduced-test)))))

;;;;;
#| Calling (RETURN-BEAT ((0 45 1000 4 96) (1000 57 500 4 96))) 
 RETURN-BEAT returned 1|#
;;;;;

(defun RETURN-BEAT (channel-events &optional (start-time (very-first channel-events)))
  "Returns the beat number of the initiating event."
  (cond ((null channel-events) nil)
        ((and (thousandp (very-first channel-events))
              (not (equal start-time (very-first channel-events))))
         (/ (- (very-first channel-events) start-time) 1000))
        (t (return-beat (rest channel-events)
                        start-time))))

;;;;;
#|  Calling (COLLECT-BY-TIMING 1000 ((0 57 1000 4 96) (0 60 1000 3 96) . . .
  COLLECT-BY-TIMING returned ((0 57 1000 4 96) (0 60 1000 3 96) (0 69 1000 2 96) (0 76 1000 1 96))|#
;;;;;

(defun COLLECT-BY-TIMING (timing events)
  "Collects the events accoring to timing."
  (cond ((null events)())
        ((<= (+ (first (first events))(fourth (first events))) timing)
         (cons (first events)
               (collect-by-timing timing (rest events))))
        (t (collect-by-timing timing (rest events)))))

;;;;;
#|  Calling (FIRST-PLACE-WHERE-ALL-TOGETHER ((0 57 1000 4 96) (0 60 1000 3 96) (0 69 1000 2 96) . . .
FIRST-PLACE-WHERE-ALL-TOGETHER returned 1000|#
;;;;;

(defun FIRST-PLACE-WHERE-ALL-TOGETHER (events)
  "This looks ahead to get the first time they end together"
  (let* ((test (plot-timings events))
         
         (channels (get-channel-numbers-from-events events))
         
         (ordered-timings-by-channel (loop for channel in channels 
                                           collect (collect-timings-by-channel test channel))))
    
    (all-together (first ordered-timings-by-channel)
                  (rest ordered-timings-by-channel))))

;;;;;
#|   Calling (ALL-TOGETHER ((1 1000) (1 2000) (1 2500) (1 3000) . . .
   ALL-TOGETHER returned 1000|#
;;;;;

(defun ALL-TOGETHER (channel channels)
  "Returns the appropriate channel timing."
  (cond ((null channel) (second (my-last (my-last channels)))) ;;; here is our remaining problem!!!!!
        ((find-alignment-in-all-channels (second (first channel)) channels))
        (t (all-together (rest channel) channels))))

;;;;;
#|    Calling (FIND-ALIGNMENT-IN-ALL-CHANNELS 1000 (((2 1000) (2 2000) (2 2500) . . .
FIND-ALIGNMENT-IN-ALL-CHANNELS returned 1000|#
;;;;;

(defun FIND-ALIGNMENT-IN-ALL-CHANNELS (point channels)
  "run this on the channels of the channel-point-lists"
  (cond ((null channels) point)
        ((null point) point)
        ((find-alignment point (first channels))
         (find-alignment-in-all-channels point (rest channels)))
        (t ())))

;;;;;
#|     Calling (FIND-ALIGNMENT 1000 ((2 1000) (2 2000) (2 2500) (2 3000) (2 3500) (2 4000) (2 4 . . .
     FIND-ALIGNMENT returned T|#
;;;;;

(defun FIND-ALIGNMENT (point channel)
  "? (find-alignment 1000 '((4 1000) (4 1000) (4 5000)))
   t this finds the timing point in the channel"
  (cond ((null channel)())
        ((and (thousandp point)
              (assoc point (mapcar #'reverse channel) :test #'equal))
         t)
        (t (find-alignment point (rest channel)))))

;;;;;
#|   Calling (GET-CHANNEL-NUMBERS-FROM-EVENTS ((0 57 1000 4 96) (0 60 1000 3 96) . . .
GET-CHANNEL-NUMBERS-FROM-EVENTS returned (1 2 3 4)|#
;;;;;

(defun GET-CHANNEL-NUMBERS-FROM-EVENTS (events &optional (channels ()))
  "simply gets the channel numbers from the music"
  (cond ((null events) channels)
        ((not (member (fourth (first events)) channels :test #'equal))
         (get-channel-numbers-from-events (rest events) (cons (fourth (first events)) channels)))
        (t (get-channel-numbers-from-events (rest events) channels))))

;;;;;
#|Calling (COLLECT-TIMINGS-BY-CHANNEL ((4 1000) (3 1000) (2 1000)  . . .
   COLLECT-TIMINGS-BY-CHANNEL returned ((1 1000) (1 2000) (1 2500) (1 3000) (1 3500) (1 4000) . . .|#
;;;;;

(defun COLLECT-TIMINGS-BY-CHANNEL (timings channel)
  "collects the timings of the channel indicated in second arg"
  (cond ((null timings)())
        ((equal (very-first timings) channel)
         (cons (first timings)
               (collect-timings-by-channel (rest timings) channel)))
        (t (collect-timings-by-channel (rest timings) channel))))

;;;;;
#| Calling (PLOT-TIMINGS ((0 57 1000 4 96) (0 60 1000 3 96)  . . .
PLOT-TIMINGS returned ((4 1000) (3 1000) (2 1000) (1 1000) . . .|#
;;;;;

(defun PLOT-TIMINGS (events)
  "Plots out the times of each beat."
  (if (null events)()
      (cons (list (fourth (first events))(+ (very-first events)(third (first events))))
            (plot-timings (rest events)))))

;;;;;
#| Calling (CREATE-PITCH-CLASS-SET (64 67 71)) 
 CREATE-PITCH-CLASS-SET returned (4 7 11)|#
;;;;;

(defun CREATE-PITCH-CLASS-SET (pitches)
  "Sorts and gets a full pc-set."
  (my-sort #'< (remove-duplicates (create-pc-set pitches))))

;;;;;
#|  Calling (CREATE-PC-SET (64 67 71)) 
  CREATE-PC-SET returned (4 7 11)|#
;;;;;

(defun CREATE-PC-SET (pitches)
  "Creates a full PC set."
  (if (null pitches) ()
      (cons (mod (first pitches) 12)
            (create-pc-set (rest pitches)))))

;;;;;;;
;objects
;;;;;;;

(defClass BEAT-IT ()
  ((events :initarg :events :initform () :accessor events)
   (start-notes :initarg :start-notes :initform () :accessor start-notes)
   (start-note :initarg :start-note :initform () :accessor start-note)
   (start-set :initarg :start-set :initform () :accessor start-set)
   (destination-notes :initarg :destination-notes :initform () :accessor destination-notes)
   (voice-leading :initarg :voice-leading :initform () :accessor voice-leading)
   (pre-destination-notes :initarg :pre-destination-notes :initform () :accessor pre-destination-notes)
   (texture :initarg :texture :initform () :accessor texture)
   (speac :initarg :speac :initform () :accessor speac)
   (beat :initarg :beat :initform () :accessor beat)
   (length-to-cadence :initarg :length-to-cadence :initform () :accessor length-to-cadence)))

(defClass LEXICON ()
  ((beats :initarg :beats :initform () :accessor beats)))

;;;;;
#| Calling (MY-PUSH 1 TEST-IT) 
 MY-PUSH returned (1 1)|#
;;;;;

(defun MY-PUSH (stuff place-name)
  "A simple synonym for push."
  (set place-name (cons stuff (eval place-name))))

(defVar BACH-CHORALES-IN-DATABASES 
;;;1
'(b206b b306b b408b b507b b606b
b707b b907b b107b b1306b b1605b b1805b b2007b ;b2011b
b2406bs b2506 b2806b b3006b b3206b b3604 b3706 b3907 
b4003 b4006 b4008 b4311 bnotsure

;;;2
b4407b b4606b b4705b b4803b
b4807b b5505b b5708b b6005b b6206b b6402 b6408b ;b6502b 
b6507b b6606b


b6707b 
;b7007b 
b7011b b7305b b7408b 

;;;3
b7706b b7807b b8008b b8107b b8305b b8405b b8506b b8606b b8707
b8807b b10207b b10306b b8906b b9005b b9106b b9209b b9307b b9408b b9606b
b9906b b10406b b10806b b11007b b11106b b11300b b11407b b11606b b11909b
b12006b ;b12206b 

;;;4
b12206b b12506b b12606b b12705b b13306b b13506b b13906b b15105 
b15301b b15305b b15309b b15403b b15408b b15505b b15606b b15705b b15804b
b15206b b16406b 

;;;5
b16506b b16606b b16806n b16907b b17405b b17606b b17807b b18007b b18305b 
b18400b b18707b b18806b b19406b b19412b b19705b b19707b b19701b b22602b 
b22701b b22707b 

;;;6
b22711b b22902b b24403b 24410b b24415b b24417b b24425b b24432b b24434b 
b24440b b2444b b24446b b24454b b24462b b24511b b24515b b24517b b24522b 
b24526b b24527b b24537b b24530b b24812b b24828b b24833b b24846b b24853b 
b24859b b25200b b25300b 

;;;7
b25400b b25500b b25600b b25700b b25800b b25900b b26000b b26100b b26200b 
b26300b b26400b b26500b b26600b b26700b b26800b b26900b b27000b b27100b 
b27200b b27300b b27400b b27500b b27600b b27700b b27800b b27900b b28000b 
b28100b b28300b b28400b 

;;;8
b28500b b28600b b28700b b28800b b28900b b29000b b29100b b29200b b29300b
b29400b b29600b b29700b b29800b b29900b b30000b b30100b b30200b b30300b
b30400b b30500b 

;;;9
b30600b b30700b b30800b b30900b b31000b b31100b b31200b b31300b b31400b
b31500b b31600b b31700b b31800b b31900b b32100b b32200b b32800b b32900b
b3300b b33100b b33200b b33300b b33400b b33500b b33600b b33700b b33800b
b33900b b34000b b34100b 

;;;10
b34200b b34500b b34600b b34700b b34800b b34900b b35000b b35100b b35200b
b35300b b35400b b35500b b35600b b35700b b35800b b35900b b36000b b36100b
b36200b b36300b 

;;;11
b36400b b36500b b36600b b36700b b36800b b36900b b37000b b37100b b37200b
b37300b b37400b b37500b b37600b b37700b b37800n b37900b b38000b b38100b
b38200b b38300b b38400b b38500b b38600b b38700b b38800b b38900b b39000b
b39100b b39200b b39300b 

;;;12
b39400b b39500b b39600b b39700b b39800b b39900b b40000b b40100b b40200b
b40300b b40400b b40500b b40600b b40700b b40800b b40900b b41000b b41100b
b41200b b41300b b41400b b41500b b42600b b41700b b41800b b41900b b42000b 
b42100b b42200b b42300b 

;;;13
b42400b b42500b b42600bb b42700b b42800b b42900b b43000b b43100b b43200b
b43300b b43400b b43500b b43600b 
;b43700b 
b43800b 
))

(create-complete-database bach-chorales-in-databases)

;;;;;
#|? (COMPOSE-BACH)
T|#
;;;;;

(defun COMPOSE-BACH ()
  "The top-level compose function."
  (compose-b)
  (if (or (null *events*)
          (< (let ((it (my-last (sortcar #'< *events*))))
               (+ (first it)(third it)))
             15000)
          (> (let ((it (my-last (sortcar #'< *events*))))
               (+ (first it)(third it)))
             200000)
          (not (wait-for-cadence *events*))
          (check-for-parallel *events*)
          (null *end*))
    (compose-bach) 
    (progn (setq *save-events* *events*)(setq *events* (ensure-necessary-cadences (sortcar #'< *events*)))
           (if (not (check-mt (get-on-beat *events* (very-first *events*))))
             (setq *events* (delay-for-upbeat *events*)))
           (if (and (null *early-exit?*)(equal *composer* 'bach))
             (setq *events* 
                   (cadence-collapse (transpose-to-bach-range *events*)))(setq *events* ()))
           t)))

;;;;;
#| Calling (COMPOSE-B) 
 COMPOSE-B returned ((1 (B1605B-36 B6402-31 B3006B-21 . . .|#
;;;;;

(defun COMPOSE-B (&optional (counter 0))
  "The real horse for compose-bach."
  (setq *end* ())
  (setq *history* ())
  (setq *events* 
        (let ((current-beat 
               (find-triad-beginning)))
          (if (match-tonic-minor (firstn 4 (events (eval current-beat))))
            (setq *tonic* 'minor)(setq *tonic* 'major))
          (apply #'append 
                 (re-time
                  (append
                   (loop until (or (setq *early-exit?* (null (destination-notes (eval current-beat))))
                                   (if (and (> counter 36)
                                            (if (equal *tonic* 'minor)
                                              (and (> (find-events-duration (events (eval current-beat))) *beat-size*)
                                                   (match-tonic-minor (events (eval current-beat))))
                                              (and (> (find-events-duration (events (eval current-beat))) *beat-size*)
                                                   (match-bach-tonic (events (eval current-beat))))))
                                     (progn (setq *end* t) t)))
                         do (push current-beat *history*)
                         collect (events (eval current-beat))
                         do (setq *previous-beat* current-beat)
                         do (setq current-beat 
                                  (choose-one 
                                   (let ((beat-choices (beats (eval (make-lexicon-name (destination-notes (eval current-beat)))))))
                                     (if (null (rest beat-choices)) beat-choices (my-remove (list *previous-beat* (incf-beat *previous-beat*)) beat-choices)))))
                         do (incf counter))
                   (progn (push current-beat *history*)
                          (list (events 
                                 (eval current-beat)))))))))
  (if (and (null *early-exit?*)(equal *composer* 'bach))
    ;(setq *events* (transpose-to-bach-range *events*))
    *events* (setq *events* ()))
  (setq *history* (reverse *history*))
  (if *end* (push (list (1+ *compose-number*) *history*) *histories*)))

;;;;;
#| Calling (find-triad-beginning) 
 find-triad-beginning returned b43500b-14|#
;;;;;

(defun FIND-TRIAD-BEGINNING ()
  "Returns the db with a triad beginning."
  (let* ((test (choose-one (eval (first (eval *composer*)))))
         (on-beat (get-on-beat (events (eval test))(very-first (events (eval test)))))
         (pcs (create-pitch-class-set (get-pitches on-beat))))
      (if (and (triad? on-beat)
               (or (members-all '(0 4 8) pcs)
                   (members-all '(0 4 7) pcs)
                   (members-all '(0 5 8) pcs)
                   (members-all '(2 7 11) pcs))
               (<= (third (first (events (eval test)))) 1000)
               (equal (length (events (eval test))) 4))
        test
        (find-triad-beginning))))

;;;;;
#|  Calling (members-all (0 4 8) (2 7 11)) 
  members-all returned nil|#
;;;;;

(defun MEMBERS-ALL (arrows target)
  "Checks to see if arrows are all in target."
  (cond ((null arrows) t)
        ((member (first arrows) target)
         (members-all (rest arrows) target))
        (t ())))

;;;;;
#| Calling (triad? ((111000 40 500 4 96) (111000 55 500 3 96) (111000 64 1000 2 96) (111000 72 1000 1 96))) 
 triad? returned t|#
;;;;;

(defun TRIAD? (events)
  "Checks to see if the events are a triad."
  (let ((pitch-classes (get-smallest-set (create-pitch-class-set (get-pitches events)))))
    (and (equal (length pitch-classes) 3)
         (and (> (- (second pitch-classes)(first pitch-classes)) 2)
              (< (- (second pitch-classes)(first pitch-classes)) 5))
         (and (> (- (third pitch-classes)(second pitch-classes)) 2)
              (< (- (third pitch-classes)(second pitch-classes)) 5)))))

;;;;;
#|  Calling (get-smallest-set (0 4 7)) 
  get-smallest-set returned (0 4 7)|#
;;;;;

(defun GET-SMALLEST-SET (set)
  "Returns the set with the smallest outer boundaries."
  (let* ((projected-sets (project set))
         (set-differentials (get-intervals projected-sets)))
    (nth (position (first (my-sort #'< set-differentials)) set-differentials) projected-sets)))

;;;;;
#|   Calling (project (0 4 7)) 
   project returned ((0 4 7) (4 7 12) (7 12 16))|#
;;;;;

(defun PROJECT (set &optional (length (length set))(times 0))
  "Projects the pc-set through its inversions."
  (if (equal length times)()
      (cons set
            (project (append (rest set)(list (+ 12 (first set)))) length (1+ times)))))

;;;;;
#| Calling (get-intervals ((0 4 7) (4 7 12) (7 12 16))) 
 get-intervals returned (7 8 9)|#
;;;;;

(defun GET-INTERVALS (sets)
  "Returns the intervals in the sets."
  (if  (null sets)()
       (cons (abs (apply #'+ (get-interval (first sets))))
             (get-intervals (rest sets)))))

;;;;;
#|  Calling (get-interval (0 4 7)) 
  get-interval returned (4 3)|#
;;;;;

(defun GET-INTERVAL (set)
  "Returns the intervals between set members."
  (if (null (rest set))
    () (cons (- (second set)(first set))
             (get-interval (rest set)))))

;;;;;
#| Calling (match-tonic-minor ((5000 55 1000 4 96) (5000 67 1000 3 96) (5000 62 1000 2 96) (5000 71 1000 1 96))) 
 match-tonic-minor returned nil|#
;;;;;

(defun MATCH-TONIC-MINOR (the-events)
  "Matches minor tonics."
  (let ((events (get-last-beat-events (break-into-beats the-events))))
    (and (not (null events))
         (and (all-members (mapcar #'second events) (apply #'append 
                                                           (loop for note in '(60 63 67)
                                                                 collect (project-octaves note))))
              (match-harmony (my-sort #'< (mapcar #'second events)) '(60 63 67))
              (match-harmony (my-sort #'> (mapcar #'second events)) '(60 63 67))))))

;;;;;
#| Calling (break-into-beats ((20000 48 2000 4 96) (20000 55 2000 3 96) (20000 64 2000 2 96) (20000 72 2000 1 96))) 
 break-into-beats returned ((20000 48 1000 4 96) (20000 55 1000 3 96) (20000 64 1000 2 96) (20000 72 1000 1 96) (21000 48 1000 4 96) (21000 55 1000 3 96) (21000 64 1000 2 96) (21000 72 1000 1 96))|#
;;;;;

(defun BREAK-INTO-BEATS (events)
  "Breaks events into beat-sized groupings."
  (sortcar #'< (apply #'append (chop-into-bites (sortcar #'< events)))))

;;;;;
#|  Calling (chop-into-bites ((20000 48 2000 4 96) (20000 55 2000 3 96) (20000 64 2000 2 96) (20000 72 2000 1 96))) 
  chop-into-bites returned (((20000 48 1000 4 96) (21000 48 1000 4 96)) ((20000 55 1000 3 96) (21000 55 1000 3 96)) ((20000 64 1000 2 96) (21000 64 1000 2 96)) ((20000 72 1000 1 96) (21000 72 1000 1 96)))|#
;;;;;

(defun CHOP-INTO-BITES (events)
  "Chops beats into groupings."
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
#|   Calling (chop (20000 48 2000 4 96)) 
   chop returned ((20000 48 1000 4 96) (21000 48 1000 4 96))|#
;;;;;

(defun CHOP (event &optional (begin-time (first event))(duration (third event)))
  "Chops beats over 1000 into beat-sized pieces."
  (if (< duration 1000)()
      (cons (append (list begin-time)
                    (list (second event))
                    '(1000)
                    (nthcdr 3 event))
            (chop event (+ begin-time 1000)(- duration 1000)))))
  
;;;;;
#| Calling (REMAINDER (76000 41 1500 4 96)) 
 REMAINDER returned ((77000 41 500 4 96))|#
;;;;;

(defun REMAINDER (event &optional (begin-time (first event))(duration (third event)))
  "Returns the remainder of the beat."
  (cond ((null event)())
        ((= duration 1000)())
        ((< duration 1000) (list (append (list begin-time)
                                         (list (second event))
                                         (list duration)
                                         (nthcdr 3 event))))
      (t (remainder event (+ begin-time 1000)(- duration 1000)))))

;;;;;
#| Calling (GET-FULL-BEAT ((77000 41 500 4 96) (77500 43 500 4 96))) 
 GET-FULL-BEAT returned ((77000 41 500 4 96) (77500 43 500 4 96))|#
;;;;;

(defun GET-FULL-BEAT (events &optional (begin-time (very-first events))(duration 0))
  "Returns one full beat of the music."
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
#| Calling (REMAINDERS ((77000 53 500 3 96) (77500 50 500 3 96))) 
 REMAINDERS returned NIL|#
;;;;;

(defun REMAINDERS (events &optional (begin-time (very-first events))(duration 0))
  "Returns remainders of beats."
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
#| Calling (REMOVE-FULL-BEAT ((77000 41 500 4 96) (77500 43 500 4 96))) 
 REMOVE-FULL-BEAT returned NIL|#
;;;;;

(defun REMOVE-FULL-BEAT (events &optional (begin-time (very-first events))(duration 0))
  "Removes one full beat from the events arg."
  (cond ((null events)())
        ((>= (+ duration (third (first events))) 1000)
         (rest events))
        (t (remove-full-beat (rest events)
                             (+ begin-time (third (first events)))
                             (+ (third (first events)) duration)))))

;;;;;
#| Calling (GET-OTHER-CHANNELS 4 ((77000 41 500 4 96) (76000 53 1500 3 96) (76000 60 1500 2 96) (76000 69 1500 1 96) (77500 43 500 4 96) (77500 50 500 3 96) (77500 59 500 2 96) (77500 67 500 1 96))) 
 GET-OTHER-CHANNELS returned ((76000 53 1500 3 96) (76000 60 1500 2 96) (76000 69 1500 1 96) (77500 50 500 3 96) (77500 59 500 2 96) (77500 67 500 1 96))|#
;;;;;

(defun GET-OTHER-CHANNELS (channel-not-to-get events)
  "Returns all but the first arg channeled events."
  (cond ((null events)())
        ((equal (fourth (first events)) channel-not-to-get)
         (get-other-channels channel-not-to-get (rest events)))
        (t (cons (first events)
                 (get-other-channels channel-not-to-get (rest events))))))

;;;;;
#| Calling (GET-LONG-PHRASES (6000 15000)) 
 GET-LONG-PHRASES returned NIL|#
;;;;;

(defun GET-LONG-PHRASES (distances)
  "Returns phrases of greater than 120000 duration."
  (cond ((null (rest distances))())
        ((> (- (second distances)(first distances)) 12000)
         (cons (firstn 2 distances)
               (get-long-phrases (rest distances))))
        (t (get-long-phrases (rest distances)))))

;;;;;
#|    Calling (Discover-cadences ((4000)) ((3000 61 1000 1 96) (3000 69 1000 2 96) (3000 69 1000 3 96) (3000 69 1000 4 96))) 
    Discover-cadences returned ((3000 61 1000 1 96) (3000 69 1000 2 96) (3000 69 1000 3 96) (3000 69 1000 4 96))|#
;;;;;

(defun DISCOVER-CADENCES (missing-cadence-locations ordered-events)
  "Makes an appropriate cadence possible."
  (if (null missing-cadence-locations) ordered-events
      (discover-cadences (rest missing-cadence-locations) 
                     (discover-cadence (first missing-cadence-locations) ordered-events))))

;;;;;
#|  Calling (Discover-cadence (4000) ((3000 61 1000 1 96) (3000 69 1000 2 96) (3000 69 1000 3 96) (3000 69 1000 4 96))) 
  Discover-cadence returned ((3000 61 1000 1 96) (3000 69 1000 2 96) (3000 69 1000 3 96) (3000 69 1000 4 96))|#
;;;;;

(defun DISCOVER-CADENCE (missing-cadence-locations ordered-events)
  "Discovers an appropriate cadence."
  (let* ((relevant-events (get-region (first missing-cadence-locations)(second missing-cadence-locations) ordered-events))
         (places-for-cadence (find-cadence-place relevant-events))
         (best-location-for-new-cadence (if places-for-cadence (find-best-on-time places-for-cadence) nil)))
    (if (null best-location-for-new-cadence) ordered-events
    (sortcar #'< 
             (append (resolve (get-region best-location-for-new-cadence (+ best-location-for-new-cadence 1000) relevant-events))
                     (remove-region best-location-for-new-cadence (+ best-location-for-new-cadence 1000) ordered-events))))))

;;;;;
#| Calling (FIND-CADENCE-START-TIMES ((3000 61 1000 1 96) (3000 69 1000 2 96) (3000 69 1000 3 96) (3000 69 1000 4 96))) 
 FIND-CADENCE-START-TIMES returned (3000)|#
;;;;;

(defun FIND-CADENCE-START-TIMES (ordered-events)
  "Finds the cadence start times."
  (let ((distance-to-cadence (distance-to-cadence ordered-events)))
    (cond ((null ordered-events)())
          ((null distance-to-cadence)
           (find-cadence-start-times (rest ordered-events)))
          (t (cons distance-to-cadence
                   (find-cadence-start-times (clear-to distance-to-cadence ordered-events)))))))

;;;;;
#| Calling (CLEAR-TO 3000 ((3000 61 1000 1 96) (3000 69 1000 2 96) (3000 69 1000 3 96) (3000 69 1000 4 96))) 
 CLEAR-TO returned NIL|#
;;;;;

(defun CLEAR-TO (distance-to-cadence ordered-events)
  "Clears the events up to the cadence."
  (cond ((null  ordered-events)())
        ((<= (very-first  ordered-events) distance-to-cadence)
         (clear-to distance-to-cadence (rest ordered-events)))
        (t (cons (first ordered-events)
                 (clear-to distance-to-cadence (rest ordered-events))))))

;;;;;
#| Calling (DISTANCE-TO-CADENCE ((3000 61 1000 1 96) (3000 69 1000 2 96) (3000 69 1000 3 96) (3000 69 1000 4 96))) 
  FIND-2000S returned NIL
 DISTANCE-TO-CADENCE returned 3000|#
;;;;;

(defun DISTANCE-TO-CADENCE (ordered-events)
  "Returns the distance tocadence of the arg."
  (let ((quarter-note-distance (find-1000s ordered-events))
        (half-note-distance (find-2000s ordered-events)))
    (cond ((and (null quarter-note-distance)(null half-note-distance)) ())
          ((null quarter-note-distance) half-note-distance)
          ((null half-note-distance) quarter-note-distance)
          (t (if (> quarter-note-distance half-note-distance) half-note-distance
                 quarter-note-distance)))))

;;;;;
#| Calling (FIND-1000S ((3000 61 1000 1 96) (3000 69 1000 2 96) (3000 69 1000 3 96) (3000 69 1000 4 96))) 
 FIND-1000S returned 3000|#
;;;;;

(defun FIND-1000S (ordered-events &optional (start-time (very-first ordered-events)))
  "Returns the ontime if the ordered events are all duration 1000."
  (cond ((null ordered-events)())
        ((and (let ((channel-1-event (first (get-channel 1 ordered-events))))
                (and (equal (third channel-1-event) 1000)
                     (equal start-time (first channel-1-event))))
              (let ((channel-1-event (first (get-channel 2 ordered-events))))
                (and (equal (third channel-1-event) 1000)
                     (equal start-time (first channel-1-event))))
              (let ((channel-1-event (first (get-channel 3 ordered-events))))
                (and (equal (third channel-1-event) 1000)
                     (equal start-time (first channel-1-event))))
              (let ((channel-1-event (first (get-channel 4 ordered-events))))
                (and (equal (third channel-1-event) 1000)
                     (equal start-time (first channel-1-event)))))
         start-time)
        (t (find-1000s (rest ordered-events)))))

;;;;;
#| Calling (FIND-2000S ((3000 61 1000 4 96) (3000 69 1000 3 96))) 
 FIND-2000S returned NIL|#
;;;;;

(defun FIND-2000S (ordered-events &optional (start-time (very-first ordered-events)))
  "Returns events of 2000 duration."
  (cond ((null ordered-events)())
        ((and (let ((channel-1-event (first (get-channel 1 ordered-events))))
                (and (equal (third channel-1-event) 2000)
                     (equal start-time (first channel-1-event))))
              (let ((channel-1-event (first (get-channel 2 ordered-events))))
                (and (equal (third channel-1-event) 2000)
                     (equal start-time (first channel-1-event))))
              (let ((channel-1-event (first (get-channel 3 ordered-events))))
                (and (equal (third channel-1-event) 2000)
                     (equal start-time (first channel-1-event))))
              (let ((channel-1-event (first (get-channel 4 ordered-events))))
                (and (equal (third channel-1-event) 2000)
                     (equal start-time (first channel-1-event)))))
         start-time)
        (t (find-2000s (rest ordered-events)))))

;;;;;
#|  Calling (TRANSPOSE 4 ((3000 61 1000 4 96) (3000 69 1000 3 96))) 
  TRANSPOSE returned ((3000 65 1000 4 96) (3000 73 1000 3 96))|#
;;;;;

(defun TRANSPOSE (amt events)
  "Transposes the events according to its first arg."
  (loop for event in events
        collect (if (not (zerop (second event)))
                  (append (list (first event))(list (+ (second event) amt))(nthcdr 2 event))
                  event)))

;;;;;
#| Calling (GET-BEAT-LENGTH ((24000 59 1000 4 96) (24000 62 1000 3 96) (24000 67 1000 2 96) (24000 74 1000 1 96))) 
 GET-BEAT-LENGTH returned 1000|#
;;;;;

(defun GET-BEAT-LENGTH (events)
  "this is used in re-time for setting the new time!
   requires that the first in events be sorted to current time!"
  (let ((time (very-first events)))
    (first (my-sort #'> (loop for event in events
                              collect (get-note-timing event time))))))

;;;;;
#|  Calling (GET-NOTE-TIMING (24000 59 1000 4 96) 24000) 
  GET-NOTE-TIMING returned 1000|#
;;;;;

(defun GET-NOTE-TIMING (event time)
  "grunt work for get-beat-length"
  (- (+ (first event)(third event)) time))

;;;;;
#| Calling (MATCH-THEM (48 64 67 72) (24 36 48 60 72 84 96 108 120 132 28 40 52 64 76 88 100 112 124 31 43 55 67 79 91 103 115 127) 1) 
 MATCH-THEM returned T|#
;;;;;

(defun MATCH-THEM (chord full-chord allowance)
  "Matches the chord with the list of pitches within the allowance."
  (cond ((null chord) t)
        ((and (not (member (first chord) full-chord))
              (zerop allowance))
         ())
        ((not (member (first chord) full-chord))
         (match-them (rest chord) full-chord (1- allowance)))
        (t (match-them (rest chord) full-chord allowance))))

;;;;;
#| Calling (REMOVE-REGION 0 4000 ((3000 61 1000 4 96) (3000 69 1000 3 96))) 
 REMOVE-REGION returned NIL|#
;;;;;

(defun REMOVE-REGION (begin-time end-time events)
  "Removes the region boardered by begin and end times."
  (cond ((null events)())
        ((and (>= (very-first events) begin-time)
              (< (very-first events) end-time))
         (remove-region begin-time end-time (rest events)))
        (t (cons (first events)
                 (remove-region begin-time end-time (rest events))))))

;;;;;
#| Calling (GET-REGION 0 4000 ((3000 61 1000 4 96) (3000 69 1000 3 96))) 
 GET-REGION returned ((3000 61 1000 4 96) (3000 69 1000 3 96))|#
;;;;;

(defun GET-REGION (begin-time end-time events)
  "Returns the region boardered by begin and end times."
  (cond ((null events)())
        ((and (>= (very-first events) begin-time)
              (< (very-first events) end-time))
         (cons (first events)
               (get-region begin-time end-time (rest events))))
        (t (get-region begin-time end-time (rest events)))))

;;;;;
#| Calling (RESOLVE ((3000 61 1000 4 96) (3000 69 1000 3 96))) 
 RESOLVE returned ((3000 61 1000 4 96) (3000 69 1000 3 96))|#
;;;;;

(defun RESOLVE (beat &optional (on-time (very-first beat)))
  "Resolves the beat if necessary."
  (cond ((null beat)())
        ((equal (third (first beat)) 1000)
         (cons (first beat)
               (resolve (rest beat) on-time)))
        (t (let ((test (get-on-beat (get-channel (fourth (first beat)) beat) on-time)))
             (cons (if (>= (third (first test)) 1000)(first test)
                       (append (firstn 2 (first test)) '(1000) (nthcdr 3 (first test))))
                   (resolve (remove-all (get-channel (fourth (first beat)) beat) beat) on-time))))))

;;;;;
#| Calling (FIND-BEST-ON-TIME (0 1000 2000)) 
 FIND-BEST-ON-TIME returned 1000|#
;;;;;

(defun FIND-BEST-ON-TIME (on-times)
  "Finds the best ontime."
  (find-closest (+ (/ (- (my-last on-times)(first on-times)) 2)(first on-times)) on-times))

;;;;;
#| Calling (FIND-CADENCE-PLACE ((3000 61 1000 4 96) (3000 69 1000 3 96))) 
 FIND-CADENCE-PLACE returned NIL|#
;;;;;

(defun FIND-CADENCE-PLACE (ordered-events)
  "Returns the best place for a first cadence."
  (let ((beats (collect-beats ordered-events)))
    (loop for beat in beats
          if (and (on-beat (firstn 4 beat)(very-first beat))
                  (triad? (firstn 4 beat))
                  (not-beyond-1000 beat))
          collect (very-first beat))))

;;;;;
#|  Calling (NOT-BEYOND-1000 ((3000 61 1000 4 96) (3000 69 1000 3 96))) 
  NOT-BEYOND-1000 returned T|#
;;;;;

(defun NOT-BEYOND-1000 (beat &optional (channel 1))
  "Returns t if the beat does not contain events beyond the incept time."
  (cond ((equal channel 5) t)
        ((not-beyond (get-channel channel beat))
         (not-beyond-1000 beat (+ channel 1)))
        (t ())))

;;;;;
#|  Calling (NOT-BEYOND ((3000 61 1000 4 96) (3000 69 1000 3 96))) 
  NOT-BEYOND returned NIL|#
;;;;;

(defun NOT-BEYOND (channel-events)
  "Returns events beyond the initial ontime."
  (if (not (> (apply #'+ (mapcar #'third channel-events)) 1000)) t))

;;;;;
#| Calling (ON-BEAT ((3000 61 1000 4 96) (3000 69 1000 3 96)) 3000) 
 ON-BEAT returned T|#
;;;;;

(defun ON-BEAT (events ontime)
  "Returns t if the events conform to ontime."
  (cond ((null events) t)
        ((and (thousandp (very-first events))(equal (very-first events) ontime))
         (on-beat (rest events) ontime))
        (t ())))

;;;;;
#|(find-closest 6 '(1 2 3 5 7 8))
       7|#
;;;;;

(defun FIND-CLOSEST (number list)
  "finds the closest number in list to number."
  (let ((test (loop for item in list
                    collect (abs (- number item)))))
    (nth (choose-one (positions (first (my-sort '< test)) test)) list)))

;;;;;
#|(positions 1 '(4 5 3 1 2 1 4 5))
       (3 5)|#
;;;;;

(defun POSITIONS (number list)
  "Shows the positions of number in list."
  (let ((position ()))
    (loop until (null (member number list))
          do (setf position (position number list))
          collect position
          do (setf list  (substitute 'x number list 
                                     :end (1+ position))))))

;;;;;
#|(remove-all '(1 2 3) '(3 4 2 1  5 6 3))
      (4 5 6)|#
;;;;;

(defun REMOVE-ALL (stuff other-stuff)
  "Removes all of the stuff from the other-stuff."
  (loop when (null stuff) return other-stuff
        do (setf other-stuff (remove (first stuff) other-stuff :test 'equal))
        do (setf stuff (rest stuff))))

;;;;;
#| Calling (get-channel 4 ((23000 48 500 4 96) (23000 64 1000 3 96) (23000 69 2000 2 96) (23000 72 1000 1 96) (23500 50 500 4 96) (24000 52 2000 4 96) (24000 64 2000 3 96) (24000 71 2000 1 96) (25000 68 1000 2 96))) 
 get-channel returned ((23000 48 500 4 96) (23500 50 500 4 96) (24000 52 2000 4 96))|#
;;;;;

(defun GET-CHANNEL (n music)
  "Gets the nth channel of the music."
  (cond ((null music)())
        ((equal (fourth (first music)) n)
         (cons (first music)(get-channel n (cdr music))))
        (t (get-channel n (cdr music)))))

;;;;;
#| Calling (match-harmony (48 67 72 76) (60 64 67)) 
  match-harmony returned t|#
;;;;;

(defun MATCH-HARMONY (one two)
  "Checks to see if its args match mod-12."
  (match-them (my-sort #'< one)
              (apply #'append 
                     (loop for note in two
                           collect (project-octaves note)))
              (floor (/ (length one) 4))))

;;;;;
#| Calling (project-octaves 60) 
 project-octaves returned (24 36 48 60 72 84 96 108 120 132)|#
;;;;;

(defun PROJECT-OCTAVES (note)
  "Projects its arg through a series of octaves."
  (let ((base-note (reduce-it note 20)))
    (loop until (> base-note 120)
          collect (setf base-note (+ base-note 12)))))

;;;;;
#|  Calling (reduce-it 60 20) 
  reduce-it returned 12|#
;;;;;

(defun REDUCE-IT (note base)
  "Reduces its first arg mod12 below its second arg."
  (if (< note base) note (reduce-it (- note 12) base)))

;;;;;
#| Calling (all-members (48 67 72 76) (24 36 48 60 72 84 96 108 120 132 28 40 52 64 76 88 100 112 124 31 43 55 67 79 91 103 115 127)) 
 all-members returned t|#
;;;;;

(defun ALL-MEMBERS (list target)
  "Checks to see if its first arg members are present in second arg."
  (cond ((null list) t)
        ((not (member (first list) target)) ())
        (t (all-members (rest list) target))))

;;;;;
#| Calling (get-last-beat-events ((18000 48 1000 4 96) (18000 64 1000 3 96) (18000 67 1000 2 96) (18000 72 1000 1 96))) 
 get-last-beat-events returned ((18000 48 1000 4 96) (18000 64 1000 3 96) (18000 67 1000 2 96) (18000 72 1000 1 96))|#
;;;;;

(defun GET-LAST-BEAT-EVENTS (events)
  "As its name suggests."
  (let* ((begin-time (first (my-last (sortcar #'< events))))
         (last-beat (get-all-events-with-start-time-of begin-time events)))
    (if (and (equal (length last-beat) 4)
             (thousandp (third (first last-beat))))
      last-beat)))

;;;;;
#|  Calling (get-all-events-with-start-time-of 18000 ((18000 48 1000 4 96) (18000 64 1000 3 96) (18000 67 1000 2 96) (18000 72 1000 1 96))) 
  get-all-events-with-start-time-of returned ((18000 48 1000 4 96) (18000 64 1000 3 96) (18000 67 1000 2 96) (18000 72 1000 1 96))|#
;;;;;

(defun GET-ALL-EVENTS-WITH-START-TIME-OF (start-time events)
  "As its name suggests."
  (cond ((null events)())
        ((equal (very-first events) start-time)
         (cons (first events)
               (get-all-events-with-start-time-of start-time (rest events))))
        (t (get-all-events-with-start-time-of start-time (rest events)))))

;;;;;
#| Calling (get-pitches ((0 48 1000 4 96))) 
 get-pitches returned (48)|#
;;;;;

(defun GET-PITCHES (events)
  "Gets the pitches from its arg."
  (loop for event in events
        collect (second event)))

;;;;;
#| Calling (incf-beat b35300b-42) 
 incf-beat returned b35300b-3
|#
;;;;;

(defun INCF-BEAT (beat)
  "Increments the beat number."
  (implode (list (get-db-name beat) '- (1+ (my-last (explode beat))))))

;;;;;
#|  Calling (get-db-name b35300b-42) 
  get-db-name returned b35300b|#
;;;;;

(defun GET-DB-NAME (lexicon)
  "Returns the database name."
  (implode (get-db-n (explode lexicon))))

;;;;;
#|   Calling (get-db-n (b 3 5 3 0 0 b - 4 2)) 
   get-db-n returned (b 3 5 3 0 0 b)|#
;;;;;

(defun GET-DB-N (exploded-lexicon)
  "Checks for dashes in the db-name."
  (cond ((equal (first exploded-lexicon) '-)
         ())
        ((null exploded-lexicon)())
        (t (cons (first exploded-lexicon)
                 (get-db-n (rest exploded-lexicon))))))

;;;;;
#| Calling (match-bach-tonic ((56000 62 1000 4 96) (56000 67 1000 3 96) (56000 74 500 2 96) (56000 81 2000 1 96) (56500 72 250 2 96) (56750 71 250 2 96) (57000 50 1000 4 96) (57000 66 1000 3 96) (57000 72 1000 2 96))) 
 match-bach-tonic returned nil|#
;;;;;

(defun MATCH-BACH-TONIC (the-events)
  "Returns true if the events are tonic."
  (let ((events (get-last-beat-events (break-into-beats the-events))))
    (and (not (null events))
         (and (all-members (mapcar #'second events) (apply #'append 
                                                           (loop for note in '(60 64 67)
                                                                 collect (project-octaves note))))(match-harmony (my-sort #'< (mapcar #'second events)) '(60 64 67))
              (match-harmony (my-sort #'> (mapcar #'second events)) '(60 64 67))))))

;;;;;
#| Calling (find-events-duration ((53000 52 500 4 96) (53000 67 500 3 96) (53000 76 1000 2 96) (53000 84 1000 1 96) (53500 54 500 4 96) (53500 69 500 3 96))) 
 find-events-duration returned 1000|#
;;;;;

(defun FIND-EVENTS-DURATION (events &optional (duration 0))
  "Returns the events duration."
  (cond ((null events) duration)
        ((equal (fourth (first events)) 1)
         (find-events-duration (rest events) (+ duration (third (first events)))))
        (t (find-events-duration (rest events) duration))))

;;;;;
#|Calling (re-time (((15000 60 1000 4 96) (15000 64 1000 3 96) . . .
re-time returned (((0 60 1000 4 96) (0 64 1000 3 96) . . .|#
;;;;;

(defun RE-TIME (event-lists &optional (current-time 0))
  "Re-times the beats to fit together."
  (if (null event-lists)()
      (cons (loop for event in (set-to-zero (first event-lists))
                  collect (cons (+ (first event) current-time) (rest event)))
            (re-time (rest event-lists) (+ current-time 
                                           (get-beat-length 
                                            (first event-lists)))))))

;;;;;
#| Calling (transpose-to-bach-range ((0 60 1000 4 96) (0 64 1000 3 96)  . . .
 transpose-to-bach-range returned ((0 64 1000 4 96) (0 68 1000 3 96) . . .|#
;;;;;

(defun TRANSPOSE-TO-BACH-RANGE (events)
  "As its name suggests."
  (let* ((high-low (highest-lowest-notes events))
         (intervals-off (list (- 83 (first high-low))
                              (- 40 (second high-low))))
         (middle (put-it-in-the-middle intervals-off)))
    (progn 
           (transpose middle events))))

;;;;;
#|  Calling (put-it-in-the-middle (4 4)) 
  put-it-in-the-middle returned 2 values :
       4
       0|#
;;;;;

(defun PUT-IT-IN-THE-MIDDLE (extremes)
  "Gets the average."
  (round (/ (+ (second extremes)(first extremes)) 2)))

;;;;;
#| Calling (highest-lowest-notes ((0 60 1000 4 96) (0 64 1000 3 96) . . .
  highest-lowest-notes returned (79 36)|#
;;;;;

(defun HIGHEST-LOWEST-NOTES (events)
  "Returns the highest and lowest pitches of its arg."
  (list (first (my-sort #'> (loop for event in (get-channel 1 events)
                                  collect (second event))))
        (first (my-sort #'< (loop for event in (let ((test (get-channel 4 events)))
                                                 (if (null test)(get-channel 2 events) test))
                                  collect (second event))))))

;;;;;
#|Calling (cadence-collapse ((0 48 1000 4 96) (0 60 1000 3 96) (0 67 1000 2 96) (0 76 1000 1 96) . . . 
cadence-collapse returned ((0 48 1000 4 96) (0 60 1000 3 96) . . . .|#
;;;;;

(defun CADENCE-COLLAPSE (events)
  "Ensures the final chord will not have offbeats."
  (apply #'append (collapse (collect-beats (sortcar #'< events)))))

;;;;;
#|Calling (collapse (((0 48 1000 4 96) (0 60 1000 3 96) (0 67 1000 2 96)  . . .
collapse returned (((0 48 1000 4 96) (0 60 1000 3 96)  . . . |#
;;;;;

(defun COLLAPSE (beats)
  "Collapses the final cadence."
  (cond ((null beats)())
        ((and (equal (length (first beats)) 4)
              (equal (third (first (first beats))) 2000))
         (cons (make-1000s (first beats))
               (collapse (reset (rest beats) 1000))))
        (t (cons (first beats)
                 (collapse (rest beats))))))

;;;;;
#|   Calling (make-1000s ((32000 52 2000 4 96) (32000 59 2000 3 96) (32000 64 2000 2 96) (32000 67 2000 1 96))) 
   make-1000s returned ((32000 52 1000 4 96) (32000 59 1000 3 96) (32000 64 1000 2 96) (32000 67 1000 1 96))|#
;;;;;

(defun MAKE-1000S (beat)
  "Makes all of the beat's durations into 1000s."
  (loop for event in beat
        collect (append (firstn 2 event) '(1000) (nthcdr 3 event))))

;;;;;
#|   Calling (reset (((34000 52 500 4 96) (34000 64 500 3 96) (34000 67 500 2 96) (34000 72 1000 1 96) . . .
reset returned (((33000 52 500 4 96) (33000 64 500 3 96)  . . .|#
;;;;;

(defun RESET (beats subtraction)
  "Resets the beats appropriately."
  (if (null beats)()
      (cons (loop for event in (first beats)
                  collect (append (list (- (first event) subtraction)) (cdr event)))
            (reset (rest beats) subtraction))))

;;;;;
#| Calling (delay-for-upbeat ((0 60 1000 1 124))) 
 delay-for-upbeat returned ((3000 60 1000 1 124))
|#
;;;;;

(defun DELAY-FOR-UPBEAT (events)
  "Delays the upbeat."
  (reset-events-to events 3000))

;;;;;
#|Calling (reset-events-to ((0 60 1000 1 124)) 3000) 
  reset-events-to returned ((3000 60 1000 1 124))|#
;;;;;

(defun RESET-EVENTS-TO (events begin-time)
  "Resets the events for the delayed beat."
  (loop for event in (set-to-zero  events)
        collect (cons (+ begin-time (first event))(rest event))))

;;;;;
#|Calling (get-on-beat ((53000 46 1000 4 96) (53000 53 500 3 96) (53000 62 500 2 96) (53000 62 1000 1 96) (53500 52 250 3 96) (53500 55 500 2 96) (53750 50 250 3 96)) 53000) 
 get-on-beat returned ((53000 46 1000 4 96) (53000 53 500 3 96) (53000 62 500 2 96) (53000 62 1000 1 96))|#
;;;;;

(defun GET-ON-BEAT (events ontime)
  "Returns the on beat from the events."
  (cond ((null events) ()) 
        ((and (thousandp (very-first events))(equal (very-first events) ontime))
         (cons (first events)
               (get-on-beat (rest events) ontime)))
        (t ())))

;;;;;
#| Calling (check-mt ((0 43 1000 4 96) (0 59 1000 3 96) (0 62 1000 2 96) (0 67 1000 1 96))) 
 check-mt returned nil|#
;;;;;

(defun CHECK-MT (events)
  "Returns the major tonic."
  (get-tonic events))

;;;;;
#|  Calling (get-tonic ((0 43 1000 4 96) (0 59 1000 3 96) (0 62 1000 2 96) (0 67 1000 1 96))) 
  get-tonic returned nil|#
;;;;;

(defun GET-TONIC  (events)
  "Returns the tonic."
  (and (or (all (create-pitch-class-set (get-pitches events))
                '(0 4 7))
           (all (create-pitch-class-set (get-pitches events))
                '(0 3 7)))
       (zerop (first (create-pitch-class-set (get-pitches (get-channel 4 (sortcar #'< events))))))))

;;;;;
#|   Calling (all (2 7 11) (0 4 7)) 
   all returned nil|#
;;;;;

(defun ALL (first second)
  "Tests for presence of all of first arg in second arg."
  (cond ((null first) t)
        ((member (first first) second) 
         (all (rest first) second))
        (t ())))

;;;;;
#|Calling (ensure-necessary-cadences ((0 52 1000 4 96) (0 60 1000 3 96) . . .
 ensure-necessary-cadences returned ((0 52 1000 4 96) (0 60 1000 3 96) . . .|#
;;;;;

(defun ENSURE-NECESSARY-CADENCES (ordered-events)
  "Ensures the cadences are proper."
  (let ((cadence-start-times (find-cadence-start-times ordered-events)))
    (Discover-cadences (get-long-phrases (if (not (zerop (first cadence-start-times))) (cons 0 cadence-start-times) cadence-start-times))
                   ordered-events)))

;;;;;
#|Calling (check-for-parallel ((0 48 1000 4 96) (0 64 1000 3 96) . . .
check-for-parallel returned t|#
;;;;;

(defun CHECK-FOR-PARALLEL (events)
  "Checks for parallel motion."
  (let ((sorted-pitches-by-beat (loop for beat in (collect-beats (firstn 30 (sortcar #'< events)))
                                      collect (get-pitches (get-on-beat beat (very-first beat))))))
    (and (equal (length (first sorted-pitches-by-beat)) 4)
         (equal (length (second sorted-pitches-by-beat)) 4)
         (or (and (plusp (- (first (first sorted-pitches-by-beat))
                            (first (second sorted-pitches-by-beat))))
                  (plusp (- (second (first sorted-pitches-by-beat))
                            (second (second sorted-pitches-by-beat))))
                  (plusp (- (third (first sorted-pitches-by-beat))
                            (third (second sorted-pitches-by-beat))))
                  (plusp (- (fourth (first sorted-pitches-by-beat))
                            (fourth (second sorted-pitches-by-beat)))))
             (and (minusp (- (first (first sorted-pitches-by-beat))
                             (first (second sorted-pitches-by-beat))))
                  (minusp (- (second (first sorted-pitches-by-beat))
                             (second (second sorted-pitches-by-beat))))
                  (minusp (- (third (first sorted-pitches-by-beat))
                             (third (second sorted-pitches-by-beat))))
                  (minusp (- (fourth (first sorted-pitches-by-beat))
                             (fourth (second sorted-pitches-by-beat)))))))))

;;;;;
#|Calling (wait-for-cadence ((0 48 1000 4 96) (0 64 1000 3 96) (0 67 1000 2 96) (0 72 1000 1 96)  . . .
 wait-for-cadence returned t|#
;;;;;

(defun WAIT-FOR-CADENCE (events &optional (start-time (very-first events)))
  "Ensures the cadence is the proper length."
  (cond ((null events)())
        ((> (very-first events) (+ start-time 4000))
         t)
        ((> (third (first events)) 1000) ())
        (t (wait-for-cadence (rest events) start-time))))

(add-menu-items *apple-menu* 
                (make-instance 'menu-item 
                  :menu-item-title "About Chorale..." 
                  :menu-item-action 
                  #'(lambda nil (make-instance 'all-about-window)
                                        ))
                (make-instance 'menu-item 
                  :menu-item-title "-"
                  :menu-item-action 
                  #'(lambda nil)))

(window-close *about-window*)
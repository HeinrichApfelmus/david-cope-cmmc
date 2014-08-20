
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;     Chorale Function/Chapter 4      ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;           platform dependent        ;;;;;
                   ;;;;;          code to run chorale        ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

;;;;  the basic play functions for the programs of CMMC

(export '(*score*))

;;;;  variables for play functions.

(defVar *CHECK-RANDOM* () "Random checker.")
(defVar *SCORE* () "Score variable.")
(defVar *VARIABLE-WINDOW* () "Window variable.")
(defVar *INSTRUMENTS* (make-hash-table) 
  "hash table <Instrument-Number> => list: <Allocator> <Channel>")
(defVar *NOTE-ALLOCATOR* nil "the current QuickTime Music note allocator")
(defVar *THE-WORK* () "The work variable.")
(defVar *TEMPORARY-NAME* () "Temporary variable.")
(defVar *WORK-LIST* () "Worklist variable.")
(defVar *FILE-NAME* () "Filename variable.")
(defVar *NAME* () "Name variable.")
(defVar *INSTRUMENT* 46 "Instrument variable.")
(defVar *TEMPO* 60 "Tempo variable.")
(defVar *KEY* 0 "Key variable.")
(defVar *LOUD* 64 "Loudness variable.")
(setq *print-case* :downcase)
(defVar *CHANNEL* 1 "Channel variable.")
(defVar *INSTR* () "Instrument variable.")
(defVar *TEMPO-BUTTON* () "Tempo button")
(defVar *KEY-BUTTON* () "Key button")
(defVar *LOUD-BUTTON* () "Loud button")
(defVar *CHANNEL-1* 53 "Channel 1 variable")
(defVar *CHANNEL-2* 53 "Channel 2 variable")
(defVar *CHANNEL-3* 53 "Channel 3 variable")
(defVar *CHANNEL-4* 53 "Channel 4 variable")                                 
(defVar *CHANNEL-5* 5 "Channel 5 variable")
(defVar *CHANNEL-6* 6 "Channel 6 variable")
(defVar *CHANNEL-7* 7 "Channel 7 variable")
(defVar *CHANNEL-8* 8 "Channel 8 variable")
(defVar *CHANNEL-9* 9 "Channel 9 variable")
(defVar *CHANNEL-10* 10 "Channel 10 variable")                                 
(defVar *CHANNEL-11* 11 "Channel 11 variable")
(defVar *CHANNEL-12* 12 "Channel 12 variable")
(defVar *CHANNEL-13* 13 "Channel 13 variable")
(defVar *CHANNEL-14* 14 "Channel 14 variable")
(defVar *CHANNEL-15* 15 "Channel 15 variable")
(defVar *CHANNEL-16* 16 "Channel 16 variable")
(defVar *SCALE-NOTE* 60 "Channel variable")
(defVar *SCALE-DIRECTION* 'up "Scale direction variable")
;(defVar *EVENTS* () "Where events for playing are stored.")

;______________________________
; Instrument Table             |
;_____________________________/

(defVar *INSTRUMENTS-LIST* 
  '((1 = acoustic grand piano)
    (2 = bright acoustic piano)(3 = electric grand piano)(4 = honkytonk piano)
    (5 = Rhodes piano)(6 = chorused piano)(7 = harpsichord)(8 = clavinet)
    (9 = celesta)(10 = glockenspiel)(11 = music box)(12 = vibraphone)
    (13 = marimba)(14 = xylophone)(15 = tubular bells)(16 = dulcimer)(17 = Hammond organ)
    (18 = percussive organ)(19 = rock organ)(20 = church organ)(21 = reed organ)
    (22 = accordian)(23 = harmonica)(24 = tango accordian)(25 = acoustic nylon guitar)
    (26 = acoustic steel guitar)(27 = electric jazz guitar)(28 = electric clean guitar)
    (29 = electric guitar muted)(30 = overdriven guitar)(31 = distortion guitar)(32 = guitar harmonics)
    (33 = acoustic fretless bass)(34 = electric bass fingered)(35 = electric bass picked)
    (36 = fretless bass)(37 = slap bass 1)(38 = slap bass 2)(39 = synth bass 1)
    (40 = synth bass 2)(41 = violin)(42 = viola)(43 = cello)
    (44 = contrabass)(45 = tremolo strings)(46 = pizzicato strings)(47 = orchestral harp)
    (48 = timpani)(49 = acoustic string ensemble)(50 = acoustic string ensemble 2)(51 = synth strings 1)
    (52 = synth strings 2)(53 = ah choir)(54 = ooh choir)(55 = SynthVox)
    (56 = orchestra hit)(57 = trumpet)(58 = trombone)(59 = tuba)
    (60 = muted trumpet)(61 = French horn)(62 = brass section)(63 = SynthBrass 1)
    (64 = SynthBrass 2)(65 = soprano sax)(66 = alto sax)(67 = tenor sax)
    (68 = baritone sax)(69 = oboe)(70 = English horn)(71 = bassoon)
    (72 = clarinet)(73 = piccolo)(74 = flute)(75 = recorder)
    (76 = pan flute)(77 = bottle blow)(78 = Shakuhachi)(79 = whistle)
    (80 = ocarina)(81 = square wave)(82 = saw wave)(83 = calliope)
    (84 = chiffer)(85 = charang)(86 = solo vox)(87 = 5th saw wave)
    (88 = bass and lead)(89 = fantasy)(90 = warm)(91 = polysynth)
    (92 = choir)(93 = bowed)(94 = metal)(95 = halo)
    (96 = sweep)(97 = ice rain)(98 = sound tracks)(99 = crystal)
    (100 = atmosphere)(101 = brightness)(102 = goblins)(103 = echos)
    (104 = space)(105 = sitar)(106 = banjo)(107 = shamisen)(108 = koto)
    (109 = kalimba)(110 = bagpipe)(111 = fiddle)(112 = shannai)(113 = tinkle bell)
    (114 = agogo)(115 = steel drums)(116 = woodblock)(117 = Taiko drum)(118 = melodic tom)
    (119 = synth drum)(120 = reverse cymbal)(121 = guitar fret noise)(122 = breathnoise)
    (123 = seashore)(124 = bird tweet)(125 = telephone ring)(126 = helicopter)
    (127 = applause)(128 = gunshot)(16385 = standard kit)(16393 = room kit)
    (16401 = power kit)(16409 = electronic kit)(16410 = analog kit)(16425 = brush kit)
    (16433 = orchestra kit)))

;;;;;
#|Returns QT errors.|#
;;;;;

(defMacro QTMA-ERROR (Form) 
"in: Form {t}.
  Check for QuickTime Music Architecture error result codes"
          (let ((Error (gensym))
                (Trap (and (listp Form) 
                           (symbolp (first Form))
                           (equal (symbol-package (first Form)) (find-package :traps))
                           (symbol-name (first Form)))))
            `(let ((,Error ,Form))
               (case ,Error
                 (#.#$noErr 0)
                 (-128 0)
                 (t (error "QuickTime Music Architecture Error: ~A calling trap: ~A" ,Error ,Trap))))))

;;;;;
#|Stops the music.|#
;;;;;

(defun STOP-THE-MUSIC () 
"Close ALL the currently open Components and NoteChannels.
  This function should be called before the application is quit."
       (maphash
        #'(lambda (Instrument-Number Instrument)
            (declare (ignore Instrument-Number))
            (#_NADisposeNoteChannel (first Instrument) (second Instrument))
            (#_CloseComponent (first Instrument)))
        *Instruments*)
       (clrhash *Instruments*)
      (setq *Note-Allocator* nil))

;;;;;
#|Opens a default noteallocator if necessary.|#
;;;;;

(defun OPEN-DEFAULT-NOTE-ALLOCATOR () 
"out: Note-Allocator {NoteAllocator}.
  Return a note allocator." 
       (let ((Note-Allocator (#_OpenDefaultComponent #$kNOTEALLOCATORCOMPONENTTYPE 0)))
         (when (%null-ptr-p Note-Allocator) (error "QT Music: Could not create note allocator"))
         Note-Allocator))

;;;;;
#|Opens a noteallocator if necessary.|#
;;;;;

(defun DEFAULT-NOTE-ALLOCATOR () 
"out: Note-Allocator {NoteAllocator}.
  Return a note allocator. Open one if necessary."
       (or
        *Note-Allocator*
        (setq *Note-Allocator* (open-default-note-allocator))))

(defVar *NOTE-CHANNEL* ())
(defVar *NOTE-REQUEST* ())
(defVar *PROCESS* ())

;;;;;
#|A simple pseudonym for a function necessary with other programs - not here.|#
;;;;;

(defun SCULPT-MUSIC (events)
  events)

;;;;;
#|Instrument allocator.|#
;;;;;

(defun MAKE-INSTRUMENT-ALLOCATOR-AND-CHANNEL (Instrument-Number) "
  in:  Instrument-Number {fixnum}.
  out: Allocator {Component}, Channel {NoteChannel}, Request {NoteRequest}.
  Create, initialize and return a Note Allcator, and a Note Channel."
  (let ((Note-Allocator (open-default-note-allocator))
        (Note-Channel (#_NewPtr 4))
        (Note-Request (make-record
                       :NoteRequest 
                       :info.flags 0
                       :info.polyphony 3      ;; three simultaneous tones
                       :info.typicalPolyphony #b0010000
                       :tone.synthesizertype :|ss  |)))
      (let ((&NR.Tone (rref Note-Request :NoteRequest.tone)))
        (QTMA-error (#_NAStuffToneDescription Note-Allocator Instrument-Number &NR.Tone))
        (rlet ((&Note-Request :pointer) 
               (&Note-Channel :pointer))
          (%put-long Note-Channel 0)
          (%put-ptr &Note-Channel Note-Channel)
          (%put-ptr &Note-Request Note-Request)
          (QTMA-error (#_NANewNoteChannel Note-Allocator Note-Request &Note-Channel))
          (setq Note-Channel (%get-ptr &Note-Channel))
          (setq Note-Request (%get-ptr &Note-Request))
          (values
           Note-Allocator
           Note-Channel
           Note-Request)))))

;;;;;
#|Allows users to pick instrument from available list.|#
;;;;;

(defun USER-PICK-INSTRUMENT (&optional (Default-Instrument-Number 1)) "
  in: &optional Default-Instrument-Number {fixnum} default 1 = piano.
  out: Instrument-Number {fixnum},
       Instrument-Name {string}.
  Make the user select an instrument.
  Return the instrument number or nil if no instrument 
  was selected."
  (when (= Default-Instrument-Number 0) (setq Default-Instrument-Number 1))
  (let ((Note-Allocator (open-default-note-allocator)))
    (rlet ((Note-Request 
            :NoteRequest 
            :info.flags 0
            :info.polyphony 2      ;; two simultaneous tones
            :info.typicalPolyphony #b0010000))
      (let ((&NR.Tone (rref Note-Request :NoteRequest.tone)))
        (QTMA-error (#_NAStuffToneDescription Note-Allocator Default-Instrument-Number &NR.Tone))
        (with-pstrs ((Prompt "Pick An Instrument:"))
          (QTMA-error (#_NAPickInstrument Note-Allocator (%null-ptr) Prompt &NR.Tone #$kPickUserInsts 0 0 0)))
        (multiple-value-prog1
          (values 
           (rref Note-Request :NoteRequest.tone.instrumentnumber)
           (copy-seq (rref Note-Request :NoteRequest.tone.instrumentName)))
          (QTMA-error (#_CloseComponent Note-Allocator)))))))

;;;;;
#|Plays individual notes.|#
;;;;;

(defun PLAY-NOTE (Pitch Velocity &optional (Instrument-Number (user-pick-instrument))) "
  in:  Pitch {fixnum}, Velocity {fixnum}, &optional Instrument-Number {fixnum} default (user-pick-instrument).
  Play a note with <Pitch> at <Velocity> on <Instrument-Number>."
  (when (= 0 Instrument-Number) (return-from play-note))
  (let ((Instrument (gethash Instrument-Number *Instruments*)))
    (cond
     (Instrument                      ;;; found instrument in cache!
      (QTMA-error (#_NAPlayNote (first Instrument) (second Instrument) Pitch Velocity)))
     (t
      (multiple-value-bind (Allocator Channel Request)
                           (make-instrument-allocator-and-channel Instrument-Number)
        (setf (gethash Instrument-Number *Instruments*) (list Allocator Channel Request))
        (QTMA-error (#_NASetNoteChannelVolume Allocator Channel 100000))
        (QTMA-error (#_NAPlayNote Allocator Channel Pitch Velocity)))))))

;;;;;
#|Plays the events.|#
;;;;;

(defun PLAY-EVENTS (events &optional (instrument nil))
  "Plays the events."
  (let ((changed-events (make-play-list (delay events)))
        (instr (if (numberp instrument) instrument (find-instrument-number instrument)))
        (time (get-internal-real-time)))
    (loop until (null changed-events)
          do (if (<= (first (first changed-events)) (- (get-internal-real-time) time))
               (and (play-note (second (first changed-events)) (third (first changed-events)) 
                               (if (null instrument)
                                 (get-channel-instrument (fourth (first changed-events))) instr))
                    (setf changed-events (rest changed-events)))))))

;;;;;
#|(add-opening-rest '((0 60 1000 1 127)))
  ((0 0 1000 1 0) (1000 60 1000 1 127))|#
;;;;;

(defun ADD-OPENING-REST (events)
  "Allows the internal clock to catch up to the first event."
  (cons '(0 0 1000 1 0)
        (loop for event in events
              collect (append (list (+ (first event) 1000))
                              (cdr event)))))

(defun DELAY (events)
  "Fixes a bug in QT."
  (cons '(0 0 1000 1 0)
    (loop for i in events
          collect (cons (+ (first i) 1000)
                        (rest i)))))

;;;;;
#| (make-play-list
           ((0 60 1000 4 127) (0 67 1000 2 127) (0 64 1000 3 127) (0 72 1000 1 127)...
        ((0 60 127 4) (0 67 127 2) (0 64 127 3) (0 72 127 1)
         (1000 60 0 4) (1000 67 0 2) (1000 64 0 3) (1000 72 0 1)...|#
;;;;;

(defun MAKE-PLAY-LIST (events)
  "Returns the four-digit play list for playing."
  (let ((new-events (convert-the-events events)))
    (sortcar  
     #'< 
     (apply #'append 
            (loop for event in new-events
                  collect (list 
                           (list 
                            (first event)
                            (second event) 
                            (round (* (/  *loud* 100) (fifth event)))
                            (fourth event))
                           (list (+ (first event)(third event))
                                 (second event) 
                                 0 
                                 (fourth event))))))))

;;;;;
#| (convert-the-events
           '((0 60 1000 4 127) (0 67 1000 2 127) (0 64 1000 3 127) (0 72 1000 1 127)
             (1000 55 1000 4 127) (1000 67 1000 2 127) (1000 59 1000 3 127)...
       ((0 60 1000 4 127) (0 67 1000 2 127) (0 64 1000 3 127)
        (0 72 1000 1 127) (1000 55 1000 4 127)...|#
;;;;;

(defun CONVERT-THE-EVENTS (events &optional (tempo *tempo*))
  "Converts the events to form."
  (loop for item in events
        if (equal (first item) 'setq) 
        do (eval item)
        else
        collect (list (floor (* (/ 60 tempo)(car item)))
                      (let ((note (+ (cadr item) *key*)))
                        (if (< note 26) -1 note))
                      (floor (* (/ 60 tempo)(caddr item)))
                      (cadddr item) 
                      (cadddr (cdr item)))))

;;;;;
#|(find-instrument-number '(bowed))
    93|#
;;;;;

(defun FIND-INSTRUMENT-NUMBER (list-name &optional (instr *instruments-list*))
  "Returns the first arg's instrument name."
  (cond ((null instr)())
        ((equal (first list-name)(third (first instr)))
         (first (first instr)))
        (t (find-instrument-number list-name (rest instr)))))

;;;;;
#|(get-instrument)
   53|#
;;;;;

(defun GET-INSTRUMENT ()
  "Returns the current channel's instrument number."
  (eval (read-from-string 
         (format () "~A~A~A" 
                 '*channel- 
                 *channel* 
                 '*))))

;;;;;
#|(find-instrument-name 1)
"acoustic grand piano"|#
;;;;;

(defun FIND-INSTRUMENT-NAME (number &optional (instr *instruments-list*))
  "Returns the instrument name associated with arg number."
  (let ((test (nthcdr 2 (assoc number instr))))
    (if (null test) "acoustic grand piano" (make-list-into-string test))))

;;;;;
#|(get-channel-instrument 1)
   53|#
;;;;;

(defun GET-CHANNEL-INSTRUMENT (channel-number)
  "Returns the instrument number associated with arg number."
  (eval (second (assoc channel-number '((1 *channel-1*)(2 *channel-2*)(3 *channel-3*)(4 *channel-4*)
                                        (5 *channel-5*)(6 *channel-6*)(7 *channel-7*)(8 *channel-8*)(9 *channel-9*)(10 *channel-10*)
                                        (11 *channel-11*)(12 *channel-12*)(13 *channel-13*)(14 *channel-14*)(15 *channel-15*)
                                        (16 *channel-16*))))))

;;;important play windows

(defClass PLAY-WINDOW (window)
  nil
  (:default-initargs :window-type :document
    :window-title "QuickTime"
    :view-position #@(10 60)
    :view-size #@(130 170)))

(defMethod INITIALIZE-INSTANCE ((window PLAY-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs)
  (add-subviews window 
    (setq *instr* 
          (make-instance 'static-text-dialog-item
            :dialog-item-text 
            (find-instrument-name 
             (eval
              (read-from-string (format () "~A~A~A"
                                        "*channel-" *channel* "*"))))
            :view-position #@(26 19)
            :view-size #@(225 25)
            :view-nick-name 'instrument-display
            :view-font '("monaco" 9)))
    (make-dialog-item 
     'button-dialog-item
     #@(16 45) #@(104 18)
     "Set Instrument"
     #'(lambda (item) 
         item (set (read-from-string (format () "~A~A~A"
                                             "*channel-" *channel* "*"))
                   (user-pick-instrument (get-instrument)))
         (set-dialog-item-text
          (find-named-sibling item 'instrument-display)
          (find-instrument-name 
           (eval
            (read-from-string (format () "~A~A~A"
                                      "*channel-" *channel* "*"))))))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 75) #@(104 18)
     "Loudness"
     #'(lambda (item) 
         item (if *loud-button* (window-close *loud-button*))
         (setq *loud-button* (make-instance 'loud-window)))
     :view-font '("geneva" 10 :bold))
(make-dialog-item 
     'button-dialog-item
     #@(16 105) #@(104 18)
     "Play"
     #'(lambda (item) 
         item (stop-the-music)
         (setq *process* (process-run-function "" #'play-events (second (eval *events*)))))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 135) #@(104 18)
     "Stop"
     #'(lambda (item) 
         item 
         (stop-the-music)(if *process* (process-kill *process*)))
     :view-font '("geneva" 10 :bold))))

(defVar *LOUD-SCROLL* (make-instance
                           'scroll-bar-dialog-item
                           :view-position #@(102 18)
                           :direction :horizontal
                           :length 120
                           :setting *loud*
                           :max 100
                           :min 0
                           :dialog-item-action
                           #'(lambda (item &aux (setting (format nil 
                                                                 "~a"
                                                                 (scroll-bar-setting 
                                                                  item))))
                               (set-dialog-item-text
                                (find-named-sibling item 'loud-display)
                                setting)
                               (setq *loud* (read-from-string setting))
                               (window-update-event-handler (view-window item)))))

(defClass LOUD-WINDOW (window)
  nil
  (:default-initargs :window-type :document
    :window-title ""
    :view-position #@(10 300)
    :view-size #@(300 50)))

(defMethod INITIALIZE-INSTANCE ((window LOUD-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs)
  (set-scroll-bar-setting *loud-scroll* *loud*)
  (add-subviews window
    (make-instance 'static-text-dialog-item
      :dialog-item-text (write-to-string *loud*)
      :view-position #@(244 20)
      :view-size #@(32 15)
      :view-nick-name 'loud-display
      :view-font '("palatino" 12 :bold)) 
    (make-instance 'static-text-dialog-item
      :dialog-item-text "Set Loudness:"
      :view-position #@(14 20)
      :view-size #@(82 15)
      :view-nick-name 'loud-display
      :view-font '("palatino" 12 :bold))
    *loud-scroll*))


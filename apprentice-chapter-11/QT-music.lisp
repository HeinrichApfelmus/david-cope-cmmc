
;;;

                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;        Apprentice/Chapter 11        ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;    simple code to run Apprentice    ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

;************************
;   Instrument Table    *
;************************

(setq *print-case* :downcase)
(defVar *SCALE-NOTE* 60)
(defVar *SCALE-DIRECTION* 'up)
(defVar *INSTRUMENTS* (make-hash-table) 
  "hash table <Instrument-Number> => list: <Allocator> <Channel>")
(defVar *NOTE-ALLOCATOR* nil "the current QuickTime Music note allocator")
(defvar *process* ())
(defvar *meter* 2)
(defVar *SCORE* ())
(defVar *VARIABLE-WINDOW* ())
(defVar *INSTRUMENTS* (make-hash-table) 
  "hash table <Instrument-Number> => list: <Allocator> <Channel>")
(defVar *NOTE-ALLOCATOR* nil "the current QuickTime Music note allocator")
(defVar *THE-WORK* ())
(defVar *TEMPORARY-NAME* ())
(defVar *TITLE* ())
(defVar *WORK-LIST* ())
(defVar *FILE-NAME* ())
(defVar *INSTRUMENT* 46)
(defVar *TEMPO* 60)
(defVar *KEY* 0)
(defVar *LOUD* 64)
(setq *print-case* :downcase)
(defVar *CHANNEL* 1)
(defvar *CHANNEL-1* 1)
(defVar *INSTR* ())
(defVar *TEMPO-BUTTON* ())
(defVar *KEY-BUTTON* ())
(defVar *LOUD-BUTTON* ())
(defVar *CHANNEL-2* 2)
(defVar *CHANNEL-3* 3)
(defVar *CHANNEL-4* 4)                                 
(defVar *CHANNEL-5* 5)
(defVar *CHANNEL-6* 6)
(defVar *CHANNEL-7* 7)
(defVar *CHANNEL-8* 8)
(defVar *CHANNEL-9* 9)
(defVar *CHANNEL-10* 10)                                 
(defVar *CHANNEL-11* 11)
(defVar *CHANNEL-12* 12)
(defVar *CHANNEL-13* 13)
(defVar *CHANNEL-14* 14)
(defVar *CHANNEL-15* 15)
(defVar *CHANNEL-16* 16)

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

;************************
;   Midi Instruments    *
;************************

(defmacro QTMA-ERROR (Form) "
  in: Form {t}.
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

;***************************
;   Internal Functions     *
;***************************

(defun QUICKTIME-MUSIC-AVAILABLE-P () "
  out: True {Boolean}
  Return true if quicktime music is available"
  ;; this should be more specific since
  ;; presence of quicktime does not imply presence of 
  ;; quicktime music
  (rlet ((Result :pointer))
    (eql (#_Gestalt #$gestaltQuickTime Result) #$noErr)))

;(defVar *NOTE-ALLOCATOR* nil "the current QuickTime Music note allocator")

(defun OPEN-DEFAULT-NOTE-ALLOCATOR () "
  out: Note-Allocator {NoteAllocator}.
  Return a note allocator." 
  (let ((Note-Allocator (#_OpenDefaultComponent :|nota| 0)))
    (when (%null-ptr-p Note-Allocator) (error "QT Music: Could not create note allocator"))
    Note-Allocator))

(defun DEFAULT-NOTE-ALLOCATOR () "
  out: Note-Allocator {NoteAllocator}.
  Return a note allocator. Open one if necessary."
  (or
   *Note-Allocator*
   (setq *Note-Allocator* (open-default-note-allocator))))


;(defvar *note-allocator* ())
(defvar *Note-Channel* ())
(defvar *Note-Request* ())

;;;this sets the instrument to the channel!! and returns pointers to it.

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

(defun INSTRUMENT-NUMBER->INSTRUMENT-NAME (Instrument-Number) "
  in:  Instrument-Number {fixnum}.
  out: Instrument-Name {string}.
  Return the name of the instrument."
  (if (= Instrument-Number 0)
    "instrument undefined"
    (if (quicktime-music-available-p)
      (let ((Note-Allocator (open-default-note-allocator)))
        (rlet ((Note-Request 
                :NoteRequest 
                :info.flags 0
                :info.polyphony 2      ;; two simultaneous tones
                :info.typicalPolyphony #b0010000))
          (let ((&NR.Tone (rref Note-Request :NoteRequest.tone)))
            (QTMA-error (#_NAStuffToneDescription Note-Allocator Instrument-Number &NR.Tone))
            (prog1
              (let ((Name (rref Note-Request :NoteRequest.tone.instrumentName)))
                (if (string-equal Name "")
                  "instrument not found"
                  (copy-seq (rref Note-Request :NoteRequest.tone.instrumentName))))
              (QTMA-error (#_CloseComponent Note-Allocator))))))
      "Quicktime not installed")))

(defvar *request* ())

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

;***********************
;   Play Functions     *
;***********************

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

(defun STOP-THE-MUSIC () "
  Close ALL the currently open Components and NoteChannels.
  This function should be called before the application is quit."
  (maphash
   #'(lambda (Instrument-Number Instrument)
       (declare (ignore Instrument-Number))
       (#_NADisposeNoteChannel (first Instrument) (second Instrument))
       (#_CloseComponent (first Instrument)))
   *Instruments*)
  (clrhash *Instruments*)
  (setq *Note-Allocator* nil))

(defun PLAY-MELODY (Melody &optional (Instrument-Number 1) (Time 0.5))
  (let ((Note-Events nil))
    (dolist (Note Melody)
      (let ((Pitch (if (listp Note) (first Note) Note)))
        (let* ((Now (get-internal-real-time))
               (Stop-Time (+ Now (* (if (listp Note) (* Time 4 (second Note)) Time) internal-time-units-per-second))))
          (play-note Pitch 127 Instrument-Number)
          ;; check if some old sounds are still playing and need to be stopped
          (setq Note-Events
                (mapcan
                 #'(lambda (Event)
                     (cond
                      ;; time to stop
                      ((> Now (first Event))
                       (play-note (rest Event) 0 Instrument-Number)
                       nil)
                      (t (list Event))))
                 Note-Events))
          ;; add the sound just played to note events
          (push (cons (+ (get-internal-real-time) 4000) Pitch) Note-Events)
          ;; wait until it's time for next note
          (loop (when (> (get-internal-real-time) Stop-Time) (return))))))))

#|(defun SCULPT-MUSIC (events)
  "(setq *begin-time-in-beats* 4)
       4
   (SCULPT-MUSIC *notation-events*)
    Calling (sculpt-music ((0 60 1000 4 127) (0 67 1000 2 127) (0 64 1000 3 127)
                        (0 72 1000 1 127) (1000 55 1000 4 127) (1000 67 1000 2 127)...
       sculpt-music returned ((0 50 1000 4 127) (0 57 1000 3 127) (0 74 1000 1 127)
                        (500 66 500 2 127) (1000 52 500 4 127) (1000 64 1000 2 127)..."
  (let* ((delay (* 1000 (+ (1- *begin-time-in-beats*) (* (1- *begin-time-in-measures*) *meter*))))
         (end (* 1000 (+ (1- *end-time-in-beats*) (* (1- *end-time-in-measures*) *meter*))))
         (new-events (loop for event in events
                           if (and (>= (first event) delay)(<= (first event) end))
                           collect event)))
    (loop for new-event in new-events
          collect (cons (- (first new-event) delay)
                        (cdr new-event)))))
|#
(defun delay (events)
  (cons '(0 0 1000 1 0)
    (loop for i in events
          collect (cons (+ (first i) 1000)
                        (rest i)))))

(defun PLAY-EVENTS (events &optional (instrument nil))
  (let ((changed-events (make-play-list (delay events)))
        (instr (if (numberp instrument) instrument (find-instrument-number instrument)))
        (time (get-internal-real-time)))
    (loop until (null changed-events)
          do (if (<= (first (first changed-events)) (- (get-internal-real-time) time))
               (and (play-note (second (first changed-events)) (third (first changed-events)) 
                               (if (null instrument)
                                 (get-channel-instrument (fourth (first changed-events))) instr))
                    (setf changed-events (rest changed-events)))))))

(defun SORTCAR (function lists)
  "Sorts by the first element."
  (sort (copy-tree lists) function :key 'first))

(defun PLAY (events)
  (play-events events))


(defun MAKE-PLAY-LIST (events)
  "Calling (make-play-list
           ((0 60 1000 4 127) (0 67 1000 2 127) (0 64 1000 3 127) (0 72 1000 1 127)...
        make-play-list returned ((0 60 127 4) (0 67 127 2) (0 64 127 3) (0 72 127 1)
                          (1000 60 0 4) (1000 67 0 2) (1000 64 0 3) (1000 72 0 1)..."
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

(defun CONVERT-THE-EVENTS (events)
  "Calling (convert-the-events
           ((0 60 1000 4 127) (0 67 1000 2 127) (0 64 1000 3 127) (0 72 1000 1 127)
            (1000 55 1000 4 127) (1000 67 1000 2 127) (1000 59 1000 3 127)...
       convert-the-events returned ((0 60 1000 4 127) (0 67 1000 2 127) (0 64 1000 3 127)
                              (0 72 1000 1 127) (1000 55 1000 4 127)..."
  (loop for item in events
        if (equal (first item) 'setq) 
        do (eval item)
        else
        collect (list (floor (* (/ 60 *tempo*)(car item)))
                      (let ((note (+ (cadr item) *key*)))
                        (if (< note 26) -1 note))
                      (floor (* (/ 60 *tempo*)(caddr item)))
                      (cadddr item) 
                      (cadddr (cdr item)))))

;*****************************
;   Instrument Accessing     *
;*****************************

(defun FIND-INSTRUMENT-NUMBER (list-name &optional (instr *instruments-list*))
  "Calling (find-instrument-number nil) 
      find-instrument-number returned nil"
  (cond ((null instr)())
        ((equal (first list-name)(third (first instr)))
         (first (first instr)))
        (t (find-instrument-number list-name (cdr instr)))))

(defun GET-INSTRUMENT ()
  "Calling (get-instrument) 
      get-instrument returned 1"
  (eval (read-from-string 
         (format () "~A~A~A" 
                 '*channel- 
                 *channel* 
                 '*))))

(defun MAKE-LIST-INTO-STRING (list)
  "Calling (make-list-into-string (a b c)) 
       make-list-into-string returned a b c"
  (let ((storage ""))
    (loop for element in (butlast list)
          do (setf storage (format () "~A~A~A" storage (write-to-string element) " ")))
    (format () "~A~A" storage (write-to-string (first (last list))))))

(defun FIND-INSTRUMENT-NAME (number &optional (instr *instruments-list*))
  "Calling (find-instrument-name 35) 
       find-instrument-name returned electric bass picked"
  (let ((test (nthcdr 2 (assoc number instr))))
    (if (null test) "acoustic grand piano" (make-list-into-string test))))

(defun GET-CHANNEL-INSTRUMENT (channel-number)
  "Calling (get-channel-instrument 5) 
     get-channel-instrument returned 5"
  (eval (second (assoc channel-number '((1 *channel-1*)(2 *channel-2*)(3 *channel-3*)(4 *channel-4*)
                                        (5 *channel-5*)(6 *channel-6*)(7 *channel-7*)(8 *channel-8*)(9 *channel-9*)(10 *channel-10*)
                                        (11 *channel-11*)(12 *channel-12*)(13 *channel-13*)(14 *channel-14*)(15 *channel-15*)
                                        (16 *channel-16*))))))

;****************
;   Windows     *
;****************


(defvar *strings* '(violin viola cello
                    contrabass tremolo-strings pizzicato-strings orchestral-harp
                    acoustic-string-ensemble acoustic-string-ensemble-2 synth-strings-1
                    synth-strings-2 orchestra-hit))

(defvar *woodwinds* '(flute oboe clarinet bassoon
                      piccolo  English-horn soprano-sax alto-sax tenor-sax
                      baritone-sax recorder))

(defvar *brass* '(French-horn trumpet  trombone  tuba
                  muted-trumpet  brass-section  SynthBrass-1
                  SynthBrass-2))

(defvar *choir* '(ah-choir ooh-choir SynthVox choir))

(defvar *piano* '(acoustic-grand-piano bright-acoustic-piano electric-grand-piano honkytonk-piano Rhodes-piano
                  chorused-piano))

(defvar *hpschd-organ* '(harpsichord Hammond-organ
                         percussive-organ  rock-organ  church-organ  reed-organ
                         accordian  harmonica  tango-accordian  ))

(defvar *percussion* '(timpani celesta  glockenspiel  vibraphone
                       marimba  xylophone  tubular-bells melodic-tom bottle-blow  music-box  whistle woodblock))

(defvar *guitar* '(acoustic-nylon-guitar
                   acoustic-steel-guitar  electric-jazz-guitar  electric-clean-guitar
                   electric-guitar-muted  overdriven-guitar  distortion-guitar  guitar-harmonics
                   acoustic-fretless-bass  electri-bass-fingered  electric-bass-picked
                   fretless-bass  slap-bass-1  slap-bass-2  synth-bass-1
                   synth-bass-2))

(defvar *kits* '(standard-kit  room-kit 
                 power-kit electronic-kit analog-kit   brush-kit 
                 orchestra-kit))

(defvar *effects* '(breathnoise
                    seashore  bird-tweet  telephone-ring  helicopter
                    applause  gunshot reverse-cymbal  guitar-fret-noise bowed  metal  halo
                    sweep  ice-rain  sound-tracks  crystal
                    atmosphere  brightness  goblins  echos space tinkle-bell))

(defvar *electronic* '(square-wave  saw-wave  calliope
                       chiffer  charang  solo-vox  th-saw-wave
                       bass-and-lead  fantasy  warm  polysynth synth-drum))

(defvar *world* '(sitar  banjo  shamisen  koto
                  kalimba  bagpipe  fiddle  shannai
                  agogo  steel-drums Shakuhachi Taiko-drum clavinet  dulcimer ocarina pan-flute))
(defClass instrument-WINDOW (window)
  nil
  (:default-initargs :window-type :document
    :window-title "Instrument Selection"
    :view-position #@(190 110)
    :view-size #@(250 210)))

(defvar *instrument-number* 0)

(defMethod INITIALIZE-INSTANCE ((window instrument-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs)
  (add-subviews window 
    (make-dialog-item 
     'button-dialog-item
     #@(16 45) #@(104 18)
     "Strings"
     #'(lambda (item) 
         item 
         (setq *instrument-number* (eval (first (select-item-from-list *Strings*))))
         ;(window-close window)
         (return-from-modal-dialog window))
     
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(126 15) #@(104 18)
     "Woodwinds"
     #'(lambda (item) 
         item 
         (setq *instrument-number* (eval (first (select-item-from-list *woodwinds*))))
         ;(window-close window)
         (return-from-modal-dialog window))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(126 75) #@(104 18)
     "Brass"
     #'(lambda (item) 
         item (setq *instrument-number* (eval (first (select-item-from-list *brass*))))(window-close window))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 105) #@(104 18)
     "Choir"
     #'(lambda (item) 
         item 
         (setq *instrument-number* (eval (first (select-item-from-list *choir*))))(window-close window))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 15) #@(104 18)
     "Piano"
     #'(lambda (item) 
         item 
         (setq *instrument-number*  (eval (first (select-item-from-list *piano*))))(window-close window))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(126 45) #@(104 18)
     "Hpschd/Organ"
     #'(lambda (item) 
         item 
         (setq *instrument-number* (eval (first (select-item-from-list *hpschd-organ*))))(window-close window))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 75) #@(104 18)
     "Percussion"
     #'(lambda (item) 
         item 
         (setq *instrument-number* (eval (first (select-item-from-list *percussion*))))(window-close window))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(126 105) #@(104 18)
     "Guitar"
     #'(lambda (item) 
         item 
         (setq *instrument-number* (eval (first (select-item-from-list *guitar*))))(window-close window))
     :view-font '("geneva" 10 :bold))
    
    (make-dialog-item 
     'button-dialog-item
     #@(16 135) #@(104 18)
     "Kits"
     #'(lambda (item) 
         item 
         (setq *instrument-number* (eval (first (select-item-from-list *kits*))))(window-close window))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(126 135) #@(104 18)
     "Effects"
     #'(lambda (item) 
         item 
         (setq *instrument-number* (eval (first (select-item-from-list *effects*))))(window-close window))
     :view-font '("geneva" 10 :bold))
    
    (make-dialog-item 
     'button-dialog-item
     #@(16 165) #@(104 18)
     "Electronic"
     #'(lambda (item) 
         item 
         (setq *instrument-number* (eval (first (select-item-from-list *electronic*))))
         (window-close window))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(126 165) #@(104 18)
     "World"
     #'(lambda (item) 
         item 
         (setq *instrument-number* (eval (first (select-item-from-list *world*))))(window-close window))
     :view-font '("geneva" 10 :bold))))

(setq acoustic-grand-piano 1 bright-acoustic-piano 2 electric-grand-piano 3 honkytonk-piano 4 Rhodes-piano
      5 chorused-piano 6 harpsichord 7 clavinet 8 celesta 9 glockenspiel 10 music-box 11 vibraphone
      12 marimba 13 xylophone 14 tubular-bells 15 dulcimer 16 Hammond-organ
      17 percussive-organ 18 rock-organ 19 church-organ 20 reed-organ
      21 accordian 22 harmonica 23 tango-accordian 24 acoustic-nylon-guitar
      25 acoustic-steel-guitar 26 electric-jazz-guitar 27 electric-clean-guitar
      28 electric-guitar-muted 29 overdriven-guitar 30 distortion-guitar 31 guitar-harmonics
      32 acoustic-fretless-bass 33 electri-bass-fingered 34 electric-bass-picked
      35 fretless-bass 36 slap-bass-1 37 slap-bass-2 38 synth-bass-1
      39 synth-bass-2 40 violin 41 viola 42 cello
      43 contrabass 44 tremolo-strings 45 pizzicato-strings 46 orchestral-harp
      47 timpani 48 acoustic-string-ensemble 49 acoustic-string-ensemble-2 50 synth-strings-1
      51 synth-strings-2 52 ah-choir 53 ooh-choir 54 SynthVox
      55 orchestra-hit 56 trumpet 57 trombone 58 tuba
      59 muted-trumpet 60 French-horn 61 brass-section 62 SynthBrass-1
      63 SynthBrass-2 64 soprano-sax 65 alto-sax 66 tenor-sax
      67 baritone-sax 68 oboe 69 English-horn 70 bassoon
      71 clarinet 72 piccolo 73 flute 74 recorder
      75 pan-flute 76 bottle-blow 77 Shakuhachi 78 whistle
      79 ocarina 80 square-wave 81 saw-wave 82 calliope
      83 chiffer 84 charang 85 solo-vox 86 th-saw-wave
      87 bass-and-lead 88 fantasy 89 warm 90 polysynth
      91 choir 92 bowed 93 metal 94 halo
      95 sweep 96 ice-rain 97 sound-tracks 98 crystal
      99 atmosphere 100 brightness 101 goblins 102 echos
      103 space 104 sitar 105 banjo 106 shamisen 107 koto
      108 kalimba 109 bagpipe 110 fiddle 111 shannai 112 tinkle-bell
      113 agogo 114 steel-drums 115 woodblock 116 Taiko-drum 117 melodic-tom
      118 synth-drum 119 reverse-cymbal 120 guitar-fret-noise 121 breathnoise
      122 seashore 123 bird-tweet 124 telephone-ring 125 helicopter
      126 applause 127 gunshot 128  standard-kit 16385 room-kit 16393
      power-kit 16401  electronic-kit 16409 analog-kit 16410  brush-kit 16425
      orchestra-kit 16433)

(defClass PLAY-WINDOW (window)
  nil
  (:default-initargs :window-type :document
    :window-title "EMI MIDI Variables"
    :view-position #@(90 110)
    :view-size #@(250 170)))

(defMethod INITIALIZE-INSTANCE ((window PLAY-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs)
  (add-subviews window 
    (make-instance 'pop-up-menu
      :menu-title ""
      :view-font '("geneva" 10 :bold) ;'("Osaka" 10 :SRCOR :PLAIN (:COLOR-INDEX 0))
      :view-position #@(16 15)
      :highlight-title t
      :menu-items
      (list
       (make-instance 'menu-item
         :menu-item-title "Channel 1"
         :menu-item-action #'(lambda () (setq *channel* 1)
                              (set-dialog-item-text
                               (find-named-sibling *instr* 'instrument-display)
                               (find-instrument-name 
                                (eval
                                 (read-from-string (format () "~A~A~A"
                                                           "*channel-" *channel* "*")))))))
       (make-instance 'menu-item
         :menu-item-title "Channel 2"
         :menu-item-action #'(lambda () (setq *channel* 2)
                              (set-dialog-item-text
                               (find-named-sibling *instr* 'instrument-display)
                               (find-instrument-name 
                                (eval
                                 (read-from-string (format () "~A~A~A"
                                                           "*channel-" *channel* "*")))))))
       (make-instance 'menu-item
         :menu-item-title "Channel 3"
         :menu-item-action #'(lambda () (setq *channel* 3)
                              (set-dialog-item-text
                               (find-named-sibling *instr* 'instrument-display)
                               (find-instrument-name 
                                (eval
                                 (read-from-string (format () "~A~A~A"
                                                           "*channel-" *channel* "*")))))))
       (make-instance 'menu-item
         :menu-item-title "Channel 4"
         :menu-item-action #'(lambda () (setq *channel* 4)
                              (set-dialog-item-text
                               (find-named-sibling *instr* 'instrument-display)
                               (find-instrument-name 
                                (eval
                                 (read-from-string (format () "~A~A~A"
                                                           "*channel-" *channel* "*")))))))
       (make-instance 'menu-item
         :menu-item-title "Channel 5"
         :menu-item-action #'(lambda () (setq *channel* 5)
                              (set-dialog-item-text
                               (find-named-sibling *instr* 'instrument-display)
                               (find-instrument-name 
                                (eval
                                 (read-from-string (format () "~A~A~A"
                                                           "*channel-" *channel* "*")))))))
       (make-instance 'menu-item
         :menu-item-title "Channel 6"
         :menu-item-action #'(lambda () (setq *channel* 6)
                              (set-dialog-item-text
                               (find-named-sibling *instr* 'instrument-display)
                               (find-instrument-name 
                                (eval
                                 (read-from-string (format () "~A~A~A"
                                                           "*channel-" *channel* "*")))))))
       (make-instance 'menu-item
         :menu-item-title "Channel 7"
         :menu-item-action #'(lambda () (setq *channel* 7)
                              (set-dialog-item-text
                               (find-named-sibling *instr* 'instrument-display)
                               (find-instrument-name 
                                (eval
                                 (read-from-string (format () "~A~A~A"
                                                           "*channel-" *channel* "*")))))))
       (make-instance 'menu-item
         :menu-item-title "Channel 8"
         :menu-item-action #'(lambda () (setq *channel* 8)
                              (set-dialog-item-text
                               (find-named-sibling *instr* 'instrument-display)
                               (find-instrument-name 
                                (eval
                                 (read-from-string (format () "~A~A~A"
                                                           "*channel-" *channel* "*")))))))
       (make-instance 'menu-item
         :menu-item-title "Channel 9"
         :menu-item-action #'(lambda () (setq *channel* 9)
                              (set-dialog-item-text
                               (find-named-sibling *instr* 'instrument-display)
                               (find-instrument-name 
                                (eval
                                 (read-from-string (format () "~A~A~A"
                                                           "*channel-" *channel* "*")))))))
       (make-instance 'menu-item
         :menu-item-title "Channel 10"
         :menu-item-action #'(lambda () (setq *channel* 10)
                              (set-dialog-item-text
                               (find-named-sibling *instr* 'instrument-display)
                               (find-instrument-name 
                                (eval
                                 (read-from-string (format () "~A~A~A"
                                                           "*channel-" *channel* "*")))))))
       (make-instance 'menu-item
         :menu-item-title "Channel 11"
         :menu-item-action #'(lambda () (setq *channel* 11)
                              (set-dialog-item-text
                               (find-named-sibling *instr* 'instrument-display)
                               (find-instrument-name 
                                (eval
                                 (read-from-string (format () "~A~A~A"
                                                           "*channel-" *channel* "*")))))))
       (make-instance 'menu-item
         :menu-item-title "Channel 12"
         :menu-item-action #'(lambda () (setq *channel* 12)
                              (set-dialog-item-text
                               (find-named-sibling *instr* 'instrument-display)
                               (find-instrument-name 
                                (eval
                                 (read-from-string (format () "~A~A~A"
                                                           "*channel-" *channel* "*")))))))
       (make-instance 'menu-item
         :menu-item-title "Channel 13"
         :menu-item-action #'(lambda () (setq *channel* 13)
                              (set-dialog-item-text
                               (find-named-sibling *instr* 'instrument-display)
                               (find-instrument-name 
                                (eval
                                 (read-from-string (format () "~A~A~A"
                                                           "*channel-" *channel* "*")))))))
       (make-instance 'menu-item
         :menu-item-title "Channel 14"
         :menu-item-action #'(lambda () (setq *channel* 14)
                              (set-dialog-item-text
                               (find-named-sibling *instr* 'instrument-display)
                               (find-instrument-name 
                                (eval
                                 (read-from-string (format () "~A~A~A"
                                                           "*channel-" *channel* "*")))))))
       (make-instance 'menu-item
         :menu-item-title "Channel 15"
         :menu-item-action #'(lambda () (setq *channel* 15)
                              (set-dialog-item-text
                               (find-named-sibling *instr* 'instrument-display)
                               (find-instrument-name 
                                (eval
                                 (read-from-string (format () "~A~A~A"
                                                           "*channel-" *channel* "*")))))))
       (make-instance 'menu-item
         :menu-item-title "Channel 16"
         :menu-item-action #'(lambda () (setq *channel* 16)
                              (set-dialog-item-text
                               (find-named-sibling *instr* 'instrument-display)
                               (find-instrument-name 
                                (eval
                                 (read-from-string (format () "~A~A~A"
                                                           "*channel-" *channel* "*")))))))))
    (setq *instr* 
          (make-instance 'static-text-dialog-item
            :dialog-item-text 
            (find-instrument-name 
             (eval
              (read-from-string (format () "~A~A~A"
                                        "*channel-" *channel* "*"))))
            :view-position #@(126 19)
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
                   ;(user-pick-instrument (get-instrument))
                   (progn (modal-dialog (make-instance 'instrument-window)) *instrument-number*))
         (set-dialog-item-text
          (find-named-sibling item 'instrument-display)
          (find-instrument-name 
           (eval
            (read-from-string (format () "~A~A~A"
                                      "*channel-" *channel* "*"))))))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(126 45) #@(104 18)
     "Set Instruments"
     #'(lambda (item) 
         item 
         ;(let ((test (user-pick-instrument 0)))
         ;(if (not (zerop test))
         (progn (setq *instrument* 
                      ;(user-pick-instrument *instrument*)
                      (progn (modal-dialog (make-instance 'instrument-window)) *instrument-number*))
                (setq *channel-1* *instrument* *channel-2* *instrument* *channel-3* *instrument* 
                      *channel-4* *instrument* *channel-5* *instrument* *channel-6* *instrument* *channel-7* *instrument* 
                      *channel-8* *instrument* *channel-9* *instrument* *channel-10* *instrument* *channel-11* *instrument* 
                      *channel-12* *instrument* *channel-13* *instrument* *channel-14* *instrument* *channel-15* *instrument* 
                      *channel-16* *instrument*)
                (set-dialog-item-text
                 (find-named-sibling item 'instrument-display)
                 (find-instrument-name *instrument*))))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(126 105) #@(104 18)
     "Stop"
     #'(lambda (item) 
         item (if *process* (progn (process-kill *process*)(stop-the-music))))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 105) #@(104 18)
     "Loudness"
     #'(lambda (item) 
         item (if *loud-button* (window-close *loud-button*))
         (setq *loud-button* (make-instance 'loud-window)))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 75) #@(104 18)
     "Tempo"
     #'(lambda (item) 
         item (if *tempo-button* (window-close *tempo-button*))
         (setq *tempo-button* (make-instance 'tempo-window)))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(126 75) #@(104 18)
     "Transpose"
     #'(lambda (item) 
         item (if *key-button* (window-close *key-button*))
         (setq *key-button* (make-instance 'key-window)))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(16 135) #@(104 18)
     "Region"
     #'(lambda (item) 
         item (if *region-button* (window-close *region-button*))
         (setq *region-button* (make-instance 'region-window)))
     :view-font '("geneva" 10 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(126 135) #@(104 18)
     "Done"
     #'(lambda (item) 
         item 
         (window-close window))
     :view-font '("geneva" 10 :bold))))

(defVar *TEMPO-SCROLL* (make-instance
                         'scroll-bar-dialog-item
                         :view-position #@(102 18)
                         :direction :horizontal
                         :length 120
                         :setting *tempo*
                         :max 180
                         :min 1
                         :dialog-item-action
                         #'(lambda (item &aux (setting (format nil 
                                                               "~a"
                                                               (scroll-bar-setting 
                                                                item))))
                             (set-dialog-item-text
                              (find-named-sibling item 'tempo-display)
                              setting)
                             (setq *tempo* (read-from-string setting))
                             (window-update-event-handler (view-window item)))))







(defClass TEMPO-WINDOW (window)
  nil
  (:default-initargs :window-type :document
    :window-title ""
    :view-position #@(10 400)
    :view-size #@(300 50)))

(defMethod INITIALIZE-INSTANCE ((window TEMPO-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs)
  (set-scroll-bar-setting *tempo-scroll* *tempo*)
  (add-subviews window
    (make-instance 'static-text-dialog-item
      :dialog-item-text (write-to-string *tempo*)
      :view-position #@(244 20)
      :view-size #@(32 15)
      :view-nick-name 'tempo-display
      :view-font '("palatino" 12 :bold)) 
    (make-instance 'static-text-dialog-item
      :dialog-item-text "Set Tempo:"
      :view-position #@(24 20)
      :view-size #@(82 15)
      :view-nick-name 'tempo-display
      :view-font '("palatino" 12 :bold))
    *tempo-scroll*))

(defVar *KEY-SCROLL* (make-instance
                       'scroll-bar-dialog-item
                       :view-position #@(102 18)
                       :direction :horizontal
                       :length 120
                       :setting *key*
                       :max 20
                       :min -20
                       :dialog-item-action
                       #'(lambda (item &aux (setting (format nil 
                                                             "~a"
                                                             (scroll-bar-setting 
                                                              item))))
                           (set-dialog-item-text
                            (find-named-sibling item 'key-display)
                            setting)
                           (setq *key* (read-from-string setting))
                           (window-update-event-handler (view-window item)))))

(defClass KEY-WINDOW (window)
  nil
  (:default-initargs :window-type :document
    :window-title ""
    :view-position #@(110 200)
    :view-size #@(300 50)))

(defMethod INITIALIZE-INSTANCE ((window KEY-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs)
  (set-scroll-bar-setting *key-scroll* *key*)
  (add-subviews window
    (make-instance 'static-text-dialog-item
      :dialog-item-text (write-to-string *key*)
      :view-position #@(244 20)
      :view-size #@(32 15)
      :view-nick-name 'key-display
      :view-font '("palatino" 12 :bold)) 
    (make-instance 'static-text-dialog-item
      :dialog-item-text "Transpose:"
      :view-position #@(24 20)
      :view-size #@(82 15)
      :view-nick-name 'key-display
      :view-font '("palatino" 12 :bold))
    *key-scroll*))

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

(defVar *BEGINMEA-SCROLL* (make-instance
                            'scroll-bar-dialog-item
                            :view-position #@(102 18)
                            :direction :horizontal
                            :length 120
                            :setting *begin-time-in-measures*
                            :max 100
                            :min 1
                            :dialog-item-action
                            #'(lambda (item &aux (setting (format nil 
                                                                  "~a"
                                                                  (scroll-bar-setting 
                                                                   item))))
                                (set-dialog-item-text
                                 (find-named-sibling item 'beginmea-display)
                                 setting)
                                (setq *begin-time-in-measures* (read-from-string setting))
                                (window-update-event-handler (view-window item)))))

(defVar *BEGINBEA-SCROLL* (make-instance
                            'scroll-bar-dialog-item
                            :view-position #@(102 48)
                            :direction :horizontal
                            :length 120
                            :setting *begin-time-in-beats*
                            :max 100
                            :min 1
                            :dialog-item-action
                            #'(lambda (item &aux (setting (format nil 
                                                                  "~a"
                                                                  (scroll-bar-setting 
                                                                   item))))
                                (set-dialog-item-text
                                 (find-named-sibling item 'beginbea-display)
                                 setting)
                                (setq *begin-time-in-beats* (read-from-string setting))
                                (window-update-event-handler (view-window item)))))

(defvar *ENDMEA-SCROLL* (make-instance
                          'scroll-bar-dialog-item
                          :view-position #@(102 78)
                          :direction :horizontal
                          :length 120
                          :setting *end-time-in-measures*
                          :max 1000
                          :min 1
                          :dialog-item-action
                          #'(lambda (item &aux (setting (format nil 
                                                                "~a"
                                                                (scroll-bar-setting 
                                                                 item))))
                              (set-dialog-item-text
                               (find-named-sibling item 'endmea-display)
                               setting)
                              (setq *end-time-in-measures* (read-from-string setting))
                              (window-update-event-handler (view-window item)))))

(defVar *ENDBEA-SCROLL* (make-instance
                          'scroll-bar-dialog-item
                          :view-position #@(102 108)
                          :direction :horizontal
                          :length 120
                          :setting *end-time-in-beats*
                          :max 100
                          :min 1
                          :dialog-item-action
                          #'(lambda (item &aux (setting (format nil 
                                                                "~a"
                                                                (scroll-bar-setting 
                                                                 item))))
                              (set-dialog-item-text
                               (find-named-sibling item 'endbea-display)
                               setting)
                              (setq *end-time-in-beats* (read-from-string setting))
                              (window-update-event-handler (view-window item)))))

(defClass REGION-WINDOW (window)
  nil
  (:default-initargs :window-type :document
    :window-title ""
    :view-position #@(200 100)
    :view-size #@(300 160)))

(defMethod INITIALIZE-INSTANCE ((window REGION-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs)
  (set-scroll-bar-setting *beginmea-scroll* *begin-time-in-measures*)
  (add-subviews window
    (make-instance 'static-text-dialog-item
      :dialog-item-text (write-to-string *begin-time-in-measures*)
      :view-position #@(244 20)
      :view-size #@(32 15)
      :view-nick-name 'beginmea-display
      :view-font '("palatino" 12 :bold)) 
    (make-instance 'static-text-dialog-item
      :dialog-item-text "Begin Measure:"
      :view-position #@(4 20)
      :view-size #@(122 15)
      :view-nick-name 'beginmea-display
      :view-font '("palatino" 12 :bold))
    (make-instance 'static-text-dialog-item
      :dialog-item-text (write-to-string *begin-time-in-beats*)
      :view-position #@(244 50)
      :view-size #@(32 15)
      :view-nick-name 'beginbea-display
      :view-font '("palatino" 12 :bold)) 
    (make-instance 'static-text-dialog-item
      :dialog-item-text "Begin Beat:"
      :view-position #@(4 50)
      :view-size #@(122 15)
      :view-nick-name 'beginbea-display
      :view-font '("palatino" 12 :bold))
    (make-instance 'static-text-dialog-item
      :dialog-item-text (write-to-string *end-time-in-measures*)
      :view-position #@(244 80)
      :view-size #@(32 15)
      :view-nick-name 'endmea-display
      :view-font '("palatino" 12 :bold)) 
    (make-instance 'static-text-dialog-item
      :dialog-item-text "End Measure:"
      :view-position #@(4 80)
      :view-size #@(122 15)
      :view-nick-name 'endmea-display
      :view-font '("palatino" 12 :bold))
    (make-instance 'static-text-dialog-item
      :dialog-item-text (write-to-string *end-time-in-beats*)
      :view-position #@(244 110)
      :view-size #@(32 15)
      :view-nick-name 'endbea-display
      :view-font '("palatino" 12 :bold)) 
    (make-instance 'static-text-dialog-item
      :dialog-item-text "End Beat:"
      :view-position #@(4 110)
      :view-size #@(122 15)
      :view-nick-name 'endbea-display
      :view-font '("palatino" 12 :bold))
    (make-dialog-item 
     'button-dialog-item
     #@(96 130) #@(104 24)
     "Done"
     #'(lambda (item) 
         item 
         (window-close window))
     :view-font '("geneva" 10 :bold))
    *beginmea-scroll*
    *beginbea-scroll*
    *endmea-scroll*
    *endbea-scroll*))

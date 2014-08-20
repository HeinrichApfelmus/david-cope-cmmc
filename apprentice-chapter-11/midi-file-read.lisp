
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;        Apprentice/Chapter 11        ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;    simple code to run Apprentice    ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;


(defVar *PAGE-FLOATING-WINDOW* ())
(setq *end-time-in-measures* 1000)
(setq *end-time-in-beats* 1000)
(defVar *TEMPO-OVERRIDE* ())
(defVar *INSTRUMENT-OVERRIDE* ())
(defVar *COMPOSER* 'bach)
(defVar *CURRENT-MIDI-FILE* nil)
(setq *print-pretty* t)
(defVar *BEAT-SIZE-FOR-COMPOSING* 2000)
(defVar *DISKLAVIAR-MIDI* ())
(defVar *SAVE-TEMPO* 60)
(defVar *DIRECTION* t)
(defVar *PERFORM* ())
(defVar *RHYTHM-DEVIATION* 15)
(defVar *DYNAMIC-DEVIATION* 10)
(defVar *TEMPO-DEVIATION* 7)
(defVar *DATA* ())
(defVar *CURRENT-GESTURES* ())
(defVar *MUSIC* ())
(defConstant $NOTEOFF #x80)
(defConstant $NOTEON #x90)
(defVar *ALGORITHMIC-COMPOSITION* ())
(defVar IT () "The datastorage for MIDI file input.")
;;;;;
#|(get-filename 1PMacintosh HD:applications:MCL 4.0:Alice:databases:bach:bach-chorale-64-a-1) 
      bach-chorale-64-a-1|#
;;;;;

(defun GET-FILENAME (pathname)
  "Returns the filename from a pathname."
  (read-from-string 
   (mac-namestring 
    (file-namestring pathname))))

(defun TRANSLATE-MIDI-FILE ()
  "Calling (translate-midi-file) 
     translate-midi-file returned ((4000 27 600 3 80) (4000 39 600 3 80)
                               (5000 27 600 3 80) (5000 39 600 3 80)
                               (6000 27 600 3 80) (6000 39 600 3 80)..."
  (setq *data* nil)
  (setq *current-gestures* nil)
  (let ((test (load-from-midi-menu-item-action)))
    (if test
      (print (prepare-midi-file-read)))
    ))

(defun PAIR-LISTS (lists)
  "(pair-1 '((1 2) (a b)(3 4)(5 6)(7 8)))
        ((1 a 3 5 7) (2 b 4 6 8))"
  (if (null (very-first lists)) nil
      (cons (mapcar 'car lists)
            (pair-lists (mapcar 'cdr lists)))))

(defun PREPARE-MIDI-FILE-READ nil
  "(prepare-midi-file-read) 
        prepare-midi-file-read returned ((0 48 1000 4 64) (1000 55 1000 4 64)
                                  (2000 48 2000 4 64) (4000 55 1000 4 64) . . ."
  (apply #'append (mapcar #'pair-lists ;(setq *temp-lists* 
                          (prepare-midi *current-gestures*))))

(defun PREPARE-MIDI (gestures)
  "used in prepare-midi-file-read this function translates gestures into lists of
      on-times, Alice-notes, durations, channel numbers and dynamics:
      calling (prepare-midi (#<*gesture* #x50a471> #<*gesture* #x509dd9>
                         #<*gesture* #x509c29> #<*gesture* #x509a69>)) 
       prepare-midi returned (((0 1000 2000 4000 5000 6000) (48 55 48 55 55 48)
                          (1000 1000 2000 1000 1000 2000) (4 4 4 4 4 4)
                          (64 64 64 64 64 64))"
  (if (null gestures) nil
      (cons (list
             (mapcar #'(lambda (x)(slot-value x 'time-offset)) (slot-value (eval (first gestures)) 'Alice-notes))
             (mapcar #'(lambda (x)(slot-value x 'data1)) (slot-value (eval (first gestures)) 'Alice-notes))
             (mapcar #'(lambda (x)(slot-value x 'duration)) (slot-value (eval (first gestures)) 'Alice-notes))
             (mapcar #'(lambda (x)(slot-value x 'channel)) (slot-value (eval (first gestures)) 'Alice-notes))
             (mapcar #'(lambda (x)(slot-value x 'data2)) (slot-value (eval (first gestures)) 'Alice-notes)))
            (prepare-midi (cdr gestures)))))

(defClass *MIDI-EVENT* nil
  ((opcode    :initarg :opcode
              :initform #x80  ;note-off
              :accessor opcode)
   (channel   :initarg :channel   ;a number from 1 - 16  (not 0-15)
              :initform 1
              :accessor channel)
   (data1     :initarg :data1
              :initform 0
              :accessor data1)
   (data2     :initarg :data2
              :initform 0
              :accessor data2)
   (duration  :initarg :duration
              :initform 0
              :accessor duration)
   (time-offset :initarg :time-offset
                :initform 0
                :accessor time-offset))
  (:documentation "the building block of all midi data sequences."))

;****************
;   Constants   *
;****************

(defConstant $MFF-BUFFER-SIZE 13600)
(defConstant $POLYPRESSURE #xa0)
(defConstant $CONTROLLER #xb0)
(defConstant $PROGRAM #xc0)
(defConstant $CHANNELPRESSURE #xd0)
(defConstant $PITCHWHEEL #xe0)
(defConstant $OPCODE-ALIST '((#x90 . Alice-notes) (#xa0 . polypressures)
                             (#xb0 . controllers) (#xc0 . programs)
                             (#xd0 . channelpressures) (#xe0 . pitchwheels)
                             (#x80 . Alice-notes) ))
(defConstant $OPCODE-NICKNAME-ALIST '((#x90 . "noteon") (#x80 . "noteof") (#xa0 . "polyp")
                                      (#xb0 . "cntlr") (#xc0 . "prgrm")
                                      (#xd0 . "chanpr") (#xe0 . "ptchwl")))
(defConstant $EVENT-SLOT-NAMES
  '(opcode channel data1 data2 time-offset duration))
(defConstant $EVENT-SLOT-NAME-STRINGS
  '("opcode" "channel" "data1" "data2" "time-offset" "duration"))
(defConstant $OPCODE-NAMES '("noteon" "polypressure" "controller" "program"
                             "channelpressure" "pitchwheel"))
(defConstant $GESTURE-SLOT-NAME-STRINGS '("Alice-notes" "polypressures" "controllers"
                                          "programs" "channelpressures" "pitchwheels"))
(defVar *CURRENT-GESTURE* nil)   ;the current gesture being used and manipulated.
(defVar *UNDO-LIST* 'nil)   ;the list of copies of the gestures for which one or more slots were
;changed by the last command
(defVar *EVENTS-CLIPBOARD* 'nil)  ;holds a list of *midi-event*s for cut-events, copy-events,
;and paste-events
(defVar *SEARCH-FN-LIST* '(above below between among equal-to not-between all none
                           random-value))
(defVar *SEARCH-FN-NAME-STRINGS* '("above" "below" "between" "among" "equal-to"
                                   "not-between" "anything" "nothing" "random-chosen"))
;all pre-defined predicate function names for use with sieve
(defVar *TRANSFORM-FN-LIST*
  '(replacing adding averaging multiplying accelerating decelerating))
;;all predefined functions for use with transform-events
(defVar *TRANSFORM-FN-NAME-STRINGS* '("replacing" "incrementing" "rel-incr-ing" "averaging" "multiplying"
                                      "accelerating" "decelerating"))

;;; here are constants representing some standard midi controller numbers

(defConstant $MODULATION-WHEEL 1)
(defConstant $BREATH-CONTROLLER 2)
(defConstant $FOOT-PEDAL 4)
(defConstant $VOLUME-PEDAL 7)
(defConstant $SUSTAIN-PEDAL 64)
(defConstant $DATA2-EVENTS '(#x80 #x90 #xa0 #xb0 #xe0))
(defConstant $DATA1-EVENTS '(#xc0 #xd0))
(defConstant $END-OF-TRACK '(#xff #x2f #x00))
(defConstant $TRACK-CHUNK-TYPE '(77 84 114 107)) ;ascii codes of "mtrk"
(defConstant $TEMPO-WORD  '(84 101 109 112 111)) ;ascii codes of "tempo"
(defConstant $HEADER-CHUNK-TYPE '(77 84 104 100)) ;ascii codes of "mthd"
(defConstant $GESTURE-SLOTS '(Alice-notes polypressures controllers programs
                              channelpressures pitchwheels))
(defConstant $META-EVENTS-ALIST '((00 . sequence-number) (01 . text-event) (02 . copyright)
                                  (03 . track-name) (04 . instrument-name) (05 . lyric)
                                  (06 . marker) (07 . Alice-point) (#x20 . channel-prefix)
                                  (#x2f . end-of-track) (#x51 . set-tempo) (#x54 . smpte-offset)
                                  (#x58 . time-signature) (#x59 . key-signature)
                                  (#x7f . sequencer-specific)))
(defConstant $GESTURE-SLOT-ALIST '((#x80 . noteoff) (#x90 . Alice-notes) (#xa0 . pollypressures)
                                   (#xb0 . controllers) (#xc0 . programs) (#xd0 . channelpressures)
                                   (#xe0 . pitchwheels)))

;*************
;   Basics   *
;*************

(defun GET-MULTI-BYTE-VALUE (number-of-bytes buffer pos paramblock)
  (let ((result-pos (* number-of-bytes 8)) (result 0) the-byte)
    (loop for i from 1 to number-of-bytes
          do
          (setq result-pos (- result-pos 8))
          (multiple-value-setq (pos the-byte) (MFF-cycling-get-byte buffer pos paramblock))
          (setq result (dpb the-byte (byte 8 result-pos) result)))
    (cons pos result)))

(defun MFF-CYCLING-GET-BYTE (buffer pos paramblock)
  ;;if the position is about to run over the edge of the buffer,
  (if (= pos $MFF-buffer-size)
    (progn
      ;;read more of the file in.  if the rest of the file is smaller
      ;;than the size of the buffer, only read the rest of the file.
      ;;if it is larger than the buffer, only read in the size of the 
      ;;buffer
      (if (< (- (geteof paramblock) (getfpos paramblock)) $MFF-buffer-size)
        (fsread paramblock (- (geteof paramblock) (getfpos paramblock)) buffer)
        (fsread paramblock $MFF-buffer-size buffer))
      (setq pos 0)))
  (values (+ pos 1) (%get-byte buffer pos)))

(defun ADD-DURATION (note time-offset pending-durations)
  (let ((found nil) (new-pending-durations nil))
    ;;keep looking through the list of Alice-notes waiting for durations until
    ;;the note-on corresponding to the note-off is found
    (loop until found
          do
          ;;if the pending-durations list is empty, either the whole list
          ;;has been traversed, or there were no Alice-notes waiting for durations
          ;;so ignore this note-off
          (cond ((eq pending-durations nil)
                 (setq found t))
                ;;if the pitch of the note-off matches the pitch of the
                ;;current note-on in the list, then use the time-offset of
                ;;the note-off to compute the duration of the note-on
                ((= note (data1 (car pending-durations)))
                 (setf (duration (car pending-durations))
                       (- time-offset (time-offset (car pending-durations))))
                 ;;construct a new pending-durations list containing all the
                 ;;note-ons of the old one except the one which just received
                 ;;its duration.
                 (setq new-pending-durations (append new-pending-durations
                                                     (cdr pending-durations)))
                 (setq found t))
                (t
                 ;;add the current note to the new-pending-durations list and
                 ;;take it off the beginning of the old one.
                 (setq new-pending-durations (append new-pending-durations (list (car pending-durations))))
                 (setq pending-durations (cdr pending-durations)))))
    ;;return the new-pending-durations list
    new-pending-durations))

(defun READ-TIME-TAG (buffer pos paramblock)
  (let (the-byte)
    (multiple-value-setq (pos the-byte) (MFF-cycling-get-byte buffer pos paramblock))
    (loop with result = 0 
          for byte = (logand the-byte #xff)
          do 
          (setf result (+ (ash result 7)
                          (logand byte #x7f)))
          while (logtest #x80 byte)
          do
          (multiple-value-setq (pos the-byte) (MFF-cycling-get-byte buffer pos paramblock))
          finally (return (cons pos result)))))

(defun INTERPRET-DATA (event-type delta-time opcode data)
  ;; the (ldb (byte 4 0) opcode) statement looks in the first four bits of the
  ;; opcode to get the channel.
  ;; the (dpb 0 (byte 4 0) opcode) sets the first four bits of the opcode to zero
  ;; so that the function will later be able to compare the opcode with the
  ;; opcode constants.
  (if (not (listp data))(interpret-data event-type delta-time opcode nil)
      (let ((channel (ldb (byte 4 0) opcode)) (opcode (dpb 0 (byte 4 0) opcode)) (the-name nil)
            the-event)
        ;;if this is a regular channel message, create an appropriate *midi-event*
        (cond ((eq event-type 'sysex)
               (setq the-event (make-instance '*sysex-event* :name "no name"
               	                              :data data)))
              ((or (member event-type $gesture-slots) (eq event-type 'noteoff))
               ;;if this event has two data slots use the two bytes in the raw data
               (if (member (dpb 0 (byte 4 0) opcode) $data2-events)
                 (setq the-event (make-instance '*midi-event* :opcode opcode
                                                :channel (1+ channel)
                                                :data1 (car data) :data2 (cadr data)
                                                :duration 0 :time-offset delta-time))
                 ;; otherwise this event only has one data slot, so only use the one byte from
                 ;; the raw data
                 (setq the-event (make-instance '*midi-event* :opcode opcode
                                                :channel (1+ channel) :data1 (car data) :data2 0
                                                :duration 0 :time-offset delta-time)))
               ;;return the event created
               the-event)
              ((eq event-type 'track-name)
               ;;if the event is a track-name event, create the track name string
               (dolist (the-value data)
                 (setq the-name (concatenate 'string the-name (string (code-char the-value)))))
               ;; change all the spaces in the name to dashes so that lisp wont get confused
               (setq the-name (substitute #\- #\space the-name))
               ;;return the name
               the-name)
              ((eq event-type 'set-tempo)
               ;; convert the three data bytes to one number and return it
               (+ (caddr data) (* 256 (cadr data)) (* 65536 (car data))))
              (t   ;; do nothing and return the original raw data
               data)))))



(defun GET-EVENT-TYPE-AND-DATA (opcode buffer pos paramblock)
  (let ((data nil) event-type length the-byte)
    ;; if the event is a meta event...
    (cond ((= opcode #xf0)
           ;; a sysex event.
           (setq event-type 'sysex)
           ;; get the remaining data and store it with f0 in a string consisting of 2 hex
           ;; values followed by a new line (except for the last line) for each byte of data.
           (let (result length)
             ;; get the delta-time for the current event
             (setq result (read-time-tag buffer pos paramblock))
             (setq pos (car result))
             (setq length (cdr result))
             ;; get the data
             (setq data (make-string (+ 2 (* 3 length))))
             (replace data "f0")
             (dotimes (i length)
               (multiple-value-setq (pos the-byte) (MFF-cycling-get-byte buffer pos paramblock))
               (replace data (format nil "~%~2,'0x" the-byte) :start1 (+ 2 (* 3 i))))
             ))
    	  ((= opcode #xff)
           ;; get the command code for the particular meta-event
           (multiple-value-setq (pos the-byte) (MFF-cycling-get-byte buffer pos paramblock))
           ;; get the event type from the $meta-events-alist using the
           ;; command code as the index
           (setq event-type (cdr (assoc the-byte $meta-events-alist)))
           ;; get the length of the meta-event
           (multiple-value-setq (pos length) (MFF-cycling-get-byte buffer pos paramblock))
           (loop for i from 1 to length 
                 do
                 (multiple-value-setq (pos the-byte) (MFF-cycling-get-byte buffer pos paramblock))
                 (setq data (append data (list the-byte)))))
          ((< opcode #x80)
           ;; if the "opcode" is less than #x80 (128 decimal) it is a value between 0 and 127
           ;; corresponding to the current running status.
           (setq event-type 'running-status))
          ((member (dpb 0 (byte 4 0) opcode) $data2-events)
           ;; look at the opcode only, without the channel part of the byte
           (setq event-type (cdr (assoc (dpb 0 (byte 4 0) opcode) $gesture-slot-alist)))
           (multiple-value-setq (pos data) (MFF-cycling-get-byte buffer pos paramblock))
           (multiple-value-setq (pos the-byte) (MFF-cycling-get-byte buffer pos paramblock))
           (setq data (list data the-byte)))
          
          ;;;here is the problem!!!!!
          
          (t 
           (setq event-type (cdr (assoc (dpb 0 (byte 4 0) opcode) $gesture-slot-alist)))
           (multiple-value-setq (pos data) (MFF-cycling-get-byte buffer pos paramblock)))
          )
    (list pos event-type data)))

;**************
;   Reading   *
;**************



(defun MFF-READ-EVENT (running-status buffer pos paramblock)
  ;(when (equal running-status 'controllers)(break))
  (let ((data nil) result delta-time opcode event-type)
    ;; get the delta-time for the current event
    (setq result (read-time-tag buffer pos paramblock))
    (setq pos (car result))
    (setq delta-time (cdr result))
    ;; get the opcode for the current event
    (multiple-value-setq (pos opcode) (MFF-cycling-get-byte buffer pos paramblock))
    ;; get the event-type and raw data for the event
    (setq result (get-event-type-and-data opcode buffer pos paramblock))
    (setq pos (car result))
    (setq event-type (cadr result))
    (setq data (caddr result))
    ;; if there was no opcode, go back and get the data using the opcode of
    ;; the last event
    (when (eq event-type 'running-status)
      (setq result (get-event-type-and-data running-status buffer pos paramblock))
      (setq pos (car result))
      (setq event-type (cadr result))
      (setq data (caddr result)))
    ;;interpret the data according to the opcode
    (setq data (interpret-data event-type delta-time opcode data))
    (list pos event-type delta-time data)))

(defun READ-TRACK-HEADER (buffer pos paramblock)
  (let (chunk-type length result)
    ;;read the four bytes of the buffer starting at the position given
    ;;and put them in the list chunk-type
    (loop for i from 1 to 4
          do
          (multiple-value-setq (pos result) (MFF-cycling-get-byte buffer pos paramblock))
          (setq chunk-type (append chunk-type (list result))))
    ;;if these four bytes match the list $track-chunk-type, the header id is there
    (if (equal chunk-type $track-chunk-type)
      (progn
        ;;get the four-byte value representing the length
        (setq result (get-multi-byte-value 4 buffer pos paramblock))
        (setq pos (car result))
        (setq length (cdr result))
        (list pos length))
      ;;otherwise the header was not there so return the error.
      (format t "sorry, the expected track header was missing."))))

(defun MAKE-UNIQUE-GESTURE-NAME (&key (name 'gesture))
  (if (not (member name (mapcar #'name *current-gestures*)))
    name
    (let ((next-number 0)
          the-name)
      (loop 
        (setq next-number (1+ next-number))
        (setq the-name (read-from-string 
                        (concatenate 'string (format nil "~a.~a" name next-number))))
        (if (not (member the-name (mapcar #'name *current-gestures*)))
          (return the-name))))))

;**************
;   Gesture   *
;**************

(defun MAKE-GESTURE (&rest args &key (add-to-gestures-list t) &allow-other-keys)
  ;; instantiate the object
  (let ((the-gesture 
         (setq *music* 
               (apply #'make-instance '*gesture* :allow-other-keys t args))))
    (setf (name the-gesture) (make-unique-gesture-name :name (name the-gesture)))
    (if add-to-gestures-list
      (push the-gesture *current-gestures*))
    (set (name the-gesture) the-gesture) ;spiffy!  makes the gesture's name point to itself
    ;; this way you never lose track of which name is connected to which collection of data
    the-gesture))

(defClass *GESTURE* nil
  ;; initialize the slots of the gesture                                                         
  ((name          :initarg :name          ;the name of the gesture (a symbol)
                  :initform (make-unique-gesture-name)  ;the name must be unique.
                  :accessor name)
   (time-offset   :initarg :time-offset   ;the time offset from its parent (if any)
                  :initform 0
                  :accessor time-offset)
   (Alice-notes         :initarg :Alice-notes  ;note events in time-offset sorted order
                        :initform nil
                        :accessor Alice-notes)
   (polypressures :initarg :polypressures
                  :initform nil
                  :accessor polypressures)
   (controllers   :initarg :controllers
                  :initform nil
                  :accessor controllers)
   (programs      :initarg :programs
                  :initform nil
                  :accessor programs)
   (channelpressures :initarg :channelpressures
                     :initform nil
                     :accessor channelpressures)
   (pitchwheels   :initarg :pitchwheels
                  :initform nil
                  :accessor pitchwheels)
   (sysexs        :initarg :sysexs  ;a list of *sysex-event*s.
                  :initform nil
                  :accessor sysexs)
   (subgestures :initarg :subgestures    ;slot to hold a list of gestures
                :initform nil           
                :accessor subgestures)))

;;; scale-all-offsets-after takes a gesture and modifies all offset times of events
;;; and subgestures and all durations after the given start-time by multiplying the part of them
;;; after the start-time by the scale-factor.

(defMethod SCALE-ALL-OFFSETS-AFTER (start-time (the-gesture *GESTURE*) scale-factor)
  
  ;; first change the note offsets and durations
  (dolist (next-note (Alice-notes the-gesture))
    (let ((its-time-offset (time-offset next-note))
          (its-duration (duration next-note)))
      (if (> its-time-offset start-time)
        (progn (setf (time-offset next-note)
                     (+ start-time (round (* scale-factor (- its-time-offset start-time)))))
               (setf (duration next-note) 
                     (round (* scale-factor its-duration))))
        (if (> (+ its-time-offset its-duration) start-time)
          (setf (duration next-note)
                (+ (- start-time its-time-offset)
                   (round (* scale-factor (- its-duration
                                             (- start-time its-time-offset))))))))))
  (dolist (next-event (polypressures the-gesture))
    (if (> (time-offset next-event) start-time)
      (setf (time-offset next-event)
            (+ start-time (round (* scale-factor (- (time-offset next-event) start-time)))))))
  (dolist (next-event (controllers the-gesture))
    (if (> (time-offset next-event) start-time)
      (setf (time-offset next-event)
            (+ start-time (round (* scale-factor (- (time-offset next-event) start-time)))))))
  (dolist (next-event (programs the-gesture))
    (if (> (time-offset next-event) start-time)
      (setf (time-offset next-event)
            (+ start-time (round (* scale-factor (- (time-offset next-event) start-time)))))))
  (dolist (next-event (channelpressures the-gesture))
    (if (> (time-offset next-event) start-time)
      (setf (time-offset next-event)
            (+ start-time (round (* scale-factor (- (time-offset next-event) start-time)))))))
  (dolist (next-event (pitchwheels the-gesture))
    (if (> (time-offset next-event) start-time)
      (setf (time-offset next-event)
            (+ start-time (round (* scale-factor (- (time-offset next-event) start-time)))))))
  
  ;; recursively do the same for all subgestures
  (dolist (next-sub (subgestures the-gesture))
    (scale-all-offsets-after (- start-time (time-offset next-sub))
                             next-sub scale-factor))
  
  ;; finally, modify the subgesture offsets (this actually isn't necessary when this is called
  ;; by MFF-read since the subgesture offsets are all zero.  it's included here for completeness.)
  (dotimes (i (length (subgestures the-gesture)))
    (let ((sub-offset (time-offset (elt (subgestures the-gesture) i))))
      (if (> sub-offset start-time)
        (setf (time-offset (elt (subgestures the-gesture) i))
              (+ start-time (round (* scale-factor (- sub-offset start-time))))))))
  )

;;; MFF-read-first-track is called by MFF-read.  it looks for set tempo
;;; meta-events and extracts the tempo.  it assumes the tempo is valid for the whole
;;; file (all tracks).  it ignores everything else in the track.
;;; if there are no set tempo meta-events in the track,
;;; it sets a tempo of 500,000 usec/beat (= 120 beats/min).
;;; it returns the values pos and a list of cons pairs consisting of
;;; the tempos (in usec/beat) and their delta-times (in units of the previous tempo).

;*************
;   Tracks   *
;*************

(defun MFF-READ-FIRST-TRACK (buffer pos paramblock)
  (let (result (event-type nil) (the-tempos nil)
               data (delta-time 0))
    (setq result (read-track-header buffer pos paramblock))
    (setq pos (car result))
    (loop until (eq event-type 'end-of-track) do
          ;;read and interpret the current event from the buffer
          (setq result (MFF-read-event 'Alice-notes buffer pos paramblock))
          (setq pos (first result))
          (setq event-type (second result))
          (setq delta-time (+ (third result) delta-time)) ;add all delta-times until the next set-tempo
          ;;data is the actual event's contents.  if the event-type was a
          ;;set-tempo meta-event, it will contain the tempo value.
          (setq data (fourth result))
          (cond ((eq event-type 'set-tempo)
                 ;;add the tempo and delta-time to the-tempos
                 (setq the-tempos (append the-tempos (list (cons data delta-time))))
                 (setq delta-time 0))
                (t  ;; ignore any other event-types
                 )))
    (if (or (null the-tempos)  ;there were no set-tempo meta-events
            (> 0 (cdar the-tempos)))  ;the first set-tempo does not occur at time 0
      (push (cons 500000 0) the-tempos))
    (values pos the-tempos)))

;;;----------------------------------------------------------------------------
;;; MFF-read-track is called by MFF-read.  essentially, it creates a
;;; gesture from the current track in the midi file.
;;; input:
;;; buffer is the macptr from which to read
;;; pos specifies from where in buffer to start reading
;;; paramblock is passed so that the MFF-cycling-get-byte function can read
;;;        a new buffer from the file if the buffer needs to be cycled.
;;; output:
;;; pos is the position after the last byte read
;;; the-gesture is the new gesture

(defun MFF-READ-TRACK (buffer pos paramblock)
  (let (result (event-type nil) delta-time (total-time 0) data (gesture-name nil)
               (pending-durations nil) the-gesture (running-status 'Alice-notes))
    ;; make a gesture into which to start putting the data.
    (setq the-gesture (make-gesture))
    ;; read the header for this track
    ;; note:at this point all this does is increment the pos to the beginning
    ;; of the actual data in the track.  is there any data in the track
    ;; header which is needed?
    (setq result (read-track-header buffer pos paramblock))
    (setq pos (car result))
    ;; keep reading data from the track until the end-of-track event is encountered
    (loop until (eq event-type 'end-of-track)
          do 
          ;; read and interpret the current event from the buffer
          (setq result (MFF-read-event running-status buffer pos paramblock))
          (setq pos (car result))
          (setq event-type (cadr result))
          ;; set the current running status to this event-type so that if the
          ;; next event has no opcode, it should be the same as this event's
          
          (setq running-status event-type)
          (setq delta-time (caddr result))
          
          ;; since midi file's time-offsets are the times between events and
          ;; compsers's toolbox's time-offets are the times from the beginning of
          ;; the gesture, the total time has to be accumulated to know how far
          ;; from the beginning of the track the event is.
          (setq total-time (+ total-time delta-time))
          ;; data is the actual event's contents.  if the event-type was one of
          ;; channel slots, it will be a *midi-event*. if it was a
          ;; track-name meta-event, it will contain the string name for the
          ;; gesture.  if event-type was sysex, it will be a *sysex-event*.
          (setq data (cadddr result))
          (cond ((eq event-type 'sysex)
                 ;; put it in the sysexs slot of the gesture.
                 (setf (time-offset data) total-time)
                 (setf (sysexs the-gesture)
                       (append (sysexs the-gesture) (list data))))
          	((eq event-type 'Alice-notes)
                 ;; put the time from the beginning of the gesture into the
                 ;; time-offset slot.
                 (setf (time-offset data) total-time)
                 
                 ;; if it was a note of velocity 0 then it should be treated as
                 ;; a note off.
                 (if (= (data2 data) 0)
                   (setq pending-durations
                         (add-duration (data1 data) (time-offset data) pending-durations))
                   (progn
                     ;; if the event was a note with a non-zero velocity, add it to
                     ;; the Alice-notes slot of the gesture.
                     (setf (Alice-notes the-gesture)
                           (append (Alice-notes the-gesture) (list data)))
                     ;; add this note to the Alice-notes waiting for note-offs for duration
                     (setq pending-durations (append pending-durations (list data))))))
                ((eq event-type 'noteoff)
                 (setf (time-offset data) total-time)
                 ;; use this note-off to determine the duration of it's corresponding
                 ;; note-on.
                 (setq pending-durations
                       (add-duration (data1 data) (time-offset data) pending-durations)))
                ((member event-type $gesture-slots)
                 ;; put the event into its corresponding slot of the event block
                 (setf (time-offset data) total-time)
                 (setf (slot-value the-gesture event-type)
                       (append (slot-value the-gesture event-type) (list data))))
                ((eq event-type 'track-name)
                 ;; use the string name to create a symbol name for the gesture
                 (setq gesture-name (read-from-string data)))
                (t  ;; ignore any other event-types
                 )))
    
    ;; if the track had no name, create a unique name
    (setf (name the-gesture) (make-unique-gesture-name :name gesture-name))
    ;(set (name the-gesture) the-gesture)  ;make the new name point to the gesture
    
    (list pos the-gesture)))

(defun READ-HEADER-CHUNK (buffer paramblock)
  (let ((pos 0) chunk-type length format number-of-tracks division result the-byte)
    ;;read the first four bytes of the buffer and put them in the chunk-type
    ;;list
    (loop for i from 1 to 4
          do
          (multiple-value-setq (pos the-byte) (MFF-cycling-get-byte buffer pos paramblock))
          (setq chunk-type (append chunk-type (list the-byte))))
    ;;if the chunk-type was a header-chunk-type, get the rest of the information
    ;;given in the header.
    (if (equal chunk-type $header-chunk-type)
      (progn
        (setq result (get-multi-byte-value 4 buffer pos paramblock))
        (setq pos (car result))
        (setq length (cdr result))
        (setq result (get-multi-byte-value 2 buffer pos paramblock))
        (setq pos (car result))
        (setq format (cdr result))
        (setq result (get-multi-byte-value 2 buffer pos paramblock))
        (setq pos (car result))
        (setq number-of-tracks (cdr result))
        (setq result (get-multi-byte-value 2 buffer pos paramblock))
        (setq pos (car result))
        (setq division (cdr result))
        ;; now increment pos as specified in the midi specs in case the header chunk has
        ;; more stuff in it (which we will ignore).
        (incf pos (- length 6))
        (values pos format number-of-tracks division))
      (format t "sorry, the expected header chunk was missing."))))

(defun MFF-READ (&optional filename)
  (let (the-gesture (pos 0) midi-format divisions result number-of-tracks buffer paramblock
                    the-tempos prev-usec-per-division start-time)
    (when (not filename)
      ;; if no filename was specified, call up the choose-file-dialog
      (setq filename (catch-cancel (choose-file-dialog :mac-file-type "midi"
                                                       :button-string "import")))
      (if (equal filename :cancel)
        ;; the user cancelled so exit
        (return-from MFF-read 'cancel)))
    
    ;; setup a buffer of size specified in the constant $MFF-buffer-size
    (setq buffer (#_newptr $MFF-buffer-size))
    ;; paramblock (described in inside macintosh v.iv) is a pointer to a block
    ;; of memory in which parameters for the open file are stored, such as
    ;; read/write permission, the current file position, and its access path onto
    ;; the physical disk.
    (setq paramblock (fsopen filename))
    ;; if the file is smaller than the buffer, only read up to the end of file.
    (if (< (geteof paramblock) $MFF-buffer-size)
      (fsread paramblock (geteof paramblock) buffer)
      ;; otherwise only read a chunk from the file which fits in the buffer.
      (fsread paramblock $MFF-buffer-size buffer))
    
    ;; this reads and interprets the header chunk in the midi file format file.
    (multiple-value-setq (pos midi-format number-of-tracks divisions)
      (read-header-chunk buffer paramblock))
    (when (not (= midi-format 1))
      (message-dialog "Alice can only read format 1 midi files.")
      (fsclose paramblock)
      (#_disposeptr buffer)
      (return-from MFF-read 'error))
    
    ;; read the first track to get the tempo and ignore the rest of the first track.
    (multiple-value-setq (pos the-tempos) (MFF-read-first-track buffer pos paramblock))
    ;; read the second track before the loop starts so that it can be used as
    ;; the base gesture to which all the other subgestures can be attached.
    (setq result (MFF-read-track buffer pos paramblock))
    (setq pos (car result))
    ;; create the gesture from the results of calling MFF-read-track
    (setq the-gesture (second result))
    ;; interpret the rest of the tracks
    (loop for i from 1 to (- number-of-tracks 2)
          do
          ;; read the current track
          (setq result (MFF-read-track buffer pos paramblock))
          (setq pos (car result))
          ;; add the current gesture to the subgesture list of the first gesture
          ;; note that each subgesture will have a time-offset of 0
          (push (second result) (subgestures the-gesture))
          )
    ;; close the file
    (fsclose paramblock)
    ;; dispose of the buffer ptr
    (#_disposeptr buffer)
    ;; now adjust all offset times and durations into units of milliseconds.
    (setq prev-usec-per-division 1000)  ;one millisecond per division
    (setq start-time 0)
    (dolist (next-tempo-pair the-tempos)
      (let ((usec-per-division (/ (car next-tempo-pair) divisions))
            (delta-time (cdr next-tempo-pair)))
        (when (not (= prev-usec-per-division usec-per-division))
          (setq start-time (+ start-time (* (/ prev-usec-per-division 1000)
                                            delta-time)))
          (scale-all-offsets-after start-time the-gesture
                                   (/ usec-per-division prev-usec-per-division))
          (setq prev-usec-per-division usec-per-division))))
    ;; return the gesture
    the-gesture))

;********************
;   Toplevel Load   *
;********************

(defun LOAD-FROM-MIDI-MENU-ITEM-ACTION nil
  (let ((filename (catch-cancel (choose-file-dialog ;:mac-file-type "Midi"  ;;this doesn't work with certain mid files
                                                    :button-string "load"))))
    (unless (equal filename :cancel)(setq *file-name* (get-filename filename)))
    (if (not (equal filename :cancel))
      (with-cursor *watch-cursor* (MFF-read filename)))))

(defClass *SYSEX-EVENT* nil
  ;; name is a string used only by the user to describe the contents of the
  ;; sysex.  ct doesn't use it, except for display purposes in the sysex editor.
  ((name      :initarg :name
              :initform "no name"
              :accessor name)
   ;; data is a string.  the first two characters of each line are hex
   ;; characters corresponding to one data byte.  the remaining characters
   ;; on each line form an arbitrarily long
   ;; comment string specified by the user to indicate the use of the data byte.
   (data      :initarg :data
              :initform (concatenate 'string "f0  status byte for sysex"
                                     (string #\return) (string #\return)
                                     "f7  eox")
              :accessor data)
   (time-offset :initarg :time-offset
                :initform 0
                :accessor time-offset))
  (:documentation "the building block of all sysex messages."))

(defun CREATE-SAVE-AS-MIDI-DIALOG nil
  (let (+save-midi-dialog+)
    
    (setq +save-midi-dialog+
          (make-instance 'dialog
            :window-type :double-edge-box
            :view-position #@(91 50)
            :view-size #@(232 179)
            :close-box-p nil
            :view-font '("chicago" 12 :srcor :plain)
            :view-subviews (list
                            (make-dialog-item 'static-text-dialog-item
                                              #@(9 6)
                                              #@(217 16)
                                              "select the composition to save:"
                                              nil)
                            (make-dialog-item 'sequence-dialog-item
                                              #@(6 31)
                                              #@(219 80)
                                              "untitled"
                                              nil
                                              :cell-size #@(205 16)
                                              :table-hscrollp nil
                                              :table-vscrollp t
                                              :table-sequence (mapcar #'name *current-gestures*)
                                              :selection-type :single
                                              :view-nick-name 'scroll-box)
                            (make-dialog-item 'check-box-dialog-item
                                              #@(19 119)
                                              #@(184 17)
                                              "save multiple works?"
                                              nil
                                              :view-nick-name 'check-box
                                              :check-box-checked-p t)
                            (make-dialog-item 'button-dialog-item
                                              #@(48 150)
                                              #@(47 20)
                                              "save"
                                              #'(lambda (item)
                                                  (declare (ignore item))
                                                  (let* ((the-box (view-named 'scroll-box +save-midi-dialog+))
                                                         (the-cells (selected-cells the-box))
                                                         (whole-tree-p (check-box-checked-p
                                                                        (view-named 'check-box +save-midi-dialog+))))
                                                    (if (null the-cells) ;nothing was selected
                                                      (message-dialog "you must select a work to be saved.")
                                                      (return-from-modal-dialog (list (elt *current-gestures*
                                                                                           (cell-to-index the-box (car the-cells))) whole-tree-p)))))
                                              :default-button t)
                            (make-dialog-item 'button-dialog-item
                                              #@(131 150)
                                              #@(56 19)
                                              "cancel"
                                              #'(lambda (item)
                                                  (declare (ignore item))
                                                  (return-from-modal-dialog 'cancel))
                                              :default-button nil))))
    (modal-dialog +save-midi-dialog+)))

;*************
;   Basics   *
;*************

(defun COUNT-VARIABLE-LENGTH-BYTES (time)
  (let ((byte-offset 0) (encoded (logand time #x7f)))
    (loop while (> (setf time (ash time -7)) 0)
	  do
          (setf encoded (+ (logior (ash encoded 8) #x80)
		           (logand time #x7f))))
    (loop while (logtest #x80 encoded)
	  do
          (incf byte-offset)
          (setf encoded (ash encoded -8)))
    (+ byte-offset 1)))

(defun GET-NUMBER-OF-BYTES (event-list)
  (let ((byte-count 0) (current-time 0) (delta-time 0) (last-time 0))
    ;;go through every event in the event-list
    (dolist (the-event event-list)
      (setq current-time (time-offset the-event))
      (setf delta-time (- (time-offset the-event) last-time))
      (incf byte-count (+ (count-variable-length-bytes delta-time)
                          (cond ((typep the-event '*sysex-event*)
                                 ;; 1 byte for f0 plus
                                 ;; the length of the sysex (less the f0 bytes and the
                                 ;; length itself) and the message
                                 (+ 1 (count-variable-length-bytes
                                       (1- (length (data the-event))))
                                    (1- (length (data the-event)))))
                                ((member (opcode the-event) $data2-events)
                                 3)
                                ((member (opcode the-event) $data1-events)
                                 2)
                                (t    (error "illegal event in the midi file."))
                                )))
      (setq last-time current-time))
    byte-count))

(defun WRITE-TIME-TAG (time buffer pos paramblock)
  (let ((encoded (logand time #x7f)))
    (loop while (> (setf time (ash time -7)) 0)
	  do
          (setf encoded (+ (logior (ash encoded 8) #x80)
		           (logand time #x7f))))
    (loop counting (setq pos (MFF-cycling-put-byte (logand encoded #xff) buffer pos paramblock))
	  while (logtest #x80 encoded)
	  do
          (setf encoded (ash encoded -8)))
    pos))

;**************
;   Writing   *
;**************

(defMethod MFF-WRITE-EVENT ((event *MIDI-EVENT*) buffer pos paramblock)
  ;;write the time tag for the midi-event
  (setq pos (write-time-tag (time-offset event) buffer pos paramblock))
  ;;write the opcode of the event, adding the channel to it
  (setq pos (MFF-cycling-put-byte (+ (opcode event) (1- (channel event))) buffer pos
                                  paramblock))
  ;;write the data1 value of the event
  (setq pos (MFF-cycling-put-byte (data1 event) buffer pos paramblock))
  ;;if the event has a second data byte...
  (if (member (opcode event) $data2-events)
    ;;write the data2 value of the event
    (setq pos (MFF-cycling-put-byte (data2 event) buffer pos paramblock)))
  pos)

(defun MFF-CYCLING-PUT-BYTE (data buffer pos paramblock)
  ;;if the position is about to exceed the buffer's size,
  (if (= pos $MFF-buffer-size)
    (progn
      ;;write the buffer to the file
      (fswrite paramblock $MFF-buffer-size buffer)
      ;;and start again at the beginning of the buffer
      (setq pos 0)))
  ;;put the data into the current position of the buffer
  (%put-byte buffer data pos)
  (+ pos 1))

(defun MFF-CYCLING-PUT-LONG (data buffer pos paramblock)
  ;;if the position is about to exceed the buffer's size,
  (cond ((= pos $MFF-buffer-size)
         ;;write the buffer to the file
         (fswrite paramblock $MFF-buffer-size buffer)
         ;;and start again at the beginning of the buffer
         (setq pos 0))
        ((= pos (1- $MFF-buffer-size))
         ;; write the buffer to the file
         (fswrite paramblock (1- $MFF-buffer-size) buffer)
         ;; and start again at the beginning of the buffer
         (setq pos 0))
        ((= pos (- $MFF-buffer-size 2))
         ;; write the buffer to the file
         (fswrite paramblock (- $MFF-buffer-size 2) buffer)
         ;; and start again at the beginning of the buffer
         (setq pos 0))
        ((= pos (- $MFF-buffer-size 3))
         ;; write the buffer to the file
         (fswrite paramblock (- $MFF-buffer-size 3) buffer)
         ;; and start again at the beginning of the buffer
         (setq pos 0))
        (t    ;; else do nothing
         ))
  ;;put the data into the current position of the buffer
  (%put-long buffer data pos)
  (+ pos 4))

(defun WRITE-TRACK-HEADER (length buffer pos paramblock)
  (dolist (the-char $track-chunk-type)
    (setq pos (MFF-cycling-put-byte the-char buffer pos paramblock)))
  ;;now write the length
  (setq pos (MFF-cycling-put-long length buffer pos paramblock))
  pos)

(defGeneric PARSE (the-object &key master-offset parse-subs start end)
  (:documentation
   "parse is a generic function that applies to *midi-event*, *sysex-event* and *gesture*
objects, and to lists of *midi-event* and *sysex-event* objects.
it merges copies of all the object's data into one list of *midi-event*s and
*sysex-event*s, which it returns.
if the-object is a *gesture* object and if parse-subs is t, then all subgestures of
the-object are also parsed and added to the output list.
the master-offset is added to the time-offset of all the items.
the list of objects that is returned consists of new
copies of all objects in the object.  also, a noteoff event is added
for every noteon event in the list.  the final list is then sorted by time-offset.
this list is now ready to be sent to the function play to be written out the serial port.
the key values start and end signify which portion of the final list is to be returned.
only those events whose time-offset + master-offset starts between the times start and
end (in units of seconds) are
included and all note-off events occur at the time end.  an end value of nil indicates
to include everything from the start time to the end of the-object.  a start value of
nil indicates to include everything from the first event until the end time.")
  )

(defGeneric DUPLICATE-OBJECT (the-object &key new-name whole-tree identical-subs
                                         add-to-gestures-list)
  (:documentation
   "duplicate-object is a generic function that returns a new clone of the
object passed as an argument.  this works for objects of type
*midi-event* and *gesture*.
it takes a key value giving the new name of the copy of the object
(this is ignored for *midi-event* objects, which don't have names).
if the new-name is t, then an integer is appended to the name of the gesture to make it unique.
if the new-name is nil, then the clone has the same name as the original.
if whole-tree is true and the-object is a gesture, then the whole subtree
rooted at the gesture is duplicated.  more precisely, if the object is a gesture, we have:
if identical-subs is nil and whole-tree is nil, then the subgesture list of the
    duplicate is nil.
if identical-subs is nil and whole-tree is t, then the subgesture lists of
    the duplicates in the tree point to the new duplicate subgestures
    (so you have a whole new tree).
if identical-subs is t and whole-tree is nil, then the subgesture list of
    the duplicate is the same (points to the same objects) as the original gesture.
if identical-subs is t and whole-tree is t, then all subgestures in the tree are duplicated,
    but they are exact duplicates of the original, and so do not make up a new tree.
if whole-tree is nil, the new duplicate of the-object is returned.
if whole-tree is t, then the list of new duplicates is returned.
note:  nothing gets added to the *current-gestures* in the default case.
       the reason is that duplicate-object is mostly used for the undo operation."))

(defMethod PARSE ((the-event *MIDI-EVENT*) &key (master-offset 0) parse-subs
                  (start nil) (end nil))
  (declare (ignore parse-subs))
  (let ((output nil))
    (when (and (or (null start)
                   (>= (+ master-offset (time-offset the-event)) start))
               (or (null end)
                   (< (+ master-offset (time-offset the-event)) end)))
      ;; create note-off event if it's a  note-on event and make a new list.
      (let ((note-off-event nil)
            (copy-of-event (duplicate-object the-event)))
        (setf (time-offset copy-of-event) (+ (time-offset copy-of-event) master-offset))
        (cond ((eql (opcode the-event) $noteon)
               (setq note-off-event (duplicate-object the-event))
               (setf (time-offset note-off-event)
                     (if (null end)
                       (+ master-offset (time-offset the-event) (duration the-event))
                       (min end (+ master-offset (time-offset the-event)
                                   (duration the-event)))))
               (setf (opcode note-off-event) $noteoff)
               (setq output (list copy-of-event note-off-event)))
              (t   (setq output (list copy-of-event))))))
    output))

(defMethod PARSE ((the-gesture *GESTURE*) &key (master-offset 0) (parse-subs t)
                  (start nil) (end nil))
  (let ((output (parse-events-in-main the-gesture :master-offset master-offset
                                      :start start :end end)))
    ;; now go through the list calling parse on each subgesture.
    ;; parse-gesture will recurse through all the levels of gestures, updating the offset
    ;; according to the gesture's place in the hierarchy
    (if parse-subs   ;do the subs too
      (dolist (next-gesture (subgestures the-gesture))
        (setq output (merge 'list output 
                            (parse next-gesture :master-offset (+ (time-offset next-gesture) master-offset)
                                   :parse-subs t :start start :end end)
                            #'< :key #'time-offset))))
    ;; return the output list
    output))

;;; ------------------------------------------------
;;; this is the parse function for the *midi-event* and *sysex-event* objects stored in
;;; the gesture.
;;; it adds a noteoff event to correspond to the ending of every noteon event.
;;; it also adds master-offset to the time-offsets of all events.  it merges
;;; all the copies of all events into one list, sorted by time-offset.
;;; it also filters out all events outside of the start/end range.
;;; it could use the "parse" method for midi-events, but direct computations
;;; are probably more efficient.  it ignores parse-subs.

(defun PARSE-EVENTS-IN-MAIN (the-gesture &key (master-offset 0) (start nil) (end nil))
  ;; first create note-off events for every note-on event and make a new list.
  (let* ((output nil)
         (relative-start (if start (- start master-offset) nil))
         (relative-end (if end (- end master-offset) nil))
         (note-on-list (mapcar #'duplicate-object
                               (remove-if-not #'(lambda (event)
                                                  (and (or (null relative-start)
                                                           (>= (time-offset event) relative-start))
                                                       (or (null relative-end)
                                                           (< (time-offset event) relative-end))))
                                              (Alice-notes the-gesture))))
         (note-off-list (mapcar #'duplicate-object note-on-list))
         (polypressures-list (mapcar #'duplicate-object
                                     (remove-if-not #'(lambda (event)
                                                        (and (or (null relative-start)
                                                                 (>= (time-offset event) relative-start))
                                                             (or (null relative-end)
                                                                 (< (time-offset event) relative-end))))
                                                    (polypressures the-gesture))))
         (channelpressures-list (mapcar #'duplicate-object
                                        (remove-if-not #'(lambda (event)
                                                           (and (or (null relative-start)
                                                                    (>= (time-offset event) relative-start))
                                                                (or (null relative-end)
                                                                    (< (time-offset event) relative-end))))
                                                       (channelpressures the-gesture))))
         (pitchwheels-list (mapcar #'duplicate-object
                                   (remove-if-not #'(lambda (event)
                                                      (and (or (null relative-start)
                                                               (>= (time-offset event) relative-start))
                                                           (or (null relative-end)
                                                               (< (time-offset event) relative-end))))
                                                  (pitchwheels the-gesture))))
         (programs-list (mapcar #'duplicate-object
                                (remove-if-not #'(lambda (event)
                                                   (and (or (null relative-start)
                                                            (>= (time-offset event) relative-start))
                                                        (or (null relative-end)
                                                            (< (time-offset event) relative-end))))
                                               (programs the-gesture))))
         (controllers-list (mapcar #'duplicate-object
                                   (remove-if-not #'(lambda (event)
                                                      (and (or (null relative-start)
                                                               (>= (time-offset event) relative-start))
                                                           (or (null relative-end)
                                                               (< (time-offset event) relative-end))))
                                                  (controllers the-gesture))))
         (sysex-list (mapcar #'(lambda (next-sysex)
                                 (make-instance '*sysex-event*
                                   :name (copy-seq (name next-sysex))
                                   :data (data next-sysex)
                                   :time-offset (time-offset next-sysex)))
                             ;; note: duplicate-object was not used here since the data
                             ;; slot of the new sysex is about to be totally changed below,
                             ;; so you don't need to worry about the new sysex sharing the data
                             ;; slot with the old sysex.
                             (remove-if-not #'(lambda (event)
                                                (and (or (null relative-start)
                                                         (>= (time-offset event) relative-start))
                                                     (or (null relative-end)
                                                         (< (time-offset event) relative-end))))
                                            (sysexs the-gesture))))
         )
    ;; now process the sysex's, replacing the data slot value with a string of characters
    ;; to be sent out the port, to save time later when the output list is played.
    ;; each character has ascii value equal to the desired byte of sysex data.
    ;; %put-cstring is used in the function 'play' to copy the string into the midi-packet.
    (dolist (next-sysex sysex-list)
      (let* ((data-slot (data next-sysex))
             (new-string (make-string (1+ (count #\return data-slot))))
             (prev-newline-pos -1)
             )
        (dotimes (i (length new-string))
          (setf (elt new-string i)
                (code-char (parse-integer (subseq data-slot (1+ prev-newline-pos)
                                                  (+ 3 prev-newline-pos))
                                          :radix 16)))
          (setq prev-newline-pos
                (position #\return data-slot :start (1+ prev-newline-pos))))
        (setf (data next-sysex) new-string)))
    ;; now fix up the note-off-list so that it has the correct opcode
    ;; and time-offset (less the master-offset).
    (dolist (next-note note-off-list)
      (setf (time-offset next-note)
            (if (null end)
              (+ (time-offset next-note) (duration next-note))
              (min relative-end (+ (time-offset next-note) (duration next-note)))))
      (setf (opcode next-note) $noteoff))
    ;; now sort the note-off-list
    (setq note-off-list (sort note-off-list #'< :key #'time-offset))
    ;; now merge every event list into one long sorted list.
    (setq output (merge 'list
                        (merge 'list
                               (merge 'list channelpressures-list pitchwheels-list #'< :key #'time-offset)
                               (merge 'list controllers-list programs-list #'< :key #'time-offset)
                               #'< :key #'time-offset)
                        (merge 'list
                               (merge 'list note-on-list polypressures-list #'< :key #'time-offset)
                               (merge 'list note-off-list sysex-list  #'< :key #'time-offset)
                               #'< :key #'time-offset)
                        #'< :key #'time-offset)
          )
    ;; now add the master-offset to everything
    (dolist (next-event output)
      (setf (time-offset next-event) (+ (time-offset next-event) master-offset)))
    ;; now output the output list
    output))

(defMethod DUPLICATE-OBJECT ((the-gesture *GESTURE*) &key (new-name nil)
                             (whole-tree nil)
                             (identical-subs 
                              nil)
                             (add-to-gestures-list nil))
  (let ((new-gesture (make-gesture :add-to-gestures-list add-to-gestures-list
                                   :name (name the-gesture)
                                   :Alice-notes (mapcar #'duplicate-object
                                                        (Alice-notes the-gesture))
                                   :polypressures (mapcar #'duplicate-object
                                                          (polypressures the-gesture))
                                   :controllers (mapcar #'duplicate-object
                                                        (controllers the-gesture))
                                   :programs (mapcar #'duplicate-object
                                                     (programs the-gesture))
                                   :channelpressures (mapcar #'duplicate-object
                                                             (channelpressures the-gesture))
                                   :pitchwheels (mapcar #'duplicate-object
                                                        (pitchwheels the-gesture))
                                   :time-offset (time-offset the-gesture)
                                   :sysexs (mapcar #'duplicate-object
                                                   (sysexs the-gesture))
                                   :subgestures (if identical-subs
                                                  (subgestures the-gesture)
                                                  nil))))
    (if (null new-name)  ;give them identical names
      (setf (name new-gesture) (name the-gesture)))
    ;; otherwise make-gesture gives it a unique name
    ;; (as long as the-gesture is in *current-gestures*)
    (cond ((and whole-tree identical-subs)
           (let ((result-list (list new-gesture)))
             (dolist (next-gesture (subgestures the-gesture) result-list)
               (setf result-list (append result-list
                                         (duplicate-object next-gesture :whole-tree t
                                                           :new-name new-name
                                                           :add-to-gestures-list add-to-gestures-list))))))
          ((and whole-tree (not identical-subs))
           (let ((result-list (acons the-gesture new-gesture nil))
                 next-object)
             (dolist (next-gesture (remove the-gesture (all-gestures-in-tree the-gesture)))
               ;; for all items in the tree except the root gesture, duplicate them.
               (setq next-object (duplicate-object next-gesture
                                                   :new-name new-name
                                                   :whole-tree nil :identical-subs nil
                                                   :add-to-gestures-list add-to-gestures-list))
               (setq result-list (acons next-gesture next-object result-list)))
             ;; now fix up the subgesture lists of the new objects to correspond
             ;; to the subgesture lists of the originals.
             (dolist (next-pair result-list)
               (setf (subgestures (cdr next-pair))
                     (mapcar #'(lambda (next-sub)
                                 (cdr (assoc next-sub result-list)))
                             (subgestures (car next-pair)))))
             (mapcar #'cdr result-list)))  ;return the new gestures
          (t  new-gesture)  ;return the new-gesture object
          )))

;;;-------------------------------------------------------
;;; this is the duplicate-object method of the *midi-event* class.

(defMethod DUPLICATE-OBJECT ((the-event *MIDI-EVENT*) &key (new-name nil) (whole-tree nil)
                             (identical-subs t) (add-to-gestures-list nil))
  (declare (ignore new-name whole-tree identical-subs add-to-gestures-list))
  (make-instance '*midi-event* :opcode (opcode the-event) :channel (channel the-event)
                 :data1 (data1 the-event) :data2 (data2 the-event)
                 :duration (duration the-event) :time-offset (time-offset the-event)))

(defun ALL-GESTURES-IN-TREE (root-gesture)
  (let ((result (list root-gesture)))
    (dolist (next-sub (subgestures root-gesture) result)
      (setq result (append result (all-gestures-in-tree next-sub))))))

(defun MFF-WRITE-TRACK (the-gesture buffer pos paramblock &key (offset 0) (name nil))
  (let (event-list length (current-time 0) (last-time 0))
    ;;get all the events of the gesture into one list to be manipulated
    (setq event-list (parse the-gesture :master-offset offset))
    ;;the length of the track will be 
    ;;the number of bytes of the events themselves
    ;;+ 4 bytes for the end of track event
    (setq length (+ 4 (get-number-of-bytes event-list)))
    ;;if a name was supplied, add 3 bytes for the name-meta event declaration
    ;;+ 1 byte for the time stamp of 0 + the length of the string containing the name
    (if name
      (setq length (+ length 4 (length name))))
    (setq pos (write-track-header length buffer pos paramblock))
    (when name
      ;;write 0 for the time stamp
      (setq pos (write-time-tag 0 buffer pos paramblock))
      ;;write the meta-event byte
      (setq pos (MFF-cycling-put-byte #xff buffer pos
                                      paramblock))
      ;;3 is the code for track name meta-event
      (setq pos (MFF-cycling-put-byte 3 buffer pos
                                      paramblock))
      (setq pos (MFF-cycling-put-byte (length name) buffer pos
                                      paramblock))
      ;;put the ascii values of each individual letter of the name into the buffer
      (loop for i from 0 to (1- (length name))
            do
            (setq pos (MFF-cycling-put-byte (char-code (elt name i))
                                            buffer pos paramblock))))
    ;;since delta-times for events in midi file format are the times between
    ;;each event.  the time-offsets for events are
    ;;the times from the beginning of the track, a conversion had to be made.
    (dolist (the-event event-list)
      (setq current-time (time-offset the-event))
      (setf (time-offset the-event) (- (time-offset the-event) last-time))
      (setq pos (MFF-write-event the-event buffer pos paramblock))
      (setq last-time current-time))
    ;;now put the end of track event
    (setq pos (MFF-cycling-put-byte 0 buffer pos paramblock))
    (dolist (the-byte $end-of-track)
      (setq pos (MFF-cycling-put-byte the-byte buffer pos paramblock)))
    ;;return the current pos
    pos))

(defMethod MFF-WRITE-SEQUENCE ((the-gesture *GESTURE*) buffer pos paramblock
                               &key (offset 0) (subs-too t))
  ;;write the events of the current gesture
  (setq pos (MFF-write-track the-gesture buffer pos
                             paramblock :name (string (name the-gesture)) :offset offset))
  (if subs-too
    (dolist (next-sub (subgestures the-gesture))
      ;;recurse through all of the current gesture's subgestures and write them
      (setq pos (MFF-write-sequence next-sub buffer pos
                                    paramblock :offset (+ (time-offset next-sub) offset)))))
  pos)

(defun WRITE-TEMPO-TRACK (beats-per-minute buffer pos)
  ;; put the header in
  (dolist (the-char $track-chunk-type)
    (%put-byte buffer the-char pos)
    (incf pos))
  ;; put the length of 20 in
  (%put-long buffer 28 pos)
  (incf pos 4)
  ;; put in the track name
  (%put-byte buffer 0 pos)
  (incf pos)
  (%put-byte buffer #xff pos)
  (incf pos)
  (%put-byte buffer 3 pos)
  (incf pos)
  (%put-byte buffer 5 pos)
  (incf pos)
  (dolist (next-char $tempo-word)
    (%put-byte buffer next-char pos)
    (incf pos))
  
  ;; put in the time signature meta event
  (%put-byte buffer 0 pos)
  (incf pos)
  (%put-byte buffer #xff pos)
  (incf pos)
  (%put-byte buffer #x58 pos)
  (incf pos)
  (%put-byte buffer 4 pos)
  (incf pos)
  (%put-byte buffer 4 pos)
  (incf pos)
  (%put-byte buffer 2 pos)
  (incf pos)
  (%put-byte buffer #x24 pos)
  (incf pos)
  (%put-byte buffer 8 pos)
  (incf pos)
  
  ;; put in the set tempo meta event, (120 beats/min = 500,000 usec/beat).
  (%put-byte buffer 0 pos)
  (incf pos)
  (%put-byte buffer #xff pos)
  (incf pos)
  (%put-byte buffer #x51 pos)
  (incf pos)
  (%put-byte buffer 3 pos)
  (incf pos)
  (let ((usec-per-beat (floor 60000000 beats-per-minute)))
    (%put-byte buffer (floor usec-per-beat 65536) pos) ;puts in the upper byte
    (incf pos)
    (%put-word buffer usec-per-beat pos) ;puts in the low 16 bits
    (incf pos 2))
  
  ;; put in the end-of-track bytes.
  (%put-byte buffer 0 pos)
  (incf pos)
  (dolist (the-byte $end-of-track)
    (%put-byte buffer the-byte pos)
    (incf pos))
  ;; return pos
  pos)

(defMethod COUNT-SUBGESTURES ((the-gesture *GESTURE*))
  (let ((count 1))  ;for the root gesture
    (dolist (next-sub (subgestures the-gesture))
      (setq count (+ count (count-subgestures next-sub))))
    count))

(defun WRITE-HEADER-CHUNK (buffer format number-of-tracks division)
  (let ((pos 0))  ;start at the beginning of the file
    ;;write 'mthd' as the chunk header
    (dolist (the-char $header-chunk-type)
      (%put-byte buffer the-char pos)
      (incf pos))
    ;;write the 4 byte representation of the length 6
    (%put-long buffer 6 pos)
    (incf pos 4)
    ;;write the value of format
    (%put-word buffer format pos)
    (incf pos 2)
    ;;write the number-of-tracks
    (%put-word buffer number-of-tracks pos)
    (incf pos 2)
    ;;write the division
    (%put-word buffer division pos)
    (incf pos 2)
    pos))

;;;---------------------------------------------------------------------------------
;;; MFF-write takes the gesture given in the-gesture and writes it into a
;;; multi-track (format 1) midi file by the name of pathname.  if pathname is
;;; not given, then the user is asked to give a file name.  if the pathname was not nil,
;;; this function assumes that it is a legitimate pathname and the user wants to
;;; overwrite any existing file by that name.  if subs-too is true, then the whole
;;; subgesture tree of the-gesture is also saved, one subgesture per track.  if subs-too is
;;; nil, then the-gesture is saved in track 1 without any subgestures.

(defMethod MFF-WRITE ((the-gesture *GESTURE*) &optional (pathname nil) &key (subs-too t))
  (let (result buffer pos paramblock)
    ;; first create the new file.
    (unless pathname
      (setq result (catch-cancel
                     (choose-new-file-dialog :prompt "Save Midi data inÉ")))
      (if  (equal result :cancel)
        ;; the user canceled from creating a new file name
        (return-from MFF-write 'cancel))
      (setq pathname (mac-namestring result)))
    (create-file pathname :if-exists :supersede :mac-file-type "Midi"
                 :mac-file-creator "CPTB")
    ;;open the file with read-write permission
    (setq paramblock (fsopen pathname t))
    ;;create a buffer using the $MFF-buffer-size constant for the size of the buffer
    (setq buffer (#_newptr $MFF-buffer-size))
    ;;write the header chunk
    (setq pos (write-header-chunk buffer 1 (1+ (count-subgestures the-gesture)) 500))
    ;;write a track containing only the tempo data (120 beats per minute)
    (setq pos (write-tempo-track 120 buffer pos))
    ;;do the actual writing of the tracks
    (setq pos (MFF-write-sequence the-gesture buffer pos paramblock :subs-too subs-too))
    ;;write the leftover of the buffer since it was last cycled
    (fswrite paramblock pos buffer)
    ;;close the paramblock
    (fsclose paramblock)
    ;; dispose of the buffer
    (#_disposeptr buffer)
    ;; return t signifying a successful write
    t))

;******************
;   Saving MIDI   *
;******************
(defVar *PLACE* ())
(defVar *NEW-NAME* () "not a string")

(defun SAVE-AS-MIDI-MENU-ITEM-ACTION nil
  "calling (save-as-midi-menu-item-action) 
      save-as-midi-menu-item-action returned t"
  (let ((gesture-to-save 
         (make-a-midi-gesture))
        filename)
    (when (not (equal gesture-to-save 'cancel))
      (if *place* (progn (setq filename *place*)
                         ;(setq *new-name* (mac-file-namestring *place*))
                      (setq filename (make-pathname :directory (mac-directory-namestring *place*)
                                                   :name (write-to-string *new-name*)
                                                   :type :unspecific)))
          (progn (setq filename (catch-cancel (choose-new-file-dialog :prompt "save as midi fileÉ")))
                 (setq *place* filename)))
      (when (not (equal filename :cancel))
        (with-cursor *watch-cursor*
          (MFF-write (car gesture-to-save) filename :subs-too (cadr gesture-to-save)))))))

(defun MAKE-A-MIDI-GESTURE nil
  "calling (make-a-midi-gesture) 
       make-a-midi-gesture returned (#<*gesture* #x52b4f1> nil)"
  (let* ((midi-work (setq *algorithmic-composition* (mapcar #'(lambda (x)
                                                                (make-instance '*midi-event* :channel (fourth x) 
                                                                               :data1 (second x) :data2 (fifth x) 
                                                                               :duration (third x) :time-offset (first x) 
                                                                               :opcode 
                                                                               (if (zerop (third x)) #x80 144)))
                                                            (sortcar '< *events*
                                                                     ;;;(eval (first (eval (first *SELECTED-WORKS*))))
                                                                     ))))
         (new-gesture (make-instance '*gesture* :Alice-notes midi-work :name 'new-gesture)))
    (list (eval new-gesture) nil)))
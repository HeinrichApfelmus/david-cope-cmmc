


                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;       SPEAC Function/Chapter 7      ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;       SPEAC code to run SPEAC       ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

;;;this is the rough CL version of SPEAC

(defVar *UNISON* 0 "Interval weight designation.")
(defVar *MINOR-SECOND* 1 "Interval weight designation.")
(defVar *MAJOR-SECOND* .8 "Interval weight designation.")
(defVar *MINOR-THIRD* .225 "Interval weight designation.")
(defVar *MAJOR-THIRD* .2 "Interval weight designation.")
(defVar *PERFECT-FOURTH* .55 "Interval weight designation.")
(defVar *AUGMENTED-FOURTH* .65 "Interval weight designation.")
(defVar *PERFECT-FIFTH* .1 "Interval weight designation.")
(defVar *MINOR-SIXTH* .275 "Interval weight designation.")
(defVar *MAJOR-SIXTH* .25 "Interval weight designation.")
(defVar *MINOR-SEVENTH* .7 "Interval weight designation.")
(defVar *MAJOR-SEVENTH* .9 "Interval weight designation.")
(defVar *PERFECT-OCTAVE* 0 "Interval weight designation.")
;;;  Note that the following intervals are the same as their within-octave equivalents'
;;;  Altering the following intervals (by reducing, say, by .02) will make the output for
;;;  the book examples no longer match.
(defVar *MINOR-NINTH* 1 "Interval weight designation.")
(defVar *MAJOR-NINTH* .8 "Interval weight designation.")
(defVar *MINOR-TENTH* .225 "Interval weight designation.")
(defVar *MAJOR-TENTH* .2 "Interval weight designation.")
(defVar *PERFECT-ELEVENTH* .55 "Interval weight designation.")
(defVar *AUGMENTED-ELEVENTH* .65 "Interval weight designation.")
(defVar *PERFECT-TWELVETH* .1 "Interval weight designation.")
(defVar *MINOR-THIRTEENTH* .275 "Interval weight designation.")
(defVar *MAJOR-THIRTEENTH* .25 "Interval weight designation.")
(defVar *MINOR-FOURTEENTH* .7 "Interval weight designation.")
(defVar *MAJOR-FOURTEENTH* .9 "Interval weight designation.")
(defVar *PERFECT-FIFTEENTH* 0 "Interval weight designation.")
(defVar *MINOR-SIXTEENTH* 1 "Interval weight designation.")
(defVar *MAJOR-SIXTEENTH* .8 "Interval weight designation.")
(defVar *MINOR-SEVENTEENTH* .225 "Interval weight designation.")
(defVar *MAJOR-SEVENTEENTH* .2 "Interval weight designation.")
(defVar *PERFECT-EIGHTEENTH* .55 "Interval weight designation.")
(defVar *AUGMENTED-EIGHTEENTH* .65 "Interval weight designation.")
(defVar *PERFECT-NINETEENTH* .1 "Interval weight designation.")
(defVar *MINOR-TWENTIETH* .275 "Interval weight designation.")
(defVar *MAJOR-TWENTIETH* .25 "Interval weight designation.")
(defVar *MINOR-TWENTYFIRST* .7 "Interval weight designation.")
(defVar *MAJOR-TWENTYFIRST* .9 "Interval weight designation.")
(defVar *PERFECT-TWENTYSECOND* 0 "Interval weight designation.")
(defVar *MINOR-TWENTYTHIRD* 1 "Interval weight designation.")
(defVar *MAJOR-TWENTYTHIRD* .8 "Interval weight designation.")
(defVar *MINOR-TWENTYFOURTH* .225 "Interval weight designation.")
(defVar *MAJOR-TWENTYFOURTH* .2 "Interval weight designation.")
(defVar *PERFECT-TWENTYFIFTH* .55 "Interval weight designation.")
(defVar *AUGMENTED-TWENTYFIFTH* .65 "Interval weight designation.")
(defVar *PERFECT-TWENTYSIXTH* .1 "Interval weight designation.")
(defVar *MINOR-TWENTYSEVENTH* .275 "Interval weight designation.")
(defVar *MAJOR-TWENTYSEVENTH* .25 "Interval weight designation.")
(defVar *MINOR-TWENTYEIGHTH* .7 "Interval weight designation.")
(defVar *MAJOR-TWENTYEIGHTH* .9 "Interval weight designation.")
(defVar *PERFECT-TWENTYNINTH* 0 "Interval weight designation.")
(defVar *EXITS-AND-ENTRANCES* ())
(defVar NEW-ENTRANCE-TIME 0)
(defVar SIMULTANEOUS-EVENTS ())
(defVar EVENT ())
(defVar *OCTAVE-SEPARATION* .02)
(defVar *DOWNBEAT* 'downbeat)
(defVar *METRIC-TENSION-TABLE* '((4 (1 2)(2 2)(3 6)(4 2))
                                 (2 (1 2)(2 2))
                                 (3 (1 2)(2 2)(3 2))
                                 (6 (1 2)(2 2)(3 2)(4 8)(5 4)(6 3))
                                 (9 (1 2)(2 2)(3 2)(4 8)(5 4)(6 3)(7 14)(8 8)(9 4))))
(defVar *INTERVAL-LIST* '(*unison* *minor-second* *major-second* *minor-third* *major-third*
                             *perfect-fourth* *augmented-fourth* *perfect-fifth* *minor-sixth*
                             *major-sixth* *minor-seventh* *major-seventh* *perfect-octave* *minor-ninth*
                             *major-ninth* *minor-tenth* *major-tenth* *perfect-eleventh*
                             *augmented-eleventh* *perfect-twelveth* *minor-thirteenth* *major-thirteenth*
                             *minor-fourteenth* *major-fourteenth* *perfect-fifteenth* *minor-sixteenth*
                             *major-sixteenth* *minor-seventeenth* *major-seventeenth* *perfect-eighteenth*
                             *augmented-eighteenth* *perfect-nineteenth* *minor-twentieth*
                             *major-twentieth* *minor-twentyfirst* *major-twentyfirst*
                             *perfect-twentysecond* *minor-twentythird* *major-twentythird*
                             *minor-twentyfourth* *major-twentyfourth* *perfect-twentyfifth*
                             *augmented-twentyfifth* *perfect-twentysixth* *minor-twentyseventh*
                             *major-twentyseventh* *minor-twentyeighth* *major-twentyeighth*
                             *perfect-twentyninth*))
(defVar ROOT-STRENGTHS-AND-ROOTS '((7 0 1)(5 5 2)(4 0 3)(8 8 4)(3 0 5)(9 9 6)(2 2 7)(10 0 8)(1 1 9)(11 0 10)(0 0 11)(6 6 12))
"interval, root placement, strength with 1 being strongest")
(defVar BOOK-EXAMPLE '((0 45 1000 4 55) (0 64 1000 3 55) (0 69 1000 2 55)(0 73 1000 1 55)
                         (1000 57 1000 4 55)  (1000 64 1000 3 55)(1000 69 1000 2 55)(1000 73 500 1 55)(1500 74 500 1 55)
                         (2000 56 1000 4 55) (2000 64 1000 3 55) (2000 71 1000 2 55)(2000 76 1000 1 55) 
                         (3000 57 1000 4 55) (3000 64 1000 3 55) (3000 69 1000 2 55)(3000 73 1000 1 55)
                         (4000 54 1000 4 55) (4000 64 500 3 55)(4500 62 500 3 55)(4000 69 1000 2 55) (4000 69 1000 1 55) 
                         (5000 55 1000 4 55) (5000 62 1000 3 55) (5000 67 500 2 55)(5500 66 500 2 55)(5000 71 1000 1 55) 
                         (6000 57 1000 4 55) (6000 57 1000 3 55) (6000 64 1000 2 55)(6000 73 1000 1 55)
                         (7000 50 1000 4 55) (7000 57 1000 3 55) (7000 66 1000 2 55)(7000 74 1000 1 55)) "Figure 7.12 from book.")

#|(run-the-speac-weightings book-example 4 8 4)
(0.56 0.41 0.7799999999999999 0.51 1.33 0.51 1.26 0.51)

with the following for the example in the book:
vertical: (0.3 0.3 0.5 0.3 0.5 0.3 0.3 0.3)
metric:   (0.2 0.05 0.1 0.05000000000000001 0.2 0.05 0.1 0.05000000000000001)
duration: (0.06 0.06 0.08 0.06 0.08 0.06 0.06 0.06)
approach: (0 0 0.1 0.1 0.55 0.1 0.8 0.1)
here are the three main functions that show the various inportant windows for the book example and 
for establishing the basic speac analysis system:
(make-variables-window)
(make-analysis-window book-example)
(make-the-node-windows *levels*)
(make-form-window)
|#

;;;;;
#|(run-the-speac-weightings '((0 45 1000 4 55) (0 64 1000 3 55) (0 69 1000 2 55) . . . 4 8 4)
(0.56 0.41 0.7799999999999999 0.51 1.33 0.51 1.26 0.51)|#
;;;;;

(defun RUN-THE-SPEAC-WEIGHTINGS (events begin-beat total-beats meter)
  "This is the top-level for this SPEAC analysis program."
  (let ((vertical-tensions (create-lists-of-tensions (collect-pitch-lists (collect-beat-lists (break-at-each-entrance events)))))
        (metric-tensions (map-metric-tensions begin-beat total-beats meter))
        (duration-tensions (compute-duration-tensions events))
        (approach-tensions (get-root-motion-weightings events)))
    (map-add vertical-tensions metric-tensions duration-tensions approach-tensions)))

;;;;;
#|(map-add '(0.3 0.3 0.5 0.3 0.5 0.3 0.3 0.3) 
           '(0.2 0.05 0.1 0.05000000000000001 0.2 0.05 0.1 0.05000000000000001) 
           '(0.06 0.06 0.08 0.06 0.08 0.06 0.06 0.06) (0 0 0.1 0.1 0.55 0.1 0.8 0.1)) 
  (0.56 0.41 0.7799999999999999 0.51 1.33 0.51 1.26 0.51)|#
;;;;;

(defun MAP-ADD (list-1 list-2 list-3 list-4)
  "Maps addition across the various parameters of the analysis."
  (if (or (null list-1)(null list-2)(null list-3)(null list-4))()
      (cons (+ (first list-1)(first list-2)(first list-3)(first list-4))
            (map-add (rest list-1)(rest list-2)(rest list-3)(rest list-4)))))

;;;;;
#|(map-metric-tensions 4 8 4)
(0.2 0.05 0.1 0.05000000000000001 0.2 0.05 0.1 0.05000000000000001)|#
;;;;;

(defun MAP-METRIC-TENSIONS (start-beat total-beats meter &optional (beat 0))
  "Maps the metric tensions in a given meter."
  (if (equal total-beats beat)()
      (cons (lookup-and-figure-metric-tension meter start-beat)
            (map-metric-tensions (if (equal start-beat meter) 1 (1+ start-beat)) total-beats meter (1+ beat)))))

;;;;;
#|(lookup-and-figure-metric-tension 9 7)
0.05|#
;;;;;

(defun LOOKUP-AND-FIGURE-METRIC-TENSION (meter beat-number)
  "Looks up the relevant metric weight."
  (/ (* beat-number .1)
     (second (assoc beat-number (rest (assoc meter *metric-tension-table* :test #'equal)) :test #'equal))))

;;;;;
#|(CREATE-LISTS-OF-TENSIONS (COLLECT-pitch-lists (collect-beat-lists (BREAK-AT-EACH-ENTRANCE book-example))))
(0.3 0.3 0.5 0.3 0.5 0.3 0.3 0.3)|#
;;;;;

(defun CREATE-LISTS-OF-TENSIONS (groups-of-midi-notes)
  "Top-level function of the tension list creators."
  (compare (rate-lists (translate-groups-to-intervals groups-of-midi-notes))))

;;;;;
#|(compare '((0.3) (0.3 0.65) (0.5) (0.3) (0.92 0.5) (0.3 1.2) (0.3) (0.3)))
(0.3 0.3 0.5 0.3 0.5 0.3 0.3 0.3)|#
;;;;;

(defun COMPARE (beat-rating-lists)
  "Compares its ratings to find the most consonant for each beat."
  (loop for ratings in beat-rating-lists
        collect (apply #'min ratings)))

;;;;;
#|(collect-beat-lists '(((0 73 1000 1 55) (0 69 1000 2 55) . . .
((((0 73 1000 1 55) (0 69 1000 2 55) . . . |#
;;;;;

(defun COLLECT-BEAT-LISTS (entrance-lists)
  "Collects beat lists from its arg."
  (if (null entrance-lists)()
      (let ((test (group-beats entrance-lists)))
        (cons test
              (collect-beat-lists (nthcdr (length test) entrance-lists))))))

;;;;;
#| (group-beats '(((5000 71 500 1 55 *) (5000 67 500 2 55) . . .
(((5000 71 500 1 55 *) (5000 67 500 2 55) . . .|#
;;;;;

(defun GROUP-BEATS (entrance-lists)
  "Groups the beats together from its arg."
  (cond ((null entrance-lists)())
        ((anyp '(*) (mapcar #'sixth (first entrance-lists)))
         (cons (first entrance-lists)
               (group-beats (rest entrance-lists))))
        (t (list (first entrance-lists)))))

;;;;;
#|(collect-pitch-lists '((((0 73 1000 1 55) (0 69 1000 2 55) . . .
(((73 69 64 45)) ((73 69 64 57) . . .|#
;;;;;

(defun COLLECT-PITCH-LISTS (pitch-lists)
  "Collects the pitches from its arg."
  (loop for pitch-list in pitch-lists
        collect (collect-pitches pitch-list)))

;;;;;
#|(collect-pitches '(((6000 73 1000 1 55) (6000 64 1000 2 55) (6000 57 1000 3 55) (6000 57 1000 4 55))))
((73 64 57 57))|#
;;;;;

(defun COLLECT-PITCHES (lists)
  "Returns the pitches of each of the lists of events in its arg."
  (loop for events in lists
        collect (collect-pitch events)))
       
;;;;; 
#|(collect-pitch '((6000 73 1000 1 55) (6000 64 1000 2 55) (6000 57 1000 3 55) (6000 57 1000 4 55)))
(73 64 57 57)|#
;;;;;

(defun COLLECT-PITCH (events)
  "Returns the pitches of the events."
  (loop for event in events
        collect (second event)))

;;;;; 
#|(translate-groups-to-intervals '(((73 69 64 45)) ((73 69 64 57)  . . .
(((19 28)) ((7 16) (7 17)) ((8 15)) . . .|#
;;;;; 

(defun TRANSLATE-GROUPS-TO-INTERVALS (groups-of-midi-notes)
  "Translates groups of pitches into intervals."
  (loop for beat in groups-of-midi-notes
        collect (translate-to-intervals beat)))

;;;;; 
#|(translate-to-intervals '((73 69 64 45)))
((19 28))|#
;;;;; 

(defun TRANSLATE-TO-INTERVALS (groups-of-midi-notes)
  "Translates groups of pitches into intervals."
  (if (null groups-of-midi-notes)()
      (cons (let* ((arranged-midi-notes (remove-octaves (my-sort #'< (first groups-of-midi-notes))))
                   (bass-note (first arranged-midi-notes)))
              (loop for note in (rest arranged-midi-notes)
                    collect (- note bass-note)))
            (translate-to-intervals (rest groups-of-midi-notes)))))

;;;;; 
#|(rate-lists '(((19 28)) ((7 16) (7 17)) ((8 15))  . . .
((0.3) (0.3 0.65) (0.5) (0.3) (0.92 0.5) (0.3 1.2) (0.3) (0.3))|#
;;;;; 

(defun RATE-LISTS (lists-of-intervals)
  "Provides weights for its lists arg."
  (loop for list in lists-of-intervals
        collect (rate list)))

;;;;; 
#|(rate '((7 16))) 
  (0.3)|#
;;;;; 

(defun RATE (lists-of-intervals)
  "Translates its argument into weightings based on the stored values."
  (if (null lists-of-intervals)()
      (cons (my-round (apply #'+ (rate-the-intervals (first lists-of-intervals))))
            (rate (cdr lists-of-intervals)))))

;;;;; 
#|(rate-the-intervals '(7 16)) 
   (0.1 0.2)|#
;;;;; 

(defun RATE-THE-INTERVALS (list-of-intervals)
  "Returns the rating for the intervals in its argument."
  (if (null list-of-intervals)()
      (cons (eval (nth (first list-of-intervals) *interval-list*))
            (rate-the-intervals (cdr list-of-intervals)))))

;;;;; 
#|(my-round 1.01111)
   1.01|#
;;;;; 

(defun MY-ROUND (number)
  "Returns a number to two (if more) or one decimal points (if an integer)."
  (float (/ (round (* number 100)) 100)))

;;;;; 
#|(break-at-each-entrance ((0 45 1000 4 55) (0 64 1000 3 55)  . . .
(((0 73 1000 1 55) (0 69 1000 2 55)  . . .|#
;;;;;

(defun BREAK-AT-EACH-ENTRANCE (events)
  "Breaks events at each new entrance."
  (let ((ordered-events (sortcar '< (fix-the-triplets events))))
    (loop until (null ordered-events)
          ;;;this collects all the first simultaneous events.
          do (setf simultaneous-events (place-events-in-channel-order 
                                        (collect-simultaneous-events ordered-events)))
          ;;;this sets the new entrance time.
          do 
          (if *exits-and-entrances* 
            (setf new-entrance-time 
                  (get-new-exit-and-entrance-time ordered-events (very-first ordered-events)))
            (setf new-entrance-time 
                  (get-new-entrance-time ordered-events (very-first ordered-events)(+ (very-first ordered-events)(third (first ordered-events))))))
          ;;;this revises the collected events.
          collect 
          (reset-durations new-entrance-time simultaneous-events)
          ;;;this sets the ordered events for further collection.
          do (setq ordered-events (sortcar #'< (append (remove-zero-durations (reset-next-durations new-entrance-time simultaneous-events))
                                                       (remove-all simultaneous-events ordered-events)))))))

;;;;; 
#|(fix-the-triplets '((0 45 1000 4 55) (0 64 1000 3 55) (0 69 1000 2 55) (0 73 1000 1 55) (1000 57 1000 4 55) . . .
((0 73 1000 1 55) (1000 73 500 1 55)  . . .|#
;;;;;

(defun FIX-THE-TRIPLETS (events)
  "Removes the nils due to triplets."
    (loop for channel in (remove-nils (get-all-channels events))
          append (fix-triplets channel)))

;;;;; 
#|(get-all-channels '((0 45 1000 4 55) (0 64 1000 3 55) . . .
(((0 73 1000 1 55) (1000 73 500 1 55)  . . .|#
;;;;;

(defun GET-ALL-CHANNELS (events &optional (n 1))
  "Collects all channels in proper order."
  (loop until (equal n 17)
        collect (get-channel n events)
        do (setf n (1+ n))))

;;;;; 
#|(get-channel 1 ;((0 45 1000 4 55) (0 64 1000 3 55) . . .
((0 73 1000 1 55) (1000 73 500 1 55)  . . .|#
;;;;;

(defun GET-CHANNEL (n music)
  "Gets the nth channel of the music."
  (cond ((null music)())
        ((equal (fourth (first music)) n)
         (cons (first music)(get-channel n (cdr music))))
        (t (get-channel n (cdr music)))))

;;;;; 
#|(collect-simultaneous-events '((0 73 1000 1 55) (0 69 1000 2 55)  . . .
((0 73 1000 1 55) (0 69 1000 2 55) (0 64 1000 3 55) (0 45 1000 4 55))|#
;;;;;

(defun COLLECT-SIMULTANEOUS-EVENTS (events)
  "Returns from group of simultaneous events."
  (let ((ontime (very-first events)))
    (loop until (not (equal (very-first events) ontime))
          collect (first events)
          do (setf events (cdr events)))))

;;;;; 
#|(place-events-in-channel-order '((0 73 1000 1 55) (0 69 1000 2 55) (0 64 1000 3 55) (0 45 1000 4 55)))
((0 73 1000 1 55) (0 69 1000 2 55) (0 64 1000 3 55) (0 45 1000 4 55))|#
;;;;;

(defun PLACE-EVENTS-IN-CHANNEL-ORDER (events)
  "Places events in channel order."
  (loop for channel in (my-sort '< (mapcar #'fourth events))
        do (setf event (find-channel-event channel events))
        collect event
        do (setf events (remove event events :test #'equal :count 1))))

;;;;; 
#|(get-new-exit-and-entrance-time '((0 73 1000 1 55) . . .
1000|#
;;;;;

(defun GET-NEW-EXIT-AND-ENTRANCE-TIME (events start-time)
  "Returns  the new exit or entrance time."
  (let ((end-time (+ (get-shortest-duration start-time (sortcar #'< events)) start-time))
        (new-start-time (get-next-start-time start-time (sortcar #'< events))))
    (cond ((null end-time) new-start-time)
          ((null new-start-time) end-time)
          ((equal end-time new-start-time) end-time)
          ((> end-time new-start-time) new-start-time)
          (t end-time))))

;;;;; 
#|(get-shortest-duration 0 '((0 73 1000 1 55) (0 69 1000 2 55)  . . .
1000|#
;;;;;

(defun GET-SHORTEST-DURATION (start-time events)
  "Returns the shortest duration in its arg."
  (get-shortest (loop until (not (equal start-time (very-first events)))
                      collect (first events)
                      do (setf events (cdr events)))))

;;;;; 
#|(get-shortest '((0 73 1000 1 55) (0 69 1000 2 55) (0 64 1000 3 55) (0 45 1000 4 55)))
   1000|#
;;;;;

(defun GET-SHORTEST (events)
  "Retgurns the shortest duration of its arg."
  (first (sort (loop for event in events
                     collect (third event)) #'<)))

;;;;; 
#|(get-next-start-time 0 '((0 73 1000 1 55) (0 69 1000 2 55)  . . .
1000|#
;;;;;

(defun GET-NEXT-START-TIME (start-time events)
  "Gets the next event's start time."
  (cond ((null events)())
        ((equal start-time (very-first events))
         (get-next-start-time start-time (cdr events)))
        (t (very-first events))))

;;;;; 
#|(get-new-entrance-time '((0 73 1000 1 55) (0 69 1000 2 55) (0 64 1000 3 55) (0 45 1000 4 55) (1000 73 500 1 55) . . .
1000|#
;;;;;

(defun GET-NEW-ENTRANCE-TIME (events start-time end-time)
  "Gets the new entrance time."
  (if (equal start-time (very-first events))
    (get-new-entrance-time (cdr events) start-time end-time)
    (if (not (null (very-first events)))
      (very-first events)
      end-time)))

;;;;; 
#|(reset-durations 1000 '((0 73 1500 1 55) (0 69 1000 2 55) (0 64 1000 3 55) (0 45 1000 4 55)))
((0 73 1000 1 55 *) (0 69 1000 2 55) (0 64 1000 3 55) (0 45 1000 4 55))|#
;;;;;

(defun RESET-DURATIONS (new-entrance-time simultaneous-events)
  "Resets the durations to not exceed the new-entrance-time."
  (unless (null new-entrance-time)
    (loop for event in simultaneous-events
          collect (append (firstn 2 event)
                          (list (if (< (+ (third event)(first event)) new-entrance-time)
                                  (third event)
                                  (- new-entrance-time (first event))))
                          (nthcdr 3 event)
                          (if (> (+ (third event)(first event)) new-entrance-time)
                            '(*))))))

;;;;; 
#|(reset-next-durations 1000 '((0 73 1000 1 55) (0 69 1000 2 55) (0 64 1000 3 55) (0 45 1000 4 55))) 
  ((1000 73 0 1 55) (1000 69 0 2 55) (1000 64 0 3 55) (1000 45 0 4 55))|#
;;;;;

(defun RESET-NEXT-DURATIONS (new-entrance-time ordered-events)
  "Resets events to account for overlapped beats."
  (unless (null new-entrance-time)
    (loop for event in ordered-events
          collect (append (list new-entrance-time)
                          (list (second event))
                          (list (- (third event)(- new-entrance-time (first event))))
                          (nthcdr 3 event)))))

;;;;; 
#|(collect-simultaneous-events '((0 73 1000 1 55) (0 69 1000 2 55)  . . .
((0 73 1000 1 55) (0 69 1000 2 55) (0 64 1000 3 55) (0 45 1000 4 55))|#
;;;;;

(defun COLLECT-SIMULTANEOUS-EVENTS (events)
  "Collects simultaneous events."
  (let ((ontime (very-first events)))
    (loop until (not (equal (very-first events) ontime))
          collect (first events)
          do (setf events (cdr events)))))

;;;;; 
#|(remove-zero-durations '((1000 72 0 1 127) (1000 67 0 2 127) (1000 64 0 3 127) (1000 60 0 4 127))) 
  nil|#
;;;;;

(defun REMOVE-ZERO-DURATIONS (events)
  "Removes zero durations from events."
  (loop for event in events
        append (if (not (<= (third event) 0)) (list event))))

;;;;; 
#|(find-channel-event 1 '((0 73 1000 1 55) (0 69 1000 2 55) (0 64 1000 3 55) (0 45 1000 4 55)))
(0 73 1000 1 55)|#
;;;;;

(defun FIND-CHANNEL-EVENT (channel events)
  "Returns the first event in the channel."
  (if (or (null events)(equal channel (fourth (first events))))
    (first events)
    (find-channel-event channel (cdr events))))

;;;;; 
#|(fix-triplets '((0 73 1000 1 55) (1000 73 500 1 55) (1500 74 500 1 55) (2000 76 1000 1 55) (3000 73 1000 1 55) (4000 69 1000 1 55) (5000 71 1000 1 55) (6000 73 1000 1 55) (7000 74 1000 1 55))) 
   ((0 73 1000 1 55) . . .|#
;;;;;

(defun FIX-TRIPLETS (channel)
  "Fixes the triplet problem."
  (cond ((null (cdr channel)) channel)
        ((equal (+ (very-first channel)
                        (third (first channel)))
                (first (second channel)))
         (cons (first channel) (fix-triplets (cdr channel))))
        ((within-one (+ (very-first channel)
                        (third (first channel)))
                     (first (second channel)))
         (cons (append (firstn 2 (first channel))
                       (list (+ (- (first (second channel))
                                   (+ (very-first channel)
                                      (third (first channel))))
                                (third (first channel))))
                       (nthcdr 3 (first channel)))
               (fix-triplets (cdr channel))))
        (t (cons (first channel) (fix-triplets (cdr channel)))
                 )))

;;;;; 
#|(WITHIN-ONE 1 3)
  nil
  (WITHIN-ONE 1 2)
  t|#
;;;;;

(defun WITHIN-ONE (n1 n2)
  "Retruns t if args are within 1 of each other."
  (if (< (abs (- n1 n2)) 2) t))

;;;;; 
#|(remove-octaves '(60 67 64 72))
(60 67 64)|#
;;;;;

(defun REMOVE-OCTAVES (notes)
  "Removes all octave doublings from its argument."
  (if (null (cdr notes)) notes
      (cons (first notes)
            (remove-octaves (remove-note (first notes)(cdr notes))))))

;;;;; 
#|(remove-note 60 '(67 64 72))
(67 64)|#
;;;;;

(defun REMOVE-NOTE (note notes)
  "Removing octaves for remove-octaves."
  (if (null notes)()
      (let ((test (zerop (mod (- note (first notes)) 12))))
        (if test (remove-note note (cdr notes))
            (cons (first notes)(remove-note note (cdr notes)))))))

;;;;;
#|(compute-duration-tensions book-example)
(0.06 0.06 0.08 0.06 0.08 0.06 0.06 0.06)|#
;;;;;

(defun COMPUTE-DURATION-TENSIONS (events)
  "Computes the tensions for events."
  (let ((durations (duration-map (collect-beat-lists (break-at-each-entrance events))))
        (interval-tensions (create-lists-of-tensions 
                            (collect-pitch-lists 
                             (collect-beat-lists 
                              (break-at-each-entrance events))))))
    (loop for duration in durations
          collect (* .01 (round (* 100 (+ (* (/ duration 4000) .1)(* (first interval-tensions) .1)))))
          do (setf interval-tensions (rest interval-tensions)))))

;;;;;
#|(duration-map '((((0 73 1000 1 55) (0 69 1000 2 55) (0 64 1000 3 55) (0 45 1000 4 55))) (((1000 73 500 1 55) . . .
(1000 1000 1000 1000 1000 1000 1000 1000)|#
;;;;;

(defun DURATION-MAP (beats)
  "Maps the durations per beat."
  (let ((ontimes (loop for beat in beats
                       collect (first (very-first beat)))))
  (get-durations ontimes)))

;;;;;
#|(get-durations '(0 1000 2000 3000 4000 5000 6000 7000)) 
 (1000 1000 1000 1000 1000 1000 1000 1000)|#
;;;;;

(defun GET-DURATIONS (ontimes &optional (previous 0))
  "Returns the interval between ontimes."
  (if (null (rest ontimes)) (list previous)
      (cons (- (second ontimes)(first ontimes))
            (get-durations (rest ontimes)(- (second ontimes)(first ontimes))))))

;;;;;
#|(get-root-motion-weightings book-example)
(0 0 0.1 0.1 0 0.8 0.8 0.1)
these differ from book because the fourth beat of full measure here is taken as the onbeat chord not the 
offbeat chord. might want to change the book example|#
;;;;;

(defun GET-ROOT-MOTION-WEIGHTINGS (events)
  "Returns the weightings due to root motions."
  (let ((roots (get-chord-roots events)))
    (cons 0 (find-motion-weightings roots))))  ;the zero here is to account for the first chord not having an approach

;;;;;
#|(find-motion-weightings '(45 57 64 57 62 55 57 50)) 
 (0 0.1 0.1 0.55 0.1 0.8 0.1)|#
;;;;;

(defun FIND-MOTION-WEIGHTINGS (roots)
  "Finds the motions between chord roots in arg."
  (if (null (rest roots))()
      (cons (eval (nth (mod (abs (- (first roots)(second roots))) 12) *INTERVAL-LIST*))
           (find-motion-weightings (rest roots)))))

;;;;;
#|(get-chord-roots book-example)
(45 57 64 57 69 55 57 50)|#
;;;;;

(defun GET-CHORD-ROOTS (events)
  "Returns the chord roots of arg."
  (let* ((on-beat-pitch-lists (mapcar #'(lambda (x)(my-sort #'< x)) (mapcar #'scrunch 
                                                                            (collect-pitch-lists (collect-beat-lists (break-at-each-entrance events))))))
         (intervals (mapcar #'derive on-beat-pitch-lists))
         (roots (mapcar #'find-strongest-root-interval intervals)))
    (loop for root in roots
          collect (find-upper-lower root (my-sort #'< (find-interval-in-chord root (first on-beat-pitch-lists))))
          do (setq on-beat-pitch-lists (cdr on-beat-pitch-lists)))))

#| ??????? Calling (derive (55 64 84)) 
  derive returned (0 5 8 9)
  Calling (derive (72)) 
  derive returned (0)
  Calling (derive (41 74 76)) 
  derive returned (0 2 9 11)

|#

(defun derive (pitches)
  (my-sort #'< (remove-duplicates (derive-all-intervals pitches))))

(defun SCRUNCH (lists)
  (remove-duplicates (apply #'append lists)))

;;;;;
#|(find-upper-lower 7 '(45 64)) 
 45|#
;;;;;

(defun FIND-UPPER-LOWER (root interval)
  "Returns the root note."
  (let ((test (assoc root root-strengths-and-roots)))
     (if (zerop (second test))(first interval)(second interval))))

;;;;;
#|(find-strongest-root-interval '(0 7 4 0 0 9 5 0 8)) 
  7|#
;;;;;

(defun FIND-STRONGEST-ROOT-INTERVAL (intervals)
  "Returns the strongest root interval."
  (let ((test (find-root-strengths-and-roots intervals)))
    (nth (position (first (sortcaddr #'< test)) test :test #'equal) intervals)))

;;;;;
#|(find-root-strengths-and-roots '(0 7 4 0 0 9 5 0 8)) 
  ((0 0 11) (7 0 1) (4 0 3) (0 0 11) (0 0 11) (9 9 6) (5 5 2) (0 0 11) (8 8 4))|#
;;;;;

(defun FIND-ROOT-STRENGTHS-AND-ROOTS (pc-intervals)
  "Returns the root strengths and the roots of pcs."
  (if (null pc-intervals)()
      (cons (assoc (first pc-intervals) root-strengths-and-roots)
            (find-root-strengths-and-roots (rest pc-intervals)))))
  
;;;;;
#|(derive-all-intervals '(45 64 69 73)) 
 (0 7 0 4 0 5 9 0 4)|#
;;;;;

(defun DERIVE-ALL-INTERVALS (pitches)
  "Derives all possible intervals from pitches."
  (if (null (rest pitches)) '(0)
      (append (derive-intervals pitches)
              (derive-all-intervals (rest pitches)))))

;;;;;
#|(derive-intervals '(45 64 69 73)) 
  (0 7 0 4)|#
;;;;;

(defun DERIVE-INTERVALS (pitches &optional (first (first pitches)))
  "Derives all possible intervals from pitches."
  (if (null pitches)()
      (cons (mod (- (first pitches) first) 12)
            (derive-intervals (rest pitches) first))))


(defun SORTCADDR (function lists)
  "(sortcar '< '((2 (a b c))(3 (d e f))(1 (g h i))))
      >> ((1 (G H I)) (2 (A B C)) (3 (D E F)))"
  (mapcar #'reverse (sortcar function (mapcar #'reverse lists))))

;;;;;
#|(find-interval-in-chord 7 '(54 62 64 69))
(69 62)|#
;;;;;

(defun FIND-INTERVAL-IN-CHORD (interval chord)
  "Returns the interval in chord."
  (if (equal (length chord) 1) chord
             (find-it-in-chord interval (derive-all-pitches chord))))

;;;;;
#|(find-it-in-chord 7 '(54 54 54 62 54 64 54 69 62 62 62 64 62 69 64 64 64 69)) 
 (69 62)|#
;;;;;

(defun FIND-IT-IN-CHORD (interval chord)
  "Finds the interval in chord."
  (cond ((null chord)())
        ((equal interval (mod (- (second chord)(first chord)) 12))
         (list (second chord)(first chord)))
        (t (find-it-in-chord interval (rest chord)))))

;;;;;
#|(derive-all-pitches '(54 62 64 69))
(54 54 54 62 54 64 54 69 62 62 62 64 62 69 64 64 64 69)|#
;;;;;

(defun DERIVE-ALL-PITCHES (pitches)
  "Derives all of the pitches in arg."
  (if (null (rest pitches))()
      (append (apply #'append (derive-pitches pitches))
              (derive-all-pitches (rest pitches)))))

;;;;;
#|(derive-pitches '(54 62 64 69)) 
 ((54 54) (54 62) (54 64) (54 69))|#
;;;;;

(defun DERIVE-PITCHES (pitches &optional (pitch (first pitches)))
  "Derives all of the pitches in arg."
  (if (null pitches)()
      (cons (list pitch (first pitches))
            (derive-pitches (rest pitches) pitch))))





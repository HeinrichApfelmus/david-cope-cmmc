

                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;   Serendipity Function/Chapter 8    ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;     simple code to run search       ;;;;;
                   ;;;;;             functions               ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

#|This program operates using simple onboard search and retrieve functions for either
text or midi files on your computer, returning events for playing in either case.
Requires that you load the load-midi.lisp and play-midi-QT files first and then this file.
Running (serendipity) may take some time depending on how many files you have on your hard drive and which
route the program takes.
To play the results run (play-events (serendipity)) |#

(defVar *RS* (make-random-state t) "Variable for storing the current random state.")
(defVar *FILENAME* () "Where the name of the file is stored.")
(defVar *DATA* ())
(defVar *CURRENT-GESTURES* 'nil)
(defVar *MUSIC* ())
(defVar *FILE-NAME* ())
(defConstant $NOTEOFF #x80)
(defConstant $NOTEON #x90)
(defVar *TEMPO* 60)
(defVar *EVENTS* ()) 
(defVar *ALGORITHMIC-COMPOSITION* () "Temporary algorithmic music storage.")

;;;;;
#|(serendipity)
    ((0 33 1000 1 90) (1000 83 1000 1 90) (2000 65 1000 1 90) (3000 111 1000 1 90) . . .|#
;;;;;

(defun SERENDIPITY ()
  "Top-level function of the Serendipity function."
  (let ((choice (y-or-n-dialog "Which type of file do you wish to search for?" 
                               :yes-text "Midi"
                               :no-text "Text"
                               :cancel-text nil)))
    (if choice (find-a-midi-file)
        (make-events (let ((numbers (translate-text-file)))
                       (normalize numbers (apply #'min numbers)(apply #'max numbers) 36 106))))))

;;;;;
#|(find-a-midi-file)
   ((0 69 571 1 64) (571 72 571 1 64) (1143 71 571 1 64) (1714 74 857 1 64) . . . |#
;;;;;

(defun FIND-A-MIDI-FILE ()
  "Returns a Midi file found on the user's hard drive."
  (progn (setq *filename* (choose-one (work-through-folders (mix (find-all-folders-at-toplevel-of-hard-drive)))))
         (translate-midi-file)))

;;;;;
#|(work-through-folders
           '(#P"Macintosh HD:bach chorales mid/finale:" . . . 
   (#P"Macintosh HD:bach chorales mid/finale:bachchorales 10:251.mid" . . .|#
;;;;;

(defun WORK-THROUGH-FOLDERS (folder-pathnames)
  "Randomly finds a folder and searches through it for appropriate midi files."
  (cond ((null folder-pathnames)())
        ((find-mid-files (mix (directory (concatenate 'string 
                                                 (mac-namestring ;;;this gives me all of the folders in a chosen top-level folder
                                                  (first folder-pathnames)) 
                                                 "**:**") :directories t :files nil))))
        (t (work-through-folders (rest folder-pathnames)))))

;;;;;
#|(find-mid-files
            (#P"Macintosh HD:bach chorales mid/finale:bachchorales 10:" . . .
   (#P"Macintosh HD:bach chorales mid/finale:bachchorales 10:251.mid" . . . |#
;;;;;

(defun FIND-MID-FILES (folders)
  "Discovers midi files in the folder pathnames of its arg."
  (cond ((null folders)())
        ((directory (concatenate 'string
                                 (mac-namestring (first folders))
                                 "*.mid")))
        (t (find-mid-files (rest folders)))))

;;;;;
#|(translate-text-file)
   (104 105 116 97 252 101 101 98 114 50 44 49 57 252 101 . . .|#
;;;;;

(defun TRANSLATE-TEXT-FILE ()
  "Translates any text file into numbers for events."
  (get-text (find-a-text-file)))

;;;;;
#|(get-text #<input ccl::input-file-stream to "Macintosh HD:Palm:Holiday Files:Import Files:Canadian 
     Holidays">)
 (104 105 116 97 252 101 101 98 114 . . .|#
;;;;;

(defun GET-TEXT (stream &optional (n 100))
  "Returns pitch numbers for found text in stream."
  (if (or (null (tyi stream))(zerop n)) nil
      (cons (char-code (tyi stream))
            (get-text stream (1- n)))))

;;;;;
#|(find-a-text-file)
 #<input ccl::input-file-stream to "Macintosh HD:Palm:Holiday Files:Import Files:Canadian Holidays">|#
;;;;;

(defun FIND-A-TEXT-FILE ()
  "Fionds an appropriate text-only file for translation to events."
  (open (setq *filename* (work-through-the-folders (mix (find-all-folders-at-toplevel-of-hard-drive))))))

;;;;;
#|(work-through-the-folders
             (#P"Macintosh HD:aisc:" #P"Macintosh HD:Cleanup At Startup:" . . .
    #1P"Macintosh HD:Palm:Holiday Files:Import Files:Canadian Holidays"|#
;;;;;

(defun WORK-THROUGH-THE-FOLDERS (folder-pathnames)
  "Works through folders and tests for text status."
  (cond ((null folder-pathnames)())
        ((test-for-text-file-status (mix (directory (concatenate 'string 
                                                                 (mac-namestring ;;;this gives me all of the folders in a chosen top-level folder
                                                                  (first folder-pathnames)) 
                                                                 "**:**") :directories () :files t))))
        (t (work-through-the-folders (rest folder-pathnames)))))

;;;;;
#|(test-for-text-file-status
           (#2P"Macintosh HD:Opcode:.DS_Store" . . .
  #1P"Macintosh HD:Opcode:OMS Applications:OMS Tech Pubs:XTC Read Me"|#
;;;;;

(defun TEST-FOR-TEXT-FILE-STATUS (pathnames)
  "Tests for text file type."
  (cond ((null pathnames)())
        ((equal (mac-file-type (first pathnames)) ':text)(first pathnames))
       (t (test-for-text-file-status (rest pathnames)))))

;;;;;
#|(find-all-folders-at-toplevel-of-hard-drive)
  (#P"Macintosh HD:.Trashes:" #P"Macintosh HD:.vol:" . . .|#
;;;;;

(defun FIND-ALL-FOLDERS-AT-TOPLEVEL-OF-HARD-DRIVE ()
  "Returns pathnames for all of the toplevel folders on the hard drive."
   (directory  ;;;returns all of the folders on the main drive
    (concatenate 'string 
                 (mac-namestring 
                  (drive-name 0)) "*") 
    :directories t :files nil))


#|
;;;;;
#|(make-events '(60 62 64 52 54 55))
   ((0 60 1000 1 90) (0 62 1000 2 90) (1000 52 1000 1 90) (1000 54 1000 2 90))|#
;;;;;

(defun MAKE-EVENTS (pitch-groupings &optional (ontime 0))
  "Makes consecutive events out of the pairs of pitches in its arg."
  (if (null pitch-groupings) ()
      (append (list (make-event ontime (first pitch-groupings) 1))
              (make-events (rest pitch-groupings)(+ ontime 1000)))))

;;;;;
#|(make-event 0 60 1)
  (0 60 1000 1 90)|#
;;;;;

(defun MAKE-EVENT (ontime pitch channel)
  "Creates an event based on args."
  (list ontime
        (if (symbolp pitch) (eval pitch) pitch)
        1000
        channel
        90))
|#

;;;;;
#|(choose-one '(1 2 3 4 5)) 
   1|#
;;;;;

(defun CHOOSE-ONE (list)
  "Chooses a random member of its argument."
  (nth (random (length list) *rs*)
       list))

;;;;;
#|(mix '(4 2 5 3 6))
       (5 3 6 4 2)|#
;;;;;

(defun MIX (list)
  "Mixes its arg randomly."
  (let ((choice ()))
    (loop until (null list)
          do (setf choice (choose-one list))
          collect choice 
          do (setf list (remove choice list :count 1)))))

;;;;;
#|? (normalize (1.8508157176809255 -1.5298856564663974 -1.097537906304962 
-1.044212499898521 1.0088752655170414 -7.814716838125932 . . .
(104 94 95 95 101 75 108 106 102 102 38 . . .|#
;;;;;

(defun NORMALIZE (numbers min max low high)
  "Returns a list of normalized outputs of the cosine-function 1/cos x2."
  (if (null numbers)()
      (cons (normalize-it min max (first numbers) low high)
            (normalize (rest numbers) min max low high))))

;;;;;
#|(normalize -25.13146272476594 3.3267624923286805 -1.9036873597561845 24 108)
  93|#
;;;;;

(defun NORMALIZE-it (low1 high1 number low2 high2)
  "This function normalizes number into low-high-1 range to the low-high-2 range."
  (round (+ (* (/ (- number low1)(- high1 low1))(- high2 low2)) low2)))

;;;;;
#|The following functions make events for performing the above cosine function
as in (make-events (cosine 20 24 108))
   ((0 104 1000 1 90) (1000 94 1000 1 90) (2000 95 1000 1 90) . . . )|#
;;;;;

(defVar *RS* (make-random-state t) "Variable for storing the current random state.")

(defun MAKE-EVENTS (pitch-groupings &optional (ontime 0))
  "Makes consecutive events out of the pairs of pitches in its arg."
  (if (null pitch-groupings) ()
      (let ((duration (+ 250 (random 1750 *rs*))))
        (append (list (make-event ontime (first pitch-groupings) (1+ (random 16 *rs*))))
                (make-events (rest pitch-groupings)(+ ontime duration))))))

;;;;;
#|(make-event 0 60 1)
  (0 60 1000 1 90)|#
;;;;;

(defun MAKE-EVENT (ontime pitch channel)
  "Creates an event based on args."
  (list ontime
        (if (symbolp pitch) (eval pitch) pitch)
        1000
        channel
        90))
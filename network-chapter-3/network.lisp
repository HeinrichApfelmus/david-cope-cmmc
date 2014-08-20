

                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;     Network Function/Chapter 2      ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;     simple code to run network      ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

#|(run-neural-net)|#

#|This program operates using adaptive resonance theory (ART) based on the work of 
Gail Carpenter and Stephen Grossberg and following the general principles of
Mark Watson in his book Common Lisp Modules. The neural network here is 
unsupervised in that it does not use models for training but rather adapts to the 
patterns its fed. In this case, we use a musical encryption as follows:
0.0 | 0.05 | 0.1 | 0.15 | 0.2 |0.25 | 0.3 | 0.35 | 0.4 | 0.45 | 0.5 | 0.55 | 0.6 | 0.65 | 0.7 | 0.75 | 0.8 | 0.85 | 0.9 | 0.95 | 1.0
60  |  61  | 62  |  63  |  64 | 65  | 66  |  67  | 68  |  69  | 70  |  71  | 72  |  73  | 74  |  75  | 76  |  77  |  78 |  79  |  80
where the bottom row represents pitches and the row above it represents the 
encryption. The network pattern matches the incoming patterns and groups them according 
to similarity, composing by recombinancy using the similar patterns only. This very 
process uses that analytical capabilities of the neural network following an otherwise
data dependent model for composition. This represents but one of the many ways in which 
neural networks can be used for composition.|#

;;;Variables for use in Network
(defVar NUMBER-OF-OUTPUTS 5 "The number of outputs to the network.")
(defVar NUMBER-OF-INPUTS 5 "The number of inputs to the network.")
(defVar INPUT-PATTERNS '((0.0 0.2 0.1 0.25 0.35) (0.0 0.1 0.2 0.25 0.35) (0.0 0.2 0.25 0.35 0.45) 
                            (0.1 0.2 0.35 0.25 0.35) (0.2 0.1 0.2 0.25 0.45) (0.45 0.1 0.2 0.25 0.35) (0.2 0.2 0.1 0.25 0.35)
                            (0.35 0.25 0.2 0.25 0.35) (0.35 0.2 0.1 0.25 0.2) (0.1 0.25 0.2 0.25 0.35) 
                            (0.0 0.1 0.2 0.25 0.2) (0.25 0.2 0.1 0.2 0.25) (0.45 0.35 0.25 0.2 0.25) (0.0 0.1 0.2 0.25 0.2)
                            (0.0 0.0 0.1 0.2 0.25)) "The input patterns for the Network.")
(defVar ARRAY-1 (make-array (list number-of-inputs))) ;;;;old w
(defVar ARRAY-2 (make-array (list number-of-inputs))) ;;;;old x
(defVar ARRAY-3 (make-array (list number-of-inputs))) ;;;;old v
(defVar ARRAY-4 (make-array (list number-of-inputs))) ;;;;old r
(defVar ARRAY-5 (make-array (list number-of-inputs))) ;;;;old u
(defVar ARRAY-6 (make-array (list number-of-inputs))) ;;;;old q
(defVar ARRAY-7 (make-array (list number-of-inputs))) ;;;;old p
(defVar ARRAY-8 (make-array (list number-of-outputs))) ;;;;old y
;;(defVar TEMPorary-array-1 (make-array (list number-of-inputs)))
(defVar RESETVAL (make-array (list 1)))
(defVar Y (make-array (list number-of-outputs)))
(defVar RESET (make-array (list number-of-outputs)))
(defVar RESET-COUNTER (make-array (list number-of-outputs)))
(defVar NUMBER-OF-CATEGORIES (make-array (list number-of-outputs)))
;;(defVar TEMP2 (make-array (list number-of-outputs)))
(defVar WUP (make-array (list number-of-inputs number-of-outputs)))
(defVar WDOWN (make-array (list number-of-inputs number-of-outputs)))
(defVar *LEARNED-CATEGORIES* ())
(defVar LEARNING-CYCLE-COUNTER 0)
(defVar MAXIMUM-INDEX ())
(defVar SKIPRESET ())
(defVar INPUT (make-array (list number-of-inputs)))
(defVar DECIMALS '(0.0 0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65 0.7 0.75 
                   0.8 0.85 0.9 0.95 1.0))
(defVar PITCHES '(60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75  76 77 78 79 80))

; Model constants:
(defVar A 0.5)
(defVar B 0.2)
(defVar C -1.0)
(defVar D 0.4)
(defVar E 0.04)
(defVar ALPHA 1.0)
(defVar THETA 0.3)
(defVar VIGILANCE 0.94)
(defVar RESET-THRESHOLD 0.05)
(defVar UPLR 0.12)
(defVar DOWNLR 0.12)

;;;;;
#|(run-neural-net)
((0 60 1000 1 90) (1000 64 1000 1 90) (2000 62 1000 1 90)
 (3000 65 1000 1 90) . . .|#
;;;;;

(defun RUN-NEURAL-NET ()
  "Assumes patterns are in decimal form."
  (initialize-network 5 5)
  (initialize-the-network)
  (learn-the-patterns 50)
  (translate-into-events
   (translate-to-pitches (mapcar #'first 
                                      (firstn 5 
                                              (count-highest 
                                               (pair input-patterns (mapcar #'second *learned-categories*))))))))

;;;;;
#|(initialize-network 5 5) 
   nil|#
;;;;;

(defun INITIALIZE-NETWORK (numInputs numOutputs &optional trainingPatterns)
  "Initializes the neural network."
  ; Check for specified training patterns:
  (if trainingPatterns
    ; Make sure the number of input neurons agrees with
    ; the size of the training patterns:
    (if (equal (length (car trainingPatterns)) numInputs)
      (setq input-patterns trainingPatterns)
      (print
       (list
        "ERROR: bad input to initialize-network. numInputs should have been"
        (length (car trainingPatterns)))))
    ; No specified training patterns: use the default set
    ; defined in this package:
    (if (not (equal (length (car input-patterns)) numInputs))
      (print
       (list
        "ERROR: bad input to initialize-network. numInputs should have been"
        (length (car input-patterns))))
      ; Specified number of input neurons agrees with
      ; the size of the default training patterns defined
      ; in this package; proceed with defining network data:
      (progn
        ; Resets the network
        ; Define the network size:
        (setq number-of-inputs numInputs)
        (setq number-of-outputs numOutputs)
        ; Array storage allocation:
        (setq input (make-array (list number-of-inputs)))
        (setq array-1 (make-array (list number-of-inputs)))
        (setq array-2 (make-array (list number-of-inputs)))
        (setq array-3 (make-array (list number-of-inputs)))
        (setq array-4 (make-array (list number-of-inputs)))
        (setq array-5 (make-array (list number-of-inputs)))
        (setq array-6 (make-array (list number-of-inputs)))
        (setq array-7 (make-array (list number-of-inputs)))
        (setq resetVal (make-array (list 1)))
        (setq array-8 (make-array (list number-of-outputs)))
        (setq reset (make-array (list number-of-outputs)))
        (setq reset-counter (make-array (list number-of-outputs)))
        (setq Number-of-categories (make-array (list number-of-outputs)))
        (setq wUp (make-array (list number-of-inputs number-of-outputs)))
        (setq wDown (make-array (list number-of-outputs number-of-inputs)))
        ; Global variable to remember input patterns and
        ; their associated output category code for plotting
        ; by function ART2-Postprocess:
        (setq *learned-categories* nil)))))

;;;;;
#|(floating-point-random 0.05 0.1) 
    0.0801|#
;;;;;

(defun FLOATING-POINT-RANDOM (low high)
  "Floating point random numbers."
  (let ((range (- high low)))
    (+ (* (/ (random 1000) 1000.0) range) low)))

;;;;;
#|(find-the-largest-output) 
   2|#
;;;;;

(defun FIND-THE-LARGEST-OUTPUT (&aux (maximum-index 0) (mVal (aref array-8 0)))
  "Finds the largest output."
  (loop for output-number-index from 0 to (1- number-of-outputs)
    do (if (and
         (> (aref array-8 output-number-index) mVal)
         (not (aref reset output-number-index)))
      (setq mVal (aref array-8 output-number-index)
            maximum-index output-number-index)))
  maximum-index)

;;;;;
#|(check-array-value 2) 
   0.4|#
;;;;;

(defun CHECK-ARRAY-VALUE (index &aux mVal (MAXImum-iNDEX (find-the-largest-output)))
  "Returns d if (aref y index) is the largest value in array array-8 
   AND (aref array-8 index) has not been reset."
  mVal
  (if (and
       (equal index maximum-index)
       (not (aref reset maximum-index))
       (> (aref array-8 maximum-index) reset-threshold))
    d
    0.0))

;;;;;
#|(sigmoid-threshold-function 0.6302290114489392) 
  0.6302290114489392|#
;;;;;

(defun SIGMOID-THRESHOLD-FUNCTION (test)
  "Threshold function."
  (if (> test theta)
    test
    0.0))

;;;;;
#|(vector-L2-Norm #(0.0 0.0 0.0 0.0 0.0) 5) 
  0.001|#
;;;;;

(defun VECTOR-L2-NORM (vector vector-Length &aux (sum 0.0))
  "L2 Norm of a vector."
  (loop for length-index from 0 to (1- vector-Length)
        do (setq sum (+ sum (* (aref vector length-index) (aref vector length-index)))))
  (+ (sqrt sum) 0.001))

;;;;;
#|(Update-F1-STM-arrays)
  nil|#
;;;;;

(defun UPDATE-F1-STM-ARRAYS (&aux sum norm max1 max2)
  "Update F1 STM arrays."
  ; Calculate array-7 from array-5 input and backwards feed back:
  (loop for input-number-index from 0 to (1- number-of-inputs)
        do (progn
             (setq sum 0.0)
             (loop for output-number-index from 0 to (1- number-of-outputs)
                   do (setq sum (+ sum (* (check-array-value output-number-index) (aref wDown output-number-index input-number-index)))))
             (setf (aref array-7 input-number-index) (+ (aref array-5 input-number-index) sum))))
  ; Update array-6 using eq. 5
  (setq norm (+ (vector-L2-Norm array-7 number-of-inputs) e))
  (loop for input-number-index from 0 to (1- number-of-inputs)
        do  (setf (aref array-6 input-number-index) (/ (aref array-7 input-number-index) norm)))
  ; Update array-5 using eq. 6:
  (setq norm (vector-L2-Norm array-3 number-of-inputs))
  (loop for input-number-index from 0 to (1- number-of-inputs)
        do (setf (aref array-5 input-number-index) (/ (aref array-3 input-number-index) norm)))
  ; Update array-3 using eq. 7:
  (loop for input-number-index from 0 to (1- number-of-inputs)
        do (setf (aref array-3 input-number-index) (sigmoid-threshold-function (+ (aref array-2 input-number-index) (* b (sigmoid-threshold-function (aref array-6 input-number-index)))))))
  ; Update w using eq. 8:
  (loop for input-number-index from 0 to (1- number-of-inputs)
        do (setf (aref array-2 input-number-index) (/ (aref array-1 input-number-index) norm)))
  ; Update array-2 using eq. 9:
  (setq norm (+ (vector-L2-Norm array-1 number-of-inputs) e))
  (loop for input-number-index from 0 to (1- number-of-inputs)
        do (setf (aref array-2 input-number-index) (/ (aref array-1 input-number-index) norm)))
  ; Calculate reset array-4 from eq. 20:
  (setq max1 -1000.0 max2 -1000.0)
  (loop for input-number-index from 0 to (1- number-of-inputs)
        do (progn
             (if (< max1 (aref array-5 input-number-index)) (setq max1 (aref array-5 input-number-index)))
             (if (< max2 (aref array-7 input-number-index)) (setq max2 (aref array-7 input-number-index)))))
  (setq max1 (+ max1 0.001))
  (setq max2 (+ max2 0.001))
  (loop for input-number-index from 0 to (1- number-of-inputs)
        do (setf
            (aref array-4 input-number-index)
            (- (/ (aref array-5 input-number-index) max1) (/ (aref array-7 input-number-index) max2)))))

;;;;;
#|(Update-F2-STM-storage)
  nil|#
;;;;;

(defun UPDATE-F2-STM-STORAGE (&aux sum)
  "Updates F2 STM storage."
  (loop for output-number-index from 0 to (1- number-of-outputs)
        do (progn
             (setq sum 0.0)
             (loop for input-number-index from 0 to (1- number-of-inputs)
                   do (setq sum (+ sum (* (aref array-7 input-number-index) (aref wUp input-number-index output-number-index)))))
             (setf (aref array-8 output-number-index) sum)
             (if (aref reset output-number-index) (setf (aref array-8 output-number-index) -0.1)))))

;;;;;
#|(UPDATE-WEIGHTS)
  nil|#
;;;;;

(defun UPDATE-WEIGHTS (&aux (largest-output (find-the-largest-output)))
  "Updates the weights."
  (if (> (check-array-value largest-output) 0.02)
    (loop for increment from 0 to (1- number-of-inputs)
      do (setf
          (aref wDown largest-output increment)
          (+ (aref wDown largest-output increment)
             (*
              downLR
              d
              (- (aref array-7 increment) (aref wDown largest-output increment)))))
      do (setf
          (aref wUp increment largest-output)
          (+
           (aref wUp increment largest-output)
           (*
            upLR
            d
            (- (aref array-7 increment) (aref wUp increment largest-output))))))))

;;;;;
#|(Competitive-learning at-F2)
nil|#
;;;;;

(defun COMPETITIVE-LEARNING-AT-F2 (&aux (largest-output (find-the-largest-output)))
  "Competitive learning at slab F2."
  (if (> (aref array-8 largest-output) reset-threshold)
    (loop for output-number-index from 0 to (1- number-of-outputs)
          do (if (not (equal output-number-index largest-output))
               (setf (aref array-8 output-number-index) 0.0)))))

;;;;;
#|(Run-one-full-cycle)
  nil|#
;;;;;

(defun RUN-ONE-FULL-CYCLE ()
  "Run one full cycle."
  (update-f1-STM-arrays)
  (check-for-f2-reset)
  (competitive-learning-at-f2)
  (update-f2-STM-storage)
  (update-weights)
  (competitive-learning-at-f2))

;;;;;
#|(Check-for-F2-reset)
  nil|#
;;;;;

(defun CHECK-FOR-F2-RESET (&aux (res 0.0)(n1 (+ (vector-L2-Norm array-7 number-of-inputs) e)))
  "Check for an F2 reset condition."
  (if (and
       (> n1 0.2)
       (not skipReset))
    (if (> learning-cycle-counter 1)
      (if (> (aref array-8 (find-the-largest-output)) 0.25)
        (setq res (* 3.0 (vector-L2-Norm array-4 number-of-inputs))))  ; was 3.0
      (setq skipReset nil)))
  (setf (aref resetVal 0) res)
  (if (> res (- 1.9 vigilance))  ;; 11/14/91 change
    (progn
      (print (list "Vigilance reset =" res "  Learning cycle ="
                   learning-cycle-counter))
      (setq maximum-index (find-the-largest-output))
      (setf (aref reset maximum-index) 1)
      (setf (aref reset-counter maximum-index) 80))
    (loop for output-number-index from 0 to (1- number-of-outputs)
          do (setf (aref reset-counter output-number-index) (- (aref reset-counter output-number-index) 1))
          do (if (< (aref reset-counter output-number-index) 0)
               (progn
                 (if (aref reset output-number-index)  (setq skipReset t))
                 (setf (aref reset output-number-index) nil)))))
  (setq skipReset nil))

;;;;;
#|(zero-activations)
  nil|#
;;;;;

#|redo the dotimes to loop as in:

(loop for input-number-index from 0 to (1- number-of-intputs)
    do (progn (setf (aref array-1 input-number-index) 0.0)
              (setf (aref array-2 input-number-index) 0.0)
              (setf (aref array-3 input-number-index) 0.0)
              (setf (aref array-4 input-number-index) 0.0)
              (setf (aref array-5 input-number-index) 0.0)
              (setf (aref array-6 input-number-index) 0.0)
              (setf (aref array-7 input-number-index) 0.0)))
 

|#


(defun ZERO-ACTIVATIONS ()
  "Zero activations."
  (loop for input-number-index from 0 to (1- number-of-inputs)
        do (progn (setf (aref array-1 input-number-index) 0.0)
                  (setf (aref array-2 input-number-index) 0.0)
                  (setf (aref array-3 input-number-index) 0.0)
                  (setf (aref array-4 input-number-index) 0.0)
                  (setf (aref array-5 input-number-index) 0.0)
                  (setf (aref array-6 input-number-index) 0.0)
                  (setf (aref array-7 input-number-index) 0.0)))
  (loop for output-number-index from 0 to (1- number-of-outputs)
        do (progn (setf (aref array-8 output-number-index) 0)
                  (setf (aref reset output-number-index) 0)
                  (setf (aref reset-counter output-number-index) 0))))

;;;;;
#|(Set-learning-pattern '(0.0 0.2 0.1 0.25 0.35))
 nil|#
;;;;;

(defun SET-LEARNING-PATTERN (input-pattern &aux (length (length input-pattern)))
  "Sets up a learning pattern in the input neurons."
  (if (not (equal length number-of-inputs))
    (print (list "Error in Set-learning-pattern input:" input-pattern))
    (progn
      (setq learning-cycle-counter 0)
      (zero-activations)
      (loop for number from 0 to (1- length)
            do (setf (aref input number) (+ (pop input-pattern) (floating-point-random -0.08 0.08)))))))

;;;;;
#|(initialize-the-network)
nil|#
;;;;;

(defun INITIALIZE-THE-NETWORK ()
  "Initialize the network."
  (zero-activations)
  (loop for output-number-index from 0 to (1- number-of-outputs)
      do (progn
           (loop for input-number-index from 0 to (1- number-of-inputs)
                 do (setf
                     (aref wUp input-number-index output-number-index) (floating-point-random 0.05 0.1)
                     (aref wDown output-number-index input-number-index) (floating-point-random 0.01 0.03)))
           (setf (aref number-of-categories output-number-index) 0))))

;;;;;
#|(learn-the-patterns 50)
 nil|#
;;;;;

(defun LEARN-THE-PATTERNS (number)
  "Cycles through all training patterns once."
  (loop for input-pattern in input-patterns
    do (Set-learning-pattern input-pattern)
    do (dotimes (n number)
         (setq learning-cycle-counter (1+ learning-cycle-counter))
         (Run-one-full-cycle))
    do (setq *learned-categories*
          (cons (list array-7 (find-the-largest-output))
                *learned-categories*))))

;;;;;
#|(pair '(1 2 3) '(a b c))
((1 A) (2 B) (3 C))|#
;;;;;

(defun PAIR (one two)
  "Pairs the two args together."
  (if (or (null one)(null two))()
      (cons (list (first one)(first two))
            (pair (rest one)(rest two)))))

;;;;;
#|(make-note-decimals '((60 62)(63 65)))
((0.6 0.62) (0.63 0.65))|#
;;;;;

(defun MAKE-NOTE-DECIMALS (note-patterns)
  "Transforms note patterns into decimal patterns."
  (if (null note-patterns)()
      (cons (mapcar #'(lambda (x)(* x .01)) (first note-patterns))
            (make-note-decimals (rest note-patterns)))))

;;;;;
#|(firstn 2 '(a b c d e))
      (A B)|#
;;;;;

(defun FIRSTN (number list)
  "Returns the first n of list."
 (if (< (length list) number)(firstn (1- number) list)
     (butlast list (- (length list) number))))

;;;;;
#|(translate-to-pitches '((0.0 0.1)(0.3 0.4)))
((60 62) (66 68))|#
;;;;;

(defun TRANSLATE-TO-PITCHES (decimal-lists)
  "Transforms decimal patterns into note patterns."
  (if (null decimal-lists)()
      (cons (translate-pitches (first decimal-lists))
            (translate-to-pitches (rest decimal-lists)))))

;;;;;
#|(translate-pitches '(0.0 0.1))
(60 62)|#
;;;;;

(defun TRANSLATE-PITCHES (decimal-numbers)
  "Helps transform decimal patterns into note patterns."
  (if (null decimal-numbers)()
      (let ((test (nth (position (first decimal-numbers) decimals) pitches)))
        (cons (if (null test) .69 test)
            (translate-pitches (rest decimal-numbers))))))

;;;;;
#|(translate-to-decimals '((60 62)(63 65)))
((0.0 0.1) (0.15 0.25))|#
;;;;;

(defun TRANSLATE-TO-DECIMALS (pitch-lists)
  "Changes lists of pitches into lists of 0.0 - 1.0 range decimals."
    (if (null pitch-lists)()
      (cons (translate-decimals (first pitch-lists))
            (translate-to-decimals (rest pitch-lists)))))

;;;;;
#|(translate-decimals '(60 62))
(0.0 0.1)|#
;;;;;

(defun TRANSLATE-DECIMALS (pitch-numbers)
  "Changes pitches into 0.0 - 1.0 range decimals."
  (if (null pitch-numbers)()
      (cons (nth (position (first pitch-numbers) pitches) decimals)
            (translate-decimals (rest pitch-numbers)))))

;;;;;
#|(TRANSLATE-INTO-EVENTS '((60 64 65 67 69) (64 64 62 65 67) (67 65 64 65 67) (62 65 64 65 67)))
((0 60 1000 1 90) (1000 64 1000 1 90) (2000 65 1000 1 90) (3000 67 1000 1 90) (4000 69 1000 1 90) (5000 64 1000 1 90) . . .|#
;;;;;

(defun TRANSLATE-INTO-EVENTS (output-pitch-lists)
  "Returns sontiguous events from its pitch-lists arg."
  (make-events (apply 'append output-pitch-lists)))

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

;;;;;
#|(COUNT-HIGHEST (((0.0 0.2 0.1 0.25 0.35) 4) ((0.0 0.1 0.2 0.25 0.35) 1) ((0.0 0.2 0.25 0.35 0.4
  (((0.0 0.1 0.2 0.25 0.35) 1) ((0.2 0.1 0.2 0.25 0.45) 1) ((0.0 0.1 0.2 0.25 0.2) 1) 
   ((0.25 0.2 0.1 0.2 0.25) 1) ((0.45 0.35 0.25 0.2 0.25) 1))|#
;;;;;

(defun COUNT-HIGHEST (lists)
  "Returns the highest occuring pattern in its arg."
  (let* ((sorted-numbers (my-sort #'< (mapcar #'second lists))) ;;;(1 1 2 2 2 3 3 3 )
         (numbers-only (remove-duplicates sorted-numbers))      ;;;(1 2 3)
         (counts (count-them numbers-only sorted-numbers)))     ;;;(5 2 3)
    (find-all (nth (position (first (my-sort #'> counts)) counts) numbers-only)  lists)))

;;;;;
#|(COUNT-THEM '(0 1 2 3 4) '(0 1 1 1 1 1 2 3 3 3 4 4 4 4 4)) 
(1 5 1 3 5)|#
;;;;;

(defun COUNT-THEM (singles numbers)
  "Returns the counts of its first arg in its second arg."
  (if (null singles)()
      (cons (count (first singles) numbers)
            (count-them (rest singles) numbers))))
   
;;;;;
#|(FIND-ALL 1 '(((0.0 0.2 0.1 0.25 0.35) 4) ((0.0 0.1 0.2 0.25 0.35) 1) . . . )) 
 (((0.0 0.1 0.2 0.25 0.35) 1) ((0.2 0.1 0.2 0.25 0.45) 1) ((0.0 0.1 0.2 0.25 0.2) 1) . . .|#
;;;;;

(defun FIND-ALL (number lists)
  "Returns all of the patterns associated by cdr with number."
  (cond ((null lists)())
        ((equal (second (first lists)) number)
         (cons (first lists)
               (find-all number (rest lists))))
        (t (find-all number (rest lists)))))
      
;;;;;
#|(MY-SORT #'< '(4 1 0 3 1 4 3 2 4 3 1 1 1 4 4)) 
  (0 1 1 1 1 1 2 3 3 3 4 4 4 4 4)|#
;;;;;

(defun MY-SORT (function lists)
  "Non-destructive sort function."
  (loop for item in (sort (loop for array-2 in lists
                                collect (list array-2))  function :key #'car)
        collect (first item)))
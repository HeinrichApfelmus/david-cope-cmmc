

                     ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                     ;;;;;            By David Cope            ;;;;;
                     ;;;;;      Fuzzy Function/Chapter 2       ;;;;;
                     ;;;;;             COMMON LISP             ;;;;;
                     ;;;;;      simple code to run fuzzy       ;;;;;
                     ;;;;;               function              ;;;;;
                     ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

#|(fuzzy '(0 4 7 5 7 11 0))|#

;;;  The basic code for this program was developed by Peter Elsea at the University
;;;  of California at Santa Cruz.

; idevault values
(defVar *DEFAULT_DUR* 1000)
(defVar *DEFAULT_CH* 1)
(defVar *DEFAULT_VEL* 127)

; standard scales, chords, and notes
(defVar *MAJ_SCALE* '(1 0 1 0 1 1 0 1 0 1 0 1))
(defVar *NMIN_SCALE* '(1 0 1 1 0 1 0 1 1 0 1 0))
(defVar *HMIN_SCALE* '(1 0 1 1 0 1 0 1 1 0 0 1))
(defVar *CHORD_TEMPLATE* '(1 0 0 1 1 0 0 1 0 0 0 0))
(defVar *FZ_TRIAD* '(1 0 0 1 1 0 0.5 1 0 0 0 0))
(defVar *FZ_KEY* '(1 0 1 0 1 1 0 1 0 1 0 1))
(defVar *FZ_NOTE* '(1 0 0 0 0 0 0 0 0 0 0 0))
(defVar *CURRENT_SCALE* *maj_scale*)

; intervals up
(defVar *FZ_2* '(0 0.9 1 0 0 0 0 0 0 0 0 0))
(defVar *FZ_3* '(0 0 0 1 0.9 0 0 0 0 0 0 0))
(defVar *FZ_4* '(0 0 0 0 0 1 0.5 0 0 0 0 0))
(defVar *FZ_5* '(0 0 0 0 0 0 0.9 1 0.5 0 0 0))
(defVar *FZ_6* '(0 0 0 0 0 0 0 0 1 0.9 0 0))
(defVar *FZ_7* '(0 0 0 0 0 0 0 0 0 0 0.9 1))

;intervals down
(defVar *FZ_2B* '(0 0 0 0 0 0 0 0 0 0 1 0.9))
(defVar *FZ_3B* '(0 0 0 0 0 0 0 0 0.9 1 0 0))
(defVar *FZ_4B* '(0 0 0 0 0 0 0.5 1 0 0 0 0))
(defVar *FZ_5B* '(0 0 0 0 0.5 1 0.9 0 0 0 0 0))
(defVar *FZ_6B* '(0 0 0.9 1 0 0 0 0 0 0 0 0))
(defVar *FZ_7B* '(0 0.9 1 0 0 0 0 0 0 0 0 0))

(defVar *SOL_SET* '(0 0 0) "places are (as-root as_third as_fifth)")
(defVar *LAST_SOL* '(0 0 0))
(defVar *OLD_CHORD_SET* '(0 0 0 0 0 0 0 0 0 0 0 0))  ; pc set
(defVar *AS_ROOT_SET* '(0 0 0 0 0 0 0 0 0 0 0 0))
(defVar *AS_THIRD_SET* '(0 0 0 0 0 0 0 0 0 0 0 0))
(defVar *AS_FIFTH_SET* '(0 0 0 0 0 0 0 0 0 0 0 0))

; pitch classes
(defConstant PC_C 0)
(defConstant PC_DFLAT 1)
(defConstant PC_D 2)
(defConstant PC_EFLAT 3)
(defConstant PC_E 4)
(defConstant PC_F 5)
(defConstant PC_GFLAT 6)
(defConstant PC_G 7)
(defConstant PC_AFLAT 8)
(defConstant PC_A 9)
(defConstant PC_BFLAT 10)
(defConstant PC_B 11)

;;;;;
#|
? (fuzzy '(0 4 7 5 7 11 0))
((0 48 1000 1 127) (0 52 1000 1 127) (0 55 1000 1 127) (1000 48 1000 1 127)
  (1000 52 1000 1 127) (1000 55 1000 1 127) (2000 55 1000 1 127) (2000 59 1000 1 127)
  (2000 62 1000 1 127) (3000 50 1000 1 127) (3000 53 1000 1 127) (3000 57 1000 1 127) . . .|#
;;;;;

(defun FUZZY (pcs &optional (time 0))
      (if (null pcs)()
          (append (pick_and_play_more_rules_chord (first pcs) time)
                  (fuzzy (rest pcs) (+ time 1000)))))

;;;;;
#|(PICK_AND_PLAY_MORE_RULES_CHORD 5)
     ((0 50 1000 1 127) (0 53 1000 1 127) (0 57 1000 1 127))|#
;;;;;

(defun PICK_AND_PLAY_MORE_RULES_CHORD (thenote time)
    "Returns a chord in event notation."
          (make_chord_event
           (add_oct
            (pick_chord_w_more_rules (rem thenote 12)) 4) time))

;;;;;
#|(ADD_LISTS '(1 2 3) '(4 5 6))
(5 7 9)|#

(defun ADD_LISTS (list1 list2)
   "Add two lists, member by member"
     (mapcar #'+ list1 list2))

;;;;;
#|(PICK_CHORD_W_MORE_RULES 5)
     (2 5 9)|#
;;;;;

(defun PICK_CHORD_W_MORE_RULES (thenote)
    "Returns pitch-classes of a chord."
    (let ((the_pc (rem thenote 12)) (solSet '(0 0 0)))
      (setq *as_root_set* (make_Set (as_root the_pc)))
      (setq *as_third_set* (make_Set (as_third the_pc)))
      (setq *as_fifth_set* (make_Set (as_fifth the_pc)))
      (setq solSet (add_lists solSet (common_tones_test)))
      (setq solSet (add_lists solSet (last_sol_test)))
      (setq solSet (add_lists solSet (favor_root_for_tonic the_pc)))
      (setq solSet (add_lists solSet (dither)))
      (case (ltop (setq *last_sol* solset))
        (2 (as_fifth the_pc))
        (1 (as_third the_pc))
        (otherwise (as_root the_pc)))))

;;;;;
#|(MAKE_SET '(7 11 2))
     (0 0 1 0 0 0 0 1 0 0 0 1)|#
;;;;;

(defun MAKE_SET (pitch_list)
         (if (null pitch_list) '(0 0 0 0 0 0 0 0 0 0 0 0)
             (fz_union (ror_n *fz_note* (first pitch_list))
                       (make_set (rest pitch_list)))))

;;;;;
#|(ROR_N '(0 0 0 0 0 0 0.9 1 0.5 0 0 0) 7)
     (0 0.9 1 0.5 0 0 0 0 0 0 0 0)|#
;;;;;

(defun ROR_N (lst n)
    "Rotates 12 element list n steps to right."
    (let ((shift_by (- 12 n)))
      (append (subseq lst shift_by) (subseq lst 0 shift_by))))

;;;;;
#|(FZ_UNION '(1 0 0 0 0 0 0 0 0 0 0 0) '(0 0 0 0 0 0 0 0 0 0 0 0))
     (1 0 0 0 0 0 0 0 0 0 0 0)|#
;;;;;

(defun FZ_UNION (&rest theLists)
"Returns fuzzy union (maximums) of any number of lists"
     (fz_u theLists))

;;;;;
#|(favor_root_for_tonic 0) 
    (0.1 0 0)|#
;;;;;

(defun FAVOR_ROOT_FOR_TONIC (thePC)
   "Gives slight edge to root position for tonic"
   (if (zerop thePC) '(0.1 0 0) '(0 0 0)))

;;;;;
#|(dither) 
  (0 0.05 0)|#
;;;;;

(defun DITHER ()
     "Random tie breaker"
     (let ((choice (random 3.0)))
       (cond
        ((< choice 1) '(0.05 0 0))
        ((< choice 2) '(0 0.05 0))
        (t '(0 0 0.05)))))

;;;;;
#|(COMMON_TONES_TEST)
    (0 0 0)|#
;;;;;

(defun COMMON_TONES_TEST ()
    "Applies common tone rules."
       (common_tone_rules *as_root_set* *as_third_set* *as_fifth_set* *old_chord_set*))

;;;;;
#|(COMMON_TONE_RULES '(1 0 0 0 0 1 0 0 0 1 0 0) '(0 0 1 0 0 1 0 0 0 1
0 0) '(0 0 1 0 0 1 0 0 0 0 0 1) '(0 0 0 0 0 0 0 0 0 0 0 0))
    (0 0 0)|#
;;;;;

(defun COMMON_TONE_RULES (set1 set2 set3 master)
    "Returns set according to common tone rules."
       (let ((result '(0 0 0)))
         (setf (first result) (common_tones set1 master))
         (setf (second result) (common_tones set2 master))
         (setf (third result) (common_tones set3 master))
         result))

;;;;;
#|(COMMON_TONES '(1 0 0 0 0 1 0 0 0 1 0 0) '(0 0 0 0 0 0 0 0 0 0 0 0))
       0|#
;;;;;

(defun COMMON_TONES (set1 set4)
"Sums the set intersections."
       (sumup (fz_intersect set1 set4)))

;;;;;
#|(ltop (1 1 0))
    0|#
;;;;;

(defun LTOP (thelist)
  "Returns the topmost position of the list."
    (car (top_n_positions thelist 1)))

;;;;;
#|(FZ_COMPLEMENT '(1.0 0.0 1.0))
     (0.0 1.0 0.0)|#
;;;;;

(defun FZ_COMPLEMENT (alist &optional (compvalue 1.0))
    "Returns fuzzy complement (1 - membership) of a list."
       (if (null alist)
         nil
         (cons (- 1 (first alist) )
               (fz_complement (rest alist) compvalue))))

;;;;;
#|(FZ_INTERSECT '(1 0 1 0 1 1 0 1 0 1 0 1) '(0 0 0 0 0 0 0 0 1 0.9 0 0))
      (0 0 0 0 0 0 0 0 0 0.9 0 0)|#
;;;;;

(defun FZ_INTERSECT (lst1 lst2)
       "Returns fuzzy intersection (minimums) of two lists"
       (if (null lst1)
         nil
         (cons (minumum (first lst1) (first lst2))
               (fz_intersect (rest lst1) (rest lst2)))))

;;;;;
#|(MAXIMUM 1 0)
     1|#
;;;;;

(defun MAXIMUM (x y)
    "Returns the maximum of its two args."
     (if (and (realp x) (realp y))
              (max x  y)
              0))

;;;;;
#|(MINUMUM 1 0)
     0|#
;;;;;

(defun MINUMUM (x y)
    "Returns the minimum of its two args."
     (if (and (realp x) (realp y))
              (min x  y)
              0))

;;;;;
#|(FZ_UN '(1 0 0 0 0 0 0 0 0 0 0 0) '(0 0 0 0 0 0 0 0 0 0 0 0))
     (1 0 0 0 0 0 0 0 0 0 0 0)|#
;;;;;

(defun FZ_UN (lst1 lst2)
       "Returns fuzzy union (maximums) of two lists."
       (if (null lst1)
         nil
         (cons (maximum (first lst1) (first lst2))
               (fz_union (rest lst1) (rest lst2)))))

;;;;;
#|(FZ_U '((1 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0)))
     (1 0 0 0 0 0 0 0 0 0 0 0)|#
;;;;;

(defun FZ_U (theLists)
   "Returns fuzzy union (maximums) of many lists."
     (if (null (rest theLists))
               (first thelists)
               (fz_un (first theLists) (fz_u (rest theLists)))))

;;;;;
#|(SUMUP '(0 0 0 0 0 0 0 0 0 0.9 0 0))
      0.9|#
;;;;;

(defun SUMUP (theList)
     "Adds the members of a list."
     (if (null thelist)
       0
       (+ (first theList) (sumup (rest theList)))))

;;;;;
#|(TOP_N_POSITIONS '(0 0 1 0 0 0 0 0 0 0 0 0) 1)
      (2)|#
;;;;;

(defun TOP_N_POSITIONS (thelist howmany)
    "Returns positions of highest n values in list, or nil if all 0."
    (if (zerop howmany)  ; finished
      nil
      (if (null (sumup theList)) ; empty list
        nil
        (let (alist )
          (setf alist (copy-list thelist))  ;   copy input list
          (sort
           (cons
            (let ( ( i 0) (k 0) (hsf 0)) ;  hsf is highest so far
              (dolist (x alist )
                (if (> x hsf)
                  (setf hsf x k i))
                (setf i (+ i 1)))
              (setf (elt alist k) 0)
              k) ; k is consed
            (top_n_positions alist (1- howmany)))
           #'<)))))

;;;;;
#|(ASCEND_LIST '(2 5 9))
     (2 5 9)|#
;;;;;

(defun ASCEND_LIST (thelist)
     "Adds 12 to pitches lower than first one keeps chords in root position."
       (let ((root (first thelist)))
         (mapcar #'(lambda (x) (if (< x root) (+ x 12) x))
                 thelist)))

;;;;;
#|(ADD_OCT '(2 5 9) 4)
      (50 53 57)|#
;;;;;

(defun ADD_OCT (theList &optional (theOctave 4))
    "Adds octave to the list."
    (mapcar #'(lambda (x) (+ x (* theoctave 12)))
            (ascend_list theList)))

;;;;;
#|(MAKE_CHORD_EVENT '(50 53 57))
     ((0 50 1000 1 127) (0 53 1000 1 127) (0 57 1000 1 127))|#
;;;;;

(defun MAKE_CHORD_EVENT (pitch_list &optional (time 0))
    "Simple notelist to chord."
    (if (null pitch_list)()
        (cons (list time (first pitch_list) *default_dur* *default_ch* *default_vel*)
              (make_chord_event (rest pitch_list) time))))

;;;;;
#|(THIRD_ABOVE 5)
     9|#
;;;;;

(defun THIRD_ABOVE (thenote)
    "Returns a third above the note."
    (car (top_n_positions (fz_intersect *current_scale* (ror_n *fz_3* thenote)) 1)))

;;;;;
#|(FIFTH_ABOVE 7)
     2|#
;;;;;

(defun FIFTH_ABOVE (thenote)
    "Returns a fifth above the note."
    (car (top_n_positions (fz_intersect *current_scale* (ror_n *fz_5* thenote)) 1)))

;;;;;
#|(THIRD_BELOW 5)
     2|#
;;;;;

(defun THIRD_BELOW (thenote)
    "Returns a third below the note."
    (car (top_n_positions (fz_intersect *current_scale* (ror_n *fz_3b* thenote)) 1)))

;;;;;
#|(fifth_below 5)
     11|#
;;;;;

(defun FIFTH_BELOW (thenote)
    "Returns a fifth below the note."
    (car (top_n_positions (fz_intersect *current_scale* (ror_n *fz_5b* thenote)) 1)))

;;;;;
#|(AS_ROOT 5)
      (5 9 0)|#
;;;;;

(defun AS_ROOT (thenote)
    "Returns pitch-class chord with arg as root."
    (list thenote (third_above thenote) (fifth_above thenote)))

;;;;;
#|(AS_THIRD 5)
     (2 5 9)|#
;;;;;

(defun AS_THIRD (thenote)
    "Returns pitch-class chord with arg as third."
    (list  (third_below thenote) thenote (fifth_above (third_below thenote))))

;;;;;
#|(AS_FIFTH 5)
     (11 2 5)|#
;;;;;

(defun AS_FIFTH (thenote)
    "Returns pitch-class chord with arg as fifth."
    (list (fifth_below thenote) (third_above (fifth_below thenote)) thenote))

;;;;;
#|(LAST_SOL_TEST)
      (0 1 1)|#
;;;;;

(defun LAST_SOL_TEST ()
    "Based on *last_sol*."
       (let ((test (car (top_n_positions *last_sol* 1))))
         (cond
          ((= 0 test) '(0 1 1))
          ((= 1 test) '(1 1 0))
          (t '(1 0 0)))))

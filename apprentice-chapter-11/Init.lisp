


                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;        Apprentice/Chapter 11        ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;    simple code to run Apprentice    ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

;========================
;some global and general variable declarations
;========================

(defVar *EVENTS* () "Standard location of all musical events.")
(defVar *PROCESS* () "Storing the play process.")
(setq *warn-if-redefine* ())
(defVar *BEGIN-TIME-IN-BEATS* 1 "Setting the begin time on the first beat.")
(defVar *END-TIME-IN-MEASURES* 1000 "Setting the end time on the thousandsth measure.")
(defVar *END-TIME-IN-BEATS* 1000 "Setting the end time on the thousandsth beat.")
(defVar DAVIDCOPE () "Parts of the all-about-window.")
(defVar COMPOSING () "Parts of the all-about-window.")
(defVar ENVIRONMENT () "Parts of the all-about-window.")
(defVar COPE-TEXT () "Parts of the all-about-window.")
(defVar *REGION-BUTTON* () "Parts of the region-window.")
(defVar *BEGIN-TIME-IN-MEASURES* 1 "Begin time in measures.")

(defClass ALL-ABOUT-WINDOW (window)
  nil
  (:default-initargs :window-type :double-edge-box 
    :window-title ""
    :view-position #@(40 40)
    :view-size #@(300 180)
    :close-box-p ()
    :auto-position :centermainscreen))

(defMethod INITIALIZE-INSTANCE ((window ALL-ABOUT-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs)
  (add-subviews window 
    (setq davidcope (make-instance 'static-text-dialog-item
                  :dialog-item-text "Apprentice"
                  :view-position '#@(42 26)
                  :view-size #@(322 54)
                  :view-font '("times" 46)))
    (setq Cope-text (make-instance 'static-text-dialog-item
                 :dialog-item-text "© David Cope 1992 -"
                 :view-position '#@(80 140)
                 :view-size #@(334 54)
                 :view-font '("times" 16 :bold))))
  (set-part-color davidcope :text *red-color*)
  (set-part-color Cope-text :text *dark-green-color*))

(defVar HELLO (make-instance 'all-about-window))
(sleep 2)
(progn 
                (setq *save-doc-strings* t)
                (setq *arglist-on-space* t)
                (setq *save-local-symbols* t)
                (setq *print-case* :downcase)
                (setq *paste-with-styles* ())
                (setq *fasl-save-local-symbols* t)
                (setq *save-definitions* t)
                (setq *break-on-errors* ())
                (setq *trace-print-length* 100)
                (setq *trace-print-level* 20)
                (setq *print-pretty* t)
                (load "ccl:apprentice-chapter-11;utilities.lisp")
                (load "ccl:apprentice-chapter-11;help.lisp")
                (load "ccl:apprentice-chapter-11;objects.lisp")
                (load "ccl:apprentice-chapter-11;apprentice.lisp")
                (load "ccl:apprentice-chapter-11;IO.lisp")
                (load "ccl:apprentice-chapter-11;QT-Music.lisp")
                ;(load "ccl:apprentice-chapter-11;Sound")
                (load "ccl:apprentice-chapter-11;groupings.lisp")
                (load "ccl:apprentice-chapter-11;midi-file-read.lisp")
                (load "ccl:apprentice-chapter-11;windows.lisp")
                (stop-the-music)(if *process* (process-kill *process*)))

(add-menu-items *apple-menu* 
                (make-instance 'menu-item 
                  :menu-item-title "About Apprentice..." 
                  :menu-item-action 
                  #'(lambda nil (make-instance 'all-about-window)
                                        ))
                (make-instance 'menu-item 
                  :menu-item-title "-"
                  :menu-item-action 
                  #'(lambda nil)))

(window-close hello)
(setq *minimum-stack-overflow-size* 1002400)(setq *tempo* 120)
(setq *tempo* 120)
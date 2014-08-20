
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;     Improvise Function/Chapter 4    ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;     simple code to run Improvise    ;;;;;
                   ;;;;;               loading               ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

(load "ccl:improvise-chapter-4;utilities.lisp")
(load "ccl:improvise-chapter-4;play-midi-QT.lisp")
(load "ccl:improvise-chapter-4;improvise.lisp")
(load "ccl:improvise-chapter-4;midi-file-read.lisp")

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
(setq *warn-if-redefine* nil)
(defvar *input-name* ())

(load "ccl:improvise-chapter-4;windows.lisp")

(defun REMOVE-THEM (stuff other-stuff)
"(remove-all '(1 2 3) '(3 4 2 1  5 6 3))
>> (4 5 6)"
  (loop when (null stuff) return other-stuff
        do (setf other-stuff (remove (first stuff) other-stuff :test #'equal))
        do (setf stuff (rest stuff))))

(defvar my-menu *default-menubar*)
(defvar windows-menubar (first (last *default-menubar*)))
(defvar *improvise-window* ())

(set-menubar (list (first my-menu)
                   (second my-menu)
                   (third my-menu)
                   (fourth my-menu)
                   (fifth my-menu)
                   (make-instance 'menu :menu-title "Improvise"
                                  :menu-items 
                                  (list 
                                   (make-instance 
                                     'menu-item :menu-item-title "Improvise"
                                     :command-key #\1
                                     :menu-item-action 
                                     #'(lambda nil 
                                         (if *improvise-window* (window-close *improvise-window*))
                                         (setq *improvise-window* (make-instance 'improvise-window))))))
                   windows-menubar))

(apply #'remove-menu-items *apple-menu* 
       (list (first (menu-items *apple-menu*))
             (second (menu-items *apple-menu*))))

(add-menu-items *apple-menu* 
                (make-instance 'menu-item 
                  :menu-item-title "About Improvise..." 
                  :menu-item-action 
                  #'(lambda nil (window-ensure-on-screen (make-instance 'about-window))))
                (make-instance 'menu-item 
                  :menu-item-title "-"
                  :menu-item-action 
                  #'(lambda nil)))

(make-instance 'about-window)

(play-events '((0 62 1000 1 127)(1000 59 1000 1 127)(2000 60 1000 1 127)))



                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;     Chorale Function/Chapter 4      ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;           platform dependent        ;;;;;
                   ;;;;;          code to run chorale        ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

(load "ccl:chorale-chapter-4;utilities.lisp")
(load "ccl:chorale-chapter-4;chorale-mcl.lisp")
(load "ccl:chorale-chapter-4;play-midi-QT.lisp")
(defvar *choice-of-composer* 'bach-original)
(setq my-menu *default-menubar*)

(set-menubar (list (first my-menu)
                   (second my-menu)
                   (third my-menu)
                   (fourth my-menu)
                   (fifth my-menu)
                   (make-instance 'menu :menu-title "Chorale"
                                  :menu-items 
                                  (list  
                                   (make-instance 
                                     'menu-item :menu-item-title "Compose"
                                     :command-key #\1
                                     :menu-item-action 
                                     #'(lambda nil 
                                         (setq *choice-of-composer* (choose-one '(bach-original bach-machine-composed)))
                                         (if (equal *choice-of-composer* 'bach-original)
                                           (let* ((choice (choose-one bach-chorales-in-databases)))
                                             (setq *events* (eval choice))
                                             (setq *process* (process-run-function "" #'play-events *events*)))
                                           (progn (compose-bach)
                                                  (setq *process* (process-run-function "" #'play-events *events*))))
                                           (let ((test (y-or-n-dialog "Is this by Bach?" :cancel-text nil)))
                                                              (if 
                                                                (or (and test (equal *choice-of-composer* 'bach-original))
                                                                    (and (null test) (equal *choice-of-composer* 'bach-machine-composed)))
                                                                (message-dialog "Correct")
                                                                (message-dialog "Incorrect")))))
                                   (make-instance 
                                     'menu-item :menu-item-title "Re-Play"
                                     :command-key #\2
                                     :menu-item-action 
                                     #'(lambda nil 
                                         (progn (setq *process* (process-run-function "" #'play-events *events*)))))
                                   (make-instance 
                                     'menu-item :menu-item-title "Stop"
                                     :command-key #\3
                                     :menu-item-action 
                                     #'(lambda nil 
                                         (stop-the-music)(if *process* (process-kill *process*))))))
                   (sixth my-menu)))
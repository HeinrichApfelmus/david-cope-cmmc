


                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;       SPEAC Function/Chapter 7      ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;        load code to run SPEAC       ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;


;;;  Basic environment variables
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

;;;  Loading the files
(load (mac-namestring "ccl:speac-chapter-7;utilities.lisp"))
(load (mac-namestring "ccl:speac-chapter-7;speac.lisp"))
(load (mac-namestring "ccl:speac-chapter-7;variables.lisp"))
(load (mac-namestring "ccl:speac-chapter-7;speac-analysis.lisp"))
(load (mac-namestring "ccl:speac-chapter-7;form-window.lisp"))
(load (mac-namestring "ccl:speac-chapter-7;pattern-match.lisp"))
(load (mac-namestring "ccl:speac-chapter-7;new-form.lisp"))
(load (mac-namestring "ccl:speac-chapter-7;top-level.lisp"))
(load (mac-namestring "ccl:speac-chapter-7;menubar.lisp"))


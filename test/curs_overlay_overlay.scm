(use-modules (test automake-test-lib)
             (ncurses curses)
             (srfi srfi-1))

;; Should be able to overlay one window onto another

(automake-test
 (let* ((mainwin (initscr))
	(win (newwin 5 5 5 5)))
   (clear win)
   (refresh win)
   (clear mainwin)
   (refresh mainwin)
   (addstr win "xxx" #:y 0 #:x 0)
   (refresh win)

   (overlay win mainwin)
   (delwin win)
   (refresh mainwin)

   (let ((x1 (instr mainwin #:y 5 #:x 5 #:n 3)))
     (endwin)
     (newline)
     (format #t "instr: ~s~%" x1)
     (string=? x1 "xxx"))))


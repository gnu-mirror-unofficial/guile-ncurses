(use-modules (test automake-test-lib)
             (ncurses curses)
             (srfi srfi-1))

;; Don't have any real idea how to test intrflush!,
;; so we just check for its existence.

(automake-test
 (let ((win (initscr)))
   (clear win)
   (intrflush! #f)
   (refresh win)
   (endwin)
   (newline)
   'skipped))

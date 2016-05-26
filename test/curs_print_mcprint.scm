(use-modules (test automake-test-lib)
             (ncurses curses)
             (srfi srfi-1))

;; Don't know how to test this, and anyway, even by my standards,
;; having a line printer connected to the terminal is pretty
;; old-school.

(automake-test
 (let* ((mainwin (initscr)))
   
   (clear mainwin)
   (refresh mainwin)
   ;;(mcprint "test")
   (endwin)
   'skipped))

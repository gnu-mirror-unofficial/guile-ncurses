(use-modules (test automake-test-lib)
             (ncurses curses))

(automake-test
 (begin
   (let ((mainwin (initscr)))
     (let* ((orig-val (use-extended-names #t))
	    (should-be-true (use-extended-names #f))
	    (should-be-false (use-extended-names orig-val)))
       (endwin)
       (newline)
       (format #t "use-extended-names (orig): ~S~%" orig-val)
       (format #t "use-extended-names #1: ~S~%" should-be-true)
       (format #t "use-extended-names #2: ~S~%" should-be-false)
       ;; The man page claims that the default value is a compilation-time
       ;; decision but is normally #t

       ;; 20160501 - The return value is not documented. 
       (and
	should-be-true
	(not should-be-false))))))


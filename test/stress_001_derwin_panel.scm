
;;; 2016/05/07
;;; This stress test creates panels derived from derwins derived from windows.
;;; In guile-ncurses 1.6, letting the garbage collector clear the parent window
;;; of the derived window would cause a fault.

;;; Based on a script by John Darrington.

(use-modules (ncurses curses)
	     (ncurses panel))

(define stdscr (initscr))

;; Setup the colors and color pairs
(if (has-colors?)
    (begin
      (start-color!)
      (for-each (lambda (x)
		  (init-pair! x (quotient x 8) (remainder x 8)))
		(iota 64))))
(define last-color-pair 0)

;; This procedure will do a GC 1% of the time
(define (maybe-gc)
  (if (= 0 (random 100))
      (gc)))

;; Generate a window with a box around it and return the inner box
(define (new-boxed-win h w sy sx)
  (let* ((win (newwin h w sy sx))
	 (inner (derwin win (- h 2) (- w 4) 1 2)))

    ;; Set colors of windows
    (if (has-colors?)
	(begin
	  (color-set! win last-color-pair)
	  (bkgd win (color last-color-pair #\space))
	  (color-set! inner last-color-pair)
	  (set! last-color-pair (remainder (+ 1 last-color-pair) 64))))

    ;; Convert windows into panels
    (maybe-gc)
    (make-panel! win)
    (maybe-gc)
    (make-panel! inner)

    ;; Draw a box on the parent outer window
    (box win (acs-vline) (acs-hline))
    (maybe-gc)
    
    inner))

(while #t
       (maybe-gc)
       (let ((w1 (new-boxed-win 3 20 (random 20) (random 20)))
	     (w2 (new-boxed-win 3 20 (random 20)  (+ (random 20) 40))))
	 (maybe-gc)
	 (addstr w1 "Hello")
	 (maybe-gc)
	 (addstr w2 "Goodbye")
	 
	 (update-panels)
	 (doupdate)
	 
	 (usleep 10000)))
(endwin)

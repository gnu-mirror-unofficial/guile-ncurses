;; Copyright 2016 Free Software Foundation, Inc.

;; This file is part of Guile-Ncurses.

;; Guile-Ncurses is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; Guile-Ncurses is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with Guile-Ncurses.  If not, see
;; <http://www.gnu.org/licenses/>.

;;(use-modules (termios))
(use-modules (ncurses extra))

(define *terminal-descriptor* #f)
(define *terminal-original* #f)
(define *terminal-settings* #f)


;; Restore terminal to original settings
(define (terminal-complete)
  (when *terminal-descriptor*
	(tcsetattr! *terminal-descriptor* TCSANOW *terminal-original*)))

;; Signal handler to restore terminal and exit
(define (terminal-signal signum)
  (when *terminal-descriptor*
	(tcsetattr! *terminal-descriptor* TCSANOW *terminal-original*))

  ;; Supposedly _exit is preferred to exit on async signal handler.
  (primitive-_exit (+ 128 signum)))


;; Set up some flag setters, for brevity
(define (flag-set setter getter)
  (lambda x
    (setter *terminal-settings*
	    (logior (getter *terminal-settings*) (apply logior x)))))

(define (flag-clear setter getter)
    (lambda x
      (setter *terminal-settings*
	      (logand (getter *terminal-settings*) (logxor #xFFFFFFFF (apply logior x))))))

(define iflag-set (flag-set termios-iflag-set! termios-iflag))
(define lflag-set (flag-set termios-lflag-set! termios-lflag))
(define cflag-set (flag-set termios-cflag-set! termios-cflag))
(define iflag-clear (flag-clear termios-iflag-set! termios-iflag))
(define lflag-clear (flag-clear termios-lflag-set! termios-lflag))
(define cflag-clear (flag-clear termios-cflag-set! termios-cflag))


;; Initialize the terminal for a non-canonical, non-echo mode
;; that should be compatible with standard I/O.
;; Return #t on success
(define (terminal-init)
  (if *terminal-descriptor*
      #t				;already initialized
      ;; else
      (begin

	;; Figure out which terminal descriptor is conneted to the TTY
	(cond
	 ((isatty? (current-error-port))
	  (set! *terminal-descriptor* (current-error-port)))
	 ((isatty? (current-input-port))
	  (set! *terminal-descriptor* (current-input-port)))
	 ((isatty? (current-output-port))
	  (set! *terminal-descriptor* (current-output-port)))
	 (else
	  (set! *terminal-descriptor* #f)))

	(if (not *terminal-descriptor*)
	    #f
	    ;; else
	    (begin
	      ;; Obtain terminal settings
	      (set! *terminal-original* (tcgetattr *terminal-descriptor*))
	      (set! *terminal-settings* (tcgetattr *terminal-descriptor*))


	      ;; Disable buffering for terminal streams
	      (when (isatty? (current-input-port))
		    (setvbuf (current-input-port) _IONBF 0))
	      (when (isatty? (current-input-port))
		    (setvbuf (current-output-port) _IONBF 0))
	      (when (isatty? (current-error-port))
		    (setvbuf (current-error-port) _IONBF 0))

	      ;; Make sure to restore terminal on a signal action
	      (sigaction SIGHUP terminal-complete)
	      (sigaction SIGQUIT terminal-complete)
	      (sigaction SIGTERM terminal-complete)
	      (sigaction SIGPIPE terminal-complete)
	      (sigaction SIGALRM terminal-complete)

	      ;; Let BREAK cause SIGINT
	      (iflag-clear IGNBRK)
	      (iflag-set BRKINT)

	      ;; Ignore framing and parity errors on input
	      (iflag-set IGNPAR)
	      (iflag-clear PARMRK)

	      ;; Keep 8-bit input
	      (iflag-clear ISTRIP)

	      ;; Do no newline translation on input
	      (iflag-clear INLCR IGNCR ICRNL)

	      ;; Do not to uppercase-to-lowercase mapping on input
	      (iflag-clear IUCLC)

	      ;; Use 8-bit characters
	      (cflag-clear CSIZE)
	      (cflag-set CS8)

	      ;; Enable receipt
	      (cflag-set CREAD)

	      ;; Let INTR QUIT SUSP and DSUSP generates signals
	      (lflag-set ISIG)

	      ;; Enter non-canonical mode to disable line buffering
	      (lflag-clear ICANON)

	      ;; Disable echoing on input
	      (lflag-clear ECHO ECHOE ECHOK ECHONL)

	      ;; Disable implementation-defined input processing
	      (lflag-clear IEXTEN)

	      ;; Set reading to block until at least one byte
	      ;; is available
	      (termios-cc-set! *terminal-settings* VTIME #\x0)
	      (termios-cc-set! *terminal-settings* VMIN #\x1)


	      ;; And now apply the terminal settings
	      (tcsetattr! *terminal-descriptor* TCSANOW *terminal-settings*)
	      #t)))))

(terminal-init)

(display "ORIGINAL SETTINGS: ")
(write *terminal-original*)
(newline)
(%termios-debug *terminal-original*)
(newline)


(display "NEW SETTINGS: ")
(write *terminal-settings*)
(newline)
(%termios-debug *terminal-settings*)
(newline)

(while #t
       (write (read-char))
       (newline))


;; -*- Mode: scheme; -*-

;; extra.scm

;; Copyright 2010, 2011, 2016, 2019 Free Software Foundation, Inc.

;; This file is part of GNU Guile-Ncurses.

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

(define-module (ncurses extra)
  #:use-module (ncurses curses)
  #:use-module (srfi srfi-1)
  #:export (
            %has-termios
            %termios-debug

            termios?
            new-termios

            TCIFLUSH
            TCIOFF
            TCIOFLUSH
            TCION
            TCOFLUSH
            TCOOFF
            TCOON
            TCSADRAIN
            TCSAFLUSH
            TCSANOW

            tcdrain
            tcflow
            tcflush
            tcgetattr
            tcsetattr!

            termios-flag-set!
            termios-flag-clear!
            termios-flag-get
            termios-flag-test
            termios-makeraw!

            termios-csize-get
            termios-csize-set!

            termios-ispeed-set!
            termios-ispeed-get
            termios-ospeed-set!
            termios-ospeed-get

            termios-veof-get
            termios-veol-get
            termios-veof-get
            termios-veol-get
            termios-verase-get
            termios-vintr-get
            termios-vkill-get
            termios-vquit-get
            termios-vstart-get
            termios-vstop-get
            termios-vsusp-get
            termios-vtime-get
            termios-vmin-get

            termios-veof-set!
            termios-veol-set!
            termios-verase-set!
            termios-vintr-set!
            termios-vkill-set!
            termios-vquit-set!
            termios-vstart-set!
            termios-vstop-set!
            termios-vsusp-set!
            termios-vtime-set!
            termios-vmin-set!

            ptsmakeraw
            O_RDRW
            O_NOCTTY
            ))

(define (iflag-symbol->const x)
  (cond
   ((eqv? x 'IGNBRK) IGNBRK)
   ((eqv? x 'BRKINT) BRKINT)
   ((eqv? x 'IGNPAR) IGNPAR)
   ((eqv? x 'PARMRK) PARMRK)
   ((eqv? x 'INPCK) INPCK)
   ((eqv? x 'INLCR) INLCR)
   ((eqv? x 'IGNCR) IGNCR)
   ((eqv? x 'ICRNL) ICRNL)
   ((eqv? x 'IXON) IXON)
   ((eqv? x 'IXOFF) IXOFF)
   ((eqv? x 'IXANY) IXANY)
   (else #f)))

(define (iflag-const->symbol-list c)
  (apply append
   (map
    (lambda (f)
      (if (logtest (iflag-symbol->const f) c)
          (list f)
          (list)))
    `(IGNBRK BRKINT IGNPAR PARMRK INPCK INLCR IGNCR ICRNL IXON IXOFF IXANY))))

(define (oflag-symbol->const x)
  (cond
   ((eqv? x 'ONLCR) ONLCR)
   ((eqv? x 'OCRNL) OCRNL)
   ((eqv? x 'ONOCR) ONOCR)
   ((eqv? x 'ONLRET) ONLRET)
   ((eqv? x 'OPOST) OPOST)
   (else #f)))

(define (oflag-const->symbol-list c)
  (apply append
   (map
    (lambda (f)
      (if (logtest (oflag-symbol->const f) c)
          (list f)
          (list)))
    `(ONLCR OCRNL ONOCR ONLRET OPOST))))

(define (cflag-symbol->const x)
  (cond
   ((eqv? x 'CSTOPB) CSTOPB)
   ((eqv? x 'CREAD) CREAD)
   ((eqv? x 'PARENB) PARENB)
   ((eqv? x 'PARODD) PARODD)
   ((eqv? x 'HUPCL) HUPCL)
   ((eqv? x 'CLOCAL) CLOCAL)
   (else #f)))

(define (cflag-const->symbol-list c)
  (apply append
   (map
    (lambda (f)
      (if (logtest (cflag-symbol->const f) c)
          (list f)
          (list)))
    `(CSTOPB CREAD PARENB PARODD HUPCL CLOCAL))))

(define (lflag-symbol->const x)
  (cond
   ((eqv? x 'ISIG) ISIG)
   ((eqv? x 'ICANON) ICANON)
   ((eqv? x 'ECHO) ECHO)
   ((eqv? x 'ECHOE) ECHOE)
   ((eqv? x 'ECHOK) ECHOK)
   ((eqv? x 'ECHONL) ECHONL)
   ((eqv? x 'NOFLSH) NOFLSH)
   ((eqv? x 'TOSTOP) TOSTOP)
   (else #f)))

(define (lflag-const->symbol-list c)
  (apply append
   (map
    (lambda (f)
      (if (logtest (lflag-symbol->const f) c)
          (list f)
          (list)))
    `(ISIG ICANON ECHO ECHOE ECHOK ECHONL NOFLSH TOSTOP))))

(define (set-flag term flag)
  (let ((iconst (iflag-symbol->const flag))
        (oconst (oflag-symbol->const flag))
        (cconst (cflag-symbol->const flag))
        (lconst (lflag-symbol->const flag)))
    (cond
     (iconst
      (termios-iflag-set! term (logior iconst (termios-iflag term)))
      #t)
     (oconst
      (termios-oflag-set! term (logior oconst (termios-oflag term)))
      #t)
     (cconst
      (termios-cflag-set! term (logior cconst (termios-cflag term)))
      #t)
     (lconst
      (termios-lflag-set! term (logior lconst (termios-lflag term)))
      #t)
     (else #f))))

(define (clear-flag term flag)
  (let ((iconst (iflag-symbol->const flag))
        (oconst (oflag-symbol->const flag))
        (cconst (cflag-symbol->const flag))
        (lconst (lflag-symbol->const flag)))
    (cond
     (iconst
      (termios-iflag-set! term (logand (lognot iconst) (termios-iflag term)))
      #t)
     (oconst
      (termios-oflag-set! term (logand (lognot oconst) (termios-oflag term)))
      #t)
     (cconst
      (termios-cflag-set! term (logand (lognot cconst) (termios-cflag term)))
      #t)
     (lconst
      (termios-lflag-set! term (logand (lognot lconst) (termios-lflag term)))
      #t)
     (else #f))))

(define (get-flags term)
  (append (iflag-const->symbol-list (termios-iflag term))
          (oflag-const->symbol-list (termios-oflag term))
          (cflag-const->symbol-list (termios-cflag term))
          (lflag-const->symbol-list (termios-lflag term))))

(define (termios-flag-set! term flag-or-flags)
  (let ((flags (if (list? flag-or-flags)
                   flag-or-flags
                   (list flag-or-flags))))
    (for-each
     (lambda (x)
       (set-flag term x))
     flags)))

(define (termios-flag-clear! term flag-or-flags)
  (let ((flags (if (list? flag-or-flags)
                   flag-or-flags
                   (list flag-or-flags))))
    (for-each
     (lambda (x)
       (clear-flag term x))
     flags)))

(define (termios-flag-get term)
  (get-flags term))

(define (termios-flag-test term flag)
  (if (member flag (get-flags term))
      #t
      #f))

(define (termios-csize-get term)
  (let ((c (logand CSIZE (termios-cflag term))))
    (cond
     ((= c CS5) 5)
     ((= c CS6) 6)
     ((= c CS7) 7)
     ((= c CS8) 8)
     (else #f))))

(define (termios-csize-set! term siz)
  (let ((c (logand CSIZE (termios-cflag term)))
        (rest (lognot (logand (lognot CSIZE) (termios-cflag term)))))
    (cond
     ((= siz 5) (termios-cflag-set! term (logior rest CS5)))
     ((= siz 6) (termios-cflag-set! term (logior rest CS6)))
     ((= siz 7) (termios-cflag-set! term (logior rest CS7)))
     ((= siz 8) (termios-cflag-set! term (logior rest CS8))))))

(define (termios-makeraw! term)
  (termios-csize-set! term 8)
  (termios-flag-clear! term '(IGNBRK BRKINT PARMRK ISTROP INLCR IGNCR ICRNL IXON))
  (termios-flag-clear! tgerm '(OPOST ECHO ECHONL ICANON ISIG IEXTEN CSIZE PARENB)))

(define (termios-veof-get term) (termios-cc term VEOF))
(define (termios-veol-get term) (termios-cc term VEOL))
(define (termios-verase-get term) (termios-cc term VERASE))
(define (termios-vintr-get term) (termios-cc term VINTR))
(define (termios-vkill-get term) (termios-cc term VKILL))
(define (termios-vquit-get term) (termios-cc term VQUIT))
(define (termios-vstart-get term) (termios-cc term VSTART))
(define (termios-vstop-get term) (termios-cc term VSTOP))
(define (termios-vsusp-get term) (termios-cc term VSUSP))
(define (termios-vtime-get term)
  "Returns the maximum time between input bytes."
  (* 0.1 (char->integer (termios-cc term VTIME))))
(define (termios-vmin-get term)
  "Returns the minimum number of bytes required."
  (char->integer (termios-cc term VMIN)))

(define (termios-veof-set! term c) (termios-cc-set! term VEOF c))
(define (termios-veol-set! term c) (termios-cc-set! term VEOL c))
(define (termios-verase-set! term c) (termios-cc-set! term VERASE c))
(define (termios-vintr-set! term c) (termios-cc-set! term VINTR c))
(define (termios-vkill-set! term c) (termios-cc-set! term VKILL c))
(define (termios-vquit-set! term c) (termios-cc-set! term VQUIT c))
(define (termios-vstart-set! term c) (termios-cc-set! term VSTART c))
(define (termios-vstop-set! term c) (termios-cc-set! term VSTOP c))
(define (termios-vsusp-set! term c) (termios-cc-set! term VSUSP c))
(define (termios-vtime-set! term t)
  "Set the maximum allowed time between bytes in seconds.  The valid
range for T is 0 to 25.5, with a useful resolution of 0.1 seconds."
  (termios-cc-set! term VTIME
                   (integer->char
		    (min 0 (max 255
				(inexact->exact
				 (round
				  (* t 10.0))))))))
(define (termios-vmin-set! term n)
  "Sets the minimum number of bytes to be accepted as valid input."
  (termios-cc-set! term VMIN (integer->char n)))



;; Return the number of character cells that C takes
(define (wcwidth x)
  (cond
   ((char? x)
    (%strwidth (string x)))
   ((and (integer? x) (logtest x A_ALTCHARSET))
    1)
   ((xchar? x)
    (%strwidth (xchar-chars x)))
   ((string? x)
    (%strwidth x))
   ((and (list? x) (every xchar? x))
    (%strwidth (apply string (apply append (map xchar-chars x)))))
   (else
    (error "Invalid input ~s" x))))

(define (speed->bps x)
  "Given a speed enumeration, such as B9600, returns a speed in
bits-per-second. It returns #f is no match is found."
  (cond
   ((eqv? x #f)                         ; Catch when B?? is #f
    #f)
   ((= x B0)
    0)
   ((= x B110)
    110)
   ((= x B134)
    134)
   ((= x B1200)
    1200)
   ((= x B150)
    150)
   ((= x B1800)
    1800)
   ((= x B19200)
    19200)
   ((= x B200)
    200)
   ((= x B2400)
    2400)
   ((= x B300)
    300)
   ((= x B38400)
    38400)
   ((= x B4800)
    4800)
   ((= x B50)
    50)
   ((= x B600)
    600)
   ((= x B75)
    75)
   ((= x B9600)
    9600)

   ((eqv? x B115200)
    115200)
   ((eqv? x B14400)
    14400)
   ((eqv? x B230400)
    230400)
   ((eqv? x B28800)
    28800)
   ((eqv? x B57600)
    57600)
   ((eqv? x B7200)
    7200)
   ((eqv? x B76800)
    76800)

   (else #f)))

(define (bps->speed x)
  "Given a integer bits-per-second value, it returns a speed
enumeration value, such as B9600. If no exact match is found,
it returns the next slower valid value."
  (cond
   ((< x 50)
    B0)
   ((< x 75)
    B50)
   ((< x 110)
    B75)
   ((< x 134)
    B110)
   ((< x 150)
    B134)
   ((< x 200)
    B150)
   ((< x 300)
    B200)
   ((< x 600)
    B300)
   ((< x 1200)
    B600)
   ((< x 1800)
    B1200)
   ((< x 2400)
    B1800)
   ((< x 4800)
    B2400)
   ((< x 9600)
    B4800)
   ((< x 14400)
    B9600)
   ((< x 19200)
    (or B14400 B9600))
   ((< x 28800)
    B19200)
   ((< x 38400)
    (or B28800 B19200))
   ((< x 57600)
    B38400)
   ((< x 76800)
    (or B57600 B38400))
   ((< x 115200)
    (or B76800 B57600 B38400))
   ((< x 230400)
    (or B115200 B76800 B57600 B38400))
   (else
    (or B230400 B115200 B76800 B57600 B38400))))

(define (termios-ispeed-get term)
  (speed->bps (cfgetispeed term)))
(define (termios-ospeed-get term)
  (speed->bps (cfgetospeed term)))

(define (termios-ispeed-set! term bps)
  (cfsetispeed! term (bps->speed bps)))
(define (termios-ospeed-set! term bps)
  (cfsetospeed! term (bps->speed bps)))

(define (%termios-debug t)
  (let ((cflag (termios-cflag t))
        (iflag (termios-iflag t))
        (oflag (termios-oflag t))
        (lflag (termios-lflag t))
        (veol (termios-cc t VEOL))
        (veof (termios-cc t VEOF))
        (verase (termios-cc t VERASE))
        (vintr (termios-cc t VINTR)))
    (format #t "c_iflag = ~a~%" iflag)
    (format #t "c_oflag = ~a~%" oflag)
    (format #t "c_cflag = ~a~%" cflag)
    (format #t "l_cflag = ~a~%" cflag)
    (format #t "c_ispeed = ~a~%" (speed->bps (cfgetispeed t)))
    (format #t "c_ospeed = ~a~%" (speed->bps (cfgetospeed t)))
    (format #t "VEOF = ~s~%" veof)
    (if (logtest lflag ICANON)
        (format #t "VEOL = ~s~%" veol))
    (if (logtest lflag ICANON)
        (format #t "VERASE = ~s~%" verase))
    (if (logtest lflag ISIG)
        (format #t "VINTR = ~s~%" vintr))

    (display "Input Modes:\n")
    (if (logtest iflag IGNBRK)
        (display "IGNBRK: Break on input ignored\n"))
    (if (and (not (logtest iflag IGNBRK))
             (logtest iflag BRKINT))
        ;; (if (= (getpgrp) (tcgetpgrp t))
        ;;     (display "BRKINT: Flush input/output queues and generate SIGINT\n")
        ;;     (display "BRKINT: Flush input/output queuest\n")))
        (display "BRKINT: Flush input/output queues\n"))
    (if (and (not (logtest iflag IGNBRK))
             (not (logtest iflag BRKINT)))
        (if (logtest iflag PARMRK)
            (display "PARMRK: Break is read as 0xff 0x00 0x00\n")
            (display "------: Break is read as 0x00\n")))
    (if (logtest iflag IGNPAR)
        (display "IGNPAR: A byte with a parity error shall be ignored\n"))
    (if (and (logtest iflag PARMRK)
             (not (logtest iflag IGNPAR)))
        (begin
          (display "PARMRK: Prefix bytes with parity errors with 0xff 0x00\n")
          (if (logtest iflag ISTRIP)
              (display "ISTRIP: Don't prefix 0xff bytes with 0xff\n")
              (display "PARMRK: Prefix 0xff bytes with 0xff\n"))))
    (if (and (not (logtest iflag PARMRK))
             (not (logtest iflag IGNPAR)))
        (display "------: Bytes with parity errors become 0x00\n"))
    (if (logtest iflag INPCK)
        (display "INPCK: Input parity checking is enabled\n")
        (display "------: Input parity checking is disabled\n"))
    (if (logtest iflag ISTRIP)
        (display "ISTRIP: Valid input bytes are stripped to 7-bits\n")
        (display "------: All 8-bits of valid input bytes are processed\n"))
    (if (logtest iflag INLCR)
        (display "INLCR: Newline bytes translated to Carriage Return bytes\n"))
    (if (logtest iflag IGNCR)
        (display "IGNCR: Carriage Return bytes are ignored\n"))
    (if (and (not (logtest iflag IGNCR))
             (logtest iflag ICRNL))
        (display "ICRNL: Carriage Return bytes translated to Newline bytes\n"))
    (if (logtest iflag IXANY)
        (display "IXANY: Any input character restarts suspended output\n"))
    (if (logtest iflag IXON)
        (display "IXON: Start/stop output control enabled\n")
        (display "------: Start/stop output control disabled\n"))
    (if (logtest iflag IXOFF)
        (display "IXOFF: Start/stop input control enabled\n")
        (display "------: Start/stop input control disabled\n"))

    (display "Output Modes:\n")
    (if (not (logtest oflag OPOST))
        (display "------: Output data shall be transmitted unchanged\n")
        (begin
          (display "OPOST: output data shall be post-processed\n")
          (if (logtest oflag ONLCR)
              (display "ONLCR: Newline is transmitted as CR/NL\n"))
          (if (logtest oflag OCRNL)
              (display "OCRNL: Carrige Return is trasmitted as Newline\n"))
          (if (logtest oflag ONOCR)
              (display "ONOCR: No Carriage Return trasmitted when in column 0\n"))
          (if (logtest oflag ONLRET)
              (display "ONLRET: Newline also does Carriage Return\n"))
          (if (not (logior oflag (logand ONLCR OCRNL ONOCR ONLRET)))
              (begin
                (display "------: Newline does line-feed but does not Carriage Return.\n")
                (display "------: Carriage Return returns to column 0 but does not line feed\n")))))

    (display "Control Modes:\n")
    (if (= (logand cflag CSIZE) CS5)
        (display "CS5: five-bit character size mask\n"))
    (if (= (logand cflag CSIZE) CS6)
        (display "CS6: six-bit character size mask\n"))
    (if (= (logand cflag CSIZE) CS7)
        (display "CS7: seven-bit character size mask\n"))
    (if (= (logand cflag CSIZE) CS8)
        (display "CS8: eight-bit character size mask\n"))
    (if (logtest cflag CREAD)
        (display "CREAD: receiver is enabled\n"))
    (if (logtest cflag PARENB)
        (display "PAREND: parity generation and checking is on\n"))
    (if (logtest cflag PARODD)
        (display "PARODD: odd parity is used\n"))
    (if (logtest cflag HUPCL)
        (display "HUPCL: modem control lines will be lowered after hangup\n"))
    (if (logtest cflag CLOCAL)
        (display "CLOCAL: modem control lines will be ignored\n"))

    (display "Local Modes:\n")
    (if (logtest lflag ISIG)
        (display "ISIG: generate signals when INTR, QUIT, SUSP or DSUSP is received\n"))
    (if (logtest lflag ICANON)
        (display "ICANON: canonical mode is enabled\n"))
    (if (logtest lflag ECHO)
        (display "ECHO: input characters are echoed\n"))
    (if (and (logtest lflag ECHOE) (logtest lflag ECHO))
        (display "ECHOE: the ERASE and ERASEW characters erase the preceeding character or word\n"))
    (if (and (logtest lflag ECHOK) (logtest lflag ECHO))
        (display "ECHOK: the KILL character erases the current line\n"))
    (if (logtest lflag NOFLSH)
        (display "NOFLSH: flushing input and output queues on receipt of INT, QUIT and SUSP is disabled\n"))
    (if (logtest lflag TOSTOP)
        (display "TOSTOP: SIGTTOU is sent to the background process process group on attempted writes to its controlling terminal\n"))
    ))


(load-extension "libguile-ncurses" "gucu_extra_init")

;; These function may not exist depending of the
;; capabilities of the underlying system
(if (defined? 'tcgetsid)        (export tcgetsid))
(if (defined? 'unlockpt)        (export unlockpt))
(if (defined? 'ptsname)         (export ptsname))
(if (defined? 'openpt)          (export openpt))
(if (defined? 'grantpt)         (export grantpt))
(if (defined? '%strwidth)       (export wcwidth))

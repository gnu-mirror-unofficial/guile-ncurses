;; -*- Mode: scheme; -*-

;; extra.scm

;; Copyright 2010, 2011, 2016 Free Software Foundation, Inc.

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

            BS0
            BS1
            BSDLY
            CR0
            CR1
            CR2
            CR3
            CRDLY
            FF0
            FF1
            FFDLY
            NL0
            NL1
            NLDLY
            OCRNL
            OFILL
            ONLCR
            ONLRET
            ONOCR
            OPOST
            TAB1
            TAB2
            TAB3
            TABDLY
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
            VT0
            VT1
            VTDLY

            VEOF
            VEOL
            VERASE
            VINTR
            VKILL
            VLNEXT
            VMIN
            VQUIT
            VREPRINT
            VSTART
            VSTOP
            VSUSP

            BRKINT
            ICRNL
            IGNBRK
            IGNCR
            IGNPAR
            INLCR
            INPCK
            ISTRIP
            IXANY
            IXOFF
            IXON
            PARMRK

            CLOCAL
            CREAD
            CS5
            CS6
            CS7
            CS8
            CSIZE
            CSTOPB
            HUPCL
            PARENB
            PARODD
            VTIME

            ECHO
            ECHOE
            ECHOK
            ECHONL
            FLUSHO
            ICANON
            IEXTEN
            ISIG
            NOFLSH
            TOSTOP

            B0
            B110
            B1200
            B134
            B150
            B1800
            B19200
            B200
            B2400
            B300
            B38400
            B4800
            B50
            B600
            B75
            B9600

            B115200
            B14400
            B230400
            B28800
            B57600
            B7200
            B76800

            speed->bps
            bps->speed

            termios?
            new-termios

            cfgetispeed
            cfgetospeed
            cfmakeraw!
            cfsetispeed!
            cfsetospeed!
            ptsmakeraw
            tcdrain
            tcflow
            tcflush
            tcgetattr
            tcsendbreak
            tcsetattr!

            termios-iflag
            termios-oflag
            termios-cflag
            termios-lflag
            termios-line
            termios-cc
            termios-iflag-set!
            termios-oflag-set!
            termios-cflag-set!
            termios-lflag-set!
            termios-cc-set!

            %termios-debug
	    string-split-at-line-endings

            ))

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
enumeration value, such as B9600. It returns #f if not match
is found."
  (cond
   ((= x 0)
    B0)
   ((= x 110)
    B110)
   ((= x 134)
    B134)
   ((= x 1200)
    B1200)
   ((= x 150)
    B150)
   ((= x 1800)
    B1800)
   ((= x 19200)
    B19200)
   ((= x 200)
    B200)
   ((= x 2400)
    B2400)
   ((= x 300)
    B300)
   ((= x 38400)
    B38400)
   ((= x 4800)
    B4800)
   ((= x 50)
    B50)
   ((= x 600)
    B600)
   ((= x 75)
    B75)
   ((= x B9600)
    9600)

   ((eqv? x 115200)
    B115200)
   ((eqv? x 14400)
    B14400)
   ((eqv? x 230400)
    B230400)
   ((eqv? x 28800)
    B28800)
   ((eqv? x 57600)
    B57600)
   ((eqv? x 7200)
    B7200)
   ((eqv? x 76800)
    B76800)

   (else #f)))


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
    (if (logtest lflag IEXTEN)
        (display "IEXTEN: implementation-defined input processing is enabled\n"))

    ))


(load-extension "libguile-ncurses" "gucu_extra_init")

;; These function may not exist depending of the
;; capabilities of the underlying system
(if (defined? 'cfsetspeed!)     (export cfsetspeed!))
(if (defined? 'tcgetsid)        (export tcgetsid))
(if (defined? 'unlockpt)        (export unlockpt))
(if (defined? 'ptsname)         (export ptsname))
(if (defined? 'grantpt)         (export grantpt))
(if (defined? '%strwidth)       (export wcwidth))

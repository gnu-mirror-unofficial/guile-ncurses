;; -*- Mode: scheme; indent-tabs-mode: nil -*-

;; curses.scm

;; Copyright 2009, 2010, 2011, 2013, 2014, 2016 Free Software Foundation, Inc.

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

(define-module (ncurses curses)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (

            %filter
            %is-pad-broken
            %is-subwin-broken
            %scheme-char-from-c-char
            %scheme-char-from-c-wchar
            %scheme-char-to-c-char
            %scheme-char-to-c-wchar
            %ucs4-chars
            %wide-ncurses
            %wmove
            %xchar-from-chtype
            %xchar-to-chtype
            ALL_MOUSE_EVENTS
            A_ALTCHARSET
            A_ATTRIBUTES
            A_BLINK
            A_BOLD
            A_CHARTEXT
            A_COLOR
            A_DIM
            A_HORIZONTAL
            A_INVIS
            A_LEFT
            A_LOW
            A_NORMAL
            A_PROTECT
            A_REVERSE
            A_RIGHT
            A_STANDOUT
            A_TOP
            A_UNDERLINE
            A_VERTICAL
            NCURSES_MOUSE_VERSION
            BUTTON1_CLICKED
            BUTTON1_DOUBLE_CLICKED
            BUTTON1_PRESSED
            BUTTON1_RELEASED
            BUTTON1_TRIPLE_CLICKED
            BUTTON2_CLICKED
            BUTTON2_DOUBLE_CLICKED
            BUTTON2_PRESSED
            BUTTON2_RELEASED
            BUTTON2_TRIPLE_CLICKED
            BUTTON3_CLICKED
            BUTTON3_DOUBLE_CLICKED
            BUTTON3_PRESSED
            BUTTON3_RELEASED
            BUTTON3_TRIPLE_CLICKED
            BUTTON4_CLICKED
            BUTTON4_DOUBLE_CLICKED
            BUTTON4_PRESSED
            BUTTON4_RELEASED
            BUTTON4_TRIPLE_CLICKED
            ; BUTTON5_CLICKED
            ; BUTTON5_DOUBLE_CLICKED
            ; BUTTON5_PRESSED
            ; BUTTON5_RELEASED
            ; BUTTON5_TRIPLE_CLICKED
            BUTTON_ALT
            BUTTON_CTRL
            BUTTON_SHIFT
            COLOR_BLACK
            COLOR_BLUE
            COLOR_CYAN
            COLOR_GREEN
            COLOR_MAGENTA
            COLOR_RED
            COLOR_WHITE
            COLOR_YELLOW
            EOF
            ERR
            FALSE
            KEY_A1
            KEY_A3
            KEY_B2
            KEY_BACKSPACE
            KEY_BEG
            KEY_BREAK
            KEY_BTAB
            KEY_C1
            KEY_C3
            KEY_CANCEL
            KEY_CATAB
            KEY_CLEAR
            KEY_CLOSE
            KEY_CODE_YES
            KEY_COMMAND
            KEY_COPY
            KEY_CREATE
            KEY_CTAB
            KEY_DC
            KEY_DL
            KEY_DOWN
            KEY_EIC
            KEY_END
            KEY_ENTER
            KEY_EOL
            KEY_EOS
            KEY_EXIT
            KEY_F0
            KEY_FIND
            KEY_HELP
            KEY_HOME
            KEY_IC
            KEY_IL
            KEY_LEFT
            KEY_LL
            KEY_MARK
            KEY_MESSAGE
            KEY_MIN
            KEY_MOUSE
            KEY_MOVE
            KEY_NEXT
            KEY_NPAGE
            KEY_OPEN
            KEY_OPTIONS
            KEY_PPAGE
            KEY_PREVIOUS
            KEY_PRINT
            KEY_REDO
            KEY_REFERENCE
            KEY_REFRESH
            KEY_REPLACE
            KEY_RESET
            KEY_RESIZE
            KEY_RESTART
            KEY_RESUME
            KEY_RIGHT
            KEY_SAVE
            KEY_SBEG
            KEY_SCANCEL
            KEY_SCOMMAND
            KEY_SCOPY
            KEY_SCREATE
            KEY_SDC
            KEY_SDL
            KEY_SELECT
            KEY_SEND
            KEY_SEOL
            KEY_SEXIT
            KEY_SF
            KEY_SFIND
            KEY_SHELP
            KEY_SHOME
            KEY_SIC
            KEY_SLEFT
            KEY_SMESSAGE
            KEY_SMOVE
            KEY_SNEXT
            KEY_SOPTIONS
            KEY_SPREVIOUS
            KEY_SPRINT
            KEY_SR
            KEY_SREDO
            KEY_SREPLACE
            KEY_SRESET
            KEY_SRIGHT
            KEY_SRSUME
            KEY_SSAVE
            KEY_SSUSPEND
            KEY_STAB
            KEY_SUNDO
            KEY_SUSPEND
            KEY_UNDO
            KEY_UP
            OK
            REPORT_MOUSE_POSITION
            TRUE
            acs-block
            acs-board
            acs-btee
            acs-bullet
            acs-ckboard
            acs-darrow
            acs-degree
            acs-diamond
            acs-gequal
            acs-hline
            acs-lantern
            acs-larrow
            acs-lequal
            acs-llcorner
            acs-lrcorner
            acs-ltee
            acs-nequal
            acs-pi
            acs-plminus
            acs-plus
            acs-rarrow
            acs-rtee
            acs-s1
            acs-s3
            acs-s7
            acs-s9
            acs-sterling
            acs-ttee
            acs-uarrow
            acs-ulcorner
            acs-urcorner
            acs-vline
            addch
            addchstr
            addstr
            assume-default-colors
            attr->list
            attr-get
            attr-off!
            attr-on!
            attr-set!
            baudrate
            beep
            bkgd
            bkgdset!
            border
            box
            can-change-color?
            cbreak!
            chgat
            clear
            clearok!
            clrtobot
            clrtoeol
            color-content
            color-pair
            color-pairs
            color-set!
            colors
            cols
            copywin
            curs-set
            curscr
            curses-version
            def-prog-mode
            def-shell-mode
            define-key
            delay-output
            delch
            deleteln
            delscreen
            delwin
            derwin
            doupdate
            dupwin
            echo!
            echochar
            endwin
            erase
            erasechar
            flash
            flushinp
            getbegx
            getbegy
            getbegyx
            getbkgd
            getch
            getcurx
            getcury
            getmaxx
            getmaxy
            getmaxyx
            getmouse
            getnstr
            getparent
            getparx
            getpary
            getparyx
            getscrreg
            getsyx
            getwin
            getyx
            halfdelay!
            has-colors?
            has-ic?
            has-il?
            has-key?
            hline
            idcok!
            idlok!
            immedok!
            inch
            inchstr
            init-color!
            init-pair!
            initscr
            insch
            insdelln
            insertln
            insstr
            instr
            intrflush!
            is-cleared?
            is-idcok?
            is-idlok?
            is-immedok?
            is-keypad?
            is-leaveok?
            is-linetouched?
            is-nodelay?
            is-notimeout?
            is-pad?
            is-scrollok?
            is-subwin?
            is-syncok?
            is-wintouched?
            isendwin?
            key-f
            keyname
            keypad!
            killchar
            leaveok!
            lines
            longname
            mcprint
            meta!
            mevent?
            mouse-trafo
            mouseinterval
            mousemask
            move
            mvcur
            mvderwin
            mvwin
            napms
            newpad
            newwin
            nl!
            nocbreak!
            nodelay!
            noecho!
            nonl!
            nooutrefresh
            noqiflush!
            noraw!
            notimeout!
            noutrefresh
            open-curses-port
            overlay
            overwrite
            pair-content
            pair-number
            pechochar
            pnoutrefresh
            prefresh
            putwin
            qiflush!
            raw!
            redrawln
            redrawwin
            refresh
            reset-prog-mode
            reset-shell-mode
            resetty
            resize
            resizeterm
            savetty
            scr-dump
            scr-init
            scr-restore
            scr-set
            screen?
            screen=?
            scrl
            scroll
            scrollok!
            set-tabsize!
            set-term
            setscrreg!
            setsyx
            setupterm
            standend!
            standout!
            start-color!
            stdscr
            subpad
            subwin
            syncok!
            tabsize
            term-attrs
            termname
            tiget
            timeout!
            touchline
            touchwin
            typeahead!
            unctrl
            ungetch
            ungetmouse
            untouchline
            untouchwin
            use-default-colors
            use-env
            use-extended-names
            vline
            cursyncup
            wenclose?
            window?
            window=?
            syncdown
            syncup

            ;; error codes
            &curses-error
            &curses-wrong-type-arg-error
            &curses-out-of-range-error
            &curses-bad-state-error
            &curses-missing-function-error
            curses-error?
            curses-wrong-type-arg-error?
            curses-out-of-range-error?
            curses-bad-state-error?
            curses-missing-function-error?

            ;; xchar type library
            xchar-attr
            xchar-color
            xchar-equal?

            ;; xchar property modifiers
            blink
            blink-off
            blink-on
            bold
            bold-off
            bold-on
            color
            dim
            dim-off
            dim-on
            horizontal
            horizontal-off
            horizontal-on
            invis
            invis-off
            invis-on
            left
            left-off
            left-on
            low
            low-off
            low-on
            normal
            normal-off
            normal-on
            protect
            protect-off
            protect-on
            inverse
            inverse-off
            inverse-on
            right
            right-off
            right-on
            set-xchar-attr!
            set-xchar-color!
            set-xchar-chars!
            standout
            standout-off
            standout-on
            top
            top-off
            top-on
            underline
            underline-on
            underline-off
            vertical
            vertical-off
            vertical-on
            xchar?
            xchar-attr
            xchar-color
            xchar-chars

            ;; Forward declaration of panel library functions
            panel?
            panel=?
            make-panel!
            del-panel!
            ))

(if (string=? "1.8" (string-take (version) 3))
    (use-syntax (ice-9 syncase)))

;;; Exceptions

(define-condition-type &curses-error &error
  curses-error?)

(define-condition-type &curses-wrong-type-arg-error &curses-error
  curses-wrong-type-arg-error?
  (arg           curses-wrong-type-arg-error:arg)
  (expected-type curses-wrong-type-arg-error:expected-type))

(define-condition-type &curses-out-of-range-error &curses-error
  curses-out-of-range-error?
  (arg           curses-wrong-type-arg-error:arg))

;; Usually this indicates and attempt to use an already freed object
(define-condition-type &curses-bad-state-error &curses-error
  curses-bad-state-error?)

;; Indicates that a function isn't available because
(define-condition-type &curses-missing-function-error &curses-error
  curses-missing-function-error?
  (function           curses-missing-function-error:arg))

;;; The xchar type library

;; The xchar type -- a Guile version of the NCurses cchar_t
(define (color-name n)
  "Returns a string describing the color number N."
  (cond
   ((= n COLOR_BLACK) "black")
   ((= n COLOR_RED) "red")
   ((= n COLOR_GREEN) "green")
   ((= n COLOR_YELLOW) "yellow")
   ((= n COLOR_BLUE) "blue")
   ((= n COLOR_MAGENTA) "magenta")
   ((= n COLOR_CYAN) "cyan")
   ((= n COLOR_WHITE) "white")
   (else
    (let* ((cc (color-content n))
           (r (list-ref cc 0))
           (g (list-ref cc 1))
           (b (list-ref cc 2)))
      (format #t "R~a G~a B~a" r g b)))))

(define (attr-name attr)
  "Given an attribute stored as a integer, return a descriptive
string."
  (string-trim
   (string-append
    (if (logtest attr A_ALTCHARSET) " altcharset" "")
    (if (logtest attr A_BLINK) " blink" "")
    (if (logtest attr A_BOLD) " bold" "")
    (if (logtest attr A_DIM) " dim" "")
    (if (logtest attr A_INVIS) " invis" "")
    (if (logtest attr A_PROTECT) " protect" "")
    (if (logtest attr A_REVERSE) " reverse" "")
    (if (logtest attr A_STANDOUT) " standout" "")
    (if (logtest attr A_UNDERLINE) " underline" "")
    (if (logtest attr A_HORIZONTAL) " horizontal" "")
    (if (logtest attr A_LEFT) " left" "")
    (if (logtest attr A_LOW) " low" "")
    (if (logtest attr A_RIGHT) " right" "")
    (if (logtest attr A_TOP) " top" "")
    (if (logtest attr A_VERTICAL) " vertical" ""))))

(define (acs-char-name c)
  "If C is a member of the alternate character set, this returns the
name of the character as as string."
  (let ((n (normal c)))
    (cond
     ((xchar-equal? n (acs-ulcorner)) "ULCORNER")
     ((xchar-equal? n (acs-llcorner))  "LLCORNER")
     ((xchar-equal? n (acs-urcorner)) "URCORNER")
     ((xchar-equal? n (acs-lrcorner)) "LRCORNER")
     ((xchar-equal? n (acs-ltee)) "LTEE")
     ((xchar-equal? n (acs-rtee)) "RTEE")
     ((xchar-equal? n (acs-btee)) "BTEE")
     ((xchar-equal? n (acs-ttee)) "TTEE")
     ((xchar-equal? n (acs-hline)) "HLINE")
     ((xchar-equal? n (acs-vline)) "VLINE")
     ((xchar-equal? n (acs-plus)) "PLUS")
     ((xchar-equal? n (acs-s1)) "S1")
     ((xchar-equal? n (acs-s9))  "S9")
     ((xchar-equal? n (acs-diamond))  "DIAMOND")
     ((xchar-equal? n (acs-ckboard))  "CKBOARD")
     ((xchar-equal? n (acs-degree))  "DEGREE")
     ((xchar-equal? n (acs-plminus))  "PLMINUS")
     ((xchar-equal? n (acs-bullet))  "BULLET")
     ((xchar-equal? n (acs-larrow))  "LARROW")
     ((xchar-equal? n (acs-rarrow))  "RARROW")
     ((xchar-equal? n (acs-darrow))  "DARROW")
     ((xchar-equal? n (acs-uarrow))  "UARROW")
     ((xchar-equal? n (acs-board))  "BOARD")
     ((xchar-equal? n (acs-s3))  "S3")
     ((xchar-equal? n (acs-s7))  "S7")
     ((xchar-equal? n (acs-lequal))  "LEQUAL")
     ((xchar-equal? n (acs-gequal))  "GEQUAL")
     ((xchar-equal? n (acs-pi))  "PI")
     ((xchar-equal? n (acs-nequal))  "NEQUAL")
     ((xchar-equal? n (acs-lantern))  "LANTERN")
     ((xchar-equal? n (acs-sterling))  "STERLING")
     (else
      "UNKNOWN SURROGATE"))))


(define (print-xchar x port)
  "A printer for complex chars."
  (let ((attrs (attr-name (xchar-attr x)))
        (color (xchar-color x))
        (chars (xchar-chars x)))
    (format port "#<xchar")
    (if (not (string-null? attrs))
        (format port " ~a" attrs))
    (if (false-if-exception (pair-content color))
        (let* ((pair (pair-content color))
               (fore (car pair))
               (back (cadr pair)))
          (format port " color-pair #~a" color)
          (format port " [~a on ~a]" (color-name fore) (color-name back))))
    (if (logtest (xchar-attr x) A_ALTCHARSET)
        (format port " ~s" (acs-char-name x))
        (map (lambda (c)
               (format port " ~s" c))
             chars))
    (format port ">")))

(define rtd-xchar (make-record-type "complex-char" '(attr color chars) print-xchar))
(define make-xchar (record-constructor rtd-xchar))
(define xchar? (record-predicate rtd-xchar))
(define xchar-attr (record-accessor rtd-xchar 'attr))
(define xchar-color (record-accessor rtd-xchar 'color))
(define xchar-chars (record-accessor rtd-xchar 'chars))
(define %set-xchar-attr! (record-modifier rtd-xchar 'attr))
(define %set-xchar-color! (record-modifier rtd-xchar 'color))
(define %set-xchar-chars! (record-modifier rtd-xchar 'chars))

(define (xchar->list x)
  "Converts a complex charater X to a list of properties"
  (append (list (xchar-attr x))
          (list (xchar-color x))
          (xchar-chars x)))

(define (list->xchar x)
  "Converts a list of properties into a complex character"
  (make-xchar (first x)
              (second x)
              (drop x 2)))


;; Guile 1.8.x's equal? is sufficient to compare two xchars, but,
;; Guile 1.6.x's equal? always returns false when comparing two xchars
(define (xchar-equal? a b)
  "A comparison predicate for complex characters"
  (and
   (equal? (xchar-attr a) (xchar-attr b))
   (equal? (xchar-color a) (xchar-color b))
   (equal? (xchar-chars a) (xchar-chars b))))


;;; Rendition functions

(defmacro a-attribute (x A_ATTRIBUTE)
  `(cond
    ((char? ,x)
     (make-xchar ,A_ATTRIBUTE
                 0
                 (list ,x)))
    ((and (integer? ,x) (logtest ,x A_ALTCHARSET))
     (make-xchar (logior ,A_ATTRIBUTE A_ALTCHARSET)
                 0
                 (list (integer->char (logand x #xff)))))
    ((xchar? ,x)
     (make-xchar (if (logtest (xchar-attr ,x) A_ALTCHARSET)
                     (logior ,A_ATTRIBUTE A_ALTCHARSET)
                     ,A_ATTRIBUTE)
                 (xchar-color ,x)
                 (xchar-chars ,x)))
    ((string? ,x)
     (map (lambda (c)
            (make-xchar ,A_ATTRIBUTE 0 (list c)))
          (string->list ,x)))
    ((and (list? ,x) (every xchar? ,x))
     (map (lambda (c)
            (make-xchar (if (logtest (xchar-attr c) A_ALTCHARSET)
                            (logior ,A_ATTRIBUTE A_ALTCHARSET)
                            ,A_ATTRIBUTE)
                        (xchar-color c)
                        (xchar-chars c)))
          ,x))
    (else
     (error "Invalid input ~s" ,x))))

(defmacro a-attribute-on (x A_ATTRIBUTE)
  `(cond
    ((char? ,x)
     (make-xchar ,A_ATTRIBUTE 0 (list ,x)))
    ((and (integer? ,x) (logtest ,x A_ALTCHARSET))
     (make-xchar (logior ,A_ATTRIBUTE A_ALTCHARSET)
                 0
                 (list (integer->char (logand x #xff)))))
    ((xchar? ,x)
     (make-xchar (logior ,A_ATTRIBUTE (xchar-attr x))
                 (xchar-color ,x)
                 (xchar-chars ,x)))
    ((string? ,x)
     (map (lambda (c)
            (make-xchar ,A_ATTRIBUTE 0 (list c)))
          (string->list ,x)))
    ((and (list? ,x) (every xchar? ,x))
     (map (lambda (c)
            (make-xchar (logior ,A_ATTRIBUTE (xchar-attr c))
                        (xchar-color c)
                        (xchar-chars c)))
          ,x))
    (else
     (error "Invalid input ~s" ,x))))

(defmacro a-attribute-off (x A_ATTRIBUTE)
  `(cond
    ((char? ,x)
     (make-xchar A_NORMAL 0 (list ,x)))
    ((and (integer? ,x) (logtest ,x A_ALTCHARSET))
     (make-xchar A_ALTCHARSET
                 0
                 (list (integer->char (logand x #xff)))))
    ((xchar? ,x)
     (make-xchar (logand (lognot ,A_ATTRIBUTE) (xchar-attr x))
                 (xchar-color ,x)
                 (xchar-chars ,x)))
    ((string? ,x)
     (map (lambda (c)
            (make-xchar A_NORMAL 0 (list c)))
          (string->list ,x)))
    ((and (list? ,x) (every xchar? ,x))
     (map (lambda (c)
            (make-xchar (logand (lognot ,A_ATTRIBUTE) (xchar-attr c))
                        (xchar-color c)
                        (xchar-chars c)))
          ,x))
    (else
     (error "Invalid input ~s" ,x))))

(define (blink x)
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute BLINK.  If the input
X was a rendered character or a rendered string, the old attributes
are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_BLINK))

(define (blink-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the BLINK attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_BLINK))

(define (blink-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the BLINK attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_BLINK))

(define (bold x)
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute BOLD.  If the input
X was a rendered character or a rendered string, the old attributes
are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_BOLD))

(define (bold-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the BOLD attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_BOLD))

(define (bold-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the BOLD attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_BOLD))

(define (dim x)
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute DIM.  If the input
X was a rendered character or a rendered string, the old attributes
are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_DIM))

(define (dim-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the DIM attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_DIM))

(define (dim-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the DIM attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_DIM))

(define (horizontal x)
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute HORIZONTAL.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_HORIZONTAL))

(define (horizontal-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the HORIZONTAL attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_HORIZONTAL))

(define (horizontal-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the HORIZONTAL attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_HORIZONTAL))

(define (invis x)
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute INVIS.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_INVIS))

(define (invis-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the INVIS attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_INVIS))

(define (invis-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the INVIS attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_INVIS))

(define (left x)
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute LEFT.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_LEFT))

(define (left-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the LEFT attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_LEFT))

(define (left-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the LEFT attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_LEFT))

(define (low x)
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute LOW.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_LOW))

(define (low-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the LOW attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_LOW))

(define (low-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the LEFT attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_LOW))

(define (normal x)
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the normal, default, rendering.
If the input X was a rendered character or a rendered string, the old
attributes are cleared, but the color pair, if any, is not modified."
  (a-attribute x A_NORMAL))

(define (normal-off x) (a-attribute-off x A_NORMAL))
(define (normal-on x) (a-attribute-on x A_NORMAL))

(define (protect x)
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute PROTECT.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_PROTECT))

(define (protect-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the PROTECT attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_PROTECT))

(define (protect-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the PROTECT attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_PROTECT))

(define (inverse x)
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the INVERSE (aka REVERSE)
attribute.  If the input X was a rendered character or a rendered
string, the old attributes are replaced, but the color pair, if any,
is not modified."
  (a-attribute x A_REVERSE))

(define (inverse-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the INVERSE (aka REVERSE) attribute disabled,
but, other attributes are not modified."
  (a-attribute-off x A_REVERSE))

(define (inverse-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the INVERSE (aka REVERSE) attribute enabled,
but, other attributes are not modified."
  (a-attribute-on x A_REVERSE))

(define (right x)
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute RIGHT.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_RIGHT))

(define (right-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the RIGHT attribute disabled,
but, other attributes are not modified."
  (a-attribute-off x A_RIGHT))

(define (right-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the RIGHT attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_RIGHT))

(define (standout x)
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute STANDOUT.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_STANDOUT))

(define (standout-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the STANDOUT attribute disabled,
but, other attributes are not modified."
  (a-attribute-off x A_STANDOUT))

(define (standout-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the STANDOUT attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_STANDOUT))

(define (top x)
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute TOP.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_TOP))

(define (top-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the TOP attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_TOP))

(define (top-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the TOP attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_TOP))

(define (underline x)
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute UNDERLINE.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_UNDERLINE))

(define (underline-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the UNDERLINE attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_UNDERLINE))

(define (underline-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the UNDERLINE attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_UNDERLINE))

(define (vertical x)
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute VERTICAL.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_VERTICAL))

(define (vertical-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the VERTICAL attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_VERTICAL))

(define (vertical-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the VERTICAL attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_VERTICAL))

(define (color n x)
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the color pair N.  If the input X
was a rendered character or a rendered string, the old attributes are
not modified, but the color pair, if any, is modified."
  (cond
   ((char? x)
    (make-xchar A_NORMAL
                n
                (list x)))
   ((and (integer? x) (logtest x A_ALTCHARSET))
    (make-xchar A_ALTCHARSET
                n
                (list (integer->char (logand x #xff)))))
   ((xchar? x)
    (make-xchar (xchar-attr x)
                n
                (xchar-chars x)))
   ((string? x)
    (map (lambda (c)
           (make-xchar A_NORMAL n (list c)))
         (string->list x)))
   ((and (list? x) (every xchar? x))
    (map (lambda (c)
           (make-xchar (xchar-attr c)
                       n
                       (xchar-chars c)))
         x))
   (else
    (error "Invalid input ~s" x))))


(define (acs-block)    (list->xchar (%acs-block)))
(define (acs-board)    (list->xchar (%acs-board)))
(define (acs-btee)     (list->xchar (%acs-btee)))
(define (acs-bullet)   (list->xchar (%acs-bullet)))
(define (acs-ckboard)  (list->xchar (%acs-ckboard)))
(define (acs-darrow)   (list->xchar (%acs-darrow)))
(define (acs-degree)   (list->xchar (%acs-degree)))
(define (acs-diamond)  (list->xchar (%acs-diamond)))
(define (acs-gequal)   (list->xchar (%acs-gequal)))
(define (acs-hline)    (list->xchar (%acs-hline)))
(define (acs-lantern)  (list->xchar (%acs-lantern)))
(define (acs-larrow)   (list->xchar (%acs-larrow)))
(define (acs-lequal)   (list->xchar (%acs-lequal)))
(define (acs-llcorner) (list->xchar (%acs-llcorner)))
(define (acs-lrcorner) (list->xchar (%acs-lrcorner)))
(define (acs-ltee)     (list->xchar (%acs-ltee)))
(define (acs-nequal)   (list->xchar (%acs-nequal)))
(define (acs-pi)       (list->xchar (%acs-pi)))
(define (acs-plminus)  (list->xchar (%acs-plminus)))
(define (acs-plus)     (list->xchar (%acs-plus)))
(define (acs-rarrow)   (list->xchar (%acs-rarrow)))
(define (acs-rtee)     (list->xchar (%acs-rtee)))
(define (acs-s1)       (list->xchar (%acs-s1)))
(define (acs-s3)       (list->xchar (%acs-s3)))
(define (acs-s7)       (list->xchar (%acs-s7)))
(define (acs-s9)       (list->xchar (%acs-s9)))
(define (acs-sterling) (list->xchar (%acs-sterling)))
(define (acs-ttee)     (list->xchar (%acs-ttee)))
(define (acs-uarrow)   (list->xchar (%acs-uarrow)))
(define (acs-ulcorner) (list->xchar (%acs-ulcorner)))
(define (acs-urcorner) (list->xchar (%acs-urcorner)))
(define (acs-vline)    (list->xchar (%acs-vline)))

;; Typecheck macros
(define-syntax typecheck
  (syntax-rules ()
    ((_ val type-name type-test)
     (if (not (type-test val))
         (raise (condition (&curses-wrong-type-arg-error
                            (arg val)
                            (expected-type type-name))))))
    ((_ val type-name type-test1 type-test2)
     (if (not (and (type-test1 val) (type-test2 val)))
         (raise (condition (&curses-wrong-type-arg-error
                            (arg val)
                            (expected-type type-name))))))))

(define-syntax assert-boolean
  (syntax-rules ()
    ((_ val)
     (typecheck val 'boolean boolean?))))
(define-syntax assert-integer
  (syntax-rules ()
    ((_ val)
     (typecheck val 'integer integer? exact?))))
(define-syntax assert-list-of-chars
  (syntax-rules ()
    ((_ loc)
     (if (or (not (list? loc)) (not (every char? loc)))
         (raise (condition (&curses-wrong-type-arg-error
                            (arg str)
                            (expected-type 'list))))))))
(define-syntax assert-pad
  (syntax-rules ()
    ((_ val)
     (typecheck val 'pad window? is-pad?))))
(define-syntax assert-pos-if-defined
  (syntax-rules ()
    ((_ x y)
     (if (and x y)
         (begin
           (assert-integer x)
           (assert-integer y))))))
(define-syntax assert-string
  (syntax-rules ()
    ((_ val)
     (typecheck val 'string string?))))
(define-syntax assert-window
  (syntax-rules ()
    ((_ val)
     (typecheck val 'window window?))))
(define-syntax assert-xchar
  (syntax-rules ()
    ((_ val)
     (typecheck val 'xchar xchar?))))
(define-syntax assert-xstring
  (syntax-rules ()
    ((_ str)
     (if (or (not (list? str)) (not (every xchar? str)))
         (raise (condition (&curses-wrong-type-arg-error
                            (arg str)
                            (expected-type 'xstring))))))))

(define (set-xchar-attr! ch attr)
  "Directly set the attributes for a complex character"
  (assert-xchar ch)
  (assert-integer attr)
  (%set-xchar-attr! ch attr))

(define (set-xchar-color! ch color-pair-number)
  "Directly set the color pair number for a complex character"
  (assert-xchar ch)
  (assert-integer color-pair-number)
  (%set-xchar-color! ch color-pair-number))

(define (set-xchar-chars! ch list-of-chars)
  "Directly set the character(s) for a complex character"
  (assert-xchar ch)
  (assert-list-of-chars list-of-chars)
  (%set-xchar-chars! ch list-of-chars))

;; Scheme calling wrappers for C functions

(define* (addch win ch #:key y x)
  "Puts the character CH into the given window at its current window
position.  If Y and X are set, moves to (X,Y) first.  Returns #t on
success."
  (assert-window win)
  (assert-xchar ch)
  (assert-pos-if-defined x y)
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (%waddch win (xchar->list ch))))

(define* (addchstr win str #:key y x (n -1))
  "Adds the list of complex characters STR to the window WIN at and
after the current cursor position.  If X and Y are set, the cursor
will be moved to that position first.  If N is set, and maximum of N
complex characters will be added.  Returns #t on success or #f on
failure."
  (assert-window win)
  (assert-xstring str)
  (assert-pos-if-defined x y)
  (assert-integer n)
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (%waddchnstr win (map xchar->list str) n)))

(define* (addstr win str #:key y x (n -1))
  "Adds the string STR to the window WIN at and after the current
cursor position.  If X and Y are set, the cursor will be moved to that
position first.  If N is set, and maximum of N characters will be
added.  Returns #t on success or #f on failure."
  (assert-window win)
  (assert-string str)
  (assert-pos-if-defined x y)
  (assert-integer n)
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (%waddnstr win str n)))

(define (attr-get win)
  "Returns the rendtion of the current window as a two-element list.
The first element is the integer representation of the attributes, and
the second element is the color pair number."
  (assert-window win)
  (let* ((ac (%wattr-get win))
         (attr (car ac))
         (color (cadr ac)))
    (list
     (logand attr (logior A_STANDOUT
                          A_UNDERLINE
                          A_REVERSE
                          A_BLINK
                          A_DIM
                          A_BOLD
                          A_ALTCHARSET
                          A_INVIS
                          A_PROTECT
                          A_HORIZONTAL
                          A_LEFT
                          A_LOW
                          A_RIGHT
                          A_TOP
                          A_VERTICAL))
     color)))

(define (attr->list attr)
  "Unpacks the integer representation of an attribute into a list of
attributes."
  (append
   (if (logtest attr A_ALTCHARSET) '(altcharset) '())
   (if (logtest attr A_BLINK) '(blink) '())
   (if (logtest attr A_BOLD) '(bold) '())
   (if (logtest attr A_DIM) '(dim) '())
   (if (logtest attr A_INVIS) '(invis) '())
   (if (logtest attr A_PROTECT) '(protect) '())
   (if (logtest attr A_REVERSE) '(inverse) '())
   (if (logtest attr A_STANDOUT) '(standout) '())
   (if (logtest attr A_UNDERLINE) '(underline) '())
   (if (logtest attr A_HORIZONTAL) '(horizontal) '())
   (if (logtest attr A_LEFT) '(left) '())
   (if (logtest attr A_LOW) '(low) '())
   (if (logtest attr A_RIGHT) '(right) '())
   (if (logtest attr A_TOP)  '(top) '())
   (if (logtest attr A_VERTICAL) '(vertical) '())))

(define (attr-off! win attrs)
  "Turns off the attributes ATTRS of the given window without turning
any other attributes on or off."
  (assert-window win)
  (assert-integer attrs)
  (let ((ret (%attr-off! win attrs)))
    (if (not ret)
        (raise (condition (&curses-bad-state-error))))))

(define (attr-on! win attrs)
  "Turns on the attributes ATTRS of the given window without turning
any other attributes on or off."
  (assert-window win)
  (assert-integer attrs)
  (let ((ret (%attr-on! win attrs)))
    (if (not ret)
        (raise (condition (&curses-bad-state-error))))))

(define* (attr-set! win attr #:optional color)
  "Sets the given window to have the attributes ATTRS and optionally
the color pair given by COLOR."
  (assert-window win)
  (assert-integer attr)
  (if color
      (begin
        (%wattr-set! win attr color))
      (%wattr-set! win attr (pair-number attr))))

(define (baudrate)
  "Returns the baudrate of this terminal, or #f on failure."
  (%baudrate))

(define (beep)
  "Sounds an audible alarm on the terminal.  Returns #t on success or
#f on failure."
  (%beep))

(define (bkgd win ch)
  "Sets the background of the named window WIN to the given complex
char CH and applies it to all characters in the window."
  (assert-window win)
  (assert-xchar ch)
  (%bkgd win (xchar->list ch)))

(define (bkgdset! win ch)
  "Sets the background for the window to the given complex character
CH.  This will be combined with all new characters added to the
window."
  (assert-window win)
  (assert-xchar ch)
  (%bkgdset! win (xchar->list ch)))

(define (border win left right top bottom topleft topright bottomleft
                bottomright)
  "Draws a border on a given window using the given complex characters
TOP, LEFT, RIGHT, etc.  If the number 0 is used instead of a complex
character, then the default character will be used for that border
element."
  (assert-window win)
  (for-each
   (lambda (ch)
     (if (and (not (xchar? ch)) (not (eq? ch 0)))
         (raise (condition (&curses-wrong-type-arg-error
                            (arg ch)
                            (expected-type 'xchar))))))
   (list left right top bottom topleft topright bottomleft bottomright))

  (let ((l (if (equal? left 0)
               (xchar->list (normal (acs-vline)))
               (xchar->list left)))
        (r (if (equal? right 0)
               (xchar->list (normal (acs-vline)))
               (xchar->list right)))
        (t (if (equal? top 0)
               (xchar->list (normal (acs-hline)))
               (xchar->list top)))
        (b (if (equal? bottom 0)
               (xchar->list (normal (acs-hline)))
               (xchar->list bottom)))
        (tl (if (equal? topleft 0)
                (xchar->list (normal (acs-ulcorner)))
                (xchar->list topleft)))
        (tr (if (equal? topright 0)
                (xchar->list (normal (acs-urcorner)))
                (xchar->list topright)))
        (bl (if (equal? bottomleft 0)
                (xchar->list (normal (acs-llcorner)))
                (xchar->list bottomleft)))
        (br (if (equal? bottomright 0)
                (xchar->list (normal (acs-lrcorner)))
                (xchar->list bottomright))))
    (%border win l r t b tl tr bl br)))

(define (box win v h)
  "Draws a box on the given window using the complex character V for
the verical lines and the complex character H for the horizontal
lines.  If the number 0 is used for H or V instead of a complex
character, the default lines will be used."
  (assert-window win)
  (let ((v2 (if (equal? v 0) (xchar->list (normal (acs-vline))) (xchar->list v)))
        (h2 (if (equal? h 0) (xchar->list (normal (acs-hline))) (xchar->list h))))
    (%border win v2 v2 h2 h2
             (xchar->list (normal-on (acs-ulcorner))) (xchar->list (normal-on (acs-urcorner)))
             (xchar->list (normal-on (acs-llcorner))) (xchar->list (normal-on (acs-lrcorner))))))

(define (can-change-color?)
  "Returns #t if the terminal can change the RGB of a color number, or
#f if the terminal has preassigned, unmodifiable colors."
  (%can-change-color?))

(define (cbreak!)
  "Disables line buffering and erase/kill character processing."
  (%cbreak!))

(define* (chgat win n attr color #:key y x)
  "Changes that attributes and color pair of of a given number of
characters starting at the current cursor location in the window WIN.
If X and Y are defined, first move to that position."
  (assert-window win)
  (assert-integer n)
  (assert-integer attr)
  (assert-integer color)
  (assert-pos-if-defined x y)
  (let ((ret (and (if (and y x)
                      (%wmove win y x)
                      #t)
                  (%wchgat win n attr color))))
    (if (not ret)
        (raise (condition (&curses-bad-state-error))))))

(define (clear win)
  "Copy blanks to every position in the window, and set it to
be cleared completely and repainted at the next window refresh."
  (assert-window win)
  (let ((ret (%clear win)))
    (or ret
        (raise (condition (&curses-bad-state-error))))))

(define (clearok! win bf)
  "If clearok is called with BF as #t, the next call to 'refresh' with
this window will clear the screen completely and redraw the screen from
scratch."
  (assert-window win)
  (assert-boolean bf)
  (%clearok! win bf))

(define (clrtobot win)
  "Erases from the cursor location to the end of screen."
  (assert-window win)
  (let ((ret (%clrtobot win)))
    (or ret
        (raise (condition (&curses-bad-state-error))))))

(define (clrtoeol win)
  "Erases from the cursor location to the end of the current line."
  (assert-window win)
  (%clrtoeol win))

(define (color-content c)
  "Given a color number, this procedure returns a three-element list
containing the red, green, and blue values of the color on a 0 to 1000
scale.  It can return #f if the color is out of range or colors aren't
initialized."
  (assert-integer c)
  (%color-content c))

(define (color-set! win pair)
  "Sets the window's color pair to the color pair number PAIR."
  (assert-window win)
  (assert-integer pair)
  (let ((ret (%color-set! win pair)))
    (if (not ret)
        (raise (condition (&curses-out-of-range-error
                           (arg win)))))))

(define (curs-set vis)
  "Sets the visiblity of the cursor.  If VIS is 0, it is invisible.
1 is visible.  2 is very visible.  Returns the previous setting, or #f
on error."
  (assert-integer vis)
  (if (or (< vis 0) (> vis 2))
      (raise (condition (&curses-out-of-range-error
                         (arg vis)))))
  (%curs-set vis))

(define (cursyncup win)
  "Synchronizes the location of the cursor between WIN and all parents
of WIN."
  (assert-window win)
  (%wcursyncup win))

(define (copywin srcwin destwin sminrow smincol dminrow dmincol
                 dmaxrow dmaxcol overlay)
  "This routine copies text from SRCWIN to DESTWIN.  A rectangle is
specified in the destination window DMINROW DMINCOL to DMAXROW
DMAXCOL, and the upper-left-corner of the source window is specified
SMINROW and SMINCOL.  If OVERLAY is #t, only text and not blanks are
copied.  If overlay is #f, text and blanks are copied.  Returns #t on
success. or #f on failure.  Failure usually indicates a problem with
your coordinates."
  (assert-window srcwin)
  (assert-window destwin)
  (for-each (lambda (x) (assert-integer x))
            (list sminrow smincol dminrow dmincol dmaxrow dmaxcol))
  (%copywin srcwin destwin sminrow smincol dminrow dmincol dmaxrow dmaxcol
            overlay))

(define (curses-version)
  "Returns, as a string, the version number and patch level of the
underlying ncurses library."
  (%curses-version))

(define (def-prog-mode)
  "Cache the current terminal mode."
  (%def-prog-mode))

(define (def-shell-mode)
  "Cache the current terminal mode."
  (%def-shell-mode))

(define* (delch win #:key y x)
  "Deletes the character under the cursor in the given window,
optionally first moving to the location X, Y."
  (assert-window win)
  (assert-pos-if-defined x y)
  (and
   (if (and y x)
       (%wmove win y x)
       #t)
   (or (%wdelch win)
       (raise (condition (&curses-bad-state-error))))))

(define* (deleteln win #:key y x)
  "Deletes the line under the cursor in the given window, optionally first
moving to the position X, Y"
  (assert-window win)
  (assert-pos-if-defined x y)
  (and
   (if (and y x)
       (%wmove win y x)
       #t)
   (or (%winsdelln win -1)
       (raise (condition (&curses-bad-state-error))))))

(define (delscreen scr)
  "Frees a screen created by 'newterm'.  Can't be called before 'endwin'."
  (if (not (screen? scr))
      (raise (condition (&curses-wrong-type-arg-error
                         (arg scr)
                         (expected-type 'screen)))))
  (or (%delscreen scr)
      (raise (condition (&curses-bad-state-error)))))

(define (delwin win)
  "Forcibly frees a window.  Note that the garbage collector will normally
free a window when it is no longer being used, so calling this explicitly is
usually not necessary."
  (assert-window win)
  (%delwin win))

(define* (derwin origwin nlines ncols begin_y begin_x #:key (panel #f))
  "Creates and returns a new window with the given number of lines and
columns.  The window is at position BEGIN_Y, BEGIN_X on the original
window ORIGWIN.  The window is made in the middle of the window
ORIGWIN, so that changes made to one window will affect both windows.
The subwindow shares memory with the original window."
  (assert-window origwin)
  (for-each (lambda (x) (assert-integer x))
            (list nlines ncols begin_y begin_x))
  (let ((win (%derwin origwin nlines ncols begin_y begin_x)))
    (if (and win panel)
        (make-panel! win))
    win))

(define (doupdate)
  "Copies the virtual screen to the physical screen."
  (%doupdate))

(define (dupwin win)
  "Create a duplicate of a window."
  (assert-window win)
  (let ((ret (%dupwin win)))
    (if (not ret)
        (raise (condition (&curses-bad-state-error)))
        ret)))

(define (echo!)
  "Enable echoing of typed characters."
  (%echo!))

(define* (echochar win ch #:key y x)
  "Puts the character CH into the given window at its current window
position and then refreshes the window.  If Y and X are set, moves
to (X,Y) first.  Returns #t on success."
  (assert-window win)
  (assert-xchar ch)
  (assert-pos-if-defined x y)
  (and
   (if (and y x)
       (%wmove win y x)
       #t)
   (%wechochar win (xchar->list ch))))

(define (endwin)
  "Exit or escape from curses mode."
  (%endwin))

(define (erase win)
  "Copy blanks to every position in the window."
  (assert-window win)
  (%erase win))

(define (erasechar)
  "Returns the character that is used to erase the screen."
  (%erasechar))

(define (flash)
  "Flashes the screen.  Returns #t on success or #f on failure."
  (%flash))

(define (getbkgd win)
  "Returns the default background of the given window as a complex
char."
  (assert-window win)
  (let ((ret (%getbkgd win)))
    (or ret
        (raise (condition (&curses-bad-state-error))))))

(define (getbegx win)
  "Returns the beginning x coordinate of the specified window."
  (assert-window win)
  (cadr (getbegyx win)))

(define (getbegy win)
  "Returns the beginning y coordinate of the specified window."
  (assert-window win)
  (car (getbegyx win)))

(define (getbegyx win)
  "Returns as a two-element list (y x) the beginning coordinates of
the specified window."
  (assert-window win)
  (%getbegyx win))

(define* (getch win #:key y x)
  "Read a character from the keyboard for the given window.  If
no-delay mode, if no input is waiting, #f is returned.  If the
keypress maps to a letter, a character is returned.  If the keypress
maps to a control character, and keypad! has been called, then an
integer curses constant, like KEY_ENTER.  The character may appear on
the screen based on the setting of noecho!.  Optionally, first move to
location X, Y, if given."
  (assert-window win)
  (assert-pos-if-defined x y)
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (%wgetch win)))

(define (getcurx win)
  "Returns the current cursor X location."
  (assert-window win)
  (cadr (getyx win)))

(define (getcury win)
  "Returns the current cursor Y location."
  (assert-window win)
  (car (getyx win)))

(define (getdelay win)
  "Returns the wait time for a character in milliseconds."
  (assert-window win)
  (%getdelay win))

(define (getmaxx win)
  "Returns the window's size in the X direction ."
  (assert-window win)
  (cadr (getmaxyx win)))

(define (getmaxy win)
  "Returns the window's size in the Y direction."
  (assert-window win)
  (car (getmaxyx win)))

(define (getmaxyx win)
  "Returns as a two-element list (y x) the size of the specified
window."
  (assert-window win)
  (%getmaxyx win))

(define (getmaxy win)
  (assert-window win)
  (car (getmaxyx win)))

(define (getmouse)
  "This returns either a list of mouse information or @code{#f}.
The list is of the form (id x y z button_state)."
  (%getmouse))

(define* (getnstr win n #:key y x)
  "Receives a series of keypresses, up to N, from the given window
until a newline or carriage return is received.  The terminating
character is not included in the return string.  If a SIGWINCH (window
resize) interrupts the function, it instead returns the KEY_RESIZE
integer code. Optionally, it will move the cursort to the X, Y,
position before receiving characters."
  (assert-window win)
  (assert-integer n)
  (assert-pos-if-defined x y)
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (%wgetnstr win n)))

(define (getparent win)
  "Returns the parent window of WIN, if any.  If WIN is a root window,
it returns #f."
  (assert-window win)
  (%getparent win))

(define (getparx win)
  "If WIN is a subwindow, return the x coordinate of the subwindow
relative to the parent, or -1 if it is not a subwindow."
  (assert-window win)
  (cadr (getparyx win)))

(define (getpary win)
  "If WIN is a subwindow, return the x coordinate of the subwindow
relative to the parent, or -1 if it is not a subwindow."
  (assert-window win)
  (car (getparyx win)))

(define (getparyx win)
  "If WIN is a subwindow, this returns as a two-element list (y x) the
beginning coordinates of the subwindow relative to the parent window.
If this is not a subwindow, (-1 -1) is returned."
  (assert-window win)
  (%getparyx win))

(define (getscrreg win)
  "Returns, as a two element list, the top and bottom line numbers of the
scroll region for the window."
  (assert-window win)
  (%getscrreg win))

(define (getsyx)
  "Returns the current coordinates of the virtual screen cursor as a
two-element list (y x)."
  (%getsyx))

(define (getyx win)
  "Returns the cursor position of the given window as a two-element list."
  (assert-window win)
  (%getyx win))

(define (halfdelay! tenths)
  "Disable line buffering and erase/kill character processing, but,
only wait TENTHS tenths of seconds for a keypress"
  (assert-integer tenths)
  (if (or (< tenths 1) (> tenths 255))
      (raise (condition (&curses-out-of-range-error
                         (arg tenths)))))
  (%halfdelay! tenths))

(define (has-colors?)
  "Returns #t if the current terminal has color capability."
  (%has-colors?))

(define (has-ic?)
  "Returns #t if this terminal has a hardware capability to insert
and delete characters."
  (%has-ic?))

(define (has-il?)
  "Returns #t if this terminal has a hardware capability to insert
and delete lines."
  (%has-il?))

(define (has-key? key)
  "Given a curses integer key constant like KEY_ENTER, returns #t if the
current terminal type recognizes a key of that value."
  (assert-integer key)
  (%has-key? key))

(define (has-mouse?)
  "Return #t if the mouse driver has been successfully initialized."
  (%has-mouse?))

(define* (hline win ch n #:key y x)
  "Draws a horizontal line of length N using the complex character CH.
If X and Y are given, the cursor is first moved to that location."
  (assert-window win)
  (assert-xchar ch)
  (assert-integer n)
  (assert-pos-if-defined x y)
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (or
        (%whline win (xchar->list ch) n)
        (raise (condition (&curses-bad-state-error))))))

(define* (inch win #:key y x)
  "Returns a complex character containg the character at the current
position in the given window, optionally first moving to Y, X."
  (assert-window win)
  (assert-pos-if-defined x y)
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (or
        (list->xchar (%winch win))
        (raise (condition (&curses-bad-state-error))))))

(define* (inchstr win #:key y x (n -1))
  "Returns a the list of complex characters that are in the window
starting at the cursor location and ending at the right margin of the
window.  If N is given it returns a maximum of N characters.  If Y and
X are given, it first moves the cursor to that location."
  (assert-window win)
  (assert-pos-if-defined x y)
  (assert-integer n)
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (let ((ret (%winchnstr win n)))
         (if (not ret)
             (raise (conditions (&curses-bad-state-error)))
             (map list->xchar ret)))))

(define (idcok! win bf)
  "If idcok! is called with BF as #t, curses may use a terminal's hardware
insert/delete character feature if it has such a feature."
  (assert-window win)
  (assert-boolean bf)
  (%idcok! win bf))

(define (idlok! win bf)
  "If idlok! is called with BF as #t, curses may use a terminal's hardware
insert/delete line feature if it has such a feature."
  (assert-window win)
  (assert-boolean bf)
  (%idlok! win bf))

(define (immedok! win bf)
  "If immedok! is called with BF as #t, any chahge in the window image, such as the ones called by addch, automatically causes a call to 'refresh'."
  (assert-window win)
  (assert-boolean bf)
  (%immedok! win bf))

(define (init-color! color r g b)
  "Initializes the color number COLOR to have the red-green-blue value
R G B.  R G and B are integers between 0 and 1000.  Returns #t on
success or #f on failure."
  (for-each (lambda (x) (assert-integer x))
            (list color r g b))
  (map (lambda (x)
         (if (or (< color 0) (> color 1000))
             (raise (condition (&curses-out-of-range-error
                                (arg x))))))
       (list r g b))
  (%init-color color r g b))

(define (init-pair! pair fore back)
  "Initializes the color pair PAIR to have color number FORE as its
foreground color and color number BACK as its background color.  Returns
#t on success or #f on failure."
  (assert-integer pair)
  (assert-integer fore)
  (assert-integer back)
  (%init-pair! pair fore back))

(define (initscr)
  "Initialize curses and return the window object that is the base window."
  (or (false-if-exception (stdscr))
      (let ((ret (%initscr)))
        (if (not ret)
            (raise (condition (&curses-bad-state-error)))
            ret))))

(define* (insch win ch #:key y x)
  "Insert the complex character CH before the character under the cursor
in the given window.  Optionally move to X, Y before doing the insertion."
  (assert-window win)
  (assert-xchar ch)
  (assert-pos-if-defined x y)
  (if (and y x)
      (%wmove win y x))
  (let ((ret (%winsch win (xchar->list ch))))
    (if (not ret)
        (raise (condition (&curses-bad-state-error))))))

(define* (instr win #:key y x (n -1))
  "Returns, as a simple string, the characters starting at the current
position in the given window.  If N is given, the string will contain
no more than N characters.  If Y and X are given, move to X, Y first."
  (assert-window win)
  (assert-pos-if-defined x y)
  (assert-integer n)
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (%winnstr win n)))

(define* (insdelln win n #:key y x)
  "For positive N, inserts N lines into the specified window above the
current line.  For negative N, deletes N lines starting with the one
under the cursor.  Optionally, move to position X, Y first."
  (assert-window win)
  (assert-integer n)
  (assert-pos-if-defined x y)
  (and
   (if (and y x)
       (%wmove win y x)
       #t)
   (or (%winsdelln win n)
       (raise (condition (&curses-bad-state-error))))))

(define* (insertln win #:key y x)
  "Inserts a line in the current window above the current line,
optionally first moving to the location X, Y. "
  (assert-window win)
  (assert-pos-if-defined x y)
  (and
   (if (and y x)
       (%wmove win y x)
       #t)
   (or (%winsdelln win 1)
       (raise (condition (&curses-bad-state-error))))))

(define* (insstr win str #:key y x (n -1))
  "Insert a character string (a regular scheme string) before the
character under the cursor.  All character to the right of thec cursor
are shifted right.  If N is given, it will insert at most N
characters.  If Y and X are given, the cursor will be moved to that
location before inserting."
  (assert-window win)
  (assert-string str)
  (assert-pos-if-defined x y)
  (assert-integer n)
  (if (and y x)
      (%wmove win y x)
      #t)
  (let ((ret (%winsnstr win str n)))
    (if (not ret)
        (raise (condition (&curses-bad-state-error))))))

(define (intrflush! bf)
  "If BF is #t, this enables the intrflush option.  When an interrupt key
is pressed on the keyboard (interrupt, break, or quit) all output to the
tty driver queue will be flushed. #f disables this."
  (assert-boolean bf)
  (%intrflush! bf))

(define (is-cleared? win)
  "True if clearok is set, that is, if the next call to 'refresh'
will clear the screen completely and redraw"
  (assert-window win)
  (%is-cleared? win))

(define (is-idcok? win)
  "True if idcok is set, that is, if curses is allowed to use the hardware
insert/delete character feature of the terminal."
  (assert-window win)
  (%is-idcok? win))

(define (is-idlok? win)
  "True if idlok is set, that is, if curses is allowed to use the hardware
insert/delete line feature of the terminal."
  (assert-window win)
  (%is-idlok? win))

(define (is-immedok? win)
  "True if immedok is set, that is, if any change in the window will
cause it to automatically refresh."
  (assert-window win)
  (%is-immedok? win))

(define (is-keypad? win)
  "True if keypad is set, that is, if function keys will be interpreted for
this window."
  (assert-window win)
  (%is-keypad? win))

(define (is-leaveok? win)
  "True if leaveok is set, that is, if you don't care that the cursor gets
returned to its true position after a refresh."
  (assert-window win)
  (%is-leaveok? win))


(define (is-nodelay? win)
  "True if nodelay is set, that is, if 'getch' is a non-blocking call."
  (assert-window win)
  (%is-nodelay? win))

(define (is-notimeout? win)
  "True if notimeout is set."
  (assert-window win)
  (%is-notimeout? win))

(define (is-pad? win)
  "True if the window is a pad. Also true if we can't distinguish between
pads and normal windows, which could be true if you're running old versions
of curses that have been compiled to be opaque.  False otherwise.

To see if this function can actually detect a pad, check the value of the
%is-pad-broken constant."
  (assert-window win)
  (if %is-pad-broken
      #t
      (%is-pad? win)))

(define (is-scrollok? win)
  "True if scrollok is set.  That is, if attempting to move off the bottom
margin of the screen will cause the window to scroll."
  (assert-window win)
  (%is-scrollok? win))

(define (is-subwin? win)
  "True if this window is a subwindow of another window.  Also true if
the underlying ncurses library has no way of reporting if a window
is a subwindow, which may be true for older versions of ncurses compiled
to be opaque.  False otherwise.

To see if this function actually works, chech the value of the
%is-subwin-broken constant."
  (assert-window win)
  (if %is-subwin-broken
      #t
      (%is-subwin? win)))

(define (is-syncok? win)
  "True if syncok is enabled, e.g, if every change to a window changes
the ancestors of that window."
  (assert-window win)
  (%is-syncok? win))

(define (isendwin?)
  "Returns #t if the program has escaped from curses mode by calling 'endwin'"
  (%isendwin?))

(define (keypad! win bf)
  "If BF is true, functions keys will create integer key constants in routines
like 'getch'.  If it is #f, function keys will emit escape character
sequences that are specific to the terminal."
  (assert-window win)
  (assert-boolean bf)
  (%keypad! win bf))

(define (killchar)
  "Returns the character used to kill lines."
  (%killchar))

(define (leaveok! win bf)
  "If leaveok! is called with BF as #t, the cursor is allowed to be
left wherever it happens to be, instead of taking the time to move it
into its proper position.  This is a slight optimization for those
programs that don't use the cursor."
  (assert-window win)
  (assert-boolean bf)
  (%leaveok! win bf))

(define (longname)
  "Returns the name of the current terminal."
  (%longname))

(define (mcprint str)
  "If the terminal has a printer attached and had the capability to
send data to it, this procedure will send STR to the printer.  It
returns #f on error or otherwise the number of characters sent."
  (%mcprint str))

(define (meta! bf)
  "If BF is true, the terminal will return 8 significant bits on input.  If
it is #f, 7 bit input will be returned."
  (assert-boolean bf)
  (let ((ret (%meta! bf)))
    (if (not ret)
        (raise (condition (&curses-bad-state-error))))))

(define (mouseinterval delay)
  "Set the maximum time, in thousandths of a second, between click and
release to register as a button press.  Returns the previous value.
If delay is -1, it returns the current interval without setting it."
  (assert-integer delay)
  (%mouseinterval delay))

(define (mousemask mask)
  "Given a bit-mask of mouse events such as BUTTON1_PRESSED, this
procedure sets the mouse events that are to be captured.  It
returns a mask of those events that actually can be reported, or
zero on failure.  Setting the mouse mask to zero turns off mouse
events."
  (assert-integer mask)
  (%mousemask mask))

(define (mouse-trafo win sy sx to_screen)
  "If TO_SCREEN is #t, converts the window-relative coordinates SY and SX
to stdscr relative coordinates, returned as a two-element list (Y X).  If
TO_SCREEN is #f, it does the opposite conversion.  It returns #f if
a conversion would put a location outside the window."
  (assert-window win)
  (assert-integer sy)
  (assert-integer sx)
  (assert-boolean to_screen)
  (%mouse-trafo win sy sx to_screen))

(define (move win y x)
  "Move the cursor to the position X, Y in the window WIN. Return #t
if the position is within in the window."
  (assert-window win)
  (assert-integer y)
  (assert-integer x)
  (%wmove win y x))

(define (mvderwin win y x)
  "Moves the derived window inside its parent window so that the
upper-left corner is at Y, X."
  (assert-window win)
  (assert-integer y)
  (assert-integer x)
  (%mvderwin win y x))

(define (mvwin win y x)
  "Moves the window so that the upper-left corner is at Y, X."
  (assert-window win)
  (assert-integer y)
  (assert-integer x)
  (%mvwin win y x))

(define (napms ms)
  "Pause for MS milliseconds."
  (assert-integer ms)
  (%napms ms))

(define (newpad nlines ncols)
  "Creates and returns a pointer to a new pad data structure with the
given number of lines and columns.  A pad is like a window, except it is
not restricted by screen size.  It returns a window structure on success
or #f on failure"
  (assert-integer nlines)
  (assert-integer ncols)
  (%newpad nlines ncols))

(define (newterm type outport inport)
  "Create a new terminal whose input and output are Guile ports."
  ;; (if (not (defined? '%newterm))
  ;;     (raise (condition (&curses-missing-function-error
  ;;                        (function '%newterm)))))
  (assert-string type)
  (if (not (output-port? outport))
      (raise (condition (&curses-wrong-type-arg-error
                         (arg outport)
                         (expected-type 'output-port)))))
  (if (not (input-port? inport))
      (raise (condition (&curses-wrong-type-arg-error
                         (arg inport)
                         (expected-type 'input-port)))))
  (let ((ret (%newterm type outport inport)))
    (cond
     ((integer? ret)
      (if (= ret 1)
          (raise (condition (&curses-wrong-type-arg-error
                             (arg inport)
                             (expected-type 'input-port)))))
      (if (= ret 2)
          (raise (condition (&curses-wrong-type-arg-error
                             (arg outport)
                             (expected-type 'output-port)))))
      (if (= ret 3)
          (raise (condition (&curses-bad-state-error)))))
     (else
      ret))))

(define* (newwin nlines ncols begin_y begin_x #:key (panel #f) (name #f))
  "Creates and returns a window with the given number of lines and columns.
The upper, left-hand corner of the window is at line BEGIN_Y and column
BEGIN_X.  If either NLINES or NCOLS is zero, the window will extend to the
bottom, right-hand cornder of the screen.  A new, full-screen window is
created by calling (newwin 0 0 0 0).

If the optional keyword argument #:panel is #t, the window is converted to a panel."
  (for-each (lambda (x) (assert-integer x))
            (list nlines ncols begin_y begin_x))
  (let ((win (%newwin nlines ncols begin_y begin_x name)))
    (if (and win panel)
        (make-panel! win))
    win))

(define (nl!)
  "Enables the underlying display device to translate the return key
into newline on input and to translate newline into return and
line-feed on output."
  (%nl!))

(define (nocbreak!)
  "Enable line buffering and and erase/kill character processing."
  (%nocbreak!))

(define (nodelay! win bf)
  "If BF is true, 'getch' will be a non-blocking call. If BF is #f, 'getch'
will wait for input."
  (assert-window win)
  (assert-boolean bf)
  (let ((ret (%nodelay! win bf)))
    (if (not ret)
        (raise (condition (&curses-bad-state-error))))))

(define (noecho!)
  "Disable echoing of typed characters."
  (%noecho!))

(define (nonl!)
  "Prevents the underlying display device from translating the return
key into newline on input and translating newline into return and
line-feed on output."
  (%nonl!))

(define (notimeout! win bf)
  "If BF is false, 'getch' sets a timer for how long it will wait for the
bytes that make up an escape sequence.  If it is true, it does not set
a timer and will likely not interpret function keys correctly."
  (assert-window win)
  (assert-boolean bf)
  (let ((ret (%notimeout! win bf)))
    (if (not ret)
        (raise (condition (&curses-bad-state-error))))))

;; I hate it when people are 'clever' with dropping letters
(define (nooutrefresh win)
  "An alias for noutrefresh"
  (assert-window win)
  (noutrefresh win))

(define (noutrefresh win)
  "The procedure copies this window to the virtual screen.  Calling
'doupdate' will then copy the virtual screen to the actual screen."
  (assert-window win)
  (%noutrefresh win))

(define (noqiflush!)
  "Disable flushing of the input and output queues when an interrupt is
received."
  (let ((ret (%noqiflush!)))
    (if (not ret)
        (raise (condition (&curses-bad-state-error))))))

(define (noraw!)
  "Enable line buffering and erase/kill character processing."
  (let ((ret (%noraw!)))
    (if (not ret)
        (raise (condition (&curses-bad-state-error))))))

(define (overlay srcwin dstwin)
  "This procedure overlays window SRCWIN on top of the window DSTWIN,
so that the text in SRCWIN is copied to DSTWIN.  SRCWWIN and DSTWIN
need not be the same size.  Blanks are not copied."
  (assert-window srcwin)
  (assert-window dstwin)
  (let ((ret (%overlay srcwin dstwin)))
    (if (not ret)
        (raise (condition (&curses-bad-state-error))))))

(define (overwrite srcwin dstwin)
  "This procedure overlays window SRCWIN on top of the window DSTWIN,
so that the text in SRCWIN is copied to DSTWIN.  SRCWWIN and DSTWIN
need not be the same size.  Both text and blanks are copied."
  (assert-window srcwin)
  (assert-window dstwin)
  (let ((ret (%overwrite srcwin dstwin)))
    (if (not ret)
        (raise (condition (&curses-bad-state-error))))))

(define (pair-content pair)
  "Given a color pair number, this procedure returns a two-element
list containing the foreground color number and the background color
number.  It can return #f if the color pair number is out of range or
colors aren't initialized."
  (assert-integer pair)
  (%pair-content pair))

(define* (pechochar win ch #:key y x)
  "Add character ch to the pad WIN, and then refresh the pad."
  (assert-pad win)
  (assert-xchar ch)
  (assert-pos-if-defined x y)
  (and
   (if (and y x)
       (%wmove win y x)
       #t)
   (%pechochar win (xchar->list ch))))

(define (pnoutrefresh pad pminrow pmincol sminrow smincol smaxrow smaxcol)
  "This is analagous to wnoutrefresh except for pads instead of windows.
The additional parameters are needed to indicate what part of the pad
and screen are involved.  The PMINROW and PMINCOL parameters specify
the upper left-hand corner of the rectangle to be displayed in the
pad.  SMINROW, SMINCOL, SMAXROW and SMAXCOL specify the edges of the
rectangle to be displayed on the screen."
  (assert-pad pad)
  (for-each (lambda (x) (assert-integer x))
            (list pminrow pmincol sminrow smincol smaxrow smaxcol))
  (%pnoutrefresh pad pminrow pmincol sminrow smincol smaxrow smaxcol))


(define (prefresh pad pminrow pmincol sminrow smincol smaxrow smaxcol)
  "This is analagous to wrefresh except for pads instead of windows.
The additional parameters are needed to indicate what part of the pad
and screen are involved.  The PMINROW and PMINCOL parameters specify the
upper left-hand corner of the rectangle to be displayed in the pad.
SMINROW, SMINCOL, SMAXROW and SMAXCOL specify the edges of the rectangle
to be displayed on the screen."
  (assert-pad pad)
  (for-each (lambda (x) (assert-integer x))
            (list pminrow pmincol sminrow smincol smaxrow smaxcol))
  (%prefresh pad pminrow pmincol sminrow smincol smaxrow smaxcol))

(define (qiflush!)
  "Enable flushing of the input and output queues when an interrupt is
received."
  (let ((ret (%qiflush!)))
    (if (not ret)
        (raise (condition (&curses-bad-state-error))))))

(define (raw!)
  "Disable line buffering, interrupt processing, and erase/kill
character processing."
  (let ((ret (%raw!)))
    (if (not ret)
        (raise (condition (&curses-bad-state-error))))))

(define (redrawln win beg_line end_line)
  "This procedure notifies curses that it should not use optimization when
redrawing the following lines of the window.  It should redraw those lines
entirely."
  (assert-window win)
  (assert-integer beg_line)
  (assert-integer end_line)
  (%wredrawln win beg_line end_line))

(define (redrawwin win)
  "This procedure notifies curses that it should not use optimization on
the next screen refresh.  It should redraw the entire window."
  (assert-window win)
  (%redrawwin win))

(define (refresh win)
  "This procedure must be called to get actual output to the terminal, as
other routines simply manipulate data structures.  This procedure copies
the named window to the physical terminal screen."
  (assert-window win)
  (%refresh win))

(define (reset-prog-mode)
  "If 'def-prog-mode' was called to store the terminal's state, this
procedure will restore it to that state."
  (%reset-prog-mode))

(define (reset-shell-mode)
  "If 'def-shell-mode' was called to store the terminal's state, this
procedure will restore it to that state."
  (%reset-shell-mode))

(define (resetty)
  "Restores the state of the terminal modes."
  (%resetty))

(define (resettty)
  "An alias for 'resetty'"
  (%resetty))

(define (savetty)
  "Saves the state of the terminal modes."
  (%savetty))

(define (scr-dump filename)
  "Dumps the current contents of the virtual screen to the file
FILENAME."
  (assert-string filename)
  (%scr-dump filename))

(define (scr-restore filename)
  "Reads the file FILENAME generated by a call to 'scr-dump' and copies the
contents to the virtual screen.  A call to 'doupdate' will make it visible."
  (assert-string filename)
  (%scr-restore filename))

(define (scr-init filename)
  "Reads the file FILENAME generated by a call to 'scr-dump' and uses it to
initialize the curses data structures."
  (assert-string filename)
  (%scr-init filename))

(define (scr-set filename)
  "Reads the file FILENAME generated by a call to 'scr-dump' and uses it to
initialize the curses data structures as well as the virtual screen."
  (assert-string filename)
  (%scr-set filename))

(define (scrl win n)
  "Scroll the window by N lines.  N can be negative"
  (assert-window win)
  (assert-integer n)
  (%scrl win n))

(define (scroll win)
  "Scroll the window by 1 line."
  (assert-window win)
  (%scrl win 1))

(define (set-term term)
  "Switch to a new terminal indicated by the parameter TERM.  TERM has
the <#screen> type and is created by 'newterm'."
  (if (not (screen? term))
      (raise (condition (&curses-wrong-type-arg-error
                         (arg term)
                         (expected-type 'screen)))))
  (%set-term term))

(define (setscrreg! win top bot)
  "Sets the region of the window that will be scrolled if scrolling is
enabled.  If the cursor moves off the bottom of the BOT line of the
window, the region from line number TOP to line number BOT will
scroll."
  (assert-window win)
  (assert-integer top)
  (assert-integer bot)
  (%setscrreg! win top bot))

(define (setsyx y x)
  "Sets the virtual screen cursor to X, Y.  (Why would anyone use
this?)"
  (%setsyx y x))

(define* (setupterm #:optional (term #f))
  "Searches the terminfo database for the a terminal with the given
name.  If the optional argument term is not given or is #f, the name of
the terminal in the TERM environment variable is used.  If the
terminal is not found in the terminfo database, #f is returned.  If it
is found, one of the symbols 'terminal or 'hardcopy is returned.

If the terminal is found, then the 'tiget' procedure can be used
to query terminfo capabilities for this terminal."
  (%setupterm term))

(define (standend! win)
  "Turns off all attributes of the given window."
  (assert-window win)
  (%wattr-set! win A_NORMAL (second (%wattr-get win))))

(define (standout! win)
  "Sets the attributes for in the given window to STANDOUT."
  (%wattr-set! win A_STANDOUT (second (%wattr-get win))))

(define (start-color!)
  "Enables color support for curses.  Usually called directly after 'initscr.'"
  (let ((ret (%start-color!)))
    (if (not ret)
        (raise (condition (&curses-bad-state-error))))))

(define (subpad origwin nlines ncols begin_y begin_x)
  "Creates and returns a window strucure of a subwindow within a pad window.
The subpad has NLINES lines and NCOLS columns and begins at the position
BEGIN_X and BEGIN_Y on the pad.  The window is made in the middle of the
window ORIGWIN, so that changes made to one affect both windows.  It
returns #f on failure or a window structure on success"
  (assert-pad origwin)
  (assert-integer nlines)
  (assert-integer ncols)
  (assert-integer begin_y)
  (assert-integer begin_x)
  (%subpad origwin nlines ncols begin_y begin_x))

(define* (subwin origwin nlines ncols begin_y begin_x #:key (panel #f) (name #f))
  "Creates and returns a new window with the given number of lines and
columns.  The window is at position BEGIN_Y, BEGIN_X on the screen.
The window is made in the middle of the window ORIGWIN, so that
changes made to one window will affect both windows.  The subwindow
shares memory with the original window.

If optional keyword argument #:panel is #t, the window is converted into a panel."
  (assert-window origwin)
  (assert-integer nlines)
  (assert-integer ncols)
  (assert-integer begin_y)
  (assert-integer begin_x)
  (let ((win (%subwin origwin nlines ncols begin_y begin_x name)))
    (if (and win panel)
        (make-panel! win))
    win))

(define (syncdown win)
  "Touches all location in children of WIN that are changed in WIN."
  (assert-window win)
  (%wsyncdown win))

(define (syncup win)
  "Touches all location in ancestors of WIN that are changed in WIN."
  (assert-window win)
  (%wsyncup win))

(define (term-attrs)
  "Returns an integer that is a 'logical or' of all the video attributes
supported by the terminal.  It can be compared to the constants A_ALTCHARSET,
A_ATTRIBUTES, A_BLINK, A_BOLD, etc"
  (%term-attrs))

(define (termname)
  "Returns the name of the current terminal"
  (%termname))

(define (timeout! win delay)
  "Sets the amount of time that 'getch' will wait for a character.  If DELAY
is negative, blocking read is used.  If DELAY is zero, non-blocking read is used.
If delay is positive, 'getch' will block for DELAY milliseconds."
  (assert-window win)
  (assert-integer delay)
  (%timeout! win delay))

(define (tiget id)
  "Searches the terminfo database for the a capability for the current
terminal. ID is a string containing the abbreviated terminfo ID
string (such as \"kbs\" for backspace, etc). If a capability by that
name is found for the current terminal, an integer, string, or flag
will be returned depending on the capability.  If a capability by that
name is not found, #f will be returned.  It is not possible to
distinguish between capabilities that are false, and capabilities that
are not present."
  (assert-string id)
  (%tiget id))

(define (touchwin win)
  (assert-window win)
  (%wtouchln win 0 (getmaxy win) #t))

(define (untouchwin win)
  (assert-window win)
  (%wtouchln win 0 (getmaxy win) #f))

(define* (touchline win start #:optional (count 1))
  (assert-window win)
  (%wtouchln win start count #t))

(define* (untouchline win start #:optional (count 1))
  (assert-window win)
  (%wtouchln win start count #f))

(define (typeahead! port-or-fd)
  "Specifies an input port or file descriptor to be used for typeahead
checking."
  (if (and (not (input-port? port-or-fd)) (not (integer? port-or-fd)))
      (raise (condition (&curses-wrong-type-arg-error
                         (arg port-or-fd)
                         (expected-type 'input-port)))))
  (cond
   ((port? port-or-fd) (%typeahead! (fileno port-or-fd)))
   (else (%typeahead! port-or-fd))))

(define (unctrl ch)
  "Returns a character string which is a printable representation of
the character CH. Control character will be escaped with the ^X notation.
The DEL character will be displayed as ^?."
  (cond
   ((char? ch)
    (%unctrl (xchar->list (make-xchar A_NORMAL 0 (list ch)))))
   ((xchar? ch)
    (%unctrl (xchar->list ch)))
   (else
    (raise (condition (&curses-wrong-type-arg-error
                       (arg ch)
                       (expected-type 'xchar)))))))

(define (ungetch ch)
  "Pushes back a character onto the input queue so that it can later
be retrieved with getch.  CH must either be a character or a curses
key constant like KEY_ENTER."
  (if (and (not (integer? ch)) (not (char? ch)))
      (raise (condition (&curses-wrong-type-arg-error
                         (arg ch)
                         (expected-type 'integer/char)))))
  (%ungetch ch))

(define (ungetmouse event)
  "Pushes the mouse event EVENT back onto the input queue."
  (if (not (mevent? event))
      (raise (condition (&curses-wrong-type-arg-error
                         (arg event)
                         (expected-type 'mevent)))))
  (%ungetmouse event))

(define (use-extended-names enable)
  "If ENABLE is #t, this enables whether to allow user-defined,
nonstandard names in the terminfo interface.  If ENABLE is #f, this is
disabled"
  (assert-boolean enable)
  (%use-extended-names enable))

(define* (vline win ch n #:key y x)
  "Draws a vertical line of length N using the complex character CH.
If X and Y are given, the cursor is first moved to that location."
  (assert-window win)
  (assert-xchar ch)
  (assert-integer n)
  (assert-pos-if-defined x y)
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (or
        (%wvline win (xchar->list ch) n)
        (raise (condition (&curses-bad-state-error))))))

(define (wenclose? win y x)
  "Returns #t if the screen-relative coordinates Y and X are enclosed
by the given window."
  (assert-window win)
  (assert-integer x)
  (assert-integer y)
  (%wenclose? win y x))

(load-extension "libguile-ncurses" "gucu_init")
(load-extension "libguile-ncurses" "gucu_panel_init_type")

;; These functions may not exist depending on the
;; compile time options
(if (defined? 'KEY_EVENT)    (export KEY_EVENT))
(if (defined? 'key-defined)  (export key-defined))
(if (defined? 'ptsraw)       (export ptsraw))
(if (defined? '%newterm)     (export newterm))
(if (defined? 'BUTTON5_PRESSED) (export BUTTON5_PRESSED))
(if (defined? 'BUTTON5_RELEASED) (export BUTTON5_RELEASED))
(if (defined? 'BUTTON5_CLICKED) (export BUTTON5_CLICKED))
(if (defined? 'BUTTON5_DOUBLE_CLICKED) (export BUTTON5_DOUBLE_CLICKED))
(if (defined? 'BUTTON5_TRIPLE_CLICKED) (export BUTTON5_TRIPLE_CLICKED))
(if (defined? '%has-mouse?)  (export has-mouse?))
(if (defined? '%getdelay)    (export getdelay))

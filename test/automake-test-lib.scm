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

(define-module (test automake-test-lib)
  #:export (EXIT_SUCCESS
	    EXIT_FAILURE
	    EXIT_SKIPPED
	    EXIT_HARD_ERROR
	    automake-test
	    maybe-sleep))

(define EXIT_SUCCESS 0)
(define EXIT_FAILURE 1)
(define EXIT_SKIPPED 77)
(define EXIT_HARD_ERROR 99)

(define *sleep* #f)

(define (maybe-sleep n)
  (if (or *sleep*
          (getenv "GUCU_VERBOSE_TEST"))
      (sleep n)))

(define-syntax hard-error-if-exception
  (syntax-rules ()
    ((hard-error-if-exception expr)
     (catch #t
	    (lambda () expr)
	    (lambda args EXIT_HARD_ERROR)))
    ((hard-error-if-exception expr #:warning template arg ...)
     (catch #t
	    (lambda () expr)
	    (lambda (key . args)
	      (for-each (lambda (s)
			  (if (not (string-null? s))
			      (format (current-warning-port) ";;; ~a\n" s)))
			(string-split
			 (call-with-output-string
			  (lambda (port)
			    (format port template arg ...)
			    (print-exception port #f key args)))
			 #\newline))
	      EXIT_HARD_ERROR)))))

(define (automake-test x)
  (let ((ret (hard-error-if-exception x)))
    (cond
     ((eqv? ret EXIT_HARD_ERROR)
      (exit EXIT_HARD_ERROR))
     ((eqv? ret #t)
      (exit EXIT_SUCCESS))
     ((eqv? ret #f)
      (exit EXIT_FAILURE))
     ((eqv? ret 'skipped)
      (exit EXIT_SKIPPED)))))

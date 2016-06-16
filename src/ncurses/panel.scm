;; -*- Mode: scheme; -*-

;; panel.scm

;; Copyright 2009, 2010, 2016 Free Software Foundation, Inc.

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

(define-module (ncurses panel)
  #:use-module (srfi srfi-1)
  #:export (
            ;; These were forward declared in (ncurses curses)
            ;; panel?
            ;; make-panel!
            ;; del-panel!

            ;; These are declared here
            bottom-panel
            top-panel
            show-panel
            update-panels
            hide-panel
            panel-window
            replace-panel!
            move-panel
            panel-hidden?

            ;;panel-above
            ;;panel-below
            ;;set-panel-userdata
            ;;panel-userdata
            panels-map
            panels-for-each
            ))

(define (panels-map proc)
  "Applies the procedure PROC to all currently allocated #<panel>s.
The panels are traversed in top-down order.  The return values of PROC
are collected in a list.  Note that both shown and hidden panels are
traversed."
  (map-in-order proc (%panels-list)))

(define (panels-for-each proc)
  "Applies the procedure PROC to all currently allocated #<panel>s.
The panels are traversed in top-down order.  The return values of PROC
are discarded.  Note that both shown and hidden panels are
traversed. The return value is unspecified."
  (for-each proc (%panels-list)))


(load-extension "libguile-ncurses" "gucu_panel_init")

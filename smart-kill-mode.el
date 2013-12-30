;; Copyright (C) 2013  Free Software Foundation, Inc.
;;
;; Author: feigo  <feigo.zj@gmail.com>
;; Keywords:
;; Created: 2013-03-28
;; Version: 0.0.1
;; Keywords: smart kill mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation,


;; Installation:
;;   Put the smart-kill-mode.el to your load-path.
;;   And add to .emacs: (require 'smart-kill-mode)


(require 'thingatpt)


(require 'thingatpt-ext)

(defvar smart-kill-after-hook nil)

(defvar smart-kill-keymap nil)

(defvar smart-kill-arg nil 
  "arg num")

(defvar smart-cut-keymap nil)
(defvar smart-kill-negative-arg nil)


(defvar smart-kill-flag nil)



(defun test-point-forward (arg) 
  (let ((bounds (bounds-of-thing-at-point arg))) 
    (unless bounds 
      (forward-thing arg 1))))


(defun smart-string(bcopy delimit &optional arg) 
  "only not comment string " 
  (if smart-kill-negative-arg 
      (setq arg (- 0 arg))) 
  (let ((bounds (thing-at-point-bounds-of-delimited-thing-at-point delimit)) 
        (beg) 
        (end)) 
    (if bounds 
        (progn 
          (setq beg (car bounds)) 
          (setq end (cdr bounds)) 
          (show-msg-begin-end beg end bcopy) 
          (smart-copy-or-cut-region beg end bcopy t)) 
      (message "no delimit -%s to %s - " delimit delimit))))

(defun smart-string-inside(bcopy delimit &optional arg) 
  "only not comment string " 
  (if smart-kill-negative-arg 
      (setq arg (- 0 arg))) 
  (let ((bounds (thing-at-point-bounds-of-delimited-thing-at-point delimit)) 
        (beg) 
        (end)) 
    (if bounds 
        (progn 
          (setq beg (+ (car bounds) 1) ) 
          (setq end (- (cdr bounds) 1) ) 
          (show-msg-begin-end beg end bcopy) 
          (smart-copy-or-cut-region beg end bcopy t)) 
      (message "no delimit -%s to %s - " delimit delimit))))

(defun smart-delimit(bcopy ldel rdel &optional arg) 
  "" 
  (if smart-kill-negative-arg 
      (setq arg (- 0 arg))) 
  (let ((bounds (thing-at-point-bounds-of-pair-delimited-thing-at-point ldel rdel))

        ;; (bounds (tap-bounds-of-thing-nearest-point 'string))
        (beg) 
        (end)) 
    (if  bounds 
        (progn 
          (setq beg (car bounds)) 
          (setq end (cdr bounds)) 
          (smart-copy-or-cut-region beg end bcopy)) 
      (message "nothing delimit -%s to %s - " ldel rdel))))


;; (define-key global-map "\C-cl" ' test-ext )


(defun smart-block-kill( &optional arg) 
  (interactive "p") 
  (toggle-off-smart-kill-mode) 
  (smart-delimit smart-kill-flag "\{" "\}" (get-max-arg arg)))

(defun smart-bracket-kill( &optional arg) 
  (interactive "p") 
  (toggle-off-smart-kill-mode) 
  (smart-delimit smart-kill-flag "\[" "\]" (get-max-arg arg)))

(defun smart-angle-bracket-kill( &optional arg) 
  (interactive "p") 
  (toggle-off-smart-kill-mode) 
  (smart-delimit smart-kill-flag "\(" "\)" (get-max-arg arg)))


(defun smart-string-kill( &optional arg) 
  (interactive "p") 
  (toggle-off-smart-kill-mode) 
  (smart-string smart-kill-flag "\"" (get-max-arg arg)))

(defun smart-string-inside-kill( &optional arg) 
  (interactive "p") 
  (toggle-off-smart-kill-mode) 
  (smart-string-inside smart-kill-flag "\"" (get-max-arg arg)))






(defun smart-paragraph(bcopy &optional arg) 
  "" 
  (save-excursion

    ;; (test-point-forward 'paragraph)
    (let (    (beg 
               (progn 
                 (forward-paragraph arg) 
                 (point))) 
              (end 
               (progn 
                 (backward-paragraph arg) 
                 (point)))) 
      (show-msg-begin-end beg end bcopy) 
      (smart-copy-or-cut-region beg end bcopy t))))



(defun smart-paragraph-kill( &optional arg) 
  (interactive "p") 
  (toggle-off-smart-kill-mode)
  ;; ( beg)
  (smart-paragraph smart-kill-flag (get-max-arg arg))
  ;; (if smart-kill-flag
  ;;     (message "%d paragraph copied" arg)
  ;;   (message "%d paragraph cut" arg))
  )
(defun smart-function(bcopy &optional arg) 
  "only for lisp and c++ c mode" 
  (if smart-kill-negative-arg 
      (setq arg (- 0 arg))) 
  (save-excursion 
    (test-point-forward 'defun) 
    (if (< arg 0) 
        (progn 
          (forward-thing 'defun arg) 
          (setq arg (- 0 arg)))) 
    (let (           (bounds (get-thing-forward-defun) ) beg end) 
      (when bounds 
        (setq beg (car bounds)) 
        (setq end (cdr bounds)) 
        (show-msg-begin-end beg end bcopy) 
        (smart-copy-or-cut-region beg end bcopy t )))))



(defun smart-paragraph-copy-and-yank( ) 
  (interactive ) 
  (let((bounds (get-thing-forward-paragraph))) 
    (toggle-off-smart-kill-mode) 
    (when bounds) 
    (smart-copy-or-cut-region (car bounds) 
                              (cdr bounds) t t ) 
    (insert-copy-and-yank (car bounds) 
                          (cdr bounds) t )))

(defun insert-copy-and-yank(beg end &optional goto) 
  ""
  (goto-char beg) 
  (open-line 2) 
  (forward-line 1) 
  (yank) 
  (unless goto 
    (goto-char beg) 
    (forward-line 1)))

(defun smart-mark-copy-and-yank( ) 
  (interactive ) 
  (let((beg ) 
       (end )) 
    (when (region-active-p) 
      (setq beg (region-beginning)) 
      (setq end (region-end)) 
      (smart-copy-or-cut-region  beg end t t ) 
      (insert-copy-and-yank beg end  t ))))

(defun smart-function-copy-and-yank( ) 
  (interactive ) 
  (let((bounds (get-thing-forward-defun))) 
    (toggle-off-smart-kill-mode) 
    (when bounds) 
    (smart-copy-or-cut-region (car bounds) 
                              (cdr bounds) t t ) 
    (insert-copy-and-yank (car bounds) 
                          (cdr bounds) t)))


(defun smart-function-kill( &optional arg) 
  (interactive "p") 
  (toggle-off-smart-kill-mode) 
  (smart-function smart-kill-flag (get-max-arg arg)))


(defun smart-yank-n-times (&optional arg) 
  "yank prefix-arg number of times. Call yank-pop if last command was yank." 
  (interactive "*p") 
  (toggle-off-smart-kill-mode) 
  (if (or (string= last-command "yank") 
          (string= last-command "yank-pop")) 
      (yank-pop arg) 
    (if (>= arg 1) 
        (dotimes 'arg 
          (yank)) 
      (message "Previous arg was not a yank, and called without a prefix."))))




(defun smart-append-current-line (&optional arg) 
  "copy current line and append N linee  after current " 
  (interactive "p") 
  (toggle-off-smart-kill-mode) 
  (setq arg (get-max-arg arg)) 
  (save-excursion 
    (unless arg 
      (setq arg 1)) 
    (smart-whole-line-kill  1) 
    (message "1") 
    (forward-line) 
    (beginning-of-line) 
    (if (> arg 1) 
        (smart-yank-n-times arg) 
      (call-interactively 'yank))))


(defun smart-copy-or-cut-region (beg end bcopy &optional bnomessage) 
  (if bcopy 
      (progn 
        (unless bnomessage 
          (message "[%s] copied." 
                   (buffer-substring 
                    beg
                    end))) 
        (copy-region-as-kill beg end )) 
    (progn 
      (unless bnomessage 
        (message "[%s] cut." 
                 (buffer-substring 
                  beg
                  end))) 
      (kill-region beg end ))))


(defun smart-line-beginning (bcopy) 
  (let ((end (point)) 
        (beg (line-beginning-position))) 
    (smart-copy-or-cut-region beg end bcopy )))


(defun smart-line-beginning-kill-rect (&optional arg) 
  " kill rect " 
  (let ((startpt nil) 
        (endpt nil)) 
    (progn 
      (setq startpt (point)) 
      (next-line (- arg 1)) 
      (move-beginning-of-line 1) 
      (setq endpt (point)) 
      (kill-rectangle startpt endpt t) 
      (message "%d lines begin killed"))))






(defun smart-line-beginning-kill (&optional arg) 
  "copy line from point to end" 
  (interactive "p") 
  (toggle-off-smart-kill-mode)

                                        ;(save-excursion
  (when  (< arg 0) 
    (progn 
      (next-line (+ arg 1)) 
      (setq arg (- 0 arg)))) 
  (if  (= arg 1) 
      (smart-line-beginning smart-kill-flag) 
    (smart-line-beginning-kill-rect  (get-max-arg arg))))


(defun smart-line-end (bcopy ) 
  "cut line from point to end" 
  (let ((beg (point)) 
        (end (line-end-position))) 
    (smart-copy-or-cut-region beg end bcopy)))


(defun smart-line-end-kill-rect (&optional arg) 
  " kill rect " 
  (let ((startpt nil) 
        (endpt nil)) 
    (progn 
      (setq startpt (point)) 
      (next-line (- arg 1)) 
      (setq endpt (point)) 
      (rect-kill-right-rectangle startpt endpt t) 
      (message "%d lines end killed" arg))))


(defun smart-line-end-kill (&optional arg) 
  "copy line from point to end" 
  (interactive "p") 
  (toggle-off-smart-kill-mode) 
  (save-excursion 
    (when  (< arg 0) 
      (progn 
        (next-line (+ arg 1)) 
        (setq arg (- 0 arg)))) 
    (if (= arg 1) 
        (smart-line-end smart-kill-flag) 
      (smart-line-end-kill-rect  (get-max-arg arg)))))


(defun smart-whole-line(bcopy  &optional arg) 
  (toggle-off-smart-kill-mode) 
  (if smart-kill-negative-arg 
      (setq arg (- 0 arg))) 
  (let ((argtemp arg) 
        (beg ) 
        (end )) 
    (when  (< arg 0) 
      (progn 
        (next-line (+ arg 1)) 
        (setq arg (- 0 arg)  ))) 
    (setq beg (line-beginning-position)) 
    (setq end (1+ (line-end-position arg))) 
    (show-msg-begin-end beg (- end 1) bcopy) 
    (smart-copy-or-cut-region beg end bcopy t) 
    (when (and bcopy 
               (> argtemp 0)) 
      (next-line (- arg 1)))))



(defun smart-whole-line-kill ( &optional arg) 
  "Save current line into Kill-Ring without mark the line" 
  (interactive "p") 
  (smart-whole-line smart-kill-flag (get-max-arg arg)))


(defun smart-word(bcopy &optional arg ) 
  (save-excursion 
    (test-point-forward 'word) 
    (when (< arg 0) 
      (progn 
        (forward-thing 'word arg) 
        (setq arg (- 0 arg)))) 
    (let (    (beg (beginning-of-thing 'word)) 
              (end 
               (progn 
                 (forward-word arg ) 
                 (end-of-thing 'word)))) 
      (smart-copy-or-cut-region beg end bcopy))))



(defun get-max-arg(arg ) 
  "get max of the  former arg and arg" 
  (let (x) 
    (if  (/= arg 1) 
        (setq x arg) 
      (setq x (max arg smart-kill-arg))) 
    (setq smart-kill-arg 1)
    x))

(defun smart-word-kill(&optional arg) 
  (interactive "p") 
  (toggle-off-smart-kill-mode) 
  (if smart-kill-negative-arg 
      (setq arg (- 0 arg))) 
  (smart-word smart-kill-flag (get-max-arg arg)))



(defun smart-symbol(bcopy &optional arg) 
  (if smart-kill-negative-arg 
      (setq arg (- 0 arg))) 
  (save-excursion 
    (test-point-forward 'symbol)
    ;; ----------
    ;; (let ((bounds (bounds-of-thing-at-point 'symbol)))
    ;; (unless bounds
    ;;   (forward-thing 'symbol 1)
    ;; ))
    ;; ------------
    (when (< arg 0) 
      (progn 
        (forward-thing 'symbol arg) 
        (setq arg (- 0 arg)))) 
    (let (    (beg (beginning-of-thing 'symbol)) 
              (end 
               (progn 
                 (forward-thing 'symbol arg ) 
                 (end-of-thing 'symbol)))) 
      (smart-copy-or-cut-region beg end bcopy))))



(defun smart-symbol-kill( &optional arg) 
  (interactive "p") 
  (toggle-off-smart-kill-mode) 
  (smart-symbol smart-kill-flag (get-max-arg arg)))




(defun smart-goto-match-paren (&optional arg) 
  "Go to the matching parenthesis if on parenthesis. Else go to the
   opening parenthesis one level up." 
  (ignore-errors 
    (cond ((looking-at "\\s\(") 
           (forward-list 1)) 
          (t (backward-char 1) 
             (cond ((looking-at "\\s\)") 
                    (forward-char 1) 
                    (backward-list 1)) 
                   (t 
                    (while (not (looking-at "\\s(")) 
                      (backward-char 1) 
                      (cond ((looking-at "\\s\)") 
                             (message "->> )") 
                             (forward-char 1) 
                             (backward-list 1) 
                             (backward-char 1))))))))))

(defun show-msg-begin-end(beg end bcopy) 
  (let( (msg )) 
    (if (> beg end) 
        (setq msg (get-msg-begin-end end beg )) 
      (setq msg (get-msg-begin-end beg end))) 
    (if bcopy 
        (message "%s copy " msg ) 
      (message "%s cut " msg ))))

(defun get-msg-begin-end(beg end) 
  (save-excursion 
    (let ((beg-line (line-number-at-pos beg)) 
          (end-line (line-number-at-pos end)) 
          (line-count ) 
          (msg) 
          (msg-beg) 
          (msg-end)) 
      (setq line-count (+ (- end-line beg-line) 1))
      ;; (setq line-count (+ line-count 1))
      (message "beg-line=%d end-line= %d" beg-line end-line) 
      (if (eq line-count 1) 
          (setq msg (format "[[[%s]]] " 
                            (buffer-substring 
                             beg
                             end))) 
        (progn 
          (goto-char beg) 
          (end-of-line) 
          (setq msg-beg 
                (buffer-substring 
                 beg
                 (point))) 
          (goto-char end) 
          (beginning-of-line) 
          (setq msg-end 
                (buffer-substring 
                 (point)
                 end)) 
          (setq msg (format "from %d to %d all %d lines ====>>>>%s......%s<<====" beg-line end-line
                            line-count  msg-beg msg-end))))
      msg)))




(defun smart-parenthsis(bcopy  &optional arg) 
  (save-excursion 
    (let (    (beg ) 
              (end ) 
              (bounds )) 
      (setq bounds (get-thing-forward-list) ) 
      (when bounds 
        (setq beg (car bounds)) 
        (setq end (cdr bounds)) 
        (show-msg-begin-end beg end bcopy) 
        (smart-copy-or-cut-region beg end bcopy t )))))





(defun smart-parenthsis-kill(  &optional arg) 
  (interactive "p") 
  (toggle-off-smart-kill-mode) 
  (smart-parenthsis smart-kill-flag (get-max-arg arg)))



(defun toggle-off-smart-kill-mode ( ) 
  (interactive) 
  (smart-kill-mode -1))

(defun toggle-negative-arg ( ) 
  (interactive)
  ;; (smart-kill-mode -1)
  ;; (push last-command-event unread-command-events)
  (setq smart-kill-negative-arg (not smart-kill-negative-arg)) 
  (message "toggle negative arg %s " smart-kill-negative-arg))

(defun close-smart-kill-mode ( ) 
  (interactive) 
  (smart-kill-mode -1) 
  (push last-command-event unread-command-events) 
  (message " sKill mode toggle off"))

;; (setq smart-kill-keymap nil                                           )
(if smart-kill-keymap 
    nil 
  (setq smart-kill-keymap (make-keymap                                   )) 
  (set-char-table-range (nth 1 smart-kill-keymap                         ) t 'close-smart-kill-mode) 
  (suppress-keymap smart-kill-keymap                                     ) 
  (define-key smart-kill-keymap "a"    'smart-line-beginning-kill     ) 
  (define-key smart-kill-keymap "e"    'smart-line-end-kill           ) 
  (define-key smart-kill-keymap "w"    'smart-word-kill               ) 
  (define-key smart-kill-keymap "f"    'smart-function-kill           ) 
  (define-key smart-kill-keymap "F"    'smart-function-copy-and-yank  ) 
  (define-key smart-kill-keymap "\C-f" 'smart-function-copy-and-yank  ) 
  (define-key smart-kill-keymap "l"    'smart-parenthsis-kill         ) 
  (define-key smart-kill-keymap "h"    'smart-paragraph-kill          ) 
  (define-key smart-kill-keymap "\C-h" 'smart-paragraph-copy-and-yank ) 
  (define-key smart-kill-keymap "H"    'smart-paragraph-copy-and-yank ) 
  (define-key smart-kill-keymap "s"    'smart-symbol-kill             ) 
  (define-key smart-kill-keymap "y"    'smart-append-current-line     ) 
  (define-key smart-kill-keymap "d"    'smart-whole-line-kill         ) 
  (define-key smart-kill-keymap "u"    'toggle-negative-arg           ) 
  (define-key smart-kill-keymap "'"    'smart-string-kill       ) 
  (define-key smart-kill-keymap "q"    'smart-string-inside-kill      ) 
  (define-key smart-kill-keymap "Q"    'smart-string-kill             ) 
  (define-key smart-kill-keymap "\["   'smart-bracket-kill            ) 
  (define-key smart-kill-keymap "\]"   'smart-block-kill              ) 
  (define-key smart-kill-keymap "\\"   'smart-angle-bracket-kill      ))





(define-minor-mode smart-kill-mode "Doc description, smart kill mode."
  ;; The initial value - Set to 1 to enable by default
  nil
  ;; The indicator for the mode line.
  " sKillMode"
  ;; The minor mode keymap
  smart-kill-keymap 
  :global nil 
  ;; :after-hook (if smart-kill-flag 
  ;;                 (message "smart copy mode toggle on") 
  ;;               (message "smart cut mode toggle on"))
)




(defun smart-kill-ring-save (&optional arg) 
  "When called interactively with no active region, smart copy  instead." 
  (interactive "p") 
  (if mark-active 
      (call-interactively 'copy-region-as-kill) 
    (progn 
      (setq smart-kill-flag t) 
      (setq smart-kill-arg arg) 
      (setq smart-kill-negative-arg nil)
      (define-key smart-kill-keymap "y"    'smart-append-current-line     ) 
      (define-key smart-kill-keymap "W" 'close-smart-kill-mode               ) 
      (define-key smart-kill-keymap "\C-s" 'close-smart-kill-mode             )

      ( smart-kill-mode 1))))

(require 'smart-yank-mode)

(defun smart-kill-region (&optional arg) 
  "When called interactively with no active region, smart kill instead." 
  (interactive "p") 
  (if mark-active 
      (call-interactively 'kill-region) 
    (progn 
      (setq smart-kill-flag nil) 
      (setq smart-kill-negative-arg nil) 
      (setq smart-kill-arg arg) 
      (define-key smart-kill-keymap "y" 
        (lambda() 
          (interactive) 
          ( smart-kill-mode -1) 
          (smart-yank-call)))
      (define-key smart-kill-keymap "W" 'smart-yank-word) 
      (define-key smart-kill-keymap "\C-s" 'smart-yank-symbol)
      (smart-kill-mode 1))))


(define-key global-map "\C-w" ' smart-kill-region )
(define-key global-map "\M-w" ' smart-kill-ring-save )

(define-key global-map (kbd "C-x C-y") 'smart-yank-n-times)
(define-key global-map (kbd "C-x y") 'smart-mark-copy-and-yank)

(provide 'smart-kill-mode)

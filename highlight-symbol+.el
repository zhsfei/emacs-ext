;;; 

;; Filename: highlight-symbol+.el
;; Description: Support multi repeat command
;; Author: <zhsfei@gmail.com>
;; Maintainer: zhsfei
;; Copyright (C) :2013 zhsfei all rights reserved.
;; Created: :2013-11-29 
;; Version: 0.0.1
;; Keywords: highlight-symbol+  repeat command

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
;;   Put the this package to your load-path.
;;   And add to .emacs: (require 'highlight-symbol+)

;; example 
;; if you have smart-repeat-mode.el
;; (smart-repeat-define-alist  '("\C-ch" "\C-cy") 
;;                             '(("n"  highlight-symbol-next  ) 
;;                               ("p"  highlight-symbol-prev) 
;;                               ("y"  hl-symbol-pop-to-marker) 
;;                               ("h" . highlight-symbol-at-point) 
;;                               ("r"  . highlight-symbol-remove-all)))



(eval-when-compile (require 'cl))
(require 'highlight-symbol)



(defvar hl-ring-global-mark (make-ring 16))

(make-variable-buffer-local 'hl-ring-global-mark)

(defvar hl-ring-global-mark-indexer 0)

(make-variable-buffer-local 'hl-ring-global-mark-indexer)

(defvar hl-symbol-local-old-point nil)
(make-variable-buffer-local 'hl-symbol-local-old-point)


(defvar hl-symbol-global-old-point nil)

(defun hl-symbol-save-old-point(&optional bstart)
  (setq hl-symbol-local-old-point (point-marker))
  (setq hl-symbol-global-old-point hl-symbol-local-old-point )

  (let ((bounds (bounds-of-thing-at-point 'symbol))
        ( symbol (highlight-symbol-get-symbol) ))
    
      (when (and (not bstart)
                 (not (ring-empty-p hl-ring-global-mark)))
        (ring-remove hl-ring-global-mark 0)
        )
      (ring-insert hl-ring-global-mark hl-symbol-global-old-point )
))



(defadvice highlight-symbol-remove-all (after highlight-symbol-remove-all activate)
  (setq hl-ring-global-mark (make-ring 16))
  (setq hl-ring-global-mark-indexer 0)
  (setq hl-symbol-local-old-point nil)
)

(defadvice highlight-symbol-at-point (before highlight-symbol-at-point activate)
  (hl-symbol-save-old-point t)
)

(defadvice highlight-symbol-jump (after highlight-symbol-jump activate)
  (hl-symbol-save-old-point)
)


(defun hl-symbol-jump-to-marker (mark )
  (switch-to-buffer (marker-buffer mark))
  (goto-char (marker-position mark))
  )



(defun hl-symbol-pop-to-marker ( )
  (interactive)
  (let (

        (this-cmd  this-command)
        (last-cmd  last-command)
        )
    (if (not  (eq this-cmd last-cmd))
        (progn (message "marker old")
        (hl-symbol-back-last-point))
    (progn 
      (setq hl-ring-global-mark-indexer (1+ hl-ring-global-mark-indexer))
      (when (>= hl-ring-global-mark-indexer (ring-length hl-ring-global-mark )  
              )
          (setq hl-ring-global-mark-indexer 0)
        )
      (setq hl-symbol-local-old-point (ring-ref hl-ring-global-mark hl-ring-global-mark-indexer ))
      (hl-symbol-back-last-point)
      )
)
)

)





(defun hl-symbol-back-last-point ()
  (interactive)
  (cond 
   ((marker-buffer hl-symbol-local-old-point)
    (hl-symbol-jump-to-marker hl-symbol-local-old-point)
    )
  ((marker-buffer hl-symbol-global-old-point)

    (hl-symbol-jump-to-marker hl-symbol-global-old-point))
    (t          (message "no hi-symbol old  marker")
                nil))
   )



(provide 'highlight-symbol+)

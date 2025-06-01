;;; backline.el --- Preserve appearance of outline headings  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2025 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.backline@jonas.bernoulli.dev>
;; Homepage: https://github.com/tarsius/backline
;; Keywords: outlines

;; Package-Version: 1.1.0
;; Package-Requires: (
;;     (emacs "26.1")
;;     (compat "30.1")
;;     (outline-minor-faces "1.1.3"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; An outline heading does not extend to the right edge of the window
;; when its body is collapsed.  This is unfortunate when the used face
;; sets the background color or another property that is visible on
;; whitespace.  This package adds overlays to extend the appearance of
;; headings all the way to the right window edge.

;;   (use-package backline
;;     :after outline
;;     :config (advice-add 'outline-flag-region :after 'backline-update))

;; The above advice requires that `outline-minor-faces-mode' (from the
;; `outline-minor-faces' package) is enabled.

;;   (use-package outline-minor-faces
;;     :after outline
;;     :config (add-hook 'outline-minor-mode-hook
;;                       #'outline-minor-faces-mode))

;; Do NOT set `outline-minor-mode-highlight' (provided by `outline' since
;; Emacs 28.1) to a non-nil value, because that is incompatible with this
;; package and `outline-minor-faces' (which is an older and still superior
;; alternative).  See `outline-minor-faces' for details.

;;; Code:

(require 'compat)

(require 'outline)
(require 'outline-minor-faces)

(defvar outline-search-function) ;since Emacs 29.1

;;;###autoload
(defun backline-update (from to _flag)
  "For collapsed sections extend their headers' appearance to the window edge.
Do nothing if `outline-minor-mode' isn't enable in the current buffer."
  (when outline-minor-mode
    ;; The two (dolist (ov (overlays-in ...)) ...) operate on very
    ;; different regions and thus cannot be merged.  Here we purge
    ;; the `backline' overlays of all children.  Below we may only
    ;; deal with one section; should individual children need to be
    ;; "flagged", then we are called again, and deal with *adding*
    ;; new `backline' overlays then, if required; and individually.
    (dolist (ov (overlays-in from (1+ (backline--end-of-subtree to))))
      (when (eq (overlay-get ov 'backline-heading) t)
        (delete-overlay ov)))
    (save-excursion
      (let ((toplvl (outline-minor-faces--top-level)))
        (dolist (ov (overlays-in (1- from) (1+ to)))
          (when (eq (overlay-get ov 'invisible) 'outline)
            (goto-char (overlay-start ov))
            (goto-char (line-beginning-position))
            (let ((end (overlay-end ov))
                  (lvl (if (eq outline-level 'lisp-outline-level)
                           ;; Known to use `looking-at' internally.
                           (lisp-outline-level)
                         (looking-at outline-regexp)
                         (funcall outline-level))))
              (unless (= lvl 1000)
                (let ((face (aref outline-minor-faces
                                  (% (- lvl toplvl)
                                     (length outline-minor-faces))))
                      (ov (make-overlay end (min (1+ end) (point-max)) nil t)))
                  (overlay-put ov 'evaporate t)
                  (overlay-put ov 'backline-heading t)
                  (overlay-put ov 'face face))))))))))

(defun backline--end-of-subtree (pos)
  (save-excursion
    (goto-char pos)
    (condition-case nil
        ;; `elisp-outline-search' (new in Emacs 31) is too slow and since
        ;; we only care about outline headings anyway, of no use here.
        (let ((outline-search-function nil))
          (outline-end-of-subtree))
      ;; When `outline-hide-sublevels' calls `outline-back-to-heading'
      ;; that searches backward from the very beginning of the buffer.
      (outline-before-first-heading (goto-char (point-max))))
    (point)))

;;; _
(provide 'backline)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; backline.el ends here

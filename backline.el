;;; backline.el --- Preserve appearance of outline headings  -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/tarsius/backline
;; Keywords: outlines

;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Outline and Org headings do not extend to the right edge of the
;; window when their bodies are collapsed.  This has unfortunate
;; consequences when the faces used for headings set the background
;; color or another property that is visible on whitespace.  This
;; package adds overlays to extend the appearance of headings all
;; the way to the right window edge.

;; This package supports the major-modes `outline-mode' and `org-mode'
;; and the minor-mode `outline-minor-mode'.

;; Unlike `outline-mode', `outline-minor-mode' by itself does not
;; highlight headings.  You have to install the `outline-minor-faces'
;; for that because it is not a dependency of this packages.

;; Likewise this package only has the desired effect in `org-mode'
;; after you have set `org-fontify-whole-heading-line' to a non-nil
;; value.

;; Unfortunately `outline-mode' doesn't have such a variable, making
;; it necessary to redefine the value of `outline-font-lock-keywords'
;; slightly.

;; Starting with Emacs 27 faces no longer extend to the window edge
;; by default.  Unfortunately the `outline' and `org-mode' faces have
;; not been adjusted accordingly yet, so you get to do that yourself
;; too.

;;   (use-package backline
;;     :after outline
;;     :config
;;     (advice-add 'outline-flag-region :after 'backline-update))

;;   (use-package backline
;;     :after org
;;     :config
;;     (advice-add 'org-flag-region :after 'backline-update)
;;     (add-hook 'org-cycle-hook 'backline-org-cycle-update-children))

;;   (use-package outline-minor-faces
;;     :after outline
;;     :config
;;     (add-hook 'outline-minor-mode-hook
;;               'outline-minor-faces-add-font-lock-keywords))

;;   (use-package org
;;     :defer t
;;     :config
;;     (setq org-fontify-whole-heading-line t)
;;     (when (>= emacs-major-version 27)
;;       (set-face-attribute 'org-level-1 nil :extend t)
;;       (set-face-attribute 'org-level-2 nil :extend t)
;;       (set-face-attribute 'org-level-3 nil :extend t)
;;       (set-face-attribute 'org-level-4 nil :extend t)
;;       (set-face-attribute 'org-level-5 nil :extend t)
;;       (set-face-attribute 'org-level-6 nil :extend t)
;;       (set-face-attribute 'org-level-7 nil :extend t)
;;       (set-face-attribute 'org-level-8 nil :extend t)))

;;   (use-package outline
;;     :defer t
;;     :config
;;     (setq outline-font-lock-keywords
;;           '((eval . (list (concat "^\\(?:" outline-regexp "\\).+\n")
;;                           0 '(outline-font-lock-face) nil t))))
;;     (when (>= emacs-major-version 27)
;;       (set-face-attribute 'outline-1 nil :extend t)
;;       (set-face-attribute 'outline-2 nil :extend t)
;;       (set-face-attribute 'outline-3 nil :extend t)
;;       (set-face-attribute 'outline-4 nil :extend t)
;;       (set-face-attribute 'outline-5 nil :extend t)
;;       (set-face-attribute 'outline-6 nil :extend t)
;;       (set-face-attribute 'outline-7 nil :extend t)
;;       (set-face-attribute 'outline-8 nil :extend t)))

;;; Code:

(require 'outline)

(require 'outline-minor-faces nil t)
(declare-function outline-minor-faces--get-face "outline-minor-faces" ())
(declare-function outline-minor-faces--level "outline-minor-faces" ())

(declare-function org-at-item-p "org-list" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading))
(declare-function org-list-get-item-end-before-blank "org-list" (item struct))
(declare-function org-list-struct "org-list" ())
(defvar org-level-faces)

;;;###autoload
(defun backline-update (from to &rest _ignore)
  "When hidings, add an overlay to extend header's appearance to window edge."
  ;; `outline-hide-sublevels' tries to hide this range, in which case
  ;; `outline-back-to-heading' somehow concludes that point is before
  ;; the first heading causing it to raise an error.  Luckily we don't
  ;; actually have to do anything for that range, so we can just skip
  ;; ahead to the calls that hide the subtrees individually.
  (unless (and (= from   (point-min))
               (= to (1- (point-max))))
    (ignore-errors ; Other instances of "before first heading" error.
      (remove-overlays from
                       (save-excursion
                         (goto-char from)
                         (if (derived-mode-p 'org-mode)
                             (org-end-of-subtree)
                           (outline-end-of-subtree))
                         (1+ (point)))
                       'backline-heading t)
      (dolist (ov (overlays-in (max (1- from) (point-min))
                               (min (1+ to)   (point-max))))
        (when (eq (overlay-get ov 'invisible) 'outline)
          (let ((end (overlay-end ov)))
            (unless (and outline-minor-mode
                         (save-excursion
                           (goto-char end)
                           (outline-back-to-heading)
                           ;; If we depended on `bicycle', then we could use:
                           ;; (bicycle--code-level-p)
                           (= (funcall outline-level)
                              (or (bound-and-true-p outline-code-level) 1000))))
              (let ((o (make-overlay end
                                     (min (1+ end) (point-max))
                                     nil 'front-advance)))
                (overlay-put o 'evaporate t)
                (overlay-put o 'backline-heading t)
                (overlay-put o 'face
                             (save-excursion
                               (goto-char end)
                               (outline-back-to-heading)
                               (cond
                                (outline-minor-mode
                                 (outline-back-to-heading)
                                 (outline-minor-faces--get-face))
                                ((derived-mode-p 'org-mode)
                                 (org-back-to-heading)
                                 (backline--get-face org-level-faces))
                                ((derived-mode-p 'outline-mode)
                                 (outline-back-to-heading)
                                 (backline--get-face outline-font-lock-faces)
                                 ))))))))))))

(defun backline--get-face (faces)
  (save-excursion
    (goto-char (match-beginning 0))
    (nth (1- (% (outline-minor-faces--level)
                (length faces)))
         faces)))

(defun backline-org-cycle-update-children (status)
  ;; We advice `org-flag-region' to run `backline-update'.  We could
  ;; use `org-cycle-hook' instead, but that does not have access to
  ;; all the information that we need. `org-cycle' calls
  ;; `org-flag-region' except when STATUS is `children', which forces
  ;; us to add this wrapper function to `org-cycle-hook'.
  (when (eq status 'children)
    ;; This code was copied from `org-cycle-internal-local', which
    ;; does not pass along `eoh' and `eos', forcing us to calculate
    ;; them again.
    (let ((goal-column 0) eoh eos struct)
      (save-excursion
        (if (org-at-item-p)
	    (progn
	      (beginning-of-line)
	      (setq struct (org-list-struct))
	      (setq eoh (point-at-eol))
	      (setq eos (org-list-get-item-end-before-blank (point) struct)))
	  (org-back-to-heading)
	  (setq eoh (save-excursion (outline-end-of-heading) (point)))
	  (setq eos (save-excursion (org-end-of-subtree t t)
				    (when (bolp) (backward-char)) (point)))))
      (backline-update eoh eos))))

;;; _
(provide 'backline)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; backline.el ends here

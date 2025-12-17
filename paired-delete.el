;;; paired-delete.el --- Delete in pairs mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2025  Thomas Fini Hansen

;; Author: Thomas Fini Hansen <xen@xen.dk>
;; Keywords: convenience
;; Package-Requires: ((emacs "28.1") (smartparens "1.11"))
;; Package-Version: 1.0.0
;; Keywords: convenience
;; URL: https://github.com/xendk/paired-delete.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Companion mode for smartparens to also delete both parts of a pair.
;; Limited to single character pairs.

;;; Code:

(require 'smartparens)

(defvar paired-delete-char-disabled)

(defcustom paired-delete-ignore-modes-list '(minibuffer-inactive-mode
                                             vterm-mode)
  "Modes where `paired-delete-mode' is inactive if enabled globally."
  :type '(repeat symbol)
  :group 'paired-delete)

(defun paired-delete (backwards-p &optional arg)
  "Deletes the matching pair if deleting a pair.

BACKWARDS-P indicates whether we're deleting forwards or backwards and we'll
only work when ARG is 1 or the region is not active."
  (when (and (= arg 1)
             (bound-and-true-p smartparens-mode)
             (not (use-region-p)))
    ;; Set sp-navigate-consider-symbols to only get balanced symbols.
    ;; Else it would consider `//' a pair in Crystal (which it is),
    ;; but sp-unwrap-sexp won't work on it, but rather unwrap the next
    ;; pair it finds.
    (let* ((sp-navigate-consider-symbols nil)
           (ok (sp-get-thing backwards-p)))
      (when ok
        (sp-get ok
          (cond
           ((and backwards-p (or (= (point) :beg-in) (= (point) :end)))
            (sp-backward-unwrap-sexp))
           ((and (not backwards-p) (or (= (point) :beg) (= (point) :end-in)))
            (sp-unwrap-sexp))
           (t nil)))))))

(defun paired-delete-delete-char-advice (orig-fun n &optional kill-flag)
  "Advice for delete char.

ORIG-FUN is the overridden function. Passes N and KILL-FLAG to original."
  (if (and paired-delete-mode
           (not (bound-and-true-p paired-delete-char-disabled)))
      (let ((paired-delete-char-disabled t))
        (save-match-data
          (if (not (paired-delete (> 0 n) (abs n)))
              (funcall orig-fun n kill-flag))))
    (funcall orig-fun n kill-flag)))

(defun paired-delete-disable-advice (orig-fun &rest args)
  "Advice to disable paired delete for some functions.  Call ORIG-FUN with ARGS."
  (let ((paired-delete-char-disabled t))
    (apply orig-fun args)))

(put 'paired-delete 'delete-selection 'supersede)

;;;###autoload
(define-minor-mode paired-delete-mode
  "Toggle paired delete mode."
  :init-value nil
  (if paired-delete-mode
      (progn
        (advice-add 'delete-char :around #'paired-delete-delete-char-advice)
        (advice-add 'sp-insert-pair :around #'paired-delete-disable-advice)
        (advice-add 'sp-skip-closing-pair :around #'paired-delete-disable-advice))
    (advice-remove 'delete-char #'paired-delete-delete-char-advice)
    (advice-remove 'sp-insert-pair #'paired-delete-disable-advice)
    (advice-remove 'sp-skip-closing-pair #'paired-delete-disable-advice)))

;;;###autoload
(defun paired-delete-mode-maybe ()
  "Turn on `paired-delete-mode'.

This function is used to turn on `global-paired-delete-mode'.

By default `global-paired-delete-mode' ignores buffers with
`mode-class' set to special, but only if they are also not comint
buffers.

Additionally, buffers on `paired-delete-ignore-modes-list' are ignored.

You can still turn on paired-delete-mode in these mode manually (or
in mode's startup-hook etc.) by calling `paired-delete-mode'."
  (interactive)
  (unless (or (member major-mode paired-delete-ignore-modes-list)
              (and (not (derived-mode-p 'comint-mode))
                   (eq (get major-mode 'mode-class) 'special)))
    (paired-delete-mode t)))

;;;###autoload
(define-globalized-minor-mode global-paired-delete-mode
  paired-delete-mode
  paired-delete-mode-maybe
  :group 'paired-delete)

(provide 'paired-delete)
;;; paired-delete.el ends here

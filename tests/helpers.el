;;; -*- lexical-binding: t; -*-

(require 'buttercup)

(defvar point-placeholder "\002")
(defvar mark-placeholder "\003")

;; Thanks to Fuco1 for much inspiration and outright copying of.
;; See
;; https://github.com/Fuco1/.emacs.d/blob/master/tests/my-test-helper.el
(defmacro with-buffer-content (initial initform &rest forms)
  "Setup a new buffer, then run FORMS.

First, INITFORM are run in the newly created buffer.

Then INITIAL is inserted (it is expected to evaluate to string). If
INITIAL contains `point-placeholder' put point there as the initial
position (the character is then removed). If it contains
`mark-placeholder', put mark there (the character is then removed).

Finally, FORMS are run."
  (declare (indent 2)
           (debug (form form body)))
  `(save-window-excursion
     (let ((case-fold-search nil))
       (with-temp-buffer
         (set-window-buffer (selected-window) (current-buffer))
         (set-input-method nil)
         ,initform
         (insert ,initial)
         (goto-char (point-min))
         (when (search-forward mark-placeholder nil t)
           (delete-char -1)
           (set-mark (point))
           (activate-mark))
         (goto-char (point-min))
         (when (search-forward point-placeholder nil t)
           (delete-char -1))
         ,@forms))))

(defmacro with-fundemental-buffer-content (initial &rest forms)
  "Setup a fundemetal-mode buffer.

See `with-buffer-content'."
  (declare (indent 1)
           (debug (form body)))
  `(with-buffer-content ,initial
                        ;; If we don't explicitly set a mode,
                        ;; globalized minor modes aren't triggered.
                        (fundamental-mode)
                        ,@forms))

(buttercup-define-matcher :to-have-contents (buffer expected)
  "Check that BUFFER matches EXPECTED.

If EXPECTED contains `point-placeholder'/`mark-placeholder', check that
point/mark is at the specified position."
  (with-current-buffer (funcall buffer)
    (let* ((contents (buffer-string))
           (expected-raw (funcall expected))
           (expected (replace-regexp-in-string (concat "[" point-placeholder "|" mark-placeholder "]") "" expected-raw))
           (buffer-matches (equal expected contents))
           (point-pos (string-match-p
                       point-placeholder (string-replace mark-placeholder "" expected-raw)))
           (mark-pos (string-match-p
                      mark-placeholder (string-replace point-placeholder "" expected-raw))))
      (cond
       ((and point-pos (not (equal (1+ point-pos) (point))))
        (cons nil (format "Expected point to be at %d, but it was at %d" (1+ point-pos) (point))))
       ((and mark-pos (not (equal (1+ mark-pos) (mark))))
        (cons nil (format "Expected mark to be at %d, but it was at %d" (1+ mark-pos) (mark))))
       ((not buffer-matches)
        (cons nil (format "Expected buffer to match \"%s\", but it was \"%s\"." expected contents)))
       (t
        (cons t (format "Expected buffer to not match \"%s\", but it did." expected)))))))

(provide 'tests/helpers)
;;; helpers.el ends here

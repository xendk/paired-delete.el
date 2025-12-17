;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'tests/helpers)
(require 'paired-delete)

(describe "paired-delete"
  (before-each
    (smartparens-global-mode 1)
    (global-paired-delete-mode 1))

  (it "deletes simple pairs"
    (with-fundemental-buffer-content
        (concat "some(" point-placeholder ")text")
      (delete-char -1)
      (expect (current-buffer) :to-have-contents "sometext")))

  (it "deletes pairs, but not their content"
    (with-fundemental-buffer-content
        (concat "some(" point-placeholder "banana)text")
      (delete-char -1)
      (expect (current-buffer) :to-have-contents "somebananatext")))

  (it "deletes pairs forwards"
    (with-fundemental-buffer-content
        (concat "some" point-placeholder "(banana)text")
      (delete-char 1)
      (expect (current-buffer) :to-have-contents "somebananatext"))))

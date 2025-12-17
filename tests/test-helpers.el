;;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'tests/helpers)

(describe "with-buffer-content"
  (it "provides a buffer with the given content"
    (with-buffer-content "some content" ()
      (expect (buffer-string) :to-equal "some content")))

  (it "places point and mark"
    (with-buffer-content "so\002me cont\003ent" ()
      (expect (buffer-string) :to-equal "some content")
      (expect (point) :to-equal 3)
      (expect (mark) :to-equal 10))))

(describe ":to-have-contents"
  (it "matches buffer contents"
    (with-buffer-content "some content" ()
      (expect (current-buffer) :to-have-contents "some content")
      (expect (current-buffer) :not :to-have-contents "banana")))
  (it "matches point and mark"
    (with-buffer-content "so\002me cont\003ent" ()
      (expect (current-buffer) :to-have-contents "so\002me cont\003ent")
      (expect (current-buffer) :not :to-have-contents "s\002ome cont\003ent")
      (expect (current-buffer) :not :to-have-contents "so\002me conte\003nt"))))

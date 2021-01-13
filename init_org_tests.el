;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

(setq test '(("hello" . "howdy")
             ("w of m" . "words_of_mormon")
             ("fart" . "face")))

(setq test2 (list "hi" "hey" test))

(defun alist-keys (alist)
  (mapcar 'car alist))

(alist-keys test)

(setq test "https://www.churchofjesuschrist.org/study/scriptures/%s/%s/%s?lang=eng")

(format test "hi" "howdy" "fart")

(assoc "w of m" test)
(type-of test)
(mapcar 'car test)
(type-of (nth 2 test2))
(mapcar 'car (nth 2 test2))
(mapcar 'car test)
(type-of (intern-soft (nth 2 test2)))
(type-of (intern "test"))

(let ((m (nth 2 test2)))
  (type-of m))

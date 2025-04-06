
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Thing-Edit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(one-key-create-menu
 "THING-EDIT"
 '(
   ;; Copy.
   (("w" . "Copy Word") . thing-copy-word)
   (("s" . "Copy Symbol") . thing-copy-symbol)
   (("m" . "Copy Email") . thing-copy-email)
   (("f" . "Copy Filename") . thing-copy-filename)
   (("u" . "Copy URL") . thing-copy-url)
   (("x" . "Copy Sexp") . thing-copy-sexp)
   (("g" . "Copy Page") . thing-copy-page)
   (("t" . "Copy Sentence") . thing-copy-sentence)
   (("o" . "Copy Whitespace") . thing-copy-whitespace)
   (("i" . "Copy List") . thing-copy-list)
   (("c" . "Copy Comment") . thing-copy-comment)
   (("h" . "Copy Function") . thing-copy-defun)
   (("p" . "Copy Parentheses") . thing-copy-parentheses)
   (("l" . "Copy Line") . thing-copy-line)
   (("a" . "Copy To Line Begin") . thing-copy-to-line-beginning)
   (("e" . "Copy To Line End") . thing-copy-to-line-end)
   ;; Cut.
   (("W" . "Cut Word") . thing-cut-word)
   (("S" . "Cut Symbol") . thing-cut-symbol)
   (("M" . "Cut Email") . thing-cut-email)
   (("F" . "Cut Filename") . thing-cut-filename)
   (("U" . "Cut URL") . thing-cut-url)
   (("X" . "Cut Sexp") . thing-cut-sexp)
   (("G" . "Cut Page") . thing-cut-page)
   (("T" . "Cut Sentence") . thing-cut-sentence)
   (("O" . "Cut Whitespace") . thing-cut-whitespace)
   (("I" . "Cut List") . thing-cut-list)
   (("C" . "Cut Comment") . thing-cut-comment)
   (("H" . "Cut Function") . thing-cut-defun)
   (("P" . "Cut Parentheses") . thing-cut-parentheses)
   (("L" . "Cut Line") . thing-cut-line)
   (("A" . "Cut To Line Begin") . thing-cut-to-line-beginning)
   (("E" . "Cut To Line End") . thing-cut-to-line-end)
   )
 t)

(provide 'init-thing-edit)

;;; init-thing-edit.el ends here

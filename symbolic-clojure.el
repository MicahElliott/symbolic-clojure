;;; symbolic-clojure.el --- Turn some clojure builtins into symbols. -*- lexical-binding: t -*-

;; Copyright (C) 2022 Micah Elliott

;; Author: Micah Elliott <mde@micahelliott.com
;; URL: https://github.com/MicahElliott/symbolic-clojure
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: symbols, clojure

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package will turn some clojure builtins into symbols. It was
;; inspired by this post by abo-abo:
;; https://endlessparentheses.com/using-prettify-symbols-in-clojure-and-elisp-without-breaking-indentation.html
;; but took it to extremes.
;;
;;; Code:

;; (require 'clojure-mode)

(make-variable-buffer-local
 (defvar foo-count 0
   "Number of foos inserted into the current buffer."))

(defun insert-foo ()
  (interactive)
  (setq foo-count (1+ foo-count))
  (insert "foo"))

;;;###autoload
(define-minor-mode symbolic-clojure-mode
  "Turn some clojure builtins into symbols."
  :lighter " foo"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c f") 'insert-foo)
            map))


;; Pretty/fancy glyphs for code
;; https://endlessparentheses.com/using-prettify-symbols-in-clojure-and-elisp-without-breaking-indentation.html
;; FIXME Maybe has to be set before global mode turned on?
;; https://stackoverflow.com/questions/18172728/the-difference-between-setq-and-setq-default-in-emacs-lisp
;; (add-to-list 'endless/clojure-prettify-alist '(">=" . (?\s (Br . Bl) ?\s (Bc . Bc) ?≥)))
;;;###autoload
(setq-default clojure-prettify-alist
	      '(("fn" . (?\s (Br . Bl) ?\s (Bc . Bc) ?λ))
		("#(" . (?ƒ (Br . Bl) ?\( ))
		;; ("if" . (?\s (Br . Bl) ?\s (Bc . Bc) ?⊃))
		("if" . (?⊃ (Br . Bl) ?⊃))
		("ns" . (?\s (Br . Bl) ?\s (Bc . Bc) ?§))
                ("=" .  ?≡)
		("/" .  ?÷)
		("*" .  ?×)
		("\"" .  ?»)
                ;; (";;" . ?∥)
		(";; " . (?\s (Br . Bl) ?\s (Bc . Bc) ?|))
		;; ("#\"" . (?\s (Br . Bl) ?\s (Bc . Bc) ?®))
		(";;;" . (?\s (Br . Bl) ?\s (Bc . Bc) ?¶))
		("---" . (?\s (Br . Bl) ?\s (Bc . Bc) ?λ))
		;; ("" . ?|)
		("###" . ?|)
		;; (";;;" . "||")
		("#_>" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br) #x21a6 (Bc . Bl) #x21a6))
		("->" . (?- (Br . Bc) ?- (Br . Bc) ?>))
		("->>" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
			       (Bl . Bl) ?- (Bc . Br) ?- (Bc . Bc) ?>
			       (Bc . Bl) ?- (Br . Br) ?>))
		("or" . (?\s (Br . Bl) ?\s (Bc . Bc) ?\∥))
		;; (":require" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?✱ (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s))
		;; (":require" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?ɼ (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s))
		;; (":require" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?Ʀ Ȑ Я ⁜  ⅀ ⅅ (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s))
		(":require" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?ℛ (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s))
		;; (":import" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?ἵ (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s))
		(":import" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?ℐ (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s))
		("do" . (?\s (Br . Bl) ?\s (Bc . Bc) ?⊨))
		;; ("do" . (?\s (Br . Bl) ?\s (Bc . Bc) ?⥽))
		;; ("and" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br) ?\∧ (Bc . Bl) ?\∧))
		("and" .   (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?∧))
		;; (":as" .   (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?ä))
		;; symbols for try/catch/throw ₮ ₡ ⨂ ⌁ ε ⎋ ⎊ ⏏
		(":as" .   (?~ (Br . Bl) ?\s (Br . Bl) ?\s))
		("for" .   (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?∀))
		("not" .   (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?¬))
		;; ("let" .   (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?∃))
		("let" .   (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?ⅅ))
		;; ("nil" .   (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br) ?∅))
		("nil" .   (?∅ (Br . Bl) ?\s (Br . Bl) ?\s))
		("def" .   (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br) ?≔))
		;; ("try" .   (?¿ (Br . Bl) ?T (Br . Bl) ?\s))
		("try" .   (?\s  (Br . Bl) ?\s  (Br . Bl) ?\s (Bc . Br) ?¿ (Bc . Bl) ?T))
                ("not=" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?\≢))
		("defn" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br) ?λ (Bc . Bl) ?≔))
		;; ("when" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br) ?⊃ (Bc . Bl) ?⊃))
		("when" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br)  ?⊃ (Bc . Bl) ?\s))
		("comp" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br)  ?∘ (Bc . Bl) ?\s))
		("juxt" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br)  ?◫ (Bc . Bl) ?\s))
                ;; ⁁ ƒ ∘ ⬗ ◫ ⧑ ⌨ ◧ ⚠ ₪
		;; ("loop" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br)  ?∞ (Bc . Bl) ?\s))
		;; ("loop" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?∞ (Br . Bl)  ?∞ (Bc . Bl) ?\s))
		("loop" .  (?\s (Br . Bl) ?∞ (Br . Bl)  ?∞ (Br . Bl) ?\s (Br . Bl) ?\s))
		("cond" .  (?⊃ (Br . Bl)  ?⊃ (Br . Bl) ?⊃ (Br . Bl) ?\s))
		(":else" .  (?Ω  (Br . Bl)  ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s))
		;; ("true" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?⊤ (Bc . Bl) ?⊤))
		("true" .  (?⊤  (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s))
		;; ("nil?" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br) ?∅ (Bc . Bl) ??))
		("nil?" .  (?¿ (Br . Bl) ?∅  (Br . Bl) ?? (Br . Bl) ?\s))
		;; ("catch" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br) ?≔))
		("false" . (?⊥  (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s))
		;; ("catch" . (?¿  (Br . Bl) ?C (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s))
		("catch" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br) ?\s (Bc . Br) ?¿  (Bc . Bl) ?C (Br . Bl)  ?\s))
		("count" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br) ?\s (Bc . Br)  ?⍴ (Br . Bl)  ?\s))
		("throw" . (?¿  (Br . Bl) ?⊗ (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s)) ; think "raise"
		("recur" . (?\s (Br . Bl) ?↻  (Br . Bl) ?↻ (Br . Bl) ?\s (Br . Bl) ?\s))
		("doseq" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br) ?⊨ (Bc . Bl) ?∀  (Br . Bl) ?\s))
                ("true?" . (?¿ (Br . Bl) ?⊤  (Bc . Br) ??  (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s))
                ("defn-" . (?— (Br . Bl) ?λ (Br . Bl) ?≔ (Br . Bl) ?—  (Br . Bl) ?\s))
		("false?" . (?¿ (Br . Bl) ?⊥ (Br . Bl) ?? (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s))
		(":refer" . (?※ (Br . Bl)  ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s))
		("if-not" . (?\s (Br . Bl) ?\s (Br . Bl) ?⊃ (Bc . Bc) ?⊃ (Br . Bl) ?¬  (Br . Bl) ?\s (Br . Bl) ?\s)) ; tricky!!
		("if-let" . (?\s (Br . Bl) ?\s (Br . Bl) ?⊃ (Bc . Bc) ?⊃ (Br . Bl) ?ⅅ  (Br . Bl) ?\s (Br . Bl) ?\s))
		("when-not" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br) ?⊃ (Bc . Bl) ?¬))
		("when-let" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br) ?⊃ (Bc . Bl) ?ⅅ))))

(prettify-symbols-mode +1)

(eval-after-load 'clojure-mode '(setq clojure--prettify-symbols-alist (append clojure-prettify-alist clojure--prettify-symbols-alist)))


;;;###autoload
(add-hook 'clojure-mode-hook 'symbolic-clojure-mode)

(provide 'symbolic-clojure)
;;; symbolic-clojure.el ends here

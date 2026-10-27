;;; init-markdown.el --- writting                    -*- lexical-binding: t; -*-

;; Copyright (C) 2026  fly_lilee

;; Author: fly_lilee <liyl2015@mail.ustc.edu.cn>
;; Keywords: docs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

;;; markdown
;; Ref: https://github.com/Thysrael/emacs.d

(autoload 'markdown-ts-mode "markdown-ts-mode" nil t)

(add-hook 'markdown-ts-mode-hook #'markdown-indent-mode)

(setopt markdown-ts-appear-heading-bullets '(?◉ ?○ ?✸ ?✿)
        markdown-ts-appear-table-reveal 'table
        markdown-ts-appear-enable-math-preview t
        latex-to-svg-frontend-center-display-math t
        markdown-ts-appear-diagram-renderers '(("mermaid" . markdown-ts-appear-diagram-mmdc)))

(setopt markdown-ts-inline-images t
        markdown-ts-table-auto-align t
        markdown-ts-unchecked-checkbox '("󰄱" . "□")
        markdown-ts-checked-checkbox '("󰄲" . "■"))

(with-eval-after-load 'markdown-ts-mode
  (require 'markdown-ts-mode-x)

  (defun my/markdown-ts-editing-setup ()
    "Enable personal Markdown editing helpers in the current buffer."
    (add-hook 'after-change-functions #'my/markdown-ts-refontify-fence nil t)
    (add-hook 'post-self-insert-hook #'my/markdown-ts-expand-code-fence nil t))

  (defun my/markdown-ts-refontify-fence (beg end _old-length)
    "Refontify from BEG after editing a Markdown code fence through END."
    (let ((line-beg (save-excursion
                      (goto-char beg)
                      (line-beginning-position)))
          (line-end (save-excursion
                      (goto-char end)
                      (line-end-position))))
      (when (save-excursion
              (goto-char line-beg)
              (re-search-forward
               "^[ \t]*\\(?:`\\{3,\\}\\|~\\{3,\\}\\)" line-end t))
        (font-lock-flush line-beg (point-max))
        (font-lock-ensure line-beg (point-max)))))

  (defun my/markdown-ts-expand-code-fence ()
    "Prompt for a language and complete a newly typed code fence."
    (when (and (eq last-command-event ?`)
               (eolp)
               (equal (buffer-substring-no-properties
                       (line-beginning-position) (point))
                      "```"))
      (delete-region (line-beginning-position) (point))
      (condition-case nil
          (let ((language
                 (completing-read "Language: "
                                  (my/markdown-ts-code-languages)
                                  nil nil nil
                                  'markdown-ts-language-history)))
            (insert "```" language "\n\n```")
            (forward-line -1))
        (quit (insert "```")))))

  (defun my/markdown-ts-code-languages ()
    "Return known language names for Markdown fenced code blocks."
    (let (languages)
      (dolist (mapping (append (and (boundp 'markdown-ts-code-block-modes)
                                    markdown-ts-code-block-modes)
                               (and (boundp 'markdown-code-lang-modes)
                                    markdown-code-lang-modes)))
        (push (format "%s" (car mapping)) languages))
      (dolist (mapping auto-mode-alist)
        (when (and (symbolp (cdr mapping))
                   (string-suffix-p "-ts-mode" (symbol-name (cdr mapping))))
          (push (string-remove-suffix "-ts-mode" (symbol-name (cdr mapping)))
                languages)))
      (dolist (mapping major-mode-remap-alist)
        (dolist (mode (list (car mapping) (cdr mapping)))
          (when (symbolp mode)
            (push (string-remove-suffix
                   "-ts" (string-remove-suffix "-mode" (symbol-name mode)))
                  languages))))
      (sort (delete-dups languages) #'string<)))


  (defun my/markdown-ts-newline ()
    "Continue a plain ordered or unordered Markdown list."
    (interactive)
    (font-lock-ensure (line-beginning-position) (line-end-position))
    (if (or (markdown-ts-at-code-block-p)
            (not (save-excursion
                   (goto-char (line-beginning-position))
                   (re-search-forward my/markdown-list-item-regexp
                                      (line-end-position) t))))
        (markdown-ts-newline)
      (let ((prefix (match-string-no-properties 1))
            (marker (match-string-no-properties 2))
            (number (match-string-no-properties 3))
            (delimiter (match-string-no-properties 4))
            (content (match-string-no-properties 5)))
        (if (string-empty-p (string-trim content))
            (delete-region (line-beginning-position) (line-end-position))
          (newline)
          (insert prefix
                  (if number
                      (concat (number-to-string (1+ (string-to-number number)))
                              delimiter)
                    marker)
                  " ")))))

  (defun my/markdown-ts-limit-image-height (source &rest _)
    "Limit previews for SOURCE to eight character heights without a width cap."
    (dolist (view (overlay-get source 'markdown-ts-appear-image--views))
      (let* ((text (overlay-get view 'after-string))
             (position (text-property-not-all 0 (length text) 'display nil text))
             (image (get-text-property position 'display text))
             (height '(8 . ch)))
        (unless (and (not (plist-member (cdr image) :max-width))
                     (equal (image-property image :max-height) height))
          (setq text (copy-sequence text)
                image (copy-sequence image))
          (setf (image-property image :max-width) nil
                (image-property image :max-height) height)
          (put-text-property position (1+ position) 'display image text)
          (overlay-put view 'after-string text)))))

  (defvar my/markdown-image-directory-history nil
    "History of directories used for yanked Markdown images.")

  (defvar my/markdown-image-basename-history nil
    "History of basenames used for yanked Markdown images.")

  (defvar-local my/markdown-image-default-directory nil
    "Default image directory, relative to the current Markdown file.")

  (defconst my/markdown-list-item-regexp
    "^\\([ \t]*\\(?:>[ \t]*\\)*\\)\\([-+*]\\|\\([0-9]+\\)\\([.)]\\)\\)[ \t]+\\(.*\\)$"
    "Regexp matching a plain Markdown list item.")

  (put 'my/markdown-image-default-directory 'safe-local-variable
       #'string-or-null-p)

  (defun my/markdown-ts-yank-image (mimetype data)
    "Save image DATA of MIMETYPE and insert a Markdown image link.
Convert TIFF images to PNG for Markdown renderer compatibility."
    (unless buffer-file-name
      (user-error "Save the Markdown buffer before yanking an image"))
    (let* ((base-directory (file-name-directory buffer-file-name))
           (default-directory-name
            (or my/markdown-image-default-directory
                (file-name-base buffer-file-name)))
           (directory-name
            (read-string "Image directory: " default-directory-name
                         'my/markdown-image-directory-history))
           (directory (expand-file-name directory-name base-directory))
           (extension
            (if (eq mimetype 'image/tiff)
                "png"
              (symbol-name (mailcap-mime-type-to-extension mimetype))))
           (basename
            (read-string "Image basename: " nil
                         'my/markdown-image-basename-history))
           (filename (concat basename "." extension))
           (path (expand-file-name filename directory))
           (relative-path (file-relative-name path base-directory)))
      (when (or (string-empty-p basename)
                (member basename '("." ".."))
                (not (equal basename (file-name-nondirectory basename))))
        (user-error "Image basename must be a non-empty file basename"))
      (when (file-exists-p path)
        (user-error "Image already exists: %s" path))

      (when (eq mimetype 'image/tiff)
        (require 'image-converter)
        (let ((image-convert-to-format "png"))
          (setq data (image-convert data mimetype))))
      (make-directory directory t)
      (let ((coding-system-for-write 'emacs-internal))
        (with-temp-file path
          (insert data)))
      (insert "!["
              (string-replace "]" "\\]" (string-replace "\\" "\\\\" basename))
              "]" "("
              (replace-regexp-in-string
               (rx (or control (any " %?#()<>\"\\[]")))
               #'url-hexify-string relative-path t t)
              ")")))

  (defun my/markdown-ts-yank-media-setup ()
    "Register the Markdown clipboard image handler."
    (require 'mailcap)
    (require 'url-util)
    (require 'yank-media)
    (setq-local yank-media-preferred-types
                (append yank-media-preferred-types
                        (unless (memq 'image/tiff yank-media-preferred-types)
                          '(image/tiff))))
    (yank-media-handler "image/.*" #'my/markdown-ts-yank-image))


  (pretty-transient-define-prefix my/markdown-ts-insert-<structure ()
    "Transient markdown block structure menu.

Insert a block structure.  If there is an active region, wrap it;
otherwise insert an empty block and place point inside.

  `  fenced code block (back-tick)
  ~  fenced code block (tilde)
  q  block quote
  d  divider/rule
  t  table
  <  insert <"
    [["Code"
      ("`" "Back-tick code" (markdown-ts--insert-code-block ?`))
      ("~" "Tilde code" (markdown-ts--insert-code-block ?~))]
     ["Block"
      ("q" "Quote" (markdown-ts--insert-block-quote))
      ("d" "Divider" (markdown-ts--insert-divider))
      ("t" "Table" (markdown-ts-table-insert-table))]
     ["Misc"
      ("<" "Insert <" (insert "<"))]]
    [("q" "Quit" transient-quit-one)])

  (pretty-transient-define-prefix my/markdown-ts-insert->structure ()
    "Transient markdown emphasis menu.

Emphasize text with markers.  If there is an active region, wrap it;
otherwise insert marker pairs and place point between.

  = verbatim  ~ code  + strikethrough
  _ underline  / italic  a bold + italic
  e emphasize  > insert >"
    [
     ["Emphasize"
      ("=" "Verbatim" (markdown-ts-emphasize ?c))
      ("~" "Code" (markdown-ts-emphasize ?c))
      ("+" "Delete" (markdown-ts-emphasize ?s))
      ("_" "Underline" (markdown-ts-emphasize ?I))
      ("/" "Italic" (markdown-ts-emphasize ?i))
      ("a" "Bold & Italic" (markdown-ts-emphasize ?a))
      ("e" "Emphasize" markdown-ts-emphasize)]
     ["Misc"
      (">" "Insert >" (insert ">"))]]
    [("q" "Quit" transient-quit-one)])

  (keymap-binds markdown-ts-mode-map
    ("C-c C-e" . markdown-ts-convert)
    ("RET" . my/markdown-ts-newline)
    ("<return>" . my/markdown-ts-newline)
    ("<kp-enter>" . my/markdown-ts-newline)
    (("S-RET" "S-<return>") . markdown-ts-insert-list-item)
    (("C-s-y" "C-M-y") . yank-media)
    ("<" . (lambda ()
             "Insert markdown template."
             (interactive)
             (if (or (region-active-p) (looking-back "^\s*" (line-beginning-position)))
                 (call-interactively #'my/markdown-ts-insert-<structure)
               (self-insert-command 1))))
    (">" . my/markdown-ts-insert->structure))

  (advice-add 'markdown-ts-appear-image--sync-views :after
              #'my/markdown-ts-limit-image-height)

  (add-hook 'markdown-ts-mode-hook #'my/markdown-ts-editing-setup)
  (add-hook 'markdown-ts-mode-hook #'my/markdown-ts-yank-media-setup)
  (add-hook 'markdown-ts-mode-hook #'markdown-ts-appear-mode))

(provide 'init-markdown)
;;; init-markdown.el ends here

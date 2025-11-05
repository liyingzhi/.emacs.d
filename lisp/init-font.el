;;; init-font.el --- font                            -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst user/default-mac-font-size 230
  "The default font size in mac.")

(defconst user/default-win-font-size 110
  "The default font size in windows.")

(defconst user/default-linux-font-size 190
  "The default font size in linux.")

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun set-face-like-default (face)
  "Set FACE attributes to match the default face."
  (set-face-attribute face nil
                      :family (face-attribute 'default :family)
                      :height (face-attribute 'default :height)
                      :weight (face-attribute 'default :weight)
                      :slant (face-attribute 'default :slant)))

(defun setup-fonts (&optional font-size)
  "Setup fonts.
FONT-SIZE is the default font size."

  ;; Setting the default


  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("MonoLisa Lucius" "Jetbrains Mono" "Source Code Pro" "PragmataPro Mono Liga"
                           "Aporetic Sans Mono" "Aporetic Sans" "Cascadia Code" "Fira Code"
                           "SF Mono" "Hack" "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        ;; :slant 'italic
                                        ;; :weight 'medium
                                        :height (if font-size
                                                    font-size
                                                  user/font-size)))
    (set-face-like-default 'fixed-pitch-serif)
    (set-face-like-default 'variable-pitch)

    ;; Set mode-line font
    ;; (cl-loop for font in '("Menlo" "SF Pro Display" "Helvetica")
    ;;          when (font-installed-p font)
    ;;          return (progn
    ;;                   (set-face-attribute 'mode-line nil :family font :height 120)
    ;;                   (when (facep 'mode-line-active)
    ;;                     (set-face-attribute 'mode-line-active nil :family font :height 120))
    ;;                   (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    ;; Specify font for all unicode characters
    ;; https://www.wfonts.com/font/symbola
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (if (< emacs-major-version 27)
                        (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; "Emacs 28 now has 'emoji . before, emoji is part of 'symbol"
    ;; 根据上面这句话应该写成 'emoji 就可以了，但是由于 Emoji 本身
    ;; 分布比较散，所以还是先设置 'unicode 后再设置 CJK 比较靠谱。
    ;; 特例：'emoji 就会导致 ⛈️ fallback 到 ⛈
    ;; https://emacs-china.org/t/emacs/15676/34

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Noto Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (cond
                     ((< emacs-major-version 27)
                      (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
                     ((< emacs-major-version 28)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
                     (t
                      (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("LXGW WenKai" "LXGW Neo Xihei" "WenQuanYi Zen Hei" "PingFang SC" "Microsoft Yahei" "STFangsong")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.2)))
                      (dolist (charset '(kana han symbol cjk-misc bopomofo))
                        (set-fontset-font t charset
                                          (font-spec :family font)))))

    ;; Setting fall-back fonts
    ;; https://idiocy.org/emacs-fonts-and-fontsets.html
    (dolist (font '("Jigmo" "Jigmo2" "Jigmo3"))
      (when (member font (font-family-list))
        (set-fontset-font "fontset-default" 'han font nil 'append)))

    ;; Force Emacs to search by using font-spec
    (set-fontset-font t 'han (font-spec :script 'han) nil 'append)

    (when sys/linuxp
      ;; Set character composition rule for U+FE0F on Linux 2025-10-31
      ;;
      ;; Background:
      ;; On some Linux systems, U+FE0F (VARIATION SELECTOR-16) may display as a box
      ;; instead of the expected variant display (such as a colored emoji). This is
      ;; due to the lack of proper character composition rules.
      ;;
      ;; Solution:
      ;; Use the `set-char-table-range` function to set a composition rule for U+FE0F
      ;; in the `composition-function-table`. This ensures it combines correctly with
      ;; preceding characters to display as the intended variant.
      ;;
      ;; To avoid unnecessary settings on non-Linux systems, the `when` conditional
      ;; is used to apply this rule only in a Linux environment.
      ;; https://t.me/emacs_china/297476
      (set-char-table-range composition-function-table #xFE0F '(["\\c.\\c^+" 1 compose-gstring-for-graphic])))

    ;; Some characters are not being covered, so this workaround is used. 2025-04-07
    ;; https://github.com/ryanoasis/nerd-fonts/wiki/Glyph-Sets-and-Code-Points
    (let ((ranges '((#xE5FA . #xE6B7)    ;; Seti-UI + Custom
                    (#xE700 . #xE8EF)    ;; Devicons
                    (#xED00 . #xF2FF)    ;; Font Awesome
                    (#xE200 . #xE2A9)    ;; Font Awesome Extension
                    (#xF0001 . #xF1AF0)  ;; Material Design Icons
                    (#xE300 . #xE3E3)    ;; Weather
                    (#xF400 . #xF533)    ;; Octicons
                    (#x2665 . #x2665)    ;; Octicons
                    (#x26A1 . #x26A1)    ;; Octicons
                    (#xE000 . #xE00A)    ;; Pomicons
                    (#xEA60 . #xEC1E)))) ;; Codicons
      (dolist (range ranges)
        (set-fontset-font t range "Symbols Nerd Font Mono")))))

;;; setup default font
(add-hook 'window-setup-hook #'setup-fonts)
(add-hook 'server-after-make-frame-hook #'setup-fonts)

(defun set-font-size (symbol value)
  "Set font SYMBOL VALUE."
  (set-default-toplevel-value symbol value)
  (setup-fonts value))

(defcustom user/font-size (cond (sys/macp user/default-mac-font-size)
                                (sys/win32p user/default-win-font-size)
                                (t user/default-linux-font-size))
  "The font size."
  :group 'user
  :type 'number
  :set #'set-font-size)

(defcustom user/ligature nil
  "Is use ligature."
  :group 'user
  :type 'boolean)

(defun set-buffer-font (font face-name)
  "Set the current buffer's font to FONT using FACE-NAME.
If FONT is nil, use the default face entirely.
If FONT is a string, use it as the font family
while preserving other default attributes."
  (unless (facep face-name)
    (make-face face-name))
  (if (null font)
      ;;Inheritance default face
      (copy-face 'default face-name)
    ;;Replace fonts only, keeping other attributes unchanged.
    (progn
      (copy-face 'default face-name)
      (set-face-attribute face-name nil :font font)))
  (setq-local buffer-face-mode-face face-name)
  (buffer-face-mode))

(defun set-font-for-modes (font-alist)
  "Set fonts for different modes based on FONT-ALIST."
  (dolist (entry font-alist)
    (let ((mode (car entry))
          (font (cdr entry)))
      (add-hook (intern (format "%s-hook" mode))
                (lambda ()
                  (let ((face-name (intern (format "%s-font-face" mode))))
                    (set-buffer-font font face-name)))))))

;;; setup buffer-specified font
;; (defconst *fallback-fonts* '("Jigmo" "Jigmo2" "Jigmo3"))
;; (defconst *font-size* 15)
;; (defconst *default-font* (format "MonoLisa Lucius %d" *font-size*))
;; (defconst *org-font* (format "Aporetic Serif Mono %d" *font-size*))
;; (defconst *term-default-font* (format "Aporetic Serif Mono %d" *font-size*))
;; (defconst *prog-font* (format "Aporetic Serif Mono %d" *font-size*))
;; (defconst *zh-default-font* "LXGW WenKai")
;; (defconst *nerd-icons-font* "Symbols Nerd Font Mono")
;; (defconst *emoji-fonts* '("Apple Color Emoji"
;;                           "Noto Color Emoji"
;;                           "Noto Emoji"
;;                           "Segoe UI Emoji"))
;; (defconst *symbol-font* '("Apple Symbols"
;;                           "Segoe UI Symbol"
;;                           "Symbola"
;;                           "Symbol"))

;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (set-font-for-modes
;;              `((vterm-mode . ,*term-default-font*)
;;                (nxml-mode  . ,*prog-font*)
;;                (org-mode   . ,*org-font*)
;;                (latex-mode . ,*prog-font*)
;;                (prog-mode  . ,*prog-font*)))))

(add-hook 'after-init-hook
          (lambda ()
            (set-font-for-modes
             `((vterm-mode . ,user/*term-default-font*)))))

(defun +suggest-other-faces (func &rest args)
  "Temporarily disable `global-hl-line-mode' while executing FUNC with ARGS."
  (let ((was-hl-line-mode-enabled global-hl-line-mode))
    (when was-hl-line-mode-enabled
      (global-hl-line-mode -1))
    (unwind-protect
        (apply func args)
      (when was-hl-line-mode-enabled
        (global-hl-line-mode 1)))))

(advice-add 'face-at-point :around #'+suggest-other-faces)


;;; 连体字体
(with-eval-after-load 'ligature
  (ligature-set-ligatures 't '("www"))
  ;; Enable the www ligature in every possible major mode
  (ligature-set-ligatures 'prog-mode
                          '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                            ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                            "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                            "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                            "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                            "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                            "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                            "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                            "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                            "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%")))

(when user/ligature
  (add-hook 'after-init-hook
            #'global-ligature-mode))

;;; 替换符号
(add-hooks '(emacs-lisp-mode lisp-mode)
           #'(lambda ()
               (require 'pretty-mode)
               (when user/pretty-mode
                 (pretty-mode 1))))

(provide 'init-font)
;;; init-font.el ends here

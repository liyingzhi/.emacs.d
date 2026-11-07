;;; init-font.el --- font                            -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst user/default-mac-font-size 230
  "The default font size in mac.")

(defconst user/default-win-font-size 110
  "The default font size in windows.")

(defconst user/default-linux-font-size 190
  "The default font size in linux.")

(defvar +default-fontset-configured-p nil
  "Non-nil after the default fontset has been configured.")

(defcustom user/cjk-latin-align-width t
  "Non-nil means strictly align CJK glyph width to Latin advance width.
When enabled, CJK advance is forced to an exact integer multiple
\(typically 2x) of the Latin advance, with no residual spacing.
When nil, the font's natural metrics are used as-is."
  :group 'user
  :type 'boolean)

(defcustom user/ligature nil
  "Is use ligature."
  :group 'user
  :type 'boolean)

(defcustom user/icon-font-size nil
  "The icon font size.
An nil value specifies icon font size same with `user/font-size'.
An integer value specifies font height in units of 1/10 pt."
  :group 'user
  :type '(choice (const nil) number))

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun set-face-like-default (face)
  "Set FACE attributes to match the default face."
  (set-face-attribute face nil
                      :family (face-attribute 'default :family nil 'default)
                      :height 'unspecified
                      :weight 'unspecified
                      :slant 'unspecified))

(defconst user/fallback-fonts
  '("Jetbrains Mono" "Source Code Pro" "PragmataPro Mono Liga"
    "Aporetic Sans Mono" "Aporetic Sans" "Cascadia Code" "Fira Code"
    "SF Mono" "Hack" "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
  "Monospace fonts to try in order when `user/favourite-font' is unavailable.")

(defconst user/fallback-zh-fonts
  '("LXGW Neo Xihei" "WenQuanYi Zen Hei"
    "PingFang SC" "Microsoft Yahei" "STFangsong")
  "Zh fonts to try in order when `user/favourite-zh-font' is unavailable.")

;; Ignore misc-fixed bitmap fonts to prevent them from being used as fallback
(add-list-to-list 'face-ignored-fonts '("-misc-fixed-.*"))

;; setup nerd-icons font
(require 'nerd-icons)

(defun my/nerd-icons-set-font (font-size &optional font-family frame)
  "Set fontset entries for nerd icons at FONT-SIZE using FONT-FAMILY on FRAME.
For each known nerd icon character range, configure the fontset with
a `font-spec' using FONT-FAMILY and size derived from FONT-SIZE.
FONT-FAMILY defaults to `nerd-icons-font-family'.  FRAME defaults to
the selected frame.
This is a variant of `nerd-icons-set-font' that additionally accepts a
numeric FONT-SIZE argument to control the icon glyph size."
  (let ((font-f (or font-family nerd-icons-font-family))
        (charsets '((#xe5fa . #xe6b9)  ;; Seti-UI + Custom
                    (#xe700 . #xe8ef)  ;; Devicons
                    (#xed00 . #xf2ff)  ;; Font Awesome
                    (#xe200 . #xe2a9)  ;; Font Awesome Extension
                    (#xf500 . #xfd46) (#xf0001 . #xf1af0) ;; Material Design Icons
                    (#xe300 . #xe3e3)  ;; Weather
                    (#xf400 . #xf533) #x2665 #x26a1  ;; Octicons
                    (#xe0a0 . #xe0a3) (#xe0b0 . #xe0d7)  ;; Powerline Symbols + Extra
                    (#x23fb . #x23fe) #x2b58  ;; IEC Power Symbols
                    (#xf300 . #xf381)  ;; Font Logos
                    (#xe000 . #xe00a)  ;; Pomicons
                    (#xea60 . #xec1e))))  ;; Codicons
    (cl-loop for charset in charsets do
             (set-fontset-font
              (frame-parameter nil 'font)
              charset
              (font-spec :family font-f
                         :weight nil
                         :size (/ font-size 10.0))
              frame
              'prepend))))

(defun setup-fonts (&optional font-size)
  "Setup fonts.
FONT-SIZE is the default font size."
  ;; Setting the default
  (when (display-graphic-p)

    ;; Setup default font
    (cl-loop for font in (append user/favourite-fonts
                                 user/fallback-fonts)
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        ;; :slant 'italic
                                        ;; :weight 'medium
                                        :height (if font-size
                                                    font-size
                                                  user/font-size)))

    ;; Inline code and code blocks commonly inherit `fixed-pitch'.  Keep it
    ;; in sync with `default' so they use the configured default font too.
    (set-face-like-default 'fixed-pitch)
    (set-face-like-default 'fixed-pitch-serif)
    (set-face-like-default 'variable-pitch)
    ;; Enable `variable-pitch-mode' on ROI hooks so that `buffer-face-mode' is
    ;; set up buffer-locally, replacing the `default' face with `variable-pitch'.

    ;; Set mode-line font
    ;; (cl-loop for font in '("Menlo" "SF Pro Display" "Helvetica")
    ;;          when (font-installed-p font)
    ;;          return (progn
    ;;                   (set-face-attribute 'mode-line nil :family font :height 120)
    ;;                   (when (facep 'mode-line-active)
    ;;                     (set-face-attribute 'mode-line-active nil :family font :height 120))
    ;;                   (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    (unless +default-fontset-configured-p
      ;; Setup specify font for all unicode characters
      ;; https://www.wfonts.com/font/symbola
      (cl-loop for font in '("Apple Symbols" "Segoe UI Symbol" "Symbola" "Symbol")
               when (font-installed-p font)
               return (if (< emacs-major-version 27)
                          (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
                        (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

      ;; Set up Emoji font
      ;; Use the dedicated emoji script without changing unrelated Unicode
      ;; fallbacks.  Keep emoji slightly smaller so Corfu rows are not clipped.
      ;; Overwrite the default list so VS16 sequences use this emoji font.
      (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Noto Emoji" "Segoe UI Emoji")
               when (font-installed-p font)
               return (cond
                       ((< emacs-major-version 27)
                        (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
                       ((< emacs-major-version 28)
                        (set-fontset-font t 'symbol (font-spec :family font)))
                       (t
                        (set-fontset-font t 'emoji (font-spec :family font)))))

      ;; Force Emacs to search by using font-spec
      (set-fontset-font t 'han (font-spec :script 'han) nil 'append)
      (setq +default-fontset-configured-p t))

    ;; Setup specify font for CJK characters font
    (cl-loop for font in (append user/favourite-zh-fonts
                                 user/fallback-zh-fonts)
             when (font-installed-p font)
             return (progn
                      ;; (setq face-font-rescale-alist `((,font . 1.2)))
                      ;; (add-list-to-list 'face-font-rescale-alist `((,font . 1.2)))

                      (when user/cjk-latin-align-width
                        ;; Correct the CJK rescale factor so that CJK characters align with Latin characters
                        (my/setup-cjk-rescale font))

                      ;; Do not use 'unicode charset, it will cause the English font setting invalid
                      ;; kana       = Japanese Hiragana & Katakana (e.g., あ, ア)
                      ;; han        = Chinese characters used in Chinese/Japanese/Korean (e.g., 中, 日, 韓)
                      ;; cjk-misc   = CJK punctuation & symbols (e.g., 、 。 ① ②)
                      ;; bopomofo   = Taiwanese phonetic symbols (e.g., ㄅ, ㄆ, ㄇ)
                      ;; hangul     = Korean Hangul alphabet (e.g., 가, 나, 한)
                      (dolist (charset '(kana han cjk-misc bopomofo hangul))
                        (set-fontset-font t charset
                                          (font-spec :family font)))))

    ;; Setup fall-back fonts
    ;; https://idiocy.org/emacs-fonts-and-fontsets.html
    (dolist (font '("Jigmo" "Jigmo2" "Jigmo3"))
      (when (member font (font-family-list))
        (set-fontset-font "fontset-default" 'han font nil 'append)))

    ;; Set up tai-viet characters font
    (set-fontset-font (frame-parameter nil 'font)
                      'tai-viet
                      (font-spec :family "Noto Sans Tai Viet"))

    ;; Set up special characters font
    (+setup-character-display)

    ;; Set up nerd-icon characters font
    (unless (find-font (font-spec :name nerd-icons-font-family))
      (nerd-icons-install-fonts t))
    (if (and user/icon-font-size (> user/icon-font-size 0))
        (my/nerd-icons-set-font user/icon-font-size)
      (nerd-icons-set-font))))

(defun +setup-character-display ()
  "Set up special character composition and display."
  (when sys/linuxp
    ;; Compose VARIATION SELECTOR-16 with the preceding character.
    ;; https://t.me/emacs_china/297476
    (set-char-table-range composition-function-table #xFE0F '(["\\c.\\c^+" 1 compose-gstring-for-graphic])))

  ;; Hide U+FFF4 on all platforms.
  (set-char-table-range glyphless-char-display #xFFF4 'zero-width))

(defun my/font-set-rescale (font scale)
  "Set the rescale factor for FONT to SCALE in `face-font-rescale-alist'.
If an entry for FONT already exists, update it in place; otherwise
push a new (FONT . SCALE) entry onto the alist."
  (let ((entry (assoc font face-font-rescale-alist)))
    (if entry
        (setcdr entry scale)
      (push `(,font . ,scale) face-font-rescale-alist))))

(defun my/font-latin-cjk-width-ratio (latin cjk)
  "Return the ratio of 2 LATIN glyph widths to 1 CJK glyph width.

LATIN and CJK are single-character strings (e.g. \"h\" and \"汉\").
The widths are measured via `string-pixel-width' in the current font.
The font's rescale factor should be 1 at measurement time so the result
can be used as a CJK rescale factor; return 1.0 when 2 Latin glyphs are
exactly as wide as 1 CJK glyph."
  (/ (* 2.0 (string-pixel-width latin))
     (string-pixel-width cjk)))

(defun my/setup-cjk-rescale (font)
  "Correct FONT's CJK width rescale and map the `han' script to FONT.

FONT is a font family name, e.g. \"Noto Sans SC\".  The rescale factor
is the ratio of two Latin glyph widths to one CJK glyph width,
measured at rescale factor 1 (see
`my/font-latin-cjk-width-ratio'), and is written to
`face-font-rescale-alist' so that CJK text is exactly twice as wide
as Latin.  This corrects fonts whose CJK glyphs are not already twice
as wide as their Latin glyphs.

Return the face spec of the `han' entry (see `set-fontset-font')."
  (my/font-set-rescale font 1)
  (set-fontset-font t 'han (font-spec :family font))
  (my/font-set-rescale font (my/font-latin-cjk-width-ratio "h" "汉"))
  (set-fontset-font t 'han (font-spec :family font)))

(defun my/measure-font-pixel-width (string &optional height font-family)
  "Return the pixel width of STRING rendered at HEIGHT in FONT-FAMILY.

STRING is measured in a temporary buffer whose `default' face is
remapped to HEIGHT and/or FONT-FAMILY, so the result reflects the
actual glyph metrics of that font configuration.

HEIGHT, if non-nil, is a face height spec: an integer is an
absolute height in 1/10 pt (120 means 12 pt), while a float is a
scale factor relative to the default face (1.5 means 1.5x).

FONT-FAMILY, if non-nil, is a font family name such as \"Maple
Mono\".  Either argument may be given alone; when both are nil,
STRING is measured in the default face.

This requires a graphical frame.  On a TTY frame, Emacs falls back
to character-width-based estimation, which is independent of
HEIGHT and FONT-FAMILY.

If STRING contains newlines, return the width of the widest
newline-free substring, as with `string-pixel-width'.

For example:
  (my/measure-font-pixel-width \"h\")
  (my/measure-font-pixel-width \"h\" 150)
  (my/measure-font-pixel-width \"h\" 150 \"Iosevka Fixed\")
  (my/measure-font-pixel-width \"汉\" 150 \"LXGW WenKai Mono\")"
  (declare (important-return-value t))
  (when font-family
    (unless (font-installed-p font-family)
      (user-error "Font %s is not installed" font-family)))
  (with-temp-buffer
    (when (or height font-family)
      (buffer-face-set (append (when height (list :height height))
                               (when font-family (list :family font-family)))))
    (string-pixel-width string (current-buffer))))

(defun my/setup-cjk-rescale-explicit (font-cjk &optional font-latin height-cjk height-latin)
  "Correct the CJK rescale for FONT-CJK and map the `han' script to it.

FONT-CJK is a font family name, e.g. \"LXGW WenKai Mono\".  Its rescale
factor is set so that CJK text is exactly twice as wide as Latin,
fixing fonts whose CJK glyphs are not already twice as wide as their
Latin glyphs.  FONT-LATIN, if non-nil, is the font family used to
measure the width of \"h\"; when omitted, the measurement is made
in the default face.  HEIGHT-CJK and HEIGHT-LATIN, if given, are height
specs for the measurement, as accepted by `my/measure-font-pixel-width'.

FONT-CJK's rescale factor is first reset to 1, and the `han' script
is mapped to FONT-CJK.  The pixel width of \"汉\" in FONT-CJK and of
\"h\" in FONT-LATIN are then measured via `my/measure-font-pixel-width';
FONT-CJK's rescale factor is set to twice the width of \"h\" divided by
the width of \"汉\" and written to `face-font-rescale-alist'.  Finally
the `han' script is mapped to FONT-CJK.

For example:
  (my/setup-cjk-rescale-explicit \"LXGW WenKai Mono\")
  (my/setup-cjk-rescale-explicit \"LXGW WenKai Mono\" \"Iosevka Fixed\")
  (my/setup-cjk-rescale-explicit \"LXGW WenKai Mono\" nil 150)
  (my/setup-cjk-rescale-explicit \"LXGW WenKai Mono\" \"Iosevka Fixed\" 150 150)

Return the face spec of the `han' entry (see `set-fontset-font')."
  (my/font-set-rescale font-cjk 1)
  (set-fontset-font t 'han (font-spec :family font-cjk))
  (let* ((cjk-char-width (my/measure-font-pixel-width "汉" height-cjk font-cjk))
         (latin-char-width (my/measure-font-pixel-width "h" height-latin font-latin))
         (cjk-to-latin-ratio (/ (* 2.0 latin-char-width) cjk-char-width)))
    (my/font-set-rescale font-cjk cjk-to-latin-ratio)
    (set-fontset-font t 'han (font-spec :family font-cjk))))

(defun set-font-size (symbol value)
  "Set font SYMBOL VALUE."
  (set-default-toplevel-value symbol value)
  (setup-fonts value))

(defcustom user/font-size (cond (sys/macp user/default-mac-font-size)
                                (sys/win32p user/default-win-font-size)
                                (t user/default-linux-font-size))
  "The font size.
An integer value specifies font height in units of 1/10 pt."
  :group 'user
  :type 'number
  :set #'set-font-size)

;;; setup buffer-specified font
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


(set-font-for-modes `((vterm-mode . ,user/*term-default-font*)))

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

;;; setup buffer-specified font
;; setopt on user/font-size auto-calls set-font-size (the defcustom :set function),
;; which in turn calls setup-fonts

;; (setup-fonts)
(add-hook 'server-after-make-frame-hook #'setup-fonts)

(when user/ligature (global-ligature-mode))

(provide 'init-font)
;;; init-font.el ends here

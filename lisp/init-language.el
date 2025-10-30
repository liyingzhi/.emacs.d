;;; init-language.el --- init language package       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; gt
(require 'init-gt)

;;; immersive-translate

(setopt immersive-translate-backend 'trans)
(add-hook 'elfeed-show-mode-hook #'immersive-translate-setup)
(add-hook 'nov-pre-html-render-hook #'immersive-translate-setup)

;;; fanyi
(custom-set-variables
 '(fanyi-providers '(fanyi-haici-provider
                     fanyi-youdao-thesaurus-provider
                     fanyi-longman-provider)))

(with-eval-after-load 'fanyi
  (keymap-sets fanyi-mode-map
    '(("M-<left>" . previous-buffer-dedicated-window)
      ("M-<right>" . next-buffer-dedicated-window))))

(pretty-hydra-define-e hydra-language
  (:title "Language" :color amaranth :quit-key ("C-g" "q" "<escape>") :all-exit t)
  ("Dict"
   (("s" sdcv-search-pointer+ "sdcv dict")
    ("f" fanyi-dwim2 "Fanyi Point")
    ("F" fanyi-dwim "Fanyi Input"))

   "English"
   (("t" gt-translate "show (en->zh) with taker")
    ("e" (lambda ()
           (interactive)
           (activate-input-method default-input-method)
           (call-interactively #'gptel-translate-to-english-insert))
     "Insert (*->en) with prompt"))

   "Preset"
   (("g" gt-translate-prompt "(en->zh) with taker & prompt")
    ("u" gt-use-text-utility "text utility")
    ("k" gt-use-korean-gt "show (ko->zh) with taker"))))

;;; spell
(require 'init-spell)

;;; input
(setq default-input-method "rime")

(require 'init-pyim)

(when (equal default-input-method "rime")
  (require 'init-rime))

(provide 'init-language)
;;; init-language.el ends here

;;; init-language.el --- init language package       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; gt
(require 'init-gt)

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
  ("dict"
   (("s" sdcv-search-pointer+ "sdcv dict")
    ("f" fanyi-dwim2 "Fanyi Point")
    ("F" fanyi-dwim "Fanyi Input"))
   "english"
   (("t" gt-translate "show translated from")
    ("e" gptel-translate-to-english-insert "Insert translated to"))
   "preset"
   (("g" gt-translate-prompt "translate with prompt")
    ("u" gt-use-text-utility "text utility")
    ("k" gt-use-korean-gt "translate Korean"))))

(provide 'init-language)
;;; init-language.el ends here

(global-set-keys
 '(("s-." . embark-act)
   ("M-<left>" . previous-buffer)
   ("M-<right>" . next-buffer)
   ;; ("M-<left>" . bury-buffer)
   ;; ("M-<right>" . unbury-buffer)
   ("C-s-;" . embark-dwim)
   ("C-M-;" . embark-dwim)
   ("C-h B" . embark-bindings)))

(global-set-keys
 '(("RET" . newline-and-indent)
   ("S-<return>" . comment-indent-new-line)
   (("s-o" "M-o") . ace-window)
   (("s-n" "M-n") . scroll-up-1/3)
   (("s-p" "M-p") . scroll-down-1/3)
   (("M-N" "s-N") . scroll-other-window-up-1/3)
   (("M-P" "s-P") . scroll-other-window-down-1/3)
   (("s-x" "M-x") . execute-extended-command)
   ("C-s-f" . forward-sexp)
   ("C-s-b" . backward-sexp)))

(global-set-keys
 '(("C-M-c" . kill-emacs)
   ("C-M-r" . restart-emacs)
   ("C-M-y" . my/copy-current-line)
   ("C-M-e" . my/select-end-of-buffer-to-point)))

(with-eval-after-load 'eww
  (keymap-sets eww-mode-map
    '(("M-n" . scroll-up-1/3)
      ("M-p" . scroll-down-1/3))))

(with-eval-after-load 'compile
  (keymap-sets compilation-mode-map
    '(("s-n" . compilation-next-error)
      ("s-p" . compilation-previous-error))))

;;;###autoload
(defmacro lazy-one-key-create-menu (title &rest keybinds)
  (let (one-key-key-alist)
    (dolist (ele keybinds)
      (when (plist-get ele :filename)
        (autoload (plist-get ele :command) (plist-get ele :filename) nil t))
      (push
       (if (plist-get ele :run)
           (cons (cons (plist-get ele :key) (plist-get ele :description)) (plist-get ele :run))
         (cons (cons (plist-get ele :key) (plist-get ele :description)) (plist-get ele :command)))
       one-key-key-alist))
    `(one-key-create-menu ,title (quote ,one-key-key-alist))))

(lazy-one-key-create-menu
 "Toggle"
 (:key "t" :description "Toggle telega" :command +lizqwer/toggle-telega :filename "init-telega")
 (:key "b" :description "Toggle imenu list mode" :command imenu-list-smart-toggle)
 (:key "T" :description "Toggle transparent" :command +lizqwer/toggle-transparent :filename "init-func")
 (:key "e" :description "Toggle dark theme" :command +lizqwer/toggle-dark-theme :filename "init-func")
 (:key "r" :description "Toggle redacted" :command redacted-mode)
 (:key "c" :description "Toggle center cursor" :command global-centered-cursor-mode)
 (:key "s" :description "Toggle interaction-log" :command interaction-log-mode :filename "interaction-log")
 (:key "w" :description "Toggle sub word or super word" :command toggle-sub-word-or-super-word :filename "init-edit")

 (:key "i" :description "Toggle immersive-translate" :command immersive-translate-auto-mode :filename "init-immersive-translate"))

(defun consult-fd-in-home ()
  (interactive)
  (consult-fd "~"))

(lazy-one-key-create-menu
 "Search"
 (:key "l" :description "Search in buffer" :command consult-line)
 (:key "L" :description "Search in line in multi buffer" :command consult-line-multi)
 (:key "i" :description "Search imenu" :command consult-imenu)
 (:key "I" :description "Search imenu in multi buffer" :command consult-imenu-multi)
 (:key "o" :description "Search outline" :command consult-outline)
 (:key "B" :description "Bookmark" :command consult-bookmark)
 (:key "s" :description "Blink Search" :command blink-search :filename "init-blink-search")
 (:key "j" :description "color rg search symbol in current directory" :command color-rg-search-input)
 (:key "p" :description "color rg search symbol in project" :command color-rg-search-input-in-project)
 (:key "r" :description "rg menu" :command rg-menu :filename "init-rg")
 (:key "b" :description "Webjump" :command webjump :filename "init-webjump")
 (:key "g" :description "Google this" :command one-key-menu-google :filename "init-google-this")
 (:key "f" :description "Search file in home" :command consult-fd-in-home)
 (:key "y" :description "Search YASsnippet" :command consult-yasnippet)
 (:key "w" :description "Search in omni sources" :command consult-omni-multi :filename "init-consult-omni")
 (:key "m" :description "Search global mark" :command consult-global-mark))

(defun ibuffer-refersh ()
  (interactive)
  (when-let* ((buffer (get-buffer "*Ibuffer*")))
    (kill-buffer buffer))
  (ibuffer))

(lazy-one-key-create-menu
 "Buffer"
 (:key "b" :description "Switch buffer" :command consult-buffer)
 (:key "B" :description "Switch buffer other window" :command consult-buffer-other-window)
 (:key "I" :description "ibuffer" :command ibuffer-refersh)
 (:key "k" :description "Kill buffer" :command kill-buffer-and-window)
 (:key "T" :description "Switch telega buffers" :command telega-switch-buffer :filename "init-telega")
 (:key "i" :description "Switch telega important chat" :command telega-switch-important-chat :filename "init-telega")
 (:key "t" :description "Switch telega chat" :command telega-chat-with :filename "init-telega")
 (:key "r" :description "Revert buffer" :command revert-buffer-quick)
 (:key "h" :description "bury buffer" :command bury-buffer)
 (:key "l" :description "unbury buffer" :command unbury-buffer)
 (:key "s" :description "Save buffer" :command save-buffer))

(lazy-one-key-create-menu
 "FileAction"
 (:key "c" :description "Copy File name" :command +lizqwer/copy-file-name-to-clipboard :filename "init-func")
 (:key "C" :description "Copy this file to" :command bufferfile-copy :filename "init-bufferfile")
 (:key "p" :description "Copy File Path" :command +lizqwer/copy-file-path-to-clipboard :filename "init-func")
 (:key "d" :description "Delete this file" :command bufferfile-delete :filename "init-bufferfile")
 (:key "r" :description "Rename this file" :command bufferfile-rename :filename "init-bufferfile")
 (:key "b" :description "Browse this file" :command browse-this-file :filename "init-func"))

(one-key-create-menu
 "File"
 '((("f" . "Find file") . find-file)
   (("o" . "Find other file") . ff-find-other-file)
   (("F" . "Find file other window") . find-file-other-window)
   (("s" . "Fuzzy search") . consult-fd)
   (("a" . "Action file") . one-key-menu-fileaction)
   (("r" . "Recent file") . consult-recent-file)
   (("h" . "Find in main dir") . (lambda ()
                                   (interactive)
                                   (ido-find-file-in-dir "~/")))
   (("p" . "Find in project") . (lambda ()
                                  (interactive)
                                  (require 'project)
                                  (ido-find-file-in-dir "~/MyProject")))
   (("c" . "Find custom file") . find-custom-file)))


(defmacro open-dir (path)
  `(lambda ()
     (interactive)
     (find-file ,path)))

(one-key-create-menu
 "Directory"
 `((("h" . "Home Dir") . ,(open-dir "~/"))
   (("c" . "Config Dir") . ,(open-dir "~/.emacs.d/"))
   (("g" . "Github Dir") . ,(open-dir "~/github/"))
   (("p" . "Project Dir") . ,(open-dir "~/MyProject/"))
   (("d" . "Document Dir") . ,(open-dir "~/Documents/"))
   (("s" . "Fuzzy search Dir") . consult-fd-dir)
   (("v" . "Dirvish") . (lambda ()
                          (interactive)
                          (when user/dirvish
                            (call-interactively #'dirvish-dwim))))
   (("j" . "Dired jump") . dired-jump)
   (("J" . "Dired jump other window") . dired-jump-other-window)))

(one-key-create-menu
 "sort-tab"
 '((("n" . "Sort tab select next tab") . sort-tab-select-next-tab)
   (("p" . "Sort tab select prev tab") . sort-tab-select-prev-tab)
   (("c" . "Sort tab close current tab") . sort-tab-close-current-tab)
   (("m" . "Sort tab close all mode tabs") . sort-tab-close-mode-tabs)))

(one-key-create-menu
 "Git"
 '((("n" . "Next hunk") . diff-hl-next-hunk)
   (("p" . "Previous hunk") . diff-hl-previous-hunk)
   (("s" . "Show hunk") . diff-hl-show-hunk)
   (("b" . "Blame") . magit-blame)))

(lazy-one-key-create-menu
 "Useful"
 (:key "T" :description "Sdcv translate" :command sdcv-search-pointer+ :filename "init-sdcv")
 (:key "S" :description "Sudo edit find file" :command sudo-edit-find-file :filename "sudo-edit")
 (:key "e" :description "Toggle sdcv" :command lsp-bridge-toggle-sdcv-helper :filename "init-lsp-bridge")
 (:key "D" :description "Docker" :command docker)
 (:key "p" :description "peek code" :command peek-overlay-dwim :filename "init-peek")
 (:key "d" :description "Devdocs" :command devdocs-lookup)
 (:key "s" :description "screenshot" :command screenshot)
 (:key "C" :description "Insert color" :command my-insert-color-hex :filename "init-func")
 (:key "g" :description "gptel" :command gptel)
 (:key "G" :description "gptel menu" :command gptel-menu)
 (:key "h" :description "gptel aibo" :command gptel-aibo)
 (:key "a" :description "Aider" :command aidermacs-transient-menu)
 (:key "o" :description "Casual-main-tmenu" :command casual-editkit-main-tmenu)
 (:key "t" :description "llm task with gt engine" :command my/gt-ai-oneshot)
 (:key "c" :description "Up-down convert dispatch" :command my/string-convert-dispatch)
 (:key "j" :description "Calculator" :command calc))

(lazy-one-key-create-menu
 "EAF"
 (:key "s" :description "monitor" :command eaf-open-system-monitor)
 (:key "j" :description "jupyter" :command eaf-open-jupyter)
 (:key "w" :description "Office" :command eaf-open-office)
 (:key "i" :description "idea-mindmap" :command eaf-create-mindmap)
 (:key "m" :description "Map" :command eaf-open-map)
 (:key "g" :description "eaf-git" :command eaf-open-git)
 (:key "d" :description "eaf-dired" :command eaf-open-in-file-manager)
 (:key "t" :description "pyqterminal" :command eaf-open-pyqterminal)
 (:key "b" :description "Browser" :command eaf-open-browser)
 (:key "B" :description "Browser" :command eaf-open-browser-other-window)
 (:key "h" :description "Browser-history" :command eaf-open-browser-with-history)
 (:key "o" :description "eaf-open" :command eaf-open)
 (:key "c" :description "music-online" :command eaf-open-cloud-music)
 (:key "l" :description "music-offline" :command eaf-open-music-player)
 (:key "n" :description "open mind elixir" :command eaf-open-mind-elixir))

(defun lsp-diagnostic ()
  (interactive)
  (pcase user/lsp-client
    ('eglot
     (call-interactively #'consult-flycheck))
    ('lsp-bridge
     (autoload #'one-key-menu-diagnostic "init-lsp-bridge" nil t)
     (call-interactively #'one-key-menu-diagnostic))))

(defun lsp-rename ()
  (interactive)
  (pcase user/lsp-client
    ('eglot
     (call-interactively #'eglot-rename))
    ('lsp-bridge
     (autoload #'lsp-bridge-rename "init-lsp-bridge" nil t)
     (call-interactively #'lsp-bridge-rename))))
(defun lsp-restart ()
  (interactive)
  (pcase user/lsp-client
    ('eglot
     (call-interactively #'eglot-shutdown)
     (call-interactively #'eglot))
    ('lsp-bridge
     (autoload #'lsp-bridge-restart-process "init-lsp-bridge" nil t)
     (call-interactively #'lsp-bridge-restart-process))))

(defun lsp-code-action ()
  (interactive)
  (pcase user/lsp-client
    ('eglot
     (call-interactively #'eglot-code-actions))
    ('lsp-bridge
     (autoload #'lsp-bridge-code-action "init-lsp-bridge" nil t)
     (call-interactively #'lsp-bridge-code-action))))

(defun lsp-search-symbol ()
  (interactive)
  (pcase user/lsp-client
    ('eglot
     (call-interactively #'consult-eglot-symbols))
    ('lsp-bridge
     (autoload #'lsp-bridge-workspace-list-symbols "init-lsp-bridge" nil t)
     (call-interactively #'lsp-bridge-workspace-list-symbols))))

(defun code-peek ()
  (interactive)
  (pcase user/lsp-client
    ('eglot
     (if (bound-and-true-p citre-mode)
         (call-interactively #'citre-peek)
       (message "Eglot not support peek")))
    ('lsp-bridge
     (autoload #'lsp-bridge-peek "init-lsp-bridge" nil t)
     (call-interactively #'lsp-bridge-peek))))

(defun code-ace-peek ()
  (interactive)
  (if (equal user/lsp-client 'eglot)
      (if (bound-and-true-p citre-mode)
          (call-interactively #'citre-ace-peek)
        (message "Eglot not support peek"))
    (message "lsp-bridge not support peek")))


(lazy-one-key-create-menu
 "Code"
 (:key "f" :description "Format code" :command format-code-buffer :filename "init-format")
 (:key "d" :description "Diagnostic" :command lsp-diagnostic)
 (:key "r" :description "Lsp rename" :command lsp-rename)
 (:key "R" :description "Lsp restart" :command lsp-restart)
 (:key "a" :description "Lsp code action" :command lsp-code-action)
 (:key "s" :description "Lsp search symbol" :command lsp-search-symbol)
 (:key "p" :description "Code peek" :command code-peek)
 (:key "P" :description "Code peek" :command code-ace-peek))

(lazy-one-key-create-menu
 "Org"
 (:key "w" :description "Open org file" :command open-my-org-file :filename "init-org")
 (:key "c" :description "Open org capture" :command org-capture :filename "init-org")
 (:key "a" :description "Open org agenda" :command one-key-menu-agenda :filename "init-org")
 (:key "l" :description "Org store link" :command org-store-link :filename "init-org")
 (:key "i" :description "Org insert link" :command org-insert-link :filename "init-org")
 (:key "s" :description "Org search" :command consult-notes)
 (:key "r" :description "Org roam" :command one-key-menu-roam)
 (:key "t" :description "Vterm" :command multi-vterm-open :filename "multi-vterm")
 (:key "m" :description "Media note" :command org-media-note-show-interface)
 (:key "d" :description "Denote" :command denote-open-or-create))

(lazy-one-key-create-menu
 "Insert"
 (:key "i" :description "Insert import" :command insert-import :filename "init-func")
 (:key "f" :description "Insert file" :command hydra-insert-file/body)
 (:key "j" :description "Insert json to type" :command quicktype))

;;; symbol overlay
(lazy-load-global-keys
 '(("M-i" . symbol-overlay-put))
 "symbol-overlay")

;;; webjump
;; (lazy-load-global-keys
;;  '(("C-c /" . webjump))
;;  "init-webjump")

(global-set-key (kbd "C-c /")
                #'google-this)

;;; yank
(global-set-key (kbd "M-y") #'consult-yank-pop)

;;; gif screenshot
(with-eval-after-load 'gif-screencast
  (keymap-sets gif-screencast-mode-map
    '(("<f8>" . gif-screencast-toggle-pause)
      ("<f9>" . gif-screencast-stop))))

;;; vterm
(with-eval-after-load 'vterm
  (keymap-sets vterm-mode-map
    '(("C-y" . vterm-yank))))

(defun my/one-key-menu-auto-popup-advice (fn)
  (let ((one-key-popup-window t))
    (funcall fn)))

(advice-add 'one-key-menu-thing-edit :around #'my/one-key-menu-auto-popup-advice)
(advice-add 'one-key-menu-mark-macro :around #'my/one-key-menu-auto-popup-advice)
(advice-add 'one-key-menu-tool-kit :around #'my/one-key-menu-auto-popup-advice)

(provide 'init-key)

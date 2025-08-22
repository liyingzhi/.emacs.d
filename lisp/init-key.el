;; ;;; ### Watch other window ###
;; ;;; --- 滑动其他窗口
(require 'watch-other-window)
(global-set-keys
 '((("M-N" "s-N") . (lambda ()
                      (interactive)
                      (watch-other-window-internal "up"
                                                   (/ (window-body-height) 3))))
   (("M-P" "s-P") . (lambda ()
                      (interactive)
                      (watch-other-window-internal "down"
                                                   (/ (window-body-height) 3))))))

(global-set-keys
 '(("C-M-c" . kill-emacs)
   ("C-M-r" . restart-emacs)
   ("C-M-y" . my/copy-current-line)
   ("C-M-e" . my/select-end-of-buffer-to-point)))

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
 "Org"
 (:key "w" :description "Open org file" :command open-my-org-file :filename "init-org")
 (:key "c" :description "Open org capture" :command org-capture :filename "init-org")
 (:key "a" :description "Open org agenda" :command one-key-menu-agenda :filename "init-org")
 (:key "l" :description "Org store link" :command org-store-link :filename "init-org")
 (:key "s" :description "Org search" :command consult-notes)
 (:key "r" :description "Org roam" :command one-key-menu-roam))

(lazy-one-key-create-menu
 "Buffer"
 (:key "b" :description "Switch buffer" :command consult-buffer)
 (:key "B" :description "Switch buffer other window" :command consult-buffer-other-window)
 (:key "k" :description "Kill buffer" :command kill-buffer-and-window)
 (:key "r" :description "Revert buffer" :command revert-buffer)
 (:key "s" :description "Save buffer" :command save-buffer))

(lazy-one-key-create-menu
 "FileAction"
 (:key "c" :description "Copy File name" :command +lizqwer/copy-file-name-to-clipboard )
 (:key "C" :description "Copy File Path" :command +lizqwer/copy-file-path-to-clipboard )
 (:key "d" :description "Delete this file" :command delete-this-file )
 (:key "r" :description "Rename this file" :command rename-this-file )
 (:key "b" :description "Browse this file" :command browse-this-file ))

(one-key-create-menu
 "File"
 '((("f" . "Find file") . find-file)
   (("o" . "Find other file") . ff-find-other-file)
   (("F" . "Find file other window") . find-file-other-window)
   (("w" . "Save file") . write-file)
   (("s" . "Fuzzy search") . consult-fd)
   (("a" . "Action file") . one-key-menu-fileaction)
   (("r" . "Recent file") . recentf-open)
   (("h" . "Find in main dir") . (lambda ()
                                   (interactive)
                                   (ido-find-file-in-dir "~/")))
   (("c" . "Find custom file") . find-custom-file)))

(lazy-one-key-create-menu
 "Directory"
 (:key "h" :description "Home" :command (lambda () (interactive) (find-file "~/")))
 (:key "d" :description "Downloads" :command (lambda () (interactive) (find-file "C:\\Users\\liyin\\Downloads")))
 (:key "f" :description "Home" :command (lambda () (interactive) (find-file "D:/EmacsConfig/Documents")))
 (:key "t" :description "Trashed" :command trashed)
 (:key "c" :description "Emacs Config" :command (lambda () (interactive) (find-file "D:\\EmacsConfig\\.emacs.d")))
 (:key "g" :description "Github" :command (lambda () (interactive) (find-file "D:\\emacs-work-dir\\github")))
 (:key "p" :description "Project" :command (lambda () (interactive) (find-file "D:\\emacs-work-dir\\project")))
 (:key "j" :description "Dired jump" :command dired-jump)
 (:key "J" :description "Dired jump other" :command dired-jump-other-window)
 (:key "e" :description "Dired jump extern" :command (lambda () (interactive) (when sys/win32p (my/open-explorer)))))

;;; symbol overlay
(lazy-load-global-keys
 '(("M-i" . symbol-overlay-put))
 "symbol-overlay")


(defun lsp-diagnostic ()
  (interactive)
  (pcase user/lsp-client
    ('eglot
     (call-interactively #'consult-flymake))
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
 (:key "f" :description "Format code" :command apheleia-format-buffer :filename "init-format")
 (:key "d" :description "Diagnostic" :command lsp-diagnostic)
 (:key "r" :description "Lsp rename" :command lsp-rename)
 (:key "R" :description "Lsp restart" :command lsp-restart)
 (:key "a" :description "Lsp code action" :command lsp-code-action)
 (:key "s" :description "Lsp search symbol" :command lsp-search-symbol)
 (:key "p" :description "Code peek" :command code-peek)
 (:key "P" :description "Code peek" :command code-ace-peek))

(defun consult-fd-in-home ()
  (interactive)
  (consult-fd "~"))

(lazy-one-key-create-menu
 "Search"
 (:key "l" :description "Search in buffer" :command consult-line)
 (:key "i" :description "Search imenu" :command consult-imenu)
 (:key "m" :description "Search imenu int multi buffer" :command consult-imenu-multi)
 (:key "o" :description "Search outline" :command consult-outline)
 (:key "B" :description "Bookmark" :command consult-bookmark)
 (:key "j" :description "color rg search symbol in current file" :command color-rg-search-input-in-current-file :filename "init-color-rg")
 (:key "p" :description "color rg search symbol in project" :command color-rg-search-input-in-project :filename "init-color-rg")
 (:key "f" :description "Search file in home" :command consult-fd-in-home)
 (:key "y" :description "Search YASnippet" :command consult-yasnippet))

(provide 'init-key)

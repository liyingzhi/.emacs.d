;;; init-packages.el --- straight packages configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar *package-build-in-install-list*
  '(
    ;; emacs built-in org
    (org :type built-in)

    ;; for latex-preview enhanced branch

    ;; (org
    ;;  :fork (:host nil
    ;;               :repo "https://git.tecosaur.net/tec/org-mode.git"
    ;;               :branch "dev"
    ;;               :remote "tecosaur")
    ;;  :files (:defaults "etc")
    ;;  :build t
    ;;  :pre-build
    ;;  (with-temp-file "org-version.el"
    ;;    (require 'lisp-mnt)
    ;;    (let ((version
    ;;           (with-temp-buffer
    ;;             (insert-file-contents "lisp/org.el")
    ;;             (lm-header "version")))
    ;;          (git-version
    ;;           (string-trim
    ;;            (with-temp-buffer
    ;;              (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
    ;;              (buffer-string)))))
    ;;      (insert
    ;;       (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
    ;;       (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
    ;;       "(provide 'org-version)\n")))
    ;;  :pin nil)
    ))

(defvar *package-base-install-list*
  '(gcmh
    vertico
    marginalia
    consult
    consult-notes
    consult-yasnippet
    consult-dir
    bufferfile
    bufferlo
    embark
    embark-consult
    orderless
    fussy
    (flx-rs
     :repo "jcs-elpa/flx-rs"
     :host github
     :files (:defaults "bin"))
    posframe
    request
    websocket
    rg
    xclip
    helpful
    which-key
    hydra
    pretty-hydra))

(defvar *package-tool-install-list*
  '(alert
    (knockknock :host github :repo "konrad1977/knockknock")
    centered-cursor-mode
    sudo-edit
    google-this
    interaction-log
    restclient
    dired-git-info
    dired-rsync
    dired-rsync-transient
    dired-toggle-sudo
    diredfl
    dired-subtree
    dired-quick-sort
    dired-preview
    dirvish
    trashed
    elisp-demos
    eros
    (p-search :repo "zkry/p-search" :host github)
    heap
    (rsync-project-mode
     :host github
     :repo "lizqwerscott/rsync-project-mode")
    gif-screencast
    keycast
    cal-china-x
    consult-gh
    consult-gh-forge
    (blink-search
     :host github
     :repo "manateelazycat/blink-search"
     :files (:defaults "*.el" "*.py" "backend" "core" "icons"))
    backup-walker
    show-font))

(defvar *package-language-mode-install-list*
  '(markdown-mode
    log4j-mode
    just-mode
    elvish-mode
    git-modes
    csv-mode
    sxhkdrc-mode
    meson-mode))

(defvar *package-edit-install-list*
  '((meow :host github :repo "meow-edit/meow")
    meow-tree-sitter
    (repeat-fu :host codeberg :repo "ideasman42/emacs-repeat-fu")
    grugru
    auto-rename-tag
    hungry-delete
    separedit
    symbol-overlay
    symbol-overlay-mc
    aggressive-indent
    apheleia
    avy
    vundo
    outline-indent
    visual-replace
    visual-regexp
    visual-regexp-steroids
    (fingertip :host github :repo "manateelazycat/fingertip")
    (expreg :host github
            :repo "bommbo/expreg"
            :branch "add-numbered-selection")
    (diverted :host github :repo "xenodium/diverted")
    (home-row-expreg-diverted :host github :repo "bommbo/home-row-expreg-diverted")))

(defvar *package-program-install-list*
  `(yasnippet
    yasnippet-capf
    macrostep
    mason
    ,@(pcase user/lsp-client
        ('eglot
         '(eglot
           (eglot-booster :host github :repo "jdtsmith/eglot-booster")
           consult-eglot
           flycheck
           consult-flycheck
           flycheck-eglot
           flycheck-posframe))
        ('lsp-bridge
         `(dumb-jump
           (lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                       :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                       :build (:not compile))
           ,@(when (not (display-graphic-p))
               '((popon :fetcher git :url "https://codeberg.org/akib/emacs-popon.git")
                 (acm-terminal :fetcher git :url "https://github.com/twlz0ne/acm-terminal.git"))))))
    corfu
    cape
    nerd-icons-corfu
    eldoc-box
    ,@(when user/flyoverp
        '(flyover :host github :repo "konrad1977/flyover"))
    (cond-let :host github :repo "tarsius/cond-let")
    magit
    magit-delta
    git-link
    forge
    difftastic
    devdocs
    wucuo
    flyspell-correct
    compile-multi
    projection
    projection-multi
    eshell-prompt-extras
    esh-help
    dape
    citre
    (xmake :host github :repo "lizqwerscott/xmake-emacs")
    (quicktype :host github :repo "artawower/quicktype.el")
    (color-rg :host github
              :repo "manateelazycat/color-rg")
    (peek :host sourcehut :repo "meow_king/peek")

    (auto-save :host github :repo "manateelazycat/auto-save")
    super-save
    org-sliced-images))

(defvar *package-ui-install-list*
  '(modus-themes
    solarized-theme
    (koishi-theme :host github :repo "gynamics/koishi-theme.el")
    miasma-theme
    catppuccin-theme
    (modus-catppuccin :host gitlab :repo "magus/modus-catppuccin")
    kanagawa-themes
    nerd-icons
    nerd-icons-dired
    nerd-icons-completion
    nerd-icons-ibuffer
    page-break-lines
    ligature
    dashboard
    (grid :host github :repo "ichernyshovvv/grid.el")
    enlight
    doom-modeline
    pretty-mode
    color-identifiers-mode
    lisp-extra-font-lock
    highlight-function-calls
    focus
    pulsar
    diff-hl
    rainbow-delimiters
    highlight-parentheses
    colorful-mode
    visual-fill-column
    redacted
    hl-todo
    ;; (hl-todo :host github
    ;;          :repo "lizqwerscott/hl-todo"
    ;;          :branch "fix-hl-todo--regexp")
    consult-todo
    imenu-list
    outli
    (indent-bars :host github :repo "jdtsmith/indent-bars")
    (sort-tab :host github
              :repo "manateelazycat/sort-tab")
    (awesome-tray :host github
                  :repo "manateelazycat/awesome-tray")
    (breadcrumb :host github
                :repo "joaotavora/breadcrumb")
    buffer-name-relative
    (prism :host github :repo "alphapapa/prism.el")
    casual
    casual-symbol-overlay
    time-zones
    inhibit-mouse))

(defvar *package-mms-install-list*
  '(emms))

(defvar *package-window-install-list*
  '(popper
    ace-window))

(defvar *package-language-install-list*
  '(immersive-translate
    (sdcv
     :host github
     :repo "manateelazycat/sdcv")
    fanyi
    (pdd :fetcher github :repo "lorniu/pdd.el")
    (gt :fetcher github :repo "lorniu/gt.el")
    unicode-math-input
    rime
    pyim
    pyim-basedict
    (pyim-tsinghua-dict
     :host github
     :repo "redguardtoo/pyim-tsinghua-dict"
     :files ("pyim-tsinghua-dict.el" "pyim-tsinghua-dict.pyim"))
    ))

(defvar *package-org-install-list*
  '(htmlize
    org-superstar
    org-fancy-priorities
    org-roam
    org-roam-ui
    ox-reveal
    ox-hugo
    org-appear
    org-fragtog
    org-journal
    valign
    ftable
    (pangu-spacing
     :host github
     :repo  "nailuoGG/pangu-spacing"
     :branch "remove-old-version-support"
     :files ("*.el"))

    (org-modern-indent :host github :repo "jdtsmith/org-modern-indent")
    (org-count-words :host github :repo "Elilif/org-count-words")
    org-web-tools
    org-rich-yank
    (org-media-note :host github :repo "yuchen-lea/org-media-note")
    org-download))

(defvar *package-write-install-list*
  '(nov
    pdf-tools
    ;; (reader :fetcher codeberg :repo "divyaranjan/emacs-reader"
    ;;       	:files ("*.el" "render-core.so")
    ;;       	:pre-build ("make" "all"))
    biblio
    citar
    (oxr :host github :repo "bdarcus/oxr")
    titlecase
    denote
    denote-menu
    consult-denote
    denote-journal
    denote-org
    denote-sequence
    citar-embark
    citar-denote
    denote-explore
    ox-epub
    auctex
    cdlatex
    (consult-reftex :host github :repo "karthink/consult-reftex")))

(defvar *package-ai-install-list*
  (append '((gptel :host github
                   :repo "karthink/gptel")
            gptel-magit
            (ragmacs :host github :repo "positron-solutions/ragmacs")
            (gptel-agent :host github :repo "karthink/gptel-agent"
                         :files (:defaults "agents"))
            (gptel-aibo :host github
                        :repo "dolmens/gptel-aibo")
            (gptel-quick :host github
                         :repo "karthink/gptel-quick")
            (mcp :host github
                 :repo "lizqwerscott/mcp.el")
            (macher :host github
                    :repo "kmontag/macher"))
          (when user/aider
            '((aidermacs :host github
                         :repo "MatthewZMD/aidermacs")))))

(defvar *package-toolkit-install-list*
  '((thing-edit :host github :repo "manateelazycat/thing-edit")
    (delete-block :host github :repo "manateelazycat/delete-block")
    (move-text :host github :repo "manateelazycat/move-text")
    (open-newline :host github :repo "manateelazycat/open-newline")
    (duplicate-line :host github :repo "manateelazycat/duplicate-line")
    (markmacro :host github :repo "manateelazycat/markmacro")
    dwim-shell-command))

(defvar *package-another-install-list*
  '(elfeed
    elfeed-tube
    elfeed-tube-mpv
    code-stats
    docker
    (screenshot :host github :repo "tecosaur/screenshot")
    (consult-omni :type git :host github :repo "armindarvish/consult-omni" :branch "main" :files (:defaults "sources/*.el"))))

(setq vterm-always-compile-module t)
(packages!
 '(eat
   vterm
   (meow-vterm :host github :repo "accelbread/meow-vterm")
   (multi-vterm :host github :repo "lizqwerscott/multi-vterm")))

(defun site-lisp-update ()
  "Update site-lisp packages."
  (interactive)
  (let ((output-buffer (get-buffer-create "*Update site lisp*")))
    (async-shell-command
     (concat "cd "
             user-emacs-directory
             " && git submodule foreach git pull")
     output-buffer)
    (switch-to-buffer-other-window output-buffer)))


(defun emacs-update ()
  "Update Emacs all packages."
  (interactive)
  (site-lisp-update)
  (straight-pull-recipe-repositories)
  (straight-pull-all))

(packages!
 (append *package-build-in-install-list*
         *package-base-install-list*
         *package-tool-install-list*
         *package-language-mode-install-list*
         *package-edit-install-list*
         *package-program-install-list*
         *package-ui-install-list*
         *package-mms-install-list*
         *package-window-install-list*
         *package-language-install-list*
         *package-org-install-list*
         *package-write-install-list*
         *package-ai-install-list*
         *package-toolkit-install-list*
         *package-another-install-list*))

(provide 'init-packages)
;;; init-packages.el ends here

(defvar *package-base-install-list*
  '(gcmh
    vertico
    marginalia
    consult
    consult-notes
    consult-yasnippet
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
    ag
    rg
    xclip
    helpful
    which-key
    hydra
    pretty-hydra))

(defvar *package-tool-install-list*
  '(try
    alert
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
    dirvish
    trashed
    elisp-demos
    (lazy-revert :host github :repo "yilin-zhang/lazy-revert")
    (psearch
     :host github
     :repo "twlz0ne/psearch.el"
     :files ("psearch.el"))
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
     :files (:defaults "*.el" "*.py" "backend" "core" "icons"))))

(defvar *package-language-mode-install-list*
  '(markdown-mode
    log4j-mode
    just-mode
    yaml-mode
    go-mode
    haskell-mode
    elvish-mode
    git-modes
    csv-mode))

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
    (fingertip :host github :repo "manateelazycat/fingertip")))

(defvar *package-program-install-list*
  `(dumb-jump
    yasnippet
    macrostep
    ,@(pcase user/lsp-client
        ('eglot
         '(eglot
           (eglot-booster :host github :repo "jdtsmith/eglot-booster")
           consult-eglot))
        ('lsp-bridge
         `((lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                       :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                       :build (:not compile))
           ,@(when (not (display-graphic-p))
               '((popon :fetcher git :url "https://codeberg.org/akib/emacs-popon.git")
                 (acm-terminal :fetcher git :url "https://github.com/twlz0ne/acm-terminal.git"))))))
    corfu
    cape
    nerd-icons-corfu
    eldoc-box
    flymake-popon
    magit
    magit-delta
    forge
    difftastic
    devdocs
    wucuo
    projection
    eshell-prompt-extras
    esh-help
    fish-completion
    dape
    citre
    (xmake :host github :repo "lizqwerscott/xmake-emacs")
    (quicktype :host github :repo "artawower/quicktype.el")
    (color-rg :host github
              :repo "manateelazycat/color-rg")
    (peek :host sourcehut :repo "meow_king/peek")

    (auto-save :host github :repo "manateelazycat/auto-save")
    super-save
    (image-slicing :host github :repo "ginqi7/image-slicing")))

(defvar *package-ui-install-list*
  '(solarized-theme
    (koishi-theme :host github :repo "gynamics/koishi-theme.el")
    nerd-icons
    nerd-icons-dired
    nerd-icons-completion
    nerd-icons-ibuffer
    page-break-lines
    ligature
    dashboard
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
    olivetti
    redacted
    hl-todo
    imenu-list
    outshine
    (indent-bars :host github :repo "jdtsmith/indent-bars")
    (sort-tab :host github
              :repo "manateelazycat/sort-tab")
    (awesome-tray :host github
                  :repo "manateelazycat/awesome-tray")
    (breadcrumb :host github
                :repo "joaotavora/breadcrumb")
    (highlight-matching-tag :host github :repo "manateelazycat/highlight-matching-tag")
    buffer-name-relative
    (prism :host github :repo "alphapapa/prism.el")
    casual
    casual-symbol-overlay))

(defvar *package-window-install-list*
  '(shackle
    popper
    ace-window
    (watch-other-window :host github :repo "manateelazycat/watch-other-window")
    ))

(defvar *package-language-install-list*
  '(immersive-translate
    (sdcv
     :host github
     :repo "manateelazycat/sdcv")
    fanyi
    go-translate
    rime
    pyim
    pyim-basedict
    (pyim-tsinghua-dict
     :host github
     :repo "redguardtoo/pyim-tsinghua-dict"
     :files ("pyim-tsinghua-dict.el" "pyim-tsinghua-dict.pyim"))
    ))

(defvar *package-org-install-list*
  '(org-bullets
    org-fancy-priorities
    org-roam
    org-roam-ui
    ox-reveal
    ox-hugo
    org-appear
    org-journal
    valign
    (pangu-spacing
     :host github
     :repo  "nailuoGG/pangu-spacing"
     :branch "remove-old-version-support"
     :files ("*.el"))

    (org-modern-indent :host github :repo "jdtsmith/org-modern-indent")
    pdf-tools
    org-ref
    (org-media-note :host github :repo "yuchen-lea/org-media-note")
    denote
    consult-denote
    denote-menu))

(defvar *package-ai-install-list*
  '((copilot :host github
             :repo "zerolfx/copilot.el"
             :branch "main"
             :files ("dist" "*.el"))
    (gptel :host github
           :repo "karthink/gptel")
    (gptel-quick :host github
                 :repo "karthink/gptel-quick")
    (mcp :host github
         :repo "lizqwerscott/mcp.el")
    (gptel-aibo :host github
                :repo "dolmens/gptel-aibo")
    (aidermacs :host github
               :repo "MatthewZMD/aidermacs")
    (codeium :host github :repo "Exafunction/codeium.el")
    (codeium-overlay :host github
                     :repo "liyingzhi/codeium-overlay.el")
    (minuet :host github
            :repo "milanglacier/minuet-ai.el")
    ))

(defvar *package-rust-install-list*
  '(rust-mode
    cargo))

(defvar *package-common-lisp-install-list*
  '(common-lisp-snippets
    sly
    sly-quicklisp
    sly-asdf
    ))

(defvar *package-scheme-install-list*
  '(geiser
    geiser-guile))

(defvar *package-web-install-list*
  '(web-mode
    pnpm-mode
    ))

(defvar *package-python-install-list*
  '(conda
    pyvenv))

(defvar *package-zig-install-list*
  '(zig-mode
    zig-ts-mode))

(defvar *package-unity-install-list*
  '((unity :host github :repo "elizagamedev/unity.el")))

(defvar *package-sql-install-list*
  '(sql-indent))

(defvar *package-toolkit-install-list*
  '((thing-edit :host github :repo "manateelazycat/thing-edit")
    (delete-block :host github :repo "manateelazycat/delete-block")
    (move-text :host github :repo "manateelazycat/move-text")
    (open-newline :host github :repo "manateelazycat/open-newline")
    (duplicate-line :host github :repo "manateelazycat/duplicate-line")
    (markmacro :host github :repo "manateelazycat/markmacro")))

(defvar *package-another-install-list*
  '(elfeed
    code-stats
    ;; tabspaces
    docker
    (screenshot :host github :repo "tecosaur/screenshot")
    (telega-url-shorten-nerd :host github
                             :repo "lizqwerscott/telega-url-shorten-nerd")
    (telega :host github
            :repo "zevlg/telega.el"
            :branch "master"
            :files (:defaults "contrib" "etc" "server" "Makefile"))
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
  (straight-pull-all))

(packages! *package-base-install-list*)
(packages! *package-tool-install-list*)
(packages! *package-language-mode-install-list*)
(packages! *package-edit-install-list*)
(packages! *package-program-install-list*)
(packages! *package-ui-install-list*)
(packages! *package-window-install-list*)
(packages! *package-language-install-list*)
(packages! *package-org-install-list*)
(packages! *package-ai-install-list*)
(packages! *package-rust-install-list*)
(packages! *package-common-lisp-install-list*)
(packages! *package-web-install-list*)
(packages! *package-python-install-list*)
(packages! *package-zig-install-list*)
(packages! *package-unity-install-list*)
(packages! *package-sql-install-list*)
(packages! *package-toolkit-install-list*)
(packages! *package-another-install-list*)

(provide 'init-packages)

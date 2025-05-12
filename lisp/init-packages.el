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
     :fetcher github
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
    vterm
    (lazy-revert :fetcher github :repo "yilin-zhang/lazy-revert")
    (psearch
     :fetcher github
     :repo "twlz0ne/psearch.el"
     :files ("psearch.el"))
    (p-search :repo "zkry/p-search" :fetcher github)
    heap
    (rsync-project-mode
     :fetcher github
     :repo "lizqwerscott/rsync-project-mode")
    gif-screencast
    keycast
    cal-china-x
    consult-gh
    consult-gh-forge))

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
  '((meow :fetcher github :repo "meow-edit/meow")
    meow-tree-sitter
    (repeat-fu :fetcher codeberg :repo "ideasman42/emacs-repeat-fu")
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
    (fingertip :fetcher github :repo "manateelazycat/fingertip")
    (meow-vterm :fetcher github :repo "accelbread/meow-vterm")))

(defvar *package-program-install-list*
  '(dumb-jump
    yasnippet
    macrostep
    eglot
    consult-eglot
    (eglot-booster :fetcher github :repo "jdtsmith/eglot-booster")
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
    fish-completion
    dape
    citre
    (xmake :fetcher github :repo "lizqwerscott/xmake-emacs")
    (quicktype :fetcher github :repo "artawower/quicktype.el")
    (color-rg :fetcher github
              :repo "manateelazycat/color-rg")
    (peek :fetcher sourcehut :repo "meow_king/peek")

    (auto-save :fetcher github :repo "manateelazycat/auto-save")
    super-save
    (image-slicing :fetcher github :repo "ginqi7/image-slicing")))

(defvar *package-ui-install-list*
  '(solarized-theme
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
    (indent-bars :fetcher github :repo "jdtsmith/indent-bars")
    (sort-tab :fetcher github
              :repo "manateelazycat/sort-tab")
    (awesome-tray :fetcher github
                  :repo "manateelazycat/awesome-tray")
    (breadcrumb :fetcher github
                :repo "joaotavora/breadcrumb")
    (highlight-matching-tag :fetcher github :repo "manateelazycat/highlight-matching-tag")
    buffer-name-relative
    (prism :fetcher github :repo "alphapapa/prism.el")
    casual
    casual-symbol-overlay))

(defvar *package-window-install-list*
  '(shackle
    popper
    ace-window
    (watch-other-window :fetcher github :repo "manateelazycat/watch-other-window")
    ))

(defvar *package-language-install-list*
  '(immersive-translate
    (sdcv
     :fetcher github
     :repo "manateelazycat/sdcv")
    fanyi
    go-translate
    rime
    pyim
    pyim-basedict
    (pyim-tsinghua-dict
     :fetcher github
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
     :fetcher github
     :repo  "nailuoGG/pangu-spacing"
     :branch "remove-old-version-support"
     :files ("*.el"))

    (org-modern-indent :fetcher github :repo "jdtsmith/org-modern-indent")
    pdf-tools
    org-ref
    (org-media-note :fetcher github :repo "yuchen-lea/org-media-note")
    denote
    consult-denote
    denote-menu))

(defvar *package-ai-install-list*
  '((copilot :fetcher github
             :repo "zerolfx/copilot.el"
             :branch "main"
             :files ("dist" "*.el"))
    (gptel :fetcher github
           :repo "karthink/gptel")
    (gptel-quick :fetcher github
                 :repo "karthink/gptel-quick")
    (mcp :fetcher github
         :repo "lizqwerscott/mcp.el")
    (gptel-mcp :fetcher github
               :repo "lizqwerscott/gptel-mcp.el")
    (gptel-aibo :fetcher github
                :repo "dolmens/gptel-aibo")
    (aidermacs :fetcher github
               :repo "MatthewZMD/aidermacs")
    (codeium :fetcher github :repo "Exafunction/codeium.el")
    (codeium-overlay :fetcher github
                     :repo "liyingzhi/codeium-overlay.el")
    (minuet :fetcher github
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
  '((unity :fetcher github :repo "elizagamedev/unity.el")))

(defvar *package-sql-install-list*
  '(sql-indent))

(defvar *package-toolkit-install-list*
  '((thing-edit :fetcher github :repo "manateelazycat/thing-edit")
    (delete-block :fetcher github :repo "manateelazycat/delete-block")
    (move-text :fetcher github :repo "manateelazycat/move-text")
    (open-newline :fetcher github :repo "manateelazycat/open-newline")
    (duplicate-line :fetcher github :repo "manateelazycat/duplicate-line")
    (markmacro :fetcher github :repo "manateelazycat/markmacro")))

(defvar *package-another-install-list*
  '(elfeed
    code-stats
    ;; tabspaces
    docker
    (screenshot :fetcher github :repo "tecosaur/screenshot")
    (telega-url-shorten-nerd :fetcher github
                             :repo "lizqwerscott/telega-url-shorten-nerd")
    (telega :fetcher github
            :repo "zevlg/telega.el"
            :branch "master"
            :files (:defaults "contrib" "etc" "server" "Makefile"))))

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

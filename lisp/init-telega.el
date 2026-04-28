;;; init-telega.el --- telega                        -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(wait-packages!
 '((telega :host github
           :repo "zevlg/telega.el"
           :branch "master")
   (telega-url-shorten-nerd :host github
                            :repo "lizqwerscott/telega-url-shorten-nerd")))

(setq telega-server-libs-prefix user/telega-tdlib-path)

(setq telega-mnz-languages
      '((ada . ada-mode)
        (awk . awk-mode)
        (c . c-ts-mode)
        (clojure . clojure-mode)
        (cpp . c++-ts-mode)
        (csharp . csharp-ts-mode)
        (scheme . scheme-mode)              ; not in language-detection
        (css . css-mode)
        (dart . dart-mode)
        (delphi . delphi-mode)
        (diff . diff-mode)                  ; not in language-detection
        (emacslisp . emacs-lisp-mode)
        (erlang . erlang-mode)
        (fortran . fortran-mode)
        (go . go-ts-mode)
        (groovy . groovy-mode)
        (haskell . haskell-mode)
        (html . html-mode)
        (java . java-mode)
        (javascript . javascript-mode)
        (json . json-mode)
        (kotlin . kotlin-mode)              ; not in language-detection
        (latex . latex-mode)
        (lisp . lisp-mode)
        (lua . lua-mode)
        (matlab . matlab-mode)
        (objc . objc-mode)
        (octave . octave-mode)              ; not in language-detection
        (org . org-mode)                    ; not in language-detection
        (outline . outline-mode)            ; not in language-detection
        (perl . perl-mode)
        (php . php-mode)
        (prolog . prolog-mode)
        (python . python-ts-mode)
        (r . ess-r-mode)
        (ruby . ruby-mode)
        (rust . rust-ts-mode)
        (scala . scala-mode)
        (shell . bash-ts-mode)
        (smalltalk . smalltalk-mode)
        (sml . sml-mode)
        (sql . sql-mode)
        (swift . swift-mode)
        (visualbasic . visual-basic-mode)
        (xml . xml-mode)
        (zig . zig-mode)))

;;; Config

(with-eval-after-load 'telega
  (require 'telega-url-shorten-nerd)
  (require 'telega-mnz)
  (require 'telega-chat)
  (require 'lib-telega)
  (require 'telega-emoji)
  (telega-emoji-init)

  (with-eval-after-load 'meow
    (add-to-list 'meow-mode-state-list '(telega-root-mode . motion)))

  (setopt telega-chat-show-avatars t
          telega-emoji-use-images nil
          telega-sticker-animated-play t
          telega-auto-translate-probe-language-codes nil
          telega-translate-to-language-by-default "zh-CN"
          telega-chat-input-markups (list "org" "markdown2"))

  ;;Reference: https://github.com/LuciusChen/.emacs.d/blob/main/lisp/init-social.el
  (setopt
   ;; 以下都是 telega-symbols-emojify 中的 telega-symbol
   ;; telega-symbol
   ;; remove iterm from `telega-symbols-emojify`
   telega-symbols-emojify
   (cl-reduce (lambda (emojify key)
                (assq-delete-all key emojify))
              '(verified vertical-bar checkmark forum heavy-checkmark reply reply-quote horizontal-bar forward button-close summarize-in summarize-out)
              :initial-value telega-symbols-emojify)
   telega-symbol-button-close (nerd-icons-mdicon "nf-md-close_box_outline")
   telega-symbol-verified (nerd-icons-codicon "nf-cod-verified_filled" :face 'telega-blue)
   telega-symbol-vertical-bar "│" ;; U+2502 Box Drawings Light Vertical
   telega-symbol-saved-messages-tag-end (nerd-icons-faicon "nf-fa-tag")
   telega-symbol-forum (nerd-icons-mdicon "nf-md-format_list_text")
   telega-symbol-flames (nerd-icons-mdicon "nf-md-delete_clock")
   telega-symbol-mark (propertize " " 'face 'telega-button-highlight)
   telega-symbol-reply (nerd-icons-faicon "nf-fa-reply")
   telega-symbol-reply-quote (nerd-icons-faicon "nf-fa-reply_all")
   telega-symbol-forward (nerd-icons-faicon "nf-fa-mail_forward")
   telega-symbol-checkmark (nerd-icons-mdicon "nf-md-check")
   telega-symbol-heavy-checkmark (nerd-icons-codicon "nf-cod-check_all")
   telega-symbol-summarize-in (nerd-icons-octicon "nf-oct-fold")
   telega-symbol-summarize-out (nerd-icons-octicon "nf-oct-unfold"))

  ;; 替代两行头像，防止头像因为字符高度不统一裂开。
  ;; (setopt  telega-avatar-workaround-gaps-for (when (display-graphic-p) '(return t)))
  (setf (alist-get 2 telega-avatar-factors-alist ) '(0.5 . 0.1))

  (custom-theme-set-faces 'user
                          `(telega-msg-heading ((t (:background nil)))))


  (setopt telega-box-button-styles (let ((styles (copy-tree telega-box-button-styles)))
                                     (setf (alist-get 'admin-sender-tag styles)
                                           '(:inherit owner-sender-tag
                                                      :passive-face telega-shadow))
                                     styles))

  (when sys/macp
    (setq telega-server-libs-prefix "/opt/homebrew/"))

  ;;; keymap
  (keymap-unset telega-msg-button-map "l")
  (keymap-unset telega-msg-button-map "k")
  (keymap-binds telega-msg-button-map
    ("SPC" . meow-keypad))

  (keymap-binds telega-prefix-map
    ("p" . telega-chatbuf-filter-search)
    ("d" . telega-chat-remove-member)
    ("m" . telega-describe-chat-members)
    ("h" . telega-notifications-history)
    ("x" . telega-chatbuf-thread-cancel))

  (defalias 'telega-prefix-map telega-prefix-map)

  (global-set-keys
   '(("C-c t" . ("Telega" . telega-prefix-map))

     ("C-c l t" . ("Chat Tab" . tab-bar-switch-or-create-chat))))

  ;;; notification
  (add-hook 'telega-connection-state-hook #'+tab-bar-telega-icon-update)
  (add-hook 'telega-kill-hook #'+tab-bar-telega-icon-update)

  (advice-add #'telega--on-updateUnreadChatCount :after #'+tab-bar-telega-icon-update)
  (advice-add #'telega--on-updateChatUnreadMentionCount :after #'+tab-bar-telega-icon-update)
  (advice-add #'telega--on-updateChatUnreadReactionCount :after #'+tab-bar-telega-icon-update)
  (advice-add #'telega-msg-observable-p :after  #'+tab-bar-telega-icon-update)

  (add-hook 'telega-chat-mode-hook #'my/telega-chat-capf)
  (add-hook 'telega-chat-mode-hook #'mode-line-invisible-mode)

  (unless sys/macp
    (telega-notifications-mode t))

  (global-telega-url-shorten-nerd-mode))

(when (and user/telega-start (display-graphic-p))
  (add-hook 'window-setup-hook
            (lambda ()
              (message "start telega")
              (autoload '+lizqwer/toggle-telega "lib-telega" nil t)
              (+lizqwer/toggle-telega))))

(provide 'init-telega)
;;; init-telega.el ends here

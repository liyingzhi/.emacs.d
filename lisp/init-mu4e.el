;;; init-mu4e.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Reference: https://github.com/LuciusChen/.emacs.d/blob/main/lisp/init-mu4e.el

;; brew install mu isync msmtp
;; mkdir -p ~/.maildir/qq ~/.maildir/gmail
;;
;; <<< receve mail settings >>>
;;
;; security add-generic-password -s mu4e-gmail -a xxxx@gmail.com -w
;;
;; from keychain Access - System Roots export root-certificates.pem
;;
;; file --> .mbsyncrc ↓
;;
;; IMAPAccount gmail
;; Host imap.gmail.com
;; User xxxx@gmail.com
;; PassCmd "security find-generic-password -s mu4e-gmail -a xxxx@gmail.com -w"
;; Port 993
;; SSLType IMAPS
;; SSLVersions TLSv1.2
;; AuthMechs PLAIN
;; SystemCertificates no
;; CertificateFile ~/.maildir/certificates/root-certificates.pem

;; IMAPStore gmail-remote
;; Account gmail

;; MaildirStore gmail-local
;; SubFolders Verbatim
;; Path ~/.maildir/gmail/
;; Inbox ~/.maildir/gmail/INBOX

;; Channel gmail
;; Far :gmail-remote:
;; Near :gmail-local:
;; Patterns *
;; Create Near
;; Sync All
;; Expunge Both
;; SyncState *
;;
;; mbsync -aV                                                            <──────────────┐
;; mu init -m ~/.maildir --my-address xxxx@163.com --my-address xxxx@gmail.com          │
;; mu index                                                                             │
;;                                                                                      │
;; <<< remarks >>> -->                                                                  │
;; qq 需要先开启 imap                                                                   │
;; 删除数据重新部署需要删除 Dashboard 中 database-path 位置的数据库。                   │
;; cd .cache && rm -rf mu ──────────────────────────────────────────────────────────────┘

;; <<< send mail settings >>>
;;
;; file --> .msmtprc ↓
;;
;; defaults
;; logfile ~/.maildir/msmtp.log
;; tls_trust_file ~/.maildir/certificates/root-certificates.pem

;; account gmail
;; auth on
;; host smtp.gmail.com
;; port 465
;; protocol smtp
;; from xxxx@gmail.com
;; user xxxx@gmail.com
;; passwordeval security find-generic-password -s mu4e-gmail -a xxxx@gmail.com -w
;; tls on
;; tls_starttls off
;;
;; remarks -->
;; mkdir -p ~/Mail/queued-mail && touch ~/Mail/queued-mail/index

(require 'mu4e)

;; for sending mails
(require 'smtpmail)
(setopt mu4e-mu-binary (executable-find "mu")
        ;; this command is called to sync imap servers:
        mu4e-get-mail-command (concat (executable-find "mbsync") " -a")
        ;; how often to call it in seconds:
        mu4e-update-interval 300
        ;; save attachment to desktop by default
        ;; or another choice of yours:
        mu4e-attachment-dir "~/Downloads"
        ;; rename files when moving - needed for mbsync:
        mu4e-change-filenames-when-moving t
        ;; don't keep message compose buffers around after sending:
        message-kill-buffer-on-exit t
        ;; send function:
        send-mail-function 'message-send-mail-with-sendmail
        message-send-mail-function 'message-send-mail-with-sendmail
        ;; send program:
        ;; this is exeranal. remember we installed it before.
        sendmail-program (executable-find "msmtp")
        ;; select the right sender email from the context.
        message-sendmail-envelope-from 'header
        mu4e-use-fancy-chars t
        ;; start with the first (default) context;
        mu4e-context-policy 'pick-first
        ;; ask for context if no context matches;
        mu4e-compose-context-policy 'ask)
;; header view formatting
(setq mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶")
      mu4e-headers-thread-orphan-prefix '("┬>" . "┬▶ ")
      mu4e-headers-thread-connection-prefix '("│ " . "│ ")
      mu4e-headers-thread-first-child-prefix '("├>" . "├▶")
      mu4e-headers-thread-child-prefix '("├>" . "├▶")
      mu4e-headers-thread-last-child-prefix '("└>" . "╰▶")
      mu4e-headers-unread-mark    '("u" . "📩 ")
      mu4e-headers-draft-mark     '("D" . "🚧 ")
      mu4e-headers-flagged-mark   '("F" . "🚩 ")
      mu4e-headers-new-mark       '("N" . "✨ ")
      mu4e-headers-passed-mark    '("P" . "↪ ")
      mu4e-headers-replied-mark   '("R" . "↩ ")
      mu4e-headers-seen-mark      '("S" . " ")
      mu4e-headers-trashed-mark   '("T" . "🗑️ ")
      mu4e-headers-attach-mark    '("a" . "📎 ")
      mu4e-headers-encrypted-mark '("x" . "🔑 ")
      mu4e-headers-signed-mark    '("s" . "🖊 ")
      mu4e-headers-list-mark      '("l" . "🔈 ")
      mu4e-headers-personal-mark  '("p" . "👨 ")
      mu4e-headers-calendar-mark  '("c" . "📅 ")
      mu4e-contexts
      `(,(make-mu4e-context
          :name "gmail"
          :enter-func
          (lambda () (mu4e-message "Enter liyingli2018@gmail.com context"))
          :leave-func
          (lambda () (mu4e-message "Leave liyingli2018@gmail.com context"))
          :match-func
          (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches msg
                                                  :to "liyingli2018@gmail.com")))
          :vars '((user-mail-address . "liyingli2018@gmail.com")
                  (user-full-name . "Fly lilee")
                  (mu4e-drafts-folder . "/gmail/Drafts")
                  (mu4e-refile-folder . "/gmail/Archive")
                  (mu4e-sent-folder . "/gmail/Sent")
                  (mu4e-trash-folder . "/gmail/Trash")))
        ,(make-mu4e-context
          :name "qq"
          :enter-func
          (lambda () (mu4e-message "Enter sd_liyingli@foxmail.com context"))
          :leave-func
          (lambda () (mu4e-message "Leave sd_liyingli@foxmail.com context"))
          :match-func
          (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches msg
                                                  :to "sd_liyingli@foxmail.com")))
          :vars '((user-mail-address . "sd_liyingli@foxmail.com" )
                  (user-full-name . "Fly lilee")
                  (mu4e-drafts-folder . "/qq/Drafts")
                  (mu4e-refile-folder . "/qq/Archive")
                  (mu4e-sent-folder . "/qq/Sent Messages")
                  (mu4e-trash-folder . "/qq/Deleted Messages")))))

(add-to-list 'display-buffer-alist
             '("\\*mu4e-update\\*"
               (display-buffer-below-selected)
               (window-height . 0.1)))
;; chose from account before sending
;; this is a custom function that works for me.
;; well I stole it somewhere long ago.
;; I suggest using it to make matters easy
;; of course adjust the email adresses and account descriptions
(defun +set-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field "from")))
             (account
              (cond
               ((string-match "liyingli2018@gmail.com" from) "gmail")
               ((string-match "sd_liyingli@foxmail.com" from) "qq"))))

          (setq message-sendmail-extra-arguments (list '"-a" account))))))

(add-hook 'message-send-mail-hook '+set-msmtp-account)

;; Automatically add Cc & Bcc headers in mu4e compose mode.
(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (save-excursion (message-add-header "Cc:\n"))
            (save-excursion (message-add-header "Bcc:\n"))))

;; mu4e address completion
;; (add-hook 'mu4e-compose-mode-hook 'company-mode)

(provide 'init-mu4e)
;;; init-mu4e.el ends here

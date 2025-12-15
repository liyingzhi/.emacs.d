;;; init-org-capture.el --- init org capture         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org-capture)

(defun lizqwer/setup-org-capture ()
  (setq org-capture-templates nil)

  (push '("I" "我的闪念" entry (file+headline "~/Documents/Org/inbox.org" "闪念") "* %U - %^{标题} %^g\n  %?\n")
        org-capture-templates)
  ;; (push `("I" "我的闪念" entry (file "~/Documents/Org/idea.org") ,(concat "* TODO %?\n%U"))
  ;;       org-capture-templates)

  (push '("i" "Interrupting task" entry
          (file "~/Documents/Org/interrupted-tasks.org")
          "* STARTED %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n"
          :clock-in t :clock-resume t
          :prepend t)
        org-capture-templates)

  (push '("Q" "收藏名言" entry (file+headline "~/Documents/Org/quote.org" "名言") "* %U - %^{标题} %^g\n  %?\n")
        org-capture-templates)
  ;; (push '("n" "LNKS" entry (file+headline "~/Documents/Org/lnks.org" "链接") "* [[%^{link-url}][%^{link-description}]] %^g\n:PROPERTIES:\n:LINK-CREATE-TIME: %T\n:END:\n  %?\n")
  ;;       org-capture-templates)
  (push  '("n" "note" entry (file "~/Documents/Org/note.org") "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
         org-capture-templates)
  (push '("T" "阅读记录" entry (file+headline "~/Documents/Org/quote.org" "阅读记录") "* TODO %^{标题} %t %^g\n** \_From: %f\_
\=File-Line:\= %l \n\=Description:\= %?\n") org-capture-templates)
  (push '("t" "任务" entry (file+headline "~/Documents/Org/tasks.org" "任务") "* TODO %^{标题} %^g\nSCHEDULED: %^t DEADLINE: %^t \n  %?\n") org-capture-templates)
  (push '("w" "工作任务" entry (file+headline "~/Documents/Org/tasks.org" "工作任务") "* TODO %^{任务名} :work:\nSCHEDULED: %^t DEADLINE: %^t\n  %?\n" ) org-capture-templates))

(lizqwer/setup-org-capture)

(provide 'init-org-capture)
;;; init-org-capture.el ends here

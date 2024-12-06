(require 'org-capture)

(defun lizqwer/setup-org-capture ()
  (setq org-capture-templates nil)
  (push '("i" "我的闪念" entry (file+headline "~/Documents/Org/idea.org" "闪念") "* %U - %^{标题} %^g\n  %?\n")
        org-capture-templates)
  (push '("q" "收藏名言" entry (file+headline "~/Documents/Org/quote.org" "名言") "* %U - %^{标题} %^g\n  %?\n")
        org-capture-templates)
  (push '("n" "LNKS" entry (file+headline "~/Documents/Org/lnks.org" "链接") "* [[%^{link-url}][%^{link-description}]] %^g\n:PROPERTIES:\n:LINK-CREATE-TIME: %T\n:END:\n  %?\n")
        org-capture-templates)
  (push '("t" "任务" entry (file+headline "~/Documents/Org/tasks.org" "任务") "* TODO %^{标题} %t %^g\n** \_From: %f\_
\=Flie-Line:\= %l \n\=Description:\= %?\n") org-capture-templates))

(defun +evan/setup-org-capture ()
  (setq org-capture-templates nil)
  (push '("j" "我的日志" entry (file+headline"~/Blog/org/diary.org" "日志") "* %U - %^{标题}\n  %?") org-capture-templates)
  (push '("i" "我的闪念" entry (file+headline "~/Blog/org/idea.org" "闪念") "* %U - %^{标题} %^g\n  %?\n") org-capture-templates)
  (push '("k" "我的百科" entry (file+headline "~/Blog/org/wiki.org" "WIKI") "* %^{标题} %t %^g\n  %?\n") org-capture-templates)
  (push '("t" "任务" entry (file+headline "~/Org/todo.org" "任务") "* TODO %^{标题} %t %^g\n  %?\n") org-capture-templates)
  (push '("n" "LNKS" entry (file+headline "~/Org/lnks.org" "任务") "* TODO %^{标题} %t\n  %?\n") org-capture-templates)
  (push '("b" "笔记" entry (file+headline "~/Org/note.org" "笔记") "* %^{标题} %t\n  %?\n") org-capture-templates)
  )

(lizqwer/setup-org-capture)

(provide 'init-org-capture)

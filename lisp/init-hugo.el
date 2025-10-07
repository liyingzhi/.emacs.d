(with-eval-after-load 'ox
  (require 'ox-hugo)
  (setq org-hugo-base-dir
        (file-truename "~/MyProject/website/blog/")
        org-hugo-front-matter-format "yaml"
        org-hugo-auto-set-lastmod t))

(provide 'init-hugo)

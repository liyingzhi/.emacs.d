;;; init-elfeed.el --- elfeed setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(wait-packages!
 '(elfeed
   elfeed-tube
   elfeed-tube-mpv
   elfeed-org))

(defconst user/elfeed-org-files "~/Documents/Org/elfeed.org"
  "My elfeed org files path.")

(with-eval-after-load 'elfeed
  ;; Load elfeed-org
  (require 'elfeed-org)

  ;; Initialize elfeed-org
  ;; This hooks up elfeed-org to read the configuration when elfeed
  ;; is started with =M-x elfeed=
  (elfeed-org)

  ;; Optionally specify a number of files containing elfeed
  ;; configuration. If not set then the location below is used.
  ;; Note: The customize interface is also supported.
  (setopt rmh-elfeed-org-files (list user/elfeed-org-files))

  (setopt url-queue-timeout 30)

  (add-hook 'elfeed-show-mode-hook #'visual-line-mode)

  (require 'lib-elfeed)
  (setq elfeed-search-print-entry-function #'+elfeed-search-print-entry--better-default)

  ;; Reference: https://taxodium.ink/use-elfeed-to-read-feed-in-emacs.html
  (defun spike-leung/org-open-rss-feed-as-site-in-elfeed-org-files (orig-fun &rest args)
    "Advice for `org-open-at-point' to redirect RSS links only in a specific file."
    (let* ((element (org-element-context))
           (link (and (eq (org-element-type element) 'link)
                      (org-element-property :raw-link element))))
      (if (and buffer-file-name
               (string-equal (expand-file-name (buffer-file-name))
                             (expand-file-name user/elfeed-org-files))
               link
               (string-match-p (rx (or "rss" "feed" "atom" "xml")) link))
          (let* ((url-parts (url-generic-parse-url link))
                 (scheme (url-type url-parts))
                 (host (url-host url-parts))
                 (site-url (concat scheme "://" host)))
            (message "Opening site for feed: %s" site-url)
            (browse-url site-url))
        (apply orig-fun args))))

  (advice-add 'org-open-at-point :around #'spike-leung/org-open-rss-feed-as-site-in-elfeed-org-files)


  (defun spike-leung/get-feed-candidates (&optional level)
    "Extract headings title from `rmh-elfeed-org-files' as consult candidates.
If LEVEL exist, filter heading which level is greater or equal LEVEL."
    (mapcan
     (lambda (elfeed-org-file)
       (with-current-buffer (or (find-buffer-visiting elfeed-org-file)
                                (find-file-noselect elfeed-org-file))
         (delq nil
               (org-element-map (org-element-parse-buffer 'headline) 'headline
                 (lambda (hl)
                   ;; property 的值可以在这里找： https://orgmode.org/worg/dev/org-element-api.html
                   (when (or (null level) (>= (org-element-property :level hl) level))
                     (let* ((raw-title (org-element-property :raw-value hl))
                            (title (org-link-display-format raw-title))
                            (annotation (org-entry-get hl "description"))
                            (feed-url (when (string-match org-link-bracket-re raw-title)
                                        (match-string 1 raw-title))))
                       (list :items (list title) :feed-url feed-url :annotation annotation))))
                 nil))))
     rmh-elfeed-org-files))

  (defun spike-leung/elfeed-preview-state (state candidate)
    "Return consult state function for live `elfeed' preview.
See `consult--with-preview' about STATE and CANDIDATE."
    (unless (null candidate)
      (let* ((cand (car candidate))
             (metadata (cdr candidate))
             (feed-url (plist-get metadata :feed-url)))
        (pcase state
          ('setup
           (unless (get-buffer "*elfeed-search*")
             (elfeed-apply-hooks-now)
             (elfeed-org)
             (elfeed)
             (elfeed-search-clear-filter))
           (display-buffer "*elfeed-search*" '(display-buffer-reuse-window)))
          ('preview
           (elfeed-search-clear-filter)
           (when (and cand (get-buffer "*elfeed-search*"))
             (unless (string-empty-p cand)
               (elfeed-search-set-filter (concat elfeed-search-filter " =" (string-replace " " "." cand))))))
          ('return
           (unless (string-empty-p cand)
             (elfeed-search-set-filter (concat elfeed-search-filter " =" (string-replace " " "." cand)))
             (elfeed-update-feed feed-url)))))))

  (defun spike-leung/consult-elfeed ()
    "Select feed from `rmh-elfeed-org-files' with live preview in `elfeed'."
    (interactive)
    (let* ((candidates (spike-leung/get-feed-candidates 3)))
      (consult--multi candidates
                      :prompt "Feed: "
                      :state #'spike-leung/elfeed-preview-state
                      :history 'spike-leung/consult-elfeed-history
                      :annotate (lambda (cand)
                                  (let* ((match-cand (seq-find
                                                      (lambda (v)
                                                        (string-match-p (car (plist-get v :items)) cand))
                                                      candidates))
                                         (annotation (and match-cand (plist-get match-cand :annotation))))
                                    (when annotation
                                      (concat (make-string 25 ?\s) annotation)))))
      (when (get-buffer "*elfeed-search*")
        (pop-to-buffer "*elfeed-search*"))))

  (keymap-binds elfeed-search-mode-map
    (("/") . spike-leung/consult-elfeed))

  (require 'elfeed-tube)
  (setopt elfeed-tube-backend 'yt-dlp)
  (setopt elfeed-tube-captions-languages
          '("zh" "en" "english (auto generated)")
          ;; mpv-default-options '("--http-proxy=http://127.0.0.1:7897"
          ;;                       "--ytdl-raw-options-append=proxy=http://127.0.0.1:7897")
          )
  (elfeed-tube-setup))

(autoload #'tab-bar-switch-or-create-rss "lib-elfeed" nil t)

(global-bind-keys
 ("C-c l r" . ("Rss Tab" . tab-bar-switch-or-create-rss)))

(with-eval-after-load 'elfeed-show
  (keymap-sets elfeed-show-mode-map
    '(("C-c C-o" . +open-link-with-mpv)
      ("C-c C-f" . elfeed-tube-fetch)
      ("C-c C-l" . elfeed-tube-mpv-follow-mode)
      ("C-c C-w" . elfeed-tube-mpv-where)
      ("C-c C-m" . elfeed-tube-mpv)
      ("C-x C-s" . elfeed-tube-save))))

(with-eval-after-load 'elfeed-search
  (keymap-sets elfeed-search-mode-map
    '(("l" . +elfeed-overview)
      ("F" . elfeed-tube-fetch)
      ("C-x C-s" . elfeed-tube-save))))

(provide 'init-elfeed)
;;; init-elfeed.el ends here

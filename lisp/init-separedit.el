(require 'separedit)
(keymap-set prog-mode-map "C-c '" #'separedit)

(setq separedit-default-mode 'org-mode)
(setq separedit-remove-trailing-spaces-in-comment t)
(setq separedit-continue-fill-column t)
(setq separedit-buffer-creation-hook #'auto-fill-mode)

(defun separedit/edit-org-src-block ()
  "Edit org src code block."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (when-let* ((datum (when (org-in-src-block-p) (org-element-context)))
                  (content-info (org-src--contents-area datum)))
        (lexical-let*
            ((ov (when (string-empty-p (nth 2 content-info))
                   (make-overlay (nth 1 content-info)
                                 (nth 1 content-info) nil nil t)))
             (ad (when ov
                   (lambda ()
                     (with-current-buffer (overlay-buffer ov)
                       (save-excursion
                         (goto-char (overlay-end ov))
                         (delete-overlay ov)
                         (unless (= (point) (point-at-bol))
                           (insert "\n")))))))
             (buf (separedit
                   (separedit-mark-region (nth 0 content-info)
                                          (nth 1 content-info)
                                          (org-src-get-lang-mode
                                           (plist-get (cadr datum) :language))))))
          (when ad
            (with-current-buffer buf
              (setq-local kill-buffer-hook (append (list ad) kill-buffer-hook))))))
    (message "The current buffer major mode is not derived from org-mode!")))

(defun separedit/edit-org-any-block ()
  "Edit org src code block."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (when-let* ((datum (org-element-context))
                  (content-info (org-src--contents-area datum)))
        (lexical-let*
            ((ov (when (string-empty-p (nth 2 content-info))
                   (make-overlay (nth 1 content-info)
                                 (nth 1 content-info) nil nil t)))
             (ad (when ov
                   (lambda ()
                     (with-current-buffer (overlay-buffer ov)
                       (save-excursion
                         (goto-char (overlay-end ov))
                         (delete-overlay ov)
                         (unless (= (point) (point-at-bol))
                           (insert "\n")))))))
             (buf (separedit
                   (separedit-mark-region (nth 0 content-info)
                                          (nth 1 content-info)))))
          (when ad
            (with-current-buffer buf
              (setq-local kill-buffer-hook (append (list ad) kill-buffer-hook))))))
    (message "The current buffer major mode is not derived from org-mode!")))

(defun org-in-quote-block-p ()
  "判断当前光标是否位于 QUOTE 块内。"
  (let ((case-fold-search t))
    (save-excursion
      (and (re-search-backward "^#\\+BEGIN_QUOTE" nil t)
         (re-search-forward "^#\\+END_QUOTE" nil t)
         (<= (point) (point))))))  ; 确保光标在 END_QUOTE 之前
(defun separedit/edit-org-quote-block ()
  "Edit org src code block."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (when-let* ((datum (when (org-in-quote-block-p) (org-element-context)))
                  (content-info (org-src--contents-area datum)))
        (lexical-let*
            ((ov (when (string-empty-p (nth 2 content-info))
                   (make-overlay (nth 1 content-info)
                                 (nth 1 content-info) nil nil t)))
             (ad (when ov
                   (lambda ()
                     (with-current-buffer (overlay-buffer ov)
                       (save-excursion
                         (goto-char (overlay-end ov))
                         (delete-overlay ov)
                         (unless (= (point) (point-at-bol))
                           (insert "\n")))))))
             (buf (separedit
                   (separedit-mark-region (nth 0 content-info)
                                          (nth 1 content-info)))))
          (when ad
            (with-current-buffer buf
              (setq-local kill-buffer-hook (append (list ad) kill-buffer-hook))))))
    (message "The current buffer major mode is not derived from org-mode!")))

(provide 'init-separedit)

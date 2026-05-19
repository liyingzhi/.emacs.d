;;; meow-key-enhanced.el --- meow-key-enhanced       -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun help-helpful-lsp-sly ()
  "Help function with lsp and sly info."
  (interactive)
  (if (bound-and-true-p sly-mode)
      (call-interactively #'sly-documentation)
    (if (or (equal major-mode 'emacs-lisp-mode)
            (equal major-mode 'lisp-interaction-mode))
        (helpful-at-point)
      (pcase user/lsp-client
        ('eglot
         (eldoc-box-help-at-point))
        ('lsp-bridge
         (if (bound-and-true-p lsp-bridge-mode)
             (lsp-bridge-popup-documentation)
           (message "dont't know how to help")))))))

(defun meow-next-enhance ()
  "Enhanced next line command.
Uses `scroll-up-one-line' when available and `user/move-style-motion' is nil,
otherwise falls back to `meow-next'."
  (interactive)
  (if (and (fboundp #'scroll-up-one-line) (not user/move-style-motion))
      (call-interactively #'scroll-up-one-line)
    (call-interactively #'meow-next)))

(defun meow-prev-enhance ()
  "Enhanced previous line command.
Uses `scroll-down-one-line' when available and `user/move-style-motion' is nil,
otherwise falls back to `meow-prev'."
  (interactive)
  (if (and (fboundp #'scroll-up-one-line) (not user/move-style-motion))
      (call-interactively #'scroll-down-one-line)
    (call-interactively #'meow-prev)))

(defun meow-normal-n-key-enhance ()
  "Enhanced n-key command for meow normal state.
In telega chat mode, goes to next message;
otherwise falls back to `meow-search'."
  (interactive)
  (if (eq major-mode 'telega-chat-mode)
      (call-interactively #'telega-msg-next)
    (call-interactively #'meow-search)))

(defun meow-normal-p-key-enhance ()
  "Enhanced p-key command for meow normal state.
In telega chat mode, goes to previous message;
otherwise falls back to `meow-yank'."
  (interactive)
  (if (eq major-mode 'telega-chat-mode)
      (call-interactively #'telega-msg-previous)
    (call-interactively #'meow-yank)))

(defun my/meow-quit ()
  "Meow quit."
  (interactive)
  (if (match-in #'(lambda (regex)
                    (buffer-match-p (if (symbolp regex)
                                        (cons 'derived-mode regex)
                                      regex)
                                    (buffer-name)))
                popper-reference-buffers)
      (popper--delete-popup (selected-window))
    (if (equal major-mode 'blink-search-mode)
        (blink-search-quit)
      (if (bound-and-true-p citre-peek--mode)
          (citre-peek-abort)
        ;; 检查非 popper 窗口数量
        (let* ((all-windows (window-list (selected-frame)))
               (non-popper-windows
                (seq-filter (lambda (win)
                              (let ((buf (window-buffer win)))
                                (not (match-in (lambda (regex)
                                                 (buffer-match-p (if (symbolp regex)
                                                                     (cons 'derived-mode regex)
                                                                   regex)
                                                                 (buffer-name buf)))
                                               popper-reference-buffers))))
                            all-windows)))
          (if (> (length non-popper-windows) 1)
              (delete-window)
            (meow-quit)))))))

(defun meow-tree-sitter-block ()
  "Tree-sitter version of `meow-block'.
Will automatically fall back to `meow-block' if no treesit parser is
available."
  (interactive)
  (let ((p (point))
        (m (mark))
        (node (treesit-node-at (point))))
    (if (or (equal major-mode 'emacs-lisp-mode)
            (equal major-mode 'lisp-mode)
            (equal major-mode 'lisp-data-mode)
            (equal major-mode 'lisp-interaction-mode)
            (null node))
        ;; No tree-sitter data available.
        (call-interactively #'meow-block)
      ;; If the region is active, expand the node until it contains both point
      ;; and mark. "Contain" requires that the node not be at the boundaries,
      ;; which is how consistent expansion is guaranteed.
      ;;
      ;; NOTE: this is a bit hacky and inconsistent, might be better to refactor
      ;; in the future.
      (when (region-active-p)
        (setq node (treesit-parent-until
                    node (lambda (n)
                           (or (< (treesit-node-start n)
                                  p
                                  (treesit-node-end n))
                               (< (treesit-node-start n)
                                  m
                                  (treesit-node-end n)))))))
      (thread-first
        (meow--make-selection '(expand . block)
                              (treesit-node-start node)
                              (treesit-node-end node))
        (meow--select t)))))

(provide 'meow-key-enhanced)
;;; meow-key-enhanced.el ends here

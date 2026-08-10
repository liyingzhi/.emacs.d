;;; init-denote.el --- denote                        -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'denote)
(require 'consult-denote)

(setq denote-directory "~/Documents/denote")
;; (setq denote-directory '("~/Documents/denote" "/mnt/data/shareData/"))

(setq denote-dired-directories denote-directory)
(add-to-list 'denote-prompts 'subdirectory)

(setq consult-denote-grep-command #'consult-ripgrep)
(setq consult-denote-find-command #'consult-fd)

(setq denote-date-prompt-use-org-read-date t)

(setq denote-org-store-link-to-heading 'context)

;; Remove group-function to avoid completion grouping being affected by completion-category-overrides.
(setq denote-file-prompt-extra-metadata
      (assq-delete-all 'group-function denote-file-prompt-extra-metadata))

(defun create-denote--in-work-subdir ()
  (interactive)
  (let ((denote-directory (concat denote-directory "/work")))
    (denote-org-capture)))

(defun create-denote--in-robot-subdir ()
  (interactive)
  (let ((denote-directory (concat denote-directory "/robot")))
    (denote-org-capture)))

(defun create-denote--in-personal-subdir ()
  (interactive)
  (let ((denote-directory (concat denote-directory "/personal")))
    (denote-org-capture)))

(push '("W" "New work note (with Denote)" plain
        (file denote-last-path)
        #'create-denote--in-work-subdir
        :no-save t
        :immediate-finish nil
        :kill-buffer t
        :jump-to-captured t) org-capture-templates)

(push '("r" "New robot note (with Denote)" plain
        (file denote-last-path)
        #'create-denote--in-robot-subdir
        :no-save t
        :immediate-finish nil
        :kill-buffer t
        :jump-to-captured t) org-capture-templates)

(push '("p" "New personal note (with Denote)" plain
        (file denote-last-path)
        #'create-denote--in-personal-subdir
        :no-save t
        :immediate-finish nil
        :kill-buffer t
        :jump-to-captured t) org-capture-templates)

;; (push '("j" "Journal" entry
;;         (file denote-journal-extras-path-to-new-or-existing-entry)
;;         "* %U %?\n%i\n%a"
;;         :kill-buffer t
;;         :empty-lines 1) org-capture-templates)

(defun my-denote-create-note-in-any-directory ()
  "Create new Denote note in any directory.
Prompt for the directory using minibuffer completion."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-directory (read-directory-name "New note in: " nil nil :must-match)))
    (call-interactively 'denote)))

;;; denote-review
(defvar-keymap denote-review-keymap
  :doc "Denote review keymap"
  :prefix t
  "s" #'denote-review-set-date
  "l" #'denote-review-display-list)

(global-set-keys
 '(("C-c n r" . ("Denote Review" . denote-review-keymap))))

;;; denote-project-notes
(require 'denote-project-notes)

;;; denote-wordcloud
(autoload #'denote-wordcloud-list-by-frequency "denote-wordcloud" nil t)

(defvar-keymap denote-wordcloud-keymap
  :doc "Denote wordcloud keymap"
  :prefix t
  "c" #'denote-wordcloud
  "f" #'denote-wordcloud-list-by-frequency)

(global-set-keys
 '(("C-c n w" . ("Denote Wordcloud" . denote-wordcloud-keymap))))

;;; denote-solo
(setopt denote-solo--last-directory-file
        (expand-file-name "var/denote-solo-last-directory"
                          user-emacs-directory))
(denote-solo-mode 1)

;;; denote graph

(setopt graph-fa2-engine '2d
        graph-fa2-framerate 60.0)

(defun denote-graph-fa2-open-note (id)
  "Open the Denote file corresponding to ID when clicked."
  (when-let* ((file (car (denote-directory-files id))))
    (find-file file)))

(defun denote-graph-fa2-network ()
  "Generate and display a ForceAtlas2 graph of the Denote network."
  (interactive)
  (let* ((files (denote-directory-files nil nil t))
         (nodes (mapcar (lambda (file)
                          (let ((id (denote-retrieve-filename-identifier file))
                                (type (denote-filetype-heuristics file)))
                            (list :id id
                                  :label (denote-retrieve-title-or-filename file type)
                                  :colour "#89b4fa"
                                  :radius 8.0)))
                        files))
         (edges nil)
         (buf (get-buffer-create "*denote-graph-fa2*")))

    (let ((links-xref (xref-matches-in-files (concat "denote:" denote-id-regexp) files)))
      (dolist (match links-xref)
        (let* ((loc (xref-match-item-location match))
               (source-file (xref-location-group loc))
               (source-id (denote-retrieve-filename-identifier source-file))
               (summary (xref-match-item-summary match)))
          (when (string-match denote-id-regexp summary)
            (let ((target-id (match-string 0 summary)))
              (push (cons source-id target-id) edges))))))

    (with-current-buffer buf
      (special-mode)
      (add-hook 'graph-fa2-node-clicked-functions #'denote-graph-fa2-open-note nil t))

    (pop-to-buffer buf)
    (graph-fa2-start buf nodes edges)))

(defun denote-link-graph--neighbors (file all-files)
  "Return Denote files adjacent to FILE.

Combines outgoing links (FILE links to them) and backlinks (they link to FILE).

ALL-FILES is a list of Denote files to search for linked targets; it is passed
to `denote-get-links' to avoid re-scanning the directory."
  (let ((result nil)
        (seen (make-hash-table :test #'equal)))
    (dolist (nf (append (denote-get-links file all-files)
                        (denote-get-backlinks file)))
      (let ((nid (denote-retrieve-filename-identifier nf)))
        (when (and nid (not (gethash nid seen)))
          (puthash nid t seen)
          (push nf result))))
    result))
(defun denote-graph-current-network (arg)
  "Display graph of Denote nodes connected to the node at point.

The focal node is the Denote link at point, if any, otherwise the
current Denote file.  With numeric prefix ARG, show nodes up to ARG
hops away (default 1: only directly connected nodes)."
  (interactive "p")
  (unless (and buffer-file-name
               (denote-file-has-identifier-p buffer-file-name))
    (user-error "Current buffer is not a Denote file"))
  (let* ((all-files (denote-directory-files nil nil nil nil :has-identifier))
         (file-by-id (make-hash-table :test #'equal))
         (focus-id (or (denote-get-link-identifier-or-query-term-at-point)
                       (denote-retrieve-filename-identifier buffer-file-name)))
         (depth (or arg 1)))
    (dolist (file all-files)
      (puthash (denote-retrieve-filename-identifier file) file file-by-id))
    (unless (gethash focus-id file-by-id)
      (user-error "Focus node `%s' not found in denote directory" focus-id))
    (let* ((visited (make-hash-table :test #'equal))
           (reachable (list focus-id))
           (edges nil)
           (edge-set (make-hash-table :test #'equal))
           (queue (list (cons focus-id 0))))
      (puthash focus-id t visited)
      (while queue
        (let* ((elem (pop queue))
               (current (car elem))
               (d (cdr elem))
               (current-file (gethash current file-by-id)))
          (dolist (nf (denote-link-graph--neighbors current-file all-files))
            (let ((nid (denote-retrieve-filename-identifier nf)))
              (when (and nid (gethash nid visited))
                ;; 记录可达节点之间的边，统一方向避免重复
                (let ((edge (if (string< current nid)
                                (cons current nid)
                              (cons nid current))))
                  (unless (gethash edge edge-set)
                    (puthash edge t edge-set)
                    (push edge edges))))
              (when (and nid (< d depth) (not (gethash nid visited)))
                (puthash nid t visited)
                (push nid reachable)
                (setq queue (append queue (list (cons nid (1+ d))))))))))
      (let* ((degree (make-hash-table :test #'equal))
             (_ (dolist (e edges)
                  (let ((src (car e)) (tgt (cdr e)))
                    (puthash src (1+ (gethash src degree 0)) degree)
                    (puthash tgt (1+ (gethash tgt degree 0)) degree))))
             (max-deg (max 1
                           (apply #'max 0
                                  (mapcar (lambda (id) (gethash id degree 0))
                                          reachable))))
             (nodes (mapcar
                     (lambda (id)
                       (let* ((file (gethash id file-by-id))
                              (deg (gethash id degree 0))
                              ;; 将度数归一化映射到半径范围 [8, 20]
                              (r (+ 8.0 (* 12.0 (/ (float deg) max-deg)))))
                         (list :id id
                               :label (denote-retrieve-title-or-filename
                                       file (denote-filetype-heuristics file))
                               :colour (if (string= id focus-id)
                                           "#f9e2af"
                                         "#89b4fa")
                               :radius r)))
                     reachable))
             (buf (get-buffer-create "*denote-link-graph*")))
        (with-current-buffer buf
          (special-mode)
          (add-hook 'graph-fa2-node-clicked-functions #'denote-graph-fa2-open-note nil t))
        (pop-to-buffer buf)
        (graph-fa2-start buf nodes edges)))))

(with-eval-after-load 'graph-fa2
  (keymap-binds graph-fa2-mode-map
    ("=" . graph-fa2-zoom-in)))

(provide 'init-denote)
;;; init-denote.el ends here

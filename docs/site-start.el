(defun set-home-dir (dir)
  (setenv "HOME" dir)
  (message (format "HOME location is %s" (getenv "HOME"))))

(set-home-dir "D:/EmacsConfig")

;;; init-timezones.el ---                            -*- lexical-binding: t; -*-

;;; casual-time-zone
;;fix casual-timezone-planner function use current-time-zone to get time zone  abbr ambiguity
(defun my/get-iana-via-timedatectl ()
  "Get the IANA timezone identifier via `timedatectl' command.
This function executes the system `timedatectl' command to query
the current timezone setting and extracts the IANA timezone
identifier (e.g. 'America/New_York').

Returns the timezone string if successful, nil otherwise."
  (let ((res (string-trim (shell-command-to-string "timedatectl show --property=Timezone"))))
    (when (and (string-match "^Timezone=" res))
      (substring res (length "Timezone=")))))

(defun my/current-time-zone-filter (res &rest _args)
  "Filter function for `current-time-zone' to fix timezone ambiguity.
This function is used as an advice filter for `current-time-zone'
to replace potentially ambiguous timezone abbreviations with the
full IANA timezone identifier from systemd's timedatectl.

RES is the original return value from `current-time-zone'.
_ARGS are ignored.

Returns the modified timezone information if RES is valid and
contains (seconds west daylight-saving-time zone-name), otherwise
returns RES unchanged."
  (if (and (listp res)
           (integerp (car res)))
      (let ((iana (my/get-iana-via-timedatectl)))
        (when iana
          (setf (cadr res) iana)
          res))
    res))

(with-eval-after-load 'casual-timezone-utils
  (advice-add 'current-time-zone :filter-return #'my/current-time-zone-filter)
  (setq casual-timezone-working-hours-range '((:start . 6)(:stop . 22))))

;;; time-zones
(with-eval-after-load 'time-zones
  (setq time-zones--city-list-file (expand-file-name "var/.time-zones.el" user-emacs-directory)))

(provide 'init-timezones)
;;; init-timezones.el ends here

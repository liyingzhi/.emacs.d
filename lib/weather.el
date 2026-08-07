;;; weather.el --- weather                           -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: tools

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

;; On-demand weather via Open-Meteo.  No background timer: callers use
;; `weather-ensure-fresh' when opening or refreshing a UI.  Cache TTL is
;; `weather-cache-ttl' (default 900 seconds).  Public API: `weather-info'
;; (format cached data) and `weather-ensure-fresh' (fetch if stale).

;;; Code:

(require 'json)
(require 'url)
(require 'nerd-icons)

(defgroup weather ()
  "Weather info."
  :group 'tools)

(defvar weather-temperature nil)
(defvar weather-description nil)
(defvar weather-icon nil)
(defvar weather-last-fetch-time nil
  "Time of the last successful weather fetch.")

(defvar weather--fetching nil
  "Non-nil while a weather request is in flight.")

(defvar weather--pending-callback nil
  "Callback to run after the in-flight weather request finishes.")

(defcustom weather-latitude nil
  "Latitude for weather information."
  :group 'weather
  :type 'float)

(defcustom weather-longitude nil
  "Longitude for weather information in weather package."
  :group 'weather
  :type 'float)

(defcustom weather-cache-ttl 900
  "Seconds before cached weather data is considered stale."
  :group 'weather
  :type 'natnum)

(defface weather-text-info-face
  '((t :inherit default :height 0.9 :bold nil))
  "Face added to code-usage display."
  :group 'weather)

(defface weather-description-face
  '((t :foreground "#E2943B" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for weather description."
  :group 'weather)

(defface weather-icon-face
  '((t :height 0.9))
  "Face for weather icon."
  :group 'weather)

(defface weather-temperature-face
  '((t :foreground "#f38ba8" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for temperature."
  :group 'weather)

(defun weather--icon-from-code (code)
  "Map weather CODE to a corresponding string."
  (nerd-icons-wicon
   (pcase code
     (`0 "nf-weather-day_sunny")
     ((or `1 `2 `3) "nf-weather-cloudy")
     ((or `45 `48) "nf-weather-fog")
     ((or `51 `53 `55) "nf-weather-sleet")
     ((or `56 `57) "nf-weather-snow")
     ((or `61 `63 `65) "nf-weather-day_rain_mix")
     ((or `66 `67) "nf-weather-rain_mix")
     ((or `71 `73 `75) "nf-weather-snow")
     (`77 "nf-weather-snow")
     ((or `80 `81 `82) "nf-weather-rain")
     ((or `85 `86) "nf-weather-rain_mix")
     ((or `95 `96 `99) "nf-weather-thunderstorm")
     (_ "Unknown"))))

(defun weather--code-to-string (code)
  "Map weather CODE to a corresponding string."
  (pcase code
    (`0 "Clear sky")
    ((or `1 `2 `3) "Partly cloudy")
    ((or `45 `48) "Fog")
    ((or `51 `53 `55) "Drizzle")
    ((or `56 `57) "Freezing drizzle")
    ((or `61 `63 `65) "Rain")
    ((or `66 `67) "Freezing rain")
    ((or `71 `73 `75) "Snowfall")
    (`77 "Snow grains")
    ((or `80 `81 `82) "Rain showers")
    ((or `85 `86) "Snow showers")
    ((or `95 `96 `99) "Thunderstorm")
    (_ "Unknown")))

(defun weather--configured-p ()
  "Return non-nil if latitude and longitude are configured."
  (and (floatp weather-latitude) (floatp weather-longitude)
       (> weather-latitude 0.0) (> weather-longitude 0.0)))

(defun weather--fresh-p ()
  "Return non-nil if cached weather data is still within TTL."
  (and weather-last-fetch-time
       (< (float-time (time-since weather-last-fetch-time))
          weather-cache-ttl)))

(defun weather--fetch ()
  "Fetch weather data from Open-Meteo asynchronously."
  (setq weather--fetching t)
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&current_weather=true"
                     weather-latitude weather-longitude)))
    (url-retrieve
     url
     (lambda (status)
       (unwind-protect
           (if-let* ((err (plist-get status :error)))
               (progn
                 (setq weather--pending-callback nil)
                 (message "Weather fetch error: %S" err))
             (goto-char (point-min))
             (re-search-forward "^$")
             (let* ((json-data (buffer-substring-no-properties (point) (point-max)))
                    (json-obj (json-read-from-string json-data)))
               (let-alist json-obj
                 (setq weather-temperature
                       (format "%.1f" .current_weather.temperature)
                       weather-description
                       (format "%s" (weather--code-to-string
                                     .current_weather.weathercode))
                       weather-icon
                       (weather--icon-from-code .current_weather.weathercode)
                       weather-last-fetch-time (current-time)))
               (when-let* ((cb weather--pending-callback))
                 (setq weather--pending-callback nil)
                 (funcall cb))))
         (setq weather--fetching nil)
         (when (buffer-live-p (current-buffer))
           (kill-buffer (current-buffer)))))
     nil
     t)))

(defun weather-ensure-fresh (&optional callback)
  "Ensure weather cache is fresh, then call CALLBACK.

If coordinates are not configured, do nothing.  If the cache is still
fresh, call CALLBACK immediately.  Otherwise start an asynchronous
fetch and call CALLBACK after a successful update.  Concurrent calls
while a fetch is in flight replace the pending callback."
  (when (weather--configured-p)
    (cond
     ((weather--fresh-p)
      (when callback
        (funcall callback)))
     (weather--fetching
      (setq weather--pending-callback callback))
     (t
      (setq weather--pending-callback callback)
      (weather--fetch)))))

(defun weather-info ()
  "Get weather info."
  (when (weather--configured-p)
    (if weather-description
        (format "%s %s, %s%s"
                weather-icon
                (propertize weather-description 'face 'weather-description-face)
                (propertize weather-temperature 'face 'weather-temperature-face)
                (propertize "℃" 'face 'weather-text-info-face))
      (propertize "Loading weather data..." 'face 'weather-temperature-face))))

(provide 'weather)
;;; weather.el ends here

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

;;

;;; Code:

(require 'json)
(require 'url)
(require 'nerd-icons)

(defvar weather-temperature nil)
(defvar weather-description nil)
(defvar weather-icon nil)

(defcustom weather-latitude nil
  "Latitude for weather information."
  :group 'panel
  :type 'float)

(defcustom weather-longitude nil
  "Longitude for weather information in panel package."
  :group 'panel
  :type 'float)

(defface weather-description-face
  '((t :foreground "#E2943B" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for weather description."
  :group 'panel)

(defface weather-icon-face
  '((t :height 0.9))
  "Face for weather icon."
  :group 'panel)

(defface weather-temperature-face
  '((t :foreground "#f38ba8" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for temperature."
  :group 'panel)

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
     ((or `66 `67) "nf-weather-rain-mix")
     ((or `71 `73 `75) "nf-weather-snow")
     (`77 "nf-weather-snow")
     ((or `80 `81 `82) "nf-weather-rain")
     ((or `85 `86) "nf-weather-rain-mix")
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

(defun weather--roi-window-is-active (roi-buffer)
  "Check if ROI-BUFFER is the currently active and visible window."
  (or (eq roi-buffer (window-buffer (selected-window)))
      (get-buffer-window roi-buffer 'visible)))

(defun weather-fetch-weather-data (&optional initial fn roi-buffer-name)
  "Fetch weather data from Open-Meteo API.
INITIAL indicates if this is the first fetch.
FN is a callback function to execute after fetching weather info.
ROI-BUFFER-NAME is the buffer name to check for visibility before calling FN."
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&current_weather=true"
                     weather-latitude weather-longitude)))
    (url-retrieve url
                  (lambda (_)
                    (goto-char (point-min))
                    (re-search-forward "^$")
                    (let* ((json-data (buffer-substring-no-properties (point) (point-max)))
                           (json-obj (json-read-from-string json-data)))
                      (let-alist json-obj
                        (setq weather-temperature (format "%.1f" .current_weather.temperature))
                        (setq weather-description
                              (format "%s" (weather--code-to-string .current_weather.weathercode)))
                        (setq weather-icon
                              (weather--icon-from-code .current_weather.weathercode)))
                      ;; Only set up the recurring timer after initial fetch
                      (when initial
                        (run-with-timer 900 900 #'weather-fetch-weather-data))
                      (when fn
                        (when (weather--roi-window-is-active roi-buffer-name)
                          (funcall fn)))))
                  nil
                  t)))

(defun weather--show-weather-info ()
  "Check if we have latitude and longitude to show weather info."
  (and (floatp weather-latitude) (floatp weather-longitude)
       (> weather-latitude 0.0) (> weather-longitude 0.0)))

(defun weather-info ()
  "Get weather info."
  (when (weather--show-weather-info)
    (if weather-description
        (format "%s %s, %s%s"
                weather-icon
                (propertize weather-description 'face 'weather-description-face)
                (propertize weather-temperature 'face 'weather-temperature-face)
                (propertize "℃" 'face 'weather-text-info-face))
      (propertize "Loading weather data..." 'face 'weather-temperature-face))))

(provide 'weather)
;;; weather.el ends here

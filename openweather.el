;;; openweather.el --- Weather data from OpenWeather in Emacs

;; Copyright (C) 2020 Marian Piatkowski <marianpiatkowski@web.de>

;; Author: Marian Piatkowski <marianpiatkowski@web.de>
;; URL: https://github.com/marianpiatkowski/openweather-el
;; Keywords: comm

;; This file is NOT part of GNU Emacs.

;; openweather-el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; openweather-el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with openweather-el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See https://openweathermap.org/api/one-call-api

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'url-cache)
(require 'json)
(require 'calendar)

(defgroup openweather nil
  "Weather data from OpenWeather in Emacs."
  :group 'tools)

(defun openweather--get-default-location-name ()
  "Find default location name."
  (if (boundp 'user-location-name)
      user-location-name
    (if (boundp 'calendar-location-name)
        calendar-location-name
      "")))

(defcustom openweather-location-name
  (openweather--get-default-location-name)
  "Name of the default weather location.
See `openweather-location-latitude', `openweather-location-longitude', and
`openweather-location-msl'."
  :group 'openweather
  :type 'string)

(defun openweather--get-default-location-latitude ()
  "Find default location latitude."
  (if (boundp 'user-location-latitude)
      user-location-latitude
    (if (and (boundp 'calendar-latitude)
             (or (numberp calendar-latitude)
                 (vectorp calendar-latitude))
             (require 'solar nil t))
        (calendar-latitude)
      0))) ;; TODO better default?

(defcustom openweather-location-latitude
  (openweather--get-default-location-latitude)
  "Latitude of `openweather-location-name' in degrees.
See `openweather-location-longitude' and `openweather-location-msl'."
  :group 'openweather
  :type '(number :tag "Exact"))

(defun openweather--get-default-location-longitude ()
  "Find default location latitude."
  (if (boundp 'user-location-longitude)
      user-location-longitude
    (if (and (boundp 'calendar-longitude)
             (or (numberp calendar-longitude)
                 (vectorp calendar-longitude))
             (require 'solar nil t))
        (calendar-longitude)
      0))) ;; TODO better default?

(defcustom openweather-location-longitude
  (openweather--get-default-location-longitude)
  "Longitude of `openweather-location-name' in degrees.
See `openweather-location-latitude' and `openweather-location-msl'."
  :group 'openweather
  :type '(number :tag "Exact"))

(defcustom openweather-location-msl nil
  "Whole meters above sea level of `openweather-location-name' in degrees.
See `openweather-location-latitude' and `openweather-location-msl'."
  :group 'openweather
  :type '(choice (const nil)
                 (number :tag "Exact")))

(defcustom openweather-buffer-name "*OpenWeather*"
  "Name for the OpenWeather buffer."
  :group 'openweather
  :type 'string)

(defface openweather-header
  '((t :inherit header-line))
  "Face for top header line."
  :group 'openweather)

(defface openweather-date
  '((t :inherit header-line))
  "Face for date line."
  :group 'openweather)

(defun openweather--insert (face &rest args)
  "Insert ARGS into current buffer with FACE."
  (insert (propertize (apply 'concat args) 'face face)))

(defcustom openweather-appid ""
  "You can get an APPID by logging into your OpenWeather account.
You should get it by logging-in to your account and settings this variable."
  :group 'openweather
  :type 'string)

(defcustom openweather-units 'metric
  "The unit type to use for measurements."
  :group 'openweather
  :type '(radio (const :tag "Metric (C)" metric)
                (const :tag "Imperial (F)" imperial)))

(defun openweather-kill-buffer ()
  (interactive)
  (kill-buffer openweather-buffer-name))

(defvar openweather-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'openweather-kill-buffer)
    (define-key map "g" 'openweather-update)
    map)
  "Keymap for `openweather-mode'.")

(eval-when-compile (require 'easymenu))
(easy-menu-define openweather-mode openweather-mode-map
  "Menu for OpenWeather forecast."
  '("Weather"
    ["Update" openweather-update
     :help "Fetch new data from OpenWeather"]
    ["Quit" openweather-kill-buffer
     :help "Quit"]))

(define-derived-mode openweather-mode special-mode
  "OpenWeather"
  "Major mode for showing weather forecasts.

\\{openweather-mode-map}"
  :group 'openweather)

(defvar openweather--data nil
  "Store OpenWeather data from `json-read'.")

(defun openweather-make-url (lat lon units &optional msl)
  "Create the URL from LAT and LON in UNITS to be used by `openweather-update'.
Requires your OpenWeatherMap AppID."
  (format "https://api.openweathermap.org/data/2.5/onecall?lat=%s&lon=%s&appid=%s&mode=json&units=%s&cnt=5"
          lat lon openweather-appid (url-encode-url (symbol-name units))))

(defun openweather-temperature-unit ()
  "Return the symbol appropriate for the current value of openweather units."
  (cond ((equal openweather-units 'imperial) "°F")
        ((equal openweather-units 'metric) "℃")))

(defun openweather-velocity-unit ()
  "Return the symbol appropriate for the current value of openweather units."
  (cond ((equal openweather-units 'imperial) "mph")
        ((equal openweather-units 'metric) "m/s")))

(defun openweather--calculate-time-difference (time1 time2)
  "Calculate time1 - time2."
  (format-seconds "%Y, %D, %H, %M, %z%S" (time-to-seconds (time-subtract time1 time2))))

(defun openweather--calculate-wind_dir (value)
  "Calculate wind direction from degrees."
  (cond ((or (>= value 337.5) (<= value 22.5))
         (string ?N))
        ((and (>= value 22.5) (<= value 67.5))
         (string ?N ?E))
        ((and (>= value 67.5) (<= value 112.5))
         (string ?E))
        ((and (>= value 112.5) (<= value 157.5))
         (string ?S ?E))
        ((and (>= value 157.5) (<= value 202.5))
         (string ?S))
        ((and (>= value 202.5) (<= value 247.5))
         (string ?S ?W))
        ((and (>= value 247.5) (<= value 292.5))
         (string ?W))
        ((and (>= value 292.5) (<= value 337.5))
         (string ?N ?W))))

;;; ======== formatting functions for current forecast ========

(defun openweather--format-current--dt (value)
  "Format time of current forecast."
  (let ((d (decode-time (seconds-to-time value))))
    (openweather--insert 'font-lock-function-name-face
                         (format "%02d:%02d\n" (nth 2 d) (nth 1 d)))))

(defun openweather--format-current--sunrise (value)
  "Format time of current sunrise."
  (let ((d (decode-time (seconds-to-time value))))
    (openweather--insert 'font-lock-keyword-face
                         (format "*** Sunrise %02d:%02d:%02d\n" (nth 2 d) (nth 1 d) (nth 0 d)))))

(defun openweather--format-current--sunset (value)
  "Format time of current sunset."
  (let ((d (decode-time (seconds-to-time value))))
    (openweather--insert 'font-lock-keyword-face
                         (format "*** Sunset %02d:%02d:%02d\n" (nth 2 d) (nth 1 d) (nth 0 d)))))

(defun openweather--format-current--temp (value)
  "Format temperature in current forecast."
  (let ((temp-symbol (openweather-temperature-unit)))
    (openweather--insert 'font-lock-keyword-face
                         (concat "*** Temperature "
                                 (format "%s%s\n" value temp-symbol)))))

(defun openweather--format-current--feels_like (value)
  "Format 'feels like' temperature in current forecast."
  (let ((temp-symbol (openweather-temperature-unit)))
    (openweather--insert 'font-lock-keyword-face
                         (concat "*** Feels like "
                                 (format "%s%s\n" value temp-symbol)))))

(defun openweather--format-current--pressure (value)
  "Format pressure in current forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Pressure %shPa\n" value)))

(defun openweather--format-current--humidity (value)
  "Format humidity in current forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Humidity %s%%\n" value)))

(defun openweather--format-current--dew_point (value)
  "Format dewpoint temperature in current forecast."
  (let ((temp-symbol (openweather-temperature-unit)))
    (openweather--insert 'font-lock-keyword-face
                         (concat "*** Dewpoint temperature "
                                 (format "%s%s\n" value temp-symbol)))))

(defun openweather--format-current--uvi (value)
  "Format UV index in current forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** UV index %s\n" value)))

(defun openweather--format-current--clouds (value)
  "Format cloudiness in current forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Clouds %s%%\n" value)))

(defun openweather--format-current--visibility (value)
  "Format average visibility in current forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Visibility (avg) %sm\n" value)))

(defun openweather--format-current--wind_speed (value)
  "Format wind speed in current forecast."
  (let ((velo-symbol (openweather-velocity-unit)))
    (openweather--insert 'font-lock-keyword-face
                         (concat "*** Wind speed "
                                 (format "%s%s\n" value velo-symbol)))))

(defun openweather--format-current--wind_gust (value)
  "Format wind gust in current forecast."
  (let ((velo-symbol (openweather-velocity-unit)))
    (openweather--insert 'font-lock-keyword-face
                         (concat "*** Wind gust "
                                 (format "%s%s\n" value velo-symbol)))))

(defun openweather--format-current--wind_deg (value)
  "Format wind direction in current forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Wind direction %s° (%s)\n"
                               value
                               (openweather--calculate-wind_dir value))))

(defun openweather--format-current--weather (attributes)
  "Format weather summary in current forecast."
  ;; extract single element from json array
  (let ((attrs (elt attributes 0)))
    (openweather--insert 'font-lock-keyword-face
                         (format "*** Conditions %s %s\n"
                                 (cdr (assoc 'description attrs))
                                 (cdr (assoc 'icon attrs))))))

(defun openweather--format-current--rain (attribute)
  "Format rain volume in current forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Rain volume for last %s %smm\n"
                               (car (assoc '1h attribute))
                               (cdr (assoc '1h attribute)))))

(defun openweather--format-current--snow (attribute)
  "Format snow volume in current forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Snow volume for last %s %smm\n"
                               (car (assoc '1h attribute))
                               (cdr (assoc '1h attribute)))))

;;; ======== formatting functions for minutely forecast ========

(defun openweather--format-minutely--dt (value)
  "Format time in minutely forecast."
  (let ((d (decode-time (seconds-to-time value))))
    (openweather--insert 'font-lock-keyword-face
                         (format " %02d:%02d " (nth 2 d) (nth 1 d)))))

(defun openweather--format-minutely--precipitation (value)
  "Format precipitation in minutely forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "%0.3fmm" value)))

;;; ======== formatting functions for hourly forecast ========

(defun openweather--format-hourly--dt (value)
  "Format time in hourly forecast."
  (openweather--insert 'font-lock-function-name-face
                       (format-time-string "%A %F %H:%M\n" value)))

(defun openweather--format-hourly--temp (value)
  "Format temperature in hourly forecast."
  (let ((temp-symbol (openweather-temperature-unit)))
    (openweather--insert 'font-lock-keyword-face
                         (concat "*** Temperature "
                                 (format "%s%s\n" value temp-symbol)))))

(defun openweather--format-hourly--feels_like (value)
  "Format 'feels like temperature in hourly forecast."
  (let ((temp-symbol (openweather-temperature-unit)))
    (openweather--insert 'font-lock-keyword-face
                         (concat "*** Feels like "
                                 (format "%s%s\n" value temp-symbol)))))

(defun openweather--format-hourly--pressure (value)
  "Format pressure in hourly forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Pressure %shPa\n" value)))

(defun openweather--format-hourly--humidity (value)
  "Format humidity in hourly forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Humidity %s%%\n" value)))

(defun openweather--format-hourly--dew_point (value)
  "Format dewpoint temperature in hourly forecast."
  (let ((temp-symbol (openweather-temperature-unit)))
    (openweather--insert 'font-lock-keyword-face
                         (concat "*** Dewpoint temperature "
                                 (format "%s%s\n" value temp-symbol)))))

(defun openweather--format-hourly--uvi (value)
  "Format UV index in hourly forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** UV index %s\n" value)))

(defun openweather--format-hourly--clouds (value)
  "Format cloudiness in hourly forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Clouds %s%%\n" value)))

(defun openweather--format-hourly--visibility (value)
  "Format average visibility in hourly forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Visibility (avg) %sm\n" value)))

(defun openweather--format-hourly--wind_speed (value)
  "Format wind speed in hourly forecast."
  (let ((velo-symbol (openweather-velocity-unit)))
    (openweather--insert 'font-lock-keyword-face
                         (concat "*** Wind speed "
                                 (format "%s%s\n" value velo-symbol)))))

(defun openweather--format-hourly--wind_gust (value)
  "Format wind gust in hourly forecast."
  (let ((velo-symbol (openweather-velocity-unit)))
    (openweather--insert 'font-lock-keyword-face
                         (concat "*** Wind gust "
                                 (format "%s%s\n" value velo-symbol)))))

(defun openweather--format-hourly--wind_deg (value)
  "Format wind direction in hourly forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Wind direction %s° (%s)\n"
                               value
                               (openweather--calculate-wind_dir value))))

(defun openweather--format-hourly--weather (attributes)
  "Format weather summary in hourly forecast."
  ;; extract single element from json array
  (let ((attrs (elt attributes 0)))
    (openweather--insert 'font-lock-keyword-face
                         (format "*** Conditions %s %s\n"
                                 (cdr (assoc 'description attrs))
                                 (cdr (assoc 'icon attrs))))))

(defun openweather--format-hourly--pop (value)
  "Format probability of precipitation in hourly forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Probability of precipitation %s\n" value)))

(defun openweather--format-hourly--rain (attribute)
  "Format rain volume in hourly forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Rain volume for last %s %smm\n"
                               (car (assoc '1h attribute))
                               (cdr (assoc '1h attribute)))))

(defun openweather--format-hourly--snow (attribute)
  "Format snow volume in hourly forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Snow volume for last %s %smm\n"
                               (car (assoc '1h attribute))
                               (cdr (assoc '1h attribute)))))

;;; ======== formatting functions for daily forecast ========

(defun openweather--format-daily--dt (value)
  "Format time in hourly forecast."
  (openweather--insert 'font-lock-function-name-face
                       (format-time-string "%A %F\n" value)))

(defun openweather--format-daily--sunrise (value)
  "Format time of daily sunrise."
  (let ((d (decode-time (seconds-to-time value))))
    (openweather--insert 'font-lock-keyword-face
                         (format "*** Sunrise %02d:%02d:%02d\n" (nth 2 d) (nth 1 d) (nth 0 d)))))

(defun openweather--format-daily--sunset (value)
  "Format time of daily sunset."
  (let ((d (decode-time (seconds-to-time value))))
    (openweather--insert 'font-lock-keyword-face
                         (format "*** Sunset %02d:%02d:%02d\n" (nth 2 d) (nth 1 d) (nth 0 d)))))

(defun openweather--format-daily--temp (attributes)
  "Format temperature in daily forecast."
  (openweather--insert 'font-lock-keyword-face "*** Temperature")
  (let ((temp-symbol (openweather-temperature-unit)))
    (dolist (attr attributes)
      (openweather--insert 'font-lock-keyword-face
                           (format " (%s) %s%s" (car attr) (cdr attr) temp-symbol))))
  (insert "\n"))

(defun openweather--format-daily--feels_like (attributes)
  "Format 'feels like' temperature in daily forecast."
  (openweather--insert 'font-lock-keyword-face "*** Feels like")
  (let ((temp-symbol (openweather-temperature-unit)))
    (dolist (attr attributes)
      (openweather--insert 'font-lock-keyword-face
                           (format " (%s) %s%s" (car attr) (cdr attr) temp-symbol))))
  (insert "\n"))

(defun openweather--format-daily--pressure (value)
  "Format pressure in daily forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Pressure %shPa\n" value)))

(defun openweather--format-daily--humidity (value)
  "Format humidity in daily forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Humidity %s%%\n" value)))

(defun openweather--format-daily--dew_point (value)
  "Format dewpoint temperature in daily forecast."
  (let ((temp-symbol (openweather-temperature-unit)))
    (openweather--insert 'font-lock-keyword-face
                         (concat "*** Dewpoint temperature "
                                 (format "%s%s\n" value temp-symbol)))))

(defun openweather--format-daily--wind_speed (value)
  "Format wind speed in daily forecast."
  (let ((velo-symbol (openweather-velocity-unit)))
    (openweather--insert 'font-lock-keyword-face
                         (concat "*** Wind speed "
                                 (format "%s%s\n" value velo-symbol)))))

(defun openweather--format-daily--wind_gust (value)
  "Format wind gust in daily forecast."
  (let ((velo-symbol (openweather-velocity-unit)))
    (openweather--insert 'font-lock-keyword-face
                         (concat "*** Wind gust "
                                 (format "%s%s\n" value velo-symbol)))))

(defun openweather--format-daily--wind_deg (value)
  "Format wind direction in daily forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Wind direction %s° (%s)\n"
                               value
                               (openweather--calculate-wind_dir value))))

(defun openweather--format-daily--weather (attributes)
  "Format weather summary in daily forecast."
  ;; extract single element from json array
  (let ((attrs (elt attributes 0)))
    (openweather--insert 'font-lock-keyword-face
                         (format "*** Conditions %s %s\n"
                                 (cdr (assoc 'description attrs))
                                 (cdr (assoc 'icon attrs))))))

(defun openweather--format-daily--clouds (value)
  "Format cloudiness in daily forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Clouds %s%%\n" value)))

(defun openweather--format-daily--pop (value)
  "Format probability of precipitation in daily forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Probability of precipitation %s\n" value)))

(defun openweather--format-daily--rain (value)
  "Format precipitation in daily forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Rain %smm\n" value)))

(defun openweather--format-daily--uvi (value)
  "Format UV index in daily forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** UV index %s\n" value)))

(defun openweather--format-daily--snow (value)
  "Format snow volume in daily forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Snow %smm\n" value)))

;;; ======== formatting functions for alerts ========

(defun openweather--format-alert--sender_name (value)
  "Format name of the alert source."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Sender name %s\n" value)))

(defun openweather--format-alert--event (value)
  "Format name of alert event."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Event %s\n" value)))

(defun openweather--format-alert--start (value)
  "Format date and time of the start of the alert."
  (openweather--insert 'font-lock-keyword-face
                       (format-time-string "*** Start %A %F %T\n" value)))

(defun openweather--format-alert--end (value)
  "Format date and time of the end of the alert."
  (openweather--insert 'font-lock-keyword-face
                       (format-time-string "*** End %A %F %T\n" value)))

(defun openweather--format-alert--description (value)
  "Format description of the alert."
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Description %s\n" value)))

;;; ======== end of formatting functions ========

;;; ======== functions for processing type of forecast ========

(defun openweather--process-current (attributes)
  "Format current forecast."
  (openweather--insert 'font-lock-function-name-face
                       "** Current ")
  (dolist (attr attributes)
    (let ((formatter (intern (concat "openweather--format-current--"
                                     (symbol-name (car attr))))))
      (if (fboundp formatter)
          (funcall formatter (cdr attr))
        (insert (format "Unknown entry %s\n" attr)))))
  ;; calculate daytime as well
  (openweather--insert 'font-lock-keyword-face
                       (format "*** Daytime %s"
                               (openweather--calculate-time-difference (cdr (assoc 'sunset attributes))
                                                                       (cdr (assoc 'sunrise attributes)))))
  (insert "\n"))

(defun openweather--process-minutely (attributes)
  "Format every minute forecast for 1h period."
  (openweather--insert 'font-lock-function-name-face
                       "** Every minute precipitation\n")
  ;; loop over json array and extract each element
  (dotimes (n (length attributes))
    (if (= 0 (% n 5))
        (openweather--insert 'font-lock-keyword-face "***"))
    ;; now iterate over list for this element from json array
    (dolist (attr (elt attributes n))
      (let ((formatter (intern (concat "openweather--format-minutely--"
                                       (symbol-name (car attr))))))
        (if (fboundp formatter)
            (funcall formatter (cdr attr))
          (insert (format "Unknown entry %s\n" attr)))))
    (if (= 4 (% n 5))
        (insert "\n")))
  (insert "\n"))

(defun openweather--process-hourly (attributes)
  "Format hourly forecast."
  ;; loop over json array and extract each element
  (dotimes (n (length attributes))
    (openweather--insert 'font-lock-function-name-face
                         "** Hourly for ")
    ;; now iterate over list for this element from json array
    (dolist (attr (elt attributes n))
      (let ((formatter (intern (concat "openweather--format-hourly--"
                                       (symbol-name (car attr))))))
        (if (fboundp formatter)
            (funcall formatter (cdr attr))
          (insert (format "Unknown entry %s\n" attr)))))))

(defun openweather--process-daily (attributes)
  "Format daily forecast."
  ;; loop over json array and extract each element
  (dotimes (n (length attributes))
    (openweather--insert 'font-lock-function-name-face
                         "** Daily for ")
    ;; now iterate over list for this element from json array
    (dolist (attr (elt attributes n))
      (let ((formatter (intern (concat "openweather--format-daily--"
                                       (symbol-name (car attr))))))
        (if (fboundp formatter)
            (funcall formatter (cdr attr))
          (insert (format "Unknown entry %s\n" attr)))))
    ;; calculate daytime as well
    (openweather--insert 'font-lock-keyword-face
                         (format "*** Daytime %s"
                                 (openweather--calculate-time-difference (cdr (assoc 'sunset (elt attributes n)))
                                                                         (cdr (assoc 'sunrise (elt attributes n))))))
    (insert "\n")))

(defun openweather--process-alerts (attributes)
  "Format announced weather alerts."
  ;; loop over json array and extract each element
  (dotimes (n (length attributes))
    (openweather--insert 'font-lock-function-name-face
                         "** Alerts ")
    ;; now iterate over list for this json array element
    (dolist (attr (elt attributes n))
      (let ((formatter (intern (concat "openweather--format-alert--"
                                       (symbol-name (car attr))))))
        (if (fboundp formatter)
            (funcall formatter (cdr attr))
          (insert (format "Unknown entry %s\n" attr)))))
    (insert "\n")))

;;; ======== end of functions for processing type of forecast ========

;;;###autoload
(defun openweather-update (&optional no-switch)
  "Display weather forecast.
If NO-SWITCH is non-nil then do not switch to weather forecast buffer."
  (interactive)
  (with-current-buffer (get-buffer-create openweather-buffer-name)
    (save-excursion
      (let ((inhibit-read-only t))
        (remove-images (point-min) (point-max))

        (openweather-mode)
        ;; enable outline-minor-mode to do folding of entries in buffer
        (outline-minor-mode)
        (erase-buffer)
        (goto-char (point-min))
        (openweather--insert 'openweather-header
                             (concat "Forecast for "
                                     openweather-location-name
                                     "\n"))
        (openweather--insert 'openweather-date
                             "* For "
                             (format-time-string "%A %Y-%m-%d") "\n")
        (let ((url (openweather-make-url openweather-location-latitude
                                         openweather-location-longitude
                                         openweather-units)))
          (url-retrieve url
                        (lambda (status start-time)
                          (message "The request completed in %f seconds"
                                   (float-time (time-subtract nil start-time)))
                          ;; (display-buffer (current-buffer))
                          (save-excursion
                            (goto-char (point-min))
                            (unless (search-forward "\n\n" nil t)
                              (kill-buffer)
                              (error "Error in http reply"))
                            (let ((headers (buffer-substring (point-min) (point))))
                              (unless (string-match-p
                                       (concat "^HTTP/1.1 "
                                               "\\(200 OK\\|203 "
                                               "Non-Authoritative Information\\)")
                                       headers)
                                (kill-buffer)
                                (error "Unable to fetch data"))
                              (url-store-in-cache (current-buffer))
                              (setq openweather--data (json-read))
                              (kill-buffer)))
                          )
                        `(,(current-time))
                        'silent
                        'inhibit-cookies))
        ;; process current, every minute, hourly and daily forecast by calling the functions
        ;; - openweather--process-current
        ;; - openweather--process-minutely
        ;; - openweather--process-hourly
        ;; - openweather--process-daily
        ;; also consider alerts, call function openweather--process-alerts
        ;; start from the fifth list element on in openweather--data
        (dolist (entry (nthcdr 4 openweather--data))
          (let ((formatter (intern (concat "openweather--process-"
                                           (symbol-name (car entry))))))
             (if (fboundp formatter)
                 (funcall formatter (cdr entry))
               (insert (format "Unknown entry %s\n" entry)))))


        )) ;; end of let and save-excursion
    (goto-char (point-min)))
  (unless no-switch
    (switch-to-buffer openweather-buffer-name)))

(provide 'openweather)

;;; openweather.el ends here

;;; openweather.el --- Weather data from OpenWeather in Emacs -*- lexical binding: t -*-

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
  "Format 'feels like temperature in current forecast."
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
                       (format "*** Wind direction %s°\n" value)))

(defun openweather--format-current--weather (attributes)
  "Format weather summary in current forecast."
  ;; extract single element from json array
  (let ((attrs (elt attributes 0)))
    (openweather--insert 'font-lock-keyword-face
                         (format "*** Conditions %s %s\n"
                                 (cdr (assoc 'description attrs))
                                 (cdr (assoc 'icon attrs))))))

;;; ======== formatting functions for minutely forecast ========

(defun openweather--format-minutely--dt (value)
  "Format time in minutely forecast."
  (let ((d (decode-time (seconds-to-time value))))
    (openweather--insert 'font-lock-keyword-face
                         (format "*** %02d:%02d " (nth 2 d) (nth 1 d)))))

(defun openweather--format-minutely--precipitation (value)
  "Format precipitation in minutely forecast."
  (openweather--insert 'font-lock-keyword-face
                       (format "%smm\n" value)))

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
                       (format "*** Wind direction %s°\n" value)))

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

;;; ======== end of formatting functions ========

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
    ;; now iterate over list for this element from json array
    (dolist (attr (elt attributes n))
      (let ((formatter (intern (concat "openweather--format-minutely--"
                                       (symbol-name (car attr))))))
        (if (fboundp formatter)
            (funcall formatter (cdr attr))
          (insert (format "Unknown entry %s\n" attr)))))))

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
                          (message "The request is completed in %f seconds"
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
        ;; start from the fifth list element on in openweather--data
        (dolist (entry (nthcdr 4 openweather--data))
          (let ((formatter (intern (concat "openweather--process-"
                                           (symbol-name (car entry))))))
             (if (fboundp formatter)
                 (funcall formatter (cdr entry))
               (insert (format "Unknown entry %s\n" entry)))))


        ;; (insert (format "%s" openweather--data))
        )) ;; end of let and save-excursion
    (goto-char (point-min)))
  (unless no-switch
    (switch-to-buffer openweather-buffer-name)))

(provide 'openweather)

;;; openweather.el ends here

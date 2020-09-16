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
        (insert (format "%s" openweather--data))
        )) ;; end of let and save-excursion
    (goto-char (point-min)))
  (unless no-switch
    (switch-to-buffer openweather-buffer-name)))

(provide 'openweather)

;;; openweather.el ends here

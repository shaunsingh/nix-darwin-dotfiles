(in-package #:nyxt-user)

;; NMCLI

(defun get-wifi-devices ()
  #+linux
  "Retrieve a list of WiFi devices using nmcli."
  (mapcar 
   #'first 
   (remove-if 
    (lambda (line) 
      (or (null line) 
          (string= (first line) "DEVICE")))
    (mapcar 
     (lambda (line) 
       (str:words line))
     (rest (str:lines (uiop:run-program "nmcli device status" :output :string)))))))
 
(defun get-available-networks (device)
  #+linux
  "Retrieve available networks for a given WiFi device using nmcli."
  (let* ((wifi-list-output 
           (uiop:run-program 
            (format nil "nmcli -f SSID,BSSID device wifi list ifname ~A" device) 
            :output :string))
         (lines (rest (str:lines wifi-list-output))))
    (mapcar 
     (lambda (line)
       (let ((parts (str:words line)))
         (or 
          (and parts (first parts))
          "")))
     (remove-if 
      (lambda (line) 
        (or (null line) 
            (string= line "") 
            (string= (str:trim line) "SSID")))
      lines))))

(defun prompt-wifi-device ()
  #+linux
  "Prompt user to select a WiFi device."
  (let ((devices (get-wifi-devices)))
    (or 
     (first 
      (prompt 
       :prompt "Select WiFi device"
       :sources (make-instance 
                 'prompter:source 
                 :name "WiFi Devices"
                 :constructor devices)))
     (error "No WiFi devices found"))))

(defun prompt-network (device)
  #+linux
  "Prompt user to select a network for the given device."
  (let ((networks (get-available-networks device)))
    (or 
     (first 
      (prompt 
       :prompt "Select Network"
       :sources (make-instance 
                 'prompter:source 
                 :name "Available Networks"
                 :constructor networks))))
     (error "No networks found")))

(defun prompt-wifi-password ()
  #+linux
  "Prompt user to enter WiFi password."
  (first 
   (prompt 
    :prompt "Enter WiFi Password: "
    :sources (make-instance 
              'prompter:raw-source))))

#+linux
(define-command-global connect-wifi ()
  "Connect to a WiFi network using nmcli."
  (let* ((wlan-device (prompt-wifi-device))
         (network-name (prompt-network wlan-device))
         (password (prompt-wifi-password)))
    (let ((command 
            (format nil 
                    "nmcli device wifi connect '~A' password '~A' ifname ~A" 
                    network-name password wlan-device)))
      (uiop:launch-program command)
      (echo "Connecting to ~A on ~A" network-name wlan-device))))

;; BLUETOOTHCTL

(defun get-bluetooth-devices ()
  #+linux
  "Retrieve a list of available Bluetooth devices using bluetoothctl."
  (remove-if #'null
             (mapcar (lambda (line)
                       (when (search "Device" line)
                         (nth 1 (str:words line))))
                     (str:lines (uiop:run-program "bluetoothctl devices" :output :string)))))

(defun prompt-bluetooth-device ()
  #+linux
  "Prompt user to select a Bluetooth device."
  (let ((devices (get-bluetooth-devices)))
    (or 
     (first 
      (prompt 
       :prompt "Select Bluetooth device"
       :sources (make-instance 
                 'prompter:source 
                 :name "Bluetooth Devices"
                 :constructor devices)))
     (error "No Bluetooth devices found"))))

#+linux
(define-command-global connect-bluetooth ()
  "Connect to a Bluetooth device using bluetoothctl."
  (let ((device (prompt-bluetooth-device)))
    (uiop:launch-program (format nil "bluetoothctl connect ~A" device))
    (echo "Connecting to Bluetooth device: ~A" device)))

;;; DFR 

;; utilities

#+linux
(defun get-current-brightness ()
  "Get current brightness as percentage"
  (let* ((current-brightness
           (parse-integer
            (uiop:run-program
             "brightnessctl g"
             :output :string)
            :junk-allowed t)))
    (floor (* 100 (/ current-brightness 100)))))

#+linux
(defun set-brightness-percentage (percentage)
  "Set brightness to given percentage"
  (let* ((clamped-percentage (max 0 (min 100 percentage)))
         (command
           (format nil "brightnessctl s ~A%" clamped-percentage)))
    (uiop:launch-program command)
    (echo "Brightness set to ~A%" clamped-percentage)))

#+linux
(defun get-current-volume ()
  "Get current volume as percentage"
  (parse-integer
   (uiop:run-program
    "pamixer --get-volume"
    :output :string)
   :junk-allowed t))

#+linux
(defun set-volume-percentage (percentage)
  "Set volume to given percentage"
  (let* ((clamped-percentage (max 0 (min 100 percentage)))
         (command
           (format nil "pamixer --set-volume ~A" clamped-percentage)))
    (uiop:launch-program command)
    (echo "Volume set to ~A%" clamped-percentage)))

#+linux
(defun is-muted ()
  "Check if audio is currently muted"
  (string=
   (string-trim '(#\newline #\space)
                (uiop:run-program
                 "pamixer --get-mute"
                 :output :string))
   "true"))

;; brightness

#+linux
(define-command-global set-brightness ()
  "Prompt user to set brightness percentage"
  (let* ((current-percentage (get-current-brightness))
         (new-brightness
           (first
            (prompt
             :prompt (format nil "Current Brightness: ~A%. Enter new brightness (0-100): "
                            current-percentage)
             :sources (make-instance
                       'prompter:raw-source)))))
    (set-brightness-percentage (parse-integer new-brightness))))

#+linux
(define-command-global brightness-down ()
  "Decrease brightness by 5%"
  (let ((current-percentage (get-current-brightness)))
    (set-brightness-percentage (- current-percentage 5))))

#+linux
(define-command-global brightness-up ()
  "Increase brightness by 5%"
  (let ((current-percentage (get-current-brightness)))
    (set-brightness-percentage (+ current-percentage 5))))

;; volume

#+linux
(defvar *previous-volume* 100
  "Stores the volume level before muting")

#+linux
(define-command-global set-volume ()
  "Prompt user to set volume percentage"
  (let* ((current-volume (get-current-volume))
         (new-volume
           (first
            (prompt
             :prompt (format nil "Current Volume: ~A%. Enter new volume (0-100): "
                            current-volume)
             :sources (make-instance
                       'prompter:raw-source)))))
    (set-volume-percentage (parse-integer new-volume))))

#+linux
(define-command-global volume-down ()
  "Decrease volume by 5%"
  (let ((current-volume (get-current-volume)))
    (set-volume-percentage (- current-volume 5))))

#+linux
(define-command-global volume-up ()
  "Increase volume by 5%"
  (let ((current-volume (get-current-volume)))
    (set-volume-percentage (+ current-volume 5))))

#+linux
(define-command-global toggle-mute ()
  "Toggle mute. Remembers previous volume when muting."
  (if (is-muted)
      (progn
        (set-volume-percentage *previous-volume*)
        (uiop:launch-program "pamixer --unmute")
        (echo "Unmuted. Volume restored to ~A%" *previous-volume*))
      (progn
        (setf *previous-volume* (get-current-volume))
        (uiop:launch-program "pamixer --mute")
        (echo "Muted"))))

;; backlight

#+linux
(define-command-global keyboard-backlight-down ()
  "Decrease keyboard backlight"
  (uiop:launch-program "brightnessctl -d '*::kbd_backlight' s 5%-")
  (echo "Keyboard backlight decreased"))

#+linux
(define-command-global keyboard-backlight-up ()
  "Increase keyboard backlight"
  (uiop:launch-program "brightnessctl -d '*::kbd_backlight' s +5%")
  (echo "Keyboard backlight increased"))

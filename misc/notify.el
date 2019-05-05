;;; notify.el --- notification front-end

;; Copyright (C) 2008  Mark A. Hershberger

;; Original Author: Mark A. Hershberger <mhersberger@intrahealth.org>
;; Modified by Andrey Kotlarski <m00naticus@gmail.com>
;; Modified by Andrew Gwozdziewycz <git@apgwoz.com>
;; Modified by Aidan Gauland <aidalgol@no8wireless.co.nz> October 2011
;; Modified by Olivier Sirven <the.slaa@gmail.com> November 2013
;; Modified by zhanghj <ccsmile2008@outlook.com> April 2019
;; Keywords: extensions, convenience, lisp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This provides a single function, `notify', that will produce a notify
;; pop-up via D-Bus, libnotify, simple message or growl.
;; To use, just put (autoload 'notify "notify" "Notify TITLE, BODY.")
;;  in your init file.  You may override default chosen notification
;;  method by assigning `notify-method' to one of 'notify-via-dbus
;; 'notify-via-libnotify or 'notify-via-message
;;; Code:

;; TODO: use diffrent alist for diffrent backends
(defvar notify-default-alist '((:app . "Emacs")
							   (:icon . "emblem-default")
							   (:timeout . 5000)
							   (:urgency . "low")
							   (:category . "emacs.message")
							   (:sound . "default"))
  "Notification settings' defaults.")

;;+Added by zhanghj(ccsmile2008@outlook.com) at 11:24 04/26/2019
(defvar notify-critical-alist '((:app . "Emacs")
								(:icon . "emblem-important")
								(:timeout . 9999999)
								(:urgency . "critical")
								(:category . "emacs.message")
								(:sound . "Glass"))
  "Notification settings' defaults for critical events.")

(defvar notify-delay '(0 5 0)
  "Minimum time allowed between notifications in time format.")

(defvar notify-last-notification '(0 0 0) "Time of last notification.")

(defvar notify-method nil "Notification method among
'notify-via-dbus, 'notify-via-libnotify, 'notify-via-message or
'notify-via-growl")

;; determine notification method unless already set
;; prefer growl > D-Bus > libnotify > message
(unless notify-method
  (setq notify-method
		(cond
		 ((executable-find "growlnotify") 'notify-via-growl)
		 ((executable-find "osascript") 'notify-via-osacript)
		 ((and (require 'dbus nil t)
			   (boundp 'dbus-runtime-version)
			   (dbus-ping :session "org.freedesktop.Notifications"))
		  (defvar notify-id 0 "Current D-Bus notification id.")
		  'notify-via-dbus)
		 ((executable-find "notify-send") 'notify-via-libnotify)
		 (t 'notify-via-message))))


(defun notify-via-dbus (title body notify-alist)
  "Send notification with TITLE, BODY `D-Bus'."
  (dbus-call-method :session "org.freedesktop.Notifications"
					"/org/freedesktop/Notifications"
					"org.freedesktop.Notifications" "Notify"
					(alist-get :app notify-alist)
					(setq notify-id (+ notify-id 1))
					(alist-get :icon notify-alist) title body '(:array)
					'(:array :signature "{sv}") ':int32
					(alist-get :timeout notify-alist)))

(defun notify-via-libnotify (title body notify-alist)
  "Notify with TITLE, BODY via `libnotify'."
  (call-process "notify-send" nil 0 nil
				title body "-t"
				(number-to-string (alist-get :timeout notify-alist))
				"-i" (alist-get :icon notify-alist)
				"-u" (alist-get :urgency notify-alist)
				"-c" (alist-get :category notify-alist)))

(defun notify-via-message (title body notify-alist)
  "Notify TITLE, BODY with a simple message."
  (message "%s: %s" title body))

(defun notify-via-growl (title body notify-alist)
  "Notify TITLE, BODY with a growl"
  (call-process "growlnotify" nil 0 nil
                "-a" (alist-get :app notify-alist)
                "-n" (alist-get :category notify-alist)
                "-t" (notify-via-growl-stringify title)
                "-m" (notify-via-growl-stringify body)))

(defun notify-via-growl-stringify (thing)
  (cond ((null thing) "")
        ((stringp thing) thing)
        (t (format "%s" thing))))

;;+Added by zhanghj(ccsmile2008@outlook.com) at 14:43 04/26/2019
(defun notify-via-osacript (title body notify-alist)
  (let ((arg-str (format "display notification \"%s\" with title \"%s\" sound name \"%s\""
						 body title
						 (alist-get :sound notify-alist))))
	(call-process "osascript" nil 0 nil "-e" arg-str)))


(defun notify-merge-keywords (alist args)
  "Merge property list key-values from ARGS into ALIST."
  (while args
	(if (assq (car args) alist)
		(setf (alist-get (car args) alist) (cadr args))
	  (setq alist (cons (cons (car args) (cadr args)) alist)))
    (setq args (cddr args)))
  alist)


;;;###autoload
(defun my-notify (title body &rest args)
  "Notify TITLE, BODY via `notify-method'.
ARGS may be amongst :timeout, :icon, :urgency, :app and :category."
  (when (time-less-p notify-delay
					 (time-since notify-last-notification))
	(setq notify-last-notification (current-time))
	(let ((notify-alist (notify-merge-keywords
						 (copy-alist notify-default-alist) args)))
	  (funcall notify-method title body notify-alist))))


;;+Added by zhanghj(ccsmile2008@outlook.com) at 11:24 04/26/2019
;;;###autoload
(defun my-notify-critical (title body &rest args)
  "Notify TITLE, BODY via `notify-method'.
ARGS may be amongst :timeout, :icon, :urgency, :app and :category."
  (when (time-less-p notify-delay
					 (time-since notify-last-notification))
	(setq notify-last-notification (current-time))
	(let ((notify-alist (notify-merge-keywords
						 (copy-alist notify-critical-alist) args)))
	  (funcall notify-method title body notify-alist))))




(provide 'notify)

;;; notify.el ends here

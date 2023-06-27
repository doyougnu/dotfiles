;;; dyg/+irc/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun dyg/fetch-password (&rest params)
    (require 'auth-source)
    (let ((match (car (apply #'auth-source-search params))))
      (if match
          (let ((secret (plist-get match :secret)))
            (if (functionp secret)
                (funcall secret)
              secret))
        (error "Password not found for %S" params))))

;;;###autoload
(defun dyg/nickserv-password (server)
  (dyg/fetch-password :user "doyougnu" :host "irc.libera.chat"))

;;;###autoload
(defun dyg/znc-password (server)
  (dyg/fetch-password :user "doyougnu" :host "relay.local"))

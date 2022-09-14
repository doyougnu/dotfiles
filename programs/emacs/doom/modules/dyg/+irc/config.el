;;; dyg/+irc/config.el -*- lexical-binding: t; -*-


(setq auth-sources '("~/.authinfo.gpg"))

(set-irc-server! "irc.libera.chat"
  '(:tls                t
    :reduce-lurker-spam t
    :port               6697
    :nick               "doyougnu"
    :sasl-password      (funcall #'dyg/nickserv-password)
    :channels           ("#emacs" "#haskell" "#ghc")))

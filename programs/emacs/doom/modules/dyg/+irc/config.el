;;; dyg/+irc/config.el -*- lexical-binding: t; -*-


(setq auth-sources '("~/.authinfo.gpg"))
(setq circe-color-nicks-everywhere t)
(setq irc-debug-log t)

(set-irc-server! "relay.local"
  '(:use-tls            f
    :reduce-lurker-spam t
    :port               5000
    :user               "doyougnu"
    :pass               dyg/znc-password
    :sasl-external      t
    :sasl-username      "doyougnu"
    :sasl-password      dyg/nickserv-password
    :nickserv-nick      "doyougnu"
    :nickserv-password  dyg/nickserv-password
    :channels           ("#emacs" "#haskell" "#ghc" "#commonlisp" "#lispcafe"
                         "#lobsters" "#minikanren" "#nixos" "#ghcjs")))

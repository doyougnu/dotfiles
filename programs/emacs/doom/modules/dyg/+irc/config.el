;;; dyg/+irc/config.el -*- lexical-binding: t; -*-


(setq auth-sources '("~/.authinfo.gpg"))

(set-irc-server! "relay.local"
  '(:use-tls            t
    :tls-keylist        (("~/sync/keys/auth/nick.pem"))
    :reduce-lurker-spam t
    :port               5000
    :nickserv-nick      "doyougnu"
    :nickserv-password  dyg/nickserv-password
    :sasl-username      "doyougnu"
    :sasl-password      dyg/nickserv-password
    :channels           ("#emacs" "#haskell" "#ghc" "#commonlisp" "#lispcafe"
                         "#lobsters" "#minikanren" "#nixos" "#ghcjs")))

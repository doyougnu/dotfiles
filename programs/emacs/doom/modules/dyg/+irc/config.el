;;; dyg/+irc/config.el -*- lexical-binding: t; -*-


(setq auth-sources '("~/.authinfo.gpg"))

(set-irc-server! "irc.libera.chat"
  '(:tls                t
    :reduce-lurker-spam t
    :port               6697
    :nickserv-nick      "doyougnu"
    :nickserv-password  dyg/nickserv-password
    :sasl-username      "doyougnu"
    :sasl-password      dyg/nickserv-password
    :channels           ("#emacs" "#haskell" "#ghc" "#commonlisp" "#lispcafe"
                         "#lobsters" "#minikanren" "#nixos" "#ghcjs")))

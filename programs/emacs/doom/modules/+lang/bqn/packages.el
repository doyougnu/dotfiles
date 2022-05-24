;; -*- no-byte-compile: t; -*-
;;; +lang/bqn/packages.el


;; (package! bqn-mode
;;   :recipe (:host github :repo "doyougnu/bqn-mode")
;;   :pin "89d6928d0653518c97bcb06ae156f8b1de1b8768")


(package! bqn-mode
  :recipe (:local-repo "/home/doyougnu/programming/bqn-mode"))

(package! gnu-apl-mode
  :recipe (:host github :repo "lokedhs/gnu-apl-mode")
  :pin "5d998206a963f2205dc6c4eddb41fb34187cb527")

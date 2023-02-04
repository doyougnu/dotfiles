;;; dyg/+dnd/config.el -*- lexical-binding: t; -*-


(after! projectile
  (projectile-register-project-type 'dnd '("campaign.org" "players.org" ".projectile")
                                    :project-file ".projectile"
                                    :compile      ""
                                    :src-dir      ""
                                    :configure    ""
                                    :run          ""))

;;; dyg/+dnd/config.el -*- lexical-binding: t; -*-


(after! projectile
  (projectile-register-project-type 'dnd '("campaign.org" "players.org")
                                    :project-file "campaign.org"
                                    :compile      ""
                                    :src-dir      ""
                                    :configure    ""
                                    :run          ""))

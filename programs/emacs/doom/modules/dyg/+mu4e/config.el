;;; dyg/+mu4e/config.el -*- lexical-binding: t; -*-

(after! mu4e

  (setq smtpmail-auth-credentials  (expand-file-name "~/.authinfo.gpg")
        message-send-mail-function 'sendmail-send-it
        mu4e-compose-context-policy nil)

  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "Blog"
             :enter-func (lambda () (mu4e-message "Entering Blog context"))
             :leave-func (lambda () (mu4e-message "Leaving Blog context"))
             ;; we match based on the contact-fields of the message
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "jeff@doyougnu.xyz")))
             :vars '( ( user-mail-address      . "jeff@doyougnu.xyz"  )
                      ( user-full-name         . "Jeffrey M. Young" )
                      ;; ( message-user-organization . "Homebase" )
                      ( mu4e-compose-signature . "- Jeff\n")))
           ,(make-mu4e-context
             :name "Work"
             :enter-func (lambda () (mu4e-message "Switch to the Work context"))
             :leave-func (lambda () (mu4e-message "Leaving Work context"))
             ;; no leave-func
             ;; we match based on the maildir of the message
             ;; this matches maildir /Arkham and its sub-directories
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "jeffrey.young@iohk.io")))
             :vars '( ( user-mail-address       . "jeffrey.young@iohk.io" )
                      ( user-full-name          . "Jeffrey M. Young" )
                      ( message-user-organization . "IOG" )
                      ( mu4e-compose-signature    . nil )
                      ))

           ,(make-mu4e-context
             :name "DumpMail"
             :enter-func (lambda () (mu4e-message "Switch to the Dump context"))
             :leave-func (lambda () (mu4e-message "Leaving Dump context"))
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "jmy6342@gmail.com")))
             :vars '( ( user-mail-address       . "jmy6342@gmail.com" )
                      ( user-full-name          . "Jeffrey M. Young" )
                      ( mu4e-compose-signature  . nil))))))


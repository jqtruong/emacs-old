(require 'jabber)

(setq jabber-account-list '(
                            ("jerometruong@gmail.com"
                             (:password . nil)
                             (:network-server . "talk.google.com")
                             (:port . 443)
                             (:connection-type . ssl))
                            ))


(provide 'my-jabber-stuff)

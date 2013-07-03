;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'jabber)

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
(setq jabber-account-list
      '(("jerometruong@gmail.com"
         (:password . nil)
         (:network-server . "talk.google.com")
         (:port . 443)
         (:connection-type . ssl))))

;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;
(defun jqt/first-jabber-chat-buffer ()
  "Switch to or pop the first jabber chat buffer."
  (interactive)
  (let* ((bufs (jqt/jabber-chat-buffers))
         (first-chat-buffer (elt bufs 0))
         (jabber-chat-window (when bufs
                               (get-buffer-window (elt bufs 0)))))
    (if jabber-chat-window
        (select-window jabber-chat-window)
      (pop-to-buffer first-chat-buffer))))

(defun jqt/jabber-chat-buffers ()
  "Filters buffer list for names that start with `*-jabber-chat-'."
  (remove-if-not (lambda (buffer)
                   (string-match "^\*-jabber-chat-" (buffer-name buffer))) (buffer-list)))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-, j") 'jqt/first-jabber-chat-buffer)



(provide 'my-jabber-stuff)

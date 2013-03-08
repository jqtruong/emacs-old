(defvar *victaulic-servers*
  '((web1 . "50.56.17.57")
    (web2 . "50.56.17.58")
    (db1  . "50.56.17.59")
    (db2  . "50.56.17.60")))

(defun victaulic/shell (server-name)
  ""
  (interactive "sServer name: ")
  (let (;; Store current directory for later grab. 
        (previous-directory default-directory))
    ;; Change directory to remote server.
    (cd (format "/nerdery@%s:" (cdr (assoc (intern server-name) *victaulic-servers*))))
    ;; Open shell to remote server.
    (shell (format ";shell victaulic %s" server-name))
    ;; Switch back to where we currently are.
    (switch-to-buffer nil)
    ;; Change directory back to what it actually is currently.
    (cd previous-directory)
    ;; Switch back to shell.
    (switch-to-buffer nil)))

(provide 'my-setup)

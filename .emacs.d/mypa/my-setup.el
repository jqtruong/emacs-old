(defun jqt/shell (server-name project servers)
  ""
  (let (;; Store current directory for later grab. 
        (previous-directory default-directory))
    ;; Change directory to remote server.
    (cd (format "/%s" (cdr (assoc (intern server-name) servers))))
    ;; Open shell to remote server.
    (shell (format ";shell %s %s" project server-name))
    ;; Switch back to where we currently are.
    (switch-to-buffer nil)
    ;; Change directory back to what it actually is currently.
    (cd previous-directory)
    ;; Switch back to shell.
    (switch-to-buffer nil)))

(defvar *victaulic-servers*
  '((web1 . "nerdery@50.56.17.57:docroot/")
    (web2 . "nerdery@50.56.17.58:docroot/")
    (db1  . "nerdery@50.56.17.59:")
    (db2  . "nerdery@50.56.17.60:")
    (staging . "jtruong@athens.sierrabravo.net:/vweb/victaulic.sierrabravo.net/docs/")))

(defun victaulic/shell (server-name)
  ""
  (interactive "sServer name: ")
  (jqt/shell server-name "victaulic" *victaulic-servers*))

(defvar *ponderoo-servers*
  '((django-1 . "root@108.166.7.154:/var/www/python/ponderoo/")
    (web1 . "")
    (staging . "jtruong@204.62.150.55:/var/www/qdsolutions.sierrabravo.net/")))

(defun ponderoo/shell (server-name)
  ""
  (interactive "sServer name: ")
  (jqt/shell server-name "ponderoo" *ponderoo-servers*))

(provide 'my-setup)

(cdr (assoc 'django-1 *ponderoo-servers*))

(defun java/previous-method ()
  "Move cursor to previous defined method in java-type modes."
  (interactive)
  ;; (search-backward-regexp " +[[:word:]_]+\s*\(.*\)[\s\n\t]*{" nil t)
  (recenter))

(defun java/next-method ()
  "Move cursor to next defined method in java-type modes."
  (interactive)
  )

(defun java/define-keys ()
  "Custom keybings when in java-type modes."
  (interactive)
  (define-key groovy-mode-map (kbd "M-p") 'java/previous-method)
  (define-key groovy-mode-map (kbd "M-n") 'java/next-method))

(provide 'my-java-setup)

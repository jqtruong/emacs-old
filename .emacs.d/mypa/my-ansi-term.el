;; from http://emacs-journey.blogspot.com/2012/06/improving-ansi-term.html

;;;;;;;;;;;;;;;;;;;;;
;; Solarized faces ;;
;;;;;;;;;;;;;;;;;;;;;

(defface solarized-term-color-base02
  '((t :foreground "#002b36" :background "#002b36"))
  "Face used to render red color code."
  :group 'term)

(defface solarized-term-color-red
  '((t :foreground "#dc322f" :background "#dc322f"))
  "Face used to render red color code."
  :group 'term)

(defface solarized-term-color-green
  '((t :foreground "#859900" :background "#859900"))
  "Face used to render red color code."
  :group 'term)

(defface solarized-term-color-yellow
  '((t :foreground "#b58900" :background "#b58900"))
  "Face used to render red color code."
  :group 'term)

(defface solarized-term-color-blue
  '((t :foreground "#dc322f" :background "#dc322f"))
  "Face used to render red color code."
  :group 'term)

(defface solarized-term-color-magenta
  ;; '((t :foreground "#d33682" :background "#d33682"))
  '((t :foreground "#d33682" :background "#d33682"))
  "Face used to render red color code."
  :group 'term)

(defface solarized-term-color-base2
  '((t :foreground "#eee8d5" :background "eee8d5"))
  "Face used to render red color code."
  :group 'term)

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

(defun my-term-paste (&optional string)
  "Provide normal paste/yank functionality to terms."
 (interactive)
 (process-send-string
  (get-buffer-process (current-buffer))
  (if string string (current-kill 0))))

(defun my-term-hook ()
  "Enable:
- linkable urls
- mapping of C-y to custom function to provide yanking
- solarized colors"
  (goto-address-mode)
  (define-key term-raw-map "\C-y" 'my-term-paste)
  (setq ansi-term-color-vector (vconcat `(term
                                          solarized-term-color-base02
                                          solarized-term-color-red
                                          solarized-term-color-green
                                          solarized-term-color-yellow
                                          solarized-term-color-blue
                                          solarized-term-color-magenta
                                          solarized-term-color-base2))))

(defun grails-comint-magic (string)
  "Handle grails output gracefully."
  (let ((position (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char comint-last-output-start)
      (while (re-search-forward "\033\\[1A\033\\[[0-9]+D\033\\[0K" position t)
        (replace-match "" t t)
        (previous-line)
        (delete-region
         (line-beginning-position)
         (progn (forward-line 1) (point)))))))

;;;;;;;;;;;
;; hooks ;;
;;;;;;;;;;;

(add-hook 'comint-output-filter-functions 'grails-comint-magic)



(provide 'my-ansi-term)

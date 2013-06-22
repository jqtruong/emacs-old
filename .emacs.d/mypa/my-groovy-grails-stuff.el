;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'grails-mode)
(require 'project-mode)
(require 'emacs-grails-mode-ext)
(require 'column-marker)

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
(setq grails-mode t)
;; (setq project-mode t)

(add-to-list 'auto-mode-alist '("\.gsp$" . nxml-mode)) 
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode)) 
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))

(autoload 'groovy-eval     "groovy-eval" "Groovy Evaluation" t)
(autoload 'groovy-mode     "groovy-mode" "Mode for editing Groovy source files")
(autoload 'nxml-mode       "nxml-mode" "Mode for editing GSP pages")
(autoload 'run-groovy      "inf-groovy" "Run an inferior Groovy process")
(autoload 'inf-groovy-keys "inf-groovy" "Set local key defs for inf-groovy in groovy-mode")

;; can set groovy-home here, if not in environment
(setq inferior-groovy-mode-hook
      '(lambda()
         (setq groovy-home "/Users/jtruong/.gvm/groovy/current/")))

(font-lock-add-keywords 'groovy-mode
                        '(("given:" . font-lock-keyword-face)
                          ("and:"  . font-lock-keyword-face)
                          ("when:"  . font-lock-keyword-face)
                          ("then:"  . font-lock-keyword-face)
                          ("where:" . font-lock-keyword-face)))

;;;;;;;;;;;
;; faces ;;
;;;;;;;;;;;
(defface column-marker-dark-red '((t (:background "#772211")))
  "Face used for a column marker.  Usually a background color."
  :group 'faces)

;;;;;;;;;;
;; vars ;;
;;;;;;;;;;
(defvar column-marker-dark-red-face 'column-marker-dark-red
    "Face used for a column marker.  Usually a background color.
Changing this directly affects only new markers.")

;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;
(column-marker-create column-marker-4 column-marker-dark-red-face)

;;;;;;;;;;;
;; hooks ;;
;;;;;;;;;;;
(add-hook 'groovy-mode-hook
          '(lambda ()
             (inf-groovy-keys)
             ;; 2013-05-08 09:40:43 - groovy-electric errs with
             ;; opening parenthesis and i don't really need it
             ;; (require 'groovy-electric)
             ;; (groovy-electric-mode)
             (c-set-offset 'label '+)
             (linum-mode)
             (column-marker-4 120)))

;;;;;;;;;;;;;;;
;; shortcuts ;;
;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c m") (lambda () (interactive)
                                (column-marker-4 120)))



(provide 'my-groovy-grails-stuff)


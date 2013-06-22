;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
;; Update PATH
(setq exec-path (append exec-path '("/usr/local/bin/")))
(defvar *git-repo* "~/git/")
(defvar *webapp1* "webapp_bloomhealth/bloomhealth")
(defvar *webapp2* "webapp_bhbo/bhbo")
(defvar *lib1* "lib_common")
(defvar *lib2* "lib_paymentSchedule")
(defvar *lib3* "lib_domain")

;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;
(defun g-finduses (string)
  "Calls grep inside grails apps."
  (interactive "sString: ")
  (rgrep string "*.groovy *.gsp" "grails-app")
  (rgrep string "*.groovy *.gsp" "web-app")
  (rgrep string "*.groovy *.gsp" "src")
  (rgrep string "*.groovy *.gsp" "test"))

(defun set-window-width (width)
  ""
  (interactive "NWidth: ")
  (adjust-window-trailing-edge (selected-window) (- width (window-width)) t))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-, 1") (lambda () (interactive)
                                (set-window-width (+ 120 left-margin-width))))



(provide 'my-bloom-stuff)

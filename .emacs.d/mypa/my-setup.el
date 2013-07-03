;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'my-macros)
(require 'my-eshell-setup)
(require 'my-emoticons)
(require 'my-bloom-stuff)
(require 'my-groovy-grails-stuff)
(require 'my-magit-stuff)
(require 'my-ediff-stuff)
(require 'my-jabber-stuff)
(require 'my-web-mode-stuff)
(require 'my-ace-jump-stuff)
(require 'grep-a-lot)
(require 'repository-root)
;; grep-o-matic loaded before repository-root as instructed
(require 'grep-o-matic)
(require 'hlinum)
(require 'my-windows-management-stuff)
(require 'multiple-cursors)
(require 'undo-tree-mode)

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
(grep-a-lot-setup-keys)
(setq shell-file-name "bash")
(setq shell-command-switch "-ic")
(setq ring-bell-function 'jqt/ring-bell)
(setq global-undo-tree-mode 1)

;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;
(defun jqt/ring-bell ()
  "Do nothing."
  )

(defun jqt/copy-buffer-name ()
  "Puts selected buffer's name in the kill ring."
  (interactive)
  (kill-new (buffer-name)))

(defun jqt/continue (fun)
  "Helper method to set the repeat-key before calling `repeater-map'."
  (let ((repeat-key (event-basic-type last-input-event)))
    (repeater-map repeat-key fun)))

(defun jqt/continue-more (keymaps)
  "Helper method for not much at the moment..."
  (repeater-map-more keymaps))

;;;;;;;;;;;
;; modes ;;
;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;;;;;;;;;;;
;; hooks ;;
;;;;;;;;;;;
;; Maybe put those in the sub setup/stuff files.
;; nxml
(add-hook 'nxml-mode-hook
          '(lambda ()
             (linum-mode)
             (hlinum-activate)))
;; ediff
(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
(add-hook 'ediff-quit-hook 'my-ediff-qh 'append)

;;;;;;;;;;;;;;;
;; shortcuts ;;
;;;;;;;;;;;;;;;;;;;;
;; Emacs modified ;;
;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-r") 'replace-string)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-S-s") (lambda (name) (interactive "sName: ") (shell (concat ";shell " name))))
(global-set-key (kbd "C-S-t") (lambda (name) (interactive "sName: ") (ansi-term "bash" (concat "term: " name))))
(global-set-key (kbd "C-e") 'end-of-visual-line)
;;;;;;;;;;;;;;;;;;;;;;
;; custom rules:    ;;
;; - ! for inserts  ;;
;; - ? for messages ;;
;; - , for windows  ;;
;; - . for frames   ;;
;; - ; for shells   ;;
;; - " for dired    ;;
;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x C-b")   'jqt/buffer-list)
(global-set-key (kbd "C-! b")     'jqt/copy-buffer-name)
(global-set-key (kbd "C-c u")     'uncomment-region)
(global-set-key (kbd "M-Y")       'yank-pop-forwards)
(global-set-key (kbd "C-! t")     'jqt/insert-current-date-time)
(global-set-key (kbd "C-! s")     'jqt/insert-seconds-from-date)
(global-set-key (kbd "C-; r")     'jqt/reconnect-shell)
(global-set-key (kbd "C-; m d")   'mysql/desc-table)
(global-set-key (kbd "M-? t")     'jqt/convert-from-unix-timestamp)
(global-set-key (kbd "M-? p")     'jqt/point)
(global-set-key (kbd "C-\" o a")  'jqt/dired-athens)
(global-set-key (kbd "C-x F")     'ido-find-file-in-tag-files)
(global-set-key (kbd "C-@")       'browse-url)
;;;;;;;;;;;;;;;;;;
;; map specific ;;
;;;;;;;;;;;;;;;;;;
(define-key emacs-lisp-mode-map (kbd "C-c C-c")   'comment-box)
;;;;;;;;;;;;;;;;;;;;;;
;; multiple cursors ;;
;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
;;;;;;;;;;;
;; etags ;;
;;;;;;;;;;;
(global-set-key (kbd "M-.") 'etags-select-find-tag)



(provide 'my-setup)

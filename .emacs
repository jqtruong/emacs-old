;;;;;;;;;;;;;;;;
;; Setting up ;;
;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'load-path "~/.elisp")
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Fill in tabs with spaces.
(setq-default indent-tabs-mode nil)

(setq c-default-style "bsd"
      c-basic-offset 4)

(put 'downcase-region 'disabled nil)

;; Update path
(setq exec-path (append exec-path '("/usr/local/bin/")))

;; Create shorter aliases for ack-and-a-half
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; Set frame.
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
        ;; use 120 char wide window for largeish displays
        ;; and smaller 80 column windows for smaller displays
        ;; pick whatever numbers make sense for you
        (if (> (x-display-pixel-width) 1280)
            (add-to-list 'default-frame-alist (cons 'width 120))
          (add-to-list 'default-frame-alist (cons 'width 80)))
        ;; for the height, subtract a couple hundred pixels
        ;; from the screen height (for panels, menubars and
        ;; whatnot), then divide by the height of a char to
        ;; get the height we want
        (add-to-list 'default-frame-alist 
                     (cons 'height (/ (- (x-display-pixel-height) 200)
                                      (frame-char-height)))))))

(set-frame-size-according-to-resolution)

;;;;;;;;
;; nX ;;
;;;;;;;;

;; (load "~/.emacs.d/lisp/autostart")
;; (setq inhibit-trace nil)

;;;;;;;;;
;; sms ;;
;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/mypa/sms")
(require 'sms)

;;;;;;;;;;
;; bbdb ;;
;;;;;;;;;;

(require 'bbdb-loaddefs "~/.emacs.d/mypa/bbdb/lisp/bbdb-loaddefs.el")

;;;;;;;;;
;; Org ;;
;;;;;;;;;

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "~/Work/Nerdery/Nerdery.org"))

;;;;;;;;;;;
;; SLIME ;;
;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/mypa/slime-2012-07-25")

;; cldoc
(autoload 'turn-on-cldoc-mode "cldoc" nil t)

(dolist (hook '(lisp-mode-hook
                slime-repl-mode-hook))
  (add-hook hook 'turn-on-cldoc-mode))

;; paredit
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                slime-repl-mode-hook))
  (add-hook hook #'(lambda nil (paredit-mode 1))))

(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map [?\)] 'paredit-close-parenthesis)
     (define-key paredit-mode-map [(meta ?\))]
       'paredit-close-parenthesis-and-newline)))

;; SBCL
(setq slime-lisp-implementations
      '((sbcl ("/usr/local/bin/sbcl"))))

;; Setup
(require 'slime)
(slime-setup '(slime-fancy))

;;;;;;;;;;;;;;;;;;;
;; ibuffer stuff ;;
;;;;;;;;;;;;;;;;;;;

(setq ibuffer-saved-filter-groups 
        '(("ide"
           ("dired" (mode . dired-mode))
           ("Drupal" (filename . "drupal"))
           ("php" (name . "\\.php$\\|\\.inc$"))
           ("html" (name . "\\.html\\|\\.tpl"))
           ("css" (name . "\\.css"))
           ("js" (name . "\\.js\\(on\\)?"))
           ("iOS" (filename . "iOS"))
           ("emacs" (name . "^\\*.\*\\*$"))
           ("Org" (name . "\\.org$")))))

(add-hook 'ibuffer-mode-hook '(lambda () (ibuffer-auto-mode 1)))

;; Ensure ibuffer opens with point at the current buffer's entry.
(defadvice ibuffer (around ibuffer-point-to-buffer-in-other-window)
  (let ((recent-buffer-name (buffer-name (elt (buffer-list) 1))))
    ad-do-it
    (if (bound-and-true-p ide-mode-p)
        (progn 
          (ibuffer-jump-to-buffer recent-buffer-name)
          (ide/define-keys)))))
(ad-activate 'ibuffer)

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left elide)
              (size 9 -1 :right) " "
              (mode 16 16 :left :elide) " " 
              filename-and-process)
        (mark " " (name 16 -1) " " filename)
        (mark modified read-only " "
              (name 18 18 :left elide)
              filename)))

(setq ibuffer-show-empty-filter-groups nil)

;;(size 9 -1 :right) " "
;;(mode 16 16 :left :elide) " " filename-and-process)

;;;;;;;;;;;;;;;;;
;; Shell stuff ;;
;;;;;;;;;;;;;;;;;

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell 
         (replace-regexp-in-string "[[:space:]\n]*$" "" 
                                   (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

;;;;;;;;;;;;;;
;; My stuff ;;
;;;;;;;;;;;;;;
              
(defun ide ()
  ""
  (interactive)
  ;; Set list options for all consecutive directory browse after the first dired.
  (setq dired-listing-switches "-aogh")
  ;; Open current directory.
  (dired "." "-aogh")
  (rename-buffer ";base")
  ;; Open a terminal.
  ;; (term "/bin/bash")
  ;; (rename-buffer ";term")
  ;; Open a shell.
  (shell)
  (rename-buffer ";shell")
  ;; Create small side window.
  (split-window (selected-window) 50 0)
  ;; Open and set up ibuffer.
  (ibuffer)  
  (ibuffer-switch-to-saved-filter-groups "ide")
  (ibuffer-do-sort-by-alphabetic)
  ;; Dedicate this window to ibuffer only.
  (set-window-dedicated-p (selected-window) 1)
  (setq ide-mode-p t)
  (other-window 1))

(defun ide/resize-windows ()
  ""
  (interactive)
  (if (bound-and-true-p ide-mode-p)
      (progn
        (delete-other-windows)
        ;; Create small side window.
        (split-window (selected-window) 50 1)
        ;; Open and set up ibuffer.
        (ibuffer))))

(defun ide/ibuffer-window-dedicate ()
  ""
  (interactive)
  ;; I'm curious whether it was dedicated or not.
  (message "%s" (window-dedicated-p (window-at 0 0)))
  ;; In ide mode, the ibuffer window should be the leftmost one.
  (set-window-dedicated-p (window-at 0 0) 1))

(defun ide/ibuffer-visit-buffer-other-window ()
  ""
  (interactive)
  (let ((buf (ibuffer-current-buffer t)))
    (bury-buffer (current-buffer))
    ;; Switch to the second window since current should be the *Ibuffer* window.
    ;; (select-window (car (window-list)))
    ;; Since i'm using multiple windows now for other things, i need to specify the window's coords.
    (select-window (window-at 50 0))
    (switch-to-buffer buf)))

(defun jqt/window-list ()
  ""
  (interactive)
  (message "%s" (window-list)))

(ido-mode 1)

(defun jqt/buffer-list ()
  ""
  (interactive)
  ;; (other-window 1)
  ;; Since i'm using multiple windows now for other things, i need to specify the window's coords.
  (select-window (window-at 0 0))
  (ibuffer))

;; Copied from somewhere on the web.
(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))

(defun jqt/scroll-up-a-bit (line-count)
  ""
  (interactive "p")
  (next-line line-count)
  (scroll-up line-count))

(defun jqt/scroll-down-a-bit (line-count)
  ""
  (interactive "p")
  (previous-line line-count)
  (scroll-down line-count))

(defun jqt/equalize
  ""
  (interactive)
  ;; Find the furthest "=" in the region
  (search-backward "=" nil t)
  ;; Get the point
  ;; For all lines in the region with a "=", add spaces before their "=" until aligned with the furthest one
  ;; For all other lines, indent
  )

(defun jqt/count-matches-in-buffer (regexp)
  ""
  (interactive "sRegexp: ")
  (save-excursion
    (beginning-of-buffer)
    (number-to-string (count-matches regexp))))

;; (defun jqt/display-in-other-frame (buf)
;;   "Got code sample from: http://stackoverflow.com/questions/900372/in-emacs-how-do-i-change-the-minibuffer-completion-list-window"
;;   (save-excursion
;;     (if (= 1 (length (frame-list)))
;;         (make-frame))
;;     (other-frame 1)
;;     (set-window-buffer (selected-window) buf)))
;; 
;; (add-to-list 'special-display-buffer-names '("*Completions*" jqt/display-in-other-frame))
;; (add-to-list 'special-display-buffer-names '("*Help*" jqt/display-in-other-frame))
;; (add-to-list 'special-display-buffer-names '("*Ido Completions*" jqt/display-in-other-frame))

(defun jqt/dired-athens ()
  ""
  (interactive)
  (find-file "/ssh:jtruong@athens.sierrabravo.net:public_html/"))

(defun jqt/convert-from-unix-timestamp (seconds &optional insert-p)
  ""
  (interactive "nSeconds: ")
  (let ((date-string (format-time-string "%Y-%m-%d %T" (seconds-to-time seconds))))
    (if insert-p 
        (insert (format "%s" date-string))
      (message "%s" date-string))))

;;;;;;;;;;;;;;;
;; PHP stuff ;;
;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.php$\\|\\.inc$" . php-mode))

(defun php/previous-function ()
  ""
  (interactive)
  (search-backward-regexp "function +[[:word:]_]+\s*\(.*\)[\s\n]*{" nil t)
  (recenter))

(defun php/next-function ()
  ""
  (interactive)
  (search-forward-regexp "function +[[:word:]_]+\s*\(.*\)[\s\n]*{" nil t)
  (recenter))

(defun php/function-signature ()
  ""
  (interactive)
  (save-excursion
    (search-backward-regexp "function +[[:word:]_]+\s*\(.*\)\\([\s\n]*{\\)?" nil t)
    (let ((start (point)))
      (search-forward "{")
      (message "%s" (buffer-substring start (point))))))

;;;;;;;;;;;;;;;
;; iOS stuff ;;
;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("/iOS/.*\\.h$" . objc-mode))

(fset 'ios/synthesize-property
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([67108896 5 134217826 23 64 115 121 110 116 104 101 115 105 122 101 32 67108896 5 2 134217847 32 61 32 95 25 5 14 1] 0 "%d")) arg)))

(fset 'ios/release-synthesized-property
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([67108896 19 95 13 2 23 91 5 2 32 114 101 108 101 97 115 101 93 1 14 tab] 0 "%d")) arg)))

(defun ios/import-file (name)
  ""
  (interactive "sClass: ")
  (save-excursion
    ;; Move to top
    (beginning-of-buffer)
    ;; Look for last import or the possible autogenerated comments at the top of Cocoa files.
    (if (search-forward "#import" nil t)
        (while (search-forward "#import" nil t))
      (while (search-forward "//" nil t)))
    ;; Move to end of line and insert the import in a new line
    (end-of-line)
    (newline-and-indent)
    (insert (format "#import \"%s.h\"" name))))

(defun ios/add-interface-and-implementation (name)
  ""
  (interactive "sName: ")
  ;; Set interface and implementation file names.
  (let ((interface (concat name ".h"))
        (implementation (concat name ".m")))
    ;; Check first that file does not exist.
    (unless (file-exists-p interface)
      ;; Create file.
      (find-file interface)
      ;; Stub it out.
      )))

(defun ios/add-smart-property-old (setter type name)
  ""
  (interactive "sSetter: \nsType: \nsName: ")
  (save-excursion
    ;; Some class names are categories, including '+' and a string before the extension.
    (let ((class (replace-regexp-in-string "\\(\\+.*\\)?\\(\\.h\\|\\.m\\)$" "" (buffer-name))))
      ;; Switch to interface buffer.
      (if (string-match "\\.m$" (buffer-name))
          (switch-to-buffer (ios/get-counterpart-buffer (buffer-name))))
      ;; Move to the interface's definition.
      (beginning-of-buffer)
      (search-forward (concat "@interface " class))
      ;; Move to the end of it.
      (search-forward "@end")
      ;; Move to the last property.
      (unless (search-backward "@property" nil t)
        (previous-line)
        (newline-and-indent)
        (previous-line))
      ;; Insert the property on a new line.
      (end-of-line)
      (newline-and-indent)
      (insert (format "@property (nonatomic, %s) %s %s;" setter type (if (string= setter "retain") (concat "*" name) name)))
      
      ;; Switch to the implementation.
      (switch-to-buffer (ios/get-counterpart-buffer (buffer-name)))
      ;; Move to the implementation's definition.
      (beginning-of-buffer)
      (search-forward (concat "@implementation " class))
      ;; Move to the end of it.
      (search-forward "@end")
      ;; Move to the last synthesized property.
      (unless (search-backward "@synthesize" nil t)
        (previous-line)
        (newline-and-indent)
        (previous-line))
      ;; Add a new line, indent, and insert the synthesizer.
      (end-of-line)
      (newline-and-indent)
      (insert (format "@synthesize %s = _%s;" name name))
      
      (if (string= setter "retain")
          (progn
            ;; Search and add dealloc selector if necessary.
            (unless (search-forward-regexp "-\s*\(void\)\s*dealloc" nil t)
              (search-forward "@end")
              (previous-line)
              (newline-and-indent)
              (insert "- (void)dealloc\n{")
              (newline-and-indent)
              (insert "[super dealloc];\n}\n")
              (search-backward "dealloc" nil nil 2)
              (end-of-line))
            ;; Move to super's dealloc.
            (search-forward "[super dealloc];")
            ;; Move to the last released property.
            (unless (search-backward "release];" nil t)
              (previous-line))
            ;; Add a new line, indent, and release the property.
            (end-of-line)
            (newline-and-indent)
            (insert (format "[_%s release];" name))))
      ) ; let
    ) ; save-excursion
  ) ; defun

(defun ios/add-smart-property (setter type name)
  ""
  (interactive "sSetter: \nsType: \nsName: ")
  (save-excursion
    ;; Some class names are categories, including '+' and a string before the extension.
    (let ((class (replace-regexp-in-string "\\(\\+.*\\)?\\(\\.h\\|\\.m\\)$" "" (buffer-name)))
          (pointer? (or (string= setter "strong") (string= setter "retain"))))
      ;; Switch to interface buffer.
      (if (string-match "\\.m$" (buffer-name))
          (switch-to-buffer (ios/get-counterpart-buffer (buffer-name))))
      ;; Move to the interface's definition.
      (beginning-of-buffer)
      (search-forward (concat "@interface " class))
      ;; Move to the end of it.
      (search-forward "@end")
      ;; Move to the last property.
      (unless (search-backward "@property" nil t)
        (previous-line)
        (newline-and-indent)
        (previous-line))
      ;; Insert the property on a new line.
      (end-of-line)
      (newline-and-indent)
      (insert (format "@property (%s, nonatomic) %s %s;" setter type (if pointer? (concat "*" name) name)))
      
      ;; Switch to the implementation.
      (switch-to-buffer (ios/get-counterpart-buffer (buffer-name)))
      ;; Move to the implementation's definition.
      (beginning-of-buffer)
      (search-forward-regexp (concat "@implementation " class "$"))
      (let ((p (point)))
        (search-forward "@end" nil t)
        (unless (search-backward "@synthesize" nil t)
          (goto-char p)
          (end-of-line)
          (newline-and-indent)))
      (end-of-line)
      (newline-and-indent)
      (insert (format "@synthesize %s = _%s;" name name)))))

(defun ios/remove-property (name)
  ""
  (interactive "sName: "))

(defun ios/add-property (alloc type name)
  ""
  (insert (format "@property (nonatomic, %s) %s %s;" alloc type name))
  (next-line))

(defun ios/add-property-retain (type name)
  ""
  (interactive "sType: \nsName: ")
  (ios/add-property "retain" type (concat "*" name)))

(defun ios/add-property-assign (type name)
  ""
  (interactive "sType: \nsName: ")
  (ios/add-property "assign" type name))

(defun ios/get-counterpart-buffer (name)
  ""
  (if (string-match "\\.m$" name)
      (replace-regexp-in-string "\\.m$" ".h" name)
    (if (string-match "\\.h$" name)
        (replace-regexp-in-string "\\.h$" ".m" name))))

(defun ios/get-class-name-from-buffer ()
  ""
  (interactive)
  (insert (replace-regexp-in-string "\\.h\\|\\.m$" "" (buffer-name))))

(defun ios/previous-selector ()
  ""
  (interactive)
  (search-backward-regexp "^[+-]\s*\(.+).+" nil t)
  (recenter))

(defun ios/previous-pragma-mark ()
  ""
  (interactive)
  (search-backward-regexp "^#pragma mark" nil t)
  (recenter))

(defun ios/next-selector ()
  ""
  (interactive)
  (search-forward-regexp "^[+-]\s*\(.+).+" nil t)
  (recenter))

(defun ios/next-pragma-mark ()
  ""
  (interactive)
  (search-forward-regexp "^#pragma mark" nil t)
  (recenter))

(defun ios/switch-to-counterpart ()
  ""
  (interactive)
  ;; Looks like ibuffer may interfere with switch-to-buffer's second arg NORECORD...
  (switch-to-buffer (ios/get-counterpart-buffer (buffer-name)) 1))

(defun ios/method-signature ()
  ""
  (interactive)
  (save-excursion
    (search-backward-regexp "^[+-]\s*\(.+).+" nil t)
    (let ((start (point)))
      (end-of-line)
      (message "%s" (buffer-substring start (point))))))

(defun ios/implementation-signature ()
  ""
  (interactive)
  (save-excursion
    (search-backward-regexp "^@implementation" nil t)
    (let ((start (point)))
      (end-of-line)
      (message "%s" (buffer-substring start (point))))))

(defun ios/create-instance-old (class)
  ""
  (interactive "sClass: ")
  (let* ((index (jqt/count-matches-in-buffer (concat class " *\\* *temp" class)))
         (index (if (string= index "0") "" index)))
    (previous-line)
    (end-of-line)
    (newline-and-indent)
    (insert class " *temp" class index " = [[" class " alloc] init];")
    (next-line)
    (end-of-line)
    (insert "temp" class index ";")
    (newline-and-indent)
    (insert "[temp" class index " release];")
    (newline-and-indent)
    (search-backward "init")
    (forward-word)))

(defun ios/create-instance (class)
  ""
  (interactive "sClass: ")
  (insert "[[" class " alloc] init];")
  (search-backward "init")
  (forward-word))

(defadvice ido-find-file (after ido-find-file-ios-counterpart activate)
  ""
  (if (string-match "/iOS/" (buffer-file-name))
      (progn 
        (if (string-match "\\.m$" (buffer-name))
            (find-file (replace-regexp-in-string "\\.m$" ".h" (buffer-name)))
          (if (string-match "\\.h$" (buffer-name))
              (find-file (replace-regexp-in-string "\\.h$" ".m" (buffer-name)))))
        (switch-to-buffer nil))))

;;;;;;;;;;;;;
;; c stuff ;;
;;;;;;;;;;;;;

(defun c/insert-buffer-name ()
  ""
  (interactive)
  (insert (buffer-name)))

;;;;;;;;;;;;;;;;;;
;; key bindings ;;
;;;;;;;;;;;;;;;;;;

;; mac specific settings
(when (eq system-type 'darwin) 
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  ;; sets fn-delete to be right-delete
  (global-set-key [kp-delete] 'delete-char))

;; Emacs modified
(global-set-key (kbd "M-r") 'replace-string)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-S-n") (lambda () (interactive) (jqt/scroll-up-a-bit 3)))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (jqt/scroll-down-a-bit 3)))
(global-set-key (kbd "C-S-s") (lambda (name) (interactive "sName: ") (shell (concat ";shell " name))))
(global-set-key (kbd "C-S-t") (lambda (name) (interactive "sName: ") (term "/bin/bash") (rename-buffer (concat ";term " name))))
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-frame 1)))

;; Custom all
(global-set-key (kbd "C-x C-b") 'jqt/buffer-list)
(global-set-key (kbd "C-c i b") 'c/insert-buffer-name)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "M-Y") 'yank-pop-forwards)
(global-set-key (kbd "C-c m u") 'windmove-up)
(global-set-key (kbd "C-c m l") 'windmove-left)
(global-set-key (kbd "C-c m r") 'windmove-right)
(global-set-key (kbd "C-c m d") 'windmove-down)

;; Custom PHP
(defun php/define-keys ()
  ""
  (interactive)
  (define-key php-mode-map (kbd "M-p") 'php/previous-function)
  (define-key php-mode-map (kbd "M-n") 'php/next-function))

;; Custom objc
(defun ios/define-keys ()
  ""
  (interactive)
  (define-key objc-mode-map (kbd "C-c a s p") 'ios/add-smart-property)
  (define-key objc-mode-map (kbd "C-c a p r") 'ios/add-property-retain)
  (define-key objc-mode-map (kbd "C-c a p a") 'ios/add-property-assign)
  (define-key objc-mode-map (kbd "C-c s p") 'ios/synthesize-property)
  (define-key objc-mode-map (kbd "C-c r s") 'ios/release-synthesized-property)
  (define-key objc-mode-map (kbd "C-c i c") 'ios/get-class-name-from-buffer)
  (define-key objc-mode-map (kbd "C-c i i") 'ios/import-file)
  (define-key objc-mode-map (kbd "C-c i n") 'ios/create-instance)
  (define-key objc-mode-map (kbd "C-c s f") 'ios/method-signature)
  (define-key objc-mode-map (kbd "C-c s c") 'ios/implementation-signature)
  (define-key objc-mode-map (kbd "C-c o") 'ios/switch-to-counterpart)
  (define-key objc-mode-map (kbd "M-p") 'ios/previous-selector)
  (define-key objc-mode-map (kbd "M-n") 'ios/next-selector)
  (define-key objc-mode-map (kbd "M-P") 'ios/previous-pragma-mark)
  (define-key objc-mode-map (kbd "M-N") 'ios/next-pragma-mark))

;; Custom ibuffer
(defun ide/define-keys ()
  ""
  (interactive)
  (define-key ibuffer-mode-map "o" 'ide/ibuffer-visit-buffer-other-window))


;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;
(defun jqt/two-third-it-up ()
  "With the current buffer and full screen window, split it in two and
make the left one eshell and 1/3 width, and the remaining 2/3 be the
  current buffer"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (switch-to-buffer (other-buffer))
  (other-window -1)
  (delete-window)
  (other-window 1))

(defun jqt/toggle-window-dedication (&optional on)
  ""
  (interactive "P")
  (if on
      (set-window-dedicated-p (selected-window) t)
    (set-window-dedicated-p (selected-window) (not (window-dedicated-p (selected-window)))))
  (message "Dedicated? %s" (window-dedicated-p (selected-window))))

(defun jqt/flip-windows-buffers ()
  "Move each window's buffer to the next available window."
  (interactive)
  ())

(defun jqt/split-to-compare (&optional below)
  "Split current window to compare last two buffers.

If OTHER is nil, stay in current window, else..."
  (interactive "P P")
  (if below
      (split-window-below)
    (split-window-right))
  (other-window 1)
  (set-window-buffer (selected-window) (other-buffer)))

(defun jqt/kill-next-window (&optional num)
  "Kill next NUM window."
  (interactive "p")
  (other-window (* (jqt/unit num) 1))
  (delete-window)
  (when (and num
             (not (= (abs num) 1))
             (> (length (window-list)) 1))
    (message (format "%d" num))
    (jqt/kill-next-window (if (> num 0)
                              (1- num)
                            (1+ num)))))

(defun jqt/kill-other-frame ()
  "Kill other live frame."
  (interactive)
  (when (> (length (frame-list)) 1)
    (other-frame 1)
    (delete-frame)))

(defun jqt/other-frame-or-create ()
  "Switch to other frame or create."
  (interactive)
  (if (> (length (frame-list)) 1)
      (other-frame 1)
    (ido-switch-buffer-other-frame)))

(defun jqt/unit (num)
  "Returns 1 or -1 based on NUM.

E.g. if NUM is 6, then returns 1.
E.g. if NUM is -6, then returns -1"
  (/ num (abs num)))

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
(fset 'jqt/split-window-into-three
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 49 24 51 24 51 24 43 33554438 24 98 return 33554438 24 98 return] 0 "%d")) arg)))

;;;;;;;;;;;
;; hooks ;;
;;;;;;;;;;;
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map [(control tab)] nil)))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;
;; custom rules:    ;;
;; - , for windows  ;;
;; - . for frames   ;;
;;;;;;;;;;;;;;;;;;;;;;
;; windows
(global-set-key (kbd "C-, c")             'jqt/split-to-compare)
(global-set-key (kbd "C-, k")             'jqt/kill-next-window)
(global-set-key (kbd "C-, RET")           'jqt/toggle-window-dedication)
(global-set-key (kbd "C-, l")             'jqt/last-buffer)
;; frames                                 
(global-set-key (kbd "C-. o")             'jqt/other-frame-or-create)
(global-set-key (kbd "C-. k")             'jqt/kill-other-frame)
;; buffers                                
(global-set-key (kbd "C-S-b")             'previous-buffer)
(global-set-key (kbd "C-S-f")             'next-buffer)



(provide 'my-windows-management-stuff)

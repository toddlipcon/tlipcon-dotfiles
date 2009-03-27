
(defun smart-comint-up ()
   "Implement the behaviour of the up arrow key in comint mode.  At
the end of buffer, do comint-previous-input, otherwise just move in
the buffer."
   (interactive)
   (let ((previous-region-status (if xemacs-p (region-active-p) nil)))
     (if (= (point) (point-max))
        (comint-previous-input 1)
       (previous-line 1))
     (when previous-region-status
       (activate-region))))

(defun smart-comint-down ()
   "Implement the behaviour of the down arrow key in comint mode.  At
the end of buffer, do comint-next-input, otherwise just move in the
buffer."
   (interactive)
   (let ((previous-region-status (if xemacs-p (region-active-p) nil)))
     (if (= (point) (point-max))
        (comint-next-input 1)
       (forward-line 1))
     (when previous-region-status
       (activate-region))))

;;; Set up the smart comint arrow keys.  See smart-comint-up/down.
(defun setup-smart-comint-arrow-keys ()
   "Set up the smart comint arrow keys.  See smart-comint-up/down."
   (interactive)
   (local-set-key [up] 'smart-comint-up)
   (local-set-key [down] 'smart-comint-down))

;;; This hook will be called when entering several `comint' modes:
;;; 'gud/gdb', `shell', `term', `ielm', `tex-shell', 'inferior-ess'.
(mapcar #'(lambda (hook) (add-hook hook 'setup-smart-comint-arrow-keys))
        (list 'gdb-mode-hook 'shell-mode-hook 'term-mode-hook
              'ielm-mode-hook 'tex-shell-hook 'inferior-ess-mode-hook
              'erlang-shell-mode-hook))

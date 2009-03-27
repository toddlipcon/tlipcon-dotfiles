
(defun wheel-up ()
  (interactive "")
  (scroll-down 2))
(defun wheel-down ()
  (interactive "")
  (scroll-up 2))

(global-set-key [(button4)] 'wheel-up)
(global-set-key [(button5)] 'wheel-down)

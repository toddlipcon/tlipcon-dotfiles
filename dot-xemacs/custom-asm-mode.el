
(defun my-asm-settings ()
  "set up asm settings for spim"
  (interactive)
  (make-local-variable 'asm-comment-char)
  (setq asm-comment-char ?\#)
  (font-lock-mode)
  (defadvice asm-colon (around no-special-colon-in-comments activate)
    (interactive)
    (if (and
	 (>= (point)
	     (save-excursion
	       (beginning-of-line)
	       (or (re-search-forward (format "%c" asm-comment-char) (eolp) t)
		   (end-of-line))
	       (point)))
	 (not (asm-line-matches (format "^[^%c\n]+$" asm-comment-char)))
	 )
	 (insert ":")
      ad-do-it)))

(defun my-asm-comment ()
 "an asm comment that makes me happier"
  (interactive)
  (cond
   ;; blank line? Start at beginning of line
   ((asm-line-matches "^[ \t]*$")
    (delete-horizontal-space)
    (insert asm-comment-char))
   ;; Code on the line? Go to comment column and comment there
   ((asm-line-matches (format "^[^%c\n]+$" asm-comment-char))
    (indent-for-comment))
   ;; else just insert a regular #
   (t
    (insert asm-comment-char))))

(add-hook 'asm-mode-set-comment-hook 'my-asm-settings)
(add-hook 'asm-mode-hook
	  (lambda()
	    (interactive)
	    (defun asm-comment ()
	      (interactive)
	      (my-asm-comment)
	      )
	    (font-lock-fontify-buffer)
	    (setq comment-column 64)
	    ))

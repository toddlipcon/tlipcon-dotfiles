
(defconst my-c-style
  '((c-tab-always-indent	.	nil)
    (c-access-key		.	"\\<\\(signals\\|public\\|protected\\|private\\|public slots\\|protected slots\\|private slots\\):")
    (c-comment-only-line-offset	.	0)
    (c-offsets-alist		.	((substatement-open	. 0)
					 (case-label		. +)
                                         (access-label          . -1)
					 (inline-open		. 0)
                                         (inextern-lang         . 0)
                                         (arglist-intro         . ++)
                                         (innamespace           . 0)))
    )
  "My C Programming Style")

(defun my-tab-command ()
  "My Tab Function"
  (interactive)
  (let (
	curpos
	result
	)
    (if	(save-excursion
	  (setq curpos (point))
	  (beginning-of-line)
	  (setq result (and (> (skip-chars-forward " \t") 0)
			    (> (skip-chars-forward "a-zA-Z1-9_") 0)	  
			    (or (> (skip-chars-forward " \t") 0)
				(eolp))
			    (>= (point) curpos)
			    )
		)
	  result
	  )
	( progn
	  (indent-relative)
	  (message "Used relative (res %s)" result)
	  )
      (
       progn
	(c-indent-command)
	(message "Used c-indent")
	)
      )
    )
  )

(defconst xemacs-p
  (not (null (save-match-data (string-match "XEmacs\\|Lucid" emacs-version)))))

(require 'ycmd)
(require 'company-ycmd)

(defun my-c-mode-common-hook ()
  (c-add-style "PERSONAL" my-c-style t)
  (c-set-style "personal")
  (setq c-basic-offset 2)
  (setq tab-width 2)
  (setq fill-column 90)
  (font-lock-mode)
  (line-number-mode t)
  (google-set-c-style)
  (ycmd-mode)
  (company-mode)
  (company-ycmd-setup)
  (flycheck-mode)
  (flycheck-ycmd-setup)
  (flyspell-prog-mode)
;  (imenu-add-menubar-index)
)

; flyspell overrides C-., but would rather use it for flycheck-next-error
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-.") nil))

(defun my-cperl-indent ()
  "My cperl tab"
  (interactive)
  (let (
	startpos
	cperlthinks
	oldeol
	neweol
	)
    (if
	(looking-at "^#")
	(save-excursion	  
	  (beginning-of-line)
	  (insert-tab)
	  )
      )
    (cperl-indent-command)
    (setq startpos (point))
    (if (save-excursion
	  (end-of-line)
	  (setq oldeol (point))
	  (beginning-of-line)
	  (skip-chars-forward " \t")	  
	  (setq cperlthinks (current-column))
	  (looking-at "#")
	  )
	(progn
	  (beginning-of-line)
	  (delete-horizontal-space)
	  (indent-to (- cperlthinks cperl-indent-level))
	  (end-of-line)
	  (goto-char (- startpos (- oldeol (point))))
	  )
      )   
    )
  )

(defun my-cperl-mode-hook ()
  (setq tab-width 4)
  (setq cperl-indent-level 4)
  (setq cperl-extra-newline-before-brace nil)
  (setq cperl-break-one-line-blocks-when-indent nil)
  (setq cperl-indent-left-aligned-comments nil)
  (setq cperl-comment-column 1)
  (setq cperl-indent-parens-as-block t)
  (font-lock-mode)
  (line-number-mode t)
  (cperl-define-key "\t" 'my-cperl-indent)
  (substitute-key-definition
   'indent-for-comment 'my-cperl-indent
     cperl-mode-map global-map
     )
  (cperl-set-style "C++")

)

(defun my-php-mode-hook ()
  (setq tab-width 4)
  (setq ident-tabs-mode t)
)

(defun my-java-mode-hook ()
  (setq c-basic-offset 2)
  (setq ident-tabs-mode nil)
  (font-lock-mode)
  (line-number-mode t)
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'cperl-mode-hook 'my-cperl-mode-hook)
(add-hook 'php-mode-hook 'my-php-mode-hook)
(add-hook 'java-mode-hook 'my-java-mode-hook)


(add-hook 'sql-mode-hook (lambda ()
			   (sql-set-product 'mysql)
			   (font-lock-mode)
			   ))

(set-variable 'sgml-default-doctype-name "HTML")


;; make backup files .foo~ instead of foo~
(defun make-backup-file-name (orig)
  (concat (file-name-directory orig)
          "/."
          (file-name-nondirectory orig)
          "~"))


;; ruby-mode
(autoload 'ruby-mode "ruby-mode" "Load ruby-mode")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

(add-hook 'ruby-mode-hook
          (lambda ()
            (turn-on-font-lock)
            (define-key ruby-mode-map [(meta backspace)] 'backward-kill-word)))

;; cycle buffer
(load-file "~/.xemacs/cycle-buffer.el")
(global-set-key [(control x) right] 'cycle-buffer)
(global-set-key [(control x) left] 'cycle-buffer-backward)


;; uniquify - change buffer names to eg Makefile|dir1 and Makefile|dir2 instead of Makefile and Makefile<2>
(load-library "uniquify")
(set-variable 'uniquify-buffer-name-style 'post-forward)

;; iswitchb - better buffer switching
;(load-library "iswitchb")
;(iswitchb-mode 1)
;; when switching to a buffer that's open in another frame, open it
;; in this window as well, rather than raising that frame
;(set-variable 'iswitchb-default-method 'samewindow)

(load-file "~/.xemacs/fcsh-mode.el")
(load-file "~/.xemacs/erlang-custom.el")
(load-file "~/.xemacs/smart-comint.el")
(load-file "~/.xemacs/cloudera.el")
(load-file "~/.xemacs/scroll-wheel.el")
(load-file "~/.xemacs/custom-asm-mode.el")
(load-file "~/.xemacs/thrift.el")

;; ack mode
(load-file "~/.xemacs/xemacs-ack.el")

(global-set-key [(control return)] 'compile)

; Use C++ mode for .h files
(setq auto-mode-alist
      (append auto-mode-alist '(("\.h:"   . c++-mode))))

(defconst xemacsp (string-match "Lucid\\|XEmacs" emacs-version) "\
Non nil if using XEmacs.")

(if xemacsp (gnuserv-start) (server-start))

; (setq semantic-load-turn-useful-things-on t)
; (require 'semantic-load)
; (require 'semantic-ia)


(defconst my-c-style
  '((c-tab-always-indent	.	nil)
    (c-access-key		.	"\\<\\(signals\\|public\\|protected\\|private\\|public slots\\|protected slots\\|private slots\\):")
    (c-comment-only-line-offset	.	0)
    (c-offsets-alist		.	((substatement-open	. 0)
					 (case-label		. +)
					 (inline-open		. 0)))
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


(defun my-c-mode-common-hook ()
  (c-add-style "PERSONAL" my-c-style t)
  (setq tab-width 4)
  (font-lock-mode)
  (line-number-mode t)
)

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

(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 0)

(add-hook 'mmm-mode-hook (lambda ()
			   (mmm-set-major-mode-preferences 'html 'xml-mode)
			   ))


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
(iswitchb-default-keybindings)

(load-file "~/.xemacs/custom-fonts.el")
(load-file "~/.xemacs/lineker.el")
(load-file "~/.xemacs/fcsh-mode.el")
(load-file "~/.xemacs/erlang-custom.el")
(load-file "~/.xemacs/smart-comint.el")
(load-file "~/.xemacs/cloudera.el")
(load-file "~/.xemacs/scroll-wheel.el")
(load-file "~/.xemacs/custom-asm-mode.el")
(load-file "~/.xemacs/thrift.el")

(global-set-key [(control return)] 'compile)

; Use C++ mode for .h files
(setq auto-mode-alist
      (append auto-mode-alist '(("\.h:"   . c++-mode))))

(gnuserv-start)

; (setq semantic-load-turn-useful-things-on t)
; (require 'semantic-load)
; (require 'semantic-ia)


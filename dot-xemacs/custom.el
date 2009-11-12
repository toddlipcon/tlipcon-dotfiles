(load-file "~/.xemacs/darkcolor.el")

;(set-face-font 'default '("-*-fixed-medium-r-*--14-*-iso8859-1"));;



(defvar compilation-directory nil
  "*If non-nil, the directory in which the `\[compile]` command runs.
If nil, the value of `default-directory` is used.")

(defadvice compile (around override-default-directory activate)
  (let ((default-directory (or compilation-directory default-directory)))
    ad-do-it))

(setq load-path
      (append load-path '("/Users/tlipcon/.xemacs/")))

(setq cperl-invalid-face nil)

(setq minibuffer-max-depth nil)
(put 'narrow-to-region 'disabled nil)
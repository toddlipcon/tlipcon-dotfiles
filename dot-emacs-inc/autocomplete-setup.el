(require 'auto-complete)
(load-file "~/.emacs-inc/auto-complete-clang.el")
(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (split-string
               "
 /usr/include/c++/4.6
 /usr/include/c++/4.6/x86_64-linux-gnu/.
 /usr/include/c++/4.6/backward
 /usr/lib/gcc/x86_64-linux-gnu/4.6/include
 /usr/local/include
 /usr/lib/gcc/x86_64-linux-gnu/4.6/include-fixed
 /usr/include/x86_64-linux-gnu
 /usr/include
 /home/todd/git/kudu/src
 /home/todd/git/kudu/thirdparty/installed/include
 /home/todd/git/kudu/thirdparty/gtest-1.6.0/include
"
               )))

(setq ac-auto-start nil)
(define-key ac-mode-map (kbd "C-M-/") 'auto-complete)
(define-key ac-completing-map "\e" 'ac-stop)

(setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ) ac-sources))
  (auto-complete-mode t)
  (set-cursor-color "gray70"))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
(add-hook 'c++-mode-hook 'my-ac-cc-mode-setup)

(defun cloudera-insert-copyright ()
  "Insert the standard cloudera copyright notice at the top of the current
   buffer."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (cond
     ((or (string-match "\\.py$" (buffer-file-name))
          (string-match "/targets$" (buffer-file-name)))
      (progn
        (insert "#!/usr/bin/env python2.5")
        (newline)
        (insert "# (c) Copyright 2009 Cloudera, Inc.")
        (newline))))))

(defun apache-insert-license ()
  "Insert the Java Apache 2.0 license at the top of the file."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (cond
     ((string-match "\\.java$" (buffer-file-name))
      (insert-file "~/.xemacs/data/apache.license")))))

; HTML mode - use hm--html-mode
(setq auto-mode-alist (cons '("\\.html$" . hm--html-mode)
                            auto-mode-alist))

(add-hook 'hm--html-mode-hook
          (lambda ()
            (setq tab-width 2)
            (setq indent-tabs-mode t)
            (setq hm--html-automatic-changed-comment nil)
            (setq hm--html-automatic-create-title-date nil)
))


; Python mode for targets files
(setq auto-mode-alist (cons '("targets$" . python-mode)
                            auto-mode-alist))

; Settings for python style
(add-hook 'python-mode-hook
          (lambda ()
            (setq py-indent-offset 2)
            (setq indent-tabs-mode nil)))

; java style
(defun set-c-settings ()
  "Set up cc-mode general settings common to several modes."
  (setq c-basic-offset 2)
  (setq indent-tabs-mode nil))

(add-hook 'java-mode-hook 'set-c-settings)
(add-hook 'jde-mode-hook 'set-c-settings)

;; highlight trailing whitespaces in all modes - our coding guidelines say
;; trailing whitespace is bad
(setq-default show-trailing-whitespace t)

;; delete all the trailing whitespaces and tabs across the current buffer
(defun cloudera-delete-trailing-whitespaces-and-untabify ()
  "Delete all the trailing white spaces, and convert all tabs to multiple
spaces across the current buffer."
  (interactive "*")
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max)))


(global-set-key [(control c) (t)] 'my-delete-trailing-whitespaces-and-untabify)

;; show the line number in each mode line
(line-number-mode 1)

;; show the column number in each mode line
(column-number-mode 1)
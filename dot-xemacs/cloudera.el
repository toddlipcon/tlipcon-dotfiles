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
        (insert "# (c) Copyright 2009 Cloudera, Inc. All rights reserved.")
        (newline))))))

(defun apache-insert-license ()
  "Insert the Java Apache 2.0 license at the top of the file."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (cond
     ((string-match "\\.java$" (buffer-file-name))
      (insert-file "~/.xemacs/data/apache.license")))))

(defun wikify-jira-links ()
   "Convert <proj>-<num> to {(jh|jc):<proj>-<num>}"
   (interactive)
   (setq areg "\\(hadoop\\|hdfs\\|mr\\|mapreduce\\|avro\\|hbase\\)-[0-9]+")
   (setq creg "\\(cloudera\\|cdh\\|desktop\\)-[0-9]+")
   (save-excursion
     (goto-char 0)
     (replace-regexp "{j[ch]:\\([^}]+\\)}" "\\1")
     (goto-char 0)
     (replace-regexp areg "{jh:\\\&}")
     (replace-regexp creg "{jc:\\\&}")
     (replace-string "{jh:mr-" "{jh:mapreduce-")))

(define-skeleton tex-bullets-slide
  "Inserts a bullet point slide."
  "Title: "
  "\\begin{frame}[t]\n"
  "\\frametitle{" str | "Title" "}\n"
  "\\begin{itemize}\n"
  "  \\item " _ "\n"
  "\\end{itemize}\n"
  "\\end{frame}\n\n"
  )


(defun oah ()
  "Shortcut for org.apache.hadoop"
  (interactive)
  (insert "org.apache.hadoop"))

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
  (interactive)
  (setq c-basic-offset 
        (if (string-match "jcarder" (buffer-file-name)) ;; jcarder uses 4 indent
            4
          2))
  (setq indent-tabs-mode nil))

(if (not (functionp 'replace-in-string))
  (defun replace-in-string (str search replace)
    (replace-regexp-in-string (regexp-quote search) replace str)))

(defun switch-to-test ()
  "Switch to corresponding unit test."
  (interactive)
  (let ((newf
         (if
             (string-match "/Test[^/]+\\.java$" (buffer-file-name))
             (let*
                 ((newf (replace-regexp-in-string "/Test\\([^/]+\\.java\\)$" "/\\1" (buffer-file-name)))
                  (newf (replace-in-string newf "hadoop-hdfs/src/test/hdfs" "hadoop-hdfs/src/java"))
                  (newf (replace-in-string newf "hadoop-common/src/test/core" "hadoop-common/src/java"))
                  (newf (replace-in-string newf "hadoop-mapreduce/src/test/mapred" "hadoop-common/src/java")))
               newf)
           (let*
               ((newf (replace-regexp-in-string "/\\([^/]+\\.java\\)$" "/Test\\1" (buffer-file-name)))
                (newf (replace-in-string newf "hadoop-hdfs/src/java" "hadoop-hdfs/src/test/hdfs"))
                (newf (replace-in-string newf "hadoop-common/src/java" "hadoop-common/src/test/core"))
                (newf (replace-in-string newf "hadoop-mapreduce/src/java" "hadoop-common/src/test/mapred")))
             newf))))
    (message "Switching to file %s..." newf)
    (find-file newf)))
(global-set-key [(control x) (t)] 'switch-to-test)




(add-hook 'java-mode-hook 'set-c-settings)
(add-hook 'jde-mode-hook 'set-c-settings)
(add-hook 'c++-mode-hook 'set-c-settings)

;; use C++ mode for .h files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

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

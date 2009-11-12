(load "font-lock")

(set-face-font 'default "-*-fixed-medium-r-*--10-*-iso8859-1");

(cond ((fboundp 'global-font-lock-mode)
       ;; Customize face attributes
       (setq font-lock-face-attributes
             ;; Symbol-for-Face Foreground Background Bold Italic Underline
             '((font-lock-comment-face       "Purple")
               (font-lock-string-face        "green")
               (font-lock-keyword-face       "orange red")
               (font-lock-function-name-face "orange1")
               (font-lock-variable-name-face "white")
               (font-lock-type-face          "white" "black" "false"
"false" "true")
               (font-lock-constant-face     "Purple")
               ))
       ;; Load the font-lock package.
       (require 'font-lock)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)))


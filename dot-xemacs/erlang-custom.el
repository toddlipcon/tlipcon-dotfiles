(defun add-erlang-skel (skel)
  (set-variable 'erlang-skel (append erlang-skel skel)))

(defvar erlang-skel-function 
  '((erlang-skel-skip-blank) o >
    "%%--------------------------------------------------------------------" n
    "%% Function: "
    (P "Function name: " funcname)
    "(" (P "Args: " args) ") -> "
    (P "Return spec: ") n
    "%% " n
    "%% Description: " n
    "%%--------------------------------------------------------------------" n
    (s funcname) "(" (s args) ") ->"))

(defun add-erlang-skel-function ()
  (add-erlang-skel '(() ; separator
                     ("Function" "function" erlang-skel-function)))
  (erlang-skel-init))

(add-hook 'erlang-mode-hook
          'add-erlang-skel-function)
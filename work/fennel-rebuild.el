(defun rebuild-fennel ()
  (let (
        (fennel-dir-path (expand-file-name "_app/fennel-ls" init-file-dir))
        (fennel-ls-path (expand-file-name "fennel-ls" fennel-dir-path))
        )
    (default-directory fennel-dir-path)
    (delete-file fennel-fir-path)
    (shell-command "make" (&optional "*Make*"))
    (message "Done")

    (message a)
    ))

rebuild-fennel

(rebuild-fennel)
(message this-file-dir)

(defun print-shit ()
  (message "Shit"))

(print-shit)

(string-replace "\n" "--[\r][\n]" "a\nb")

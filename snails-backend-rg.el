(require 'snails-core)
(require 'exec-path-from-shell)

(exec-path-from-shell-initialize)

(snails-create-async-backend
 :name
 "rg"

 :build-command
 (lambda (input)
   (when (and (executable-find "rg")
              (> (length input) 5))
     (list "rg" "--no-heading" "--column" "--color" "never" input (snails-project-root-dir))
     ))

 :candidate-filter
 (lambda (candidate-list)
   (let (candidates)
     (dolist (candidate candidate-list)
       (add-to-list 'candidates
                    (list
                     (snails-wrap-file-icon candidate)
                     candidate)
                    t))
     candidates))

 :candiate-do
 (lambda (candidate)
   (let ((file-info (split-string candidate ":")))
     (when (> (length file-info) 3)
       (find-file (nth 0 file-info))
       (goto-line (string-to-number (nth 1 file-info)))
       (goto-column (max (- (string-to-number (nth 2 file-info)) 1) 0))
       ))))

(provide 'snails-backend-rg)

(require 'snails-core)
(when (featurep 'cocoa)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(snails-create-async-backend
 :name
 "FD"

 :build-command
 (lambda (input)
   (when (and (executable-find "fd")
              (> (length input) 5))
     (list "fd" "-c" "never" "-a" "-tf" input "--search-path" (snails-project-root-dir))
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
   (find-file candidate)
   ))

(provide 'snails-backend-fd)

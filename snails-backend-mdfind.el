(require 'snails-core)

(snails-create-async-backend
 :name
 "MDFIND"

 :build-command
 (lambda (input)
   (when (and (featurep 'cocoa)
              (> (length input) 5))
     (list "mdfind" "-name" (format "'%s'" input))))

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
   (find-file candidate)))

(provide 'snails-backend-mdfind)

(require 'snails-core)

(snails-create-async-backend
 "MDFIND"
 (lambda (input)
   (when (and (featurep 'cocoa)
              (> (length input) 5))
     (list "mdfind" (format "'%s'" input))))
 (lambda (result-list)
   (let (candidates)
     (dolist (result result-list)
       (add-to-list 'candidates
                    (list
                     (snails-wrap-file-icon result)
                     result)
                    t))
     candidates))
 (lambda (candidate)
   (find-file candidate)))

(provide 'snails-backend-mdfind)

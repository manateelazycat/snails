(require 'snails-core)

(snails-create-sync-backend
 :name
 "AWESOME TAB GROUP"

 :candidate-filter
 (lambda (input)
   (let (candidates)
     (when (featurep 'awesome-tab)
       (dolist (group (awesome-tab-get-groups))
         (when (or
                (string-equal input "")
                (string-match-p (regexp-quote input) group))
           (add-to-list 'candidates
                        (list
                         group
                         group)
                        t))))
     candidates))

 :candiate-do
 (lambda (candidate)
   (awesome-tab-switch-group candidate)))

(provide 'snails-backend-awesome-tab)

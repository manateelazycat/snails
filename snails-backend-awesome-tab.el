(require 'snails-core)

(snails-create-sync-backend
 ;; Backend name.
 "AWESOME TAB GROUP"
 ;; Search function.
 ;; accpet input string, and return candidate list
 ;; example format: ((display-name-1 candidate-1) (display-name-2 candidate-2))
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
 ;; Confirm function.
 ;; accpet candidate search, and do anything you want.
 (lambda (candidate)
   (awesome-tab-switch-group candidate)))

(provide 'snails-backend-awesome-tab)

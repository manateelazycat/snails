(require 'snails-core)
(require 'recentf)

(recentf-mode 1)

(snails-create-backend
 ;; Backend name.
 "RECENTF"
 ;; Search function.
 ;; accpet input string, and return candidate list
 ;; example format: ((display-name-1 candidate-1) (display-name-2 candidate-2))
 (lambda (input)
   (let (candidates)
     (dolist (file recentf-list)
       (when (or
              (string-equal input "")
              (string-match-p (regexp-quote input) file))
         (add-to-list 'candidates
                      (list
                       (snails-wrap-file-icon file)
                       file)
                      t)))
     candidates))
 ;; Confirm function.
 ;; accpet candidate search, and do anything you want.
 (lambda (candidate)
   (find-file candidate)))

(provide 'snails-backend-recentf)

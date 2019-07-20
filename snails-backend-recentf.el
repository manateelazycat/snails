(require 'snails-core)
(require 'recentf)

(recentf-mode 1)

(snails-create-backend
 "RECENTF"
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
 (lambda (candidate)
   (find-file candidate)))

(provide 'snails-backend-recentf)

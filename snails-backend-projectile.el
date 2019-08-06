;;; snails-backend-projectile.el --- Projectile backend for snails

;;; Commentary:
;;
;; Projectile backend for snails

;;; Requires
(require 'snails-core)
(require 'projectile)

;;; Code:

(snails-create-sync-backend
 :name
 "PROJECTILE"

 :candidate-filter
 (lambda (input)
   (let (candidates)
     (dolist (file (projectile-current-project-files))
       (when (or
              (string-equal input "")
              (snails-match-input-p input file))
         (snails-add-candiate 'candidates (snails-wrap-file-icon file) file)))
     (snails-sort-candidates input candidates 1 1)
     candidates))

 :candiate-do
 (lambda (candidate)
   (find-file candidate)))


(provide 'snails-backend-projectile)

;;; snails-backend-projectile.el ends here

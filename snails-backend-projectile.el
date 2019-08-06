;;; snails-backend-projectile.el --- Projectile backend for snails

;;; Commentary:
;;
;; Projectile backend for snails

;;; Requires
(require 'snails-core)
(require 'projectile)

;;; Code:


(defun snails-backend-projectile--project-root ()
  "Find projectile root."
  (let ((filename (buffer-file-name snails-start-buffer)))
    (when filename
      (projectile-project-root (file-name-directory filename)))))


(defun snails-backend-projectile--files ()
  "List project files."
  (let ((project-root (snails-backend-projectile--project-root)))
    (when project-root
      (projectile-project-files project-root))))


(snails-create-sync-backend
 :name
 "PROJECTILE"

 :candidate-filter
 (lambda (input)
   (let ((candidates)
         (project-files (snails-backend-projectile--files)))
     (when project-files
       (dolist (file project-files)
         (when (or
                (string-equal input "")
                (snails-match-input-p input file))
           (snails-add-candiate 'candidates (snails-wrap-file-icon file) file))))
     (snails-sort-candidates input candidates 1 1)
     candidates))

 :candiate-do
 (lambda (candidate)
  (let ((project-root (snails-backend-projectile--project-root)))
   (find-file (expand-file-name candidate project-root)))))


(provide 'snails-backend-projectile)

;;; snails-backend-projectile.el ends here

(require 'snails-core)
(require 'exec-path-from-shell)

(exec-path-from-shell-initialize)

(snails-create-async-backend
 :name
 "RG"

 :build-command
 (lambda (input)
   (when (and (executable-find "rg")
              (> (length input) 5))
     (let ((project (project-current)))
       (when project
         (list "rg" "--no-heading" "--column" "--color" "never" input (expand-file-name (cdr project)))
         ))))

 :candidate-filter
 (lambda (candidate-list)
   (let (candidates)
     (dolist (candidate candidate-list)
       (let ((file-info (split-string candidate ":"))
             (project-dir (expand-file-name (cdr (project-current)))))
         (add-to-list 'candidates
                      (list
                       (snails-wrap-file-icon-with-candidate
                        (nth 0 file-info)
                        (format "PDIR/%s" (nth 1 (split-string candidate project-dir))))
                       candidate)
                      t)))
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

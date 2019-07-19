(require 'snails-core)
(require 'recentf)

(recentf-mode 1)

(defvar snails-backend-recentf-name "RECENTLY FILES")

(defun snails-backend-recentf-search (input input-ticker update-callback)
  (let (candidates)
    (dolist (file recentf-list)
      (when (or
             (string-equal input "")
             (string-match-p (regexp-quote input) file))
        (add-to-list 'candidates
                     (list
                      (snails-wrap-file-icon file)
                      file) t)))
    (funcall
     update-callback
     snails-backend-recentf-name
     input-ticker
     candidates)))

(defun snails-backend-recentf-do (candidate)
  (find-file candidate))

(defvar snails-backend-recentf
  '(("name" . snails-backend-recentf-name)
    ("search" . snails-backend-recentf-search)
    ("do" . snails-backend-recentf-do)
    ))

(provide 'snails-backend-recentf)

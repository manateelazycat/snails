(defvar snails-backend-name-buffer-list "BUFFERS")

(defun snails-backend-search-buffer-list (input input-ticker update-callback)
  (let (candidates)
    (dolist (buf (buffer-list))
      (when (or
             (string-equal input "")
             (string-match-p (regexp-quote input) (buffer-name buf)))
        (add-to-list 'candidates
                     (list
                      (if (featurep 'all-the-icons)
                          (format "%s %s"
                                  (with-current-buffer buf
                                    (all-the-icons-icon-for-buffer))
                                  (string-trim-left (buffer-name buf)))
                        (buffer-name buf))
                      (buffer-name buf)) t)))
    (funcall
     update-callback
     snails-backend-name-buffer-list
     input-ticker
     candidates)))

(defun snails-backend-do-buffer-list (candidate)
  (switch-to-buffer candidate))

(defvar snails-backend-buffer-list
  '(("name" . snails-backend-name-buffer-list)
    ("search" . snails-backend-search-buffer-list)
    ("do" . snails-backend-do-buffer-list)
    ))

(provide 'snails-backend-buffer-list)

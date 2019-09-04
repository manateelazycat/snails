
(require 'subr-x)
(defvar search-info nil)

(snails-create-async-backend
 :name
 "FD"

 :build-command
 (lambda (input)
   (let ((input-info (split-string input "@")))
     (setq search-info input-info)
     (if (> (length search-info) 1)
         (progn ;; combine search
           (when (and (executable-find "fd")
                      (executable-find "rg")
                      (> (length (nth 0 input-info)) 5)
                      (> (length (nth 1 input-info)) 5))
             (list "fd" "-c" "never" "-a" "-td"  (nth 1 input-info) "--search-path" (expand-file-name (getenv "HOME")))
             )
           )
       (progn ;; fd search only
         (when (and (executable-find "fd")
                    (> (length input) 5))
           (list "fd" "-c" "never" "-a" "-tf" input "--search-path" (or snails-project-root-dir (expand-file-name default-directory)))
           )
         )
       )
     )
   )


 :candidate-filter
 (lambda (candidate-list)
   (if (< (length  search-info) 2)
       (progn ;; fd search only
              (let (candidates)
                (dolist (candidate candidate-list)
                  (snails-add-candiate
                   'candidates
                   (snails-wrap-file-icon-with-candidate candidate candidate)
                   candidate))
                candidates)
              )
     (progn ;; combine search
            (let (candidates)
              (dolist (candidate candidate-list)
                (let ((rg-search-str "rg --no-heading --color never --column --max-columns 300 " ))
                  (setq rg-search-str(concat rg-search-str  (nth 0 search-info) " " ))
                  (setq rg-search-str(concat rg-search-str  candidate))
                  (let ((file-list (butlast (split-string (shell-command-to-string rg-search-str) "\n"))))
                    (when file-list
                      (snails-add-candiate
                       'candidates
                       (snails-wrap-file-icon-with-candidate
                        candidate
                        (string-remove-prefix (or (getenv "HOME") "") candidate))
                       candidate)

                      (dolist (f file-list)
                        (snails-add-candiate
                         'candidates
                         (snails-wrap-file-icon-with-candidate
                          (nth 0 (split-string f ":"))
                          (string-remove-prefix (or (getenv "HOME") "") f))
                         f)
                        )
                      )
                    )
                  )
                )
              candidates)
            )
     )


   )


 :candiate-do
 (lambda (candidate)
   (if (file-directory-p candidate )
       (progn ;; folder
         (find-file candidate)
         )

     (progn ;; file
       (let ((file-info (split-string candidate ":")))
         (when (> (length file-info) 3)
           ;; Open file and jump to position.
           (find-file (nth 0 file-info))
           (goto-line (string-to-number (nth 1 file-info)))
           (move-to-column (max (- (string-to-number (nth 2 file-info)) 1) 0))

           ;; Flash match line.
           (snails-flash-line)
           ))))
   ))

(provide 'snails-backend-fd)

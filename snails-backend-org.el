;;; snails-backend-org.el --- Ripgrep backend for snails crawling on org files

;;; Note
;; org-directory should be like /home/xx/xxxx, do not use ~/xx/xxx
;; you have to type at least 5 characters to start the search


;;; TODO
;; improve: only unflod the block containning the search content when org file is folded.
;;
;;


;;; Require
(require 'snails-core)

;;; Code:

(snails-create-async-backend
 :name
 "ORG"

 :build-command
 (lambda (input)
   (when (and (executable-find "rg")
              (> (length input) 5))
     (when org-directory
       (list "rg" "--no-heading" "--column" "--color" "never" "--max-columns" "300" input org-directory)
       )))

 :candidate-filter
 (lambda (candidate-list)
   (let (candidates)
     (dolist (candidate candidate-list)
       (snails-add-candiate
        'candidates
        (snails-wrap-file-icon-with-candidate
         (nth 0 (split-string candidate ":"))
         (string-remove-prefix (or org-directory "") candidate))
        candidate))
     candidates))

 :candiate-do
 (lambda (candidate)
   (let ((file-info (split-string candidate ":")))
     (when (> (length file-info) 3)
       ;; Open file and jump to position.
       (find-file (nth 0 file-info))
       (goto-line (string-to-number (nth 1 file-info)))
	   (outline-show-all)
       (recenter-top-bottom)
       (move-to-column (max (- (string-to-number (nth 2 file-info)) 1) 0))

       ;; Flash match line.
       (snails-flash-line)
       ))))

(provide 'snails-backend-org)

;;; snails-backend-org.el ends here

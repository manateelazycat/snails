(require 'snails-core)

(defvar snails-backend-buffer-blacklist
  (list
   snails-input-buffer
   snails-content-buffer
   " *code-conversion-work*"
   " *Echo Area "
   " *Minibuf-"
   " *Custom-Work*"
   ))

(defun snails-backend-buffer-not-blacklist-buffer (buf)
  (catch 'failed
    (dolist (backlist-buf snails-backend-buffer-blacklist)
      (when (string-prefix-p backlist-buf (buffer-name buf))
        (throw 'failed nil)))
    t))

(snails-create-backend
 ;; Backend name.
 "BUFFER"
 ;; Search function.
 ;; accpet input string, and return candidate list
 ;; example format: ((display-name-1 candidate-1) (display-name-2 candidate-2))
 (lambda (input)
   (let (candidates)
     (dolist (buf (buffer-list))
       (when (and
              (snails-backend-buffer-not-blacklist-buffer buf)
              (or
               (string-equal input "")
               (string-match-p (regexp-quote input) (buffer-name buf))))
         (add-to-list 'candidates
                      (list
                       (snails-wrap-buffer-icon buf)
                       (buffer-name buf)) t)))
     candidates))
 ;; Confirm function.
 ;; accpet candidate search, and do anything you want.
 (lambda (candidate)
   (switch-to-buffer candidate)))

(provide 'snails-backend-buffer)

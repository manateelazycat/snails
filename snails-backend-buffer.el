(require 'snails-core)

(defvar snails-backend-buffer-name "BUFFERS")

(defvar snails-backend-buffer-blacklist
  (list
   snails-input-buffer
   snails-content-buffer
   " *code-conversion-work*"
   " *Echo Area "
   " *Minibuf-"
   " *Custom-Work*"
   ))

(defun snails-backend-buffer-search (input input-ticker update-callback)
  (let (candidates)
    (dolist (buf (buffer-list))
      (message "'%s'" (buffer-name buf))
      (when (and
             (snails-backend-buffer-not-blacklist-buffer buf)
             (or
              (string-equal input "")
              (string-match-p (regexp-quote input) (buffer-name buf))))
        (add-to-list 'candidates
                     (list
                      (snails-wrap-buffer-icon buf)
                      (buffer-name buf)) t)))
    (funcall
     update-callback
     snails-backend-buffer-name
     input-ticker
     candidates)))

(defun snails-backend-buffer-not-blacklist-buffer (buf)
  (catch 'failed
    (dolist (backlist-buf snails-backend-buffer-blacklist)
      (when (string-prefix-p backlist-buf (buffer-name buf))
        (throw 'failed nil)))
    t))

(defun snails-backend-buffer-do (candidate)
  (switch-to-buffer candidate))

(defvar snails-backend-buffer
  '(("name" . snails-backend-buffer-name)
    ("search" . snails-backend-buffer-search)
    ("do" . snails-backend-buffer-do)
    ))

(provide 'snails-backend-buffer)

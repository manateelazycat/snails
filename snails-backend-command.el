(require 'snails-core)

(snails-create-backend
 ;; Backend name.
 "COMMAND"
 ;; Search function.
 ;; accpet input string, and return candidate list
 ;; example format: ((display-name-1 candidate-1) (display-name-2 candidate-2))
 (lambda (input)
   (let (candidates)
     (let ((commands (let ((cmds  ()))
                       (mapatoms (lambda (s) (when (commandp s) (push (format "%s" s) cmds))))
                       cmds)))
       (dolist (command commands)
         (when (or
                (string-equal input "")
                (string-match-p (regexp-quote input) command))
           (add-to-list 'candidates
                        (list
                         command
                         command)
                        t))))
     candidates))
 ;; Confirm function.
 ;; accpet candidate search, and do anything you want.
 (lambda (candidate)
   (call-interactively (intern candidate))))

(provide 'snails-backend-command)

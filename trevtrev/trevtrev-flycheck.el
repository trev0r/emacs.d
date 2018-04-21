;;; trevtrev-flycheck.el --- Flycheck setup

(package-require 'flycheck)
(add-hook 'find-file-hook
          (lambda ()
            (when (not (equal 'emacs-lisp-mode major-mode))
              (flycheck-mode))))

(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

(global-set-key
 (kbd "C-c c")
 (lambda () (interactive)
   (if flycheck-checker
       (progn
         (save-buffer)
         (flycheck-compile flycheck-checker)))
   (message
    "No checker selected for this buffer. Try M-x flycheck-select-checker")))

(provide 'trevtrev-flycheck)

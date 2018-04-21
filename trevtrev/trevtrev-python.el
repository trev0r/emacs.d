;;; trevtrev-python.el -- Python configuration

;; Require, not autoload, to override Emacs bundled python.el
(package-require 'python-mode)

;; Pytest bindings
(package-require 'pytest)
(add-hook
 'python-mode-hook
 (lambda ()
   (define-key python-mode-map (kbd "C-c C-,") 'pytest-run-file)))
;; Spacing
(add-hook 'python-mode-hook 'guess-style-guess-tabs-mode)
(add-hook 'python-mode-hook (lambda ()
                              (guess-style-guess-tab-width)))

(provide 'trevtrev-python)

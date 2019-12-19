;;; codestyle.el -- Indentation styles et al for all modes in one central location

;; Tab indentation is a disease; a cancer of this planet.
(set-default 'indent-tabs-mode nil)

;; Always newline-and-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Default indentation
(setq-default tab-width 4)
;; Javascript
(setq-default js2-basic-offset 4)
;; JSON
(setq-default js-indent-level 4)

;; Python
(setq-default py-indent-offset 4)
;; XML
(setq-default nxml-child-indent 2)

;; Default formatting style for C based modes
(setq c-default-style "java")
(setq-default c-basic-offset 2)

;; I mean, _seriously_...
(setq sentence-end-double-space nil)

;; Enforce proper whitespace
(package-require 'ethan-wspace)
(setq mode-require-final-newline nil)
(setq require-final-newline nil)
(global-ethan-wspace-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'trevtrev-codestyle)

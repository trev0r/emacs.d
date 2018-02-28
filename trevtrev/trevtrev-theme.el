;;; theme.el --- Appearance matters

;; Let's see what we're running on
(setq on-console (null window-system))

;; No splash screen
(setq inhibit-startup-message t)

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; Some X11 setup
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; Show line numbers in buffers
(global-linum-mode t)
(setq linum-format (if on-console "%4d " "%4d"))

;; Show column numbers in modeline
(setq column-number-mode t)

;; Show current function in modeline
(which-function-mode)

;; Redefine linum-on to ignore terminal buffers, because just turning
;; it off in term-mode-hook doesn't work.
(setq linum-disabled-modes
      '(term-mode slime-repl-mode magit-status-mode help-mode nrepl-mode
                  xwidget-webkit-mode revealjs-mode
                  mu4e-main-mode mu4e-headers-mode mu4e-view-mode
                  mu4e-compose-mode))
(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes))
    (linum-mode 1)))

;; Highlight current line
;(global-hl-line-mode)

;; git-gutter-fringe
(package-require 'git-gutter-fringe)
(require 'git-gutter-fringe)
(global-git-gutter-mode t)


;; Set custom theme path
(setq custom-theme-directory (concat dotfiles-dir "themes"))
(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

;; Install themes
(package-require 'badger-theme)
(package-require 'zenburn-theme)
(package-require 'leuven-theme)

(defun theme-light ()
  (interactive)
  (load-theme 'leuven)
  (set-face-attribute 'mode-line nil
              :foreground "#cccccc"
              :background "#000000"
              :box nil
              :weight 'bold)
  (set-face-attribute 'mode-line-buffer-id nil
              :foreground "white"
              :weight 'bold)
  (set-face-attribute 'mode-line-inactive nil
              :foreground "#cccccc"
              :background "#666666"
              :box nil
              :weight 'bold))


  ;; (set-face-background 'default "#ffffff")
  ;; (set-face-foreground 'default "#000000")
  ;; (set-face-background 'region "#d4d4d4")
  ;; (set-face-foreground 'region nil)
  ;; (set-face-background 'linum "#f0f0f0")
  ;; (set-face-background 'fringe "#f0f0f0")


(defun theme-dark ()
  (interactive)

  (load-theme 'badger t)

  (set-face-background 'default "#222")
  (set-face-background 'region "#374186")
  (set-face-background 'fringe "#191919")
  (set-face-attribute 'linum nil :background nil)
  (set-face-foreground 'which-func "#cccccc"))

(theme-light)

(provide 'trevtrev-theme)

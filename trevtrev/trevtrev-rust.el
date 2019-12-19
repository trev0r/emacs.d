;;; trevtrev-rust.el -- Rust configuration

(package-require 'rust-mode)
(package-require 'cargo)
(package-require 'flymake-rust)

(add-hook 'rust-mode-hook 'cargo-minor-mode)

(provide 'trevtrev-rust)

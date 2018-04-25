; rust-mode lsp-mode lsp-rust cargo flycheck company-lsp lsp-ui company

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(setq compilation-ask-about-save nil)

(require 'company-lsp)
(push 'company-lsp company-backends)

(add-hook 'after-init-hook 'global-company-mode)
(setq rust-format-on-save t)

(setq lsp-ui-sideline-code-actions-prefix "")

(with-eval-after-load 'lsp-mode
  (setq lsp-rust-rls-command '("rls"))
  (require 'lsp-flycheck)
  (require 'lsp-rust))

(require 'lsp-mode)

(add-hook 'rust-mode-hook #'lsp-rust-enable)
(add-hook 'rust-mode-hook #'flycheck-mode)
(add-hook 'rust-mode-hook 'cargo-minor-mode)

; TODO: Update to cargo.el master for these to work
(setq cargo-process--custom-path-to-bin "nix-shell")
(setq cargo-process--command-bench "--run 'cargo bench'")
(setq cargo-process--command-build "--run 'cargo build'")
(setq cargo-process--command-clean "--run 'cargo clean'")
(setq cargo-process--command-doc "--run 'cargo doc'")
(setq cargo-process--command-doc-open "--run 'cargo doc --open'")
(setq cargo-process--command-new "--run 'cargo new'")
(setq cargo-process--command-init "--run 'cargo init'")
(setq cargo-process--command-run "--run 'cargo run'")
(setq cargo-process--command-run-bin "--run 'cargo run --bin'")
(setq cargo-process--command-run-example "--run 'cargo run --example'")
(setq cargo-process--command-search "--run 'cargo search'")
(setq cargo-process--command-test "--run 'cargo test'")
(setq cargo-process--command-current-test "--run 'cargo test'")
(setq cargo-process--command-current-file-tests "--run 'cargo test'")
(setq cargo-process--command-update "--run 'cargo update'")
(setq cargo-process--command-fmt "--run 'cargo fmt'")
(setq cargo-process--command-check "--run 'cargo check'")
(setq cargo-process--command-clippy "--run 'cargo clippy'")

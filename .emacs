;; Adding packages repositories
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)


(package-initialize)


;; Installing required packages

; list the packages
(setq package-list
      '(flymake-ruby
	rvm
	flx-ido
	projectile
	projectile-rails
	robe
	nyan-mode
	monokai-theme
	;;color-theme-monokai
	nlinum
	enh-ruby-mode
	smartparens
	rainbow-mode
	company
	magit
	cider
	paredit
	paredit-everywhere
	markdown-mode
	yaml-mode
	rainbow-blocks
	rainbow-delimiters
	hl-line
	rust-mode
	flycheck-rust
	racer
	cargo))

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; cua mdoe only for rectangles
(setq cua-enable-cua-keys nil) ;; only for rectangles
(cua-mode t)

;; disable menu
(menu-bar-mode -1)

;; Ruby things

(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

(setq ruby-deep-indent-paren nil)

(global-set-key (kbd "C-c r r") 'inf-ruby)

(require 'rvm)

(rvm-use-default)

; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

(require 'ido)
(ido-mode t)
(flx-ido-mode t)

(require 'projectile)
(require 'projectile-rails)
(projectile-global-mode)


(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

(require 'enh-ruby-mode)

(add-hook 'projectile-mode-hook 'projectile-rails-on)

; must be added after any path containing old ruby-mode
(add-to-list 'load-path "(path-to)/Enhanced-Ruby-Mode")
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(add-hook 'enh-ruby-mode-hook 'robe-mode)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(add-hook 'enh-ruby-mode 'paredit-everywhere-mode)

;; Other things

(tool-bar-mode -1)
(scroll-bar-mode -1)
(nyan-mode t)
;;(color-theme-monokai)
(load-theme 'monokai)
(require 'nlinum)
(global-nlinum-mode t)
(smartparens-global-mode t)

(global-company-mode t)
(push 'company-robe company-backends)

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-x p") 'switch-to-previous-buffer)

;; Leiningen / clojure
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-lein-command "/usr/local/bin/lein")
 '(custom-safe-themes
   (quote
    ("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" default)))
 '(package-selected-packages
   (quote
    (cargo rust-mode yaml-mode smartparens rvm robe rainbow-mode rainbow-delimiters rainbow-blocks projectile-rails paredit-everywhere nyan-mode nlinum markdown-mode magit flymake-ruby flx-ido enh-ruby-mode company color-theme-monokai cider))))

(add-hook 'clojure-mode-hook  #'enable-paredit-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(show-paren-mode t)
(global-hl-line-mode t)

(add-hook 'before-save-hook 'whitespace-cleanup)
(require 'whitespace)
(setq whitespace-line-column 2000)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; rust
(add-hook 'rust-mode-hook 'cargo-minor-mode)

(add-hook 'rust-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

 (setenv "PATH" (concat (getenv "PATH") ":/.cargo/bin"))
(setq exec-path (append exec-path '("~/.cargo/bin"))

(setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
(setq racer-rust-src-path "/home/agarrote/Development/rust-lang/rust/src") ;; Rust source code PATH

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

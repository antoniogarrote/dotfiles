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
	color-theme-monokai
	nlinum
	enh-ruby-mode
	smartparens
	rainbow-mode
	company
	magit))

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


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

(add-hook 'projectile-mode-hook 'projectile-rails-on)


(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
;(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
;  (rvm-activate-corresponding-ruby))

(require 'enh-ruby-mode)

; must be added after any path containing old ruby-mode
(add-to-list 'load-path "(path-to)/Enhanced-Ruby-Mode") 
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(add-hook 'enh-ruby-mode-hook 'robe-mode)


;; Other things

(tool-bar-mode -1)
(scroll-bar-mode -1)
(nyan-mode t)
(color-theme-monokai)
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

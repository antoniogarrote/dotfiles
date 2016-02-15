; list the packages
(setq package-list
      '(flymake-ruby
        rvm
        projectile-rails
	robe
	nyan-mode
        yalinum
        flx-ido
	monokai-theme
        markdown-mode
        rspec-mode
        rubocop
        ruby-additional
        ruby-dev
        neotree
        fiplr
        symon
        ruby-tools
        pdf-tools))

(global-set-key (kbd "C-x <C-backspace>") 'erase-buffer)

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


(require 'projectile-rails)

(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)

(require 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)

(require 'rspec-mode)

(autoload 'turn-on-ruby-dev "ruby-dev" nil t)
(add-hook 'ruby-mode-hook 'turn-on-ruby-dev)

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

;; Other things

; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)


(flx-ido-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(nyan-mode t)
(load-theme 'monokai t)
(require 'yalinum)
(global-yalinum-mode t)
;(smartparens-global-mode t)


(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-x p") 'switch-to-previous-buffer)

(global-hl-line-mode t)

(add-hook 'before-save-hook 'whitespace-cleanup)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(cua-selection-mode t)

; max-width line
(setq whitespace-line-column 120)

(require 'ruby-tools)
(add-hook 'ruby-mode 'ruby-tools-mode)

(require 'flx-ido)
(require 'ido)

(add-hook 'projectile-mode-hook 'projectile-rails-on)


(beacon-mode 1)
(setq beacon-push-mark 35)
(setq beacon-color "#666600")


(require 'neotree)
(global-set-key (kbd "C-x t") 'neotree-toggle)

(require 'symon)
(symon-mode)


;; increment number
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

;; zone
(require 'zone)
(setq zone-timer (run-with-idle-timer 60 t 'zone))

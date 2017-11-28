
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(load-file "~/.emacs.d/scripts/own-defaults.el")
(own-defaults/use-all-settings)
(own-defaults/use-all-keybindings)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-verbose t)
(setq use-package-always-ensure t)

(require 'use-package)

(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(tool-bar-mode 0)
(menu-bar-mode 0)
(when window-system
  (scroll-bar-mode -1))

(global-prettify-symbols-mode t)

(setq-default cursor-type 'bar)
(set-cursor-color "#ffffff")

(use-package monokai-theme)

(defun tk/apply-monokai-theme ()
  (load-theme 'monokai t))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (tk/apply-monokai-theme)))
  (tk/apply-monokai-theme))

(setq tk/default-font "Hack")
  (setq tk/default-font-size 12)
  (setq tk/current-font-size tk/default-font-size)

  (setq tk/font-change-increment 1.1)

  (defun tk/font-code ()
    "Return a string representing the current font (like 'Hack-14')"
    (concat tk/default-font "-" (number-to-string tk/current-font-size)))

  (defun tk/set-font-size ()
    "Set the font to `tk/default-font' at `tk/current-font-size'.
  Set that for the current frame, and also make it the default for
  other, future frames."
    (let ((font-code (tk/font-code)))
      (add-to-list 'default-frame-alist (cons 'font font-code))
      (set-frame-font font-code)))

  (defun tk/reset-font-size ()
    "Change font size back to `tk/default-font-size'."
    (interactive)
    (setq tk/current-font-size tk/default-font-size)
    (tk/set-font-size))

  (defun tk/increase-font-size ()
    "Increase current font size by a factor of `tk/font-change-increment'."
    (interactive)
    (setq tk/current-font-size
          (ceiling (* tk/current-font-size tk/font-change-increment)))
    (tk/set-font-size))

  (defun tk/decrease-font-size ()
    "Decrease current font size by a factor of `tk/font-change-increment', down to a minimum size of 1."
    (interactive)
    (setq tk/current-font-size
          (max 1
               (floor (/ tk/current-font-size tk/font-change-increment))))
    (tk/set-font-size))

  (define-key global-map (kbd "C-)") 'hrs/reset-font-size)
  (define-key global-map (kbd "C-+") 'hrs/increase-font-size)
  (define-key global-map (kbd "C-=") 'hrs/increase-font-size)
  (define-key global-map (kbd "C-_") 'hrs/decrease-font-size)
  (define-key global-map (kbd "C--") 'hrs/decrease-font-size)

  (tk/reset-font-size)

;; Preset `nlinum-format' for minimum width.
(defun my-nlinum-mode-hook ()
  (when nlinum-mode
    (setq-local nlinum-format
                (concat "%" (number-to-string
                             ;; Guesstimate number of buffer lines.
                             (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
                        "d"))))
(add-hook 'nlinum-mode-hook #'my-nlinum-mode-hook)

(setq-default tab-width 4)

(setq compilation-scroll-output t)

(setq python-indent 4)

(defun tk/search-project-for-symbol-at-point ()
  "Use `projectile-ag' to search the current project for `symbol-at-point'."
  (interactive)
  (projectile-ag (projectile-symbol-at-point)))

(global-set-key (kbd "C-c v") 'projectile-ag)
(global-set-key (kbd "C-c C-v") 'tk/search-project-for-symbol-at-point)

(setq projectile-switch-project-action 'projectile-dired)

(setq projectile-require-projectile-root nil)

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package which-key
             :config
             (which-key-mode))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda() (org-bullets-mode 1))))

(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-push-always-verify nil)
(setq git-commit-summary-max-length 50)
(add-hook 'git-commit-mode-hook 'turn-on-flyspell)

(require 'org)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "~/OneDrive/Tejas/Documents/org/chores.org"
                             "~/OneDrive/Tejas/Documents/org/cumulus.org"
                             "~/OneDrive/Tejas/Documents/org/errands.org"
                             "~/OneDrive/Tejas/Documents/org/habits.org"
                             "~/OneDrive/Tejas/Documents/org/learning.org"
                             "~/OneDrive/Tejas/Documents/org/prm.org"
                             "~/OneDrive/Tejas/Documents/org/read_write.org"))

(setq org-todo-keywords
      (quote ((sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d@/!)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-use-fast-todo-selection t)

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

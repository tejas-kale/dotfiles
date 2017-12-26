(package-initialize)

(org-babel-load-file "~/.emacs.d/configuration.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(epg-gpg-program "/usr/local/bin/gpg")
 '(package-selected-packages
   (quote
	(org-pomodoro anaconda-mode docker-tramp helm-projectile pdf-tools auto-complete-config epa-file undo-tree smart-mode-line quelpa-use-package counsel-dash exwm helm counsel swiper htmlize ox-reveal ox-twbs realgud jedi epc which-key use-package projectile org-bullets monokai-theme matlab-mode markdown-mode magit hydra flycheck ess auto-complete auto-compile async-await))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

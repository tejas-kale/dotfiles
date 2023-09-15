(setq debug-on-error nil)

(setq user-full-name "Tejas Kale"
      user-mail-address "kaletejas2006@gmail.com")

(setq doom-theme 'doom-moonlight)

(setq display-line-numbers-type t)

(setq org-directory "~/Documents/org")

(setenv "PATH" (concat "/Library/TeX/texbin/" ":" (getenv "PATH")))

(setq
 doom-font (font-spec :family "Fira Code" :size 16)
 doom-big-font (font-spec :family "Fira Code" :size 28)
 doom-variable-pitch-font (font-spec :family "Avenir Next" :size 14)
)

(setq auth-sources '("~/.authinfo.gpg"))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\venv\\'"))

(setq jiralib-url "https://relayr.atlassian.net")

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "%?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%b %d, %y (%a)>\n"))
        ("t" "with-timestamp" entry "* %<%I:%M %p>: %?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%b %d, %y (%a)>\n"))))

(map! :after evil-easymotion
      :map evilem-map
      "G" #'avy-goto-line)

(setq display-time-format "%a %d %b %H:%M")
(display-time)

(after! org
  (use-package! ox-extra
    :config
    (ox-extras-activate '(latex-header-blocks ignore-headlines))
    (setq org-latex-pdf-process
          '("pdflatex -interaction nonstopmode -output-directory %o %f"
            "bibtex %b"
            "pdflatex -interaction nonstopmode -output-directory %o %f"
            "pdflatex -interaction nonstopmode -output-directory %o %f"))
    (setq org-latex-with-hyperref nil)

    (setq org-latex-logfiles-extensions
          (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf" "acn" "acr" "alg" "glg" "gls" "ist")))

    (unless (boundp 'org-latex-classes)
      (setq org-latex-classes nil))))

(map! :leader
      :desc "extras"
      "e")

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(use-package! mu4e
 :config
 (setq mu4e-change-filenames-when-moving t)

 (setq mu4e-update-interval (* 10 60))
 (setq mu4e-get-mail-command (concat "mbsync -c " (expand-file-name "~/.mbsync/.mbsyncrc") " -a"))
 (setq mu4e-maildir "~/Maildir")

 (require 'smtpmail)
 (setq message-send-mail-function 'smtpmail-send-it
       starttls-use-gnutls t
       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
       smtpmail-auth-credentials
       '(("smtp.gmail.com" 587 "kaletejas2006@gmail.com" nil))
       smtpmail-default-smtp-server "smtp.gmail.com"
       smtpmail-smtp-server "smtp.gmail.com"
       smtpmail-smtp-service 587))

(map! :after evil-easymotion
      :map evilem-map
      "G" #'avy-goto-line)

(dirvish-override-dired-mode)

(map! :leader
      :map smerge-mode-map
      (:prefix ("e m" . "smerge")
               "m" #'smerge-keep-mine
               "n" #'smerge-next
               "o" #'smerge-keep-other
               "p" #'smerge-prev))

(setq jiralib-url "https://relayr.atlassian.net")
(defconst org-jira-progress-issue-flow
  '(("To Do" . "Planned")
    ("To Do" . "Feedback")
    ("To Do" . "Will Not Do")
    ("To Do" . "Blocked")
    ("Planned" . "To Do")
    ("Planned" . "Feedback")
    ("Planned" . "Will Not Do")
    ("Planned" . "Blocked")
    ("Planned" . "In Progress")
    ("In Progress" . "Feedback")
    ("In Progress" . "Will Not Do")
    ("In Progress" . "Blocked")
    ("In Progress" . "In Review")
    ("In Review" . "Feedback")
    ("In Review" . "Will Not Do")
    ("In Review" . "Blocked")
    ("In Review" . "Done")))

(map! :leader
      (:prefix ("e j" . "jira")
               "b" #'org-jira-browse-issue
               "c" #'org-jira-create-issue
               "g" #'org-jira-get-issues
               "r" #'org-jira-refresh-issues-in-buffer
               "s" #'org-jira-create-subtask))

(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

(setq
 projectile-project-search-path (list
                                 (expand-file-name "~/Code")
                                 (expand-file-name "~/Documents")
                                 (expand-file-name "~/Library/Mobile Documents/com~apple~CloudDocs/Documents")
                                 (expand-file-name "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents")))

(map! :after python
      :map python-mode-map
      :localleader
      (:prefix ("f" . "flycheck")
       "l" #'flycheck-list-errors))

(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(setq python-indent-def-block-scale 1)

(setq lsp-signature-auto-activate nil)

(map! :after python
      :map python-mode-map
      :localleader
      (:prefix ("v" . "venv")
       "a" #'pyvenv-activate))

(setq lsp-pylsp-plugins-flake8-enabled nil)
(setq lsp-pylsp-plugins-autopep8-enabled nil)
(setq lsp-pylsp-plugins-pyflakes-enabled nil)
(setq lsp-pylsp-plugins-pylint-enabled t)

(add-hook! 'python-mode-hook (setq flycheck-checker 'python-pylint))

(map! :leader
      (:prefix ("e i" . "ipython")
       "l" #'ein:notebooklist-open
       "r" #'ein:run))

(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(after! org
  (setq! org-tags-column -77))

(after! org
  (require 'vulpea)
  (add-hook 'org-roam-db-autosync-mode #'vulpea-db-autosync-enable)
  (add-to-list 'org-tags-exclude-from-inheritance "project"))

(defun vulpea-project-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (org-element-map                          ; (2)
       (org-element-parse-buffer 'headline) ; (1)
       'headline
     (lambda (h)
       (eq (org-element-property :todo-type h)
           'todo))
     nil 'first-match))

(add-hook 'find-file-hook #'vulpea-project-update-tag)
(add-hook 'before-save-hook #'vulpea-project-update-tag)

(defun vulpea-project-update-tag ()
      "Update PROJECT tag in the current buffer."
      (when (and (not (active-minibuffer-window))
                 (vulpea-buffer-p))
        (save-excursion
          (goto-char (point-min))
          (let* ((tags (ignore-errors
                         (vulpea-buffer-tags-get)))
                 (original-tags tags))
            (if (vulpea-project-p)
                (setq tags (cons "project" tags))
              (setq tags (remove "project" tags)))

            ;; cleanup duplicates
            (setq tags (seq-uniq tags))

            ;; update tags if changed
            (when (or (seq-difference tags original-tags)
                      (seq-difference original-tags tags))
              (ignore-errors
                (apply #'vulpea-buffer-tags-set tags)))))))

(defun vulpea-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun vulpea-project-files ()
  "Return a list of note files containing 'project' tag." ;
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (like tag (quote "%\"project\"%"))]))))

(setq org-agenda-files-not-in-roam (list
                                    (expand-file-name "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/ticklers.org")
                                    (expand-file-name "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/regulars.org")))

(setq org-agenda-files org-agenda-files-not-in-roam)

(defun vulpea-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (append (vulpea-project-files) org-agenda-files-not-in-roam)))

  ;(push org-agenda-files-not-in-roam 'org-agenda-files))

(advice-add 'org-agenda :before #'vulpea-agenda-files-update)
(advice-add 'org-todo-list :before #'vulpea-agenda-files-update)

(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

(setq org-log-into-drawer t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w!)" "HOLD(h)" "IDEA(i)" "|" "DONE(d!)" "KILL(k!)")
        (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
        (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

(map! :leader
      (:prefix ("e f" . "find")
       "h" #'howdoyou-query))

(use-package! smudge
 :config
 (setq! smudge-oauth2-client-id "01e3654bcee5437abcb921483d37cc4a")
 (setq! smudge-oauth2-client-secret "979dc0ddbe544a709e9ea79f51949d33"); (shell-command-to-string "pass show spotify.com/emacsapp"))
 (setq! smudge-transport 'connect)
 (setq! smudge-player-status-refresh-interval 10))
 ;(global-smudge-remote-mode))

(map! :leader
     (:prefix ("e s" . "spotify")
      "f" #'smudge-playlist-search
      "g" #'global-smudge-remote-mode
      "p" #'smudge-controller-toggle-play
      "s" #'smudge-track-search))

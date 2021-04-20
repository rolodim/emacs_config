;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)              ; Disable visible scrollbar
(tool-bar-mode -1)                ; Disable the toolbar
(tooltip-mode -1)                 ; Disable tooltips
(set-fringe-mode 10)              ; Don't know what it does

(menu-bar-mode -1)                ; Disable menu bar

(setq visible-bell t)             ; Set up the visible bell

(column-number-mode)
;;(global-display-line-numbers-mode t)

;;Enables line numbers for prog mode
(defun my-display-numbers-hook ()
  (display-line-numbers-mode 1)
  )
(add-hook 'prog-mode-hook 'my-display-numbers-hook)

;; Hightlight current line
(global-hl-line-mode t)

;; Automatic scrollng
(setq scroll-conservatively 101)
(setq scroll-margin 20)

(set-face-attribute 'default nil :font "Ubuntu Mono" :height 110)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Ubuntu Mono" :height 115)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 120 :weight 'regular)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-z") nil)

;; M- arrows to move focus between buffers
(windmove-default-keybindings 'meta)

;; (use-package dashboard
    ;;   :ensure t
    ;;   :config
    ;;   (dashboard-setup-startup-hook))

  (use-package dashboard
    :diminish (dashboard-mode page-break-lines-mode)
    :custom
    (dashboard-banner-logo-title "Close the world. Open the nExt.")
    (dashboard-items '((recents  . 7)
                       (bookmarks . 7)
                       (agenda . 5)))

    (dashboard-set-heading-icons t)
    (dashboard-set-navigator t)
    (dashboard-navigator-buttons
     (if (featurep 'all-the-icons)
         `(((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust -0.05)
             "M-EMACS" "Browse M-EMACS Homepage"
             (lambda (&rest _) (browse-url "https://github.com/MatthewZMD/.emacs.d")))
            (,(all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.1)
             "Configuration" "" (lambda (&rest _) (edit-configs)))
            (,(all-the-icons-faicon "cogs" :height 1.0 :v-adjust -0.1)
             "Update" "" (lambda (&rest _) (auto-package-update-now)))))
       `((("" "M-EMACS" "Browse M-EMACS Homepage"
           (lambda (&rest _) (browse-url "https://github.com/MatthewZMD/.emacs.d")))
          ("" "Configuration" "" (lambda (&rest _) (edit-configs)))
          ("" "Update" "" (lambda (&rest _) (auto-package-update-now)))))))
      :config
    (dashboard-setup-startup-hook)
    (dashboard-modify-heading-icons '((recents . "file-text")
                                      (bookmarks . "book"))))

(setq dashboard-startup-banner 'logo)

(use-package doom-themes
  :init (load-theme 'doom-palenight t)
  (doom-themes-visual-bell-config))

(use-package all-the-icons)
   ;; Run M-x all-the-icons-install-fonts to display doom-modeline correctly

 (use-package minions
   :hook (doom-modeline-mode . minions-mode))

 (use-package doom-modeline
   :ensure t
   :init (doom-modeline-mode 1)
   :custom
   (doom-modeline-height 12)
   (doom-modeline-bar-width 6)
   (doom-modeline-lsp t)
   (doom-modeline-minor-modes t)
   )

   (set-frame-parameter (selected-frame) 'alpha '(92 . 90))
   (add-to-list 'default-frame-alist '(alpha . (92 . 90)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy
    :diminish
   (use-package amx :defer t)
    :bind (("C-s" . swiper)
            :map ivy-minibuffer-map
            ("TAB" . ivy-alt-done)
            ("C-l" . ivy-alt-done)
            ("C-n" . ivy-next-line)
            ("C-p" . ivy-previous-line)
            :map ivy-switch-buffer-map
            ("C-p" . ivy-previous-line)
            ("C-l" . ivy-done)
            ("C-d" . ivy-switch-buffer-kill)
            :map ivy-reverse-i-search-map
            ("C-p" . ivy-previous-line)
            ("C-d" . ivy-reverse-i-search-kill))
    :config
    (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (ivy-rich-project-root-cache-mode t))

(use-package counsel
:bind (
         ("C-M-x" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)
         )
:config
(counsel-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t))

(use-package psession
:config
(psession-mode 1)
(psession-autosave-mode 1)
 )

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (visual-line-mode 1))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (efs/org-font-setup))

(setq org-support-shift-select 'always)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)))

(push '("config-unix" . conf-unix) org-src-lang-modes)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(setq c-default-style "java")

(use-package quickrun
    :bind
    (("<f5>" . quickrun)
     ("M-<f5>" . quickrun-shell)
     ("C-c e" . quickrun)
     ("C-c C-e" . quickrun-shell)))

(setq quickrun-timeout-seconds -1)

      (global-set-key (kbd "<f6>") (lambda ()
                                     (interactive)
                                     (setq-local compilation-read-command nil)
                                     (call-interactively 'compile)))

    (setq-default compilation-always-kill t) ; kill compilation process before starting another

    (setq-default compilation-ask-about-save nil) ; save all buffers on `compile'

    (setq-default compilation-scroll-output t)

   (defun cc-mode-compile-clean ()
     (interactive)
     (setq compile-command "cd ../ && make clean")
     (call-interactively 'compile))
   (defun cc-mode-compile ()
     (local-set-key (kbd "C-x C c") 'cc-mode-compile-clean))

   (add-hook 'c-mode-hook 'cc-mode-compile)
   (add-hook 'c++-mode-hook 'cc-mode-compile)

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

 (use-package dap-mode
  :diminish
  :config
  (require 'dap-cpptools)               
      :bind
      (:map dap-mode-map
            (("<f12>" . dap-debug)
             ("<f8>" . dap-continue)
             ("<f9>" . dap-next)
             ("<M-f11>" . dap-step-in)
             ("C-M-<f11>" . dap-step-out)
             ("<f7>" . dap-breakpoint-toggle))))

(use-package lsp-mode
    ;;:straight t
    :commands lsp
    :init
    (setq lsp-keymap-prefix "C-c l") ;; Or 'C-l', 's-l'
    :custom
    (lsp-auto-guess-root nil)
    (lsp-file-watch-threshold 2000)
    (read-process-output-max (* 1024 1024))
    :hook (((c-mode c++-mode objc-mode) . lsp-deferred)
           (lsp-mode . lsp-enable-which-key-integration)
           (lsp-mode . lsp-diagnostics-modeline-mode))
    :bind ("C-c C-c" . #'lsp-execute-code-action)
              (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
    )


  (use-package lsp-ui
    ;;:straight t
    :custom
    (lsp-ui-sideline-enable t)
    (lsp-ui-sideline-show-hover nil)
    (lsp-ui-doc-delay 0.75)
    (lsp-ui-doc-max-height 200)
    (lsp-ui-doc-position 'bottom)
    (lsp-ui-doc-show)
    :after lsp-mode)

  (use-package lsp-ivy
      :after (ivy lsp-mode))
;

(electric-pair-mode 1)

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 0)
  (highlight-indent-guides-auto-character-face-perc 7))
(setq highlight-indent-guides-method 'bitmap)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(setq-default indent-tabs-mode nil)
 (setq-default indent-line-function 'insert-tab)
 (setq-default tab-width 4)
 (setq-default c-basic-offset 4)
 (setq-default js-switch-indent-offset 4)
 (c-set-offset 'comment-intro 0)
 (c-set-offset 'innamespace 0)
 (c-set-offset 'case-label '+)
 (c-set-offset 'access-label 0)
 (c-set-offset (quote cpp-macro) 0 nil)
 (defun smart-electric-indent-mode ()
   "Disable 'electric-indent-mode in certain buffers and enable otherwise."
   (cond ((and (eq electric-indent-mode t)
               (member major-mode '(erc-mode text-mode)))
          (electric-indent-mode 0))
         ((eq electric-indent-mode nil) (electric-indent-mode 1))))
 (add-hook 'post-command-hook #'smart-electric-indent-mode)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package company
  :diminish
  :after lsp-mode
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
              (:map lsp-mode-map
               ("<tab>" . company-indent-or-complete-common))

  :hook (prog-mode . company-mode))
  :custom
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t "Numbers are helpful.")
  (company-tooltip-limit 20 "The more the merrier.")
  (company-tooltip-idle-delay 0.4 "Faster!")
  (company-minimum-prefix-length 1)
  (company-async-timeout 20 "Some requests can take a long time. That's fine.")

  :config
  ;; Use the numbers 0-9 to select company completion candidates
(let ((map company-active-map))
  (mapc (lambda (x) (define-key map (format "%d" x)
 `(lambda () (interactive) (company-complete-number ,x))))
 (number-sequence 0 9))))



      ;; (use-package company
      ;;   :diminish
      ;;   :after lsp-mode
      ;;   :hook (prog-mode . company-mode)
      ;;   :bind (:map company-active-map
      ;;         ("<tab>" . company-complete-selection))
      ;;         (:map lsp-mode-map
      ;;          ("<tab>" . company-indent-or-complete-common))
      ;;   :custom
      ;;   (company-minimum-prefix-length 1)
      ;;   (company-idle-delay 0.0))

  ;; (use-package company-box
  ;;   :diminish
  ;;   :defines company-box-icons-all-the-icons
  ;;   :hook (company-mode . company-box-mode)
  ;;   :custom
  ;;   (company-box-backends-colors nil)
  ;;   )

(use-package dumb-jump
  :bind
  (:map prog-mode-map
        (("C-c C-o" . dumb-jump-go-other-window)
         ("C-c C-j" . dumb-jump-go)
         ("C-c C-i" . dumb-jump-go-prompt)))
  :custom (dumb-jump-selector 'ivy))

(use-package flycheck
  :defer t
  :after org
  :hook
  (org-src-mode . disable-flycheck-for-elisp)
  (lsp-mode . flycheck-mode)
  :custom
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-display-errors-delay 2.0)
  :config
  (global-flycheck-mode)
  (flycheck-set-indication-mode 'left-margin)

  (defun disable-flycheck-for-elisp ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

  (add-to-list 'flycheck-checkers 'proselint))

(use-package flycheck-inline
  :config (global-flycheck-inline-mode))

;; (use-package yasnippet
    ;;   :diminish yas-minor-mode
    ;;   :init
    ;;   (use-package yasnippet-snippets :after yasnippet)
    ;;   :hook ((prog-mode LaTeX-mode org-mode) . yas-minor-mode)
    ;;   :bind
    ;;   (:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key))
    ;;   (:map yas-keymap
    ;;         (("TAB" . smarter-yas-expand-next-field)
    ;;          ([(tab)] . smarter-yas-expand-next-field)))
    ;;   :config
    ;;    (yas-reload-all)
    ;;   (defun smarter-yas-expand-next-field ()
    ;;     "Try to `yas-expand' then `yas-next-field' at current cursor position."
    ;;     (interactive)
    ;;     (let ((old-point (point))
    ;;           (old-tick (buffer-chars-modified-tick)))
    ;;       (yas-expand)
    ;;       (when (and (eq old-point (point))
    ;;                  (eq old-tick (buffer-chars-modified-tick)))
    ;;         (ignore-errors (yas-next-field))))))

(use-package yasnippet
  :defer 3 ;; takes a while to load, so do it async
  :diminish yas-minor-mode
  :config (yas-global-mode)
  :custom (yas-prompt-functions '(yas-completing-prompt)))
  (setq ac-source-yasnippet nil)

(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :custom
  (treemacs-collapse-dirs 3)
  (treemacs-deferred-git-apply-delay 0.5)
  (treemacs-display-in-side-window t)
  (treemacs-file-event-delay 5000)
  (treemacs-file-follow-delay 0.2)
  (treemacs-follow-after-init t)
  (treemacs-follow-recenter-distance 0.1)
  (treemacs-git-command-pipe "")
  (treemacs-goto-tag-strategy 'refetch-index)
  (treemacs-indentation 2)
  (treemacs-indentation-string " ")
  (treemacs-is-never-other-window nil)
  (treemacs-max-git-entries 5000)
  (treemacs-no-png-images nil)
  (treemacs-no-delete-other-windows t)
  (treemacs-project-follow-cleanup nil)
  (treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
  (treemacs-recenter-after-file-follow nil)
  (treemacs-recenter-after-tag-follow nil)
  (treemacs-show-cursor nil)
  (treemacs-show-hidden-files t)
  (treemacs-silent-filewatch nil)
  (treemacs-silent-refresh nil)
  (treemacs-sorting 'alphabetic-desc)
  (treemacs-space-between-root-nodes t)
  (treemacs-tag-follow-cleanup t)
  (treemacs-tag-follow-delay 1.5)
  (treemacs-width 35)
  :config
  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  :bind
  (("M-0"       . treemacs-select-window)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t t"   . treemacs)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map ("C-p" . treemacs-previous-line)))

  (use-package lsp-treemacs
    :after lsp)

  (use-package treemacs-magit
    :defer t
    :after (treemacs magit))

  (use-package treemacs-projectile
    :defer t
    :after (treemacs projectile))

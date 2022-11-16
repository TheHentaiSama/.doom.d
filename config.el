;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Gautier Valentin"
      user-mail-address "valentin.gautier@grenoble-inp.org")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq
 doom-font (font-spec :family "Hack Nerd Font" :size 16)
 doom-big-font (font-spec :family "Hack Nerd Font" :size 32)
 )

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; ORG ============

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Hide org markers
(setq org-hide-emphasis-markers t)

;; ================


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(setq-default tab-width 4)

;; Packages
;; Pokeline <3
(setq poke-line-minimum-window-width 81)
(use-package poke-line
  :ensure t
  :config
  (poke-line-global-mode 1)
  (poke-line-set-pokemon "celebi"))


;; Defining the place to look for banners
(setq
 +doom-dashboard-banner-dir (concat (dir!) "/banners/")
 +doom-dashboard-banner-file "doom.png"
)

;; python
;; add flake8 checker
;;(add-hook 'lsp-after-initialize-hook #'my/addpylint)
;;(defun my/addpylint () (flycheck-add-next-checker 'lsp 'python-flake8 'append))

;; sphinx doc for python
(add-hook 'python-mode-hook (lambda ()
                                (require 'sphinx-doc)
                                (sphinx-doc-mode t)))

;; rainbow delimiters with python
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)

;; enable type hinting for documentation
(setq sphinx-doc-include-types t)

;; functions used for shortcuts

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; just a few shortcuts

(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)


(map! :nvie "M-S-/" #'comment-or-uncomment-region)

(windmove-default-keybindings 'meta)

;; some config

(setq ibuffer-saved-filter-groups
              `(("Default"
                 ;; I create a group call Dired, which contains all buffer in dired-mode
                 ("Dired" (mode . dired-mode))
                 ("Temporary" (name . "\*.*\*"))
                 )))

;; Loading the filters
(add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "Default")))

;; Enable show paren mode with python
(add-hook 'after-init-hook 'show-paren-mode)
(add-hook 'after-init-hook 'flycheck-mode)
(add-hook 'after-init-hook 'smartparens-mode)

;; Disable confirm kill
(setq confirm-kill-emacs nil)

;; add a few keybinds on the dashboard
(map! :map +doom-dashboard-mode-map
      :ne "f" #'find-file
      :ne "r" #'counsel-recentf
      :ne "p" #'doom/open-private-config
      :ne "c" (cmd! (find-file (expand-file-name "config.org" doom-private-dir)))
      :ne "." (cmd! (doom-project-find-file "~/.config/")) ; . for dotfiles
      :ne "b" #'+ivy/switch-workspace-buffer
      :ne "B" #'consult-buffer
      :ne "q" #'save-buffers-kill-terminal)

;; customize imenu with lsp functions
(setq lsp-imenu-index-function #'lsp-imenu-create-categorized-index)


;; keybinding for numpydock
(map! :map python-mode-map
      :desc "generate numpy doc"
      "C-c C-n" #'numpydoc-generate)

;; disable numpydoc prompt
(setq numpydoc-insertion-style nil)
(setq numpydoc-template-short "Complete")
(setq numpydoc-template-long "")
(setq numpydoc-template-arg-desc "Complete")
(setq numpydoc-template-type-desc "Complete")
(setq numpydoc-insert-examples-block nil)

;; ;; define interactive function for switching pyenv
;; ;; and updating lsp
;; (defun switch-venv ()
;;   (interactive)
;;   (call-interactively #'pyenv-mode-set)
;;   (call-interactively #'lsp-workspace-restart)
;;   )

;; Run python does not create a new buffer, starts in current one
(add-to-list 'display-buffer-alist
'("^\\*Python\\*$" . (display-buffer-same-window)))

;; config for pyenv
;; (add-hook 'python-mode-hook #'global-pyenv-mode)

;; Org tree slide
(add-hook 'org-tree-slide-play-hook
          (lambda ()
            (setq-local org-image-actual-width nil)
            (setq-local display-line-numbers nil))
          )

(setq auth-sources '("/home/gautier/.emacs.d/.local/state/authinfo.gpg"))

(defun connect-remote ()
  (interactive)
  (dired "/ssh:gautier@linux1.dg.creatis.insa-lyon.fr:~/"))

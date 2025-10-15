;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Gautier Valentin"
      user-mail-address "valentin.gautier.inp@gmail.com")

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
 doom-font (font-spec :family "Hack Nerd Font" :size 18)
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
(setq org-agenda-files '("~/org/agenda"
                         "~/org/roam/journal"))


;; configure org roam
(use-package! org-roam
  :init
  (setq org-roam-directory "~/org/roam/"
        org-roam-index-file "~/org/roam/index.org"
        org-roam-dailies-directory "journal/"
        org-roam-dailies-capture-templates
        '(("d" "default" entry
           "** %<%H:%M>: %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n")))
        org-roam-capture-templates
         '(
           ("d" "default" plain
            "%?"
            :if-new (file+head "permanent/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
            :unnarrowed t)
           ("i" "inbox" plain
            "%?"
            :target (file+head "inbox/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
            :unnarrowed t)
           ("p" "paper" plain
            "%?"
            :target (file+head "paper/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
            :unnarrowed t)
           )
         )
  :config
  (org-roam-db-autosync-mode))

(use-package! org
  :config
  ;; Configure agenda view
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo . "%b %i %-12:c")
          (tags . " %i %-12:c")
          (search . " %i %-12:c"))))

;;Config biblio
(defvar citar-indicator-notes-icons
  (citar-indicator-create
   :symbol (nerd-icons-mdicon
            "nf-md-notebook"
            :face 'nerd-icons-blue
            :v-adjust -0.3)
   :function #'citar-has-notes
   :padding "  "
   :tag "has:notes"))

(defvar citar-indicator-links-icons
  (citar-indicator-create
   :symbol (nerd-icons-octicon
            "nf-oct-link"
            :face 'nerd-icons-orange
            :v-adjust -0.1)
   :function #'citar-has-links
   :padding "  "
   :tag "has:links"))

(defvar citar-indicator-files-icons
  (citar-indicator-create
   :symbol (nerd-icons-faicon
            "nf-fa-file"
            :face 'nerd-icons-green
            :v-adjust -0.1)
   :function #'citar-has-files
   :padding "  "
   :tag "has:files"))

(after! citar
  (setq! citar-bibliography '("~/Documents/bilbio/references.bib")
         citar-notes-paths '("~/org/roam/paper/")
         citar-indicators(list citar-indicator-files-icons
                               citar-indicator-notes-icons
                               citar-indicator-links-icons)
         citar-org-roam-subdir "paper"
         )  
  )
;; counsel-rg (&optional initial-input initial-directory extra-rg-args rg-prompt)
;; (defun ugt-counsel-rg ()
;;   "Search roam files."
;;   (interactive)
;;   (let ((initial-input "") ;; prefill search with regexp searching for lines starting with `*'
;;         (initial-directory "~/org/roam") ;; Search in
;;         ;; Exclude folders `Backups' and `Apps'; show long lines
;;         (extra-rg-args "-g!#* -g!Backups/* -g!Apps/* --max-columns 600")
;;         (rg-prompt "rg: Search org roam files (narrow with =S-SPC= or =!keyword=): "))
;;     (counsel-rg initial-input
;;                 initial-directory
;;                 extra-rg-args
;;                 rg-prompt)))

(defun ugt-consult-rg ()
  "Search org-roam files using consult-ripgrep."
  (interactive)
  (let* ((default-directory "~/org/roam")
         (consult-ripgrep-args
          "rg --null --line-buffered --color=never --max-columns 600 --no-heading --line-number --hidden -g!#* -g!Backups/* -g!Apps/* ."))
    (consult-ripgrep default-directory)))


(defun my/insert-image-org-link (img)
  "Insert an org image link, choosing the file with completion
and starting from `my-default-image-directory'."
  (interactive
   (list (read-file-name "Image: " "~/Pictures/roam images" nil t)))
  (insert (format "[[file:%s]]" img)))

(defun my/convert-pdf-to-jpg-and-insert (pdf-path)
  "Convert a PDF file at PDF-PATH to JPG and insert a link to it."
  (interactive
   (list (read-file-name "PDF: " "~/Pictures/roam images" nil t nil
                         (lambda (f) (string-match-p "\\.pdf\\'" f)))))
  (let* ((output-path (concat (file-name-sans-extension pdf-path) ".jpg"))
         (cmd (format "convert -density 200 '%s[0]' -quality 100 '%s'"
                      pdf-path output-path)))
    (shell-command cmd)
    (delete-file pdf-path)
    (insert (format "[[%s]]" (file-relative-name output-path)))))

(map! :map org-mode-map
      :desc "insert image"
      "C-c i" #'my/insert-image-org-link)

(map! :map org-mode-map
      :desc "convert to jpeg and insert image"
      "C-c I" #'my/convert-pdf-to-jpg-and-insert)

;; Hide org markers
;; (setq org-hide-emphasis-markers t)

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
(setq poke-line-minimum-window-width 64)
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
;;
;; add flake8 checker
;;(add-hook 'lsp-after-initialize-hook #'my/addpylint)
;;(defun my/addpylint () (flycheck-add-next-checker 'lsp 'python-flake8 'append))

;; rainbow delimiters with python
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)


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
      :ne "c" (cmd! (find-file (expand-file-name "config.org" doom-user-dir)))
      :ne "." (cmd! (doom-project-find-file "~/.config/")) ; . for dotfiles
      :ne "b" #'+ivy/switch-workspace-buffer
      :ne "B" #'consult-buffer
      :ne "q" #'save-buffers-kill-terminal)

;; customize imenu with lsp functions
(setq lsp-imenu-index-function #'lsp-imenu-create-categorized-index)


;; keybinding for numpydock
(map! :map python-mode-map
      :desc "generate-numpy-doc"
      "C-c C-n" #'numpydoc-generate)

;; disable numpydoc prompt
(setq numpydoc-insertion-style nil)
(setq numpydoc-template-short "Complete")
(setq numpydoc-template-long "")
(setq numpydoc-template-arg-desc "Complete")
(setq numpydoc-template-type-desc "Complete")
(setq numpydoc-insert-examples-block nil)


;; Run python does not create a new buffer, starts in current one
(add-to-list 'display-buffer-alist
'("^\\*Python\\*$" . (display-buffer-same-window)))


;; keybinding for lsp ui imenu
(map! :map python-mode-map
      :desc "lsp-ui-imenu"
      "C-c C-i" #'lsp-ui-imenu)

;; Config for pytest
(setq python-pytest-executable "python -m pytest")

;; config for pyenv
;; (add-hook 'python-mode-hook #'global-pyenv-mode)

;; Org tree slide
(add-hook 'org-tree-slide-play-hook
          (lambda ()
            (setq-local org-image-actual-width nil)
            (setq-local display-line-numbers nil))
          )

;; connect to remote
;; (setq auth-sources '("/home/gautier/.config/emacs/.local/state/authinfo.gpg"))

;; (defun connect-remote ()
;;   (interactive)
;;   (dired "/ssh:gautier@linux1.dg.creatis.insa-lyon.fr:~/"))

;; disable documentation in function, still can call it with SPC c k
(setq lsp-signature-render-documentation nil)

;; Shortcut for easily resizing windows
(defhydra doom-window-resize-hydra (:hint nil)
  "
             _k_ increase height
_h_ decrease width    _l_ increase width
             _j_ decrease height
"
  ("h" evil-window-decrease-width)
  ("j" evil-window-increase-height)
  ("k" evil-window-decrease-height)
  ("l" evil-window-increase-width)

  ("q" nil))

(map! :leader
      (:prefix "w"
       :desc "Hydra resize" :n "SPC" #'doom-window-resize-hydra/body))


;; PDF

;; Set pdf-tools as viewer for latex
(setq +latex-viewers '(pdf-tools))

;; Managing gpg secrets for tramp for example. TO be tested
;; (setq auth-sources '("~/.authinfo.gpg")
;;       auth-source-do-cache t
;;       auth-source-cache-expiry 86400 ; All day, defaut is 2h (7200)
;;       password-cache t
;;       password-cache-expiry 86400)
;; (use-package! company-auctex
;;   :after tex
;;   :config
;;   (company-auctex-init))

;; setting backend for company latex
(after! tex
        (set-company-backend! 'LaTeX-mode nil)
        (set-company-backend! 'LaTeX-mode
          'company-reftex-labels
          'company-reftex-citations
          '(company-auctex-macros company-auctex-symbols
                                              company-auctex-environments)
          '(:separate company-dabbrev company-yasnippet)
          'company-auctex-bibs
          'company-auctex-labels
          'company-capf
          )
)

(after! company
  (set-company-backend! 'prog-mode
    'company-files
    '(:separate company-capf
      company-yasnippet)
    )
  (set-company-backend! 'text-mode
    'company-files
    )
  )

(use-package! dired
  :config
  (map! :map dired-mode-map
        :desc "dired-do-kill-lines"
        :n "g K" #'dired-do-kill-lines))

(defun my/evil-toggle ()
  "Toggle Evil mode."
  (interactive)
  (if (bound-and-true-p evil-local-mode)
      (evil-local-mode -1)
    (evil-local-mode 1)))

(map! :desc "Toggle Evil mode"
      "C-c e" #'my/evil-toggle)

(after! evil-escape
  (setq evil-escape-key-sequence "jk"))


(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

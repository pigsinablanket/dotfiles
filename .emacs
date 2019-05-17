;; ---------------------------------------------------------------------
;; Daniel Reimer
;; Last Modified: May 17, 2019
;; ---------------------------------------------------------------------

;; ---------------------------------------------------------------------
;; machine local config
;; ---------------------------------------------------------------------

;; Uncomment if backscpace doesn't work
;; (normal-erase-is-backspace-mode 0)

;; ---------------------------------------------------------------------
;; install any required packages on startup
;; ---------------------------------------------------------------------

(setq package-list '(company
                     fish-mode
                     flycheck
                     jedi
                     magit
                     markdown-mode
                     rainbow-delimiters
                     spaceline
                     web-mode
                     haskell-mode
                     intero
                     ))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; ---------------------------------------------------------------------
;; hooks
;; ---------------------------------------------------------------------

(add-hook 'prog-mode-hook     'rainbow-delimiters-mode)
(add-hook 'after-init-hook    'electric-pair-mode)
(add-hook 'prog-mode-hook     'untabify-mode)
(add-hook 'after-init-hook    'flycheck-mode)
(add-hook 'before-save-hook   'delete-trailing-whitespace)
(add-hook 'haskell-mode-hook  'haskell-indentation-mode)
(add-hook 'markdown-mode-hook 'untabify-mode)
(add-hook 'after-init-hook    'global-auto-revert-mode)
(add-hook 'after-init-hook    'global-company-mode)
(add-hook 'after-init-hook    'ido-mode)
(add-hook 'haskell-mode-hook  'intero-mode)  
;;(add-hook 'python-mode-hook   'jedi:setup)

;; ---------------------------------------------------------------------
;; keybindings
;; ---------------------------------------------------------------------

(global-set-key (kbd "M-x")   'smex)
(global-set-key (kbd "M-X")   'smex-major-mode-commands)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x m") 'view-mode)
(global-set-key (kbd "M-;")   'comment-dwim-line)

;; ---------------------------------------------------------------------
;; customizations
;; ---------------------------------------------------------------------

;; highlight current line
(global-hl-line-mode 1)
(set-face-attribute 'hl-line nil :inherit nil :background "#000000")
(setq undo-tree-visualizer-diff 1)
(setq undo-tree-visualizer-timestamps 1)

;; tree undo mode
(global-undo-tree-mode)

;; show matching parens
(require 'paren)
(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-attribute 'show-paren-match nil :background "#493535")

;; cursor goes to same place from previous sessios
(save-place-mode 1)

;; read-only files open with view-mode
(setq view-read-only t)

;; highligh past 80 characters
(require 'whitespace)
(global-whitespace-mode t)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(add-hook 'window-setup-hook
          (lambda ()
            (set-face-attribute 'whitespace-line nil
                                :foreground nil
                                :background "#404040")))

;; mode-line theme
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(spaceline-toggle-buffer-size-off)

;; Enable ido
(setq ido-enable-flex-matching t)
(setq ido-case-fold t)

;; Use my-backup-file-name function to determine where to place backups
(setq make-backup-file-name-function 'my-backup-file-name)

(setq auto-mode-alist (append '(("\\.html$" . web-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.php$" . web-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.inc$" . web-mode)) auto-mode-alist))

;; Make haskell use unicode characters
(setq haskell-font-lock-symbols t)

;; Still figuring out jedi for python
;; (add-hook 'python-mode-hook '(add-to-list 'company-backends 'company-jedi))
;; (defun my/python-mode-hook ()
;;  (add-to-list 'company-backends 'company-jedi))

;(add-hook 'python-mode-hook 'my/python-mode-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (tango-dark)))
 '(package-selected-packages
   (quote
    (undo-tree helm smex electric-spacing spaceline-all-the-icons flycheck flycheck-pycheckers flymake-python-pyflakes column-enforce-mode jedi smart-mode-line smart-mode-line-powerline-theme company-jedi company autopair magit markdown-mode rainbow-delimiters fish-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))

;; ---------------------------------------------------------------------
;; functions
;; ---------------------------------------------------------------------

;; make backup to a designated dir, mirroring the full path
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
  If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
         (filePath
          (replace-regexp-in-string "[A-Za-z]:" "" fpath ))
         (backupFilePath
          (replace-regexp-in-string "//" "/"
                                    (concat backupRootDir filePath "~"))))
    (make-directory
     (file-name-directory backupFilePath)
     (file-name-directory backupFilePath))
    backupFilePath)
)

;; untabify before saving
(defvar untabify-this-buffer)
(defun untabify-all ()
  "Untabify the current buffer, unless `untabify-this-buffer' is nil."
  (and untabify-this-buffer (untabify (point-min) (point-max)))
)
(define-minor-mode untabify-mode
  "Untabify buffer on save." nil " untab" nil
  (make-variable-buffer-local 'untabify-this-buffer)
  (setq untabify-this-buffer (not (derived-mode-p 'makefile-mode)))
  (add-hook 'before-save-hook #'untabify-all)
)

;; comment line rebinding
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
   If no region is selected and current line is not blank and we are not at the end of the line,
   then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

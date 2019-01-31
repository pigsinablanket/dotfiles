;; ---------------------------------------------------------------------
;; Daniel Reimer
;; Last Modified: Jan 30, 2019
;; ---------------------------------------------------------------------

;; ---------------------------------------------------------------------
;; machine local config
;; ---------------------------------------------------------------------

;; ---------------------------------------------------------------------
;; install any required packages on startup
;; ---------------------------------------------------------------------

(setq package-list '(haskell-mode
                     magit
                     rainbow-delimiters
                     markdown-mode
                     fish-mode
                     web-mode))

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

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'prog-mode-hook 'untabify-mode)

;; ---------------------------------------------------------------------
;; keybindings
;; ---------------------------------------------------------------------

(global-set-key (kbd "C-x g") 'magit-status)

;; ---------------------------------------------------------------------
;; session customizations
;; ---------------------------------------------------------------------

(setq column-number-mode t)
(setq make-backup-file-name-function 'my-backup-file-name)

(setq auto-mode-alist (append '(("\\.html$" . web-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.php$" . web-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.inc$" . web-mode)) auto-mode-alist))

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
    (magit markdown-mode rainbow-delimiters fish-mode))))

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


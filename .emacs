(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(setq-default tab-width 2)
(setq tab-stop-list (number-sequence 2 120 2))
(setq-default indent-tabs-mode nil)

(defun my-web-mode-hook () "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)
(setq auto-mode-alist (append '(("\\.html$" . web-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.php$" . web-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.inc$" . web-mode)) auto-mode-alist))

(custom-set-variables
 '(css-indent-offset 2)
 '(indent-tabs-mode t)
 '(js-indent-level 2))
(custom-set-faces)

(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "orange red"))))
 '(font-lock-string-face ((t (:foreground "#ab00ab"))))
 '(haskell-definition-face ((t (:foreground "#1070ff"))))
 '(haskell-constructor-face ((t (:foreground "#009900"))))
 )

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'prog-mode-hook #'enable-paredit-mode)

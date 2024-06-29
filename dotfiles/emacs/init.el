;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-enabled-themes '(tsdh-dark))
 '(inhibit-startup-screen t)
 '(ispell-dictionary nil)
 '(package-selected-packages '(verilog-mode ## rust-mode popup)))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-faces-vector
;;    [default default default italic underline success warning error])
;;  '(ansi-color-names-vector
;;    ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
;;  '(custom-enabled-themes '(tsdh-dark))
;;  '(package-selected-packages
;;    '(htmlize dante csv-mode auto-correct processing-mode org-edna)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(add-to-list 'backup-directory-alist (cons "." "~/.emacs.extra/backup/"))

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.org/packages/")
   t))

(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)

;; Easy switch between visible buffers
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)


;; Disable the behaviour of centering the cursor on moving out of visible buffer boundary
(setq scroll-conservatively 100)

(add-hook 'shell-mode-hook
      (lambda ()
        (face-remap-set-base 'comint-highlight-prompt :inherit nil)))

(setq verilog-indent-level 2)

(let ((default-directory (expand-file-name "~/.emacs.d/lisp")))
(setq load-path
(append
(let ((load-path (copy-sequence load-path)))
(append
(copy-sequence (normal-top-level-add-to-load-path '(".")))
(normal-top-level-add-subdirs-to-load-path)))
load-path)))

(autoload 'qgrep "qgrep" "Quick grep" t)
(autoload 'qgrep-no-confirm "qgrep" "Quick grep" t)
(autoload 'qgrep-confirm "qgrep" "Quick grep" t)
(global-set-key (kbd "\C-c g") 'qgrep-no-confirm)
(global-set-key (kbd "\C-c G") 'qgrep-confirm)

(global-linum-mode 1)

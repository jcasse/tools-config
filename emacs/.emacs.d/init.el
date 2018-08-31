;;; PACKAGE --- Emacs configuration.

;;; Commentary:

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start Emacs Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load local packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (file-exists-p (expand-file-name "~/.emacs.d/local-packages"))
    (setq load-path
          (cons (expand-file-name "~/.emacs.d/local-packages") load-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/"))

;; Initialize the package manager, refreshing the package-archive-contents.
;; We'll activate the previously-downloaded and installed packages in a
;; little bit.
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
      (package-install 'use-package))

(eval-when-compile
    (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Key Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C-c R select recent files.
(global-set-key (kbd "C-c R")
                'recentf-open-files)
;; C-c s pushes location to org's link stack.
(global-set-key (kbd "C-c s")
                'org-store-link)
;; C-c h takes a note, files in *-notes.org.
(global-set-key (kbd "C-c h")
                'org-capture)
;; C-x g starts magit.
(global-set-key (kbd "C-x g")
                'magit-status)
;; C-x g opens magit popup of popups.
(global-set-key (kbd "C-x M-g")
                'magit-dispatch-popup)
;; C-c c comments/uncomments a region.
(global-set-key (kbd "C-c c")
                'comment-dwim)
;; Display buffer list in other window.
(define-key global-map [remap list-buffers] 'buffer-menu-other-window)

;; The following keybindings are only for two windows layout.
;; You can use M-[ or M-] to navigate the pages while your cursor is in
;; another window.
(fset 'doc-prev "\C-xo\C-x[\C-xo")
(fset 'doc-next "\C-xo\C-x]\C-xo")
(global-set-key (kbd "M-[") 'doc-prev)
(global-set-key (kbd "M-]") 'doc-next)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is a hook that executes before a source code file is loaded.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'prog-mode-hook
          '(lambda ()
             (paren-activate)
             (turn-on-auto-fill)
             (flycheck-mode)
             (flyspell-prog-mode)
             (display-line-numbers-mode)
             (rainbow-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;
;; To render the markdown in html in default browser:
;;   M-x flymd-flyit
;;
;; Markdown Mode - syntax highlighting
;; GFM Mode (GitHub Flavored Markdown) - syntax highlighting
;; Flymd - HTML rendering
;;
;; https://jblevins.org/projects/markdown-mode/
;; https://codemirror.net/mode/gfm/
;; https://leanpub.com/markdown-mode/read#gfm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (use-package flymd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CMake
;;
;; cmake-mode (Syntax highlighting)
;; https://gitlab.kitware.com/cmake/community/wikis/doc/editors/Emacs
;;
;; cmake-project (CMake build process integration with Emacs)
;; https://github.com/alamaison/emacs-cmake-project
;; To compile code:
;;   M-x cmake-project-configure-project
;;   M-x compile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cmake-mode)
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

(require 'cmake-project)
(defun maybe-cmake-project-hook ()
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
(add-hook 'c-mode-hook 'maybe-cmake-project-hook)
(add-hook 'c++-mode-hook 'maybe-cmake-project-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General preferences.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable annoying notifications.
(setq ring-bell-function 'ignore)

;; Disable scroll bar.
(toggle-scroll-bar -1)

;; Transparent.
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
;;(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
;;(add-to-list 'default-frame-alist '(alpha . (85 . 50)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Backup.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://pragmaticemacs.com/emacs/auto-save-and-backup-every-save/
;; https://emacs.stackexchange.com/questions/33/put-all-backups-into-one-backup-
;;       folder
(let ((backup-dir "~/.emacs.d/backups")
      (auto-saves-dir "~/.emacs.d/auto_saves"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FONT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Deftheme themes can override the default face.  This means that customizing
;; the default face, as I used to do, accomplishes nothing anyway.  Might as
;; well set the default Latin font explicitly then.
;;
;; This code is from cinsk at https://www.emacswiki.org/emacs/SetFonts.  The
;; default font size is in points * 10, so 140 is a 14pt font.  Fonts may be
;; clipped if the font size is unsupported, so check with your favorite font
;; browser first if you're using an unusual point size.
;;
;;
;;     (set-face-attribute 'default nil :family "Consolas" :height 245)
;;
;; Disabled.  The above code _works_, but it doesn't handle 4K displays very
;; well (a 24.5-point font is livable on a 4K monitor but a bit on the large
;; side at standard 1080P resolution.)  Whereas this solution below, from
;; https://coderwall.com/p/ifgyag/change-font-size-in-emacs-dynamically-based-
;; on-screen-resolution, is resolution-independent.
;;
;;(defun my-fontify-frame (frame)
;;  (interactive)
;;  (if window-system
;;      (progn
;;        (if (> (x-display-pixel-width) 1920)
;;            (set-frame-parameter frame 'font "Source Code Pro 18")
;;          (set-frame-parameter frame 'font "Source Code Pro 12")))))
;;
;; Fontify current frame
;;(my-fontify-frame nil)
;;
;; Fontify any future frames
;;(push 'my-fontify-frame after-make-frame-functions)

(set-face-attribute 'default nil :height 160)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Orgmode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Transparent encryption and decryption
;; https://orgmode.org/worg/org-tutorials/encrypting-files.html
(require 'epa-file)
(epa-file-enable)

;;; GPG support.

;; Allow *.asc files to be automatically encrypted by EasyPG Assistant just
;; like *.gpg files, but with ASCII armor.
;;
;; Why?  Because I like being able to copy and paste encrypted messages in
;; plaintext.  I think compressed, encrypted, ASCII-armored messages are a
;; brilliant concept.

(defun my-enable-ascii-armor-before-epa-save()
  "Sets `epa-armor' to t right before any file that looks like *.asc is saved.  The assumption is that such files are meant to be encrypted with EasyPG and, unlike EasyPG's default, ought to be ascii-armored.  Note that *.asc files must _also_ match the epa-file-name-regexp."
  (if (and (featurep 'epa-file)
           (string-match "\\.asc$" (format "%s" (buffer-file-name))))
      (progn
        ;(message "ASCII armor enabled")
        (setq epa-armor t))))

(defun my-disable-ascii-armor-after-epa-save()
  "Sets `epa-armor' to nil right after any file that looks like *.asc is saved.  Emacs's documentation currently frowns upon just leaving `epa-armor' permanently set to t."
  (if (and (featurep 'epa-file)
           (string-match "\\.asc$" (format "%s" (buffer-file-name))))
      (progn
        ;(message "ASCII armor disabled")
        (setq epa-armor nil))))

(add-hook 'before-save-hook 'my-enable-ascii-armor-before-epa-save)
(add-hook 'after-save-hook  'my-disable-ascii-armor-after-epa-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evil package. Vim emulator.
;; https://blog.aaronbieber.com/2016/01/23/living-in-evil.html
(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :ensure t))

;; Installs Magit (Emacs' most popular Git interface)
(use-package magit)

;; Installs mic-paren (like advanced version of show-paren-mode)
(use-package mic-paren
  :commands paren-activate)

;; Installs Flycheck (Runtime syntax checker frontend for all programming modes)
(use-package flycheck)

;; Auto-completer, indentation, etc.
(use-package smart-tab)
;; (define-key read-expression-map [(tab)] 'hippie-expand)
;; (defun hippie-unexpand ()
;;   (interactive)
;;   (hippie-expand 0))
;; (define-key read-expression-map [(shift tab)] 'hippie-unexpand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PATH and exec-path
;;
;; The problem on Mac OS X is that Mac OS does not set the environment the
;; same when you call a program from the global UI or when you call it from a
;; shell. This means that running Emacs from a shell will result in different
;; environment variables being set than when you run it from the finder. This
;; is especially annoying if you set environment variables in .bashrc or
;; similar, as that won't affect the "global" Emacs.
;;
;; NOTE:
;;       https://www.emacswiki.org/emacs/ExecPath
;;       https://emacs.stackexchange.com/questions/550/exec-path-and-path
;;            PATH: its value is used by Emacs' shell and eshell
;;       exec-path: its value is used by Emacs when searching for programs
;;       Emacs does set exec-path from the value of PATH on startup, but will
;;       not look at it again later.
;;
;; Adding to the variables can be done in two ways:
;;   - (add-to-list 'exec-path "<path>") ;this adds to the beginning
;;   - (setq exec-path (append '("<path1>" "<path2" ... ) exec-path))
;;
;; NOTE: The variable load-path is for EmacsLisp libraries.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get PATH from bash environment
;; (defun get-bash-path ()
;;   (interactive)
;;   (let ((path (shell-command-to-string "if [[ -f ~/.bash_profile ]]; then . ~/.bash_profile; elif [[ -f ~/.bashrc ]]; then . ~/.bashrc; fi; echo -n $PATH")))
;;     (message "path: %s" path)
;;     (setenv "PATH" path)
;;     (setq exec-path
;;           (append
;;            (split-string-and-unquote path ":")
;;            exec-path)))
;;   )
;; (get-bash-path)

;; (setq exec-path (append exec-path '("/usr/local/bin")))

;; The exec-path-from-shell package does the same thing as the function above!
;;(exec-path-from-shell-initialize)
(when (memq window-system '(mac ns)) (exec-path-from-shell-initialize))
;;(use-package exec-path-from-shell
;;  :init
;;  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Installs AUCTeX (Emacs' TeX package)
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'display-line-numbers-mode)
  (setq reftex-plug-into-AUCTeX t))

(add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document View
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'doc-view-mode-hook
          (lambda ()
            (auto-revert-mode))) ;update document image upon change

;; Auto fit image to window width
;; Does not work strait up because the PDF file is first converted to PNG
;; and then displayed as PNG. So, the (doc-view-fit-width-to-window) fails
;; because the image still has not been loaded.
;; There is a solution that employs timers:
;; http://lists.gnu.org/archive/html/emacs-devel/2012-03/msg00407.html
;; For now, do it by hand: M-x doc-view-fit-width-to-window RET


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go Mode
;; https://github.com/dominikh/go-mode.el
;; https://johnsogg.github.io/emacs-golang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install additional Go tools (godoc, etc.)
;; Execute from shell:
;; go get golang.org/x/tools/cmd/...

;; Automatically call gofmt on save
(add-hook 'before-save-hook 'gofmt-before-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables set via Emacs interface.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: We chose to enable display-line-numbers-mode on a per-mode basis
;;       instead of global, because we were not able to disable it for
;;       Doc View mode with a hook.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (rebecca)))
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "1b27e3b3fce73b72725f3f7f040fd03081b576b1ce8bbdfcb0212920aec190ad" "73a13a70fd111a6cd47f3d4be2260b1e4b717dbf635a9caee6442c949fad41cd" "64ca5a1381fa96cb86fd6c6b4d75b66dc9c4e0fc1288ee7d914ab8d2638e23a9" "27b97024320d223cbe0eb73104f2be8fcc55bd2c299723fc61d20057f313b51c" "f09acf642ecd837d2691ba05c6f3e1d496f7930b45bf41903e7b37ea6579aa79" "946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c" default)))
 '(delete-selection-mode t)
 '(display-line-numbers nil)
 '(epa-file-name-regexp "\\.\\(gpg\\|asc\\)\\(~\\|\\.~[0-9]+~\\)?\\'")
 '(epg-gpg-program "/usr/local/bin/gpg")
 '(fill-column 78)
 '(flycheck-clang-language-standard "c++17")
 '(flycheck-disabled-checkers (quote (python-pycompile python-pylint)))
 '(flycheck-python-flake8-executable "/Users/jcasse/anaconda3/bin/flake8")
 '(global-display-line-numbers-mode nil)
 '(global-smart-tab-mode t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-use-virtual-buffers t)
 '(indent-tabs-mode nil)
 '(line-number-mode nil)
 '(menu-bar-mode nil)
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("marmalade" . "https://marmalade-repo.org/packages/")
     ("melpa stable" . "https://stable.melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (cpputils-cmake cmake-project cmake-mode go-mode exec-path-from-shell auctex smart-mode-line-powerline-theme flymd smart-tab markdown-mode mic-paren flycheck rebecca-theme ahungry-theme magit evil-indent-textobject evil-surround evil-jumper evil-leader use-package evil rainbow-mode web-mode)))
 '(recentf-max-saved-items 500)
 '(recentf-mode t)
 '(red "#ffffff")
 '(save-place-mode t)
 '(savehist-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(use-package-always-ensure t)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number-current-line ((t (:inherit line-number :foreground "dark magenta")))))
(put 'dired-find-alternate-file 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  CC Mode (for editing files containing C, C++, Objective-C, Java, etc.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default c-basic-offset 4)

(defun my-c-mode-common-hook ()
  "Set up coding style for C-type family of languages."
  (setq flycheck-gcc-language-standard "c++17")
  (rainbow-mode 0))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line Numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq column-number-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spell checking.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; brew install aspell

;; gives arguments to the spell checking program
(setq ispell-extra-args '("--sug-mode=normal" "--clear-sug-split-char"))

(setq ispell-program-name '"/usr/local/bin/aspell")

; Load abbrevs when Emacs starts up.
(quietly-read-abbrev-file)

; Enable abbrev-mode by default.
(setq default-abbrev-mode t)

;; I'm tired of losing all my lovely in-memory abbrevs just because Emacs
;; closed unexpectedly, so save them every day at 6 AM.
(setq my-abbrev-timer (run-at-time "6:00am" 86400 'write-abbrev-file))

;; By setting flyspell-abbrev-p and flyspell-use-global-abbrev-table-p to
;; true in Custom, spelling corrections are now automatically added to the
;; abbrev table, too!  So now shift-F7 causes emacs to correct a misspelling
;; *permanently*.  Not bad at all.

(require 'flyspell)

(setq flyspell-abbrev-p t)
(setq flyspell-issue-message-flag nil)
(setq flyspell-use-global-abbrev-table-p t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Column 80 marker
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; Set Mode Line border.
(set-face-attribute 'mode-line nil
                    :box '(:width 0))

;; Vertical divider.
(set-face-background 'vertical-border "gray")
(set-face-foreground 'vertical-border (face-background 'vertical-border))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fset 'yes-or-no-p 'y-or-n-p)

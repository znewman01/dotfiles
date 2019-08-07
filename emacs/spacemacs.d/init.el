(setq linux? (string= system-type "gnu/linux"))
(setq mac? (string= system-type "darwin"))

(defun zjn--shell (cmd)
  (let ((strip-newlines (lambda (string) (replace-regexp-in-string "\n\\'" "" string))))
    (funcall strip-newlines (shell-command-to-string cmd))))

(setq zjn--mu4e-path
      (expand-file-name
       (if linux? "share/emacs/site-lisp/mu4e"
         "share/emacs/site-lisp/mu/mu4e")
       (if linux? (zjn--shell "nix-build '<nixpkgs>' --no-build-output -A mu")
         (zjn--shell "brew --prefix mu"))))

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   dotspacemacs-additional-packages '(
     blacken
     org-ref
     visual-fill-column
     flycheck-pycheckers
     yasnippet-snippets
     evil-collection
     dracula-theme
     leuven-theme
     direnv
   )
   dotspacemacs-excluded-packages '(
     smartparens org-projectile smeargle neotree
     yasnippet
   ))
  (let ((common-layers
         `(
           (c-c++ :variables c-c++-enable-clang-support t)
           markdown
           python
           (config :location local)
           helm
           html
           auto-completion
           emacs-lisp
           git
           org
           (shell :variables
                  shell-default-shell 'ansi-term
                  shell-default-height 25
                  shell-default-position 'bottom)
           semantic
           syntax-checking
           python
           (latex :variables latex-build-command "LatexMk")
           (mu4e :variables mu4e-installation-path ,zjn--mu4e-path)
           pdf-tools
           elfeed)))
    (setq-default
     dotspacemacs-configuration-layers
     (append common-layers
             (if linux? '(nixos) '()))
     )))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'vim
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   dotspacemacs-themes (if linux? '(dracula leuven) '(spacemacs-light spacemacs-dark))
   dotspacemacs-which-key-delay 0.2
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-emacs-command-key "SPC"
   ))

(defun dotspacemacs/user-init ()
  ; https://github.com/emacs-evil/evil-collection/issues/60
  (setq evil-want-keybinding nil)

  (setq custom-file "/dev/null")

  ; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  )

(defun dotspacemacs/user-config ()
  (if linux? (direnv-mode))
  (setq evil-want-Y-yank-to-eol nil)
  (setq powerline-default-separator 'utf-8)
  (setq spacemacs-theme-org-agenda-height nil
        spacemacs-theme-org-height nil
        spacemacs-theme-org-highlight nil)
  (define-coding-system-alias 'utf8 'utf-8)
  (if mac? (setq browse-url-generic-function 'browse-url-default-macosx-browser))
  (setq-default require-final-newline t)
  (setq split-width-threshold nil
        even-window-sizes nil)

  (with-eval-after-load 'visual-fill-column
    (add-hook 'visual-fill-column-mode-hook 'spacemacs/toggle-visual-line-navigation-on)
    (setq split-window-preferred-function 'visual-fill-column-split-window-sensibly)
    (advice-add 'text-scale-adjust :after 'visual-fill-column-adjust))

  (with-eval-after-load 'dired
    (require 'evil-collection-dired)
    (evil-collection-dired-setup))

  ;; Work-arounds
  ; https://github.com/syl20bnr/spacemacs/issues/10316
  (with-eval-after-load 'yasnippet
    (let* ((bad-dirs
            `(,(expand-file-name "snippets" dotspacemacs-directory)))
           (bad? (lambda (x) (member x bad-dirs))))
      (setq yas-snippet-dirs (remove-if bad? yas-snippet-dirs))))

  ; https://github.com/politza/pdf-tools/issues/480
  ; May need to retry to installation after everything loads
  (if mac?
      (setenv "PKG_CONFIG_PATH"
              (concat (shell-command-to-string "printf %s \"$(brew --prefix libffi)\"")
                      "/lib/pkgconfig/")))

  ;; Python
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (setq flycheck-pycheckers-checkers '(pylint mypy3))
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

  ;; C/C++
  (add-hook 'c-mode-hook
            (lambda ()
              (define-key c-mode-map [tab] 'clang-format-buffer)))
  (defun clang-format-buffer-smart ()
    "Reformat buffer if .clang-format exists in the projectile root."
    (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
      (clang-format-buffer)))
  (defun clang-format-buffer-smart-on-save ()
    "Add auto-save hook for clang-format-buffer-smart."
    (add-hook 'before-save-hook 'clang-format-buffer-smart nil t))
  (spacemacs/add-to-hooks 'clang-format-buffer-smart-on-save
                          '(c-mode-hook c++-mode-hook))

  ; RefTeX/AUCTeX
  (setq-default TeX-master nil)
  (setq TeX-parse-self t)
  (setq TeX-auto-save t)
  (add-hook 'plain-TeX-mode-hook 'LaTeX-mode)
  (require 'visual-fill-column)
  (setq visual-line-fringe-indicators '(left-curly-arrow nil))
  (add-hook 'TeX-latex-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  (add-hook 'visual-line-mode-hook #'turn-off-auto-fill)
  )
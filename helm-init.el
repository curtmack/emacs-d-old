(use-package helm-config
  :ensure helm
  :config
  (helm-mode 1)
  (helm-adaptive-mode 1)
  (helm-push-mark-mode 1))

(use-package helm-ag)

(use-package helm-aws)

(use-package helm-circe)

(use-package helm-descbinds)

(use-package helm-ls-git)

(use-package helm-make)

(use-package helm-mode-manager)

(use-package helm-rhythmbox)

(use-package helm-themes)

(use-package helm-unicode)

(use-package helm-projectile
  :config
  (evil-leader/set-key
    "pb" 'helm-projectile-switch-to-buffer
    "pd" 'helm-projectile-find-dir
    "pf" 'helm-projectile-find-file
    "ph" 'helm-projectile
    "pp" 'helm-projectile-switch-project
    "pr" 'helm-projectile-recentf
    "pv" 'projectile-vc))

(use-package evil
  :diminish undo-tree-mode)

;; evil plugins
;; many have to be configured before evil-mode is turned on, so
;; might as well configure them all up here
(use-package evil-anzu)

(use-package evil-args
  :config
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg)

  ;; bind evil-jump-out-args
  (define-key evil-normal-state-map "K" 'evil-jump-out-args))

(use-package evil-easymotion
  :config
  (evilem-default-keybindings "SPC"))

(use-package evil-escape
  :diminish evil-escape-mode
  :init
  (setq-default evil-escape-key-sequence "jk")
  :config
  (evil-escape-mode))

(use-package evil-exchange
  :config
  (evil-exchange-install))

(use-package evil-iedit-state)

(use-package evil-indent-plus
  :config
  (evil-indent-plus-default-bindings))

(use-package evil-jumper
  :config
  (evil-jumper-mode t))

(use-package evil-leader
  :config
  (evil-leader/set-leader ",")
  (global-evil-leader-mode))

;; ;; not sure where this should be configured for now
;; (use-package evil-magit)

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

;; ;; necessary?
;;(use-package evil-mc)

(use-package evil-nerd-commenter
  :config
  (evil-leader/set-key ";" 'evilnc-comment-operator))

(use-package evil-numbers)

;; ;; holding off on this
;;(use-package evil-paredit)

(use-package evil-quickscope
  :diminish evil-quickscope-mode
  :config
  (global-evil-quickscope-mode 1))

(use-package evil-rsi
  :diminish evil-rsi-mode
  :config
  (evil-rsi-mode))

(use-package evil-search-highlight-persist
  :config
  (global-evil-search-highlight-persist t))

(use-package evil-smartparens
  :diminish evil-smartparens-mode
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; ;; this is broken, don't feel like fixing it right now
;; (use-package evil-tabs
;;   :config
;;   (global-evil-tabs-mode t))

(use-package evil-visual-mark-mode
  :config
  (evil-visual-mark-mode))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode t))

;; this isn't technically part of evil, but it's vimish so it goes here
(use-package vi-tilde-fringe
  :diminish vi-tilde-fringe-mode
  :config
  (global-vi-tilde-fringe-mode))

(evil-mode 1)

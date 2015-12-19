(use-package company)
(use-package company-quickhelp)
(use-package web-completion-data)
(use-package company-web-html
  :ensure company-web)

(add-hook 'after-init-hook (lambda ()
                             (global-company-mode)
                             (company-quickhelp-mode 1)))

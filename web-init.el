(use-package web-mode
  :mode "\\.phtml\\'"
  :mode "\\.tpl\\.php\\'"
  :mode "\\.[agj]sp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.erb\\'"
  :mode "\\.mustache\\'"
  :mode "\\.djhtml\\'"
  :mode "\\.html?\\'")

(use-package js2-mode
  :mode "\\.js\\'")

(use-package js2-refactor)

(use-package json-mode
  :mode "\\.json\\'")

(use-package json-reformat)

(use-package json-snatcher)

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package markdown-toc)

(use-package mmm-mode
  :config
  (setq mmm-global-mode 'maybe)
  (mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php))

;; use-package has built-in support for keybindings, but it's not really good for evil keybinds
;; maybe adding that support myself would be a fun project, but for now, this

;; <leader>f - file operations
;; ;; need to fix this binding
;; (evil-leader/set-key "ff" (lambda () (helm :sources 'helm-files-in-current-dir-source)))
(evil-leader/set-key "fs" 'save-buffer)
(evil-leader/set-key "fS" (lambda () (evil-save (read-input "New filename:" nil nil (buffer-file-name)))))

;; <leader>q - quit operations
(evil-leader/set-key "qq" 'evil-quit-all)

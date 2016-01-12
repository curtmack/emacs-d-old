;; universal keybindings - basic operations

;; <leader><symbols> - various
(evil-leader/set-key
  ":" 'execute-extended-command)

;; <leader>a - applications
(evil-leader/set-key
  "ac" 'calc-dispatch
  "ad" 'dired
  "ap" 'list-processes
  "aP" 'proced
  "au" 'undo-tree-visualize)

;; <leader>b - buffers
(evil-leader/set-key
  "bd" 'kill-this-buffer
  "bk" 'kill-buffer
  "bw" 'read-only-mode)

;; <leader>c - compilation
(evil-leader/set-key
  "cC" 'compile
  "ck" 'kill-compilation
  "cr" 'recompile)

;; <leader>e - errors
(evil-leader/set-key
  "en" 'next-error
  "ep" 'previous-error
  "ep" 'previous-error
  "eN" 'previous-error)

;; <leader>f - file operations
(evil-leader/set-key
  "fS" 'evil-write-all
  "fs" 'save-buffer)

;; <leader>n - narrowing and widening
(evil-leader/set-key
  "nr" 'narrow-to-region
  "np" 'narrow-to-page
  "nf" 'narrow-to-defun
  "nw" 'widen)

;; <leader>q - quit operations
(evil-leader/set-key
  "qq" 'evil-quit-all)

;; <leader>u - universal argument
(evil-leader/set-key
  "u" 'universal-argument)

;; <leader>w - window
(evil-leader/set-key
  "wc" 'delete-window
  "wH" 'evil-window-move-far-left
  "wh" 'evil-window-left
  "wJ" 'evil-window-move-very-bottom
  "wj" 'evil-window-down
  "wK" 'evil-window-move-very-top
  "wk" 'evil-window-up
  "wL" 'evil-window-move-far-right
  "wl" 'evil-window-right
  "ws" 'split-window-below
  "wS" 'split-window-below-and-focus
  "wv" 'split-window-right
  "wV" 'split-window-right-and-focus
  "w=" 'balance-windows)

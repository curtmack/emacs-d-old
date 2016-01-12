;; path fix for Cygwin
;; emacs-w32 in Cygwin doesn't know how to find POSIX binaries
;; let's help it because we're nice people
(if (eq system-type 'cygwin)
    (setq exec-path (append '("/usr/local/bin" "/usr/bin") exec-path)))

;; initialize package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; utility function
(defun make-emacs-d-subpath (path)
  (expand-file-name path user-emacs-directory))

(setq message-log-max 16384)

(eval-and-compile
  ;; site-lisp
  (mapc
   #'(lambda (path)
       ;; some packages store their main .el in the root directory
       (push path load-path)
       ;; others store their main .el in a lisp/ subdirectory
       (let ((lispdir (expand-file-name "lisp/" path)))
         (when (file-exists-p lispdir)
           (push lispdir load-path))))
   (directory-files (make-emacs-d-subpath "site-lisp/")
                    t
                    directory-files-no-dot-files-regexp))

  ;; add base16 themes
  (push (make-emacs-d-subpath "site-lisp/base16/") custom-theme-load-path))

(eval-and-compile
  (require 'cl)
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(setq use-package-always-ensure t)

;; libraries
(use-package dash            :defer t)
(use-package dash-functional :defer t)
(use-package let-alist       :defer t)
(use-package popup           :defer t)
(use-package popwin          :defer t)
(use-package pos-tip         :defer t)
(use-package s               :defer t)

;; dependencies that I don't directly use
(use-package avy      :defer t)
(use-package iedit    :defer t)
(use-package magit    :defer t)

;; setup built-in mode hooks
(add-hook 'prog-mode-hook (lambda () (linum-mode t)))

;; powerline setup
(use-package powerline      :demand)
(use-package powerline-evil :demand)
(load-file (make-emacs-d-subpath "my-powerline-theme.el"))

;; smooth scrolling
(use-package smooth-scrolling :demand)

;; which key
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

;; adaptive wrap
(use-package adaptive-wrap
  :diminish adaptive-wrap-prefix-mode
  :config
  (adaptive-wrap-prefix-mode))
;; aggressive indent mode
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config
  (global-aggressive-indent-mode 1))
;; smart tabs setup
(use-package smart-tabs-mode
  :config
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python 'ruby)
  (let ((set-tabs (lambda () (setq indent-tabs-mode t))))
    (add-hook 'c-mode-hook      set-tabs)
    (add-hook 'c++-mode-hook    set-tabs)
    (add-hook 'java-mode-hook   set-tabs)
    (add-hook 'js2-mode-hook    set-tabs)
    (add-hook 'cperl-mode-hook  set-tabs)
    (add-hook 'python-mode-hook set-tabs)
    (add-hook 'ruby-mode-hook   set-tabs)))
;; page-break-lines
(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
;; rainbow-identifiers
(use-package rainbow-identifiers
  :config
  ;; ;; deliberately don't enable this for all programming modes
  ;; ;; it's pretty annoying to have all the time so it'll have a toggle binding
  ;;(add-hook 'prog-mode-hook #'rainbow-identifiers-mode)
  )

;; evil setup and keybindings
(load-file (make-emacs-d-subpath "evil-init.el"))
(load-file (make-emacs-d-subpath "evil-keybinds.el"))

;; circe setup
(load-file (make-emacs-d-subpath "circe-init.el"))

;; company setup
(load-file (make-emacs-d-subpath "company-init.el"))

;; web setup
(load-file (make-emacs-d-subpath "web-init.el"))

;; projectile setup
(load-file (make-emacs-d-subpath "projectile-init.el"))

;; helm setup
(load-file (make-emacs-d-subpath "helm-init.el"))

;; clojure setup
(load-file (make-emacs-d-subpath "clojure-init.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(adaptive-wrap-extra-indent 2)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#232c31" "#2a5491" "#237986" "#a03b1e" "#484d79" "#c59820" "#484d79" "#9ea7a6"])
 '(ansi-term-color-vector
   [unspecified "#232c31" "#2a5491" "#237986" "#a03b1e" "#484d79" "#c59820" "#484d79" "#9ea7a6"])
 '(custom-enabled-themes (quote (base16-phd-dark)))
 '(custom-safe-themes
   (quote
    ("aed73c6d0afcf2232bb25ed2d872c7a1c4f1bda6759f84afc24de6a1aec93da8" "f245c9f24b609b00441a6a336bcc556fe38a6b24bfc0ca4aedd4fe23d858ba31" "e8bba3c8e8caea2c7a8b6932b0db8d9bdb468c9b44bf554f37b56093d23fde57" "f2503f0a035c2122984e90eb184185769ee665de5864edc19b339856942d2d2d" "e8e744a1b0726814ac3ab86ad5ccdf658b9ff1c5a63c4dc23841007874044d4a" "b4ec581daad15aa7020b722523dc6bcea850bfbdbe31bfeb11c45ea51899bd75" "9e87bddff84cbce28c01fa05eb22f986d770628fa202cd5ca0cd7ed53db2f068" "890d09dcc8d2326e98eee74b307b2cc42f07ab7701bcff521e6152aa3e08f7a8" "06fc6014871028d24b4e03db24b9efee48bd73dce0afdc15e9124f09fab64afa" "9f6750057fefba39c184783c7b80ddd9c63bc6e8064846b423b4362c9e930404" "aa87469691932ff791f966bffb885ecd97ebfa4dc4d42e479f3819ac4a3fbcaf" "c70cc9c4c6257d70f5c11b90cb9e8b1e54e6edd6aa43f39879746e16a70533f5" "101a10b15bbbd0d5a0e56e4773e614962197886780afb2d62523a63a144ad96c" "43aeadb0c8634a9b2f981ed096b3c7823c511d507a51c604e4667becb5ef6e35" "e254f8e18ba82e55572c5e18f3ac9c2bd6728a7e500f6cc216e0c6f6f8ea7003" "03e3e79fb2b344e41a7df897818b7969ca51a15a67dc0c30ebbdeb9ea2cd4492" "50e7f9d112e821e42bd2b8410d50de966c35c7434dec12ddea99cb05dd368dd8" "e1551b5516e0a439b6ab019ba00cee866e735f66f22ff67a5d882ad0f1383454" "2162da67ce86c514aff010de1b040fb26663ca42afebc2de26515d741121c435" "b1bcb837df0455af8e91114b7a3bddfa084cde32ceb16b1b468d5e5e8605a835" "930227e22122d1881db7c2c1ae712dcf715697a1c4d9864f8107a2c3c2da9f8b" "d5aac94c0051c3acec2b274347b343372b4e64c3e226be7b7c56725ea26b1ba8" "cb18233197cedab557c70d171b511bed49cc702f428750925280090c31498bd2" "1edf370d2840c0bf4c031a044f3f500731b41a3fd96b02e4c257522c7457882e" "91fba9a99f7b64390e1f56319c3dbbaed22de1b9676b3c73d935bf62277b799c" "d1a42ed39a15a843cccadf107ee0242b5f78bfbb5b70ba3ce19f3ea9fda8f52d" "b6db49cec08652adf1ff2341ce32c7303be313b0de38c621676122f255ee46db" "d69a0f6d860eeff5ca5f229d0373690782a99aee2410a3eed8a31332a7101f1e" "0bd7a42bd443517e5e61dac3cabc24018fbd0c6b2b4199b3c4efd9e3727efd30" "294834baa9ca874795a3181cce7aaf228b1e3fb3899587ffd3ae7546de328c90" "09669536b4a71f409e7e2fd56609cd7f0dff2850d4cbfb43916cc1843c463b80" "d43120398682953ef18fd7e11e69c94e44d39bb2ab450c4e64815311542acbff" "5424f18165ed7fd9c3ec8ea43d801dc9c71ab9da2b044000162a47c102ef09ea" "1dfd7a150e80fdb4563f594716d09d849f4c50bcea12825bd8d284c05a87a3e1" "b6d649c9f972b491686e7fa634535653e6222c1faca1ab71b3117854470a79ae" "3f873e7cb090efbdceafb8f54afed391899172dd917bb7a354737a8bb048bd71" "8704829d51ea058227662e33f84313d268b330364f6e1f31dc67671712143caf" "0b6645497e51d80eda1d337d6cabe31814d6c381e69491931a688836c16137ed" "7c1e99f9d46c397b3fd08c7fdd44fe47c4778ab69cc22c344f404204eb471baa" "36012edb5bc7070a17e989984e0ecc1d1e9c94326bdd0fbd76c2a45ebfe7da54" "479f188da96dcf244be270724c23de58607c031626bde8ba8243799f209d16b1" "0ae52e74c576120c6863403922ee00340a3bf3051615674c4b937f9c99b24535" "bcd39b639704f6f28ab61ad1ac8eb4625be77d027b4494059e8ada22ce281252" "a17f246690840fcf3fc26cb845ffedd2d8e1161cae386c14df61dabb9af3a5a9" "232f715279fc131ed4facf6a517b84d23dca145fcc0e09c5e0f90eb534e1680f" "a7b47876e5da7cac6f5e61cca7a040a365ca2c498823654bd4076add8edf34c5" "9e76732c9af8e423236ff8e37dd3b9bc37dacc256e42cc83810fb824eaa529b9" "1a2b131a7844bad234832963d565097efc88111b196fb75757885c159c5f8137" "a922c743710bb5d7c14995345549141f01211ff5089057dc718a5a33104c3fd1" "7e346cf2cb6a8324930c9f07ce050e9b7dfae5a315cd8ed3af6bcc94343f8402" "08dc5159473fa2250619880857eee06b7f4067f5f15b0ee8878c91f135cef6d5" "07840b49217157323d6ea4ccbdecc451b5989ebdc6e06cb0b4d742a141475a44" "46b20113556c07c1173d99edc6609473a106c13871da8fc9acb6534224f1e3e4" "8e0781b24291a7b29a411ba29ed01c8c2ee696c03c3dfdb3c3e89f8655db78ed" "6ae93caf30ad7eef728589a4d7b7befadecade71d78b904a64a0480608a7b61e" "b110da1a5934e91717b5c490709aba3c60eb4595194bbf9fdcbb97d247c70cfa" "1b4243872807cfad4804d7781c51d051dfcc143b244da56827071a9c2e10ab7f" "01c5ebefcabc983c907ee30e429225337d0b4556cc1d21df0330d337275facbb" "90b1aeef48eb5498b58f7085a54b5d2c9efef2bb98d71d85e77427ce37aec223" "3a3917dbcc6571ef3942c2bf4c4240f70b5c4bc0b28192be6d3f9acd83607a24" "b2028956188cf668e27a130c027e7f240c24c705c1517108b98a9645644711d9" "8ffaf449297bd9a08517f4b03a4df9dbf3e347652746cefceb3ee57c8e584b9f" "c55d8474e898e1231c49547d50e15d05c387e4111f4085f5fb7120a7418165c2" "3fb38c0c32f0b8ea93170be4d33631c607c60c709a546cb6199659e6308aedf7" "cdfb22711f64d0e665f40b2607879fcf2607764b2b70d672ddaa26d2da13049f" default)))
 '(powerline-default-separator (quote wave))
 '(powerline-evil-tag-style (quote verbose))
 '(rainbow-identifiers-choose-face-function (quote rainbow-identifiers-cie-l*a*b*-choose-face))
 '(rainbow-identifiers-cie-l*a*b*-saturation 50))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(powerline-evil-insert-face ((t (:inherit powerline-evil-base-face :background "deep sky blue"))))
 '(powerline-evil-normal-face ((t (:inherit powerline-evil-base-face :background "forest green")))))

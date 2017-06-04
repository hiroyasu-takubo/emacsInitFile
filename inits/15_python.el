;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;pythonの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; jediの設定
;; install jedi、virtualenvをインストールする必要がある。pipでやるのが簡単？
;; (jedi:setup)
;;   (define-key jedi-mode-map (kbd "<C-tab>") nil) ;;C-tabはウィンドウの移動に用いる
;;   (setq jedi:complete-on-dot t)
;;   (setq ac-sources
;;     (delete 'ac-source-words-in-same-mode-buffers ac-sources)) ;;jediの補完候補だけでいい
;;   (add-to-list 'ac-sources 'ac-source-filename)
;;   (add-to-list 'ac-sources 'ac-source-jedi-direct)
;;   (define-key python-mode-map "\C-ct" 'jedi:goto-definition)
;;   (define-key python-mode-map "\C-cb" 'jedi:goto-definition-pop-marker)
;;   (define-key python-mode-map "\C-cr" 'helm-jedi-related-names)
(require 'jedi)
;; M-TABで起動。
(add-hook 'python-mode-hook
          '(lambda()
             (jedi:ac-setup)
             (setq jedi:complete-on-dot t)
             (local-set-key (kbd "M-TAB") 'jedi:complete)))

;; autopep8の設定
;; install autopep8のインストールが必要
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=200"))
(setq flycheck-flake8-maximum-line-length 200)
(py-autopep8-enable-on-save)

;; ;; pyflakesの設定
;; 後で設定する。
;; ;; install pyflakesのインストールが必要
;; (flycheck-mode t)
;;     ;;errorやwarningを表示する
;;     (require 'flymake-python-pyflakes)
;; (flymake-python-pyflakes-load)

;; yasnippetの設定
;; install yasnippetのインストールが必要
(require 'yasnippet)
(yas-global-mode 1)


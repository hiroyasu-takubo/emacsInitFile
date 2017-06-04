;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;HASKELLの設定 設定は書きを参照 全然設定を行ってないので、後ですること。
;;http://d.hatena.ne.jp/kitokitoki/20111217/p1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;エラー？が出るのでいったん、コメントアウト、autoloadにしてみる。
;;(require 'haskell-mode)
;;(require 'haskell-cabal)
;;haskell-modeの設定
(autoload 'haskell-mode "haskell-mode" nil t)
(autoload 'haskell-cabal "haskell-cabal" nil t)


(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

;;ghciとの連携。
(setq haskell-program-name "/usr/bin/ghci")

(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))     ;#!/usr/bin/env runghc 用
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode)) ;#!/usr/bin/env runhaskell 用

;; ghc-mod
;; cabal でインストールしたライブラリのコマンドが格納されている bin ディレクトリへのパスを exec-path に追加する
;;cabalをwhichコマンドで探すと違うパスになる。そこを設定してみる。
;;(add-to-list 'exec-path (concat (getenv "HOME") "/.cabal/bin"))
(add-to-list 'exec-path (concat (getenv "HOME") "/Users/hiro/Library/Haskell/bin/cabal"))
;;(add-to-list 'exec-path (concat (getenv "HOME") "/Users/hiro/Library/Haskell/bin"))
;; ghc-flymake.el などがあるディレクトリ ghc-mod を ~/.emacs.d 以下で管理することにした

;;ロードパスが足りていない？
;;(add-to-list 'load-path "/usr/bin/ghc")
;;(add-to-list 'load-path "/usr/bin")
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

;;runhaskellでも正しく起動しない？コメントアウトしてみる。
;;(add-hook 'haskell-mode-hook
;;	  (lambda () (ghc-init)))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;ghc-browse-documentモードの設定
(require 'anything)
(require 'anything-config)
(require 'anything-match-plugin)

(defvar anything-c-source-ghc-mod
  '((name . "ghc-browse-document")
    (init . anything-c-source-ghc-mod)
    (candidates-in-buffer)
    (candidate-number-limit . 9999999)
    (action ("Open" . anything-c-source-ghc-mod-action))))

(defun anything-c-source-ghc-mod ()
  (unless (executable-find "ghc-mod")
   (error "ghc-mod を利用できません。ターミナルで which したり、*scratch* で exec-path を確認したりしましょう"))
  (let ((buffer (anything-candidate-buffer 'global)))
    (with-current-buffer buffer
      (call-process "ghc-mod" nil t t "list"))))

(defun anything-c-source-ghc-mod-action (candidate)
  (interactive "P")
  (let* ((pkg (ghc-resolve-package-name candidate)))
    (anything-aif (and pkg candidate)
        (ghc-display-document pkg it nil)
      (message "No document found"))))

(defun anything-ghc-browse-document ()
  (interactive)
  (anything anything-c-source-ghc-mod))

;; M-x anything-ghc-browse-document() に対応するキーの割り当て
;; ghc-mod の設定のあとに書いた方がよいかもしれません
(add-hook 'haskell-mode-hook
  (lambda()
    (define-key haskell-mode-map (kbd "C-M-d") 'anything-ghc-browse-document)))



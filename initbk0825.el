;;起動時の画面表示の設定
(if (eq window-system 'ns)
    (x-focus-frame nil))

;; load-path を追加する関数を定義
    (defun add-to-load-path (&rest paths)
      (let (path)
        (dolist (path paths paths)
          (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
    	(add-to-list 'load-path default-directory)
    	(if (fboundp 'normal-top-level-add-subdir-to-load-path)
    	    (normal-top-level-add-subdirs-to-load-path))))))

    ;; load-pathに追加
    (add-to-load-path "elisp" "conf" "public_repos")

    ;; auto-install.el 
    (when (require 'auto-install nil t)
      ;; インストールディレクトリを設定
      (setq auto-install-directory "~/.emacs.d/elisp")
      ;; EmacsWikiに登録されているelispの名前を取得
      (auto-install-update-emacswiki-package-name t)
      ;; install-elispの関数を利用可能にする
      (auto-install-compatibility-setup))
	    
    ;; emacsclientを使えるようにする
    (server-start)  ;; load-path を追加する関数を定義
    (defun add-to-load-path (&rest paths)
      (let (path)
        (dolist (path paths paths)
          (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
    	(add-to-list 'load-path default-directory)
    	(if (fboundp 'normal-top-level-add-subdir-to-load-path)
    	    (normal-top-level-add-subdirs-to-load-path))))))

    ;; load-pathに追加
    (add-to-load-path "elisp")

    ;; auto-install.el 
    (when (require 'auto-install nil t)
      ;; インストールディレクトリを設定
      (setq auto-install-directory "~/.emacs.d/elisp")
      ;; EmacsWikiに登録されているelispの名前を取得
      (auto-install-update-emacswiki-package-name t)
      ;; install-elispの関数を利用可能にする
      (auto-install-compatibility-setup))
	    
    ;; emacsclientを使えるようにする
    (server-start)

;;キーバインディング
(define-key global-map (kbd "C-m") 'newline-and-indent)
(define-key global-map (kbd "C-t") 'other-window)

;;環境変数の設定
(add-to-list 'exec-path "/out/local/bin")

;;文字コードの指定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;;Mac OS X の場合のファイル名の設定
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))


;;フレームに関する設定

;;行番号を表示
(column-number-mode t)

;;行番号を常に表示する
(global-linum-mode t)

;;TABの表示幅。
(setq-default tab-witdh 4)

;;インデントにタブ文字を使うか
;;(setq-default indent-tabs-mode nil)

;;color-theme関連
(when (require 'color-theme nil t)
  ;;テーマを読み込むための設定
  (color-theme-initialize)
  ;;テーマを変更
  (color-theme-hober))
 
;;ハイライト関連の設定
(defface my-hl-line-face
  ;;背景がdarkならば背景色を紺に
  '((((class color) (background dark))
    (:background "LightGoldenrodYellow" t))
   ;;背景がlightならば背景色を緑に
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;;対応する括弧のハイライト
(setq show-paren-delay 0);;表示までの秒数
(show-paren-mode t);;有効か
;;parenのスタイル expression は括弧内も強調表示
(setq show-paren-style 'expression)
;;フェイスを変更する
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "yellow")

;;elisp関連

;;インストール
;;(install-elisp "http://www.emacswiki.org/emacs/download/redo+.el")




;;redo*の設定
(when (require 'redo+ nil t)
  ;;C-'にredoを割り当てる
  (global-set-key (kbd "C-.") 'redo)
  ;;C-.がいい？
  )

;;package.elの設定
(when (require 'package nil t)
  ;;パッケージリポジトリにMarmarladeと開発者運絵のELPAを追加
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
	       '("ELPA" . "http://tromey.com./elpa/"))
  ;;インストールしたパッケージにロードパスを通して読み込む。
  (package-initialize))


;;エラーが出るので一時コメントアウト
;;ruby-modeの設定
;;インデント設定
(setq ruby-indent-level 3 ;;インデント幅を3に。初期値は2
      ruby-deep-indent-paren-style nil;;改行時のインデントを調整する。
      ;;ruby-mode実行時にindent-tabs-modeを設定値にへんこう
      ruby-indent-tabs-mode t);;タブ文字を使用する。

;;ruby blockの設定
;;エラーが出るためコメントアウト
;;(require 'ruby-electric nil t)
;;endに対応する行のハイライト
(when (require 'ruby-block nil t)
  (setq ruby-block-highlight-toggle t))
;;インタラクティブRubyを利用する
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

;;ruby-mode-hook用の関数を定義
(defun ruby-mode-hooks()
  (inf-ruby-keys)
  (ruby-electric-mode t)
  (ruby-block-mode t))
;;ruby-mode-hook
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)



;;HTMLの設定
;;エラーが出るので、後で;;(add-to-list ("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" .nxml-mode))


;;;;;;;;;;;;;;;
;;HASKELLの設定 設定は書きを参照 全然設定を行ってないので、後ですること。
;;http://d.hatena.ne.jp/kitokitoki/20111217/p1
;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/elisp/haskell-mode-2.8.0")
;;エラーが出るので先頭でghc用のロードパスを定義してみる。
(add-to-list 'load-path "/usr/bin")

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

;;elisp内にはghc-modがない。設定ミス？
;;(add-to-list 'load-path "~/.emacs.d/elisp/ghc-mod") 
(add-to-list 'load-path "/Users/hiro/Library/Haskell/bin/ghc-mod")

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




;;anythingの設定
;;(auto-install-batch "anything")
(when (require 'anything nil t)
  (setq
   ;;候補を表示するまでの時間
   anything-idle-delay 0.3
   ;;タイプして再表示するまでの時間
   anything-input-idle-delay 0.2
   ;;候補の最大表示数
   anything-candidate-number-limit 100
   ;;候補が多い時に体感速度を速くする
   anything-quick-update t
   ;;候補選択ショートカットをアルファベットに
   anything-enable-shortcuts 'alphabet)

  (when (require 'anything-config nil t)
    ;;root権限でアクションを実行するときのコマンド
    ;;デフォルトはsu
    (setq anything-su-or-sudo "sudo"))

  (require 'anything-match-plugin nil t)

  (when (and (executable-find "cmigemo")
	     (require 'migemo nit t))
    (require 'anything-migemo nil t))
  
  (when (require 'anything-complete nil t)
    ;;lispシンボルの保管候補の再検索時間
    (anything-lisp-complete-symbol-set-timer 150))

  (require 'anything-show-completion nil t)

  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))

  (when (require ' descbinds-anything nil t)
    ;;descri-bindingをAnythingに置き換える
    (descbinds-anything-install)))

;;smart compileの設定
(require 'smart-compile)
(define-key ruby-mode-map (kbd "C-c c") 'smart-compile)
(define-key ruby-mode-map (kbd "C-c C-c") (kbd "C-c c C-m") )


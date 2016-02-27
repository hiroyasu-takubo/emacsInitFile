;; emacsの設定

;; load-pathの設定

;;;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)	    
            (normal-top-level-add-subdirs-to-load-path))))))

;;;; load-pathに追加
(add-to-load-path "elisp" "conf" "public_repos" "elpa" "el-get")
(add-to-list 'load-path "~/.emacs.d/elisp/haskell-mode-2.8.0")
;;エラーが出るので先頭でghc用のロードパスを定義してみる。
(add-to-list 'load-path "/usr/bin")
;; (add-to-list 'load-path "~/.emacs.d/elisp/auto-complete-1.4")
;;elisp内にはghc-modがない。設定ミス？
;;(add-to-list 'load-path "~/.emacs.d/elisp/ghc-mod") 
(add-to-list 'load-path "/Users/hiro/Library/Haskell/bin/ghc-mod")
;; (add-to-list 'load-path "~/.emacs.d/elisp/neotree")
;; (add-to-list 'load-path "/elpa/ruby-end-0.3.1/ruby-end")

;;;;package.elの設定
(when (require 'package nil t)
  ;;パッケージリストにMarmaladeと開発者運営のELPAを追加
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives 
	       '("ELPA" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives
	       '("melpa"."http://melpa.org/packages/"))
;;インストールしたパッケージにロードパスを通して読み込む。
(package-initialize))

;;;; auto-install.el 
(when (require 'auto-install nil t)
  ;; インストールディレクトリを設定
  (setq auto-install-directory "~/.emacs.d/elisp")
  ;; EmacsWikiに登録されているelispの名前を取得
  (auto-install-update-emacswiki-package-name t)
  ;; install-elispの関数を利用可能にする
  (auto-install-compatibility-setup))


;;;;共通設定

;;;;バックアップを作らない。
(setq make-backup-files nil)
(setq auto-save-default nil)

;;;;矩形選択
;;古いらしいので、標準添付のcua-modeを使う。
;;(autoload 'sense-region-on "sense-region"
;;  "System to toggle region and rectangle" t nil)
;;(sense-reigion-on)
(cua-mode t)
(setq cua-enable-cua-keys nil)
(define-key global-map (kbd "C-x SPC") 'cua-set-rectangle-mark)


;;;;anythingの設定
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
	     (require 'migemo nil t))
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

;;;basic preference

;起動時の画面表示の設定
(if (eq window-system 'ns)
    (x-focus-frame nil))

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
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;色の設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;color-theme関連
(when (require 'color-theme nil t)
  ;;テーマを読み込むための設定
  (color-theme-initialize)
  ;;テーマを変更
  (color-theme-hober))
 
;;ハイライト関連の設定
;; 色の一覧　http://homepage1.nifty.com/blankspace/emacs/emacs_rgb.html
(defface my-hl-line-face
  ;;背景がdarkならば背景色を紺に
  '((((class color) (background dark))
    ;; (:background "LightGoldenrodYellow" t))
    (:background "SaddleBrown" t))
   ;;背景がlightならば背景色を緑に
    (((class color) (background light))
     (:background "SeaGreen" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)

;; 現在行に色をつける。
(global-hl-line-mode t)

;; ハイライトの色
(set-face-background 'hl-line "darkolivegreen")
 
;;対応する括弧のハイライト
(setq show-paren-delay 0);;表示までの秒数
(show-paren-mode t);;有効か
;;parenのスタイル expression は括弧内も強調表示
(setq show-paren-style 'expression)
;;色を変更する
(set-face-background 'show-paren-match-face nil)
(set-face-underline 'show-paren-match-face "yellow")
;;コメントの色
(set-face-foreground 'font-lock-comment-face "LimeGreen")
;; 背景色を変更する。
;; (set-background-color "white")
;; テーマの変更
;; (load-theme "adwaita" t)

;; 履歴を次回Emacs起動時にも保存する。
(savehist-mode t)

;;ファイル内のカーソル位置を記憶する。
(setq-default save-place t)
(require 'saveplace)

;; シェルに合わせて、C-hは後退に割り当てる。
(global-set-key (kbd "C-h") 'delete-backward-char)

;; モードラインに時刻を表示
(display-time)

;;行番号・桁番号を表示する。
(line-number-mode t)
(column-number-mode t)

;; リージョンに色を付ける。
(transient-mark-mode t)

;; Gcを減らして軽くする。
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;; ログの記録行数を増やす。
(setq message-log-max 10000)

;; ミニバッファを再帰的に呼び出せるように。
(setq enable-recursive-minibuffers t)

;; ダイアログボックスを使わないようにする。
(setq use-dialog-box nil)
(defalias 'message-box 'message)

;; 履歴をたくさんほzんする。
(setq history-length 1000)

;; キーストロークをエコーエリアに速く表示する。
(setq echo-keystorkes 0.1)


;; 大きいファイルを開こうとした時に警告を発生
(setq large-file-warning-threshold (* 25 1024 1024))

;; ミニヴァッファで入力を取り消しても履歴に残す。
(defadvice abort-recusive-edit (before minibuffe-save activate)
  (when (eq (selcted-window) (active-minibuffer-window))
    (add-to-history minibuffer-history-variable (minibuffer-contents))))
;;yesと入力するのではなく、y
(defalias 'yes-or-no-p 'y-or-n-p)

;; ツールバーとスクロールバーを消す。
;; 不便？　消さない。
;; (tool-bar-mode nil)
;; (scroll-bar-mode nil)

;;
;; Auto Complete
;;
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(ac-config-default)
(add-to-list 'ac-modes 'text-mode)         ;; text-modeでも自動的に有効にする
(add-to-list 'ac-modes 'fundamental-mode)  ;; fundamental-mode
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'yatex-mode)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)          ;; 曖昧マッチ


;;;neo-tree
;(add-to-list 'load-path "/somepaht")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;;;;smooth scroll
;; スクロール速度が遅いためコメントアウト。早く出来ない？
(require 'smooth-scroll)
(smooth-scroll-mode t)

;;;;文字入力の設定

;;;;ddskkの設定
(when (require 'skk nil t)
  (global-set-key (kbd "C-x j") 'skk-auto-fill-mode)
  (setq default-input-method "japanese-skk")
  (require 'skk-study))

;;;;stickyの設定
;; TODO どのキーにstickyを割り当てるか考える。
;; (require 'sticky)
;; (use-sticky-key ";" sticky-alist:ja)





;;;elisp関連

;;インストール
;;(install-elisp "http://www.emacswiki.org/emacs/download/redo+.el")

;; (require 'auto-async-byte-compile)
;; ;;自動バイトコンパイルを無効にするファイル名
;; (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
;; (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;;redo*の設定
(when (require 'redo+ nil t)
  ;;C-'にredoを割り当てる
  (global-set-key (kbd "C-.") 'redo)
  ;;C-.がいい？
  )
 
;;;;rubyの設定

;;;;ruby-modeの設定
;;;;インデント設定

(setq ruby-indent-level 2              ;;インデント幅を2に。
      ruby-deep-indent-paren-style nil ;;改行時のインデントを調整する。
      ruby-indent-tabs-mode nil)       ;;インデントにスペースを使用する。

;;;;ruby blockの設定
;;エラーが出るためコメントアウト ruby-endを使う。
;;https://raw.github.com.ruby/ruby/trunk/misc/ruby-electric.el
;;(require 'ruby-electric nil t)

;;;;ruby-end
(require 'ruby-end)

;;endに対応する行のハイライト
(when (require 'ruby-block nil t)
  (setq ruby-block-highlight-toggle t))

;;インタラクティブRubyを利用する
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

;;flymakeの設定
(require 'flymake)
(defun flymake-ruby-init ()
  (list "ruby" (list "-c" (flymake-init-create-temp-buffer-copy
			   'flymake-create-temp-inplace))))
(add-to-list ' flymake-allowed-file-name-masks
	     '("\\.rb\\'" flymake-ruby-init))
(add-to-list 'flymake-err-line-patterns
	     '("\\(.*\\):(\\([0-9]+\\)): \\(.*\\)" 1 2 nil 3))

;;;;ruby-mode-hook ruby-mode起動時に適用する
;;add-hookがうまく言っていない？
(add-hook 'ruby-mode-hook
	  '(lamda()
		 (inf-ruby-keys)
		 (ruby-block-mode t)
		 (ruby-end-mode)
		 (flymake-ruby)
		 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;HTMLの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;エラーが出るので、後で;;(add-to-list("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" .nxml-mode))

;;smart compileの設定
;; Shift + c　で大文字が打てないため、一時的にコメントアウト。来バーインドを変えればいけるはず。
;; (require 'smart-compile)
;; (define-key ruby-mode-map (kbd "C-c c") 'smart-compile)
;; ;;なぜかキーバインドで2二回実行するとエラーになる。キーバーインドC-c c C-mをやっても問題が無い。上の方を使えば問題ないが、気持ち悪い。
;; (define-key ruby-mode-map (kbd "C- C-c") (kbd "C-c c C-m") )

;;rhtmlの設定
;; パスも通ってるが、cannot open load fileと言われる。
;; load-fileを試してみると、rhtml-fontがないといわれる。
;; rhtml-fontでload-fileしてみると、うまく行くが、また試してみると今度はrhtml-sgmlphacksが無いと言われる。何がダメなのかさっぱりわからない。
;;ひとまずコメントアウト

;; (add-to-list 'load-path "~/.emacs.d/elisp/rhtml")
;; (require 'rhtml-mode)
;; (add-hook 'rhtml-mode-hook
;;     (lambda () (rinari-launch)))


;;;;;;;;;;;;;;;
;;HASKELLの設定 設定は書きを参照 全然設定を行ってないので、後ですること。
;;http://d.hatena.ne.jp/kitokitoki/20111217/p1
;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;
;;cssm-mode
;;;;;;;;;;;;;;;;;;;;;
(defun css-mode-hooks()
  "css-mode hooks"
  ;; indent c sytle
  (setq cssm-indent-function #'cssm-c-style-indenter)
  ;; double indent level
  (setq cssm-indent-level 2)
  ;; set indent tabs mode false
  (setq-default indent-tabs-mode nil)　
  ;; insert newline before bracket
  (setq cssm-new-line-before-closing-bracket t))

(add-hook 'css-mode-hook 'css-mode-hooks)

;; ;;;install when you install emacs to new machine
;; ;; show http://qiita.com/hmikisato/items/043355e1e2dd7ad8cd43
;; (require 'cl-lib)
;; ;; backup list for elpa and melpa
;; (defvar my/packages
;;   ;; add package you want to install automatically
;;   '(auto-install auto-complete))

;; (let ((not-installed
;;        (cl-loop for x in my/packages
;; 		when (not (package-installed-p x))
;; 		collect x)))
;;   (when not-installed
;;     (package-refresh-contents)
;;     (dolist (pkg not-installed)
;;       (package-install pkg))))

;; ;; el-get preference. install el-get when el-get is not installed, 
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;; (setq el-get-generate-autoloads t)
;; (unless (require 'el-get nil 'noerror)
;;   (url-retrieve
;;    "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el"
;;    (lambda (s)
;;      (goto-char (point-max))
;;      (eval-print-last-sexp))))

;; (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;; (el-get 'sync)

;; (defvar my/el-get-packages
;;   '(howm))
;; (el-get 'sync my/el-get-packages)

;;;;;
;; end of file
;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (misterioso))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

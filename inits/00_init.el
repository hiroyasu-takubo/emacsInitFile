;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacsの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load-pathの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; load-pathに追加
(add-to-load-path "elisp" "conf" "public_repos" "elpa" "el-get" "plugins")
(setq exec-path (cons (expand-file-name "~/.rbenv/shims") exec-path))
;;エラーが出るので先頭でghc用のロードパスを定義してみる。
(add-to-list 'load-path "/usr/bin")
;;elisp内にはghc-modがない。設定ミス？
(add-to-list 'load-path "/Users/hiro/Library/Haskell/bin/ghc-mod")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;package関連の設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; package.elの設定
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

;; auto-install.el 
(when (require 'auto-install nil t)
  ;; インストールディレクトリを設定
  (setq auto-install-directory "~/.emacs.d/elisp")
  ;; EmacsWikiに登録されているelispの名前を取得
  (auto-install-update-emacswiki-package-name t)
  ;; install-elispの関数を利用可能にする
  (auto-install-compatibility-setup))

;; el-get
(require 'el-get)
;; el-getでダウンロードしたパッケージは ~/.emacs.d/ に入るようにする
(setq el-get-dir (locate-user-emacs-file ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;anythingの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;他環境移行時の設定 今はエラーが出るため、コメントアウト
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;end of file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


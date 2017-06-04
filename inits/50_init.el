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


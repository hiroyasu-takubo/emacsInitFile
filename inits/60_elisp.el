;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;elisp関連の設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Auto Complete
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


;;neo-tree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;;smooth scroll
;; TODO スクロール速度が遅いためコメントアウト。早く出来ない？
(require 'smooth-scroll)
(smooth-scroll-mode t)
;; 縦方向のスクロール行数を変更する。
(setq smooth-scroll/vscroll-step-size 4)
;; 横方向のスクロール行数を変更する。
(setq smooth-scroll/hscroll-step-size 4)

;; ;;自動バイトコンパイルを無効にするファイル名
(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;;redo*の設定
(when (require 'redo+ nil t)
  ;;C-'にredoを割り当てる
  (global-set-key (kbd "C-.") 'redo)
  ;;C-.がいい？
  )

;; guide-keyの設定
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
; Enable guide-key-mode
(guide-key-mode 1)

;; helmの設定
(require 'helm-config)
(helm-mode t)

;; helm-describesの設定
(require 'helm-descbinds)
(helm-descbinds-mode)

;; helmのデフォルトコマンド設定からの変更
;; C-hはバッファの1文字前に削除をしたい。
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)

;; helmコマンドのキーバーインド設定
(define-key global-map (kbd "M-x")   'helm-M-x)
(define-key global-map (kbd "C-x b")   'helm-buffers-list)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
;; ファイルを探し中にTABで補完するようにする。元はhelm-select-actionが割り当てられている。TABはhelmでよく使うキーらしいので、他の補完方法を考える。helmのチュートリアルを読んでみる。
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

;; grep-editの設定
(require 'grep-edit)

;; magitの設定


;; whitespace スペースの可視化
(require 'whitespace)
;; (global-whitespace-mode t)
(setq-default show-trailing-whitespace t)
(setq whitespace-style '(face           ; faceで可視化
                         ;; trailing       ; 行末
                         tabs           ; タブ
;;                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))

(setq whitespace-display-mappings
      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

;; (whitespace-newline-mode t)

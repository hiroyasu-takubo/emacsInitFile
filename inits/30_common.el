;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;共通設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;バックアップを作らない。
(setq make-backup-files nil)
(setq auto-save-default nil)

;;矩形選択
;;古いらしいので、標準添付のcua-modeを使う。
;;(autoload 'sense-region-on "sense-region"
;;  "System to toggle region and rectangle" t nil)
;;(sense-reigion-on)
(cua-mode t)
(setq cua-enable-cua-keys nil)
(define-key global-map (kbd "C-x SPC") 'cua-set-rectangle-mark)

;起動時の画面表示の設定
(if (eq window-system 'ns)
    (x-focus-frame nil))

;; emacsclientを使えるようにする
(server-start)

;;キーバインディング
(define-key global-map (kbd "C-m") 'newline-and-indent)
(define-key global-map (kbd "C-t") 'other-window)
;; C-zをやると落ちるので、無効にする
(define-key global-map (kbd "C-z") nil)

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

;; 履歴を次回Emacs起動時にも保存する。
(savehist-mode t)

;;ファイル内のカーソル位置を記憶する。
(setq-default save-place t)
(require 'saveplace)

;; シェルに合わせて、C-hは後退に割り当てる。
;; helmに対応するためコメントアウト どうするか考え中
(global-set-key (kbd "C-h") 'delete-backward-char)

;; モードラインに時刻を表示
(display-time)

;;行番号・桁番号を表示する。
(line-number-mode t)
(column-number-mode t)

;; リージョンに色を付ける。
;; 色の設定に移動するか？
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

;; 履歴をたくさん保存する。
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

;;音を鳴らさない
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; ファイルのリロードを行う。
(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  ;;(message "force-reverting value is %s" force-reverting)
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))
(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  ;;(message "force-reverting value is %s" force-reverting)
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))
(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  ;;(message "force-reverting value is %s" force-reverting)
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))
(global-set-key "\M-r" 'revert-buffer-no-confirm)

;; exec-path-from-shellの設定
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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
(set-face-background  'hl-line "darkolivegreen")
 
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


;; ツールバーとスクロールバーを消す。
;; 不便？　消さない。
;; (tool-bar-mode nil)
;; (scroll-bar-mode nil)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-search-threshold 4000)
 '(custom-enabled-themes (quote (misterioso)))
 '(package-selected-packages
   (quote
    (init-loader php-completion php-mode exec-path-from-shell evil-iedit-state py-autopep8 jedi twittering-mode sticky smooth-scroll sml-modeline scss-mode ruby-end rubocop robe magit jump helm-descbinds helm-R guide-key flycheck-color-mode-line el-get dummy-package ddskk auto-install auto-complete anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )





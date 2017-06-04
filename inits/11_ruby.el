;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;rubyの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;ruby-modeの設定

;;インデント設定
(setq ruby-indent-level 2              ;;インデント幅を2に。
      ruby-deep-indent-paren-style nil ;;改行時のインデントを調整する。
      ruby-indent-tabs-mode nil)       ;;インデントにスペースを使用する。

;;endに対応する行のハイライト
(when (require 'ruby-block nil t)
  (setq ruby-block-highlight-toggle t))

;;ruby-endの設定
(require 'ruby-end)

;;インタラクティブRubyを利用する
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;;smart compileの設定
;; これで “C-c C-c”で、編集中の ruby ファイルを実行できます。”C-c c”の方はミニバッファに “ruby xxx.rb”まで入力された状態になるので、こちらは引数など与えたいときに。
;; 無限ループなどで止まってくれないruby実行中のバッファを殺すにはM-x kill-comilation
;; Shift + c　で大文字が打てないため、一時的にコメントアウト。来バーインドを変えればいけるはず。
(require 'smart-compile)
(define-key ruby-mode-map (kbd "C-c c") 'smart-compile)
;;なぜかキーバインドで2二回実行するとエラーになる。キーバーインドC-c c C-mをやっても問題が無い。上の方を使えば問題ないが、気持ち悪い。
(define-key ruby-mode-map (kbd "C-c C-c") (kbd "C-c c C-m") )

;; rcodetoolsの設定を行う。
;; install rcodetoolsのインストールが必要
;; rubyコードの行末に # = >と入れてxmpfilterを実行すると、その行の返り値を見る事が可能。
;; rct-completeを使うと、コードの自動補完が使える。
;; ruby-toggle-bufferはテストコードとソースコードを切り替える事が出来る。
;; 説明はgithubの説明を参照 https://github.com/tnoda/rcodetools-x
(require 'rcodetools)
(setq rct-find-tag-if-available nil)
(defun ruby-mode-hook-rcodetools ()
  (define-key ruby-mode-map "\M-\C-i" 'rct-complete-symbol)
  (define-key ruby-mode-map "\C-c\C-t" 'ruby-toggle-buffer)
  (define-key ruby-mode-map "\C-c\C-f" 'rct-ri))

;; rdefsの設定を行う。
;; install rdefsのインストールが必要
;; TODO 設定の調査が必要

(require 'anzu)
(global-anzu-mode +1)

(set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)


;; robeの設定
;; Install pryのインストールが必要
;; 次のコマンドで起動 M-x inf-ruby → M-x robe-start
;; 
(autoload 'robe-mode "robe" "Code navigation, documentation lookup and completion for Ruby" t nil)
(autoload 'ac-robe-setup "ac-robe" "auto-complete robe" nil nil)

;; robocopの設定
;; Install rubocopの設定が必要。
(require 'rubocop)

;; flycheck
(require 'flycheck)
(setq flycheck-check-syntax-automatically '(mode-enabled save))

;; flycheck-color-mode-line flycheckをカラフルにする。
(require 'flycheck-color-mode-line)

;; rinariの設定
;; (require 'rinari)

;; rhtml-mode
(require 'rhtml-mode)

;; electric-pair-mode
;; TODO ミニバッファでエラーがでるようになるためコメントアウト
;; (add-to-list 'electric-pair-pairs '(?| . ?|))
;; (setq electric-pair-pairs '(?| . ?|))

;;ruby-mode-hook ruby-mode起動時に適用する
;;add-hookがうまく言っていない？

(defun ruby-mode-hooks ()
  (ruby-block-mode t)
  (inf-ruby-minor-mode)
  (setq flycheck-checker 'ruby-rubocop)
  (flycheck-mode)
  (robe-mode)
  (ac-robe-setup)
  ;; (electric-pair-mode t)
  )
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)

;; (defun rhtml-mode-hooks ()
;;   (rinari-launch)
;; )
;; (add-hook 'rhtml-mode-hook 'rhtml-mode-hooks)

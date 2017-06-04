;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;HTMLの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;エラーが出るので、後で;;(add-to-list("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" .nxml-mode))

;;rhtmlの設定
;; パスも通ってるが、cannot open load fileと言われる。
;; load-fileを試してみると、rhtml-fontがないといわれる。
;; rhtml-fontでload-fileしてみると、うまく行くが、また試してみると今度はrhtml-sgmlphacksが無いと言われる。何がダメなのかさっぱりわからない。
;;ひとまずコメントアウト

;; (add-to-list 'load-path "~/.emacs.d/elisp/rhtml")
;; (require 'rhtml-mode)
;; (add-hook 'rhtml-mode-hook
;;     (lambda () (rinari-launch)))



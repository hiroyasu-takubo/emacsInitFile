;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;cssm-modeの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun css-mode-hooks()
  "css-mode hooks"
  ;; indent c sytle
  (setq cssm-indent-function #'cssm-c-style-indenter)
  ;; double indent level
  (setq cssm-indent-level 2)
  ;; set indent tabs mode false
  (setq-default indent-tabs-mode nil)　
  ;; insert newline before bracket
  (setq cssm-new-line-before-closing-bracket t)
  )

(add-hook 'css-mode-hook 'css-mode-hooks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;scss-modeの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))


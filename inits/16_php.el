;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;php関連の設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'php-mode)
(add-hook 'php-mode-hook
      (lambda ()
        (require 'php-completion)
        (php-completion-mode t)
        (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)
        (make-local-variable 'ac-sources)
        (setq ac-sources '(
                   ac-source-words-in-same-mode-buffers
                   ac-source-php-completion
                   ac-source-filename
                   ))
        (setq tab-width 2)
        (setq indent-tabs-mode t)
        (setq c-basic-offset 2)
      )
)



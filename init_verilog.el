;; Verilog mode disable auto formatting                                   

(defun fix-verilog-indent ()
    "Fix stupid verilog indenting"
    (interactive)
    (setq-local indent-line-function 'indent-relative))

(add-hook 'verilog-mode-hook
          (lambda ()
            (progn
              ;; This screws up indentation in other buffers
              ;; (add-hook 'hack-local-variables-hook
              ;;           (lambda ()
              ;;             (setq indent-line-function 'indent-relative)))
              (setq verilog-auto-lineup 'ignore)
              (setq auto-indent-on-newline nil)
              (setq verilog-auto-newline nil))))

              

(eval-after-load 'verilog-mode
    '(progn
        ;; same for all the electric-verilog-* commands in                
        ;; the mode's map (see verilog-mode.el)                      
        (define-key verilog-mode-map (kbd ";") 'self-insert-command)
        (define-key verilog-mode-map (kbd ":") 'self-insert-command)
        (define-key verilog-mode-map (kbd "RET") 'newline)))

;;; nircmd --- Misc. system commands interface to MS-Windows -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defsubst nircmd--execute (&rest cmd)
  (if (zerop (apply #'call-process "nircmd" nil nil nil cmd))
      t
    (error "[nircmd] %s failed (params: %s)" (car cmd) (cdr cmd))))

;;;###autoload
(defun nircmd-activate-process (pid)
  (nircmd--execute "win" "activate" "process" (format "/%d" pid)))

;;;###autoload
(defun nircmd-send-key (pid key)
  "Send input KEY to the process PID."
  (nircmd-activate-process pid)
  (nircmd--execute "sendkeypress" key))

(provide 'nircmd)
;;; nircmd.el ends here

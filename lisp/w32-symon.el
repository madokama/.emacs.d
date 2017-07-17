;;; w32-symon --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 's)
(require 'subr-x)
(require 'symon)

(defvar w32-symon-status nil)
(defvar w32-symon-process)

(defvar w32-symon-pagefile-max nil)
(defvar w32-symon-memory-max nil)

(defvar w32-symon-monitors
  `((cpu "\\Processor(_Total)\\% Processor Time")
    (disk "\\PhysicalDisk(0 C:)\\% Disk Time"
          :proc ,(apply-partially #'min 100.0))
    (swap "\\Paging File(_Total)\\% Usage"
          :init
          ,(lambda ()
             (w32-symon-wmic "path Win32_PageFileUsage get AllocatedBaseSize"
               (lambda (value)
                 (setq w32-symon-pagefile-max value))))
          :proc
          ,(lambda (datum)
             (when w32-symon-pagefile-max
               (* (/ datum 100) w32-symon-pagefile-max))))
    (memory "\\Memory\\Available bytes"
            :init
            ,(lambda ()
               (w32-symon-wmic "memorychip get capacity"
                 (lambda (value)
                   (setq w32-symon-memory-max value))))
            :proc
            ,(lambda (datum)
               (when w32-symon-memory-max
                 (* 100
                    (- 1 (/ datum w32-symon-memory-max))))))))

(defun w32-symon-get-status (object &optional callback)
  (let ((cell (alist-get object w32-symon-status)))
    (if callback
        (when cell (funcall callback cell))
      cell)))

(defun w32-symon-commit-status (object value)
  (when value
    (let ((cell (w32-symon-get-status object)))
      (unless cell
        (setq cell (vector (symon--make-history-ring) nil))
        (push (cons object cell) w32-symon-status))
      (ring-insert (aref cell 0) value))))

(defun w32-symon-status-history (object)
  "Get a list of recent status values for OBJECT."
  (w32-symon-get-status object
                        (lambda (cell)
                          (ring-elements (aref cell 0)))))

(defun w32-symon-wmic (params callback)
  (declare (indent 1))
  (make-process :name "wmic"
                :command (cons "wmic" (split-string params))
                :noquery t
                :filter
                (lambda (_ data)
                  (when (string-match "^[0-9]+" data)
                    (funcall callback (string-to-number (match-string 0 data)))))))

;; (defun w32-symon-wmic (params callback)
;;   (declare (indent 1))
;;   (set-process-filter
;;    (start-process-shell-command "wmic" nil (concat "wmic " params))
;;    (lambda (_ data)
;;      (when (string-match "^[0-9]+" data)
;;        (funcall callback (string-to-number (match-string 0 data)))))))

(defun w32-symon-update ()
  (when (eq t (frame-visible-p (selected-frame)))
    (mapc (lambda (object)
            (w32-symon-get-status object
                                  (lambda (cell)
                                    (aset cell 1
                                          (symon--make-sparkline
                                           (w32-symon-status-history object))))))
          (mapcar #'car w32-symon-monitors))
    (force-mode-line-update)))

(defun powerline-symon (object &optional face)
  (w32-symon-get-status object
                        (lambda (cell)
                          (append (aref cell 1) `(:face ,face)))))

(defun w32-symon-process-data (data)
  (cl-loop for object in w32-symon-monitors
           for datum in (mapcar (lambda (m)
                                  (string-to-number (cadr m)))
                                (s-match-strings-all ",\\\"\\([0-9.]+\\)\\\""
                                                     data))
           do (w32-symon-commit-status
               (car object)
               (funcall (or (plist-get object :proc) #'identity)
                        datum))))

(defun w32-symon-cleanup ()
  (kill-process w32-symon-process)
  (setq w32-symon-status nil)
  (setq w32-symon-process nil))

(define-minor-mode w32-symon-mode
  "MS Windows system monitor suited for powerline integration"
  :init-value nil
  :global t
  (if w32-symon-mode
      (progn
        (mapc (lambda (object)
                (when-let* (init (plist-get object :init))
                  (funcall init)))
              w32-symon-monitors)
        (setq w32-symon-process
              (make-process :name "typeperf"
                            :command
                            `("typeperf" ,@(mapcar #'cadr w32-symon-monitors)
                                         "-si"
                                         ,(number-to-string symon-refresh-rate))
                            :noquery t
                            :filter
                            (lambda (_ data)
                              (dolist (line (split-string data "\n" t))
                                (w32-symon-process-data line))
                              (w32-symon-update)))))
    (w32-symon-cleanup)))

(provide 'w32-symon)
;;; w32-symon.el ends here

;;; mpv-ipc --- mpv controller library -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-macs))
(require 'subr-x)

(defun mpv-ipc--sockpath ()
  (if (eq system-type 'windows-nt)
      (format "\\\\.\\pipe\\%s" (make-temp-name "mpvsock"))
    (concat "/tmp/" (make-temp-name "mpvsock"))))

(defun mpv-ipc--sockcmd (path)
  ;; Use pipecat on Windows:
  ;; https://gist.github.com/rossy/f7c54976125abd67fc63/raw/c12e636d56d1394990cc18776f83bef918f6a639/pipecat.c
  (if (eq system-type 'windows-nt)
      (list "pipecat" path)
    (list "socat" "-" path)))

(defun mpv-ipc-connect (path cb)
  (let ((sock
         (make-process :name "mpv-ipc"
                       :command (mpv-ipc--sockcmd path)
                       :coding 'utf-8
                       :noquery t
                       :connection-type 'pipe
                       :filter
                       (lambda (_proc output)
                         (with-temp-buffer
                           (insert (string-trim-right output))
                           (goto-char (point-min))
                           (cl-loop until (eobp)
                                    do (funcall cb path (json-parse-buffer))))))))
    (process-put sock :sock path)
    sock))

(defun mpv-ipc-watch (args &optional ev-handler)
  (let ((path (mpv-ipc--sockpath))
        (cb (mpv-ipc-receive (or ev-handler #'ignore)))
        (w32-pipe-read-delay 50))
    (cl-block nil
      ;; Returns socket process if successful.
      (accept-process-output
       (make-process :name "mpv"
                     :command
                     `("mpv" ,(format "--input-ipc-server=%s" path)
                             "--msg-level=ipc=v"
                             "--idle=yes"
                             ,@args)
                     :filter
                     (lambda (proc output)
                       (when (string-match-p "Listening" output)
                         ;; IPC server initiated.
                         (let ((sock (mpv-ipc-connect path cb)))
                           (set-process-sentinel
                            proc
                            (lambda (_proc _signal)
                              (delete-process sock)))
                           (cl-return sock)))))
       10))))

(defun mpv-ipc-receive (ev-handler)
  (lambda (tag json)
    (let-hash json
      (cond (.event
             (funcall ev-handler json))
            (.request_id
             (throw tag .data))))))

(defun mpv-ipc-inputcmd (cmd &rest params)
  (format "%s%s\n"
          cmd
          (if params
              (concat " " (mapconcat (lambda (param)
                                       (format "%s" param))
                                     params " "))
            "")))

(defun mpv-ipc-jsoncmd (tag cmd &rest params)
  (format "{\"command\":[%S%s],\"request_id\":%S}\n"
          cmd
          (if params
              (concat "," (mapconcat (lambda (param)
                                       (format "%S" param))
                                     params ","))
            "")
          tag))

(defun mpv-ipc--do (ipc &rest cmd)
  (when (process-live-p ipc)
    (process-send-string ipc (apply #'mpv-ipc-inputcmd cmd))))

(defun mpv-ipc--query (ipc &rest cmd)
  (when (process-live-p ipc)
    (when-let ((tag (process-get ipc :sock)))
      (catch tag
        (process-send-string ipc
                             (apply #'mpv-ipc-jsoncmd tag cmd))
        (accept-process-output ipc 1)))))

(defun mpv-ipc--getprop (ipc prop)
  (mpv-ipc--query ipc "get_property" prop))

(defun mpv-ipc:get-path (ipc)
  (mpv-ipc--getprop ipc "path"))

(defun mpv-ipc:request-log (ipc &optional param)
  (mpv-ipc--query ipc "request_log_messages" (or param "info")))

(defun mpv-ipc--playlist (lst)
  (let ((pl (concat (make-temp-file "mpv") ".m3u")))
    (with-temp-file pl
      (cl-loop for file in lst
               do (insert file "\n")))
    pl))

(defun mpv-ipc:load (ipc lst)
  (mpv-ipc--do ipc "loadlist" (mpv-ipc--playlist lst) "replace"))

(defun mpv-ipc:pause (ipc)
  (mpv-ipc--do ipc "cycle" "pause"))

(defun mpv-ipc:stop (ipc)
  (mpv-ipc--do ipc "stop"))

(defun mpv-ipc:quit (ipc)
  (mpv-ipc--do ipc "quit"))

(defun mpv-ipc:volume (ipc amount)
  (mpv-ipc--do ipc "add" "volume" amount))

(defun mpv-ipc:mute (ipc)
  (mpv-ipc--do ipc "cycle" "mute"))

(defun mpv-ipc:keypress (ipc key)
  (mpv-ipc--do ipc "keypress" key))

(defun mpv-ipc:next (ipc)
  (mpv-ipc--do ipc "playlist-next" "weak"))

(defun mpv-ipc:prev (ipc)
  (mpv-ipc--do ipc "playlist-prev" "weak"))

(defun mpv-ipc:loop (ipc)
  (mpv-ipc--do ipc "cycle-values" "loop-file" "inf" "no"))

(defun mpv-ipc-interact (ipc)
  (cl-block nil
    (while t
      (if-let ((char (read-char "mpv>")))
          (progn
            (mpv-ipc:keypress ipc (string char))
            (when (memq char '(?q ?Q ?))
              (cl-return)))
        (cl-return)))))

(provide 'mpv-ipc)
;;; mpv-ipc.el ends here

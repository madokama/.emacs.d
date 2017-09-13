;;; emoz --- Emacs interface for Firefox -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'seq)
(require 'moz)
;; (require 'eww)

(defvar emoz--debug)

(defvar emoz-monitor-function nil)

(defvar emoz--output nil
  "Variable to keep track of REPL output.
Used to collect a single output which, if large, may come in
multiple batches.")

(defvar emoz-mutex (make-mutex "emoz"))

(defun emoz-add-monitor ()
  (add-hook 'comint-output-filter-functions #'emoz-monitor t t))

;; (defun emoz-remove-monitor ()
;;   (remove-hook 'comint-output-filter-functions #'emoz-monitor t))

;; (defsubst emoz-unquote-output (output)
;;   (replace-regexp-in-string (rx (or (and bos "\"") (and "\"" eos)))
;;                             "" output))



(defun emoz-flush-output ()
  (mapc #'insert (nreverse emoz--output))
  (setq emoz--output nil))

(defun emoz-monitor (output)
  (setq emoz--output (cons output emoz--output))
  (when (string-match-p (rx bol "repl> " eos) output)
    (when emoz-monitor-function
      (funcall emoz-monitor-function
               (save-match-data
                 (with-temp-buffer
                   (emoz-flush-output)
                   ;; Remove prompt at the end.
                   (when (re-search-backward (rx (opt "\n") "repl> " buffer-end)
                                             nil t)
                     (delete-region (point) (point-max)))
                   ;; Remove junk at the top.
                   (when (re-search-backward
                          (rx line-start (1+ (or "....> " "repl> ")))
                          nil t)
                     (delete-region (point-min) (match-end 0)))
                   (buffer-substring-no-properties (point-min) (point-max))))))))

(defun emoz-query (code &optional callback)
  (mutex-lock emoz-mutex)
  (if-let* ((proc
             (condition-case nil
                 (save-window-excursion
                   (inferior-moz-process))
               (error nil))))
      (progn
        (setq emoz-monitor-function
              (lambda (output)
                (setq emoz--debug output)
                (unwind-protect
                     (when callback
                       (funcall callback output))
                  (setq emoz-monitor-function nil)
                  (comint-delete-output)
                  (mutex-unlock emoz-mutex))))
        (process-send-string proc code))
    (mutex-unlock emoz-mutex)))

;; (defun emoz-capture ()
;;   (emoz-query "content.document.body.innerText"
;;                  (lambda (output)
;;                    (message "[MOZ]%s"
;;                             ))))

(defun emoz--wrap-js (code)
  (format "(function() { %s })()" code))



;;; Applications

;; (defun emoz-send-to-devices (url)
;;   (emoz-query
;;    (emoz--wrap-js (format "let engine = Weave.Service.clientsEngine;
;;     for each (client in engine._store._remoteClients) {
;;         engine.sendURIToClientForDisplay(%S, client.id);
;;     }
;;     engine.sync();"
;;                          url))))

(defun emoz-current-url (callback)
  (emoz-query
   (emoz--wrap-js "let wm = Cc['@mozilla.org/appshell/window-mediator;1']
    .getService(Ci.nsIWindowMediator);
let recentWindow = wm.getMostRecentWindow('navigator:browser');
repl.print(recentWindow.gBrowser.currentURI.spec);")
   callback))

(declare-function url-host "url-parse")
(declare-function url-generic-parse-url "url-parse")

;; https://developer.mozilla.org/en-US/Add-ons/Code_snippets/Cookies
;; https://curl.haxx.se/docs/

;; sample:
;; # Netscape HTTP Cookie File
;; # https://curl.haxx.se/docs/http-cookies.html
;; # This file was generated by libcurl! Edit at your own risk.
;; .sites.google.com	TRUE	/site/mymemomemo	FALSE	1484548453	__utma	80043285.2103408779.1421476402.1421476402.1421476402.1
;; .support.google.com	TRUE	/	FALSE	1484983738	__utma	93394463.1460244844.1421911738.1421911738.1421911738.1
;; .productforums.google.com	TRUE	/	FALSE	1485341405	__utma	96725982.2117230450.1422269403.1422269403.1422269403.1
;; #HttpOnly_.github.com	TRUE	/	TRUE	2107981751	logged_in	no

(defvar url-cookie-file)
(declare-function url-do-setup "url")

;;;###autoload
(defun emoz-sync-cookies ()
  "Sync cookies with firefox."
  (interactive)
  (emoz-query
   (emoz--wrap-js "let truth = function(p) { return p ? 'TRUE' : 'FALSE' };
let iter = Services.cookies.enumerator;
while (iter.hasMoreElements()) {
    let c = iter.getNext().QueryInterface(Ci.nsICookie2);
    repl.print(`${c.host}\t${truth(c.isDomain)}\t${c.path}\t${truth(c.isSecure)}\t${c.expires}\t${c.name}\t${c.value}`);
}")
   (lambda (output)
     (url-do-setup)
     (let ((tmp (make-temp-file "cookie"))
           (coding-system-for-write
            (if (eq system-type 'windows-nt) 'utf-8-dos 'utf-8)))
       (with-temp-file tmp
         (insert "# Netscape HTTP Cookie File\n"
                 output))
       (rename-file tmp (concat url-cookie-file ".curl") t)
       (message "Cookies synced.")))))

(defun emoz-tabs (callback)
  (emoz-query
   (emoz--wrap-js "let browserEnum = Cc['@mozilla.org/appshell/window-mediator;1']
    .getService(Ci.nsIWindowMediator)
    .getEnumerator('navigator:browser');
let escape = function(str) { return str.replace(/[\\\\\"]/g, '\\\\$&') };
repl.print('(');
while (browserEnum.hasMoreElements()) {
    let tabbrowser = browserEnum.getNext().gBrowser;
    for (let tab of tabbrowser.mTabContainer.childNodes) {
        repl.print('(');
        repl.print(`:accessed ${tab.lastAccessed}`);
        repl.print(`:title \"${escape(tab.label)}\"`);
        repl.print(`:url \"${tabbrowser.getBrowserForTab(tab).currentURI.spec}\"`);
        repl.print(')');
    }
}
let tabsEngine = Weave.Service.engineManager.get('tabs');
for each (let client in tabsEngine.getAllClients()) {
    for (let tab of client.tabs) {
        repl.print('(');
        repl.print(`:accessed ${tab.lastUsed * 1000}`);
        repl.print(`:title \"${escape(tab.title)}\"`);
        repl.print(`:url \"${tab.urlHistory[0]}\"`);
        repl.print(')');
    }
}
repl.print(')');")
   (lambda (output)
     (funcall callback
              (cl-delete-duplicates
               (seq-sort-by (lambda (plst)
                              (plist-get plst :accessed))
                            #'>
                            (car (read-from-string output)))
               :test (lambda (a b)
                       (string= (plist-get a :url)
                                (plist-get b :url)))
               :from-end t)))))

(add-hook 'inferior-moz-mode-hook #'emoz-add-monitor)

(provide 'emoz)
;;; emoz.el ends here

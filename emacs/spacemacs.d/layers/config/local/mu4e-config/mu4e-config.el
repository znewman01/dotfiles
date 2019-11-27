(require 'mu4e)
(provide 'mu4e-config)

(require 'evil-collection)
(evil-collection-init 'mu4e)

; TODO: add imapfilter
; TODO: http://pragmaticemacs.com/emacs/using-postfix-instead-of-smtpmail-to-send-email-in-mu4e/

(setq mu4e-maildir "~/Maildir")

; Speed up indexing. Run "mu index" every once in a while to reindex.
(setq
  mu4e-index-cleanup t      ;; don't do a full cleanup check
  mu4e-index-lazy-check nil)

(defmacro zjn--make-match (folder)
  `(lambda (msg)
     (when msg
       (string-prefix-p ,(concat "/" folder)
                        (mu4e-message-field msg :maildir)))))
(setq message-send-mail-function 'smtpmail-send-it)
(setq mu4e-contexts
      `(
        ,(make-mu4e-context
          :name "Fastmail"
          :match-func (zjn--make-match "fastmail")
          :vars '((mu4e-trash-folder . "/fastmail/Trash")
                  (mu4e-sent-folder . "/fastmail/Sent")
                  (mu4e-drafts-folder . "/fastmail/Drafts")
                  (mu4e-refile-folder . "/fastmail/Archive")
                  (user-mail-address . "z@znewman.net")
                  (user-full-name . "Zachary Newman")
                  (smtpmail-local-domain . "znewman.net")
                  (smtpmail-smtp-server . "smtp.fastmail.com")
                  (smtpmail-stream-type . ssl)
                  (smtpmail-smtp-service . 465)))
        ,(make-mu4e-context
           :name "MIT"
           :match-func (zjn--make-match "mit")
           :vars '((mu4e-trash-folder . "/mit/Deleted")
                   (mu4e-sent-folder . "/mit/Sent")
                   (mu4e-drafts-folder . "/mit/Drafts")
                   (mu4e-refile-folder . "/mit/Archive")
                   (user-mail-address . "zjn@mit.edu")
                   (user-full-name . "Zachary Newman")
                   (smtpmail-local-domain . "mit.edu")
                   (smtpmail-smtp-server . "outgoing.mit.edu")
                   (smtpmail-stream-type . ssl)
                   (smtpmail-smtp-service . 465)))
        ,(make-mu4e-context
           :name "Gmail"
           :match-func (zjn--make-match "gmail")
           :vars '((mu4e-trash-folder . "/gmail/[Gmail]/Trash")
                   (mu4e-sent-folder . "/gmail/[Gmail]/SentMail")
                   (mu4e-drafts-folder . "/gmail/[Gmail]/Drafts")
                   (mu4e-refile-folder . "/gmail/[Gmail]/AllMail")
                   (user-mail-address . "znewman01@gmail.com")
                   (user-full-name . "Zachary Newman")
                   (smtpmail-local-domain . "gmail.com")
                   (smtpmail-smtp-server . "smtp.gmail.com")
                   (smtpmail-stream-type . starttls)
                   (smtpmail-smtp-service . 587)))))

; Don't set the Maildir "T" flag when trashing things
; Per http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
(setf
 (alist-get 'trash mu4e-marks)
 (plist-put (cdr (assq 'trash mu4e-marks))
            :action
            (lambda (docid msg target)
              (mu4e~proc-move docid (mu4e~mark-check-target target) "-N"))))

(setq mu4e-get-mail-command "mbsync -a")

(defun zjn--get-mu4e-vars (var)
  (mapcar (lambda (context)
            (alist-get var (mu4e-context-vars context)))
          mu4e-contexts))
(defun zjn--join (seq sep) (mapconcat #'identity seq sep))
(let* ((trash-folders (zjn--get-mu4e-vars 'mu4e-trash-folder))
       (sent-folders (zjn--get-mu4e-vars 'mu4e-sent-folder))
       (skip-trash-and-sent
        (lambda (query) (zjn--join (cons query
                                         (mapcar (apply-partially #'concat "NOT maildir:")
                                                 (append trash-folders sent-folders '("/mit/Junk" "/gmail/[Gmail]/Spam"))))
                               " AND "))))
  (setq mu4e-bookmarks
        (mapcar (apply-partially #'apply #'make-mu4e-bookmark)
                `((:name "All Inboxes"
                   :query "maildir:/gmail/Inbox OR maildir:/mit/INBOX OR maildir:/fastmail/INBOX"
                   :key ?i)
                  (:name "Unread messages"
                   :query ,(funcall skip-trash-and-sent "flag:unread AND NOT flag:trashed")
                   :key ?u)
                  (:name "Today's messages"
                   :query ,(funcall skip-trash-and-sent "date:today..now")
                   :key ?t)
                  (:name "Last 7 days"
                   :query ,(funcall skip-trash-and-sent "date:7d..now")
                   :key ?w)))))


(setq mu4e-completing-read-function 'completing-read)
(setq mu4e-context-policy 'pick-first)
(setq mu4e-confirm-quit nil)
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-view-show-addresses t)
(setq send-mail-function 'smtpmail-send-it)

; Don't ask for confirmation when executing
(define-key mu4e-headers-mode-map (kbd "x") (lambda() (interactive) (mu4e-mark-execute-all t)
                                              (switch-to-buffer "*mu4e-headers*")))

(define-key mu4e-headers-mode-map (kbd "C-c c") 'org-mu4e-store-and-capture)
(define-key mu4e-view-mode-map    (kbd "C-c c") 'org-mu4e-store-and-capture)

(defun zjn--confirm-empty-subject ()
  "Allow user to quit when current message subject is empty."
  (or (message-field-value "Subject")
      (yes-or-no-p "Really send without Subject? ")
      (keyboard-quit)))
(add-hook 'message-send-hook #'zjn--confirm-empty-subject)


(defun zjn--fwd ()
  (interactive)
  (save-excursion
    (let ((msg (mu4e-message-at-point))
          (instapaper-email "readlater.safqxp45y6q@instapaper.com"))
      (mu4e-context-switch nil "Gmail")
      (compose-mail instapaper-email
                    (mu4e-message-field msg :subject))
      (mu4e-compose-attach-message msg)
      (if (yes-or-no-p "Forward to instapaper?")
          (message-send-and-exit)
        (let ((message-kill-buffer-query nil))
          (mu4e-message-kill-buffer))
        (message "Discarded.")))))

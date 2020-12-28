;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

; you think we'd know this...needed for "doom" tool to work
(setenv "EMACSDIR" "~/.emacs.d")


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Zachary Newman"
      user-mail-address "z@znewman.net")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
; (setq doom-theme 'doom-one-light)
(flucui-themes-load-style 'light)
(setq doom-font (font-spec :family "Iosevka" :height 11))

(after! tex
  (add-to-list 'TeX-command-list '("Tectonic" "tectonic %t" TeX-run-compile nil (latex-mode) :help "Run Tectonic")))
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/notes/")
(after! org
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  (defun org-file (f)
    (concat org-directory "/" f))
  (setq org-agenda-files
        (list (org-file "personal.org")
              (org-file "gtd.org")
              (org-file "research/default.org")
              (org-file "research/hnp.org")
              (org-file "research/radio.org")
              (org-file "research/broadcast.org")
              (org-file "research/accumulators.org")
              (org-file "research/tor.org")
              (org-file "6.893/6.893.org")
              (org-file "6852-ta.org")
              (org-file "school.org")))
   ; performance
   (setq org-agenda-dim-blocked-tasks nil
         org-agenda-inhibit-startup t
         org-agenda-ignore-properties '(effort appt stat category)
         org-agenda-use-tag-inheritance '(search timeline agenda))
  (setq org-archive-location "archive/%s::")
  (setq org-default-notes-file (org-file "gtd.org"))
  (setq org-log-done t)
  (setq org-adapt-indentation nil)
  (setq org-ctrl-k-protect-subtree t)
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-todo-keywords
        '((sequence "NEXT(n)" "BUY" "TODO(t)" "|" "DONE(d!)")
          (sequence "HABIT(h)" "|" "HABITDONE(H)")
          (sequence "PROJ(p)" "BLOCKEDPROJ(b)" "|" "PROJDONE(P)")
          (sequence "WAITING(w)" "SOMEDAY(s)" "|" "CANCELLED(c)")))
  (setq org-enforce-todo-dependencies nil)
  (setq org-tag-persistent-alist '((:startgroup . nil)
                                   ("@errand" . ?e)
                                   ("@home" . ?h)
                                   ("@campus" . ?c)
                                   (:endgroup . nil)
                                   ("internet" . ?i)
                                   ("code" . nil)
                                   ("gradschool" . ?g)
                                   ("personal" . ?p)
                                   ("katie" . ?k)
                                   ))
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-prefix-format '((agenda . " %?-12t% s")
                                   (timeline . "  % s")
                                   (todo . "")
                                   (tags . "")
                                   (search . "g%-8:e")))
  (defun zjn-any (@list) (eval `(or ,@ @list)))
  (defun zjn-all (@list) (eval `(and ,@ @list)))

  (defun zjn-collect-matching (pred list)
    (apply 'append (mapcar (lambda (elem) (if (funcall pred elem) (list elem) '()))
            list)))

  (defun zjn-current-time-invalid-for-tag (tag)
    (let* ((range (mapcar 'string-to-number (split-string tag "_")))
           (curr (string-to-number (format-time-string "%H%M"))))
      (or (< curr (car range)) (> curr (cadr range)))))

  (setq zjn-days-of-week '("sun" "mon" "tue" "wed" "thu" "fri" "sat"))

  (defun zjn-current-day-invalid-for-tag (tag)
    (not (string= (downcase (format-time-string "%a"))
                  tag)))

  (defun zjn-org-skip-subtree-if-bad-time ()
    "Skip entries with invalid time tags or day of week tags."
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (tags (org-get-tags))
           (time-tags (seq-filter (lambda (tag) (string-match-p "[0-9]\\{4\\}_[0-9]\\{4\\}$" tag)) tags))
           (day-tags (seq-intersection zjn-days-of-week tags)))
      (if (or (and time-tags
                   (zjn-all (mapcar 'zjn-current-time-invalid-for-tag time-tags)))
              (and day-tags
                   (zjn-all (mapcar 'zjn-current-day-invalid-for-tag day-tags))))
          subtree-end
        nil)))

  (setq org-stuck-projects '("-SOMEDAY/PROJ" ("NEXT" "BUY") nil ""))
  (setq org-agenda-todo-ignore-scheduled 'past)
  (setq org-agenda-todo-ignore-deadlines 'near)
  (setq org-agenda-tags-todo-honor-ignore-options t)
  (setq org-agenda-custom-commands
        `(
          ("n" "Default View"  ; a little faster
           ((agenda "" ((org-agenda-span 'day) (org-agenda-start-day "+0d")))
            (tags-todo "-personal-SOMEDAY-yak-@errand/NEXT-BUY"
                  ((org-agenda-skip-function 'zjn-org-skip-subtree-if-bad-time)
                   (org-agenda-overriding-header "Work:")))
            (tags-todo "personal-SOMEDAY-yak-@errand/NEXT-BUY"
                  ((org-agenda-skip-function 'zjn-org-skip-subtree-if-bad-time)
                   (org-agenda-overriding-header "Other tasks (excluding errands):")))
            (tags-todo "-SOMEDAY+@errand+TODO=\"NEXT\""
                  ((org-agenda-skip-function 'zjn-org-skip-subtree-if-bad-time)
                   (org-agenda-overriding-header "Errands:")))
            ))
          ("f" "Full View"  ; more complete
           ((agenda "" ((org-agenda-span 'day) (org-agenda-start-day "+0d")))
            (stuck "")
            (tags-todo "-personal-SOMEDAY-yak-@errand/NEXT-BUY"
                  ((org-agenda-skip-function 'zjn-org-skip-subtree-if-bad-time)
                   (org-agenda-overriding-header "Work:")))
            (tags-todo "personal-SOMEDAY-yak-@errand/NEXT-BUY"
                  ((org-agenda-skip-function 'zjn-org-skip-subtree-if-bad-time)
                   (org-agenda-overriding-header "Other tasks (excluding errands):")))
            (tags-todo "-SOMEDAY+@errand+TODO=\"NEXT\""
                  ((org-agenda-skip-function 'zjn-org-skip-subtree-if-bad-time)
                   (org-agenda-overriding-header "Errands:")))
            (tags-todo "-SOMEDAY/PROJ|BLOCKEDPROJ"
                  ((org-agenda-overriding-header "All current projects:")))
            (tags-todo "yak-SOMEDAY/NEXT|BUY"
                  ((org-agenda-overriding-header "Yak shaving:")))))
           ))

  (setq org-show-context-detail
        (quote
         ((agenda . canonical)
          (bookmark-jump . lineage)
          (isearch . lineage)
          (default . ancestors))))
  (advice-add 'org-id-new :filter-return #'upcase)

  ; IACR capture!
  (defun zjn--fetch-iacr-info (id)
    (let ((default-directory "~/git/iacr-dl"))
      (car (process-lines "nix-shell"
                          "--run"
                          (concat "python -m iacr " id)))))


  (require 'json)
  (require 'org-attach)
  (defun zjn--format-iacr-org (region)
    (let* ((id (if (string-empty-p region)
                   (read-string "IACR ePrint ID (ex. 2019/001)? ")
                 region))
           (json-string (zjn--fetch-iacr-info id))
           (json-object-type 'hash-table)
           (json-array-type 'list)
           (json-key-type 'string)
           (article (json-read-from-string json-string)))
      (save-excursion
        (org-back-to-heading t)
        (end-of-line)
        ; Use insert rather than the format string so we don't clobber the article
        ; attachment
        (insert (gethash "title" article)
                "\nhttps://eprint.iacr.org/"
                (gethash "id" article)
                "\nAuthor(s): "
                (mapconcat 'identity (gethash "authors" article) ", ")
                "\n#+BEGIN_SRC bibtex\n"
                (gethash "bibtex" article)
                "#+END_SRC"))
      (let ((org-attach-directory "~/notes/data"))
        (org-attach-attach (gethash "pdf_link" article) nil 'url)))
    "")  ; needs to return string to satisfy org-capture

  (setq org-capture-templates nil)
  (push '("i" "IACR" entry (file+headline "~/notes/research/default.org" "Paper queue")
          "* %(zjn--format-iacr-org \"%i\")\n")
        org-capture-templates)

  ; arXiv capture!
  ; Doesn't handle authors with non-ASCII names....
  (defun zjn--format-arxiv-org (region)
    (let* ((id (if (string-empty-p region)
                   (read-string "arXiv ID (ex. 1905.11379)? ")
                 region))
           (api-url (format "http://export.arxiv.org/api/query?id_list=%s" id)))
      (request
       api-url
       :parser (lambda () (libxml-parse-xml-region (point) (point-max)))
       :success
       (cl-function
        (lambda (&key data &allow-other-keys)
          (let* ((entry (first (xml-get-children data 'entry)))
                (title (replace-regexp-in-string " *\n *" " " (caddar (xml-get-children entry 'title))))
                (authors (mapconcat (lambda (x) (caddr (caddr x)))
                                    (xml-get-children entry 'author) ", "))
                (pdf-link
                 (concat (cdr
                          (assoc 'href
                                 (cadar
                                  (cl-remove-if-not
                                   (lambda (x)
                                     (string= (cdr (assoc 'title (cadr x))) "pdf"))
                                   (xml-get-children entry 'link)))))
                  ".pdf"))
                (article-link
                 (cdr (assoc 'href (cadar
                                    (cl-remove-if-not
                                     (lambda (x)
                                       (string= (cdr (assoc 'rel (cadr x))) "alternate"))
                                     (xml-get-children entry 'link)))))))
            (save-excursion
              (org-back-to-heading t)
              (end-of-line)
              (insert title
                      "\n"
                      article-link
                      "\nAuthor(s): "
                      authors)
              (sit-for 0.1)
              (let ((org-attach-directory "~/notes/data"))
                (org-attach-attach pdf-link nil 'url))))))))
      "")


  (push '("a" "arXiv" entry (file+headline "~/notes/research/default.org" "Paper queue")
          "* %?%(zjn--format-arxiv-org \"%i\")\n")
        org-capture-templates)

  (push '("l" "Link to current file" entry
          (file+headline "~/notes/gtd.org" "Inbox")
          "** NEXT %?\n%a\n%i\n")
        org-capture-templates)

  (push '("r" "Capture current region" entry
          (file+headline "~/notes/gtd.org" "Inbox")
          "** NEXT %?\n%i\n")
        org-capture-templates)

  (push '("t" "Normal TODO" entry
          (file+headline "~/notes/gtd.org" "Inbox")
          "** NEXT %?\n")
        org-capture-templates)

  (push '("p" "Paper" entry
          (file+headline "~/notes/research/default.org" "Paper queue")
          "** %i\nURL:\nAuthor(s):\n\n#+BEGIN_SRC bibtex\n#+END_SRC")
        org-capture-templates)

  (setq org-ref-completion-library 'org-ref-ivy-cite)
  (require 'org-ref)
  (setq org-latex-pdf-process '("tectonic %f"))
  (org-ref-find-bibliography)
  (setq biblio-crossref-user-email-address "z@znewman.net")
  (require 'biblio)
  (defun zjn/in-bib (orig &rest args)
    (with-current-buffer (car (org-ref-find-bibliography))
      (apply orig args)))
  (advice-add 'crossref-lookup :around #'zjn/in-bib)
  )

(after! mu4e
  (require 'org-mu4e)
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
  ; After mu 1.4, will need to remove this:
  ; https://emacs.stackexchange.com/questions/47206/mu4e-adds-sender-with-reply-all
  ; https://www.djcbsoftware.nl/code/mu/mu4e/Writing-messages.html
  (setq mu4e-user-mail-address-list (quote ("z@znewman.net" "znewman01@gmail.com" "zjn@mit.edu" "zjn@csail.mit.edu")))
  (setq mu4e-attachment-dir "/home/zjn/Downloads")
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
            :name "CSAIL"
            :match-func (zjn--make-match "csail")
            :vars '((mu4e-trash-folder . "/csail/Trash")
                    (mu4e-sent-folder . "/csail/Sent")
                    (mu4e-drafts-folder . "/csail/Drafts")
                    (mu4e-refile-folder . "/csail/Archive")
                    (user-mail-address . "zjn@csail.mit.edu")
                    (user-full-name . "Zachary Newman")
                    (smtpmail-local-domain . "csail.mit.edu")
                    (smtpmail-smtp-server . "outgoing.csail.mit.edu")
                    (smtpmail-stream-type . starttls)
                    (smtpmail-smtp-service . 587)))
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
                     :query "maildir:/gmail/Inbox OR maildir:/mit/INBOX OR maildir:/fastmail/INBOX OR maildir:/csail/INBOX"
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
  )

(after! elfeed
  (setq elfeed-feeds
        '(("http://bristolcrypto.blogspot.com/feeds/posts/default")
          ("https://www.schneier.com/blog/atom.xml")
          ("https://www.mattblaze.org/blog/rss20.xml")
          ("https://blog.cryptographyengineering.com/feed/")
          ("https://blog.techorganic.com/atom.xml")
          ("https://joy.recurse.com/feed.atom")
          ("https://blog.erratasec.com/feeds/posts/default?alt=rss")
          ("http://barrebas.github.io/atom.xml")
          ("http://paperpools.blogspot.com/feeds/posts/default")
          ("http://lambda-the-ultimate.org/rss.xml")
          ("http://feeds.feedburner.com/Fsharpforfunandprofit?format=xml")
          ("http://feeds.feedburner.com/crajobs?format=xml")
          ("https://islandsofnewyork.blog?feed=atom")
          ("https://blog.acolyer.org/feed/")
          ("https://www.iacr.org/news/rss")
          ("https://slatestarcodex.com/feed/")
          ("http://www.christianmoscardi.com/feed.xml")
          ("https://mass.streetsblog.org/feed/")
          ("http://scholars-stage.blogspot.com/feeds/posts/default")
          ("http://feeds.feedburner.com/creditslips/feed?format=xml")
          ("https://this-week-in-rust.org/atom.xml")
          ("http://squidarth.com/feed.xml")
          ("https://windowsontheory.org/feed/")
          ("https://www.interfluidity.com/feed")
          ("https://schlosser.io/rss.xml")
          ("https://qualiacomputing.com/feed/")
          ("https://algorithmsoup.wordpress.com/feed.xml")
          ("https://www.scottaaronson.com/blog/?feed=rss2")))

  (defun elfeed-show-browse-url ()
    (interactive)
    (browse-url (elfeed-entry-link elfeed-show-entry)))
  (map! :mode 'elfeed-search-mode :n "o" #'elfeed-search-browse-url)
  (map! :mode 'elfeed-show-mode :n "o" #'elfeed-show-browse-url)

  ; Instapaper + Elfeed
  (require 'request)

  (defun add-to-instapaper (url success-callback)
    (let* ((username "znewman01@gmail.com")
           ; work around "gpg: selecting card failed" issue
           (password (car (process-lines "sh" "-c" "pass show instapaper 2>/dev/null"))))
      (request "https://www.instapaper.com/api/add"
        :params `(("url" . ,url)
                  ("username" . ,username)
                  ("password" . ,password))
        :success success-callback
        :error (cl-function
                (lambda (&key error-thrown &allow-other-keys)
                  (warn "Issue adding to Instapaper! %S" error-thrown))))))

  (defun add-elfeed-entry-to-instapaper ()
    (interactive)
    (let ((entry (elfeed-search-selected :single)))
      (add-to-instapaper
       (elfeed-entry-link entry)
       (cl-function (lambda (&key data &allow-other-keys)
                      (message "Added to Instapaper!")
                      (elfeed-untag entry 'unread)
                      (elfeed-search-update-entry entry)))))
    (unless (use-region-p) (forward-line)))

  (defun add-elfeed-shown-to-instapaper ()
    (interactive)
    (add-to-instapaper
     (elfeed-entry-link elfeed-show-entry)
     (cl-function (lambda (&key data &allow-other-keys)
                    (message "Added to Instapaper!")))))

  (map! :mode 'elfeed-search-mode :n "i" #'add-elfeed-entry-to-instapaper)
  (map! :mode 'elfeed-show-mode :n "i" #'add-elfeed-shown-to-instapaper)

  (defun add-elfeed-entry-to-paper-queue-iacr ()
    (interactive)
    (let ((entry (elfeed-search-selected :single)))
      (org-capture-string (elfeed-entry-link entry) "i"))
    (unless (use-region-p) (forward-line)))

  (defun add-elfeed-shown-to-paper-queue-iacr ()
    (interactive)
    (org-capture-string (elfeed-entry-link elfeed-show-entry) "i"))

  (map! :mode 'elfeed-search-mode :n "I" #'add-elfeed-entry-to-paper-queue-iacr)
  (map! :mode 'elfeed-show-mode :n "I" #'add-elfeed-shown-to-paper-queue-iacr)
  )


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)
(setq doom-localleader-key ",")

(after! projectile
  (setq projectile-project-search-path '("~/git"))
  (defun zjn-projectile-root-for-some-major-modes (_dir)
    (message "%s" major-mode)
    (let ((modes '(mu4e-headers-mode mu4e-main-mode mu4e-view-mode org-agenda-mode)))
      (if (memq major-mode modes) "~/Dropbox/notes")))
  ; (push 'zjn-projectile-root-for-some-major-modes projectile-project-root-files-functions))
  )

(setq tramp-inline-compress-start-size 1000000)

(after! lsp-mode
  (push "[/\\\\]\\venv$" lsp-file-watch-ignored)
  (push "[/\\\\]\\.venv$" lsp-file-watch-ignored))
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

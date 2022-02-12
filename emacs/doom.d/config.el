;;; -*- lexical-binding: t; -*-

(setenv "EMACSDIR" "~/.emacs.d")

(setq user-full-name "Zachary Newman"
      user-mail-address "z@znewman.net")

(setq doom-localleader-key ",")

(setq doom-leader-alt-key "s-SPC")
(setq doom-localleader-alt-key "s-SPC m")

(setq enable-local-variables t)

(require 'url)
(require 'f)
(require 's)
(require 'dash)
(require 'json)

(defun zjn--fetch-iacr-info (id)
  (require 'json)
  (let* ((default-directory "~/git/iacr-dl")
         (blob (car (process-lines "nix-shell" "--run" (concat "python -m iacr " id))))
         (json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type nil))
    (json-read-from-string blob)))

(defun zjn--password (name)

  ; sh is to work around "gpg: selecting card failed" issue
  (car (process-lines "sh" "-c" "pass show instapaper 2>/dev/null")))

(defun zjn--add-to-instapaper (url success-callback)
  (require 'request)
  (let* ((username "znewman01@gmail.com")
         (password (zjn--password "instapaper")))
    (request "https://www.instapaper.com/api/add"
      :params `(("url" . ,url)
                ("username" . ,username)
                ("password" . ,password))
      :success success-callback
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (warn "Issue adding to Instapaper! %S" error-thrown))))))

(defun zjn--search-in-firefox-inner (default-text force-edit-text &optional prefix)
  (let* ((edit-text (or (s-blank? default-text) force-edit-text))
         (initial-position (if (s-blank? prefix) (length default-text) (length prefix)))
         (default-text (s-concat prefix default-text))
         (search-string (if edit-text (read-string "search string: " (cons default-text initial-position)) default-text))
         (url (s-concat "https://duckduckgo.com/?t=ffab&q=" (url-encode-url search-string))))
    (call-process "xdg-open" nil nil nil url)))

(defun zjn--current-region ()
  (if (doom-region-active-p) (buffer-substring (mark) (point)) ""))

(defun zjn--search-in-firefox (edit-text)
  (interactive "P")
  (zjn--search-in-firefox-inner (zjn--current-region) edit-text))

(defun zjn--search-in-firefox-bang (edit-text)
  (interactive "P")
  (zjn--search-in-firefox-inner (zjn--current-region) edit-text "! "))

(map! :leader
      (:prefix ("s" . "search")
       :desc "Search in DDG" "o" #'zjn--search-in-firefox
       :desc "Search in DDG (!)" "O" #'zjn--search-in-firefox-bang))

(after! org
  (require 'org-ql)
  (require 'org-super-agenda)
  (require 'org-attach)
  (require 'org-capture)
  (setq org-log-done t)
  (setq org-log-state-notes-into-drawer t)
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
                                   ("katie" . ?k)))
  (setq org-directory "~/notes")
  (defun org-file (f)
    (concat org-directory "/" f))
  (setq org-agenda-files
        (mapcar #'org-file
                '("personal.org"
                  "gtd.org"
                  "projects.org"
                  "chainguard.org"
                  "research/default.org"
                  "research/broadcast.org"
                  "research/accumulators.org"
                  "research/tor.org"
                  "school.org")))
  (setq org-archive-location "archive/%s::")
  (setq org-attach-directory (org-file ".attach"))
  (setq org-attach-id-dir (org-file ".attach"))
  (setq org-default-notes-file (org-file "gtd.org"))
  (setq org-id-locations-file (org-file ".org-id-locations"))
  (setq org-capture-templates nil)
  (push '("l" "Link to current file" entry
          (file+headline "~/notes/gtd.org" "Inbox")
          "** NEXT %?\n%a\n%i\n")
        org-capture-templates)
  
  (push '("t" "Normal TODO" entry
          (file+headline "~/notes/gtd.org" "Inbox")
          "** NEXT %?\n")
        org-capture-templates)
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  (setq org-adapt-indentation nil)
  (setq org-ctrl-k-protect-subtree t)
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-startup-indented nil)
  (setq org-startup-folded 'fold)
  (setq org-show-context-detail
      (quote
          ((agenda . ancestors)
          (bookmark-jump . ancestors)
          (isearch . ancestors)
          (default . ancestors))))
  (advice-add 'org-id-new :filter-return #'upcase)
  (setq org-agenda-dim-blocked-tasks nil
      org-agenda-inhibit-startup t
      org-agenda-ignore-properties '(effort appt stat category))
  (setq org-hide-emphasis-markers t)
  (setq org-startup-with-latex-preview nil)
  (require 'org-fragtog)
  (add-hook 'org-mode-hook 'org-fragtog-mode)
  (require 'org-ref)
  (require 'bibtex-completion)
  (defun zjn--import-iacr (id)
    (interactive "sIACR ePrint ID? ")
    (let* ((article (zjn--fetch-iacr-info id))
           (download-fname (format "iacr:%s.pdf" (s-replace "/" ":" (alist-get 'id article))))
           (download-path (f-join bibtex-completion-library-path download-fname))
           (fixed-bibtex (s-replace "cryptoeprint" "iacr" (alist-get 'bibtex article))))
      (message "Found %s." (alist-get 'id article))
      (write-region fixed-bibtex nil bibtex-completion-bibliography 'append)
      (url-copy-file (alist-get 'pdf_link article) download-path t)
      (bibtex-completion-clear-cache)))
  
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
      (org-attach-attach (gethash "pdf_link" article) nil 'url))
      "")  ; needs to return string to satisfy org-capture
  
  (push '("i" "IACR" entry (file+headline "~/notes/research/default.org" "Paper queue")
          "* %(zjn--format-iacr-org \"%i\")\n")
        org-capture-templates)
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
          (let* ((entry (car (xml-get-children data 'entry)))
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
              (org-attach-attach pdf-link nil 'url)))))))
    "")
  (require 'org-anki)
  (setq org-anki-default-deck "Default"))

(after! org-super-agenda
  (require 'evil-org-agenda)
  (org-super-agenda-mode)
  (setq org-super-agenda-header-map evil-org-agenda-mode-map) ; https://github.com/alphapapa/org-super-agenda/issues/50
  (setq org-agenda-prefix-format '((agenda . " %?-12t% s")
                                   (timeline . "  % s")
                                   (todo . "")
                                   (tags . "")
                                   (search . "g%-8:e")))

  (setq org-agenda-custom-commands
    (let ((agenda-common
           '(agenda ""
                    ((org-agenda-span 'day)
                     (org-agenda-start-day "+0d")
                     (org-agenda-skip-timestamp-if-done t)
                     (org-agenda-skip-scheduled-if-done t)
                     (org-agenda-skip-deadline-if-done t)
                     (org-super-agenda-groups '((:discard (:not (:time-grid t)))
                                                (:name "Agenda:" :time-grid t))))))
          (stuck-common
           '(org-ql-block '(and
                            (not (tags "SOMEDAY"))
                            (or (and (todo "PROJ")
                                     (not (descendants (todo "NEXT"))))
                                (and (todo "BLOCKEDPROJ")
                                     (not (scheduled t))
                                     (not (deadline t)))
                                (and (todo "WAITING")
                                     (not (or (scheduled t) (deadline t))))
                                (and (todo "SOMEDAY")
                                     (not (tags "SOMEDAY")))
                                (and (todo "TODO")
                                     (not (ancestors (todo "PROJ" "BLOCKEDPROJ"))))))
                          ((org-ql-block-header "Stuck projects:"))))
          (tasks-common
           (lambda (desc)
             `(org-ql-block '(and
                             (or (and (todo "NEXT") (not (tags "SOMEDAY")))
                                 (deadline auto)
                                 (scheduled :to today))
                             (not (done))
                             (not (scheduled :on today :with-time t)))
                              ((org-super-agenda-groups '((:discard (:time-grid t))
                                                          (:name "Overdue:"
                                                                 :deadline past)
                                                          (:name "Upcoming:"
                                                                 :deadline future
                                                                 :deadline today)
                                                          (:name "Habits"
                                                                 :todo "HABIT")
                                                          (:name "Scheduled:"
                                                                 :scheduled past
                                                                 :scheduled today)
                                                          (:name "Work:"
                                                                 :tag "chainguard")
                                                          (:name "Errands:" :order 1
                                                                 :tag "@errand")
                                                          (:discard (:tag "yak"))
                                                          (:name "Other tasks:"
                                                                 :anything t)))
                               (org-ql-block-header ,(concat desc " tasks:")))))))
      `(("nw" "Work"
         (,agenda-common
          ,stuck-common
          ,(funcall tasks-common "Chainguard"))
         ((org-agenda-tag-filter-preset '("+chainguard"))))
        ("na" "All"
         (,agenda-common
          ,stuck-common
          ,(funcall tasks-common "All"))))))

  ; https://lists.gnu.org/archive/html/emacs-orgmode/2015-06/msg00266.html
  (defun org-agenda-delete-empty-blocks ()
    "Remove empty agenda blocks.
  A block is identified as empty if there are fewer than 2
  non-empty lines in the block (excluding the line with
  `org-agenda-block-separator' characters)."
    (when org-agenda-compact-blocks
      (user-error "Cannot delete empty compact blocks"))
    (setq buffer-read-only nil)
    (save-excursion
      (goto-char (point-min))
      (let* ((blank-line-re "^\\s-*$")
             (content-line-count (if (looking-at-p blank-line-re) 0 1))
             (start-pos (point))
             (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
        (while (and (not (eobp)) (forward-line))
          (cond
           ((looking-at-p block-re)
            (when (< content-line-count 2)
              (delete-region start-pos (1+ (point-at-bol))))
            (setq start-pos (point))
            (forward-line)
            (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
           ((not (looking-at-p blank-line-re))
            (setq content-line-count (1+ content-line-count)))))
        (when (< content-line-count 2)
          (delete-region start-pos (point-max)))
        (goto-char (point-min))
        ;; The above strategy can leave a separator line at the beginning
        ;; of the buffer.
        (when (looking-at-p block-re)
          (delete-region (point) (1+ (point-at-eol))))))
    (setq buffer-read-only t))
  (add-hook 'org-agenda-finalize-hook #'org-agenda-delete-empty-blocks))

(after! oc
    (setq org-cite-global-bibliography '("~/notes/lit/default.bib"))
    (org-link-set-parameters "cite" :display 'org-link))

(after! org-ref
  (setq org-ref-default-bibliography '("~/Sync/notes/lit/default.bib")
        org-ref-pdf-directory "~/Sync/notes/lit/"))

(map! :leader "a" (cmd! (org-agenda nil "nw")))
(map! :mode org-capture-mode :localleader "s r" #'org-capture-refile)
(map! :mode org-mode :n "t" #'org-todo)
(map! :map org-agenda-mode-map :localleader "." #'counsel-org-goto-all
    :localleader "/" #'counsel-org-goto-all)
(map! :leader "s /" #'counsel-org-goto-all)

(defun zjn/with-pkgs (interpreter &rest pkgs)
  (s-concat
    "#!/usr/bin/env nix-shell\n"
     "#!nix-shell -p " (s-join " " pkgs) " -i " interpreter))
(defun zjn/with-pkgs-bash (&rest pkgs)
  (apply #'zjn/with-pkgs (cons "bash" pkgs)))

(after! org-roam
  (setq org-roam-directory "~/Sync/notes/roam"
        org-roam-completion-everywhere nil
        +org-roam-open-buffer-on-find-file nil))
;
; TODO: replace with org-roam-capture-templates
;  (setq orb-templates
;        '(("r" "ref" plain #'org-roam-capture--get-point "" :file-name "bib/${citekey}" :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n" :unnarrowed t :immediate-finish t)))
;   (org-roam-mode)
;  (map! :mode org-mode :leader "n r n" #'orb-note-actions))

(setq bibtex-completion-bibliography "~/Sync/notes/lit/default.bib"
      bibtex-completion-library-path "~/Sync/notes/lit/"
      bibtex-completion-notes-path "~/Sync/notes/roam/bib/")
(after! ivy-bibtex
  (require 'org-roam)
  (org-roam-setup)
  ;; Basic configuration
  (setq reftex-default-bibliography '("~/Sync/notes/lit/default.bib")
        biblio-crossref-user-email-address "crossref@z.znewman.net")
  (setq bibtex-completion-fallback-options
        '(("DBLP (computer science bibliography)      (biblio.el)"
           . (lambda (search-expression) (biblio--lookup-1 #'biblio-dblp-backend search-expression)))
          ("CrossRef                                  (biblio.el)"
           . (lambda (search-expression) (biblio-lookup #'biblio-crossref-backend search-expression)))
          ("arXiv                                     (biblio.el)"
           . (lambda (search-expression) (biblio-lookup #'biblio-arxiv-backend search-expression)))
          ("Google Scholar                            (web)"
           . "https://scholar.google.com/scholar?q=%s")
          ("IACR                                      (web)"
           ."https://duckduckgo.com/?q=site%%3Aeprint.iacr.org+%s")))

  ;; Now make it work like I want
  ;; - if there's no match, ask where we want to search
  ;; - if there is a match and I hit enter, ask what I want to do (abbreviated
  ;; - to most common actions)
  (defun zjn--bibtex-open-pdf (keys)
    (bibtex-completion-open-pdf keys #'bibtex-completion-add-pdf-to-library))
  (ivy-bibtex-ivify-action zjn--bibtex-open-pdf zjn--ivy-bibtex-pdf)

  (defun zjn--bibtex-open-notes-and-pdf (keys)
    (let* ((key (car keys))
           (org-roam-find-file-function (lambda (file) (switch-to-buffer (find-file-noselect file) nil 'force-same-window)))
           (pdf (bibtex-completion-find-pdf key)))
      (+workspace-switch key t)
      (delete-other-windows)
      (when pdf
        (let ((pdf-buffer (find-file (car pdf))))
          (switch-to-buffer pdf-buffer nil 'force-same-window)
          (split-window-right))
        (windmove-right))
      (let ((org-capture-link-is-already-stored t))
                                        ; prevent trying to grab a link to the PDF
        (orb-edit-notes key))))
  (ivy-bibtex-ivify-action zjn--bibtex-open-notes-and-pdf zjn--ivy-bibtex-notes)

  (setq zjn--ivy-bibtex-short-actions
        '((?p "[p]df" zjn--ivy-bibtex-pdf)
          (?n "[n]otes" zjn--ivy-bibtex-notes)
          (?i "[i]nsert" ivy-bibtex-insert-citation)))
  (defun zjn--ivy-bibtex-get-action ()
    (let* ((actions zjn--ivy-bibtex-short-actions)
           (names (cl-mapcar (lambda (entry) (cadr entry)) actions))
           (chars (cl-mapcar (lambda (entry) (car entry)) actions))
           (prompt (s-concat (s-join " " names) " "))
           (choice (read-char-choice prompt chars))
           (entry (cl-find-if (lambda (entry) (= (car entry) choice)) actions))
           (action (third entry)))
      action))

  (defun zjn/ivy-bibtex-open-or-search (candidate)
    "Dispatches to other actions, or searches using fallback options if no match found."
    (if (listp candidate)
        (let ((key (cdr (assoc "=key=" (cdr candidate))))
              (action (zjn--ivy-bibtex-get-action)))
          (funcall action candidate))
      (ivy-bibtex-fallback candidate)))

  (setq ivy-bibtex-default-action #'zjn/ivy-bibtex-open-or-search)

  (defun zjn--ivy-bibtex-insert-or-search (candidate)
    (if (listp candidate)
        (ivy-bibtex-insert-citation candidate)
      (ivy-bibtex-fallback candidate)))
  (map! :mode org-mode
        "C-c ]"
        (cmd! (let ((ivy-bibtex-default-action #'zjn--ivy-bibtex-insert-or-search)) (ivy-bibtex))))

  ;; Rename citation keys
  (defun zjn--bibtex-rename (keys)
    "Rename the citation key given by the first element of KEYS.

  Updates the .bib file and moves the PDF and .org (notes) file.

  org-roam updates back-references to the notes file, but PDF references and
  citations are LTTR.
  "
    ; (org-roam-mode 1)  ; for rename-file advice
    (cl-flet ((update (path new-key suffix)
                      (when path
                        (f-join (f-parent path) (concat new-key suffix)))))
      (let* ((old-key (car keys))
             (new-key (read-string (format "New cite key (was %s): " old-key)))
             (old-note (caar (org-roam-db-query
                              [:select file :from refs :where (= ref $s1)]
                              old-key)))
             (new-note (update old-note new-key ".org"))
             (old-pdfs (bibtex-completion-find-pdf old-key))
             (old-pdf (car old-pdfs))
             (new-pdf (update old-pdf new-key ".pdf")))
        ;; 1. BibTeX entry
        (save-excursion
          (bibtex-completion-show-entry (list old-key))
          (zjn--bib-replace-key new-key)
          (bibtex-reformat)
          (bibtex-sort-buffer)
          (save-buffer))
        ;; 2. PDF
        (when (> (length old-pdfs) 1)
          (error "Cannot rename when there's supplemental PDFs."))
        (rename-file old-pdf new-pdf)
                                        ; eventually might have to fix up org-noter or something...
        ;; 3. org-roam
        (when old-note
          (rename-file old-note new-note)
          (bibtex-completion-clear-cache)
          (org-roam-build-cache)))))
  (ivy-bibtex-ivify-action zjn--bibtex-rename zjn--ivy-bibtex-rename)
  (let ((ivy-actions (copy-alist (plist-get ivy--actions-list 'ivy-bibtex))))
    (setf (alist-get "m" ivy-actions nil nil #'equal)
          (list #'zjn--ivy-bibtex-rename "Rename (move) the citation key."))
    (ivy-set-actions 'ivy-bibtex ivy-actions)))


; TODO: handle author names in "Last, First" format.
(defun zjn--default-key-name ()
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (author-string (cdr (assoc-string "author" entry)))
         (authors (s-split " and " (s-collapse-whitespace author-string)))
         (year (cdr (assoc-string "year" entry))))
    (concat
     (cond ((= (length authors) 1) (substring (car (last (s-split " " (car authors)))) 0 3))
           ((> (length authors) 4) (concat (apply 'concat (mapcar (lambda (a) (substring (car (last (s-split " " a))) 0 1)) (subseq authors 0 3))) "+"))
           (t (apply 'concat (mapcar (lambda (a) (substring (car (last (s-split " " a))) 0 1)) authors))))
     (substring year -2))))
(defun zjn--bib-get-key (entry)
  "The key we want to use by default."
  (bibtex-beginning-of-entry)
  (while (save-excursion
           (s-equals? (cdr (assoc-string "=type=" (bibtex-parse-entry)))
                      "proceedings"))
    (bibtex-previous-entry)
    (bibtex-beginning-of-entry))
  (read-string "Key: " (zjn--default-key-name)))
(defun zjn--bib-replace-key (new-key)
  "Replace the bibtex key of the current entry."
                                        ; cribbed from bibtex-clean-entry
  (save-excursion
    (re-search-forward bibtex-entry-maybe-empty-head)
    (if (match-beginning bibtex-key-in-head)
        (delete-region (match-beginning bibtex-key-in-head)
                       (match-end bibtex-key-in-head)))
    (insert new-key)))
(defun zjn--bib-replace-last-key (new-key)
  (goto-char (point-max))
  (bibtex-beginning-of-entry)
  (while (save-excursion
           (s-equals? (cdr (assoc-string "=type=" (bibtex-parse-entry)))
                      "proceedings"))
    (bibtex-previous-entry))
  (zjn--bib-replace-key new-key))
(defun zjn--bib-get-url (entry)
  (let ((url (alist-get 'url entry))
        (direct-url (alist-get 'direct-url entry)))
    (cond
     (direct-url)
     ((s-starts-with? "https://eprint.iacr.org/" url) (s-concat url ".pdf"))
     (t (read-string "URL (blank for none): ")))))
(defun zjn--bib-add (bibtex entry)
  "Add BIBTEX (from ENTRY) to end of a user-specified bibtex file."
  (with-temp-file bibtex-completion-bibliography
    (bibtex-set-dialect)
    (insert-file-contents bibtex-completion-bibliography)
    (goto-char (point-max))
    (insert (s-concat "\n\n" bibtex))
    (goto-char (point-max))
    (let ((key (zjn--bib-get-key entry)))
      (zjn--bib-replace-last-key key)
      (bibtex-reformat)
      (bibtex-sort-buffer)
      (let ((url (zjn--bib-get-url entry)))
        (when (s-present? url)
          (let* ((fname (s-concat key ".pdf"))
                 (dest (f-join bibtex-completion-library-path fname)))
            (url-copy-file url dest t))))))
  (message "Inserted bibtex entry for %S."
           (biblio--prepare-title (biblio-alist-get 'title entry))))
(defun zjn/bib-add ()
  "Insert BibTeX of current entry at the end of user-specified bibtex file and go there."
  (interactive)
  (biblio--selection-forward-bibtex #'zjn--bib-add t))
(after! biblio
  (setq biblio-crossref-user-email-address "crossref@z.znewman.net")
  (setq bibtex-autokey-year-length 4)
  (setq bibtex-autokey-titleword-length 100)
  (setq bibtex-autokey-titlewords 2)
  (map! :mode biblio-selection-mode
        "RET" #'zjn/bib-add))

(after! mu4e
  (setq mail-user-agent 'mu4e-user-agent)
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq mu4e-root-maildir "~/Maildir")
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-index-cleanup t      ;; don't do a full cleanup check
        mu4e-index-lazy-check nil)
  (setq mu4e-completing-read-function 'completing-read)
  (setq mu4e-confirm-quit nil)
  (setq auth-source-save-behavior nil)
  (setq mu4e-context-policy 'pick-first)
  (defmacro zjn--make-match (folder)
    `(lambda (msg)
        (when msg
            (string-prefix-p ,(concat "/" folder)
                          (mu4e-message-field msg :maildir)))))
  (require 'mu4e-context)
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
            :name "oCSAIL"
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
                    (smtpmail-smtp-user "znewman01@gmail.com")
                    (user-full-name . "Zachary Newman")
                    (smtpmail-local-domain . "gmail.com")
                    (smtpmail-smtp-server . "smtp.gmail.com")
                    (smtpmail-stream-type . starttls)
                    (smtpmail-smtp-service . 587)))
          ,(make-mu4e-context
            :name "Chainguard"
            :match-func (zjn--make-match "chainguard")
            :vars '((mu4e-trash-folder . "/chainguard/[Gmail]/Trash")
                    (mu4e-sent-folder . "/chainguard/[Gmail]/SentMail")
                    (mu4e-drafts-folder . "/chainguard/[Gmail]/Drafts")
                    (mu4e-refile-folder . "/chainguard/[Gmail]/AllMail")
                    (user-mail-address . "zjn@chainguard.dev")
                    (user-full-name . "Zachary Newman")
                    (smtpmail-local-domain . "gmail.com")
                    (smtpmail-smtp-user "zjn@chainguard.dev")
                    (smtpmail-smtp-server . "smtp.gmail.com")
                    (smtpmail-stream-type . starttls)
                    (smtpmail-smtp-service . 587)))))
  (defun zjn--get-mu4e-vars (var)
    "Get mu4e vars /in current mu4e context/"
    (mapcar (lambda (context)
              (alist-get var (mu4e-context-vars context)))
            mu4e-contexts))
  (let* ((trash-folders (zjn--get-mu4e-vars 'mu4e-trash-folder))
         (sent-folders (zjn--get-mu4e-vars 'mu4e-sent-folder))
         (query-skipping
          (lambda (query maildirs)
            (s-join " AND "
                    (cons query
                          (mapcar (apply-partially #'concat "NOT maildir:") maildirs)))))
         (skip-trash-and-sent
           (lambda (query) (funcall query-skipping query (-concat trash-folders sent-folders '("/mit/Junk" "/gmail/[Gmail]/Spam" "/chainguard/[Gmail]/Spam"))))))
      (setq mu4e-bookmarks
          (mapcar (apply-partially #'apply #'make-mu4e-bookmark)
                  `((:name "All Inboxes"
                      :query "maildir:/gmail/Inbox OR maildir:/mit/INBOX OR maildir:/fastmail/INBOX OR maildir:/csail/INBOX OR maildir:/chainguard/Inbox"
                      :key ?i)
                      (:name "Unread messages"
                      :query ,(funcall skip-trash-and-sent "flag:unread AND NOT flag:trashed")
                      :key ?u)
                      (:name "Last 7 days"
                      :query ,(funcall skip-trash-and-sent "date:7d..now")
                      :key ?w)))))
  (setq mu4e-headers-sort-field :date)
  (setq mu4e-attachment-dir "/home/zjn/Downloads")
  (mkdir mu4e-attachment-dir t)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-change-filenames-when-moving t)
  (require 'mu4e-mark)
  (setf
    (alist-get 'trash mu4e-marks)
    (plist-put (cdr (assq 'trash mu4e-marks))
               :action
               (lambda (docid msg target)
                 (mu4e~proc-move docid (mu4e~mark-check-target target) "-N"))))
  (setq mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed AND (maildir:/gmail/Inbox OR maildir:/mit/INBOX OR maildir:/fastmail/INBOX OR maildir:/csail/INBOX OR maildir:/chainguard/Inbox)")
  (require 'org-mu4e)
  (defun zjn--confirm-empty-subject ()
    "Allow user to quit when current message subject is empty."
    (or (message-field-value "Subject")
        (yes-or-no-p "Really send without Subject? ")
        (keyboard-quit)))
  (add-hook 'message-send-hook #'zjn--confirm-empty-subject)
  (setq mu4e-compose-context-policy 'ask)
  (setq send-mail-function 'smtpmail-send-it))

(after! elfeed
  (setq elfeed-db-directory (expand-file-name "~/Sync/elfeed"))
  (setq elfeed-enclosure-default-dir (expand-file-name "~/Sync/elfeed/enclosures"))
  (setq elfeed-feeds
        '(("http://bristolcrypto.blogspot.com/feeds/posts/default")
          ("https://www.schneier.com/blog/atom.xml")
          ("https://www.mattblaze.org/blog/rss20.xml")
          ("https://alinush.github.io/feed.xml")
          ("https://blog.chainguard.dev/rss/")
          ("https://dlorenc.medium.com/feed")
          ("https://jvns.ca/atom.xml")
          ("https://blog.cryptographyengineering.com/feed/")
          ("https://blog.techorganic.com/atom.xml")
          ("https://joy.recurse.com/feed.atom")
          ("https://blog.erratasec.com/feeds/posts/default?alt=rss")
          ("http://barrebas.github.io/atom.xml")
          ("http://paperpools.blogspot.com/feeds/posts/default")
          ("http://lambda-the-ultimate.org/rss.xml")
          ("http://feeds.feedburner.com/Fsharpforfunandprofit?format=xml")
          ("https://islandsofnewyork.blog?feed=atom")
          ("https://blog.acolyer.org/feed/")
          ("https://www.iacr.org/news/rss")
          ("https://slatestarcodex.com/feed/")
          ("http://www.christianmoscardi.com/feed.xml")
          ("https://danluu.com/atom.xml")
          ("https://mass.streetsblog.org/feed/")
          ("https://scholars-stage.org/?feed=atom")
          ("http://feeds.feedburner.com/creditslips/feed?format=xml")
          ("https://this-week-in-rust.org/atom.xml")
          ("http://squidarth.com/feed.xml")
          ("https://windowsontheory.org/feed/")
          ("https://www.interfluidity.com/feed")
          ("https://schlosser.io/rss.xml")
          ("https://blog.sigstore.dev/feed")
          ("https://weekly.nixos.org/feeds/all.rss.xml")
          ("https://qualiacomputing.com/feed/")
          ("https://vitalik.ca/feed.xml")
          ("https://www.tweag.io/rss.xml")
          ("https://algorithmsoup.wordpress.com/feed.xml")
          ("https://stefan.vanburen.xyz/blog/index.xml")
          ("https://www.scottaaronson.blog/?feed=rss2")))

  (defun elfeed-show-browse-url ()
    (interactive)
    (browse-url (elfeed-entry-link elfeed-show-entry)))

                                        ; Instapaper + Elfeed

  (defun add-elfeed-entry-to-instapaper ()
    (interactive)
    (let ((entry (elfeed-search-selected :single)))
      (zjn--add-to-instapaper
       (elfeed-entry-link entry)
       (cl-function (lambda (&key data &allow-other-keys)
                      (message "Added to Instapaper!")
                      (elfeed-untag entry 'unread)
                      (elfeed-search-update-entry entry)))))
    (unless (use-region-p) (forward-line)))

  (defun add-elfeed-shown-to-instapaper ()
    (interactive)
    (zjn--add-to-instapaper
     (elfeed-entry-link elfeed-show-entry)
     (cl-function (lambda (&key data &allow-other-keys)
                    (message "Added to Instapaper!")))))


  (require 'elfeed-db)
  (add-hook 'elfeed-show-mode-hook #'elfeed-db-save)
  (defun add-elfeed-entry-to-paper-queue-iacr ()
    (interactive)
    (let ((entry (elfeed-search-selected :single)))
      (zjn--import-iacr (elfeed-entry-link entry))
      (message "Imported IACR article!")))
  (defun add-elfeed-shown-to-paper-queue-iacr ()
    (interactive)
    (zjn--import-iacr (elfeed-entry-link elfeed-show-entry))
    (message "Imported IACR article!"))

  (map! :mode 'elfeed-search-mode
        :n "I" #'add-elfeed-entry-to-paper-queue-iacr
        :n "o" #'elfeed-search-browse-url
        :n "i" #'add-elfeed-entry-to-instapaper
        :n "u" #'elfeed-update
        :n "s" #'elfeed-db-save

        :mode 'elfeed-show-mode
        :n "I" #'add-elfeed-shown-to-paper-queue-iacr
        :n "o" #'elfeed-show-browse-url
        :n "i" #'add-elfeed-shown-to-instapaper))
(map! :leader (:prefix-map ("o" . "open")
               :desc "RSS" "e" #'=rss))

(add-to-list 'custom-theme-load-path "~/.doom-themes")
(setq doom-theme nil)
(load-theme 'base16-zjn t)

(set-face-background '+workspace-tab-selected-face (plist-get base16-zjn-colors :base02))
(set-face-foreground '+workspace-tab-selected-face (plist-get base16-zjn-colors :base0D))

(setq zjn--mono "Roboto Mono")
(setq zjn--sans "Bitstream Vera Sans")
(setq zjn--serif "TeX Gyre Pagella")
(setq doom-font (font-spec :family zjn--mono :height 80 :weight 'semi-light))
(setq doom-variable-pitch-font (font-spec :family zjn--serif :height 60))

(setq-default left-margin-width 1
              right-margin-width 1)

(after! projectile
  (setq projectile-project-search-path '("~/git"))
  (defun zjn-projectile-root-for-some-major-modes (_dir)
    (message "%s" major-mode)
    (let ((modes '(mu4e-headers-mode mu4e-main-mode mu4e-view-mode org-agenda-mode)))
      (if (memq major-mode modes) "~/Sync/notes")))
                                        ; (push 'zjn-projectile-root-for-some-major-modes projectile-project-root-files-functions))
  (setq +workspaces-on-switch-project-behavior t)
  )

(after! company
  (setq company-idle-delay 0.2))
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
(setq display-line-numbers-type nil)

(after! shell
  (set-popup-rule! "^\\*shell\\*" :quit nil))

(setq tramp-inline-compress-start-size 1000000)

(after! rustic
  (setq rustic-lsp-server 'rust-analyzer)

  (map! :map (conf-toml-mode-map rustic-mode-map)
        :localleader
        (:prefix ("c" . "cargo")
         :desc "cargo audit"    "a" #'+rust/cargo-audit
         :desc "cargo build"    "b" #'rustic-cargo-build
         :desc "cargo bench"    "B" #'rustic-cargo-bench
         :desc "cargo check"    "c" #'rustic-cargo-check
         :desc "cargo clippy"   "C" #'rustic-cargo-clippy
         :desc "cargo doc"      "d" #'rustic-cargo-doc
         :desc "cargo fmt"      "f" #'rustic-cargo-fmt
         :desc "cargo new"      "n" #'rustic-cargo-new
         :desc "cargo outdated" "o" #'rustic-cargo-outdated
         :desc "cargo run"      "r" #'rustic-cargo-run)
        (:prefix ("t" . "cargo test")
         :desc "all"          "a" #'rustic-cargo-test
         :desc "current test" "t" #'rustic-cargo-current-test)))

(after! lsp-mode
  (push "[/\\\\]\\.hypothesis" lsp-file-watch-ignored)
  (push "[/\\\\]\\venv$" lsp-file-watch-ignored)
  (push "[/\\\\]\\.venv$" lsp-file-watch-ignored))

(after! go
  (require 'inheritenv)
  (inheritenv-add-advice 'call-process-region)
  (inheritenv-add-advice 'gofmt-before-save))

(after! latex
  (add-to-list 'TeX-command-list '("Tectonic" "tectonic --synctex %t" TeX-run-compile nil (latex-mode) :help "Run Tectonic"))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-output-extension "pdf")
  (add-hook! LaTeX-mode
    (setq TeX-command-default "Tectonic"
          TeX-output-extension "pdf")))

(after! pdf-view
  (require 'inheritenv)
  (inheritenv-add-advice 'pdf-annot-print-annotation)
  (defun zjn/save-buffer-no-args () (save-buffer)) ; needed to make args line up
  (advice-add 'pdf-annot-edit-contents-commit :after 'zjn/save-buffer-no-args))
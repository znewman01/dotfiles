;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

; you think we'd know this...needed for "doom" tool to work
(setenv "EMACSDIR" "~/.emacs.d")

; IACR capture!
(require 'json)
(defun zjn--fetch-iacr-info (id)
  (let* ((default-directory "~/git/iacr-dl")
         (blob (car (process-lines "nix-shell" "--run" (concat "python -m iacr " id))))
         (json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type nil))
    (json-read-from-string blob)))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Zachary Newman"
      user-mail-address "z@znewman.net")

(setq bibtex-completion-bibliography "~/Dropbox/notes/lit/default.bib"
      bibtex-completion-library-path "~/Dropbox/notes/lit/"
      bibtex-completion-notes-path "~/Dropbox/notes/roam/bib/")
(after! ivy-bibtex
  (require 'org-roam)
  (org-roam-mode)
  ;; Basic configuration
  (setq reftex-default-bibliography '("~/Dropbox/notes/lit/default.bib")
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
    (let* ((key (first keys))
           (org-roam-find-file-function (lambda (file) (switch-to-buffer (find-file-noselect file) nil 'force-same-window)))
           (pdf (bibtex-completion-find-pdf key)))
      (+workspace-switch key t)
      (delete-other-windows)
      (when pdf
        (let ((pdf-buffer (find-file (first pdf))))
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
           (names (cl-mapcar (lambda (entry) (second entry)) actions))
           (chars (cl-mapcar (lambda (entry) (first entry)) actions))
           (prompt (s-concat (s-join " " names) " "))
           (choice (read-char-choice prompt chars))
           (entry (cl-find-if (lambda (entry) (= (first entry) choice)) actions))
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
    (org-roam-mode 1)  ; for rename-file advice
    (cl-flet ((update (path new-key suffix)
                      (when path
                        (f-join (f-parent path) (concat new-key suffix)))))
      (let* ((old-key (first keys))
             (new-key (read-string (format "New cite key (was %s): " old-key)))
             (old-note (caar (org-roam-db-query
                               [:select file :from refs :where (= ref $s1)]
                               old-key)))
             (new-note (update old-note new-key ".org"))
             (old-pdfs (bibtex-completion-find-pdf old-key))
             (old-pdf (first old-pdfs))
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

(defun zjn--bib-get-key (entry)
  "The key we want to use by default."
  (bibtex-beginning-of-entry)
  (while (save-excursion
           (s-equals? (cdr (assoc-string "=type=" (bibtex-parse-entry)))
                      "proceedings"))
    (bibtex-previous-entry)
    (bibtex-beginning-of-entry))
  (read-string "Key: " (s-replace ":" "" (s-replace ":_" ":" (bibtex-generate-autokey)))))
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

(after! company
  (setq company-idle-delay 0.2))

; Make pretty
(setq doom-theme 'doom-one-light)
(setq zjn--mono "Roboto Mono")
(setq zjn--sans "Bitstream Vera Sans")
(setq zjn--serif "TeX Gyre Pagella")
(setq doom-font (font-spec :family zjn--mono :height 80 :weight 'semi-light))
(setq doom-variable-pitch-font (font-spec :family zjn--serif :height 60))
(setq-default left-margin-width 1
              right-margin-width 1)

(after! latex
  (add-to-list 'TeX-command-list '("Tectonic" "tectonic --synctex %t" TeX-run-compile nil (latex-mode) :help "Run Tectonic"))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-output-extension "pdf")
  (add-hook! LaTeX-mode
    (setq TeX-command-default "Tectonic"
          TeX-output-extension "pdf")))


(setq org-directory "~/notes/")
(map! :leader "a" (cmd! (org-agenda nil "n")))
(after! org
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  (defun org-file (f)
    (concat org-directory "/" f))
  (setq org-agenda-files
        (list (org-file "personal.org")
              (org-file "gtd.org")
              (org-file "inbox.org")  ; Beorg
              (org-file "research/default.org")
              ; (org-file "research/hnp.org")
              ; (org-file "research/radio.org")
              (org-file "research/broadcast.org")
              (org-file "research/accumulators.org")
              (org-file "research/tor.org")
              (org-file "6852-ta.org")
              (org-file "school.org")))
   ; performance
   (setq org-agenda-dim-blocked-tasks nil
         org-agenda-inhibit-startup t
         org-agenda-ignore-properties '(effort appt stat category))
  (defun noop (&rest args) nil)
  (advice-add #'org-font-lock-add-priority-faces :override #'noop)

  (require 'org-starless)
  (require 'org-padding)
  (setq org-padding-block-begin-line-padding '(nil . nil))
  (setq org-padding-block-end-line-padding '(nil . nil))
  (setq org-padding-heading-padding-alist
    '((2.0 . 0.5) (2.0 . 0.5) (2.0 . 0.5) (2.0 . 0.5) (2.0 . 0.5) (2.0 . 0.5) (2.0 . 0.5) (2.0 . 0.5)))

  (require 'org-superstar)
  (remove-hook! org-mode org-superstar-mode)
  (setq org-superstar-item-bullet-alist
  '((?* . ?•)
    (?+ . ?⁘)
    (?- . ?•)))

  (define-minor-mode zjn--org-pretty-mode
    "make org pretty"
    :lighter "pretty"
    (if zjn--org-pretty-mode
        (progn
          (org-starless-mode 1)
          (org-padding-mode 1)
          (org-superstar-mode 1)
          (setq left-margin-width 8
                right-margin-width 8)
          (set-window-buffer (selected-window) (current-buffer))
          (mixed-pitch-mode 1))
      (mixed-pitch-mode -1)
      (org-starless-mode -1)
      (org-padding-mode -1)
      (org-superstar-mode -1)
      (setq left-margin-width (default-value 'left-margin-width)
            right-margin-width (default-value 'right-margin-width))
      (set-window-buffer (selected-window) (current-buffer))))

  (setq org-archive-location "archive/%s::")
  (setq org-default-notes-file (org-file "gtd.org"))
  (setq org-log-done t)
  (setq org-adapt-indentation nil)
  (setq org-ctrl-k-protect-subtree t)
  (setq org-catch-invisible-edits 'show-and-error)
  ; when modifying TODO keywords, update Beorg
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
    (not (string= tag (downcase (format-time-string "%a")))))

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

  (setq org-startup-indented nil)
  (setq org-stuck-projects '("-SOMEDAY/PROJ" ("NEXT" "BUY") nil ""))
  (setq org-agenda-todo-ignore-scheduled 'past)
  (setq org-agenda-todo-ignore-deadlines 'near)
  (setq org-agenda-tags-todo-honor-ignore-options t)
  (setq org-agenda-custom-commands
        `(
          ("n" "Default View"  ; a little faster
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
  (add-hook 'org-agenda-finalize-hook #'org-agenda-delete-empty-blocks)

  (require 'url)
  (require 'f)
  (require 's)
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
  (org-link-set-parameters "cite" :display 'org-link)

  ; https://zzamboni.org/post/beautifying-org-mode-in-emacs/
  (setq org-hide-emphasis-markers t)


  (map! :mode org-capture-mode :localleader "s r" #'org-capture-refile)
  (map! :mode org-mode :n "t" #'org-todo)
  (map! :map org-agenda-mode-map :localleader "." #'counsel-org-goto-all
                                 :localleader "/" #'counsel-org-goto-all)
  (map! :leader "s /" #'counsel-org-goto-all)

  (setq org-startup-folded 'fold)
  (setq org-show-context-detail
        (quote
         ((agenda . ancestors)
          (bookmark-jump . ancestors)
          (isearch . ancestors)
          (default . ancestors))))
  (advice-add 'org-id-new :filter-return #'upcase)

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

  (setq org-preview-latex-default-process 'imagemagick)
  ; (plist-put org-format-latex-options :background "Transparent")
  (setq org-latex-pdf-process '("tectonic %f"))
  (require 'org-fragtog)
  (add-hook 'org-mode-hook 'org-fragtog-mode))



(after! deft
  (setq deft-directory "~/Dropbox/notes/roam"
        deft-recursive t))
(after! org-roam
  (setq org-roam-directory "~/Dropbox/notes/roam"
        org-roam-db-update-method 'immediate
        org-roam-link-auto-replace nil
        org-roam-completion-everywhere nil
        +org-roam-open-buffer-on-find-file nil
        org-roam-tag-sources '(prop last-directory)
        emacsql-sqlite3-executable (executable-find "sqlite3"))
  (add-hook! org-roam-mode (org-roam-bibtex-mode))
  (require 'org-roam-bibtex)

  (setq orb-templates
        '(("r" "ref" plain #'org-roam-capture--get-point "" :file-name "bib/${citekey}" :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n" :unnarrowed t :immediate-finish t)))
  (org-roam-mode)
  (map! :mode org-mode :leader "n r n" #'orb-note-actions)

  (defun zjn/org-roam-find-file-maybe-other-window (&optional arg)
    "Like `function:org-roam-find-file' but when called with prefix open in other-window."
    (interactive "P")
    (let ((org-roam-find-file-function
           (if (and arg (not org-roam-find-file-function))
               #'find-file-other-window
             org-roam-find-file-function)))
      (org-roam-find-file)))
  (map! :leader "n r b" #'zjn/org-roam-find-file-maybe-other-window))

(after! mu4e
  (require 'org-mu4e)
  (setq mu4e-root-maildir "~/Maildir")
  ; Speed up indexing. Run "mu index" every once in a while to reindex.
  (setq
    mu4e-index-cleanup t      ;; don't do a full cleanup check
    mu4e-index-lazy-check nil)

  (setq mail-user-agent 'mu4e-user-agent)
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
  (add-hook 'message-send-hook #'zjn--confirm-empty-subject))

(after! elfeed
  (setq elfeed-feeds
        '(("http://bristolcrypto.blogspot.com/feeds/posts/default")
          ("https://www.schneier.com/blog/atom.xml")
          ("https://www.mattblaze.org/blog/rss20.xml")
          ("https://jvns.ca/atom.xml")
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
          ("https://danluu.com/atom.xml")
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
          ("https://svbn.me/blog/index.xml")
          ("https://www.scottaaronson.com/blog/?feed=rss2")))

  (defun elfeed-show-browse-url ()
    (interactive)
    (browse-url (elfeed-entry-link elfeed-show-entry)))

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

        :mode 'elfeed-show-mode
        :n "I" #'add-elfeed-shown-to-paper-queue-iacr
        :n "o" #'elfeed-show-browse-url
        :n "i" #'add-elfeed-shown-to-instapaper))
(map! :leader (:prefix-map ("o" . "open")
               :desc "RSS" "e" #'=rss))


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

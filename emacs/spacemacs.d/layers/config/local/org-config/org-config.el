(require 'org)
(provide 'org-config)

;; Basic set-up
(setq org-directory "~/notes")
(defun org-file (f)
  (concat org-directory "/" f))
(identity org-agenda-files)
(setq org-agenda-files
      (list (org-file "personal.org")
            (org-file "gtd.org")
            (org-file "research/default.org")
            (org-file "research/hnp.org")
            (org-file "research/radio.org")
            (org-file "research/broadcast.org")
            (org-file "school.org")
            (org-file "6.875/6.875.org")
            (org-file "6.876/6.876.org")))
(setq org-archive-location "archive/%s::")
(setq org-default-notes-file (concat org-directory "gtd.org"))

(setq org-refile-targets '((org-agenda-files :maxlevel . 9)))
(setq org-log-done t)
(setq org-adapt-indentation nil)
(setq org-ctrl-k-protect-subtree t)
(setq org-catch-invisible-edits 'show-and-error)
(add-hook 'auto-save-hook 'org-save-all-org-buffers)

(setq org-todo-keywords
      '((sequence "NEXT(n)" "BUY" "TODO(t)" "|" "DONE(d!)")
        (sequence "HABIT(h)" "|" "HABITDONE(H)")
        (sequence "PROJ(p)" "BLOCKEDPROJ(b)" "|" "PROJDONE(P)")
        (sequence "WAITING(w)" "SOMEDAY(s)" "|" "CANCELLED(c)")))


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

;; Org agendas
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday 6)
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

(defmacro zjn-shopping-list (stores)
  `(,@(mapcar (lambda (store)
                `(tags ,(concat store "-SOMEDAY/BUY")
                       ((org-agenda-overriding-header
                         ,(concat "Shopping list " store ":")))))
              stores)
    (tags ,(concat "-SOMEDAY-"
                   (string-join stores "-")
                   "/|BUY")
          ((org-agenda-overriding-header "Other errands:")))))

(setq org-stuck-projects '("-SOMEDAY/PROJ" ("NEXT" "BUY") nil ""))
(setq org-agenda-todo-ignore-scheduled 'past)
(setq org-agenda-todo-ignore-deadlines 'near) ;; test this with deadlines with manually set window (e.g. <-7d>)
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-agenda-custom-commands
      `(
        ("nn" "Default View"
         ((agenda "" ((org-agenda-span 1)))
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
        ("nw" "Work View"
         ((agenda "" ((org-agenda-span 1)))
          (stuck "")
          (tags-todo "-SOMEDAY-yak/NEXT|BUY"
                ((org-agenda-skip-function 'zjn-org-skip-subtree-if-bad-time)
                 (org-agenda-overriding-header "Tasks:")))
          (tags-todo "-SOMEDAY/PROJ|BLOCKEDPROJ"
                ((org-agenda-overriding-header "All current projects:")))
          (tags-todo "yak-SOMEDAY/NEXT|BUY"
                ((org-agenda-overriding-header "Yak shaving:"))))
         ((org-agenda-tag-filter-preset '("-personal"))))
       ("ns" "Shopping Lists"
        ,(macroexpand '(zjn-shopping-list ("@target" "@grocery" "@hardware" "@wholefoods" "internet"))))
        ))

(setq org-columns-default-format "%Effort %CLOCKSUM %6TODO %100ITEM")
(setq org-show-context-detail
      (quote
       ((agenda . canonical)
        (bookmark-jump . lineage)
        (isearch . lineage)
        (default . ancestors))))


;; org-latex
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("resume"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section{%s}")
               ("\\subsection{%s}" . "\\subsection{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection{%s}")))
(require 'ob-dot)
(setq zjn-org-babel-languages '((emacs-lisp . t)
                                (shell . t)
                                (sqlite . t)
                                (dot . t)
                                (haskell . t)
                                (python . t)))
(org-babel-do-load-languages 'org-babel-load-languages
                             zjn-org-babel-languages)
; (setq org-confirm-babel-evaluate nil)

;; Use tectonic
(setq org-latex-pdf-process '("tectonic %f"))


(require 'evil)
(evil-define-operator evil-LaTeX-fill-and-move (beg end)
  (cl-letf (((symbol-function #'fill-region) #'LaTeX-fill-region))
    (evil-fill-and-move beg end)))

(evil-define-operator evil-LaTeX-fill (beg end)
  (cl-letf (((symbol-function #'fill-region) #'LaTeX-fill-region))
    (evil-fill beg end)))

;; TODO use [remap evil-fill ...] ... instead?
(evil-define-key 'normal LaTeX-mode-map "gq"
  #'evil-LaTeX-fill-and-move)
(evil-define-key 'normal LaTeX-mode-map "gw"
  #'evil-LaTeX-fill)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-to-list 'TeX-command-list
                         '("latexmk" "(run-latexmk)"
                           TeX-run-function nil t :help "Run latexmk") t)
            (setq TeX-command-default "latexmk")))

(require 'org-ref)

; Printing

; TODO: make this a variable. then add a similar hook to the lpr module
(defun zjn--print-file (file)
  (call-process lpr-command nil nil nil "-P" "xerox8/twoside" file))

(defun zjn-org-attach-print (&optional file)
  (interactive)
  (let* ((attach-dir  (org-attach-dir))
         (files (org-attach-file-list attach-dir))
         (file (or file
                   (completing-read "Print attachment: "
                                    (mapcar (lambda (f)
                                              (list (file-name-nondirectory f)))
                                            files)))))
    (setq file (expand-file-name file attach-dir))
    (unless (file-exists-p file)
      (error "No such attachment: %s" file))
    (zjn--print-file file)))
(spacemacs/set-leader-keys-for-major-mode 'org-mode "ep" 'zjn-org-attach-print)

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
         (api-url (format "https://export.arxiv.org/api/query?id_list=%s" id)))
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


; HACK: handle case issue
; https://lists.gnu.org/archive/html/emacs-orgmode/2019-07/msg00081.html
(advice-add 'org-id-new :filter-return #'upcase)


; garbage collection for attachments
(require 'seq)

(defun zjn--dir-files-no-dots (path)
  (seq-filter (lambda (x) (not (member x '("." ".."))))
              (directory-files path)))

(defun zjn--get-all-org-attach-ids (attach-dir)
  (let ((prefixes (zjn--dir-files-no-dots attach-dir)))
    (mapcan (lambda (prefix)
              (let* ((prefix-dir (expand-file-name prefix attach-dir))
                     (remainders (zjn--dir-files-no-dots prefix-dir)))
                (mapcar (lambda (remainder) (cons prefix remainder)) remainders)))
            prefixes)))

(defun zjn--any-files-contain (files value)
  (let ((args `("rg" nil nil nil "-i" ,value ,@files)))
    (eq (apply #'call-process args) 0)))

(defun zjn-org-attach-collect-garbage ()
  (interactive)
  (error "do not call me until you handle archive/ and 6.875/ structure!")
  (let* ((default-directory org-directory)
         (org-files (seq-filter (lambda (f) (or (string-suffix-p ".org" f)
                                                (string-suffix-p ".org_archive" f)))
                                (directory-files org-directory)))
         (attach-dir (expand-file-name org-attach-directory org-directory))
         (org-attach-ids (zjn--get-all-org-attach-ids attach-dir))
         (unused-ids
          (seq-filter (lambda (id)
                        (not (zjn--any-files-contain
                              org-files
                              (concat (car id) (cdr id)))))
                        org-attach-ids)))
    ; Delete non-referenced data directories
    (y-or-n-p (format "Delete %d directories?" (length unused-ids)))
    (mapcar (lambda (id)
              (let ((dir-abs (expand-file-name (cdr id)
                                               (expand-file-name (car id) "data"))))
                (mapcar (lambda (f) (delete-file (expand-file-name f dir-abs)))
                        (zjn--dir-files-no-dots dir-abs))
                (delete-directory dir-abs)))
            unused-ids)
    ; And any now-empty data directories
    (mapcar (lambda (dir)
              (let ((abs-dir (expand-file-name dir attach-dir)))
                (if (not (zjn--dir-files-no-dots abs-dir))
                    (delete-directory abs-dir))))
            (zjn--dir-files-no-dots attach-dir))))

;; -*- lexical-binding:t -*-

(require 'elfeed)
(provide 'elfeed-config)

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
        ("https://blog.acolyer.org/feed/")
        ("https://www.iacr.org/news/rss")
        ("https://slatestarcodex.com/feed/")
        ("http://www.christianmoscardi.com/feed.xml")
        ("https://mass.streetsblog.org/feed/")
        ("http://scholars-stage.blogspot.com/feeds/posts/default")
        ("https://www.scottaaronson.com/blog/?feed=rss2")))

; Instapaper + Elfeed
(require 'request)

(defun add-to-instapaper (url success-callback)
  (let* ((username "znewman01@gmail.com")
         (password (car (process-lines "pass" "show" "instapaper")))
         (auth-string (base64-encode-string (format "%s:%s" username password)))
         (auth-header (format "Basic %s" auth-string)))
    (request "https://www.instapaper.com/api/add"
             :headers `(("Authorization" . ,auth-header))
             :params `(("url" . ,url))
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
                    (elfeed-search-update-entry entry))))
    (unless (use-region-p) (forward-line))))

(define-key elfeed-search-mode-map "i" #'add-elfeed-entry-to-instapaper)

(defun add-elfeed-shown-to-instapaper ()
  (interactive)
  (add-to-instapaper
   (elfeed-entry-link elfeed-show-entry)
   (cl-function (lambda (&key data &allow-other-keys)
                  (message "Added to Instapaper!")))))
(define-key elfeed-show-mode-map "i" #'add-elfeed-shown-to-instapaper)

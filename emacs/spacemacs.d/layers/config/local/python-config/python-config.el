;; -*- lexical-binding:t -*-

(provide 'python-config)

(require 'nose)

(defun nose2-command ()
  (identity "nose2"))

(setq nose2-use-verbose nil)
(setq python-test-runner 'nose2)

(defun nose2-find-project-root ()
  (nose-find-project-root))

(defun run-nose2 (&optional tests)
  (let* ((nose2 (nose2-command))
         (where (nose2-find-project-root))
         (tnames (or tests "")))
    (if (not where)
        (error
         (format (concat "abort: nosemacs couldn't find a project root, "
                         "looked for any of %S") nose-project-root-files)))
    (funcall '(lambda (command)
                (compilation-start command
                                   nil
                                   (lambda (mode) (concat "*nose2*"))))
             (format
              (concat "%s "
                      (if nose2-use-verbose "--verbose " "")
                      "--top-level-directory %s %s")
              nose2 where tnames))))


(defun nose2-all ()
  "run all tests"
  (interactive)
  (run-nose2))

(require 'subr-x)
(defun zjn--get-relative-module-path (module-file-name)
  (replace-regexp-in-string
   "/" "." (string-remove-suffix
            ".py" (file-relative-name buffer-file-name module-file-name))))


(defun nose2-module ()
  (interactive)
  (let ((tests (zjn--get-relative-module-path (nose2-find-project-root))))
    (run-nose2 tests)))

(defun nose2-one ()
  (interactive)
  (let* ((module (zjn--get-relative-module-path (nose2-find-project-root)))
         (tests (format "%s.%s" module (nose-py-testable))))
    (run-nose2 tests)))




(defun spacemacs/python-test-all (arg)
  "Run all tests."
  (interactive "P")
  (spacemacs//python-call-correct-test-function arg '((pytest . pytest-all)
                                           (nose . nosetests-all)
                                           (nose2 . nose2-all))))


; 
; (defun spacemacs/python-test-pdb-all (arg)
;   "Run all tests in debug mode."
;   (interactive "P")
;   (spacemacs//python-call-correct-test-function arg '((pytest . pytest-pdb-all)
;                                            (nose . nosetests-pdb-all))))

(defun spacemacs/python-test-module (arg)
  "Run all tests in the current module."
  (interactive "P")
  (spacemacs//python-call-correct-test-function arg '((pytest . pytest-module)
                                           (nose . nosetests-module)
                                           (nose2 . nose2-module))))

; (defun spacemacs/python-test-pdb-module (arg)
;   "Run all tests in the current module in debug mode."
;   (interactive "P")
;   (spacemacs//python-call-correct-test-function
;    arg
;    '((pytest . pytest-pdb-module)
;      (nose . nosetests-pdb-module))))
; 
; (defun spacemacs/python-test-suite (arg)
;   "Run all tests in the current suite."
;   (interactive "P")
;   (spacemacs//python-call-correct-test-function arg '((nose . nosetests-suite))))
; 
; (defun spacemacs/python-test-pdb-suite (arg)
;   "Run all tests in the current suite in debug mode."
;   (interactive "P")
;   (spacemacs//python-call-correct-test-function arg '((nose . nosetests-pdb-suite))))

(defun spacemacs/python-test-one (arg)
  "Run current test."
  (interactive "P")
  (spacemacs//python-call-correct-test-function arg '((pytest . pytest-one)
                                           (nose . nosetests-one)
                                           (nose2 . nose2-one))))

; (defun spacemacs/python-test-pdb-one (arg)
;   "Run current test in debug mode."
;   (interactive "P")
;   (spacemacs//python-call-correct-test-function arg '((pytest . pytest-pdb-one)
;                                            (nose . nosetests-pdb-one))))

(setq config-packages
  '(
    (org-config      :location local)
    (mu4e-config      :location local)
    (python-config      :location local)
    (elfeed-config      :location local)))

(defun config/init-org-config ()
  (use-package org-config
    :after org))

(defun config/init-mu4e-config ()
  (use-package mu4e-config
    :after mu4e))

(defun config/init-python-config ()
  (use-package python-config
    :after python))

(defun config/init-elfeed-config ()
  (use-package elfeed-config
    :after elfeed))

;;; configuration-tests.el --- Configuration regression checks -*- lexical-binding: t -*-

;; Run with: emacs -Q --batch -l .emacs.d/tests/configuration-tests.el
;;           -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'package)
(package-initialize)
(require 'use-package)
(require 'org)
(require 'org-capture)
(require 'recentf)

(defvar qu-config-test/init-file
  (or (getenv "EMACS_INIT_UNDER_TEST")
      (expand-file-name "../init.el" (file-name-directory load-file-name))))

(defvar qu-config-test/forms
  (with-temp-buffer
    (insert-file-contents qu-config-test/init-file)
    (check-parens)
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file (nreverse forms))))))

;; Load only the declarations under test, without installing packages or
;; starting the user's other modes and services.
(let ((use-package-ensure-function #'ignore))
  (dolist (form qu-config-test/forms)
    (when (or (and (eq (car-safe form) 'setq)
                   (memq (cadr form) '(history-length recentf-max-saved-items)))
              (and (eq (car-safe form) 'global-set-key)
                   (memq 'comment-or-uncomment-region (flatten-tree form)))
              (and (eq (car-safe form) 'use-package)
                   (memq (cadr form) '(consult embark org org-download))))
      (eval form t))))

(defvar qu-config-test/embark-lazy (not (featurep 'embark)))

(ert-deftest qu-config-test/embark-bindings-bootstrap ()
  (should qu-config-test/embark-lazy)
  (dolist (binding '(("C-." . embark-act)
                     ("C-;" . embark-dwim)
                     ("C-'" . embark-act-all)
                     ("C-c ." . embark-act)
                     ("C-c ;" . embark-dwim)
                     ("C-c C-'" . embark-act-all)))
    (should (eq (key-binding (kbd (car binding))) (cdr binding)))))

(ert-deftest qu-config-test/comment-binding-preserved ()
  (should (eq (key-binding (kbd "C-c '")) 'comment-or-uncomment-region)))

(ert-deftest qu-config-test/project-search-uses-ripgrep ()
  (should (eq (key-binding (kbd "M-s g")) 'consult-ripgrep))
  (should (eq (key-binding (kbd "M-s r")) 'consult-ripgrep))
  (should (eq (key-binding (kbd "M-s G")) 'consult-git-grep)))

(ert-deftest qu-config-test/history-retention ()
  (should (= history-length 200))
  (should (= recentf-max-saved-items 300)))

(ert-deftest qu-config-test/idea-capture-starts-in-body ()
  (let* ((file (make-temp-file "emacs-capture-check-" nil ".org"))
         (org-capture-templates (copy-tree org-capture-templates))
         (template (assoc "i" org-capture-templates)))
    (setf (nth 3 template) (list 'file file))
    (save-window-excursion
      (unwind-protect
          (progn
            (org-capture nil "i")
            (should (> (line-number-at-pos) 1))
            (should (string-blank-p
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))
        (when (bound-and-true-p org-capture-mode) (org-capture-kill))
        (when-let* ((buffer (get-file-buffer file)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (delete-file file)))))

(ert-deftest qu-config-test/common-lisp-babel-removed ()
  (should-not (assq 'lisp org-babel-load-languages))
  (dolist (language '(shell python makefile C))
    (should (assq language org-babel-load-languages))))

(ert-deftest qu-config-test/refile-does-not-save-unrelated-buffers ()
  (let ((saves 0))
    (cl-letf (((symbol-function 'org-save-all-org-buffers)
               (lambda () (cl-incf saves))))
      (org-refile 0))
    (should (= saves 0))))

(ert-deftest qu-config-test/latex-default-packages-not-duplicated ()
  (require 'ox-latex)
  (let ((output (org-export-string-as "#+title: Test\n* Heading\nBody\n" 'latex)))
    (dolist (package '("fontenc" "amsmath"))
      (with-temp-buffer
        (insert output)
        (should (= 1 (how-many
                      (concat "^\\\\usepackage\\(?:\\[[^]]*\\]\\)?{" package "}")
                      (point-min) (point-max))))))))

;;; configuration-tests.el ends here

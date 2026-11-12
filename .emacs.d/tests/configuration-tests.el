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
(require 'doom-modeline)

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
              (and (eq (car-safe form) 'defun)
                   (memq (cadr form) '(qu/first-available qu/font-setup)))
              (and (eq (car-safe form) 'global-set-key)
                   (memq 'comment-or-uncomment-region (flatten-tree form)))
              (and (eq (car-safe form) 'use-package)
                   (memq (cadr form) '(consult embark org org-download doom-modeline))))
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

(ert-deftest qu-config-test/body-font-is-light-with-size-preserved ()
  (let (requested-font)
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'qu/first-available) (lambda (&rest fonts) (car fonts)))
              ((symbol-function 'set-frame-font)
               (lambda (font &rest _) (setq requested-font font)))
              ((symbol-function 'set-face-attribute) #'ignore)
              ((symbol-function 'set-fontset-font) #'ignore))
      (qu/font-setup))
    (should (equal (font-get requested-font :weight)
                   (font-get (font-spec :weight 'light) :weight)))
    (should (= (font-get requested-font :size) 15.0))))

(ert-deftest qu-config-test/secondary-org-headings-use-body-color ()
  (dolist (face '(org-level-2 org-level-3 org-level-4 org-level-5
                 org-level-6 org-level-7 org-level-8))
    (should (eq (face-attribute face :inherit) 'default))
    (should (eq (face-attribute face :foreground) 'unspecified))))

(ert-deftest qu-config-test/org-auxiliary-faces-share-existing-styles ()
  (dolist (face '(org-table org-meta-line))
    (should (eq (face-attribute face :inherit) 'shadow)))
  (dolist (face '(org-block-begin-line org-block-end-line))
    (should (eq (face-attribute face :inherit) 'org-meta-line))))

(ert-deftest qu-config-test/modeline-omits-major-mode-icon ()
  (should-not doom-modeline-major-mode-icon))

(ert-deftest qu-config-test/frame-padding-preserves-transparency ()
  (let (default-frame-alist)
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "early-init.el" (file-name-directory qu-config-test/init-file)))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (eq (car-safe form) 'add-to-list)
                         (equal (cadr form) '(quote default-frame-alist)))
                (eval form t))))
        (end-of-file nil)))
    (should (equal (alist-get 'internal-border-width default-frame-alist) 8))
    (should (= (alist-get 'alpha-background default-frame-alist) 85))))

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

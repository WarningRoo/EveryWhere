;;; project-navigation-tests.el --- Project navigation checks -*- lexical-binding: t -*-

;; Run with: emacs -Q --batch -l .emacs.d/tests/project-navigation-tests.el
;;           -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'package)
(package-initialize)
(require 'use-package)
(require 'project)

(defvar qu-test/init-file
  (or (getenv "EMACS_INIT_UNDER_TEST")
      (expand-file-name "../init.el" (file-name-directory load-file-name))))

(defvar qu-test/state-directory (make-temp-file "emacs-navigation-state-" t))
(setq user-emacs-directory (file-name-as-directory qu-test/state-directory))
(add-hook 'kill-emacs-hook
          (lambda () (delete-directory qu-test/state-directory t)) t)

;; Evaluate the relevant declarations, without running unrelated user setup.
(let ((use-package-ensure-function #'ignore))
  (with-temp-buffer
    (insert-file-contents qu-test/init-file)
    (let (form)
      (condition-case nil
          (while t
            (setq form (read (current-buffer)))
            (when (and (eq (car-safe form) 'use-package)
                       (memq (cadr form) '(project treemacs dirvish consult tab-line)))
              (eval form t)))
        (end-of-file nil)))))

(defvar qu-test/treemacs-deferred (not (featurep 'treemacs)))

(ert-deftest qu-test/sidebar-binding ()
  (should (eq (key-binding (kbd "C-c d")) 'treemacs)))

(ert-deftest qu-test/treemacs-loads-on-demand ()
  (should qu-test/treemacs-deferred))

(ert-deftest qu-test/native-project-menu ()
  (should (equal
           (assq 'treemacs-add-and-display-current-project-exclusively
                 project-switch-commands)
           '(treemacs-add-and-display-current-project-exclusively "File tree" ?t)))
  (should (assq 'project-find-file project-switch-commands)))

(ert-deftest qu-test/existing-navigation-bindings ()
  (should (eq (key-binding (kbd "C-x p p")) 'project-switch-project))
  (should (eq (key-binding (kbd "C-x p f")) 'project-find-file))
  (should (eq (key-binding (kbd "C-x t t")) 'other-tab-prefix))
  (should (eq (key-binding (kbd "C-x p b")) 'consult-project-buffer))
  (should (eq (key-binding (kbd "M-s r")) 'consult-ripgrep))
  (should (eq (lookup-key tab-line-mode-map (kbd "C-<prior>"))
              'tab-line-switch-to-prev-tab))
  (should (eq (lookup-key tab-line-mode-map (kbd "C-<next>"))
              'tab-line-switch-to-next-tab)))

(ert-deftest qu-test/tab-line-label-padding ()
  (dolist (name '("main.c" "a_long_filename_that_needs_truncation.h"))
    (with-temp-buffer
      (rename-buffer name t)
      (let ((label (funcall tab-line-tab-name-function (current-buffer))))
        (should (string-prefix-p " " label))
        (should (string-suffix-p " " label))
        (should (<= (string-width (string-trim label)) 24))))))

(ert-deftest qu-test/tab-line-separator-and-faces ()
  (should (stringp tab-line-separator))
  (should (equal (get-text-property 0 'display tab-line-separator)
                 '(space :width (1))))
  (should-not (equal (face-attribute 'tab-line :background)
                     (face-attribute 'tab-line-tab-inactive :background)))
  (should-not (equal (face-attribute 'tab-line-tab-current :background)
                     (face-attribute 'tab-line-tab-inactive :background))))

(ert-deftest qu-test/tab-line-background-colored-box-padding ()
  (dolist (face '(tab-line-tab tab-line-tab-inactive
                  tab-line-tab-current tab-line-highlight))
    (let ((box (face-attribute face :box nil t)))
      (should (listp box))
      (should (= (or (plist-get box :line-width) 0) 5))
      (should (equal (plist-get box :color)
                     (face-attribute face :background nil t)))
      (should-not (plist-get box :style)))))

(ert-deftest qu-test/tab-line-keyboard-switching ()
  (let* ((first (generate-new-buffer "tab-first.txt"))
         (second (generate-new-buffer "tab-second.txt"))
         (tab-line-tabs-function (lambda () (list first second))))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer first)
          (tab-line-mode 1)
          (should (eq (key-binding (kbd "C-<next>"))
                      'tab-line-switch-to-next-tab))
          (call-interactively (key-binding (kbd "C-<next>")))
          (should (eq (window-buffer) second))
          (tab-line-mode 1)
          (call-interactively (key-binding (kbd "C-<prior>")))
          (should (eq (window-buffer) first)))
      (kill-buffer first)
      (kill-buffer second))))

(defmacro qu-test/with-projects (&rest body)
  (declare (indent 0))
  `(let* ((sandbox (make-temp-file "emacs-project-navigation-" t))
          (root-a (expand-file-name "a/" sandbox))
          (root-b (expand-file-name "b/" sandbox))
          (loose (expand-file-name "loose/" sandbox)))
     (unwind-protect
         (progn
           (dolist (root (list root-a root-b loose))
             (make-directory (expand-file-name "src/" root) t))
           (dolist (root (list root-a root-b))
             (write-region "" nil (expand-file-name ".project" root) nil 'silent)
             (write-region "sample\n" nil (expand-file-name "src/sample.txt" root)
                           nil 'silent))
           ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (file-in-directory-p file sandbox)
             (kill-buffer buffer))))
       (delete-directory sandbox t))))

(ert-deftest qu-test/non-vc-project-root-and-files ()
  (qu-test/with-projects
    (let* ((default-directory (expand-file-name "src/" root-a))
           (project (project-current nil)))
      (should project)
      (should (file-equal-p root-a (project-root project)))
      (should (member (expand-file-name "src/sample.txt" root-a)
                      (project-files project))))))

(ert-deftest qu-test/git-project-root ()
  (skip-unless (executable-find "git"))
  (qu-test/with-projects
    (delete-file (expand-file-name ".project" root-a))
    (let ((default-directory root-a))
      (should (zerop (call-process "git" nil nil nil "init" "--quiet")))
      (let ((default-directory (expand-file-name "src/" root-a)))
        (should (file-equal-p root-a (project-root (project-current nil))))))))

(ert-deftest qu-test/consult-follows-current-project ()
  (require 'consult)
  (qu-test/with-projects
    (dolist (root (list root-a root-b))
      (with-temp-buffer
        (setq default-directory (expand-file-name "src/" root))
        (let ((detected-root (funcall consult-project-function nil)))
          (should detected-root)
          (should (file-equal-p root detected-root)))))
    (with-temp-buffer
      (setq default-directory loose)
      (should-not (project-current nil))
      (should-not (funcall consult-project-function nil)))))

(ert-deftest qu-test/treemacs-public-mouse-action ()
  (require 'treemacs)
  (should (eq (lookup-key treemacs-mode-map [mouse-1])
              'treemacs-single-click-expand-action))
  (should (eq (lookup-key treemacs-mode-map (kbd "TAB")) 'treemacs-TAB-action))
  (should (eq (lookup-key treemacs-mode-map (kbd "RET")) 'treemacs-RET-action)))

(ert-deftest qu-test/treemacs-native-following ()
  (require 'treemacs)
  (should (bound-and-true-p treemacs-project-follow-mode))
  (should treemacs-follow-mode)
  (should treemacs-filewatch-mode)
  (when (executable-find "git")
    (should treemacs-git-mode)
    (should (eq treemacs--git-mode 'simple)))
  (should (= treemacs-collapse-dirs 0))
  (should-not (bound-and-true-p dirvish-side-follow-mode)))

(ert-deftest qu-test/dirvish-file-manager-preserved ()
  (require 'dirvish)
  (should dirvish-override-dired-mode)
  (should (eq (lookup-key dirvish-mode-map [mouse-2]) 'qu/dirvish-mouse-open))
  (should (commandp 'dired-jump)))

(ert-deftest qu-test/treemacs-opens-without-extra-splits ()
  (require 'treemacs)
  (qu-test/with-projects
    (let ((treemacs-persist-file (expand-file-name "treemacs-persist" sandbox))
          (project-list-file (expand-file-name "projects" sandbox))
          (default-directory root-a))
      (save-window-excursion
        (unwind-protect
            (progn
              (switch-to-buffer (get-buffer-create "*navigation-test*"))
              (setq default-directory root-a)
              (call-interactively
               #'treemacs-add-and-display-current-project-exclusively)
              (should (= (length (window-list)) 2))
              ;; Reopening a project's tree must not toggle it off.
              (call-interactively
               #'treemacs-add-and-display-current-project-exclusively)
              (should (= (length (window-list)) 2))
              (with-selected-window (treemacs-get-local-window)
                (treemacs-goto-file-node (expand-file-name "src" root-a))
                (funcall (lookup-key treemacs-mode-map [mouse-1])
                         (list 'mouse-1 (list (selected-window) (point) '(0 . 0) 0)))
                (should (= (length (window-list)) 2))
                (treemacs-goto-file-node
                 (expand-file-name "src/sample.txt" root-a))
                (funcall (lookup-key treemacs-mode-map [mouse-1])
                         (list 'mouse-1 (list (selected-window) (point) '(0 . 0) 0))))
              (should (= (length (window-list)) 2))
              (should (get-buffer-window
                       (get-file-buffer (expand-file-name "src/sample.txt" root-a))))
              (with-selected-window (treemacs-get-local-window)
                (treemacs-goto-file-node
                 (expand-file-name "src/sample.txt" root-a))
                (treemacs-RET-action))
              (should (= (length (window-list)) 2))
              (let ((project-switch-commands
                     'treemacs-add-and-display-current-project-exclusively))
                (project-switch-project root-b))
              (should (treemacs-get-local-window))
              (should (= (length (window-list)) 2))
              (should (equal
                       (mapcar #'treemacs-project->path
                               (treemacs-workspace->projects (treemacs-current-workspace)))
                       (list (directory-file-name root-b)))))
          (when (treemacs-get-local-window)
            (treemacs-quit))
          (when-let* ((buffer (treemacs-get-local-buffer)))
            (kill-buffer buffer)))))))

;;; project-navigation-tests.el ends here

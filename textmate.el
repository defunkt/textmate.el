;; textmate.el --- TextMate minor mode for Emacs

;; Copyright (C) 2008 Chris Wanstrath <chris@ozmm.org>

;; Licensed under the same terms as Emacs.

;; Version: 0.1.0
;; Keywords: textmate osx mac
;; Created: 22 Nov 2008
;; Author: Chris Wanstrath <chris@ozmm.org>

;; This file is NOT part of GNU Emacs.

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; This minor mode exists to mimick TextMate's awesome
;; features. 

;;    ⌘T - Go to File
;;  ⇧⌘T - Go to Symbol
;;    ⌘L - Go to Line
;;    ⌘/ - Comment Line (or Selection/Region)
;;    ⌘] - Shift Right (currently indents region)
;;    ⌘[ - Shift Left  (not yet implemented)
;;  ⌥⌘] - Align Assignments
;;  ⌥⌘[ - Indent Line
;;  ⌘RET - Insert Newline at Line's End
;;  ⌥⌘T - Reset File Cache (for Go to File)

;; A "project" in textmate-mode is determined by the presence of
;; a .git directory. If no .git directory is found in your current
;; directory, textmate-mode will traverse upwards until one (or none)
;; is found. The directory housing the .git directory is presumed
;; to be the project's root.

;; In other words, calling Go to File from 
;; ~/Projects/fieldrunners/app/views/towers/show.html.erb will use
;; ~/Projects/fieldrunners/ as the root if ~/Projects/fieldrunners/.git
;; exists.

;;; Installation

;; $ cd ~/.emacs.d/vendor
;; $ git clone git://github.com/defunkt/textmate.el.git
;;
;; In your emacs config:
;;
;; (add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
;; (require 'textmate)
;; (textmate-mode)

;;; Minor mode

(defvar textmate-use-file-cache t
  "* Should `textmate-goto-file' keep a local cache of files?")
(defvar *textmate-project-root* nil)
(defvar *textmate-project-files* '())
(defvar *textmate-gf-exclude* "vendor\\|fixtures\\|tmp\\|log\\|\\(.*\\.\\(nib\\|framework\\|app\\|pbproj\\|pbxproj\\|xcode\\(proj\\)?\\|bundle\\)$\\)")
(defvar textmate-completing-library 'ido "The library `textmade-goto-symbol' and `textmate-goto-file' should use for completing filenames and symbols (`ido' by default)")
(defvar textmate-completing-function-alist '((ido ido-completing-read) (icicles  icicles-completing-read)))

(defvar textmate-mode-map (make-sparse-keymap))

;;; Bindings

(defun textmate-ido-fix ()
  "Add up/down keybindings for ido."
  (define-key ido-completion-map [up] 'ido-prev-match)
  (define-key ido-completion-map [down] 'ido-next-match))

(defun textmate-bind-keys ()
  (add-hook 'ido-setup-hook 'textmate-ido-fix)
  (if (boundp 'aquamacs-version) 
      (textmate-bind-aquamacs-keys)
    (textmate-bind-carbon-keys)))

(defun textmate-bind-aquamacs-keys ()
  (define-key textmate-mode-map [A-return] 'textmate-next-line)
  (define-key textmate-mode-map (kbd "A-M-t") 'textmate-clear-cache)
  (define-key textmate-mode-map (kbd "A-M-]") 'align)
  (define-key textmate-mode-map (kbd "A-M-[") 'indent-according-to-mode)
  (define-key textmate-mode-map (kbd "A-]") 'indent-region)
  (define-key textmate-mode-map (kbd "A-/") 'comment-region-or-line)
  (define-key osx-key-mode-map (kbd "A-t") 'textmate-goto-file)     ;; Need `osx-key-mode-map' to override 
  (define-key osx-key-mode-map (kbd "A-T") 'textmate-goto-symbol))  ;; Aquamacs menu item key bindings.

(defun textmate-bind-carbon-keys ()
  ;; Are these any good? Anyone have good Carbon defaults?
  (define-key textmate-mode-map [M-return] 'textmate-next-line)
;  (define-key textmate-mode-map (kbd "A-M-t") 'textmate-clear-cache)  
;  (define-key textmate-mode-map [(meta ])] 'align)
;  (define-key textmate-mode-map (kbd "A-M-[") 'indent-according-to-mode)
;  (define-key textmate-mode-map (kbd "A-/") 'comment-region-or-line)  
  (define-key textmate-mode-map [(control tab)] 'indent-region)
  (define-key textmate-mode-map [(meta t)] 'textmate-goto-file)
  (define-key textmate-mode-map [(meta T)] 'textmate-goto-symbol))

(defun textmate-completing-read (&rest args)
  (let ((reading-fn (cadr (assoc textmate-completing-library textmate-completing-function-alist))))
  (apply (symbol-function reading-fn) args)))

;;; Commands

(defun textmate-next-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

;; http://chopmo.blogspot.com/2008/09/quickly-jumping-to-symbols.html
(defun textmate-goto-symbol ()
  "Will update the imenu index and then use ido to select a symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))
                              
                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))
                              
                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))
                             
                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (textmate-completing-read "Symbol: " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(defun textmate-goto-file (&optional starting)
  (interactive)
  (textmate-find-project-root)
  (find-file (concat (expand-file-name *textmate-project-root*) "/"
                     (textmate-completing-read "Find file: " 
                                          (or (textmate-cached-project-files) 
                                              (textmate-cache-project-files *textmate-project-root*))))))

(defun textmate-clear-cache ()
  (interactive)
  (setq *textmate-project-root* nil)
  (setq *textmate-project-files* nil)
  (message "textmate-mode cache cleared."))

;;; Utilities

(defun textmate-project-files (root)
  (split-string 
   (shell-command-to-string 
    (concat 
     "find " 
     root
     " -type f  | grep -vE '"
     *textmate-gf-exclude*
     "' | sed 's:"
     *textmate-project-root* 
     "/::'")) "\n" t))

(defun textmate-cache-project-files (root)
  (let ((files (textmate-project-files root)))
    (setq *textmate-project-files* `(,root . ,files))
    files))

(defun textmate-cached-project-files ()
  (when (and 
         textmate-use-file-cache
         (not (null *textmate-project-files*))
         (equal *textmate-project-root* (car *textmate-project-files*)))
    (cdr *textmate-project-files*)))

(defun textmate-find-project-root ()
  (when (or (null *textmate-project-root*) (not (string-match default-directory *textmate-project-root*)))
    (setq *textmate-project-root* (expand-file-name (concat (textmate-project-root) "/")))))

(defun textmate-project-root (&optional root)
  (when (null root) (setq root default-directory))
  (cond
   ((member ".git" (directory-files root)) (expand-file-name root))
   ((equal (expand-file-name root) "/") (error "Can't find any .git directory"))
   (t (textmate-project-root (concat (file-name-as-directory root) "..")))))

;;;###autoload
(define-minor-mode textmate-mode "TextMate Emulation Minor Mode"
  :lighter " mate" :global t :keymap textmate-mode-map
  (textmate-bind-keys)
  ;; prefer icicles to ido
  (if (fboundp 'icy-mode)
      (progn
	(icy-mode t)
	(setq textmate-completing-library 'icicles))
    (progn (ido-mode t)
	   (setq ido-enable-flex-matching t))))

(provide 'textmate)
;;; textmate.el ends here

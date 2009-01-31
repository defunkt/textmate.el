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
;;    ⌘] - Shift Right
;;    ⌘[ - Shift Left
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

;;; Depends on imenu
(require 'imenu)

;;; Minor mode

(defvar textmate-use-file-cache t
  "* Should `textmate-goto-file' keep a local cache of files?")

(defvar textmate-completing-library 'ido 
  "The library `textmade-goto-symbol' and `textmate-goto-file' should use for completing filenames and symbols (`ido' by default)")

(defvar *textmate-completing-function-alist* '((ido ido-completing-read) 
                                               (icicles  icicle-completing-read) 
                                               (none completing-read)) 
  "The function to call to read file names and symbols from the user")

(defvar *textmate-completing-minor-mode-alist* 
  `((ido ,(lambda (a) (progn (ido-mode a) (setq ido-enable-flex-matching t)))) 
    (icicles ,(lambda (a) (icy-mode a))) 
    (none ,(lambda (a) ())))
  "The list of functions to enable and disable completing minor modes")

(defvar *textmate-mode-map* (make-sparse-keymap))
(defvar *textmate-project-root* nil)
(defvar *textmate-project-files* '())
(defvar *textmate-gf-exclude* 
  "/\\.|vendor|fixtures|tmp|log|build|\\.xcodeproj|\\.nib|\\.framework|\\.app|\\.pbproj|\\.pbxproj|\\.xcode|\\.xcodeproj|\\.bundle")

(defvar *textmate-keybindings-list* `((textmate-next-line 
                                     [A-return]    [M-return])
                                     (textmate-clear-cache 
                                      ,(kbd "A-M-t") [(control c)(control t)])
                                     (align 
                                      ,(kbd "A-M-]") [(control c)(control a)])
                                     (indent-according-to-mode 
                                      ,(kbd "A-M-[") nil)
                                     (textmate-shift-right
                                      ,(kbd "A-]")   [(control tab)])
                                     (textmate-shift-left
                                      ,(kbd "A-[")   [(control shift tab)])
                                     (comment-or-uncomment-region-or-line 
                                      ,(kbd "A-/")   [(control c)(control k)])
                                     (textmate-goto-file 
                                      ,(kbd "A-t")   [(meta t)])
                                     (textmate-goto-symbol 
                                      ,(kbd "A-T")   [(meta T)])))

;;; Bindings

(defun textmate-ido-fix ()
  "Add up/down keybindings for ido."
  (define-key ido-completion-map [up] 'ido-prev-match)
  (define-key ido-completion-map [down] 'ido-next-match))

(defun textmate-bind-keys ()
  (add-hook 'ido-setup-hook 'textmate-ido-fix)

  ; weakness until i figure out how to do this right
  (when (boundp 'osx-key-mode-map)
    (define-key osx-key-mode-map (kbd "A-t") 'textmate-goto-file)
    (define-key osx-key-mode-map (kbd "A-T") 'textmate-goto-symbol)) 
 
  (let ((member) (i 0) (access (if (boundp 'aquamacs-version) 'cadr 'caddr)))
    (setq member (nth i *textmate-keybindings-list*))
    (while member
      (if (funcall access member)
       (define-key *textmate-mode-map* (funcall access member) (car member)))
      (setq member (nth i *textmate-keybindings-list*))
      (setq i (+ i 1)))))

(defun textmate-completing-read (&rest args)
  (let ((reading-fn (cadr (assoc textmate-completing-library *textmate-completing-function-alist*))))
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

(defun textmate-goto-file ()
  (interactive)
  (let ((root (textmate-project-root)))
    (when (null root) 
      (error "Can't find any .git directory"))
    (find-file 
     (concat 
      (expand-file-name root) "/"
      (textmate-completing-read 
       "Find file: "
       (textmate-cached-project-files root))))))

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

(defun textmate-cached-project-files (&optional root)
  (cond
   ((null textmate-use-file-cache) (textmate-project-files root))
   ((equal (textmate-project-root) (car *textmate-project-files*))
    (cdr *textmate-project-files*))
   (t (cdr (setq *textmate-project-files* 
                 `(,root . ,(textmate-project-files root)))))))

(defun textmate-project-root ()
  (when (or 
         (null *textmate-project-root*) 
         (not (string-match *textmate-project-root* default-directory)))
    (let ((root (textmate-find-project-root)))
      (if root
          (setq *textmate-project-root* (expand-file-name (concat root "/")))
        (setq *textmate-project-root* nil))))
  *textmate-project-root*)

(defun textmate-find-project-root (&optional root)
  (when (null root) (setq root default-directory))
  (cond
   ((member ".git" (directory-files root)) (expand-file-name root))
   ((equal (expand-file-name root) "/") nil)
   (t (textmate-find-project-root (concat (file-name-as-directory root) "..")))))

(defun textmate-shift-right (&optional arg)
  "Shift the line or region to the ARG places to the right.

A place is considered `tab-width' character columns."
  (interactive)
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (indent-rigidly beg end (* (or arg 1) tab-width))))

(defun textmate-shift-left (&optional arg)
  "Shift the line or region to the ARG places to the left."
  (interactive)
  (textmate-shift-right (* -1 (or arg 1))))

;;;###autoload
(define-minor-mode textmate-mode "TextMate Emulation Minor Mode"
  :lighter " mate" :global t :keymap *textmate-mode-map*
  (textmate-bind-keys)
  ; activate preferred completion library
  (dolist (mode *textmate-completing-minor-mode-alist*)
    (if (eq (car mode) textmate-completing-library)
        (funcall (cadr mode) t)
      (when (fboundp 
             (cadr (assoc (car mode) *textmate-completing-function-alist*)))
        (funcall (cadr mode) -1)))))

(provide 'textmate)
;;; textmate.el ends here

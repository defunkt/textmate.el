;;; textmate.el --- TextMate minor mode for Emacs

;; Copyright (C) 2008, 2009 Chris Wanstrath <chris@ozmm.org>

;; Licensed under the same terms as Emacs.

;; Keywords: textmate osx mac
;; Created: 22 Nov 2008
;; Author: Chris Wanstrath <chris@ozmm.org>
;; Version: 2

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This minor mode exists to mimick TextMate's awesome
;; features.

;;    ⌘T - Go to File
;;  ⇧⌘T - Go to Symbol
;;    ⌘L - Go to Line
;;  ⇧⌘L - Select Line (or expand Selection to select lines)
;;    ⌘/ - Comment Line (or Selection/Region)
;;    ⌘] - Shift Right
;;    ⌘[ - Shift Left
;;  ⌥⌘] - Align Assignments
;;  ⌥⌘[ - Indent Line
;;    ⌥↑ - Column Up
;;    ⌥↓ - Column Down
;;  ⌘RET - Insert Newline at Line's End
;;  ⌥⌘T - Reset File Cache (for Go to File)

;; A "project" in textmate-mode is determined by the presence of
;; a .git directory, an .hg directory, a Rakefile, or a Makefile.

;; You can configure what makes a project root by appending a file
;; or directory name onto the `*textmate-project-roots*' list.

;; If no project root indicator is found in your current directory,
;; textmate-mode will traverse upwards until one (or none) is found.
;; The directory housing the project root indicator (e.g. a .git or .hg
;; directory) is presumed to be the project's root.

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

;;; Needed for flet
(eval-when-compile
  (require 'cl))

;;; Minor mode

(defvar *textmate-gf-exclude*
  "(/|^)(\\.+[^/]+|vendor|fixtures|tmp|log|classes|build)($|/)|(\\.xcodeproj|\\.nib|\\.framework|\\.app|\\.pbproj|\\.pbxproj|\\.xcode|\\.xcodeproj|\\.bundle|\\.pyc)(/|$)"
  "Regexp of files to exclude from `textmate-goto-file'.")

(defvar *textmate-project-roots*
  '(".git" ".hg" "Rakefile" "Makefile" "README" "build.xml" ".emacs-project")
  "The presence of any file/directory in this list indicates a project root.")

(defvar textmate-use-file-cache t
  "Should `textmate-goto-file' keep a local cache of files?")

(defvar textmate-completing-library 'ido
  "The library `textmade-goto-symbol' and `textmate-goto-file' should use for
completing filenames and symbols (`ido' by default)")

(defvar textmate-find-files-command "find \"%s\" -type f"
  "The command `textmate-project-files' uses to find files. %s will be replaced
by the project root.")

(defvar *textmate-completing-function-alist* '((ido ido-completing-read)
                                               (icicles  icicle-completing-read)
                                               (none completing-read))
  "The function to call to read file names and symbols from the user")

(defvar *textmate-completing-minor-mode-alist*
  `((ido ,(lambda (a) (progn (ido-mode a) (setq ido-enable-flex-matching t))))
    (icicles ,(lambda (a) (icy-mode a)))
    (none ,(lambda (a) ())))
  "The list of functions to enable and disable completing minor modes")

(defvar *textmate-mode-map*
  (let ((map (make-sparse-keymap)))
    (cond ((featurep 'aquamacs)
     (define-key map [A-return] 'textmate-next-line)
     (define-key map (kbd "A-M-t") 'textmate-clear-cache)
     (define-key map (kbd "A-M-]") 'align)
     (define-key map (kbd "A-M-[") 'indent-according-to-mode)
     (define-key map (kbd "A-]")  'textmate-shift-right)
     (define-key map (kbd "A-[") 'textmate-shift-left)
     (define-key map (kbd "A-/") 'comment-or-uncomment-region-or-line)
     (define-key map (kbd "A-L") 'textmate-select-line)
     (define-key map (kbd "A-t") 'textmate-goto-file)
     (define-key map (kbd "A-T") 'textmate-goto-symbol)
     (define-key map (kbd "M-<up>") 'textmate-column-up)
     (define-key map (kbd "M-<down>") 'textmate-column-down)
     (define-key map (kbd "M-S-<up>") 'textmate-column-up-with-select)
     (define-key map (kbd "M-S-<down>") 'textmate-column-down-with-select))
    ((and (featurep 'mac-carbon) (eq window-system 'mac) mac-key-mode)
     (define-key map [(alt meta return)] 'textmate-next-line)
     (define-key map [(alt meta t)] 'textmate-clear-cache)
     (define-key map [(alt meta \])] 'align)
     (define-key map [(alt meta \[)] 'indent-according-to-mode)
     (define-key map [(alt \])]  'textmate-shift-right)
     (define-key map [(alt \[)] 'textmate-shift-left)
     (define-key map [(meta /)] 'comment-or-uncomment-region-or-line)
     (define-key map [(alt t)] 'textmate-goto-file)
           (define-key map [(alt shift l)] 'textmate-select-line)
     (define-key map [(alt shift t)] 'textmate-goto-symbol)
     (define-key map [(alt up)] 'textmate-column-up)
     (define-key map [(alt down)] 'textmate-column-down)
     (define-key map [(alt shift up)] 'textmate-column-up-with-select)
     (define-key map [(alt shift down)] 'textmate-column-down-with-select))
    ((featurep 'ns)  ;; Emacs.app
     (define-key map [(super meta return)] 'textmate-next-line)
     (define-key map [(super meta t)] 'textmate-clear-cache)
     (define-key map [(super meta \])] 'align)
     (define-key map [(super meta \[)] 'indent-according-to-mode)
     (define-key map [(super \])]  'textmate-shift-right)
     (define-key map [(super \[)] 'textmate-shift-left)
     (define-key map [(super /)] 'comment-or-uncomment-region-or-line)
     (define-key map [(super t)] 'textmate-goto-file)
     (define-key map [(super shift l)] 'textmate-select-line)
     (define-key map [(super shift t)] 'textmate-goto-symbol)
     (define-key map [(meta up)] 'textmate-column-up)
     (define-key map [(meta down)] 'textmate-column-down)
     (define-key map [(meta shift up)] 'textmate-column-up-with-select)
     (define-key map [(meta shift down)] 'textmate-column-down-with-select))
    (t ;; Any other version
     (define-key map [(meta return)] 'textmate-next-line)
     (define-key map [(control c)(control t)] 'textmate-clear-cache)
     (define-key map [(control c)(control a)] 'align)
     (define-key map [(control tab)] 'textmate-shift-right)
     (define-key map [(control shift tab)] 'textmate-shift-left)
     (define-key map [(control c)(control k)] 'comment-or-uncomment-region-or-line)
     (define-key map [(meta t)] 'textmate-goto-file)
     (define-key map [(meta shift l)] 'textmate-select-line)
     (define-key map [(meta shift t)] 'textmate-goto-symbol)
     (define-key map [(alt up)] 'textmate-column-up)
     (define-key map [(alt down)] 'textmate-column-down)
     (define-key map [(alt shift up)] 'textmate-column-up-with-select)
     (define-key map [(alt shift down)] 'textmate-column-down-with-select)))
    map))

(defvar *textmate-project-root* nil
  "Used internally to cache the project root.")
(defvar *textmate-project-files* '()
  "Used internally to cache the files in a project.")

(defcustom textmate-word-characters "a-zA-Z0-9_" "Word Characters for Column Movement")
;;; Bindings

(defun textmate-ido-fix ()
  "Add up/down keybindings for ido."
  (define-key ido-completion-map [up] 'ido-prev-match)
  (define-key ido-completion-map [down] 'ido-next-match))

(defun textmate-completing-read (&rest args)
  "Uses `*textmate-completing-function-alist*' to call the appropriate completing
function."
  (let ((reading-fn
         (cadr (assoc textmate-completing-library
                      *textmate-completing-function-alist*))))
  (apply (symbol-function reading-fn) args)))

;;; allow-line-as-region-for-function adds an "-or-line" version of
;;; the given comment function which (un)comments the current line is
;;; the mark is not active.  This code comes from Aquamac's osxkeys.el
;;; and is licensed under the GPL

(defmacro allow-line-as-region-for-function (orig-function)
`(defun ,(intern (concat (symbol-name orig-function) "-or-line"))
   ()
   ,(format "Like `%s', but acts on the current line if mark is not active."
            orig-function)
   (interactive)
   (if mark-active
       (call-interactively (function ,orig-function))
     (save-excursion
       ;; define a region (temporarily) -- so any C-u prefixes etc. are preserved.
       (beginning-of-line)
       (set-mark (point))
       (end-of-line)
       (call-interactively (function ,orig-function))))))

(defun textmate-define-comment-line ()
  "Add or-line (un)comment function if not already defined"
  (unless (fboundp 'comment-or-uncomment-region-or-line)
    (allow-line-as-region-for-function comment-or-uncomment-region)))

;;; Commands

(defun textmate-next-line ()
  "Inserts an indented newline after the current line and moves the point to it."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun textmate-select-line ()
  "If the mark is not active, select the current line.
Otherwise, expand the current region to select the lines the region touches."
  (interactive)
  (if mark-active ;; expand the selection to select lines
      (let ((top (= (point) (region-beginning)))
            (p1 (region-beginning))
            (p2 (region-end)))
        (goto-char p1)
        (beginning-of-line)
        (push-mark (point))
        (goto-char p2)
        (unless (looking-back "\n")
          (progn
            (end-of-line)
            (if (< (point) (point-max)) (forward-char))))
        (setq mark-active t
              transient-mark-mode t)
        (if top (exchange-point-and-mark)))
    (progn
      (beginning-of-line)
      (push-mark (point))
      (end-of-line)
      (if (< (point) (point-max)) (forward-char))
      (setq mark-active t
            transient-mark-mode t))))

;; http://chopmo.blogspot.com/2008/09/quickly-jumping-to-symbols.html
(defun textmate-goto-symbol ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
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
                               (setq position
                                     (get-text-property 1 'org-imenu-marker
                                                        symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning
    ;; of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil
                                       (mapcar
                                        (lambda (symbol)
                                          (if (string-match regexp symbol)
                                              symbol))
                                        symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol)
                    (setq symbol-names (cons symbol
                                             (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " (reverse symbol-names)))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char (if (overlayp position) (overlay-start position) position)))))

(defun textmate-goto-file ()
  "Uses your completing read to quickly jump to a file in a project."
  (interactive)
  (let ((root (textmate-project-root)))
    (when (null root) 
      (error "Can't find any .git directory"))
    (find-file 
     (concat 
      (expand-file-name root) "/"
      (textmate-completing-read 
       "Find file: "
       (mapcar
	(lambda (e)
	  (replace-regexp-in-string (textmate-project-root) "" e))
	(textmate-cached-project-files (textmate-project-root))))))))

(defun textmate-clear-cache ()
  "Clears the project root and project files cache. Use after adding files."
  (interactive)
  (setq *textmate-project-root* nil)
  (setq *textmate-project-files* nil)
  (message "textmate-mode cache cleared."))

;;; Utilities

(defun textmate-find-project-files (root)
  "Finds all files in a given project."
  (split-string
    (shell-command-to-string
     (concat
      (textmate-string-replace "%s" root textmate-find-files-command)
      "  | grep -vE '"
      *textmate-gf-exclude*
      "' | sed 's:"
      *textmate-project-root*
      "/::'")) "\n" t))

(defun textmate-project-files (root)
  (sort
    (textmate-find-project-files root)
    '(lambda (a b) (< (length a) (length b)))))

;; http://snipplr.com/view/18683/stringreplace/
(defun textmate-string-replace (this withthat in)
  "replace THIS with WITHTHAT' in the string IN"
  (with-temp-buffer
    (insert in)
    (goto-char (point-min))
    (while (search-forward this nil t)
      (replace-match withthat nil t))
    (buffer-substring (point-min) (point-max))))

(defun textmate-cached-project-files (&optional root)
  "Finds and caches all files in a given project."
  (cond
   ((null textmate-use-file-cache) (textmate-project-files root))
   ((equal (textmate-project-root) (car *textmate-project-files*))
    (cdr *textmate-project-files*))
   (t (cdr (setq *textmate-project-files*
                 `(,root . ,(textmate-project-files root)))))))

(defun textmate-project-root ()
  "Returns the current project root."
  (when (or
         (null *textmate-project-root*)
         (not (string-match *textmate-project-root* default-directory)))
    (let ((root (textmate-find-project-root)))
      (if root
          (setq *textmate-project-root* (expand-file-name (concat root "/")))
        (setq *textmate-project-root* nil))))
  *textmate-project-root*)

(defun root-match(root names)
  (member (car names) (directory-files root)))

(defun root-matches(root names)
  (if (root-match root names)
      (root-match root names)
      (if (eq (length (cdr names)) 0)
          'nil
          (root-matches root (cdr names))
          )))

(defun textmate-find-project-root (&optional root)
  "Determines the current project root by recursively searching for an indicator."
  (when (null root) (setq root default-directory))
  (cond
   ((root-matches root *textmate-project-roots*)
    (expand-file-name root))
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

(defun textmate-go-column (direction arg)
  "Move down a column"
  (let* ((orig-line (line-number-at-pos))
         (orig-column (current-column))
         (prefix-match-regex (if (<= orig-column 1) "^" (format "^.\\{%d\\}" (- orig-column 1))) )
         (word-regex (concat "[" textmate-word-characters "]"))
         (non-word-regex (concat "[^\n" textmate-word-characters "]"))
         (matching-regex (concat prefix-match-regex
                                 (cond ((looking-back "^") "")
                                       ((looking-back word-regex) word-regex)
                                       (t non-word-regex))
                                 (cond ((looking-at "$") "$")
                                       ((looking-at word-regex) word-regex)
                                       (t non-word-regex))))
         (do-search (if (= direction 1)
                        (lambda () (search-forward-regexp matching-regex nil t))
                      (lambda () (search-backward-regexp matching-regex nil t)))))
    (forward-char direction)
    (funcall do-search)
    (backward-char direction)
    (move-to-column orig-column)
    (if (= (line-number-at-pos) (+ orig-line direction)) ;; did you only move one line?
        (progn
          (while (= (line-number-at-pos) (+ orig-line direction))
            (setq orig-line (line-number-at-pos))
            (funcall do-search)
            (move-to-column orig-column))
          (goto-line orig-line)
          (move-to-column orig-column)))))

(defun textmate-column-up (arg)
  "Move up a column, textmate-style"
  (interactive "P")
  (textmate-go-column -1 arg))

(defun textmate-column-down (arg)
  "Move down a column, textmate-style"
  (interactive "P")
  (textmate-go-column 1 arg))

(defun textmate-column-up-with-select (arg)
  "Move up a column, selecting with shift-select"
  (interactive "P")
  (unless mark-active (progn (push-mark (point))
                             (setq mark-active t transient-mark-mode t)))
  (let (deactivate-mark) (textmate-column-up arg)))

(defun textmate-column-down-with-select (arg)
  "Move down a column, selecting with shift-select"
  (interactive "P")
  (unless mark-active (progn (push-mark (point))
                             (setq mark-active t transient-mark-mode t)))
  (let (deactivate-mark) (textmate-column-down arg)))

;;;###autoload
(define-minor-mode textmate-mode "TextMate Emulation Minor Mode"
  :lighter " mate" :global t :keymap *textmate-mode-map*
  (add-hook 'ido-setup-hook 'textmate-ido-fix)
  (textmate-define-comment-line)
  ; activate preferred completion library
  (dolist (mode *textmate-completing-minor-mode-alist*)
    (if (eq (car mode) textmate-completing-library)
        (funcall (cadr mode) t)
      (when (fboundp
             (cadr (assoc (car mode) *textmate-completing-function-alist*)))
        (funcall (cadr mode) -1)))))

(provide 'textmate)
;;; textmate.el ends here

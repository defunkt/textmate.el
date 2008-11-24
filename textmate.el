;; textmate.el --- TextMate minor mode for Emacs
;; Chris Wanstrath <chris@ozmm.org>
;; Licensed under the same terms as Emacs.

;; I didn't write most of this code. 
;; I don't know if it works without Aquamacs. 
;; Patches welcome.

;;
;; Installation
;;

;; $ cd ~/.emacs.d/vendor
;; $ git clone git://github.com/defunkt/textmate.el.git
;;
;; In your emacs config:
;;
;; (add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
;; (require 'textmate)

;;; Minor mode

(defvar textmate-mode-map (make-sparse-keymap))
(defvar *textmate-project-root* nil)
(defvar *textmate-fip-exclude-pattern* "vendor\\|fixtures\\|tmp\\|log")

;;; Bindings

(defun textmate-bind-keys ()
  (if (boundp 'aquamacs-version) 
      (textmate-bind-aquamacs-keys)
    (textmate-bind-carbon-keys)))

(defun textmate-bind-aquamacs-keys ()
  (define-key textmate-mode-map [A-return] 'textmate-insert-blank-line-after-current)
  (define-key textmate-mode-map (kbd "A-M-]") 'align)
  (define-key textmate-mode-map (kbd "A-]") 'indent-region)
  ;; Needed to override menu items
  (define-key osx-key-mode-map (kbd "A-t") 'textmate-find-in-project)
  (define-key osx-key-mode-map (kbd "A-T") 'textmate-find-symbol))

(defun textmate-bind-carbon-keys ()
  ;; Are these any good? Anyone have good Carbon defaults?
  (define-key textmate-mode-map [M-return] 'textmate-insert-blank-line-after-current)
;  (define-key textmate-mode-map [(meta ])] 'align)
  (define-key textmate-mode-map [(control tab)] 'indent-region)
  (define-key textmate-mode-map [(meta t)] 'textmate-find-in-project)
  (define-key textmate-mode-map [(meta T)] 'textmate-find-symbol))

;;; Commands

(defun textmate-insert-blank-line-after-current ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

;; http://chopmo.blogspot.com/2008/09/quickly-jumping-to-symbols.html
(defun textmate-find-symbol ()
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
    (let* ((selected-symbol (ido-completing-read "Symbol: " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

;;; Utilities

(defun textmate-find-in-project (&optional starting)
  (interactive)
  (textmate-find-project-root)
  (find-file (concat *textmate-project-root* "/"
                     (ido-completing-read "Find file: " (textmate-project-files *textmate-project-root*)))))

(defun textmate-project-files (&optional root)
  (cond
   ((null root) '())
   ((listp root) 
    (mapcar
     (lambda (path)
       (replace-regexp-in-string (expand-file-name (concat *textmate-project-root* "/")) "" path))
     (remq nil (flatten (cons (textmate-project-files (car root)) (textmate-project-files (cdr root)))))))
   ((string-match *textmate-fip-exclude-pattern* root) '())
   ((file-directory-p root) (textmate-project-files (directory-files root t "^[^.]+" t)))
   (t root)))

(defun textmate-find-project-root ()
  (when (or (null *textmate-project-root*) (not (string-match default-directory *textmate-project-root*)))
    (setq *textmate-project-root* (textmate-project-root))))

(defun textmate-project-root (&optional root)
  (when (null root) (setq root default-directory))
  (cond
   ((member ".git" (directory-files root)) root)
   ((eq root "/") nil)
   (t (textmate-project-root (concat root "..")))))

;;;###autoload
(define-minor-mode textmate-mode "TextMate Emulation Minor Mode"
  :lighter " mate" :global t :keymap textmate-mode-map
  (textmate-bind-keys))

(provide 'textmate)
;;; textmate.el ends here
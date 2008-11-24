;; textmate.el --- Basic TextMate emulation for Emacs
;; Chris Wanstrath <chris@ozmm.org>
;; Licensed under the same terms as Emacs.

;; I didn't write most of this code. 
;; I don't know if it works without Aquamacs. 
;; Patches welcome.

;;
;; Installation
;;

;; (add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
;; (add-to-list 'load-path "~/.emacs.d/vendor/textmate.el/vendor/fuzzy-find-in-project")
;; (load "textmate")

;;
;; Dependencies
;;

(add-to-list 'load-path (expand-file-name "vendor/fuzzy-find-in-project"))
(require 'fuzzy-find-in-project)

;;
;; Keybindings
;;

(global-set-key [A-return] 'textmate-insert-blank-line-after-current)
(global-set-key (kbd "A-M-]") 'align)
(global-set-key (kbd "A-]") 'indent-region)
(define-key osx-key-mode-map (kbd "A-t") 'fuzzy-find-in-project)
(define-key osx-key-mode-map (kbd "A-T") 'textmate-ido-goto-symbol)

;;
;; Defun(kt)s
;;

(defun textmate-insert-blank-line-after-current ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

;; http://chopmo.blogspot.com/2008/09/quickly-jumping-to-symbols.html
(defun textmate-ido-goto-symbol ()
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
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))


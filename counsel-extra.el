;;; counsel-extra.el --- Configure extra -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/counsel-extra
;; Keywords: lisp, convenience
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:

;; This file configures operations with extra

;; Commands

;; M-x `counsel-extra-bookmark'
;;      Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist.

;; M-x `counsel-extra-bookmark-in-other-window'
;;      Open bookmark in other window.

;; M-x `counsel-extra-find-symbol-in-other-window' (&optional it)
;;      Find symbol definition that corresponds to string IT in other window.

;; M-x `counsel-extra-call-in-other-window' (func &rest args)
;;      Switch to other window and call FUNC with ARGS.

;; M-x `counsel-extra-open-file-other-window'
;;      Quit the minibuffer and call `find-file-other-window' action.

;; M-x `counsel-extra-move-file'
;;      Quit the minibuffer and call `counsel-find-file-move' action.

;; M-x `counsel-extra-delete-file'
;;      Quit the minibuffer and call `counsel-find-file-delete' action.

;; M-x `counsel-extra-copy-file'
;;      Quit the minibuffer and call `counsel-find-file-copy' action.

;; M-x `counsel-extra-dired'
;;      Open file in Dired.

;; M-x `counsel-extra-expand-dir-done'
;;      Counsel expand dir done.

;; M-x `counsel-extra-expand-dir-maybe'
;;      Counsel expand dir maybe.

;; M-x `counsel-extra-ivy-mark' (&optional _c)
;;      Mark or unmark current ivy candidate and go to the next match.

;; M-x `counsel-extra-ivy-insert'
;;      Insert ivy marked candidates or current choice.

;; M-x `counsel-extra-switch-to-buffer-other-window'

;; M-x `counsel-extra-ivy-browse-url'
;;      If current ivy choice is url, open it in browser, else search in google.

;;; Code:


(require 'ivy)
(require 'counsel)

(declare-function bookmark-location "bookmark")
(declare-function bookmark-all-names "bookmark")

(defvar counsel-extra-window-last-key nil)

(defun counsel-extra-pp-fontify (content &optional mode-fn &rest args)
  "Fontify CONTENT according to MODE-FN called with ARGS.
If CONTENT is not a string, instead of MODE-FN emacs-lisp-mode will be used."
  (with-temp-buffer
    (delay-mode-hooks
      (apply (or mode-fn 'emacs-lisp-mode) args)
      (goto-char (point-min))
      (insert (if (or (eq major-mode 'emacs-lisp-mode)
                      (not (stringp content)))
                  (pp-to-string content)
                content))
      (font-lock-ensure)
      (buffer-string))))

(defun counsel-extra-pp-minibuffer-select-window ()
  "Select minibuffer window if it is active."
  (when-let ((wind (active-minibuffer-window)))
    (select-window wind)))

(defun counsel-extra-pp-inspect (content &rest setup-fns)
  "Display CONTENT in popup window.
SETUP-FNS can includes keymaps, syntax tables and functions."
  (let ((buffer (get-buffer-create "*counsel-extra-pp-insepct*"))
        (keymaps (seq-filter 'keymapp setup-fns))
        (stx-table (seq-find 'syntax-table-p setup-fns))
        (mode-fn (seq-find 'functionp setup-fns)))
    (with-current-buffer buffer
      (with-current-buffer-window
          buffer
          (cons (or 'display-buffer-in-direction)
                '((window-height . window-preserve-size)))
          (lambda (window _value)
            (with-selected-window window
              (setq buffer-read-only t)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (progn  (save-excursion
                          (insert content))
                        (add-hook 'kill-buffer-hook
                                  'counsel-extra-pp-minibuffer-select-window
                                  nil t)
                        (visual-line-mode 1)
                        (when mode-fn
                          (funcall mode-fn))
                        (use-local-map
                         (let ((map (make-sparse-keymap)))
                           (define-key map (kbd "q") 'kill-this-buffer)
                           (define-key map (kbd "C-x 0") 'kill-this-buffer)
                           (add-hook
                            'read-only-mode-hook
                            (lambda ()
                              (if buffer-read-only
                                  (define-key map (kbd "q")
                                              'kill-this-buffer)
                                (define-key map (kbd "q")
                                            'self-insert-command)))
                            t)
                           (when keymaps
                             (setq map (make-composed-keymap
                                        keymaps
                                        map)))
                           (set-keymap-parent map (current-local-map))
                           map))))))
        (insert content))
      (when stx-table
        (set-syntax-table stx-table))
      (setq header-line-format "*inspect*")
      (unless (active-minibuffer-window)
        (select-window (get-buffer-window buffer))))))

(defun counsel-extra-pp-setup-fn (&optional content inspect-keymap mode-fn)
	"Setup popup window and run `counsel-extra-pp-inspect'.
Optional argument CONTENT is string.
INSPECT-KEYMAP is keymap.
MODE-FN is a function."
  (lambda (window _value)
    (with-selected-window window
      (visual-line-mode 1)
      (unwind-protect
          (setq counsel-extra-window-last-key
                (read-key-sequence ""))
        (quit-restore-window window 'kill)
        (let ((key-descr (and
                          counsel-extra-window-last-key
                          (key-description counsel-extra-window-last-key))))
          (if (member key-descr '("C-o" "C-c C-o" "C-c o" "M-o"))
              (run-at-time '0.5 nil 'counsel-extra-pp-inspect content
                           inspect-keymap
                           mode-fn)
            (when-let ((wind (active-minibuffer-window)))
              (with-selected-window wind
                (when-let ((command
                            (prog1
                                (key-binding
                                 counsel-extra-window-last-key)
                              (setq counsel-extra-window-last-key nil))))
                  (if (and (commandp command)
                           (equal 0
                                  (car (func-arity command))))
                      (funcall command)
                    (and key-descr (execute-kbd-macro
                                    key-descr)))))))
          (setq counsel-extra-window-last-key nil))))))

(defun counsel-extra-pp (content &optional
                                 mode-fn
                                 inspect-keymap
                                 display-buff-fn)
  "Display CONTENT in popup window.
Optional argument CONTENT is string.
INSPECT-KEYMAP is keymap.
MODE-FN is a function."
  (let ((buffer (get-buffer-create "*counsel-pp*"))
        (content (if (or
                      mode-fn
                      (not (stringp content)))
                     (apply 'counsel-extra-pp-fontify
                            (list content mode-fn))
                   content)))
    (with-current-buffer buffer
      (with-current-buffer-window
          buffer
          (cons (or display-buff-fn 'display-buffer-at-bottom)
                '((window-height . fit-window-to-buffer)))
          (counsel-extra-pp-setup-fn content inspect-keymap mode-fn)
        (insert content)))))

(defun counsel-extra-pp-file (file)
  "Preview FILE."
  (when-let ((filename (and
                        file
                        (file-readable-p file)
                        (file-exists-p file)
                        file))
             (buffer (get-buffer-create "*counsel-pp*")))
    (let* ((content))
      (with-temp-buffer
        (let ((buffer-file-name file))
          (set-auto-mode)
          (insert-file-contents filename)
          (font-lock-ensure)
          (setq content (buffer-string))))
      (with-current-buffer buffer
        (with-current-buffer-window
            buffer
            (cons 'display-buffer-same-window
                  '((window-height . fit-window-to-buffer)))
            (counsel-extra-pp-setup-fn content)
          (insert content)
          (setq header-line-format filename))))))

(defun counsel-extra-read-file-display-transformer (str)
  "Transform filename STR when reading files."
  (when-let ((filename (expand-file-name str (ivy-state-directory ivy-last))))
    (let ((parts (delete nil `(,str ,(file-symlink-p filename))))
          (face (cond
                 ((not (file-readable-p filename)) 'ivy-match-required-face)
                 ((file-accessible-directory-p filename) 'ivy-subdir)
                 ((and
                   (file-regular-p filename)
                   (file-executable-p filename))
                  'compilation-info)
                 (t nil)))
          result)
      (setq result (string-join parts " => "))
      (if face
          (propertize result 'face face)
        result))))

(defun counsel-extra-ivy-sort-file-function (a b)
  "Compare filename A and filename B."
  (let* ((x (concat ivy--directory a))
         (y (concat ivy--directory b))
         (x-mtime (nth 5 (file-attributes x)))
         (y-mtime (nth 5 (file-attributes y)))
         (a-count (if (string-match-p "^[.]" a)
                      1
                    0))
         (b-count (if (string-match-p "^[.]" b)
                      1
                    0))
         (result (> b-count a-count)))
    (cond ((and (file-directory-p x)
                (file-directory-p y))
           result)
          ((file-directory-p x)
           t)
          ((and (file-directory-p y))
           nil)
          ((= a-count b-count)
           (time-less-p y-mtime x-mtime))
          (t result))))

(defun counsel-extra-ivy-browse-url-action (item)
  "If ITEM is url, open it in browser, else search in google."
  (let ((url (cond ((string-match-p "^http[s]?://" item)
                    item)
                   (t (format "http://www.google.com/search?q=%s"
                              (url-hexify-string item))))))
    (browse-url url)))

(defun counsel-extra-ivy-trim-mark-prefix (item)
	"Trim `ivy-mark-prefix' prefix from ITEM."
  (if (and
       item
       ivy-mark-prefix
       (not (symbolp item))
       (stringp item))
      (replace-regexp-in-string (format "^[%s]+" ivy-mark-prefix) "" item)
    item))

(defun counsel-extra-strip-text-props (item)
  "If ITEM is string, return it without text properties.

 If ITEM is symbol, return it is `symbol-name.'
 Otherwise return nil."
  (cond ((stringp item)
         (let ((str (seq-copy item)))
           (set-text-properties 0 (length str) nil str)
           str))
        ((and item (symbolp item))
         (symbol-name item))
        (nil item)))

(defun counsel-extra-get-word (&optional chars)
  "Get thing at point matching CHARS.
Optional argument CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \ (but
 not at the end of a range; quoting is never needed there)"
  (unless chars (setq chars "-'*\"_~$A-Za-z0-9:.#\\+"))
  (when-let ((bounds (if (use-region-p)
                         (cons (region-beginning) (region-end))
                       (save-excursion
                         (let* ((a (save-excursion
                                     (skip-chars-backward chars)
                                     (point)))
                                (b (save-excursion
                                     (skip-chars-forward chars)
                                     (point))))
                           (if (string-blank-p
                                (buffer-substring-no-properties a b))
                               nil
                             (cons a b)))))))
    (buffer-substring-no-properties (car bounds)
                                    (cdr bounds))))

(defun counsel-extra-insert (item &optional separator)
  "Insert or complete ITEM and SEPARATOR.
If word at point is prefix of ITEM, complete it, else insert ITEM.
Optional argument SEPARATOR is a string to insert just after ITEM.
Default value of SEPARATOR is space."
  (let ((parts))
    (setq parts
          (if-let ((current-word (counsel-extra-get-word
                                  "-*_~$A-Za-z0-9:#\\+")))
              (progn
                (if (string-prefix-p current-word item)
                    (list (substring-no-properties
                           item (length current-word)))
                  (list (or separator "\s") item)))
            (list item)))
    (apply 'insert parts)))

(defun counsel-extra-expand-file-when-exists (name &optional directory)
  "Expand filename NAME to absolute if it exits.

Second arg DIRECTORY is directory to start with if FILENAME is relative.
If DIRECTORY is nil or missing, the current buffer's value of
`default-directory'is used."
  (when-let ((file (unless (null name)
                     (if directory
                         (expand-file-name name directory)
                       (expand-file-name name)))))
    (when (file-exists-p file)
      file)))

(defun counsel-extra-file-parent (path)
  "Return the parent directory to PATH without slash."
  (let ((parent (file-name-directory
                 (directory-file-name
                  (expand-file-name path default-directory)))))
    (when (and (file-exists-p path)
               (file-exists-p parent)
               (not (equal
                     (file-truename (directory-file-name
                                     (expand-file-name path)))
                     (file-truename (directory-file-name
                                     (expand-file-name parent))))))
      (if (file-name-absolute-p path)
          (directory-file-name parent)
        (file-relative-name parent)))))

(defun counsel-extra-call-in-other-window-0 (func &optional args)
  "Move to other window and call FUNC with ARGS."
  (let ((current-window (selected-window)))
    (cond ((window-right current-window)
           (windmove-right))
          ((window-left current-window)
           (windmove-left))
          (t (split-window-right)
             (windmove-right)))
    (apply func args)))

;;;###autoload
(defun counsel-extra-ivy-copy ()
  "Copy current ivy candidate without text proerties."
  (interactive)
  (let ((item (ivy-state-current ivy-last)))
    (kill-new (if (stringp item)
                  (counsel-extra-strip-text-props item)
                (counsel-extra-strip-text-props (car item))))))

;;;###autoload
(defun counsel-extra-pp-ivy ()
  "Print properties of current ivy choice."
  (interactive)
  (let ((curr (ivy-state-current ivy-last)))
    (counsel-extra-pp (text-properties-at 0 curr))))

;;;###autoload
(defun counsel-extra-ivy-browse-url ()
  "If current ivy choice is url, open it in browser, else search in google."
  (interactive)
  (ivy-exit-with-action #'counsel-extra-ivy-browse-url-action
                        (ivy-state-current ivy-last)))

;;;###autoload
(defun counsel-extra-switch-to-buffer-other-window ()
  "Exit minibuffer with `ivy--switch-buffer-other-window-action'."
  (interactive)
  (ivy-exit-with-action
   'ivy--switch-buffer-other-window-action))

;;;###autoload
(defun counsel-extra-ivy-insert ()
  "Insert ivy marked candidates or current choice."
  (interactive)
  (if-let ((marked-str
            (when ivy-marked-candidates
              (mapconcat
               (lambda
                 (arg)
                 (counsel-extra-ivy-trim-mark-prefix
                  (counsel-extra-strip-text-props arg)))
               ivy-marked-candidates "\n"))))
      (let ((str (concat (if (with-ivy-window
                               (counsel-extra-get-word))
                             "\n" "")
                         marked-str)))
        (ivy-quit-and-run (counsel-extra-insert str)))
    (ivy-exit-with-action #'counsel-extra-insert)))

;;;###autoload
(defun counsel-extra-ivy-mark (&optional _c)
  "Mark or unmark current ivy candidate and go to the next match."
  (interactive)
  (if (and ivy-marked-candidates (ivy--marked-p))
      (funcall 'ivy--unmark (ivy-state-current ivy-last))
    (funcall 'ivy--mark (ivy-state-current ivy-last)))
  (ivy-next-line))

;;;###autoload
(defun counsel-extra-expand-dir-maybe ()
	"Counsel expand dir maybe."
  (interactive)
  (let ((curr (ivy-state-current ivy-last))
        (dir))
    (if (and
         (> ivy--length 0)
         (not (string= curr "./"))
         (setq dir
               (ivy-expand-file-if-directory
                curr)))
        (progn
          (ivy--cd dir)
          (ivy--exhibit))
      (if-let ((ext (member (file-name-extension curr)
                            '("mp4" "mkv" "xlsx" "png" "jpg" "jpeg"
                              "webm" "3gp" "mp4" "MOV"))))
          (ivy-call)
        (with-selected-window (ivy--get-window ivy-last)
          (counsel-extra-pp-file
           (expand-file-name curr ivy--directory)))))))

;;;###autoload
(defun counsel-extra-expand-dir-done ()
	"Counsel expand dir done."
  (interactive)
  (let
      ((curr
        (ivy-state-current ivy-last))
       (dir))
    (if
        (and
         (> ivy--length 0)
         (not
          (string= curr "./"))
         (setq dir
               (ivy-expand-file-if-directory
                curr)))
        (progn
          (ivy--cd dir)
          (ivy--exhibit))
      (ivy-done))))

;;;###autoload
(defun counsel-extra-dired ()
  "Open file in Dired."
  (interactive)
  (when-let* ((filename (counsel-extra-expand-file-when-exists
                         (ivy-state-current
                          ivy-last)
                         ivy--directory))
              (parent (file-name-as-directory
                       (counsel-extra-file-parent filename))))
    (when (file-directory-p parent)
      (ivy-quit-and-run
        (funcall 'dired parent)))))

(defun counsel-extra-find-symbol (x)
  "Find symbol definition that corresponds to string X."
  (counsel--push-xref-marker)
  (let ((full-name (get-text-property 0 'full-name x)))
    (if full-name
        (find-library full-name)
      (let ((sym (read x)))
        (cond ((and (eq (ivy-state-caller ivy-last)
                        'counsel-describe-variable)
                    (boundp sym))
               (find-variable sym))
              ((fboundp sym)
               (find-function sym))
              ((boundp sym)
               (find-variable sym))
              ((or (featurep sym)
                   (locate-library
                    (prin1-to-string sym)))
               (find-library
                (prin1-to-string sym)))
              (t
               (error "Couldn't find definition of %s"
                      sym)))))))

;;;###autoload
(defun counsel-extra-copy-file ()
    "Quit the minibuffer and call `counsel-find-file-copy' action."
    (interactive)
    (ivy-exit-with-action #'counsel-find-file-copy))

;;;###autoload
(defun counsel-extra-delete-file ()
  "Quit the minibuffer and call `counsel-find-file-delete' action."
  (interactive)
  (ivy-exit-with-action #'counsel-find-file-delete))

;;;###autoload
(defun counsel-extra-move-file ()
  "Quit the minibuffer and call `counsel-find-file-move' action."
  (interactive)
  (ivy-exit-with-action #'counsel-find-file-move))

;;;###autoload
(defun counsel-extra-open-file-other-window ()
  "Quit the minibuffer and call `find-file-other-window' action."
  (interactive)
  (ivy-exit-with-action #'find-file-other-window))

;;;###autoload
(defun counsel-extra-call-in-other-window (func &rest args)
  "Switch to other window and call FUNC with ARGS."
  (interactive)
  (if (minibuffer-window-active-p (selected-window))
      (with-minibuffer-selected-window
        (counsel-extra-call-in-other-window-0 func args))
    (counsel-extra-call-in-other-window-0 func args)))

;;;###autoload
(defun counsel-extra-find-symbol-in-other-window (&optional it)
  "Find symbol definition that corresponds to string IT in other window."
  (interactive)
  (unless it (setq it (ivy-state-current ivy-last)))
  (ivy-quit-and-run
    (counsel-extra-call-in-other-window 'counsel-extra-find-symbol it)))

;;;###autoload
(defun counsel-extra-bookmark-in-other-window ()
	"Open bookmark in other window."
  (interactive)
  (require 'bookmark)
  (let ((curr (ivy-state-current ivy-last)))
    (ivy-quit-and-run
      (counsel-extra-call-in-other-window
       (lambda (x)
         (cond ((and counsel-bookmark-avoid-dired
                     (member x (bookmark-all-names))
                     (file-directory-p (bookmark-location x)))
                (let ((default-directory (bookmark-location x)))
                  (counsel-find-file)))
               ((member x (bookmark-all-names))
                (bookmark-jump x))
               (t
                (bookmark-set x))))
       curr))))

(defvar counsel-extra-bookmark-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") 'counsel-extra-bookmark-in-other-window)
    map)
  "Keymap for `counsel-bookmark'.")

;;;###autoload
(defun counsel-extra-bookmark ()
  "Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist."
  (interactive)
  (require 'bookmark)
  (ivy-read "Create or jump to bookmark: "
            (bookmark-all-names)
            :history 'bookmark-history
            :keymap counsel-extra-bookmark-map
            :action (lambda (x)
                      (cond ((and counsel-bookmark-avoid-dired
                                  (member x (bookmark-all-names))
                                  (file-directory-p (bookmark-location x)))
                             (with-selected-window (ivy--get-window ivy-last)
                               (let ((default-directory
                                      (bookmark-location x)))
                                 (counsel-find-file))))
                            ((member x (bookmark-all-names))
                             (with-selected-window (ivy--get-window ivy-last)
                               (bookmark-jump x)))
                            (t
                             (bookmark-set x))))
            :caller 'counsel-extra-bookmark))

(ivy-set-actions
 'counsel-extra-bookmark
 `(("j" bookmark-jump-other-window "other window")
   ("d" bookmark-delete "delete")
   ("e" bookmark-rename "edit")
   ("s" bookmark-set "overwrite")
   ("x" ,(counsel--apply-bookmark-fn #'counsel-find-file-extern)
        "open externally")
   ("r" ,(counsel--apply-bookmark-fn #'counsel-find-file-as-root)
        "open as root")))

;;;###autoload
(defun counsel-extra-add-extra-actions ()
  "Add extra actions ot all ivy callers."
  (interactive)
  (ivy-add-actions
   t
   '(("i" counsel-extra-insert "insert")
     ("p" counsel-extra-pp "print")
     ("g" counsel-extra-ivy-browse-url-action "google it"))))

;;;###autoload
(defun counsel-extra-configure-find-file ()
  "Configure `counsel-find-file' and `read-file-name-internal'."
  (interactive)
  (ivy-configure 'counsel-find-file
    :occur #'counsel-find-file-occur
    :display-transformer-fn #'counsel-extra-read-file-display-transformer)
  (ivy-configure 'read-file-name-internal
    :sort-fn #'counsel-extra-ivy-sort-file-function
    :display-transformer-fn #'counsel-extra-read-file-display-transformer)
  (add-to-list 'ivy-sort-functions-alist
               '(read-file-name-internal
                 . counsel-extra-ivy-sort-file-function)))

(provide 'counsel-extra)
;;; counsel-extra.el ends here
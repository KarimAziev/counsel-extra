;;; counsel-extra.el --- Extend counsel commands -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/counsel-extra
;; Keywords: lisp, convenience
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (ivy "0.14.0") (counsel "0.14.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; Extends the functionality of Ivy and Counsel packages by providing extra utilities and commands.

;; Main Commands

;; `counsel-extra-M-x' Extra version of `execute-extended-command'.

;; `counsel-extra-list-processes' Offer completion for `process-list'. The
;; default action is to switch to the process buffer. An extra action allows to
;; delete the selected process.


;;  `counsel-extra-imenu-jump-to-item-in-other-window' Jump to imenu item in other window.
;;  `counsel-extra-imenu-insert-cmd'                   Quit the minibuffer and insert imenu item.

;;  `counsel-extra-configure-find-file'           Configure `counsel-find-file' and `read-file-name-internal'.
;;  `counsel-extra-add-extra-actions'             Add extra actions to all Ivy callers.
;;  `counsel-extra-bookmark'                      Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist.
;;  `counsel-extra-bookmark-in-other-window'      Open bookmark in other window.
;;  `counsel-extra-find-symbol-in-other-window'   Find symbol definition that corresponds in other window

;; File commands

;;  `counsel-extra-open-file-other-window' Quit the minibuffer and call `find-file-other-window' action.
;;  `counsel-extra-move-file'              Quit the minibuffer and call `counsel-find-file-move' action.
;;  `counsel-extra-delete-file'            Quit the minibuffer and call `counsel-find-file-delete' action.
;;  `counsel-extra-copy-file'              Quit the minibuffer and call `counsel-find-file-copy' action.
;;  `counsel-extra-dired'                  Open file in Dired.
;;  `counsel-extra-expand-dir-done'        Visit or preview file.
;;   If it is a valid directory, visit it and stay in minibuffer, otherwise execute default ivy action and exit minibuffer.
;;  `counsel-extra-expand-dir-maybe'       Visit or preview file and stay in minibuffer.
;;   If it is not a valid directory, preview the file

;; Misc commands

;;  `counsel-extra-ivy-mark' Mark or unmark current ivy candidate and go to the
;;  next match.
;;  `counsel-extra-ivy-insert' Return a string of marked candidates and insert
;;  it into the buffer.
;;  `counsel-extra-switch-to-buffer-other-window' Exit minibuffer with
;;  `ivy--switch-buffer-other-window-action'.
;;  `counsel-extra-ivy-browse-url' If current ivy choice is url, open it in
;;  browser, else search in google.
;;  `counsel-extra-ivy-copy' - Copy current ivy candidate without text
;;  properties

;; Color commands

;; `counsel-extra-colors-emacs' Show a list of all supported colors for a
;;  particular frame. You can insert or kill the name or hexadecimal =rgb=
;;  value of the selected color. Unlike =counsel-colors-emacs= it is allows to
;;  define extra commands in it's keymap - .
;;  Keymap for `counsel-extra-emacs-colors-map':

;; C-j       `counsel-extra-colors-flash-buffer' Temporarly change minibuffer background color
;; C-c C-w   `counsel-extra-colors-copy-hex'     Copy hex color and exit minibuffer.
;; C-c C-i   `counsel-extra-colors-insert-hex'   Insert hex color and exit minibuffer.
;; C-c M-i   `counsel-extra-colors-insert-color' Insert color without exitting minibuffer.


;;; Code:


(require 'ivy)
(require 'counsel)

(declare-function face-remap-remove-relative "face-remap")
(declare-function face-remap-add-relative "face-remap")
(declare-function bookmark-location "bookmark")

(defcustom counsel-extra-align-M-x-description nil
  "Whether to align command descriptions.
If nil, don't align, if integer align to those column."
  :type '(choice (const :tag "No align" nil)
                 (integer :tag "Column" 50))
  :group 'counsel-extra)

(defcustom counsel-extra-show-modified-time nil
  "Whether to show file modified time when reading filename."
  :group 'counsel-extra
  :type 'boolean)

(defcustom counsel-extra-align-modified-time 80
  "Whether to align modified time when reading filename.
If nil, don't align, if integer align to those column.
This option has effect only if `counsel-extra-show-modified-time' is enabled."
  :type '(choice (const :tag "No align" nil)
                 (integer :tag "Column" 80))
  :group 'counsel-extra)

(defvar counsel-extra-preview-momentary-buffer-name "*counsel-extra-preview-*")

(declare-function bookmark-all-names "bookmark")

(defun counsel-extra-preview-fontify (content &optional mode-fn &rest args)
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



(defun counsel-extra-preview (content &rest setup-args)
  "Momentarily display CONTENT in popup window.
Display remains until next event is input.

Persist popup if input is a key binding of a command
 `counsel-extra-preview-open-inspector'in `counsel-extra-preview-switch-keymap'.

SETUP-ARGS can includes keymaps, syntax table, filename and function.
See a function `counsel-extra-preview-open-inspector'."
  (let ((buffer (get-buffer-create
                 counsel-extra-preview-momentary-buffer-name))
        (mode-fn (seq-find #'functionp setup-args)))
    (with-current-buffer buffer
      (with-current-buffer-window buffer
          buffer
          (cons 'display-buffer-in-side-window
                '((window-height . fit-window-to-buffer)))
        (lambda (window _value)
          (with-selected-window window
            (setq buffer-read-only t)
            (let ((inhibit-read-only t))
              (unwind-protect
                  (read-key-sequence "")
                (quit-restore-window window 'kill)))))
        (insert (if (or
                     mode-fn
                     (not (stringp content)))
                    (apply
                     #'counsel-extra-preview-fontify
                     (list content mode-fn))
                  content))))))

(defun counsel-extra--preview-file (file)
  "Preview FILE in other window.
This function doesn't really visit FILE to avoid unwanted side effects, such
as running find file hooks, starting lsp or eglot servers and so on."
  (when (and file (file-exists-p file))
    (with-minibuffer-selected-window
      (let ((buffer (get-buffer-create "*counsel-extra-preview-file*")))
        (with-current-buffer buffer
          (with-current-buffer-window buffer
              (cons 'display-buffer-in-direction
                    '((window-height . fit-window-to-buffer)))
              (lambda (window _value)
                (with-selected-window window
                  (setq buffer-read-only t)
                  (let ((inhibit-read-only t))
                    (unwind-protect
                        (read-key-sequence "")
                      (quit-restore-window window 'kill)
                      (setq unread-command-events
                            (append (this-single-command-raw-keys)
                                    unread-command-events))))))
            (if (file-directory-p file)
                (dired file)
              (insert-file-contents file)
              (let ((buffer-file-name file))
                (ignore-errors
                  (delay-mode-hooks (set-auto-mode)
                                    (font-lock-ensure))))
              (setq header-line-format
                    (abbreviate-file-name file)))))))))

(defun counsel-extra-format-time-readable (time)
  "Calculate and format the time difference from the current TIME.

Argument TIME is the time value that will be compared with the current time to
calculate the time difference."
  (let ((diff-secs (-
                    (float-time (current-time))
                    (float-time time))))
    (pcase-let ((`(,format-str . ,value)
                 (cond ((< diff-secs 60)
                        (cons "%d second" (truncate diff-secs)))
                       ((< diff-secs 3600)
                        (cons "%d minute" (truncate (/ diff-secs 60))))
                       ((< diff-secs 86400)
                        (cons "%d hour" (truncate (/ diff-secs 3600))))
                       ((< diff-secs 2592000)
                        (cons "%d day" (truncate (/ diff-secs 86400))))
                       (t
                        (cons "%d month" (truncate (/ diff-secs 2592000)))))))
      (format (concat format-str (if (= value 1) " ago" "s ago")) value))))

(defun counsel-extra-read-file-display-transformer (str)
  "Transform filename STR when reading files."
  (let ((filename (expand-file-name str (ivy-state-directory ivy-last))))
    (let ((parts (delete nil `(,str ,(file-symlink-p filename))))
          (mod-time
           (and counsel-extra-show-modified-time
                (counsel-extra-format-time-readable
                 (file-attribute-modification-time
                  (file-attributes (if (file-directory-p
                                        filename)
                                       (file-name-as-directory
                                        filename)
                                     filename))))))
          (face
           (cond ((not (file-readable-p filename)) 'ivy-match-required-face)
                 ((file-accessible-directory-p filename) 'ivy-subdir)
                 ((and
                   (file-regular-p filename)
                   (file-executable-p filename))
                  'compilation-info)
                 (t nil)))
          result)
      (when face (setcar parts (propertize (car parts) 'face face)))
      (setq result (string-join parts " => "))
      (if mod-time
          (concat result
                  (if counsel-extra-align-modified-time
                      (propertize " " 'display
                                  (list 'space :align-to
                                        counsel-extra-align-modified-time))
                    " ")
                  mod-time)
        result))))


(defun counsel-extra-ivy-sort-file-function (a b)
  "Compare filename A and filename B."
  (let* ((x (concat ivy--directory a))
         (y (concat ivy--directory b))
         (x-mtime (nth 5 (file-attributes x)))
         (y-mtime (nth 5 (file-attributes y))))
    (cond ((and (file-directory-p x)
                (file-directory-p y))
           (time-less-p y-mtime x-mtime))
          ((file-directory-p x)
           t)
          ((and (file-directory-p y))
           nil)
          (t (time-less-p y-mtime x-mtime)))))

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
    (setq item (if (consp item)
                   (car item)
                 item))
    (setq parts
          (if-let ((current-word (counsel-extra-get-word
                                  "-*_~$A-Za-z0-9:#\\+")))
              (progn
                (if (string-prefix-p current-word item)
                    (list (substring-no-properties
                           item (length current-word)))
                  (list (or separator "\s") item)))
            (list item)))
    (apply #'insert parts)))

(defun counsel-extra-expand-file-when-exists (name &optional directory)
  "Expand filename NAME to absolute if it exits.

Second arg DIRECTORY is directory to start with if FILENAME is relative.
If DIRECTORY is nil or missing, the current buffer's value of
`default-directory'is used."
  (when-let ((file (when name
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
  "Copy current ivy candidate without text properties."
  (interactive)
  (let ((item (ivy-state-current ivy-last)))
    (kill-new (if (stringp item)
                  (counsel-extra-strip-text-props item)
                (counsel-extra-strip-text-props (car item))))))


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
   #'ivy--switch-buffer-other-window-action))

;;;###autoload
(defun counsel-extra-ivy-insert ()
  "Return a string of marked candidates and insert it into the buffer."
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
    (ivy-quit-and-run
      (counsel-extra-insert (ivy-state-current ivy-last)))))

;;;###autoload
(defun counsel-extra-ivy-mark (&optional _c)
  "Mark or unmark current ivy candidate and go to the next match."
  (interactive)
  (if (and ivy-marked-candidates (ivy--marked-p))
      (funcall #'ivy--unmark (ivy-state-current ivy-last))
    (funcall #'ivy--mark (ivy-state-current ivy-last)))
  (ivy-next-line))

;;;###autoload
(defun counsel-extra-expand-dir-maybe ()
  "Visit or preview currently selected directory or file and stay in minibuffer.
If it is not a valid directory, preview the file."
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
        (counsel-extra--preview-file
         (expand-file-name curr ivy--directory))))))

;;;###autoload
(defun counsel-extra-expand-dir-done ()
  "Visit or preview currently selected directory or file.
If it is a valid directory, visit it and stay in minibuffer, otervise
execute default ivy action and exit minibuffer."
  (interactive)
  (let ((curr
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
        (funcall #'dired parent)))))

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
    (define-key map (kbd "C-c C-o") #'counsel-extra-bookmark-in-other-window)
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
  "Add extra actions to all Ivy callers."
  (interactive)
  (ivy-add-actions
   t
   '(("i" counsel-extra-insert "insert")
     ("g" counsel-extra-ivy-browse-url-action "google it"))))

;;;###autoload
(defun counsel-extra-configure-find-file ()
  "Add display transformers to `counsel-find-file' and `read-file-name-internal'.
Also it is configures `ivy-sort-functions-alist'."
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

(defvar counsel-extra-M-X-last-command nil)
(defvar counsel-extra-M-X-externs nil)
(defvar counsel-extra-M-x-initial-input nil)


(defun counsel-extra-annotate-transform-get-function-key (sym buffer)
  "Return string with active key for command SYM in BUFFER.
SYM should be a symbol."
  (when (commandp sym)
    (with-current-buffer buffer
      (let ((k (where-is-internal sym nil t)))
        (when k
          (let ((i (cl-search [?\C-x ?6] k)))
            (when i
              (let ((dup (vconcat (substring k 0 i)
                                  [f2]
                                  (substring k (+ i 2))))
                    (map (current-global-map)))
                (when (equal (lookup-key map k)
                             (lookup-key map dup))
                  (setq k dup)))))
          (propertize (format "(%s)" (key-description k))
                      'face 'font-lock-variable-name-face))))))


(defun counsel-extra-annotate-transform-get-function-doc (sym)
  "Return a stirng with short documentation of symbol SYM or nil.
SYM should be a symbol."
  (when-let ((documentation
              (if (fboundp sym)
                  (documentation sym t)
                (documentation-property
                 sym
                 'variable-documentation
                 t))))
    (and (stringp documentation)
         (string-match ".*$" documentation)
         (propertize (format "\s%s" (match-string
                                     0
                                     documentation))
                     'face
                     'font-lock-negation-char-face))))


(defun counsel-extra-annotate-transform-function-name (name)
  "Transforms a function NAME into an annotated string.
Argument NAME is the name of the function."
  (or (ignore-errors
        (let ((buff
               (if-let ((minw (minibuffer-selected-window)))
                   (with-selected-window minw
                     (current-buffer))
                 (current-buffer)))
              (sym))
          (setq sym (intern name))
          (when (symbolp sym)
            (let ((result (concat
                           name "\s"
                           (or
                            (when-let ((key (counsel-extra-annotate-transform-get-function-key
                                             sym buff)))
                              key)
                            "")
                           (or
                            (when-let ((doc (counsel-extra-annotate-transform-get-function-doc
                                             sym)))
                              (concat
                               (if counsel-extra-align-M-x-description
                                   (propertize " " 'display
                                               `(space :align-to
                                                       ,(or
                                                         counsel-extra-align-M-x-description
                                                         50)))
                                 " ")
                               doc))
                            ""))))
              (cond ((eq sym major-mode)
                     (propertize result 'face 'font-lock-variable-name-face))
                    ((and
                      (memq sym minor-mode-list)
                      (boundp sym)
                      (buffer-local-value sym buff))
                     (propertize result 'face 'font-lock-builtin-face))
                    (t result))))))
      name))

(defun counsel-extra-M-X-find-symbol-in-other-window (it)
  "Find symbol definition that corresponds to string IT in other window."
  (counsel-extra-call-in-other-window 'counsel-extra-find-symbol it))

;;;###autoload
(defun counsel-extra-M-X-find-symbol-in-other-window-cmd ()
  "Quit the minibuffer and find symbol in other window."
  (interactive)
  (ivy-exit-with-action #'counsel-extra-M-X-find-symbol-in-other-window))

(defvar counsel-extra-M-x-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-j") #'counsel-find-symbol)
    (define-key map (kbd "C-c C-l") #'counsel--info-lookup-symbol)
    map))

;;;###autoload
(defun counsel-extra-kill-process ()
  "Kill the currently selected process in the Ivy process list."
  (interactive)
  (counsel-list-processes-action-delete (ivy-state-current ivy-last))
  (with-temp-buffer
    (list-processes--refresh))
  (ivy--kill-current-candidate))

(defvar counsel-extra-process-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'counsel-extra-kill-process)
    map))

;;;###autoload
(defun counsel-extra-list-processes ()
  "Offer completion for `process-list'.

The default action is to switch to the process buffer.
An extra action allows to delete the selected process."
  (interactive)
  (with-temp-buffer
    (list-processes--refresh))
  (ivy-read "Process: " (mapcar #'process-name (process-list))
            :require-match t
            :keymap counsel-extra-process-map
            :action
            '(2
              ("o" counsel-list-processes-action-delete "kill")
              ("s" counsel-list-processes-action-switch "switch"))
            :caller 'counsel-extra-list-processes))

(defun counsel-extra-M-X-action (cmd)
  "Execute or describe command CMD dependending on whether minibuffer is live.

If the minibuffer window is active, describe CMD with the `helpful-command' or
 `describe-command', otherwise call `counsel-M-x-action' with CMD."
  (if
      (minibuffer-window-active-p (active-minibuffer-window))
      (funcall (if (fboundp 'helpful-command)
                   'helpful-command
                 'describe-command)
               (intern-soft cmd))
    (setq counsel-extra-M-X-last-command ivy--index)
    (funcall #'counsel-M-x-action cmd)))

(defun counsel-extra-M-X-unwind-fn ()
  "Set value of the variable `ivy-text' to `counsel-extra-M-x-initial-input'."
  (setq counsel-extra-M-x-initial-input ivy-text))

;;;###autoload
(defun counsel-extra-M-x ()
  "Extra version of `execute-extended-command'."
  (interactive)
  (setq this-command last-command)
  (setq real-this-command real-last-command)
  (setq counsel-extra-M-X-externs
        (counsel--M-x-externs))
  (ivy-configure 'counsel-extra-M-x
    :display-transformer-fn #'counsel-extra-annotate-transform-function-name)
  (ivy-read (counsel--M-x-prompt) counsel-extra-M-X-externs
            :predicate (if counsel-extra-M-X-externs
                           #'counsel--M-x-externs-predicate
                         (counsel--M-x-make-predicate))
            :require-match t
            :initial-input counsel-extra-M-x-initial-input
            :history 'counsel-M-x-history
            :preselect (or counsel-extra-M-X-last-command
                           (ivy-thing-at-point))
            :unwind #'counsel-extra-M-X-unwind-fn
            :keymap counsel-extra-M-x-keymap
            :action #'counsel-extra-M-X-action
            :caller 'counsel-extra-M-x))

(ivy-set-actions 'counsel-extra-M-x
                 '(("j" counsel--find-symbol "Jump to defintion")
                   ("d" counsel-extra-M-X-find-symbol-in-other-window
                    "Jump to symbol other window")
                   ("l" counsel-info-lookup-symbol "Lookup in the info docs")))

(defun counsel-extra-imenu-action-other-window (item)
  "Open the selected imenu ITEM in another window.
Argument ITEM is the selected item."
  (let ((buff (current-buffer)))
    (select-window (or
                    (window-right (selected-window))
                    (window-left (selected-window))
                    (split-window-right)))
    (pop-to-buffer-same-window buff t)
    (funcall (counsel-extra-get-imenu-action) item)))

(defun counsel-extra-imenu-insert (item)
  "Insert the extra imenu ITEM ITEM into the counsel extra buffer.
Argument ITEM is the item to be inserted."
  (counsel-extra-insert
   (car
    (seq-drop-while
     (apply-partially #'string-match-p ":$")
     (seq-filter #'stringp
                 (split-string (if (consp item)
                                   (car item)
                                 item)
                               nil t))))))

;;;###autoload
(defun counsel-extra-imenu-insert-cmd ()
  "Quit the minibuffer and insert imenu item."
  (interactive)
  (ivy-exit-with-action 'counsel-extra-imenu-insert))

;;;###autoload
(defun counsel-extra-imenu-jump-to-item-in-other-window ()
  "Jump to imenu item in other window."
  (interactive)
  (ivy-exit-with-action #'counsel-extra-imenu-action-other-window))

(defun counsel-extra-get-imenu-action ()
  "Return corresponding aciton for `counsel-imenu' or `imenu'."
  (if (eq (ivy-state-caller ivy-last) 'counsel-imenu)
      #'counsel-imenu-action
    #'imenu))

(defun counsel-extra-imenu-default (item)
  "Jump to imenu ITEM either in its original window or other window.
If the completion was successfully selected jump it original window."
  (if ivy-exit
      (funcall (counsel-extra-get-imenu-action) item)
    (with-ivy-window
      (counsel-extra-imenu-action-other-window item))))

(ivy-set-actions 'counsel-imenu
                 `(("o" counsel-extra-imenu-default "jump to item")
                   ("j" counsel-extra-imenu-action-other-window
                    "jump in other window")
                   ("i" counsel-extra-imenu-insert "insert")))

(ivy-set-actions 'imenu
                 `(("o" counsel-extra-imenu-default "jump to item")
                   ("j" counsel-extra-imenu-action-other-window
                    "jump in other window")
                   ("i" counsel-extra-imenu-insert "insert")))



(make-face 'counsel-extra-highlight-minibuffer-face)

(defun counsel-extra-colors--name-to-hex (name)
  "Return hexadecimal RGB value of color with NAME.

Return nil if NAME does not designate a valid color."
  (require 'color)
  (let ((rgb (color-name-to-rgb name)))
    (when rgb
      (when (fboundp 'color-rgb-to-hex)
        (apply #'color-rgb-to-hex rgb)))))

(defvar-local counsel-extra-flash-bg-remap-cookies nil)

(defun counsel-extra-flash-buffer (&optional color)
  "Add a face remapping with background COLOR to current buffer."
  (require 'face-remap)
  (counsel-extra--flash-buffer-cleanup)
  (when color
    (set-face-attribute 'counsel-extra-highlight-minibuffer-face nil
                        :background color)
    (setq counsel-extra-flash-bg-remap-cookies
          (face-remap-add-relative
           'default
           'counsel-extra-highlight-minibuffer-face))))

(defun counsel-extra--flash-buffer-cleanup ()
  "Remove a face remappings `counsel-extra-flash-bg-remap-cookies' in all buffers."
  (require 'face-remap)
  (dolist (buff (buffer-list))
    (when (buffer-local-value 'counsel-extra-flash-bg-remap-cookies buff)
      (with-current-buffer buff
        (face-remap-remove-relative counsel-extra-flash-bg-remap-cookies)
        (setq counsel-extra-flash-bg-remap-cookies nil)))))

(defun counsel-extra-colors-insert-hex-action (color)
  "Insert hex COLOR."
  (insert (if (nth 3 (syntax-ppss (point)))
              (get-text-property 0 'hex color)
            (format "\"%s\"" (get-text-property 0 'hex color)))))

;;;###autoload
(defun counsel-extra-colors-insert-color ()
  "Insert color without exitting minibuffer."
  (interactive)
  (let ((current
         (or
          (ivy-state-current
           ivy-last)
          ivy-text)))
    (with-selected-window
        (ivy--get-window ivy-last)
      (insert current))))
;;;###autoload
(defun counsel-extra-colors-insert-hex ()
  "Insert hex color and exit minibuffer."
  (interactive)
  (ivy-exit-with-action #'counsel-extra-colors-insert-hex-action))

;;;###autoload
(defun counsel-extra-colors-copy-hex ()
  "Copy hex color and exit minibuffer."
  (interactive)
  (ivy-exit-with-action #'counsel-colors-action-kill-hex))



;;;###autoload
(defun counsel-extra-colors-flash-buffer ()
  "Temporarly change minibuffer background color with selected color."
  (interactive)
  (let ((current
         (or
          (ivy-state-current
           ivy-last)
          ivy-text)))
    (with-selected-window
        (active-minibuffer-window)
      (funcall #'counsel-extra-flash-buffer current))))

(defvar counsel-extra-emacs-colors-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j") #'counsel-extra-colors-flash-buffer)
    (define-key map (kbd "C-c TAB") #'counsel-extra-colors-insert-hex)
    (define-key map (kbd "C-c M-i") #'counsel-extra-colors-insert-color)
    (define-key map (kbd "C-c C-w") #'counsel-extra-colors-copy-hex)
    map))


;;;###autoload
(defun counsel-extra-colors-emacs ()
  "Show a list of all supported colors for a particular frame.

You can insert or kill the name or hexadecimal RGB value of the
selected color.

Unlike `counsel-colors-emacs' it is allows to define extra commands in
it's keymap - `counsel-extra-emacs-colors-map'."
  (interactive)
  (let* ((colors
          (delete nil
                  (mapcar (lambda (cell)
                            (let* ((name (car cell))
                                   (dups (cdr cell))
                                   (hex (counsel-colors--name-to-hex name)))
                              (when hex
                                (propertize name 'hex hex 'dups dups))))
                          (list-colors-duplicates))))
         (counsel--colors-format
          (format "%%-%ds %%s %%s%%s"
                  (apply #'max 0 (mapcar #'string-width colors)))))
    (ivy-read "Emacs color: " colors
              :require-match t
              :history 'counsel-colors-emacs-history
              :keymap counsel-extra-emacs-colors-map
              :action #'counsel-extra-colors-insert-hex-action
              :caller 'counsel-extra-colors-emacs)))

(ivy-set-actions 'counsel-extra-colors-emacs
                 '(("B" counsel-extra-colors-insert-hex-action
                 "Insert hex color")
                   ("j" counsel-extra-flash-buffer "Preview background color")
                   ("i" insert "Insert")
                   ("w" counsel-colors-action-kill-hex "Copy hex color")
                   ("T" counsel-extra-colors-insert-hex-action
                   "Insert hex color")))

(ivy-configure 'counsel-extra-colors-emacs
  :format-fn #'counsel--colors-emacs-format-function)

(provide 'counsel-extra)
;;; counsel-extra.el ends here
;; Local Variables:
;; checkdoc-symbol-words: (counsel-extra-M-x)
;; End:

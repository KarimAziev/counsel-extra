;;; counsel-extra.el --- Extend counsel commands -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/counsel-extra
;; Keywords: lisp, convenience
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1") (ivy "0.14.0") (counsel "0.14.0"))
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
(require 'transient)
(require 'counsel)

(declare-function face-remap-remove-relative "face-remap")
(declare-function face-remap-add-relative "face-remap")
(declare-function bookmark-location "bookmark")
(declare-function bookmark-all-names "bookmark")

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

(defcustom counsel-extra-M-X-predicates (delete-dups
                                         (and (boundp
                                               'read-extended-command-predicate)
                                              (remove nil
                                                      (append
                                                       (list
                                                        read-extended-command-predicate)
                                                       (mapcar (lambda
                                                                 (opt)
                                                                 (or (functionp
                                                                      opt)
                                                                     (seq-find
                                                                      'functionp
                                                                      opt)))
                                                               (cdr
                                                                (get
                                                                 'read-extended-command-predicate
                                                                 'custom-type)))))))
  "Command filtering predicates in `counsel-extra-execute-extended-command'.

Each predicate is a function that accepts two arguments: the command's symbol
and the current buffer.

Predicates can be cycled through during command completion by executing command
`counsel-extra-M-X-cycle-predicates' - which is bound to \\<counsel-extra-M-x-keymap>\\[counsel-extra-M-X-cycle-predicates]
in `counsel-extra-M-x-keymap'.

The default predicates are derived from the `read-extended-command-predicate'
variable if it is set, including any related custom types defined for it."
  :group 'counsel-extra
  :type '(repeat function))

(defcustom counsel-extra-max-files-sorting-length 1000
  "Maximum number of files before disabling `counsel-extra-ivy-sort-file-function'."
  :group 'counsel-extra
  :type 'integer)


(defun counsel-extra--preview-file (file)
  "Preview FILE in other window.
This function doesn't really visit FILE to avoid unwanted side effects, such
as running find file hooks, starting lsp or eglot servers and so on."
  (cond ((or (not file)
             (not (file-exists-p file)))
         (message "File is not exists"))
        ((not (file-readable-p file))
         (message "File is not readable"))
        ((and large-file-warning-threshold
              (let ((size
                     (file-attribute-size
                      (file-attributes
                       (file-truename file)))))
                (and size
                     (> size
                        large-file-warning-threshold)
                     (message
                      "File is too large (%s) for preview "
                      (file-size-human-readable size))))))
        (t (with-minibuffer-selected-window
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
                           (abbreviate-file-name file))))))))))

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
  (let ((filename (expand-file-name str (or ivy--directory
                                            (ivy-state-directory
                                             ivy-last)))))
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
  (when-let* ((bounds (if (use-region-p)
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

(defun counsel-extra-insert (item &optional separator word-pattern)
  "Insert text with optional prefix handling.

Argument ITEM is the string or cons cell to be inserted.

Optional argument SEPARATOR is the string used to separate ITEM from the current
word. It defaults to a single space.

Optional argument WORD-PATTERN is a regular expression pattern that defines what
constitutes a word for the purpose of insertion."
  (let ((parts))
    (setq item (if (consp item)
                   (car item)
                 item))
    (setq parts
          (if-let* ((current-word (counsel-extra-get-word
                                  (or word-pattern
                                      "-*_~$A-Za-z0-9:#\\+"))))
              (progn
                (if (string-prefix-p current-word item t)
                    (list (substring-no-properties
                           item (length current-word)))
                  (list (or separator "\s") item)))
            (list item)))
    (apply #'insert parts)))

(defun counsel-extra--get-completion-prefix (item)
  "Extract prefix for completion from ITEM at point.

Argument ITEM is a string representing the completion item."
  (let* ((pos (point))
         (item-chars (reverse (append item nil)))
         (char (char-before pos)))
    (catch 'found
      (while
          (when-let* ((chars (member char item-chars)))
            (setq item-chars (cdr chars))
            (let* ((str (mapconcat #'char-to-string (reverse chars) ""))
                   (beg (- pos
                           (length str)))
                   (prefix (and (>= beg (point-min))
                                (buffer-substring-no-properties beg pos))))
              (if (and prefix
                       (string-prefix-p prefix str))
                  (throw 'found (substring-no-properties item (length prefix)))
                t)))))))

(defun counsel-extra--insert (item)
  "Insert ITEM or its completion prefix into the buffer.

Argument ITEM is a string that will be inserted into the buffer."
  (when item
    (insert (or (counsel-extra--get-completion-prefix item) item))))

(defun counsel-extra-expand-file-when-exists (name &optional directory)
  "Expand filename NAME to absolute if it exits.

Second arg DIRECTORY is directory to start with if FILENAME is relative.
If DIRECTORY is nil or missing, the current buffer's value of
`default-directory'is used."
  (when-let* ((file (when name
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
  (if-let* ((marked-str
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

(defun counsel-extra-expand-dir-maybe-action (curr)
  "Expand directory or handle file based on its type and extension.

Argument CURR is the current file or directory name to be processed.

Usage example:

\\=(ivy-add-actions \\='read-file-name-internal
                 (list \\='(\"o\" counsel-extra-expand-dir-maybe-action \"Default\")))"
  (let ((wnd (active-minibuffer-window)))
    (when wnd
      (with-selected-window wnd
        (let ((dir))
          (cond ((and
                  (> ivy--length 0)
                  (not (string= curr "./"))
                  (setq dir
                        (ivy-expand-file-if-directory
                         curr)))
                 (counsel-extra--ivy-cd dir))
                ((let ((ext (file-name-extension curr)))
                   (when ext
                     (let ((extensions (if
                                           (boundp
                                            'counsel-find-file-extern-extensions)
                                           counsel-find-file-extern-extensions
                                         '("mp4" "mkv" "xlsx" "png" "jpg" "jpeg"
                                           "webm" "3gp" "MOV"))))
                       (or (member ext extensions)
                           (member (downcase ext) extensions)
                           (member (upcase ext) extensions)))))
                 (counsel-find-file-extern
                  (expand-file-name curr ivy--directory)))
                (t (counsel-extra--preview-file
                    (expand-file-name curr ivy--directory)))))))))


;;;###autoload
(defun counsel-extra-expand-dir-maybe ()
  "Handle selected minibuffer file action without closing minibuffer.

This function examines the currently selected file in the minibuffer and
performs one of the following actions based on the file type:

- If the selected file is a directory, it expands this directory within the
  minibuffer, allowing further navigation within it.

- If the selected file has an extension that is listed in
  `counsel-find-file-extern-extensions', the file is opened with an external
  application associated with its file type. This list can be customized to
  support opening additional file types with external applications.

- Otherwise, the selected file is previewed in a temporary buffer. This allows
  for a quick look at the file contents without opening it in a permanent buffer
  or leaving the minibuffer."
  (interactive)
  (when-let* ((curr (ivy-state-current ivy-last)))
    (counsel-extra-expand-dir-maybe-action curr)))

(defun counsel-extra--ivy-cd (dir)
  "Change directory with optional file sorting inhibition based on DIR.

Argument DIR is the directory to change into using Ivy."
  (if (counsel-extra--file-sorting-in-dir-p dir)
      (counsel-extra--apply-fn-inhibit-sorting (lambda (&rest args)
                                                 (apply #'ivy--cd args)
                                                 (ivy--exhibit))
                                               dir)
    (ivy--cd dir)
    (ivy--exhibit)))

;;;###autoload
(defun counsel-extra-expand-dir-done ()
  "Expand directory in Ivy completion or finish selection."
  (interactive)
  (let ((curr (ivy-state-current ivy-last))
        (dir))
    (if (not (and
              (> ivy--length 0)
              (not
               (string= curr "./"))
              (setq dir
                    (ivy-expand-file-if-directory
                     curr))))
        (ivy-done)
      (counsel-extra--ivy-cd dir))))

;;;###autoload
(defun counsel-extra-dired ()
  "Open Dired in the parent directory of the selected file."
  (interactive)
  (when-let* ((filename (counsel-extra-expand-file-when-exists
                         (ivy-state-current
                          ivy-last)
                         ivy--directory))
              (parent (if (file-accessible-directory-p filename)
                          filename
                        (file-name-as-directory
                         (counsel-extra-file-parent filename)))))
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
    :display-transformer-fn #'counsel-extra-read-file-display-transformer)
  (ivy-add-actions 'read-file-name-internal
                   '(("o" counsel-extra-expand-dir-maybe-action "default")))
  (add-to-list 'ivy-sort-functions-alist
               '(read-file-name-internal
                 . (counsel-extra-ivy-sort-file-function
                    ivy-sort-file-function-default)))
  (advice-add 'counsel-find-file
              :around #'counsel-extra-find-file-advice))

(defun counsel-extra--file-sorting-in-dir-p (dir)
  "Check if DIR has more files than `counsel-extra-max-files-sorting-length'.

Argument DIR is the directory to check for file accessibility and count."
  (when (file-accessible-directory-p dir)
    (> (length (directory-files dir nil nil t))
       counsel-extra-max-files-sorting-length)))

(defun counsel-extra--apply-fn-inhibit-sorting (fn &rest args)
  "Temporarily disable sorting in `ivy-sort-functions-alist' and apply FN.

Argument FN is a function to be applied with sorting temporarily inhibited.

Remaining arguments ARGS are the arguments passed to the function FN."
  (let ((removed-cells))
    (unwind-protect
        (progn
          (dolist (cell ivy-sort-functions-alist)
            (cond ((and (consp (cdr cell))
                        (eq 'counsel-extra-ivy-sort-file-function
                            (cadr cell)))
                   (setcdr cell (nconc (cddr cell)
                                       (list (cadr cell))))
                   (push cell removed-cells))
                  ((eq 'counsel-extra-ivy-sort-file-function (cdr cell))
                   (push cell removed-cells)
                   (setcdr cell #'ivy-sort-file-function-default))))
          (apply fn args))
      (dolist (cell removed-cells)
        (cond ((and (consp (cdr cell))
                    (not (eq 'counsel-extra-ivy-sort-file-function
                             (cadr cell))))
               (delq 'counsel-extra-ivy-sort-file-function (cdr cell))
               (setcdr cell (nconc
                             (list 'counsel-extra-ivy-sort-file-function)
                             (cdr cell))))
              (t (setcdr cell 'counsel-extra-ivy-sort-file-function)))))))

(defun counsel-extra-find-file-advice (&optional fn initial-input
                                                 initial-directory &rest args)
  "Apply FN to find files and possibly disable sorting.

If the target directory contains more files than specified in
the custom variable `counsel-extra-max-files-sorting-length', the function
will temporarily remove `counsel-extra-ivy-sort-file-function' from
`ivy-sort-functions-alist'.

Argument FN is the advised function, which is expected to be
`counsel-find-file'.

Optional argument INITIAL-INPUT is the initial input for the function.

Optional argument INITIAL-DIRECTORY is the starting directory for the file
search.

Remaining arguments ARGS are additional arguments passed to the function FN.

This function is intended to be used as an around advice:

\\=(advice-add \\='counsel-find-file
            :around #\\='counsel-extra-find-file-advice)."
  (when fn
    (let ((inhibit-sort-fns))
      (let ((tramp-archive-enabled nil))
        (let ((preselect (counsel--preselect-file))
              (dir (if (eq major-mode 'dired-mode)
                       (dired-current-directory)
                     (or initial-directory default-directory))))
          (when (and preselect (file-exists-p preselect))
            (let ((directory
                   (let* ((expanded-filename (expand-file-name preselect))
                          (parent (file-name-directory (directory-file-name
                                                        expanded-filename))))
                     (cond ((or (null parent)
                                (equal parent expanded-filename))
                            nil)
                           ((not (file-name-absolute-p preselect))
                            (file-relative-name parent))
                           (t
                            parent)))))
              (when (file-accessible-directory-p directory)
                (setq dir directory))))
          (setq inhibit-sort-fns (counsel-extra--file-sorting-in-dir-p dir))))
      (if (not inhibit-sort-fns)
          (apply fn initial-input initial-directory args)
        (apply #'counsel-extra--apply-fn-inhibit-sorting fn initial-input
               initial-directory
               args)))))

(defvar counsel-extra-M-X-last-command nil)
(defvar counsel-extra-M-X-externs nil)
(defvar counsel-extra-M-x-initial-input nil)


(defun counsel-extra-annotate-transform-get-function-key (sym buffer)
  "Return string with active key for command SYM in BUFFER.
SYM should be a symbol."
  (when (commandp sym)
    (with-current-buffer buffer
      (let ((k (where-is-internal sym nil t)))
        (when (and k (eq sym (key-binding k)))
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
  (when-let* ((documentation
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
               (if-let* ((minw (minibuffer-selected-window)))
                   (with-selected-window minw
                     (current-buffer))
                 (current-buffer)))
              (sym))
          (setq sym (intern name))
          (when (symbolp sym)
            (let ((result (string-trim-right
                           (concat
                            name "\s"
                            (or
                             (when-let* ((key (counsel-extra-annotate-transform-get-function-key
                                               sym buff)))
                               key)
                             "")
                            (or
                             (when-let* ((doc (counsel-extra-annotate-transform-get-function-doc
                                               sym)))
                               (concat
                                (if
                                    counsel-extra-align-M-x-description
                                    (propertize " " 'display
                                                `(space
                                                  :align-to
                                                  ,(or
                                                    counsel-extra-align-M-x-description
                                                    50)))
                                  " ")
                                doc))
                             "")))))
              (cond ((eq sym major-mode)
                     (propertize result 'face 'font-lock-variable-name-face))
                    ((and
                      (memq sym minor-mode-list)
                      (boundp sym)
                      (buffer-local-value sym buff))
                     (propertize result 'face 'font-lock-builtin-face))
                    (t result))))))
      name))

(defun counsel-extra-M-X-find-symbol-in-other-window (name)
  "Find symbol in a new window.

Argument NAME is a string representing the symbol to find."
  (counsel-extra-call-in-other-window 'counsel-extra-find-symbol name))

;;;###autoload
(defun counsel-extra-M-X-find-symbol-in-other-window-cmd ()
  "Quit the minibuffer and find symbol in other window."
  (interactive)
  (ivy-exit-with-action #'counsel-extra-M-X-find-symbol-in-other-window))

(defvar counsel-extra-M-x-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-j") #'counsel-find-symbol)
    (define-key map (kbd "C-c C-l") #'counsel--info-lookup-symbol)
    (define-key map (kbd "M-X") #'counsel-extra-M-X-cycle-predicates)
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


(defun counsel-extra-M-X-cycle-predicates ()
  "Cycle through command filtering predicates."
  (interactive)
  (when (boundp 'read-extended-command-predicate)
    (let ((next (cadr (memq read-extended-command-predicate
                            (append (list nil)
                                    counsel-extra-M-X-predicates)))))
      (ivy-quit-and-run
        (let* ((read-extended-command-predicate
                next)
               (prompt
                (string-join
                 (delq nil
                       (list
                        (counsel--M-x-prompt)
                        (cond ((not
                                read-extended-command-predicate)
                               "(No filters) ")
                              ((symbolp
                                read-extended-command-predicate)
                               (format "(%s) "
                                       (symbol-name
                                        read-extended-command-predicate))))))
                 "")))
          (counsel-extra--M-x-read
           prompt
           (or counsel-extra-M-X-externs
               obarray)))))))


(defun counsel-extra--M-x-read (prompt collection)
  "Read command name from a COLLECTION.

Argument PROMPT is a string to display as the prompt in the minibuffer.

Argument COLLECTION is a list or array to be used for completion."
  (ivy-read prompt collection
            :predicate
            (let ((buf (current-buffer)))
              (lambda (str)
                (when-let* ((sym (intern-soft str)))
                  (and (commandp sym)
                       (not (get sym 'byte-obsolete-info))
                       (not (get sym 'byte-obsolete-info))
                       (not (eq (get sym 'command-predicate)
                                'transient--suffix-only))
                       (cond ((not (bound-and-true-p
                                    read-extended-command-predicate)))
                             ((functionp read-extended-command-predicate)
                              (condition-case-unless-debug err
                                  (funcall read-extended-command-predicate sym
                                           buf)
                                (error (message
                                        "read-extended-command-predicate: %s: %s"
                                        sym (error-message-string err))))))))))
            :require-match t
            :initial-input counsel-extra-M-x-initial-input
            :history 'counsel-M-x-history
            :preselect (or counsel-extra-M-X-last-command
                           (ivy-thing-at-point))
            :unwind #'counsel-extra-M-X-unwind-fn
            :keymap counsel-extra-M-x-keymap
            :action #'counsel-extra-M-X-action
            :caller 'counsel-extra-M-x))

;;;###autoload
(defun counsel-extra-M-x ()
  "Extra version of `execute-extended-command'.

Execute \\<counsel-extra-M-x-keymap>\\[counsel-extra-M-X-cycle-predicates] (`counsel-extra-M-X-cycle-predicates')
during command completion to cycle through filtering predicates defined in
`counsel-extra-M-X-cycle-predicates'.

Other commands available during command completion:

\\<counsel-extra-M-x-keymap>\\{counsel-extra-M-x-keymap}."
  (interactive)
  (setq this-command last-command)
  (setq real-this-command real-last-command)
  (setq counsel-extra-M-X-externs
        (counsel--M-x-externs))
  (ivy-configure 'counsel-extra-M-x
    :display-transformer-fn #'counsel-extra-annotate-transform-function-name)
  (counsel-extra--M-x-read (counsel--M-x-prompt)
                           (or counsel-extra-M-X-externs
                               obarray)))

(defalias 'counsel-extra-execute-extended-command
  #'counsel-extra-M-x)

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

;;;###autoload
(defun counsel-extra-search ()
  "Search dynamically with Ivy interface and insert results."
  (interactive)
  (require 'request)
  (require 'json)
  (let* ((word-pattern "-*_~A-Za-z0-9:$")
         (input (counsel-extra-get-word word-pattern)))
    (minibuffer-with-setup-hook
        (lambda ()
          (when (and input
                     (minibufferp)
                     (string-empty-p (string-trim
                                      (minibuffer-contents-no-properties))))
            (insert input)))
      (ivy-read "search: " #'counsel-search-function
                :action #'counsel-extra--insert
                :dynamic-collection t
                :caller 'counsel-extra-search))))

;;;###autoload (autoload 'counsel-extra-color-menu "counsel-extra" nil t)
(transient-define-prefix counsel-extra-color-menu ()
  [[("c" "emacs colors" counsel-extra-colors-emacs)
    ("w" "web colors" counsel-colors-web)
    ("t" "text properties at point" describe-text-properties)
    ("l" "load theme" counsel-load-theme)
    ("f" "faces list" counsel-faces)
    ("F" "customize face" customize-face)
    ("d" "describe face" counsel-describe-face)
    ("o" "fonts" counsel-fonts)]])

(provide 'counsel-extra)
;;; counsel-extra.el ends here
;; Local Variables:
;; checkdoc-symbol-words: (counsel-extra-M-x)
;; End:

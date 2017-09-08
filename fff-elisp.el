;;; fff-elisp.el --- find emacs lisp libaries and function definitions

;; Copyright (C) 1996, 97, 99, 04, 2005 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions, searching, files, commands, tools
;; Created: 1996-03-26; split from fff.el 1999-10-28

;; $Id: fff-elisp.el,v 1.14 2017/08/28 18:04:13 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

;; This package is an extension of fff.el which provides shortcut commands
;; for visiting emacs lisp libraries based on file name or function defined
;; therein.

;; To use this package, put the following in your .emacs:
;;
;;   (autoload 'fff-find-emacs-lisp-library                "fff" nil t)
;;   (autoload 'fff-insert-emacs-lisp-library              "fff" nil t)
;;   (autoload 'fff-locate-emacs-lisp-library              "fff" nil t)
;;   (autoload 'fff-find-loaded-elisp-function-or-variable "fff" nil t)

;; The command `fff-elisp-install-map' will bind these commands to a common
;; prefix of "C-c C-f" (you can change this).  To find a list of them, run
;; that command and then type "C-c C-f C-h".
;;
;; If you are using Emacs 19 or later, you can have this happen
;; automatically by putting the following in your .emacs:
;;
;;     (eval-after-load "fff-elisp" '(fff-elisp-install-map))

;; Related works:
;;    * find-func.el by Jens Petersen <petersen@kurims.kyoto-u.ac.jp>

;;; Code:

(require 'fff)

(defvar fff-emacs-lisp-def-regexp
  "^\\s-*(\\s-*def\\S-+\\s-+'?\\(%s\\)\\(\\s-\\|$\\)"
  "The regexp used to find symbol definitions in an emacs lisp source file.
This regexp must contain a `%s' where the symbol name is to be inserted in
the template.
If \\\(\\\) registers are included in the regular expression so that \\1
matches, point will be positioned at that match instead of \\0.")

(defvar fff-emacs-lisp-subr-regexp "^\\s-*DEFUN\\s-*(\\s-*\"%s\""
    "The regexp used to find function definitions in an emacs C source file.
This regexp must contain a `%s' where the symbol name is to be inserted in
the template.
If \\\(\\\) registers are included in the regular expression so that \\1
matches, point will be positioned at that match instead of \\0.")

(defvar fff-emacs-lisp-library-completion-table nil
  "Used by `fff-elisp-complete-emacs-lisp-library' to cache completions.
That function resets this variable if load-path changes.  However, it
will not be updated automatically if libraries are added to existing
directories.  Use `fff-elisp-flush-library-completion-table' to reset the
cache.")

(defconst fff-emacs-library-suffixes
  '(".el" ".el.gz" ".gz" ""))


;;;###autoload
(defun fff-find-emacs-lisp-library (lib &optional which)
  "Visit the first emacs lisp source file named LIB.
The variable `load-path' is searched for candidates.

If no matches are found in load-path, but a lisp file was loaded by that
name previously and that fact is recorded in the variable `load-history',
then visit that file instead.

If called interactively with a generic prefix argument and there is more
than one possible match, a list is displayed from which the user can select
the desired match.  If called from a program with a non-numeric value for
WHICH and there is more than one match, an error is signalled.

If called interactively with a numeric prefix argument N and
there are at least N many matches, the Nth file will be visited.

If called interactively, you may attempt to complete a name in the
minibuffer if that library has previously been loaded.

If no matches are found, an error is signalled."
  (interactive (list (fff-completing-read-emacs-lisp-library
                      "Find library (fff emacs-lisp): ")
                     current-prefix-arg))
  (fff-<op>-emacs-lisp-library lib which fff-match-predicate
                               'find-file (interactive-p)))

;;;###autoload
(defun fff-insert-emacs-lisp-library (lib &optional which)
  "Insert the emacs lisp source file named LIB in the current buffer.
This function behaves exactly like `fff-find-emacs-lisp-library', except
that the contents of the library file is inserted in the current buffer
instead of being visited in another buffer."
  (interactive (list (fff-completing-read-emacs-lisp-library
                      "Insert library (fff emacs-lisp): ")
                     current-prefix-arg))
  (fff-<op>-emacs-lisp-library lib which fff-match-predicate
                               'insert-file (interactive-p)))

(defun fff-<op>-emacs-lisp-library (lib &optional which pred op interactivep)
  (let ((file (fff-locate-emacs-lisp-library lib which pred
                                             fff-emacs-library-suffixes))
        (lib-sym))
    (cond ((fff-length1-p file)
           (message "%s" (car file))
           (funcall op (car file)))
          ((null file)
           (setq file
                 (cond ((fff-locate-loaded-emacs-lisp-library lib))
                       ((and (stringp lib)
                             (setq lib-sym (intern-soft lib)))
                        (fff-locate-loaded-emacs-lisp-library lib-sym))))
           (cond ((stringp file)
                  (setq file
                        (or (fff-emacs-lisp-bytecode-source-file-name file)
                            file))
                  (funcall op file)
                  (message "Library %s not found in load-path, %s"
                           lib "but found in load-history."))
                 (t
                  (signal 'file-error
                          (list (format "Library %s not found in load-path"
                                        lib))))))
          ((and (numberp which)
                (<= which (length file)))
           (funcall op (nth (1- which) file)))
          (t
           (if interactivep
               (fff-display-matches lib file op)
             (signal 'file-error
                     (list (format "Multiple instances of %s in load-path" lib)
                           file)))))))

;;;###autoload
(defun fff-locate-emacs-lisp-library (lib &optional which pred suffixes)
  "Return a list of all files named LIB in the Emacs Lisp load-path.
If called interactively, display the name of the first file found.  When
calling from a program, this is the same as passing `nil' as the second
argument.

If called interactively with a generic prefix argument, display the file
names of those libraries.

If called interactively with a numeric prefix argument WHICH and there are
at least WHICH many matches, display the name of the WHICH'th occurence of
that library.

Optional third argument PRED can be an arbitrary function of one argument
\(e.g. 'file-readable-p\), which should return non-`nil' if a file name
candidate should be returned.

If called from a program, the optional fourth argument SUFFIXES may
provide a list of suffixes to try before trying the literal LIB name,
e.g. '\(\".elc\" \".el\" \"\"\).  If not provided, no suffixes are tried."
  (interactive (list (fff-completing-read-emacs-lisp-library
                      "Locate library (fff emacs-lisp): ")
                     current-prefix-arg
                     nil
                     fff-emacs-library-suffixes))
  (let* ((names (if suffixes
                    (fff-suffix lib suffixes)
                  (list lib)))
         (matches (fff-files-in-directory-list names load-path
                                               (not which) pred)))

    ;; If a specific entry was requested, operate on only that result.
    (cond ((and (numberp which)
                (<= which (length matches)))
           (setq matches (nthcdr (1- which) matches))
           (setcdr matches nil)))

    (and (interactive-p)
         (cond ((null matches)
                (message "%s not found in load-path" lib))
               ;; redisplay-dont-pause is an Emacs 21 variable.  If
               ;; defined, it means that the echo area is dynamically
               ;; resized when a message is too large for the current
               ;; height.  In that case, we don't need to display in a
               ;; temporary buffer.
               ((or (boundp 'redisplay-dont-pause)
                    (and (fff-length1-p matches)
                         (> (window-width (minibuffer-window))
                            (length (car matches)))))
                (message "%s" (car matches)))
               (t
                (fff-display-matches lib matches))))
    matches))

;;;###autoload
(defun fff-find-loaded-emacs-lisp-function-or-variable (symbol)
  "Visit the file which contains the currently-loaded definition of SYMBOL.
Point is positioned at the beginning of the definition if it can be
located.

If the definition was loaded from a byte-compiled file, an attempt is made
to locate the corresponding source file.
First, look for the source file mentioned in the bytecode comment headers.
Next, try looking for the source file in the same directory as the bytecode.
Next, search for the first analogously-name source file in load-path.

This command only works in those versions of Emacs/XEmacs which have the
`load-history' variable."
  (interactive (list (fff-completing-read-emacs-lisp-symbol
                      "Find function or var"
                      (fff-emacs-lisp-function-or-variable-at-point))))
  (if (and (fboundp symbol)
           (subrp (symbol-function symbol)))
      (fff-find-emacs-subr symbol)
    (let ((name (fff-emacs-lisp-function-loadfile symbol))
          (srcname nil)
          (altname nil))
      (cond (name
             (setq srcname (fff-emacs-lisp-bytecode-source-file-name name))
             (save-match-data
               (cond ((and srcname
                           (file-exists-p srcname))
                      (find-file srcname)
                      (and (file-newer-than-file-p srcname name)
                           (message "Warning: source file newer than %s"
                                    "byte-compiled file")))
                     ((string-match "[^/]+\\.elc" name)
                      (setq altname (substring name 0 -1))
                      (or (file-exists-p altname)
                          (setq altname
                                (car (fff-locate-emacs-lisp-library
                                      (substring name (match-beginning 0) -1)))))
                      (cond ((and altname
                                  (file-exists-p altname))
                             (find-file altname)
                             (unless (string= altname (substring name 0 -1))
                               ;; source file was not in the same directory as elc
                               (message "Warning: source file may not %s"
                                        "correspond to byte-compiled file")))
                            (t (find-file name))))
                     (t (find-file name))))
             (fff-emacs-lisp-goto-definition symbol))
            (t
             (error "%s not defined in any currently-loaded file" symbol))))))

(defun fff-find-emacs-subr (symbol)
  (let ((file (fff-emacs-subr-source-file symbol))
        (re (format fff-emacs-lisp-subr-regexp symbol)))
    (find-file file)
    (save-match-data
      (cond ((re-search-forward re nil t)
             (if (match-beginning 1)
                 (goto-char (match-beginning 1))
               (goto-char (match-beginning 0))))
            (t
             (error "Cannot find definition of %s" symbol))))))

;; Return the name of the file the function was, or would be, loaded from.
;; This is not necessarily a source file; it might be an elc file.
;; Returns nil if no load file can be found.
(defun fff-emacs-lisp-function-loadfile (symbol)
  (let* ((fn (and (fboundp symbol)
                  (symbol-function symbol)))

         (name (cond ((and (fboundp 'byte-code-function-p)
                           (byte-code-function-p fn)
                           (>= (length fn) 5)
                           (consp (aref fn 4)))
                      (car (aref fn 4)))

                     ((and (consp fn)
                           (eq (car fn) 'autoload))
                      (car (cdr fn)))

                     (t
                      (fff-elisp-load-history-file-name
                       (fff-elisp-load-history-elt-by 'symbol symbol))))))
    (cond ((null name) nil)
          ((not (file-name-absolute-p name))
           (car (fff-locate-emacs-lisp-library
                 name nil nil fff-emacs-library-suffixes)))
          (t name))))

;; Use information in DOC file to locate source file of subr
(defun fff-emacs-subr-source-file (symbol)
  (unless (or (subrp symbol)
              (subrp (symbol-function symbol)))
    (error (format "%s is not a subr" symbol)))

  (let ((doc-file (concat doc-directory "/DOC"))
        (name (if (subrp symbol)
                  (subr-name symbol)
                (symbol-name symbol)))
        (file nil))
    (when (file-exists-p doc-file)
      (with-temp-buffer
        (insert-file-contents-literally
         (expand-file-name internal-doc-file-name doc-directory))
        (while (null file)
          (let ((p (search-forward (concat "\C-_F" name "\n"))))
            (re-search-backward "\C-_S\\(.*?\\)\n")
            (let ((obj (match-string 1)))
              (if (member obj build-files)
                  (setq file obj)
                (goto-char p)))))
        (cond ((string-match "^ns.*\\(\\.o\\)\\'" file)
               (setq file (replace-match ".m" t t file 1)))
              ((string-match "\\.\\(o\\|obj\\)\\'" file)
               (setq file (replace-match ".c" t t file))))
        (if (string-match "\\.\\(c\\|m\\)\\'" file)
          (format "%ssrc/%s" source-directory file)
          file)))))

(defun fff-emacs-lisp-goto-definition (symbol)
  (save-match-data
    (let ((p (point))
          (re (format fff-emacs-lisp-def-regexp symbol))
          (syntable (syntax-table)))
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (goto-char (point-min))
      (if (prog1
              (re-search-forward re nil t)
            (set-syntax-table syntable))
          (if (match-beginning 1)
              (goto-char (match-beginning 1))
            (goto-char (match-beginning 0)))
        (goto-char p)
        (error "Cannot find definition of %s" symbol)))))

;; If a library cannot be found directly in the load-path, try searching
;; for it in the list of libraries which have already been loaded.
;; `library' can be a string or a symbol; if the latter, it should be the
;; name of a feature which is known to be provided.
;; If the absolute pathname of the library cannot be found, or if the
;; file no longer seems to exists, return nil.
;;
;; This function depends on load-history, which is missing in Emacs 18 and
;; XEmacs 19.15 or earlier.
(defun fff-locate-loaded-emacs-lisp-library (lib)
  (cond ((and (boundp 'load-history)
              load-history)
         (let (data)
           (and (symbolp lib)
                (featurep lib)
                (setq data (fff-elisp-load-history-elt-by 'feature lib)))
           (cond ((null data)
                  (and (symbolp lib)
                       (setq lib (symbol-name lib)))
                  (setq data (fff-elisp-load-history-elt-by 'name lib))))
           (and data
                (fff-elisp-load-history-file-name data))))))

(defun fff-elisp-load-history-elt-by (method key)
  (let ((found nil)
        (hist load-history)
        (elt nil))
    (cond ((eq method 'feature)
           (setq elt (cons 'provide key))
           (while hist
             (if (member elt (car hist))
                 (setq found (car hist)
                       hist nil)
               (setq hist (cdr hist)))))
          ((eq method 'symbol)
           (setq elt (cons 'defun key))
           (while hist
             (if (or (memq key (car hist))
                     (member elt (car hist)))
                 (setq found (car hist)
                       hist nil)
               (setq hist (cdr hist)))))
          ((eq method 'name)
           (while hist
             (setq elt (car (car hist)))
             (if (or (string= key elt)
                     (string= key (setq elt (file-name-nondirectory elt)))
                     (string= key (file-name-sans-extension elt)))
                 (setq found (car hist)
                       hist nil)
               (setq hist (cdr hist))))))
    found))

(defun fff-elisp-load-history-file-name (data)
  (and data
       (let ((dir nil)
             (name nil)
             (names nil))
         (cond ((file-name-absolute-p (car data))
                (setq dir (file-name-directory (car data)))
                (setq name (file-name-nondirectory (car data))))
               (t
                (setq name (car data))))
         (setq names (fff-suffix name fff-emacs-library-suffixes))
         (cond ((null dir)
                (car (fff-files-in-directory-list names load-path t)))
               ((file-exists-p name)
                name)
               (t
                (car (fff-files-in-directory-list names (list dir) t)))))))

;; Return the name of the lisp file from which a bytecoded file was generated.
;; The returned name doesn't necessarily exist; it is extracted from the
;; bytecode file comments.
;; If no name can be found, return nil.
;;
;; n.b. Emacs 24 and later no longer record the source file name in the
;; compiled file.
(defun fff-emacs-lisp-bytecode-source-file-name (elcfile)
  (let ((buf (generate-new-buffer " *emacs lisp bytecode*"))
        (magic ";ELC")
        (source-name nil)
        (size 1024)
        data)
    (unwind-protect
        (save-excursion
          (set-buffer buf)
          (buffer-disable-undo buf)
          (emacs-lisp-mode)
          (setq data (fff-insert-file-contents-next-region elcfile size))
          (save-match-data
            (cond ((< data (length magic)))
                  ((string= (buffer-substring 1 (1+ (length magic))) magic)
                   (let ((case-fold-search t)
                         (re "^;+\\s-+from\\s-+file\\s-+\\(.*\\)\n"))
                     (while (and (> data 0)
                                 (null source-name))
                       (beginning-of-line)
                       (if (re-search-forward re nil t)
                           (setq source-name
                                 (buffer-substring (match-beginning 1)
                                                   (match-end 1)))
                         (setq data (fff-insert-file-contents-next-region
                                     elcfile size)))))))))
      (kill-buffer buf))
    source-name))


(defun fff-completing-read-emacs-lisp-symbol (prompt &optional init default)
  (and init (setq init (cons (if (symbolp init) (symbol-name init) init) 0)))
  ;; Emacs 20.0 and later support the default parameter to completing read.
  ;; XEmacs and earlier versions of Emacs do not.
  (cond ((and (eq (fff-emacs-variant) 'emacs)
              (string-lessp "20" emacs-version)))
        (t
         (setq default nil)))
  (if default
      (setq prompt (format "%s (default %s): " prompt default))
    (setq prompt (format "%s: " prompt)))
  (let* ((pred (function (lambda (s) (or (fboundp s) (boundp s)))))
         (result (if default
                     (completing-read prompt obarray pred t init nil default)
                   (completing-read prompt obarray pred t init nil))))
    (if (symbolp result)
        result
      (intern-soft result))))

;; This definition is fairly trivial now, but it's here so that the
;; interactive behavior of all the callers can be changed a little more
;; easily.
(defun fff-completing-read-emacs-lisp-library (prompt)
  (completing-read prompt 'fff-elisp-complete-emacs-lisp-library))

;; Complete an emacs lisp library name.
;; Compute table cache if necessary.
(defun fff-elisp-complete-emacs-lisp-library (string predicate action)
  (let ((table (car fff-emacs-lisp-library-completion-table))
        (cached-load-path  (cdr fff-emacs-lisp-library-completion-table)))
    (cond ((or (null table)
               (not (equal load-path cached-load-path)))
           (setq fff-emacs-lisp-library-completion-table
                 (cons (fff-elisp-make-emacs-lisp-library-completions)
                       (copy-sequence load-path)))
           (setq table
                 (car fff-emacs-lisp-library-completion-table))))
    (if action
        (all-completions string table predicate)
      (try-completion string table predicate))))

(defun fff-elisp-make-emacs-lisp-library-completions ()
  (let ((table nil))
    ;; Initialize table with files in load path
    (setq table (fff-file-name-completions-in-path
                 nil load-path
                 (function (lambda (s) (string-match "\\.elc?\\(?:\\.gz\\)?$" s)))
                 (function (lambda (s)
                             (if (string-match "\\.elc?\\(?:\\.gz\\)?$" s)
                                 (substring s 0 (match-beginning 0))
                               s)))))
    ;; Now add loaded features, in case they differ from file names.
    (fff-symbol-list->obarray features table)
    ;; Now add files in load history, if available.
    (and (boundp 'load-history)
         (let ((lh load-history)
               (file nil))
           (while lh
             (setq file (car (car lh)))
             (setq lh (cdr lh))

             (cond (file
                    (and (string-match "/" file)
                         (setq file (file-name-nondirectory file)))
                    (and (string-match "\\.elc?\\(?:\\.gz\\)?$" file)
                         (setq file (substring file 0 (match-beginning 0))))

                    (intern file table))))))
    table))

(defun fff-complete-feature (string predicate &optional allp)
  (let ((table (fff-symbol-list->obarray features fff-default-obarray-size))
        (fn (if allp 'all-completions 'try-completion)))
    (funcall fn string table predicate)))

;; Return the name of the function called in the current sexp if fbound and
;; not a subr, else if point is on an fbound or bound symbol, return that.
(defun fff-emacs-lisp-function-or-variable-at-point ()
  (let ((syms (list (fff-function-at-point)
                    (intern-soft (current-word))))
        (sym nil)
        (result nil))
    (while syms
      (setq sym (car syms))
      (setq syms (cdr syms))
      (cond ((null sym))
            ((and (fboundp sym)
                  (subrp (symbol-function sym))))
            ((or (fboundp sym) (boundp sym))
             (setq result sym
                   syms nil))))
    result))

(defalias 'fff-function-at-point
  (if (fboundp 'function-at-point)
      'function-at-point
    'function-called-at-point))


;;;###autoload
(defun fff-elisp-install-map ()
  "Install the fff elisp keymap."
  (interactive)
  (fff-install-map)
  ;; Listed in reverse of desired order so that menu bar will be in
  ;; correct order.
  (fff-define-key "\C-i\C-l"
                  'fff-insert-emacs-lisp-library
                  "Insert emacs lisp library")

  (fff-define-key "\C-d"
                  'fff-find-loaded-emacs-lisp-function-or-variable
                  "Find emacs lisp function or variable definition")

  (fff-define-key "\C-l"
                  'fff-find-emacs-lisp-library
                  "Find emacs lisp library"))

(defun fff-elisp-flush-library-completion-table ()
  "Flush the completeion cache from `fff-emacs-lisp-library-completion-table'."
  (interactive)
  (setq fff-emacs-lisp-library-completion-table nil))

(provide 'fff-elisp)

;;; fff-elisp.el ends here

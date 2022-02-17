;;; dired-tags.el --- Add tags to files in dired     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; Keywords: data, files
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (xattr "0.0.3"))
;; URL: https://github.com/xFA25E/dired-tags

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A mode to edit tags

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'subr-x)
(require 'wid-edit)
(require 'xattr)

(defgroup dired-tags nil
  "Dired tags"
  :group 'dired)

(defcustom dired-tags-xattr-namespace
  "emacs.dired.tags"
  "Extended attributes namespace used to save tags.
Note, this must not be empty string.  It is concatenated to
\"user.\"."
  :type 'string
  :group 'dired-tags)

(defface dired-tags-tag-face
  '((default :inherit font-lock-constant-face)
    (((supports (:box t))) :box t)
    (t                     :inverse-video t))
  "Face used to visualize tags."
  :group 'dired-tags)

(defface dired-tags-file-face
  '((t :inherit font-lock-variable-name-face))
  "Face used to visualize file."
  :group 'dired-tags)

(defun dired-tags--name ()
  "Get extended attribute name."
  (concat "user." dired-tags-xattr-namespace))

(defun dired-tags--encode (tags)
  "Encode TAGS to a value that can be decoded."
  (mapconcat #'url-hexify-string tags ","))

(defun dired-tags--decode (value)
  "Decode tags VALUE to a list of tags."
  (mapcar #'url-unhex-string (split-string value "," t)))

(defun dired-tags--list (file)
  "Return a FILE's tag list."
  (dired-tags--decode (or (xattr-get file (dired-tags--name)) "")))

(defun dired-tags--save (tags file)
  "Save TAGS to FILE.
Return TAGS."
  (let ((name (dired-tags--name)))
    (if tags
        (xattr-set file name (dired-tags--encode tags))
      (xattr-remove file name))
    tags))

(defun dired-tags--add (tag file)
  "Add TAG to FILE.
Return FILE tags."
  (thread-first (cons tag (dired-tags--list file))
    (cl-delete-duplicates :test #'string=)
    (dired-tags--save file)))

(defun dired-tags--remove (tag file)
  "Remove TAG from FILE.
Return FILE tags."
  (dired-tags--save (delete tag (dired-tags--list file)) file))

(defun dired-tags--format-tags (tags)
  "Return a propertized string with TAGS suitable for display."
  (mapconcat (lambda (tag) (propertize tag 'face 'dired-tags-tag-face)) tags " "))

(defun dired-tags--format-file (file)
  "Return a propertize string with FILE suitable for display."
  (propertize file 'face 'dired-tags-file-face))

(defun dired-tags--action (action &optional arg)
  "Apply ACTION on every marked dired file.
ACTION is a function that takes a file as an argument and returns
file tags.  Display file tags at the end for every file.

ARG is the same as in `dired-map-over-marks'."
  (let ((report ""))
    (dired-map-over-marks
     (when-let ((file (dired-get-filename nil t)))
       (cl-callf concat report
         (dired-tags--format-file (dired-get-filename t t)) ": "
         (dired-tags--format-tags (funcall action file)) "\n"))
     arg)
    (message "%s" (string-trim report))))

;;;###autoload
(defun dired-tags-list (&optional arg)
  "Display tags of marked dired files.
ARG is the same as in `dired-map-over-marks'."
  (interactive "P")
  (dired-tags--action #'dired-tags--list arg))

;;;###autoload
(defun dired-tags-add (tag &optional arg)
  "Add TAG to marked dired files.
ARG is the same as in `dired-map-over-marks'."
  (interactive "sTag to add: \nP")
  (cl-assert (not (string-empty-p tag)) nil "Tag must not be empty")
  (dired-tags--action (apply-partially #'dired-tags--add tag) arg))

;;;###autoload
(defun dired-tags-remove (tag &optional arg)
  "Remove TAG from marked dired files.
ARG is the same as in `dired-map-over-marks'."
  (interactive "sTag to remove: \nP")
  (cl-assert (not (string-empty-p tag)) nil "Tag must not be empty")
  (dired-tags--action (apply-partially #'dired-tags--remove tag) arg))

;;;###autoload
(defun dired-tags-mark-regexp (tag-regexp)
  "Mark dired files by TAG-REGEXP."
  (interactive "sTag regexp: ")
  (let ((tag-match-p (apply-partially #'string-match-p tag-regexp)))
    (dired-mark-if
     (when-let ((file (dired-get-filename nil t)))
       (cl-some tag-match-p (dired-tags--list file)))
     "matching tag file")))

(defvar-local dired-tags-edit-widget nil "Tags widget.")

(defun dired-tags-edit-quit (&rest _ignore)
  "Kill tags widget buffer."
  (interactive)
  (when dired-tags-edit-widget
    (quit-window t)))

(defun dired-tags-edit-save (&rest _ignore)
  "Save widget tags to file."
  (interactive)
  (when-let ((widget dired-tags-edit-widget))
    (dired-tags--save
     (cl-delete-duplicates (widget-value widget) :test #'string=)
     (widget-get widget :file)))
  (dired-tags-edit-quit))

(defun dired-tags-edit--make-map (parent)
  "Make tags editor map from PARENT."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map parent)
    (define-key map "\C-c\C-k" #'dired-tags-edit-quit)
    (define-key map "\C-c\C-c" #'dired-tags-edit-save)
    map))

;;;###autoload
(defun dired-tags-edit ()
  "Edit tags of dired file at point."
  (interactive)
  (let ((file (dired-get-filename))
        (local-file (dired-get-filename t)))
    (with-current-buffer (get-buffer-create "*Dired Tags Editor*")
      (kill-all-local-variables)
      (buffer-disable-undo)
      (with-silent-modifications (erase-buffer) (remove-overlays))
      (widget-insert "Edit " (dired-tags--format-file local-file) " tags:\n")
      (let* ((keymap (dired-tags-edit--make-map widget-field-keymap))
             (widget (widget-create 'editable-list :file file
                                    :value (dired-tags--list file)
                                    `(editable-field :keymap ,keymap))))
        (setq-local dired-tags-edit-widget widget)
        (widget-insert "\n")
        (widget-create 'push-button :notify #'dired-tags-edit-save "Save and Quit (C-c C-c)")
        (widget-insert " ")
        (widget-create 'push-button :notify #'dired-tags-edit-quit "Quit (C-c C-k)")
        (use-local-map (dired-tags-edit--make-map widget-keymap)))
      (widget-setup)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(define-prefix-command 'dired-tags-prefix-map)

(easy-mmode-define-keymap
 '(("\C-a" . dired-tags-add)
   ("\C-r" . dired-tags-remove)
   ("\C-l" . dired-tags-list)
   ("\C-m" . dired-tags-mark-regexp)
   ("\C-e" . dired-tags-edit))
 nil
 'dired-tags-prefix-map)

(provide 'dired-tags)
;;; dired-tags.el ends here

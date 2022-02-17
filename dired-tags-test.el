;;; dired-tags-test.el --- Tests for dired-tags      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <val@nixos>
;; Keywords:

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

;; Tests for dired-tags

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'dired-tags)

(defmacro dired-tags-test-with-empty-temp-file (variable &rest body)
  "Wrap BODY in a let form with VARIABLE bind to a temporary file."
  `(let ((,variable (make-temp-file (expand-file-name "./test"))))
     (unwind-protect (progn ,@body)
       (delete-file ,variable t))))

(defmacro dired-tags-test-with-temp-file (variable &rest body)
  "Wrap BODY in a let form with VARIABLE bind to a temporary file."
  `(dired-tags-test-with-empty-temp-file ,variable
     (xattr-set ,variable (concat "user." dired-tags-xattr-namespace)
                (dired-tags--encode '("tag1" "tag2" "tag 3")))
     ,@body))

(defun dired-tags-test-equal (tags1 tags2)
  "Check whether TAGS1 and TAGS2 tag-lists are equal."
  (not (cl-set-exclusive-or tags1 tags2 :test #'string=)))

(ert-deftest dired-tags-test-equal ()
  (should (dired-tags-test-equal '("tag1" "tag2") '("tag2" "tag1")))
  (should-not (dired-tags-test-equal '("tag1" "tag2" "tag3") '("tag2" "tag1")))
  (should (dired-tags-test-equal '("tag 1" "tag 2" "tag3") '("tag3" "tag 2" "tag 1")))
  (should-not (dired-tags-test-equal '("tag 1" "tag 2" "tag3") '("tag3" "tag 2" "tag 1" "tag4"))))

(ert-deftest dired-tags-test-decode-encode ()
  (cl-flet ((noop (tags) (dired-tags--decode (dired-tags--encode tags))))
    (dolist (tags '(("tag1" "tag2" "tag3")
                    ("tag 1" "tag 2" "tag 3")
                    ("tag,1" "tag,2" "tag,3" ")(^(*&*&Y(*&)))")
                    ()))
      (should (dired-tags-test-equal (noop tags) tags)))))

(ert-deftest dired-tags-test-temp-file ()
  "Test creation of temparary random file."
  (let ((f nil))
    (dired-tags-test-with-temp-file file
      (should (file-exists-p file))
      (setq f file))
    (should-not (file-exists-p f))))

(ert-deftest dired-tags-test-name ()
  (should (string= (dired-tags--name) "user.emacs.dired.tags"))
  (let ((dired-tags-xattr-namespace "tags"))
    (should (string= (dired-tags--name) "user.tags"))))

(ert-deftest dired-tags-test-list ()
  (dired-tags-test-with-temp-file file
    (should (dired-tags-test-equal
             (dired-tags--list file)
             '("tag1" "tag2" "tag 3"))))
  (dired-tags-test-with-empty-temp-file file
    (should-not (dired-tags--list file))))

(ert-deftest dired-tags-test-save ()
  (dired-tags-test-with-empty-temp-file file
    (should (dired-tags-test-equal (dired-tags--save '("tag1" "tag2" "tag 3") file)
                                   '("tag1" "tag2" "tag 3")))
    (should (dired-tags-test-equal (dired-tags--list file)
                                   '("tag1" "tag2" "tag 3"))))
  (dired-tags-test-with-temp-file file
    (should (dired-tags-test-equal (dired-tags--save '("tag4" "tag5") file)
                                   '("tag4" "tag5")))
    (should (dired-tags-test-equal (dired-tags--list file)
                                   '("tag4" "tag5")))))

(ert-deftest dired-tags-test-add ()
  (dired-tags-test-with-temp-file file
    (should (dired-tags-test-equal
             (dired-tags--add "tag4" file)
             '("tag1" "tag2" "tag 3" "tag4")))
    (should (dired-tags-test-equal
             (dired-tags--list file)
             '("tag1" "tag2" "tag 3" "tag4")))
    (should (dired-tags-test-equal
             (dired-tags--add "tag 5" file)
             '("tag1" "tag2" "tag 3" "tag4" "tag 5")))
    (should (dired-tags-test-equal
             (dired-tags--list file)
             '("tag1" "tag2" "tag 3" "tag4" "tag 5"))))
  (dired-tags-test-with-empty-temp-file file
    (should (dired-tags-test-equal (dired-tags--add "tag4" file) '("tag4")))
    (should (dired-tags-test-equal (dired-tags--list file) '("tag4")))
    (should (dired-tags-test-equal (dired-tags--add "tag 5" file) '("tag4" "tag 5")))
    (should (dired-tags-test-equal (dired-tags--list file) '("tag4" "tag 5")))))

(ert-deftest dired-tags-test-remove ()
  (dired-tags-test-with-temp-file file
    (should (dired-tags-test-equal (dired-tags--remove "tag4" file) '("tag1" "tag2" "tag 3")))
    (should (dired-tags-test-equal (dired-tags--list file) '("tag1" "tag2" "tag 3")))
    (should (dired-tags-test-equal (dired-tags--remove "tag1" file) '("tag2" "tag 3")))
    (should (dired-tags-test-equal (dired-tags--list file) '("tag2" "tag 3"))))
  (dired-tags-test-with-empty-temp-file file
    (should (dired-tags-test-equal (dired-tags--remove "tag4" file) '()))
    (should (dired-tags-test-equal (dired-tags--list file) '()))))

(provide 'dired-tags-test)
;;; dired-tags-test.el ends here

;; Local Variables:
;; eval: (dolist (sym '(dired-tags-test-with-temp-file dired-tags-test-with-empty-temp-file)) (put sym 'lisp-indent-function 1))
;; End:

;;; relax.el --- For browsing and interacting with CouchDB

;; Copyright (C) 2009 Phil Hagelberg
;;
;; Author: Phil Hagelberg
;; URL: http://github.com/technomancy/relax.el
;; Version: 0.1
;; Keywords: database http
;; Created: 2009-05-11
;; Package-Requires: ((json "1.2"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Interact with CouchDB databases from within Emacs, with ease!

;; Needs the json.el package, which comes with Emacs 23, but is also
;; available from ELPA or from http://edward.oconnor.cx/elisp/json.el

;; javascript.el is also required. Get it from
;; http://www.brgeight.se/downloads/emacs/javascript.el and replace
;; (provide 'javascript-mode) with (provide 'javascript)

;; Right now it just does listing, reading, and updating of documents.

;;; TODO:

;; All kinds of things:
;; * attachment handling
;; * pagination
;; * hide _rev and _id fields?
;; * error handling

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'thingatpt)
(require 'url)
(require 'json)
(require 'javascript)
(require 'mm-util) ;; for replace-regexp-in-string

(defvar relax-host "127.0.0.1")
(defvar relax-port 5984)
(defvar relax-db-path "")

;;; Utilities

(defun relax-url (&optional id)
  (replace-regexp-in-string "\\([^:]\\)//*" "\\1/"
                            (format "http://%s:%s/%s/%s"
                                    relax-host (number-to-string relax-port)
                                    relax-db-path (or id ""))))

(defun relax-trim-headers ()
  (goto-char (point-min))
  (search-forward "\n\n")
  (delete-region (point-min) (point)))

(defun relax-json-encode (obj)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-encode obj)))

(defun relax-json-decode (str)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-read-from-string str)))

(defun relax-load-json-buffer (json-buffer)
  (with-current-buffer json-buffer
    (relax-json-decode
     (buffer-substring (point-min) (point-max)))))

(defun relax-kill-http-buffer ()
  (kill-buffer http-buffer))

(defun relax-kill-document (doc rev &optional callback)
  (let ((url-request-method "DELETE")
        (url (concat (relax-url doc) "?rev=" rev)))
    (url-retrieve url (or callback 'message))))

;;; DB-level

(defvar relax-mode-hook nil)

(defvar relax-mode-map (let ((map (make-sparse-keymap)))
                         (define-key map (kbd "RET") 'relax-doc)
                         (define-key map (kbd "C-o") 'relax-new-doc)
                         (define-key map (kbd "g") 'relax-update-db)

                         (define-key map (kbd "SPC") 'scroll-down)
                         (define-key map (kbd "<backspace>") 'scroll-up)
                         (define-key map "q" 'quit-window)
                         ;; TODO:
                         (define-key map (kbd "C-k") 'relax-kill-doc)
                         (define-key map "[" 'relax-prev-page)
                         (define-key map "]" 'relax-next-page)
                         (define-key map (kbd "C-M-k") 'relax-kill-db)
                         map))

(defun relax (db-url)
  "Connect to the CouchDB database at db-url."
  (interactive (list (read-from-minibuffer "CouchDB URL: " (relax-url))))
  (let ((url (url-generic-parse-url db-url)))
    (setq relax-host (url-host url)
          relax-port (url-port url)
          relax-db-path (url-filename url)))

  ;; TODO: update existing DB buffer if present
  (url-retrieve (relax-url "_all_docs") 'relax-mode (list db-url)))

(defun relax-mode (status database-url)
  "Major mode for interacting with CouchDB databases."
  (let ((json-buffer (current-buffer)))
    (relax-trim-headers)
    (switch-to-buffer (concat "*relax " database-url "*"))
    (buffer-disable-undo)
    (kill-all-local-variables)

    (set (make-local-variable 'http-buffer) json-buffer)
    (set (make-local-variable 'kill-buffer-hook) '(relax-kill-http-buffer))
    (set (make-local-variable 'db-url) database-url)
    (set (make-local-variable 'doc-list)
         (relax-load-json-buffer json-buffer)))

  (use-local-map relax-mode-map)
  (setq mode-name "relax")
  (setq major-mode 'relax-mode)

  (insert "== " db-url "\n")
  (insert (format "Total: %s, offset %s\n\n"
                  (getf doc-list :total_rows)
                  (getf doc-list :offset)))
  (relax-insert-doc-list (getf doc-list :rows))
  (setq buffer-read-only t)

  (run-hooks 'relax-mode-hook))

(defun relax-insert-doc-list (docs)
  (dolist (doc docs)
    (insert (format "  [%s @rev %s]\n" (getf doc :id) (getf (getf doc :value) :rev)))))

(defun relax-new-doc (choose-id)
  (interactive "P")
  (let ((url-request-method (if choose-id "PUT" "POST"))
        (url-request-data "{}")
        (id (if choose-id (read-from-minibuffer "Document ID: "))))
    (url-retrieve (relax-url id) 'relax-visit-new-doc)))

(defun relax-visit-new-doc (status)
  (goto-char (point-min))
  (search-forward "Location: ")
  (let ((doc-url (buffer-substring (point) (progn (end-of-line) (point)))))
    (url-retrieve doc-url 'relax-doc-load (list doc-url))))

(defun relax-update-db ()
  (interactive)
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))
  (url-retrieve (relax-url "_all_docs") 'relax-mode (list db-url)))

(defun relax-db-kill-doc ()
  (interactive)
  (let ((url-request-method "DELETE")
        (url (concat doc-url "?rev=" (getf doc :_rev))))
    (url-retrieve url 'message)))

;;; Document-level

(defvar relax-doc-mode-hook nil)

(defvar relax-doc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-s") 'relax-submit)
    (define-key map (kbd "C-c C-u") 'relax-update-doc)
    (define-key map (kbd "C-c C-k") 'relax-kill-doc)
    map))

(defun relax-doc-load (status document-url)
  (let ((json-buffer (current-buffer)))
    (relax-trim-headers)
    (let ((doc-string (buffer-substring-no-properties (point-min) (point-max))))
      (switch-to-buffer (concat "*relax " document-url "*"))

      (javascript-mode)
      (relax-doc-mode t)
      (set (make-local-variable 'http-buffer) json-buffer)
      (set (make-local-variable 'kill-buffer-hook) '(relax-kill-http-buffer))
      (set (make-local-variable 'doc-url) document-url)
      (set (make-local-variable 'doc)
           (relax-load-json-buffer json-buffer))
      (insert doc-string)))

  (save-excursion ;; prettify
    (goto-char (point-min))
    (replace-string "\",\"" "\",\n\"")
    (indent-region (point-min) (point-max))
    (font-lock-fontify-buffer))
  (message "Loaded %s" doc-url))

(define-minor-mode relax-doc-mode
  "Minor mode for interacting with CouchDB documents."
  nil
  "relax doc")

(defun relax-doc ()
  "Open a buffer viewing the document at point."
  (interactive)

  ;; TODO: make sure point is over DB ID.
  (let ((doc-url (relax-url (word-at-point))))
    (url-retrieve doc-url 'relax-doc-load (list doc-url))))

(defun relax-submit ()
  (interactive)
  (let ((url-request-method "PUT")
        (url-request-data (buffer-substring (point-max) (point-min))))
    (lexical-let ((doc-buffer (current-buffer)))
      (url-retrieve doc-url (lambda (status)
                              (switch-to-buffer doc-buffer)
                              (relax-update-doc))))))

(defun relax-update-doc ()
  (interactive)
  (delete-region (point-min) (point-max))
  (url-retrieve doc-url 'relax-doc-load (list doc-url)))

(defun relax-kill-doc ()
  "Delete this revision of the current document from the database."
  (interactive)
  (lexical-let ((target-buffer (current-buffer)))
    (relax-kill-document (getf doc :_id) (getf doc :_rev)
                         (lambda (status)
                           (kill-buffer target-buffer)
                           (relax-update-db)))))

(provide 'relax) ;;; relax.el ends here
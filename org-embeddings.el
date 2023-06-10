;;; org-embeddings.el --- Get embeddings of org subtrees -*- lexical-binding: t; -*-
;;
;; Filename: org-embeddings.el
;; Description:
;; Author: Renat Galimov
;; Maintainer:
;; Created: Wed May 24 07:23:28 2023 (+0300)
;; Version:
;; Package-Requires: ((openai) (org))
;; Last-Updated: Sat Jun 10 07:45:53 2023 (+0300)
;;           By: Renat Galimov
;;     Update #: 624
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(require 'openai)
(require 'openai-embedding)
(require 'org-element)

(defgroup org-embeddings nil
  "Get embeddings of org subtrees."
  :group 'org)

;; Get an embedding of a current org subtree

(defvar org-embeddings-json-file
  (expand-file-name "org-embeddings.json" user-emacs-directory)
  "File to store embeddings in.")

;; ===== ELEMENT =====

(defun org-embeddings-embeddable-p (element)
  "Check if an ELEMENT is embeddable."
  (or (and (eq (org-element-type element) 'headline)
           (org-element-property :ID element))
      (org-element-property :attr_embeddings element)))

(defun org-embeddings-element-title (element)
  "Get title of an ELEMENT."
  (unless element
    (error "No element given"))
  (cond ((eq (org-element-type element) 'headline)
         (org-element-property :raw-value element))))


(defun org-embeddings-element-at-point ()
  "Get an element we can calculate embeddings for.

Search for an embeddable element going up the file."

  ;; TODO: I want to go up the tree, not the file.
  ;; This should stop if the current element is a headline, which doesn't have an ID.

  (interactive)
  (save-window-excursion
    (save-excursion
      (cl-loop with element = (org-element-at-point)
               while element
               until (org-embeddings-embeddable-p element)
               do (setq element
                        ;; Don't go up if the current element is a headline
                        (if (eq (org-element-type element) 'headline)
                            nil
                          (org-embeddings-element-previous element)))
               finally return element))))

(defun org-embeddings-source-from-element (element)
    "Create a source from an ELEMENT."
    (unless element
        (error "No element given"))
    (let ((id (org-element-property :ID element))
          (text (org-embeddings-element-text element))
          (metadata `(:file ,(buffer-file-name) :title ,(org-embeddings-element-title element))))
      (make-org-embeddings-source :id id :text text :metadata metadata)))

(defun org-embeddings-element-text (element)
  "Get text of an ELEMENT for API."
  (unless element
    (error "No element given"))
  (let ((begin (org-element-property :contents-begin element))
        (end (org-element-property :contents-end element)))
    (buffer-substring-no-properties begin end)))

(defun org-embeddings-element-previous (element)
  "Move point to the previous ELEMENT end."
  (unless element
    (error "No element given"))

  (let ((pos (org-element-property :begin element)))
    (if (or (not pos) (= pos 1))
        nil
      (goto-char (1- pos))
      (org-element-at-point))))


;; ===== STORE =====

(cl-defstruct org-embeddings-source
  "Request to store an embedding."
  (id nil :type string :documentation "ID of the object to store")
  (text nil :type string :documentation "Text to get embeddings for")
  (metadata nil :type plist :documentation "Metadata to store with the resulting embedding"))

(defun org-embeddings-store-get (model id)
  "Get an embedding for MODEL and ID from the store."
  ;; TODO: Temporarily I use hardcoded JSON
  (org-embeddings-json-get model id))

;; ===== JSON =====

(defcustom org-embeddings-json-directory (expand-file-name "org-embeddings/json" user-emacs-directory)
  "Directory to store JSON files in."
  :type 'directory
  :group 'org-embeddings)

(defcustom org-embeddings-json-file-prefix "org-embeddings-"
  "Prefix for JSON files."
  :type 'string
  :group 'org-embeddings)

(defun org-embeddings-plist-to-hash-table (input)
    "Convert INPUT plist to a hash table."
    (cl-loop for (key value) on input by #'cddr
             with result = (make-hash-table :test 'equal)
             do (puthash (substring (symbol-name key) 1) value result)
             finally return result))


(defun org-embeddings-alist-to-hash-table (input)
  "Convert INPUT alist to a hash table."
  (let ((result (make-hash-table :test 'equal)))
    (cl-loop for (key . value) in input
             do (puthash (symbol-name key) value result)
             finally return result)))


(cl-defun org-embeddings-json-store (model id vector &optional metadata)
  "Add VECTOR with ID to a JSON file for MODEL.

Use it for testing, primarily, as it doesn't work well with
larger databases."
  (let ((current-embeddings (org-embeddings-json-load model))
        (file (org-embeddings-json-file-name model))
        (new-object (make-hash-table :test 'equal)))
    (puthash "vector" vector new-object)
    (cond ((hash-table-p metadata)
           (puthash "metadata" metadata new-object))
          ((plistp metadata)
           (puthash "metadata" (org-embeddings-plist-to-hash-table metadata) new-object))
          ((listp metadata)
           (puthash "metadata" (org-embeddings-alist-to-hash-table metadata) new-object)))
    (puthash id new-object current-embeddings)

    (with-temp-file file
      (insert (json-serialize current-embeddings :null-object nil)))
    (org-embeddings-log "info" "Embedding `%s' JSON saved to `%s'" id file)))

(defun org-embeddings-json-load (model)
  "Load embeddings for MODEL from JSON file.

Returns an empty hash table if the file doesn't exist."
  (let ((file (org-embeddings-json-file-name model)))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (json-parse-buffer :null-object nil :object-type 'hash-table))
      (make-hash-table))))

(defun org-embeddings-json-get (model id)
    "Get an embedding for MODEL and ID from a JSON file."
    (gethash id (org-embeddings-json-load model)))

(defun org-embeddings-json-file-name (model)
  "Get a file name for storing embeddings for PATH and MODEL."
  (unless (file-exists-p org-embeddings-json-directory)
    (make-directory org-embeddings-json-directory t))
  (expand-file-name
   (concat org-embeddings-json-file-prefix model ".json")
   org-embeddings-json-directory))

;; ===== OPENAI =====

(defconst org-embeddings-openai-default-token-limit 2046
  "Default token limit for OpenAI models.

This is set as a safe default, as the limit for the smallest model.")

(defconst org-embeddings-openai-model-tokens
  '((text-embedding-ada-002 . 8191)
    (t . 2046))
  "Alist of OpenAI models and their token limits.")

(defcustom org-embeddings-openai-default-model 'text-embedding-ada-002
  "Default OpenAI model to use."
  :type 'symbol
  :group 'org-embeddings)

(defun org-embeddings-openai-trim (model text)
  "Trim TEXT to fit into MODEL token limit."
  (unless model
    (error "No model given"))
  (unless text
    (error "No text given"))
  (if (<= (length text) org-embeddings-openai-default-token-limit)
      text
    (substring text 0 (or
                       (alist-get model org-embeddings-openai-model-tokens)
                       org-embeddings-openai-default-token-limit))))

(defun org-embeddings-openai-create (source)
  "Create an embedding for SOURCE."
  (unless source
    (error "No source given"))

  (let ((text (org-embeddings-source-text source))
        (id (org-embeddings-source-id source))
        (metadata (org-embeddings-source-metadata source)))
    (openai-embedding-create
     (org-embeddings-openai-trim org-embeddings-openai-default-model text)
     (lambda (data)
       (let ((vector (alist-get 'embedding (seq-elt (alist-get 'data data) 0)))
             (model (alist-get 'model data)))
         (org-embeddings-json-store model id vector metadata)))
     :model (or org-embeddings-openai-default-model (error "No default OpenAI model set")))))

;; ===== CREATE =====

(defun org-embeddings-create (&optional element)
  "Get an embedding of an ELEMENT or a current org subtree."
  (interactive)
  (org-embeddings-openai-create
   (if current-prefix-arg
       (org-embeddings-file-get)
     (let ((element (or element (org-embeddings-element-at-point))))
       (unless element
         (error "No element at point that we can get an embedding of"))
       (org-embeddings-source-from-element element)))))


;; ===== LOG =====

(defun org-embeddings-log-buffer ()
  "Get a buffer for logging."
  (get-buffer-create "*org-embeddings*"))

(defun org-embeddings-log (level message &rest args)
  "Log a MESSAGE with LEVEL and ARGS."
    (with-current-buffer (org-embeddings-log-buffer)
        (goto-char (point-max))
        (insert (format-time-string "%Y-%m-%d %H:%M:%S"))
        (insert " ")
        (insert (format "%s " level))
        (insert (apply 'format message args))
        (insert "\n")))


;; ===== FILE =====

(defun org-embeddings-file-get ()
  "Get a text and ID of current file to process.

Returns `org-embeddings-source' object."
  (save-window-excursion
    (save-excursion
      (goto-char (point-min))
      (let ((id (org-id-get)))
        (unless id
          (error "No ID found"))
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (make-org-embeddings-source :id id :text text :metadata `(:file ,(buffer-file-name))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-embeddings.el ends here

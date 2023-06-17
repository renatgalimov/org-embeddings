;;; org-embeddings.el --- Get embeddings of org subtrees -*- lexical-binding: t; -*-
;;
;; Filename: org-embeddings.el
;; Description:
;; Author: Renat Galimov
;; Maintainer:
;; Created: Wed May 24 07:23:28 2023 (+0300)
;; Version:
;; Package-Requires: ((openai) (org))
;; Last-Updated: Sat Jun 17 07:24:58 2023 (+0300)
;;           By: Renat Galimov
;;     Update #: 1112
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
(require 'openai-completion)
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
  ;; TODO: This function is useless. You need a way to export the element.
  ;; Instead of this make a function that makes the source from a region.

  "Create a source from an ELEMENT."
  (unless element
    (error "No element given"))
  (cl-letf ((org-export-use-babel nil))
    (let ((id (org-element-property :ID element))
          (text (org-export-as 'ascii nil nil t org-embeddings-export-plist))
          (metadata `(:file ,(buffer-file-name) :title ,(org-embeddings-element-title element))))
      (make-org-embeddings-source :id id :text text :metadata metadata))))

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

(defconst org-embeddings-json-cache '()
  "Cache of JSON files.")


(defun -dbg (v)
  (message "dbg - %s" v)
  v)
(defun org-embeddings-plist-to-hash-table (input)
  "Convert INPUT plist to a hash table."
  (org-embeddings-log "debug" "Converting plist to hash table")
  (cl-loop for (key value) on input by #'cddr
           with result = (make-hash-table :test 'equal)
           do (puthash (substring (symbol-name key) 1) value result)
           finally return result))

;;(gethash "metadata" (gethash "45970681-A829-4DB4-A304-81A0C409707A" (plist-get org-embeddings-json-cache 'text-embedding-ada-002-v2)))
(defun org-embeddings-alist-to-hash-table (input)
  "Convert INPUT alist to a hash table."
  (let ((result (make-hash-table :test 'equal)))
    (cl-loop for (key . value) in input
             do (puthash (symbol-name key) value result)
             finally return result)))

;; So you just update the global JSON file and then flush it into a file.
(cl-defun org-embeddings-json-store (model id hash vector &optional metadata)
  "Add VECTOR with ID to a JSON file for MODEL.

If FLUSH is given, automatically save the cache to a file.

Use it for testing, primarily, as it doesn't work well with
larger databases."
  (unless (stringp id)
    (error "ID should be a string"))
  (unless (stringp hash)
    (error "Hash should be a string"))
  (unless (vectorp vector)
    (error "Vector should be a vector"))

  (unless (plist-member org-embeddings-json-cache model)
    (org-embeddings-json-load model))

  (let ((current-embeddings (plist-get org-embeddings-json-cache model))
        (new-object (make-hash-table :test 'equal)))
    (org-embeddings-log "debug" "Current embeddings: %s" current-embeddings)
    (puthash "vector" vector new-object)
    (puthash "hash" hash new-object)
    (cond ((and metadata (hash-table-p metadata))
           (puthash "metadata" metadata new-object))
          ((plistp metadata)
           (puthash "metadata" (org-embeddings-plist-to-hash-table metadata) new-object))
          ((listp metadata)
           (puthash "metadata" (org-embeddings-alist-to-hash-table metadata) new-object))
          (metadata (error "Metadata should be a plist, alist or a hash table. Got `%s' of type `%s'" metadata (type-of metadata))))
    (puthash id new-object current-embeddings)
    (org-embeddings-log "info" "Embedding `%s' JSON saved" id)))

(defun org-embeddings-json-load (model)
  "Load embeddings for MODEL from JSON file to a cache.

If value for a key is already in the cache, it won't be updated.
Returns an empty hash table if the file doesn't exist."
  (unless model
    (error "No model given"))

  (let ((file (org-embeddings-json-file-name model))
        (cache (plist-get org-embeddings-json-cache model))
        (loaded-cache))

    (unless cache
      (setq cache (make-hash-table :test 'equal))
      (org-embeddings-log "debug" "Fresh cache for model `%s' created" model)
      (setq org-embeddings-json-cache (plist-put org-embeddings-json-cache model cache)))

    (if (not (file-exists-p file))
        (org-embeddings-log "info" "JSON file for model `%s' doesn't exist" model)
      (setq loaded-cache (with-temp-buffer
                           (insert-file-contents file)
                           (goto-char (point-min))
                           (json-parse-buffer :null-object nil :object-type 'hash-table)))

      (maphash (lambda (key value)
                 (when (not (gethash key cache))
                   (puthash key value cache)))
               loaded-cache)
      (org-embeddings-log "info" "JSON file for model `%s' loaded" model))
    cache))

(defun org-embeddings-json-flush (&optional model)
  "Flush the cache to JSON files.

If MODEL is given - save only for that model."
  (cl-loop for (model current-embeddings) on org-embeddings-json-cache by #'cddr
           if (or (not model) (equal model model))
           do (with-temp-file (org-embeddings-json-file-name model)
                (org-embeddings-log "info" "Flushing JSON for model `%s' to `%s'" model (org-embeddings-json-file-name model))
                (insert (json-serialize current-embeddings :null-object nil)))))

(defun org-embeddings-json-get (model id)
  "Get an embedding for MODEL and ID from a JSON file."
  (gethash id (org-embeddings-json-load model)))

(defun org-embeddings-json-file-name (model)
  "Get a file name for storing embeddings for PATH and MODEL."
  (unless (symbolp model)
    (error "Model should be a symbol"))
  (unless (file-exists-p org-embeddings-json-directory)
    (make-directory org-embeddings-json-directory t))
  (expand-file-name
   (concat org-embeddings-json-file-prefix (symbol-name model) ".json")
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
    (string-limit text (or
                        (alist-get model org-embeddings-openai-model-tokens)
                        org-embeddings-openai-default-token-limit))))


(defun org-embeddings-openai-create-req (text id metadata)
  "Create a request for OpenAI API for TEXT with ID and METADATA.

TEXT: string
ID: string
METADATA: plist"

  (openai-embedding-create
   (org-embeddings-openai-trim org-embeddings-openai-default-model text)
   (lambda (data)
     (let ((vector (alist-get 'embedding (seq-elt (alist-get 'data data) 0)))
           (model (intern (alist-get 'model data))))
       (org-embeddings-log "debug" "Saving emedding `%s'" id)
       (org-embeddings-log "debug" "%s" text)
       (org-embeddings-json-store model id (secure-hash 'sha1 text) vector metadata)))
   :model (or org-embeddings-openai-default-model (error "No default OpenAI model set"))))

(defun org-embeddings-openai-create (source)
  "Create an embedding for SOURCE."
  (unless source
    (error "No source given"))

  (let ((text (org-embeddings-source-text source))
        (id (org-embeddings-source-id source))
        (metadata (org-embeddings-source-metadata source)))
    (unless id
      (error "No ID given"))
    (unless text
      (error "No text given"))
    (org-embeddings-log "debug" "Creating OpenAI embedding for `%s'" text)
    ;; No title
    (if (not (plist-member metadata :title))
        (org-embeddings-openai-tldr text (lambda (choice)
                                           (org-embeddings-openai-create-req text id (plist-put metadata :title choice))))
      (org-embeddings-openai-create-req text id metadata))))

(defun org-embeddings-openai-tldr (text callback)
  "Create a TLDR for SOURCE and call CALLBACK with it."
  (unless text
    (error "No source given"))

  (org-embeddings-log "debug" "Creating TLDR for `%s'" text)
  (openai-completion (format "%s\nTLDR, single line, 16 words at most:" text)
                     (lambda (data)
                       (let ((choice (string-trim (alist-get 'text (seq-elt (alist-get 'choices data) 0)))))
                         (org-embeddings-log "debug" "TLDR: %s" choice)
                         (funcall callback choice)))
                     :max-tokens 32
                     :temperature 1.0
                     :frequency-penalty 0.0
                     :presence-penalty 1.0
                     :n 1))

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
       (if (bufferp element)
           (with-current-buffer element
             (org-embeddings-file-get))
         (org-embeddings-source-from-element element))))))


;; TODO:: This is a demo-only function. Perhaps, when I have time - I need to write a helm source.
(defun org-embeddings-search (&optional query)
  "Search for an embedding similar to QUERY."
  (interactive "sSearch: ")

  (with-current-buffer (get-buffer-create "*org-embeddings-search*")
    (erase-buffer)
    (insert "Searching for `" query "'\n\n")
    (display-buffer (current-buffer)))

  (openai-embedding-create
   query
   (lambda (data)
     (let ((vector (alist-get 'embedding (seq-elt (alist-get 'data data) 0)))
           (model (intern (alist-get 'model data))))
       (let ((vectors-by-id (org-embeddings-json-load model))
             (locations))
         (maphash (lambda (_ object)
                    (let ((similarity (org-embeddings-vector-cosine-similarity vector (gethash "vector" object))))
                      (when (> similarity 0.75)
                        (push (cons similarity object) locations))))
                  vectors-by-id)
         (with-current-buffer (get-buffer-create "*org-embeddings-search*")
           (erase-buffer)
           (org-mode)
           (insert (format "* Search results for `%s'\n\n" query))
           (insert (format " - model: %s\n" model))
           (insert (format "%s\n\n" (cl-loop for location in (seq-sort (lambda (a b) (> (car a) (car b))) locations)
                                             for similarity = (car location)
                                             for file = (gethash "file" (gethash "metadata" (cdr location)))
                                             for title = (gethash "title" (gethash "metadata" (cdr location)))
                                             concat (format "- %.2f [[%s][%s]]\n" similarity file (or title file)))))
           (display-buffer (current-buffer))))))
   :model (or org-embeddings-openai-default-model (error "No default OpenAI model set"))))
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
    (insert (format "%s " (upcase level)))
    (insert (apply 'format message args))
    (insert "\n")))


;; ===== FILE =====

(defconst org-embeddings-export-plist
  '(:with-author nil :with-email nil :with-stat nil :with-priority nil :with-toc nil :with-properties nil :with-planning nil)
  "Plist of options for `org-export-as'.")

(defun org-embeddings-file-get ()
  "Get a text and ID of current file to process.

Returns `org-embeddings-source' object."
  (save-window-excursion
    (save-mark-and-excursion
      (goto-char (point-min))
      (let ((id (org-id-get)))
        (unless id
          (error "No ID found"))
        (cl-letf ((org-export-use-babel nil))
          (let ((text (org-export-as 'ascii nil nil t org-embeddings-export-plist)))
            (make-org-embeddings-source :id id :text text :metadata `(:file ,(buffer-file-name)))))))))

;; ===== VECTOR ====

(defun org-embeddings-vector-distance (left right)
  "Calculate a distance between LEFT and RIGHT vectors."
  (unless (and (vectorp left) (vectorp right))
    (error "Both LEFT and RIGHT must be vectors"))

  (unless (= (length left) (length right))
    (error "Both LEFT and RIGHT must be of the same length"))

  (cl-loop
   with sum = 0
   for element-left across left
   for element-right across right
   do (setq sum (+ sum (expt (- element-left element-right) 2)))
   finally return (sqrt sum)))


(defun org-embeddings-vector-cosine-similarity (left right)
  "Calculate a cosine similarity between LEFT and RIGHT vectors."
  (unless (and (vectorp left) (vectorp right))
    (error "Both LEFT and RIGHT must be vectors"))

  (unless (= (length left) (length right))
    (error "Both LEFT and RIGHT must be of the same length"))

  (let ((dot-product (cl-loop
                      with sum = 0
                      for element-left across left
                      for element-right across right
                      do (setq sum (+ sum (* element-left element-right)))
                      finally return sum))
        (left-length (sqrt (cl-loop
                            with sum = 0
                            for element-left across left
                            do (setq sum (+ sum (expt element-left 2)))
                            finally return sum)))
        (right-length (sqrt (cl-loop
                             with sum = 0
                             for element-right across right
                             do (setq sum (+ sum (expt element-right 2)))
                             finally return sum))))
    (/ dot-product (* left-length right-length))))


(when (featurep 'org-roam)
  (require 'org-roam)
  (defun org-embeddings-index-daily-files ()
    "Index all daily files."
    (org-embeddings-json-load org-embeddings-openai-default-model)
    (cl-loop for file in (directory-files (expand-file-name org-roam-dailies-directory org-roam-directory) t "\\.org$")
             for content-length = (nth 7 (file-attributes file))
             when (> content-length 168)
             do (progn
                  (org-embeddings-create (find-file-noselect file))
                  (org-embeddings-log "debug" "Indexing %s" file)
                  (sleep-for 5)))
    (org-embeddings-json-flush org-embeddings-openai-default-model)))


(provide 'org-embeddings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-embeddings.el ends here

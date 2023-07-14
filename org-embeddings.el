;;; org-embeddings.el --- Get embeddings of org subtrees -*- lexical-binding: t; -*-
;;
;; Filename: org-embeddings.el
;; Description:
;; Author: Renat Galimov
;; Maintainer:
;; Created: Wed May 24 07:23:28 2023 (+0300)
;; Version:
;; Package-Requires: ((openai) (org) (deferred))
;; Last-Updated: Fri Jul 14 11:11:23 2023 (+0300)
;;           By: Renat Galimov
;;     Update #: 2064
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
(require 'deferred)

(defgroup org-embeddings nil
  "Get embeddings of org subtrees."
  :group 'org)

;; Get an embedding of a current org subtree

(defvar org-embeddings-json-file
  (expand-file-name "org-embeddings.json" user-emacs-directory)
  "File to store embeddings in.")

;; ===== UTILS =====

(defun org-embeddings-metadata (&rest args)
  "Create a metadata hash table from ARGS."
  (let ((result (make-hash-table :test 'equal)))
    (while args
      ;; Strip leading :
      (puthash (substring (symbol-name (pop args)) 1) (pop args) result))
    result))


;; ===== ERRORS =====

(define-error 'org-embeddings-error "Generic org-embeddings error")
(define-error 'org-embeddings-data-error "Cannot extract needed data from the object" 'org-embeddings-error)
(define-error 'org-embeddings-http-error "HTTP error" 'org-embeddings-error)

;; ===== FILE =====

(defconst org-embeddings-export-plist
  '(:with-author nil :with-email nil :with-stat nil :with-priority nil :with-toc nil :with-properties nil :with-planning nil)
  "Plist of options for `org-export-as'.")



(defun org-embeddings-file-get (path)
  "Get a text and ID of current file to process.

Returns `org-embeddings-source' object."
  (org-embeddings-log "debug" "Getting source for file %s" path)
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let ((id (org-id-get (point-min) nil)))
      (unless id
        (org-embeddings-log "debug" "No ID found in %s" path)
        (error "No ID found"))
      (cl-letf ((org-export-use-babel nil))
        (org-embeddings-log "debug" "Exporting embeddings source with id %s" id)
        (let ((text (org-export-as 'ascii nil nil t org-embeddings-export-plist)))
          (make-org-embeddings-source :id id :text text :metadata (org-embeddings-metadata :file path)))))))

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
          (metadata (org-embeddings-metadata :file (buffer-file-name) :title (org-embeddings-element-title element))))
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


;; ===== SOURCE =====
(cl-defstruct
    (org-embeddings-source
     (:constructor make-org-embeddings-source (&key id text metadata &aux (hash (secure-hash 'sha256 text)))))
  "We send this to the API to get embeddings."
  (id nil :type string :documentation "ID of the object to store" :read-only t)
  (text "" :type string :documentation "Text to get embeddings for" :read-only t)
  (metadata nil :type hash-table :documentation "Metadata for a source")
  (hash nil :type string :documentation "Hash of the source text" :read-only t))


;; ===== STORE =====

(cl-defstruct
    org-embeddings-record
  "Internal representation of the embedding for usage within the library."
  (id nil :type string :documentation "ID of the object to store" :read-only t)
  (hash nil :type string :documentation "Hash of the source text" :read-only t)
  (vector nil :type vector :documentation "Resulting embedding" :read-only t)
  (model nil :type string :documentation "Model to use for embeddings" :read-only t)
  (metadata nil :type hash-table :documentation "Metadata to store with the resulting embedding" :read-only t))


(defun org-embeddings-store (record &optional backend-args)
  "Store a RECORD in the store.

Pass BACKEND-ARGS to the backend."

  (unless (and record (org-embeddings-record-p record))
    (signal 'wrong-type-argument (list 'org-embeddings-record-p record)))

  ;; Currently - I hardcode JSON store
  ;; Later this would be configurable
  (apply #'org-embeddings-json-store record backend-args))

(defun org-embeddings-store-get (model id)
  "Get an embedding for MODEL and ID from the store.

Return an `org-embeddings-record' object."
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

(defconst org-embeddings-json-flush-timeout 10
  "Timeout to flush JSON cache.")

(defvar org-embeddings-json-flush-timer nil
  "Timer to flush JSON cache.")

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
(defun org-embeddings-json-store (record)
  "Store a RECORD in JSON format.

Use it for testing, primarily, as it doesn't work well with
larger databases."

  (unless (and record (org-embeddings-record-p record))
    (signal 'wrong-type-argument (list 'org-embeddings-record-p record)))

  (let ((model (org-embeddings-record-model record)))

    (unless (plist-member org-embeddings-json-cache model)
      (org-embeddings-json-load model))

    (let ((current-embeddings (plist-get org-embeddings-json-cache model))
          (id (org-embeddings-record-id record))
          (new-object (make-hash-table :test 'equal)))

      (puthash id record current-embeddings)
      (org-embeddings-log "info" "Embedding `%s' JSON saved" id))
    (if org-embeddings-json-flush-timer
        (progn (org-embeddings-log "debug" "Canceling JSON flush timer: %s" org-embeddings-json-flush-timer)
               (cancel-timer org-embeddings-json-flush-timer))
      (org-embeddings-log "debug" "No JSON flush timer to cancel"))
    (org-embeddings-log "debug" "Setting JSON flush timer")
    (setq org-embeddings-json-flush-timer
          (if (> (time-convert (or (current-idle-time) '(0 0 0 0)) 'integer) org-embeddings-json-flush-timeout)
              (run-with-timer org-embeddings-json-flush-timeout nil #'org-embeddings-json-flush model)
            (run-with-idle-timer org-embeddings-json-flush-timeout nil #'org-embeddings-json-flush model)))))


(defun org-embeddings-json-hashtable-to-record (model hashtable)
  "Convert a HASHTABLE to an `org-embeddings-record' object for a MODEL."

  (unless (and hashtable (hash-table-p hashtable))
    (signal 'wrong-type-argument (list 'hash-table-p hashtable)))

  (make-org-embeddings-record
   :id (gethash "id" hashtable)
   :hash (gethash "hash" hashtable)
   :vector (gethash "vector" hashtable)
   :model model
   :metadata (org-embeddings-alist-to-hash-table (gethash "metadata" hashtable))))

(defun org-embeddings-json-record-to-hashtable (record)
  "Convert a RECORD object to a hash table."
  (unless (and record (org-embeddings-record-p record))
    (signal 'wrong-type-argument (list 'org-embeddings-record-p record)))

  (let ((result (make-hash-table :test 'equal)))
    (puthash "id" (org-embeddings-record-id record) result)
    (puthash "hash" (org-embeddings-record-hash record) result)
    (puthash "vector" (org-embeddings-record-vector record) result)
    (puthash "metadata" (org-embeddings-record-metadata record) result)
    result))

(defun org-embeddings-json-load (model)
  "Load embeddings for MODEL from JSON file to a cache.

A cache is a hash-table mapping ID's to `org-embeddings-record' objects.

If value for a key is already in the cache, it won't be updated.
Returns an empty hash table if the file doesn't exist."
  (unless (and model (symbolp model))
    (signal 'wrong-type-argument (list 'symbolp model)))

  (let ((file (org-embeddings-json-file-name model))
        (current-cache (plist-get org-embeddings-json-cache model)))
    (unless current-cache
      (setq current-cache (make-hash-table :test 'equal)))

    (if (file-exists-p file)
        (let ((loaded-cache (with-temp-buffer
                              (insert-file-contents file)
                              (goto-char (point-min))
                              (json-parse-buffer :null-object nil :object-type 'hash-table))))
          (maphash (lambda (key value)
                     (when (not (gethash key current-cache))
                       (puthash key (org-embeddings-json-hashtable-to-record model value) current-cache)))
                   loaded-cache)
          (org-embeddings-log "info" "JSON file for model `%s' loaded from: %s" model file))
      (org-embeddings-log "info" "JSON file for model `%s' doesn't exist" model))
    (setq org-embeddings-json-cache (plist-put org-embeddings-json-cache model current-cache))
    current-cache))

(defun org-embeddings-json-flush (&optional model)
  "Flush the cache to JSON files.

If MODEL is given - save only for that model."
  (cl-loop for (current-model current-embeddings) on org-embeddings-json-cache by #'cddr
           if (or (not model) (equal model current-model))
           do (with-temp-file (org-embeddings-json-file-name model)
                (org-embeddings-log "info" "Flushing JSON for model `%s' to `%s'" model (org-embeddings-json-file-name model))
                (let ((hashtable (make-hash-table :test 'equal)))
                  (maphash (lambda (key value)
                             (puthash key (org-embeddings-json-record-to-hashtable value) hashtable))
                           current-embeddings)
                  (insert (json-serialize hashtable :null-object nil)))))
  (when org-embeddings-json-flush-timer
    (cancel-timer org-embeddings-json-flush-timer))
  (setq org-embeddings-json-flush-timer nil))

(defun org-embeddings-json-get (model id)
  "Get an embedding for MODEL and ID from a JSON file."
  (unless (plist-member org-embeddings-json-cache model)
    (org-embeddings-json-load model))

  (gethash id (plist-get org-embeddings-json-cache model)))

(defun org-embeddings-json-file-name (model)
  "Get a file name for storing embeddings for PATH and MODEL."
  (unless (symbolp model)
    (signal 'wrong-type-argument (list 'symbolp model)))
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

(cl-defun org-embeddings-openai-create (source)
  "Create an embedding for a SOURCE.

Returns a deferred object resolving to `org-embeddings-source' struct."

  (unless (and source (org-embeddings-source-p source))
    (signal 'wrong-type-argument (list 'org-embeddings-source-p source)))

  (let ((text (org-embeddings-openai-trim org-embeddings-openai-default-model (org-embeddings-source-text source)))
        (id (org-embeddings-source-id source))
        (metadata (org-embeddings-source-metadata source))
        (hash (org-embeddings-source-hash source))
        (result (deferred:new)))

    (unless id
      (signal 'org-embeddings-data-error '("ID")))
    (unless (and text (> (length text) 0))
      (signal 'org-embeddings-data-error '("TEXT")))

    (org-embeddings-log "debug" "Creating OpenAI embedding for `%s'" id)

    ;; Copy the value since we're going to pass it to the callback.
    (openai-embedding-create text
                             (lambda (data)
                               (if openai-error
                                   (deferred:errorback-post result (list openai-error data))

                                 (let ((vector (alist-get 'embedding (seq-elt (alist-get 'data data) 0)))
                                       (model (intern (alist-get 'model data))))
                                   (deferred:callback-post result
                                     (make-org-embeddings-record :id id :vector vector :model org-embeddings-openai-default-model :metadata metadata :hash hash)))))
                             :model (or org-embeddings-openai-default-model (error "No default OpenAI model set")))
    result))

(defun org-embeddings-openai-tldr (text)
  "Create a TLDR for TEXT and call AND-THEN with it.

Return a deferred object."

  (unless (and text (stringp text))
    (signal 'wrong-type-argument (list 'stringp text)))

  (let ((result (deferred:new)))
    (openai-completion (format "%s\nTLDR, single line, 16 words at most:" text)
                       (lambda (data)
                         (if openai-error
                             (deferred:errorback-post result (list openai-error data))
                           (let ((choice (string-trim (alist-get 'text (seq-elt (alist-get 'choices data) 0)))))
                             (org-embeddings-log "debug" "TLDR: %s" choice)
                             (deferred:callback-post result choice))))
                       :max-tokens 32
                       :temperature 1.0
                       :frequency-penalty 0.0
                       :presence-penalty 1.0
                       :n 1)
    result))


;; ===== CREATE =====

(defun org-embeddings-create (&optional source-or-element)
  "Get an embedding of an ELEMENT or a current org subtree.

FORCE: Get embedding even if hash didn't change.

Create an embedding and store it. Return a deferred object
resolving to a vector after the embedding is saved."
  (interactive)
  (unless source-or-element
    (signal 'wrong-type-argument (list 'org-elementp source-or-element)))

  (let ((source source-or-element))
    (unless (org-embeddings-source-p source)
      (setq source (org-embeddings-source-from-element (org-embeddings-element-at-point))))
    (org-embeddings-log "debug" "Creating embedding for `%s'" (org-embeddings-source-id source))
    (deferred:$
      (org-embeddings-openai-create source)
      (deferred:nextc it (lambda (record) (org-embeddings-store record))))))

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
           (model org-embeddings-openai-default-model))
       (let ((vectors-by-id (org-embeddings-json-load model))
             (locations))
         (maphash (lambda (_ object)
                    (let ((similarity (org-embeddings-vector-cosine-similarity vector (org-embeddings-record-vector object))))
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
                                             for metadata = (org-embeddings-record-metadata (cdr location))
                                             for file = (gethash "file" metadata)
                                             for title = (gethash "title" metadata)
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

  (defun org-embeddings-index-single-daily-file (target-file)
    "Index a single daily file TARGET-FILE."
    (org-embeddings-log "debug" "----- Indexing daily at `%s' -----" target-file)
    (condition-case err
        (let ((source (org-embeddings-file-get target-file)))
          (let ((current-embedding (org-embeddings-store-get org-embeddings-openai-default-model (org-embeddings-source-id source))))
            (if current-embedding
                (org-embeddings-log "debug" "Current embedding: %s" (org-embeddings-record-hash current-embedding))
              (org-embeddings-log "debug" "No current embedding found"))
            (cond ((and current-embedding (equal (org-embeddings-source-hash source) (org-embeddings-record-hash current-embedding)))
                   (org-embeddings-log "debug" "Embedding for `%s' is up to date" (org-embeddings-source-id source)))
                  ((and current-embedding (not (equal (org-embeddings-source-hash source) (org-embeddings-record-hash current-embedding))))
                   (org-embeddings-log "debug" "Embedding hashes differ for `%s': `%s' vs `%s'" (org-embeddings-source-id source) (org-embeddings-source-hash source) (org-embeddings-record-hash current-embedding)))
                  (t
                   (deferred:$
                     (deferred:nextc (org-embeddings-openai-tldr (org-embeddings-source-text source))
                       (lambda (tldr)
                         (org-embeddings-log "debug" "Got TLDR `%s' for `%s'" tldr (org-embeddings-source-id source))
                         (puthash "title" tldr (org-embeddings-source-metadata source))
                         (org-embeddings-create source)))
                     (deferred:error it
                       (lambda (err)
                         (org-embeddings-log "error" "Failed to index `%s': %s" target-file err)
                         (deferred:succeed nil)))
                     (deferred:nextc it (lambda (_) (deferred:wait-idle 1))))))))
      (error (progn
               (org-embeddings-log "error" "Failed to index `%s': %s" target-file err)
               (deferred:succeed)))))

  (defun org-embeddings-index-daily-files ()
    "Index all daily files."

    ;; Create a queue of target files
    ;; Start a new file in a callback

    (org-embeddings-json-load org-embeddings-openai-default-model)
    (deferred:$
      (deferred:loop (directory-files (expand-file-name org-roam-dailies-directory org-roam-directory) t "\\.org$")
        (lambda (target-file)
          (org-embeddings-index-single-daily-file target-file)))
      (deferred:nextc it (lambda (_) (org-embeddings-log "info" "===== Finished indexing daily files ====="))))))

;; (deferred:sync!   (org-embeddings-index-daily-files))
(provide 'org-embeddings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-embeddings.el ends here

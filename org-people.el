;;; -*- lexical-binding: t; -*-
;;
;; org-people.el - A package for working with a contact-list in an org-mode file
;;
;; Author: Steve Kemp <steve@steve.fi>
;; Version: 0.1
;; Package-Requires: ((emacs "28.0") (org "9.0"))
;; Keywords: outlines, contacts, people
;; URL: https://github.com/skx/org-people
;;
;;

(require 'org)
(require 'cl-lib)


;; Configuration / Storage

(defvar org-people--cache nil
  "Cached result of parsing the contact list.
This is used by `org-people' to avoid unnecessary parsing.")


(defvar org-people--cache-mtime nil
  "Modification time of the cached contacts file.
This is used by `org-people' to avoid unnecessary parsing.")


(defgroup org-people nil
  "Contact list helpers, using org-mode file as a backing store."
  :group 'org)

(defcustom org-people-file
  (expand-file-name "~/Org/PEOPLE.org")
  "The path to the org file containing contact details.
While the format isn't defined specifically it is assumed properties are used for storing data, and
we only process level-two headings beneath the header named by `org-people-headline'."
  :type 'file
  :group 'org-people)


(defcustom org-people-headline
  "People"
  "The name of the header beneath which contacts are parsed by `org-people'."
  :type 'string
  :group 'org-people)



;; Core

(defun org-people (&optional tags)
  "Return hash table of NAME -> PLIST from the file specified in `org-people-file'.

We only include data from level-2 headlines beneath the header named by `org-people-headline'.

If TAGS is non-nil (list of strings) then only entries with at least one matching tag are included.

Because this is the core of our package and parsing could be slow we cache data inside
the variable `org-people--cache' and record the mtime of the source file inside
`org-people--cache-mtime' to rebuild the cache when the source file changes."
  (unless (file-readable-p org-people-file)
    (error "org-people-file not readable: %s" org-people-file))
  (let ((file-mtime (nth 5 (file-attributes org-people-file))))
    ;; Return cached if valid
    (when (and org-people--cache
               (equal org-people--cache-mtime file-mtime))
      org-people--cache)
    ;; Otherwise, rebuild
    (let ((table (make-hash-table :test #'equal)))
      (with-current-buffer (find-file-noselect org-people-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         ;; Find the '* People' heading
         (when (re-search-forward
                (format org-complex-heading-regexp-format
                        (regexp-quote org-people-headline))
                nil t)
           (let ((root-level (org-outline-level))
                 (subtree-end (save-excursion (org-end-of-subtree t))))
             (forward-line)
             ;; Walk all level-2 children
             (while (and (< (point) subtree-end)
                         (re-search-forward org-heading-regexp subtree-end t))
               (when (= (org-outline-level) (1+ root-level))
                 (let ((name (nth 4 (org-heading-components)))
                       (plist nil)
                       (entry-tags (org-get-tags)))
                   ;; Skip if tags are required and no match
                   (when (or (null tags)
                             (cl-intersection tags entry-tags :test #'string=))
                     ;; Only explicit property drawer values
                     (dolist (prop (org-entry-properties nil 'standard))
                       (let ((key (intern (concat ":" (car prop))))
                             (val (cdr prop)))
                         (setq plist (plist-put plist key val))))
                     (when plist
                       (puthash name plist table))))))))))
      ;; Cache results
      (setq org-people--cache table)
      (setq org-people--cache-mtime file-mtime)
      table)))


(defun org-people-names ()
  "Return all known contact names found inside the file `org-people-file'."
  (sort (hash-table-keys (org-people)) #'string<))


(defun org-people-select-by-name ()
  "Use `completing-read' to select a single contact name.
All known contacts are presented, as determined by `org-people-names'."
  (completing-read "Contact name: " (org-people-names) nil t))


(defun org-people-get-by-name (name)
  "Return plist for NAME from the contact-file.

This is basically a way of finding \"all data\" about a given person."
  (gethash name (org-people)))


(defun org-people-insert ()
  "Insert a specific piece of data from a contact.

This uses `org-people-select-by-name' to first prompt the user for contact
name, and then the attribute which should be inserted."
  (interactive)
  (let* ((person (org-people-select-by-name))
         (values (org-people-get-by-name person)))
    (unless values
      (user-error "No properties defined for %s" person))
    (let* ((keys (cl-loop for (k v) on values by #'cddr collect k))
           (choice (intern (completing-read
                            "Attribute: "
                            (mapcar #'symbol-name keys)
                            nil t))))
      (insert (or (plist-get values choice) "")))))





;;
;; These are helpful functions, added over time, using the primitives above.
;;
;; These could have been written by users of our package.
;;




;;
;; Return single fields from a given contact.
;;
;; If no contact is specified prompt for one.
;;

(defun org-people-get-property (property &optional name)
  "Get PROPERTY for NAME."
  (when-let ((name (or name (org-people-select-by-name)))
             (plist (gethash name (org-people))))
    (plist-get plist property)))

(defun org-people-get-address (&optional name)
  "Get the :ADDRESS property of the given contact"
  (org-people-get-property :ADDRESS name))

(defun org-people-get-email (&optional name)
  "Get the :EMAIL property of the given contact"
  (org-people-get-property :EMAIL name))

(defun org-people-get-phone (&optional name)
  "Get the :PHONE property of the given contact"
  (org-people-get-property :PHONE name))




;;
;; Insert single fields from a given contact.
;;
;; If no contact is specified prompt for one.
;;

(defun org-people-insert-address ()
  "Insert the :ADDRESS property of a contact selected interactively by `org-people-select-by-name'."
  (interactive)
  (insert (or (org-people-get-address) "")))


(defun org-people-insert-email ()
  "Insert the :EMAIL property of a contact selected interactively by `org-people-select-by-name'."
  (interactive)
  (insert (or (org-people-get-email) "")))


(defun org-people-insert-phone ()
  "Insert the :PHONE property of a contact selected interactively by `org-people-select-by-name'."
  (interactive)
  (insert (or (org-people-get-phone) "")))


(defun org-people-insert-name ()
  "Insert the full name of a contact selected interactively by `org-people-select-by-name'."
  (interactive)
  (insert (or (org-people-select-by-name) "")))




;; get contacts by tag, in a way that is useful for inserting into a table.
;;
;;   Name, Phone, Email
(defun org-people-by-tag (tag)
  (let ((people (org-people (list tag)))
        (result))
    (maphash
     (lambda (name plist)
       (let ((phone (or (plist-get plist :PHONE) ""))
             (email (or (plist-get plist :EMAIL) "")))
         (push (list name phone email) result)))
     people)
    (nreverse result)))





;; package time is over now.
(provide 'org-people)

;;; -*- lexical-binding: t; -*-
;;
;; org-people.el - A package for working with a contact-list in an org-mode file
;;
;; Author: Steve Kemp <steve@steve.fi>
;; Version: 0.8
;; Package-Requires: ((emacs "28.0") (org "9.0"))
;; Keywords: outlines, contacts, people
;; URL: https://github.com/skx/org-people
;;
;; Version History (brief)
;;
;; 0.8 - Provide "[[person:Name Here]]" support with completion, clicking, and export attributes.
;;       Make org-people-open public and usefully available.
;;
;; 0.7 - Provide annotations for name-completion.
;;       Switch the org-people-summary to using tabulated-list-mode.
;;
;; 0.6 - The table-creating function has been renamed and updated.
;;       Now you can specify the fields to return.
;;
;; 0.5 - Drop simple functions.  They can be user-driver.
;;       Added filtering options and rewrote code to use them.
;;
;; 0.4 - Allow searching by property value.
;;       Added org-people-get-by-email, etc, using this new facility.
;;
;; 0.3 - :TAGS shows up as a comma-separated list in org-people-summary.
;;       org-people-summary is set to view-mode, so "q" buries the buffer.
;;
;; 0.2 - Added org-people-summary.
;;       Updated all contacts to have :TAGS and :NAME properties where appropriate.
;;
;; 0.1 - initial release
;;


(require 'org)
(require 'cl-lib)


;;
;; Configuration / Storage
;;

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


(defcustom org-people-summary-buffer-name
  "*Contacts*"
  "The name of the buffer to create/use with `org-people-summary'."
  :type 'string
  :group 'org-people)




;;
;; Core
;;

(defun org-people-parse ()
  "Return hash table of NAME -> PLIST from the file specified in `org-people-file'.

We only include data from level-2 headlines beneath the header named by `org-people-headline'.

If TAGS is non-nil (list of strings) then only entries with at least one matching tag are included.

Because this is the core of our package and parsing could be slow we cache data inside
the variable `org-people--cache' and record the mtime of the source file inside
`org-people--cache-mtime' to rebuild the cache when the source file changes."
  (unless (file-readable-p org-people-file)
    (user-error "org-people-file not readable: %s" org-people-file))
  (let ((file-mtime (nth 5 (file-attributes org-people-file))))
    (when (and org-people--cache
               (equal org-people--cache-mtime file-mtime))
      org-people--cache)
    ;; Otherwise parse afresh
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
                   ;; Get any associated properties
                   (dolist (prop (org-entry-properties nil 'standard))
                     (let ((key (intern (concat ":" (car prop))))
                           (val (cdr prop)))
                       (setq plist (plist-put plist key val))))
                   (when plist
                     (setq plist (plist-put plist :NAME name))
                     (setq plist (plist-put plist :TAGS entry-tags))
                     (puthash name plist table)))))))))
      ;; Cache results
      (setq org-people--cache table)
      (setq org-people--cache-mtime file-mtime)
      table)))





;;
;; Filtering and searching functions that build upon the data-structure
;; which org-people-parse returns.
;;

(defun org-people-names ()
  "Return all known contact names found inside the file `org-people-file'.

This uses `org-people-parse' to get the list of known contacts, with a cache
to avoid re-parsing unnecessarily."
  (sort (hash-table-keys (org-people-parse)) #'string<))


(defun org-people--completion-annotation (name)
  "Return an annotation string for contact NAME."
  (let* ((plist (org-people-get-by-name name))
         (email (plist-get plist :EMAIL))
         (phone (plist-get plist :PHONE)))
    (concat
     (when email
       (format "  [%s]" email))
     (when phone
       (format "  ☎ %s" phone)))))


(defun org-people--completion-table (string pred action)
  "Return a completion-table for contact completion."
  (if (eq action 'metadata)
      '(metadata
        (annotation-function . org-people--completion-annotation)
        (category . org-people))
    (complete-with-action action
                          (org-people-names)
                          string pred)))


(defun org-people-select-interactively ()
  "Use `completing-read' to select a single contact name, with annotations.

All known contacts are presented, as determined by `org-people-names'."
  (completing-read
   "Contact name: "
   #'org-people--completion-table
   nil t))


(defun org-people-get-by-name (name)
  "Return plist for NAME from the contact-file.

This is basically the way of getting all data known about a given person."
  (gethash name (org-people-parse)))


(defun org-people-filter (pred-p)
  "Filter all known contacts by the given predicate.

PRED-P should be a function which accepts the plist of properties associated
with a given contact, and returns `t' if they should be kept.

See `org-people-get-by-property' for an example use of this function."
  (cl-loop
   for plist being the hash-values of (org-people-parse)
   when (funcall pred-p plist)
   collect plist))


(defun org-people-get-by-property (property value &optional regexp)
  "Return contacts by searching the contents of a specific property.

By default an exact string match is applied, however if REGEXP is true
regexp is used instead."
  (org-people-filter (lambda(plist)
                       (let ((found (plist-get plist property)))
                         (if regexp
                             (if (string-match value (or found ""))
                                 t)
                           (if (string-equal value (or found ""))
                             t))))))


(defun org-people-tags-to-table (tag &optional props)
  "Return a list of contacts filtered by TAG.

This function is designed to create an `org-mode' table, like so:

#+NAME: family-contacts
#+BEGIN_SRC elisp :results value table
(org-people-tags-to-table \"family\" '(:NAME :PHONE))
#+END_SRC

PROPS is a list of property symbols to include, is nil we
default to '(:NAME :PHONE :EMAIL)."
  (let ((people (org-people-parse))
        (props (or props '(:NAME :PHONE :EMAIL)))
        (result))
    (push props result) ; header
    (maphash
     (lambda (name plist)
       (let ((tags (or (plist-get plist :TAGS) '())))
         (when (member tag tags)
           (push
            (mapcar
             (lambda (prop)
               (if (eq prop :NAME)
                   name
                 (or (plist-get plist prop) "")))
             props)
            result))))
     people)
    (nreverse result)))


(defun org-people-insert ()
  "Insert a specific piece of data from a contact.

This uses `org-people-select-interactively' to first prompt the user for contact
name, and then a second interactive selection of the specific attribute value
which should be inserted."
  (interactive)
  (let* ((person (org-people-select-interactively))
         (values (org-people-get-by-name person)))
    (unless values
      (user-error "No properties defined for %s" person))
    (let* ((keys (cl-loop for (k v) on values by #'cddr collect k))
           (choice (intern (completing-read
                            "Attribute: "
                            (mapcar #'symbol-name keys)
                            nil t))))
      (insert (or (plist-get values choice) "")))))


(defun org-people-browse-name (&optional name)
  "Open the Org entry for NAME.

If NAME is not set then prompt for it interactively.

This is used by our [[people:xxx]] handler."
  (interactive)
  (if (not name)
      (setq name (org-people-select-interactively)))
  ;; Open the org file
  (find-file org-people-file)
  (goto-char (point-min))
  ;; Search for the headline with this name under the main headline
  (let ((headline-regexp
         (format org-complex-heading-regexp-format
                 (regexp-quote org-people-headline)))
        found)
    (when (re-search-forward headline-regexp nil t)
      (let ((root-level (org-outline-level))
            (subtree-end (save-excursion (org-end-of-subtree t))))
        (cl-block find-contact
          (forward-line)
          (while (and (< (point) subtree-end)
                      (re-search-forward org-heading-regexp subtree-end t))
            (when (and (= (org-outline-level) (1+ root-level))
                       (string= name (nth 4 (org-heading-components))))
              (setq found t)
              (goto-char (match-beginning 0))
              (org-show-entry)
              (org-reveal)
              (message "Opened %s" name)
              (cl-return-from find-contact))))))
    (unless found
      (user-error "Could not find org entry for %s" name))))




;;
;; Summary-view of contacts.
;;


(defun org-people--list ()
  "Return a list of all contact plists."
  (cl-loop
   for plist being the hash-values of (org-people-parse)
   collect plist))


(define-derived-mode org-people-summary-mode tabulated-list-mode "Org-People"
  "Major mode for listing Org People contacts."

  (setq tabulated-list-format
        [("Name" 30 t)
         ("Email" 30 t)
         ("Phone" 15 t)
         ("Tags"  20 t)])

  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))

  (add-hook 'tabulated-list-revert-hook
            #'org-people-summary--refresh
            nil t)

  (tabulated-list-init-header))

(defun org-people-summary--entry (plist)
  "Convert PLIST to a `tabulated-list-mode' entry."
  (let* ((name  (or (plist-get plist :NAME) ""))
         (email (or (plist-get plist :EMAIL) ""))
         (phone (or (plist-get plist :PHONE) ""))
         (tags  (mapconcat #'identity
                           (or (plist-get plist :TAGS) '())
                           ",")))
    (list name (vector name email phone tags))))


(defun org-people-summary--refresh ()
  "Populate `tabulated-list-entries'."
  (setq tabulated-list-entries
        (mapcar #'org-people-summary--entry
                (org-people--list))))


(defun org-people-summary--open ()
  "Open the Org entry for the contact at point."
  (interactive)
  (let* ((name (tabulated-list-get-id))
         (plist (org-people-get-by-name name)))
    (unless plist
      (user-error "No contact found: %s" name))
    (org-people-browse-name name)))


(defun org-people--export-person-link (path desc backend)
  "Export a person link for BACKEND.
PATH is the person name, DESC is the description.

We just make the name bold."
  (let ((name (or desc path)))
    (cond
     ((eq backend 'html)
      (format "<strong>%s</strong>" name))
     (t
      name))))


(defun org-people-summary--copy-field ()
  "Copy the value of the field under point to the clipboard."
 (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (columns tabulated-list-format)
         (start 0)
         col)
    ;; Determine which column the point is in
    (catch 'found
      (dotimes (i (length columns))
        (let* ((col-info (if (vectorp columns) (aref columns i) (nth i columns)))
               (width (if (vectorp col-info) (aref col-info 1) (nth 1 col-info))))
          (when (<= start (current-column) (+ start width))
            (setq col i)
            (throw 'found t))
          (setq start (+ start width)))))
    ;; Copy value if found
    (if (and entry col)
        (let ((value (aref entry col)))  ;; entry is always a vector
          (kill-new value)
          (message "Copied: %s" value))
      (message "Could not determine field under point"))))


(defun org-people-summary--filter-by-property ()
  "Filter contacts interactively by a property value."
  (interactive)
  (let* ((prop-str (completing-read
                    "Property (e.g., :EMAIL, :PHONE, :TAGS): "
                    '(":NAME" ":EMAIL" ":PHONE" ":TAGS")
                    nil t))
         (prop (intern prop-str))
         (value (read-string (format "Value to match for %s: " prop-str))))
    ;; Filter list
    (let ((filtered
           (org-people-filter
            (lambda (plist)
              (let ((v (plist-get plist prop)))
                (cond
                 ((listp v) (member value v))
                 ((stringp v) (string-match value v))
                 (t nil)))))))
      ;; Refresh buffer
      (with-current-buffer org-people-summary-buffer-name
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq tabulated-list-entries
                (mapcar #'org-people-summary--entry filtered))
          (tabulated-list-print t))))))


(defun org-people-summary ()
  "Display contacts using `tabulated-list-mode'.

This allow sorting by each column, etc.

Filtering can be applied (using a regexp) by pressing 'f'."
  (interactive)

  (let ((buf (get-buffer-create org-people-summary-buffer-name)))
    (with-current-buffer buf
      (org-people-summary-mode)
      (org-people-summary--refresh)
      (tabulated-list-print t))
    (pop-to-buffer buf)))


;; Open the contact details on RET, filter by `f'.
(define-key org-people-summary-mode-map (kbd "RET") #'org-people-summary--open)
(define-key org-people-summary-mode-map (kbd "c") #'org-people-summary--copy-field)
(define-key org-people-summary-mode-map (kbd "f") #'org-people-summary--filter-by-property)


;;
;; Define a handler for "person:XXX" and "org-person:XXX"
;;
(org-link-set-parameters
 "person"
 :complete #'org-people-select-interactively
 :export   #'org-people--export-person-link
 :follow   #'org-people-open
 :help-echo "Open the contacts-file at the position of the named person, via org-people")


(org-link-set-parameters
 "org-person"
 :complete #'org-people-select-interactively
 :export   #'org-people--export-person-link
 :follow   #'org-people-open
 :help-echo "Open the contacts-file at the position of the named person, via org-people")


;; package time is over now.
(provide 'org-people)

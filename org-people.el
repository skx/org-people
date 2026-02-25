;;; -*- lexical-binding: t; -*-
;;
;; org-people.el - A package for working with a contact-list in an org-mode file
;;
;; Author: Steve Kemp <steve@steve.fi>
;; Version: 0.7
;; Package-Requires: ((emacs "28.0") (org "9.0"))
;; Keywords: outlines, contacts, people
;; URL: https://github.com/skx/org-people
;;
;; Version History (brief)
;;
;; 0.7 - Provide annotations for name-completion.
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


(defcustom org-people-summary-template
  "{NAME} {PHONE} {EMAIL} {TAGS}"
  "The format of the string to insert into the buffer when `org-people-summary' is invoked.

Each entry will be formatted with this string by `org-people--format-plist' and terminated with a newline."
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
(org-people-tags-to-table "family" '(:NAME :PHONE))
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





;;
;; These are helpful functions, added over time, using the primitives above.
;;
;; These could have been written by users of our package.
;;


(defun org-people--format-plist (plist template)
  "Format TEMPLATE replacing:

  {KEY}
  {KEY|fallback}
  {KEY:WIDTH}
  {KEY:WIDTH|fallback}

WIDTH works like `format':
  positive  -> right aligned
  negative  -> left aligned

Values longer than WIDTH are truncated.

When a key is not found it is replaced with the fallback if present, otherwise nothing.

This function is used by `org-people-summary'."
  (replace-regexp-in-string
   "{\\([^}|:]+\\)\\(?::\\([-0-9]+\\)\\)?\\(?:|\\([^}]*\\)\\)?}"
   (lambda (match)
     ;; Re-match against the match itself to extract groups
     (string-match
      "{\\([^}|:]+\\)\\(?::\\([-0-9]+\\)\\)?\\(?:|\\([^}]*\\)\\)?}"
      match)
     (let* ((key-str (match-string 1 match))
            (width-str (match-string 2 match))
            (fallback (match-string 3 match))
            (key (intern (concat ":" key-str)))
            (val (plist-get plist key)))

       ;; If the value is a list, flatten.  This is for :TAGS
       (if (listp val)
           (setq val (mapconcat 'identity val ",")))

       ;; Apply fallback if missing or empty
       (setq val (if (and val (not (equal val "")))
                     val
                   (or fallback "")))

       ;; Apply width and truncation
       (if width-str
           (let* ((width (string-to-number width-str))
                  (abs-width (abs width))
                  ;; truncate if needed
                  (val (if (> (length val) abs-width)
                           (substring val 0 abs-width)
                         val)))
             ;; pad using format
             (format (format "%%%ds" width) val))
         val)))
   template t t))

(defun org-people-summary ()
  "Create a popup buffer containing a summary of all known contacts.

By default we insert the name, phone, and email addresses, but the
specified fields to be inserted are specified by the format string
which is stored in `org-people-summary-template'.

The buffer created is specified by `org-people-summary-buffer-name',
and `view-mode' is enabled once complete to allow 'q' to bury the
results."
  (interactive)
  ;; get, and kill, any pre-existing buffer with this name.
  (when-let ((buf (get-buffer org-people-summary-buffer-name)))
    (kill-buffer buf))

  ;; create replacement buffer.
  (pop-to-buffer (get-buffer-create org-people-summary-buffer-name))
  (let ((people (org-people-parse)))
    (maphash
     (lambda (name plist)
       (insert (org-people--format-plist plist org-people-summary-template))
       (insert "\n"))
     people))
  (goto-char (point-min))
  (view-mode))






;; package time is over now.
(provide 'org-people)

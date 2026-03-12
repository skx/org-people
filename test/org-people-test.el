;;; org-people-test.el --- ERT tests for org-people.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'org-people)

;;; Code:

(defmacro org-people--with-mocked-people (&rest body)
  "Run BODY with org-people reading from a fixed org file."
  `(let ((org-people-search-type (list "org-people-test.org")))
     (find-file "./org-people-test.org")
     (unwind-protect
         (progn ,@body)
       (kill-buffer (current-buffer)))))

;; ----------------------------------------------------------------------
;; Test Parsing
;; ----------------------------------------------------------------------

(ert-deftest org-people-parse-test ()
  "Test that `org-people-parse' correctly parses entries."
  (org-people--with-mocked-people
   (let ((table (org-people-parse)))
     (should (hash-table-p table))
     (should (equal (sort (hash-table-keys table) #'string<)
                    '("Alice Smith" "Bob Jones" "Carol White" "Steve Kemp")))
     (should (equal (sort (hash-table-keys table) #'string<)
                    (org-people-names)))
     (let ((alice (gethash "Alice Smith" table)))
       (should (equal (plist-get alice :EMAIL) "alice@example.com"))
       (should (equal (plist-get alice :PHONE) "111-222-3333"))
       (should (equal (plist-get alice :TAGS) '("friend" "work")))))))

;; ----------------------------------------------------------------------
;; Test get-by-name
;; ----------------------------------------------------------------------

(ert-deftest org-people-get-by-name-test ()
  "Test `org-people-get-by-name' returns correct plist."
  (org-people--with-mocked-people
   (let ((alice (org-people-get-by-name "Alice Smith")))
     (should (equal (plist-get alice :EMAIL) "alice@example.com"))
     (should (equal (plist-get alice :PHONE) "111-222-3333"))
     (should (equal (plist-get alice :NAME) "Alice Smith")))))

;; ----------------------------------------------------------------------
;; Test person-to-table
;; ----------------------------------------------------------------------

(ert-deftest org-people-person-to-table-test ()
  "Test `org-people-person-to-table' generates expected table."
  (org-people--with-mocked-people
   (let ((rows (org-people-person-to-table "Bob Jones")))
     (should (equal (mapcar #'car rows)
                    '("Category" "Email" "Name" "Phone" "Tags")))
     (let ((plist (org-people-get-by-name "Bob Jones")))
       (dolist (row rows)
         (let ((key (car row))
               (val (cadr row)))
           (cond
            ((string= key "Category")
             (should (equal val (plist-get plist :CATEGORY))))
            ((string= key "Email")
             (should (equal val (plist-get plist :EMAIL))))
            ((string= key "Name")
             (should (equal val (plist-get plist :NAME))))
            ((string= key "Phone")
             (should (equal val (plist-get plist :PHONE))))
            ((string= key "Tags")
             (should (equal val (plist-get plist :TAGS)))))))))))

;; ----------------------------------------------------------------------
;; Test struct parsing
;; ----------------------------------------------------------------------
(ert-deftest org-people-summary--make-column ()
  "Test we parse the legacy structures correctly."
  (let ((a '(:NAME :title "Steve" :width 21))
        (b '(:NAME 29)))
    (should (equal (org-people-column-title (org-people-summary--make-column a)) "Steve"))
    (should (equal (org-people-column-title (org-people-summary--make-column b)) "Name"))
    (should (equal (org-people-column-width (org-people-summary--make-column a)) 21))
    (should (equal (org-people-column-width (org-people-summary--make-column b)) 29))
    ))

;; ----------------------------------------------------------------------
;; Test tags-to-table
;; ----------------------------------------------------------------------

(ert-deftest org-people-tags-to-table-test ()
  "Test `org-people-tags-to-table' returns correct filtered table."
  (org-people--with-mocked-people
   (let ((table (org-people-tags-to-table "work" '(:NAME :EMAIL))))
     (should (equal (car table) '(:NAME :EMAIL)))
     (let ((names (mapcar #'car (cdr table))))
       (should (member "Alice Smith" names))
       (should (member "Carol White" names))
       (should-not (member "Bob Jones" names))))))

;; ----------------------------------------------------------------------
;; Test get-by-property
;; ----------------------------------------------------------------------

(ert-deftest org-people-get-by-property-regexp-test ()
  "Test `org-people-get-by-property' with regexp."
  (org-people--with-mocked-people
   (let ((results (org-people-get-by-property :EMAIL ".....@example.com" t)))
     (should (listp results))
     (should (equal (sort (mapcar (lambda (plist) (plist-get plist :NAME)) results)
                          #'string<)
                    '("Alice Smith" "Carol White"))))))

(ert-deftest org-people-get-by-property-literal-test ()
  "Test `org-people-get-by-property' literal match."
  (org-people--with-mocked-people
   (let ((results (org-people-get-by-property :EMAIL "bob@example.com")))
     (should (listp results))
     (should (equal (sort (mapcar (lambda (plist) (plist-get plist :NAME)) results)
                          #'string<)
                    '("Bob Jones"))))))

;; ----------------------------------------------------------------------
;; Alias tests
;; ----------------------------------------------------------------------

(ert-deftest org-people-alias-table-test ()
  "Test alias table includes canonical names and nickname."
  (org-people--with-mocked-people
   (let* ((table (org-people--alias-table))
          (alice (gethash "Alice Smith" table))
          (nick (gethash "Ally" table)))
     (should (equal alice "Alice Smith"))
     (should (equal nick "Alice Smith")))))

;; ----------------------------------------------------------------------
;; Completion tests
;; ----------------------------------------------------------------------

(ert-deftest org-people-completion-table-test ()
  "Test completion table metadata and results."
  (org-people--with-mocked-people
   (let ((meta (org-people--completion-table "" nil 'metadata)))
     (should (eq (car meta) 'metadata))
     (should (assq 'annotation-function (cdr meta))))
   (let ((result (org-people--completion-table "Ali" nil t)))
     (should (member "Alice Smith" result)))))

(ert-deftest org-people-completion-annotation-test ()
  "Test annotation string contains email and phone."
  (org-people--with-mocked-people
   (let ((annotation (org-people--completion-annotation "Alice Smith")))
     (should (string-match "alice@example.com" annotation))
     (should (string-match "111-222-3333" annotation)))))

(ert-deftest org-people-completion-annotation-alias-test ()
  "Test annotation shows canonical name when alias used."
  (org-people--with-mocked-people
   (let ((annotation (org-people--completion-annotation "Ally")))
     (should (string-match "(Alice Smith)" annotation)))))

(ert-deftest org-people-select-interactively-test ()
  "Test interactive selection resolves alias."
  (org-people--with-mocked-people
   (cl-letf (((symbol-function 'completing-read)
              (lambda (&rest _) "Ally")))
     (should (equal (org-people-select-interactively)
                    "Alice Smith")))))

;; ----------------------------------------------------------------------
;; org-people-insert
;; ----------------------------------------------------------------------

(ert-deftest org-people-insert-test ()
  "Test inserting a property value."
  (org-people--with-mocked-people
   (with-temp-buffer
     (cl-letf (((symbol-function 'org-people-select-interactively)
                (lambda () "Alice Smith"))
               ((symbol-function 'completing-read)
                (lambda (&rest _) ":EMAIL")))
       (org-people-insert)
       (should (equal (buffer-string) "alice@example.com"))))))

;; ----------------------------------------------------------------------
;; Property discovery
;; ----------------------------------------------------------------------

(ert-deftest org-people-properties-test ()
  "Test property extraction across all contacts."
  (org-people--with-mocked-people
   (let* ((table (org-people-parse))
          (props (org-people--properties table)))
     (should (memq :EMAIL props))
     (should (memq :PHONE props))
     (should (memq :NAME props)))))

;; ----------------------------------------------------------------------
;; org-people-filter
;; ----------------------------------------------------------------------

(ert-deftest org-people-filter-test ()
  "Test predicate filtering."
  (org-people--with-mocked-people
   (let ((results
          (org-people-filter
           (lambda (plist)
             (string-match-p "example.com"
                             (or (plist-get plist :EMAIL) ""))))))
     (should (= (length results) 3)))))

;; ----------------------------------------------------------------------
;; Summary entry
;; ----------------------------------------------------------------------

(ert-deftest org-people-summary-entry-test ()
  "Test conversion of plist to tabulated-list entry."
  (org-people--with-mocked-people
   (org-people-summary)
   (let* ((plist (org-people-get-by-name "Alice Smith"))
          (entry (org-people-summary--entry plist)))
     (should (equal (car entry) "Alice Smith"))
     (should (vectorp (cadr entry)))
     (should (equal (aref (cadr entry) 1) "alice@example.com")))))

;; ----------------------------------------------------------------------
;; Summary open
;; ----------------------------------------------------------------------

(ert-deftest org-people-summary-open-test ()
  "Test RET opens correct contact."
  (org-people--with-mocked-people
   (org-people-summary)
   (let ((opened nil))
     (cl-letf (((symbol-function 'org-people-browse-name)
                (lambda (name) (setq opened name))))
       (with-temp-buffer
         (org-people-summary-mode)
         (org-people-summary--refresh)
         (setq tabulated-list-entries
               (mapcar #'org-people-summary--entry
                       (org-people--all-plists)))
         (tabulated-list-print)
         (goto-char (point-min))
         (forward-line)
         (org-people-summary--open)
         (should (equal opened "Bob Jones")))))))

;; ----------------------------------------------------------------------
;; Copy field
;; ----------------------------------------------------------------------

(ert-deftest org-people-summary-copy-field-test ()
  "Test copying field under point."
  (org-people--with-mocked-people
   (let ((copied nil))
     (cl-letf (((symbol-function 'kill-new)
                (lambda (value) (setq copied value))))
       (with-temp-buffer
         (org-people-summary)
         (tabulated-list-print)
         (goto-char (point-min))
         (forward-char 35)
         (org-people-summary--copy-field)
         (should (equal copied "alice@example.com"))
         (forward-char 35)
         (org-people-summary--copy-field)
         (should (equal copied "111-222-3333"))
         (forward-line)
         (org-people-summary--copy-field)
         (should (equal copied "Bob Jones")))))))

;; ----------------------------------------------------------------------
;; Column toggling
;; ----------------------------------------------------------------------

(ert-deftest org-people-summary-toggle-column-test ()
  "Test toggling column visibility."
  (org-people--with-mocked-people
   (org-people-summary)
   (let* ((col (car org-people-summary--columns))
          (initial (org-people-column-visible col)))
     (setf (org-people-column-visible col) (not initial))
     (org-people-summary--refresh)
     (should (eq (org-people-column-visible col) (not initial))))))

(ert-deftest org-people-summary-show-all-columns-test ()
  "Test restoring hidden columns."
  (org-people--with-mocked-people
   (org-people-summary)
   (setf (org-people-column-visible (car org-people-summary--columns)) nil)
   (org-people-summary-show-all-columns)
   (should (cl-every #'org-people-column-visible
                     org-people-summary--columns))))

;; ----------------------------------------------------------------------
;; Link handling
;; ----------------------------------------------------------------------

(ert-deftest org-people-browse-name-test ()
  "Test jumping to marker."
  (org-people--with-mocked-people
   (let ((marker (plist-get (org-people-get-by-name "Alice Smith") :MARKER)))
     (should (markerp marker))
     (should (buffer-live-p (marker-buffer marker))))))

(ert-deftest org-people-export-person-link-html-test ()
  "Test HTML export of org-people link."
  (org-people--with-mocked-people
    (let ((plist (org-people-get-by-name "Steve Kemp")))
       (should (equal (plist-get plist :WEBSITE) "https://steve.fi/")))

    (should (equal
             (org-people--export-person-link "Alice Smith" nil 'html)
             "Alice Smith"))
    (should (equal
             (org-people--export-person-link "Steve Kemp" nil 'html)
             "<a href=\"https://steve.fi/\">Steve Kemp</a>"))))

;; ----------------------------------------------------------------------
;; add-descriptions
;; ----------------------------------------------------------------------

(ert-deftest org-people-add-descriptions-test ()
  "Ensure missing descriptions are added."
  (with-temp-buffer
    (insert "[[org-people:Alice Smith]]")
    (goto-char (point-min))
    (org-people-add-descriptions)
    (should (equal (buffer-string)
                   "[[org-people:Alice Smith][Alice Smith]]"))))

;; ----------------------------------------------------------------------
;; vCard export
;; ----------------------------------------------------------------------

(ert-deftest org-people-export-to-vcard-test ()
  "Test vCard buffer creation and contents."
  (org-people--with-mocked-people
   (org-people-export-to-vcard "Alice Smith")
   (let ((buf (get-buffer-create org-people-vcard-buffer-name)))
     (should buf)
     (with-current-buffer buf
       (goto-char (point-min))
       (should (search-forward "BEGIN:VCARD" nil t))
       (should (search-forward "FN:Alice Smith" nil t))
       (should (search-forward "EMAIL;TYPE=INTERNET:alice@example.com" nil t))
       (set-buffer-modified-p nil)))))

;; ----------------------------------------------------------------------
;; CSV export
;; ----------------------------------------------------------------------

(ert-deftest org-people-export-to-csv-test ()
  "Test CSV buffer creation and contents."
  (org-people--with-mocked-people
   ;; bob is fine
   (org-people-export-to-csv "Bob Jones")
   (let ((buf (get-buffer-create org-people-csv-buffer-name)))
     (should buf)
     (with-current-buffer buf
       (goto-char (point-min))
       (should (search-forward "\"Bob Jones\", " nil t))
       (goto-char (point-min))
       (should (search-forward "\"bob@example.com\", " nil t))
       (set-buffer-modified-p nil)))
   ;; carol has no phone
   (org-people-export-to-csv "Carol White")
   (let ((buf (get-buffer-create org-people-csv-buffer-name)))
     (should buf)
     (with-current-buffer buf
       (goto-char (point-min))
       (should (search-forward "\"Carol White\", " nil t))
       (goto-char (point-min))
       (should (search-forward "\"carol@example.com\"," nil t))
       (goto-char (point-min))
       (should (search-forward ", \"\"" nil t)) ; empty phone number is ""
       (set-buffer-modified-p nil))))
  ;; CSV escaping
  (should (equal "\"Steve\"" (org-people--csv-escape "Steve")))
  (should (equal "\"Quote \"\" Name\"" (org-people--csv-escape "Quote \" Name")))
  )


;; ----------------------------------------------------------------------
;; Run tests
;; ----------------------------------------------------------------------

(ert-run-tests-interactively t)

;;; org-people-test.el ends here

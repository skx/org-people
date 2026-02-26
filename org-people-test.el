;;; org-people-test.el --- ERT tests for org-people.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-people)

;; ----------------------------------------------------------------------
;; Mock Org content (no leading spaces, column 0)
;; ----------------------------------------------------------------------
(defconst org-people--mock-org
"* People
** Alice Smith  :friend:work:
:PROPERTIES:
:EMAIL: alice@example.com
:PHONE: 111-222-3333
:END:

** Bob Jones :family:
:PROPERTIES:
:EMAIL: bob@example.com
:PHONE: 444-555-6666
:END:

** Carol White :work:
:PROPERTIES:
:EMAIL: carol@example.com
:END:
")

;; ----------------------------------------------------------------------
;; Helper: write contents to a temporary file
;; ----------------------------------------------------------------------
(defvar org-people--test-file "/tmp/org-people-test.org")

(defmacro org-people--with-temp-file (contents &rest body)
  "Run BODY with org-people reading from a temporary org file with CONTENTS.

The file used is `org-people--test-file'."
  `(let ((org-people-file org-people--test-file))
     (with-temp-file org-people--test-file
       (insert ,contents)
       (unless (string-suffix-p "\n" ,contents)
         (insert "\n")))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p org-people--test-file)
         (delete-file org-people--test-file)))))

;; ----------------------------------------------------------------------
;; Test 1: Parsing
;; ----------------------------------------------------------------------
(ert-deftest org-people-parse-test ()
  "Test that org-people-parse correctly parses entries."
  (org-people--with-temp-file org-people--mock-org
    (let ((table (org-people-parse)))
      (should (hash-table-p table))
      (should (equal (sort (hash-table-keys table) #'string<)
                     '("Alice Smith" "Bob Jones" "Carol White")))
      (let ((alice (gethash "Alice Smith" table)))
        (should (equal (plist-get alice :EMAIL) "alice@example.com"))
        (should (equal (plist-get alice :PHONE) "111-222-3333"))
        (should (equal (plist-get alice :TAGS) '("friend" "work")))))))

;; ----------------------------------------------------------------------
;; Test 2: get-by-name
;; ----------------------------------------------------------------------
(ert-deftest org-people-get-by-name-test ()
  "Test org-people-get-by-name returns correct plist."
  (org-people--with-temp-file org-people--mock-org
    (let ((alice (org-people-get-by-name "Alice Smith")))
      (should (equal (plist-get alice :EMAIL) "alice@example.com"))
      (should (equal (plist-get alice :PHONE) "111-222-3333"))
      (should (equal (plist-get alice :NAME) "Alice Smith")))))

;; ----------------------------------------------------------------------
;; Test 3: person-to-table
;; ----------------------------------------------------------------------
(ert-deftest org-people-person-to-table-test ()
  "Test org-people-person-to-table generates expected table."
  (org-people--with-temp-file org-people--mock-org
    (let ((rows (org-people-person-to-table "Bob Jones")))
      (should (equal (mapcar #'car rows) '("Category" "Email" "Name" "Phone" "Tags")))
      (let ((plist (org-people-get-by-name "Bob Jones")))
        (dolist (row rows)
          (let ((key (car row)) (val (cadr row)))
            (cond
             ((string= key "Category") (should (equal val (plist-get plist :CATEGORY))))
             ((string= key "Email") (should (equal val (plist-get plist :EMAIL))))
             ((string= key "Name")  (should (equal val (plist-get plist :NAME))))
             ((string= key "Phone") (should (equal val (plist-get plist :PHONE))))
             ((string= key "Tags")  (should (equal val (plist-get plist :TAGS)))))))))))

;; ----------------------------------------------------------------------
;; Test 4: tags-to-table
;; ----------------------------------------------------------------------
(ert-deftest org-people-tags-to-table-test ()
  "Test org-people-tags-to-table returns correct filtered table."
  (org-people--with-temp-file org-people--mock-org
    (let ((table (org-people-tags-to-table "work" '(:NAME :EMAIL))))
      (should (equal (car table) '(:NAME :EMAIL)))
      (let ((names (mapcar #'car (cdr table))))
        (should (member "Alice Smith" names))
        (should (member "Carol White" names))
        (should-not (member "Bob Jones" names))))))

;; ----------------------------------------------------------------------
;; Test 5: confirm written data
;; ----------------------------------------------------------------------
(ert-deftest org-people-temp-file-content-test ()
  "Ensure temp org-people file exists and contains expected contact data."
  (org-people--with-temp-file org-people--mock-org
    (should (file-exists-p org-people--test-file))
    (with-temp-buffer
      (insert-file-contents org-people--test-file)
      (should (string-match-p "\\*\\* Alice Smith" (buffer-string)))
      (should (string-match-p ":EMAIL: alice@example.com" (buffer-string)))
      (should (string-match-p ":PHONE: 111-222-3333" (buffer-string)))
      (should (string-match-p "\\*\\* Bob Jones" (buffer-string)))
      (should (string-match-p "\\*\\* Carol White" (buffer-string))))))


(ert-run-tests-interactively t)

;;; org-people-test.el ends here

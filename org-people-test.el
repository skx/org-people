;;; org-people-test.el --- ERT tests for org-people.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-people)

;; ----------------------------------------------------------------------
;; Mock Org content (no leading spaces, column 0)
;; ----------------------------------------------------------------------
(defconst org-people--mock-org
"* People
** Alice Smith  :friend:work:contact:
:PROPERTIES:
:EMAIL: alice@example.com
:PHONE: 111-222-3333
:END:
Alice doesn't love [[org-people:Bob Jones]]

** Bob Jones :family:contact:
:PROPERTIES:
:EMAIL: bob@example.com
:PHONE: 444-555-6666
:END:

** Carol White :work:contact:
:PROPERTIES:
:EMAIL: carol@example.com
:END:
")


;; ----------------------------------------------------------------------
;; Helper: write contents to fixed file and ensure that is used.
;; ----------------------------------------------------------------------
(defvar org-people--test-file "org-people-test.org")

(defmacro org-people--with-mocked-people (contents &rest body)
  "Run BODY with org-people reading from a fixed org file with CONTENTS."
  `(let ((org-people-search-type (list org-people--test-file)))
     ;; Clean up any previous buffer + file
     (when-let ((buf (get-file-buffer org-people--test-file)))
       (kill-buffer buf))
     (when (file-exists-p org-people--test-file)
       (delete-file org-people--test-file))

     ;; Write fresh file
     (with-temp-file org-people--test-file
       (insert ,contents)
       (unless (string-suffix-p "\n" ,contents)
         (insert "\n")))

     (unwind-protect
         (progn ,@body)
       ;; Final cleanup: kill buffer and delete file
       (when-let ((buf (get-file-buffer org-people--test-file)))
         (kill-buffer buf))
       (when (file-exists-p org-people--test-file)
         (delete-file org-people--test-file)))))

;;
;; Test cases follow
;;

;; ----------------------------------------------------------------------
;; Test confirm written data
;; ----------------------------------------------------------------------
(ert-deftest org-people-temp-file-content-test ()
  "Ensure temp org-people file exists and contains expected contact data."
  (org-people--with-mocked-people org-people--mock-org
    (should (file-exists-p org-people--test-file))
    (with-temp-buffer
      (insert-file-contents org-people--test-file)
      (should (string-match-p "\\*\\* Alice Smith" (buffer-string)))
      (should (string-match-p ":EMAIL: alice@example.com" (buffer-string)))
      (should (string-match-p ":PHONE: 111-222-3333" (buffer-string)))
      (should (string-match-p "\\*\\* Bob Jones" (buffer-string)))
      (should (string-match-p "\\*\\* Carol White" (buffer-string))))))


;; ----------------------------------------------------------------------
;; Test Parsing
;; ----------------------------------------------------------------------
(ert-deftest org-people-parse-test ()
  "Test that org-people-parse correctly parses entries."
  (org-people--with-mocked-people org-people--mock-org
    (let ((table (org-people-parse)))
      (should (hash-table-p table))
      (should (equal (sort (hash-table-keys table) #'string<)
                     '("Alice Smith" "Bob Jones" "Carol White")))
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
  "Test org-people-get-by-name returns correct plist."
  (org-people--with-mocked-people org-people--mock-org
    (let ((alice (org-people-get-by-name "Alice Smith")))
      (should (equal (plist-get alice :EMAIL) "alice@example.com"))
      (should (equal (plist-get alice :PHONE) "111-222-3333"))
      (should (equal (plist-get alice :NAME) "Alice Smith")))))

;; ----------------------------------------------------------------------
;; Test person-to-table
;; ----------------------------------------------------------------------
(ert-deftest org-people-person-to-table-test ()
  "Test org-people-person-to-table generates expected table."
  (org-people--with-mocked-people org-people--mock-org
    (let ((rows (org-people-person-to-table "Bob Jones")))
      (should (equal (mapcar #'car rows) '("Category" "Email" "Marker" "Name" "Phone" "Tags")))
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
;; Test tags-to-table
;; ----------------------------------------------------------------------
(ert-deftest org-people-tags-to-table-test ()
  "Test org-people-tags-to-table returns correct filtered table."
  (org-people--with-mocked-people org-people--mock-org
    (let ((table (org-people-tags-to-table "work" '(:NAME :EMAIL))))
      (should (equal (car table) '(:NAME :EMAIL)))
      (let ((names (mapcar #'car (cdr table))))
        (should (member "Alice Smith" names))
        (should (member "Carol White" names))
        (should-not (member "Bob Jones" names))))))


;; ----------------------------------------------------------------------
;; Test org-people-get-by-property (with regexp, then with literal)
;; ----------------------------------------------------------------------
(ert-deftest org-people-get-by-property-regexp-test ()
  "Test org-people-get-by-property returns correct filtered table with a regexp"
  ;; regexp
  (org-people--with-mocked-people org-people--mock-org
    (let ((results (org-people-get-by-property :EMAIL ".....@example.com" t)))
      (should (listp results))
      (should (equal (sort (mapcar (lambda (plist) (plist-get plist :NAME)) results))
                     '("Alice Smith" "Carol White")))
      )))

(ert-deftest org-people-get-by-property-literal-test ()
  "Test org-people-get-by-property returns correct filtered table without a regexp"
  ;; literal
  (org-people--with-mocked-people org-people--mock-org
  (let ((results (org-people-get-by-property :EMAIL "bob@example.com")))
    (should (listp results))
    (should (equal (sort (mapcar (lambda (plist) (plist-get plist :NAME)) results))
                   '("Bob Jones")))
    )))



;;;
;; Run the test cases
;;;
(ert-run-tests-interactively t)

;;; org-people-test.el ends here

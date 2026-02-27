;;; org-people-test.el --- ERT tests for org-people.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-people)

(defmacro org-people--with-mocked-people ( &rest body)
  "Run BODY with org-people reading from a fixed org file."
  `(let ((org-people-search-type (list "org-people-test.org")))

     (find-file "./org-people-test.org")

     ;; Run the test-body
     (unwind-protect
         (progn ,@body))))

;;
;; Test cases follow
;;


;; ----------------------------------------------------------------------
;; Test Parsing
;; ----------------------------------------------------------------------
(ert-deftest org-people-parse-test ()
  "Test that org-people-parse correctly parses entries."
  (org-people--with-mocked-people
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
  (org-people--with-mocked-people
    (let ((alice (org-people-get-by-name "Alice Smith")))
      (should (equal (plist-get alice :EMAIL) "alice@example.com"))
      (should (equal (plist-get alice :PHONE) "111-222-3333"))
      (should (equal (plist-get alice :NAME) "Alice Smith")))))

;; ----------------------------------------------------------------------
;; Test person-to-table
;; ----------------------------------------------------------------------
(ert-deftest org-people-person-to-table-test ()
  "Test org-people-person-to-table generates expected table."
  (org-people--with-mocked-people
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
;; Test tags-to-table
;; ----------------------------------------------------------------------
(ert-deftest org-people-tags-to-table-test ()
  "Test org-people-tags-to-table returns correct filtered table."
  (org-people--with-mocked-people
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
  (org-people--with-mocked-people
    (let ((results (org-people-get-by-property :EMAIL ".....@example.com" t)))
      (should (listp results))
      (should (equal (sort (mapcar (lambda (plist) (plist-get plist :NAME)) results))
                     '("Alice Smith" "Carol White")))
      )))

(ert-deftest org-people-get-by-property-literal-test ()
  "Test org-people-get-by-property returns correct filtered table without a regexp"
  ;; literal
  (org-people--with-mocked-people
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

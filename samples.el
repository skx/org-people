;;;;;
;;;
;;; Sample usage
;;;
;;;;;
;;
;; Get all known data about the given contact.
;;
(my/org-contact-get "Steve Kemp")


;;
;; Use the helper functions to get the details
;;
(my/org-contact-email "Steve Kemp")
(my/org-contact-phone "Oiva Adam Kemp")
(my/org-contact-address "Oiva Adam Kemp")


;;
;; Or just get the contact record, and lookup the value "manually".
;;
(plist-get (my/org-contact-get "Steve Kemp") :PHONE)
(plist-get (my/org-contact-get "Steve Kemp") :ADDRESS)
(plist-get (my/org-contact-get "Kirsi Kemp") :ADDRESS)

;;
;; Get the name/phone of each family-member
;;
(let ((people (my/org-contacts '("family")))
      (result))
  (maphash
   (lambda (name plist)
     (let ((phone (plist-get plist :PHONE)))
       (when phone
         (push (list :NAME name :PHONE phone) result))))
   people)
  (nreverse result))

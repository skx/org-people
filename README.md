# org-people

This package allows easy contact-management via org-mode.

The assumption is that you have a single file containing contact-details, stored in a tree:

* The headline of the item is the contact-name.
* The attributes of the people are stored as properties.

Here is an example "contacts.org" file:

```
* People
  ** Alice                        :family:
  :PROPERTIES
  :ADDRESS: 32 Something Street
  :EMAIL: alice@example.com
  :PHONE: +123 456 789
  :CHILDREN: Mallory
  :END
 ** Bob                           :colleague:
  :PROPERTIES
  :ADDRESS: 32 Something Lane
  :EMAIL: bob@example.com
  :PHONE: +123 456 987
  :END
```

Here you see all contacts are stored beneath the "People" header, and we have two entries.

We assume that you have "ADDRESS", "EMAIL", and "PHONE" properties.  There is no restriction
upon the names of the properties, but these are the obvious basic values.



## Adding Entries

If you use `org-capture` you may use the following template to add a new entry:

```
(setq org-capture-templates
  (append org-capture-templates
       '(("p" "People" entry (file+headline "~/Private/Org/PEOPLE.org" "People")
          "* %^{Name}\n:PROPERTIES:\n:EMAIL: %^{Email}\n:PHONE: %^{Phone}\n:END:\n%?"
         :empty-lines 1))))
```



## Working With People

These are the main user-focused functions within the package to work with contacts:

* `org-people-insert`
  * Insert contact-data into the buffer, via interactive prompts (with `completing-read`).
* `org-people-summary`
  * Parse all known contacts and pop to a buffer containing a summary of their details.
* `org-people-tags-to-table`
  * Designed to create auto-updating tables inside `org-mode` documents.

Suggested usage:

```
; insert a contact "thing" at the current point.
(global-set-key (kbd "C-c p") 'org-people-insert)

; show a summary of contacts.
(global-set-key (kbd "C-c P") 'org-people-summary)

; or show the whole file.
(global-set-key (kbd "C-c P") '(lambda() (interactive) (find-file org-people-file)))
```

There are also functions for working with the parsed contacts:

* `org-people-parse`
  * Parse all known contacts and return a hash of them.
  * The key is the person's name, the values is a plist of all known properties.
* `org-people-select-interactively`
  * Prompt the user for a contact name, and return the known data about that contact.
* `org-people-get-by-name`
  * Get data about a contact, by name.
* `org-people-filter`
  * Find matching contacts via arbitrary filter/predicate.



## Configuration

There are two configuration items you might wish to change:

* `org-people-file`
  * The name of the org-file to read entries from.
* `org-people-headline`
  * The headline beneath which contacts are stored.



## Dynamic `org-mode` tables

If you tag the entries within the people hierarchy you can then create org-mode tables of matching entries.

For example the following can auto-update:

    #+NAME: get-colleagues-contacts
    #+BEGIN_SRC elisp :results value table
    (org-people-tags-to-table "family")
    #+END_SRC

If you prefer different columns you can specify them:

    #+NAME: get-colleagues-contacts
    #+BEGIN_SRC elisp :results value table
    (org-people-tags-to-table "family" '(:NAME :PHONE))
    #+END_SRC

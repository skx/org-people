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

We assume that you have "ADDRESS", "EMAIL", and "PHONE" properties, as we define helpers for working with those specific properties, but they are not mandatory and any additional properties you add can be found and worked with.



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

There are several utility functions within the package to work with contacts:

* `org-people`
  * Parse all known contacts and return a hash of them.
  * The key is the person's name, the values is a plist of all known properties.
* `org-peopl-names`
  * Return a list of all known people-names.
* `org-people-select-by-name`
  * Programatically prompt the user for a contact name, with completion.
* `org-people-get-by-name`
  * Get all data, i.e. the plist, associated with a given contact name.

However the main meat of the package is `org-people-insert` which prompts you to enter a contact name, with completion, and then the attribute which you wish to insert.  This allows you to quickly and easily insert the email, phone, or similar for a given contact-person.

Suggested usage:

```
(global-set-key (kbd "C-c p") 'org-people-insert)
```



## Configuration

There are two configuration items you might wish to change:

* `org-people-file`
  * The name of the org-file to read entries from.
* `org-people-headline`
  * The headline beneath which contacts are stored.



## Dynamic `org-mode` tables

If you tag the entries within the people hierarchy you can then create org-mode tables of matching entries.

For example the following can auto-update:

```
#+NAME: get-colleact-contacts
#+BEGIN_SRC elisp :results value table
(cons '("Name" "Phone" "Email") (org-people-by-tag "colleagues"))
#+END_SRC
```

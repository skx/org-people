# org-people

This package allows easy contact-management via the native org-mode facilities.

Contacts are found from any of your org-agenda files, providing they have a tag of "contacts",
for example here is an example "contacts.org" file which has some contacts beneath a "People" tree:

```
* People
  ** Alice                        :family:contact:
  :PROPERTIES
  :ADDRESS: 32 Something Street
  :EMAIL: alice@example.com
  :PHONE: +123 456 789
  :CHILDREN: Mallory
  :END
 ** Bob                           :colleague:contact:
  :PROPERTIES
  :ADDRESS: 32 Something Lane
  :EMAIL: bob@example.com
  :PHONE: +123 456 987
  :END
```

It is assumed that you'll have "ADDRESS", "EMAIL", and "PHONE" other properties, however no properties are mandatory or expected.  You can add an arbitrary number of properties - providing at least one is present, the tag is in-place all will be well.  The headline will be used as the contact name.



## Adding Entries

If you use `org-capture` you may use the following template to add a new entry like so:

```
(setq org-capture-templates
  (append org-capture-templates
       '(("p" "People" entry (file+headline "~/Private/Org/PEOPLE.org" "People")
          "* %^{Name}\n:PROPERTIES:\n:EMAIL: %^{Email}\n:PHONE: %^{Phone}\n:END:\n%?"
         :empty-lines 1))))
```



## API / Functions

These are the main user-focused functions within the package to work with contacts:

* `org-people-insert`
  * Insert contact-data into the buffer, via interactive prompts (with `completing-read`).
* `org-people-summary`
  * Parse all known contacts and pop to a buffer containing a summary of their details.
  * This uses `tabulated-list-mode` and allows you to sort and copy fields, etc.
* `org-people-tags-to-table`
  * Designed to create auto-updating tables inside `org-mode` documents.
* `org-people-person-to-table`
  * Designed to create auto-updating tables inside `org-mode` documents.

Suggested usage:

```
; insert a contact "thing" at the current point.
(global-set-key (kbd "C-c p") 'org-people-insert)

; show a summary of contacts - allow jumping to the definition, copying and filtering, etc.
(global-set-key (kbd "C-c P") 'org-people-summary)
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

No special configuration is required, although if you wish to use a different tag to identify the
contacts you can specify that via `org-people-search-tag`.

If you wished to limit parsing to only a single named file you could set `org-people-search-type` to be a list containing the name(s) of files to process, otherwise all agenda files will be read.



## Limitations

If you have two contacts with the same name one will overwrite the other.  This is annoying, but not a bug.



## org-mode links

This package defines an `org-people:` handler, to jump to your contact-list entries when clicked.

Inside org-mode files just create links using that protocols:

    * This is a headline
    [[org-people:Steve Kemp]] wrote this package.

This will jump to the definition of the named person (me).

When exported to HTML the person name will be made bold, rather than becoming a link, which is probably what you want.

You can add such a link via C-c C-l, as per usual, with TAB-completion support



## Dynamic `org-mode` tables

If you tag the entries within the people hierarchy you can then create org-mode tables of matching entries.
For example the following can auto-update:

    #+NAME: get-family-contacts
    #+BEGIN_SRC elisp :results value table
    (org-people-tags-to-table "family")
    #+END_SRC

If you prefer different columns you can specify them:

    #+NAME: get-family-contacts
    #+BEGIN_SRC elisp :results value table
    (org-people-tags-to-table "family" '(:LINK :PHONE))
    #+END_SRC

You can also include all data about a single named individual, by name:

    #+NAME: steve-kemp
    #+BEGIN_SRC elisp :results value table :colnames '("Field" "Value")
    (org-people-person-to-table "Steve Kemp")
    #+END_SRC

    #+RESULTS: steve-kemp
    | Field       | Value                                   |
    |-------------+-----------------------------------------|
    | Address     | Helsinki, Finland                       |
    | Category    | PEOPLE                                  |
    | Country     | Finland                                 |
    | Email       | steve@steve.fi                          |
    | Name        | Steve Kemp                              |
    | Phone       | +358123456789                           |
    | Tags        | (me)                                    |



## Testing

You can run `make test` via the supplied [Makefile](Makefile) to run the tests in a batch-mode, otherwise load the file [org-people-test.el](org-people-test.el) and run `M-x eval buffer`, you should see the test results in a new buffer.

If any tests fail that's a bug.

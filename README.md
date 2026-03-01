# org-people

This package allows easy contact-management via the native org-mode facilities.

Contacts are found from any of your org-agenda files, providing they have a tag
of "contacts", for example you can see the following simple hierarchy below, or
look at `org-people-test.org` file within this repository which is used by the
test-cases:

```
* People
  ** Alice                        :family:contact:
  :PROPERTIES
  :ADDRESS: 32 Something Street
  :EMAIL: alice@example.com
  :PHONE: +123 456 789
  :CHILDREN: Mallory
  :NICKNAME: Allu
  :END
 ** Bob                           :colleague:contact:
  :PROPERTIES
  :ADDRESS: 32 Something Lane
  :EMAIL: bob@example.com
  :PHONE: +123 456 987
  :END
```

It is assumed that you'll have "ADDRESS", "EMAIL", and "PHONE" other properties, however no properties are mandatory or expected.  The contact will be added providing there is at least one property in-place, and the "contact" tag is present.  The headline itself is used as the contact name.

The special `:NICKNAME:` property will be used for completion, alongside the person's name, if it is present, but this is optional.



## Installation / Configuration Example

The legacy way to install would be to clone this repository and ensure the directory is available upon your load-path, or copy your local lisp tree.

This package _should_ shortly be available upon MELPA.

Suggested usage if you're using the traditional approach:

```
(require 'org-people)

; insert a contact "thing" at the current point.
(global-set-key (kbd "C-c p") 'org-people-insert)

; show a summary/table of contacts
; The table is sortable, filterable, & etc.
(global-set-key (kbd "C-c P") 'org-people-summary)
```

If you prefer `use-package` then this works:

```
(use-package org-people
  :after org
  :bind
    (("C-c p" . org-people-insert)
     ("C-c P" . org-people-summary)))
```



## Adding Entries

If you use `org-capture` you may use the following template to add a new entry:

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
  * Insert contact-data, via interactive prompts (with `completing-read`).
* `org-people-summary`
  * Parse all known contacts and pop to a buffer containing a summary of their details.
  * This uses `tabulated-list-mode` and allows you run a few different actions
    * Sort by the available fields.
    * Copy fields to the clipboard.
    * Export single contacts to VCF format.
    * Filter the view by property values.
* `org-people-tags-to-table`
  * Designed to create auto-updating tables inside `org-mode` documents.
* `org-people-person-to-table`
  * Designed to create auto-updating tables inside `org-mode` documents.


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

No special configuration is required, although if you wish to use a different tag to identify the contacts you may specify that via `org-people-search-tag`.

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

If you tag the contact-entries you can use those tags to create org-mode tables of matching entries.

For example the following can auto-update:

    #+NAME: get-family-contacts
    #+BEGIN_SRC elisp :results value table
    (org-people-tags-to-table "family")
    #+END_SRC

If you prefer different columns to be included within your generated table you can specify them directly:

    #+NAME: get-family-contacts
    #+BEGIN_SRC elisp :results value table
    (org-people-tags-to-table "family" '(:LINK :PHONE))
    #+END_SRC

Rather than handling groups of contacts, via tags, you can also create a table including all data about a single named individual, by name:

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



## Summary View

The `org-people-summary` function shows a table of all your known contacts.

You can customize the displayed fields, or their order, by modifying the `org-people-summary-properties` variable, which has the following defaults:

    (defvar org-people-summary-properties
       '(:NAME :EMAIL :PHONE :TAGS)
       "List of properties to display in `org-people-summary'.")

Some keybindings are setup:

* `RET` goto the definition of the contact
* `c` Copy the field under the point.
* `f` Filter the view, by property.
  * Even properties which are not visible can be used.
  * e.g. ":ADDRESS" "Finland" will show only Finnish residents.
* `v` - Export the contact.



## Testing

You can run `make test` via the supplied [Makefile](Makefile) to run the tests in a batch-mode, otherwise load the file [org-people-test.el](org-people-test.el) and run `M-x eval buffer`, you should see the test results in a new buffer.

If any tests fail that's a bug.

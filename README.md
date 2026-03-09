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

It is assumed that you'll have "ADDRESS", "EMAIL", and "PHONE" other similar properties, however no specific properties are mandatory or expected.  The contact will be recognized providing there is at least one property present, and the "contact" tag is present.  The headline itself is used as the contact name.

The special `:NICKNAME:` property will be used for completion, alongside the person's name, if it is present, but this is optional.



## Installation / Configuration Example

The legacy way to install would be to clone this repository and ensure the directory is available upon your load-path, or copy your local lisp tree.

The package is now available upon MELPA, if you wish to install it from there.

Suggested usage if you're using the traditional approach:

```
(require 'org-people)

; insert a contact "thing" at the current point.
(global-set-key (kbd "C-c p") 'org-people-insert)

; Show a table of all known contacts.  The table is sortable, filterable, & etc.
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

Finally the configuration of the columns has been expanded, in the past we allowed setting the name and column
width like so:

```
(setq org-people-summary-properties
   '((:NAME 30
     (:EMAIL 35))))
```

Now you may add optional configuration to override the column names, the width and even the function which
populates the value.  This allows you to create dynamic values.  For example see the last item here:

```
(setq org-people-summary-properties
      '((:NAME  :width 25)
        (:EMAIL :width 30)
        (:PHONE :width 15 :title "Digits")
        :TAGS
        (:MEOW  :getter (lambda (plist) (concat (plist-get plist :COUNTRY) " [" (plist-get plist :FLAG) "]" )))))
```



## Limitations

If you have two contacts with the same name one will overwrite the other.  This is annoying, but not a bug.



## org-mode links

This package defines a link-handler for the `org-people:` protocol, which will to the definition of the given contact when clicked.   You can add such a link via `C-c C-l`, as expected, and you'll find TAB completion works for populating the protocol-name and the contact's name.

A link would look like this:

    * This is a headline
    [[org-people:Steve Kemp]] wrote this package.

When exported to HTML the person name's will be made bold, rather than becoming a link, which is probably what you want.

The utility function `org-people-add-descriptions` will update all `org-people:` links within the current document to ensure the description matches the link target, which makes the display more readable.



## Dynamic `org-mode` tables

If you tag the contacts with more than just the `contacts` value then you may use those tags to build simple tables of matching entries.  For example the following can auto-update:

    #+NAME: get-family-contacts
    #+BEGIN_SRC elisp :results value table
    (org-people-tags-to-table "family")
    #+END_SRC

If you prefer to include different columns in your generated table you can specify them directly:

    #+NAME: get-family-contacts
    #+BEGIN_SRC elisp :results value table
    (org-people-tags-to-table "family" '(:LINK :PHONE))
    #+END_SRC

You may also create a table including all known data about a single named individual:

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

In this case properties listed in `org-people-ignored-properties` will be ignored and excluded from the generated table.



## Summary View

The `org-people-summary` function shows a table of all your known contacts.

You can customize the displayed fields, or their order, by modifying the `org-people-summary-properties` variable, which defaults to showing the name, email, phone-number and tags associated with each entry.

If a given column would be 100% empty (i.e. no known contacts have a property
with that name) then the column will be removed from display.

Some keybindings are setup in the `org-people-summary-mode-map`:

* `RET` jump to the definition of the contact.
* `c` Copy the field under the point.
* `f` Filter the view, by property.
  * Even properties which are not visible can be used.
  * e.g. ":ADDRESS" "Finland" will show only Finnish residents.
* `R` reset the state of columns.
* `s` Initiate a search forward, via `isearch-forward`.
* `t` Toggle visibility of a named column.
* `T` Hide the current column.
* `v` - Export the contact to a VCF file.



## Testing

You can run `make test` via the supplied [Makefile](Makefile) to run the tests in a batch-mode, otherwise load the file [org-people-test.el](org-people-test.el) and run `M-x eval buffer`, you should see the test results in a new buffer.

If any tests fail that's a bug.

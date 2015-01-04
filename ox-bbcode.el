;;; ox-bbcode.el --- BBCODE Back-End for Org Export Engine

;; Copyright (C) 2012, 2013  Free Software Foundation, Inc.

;; Author: Levin Du <zslevin at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is heavily based on ox-ascii.el written by Nicolas Goaziou
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This library implements an BBCODE back-end for Org generic exporter.
;;
;; It provides two commands for export, depending on the desired
;; output: `org-bbcode-export-as-bbcode' (temporary buffer) and
;; `org-bbcode-export-to-bbcode' ("txt" file).  Also, three publishing
;; functions are available: `org-bbcode-publish-to-bbcode',
;; `org-bbcode-publish-to-latin1' and `org-bbcode-publish-to-utf8'.
;;
;; Output encoding is specified through `org-bbcode-charset' variable,
;; among `bbcode', `latin1' and `utf-8' symbols.
;;
;; By default, horizontal rules span over the full text with, but with
;; a given width attribute (set though #+ATTR_BBCODE: :width <num>)
;; they can be shortened and centered.

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox)
(require 'ox-publish)
(require 'ox-ascii)

(declare-function aa2u "ext:ascii-art-to-unicode" ())

;;; Define Back-End
;;
;; We also install a filter for headlines and sections, in order to
;; control blank lines separating them in output string.

(org-export-define-backend 'bbcode
  '((bold . org-bbcode-bold)
    (center-block . org-bbcode-center-block)
    (clock . org-bbcode-clock)
    (code . org-bbcode-code)
    (comment . (lambda (&rest args) ""))
    (comment-block . (lambda (&rest args) ""))
    (drawer . org-bbcode-drawer)
    (dynamic-block . org-bbcode-dynamic-block)
    (entity . org-bbcode-entity)
    (example-block . org-bbcode-example-block)
    (export-block . org-bbcode-export-block)
    (export-snippet . org-bbcode-export-snippet)
    (fixed-width . org-bbcode-fixed-width)
    (footnote-reference . org-bbcode-footnote-reference)
    (headline . org-bbcode-headline)
    (horizontal-rule . org-bbcode-horizontal-rule)
    (inline-src-block . org-bbcode-inline-src-block)
    (inlinetask . org-bbcode-inlinetask)
    (inner-template . org-bbcode-inner-template)
    (italic . org-bbcode-italic)
    (item . org-bbcode-item)
    (keyword . org-bbcode-keyword)
    (latex-environment . org-bbcode-latex-environment)
    (latex-fragment . org-bbcode-latex-fragment)
    (line-break . org-bbcode-line-break)
    (link . org-bbcode-link)
    (paragraph . org-bbcode-paragraph)
    (plain-list . org-bbcode-plain-list)
    (plain-text . org-bbcode-plain-text)
    (planning . org-bbcode-planning)
    (quote-block . org-bbcode-quote-block)
    (quote-section . org-bbcode-quote-section)
    (radio-target . org-bbcode-radio-target)
    (section . org-bbcode-section)
    (special-block . org-bbcode-special-block)
    (src-block . org-bbcode-src-block)
    (statistics-cookie . org-bbcode-statistics-cookie)
    (strike-through . org-bbcode-strike-through)
    (subscript . org-bbcode-subscript)
    (superscript . org-bbcode-superscript)
    (table . org-bbcode-table)
    (table-cell . org-bbcode-table-cell)
    (table-row . org-bbcode-table-row)
    (target . org-bbcode-target)
    (template . org-bbcode-template)
    (timestamp . org-bbcode-timestamp)
    (underline . org-bbcode-underline)
    (verbatim . org-bbcode-verbatim)
    (verse-block . org-bbcode-verse-block))
  :export-block "BBCODE"
  :menu-entry
  '(?b "Export to BBCODE"
       ((?b "As BBCODE buffer" org-bbcode-export-as-bbcode)
	(?f "As BBCODE file" org-bbcode-export-to-bbcode)
	(?o "As BBCODE file and open"
	    (lambda (a s v b)
	      (if a (org-bbcode-export-to-bbcode t s v b)
		(org-open-file (org-bbcode-export-to-bbcode nil s v b)))))))
  :filters-alist '((:filter-headline . org-bbcode-filter-headline-blank-lines)
		   (:filter-parse-tree org-bbcode-filter-paragraph-spacing
				       org-bbcode-filter-comment-spacing
				       org-bbcode-filter-misc-spacing)
		   (:filter-section . org-bbcode-filter-headline-blank-lines)
		   )
  :options-alist '((:bbcode-charset nil nil 'utf-8)))



;;; User Configurable Variables

(defgroup org-export-bbcode nil
  "Options for exporting Org mode files to BBCODE."
  :tag "Org Export BBCODE"
  :group 'org-export)

(defcustom org-bbcode-text-width 72
  "Maximum width of exported text."
  :group 'org-export-bbcode
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'integer)

(defcustom org-bbcode-text-markup-alist
  '((bold . "[b]%s[/b]")
    (code . "[font=mono]%s[/font]")
    (italic . "[i]%s[/i]")
    (strike-through . "[s]%s[/s]")
    (underline . "[u]%s[/u]")
    (verbatim . "[font=mono]%s[/font]"))
  "Alist of BBCODE expressions to convert text markup.

The key must be a symbol among `bold', `code', `italic',
`strike-through', `underline' and `verbatim'.  The value is
a formatting string to wrap fontified text with.

If no association can be found for a given markup, text will be
returned as-is."
  :group 'org-export-bbcode
  :type '(alist :key-type (symbol :tag "Markup type")
		:value-type (string :tag "Format string"))
  :options '(bold code italic strike-through underline verbatim))

(defcustom org-bbcode-headline-spacing '(1 . 2)
  "Number of blank lines inserted around headlines.

This variable can be set to a cons cell.  In that case, its car
represents the number of blank lines present before headline
contents whereas its cdr reflects the number of blank lines after
contents.

A nil value replicates the number of blank lines found in the
original Org buffer at the same place."
  :group 'org-export-bbcode
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Replicate original spacing" nil)
	  (cons :tag "Set an uniform spacing"
		(integer :tag "Number of blank lines before contents")
		(integer :tag "Number of blank lines after contents"))))

(defcustom org-bbcode-indented-line-width 'auto
  "Additional indentation width for the first line in a paragraph.
If the value is an integer, indent the first line of each
paragraph by this number.  If it is the symbol `auto' preserve
indentation from original document."
  :group 'org-export-bbcode
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (integer :tag "Number of white spaces characters")
	  (const :tag "Preserve original width" auto)))

(defcustom org-bbcode-paragraph-spacing 'auto
  "Number of white lines between paragraphs.
If the value is an integer, add this number of blank lines
between contiguous paragraphs.  If is it the symbol `auto', keep
the same number of blank lines as in the original document."
  :group 'org-export-bbcode
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (integer :tag "Number of blank lines")
	  (const :tag "Preserve original spacing" auto)))

(defcustom org-bbcode-format-drawer-function nil
  "Function called to format a drawer in BBCODE.

The function must accept three parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.
  WIDTH     the text width within the drawer.

The function should return either the string to be exported or
nil to ignore the drawer.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-bbcode-format-drawer-default (name contents width)
  \"Format a drawer element for BBCODE export.\"
  contents)"
  :group 'org-export-bbcode
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'function)

(defcustom org-bbcode-format-inlinetask-function nil
  "Function called to format an inlinetask in BBCODE.

The function must accept six parameters:
  TODO      the todo keyword, as a string
  TODO-TYPE the todo type, a symbol among `todo', `done' and nil.
  PRIORITY  the inlinetask priority, as a string
  NAME      the inlinetask name, as a string.
  TAGS      the inlinetask tags, as a list of strings.
  CONTENTS  the contents of the inlinetask, as a string.

The function should return either the string to be exported or
nil to ignore the inline task."
  :group 'org-export-bbcode
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'function)



;;; Internal Functions

(defun org-bbcode--build-title
  (element info text-width &optional underline notags toc)
  "Format ELEMENT title and return it.

ELEMENT is either an `headline' or `inlinetask' element.  INFO is
a plist used as a communication channel.  TEXT-WIDTH is an
integer representing the maximum length of a line.

When optional argument UNDERLINE is non-nil, underline title,
without the tags, according to `org-bbcode-underline'
specifications.

If optional argument NOTAGS is non-nil, no tags will be added to
the title.

When optional argument TOC is non-nil, use optional title if
possible.  It doesn't apply to `inlinetask' elements."
  (let* ((headlinep (eq (org-element-type element) 'headline))
	 (level (org-export-get-relative-level element info))
	 (font-size (if (> level 3) 4 (- 7 level)))
	 (numbers
	  ;; Numbering is specific to headlines.
	  (and headlinep (org-export-numbered-headline-p element info)
	       ;; All tests passed: build numbering string.
	       (concat
		(mapconcat
		 'number-to-string
		 (org-export-get-headline-number element info) ".")
		" ")))
	 (text
	  (org-trim
	   (org-export-data
	    (if (and toc headlinep) (org-export-get-alt-title element info)
	      (org-element-property :title element))
	    info)))
	 (todo
	  (and (plist-get info :with-todo-keywords)
	       (let ((todo (org-element-property :todo-keyword element)))
		 (and todo (concat (org-export-data todo info) " ")))))
	 (tags (and (not notags)
		    (plist-get info :with-tags)
		    (let ((tag-list (org-export-get-tags element info)))
		      (and tag-list
			   (format ":%s:"
				   (mapconcat 'identity tag-list ":"))))))
	 (priority
	  (and (plist-get info :with-priority)
	       (let ((char (org-element-property :priority element)))
		 (and char (format "(#%c) " char)))))
	 (first-part (concat (if toc "[b]" (format "[size=%d][b]" font-size))
			     numbers todo priority text
			     (if toc "[/b]" "[/b][/size]"))))
    (concat
     first-part
     ;; Align tags, if any.
     (when tags
       (format
	(format " %%%ds"
		(max (- text-width  (1+ (length first-part))) (length tags)))
	tags))
     )))

(defun org-bbcode--has-caption-p (element info)
  "Non-nil when ELEMENT has a caption affiliated keyword.
INFO is a plist used as a communication channel.  This function
is meant to be used as a predicate for `org-export-get-ordinal'."
  (org-element-property :caption element))

(defun org-bbcode--build-caption (element info)
  "Return caption string for ELEMENT, if applicable.

INFO is a plist used as a communication channel.

The caption string contains the sequence number of ELEMENT along
with its real caption.  Return nil when ELEMENT has no affiliated
caption keyword."
  (let ((caption (org-export-get-caption element)))
    (when caption
      ;; Get sequence number of current src-block among every
      ;; src-block with a caption.
      (let ((reference
	     (org-export-get-ordinal
	      element info nil 'org-bbcode--has-caption-p))
	    (title-fmt (org-bbcode--translate
			(case (org-element-type element)
			  (table "Table %d:")
			  (src-block "Listing %d:"))
			info)))
	(org-ascii--fill-string
	 (concat (format title-fmt reference)
		 " "
		 (org-export-data caption info))
	 (org-ascii--current-text-width element info) info)))))

(defun org-bbcode--build-toc (info &optional n keyword)
  "Return a table of contents.

INFO is a plist used as a communication channel.

Optional argument N, when non-nil, is an integer specifying the
depth of the table.

Optional argument KEYWORD specifies the TOC keyword, if any, from
which the table of contents generation has been initiated."
  (let ((title (org-bbcode--translate "Table of Contents" info)))
    (concat
     title "\n"
     (make-string (length title)
		  (if (eq (plist-get info :bbcode-charset) 'utf-8) ?─ ?_))
     "\n"
     (let ((text-width
	    (if keyword (org-ascii--current-text-width keyword info)
	      org-bbcode-text-width)))
       (mapconcat
	(lambda (headline)
	  (let* ((level (org-export-get-relative-level headline info))
		 (indent (* (1- level) 3)))
	    (concat
	     (unless (zerop indent) (concat (make-string (1- indent) ?.) " "))
	     (org-bbcode--build-title
	      headline info (- text-width indent) nil
	      (or (not (plist-get info :with-tags))
		  (eq (plist-get info :with-tags) 'not-in-toc))
	      'toc))))
	(org-export-collect-headlines info n) "\n")))))

(defun org-bbcode--list-listings (keyword info)
  "Return a list of listings.

KEYWORD is the keyword that initiated the list of listings
generation.  INFO is a plist used as a communication channel."
  (let ((title (org-bbcode--translate "List of Listings" info)))
    (concat
     title "\n"
     (make-string (length title)
		  (if (eq (plist-get info :bbcode-charset) 'utf-8) ?─ ?_))
     "\n\n"
     (let ((text-width
	    (if keyword (org-ascii--current-text-width keyword info)
	      org-bbcode-text-width ))
	   ;; Use a counter instead of retreiving ordinal of each
	   ;; src-block.
	   (count 0))
       (mapconcat
	(lambda (src-block)
	  ;; Store initial text so its length can be computed.  This is
	  ;; used to properly align caption right to it in case of
	  ;; filling (like contents of a description list item).
	  (let ((initial-text
		 (format (org-bbcode--translate "Listing %d:" info)
			 (incf count))))
	    (concat
	     initial-text " "
	     (org-trim
	      (org-ascii--indent-string
	       (org-ascii--fill-string
		;; Use short name in priority, if available.
		(let ((caption (or (org-export-get-caption src-block t)
				   (org-export-get-caption src-block))))
		  (org-export-data caption info))
		(- text-width (length initial-text)) info)
	       (length initial-text))))))
	(org-export-collect-listings info) "\n")))))

(defun org-bbcode--list-tables (keyword info)
  "Return a list of tables.

KEYWORD is the keyword that initiated the list of tables
generation.  INFO is a plist used as a communication channel."
  (let ((title (org-bbcode--translate "List of Tables" info)))
    (concat
     title "\n"
     (make-string (length title)
		  (if (eq (plist-get info :bbcode-charset) 'utf-8) ?─ ?_))
     "\n\n"
     (let ((text-width
	    (if keyword (org-ascii--current-text-width keyword info)
	      org-bbcode-text-width ))
	   ;; Use a counter instead of retreiving ordinal of each
	   ;; src-block.
	   (count 0))
       (mapconcat
	(lambda (table)
	  ;; Store initial text so its length can be computed.  This is
	  ;; used to properly align caption right to it in case of
	  ;; filling (like contents of a description list item).
	  (let ((initial-text
		 (format (org-bbcode--translate "Table %d:" info)
			 (incf count))))
	    (concat
	     initial-text " "
	     (org-trim
	      (org-ascii--indent-string
	       (org-ascii--fill-string
		;; Use short name in priority, if available.
		(let ((caption (or (org-export-get-caption table t)
				   (org-export-get-caption table))))
		  (org-export-data caption info))
		(- text-width (length initial-text)) info)
	       (length initial-text))))))
	(org-export-collect-tables info) "\n")))))

(defun org-bbcode--unique-links (element info)
  "Return a list of unique link references in ELEMENT.

ELEMENT is either a headline element or a section element.  INFO
is a plist used as a communication channel."
  (let* (seen
	 (unique-link-p
	  (function
	   ;; Return LINK if it wasn't referenced so far, or nil.
	   ;; Update SEEN links along the way.
	   (lambda (link)
	     (let ((footprint
		    (cons (org-element-property :raw-link link)
			  (org-element-contents link))))
	       ;; Ignore LINK if it hasn't been translated already.
	       ;; It can happen if it is located in an affiliated
	       ;; keyword that was ignored.
	       (when (and (org-string-nw-p
			   (gethash link (plist-get info :exported-data)))
			  (not (member footprint seen)))
		 (push footprint seen) link)))))
	 ;; If at a section, find parent headline, if any, in order to
	 ;; count links that might be in the title.
	 (headline
	  (if (eq (org-element-type element) 'headline) element
	    (or (org-export-get-parent-headline element) element))))
    ;; Get all links in HEADLINE.
    (org-element-map headline 'link
      (lambda (l) (funcall unique-link-p l)) info nil nil t)))

(defun org-bbcode--describe-links (links width info)
  "Return a string describing a list of links.

LINKS is a list of link type objects, as returned by
`org-bbcode--unique-links'.  WIDTH is the text width allowed for
the output string.  INFO is a plist used as a communication
channel."
  (mapconcat
   (lambda (link)
     (let ((type (org-element-property :type link))
	   (anchor (let ((desc (org-element-contents link)))
		     (if desc (org-export-data desc info)
		       (org-element-property :raw-link link)))))
       (cond
	;; Coderefs, radio links and fuzzy links are ignored.
	((member type '("coderef" "radio" "fuzzy")) nil)
	;; Id and custom-id links: Headlines refer to their numbering.
	((member type '("custom-id" "id"))
	 (let ((dest (org-export-resolve-id-link link info)))
	   (concat
	    (org-ascii--fill-string
	     (format
	      "[%s] %s"
	      anchor
	      (if (not dest) (org-bbcode--translate "Unknown reference" info)
		(format
		 (org-bbcode--translate "See section %s" info)
		 (mapconcat 'number-to-string
			    (org-export-get-headline-number dest info) "."))))
	     width info) "\n\n")))
	;; Do not add a link that cannot be resolved and doesn't have
	;; any description: destination is already visible in the
	;; paragraph.
	((not (org-element-contents link)) nil)
	(t
	 (concat
	  (org-ascii--fill-string
	   (format "[%s] %s" anchor (org-element-property :raw-link link))
	   width info)
	  "\n\n")))))
   links ""))

(defun org-bbcode--checkbox (item info)
  "Return checkbox string for ITEM or nil.
INFO is a plist used as a communication channel."
  (case (org-element-property :checkbox item)
    (on "[X] ")
    (off "[ ] ")
    (trans "[-] ")))

;;;; Plain Text

(defcustom org-bbcode-protect-char-alist
  '(("&" . "&amp;")
    ("<" . "&lt;")
    (">" . "&gt;"))
  "Alist of characters to be converted by `org-bbcode-protect'."
  :group 'org-export-bbcode
  :type '(repeat (cons (string :tag "Character")
		       (string :tag "BBCODE equivalent"))))

(defun org-bbcode-encode-plain-text (text)
  "Convert plain text characters from TEXT to BBCODE equivalent.
Possible conversions are set in `org-bbcode-protect-char-alist'."
  (mapc
   (lambda (pair)
     (setq text (replace-regexp-in-string (car pair) (cdr pair) text t t)))
   org-bbcode-protect-char-alist)
  text)


;;; Template

(defun org-bbcode-template--document-title (info)
  "Return document title, as a string.
INFO is a plist used as a communication channel."
  (let* ((text-width org-bbcode-text-width)
	 ;; Links in the title will not be resolved later, so we make
	 ;; sure their path is located right after them.
	 (org-bbcode-links-to-notes nil)
	 (title (org-export-data (plist-get info :title) info))
	 (author (and (plist-get info :with-author)
		      (let ((auth (plist-get info :author)))
			(and auth (org-export-data auth info)))))
	 (email (and (plist-get info :with-email)
		     (org-export-data (plist-get info :email) info)))
	 (date (and (plist-get info :with-date)
		    (org-export-data (org-export-get-date info) info))))
    ;; There are two types of title blocks depending on the presence
    ;; of a title to display.
    (if (string= title "")
	;; Title block without a title.  DATE is positioned at the top
	;; right of the document, AUTHOR to the top left and EMAIL
	;; just below.
	(cond
	 ((and (org-string-nw-p date) (org-string-nw-p author))
	  (concat
	   author
	   (make-string (- text-width (string-width date) (string-width author)) ? )
	   date
	   (when (org-string-nw-p email) (concat "\n" email))
	   "\n\n\n"))
	 ((and (org-string-nw-p date) (org-string-nw-p email))
	  (concat
	   email
	   (make-string (- text-width (string-width date) (string-width email)) ? )
	   date "\n\n\n"))
	 ((org-string-nw-p date)
	  (concat
	   (org-ascii--justify-string date text-width 'right)
	   "\n\n\n"))
	 ((and (org-string-nw-p author) (org-string-nw-p email))
	  (concat author "\n" email "\n\n\n"))
	 ((org-string-nw-p author) (concat author "\n\n\n"))
	 ((org-string-nw-p email) (concat email "\n\n\n")))
      ;; Title block with a title.  Document's TITLE, along with the
      ;; AUTHOR and its EMAIL are both overlined and an underlined,
      ;; centered.  Date is just below, also centered.
      (let* ((utf8p (eq (plist-get info :bbcode-charset) 'utf-8))
	     ;; Format TITLE.  It may be filled if it is too wide,
	     ;; that is wider than the two thirds of the total width.
	     (title-len (min (string-width title) (/ (* 2 text-width) 3)))
	     (formatted-title (org-ascii--fill-string title title-len info))
	     )
	(concat
	 (org-bbcode-center
	  (org-bbcode-font-size (upcase title) 6)
	  "\n")
	 (cond
	  ((and (org-string-nw-p author) (org-string-nw-p email))
	   (org-bbcode-center (concat author " <" email + ">") "\n"))
	  ((org-string-nw-p author)
	   (org-bbcode-center author "\n"))
	  ((org-string-nw-p email)
	   (org-bbcode-center email "\n"))
	  (when (org-string-nw-p date)
	    (org-bbcode-center date "\n"))
	  "\n\n"))))))

(defun org-bbcode-inner-template (contents info)
  "Return body of document string after BBCODE conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (org-element-normalize-string
   (org-ascii--indent-string
    (concat
     ;; 1. Document's body.
     contents
     ;; 2. Footnote definitions.
     (let ((definitions (org-export-collect-footnote-definitions
			 (plist-get info :parse-tree) info))
	   ;; Insert full links right inside the footnote definition
	   ;; as they have no chance to be inserted later.
	   (org-bbcode-links-to-notes nil))
       (when definitions
	 (concat
	  "\n\n\n"
	  (let ((title (org-bbcode--translate "Footnotes" info)))
	    (concat
	     title "\n"
	     (make-string
	      (length title)
	      (if (eq (plist-get info :bbcode-charset) 'utf-8) ?─ ?_))))
	  "\n\n"
	  (let ((text-width org-bbcode-text-width ))
	    (mapconcat
	     (lambda (ref)
	       (let ((id (format "[%s] " (car ref))))
		 ;; Distinguish between inline definitions and
		 ;; full-fledged definitions.
		 (org-trim
		  (let ((def (nth 2 ref)))
		    (if (eq (org-element-type def) 'org-data)
			;; Full-fledged definition: footnote ID is
			;; inserted inside the first parsed paragraph
			;; (FIRST), if any, to be sure filling will
			;; take it into consideration.
			(let ((first (car (org-element-contents def))))
			  (if (not (eq (org-element-type first) 'paragraph))
			      (concat id "\n" (org-export-data def info))
			    (push id (nthcdr 2 first))
			    (org-export-data def info)))
		      ;; Fill paragraph once footnote ID is inserted
		      ;; in order to have a correct length for first
		      ;; line.
		      (org-ascii--fill-string
		       (concat id (org-export-data def info))
		       text-width info))))))
	     definitions "\n\n\n"))))))
    0)))

(defun org-bbcode-template (contents info)
  "Return complete document string after BBCODE conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; 1. Build title block.
   (concat (org-bbcode-template--document-title info)
	    ;; 2. Table of contents.
	    (let ((depth (plist-get info :with-toc)))
	      (when depth
		(concat
		 (org-bbcode--build-toc info (and (wholenump depth) depth))
		 "\n\n"))))
   ;; 3. Document's body.
   contents
   ;; 4. Creator.  Ignore `comment' value as there are no comments in
   ;;    BBCODE.  Justify it to the bottom right.
   (org-ascii--indent-string
    (let ((creator-info (plist-get info :with-creator))
	  (text-width org-bbcode-text-width))
      (unless (or (not creator-info) (eq creator-info 'comment))
	(concat
	 "\n\n\n"
	 (org-ascii--fill-string
	  (plist-get info :creator) text-width info 'right))))
    5)))

(defun org-bbcode--translate (s info)
  "Translate string S according to specified language and charset.
INFO is a plist used as a communication channel."
  (org-export-translate s :utf-8 info))



;;; Transcode Functions

;;;; Bold

(defun org-bbcode-bold (bold contents info)
  "Transcode BOLD from Org to BBCODE.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format (or (cdr (assq 'bold org-bbcode-text-markup-alist)) "%s")
	  contents))

;;;; Center Block

(defun org-bbcode-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to BBCODE.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-bbcode-center (contents)))

(defun org-bbcode-center (contents &optional tail)
  (format "[align=center]%s[/align]%s" contents (or tail "")))

(defun org-bbcode-font-size (contents font-size)
  (format "[size=%d]%s[/size]" font-size contents))

(defun org-bbcode-color (contents color)
  (format "[color=%s]%s[/color]" color contents))

;;;; Clock

(defun org-bbcode-clock (clock contents info)
  "Transcode a CLOCK object from Org to BBCODE.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-ascii-clock clock contents info))

;;;; Code

(defun org-bbcode-code (code contents info)
  "Return a CODE object from Org to BBCODE.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format (or (cdr (assq 'code org-bbcode-text-markup-alist)) "%s")
	  (org-element-property :value code)))


;;;; Drawer

(defun org-bbcode-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to BBCODE.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((name (org-element-property :drawer-name drawer))
	(width (org-ascii--current-text-width drawer info)))
    (if (functionp org-bbcode-format-drawer-function)
	(funcall org-bbcode-format-drawer-function name contents width)
      ;; If there's no user defined function: simply
      ;; display contents of the drawer.
      contents)))


;;;; Dynamic Block

(defun org-bbcode-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to BBCODE.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  contents)


;;;; Entity

(defun org-bbcode-entity (entity contents info)
  "Transcode an ENTITY object from Org to BBCODE.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (org-element-property
   :utf-8
   entity))


;;;; Example Block

(defun org-bbcode-example-block (example-block contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to BBCODE.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-ascii--box-string
   (org-export-format-code-default example-block info) info))


;;;; Export Snippet

(defun org-bbcode-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to BBCODE.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'bbcode)
    (org-element-property :value export-snippet)))


;;;; Export Block

(defun org-bbcode-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to BBCODE.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "BBCODE")
    (org-remove-indentation (org-element-property :value export-block))))


;;;; Fixed Width

(defun org-bbcode-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to BBCODE.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format (or (cdr (assq 'code org-bbcode-text-markup-alist)) "%s")
	  (org-remove-indentation
	   (org-element-property :value fixed-width)) info))


;;;; Footnote Definition

;; Footnote Definitions are ignored.  They are compiled at the end of
;; the document, by `org-bbcode-inner-template'.


;;;; Footnote Reference

(defun org-bbcode-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to BBCODE.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "<%s>" (org-export-get-footnote-number footnote-reference info)))


;;;; Headline

(defun org-bbcode-headline (headline contents info)
  "Transcode a HEADLINE element from Org to BBCODE.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  ;; Don't export footnote section, which will be handled at the end
  ;; of the template.
  (unless (org-element-property :footnote-section-p headline)
    (let* ((low-level-rank (org-export-low-level-p headline info))
	   (width (org-ascii--current-text-width headline info))
	   ;; Blank lines between headline and its contents.
	   ;; `org-bbcode-headline-spacing', when set, overwrites
	   ;; original buffer's spacing.
	   (pre-blanks
	    (make-string
	     (if org-bbcode-headline-spacing (car org-bbcode-headline-spacing)
	       (org-element-property :pre-blank headline)) ?\n))
	   ;; Even if HEADLINE has no section, there might be some
	   ;; links in its title that we shouldn't forget to describe.
	   (links
	    (unless (or (eq (caar (org-element-contents headline)) 'section))
	      (let ((title (org-element-property :title headline)))
		(when (consp title)
		  (org-bbcode--describe-links
		   (org-bbcode--unique-links title info) width info))))))
      ;; Deep subtree: export it as a list item.
      (if low-level-rank
	  (concat
	   ;; Bullet.
	   (let ((bullets '(?§ ?¶ ?◊)))
	     (char-to-string
	      (nth (mod (1- low-level-rank) (length bullets)) bullets)))
	   " "
	   ;; Title.
	   (org-bbcode--build-title headline info width)
	   "\n"
	   ;; Contents, indented by length of bullet.
	   pre-blanks
	   (org-ascii--indent-string
	    (concat contents
		    (when (org-string-nw-p links) (concat "\n\n" links)))
	    2))
	;; Else: Standard headline.
	(concat
	 (org-bbcode--build-title headline info width 'underline)
	 "\n" pre-blanks
	 (concat (when (org-string-nw-p links) links) contents))))))

;;;; Src Code

(defun org-bbcode-fontify-code (code lang)
  "Color CODE with bbcodeize library.
CODE is a string representing the source code to colorize.  LANG
is the language used for CODE, as a string, or nil."
  (when code
    (cond
     ;; Case 1: No lang.  Possibly an example block.
     ((not lang)
      ;; Simple transcoding.
      (org-bbcode-encode-plain-text code))
     ;; Case 2: No htmlize or an inferior version of htmlize
     ((not (and (require 'bbcodeize nil t) (fboundp 'bbcodeize-region-for-paste)))
      ;; Emit a warning.
      (message "Cannot fontify src block (bbcodeize required)")
      ;; Simple transcoding.
      (org-bbcode-encode-plain-text code))
     (t
      ;; Map language
      (setq lang (or (assoc-default lang org-src-lang-modes) lang))
      (let* ((lang-mode (and lang (intern (format "%s-mode" lang)))))
	(cond
	 ;; Case 1: Language is not associated with any Emacs mode
	 ((not (functionp lang-mode))
	  ;; Simple transcoding.
	  (org-bbcode-encode-plain-text code))
	 ;; Case 2: Default.  Fontify code.
	 (t
	  ;; bbcodeize
	  (setq code (with-temp-buffer
		       ;; Switch to language-specific mode.
		       (funcall lang-mode)
		       (insert code)
		       ;; Fontify buffer.
		       (font-lock-fontify-buffer)
		       ;; Remove formatting on newline characters.
		       (save-excursion
			 (let ((beg (point-min))
			       (end (point-max)))
			   (goto-char beg)
			   (while (progn (end-of-line) (< (point) end))
			     (put-text-property (point) (1+ (point)) 'face nil)
			     (forward-char 1))))
		       (org-src-mode)
		       (set-buffer-modified-p nil)
		       ;; bbcodeize region.
		       (bbcodeize-region-for-paste
			(point-min) (point-max)))))))))))

(defun org-bbcode-do-format-code
  (code &optional lang refs retain-labels num-start)
  "Format CODE string as source code.
Optional arguments LANG, REFS, RETAIN-LABELS and NUM-START are,
respectively, the language of the source code, as a string, an
alist between line numbers and references (as returned by
`org-export-unravel-code'), a boolean specifying if labels should
appear in the source code, and the number associated to the first
line of code."
  (let* ((code-lines (org-split-string code "\n"))
	 (code-length (length code-lines))
	 (num-fmt
	  (and num-start
	       (format "%%%ds: "
		       (length (number-to-string (+ code-length num-start))))))
	 (code (org-bbcode-fontify-code code lang)))
    (concat "[align=center][table=90%,LemonChiffon][tr][td]"
	    (org-export-format-code
	     code
	     (lambda (loc line-num ref)
	       (setq loc
		     (concat
		      ;; Add line number, if needed.
		      (when num-start
			(format "%s"
				(format num-fmt line-num)))
		      ;; Transcoded src line.
		      loc
		      ;; Add label, if needed.
		      (when (and ref retain-labels) (format " (%s)" ref)))))
	     num-start refs)
	    "[/td][/tr][/table][/align]\n")))

(defun org-bbcode-format-code (element info)
  "Format contents of ELEMENT as source code.
ELEMENT is either an example block or a src block.  INFO is
a plist used as a communication channel."
  (let* ((lang (org-element-property :language element))
	 ;; Extract code and references.
	 (code-info (org-export-unravel-code element))
	 (code (car code-info))
	 (refs (cdr code-info))
	 ;; Does the src block contain labels?
	 (retain-labels (org-element-property :retain-labels element))
	 ;; Does it have line numbers?
	 (num-start (case (org-element-property :number-lines element)
		      (continued (org-export-get-loc element info))
		      (new 0))))
    (org-bbcode-do-format-code code lang refs retain-labels num-start)))

;;;; Horizontal Rule

(defun org-bbcode-horizontal-rule (horizontal-rule contents info)
  "Transcode an HORIZONTAL-RULE object from Org to BBCODE.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((text-width (org-ascii--current-text-width horizontal-rule info))
	(spec-width
	 (org-export-read-attribute :attr_bbcode horizontal-rule :width)))
    (org-ascii--justify-string
     (make-string (if (and spec-width (string-match "^[0-9]+$" spec-width))
		      (string-to-number spec-width)
		    text-width)
		  (if (eq (plist-get info :bbcode-charset) 'utf-8) ?― ?-))
     text-width 'center)))


;;;; Inline Src Block

(defun org-bbcode-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to BBCODE.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (format org-bbcode-verbatim-format
	  (org-element-property :value inline-src-block)))


;;;; Inlinetask

(defun org-bbcode-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to BBCODE.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((width (org-ascii--current-text-width inlinetask info)))
    ;; If `org-bbcode-format-inlinetask-function' is provided, call it
    ;; with appropriate arguments.
    (if (functionp org-bbcode-format-inlinetask-function)
	(funcall org-bbcode-format-inlinetask-function
		 ;; todo.
		 (and (plist-get info :with-todo-keywords)
		      (let ((todo (org-element-property
				   :todo-keyword inlinetask)))
			(and todo (org-export-data todo info))))
		 ;; todo-type
		 (org-element-property :todo-type inlinetask)
		 ;; priority
		 (and (plist-get info :with-priority)
		      (org-element-property :priority inlinetask))
		 ;; title
		 (org-export-data (org-element-property :title inlinetask) info)
		 ;; tags
		 (and (plist-get info :with-tags)
		      (org-element-property :tags inlinetask))
		 ;; contents and width
		 contents width)
      ;; Otherwise, use a default template.
      (let* ((utf8p (eq (plist-get info :bbcode-charset) 'utf-8)))
	(org-ascii--indent-string
	 (concat
	  ;; Top line, with an additional blank line if not in UTF-8.
	  (make-string width (if utf8p ?━ ?_))  "\n"
	  (unless utf8p (concat (make-string width ? ) "\n"))
	  ;; Add title.  Fill it if wider than inlinetask.
	  (let ((title (org-bbcode--build-title inlinetask info width)))
	    (if (<= (length title) width) title
	      (org-ascii--fill-string title width info)))
	  "\n"
	  ;; If CONTENTS is not empty, insert it along with
	  ;; a separator.
	  (when (org-string-nw-p contents)
	    (concat (make-string width (if utf8p ?─ ?-)) "\n" contents))
	  ;; Bottom line.
	  (make-string width (if utf8p ?━ ?_)))
	 ;; Flush the inlinetask to the right.
	 3)))))


;;;; Italic

(defun org-bbcode-italic (italic contents info)
  "Transcode italic from Org to BBCODE.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format (or (cdr (assq 'italic org-bbcode-text-markup-alist)) "%s") contents))

;;;; Item
(defun org-bbcode-checkbox (checkbox)
  "Format CHECKBOX into BBCODE."
  (case checkbox (on "[X]>")
	(off "[&#xa0;]")
	(trans "[-]")
	(t "")))

(defun org-bbcode-format-list-item (contents type checkbox info
					     &optional term-counter-id
					     headline)
  "Format a list item into BBCODE."
  (let ((checkbox (concat (org-bbcode-checkbox checkbox) (and checkbox " "))))
    (concat "[*]" checkbox contents)))

(defun org-bbcode-item (item contents info)
  "Transcode an ITEM element from Org to BBCODE.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((plain-list (org-export-get-parent item))
	 (type (org-element-property :type plain-list))
	 (counter (org-element-property :counter item))
	 (checkbox (org-element-property :checkbox item))
	 (tag (let ((tag (org-element-property :tag item)))
		(and tag (org-export-data tag info)))))
    (org-bbcode-format-list-item
     contents type checkbox info (or tag counter))))


;;;; Keyword

(defun org-bbcode-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to BBCODE.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ((string= key "BBCODE") value)
     ((string= key "TOC")
      (let ((value (downcase value)))
	(cond
	 ((string-match "\\<headlines\\>" value)
	  (let ((depth (or (and (string-match "[0-9]+" value)
				(string-to-number (match-string 0 value)))
			   (plist-get info :with-toc))))
	    (org-bbcode--build-toc
	     info (and (wholenump depth) depth) keyword)))
	 ((string= "tables" value)
	  (org-bbcode--list-tables keyword info))
	 ((string= "listings" value)
	  (org-bbcode--list-listings keyword info))))))))


;;;; Latex Environment

(defun org-bbcode-latex-environment (latex-environment contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to BBCODE.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (when (plist-get info :with-latex)
    (org-remove-indentation (org-element-property :value latex-environment))))


;;;; Latex Fragment

(defun org-bbcode-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to BBCODE.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (when (plist-get info :with-latex)
    (org-element-property :value latex-fragment)))


;;;; Line Break

(defun org-bbcode-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to BBCODE.
CONTENTS is nil.  INFO is a plist holding contextual
  information."  hard-newline)


;;;; Link

(defun org-bbcode-link (link desc info)
  "Transcode a LINK object from Org to BBCODE.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information."
  (let ((raw-link (org-element-property :raw-link link))
	(type (org-element-property :type link)))
    (cond
     ((string= type "coderef")
      (let ((ref (org-element-property :path link)))
	(format (org-export-get-coderef-format ref desc)
		(org-export-resolve-coderef ref info))))
     ;; Do not apply a special syntax on radio links.  Though, use
     ;; transcoded target's contents as output.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(when destination
	  (org-export-data (org-element-contents destination) info))))
     ;; Do not apply a special syntax on fuzzy links pointing to
     ;; targets.
     ((string= type "fuzzy")
      (let ((destination (org-export-resolve-fuzzy-link link info)))
	(if (org-string-nw-p desc) desc
	  (when destination
	    (let ((number
		   (org-export-get-ordinal
		    destination info nil 'org-bbcode--has-caption-p)))
	      (when number
		(if (atom number) (number-to-string number)
		  (mapconcat 'number-to-string number "."))))))))
     (t
      (if (not (org-string-nw-p desc)) (format "[%s]" raw-link)
	(concat
	 (format "[%s]" desc)
	 (unless org-bbcode-links-to-notes (format " (%s)" raw-link))))))))


;;;; Paragraph

(defun org-bbcode-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to BBCODE.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (replace-regexp-in-string "\\(?:\n[ \t]*\\)*\\'" "" contents))

;;;; Plain List
(defun org-bbcode-begin-plain-list (type &optional arg1)
  "Insert the beginning of the BBCODE list depending on TYPE.
When ARG1 is a string, use it as the start parameter for ordered
lists."
  (case type
    (ordered
     (format "[list=%s]" (or arg1 1)))
    (unordered "[list]")
    (descriptive "[list]")))

(defun org-bbcode-end-plain-list (type)
  "Insert the end of the BBCODE list depending on TYPE."
  (case type
    (ordered "[/list]")
    (unordered "[/list]")
    (descriptive "[/list]")))

(defun org-bbcode-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to BBCODE.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* (arg1 ;; (assoc :counter (org-element-map plain-list 'item
	 (type (org-element-property :type plain-list)))
    (format "%s\n%s%s"
	    (org-bbcode-begin-plain-list type)
	    contents (org-bbcode-end-plain-list type))))


;;;; Plain Text

(defun org-bbcode-encode-plain-text (text)
  "Convert plain text characters from TEXT to BBCODE equivalent.
Possible conversions are set in `org-bbcode-protect-char-alist'."
  (mapc
   (lambda (pair)
     (setq text (replace-regexp-in-string (car pair) (cdr pair) text t t)))
   org-bbcode-protect-char-alist)
  text)

(defun org-bbcode-plain-text (text info)
  "Transcode a TEXT string from Org to BBCODE.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (let ((output text))
    ;; Protect following characters: <, >, &.
    (setq output (org-bbcode-encode-plain-text output))
    ;; Handle smart quotes.  Be sure to provide original string since
    ;; OUTPUT may have been modified.
    (when (plist-get info :with-smart-quotes)
      (setq output (org-export-activate-smart-quotes output :bbcode info text)))
    ;; Handle break preservation if required.
    (when (plist-get info :preserve-breaks)
      (setq output
	    (replace-regexp-in-string
	     "\\(\\\\\\\\\\)?[ \t]*\n"
	     (concat (org-bbcode-close-tag "br" nil info) "\n") output)))
    ;; Return value.
    output))


;;;; Planning

(defun org-bbcode-planning (planning contents info)
  "Transcode a PLANNING element from Org to BBCODE.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (mapconcat
   'identity
   (delq nil
	 (list (let ((closed (org-element-property :closed planning)))
		 (when closed
		   (concat org-closed-string " "
			   (org-translate-time
			    (org-element-property :raw-value closed)))))
	       (let ((deadline (org-element-property :deadline planning)))
		 (when deadline
		   (concat org-deadline-string " "
			   (org-translate-time
			    (org-element-property :raw-value deadline)))))
	       (let ((scheduled (org-element-property :scheduled planning)))
		 (when scheduled
		   (concat org-scheduled-string " "
			   (org-translate-time
			    (org-element-property :raw-value scheduled)))))))
   " "))


;;;; Quote Block

(defun org-bbcode-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to BBCODE.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "[quote]\n%s[/quote]" contents))


;;;; Quote Section

(defun org-bbcode-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to BBCODE.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-remove-indentation
		(org-element-property :value quote-section))))
    (when value (format "[quote]\n%s[/quote]" value))))

;;;; Radio Target

(defun org-bbcode-radio-target (radio-target contents info)
  "Transcode a RADIO-TARGET object from Org to BBCODE.
CONTENTS is the contents of the target.  INFO is a plist holding
contextual information."
  contents)


;;;; Section

(defun org-bbcode-section (section contents info)
  "Transcode a SECTION element from Org to BBCODE.
CONTENTS is the contents of the section.  INFO is a plist holding
contextual information."
  contents)

;;;; Special Block

(defun org-bbcode-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to BBCODE.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  contents)


;;;; Src Block

(defun org-bbcode-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to BBCODE.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let ((caption (org-bbcode--build-caption src-block info))
	(code (org-export-format-code-default src-block info)))
    (if (equal code "") ""
      (concat
       (when (and caption org-bbcode-caption-above) (concat caption "\n"))
       (org-bbcode-format-code src-block info)
       ;; "[code]"
       ;; (replace-regexp-in-string "\\(?:\n[ \t]*\\)*\\'" "" code)
       ;; "[/code]"
       (when (and caption (not org-bbcode-caption-above))
	 (concat "\n" caption))))))


;;;; Statistics Cookie

(defun org-bbcode-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to BBCODE.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value statistics-cookie))


;;;; Subscript

(defun org-bbcode-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to BBCODE.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (if (org-element-property :use-brackets-p subscript)
      (format "_{%s}" contents)
    (format "_%s" contents)))


;;;; Superscript

(defun org-bbcode-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to BBCODE.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (if (org-element-property :use-brackets-p superscript)
      (format "_{%s}" contents)
    (format "_%s" contents)))


;;;; Strike-through

(defun org-bbcode-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to BBCODE.
CONTENTS is text with strike-through markup.  INFO is a plist
holding contextual information."
  (format (or (cdr (assq 'strike-through org-bbcode-text-markup-alist)) "%s")
	  contents))


;;;; Table

(defun org-bbcode-table (table contents info)
  "Transcode a TABLE element from Org to BBCODE.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (let ((caption (org-bbcode--build-caption table info)))
    (concat
     ;; Possibly add a caption string above.
     (when (and caption org-bbcode-caption-above) (concat caption "\n"))
     ;; Insert table.  Note: "table.el" tables are left unmodified.
     (format "[table]%s[/table]" contents)
     ;; Possible add a caption string below.
     (when (and caption (not org-bbcode-caption-above))
       (concat "\n" caption)))))


;;;; Table Cell

(defun org-bbcode-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to BBCODE.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((table-row (org-export-get-parent table-cell))
	 (table (org-export-get-parent-table table-cell)))
    (when (or (not contents) (string= "" (org-trim contents)))
      (setq contents "&#xa0;"))
    (cond
     ((and (org-export-table-has-header-p table info)
	   (= 1 (org-export-table-row-group table-row info)))
      (concat "[td]"
	      (org-bbcode-color contents "blue")
	      "[/td]"))
     (t (concat "[td]"
		contents "[/td]")))))

;;;; Table Row

(defun org-bbcode-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to BBCODE.
CONTENTS is the row contents.  INFO is a plist used as
a communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (concat
     ;; Actual table row
     (concat "[tr]"
	     contents
	     "[/tr]")
     ;; End a rowgroup?
     )))

;;;; Timestamp

(defun org-bbcode-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to BBCODE.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-ascii-plain-text (org-timestamp-translate timestamp) info))


;;;; Underline

(defun org-bbcode-underline (underline contents info)
  "Transcode UNDERLINE from Org to BBCODE.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format (or (cdr (assq 'underline org-bbcode-text-markup-alist)) "%s")
	  contents))


;;;; Verbatim

(defun org-bbcode-verbatim (verbatim contents info)
  "Return a VERBATIM object from Org to BBCODE.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format (or (cdr (assq 'verbatim org-bbcode-text-markup-alist)) "%s")
	  (org-bbcode-encode-plain-text (org-element-property :value verbatim))))

;;;; Verse Block

(defun org-bbcode-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to BBCODE.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  (concat
   "[code]"
   (replace-regexp-in-string "\\(?:\n[ \t]*\\)*\\'" "" contents)
   "[/code]"))



;;; Filters

(defun org-bbcode-filter-headline-blank-lines (headline back-end info)
  "Filter controlling number of blank lines after a headline.

HEADLINE is a string representing a transcoded headline.
BACK-END is symbol specifying back-end used for export.  INFO is
plist containing the communication channel.

This function only applies to `bbcode' back-end.  See
`org-bbcode-headline-spacing' for information."
  (if (not org-bbcode-headline-spacing) headline
    (let ((blanks (make-string (1+ (cdr org-bbcode-headline-spacing)) ?\n)))
      (replace-regexp-in-string "\n\\(?:\n[ \t]*\\)*\\'" blanks headline))))

(defun org-bbcode-filter-paragraph-spacing (tree back-end info)
  "Filter controlling number of blank lines between paragraphs.

TREE is the parse tree.  BACK-END is the symbol specifying
back-end used for export.  INFO is a plist used as
a communication channel.

See `org-bbcode-paragraph-spacing' for information."
  (when (wholenump org-bbcode-paragraph-spacing)
    (org-element-map tree 'paragraph
      (lambda (p)
	(when (eq (org-element-type (org-export-get-next-element p info))
		  'paragraph)
	  (org-element-put-property
	   p :post-blank org-bbcode-paragraph-spacing)))))
  tree)

(defun org-bbcode-filter-comment-spacing (tree backend info)
  "Filter removing blank lines between comments.
TREE is the parse tree.  BACK-END is the symbol specifying
back-end used for export.  INFO is a plist used as
a communication channel."
  (org-element-map tree '(comment comment-block)
    (lambda (c)
      (when (memq (org-element-type (org-export-get-next-element c info))
		  '(comment comment-block))
	(org-element-put-property c :post-blank 0))))
  tree)

(defun org-bbcode-filter-misc-spacing (tree backend info)
  "Filter removing blank lines between comments.
TREE is the parse tree.  BACK-END is the symbol specifying
back-end used for export.  INFO is a plist used as
a communication channel."
  (org-element-map tree '(src-block plain-list)
    (lambda (c)
      (org-element-put-property c :post-blank 0)))
  tree)



;;; End-user functions

;;;###autoload
(defun org-bbcode-export-as-bbcode
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a text buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title and
table of contents from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org BBCODE Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (if async
      (org-export-async-start
       (lambda (output)
	 (with-current-buffer (get-buffer-create "*Org BBCODE Export*")
	   (erase-buffer)
	   (insert output)
	   (goto-char (point-min))
	   (text-mode)
	   (org-export-add-to-stack (current-buffer) 'bbcode)))
       `(org-export-as 'bbcode ,subtreep ,visible-only ,body-only
		       ',ext-plist))
    (let ((outbuf (org-export-to-buffer
		   'bbcode "*Org BBCODE Export*"
		   subtreep visible-only body-only ext-plist)))
      (with-current-buffer outbuf (text-mode))
      (when org-export-show-temporary-export-buffer
	(switch-to-buffer-other-window outbuf)))))

;;;###autoload
(defun org-bbcode-export-to-bbcode
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a text file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title and
table of contents from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".txt" subtreep)))
    (if async
	(org-export-async-start
	 (lambda (f) (org-export-add-to-stack f 'bbcode))
	 `(expand-file-name
	   (org-export-to-file
	    'bbcode ,outfile ,subtreep ,visible-only ,body-only ',ext-plist)))
      (org-export-to-file
       'bbcode outfile subtreep visible-only body-only ext-plist))))

;;;###autoload
(defun org-bbcode-publish-to-bbcode (plist filename pub-dir)
  "Publish an Org file to BBCODE.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to
   'bbcode filename ".txt" `(:bbcode-charset bbcode ,@plist) pub-dir))

;;;###autoload
(defun org-bbcode-publish-to-latin1 (plist filename pub-dir)
  "Publish an Org file to Latin-1.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to
   'bbcode filename ".txt" `(:bbcode-charset latin1 ,@plist) pub-dir))

;;;###autoload
(defun org-bbcode-publish-to-utf8 (plist filename pub-dir)
  "Publish an org file to UTF-8.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to
   'bbcode filename ".txt" `(:bbcode-charset utf-8 ,@plist) pub-dir))


(provide 'ox-bbcode)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; coding: utf-8-emacs
;; End:

;;; ox-bbcode.el ends here

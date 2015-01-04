;;; bbcodeize.el --- Convert buffer text and decorations to bbcode.

;; Copyright (C) 2014-2015 Levin Du

;; Author: Levin Du <zslevin at gmail dot com>
;; Keywords: hypermedia, extensions
;; Version: 1.00

;; This file is heavily based on htmlize.el written by Hrvoje Niksic
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

;; This package converts the buffer text and the associated
;; decorations to BBCODE.

;; To use it, just switch to the buffer you want BBCODE-ized and type
;; `M-x bbcodeize-buffer'.  You will be switched to a new buffer that
;; contains the resulting BBCODE.  You can edit and inspect this
;; buffer, or you can just save it with C-x C-w.  `M-x bbcodeize-file'
;; will find a file, fontify it, and save the BBCODE version in
;; FILE..bbcode.txt, without any additional intervention.  `M-x
;; bbcodeize-many-files' allows you to bbcodeize any number of files in
;; the same manner.  `M-x bbcodeize-many-files-dired' does the same for
;; files marked in a dired buffer.
(require 'htmlize)

;;;###autoload
(defun bbcodeize-buffer (&optional buffer)
  "Convert BUFFER to BBCODE, preserving colors and decorations.

The generated BBCODE is available in a new buffer, which is returned.
When invoked interactively, the new buffer is selected in the current
window.  The title of the generated document will be set to the buffer's
file name or, if that's not available, to the buffer's name.

Note that bbcodeize doesn't fontify your buffers, it only uses the
decorations that are already present.  If you don't set up font-lock or
something else to fontify your buffers, the resulting BBCODE will be
plain.  Likewise, if you don't like the choice of colors, fix the mode
that created them, or simply alter the faces it uses."
  (interactive)
  (let ((bbcodebuf (with-current-buffer (or buffer (current-buffer))
		   (bbcodeize-buffer-1))))
    (when (interactive-p)
      (switch-to-buffer bbcodebuf))
    bbcodebuf))

;;;###autoload
(defun bbcodeize-region (beg end)
  "Convert the region to bbcode, preserving colors and decorations."
  (interactive "r")
  ;; Don't let zmacs region highlighting end up.
  (when (fboundp 'zmacs-deactivate-region)
    (zmacs-deactivate-region))
  (let ((bbcodebuf (save-restriction
		   (narrow-to-region beg end)
		   (bbcodeize-buffer-1))))
    (when (interactive-p)
      (switch-to-buffer bbcodebuf))
    bbcodebuf))

(defun bbcodeize-make-file-name (file)
  "Make an bbcode file name from FILE."
  (concat file ".bbcode.txt"))

(defun bbcodeize-buffer-1 ()
  ;; Internal function; don't call it from outside this file.  Bbcodeize
  ;; current buffer, writing the resulting bbcode to a new buffer, and
  ;; return it.
  (save-excursion
    ;; Protect against the hook changing the current buffer.
    (save-excursion
      (run-hooks 'bbcodeize-before-hook))
    ;; Convince font-lock support modes to fontify the entire buffer
    ;; in advance.
    (htmlize-ensure-fontified)
    (clrhash htmlize-extended-character-cache)
    (clrhash htmlize-memoization-table)
    ;; It's important that the new buffer inherits default-directory
    ;; from the current buffer.
    (let ((htmlbuf (generate-new-buffer (if (buffer-file-name)
                                            (bbcodeize-make-file-name
                                             (file-name-nondirectory
                                              (buffer-file-name)))
                                          "*bbcode*")))
          (completed nil))
      (unwind-protect
          (let* ((buffer-faces (htmlize-faces-in-buffer))
                 (face-map (htmlize-make-face-map (adjoin 'default buffer-faces)))
                 (title (if (buffer-file-name)
                            (file-name-nondirectory (buffer-file-name))
                          (buffer-name))))
            (with-current-buffer htmlbuf
              (buffer-disable-undo))
            (let ((text-markup
                   ;; Get the inserter method, so we can funcall it inside
                   ;; the loop.  Not calling `htmlize-method' in the loop
                   ;; body yields a measurable speed increase.
                   (indirect-function 'bbcodeize-font-text-markup))
                  ;; Declare variables used in loop body outside the loop
                  ;; because it's faster to establish `let' bindings only
                  ;; once.
                  next-change text face-list trailing-ellipsis
                  fstruct-list last-fstruct-list
                  (close-markup (lambda ())))
              ;; This loop traverses and reads the source buffer, appending
              ;; the resulting HTML to HTMLBUF.  This method is fast
              ;; because: 1) it doesn't require examining the text
              ;; properties char by char (htmlize-next-face-change is used
              ;; to move between runs with the same face), and 2) it doesn't
              ;; require frequent buffer switches, which are slow because
              ;; they rebind all buffer-local vars.
              (goto-char (point-min))
              (while (not (eobp))
                (setq next-change (htmlize-next-face-change (point)))
                ;; Get faces in use between (point) and NEXT-CHANGE, and
                ;; convert them to fstructs.
                (setq face-list (htmlize-faces-at-point)
                      fstruct-list (delq nil (mapcar (lambda (f)
                                                       (gethash f face-map))
                                                     face-list)))
                (multiple-value-setq (text trailing-ellipsis)
                  (htmlize-extract-text (point) next-change trailing-ellipsis))
                ;; Don't bother writing anything if there's no text (this
                ;; happens in invisible regions).
                (when (> (length text) 0)
                  ;; Open the new markup if necessary and insert the text.
                  (when (not (equalp fstruct-list last-fstruct-list))
                    (funcall close-markup)
                    (setq last-fstruct-list fstruct-list
                          close-markup (funcall text-markup fstruct-list htmlbuf)))
                  (princ text htmlbuf))
                (goto-char next-change))

              ;; We've gone through the buffer; close the markup from
              ;; the last run, if any.
              (funcall close-markup))

            ;; Insert the epilog and post-process the buffer.
            (with-current-buffer htmlbuf
              (buffer-enable-undo))
            (setq completed t)
            htmlbuf)

        (when (not completed)
          (kill-buffer htmlbuf))
        (htmlize-delete-tmp-overlays)))))

(defun bbcodeize-font-text-markup (fstruct-list buffer)
  (let* ((merged (htmlize-merge-faces fstruct-list))
	 (markup (htmlize-memoize
		  merged
		  (cons (concat
			 (and (htmlize-fstruct-foreground merged)
			      (format "[color=%s]" (htmlize-fstruct-foreground merged)))
			 (and (htmlize-fstruct-boldp merged)      "[b]")
			 (and (htmlize-fstruct-italicp merged)    "[i]")
			 (and (htmlize-fstruct-underlinep merged) "[u]")
			 (and (htmlize-fstruct-strikep merged)    "[s]"))
			(concat
			 (and (htmlize-fstruct-strikep merged)    "[/s]")
			 (and (htmlize-fstruct-underlinep merged) "[/u]")
			 (and (htmlize-fstruct-italicp merged)    "[/i]")
			 (and (htmlize-fstruct-boldp merged)      "[/b]")
			 (and (htmlize-fstruct-foreground merged) "[/color]"))))))
    (princ (car markup) buffer)
    (htmlize-lexlet ((markup markup) (buffer buffer))
      (lambda ()
        (princ (cdr markup) buffer)))))


(defun bbcodeize-region-for-paste (beg end)
  "Bbcodeize the region and return just the bbcode as a string."
  (let ((htmlbuf (bbcodeize-region beg end)))
    (unwind-protect
	(with-current-buffer htmlbuf
	  (buffer-substring (point-min) (point-max)))
      (kill-buffer htmlbuf))))

;;;###autoload
(defun bbcodeize-file (file &optional target)
  "Load FILE, fontify it, convert it to BBCODE, and save the result.

Contents of FILE are inserted into a temporary buffer, whose major mode
is set with `normal-mode' as appropriate for the file type.  The buffer
is subsequently fontified with `font-lock' and converted to BBCODE.  Note
that, unlike `bbcodeize-buffer', this function explicitly turns on
font-lock.  If a form of highlighting other than font-lock is desired,
please use `bbcodeize-buffer' directly on buffers so highlighted.

Buffers currently visiting FILE are unaffected by this function.  The
function does not change current buffer or move the point.

If TARGET is specified and names a directory, the resulting file will be
saved there instead of to FILE's directory.  If TARGET is specified and
does not name a directory, it will be used as output file name."
  (interactive (list (read-file-name
		      "BBCODE-ize file: "
		      nil nil nil (and (buffer-file-name)
				       (file-name-nondirectory
					(buffer-file-name))))))
  (let ((output-file (if (and target (not (file-directory-p target)))
			 target
		       (expand-file-name
			(bbcodeize-make-file-name (file-name-nondirectory file))
			(or target (file-name-directory file)))))
	;; Try to prevent `find-file-noselect' from triggering
	;; font-lock because we'll fontify explicitly below.
	(font-lock-mode nil)
	(font-lock-auto-fontify nil)
	(global-font-lock-mode nil)
	;; Ignore the size limit for the purposes of bbcodeization.
	(font-lock-maximum-size nil)
	;; Disable font-lock support modes.  This will only work in
	;; more recent Emacs versions, so bbcodeize-buffer-1 still needs
	;; to call bbcodeize-ensure-fontified.
	(font-lock-support-mode nil))
    (with-temp-buffer
      ;; Insert FILE into the temporary buffer.
      (insert-file-contents file)
      ;; Set the file name so normal-mode and bbcodeize-buffer-1 pick it
      ;; up.  Restore it afterwards so with-temp-buffer's kill-buffer
      ;; doesn't complain about killing a modified buffer.
      (let ((buffer-file-name file))
	;; Set the major mode for the sake of font-lock.
	(normal-mode)
	(font-lock-mode 1)
	(unless font-lock-mode
	  ;; In GNU Emacs (font-lock-mode 1) doesn't force font-lock,
	  ;; contrary to the documentation.  This seems to work.
	  (font-lock-fontify-buffer))
	;; bbcodeize the buffer and save the BBCODE.
	(with-current-buffer (bbcodeize-buffer-1)
	  (unwind-protect
	      (progn
		(run-hooks 'bbcodeize-file-hook)
		(write-region (point-min) (point-max) output-file))
	    (kill-buffer (current-buffer)))))))
  ;; I haven't decided on a useful return value yet, so just return
  ;; nil.
  nil)

;;;###autoload
(defun bbcodeize-many-files (files &optional target-directory)
  "Convert FILES to BBCODE and save the corresponding BBCODE versions.

FILES should be a list of file names to convert.  This function calls
`bbcodeize-file' on each file; see that function for details.  When
invoked interactively, you are prompted for a list of files to convert,
terminated with RET.

If TARGET-DIRECTORY is specified, the BBCODE files will be saved to that
directory.  Normally, each BBCODE file is saved to the directory of the
corresponding source file."
  (interactive
   (list
    (let (list file)
      ;; Use empty string as DEFAULT because setting DEFAULT to nil
      ;; defaults to the directory name, which is not what we want.
      (while (not (equal (setq file (read-file-name
				     "BBCODE-ize file (RET to finish): "
				     (and list (file-name-directory
						(car list)))
				     "" t))
			 ""))
	(push file list))
      (nreverse list))))
  ;; Verify that TARGET-DIRECTORY is indeed a directory.  If it's a
  ;; file, bbcodeize-file will use it as target, and that doesn't make
  ;; sense.
  (and target-directory
       (not (file-directory-p target-directory))
       (error "target-directory must name a directory: %s" target-directory))
  (dolist (file files)
    (bbcodeize-file file target-directory)))

;;;###autoload
(defun bbcodeize-many-files-dired (arg &optional target-directory)
  "BBCODEize dired-marked files."
  (interactive "P")
  (bbcodeize-many-files (dired-get-marked-files nil arg) target-directory))

(provide 'bbcodeize)

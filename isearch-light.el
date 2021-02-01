;;; isearch-light.el --- Simple incremental search in current-buffer -*- lexical-binding: t -*-

;; Author:      Thierry Volpiatto <thievol@posteo.net>
;; Copyright (C) 2021 Thierry Volpiatto <thievol@posteo.net>

;; Version: 1.0
;; URL: https://github.com/thierryvolpiatto/isearch-light

;; Compatibility: GNU Emacs 24.3+
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Isearch-light is a small package to search regexp incrementaly in
;; current-buffer.  It is simple to use, just call M-x `isl'.

;;; Code:

(require 'cl-lib)

(defvar iedit-aborting)
(defvar iedit-read-only-occurrences-overlays)
(defvar iedit-read-only-occurrences-overlays)
(defvar iedit-case-sensitive)
(defvar iedit-occurrences-overlays)
(defvar iedit-mode)
(defvar helm-occur-always-search-in-current)
(defvar hs-minor-mode)
(defvar hs-show-hook)
(declare-function iedit-make-read-only-occurrence-overlay "ext:iedit-lib.el")
(declare-function iedit-make-occurrence-overlay "ext:iedit-lib.el")
(declare-function iedit-update-index "ext:iedit-lib.el")
(declare-function iedit-lib-cleanup "ext:iedit-lib.el")
(declare-function iedit-start "ext:iedit.el")
(declare-function iedit-done "ext:iedit.el")
(declare-function outline-show-entry "outline.el")
(declare-function org-reveal "org.el")
(declare-function helm-multi-occur-1 "ext:helm-occur.el")
(declare-function hs-show-block "hideshow.el")
(declare-function markdown-show-entry "markdown-mode.el")

;; Internals
(defvar isl-pattern "")
(defvar isl-current-buffer nil)
(defvar isl--item-overlays nil)
(defvar isl--iterator nil)
(defvar isl--last-overlay nil)
(defvar isl--direction nil)
(defvar isl-initial-pos nil)
(defvar isl--number-results 0)
(defvar isl-history nil)
(defvar isl--yank-point nil)
(defvar isl--quit nil)
(defvar-local isl--buffer-invisibility-spec nil)
(defconst isl-space-regexp "\\s\\\\s-"
  "Match a quoted space in a string.")

;; User vars
(defvar isl-timer-delay 0.01)

(defgroup isearch-light nil
  "Open isl."
  :prefix "isl"
  :group 'matching)

(defcustom isl-search-function #'re-search-forward
  "The search function that will be used by default when starting `isl'.
Possible values are `re-search-forward' and `search-forward', the
first use regexp matching while the second is using literal matching.
Its value can be changed during `isl' session with `\\<isl-map>\\[isl-change-matching-style]'."
  :type '(choice
           (function :tag "Regexp matching" re-search-forward)
           (function :tag "Literal matching" search-forward)))

(defcustom isl-case-fold-search 'smart
  "The `case-fold-search' value.
Possible value are nil, t or smart.
Value smart means use `case-fold-search' when upcase chars are detected
in pattern."
  :type 'symbol)

(defcustom isl-after-position-string ">"
  "The string used to notify in mode-line when position is above initial pos."
  :type 'string)

(defcustom isl-before-position-string "<"
  "The string used to notify in mode-line when position is below initial pos."
  :type 'string)

(defcustom isl-direction-down-string "↓"
  "The string used in mode-line to notify search direction."
  :type 'string)

(defcustom isl-direction-up-string "↑"
  "The string used in mode-line to notify search direction."
  :type 'string)

(defcustom isl-save-pos-to-mark-ring t
  "Save initial position to mark-ring on exit when non nil."
  :type 'boolean)

(defcustom isl-requires-pattern 1
  "Start updating after this number of chars."
  :type 'integer)

(defface isl-match
  '((t :background "Brown4"))
  "Face used to highlight the items matched.")

(defface isl-on
  '((t :background "SandyBrown"
       :foreground "black"))
  "Face used to highlight the item where point is.")

(defface isl-line
  '((t :background "Darkgoldenrod1" :extend t))
  "Face used to flash line on exit.")

(defface isl-number
  '((t :foreground "green"))
  "Face used to highlight number in mode-line.")

(defface isl-string
  '((t :foreground "Lightgoldenrod1" :bold t))
  "Face used to highlight pattern in mode-line.")

(defvar isl-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-s")    'isl-goto-next)
    (define-key map (kbd "C-r")    'isl-goto-prev)
    (define-key map (kbd "<down>") 'isl-goto-next)
    (define-key map (kbd "<up>")   'isl-goto-prev)
    (define-key map (kbd "RET")    'isl-exit-at-point)
    (define-key map (kbd "C-w")    'isl-yank-word-at-point)
    (define-key map (kbd "M-r")    'isl-change-matching-style)
    (define-key map (kbd "M-<")    'isl-goto-first)
    (define-key map (kbd "M->")    'isl-goto-last)
    (define-key map (kbd "M-s")    'isl-jump-to-helm-occur)
    (define-key map (kbd "C-;")    'isl-jump-to-iedit-mode)
    map))

;;; Actions
;;
(defun isl--goto-overlay (overlay)
  "Goto OVERLAY."
  (let ((pos (and overlay (overlay-end overlay))))
    (when (and overlay pos)
      (setq isl--last-overlay overlay)
      (overlay-put overlay 'face 'isl-on)
      (goto-char pos)
      (setq isl--yank-point pos))))

(defun isl--highlight-last-overlay (face)
  "Highlight `isl--last-overlay' with FACE."
  (when (overlayp isl--last-overlay)
    (overlay-put isl--last-overlay 'face face)))

(defun isl-goto-next-1 ()
  "Main function that allow moving from one to another overlay.
It put overlay on current position, move to next overlay using
`isl--iterator', set `isl--yank-point' and then setup mode-line."
  (with-selected-window (minibuffer-selected-window)
    (isl--highlight-last-overlay 'isl-match)
    (when isl--iterator
      (isl--goto-overlay (isl-iter-next isl--iterator)))
    (isl-setup-mode-line)))

(defun isl--find-and-goto-overlay (overlay)
  "Consume iterators up to OVERLAY and jump to it."
  (with-selected-window (minibuffer-selected-window)
    (let (ov)
      (while (not (eql (setq ov (isl-iter-next isl--iterator))
                       overlay)))
      (isl--highlight-last-overlay 'isl-match)
      (and ov (isl--goto-overlay ov)))
    (isl-setup-mode-line)))

(defun isl-goto-first ()
  "Goto first match."
  (interactive)
  (isl--find-and-goto-overlay (car isl--item-overlays)))

(defun isl-goto-last ()
  "Goto last match."
  (interactive)
  (isl--find-and-goto-overlay (car (last isl--item-overlays))))

(defun isl-goto-next ()
  "Go to next match in isl."
  (interactive)
  (when (eq isl--direction 'backward)
    (setq isl--direction 'forward)
    (isl-set-iterator t))
  (isl-goto-next-1))

(defun isl-goto-prev ()
  "Go to previous match in isl."
  (interactive)
  (when (eq isl--direction 'forward)
    (setq isl--direction 'backward)
    (isl-set-iterator t))
  (isl-goto-next-1))

(defun isl-exit-at-point ()
  "The exit command for isl."
  (interactive)
  (with-selected-window (minibuffer-selected-window)
    ;; Ensure user haven't scrolled to another place.
    (goto-char (overlay-end isl--last-overlay))
    (when isl-save-pos-to-mark-ring
      (set-marker (mark-marker) isl-initial-pos)
      (push-mark isl-initial-pos 'nomsg))
    (let ((ov (make-overlay (point-at-bol) (point-at-eol))))
      (overlay-put ov 'face 'isl-line)
      (sit-for 0.2)
      (delete-overlay ov)))
  ;; Call `exit-minibuffer' out of the `with-selected-window' block to
  ;; avoid error with the emacs-28 version.
  (exit-minibuffer))

(defun isl-yank-word-at-point ()
  "Yank word at point in minibuffer.
The word at point is relative to the current position in buffer, not
the initial position i.e. the position before launching isl."
  (interactive)
  (let (str)
    (with-current-buffer isl-current-buffer
      (when (or (memq (char-syntax (or (char-after) 0)) '(?w ?_))
                (memq (char-syntax (or (char-after (1+ (point))) 0))
                      '(?w ?_)))
        (setq str (buffer-substring-no-properties (or isl--yank-point (point))
                                                  (save-excursion
                                                    (forward-word)
                                                    (point))))))
    (when str
      (with-selected-window (minibuffer-window)
        (insert str)))))

(defun isl-matching-style ()
  "Return current matching style as a string."
  (cl-ecase isl-search-function
    (re-search-forward "Regex")
    (search-forward "Literal")))

(defun isl-change-matching-style ()
  "Toggle style matching in `isl' i.e. regexp/literal."
  (interactive)
  (with-current-buffer isl-current-buffer
    (setq-local isl-search-function
                (cl-ecase isl-search-function
                  (re-search-forward #'search-forward)
                  (search-forward #'re-search-forward)))
    (when (string= isl-pattern "")
      (let* ((style (isl-matching-style))
             (mode-line-format (format " Switching to %s searching" style)))
        (force-mode-line-update)
        (sit-for 1)))
    (isl-update)))

(defun isl-jump-to-helm-occur ()
  "Invoke `helm-occur' from isl."
  (interactive)
  (cl-assert (require 'helm-occur nil t))
  (let ((input isl-pattern)
        (bufs (list isl-current-buffer)))
    (run-at-time 0.1 nil
                 (lambda ()
                   ;; Use `helm-occur-always-search-in-current' as a
                   ;; flag for `helm-occur--select-closest-candidate'.
                   (let ((helm-occur-always-search-in-current t))
                     (helm-multi-occur-1 bufs input))))
    (abort-recursive-edit)))

;; Iedit
;;
(defun isl--advice-iedit-start (old--fn &rest args)
  (cl-letf (((symbol-function 'iedit-make-occurrences-overlays)
             #'isl--iedit-make-occurrences-overlays))
    (apply old--fn args)))

(defun isl--iedit-make-occurrences-overlays (occurrence-regexp beg end)
  "Create occurrence overlays for `occurrence-regexp' in a region.
Return the number of occurrences."
  (setq iedit-aborting nil)
  (setq iedit-occurrences-overlays nil)
  (setq iedit-read-only-occurrences-overlays nil)
  ;; Find and record each occurrence's markers and add the overlay to the occurrences
  (let ((counter 0)
        (case-fold-search (not iedit-case-sensitive))
	(length 0)
        bounds)
    (save-excursion
      (save-selected-window
        (goto-char beg)
        (while (setq bounds (isl-multi-search-fwd occurrence-regexp end t))
          (let ((beginning (car bounds))
                (ending (cdr bounds)))
	    (if (and (> length 0) (/= (- ending beginning) length))
		(throw 'not-same-length 'not-same-length)
	      (setq length (- ending beginning)))
            (if (text-property-not-all beginning ending 'read-only nil)
                (push (iedit-make-read-only-occurrence-overlay beginning ending)
                      iedit-read-only-occurrences-overlays)
              (push (iedit-make-occurrence-overlay beginning ending)
                    iedit-occurrences-overlays))
            (setq counter (1+ counter))))))
    (iedit-update-index)
    counter))

(defun isl-jump-to-iedit-mode ()
  "Start Iedit mode from `isl' using last search string as the regexp."
  (interactive)
  (cl-assert (require 'iedit nil t))
  (let ((regexp (if (eq isl-search-function 'search-forward)
                    (regexp-quote isl-pattern)
                  isl-pattern))
        (pos (with-current-buffer isl-current-buffer
               (overlay-end isl--last-overlay))))
    (run-at-time
     0.1 nil
     (lambda ()
       (let ((iedit-case-sensitive (not (isl-set-case-fold-search regexp)))
	     result)
         (setq mark-active nil)
         (run-hooks 'deactivate-mark-hook)
         (when iedit-mode
           (iedit-lib-cleanup))
         (advice-add 'iedit-start :around #'isl--advice-iedit-start)
         (unwind-protect
             (progn
               (setq result
	             (catch 'not-same-length
	               (iedit-start regexp (point-min) (point-max))))
               (cond ((not iedit-occurrences-overlays)
                      (message "No matches found for %s" regexp)
                      (iedit-done))
                     ((equal result 'not-same-length)
                      (message "Matches are not the same length.")
                      (iedit-done)))
               (goto-char pos))
           (advice-remove 'iedit-start #'isl--advice-iedit-start)))))
    (abort-recursive-edit)))

(defun isl-iter-circular (seq)
  "Infinite iteration on SEQ."
  (let ((lis seq))
     (lambda ()
       (let ((elm (car lis)))
         (setq lis (pcase lis (`(,_ . ,ll) (or ll seq))))
         elm))))

(defun isl-iter-next (iterator)
  "Return next elm of ITERATOR."
  (and iterator (funcall iterator)))

(defun isl-delete-overlays ()
  "Cleanup ovelays."
  (when isl--item-overlays
    (remove-overlays nil nil 'isl t)
    (setq isl--item-overlays nil)))

(cl-defun isl-set-case-fold-search (&optional (pattern isl-pattern))
  "Return a suitable value for `case-fold-search'.
This is done according to `isl-case-fold-search'.
Optional argument PATTERN default to `isl-pattern'."
  (cl-case isl-case-fold-search
    (smart (let ((case-fold-search nil))
             (if (string-match "[[:upper:]]" pattern) nil t)))
    (t isl-case-fold-search)))

(defun isl-split-string (str)
  (split-string
   (replace-regexp-in-string
    isl-space-regexp "\\s-" str nil t)))

(defun isl-patterns (str)
  "Returns an alist of (pred . regexp) elements from STR."
  (cl-loop for s in (isl-split-string str)
           collect (if (char-equal ?! (aref s 0))
                       (cons 'not (substring s 1))
                     (cons 'identity s))))

(defun isl-multi-search-fwd (str &optional _bound _noerror _count)
  "Returns position of symbol matched by STR.
When arg STR contains spaces, it is converted in patterns with
`isl-patterns' , when first pattern of list match a symbol
subsequent patterns are used to check if all patterns match this
symbol.  The return value is a cons cell (beg . end) denoting
symbol position."
  ;; Prevent infloop crashing Emacs with incorrect configuration.
  (cl-assert (memq isl-search-function '(re-search-forward search-forward)))
  (let* ((pattern (isl-patterns str))
         (initial (or (assq 'identity pattern)
                      '(identity . "")))
         (rest    (cdr pattern)))
    (cl-loop while (funcall isl-search-function (cdr initial) nil t)
             for bounds = (if rest
                              (bounds-of-thing-at-point 'symbol)
                            (cons (match-beginning 0) (match-end 0)))
             if (or (not rest)
                    (cl-loop for (pred . re) in rest
                             always (funcall pred
                                             (progn
                                               (goto-char (car bounds))
                                               (funcall isl-search-function
                                                        re (cdr bounds) t)))))
             do (goto-char (cdr bounds)) and return bounds
             else do (goto-char (cdr bounds))
             finally return nil)))

(defun isl-update ()
  "Update `current-buffer' when `isl-pattern' changes."
  (with-selected-window (minibuffer-selected-window)
    (while-no-input
      (isl-delete-overlays)
      ;; We don't use the isearch-invisible mechanism which is heavy
      ;; and don't behave as we want, instead remove invisibility in
      ;; all buffer and on exit restore it and unhide only the place
      ;; where point is with appropriate functions belonging to
      ;; major-mode e.g. org => org-reveal etc...
      (when (and buffer-invisibility-spec
                 (listp buffer-invisibility-spec))
        (mapc 'remove-from-invisibility-spec buffer-invisibility-spec))
      (let ((case-fold-search (isl-set-case-fold-search))
            (count 1)
            ov
            bounds)
        (unless (string= isl-pattern "")
          (save-excursion
            (goto-char (point-min))
            (condition-case-unless-debug nil
                (while (setq bounds (isl-multi-search-fwd isl-pattern nil t))
                  (setq ov (make-overlay (car bounds) (cdr bounds)))
                  (push ov isl--item-overlays)
                  (overlay-put ov 'isl t)
                  (overlay-put ov 'pos count)
                  (overlay-put ov 'face 'isl-match)
                  (cl-incf count))
              (invalid-regexp nil))
            (setq isl--item-overlays (reverse isl--item-overlays)))
          (if (null isl--item-overlays)
              (progn (setq isl--number-results 0) (goto-char isl-initial-pos))
            (setq isl--last-overlay
                  (isl-closest-overlay isl-initial-pos isl--item-overlays)
                  isl--number-results (length isl--item-overlays))
            (isl--highlight-last-overlay 'isl-on)
            (isl-set-iterator)
            (goto-char (overlay-end (isl-iter-next isl--iterator)))
            (setq isl--yank-point (point)))))
      (isl-setup-mode-line))))

(defun isl-setup-mode-line ()
  "Setup `mode-line-format' for isl."
  (let ((style (isl-matching-style))
        (position (with-current-buffer isl-current-buffer
                     (if (> (point) isl-initial-pos)
                         isl-after-position-string
                       isl-before-position-string)))
        (direction (if (eq isl--direction 'forward)
                       isl-direction-down-string
                     isl-direction-up-string)))
    (setq mode-line-format
          (cond ((or (string= isl-pattern "")
                     (<= (length isl-pattern)
                         isl-requires-pattern))
                 (default-value 'mode-line-format))
                ((zerop isl--number-results)
                 `(" " mode-line-buffer-identification " "
                   (:eval ,(format "No results found for `%s' [%s %s]"
                                   (propertize isl-pattern
                                               'face 'isl-string)
                                   style
                                   direction))
                   " " mode-line-position))
                (t `(" " mode-line-buffer-identification " "
                     (:eval ,(format "[%s/%s] result(s) found for `%s' [%s %s %s]"
                                     (propertize
                                      (number-to-string
                                       (overlay-get isl--last-overlay 'pos))
                                      'face 'isl-number)
                                     (propertize (number-to-string
                                                  isl--number-results)
                                                 'face 'isl-number)
                                     (propertize isl-pattern
                                                 'face 'isl-string)
                                     style
                                     direction
                                     position))
                     " " mode-line-position))))))

(defun isl-closest-overlay (pos overlays)
  "Return closest overlay from POS in OVERLAYS list."
  (cl-loop for ov in overlays
           for ovpos = (overlay-start ov)
           for diff = (if (> pos ovpos) (- pos ovpos) (- ovpos pos))
           collect (cons diff ov) into res
           minimize diff into min
           finally return (cdr (assq min res))))

(defun isl-set-iterator (&optional skip-first)
  "Build `isl--iterator' against `isl--item-overlays' according to context.
When SKIP-FIRST is specified build overlay with the current overlay
appended at end."
  (let* ((revlst (if (eq isl--direction 'forward)
                     isl--item-overlays
                   (reverse isl--item-overlays)))
         (fwdlst (memql isl--last-overlay revlst))
         (ovlst (append (if skip-first (cdr fwdlst) fwdlst)
                        (butlast revlst (length fwdlst))
                        (and skip-first (list (car fwdlst))))))
      (setq isl--iterator (isl-iter-circular ovlst))))

(defun isl-check-input ()
  "Check minibuffer input."
  (with-selected-window (minibuffer-window)
    (let ((input (minibuffer-contents)))
      (when (not (string= input isl-pattern))
        (setq isl-pattern input)
        (if (> (length input) isl-requires-pattern)
            (isl-update)
          (with-selected-window (minibuffer-selected-window)
            (isl-delete-overlays)
            (isl-setup-mode-line)
            (goto-char isl-initial-pos)))))))

(defun isl-read-from-minibuffer (prompt)
  "Read input from minibuffer with prompt PROMPT."
  (let (timer)
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (setq timer (run-with-idle-timer
                           isl-timer-delay 'repeat #'isl-check-input)))
          (read-from-minibuffer
           prompt nil isl-map nil 'isl-history (thing-at-point 'symbol t)))
      (cancel-timer timer))))

(defun isl-cleanup ()
  "Cleanup various things when `isl' exit."
  (let* ((pos (and isl--last-overlay ; nil when quitting with no results.
                   (overlay-end isl--last-overlay)))
         (hs-show-hook (list (lambda () (and pos (goto-char pos))))))
    (isl-delete-overlays)
    (setq mode-line-format (default-value 'mode-line-format)
          isl--yank-point nil
          isl--iterator nil
          isl--item-overlays nil
          isl--last-overlay nil
          isl--number-results nil
          isl-search-function (default-value 'isl-search-function)
          buffer-invisibility-spec isl--buffer-invisibility-spec)
    (if isl--quit
        (setq isl--quit nil)
      (condition-case-unless-debug _err
        (cond ((eq major-mode 'org-mode)
               (org-reveal))
              ((eq major-mode 'outline-mode)
               (outline-show-entry))
              ((and (boundp 'hs-minor-mode)
                    hs-minor-mode)
               (hs-show-block))
              ((and (boundp 'markdown-mode-map)
                    (derived-mode-p 'markdown-mode))
               (markdown-show-entry)))
        (error nil)))))

;;;###autoload
(defun isl ()
  "Start incremental searching in current buffer."
  (interactive)
  (setq isl-initial-pos (point)
        isl-pattern ""
        isl--direction 'forward
        isl-current-buffer (current-buffer)
        isl--buffer-invisibility-spec buffer-invisibility-spec)
  (unwind-protect
      (condition-case-unless-debug nil
          (isl-read-from-minibuffer "Search: ")
        (quit
         (setq isl--quit t)
         (goto-char isl-initial-pos)))
    (isl-cleanup)
    (and (not (string= isl-pattern ""))
         (recenter))))

;;;###autoload
(defun isl-narrow-to-defun ()
  "Start incremental searching in current defun."
  (interactive)
  (save-restriction
    (narrow-to-defun)
    (isl)))

(provide 'isearch-light)

;;; isearch-light.el ends here

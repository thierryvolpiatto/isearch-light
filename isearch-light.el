;;; isearch-light.el --- simple incremental search in current-buffer -*- lexical-binding: t -*- 

;; Author:      Thierry Volpiatto <thievol@posteo.net>
;; Copyright (C) 2021 Thierry Volpiatto <thievol@posteo.net>

;; Version: 1.0
;; X-URL: https://github.com/thierryvolpiatto/isearch-light

;; Compatibility: GNU Emacs 26.3+
;; Package-Requires: ((emacs "26") (cl-lib "0.5") (iterator 1.0))

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
(require 'iterator)

;; Internals
(defvar isl-pattern "")
(defvar isl-current-buffer nil)
(defvar isl-item-overlays nil)
(defvar isl-iterator nil)
(defvar isl-last-overlay nil)
(defvar isl-direction nil)
(defvar isl-initial-pos nil)
(defvar isl-number-results 0)
(defvar isl-history nil)
(defvar isl-yank-point nil)
(defvar isl-search-function #'re-search-forward)

;; User vars
(defvar isl-case-fold-search 'smart
  "The `case-fold-search' value.")
(defvar isl-after-position-string "<")
(defvar isl-before-position-string ">")
(defvar isl-direction-down-string "↓")
(defvar isl-direction-up-string "↑")

(defgroup isearch-light nil
  "Open isl."
  :prefix "isl-"
  :group 'matching)

(defface isl-match
  `((t :background "Brown4"))
  "Face used to highlight the items matched."
  :group 'isearch-light)

(defface isl-on
  `((t :background "SandyBrown"
       :foreground "black"))
  "Face used to highlight the item where point is."
  :group 'isearch-light)

(defface isl-line
  `((t :background "Darkgoldenrod1"))
  "Face used to flash line on exit."
  :group 'isearch-light)

(defface isl-number
  `((t :foreground "green"))
  "Face used to highlight number in mode-line."
  :group 'isearch-light)

(defface isl-string
  `((t :foreground "Lightgoldenrod1" :bold t))
  "Face used to highlight pattern in mode-line."
  :group 'isearch-light)

(defvar isl-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-s")    'isl-goto-next)
    (define-key map (kbd "C-r")    'isl-goto-prev)
    (define-key map (kbd "<down>") 'isl-goto-next)
    (define-key map (kbd "<up>")   'isl-goto-prev)
    (define-key map (kbd "RET")    'isl-exit-at-point)
    (define-key map (kbd "C-w")    'isl-yank-word-at-point)
    (define-key map (kbd "M-r")    'isl-toggle-style-matching)
    map))

;;; Actions
;;
(defun isl-goto-next-1 ()
  (with-selected-window (get-buffer-window isl-current-buffer)
    (when (overlayp isl-last-overlay)
      (overlay-put isl-last-overlay 'face 'isl-match))
    (when isl-iterator
      (let* ((ov (iterator:next isl-iterator))
             (pos (and ov (overlay-end ov))))
        (when (and ov pos)
          (setq isl-last-overlay ov)
          (overlay-put ov 'face 'isl-on)
          (goto-char pos)
          (setq isl-yank-point pos))))
    (isl--setup-mode-line)))

(defun isl-goto-next ()
  (interactive)
  (when (eq isl-direction 'backward)
    (setq isl-direction 'forward)
    (isl--set-iterator t))
  (isl-goto-next-1))

(defun isl-goto-prev ()
  (interactive)
  (when (eq isl-direction 'forward)
    (setq isl-direction 'backward)
    (isl--set-iterator t))
  (isl-goto-next-1))

(defun isl-exit-at-point ()
  (interactive)
  (with-selected-window (get-buffer-window isl-current-buffer)
    (let ((ov (make-overlay (point-at-bol) (point-at-eol))))
      (overlay-put ov 'face 'isl-line)
      (sit-for 0.1)
      (delete-overlay ov))
    (exit-minibuffer)))

(defun isl-yank-word-at-point ()
  (interactive)
  (let (str)
    (with-current-buffer isl-current-buffer
      (when (or (memq (char-syntax (or (char-after) 0)) '(?w))
                (memq (char-syntax (or (char-after (1+ (point))) 0))
                      '(?w)))
        (setq str (buffer-substring-no-properties (or isl-yank-point (point))
                                                  (save-excursion
                                                    (forward-word)
                                                    (point))))))
    (when str
      (with-selected-window (minibuffer-window)
        (insert str)))))

(defun isl-toggle-style-matching ()
  (interactive)
  (with-current-buffer isl-current-buffer
    (setq-local isl-search-function
                (if (eq isl-search-function 're-search-forward)
                    #'search-forward
                  #'re-search-forward))
    (when (string= isl-pattern "")
      (let* ((style (cl-case isl-search-function
                      (re-search-forward "Regex")
                      (search-forward "Literal")))
             (mode-line-format (format " Switching to %s searching" style)))
        (force-mode-line-update)
        (sit-for 1)))
    (isl--setup-mode-line)))


(defun isl-delete-overlays ()
  (when isl-item-overlays
    (mapc 'delete-overlay isl-item-overlays)
    (setq isl-item-overlays nil)))

(cl-defun isl-set-case-fold-search (&optional (pattern isl-pattern))
  (cl-case isl-case-fold-search
    (smart (let ((case-fold-search nil))
             (if (string-match "[[:upper:]]" pattern) nil t)))
    (t isl-case-fold-search)))

(defun isl-update-overlays ()
  (with-selected-window (get-buffer-window isl-current-buffer)
    (isl-delete-overlays)
    (let ((case-fold-search (isl-set-case-fold-search))
          ov)
      (while-no-input
        (unless (string= isl-pattern "")
          (save-excursion
            (goto-char (point-min))
            (condition-case-unless-debug nil
                (while (funcall isl-search-function isl-pattern nil t)
                  (setq ov (make-overlay (match-beginning 0) (match-end 0)))
                  (push ov isl-item-overlays)
                  (overlay-put ov 'face 'isl-match))
              (invalid-regexp nil))
            (setq isl-item-overlays (reverse isl-item-overlays)))
          (if (null isl-item-overlays)
              (progn (setq isl-number-results 0) (goto-char isl-initial-pos))
            (setq isl-last-overlay
                  (isl-closest-overlay isl-initial-pos isl-item-overlays)
                  isl-number-results (length isl-item-overlays))
            (overlay-put isl-last-overlay 'face 'isl-on)
            (isl--set-iterator)
            (goto-char (overlay-end (iterator:next isl-iterator)))
            (setq isl-yank-point (point)))))
      (isl--setup-mode-line))))

(defun isl--setup-mode-line ()
  (let ((style (cl-case isl-search-function
                 (re-search-forward "Regex")
                 (search-forward "Literal")))
        (position (with-current-buffer isl-current-buffer
                     (if (> (point) isl-initial-pos)
                         isl-before-position-string
                       isl-after-position-string)))
        (direction (if (eq isl-direction 'forward)
                       isl-direction-down-string
                     isl-direction-up-string)))
    (setq mode-line-format
          (cond ((string= isl-pattern "")
                 (default-value 'mode-line-format))
                ((zerop isl-number-results)
                 `(" " mode-line-buffer-identification " "
                   (:eval ,(format "No results found for `%s' [%s %s]"
                                   (propertize isl-pattern
                                               'face 'isl-on)
                                   style
                                   direction))
                   " " mode-line-position))
                (t `(" " mode-line-buffer-identification " "
                     (:eval ,(format "[%s] result(s) found for `%s' [%s %s %s]"
                                     (propertize (number-to-string isl-number-results)
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

(defun isl--set-iterator (&optional skip-first)
  (let* ((revlst (if (eq isl-direction 'forward)
                     isl-item-overlays
                   (reverse isl-item-overlays))) 
         (ovlst (append (if skip-first
                            (cdr (memql isl-last-overlay revlst))
                          (memql isl-last-overlay revlst))
                        (butlast revlst
                                 (length (memql isl-last-overlay
                                                revlst))))))
      (setq isl-iterator (iterator:circular ovlst))))

(defun isl-check-input ()
  (with-selected-window (minibuffer-window)
    (let ((input (minibuffer-contents)))
      (when (not (string= input isl-pattern))
        (setq isl-pattern input)
        (isl-update-overlays)))))

(defun isl-read-from-minibuffer (prompt)
  (let (timer
        (cursor-in-echo-area t))
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (setq timer (run-with-idle-timer
                           0.1 'repeat #'isl-check-input)))
          (read-from-minibuffer
           prompt nil isl-map nil 'isl-history (thing-at-point 'symbol t)))
      (cancel-timer timer))))

(defun isl-1 ()
  (setq isl-item-overlays nil
        isl-pattern ""
        isl-current-buffer (current-buffer))
  (condition-case-unless-debug nil
      (unwind-protect
          (isl-read-from-minibuffer "Search: ")
        (isl-delete-overlays)
        (setq mode-line-format (default-value 'mode-line-format)
              isl-yank-point nil
              isl-search-function (default-value 'isl-search-function)))
    (quit (goto-char isl-initial-pos))))

;;;###autoload
(defun isl ()
  (interactive)
  (setq isl-direction 'forward
        isl-initial-pos (point))
  (isl-1))

;;;###autoload
(defun isl-narrow-to-defun ()
  (interactive)
  (save-restriction
    (narrow-to-defun)
    (isl)))

(provide 'isearch-light)

;;; isearch-light.el ends here

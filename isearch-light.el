;;; isearch-light.el --- simple incremental search in current-buffer -*- lexical-binding: t -*- 

;	$Id: isearch-light.el,v 1.7 2021/01/06 16:05:31 thierry Exp thierry $	


;;; Code:

(require 'iterator)

(defvar isl-pattern "")
(defvar isl-current-buffer nil)
(defvar isl-item-overlays nil)
(defvar isl-iterator nil)
(defvar isl-last-overlay nil)
(defvar isl-direction nil)
(defvar isl-initial-pos nil)
(defvar isl-case-fold-search 'smart
  "The `case-fold-search' value.")

(defvar isl-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-z")    'isl-goto-next)
    (define-key map (kbd "M-z")    'isl-goto-prev)
    (define-key map (kbd "<down>") 'isl-goto-next)
    (define-key map (kbd "<up>")   'isl-goto-prev)
    (define-key map (kbd "RET")    'isl-exit-at-point)
    map))


;;; Actions
;;
(defun isl-goto-next-1 ()
  (with-selected-window (get-buffer-window isl-current-buffer)
    (when (overlayp isl-last-overlay)
      (overlay-put isl-last-overlay 'face '(:background "brown")))
    (when isl-iterator
      (let ((ov (iterator:next isl-iterator)))
        (when ov
          (setq isl-last-overlay ov)
          (overlay-put ov 'face '(:background "green"))
          (goto-char (overlay-start ov)))))))

(defun isl-goto-next ()
  (interactive)
  (when (eq isl-direction 'backward)
    (setq isl-direction 'forward)
    (isl--set-iterator)
    (message "Searching forward"))
  (isl-goto-next-1))

(defun isl-goto-prev ()
  (interactive)
  (when (eq isl-direction 'forward)
    (setq isl-direction 'backward)
    (isl--set-iterator)
    (message "Searching backward"))
  (isl-goto-next-1))

(defun isl-exit-at-point ()
  (interactive)
  (with-selected-window (get-buffer-window isl-current-buffer)
    (let ((ov (make-overlay (point-at-bol) (point-at-eol))))
      (overlay-put ov 'face '(:background "red"))
      (sit-for 0.1)
      (delete-overlay ov))
    (exit-minibuffer)))

(defun isl-delete-overlays ()
  (when isl-item-overlays
    (mapc 'delete-overlay isl-item-overlays)
    (setq isl-item-overlays nil)))

(defun isl-display-string-in-mode-line (str)
  (let ((mode-line-format str))
    (force-mode-line-update)
    (sit-for 12))
  (force-mode-line-update))

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
            (while (re-search-forward isl-pattern nil t)
              (setq ov (make-overlay (match-beginning 0) (match-end 0)))
              (push ov isl-item-overlays)
              (overlay-put ov 'face '(:background "brown")))
            (setq isl-item-overlays (reverse isl-item-overlays)))
          (if (null isl-item-overlays)
              (isl-display-string-in-mode-line
               (propertize
                (format " [No matches for `%s']"
                        isl-pattern)
                'face '(:foreground "Gray")))
            (setq isl-last-overlay
                  (isl-closest-overlay isl-initial-pos isl-item-overlays))
            (overlay-put isl-last-overlay 'face '(:background "green"))
            (isl--set-iterator)
            (goto-char (overlay-start (iterator:next isl-iterator)))))))))

(defun isl-closest-overlay (pos overlays)
  "Return closest overlay from POS in OVERLAYS list."
  (cl-loop for ov in overlays
           for ovpos = (overlay-start ov)
           for diff = (if (> pos ovpos) (- pos ovpos) (- ovpos pos))
           collect (cons diff ov) into res
           minimize diff into min
           finally return (cdr (assq min res))))

(defun isl--set-iterator ()
  (let* ((revlst (if (eq isl-direction 'forward)
                     isl-item-overlays
                   (reverse isl-item-overlays))) 
         (ovlst (append (member isl-last-overlay revlst)
                        (butlast revlst
                                 (length (member isl-last-overlay
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
           prompt nil isl-map nil nil (thing-at-point 'symbol)))
      (cancel-timer timer))))

(defun isl-1 ()
  (setq isl-item-overlays nil
        isl-pattern ""
        isl-current-buffer (current-buffer))
  (condition-case-unless-debug nil
      (unwind-protect
          (isl-read-from-minibuffer "search: ")
        (isl-delete-overlays))
    (quit (goto-char isl-initial-pos))))

;;;###autoload
(defun isl ()
  (interactive)
  (setq isl-direction 'forward
        isl-initial-pos (point))
  (isl-1))

(provide 'isearch-light)

;;; isearch-light.el ends here

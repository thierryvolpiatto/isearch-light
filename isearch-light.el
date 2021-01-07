;;; isearch-light.el --- simple incremental search in current-buffer -*- lexical-binding: t -*- 

;	$Id: isearch-light.el,v 1.7 2021/01/06 16:05:31 thierry Exp thierry $	


;;; Code:

(require 'iterator)

(defvar il-search-pattern "")
(defvar il-search-current-buffer nil)
(defvar il-search-item-overlays nil)
(defvar il-search-iterator nil)
(defvar il-search-last-overlay nil)
(defvar il-search-direction nil)

(defvar il-search-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-z") 'il-search-goto-next)
    (define-key map (kbd "M-z") 'il-search-goto-prev)
    (define-key map (kbd "RET") 'il-search-exit-at-point)
    map))


;;; Actions
;;
(defun il-search-goto-next-1 ()
  (with-selected-window (get-buffer-window il-search-current-buffer)
    (when il-search-last-overlay
      (overlay-put il-search-last-overlay 'face '(:background "brown")))
    (when il-search-iterator
      (let ((ov (iterator:next il-search-iterator)))
        (if ov
            (progn
              (setq il-search-last-overlay ov)
              (overlay-put ov 'face '(:background "green"))
              (goto-char (overlay-start ov)))
          (message "no more occurences of %s" il-search-pattern)
          (sit-for 0.5)
          (setq il-search-iterator (iterator:list (reverse il-search-item-overlays)))
          )))))

(defun il-search-goto-next ()
  (interactive)
  (when (eq il-search-direction 'backward)
    (setq il-search-direction 'forward
          il-search-pattern ""
          il-search-last-overlay nil)
    (il-search-check-input))
  (il-search-goto-next-1))

(defun il-search-goto-prev ()
  (interactive)
  (when (eq il-search-direction 'forward)
    (setq il-search-direction 'backward
          il-search-pattern ""
          il-search-last-overlay nil)
    (il-search-check-input))
  (il-search-goto-next-1))

(defun il-search-exit-at-point ()
  (interactive)
  (exit-minibuffer))

(defun il-search-delete-overlays ()
  (when il-search-item-overlays
    (mapc 'delete-overlay il-search-item-overlays)
    (setq il-search-item-overlays nil)))

(defun il-search-update-overlays (direction)
  (with-selected-window (get-buffer-window il-search-current-buffer)
    (il-search-delete-overlays)
    (let (ov
          (fn (cl-case direction
                (forward #'re-search-forward)
                (backward #'re-search-backward))))
      (with-local-quit
        (save-excursion
          (while (funcall fn il-search-pattern nil t)
            (setq ov (make-overlay (match-beginning 0) (match-end 0)))
            (push ov il-search-item-overlays)
            (overlay-put ov 'face '(:background "brown")))
          (setq il-search-iterator (iterator:list (reverse il-search-item-overlays))))))))

(defun il-search-check-input ()
  (let ((input (minibuffer-contents)))
    (when (not (string= input il-search-pattern))
      (setq il-search-pattern input)
      (il-search-update-overlays il-search-direction))))

(defun il-search-read-from-minibuffer (prompt)
  (let (timer)
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (setq timer (run-with-idle-timer
                           0.1 'repeat #'il-search-check-input)))
          (read-from-minibuffer prompt nil il-search-map))
      (cancel-timer timer))))

(defun il-search-1 ()
  (setq il-search-item-overlays nil
        il-search-pattern ""
        il-search-current-buffer (current-buffer))
  (condition-case-unless-debug nil
      (unwind-protect
          (il-search-read-from-minibuffer "test: ")
        (il-search-delete-overlays))
    (quit nil)))

;;;###autoload
(defun il-search-forward ()
  (interactive)
  (setq il-search-direction 'forward)
  (il-search-1))

;;;###autoload
(defun il-search-backward ()
  (interactive)
  (setq il-search-direction 'backward)
  (il-search-1))


(provide 'isearch-light)

;;; isearch-light.el ends here

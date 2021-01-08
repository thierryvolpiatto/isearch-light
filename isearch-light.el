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
(defvar il-initial-pos nil)

(defvar il-search-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-z") 'il-search-goto-next)
    (define-key map (kbd "M-z") 'il-search-goto-prev)
    (define-key map (kbd "<down>") 'il-search-goto-next)
    (define-key map (kbd "<up>") 'il-search-goto-prev)
    (define-key map (kbd "RET") 'il-search-exit-at-point)
    map))


;;; Actions
;;
(defun il-search-goto-next-1 ()
  (with-selected-window (get-buffer-window il-search-current-buffer)
    (when (overlayp il-search-last-overlay)
      (overlay-put il-search-last-overlay 'face '(:background "brown")))
    (when il-search-iterator
      (let ((ov (iterator:next il-search-iterator)))
        (when ov
          (setq il-search-last-overlay ov)
          (overlay-put ov 'face '(:background "green"))
          (goto-char (overlay-start ov)))))))

(defun il-search-goto-next ()
  (interactive)
  (when (eq il-search-direction 'backward)
    (setq il-search-direction 'forward)
    (il-search--set-iterator)
    (message "Changing direction"))
  (il-search-goto-next-1))

(defun il-search-goto-prev ()
  (interactive)
  (when (eq il-search-direction 'forward)
    (setq il-search-direction 'backward)
    (il-search--set-iterator)
    (message "Changing direction"))
  (il-search-goto-next-1))

(defun il-search-exit-at-point ()
  (interactive)
  (with-selected-window (get-buffer-window il-search-current-buffer)
    (let ((ov (make-overlay (point-at-bol) (point-at-eol))))
      (overlay-put ov 'face '(:background "red"))
      (sit-for 0.1)
      (delete-overlay ov))
    (exit-minibuffer)))

(defun il-search-delete-overlays ()
  (when il-search-item-overlays
    (mapc 'delete-overlay il-search-item-overlays)
    (setq il-search-item-overlays nil)))

(defun il-search-update-overlays ()
  (with-selected-window (get-buffer-window il-search-current-buffer)
    (il-search-delete-overlays)
    (let (ov)
      (while-no-input
        (unless (string= il-search-pattern "")
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward il-search-pattern nil t)
              (setq ov (make-overlay (match-beginning 0) (match-end 0)))
              (push ov il-search-item-overlays)
              (overlay-put ov 'face '(:background "brown")))
            (setq il-search-item-overlays (reverse il-search-item-overlays)))
          (if (null il-search-item-overlays)
              (progn
                (let ((mode-line-format (propertize
                                         " [No matches]"
                                         'face '(:foreground "DimGray"))))
                  (force-mode-line-update)
                  (sit-for 12))
                (force-mode-line-update))
            (setq il-search-last-overlay
                  (il-search-closest-overlay (point) il-search-item-overlays))
            (goto-char (overlay-start il-search-last-overlay))
            (il-search--set-iterator)))))))

(defun il-search-closest-overlay (pos overlays)
  "Return closest overlay from POS in OVERLAYS list."
  (cl-loop for ov in overlays
           for ovpos = (overlay-start ov)
           for diff = (if (> pos ovpos) (- pos ovpos) (- ovpos pos))
           collect (cons diff ov) into res
           minimize diff into min
           finally return (cdr (assq min res))))

(defun il-search--set-iterator ()
  (let* ((revlst (if (eq il-search-direction 'forward)
                     il-search-item-overlays
                   (reverse il-search-item-overlays))) 
         (ovlst (append (member il-search-last-overlay revlst)
                        (butlast revlst
                                 (length (member il-search-last-overlay
                                                 revlst))))))
      (setq il-search-iterator (iterator:circular ovlst))))

(defun il-search-check-input ()
  (with-selected-window (minibuffer-window)
    (let ((input (minibuffer-contents)))
      (when (not (string= input il-search-pattern))
        (setq il-search-pattern input)
        (il-search-update-overlays)))))

(defun il-search-read-from-minibuffer (prompt)
  (let (timer
        (cursor-in-echo-area t))
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (setq timer (run-with-idle-timer
                           0.1 'repeat #'il-search-check-input)))
          (read-from-minibuffer
           prompt nil il-search-map nil nil (thing-at-point 'symbol)))
      (cancel-timer timer))))

(defun il-search-1 ()
  (setq il-search-item-overlays nil
        il-search-pattern ""
        il-search-current-buffer (current-buffer))
  (condition-case-unless-debug nil
      (unwind-protect
          (il-search-read-from-minibuffer "search: ")
        (il-search-delete-overlays))
    (quit (goto-char il-initial-pos))))

;;;###autoload
(defun il-search ()
  (interactive)
  (setq il-search-direction 'forward
        il-initial-pos (point))
  (il-search-1))

(provide 'isearch-light)

;;; isearch-light.el ends here

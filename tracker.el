;;;; tracker.el --- Tracker mode for emacsBPM: 120 Pat: 000/001S Step: 000/016
;;;; by defaultxr

;; (let ((inhibit-read-only t))
;;   (remove-list-of-text-properties (point-min)
;;                                   (save-excursion
;;                                     (goto-char (point-min))
;;                                     (search-forward (string ?\n ?\n)))
;;                                 '(read-only intangible)))

;; (tracker-start)
;; (tracker-stop)

;; 000 (message "YO")
;; 001 (message "1")
;; 002 (message "2")
;; 003 (if (eq 2 3)
;;         (message "Howdy!")
;;       (message "NO WAY"))
;; 004 (message "NAH")
;; 005*;(tracker-stop)
;; 006 (message "FOO")
;; 007*(
;; 008*(
;; 009 (message "HEY")
;; 010*(
;; 011*(
;; 012*(
;; 013*(
;; 014*(
;; 015 (message "sup")

;;;; global variables for tracker-mode
    
(defvar tracker-mode-hook nil)
(defvar tracker-running nil)
(defvar tracker-current-pattern 0)

;;;; keymap for tracker-mode

(defvar tracker-mode-map
  (let ((map (make-sparse-keymap "Tracker-mode")))
    (define-key map (kbd "<M-down>") 'tracker-decrease-number)
    (define-key map (kbd "<M-up>") 'tracker-increase-number)
    ;(define-key map (kbd "C-a") 'tracker-back-to-indent)
    (define-key map (kbd "C-c n") 'tracker-next-pattern)
    (define-key map (kbd "C-c p") 'tracker-previous-pattern)
    map)
  "Keymap for `tracker-mode'.")

;;;; internal tracker commands

(defun tracker-highlight-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end))) 
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.5) nil 'delete-overlay overlay)))

(defun tracker-point-to-step (step)
  (goto-char (point-min))
  (search-forward-regexp (format "^%03d." step) nil t))

(defun tracker-get-step-region (step)
  "Returns the boundaries of the elisp code for one step of the pattern."
  (save-excursion
    (let* ((step-start (tracker-point-to-step step))
           (start (when (and step-start (eq (char-after) 40))
                    step-start))
           (end (when start
                  (goto-char start)
                  (condition-case nil
                      (progn
                        (forward-sexp)
                        (point))
                    (error nil)))))
      (when (and start end)
        (list start end)))))

(defun tracker-mark-step (step type)
  (save-excursion
    (when (tracker-point-to-step step)
      (backward-char)
      (delete-char 1)
      (insert (case type
                ('error "*")
                ('correct " "))))))

(defun tracker-eval-step (step)
  "Highlights and evaluates the code in one step of the tracker."
  (let* ((region (tracker-get-step-region step))
         (start (car region))
         (end (cadr region))
         (elisp (if (and start end)
                    (read (buffer-substring start end))
                  nil)))
    (if (and start end)
        (progn
          (tracker-mark-step step 'correct)
          (tracker-highlight-region start end))
      (tracker-mark-step step 'error))
    (eval elisp)))

(defun tracker-loop (step)
  (when tracker-running
    (progn (tracker-eval-step step)
           (run-with-timer 0.2 nil 'tracker-loop (mod (1+ step) 16)))))

;; (defun tracker-modify-fixed-width-field

;;;; code to create the tracker interface & fields within the buffer

(defun tracker-make-interface-element (start end &optional name)
  "Make a block of text read-only & intangible."
  (add-text-properties start end '(intangible (or name t) read-only (or name t) rear-nonsticky t)))

(defun tracker-make-header (&optional steps)
  "Write a header for the tracker and apply text properties to it."
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at "^TRACKER: ") ; title
      (insert (concat "TRACKER: Untitled" (string ?\n))))
    (goto-char (point-min))
    (next-line)
    (unless (looking-at "^BPM: [0-9]{3} ") ; bpm
      (insert "BPM: 120 "))
    (unless (looking-at "Pat: [0-9]{3}/[0-9]{3}[PLS] ") ; pattern
      (insert "Pat: 000/001S "))
    (unless (looking-at "Step: [0-9]{3}/[0-9]{3}$") ; step
      (insert (format "Step: 000/%03d%s" (or steps 16) (string ?\n ?\n))))
    (goto-char (point-min))
    (flet ((make-string-into-interface (string &optional name)
                                       (let ((item (search-forward string)))
                                         (tracker-make-interface-element (- item (length string)) item name))))
      (make-string-into-interface "TRACKER: " 'tracker-mark)
      (add-text-properties (point) (1- (search-forward (string ?\n))) '(field title)) ; make the title into a field
      (backward-char)
      (make-string-into-interface (concat (string ?\n) "BPM: ") 'bpm-mark)
      (add-text-properties (point) (+ (point) 3) '(field bpm)) ; bpm field
      (make-string-into-interface " Pat: " 'pat-mark)
      (add-text-properties (point) (+ (point) 3) '(field cpat)) ; current pattern
      (make-string-into-interface "/" 'slash-mark)
      (add-text-properties (point) (+ (point) 3) '(field tpat)) ; total patterns
      (tracker-make-interface-element (+ (point) 3) (+ (point) 5) 'status)
      (make-string-into-interface "Step: " 'step-mark)
      (add-text-properties (point) (+ (point) 3) '(field cstep)) ; current step
      (make-string-into-interface "/" 'slash-mark-2)
      (add-text-properties (point) (+ (point) 3) '(field tstep)))) ; total number of steps
  (goto-char (point-min))
  (dotimes (foo 3)
    (next-line)))
    
(defun tracker-make-pattern (number &optional steps)
  "Write the default steps template for the tracker."
  (save-excursion
    (insert (propertize
             (concat "Pattern " (number-to-string number) (string ?\n)
                     (let ((result ""))
                       (dotimes (step (or steps 16) result)
                         (setf result (concat result (format "%03d %s" step (string ?\n)))))))
             'invisible (intern (concat "tracker-pattern-" (number-to-string number)))))
    (add-to-invisibility-spec (intern (concat "tracker-pattern-" (number-to-string number))))))o

(defun tracker-view-pattern (number)
  "Switches the view to the selected pattern."
  (interactive "NPattern: ")
  (add-to-invisibility-spec tracker-current-pattern)
  (remove-from-invisibility-spec (intern (concat "tracker-pattern-" (number-to-string number))))
  (setf tracker-current-pattern number))

(defun tracker-next-pattern ()
  "Switches the view to the next pattern."
  (interactive)
  (tracker-view-pattern (+ 1 tracker-current-pattern)))

(defun tracker-previous-pattern ()
  "Switches the view to the previous pattern."
  (interactive)
  (tracker-view-pattern (- tracker-current-pattern 1)))

(defun tracker-write-template ()
  "Writes the default template for the tracker."
  (interactive)
  (tracker-make-header)
  (setf buffer-invisibility-spec 'tracker-pattern-0)
  (tracker-make-pattern 0)
  (remove-from-invisibility-spec 'tracker-pattern-0))

;;;; tracker main functions

(defun tracker-start ()
  "Starts the tracker."
  (interactive)
  (setf tracker-running t)
  (tracker-loop 0))

(defun tracker-stop ()
  "Stops the tracker."
  (interactive)
  (setf tracker-running nil))

;;;; convenience functions

;;; change numbers
(defun tracker-change-number (arg)
  "Increase or decrease a number under the point."
  (interactive "p")
  (save-excursion
    (search-backward-regexp "[^0-9-]")
    (forward-char)
    (let* ((start (point))
           (end (1- (save-excursion (search-forward-regexp "[^0-9-]"))))
           (string (buffer-substring start end)))
      (when (not (eq 0 (- end start)))
        (delete-char (- end start))
        (insert (number-to-string (+ (or arg 1) (string-to-number string))))))))

(defun tracker-increase-number (arg)
  "Increase a number under the point."
  (interactive "p")
  (tracker-change-number (or arg 1)))

(defun tracker-decrease-number (arg)
  "Decrease a number under the point."
  (interactive "p")
  (tracker-change-number (- (or arg 1))))

                                        ;(defun tracker-back-to-indent ()

;;;; initialize & register the mode with emacs

(defun tracker-mode ()
  "Major mode for using emacs as a music tracker."
  (interactive)
  (setq major-mode 'tracker-mode)
  (setq mode-name "Tracker")
  (use-local-map tracker-mode-map)
  (tracker-write-template)
  (run-hooks 'tracker-mode-hook))

(define-derived-mode tracker-mode lisp-mode "Tracker"
  "Major mode for using emacs as a music tracker.")

(provide 'tracker-mode)

;;;; tracker.el --- Tracker mode for emacs
;;;; by defaultxr

;; (let ((inhibit-read-only t))
;;   (remove-list-of-text-properties (point-min)
;;                                   (save-excursion
;;                                     (goto-char (point-min))
;;                                     (search-forward (string ?\n ?\n)))
;;                                 '(read-only intangible)))

;; (tracker-start)
;; (tracker-stop)

;;;; global variables for tracker-mode

(defvar tracker-mode-hook nil
  "Hook to run when tracker mode starts.")
(defvar tracker-running nil
  "Whether the tracker is currently playing through the sequence.")
(defvar tracker-current-pattern 0
  "The current pattern being viewed.")
(defvar tracker-playing-pattern 0
  "The pattern currently being played by the tracker.")
(defvar tracker-buffer nil
  "The buffer that the tracker is running in.")
(defvar tracker-number-of-patterns 0
  "The total number of patterns in the tracker's buffer.")
(defvar tracker-latched nil
  "Whether or not the tracker is latched on the current pattern.")

;;;; keymap for tracker-mode

(defvar tracker-mode-map
  (let ((map (make-sparse-keymap "Tracker-mode")))
    (define-key map (kbd "<M-down>") 'tracker-decrease-number)
    (define-key map (kbd "<M-up>") 'tracker-increase-number)
    ;; (define-key map (kbd "C-a") 'tracker-back-to-indent)
    (define-key map (kbd "C-c n") 'tracker-next-pattern)
    (define-key map (kbd "C-c p") 'tracker-previous-pattern)
    ;; (define-key map (kbd "M-n") 'tracker-next-field) ; goes to next step in the pattern, etc
    ;; (define-key map (kbd "M-p") 'tracker-previous-field)
    (define-key map (kbd "C-c s") 'tracker-start-stop)
    (define-key map (kbd "C-c w") 'tracker-write-template) ; prolly do this automatically when tracker-mode starts...
    ;; (define-key map (kbd "C-c l") 'tracker-latch-pattern) ; latch loops the current pattern vs. proceeding to next
    map)
  "Keymap for `tracker-mode'.")

;;;; internal tracker commands

(defun tracker-highlight-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end))) 
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.5) nil 'delete-overlay overlay)))

(defun tracker-point-to-step (step &optional pattern)
  (goto-char (point-min))
  (search-forward-regexp (format "^Pattern %d\n" (or pattern tracker-current-pattern)) nil t)
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
                ('error "E")
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

(defun tracker-find-next-non-numeral-character ()
  "Return the position of the next non-numeral character after the point."
  (save-excursion
    (search-forward-regexp "[^0-9]" nil t)))

(defun tracker-number-of-numbers-at-point ()
  "Return the number of numbers at the point."
  (- (tracker-find-next-non-numeral-character) (1+ (point))))

(defun tracker-make-header (&optional steps)
  "Write a header for the tracker and apply text properties to it."
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at "^TRACKER: ") ; title
      (insert (concat "TRACKER: Untitled" (string ?\n))))
    (goto-line 2)
    (unless (looking-at "^BPM: [0-9]+ Pat: [0-9]+/[0-9]+/[0-9]+ [PLS]+ Step: [0-9]+/[0-9]+/[0-9]+")
      (insert (concat "BPM: 120 Pat: 0/0/1 S Step: 0/0/16" (string ?\n ?\n)))) ; pat and step are in the format 'point/tracker playing/total'
    (goto-char (point-min))
    (flet ((make-string-into-interface (string &optional name)
                                       (let ((item (search-forward string)))
                                         (tracker-make-interface-element (- item (length string)) item name))))
      (make-string-into-interface "TRACKER: " 'tracker-mark)
      (add-text-properties (point) (1- (search-forward (string ?\n))) '(field title)) ; make the title into a field
      (backward-char)
      (make-string-into-interface "BPM: " 'bpm-mark)
      (add-text-properties (point) (tracker-find-next-non-numeral-character) '(field bpm)) ; bpm field
      (make-string-into-interface " Pat: " 'pat-mark)
      (add-text-properties (point) (tracker-find-next-non-numeral-character) '(field vpat)) ; pattern being viewed
      (make-string-into-interface "/" 'slash-mark)
      (add-text-properties (point) (tracker-find-next-non-numeral-character) '(field ppat)) ; pattern being played
      (make-string-into-interface "/" 'slash-mark-2)
      (add-text-properties (point) (tracker-find-next-non-numeral-character) '(field tpat)) ; total patterns
      (tracker-make-interface-element (1- (search-forward " ")) (search-forward " ") 'status)
      (make-string-into-interface "Step: " 'step-mark)
      (add-text-properties (point) (tracker-find-next-non-numeral-character) '(field vstep)) ; current step the point is on
      (make-string-into-interface "/" 'slash-mark-3)
      (add-text-properties (point) (tracker-find-next-non-numeral-character) '(field pstep)) ; current step being played
      (make-string-into-interface "/" 'slash-mark-4)
      (add-text-properties (point) (tracker-find-next-non-numeral-character) '(field tstep)))) ; total number of steps
  (goto-char (point-min))
  (dotimes (foo 3)
    (next-line)))

(defun tracker-goto-bpm-field ()
  "Place the point at the beginning of the BPM field."
  (goto-char (point-min))
  (search-forward "BPM: "))

(defun tracker-goto-currently-viewed-pattern ()
  "Place the point at the beginning of the current pattern field."
  (goto-char (point-min))
  (search-forward " Pat: "))

(defun tracker-goto-currently-playing-pattern ()
  "Place the point at the beginning of the currently playing pattern field."
  (tracker-goto-currently-viewed-pattern)
  (search-forward "/"))

(defun tracker-goto-total-number-of-patterns ()
  "Place the point at the total number of patterns field."
  (tracker-goto-currently-playing-pattern)
  (search-forward "/"))

(defun tracker-set-status (status)
  "Changes the status indicator."
  (save-excursion
    (let ((inhibit-read-only t)
          (inhibit-point-motion-hooks t))
      (tracker-goto-total-number-of-patterns)
      (goto-char (1- (tracker-find-next-non-numeral-character)))
      (delete-char (- (- (save-excursion (search-forward "Step: ")) 6) (point)))
      (insert (propertize (concat " " status " ") 'intangible 'status 'read-only 'status 'rear-nonsticky t)))))

(defun tracker-update-status ()
  "Sets the status indicator to the correct values."
  (tracker-set-status (concat (if tracker-running "P" "S") (when tracker-latched "L"))))

(defun tracker-make-pattern (number &optional steps)
  "Write a specific pattern if it does not exist. You should probably call `tracker-make-new-pattern' instead of this."
  (save-excursion
    (goto-char (point-min))
    (when (not (search-forward (concat "Pattern " number (string ?\n)) nil t))
      (insert (propertize
               (concat "Pattern " (number-to-string number) (string ?\n)
                       (let ((result ""))
                         (dotimes (step (or steps 16) result)
                           (setf result (concat result (format "%03d %s" step (string ?\n)))))))
               'invisible (intern (concat "tracker-pattern-" (number-to-string number)))))
      (incf tracker-number-of-patterns))
    (add-to-invisibility-spec (intern (concat "tracker-pattern-" (number-to-string number))))))

(defun tracker-make-new-pattern (&optional steps)
  "Make a new pattern after the last."
  (interactive "NSteps: ")
  (tracker-make-pattern tracker-number-of-patterns (or steps 16)))

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

(defun tracker-start-stop ()
  "Starts or stops the tracker."
  (interactive)
  (if tracker-running
      (tracker-stop)
    (tracker-start)))

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

;; (defun tracker-back-to-indent ()

;;;; initialize & register the mode with emacs

(defun tracker-mode ()
  "Major mode for using emacs as a music tracker."
  (interactive)
  (setq major-mode 'tracker-mode)
  (setq mode-name "Tracker")
  (use-local-map tracker-mode-map)
  (setf tracker-buffer (current-buffer))
  (tracker-write-template)
  (run-hooks 'tracker-mode-hook))

(define-derived-mode tracker-mode emacs-lisp-mode "Tracker"
  "Major mode for using emacs as a music tracker.")

(provide 'tracker-mode)

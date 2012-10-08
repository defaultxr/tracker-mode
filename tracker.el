;;;; tracker.el --- Tracker mode for emacs
;;;; by defaultxr

;;;; global variables

;; FIX: turn the marked variables into functions

(defvar tracker-mode-hook nil
  "Hook to run when tracker mode starts.")
(defvar tracker-running nil ;; FIX
  "Whether the tracker is currently playing through the sequence.")
(defvar tracker-current-pattern 0 ;; FIX
  "The current pattern being viewed.")
(defvar tracker-playing-pattern 0 ;; FIX
  "The pattern currently being played by the tracker.")
(defvar tracker-buffer nil
  "The buffer that the tracker is running in.")
(defvar tracker-number-of-patterns 0 ;; FIX
  "The total number of patterns in the tracker's buffer.")
(defvar tracker-latched nil ;; FIX
  "Whether or not the tracker is latched on the current pattern.")

;;;; keymap

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
    ;; (define-key map (kbd "C-c w") 'tracker-write-template) ; prolly do this automatically when tracker-mode starts...
    (define-key map (kbd "C-c l") 'tracker-latch-pattern) ; latch loops the current pattern vs. proceeding to next
    (define-key map (kbd "C-c i") 'tracker-make-new-pattern) ; i for "Insert pattern"
    map)
  "Keymap for `tracker-mode'.")

;;;; internal tracker commands

(defun tracker-highlight-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end))) 
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.5) nil 'delete-overlay overlay)))

;;;; functions to move the point around

(defun tracker-goto-step (step)
  "Places the point at the specified step in the pattern."
  (goto-char (point-min))
  (search-forward-regexp (format "^Pattern [0-9]+:\n") nil t)
  (when (search-forward-regexp (format "^%03d. " step) nil t)
    (backward-char 5)
    (point)))

(defun tracker-goto-bpm ()
  "Places the point at the beginning of the BPM field."
  (goto-char (point-min))
  (search-forward "BPM: "))

(defun tracker-goto-currently-viewed-pattern ()
  "Places the point at the beginning of the current pattern field."
  (goto-char (point-min))
  (search-forward " Pat: "))

(defun tracker-goto-currently-playing-pattern ()
  "Places the point at the beginning of the currently playing pattern field."
  (tracker-goto-currently-viewed-pattern)
  (search-forward "/"))

(defun tracker-goto-total-number-of-patterns ()
  "Places the point at the total number of patterns field."
  (tracker-goto-currently-playing-pattern)
  (search-forward "/"))

(defun tracker-goto-status ()
  "Places the point at the status field."
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (goto-char (point-min))
    (goto-char (progn (search-forward "Step: ")
                      (1+ (search-backward " " nil nil 3))))))

(defun tracker-goto-currently-viewed-step ()
  "Places the point at the beginning of the current step field."
  (goto-char (point-min))
  (search-forward " Step: "))

(defun tracker-goto-currently-playing-step ()
  "Places the point at the beginning of the currently playing step field."
  (tracker-goto-currently-viewed-step)
  (search-forward "/"))

(defun tracker-goto-total-number-of-steps ()
  "Places the point at the total number of steps field."
  (tracker-goto-currently-playing-step)
  (search-forward "/"))

;;;; functions to get data

;; (defun tracker-status ()
;;   "Returns the tracker status field."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let ((status-begin (progn (search-forward "Step: ")
;;                                (search-backward " " nil nil 2))))
;;       (buffer-substring status-begin (search-forward " ")))))

(defun tracker-status ()
  "Returns the tracker status field."
  (save-excursion
    (tracker-goto-status)
    (buffer-substring (point) (1- (search-forward " ")))))

;; (defun tracker-

;;;; code to create the tracker interface & fields within the buffer

(defun tracker-make-interface-element (start end &optional name)
  "Make a block of text read-only & intangible."
  (add-text-properties start end '(intangible (or name t) read-only (or name t) rear-nonsticky t)))

(defun tracker-find-next-non-numeral-character ()
  "Return the position of the next non-numeral character after the point."
  ;; FIX: get rid of this function
  (save-excursion
    (search-forward-regexp "[^0-9]" nil t)))

(defun tracker-chars-until (string)
  "Counts the number of characters until the first occurance of STRING."
  (1- (- (save-excursion (search-forward string)) (point))))

(defun tracker-make-header (&optional steps)
  "Write a header for the tracker and apply text properties to it."
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at "^TRACKER: ") ; title
      (insert (concat "TRACKER: Untitled" (string ?\n))))
    (goto-line 2)
    (unless (looking-at "^BPM: [0-9]+ Pat: [0-9]+/[0-9]+/[0-9]+ [PLSF]+ Step: [0-9]+/[0-9]+/[0-9]+")
      (insert (concat "BPM: 120 Pat: 0/0/1 S Step: 0/0/16" (string ?\n ?\n)))) ; pat and step are in the format 'point/tracker playing/total'
    (goto-char (point-min))
    (flet ((make-string-into-interface (string &optional name)
                                       (let ((item (search-forward string)))
                                         (tracker-make-interface-element (- item (length string)) item name))))
      (make-string-into-interface "TRACKER: " 'tracker-mark)
      (backward-char)
      (make-string-into-interface "BPM: " 'bpm-mark)
      (make-string-into-interface " Pat: " 'pat-mark)
      (make-string-into-interface "/" 'slash-mark)
      (make-string-into-interface "/" 'slash-mark-2)
      (tracker-make-interface-element (1- (search-forward " ")) (search-forward " ") 'status)
      (make-string-into-interface "Step: " 'step-mark)
      (make-string-into-interface "/" 'slash-mark-3)
      (make-string-into-interface "/" 'slash-mark-4)))
  (goto-char (point-min))
  (dotimes (foo 3)
    (next-line)))

(defun tracker-get-bpm ()
  "Returns the BPM."
  (save-excursion
    (string-to-number (buffer-substring (tracker-goto-bpm) (1- (search-forward " "))))))

(defun tracker-set-status (status)
  "Changes the status indicator."
  (save-excursion
    (let ((inhibit-read-only t)
          (inhibit-point-motion-hooks t))
      (tracker-goto-status)
      (delete-char (- (- (save-excursion (search-forward "Step: ")) 6) (point)))
      (insert (propertize (concat status " ") 'intangible 'status 'read-only 'status 'rear-nonsticky t)))))

(defun tracker-update-status ()
  "Sets the status indicator to the correct values."
  (tracker-set-status (concat (if tracker-running "P" "S") (when tracker-latched "L"))))

(defun tracker-count-patterns ()
  "Counts the patterns in the current buffer and updates `tracker-number-of-patterns' accordingly."
  (save-excursion
    (goto-char (point-min))
    (setf tracker-number-of-patterns (count-matches "^Pattern [0-9]+:$"))
    (tracker-goto-total-number-of-patterns)
    (delete-char (tracker-chars-until " "))
    (insert (number-to-string tracker-number-of-patterns))))

(defun tracker-make-pattern (number steps)
  "Write a specific pattern if it does not exist. You should probably call `tracker-make-new-pattern' instead of this."
  (save-excursion
    (goto-char (point-min))
    (when (not (search-forward (concat "Pattern " (number-to-string number) ":\n") nil t))
      (goto-char (save-excursion
                   (goto-char (point-min))
                   (let ((res (search-forward "Scratch:\n\n" nil t)))
                     (if res
                         (- res 10)
                       (point-max)))))
      (insert (propertize
               (concat "Pattern " (number-to-string number) ":\n"
                       (let ((result ""))
                         (dotimes (step steps result)
                           (setf result (concat result (format "%03d  %s" step (string ?\n)))))
                         result)
                       (string ?\n ?\n))
               'invisible (intern (concat "tracker-pattern-" (number-to-string number)))))
      (tracker-count-patterns)
      (add-to-invisibility-spec (intern (concat "tracker-pattern-" (number-to-string number)))))))

;;;; main loop and associated stuff

(defun tracker-mark-step (step type)
  (save-excursion
    (when (tracker-goto-step step)
      (forward-char 3)
      (delete-char 1)
      (insert (case type
                ('error "E")
                ('correct " "))))))

(defun tracker-step (step pattern)
  "Highlights and evaluates the code in one step of the tracker."
  (save-excursion
    (tracker-goto-currently-playing-step)
    (delete-char (tracker-chars-until "/"))
    (insert (number-to-string step))
    (tracker-goto-currently-playing-pattern)
    (delete-char (tracker-chars-until "/"))
    (insert (number-to-string pattern))
    (let ((old-pat tracker-current-pattern))
      (tracker-view-pattern pattern)
      (when (tracker-goto-step step)
        (forward-char 5)
        (let ((result (condition-case nil
                          (save-excursion
                            (let ((start (point)))
                              (when (looking-at "(")
                                (forward-sexp)
                                (point))))
                        (error nil)))
              (delay (tracker-next-step-delay (tracker-get-bpm)))
              (bol (save-excursion (beginning-of-line) (point))))
          (if result
              (progn ;; no error upon reading
                (tracker-mark-step step 'correct)
                (tracker-highlight-region bol result delay)
                (let ((elisp (read (buffer-substring (point) result))))
                  (eval elisp)
                  ;; TODO: save a copy of this one in case it stops working
                  ))
            (progn ;; error while reading
              (tracker-mark-step step 'error)
              (tracker-highlight-region bol (point) delay)
              ;; TODO: try to reload last working one
              ))))
      (tracker-view-pattern old-pat))))

(defun tracker-loop (step pattern)
  "The main loop of the tracker."
  (when tracker-running
    (save-excursion
      (set-buffer tracker-buffer)
      (progn (tracker-step step pattern)
             (let ((delay (tracker-next-step-delay (tracker-get-bpm))))
               (run-with-timer delay nil 'tracker-loop (mod (1+ step) 16) pattern))))))

(defun tracker-next-step-delay (bpm)
  "Convert a BPM into the interval between each 16th note."
  (/ (/ 60 (float bpm)) 4))

;;;; interactive commands

(defun tracker-make-new-pattern (&optional steps)
  "Make a new pattern after the last."
  (interactive "NSteps: ") ; FIX: allow default number of 16
  (tracker-make-pattern tracker-number-of-patterns (or steps 16)))

(defun tracker-view-pattern (number)
  "Switches the view to the selected pattern."
  (interactive "NPattern: ")
  (save-excursion
    (goto-char (point-min))
    (let ((old-spec buffer-invisibility-spec))
      (setf buffer-invisibility-spec nil)
      (let ((str (concat "Pattern " (number-to-string tracker-current-pattern) ":\n"))
            (end (save-excursion
                   (if (search-forward (concat "Pattern " (number-to-string (1+ tracker-current-pattern)) ":\n") nil t)
                       (progn (backward-char 2)
                              (beginning-of-line)
                              (point))
                     (progn (search-forward "Scratch:\n")
                            (backward-char 2)
                            (beginning-of-line)
                            (point))))))
        (add-text-properties (- (search-forward str) (length str)) end
                             (list 'invisible (intern (concat "tracker-pattern-" (number-to-string tracker-current-pattern))))))
      (setf buffer-invisibility-spec old-spec)))
  (let ((number (max (min number (1- tracker-number-of-patterns)) 0)))
    (add-to-invisibility-spec (intern (concat "tracker-pattern-" (number-to-string tracker-current-pattern))))
    (remove-from-invisibility-spec (intern (concat "tracker-pattern-" (number-to-string number))))
    (setf tracker-current-pattern number)
    (save-excursion
      (tracker-goto-currently-viewed-pattern)
      (delete-char (tracker-chars-until "/"))
      (insert (number-to-string number)))))

(defun tracker-next-pattern ()
  "Switches the view to the next pattern."
  (interactive)
  (tracker-view-pattern (1+ tracker-current-pattern)))

(defun tracker-previous-pattern ()
  "Switches the view to the previous pattern."
  (interactive)
  (tracker-view-pattern (1- tracker-current-pattern)))

(defun tracker-write-template ()
  "Writes the default template for the tracker."
  (interactive)
  (tracker-make-header)
  (setf buffer-invisibility-spec nil)
  (tracker-make-new-pattern)
  (remove-from-invisibility-spec 'tracker-pattern-0)
  (goto-char (point-max))
  (insert "Scratch:\n\n"))

(defun tracker-latch-pattern ()
  "Toggles the latch."
  (interactive)
  (setf tracker-latched (not tracker-latched))
  (tracker-update-status))

(defun tracker-start ()
  "Starts the tracker."
  (interactive)
  (setf tracker-running t)
  (tracker-update-status)
  (tracker-loop 0 tracker-current-pattern))

(defun tracker-stop ()
  "Stops the tracker."
  (interactive)
  (setf tracker-running nil)
  (tracker-update-status))

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

;; (defun tracker-back-to-indent () ; FIX

;;;; initialize & register the mode with emacs

(define-derived-mode tracker-mode emacs-lisp-mode "Tracker"
  "Major mode for using emacs as a music tracker.")

(defun tracker-mode ()
  "Major mode for using emacs as a music tracker."
  (interactive)
  (when (save-excursion
          (goto-char (point-min))
          (not (or (looking-at "^TRACKER: ")
                   (= (point-max) (point-min)))))
    (switch-to-buffer (get-buffer-create "*Tracker*")))
  (setq major-mode 'tracker-mode)
  (setq mode-name "Tracker")
  (use-local-map tracker-mode-map)
  (setf tracker-buffer (current-buffer))
  (tracker-write-template)
  (run-hooks 'tracker-mode-hook))

(provide 'tracker-mode)

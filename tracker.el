;;;; tracker.el --- Tracker mode for emacs
;;;; by defaultxr

;;;; load stuff

(require 'osc)

;;;; global variables

(defvar tracker-mode-hook nil
  "Hook to run when tracker mode starts.")

(defvar tracker-buffer nil
  "The buffer that the tracker is running in.")

(defvar tracker-osc-client (osc-make-client "localhost" 57110)
  "OSC Client for the tracker.")

;;;; osc.el doesn't seem to work...

;; (setq my-client (osc-make-client "localhost" 57110))
;; (osc-send-message my-client "/s_new" "default" 3000 0 1)
;; (featurep 'make-network-process 'host)
;; (osc-send-message tracker-osc-client "/quit")
;; (setq my-server (osc-make-server "localhost" 7770
;;                                  (lambda (path &rest args)
;;                                    (message "OSC %s: %S" path args))))
;; (setq mclient (osc-make-client "localhost" 7772))
;; (osc-send-message mclient "/hi" -2)
;; (process-contact my-client)
;; (delete-process mclient)

;;;;

;;;; keymap

(defvar tracker-mode-map
  (let ((map (make-sparse-keymap "Tracker-mode")))
    (define-key map (kbd "<M-down>") 'tracker-decrease-number)
    (define-key map (kbd "<M-up>") 'tracker-increase-number)
    (define-key map (kbd "C-a") 'tracker-back-to-indent)
    (define-key map (kbd "C-c n") 'tracker-next-pattern)
    (define-key map (kbd "C-c C-n") 'tracker-next-pattern)
    (define-key map (kbd "C-c p") 'tracker-previous-pattern)
    (define-key map (kbd "C-c C-p") 'tracker-previous-pattern)
    ;; (define-key map (kbd "M-n") 'tracker-next-field) ; goes to next step in the pattern, etc
    ;; (define-key map (kbd "M-p") 'tracker-previous-field)
    (define-key map (kbd "C-c s") 'tracker-start-stop)
    (define-key map (kbd "C-c C-s") 'tracker-start-stop)
    (define-key map (kbd "C-c l") 'tracker-latch-pattern) ; latch loops the current pattern vs. proceeding to next
    (define-key map (kbd "C-c C-l") 'tracker-latch-pattern)
    (define-key map (kbd "C-c i") 'tracker-make-new-pattern) ; i for "Insert pattern"
    (define-key map (kbd "C-c C-i") 'tracker-make-new-pattern)
    (define-key map (kbd "M-g b") 'tracker-goto-bpm)
    (define-key map (kbd "M-g s") 'tracker-goto-step)
    (define-key map (kbd "M-g t") 'tracker-goto-title)
    (define-key map (kbd "M-g p") 'tracker-view-pattern) ; this overrides the original M-g p action, but i don't think you'll use it in tracker mode anyway... (?)
    map)
  "Keymap for `tracker-mode'.")

;;;; internal tracker commands

(defun tracker-highlight-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (set-buffer tracker-buffer)
  (let ((overlay (make-overlay start end))) 
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.5) nil 'delete-overlay overlay)))

;;;; functions to move the point around

(defun tracker-goto-title ()
  "Places the point at the end of the title field."
  (interactive)
  (set-buffer tracker-buffer)
  (goto-char (point-min))
  (search-forward "TRACKER: ")
  (end-of-line))

(defun tracker-goto-step (step)
  "Places the point at the specified step in the current pattern."
  (interactive "NStep: ")
  (set-buffer tracker-buffer)
  (goto-char (point-min))
  (search-forward-regexp (concat "Pattern " (number-to-string (tracker-current-pattern)) ":$"))
  (when (search-forward-regexp (format "^%03d. " step))
    (backward-char 5)
    (point)))

(defun tracker-goto-bpm ()
  "Places the point at the beginning of the BPM field."
  (interactive)
  (set-buffer tracker-buffer)
  (goto-char (point-min))
  (search-forward "BPM: "))

(defun tracker-goto-currently-viewed-pattern ()
  "Places the point at the beginning of the current pattern field."
  (set-buffer tracker-buffer)
  (goto-char (point-min))
  (search-forward " Pat: "))

(defun tracker-goto-currently-playing-pattern ()
  "Places the point at the beginning of the currently playing pattern field."
  (set-buffer tracker-buffer)
  (tracker-goto-currently-viewed-pattern)
  (search-forward "/"))

(defun tracker-goto-total-number-of-patterns ()
  "Places the point at the total number of patterns field."
  (set-buffer tracker-buffer)
  (tracker-goto-currently-playing-pattern)
  (search-forward "/"))

(defun tracker-goto-status ()
  "Places the point at the status field."
  (set-buffer tracker-buffer)
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (goto-char (point-min))
    (goto-char (progn (search-forward "Step: ")
                      (1+ (search-backward " " nil nil 3))))))

(defun tracker-goto-currently-viewed-step ()
  "Places the point at the beginning of the current step field."
  (set-buffer tracker-buffer)
  (goto-char (point-min))
  (search-forward " Step: "))

(defun tracker-goto-currently-playing-step ()
  "Places the point at the beginning of the currently playing step field."
  (set-buffer tracker-buffer)
  (tracker-goto-currently-viewed-step)
  (search-forward "/"))

(defun tracker-goto-total-number-of-steps ()
  "Places the point at the total number of steps field."
  (set-buffer tracker-buffer)
  (tracker-goto-currently-playing-step)
  (search-forward "/"))

(defun tracker-goto-pattern (number)
  "Places the point before the 'P' in 'Pattern' of the specified pattern. Returns nil if the pattern could not be found."
  (set-buffer tracker-buffer)
  (goto-char (point-min))
  (search-forward (concat "Pattern " (number-to-string number) ":\n") nil t))

;;;; functions to get data

(defun tracker-status ()
  "Returns the tracker status field."
  (set-buffer tracker-buffer)
  (save-excursion
    (tracker-goto-status)
    (buffer-substring (point) (1- (search-forward " ")))))

(defun tracker-playing ()
  "Returns t if the tracker is currently playing, nil if it is not."
  (set-buffer tracker-buffer)
  (if (string-match "P" (tracker-status))
      t
    nil))

(defun tracker-current-pattern ()
  "Returns the number of the pattern that is currently being viewed."
  (set-buffer tracker-buffer)
  (save-excursion
    (tracker-goto-currently-viewed-pattern)
    (string-to-number (buffer-substring (point) (save-excursion (search-forward "/" nil t))))))

(defun tracker-playing-pattern ()
  "Returns the number of the pattern that is currently being played."
  (set-buffer tracker-buffer)
  (save-excursion
    (tracker-goto-currently-playing-pattern)
    (string-to-number (buffer-substring (point) (save-excursion (search-forward "/" nil t))))))

(defun tracker-number-of-patterns ()
  "Returns the tracker's status field's total number of patterns in the buffer."
  (set-buffer tracker-buffer)
  (save-excursion
    (tracker-goto-total-number-of-patterns)
    (string-to-number (buffer-substring (point) (save-excursion (search-forward " " nil t))))))

(defun tracker-count-number-of-patterns ()
  "Counts and returns the total number of patterns in the buffer."
  (set-buffer tracker-buffer)
  (save-excursion
    (goto-char (point-min))
    (count-matches "^Pattern [0-9]+:$")))

(defun tracker-number-of-steps ()
  "Returns the number of steps in the current pattern."
  (set-buffer tracker-buffer)
  (save-excursion
    (tracker-goto-total-number-of-steps)
    (string-to-number (buffer-substring (point) (save-excursion (search-forward " " nil t))))))

(defun tracker-latched ()
  "Returns t if the tracker is latched on the current pattern, nil otherwise."
  (set-buffer tracker-buffer)
  (if (string-match "L" (tracker-status))
      t
    nil))

;;;; code to create the tracker interface & fields within the buffer

(defun tracker-make-interface-element (start end &optional name)
  "Make a block of text read-only & intangible."
  (set-buffer tracker-buffer)
  (add-text-properties start end '(intangible (or name t) read-only (or name t) rear-nonsticky t)))

(defun tracker-chars-until (string)
  "Counts the number of characters until the first occurance of STRING."
  (set-buffer tracker-buffer)
  (1- (- (save-excursion (search-forward string)) (point))))

(defun tracker-make-header (&optional steps)
  "Write a header for the tracker and apply text properties to it."
  (set-buffer tracker-buffer)
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at "^TRACKER: ") ; title
      (insert (concat "TRACKER: Untitled" (string ?\n))))
    (goto-line 2)
    (unless (looking-at "^BPM: [0-9]+ Pat: [0-9]+/[0-9]+/[0-9]+ [PLSF]+ Step: [0-9]+/[0-9]+/[0-9]+")
      (insert (concat "BPM: 120 Pat: 0/0/0 S Step: 0/0/16" (string ?\n ?\n)))) ; pat and step are in the format 'point/tracker playing/total'
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
  (set-buffer tracker-buffer)
  (save-excursion
    (string-to-number (buffer-substring (tracker-goto-bpm) (1- (search-forward " "))))))

(defun tracker-set-status (status)
  "Changes the status indicator."
  (set-buffer tracker-buffer)
  (save-excursion
    (let ((inhibit-read-only t)
          (inhibit-point-motion-hooks t))
      (tracker-goto-status)
      (delete-char (- (- (save-excursion (search-forward "Step: ")) 6) (point)))
      (insert (propertize (concat status " ") 'intangible 'status 'read-only 'status 'rear-nonsticky t)))))

(defun tracker-update-number-of-patterns ()
  "Updates the tracker status line with the correct number of patterns."
  (set-buffer tracker-buffer)
  (save-excursion
    (let ((inhibit-read-only t)
          (inhibit-point-motion-hooks t))
      (tracker-goto-total-number-of-patterns)
      (delete-char (tracker-chars-until " "))
      (insert (number-to-string (tracker-count-number-of-patterns))))))

(defun tracker-make-pattern (number steps)
  "Write a specific pattern if it does not exist. You should probably call `tracker-make-new-pattern' instead of this."
  (set-buffer tracker-buffer)
  (save-excursion
    (goto-char (point-min))
    (when (not (tracker-goto-pattern number))
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
      (tracker-update-number-of-patterns)
      (add-to-invisibility-spec (intern (concat "tracker-pattern-" (number-to-string number)))))))

;;;; main loop and associated stuff

(defun tracker-mark-step (step type)
  (set-buffer tracker-buffer)
  (save-excursion
    (when (tracker-goto-step step)
      (forward-char 3)
      (delete-char 1)
      (insert (case type
                ('error "E")
                ('correct " "))))))

(defun tracker-step (step pattern)
  "Highlights and evaluates the code in one step of the tracker."
  (set-buffer tracker-buffer)
  (save-excursion
    (set-buffer tracker-buffer)
    (tracker-goto-currently-playing-step)
    (delete-char (tracker-chars-until "/"))
    (insert (number-to-string step))
    (tracker-goto-currently-playing-pattern)
    (delete-char (tracker-chars-until "/"))
    (insert (number-to-string pattern))
    (let ((old-pat (tracker-current-pattern)))
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
  (set-buffer tracker-buffer)
  (when (tracker-playing)
    (save-excursion
      (set-buffer tracker-buffer)
      (progn (tracker-step step pattern)
             (let* ((delay (tracker-next-step-delay (tracker-get-bpm)))
                    (next-step (mod (1+ step) (tracker-number-of-steps)))
                    (next-pattern (if (or (tracker-latched) (not (eq next-step 0)))
                                      pattern
                                    (mod (1+ pattern) (tracker-number-of-patterns)))))
               (run-with-timer delay nil 'tracker-loop next-step next-pattern))))))

(defun tracker-next-step-delay (bpm)
  "Convert a BPM into the interval between each 16th note."
  (/ (/ 60 (float bpm)) 4))

;;;; interactive commands

(defun tracker-make-new-pattern (steps)
  "Make a new pattern after the last."
  (interactive
   (if (and current-prefix-arg (not (consp current-prefix-arg)))
       (list (prefix-numeric-value current-prefix-arg))
     (list (read-number "Steps: " 16))))
  (set-buffer tracker-buffer)
  (tracker-make-pattern (tracker-number-of-patterns) steps))

(defun tracker-view-pattern (number)
  "Switches the view to the specified pattern."
  (interactive "NPattern: ")
  (set-buffer tracker-buffer)
  (save-excursion
    (goto-char (point-min))
    (let ((old-spec buffer-invisibility-spec))
      (setf buffer-invisibility-spec nil)
      (let ((str (concat "Pattern " (number-to-string (tracker-current-pattern)) ":\n"))
            (end (save-excursion
                   (if (search-forward (concat "Pattern " (number-to-string (1+ (tracker-current-pattern))) ":\n") nil t)
                       (progn (backward-char 2)
                              (beginning-of-line)
                              (point))
                     (progn (search-forward "Scratch:\n")
                            (backward-char 2)
                            (beginning-of-line)
                            (point))))))
        (add-text-properties (- (search-forward str) (length str)) end
                             (list 'invisible (intern (concat "tracker-pattern-" (number-to-string (tracker-current-pattern)))))))
      (setf buffer-invisibility-spec old-spec)))
  (let ((number (max (min number (1- (tracker-number-of-patterns))) 0)))
    (add-to-invisibility-spec (intern (concat "tracker-pattern-" (number-to-string (tracker-current-pattern)))))
    (remove-from-invisibility-spec (intern (concat "tracker-pattern-" (number-to-string number))))
    (save-excursion
      (tracker-goto-currently-viewed-pattern)
      (delete-char (tracker-chars-until "/"))
      (insert (number-to-string number)))))

(defun tracker-next-pattern ()
  "Switches the view to the next pattern."
  (interactive)
  (set-buffer tracker-buffer)
  (tracker-view-pattern (1+ (tracker-current-pattern))))

(defun tracker-previous-pattern ()
  "Switches the view to the previous pattern."
  (interactive)
  (set-buffer tracker-buffer)
  (tracker-view-pattern (1- (tracker-current-pattern))))

(defun tracker-write-template ()
  "Writes the default template for the tracker."
  (interactive)
  (set-buffer tracker-buffer)
  (tracker-make-header)
  (setf buffer-invisibility-spec nil)
  (tracker-make-new-pattern 16)
  (remove-from-invisibility-spec 'tracker-pattern-0)
  (goto-char (point-max))
  (insert "Scratch:\n\n"))

(defun tracker-latch-pattern ()
  "Toggles the latch."
  (interactive)
  (set-buffer tracker-buffer)
  (if (tracker-latched)
      (tracker-set-status (replace-regexp-in-string "L" "" (tracker-status)))
    (tracker-set-status (concat (tracker-status) "L"))))

(defun tracker-start ()
  "Starts the tracker."
  (interactive)
  (set-buffer tracker-buffer)
  (tracker-set-status (concat "P" (replace-regexp-in-string "S" "" (replace-regexp-in-string "P" "" (tracker-status)))))
  (tracker-loop 0 (tracker-current-pattern)))

(defun tracker-stop ()
  "Stops the tracker."
  (interactive)
  (set-buffer tracker-buffer)
  (tracker-set-status (concat "S" (replace-regexp-in-string "S" "" (replace-regexp-in-string "P" "" (tracker-status))))))

(defun tracker-start-stop ()
  "Starts or stops the tracker."
  (interactive)
  (set-buffer tracker-buffer)
  (if (tracker-playing)
      (tracker-stop)
    (tracker-start)))

;;;; convenience functions

;;; math

(defun wrap (number bottom top)
  "Returns the number wrapped within the range of 'bottom' to 'top'."
  (+ (mod (- number bottom) (- (1+ top) bottom)) bottom))

(defun rand (bottom &optional top)
  "Generates a random number within the range of 0 to 'bottom' or 'bottom' to 'top' if top is provided."
  ;; FIX: detect if any of the numbers are floats instead of ints, and if so, return a float within the range, rather than an int.
  (if top
      (+ bottom (random (1+ (- top bottom))))
    (random (1+ bottom))))

(defun choice (&rest choices)
  "Returns a random argument."
  (nth (random (length choices)) choices))

;;; change numbers

(defun tracker-change-number (arg)
  "Increase or decrease a number under the point."
  (interactive "p")
  (set-buffer tracker-buffer)
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
  (set-buffer tracker-buffer)
  (tracker-change-number (or arg 1)))

(defun tracker-decrease-number (arg)
  "Decrease a number under the point."
  (interactive "p")
  (set-buffer tracker-buffer)
  (tracker-change-number (- (or arg 1))))

(defun tracker-back-to-indent ()
  (interactive)
  (set-buffer tracker-buffer)
  (beginning-of-line)
  (when (looking-at "[0-9][0-9][0-9][E ] ")
    (forward-char 5)))

;;; supercollider stuff

(defun sc-symbol (sym)
  (concat "\\" (symbol-name sym)))

(defun sc-parse (sym)
  (cond ((symbolp sym) (sc-symbol sym))
        ((numberp sym) (number-to-string sym))))

(defun sc-arg-list (&rest rest)
  (when rest
    (concat (sc-parse (car rest)) ","
            (sc-parse (cadr rest)) ","
            (apply 'sc-arg-list (cddr rest)))))

(defun synth (synth &rest args)
  (let ((args (apply 'sc-arg-list args)))
    (sclang-eval-string (concat "Synth(\\" (symbol-name synth) ", [" args "]);"))))

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

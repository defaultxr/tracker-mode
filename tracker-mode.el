;;; tracker-mode --- Tracker major mode for Emacs.

;; Copyright (C) 2012-2020 modula t.

;; Author: modula t. <defaultxr@gmail.com>
;; Homepage: https://github.com/defaultxr/tracker.el
;; Version: 0.7
;; Package-Requires: ((osc "0.4") (emacs "24.4") cl-lib)
;; Keywords: multimedia

;; This file is not part of GNU Emacs.

;; Tracker-Mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Foobar is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Tracker-Mode.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides a major mode for Emacs that emulates (or attempts to)
;; music trackers like Renoise, MilkyTracker, or SunVox.
;;
;; A tracker is a sequencer used for making music. In a tracker, musical data
;; is typically represented as a grid of rows and columns. Each column usually
;; represents a separate instrument or channel, and each row represents a step
;; in the sequence.
;;
;; Tracker-Mode can not (yet) handle multiple channels simultaneously, however
;; it does have the ability to save multiple patterns and switch between them.
;;
;; Tracker-Mode does not produce sound on its own, as it is just a sequencer
;; that triggers synthesizers by sending OSC messages. Right now only
;; SuperCollider is being tested as its sound engine, but it should be possible
;; to send to other synths as well with some minor hacking.

;;; Code:

;;; load stuff

(require 'osc)

;;; custom

(defgroup tracker-mode nil
  "Tracker-Mode."
  :group 'external
  :prefix "tracker-mode-")

(defcustom tracker-mode-osc-host "127.0.0.1"
  "The host that the synth process is running on."
  :type '(string))

(defcustom tracker-mode-osc-port 57110
  "The port to send Tracker-Mode's OSC messages to."
  :type '(integer))

;;; global variables

(defvar tracker-mode-hook nil
  "Hook to run when tracker mode starts.")

;; (defvar tracker-buffer nil
;;   "The buffer that the tracker is running in.")

(defun tracker-mode-make-osc-client (host port)
  "Make the OSC connection to Fluxus on host HOST and port PORT."
  (make-network-process
   :name "Tracker-Mode OSC"
   :coding 'binary
   :host host
   :service port
   :type 'datagram
   :family 'ipv4
   :noquery t))

(defvar tracker-osc-client (tracker-mode-make-osc-client tracker-mode-osc-host tracker-mode-osc-port)
  "OSC Client for the tracker.")

(defun tracker-mode-send-msg (&rest args)
  "Send an OSC message to tracker-mode's set host and port."
  (apply #'osc-send-message tracker-osc-client args))

(defun tracker-mode-synth (name &optional args)
  "Trigger a synth to start."
  (tracker-mode-send-msg "/s_new" name -1.0 0 1))

(defun tracker-mode-release (node)
  "Send a release message to the synth with the specified node ID."
  (tracker-mode-send-msg "/n_set" node "gate" 0))

;;; internal tracker commands

(defvar tracker-pattern-regexp "^;+ Pattern \\([0-9]+\\):"
  "The regexp used to match against pattern headers.")

(defvar tracker-step-regexp "^\\([0-9][0-9][0-9]\\)\\(.\\) "
  "The regexp used to match against pattern steps.")

;; (defvar tracker-header-regexp "^TRACK: \\(.+\\) BPM: \\([0-9]+\\) . \\([0-9]+\\) \\([0-9]+\\)$"
;;   "The regexp used to match against the header.")

(defun tracker-highlight-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end))) 
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.5) nil 'delete-overlay overlay)))

(defmacro tracker-without-undo (&rest body)
  "Perform BODY without modifying the buffer's undo list."
  ;; there might be a better way to do this...
  `(let ((buffer-undo-list t))
     ,@body))

;;; functions to move the point around

(defun tracker-goto-title ()
  "Place the point at the end of the title field."
  (interactive)
  (goto-char (point-min))
  (search-forward ";; TRACK: ")
  (end-of-line))

(defun tracker-goto-step (step &optional pattern)
  "Place the point at the specified step in the current pattern."
  (interactive "NStep: ")
  (search-backward-regexp tracker-pattern-regexp)
  (search-forward-regexp (format "^%03d. " step)))

(defun tracker-goto-bpm ()
  "Place the point at the beginning of the BPM field."
  (interactive)
  (goto-char (point-min))
  (search-forward-regexp "^;; BPM: "))

(defun tracker-goto-currently-viewed-pattern ()
  "Places the point at the beginning of the current pattern field."
  (interactive)
  (tracker-goto-pattern (tracker-current-pattern)))

(defun tracker-goto-currently-playing-pattern ()
  "Places the point at the beginning of the currently playing pattern field."
  (interactive)
  (tracker-goto-pattern (tracker-playing-pattern)))

(defun tracker-goto-scratch ()
  "Place the point at the beginning of the scratch field."
  (interactive)
  (goto-char (point-min))
  (search-forward-regexp "^;; Scratch:")
  (forward-char))

(defun tracker-goto-end-of-current-pattern ()
  "Place the point at the end of the current pattern."
  (tracker-goto-step (1- (tracker-number-of-steps))))

;; (defun tracker-goto-status ()
;;   "Places the point at the status field."
;;   (let ((inhibit-read-only t)
;;         (inhibit-point-motion-hooks t))
;;     (goto-char (point-min))
;;     (goto-char (progn (search-forward "Step: ")
;;                       (1+ (search-backward " " nil nil 3))))))

;; (defun tracker-goto-currently-playing-step ()
;;   "Places the point at the beginning of the currently playing step field."
;;   (set-buffer tracker-buffer)
;;   (goto-char (point-min))
;;   (search-forward " Step: "))

(defun tracker-goto-pattern (&optional pattern)
  "Places the point at the end of the \"Pattern N:\" line for the specified pattern. Returns nil if the pattern could not be found."
  (interactive "NPattern: ")
  (goto-char (point-min))
  (let ((pattern (or pattern (tracker-pattern-under-point))))
    (and (search-forward (concat ";; Pattern " (number-to-string pattern) ":\n") nil t)
         (backward-char))))

;;; functions to get data

(defun tracker-patterns ()
  "Get a list of the pattern numbers in the current buffer."
  (let (res)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp tracker-pattern-regexp nil t)
        (push (string-to-number (match-string-no-properties 1)) res)))
    (nreverse res)))

(defun tracker-number-of-patterns ()
  "Counts the total number of patterns in the buffer."
  (save-excursion
    (goto-char (point-min))
    (count-matches tracker-pattern-regexp)))

(defun tracker-current-pattern ()
  "Returns the number of the pattern that is currently being viewed."
  (or (tracker-pattern-under-point) 0))

(defun tracker-pattern-under-point ()
  "Returns the number of the pattern that the point is in, or nil if none."
  (save-excursion
    (when (search-backward-regexp tracker-pattern-regexp nil t)
      (string-to-number (match-string 1)))))

(defun tracker-pattern-steps (&optional pattern)
  "Get a list of the step numbers in PATTERN."
  (let ((pattern (or pattern
                     (tracker-pattern-under-point)
                     0))
        res)
    (save-excursion
      (tracker-goto-pattern pattern)
      (while (search-forward-regexp tracker-step-regexp nil t)
        (push (string-to-number (match-string-no-properties 1)) res)))
    (nreverse res)))

(defun tracker-number-of-steps (&optional pattern)
  "Get the number of currently-defined steps in a pattern."
  (when-let ((pattern (or pattern (tracker-pattern-under-point))))
    (save-excursion
      (tracker-goto-pattern pattern)
      (count-matches tracker-step-regexp
                     (point)
                     (save-excursion
                       (ignore-errors
                         (tracker-goto-pattern (1+ pattern)))
                       (point))))))

(defun tracker-step-under-point ()
  "Returns the step that the point is on, or nil if the point is not located on a step."
  (when-let ((pattern (tracker-pattern-under-point)))
    (when (save-excursion
            (search-backward (concat ";; Pattern " (number-to-string pattern) ":\n") nil t))
      (save-excursion
        (when (search-backward-regexp tracker-step-regexp nil t)
          (string-to-number (buffer-substring (point) (+ 3 (point)))))))))

(defvar tracker-playing-p nil
  "Whether or not the tracker is currently playing.")

(make-variable-buffer-local 'tracker-playing-p)
(set-default 'tracker-playing-p nil)

;; FIX
(defun tracker-playing-pattern ()
  "Returns the number of the pattern that is currently being played."
  (save-excursion
    (tracker-goto-currently-playing-pattern)
    (string-to-number (buffer-substring (point) (save-excursion (search-forward "/" nil t))))))

(defvar tracker-latched-p nil
  "Whether or not the tracker should loop only this pattern when playing.")

(make-variable-buffer-local 'tracker-latched-p)
(set-default 'tracker-latched-p nil)

(make-variable-buffer-local 'tracker-bpm)
(set-default 'tracker-bpm 120)

(defun tracker-bpm ()
  "Get the confirmed BPM of the track."
  tracker-bpm)

(defun tracker-listed-bpm ()
  "Get the BPM listed in the track."
  (save-excursion
    (string-to-number (buffer-substring-no-properties
                       (tracker-goto-bpm)
                       (1- (search-forward " "))))))

(defun tracker-tempo ()
  "Get the tracker tempo as beats per second."
  (/ (tracker-bpm) 60))

(make-variable-buffer-local 'tracker-track-name)
(set-default 'tracker-track-name "")

(defun tracker-track-name ()
  "Get the confirmed track name of the track."
  tracker-track-name)

;;; interface code

(defun tracker-buffer-p (&optional buffer)
  "True if BUFFER appears to be a `tracker-mode' buffer."
  (let ((buffer (or buffer (current-buffer))))
    (save-excursion
      (goto-char (point-min))
      (and (looking-at "^;; TRACK: ")
           (progn
             (goto-line 2)
             (looking-at "^;; BPM: [0-9]+"))))))

(defun tracker-write-template ()
  "Writes the default template for the tracker."
  (tracker-without-undo
   (save-excursion
     (goto-char (point-min))
     (unless (looking-at "^;; TRACK: ") ; title
       (insert (concat ";; TRACK: Untitled" (string ?\n))))
     (goto-line 2)
     (unless (looking-at "^;; BPM: [0-9]+")
       (insert (concat ";; BPM: 120" (string ?\n ?\n)))))
   (setf buffer-invisibility-spec nil)
   (save-excursion
     (goto-char (point-min))
     (unless (search-forward-regexp tracker-pattern-regexp nil t)
       (tracker-insert-pattern 16)))
   (remove-from-invisibility-spec 'tracker-pattern-0)))

(defun tracker ()
  "Initialize a buffer for `tracker-mode', creating one if necessary."
  (interactive)
  (unless (tracker-buffer-p)
    (get-buffer-create "*Tracker*")
    (switch-to-buffer "*Tracker*"))
  (tracker-mode))

(defun tracker-update-header ()
  "Update the header line of the tracker buffer."
  (setf header-line-format (concat "Track: "
                                   (tracker-track-name)
                                   " BPM: " (number-to-string (tracker-bpm))
                                   " "
                                   (if tracker-playing-p "▶" "⏹")
                                   " "
                                   (when tracker-current-playing-pattern
                                     (number-to-string tracker-current-playing-pattern))
                                   " "
                                   (when tracker-current-playing-step
                                     (number-to-string tracker-current-playing-step))))
  (force-mode-line-update))

(defun tracker-after-change-function (start end length)
  "Function to run after the buffer is modified to mark a step as modified."
  (save-excursion
    (goto-char start)
    (when-let ((pattern (tracker-pattern-under-point))
               (step (tracker-step-under-point)))
      (tracker-mark-step step pattern 'modified))))

(defun tracker-mark-step (step pattern type)
  "Marks the specified step in the current pattern as correct or containing an error."
  (tracker-without-undo
   (save-excursion
     (when (tracker-goto-step step pattern)
       (beginning-of-line)
       (forward-char 3)
       (delete-char 1)
       (insert (case type
                 (error "E")
                 (modified "M")
                 (correct " ")))))))

(defvar tracker-confirmed-steps nil
  "The hash table of the track's confirmed steps.")

(make-variable-buffer-local 'tracker-confirmed-steps)
(set-default 'tracker-confirmed-steps nil)

(defun tracker-make-confirmed-steps-hash ()
  "Create the `tracker-confirmed-steps' hash table."
  (setf tracker-confirmed-steps (make-hash-table :test 'equal)))

(defun tracker-step-id (step pattern)
  "Make a key for the tracker steps hash."
  (list pattern step))

(defun tracker-get-confirmed-step (step pattern)
  (gethash (tracker-step-id step pattern) tracker-confirmed-steps))

(defun tracker-set-confirmed-step (step pattern value)
  "Deletes a confirmed step."
  (setf (gethash (tracker-step-id step pattern) tracker-confirmed-steps) value))

(defun tracker-delete-confirmed-step (step pattern)
  "Deletes a confirmed step."
  (remhash (tracker-step-id step pattern) tracker-confirmed-steps))

;;; main loop and associated stuff

(defun tracker-next-step-delay (bpm)
  "Convert a BPM into the interval between each 16th note."
  (/ (/ 60 (float bpm)) 4))

(defvar tracker-current-playing-pattern nil
  "The number of the currently-playing pattern.")

(make-variable-buffer-local 'tracker-current-playing-pattern)
(set-default 'tracker-current-playing-pattern nil)

(defvar tracker-current-playing-step nil
  "The number of the currently-playing step.")

(make-variable-buffer-local 'tracker-current-playing-step)
(set-default 'tracker-current-playing-step nil)

(defun tracker-step (step pattern buffer)
  "Highlights and plays one step of the tracker."
  (setf tracker-current-playing-step step
        tracker-current-playing-pattern pattern)
  (when-let ((c-step (tracker-get-confirmed-step step pattern)))
    (condition-case err (funcall c-step)
      (error
       (message "Tracker got a %s when attempting to run step %d in pattern %d."
                (car err) step pattern)
       (tracker-mark-step step pattern 'error))))
  (tracker-update-header))

(defun tracker-loop (step pattern &optional buffer)
  "The main loop of the tracker."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (when tracker-playing-p
        (save-excursion
          (tracker-step step pattern buffer)
          (let* ((delay (tracker-next-step-delay (tracker-bpm)))
                 (next-step (or (cadr (member step (tracker-pattern-steps)))
                                0))
                 (next-pattern (if (or tracker-latched-p
                                       (not (eql next-step 0)))
                                   pattern
                                 (mod (1+ pattern) (tracker-number-of-patterns)))))
            (run-with-timer delay nil 'tracker-loop next-step next-pattern buffer)))))))

;;; pattern editing

(defun tracker-insert-pattern (&optional steps)
  "Insert a new pattern after the current."
  (interactive
   (if (and current-prefix-arg (not (consp current-prefix-arg)))
       (list (prefix-numeric-value current-prefix-arg))
     (list (read-number "Steps: " 16))))
  (let ((number (tracker-number-of-patterns))
        (buffer-invisibility-spec nil))
    (save-excursion
      (or (ignore-errors ;; FIX
            (tracker-goto-end-of-current-pattern))
          (goto-char (point-max)))
      (insert (propertize
               (concat ";; Pattern " (number-to-string number) ":\n"
                       (let ((result ""))
                         (dotimes (step steps result)
                           (setf result (concat result (format "%03d  %s" step (string ?\n)))))
                         result)
                       (string ?\n))
               'invisible (intern (concat "tracker-pattern-" (number-to-string number))))))
    (add-to-invisibility-spec (intern (concat "tracker-pattern-" (number-to-string number))))))

(defun tracker-delete-pattern ()
  "Delete the pattern under point."
  (interactive)
  (if-let ((pattern (tracker-pattern-under-point)))
      (progn
        (dolist (step (tracker-pattern-steps pattern))
          (remhash (tracker-step-id step pattern) tracker-confirmed-steps))
        (save-excursion
          (delete-region (save-excursion
                           (search-backward-regexp tracker-pattern-regexp nil t)
                           (point))
                         (save-excursion
                           (search-forward-regexp tracker-pattern-regexp nil t)
                           (beginning-of-line)
                           (point)))))
    (message "The point does not appear to be in a pattern.")))

(defun tracker-next-pattern ()
  "Switches the view to the next pattern."
  (interactive)
  (tracker-goto-pattern (mod (1+ (or (tracker-pattern-under-point) 0))
                             (tracker-number-of-patterns))))

(defun tracker-previous-pattern ()
  "Switches the view to the previous pattern."
  (interactive)
  (tracker-goto-pattern (mod (1- (or (tracker-pattern-under-point) 0))
                             (tracker-number-of-patterns))))

(defun tracker-latch (&optional enable)
  "Enable or disable latching the current pattern."
  (interactive "p")
  (setf tracker-latched-p (if (booleanp enable)
                              enable
                            (if (> enable 0) t nil)))
  (tracker-update-header)
  (message (if tracker-latched-p
               (concat "Latched pattern " (number-to-string tracker-current-playing-pattern) ".")
             "Tracker unlatched.")))

(defun tracker-toggle-latch ()
  "Toggles whether to loop the current pattern."
  (interactive)
  (tracker-latch (not tracker-latched-p)))

;;; steps

(defun tracker-read-step (step)
  "Attempt to read the elisp from a specified step in the current pattern."
  (save-excursion
    (when (and (tracker-goto-step step)
               (looking-at "("))
      (read (buffer-substring (point) (progn (forward-sexp) (point)))))))

(defun tracker-confirm-step ()
  "Confirms the edits to the current step."
  (interactive)
  (if-let ((current-step (tracker-step-under-point)))
      (save-excursion
        (let ((current-pattern (tracker-pattern-under-point))
              (elisp (tracker-read-step current-step)))
          (tracker-set-confirmed-step current-step current-pattern (eval `(lambda () ,elisp)))
          (tracker-mark-step current-step current-pattern 'correct)))
    (save-excursion
      (beginning-of-line)
      (cond ((looking-at "^;; BPM: \\(.+\\)$")
             (setf tracker-bpm (string-to-number (match-string-no-properties 1)))
             (tracker-update-header))
            ((looking-at "^;; TRACK: \\(.+\\)$")
             (setf tracker-track-name (match-string-no-properties 1))
             (tracker-update-header))
            (t
             (message "The point does not appear to be on a step."))))))

;;; transport

(defun tracker-play ()
  "Starts the tracker."
  (interactive)
  (setf tracker-playing-p t)
  (tracker-loop 0 (or (tracker-pattern-under-point) 0) (current-buffer))
  (message "Tracker started."))

(defun tracker-stop ()
  "Stops the tracker."
  (interactive)
  (setf tracker-playing-p nil)
  (message "Tracker stopped.")
  (tracker-update-header))

(defun tracker-play-stop ()
  "Starts or stops the tracker."
  (interactive)
  (if tracker-playing-p
      (tracker-stop)
    (tracker-play)))

;;; utility functions

;; (defun wrap (number bottom top)
;;   "Returns the number wrapped within the range of 'bottom' to 'top'."
;;   (+ (mod (- number bottom) (- (1+ top) bottom)) bottom))

;; (defun rand (bottom &optional top)
;;   "Generates a random number within the range of 0 to 'bottom' or 'bottom' to 'top' if top is provided."
;;   ;; FIX: detect if any of the numbers are floats instead of ints, and if so, return a float within the range, rather than an int.
;;   (if top
;;       (+ bottom (random (1+ (- top bottom))))
;;     (random (1+ bottom))))

;; (defun choice (&rest choices)
;;   "Returns a random argument."
;;   (nth (random (length choices)) choices))

;;; change numbers

(defun tracker-change-number (arg)
  "Increase or decrease a number under the point."
  (interactive "p")
  (tracker-without-undo
   (save-excursion
     (search-backward-regexp "[^0-9-]")
     (forward-char)
     (let* ((start (point))
            (end (1- (save-excursion (search-forward-regexp "[^0-9-]"))))
            (string (buffer-substring start end)))
       (when (not (eq 0 (- end start)))
         (delete-char (- end start))
         (insert (number-to-string (+ (or arg 1) (string-to-number string)))))))))

(defun tracker-increase-number (arg)
  "Increase a number under the point."
  (interactive "p")
  (tracker-change-number (or arg 1)))

(defun tracker-decrease-number (arg)
  "Decrease a number under the point."
  (interactive "p")
  (tracker-change-number (- (or arg 1))))

(defun tracker-back-to-indent ()
  (interactive)
  (beginning-of-line)
  (when (looking-at tracker-step-regexp)
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

;; (defmacro retrig (num delay &rest body)
;;   `(run-with-timer ,delay nil (lambda () ,body)))


;;; keymap

(defvar tracker-mode-map
  (let ((map (make-sparse-keymap "Tracker-Mode")))
    (define-key map (kbd "<M-down>") 'tracker-decrease-number)
    (define-key map (kbd "<M-up>") 'tracker-increase-number)
    (define-key map (kbd "C-a") 'tracker-back-to-indent) ;; FIX: this breaks C-S-a; prevents activating the region
    (define-key map (kbd "C-c C-n") 'tracker-next-pattern)
    (define-key map (kbd "C-c C-p") 'tracker-previous-pattern)
    ;; (define-key map (kbd "M-n") 'tracker-next-field) ; goes to next step in the pattern, etc
    ;; (define-key map (kbd "M-p") 'tracker-previous-field)
    (define-key map (kbd "C-c C-s") 'tracker-play-stop)
    (define-key map (kbd "C-c C-l") 'tracker-toggle-latch)
    (define-key map (kbd "C-c C-i") 'tracker-insert-pattern)
    (define-key map (kbd "C-c C-d") 'tracker-delete-pattern)
    (define-key map (kbd "C-c C-c") 'tracker-confirm-step)
    ;; (define-key map (kbd "C-c C-k") 'tracker-revert-step) ;; FIX
    (define-key map (kbd "M-g b") 'tracker-goto-bpm)
    (define-key map (kbd "M-g s") 'tracker-goto-step)
    (define-key map (kbd "M-g t") 'tracker-goto-title)
    (define-key map (kbd "M-g p") 'tracker-goto-pattern) ; this overrides the original M-g p action, but i don't think you'll use it in tracker mode anyway... (?)
    map)
  "Keymap for `tracker-mode'.")

;;; initialize & register the mode with emacs

;;;###autoload
(define-derived-mode tracker-mode emacs-lisp-mode "Tracker"
  "Major mode for using emacs as a music tracker."
  (unless (tracker-buffer-p)
    (error "This does not appear to be a tracker-mode-formatted buffer. Try M-x tracker to create and initialize a new tracker-mode buffer."))
  (use-local-map tracker-mode-map)
  (tracker-write-template)
  (tracker-make-confirmed-steps-hash)
  (tracker-update-header)
  (add-to-list 'after-change-functions 'tracker-after-change-function)
  (run-hooks 'tracker-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.trk\\'" . tracker-mode))
(add-to-list 'auto-mode-alist '("\\.track\\'" . tracker-mode))

(provide 'tracker-mode)

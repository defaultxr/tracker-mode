;;; tracker-mode.el --- Tracker-inspired sequencer -*- lexical-binding: t -*-

;; Copyright (C) 2012-2022 modula t.

;; Author: modula t. <defaultxr at gmail dot com>
;; Homepage: https://github.com/defaultxr/tracker-mode
;; Version: 0.7
;; Keywords: multimedia
;; Package-Requires: ((osc "0.4") (emacs "25.1"))

;; This file is not part of GNU Emacs.

;;; License:

;; Tracker-Mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Tracker-Mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Tracker-Mode.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides a major mode for Emacs for sequencing of code
;; emulates (or attempts to) music trackers like Renoise,
;; MilkyTracker, or SunVox.
;;
;; A tracker is a sequencer used for making music.  In a tracker,
;; musical data is typically represented as a grid of rows and
;; columns.  Each column usually represents a separate instrument or
;; channel, and each row represents a step in the sequence.
;;
;; Tracker-Mode can not (yet) handle multiple channels simultaneously,
;; however it does have the ability to save multiple patterns and
;; switch between them.
;;
;; Tracker-Mode does not produce sound on its own, as it is just a
;; sequencer that triggers synthesizers by sending OSC messages.
;; Right now only SuperCollider is being tested as its sound engine,
;; but it should be possible to send to other synths as well with some
;; minor hacking.

;;; Code:

;;; load stuff

(require 'osc)
(require 'cl-lib)

;;; custom

(defgroup tracker nil
  "Tracker-Mode."
  :group 'external
  :prefix "tracker-")

(defcustom tracker-osc-host "127.0.0.1"
  "The host that the synth process is running on."
  :type '(string)
  :group 'tracker)

(defcustom tracker-osc-port 57110
  "The port to send Tracker-Mode's OSC messages to."
  :type '(integer)
  :group 'tracker)

;;; global variables

(defvar tracker-mode-hook nil
  "Hook to run when tracker mode starts.")

;;; internal tracker variables + functions

(defvar tracker-title-regexp "^;+ T[Rr][Aa][Cc][Kk]: \\(.*\\)$"
  "The regexp used to match against the track's title line.")

(defvar tracker-bpm-regexp "^;+ BPM: \\([0-9]+\\)$"
  "The regexp used to match against the track's BPM line.")

(defvar tracker-pattern-regexp "^;+ Pattern \\([0-9]+\\):"
  "The regexp used to match against pattern headers.")

(defvar tracker-step-regexp "^\\([0-9][0-9][0-9]\\)\\(.\\) "
  "The regexp used to match against pattern steps.")

(defmacro tracker-without-undo (&rest body)
  "Perform BODY without modifying the buffer's undo list."
  ;; there might be a better way to do this...
  `(progn
     (buffer-disable-undo)
     ,@body
     (buffer-enable-undo)))

;;; utility functions

(defun tracker-symbol-to-string (symbol)
  "Convert SYMBOL to a string."
  (downcase (string-trim-left (symbol-name symbol) ":")))

;;; functions to move the point around

(defun tracker-goto-title ()
  "Place the point at the end of the title field."
  (interactive)
  (goto-char (point-min))
  (search-forward-regexp tracker-title-regexp)
  (end-of-line))

(defun tracker-goto-step (step &optional pattern)
  "Place the point at the start of STEP in PATTERN (or the current if none specified)."
  (interactive "NStep: ")
  (tracker-goto-pattern (or pattern (tracker-pattern-at-point)))
  (search-forward-regexp (format "^%03d. " step)))

(defun tracker-goto-bpm ()
  "Place the point at the end of the BPM field."
  (interactive)
  (goto-char (point-min))
  (search-forward-regexp tracker-bpm-regexp))

(defun tracker-goto-currently-viewed-pattern ()
  "Place the point at the beginning of the current pattern field."
  (interactive)
  (tracker-goto-pattern (tracker-pattern-at-point)))

(defun tracker-goto-currently-playing-pattern ()
  "Place the point at the beginning of the currently-playing pattern."
  (interactive)
  (tracker-goto-pattern (tracker-playing-pattern)))

(defun tracker-goto-scratch ()
  "Place the point at the beginning of the scratch field."
  (interactive)
  (goto-char (point-min))
  (search-forward-regexp "^;; Scratch:")
  (forward-char))

(defun tracker-goto-end-of-pattern (&optional pattern)
  "Place the point at the end of PATTERN."
  (goto-char (second (tracker-step-bounds (1- (tracker-steps-count)) pattern))))

(defun tracker-goto-playing-step ()
  "Place the point at the beginning of the currently-playing step."
  (interactive)
  (tracker-goto-step tracker-current-playing-step tracker-current-playing-pattern))

(defun tracker-goto-pattern (&optional pattern)
  "Place the point at the end of PATTERN's header line, or return nil if PATTERN could not be found."
  (interactive "NPattern: ")
  (goto-char (point-min))
  (let ((pattern (or pattern (tracker-pattern-at-point))))
    (and (search-forward (concat ";; Pattern " (number-to-string pattern) ":\n") nil t)
         (backward-char))))

(defun tracker-goto-next-pattern (&optional num wrap)
  "Move point to the next pattern."
  (interactive "p")
  (let* ((num (or num 1))
         (desired (+ num (or (tracker-pattern-at-point) 0))))
    (if wrap
        (tracker-goto-pattern (mod desired (tracker-patterns-count)))
      (cond ((>= desired (tracker-patterns-count))
             (goto-char (point-max)))
            ((< desired 0)
             (goto-char (point-min)))
            (t
             (tracker-goto-pattern desired))))))

(defun tracker-goto-previous-pattern (&optional num wrap)
  "Move point to the previous pattern."
  (interactive "p")
  (let ((num (or num 1)))
    (tracker-next-pattern (- num))))

;;; functions to get data

(defun tracker-patterns ()
  "Get a list of the pattern numbers in the current buffer."
  (let (res)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp tracker-pattern-regexp nil t)
        (push (string-to-number (match-string-no-properties 1)) res)))
    (nreverse res)))

(defun tracker-patterns-count ()
  "Get the total number of patterns in the buffer."
  (save-excursion
    (goto-char (point-min))
    (count-matches tracker-pattern-regexp)))

(defun tracker-pattern-at-point ()
  "Get the number of the pattern that the point is in, or nil if none."
  (save-excursion
    (when (search-backward-regexp tracker-pattern-regexp nil t)
      (string-to-number (match-string 1)))))

(defun tracker-pattern-steps (&optional pattern)
  "Get a list of the step numbers in PATTERN."
  (let ((pattern (or pattern
                     (tracker-pattern-at-point)
                     0))
        res)
    (save-excursion
      (let ((pattern-end (progn
                           (tracker-goto-end-of-pattern)
                           (point))))
        (tracker-goto-pattern pattern)
        (while (search-forward-regexp tracker-step-regexp pattern-end t)
          (push (string-to-number (match-string-no-properties 1)) res))))
    (nreverse res)))

(defun tracker-steps-count (&optional pattern)
  "Get the number of steps in PATTERN."
  (when-let ((pattern (or pattern (tracker-pattern-at-point))))
    (save-excursion
      (tracker-goto-pattern pattern)
      (count-matches tracker-step-regexp
                     (point)
                     (save-excursion
                       (or (when (search-forward-regexp tracker-pattern-regexp nil t)
                             (beginning-of-line)
                             (point))
                           (point-max)))))))

(defun tracker-step-at-point ()
  "Get the step number that the point is on, or nil if the point is not located on a step."
  (when-let ((pattern (tracker-pattern-at-point)))
    (when (save-excursion
            (search-backward (concat ";; Pattern " (number-to-string pattern) ":\n") nil t))
      (save-excursion
        (when (search-backward-regexp tracker-step-regexp nil t)
          (string-to-number (buffer-substring (point) (+ 3 (point)))))))))

(defvar tracker-playing-p nil
  "True if the tracker is currently playing, or nil if it is stopped.")

(make-variable-buffer-local 'tracker-playing-p)
(set-default 'tracker-playing-p nil)

(defun tracker-playing-pattern ()
  "Get the number of the currently-playing pattern."
  tracker-current-playing-pattern)

(defvar tracker-latched-p nil
  "True if the tracker should loop only its current pattern when playing.")

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
  "True if BUFFER looks like a `tracker-mode' buffer."
  (save-excursion
    (with-current-buffer (or buffer (current-buffer))
      (goto-char (point-min))
      (and (looking-at tracker-title-regexp)
           (progn
             (goto-line 2)
             (looking-at tracker-bpm-regexp))))))

(defvar tracker-modifying-buffer-p nil
  "True if tracker-mode is modifying the buffer and `tracker-after-change-function' should ignore changes.")

(make-variable-buffer-local 'tracker-modifying-buffer-p)
(set-default 'tracker-modifying-buffer-p nil)

(defmacro tracker-modifying-buffer (&rest body)
  "Run BODY with variables set to allow modifications to the tracker-mode template/UI elements."
  `(let ((tracker-modifying-buffer-p t)
         (inhibit-read-only t))
     ,@body))

(defun tracker-ui-element (start end &optional ro-message additional-properties)
  "Write a tracker interface element to the current buffer, at point."
  (let ((intangible (cl-getf additional-properties additional-properties (gensym)))
        (props `(read-only ,(or ro-message "Cannot edit tracker-mode template") ,@additional-properties)))
    (set-text-properties start (+ start 1) (cl-list* 'cursor-intangible intangible props))
    (set-text-properties (+ start 1) (- end 1) (cl-list* 'cursor-intangible intangible props))
    (set-text-properties (- end 1) end (cl-list* 'rear-nonsticky t props))))

(defun tracker-add-ui-element (text &optional ro-message additional-properties)
  "Search for TEXT, and if it exists, propertize it as a UI element. If it does not exist, insert it as a UI element. Returns t if the text was inserted, or nil if it was already there."
  (let* ((inserted nil)
         (end (save-excursion (if (search-forward text nil t)
                                  (point)
                                (progn
                                  (insert text)
                                  (setq inserted t)
                                  (point))))))
    (tracker-ui-element (- end (length text)) end ro-message additional-properties)
    (goto-char end)
    inserted))

(defun tracker-propertize-patterns ()
  "Find all patterns in the buffer and ensure their UI elements are correctly propertized.

See also: `tracker-insert-pattern', `tracker-write-template'"
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "^;; Pattern [0-9]+:$" nil t)
      (tracker-ui-element (save-excursion
                            (beginning-of-line)
                            (1- (point)))
                          (point)
                          "Cannot edit tracker-mode template; use `tracker-delete-pattern' to delete patterns")
      (let ((pattern-end (save-excursion
                           (tracker-goto-end-of-pattern)
                           (point))))
        (while (search-forward-regexp "^[0-9][0-9][0-9].." pattern-end t)
          (tracker-ui-element (save-excursion
                                (beginning-of-line)
                                (1- (point)))
                              (point)
                              "Cannot edit step markers; use `tracker-resize-pattern' to change the number of steps in a pattern"))))))

(defun tracker-write-template ()
  "Write the default template for the tracker, or propertize it if it already exists."
  (tracker-modifying-buffer
   (tracker-without-undo
    (save-excursion
      (goto-char (point-min))
      (when (and (/= (point-min) (point-max))
                 (not (tracker-buffer-p)))
        (error "This does not appear to be a tracker-mode-formatted buffer.  Try M-x tracker to create and initialize a new tracker-mode buffer"))
      (when (tracker-add-ui-element ";; Track: ")
        (insert "Untitled"))
      (when (tracker-add-ui-element "\n;; BPM: ")
        (insert "120\n\n"))
      (goto-char (point-min))
      (if (search-forward-regexp tracker-pattern-regexp nil t)
          (tracker-propertize-patterns)
        (tracker-insert-pattern 16))))))

(defface tracker-header-heading-face
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight header headings."
  :group 'tracker)

(defun tracker-update-header ()
  "Update the header line of the tracker buffer."
  (cl-flet ((heading (string &optional help-echo)
                     (apply 'propertize string
                            'face 'tracker-header-heading-face
                            (when help-echo
                              (list 'help-echo help-echo)))))
    (setf header-line-format (concat (heading "Track:" "The name of the track.")
                                     " "
                                     (tracker-track-name)
                                     " "
                                     (heading "BPM:" "Current tempo in beats per minute.")
                                     " "
                                     (number-to-string (tracker-bpm))
                                     " "
                                     (heading (if tracker-playing-p "▶" "⏹")
                                              (concat "Transport status: "
                                                      (if tracker-playing-p "playing" "stopped")
                                                      "."))
                                     (heading (if tracker-latched-p "L" " ")
                                              (if tracker-latched-p
                                                  "Latch on."
                                                "Latch off."))
                                     " "
                                     (heading "Pattern:" "Currently-playing pattern.")
                                     " "
                                     (when tracker-current-playing-pattern
                                       (number-to-string tracker-current-playing-pattern))
                                     " "
                                     (heading "Step:" "Currently-playing step.")
                                     " "
                                     (when tracker-current-playing-step
                                       (number-to-string tracker-current-playing-step)))))
  (force-mode-line-update))

(defun tracker-point-in-step-field-p ()
  "True when point is inside a step field."
  (and (tracker-step-at-point)
       (>= (current-column) 5)))

(defun tracker-after-change-function (start end length)
  "Mark the associated step as modified after the buffer is modified."
  (unless tracker-modifying-buffer-p
    (save-excursion
      (goto-char start)
      (when (tracker-point-in-step-field-p)
        (when-let ((pattern (tracker-pattern-at-point))
                   (step (tracker-step-at-point)))
          (tracker-mark-step step pattern 'modified))))))

(defun tracker-mark-step (step pattern type)
  "Mark STEP in PATTERN as modified (M), erroring (E), or confirmed (blank).  TYPE should be either 'error, 'modified, or 'confirmed."
  (unless (eql type 'modified)
    (buffer-disable-undo))
  (tracker-modifying-buffer
   (save-excursion
     (when (tracker-goto-step step pattern)
       (beginning-of-line)
       (forward-char 3)
       (delete-char 1)
       (insert (cl-case type
                 (error "E")
                 (modified "M")
                 (confirmed " "))))))
  (unless (eql type 'modified)
    (buffer-enable-undo)))

(defvar tracker-confirmed-steps nil
  "The hash table mapping pattern and step numbers to the code for that step.")

(make-variable-buffer-local 'tracker-confirmed-steps)
(set-default 'tracker-confirmed-steps nil)

(defun tracker-make-confirmed-steps-hash ()
  "Create the `tracker-confirmed-steps' hash table."
  (setf tracker-confirmed-steps (make-hash-table :test 'equal)))

(defun tracker-step-id (step pattern &optional key)
  "Get a key for the `tracker-confirmed-steps' hash to access STEP in PATTERN.  KEY, if provided, specifies a different datum about the step, for example 'string is the step as an unparsed string."
  (list pattern step key))

(defun tracker-confirmed-step (step pattern &optional key)
  "Get the confirmed step STEP in PATTERN from the `tracker-confirmed-steps' hash.  KEY, if provided, specifies a different datum about the step, for example 'string is the step as an unparsed string."
  (gethash (tracker-step-id step pattern key) tracker-confirmed-steps))

(defun tracker-confirmed-step-set (code step pattern &optional key)
  (when key
    (error "KEY is not supported when setting `tracker-confirmed-step'"))
  (let ((code (if (string= "" code)
                  "nil"
                code)))
    (puthash (tracker-step-id step pattern)
             (eval `(lambda () ,(read code)))
             tracker-confirmed-steps)
    (puthash (tracker-step-id step pattern 'string)
             code
             tracker-confirmed-steps)))

(gv-define-setter tracker-confirmed-step (code step pattern &optional key)
  `(tracker-confirmed-step-set ,code ,step ,pattern ,key))

;;; main loop and associated stuff

(defun tracker-next-step-delay (bpm)
  "Convert a BPM into the interval between each 16th note."
  (/ (/ 60 (float bpm)) 4))

(defvar tracker-current-playing-pattern nil
  "The currently-playing pattern number.")

(make-variable-buffer-local 'tracker-current-playing-pattern)
(set-default 'tracker-current-playing-pattern nil)

(defvar tracker-current-playing-step nil
  "The currently-playing step number.")

(make-variable-buffer-local 'tracker-current-playing-step)
(set-default 'tracker-current-playing-step nil)

(defun tracker-step (step pattern &optional buffer)
  "Play STEP of PATTERN in BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (setf tracker-current-playing-step step
            tracker-current-playing-pattern pattern)
      (when-let ((c-step (tracker-confirmed-step step pattern)))
        (condition-case err (funcall c-step)
          (error
           (message "Tracker got a %s when attempting to run step %d in pattern %d."
                    (car err) step pattern)
           (tracker-mark-step step pattern 'error))))
      (tracker-update-header))))

(defun tracker-loop (step pattern buffer)
  "The main loop of the tracker; play STEP in PATTERN in BUFFER with `tracker-step', then queue the next iteration of the loop.

Note that this is an \"internal\" function; users should start/stop the tracker with C-c C-s or `tracker-play-or-stop' instead."
  (when (buffer-live-p buffer)
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
                                 (mod (1+ pattern) (tracker-patterns-count)))))
            (run-with-timer delay nil 'tracker-loop next-step next-pattern buffer)))))))

;;; pattern editing

(defun tracker-insert-steps (steps &optional start)
  "Insert STEPS additional steps at point, numbered starting at START."
  (let ((start (or start 0)))
    (dotimes (step steps)
      (let ((start-point (point)))
        (insert (format "%03d  \n" (+ start step)))
        (tracker-ui-element (- (point) 7) (1- (point)) "Cannot edit step markers; use `tracker-resize-pattern' to change the number of steps in a pattern")))))

(defun tracker-insert-pattern (&optional size) ; FIX: this inserts after the current pattern, but the pattern number is wrong if it's not the last pattern in the buffer
  "Insert a new pattern after the current one containing SIZE steps."
  (interactive
   (if (and current-prefix-arg (not (consp current-prefix-arg)))
       (list (prefix-numeric-value current-prefix-arg))
     (list (read-number "Number of steps: " 16))))
  (let ((inhibit-read-only t)
        (number (tracker-patterns-count)))
    (save-excursion
      (if (= 0 (tracker-patterns-count))
          (goto-char (point-max))
        (tracker-goto-end-of-pattern))
      (or (search-forward "\n\n" nil t)
          (progn
            (ignore-errors (forward-char 2))
            (unless (bolp)
              (insert "\n\n"))))
      (let ((start (point)))
        (insert (concat ";; Pattern " (number-to-string number) ":\n"))
        (tracker-ui-element (- start 1) (point) "Cannot edit tracker-mode template; use `tracker-delete-pattern' to delete patterns"))
      (tracker-insert-steps size)
      (insert "\n"))))

(defun tracker-delete-pattern ()
  "Delete the pattern under point."
  (interactive)
  (if-let ((pattern (tracker-pattern-at-point)))
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

(defun tracker-resize-pattern (steps)
  "Add or remove steps to the pattern under point to make it STEPS steps in length."
  (interactive "NSteps? ")
  (let ((current-num-steps (tracker-steps-count)))
    (tracker-modifying-buffer
     (if (> steps current-num-steps)
         (progn
           (tracker-goto-end-of-pattern)
           (search-forward "\n" nil t)
           (unless (bolp)
             (end-of-line)
             (insert "\n"))
           (tracker-insert-steps (- steps current-num-steps) current-num-steps))
       (delete-region (save-excursion
                        (tracker-goto-step steps)
                        (beginning-of-line)
                        (1- (point)))
                      (save-excursion
                        (tracker-goto-end-of-pattern)
                        (point)))))))

(defun tracker-latch (&optional enable)
  "Turn on or off latching of the currently-playing pattern.  ENABLE should be t or a positive number to turn on, or nil or a non-positive number to turn off.

See also: `tracker-latch-toggle'"
  (interactive "p")
  (setf tracker-latched-p (if (booleanp enable)
                              enable
                            (if (> enable 0) t nil)))
  (tracker-update-header)
  (message (if tracker-latched-p
               (concat "Latched pattern " (number-to-string tracker-current-playing-pattern) ".")
             "Tracker unlatched.")))

(defun tracker-latch-toggle ()
  "Toggle whether to loop the current pattern."
  (interactive)
  (tracker-latch (not tracker-latched-p)))

;;; steps

(defun tracker-step-bounds (&optional step pattern)
  "Get the bounds of the current text in STEP of PATTERN."
  (save-excursion
    (let ((start (progn
                   (tracker-goto-step (or step (tracker-step-at-point)) (or pattern (tracker-pattern-at-point)))
                   (point))))
      (list
       start
       (let ((next-pat-pos (save-excursion
                             (if (search-forward-regexp tracker-pattern-regexp nil t)
                                 (progn
                                   (beginning-of-line)
                                   (point))
                               (point-max)))))
         (max start
              (or (when (search-forward-regexp tracker-step-regexp next-pat-pos t)
                    (backward-sexp 2)
                    (forward-sexp)
                    (point))
                  (save-excursion
                    (goto-char next-pat-pos)
                    (backward-sexp)
                    (forward-sexp)
                    (point)))))))))

(defun tracker-step-string (&optional step pattern)
  "Get the code from STEP in PATTERN as a string."
  (apply #'buffer-substring-no-properties (tracker-step-bounds step pattern)))

(defun tracker-confirm-step ()
  "Confirm edits to the current step."
  (interactive)
  (save-excursion
    (if-let ((step (tracker-step-at-point)))
        (let* ((pattern (tracker-pattern-at-point))
               (elisp (tracker-step-string step pattern)))
          (setf (tracker-confirmed-step step pattern) elisp)
          (tracker-mark-step step pattern 'confirmed))
      (progn
        (beginning-of-line)
        (cond ((looking-at tracker-title-regexp)
               (setf tracker-track-name (match-string-no-properties 1))
               (tracker-update-header))
              ((looking-at tracker-bpm-regexp)
               (setf tracker-bpm (string-to-number (match-string-no-properties 1)))
               (tracker-update-header))
              (t
               (message "The point does not appear to be on a step.")))))))

(defun tracker-revert-step (&optional step pattern)
  "Undo edits to STEP in PATTERN, reverting back to its last confirmed code."
  (interactive)
  (let ((step (or step (tracker-step-at-point)))
        (pattern (or pattern (tracker-pattern-at-point))))
    (apply #'delete-region (tracker-step-bounds step pattern))
    (tracker-goto-step step pattern)
    (insert (or (tracker-confirmed-step step pattern 'string) ""))
    (tracker-mark-step step pattern 'confirmed)))

;;; transport

(defun tracker-play ()
  "Start playing the tracker."
  (interactive)
  (setf tracker-playing-p t)
  (tracker-loop 0 (or (tracker-pattern-at-point) 0) (current-buffer))
  (message "Tracker started."))

(defun tracker-stop ()
  "Stop playing the tracker."
  (interactive)
  (setf tracker-playing-p nil)
  (message "Tracker stopped.")
  (tracker-update-header))

(defun tracker-play-or-stop ()
  "Play the tracker if it is stopped, or stop it if it is playing."
  (interactive)
  (if tracker-playing-p
      (tracker-stop)
    (tracker-play)))

;;; change numbers

(defun tracker-change-number (change)
  "Increase or decrease a number under the point by CHANGE."
  (interactive "p")
  (save-excursion
    (search-backward-regexp "[^0-9-]")
    (forward-char)
    (let* ((start (point))
           (end (1- (save-excursion (search-forward-regexp "[^0-9-]"))))
           (string (buffer-substring-no-properties start end)))
      (unless (eql 0 (- end start))
        (delete-char (- end start))
        (insert (number-to-string (+ (or change 1) (string-to-number string))))))))

(defun tracker-increase-number (&optional increase)
  "Increase the number under the point by INCREASE."
  (interactive "p")
  (tracker-change-number (or increase 1)))

(defun tracker-decrease-number (&optional decrease)
  "Decrease the number under the point by DECREASE."
  (interactive "p")
  (tracker-change-number (- (or decrease 1))))

(defun tracker-back-to-indent ()
  "Move point back to the beginning of the line, skipping forward to the start of the step input if on a step."
  (interactive)
  (beginning-of-line)
  (when (looking-at tracker-step-regexp)
    (forward-char 5)))


;;; keymap

(defvar tracker-mode-map
  (let ((map (make-sparse-keymap "Tracker-Mode")))
    (define-key map (kbd "<M-down>") 'tracker-decrease-number)
    (define-key map (kbd "<M-up>") 'tracker-increase-number)
    (define-key map (kbd "C-c C-n") 'tracker-goto-next-pattern)
    (define-key map (kbd "C-c C-p") 'tracker-goto-previous-pattern)
    ;; (define-key map (kbd "M-n") 'tracker-next-field) ; goes to next step in the pattern, etc
    ;; (define-key map (kbd "M-p") 'tracker-previous-field)
    (define-key map (kbd "C-c C-s") 'tracker-play-or-stop)
    (define-key map (kbd "C-c C-l") 'tracker-latch-toggle)
    (define-key map (kbd "C-c C-i") 'tracker-insert-pattern)
    (define-key map (kbd "C-c C-d") 'tracker-delete-pattern)
    (define-key map (kbd "C-c C-c") 'tracker-confirm-step)
    (define-key map (kbd "C-c C-k") 'tracker-revert-step)
    (define-key map (kbd "M-g b") 'tracker-goto-bpm)
    (define-key map (kbd "M-g s") 'tracker-goto-step)
    (define-key map (kbd "M-g t") 'tracker-goto-title)
    (define-key map (kbd "M-g p") 'tracker-goto-pattern) ; this overrides the original M-g p action, but i don't think you'll use it in tracker mode anyway... (?)
    map)
  "Keymap for `tracker-mode'.")

;;; main functions/initialization

(defun tracker ()
  "Initialize a new buffer for `tracker-mode'."
  (interactive)
  (unless (tracker-buffer-p)
    (get-buffer-create "*Tracker*")
    (switch-to-buffer "*Tracker*"))
  (tracker-mode))

;;;###autoload
(define-derived-mode tracker-mode emacs-lisp-mode "Tracker"
  "Tracker-inspired livecodable sequencer mode for Emacs."
  (use-local-map tracker-mode-map)
  (tracker-write-template)
  (cursor-intangible-mode 1)
  (tracker-make-confirmed-steps-hash)
  (tracker-update-header)
  (add-to-list 'after-change-functions 'tracker-after-change-function)
  (run-hooks 'tracker-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.trk\\'" . tracker-mode))
(add-to-list 'auto-mode-alist '("\\.track\\'" . tracker-mode))

(provide 'tracker-mode)

;;; tracker-mode.el ends here

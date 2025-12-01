;;; skilled-buffers.el --- Dual-window buffer management system -*- lexical-binding: t; -*-

;; Author: Aaron Strick
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, windows
;; URL: https://github.com/strickinato/skilled-buffers

;;; Commentary:
;; A dual-window buffer management system with PRIMARY and SECONDARY windows.
;; Each window has its own ordered list of promoted buffers.
;;
;; The PRIMARY window (left) displays buffers from the PRIMARY ordered list.
;; The SECONDARY window (right) displays buffers from the SECONDARY ordered list,
;; but can also be interrupted by new buffers/popups.
;;
;; Main functions:
;; PRIMARY:
;; - `skilled-buffers-promote-primary': Promote current buffer to PRIMARY
;; - `skilled-buffers-demote-primary': Demote current buffer from PRIMARY
;; - `skilled-buffers-next': Cycle to next PRIMARY buffer
;; - `skilled-buffers-previous': Cycle to previous PRIMARY buffer
;; - `skilled-buffers-switch-to-N': Jump to Nth PRIMARY buffer
;;
;; SECONDARY:
;; - `skilled-buffers-promote-secondary': Promote current buffer to SECONDARY
;; - `skilled-buffers-demote-secondary': Demote current buffer from SECONDARY
;; - `skilled-buffers-secondary-next': Cycle to next SECONDARY buffer
;; - `skilled-buffers-secondary-previous': Cycle to previous SECONDARY buffer
;; - `skilled-buffers-secondary-switch-to-N': Jump to Nth SECONDARY buffer

;;; Code:

(require 'cl-lib)
(require 'tab-line nil t)  ; Load tab-line if available, don't error if missing

;;; Customization

(defgroup skilled-buffers nil
  "Dual-window buffer management system."
  :group 'convenience
  :prefix "skilled-buffers-")

(defcustom skilled-buffers-save-file
  (expand-file-name "skilled-buffers-save.el" user-emacs-directory)
  "File to save promoted buffer list between sessions."
  :type 'file
  :group 'skilled-buffers)

(defcustom skilled-buffers-dashboard-buffer-name "*doom*"
  "Buffer to show in PRIMARY when no buffers are promoted."
  :type 'string
  :group 'skilled-buffers)

;;; Variables

(defvar skilled-buffers-primary-window nil
  "The PRIMARY window (left side).")

(defvar skilled-buffers-secondary-window nil
  "The SECONDARY window (right side).")

(defvar skilled-buffers-primary-list '()
  "Ordered list of buffer objects promoted to PRIMARY window.")

(defvar skilled-buffers-primary-index 0
  "Current index into `skilled-buffers-primary-list'.")

(defvar skilled-buffers-secondary-list '()
  "Ordered list of buffer objects promoted to SECONDARY window.")

(defvar skilled-buffers-secondary-index 0
  "Current index into `skilled-buffers-secondary-list'.")

(defvar skilled-buffers-frame nil
  "The frame where skilled-buffers layout is active.")

;;; Layout Management

(defun skilled-buffers-setup-layout ()
  "Initialize the dual-window layout with PRIMARY (left) and SECONDARY (right).
Creates a 50/50 vertical split and stores window references."
  (interactive)
  (delete-other-windows)
  (setq skilled-buffers-frame (selected-frame))
  
  ;; Create vertical split (50/50)
  (split-window-horizontally)
  
  ;; Store window references
  (setq skilled-buffers-primary-window (frame-first-window))
  (setq skilled-buffers-secondary-window (next-window skilled-buffers-primary-window))
  
  ;; Show dashboard in PRIMARY if no promoted buffers
  (when (null skilled-buffers-primary-list)
    (skilled-buffers--show-dashboard-in-primary))
  
  (message "Skilled buffers layout initialized"))

(defun skilled-buffers-ensure-layout ()
  "Manually restore the dual-window layout if it has been disrupted.
Call this function if you want to restore the layout after changes."
  (interactive)
  (skilled-buffers-setup-layout))

(defun skilled-buffers--active-frame-p ()
  "Return t if current frame is the skilled-buffers frame."
  (eq (selected-frame) skilled-buffers-frame))

(defun skilled-buffers--show-dashboard-in-primary ()
  "Show the dashboard buffer in PRIMARY window."
  (when (and skilled-buffers-primary-window
             (window-live-p skilled-buffers-primary-window))
    (with-selected-window skilled-buffers-primary-window
      (if (get-buffer skilled-buffers-dashboard-buffer-name)
          (switch-to-buffer skilled-buffers-dashboard-buffer-name)
        ;; Fallback to *scratch* if dashboard doesn't exist
        (switch-to-buffer "*scratch*")))))

;;; Buffer Promotion - PRIMARY

(defun skilled-buffers-promote-primary ()
  "Promote current buffer to the PRIMARY window.
If already promoted to PRIMARY, display message.
Buffers can be in both PRIMARY and SECONDARY lists."
  (interactive)
  (let ((buf (current-buffer)))
    (if (memq buf skilled-buffers-primary-list)
        (message "Already promoted to PRIMARY")
      ;; Add to end of list
      (setq skilled-buffers-primary-list (append skilled-buffers-primary-list (list buf)))
      (setq skilled-buffers-primary-index (1- (length skilled-buffers-primary-list)))
      
      ;; Display in PRIMARY
      (skilled-buffers--display-in-primary buf)
      
      (message "Promoted to PRIMARY: %s" (buffer-name buf)))))

(defun skilled-buffers-demote-primary ()
  "Demote current buffer from PRIMARY window.
If currently viewing in PRIMARY, move buffer to SECONDARY and show
previous promoted buffer (or dashboard) in PRIMARY."
  (interactive)
  (let ((buf (current-buffer)))
    (if (not (memq buf skilled-buffers-primary-list))
        (message "Buffer is not promoted to PRIMARY")
      ;; Remove from list
      (setq skilled-buffers-primary-list (delq buf skilled-buffers-primary-list))
      
      ;; Adjust index if necessary
      (when (>= skilled-buffers-primary-index (length skilled-buffers-primary-list))
        (setq skilled-buffers-primary-index (max 0 (1- (length skilled-buffers-primary-list)))))
      
      ;; If we're in PRIMARY, move buffer to SECONDARY
      (when (and (skilled-buffers--active-frame-p)
                 skilled-buffers-primary-window
                 (window-live-p skilled-buffers-primary-window)
                 (eq (selected-window) skilled-buffers-primary-window))
        ;; Show buffer in SECONDARY
        (when (and skilled-buffers-secondary-window
                   (window-live-p skilled-buffers-secondary-window))
          (with-selected-window skilled-buffers-secondary-window
            (switch-to-buffer buf)))
        
        ;; Show next promoted buffer in PRIMARY (or dashboard)
        (if skilled-buffers-primary-list
            (skilled-buffers--display-in-primary 
             (nth skilled-buffers-primary-index skilled-buffers-primary-list))
          (skilled-buffers--show-dashboard-in-primary)))
      
      (message "Demoted from PRIMARY: %s" (buffer-name buf)))))

(defun skilled-buffers-demote-primary-by-name ()
  "Interactively select and demote a PRIMARY promoted buffer."
  (interactive)
  (if (null skilled-buffers-primary-list)
      (message "No PRIMARY promoted buffers")
    (let* ((buffer-names (mapcar #'buffer-name skilled-buffers-primary-list))
           (choice (completing-read "Demote PRIMARY buffer: " buffer-names nil t))
           (buf (get-buffer choice)))
      (when buf
        (with-current-buffer buf
          (skilled-buffers-demote-primary))))))

;; Backward compatibility aliases
(defalias 'skilled-buffers-promote 'skilled-buffers-promote-primary)
(defalias 'skilled-buffers-demote 'skilled-buffers-demote-primary)
(defalias 'skilled-buffers-demote-by-name 'skilled-buffers-demote-primary-by-name)

;;; Buffer Promotion - SECONDARY

(defun skilled-buffers-promote-secondary ()
  "Promote current buffer to the SECONDARY window.
If already promoted to SECONDARY, display message.
Buffers can be in both PRIMARY and SECONDARY lists."
  (interactive)
  (let ((buf (current-buffer)))
    (if (memq buf skilled-buffers-secondary-list)
        (message "Already promoted to SECONDARY")
      ;; Add to end of list
      (setq skilled-buffers-secondary-list (append skilled-buffers-secondary-list (list buf)))
      (setq skilled-buffers-secondary-index (1- (length skilled-buffers-secondary-list)))
      
      ;; Display in SECONDARY
      (skilled-buffers--display-in-secondary buf)
      
      (message "Promoted to SECONDARY: %s" (buffer-name buf)))))

(defun skilled-buffers-demote-secondary ()
  "Demote current buffer from SECONDARY list.
The buffer remains visible in SECONDARY window (whatever was last there stays)."
  (interactive)
  (let ((buf (current-buffer)))
    (if (not (memq buf skilled-buffers-secondary-list))
        (message "Buffer is not promoted to SECONDARY")
      ;; Remove from list
      (setq skilled-buffers-secondary-list (delq buf skilled-buffers-secondary-list))
      
      ;; Adjust index if necessary
      (when (>= skilled-buffers-secondary-index (length skilled-buffers-secondary-list))
        (setq skilled-buffers-secondary-index (max 0 (1- (length skilled-buffers-secondary-list)))))
      
      (message "Demoted from SECONDARY: %s" (buffer-name buf)))))

(defun skilled-buffers-demote-secondary-by-name ()
  "Interactively select and demote a SECONDARY promoted buffer."
  (interactive)
  (if (null skilled-buffers-secondary-list)
      (message "No SECONDARY promoted buffers")
    (let* ((buffer-names (mapcar #'buffer-name skilled-buffers-secondary-list))
           (choice (completing-read "Demote SECONDARY buffer: " buffer-names nil t))
           (buf (get-buffer choice)))
      (when buf
        (with-current-buffer buf
          (skilled-buffers-demote-secondary))))))

(defun skilled-buffers--display-in-primary (buffer)
  "Display BUFFER in PRIMARY window."
  (when (and (skilled-buffers--active-frame-p)
             skilled-buffers-primary-window
             (window-live-p skilled-buffers-primary-window)
             (buffer-live-p buffer))
    (with-selected-window skilled-buffers-primary-window
      (switch-to-buffer buffer))))

(defun skilled-buffers--display-in-secondary (buffer)
  "Display BUFFER in SECONDARY window."
  (when (and (skilled-buffers--active-frame-p)
             skilled-buffers-secondary-window
             (window-live-p skilled-buffers-secondary-window)
             (buffer-live-p buffer))
    (with-selected-window skilled-buffers-secondary-window
      (switch-to-buffer buffer))))

;;; Navigation - PRIMARY

(defun skilled-buffers-next ()
  "Cycle to next promoted buffer in PRIMARY window."
  (interactive)
  (if (null skilled-buffers-primary-list)
      (message "No PRIMARY promoted buffers")
    (setq skilled-buffers-primary-index 
          (mod (1+ skilled-buffers-primary-index) (length skilled-buffers-primary-list)))
    (let ((buf (nth skilled-buffers-primary-index skilled-buffers-primary-list)))
      (skilled-buffers--display-in-primary buf)
      (message "PRIMARY: %s (%d/%d)" 
               (buffer-name buf)
               (1+ skilled-buffers-primary-index)
               (length skilled-buffers-primary-list)))))

(defun skilled-buffers-previous ()
  "Cycle to previous promoted buffer in PRIMARY window."
  (interactive)
  (if (null skilled-buffers-primary-list)
      (message "No PRIMARY promoted buffers")
    (setq skilled-buffers-primary-index 
          (mod (1- skilled-buffers-primary-index) (length skilled-buffers-primary-list)))
    (let ((buf (nth skilled-buffers-primary-index skilled-buffers-primary-list)))
      (skilled-buffers--display-in-primary buf)
      (message "PRIMARY: %s (%d/%d)" 
               (buffer-name buf)
               (1+ skilled-buffers-primary-index)
               (length skilled-buffers-primary-list)))))

(defun skilled-buffers-switch-to-n (n)
  "Switch PRIMARY window to Nth promoted buffer (1-indexed).
Called with numeric prefix argument."
  (interactive "p")
  (if (null skilled-buffers-primary-list)
      (message "No PRIMARY promoted buffers")
    (let ((index (1- n)))
      (if (and (>= index 0) (< index (length skilled-buffers-primary-list)))
          (progn
            (setq skilled-buffers-primary-index index)
            (let ((buf (nth index skilled-buffers-primary-list)))
              (skilled-buffers--display-in-primary buf)
              (message "PRIMARY: %s (%d/%d)" 
                       (buffer-name buf)
                       (1+ index)
                       (length skilled-buffers-primary-list))))
        (message "No PRIMARY buffer at position %d (have %d promoted buffers)" 
                 n (length skilled-buffers-primary-list))))))

;; Convenience functions for direct access
(defun skilled-buffers-switch-to-1 () "Switch to 1st promoted buffer." (interactive) (skilled-buffers-switch-to-n 1))
(defun skilled-buffers-switch-to-2 () "Switch to 2nd promoted buffer." (interactive) (skilled-buffers-switch-to-n 2))
(defun skilled-buffers-switch-to-3 () "Switch to 3rd promoted buffer." (interactive) (skilled-buffers-switch-to-n 3))
(defun skilled-buffers-switch-to-4 () "Switch to 4th promoted buffer." (interactive) (skilled-buffers-switch-to-n 4))
(defun skilled-buffers-switch-to-5 () "Switch to 5th promoted buffer." (interactive) (skilled-buffers-switch-to-n 5))
(defun skilled-buffers-switch-to-6 () "Switch to 6th promoted buffer." (interactive) (skilled-buffers-switch-to-n 6))
(defun skilled-buffers-switch-to-7 () "Switch to 7th promoted buffer." (interactive) (skilled-buffers-switch-to-n 7))
(defun skilled-buffers-switch-to-8 () "Switch to 8th promoted buffer." (interactive) (skilled-buffers-switch-to-n 8))
(defun skilled-buffers-switch-to-9 () "Switch to 9th promoted buffer." (interactive) (skilled-buffers-switch-to-n 9))

;;; Navigation - SECONDARY

(defun skilled-buffers-secondary-next ()
  "Cycle to next promoted buffer in SECONDARY window."
  (interactive)
  (if (null skilled-buffers-secondary-list)
      (message "No SECONDARY promoted buffers")
    (setq skilled-buffers-secondary-index 
          (mod (1+ skilled-buffers-secondary-index) (length skilled-buffers-secondary-list)))
    (let ((buf (nth skilled-buffers-secondary-index skilled-buffers-secondary-list)))
      (skilled-buffers--display-in-secondary buf)
      (message "SECONDARY: %s (%d/%d)" 
               (buffer-name buf)
               (1+ skilled-buffers-secondary-index)
               (length skilled-buffers-secondary-list)))))

(defun skilled-buffers-secondary-previous ()
  "Cycle to previous promoted buffer in SECONDARY window."
  (interactive)
  (if (null skilled-buffers-secondary-list)
      (message "No SECONDARY promoted buffers")
    (setq skilled-buffers-secondary-index 
          (mod (1- skilled-buffers-secondary-index) (length skilled-buffers-secondary-list)))
    (let ((buf (nth skilled-buffers-secondary-index skilled-buffers-secondary-list)))
      (skilled-buffers--display-in-secondary buf)
      (message "SECONDARY: %s (%d/%d)" 
               (buffer-name buf)
               (1+ skilled-buffers-secondary-index)
               (length skilled-buffers-secondary-list)))))

(defun skilled-buffers-secondary-switch-to-n (n)
  "Switch SECONDARY window to Nth promoted buffer (1-indexed).
Called with numeric prefix argument."
  (interactive "p")
  (if (null skilled-buffers-secondary-list)
      (message "No SECONDARY promoted buffers")
    (let ((index (1- n)))
      (if (and (>= index 0) (< index (length skilled-buffers-secondary-list)))
          (progn
            (setq skilled-buffers-secondary-index index)
            (let ((buf (nth index skilled-buffers-secondary-list)))
              (skilled-buffers--display-in-secondary buf)
              (message "SECONDARY: %s (%d/%d)" 
                       (buffer-name buf)
                       (1+ index)
                       (length skilled-buffers-secondary-list))))
        (message "No SECONDARY buffer at position %d (have %d promoted buffers)" 
                 n (length skilled-buffers-secondary-list))))))

;; Convenience functions for direct access to SECONDARY
(defun skilled-buffers-secondary-switch-to-1 () "Switch SECONDARY to 1st promoted buffer." (interactive) (skilled-buffers-secondary-switch-to-n 1))
(defun skilled-buffers-secondary-switch-to-2 () "Switch SECONDARY to 2nd promoted buffer." (interactive) (skilled-buffers-secondary-switch-to-n 2))
(defun skilled-buffers-secondary-switch-to-3 () "Switch SECONDARY to 3rd promoted buffer." (interactive) (skilled-buffers-secondary-switch-to-n 3))
(defun skilled-buffers-secondary-switch-to-4 () "Switch SECONDARY to 4th promoted buffer." (interactive) (skilled-buffers-secondary-switch-to-n 4))
(defun skilled-buffers-secondary-switch-to-5 () "Switch SECONDARY to 5th promoted buffer." (interactive) (skilled-buffers-secondary-switch-to-n 5))
(defun skilled-buffers-secondary-switch-to-6 () "Switch SECONDARY to 6th promoted buffer." (interactive) (skilled-buffers-secondary-switch-to-n 6))
(defun skilled-buffers-secondary-switch-to-7 () "Switch SECONDARY to 7th promoted buffer." (interactive) (skilled-buffers-secondary-switch-to-n 7))
(defun skilled-buffers-secondary-switch-to-8 () "Switch SECONDARY to 8th promoted buffer." (interactive) (skilled-buffers-secondary-switch-to-n 8))
(defun skilled-buffers-secondary-switch-to-9 () "Switch SECONDARY to 9th promoted buffer." (interactive) (skilled-buffers-secondary-switch-to-n 9))

;;; Window Focus

(defun skilled-buffers-jump-to-primary ()
  "Move point/focus to PRIMARY window."
  (interactive)
  (when (and (skilled-buffers--active-frame-p)
             skilled-buffers-primary-window
             (window-live-p skilled-buffers-primary-window))
    (select-window skilled-buffers-primary-window)))

(defun skilled-buffers-jump-to-secondary ()
  "Move point/focus to SECONDARY window."
  (interactive)
  (when (and (skilled-buffers--active-frame-p)
             skilled-buffers-secondary-window
             (window-live-p skilled-buffers-secondary-window))
    (select-window skilled-buffers-secondary-window)))

;;; Buffer List Management

(defun skilled-buffers-show-list ()
  "Display the ordered lists of promoted buffers for both PRIMARY and SECONDARY."
  (interactive)
  (if (and (null skilled-buffers-primary-list) 
           (null skilled-buffers-secondary-list))
      (message "No promoted buffers")
    (let ((msg "Skilled Buffers:\n\n"))
      ;; PRIMARY list
      (setq msg (concat msg "PRIMARY:\n"))
      (if (null skilled-buffers-primary-list)
          (setq msg (concat msg "  (empty)\n"))
        (setq msg (concat msg
                          (mapconcat
                           (lambda (i)
                             (let ((buf (nth i skilled-buffers-primary-list)))
                               (format "  %d. %s%s"
                                       (1+ i)
                                       (if (= i skilled-buffers-primary-index) "[current] → " "")
                                       (buffer-name buf))))
                           (number-sequence 0 (1- (length skilled-buffers-primary-list)))
                           "\n")
                          "\n")))
      
      ;; SECONDARY list
      (setq msg (concat msg "\nSECONDARY:\n"))
      (if (null skilled-buffers-secondary-list)
          (setq msg (concat msg "  (empty)\n"))
        (setq msg (concat msg
                          (mapconcat
                           (lambda (i)
                             (let ((buf (nth i skilled-buffers-secondary-list)))
                               (format "  %d. %s%s"
                                       (1+ i)
                                       (if (= i skilled-buffers-secondary-index) "[current] → " "")
                                       (buffer-name buf))))
                           (number-sequence 0 (1- (length skilled-buffers-secondary-list)))
                           "\n"))))
      
      (message "%s" msg))))

(defun skilled-buffers-clear-primary ()
  "Remove all promoted buffers from the PRIMARY list."
  (interactive)
  (when (yes-or-no-p "Clear all PRIMARY promoted buffers? ")
    (setq skilled-buffers-primary-list '())
    (setq skilled-buffers-primary-index 0)
    (skilled-buffers--show-dashboard-in-primary)
    (message "Cleared all PRIMARY promoted buffers")))

(defun skilled-buffers-clear-secondary ()
  "Remove all promoted buffers from the SECONDARY list."
  (interactive)
  (when (yes-or-no-p "Clear all SECONDARY promoted buffers? ")
    (setq skilled-buffers-secondary-list '())
    (setq skilled-buffers-secondary-index 0)
    (message "Cleared all SECONDARY promoted buffers")))

(defun skilled-buffers-clear ()
  "Remove all promoted buffers from both PRIMARY and SECONDARY lists."
  (interactive)
  (when (yes-or-no-p "Clear all promoted buffers (PRIMARY and SECONDARY)? ")
    (setq skilled-buffers-primary-list '())
    (setq skilled-buffers-primary-index 0)
    (setq skilled-buffers-secondary-list '())
    (setq skilled-buffers-secondary-index 0)
    (skilled-buffers--show-dashboard-in-primary)
    (message "Cleared all promoted buffers")))

(defun skilled-buffers-reorder-up ()
  "Move current buffer up one position in the PRIMARY promoted list."
  (interactive)
  (let* ((buf (current-buffer))
         (pos (cl-position buf skilled-buffers-primary-list)))
    (if (not pos)
        (message "Buffer is not promoted to PRIMARY")
      (if (= pos 0)
          (message "Already at top of PRIMARY list")
        ;; Swap with previous
        (let ((prev-buf (nth (1- pos) skilled-buffers-primary-list)))
          (setf (nth (1- pos) skilled-buffers-primary-list) buf)
          (setf (nth pos skilled-buffers-primary-list) prev-buf)
          (when (= skilled-buffers-primary-index pos)
            (setq skilled-buffers-primary-index (1- pos)))
          (message "Moved %s up in PRIMARY" (buffer-name buf)))))))

(defun skilled-buffers-reorder-down ()
  "Move current buffer down one position in the PRIMARY promoted list."
  (interactive)
  (let* ((buf (current-buffer))
         (pos (cl-position buf skilled-buffers-primary-list)))
    (if (not pos)
        (message "Buffer is not promoted to PRIMARY")
      (if (= pos (1- (length skilled-buffers-primary-list)))
          (message "Already at bottom of PRIMARY list")
        ;; Swap with next
        (let ((next-buf (nth (1+ pos) skilled-buffers-primary-list)))
          (setf (nth (1+ pos) skilled-buffers-primary-list) buf)
          (setf (nth pos skilled-buffers-primary-list) next-buf)
          (when (= skilled-buffers-primary-index pos)
            (setq skilled-buffers-primary-index (1+ pos)))
          (message "Moved %s down in PRIMARY" (buffer-name buf)))))))

(defun skilled-buffers-secondary-reorder-up ()
  "Move current buffer up one position in the SECONDARY promoted list."
  (interactive)
  (let* ((buf (current-buffer))
         (pos (cl-position buf skilled-buffers-secondary-list)))
    (if (not pos)
        (message "Buffer is not promoted to SECONDARY")
      (if (= pos 0)
          (message "Already at top of SECONDARY list")
        ;; Swap with previous
        (let ((prev-buf (nth (1- pos) skilled-buffers-secondary-list)))
          (setf (nth (1- pos) skilled-buffers-secondary-list) buf)
          (setf (nth pos skilled-buffers-secondary-list) prev-buf)
          (when (= skilled-buffers-secondary-index pos)
            (setq skilled-buffers-secondary-index (1- pos)))
          (message "Moved %s up in SECONDARY" (buffer-name buf)))))))

(defun skilled-buffers-secondary-reorder-down ()
  "Move current buffer down one position in the SECONDARY promoted list."
  (interactive)
  (let* ((buf (current-buffer))
         (pos (cl-position buf skilled-buffers-secondary-list)))
    (if (not pos)
        (message "Buffer is not promoted to SECONDARY")
      (if (= pos (1- (length skilled-buffers-secondary-list)))
          (message "Already at bottom of SECONDARY list")
        ;; Swap with next
        (let ((next-buf (nth (1+ pos) skilled-buffers-secondary-list)))
          (setf (nth (1+ pos) skilled-buffers-secondary-list) buf)
          (setf (nth pos skilled-buffers-secondary-list) next-buf)
          (when (= skilled-buffers-secondary-index pos)
            (setq skilled-buffers-secondary-index (1+ pos)))
          (message "Moved %s down in SECONDARY" (buffer-name buf)))))))

;;; Cleanup

(defun skilled-buffers-cleanup ()
  "Remove dead buffers from both PRIMARY and SECONDARY promoted lists."
  (let ((primary-before (length skilled-buffers-primary-list))
        (secondary-before (length skilled-buffers-secondary-list)))
    
    ;; Clean PRIMARY list
    (setq skilled-buffers-primary-list 
          (cl-remove-if-not #'buffer-live-p skilled-buffers-primary-list))
    
    ;; Adjust PRIMARY index if necessary
    (when (>= skilled-buffers-primary-index (length skilled-buffers-primary-list))
      (setq skilled-buffers-primary-index (max 0 (1- (length skilled-buffers-primary-list)))))
    
    ;; Clean SECONDARY list
    (setq skilled-buffers-secondary-list 
          (cl-remove-if-not #'buffer-live-p skilled-buffers-secondary-list))
    
    ;; Adjust SECONDARY index if necessary
    (when (>= skilled-buffers-secondary-index (length skilled-buffers-secondary-list))
      (setq skilled-buffers-secondary-index (max 0 (1- (length skilled-buffers-secondary-list)))))
    
    (let ((primary-removed (- primary-before (length skilled-buffers-primary-list)))
          (secondary-removed (- secondary-before (length skilled-buffers-secondary-list))))
      (when (or (> primary-removed 0) (> secondary-removed 0))
        (message "Removed %d dead buffer(s) from PRIMARY, %d from SECONDARY" 
                 primary-removed secondary-removed)
        
        ;; If we're in PRIMARY and current buffer was removed, show next
        (when (and (skilled-buffers--active-frame-p)
                   skilled-buffers-primary-window
                   (window-live-p skilled-buffers-primary-window)
                   (eq (selected-window) skilled-buffers-primary-window))
          (if skilled-buffers-primary-list
              (skilled-buffers--display-in-primary 
               (nth skilled-buffers-primary-index skilled-buffers-primary-list))
            (skilled-buffers--show-dashboard-in-primary)))))))

(defun skilled-buffers--kill-buffer-hook ()
  "Hook function to clean up when a buffer is killed."
  (when (or (memq (current-buffer) skilled-buffers-primary-list)
            (memq (current-buffer) skilled-buffers-secondary-list))
    (skilled-buffers-cleanup)))

;;; Persistence

(defun skilled-buffers-save-list ()
  "Save both PRIMARY and SECONDARY promoted buffer lists to file."
  (interactive)
  (let ((primary-file-names 
         (cl-remove-if #'null
                       (mapcar (lambda (buf)
                                 (buffer-file-name buf))
                               skilled-buffers-primary-list)))
        (secondary-file-names 
         (cl-remove-if #'null
                       (mapcar (lambda (buf)
                                 (buffer-file-name buf))
                               skilled-buffers-secondary-list))))
    (with-temp-file skilled-buffers-save-file
      (insert ";; Skilled Buffers Save File\n")
      (insert ";; Auto-generated - do not edit manually\n\n")
      (insert "(setq skilled-buffers-saved-primary-files\n  '(")
      (insert (mapconcat (lambda (f) (format "%S" f))
                         primary-file-names
                         "\n    "))
      (insert "))\n\n")
      (insert "(setq skilled-buffers-saved-secondary-files\n  '(")
      (insert (mapconcat (lambda (f) (format "%S" f))
                         secondary-file-names
                         "\n    "))
      (insert "))\n"))
    (message "Saved %d PRIMARY, %d SECONDARY buffer(s)" 
             (length primary-file-names)
             (length secondary-file-names))))

(defun skilled-buffers-restore-list ()
  "Restore both PRIMARY and SECONDARY promoted buffer lists from file."
  (interactive)
  (when (file-exists-p skilled-buffers-save-file)
    (load skilled-buffers-save-file t t)
    (let ((primary-restored 0)
          (secondary-restored 0))
      
      ;; Restore PRIMARY list
      (when (boundp 'skilled-buffers-saved-primary-files)
        (dolist (file skilled-buffers-saved-primary-files)
          (when (file-exists-p file)
            (let ((buf (find-file-noselect file)))
              (unless (memq buf skilled-buffers-primary-list)
                (setq skilled-buffers-primary-list 
                      (append skilled-buffers-primary-list (list buf)))
                (setq primary-restored (1+ primary-restored))))))
        (setq skilled-buffers-primary-index 0)
        (when (and (> primary-restored 0) skilled-buffers-primary-list)
          (skilled-buffers--display-in-primary 
           (car skilled-buffers-primary-list))))
      
      ;; Restore SECONDARY list
      (when (boundp 'skilled-buffers-saved-secondary-files)
        (dolist (file skilled-buffers-saved-secondary-files)
          (when (file-exists-p file)
            (let ((buf (find-file-noselect file)))
              (unless (memq buf skilled-buffers-secondary-list)
                (setq skilled-buffers-secondary-list 
                      (append skilled-buffers-secondary-list (list buf)))
                (setq secondary-restored (1+ secondary-restored))))))
        (setq skilled-buffers-secondary-index 0))
      
      (when (or (> primary-restored 0) (> secondary-restored 0))
        (message "Restored %d PRIMARY, %d SECONDARY buffer(s)" 
                 primary-restored secondary-restored)))))

;;; Display Buffer Integration

(defun skilled-buffers--display-buffer-action (buffer alist)
  "Custom display buffer action for routing buffers to correct window.
BUFFER is the buffer to display, ALIST is the action alist.

Routing rules:
- PRIMARY list buffers → PRIMARY window
- SECONDARY list buffers → SECONDARY window (when explicitly switched to)
- All other buffers → SECONDARY window (default)"
  (when (skilled-buffers--active-frame-p)
    (cond
     ;; PRIMARY list buffers go to PRIMARY window
     ((memq buffer skilled-buffers-primary-list)
      (when (and skilled-buffers-primary-window
                 (window-live-p skilled-buffers-primary-window))
        (window--display-buffer buffer skilled-buffers-primary-window 'reuse alist)))
     
     ;; All other buffers (including SECONDARY list) go to SECONDARY window
     ;; SECONDARY list buffers are shown when explicitly requested via navigation functions
     (t
      (when (and skilled-buffers-secondary-window
                 (window-live-p skilled-buffers-secondary-window))
        (window--display-buffer buffer skilled-buffers-secondary-window 'reuse alist))))))

(defun skilled-buffers--window-buffer-change-function (frame)
  "Hook function called when window buffers change.
Ensures buffers are displayed in the correct window according to promotion rules.
This catches ALL methods of opening/switching buffers (projectile, find-file, etc)."
  (when (and (eq frame skilled-buffers-frame)
             (skilled-buffers--active-frame-p))
    (let ((primary-buffer (when (and skilled-buffers-primary-window
                                     (window-live-p skilled-buffers-primary-window))
                            (window-buffer skilled-buffers-primary-window)))
          (secondary-buffer (when (and skilled-buffers-secondary-window
                                       (window-live-p skilled-buffers-secondary-window))
                              (window-buffer skilled-buffers-secondary-window))))
      
      ;; Check if PRIMARY window has a buffer that shouldn't be there
      (when (and primary-buffer
                 (not (memq primary-buffer skilled-buffers-primary-list)))
        ;; Move this buffer to SECONDARY
        (when (and skilled-buffers-secondary-window
                   (window-live-p skilled-buffers-secondary-window))
          (set-window-buffer skilled-buffers-secondary-window primary-buffer))
        ;; Show appropriate buffer in PRIMARY
        (if skilled-buffers-primary-list
            (set-window-buffer skilled-buffers-primary-window
                               (nth skilled-buffers-primary-index skilled-buffers-primary-list))
          (skilled-buffers--show-dashboard-in-primary)))
      
      ;; Check if SECONDARY window has a PRIMARY buffer that should be in PRIMARY
      (when (and secondary-buffer
                 (memq secondary-buffer skilled-buffers-primary-list))
        ;; This buffer belongs in PRIMARY
        (when (and skilled-buffers-primary-window
                   (window-live-p skilled-buffers-primary-window))
          (set-window-buffer skilled-buffers-primary-window secondary-buffer)
          ;; Update PRIMARY index to match
          (let ((idx (cl-position secondary-buffer skilled-buffers-primary-list)))
            (when idx
              (setq skilled-buffers-primary-index idx)))
          ;; Show previous SECONDARY buffer or a SECONDARY promoted buffer
          (when (and skilled-buffers-secondary-window
                     (window-live-p skilled-buffers-secondary-window))
            (if skilled-buffers-secondary-list
                (set-window-buffer skilled-buffers-secondary-window
                                   (nth skilled-buffers-secondary-index skilled-buffers-secondary-list))
              ;; Just leave whatever was there before in SECONDARY
              nil)))))))

;;; Tab Line Integration

(defface skilled-buffers-tab-selected
  '((t :inherit tab-line-tab-current
     :box (:line-width 2 :color nil :style nil)))
  "Face for selected tab in skilled-buffers."
  :group 'skilled-buffers)

(defface skilled-buffers-tab-unselected
  '((t :inherit tab-line-tab-inactive
     :box (:line-width 1 :color nil :style nil)))
  "Face for unselected tabs in skilled-buffers."
  :group 'skilled-buffers)

(defface skilled-buffers-tab-ephemeral
  '((t :inherit tab-line-tab-inactive
     :slant italic
     :box (:line-width 1 :color nil :style nil)))
  "Face for ephemeral tabs (current buffer not in promoted list) in skilled-buffers."
  :group 'skilled-buffers)

(defun skilled-buffers--tab-line-tabs ()
  "Generate tab list for current window based on PRIMARY or SECONDARY list."
  (when (skilled-buffers--active-frame-p)
    (let* ((current-window (selected-window))
           (current-buffer (window-buffer current-window))
           (is-primary (eq current-window skilled-buffers-primary-window))
           (is-secondary (eq current-window skilled-buffers-secondary-window))
           (buffer-list (cond
                         (is-primary skilled-buffers-primary-list)
                         (is-secondary skilled-buffers-secondary-list)
                         (t nil))))
      (when (or is-primary is-secondary)
        ;; Show promoted buffers as tabs
        (let ((tabs (mapcar (lambda (buf)
                              `(tab
                                (name . ,(buffer-name buf))
                                (buffer . ,buf)
                                (selected . ,(eq buf current-buffer))))
                            (cl-remove-if-not #'buffer-live-p buffer-list))))
          ;; If in SECONDARY and current buffer is not in the list, add ephemeral tab
          (when (and is-secondary
                     (not (memq current-buffer skilled-buffers-secondary-list))
                     (buffer-live-p current-buffer))
            (setq tabs (append tabs
                               `((tab
                                  (name . ,(buffer-name current-buffer))
                                  (buffer . ,current-buffer)
                                  (selected . t)
                                  (ephemeral . t))))))
          tabs)))))

(defun skilled-buffers--tab-line-tab-name-function (tab tabs)
  "Format a single tab for display. No close button."
  (let* ((buffer (alist-get 'buffer tab))
         (name (alist-get 'name tab))
         (selected (alist-get 'selected tab))
         (ephemeral (alist-get 'ephemeral tab))
         (face (cond
                (ephemeral 'skilled-buffers-tab-ephemeral)
                (selected 'skilled-buffers-tab-selected)
                (t 'skilled-buffers-tab-unselected))))
    (propertize (concat " " name " ")
                'face face
                'mouse-face 'tab-line-highlight
                'keymap (let ((map (make-sparse-keymap)))
                          (define-key map [tab-line mouse-1]
                                      `(lambda () (interactive)
                                         (when (buffer-live-p ,buffer)
                                           (switch-to-buffer ,buffer))))
                          map))))

(defun skilled-buffers--setup-tab-line ()
  "Setup tab-line for skilled-buffers windows."
  (when (and (skilled-buffers--active-frame-p)
             (fboundp 'tab-line-mode))
    ;; Enable tab-line in PRIMARY window
    (when (and skilled-buffers-primary-window
               (window-live-p skilled-buffers-primary-window))
      (with-selected-window skilled-buffers-primary-window
        (setq-local tab-line-tabs-function #'skilled-buffers--tab-line-tabs)
        (setq-local tab-line-tab-name-function #'skilled-buffers--tab-line-tab-name-function)
        (setq-local tab-line-close-button-show nil)
        (setq-local tab-line-new-button-show nil)
        (tab-line-mode 1)))
    
    ;; Enable tab-line in SECONDARY window
    (when (and skilled-buffers-secondary-window
               (window-live-p skilled-buffers-secondary-window))
      (with-selected-window skilled-buffers-secondary-window
        (setq-local tab-line-tabs-function #'skilled-buffers--tab-line-tabs)
        (setq-local tab-line-tab-name-function #'skilled-buffers--tab-line-tab-name-function)
        (setq-local tab-line-close-button-show nil)
        (setq-local tab-line-new-button-show nil)
        (tab-line-mode 1)))))

(defun skilled-buffers--update-tab-line ()
  "Force update of tab-line display in both windows."
  (when (and (skilled-buffers--active-frame-p)
             (fboundp 'tab-line-mode))
    (dolist (win (list skilled-buffers-primary-window skilled-buffers-secondary-window))
      (when (and win (window-live-p win))
        (with-selected-window win
          (when tab-line-mode
            (setq-local tab-line-tabs-function #'skilled-buffers--tab-line-tabs)
            (setq-local tab-line-tab-name-function #'skilled-buffers--tab-line-tab-name-function)
            (setq-local tab-line-close-button-show nil)
            (setq-local tab-line-new-button-show nil))
          (force-mode-line-update))))))

;;; Minor Mode

;;;###autoload
(define-minor-mode skilled-buffers-mode
  "Toggle skilled-buffers dual-window management system."
  :global t
  :group 'skilled-buffers
  :lighter " Skilled"
  (if skilled-buffers-mode
      (progn
        ;; Enable
        (add-hook 'kill-buffer-hook #'skilled-buffers--kill-buffer-hook)
        (add-hook 'kill-emacs-hook #'skilled-buffers-save-list)
        (add-hook 'buffer-list-update-hook #'skilled-buffers--update-tab-line)
        
        ;; Add window-buffer-change hook to catch ALL buffer switches
        ;; This handles projectile, find-file, switch-to-buffer, and any other method
        (add-hook 'window-buffer-change-functions #'skilled-buffers--window-buffer-change-function)
        
        ;; Add display buffer action at the BEGINNING (highest priority)
        ;; This ensures skilled-buffers takes precedence over other rules (magit, etc.)
        ;; Use ^.* to properly match all buffer names from the start
        (push '("^.*" skilled-buffers--display-buffer-action)
              display-buffer-alist)
        
        ;; Setup tab-line
        (skilled-buffers--setup-tab-line)
        
        (message "Skilled buffers mode enabled"))
    
    ;; Disable
    (remove-hook 'kill-buffer-hook #'skilled-buffers--kill-buffer-hook)
    (remove-hook 'kill-emacs-hook #'skilled-buffers-save-list)
    (remove-hook 'buffer-list-update-hook #'skilled-buffers--update-tab-line)
    (remove-hook 'window-buffer-change-functions #'skilled-buffers--window-buffer-change-function)
    
    ;; Remove display buffer action
    (setq display-buffer-alist
          (cl-remove-if (lambda (entry)
                          (eq (cadr entry) 'skilled-buffers--display-buffer-action))
                        display-buffer-alist))
    
    ;; Disable tab-line in both windows
    (when (fboundp 'tab-line-mode)
      (when (and skilled-buffers-primary-window
                 (window-live-p skilled-buffers-primary-window))
        (with-selected-window skilled-buffers-primary-window
          (tab-line-mode -1)))
      (when (and skilled-buffers-secondary-window
                 (window-live-p skilled-buffers-secondary-window))
        (with-selected-window skilled-buffers-secondary-window
          (tab-line-mode -1))))
    
    (message "Skilled buffers mode disabled")))

(provide 'skilled-buffers)
;;; skilled-buffers.el ends here

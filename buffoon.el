;;; buffoon.el --- Dual-window buffer management system -*- lexical-binding: t; -*-

;; Author: Aaron Strick
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, windows
;; URL: https://github.com/strickinato/buffoon.el

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
;; - `buffoon-promote-primary': Promote current buffer to PRIMARY
;; - `buffoon-demote-primary': Demote current buffer from PRIMARY
;; - `buffoon-next': Cycle to next PRIMARY buffer
;; - `buffoon-previous': Cycle to previous PRIMARY buffer
;; - `buffoon-switch-to-N': Jump to Nth PRIMARY buffer
;;
;; SECONDARY:
;; - `buffoon-promote-secondary': Promote current buffer to SECONDARY
;; - `buffoon-demote-secondary': Demote current buffer from SECONDARY
;; - `buffoon-secondary-next': Cycle to next SECONDARY buffer
;; - `buffoon-secondary-previous': Cycle to previous SECONDARY buffer
;; - `buffoon-secondary-switch-to-N': Jump to Nth SECONDARY buffer

;;; Code:

(require 'cl-lib)
(require 'tab-line nil t)  ; Load tab-line if available, don't error if missing

;;; Customization

(defgroup buffoon nil
  "Dual-window buffer management system."
  :group 'convenience
  :prefix "buffoon-")

(defcustom buffoon-save-file
  (expand-file-name "buffoon-save.el" user-emacs-directory)
  "File to save promoted buffer list between sessions."
  :type 'file
  :group 'buffoon)

(defcustom buffoon-dashboard-buffer-name "*doom*"
  "Buffer to show in PRIMARY when no buffers are promoted."
  :type 'string
  :group 'buffoon)

;;; Variables

(defvar buffoon-primary-window nil
  "The PRIMARY window (left side).")

(defvar buffoon-secondary-window nil
  "The SECONDARY window (right side).")

(defvar buffoon-primary-list '()
  "Ordered list of buffer objects promoted to PRIMARY window.")

(defvar buffoon-primary-index 0
  "Current index into `buffoon-primary-list'.")

(defvar buffoon-secondary-list '()
  "Ordered list of buffer objects promoted to SECONDARY window.")

(defvar buffoon-secondary-index 0
  "Current index into `buffoon-secondary-list'.")

(defvar buffoon-frame nil
  "The frame where buffoon layout is active.")

;;; Layout Management

(defun buffoon-setup-layout ()
  "Initialize the dual-window layout with PRIMARY (left) and SECONDARY (right).
Creates a 50/50 vertical split and stores window references."
  (interactive)
  (delete-other-windows)
  (setq buffoon-frame (selected-frame))
  
  ;; Create vertical split (50/50)
  (split-window-horizontally)
  
  ;; Store window references
  (setq buffoon-primary-window (frame-first-window))
  (setq buffoon-secondary-window (next-window buffoon-primary-window))
  
  ;; Show dashboard in PRIMARY if no promoted buffers
  (when (null buffoon-primary-list)
    (buffoon--show-dashboard-in-primary))
  
  (message "Skilled buffers layout initialized"))

(defun buffoon-ensure-layout ()
  "Manually restore the dual-window layout if it has been disrupted.
Call this function if you want to restore the layout after changes."
  (interactive)
  (buffoon-setup-layout))

(defun buffoon--active-frame-p ()
  "Return t if current frame is the buffoon frame."
  (eq (selected-frame) buffoon-frame))

(defun buffoon--show-dashboard-in-primary ()
  "Show the dashboard buffer in PRIMARY window."
  (when (and buffoon-primary-window
             (window-live-p buffoon-primary-window))
    (with-selected-window buffoon-primary-window
      (if (get-buffer buffoon-dashboard-buffer-name)
          (switch-to-buffer buffoon-dashboard-buffer-name)
        ;; Fallback to *scratch* if dashboard doesn't exist
        (switch-to-buffer "*scratch*")))))

;;; Buffer Promotion - PRIMARY

(defun buffoon-promote-primary ()
  "Promote current buffer to the PRIMARY window.
If already promoted to PRIMARY, display message.
Buffers can be in both PRIMARY and SECONDARY lists."
  (interactive)
  (let ((buf (current-buffer)))
    (if (memq buf buffoon-primary-list)
        (message "Already promoted to PRIMARY")
      ;; Add to end of list
      (setq buffoon-primary-list (append buffoon-primary-list (list buf)))
      (setq buffoon-primary-index (1- (length buffoon-primary-list)))
      
      ;; Display in PRIMARY
      (buffoon--display-in-primary buf)
      
      (message "Promoted to PRIMARY: %s" (buffer-name buf)))))

(defun buffoon-demote-primary ()
  "Demote current buffer from PRIMARY window.
If currently viewing in PRIMARY, move buffer to SECONDARY and show
previous promoted buffer (or dashboard) in PRIMARY."
  (interactive)
  (let ((buf (current-buffer)))
    (if (not (memq buf buffoon-primary-list))
        (message "Buffer is not promoted to PRIMARY")
      ;; Remove from list
      (setq buffoon-primary-list (delq buf buffoon-primary-list))
      
      ;; Adjust index if necessary
      (when (>= buffoon-primary-index (length buffoon-primary-list))
        (setq buffoon-primary-index (max 0 (1- (length buffoon-primary-list)))))
      
      ;; If we're in PRIMARY, move buffer to SECONDARY
      (when (and (buffoon--active-frame-p)
                 buffoon-primary-window
                 (window-live-p buffoon-primary-window)
                 (eq (selected-window) buffoon-primary-window))
        ;; Show buffer in SECONDARY
        (when (and buffoon-secondary-window
                   (window-live-p buffoon-secondary-window))
          (with-selected-window buffoon-secondary-window
            (switch-to-buffer buf)))
        
        ;; Show next promoted buffer in PRIMARY (or dashboard)
        (if buffoon-primary-list
            (buffoon--display-in-primary 
             (nth buffoon-primary-index buffoon-primary-list))
          (buffoon--show-dashboard-in-primary)))
      
      (message "Demoted from PRIMARY: %s" (buffer-name buf)))))

(defun buffoon-demote-primary-by-name ()
  "Interactively select and demote a PRIMARY promoted buffer."
  (interactive)
  (if (null buffoon-primary-list)
      (message "No PRIMARY promoted buffers")
    (let* ((buffer-names (mapcar #'buffer-name buffoon-primary-list))
           (choice (completing-read "Demote PRIMARY buffer: " buffer-names nil t))
           (buf (get-buffer choice)))
      (when buf
        (with-current-buffer buf
          (buffoon-demote-primary))))))

;; Backward compatibility aliases
(defalias 'buffoon-promote 'buffoon-promote-primary)
(defalias 'buffoon-demote 'buffoon-demote-primary)
(defalias 'buffoon-demote-by-name 'buffoon-demote-primary-by-name)

;;; Buffer Promotion - SECONDARY

(defun buffoon-promote-secondary ()
  "Promote current buffer to the SECONDARY window.
If already promoted to SECONDARY, display message.
Buffers can be in both PRIMARY and SECONDARY lists."
  (interactive)
  (let ((buf (current-buffer)))
    (if (memq buf buffoon-secondary-list)
        (message "Already promoted to SECONDARY")
      ;; Add to end of list
      (setq buffoon-secondary-list (append buffoon-secondary-list (list buf)))
      (setq buffoon-secondary-index (1- (length buffoon-secondary-list)))
      
      ;; Display in SECONDARY
      (buffoon--display-in-secondary buf)
      
      (message "Promoted to SECONDARY: %s" (buffer-name buf)))))

(defun buffoon-demote-secondary ()
  "Demote current buffer from SECONDARY list.
The buffer remains visible in SECONDARY window (whatever was last there stays)."
  (interactive)
  (let ((buf (current-buffer)))
    (if (not (memq buf buffoon-secondary-list))
        (message "Buffer is not promoted to SECONDARY")
      ;; Remove from list
      (setq buffoon-secondary-list (delq buf buffoon-secondary-list))
      
      ;; Adjust index if necessary
      (when (>= buffoon-secondary-index (length buffoon-secondary-list))
        (setq buffoon-secondary-index (max 0 (1- (length buffoon-secondary-list)))))
      
      (message "Demoted from SECONDARY: %s" (buffer-name buf)))))

(defun buffoon-demote-secondary-by-name ()
  "Interactively select and demote a SECONDARY promoted buffer."
  (interactive)
  (if (null buffoon-secondary-list)
      (message "No SECONDARY promoted buffers")
    (let* ((buffer-names (mapcar #'buffer-name buffoon-secondary-list))
           (choice (completing-read "Demote SECONDARY buffer: " buffer-names nil t))
           (buf (get-buffer choice)))
      (when buf
        (with-current-buffer buf
          (buffoon-demote-secondary))))))

(defun buffoon--display-in-primary (buffer)
  "Display BUFFER in PRIMARY window."
  (when (and (buffoon--active-frame-p)
             buffoon-primary-window
             (window-live-p buffoon-primary-window)
             (buffer-live-p buffer))
    (with-selected-window buffoon-primary-window
      (switch-to-buffer buffer))))

(defun buffoon--display-in-secondary (buffer)
  "Display BUFFER in SECONDARY window."
  (when (and (buffoon--active-frame-p)
             buffoon-secondary-window
             (window-live-p buffoon-secondary-window)
             (buffer-live-p buffer))
    (with-selected-window buffoon-secondary-window
      (switch-to-buffer buffer))))

;;; Navigation - PRIMARY

(defun buffoon-next ()
  "Cycle to next promoted buffer in PRIMARY window."
  (interactive)
  (if (null buffoon-primary-list)
      (message "No PRIMARY promoted buffers")
    (setq buffoon-primary-index 
          (mod (1+ buffoon-primary-index) (length buffoon-primary-list)))
    (let ((buf (nth buffoon-primary-index buffoon-primary-list)))
      (buffoon--display-in-primary buf)
      (message "PRIMARY: %s (%d/%d)" 
               (buffer-name buf)
               (1+ buffoon-primary-index)
               (length buffoon-primary-list)))))

(defun buffoon-previous ()
  "Cycle to previous promoted buffer in PRIMARY window."
  (interactive)
  (if (null buffoon-primary-list)
      (message "No PRIMARY promoted buffers")
    (setq buffoon-primary-index 
          (mod (1- buffoon-primary-index) (length buffoon-primary-list)))
    (let ((buf (nth buffoon-primary-index buffoon-primary-list)))
      (buffoon--display-in-primary buf)
      (message "PRIMARY: %s (%d/%d)" 
               (buffer-name buf)
               (1+ buffoon-primary-index)
               (length buffoon-primary-list)))))

(defun buffoon-switch-to-n (n)
  "Switch PRIMARY window to Nth promoted buffer (1-indexed).
Called with numeric prefix argument."
  (interactive "p")
  (if (null buffoon-primary-list)
      (message "No PRIMARY promoted buffers")
    (let ((index (1- n)))
      (if (and (>= index 0) (< index (length buffoon-primary-list)))
          (progn
            (setq buffoon-primary-index index)
            (let ((buf (nth index buffoon-primary-list)))
              (buffoon--display-in-primary buf)
              (message "PRIMARY: %s (%d/%d)" 
                       (buffer-name buf)
                       (1+ index)
                       (length buffoon-primary-list))))
        (message "No PRIMARY buffer at position %d (have %d promoted buffers)" 
                 n (length buffoon-primary-list))))))

;; Convenience functions for direct access
(defun buffoon-switch-to-1 () "Switch to 1st promoted buffer." (interactive) (buffoon-switch-to-n 1))
(defun buffoon-switch-to-2 () "Switch to 2nd promoted buffer." (interactive) (buffoon-switch-to-n 2))
(defun buffoon-switch-to-3 () "Switch to 3rd promoted buffer." (interactive) (buffoon-switch-to-n 3))
(defun buffoon-switch-to-4 () "Switch to 4th promoted buffer." (interactive) (buffoon-switch-to-n 4))
(defun buffoon-switch-to-5 () "Switch to 5th promoted buffer." (interactive) (buffoon-switch-to-n 5))
(defun buffoon-switch-to-6 () "Switch to 6th promoted buffer." (interactive) (buffoon-switch-to-n 6))
(defun buffoon-switch-to-7 () "Switch to 7th promoted buffer." (interactive) (buffoon-switch-to-n 7))
(defun buffoon-switch-to-8 () "Switch to 8th promoted buffer." (interactive) (buffoon-switch-to-n 8))
(defun buffoon-switch-to-9 () "Switch to 9th promoted buffer." (interactive) (buffoon-switch-to-n 9))

;;; Navigation - SECONDARY

(defun buffoon-secondary-next ()
  "Cycle to next promoted buffer in SECONDARY window."
  (interactive)
  (if (null buffoon-secondary-list)
      (message "No SECONDARY promoted buffers")
    (setq buffoon-secondary-index 
          (mod (1+ buffoon-secondary-index) (length buffoon-secondary-list)))
    (let ((buf (nth buffoon-secondary-index buffoon-secondary-list)))
      (buffoon--display-in-secondary buf)
      (message "SECONDARY: %s (%d/%d)" 
               (buffer-name buf)
               (1+ buffoon-secondary-index)
               (length buffoon-secondary-list)))))

(defun buffoon-secondary-previous ()
  "Cycle to previous promoted buffer in SECONDARY window."
  (interactive)
  (if (null buffoon-secondary-list)
      (message "No SECONDARY promoted buffers")
    (setq buffoon-secondary-index 
          (mod (1- buffoon-secondary-index) (length buffoon-secondary-list)))
    (let ((buf (nth buffoon-secondary-index buffoon-secondary-list)))
      (buffoon--display-in-secondary buf)
      (message "SECONDARY: %s (%d/%d)" 
               (buffer-name buf)
               (1+ buffoon-secondary-index)
               (length buffoon-secondary-list)))))

(defun buffoon-secondary-switch-to-n (n)
  "Switch SECONDARY window to Nth promoted buffer (1-indexed).
Called with numeric prefix argument."
  (interactive "p")
  (if (null buffoon-secondary-list)
      (message "No SECONDARY promoted buffers")
    (let ((index (1- n)))
      (if (and (>= index 0) (< index (length buffoon-secondary-list)))
          (progn
            (setq buffoon-secondary-index index)
            (let ((buf (nth index buffoon-secondary-list)))
              (buffoon--display-in-secondary buf)
              (message "SECONDARY: %s (%d/%d)" 
                       (buffer-name buf)
                       (1+ index)
                       (length buffoon-secondary-list))))
        (message "No SECONDARY buffer at position %d (have %d promoted buffers)" 
                 n (length buffoon-secondary-list))))))

;; Convenience functions for direct access to SECONDARY
(defun buffoon-secondary-switch-to-1 () "Switch SECONDARY to 1st promoted buffer." (interactive) (buffoon-secondary-switch-to-n 1))
(defun buffoon-secondary-switch-to-2 () "Switch SECONDARY to 2nd promoted buffer." (interactive) (buffoon-secondary-switch-to-n 2))
(defun buffoon-secondary-switch-to-3 () "Switch SECONDARY to 3rd promoted buffer." (interactive) (buffoon-secondary-switch-to-n 3))
(defun buffoon-secondary-switch-to-4 () "Switch SECONDARY to 4th promoted buffer." (interactive) (buffoon-secondary-switch-to-n 4))
(defun buffoon-secondary-switch-to-5 () "Switch SECONDARY to 5th promoted buffer." (interactive) (buffoon-secondary-switch-to-n 5))
(defun buffoon-secondary-switch-to-6 () "Switch SECONDARY to 6th promoted buffer." (interactive) (buffoon-secondary-switch-to-n 6))
(defun buffoon-secondary-switch-to-7 () "Switch SECONDARY to 7th promoted buffer." (interactive) (buffoon-secondary-switch-to-n 7))
(defun buffoon-secondary-switch-to-8 () "Switch SECONDARY to 8th promoted buffer." (interactive) (buffoon-secondary-switch-to-n 8))
(defun buffoon-secondary-switch-to-9 () "Switch SECONDARY to 9th promoted buffer." (interactive) (buffoon-secondary-switch-to-n 9))

;;; Window Focus

(defun buffoon-jump-to-primary ()
  "Move point/focus to PRIMARY window."
  (interactive)
  (when (and (buffoon--active-frame-p)
             buffoon-primary-window
             (window-live-p buffoon-primary-window))
    (select-window buffoon-primary-window)))

(defun buffoon-jump-to-secondary ()
  "Move point/focus to SECONDARY window."
  (interactive)
  (when (and (buffoon--active-frame-p)
             buffoon-secondary-window
             (window-live-p buffoon-secondary-window))
    (select-window buffoon-secondary-window)))

;;; Buffer List Management

(defun buffoon-show-list ()
  "Display the ordered lists of promoted buffers for both PRIMARY and SECONDARY."
  (interactive)
  (if (and (null buffoon-primary-list) 
           (null buffoon-secondary-list))
      (message "No promoted buffers")
    (let ((msg "Skilled Buffers:\n\n"))
      ;; PRIMARY list
      (setq msg (concat msg "PRIMARY:\n"))
      (if (null buffoon-primary-list)
          (setq msg (concat msg "  (empty)\n"))
        (setq msg (concat msg
                          (mapconcat
                           (lambda (i)
                             (let ((buf (nth i buffoon-primary-list)))
                               (format "  %d. %s%s"
                                       (1+ i)
                                       (if (= i buffoon-primary-index) "[current] → " "")
                                       (buffer-name buf))))
                           (number-sequence 0 (1- (length buffoon-primary-list)))
                           "\n")
                          "\n")))
      
      ;; SECONDARY list
      (setq msg (concat msg "\nSECONDARY:\n"))
      (if (null buffoon-secondary-list)
          (setq msg (concat msg "  (empty)\n"))
        (setq msg (concat msg
                          (mapconcat
                           (lambda (i)
                             (let ((buf (nth i buffoon-secondary-list)))
                               (format "  %d. %s%s"
                                       (1+ i)
                                       (if (= i buffoon-secondary-index) "[current] → " "")
                                       (buffer-name buf))))
                           (number-sequence 0 (1- (length buffoon-secondary-list)))
                           "\n"))))
      
      (message "%s" msg))))

(defun buffoon-clear-primary ()
  "Remove all promoted buffers from the PRIMARY list."
  (interactive)
  (when (yes-or-no-p "Clear all PRIMARY promoted buffers? ")
    (setq buffoon-primary-list '())
    (setq buffoon-primary-index 0)
    (buffoon--show-dashboard-in-primary)
    (message "Cleared all PRIMARY promoted buffers")))

(defun buffoon-clear-secondary ()
  "Remove all promoted buffers from the SECONDARY list."
  (interactive)
  (when (yes-or-no-p "Clear all SECONDARY promoted buffers? ")
    (setq buffoon-secondary-list '())
    (setq buffoon-secondary-index 0)
    (message "Cleared all SECONDARY promoted buffers")))

(defun buffoon-clear ()
  "Remove all promoted buffers from both PRIMARY and SECONDARY lists."
  (interactive)
  (when (yes-or-no-p "Clear all promoted buffers (PRIMARY and SECONDARY)? ")
    (setq buffoon-primary-list '())
    (setq buffoon-primary-index 0)
    (setq buffoon-secondary-list '())
    (setq buffoon-secondary-index 0)
    (buffoon--show-dashboard-in-primary)
    (message "Cleared all promoted buffers")))

(defun buffoon-reorder-up ()
  "Move current buffer up one position in the PRIMARY promoted list."
  (interactive)
  (let* ((buf (current-buffer))
         (pos (cl-position buf buffoon-primary-list)))
    (if (not pos)
        (message "Buffer is not promoted to PRIMARY")
      (if (= pos 0)
          (message "Already at top of PRIMARY list")
        ;; Swap with previous
        (let ((prev-buf (nth (1- pos) buffoon-primary-list)))
          (setf (nth (1- pos) buffoon-primary-list) buf)
          (setf (nth pos buffoon-primary-list) prev-buf)
          (when (= buffoon-primary-index pos)
            (setq buffoon-primary-index (1- pos)))
          (message "Moved %s up in PRIMARY" (buffer-name buf)))))))

(defun buffoon-reorder-down ()
  "Move current buffer down one position in the PRIMARY promoted list."
  (interactive)
  (let* ((buf (current-buffer))
         (pos (cl-position buf buffoon-primary-list)))
    (if (not pos)
        (message "Buffer is not promoted to PRIMARY")
      (if (= pos (1- (length buffoon-primary-list)))
          (message "Already at bottom of PRIMARY list")
        ;; Swap with next
        (let ((next-buf (nth (1+ pos) buffoon-primary-list)))
          (setf (nth (1+ pos) buffoon-primary-list) buf)
          (setf (nth pos buffoon-primary-list) next-buf)
          (when (= buffoon-primary-index pos)
            (setq buffoon-primary-index (1+ pos)))
          (message "Moved %s down in PRIMARY" (buffer-name buf)))))))

(defun buffoon-secondary-reorder-up ()
  "Move current buffer up one position in the SECONDARY promoted list."
  (interactive)
  (let* ((buf (current-buffer))
         (pos (cl-position buf buffoon-secondary-list)))
    (if (not pos)
        (message "Buffer is not promoted to SECONDARY")
      (if (= pos 0)
          (message "Already at top of SECONDARY list")
        ;; Swap with previous
        (let ((prev-buf (nth (1- pos) buffoon-secondary-list)))
          (setf (nth (1- pos) buffoon-secondary-list) buf)
          (setf (nth pos buffoon-secondary-list) prev-buf)
          (when (= buffoon-secondary-index pos)
            (setq buffoon-secondary-index (1- pos)))
          (message "Moved %s up in SECONDARY" (buffer-name buf)))))))

(defun buffoon-secondary-reorder-down ()
  "Move current buffer down one position in the SECONDARY promoted list."
  (interactive)
  (let* ((buf (current-buffer))
         (pos (cl-position buf buffoon-secondary-list)))
    (if (not pos)
        (message "Buffer is not promoted to SECONDARY")
      (if (= pos (1- (length buffoon-secondary-list)))
          (message "Already at bottom of SECONDARY list")
        ;; Swap with next
        (let ((next-buf (nth (1+ pos) buffoon-secondary-list)))
          (setf (nth (1+ pos) buffoon-secondary-list) buf)
          (setf (nth pos buffoon-secondary-list) next-buf)
          (when (= buffoon-secondary-index pos)
            (setq buffoon-secondary-index (1+ pos)))
          (message "Moved %s down in SECONDARY" (buffer-name buf)))))))

;;; Cleanup

(defun buffoon-cleanup ()
  "Remove dead buffers from both PRIMARY and SECONDARY promoted lists."
  (let ((primary-before (length buffoon-primary-list))
        (secondary-before (length buffoon-secondary-list)))
    
    ;; Clean PRIMARY list
    (setq buffoon-primary-list 
          (cl-remove-if-not #'buffer-live-p buffoon-primary-list))
    
    ;; Adjust PRIMARY index if necessary
    (when (>= buffoon-primary-index (length buffoon-primary-list))
      (setq buffoon-primary-index (max 0 (1- (length buffoon-primary-list)))))
    
    ;; Clean SECONDARY list
    (setq buffoon-secondary-list 
          (cl-remove-if-not #'buffer-live-p buffoon-secondary-list))
    
    ;; Adjust SECONDARY index if necessary
    (when (>= buffoon-secondary-index (length buffoon-secondary-list))
      (setq buffoon-secondary-index (max 0 (1- (length buffoon-secondary-list)))))
    
    (let ((primary-removed (- primary-before (length buffoon-primary-list)))
          (secondary-removed (- secondary-before (length buffoon-secondary-list))))
      (when (or (> primary-removed 0) (> secondary-removed 0))
        (message "Removed %d dead buffer(s) from PRIMARY, %d from SECONDARY" 
                 primary-removed secondary-removed)
        
        ;; If we're in PRIMARY and current buffer was removed, show next
        (when (and (buffoon--active-frame-p)
                   buffoon-primary-window
                   (window-live-p buffoon-primary-window)
                   (eq (selected-window) buffoon-primary-window))
          (if buffoon-primary-list
              (buffoon--display-in-primary 
               (nth buffoon-primary-index buffoon-primary-list))
            (buffoon--show-dashboard-in-primary)))))))

(defun buffoon--kill-buffer-hook ()
  "Hook function to clean up when a buffer is killed."
  (when (or (memq (current-buffer) buffoon-primary-list)
            (memq (current-buffer) buffoon-secondary-list))
    (buffoon-cleanup)))

;;; Persistence

(defun buffoon-save-list ()
  "Save both PRIMARY and SECONDARY promoted buffer lists to file."
  (interactive)
  (let ((primary-file-names 
         (cl-remove-if #'null
                       (mapcar (lambda (buf)
                                 (buffer-file-name buf))
                               buffoon-primary-list)))
        (secondary-file-names 
         (cl-remove-if #'null
                       (mapcar (lambda (buf)
                                 (buffer-file-name buf))
                               buffoon-secondary-list))))
    (with-temp-file buffoon-save-file
      (insert ";; Skilled Buffers Save File\n")
      (insert ";; Auto-generated - do not edit manually\n\n")
      (insert "(setq buffoon-saved-primary-files\n  '(")
      (insert (mapconcat (lambda (f) (format "%S" f))
                         primary-file-names
                         "\n    "))
      (insert "))\n\n")
      (insert "(setq buffoon-saved-secondary-files\n  '(")
      (insert (mapconcat (lambda (f) (format "%S" f))
                         secondary-file-names
                         "\n    "))
      (insert "))\n"))
    (message "Saved %d PRIMARY, %d SECONDARY buffer(s)" 
             (length primary-file-names)
             (length secondary-file-names))))

(defun buffoon-restore-list ()
  "Restore both PRIMARY and SECONDARY promoted buffer lists from file."
  (interactive)
  (when (file-exists-p buffoon-save-file)
    (load buffoon-save-file t t)
    (let ((primary-restored 0)
          (secondary-restored 0))
      
      ;; Restore PRIMARY list
      (when (boundp 'buffoon-saved-primary-files)
        (dolist (file buffoon-saved-primary-files)
          (when (file-exists-p file)
            (let ((buf (find-file-noselect file)))
              (unless (memq buf buffoon-primary-list)
                (setq buffoon-primary-list 
                      (append buffoon-primary-list (list buf)))
                (setq primary-restored (1+ primary-restored))))))
        (setq buffoon-primary-index 0)
        (when (and (> primary-restored 0) buffoon-primary-list)
          (buffoon--display-in-primary 
           (car buffoon-primary-list))))
      
      ;; Restore SECONDARY list
      (when (boundp 'buffoon-saved-secondary-files)
        (dolist (file buffoon-saved-secondary-files)
          (when (file-exists-p file)
            (let ((buf (find-file-noselect file)))
              (unless (memq buf buffoon-secondary-list)
                (setq buffoon-secondary-list 
                      (append buffoon-secondary-list (list buf)))
                (setq secondary-restored (1+ secondary-restored))))))
        (setq buffoon-secondary-index 0))
      
      (when (or (> primary-restored 0) (> secondary-restored 0))
        (message "Restored %d PRIMARY, %d SECONDARY buffer(s)" 
                 primary-restored secondary-restored)))))

;;; Display Buffer Integration

(defun buffoon--display-buffer-action (buffer alist)
  "Custom display buffer action for routing buffers to correct window.
BUFFER is the buffer to display, ALIST is the action alist.

Routing rules:
- PRIMARY list buffers → PRIMARY window
- SECONDARY list buffers → SECONDARY window (when explicitly switched to)
- All other buffers → SECONDARY window (default)"
  (when (buffoon--active-frame-p)
    (cond
     ;; PRIMARY list buffers go to PRIMARY window
     ((memq buffer buffoon-primary-list)
      (when (and buffoon-primary-window
                 (window-live-p buffoon-primary-window))
        (window--display-buffer buffer buffoon-primary-window 'reuse alist)))
     
     ;; All other buffers (including SECONDARY list) go to SECONDARY window
     ;; SECONDARY list buffers are shown when explicitly requested via navigation functions
     (t
      (when (and buffoon-secondary-window
                 (window-live-p buffoon-secondary-window))
        (window--display-buffer buffer buffoon-secondary-window 'reuse alist))))))

(defun buffoon--window-buffer-change-function (frame)
  "Hook function called when window buffers change.
Ensures buffers are displayed in the correct window according to promotion rules.
This catches ALL methods of opening/switching buffers (projectile, find-file, etc)."
  (when (and (eq frame buffoon-frame)
             (buffoon--active-frame-p))
    (let ((primary-buffer (when (and buffoon-primary-window
                                     (window-live-p buffoon-primary-window))
                            (window-buffer buffoon-primary-window)))
          (secondary-buffer (when (and buffoon-secondary-window
                                       (window-live-p buffoon-secondary-window))
                              (window-buffer buffoon-secondary-window))))
      
      ;; Check if PRIMARY window has a buffer that shouldn't be there
      (when (and primary-buffer
                 (not (memq primary-buffer buffoon-primary-list)))
        ;; Move this buffer to SECONDARY
        (when (and buffoon-secondary-window
                   (window-live-p buffoon-secondary-window))
          (set-window-buffer buffoon-secondary-window primary-buffer))
        ;; Show appropriate buffer in PRIMARY
        (if buffoon-primary-list
            (set-window-buffer buffoon-primary-window
                               (nth buffoon-primary-index buffoon-primary-list))
          (buffoon--show-dashboard-in-primary)))
      
      ;; Check if SECONDARY window has a PRIMARY buffer that should be in PRIMARY
      (when (and secondary-buffer
                 (memq secondary-buffer buffoon-primary-list))
        ;; This buffer belongs in PRIMARY
        (when (and buffoon-primary-window
                   (window-live-p buffoon-primary-window))
          (set-window-buffer buffoon-primary-window secondary-buffer)
          ;; Update PRIMARY index to match
          (let ((idx (cl-position secondary-buffer buffoon-primary-list)))
            (when idx
              (setq buffoon-primary-index idx)))
          ;; Show previous SECONDARY buffer or a SECONDARY promoted buffer
          (when (and buffoon-secondary-window
                     (window-live-p buffoon-secondary-window))
            (if buffoon-secondary-list
                (set-window-buffer buffoon-secondary-window
                                   (nth buffoon-secondary-index buffoon-secondary-list))
              ;; Just leave whatever was there before in SECONDARY
              nil)))))))

;;; Tab Line Integration

(defface buffoon-tab-selected
  '((t :inherit tab-line-tab-current
     :box (:line-width 2 :color nil :style nil)))
  "Face for selected tab in buffoon."
  :group 'buffoon)

(defface buffoon-tab-unselected
  '((t :inherit tab-line-tab-inactive
     :box (:line-width 1 :color nil :style nil)))
  "Face for unselected tabs in buffoon."
  :group 'buffoon)

(defface buffoon-tab-ephemeral
  '((t :inherit tab-line-tab-inactive
     :slant italic
     :box (:line-width 1 :color nil :style nil)))
  "Face for ephemeral tabs (current buffer not in promoted list) in buffoon."
  :group 'buffoon)

(defun buffoon--tab-line-tabs ()
  "Generate tab list for current window based on PRIMARY or SECONDARY list."
  (when (buffoon--active-frame-p)
    (let* ((current-window (selected-window))
           (current-buffer (window-buffer current-window))
           (is-primary (eq current-window buffoon-primary-window))
           (is-secondary (eq current-window buffoon-secondary-window))
           (buffer-list (cond
                         (is-primary buffoon-primary-list)
                         (is-secondary buffoon-secondary-list)
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
                     (not (memq current-buffer buffoon-secondary-list))
                     (buffer-live-p current-buffer))
            (setq tabs (append tabs
                               `((tab
                                  (name . ,(buffer-name current-buffer))
                                  (buffer . ,current-buffer)
                                  (selected . t)
                                  (ephemeral . t))))))
          tabs)))))

(defun buffoon--tab-line-tab-name-function (tab tabs)
  "Format a single tab for display. No close button."
  (let* ((buffer (alist-get 'buffer tab))
         (name (alist-get 'name tab))
         (selected (alist-get 'selected tab))
         (ephemeral (alist-get 'ephemeral tab))
         (face (cond
                (ephemeral 'buffoon-tab-ephemeral)
                (selected 'buffoon-tab-selected)
                (t 'buffoon-tab-unselected))))
    (propertize (concat " " name " ")
                'face face
                'mouse-face 'tab-line-highlight
                'keymap (let ((map (make-sparse-keymap)))
                          (define-key map [tab-line mouse-1]
                                      `(lambda () (interactive)
                                         (when (buffer-live-p ,buffer)
                                           (switch-to-buffer ,buffer))))
                          map))))

(defun buffoon--setup-tab-line ()
  "Setup tab-line for buffoon windows."
  (when (and (buffoon--active-frame-p)
             (fboundp 'tab-line-mode))
    ;; Enable tab-line in PRIMARY window
    (when (and buffoon-primary-window
               (window-live-p buffoon-primary-window))
      (with-selected-window buffoon-primary-window
        (setq-local tab-line-tabs-function #'buffoon--tab-line-tabs)
        (setq-local tab-line-tab-name-function #'buffoon--tab-line-tab-name-function)
        (setq-local tab-line-close-button-show nil)
        (setq-local tab-line-new-button-show nil)
        (tab-line-mode 1)))
    
    ;; Enable tab-line in SECONDARY window
    (when (and buffoon-secondary-window
               (window-live-p buffoon-secondary-window))
      (with-selected-window buffoon-secondary-window
        (setq-local tab-line-tabs-function #'buffoon--tab-line-tabs)
        (setq-local tab-line-tab-name-function #'buffoon--tab-line-tab-name-function)
        (setq-local tab-line-close-button-show nil)
        (setq-local tab-line-new-button-show nil)
        (tab-line-mode 1)))))

(defun buffoon--update-tab-line ()
  "Force update of tab-line display in both windows."
  (when (and (buffoon--active-frame-p)
             (fboundp 'tab-line-mode))
    (dolist (win (list buffoon-primary-window buffoon-secondary-window))
      (when (and win (window-live-p win))
        (with-selected-window win
          (when tab-line-mode
            (setq-local tab-line-tabs-function #'buffoon--tab-line-tabs)
            (setq-local tab-line-tab-name-function #'buffoon--tab-line-tab-name-function)
            (setq-local tab-line-close-button-show nil)
            (setq-local tab-line-new-button-show nil))
          (force-mode-line-update))))))

;;; Minor Mode

;;;###autoload
(define-minor-mode buffoon-mode
  "Toggle buffoon dual-window management system."
  :global t
  :group 'buffoon
  :lighter " Skilled"
  (if buffoon-mode
      (progn
        ;; Enable
        (add-hook 'kill-buffer-hook #'buffoon--kill-buffer-hook)
        (add-hook 'kill-emacs-hook #'buffoon-save-list)
        (add-hook 'buffer-list-update-hook #'buffoon--update-tab-line)
        
        ;; Add window-buffer-change hook to catch ALL buffer switches
        ;; This handles projectile, find-file, switch-to-buffer, and any other method
        (add-hook 'window-buffer-change-functions #'buffoon--window-buffer-change-function)
        
        ;; Add display buffer action at the BEGINNING (highest priority)
        ;; This ensures buffoon takes precedence over other rules (magit, etc.)
        ;; Use ^.* to properly match all buffer names from the start
        (push '("^.*" buffoon--display-buffer-action)
              display-buffer-alist)
        
        ;; Setup tab-line
        (buffoon--setup-tab-line)
        
        (message "Skilled buffers mode enabled"))
    
    ;; Disable
    (remove-hook 'kill-buffer-hook #'buffoon--kill-buffer-hook)
    (remove-hook 'kill-emacs-hook #'buffoon-save-list)
    (remove-hook 'buffer-list-update-hook #'buffoon--update-tab-line)
    (remove-hook 'window-buffer-change-functions #'buffoon--window-buffer-change-function)
    
    ;; Remove display buffer action
    (setq display-buffer-alist
          (cl-remove-if (lambda (entry)
                          (eq (cadr entry) 'buffoon--display-buffer-action))
                        display-buffer-alist))
    
    ;; Disable tab-line in both windows
    (when (fboundp 'tab-line-mode)
      (when (and buffoon-primary-window
                 (window-live-p buffoon-primary-window))
        (with-selected-window buffoon-primary-window
          (tab-line-mode -1)))
      (when (and buffoon-secondary-window
                 (window-live-p buffoon-secondary-window))
        (with-selected-window buffoon-secondary-window
          (tab-line-mode -1))))
    
    (message "Skilled buffers mode disabled")))

(provide 'buffoon)
;;; buffoon.el ends here

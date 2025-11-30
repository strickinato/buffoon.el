;;; skilled-buffers.el --- Dual-window buffer management system -*- lexical-binding: t; -*-

;; Author: Aaron Strick
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, windows
;; URL: https://github.com/strickinato/skilled-buffers

;;; Commentary:
;; A dual-window buffer management system with PRIMARY and SECONDARY windows.
;; Manually promote buffers to an ordered list that controls PRIMARY display.
;;
;; The PRIMARY window (left) displays promoted buffers from an ordered list.
;; The SECONDARY window (right) displays all other buffers by default.
;;
;; Main functions:
;; - `skilled-buffers-setup-layout': Initialize the dual-window layout
;; - `skilled-buffers-promote': Promote current buffer to PRIMARY
;; - `skilled-buffers-demote': Demote current buffer from PRIMARY
;; - `skilled-buffers-next': Cycle to next promoted buffer
;; - `skilled-buffers-previous': Cycle to previous promoted buffer

;;; Code:

(require 'cl-lib)

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

(defvar skilled-buffers-list '()
  "Ordered list of buffer objects promoted to PRIMARY window.")

(defvar skilled-buffers-index 0
  "Current index into `skilled-buffers-list'.")

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
  (when (null skilled-buffers-list)
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

;;; Buffer Promotion

(defun skilled-buffers-promote ()
  "Promote current buffer to the PRIMARY window.
If already promoted, display message. If in SECONDARY, move to PRIMARY
and show previous buffer in SECONDARY."
  (interactive)
  (let ((buf (current-buffer)))
    (if (memq buf skilled-buffers-list)
        (message "Already promoted")
      ;; Add to end of list
      (setq skilled-buffers-list (append skilled-buffers-list (list buf)))
      (setq skilled-buffers-index (1- (length skilled-buffers-list)))
      
      ;; Display in PRIMARY
      (skilled-buffers--display-in-primary buf)
      
      (message "Promoted: %s" (buffer-name buf)))))

(defun skilled-buffers-demote ()
  "Demote current buffer from PRIMARY window.
If currently viewing in PRIMARY, move buffer to SECONDARY and show
previous promoted buffer (or dashboard) in PRIMARY."
  (interactive)
  (let ((buf (current-buffer)))
    (if (not (memq buf skilled-buffers-list))
        (message "Buffer is not promoted")
      ;; Remove from list
      (setq skilled-buffers-list (delq buf skilled-buffers-list))
      
      ;; Adjust index if necessary
      (when (>= skilled-buffers-index (length skilled-buffers-list))
        (setq skilled-buffers-index (max 0 (1- (length skilled-buffers-list)))))
      
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
        (if skilled-buffers-list
            (skilled-buffers--display-in-primary 
             (nth skilled-buffers-index skilled-buffers-list))
          (skilled-buffers--show-dashboard-in-primary)))
      
      (message "Demoted: %s" (buffer-name buf)))))

(defun skilled-buffers-demote-by-name ()
  "Interactively select and demote a promoted buffer."
  (interactive)
  (if (null skilled-buffers-list)
      (message "No promoted buffers")
    (let* ((buffer-names (mapcar #'buffer-name skilled-buffers-list))
           (choice (completing-read "Demote buffer: " buffer-names nil t))
           (buf (get-buffer choice)))
      (when buf
        (with-current-buffer buf
          (skilled-buffers-demote))))))

(defun skilled-buffers--display-in-primary (buffer)
  "Display BUFFER in PRIMARY window."
  (when (and (skilled-buffers--active-frame-p)
             skilled-buffers-primary-window
             (window-live-p skilled-buffers-primary-window)
             (buffer-live-p buffer))
    (with-selected-window skilled-buffers-primary-window
      (switch-to-buffer buffer))))

;;; Navigation

(defun skilled-buffers-next ()
  "Cycle to next promoted buffer in PRIMARY window."
  (interactive)
  (if (null skilled-buffers-list)
      (message "No promoted buffers")
    (setq skilled-buffers-index 
          (mod (1+ skilled-buffers-index) (length skilled-buffers-list)))
    (let ((buf (nth skilled-buffers-index skilled-buffers-list)))
      (skilled-buffers--display-in-primary buf)
      (message "Switched to: %s (%d/%d)" 
               (buffer-name buf)
               (1+ skilled-buffers-index)
               (length skilled-buffers-list)))))

(defun skilled-buffers-previous ()
  "Cycle to previous promoted buffer in PRIMARY window."
  (interactive)
  (if (null skilled-buffers-list)
      (message "No promoted buffers")
    (setq skilled-buffers-index 
          (mod (1- skilled-buffers-index) (length skilled-buffers-list)))
    (let ((buf (nth skilled-buffers-index skilled-buffers-list)))
      (skilled-buffers--display-in-primary buf)
      (message "Switched to: %s (%d/%d)" 
               (buffer-name buf)
               (1+ skilled-buffers-index)
               (length skilled-buffers-list)))))

(defun skilled-buffers-switch-to-n (n)
  "Switch PRIMARY window to Nth promoted buffer (1-indexed).
Called with numeric prefix argument."
  (interactive "p")
  (if (null skilled-buffers-list)
      (message "No promoted buffers")
    (let ((index (1- n)))
      (if (and (>= index 0) (< index (length skilled-buffers-list)))
          (progn
            (setq skilled-buffers-index index)
            (let ((buf (nth index skilled-buffers-list)))
              (skilled-buffers--display-in-primary buf)
              (message "Switched to: %s (%d/%d)" 
                       (buffer-name buf)
                       (1+ index)
                       (length skilled-buffers-list))))
        (message "No buffer at position %d (have %d promoted buffers)" 
                 n (length skilled-buffers-list))))))

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
  "Display the ordered list of promoted buffers."
  (interactive)
  (if (null skilled-buffers-list)
      (message "No promoted buffers")
    (let ((msg (concat "Skilled Buffers:\n"
                       (mapconcat
                        (lambda (i)
                          (let ((buf (nth i skilled-buffers-list)))
                            (format "  %d. %s%s"
                                    (1+ i)
                                    (if (= i skilled-buffers-index) "[current] → " "")
                                    (buffer-name buf))))
                        (number-sequence 0 (1- (length skilled-buffers-list)))
                        "\n"))))
      (message "%s" msg))))

(defun skilled-buffers-clear ()
  "Remove all promoted buffers from the list."
  (interactive)
  (when (yes-or-no-p "Clear all promoted buffers? ")
    (setq skilled-buffers-list '())
    (setq skilled-buffers-index 0)
    (skilled-buffers--show-dashboard-in-primary)
    (message "Cleared all promoted buffers")))

(defun skilled-buffers-reorder-up ()
  "Move current buffer up one position in the promoted list."
  (interactive)
  (let* ((buf (current-buffer))
         (pos (cl-position buf skilled-buffers-list)))
    (if (not pos)
        (message "Buffer is not promoted")
      (if (= pos 0)
          (message "Already at top of list")
        ;; Swap with previous
        (let ((prev-buf (nth (1- pos) skilled-buffers-list)))
          (setf (nth (1- pos) skilled-buffers-list) buf)
          (setf (nth pos skilled-buffers-list) prev-buf)
          (when (= skilled-buffers-index pos)
            (setq skilled-buffers-index (1- pos)))
          (message "Moved %s up" (buffer-name buf)))))))

(defun skilled-buffers-reorder-down ()
  "Move current buffer down one position in the promoted list."
  (interactive)
  (let* ((buf (current-buffer))
         (pos (cl-position buf skilled-buffers-list)))
    (if (not pos)
        (message "Buffer is not promoted")
      (if (= pos (1- (length skilled-buffers-list)))
          (message "Already at bottom of list")
        ;; Swap with next
        (let ((next-buf (nth (1+ pos) skilled-buffers-list)))
          (setf (nth (1+ pos) skilled-buffers-list) buf)
          (setf (nth pos skilled-buffers-list) next-buf)
          (when (= skilled-buffers-index pos)
            (setq skilled-buffers-index (1+ pos)))
          (message "Moved %s down" (buffer-name buf)))))))

;;; Cleanup

(defun skilled-buffers-cleanup ()
  "Remove dead buffers from the promoted list."
  (let ((before-count (length skilled-buffers-list)))
    (setq skilled-buffers-list 
          (cl-remove-if-not #'buffer-live-p skilled-buffers-list))
    
    ;; Adjust index if necessary
    (when (>= skilled-buffers-index (length skilled-buffers-list))
      (setq skilled-buffers-index (max 0 (1- (length skilled-buffers-list)))))
    
    (let ((removed (- before-count (length skilled-buffers-list))))
      (when (> removed 0)
        (message "Removed %d dead buffer(s) from promoted list" removed)
        
        ;; If we're in PRIMARY and current buffer was removed, show next
        (when (and (skilled-buffers--active-frame-p)
                   skilled-buffers-primary-window
                   (window-live-p skilled-buffers-primary-window)
                   (eq (selected-window) skilled-buffers-primary-window))
          (if skilled-buffers-list
              (skilled-buffers--display-in-primary 
               (nth skilled-buffers-index skilled-buffers-list))
            (skilled-buffers--show-dashboard-in-primary)))))))

(defun skilled-buffers--kill-buffer-hook ()
  "Hook function to clean up when a buffer is killed."
  (when (memq (current-buffer) skilled-buffers-list)
    (skilled-buffers-cleanup)))

;;; Persistence

(defun skilled-buffers-save-list ()
  "Save the promoted buffer list to file."
  (interactive)
  (let ((buffer-file-names 
         (cl-remove-if #'null
                       (mapcar (lambda (buf)
                                 (buffer-file-name buf))
                               skilled-buffers-list))))
    (with-temp-file skilled-buffers-save-file
      (insert ";; Skilled Buffers Save File\n")
      (insert ";; Auto-generated - do not edit manually\n\n")
      (insert "(setq skilled-buffers-saved-files\n  '(")
      (insert (mapconcat (lambda (f) (format "%S" f))
                         buffer-file-names
                         "\n    "))
      (insert "))\n"))
    (message "Saved %d promoted buffer(s)" (length buffer-file-names))))

(defun skilled-buffers-restore-list ()
  "Restore the promoted buffer list from file."
  (interactive)
  (when (file-exists-p skilled-buffers-save-file)
    (load skilled-buffers-save-file t t)
    (when (boundp 'skilled-buffers-saved-files)
      (let ((restored 0))
        (dolist (file skilled-buffers-saved-files)
          (when (file-exists-p file)
            (let ((buf (find-file-noselect file)))
              (unless (memq buf skilled-buffers-list)
                (setq skilled-buffers-list 
                      (append skilled-buffers-list (list buf)))
                (setq restored (1+ restored))))))
        (setq skilled-buffers-index 0)
        (when (and (> restored 0) skilled-buffers-list)
          (skilled-buffers--display-in-primary 
           (car skilled-buffers-list)))
        (message "Restored %d promoted buffer(s)" restored)))))

;;; Display Buffer Integration

(defun skilled-buffers--display-buffer-action (buffer alist)
  "Custom display buffer action for routing buffers to correct window.
BUFFER is the buffer to display, ALIST is the action alist."
  (when (skilled-buffers--active-frame-p)
    (cond
     ;; Promoted buffers go to PRIMARY
     ((memq buffer skilled-buffers-list)
      (when (and skilled-buffers-primary-window
                 (window-live-p skilled-buffers-primary-window))
        (window--display-buffer buffer skilled-buffers-primary-window 'reuse alist)))
     
     ;; Non-promoted buffers go to SECONDARY
     (t
      (when (and skilled-buffers-secondary-window
                 (window-live-p skilled-buffers-secondary-window))
        (window--display-buffer buffer skilled-buffers-secondary-window 'reuse alist))))))

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
        
        ;; Add display buffer action
        (add-to-list 'display-buffer-alist
                     '(".*" skilled-buffers--display-buffer-action))
        
        (message "Skilled buffers mode enabled"))
    
    ;; Disable
    (remove-hook 'kill-buffer-hook #'skilled-buffers--kill-buffer-hook)
    (remove-hook 'kill-emacs-hook #'skilled-buffers-save-list)
    
    ;; Remove display buffer action
    (setq display-buffer-alist
          (cl-remove-if (lambda (entry)
                          (eq (cadr entry) 'skilled-buffers--display-buffer-action))
                        display-buffer-alist))
    
    (message "Skilled buffers mode disabled")))

(provide 'skilled-buffers)
;;; skilled-buffers.el ends here

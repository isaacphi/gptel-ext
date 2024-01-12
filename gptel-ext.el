;;; gptel-ext.el --- Extra functions for gptel

;; Author: Phil Isaac <isaac.phil@gmail.com>
;; URL: https://github.com/isaacphi/gptel-ext
;; Version: 0.0.1

;;; Commentary:
;;
;; Extend gptel with some extra functions
;;
;;; Code:

(require 'gptel)

(defgroup gptel-ext nil
  "Extensions for gptel."
  :group 'gptel-ext)

(defvar gptel-ext-ask-document-prefix "Your task is to answer questions about the following document. If you don't know the answer, reply with \"I don't know\"\n\n###### DOCUMENT START ######\n\n"
  "Prefix to use when asking questions about a document.")

(defvar gptel-ext-ask-document-suffix "\n\n###### DOCUMENT END ######\n\n### Question: "
  "Suffix to use when asking questions about a document.")

(defvar gptel-ext-refactor-directive "You are a programmer. Refactor my code to improve readability. Reply only with the code."
  "Directive to use when refactoring code.")

(defvar gptel-ext-rewrite-and-replace-directive "You are a programmer. Re-write this code."
  "Directive to use when replacing code.")

;;;###autoload
(defun gptel-ext-send-whole-buffer ()
  "Send the whole buffer to Chat."
  (interactive)
  (mark-whole-buffer)
  (goto-char (point-max))
  (gptel-send))

;;;###autoload
(defun gptel-ext-ask-document ()
  "Loads the current buffer into a session so you can ask questions about it."
  (interactive)
  (let ((nbuf (concat "Ask: " (buffer-name (current-buffer)))))
    (gptel
     nbuf
     :initial (concat
               gptel-ext-ask-document-prefix
               (buffer-substring-no-properties (point-min) (point-max))
               gptel-ext-ask-document-suffix))
    (pop-to-buffer nbuf)))

;; extracted from the wiki
;;
;;;###autoload
(defvar gptel-ext-quick--history nil)
(defun gptel-ext-quick (prompt)
  (interactive (list (read-string "Ask: " nil gptel-quick--history)))
  (when (string= prompt "") (user-error "A prompt is required."))
  (gptel-request
   prompt
   :callback
   (lambda (response info)
     (if (not response)
         (message "gptel-ext-quick failed with message: %s" (plist-get info :status))
       (with-current-buffer (get-buffer-create "*gptel-quick*")
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert response))
         (special-mode)
         (display-buffer (current-buffer)
                         `((display-buffer-in-side-window)
                           (side . bottom)
                           (window-height . ,#'fit-window-to-buffer))))))))

;; extracted from the wiki
;;
;;;###autoload
(defun gptel-ext-rewrite-and-replace (bounds &optional directive)
  "Rewrite the region or sentence at point and replace it with the response."
  (interactive
   (list
    (cond
     ((use-region-p) (cons (region-beginning) (region-end)))
     ((derived-mode-p 'text-mode)
      (list (bounds-of-thing-at-point 'sentence)))
     (t (cons (line-beginning-position) (line-end-position))))
    (and current-prefix-arg
         (read-string gptel-ext-rewrite-and-replace-directive))))
  (gptel-request
   (buffer-substring-no-properties (car bounds) (cdr bounds)) ;the prompt
   :system (or directive gptel-ext-rewrite-and-replace-directive))
   :buffer (current-buffer)
   :context (cons (set-marker (make-marker) (car bounds))
                  (set-marker (make-marker) (cdr bounds)))
   :callback
   (lambda (response info)
     (if (not response)
         (message "Chat response failed with: %s" (plist-get info :status))
       (let* ((bounds (plist-get info :context))
              (beg (car bounds))
              (end (cdr bounds))
              (buf (plist-get info :buffer)))
         (with-current-buffer buf
           (save-excursion
             (goto-char beg)
             (kill-region beg end)
             (insert response)
             (set-marker beg nil)
             (set-marker end nil)
             (message "Rewrote line. Original line saved to kill-ring.")))))))

;;;###autoload
(defun gptel-ext-refactor (bounds)
  "Refactor the region or sentence at point."
  (interactive
   (list
    (cond
     ((use-region-p) (cons (region-beginning) (region-end)))
     ((derived-mode-p 'text-mode)
      (list (bounds-of-thing-at-point 'sentence)))
     (t (cons (line-beginning-position) (line-end-position))))))
  (message "Refactoring...")
  (gptel-ext-rewrite-and-replace bounds gptel-ext-refactor-directive))

(cl-defun gptel-ext-clean-up-gptel-refactored-code ()
  "Clean up the code responses for refactored code in the current buffer.
Current buffer is guaranteed to be the response buffer."
  (when gptel-mode ; Don't want this to happen in the dedicated buffer.
    (cl-return-from my/clean-up-gptel-refactored-code))
  (save-excursion
    (let* ((res-beg (point-min))
           (res-end nil)
           (contents nil))
      (unless (get-text-property res-beg 'gptel)
        (setq res-beg (next-single-property-change res-beg 'gptel)))
      (while res-beg
        (setq res-end (next-single-property-change res-beg 'gptel))
        (unless res-end
          (setq res-end (point-max)))
        (setq contents (buffer-substring-no-properties res-beg
                                                       res-end))
        (setq contents (replace-regexp-in-string "\n*``.*\n*"
                                                 ""
                                                 contents))
        (delete-region res-beg res-end)
        (goto-char res-beg)
        (insert contents)
        (setq res-end (point))
        ;; Indent the code to match the buffer indentation if it's messed up.
        (indent-region res-beg res-end)
        (pulse-momentary-highlight-region res-beg res-end)
        (setq res-beg (next-single-property-change res-beg 'gptel))))))

(add-hook 'gptel-post-response-hook #'gptel-ext-clean-up-gptel-refactored-code)

(provide 'gptel-extensions)

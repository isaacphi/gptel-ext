;; gptel-ext.el --- Extra functions for gptel

;; Author: Phil Isaac <isaac.phil@gmail.com>
;; URL: https://github.com/isaacphi/gptel-ext
;; Version: 0.0.1

;;; Commentary:
;;
;; Extend gptel with some extra functions
;;
;;; Code:

(require 'gptel)
(require 'projectile)

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

(defvar gptel-ext-project-top-prompt
  "You will be provided with the full text of files in a software projet. You are to act as a helpful software engineer, providing concise answers about this project.\n"
  "Directive used before dumping project file text into chat.")

(defvar gptel-ext-project-bottom-prompt
 "Following the same heading format as above, summarize all of the project files.
More Important files should have longer summaries but none should be more than a couple of paragraphs. The summaries should include information about the interface of each file at the bottom: what are the important functions and constants?"
  "Directive used before dumping project file text into chat.")

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

(defun gptel-ext-project-summary ()
  (interactive)
  ;; Check if Projectile is loaded (featurep 'projectile)
  ;; and if the current buffer is in a Projectile project (projectile-project-p).
  (unless (and (featurep 'projectile) (projectile-project-p))
    (error "Projectile is not available, or you're not in a Projectile project"))

  (let* ((buffer-name (read-string "Enter buffer name: " "*GPTel Project*"))
         (buffer (get-buffer buffer-name)))
    ;; If buffer exists, pop to the bottom and end the function.
    (if buffer
        (progn
          (pop-to-buffer buffer)
          (goto-char (point-max))
          ;; TEMPORARY: always go to this buffer and clear it
          (erase-buffer))
      ;; (return))
      ;; Else, create the buffer.
      (setq gptel-buffer (get-buffer-create buffer-name))
      (with-current-buffer gptel-buffer
        (org-mode)
        (gptel-mode))))

  ;; If a new buffer is created, continue with setup.
  (when (buffer-live-p gptel-buffer)
    ;; Within the buffer, start inserting the content.
    (with-current-buffer gptel-buffer
      (insert "* Project Name:" (projectile-project-name) "\n")
      (insert gptel-ext-project-top-prompt)
      (insert "** Files:\n")
      (let ((project-files (projectile-project-files (projectile-project-root))))
        (dolist (file project-files)
          (let ((file-type (file-name-extension file)))
            (insert "*** " file "\n")
            (condition-case err
                ;; TODO: use list of files other than org-babel-tangle-lang-exts
                (if (member file-type (mapcar 'cdr org-babel-tangle-lang-exts))
                    (progn
                      (insert "#+BEGIN_SRC " (or (car (rassoc file-type org-babel-tangle-lang-exts)) "text") "\n")
                      (let ((file-contents (with-temp-buffer
                                (insert-file-contents (expand-file-name file (projectile-project-root)))
                                (buffer-string))))
                        (if (string= file-type "org")
                            (insert (replace-regexp-in-string "^\*" ",\*" file-contents))
                          (insert file-contents)))
                      ;; (insert (with-temp-buffer
                      ;;           (insert-file-contents (expand-file-name file (projectile-project-root)))
                      ;;           (buffer-string)))
                      (insert "#+END_SRC\n\n"))
                  (insert "File type " file-type " not supported.\n"))
              (error (insert "Error: Could not load the file contents. " (error-message-string err) "\n")))
            )))
      ;; After the file list, prepare the buffer for chat input from the user.
      (insert (gptel-prompt-prefix-string)))
      ;; Ask the AI to summarize each file
      (insert gptel-ext-project-bottom-prompt)
      (goto-char (point-max))) ;; Move the cursor to the end of the buffer.

    ;; Finally, display the newly populated buffer to the user.
    (pop-to-buffer gptel-buffer))

(provide 'gptel-extensions)

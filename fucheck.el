(defun fucheck-init ()
  "Initialises buffer for use with Fucheck. Ought to be a minor mode."
  (defvar fucheck-collapse-on-file-open t
    "Collapses Fucheck tests when a file is opened where fucheck is initialized. Global option.")
  (add-to-invisibility-spec '(fucheck . t))
  (setq-local fucheck-process-string nil)
  (setq-local fucheck-overlay-alist nil)
  (when fucheck-collapse-on-file-open
    (fucheck-collapse-all-tests)))

(defun fucheck-next-test (&optional count print-message)
  "Moves to the next Fucheck test. The numeric prefix argument COUNT is how many tests to move, and PRINT-MESSAGE is the message displayed when no tests are found when called interactively. Returns the name of the next test."
  (interactive (list current-prefix-arg "Could not find Fucheck test"))
  (let ((pos (re-search-forward
              "^[ 	]*--[ 	]*fucheck[ 	]*\\_<\\([A-Za-z][A-Za-z0-9_']*\\)"
              nil t (cond ((listp count) (car count))
                          ((equal '- count) -1)
                          (t count)))))
    (if pos (match-string 1) (when print-message (message print-message)) nil)))

(defun fucheck-appropriate-action (&optional action pos)
  "Figures out whether to collapse or expand tests. If action is either of the valid actions, the symbols 'collapse' or 'expand', return that, otherwise see if the text is hidden at position POS, which defaults to point."
  (cond ((or (equal action (quote collapse)) (equal action (quote expand))) action)
        ((get-text-property (if pos pos (point)) (quote invisible)) (quote expand))
        (t (quote collapse))))

(defun fucheck-delete-overlay (overlay)
  "Delete overlay used by Fucheck."
  (delete-overlay overlay)
  (setq-local fucheck-overlay-alist (assq-delete-all test fucheck-overlay-alist)))

(defun fucheck-make-overlay (start end test)
  "Make overlay used by Fucheck."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'invisible 'fucheck)
    (overlay-put overlay 'fucheck test)
    (set 'fucheck-overlay-alist
         (cons (cons test overlay) fucheck-overlay-alist))
    overlay))

(defun fucheck-overlay-at (&optional pos name)
  "Check whether an overlay used by Fucheck is at position POS by the name NAME."
  (unless pos (setq pos (point)))
  (reduce (lambda (x y) (or x y)) ; Should exit on first non-nil
          (mapcar (lambda (o)
                    (and (if name
                             (equal name (overlay-get o 'fucheck))
                           (overlay-get o 'fucheck))
                         (equal 'fucheck (overlay-get o 'invisible)) o))
                  (overlays-at pos))
          :initial-value nil))

(defun fucheck-fix-message (msg-list)
  "Fix up the list of strings printed to STDOUT by the Fucheck application. Used as a sentinel for Fucheck processes."
  (let* ((msg (apply 'concatenate 'string (reverse msg-list)))
         (end (- (length msg) 1)))
    (if (= 0 (length msg))
        "Fucheck exited without message"
      (while (char-equal ?\n (aref msg end))
        (setq end (- end 1)))
      (substring msg 0 (1+ end)))))

(defun fucheck-collapse-test (&optional action)
  "Collapse/expand the test at or above point. Toggles if ACTION is NIL, expands if it is the symbol 'expand' and collapses if it is the symbol 'collapse'."
  (interactive)
  (save-excursion
    (end-of-line)
    (when (fucheck-next-test -1) ; Get test name from here
      (end-of-line)

      (let* ((fut-fun-regex
              "^[ 	]*\\(?:let\\|entry\\)[ 	]*\\(?:gen\\|prop\\|show\\|labels\\|cond\\|state\\)_")
             (start (point))
             (test-name (match-string 1))
             (test (intern test-name))
             (overlay (fucheck-overlay-at))
             (inv-prop (fucheck-appropriate-action action)))

        (cond ((and overlay (equal action 'collapse)))
              (overlay (fucheck-delete-overlay overlay))
              (t (while (re-search-forward
                         (concatenate 'string fut-fun-regex test-name "[ \t\n\r]") nil t))
                 (let* ((empty-line (save-excursion (re-search-forward "^[ \t]*\n" nil t)))
                        (next-test (when (fucheck-next-test) (beginning-of-line) (point)))
                        (end (- (cond ((and empty-line next-test) (min empty-line next-test))
                                      (empty-line empty-line)
                                      (next-test next-test)
                                      (t (+ 1 (point-max))))
                                1)))
                   (fucheck-make-overlay start end test-name))))))))


(defun fucheck-collapse-all-tests (&optional action)
  "Collapses/expands all tests in the file. Toggles if ACTION is NIL, expands if it is the symbol 'expand' and collapses if it is the symbol 'collapse'."
  (interactive)
  (save-excursion
    (unless action (setq action 'collapse))
    (if fucheck-overlay-alist
        (progn
          (mapcar (lambda (x) (delete-overlay (cdr x))) fucheck-overlay-alist)
          (setq-local fucheck-overlay-alist nil))
      (goto-char (point-min))
      (fucheck-next-test)
      (while (progn (fucheck-collapse-test action)
                    (fucheck-next-test))))))

(defun fucheck-tests-in-region (start end)
  "Returns a list of the tests in the region if region is active, otherwise the nearest test above point."
  (save-excursion
    (if (not (use-region-p))
        (progn
          (end-of-line)
          (list (fucheck-next-test -1)))
      (let ((real-end (progn (goto-char end)
                             (end-of-line)
                             (point)))
            (tests nil)
            (current-test (progn
                            (goto-char start)
                            (beginning-of-line)
                            (fucheck-next-test))))
        (while (and current-test (<= (point) real-end))
          (setq tests (cons current-test tests))
          (setq current-test (fucheck-next-test)))
        tests))))

(defun fucheck-test-region (start end &optional prefix)
  "Tests all tests in the region. If region is not active, test the test at or above point. Writes the result to the echo area."
  (interactive "r\nP")
  (save-excursion
    (let ((backend (if prefix "opencl" "c"))
          (tests (fucheck-tests-in-region start end)))
      (make-process
       :name (apply 'concat "fucheck" (mapcar (lambda (x) (concat "-" x)) tests))
       :command (concatenate 'list (list "fucheck" backend "--only") tests (list buffer-file-name))
       :filter (lambda (proc string)
                 (process-put proc 'fucheck-string
                              (cons string (process-get proc 'fucheck-string))))
       :sentinel (lambda (proc event)
                   (message "%s"
                            (fucheck-fix-message (process-get proc 'fucheck-string))))))))

(defun fucheck-test-all (&optional prefix)
  "Tests all tests in the file. Writes the result to a buffer."
  (interactive "P")
  (let ((backend (if prefix "opencl" "c"))
        (name (generate-new-buffer-name
               (concat "fucheck-" (file-name-nondirectory (buffer-file-name))))))
    (make-process
     :name name
     :buffer name
     :command (list "fucheck" backend (buffer-file-name))
     :sentinel (lambda (proc event)
                 (let ((sel-win (selected-window)))
                   (pop-to-buffer (process-buffer proc))
                   (select-window sel-win))))))


(defun fucheck-preprocess ()
  "Writes the preprocessed code that Fucheck used to a buffer for inspection. Enables futhark-mode, but disables eldoc-mode as that requires the buffer to be associated with a file."
  (interactive)
  (let ((name (generate-new-buffer-name
               (concat "fucheck-" (file-name-nondirectory (buffer-file-name)) "-preprocess"))))
    (make-process
     :name name
     :buffer name
     :command (list "fucheck" "-o" (buffer-file-name))
     :sentinel (lambda (proc event)
                 (pop-to-buffer (process-buffer proc))
                 (futhark-mode)
                 (eldoc-mode -1)))))

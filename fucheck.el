;; fucheck-init
(defun fucheck-init ()
  (defvar fucheck-collapse-on-file-open t)
  (add-to-invisibility-spec '(fucheck . t))
  (setq-local fucheck-overlay-alist nil)
  (when fucheck-collapse-on-file-open
    (fucheck-collapse-all-tests)))

;; fucheck-next-test
(defun fucheck-next-test (&optional count print-message)
  (interactive (list current-prefix-arg "Could not find Fucheck test"))
  (let ((res (re-search-forward
              "^[ 	]*--[ 	]*fucheck[ 	]*\\_<\\([A-Za-z][A-Za-z0-9_']*\\)"
              nil t (cond ((listp count) (car count))
                          ((equal '- count) -1)
                          (t count)))))
    (if res res (when print-message (message print-message)) nil)))


;; fucheck-collapse-test
(defun fucheck-collapse-test (&optional action)
  (interactive)
  (save-excursion
    (end-of-line)
    (when (fucheck-next-test -1)
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
                                      (next-test next-test) (t (+ 1 (point-max))))
                                1)))
                   (fucheck-make-overlay start end test))))))))



;; fucheck-collapse-all-tests
(defun fucheck-collapse-all-tests (&optional action)
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

;; appropriate-action
(defun fucheck-appropriate-action (&optional action pos)
  (cond ((or (equal action (quote collapse)) (equal action (quote expand))) action)
        ((get-text-property (if pos pos (point)) (quote invisible)) (quote expand))
        (t (quote collapse))))

;; delete-overlay
(defun fucheck-delete-overlay (overlay)
  (delete-overlay overlay)
  (setq-local fucheck-overlay-alist (assq-delete-all test fucheck-overlay-alist)))

;; make-overlay
(defun fucheck-make-overlay (start end test)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'invisible 'fucheck)
    (overlay-put overlay 'fucheck 'test)
    (set (make-local-variable 'fucheck-overlay-alist)
         (cons (cons test overlay) fucheck-overlay-alist))
    overlay))

;; overlay-at
(defun fucheck-overlay-at (&optional pos name)
  (if pos nil (setq pos (point)))
  (reduce (lambda (x y) (or x y))
          (mapcar (lambda (o)
                    (and (if name
                             (equal name (overlay-get o (quote fucheck)))
                           (overlay-get o (quote fucheck)))
                         (equal (quote fucheck) (overlay-get o (quote invisible))) o))
                  (overlays-at pos))
          :initial-value nil))

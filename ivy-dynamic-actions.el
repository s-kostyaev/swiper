(cl-defun ivy-read (prompt collection
                    &key
                      predicate require-match initial-input
                      history preselect keymap update-fn sort
                      action unwind re-builder matcher dynamic-collection caller dynamic-action)
  "Read a string in the minibuffer, with completion.

PROMPT is a format string, normally ending in a colon and a
space; %d anywhere in the string is replaced by the current
number of matching candidates. For the literal % character,
escape it with %%. See also `ivy-count-format'.

COLLECTION is either a list of strings, a function, an alist, or
a hash table.

PREDICATE is applied to filter out the COLLECTION immediately.
This argument is for `completing-read' compat.

When REQUIRE-MATCH is non-nil, only memebers of COLLECTION can be
selected, i.e. custom text.

If INITIAL-INPUT is not nil, then insert that input in the
minibuffer initially.

HISTORY is a name of a variable to hold the completion session
history.

KEYMAP is composed with `ivy-minibuffer-map'.

If PRESELECT is not nil, then select the corresponding candidate
out of the ones that match the INITIAL-INPUT.

UPDATE-FN is called each time the current candidate(s) is changed.

When SORT is t, use `ivy-sort-functions-alist' for sorting.

ACTION is a lambda function to call after selecting a result. It
takes a single string argument.

UNWIND is a lambda function to call before exiting.

RE-BUILDER is a lambda function to call to transform text into a
regex pattern.

MATCHER is to override matching.

DYNAMIC-COLLECTION is a boolean to specify if the list of
candidates is updated after each input by calling COLLECTION.

CALLER is a symbol to uniquely identify the caller to `ivy-read'.
It is used, along with COLLECTION, to determine which
customizations apply to the current completion session."
  (let ((extra-actions (delete-dups
                        (append (plist-get ivy--actions-list t)
                                (plist-get ivy--actions-list this-command)
                                (plist-get ivy--actions-list caller)))))
    (when extra-actions
      (setq action
            (cond ((functionp action)
                   `(1
                     ("o" ,action "default")
                     ,@extra-actions))
                  ((null action)
                   `(1
                     ("o" identity "default")
                     ,@extra-actions))
                  (t
                   (delete-dups (append action extra-actions)))))))
  (let ((extra-sources (plist-get ivy--sources-list caller)))
    (if extra-sources
        (progn
          (setq ivy--extra-candidates nil)
          (dolist (source extra-sources)
            (cond ((equal source '(original-source))
                   (setq ivy--extra-candidates
                         (cons source ivy--extra-candidates)))
                  ((null (cdr source))
                   (setq ivy--extra-candidates
                         (cons
                          (list (car source) (funcall (car source)))
                          ivy--extra-candidates))))))
      (setq ivy--extra-candidates '((original-source)))))
  (let ((recursive-ivy-last (and (active-minibuffer-window) ivy-last))
        (transformer-fn
         (plist-get ivy--display-transformers-list
                    (or caller (and (functionp collection)
                                    collection)))))
    (setq ivy-last
          (make-ivy-state
           :prompt prompt
           :collection collection
           :predicate predicate
           :require-match require-match
           :initial-input initial-input
           :history history
           :preselect preselect
           :keymap keymap
           :update-fn update-fn
           :sort sort
           :action action
           :dynamic-action dynamic-action
           :window (selected-window)
           :buffer (current-buffer)
           :unwind unwind
           :re-builder re-builder
           :matcher matcher
           :dynamic-collection dynamic-collection
           :display-transformer-fn transformer-fn
           :caller caller))
    (ivy--reset-state ivy-last)
    (prog1
        (unwind-protect
             (minibuffer-with-setup-hook
                 #'ivy--minibuffer-setup
               (let* ((hist (or history 'ivy-history))
                      (minibuffer-completion-table collection)
                      (minibuffer-completion-predicate predicate)
                      (resize-mini-windows (cond
                                             ((display-graphic-p) nil)
                                             ((null resize-mini-windows) 'grow-only)
                                             (t resize-mini-windows))))
                 (read-from-minibuffer
                  prompt
                  (ivy-state-initial-input ivy-last)
                  (make-composed-keymap keymap ivy-minibuffer-map)
                  nil
                  hist)
                 (when (eq ivy-exit 'done)
                   (let ((item (if ivy--directory
                                   ivy--current
                                 ivy-text)))
                     (unless (equal item "")
                       (set hist (cons (propertize item 'ivy-index ivy--index)
                                       (delete item
                                               (cdr (symbol-value hist))))))))
                 ivy--current))
          (remove-hook 'post-command-hook #'ivy--exhibit)
          (when (setq unwind (ivy-state-unwind ivy-last))
            (funcall unwind))
          (unless (eq ivy-exit 'done)
            (when recursive-ivy-last
              (ivy--reset-state (setq ivy-last recursive-ivy-last)))))
      (ivy-call)
      (when (and recursive-ivy-last
                 ivy-recursive-restore)
        (ivy--reset-state (setq ivy-last recursive-ivy-last))))))
(cl-defstruct ivy-state
  prompt collection
  predicate require-match initial-input
  history preselect keymap update-fn sort
  ;; The window in which `ivy-read' was called
  window
  ;; The buffer in which `ivy-read' was called
  buffer
  ;; The value of `ivy-text' to be used by `ivy-occur'
  text
  action
  unwind
  re-builder
  matcher
  ;; When this is non-nil, call it for each input change to get new candidates
  dynamic-collection
  ;; A lambda that transforms candidates only for display
  display-transformer-fn
  caller
  dynamic-action)

(defun ivy-read-action ()
  "Change the action to one of the available ones.

Return nil for `minibuffer-keyboard-quit' or wrong key during the
selection, non-nil otherwise."
  (interactive)
  (let ((actions
         (if (ivy-state-dynamic-action ivy-last)
             (cons 1 (funcall (ivy-state-dynamic-action ivy-last) ivy--current))
           (ivy-state-action ivy-last))))
    (if (null (ivy--actionp actions))
        t
      (let* ((hint (funcall ivy-read-action-format-function (cdr actions)))
             (resize-mini-windows 'grow-only)
             (key (string (read-key hint)))
             (action-idx (cl-position-if
                          (lambda (x) (equal (car x) key))
                          (cdr actions))))
        (cond ((string= key "")
               nil)
              ((null action-idx)
               (message "%s is not bound" key)
               nil)
              (t
               (message "")
               (setcar actions (1+ action-idx))
               (ivy-set-action actions)))))))

(defvar test-result nil)
(defun test-action-1 (x)
  (setq test-result (format "action 1: %S" x)))

(defun test-action-2 (x)
  (setq test-result (format "action 2: %S" x)))

(defun test-action-3 (x)
  (setq test-result (format "action 3: %S" x)))

(defun test-dynamic-action (cand)
  (cond ((equal cand "one")
         '(("a" test-action-1 "action-1")
           ("b" test-action-2 "action-2")))
        ((equal cand "two")
         '(("a" test-action-2 "action-2")
           ("b" test-action-3 "action-3")))
        ((equal cand "three")
         '(("a" test-action-1 "action-1")
           ("b" test-action-2 "action-2")
           ("c" test-action-3 "action-3")))))
(progn
  (ivy-read "test: " '("one" "two" "three")
            :dynamic-action 'test-dynamic-action)
  test-result)

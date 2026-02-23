;;; ascii-tree.el --- Export org/markdown buffers as ASCII trees -*- lexical-binding: t; -*-

;; Author: Philippe IVALDI
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (markdown-mode "2.4"))
;; Keywords: org, markdown, outline, tree, ascii
;; URL: https://github.com/pivaldi/ascii-tree

;;; Commentary:
;; Export the current org-mode or markdown buffer as a tree-like ASCII view
;; into a dedicated buffer.

;;; Code:

(require 'cl-lib)

(defun ascii-tree-to-org (start end)
  "Convert an ASCII tree structure to Org-mode format."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
         (text (replace-regexp-in-string "\n+\\'" "" text))
         (lines (split-string text "\n"))
         (output-buf (get-buffer-create "*ascii-tree-org*"))
         (accum nil)
         (flush-accum 
          (lambda ()
            (save-match-data 
              (when accum
                (with-current-buffer output-buf
                  (setq accum (nreverse accum))
                  (let ((in-code-block nil))
                    (dolist (line accum)
                      (cond
                       ((string-match "^[ \t\240]*\\[code[ \t]*\\(.*\\)\\][ \t\240]*$" line)
                        (setq in-code-block t)
                        (let ((lang (match-string 1 line)))
                          (insert "#+BEGIN_SRC " (if (string= lang "") "" lang) "\n")))
                       ((string-match-p "^[ \t\240]*\\[endcode\\][ \t\240]*$" line)
                        (setq in-code-block nil)
                        (insert "#+END_SRC\n"))
                       ((string-match-p "^[ \t\240]*$" line)
                        (if in-code-block (insert "\n") (insert ":\n")))
                       (t (insert line "\n")))))
                  (setq accum nil)))))))

    (with-current-buffer output-buf
      (erase-buffer)
      (org-mode))

    (dolist (line lines)
      (cond
       ((string-match "^\\([│ \t\240]*\\)[├└]──[ \t\240]*\\([^ \t\240#]+\\)\\([ \t\240]*#[ \t\240]*\\(.*\\)\\)?$" line)
        (let* ((prefix (match-string 1 line))
               (file (match-string 2 line))
               (desc (match-string 4 line))
               (clean-prefix (replace-regexp-in-string "[\t]" "    " prefix))
               (clean-prefix (replace-regexp-in-string "[\240]" " " clean-prefix))
               (level (1+ (/ (length clean-prefix) 4))))
          (funcall flush-accum)
          (with-current-buffer output-buf
            ;; FIX: Removed artificial padding here too
            (if desc
                (insert (make-string level ?*) " " file " -- " desc "\n")
              (insert (make-string level ?*) " " file "\n")))))
       ((string-match "^[│ \t\240]*#[ \t\240]?\\(.*\\)$" line)
        (push (match-string 1 line) accum))
       ((string-match-p "^[│ \t\240]*$" line)
        ;; FIX: Restored the push to accumulator so Org generates `:` markers
        (push "" accum))
       ((and (not (string-match-p "[│├└#]" line)) (not (string-match-p "^[ \t\240]*$" line)))
        (let ((title (replace-regexp-in-string "\\`[ \t\240\n\r]+\\|[ \t\240\n\r]+\\'" "" line)))
          (funcall flush-accum)
          (with-current-buffer output-buf (insert "#+title: " title "\n"))))))
    (funcall flush-accum)
    (switch-to-buffer output-buf)))

(defun ascii-tree-from-org (start end)
  "Convert an Org-mode tree back into an ASCII tree structure.
Generates clean terminal-style indentation (spaces under `└──` nodes).
The result is placed in a new buffer called *ascii-tree-tree*."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
         (lines (split-string text "\n"))
         (output-buf (get-buffer-create "*ascii-tree-tree*"))
         (heading-info nil)
         (last-sibling-hash (make-hash-table :test 'eq))
         (is-last-at-level (make-vector 100 nil))
         (current-level 0)
         (in-code-block nil))

    ;; Pass 1: Identify all headings to determine which are "last siblings"
    (let ((idx 0))
      (dolist (line lines)
        (when (string-match "^\\(\\*+\\)[ \t]+" line)
          ;; Store as (line-index . heading-level)
          (push (cons idx (length (match-string 1 line))) heading-info))
        (setq idx (1+ idx))))

    (setq heading-info (nreverse heading-info))

    ;; Process headings with lookahead to flag the last items in a branch
    (let ((tail heading-info))
      (while tail
        (let* ((curr (car tail))
               (idx (car curr))
               (lvl (cdr curr))
               (is-last t)
               (lookahead (cdr tail)))
          (while lookahead
            (let ((next-lvl (cdr (car lookahead))))
              (cond
               ((< next-lvl lvl)
                ;; We hit a parent/ancestor before finding another sibling -> last sibling!
                (setq lookahead nil))
               ((= next-lvl lvl)
                ;; We found another sibling -> NOT the last sibling!
                (setq is-last nil)
                (setq lookahead nil))
               (t
                ;; We found a child (next-lvl > lvl) -> keep looking ahead
                (setq lookahead (cdr lookahead))))))
          (puthash idx is-last last-sibling-hash)
          (setq tail (cdr tail)))))

    ;; Helper lambda to dynamically generate the correct structural prefix
    (let ((make-prefix
           (lambda (for-text)
             (let ((res "")
                   (d 1))
               ;; Build the pipe/space indentations for parent levels
               (while (< d current-level)
                 (if (aref is-last-at-level d)
                     (setq res (concat res "    "))
                   (setq res (concat res "│   ")))
                 (setq d (1+ d)))

               ;; Build the marker for the current level
               (if for-text
                   (when (> current-level 0)
                     (if (aref is-last-at-level current-level)
                         (setq res (concat res "    "))
                       (setq res (concat res "│   "))))
                 (if (aref is-last-at-level current-level)
                     (setq res (concat res "└── "))
                   (setq res (concat res "├── "))))
               res))))

      ;; Pass 2: Reconstruct the visually perfect tree
      (with-current-buffer output-buf
        (erase-buffer))

      (let ((idx 0))
        (dolist (line lines)
          (cond
           ;; 1. Root Title
           ((string-match "^[ \t]*#\\+title:[ \t]*\\(.*\\)$" line)
            (with-current-buffer output-buf
              (insert (match-string 1 line) "\n")))

           ;; 2. Code Block Start
           ((string-match "^[ \t]*#\\+BEGIN_SRC[ \t]+\\(.*\\)$" line)
            (setq in-code-block t)
            (with-current-buffer output-buf
              (insert (funcall make-prefix t) "#[code " (match-string 1 line) "]\n")))

           ;; 3. Code Block End
           ((string-match-p "^[ \t]*#\\+END_SRC[ \t]*$" line)
            (setq in-code-block nil)
            (with-current-buffer output-buf
              (insert (funcall make-prefix t) "#[endcode]\n")))

           ;; 4. Node with Description
           ((string-match "^\\(\\*+\\)[ \t]+\\(.*?\\)[ \t]*--[ \t]*\\(.*\\)$" line)
            (let* ((stars (match-string 1 line))
                   (file (match-string 2 line))
                   (desc (match-string 3 line))
                   (level (length stars)))
              (setq current-level level)
              ;; Update state: is the active branch a last child?
              (aset is-last-at-level level (gethash idx last-sibling-hash))
              (with-current-buffer output-buf
                (insert (funcall make-prefix nil) file "  # " desc "\n"))))

           ;; 5. Node without Description
           ((string-match "^\\(\\*+\\)[ \t]+\\(.*\\)$" line)
            (let* ((stars (match-string 1 line))
                   (file (match-string 2 line))
                   (level (length stars)))
              (setq current-level level)
              (aset is-last-at-level level (gethash idx last-sibling-hash))
              (with-current-buffer output-buf
                (insert (funcall make-prefix nil) file "\n"))))

           ;; 6. The ":" Empty Line Marker
           ((string-match-p "^[ \t]*:[ \t]*$" line)
            (with-current-buffer output-buf
              (insert (funcall make-prefix t) "#\n")))

           ;; 7. True Blank Line
           ((string-match-p "^[ \t]*$" line)
            (with-current-buffer output-buf
              (let ((prefix (replace-regexp-in-string "[ \t]+$" "" (funcall make-prefix t))))
                (if (string= prefix "")
                    (insert "\n")
                  (insert prefix "\n")))))

           ;; 8. Regular Continuation Content
           (t
            (let ((content line))
              (unless in-code-block
                (setq content (replace-regexp-in-string "^[ \t]\\{1,4\\}" "" content)))
              (with-current-buffer output-buf
                (insert (funcall make-prefix t) "# " content "\n")))))
          (setq idx (1+ idx)))))
    (switch-to-buffer output-buf)))

(defun ascii-tree-to-md (start end)
  "Convert an ASCII tree structure to Markdown format."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
         (text (replace-regexp-in-string "\n+\\'" "" text))
         (lines (split-string text "\n"))
         (output-buf (get-buffer-create "*ascii-tree-md*"))
         (accum nil)
         (flush-accum 
          (lambda ()
            (save-match-data 
              (when accum
                (with-current-buffer output-buf
                  (setq accum (nreverse accum))
                  (let ((in-code-block nil))
                    (dolist (line accum)
                      (cond
                       ((string-match "^[ \t\240]*\\[code[ \t]*\\(.*\\)\\][ \t\240]*$" line)
                        (setq in-code-block t)
                        (let ((lang (match-string 1 line)))
                          (insert "```" (if (string= lang "") "" lang) "\n")))
                       ((string-match-p "^[ \t\240]*\\[endcode\\][ \t\240]*$" line)
                        (setq in-code-block nil)
                        (insert "```\n"))
                       ((string-match-p "^[ \t\240]*$" line)
                        (if in-code-block (insert "\n") (insert ":\n")))
                       (t (insert line "\n")))))
                  (setq accum nil)))))))

    (with-current-buffer output-buf
      (erase-buffer)
      (if (fboundp 'markdown-mode) (markdown-mode) (text-mode)))

    (dolist (line lines)
      (cond
       ((string-match "^\\([│ \t\240]*\\)[├└]──[ \t\240]*\\([^ \t\240#]+\\)\\([ \t\240]*#[ \t\240]*\\(.*\\)\\)?$" line)
        (let* ((prefix (match-string 1 line))
               (file (match-string 2 line))
               (desc (match-string 4 line))
               (clean-prefix (replace-regexp-in-string "[\t]" "    " prefix))
               (clean-prefix (replace-regexp-in-string "[\240]" " " clean-prefix))
               (level (1+ (/ (length clean-prefix) 4))))
          (funcall flush-accum)
          (with-current-buffer output-buf
            ;; FIX: Removed the artificial \n padding to guarantee idempotency
            (if desc
                (insert (make-string (1+ level) ?#) " " file " -- " desc "\n")
              (insert (make-string (1+ level) ?#) " " file "\n")))))
       ((string-match "^[│ \t\240]*#[ \t\240]?\\(.*\\)$" line)
        (push (match-string 1 line) accum))
       ((string-match-p "^[│ \t\240]*$" line)
        (funcall flush-accum)
        (with-current-buffer output-buf (insert "\n")))
       ((and (not (string-match-p "[│├└#]" line)) (not (string-match-p "^[ \t\240]*$" line)))
        (let ((title (replace-regexp-in-string "\\`[ \t\240\n\r]+\\|[ \t\240\n\r]+\\'" "" line)))
          (funcall flush-accum)
          (with-current-buffer output-buf (insert "# " title "\n"))))))
    (funcall flush-accum)
    (switch-to-buffer output-buf)))

(defun ascii-tree-from-md (start end)
  "Convert a Markdown tree back into an ASCII tree structure.
Generates clean terminal-style indentation and perfectly connects structural blank lines and text.
The result is placed in a new buffer called *ascii-tree-tree*."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
         (lines (split-string text "\n"))
         (output-buf (get-buffer-create "*ascii-tree-tree*"))
         (heading-info nil)
         (last-sibling-hash (make-hash-table :test 'eq))
         (is-last-at-level (make-vector 100 nil))
         (next-heading-level-array (make-vector (length lines) 0))
         (current-level 0)
         (in-code-block nil))

    ;; Pass 1a: Track next heading levels so lines know if a child is coming
    (let ((idx (1- (length lines)))
          (nxt-lvl 0))
      (while (>= idx 0)
        (aset next-heading-level-array idx nxt-lvl)
        (when (string-match "^\\(#+\\)[ \t]+" (nth idx lines))
          (setq nxt-lvl (1- (length (match-string 1 (nth idx lines))))))
        (setq idx (1- idx))))

    ;; Pass 1b: Identify all headings to determine which are "last siblings"
    (let ((idx 0))
      (dolist (line lines)
        (when (string-match "^\\(#+\\)[ \t]+" line)
          (push (cons idx (1- (length (match-string 1 line)))) heading-info))
        (setq idx (1+ idx))))

    (setq heading-info (nreverse heading-info))

    ;; Lookahead for siblings
    (let ((tail heading-info))
      (while tail
        (let* ((curr (car tail))
               (idx (car curr))
               (lvl (cdr curr))
               (is-last t)
               (lookahead (cdr tail)))
          (while lookahead
            (let ((next-lvl (cdr (car lookahead))))
              (cond
               ((< next-lvl lvl) (setq lookahead nil))
               ((= next-lvl lvl) (setq is-last nil) (setq lookahead nil))
               (t (setq lookahead (cdr lookahead))))))
          (puthash idx is-last last-sibling-hash)
          (setq tail (cdr tail)))))

    ;; Pass 2: The intelligent prefix generator
    (let ((make-prefix
           (lambda (type current-idx)
             (let ((res "")
                   (d 1)
                   (nxt (aref next-heading-level-array current-idx)))
               (cond
                ;; NODE lines cap with ├── or └──
                ((eq type 'node)
                 (while (< d current-level)
                   (if (aref is-last-at-level d)
                       (setq res (concat res "    "))
                     (setq res (concat res "│   ")))
                   (setq d (1+ d)))
                 (when (> current-level 0)
                   (if (aref is-last-at-level current-level)
                       (setq res (concat res "└── "))
                     (setq res (concat res "├── "))))
                 res)

                ;; TEXT lines cover current depth, PLUS extending pipes if children are coming
                ((eq type 'text)
                 (let ((target (max current-level nxt)))
                   (while (<= d target)
                     (if (<= d current-level)
                         (if (aref is-last-at-level d)
                             (setq res (concat res "    "))
                           (setq res (concat res "│   ")))
                       (setq res (concat res "│   ")))
                     (setq d (1+ d)))
                   res))

                ;; BLANK lines drop to 'nxt' depth and connect to upcoming node
                ((eq type 'blank)
                 (let ((target nxt))
                   (while (< d target)
                     (if (aref is-last-at-level d)
                         (setq res (concat res "    "))
                       (setq res (concat res "│   ")))
                     (setq d (1+ d)))
                   (when (> target 0)
                     (setq res (concat res "│   ")))
                   res)))))))

      ;; Pass 3: Rebuild the tree
      (with-current-buffer output-buf
        (erase-buffer))

      (let ((idx 0))
        (dolist (line lines)
          (cond
           ;; 1. Code Block End
           ((and in-code-block (string-match-p "^[ \t]*```[ \t]*$" line))
            (setq in-code-block nil)
            (with-current-buffer output-buf
              (insert (funcall make-prefix 'text idx) "#[endcode]\n")))

           ;; 2. Code Block Start
           ((and (not in-code-block) (string-match "^[ \t]*```\\(.*\\)$" line))
            (setq in-code-block t)
            (with-current-buffer output-buf
              (insert (funcall make-prefix 'text idx) "#[code " (match-string 1 line) "]\n")))

           ;; 3. Node with Description
           ((and (not in-code-block) (string-match "^\\(#+\\)[ \t]+\\(.*?\\)[ \t]*--[ \t]*\\(.*\\)$" line))
            (let* ((hashes (match-string 1 line))
                   (file (match-string 2 line))
                   (desc (match-string 3 line))
                   (level (1- (length hashes))))
              (if (= level 0)
                  (with-current-buffer output-buf (insert file "  # " desc "\n"))
                (setq current-level level)
                (aset is-last-at-level level (gethash idx last-sibling-hash))
                (with-current-buffer output-buf
                  (insert (funcall make-prefix 'node idx) file "  # " desc "\n")))))

           ;; 4. Node without Description
           ((and (not in-code-block) (string-match "^\\(#+\\)[ \t]+\\(.*\\)$" line))
            (let* ((hashes (match-string 1 line))
                   (file (match-string 2 line))
                   (level (1- (length hashes))))
              (if (= level 0)
                  (with-current-buffer output-buf (insert file "\n"))
                (setq current-level level)
                (aset is-last-at-level level (gethash idx last-sibling-hash))
                (with-current-buffer output-buf
                  (insert (funcall make-prefix 'node idx) file "\n")))))

           ;; 5. The ":" Empty Line Marker
           ((and (not in-code-block) (string-match-p "^[ \t]*:[ \t]*$" line))
            (with-current-buffer output-buf
              (insert (funcall make-prefix 'text idx) "#\n")))

           ;; 6. True Blank Line
           ((string-match-p "^[ \t]*$" line)
            (with-current-buffer output-buf
              (if in-code-block
                  ;; Code block blanks stay aligned with code
                  (let ((prefix (replace-regexp-in-string "[ \t]+$" "" (funcall make-prefix 'text idx))))
                    (if (string= prefix "") (insert "\n") (insert prefix "\n")))
                ;; Structural blanks connect via the 'blank' geometric rules
                (let ((prefix (replace-regexp-in-string "[ \t]+$" "" (funcall make-prefix 'blank idx))))
                  (if (string= prefix "") (insert "\n") (insert prefix "\n"))))))

           ;; 7. Regular Continuation Content
           (t
            (let ((content line))
              (unless in-code-block
                (setq content (replace-regexp-in-string "^[ \t]\\{1,4\\}" "" content)))
              (with-current-buffer output-buf
                (insert (funcall make-prefix 'text idx) "# " content "\n")))))
          (setq idx (1+ idx)))))
    (switch-to-buffer output-buf)))

(provide 'ascii-tree)
;;; ascii-tree.el ends here


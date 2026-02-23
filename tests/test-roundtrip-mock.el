;;; test-roundtrip-mock.el --- True round-trip test for mock.txt -*- lexical-binding: t; -*-

(require 'ert)
(require 'ascii-tree)

(defun ascii-tree-test--normalize-alignment (line)
  "Remove alignment spaces (multiple spaces before #) for structural comparison."
  (replace-regexp-in-string " \\{2,\\}#" " #" line))

(ert-deftest ascii-tree-roundtrip-mock-txt ()
  "True round-trip test: mock.txt -> import -> org -> export -> should equal mock.txt.
This test verifies that the full import/export cycle preserves the original tree structure.
Alignment spaces (extra spaces before comments) are normalized for comparison."
  (let ((mock-path (expand-file-name "tests/mock.txt")))
    (skip-unless (file-exists-p mock-path))

    (let* (;; Step 1: Read original tree format (GROUND TRUTH)
           (original-tree (with-temp-buffer
                            (insert-file-contents mock-path)
                            (buffer-string)))

           ;; Step 2: Import tree -> org format
           (lines (split-string original-tree "\n"))
           (parsed (ascii-tree-import--parse-lines lines))
           (org-output (ascii-tree-import--emit-org parsed)))

      ;; Sanity check: import should produce valid org content
      (should (> (length parsed) 0))
      (should (string-match-p "^\\* " org-output))

      ;; Step 3: Export org -> tree format (RESULT)
      (with-temp-buffer
        (insert org-output)
        (org-mode)
        (let* ((tree (org-element-parse-buffer))
               (out-buf (get-buffer-create " *roundtrip-test*"))
               (src-buf (current-buffer)))
          (with-current-buffer out-buf (erase-buffer))
          (ascii-tree--walk tree '() out-buf src-buf)

          ;; Step 4: Compare result to original
          (let* ((result-tree (with-current-buffer out-buf (buffer-string)))
                 ;; Split preserving empty lines (don't use OMIT-NULLS parameter!)
                 (original-lines (mapcar #'string-trim-right
                                         (split-string original-tree "\n")))
                 (result-lines (mapcar #'string-trim-right
                                       (split-string result-tree "\n")))
                 ;; Normalize alignment for structural comparison
                 (orig-normalized (mapcar #'ascii-tree-test--normalize-alignment original-lines))
                 (res-normalized (mapcar #'ascii-tree-test--normalize-alignment result-lines))
                 ;; Collect all differences
                 (differences '())
                 (structural-diffs 0)
                 (alignment-diffs 0))

            ;; Check line count first
            (should (= (length original-lines) (length result-lines)))

            ;; Collect ALL differences (don't stop at first)
            (dotimes (i (length original-lines))
              (let ((orig (nth i original-lines))
                    (res (nth i result-lines))
                    (orig-norm (nth i orig-normalized))
                    (res-norm (nth i res-normalized)))
                (unless (string= orig res)
                  (if (string= orig-norm res-norm)
                      ;; Only alignment difference (cosmetic)
                      (progn
                        (setq alignment-diffs (1+ alignment-diffs))
                        (push (list i "ALIGNMENT" orig res) differences))
                    ;; Structural difference (CRITICAL BUG)
                    (progn
                      (setq structural-diffs (1+ structural-diffs))
                      (push (list i "STRUCTURAL" orig res) differences))))))

            ;; Report findings
            (when differences
              (message "\n=== ROUNDTRIP COMPARISON ===")
              (message "Total lines: %d" (length original-lines))
              (message "Alignment differences: %d (cosmetic)" alignment-diffs)
              (message "Structural differences: %d (BUGS)" structural-diffs)

              ;; Show first few structural differences
              (let ((struct-diffs (seq-filter (lambda (d) (string= (nth 1 d) "STRUCTURAL"))
                                              (reverse differences))))
                (when struct-diffs
                  (message "\n=== STRUCTURAL BUGS ===")
                  (dolist (diff (seq-take struct-diffs 5))
                    (message "Line %d:" (1+ (nth 0 diff)))
                    (message "  Original: '%s'" (nth 2 diff))
                    (message "  Result:   '%s'" (nth 3 diff))))))

            ;; Test should pass if structure matches (even if alignment differs)
            (should (= structural-diffs 0)))

          (kill-buffer out-buf))))))

;;; test-roundtrip-mock.el ends here

;;; test-roundtrip-mock.el --- True round-trip test for mock.txt -*- lexical-binding: t; -*-

(require 'ert)
(require 'ascii-tree-export)

(ert-deftest ascii-tree-roundtrip-mock-txt ()
  "True round-trip test: mock.txt -> import -> org -> export -> should equal mock.txt.
This test verifies that the full import/export cycle preserves the original tree structure.
The exported result should match the original input (allowing only trailing whitespace differences)."
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
          (ascii-tree-export--walk tree '() out-buf src-buf)

          ;; Step 4: Compare result to original
          (let* ((result-tree (with-current-buffer out-buf (buffer-string)))
                 ;; Normalize: trim trailing whitespace from each line for comparison
                 (normalize-lines (lambda (text)
                                   (mapcar #'string-trim-right
                                          (split-string text "\n" t))))
                 (original-lines (funcall normalize-lines original-tree))
                 (result-lines (funcall normalize-lines result-tree)))

            ;; The roundtrip should preserve the exact same number of lines
            (unless (= (length original-lines) (length result-lines))
              (message "\n=== ORIGINAL (%d lines) ===" (length original-lines))
              (dolist (line original-lines)
                (message "%s" line))
              (message "\n=== RESULT (%d lines) ===" (length result-lines))
              (dolist (line result-lines)
                (message "%s" line))
              (should (= (length original-lines) (length result-lines))))

            ;; Each line should match exactly (after trimming trailing whitespace)
            (dotimes (i (length original-lines))
              (let ((orig (nth i original-lines))
                    (res (nth i result-lines)))
                (unless (string= orig res)
                  (message "\nLine %d mismatch:" (1+ i))
                  (message "  Original: '%s'" orig)
                  (message "  Result:   '%s'" res))
                (should (string= orig res)))))

          (kill-buffer out-buf))))))

;;; test-roundtrip-mock.el ends here

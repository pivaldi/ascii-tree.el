(require 'ert)

(ert-deftest test-complex-tree-org-roundtrip ()
  "Test full round-trip conversion: Tree -> Org -> Tree.
Uses an idempotency pattern to account for the parser normalizing raw spacing."
  (let* ((mock-path (expand-file-name "tests/mock.txt"))
         (raw-tree (with-temp-buffer
                     (insert-file-contents mock-path)
                     (buffer-string))))
    (unwind-protect
        (progn
          ;; 1. Parse RAW TREE -> ORG
          (with-temp-buffer
            (insert raw-tree)
            (ascii-tree-to-org (point-min) (point-max)))

          ;; 2. Parse ORG -> NORMALIZED TREE
          ;; This generates a perfectly formatted pipe tree
          (with-current-buffer "*ascii-tree-org*"
            (ascii-tree-from-org (point-min) (point-max)))

          ;; Capture the standardized baseline
          (let ((normalized-tree (with-current-buffer "*ascii-tree-tree*" (buffer-string))))

            ;; 3. Parse NORMALIZED TREE -> ORG 2
            (with-current-buffer "*ascii-tree-tree*"
              (ascii-tree-to-org (point-min) (point-max)))

            ;; 4. Parse ORG 2 -> FINAL TREE
            (with-current-buffer "*ascii-tree-org*"
              (ascii-tree-from-org (point-min) (point-max)))

            ;; 5. Assert complete idempotency
            (with-current-buffer "*ascii-tree-tree*"
              (should (string= (buffer-string) normalized-tree)))))

      ;; Cleanup buffers after test execution
      (when (get-buffer "*ascii-tree-tree*") (kill-buffer "*ascii-tree-tree*"))
      (when (get-buffer "*ascii-tree-org*") (kill-buffer "*ascii-tree-org*")))))

(ert-deftest test-complex-tree-md-roundtrip ()
  "Test full round-trip conversion: Tree -> Markdown -> Tree.
Uses an idempotency pattern to account for the parser normalizing raw spacing."
  (let* ((mock-path (expand-file-name "tests/mock.txt"))
         (raw-tree (with-temp-buffer
                     (insert-file-contents mock-path)
                     (buffer-string))))
    (unwind-protect
        (progn
          ;; 1. Parse RAW TREE -> ORG
          (with-temp-buffer
            (insert raw-tree)
            (ascii-tree-to-md (point-min) (point-max)))

          ;; 2. Parse MD -> NORMALIZED TREE
          ;; This generates a perfectly formatted pipe tree
          (with-current-buffer "*ascii-tree-md*"
            (ascii-tree-from-md (point-min) (point-max)))

          ;; Capture the standardized baseline
          (let ((normalized-tree (with-current-buffer "*ascii-tree-tree*" (buffer-string))))

            ;; 3. Parse NORMALIZED TREE -> MD 2
            (with-current-buffer "*ascii-tree-tree*"
              (ascii-tree-to-md (point-min) (point-max)))

            ;; 4. Parse MD 2 -> FINAL TREE
            (with-current-buffer "*ascii-tree-md*"
              (ascii-tree-from-md (point-min) (point-max)))

            ;; 5. Assert complete idempotency
            (with-current-buffer "*ascii-tree-tree*"
              (should (string= (buffer-string) normalized-tree)))))

      ;; Cleanup buffers after test execution
      (when (get-buffer "*ascii-tree-md*") (kill-buffer "*ascii-tree-md*"))
      (when (get-buffer "*ascii-tree-tree*") (kill-buffer "*ascii-tree-tree*")))))

;;; test-whole-buffer.el --- Test whole buffer operations -*- lexical-binding: t; -*-

(require 'ert)
(require 'ascii-tree)

(ert-deftest test-ascii-tree-to-org-whole-buffer ()
  "Test `ascii-tree-to-org` operates on whole buffer when no region is active."
  (let ((input-string "service-manager/
├── go.mod        # Go module
└── readme.txt")
        (expected-output "#+title: service-manager/
* go.mod -- Go module
* readme.txt\n")
        (input-buf (generate-new-buffer "*test-tree-input*")))

    (unwind-protect
        (progn
          ;; Setup buffer with content but NO active region
          (with-current-buffer input-buf
            (insert input-string)
            ;; Move point to beginning - no region active
            (goto-char (point-min))
            ;; Call function - should use whole buffer since no region
            (ascii-tree-to-org (point-min) (point-max)))

          ;; Verify output
          (with-current-buffer "*ascii-tree-org*"
            (should (string= (buffer-string) expected-output))))

      ;; Cleanup
      (kill-buffer input-buf)
      (when (get-buffer "*ascii-tree-org*")
        (kill-buffer "*ascii-tree-org*")))))

(ert-deftest test-ascii-tree-to-md-whole-buffer ()
  "Test `ascii-tree-to-md` operates on whole buffer when no region is active."
  (let ((input-string "service-manager/
├── go.mod        # Go module
└── readme.txt")
        (expected-output "# service-manager/
## go.mod -- Go module
## readme.txt\n")
        (input-buf (generate-new-buffer "*test-tree-input*")))

    (unwind-protect
        (progn
          (with-current-buffer input-buf
            (insert input-string)
            (goto-char (point-min))
            (ascii-tree-to-md (point-min) (point-max)))

          (with-current-buffer "*ascii-tree-md*"
            (should (string= (buffer-string) expected-output))))

      (kill-buffer input-buf)
      (when (get-buffer "*ascii-tree-md*")
        (kill-buffer "*ascii-tree-md*")))))

(ert-deftest test-ascii-tree-from-org-whole-buffer ()
  "Test `ascii-tree-from-org` operates on whole buffer when no region is active."
  (let ((input-org "#+title: service-manager/
* go.mod -- Go module
* readme.txt")
        (expected-output "service-manager/
├── go.mod  # Go module
└── readme.txt\n")
        (input-buf (generate-new-buffer "*test-org-input*")))

    (unwind-protect
        (progn
          (with-current-buffer input-buf
            (insert input-org)
            (goto-char (point-min))
            (ascii-tree-from-org (point-min) (point-max)))

          (with-current-buffer "*ascii-tree-tree*"
            (should (string= (buffer-string) expected-output))))

      (kill-buffer input-buf)
      (when (get-buffer "*ascii-tree-tree*")
        (kill-buffer "*ascii-tree-tree*")))))

(ert-deftest test-ascii-tree-from-md-whole-buffer ()
  "Test `ascii-tree-from-md` operates on whole buffer when no region is active."
  (let ((input-md "# service-manager/

## go.mod -- Go module

## readme.txt")
        (expected-output "service-manager/
│
├── go.mod  # Go module
│
└── readme.txt\n")
        (input-buf (generate-new-buffer "*test-md-input*")))

    (unwind-protect
        (progn
          (with-current-buffer input-buf
            (insert input-md)
            (goto-char (point-min))
            (ascii-tree-from-md (point-min) (point-max)))

          (with-current-buffer "*ascii-tree-tree*"
            (should (string= (buffer-string) expected-output))))

      (kill-buffer input-buf)
      (when (get-buffer "*ascii-tree-tree*")
        (kill-buffer "*ascii-tree-tree*")))))

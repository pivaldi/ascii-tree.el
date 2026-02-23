;;; test-basic.el --- Basic smoke tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'ascii-tree)

(ert-deftest test-ascii-tree-to-org ()
  "Test `ascii-tree-to-org` with a sample tree snippet including code blocks."
  (let* ((input-string "service-manager/
├── go.mod        # Go module
│   #[code go]
│   # module example.com/service
│   #[endcode]
│
└── no-desc-dir/
    └── blank.txt")

         (expected-output "#+title: service-manager/
* go.mod -- Go module
#+BEGIN_SRC go
module example.com/service
#+END_SRC
:
* no-desc-dir/
** blank.txt\n")
         (input-buf (generate-new-buffer "*test-tree-input*")))

    (unwind-protect
        (progn
          ;; 1. Setup the dummy input buffer
          (with-current-buffer input-buf
            (insert input-string)
            ;; 2. Run the function on the whole buffer
            (ascii-tree-to-org (point-min) (point-max)))

          ;; 3. Check the output buffer
          (with-current-buffer "*ascii-tree-org*"
            ;; `should` is ERT's assertion. If this is false, the test fails.
            (should (string= (buffer-string) expected-output))))

      ;; 4. Cleanup regardless of whether the test passed or failed
      (kill-buffer input-buf)
      (when (get-buffer "*ascii-tree-org*")
        (kill-buffer "*ascii-tree-org*")))))

(ert-deftest test-ascii-tree-from-org ()
  "Test `ascii-tree-from-org` to ensure it correctly builds the ASCII tree."
  (let* ((input-org "#+title: service-manager/
* go.mod -- Go module
#+BEGIN_SRC go
module example.com/service
#+END_SRC
:
* src/
** main.go -- Main entrypoint")

         ;; Notice the exact trailing newline expectations and pipe alignments
         (expected-tree "service-manager/
├── go.mod  # Go module
│   #[code go]
│   # module example.com/service
│   #[endcode]
│   #
└── src/
    └── main.go  # Main entrypoint\n")

         (input-buf (generate-new-buffer "*test-org-input*")))

    (unwind-protect
        (progn
          ;; 1. Setup the dummy input buffer
          (with-current-buffer input-buf
            (insert input-org)
            ;; 2. Run the reverse function
            (ascii-tree-from-org (point-min) (point-max)))

          ;; 3. Check the output buffer
          (with-current-buffer "*ascii-tree-tree*"
            (should (string= (buffer-string) expected-tree))))

      ;; 4. Cleanup
      (kill-buffer input-buf)
      (when (get-buffer "*ascii-tree-tree*")
        (kill-buffer "*ascii-tree-tree*")))))

(ert-deftest test-ascii-tree-to-md ()
  "Test `ascii-tree-to-md` with a sample tree snippet including code blocks."
  (let* ((input-string "service-manager/
├── go.mod        # Go module
│   #[code go]
│   # module example.com/service
│   #[endcode]
│
└── no-desc-dir/
    └── blank.txt")

         (expected-output "# service-manager/
## go.mod -- Go module
```go
module example.com/service
```

## no-desc-dir/
### blank.txt\n")
         (input-buf (generate-new-buffer "*test-tree-input*")))

    (unwind-protect
        (progn
          ;; 1. Setup the dummy input buffer
          (with-current-buffer input-buf
            (insert input-string)
            ;; 2. Run the function on the whole buffer
            (ascii-tree-to-md (point-min) (point-max)))

          ;; 3. Check the output buffer
          (with-current-buffer "*ascii-tree-md*"
            ;; `should` is ERT's assertion. If this is false, the test fails.
            (should (string= (buffer-string) expected-output))))

      ;; 4. Cleanup regardless of whether the test passed or failed
      (kill-buffer input-buf)
      (when (get-buffer "*ascii-tree-md*")
        (kill-buffer "*ascii-tree-md*")))))

(ert-deftest test-ascii-tree-from-md ()
  "Test `ascii-tree-from-md` to ensure it correctly builds the ASCII tree."
  (let* ((input-md "# service-manager/
## go.mod -- Go module
```go
module example.com/service
```

## src/
### main.go -- Main entrypoint")

         ;; Notice the exact trailing newline expectations and pipe alignments
         (expected-tree "service-manager/
├── go.mod  # Go module
│   #[code go]
│   # module example.com/service
│   #[endcode]
│
└── src/
    └── main.go  # Main entrypoint\n")

         (input-buf (generate-new-buffer "*test-md-input*")))

    (unwind-protect
        (progn
          ;; 1. Setup the dummy input buffer
          (with-current-buffer input-buf
            (insert input-md)
            ;; 2. Run the reverse function
            (ascii-tree-from-md (point-min) (point-max)))

          ;; 3. Check the output buffer
          (with-current-buffer "*ascii-tree-tree*"
            (should (string= (buffer-string) expected-tree))))

      ;; 4. Cleanup
      (kill-buffer input-buf)
      (when (get-buffer "*ascii-tree-tree*")
        (kill-buffer "*ascii-tree-tree*")))))

;;; test-basic.el ends here

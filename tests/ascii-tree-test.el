;;; ascii-tree-test.el --- ERT tests for ascii-tree -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ascii-tree)

(ert-deftest ascii-tree--build-prefix-root ()
  "Level-1 headlines have no prefix."
  (should (equal "" (ascii-tree--build-prefix '()))))

(ert-deftest ascii-tree--build-prefix-non-last ()
  "Non-last child of root gets ├──."
  (should (equal "├── " (ascii-tree--build-prefix '(nil)))))

(ert-deftest ascii-tree--build-prefix-last ()
  "Last child of root gets └──."
  (should (equal "└── " (ascii-tree--build-prefix '(t)))))

(ert-deftest ascii-tree--build-prefix-nested-non-last ()
  "Nested non-last: parent pipe + connector."
  (should (equal "│   ├── " (ascii-tree--build-prefix '(nil nil)))))

(ert-deftest ascii-tree--build-prefix-nested-last ()
  "Nested last child under non-last parent."
  (should (equal "│   └── " (ascii-tree--build-prefix '(nil t)))))

(ert-deftest ascii-tree--build-prefix-under-last-parent ()
  "Child under a last-sibling parent uses spaces, not pipe."
  (should (equal "    ├── " (ascii-tree--build-prefix '(t nil)))))

(defun ascii-tree-test--make-headline (title level)
  "Create a minimal org-element headline node for testing."
  (org-element-create 'headline
                      (list :level level :raw-value title)))

(ert-deftest ascii-tree--format-headline-simple ()
  "Headline with no subtitle."
  (let ((hl (ascii-tree-test--make-headline "Editor" 2)))
    (should (equal "├── Editor"
                   (ascii-tree--format-headline hl '(nil))))))

(ert-deftest ascii-tree--format-headline-with-subtitle ()
  "Headline with subtitle separated by ascii-tree-subtitle-separator."
  (let ((hl (ascii-tree-test--make-headline "Diagnostics -- Lorem ipsum dolor sit amet" 2)))
    (should (equal "├── Diagnostics # Lorem ipsum dolor sit amet"
                   (ascii-tree--format-headline hl '(nil))))))

(ert-deftest ascii-tree--format-headline-root ()
  "Root headline (empty spine) has no connector."
  (let ((hl (ascii-tree-test--make-headline "Basic Keybindings" 1)))
    (should (equal "Basic Keybindings"
                   (ascii-tree--format-headline hl '())))))

(ert-deftest ascii-tree--format-headline-last-sibling ()
  "Last sibling uses └── connector."
  (let ((hl (ascii-tree-test--make-headline "Editor" 2)))
    (should (equal "└── Editor"
                   (ascii-tree--format-headline hl '(t))))))

(ert-deftest ascii-tree--content-prefix-single ()
  "Non-last level-1: content prefixed with │   ."
  (should (equal "│   " (ascii-tree--content-prefix '(nil)))))

(ert-deftest ascii-tree--content-prefix-last ()
  "Last level-1: content prefixed with spaces only."
  (should (equal "    " (ascii-tree--content-prefix '(t)))))

(ert-deftest ascii-tree--content-prefix-nested ()
  "Level-2 non-last under non-last level-1."
  (should (equal "│   │   " (ascii-tree--content-prefix '(nil nil)))))

(ert-deftest ascii-tree--format-content-paragraph ()
  "Paragraph text is returned as indented lines."
  (with-temp-buffer
    (org-mode)
    (insert "* H\nSome text here.\n")
    (let* ((tree (org-element-parse-buffer))
           (hl   (org-element-map tree 'headline #'identity nil t))
           (para (org-element-map hl 'paragraph #'identity nil t))
           (lines (ascii-tree--format-content para '(nil) (current-buffer))))
      (should (member "│   Some text here." lines)))))

(ert-deftest ascii-tree--format-content-plain-list ()
  "Plain list items are returned as indented lines."
  (with-temp-buffer
    (org-mode)
    (insert "* H\n- Item one\n- Item two\n")
    (let* ((tree  (org-element-parse-buffer))
           (hl    (org-element-map tree 'headline #'identity nil t))
           (plist (org-element-map hl 'plain-list #'identity nil t))
           (lines (ascii-tree--format-content plist '(nil) (current-buffer))))
      (should (cl-some (lambda (l) (string-match-p "Item one" l)) lines))
      (should (cl-some (lambda (l) (string-match-p "Item two" l)) lines)))))

(ert-deftest ascii-tree--walk-integration ()
  "Walk a simple org AST and verify output lines."
  (with-temp-buffer
    (org-mode)
    (insert "#+title: My Doc\n\n* Section\n\nSome text.\n\n** Sub -- tag\n\n- Item\n")
    (let* ((tree   (org-element-parse-buffer))
           (out    (get-buffer-create " *ascii-tree-test-output*"))
           (src    (current-buffer)))
      (with-current-buffer out (erase-buffer))
      (ascii-tree--walk tree '() out src)
      (let ((content (with-current-buffer out (buffer-string))))
        (should (string-match-p "Section" content))
        (should (string-match-p "Sub # tag" content))
        (should (string-match-p "Some text" content))
        (should (string-match-p "Item" content)))
      (kill-buffer out))))

(ert-deftest ascii-tree-entry-point ()
  "Entry point produces correct output buffer for a simple org file."
  (with-temp-buffer
    (org-mode)
    ;; Simulate a file-visiting buffer
    (setq buffer-file-name "/tmp/test-export.org")
    (insert "* Alpha\n- First item\n** Beta\n- Second item\n")
    (ascii-tree)
    (let* ((out-name "*ascii-tree-test-export.org*")
           (out-buf  (get-buffer out-name)))
      (should out-buf)
      (let ((content (with-current-buffer out-buf (buffer-string))))
        (should (string-match-p "Alpha" content))
        (should (string-match-p "Beta" content))
        (should (string-match-p "First item" content)))
      (kill-buffer out-buf))))

;;; Markdown support tests

(ert-deftest ascii-tree--md-parse-headings-basic ()
  "Parse a simple markdown buffer into (level title content) triples."
  (with-temp-buffer
    (insert "# Alpha\n## Beta\n### Gamma\n## Delta\n# Epsilon\n")
    (let ((headings (ascii-tree--md-parse-headings)))
      (should (equal 5 (length headings)))
      (should (equal 1       (car  (nth 0 headings))))
      (should (equal "Alpha" (cadr (nth 0 headings))))
      (should (equal 2       (car  (nth 1 headings))))
      (should (equal "Beta"  (cadr (nth 1 headings)))))))

(ert-deftest ascii-tree--md-parse-headings-captures-content ()
  "Non-heading lines are captured as content of their preceding heading."
  (with-temp-buffer
    (insert "# Title\nSome paragraph text.\n- list item\n## Sub\n")
    (let ((headings (ascii-tree--md-parse-headings)))
      (should (equal 2 (length headings)))
      (should (equal "Title" (cadr (nth 0 headings))))
      (should (member "Some paragraph text." (caddr (nth 0 headings))))
      (should (member "- list item"          (caddr (nth 0 headings))))
      (should (equal "Sub"   (cadr (nth 1 headings))))
      (should (null          (caddr (nth 1 headings)))))))

(ert-deftest ascii-tree--walk-markdown-tree-shape ()
  "walk-markdown produces correct tree connectors and content lines."
  (let* ((headings '((1 "A"  ("Para under A."))
                     (2 "A1" nil)
                     (2 "A2" ("Item in A2."))
                     (1 "B"  nil)))
         (out (get-buffer-create " *md-walk-test*")))
    (with-current-buffer out (erase-buffer))
    (ascii-tree--walk-markdown headings out)
    (let ((content (with-current-buffer out (buffer-string))))
      (should (string-match-p "├── A"         content))
      (should (string-match-p "Para under A"  content))
      (should (string-match-p "├── A1"        content))
      (should (string-match-p "└── A2"        content))
      (should (string-match-p "Item in A2"    content))
      (should (string-match-p "└── B"         content)))
    (kill-buffer out)))

(ert-deftest ascii-tree-markdown-entry-point ()
  "Entry point handles a markdown-mode buffer."
  (with-temp-buffer
    (markdown-mode)
    (setq buffer-file-name "/tmp/test-export.md")
    (insert "# Alpha\n## Beta\n### Gamma\n")
    (ascii-tree)
    (let* ((out-name "*ascii-tree-test-export.md*")
           (out-buf  (get-buffer out-name)))
      (should out-buf)
      (let ((content (with-current-buffer out-buf (buffer-string))))
        (should (string-match-p "Alpha" content))
        (should (string-match-p "Beta"  content))
        (should (string-match-p "Gamma" content)))
      (kill-buffer out-buf))))

;;; ascii-tree-import tests

(ert-deftest ascii-tree-import--strip-tree-chars-root ()
  "Root level has depth 0."
  (let ((result (ascii-tree-import--strip-tree-chars "├── contracts/")))
    (should (equal 0 (car result)))
    (should (equal "contracts/" (cdr result)))))

(ert-deftest ascii-tree-import--strip-tree-chars-depth-1 ()
  "First level child has depth 1."
  (let ((result (ascii-tree-import--strip-tree-chars "│   ├── go.mod")))
    (should (equal 1 (car result)))
    (should (equal "go.mod" (cdr result)))))

(ert-deftest ascii-tree-import--strip-tree-chars-depth-2 ()
  "Second level child has depth 2."
  (let ((result (ascii-tree-import--strip-tree-chars "│   │   ├── api.go")))
    (should (equal 2 (car result)))
    (should (equal "api.go" (cdr result)))))

(ert-deftest ascii-tree-import--strip-tree-chars-last-sibling ()
  "Last sibling uses └── connector."
  (let ((result (ascii-tree-import--strip-tree-chars "│   └── README.md")))
    (should (equal 1 (car result)))
    (should (equal "README.md" (cdr result)))))

(ert-deftest ascii-tree-import--strip-tree-chars-content-line ()
  "Content line with only pipes/spaces."
  (let ((result (ascii-tree-import--strip-tree-chars "│   │   Dependencies: ZERO")))
    (should (equal 2 (car result)))
    (should (equal "Dependencies: ZERO" (cdr result)))))

(ert-deftest ascii-tree-import--strip-tree-chars-spaces-only ()
  "Spaces instead of pipes (under last sibling)."
  (let ((result (ascii-tree-import--strip-tree-chars "    └── final.txt")))
    (should (equal 1 (car result)))
    (should (equal "final.txt" (cdr result)))))

(ert-deftest ascii-tree-import--parse-headline-no-separator ()
  "Headline with no subtitle separator."
  (let ((result (ascii-tree-import--parse-headline "README.md")))
    (should (equal "README.md" (car result)))
    (should (null (cdr result)))))

(ert-deftest ascii-tree-import--parse-headline-double-dash ()
  "Headline with -- separator (ascii-tree convention)."
  (let ((result (ascii-tree-import--parse-headline "go.mod -- Module definition")))
    (should (equal "go.mod" (car result)))
    (should (equal "Module definition" (cdr result)))))

(ert-deftest ascii-tree-import--parse-headline-hash ()
  "Headline with # separator (tree command convention)."
  (let ((result (ascii-tree-import--parse-headline "contracts/ # Unified module")))
    (should (equal "contracts/" (car result)))
    (should (equal "Unified module" (cdr result)))))

(ert-deftest ascii-tree-import--parse-headline-hash-priority ()
  "# separator takes priority over --."
  (let ((result (ascii-tree-import--parse-headline "file.txt # comment -- note")))
    (should (equal "file.txt" (car result)))
    (should (equal "comment -- note" (cdr result)))))

(ert-deftest ascii-tree-import--parse-lines-simple ()
  "Parse simple tree with headlines only."
  (let* ((lines '("├── contracts/"
                  "│   ├── go.mod"
                  "│   └── README.md"))
         (result (ascii-tree-import--parse-lines lines)))
    (should (equal 3 (length result)))
    ;; First headline
    (should (equal 0 (nth 0 (nth 0 result))))
    (should (equal "contracts/" (nth 1 (nth 0 result))))
    (should (null (nth 2 (nth 0 result))))
    ;; Second headline
    (should (equal 1 (nth 0 (nth 1 result))))
    (should (equal "go.mod" (nth 1 (nth 1 result))))))

(ert-deftest ascii-tree-import--parse-lines-with-content ()
  "Parse tree with content lines."
  (let* ((lines '("├── contracts/ # Module"
                  "│   Some description"
                  "│   "
                  "│   ├── go.mod -- Manifest"))
         (result (ascii-tree-import--parse-lines lines)))
    (should (equal 2 (length result)))
    ;; First headline with content (content now stored as (depth . text) pairs)
    (should (equal "contracts/" (nth 1 (nth 0 result))))
    (should (equal "Module" (nth 2 (nth 0 result))))
    (should (equal '((1 . "Some description")) (nth 3 (nth 0 result))))
    ;; Second headline
    (should (equal "go.mod" (nth 1 (nth 1 result))))
    (should (equal "Manifest" (nth 2 (nth 1 result))))))

(ert-deftest ascii-tree-import--parse-lines-multi-line-content ()
  "Content with multiple lines."
  (let* ((lines '("├── api.go"
                  "│   type Service interface {"
                  "│     GetData() error"
                  "│   }"))
         (result (ascii-tree-import--parse-lines lines)))
    (should (equal 1 (length result)))
    (should (equal 3 (length (nth 3 (nth 0 result)))))
    ;; Content now stored as (depth . text) pairs - check the text part
    (should (member (cons 1 "type Service interface {") (nth 3 (nth 0 result))))))

(ert-deftest ascii-tree-import--emit-org-simple ()
  "Emit org format for simple headlines."
  (let* ((parsed '((0 "contracts/" nil nil)
                   (1 "go.mod" nil nil)))
         (result (ascii-tree-import--emit-org parsed)))
    (should (string-match-p "^\\* contracts/" result))
    (should (string-match-p "^\\*\\* go.mod" result))))

(ert-deftest ascii-tree-import--emit-org-with-subtitle ()
  "Emit org format with subtitle separator."
  (let* ((parsed '((0 "go.mod" "Module definition" nil)))
         (result (ascii-tree-import--emit-org parsed)))
    (should (string-match-p "\\* go.mod -- Module definition" result))))

(ert-deftest ascii-tree-import--emit-org-with-content ()
  "Emit org format with content lines."
  ;; Content now stored as (depth . text) pairs
  (let* ((parsed '((0 "contracts/" "Module" ((0 . "Some description") (0 . "More text")))))
         (result (ascii-tree-import--emit-org parsed)))
    (should (string-match-p "\\* contracts/ -- Module" result))
    (should (string-match-p "Some description" result))
    (should (string-match-p "More text" result))))

(ert-deftest ascii-tree-import--emit-org-depth-levels ()
  "Org level = depth + 1."
  (let* ((parsed '((0 "L1" nil nil)
                   (1 "L2" nil nil)
                   (2 "L3" nil nil)))
         (result (ascii-tree-import--emit-org parsed)))
    (should (string-match-p "^\\* L1" result))
    (should (string-match-p "^\\*\\* L2" result))
    (should (string-match-p "^\\*\\*\\* L3" result))))

(ert-deftest ascii-tree-import-entry-point-buffer ()
  "Entry point converts whole buffer when no region active."
  (with-temp-buffer
    (insert "├── contracts/\n")
    (insert "│   Module definition\n")
    (insert "│   ├── go.mod -- Manifest\n")
    (ascii-tree-import)
    (let* ((out-buf (get-buffer "*ascii-tree-import-result*")))
      (should out-buf)
      (with-current-buffer out-buf
        (should (eq major-mode 'org-mode))
        (let ((content (buffer-string)))
          (should (string-match-p "\\* contracts/" content))
          (should (string-match-p "Module definition" content))
          (should (string-match-p "\\*\\* go.mod -- Manifest" content))))
      (kill-buffer out-buf))))

(ert-deftest ascii-tree-import-entry-point-region ()
  "Entry point converts only selected region when active."
  (with-temp-buffer
    (insert "Ignore this\n")
    (insert "├── contracts/\n")
    (insert "│   └── go.mod\n")
    (insert "Ignore this too\n")
    ;; Select region (lines 2-3)
    (goto-char (point-min))
    (forward-line 1)
    (set-mark (point))
    (forward-line 2)
    (activate-mark)
    (ascii-tree-import)
    (let* ((out-buf (get-buffer "*ascii-tree-import-result*")))
      (should out-buf)
      (with-current-buffer out-buf
        (let ((content (buffer-string)))
          (should (string-match-p "\\* contracts/" content))
          (should (string-match-p "\\*\\* go.mod" content))
          (should-not (string-match-p "Ignore" content))))
      (kill-buffer out-buf))))

(ert-deftest ascii-tree-import-entry-point-empty-input ()
  "Entry point signals error on empty input."
  (with-temp-buffer
    (should-error (ascii-tree-import) :type 'user-error)))

(ert-deftest ascii-tree-roundtrip-mock-txt ()
  "Round-trip conversion preserves structure from mock.txt."
  (let* ((test-dir (file-name-directory (or load-file-name
                                            (buffer-file-name (current-buffer)))))
         (mock-path (expand-file-name "mock.txt" test-dir))
         (original (when (file-exists-p mock-path)
                     (with-temp-buffer
                       (insert-file-contents mock-path)
                       (buffer-string)))))

    ;; Skip test if mock.txt not found
    (skip-unless original)

    (let* ((lines (split-string original "\n"))
           (parsed (ascii-tree-import--parse-lines lines))
           (org-output (ascii-tree-import--emit-org parsed)))

      ;; Test import produces valid org
      (should (> (length parsed) 100))
      (should (string-match-p "^\\* " org-output))

      ;; Test export back to tree preserves key structures
      (with-temp-buffer
        (insert org-output)
        (org-mode)
        (let* ((tree (org-element-parse-buffer))
               (out-buf (get-buffer-create " *roundtrip-test*"))
               (src-buf (current-buffer)))
          (with-current-buffer out-buf (erase-buffer))
          (ascii-tree--walk tree '() out-buf src-buf)

          (let ((result (with-current-buffer out-buf (buffer-string))))
            ;; Verify key content is preserved
            (should (string-match-p "contracts/" result))
            (should (string-match-p "definitions/" result))
            (should (string-match-p "serviceasvc/" result))
            (should (string-match-p "ZERO dependencies" result))

            ;; Verify depth is preserved for specific lines (using last pipe position)
            (let ((input-line (seq-find (lambda (l) (string-match "ZERO dependencies - truly" l)) lines))
                  (output-line (seq-find (lambda (l) (string-match "ZERO dependencies - truly" l))
                                         (split-string result "\n"))))
              (when (and input-line output-line)
                (let ((input-depth (car (ascii-tree-import--strip-tree-chars input-line)))
                      (output-depth (car (ascii-tree-import--strip-tree-chars output-line))))
                  ;; Depth should be 3 in both (alignment spaces ignored)
                  (should (= input-depth 3))
                  (should (= output-depth 3)))))))

        (kill-buffer out-buf)))))))

;;; Regression tests for multi-line content

(ert-deftest ascii-tree--multiline-fixed-width ()
  "Multi-line fixed-width blocks should be fully preserved.
Regression test: org-element treats consecutive : lines as one element,
but we must extract all lines, not just the first one."
  (with-temp-buffer
    (org-mode)
    (insert "* Headline\n")
    (insert "    : Line 1\n")
    (insert "    : Line 2\n")
    (insert "    : Line 3\n")
    (insert "    : Line 4\n")
    (let* ((tree (org-element-parse-buffer))
           (out-buf (get-buffer-create " *multiline-test*"))
           (src-buf (current-buffer)))
      (with-current-buffer out-buf (erase-buffer))
      (ascii-tree--walk tree '() out-buf src-buf)
      (let* ((result (with-current-buffer out-buf (buffer-string)))
             (result-lines (split-string (string-trim result) "\n"))
             (content-lines (seq-filter (lambda (l)
                                          (not (string-match-p "^[├└]──" l)))
                                        result-lines)))
        ;; Should have 4 content lines, not just 1
        (should (= (length content-lines) 4))
        (should (string-match-p "Line 1" result))
        (should (string-match-p "Line 2" result))
        (should (string-match-p "Line 3" result))
        (should (string-match-p "Line 4" result)))
      (kill-buffer out-buf))))

(ert-deftest ascii-tree--multiline-comments ()
  "Multi-line comment blocks should be fully preserved.
Regression test: org-element treats consecutive # lines as one element,
but we must extract all lines, not just the first one."
  (with-temp-buffer
    (org-mode)
    (insert "* Headline\n")
    (insert "# Comment 1\n")
    (insert "# Comment 2\n")
    (insert "# Comment 3\n")
    (let* ((tree (org-element-parse-buffer))
           (out-buf (get-buffer-create " *multiline-comment-test*"))
           (src-buf (current-buffer)))
      (with-current-buffer out-buf (erase-buffer))
      (ascii-tree--walk tree '() out-buf src-buf)
      (let* ((result (with-current-buffer out-buf (buffer-string)))
             (result-lines (split-string (string-trim result) "\n")))
        ;; Should have 4 lines: 1 headline + 3 comments
        (should (= (length result-lines) 4))
        (should (string-match-p "Comment 1" result))
        (should (string-match-p "Comment 2" result))
        (should (string-match-p "Comment 3" result)))
      (kill-buffer out-buf))))

(ert-deftest ascii-tree-import-export-multiline-roundtrip ()
  "Round-trip test: multi-line content in tree -> org -> tree.
Tests that multi-line content with # markers is preserved through full cycle."
  (let* ((input "├── api.go # Interface\n│   │   # type Service interface {\n│   │   #   Get() error\n│   │   # }\n")
         (lines (split-string input "\n"))
         (parsed (ascii-tree-import--parse-lines lines))
         (org-output (ascii-tree-import--emit-org parsed)))

    ;; Verify import captured all 3 content lines (Interface is subtitle, not content)
    (should (= (length (nth 3 (car parsed))) 3))

    ;; Export back to tree format
    (with-temp-buffer
      (insert org-output)
      (org-mode)
      (let* ((tree (org-element-parse-buffer))
             (out-buf (get-buffer-create " *roundtrip-multiline*"))
             (src-buf (current-buffer)))
        (with-current-buffer out-buf (erase-buffer))
        (ascii-tree--walk tree '() out-buf src-buf)

        (let* ((result (with-current-buffer out-buf (buffer-string)))
               (input-lines (split-string (string-trim input) "\n"))
               (result-lines (split-string (string-trim result) "\n")))
          ;; Should have same number of lines
          (should (= (length input-lines) (length result-lines)))
          ;; All content should be preserved
          (should (string-match-p "type Service interface" result))
          (should (string-match-p "Get() error" result)))

        (kill-buffer out-buf)))))

;;; ascii-tree-test.el ends here

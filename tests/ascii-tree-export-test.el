;;; ascii-tree-export-test.el --- ERT tests for ascii-tree-export -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ascii-tree-export)

(ert-deftest ascii-tree-export--build-prefix-root ()
             "Level-1 headlines have no prefix."
             (should (equal "" (ascii-tree-export--build-prefix '()))))

(ert-deftest ascii-tree-export--build-prefix-non-last ()
             "Non-last child of root gets ├──."
             (should (equal "├── " (ascii-tree-export--build-prefix '(nil)))))

(ert-deftest ascii-tree-export--build-prefix-last ()
             "Last child of root gets └──."
             (should (equal "└── " (ascii-tree-export--build-prefix '(t)))))

(ert-deftest ascii-tree-export--build-prefix-nested-non-last ()
             "Nested non-last: parent pipe + connector."
             (should (equal "│   ├── " (ascii-tree-export--build-prefix '(nil nil)))))

(ert-deftest ascii-tree-export--build-prefix-nested-last ()
             "Nested last child under non-last parent."
             (should (equal "│   └── " (ascii-tree-export--build-prefix '(nil t)))))

(ert-deftest ascii-tree-export--build-prefix-under-last-parent ()
             "Child under a last-sibling parent uses spaces, not pipe."
             (should (equal "    ├── " (ascii-tree-export--build-prefix '(t nil)))))

(defun ascii-tree-export-test--make-headline (title level)
  "Create a minimal org-element headline node for testing."
  (org-element-create 'headline
                      (list :level level :raw-value title)))

(ert-deftest ascii-tree-export--format-headline-simple ()
             "Headline with no subtitle."
             (let ((hl (ascii-tree-export-test--make-headline "Editor" 2)))
               (should (equal "├── Editor"
                              (ascii-tree-export--format-headline hl '(nil))))))

(ert-deftest ascii-tree-export--format-headline-with-subtitle ()
             "Headline with subtitle separated by ascii-tree-export-subtitle-separator."
             (let ((hl (ascii-tree-export-test--make-headline "Diagnostics -- Lorem ipsum dolor sit amet" 2)))
               (should (equal "├── Diagnostics # Lorem ipsum dolor sit amet"
                              (ascii-tree-export--format-headline hl '(nil))))))

(ert-deftest ascii-tree-export--format-headline-root ()
             "Root headline (empty spine) has no connector."
             (let ((hl (ascii-tree-export-test--make-headline "Basic Keybindings" 1)))
               (should (equal "Basic Keybindings"
                              (ascii-tree-export--format-headline hl '())))))

(ert-deftest ascii-tree-export--format-headline-last-sibling ()
             "Last sibling uses └── connector."
             (let ((hl (ascii-tree-export-test--make-headline "Editor" 2)))
               (should (equal "└── Editor"
                              (ascii-tree-export--format-headline hl '(t))))))

(ert-deftest ascii-tree-export--content-prefix-single ()
             "Non-last level-1: content prefixed with │   ."
             (should (equal "│   " (ascii-tree-export--content-prefix '(nil)))))

(ert-deftest ascii-tree-export--content-prefix-last ()
             "Last level-1: content prefixed with spaces only."
             (should (equal "    " (ascii-tree-export--content-prefix '(t)))))

(ert-deftest ascii-tree-export--content-prefix-nested ()
             "Level-2 non-last under non-last level-1."
             (should (equal "│   │   " (ascii-tree-export--content-prefix '(nil nil)))))

(ert-deftest ascii-tree-export--format-content-paragraph ()
             "Paragraph text is returned as indented lines."
             (with-temp-buffer
               (org-mode)
               (insert "* H\nSome text here.\n")
               (let* ((tree (org-element-parse-buffer))
                      (hl   (org-element-map tree 'headline #'identity nil t))
                      (para (org-element-map hl 'paragraph #'identity nil t))
                      (lines (ascii-tree-export--format-content para '(nil))))
                 (should (member "│   Some text here." lines)))))

(ert-deftest ascii-tree-export--format-content-plain-list ()
             "Plain list items are returned as indented lines."
             (with-temp-buffer
               (org-mode)
               (insert "* H\n- Item one\n- Item two\n")
               (let* ((tree  (org-element-parse-buffer))
                      (hl    (org-element-map tree 'headline #'identity nil t))
                      (plist (org-element-map hl 'plain-list #'identity nil t))
                      (lines (ascii-tree-export--format-content plist '(nil))))
                 (should (cl-some (lambda (l) (string-match-p "Item one" l)) lines))
                 (should (cl-some (lambda (l) (string-match-p "Item two" l)) lines)))))

(ert-deftest ascii-tree-export--walk-integration ()
             "Walk a simple org AST and verify output lines."
             (with-temp-buffer
               (org-mode)
               (insert "#+title: My Doc\n\n* Section\n\nSome text.\n\n** Sub -- tag\n\n- Item\n")
               (let* ((tree   (org-element-parse-buffer))
                      (out    (get-buffer-create " *ascii-tree-export-test-output*")))
                 (with-current-buffer out (erase-buffer))
                 (ascii-tree-export--walk tree '() out)
                 (let ((content (with-current-buffer out (buffer-string))))
                   (should (string-match-p "Section" content))
                   (should (string-match-p "Sub # tag" content))
                   (should (string-match-p "Some text" content))
                   (should (string-match-p "Item" content)))
                 (kill-buffer out))))

(ert-deftest ascii-tree-export-entry-point ()
             "Entry point produces correct output buffer for a simple org file."
             (with-temp-buffer
               (org-mode)
               ;; Simulate a file-visiting buffer
               (setq buffer-file-name "/tmp/test-export.org")
               (insert "* Alpha\n- First item\n** Beta\n- Second item\n")
               (ascii-tree-export)
               (let* ((out-name "*ascii-tree-export-test-export.org*")
                      (out-buf  (get-buffer out-name)))
                 (should out-buf)
                 (let ((content (with-current-buffer out-buf (buffer-string))))
                   (should (string-match-p "Alpha" content))
                   (should (string-match-p "Beta" content))
                   (should (string-match-p "First item" content)))
                 (kill-buffer out-buf))))

;;; Markdown support tests

(ert-deftest ascii-tree-export--md-parse-headings-basic ()
  "Parse a simple markdown buffer into (level title content) triples."
  (with-temp-buffer
    (insert "# Alpha\n## Beta\n### Gamma\n## Delta\n# Epsilon\n")
    (let ((headings (ascii-tree-export--md-parse-headings)))
      (should (equal 5 (length headings)))
      (should (equal 1       (car  (nth 0 headings))))
      (should (equal "Alpha" (cadr (nth 0 headings))))
      (should (equal 2       (car  (nth 1 headings))))
      (should (equal "Beta"  (cadr (nth 1 headings)))))))

(ert-deftest ascii-tree-export--md-parse-headings-captures-content ()
  "Non-heading lines are captured as content of their preceding heading."
  (with-temp-buffer
    (insert "# Title\nSome paragraph text.\n- list item\n## Sub\n")
    (let ((headings (ascii-tree-export--md-parse-headings)))
      (should (equal 2 (length headings)))
      (should (equal "Title" (cadr (nth 0 headings))))
      (should (member "Some paragraph text." (caddr (nth 0 headings))))
      (should (member "- list item"          (caddr (nth 0 headings))))
      (should (equal "Sub"   (cadr (nth 1 headings))))
      (should (null          (caddr (nth 1 headings)))))))

(ert-deftest ascii-tree-export--walk-markdown-tree-shape ()
  "walk-markdown produces correct tree connectors and content lines."
  (let* ((headings '((1 "A"  ("Para under A."))
                     (2 "A1" nil)
                     (2 "A2" ("Item in A2."))
                     (1 "B"  nil)))
         (out (get-buffer-create " *md-walk-test*")))
    (with-current-buffer out (erase-buffer))
    (ascii-tree-export--walk-markdown headings out)
    (let ((content (with-current-buffer out (buffer-string))))
      (should (string-match-p "├── A"         content))
      (should (string-match-p "Para under A"  content))
      (should (string-match-p "├── A1"        content))
      (should (string-match-p "└── A2"        content))
      (should (string-match-p "Item in A2"    content))
      (should (string-match-p "└── B"         content)))
    (kill-buffer out)))

(ert-deftest ascii-tree-export-markdown-entry-point ()
  "Entry point handles a markdown-mode buffer."
  (with-temp-buffer
    (markdown-mode)
    (setq buffer-file-name "/tmp/test-export.md")
    (insert "# Alpha\n## Beta\n### Gamma\n")
    (ascii-tree-export)
    (let* ((out-name "*ascii-tree-export-test-export.md*")
           (out-buf  (get-buffer out-name)))
      (should out-buf)
      (let ((content (with-current-buffer out-buf (buffer-string))))
        (should (string-match-p "Alpha" content))
        (should (string-match-p "Beta"  content))
        (should (string-match-p "Gamma" content)))
      (kill-buffer out-buf))))

;;; ascii-tree-export-test.el ends here

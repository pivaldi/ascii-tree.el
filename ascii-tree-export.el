;;; ascii-tree-export.el --- Export org/markdown buffers as ASCII trees -*- lexical-binding: t; -*-

;; Author: Philippe IVALDI
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (markdown-mode "2.4"))
;; Keywords: org, markdown, outline, tree, ascii
;; URL: https://github.com/pivaldi/ascii-tree-export

;;; Commentary:
;; Export the current org-mode or markdown buffer as a tree-like ASCII view
;; into a dedicated buffer.

;;; Code:

(require 'org-element)
(require 'markdown-mode)

(defgroup ascii-tree-export nil
  "Export org buffers as ASCII trees."
  :group 'org
  :prefix "ascii-tree-export-")

(defcustom ascii-tree-export-subtitle-separator " -- "
  "String separating a headline title from its subtitle.
Headlines of the form \"Title -- Subtitle\" are rendered as
\"├── Title # Subtitle\" in the output tree.
Set to \" | \" to use the legacy pipe convention."
  :type 'string
  :group 'ascii-tree-export)

(defun ascii-tree-export--build-prefix (spine)
  "Build the tree prefix string for a node described by SPINE.
SPINE is a list of booleans, one per level from root to this node.
Each boolean is t if that ancestor (or this node) is the last sibling
at its level, nil otherwise.
An empty spine means a root-level node — no prefix is generated."
  (if (null spine)
      ""
    (let* ((ancestors (butlast spine))
           (is-last   (car (last spine)))
           (indent    (mapconcat (lambda (last-p)
                                   (if last-p "    " "│   "))
                                 ancestors
                                 ""))
           (connector (if is-last "└── " "├── ")))
      (concat indent connector))))

(defun ascii-tree-export--format-headline (hl-node spine)
  "Format HL-NODE as a tree headline string using SPINE.
Splits the headline's raw-value on `ascii-tree-export-subtitle-separator'.
If a subtitle is present, renders as \"PREFIX Title # Subtitle\".
Otherwise renders as \"PREFIX Title\"."
  (let* ((raw    (org-element-property :raw-value hl-node))
         (parts  (split-string raw (regexp-quote ascii-tree-export-subtitle-separator)))
         (title  (string-trim (car parts)))
         (sub    (when (cdr parts) (string-trim (cadr parts))))
         (prefix (ascii-tree-export--build-prefix spine))
         (text   (if sub
                     (concat title " # " sub)
                   title)))
    (concat prefix text)))

(defun ascii-tree-export--content-prefix (spine)
  "Build the indentation prefix for content lines under a headline.
SPINE is the same list used in `ascii-tree-export--build-prefix'.
Unlike headlines, content lines use no connector — just │   or spaces."
  (mapconcat (lambda (last-p) (if last-p "    " "│   "))
             spine
             ""))

(defun ascii-tree-export--format-content (node spine)
  "Format content NODE into a list of indented strings using SPINE.
Dispatches on node type: paragraph, plain-list, src-block, table,
or falls back to `org-element-interpret-data'."
  (let* ((prefix (ascii-tree-export--content-prefix spine))
         (raw    (pcase (org-element-type node)
                   ('src-block
                    (let ((lang  (org-element-property :language node))
                          (value (org-element-property :value node)))
                      (concat "#+begin_src " lang "\n" value "#+end_src\n")))
                   (_
                    (org-element-interpret-data node))))
         (lines  (split-string (string-trim-right raw) "\n")))
    (mapcar (lambda (line) (concat prefix line)) lines)))

(defun ascii-tree-export--walk (node spine buffer)
  "Recursively walk NODE and write ASCII tree lines into BUFFER.
SPINE tracks is-last booleans for ancestors of the current node.
Call with the root parse tree and an empty spine: (ascii-tree-export--walk tree '() buf)"
  (let* ((children  (org-element-contents node))
         (headlines (seq-filter (lambda (c)
                                  (eq (org-element-type c) 'headline))
                                children))
         (sections  (seq-filter (lambda (c)
                                  (eq (org-element-type c) 'section))
                                children))
         (section-content (seq-mapcat #'org-element-contents sections)))
    ;; Write content elements belonging to this node
    (dolist (el section-content)
      (unless (memq (org-element-type el) '(headline planning property-drawer))
        (dolist (line (ascii-tree-export--format-content el spine))
          (with-current-buffer buffer
            (insert line "\n")))))
    ;; Walk child headlines
    (let ((n (length headlines)))
      (seq-do-indexed
       (lambda (hl idx)
         (let* ((is-last  (= idx (1- n)))
                (new-spine (append spine (list is-last)))
                (hl-line   (ascii-tree-export--format-headline hl new-spine)))
           (with-current-buffer buffer
             (insert hl-line "\n"))
           (ascii-tree-export--walk hl new-spine buffer)))
       headlines))))

(defun ascii-tree-export--md-parse-headings ()
  "Return list of (LEVEL TITLE CONTENT-LINES) from current markdown buffer.
CONTENT-LINES is a list of non-empty body lines belonging to that heading,
collected between it and the next heading."
  (let (headings current-level current-title current-content)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (if (string-match "^\\(#+\\)[[:space:]]+\\(.*\\)$" line)
              (progn
                (when current-title
                  (push (list current-level current-title
                               (nreverse current-content))
                        headings))
                (setq current-level   (length (match-string 1 line))
                      current-title   (string-trim (match-string 2 line))
                      current-content nil))
            (when (and current-title
                       (not (string-empty-p (string-trim line))))
              (push (string-trim line) current-content))))
        (forward-line 1)))
    (when current-title
      (push (list current-level current-title (nreverse current-content))
            headings))
    (nreverse headings)))

(defun ascii-tree-export--md-last-p (headings i)
  "Return t if heading at index I in HEADINGS is the last sibling at its level.
A heading is last when the next heading with level <= its own has a
strictly smaller level, or when no such heading exists."
  (let ((level (car (nth i headings)))
        (n     (length headings)))
    (catch 'done
      (let ((j (1+ i)))
        (while (< j n)
          (let ((next-level (car (nth j headings))))
            (when (<= next-level level)
              (throw 'done (< next-level level))))
          (setq j (1+ j))))
      t)))

(defun ascii-tree-export--walk-markdown (headings buffer)
  "Write ASCII tree from flat HEADINGS list into BUFFER.
HEADINGS is a list of (LEVEL TITLE CONTENT-LINES) triples."
  (let ((n     (length headings))
        (stack '()))  ; list of (level . is-last), ancestors in level order
    (dotimes (i n)
      (let* ((h         (nth i headings))
             (level     (car h))
             (title     (cadr h))
             (content   (caddr h))
             (is-last   (ascii-tree-export--md-last-p headings i))
             (ancestors (seq-filter (lambda (e) (< (car e) level)) stack))
             (spine     (append (mapcar #'cdr ancestors) (list is-last)))
             (fake-hl   (org-element-create 'headline (list :raw-value title)))
             (line      (ascii-tree-export--format-headline fake-hl spine))
             (prefix    (ascii-tree-export--content-prefix spine)))
        (setq stack (append ancestors (list (cons level is-last))))
        (with-current-buffer buffer
          (insert line "\n")
          (dolist (cline content)
            (insert prefix cline "\n")))))))

;;;###autoload
(defun ascii-tree-export ()
  "Export current org-mode or markdown buffer as an ASCII tree.
Output goes to *ascii-tree-export-FILENAME* where FILENAME is the
visiting file name, or the buffer name if unsaved."
  (interactive)
  (unless (or (derived-mode-p 'org-mode) (derived-mode-p 'markdown-mode))
    (user-error "ascii-tree-export: current buffer is not in org-mode or markdown-mode"))
  (let* ((source-name (or (and (buffer-file-name)
                               (file-name-nondirectory (buffer-file-name)))
                          (buffer-name)))
         (out-name    (format "*ascii-tree-export-%s*" source-name))
         (out-buf     (get-buffer-create out-name)))
    (with-current-buffer out-buf
      (erase-buffer))
    (cond
     ((derived-mode-p 'org-mode)
      (ascii-tree-export--walk (org-element-parse-buffer) '() out-buf))
     ((derived-mode-p 'markdown-mode)
      (ascii-tree-export--walk-markdown (ascii-tree-export--md-parse-headings) out-buf)))
    (display-buffer out-buf)))

(provide 'ascii-tree-export)
;;; ascii-tree-export.el ends here

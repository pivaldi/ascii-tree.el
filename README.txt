#+title: ascii-tree
#+author: Philippe IVALDI
#+language: en
Export an =org-mode= or =markdown= buffer as an ASCII tree into a dedicated buffer.
├── Example
│   ├── Without Content
│   │   Given this org file:
│   │   #+begin_src org
│   │   * Emacs
│   │   ** Editor -- core editing
│   │   ** Org-mode
│   │   *** Capture
│   │   *** Agenda
│   │   ** Magit
│   │   * Vim
│   │   ** Normal mode
│   │   ** Insert mode
│   │   #+end_src
│   │   Running =M-x ascii-tree= produces:
│   │   #+begin_example
│   │   ├── Emacs
│   │   │   ├── Editor # core editing
│   │   │   ├── Org-mode
│   │   │   │   ├── Capture
│   │   │   │   └── Agenda
│   │   │   └── Magit
│   │   └── Vim
│   │       ├── Normal mode
│   │       └── Insert mode
│   │   #+end_example
│   │   The same command works on a =.md= file…
│   └── With Content
│       See file [[file:README.txt][README.txt]] exported from this README.org…
├── Installation
│   ├── Manual
│   │   Clone or download =ascii-tree.el= and add it to your load path:
│   │   #+begin_src emacs-lisp
│   │   (add-to-list 'load-path "/path/to/ascii-tree")
│   │   (require 'ascii-tree)
│   │   #+end_src
│   ├── package-vc (Emacs 29+)
│   │   #+begin_src emacs-lisp
│   │   (package-vc-install "https://github.com/pivaldi/ascii-tree")
│   │   #+end_src
│   └── use-package + straight
│       #+begin_src emacs-lisp
│       (use-package ascii-tree
│         :straight (:host github :repo "pivaldi/ascii-tree"))
│       #+end_src
├── Usage
│   Open any =.org= or =.md= file and run:
│   #+begin_example
│   M-x ascii-tree
│   #+end_example
│   A buffer named =*ascii-tree-FILENAME*= opens with the tree output.
├── Configuration
│   └── Subtitle separator
│       Headlines of the form =Title -- Subtitle= are rendered as =Title # Subtitle= in
│       the tree. The separator string is configurable:
│       #+begin_src emacs-lisp
│       ;; Default: " -- "
│       (setq ascii-tree-subtitle-separator " -- ")
│
│       ;; Legacy pipe convention
│       (setq ascii-tree-subtitle-separator " | ")
│       #+end_src
├── Supported formats
│   | Format   | Headlines | Paragraphs | Lists | Src blocks | Tables |
│   |----------+-----------+------------+-------+------------+--------|
│   | Org-mode | yes       | yes        | yes   | yes        | yes    |
│   | Markdown | yes       | yes        | yes   | —          | —      |
│   Org-mode content is extracted via =org-element=, giving full structural fidelity.
│   Markdown content is collected as raw text lines between headings (no AST parsing).
├── Requirements
│   - Emacs 29.1+
│   - [[https://jblevins.org/projects/markdown-mode/][markdown-mode]] 2.4+
└── License
    MIT License.

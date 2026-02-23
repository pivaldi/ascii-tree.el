# ascii-tree.el 🌳

Seamlessly convert ASCII directory trees into Markdown or Org-mode outlines, and accurately reconstruct them back into pixel-perfect ASCII trees.

`ascii-tree.el` bridges the gap between **architecture-as-code** and standard terminal outputs. It allows you to take standard `tree` command outputs, document them richly using Markdown or Org-mode, and convert them back into clean ASCII diagrams without losing structural integrity.

![Demo](assets/demo.gif)

## ✨ Features

* **100% Idempotent Round-Tripping:** Converting an ASCII Tree → Markdown/Org → ASCII Tree is completely lossless. No multiplied newlines, no phantom pipes, no shifting indentations.
* **Smart Geometric Rendering:** The reverse parser uses forward-lookahead to perfectly rebuild terminal pipes (`│`, `├──`, `└──`), spacing, and structural blank lines.
* **Extended Syntax Support:** Native support for inline file descriptions, multi-line continuations, and embedded code blocks right inside your ASCII tree.

Refer to the complex example used to test the idempotency round-tripping of this package:
* The tree version: [mock.txt](tests/mock.txt);
* The Org version: [mock.org](tests/mock.org);
* The Markdown version: [mock.md](tests/mock.md);

## 📦 Installation

Since the package is not yet on MELPA, you can install it using `use-package` with `vc` (Emacs 29+) or `straight.el`.

### Using `use-package` and `vc`:

```elisp
(use-package ascii-tree
  :vc (:url "https://github.com/pivaldi/ascii-tree.el" :rev :newest)
  :bind (("C-c t m" . ascii-tree-to-md)
         ("C-c m t" . ascii-tree-from-md)
         ("C-c t o" . ascii-tree-to-org)
         ("C-c o t" . ascii-tree-from-org)))

```

### Manual Installation:

Download `ascii-tree.el`, place it in your `load-path`, and add this to your `init.el`:

```elisp
(add-to-list 'load-path "/path/to/ascii-tree.el")
(require 'ascii-tree)

```

## 🚀 Usage

All commands operate on the **active region** (highlighted text).

1. Select the ASCII tree (or the Markdown/Org block).
2. Run the desired interactive command.
3. The converted output is immediately placed into a new buffer (`*ascii-tree-md*`, `*ascii-tree-org*`, or `*ascii-tree-tree*`).

### Available Commands

| Command | Description | Suggested Binding |
| --- | --- | --- |
| `M-x ascii-tree-to-md` | Converts an ASCII tree to Markdown | `C-c t m` |
| `M-x ascii-tree-from-md` | Converts a Markdown outline back to an ASCII tree | `C-c m t` |
| `M-x ascii-tree-to-org` | Converts an ASCII tree to Org-mode | `C-c t o` |
| `M-x ascii-tree-from-org` | Converts an Org-mode outline back to an ASCII tree | `C-c o t` |

## 📖 Syntax Guide & Examples

`ascii-tree` expects standard `tree` output but adds a few powerful syntax extensions.

### 1. Descriptions & Continuations

Add a `#` after a node to add a description. Lines beneath a node starting with `#` are treated as continuation text.

**Input (ASCII Tree):**

```text
service-manager/
├── src/
│   ├── main.go  # Main entrypoint
│   │   # Wires dependencies
│   │   # Starts the server

```

**Output (Markdown):**

```markdown
# service-manager/

## src/

### main.go -- Main entrypoint
    Wires dependencies
    Starts the server

```

### 2. Embedded Code Blocks

Use `#[code <lang>]` and `#[endcode]` to embed snippets inside your tree.

**Input (ASCII Tree):**

```text
service-manager/
├── api.go  # Public interface
│   #[code go]
│   # type Service interface {
│   #    DoThing() error
│   # }
│   #[endcode]

```

**Output (Org-mode):**

```org
#+title: service-manager/

* api.go -- Public interface
#+BEGIN_SRC go
type Service interface {
   DoThing() error
}
#+END_SRC

```

### 3. Blank Structural Lines

When creating visual breaks in your ASCII tree, use pure pipes (`│`) or empty spaces. The parser maps these directly to blank lines in MD/Org and perfectly redraws the connections when converting back.

## 🛠️ Testing

`ascii-tree.el` includes a robust ERT test suite that strictly enforces idempotency to prevent geometric regressions.

To run the unit tests headlessly via terminal: `./test.sh`

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

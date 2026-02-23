# demo.tape
# Generates a GIF of the ascii-tree.el workflow

Output assets/demo.gif

Set Shell zsh

# Set up the visual style
Require emacs
Require tree
Set FontSize 16
# Set FontFamily "terminus"
Set Width 1200
Set Height 900
Set TypingSpeed 0.08
# Set Theme "Dracula"
Set Padding 20

# 1. Run tree in the terminal
Type "tree -R --noreport service-manager > architecture.md"
Enter
Sleep 1s

# 2. Open it in Emacs
Type "emacs -q -nw -l ascii-tree.el architecture.md"
Enter
Sleep 2s

# 3. Select all and convert to Org
Ctrl+X
Type "h"
Sleep 1s
Alt+x
Sleep 500ms
Type "ascii-tree-to-org"
Enter
Sleep 2.5s

# 4. We are now in Org! Let's add a description and a code block.
Alt+x
Type "beginning-of-buffer"
Enter
Ctrl+S
Type "main.go"
Enter
Sleep 500ms
Type " -- Main entrypoint"
Enter
Type "#+BEGIN_SRC go"
Enter
Type "func main() {"
Enter
Type '  fmt.Println("Hello World")'
Enter
Left 2
Type "}"
Enter
Type "#+END_SRC"
Sleep 500ms
Ctrl+S
Type "README.md"
Ctrl+S
Enter 2
Type "* internal"
Enter
Type "** adapters"
Enter
Type "*** events"
Enter
Type "**** dispatcher.go"
Enter 3
Enter
Type "** application"
Enter
Type "*** dto.go"
Sleep 2s

# 5. Select all and convert back to an ASCII tree
Ctrl+X
Type "h"
Sleep 1s
Alt+x
Sleep 500ms
Type "ascii-tree-from-org"
Enter

# Pause to admire the pixel-perfect generated ASCII tree with comments!
Sleep 5s

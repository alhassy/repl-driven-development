<div align="center">

<h1>  Editor Integrated REPLs for all languages </h1>

<a href="https://melpa.org/#/repl-driven-development"><img alt="MELPA" src="https://img.shields.io/badge/repl--driven--development-Melpa-green?logo=Gnu-Emacs"></img></a>

<a href="https://twitter.com/intent/tweet?text=This looks super neat (•̀ᴗ•́)و::&url=https://github.com/alhassy/repl-driven-development"><img src="https://img.shields.io/twitter/url?url=https://github.com/alhassy/repl-driven-development"></a> <a href="https://github.com/alhassy/repl-driven-development/issues"><img src="https://img.shields.io/badge/contributions-welcome-green?logo=nil"></a>

<a href="https://alhassy.com/"><img src="https://img.shields.io/badge/author-musa_al--hassy-purple?logo=nintendo-3ds"></a> <a href="https://www.buymeacoffee.com/alhassy"><img src="https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee"></a>

This library provides the Emacs built-in <kbd style="">C─x C─e</kbd> behaviour for arbitrary languages, provided they have a REPL shell command.

|                                                                          |
|------------------------------------------------------------------------- |
| **It provides a “send line to REPL process” command, for your language.** |

</div>

<div align="center">

<img src="http://alhassy.com/images/rdd-workflow.png" width=400 height=300 />

![img](rdd.gif)

</div>


#  <a href="javascript:window.scrollTo(0,0)" style="color: black !important; border-bottom: none !important;" class="tooltip" title="Go to the top of the page"> Ξ </a> 

1.  [Motivation](#motivation)
2.  [Mini-Tutorial](#mini-tutorial)
3.  [Installation & Usage Instructions](#orgb9da7df)
4.  [Videos](#videos)
    1.  [REPL Driven Development :: Teaching a JavaScript runtime, incrementally, to be a web server 🍽️ 🔁 🤖](#teaching-a-javascript-runtime-incrementally-to-be-a-web-server-)
5.  [Bye!](#Bye)


# Motivation

Whenever reading/refactoring some code, if you can make some of it self-contained, then you can immediately try it out! No need to load your entire program; nor copy-paste into an external REPL. The benefits of Emacs' built-in <kbd style="">C─x C─e</kbd> for Lisp, and Lisp's Repl Driven Development philosophy, are essentially made possible for arbitrary languages (to some approximate degree, but not fully).

<div align="center">

<img src="http://alhassy.com/images/rdd-benefits.png" width=250 height=250 />

</div>

Just as <kbd style="">C-u C-x C-e</kbd> inserts the resulting expression at the current cursour position, so too all `repl-driven-development` commands allow for a <kbd style="">C─u</kbd> prefix which inserts the result. This allows for a nice scripting experience where results are kept for future use.

Finally, just as <kbd style="">C─h e</kbd> shows you the `*Messages*` buffer where you can see the evaluations of your Emacs Lisp via <kbd style="">C─x C─e</kbd>; likewise, <kbd style="">C─h e</kbd> shows you the output results of any REPL command created by `repl-driven-development`.


# Mini-Tutorial

Often, while reading a README file, we will (1) copy a shell command, (2) open a terminal, and (3) paste the shell command to run it. We can evaluate arbitrary regions in a shell in one step via <kbd style="">C─x C─t</kbd> with:

```emacs-lisp
   (repl-driven-development [C-x C-t] "bash")
```

For example, execute <kbd style="">C─x C─t</kbd> anywhere on each line below and see results in an overlay, right by your cursor.

```shell
  echo "It is $(date) and I am at $PWD, my name is $(whoami) and I have: $(ls)"

  say "My name is $(whoami) and I like Emacs"
```

Notice as each line is sent to the Bash process, the line is highlighted briefly in yellow. Moreover, you can hover over the text to see a tooltip with the resulting shell output. Finally, if you invoke <kbd style="">C-h k C-x C-t</kbd> you get help about this new <kbd style="">C─x C─t</kbd> command, such as inserting results at point via <kbd style="">C-u C-x C-t</kbd> or to reset/refresh the current Bash process with <kbd style="">C-u -1 C-x C-t</kbd>.

This also works for any command-line REPL; for example, for Python:

```emacs-lisp
   (repl-driven-development [C-x C-p] "python3")
```

Then, we can submit the following Python snippets with <kbd style="">C─x C─p</kbd> on each line.

```python
  sum([1, 2, 3, 4])

  list(map(lambda i: 'Fizz'*(not i%3)+'Buzz'*(not i%5) or i, range(1,101)))
```

These work fine, however there are some shortcomings of this REPL. For example, echoing results could be prettier and it doesn't handle multi-line input very well. You can address these issues using the various hooks / keyword arguments of the `repl-driven-development` macro.

However, this package comes with preconfigured REPLS for: `python, terminal, java, javascript`.

Simply use the name of these configurations:

```emacs-lisp
  (repl-driven-development [C-x C-p] python)
```

Now we can submit the following, with <kbd style="">C─x C─p</kbd>, with no issues:

```python
  def square(x):
    return x * x

  square(5)
```

Since these new REPL commands are just Emacs functions, we can use several at the time, alternating between them. For example:

```emacs-lisp
  ;; C-x C-e on the next two lines
  (repl-driven-development [C-x C-t] terminal)
  (repl-driven-development [C-x C-p] python)
```

```shell
  echo Hello... > /tmp/o       # C-x C-t here
```

```python
  print(open("/tmp/o").read()) # C-x C-p here
```

```shell
  echo ...and bye >> /tmp/o    # C-x C-t again
```

```python
  print(open("/tmp/o").read()) # C-x C-p again
```

Let's conclude with a GUI example in Java.

```emacs-lisp
  ;; Set “C-x C-j” to evaluate Java code in a background REPL.
  (repl-driven-development [C-x C-j] "jshell")
```

```java
  // Select this Java snippet, then press “C-x C-j” to evaluate it
  import javax.swing.*;
  JOptionPane.showMessageDialog(new JFrame(){{setAlwaysOnTop(true);}}, "Super nice!")
```

We can use a preconfigured Java REPL, to remove the annoying “jshell>” prompt from overlay echos, handle multi-line input, and more.

```emacs-lisp
  (repl-driven-development [C-x C-j] java)
```

```java
 // REPL result values are shown as overlays:
 // See a list of 23 numbers, which are attached as a tooltip to this text.
 IntStream.range(0, 23).forEach(x -> System.out.println(x))
```

For more documentation, and examples, see <http://alhassy.com/repl-driven-development>


# Installation & Usage Instructions

This package is on Melpa, <a href="https://melpa.org/#/repl-driven-development"><img alt="MELPA" src="https://img.shields.io/badge/repl--driven--development-Melpa-green?logo=Gnu-Emacs"></img></a>, so you can install it with [use-package](https://github.com/alhassy/emacs.d#use-package-the-start-of-initel):

```emacs-lisp
(use-package repl-driven-development
  :ensure t
  :config
  (repl-driven-development [C-x C-j] java)       ;; e“X”ecute “j”ava
  (repl-driven-development [C-x C-n] javascript) ;; e“X”ecute “n”odejs
  (repl-driven-development [C-x C-p] python)     ;; e“X”ecute “p”ython
  (repl-driven-development [C-x C-t] terminal))  ;; e“X”ecute “t”erminal
```

The above mentions the four pre-configured REPLs that the package comes with: These are like their CLI equivalents, but offer more bells and whistles.

-   For example, the pre-configured `java` REPL is like `(repl-driven-development [C-x C-j] "jshell")` but it supports multi-line input: JShell eagerly inserts semicolons onto expressions, so, say, a multi-line Stream pipeline would be interpreted as multiple distinct statements by JShell, whereas our `java` configuration handles this by stripping the newlines (and any intermediary comments).

You can use any process, for example let's use the `tclsh` command line process:

```emacs-lisp
(repl-driven-development [s-t] "tclsh"        ;; Make “⌘-t” e“X”ecute “T”cl code,
                         :blink 'pulsar-blue  ;; and highlight submitted lines blue
                         :prompt "%")         ;; and don't show me the tclsh prompt, which is “%”.
```

The `repl-driven-development` macro has been tried at least with the following processes.

|                                                                        |
|----------------------------------------------------------------------- |
| *Please make a Pull Request, or Issue, to increase the following list!* |

<div><a style="width: 1%;float: left; padding: 0px" id="javascript" href="#javascript">🔗</a> <details class="float-child" style="background-color: nil"><summary> <strong> <font face="Courier" size="3" color="green">JavaScript ---and a minimal server             </font> </strong> </summary>

|                                                                                                |
|----------------------------------------------------------------------------------------------- |
| *🤔 We suggest using the preconfigured `javascript` configuration that ships with this package.* |

<div class="org-center">
<p>
<a href="https://alhassy.github.io/JavaScriptCheatSheet/CheatSheet.pdf"><img src="https://img.shields.io/badge/JavaScript-Colourful%C2%A0PDF%C2%A0CheatSheet-success?logo=javascript"></a>
</p>
</div>

We can set up a JavaScript REPL in the background as follows&#x2026;

```emacs-lisp
   ;; C-x C-j now evaluates arbitrary JavaScript code
   (repl-driven-development [C-x C-j] "node -i")
```

That's it! Press <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-x C-e ∷ eval-last-sexp<br>Evaluate sexp before point; print value in the echo area.<br>Interactively, EVAL-LAST-SEXP-ARG-INTERNAL is the prefix argument.<br>With a non ‘-’ prefix argument, print output into current buffer.<br><br>This commands handles ‘defvar’, ‘defcustom’ and ‘defface’ the<br>same way that ‘eval-defun’ does.&emsp;See the doc string of that<br>function for details.<br><br>Normally, this function truncates long output according to the<br>value of the variables ‘eval-expression-print-length’ and<br>‘eval-expression-print-level’.&emsp;With a prefix argument of zero,<br>however, there is no such truncation.<br>Integer values are printed in several formats (decimal, octal,<br>and hexadecimal).&emsp;When the prefix argument is -1 or the value<br>doesn’t exceed ‘eval-expression-print-maximum-character’, an<br>integer value is also printed as a character of that codepoint.<br><br>If ‘eval-expression-debug-on-error’ is non-nil, which is the default,<br>this command arranges for all errors to enter the debugger.<br><br>This function has :around advice: ‘ad-Advice-eval-last-sexp’.<br><br>(fn EVAL-LAST-SEXP-ARG-INTERNAL)"><kbd style="border-color: red">C-x C-e</kbd></abbr> on the above line so that <kbd style="">C-x C-j</kbd> will now evaluate a selection, or the entire line, as if it were JavaScript code.

-   Why <kbd style="">C-x C-j</kbd>  ?  Well, <kbd style="">C-x C-“e</kbd>” for Emacs Lisp code, and <kbd style="">C-x C-“j</kbd>” for JavaScript code!
-   For instance, copy-paste the following examples into a JavaScript file &#x2014;or just press <kbd style="">C-x C-j</kbd> *in any buffer* to evaluate them!

```javascript
1 + 2                                     // ⮕ 3

1 + '2'                                   // ⮕ '12'

let me = {name: 'Jasim'}; Object.keys(me) // ⮕ ['name']

me.doesNotExist('whoops')                 // ⮕ Uncaught TypeError
```

All of these results are echoed inline in an overlay, by default. Moreover, there is a **REPL** buffer created for your REPL so you can see everything you've sent to it, and the output it sent back. This is particularly useful for lengthy error messages, such as those of Java, which cannot be rendered nicely within an overlay.

How this works is that Emacs spawns a new “node -i” process, then <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-x C-j ∷ dired-jump<br>Jump to Dired buffer corresponding to current buffer.<br>If in a buffer visiting a file, Dired that file’s directory and<br>move to that file’s line in the directory listing.<br><br>If the current buffer isn’t visiting a file, Dired ‘default-directory’.<br><br>If in Dired already, pop up a level and goto old directory’s line.<br>In case the proper Dired file line cannot be found, refresh the dired<br>buffer and try again.<br><br>When OTHER-WINDOW is non-nil, jump to Dired buffer in other window.<br><br>When FILE-NAME is non-nil, jump to its line in Dired.<br>Interactively with prefix argument, read FILE-NAME.<br><br>(fn &optional OTHER-WINDOW FILE-NAME)"><kbd style="border-color: red">C-x C-j</kbd></abbr> sends text to that process. Whenever the process emits any output &#x2014;on stdout or stderr&#x2014; then we emit that to the user via an overlay starting with “⮕”.

Finally, “C-h k C-x C-j” will show you the name of the function that is invoked when you press C-x C-j, along with minimal docs.

A useful example would be a minimal server, and requests for it.

```javascript
// First get stuff with C-x C-e:
// (async-shell-command "npm install -g express axios")

let app = require('express')()
let clicked = 1
app.get('/hi', (req, res) => res.send(`Hello World × ${clicked++}`))

let server = app.listen(3000)
// Now visit   http://localhost:3000/hi   a bunch of times!

// Better yet, see the output programmatically...
let axios = require('axios')
// Press C-x C-j a bunch of times on the following expression ♥‿♥
console.log((await axios.get('http://localhost:3000/hi')).data)

// Consider closing the server when you're done with it.
server.close()
```

Just as “Emacs is a Lisp Machine”, one can use “VSCodeJS” to use “VSCode as a JS Machine”. See <http://alhassy.com/vscode-is-itself-a-javascript-repl>.

</details> </div>

<div><a style="width: 1%;float: left; padding: 0px" id="python" href="#python">🔗</a> <details class="float-child" style="background-color: #add8e6"><summary> <strong> <font face="Courier" size="3" color="green">Python </font> </strong> </summary>

|                                                                                            |
|------------------------------------------------------------------------------------------- |
| *🤔 We suggest using the preconfigured `python` configuration that ships with this package.* |

<div class="org-center">
<p>
<a href="https://alhassy.github.io/PythonCheatSheet/CheatSheet.pdf"><img src="https://img.shields.io/badge/Python-Colourful%C2%A0PDF%C2%A0CheatSheet-success?logo=python"></a>
</p>
</div>

We can set up a Python REPL in the background as follows&#x2026;

```emacs-lisp
    ;; C-x C-p now evaluates arbitrary Python code
    (repl-driven-development [C-x C-p] "python3 -i")
```

Example use&#x2026;

```python
1 + 2             # ⮕ 3

hello = 'world!'  # (No output; this is an effectful operation)

print(hello)      # ⮕ world!

2 + 'hi'          # 🚫 TypeError: unsupported operand type(s) for +
```

Learn more by reading&#x2026; [Python: A Gentle Introduction to Socket Programming](https://cs.lmu.edu/~ray/notes/pythonnetexamples/)

</details> </div>

<div><a style="width: 1%;float: left; padding: 0px" id="java" href="#java">🔗</a> <details class="float-child" style="background-color: nil"><summary> <strong> <font face="Courier" size="3" color="green">Java                                                        </font> </strong> </summary>

|                                                                                          |
|----------------------------------------------------------------------------------------- |
| *🤔 We suggest using the preconfigured `java` configuration that ships with this package.* |

<div class="org-center">
<p>
<a href="https://alhassy.com/java-cheat-sheet.pdf"><img src="https://img.shields.io/badge/Java-Colourful%C2%A0PDF%C2%A0CheatSheet-success?logo=coffeescript"></a>
</p>
</div>

We can set up a Java REPL in the background as follows&#x2026;

```emacs-lisp
(repl-driven-development [C-x C-j] "jshell --enable-preview" :prompt "jshell>")
```

Now, we can select the following and press `C-x C-j` to evaluate the Java code:

```java
// Ensure you're not fullscreen, and you'll see a dialog window appear.
import javax.swing.*;
JOptionPane.showMessageDialog(new JFrame(), "Super nice!");
```

Or doing algebraic datatypes in Java:

```java
sealed interface Maybe {
    record None() implements Maybe {}
    record Just(int x) implements Maybe {}
}

var thisPrettyPrintsNicelyInTheREPL = new Maybe.Just(3);

new Maybe.Just(3).equals(new Maybe.Just(3)) // yay
```

</details> </div>

<div><a style="width: 1%;float: left; padding: 0px" id="ruby" href="#ruby">🔗</a> <details class="float-child" style="background-color: nil"><summary> <strong> <font face="Courier" size="3" color="green">Ruby                                                             </font> </strong> </summary>

<div class="org-center">
<p>
<a href="https://alhassy.github.io/RubyCheatSheet/CheatSheet.pdf"><img src="https://img.shields.io/badge/Ruby-Colourful%C2%A0PDF%C2%A0CheatSheet-success?logo=ruby"></a>
</p>
</div>

We can set up a REPL in the background as follows&#x2026;

```emacs-lisp
   ;; “C-x e r” now “e”valuates arbitrary “r”uby code
   (repl-driven-development [C-x e r] "irb --inf-ruby-mode" :prompt "irb(main):.*>")
```

For example&#x2026;

```ruby
2 + 2

33 + 4

5.times { print "Odelay!" } # ⮕ Odelay! Odelay! Odelay! Odelay! Odelay! 5

['ruby', 'is', 'readable'].map { | food | food.capitalize } # ⮕ ["Ruby", "Is", "Readable"]
```

</details> </div>

<div><a style="width: 1%;float: left; padding: 0px" id="clojure" href="#clojure">🔗</a> <details class="float-child" style="background-color: nil"><summary> <strong> <font face="Courier" size="3" color="green">Clojure                                                  </font> </strong> </summary>

<div class="org-center">
<p>
<a href="https://alhassy.github.io/ClojureCheatSheet/CheatSheet.pdf"><img src="https://img.shields.io/badge/Clojure-Colourful%C2%A0PDF%C2%A0CheatSheet-success?logo=awslambda"></a>
</p>
</div>

We can set up a REPL in the background as follows&#x2026;

```emacs-lisp
   ;; “C-x C-k” now evaluates arbitrary Clojure code
   (repl-driven-development [C-x C-k] "clojure" :prompt "user=>")
```

For example&#x2026;

```clojure
(+ 1 2) ;; ⮕ 3

(defn square [x] (* x x)) ;; ⮕ #'user/square
(square 3) ;; ⮕ 9
```

</details> </div>

<div><a style="width: 1%;float: left; padding: 0px" id="typescript" href="#typescript">🔗</a> <details class="float-child" style="background-color: nil"><summary> <strong> <font face="Courier" size="3" color="green">TypeScript                                            </font> </strong> </summary>

We can set up a REPL in the background as follows&#x2026;

```emacs-lisp
   ;; C-x C-j now evaluates arbitrary JavaScript code
   (repl-driven-development [C-x C-t] "npx ts-node")
```

Then we can use it as follows:

```typescript
22 + 2

"hello"
```

However, the output is ugly since it mentions the `^M` character.

-   Look at the `python` configuration that ships with this package for a starting point on how to address this issue.

</details> </div>

<div><a style="width: 1%;float: left; padding: 0px" id="haskell" href="#haskell">🔗</a> <details class="float-child" style="background-color: #add8e6"><summary> <strong> <font face="Courier" size="3" color="green">Haskell                                          </font> </strong> </summary>

<div class="org-center">
<p>
<a href="https://alhassy.github.io/HaskellCheatSheet/CheatSheet.pdf"><img src="https://img.shields.io/badge/Haskell-Colourful%C2%A0PDF%C2%A0CheatSheet-success?logo=awslambda"></a>
</p>
</div>

We can set up a REPL in the background as follows&#x2026;

```emacs-lisp
   ;; C-x C-h now evaluates arbitrary Haskell code
   (repl-driven-development [C-x C-h] "ghci" :prompt "ghci>")
```

For example&#x2026;

```haskell
-- Sum of the first 100 squares
sum [ x ** 2 | x <- [1..100]] -- ⇒ 338350.0

-- The positive evens at-most 12
[x | x <- [1..12], x `mod` 2 == 0] -- [2,4,6,8,10,12]

-- Define a function...
myLast = head . reverse

-- Then use it...
myLast [1, 2, 3] -- ⇒ 3
```

Note that Haskell has “typed holes” with the syntax `_A`:

```haskell
1 + _A  -- ⇒ Found hole: _A::a; it :: forall {a}. Num a = a
```

Another language with typed holes is Arend&#x2026;

</details> </div>

<div><a style="width: 1%;float: left; padding: 0px" id="arend" href="#arend">🔗</a> <details class="float-child" style="background-color: nil"><summary> <strong> <font face="Courier" size="3" color="green">Arend: Quickly making a terse Emacs interface for a language without one </font> </strong> </summary>

The [Arend Theorem Prover](https://arend-lang.github.io/download#console-application) has an IntelliJ interface (since it's a JetBrains proof assistant), but no Emacs counterpart &#x2014;which may be annoying for Agda/Coq programmers accustomed to Emacs but want to experiment with Arend.

We can set up an Arend REPL in the background as follows&#x2026;

```emacs-lisp
    ;; C-x C-a now evaluates arbitrary Arend code
    (repl-driven-development [C-x C-a]
                             (format "java -jar %s -i"
                                     (f-expand "~/Downloads/Arend.jar")))
```

Then,

```arend
1 Nat.+ 1 -- ⇒ 2
:type 4  -- ⇒ Fin 5

-- Declare a constant
\\func f => 1
:type f -- ⇒ Nat
f -- ⇒ 1

-- Declare a polymorphic identity function, then use it
\\func id {A : \\Type} (a : A) => a
id 12  -- ⇒ 12

-- Arend has “typed holes”
1 Nat.+ {?}  -- ⇒ Nat.+{?}: Goal: Expectedtype: Nat
```

</details> </div>

<div><a style="width: 1%;float: left; padding: 0px" id="purescript" href="#purescript">🔗</a> <details class="float-child" style="background-color: #add8e6"><summary> <strong> <font face="Courier" size="3" color="green">PureScript                                    </font> </strong> </summary>

First `brew install spago`, then we can set up a PureScript REPL in the background as follows&#x2026;

```emacs-lisp
    ;; C-x C-p now evaluates arbitrary PureScript code
    (repl-driven-development [C-x C-p] "spago repl")
```

For example&#x2026;.

```purescript
import Prelude

-- Define a function
add1 = (\x -> x + 1)

-- Use the function
add1 2    -- ⇒ 3

-- Experiment with a typed hole
1 + ?A  -- ⇒ Hole ?A has the inferred type Int
```

</details> </div>

<div><a style="width: 1%;float: left; padding: 0px" id="idris" href="#idris">🔗</a> <details class="float-child" style="background-color: nil"><summary> <strong> <font face="Courier" size="3" color="green">Idris                                                      </font> </strong> </summary>

First `brew install idris2`, then we can set up an Idris REPL in the background as follows&#x2026;

```emacs-lisp
    ;; C-x C-i now evaluates arbitrary Idris code
    (repl-driven-development [C-x C-i] "idris2")
```

Here's some random code&#x2026;

```purescript
-- Like Lisp, Idris uses “the” for type annotations
the Nat 4  -- ⇒ 4 : Nat

with List sum [1,2,3] -- ⇒ 6

-- defining a new type (REPL specific notation)
:let data Foo : Type where Bar : Foo

:t Bar -- ⇒ Foo

-- Experiment with a typed hole [Same notation as Haskell]
1 + ?A -- prim__add_Integer 1 ?A
```

</details> </div>

<div><a style="width: 1%;float: left; padding: 0px" id="racket" href="#racket">🔗</a> <details class="float-child" style="background-color: #add8e6"><summary> <strong> <font face="Courier" size="3" color="green">Racket                                            </font> </strong> </summary>

|                                                                   |
|------------------------------------------------------------------ |
| Racket is a modern programming language in the Lisp/Scheme family. |

First `brew install --cask racket`, then we can set up an Racket REPL in the background as follows&#x2026;

```emacs-lisp
    ;; C-x C-i now evaluates arbitrary Racket code
    (repl-driven-development [C-x C-r] "racket -I slideshow")
```

Here's some random code&#x2026;

```racket
(define (series mk) (hc-append 4 (mk 5) (mk 10) (mk 20)))

;; Shows 3 circles of increasing radius, in an external window
(show-pict (series circle))
```

Meeting Racket for the first time is probably best done with *DrRacket*.

</details> </div>

<div><a style="width: 1%;float: left; padding: 0px" id="common" href="#common">🔗</a> <details class="float-child" style="background-color: lisp"><summary> <strong> <font face="Courier" size="3" color="green">Common Lisp                                          </font> </strong> </summary>

We can set up a REPL in the background as follows&#x2026;

```emacs-lisp
   ;; “C-x e p” now “e”valuates arbitrary “c”ommon-lisp code
   (repl-driven-development [C-x e c] "sbcl" :prompt "\\*")
```

For example&#x2026;

```common-lisp
(print "hello world")
```

</details> </div>

<div><a style="width: 1%;float: left; padding: 0px" id="perl" href="#perl">🔗</a> <details class="float-child" style="background-color: nil"><summary> <strong> <font face="Courier" size="3" color="green">Perl                                                        </font> </strong> </summary>

We can set up a REPL in the background as follows&#x2026;

```emacs-lisp
   ;; “C-x e p” now “e”valuates arbitrary “p”erl code
   (repl-driven-development [C-x e p] "perl -de0")
```

For example&#x2026;

```perl
print(1..5)
```

</details> </div>

<div><a style="width: 1%;float: left; padding: 0px" id="julia" href="#julia">🔗</a> <details class="float-child" style="background-color: nil"><summary> <strong> <font face="Courier" size="3" color="green">Julia                                                      </font> </strong> </summary>

We can set up a REPL in the background as follows&#x2026;

```emacs-lisp
   ;; “C-x e j” now “e”valuates arbitrary “j”uila code
   (repl-driven-development [C-x e j] "julia" :prompt "julia>")
```

For example&#x2026; Let's get a random 2×2 matrix&#x2026;

```julia
rand(2, 2)
```

</details> </div>

<div><a style="width: 1%;float: left; padding: 0px" id="smalltalk" href="#smalltalk">🔗</a> <details class="float-child" style="background-color: nil"><summary> <strong> <font face="Courier" size="3" color="green">GNU Smalltalk                                          </font> </strong> </summary>

We can set up a REPL in the background as follows&#x2026;

```emacs-lisp
   ;; “C-x e j” now “e”valuates arbitrary “s”malltalk code
   (repl-driven-development [C-x e s] "gst" :prompt "st")
```

For example&#x2026;

```smalltalk
'Hello World!' printNl !
```

</details> </div>

<div><a style="width: 1%;float: left; padding: 0px" id="tcl" href="#tcl">🔗</a> <details class="float-child" style="background-color: nil"><summary> <strong> <font face="Courier" size="3" color="green">Tcl                                                          </font> </strong> </summary>

We can set up a REPL in the background as follows&#x2026;

```emacs-lisp
   ;; “C-x e t” now “e”valuates arbitrary “t”cl code
   (repl-driven-development [C-x e t] "tclsh" :prompt "%")
```

For example&#x2026;

```tcl
set a 1
set b 2
puts $a$b[expr 2 + 3]{bye}
```

</details> </div>

<div><a style="width: 1%;float: left; padding: 0px" id="lua" href="#lua">🔗</a> <details class="float-child" style="background-color: nil"><summary> <strong> <font face="Courier" size="3" color="green">Lua                                                          </font> </strong> </summary>

We can set up a REPL in the background as follows&#x2026;

```emacs-lisp
   ;; “C-x e l” now “e”valuates arbitrary “l”ua code
   (repl-driven-development [C-x e l] "lua")
```

For example&#x2026;

```lua
print("Hello, world")
```

</details> </div>


# Videos


## REPL Driven Development :: Teaching a JavaScript runtime, incrementally, to be a web server 🍽️ 🔁 🤖

<div align="center">

<img src="http://alhassy.com/images/rdd-teaching-a-js-runtime-to-be-a-webserver.png" width=400 height=300 />

<a href="https://www.youtube.com/watch?v=b6Z3NQVn4lY"><img src="https://img.youtube.com/vi/b6Z3NQVn4lY/0.jpg" /></a>

</div>


# Bye!

<img src="https://img.shields.io/badge/thanks-for_reading-nil?logo=nil">

<a href="https://twitter.com/intent/tweet?text=This looks super neat (•̀ᴗ•́)و::&url=https://github.com/alhassy/repl-driven-development"><img src="https://img.shields.io/twitter/url?url=https://github.com/alhassy/repl-driven-development"></a>

<a href="https://www.buymeacoffee.com/alhassy"><img src="https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee"></a>
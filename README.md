<div align="center">

<h1>  Editor Integrated REPLs for all languages </h1>

<a href="https://melpa.org/#/repl-driven-development"><img alt="MELPA" src="https://img.shields.io/badge/repl--driven--development-1.0.8-green?logo=Gnu-Emacs"></img></a>

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


# Table of Contents

1.  [Motivation](#motivation)
2.  [Official Manual](#official-manual)
3.  [Mini-Tutorial](#mini-tutorial)
4.  [Videos](#videos)
    1.  [REPL Driven Development :: Teaching a JavaScript runtime, incrementally, to be a web server 🍽️ 🔁 🤖](#teaching-a-javascript-runtime-incrementally-to-be-a-web-server-)


# Motivation

Whenever reading/refactoring some code, if you can make some of it self-contained, then you can immediately try it out! No need to load your entire program; nor copy-paste into an external REPL. The benefits of Emacs' built-in <kbd style="">C─x C─e</kbd> for Lisp, and Lisp's Repl Driven Development philosophy, are essentially made possible for arbitrary languages (to some approximate degree, but not fully).

<div align="center">

<img src="http://alhassy.com/images/rdd-benefits.png" width=250 height=250 />

</div>

Just as <kbd style="">C-u C-x C-e</kbd> inserts the resulting expression at the current cursour position, so too all `repl-driven-development` commands allow for a <kbd style="">C─u</kbd> prefix which inserts the result. This allows for a nice scripting experience where results are kept for future use.

Finally, just as <kbd style="">C─h e</kbd> shows you the `*Messages*` buffer where you can see the evaluations of your Emacs Lisp via <kbd style="">C─x C─e</kbd>; likewise, <kbd style="">C─h e</kbd> shows you the output results of any REPL command created by `repl-driven-development`.


# Official Manual

See <http://alhassy.com/repl-driven-development>

<kbd style="">C-h o repl-driven-development</kbd> also has extensive docs, via a JavaScript server example.


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


# Videos


## REPL Driven Development :: Teaching a JavaScript runtime, incrementally, to be a web server 🍽️ 🔁 🤖

<div align="center">

<img src="http://alhassy.com/images/rdd-teaching-a-js-runtime-to-be-a-webserver.png" width=400 height=300 />

<a href="https://www.youtube.com/watch?v=b6Z3NQVn4lY"><img src="https://img.youtube.com/vi/b6Z3NQVn4lY/0.jpg" /></a>

</div>
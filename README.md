
<div align="center">

<h1>  Editor Integrated REPLs for all languages </h1>

<a href="https://melpa.org/#/repl-driven-development"><img alt="MELPA" src="https://img.shields.io/badge/repl--driven--development-Melpa-green?logo=Gnu-Emacs"></img></a>

<a href="https://twitter.com/intent/tweet?text=This looks super neat (•̀ᴗ•́)و::&url=https://github.com/alhassy/repl-driven-development"><img src="https://img.shields.io/twitter/url?url=https://github.com/alhassy/repl-driven-development"></a>
<a href="https://github.com/alhassy/repl-driven-development/issues"><img src="https://img.shields.io/badge/contributions-welcome-green?logo=nil"></a>

<a href="https://alhassy.github.io/"><img src="https://img.shields.io/badge/author-musa_al--hassy-purple?logo=nintendo-3ds"></a>
<a href="https://www.buymeacoffee.com/alhassy"><img src="https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee"></a>

This library provides the Emacs built-in <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-x C-e ∷ eval-last-sexp<br>Evaluate sexp before point; print value in the echo area.<br>Interactively, EVAL-LAST-SEXP-ARG-INTERNAL is the prefix argument.<br>With a non ‘-’ prefix argument, print output into current buffer.<br><br>This commands handles ‘defvar’, ‘defcustom’ and ‘defface’ the<br>same way that ‘eval-defun’ does.&emsp;See the doc string of that<br>function for details.<br><br>Normally, this function truncates long output according to the<br>value of the variables ‘eval-expression-print-length’ and<br>‘eval-expression-print-level’.&emsp;With a prefix argument of zero,<br>however, there is no such truncation.<br>Integer values are printed in several formats (decimal, octal,<br>and hexadecimal).&emsp;When the prefix argument is -1 or the value<br>doesn’t exceed ‘eval-expression-print-maximum-character’, an<br>integer value is also printed as a character of that codepoint.<br><br>If ‘eval-expression-debug-on-error’ is non-nil, which is the default,<br>this command arranges for all errors to enter the debugger.<br><br>This function has :around advice: ‘ad-Advice-eval-last-sexp’.<br><br>(fn EVAL-LAST-SEXP-ARG-INTERNAL)"><kbd style="border-color: red">C-x C-e</kbd></abbr> behaviour for
arbitrary languages, provided they have a REPL shell command.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left"><b>It provides a “send line to REPL process” command, for your language.</b></td>
</tr>
</tbody>
</table>

</div>

![img](images/short_example.png)


# Table of Contents

1.  [Motivation](#motivation)
2.  [Official Manual](#official-manual)
3.  [Mini-Tutorial](#mini-tutorial)


# Motivation

Whenever reading/refactoring some code, if you can make some of it
self-contained, then you can immediately try it out! No need to
load your entire program; nor copy-paste into an external REPL.  The
benefits of Emacs' built-in <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-x C-e ∷ eval-last-sexp<br>Evaluate sexp before point; print value in the echo area.<br>Interactively, EVAL-LAST-SEXP-ARG-INTERNAL is the prefix argument.<br>With a non ‘-’ prefix argument, print output into current buffer.<br><br>This commands handles ‘defvar’, ‘defcustom’ and ‘defface’ the<br>same way that ‘eval-defun’ does.&emsp;See the doc string of that<br>function for details.<br><br>Normally, this function truncates long output according to the<br>value of the variables ‘eval-expression-print-length’ and<br>‘eval-expression-print-level’.&emsp;With a prefix argument of zero,<br>however, there is no such truncation.<br>Integer values are printed in several formats (decimal, octal,<br>and hexadecimal).&emsp;When the prefix argument is -1 or the value<br>doesn’t exceed ‘eval-expression-print-maximum-character’, an<br>integer value is also printed as a character of that codepoint.<br><br>If ‘eval-expression-debug-on-error’ is non-nil, which is the default,<br>this command arranges for all errors to enter the debugger.<br><br>This function has :around advice: ‘ad-Advice-eval-last-sexp’.<br><br>(fn EVAL-LAST-SEXP-ARG-INTERNAL)"><kbd style="border-color: red">C-x C-e</kbd></abbr> for Lisp, and Lisp's Repl
Driven Development philosophy, are essentially made possible for
arbitrary languages (to some approximate degree, but not fully).

Just as <kbd style="">C-u C-x C-e</kbd> inserts the resulting expression at the
current cursour position, so too all `repl-driven-development`
commands allow for a <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-u ∷ universal-argument<br>Begin a numeric argument for the following command.<br>Digits or minus sign following C-u make up the numeric argument.<br>C-u following the digits or minus sign ends the argument.<br>C-u without digits or minus sign provides 4 as argument.<br>Repeating C-u without digits or minus sign<br> multiplies the argument by 4 each time.<br>For some commands, just C-u by itself serves as a flag<br>that is different in effect from any particular numeric argument.<br>These commands include C-SPC and M-x start-kbd-macro."><kbd style="border-color: red">C-u</kbd></abbr> prefix which inserts the result.
This allows for a nice scripting experience where results
are kept for future use.

Finally, just as <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-h e ∷ view-echo-area-messages<br>View the log of recent echo-area messages: the ‘<strong>Messages</strong>’ buffer.<br>The number of messages retained in that buffer is specified by<br>the variable ‘message-log-max’."><kbd style="border-color: red">C-h e</kbd></abbr> shows you the `*Messages*` buffer
where you can see the evaluations of your Emacs Lisp via
<abbr class="tooltip" style="border: none; text-decoration: none;" title="C-x C-e ∷ eval-last-sexp<br>Evaluate sexp before point; print value in the echo area.<br>Interactively, EVAL-LAST-SEXP-ARG-INTERNAL is the prefix argument.<br>With a non ‘-’ prefix argument, print output into current buffer.<br><br>This commands handles ‘defvar’, ‘defcustom’ and ‘defface’ the<br>same way that ‘eval-defun’ does.&emsp;See the doc string of that<br>function for details.<br><br>Normally, this function truncates long output according to the<br>value of the variables ‘eval-expression-print-length’ and<br>‘eval-expression-print-level’.&emsp;With a prefix argument of zero,<br>however, there is no such truncation.<br>Integer values are printed in several formats (decimal, octal,<br>and hexadecimal).&emsp;When the prefix argument is -1 or the value<br>doesn’t exceed ‘eval-expression-print-maximum-character’, an<br>integer value is also printed as a character of that codepoint.<br><br>If ‘eval-expression-debug-on-error’ is non-nil, which is the default,<br>this command arranges for all errors to enter the debugger.<br><br>This function has :around advice: ‘ad-Advice-eval-last-sexp’.<br><br>(fn EVAL-LAST-SEXP-ARG-INTERNAL)"><kbd style="border-color: red">C-x C-e</kbd></abbr>; likewise, <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-h e ∷ view-echo-area-messages<br>View the log of recent echo-area messages: the ‘<strong>Messages</strong>’ buffer.<br>The number of messages retained in that buffer is specified by<br>the variable ‘message-log-max’."><kbd style="border-color: red">C-h e</kbd></abbr> shows you the output results
of any REPL command created by  `repl-driven-development`.


# Official Manual

See <http://alhassy.com/repl-driven-development>

<kbd style="">C-h o repl-driven-development</kbd> also has extensive docs,
via a JavaScript server example.


# Mini-Tutorial

Often, while reading a README file, we will (1) copy a shell command, (2) open a
terminal, and (3) paste the shell command to run it.  We can evaluate arbitrary
regions in a shell in one step via <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-x C-t ∷ transpose-lines<br>Exchange current line and previous line, leaving point after both.<br>With argument ARG, takes previous line and moves it past ARG lines.<br>With argument 0, interchanges line point is in with line mark is in.<br><br>(fn ARG)"><kbd style="border-color: red">C-x C-t</kbd></abbr> with:

    (repl-driven-development [C-x C-t] "bash")

For example, execute <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-x C-t ∷ transpose-lines<br>Exchange current line and previous line, leaving point after both.<br>With argument ARG, takes previous line and moves it past ARG lines.<br>With argument 0, interchanges line point is in with line mark is in.<br><br>(fn ARG)"><kbd style="border-color: red">C-x C-t</kbd></abbr> anywhere on each line below and see results in an
overlay, right by your cursor.

    echo "It is $(date) and I am at $PWD, my name is $(whoami) and I have: $(ls)"
    
    say "My name is $(whoami) and I like Emacs"

Notice as each line is sent to the Bash process, the line is highlighted briefly
in yellow.  Moreover, you can hover over the text to see a tooltip with the
resulting shell output.  Finally, if you invoke <kbd style="">C-h k C-x C-t</kbd> you get help
about this new <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-x C-t ∷ transpose-lines<br>Exchange current line and previous line, leaving point after both.<br>With argument ARG, takes previous line and moves it past ARG lines.<br>With argument 0, interchanges line point is in with line mark is in.<br><br>(fn ARG)"><kbd style="border-color: red">C-x C-t</kbd></abbr> command, such as inserting results at point via
<kbd style="">C-u C-x C-t</kbd> or to reset/refresh the current Bash process with <kbd style="">C-u -1 C-x C-t</kbd>.

This also works for any command-line REPL; for example, for Python:

    (repl-driven-development [C-x C-p] "python3")

Then, we can submit the following Python snippets with <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-x C-p ∷ mark-page<br>Put mark at end of page, point at beginning.<br>A numeric arg specifies to move forward or backward by that many pages,<br>thus marking a page other than the one point was originally in.<br><br>(fn &optional ARG)"><kbd style="border-color: red">C-x C-p</kbd></abbr> on each line.

    sum([1, 2, 3, 4])
    
    list(map(lambda i: 'Fizz'*(not i%3)+'Buzz'*(not i%5) or i, range(1,101)))

These work fine, however there are some shortcomings of this REPL.
For example, echoing results could be prettier and it doesn't handle
multi-line input very well.  You can address these issues using the various
hooks / keyword arguments of the `repl-driven-development` macro.

However, this package comes with preconfigured REPLS for: `python, terminal, java, javascript`.

Simply use the name of these configurations:

    (repl-driven-development [C-x C-p] python)

Now we can submit the following, with <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-x C-p ∷ mark-page<br>Put mark at end of page, point at beginning.<br>A numeric arg specifies to move forward or backward by that many pages,<br>thus marking a page other than the one point was originally in.<br><br>(fn &optional ARG)"><kbd style="border-color: red">C-x C-p</kbd></abbr>, with no issues:

    def square(x):
      return x * x
    
    square(5)

Since these new REPL commands are just Emacs functions, we can use
several at the time, alternating between them.  For example:

    ;; C-x C-e on the next two lines
    (repl-driven-development [C-x C-t] terminal)
    (repl-driven-development [C-x C-p] python)

    echo Hello... > /tmp/o       # C-x C-t here

    print(open("/tmp/o").read()) # C-x C-p here

    echo ...and bye >> /tmp/o    # C-x C-t again

    print(open("/tmp/o").read()) # C-x C-p again

Let's conclude with a GUI example in Java.

    ;; Set “C-x C-j” to evaluate Java code in a background REPL.
    (repl-driven-development [C-x C-j] "jshell")

    // Select this Java snippet, then press “C-x C-j” to evaluate it
    import javax.swing.*;
    JOptionPane.showMessageDialog(new JFrame(){{setAlwaysOnTop(true);}}, "Super nice!")

We can use a preconfigured Java REPL, to remove the annoying “jshell>” prompt
from overlay echos, handle multi-line input, and more.

    (repl-driven-development [C-x C-j] java)

    // REPL result values are shown as overlays:
    // See a list of 23 numbers, which are attached as a tooltip to this text.
    IntStream.range(0, 23).forEach(x -> System.out.println(x))

For more documentation, and examples,
see <http://alhassy.com/repl-driven-development>


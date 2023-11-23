;;; repl-driven-development.el --- Send arbitrary code to a REPL in the background

;; Copyright (c) 2023 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 1.0.13
;; Package-Requires: ((s "1.12.0") (f "0.20.0") (lf "1.0") (dash "2.16.0") (eros "0.1.0") (bind-key "2.4.1") (emacs "29") (f "0.20.0") (devdocs "0.5") (pulsar "1.0.1") (peg "1.0.1") (hierarchy "0.6.0") (json-navigator "0.1.1"))
;; Keywords: repl-driven-development, rdd, repl, lisp, eval, java, python, ruby, programming, convenience
;; Repo: https://github.com/alhassy/repl-driven-development
;; Homepage: http://alhassy.com/repl-driven-development

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides the Emacs built-in “C-x C-e” behaviour for
;; arbitrary languages, provided they have a REPL shell command.
;;
;; ============================Benefits===============================
;;
;; Whenever reading/refactoring some code, if you can make some of it
;; self-contained, then you can immediately try it out! No need to
;; load your entire program; nor copy-paste into an external REPL.  The
;; benefits of Emacs' built-in “C-x C-e” for Lisp, and Lisp's Repl
;; Driven Development philosophy, are essentially made possible for
;; arbitrary languages (to some approximate degree, but not fully).
;;
;; Just as “C-u C-x C-e” inserts the resulting expression at the
;; current cursour position, so too all repl-driven-development
;; commands allow for a C-u prefix which inserts the result.
;; This allows for a nice scripting experience where results
;; are kept for future use.
;;
;;
;; ===============================Official Manual========================
;;
;; See http://alhassy.com/repl-driven-development
;;
;; “C-h o repl-driven-development” also has extensive docs,
;; via a JavaScript server example.
;;
;; ===============================Mini-Tutorial==========================
;;
;; Often, while reading a README file, we will (1) copy a shell command,
;; (2) open a terminal, and (3) paste the shell command to run it.
;; We can evaluate arbitrary regions in a shell in one step via “C-x C-t”
;; with:
;;
;;    (repl-driven-development [C-x C-t] "bash")
;;
;; For example, execute “C-x C-t” anywhere on each line below and see results in an
;; overlay, right by your cursor.
;;
;;   echo "It is $(date) and I am at $PWD, my name is $(whoami) and I have: $(ls)"
;;
;;   say "My name is $(whoami) and I like Emacs"
;;
;; Notice as each line is sent to the Bash process, the line is highlighted briefly in yellow.
;; Moreover, you can hover over the text to see a tooltip with the resulting shell output.
;; Finally, if you invoke “C-h k C-x C-t” you get help about this new “C-x C-t” command,
;; such as inserting results at point via “C-u C-x C-t” or to reset/refresh the current
;; Bash process with “C-u -1 C-x C-t”.
;;
;; This also works for any command-line REPL; for example, for Python:
;;
;;    (repl-driven-development [C-x C-p] "python3")
;;
;; Then, we can submit the following Python snippets with “C-x C-p” on each line.
;;
;;   sum([1, 2, 3, 4])
;;
;;   list(map(lambda i: 'Fizz'*(not i%3)+'Buzz'*(not i%5) or i, range(1,101)))
;;
;; These work fine, however there are some shortcomings of this REPL.
;; For example, echoing results could be prettier and it doesn't handle
;; multi-line input very well.  You can address these issues using the various
;; hooks / keyword arguments of the “repl-driven-development” macro.
;;
;; However, this package comes with preconfigured REPLS for: python, terminal,
;; java, javascript.
;;
;; Simply use the name of these configurations:
;;
;;   (repl-driven-development [C-x C-p] python)
;;
;; Now we can submit the following, with “C-x C-p”, with no issues:
;;
;;   def square(x):
;;     return x * x
;;
;;   square(5)
;;
;; Since these new REPL commands are just Emacs functions, we can use
;; several at the time, alternating between them.  For example:
;;
;;   ;; C-x C-e on the next two lines
;;   (repl-driven-development [C-x C-t] terminal)
;;   (repl-driven-development [C-x C-p] python)
;;
;;   echo Hello... > /tmp/o       # C-x C-t here
;;
;;   print(open("/tmp/o").read()) # C-x C-p here
;;
;;   echo ...and bye >> /tmp/o    # C-x C-t again
;;
;;   print(open("/tmp/o").read()) # C-x C-p again
;;
;; Let's conclude with a GUI example in Java.
;;
;;   ;; Set “C-x C-j” to evaluate Java code in a background REPL.
;;   (repl-driven-development [C-x C-j] "jshell")
;;
;;   // Select this Java snippet, then press “C-x C-j” to evaluate it
;;   import javax.swing.*;
;;   JOptionPane.showMessageDialog(new JFrame(){{setAlwaysOnTop(true);}}, "Super nice!")
;;
;; We can use a preconfigured Java REPL, to remove the annoying “jshell>” prompt
;; from overlay echos, handle multi-line input, and more.
;;
;;   (repl-driven-development [C-x C-j] java)
;;
;;  // REPL result values are shown as overlays:
;;  // See a list of 23 numbers, which are attached as a tooltip to this text.
;;  IntStream.range(0, 23).forEach(x -> System.out.println(x))
;;
;; For more documentation, and examples,
;; see http://alhassy.com/repl-driven-development
;;
;;; Code:

;; TODO[Low Priority]: Implement pretty printing for Python.
;; TODO[Low Priority]: Implement a simple Read Protocol for JS. (eg JSON.parse)
;; TODO: terminal-eval-insert-last-output,
;; terminal-eval-copy-last-output-to-clipboard,
;; terminal-eval-show-last-input-and-output
;; [An Org mode buffer with the last-input and last output, headings]
;; TODO: Add precondition checks to each method.
;; (cl-assert (symbolp repl))
;; (cl-assert (stringp (rdd@ repl input)))
;; TODO: Make eval-last-sexp, but allow rdd macro to consume key :backward-sexp.
;; For example, in Java backward-sexp is not that helpful, whereas
;; M-a in java-mode is bound to the useful c-beginning-of-statement.
;; TODO: Make *-eval-expression

;;; requires and package preamble

(require 's)               ;; “The long lost Emacs string manipulation library”
(require 'f)
(require 'dash)            ;; “A modern list library for Emacs”
(require 'cl-lib)          ;; New Common Lisp library; ‘cl-???’ forms.
(require 'eros)            ;; Simple Emacs Overlays
(require 'bind-key)        ;; Bind keys
(require 'lf)              ;; Template strings with lf-string
;; [lf requires nil lexical binding!]
(require 'peg)              ;; Parsing expression grammars
(require 'json)
(require 'seq)
(require 'devdocs)

(defconst repl-driven-development-version (package-get-version))
(defun repl-driven-development-version ()
  "Print the current `repl-driven-development' version in the minibuffer."
  (interactive)
  (message repl-driven-development-version))

(defmacro rdd@ (name property)
  "Get/set PROPERTY under namespace NAME.

      Usage:
      (rdd@ \"foo\" name)                ;; ⇒ nil
      (setf (rdd@ \"foo\" name) 'Jasim)
      (rdd@ \"foo\" name)                ;; ⇒ 'Jasim"
  `(get (intern (format "%s" ,name)) (quote ,property)))

;;; defvars

(defvar repl-driven-development-echo-duration 5
  "Amount of seconds to show the result overlay.")

(defvar repl-driven-development-echo-output-in-modeline nil
  "In addition to the overlays, should REPL output be emitted in the modeline?

You can always use `C-h e' to see output in the *Messages* buffer.")

;;; main entry point

;;;###autoload
(cl-defmacro repl-driven-development
    (keys cli
          &key (prompt ">") docs (init "") (blink ''pulsar-yellow) name
          (input-rewrite-fn ''identity) (echo-rewrite-fn ''identity))
  "Make Emacs itself a REPL for your given language of choice.

  Suppose you're exploring a Python/Ruby/Java/JS/TS/Haskell/Lisps/etc
  API, or experimenting with an idea and want immediate feedback.
  You could open a terminal and try things out there; with no editor
  support, and occasionally copy-pasting things back into your editor
  for future use.  Better yet, why not use your editor itself as a REPL.

  Implementation & behavioural notes can be found in the JavaScript
  Example below.

  ######################################################################
  ### JavaScript Example ---Basic usage, and a minimal server ##########
  ######################################################################

     ;; C-x C-j now evaluates arbitrary JavaScript code
     (repl-driven-development [C-x C-j] \"node\")

  That's it! Press “C-x C-e” on the above line so that “C-x C-j”
  will now evaluate a selection, or the entire line, as if it were
  JavaScript code.  ⟦Why C-x C-j? C-x C-“e” for Emacs Lisp code, and C-x
  C-“j” for JavaScript code!⟧ For instance, copy-paste the
  following examples into a JS file ---or just press “C-x C-j” to
  evaluate them!

      1 + 2                                     // ⮕ 3
      1 + '2'                                   // ⮕ '12'
      let me = {name: 'Jasim'}; Object.keys(me) // ⮕ ['name']
      me.doesNotExist('whoops')                 // ⮕ Uncaught TypeError
      [ ...Array(45).keys() ]          // ⮕ Multi-line overlay of 0..44

  All of these results are echoed inline in an overlay, by default.
  Moreover, there is a *REPL* buffer created for your REPL so you
  can see everything you've sent to it, and the output it sent
  back.  This is particularly useful for lengthy error messages,
  such as those of Java, which cannot be rendered nicely within an
  overlay.

  How this works is that Emacs spawns a new “node” process, then
  C-x C-j sends text to that process.  Whenever the process emits
  any output ---on stdout or stderr--- then we emit that to the
  user via an overlay.

  Finally, “C-h k  C-x C-j” will show you the name of the function
  that is invoked when you press C-x C-j, along with minimal docs.

  A useful example would be a minimal server, and requests for it.

     // First get stuff with C-x C-e:
     // (async-shell-command \"npm install -g express axios\")

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

  Just as “Emacs is a Lisp Machine”, one can use “VSCodeJS” to use
  “VSCode as a JS Machine”.
  See http://alhassy.com/vscode-is-itself-a-javascript-repl.

  ######################################################################
  ### Description of Arguments #########################################
  ######################################################################

  - KEYS [Vector]: A vector such as [C-x C-p] that declares the keybindings for
    the new REPL evaluator.

  - CLI [String|Symbol]: A string denoting the terminal command to start your
    repl; you may need an “-i” flag to force it to be interactive even though
    we use it from a child process rather than a top-level shell.

    This argument may also be one of the following unquoted symbols:

        java, python, terminal, javascript

    These are preconfigured REPLs; e.g., see the docs of
    function `repl-driven-development-preconfiguration:python'.

  - PROMPT [Regular Expression]:
    What is the prompt that your REPL shows, e.g., “>”.
    We try to ignore showing it in an overlay that would otherwise hide
    useful output.

  - DOCS [String]: A space-seperated string denoting a list of language
    documents you'd like to associate with your repl.
    Invoking your repl with “C-u C-u” will show the documentation
    of the word at point.  This is done using `devdocs'.

    For example,
      (repl-driven-development [C-x C-j] \"node\" :docs \"javascript express\")
    Would allow us to invoke “C-u C-u C-x C-j” with the cursor on the
    word, say, “listen” and we'll see some useful docs (along with
    example uses) of this Express library method “listen”.

    Visit https://devdocs.io/ to see the list of documented languages
    and libraries.

  - INIT [String | List<String>]: Any initial code you'd like your
    repl to be initiated with. For example, imports of standard libraries
    is probably something you'd always like to have on-hand; or perhaps
    some useful variables/declarations/functions.

  - BLINK [Face]: Any face with a background. It is used to briefly highlight
    the current line that is being sent to the REPL process.

  - INPUT-REWRITE-FN [1-arg function]: A function called to rewrite text
    before submitting it to the repl. For example usage, see the docs of
    function `repl-driven-development-preconfiguration:python'.

  - ECHO-REWRITE-FN [1-arg function]: A function called to rewrite repl
    output before echoing it to the user.  For example usage, see the docs
    of function `repl-driven-development-preconfiguration:python'.

    Intentionally meant for human friendly pretty-printing, not for
    a READ protocol. Those serve different goals.
    The default READ protocol is this echo-rewrite-fn.
    Enter “M-x .*-read” to see the docs of the READ protocol
    for any REPL defined with this macro.

  - NAME [Symbol]: The name of the function associated to the keybinding
    KEYS. By default, the name is “CLI-eval”. This is used to namespace
    almost all other functions created by this macro.

  ### Misc Remarks #####################################################
  For more documentation, and examples,
  see URL `http://alhassy.com/repl-driven-development'.

  VSCode has a similar utility for making in-editor REPLs, by the
  same author: See URL `http://alhassy.com/making-vscode-itself-a-java-repl'."
  (if (symbolp cli)
      `(or (ignore-errors (funcall (intern (format "repl-driven-development-preconfiguration:%s" (quote ,cli))) ,keys))
           (message "It seems there is no preconfigured setup for “%s”; consider using a string with the name of a CLI process." (quote ,cli)))
   `(-let* (((repl . args) (s-split " " ,cli)))
          ;; (repl-fun-name string)
          (setf (rdd@ repl cmd) repl) ;; String
          (setf (rdd@ repl prompt) ,prompt) ;; String (Regular Expression)
          (setf (rdd@ repl keybinding)
                (s-join " " (mapcar #'pp-to-string ,keys))) ;; String
          ;; String: Space separated list
          (setf (rdd@ repl docs) ,docs)
          ;; The following is set to “nil” so that consecutive
          ;; calls to this method, but possibly with differing docs,
          ;; results in those docs being installed as needed.
          (setf (rdd@ repl docs-installed?) nil)
          ;; Used to avoid scenarios where input is echoed thereby
          ;; accidentally treating it as a repl output
          (setf (rdd@ repl input) "") ;; String
          (setf (rdd@ repl input/start) 0)
          (setf (rdd@ repl input/end) 0)
          (setf (rdd@ repl input-rewrite-fn) ,input-rewrite-fn)
          (setf (rdd@ repl echo-rewrite-fn) ,echo-rewrite-fn)
          (setf (rdd@ repl fun-name)
                (or ,name (intern (format "%s-eval" (rdd@ repl cmd)))))

          (setf (rdd@ repl init) ,init)
          (cl-assert (or (stringp ,init) (listp ,init)))
          (when (listp ,init) (setq ,init (s-join "\n" ,init)))
          (cl-assert (stringp ,init))

          (setf (rdd@ repl blink) ,blink)
          ;; Identifier "repl-driven-development" is made unique by
          ;; start-process.
          (setf (rdd@ repl process)
                (apply #'start-process "repl-driven-development"
                       nil repl args))

          (eval (repl-driven-development--make-eval-function repl))

          (process-send-string (rdd@ repl process) ,init)
          (process-send-string (rdd@ repl process) "\n")

          ;; Callback: Write the actual output to the REPL buffer
          ;; and emit overlay.
          (set-process-filter
           (rdd@ repl process)
           (repl-driven-development--main-callback (intern repl)))

          ;; Return the REPL symbol to the user (whose symbol-plist can be used
          ;; to get various information!)
          repl)))

;;; main-callback: insert or echo
(defun repl-driven-development--main-callback (repl)
  "Return the callback that works on REPL."
  `(lambda (process output)
     ;; This is done to provide a richer, friendlier, interaction.
     ;; ^M at the end of line in Emacs is indicating a carriage return (\r)
     ;; followed by a line feed (\n).
     (setq output (s-trim (s-replace-regexp ,(rdd@ repl prompt) ""
                                            (s-replace "\r\n" "" output))))
     ;; Output is always non-empty
     (unless (s-blank? (s-trim output))
       (setf (rdd@ (quote ,repl) output) output))

     (repl-driven-development--insert-or-echo (quote ,repl) output)))

(defun repl-driven-development--insert-or-echo (repl output)
  "If there's a C-u, then insert the OUTPUT; else echo it in overlay.

The echo only happens when OUTPUT differs from REPL's input."
  (cl-assert (stringp output))
  (cl-assert (symbolp repl))
  (cl-assert (stringp (rdd@ repl input)))
  (pcase current-prefix-arg
    ('(4) (unless (equal output (s-trim (rdd@ repl input)))
            (insert " " (funcall
                         (intern (format "%s-read"
                                         (rdd@ repl fun-name))) output))))
    ;; All other prefixes are handled by repl-fun-name, above.
    (_
     ;; Show output as an overlay at the current cursor position

     ;; For some reason, sometimes Emacs emit the input as part of the
     ;; output, so let's chop it off. For example, I've noticed this
     ;; with Python on *my* machine, but others have noticed it with Java[0],
     ;; but I couldn't reproduce this with Java.
     ;; [0] https://github.com/alhassy/repl-driven-development/issues/5
     (setq output (s-chop-prefix (rdd@ repl input) output))

     ;; ﴾ Since eros is intended to be used with ELisp, not arbitrary langs,
     ;; it does some sexp look-about, which may not mix well with, say, JS
     ;; arrow functions, so we freeze such movements, locally. ﴿
     (setq output
           (apply (rdd@ repl echo-rewrite-fn)
                  (list (repl-driven-development--ignore-ansi-color-codes
                         output))))
     (unless (s-blank? (s-trim output))
       (unless  (equal output (s-trim (rdd@ repl input)))
         ;; Tooltips ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (mapc #'delete-overlay
               (overlays-at (rdd@ repl input/start)))
         (let ((overlay (make-overlay (rdd@ repl input/start)
                                      (rdd@ repl input/end))))
           (overlay-put overlay 'help-echo output))
         ;; Messages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;; I want “C-h e” to show eval result ---just as “C-x C-e” does.
         (let ((inhibit-message
                (not repl-driven-development-echo-output-in-modeline)))
           (message "﴾%s﴿⇒ %s" (rdd@ repl fun-name)  output))
         ;; Overlays ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (require 'eros)
         (cl-letf (((symbol-function 'backward-sexp) (lambda (&rest _) 0)))
           (eros--make-result-overlay output
             :format  " ⮕ %s"
             :duration repl-driven-development-echo-duration)))))))

;;; make-eval-function
(defun repl-driven-development--make-eval-function (repl)
  "Constructs code denoting a function that sends a region to a REPL process."
  `(progn
     ;; TODO: Make a defun with a callback for repl testing a la
     ;; set-process-filter.

     ;; restart repl, [then send to repl --does not work since REPLs take a sec
     ;; to load. That's OK, not a deal-breaker!]
     (defun ,(intern (format "%s-restart" (rdd@ repl fun-name))) ()
       "Restart the REPL process."
       (interactive)
       (kill-buffer (process-buffer (rdd@ ,repl process)))
       (repl-driven-development (rdd@ ,repl keybinding)
                                (rdd@ ,repl cmd)
                                :prompt (rdd@ ,repl prompt)
                                :docs (rdd@ ,repl docs)
                                :init (rdd@ ,repl init)
                                :blink (rdd@ ,repl blink)))

     (defun ,(intern (format "%s-docs-at-point" (rdd@ repl fun-name))) ()
       "Documentation at point."
       (interactive)
       (unless (rdd@ ,repl docs-installed?)
         (repl-driven-development--install-any-not-yet-installed-docs
          (rdd@ ,repl docs))
         (setf (rdd@ ,repl docs-installed?) t))
       (repl-driven-development--docs-at-point (rdd@ ,repl docs)))

     (defun ,(intern (format "%s-buffer" (rdd@ repl fun-name))) ()
       ,(format "Execute the accessible portion of current buffer as %s code.

You can use \\[narrow-to-region] to limit the part of buffer to be evaluated."
                (rdd@ repl cmd))
       (interactive)
       (mark-whole-buffer)
       (call-interactively (rdd@ ,repl fun-name)))

     (defun ,(intern (format "%s-defun" (rdd@ repl fun-name))) ()
       ,(format "Evaluate innermost defun at point.

This method is only useful in a buffer whose major mode supports “%s” code.

⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
                     ﴾ Top-Level Definitions, or “Defuns” ﴿

In Emacs, a major definition at the top level in a buffer, such
as a function or class, is called a “defun”.

Just as “M-a, M-e, M-h” move to the start of a paragraph, end of a
paragraph, and select a paragraph, so to the “C-M-𝓍” forms are for
defuns:

⇒ \\[beginning-of-defun] and \\[end-of-defun] to move to the start/end of a defun.
⇒ \\[mark-defun] to highlight the current defun/class
  ↣ If mark is active, it extends the region until the end of the next defun.
  ↣ This selects the current class, or “topmost defun”, if point is outside
    a function but inside a class (e.g., in Java).
⇒ \\[imenu] gives you an interactive menu to see all defuns and to jump to them.

Note: Programming modes generally bind “M-a” and “M-e” to move forward and
backward between semantic units of code, e.g., statements.

Useful tips:
1. \\[comment-dwim] inserts a new comment, or toggles commenting a selection.
2. \\[mark-word] selects the current word; repeated calls select more words.

⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
                         ﴾ Subexpressions, or “Sexps” ﴿

Emacs calls “sexp” any subexpression that is enclosed in matching delimiters,
such as (parentheses), [brackets], {braces}, \"quotes\", or is a number, or a
single identifer.

We can use \\[backward-sexp], \\[forward-sexp], \\[kill-sexp], \\[mark-sexp] / “C-M-SPC” to move backward, forward, kill,
and highlight sexps.

For example, in front of a {braced code fragment}, press \\[mark-sexp] to highlight
that entire fragment. In front of a word, \\[mark-sexp] highlights just that word."
                (rdd@ repl cmd))
       (interactive)
       (mark-defun) (call-interactively (rdd@ ,repl fun-name)))

     (defun ,(intern (format "%s-read" (rdd@ repl fun-name))) (str)
       "Read STR into code executable by the REPL.

This is intended to result in executable code, from a possibly prettified string.

The read protocol is the “R” of “REPL”; it is fundamental if you want to insert
the result of an evaluation into the current buffer, say, for forming tests,
and so require the inserted text to also be executable.

By default, this method returns the human pretty-printing that the overlay echo
mechanism uses.

YOU SHOULD REDEFINE THIS METHOD, TO BE AN APPROPRIATE READ PROTOCOL.
(If you care about how inserted code looks.)

For an example, see `repl-driven-development--java-read'."
       (interactive "sRead: ")
       (apply (rdd@ ,repl echo-rewrite-fn) (list str)))

     (defun ,(intern (format "%s-string" (rdd@ repl fun-name))) (string)
       ,(format "Send STRING to the %s process, followed by a newline, to be executed.

To submit a region, use `%s'."
                (rdd@ repl cmd)
                (rdd@ repl fun-name))
       (setf (rdd@ ,repl input) string)
       (process-send-string (rdd@ ,repl process)
                            (apply (rdd@ ,repl input-rewrite-fn) (list string)))
       (process-send-string (rdd@ ,repl process) "\n"))

     (defun
         ,(intern (format "%s-display-output" (rdd@ repl fun-name)))
         ()
       "Show most recent REPL result. With C-u prefix, result is shown in its \
        own buffer.

If you see “...” then chances are that you have exceeded the default truncation
threshold for your REPL. Consider increasing the threshold, if possible, by
reading the docs of your REPL. For an example, see
`repl-driven-development-preconfiguration:java'."
       (interactive)
       (if (not current-prefix-arg)
           (display-message-or-buffer (rdd@ ,repl output))
         (switch-to-buffer (format "*%s-most-recent-result*"
                                   (rdd@ ,repl fun-name)))
         (insert (rdd@ ,repl output))))

     (bind-key*
      (rdd@ ,repl keybinding)
      (defun ,(rdd@ repl fun-name) (region-beg region-end)
        ,(repl-driven-development--make-eval-function-docstring repl)
        (interactive "r")

        (require 'pulsar)
        (-let [pulsar-face (rdd@ ,repl blink)]
          (pulsar-mode +1)
          (pulsar-pulse-line))

        (pcase current-prefix-arg
          (0  (,(intern (format "%s-display-output"
                                (rdd@ repl fun-name)))))
          (-1 (,(intern (format "%s-restart" (rdd@ repl fun-name)))))
          ;; ('(4)  (insert " " output)) ;; C-u ;; handled when we actually have
          ;; the output; see the process filter below
          ('(16) ;; C-u C-u ⇒ documentation lookup
           (,(intern (format "%s-docs-at-point" (rdd@ repl fun-name)))))
          (_
           (when (called-interactively-p 'interactive)
             (if (use-region-p)
                 (deactivate-mark)
               (beginning-of-line)
               (setq region-beg (point))
               (end-of-line)
               (setq region-end (point))))
           (setf (rdd@ ,repl input/start) region-beg)
           (setf (rdd@ ,repl input/end) region-end)
           (,(intern (format "%s-string" (rdd@ repl fun-name)))
            (s-trim-left (buffer-substring-no-properties
                          region-beg
                          region-end)))))))))

;;; docs-at-point

(defun repl-driven-development--install-any-not-yet-installed-docs (docs)
  "Install any not-yet-installed DOCS; return a List<String> of the installed \
docs.

DOCS is a space seperated sequence of identifiers for dev-docs."
  (when docs
    (cl-assert (stringp docs))
    (setq docs (--reject (s-blank? it) (s-split " " docs)))
    (cl-assert (listp docs))
    (-let [installed (mapcar #'f-base (f-entries devdocs-data-dir))]
      (mapc (lambda (it) (unless (member it installed)
                      (devdocs-install (list (cons 'slug it))))) docs))
    docs))

(defun repl-driven-development--docs-at-point (docs)
  "Lookup documentation at point using the given DOCS."
  ;; Test this by writing a word such as “IntStream.range(0, 44)”
  ;; then M-: (repl-driven-development--docs-at-point '("openjdk~19"))
  ;; anywhere on the phrase
  ;;
  ;; devdocs-lookup will ask to setup current docs when there's a
  ;; current-prefix, so we null it.
  ;; If user does have it setup, we want to temporarily change its value for
  ;; use with the current repl.
  (cl-assert (stringp docs))
  (let ((devdocs-history nil)
        (current-prefix-arg nil)
        (devdocs-current-docs (s-split " " docs))
        (word (or (thing-at-point 'symbol) "")))
    ;; (devdocs-lookup nil word) ⇒ Quits abruptly when keyword is not a
    ;; valid candidate!
    (minibuffer-with-setup-hook
        `(lambda () (insert ,word))
      (call-interactively #'devdocs-lookup))))

;;; docstrings

(defun repl-driven-development--make-eval-function-docstring (repl)
  "Make the docstring for a REPL function working with command CLI."
  (let ((keys (rdd@ repl keybinding))
        (cmd (rdd@ repl cmd)))
    (setq repl (rdd@ repl fun-name))
    (lf-string
     "Executes the selected region, if any or otherwise the entire current line,
    and evaluates it with the command-line tool “${cmd}”.

    Output is shown as an overlay at the current cursor position.
    It is shown for `repl-driven-development-echo-duration' many seconds.

    ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
                             ﴾ Familiar Workflow ﴿

    You can execute arbitrary Lisp anywhere by pressing \\[eval-last-sexp], you can
    insert the result with “C-u C-x C-e”, and see the output echoed in the
    mode-line and in the *Messages* buffer with \\[view-echo-area-messages].
    Likewise, you can execute “${cmd}” code by pressing “${keys}”, insert output
    with “C-u ${keys}”, and see the output echoed near your cursor and in the
    *Messages* buffer with “C-h e”.

    Other familiar functions include `${repl}-defun', `${repl}-buffer', and `${repl}-string'.

    ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
                       ﴾ C-u ${keys}  ≈  Insert result ﴿

    With a “C-u” prefix, the output is inserted at point
    (and not echoed in an overlay).

    Since ${repl} may pretty-print its output, inserting it may result in
    non-executable code. If you want executable code, you must specify
    how pretty-printed output must be converted into ${repl}-executable code.
    Do so by redefining `${repl}-read'.

    ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
                 ﴾ C-u 0 ${keys}   ≈  `${repl}-display-output' ﴿

    Sometimes it may be useful to look at a large output in a dedicated buffer.
    However, the output of a command is also attached to the input via a
    tooltip: Hover to see it! See also `tooltip-delay'.
    Moreover, “C-h e” shows you the output in the *Messages* buffer.
    See also `repl-driven-development-echo-output-in-modeline'.

    ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
              ﴾ C-u C-u ${keys}  ≈  `${repl}-docs-at-point' ﴿

    With a “C-u C-u” prefix, documentation is looked-up for the word at point.

    This is done using `devdocs', and so the documentation generally provides
    example uses as well. Visit https://devdocs.io/ to see the list of documented
    languages and libraries.

    ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
                 ﴾ C-u -1 ${keys}  ≈  `${repl}-restart' ﴿

    In the event you've messed-up your REPL, starting from a blank slate may be
    helpful.

    ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
                            ﴾ Implementation Notes ﴿

    The interactive method is asynchronous: Whenever you send text for
    evaluation, you immediately regain control in Emacs; you may send more text
    and it will be queued for evaluation. For example, evaluating a sleep
    command for 3 seconds does not block Emacs.

    ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
                                  ﴾ See also ﴿

    See `repl-driven-development' for more useful docs.

    See URL `http://www.alhassy.com/repl-driven-development' to learn more about
    RDD see examples and many gifs.")))

(defun repl-driven-development--ignore-ansi-color-codes (string-with-codes)
  "Ignore ANSI color codes in STRING-WITH-CODES."
  (with-temp-buffer
    (insert string-with-codes)
    (ansi-color-apply-on-region (point-min) (point-max))
    (buffer-string)))

;;; pre-configured repls
(defun repl-driven-development-preconfiguration:terminal (keys)
  "A Bash REPL configuration, bound to keybinding KEYS."
  ;; Disable “bash” intro message
  (setenv "BASH_SILENCE_DEPRECATION_WARNING" "1")
  (repl-driven-development
   keys
   "bash"
   :name 'terminal-eval
   :docs "bash"
   :prompt "^bash.*?\\$"
   :init "echo \"Enjoy the Terminal with Emacs (｡◕‿◕｡)\""))

(defun repl-driven-development-preconfiguration:applescript (keys)
  "An AppleScript REPL configuration, bound to keybinding KEYS.

✔ Supports multi-line output.
⚠ Each snippet must be a self-contained program; i.e., snippets do not share state."
  ;; Disable “bash” intro message
  (setenv "BASH_SILENCE_DEPRECATION_WARNING" "1")
  (repl-driven-development
   keys "bash"
   :name 'applescript-eval
   :blink 'pulsar-blue
   :prompt "\\(bash.*?\\$\\)\\|>"
   :init "echo \"Enjoy AppleScript with Emacs (｡◕‿◕｡)\"
          while true; do
          read input
          osa_script=\"\"
          while [ \"$input\" != \";;\" ]; do
              osa_script=\"$osa_script$input\"$'\n'
              read input
          done
          echo $(osascript -e \"$osa_script\") && echo \"Done (◕‿◕)\"
          done"
   :input-rewrite-fn (lambda (in) (format "%s\n;;" in))))

(defun repl-driven-development-preconfiguration:javascript (keys)
  "A NodeJS REPL configuration, bound to keybinding KEYS.

✔ Defines variable “axios”, if the library is installed,
e.g., “npm i -g axios”

✔ Docs: Vanialla JS, Web APIs (e.g., “fetch”), Axios, Express.
"
  (setenv "NODE_DISABLE_COLORS" "1") ;; So we don't display funky colour code upon insert
  (repl-driven-development
   keys "node"
   :name 'javascript-eval
   :prompt ">"
   :docs "javascript dom axios express"
   :input-rewrite-fn
   #'repl-driven-development--strip-out-C-style-comments&newlines
   ;; Load “axios”, if it's present.
   ;; By keeping everything in “one line”, there is only one output emitted.
   ;; Namely, I don't want users to see the output of loading axios.
   :init (s-join ";" '("var axios = null"
                       "try { axios = require('axios') } catch (error) { }"
                        "shell = command => require('child_process').execSync(command).toString().trim()"
                        ;; Escape quotes/etc by going to base64 via JS built-in `btoa`, then decode base64 on the Lisp side
                        "emacs = { eval: sexp => shell(`emacsclient -e '(eval (read (base64-decode-string \"${btoa(sexp)}\")))'`) }"
                       "\"Enjoy JavaScript with Emacs (｡◕‿◕｡)!\""))))

(defun repl-driven-development--strip-out-C-style-comments&newlines (str)
  "Strip out C-style single-line and multi-line comments from STR.

Do not touch the tokens “https://” nor “http://”."
  (let ((http (pp-to-string (gensym)))
        (https (pp-to-string (gensym))))
  (thread-last
    str
    (s-replace-regexp "/\\*.\\*/" "")
    (s-replace-regexp "http://" http)
    (s-replace-regexp "https://" https)
    (s-replace-regexp "//.*$" "")
    (s-replace-regexp "\n" "")
    (s-replace-regexp http "http://")
    (s-replace-regexp https "https://"))))

(defun repl-driven-development-preconfiguration:python (keys)
  "A Python REPL configuration, bound to keybinding KEYS.

This configuration fixes the following shortcomings of the default Python CLI
repl:

❌ The Python repl abruptly terminates def|class definitions when there is an
  empty new line in their definition.
✔ This configuration strips out all empty newlines.

❌ The Python repl requires an extra new line after a def|class definition to
  confirm that the definition has concluded.
✔ This configuration automatically adds such extra new lines.

❌ The Python repl emits nothing when a def|class declaration is submitted.
✔ This configuration emits a “Defined ⋯” message, along with the declaration's
   body.

NOTE: Get a nice IDE experience with “sudo npm install -g pyright”, then
open a Python file and invoke “M-x eglot”."
  (repl-driven-development
   keys
   "python3 -q"  ;; “-q” to avoid showing intro message
   :prompt ">>>"
   :name 'python-eval
   :docs "python~3.5"
   :blink 'pulsar-red
   ;; Remove empty lines: In the middle of a def|class, they abruptly terminate
   ;; the def|class!
   :input-rewrite-fn (lambda (in) (concat (s-replace-regexp "^\s*\n" "" in) "\n\n\r"))
   ;; For some reason, Python (in Emacs shells) emits the input as part of the
   ;; output, so let's chop it off.
   ;; Default Python repl emits nothing on def|class declarations,
   ;; let's change that.
   :echo-rewrite-fn
   (lambda (echo)
     (let* ((input  (rdd@ "python3" input))
            (result (s-chop-prefix input echo)))
       (cond ((s-starts-with? "def" input)
              (s-replace-regexp " *def \\([^(]*\\).*" "Defined “\\1”" input))
             ((s-starts-with? "class" input)
              (s-replace-regexp " *class \\([^(:]*\\).*" "Defined “\\1”:" input))
             (t result))))
      :init "print(\"Enjoy Python with Emacs (｡◕‿◕｡)\")"))

(defun repl-driven-development-preconfiguration:java (keys)
  "A Java REPL configuration, bound to keybinding KEYS.

✔ This REPL is aware of all JARs in the ~/.m2/repository; the location of
external JARs installed via the mvn tool.

✔ Notify me when imports succeed.

✔ Show types of results.

✔ JShell has a truncation for outputs exceeding 1k chars, we increase the
  threshold to size 40k chars.

Implementation details below.

⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
                            ﴾ Setting a Classpath ﴿

The classpath lets jshell access non-standard-library code.

    % jshell --class-path .:myJar:myOtherJar:dir/to/compiled/classes

Paths can be to Java Archives (JARs) or to directories of *compiled* class files
---which must be in a *named package*.

You also use the '/env' command to set the classpath:

jshell> /env --class-path myOwnClassPath
|  Setting new options and restoring state.
jshell> import my.cool.code.*

Note that this command resets the execution state, reloading any
current snippets with the new classpath setting (or other
environment setting).

⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
                           ﴾ JShell Feedback Modes ﴿

The built-in feedback modes cannot have their
prompt|truncation|format changed, but we can inheirt from a
built-in mode, then change those properties.

⟅ ✔ Notify me when imports succeed ⟆

In the format, inherited from normal mode, an import doesn't give
any feedback, and the type of a value is not shown.

    import java.beans.* // Submit this twice and both times see nothing :-(

    /set mode myNewMode normal -command
    /set feedback myNewMode
    /help /set format  // There is extensive help on this command
                      // with /help /set format.

    /set format myNewMode display \"{pre}added import {name}{post}\" \
import-added
    /set format myNewMode display \"{pre}re-added import {name}{post}\" \
import-modified,replaced

    import java.awt.Graphics // Submit this twice and see two distinct messages

This is automatically part of the Emacs Java REPL.

⟅ ✔ Show types of results ⟆

Let's also change the default “var ==> value” output to include the type of the
resulting value.

    // The payload does not indicate the type of this thing,
    // I'd like to know what kind of data I'm working with!
    Locale.CANADA.getUnicodeLocaleAttributes() // $10 ==> []

    /set format myNewMode result \"{type} {name} = {value}{post}\" \
added,modified,replaced-primary-ok

    2 + 4                                      // int $11 = 6
    String me = \"hello\"                      // Emits: String me ⇒ \"hello\"
    Locale.CANADA.getUnicodeLocaleAttributes() // Set<String> $12 = []

⟅ ✔ Extended Truncation Limit ⟆

We can increase the truncation as follows,
this is done automatically for the Java REPL.

   /set truncation myNewMode 40000
   IntStream.range(0, 40000).mapToObj(x -> x).toList()

⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
                               ﴾ Useful Reading ﴿

- The excellent JShell Tutorial by Robert Field at URL
  `https://cr.openjdk.org/~rfield/tutorial/JShellTutorial.html'.
- A Gentle Intro to JShell at URL
  `https://www.theserverside.com/blog/Coffee-Talk-Java-News-Stories-and-Opinions/Java-JShell-Online-Commands-How-to-Tutorial-Exit'.

⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
                            ﴾ Implementation Notes ﴿

We do not use the “--startup JAVASE” option to import all Java SE packages by
default; since the simplest call “List.of(1)” results in an ambiguity error:
java.util.List and java.awt.List match."
  (repl-driven-development
   keys
   ;; enable assertions, and add everything installed, via `mvn', in scope.
   (format "jshell --class-path %s --enable-preview -R -ea --feedback silent"
           (concat ".:" (shell-command-to-string "find ~/.m2/repository -name \"*.jar\" -type f 2>/dev/null | tr '\n' ':'")))
   :name 'java-eval
   :prompt "jshell>"
   :input-rewrite-fn
   #'repl-driven-development--strip-out-C-style-comments&newlines
   :init "/set mode EmacsJavaMode normal -command
/set format EmacsJavaMode display \"{pre}added import {name}{post}\" import-added
/set format EmacsJavaMode display \"{pre}re-added import {name}{post}\" import-modified,replaced
/set format EmacsJavaMode result \"{type} {name} = {value}{post}\" added,modified,replaced-primary-ok
/set truncation EmacsJavaMode 40000
/set feedback EmacsJavaMode
import javax.swing.*;
System.out.println(\"Enjoy Java with Emacs (｡◕‿◕｡))\")")

  (defalias 'java-eval-read #'repl-driven-development--java-read))

;; TODO Consider using my own overlays, like I do for tooltips, instead of using
;;      eros. Then, for example, I don't need to worry about this truncation
;;      limitation: https://github.com/xiongtx/eros/blob/master/eros.el#L202
;;      Then again, this truncation is fine
;;      (since we have repl-display-output) and eros is lovely.

;;; READ Protocol for Java

(defun repl-driven-development--java-read (a-pretty-printed-record)
  "Read executable Java code from pretty-printed record representations.

A-PRETTY-PRINTED-RECORD is a a string denoting pretty printed Java output.

If called interactively via M-x, copies executable Java to clipboard; otherwise
returns the executable Java as a string.

Using PEGs; see URL `https://elpa.gnu.org/packages/peg.html'."
  (interactive "sJava Read: ")
  (let ((result (thread-last
                  a-pretty-printed-record ;; Has shape “type name = value”.
                  (s-replace-regexp "^[^ ]* [^ ]* = " "")
                  repl-driven-development--parse-pretty-printed-java
                  repl-driven-development--lisp-to-java)))
    (if (called-interactively-p 'interactive)
        (progn (kill-new result)
               (message "Copied: %s" result))
      (insert "\n")
      result)))

(defun repl-driven-development--parse-pretty-printed-java (&optional str)
  "If called interactively, run the parser at point.
If called from Lisp, run the parse on the given string STR.

Return a sexp with info about the parsed item.

⦉𝒯𝒾𝓅⦊ When actually implementing this parser, I enclosed the
defun in (bind-key [M-p] ⋯).  Now I could quickly iterate on the
parser: Make a small modification to the parser, have one of my
test cases in the same buffer, and invoke M-p to parse it.  This
let me see “how far” the parsing got and where it got stuck."
  (interactive)
  (with-peg-rules
      ((expr (or structured-expr literal))
       (structured-expr (or map list record number))
       (list (or empty-list non-empty-list))
       (empty-list "[]"
                   `(val -- (list :type :list :items nil)))
       (non-empty-list "[" expr (list (* "," (* [space]) structured-expr)) "]"
                       `(first rest -- (list :type :list
                                             :items (cons first rest))))
       (map "{" (list (opt payloads)) "}"
            `(entries -- (cons :type (cons :map (cons :entries entries)))))
       (literal (substring (* [a-z A-Z 0-9 " \"-;!#%&'*+,./:;<=>?@[]^_`{|}~"])))
       (record identifier "[" (opt payloads) "]"
               `(k vs -- (list :type :record
                               :name (plist-get k :value)
                               :fields vs)))
       (identifier (substring (+ (or [alpha] comma "-" [space])))
                   `(val -- (list :type :string :value val)))
       (comma    "," (not (and [space] (+ [alpha]) "=")))
       (payloads (list payload (* (and "," (opt (+ [space])) payload))))
       (payload identifier "=" (or structured-expr identifier)
                `(k v -- (list :name (plist-get k :value) :value v)))
       (number (substring (and (+ [digit]) (opt (and "." (+ [digit])))))
               `(val -- (list :type :number :value (string-to-number val)))))
    (if (called-interactively-p 'interactive)
        (peg-run (peg expr)
                 #'ignore
                 (lambda (args) (insert " ;; ⇒ "
                                   (pp-to-string (car (funcall args))))))
      (with-temp-buffer
        (insert str)
        (goto-char (point-min))
        (let (result)
          ;; peg-run ⇒ Parse with matcher at point,
          ;;           & run success|failure function.
          (peg-run (peg expr) #'ignore
                   (lambda (args) (setq result (car (funcall args)))))
          result)))))

(defun repl-driven-development--lisp-to-java (data)
  "Convert the given Lisp sexp DATA into a JShell executable snippet."
  (s-replace-regexp
   "\n\\(,\\|)\\)" "\\1"
   (pcase (plist-get data :type)
     (:number (format "%s" (plist-get data :value)))
     (:string (-let [it (plist-get data :value)]
                (if (equal it "null")
                    it
                  ;; Escapes quotes.
                  (pp-to-string it))))
     (:list (thread-last (plist-get data :items)
                         (mapcar #'repl-driven-development--lisp-to-java)
                         (s-join ", ")
                         (format "List.of(%s)")))
     ;; Invariant: A :record type have :value in all fields.
     (:record (thread-last
                (plist-get data :fields)
                (--map (repl-driven-development--lisp-to-java (plist-get it :value)))
                (s-join ", ")
                (format "new %s(%s)" (plist-get data :name))))
     (:map (thread-last
             (plist-get data :entries)
             (--map (format "%s, %s"
                            (plist-get it :name)
                            (repl-driven-development--lisp-to-java
                             (plist-get it :value))))
             (s-join ", ")
             (format "Map.of(%s)")))
     (else (error "lisp-to-java: Unknown data type “%s”" else)))))

;;; navigate-most-recent-result

(cl-defun java-eval-navigate-output (&optional (str (rdd@ 'jshell output)))
  "Render STR, the last JShell output, as a clickable interactive hierarchy.

For example,

  ;; See a JSON dropdown of three objects, key-value pairs.
  (java-eval-navigate-output \"[Person[name=Jasim, age=72], Person[name=Kathy, age=82], Person[name=Jaafar, age=31]]\")

  ;; See a deeply-nested object that you can inspect
  (java-eval-navigate-output \"Person[name=Jasim, age=72, child=Person[name=Hassan, age=22, child=Person[name=Abbas, age=31, child=null]]]\")

﴾ Remark: We are not limited to textual output ﴿
We can use the full power of Emacs to render data in any kind of format
that is useful for the domain at hand. For example, rendering tabular data
in an Org buffer; HTML data into a xwidget-webkit browser; or any kind of
suitable major mode; or even opening an external program."
  (interactive)
  (if (and
       (require 'hierarchy)        ;; Interactive, clickable, views of hierarchical data
       (require 'json-navigator))   ;; hierarchy.el specifically for JSON data
      (with-temp-buffer
        (thread-last
          str ;; Has shape “type name = value”.
          (s-replace-regexp "^[^ ]* [^ ]* = " "")
          repl-driven-development--parse-pretty-printed-java
          repl-driven-development--java-lisp-to-json-lisp
          json-insert)
        (goto-char (point-min))
        (json-navigator-navigate-after-point))
    (message "Please delete ~/.emacs.d/elpa/hierarchy-*/hierarchy.elc")))

(defun repl-driven-development--java-lisp-to-json-lisp (data)
  "Convert the given Lisp sexp DATA into a JSON Lisp representation."
  (cl-assert (plist-member data :type))
  (pcase (plist-get data :type)
    (:number (plist-get data :value))
    (:string (plist-get data :value))
    (:list (thread-last (plist-get data :items)
                        (mapcar #'repl-driven-development--java-lisp-to-json-lisp)
                        (seq--into-vector)))
    (:record (thread-last
               (plist-get data :fields)
               (--map (list (intern (plist-get it :name))
                            (repl-driven-development--java-lisp-to-json-lisp (plist-get it :value))))
               (apply #'-concat)
               (-cons* :type (plist-get data :name))))
    (else (error "java-lisp-to-json-lisp: Unknown data type “%s”" else))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'repl-driven-development)

;;; repl-driven-development.el ends here

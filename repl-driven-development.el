;;; repl-driven-development.el --- Send arbitrary code to a REPL in the background

;; Copyright (c) 2023 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 1.0.8
;; Package-Requires: ((s "1.12.0") (lf "1.0") (dash "2.16.0") (eros "0.1.0") (bind-key "2.4.1") (emacs "29.1") (f "0.20.0") (devdocs "0.5") (pulsar "1.0.1"))
;; Keywords: repl-driven-development, rdd, repl, lisp, java, python, ruby, programming, convenience
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

;; String and list manipulation libraries
;; https://github.com/magnars/dash.el
;; https://github.com/magnars/s.el

;; TODO: Clean up code ---i.e., address various TODOs.
;; TODO: Add mini-tutorial, from pkg docs, to Github README.
;; TODO: Implement Read Protocol for Java.
;; TODO[Low Priority]: Implement pretty printing for Python.
;; TODO[Low Priority]: Implement a simple Read Protocol for JS. (eg JSON.parse)
;; TODO: terminal-repl-insert-last-output,
;; terminal-repl-copy-last-output-to-clipboard,
;; terminal-repl-show-last-input-and-output
;; [An Org mode buffer with the last-input and last output, headings]
;; TODO: Add precondition checks to each method.
;; (cl-assert (symbolp repl))
;; (cl-assert (stringp (rdd@ repl current-input)))

(when nil ⨾⨾ Rich Comment consisting of executable code to try things out.

      ;; Testing setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (load-file "./testing-setup.el") ;; See my init.el
      (eval-buffer)
      ;; Style errors, package errors
      (my/show-errors)
      ;; Byte-compiles the file with all warnings enabled.
      (elisp-lint--byte-compile  (buffer-file-name))
      ;; Show me references to unbound symbols
      (elint-current-buffer)
      (my/load-file-in-new-emacs)
      (outshine-mode)
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; A simple terminal REPL works as expected.
      (repl-driven-development [C-x C-t] "bash" :blink 'pulsar-green)
      echo -e "$(whoami): On $(date), I sit at $(pwd) pondering ... \n $(ls)"

      ;; Insert the result of the above shell command with C-u C-x C-t.

      ;; Or see it in its own buffer with M-x ...
      (bash-repl-display-most-recent-result) ;; i.e., (rdd@ "bash" output)

      ;; We can also restart the repl... let's set some state
      export X=123
      echo $X
      ;; Now restart it with C-u -1 C-x C-t
      echo $X
      ;; C-x C-t on the above line emits no value

      ;; Init code works upon initialisation, neato!
      (repl-driven-development [C-x C-t] "bash" :init "echo $(fortune)")

      ;; We can get rid of the prompt at the end with :prompt
      (repl-driven-development [C-x C-t] "bash" :prompt "^[^ ]*\\$")

      (repl-driven-development [C-x C-t] terminal)

      ;; We can change the blinking colours via rdd@.
      (repl-driven-development [C-x C-n] "node" :blink 'pulsar-blue)
      [...Array(14).keys()].map(x => x % 3 == 0 ? "Fizz" : x)
      ;; Change colour with C-x C-e, then C-x C-n on the line after.
      (setf (rdd@ "node" blink) 'pulsar-green)
      Object.keys({name: "mikle", 1: "one"})

      ;; We can use a preconfigured REPL.
      ;;
      ;; Notice associated buffer's name involves only the command "jshell",
      ;; not the args. See it via C-u 0 C-x C-j.
      (repl-driven-development [C-x C-j] java)
      ;;
      ;; This allows us to submit multi-line input seamlessly.
      ;; Select the following 6 lines, then submit this region with C-x C-j
      IntStream
      /* a multi-line
      * comment */
      .range(0, 23)
      // Now print it out
      .forEach(x -> System.out.println(x))

      ;; Likewise for NodeJS
      (repl-driven-development [C-x C-n] javascript)
      ;; Then submit:
      [...Array(40).keys()]
      // yay, a comment in the middle
      .map(x => x % 3 == 0 ? "Fizz" : x)

      ;; Likewise for Python
      (repl-driven-development  [C-x C-p] python)
      ;; Send each line, one at a time.
      "
      1 + 2 * 3

      def foo(x): return x*x

      foo(5)

      list(map(lambda i: 'Fizz'*(not i%3)+'Buzz'*(not i%5) or i, range(1,101)))

      # (Above shows Result Truncated due to my use of eros, which has this limit.
      # TODO[Low Priority]: Fix this.)
      #
      # We can do multi-line def ---The quotes are to allow me to indent,
      # otherwise my aggressive-formatter strips the whitespace away.

      def square(x):
      return x * x

      square(5)

      # Likewise for class-es:

      class MyClass():
      i = 12345

      def f(self):
      return 'hello world'


      x = MyClass()
      x.i
      x.f()
      "
      ;; Notice that the code is identend nicely.
      )

(require 's)               ;; “The long lost Emacs string manipulation library”
(require 'dash)            ;; “A modern list library for Emacs”
(require 'cl-lib)          ;; New Common Lisp library; ‘cl-???’ forms.
(require 'eros)            ;; Simple Emacs Overlays
(require 'bind-key)        ;; Bind keys
(require 'lf)              ;; Template strings with lf-string
;; [lf requires nil lexical binding!]

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

(defvar repl-driven-development-echo-duration 5)

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
    function `repl-driven-development--preconfigured-python-REPL'.

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
    function `repl-driven-development--preconfigured-python-REPL'.

  - ECHO-REWRITE-FN [1-arg function]: A function called to rewrite repl
    output before echoing it to the user.  For example usage, see the docs
    of function `repl-driven-development--preconfigured-python-REPL'.

    Intentionally meant for human friendly pretty-printing, not for
    a READ protocol. Those serve different goals.
    The default READ protocol is this echo-rewrite-fn.
    Enter “M-x .*-repl-read” to see the docs of the READ protocol
    for any REPL defined with this macro.

  - NAME [Symbol]: The name of the function associated to the keybinding
    KEYS. By default, the name is “CLI-repl”. This is used to namespace all
    other functions created by this macro.

  ### Misc Remarks #####################################################
  VSCode has a similar utility for making in-editor REPLs, by the
  same author: http://alhassy.com/making-vscode-itself-a-java-repl"
  (pcase cli
    ('java
     ;; JShell does semicolon insertion eagerly, so it things the following
     ;; are three separate expressions! We can fix this by removing new lines.
     `(repl-driven-development--preconfigured-java-REPL ,keys))
    ;; Likewise JS does eager semicolon insertion.
    ('javascript
     `(repl-driven-development--preconfigured-javascript-REPL ,keys))
    ('terminal
     `(repl-driven-development--preconfigured-terminal-REPL ,keys))
    ('python `(repl-driven-development--preconfigured-python-REPL ,keys))
    (_ `(-let* (((repl . args) (s-split " " ,cli)))
          ;; (repl-fun-name string)
          (setf (rdd@ repl cmd) repl) ;; String
          (setf (rdd@ repl prompt) ,prompt) ;; String (Regular Expression)
          (setf (rdd@ repl keybinding)
                (s-join " " (mapcar #'pp-to-string ,keys))) ;; String
          ;; String: Space separated list
          (setf (rdd@ repl docs) (s-join " " ,docs))
          ;; Used to avoid scenarios where input is echoed thereby
          ;; accidentally treating it as a repl output
          (setf (rdd@ repl current-input) "") ;; String
          (setf (rdd@ repl current-input/start) 0)
          (setf (rdd@ repl current-input/end) 0)
          (setf (rdd@ repl input-rewrite-fn) ,input-rewrite-fn)
          (setf (rdd@ repl echo-rewrite-fn) ,echo-rewrite-fn)
          (setf (rdd@ repl fun-name)
                (or ,name (intern (format "%s-repl" (rdd@ repl cmd)))))

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

          (setq docs
                (repl-driven-development--install-any-not-yet-installed-docs
                 ,docs))
          (eval (repl-driven-development--make-repl-function repl))

          (process-send-string (rdd@ repl process) ,init)
          (process-send-string (rdd@ repl process) "\n")

          ;; Callback: Write the actual output to the REPL buffer
          ;; and emit overlay.
          (set-process-filter
           (rdd@ repl process)
           (repl-driven-development--main-callback (intern repl)))

          ;; Return the REPL process to the user.
          (rdd@ repl process)))))

(defun repl-driven-development--preconfigured-terminal-REPL (keys)
  "A Bash REPL configuration, bound to keybinding KEYS."
  (repl-driven-development
   keys
   "bash"
   :name 'terminal-repl
   :prompt "^[^ ]*\\$"))

(defun repl-driven-development--preconfigured-javascript-REPL (keys)
  "A NodeJS REPL configuration, bound to keybinding KEYS."
  (repl-driven-development
   keys "node"
   :name 'javascript-repl
   :prompt ">"
   :input-rewrite-fn
   #'repl-driven-development--strip-out-C-style-comments&newlines))

(defun repl-driven-development--preconfigured-python-REPL (keys)
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
   body."
  (repl-driven-development
   keys
   "python3"
   :prompt ">>>"
   :name 'python-repl
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
     (let* ((input  (rdd@ "python3" current-input))
            (result (s-chop-prefix input echo)))
       (cond ((s-starts-with? "def" input)
              (s-replace-regexp " *def \\([^(]*\\).*" "Defined “\\1”" input))
             ((s-starts-with? "class" input)
              (s-replace-regexp " *class \\([^(:]*\\).*" "Defined “\\1”:" input))
             (t result))))))

(defun repl-driven-development--preconfigured-java-REPL (keys)
  "A Java REPL configuration, bound to keybinding KEYS."
  (repl-driven-development
   keys
   "jshell"
   :name 'java-repl
   :prompt "jshell>"
   :input-rewrite-fn
   #'repl-driven-development--strip-out-C-style-comments&newlines))

(defun repl-driven-development--strip-out-C-style-comments&newlines (str)
  "Strip out C-style single-line and multi-line comments from STR."
  (thread-last
    str
    (s-replace-regexp "/\\*.\\*/" "")
    (s-replace-regexp "//.*$" "")
    (s-replace-regexp "\n" "")))

(defun repl-driven-development--main-callback (repl)
  "Return the callback that works on REPL."
  `(lambda (process output)
     ;; This is done to provide a richer, friendlier, interaction.
     ;; ^M at the end of line in Emacs is indicating a carriage return (\r)
     ;; followed by a line feed (\n).
     (setq output (s-trim (s-replace-regexp ,(rdd@ repl prompt) ""
                                            (s-replace "\r\n" "" output))))
     (setf (rdd@ (quote ,repl) output) output)

     (repl-driven-development--insert-or-echo (quote ,repl) output)))

(defun repl-driven-development--install-any-not-yet-installed-docs (docs)
  "Install any not-yet-installed DOCS; return a List<String> of the installed \
docs."
  (when docs
    (require 'devdocs)
    (cl-assert (stringp docs))
    (setq docs (--reject (s-blank? it) (s-split " " docs)))
    (cl-assert (listp docs))
    (-let [installed (mapc #'f-base (f-entries devdocs-data-dir))]
      (mapc (lambda (it) (unless (member it installed)
                      (devdocs-install (list (cons 'slug it))))) docs))
    docs))

(defun repl-driven-development--insert-or-echo (repl output)
  "If there's a C-u, then insert the OUTPUT; else echo it in overlay.

The echo only happens when OUTPUT differs from REPL's input."
  (cl-assert (stringp output))
  (cl-assert (symbolp repl))
  (cl-assert (stringp (rdd@ repl current-input)))
  (pcase current-prefix-arg
    ('(4) (unless (equal output (s-trim (rdd@ repl current-input)))
            (insert " " (funcall
                         (intern (format "%s-read"
                                         (rdd@ repl fun-name))) output))))
    ;; All other prefixes are handled by repl-fun-name, above.
    (_
     ;; Show output as an overlay at the current cursor position
     ;; ﴾ Since eros is intended to be used with ELisp, not arbitrary langs,
     ;; it does some sexp look-about, which may not mix well with, say, JS
     ;; arrow functions, so we freeze such movements, locally. ﴿
     (setq output
           (apply (rdd@ repl echo-rewrite-fn)
                  (list (repl-driven-development--ignore-ansi-color-codes
                         output))))
     (unless (s-blank? (s-trim output))
       (unless  (equal output (s-trim (rdd@ repl current-input)))
         ;; Tooltips ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (mapc #'delete-overlay
               (overlays-at (rdd@ repl current-input/start)))
         (let ((overlay (make-overlay (rdd@ repl current-input/start)
                                      (rdd@ repl current-input/end))))
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

(defvar repl-driven-development-echo-output-in-modeline nil
  "In addition to the overlays, should REPL output be emitted in the modeline?

You can always use `C-h e' to see output in the *Messages* buffer.")

(defun repl-driven-development--make-repl-function (repl)
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
       (repl-driven-development--docs-at-point (quote ,(rdd@ repl docs))))

     (defun ,(intern (format "%s-read" (rdd@ repl fun-name))) (str)
       "Read STR into code executable by the REPL.

This is intended to result in executable code, from a possibly prettified string.

The read protocol is the “R” of “REPL”; it is fundamental if you want to insert
the result of an evaluation into the current buffer, say, for forming tests,
and so require the inserted text to also be executable.

By default, this method returns the human pretty-printing that the overlay echo
mechanism uses.

YOU SHOULD REDEFINE THIS METHOD, TO BE AN APPROPRIATE READ PROTOCOL.
(If you care about how inserted code looks.)"
       (interactive "sRead: ")
       (apply (rdd@ ,repl echo-rewrite-fn) (list str)))

     (defun ,(intern (format "%s-submit" (rdd@ repl fun-name))) (str)
       ,(format "Send STR to the REPL process, followed by a newline.

To submit a region, use `%s'." (rdd@ repl fun-name))
       (setf (rdd@ ,repl current-input) str)
       (process-send-string (rdd@ ,repl process)
                            (apply (rdd@ ,repl input-rewrite-fn) (list str)))
       (process-send-string (rdd@ ,repl process) "\n"))

     (defun
         ,(intern (format "%s-display-most-recent-result" (rdd@ repl fun-name)))
         ()
       "Show most recent REPL result. With C-u prefix, result is shown in its \
        own buffer."
       (interactive)
       (if (not current-prefix-arg)
           (display-message-or-buffer (rdd@ ,repl output))
         (switch-to-buffer (format "*%s-most-recent-result*"
                                   (rdd@ ,repl fun-name)))
         (insert (rdd@ ,repl output))))

     (bind-key*
      (rdd@ ,repl keybinding)
      (defun ,(rdd@ repl fun-name) (region-beg region-end)
        ,(repl-driven-development--make-repl-function-docstring repl)
        (interactive "r")

        (require 'pulsar)
        (-let [pulsar-face (rdd@ ,repl blink)]
          (pulsar-mode +1)
          (pulsar-pulse-line))

        (pcase current-prefix-arg
          (0  (,(intern (format "%s-display-most-recent-result"
                                (rdd@ repl fun-name)))))
          (-1 (,(intern (format "%s-restart" (rdd@ repl fun-name)))))
          ;; ('(4)  (insert " " output)) ;; C-u ;; handled when we actually have
          ;; the output; see the process filter below
          ('(16) ;; C-u C-u ⇒ documentation lookup
           (,(intern (format "%s-docs-at-point" (rdd@ repl fun-name)))))
          (_
           (if (use-region-p)
               (deactivate-mark)
             (beginning-of-line)
             (setq region-beg (point))
             (end-of-line)
             (setq region-end (point)))
           (setf (rdd@ ,repl current-input/start) region-beg)
           (setf (rdd@ ,repl current-input/end) region-end)
           (,(intern (format "%s-submit" (rdd@ repl fun-name)))
            (s-trim-left (buffer-substring-no-properties
                          region-beg
                          region-end)))))))))


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
  (let ((devdocs-history nil)
        (current-prefix-arg nil)
        (devdocs-current-docs docs)
        (word (or (thing-at-point 'symbol) "")))
    ;; (devdocs-lookup nil word) ⇒ Quits abruptly when keyword is not a
    ;; valid candidate!
    (minibuffer-with-setup-hook
        `(lambda () (insert ,word))
      (call-interactively #'devdocs-lookup))))

(defun repl-driven-development--make-repl-function-docstring (repl)
  "Make the docstring for a REPL function working with command CLI."
  (let ((keys (rdd@ repl keybinding))
        (cmd (rdd@ repl cmd)))
    (setq repl (rdd@ repl fun-name))
    (lf-string
     "Executes the selected region, if any or otherwise the entire current line,
    and evaluates it with the command-line tool “${repl}”.

    Output is shown as an overlay at the current cursor position.
    It is shown for `repl-driven-development-echo-duration' many seconds.

    ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
                             ﴾ Familiar Workflow ﴿

    You can execute arbitrary Lisp anywhere by pressing “C-x C-e”, you can
    insert the result with “C-u C-x C-e”, and see the output echoed in the
    mode-line and in the *Messages* buffer with “C-h e”.
    Likewise, you can execute “${cmd}” code by pressing “${keys}”, insert output
    with “C-u ${keys}”, and see the output echoed near your cursor and in the
    *Messages* buffer with “C-h e”.

    ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
                       ﴾ C-u ${keys}  ≈  Insert result ﴿

    With a “C-u” prefix, the output is inserted at point
    (and not echoed in an overlay).

    Since ${repl} may pretty-print its output, inserting it may result in
    non-executable code. If you want executable code, you must specify
    how pretty-printed output must be converted into ${repl}-executable code.
    Do so by redefining `${repl}-read'.

    ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
          ﴾ C-u 0 ${keys}   ≈  `${repl}-display-most-recent-result' ﴿

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
    From Lisp, consider using `${repl}-submit'.

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'repl-driven-development)

;;; repl-driven-development.el ends here

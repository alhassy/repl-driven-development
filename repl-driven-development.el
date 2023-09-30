;;; repl-driven-development.el --- Send arbitrary code to a REPL in the background  -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 1.0.4
;; Package-Requires: ((s "1.12.0") (dash "2.16.0") (eros "0.1.0") (bind-key "2.4.1") (emacs "27.1") (f "0.20.0") (devdocs "0.5") (pulsar "1.0.1"))
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
;;
;; Minimal Working Example [Java]:
;;
;;   ;; Set “C-x C-j” to evaluate Java code in a background REPL.
;;   (repl-driven-development [C-x C-j] "jshell" :prompt "jshell>")
;;
;;   // Select this Java snippet, then press “C-x C-j” to evaluate it
;;   import javax.swing.*;
;;   var frame = new JFrame(){{ setAlwaysOnTop(true); }};
;;   JOptionPane.showMessageDialog(frame, "Super nice!");
;;
;;   // REPL result values are shown as overlays:
;;   2 + 4 // ⇒ 6
;;
;;
;; Benefits:
;;
;; Whenever reading/refactoring some code, if you can make some of it
;; self-contained, then you can immediately try it out! No need to
;; load your entire program; nor copy-paste into an external REPL. The
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
;; This file has been tangled from a literate, org-mode, file.

;;; Code:

;; String and list manipulation libraries
;; https://github.com/magnars/dash.el
;; https://github.com/magnars/s.el

;; TODO: Clean up code ---i.e., address various TODOs.
;; TODO: Make a mini-tutorial and place it in the pkg docs.
;; TODO: Implement Read Protocol for Java.
;; TODO[Low Priority]: Implement pretty printing for Python.
;; TODO[Low Priority]: Implement a simple Read Protocol for JS. (e.g., JSON.stringify({}, null, 2))

(when nil ⨾⨾ Rich Comment consisting of executable code to try things out.

      (eval-buffer)
      ;; Nearly instantaneous display of tooltips.
      (setq tooltip-delay 0)

      (repl-driven-development [C-x C-t] "bash" :blink 'pulsar-green)
      echo "It is $(date) and I am at $(pwd), my name is $(whoami) and I have: \n $(ls)"
      ;; We can also restart the repl... let's set some state
      export X=123
      echo $X
      ;; Now restart it with C-u -1 C-x C-t
      echo $X
      ;; C-x C-t on the above line emits no value

      ;; Init code works upon initialisation, neato!
      (repl-driven-development [C-x C-t] "bash" :init "echo $(fortune)")

      ;; We can get rid of the prompt at the end with
      (repl-driven-development [C-x C-t] "bash" :init "echo $(fortune)" :prompt "^[^ ]*\\$")

      (repl-driven-development [C-x C-n] "node" :blink 'pulsar-blue)
      [...Array(14).keys()].map(x => x % 3 == 0 ? "Fizz" : x)
      ;; Change colour with C-x C-e, then C-x C-n on the line after.
      (setf (rdd@ "node" blink) 'pulsar-green)
      Object.keys({name: "mikle", 1: "one"})

      ;; Notice associated buffer's name involves only the command "jshell", not the args.
      ;; See it via C-u 0 C-x C-j.
      (repl-driven-development [C-x C-j] java)
      IntStream.range(0, 23).forEach(x -> System.out.println(x))

      ;; Select the following 6 lines, then submit this region with C-x C-j
      ;; IntStream
      ;; /* a multi-line
      ;; * comment */
      ;; .range(0, 23)
      ;; // Now print it out
      ;; .forEach(x -> System.out.println(x))

      ;; Likewise JS
      (repl-driven-development [C-x C-n] javascript)
      ;; [...Array(40).keys()]
      ;; // yay, a comment in the middle
      ;; .map(x => x % 3 == 0 ? "Fizz" : x)

      (repl-driven-development  [C-x C-p] "python3" :prompt ">>>"
                                :input-rewrite-fn (lambda (in)
                                                    ;; Remove empty lines: In the middle of a def|class, they abruptly terminate the def|class!
                                                    (concat (s-replace-regexp "^\s*\n" "" in) "\n\n\r"))
                                :echo-rewrite-fn (lambda (echo)
                                                   (-let [result (s-chop-prefix (rdd@ "python3" current-input) echo)]
                                                     ;; Default python repl emits nothing on def|class declarations, let's change that.
                                                     (cond ((s-starts-with? "def" (rdd@ "python3" current-input))
                                                            (s-replace-regexp " *def \\([^(]*\\).*" "Defined “\\1”"
                                                                              (rdd@ "python3" current-input)))
                                                           ((s-starts-with? "class" (rdd@ "python3" current-input))
                                                            (s-replace-regexp " *class \\([^(:]*\\).*" "Defined “\\1”"
                                                                              (rdd@ "python3" current-input)))
                                                           (t result)
                                                           ))))
      ;; Send each line, one at a time.
      1 + 2 * 3
      def foo(x): return x*x
      foo(5)
      list(map(lambda i: 'Fizz'*(not i%3)+'Buzz'*(not i%5) or i, range(1,101)))
      ;; (Above shows Result Truncated due to my use of eros, which has this limit. TODO[Low Priority]: Fix this.)

      ;; We can do multi-line def ---MA: The quotes are to allow me to indent, otherwise my aggressive-formatter strips the whitespace away.
      "
def square(x):
   return x * x
"
      square(5)

      ;; Likewise for class-es:
      "
          class AMyClass():
              i = 12345

              def f(self):
                  return 'hello world'
"

      x = AMyClass()
      x.i
      x.f()

      Notice that the code is identend nicely.
      )

(require 's)               ;; “The long lost Emacs string manipulation library”
(require 'dash)            ;; “A modern list library for Emacs”
(require 'cl-lib)          ;; New Common Lisp library; ‘cl-???’ forms.
(require 'eros)            ;; Simple Emacs Overlays
(require 'bind-key)        ;; Bind keys

(defconst repl-driven-development-version (package-get-version))
(defun repl-driven-development-version ()
  "Print the current repl-driven-development version in the minibuffer."
  (interactive)
  (message repl-driven-development-version))

(defmacro rdd@ (cmd property)
  "Get/set PROPERTY under namespace CMD.

      Usage:
      (rdd@ \"foo\" name)                ;; ⇒ nil
      (setf (rdd@ \"foo\" name) 'Jasim)
      (rdd@ \"foo\" name)                ;; ⇒ 'Jasim"
      `(get (intern (format "repl/%s" ,cmd)) (quote ,property)))

;;;###autoload
(cl-defmacro repl-driven-development (keys cli &key (prompt ">") docs (init "") (blink ''pulsar-yellow) (input-rewrite-fn ''identity) (echo-rewrite-fn ''identity))
  "Make Emacs itself a REPL for your given language of choice.

  Suppose you're exploring a Python/Ruby/Java/JS/TS/Haskell/Lisps/etc
  API, or experimenting with an idea and want immediate feedback.
  You could open a terminal and try things out there; with no editor
  support, and occasionally copy-pasting things back into your editor
  for future use. Better yet, why not use your editor itself as a REPL.

  Implementation & behavioural notes can be found in the JavaScript
  Example below.

  ######################################################################
  ### JavaScript Example ---Basic usage, and a minimal server ##########
  ######################################################################

     ;; C-x C-j now evaluates arbitrary JavaScript code
     (repl-driven-development [C-x C-j] \"node\")

  That's it! Press “C-x C-e” on the above line so that “C-x C-j”
  will now evaluate a selection, or the entire line, as if it were
  JavaScript code. ⟦Why C-x C-j? C-x C-“e” for Emacs Lisp code, and C-x
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
  C-x C-j sends text to that process. Whenever the process emits
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

  - CLI [String]: A string denoting the terminal command to start your repl;
    you may need an “-i” flag to force it to be interactive even though
    we use it from a child process rather than a top-level shell.

  - PROMPT [Regular Expression]:
    What is the prompt that your REPL shows, e.g., “>”.
    We try to ignore showing it in an overlay that would otherwise hide
    useful output.

  - DOCS [String]: A space-seperated string denoting a list of language documents
    you'd like to associate with your repl.
    Invoking your repl with “C-u C-u” will show the documentation
    of the word at point. This is done using `devdocs'.

    For example,
      (repl-driven-development [C-x C-j] \"node\" :docs \"javascript express\")
    Would allow us to invoke “C-u C-u C-x C-j” with the cursor on the
    word, say, “listen” and we'll see some useful docs (along with
    example uses) of this Express library method “listen”.

    Visit https://devdocs.io/ to see the list of documented languages
    and libraries.

  - init [String | List<String>]: Any initial code you'd like your
    repl to be initiated with. For example, imports of standard libraries
    is probably something you'd always like to have on-hand; or perhaps
    some useful variables/declarations/functions.

  - BLINK [Face]: Any face with a background. It is used to briefly highlight
    the current line that is being sent to the REPL process.

  Finally, you may register callbacks via `repl-driven-development-output-hook'.

  ### Misc Remarks #####################################################
  VSCode has a similar utility for making in-editor REPLs, by the
  same author: http://alhassy.com/making-vscode-itself-a-java-repl
  "

  ;; TODO: Document that `cli` can be the unquoted symbols: java, python, terminal, javascript
  (-let [strip-out-C-style-comments&newlines  '(lambda (in) (thread-last in
                                                                      ;; strip out comments!
                                                                      (s-replace-regexp "/\\*.\\*/" "")
                                                                      (s-replace-regexp "//.*$" "")
                                                                      (s-replace-regexp "\n" "")
                                                                      ))]
    (pcase cli
      ('java       ;; JShell does semicolon insertion eagerly, so it things the following are three separate
      ;; expressions! We can fix this by removing new lines.
       `(repl-driven-development ,keys "jshell" :prompt "jshell>" :input-rewrite-fn ,strip-out-C-style-comments&newlines))
       ;; Likewise JS does eager semicolon insertion.
      ('javascript `(repl-driven-development ,keys "node" :prompt ">" :input-rewrite-fn ,strip-out-C-style-comments&newlines))
      ('terminal `(repl-driven-development ,keys "bash"    :prompt "^[^ ]*\\$"))
      ('python `(repl-driven-development   ,keys "python3" :prompt ">>>"))
      (_ `(-let* (((repl . args) (s-split " " ,cli)))
     ;; (repl-fun-name string)
     (setf (rdd@ repl cmd) repl) ;; String
     (setf (rdd@ repl prompt) ,prompt) ;; String (Regular Expression)
     (setf (rdd@ repl keybinding) ,keys) ;; String
     (setf (rdd@ repl docs) (s-join " " ,docs)) ;; String: Space separated list
     ;; Used to avoid scenarios where input is echoed thereby accidentally treating it as a repl output
     (setf (rdd@ repl current-input) "") ;; String
     (setf (rdd@ repl current-input/start) 0)
     (setf (rdd@ repl current-input/end) 0)

     ;; TODO: Document these two
     (setf (rdd@ repl input-rewrite-fn) ,input-rewrite-fn)
     (setf (rdd@ repl echo-rewrite-fn) ,echo-rewrite-fn) ;; Intentionally meant for human friendly pretty-printing, not for the READ protocol. Those serve different goals. Document this. By default, the READ protocol should be this echo-rewrite-fn.

     (setf (rdd@ repl init) ,init)
     (cl-assert (or (stringp ,init) (listp ,init)))
     (when (listp ,init) (setq ,init (s-join "\n" ,init)))
     (cl-assert (stringp ,init))

     (setf (rdd@ repl blink) ,blink)
     ;; Identifier "repl-driven-development" is made unique by start-process.
     (setf (rdd@ repl process)
           (apply #'start-process "repl-driven-development"
                  (format "*REPL/%s*" repl) repl args))
     ;; https://stackoverflow.com/q/4120054
     ;; (set-process-coding-system repl-process 'unix)
     (with-current-buffer (process-buffer (rdd@ repl process))
       (setq buffer-display-table (make-display-table))
       (aset buffer-display-table ?\^M [])
       (setq buffer-read-only t))

     (setq docs (rdd---install-any-not-yet-installed-docs ,docs))
     (eval (rdd---make-repl-function repl))

     (process-send-string (rdd@ repl process) ,init)
     (process-send-string (rdd@ repl process) "\n")

     ;; Callback: Write the actual output to the REPL buffer and emit overlay.
     (set-process-filter (rdd@ repl process) (rdd---main-callback (intern repl)))

     ;; Return the REPL process to the user.
     (rdd@ repl process))))))

;; TODO. (use-package erefactor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rdd---main-callback (repl)
  `(lambda (process output)

     ;; The *REPL* buffer shows things exactly as they'd look like
     ;; in a standard interaction in the terminal.
     (rdd---insertion-filter process output)

     ;; This is done to provide a richer, friendlier, interaction.
     ;; ^M at the end of line in Emacs is indicating a carriage return (\r) followed by a line feed (\n).
     (setq output (s-trim (s-replace-regexp ,(rdd@ repl prompt) "" (s-replace "\r\n" "" output))))

     ;; thread `output' through output hooks
     ;; i.e., run all hooks on REPL output, each possibly modifying output
     (require 'cl)
     (cl-loop for fun in repl-driven-development/output-hook
              do (setq output (funcall fun output)))

     (rdd---insert-or-echo (quote ,repl) output)))

(defun rdd---install-any-not-yet-installed-docs (docs)
  "Install any not-yet-installed docs; returns a List<String> of the intalled docs."
  (when docs
    (require 'devdocs)
    (cl-assert (stringp docs))
    (setq docs (--reject (s-blank? it) (s-split " " docs)))
    (cl-assert (listp docs))
    (-let [installed (mapcar #'f-base (f-entries devdocs-data-dir))]
      (--map (unless (member it installed) (devdocs-install (list (cons 'slug it)))) docs))
    docs)

  (defun rdd---insert-or-echo (repl output)
    "If there's a C-u, then insert the output; else echo it in overlay"
    (cl-assert (stringp output))
    (pcase current-prefix-arg
      ('(4) (unless (equal output (s-trim (rdd@ repl current-input)))
              (insert " " (funcall (intern (format "repl/%s/read" (rdd@ repl cmd))) output)))) ;; (funcall (intern "repl/node/read") "hola")
      ;; All other prefixes are handled by repl-fun-name, above.
      (_
       ;; Show output as an overlay at the current cursor position
       ;; ﴾ Since eros is intended to be used with ELisp, not arbitrary langs,
       ;; it does some sexp look-about, which may not mix well with, say, JS
       ;; arrow functions, so we freeze such movements, locally. ﴿
       (setq output (apply (rdd@ repl echo-rewrite-fn) (list (rdd---ignore-ansi-color-codes output))))
       (unless (s-blank? (s-trim output))
         ;; TODO [Optimisation]: Consider inlining, since I have the region boundaries already! (from the repl calling function!)
         (unless  (equal output (s-trim (rdd@ repl current-input)))
           ;; MA: Not sure why, but the following line cause the top-most rdd call to stall.
           ;; To avoid the stall, I use a -let.
           (mapcar #'delete-overlay (overlays-at (rdd@ repl current-input/start)))
           (let ((overlay (make-overlay (rdd@ repl current-input/start) (rdd@ repl current-input/end))))
             (overlay-put overlay 'help-echo output))))
       (thread-yield)
       (require 'eros)
       (cl-letf (((symbol-function 'backward-sexp) (lambda (&rest _) 0)))
         (eros--make-result-overlay output
           :format  " ⮕ %s"
           :duration repl-driven-development/echo-duration))))))

(defvar repl-driven-development--insert-into-repl-buffer t)

(defun rdd---make-repl-function (repl)
  "Constructs code denoting a function that sends a region to a REPL process"

  (-let* ((repl-fun-name (intern (concat "repl/" (rdd@ repl cmd)))))
    `(progn
       ;; TODO: Make a defun with a callback for repl testing a la set-process-filter.
       ;; (format "%s/jump-to-process-buffer" repl-fun-name)
       ;; (format "%s/restart" repl-fun-name)
       ;; (format "%s/docs-at-point" repl-fun-name)
       ;; (format "%s/submit" repl-fun-name)

       (defun ,(intern (format "%s/jump-to-process-buffer" repl-fun-name)) ()
         "Toggle to the buffer associated with this REPL process; see a log of your submissions.

Invoke once to go to the REPL buffer; invoke again to jump back to your original buffer."
         (interactive)
         (if (equal (current-buffer) (process-buffer (rdd@ ,repl process)))
             (switch-to-buffer (get (quote ,(intern (format "%s/jump-to-process-buffer" repl-fun-name))) 'location))
           (setf (get (quote ,(intern (format "%s/jump-to-process-buffer" repl-fun-name))) 'location) (current-buffer))
           (switch-to-buffer (process-buffer (rdd@ ,repl process)))))

       ;; restart repl, [then send to repl --does not work since REPLs take a sec to load. That's OK, not a deal-breaker!]
       (defun ,(intern (format "%s/restart" repl-fun-name)) ()
         "Restart the REPL process."
         (interactive)
         (kill-buffer (process-buffer (rdd@ ,repl process)))
         (repl-driven-development (rdd@ ,repl keybinding)
                                  (rdd@ ,repl cmd)
                                  :prompt (rdd@ ,repl prompt)
                                  :docs (rdd@ ,repl docs)
                                  :init (rdd@ ,repl init)
                                  :blink (rdd@ ,repl blink)))

       (defun ,(intern (format "%s/docs-at-point" repl-fun-name)) ()
         "Documentation at point."
         (interactive)
         (rdd---docs-at-point (quote ,(rdd@ repl docs))))

       (defun ,(intern (format "%s/read" repl-fun-name)) (str)
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

       (defun ,(intern (format "%s/submit" repl-fun-name)) (str)
         ,(format "Send STR to the REPL process, followed by a newline.

To submit a region, use `%s'." repl-fun-name)
         (setf (rdd@ ,repl current-input) str)
         (process-send-string (rdd@ ,repl process) (apply (rdd@ ,repl input-rewrite-fn) (list str)))
         (process-send-string (rdd@ ,repl process) "\n"))

       ;; TODO: Replace rdd---current-input with a symbolic property 'current-input that lives under symbol 'repl/𝑳𝑨𝑵𝑮.
       ;; Then make a method (repl/𝑳𝑨𝑵𝑮/current-input &optional new-value) to easily get/set this thing.
       ;; WHY? So that the current-input is namespaced for distinct repl and not globally shared.

       (bind-key* (s-join " " (mapcar #'pp-to-string (rdd@ ,repl keybinding)))
                  (defun ,repl-fun-name (region-beg region-end)
                    ,(rdd---make-repl-function-docstring (rdd@ repl cmd) "")
                    (interactive "r")

                    (require 'pulsar)
                    (-let [pulsar-face (rdd@ ,repl blink)]
                      (pulsar-mode +1)
                      (pulsar-pulse-line))

                    (pcase current-prefix-arg
                      (0  (,(intern (format "%s/jump-to-process-buffer" repl-fun-name))))
                      (-1 (,(intern (format "%s/restart" repl-fun-name))))
                      ;; ('(4)  (insert " " output)) ;; C-u ;; handled when we actually have the output; see the process filter below
                      ('(16) ;; C-u C-u ⇒ documentation lookup
                       (,(intern (format "%s/docs-at-point" repl-fun-name))))
                      (_
                       (if (use-region-p)
                           (deactivate-mark)
                         (beginning-of-line)
                         (setq region-beg (point))
                         (end-of-line)
                         (setq region-end (point)))
                       (setf (rdd@ ,repl current-input/start) region-beg)
                       (setf (rdd@ ,repl current-input/end) region-end)
                       ;; TODO: Need to make this newline deletion a toggle, otherwise I suspect issues with python!
                       (,(intern (format "%s/submit" repl-fun-name))
                        ;; TODO: s-replace-regexp "\n" ""
                        (s-trim-left (buffer-substring-no-properties region-beg region-end))))))))))

(defun rdd---docs-at-point (docs)
  ;; Test this by writing a word such as “IntStream.range(0, 44)” then M-: (rdd---docs-at-point '("openjdk~19"))
  ;; anywhere on the phrase

  ;; devdocs-lookup will ask to setup current docs when there's a current-prefix, so we null it.
  ;; If user does have it setup, we want to temporarily change its value for use with the current repl.
  (let ((devdocs-history nil) (current-prefix-arg nil) (devdocs-current-docs docs) (word (or (thing-at-point 'symbol) "")))
    ;; (devdocs-lookup nil word) ⇒ Quits abruptly when keyword is not a valid candidate!
    (minibuffer-with-setup-hook
        `(lambda () (insert ,word))
      (call-interactively #'devdocs-lookup))))

;; TODO: Add docs about *REPL* buffer, its purpose, and alternatives
(cl-defmethod rdd---make-repl-function-docstring ((cli string) (additional-remarks string))
  "Makes the docstring for a repl function working with command CLI."
  (s-replace-regexp "^\s+" ""
                    (format
                     "Executes the selected region, if any or otherwise the entire current line,
    and evaluates it with the command-line tool “%s”.

    Output is shown as an overlay at the current cursor position.
    It is shown for `repl-driven-development/echo-duration' many seconds.

    ##### C-u Prefix: Insert result

    With a “C-u” prefix, the output is inserted at point
    (and not echoed in an overlay).

    Since %s may pretty-print its output, inserting it may result in
    non-executable code. If you want executable code, you must specify
    how pretty-printed output must be converted into %s-executable code.
    Do so by redefining `%s'.

    ##### C-u C-u Prefix: Documentation via `%s'

    With a “C-u C-u” prefix, documentation is looked-up for the word at point.

    This is done using `devdocs', and so the documentation generally provides
    example uses as well. Visit https://devdocs.io/ to see the list of documented
    languages and libraries.

    ##### “C-u 0” Prefix: See associated buffer via `%s'

    Sometimes it may be useful to look at a large output in a dedicated buffer.
    However, the output of a command is also attached to the input via a tooltip:
    Hover to see it! See also `tooltip-delay'.

    ##### “C-u -1” Prefix: Restart REPL via `%s'

    In the event you've messed-up your REPL, starting from a blank slate may be
    helpful.

    ##### Implementation Notes

    The interactive method is asynchronous: Whenever you send text for evaluation,
    you immediately regain control in Emacs; you may send more text and it will be
    queued for evaluation. For example, evaluating a sleep command for 3 seconds
    does not block Emacs.

    From Lisp, consider using `%s'.

    ##### See also

    See `repl-driven-development' for more useful docs.

    See www.alhassy.com/repl-driven-development to learn more about RDD and see
    examples and many gifs.
"
                     cli
                     cli
                     cli
                     (format "repl/%s/read" cli)
                     (format "repl/%s/docs-at-point" cli)
                     (format "repl/%s/jump-to-process-buffer" cli)
                     (format "repl/%s/restart" cli)
                     (format "repl/%s/submit" cli))))

(defun repl-driven-development--santise-output (output prompt input)
  "Remove PROMPT from OUTPUT, and ensure OUTPUT does not contain a copy of INPUT."
  (setq output (s-trim (s-replace "\r" "" (s-replace-regexp prompt "" output))))
  (-let [no-input-echo (s-trim (s-chop-prefix input output))]
    (if (s-blank? (s-trim (s-collapse-whitespace no-input-echo))) output no-input-echo)))

(defvar repl-driven-development/output-hook nil
  "A list of functions to execute after REPL output has been computed.

Each function consumes a single argument: The output result, as a string.

For example:

     ;; I'd like “C-h e” to show eval result ---just as “C-x C-e” does.
     (add-hook 'repl-driven-development/output-hook
               (lambda (output)
                (let ((inhibit-message t))
                  (message \"REPL⇒ %s\" output))
                output))
")

(defun rdd---ignore-ansi-color-codes (string-with-codes)
  "Ignore ANSI color codes in a string"
  (with-temp-buffer
    (insert string-with-codes)
    (ansi-color-apply-on-region (point-min) (point-max))
    (buffer-string)))

(defun rdd---insertion-filter (proc string)
  "Src: https://www.gnu.org/software/emacs/manual/html_node/elisp/Filter-Functions.html"
  (when (and repl-driven-development--insert-into-repl-buffer (buffer-live-p (process-buffer proc)))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
         (let (buffer-read-only)(insert (rdd---ignore-ansi-color-codes string))) ;; Main difference
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defvar repl-driven-development/echo-duration 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'repl-driven-development)

;;; repl-driven-development.el ends here

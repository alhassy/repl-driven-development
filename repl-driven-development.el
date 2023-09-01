;;; repl-driven-development.el --- Send arbitrary code to a REPL in the background  -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 1.0.1
;; Package-Requires: ((s "1.12.0") (dash "2.16.0") (eros "0.1.0") (bind-key "2.4.1") (emacs "27.1") (f "0.20.0") (devdocs "0.5") (pulsar "1.0.1"))
;; Keywords: repl-driven-development, rdd, repl, lisp, java, python, ruby, programming, convenience
;; Repo: https://github.com/alhassy/repl-driven-development
;; Homepage: https://alhassy.com/repl-driven-development

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

(require 's)               ;; “The long lost Emacs string manipulation library”
(require 'dash)            ;; “A modern list library for Emacs”
(require 'cl-lib)          ;; New Common Lisp library; ‘cl-???’ forms.
(require 'eros)            ;; Simple Emacs Overlays
(require 'org)
(require 'bind-key)

(defconst repl-driven-development-version (package-get-version))
(defun repl-driven-development-version ()
  "Print the current repl-driven-development version in the minibuffer."
  (interactive)
  (message repl-driven-development-version))

(defvar rdd---current-input nil
  "Used to avoid scenarios where input is echoed thereby accidentally treating it as a repl output.")

(defvar rdd---current-output nil
  "The output of the most recent repl call; this is used for testing.")

  ;;;###autoload
(cl-defun repl-driven-development (keys cli &key (prompt ">") docs (prologue ""))
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

  - PROLOGUE [String | List<String>]: Any initial code you'd like your
    repl to be initiated with. For example, imports of standard libraries
    is probably something you'd always like to have on-hand; or perhaps
    some useful variables/declarations/functions.

  Finally, you may register callbacks via `repl-driven-development-output-hook'.

  ### Misc Remarks #####################################################
  VSCode has a similar utility for making in-editor REPLs, by the
  same author: http://alhassy.com/making-vscode-itself-a-java-repl
  "
    (cl-assert (or (stringp prologue) (listp prologue)))
    (when (listp prologue) (setq prologue (s-join "\n" prologue)))
    (cl-assert (stringp prologue))
    (-let* (((cmd . args) (s-split " " cli))
            ;; Identifier "repl-driven-development" is made unique
            ;; by start-process.
            (repl (apply #'start-process "repl-driven-development"
                         (format "*REPL/%s*" cli) cmd args)))

      ;; https://stackoverflow.com/q/4120054
      ;; (set-process-coding-system repl 'unix)
      (with-current-buffer  (format "*REPL/%s*" cli)
        (setq buffer-display-table (make-display-table))
        (aset buffer-display-table ?\^M [])
        (setq buffer-read-only t))

     (setq docs (rdd---install-any-not-yet-installed-docs docs))
     (eval `(rdd---make-repl-function ,repl ,keys ,cmd ,docs
         (repl-driven-development ,keys ,cli :prompt ,prompt :docs ,(s-join " " docs) :prologue ,prologue)))

     (process-send-string repl prologue)
     (process-send-string repl "\n")

     ;; Callback: Write the actual output to the REPL buffer and emit overlay.
     (set-process-filter repl (rdd---main-callback prompt))

     ;; Return the REPL process to the user.
     repl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rdd---main-callback (prompt)
 `(lambda (process output)

           ;; The *REPL* buffer shows things exactly as they'd look like
           ;; in a standard interaction in the terminal.
           (rdd---insertion-filter process output)

           ;; This is done to provide a richer, friendlier, interaction.
           ;; ^M at the end of line in Emacs is indicating a carriage return (\r) followed by a line feed (\n).
           (setq output (s-trim (s-replace-regexp ,prompt "" (s-replace "\r\n" "" output))))

           ;; thread `output' through output hooks
           ;; i.e., run all hooks on REPL output, each possibly modifying output
           (require 'cl)
           (cl-loop for fun in repl-driven-development/output-hook
                    do (setq output (funcall fun output)))

           (rdd---insert-or-echo output)))

(defun rdd---install-any-not-yet-installed-docs (docs)
  "Install any not-yet-installed docs; returns a List<String> of the intalled docs."
  (when docs
    (require 'devdocs)
    (cl-assert (stringp docs))
    (setq docs (--reject (s-blank? it) (s-split " " docs)))
    (cl-assert (listp docs))
    (-let [installed (mapcar #'f-base (f-entries devdocs-data-dir))]
      (--map (unless (member it installed) (devdocs-install (list (cons 'slug it)))) docs))
    docs))

(defun rdd---insert-or-echo (output)
  "If there's a C-u, then insert the output; else echo it in overlay"
  (cl-assert (stringp output))
  (pcase current-prefix-arg
    ('(4) (unless (equal output (s-trim rdd---current-input)) (insert " " output)))
    ;; All other prefixes are handled by repl-fun-name, above.
    (_
     ;; Show output as an overlay at the current cursor position
     ;; ﴾ Since eros is intended to be used with ELisp, not arbitrary langs,
     ;; it does some sexp look-about, which may not mix well with, say, JS
     ;; arrow functions, so we freeze such movements, locally. ﴿
     (setq output (rdd---ignore-ansi-color-codes output))
     (unless (s-blank? (s-trim output))
       (setq repl-driven-development-current--output output)
       (thread-yield)
       (require 'eros)
       (cl-letf (((symbol-function 'backward-sexp) (lambda (&rest _) 0)))
         (eros--make-result-overlay output
           :format  " ⮕ %s"
           :duration repl-driven-development/echo-duration)))))
  )

(defvar repl-driven-development--insert-into-repl-buffer t)

;; (fmakunbound #'repl-driven-development--make-repl-function)
(defmacro rdd---make-repl-function (repl keys cmd docs incantation-to-restart-repl)
 ;; cl-defmethod repl-driven-development--make-repl-function ((repl process) (cli string) (repl-fun-name string) (docs list))
  "Constructs code denoting a function that sends a region to a REPL process"
  (-let* ((repl-fun-name (intern (concat "repl/" cmd))))
      `(progn
    ;; TODO: Consider deleting this and setting the callback for repl testing directly a la set-process-filter.
    (defun ,(intern (format "%s/sync" repl-fun-name)) (string)
     "Block until we see the snetiantial marker; then emit the repl output. This is an sync call to the repl."
     (thread-join (make-thread `(lambda ()
       (setq DONE (format "\"DONE TEST %s\"" (gensym)))
       ;; (process-send-string jshell (format "Thread.sleep(3000)\n1 + 9\n%s\n" DONE))
       (process-send-string ,,repl (format "%s\n%s\n" ,string DONE))
       (setq my/threshold 0)
       (setq results nil)
       (setq waiting-seconds .5) ;; half a second
       (loop
        (sleep-for .01)
        (incf my/threshold)
        (push repl-driven-development-current--output results)
        (when (or (< 1000 (* my/threshold waiting-seconds)) (s-matches? DONE repl-driven-development-current--output))
          (return)))
       (thread-yield)
       (cadr (-uniq results))))))

 (bind-key* (s-join " " (mapcar #'pp-to-string ,keys))
  (defun ,repl-fun-name (region-beg region-end)
    ,(rdd---make-repl-function-docstring cmd "")
    (interactive "r")

    (require 'pulsar)
    (setq pulsar-face 'pulsar-yellow)
    (pulsar-mode +1)
    (pulsar-pulse-line)

    (pcase current-prefix-arg
      ;; 0 ⇒ Jump to repl [TODO: Add a  keybinding for “C-u 0 C-x C-j” to return to original position.]
      (0 (switch-to-buffer (--> (buffer-list) (--map (buffer-name it) it) (--filter (s-starts-with? "*REPL/jshell" it) it) car)))
      (-1
       ;; restart repl, [then send to repl --does not work since REPLs take a sec to load. That's OK, not a deal-breaker!]
         (kill-buffer (process-buffer ,repl))
         ,incantation-to-restart-repl)
      ;; ('(4)  (insert " " output)) ;; C-u ;; handled when we actually have the output; see the process filter below
      ('(16) ;; C-u C-u ⇒ documentation lookup
       (rdd---docs-at-point (quote ,docs)))
      (_
       (if (use-region-p)
           (deactivate-mark)
         (beginning-of-line)
         (setq region-beg (point))
         (end-of-line)
         (setq region-end (point)))
       (setq rdd---current-input (s-trim-left (buffer-substring-no-properties region-beg region-end)))
       (process-send-string ,repl rdd---current-input)
       (process-send-string ,repl "\n")
       ))
    )))))

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

    ## C-u Prefix: Insert result ###################################################

    With a “C-u” prefix, the output is inserted at point
    (and not echoed in an overlay).

    ## C-u C-u Prefix: Documentation ##############################################

    With a “C-u C-u” prefix, documentation is looked-up for the word at point.

    This is done using `devdocs', and so the documentation generally provides
    example uses as well. Visit https://devdocs.io/ to see the list of documented
    languages and libraries.

    ## “C-u 0” Prefix: See associated buffer #####################################

    Sometimes it may be useful to look at a large output in a dedicated buffer.

    ## “C-u -1” Prefix: Restart REPL #############################################

    In the event you've messed-up your REPL, starting from a blank slate may be
    helpful.

    ## Implementation Notes ########################################################

    The interactive method is asynchronous: Whenever you send text for evaluation,
    you immediately regain control in Emacs; you may send more text and it will be
    queued for evaluation. For example, evaluating a sleep command for 3 seconds
    does not block Emacs.

    ## See also ####################################################################

    See `repl-driven-development' for more useful docs.

    See www.alhassy.com/repl-driven-development to learn more about RDD and see
    examples and many gifs.
"
   cli
   )))

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

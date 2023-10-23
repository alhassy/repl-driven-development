(when nil ⨾⨾ Rich Comment consisting of executable code to try things out.

      ⨾⨾ Testing setup ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾

      (load-file "./testing-setup.el") ;; See my init.el
      (eval-buffer "repl-driven-development.el")
      ;; Style errors, package errors
      (my/show-errors)
      ;; Byte-compiles the file with all warnings enabled.
      (elisp-lint--byte-compile  (buffer-file-name))
      ;; Show me references to unbound symbols
      (elint-current-buffer)
      (my/load-file-in-new-emacs)
      (progn (outshine-mode) (outline-minor-mode))

      ⨾⨾ A simple terminal REPL works as expected ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾

      (repl-driven-development [C-x C-t] "bash" :blink 'pulsar-green)
      echo -e "$(whoami): On $(date), I sit at $(pwd) pondering ... \n $(ls)"

      ;; Nice docs
      (documentation #'bash-eval)

      ;; The output is echoed via an overlay; however on the source to see it
      ;; in a tooltip; invoke C-h e to see it in the *Messages* buffer;
      ;; Or see it in its own buffer with M-x ...
      (bash-eval-display-output) ;; i.e., (rdd@ "bash" output)

      ;; Insert the result of the above shell command with C-u C-x C-t.

      ;; We can also restart the repl... let's set some state
      export X=123
      echo $X
      ;; Now restart it with C-u -1 C-x C-t
      echo $X
      ;; C-x C-t on the above line emits no value

      ;; Init code works upon initialisation, neato!
      (repl-driven-development [C-x C-t] "bash" :init "echo $(fortune)")

      ;; We can get rid of the prompt at the end with :prompt
      (repl-driven-development [C-x C-t] "bash" :prompt "^bash.*?\\$")
      pwd
      echo "bye$"

      (repl-driven-development [C-x C-t] terminal)

      ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
      ;; We can change the blinking colours via rdd@.
      (repl-driven-development [C-x C-n] "node" :blink 'pulsar-blue)
      [...Array(14).keys()].map(x => x % 3 == 0 ? "Fizz" : x)
      ;; Change colour with C-x C-e, then C-x C-n on the line after.
      (setf (rdd@ "node" blink) 'pulsar-green)
      Object.keys({name: "mikle", 1: "one"})

      ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
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

      ;; There is no repetation of input via output
      ;; https://github.com/alhassy/repl-driven-development/issues/5
      System.out.println("Hi")

      IntStream.range(0, 40) .mapToObj(i -> i % 15 == 0 ? "FizzBuzz" : i % 3 == 0 ? "Fizz" : i % 5 == 0 ? "Buzz" : String.valueOf(i)).toList()

      // Let's make a type to model stuff we're working with
      record Person(String name, int age) { }

      // Let's write a super duper complex algorithm
      List<Person> foo(String... names) { return Arrays.stream(names).map(n -> new Person(n, n.length())).toList(); }

      // Let's run our algorithm and get *executable* outputs that can then be used for regression tests
      foo("musa", "hamid") // C-x C-j shows me “human readable” results
      // C-u C-x C-j shows me “java readable” code that can be used for regression tests
      foo("musa", "hamid")
      // ⇒ List.of(new Person("musa", 4), new Person("hamid", 5))


      ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
      ;; Likewise for NodeJS
      (repl-driven-development [C-x C-n] javascript)
      ;; Then submit:
      [...Array(40).keys()]
      // yay, a comment in the middle
      .map(x => x % 3 == 0 ? "Fizz" : x)

      ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
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
      ;; Notice that the code is indented nicely.
      )


(cl-flet ((java-read (str)
            (thread-last
              str
              repl-driven-development--parse-pretty-printed-java
              repl-driven-development--lisp-to-java)))
  (require 'ert)
  ;; Non-record *values* are read literally
  (should (equal (java-read "123") "123"))
  ;; Records with a single field become constructor calls on that field: As a number (if possible), otherwise as a string.
  (should (equal (java-read  "Person[name=Jaafar]") "new Person(\"Jaafar\")"))
  (should (equal (java-read  "Person[age=31.2]") "new Person(31.2)"))
  ;; Record payloads can include spaces and commas
  (should (equal (java-read "Person[name=AsSaddiq, Jaafar, the first, age=thirty and one, years=31]")
                 "new Person(\"AsSaddiq, Jaafar, the first\", \"thirty and one\", 31)"))
  ;; We can read nested records.
  ;; For arbitrarly deep nesting, we cannot use regular expressions, and so we need to move to using PEGs.
  (should (equal (java-read  "Person[name=Jaafar, child=Person[name=Yacoub]]") "new Person(\"Jaafar\", new Person(\"Yacoub\"))"))
  (should (equal (java-read  "Person[name=Jaafar, child=Person[name=Yacoub], child=Person[name=Jasim]]")
                 "new Person(\"Jaafar\", new Person(\"Yacoub\"), new Person(\"Jasim\"))"))
  (should (equal (java-read  "Person[name=Hamid, child=Person[name=Jaafar, child=Person[name=Yacoub]]]")
                 "new Person(\"Hamid\", new Person(\"Jaafar\", new Person(\"Yacoub\")))"))
  (should (equal (java-read "Person[name=hamid, child=Person[name=Jaafar, age=12]]") "new Person(\"hamid\", new Person(\"Jaafar\", 12))"))
  ;; We can read lists.
  (should (equal (java-read "[1, 2, 3]") "List.of(1, 2, 3)"))
  (should (equal (java-read "[]") "List.of()"))
  (should (equal (java-read "[Person[name=Jasim, age=72, zindex=0.5], Person[name=Kathy, age=82, zindex=2.78], Person[name=Jaafar, age=31, zindex=3]]")
                 "List.of(new Person(\"Jasim\", 72, 0.5), new Person(\"Kathy\", 82, 2.78), new Person(\"Jaafar\", 31, 3))"))
  (should (equal (java-read "[[1], [2, 3], [4, 5, 6]]") "List.of(List.of(1), List.of(2, 3), List.of(4, 5, 6))"))
  ;; We can mix the various structures (records & lists)
  ;; TODO: (java-read "Person[children=[]]")
  (should (equal (java-read "A[a=[B[b=[C[c=D[d=[E[e=hello]]]]]]]]")
                 "new A(List.of(new B(List.of(new C(new D(List.of(new E(\"hello\"))))))))"))
  ;; Null is known
  (should (equal (java-read "Person[name=null]") "new Person(null)"))
  ;; Maps are known
  (should (equal (java-read "{}") "Map.of()"))
  (should (equal (java-read "{a=1, b=2, c=3}") "Map.of(a, 1, b, 2, c, 3)"))
  (should (equal (java-read "{a=Alice, b=Bob, c=Kathy}") "Map.of(a, \"Alice\", b, \"Bob\", c, \"Kathy\")"))
  ;; TODO: Allow numbers as keys.
  ;; {1=hello, 2=world}
  ;; TODO: (java-read "Person[]")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: Document the `emacs' function in the javascript repl.
;;
;; emacs.eval(`user-full-name`)
;; emacs.eval(`(load-theme 'spacemacs-dark)`)
;; emacs.eval(`(load-theme 'spacemacs-light)`)
;; emacs.eval(`(message-box "hi")`)
;; // Multi-line also works fine
;; emacs.eval(`(progn
;; (message-box "Hello")
;; (message-box "World")
;; )`)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO: Add to main rdd.el macro: Mention connecting to JS Server via C-x C-t telnet.

;; Something to consider using
;;
;; Current bug: It makes C-x C-t insert unexpected text, the docstring of C-x C-t!

(defmacro rdd-defun (repl name args-list doc-string &rest body)
  "Defun NAME with ARGS_LIST and DOC-STRING and BODY, in the REPL namespace.

This is like `defun' but the source code, excluding DOC-STRING,
is appended to the function's docstring."
  ;; TODO: fun-name keyword of main rdd.el macro should be replace by a new :name keyword,
  ;; then fun-name is just “⟨name⟩-eval”, and derived methods are “⟨name⟩-eval-⟨derivation⟩”.
  (let* ( (name-sym (intern (format "%s" name)))
          (fun-name-sym (intern (format "%s" (rdd@ repl fun-name))))
          (command-name (if (equal name-sym fun-name-sym) name-sym (intern (format "%s-%s" fun-name-sym name-sym)))))
    `(progn
       (setf (rdd@ ,repl ,command-name)
             ;; restart repl, [then send to repl --does not work since REPLs take a sec
             ;; to load. That's OK, not a deal-breaker!]
             (-cons* 'defun (quote ,command-name) (quote ,args-list) (quote ,body)))

       (eval (rdd@ ,repl ,command-name))

       ;; TODO: Add to requires
       (require 'cl-extra)



       (put (quote ,command-name)
            'function-documentation
            (concat
             ,doc-string
             (format "\n\n\t\t﴾Source Code of “%s”﴿\n\n" (quote ,command-name))
             (with-temp-buffer (cl-prettyprint (rdd@ ,repl ,command-name)) (buffer-substring-no-properties (point-min) (point-max)))
             )))))
;; MA: For testing.
;; (insert (pp-to-string (macroexpand '(rdd-defun "bash" restart-NEW1 (x y) "6 hola amigos ~ Restart the REPL process." (interactive) 'BYE))))

;; Intended usage sites:

-     ;; restart repl, [then send to repl --does not work since REPLs take a sec
-     ;; to load. That's OK, not a deal-breaker!]
-     (defun ,(intern (format "%s-restart" (rdd@ repl fun-name))) ()
        "Restart the REPL process."
        (interactive)
        (kill-buffer (process-buffer (rdd@ ,repl process)))
        (repl-driven-development (rdd@ ,repl keybinding)
                                 (rdd@ ,repl cmd)
                                 :prompt (rdd@ ,repl prompt)
                                 :docs (rdd@ ,repl docs)
                                 :init (rdd@ ,repl init)
                                 :blink (rdd@ ,repl blink)))

;; and

(eval (rdd-defun
       ,repl
       ,(rdd@ repl fun-name)
       (region-beg region-end)
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
                         region-end))))))))))

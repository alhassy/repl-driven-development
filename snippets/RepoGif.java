/*
  Often, while reading a README file, we will (1) copy a shell command,
  (2) open a terminal, and (3) paste the shell command to run it.
  We can evaluate arbitrary regions in a shell in one step via “C-x C-t”
  with:

      ;; In Emacs, press “C-x C-e” to evaluate the following line.
      ;; This lets us use “C-x C-t” to send lines to a bash process.
      (repl-driven-development [C-x C-t] "bash")





































  For example, execute “C-x C-t” anywhere on the next line …

      echo "It is $(date) and I am at $PWD, my name is $(whoami) and I have: $(ls)"







































  ⇒ Results are overlayed next to my cursor, also in a tooltip, also in “C-h e”.
  ⇒ “C-h k C-x C-t” gives extensive help about this new “C-x C-t” command.
     ↣ For example, insert the result of the above shell command with “C-u C-x C-t”.







































 ★ This also works for any process, such as “bash, python3, jshell, nodejs, clojure,
    ghci, irb, spago, irb, npx ts-node”.







































 There are also “preconfigured REPLs” with helpful bells & whistels, such as “java”…

    ;; “C-x C-j” now evaluates arbitrary Java code ♥‿♥
    (repl-driven-development [C-x C-j] java)

For example,

    import javax.swing.*;
    JOptionPane.showMessageDialog(new JFrame(){{setAlwaysOnTop(true);}}, "Super nice!")







































*/

import java.util.*;

class RepoGif {







































    // Let's make a type to model stuff we're working with
    record Person(String name, int age) { }







































    // Let's write a super duper complex algorithm
    List<Person> foo(String... names) {
        return Arrays.stream(names).map(n -> new Person(n, n.length())).toList();
    }







































    void scratchpad() {

        // “C-x C-j” shows me “human readable” results
        foo("Hamid", "Jaafar");







































            // “C-u C-x C-j” shows me “java readable” code that can be used for regression tests
        foo("Hamid", "Jaafar");







































        // We can also interactively navigate large data-dumps…
        // …when overlay output is cramped
        foo("Hamid", "Jaafar", "Musa", "Montather");
    }
}














































/*
(setq frame-title-format "   Editor Integrated REPLs for all Languages    ᕦ( ᴼ ڡ ᴼ )ᕤ")
(defun doom-modeline-buffer-file-name ()  "⇒ www.alhassy.com/repl-driven-development ⇐")
(eglot-java-mode +1)          ;; Inline overlays of arg names, syntax checks, typechecking
(company-mode +1)             ;; Completions for methods
(flyspell-mode -1)            ;; Don't check my spelling
(rainbow-delimiters-mode +1)  ;; Colourful rainbow parens
(rainbow-identifiers-mode +1) ;; Name-based/semantic highlighting
(keycast-header-line-mode +1) ;; Show my keybindings
(bind-key "M-SPC" (cl-defun my/delete-vertical-space ()
    (interactive) (delete-all-space) (insert "\n\n") (indent-for-tab-command)  (c-electric-backspace 4)))
*/

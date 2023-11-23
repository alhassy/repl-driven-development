/**
 * Resulting Youtube Short :: https://www.youtube.com/watch?v=y1BA3GJsWEE
 *
 * I keep my code within a class and within a method, so that I can have access
 * to Eglot's IDE features for Java: Code completion, docs, argument labels,
 * tooltips, etc. In the main() method, I generally “C-M-SPC C-x C-n”, since
 * that's what I actually want to see / work with.
 *
 *** Gif Setup *****************************************************************
 (progn
  ;; Make the M-RET command insert “* ” when I'm in a JavaDoc
  (bind-key* "M-RET" #'c-indent-new-comment-line 'java-mode-map)
  ;; Auto-format on save;; For example, for Java, I need to:
  ;; brew install clang-format
  (format-all-mode +1) ;; or: M-x format-all
  ;; Don't prompt me which formatter to use; just use the defaults.
  ;; For example, for Java, I need to: brew install clang-format
  (setq format-all-formatters format-all-default-formatters)
  (setq frame-title-format
  "   Java: REPL Driven Development with  𝓔𝓶𝓪𝓬𝓼   ")
  (defun doom-modeline-buffer-file-name ()  "
        https://github.com/alhassy/repl-driven-development ")
  (eglot-java-mode +1) ;; Inline overlays of arg names, syntax/type checks
  (company-mode +1) ;; Completions for methods
  (flyspell-mode -1) ;; Don't check my spelling
  (rainbow-delimiters-mode +1)  ;; Colourful rainbow parens
  (rainbow-identifiers-mode +1) ;; Name-based/semantic highlighting
  (keycast-header-line-mode +1) ;; Show my keybindings
  (re-search-forward "public static void main()")
  (mark-sexp) (narrow-to-region (region-beginning) (region-end))
  (pop-mark)
  (scroll-down -2)
  ;; (my/load-theme 'shanty-themes-light)
  (my/load-theme 'spacemacs-light)
  (message "Fold specific regions, then click on them to show/discuss them."))

 *** Gif maker; needs privileges to capture screen ***************************
 *
 * ⇒ Move the screen capture frame while recording.
 * ⇒ Pause and restart recording, with optional inserted text messages.
 * ⇒ Global hotkey (shift+space) to toggle pausing while recording
 * (system-packages-ensure "licecap") ;; Use: ⌘-SPACE licecap
 *
 * Finally, ⌘-SPACE screenbrush. Use M-TAB to toggle it on/off; SHIFT for
 * flashlight focus.
 */
import java.util.*;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
class YoutubeShorts {
  public static void main() {
    /**
       ;; “C-x C-j” now evaluates arbitrary Java code ♥‿♥
        (repl-driven-development [C-x C-j] java)
     */

    // See a message box
    JOptionPane.showMessageDialog(new JFrame() {
      { setAlwaysOnTop(true); }
    }, "Super nice!");

    // Let's make a type to model stuff we're working with …
    record Person(String name, int age) {

      // … along with a super duper complex algorithm
      static List<Person> foo(String... names) {
        return Arrays.stream(names)
            .map(n -> new Person(n, n.length()))
            .toList();
      }
    }

    // “C-x C-j” shows me “human readable” results
    Person.foo("Hamid", "Jaafar");

    // “C-u C-x C-j” shows me “java readable” code that can be used for
    // regression tests
    Person.foo("Hamid", "Jaafar");

    // We can also interactively navigate large data-dumps…
    // …when overlay output is cramped
    Person.foo("Hamid", "Jaafar", "Musa", "Montather");

  }
}

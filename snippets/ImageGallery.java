/**
 * “Teaching a Java runtime to be an image gallery application”
 *
 * I keep my code within a class and within a method, so that I can have access
 * to Eglot's IDE features for Java: Code completion, docs, argument labels,
 * tooltips, etc. In the main() method, I generally “C-M-SPC C-x C-n”, since
 * that's what I actually want to see / work with.
 *
 *** Gif Setup *****************************************************************
 (progn
  ;; Make the M-RET command insert “* ”
  (bind-key* "M-RET" (lambda () (interactive) (insert "\n") (c-indent-command) (insert "* ")))
  ;; Auto-format on save;; For example, for Java, I need to:
  ;; brew install clang-format
  (format-all-mode +1) ;; or: M-x format-all
  ;; Don't prompt me which formatter to use; just use the defaults.
  ;; For example, for Java, I need to: brew install clang-format
  (setq format-all-formatters format-all-default-formatters)
  (setq frame-title-format
  "   Teaching a 𝒥𝒶𝓋𝒶 runtime to be an image gallery application")
  (defun doom-modeline-buffer-file-name ()  "
        https://github.com/alhassy/repl-driven-development ")
  (eglot-java-mode +1) ;; Inline overlays of arg names, syntax/type checks
  (company-mode +1) ;; Completions for methods
  (flyspell-mode -1) ;; Don't check my spelling
  (rainbow-delimiters-mode +1)  ;; Colourful rainbow parens
  (rainbow-identifiers-mode +1) ;; Name-based/semantic highlighting
  (keycast-header-line-mode +1) ;; Show my keybindings
  (re-search-forward "public static void main.String.. args.")
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
 *
 *** Setup the directory of photos! ******************************************
// rm -rf ~/Downloads/random-images/; cp -r ~/Documents/unsplash-images
~/Downloads/random-images/; cd ~/Downloads/random-images; mogrify -resize
300x300 ./*.jpg; cp pouring-coffee---photo-by-mhrezaa-on-unsplash.jpg
love-0.jpg; cp pouring-water---photo-by-mhrezaa-on-unsplash.jpg love-1.jpg
 *
 * END SETUP
 */

/*
      ;; Set “C-x C-j” to evaluate arbitrary Java code (｡◕‿◕｡)
      (repl-driven-development [C-x C-j] java)
*/

import java.awt.*;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.Arrays;

import javax.swing.*;

class ImageGallery {
  public static void main(String[] args) {
         /*         𝓖𝓸𝓪𝓵: 𝑺𝒆𝒆 𝒕𝒉𝒊𝒏𝒈𝒔 𝑨𝑺𝑨𝑷, 𝒂𝒏𝒅 𝒄𝒓𝒆𝒂𝒕𝒆 𝒃𝒚 𝒓𝒆𝒂𝒄𝒕𝒊𝒏𝒈 (｡◕‿◕｡)         */

        var frame = new JFrame(); // ⇒ Your cursor loses control: There's a JFrame created!








































        frame.setVisible(true); // It's invisible by default, lame! ﴾“x” button hides frame, not kills it﴿









































        frame.setAlwaysOnTop(true); // I wanna see it even when I'm in my editor.









































        // We see a super tiny new dialog; let's enlarge it
        frame.setSize(450, 450);









































        // Whoops we forgot to set the title!
        frame.setTitle("Hola¡");









































        // Nice; let's try another title
        frame.setTitle("Photo Gallery ~ Designed via REPL driven development");









































        // Let's add some colour to our lives!
        frame.getContentPane().setBackground(Color.CYAN);









































        // Let's “add” some content
        File directory = new File("/Users/musa/Downloads/random-images/");









































        var allImages = Arrays.stream(directory.listFiles())
            .filter(it -> it.getName().endsWith(".jpg"))
            .map(it -> new ImageIcon(it.getAbsolutePath()))
            .toList();









































        var image = new JLabel(allImages.get(0));
        frame.add(image);









































        // Wait... nothing changed?!









































        // We need to re-trigger the repainting of our GUI; e.g., by changing its
        // size (either manually or via code, to a new size):
        frame.revalidate();









































        // Can we change the image easily?
        image.setIcon(allImages.get(1));









































        // Yay, it worked!
        // Let's change get(𝒾) for 𝒾:0..8 to see this in-action a few times!









































        // Let's add a button for user interactivity!
        var button = new JButton("Next Image");
        frame.add(button);









































        // Now let's experiment to find the ideal position of the button ;-]
        frame.setLayout(new FlowLayout());









































        // Let's hook a callback onto it
        int clicks = 0;
        ActionListener  watDo = e -> frame.setTitle("You did the thing! x " + clicks++);
        button.addActionListener(watDo);









































        // We can also remove the listener:
        button.removeActionListener(watDo);









































        // Let's add modify the listener: Change the existing image!
        // (In the video, I would not copy-paste, but instead modify the above
        // snippet!)
        watDo = e -> image.setIcon(allImages.get(clicks++ % allImages.size()));









































        // I'd like to see the name of the image, whenever we click on a new image.
        // Also fit the frame to the minimal amount of space required: Pack it.
        watDo = e -> {
                var icon = allImages.get(clicks++ % allImages.size());
                image.setIcon(icon);
                frame.setTitle(icon.toString());
                frame.pack();
            };









































        // Neato! We have a photo gallery app xD

        // This was fun! Bye 👋
  }
}

/**
 * To “update” an action listener, we first need to remove it, then redefine it, then register it.
 *
 * Why? I'm not sure.
 *
 * It'd be nice if we could just update the binding, but under-the-hood it may be the case that
 * the binding is copied and not used by reference.
 *
 * TLDR: All lines of the form “watDo = ⋯” should really be of the form:
 *
 *     button.removeActionListener(watDo);
 *     watDo = ⋯;
 *     button.addActionListener(watDo);
 *
 * In the video, I take care to run the remove, update, add lines via C-x C-j.
 *
 *
 * ** “clicks” should really be an atomic integer / AtomicReference **************************
 *
 *       import java.util.concurrent.atomic.AtomicInteger;
 *       AtomicInteger clicks = new AtomicInteger(10);
 *       System.out.println(clicks.addAndGet(2));
 *
 * ** Final Program ************************************************************************
 *

    import java.awt.*;
    import java.awt.event.ActionListener;
    import java.io.File;
    import java.util.Arrays;
    import javax.swing.*;

    var frame = new JFrame();
    frame.setVisible(true);
    frame.setAlwaysOnTop(true);
    frame.setSize(450, 450);
    frame.getContentPane().setBackground(Color.CYAN);
    // Let's add an image first, so that we have an initial image that can be
    // mutated
    var image = new JLabel();
    var button = new JButton("Next Image");
    frame.add(image);
    frame.add(button);
    frame.setLayout(new FlowLayout());
    int clicks = 0;
    File directory = new File("/Users/musa/Downloads/random-images/");
    var allFiles = Arrays.stream(directory.listFiles())
                       .filter(it -> it.getName().endsWith(".jpg"))
                       .map(it -> new ImageIcon(it.getAbsolutePath()))
                       .toList();
    button.addActionListener(e -> {
                var icon = allFiles.get(clicks++ % allFiles.size());
                image.setIcon(icon);
                frame.setTitle(icon.toString());
                frame.pack();
                });
 */

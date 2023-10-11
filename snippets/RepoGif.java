import java.awt.Image;
import java.lang.reflect.Array;
import java.util.stream.IntStream;

import javax.swing.*;
// import java.awt.*;

class RepoGif {
    public static void main(String[] args) {

// ;; Set “C-x C-j” to evaluate Java code in a background REPL.
// (repl-driven-development [C-x C-j] "jshell" :prompt "jshell>")

// ;; Setup the directory of photos!
// rm -rf ~/Downloads/random-images/; cp -r ~/Documents/unsplash-images ~/Downloads/random-images/; cd ~/Downloads/random-images; mogrify -resize 300x300 ./*.jpg; cp pouring-coffee---photo-by-mhrezaa-on-unsplash.jpg love-0.jpg; cp pouring-water---photo-by-mhrezaa-on-unsplash.jpg love-1.jpg

// Select this Java snippet, then press “C-x C-j” to evaluate it

var frame = new JFrame(){{ setVisible(true); setAlwaysOnTop(true); }}; // ⇒ Your cursor loses control: There's a JFrame created!

// We see a super tiny new dialog; let's enlarge it
frame.setSize(450, 450);

// Whoops we forgot to set the title!
frame.setTitle("Hola¡");

/*
(eglot-java-mode +1)          ;; Inline overlays of arg names, syntax checks, typechecking
(company-mode +1)             ;; Completions for methods
(flyspell-mode -1)            ;; Don't check my spelling
(rainbow-delimiters-mode +1)  ;; Colourful rainbow parens
(rainbow-identifiers-mode +1) ;; Name-based/semantic highlighting

M-.   ⇒ Jump to definition
M-?   ⇒ Find references
C-h . ⇒ Show documents buffer
M-a/e ⇒ Move to start/end of statement; quick movement between statements

M-x eglot-format ⇒ Pretty print buffer
M-x eglot-rename ⇒ Rename a symbol
M-x eglot-code-action-quickfix ⇒ Provide a list of things to fix issue at hand
M-x eglot-code-actions ⇒ Do things at point

(bind-key "M-!" #'eglot-code-actions prog-mode-map)

Finally:
(keycast-header-line-mode +1) shows the current key press & binding at the top of the selected window, in its header line.
discretely displays the current command and key or mouse binding on your mode line.

*/

// Nice; let's try another title
frame.setTitle("Photo Gallery ~ Designed via REPL driven development");

// Let's add some content via “add”
frame.add(new JLabel(new ImageIcon("/Users/musa/Downloads/random-images/love-0.jpg")));

// Now let's revalidate...eek not great (ie C-x C-j on the revalidate() line above)
// Let's pack instead; yuck now the frame's massive  (ie C-x C-j on the pack() line above)
// Let's erase and resize...

// NO LONGER NEEDED ---adds an extra layer of complexity not desirable for a quick RDD demo.
// // Let's resize the image
// var icon = new ImageIcon("/Users/musa/Documents/random-images/person-0.jpeg")
// var img = new ImageIcon(icon.getImage().getScaledInstance(100, 100, Image.SCALE_SMOOTH));
// frame.add(new JLabel(img));
// frame.revalidate();
//
// // Neato!

// Wait... nothing changed?!

// We need to re-trigger the repainting of our GUI; e.g., by changing its

// size (either manually or via code, to a new size), or by explicitly

// invoking “pack” which packs all the components in the JFrame together (thereby resizing).
frame.pack();

// I don't want the resizing behaviour of “pack”, so let's use something else
frame.revalidate();

// For fun, let's press “C-x C-j” a bunch of times on the add-image line above
// then let's revalidate ;-)

// Neat, but let's erase everything
// (MA: When live casting, select this line and invoke “M-s h r” so this line highlighted for quick reuse.)
frame.getContentPane().removeAll(); frame.repaint();

// Can we change the image easily?
var image = new JLabel(new ImageIcon("/Users/musa/Downloads/random-images/love-0.jpg"));
frame.add(image);
frame.revalidate();
image.setIcon(new ImageIcon("/Users/musa/Downloads/random-images/love-1.jpg"));
// Yay, it worked!
// Let's change person-𝒾 for various 𝒾 to see this in-action a few times!

// Let's add a button for user interactivity!
frame.add(new JButton("Click"));

// Let's rewrite our button so that we can hook a callback onto it
JButton button = new JButton("Click");
int clicks = 0;
button.addActionListener(e -> frame.setTitle("You did the thing! x " + clicks++));
frame.add(button);
// Now let's experiment to find the ideal position of the button ;-]
button.setBounds(200, 320, 100, 50);

// Let's add another listener: Which adds more images to our frame lol
button.addActionListener(e -> { frame.add(image); frame.revalidate(); });

// Let's add some colour to our lives!
frame.getContentPane().setBackground(Color.cyan);

// Let's (erase everything &) make the button toggle between two images.
// (In the video, I would not copy-paste, but instead modify the above snippet!)
JButton button = new JButton("Next");
frame.add(image);
frame.revalidate();
int clicks = 0;
frame.add(button);
button.addActionListener(e -> image.setIcon(new ImageIcon(String.format("/Users/musa/Downloads/random-images/love-%s.jpg", clicks++ % 2))));

// Neato! We have a photo gallery app that works on 2 photos,
// so, by induction, this can be made to work on 𝓃 photos. Let's do it.

// Neato, now let's make this into a photo-gallery viewer xD

File directory = new File("/Users/musa/Downloads/random-images/");
var allFiles = directory.listFiles();
// allFiles // ⇒ Ah shucks, there's non-image files in-here!
// Actually, let's use a stream pipeline to filter out what we actually want: Images.
var allFiles = Array.stream(directory.listFiles()).filter(it -> it.getName().endsWith(".jpg")).toList();
// Finally, let's make these into images
// MA: Get the repl to replace "\n^\s*" with just ""; it seems otherwise I cannot easily have multi-line text to send to the repl; i.e., pretty formatting the following wont evaluate
var allFiles = Arrays.stream(directory.listFiles()).filter(it -> it.getName().endsWith(".jpg")).map(it -> new ImageIcon(it.getAbsolutePath())).toList();

// Let's add an image first, so that we have an initial image that can be mutated
var image = new JLabel();
JButton button = new JButton("Next Image");
int clicks = 0;
button.addActionListener(e -> image.setIcon(allFiles.get(clicks++ % allFiles.size())));
frame.add(button);
frame.add(image);
button.setBounds(200, 320, 100, 50);

// Let's also change the title
JButton button = new JButton("Next Image");
int clicks = 0;
button.addActionListener(e -> { var icon = allFiles.get(clicks++ % allFiles.size()); image.setIcon(icon); frame.setTitle(icon.toString()); });
frame.add(button);
frame.add(image);
button.setBounds(200, 320, 100, 50);

// YAY!

    }
}

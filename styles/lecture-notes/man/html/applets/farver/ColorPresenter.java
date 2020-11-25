import java.applet.Applet;
import java.awt.*;
import java.awt.event.*;

/** Color class extended with internet color encoding operation
    which is redefined via toString() */
class INColor extends Color {

  public INColor(int red, int green, int blue) {
    super(red,green,blue);
  }

  private String twoCharColor(int colorNumber){
  /* Convert the parameter, a color, to a hexadecimal number of length exactly two */
     if (colorNumber >= 0 && colorNumber < 16) return "0" + Integer.toHexString(colorNumber);
     else if (colorNumber >= 16 && colorNumber < 256) return  Integer.toHexString(colorNumber);
     else return "??";
  }

  public String toString () {
  /** Return the seven character long internet color encoding of this color */
    return "#" + twoCharColor(getRed()) + twoCharColor(getGreen()) + twoCharColor(getBlue());
  }

}

class ColorPanel extends Panel {

  public ColorPanel (Label lab, TextField text, Scrollbar slider, ColorListener listener){

     GridBagConstraints c = new GridBagConstraints();
     GridBagLayout gridbag = new GridBagLayout();
     setLayout(gridbag);

     c.fill = GridBagConstraints.HORIZONTAL;

     add(lab); 

     c.gridwidth = GridBagConstraints.REMAINDER; //It ends a row.
     gridbag.setConstraints(text, c);
     add(text);

     c.weightx = 1.0;
     c.gridwidth = 1;
     gridbag.setConstraints(slider, c);
     slider.setMaximum(255); 
     slider.setBlockIncrement(10);
     slider.setUnitIncrement(1);
     add(slider);

     text.addActionListener (listener);
     slider.addAdjustmentListener (listener);
  }
} // end class ColorPanel


public class ColorPresenter extends Applet {

   public static final int Red = 0;
   public static final int Green = 1;
   public static final int Blue = 2;

   /** Read the color from a text field specified by the parameter */
   public int readColor (int COLOR) {
       String str;
       if (COLOR == Red) str = redTextField.getText();
       else if (COLOR == Green) str = greenTextField.getText();
       else if (COLOR == Blue) str = blueTextField.getText();
       else str = "";
       return Integer.parseInt(str);
   }

   /** Write the color to a text field as specified by the parameters */
   public void writeColor (int COLOR, int val){
       if      (COLOR == Red) redTextField.setText(Integer.toString(val,10));
       else if (COLOR == Green) greenTextField.setText(Integer.toString(val,10));
       else if (COLOR == Blue) blueTextField.setText(Integer.toString(val,10));
   }

   /** Construct individual components */
   private Label title = new Label ("Color adjuster", Label.CENTER);

   private Label redLabel = new Label ("Red:");
   private TextField redTextField = new TextField ("0",5);
   private Scrollbar redSlider = new Scrollbar(Scrollbar.HORIZONTAL,0,1,0,255);

   private Label greenLabel = new Label ("Green:");
   private TextField greenTextField = new TextField ("0",5);
   private Scrollbar greenSlider = new Scrollbar(Scrollbar.HORIZONTAL,0,1,0,255);

   private Label blueLabel = new Label ("Blue:");
   private TextField blueTextField = new TextField ("0",5);
   private Scrollbar blueSlider = new Scrollbar(Scrollbar.HORIZONTAL,0,1,0,255);
   
   private Label hexTextField = new Label ("#000000");
   private ColorBlop colorBlop = new ColorBlop(Color.white);

   private ColorListener listener = new ColorListener (this);

   private ColorPanel redGroup = 
     new ColorPanel(redLabel, redTextField, redSlider, listener);
   private ColorPanel greenGroup = 
     new ColorPanel(greenLabel, greenTextField, greenSlider, listener);
   private ColorPanel blueGroup =
     new ColorPanel(blueLabel, blueTextField, blueSlider, listener);

   /** Applet init */
   public void init() {

       setLayout(new GridLayout(6,1));

       add(title);
       add(redGroup); add(greenGroup); add(blueGroup); 
       add(colorBlop);    
       add(hexTextField);
    
       resize (400,250);
       adjust();
   } //end init

   private boolean inColorRange (int c){
       return c >= 0 && c <= 255;
   }

   /* Given the scrollbar sl has changed it's value to val,
       adjust the user interface*/
   public void sliderAdjust (Scrollbar sl, int val){
     if (sl == redSlider) writeColor(Red, val);
     else if (sl == greenSlider) writeColor(Green, val);
     else if (sl == blueSlider) writeColor(Blue, val);
     adjust();
   }
 
   /* Given that a text field has been changed,
      adjust the user interface */   
   public void adjust() {
      Color newColor;
      int r,g,b;
      r = readColor(Red); g = readColor(Green); b = readColor(Blue);
      if (inColorRange(r) && inColorRange(g) && inColorRange(b)){
         newColor = new INColor(readColor(Red), readColor(Green), readColor(Blue));
         colorBlop.setColor(newColor);
         colorBlop.repaint();
         hexTextField.setText(newColor.toString());

         // adjust scrollbars:
         redSlider.setValue(r); greenSlider.setValue(g); blueSlider.setValue(b);
      } else  hexTextField.setText("ERROR");
    }

  /* Draws a box around this panel. */
  public void paint(Graphics g) {
      Dimension d = getSize();
      g.drawRect(0,0, d.width - 3, d.height - 3);
  }
        
  /* Puts a little space between the panel and its contents */
  public Insets getInsets() {
     return new Insets(10,10,10,10);
  }

  public static void main(String[] args){
     Frame f = new Frame("Color adjuster");
     f.addWindowListener(new WindowAdapter() {
         public void windowClosing(WindowEvent e) {
             System.exit(0);
           }
       } );
     ColorPresenter adjuster = new ColorPresenter();
     adjuster.init();
     f.add("Center", adjuster);
     f.pack();
     f.setVisible(true);
 } // end main
}

class ColorListener implements ActionListener, AdjustmentListener {
  // Listens to events from components of a ColorPresenter

  private ColorPresenter presentation;
  private INColor currentColor;

  public ColorListener (ColorPresenter presentation){
    this.presentation = presentation;
  }

  public void actionPerformed(ActionEvent action){
    presentation.adjust();
  }

  public void adjustmentValueChanged (AdjustmentEvent adjustment){
    presentation.sliderAdjust((Scrollbar)adjustment.getAdjustable(), 
                              adjustment.getValue());
  }
} // end class ColorListener

class ColorBlop extends Canvas {

  private Dimension size = new Dimension(100,100);
  private Color currentColorOfBlop;

  ColorBlop(Color c){
    currentColorOfBlop = c;
  }

  public void setColor (Color c){
    currentColorOfBlop = c;
  }

  public void paint (Graphics page) {
    page.setColor(currentColorOfBlop);
    page.fillRect(0,0,400,200);
  }

  public Dimension getMinimumSize() {
       return size;
  }

  public Dimension getPreferredSize() {
      return getMinimumSize();
  }
} // end class ColorBlop

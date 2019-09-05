package kawapad;

import java.awt.Color;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.SwingUtilities;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;
import javax.swing.text.Segment;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;

/**
 * See https://stackoverflow.com/a/28773736 by diadyne
 * 
 * This answer is great and extremely precious. Without the answer, this could
 * not be implemented. I would like to state the best appreciation here. Thank
 * you. Even though the answer is not taken as an accepted answer, in fact it
 * is the correct anser. This should be accepted and deserves more likes.
 */
public abstract class KawapadDocumentFilter extends DocumentFilter {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    private static final StyleContext styleContext = StyleContext.getDefaultStyleContext();
    public static AttributeSet createAttributeSet(Color foreground) {
        return styleContext.addAttribute( styleContext.getEmptySet(), StyleConstants.Foreground, foreground );
    }
    public static AttributeSet createAttributeSet(Color foreground, Color background) {
        return 
                styleContext.addAttribute(
                    styleContext.addAttribute(
                        styleContext.getEmptySet(), 
                        StyleConstants.Foreground, 
                        foreground ),
                    StyleConstants.Background, 
                    background );
    }
    public static Color getColorFromAttributeSet(AttributeSet attr) {
        return (Color) attr.getAttribute( StyleConstants.Foreground );
    }
    public static Color getForegroundColor(AttributeSet attr) {
        return (Color) attr.getAttribute( StyleConstants.Foreground );
    }
    public static Color getBackgroundColor(AttributeSet attr) {
        return (Color) attr.getAttribute( StyleConstants.Background );
    }
    public static final AttributeSet    darkGreenAttributeSet   = createAttributeSet( new Color(0x00008800 ) );
    public static final AttributeSet    greenAttributeSet   = createAttributeSet( Color.GREEN );
    public static final AttributeSet    blueAttributeSet    = createAttributeSet( Color.BLUE );
    public static final AttributeSet    redAttributeSet     = createAttributeSet( Color.RED );
    public static final AttributeSet    grayAttributeSet    = createAttributeSet( Color.GRAY );
    public static final AttributeSet    orangeAttributeSet  = createAttributeSet( Color.ORANGE );
    public static final AttributeSet    whiteAttributeSet   = createAttributeSet( Color.WHITE );

    private final StyledDocument document;
    public KawapadDocumentFilter(StyledDocument document) {
        this.document = document;
    }
    public interface SyntaxElement {
        Object getName();
        void setPattern(Pattern pattern);
        Pattern getPattern();
        AttributeSet getAttributeSet();
        void setAttributeSet(AttributeSet attributeSet);
        void setColor(Color foreground, Color background);
        void setColor(Color foreground);
        Color getForegroundColor();
        Color getBackgroundColor();
        
        public static final class Default implements SyntaxElement {
            Object name;
            Pattern pattern;
            AttributeSet attributeSet;
            public Default(Object name, Pattern pattern, AttributeSet attributeSet) {
                super();
                this.name = name;
                this.pattern = pattern;
                this.attributeSet = attributeSet;
            }
            @Override
            public Object getName() {
                return name;
            }
            @Override
            public void setPattern(Pattern pattern) {
                this.pattern = pattern;
            }
            @Override
            public Pattern getPattern() {
                return pattern;
            }
            @Override
            public AttributeSet getAttributeSet() {
                return attributeSet;
            }
            @Override
            public void setAttributeSet( AttributeSet attributeSet ) {
                this.attributeSet = attributeSet;
            }
            @Override
            public void setColor( Color foreground, Color background ) {
                this.attributeSet = createAttributeSet( foreground, background );
            }
            @Override
            public void setColor( Color foreground) {
                this.attributeSet = createAttributeSet( foreground );
            }
            @Override
            public Color getForegroundColor() {
                return KawapadDocumentFilter.getForegroundColor( this.attributeSet );
            }
            @Override
            public Color getBackgroundColor() {
                return KawapadDocumentFilter.getBackgroundColor( this.attributeSet );
            }
        }
    }
    public static SyntaxElement createSyntaxElement( Object name, Pattern pattern, AttributeSet attributeSet ) {
        return new SyntaxElement.Default( name, pattern, attributeSet );
    }
    
    public static class SyntaxElementList extends ArrayList<SyntaxElement> {
        public SyntaxElementList() {
            super();
        }
        public SyntaxElementList(Collection<? extends SyntaxElement> c) {
            super( c );
        }
        public SyntaxElement get( Object name ) {
            for ( SyntaxElement se : this ) {
                if ( name.equals( se.getName() ) ) {
                    return se;
                }
            }
            throw new IllegalArgumentException( "could not find " + name + "." );
        }
    }
    
    protected abstract Collection<SyntaxElement> createSyntaxElementList();

    private SyntaxElementList syntaxElementList;
    public void resetSyntaxElementList() {
        this.syntaxElementList = null;
    }
    public SyntaxElementList getSyntaxElementList() {
        if ( syntaxElementList == null ) {
            this.syntaxElementList = new SyntaxElementList( createSyntaxElementList() );
        }
        return syntaxElementList;
    }
    public abstract AttributeSet getDefaultAttributeSet();  
    
    @Override
    public void insertString(FilterBypass fb, int offset, String text, AttributeSet attributeSet)
            throws BadLocationException {
        super.insertString( fb, offset, text, attributeSet );
        handleTextChanged();
    }
    
    @Override
    public void remove(FilterBypass fb, int offset, int length) throws BadLocationException {
        super.remove( fb, offset, length );
        handleTextChanged();
    }
    
    @Override
    public void replace(FilterBypass fb, int offset, int length, String text, AttributeSet attributeSet)
            throws BadLocationException {
        super.replace( fb, offset, length, text, attributeSet );
        handleTextChanged();
    }
    
    Timer timer = new Timer(true);
    final Object lock = new Object();
    transient TimerTask theLastRunnable = null;
    private void handleTextChanged() {
        TimerTask r = null;
        synchronized (lock){
            r= new TimerTask() {
                @Override
                public void run() {
                    boolean f=false;
                    synchronized( lock ) {
                        if ( theLastRunnable == this ) {
                            theLastRunnable = null;
                            f=true;
                        }
                    }
                    if ( f ) {
                        SwingUtilities.invokeLater( new Runnable() {
                            @Override
                            public void run() {
                                update();
                            }
                        } );
                    }
                }
            };
            this.theLastRunnable = r;
        }
        if ( r != null )
            timer.schedule( r, 100 );
    }

//    ElapsedTime ep = new ElapsedTime();
    
    void update() {
        AttributeSet defaultAttr = getDefaultAttributeSet();
        synchronized ( document ) {
            // clear
//            ep.start();
            document.setCharacterAttributes(0, document.getLength(), defaultAttr , true);
//            ep.end();
//            logInfo( ep.getMessage( "Syntax Clear" ));
            // 
            Segment text = SchemeParentheses.getText( document );
            for ( SyntaxElement e : getSyntaxElementList() ) {
//                ep.start();
                updateTextStyles( document, text, e.getPattern(), defaultAttr, e.getAttributeSet() );
//                ep.end();
//                logInfo( ep.getMessage( "Syntax Set:" + e.getName() ));
            }
        }
    }
    
    public static final String GROUP = "K";
    static void updateTextStyles( StyledDocument document, Segment text, Pattern pattern, AttributeSet defaultAttr, AttributeSet attr ) {
        // Look for tokens and highlight them
        Matcher matcher = pattern.matcher( text );
        while (matcher.find()) {
            // Change the color of recognized tokens
            if ( 0 < matcher.groupCount() ) {
                document.setCharacterAttributes( matcher.start(GROUP), matcher.end(GROUP) - matcher.start(GROUP), attr, false );
            } else {
                document.setCharacterAttributes( matcher.start(),      matcher.end()      - matcher.start(),      attr, false );
            }
        }
    }
    
    // Use a regular expression to find the words you are looking for
    Pattern pattern = buildPattern();

    /**
     * Build the regular expression that looks for the whole word of each word that
     * you wish to find. The "\\b" is the beginning or end of a word boundary. The
     * "|" is a regex "or" operator.
     * 
     * @return
     */
    static String[] KEYWORDS    = { "display" };
    static String[] LETTERS     = { "(", ")", };
    
    private Pattern buildPattern() {
        StringBuilder sb = new StringBuilder();
        for (String token : KEYWORDS) {
            sb.append( "\\b" ); // Start of word boundary
            sb.append( token );
            sb.append( "\\b|" ); // End of word boundary and an or for the next word
        }
        for (String token : LETTERS) {
            sb.append( "\\" );
            sb.append( token );
            sb.append( "|" ); // End of word boundary and an or for the next word
        }
        if (sb.length() > 0) {
            sb.deleteCharAt( sb.length() - 1 ); // Remove the trailing "|"
        }
        
        Pattern p = Pattern.compile( sb.toString() );
        
        return p;
    }

}

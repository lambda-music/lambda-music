package kawapad;

import java.awt.Color;
import java.lang.invoke.MethodHandles;
import java.util.Collection;
import java.util.Objects;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.SwingUtilities;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.DocumentFilter;
import javax.swing.text.JTextComponent;
import javax.swing.text.Segment;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;

import kawapad.SyntaxHighlighterDocumentAttribute.ArrayDocumentAttribute;
import lamu.lib.log.Logger;

/**
 * See https://stackoverflow.com/a/28773736 by diadyne
 * 
 * This answer is great and extremely precious. Without the answer, this could
 * not be implemented. I would like to state the best appreciation here. Thank
 * you. Even though the answer is not taken as an accepted answer, in fact it
 * is the correct answer. This should be accepted and deserves more likes.
 */
public abstract class SyntaxHighlighter extends DocumentFilter {
    public static final String GROUP = "K";

    // =================================================================================================================
    
    static final boolean DEBUG = false;
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    // =================================================================================================================

    private static final StyleContext styleContext = StyleContext.getDefaultStyleContext();
    public static AttributeSet createAttributeSet() {
        return styleContext.getEmptySet();
    }
    public static AttributeSet createAttributeSet( Color color ) {
        return setForeground( color, createAttributeSet() );
    }
    public static AttributeSet createAttributeSet( Color foreground, Color background ) {
        AttributeSet element = styleContext.getEmptySet();
        if ( foreground != null)
            element = setForeground(foreground, element);
        
        if ( background != null)
            element = setBackground(background, element);
        
        return element;
    }
    public static AttributeSet setBackground( Color color, AttributeSet element ) {
        return styleContext.addAttribute( element , StyleConstants.Background, color );
    }
    public static AttributeSet setForeground( Color color, AttributeSet element ) {
        return styleContext.addAttribute( element, StyleConstants.Foreground, color );
    }
    public static Color getForegroundColor(AttributeSet attr) {
        return (Color) attr.getAttribute( StyleConstants.Foreground );
    }
    public static Color getBackgroundColor(AttributeSet attr) {
        return (Color) attr.getAttribute( StyleConstants.Background );
    }
    
    static class SyntaxElementConstant {
        String name;
        SyntaxElementConstant(String name) {
            super();
            this.name = name;
        }
        @Override
        public String toString() {
            return this.name;
        }
    }
    public static final Object KEY_SYNTAX_ELEMENT = new SyntaxElementConstant( "syntax-element" );
    public static AttributeSet setSyntaxElement(AttributeSet attr, SyntaxElement se) {
        return styleContext.addAttribute( attr, KEY_SYNTAX_ELEMENT, se );
    }
    public static SyntaxElement getSyntaxElement( AttributeSet attr ) {
        SyntaxElement element = (SyntaxElement) attr.getAttribute( KEY_SYNTAX_ELEMENT );
        if ( element == null )
            element = DEFAULT_SYNTAX_ELEMENT;
        return element;
    }

    
    public static final SyntaxElement DEFAULT_SYNTAX_ELEMENT = 
        SyntaxElement.create( "DEFAULT_SYNTAX", null, styleContext.getEmptySet() );
    
    private JTextComponent textComponent;
    public SyntaxHighlighter(JTextComponent kawapad) {
        this.textComponent = kawapad;
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
    
    public abstract SyntaxElement getDefaultAttributeSet();  
    
    @Override
    public void insertString(FilterBypass fb, int offset, String text, AttributeSet attributeSet) throws BadLocationException {
        super.insertString( fb, offset, text, attributeSet );
        handleTextChanged(fb, offset, text.length());
    }
    
    @Override
    public void remove(FilterBypass fb, int offset, int length) throws BadLocationException {
        super.remove( fb, offset, length );
        handleTextChanged(fb, offset, length);
    }
    
    @Override
    public void replace(FilterBypass fb, int offset, int length, String text, AttributeSet attributeSet) throws BadLocationException {
        super.replace( fb, offset, length, text, attributeSet );
        handleTextChanged(fb, offset, length);
    }

    final StyledDocument emptyDocument = new DefaultStyledDocument();
    final Timer timer = new Timer(true);
    final Object lock = new Object();
    volatile TimerTask theLastRunnable = null;
    
    private void handleTextChanged( FilterBypass fb, int offset, int length ) {
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
                                updateTextStyles( 
//                                    DefaultSyntaxHighlighterProcessor.INSTANCE,
                                    NewSyntaxHighlighterProcessor.INSTANCE,
                                    textComponent,
                                    (SyntaxHighlighterStyledDocument)textComponent.getDocument(),
                                    emptyDocument,
                                    getSyntaxElementList(), 
                                    getDefaultAttributeSet(), 
                                    offset, length);
                            }
                        } );
                    }
                }
            };
            this.theLastRunnable = r;
        }
        if ( r != null )
            timer.schedule( r, 500 );
    }
    
    interface SyntaxHighlighterProcessor {
        void process(
            SyntaxHighlighterStyledDocument document, 
            SyntaxElementList syntaxElementList,
            SyntaxElement defaultSyntaxElement );
    }
    
    static void updateTextStyles(
        SyntaxHighlighterProcessor processor,
        JTextComponent textComponent, 
        SyntaxHighlighterStyledDocument document, 
        StyledDocument emptyDocument, 
        SyntaxElementList syntaxElementList, 
        SyntaxElement defaultSyntaxElement, 
        int offset, 
        int length ) 
    {
        synchronized ( document ) {
            int dot = textComponent.getCaret().getDot();
            int mark = textComponent.getCaret().getMark();
            textComponent.setDocument( emptyDocument );
            ((SyntaxHighlighterStyledDocument)document).callWriteLock();
            try {
                processor.process( document, syntaxElementList, defaultSyntaxElement );
            } finally {
                ((SyntaxHighlighterStyledDocument)document).callWriteUnlock();
                textComponent.setDocument( document );
                textComponent.getCaret().setDot( mark );
                if ( mark!=dot)
                    textComponent.getCaret().moveDot( dot );
            }
        }
    }
    
    static void updateTextStylesWithAllSyntaxElement(
        Segment text,
        SyntaxHighlighterDocumentAttribute document, 
        SyntaxElementList syntaxElementList,
        ElapsedTime ep ) 
    {
        for ( SyntaxElement element : syntaxElementList ) {
            if ( DEBUG ) ep.start();
            updateTextStylesWithSyntaxElement( document, text, element  );
            if ( DEBUG ) ep.end();
            if ( DEBUG ) logInfo( ep.getMessage( "Syntax Set:" + element.getName() ));
        }
    }

    static void updateTextStylesWithSyntaxElement( SyntaxHighlighterDocumentAttribute document, Segment text, SyntaxElement element ) {
        // Look for tokens and highlight them
        Pattern pattern = element.getPattern();
        
        // if its pattern is null, it means that it is a pseudo element.
        // (Fri, 27 Mar 2020 09:39:14 +0900)
        if ( pattern == null ) {
            return ;
        }
        
        Matcher matcher = pattern.matcher( text );
        ElapsedTime ep = new ElapsedTime();
        @SuppressWarnings("unused")
        double t =0;
        
        while (matcher.find()) {
            // Change the color of recognized tokens
            int start;
            int end;
            if ( 0 < matcher.groupCount() ) {
                start = matcher.start(GROUP);
                end = matcher.end(GROUP);
            } else {
                start = matcher.start();
                end = matcher.end();
            }
            if ( DEBUG )
                ep.start();
            document.setCharacterAttributes( start, end - start, element, true );
            if ( DEBUG )
                ep.end();
            
            t+=ep.elapsedTime();
        }
        if ( DEBUG )
            logInfo( ElapsedTime.format( "set-total", t) );
    }

    
    /**
     *  The old version of syntax highlighter. This class is not used anymore.
     */
    static class DefaultSyntaxHighlighterProcessor implements SyntaxHighlighterProcessor {
        static final SyntaxHighlighterProcessor INSTANCE = new DefaultSyntaxHighlighterProcessor();
        @Override
        public void process(SyntaxHighlighterStyledDocument document, SyntaxElementList syntaxElementList, 
            SyntaxElement defaultSyntaxElement ) 
        {
            proc(document, syntaxElementList, defaultSyntaxElement);
        }

        public static void clearAttributes(SyntaxHighlighterStyledDocument document, SyntaxElement defaultSyntaxElement,
            ElapsedTime ep) {
            // clear
            if ( DEBUG ) ep.start();
            document.setCharacterAttributes( 0, document.getLength(), defaultSyntaxElement.getAttributeSet() , true);
            if ( DEBUG ) ep.end();
            if ( DEBUG ) logInfo( ep.getMessage( "Syntax Clear" ));
        }

        public static void proc(SyntaxHighlighterStyledDocument document, 
            SyntaxElementList syntaxElementList, SyntaxElement defaultSyntaxElement ) 
        {
            if ( DEBUG ) logInfo( "NewSyntaxHighlighterProcessor" );
            
            ElapsedTime ep = new ElapsedTime();
            clearAttributes(document, defaultSyntaxElement, ep);
            
            //
            SyntaxHighlighterDocumentAttribute doc = 
                SyntaxHighlighterDocumentAttribute.create( document );
            
            updateTextStylesWithAllSyntaxElement( 
                KawapadSelection.getText( document ), 
                doc, syntaxElementList, ep);
        }

    }

    public static void differentialUpdate( ArrayDocumentAttribute from, ArrayDocumentAttribute to, 
        SyntaxHighlighterDocumentAttribute result, SyntaxElement defaultSyntaxElement ) 
    {
        if ( DEBUG ) SyntaxHighlighter.logInfo("differentialUpdate");
        
        SyntaxElement[] arrFrom = from.getArray();
        SyntaxElement[] arrTo = to.getArray();
        
        int prevPos = -1;
        SyntaxElement prevValue = null;
        for ( int currPos=0; currPos<arrTo.length; currPos++ ) {
            SyntaxElement currValue ;
            
            //
            // it denotes that two values are identical when currValue is null,
            //
            
            if ( Objects.equals( arrFrom[currPos], arrTo[currPos] ) ) {
                currValue = null;
            } else {
                currValue = arrTo[currPos];
            }
            
            if ( prevValue == currValue ) {
                
            } else {
//                logInfo( "prevValue:" + prevValue );
                if ( prevValue != null ) {
                    result.setCharacterAttributes( prevPos, currPos-prevPos, prevValue, true );
                }

                prevValue = currValue;
                prevPos = currPos;
            }
        }

    }
    
    /**
     * This class is the new version of Syntax Highlighter. This version works a lot faster than the old one. 
     */
    static class NewSyntaxHighlighterProcessor implements SyntaxHighlighterProcessor {
        static final NewSyntaxHighlighterProcessor INSTANCE = new NewSyntaxHighlighterProcessor();
        @Override
        public void process(SyntaxHighlighterStyledDocument document, SyntaxElementList syntaxElementList, SyntaxElement defaultSyntaxElement ) {
            proc(document, syntaxElementList, defaultSyntaxElement);
        }
        public static void proc(SyntaxHighlighterStyledDocument document, SyntaxElementList syntaxElementList, SyntaxElement defaultSyntaxElement) {
            ElapsedTime ep = new ElapsedTime();
            if ( DEBUG ) 
                logInfo( ep.getMessage( "NewSyntaxHighlighterProcessor" ));

            // read
            if ( DEBUG ) ep.start();
            SyntaxHighlighterDocumentAttribute orig = SyntaxHighlighterDocumentAttribute.create( document );
            ArrayDocumentAttribute from = SyntaxHighlighterDocumentAttribute.create( orig.getLength() );
            orig.copyTo( from );
            ArrayDocumentAttribute to = SyntaxHighlighterDocumentAttribute.create( orig.getLength(), DEFAULT_SYNTAX_ELEMENT );
            if ( DEBUG ) ep.end();
            if ( DEBUG ) logInfo( ep.getMessage( "Read and Duplicate" ));
            
//            logInfo( to .toString() );
            updateTextStylesWithAllSyntaxElement( 
                KawapadSelection.getText( document ), 
                to, syntaxElementList, ep);
 
            if ( DEBUG ) ep.start();
            differentialUpdate(from, to, orig, defaultSyntaxElement );
            if ( DEBUG ) ep.end();
            if ( DEBUG ) logInfo( ep.getMessage( "differentialUpdate" ));
        }
    }
}

package kawapad;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import javax.swing.text.Document;
import javax.swing.text.Highlighter.HighlightPainter;
import javax.swing.text.JTextComponent;
import javax.swing.text.Segment;

import kawapad.CaretTransformer.CaretPos;

public class KawapadTemporarySearchHighlighter extends KawapadTemporaryHighlighter {    
    public static void highlightSearchPatterns( JTextComponent component, HighlightPainter highlightPainter, 
        CharSequence text, Pattern pattern) throws BadLocationException 
    {
        for ( Matcher m = pattern.matcher( text ); m.find(); ) {
            addHighlight( component, 
                m.start(GROUP_ID),
                m.end(GROUP_ID),
                highlightPainter );
        }
    }

    public static String getCurrentWord( CharSequence text, Caret caret ) {
        if ( caret.getDot() == caret.getMark() ) {
            return getCurrentWord( text, new CaretPos( caret ) );
        } else {
            if ( caret.getDot() < caret.getMark() ) {
                return text.subSequence( caret.getDot(), caret.getMark() ).toString();
            } else {
                return text.subSequence( caret.getMark(), caret.getDot()).toString();
            }
        }
    }
    public static String getCurrentWord( CharSequence text, CaretPos caret ) {
        CaretPos before = caret.duplicate();
        CaretPos after = before.duplicate();
        KawapadSelection.LISPWORD_SELECT_CURRENT_TRANSFORMER.process( text, before, after );
        if ( after.left <= after.right ) {
            return text.subSequence( after.left, after.right + 1 ).toString();
        } else {
            return null;
        }
    }
    
    public static void highlightSearchPatterns( JTextComponent component, HighlightPainter highlightPainter, 
        String searchString, boolean wordSearch ) throws BadLocationException 
    {
        Document document = component.getDocument();
        Segment text = new Segment();
        document.getText( 0, document.getLength(), text );

        if ( searchString != null ) {
            highlightSearchPatterns( component, highlightPainter, text, Pattern.compile( 
                searchStringToPattern( searchString, wordSearch ) ) );
        }
    }
    
    public static final String GROUP_ID = "HELLO";

    static String searchStringToPattern( String searchString, boolean wordSearch ) {
        if ( DEBUG )
            logInfo( "searchStringToPattern=" +  wordSearch );
        
        if ( wordSearch ) {
            return "(?:^|\"|\\s|\\(|\\))" + "(?<"+GROUP_ID+">" + Pattern.quote( searchString ) + ")" + "(?=$|\"|\\s|\\(|\\))"  ;
//            return "(?=^|\\s?|\\(?|\\)?)" + "(?<"+GROUP_ID+">" + Pattern.quote( searchString ) + ")"; // + "(?<=$|\\s|\\(|\\))"  ;
        } else {
            return "(?<" + GROUP_ID + ">" + Pattern.quote( searchString ) +")";
        }
    }
}

package kawapad;

import java.awt.Color;

import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Document;
import javax.swing.text.Highlighter.HighlightPainter;
import javax.swing.text.JTextComponent;
import javax.swing.text.Segment;

import kawapad.SchemeParenthesisParser.ParserState;

public class KawapadTemporaryParenthesisHighlighter extends KawapadTemporaryHighlighter {
    static transient Color parenthesisHighlightColor;
    static transient HighlightPainter parenthesisHighlightPainter;
    synchronized static void setParenthesisHighlightColor( Color color ) {
        parenthesisHighlightColor = color;
        parenthesisHighlightPainter = new DefaultHighlighter.DefaultHighlightPainter( parenthesisHighlightColor );
    }
    static {
        setParenthesisHighlightColor( new Color( 0x00, 0x88, 0x88, 0xff ) );
    }
    public static void forceClearHighlightedParenthesis() {
        eliminateClearingHighlightQueue();
    }
    public static void clearHighlightedParenthesis() {
        popClearingHighlightQueue();
    }

    public static void highlightMatchingParenthesis( JTextComponent component, int position ) throws BadLocationException {
        if ( Kawapad.ENABLED_SHOW_CORRESPONDING_PARENTHESES ) {
            Document document = component.getDocument();
            Segment text = new Segment();
            document.getText( 0, document.getLength(), text );
            ParserState parserState = 
                    SchemeParenthesisParser.lookupParenthesis( text, position );
            if ( Kawapad.DEBUG_PARENTHESIS )
                logInfo( "highlightMatchingParenthesis:" + position + "=>"+ parserState.isFound() );
            if ( parserState.isFound() )
                addParenthesisHighlight( component, 
                    parserState.getIterator().getInitialIndex(), 
                    parserState.getIterator().getIndex(),
                    parenthesisHighlightPainter );
        }
    }
}

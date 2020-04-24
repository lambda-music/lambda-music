package kawapad;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Highlighter.HighlightPainter;
import javax.swing.text.JTextComponent;
import javax.swing.text.Segment;

import kawapad.SchemeParenthesisParser.ParserState;

public class KawapadTemporaryParenthesisHighlighter extends KawapadTemporaryHighlighter {
    public static void forceClearHighlightedParenthesis() {
        eliminateClearingHighlightQueue();
    }
    public static void clearHighlightedParenthesis() {
        popClearingHighlightQueue();
    }

    public static void highlightMatchingParenthesis( JTextComponent component, HighlightPainter highlightPainter, int position ) throws BadLocationException {
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
                    highlightPainter );
        }
    }
}

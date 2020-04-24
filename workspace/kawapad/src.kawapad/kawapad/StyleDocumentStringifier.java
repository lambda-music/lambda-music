package kawapad;

import java.util.regex.Pattern;

import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.StyledDocument;

public class StyleDocumentStringifier {
    static String indent( int level ) {
        StringBuilder sb = new StringBuilder();
        for ( int i=0; i<level; i++ ) {
            sb.append("    ");
        }
        return sb.toString();
    }
    static Pattern SET_INDENT = Pattern.compile("^",Pattern.MULTILINE | Pattern.DOTALL );
    static String setIndent( String s, String indent ) {
        return SET_INDENT.matcher(s).replaceAll( indent );
    }
    static void log( StyledDocument document ) {
        Element[] elements = document.getRootElements();
        for ( int i=0; i<elements.length; i++ ) {
            log( 1, "element["+i+"]", elements[i] );
        }
    }
    static void log( int level, String prefix, Element element ) {
        try {
            int startOffset = element.getStartOffset();
            int endOffset = element.getEndOffset();
            String text = element.getDocument().getText( startOffset, endOffset - startOffset);
            String s = element.toString().trim() + "\n"  + text;
            String ss = setIndent( s, prefix +  ">" + indent( level )  );
            System.err.println( ss );
        } catch (BadLocationException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        for ( int i=0; i<element.getElementCount(); i++ ) {
            log( level+ 1, prefix+ "["+i+"]", element.getElement(i) );
        }
    }
    
    
}

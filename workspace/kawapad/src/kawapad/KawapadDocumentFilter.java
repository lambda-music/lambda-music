package kawapad;

import java.awt.Color;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.SwingUtilities;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.DocumentFilter;
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
	private static final StyleContext styleContext = StyleContext.getDefaultStyleContext();
	public static AttributeSet createAttribute(Color foreground) {
		return styleContext.addAttribute( styleContext.getEmptySet(), StyleConstants.Foreground, foreground );
	}
	public static final AttributeSet	greenAttributeSet	= createAttribute( Color.GREEN );
	public static final AttributeSet	blueAttributeSet	= createAttribute( Color.BLUE );
	public static final AttributeSet	redAttributeSet     = createAttribute( Color.RED );
	public static final AttributeSet	orangeAttributeSet     = createAttribute( Color.ORANGE );
	public static final AttributeSet	whiteAttributeSet	= createAttribute( Color.WHITE );

	private final StyledDocument document;
	public KawapadDocumentFilter(StyledDocument document) {
		this.document = document;
	}
	public static final class SyntaxElement {
		Pattern pattern;
		AttributeSet attributeSet;
		public SyntaxElement(Pattern pattern, AttributeSet attributeSet) {
			super();
			this.pattern = pattern;
			this.attributeSet = attributeSet;
		}
	}
	
	public abstract List<SyntaxElement> getSyntaxElementList();  
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
	
	private void handleTextChanged() {
		AttributeSet defaultAttr = getDefaultAttributeSet();
		SwingUtilities.invokeLater( new Runnable() {
			@Override
			public void run() {
		        document.setCharacterAttributes(0, getText(document).length(), defaultAttr , true);
				for ( SyntaxElement e : getSyntaxElementList() ) {
					updateTextStyles( document, e.pattern , defaultAttr, e.attributeSet );
				}
			}
		});
	}
	
	
	
	static String getText( Document document ) {
		try {
			return document.getText( 0, document.getLength() );
		} catch (BadLocationException e) {
			e.printStackTrace();
			return "";
		}
	}

	static void updateTextStyles( StyledDocument document, Pattern pattern, AttributeSet defaultAttr, AttributeSet attr ) {
		
		// Look for tokens and highlight them
		Matcher matcher = pattern.matcher( getText( document ) );
		while (matcher.find()) {
			// Change the color of recognized tokens
			document.setCharacterAttributes( matcher.start(), matcher.end() - matcher.start(), attr, false );
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
	static String[]	KEYWORDS	= { "display" };
	static String[]	LETTERS		= { "(", ")", };
	
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
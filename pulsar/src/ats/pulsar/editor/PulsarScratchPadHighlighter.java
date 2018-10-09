package ats.pulsar.editor;

import java.awt.Color;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JTextPane;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentListener;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

import ats.pulsar.editor.SimpleSchemeParser.ParserState;

public class PulsarScratchPadHighlighter {
	public static String lispWordToPatternString(Collection<String> lispWordCollection) {
		String[] lispWords = lispWordCollection.toArray( new String[ lispWordCollection.size() ] );
		Arrays.sort( lispWords, new Comparator<String>() {
			@Override
			public int compare(String o1, String o2) {
				return o2.length() - o1.length();
			}
		} );
		
		for ( int i=0; i<lispWords.length; i++ ) {
			if ( lispWords[i].matches( "\\w$" ) ) {
				lispWords[i] = "\\b" + Pattern.quote( lispWords[i] ) + "\\b";
			} else {
				lispWords[i] = "\\b" + Pattern.quote( lispWords[i] ) + "";
			}
//			lispWords[i] = "(\\s)(" + Pattern.quote( lispWords[i] ) + ")(\\s)";
		}
		String r = String.join( "|" , lispWords  );
		// System.err.println( Arrays.asList( lispWords ) ); 
//		System.err.println( r );
		return r;
	}


	public static void resetStyles( JTextPane textPane ) {
		StyledDocument document = textPane.getStyledDocument();
		String text = textPane.getText();

		// Reset all styles
		{
			SimpleAttributeSet plane = new SimpleAttributeSet();
			document.setCharacterAttributes(0, text.length(), plane, true);
		}
	}

	public static void highlightSyntax( JTextPane textPane, String patternString ) {
		StyledDocument document = textPane.getStyledDocument();
		String text = textPane.getText();
		{
	    	SimpleAttributeSet bold = new SimpleAttributeSet();
	    	bold.addAttribute(StyleConstants.CharacterConstants.Bold, Boolean.TRUE );

	    	Pattern pattern = Pattern.compile( patternString );
	    	Matcher matcher = pattern.matcher( text );
	    	while ( matcher.find() ) {
	    		// System.err.println( matcher.start() + ":" + matcher.end() );
	    		document.setCharacterAttributes(
	    				matcher.start(1),
	    				matcher.end(1) - matcher.start(1),
	    				bold, true);
	    	}
	    }

	    {
	    	SimpleAttributeSet gray = new SimpleAttributeSet();
	    	gray.addAttribute(StyleConstants.ColorConstants.Foreground, Color.gray );

	    	Pattern pattern = Pattern.compile( ";.*$|\\#\\|[\\w\\W]*?\\|\\#", Pattern.MULTILINE );
	    	Matcher matcher = pattern.matcher( text );
	    	while ( matcher.find() ) {
	    		// System.err.println( matcher.start() + ":" + matcher.end() );
	    		document.setCharacterAttributes(
	    				matcher.start(),
	    				matcher.end() - matcher.start(),
	    				gray, true);
	    	}
	    }
	}
	
	public static void highlightSyntax( JTextPane textPane, Collection<String> keywordList ) {
		StyledDocument document = textPane.getStyledDocument();
		String text = textPane.getText();
		{
	    	SimpleAttributeSet bold = new SimpleAttributeSet();
	    	bold.addAttribute(StyleConstants.CharacterConstants.Bold, Boolean.TRUE );

	    	for ( String keyword : keywordList ) {
	    		String patternString = "(^|\\s|[\\(])(" + Pattern.quote( keyword ) + ")($|\\b|[\\)])";
	    		
	    		Pattern pattern = Pattern.compile( patternString );
	    		Matcher matcher = pattern.matcher( text );
	    		while ( matcher.find() ) {
	    			// System.err.println( matcher.start() + ":" + matcher.end() );
	    			document.setCharacterAttributes(
	    					matcher.start(2),
	    					matcher.end(2) - matcher.start(2),
	    					bold, true);
	    		}
	    	}
	    }

	    {
	    	SimpleAttributeSet gray = new SimpleAttributeSet();
	    	gray.addAttribute(StyleConstants.ColorConstants.Foreground, Color.gray );

	    	Pattern pattern = Pattern.compile( ";.*$|\\#\\|[\\w\\W]*?\\|\\#", Pattern.MULTILINE );
	    	Matcher matcher = pattern.matcher( text );
	    	while ( matcher.find() ) {
	    		// System.err.println( matcher.start() + ":" + matcher.end() );
	    		document.setCharacterAttributes(
	    				matcher.start(),
	    				matcher.end() - matcher.start(),
	    				gray, true);
	    	}
	    }
	}


	public static void highlightParentheses(StyledDocument doc, int open_pos, int close_pos) {
		SimpleAttributeSet attr = new SimpleAttributeSet();
		StyleConstants.setBackground( attr, Color.GREEN );
		doc.setCharacterAttributes(open_pos, 1, attr, true);
		doc.setCharacterAttributes(close_pos, 1, attr, false);
		attr = new SimpleAttributeSet();
		//		StyleConstants.setBold(attr, true);
		doc.setCharacterAttributes(open_pos, close_pos - open_pos + 1, attr, false);
	}

	public static void highlightMatchingParenthesis(JTextPane pane, int position) {
		String text = pane.getText();
		ParserState parserState = 
				SimpleSchemeParenthesisChecker.lookupParenthesis( text, position );
		if ( parserState.isFound() )
			highlightParentheses( pane.getStyledDocument(), parserState.getIterator().getInitialIndex(), parserState.getIterator().getIndex() );
	}
}

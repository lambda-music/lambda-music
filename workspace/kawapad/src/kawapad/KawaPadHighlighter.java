/*
 * Kawapad written by Atsushi Oka 
 * Copyright 2018 Atsushi Oka
 *
 * This file is part of Metro Musical Sequencing Framework. 
 * 
 * Kawapad is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Kawapad is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Kawapad.  If not, see <https://www.gnu.org/licenses/>.
 */

package kawapad;

import java.awt.Color;
import java.lang.invoke.MethodHandles;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JTextPane;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

import kawapad.SimpleSchemeParser.ParserState;

public class KawaPadHighlighter {
	static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
	static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
	static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
	static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
	
	private static final boolean DEBUG = false;
	
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
	
	public static void highlightSyntax_1( JTextPane textPane, Collection<String> keywordList ) {
		StyledDocument document = textPane.getStyledDocument();
		String text = textPane.getText();
		{
	    	SimpleAttributeSet bold = new SimpleAttributeSet();
	    	bold.addAttribute(StyleConstants.CharacterConstants.Bold, Boolean.TRUE );

	    	for ( String keyword : keywordList ) {
	    		String patternString = "(^|\\s|[\\(])(" + Pattern.quote( keyword ) + ")($|\\s|[\\)])";
	    		
	    		Pattern pattern = Pattern.compile( patternString );
	    		Matcher matcher = pattern.matcher( text );
	    		while ( matcher.find() ) {
//	    			 System.err.println(  matcher.start() + ":" + matcher.end() );
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
	
	static SimpleAttributeSet commentGray = new SimpleAttributeSet();
	static {
		commentGray.addAttribute(StyleConstants.ColorConstants.Foreground, Color.gray );
	}
	
	static  SimpleAttributeSet commentLightGray = new SimpleAttributeSet();
	static {
		commentLightGray.addAttribute(StyleConstants.ColorConstants.Foreground, new Color( 0,0,0,16 ));
	}
	
	static SimpleAttributeSet commentItalic = new SimpleAttributeSet();
	static {
		commentItalic.addAttribute( StyleConstants.Italic, true );
		commentItalic.addAttribute(StyleConstants.ColorConstants.Foreground, Color.gray );
	}
	
	static SimpleAttributeSet commentBold = new SimpleAttributeSet();
	static {
		commentBold.addAttribute(StyleConstants.CharacterConstants.Bold, Boolean.TRUE );
		commentBold.addAttribute(StyleConstants.ColorConstants.Foreground, Color.gray );
	}
	
	static SimpleAttributeSet commentUnderline = new SimpleAttributeSet();
	static {
		commentUnderline.addAttribute(StyleConstants.Underline , Boolean.TRUE );
		commentUnderline.addAttribute(StyleConstants.ColorConstants.Foreground, Color.gray );
	}
	
	static Pattern patternComment  = Pattern.compile( ";.*$|\\#\\|[\\S\\s]*?\\|\\#", Pattern.MULTILINE );
	static Pattern patternCommentItalic = Pattern.compile( "//(\\S*?)//", Pattern.MULTILINE );
	static Pattern patternCommentBold = Pattern.compile( "\\|\\|(\\S*?)\\|\\|", Pattern.MULTILINE );
	static Pattern patternCommentUnderline = Pattern.compile( "__(\\S*?)__", Pattern.MULTILINE );

	public static void highlightSyntax( JTextPane textPane, Collection<String> keywordList ) {
		if ( DEBUG ) 
			logInfo("highlightSyntax() begin >>");
		StyledDocument document = textPane.getStyledDocument();
		String text = textPane.getText();
		{
	    	SimpleAttributeSet bold = new SimpleAttributeSet();
	    	bold.addAttribute(StyleConstants.CharacterConstants.Bold, Boolean.TRUE );

	    	{
	    		String patternString = lispWordToPatternString( keywordList );
	    		
	    		Pattern pattern = Pattern.compile( patternString );
	    		Matcher matcher = pattern.matcher( text );
	    		while ( matcher.find() ) {
//	    			 System.err.println(  matcher.start() + ":" + matcher.end() );
	    			document.setCharacterAttributes(
	    					matcher.start(),
	    					matcher.end() - matcher.start(),
	    					bold, true);
	    		}
	    	}
	    }

	    {
			Matcher matcher = patternComment.matcher( text );
	    	while ( matcher.find() ) {
	    		// System.err.println( matcher.start() + ":" + matcher.end() );
	    		document.setCharacterAttributes(
	    				matcher.start(),
	    				matcher.end() - matcher.start(),
	    				commentGray, true);
	    		
	    		{
	    			String substring = text.substring( 
	    				matcher.start(),
	    				matcher.end() );

	    			Matcher matcher2 = patternCommentItalic.matcher( substring );
	    			while ( matcher2.find() ) {
	    				// System.err.println( matcher.start() + ":" + matcher.end() );
	    				document.setCharacterAttributes(
	    					matcher.start()+ matcher2.start(),
	    					matcher2.end() - matcher2.start() + 1,
	    					commentLightGray, true);
	    				
	    				document.setCharacterAttributes(
	    					matcher.start()+matcher2.start(1),
	    					matcher2.end(1) - matcher2.start(1),
	    					commentItalic, true);
	    			}
	    			
	    			Matcher matcher3 = patternCommentBold.matcher( substring );
	    			while ( matcher3.find() ) {
	    				// System.err.println( matcher.start() + ":" + matcher.end() );
	    				document.setCharacterAttributes(
	    					matcher.start()+matcher3.start(),
	    					matcher3.end() - matcher3.start() + 1,
	    					commentLightGray, true);
	    				
	    				document.setCharacterAttributes(
	    					matcher.start()+matcher3.start(1),
	    					matcher3.end(1) - matcher3.start(1),
	    					commentBold, true);
	    			}
	    			Matcher matcher4 = patternCommentUnderline.matcher( substring );
	    			while ( matcher4.find() ) {
	    				// System.err.println( matcher.start() + ":" + matcher.end() );
	    				document.setCharacterAttributes(
	    					matcher.start()+matcher4.start(),
	    					matcher4.end() - matcher4.start() + 1,
	    					commentLightGray, true);
	    				
	    				document.setCharacterAttributes(
	    					matcher.start()+matcher4.start(1),
	    					matcher4.end(1) - matcher4.start(1),
	    					commentUnderline, true);
	    			}
	    		}
	    	}
	    }
		if ( DEBUG ) 
			logInfo("highlightSyntax() end <<");
	}

	static class ClearPosition {
		StyledDocument document;
		int open_pos;
		int close_pos;
		public ClearPosition(StyledDocument document, int open_pos, int close_pos) {
			super();
			this.document = document;
			this.open_pos = open_pos;
			this.close_pos = close_pos;
		}
		public void clear() {
			document.setCharacterAttributes( open_pos,  1, new SimpleAttributeSet(), true );
			document.setCharacterAttributes( close_pos, 1, new SimpleAttributeSet(), true );
		}
	}

	static ArrayDeque<ClearPosition> clearQueue = new ArrayDeque<>();
	static void addClearQueue(StyledDocument document, int open_pos, int close_pos) {
		synchronized ( clearQueue ) {
			ClearPosition cp = new ClearPosition( document, open_pos, close_pos);
			clearQueue.push(cp);
		}
	}
	static void eliminateClearQueue() {
		synchronized ( clearQueue ) {
			for ( ClearPosition cp : clearQueue ) {
				cp.clear();
			}
			clearQueue.clear();
		}
	}
	static void popClearQueue() {
		synchronized ( clearQueue ) {
			if ( ! clearQueue.isEmpty() ) {
				clearQueue.pop().clear();
			}
		}
	}
	
	static void highlightParentheses(StyledDocument document, int open_pos, int close_pos) {
		SimpleAttributeSet attr = new SimpleAttributeSet();
		StyleConstants.setBackground( attr, Color.GREEN );
		
		document.setCharacterAttributes( open_pos,  1, attr, true );
		document.setCharacterAttributes( close_pos, 1, attr, true );
		
		addClearQueue(document, open_pos, close_pos);

//		Element open_elem  = doc.getCharacterElement(open_pos );
//		Element close_elem = doc.getCharacterElement(close_pos);
		
//		final Timer timer = new Timer( 500, new ActionListener() {
//			@Override
//			public void actionPerformed(ActionEvent e) {
//				popClearQueue();
////				System.err.println("parentheses::the attribute removed");
//			}
//		});
//		timer.setRepeats(false);
//		timer.start();
	}

	public static void forceClearHighlightedParenthesis() {
		eliminateClearQueue();
	}
	public static void clearHighlightedParenthesis() {
		popClearQueue();
	}

	public static void highlightMatchingParenthesis(JTextPane pane, int position) {
		if ( KawaPad.ENABLED_SHOW_CORRESPONDING_PARENTHESES ) {
			String text = pane.getText();
			ParserState parserState = 
					SimpleSchemeParenthesisChecker.lookupParenthesis( text, position );
			if ( parserState.isFound() )
				highlightParentheses( pane.getStyledDocument(), parserState.getIterator().getInitialIndex(), parserState.getIterator().getIndex() );
		}
	}
}

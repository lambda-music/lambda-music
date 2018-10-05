package ats.pulsar.editor;

import java.awt.Color;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

import ats.pulsar.editor.SimpleSchemeParser.ParserState;
import ats.pulsar.editor.lib.GroupedUndoManager;

public class PulsarScratchPadTextPaneController implements CaretListener, DocumentListener  {
	public static interface PulsarScratchPadTextPaneListener {
		Collection<String> getLispWords();
	}
	
	public static PulsarScratchPadTextPaneController create( JTextPane textPane, PulsarScratchPadTextPaneListener listener ) {
		return new PulsarScratchPadTextPaneController(textPane, listener );
	}

	JTextPane textPane;
	PulsarScratchPadTextPaneListener listener;
	GroupedUndoManager undoManager;
	public PulsarScratchPadTextPaneController(JTextPane textPane, PulsarScratchPadTextPaneListener listener) {
		super();
		this.textPane = textPane;
		this.listener = listener;
		textPane.getDocument().addDocumentListener( this );
		textPane.addCaretListener( this );
	}

	// CaretListener
	public void caretUpdate(CaretEvent e) {
		System.err.println("PulsarScratchPadTextPaneController.caretUpdate()");
//		undoManager.notifySignificant();

//		if ( undoManager != null )
//			undoManager.startGroup();
//
		if ( ! undoManager.isSuspended() )
			invokeUpdateMarker();
	}

	//DocumentListener
	public void insertUpdate(DocumentEvent e) {
		System.err.println("PulsarScratchPadTextPaneController.insertUpdate()");
//		if ( undoManager != null )
//			undoManager.notifySignificant();
		
//		if ( ! undoManager.isSuspended() )
//			invokeUpdateMarker();
	}
	public void removeUpdate(DocumentEvent e) {
		System.err.println("PulsarScratchPadTextPaneController.removeUpdate()");
//		if ( undoManager != null )
//			undoManager.notifySignificant();

//		if ( ! undoManager.isSuspended() )
//			invokeUpdateMarker();
	}
	public void changedUpdate(DocumentEvent e) {
		System.err.println("PulsarScratchPadTextPaneController.changedUpdate() : ignored");
		return;
	}

	void invokeUpdateMarker() {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				lookupMatchingParenthesis();
			}
		});
	}
	
	void resetStyles() {
		StyledDocument document = textPane.getStyledDocument();
		String text = textPane.getText();

		// Reset all styles
		{
			SimpleAttributeSet plane = new SimpleAttributeSet();
			document.setCharacterAttributes(0, text.length(), plane, true);
		}
		
	    {
	    	SimpleAttributeSet bold = new SimpleAttributeSet();
	    	bold.addAttribute(StyleConstants.CharacterConstants.Bold, Boolean.TRUE );

	    	Pattern pattern = Pattern.compile( getLispWordPattern() );
	    	Matcher matcher = pattern.matcher( text );
	    	while ( matcher.find() ) {
	    		// System.err.println( matcher.start() + ":" + matcher.end() );
	    		document.setCharacterAttributes(
	    				matcher.start(),
	    				matcher.end() - matcher.start(),
	    				bold, true);
	    	}
	    }

	    {
	    	SimpleAttributeSet gray = new SimpleAttributeSet();
	    	gray.addAttribute(StyleConstants.ColorConstants.Foreground, Color.gray );

	    	Pattern pattern = Pattern.compile( ";.*$|\\#\\|[\\w\\W]*\\|\\#", Pattern.MULTILINE );
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
	public static void markParentheses(StyledDocument doc, int open_pos, int close_pos) {
		SimpleAttributeSet attr = new SimpleAttributeSet();
		StyleConstants.setBackground( attr, Color.GREEN );
		doc.setCharacterAttributes(open_pos, 1, attr, true);
		doc.setCharacterAttributes(close_pos, 1, attr, false);
		attr = new SimpleAttributeSet();
		//		StyleConstants.setBold(attr, true);
		doc.setCharacterAttributes(open_pos, close_pos - open_pos + 1, attr, false);
	}


	private String getLispWordPattern() {
		Collection<String> lispWordCollection = listener.getLispWords();
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
		}
		String r = String.join( "|" , lispWords  );
		// System.err.println( Arrays.asList( lispWords ) ); 
//		System.err.println( r );
		return r; 
	}

	

	public void lookupMatchingParenthesis() {
		lookupMatchingParenthesis(textPane, textPane.getCaretPosition());
	}

	public void lookupMatchingParenthesis(JTextPane pane, int position) {
		String text = pane.getText();
		resetStyles();
		ParserState parserState = 
				SimpleSchemeParenthesisChecker.lookupParenthesis( text, position );
		if ( parserState.isFound() )
			markParentheses( pane.getStyledDocument(), parserState.getIterator().getInitialIndex(), parserState.getIterator().getIndex() );
	}
}

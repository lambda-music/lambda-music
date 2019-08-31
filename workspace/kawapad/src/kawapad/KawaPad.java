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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.HeadlessException;
import java.awt.MenuBar;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import javax.swing.text.DefaultCaret;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.DefaultEditorKit.DefaultKeyTypedAction;
import javax.swing.text.JTextComponent;
import javax.swing.text.TextAction;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;

import gnu.expr.Language;
import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.Procedure1;
import gnu.mapping.Procedure2;
import gnu.mapping.Procedure3;
import gnu.mapping.Symbol;
import gnu.mapping.Values;
import kawa.standard.Scheme;
import kawapad.SimpleSchemeParser.ParserState;
import kawapad.lib.CompoundGroupedUndoManager;
import kawapad.lib.GroupedUndoManager;
import pulsar.lib.PulsarLogger;
import pulsar.lib.scheme.SchemeUtils;
import pulsar.lib.scheme.SchemeUtils.ExecuteSchemeResult;
import pulsar.lib.scheme.scretary.SchemeSecretary;
import pulsar.lib.secretary.SecretaryMessage;
import pulsar.lib.secretary.SecretaryMessage.NoReturnNoThrow;
import pulsar.lib.swing.Action2;

/**
 * 
 * (Tue, 09 Jul 2019 10:28:51 +0900)
 * <ol>
 * <li>Every scheme object must be initialized by {@link KawaPad#staticInitScheme(Scheme)}</li>
 * <li>{@link KawaPad#initialize() } must be called before use the object.</li>
 * </ol>
 * <pre> 
 * new KawaPad( initSchemeForScratchPad( new Scheme() ) ).initialize();
 * </pre>
 * 
 * There are several global variables which are fundamental to this tool.
 * 
 * - scheme
 *     A reference to the current instance of {@link Scheme} class.
 *   
 * - frame
 *     A reference to the current frame where the script was invoked.
 *     Note that kawa is not multithread safe. In kawa only once thread 
 *     can be executed at once.
 *  
 * @author Ats Oka
 */
public class KawaPad extends JFrame {
	static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
	static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
	static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
	static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
	private static final String FLAG_DONE_INIT_PULSAR_SCRATCHPAD = "flag-done-init-pulsar-scratchpad";
	private static final boolean DEBUG_UNDO_BUFFER = false;

	/**
	 * This is a map for debugging or something. This map is intended to be used for
	 * keeping values from Scheme.
	 * 
	 * The current environment is frequently scrapped and replaced in KawaPad/Pulsar
	 * system. In Scheme, you cannot keep the same value between those multiple
	 * environments. This map is intended to be used as a place to keep values
	 * without environments. (Sat, 17 Aug 2019 13:10:44 +0900)
	 */
	public static final Map<Object,Object> memoMap = new HashMap<Object,Object>();
	
	///////////////////////////////////////////////////////////////////////////////////////////////

	private static final boolean DEBUG = false;

	///////////////////////////////////////////////////////////////////////////////////////////////

	

	/**
	 * Initialize variables which is necessary to set whenever the environment is created.
	 * One of such variables is a reference to the frame object. This reference must be
	 * cleared when the frame is disposed.
	 */
	public static void registerLocalSchemeInitializers( SchemeSecretary schemeSecretary, KawaPad kawaPad ) {
		schemeSecretary.registerSchemeInitializer( kawaPad, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
			@Override
			public void execute0( Scheme scheme, Object[] args ) {
				kawaPad.initScheme( scheme );				
			}
		});
//		WARNING This should be done only in init(); (Tue, 06 Aug 2019 18:07:49 +0900)
//		schemeSecretary.registerSchemeInitializer( kawaPad, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
//			@Override
//			public void execute0( Scheme scheme, Object[] args ) {
//				logInfo( "eventinvokeEventHandler of KawaPad#registerLocalSchemeInitializers " );
////				eventHandlers.invokeEventHandler( kawaPad, EventHandlers.INIT );
//				eventHandlers.invokeEventHandler( kawaPad, EventHandlers.CREATE );
//			}
//		});
	}
	
	public static void invokeLocalSchemeInitializers( SchemeSecretary schemeSecretary, KawaPad kawaPad ) {
		schemeSecretary.invokeSchemeInitializers( kawaPad );
	}

	/**
	 * Remove initializers that initialize variables for the current frame.
	 */
	public static void unregisterLocalSchemeInitializers(SchemeSecretary schemeSecretary, KawaPad kawaPad ) {
		schemeSecretary.unregisterSchemeInitializer( kawaPad );
	}

	/**
	 * This initializes variables which do not need to refer the reference to the
	 * current frame. This initializer does not have to be removed even if  
	 * frames are disposed.
	 */
	public static void registerGlobalSchemeInitializer( SchemeSecretary schemeSecretary ) {
		schemeSecretary.registerSchemeInitializer( KawaPad.class, staticInitializer01 );
		schemeSecretary.registerSchemeFinalizer( KawaPad.class, new NoReturnNoThrow<Scheme>() {
			@Override
			public void execute0(Scheme scheme, Object[] args) {
				logInfo("finalizer() eventHandlers.clear()");
				eventHandlers.clear();
			}
		});
	}
	static SecretaryMessage.NoReturnNoThrow<Scheme> staticInitializer01 = new SecretaryMessage.NoReturnNoThrow<Scheme>() {
		@Override
		public void execute0( Scheme scheme, Object[] args ) {
			staticInitScheme( scheme );				
		}
	};
	// I added this for the sake of symmetricity, but this didn't use it.
	// I left it for future use. (Mon, 12 Aug 2019 14:24:38 +0900)
	public static void unregisterGlobalSchemeInitializer( SchemeSecretary schemeSecretary ) {
		schemeSecretary.unregisterSchemeFinalizer( KawaPad.class );
	}

	///////////////////////////////////////////////////////////////////////////////////////////////
	
	private final String frameName = newFrameName();
	public String getFrameName() {
		return this.frameName;
	}
	
	// private Scheme scheme;

	protected SchemeSecretary schemeSecretary;
	public KawaPad createKawaPad() {
		return new KawaPad( this.schemeSecretary, this.getTitle() ); 
	}
	
	public KawaPad( SchemeSecretary schemeSecretary, String title ) throws HeadlessException {
		super(title);
		this.schemeSecretary = schemeSecretary;
		this.setDefaultCloseOperation( DO_NOTHING_ON_CLOSE );
		
		registerLocalSchemeInitializers( schemeSecretary, this );
//		DELETED >>> INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
//		invokeLocalSchemeInitializers( schemeSecretary, this);
//		DELETED <<< INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
	
	}
	
	/**
	 * KawaPad#init() must be called whenever any KawaPad instance is created. Due
	 * to INIT_03 (see the comment in the source code), the constructor cannot call
	 * this method directory; we decided to mandate the users to call this method
	 * manually.
	 */

//	ADDED >>> (Tue, 06 Aug 2019 09:29:54 +0900)
	public void init() {
//		ADDED >>> (Tue, 06 Aug 2019 08:47:14 +0900)
		/*
		 * At that time, I didn't realize that creation of a frame should be done in a INIT_03
		 * different way from the creation of a scheme object. (Tue, 06 Aug 2019 08:47:14 +0900) 
		 */
		eventHandlers.invokeEventHandler( KawaPad.this, EventHandlers.CREATE,  KawaPad.this );
//		ADDED <<< (Tue, 06 Aug 2019 08:47:14 +0900)
	}
//	ADDED <<< (Tue, 06 Aug 2019 09:29:54 +0900)

	
	public void quit() {
		this.dispatchEvent( new WindowEvent(this, WindowEvent.WINDOW_CLOSING) );
	}
	
	@Override
	public void dispose() {
		unregisterLocalSchemeInitializers( this.schemeSecretary, this );
		super.dispose();
	}

	
	public class KawaPadTextPane extends JTextPane {
		private KawaPad kawaPad;
		public KawaPadTextPane( KawaPad kawaPad ) {
			super();
			this.kawaPad = kawaPad;
		}
		public KawaPad getKawaPad() {
			return kawaPad;
		}
	}

	
	public final AbstractAction NEW_SCRATCHPAD_ACTION = new AbstractAction() {
		@Override
		public void actionPerformed(ActionEvent e) {
			createKawaPad();
		}
		{
			putValue( Action2.NAME, "Create a New Scratchpad" );
			putValue( Action.MNEMONIC_KEY, (int)'n' );
			putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_N, ActionEvent.CTRL_MASK) );
		}
	};

	

	public final class ScratchPadThreadManager {
		private final class ScratchPadThread extends Thread {
			private final Runnable r;
			private ScratchPadThread(Runnable r) {
				this.r = r;
			}

			@Override
			public void run() {
				try {
					if ( DEBUG )
						logInfo( "ScratchPadThreadManager:run" );
					// ==== WORKAROUND SEE acvpoeov === (Tue, 23 Jul 2019 11:37:32 +0900) //
//					schemeSecretary.initializeSchemeForCurrentThread();
					// ==== WORKAROUND SEE acvpoeov === (Tue, 23 Jul 2019 11:37:32 +0900) //
					r.run();
				} finally {
					if ( DEBUG )
						logInfo( "ScratchPadThreadManager:end" );
					removeScratchPadThread( this );
				}
			}
			@Override
			public void interrupt() {
				logInfo("interrupted");
				super.interrupt();
			}
		}
		private ArrayDeque<Thread> scratchPadThreadList = new ArrayDeque<>();
		public void addScratchPadThread( Thread t ) {
			synchronized ( scratchPadThreadList ) {
				scratchPadThreadList.add( t );
			}
		}
		public void startScratchPadThread( Runnable r ) {
			Thread t = new ScratchPadThread(r);
			addScratchPadThread(t);
			t.start();
		}
		public void removeScratchPadThread( Thread t ) {
			synchronized ( scratchPadThreadList ) {
				scratchPadThreadList.remove( t );
			}
		}
		public void interruptScratchPadThreads() {
			logInfo("interruptScratchPadThreads");
			synchronized ( scratchPadThreadList ) {
				for ( Thread t : scratchPadThreadList ) {
					logInfo( "interrupt start" );
					t.interrupt();
					logInfo( "interrpt end" );
				}
			}
		}
	}
	
	final ScratchPadThreadManager threadManager = new ScratchPadThreadManager();
	public ScratchPadThreadManager getThreadManager() {
		return threadManager;
	}

	///////////////////////////////////////////////////////////////////////////////////

//	private final class InsertTextToTextPane implements Runnable {
//		private final String result;
//		private boolean isThereSelection;
//		private InsertTextToTextPane(String result, boolean isThereSelection) {
//			this.result = result;
//			this.isThereSelection = isThereSelection;
//		}
//		
//		@Override
//		public void run() {
//			try {
//				if ( textPane.getSelectedText() != null ) {
//					try {
//						undoManager.startGroup();
//						undoManager.setSuspended(true);
//						int selectionEnd = textPane.getSelectionEnd();
//						textPane.getDocument().insertString( selectionEnd, result, null);
//						textPane.setSelectionEnd( selectionEnd + result.length() );
//						textPane.setSelectionStart(selectionEnd + 1 );
//					} finally {
//						undoManager.setSuspended(false);
//						undoManager.startGroup();
//					}
//				} else {
//					try {
//						undoManager.startGroup();
//						undoManager.setSuspended(true);
//						int dot = textPane.getCaret().getDot();
//						textPane.getDocument().insertString( textPane.getText().length(), result, null);
//						textPane.getCaret().moveDot(dot);
//					} finally {
//						undoManager.setSuspended(false);
//						undoManager.startGroup();
//					}
//				}
//			} catch (BadLocationException e1) {
//				e1.printStackTrace();
//			}
//		}
//	}
	private final class SetTextToTextPane implements Runnable {
		private final File file;
		private final String text;
		private SetTextToTextPane( File file, String text ) {
			this.file = file;
			this.text = text;
		}
		@Override
		public void run() {
			setTextProc( file, text );
		}
	}

	private final class InsertTextToTextPane implements Runnable {
		private final String result;
		private InsertTextToTextPane( String result ) {
			this.result = result;
		}
		
		@Override
		public void run() {
			try {
				if ( textPane.getSelectedText() != null ) {
					try {
						getUndoManager().startGroup();
						getUndoManager().setSuspended(true);
						int selectionEnd = textPane.getSelectionEnd();
						textPane.getDocument().insertString( selectionEnd, result, null);
						textPane.setSelectionEnd( selectionEnd + result.length() );
						textPane.setSelectionStart(selectionEnd  );
					} finally {
						getUndoManager().setSuspended(false);
						getUndoManager().startGroup();
					}
				} else {
					try {
						getUndoManager().startGroup();
						getUndoManager().setSuspended(true);
						int dot = textPane.getCaret().getDot();
						textPane.getDocument().insertString( dot, result, null);
						textPane.getCaret().moveDot(dot);
					} finally {
						getUndoManager().setSuspended(false);
						getUndoManager().startGroup();
					}
				}
				logInfo( "InsertTextToTextPane() done" );
				updateHighlightLater();
			} catch (BadLocationException e1) {
				e1.printStackTrace();
			}
		}
	}
	
	private final class ReplaceTextOnTextPane implements Runnable {
		private final String result;
		private ReplaceTextOnTextPane( String result ) {
			this.result = result;
		}
		
		@Override
		public void run() {
			try {
				if ( textPane.getSelectedText() != null ) {
					try {
						getUndoManager().startGroup();
						getUndoManager().setSuspended(true);
						textPane.replaceSelection( result );
					} finally {
						getUndoManager().setSuspended(false);
						getUndoManager().startGroup();
					}
				} else {
					try {
						getUndoManager().startGroup();
						getUndoManager().setSuspended(true);
						int dot = textPane.getCaret().getDot();
						textPane.getDocument().insertString( dot, result, null);
						textPane.getCaret().moveDot(dot);
					} finally {
						getUndoManager().setSuspended(false);
						getUndoManager().startGroup();
					}
				}
				logInfo( "ReplaceTextOnTextPane() done" );
				updateHighlightLater();
			} catch (BadLocationException e1) {
				e1.printStackTrace();
			}
		}
	}
	
	private final class ReplaceTextWithEntireBlockOnTextPane implements Runnable {
		private final String result;
		private ReplaceTextWithEntireBlockOnTextPane( String result ) {
			this.result = result;
		}
		
		@Override
		public void run() {
			try {
				if ( textPane.getSelectedText() != null ) {
					try {
						getUndoManager().startGroup();
						getUndoManager().setSuspended(true);

						// In order to avoid entering an infinite loop,
						// we use /for/ loop instead of /while/ loop;
						for ( int i=0; i<100; i++ ) {
							if ( expandSelectedParentheses( textPane ) ) {
								break;
							}
						}
						textPane.replaceSelection( result );
					} finally {
						getUndoManager().setSuspended(false);
						getUndoManager().startGroup();
					}
				} else {
					try {
						getUndoManager().startGroup();
						getUndoManager().setSuspended(true);
						int dot = textPane.getCaret().getDot();
						textPane.getDocument().insertString( dot, result, null);
						textPane.getCaret().moveDot(dot);
					} finally {
						getUndoManager().setSuspended(false);
						getUndoManager().startGroup();
					}
				}
				logInfo( "ReplaceTextWithEntireBlockOnTextPane() done" );
				updateHighlightLater();
			} catch (BadLocationException e1) {
				e1.printStackTrace();
			}
		}
	}
	public class PulsarScratchPadListener implements CaretListener, DocumentListener  {
		public PulsarScratchPadListener() {
			super();
			textPane.getDocument().addDocumentListener( this );
			textPane.addCaretListener( this );
		}
		// CaretListener
		public void caretUpdate(CaretEvent e) {
			checkSelectionStack();
//			System.err.println("PulsarScratchPadTextPaneController.caretUpdate()");
			if ( ! getUndoManager().isSuspended() ) {
				updateHighlightParenthesesLater();
				eventHandlers.invokeEventHandler( KawaPad.this, EventHandlers.CARET,  KawaPad.this);
				eventHandlers.invokeEventHandler( KawaPad.this, EventHandlers.CHANGE,  KawaPad.this);
			}
		}
		//DocumentListener
		public void insertUpdate(DocumentEvent e) {
			fileModified = true;
//			System.err.println("PulsarScratchPadTextPaneController.insertUpdate()");
			if ( ! getUndoManager().isSuspended() ) {
				updateHighlightLater();
				eventHandlers.invokeEventHandler( KawaPad.this, EventHandlers.INSERT,  KawaPad.this);
				eventHandlers.invokeEventHandler( KawaPad.this, EventHandlers.CHANGE,  KawaPad.this);
			}
		}
		public void removeUpdate(DocumentEvent e) {
			fileModified = true;
//			System.err.println("PulsarScratchPadTextPaneController.removeUpdate()");
			if ( ! getUndoManager().isSuspended() ) {
				updateHighlightLater();
				eventHandlers.invokeEventHandler( KawaPad.this, EventHandlers.REMOVE,  KawaPad.this);
				eventHandlers.invokeEventHandler( KawaPad.this, EventHandlers.CHANGE,  KawaPad.this);
			}
		}
		public void changedUpdate(DocumentEvent e) {
//			fileModified = true;
//			System.err.println("PulsarScratchPadTextPaneController.changedUpdate() : ignored");
			if ( ! getUndoManager().isSuspended() ) {
				eventHandlers.invokeEventHandler( KawaPad.this, EventHandlers.ATTRIBUTE,  KawaPad.this);
				eventHandlers.invokeEventHandler( KawaPad.this, EventHandlers.CHANGE,  KawaPad.this);
			}
			return;
		}
	}
	public void updateHighlightLater() {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				updateHighlight();
			}
		});
	}
	public void updateHighlightParenthesesLater() {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				KawaPadHighlighter.forceClearHighlightedParenthesis();
				highlightMatchningParentheses();
			}

		});
	}
	
	
	String getLispWordPatternString() {
		return KawaPadHighlighter.lispWordToPatternString( getLispWords() ); 
	}
	public void updateHighlight() {
		KawaPadHighlighter.resetStyles( textPane );
//		KawaPadHighlighter.highlightSyntax( textPane, getLispWordPatternString() );
		KawaPadHighlighter.highlightSyntax( textPane, getLispWords() );
	}
	public void highlightMatchningParentheses() {
		KawaPadHighlighter.highlightMatchingParenthesis( textPane, textPane.getCaretPosition() );
	}


	/**
	 * getSelectedText() which treats its endpoint as inclusive-like.
	 * The endpoint of the current selection is to move one unit to the left.    
	 * 
	 * @param c
	 * @return
	 */
    public static String getSelectedText( JTextComponent c ) {
    	Caret caret = c.getCaret();
    	
    	String text = c.getText();
    	int dot = caret.getDot();
		int mark = caret.getMark();

		if ( dot == mark )
    		return null;

		int pos;
		int len;
		if ( dot < mark ){
    		pos = dot;
    		len = mark - pos ;
    	} else {
    		pos = mark;
    		len = dot - pos ;
    	}

		if ( pos < 0 )
			pos=0;
		if ( text.length() < pos )
			pos = text.length();
		if ( len < 0 )
			len=0;
		if ( text.length() < pos + len )
			len  = text.length() - pos;
		
    	String s;
		try {
			s = c.getText( pos,len );
		} catch (BadLocationException e) {
			s= "";
		}
    	
    	if (s.endsWith("\n")) {
//        	caret.setDot( mark );
//        	caret.moveDot( dot );
    		return s;
    	} else {
            if ( dot < mark ) {
//            	caret.setDot( mark + 1 );
//            	caret.moveDot( dot );
            	return c.getSelectedText();
            } else {
////            	caret.setDot( dot );
//            	caret.moveDot( dot + 1 );
            	return c.getSelectedText();
            }
    	}
    }

	final class EvaluateRunnable implements Runnable {
		String schemeScript;
		boolean insertText;
		boolean replaceText;
		public EvaluateRunnable(String schemeScript, boolean insertText, boolean replaceText ) {
			super();
			this.schemeScript = schemeScript;
			this.insertText = insertText;
			this.replaceText = replaceText;
		}
		@Override
		public void run() {
			logInfo( schemeScript );
			HashMap<String,Object> variables = new HashMap<>();
			variables.put( "frame", KawaPad.this );
			ExecuteSchemeResult result = SchemeUtils.evaluateScheme( schemeSecretary, variables, schemeScript, "scratchpad" );

			if ( insertText || ! result.succeeded() ) {
				if ( replaceText && result.succeeded() ) {
					if ( result.isDocument )  {
						logWarn( "**KAWAPAD_PAGE**" );
						SwingUtilities.invokeLater( new ReplaceTextWithEntireBlockOnTextPane( "(" + result.result.replaceFirst( "\n$", "" ) +" )" ) );
					} else {
						SwingUtilities.invokeLater( new ReplaceTextOnTextPane( result.result ) );
					}
				} else {
					String resultString = SchemeUtils.formatResult( result.result ); 
					// We want to make sure the result string ends with "\n" to avoid to get an extra line.
					if ( ! schemeScript.endsWith( "\n" ) ) {
						resultString = "\n" + SchemeUtils.formatResult( result.result ); 
					}
					logInfo( resultString );
					SwingUtilities.invokeLater( new InsertTextToTextPane( resultString ) );
				}
			}
		}
	}

	public final AbstractAction RESET_ACTION = new ResetAction();
	private final class ResetAction extends AbstractAction {
		@Override
		public void actionPerformed(ActionEvent e) {
			schemeSecretary.newScheme();
		}
		{
			putValue( Action2.NAME, "Reset the Environment" );
			putValue( Action.MNEMONIC_KEY, (int)'s' );
//			putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_E, ActionEvent.CTRL_MASK) );
		}
	}
	String getTextDefault() {
		String schemeScript;
		{
			schemeScript = getSelectedText( textPane );
			if ( schemeScript == null ) {
				schemeScript =  textPane.getText();
			}
		}
		return schemeScript;
	}

	public final AbstractAction EVALUATE_REPLACE_ACTION = new EvaluateReplaceAction();
	private final class EvaluateReplaceAction extends AbstractAction {
		@Override
		public void actionPerformed(ActionEvent event) {
			String schemeScript;
			{
				schemeScript = getSelectedText( textPane );
				if ( schemeScript == null ) {
					textPane.getActionMap().get( DefaultEditorKit.backwardAction ).actionPerformed( event );
					PARENTHESIS_SELECT_ACTION.actionPerformed( event );
					SwingUtilities.invokeLater( new Runnable() {
						@Override
						public void run() {
							String schemeScript2 = getSelectedText( textPane );
							threadManager.startScratchPadThread( new EvaluateRunnable( schemeScript2,  true, true ) );
						}
					});

				} else {
					threadManager.startScratchPadThread( new EvaluateRunnable( schemeScript,  true, true ) );
				}
			}

		}
		{
			putValue( Action2.NAME, "Evaluate Replace" );
			putValue( Action.MNEMONIC_KEY, (int)'t' );
			putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, ActionEvent.CTRL_MASK) );
		}
	}

	public final AbstractAction EVALUATE_ACTION = new EvaluateAction();
	private final class EvaluateAction extends AbstractAction {
		@Override
		public void actionPerformed(ActionEvent e) {
			//	JOptionPane.showMessageDialog( JPulsarScratchPad.this, "", "AAAA" , JOptionPane.INFORMATION_MESSAGE  );
			threadManager.startScratchPadThread( new EvaluateRunnable( getTextDefault(), true, false ) );
		}
		{
			putValue( Action2.NAME, "Evaluate" );
			putValue( Action.MNEMONIC_KEY, (int)'e' );
			putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_E, ActionEvent.CTRL_MASK) );
		}
	}

	public final AbstractAction RUN_ACTION = new RunAction();
	private final class RunAction extends AbstractAction {
		@Override
		public void actionPerformed(ActionEvent e) {
			//	JOptionPane.showMessageDialog( JPulsarScratchPad.this, "", "AAAA" , JOptionPane.INFORMATION_MESSAGE  );
			threadManager.startScratchPadThread( new EvaluateRunnable( getTextDefault(), false, false ) );
		}
		{
			putValue( Action2.NAME, "Run" );
			putValue( Action.MNEMONIC_KEY, (int)'r' );
			putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_R, ActionEvent.CTRL_MASK) );
		}
	}

	public void insertText( String t ) {
//		boolean isThereSelection=true;
//		String text = textPane.getSelectedText();
//		if ( text == null ) {
//			text = textPane.getText();
//			isThereSelection = false;
//		}
//		isThereSelection = true;
//		
//		// ??? IS THIS NECESSARY?
//		textPane.getActionMap();
		SwingUtilities.invokeLater( new InsertTextToTextPane(t) );
	}
	public void setNewText( String t ) throws IOException {
		if ( ! confirmSave( ConfirmType.OPEN_FILE ) ) {
			return;
		}
		SwingUtilities.invokeLater( new SetTextToTextPane(null, t) );
	}

	public final AbstractAction INTERRUPT_ACTION = new InterruptAction();
	private final class InterruptAction extends AbstractAction {
		@Override
		public void actionPerformed(ActionEvent e) {
			threadManager.interruptScratchPadThreads();
		}
		{
			putValue( Action2.NAME, "Interrupt" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_K, ActionEvent.CTRL_MASK) );
			putValue( Action.MNEMONIC_KEY , (int) 'i' );
		}
	}
	
	
	
	
	static class ParenthesisAction extends TextAction {
		public static final int STRATEGY_DYNAMIC = -1024;
		public static final int STRATEGY_SIMPLE_PARENTHESIS_JUMP = 1;
		public static final int STRATEGY_CORRESPONDING_PARENTHESIS_JUMP = 2;
		public static int lookupCorrespondingParenthesis2(String text, int currDot, int direction, int constantStrategy ) throws InternalError {
			char currentChar = text.charAt( currDot );
			int totalOffset = 0;
			
			// 0 : do nothing
			// 1 : look for "(" or ")"
			// 2 : look for the corresponding parenthesis.
			int strategy;
			
			// constantStrategy < 0 means dynamic strategy 
			// (Tue, 13 Aug 2019 21:59:23 +0900)
			if ( constantStrategy == STRATEGY_DYNAMIC ) {
				switch ( currentChar ) {
					case '(' :
						if ( direction < 0 ) {
							totalOffset += -1;
							strategy = STRATEGY_SIMPLE_PARENTHESIS_JUMP ;
						} else {
							totalOffset +=  0;
							strategy = STRATEGY_CORRESPONDING_PARENTHESIS_JUMP;
						}
						break;
					case ')' : 
						if ( direction < 0 ) {
							totalOffset +=  0;
							strategy = STRATEGY_CORRESPONDING_PARENTHESIS_JUMP;
						} else {
							totalOffset +=  1;
							strategy = STRATEGY_SIMPLE_PARENTHESIS_JUMP;
						}
						break;
					default :
						strategy = 1;
				}
			} else {
				switch ( currentChar ) {
					case '(' :
						if ( direction < 0 ) {
							totalOffset += -1;
							strategy = STRATEGY_SIMPLE_PARENTHESIS_JUMP ;
						} else {
							totalOffset +=  1;
							strategy = STRATEGY_SIMPLE_PARENTHESIS_JUMP;
						}
						break;
					case ')' : 
						if ( direction < 0 ) {
							totalOffset +=  -1;
							strategy = STRATEGY_SIMPLE_PARENTHESIS_JUMP;
						} else {
							totalOffset +=  1;
							strategy = STRATEGY_SIMPLE_PARENTHESIS_JUMP;
						}
						break;
					default :
						strategy = 1;
				}
			}
			
			int newDot=-1;
			
			switch ( strategy ) {
				case 0 : 
					// do nothing
					newDot = currDot + totalOffset;
					break;
				case STRATEGY_SIMPLE_PARENTHESIS_JUMP : { 
					// strategy 1 : no parenthesis is found under the cursor.
					int pos = lookupParenthesis(text, currDot + totalOffset, direction );
					if ( 0<=pos ) {
						newDot = pos;
					} else {
						if ( direction < 0 )
							newDot = 0;
						else
							newDot = text.length();
					}
					break;
				}
				case STRATEGY_CORRESPONDING_PARENTHESIS_JUMP : { 
					// strategy 0: a parenthesis is found under the cursor.
					int pos = lookupCorrespondingParenthesis( text, currDot + totalOffset );
					if ( 0<=pos ) {
						newDot = pos;
					} else {
						newDot = currDot + totalOffset;
					}
					break;
				}
				default :
					throw new InternalError();
			}
			return newDot;
		}

		boolean doSelect = false;
		int direction = 0;
		int constantStrategy; // <0 means dynamic strategy (Tue, 13 Aug 2019 21:59:23 +0900)
		ParenthesisAction( String name, boolean doSelect, int direction, int constantStrategy ) {
			super(name);
			this.doSelect = doSelect;
			this.direction = direction;
			this.constantStrategy = constantStrategy;
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			JTextPane textPane = (JTextPane) getTextComponent(e);
			String text = textPane.getText();
			Caret caret = textPane.getCaret();
			int currDot = caret.getDot();
			int newDot = lookupCorrespondingParenthesis2(text, currDot, direction, constantStrategy );
			if ( doSelect ) {
				caret.moveDot( newDot );
			} else {
				caret.setDot( newDot );
			}
		}
	}

	public final AbstractAction SIMPLE_PARENTHESIS_JUMP_LEFT_ACTION = new ParenthesisAction( "simple-parenthesis-jump-left", false, -1, ParenthesisAction.STRATEGY_SIMPLE_PARENTHESIS_JUMP ) {
		{
			putValue( Action2.NAME, "Go to the Previous Parenthesis" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_LEFT, KeyEvent.ALT_MASK ) );
//			putValue( Action.MNEMONIC_KEY , (int) 'd' );
		}
	};
	public final AbstractAction SIMPLE_PARENTHESIS_JUMP_RIGHT_ACTION = new ParenthesisAction( "simple-parenthesis-jump-right", false, +1, ParenthesisAction.STRATEGY_SIMPLE_PARENTHESIS_JUMP  ) {
		{
			putValue( Action2.NAME, "Go to the Next Parenthesis" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_RIGHT, KeyEvent.ALT_MASK ) );
//			putValue( Action.MNEMONIC_KEY , (int) 'd' );
		}
	};
	public final AbstractAction SIMPLE_PARENTHESIS_SELECT_JUMP_LEFT_ACTION = new ParenthesisAction( "simple-parenthesis-select-jump-left", true, -1, ParenthesisAction.STRATEGY_SIMPLE_PARENTHESIS_JUMP ) {
		{
			putValue( Action2.NAME, "Select the Previous Parenthesis" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_LEFT, KeyEvent.ALT_MASK |KeyEvent.SHIFT_MASK ) );
//			putValue( Action.MNEMONIC_KEY , (int) 'd' );
		}
	};
	public final AbstractAction SIMPLE_PARENTHESIS_SELECT_JUMP_RIGHT_ACTION = new ParenthesisAction( "simple-parenthesis-select-jump-right", true, +1, ParenthesisAction.STRATEGY_SIMPLE_PARENTHESIS_JUMP  ) {
		{
			putValue( Action2.NAME, "Select the Next Parenthesis" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_RIGHT, KeyEvent.ALT_MASK | KeyEvent.SHIFT_MASK ) );
//			putValue( Action.MNEMONIC_KEY , (int) 'd' );
		}
	};
	
	public final AbstractAction PARENTHESIS_JUMP_LEFT_ACTION = new ParenthesisAction( "parenthesis-jump-left", false, -1, ParenthesisAction.STRATEGY_DYNAMIC ) {
		{
			putValue( Action2.NAME, "Lookup the Corresponding Parenthesis on the Left" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_LEFT, KeyEvent.ALT_MASK | KeyEvent.CTRL_MASK ) );
//			putValue( Action.MNEMONIC_KEY , (int) 'd' );
		}
	};
	public final AbstractAction PARENTHESIS_JUMP_RIGHT_ACTION = new ParenthesisAction( "parenthesis-jump-right", false, +1, ParenthesisAction.STRATEGY_DYNAMIC  ) {
		{
			putValue( Action2.NAME, "Lookup the Corresponding Parenthesis on the Right" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_RIGHT, KeyEvent.ALT_MASK | KeyEvent.CTRL_MASK  ) );
//			putValue( Action.MNEMONIC_KEY , (int) 'd' );
		}
	};
	public final AbstractAction PARENTHESIS_SELECT_JUMP_LEFT_ACTION = new ParenthesisAction( "parenthesis-sel-jump-left", true, -1, ParenthesisAction.STRATEGY_DYNAMIC  ) {
		{
			putValue( Action2.NAME, "Lookup and Select the Corresponding Parenthesis on the Left" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_LEFT, KeyEvent.ALT_MASK | KeyEvent.CTRL_MASK | KeyEvent.SHIFT_MASK ) );
//			putValue( Action.MNEMONIC_KEY , (int) 'd' );
		}
	};
	public final AbstractAction PARENTHESIS_SELECT_JUMP_RIGHT_ACTION = new ParenthesisAction( "parenthesis-sel-jump-right", true, +1, ParenthesisAction.STRATEGY_DYNAMIC  ) {
		{
			putValue( Action2.NAME, "Lookup and Select the Corresponding Parenthesis to the Right" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_RIGHT, KeyEvent.ALT_MASK | KeyEvent.CTRL_MASK | KeyEvent.SHIFT_MASK ) );
//			putValue( Action.MNEMONIC_KEY , (int) 'd' );
		}
	};
	
	class ParenthesisStackElement {
		int mark;
		int dot;
		public ParenthesisStackElement(int mark, int dot) {
			super();
			this.mark = mark;
			this.dot = dot;
		}
	}
	
	transient boolean parenthesisStackLocked = false;
	ArrayDeque<ParenthesisStackElement> parenthesisStack = new ArrayDeque<>();
	void checkSelectionStack() {
		synchronized ( parenthesisStack ) {
			if ( ! parenthesisStackLocked ) {
				parenthesisStack.clear();
			}
		}
	}

	class ParenthesisSelectAction extends TextAction {
		ParenthesisSelectAction(String name) {
			super(name);
		}
		
		@Override
		public void actionPerformed(ActionEvent e) {
			JTextPane textPane = (JTextPane) getTextComponent(e);
			expandSelectedParentheses( textPane );
		}
	}
	public static final int THE_FINAL_CORRECTION = 1;
	boolean expandSelectedParentheses(JTextPane textPane) {
		String text  = textPane.getText();
		Caret caret  = textPane.getCaret();
		int currDot  = caret.getDot();
		int currMark = caret.getMark();
		int leftPos;
		int rightPos;
		if ( currDot < currMark ) {
			leftPos = currDot;
			rightPos = currMark - THE_FINAL_CORRECTION;
		} else {
			leftPos = currMark;
			rightPos = currDot - THE_FINAL_CORRECTION;
		}
		
		// if there is a selection area now, it is to expand one on the left side.
		if ( leftPos != rightPos )
			rightPos ++;
		else if ( text.charAt(leftPos) == '(') {
			leftPos ++;
			rightPos++;
		}
		
		if ( leftPos < 0 )
			leftPos = 0;
		else if ( text.length() < leftPos )
			leftPos = text.length();
		if ( rightPos < 0 )
			rightPos = 0;
		else if ( text.length() < rightPos )
			rightPos = text.length();
		
		String left_leftString   = text.substring(0, leftPos);
		String left_rightString  = text.substring(leftPos);
		String right_leftString  = text.substring(0, rightPos);
		String right_rightString = text.substring(rightPos);
		int posL;
		int posR;
		int diff; // the length in char of the inserted text in the middle of the argument string.
		
		/*
		 * We will search twice :
		 *  - once for simply looking for the corresponding parenthesis.
		 *   - once we presume that we are in a block of quotations. We well try to close it before searching
		 *     the corresponding parenthesis; otherwise, we will search for the next half quotation which
		 *     is not what we want.
		 */
		{
			// the first search
			diff = 1;
			posL = lookupCorrespondingParenthesis( left_leftString + ")" + left_rightString, leftPos );
			posR = lookupCorrespondingParenthesis( right_leftString + "(" + right_rightString, rightPos );
			
			if ( 0<=posL && 0<=posR ) {
				synchronized ( parenthesisStack ) {
					try {
						parenthesisStackLocked = true;
						caret.setDot(posL);
						caret.moveDot(posR-diff + THE_FINAL_CORRECTION);
						parenthesisStack.push(new ParenthesisStackElement(currMark, currDot));
						return true;
					} finally {
						parenthesisStackLocked = false;
					}
				}
			}
		}
		{
			// the second search
			diff = 2;
			posL = lookupCorrespondingParenthesis( left_leftString + "(\"" + left_rightString, leftPos   );
			posR = lookupCorrespondingParenthesis( right_leftString + "\")" + right_rightString, rightPos +1 );
			if ( 0<=posL && 0<=posR ) {
				synchronized ( parenthesisStack ) {
					try {
						parenthesisStackLocked = true;
						caret.setDot(posL-diff);
						caret.moveDot(posR + THE_FINAL_CORRECTION);
						parenthesisStack.push(new ParenthesisStackElement(currMark, currDot));
						return true;
					} finally {
						parenthesisStackLocked = false;
					}
				}
			}
			return false;
		}
	}	
	public final AbstractAction PARENTHESIS_SELECT_ACTION = new ParenthesisSelectAction( "parenthesis-select" ) {
		{
			putValue( Action2.NAME, "Select Inside the Current Parentheses" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke( KeyEvent.VK_UP, KeyEvent.ALT_MASK | KeyEvent.SHIFT_MASK ) );
//			putValue( Action.MNEMONIC_KEY , (int) 'd' );
		}
	};
	
	class ParenthesisSelect2Action extends TextAction {
		ParenthesisSelect2Action(String name) {
			super(name);
		}
		@Override
		public void actionPerformed(ActionEvent e) {
			JTextPane textPane = (JTextPane) getTextComponent(e);
			String text  = textPane.getText();
			Caret caret  = textPane.getCaret();
			int currDot  = caret.getDot();
			int currMark = caret.getMark();
			int leftPos;
			int rightPos;
			if ( currDot < currMark ) {
				leftPos = currDot;
				rightPos = currMark;
			} else {
				leftPos = currMark;
				rightPos = currDot;
			}
			
			int posL;
			int posR;
			{
				posL = WordJumpAction.lookup( leftPos,  text , -1 );
				posR = WordJumpAction.lookup( rightPos, text ,  1 );
				
				if ( 0<=posL && 0<=posR ) {
					synchronized ( parenthesisStack ) {
						try {
							parenthesisStackLocked = true;
							caret.setDot(posL);
							caret.moveDot(posR);
							parenthesisStack.push(new ParenthesisStackElement(currMark, currDot));
							return;
						} finally {
							parenthesisStackLocked = false;
						}
					}
				}
			}
		}
	}
	public final AbstractAction PARENTHESIS_SELECT_2_ACTION = new ParenthesisSelect2Action("parenthesis-select-2-action") {
		{
			putValue( Action2.NAME, "Deselect Inside the Current Parentheses" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke( KeyEvent.VK_UP, KeyEvent.CTRL_MASK | KeyEvent.ALT_MASK ) );
//			putValue( Action.MNEMONIC_KEY , (int) 'd' );
		}
	};

	class ParenthesisDeselectAction extends TextAction {
		ParenthesisDeselectAction(String name) {
			super(name);
		}
		@Override
		public void actionPerformed(ActionEvent e) {
			JTextPane textPane = (JTextPane) getTextComponent(e);
			synchronized ( parenthesisStack ) {
				try {
					parenthesisStackLocked = true;
					if ( textPane.getSelectedText() != null ) {
						if ( ! parenthesisStack.isEmpty() ) {
							ParenthesisStackElement elem = parenthesisStack.pop();
							Caret caret = textPane.getCaret();
							caret.setDot( elem.mark );
							caret.moveDot( elem.dot );
						}
					} else {
						parenthesisStack.clear();
					}
				} finally {
					parenthesisStackLocked = false;
				}
			}
		}
	}
	public final AbstractAction PARENTHESIS_DESELECT_ACTION = new ParenthesisDeselectAction( "parenthesis-deselect" ) {
		{
			putValue( Action2.NAME, "Deselect Inside the Current Parentheses" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke( KeyEvent.VK_DOWN, KeyEvent.ALT_MASK | KeyEvent.SHIFT_MASK ) );
//			putValue( Action.MNEMONIC_KEY , (int) 'd' );
		}
	};
	public final AbstractAction PARENTHESIS_DESELECT_2_ACTION = new ParenthesisDeselectAction( "parenthesis-deselect-2" ) {
		{
			putValue( Action2.NAME, "Deselect Inside the Current Parentheses" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke( KeyEvent.VK_DOWN, KeyEvent.CTRL_MASK | KeyEvent.ALT_MASK ) );
//			putValue( Action.MNEMONIC_KEY , (int) 'd' );
		}
	};

	abstract class TextFilter {
		abstract String process( String text );
	}

	static void formatProc(JTextComponent textPane, TextFilter filter ) {
		String text = textPane.getText();
		boolean reverse = textPane.getCaret().getMark() < textPane.getCaret().getDot()  ;
		int beginIndex;
		int endIndex;
		int min = Math.min( textPane.getCaret().getDot(), textPane.getCaret().getMark() );
		int max = Math.max( textPane.getCaret().getDot(), textPane.getCaret().getMark() );
		String postfix = "";
		
		boolean isThereSelection;
		
		// if there is a selected area :
		if ( min < max ) {
			isThereSelection = true;
			beginIndex = SimpleSchemeIndentChanger.lookupLineStart(text, min  );
			endIndex = SimpleSchemeIndentChanger.lookupLineEnd(text, max );
			postfix = "\n";
		} else {
			isThereSelection = false;
			
			/*
			 * Check if the position is on the head of a line : The first position of any
			 * line is treated as a part of the previous line. See the lookupLineEnd() 's
			 * comment.
			 */
			if ( min ==0 || text.charAt(min-1 ) == '\n' ) {
				beginIndex = min;
				endIndex = SimpleSchemeIndentChanger.lookupLineEnd(text, min+1 );
				postfix = "\n";
			} else {
				beginIndex = SimpleSchemeIndentChanger.lookupLineStart(text, min  );
				endIndex   = SimpleSchemeIndentChanger.lookupLineEnd(text, max );
				postfix = "\n";
			}
		}
		
		String selectedText = text.substring(beginIndex, endIndex );
		String modifiedText = filter.process( selectedText ) + postfix;
		
		textPane.setSelectionStart( beginIndex );
		textPane.setSelectionEnd(   endIndex   );
		textPane.replaceSelection( modifiedText );
		
		if (isThereSelection ) {
			if ( reverse ) {
				textPane.setCaretPosition(  beginIndex );
				textPane.moveCaretPosition( beginIndex + modifiedText.length() );
			} else {
				textPane.setCaretPosition(  beginIndex + modifiedText.length() );
				textPane.moveCaretPosition( beginIndex );
			}
		} else {
			int spaces = SimpleSchemeIndentChanger.countFirstSpaces( textPane.getText().substring( beginIndex ) );
			textPane.setCaretPosition(  beginIndex + spaces );
//			textPane.moveCaretPosition( beginIndex + spaces );
		}
	}

	private class FormatAction extends AbstractAction {
		int difference;
		public FormatAction(int difference) {
			super();
			this.difference = difference;
		}
		@Override
		public void actionPerformed(ActionEvent e) {
			//	JOptionPane.showMessageDialog( JPulsarScratchPad.this, "", "AAAA" , JOptionPane.INFORMATION_MESSAGE  );
			try {
				getUndoManager().startGroup();
				getUndoManager().setSuspended(true);
				formatProc( textPane, new TextFilter() {
					@Override
					String process(String text) {
						return SimpleSchemeIndentChanger.changeIndentRelativeMultiline( text, difference );
					}
				});
			} finally {
				getUndoManager().setSuspended(false);
				getUndoManager().startGroup();

				/*
				 * (Fri, 05 Oct 2018 02:20:49 +0900)
				 * 
				 * Note that this calling startGroup() after setSuspended(false) is necessary.
				 * Continuing the process without starting a new group here, causes problems.
				 * Without starting a new group here, undoing after performing any text format
				 * actions with selecting any part of the formatted block causes throwing an
				 * exception, and then the synchronization between the document and undo buffer
				 * will be broken.
				 * 
				 */
			}
		}
	}
	public final AbstractAction INCREASE_INDENT_ACTION = new FormatAction( +2 ) {
		{
			putValue( Action2.NAME, "Increase Indentation" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_TAB , 0 ) ) ;
			putValue( Action.MNEMONIC_KEY , (int) 'c' );
		}
	};
	public final AbstractAction DECREASE_INDENT_ACTION = new FormatAction( -2 ) {
		{
			putValue( Action2.NAME, "Decrease Indentation" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_TAB , KeyEvent.SHIFT_MASK ) );
			putValue( Action.MNEMONIC_KEY , (int) 'd' );
		}
	};
	
	private class PrettifyAction extends AbstractAction {
		@Override
		public void actionPerformed(ActionEvent e) {
			//	JOptionPane.showMessageDialog( JPulsarScratchPad.this, "", "AAAA" , JOptionPane.INFORMATION_MESSAGE  );
			formatProc( textPane, new TextFilter() {
				@Override
				String process(String text) {
					return prettify( text );
				}
			});
		}
	}
	
	public static final String prettify( Collection<String> lispWords, String text  ) {
		return SimpleSchemePrettifier.prettify( lispWords, text );
	}
	public static final String prettify( Environment env, String text ) {
		return prettify( getLispWords0( env ), text );
	}
	public final String prettify( String text ) {
		return schemeSecretary.executeSecretarially( new SecretaryMessage.NoThrow<Scheme, String>() {
			@Override
			public String execute0( Scheme scheme, Object[] args ) {
				return prettify( scheme.getEnvironment(), text );
			}
		}, text );
	}

	public final AbstractAction PRETTIFY_ACTION = new PrettifyAction() {
		{
			putValue( Action2.NAME, "Correct Indentation" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_I , KeyEvent.CTRL_MASK ));
			putValue( Action.MNEMONIC_KEY , (int) 'i' );
		}
	};

	

	////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// 
	// Defining an interface the for scheme interpreter. 
	//
	////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/*
	 *  
	 * 
	 */

	private static List<String> FALLBACK_LISP_WORDS = Arrays.asList( "let", "lambda" );

	public Collection<String> getLispWords() {
		return schemeSecretary.executeSecretarially( new SecretaryMessage.NoThrow<Scheme, Collection<String>>() {
			@Override
			public Collection<String> execute0(Scheme scheme, Object[] args) {
				return getLispWords0( scheme.getEnvironment() );
			}
		});
	}

	public static Collection<String> getLispWords0( Environment env ) {
		Collection<String> lispWords = FALLBACK_LISP_WORDS;
		try {
			Object object = env.get( Symbol.valueOf("lisp-words") );
			if ( object instanceof Pair ) {
				Pair p = (Pair) object;
				lispWords = SchemeUtils.<Object,String>convertList( p, (o)->{
					return SchemeUtils.anyToString( o );
				});
			}
		} catch ( Throwable t ) {
			logError("", t);
		}
		return lispWords;
	}

	
	////////////////////////////////////////////////////////////////////////////

	static final class SchemeProcedure {
		/*
		 *  The variable environment and language are not necessary anymore
		 *  but there are many that setting these so left them their be.
		 *  This class should also be replaced to Invokable.
		 *  (Wed, 24 Jul 2019 16:21:59 +0900)
		 */
		@SuppressWarnings("unused")
		private final Environment environment;
		@SuppressWarnings("unused")
		private final Language language;
		private final Procedure procedure;
		SchemeProcedure( Procedure procedure , Environment environmen ) {
			this.environment = environmen;
			this.language = Language.getDefaultLanguage();
			this.procedure = procedure;
		}
		public Object invoke( Object... args ) {
			try {
//				Environment.setCurrent( this.environment );
//				Environment.setCurrent( environment );
//				Language.setCurrentLanguage( this.language );
				return procedure.applyN( args );
			} catch (Throwable e) {
				logError( "SchemeInvokableProcedure:error" , e );
				return e;
				//					throw new RuntimeException(e);
			}
		}
	}

	
	public static class EventHandlers {
		private static final String INIT      = "init";   // occurs when initializing scheme objects. (Tue, 06 Aug 2019 08:37:12 +0900)
		private static final String CREATE    = "create"; // occurs when creating form objects.       (Tue, 06 Aug 2019 08:37:12 +0900)
		private static final String CARET     = "caret";
		private static final String INSERT    = "insert";
		private static final String REMOVE    = "remove";
		private static final String ATTRIBUTE = "attribute";
		private static final String CHANGE    = "change";
		private static final String TYPED     = "typed";

		final Map<Symbol,Map<Symbol,SchemeProcedure>> map = new HashMap<>();
		{
			map.put( Symbol.valueOf(INIT),      new HashMap<>() );
			map.put( Symbol.valueOf(CREATE),    new HashMap<>() );
			map.put( Symbol.valueOf(CARET),     new HashMap<>() );
			map.put( Symbol.valueOf(INSERT),    new HashMap<>() );
			map.put( Symbol.valueOf(REMOVE),    new HashMap<>() );
			map.put( Symbol.valueOf(ATTRIBUTE), new HashMap<>() );
			map.put( Symbol.valueOf(CHANGE),    new HashMap<>() );
			map.put( Symbol.valueOf(TYPED),     new HashMap<>() );
		}
		Map<Symbol, SchemeProcedure> getEventType(Symbol eventTypeID) {
			Map<Symbol, SchemeProcedure> eventType = map.get( eventTypeID );
			if ( eventType == null )
				throw new RuntimeException( "unknown event type + ( " + eventTypeID + ")"  );
			return eventType;
		}
		public void clear() {
			logInfo("EventHandlers#clear()");
			map.get( Symbol.valueOf(INIT)).clear();
			map.get( Symbol.valueOf(CREATE)).clear();
			map.get( Symbol.valueOf(CARET)).clear();
			map.get( Symbol.valueOf(INSERT)).clear();
			map.get( Symbol.valueOf(REMOVE)).clear();
			map.get( Symbol.valueOf(ATTRIBUTE)).clear();
			map.get( Symbol.valueOf(CHANGE)).clear();
			map.get( Symbol.valueOf(TYPED)).clear();
		}
		Map<Symbol, SchemeProcedure> getEventType(String eventTypeID) {
			return getEventType( Symbol.valueOf(eventTypeID));
		}
		
		public void register( Symbol eventTypeID, Symbol procID, Procedure proc ) {
			register( eventTypeID, procID, new SchemeProcedure( proc, Environment.getCurrent() ) );
		}
		public void register( Symbol eventTypeID, Symbol procID, SchemeProcedure proc ) {
			Map<Symbol, SchemeProcedure> eventType = getEventType(eventTypeID);
			eventType.put( procID, proc );
		}
		public void unregister( Symbol eventTypeID, Symbol procID ) {
			Map<Symbol, SchemeProcedure> eventType = getEventType(eventTypeID);
			eventType.remove( procID );
		}
		public void invokeEventHandler( KawaPad kawaPad, String eventTypeID, Object ... args ) {
//			logInfo( "eventHandlers.invokeEventHandler(outer)" );
			kawaPad.schemeSecretary.executeSecretarially( new SecretaryMessage.NoReturnNoThrow<Scheme>() {
				@Override
				public void execute0( Scheme scheme, Object[] args ) {
					kawaPad.threadManager.startScratchPadThread( new Runnable() {
						@Override
						public void run() {
							kawaPad.schemeSecretary.initializeSchemeForCurrentThread();
							synchronized ( scheme ) {
								//	logInfo( "eventHandlers.invokeEventHandler(inner)" );
								Environment env = scheme.getEnvironment();
								try {
									SchemeUtils.putVar( env, "scheme", scheme );
									SchemeUtils.putVar( env, "frame", kawaPad );
									
									for( Entry<Symbol,SchemeProcedure> e :  getEventType(eventTypeID).entrySet() ) {
										try {
											e.getValue().invoke( args );
										} catch ( Throwable t ) {
											logError("invoking event handlers : ", t);
										}
									}
									
								} finally {
									SchemeUtils.putVar( env, "scheme", false );
									SchemeUtils.putVar( env, "frame", false );
								}
								
							}
						}
					});
				}
			}, kawaPad );
		}
		
//		void invokeEventHandlers( String )
		
	}
	public static final EventHandlers eventHandlers = new EventHandlers();

	static transient int frameCounter = 0;
	static String getFrameName( int frameCounter ) {
		return "frame" + frameCounter;
	}
	synchronized static String newFrameName() {
		return "frame" + ( frameCounter ++ );
	}

	public static File getInitFile() {
		return new File( System.getProperty("user.home"), ".pulsar/kawapad-extension.scm" );
	}

	protected void initScheme( Scheme scheme ) {
		logInfo( "KawaPad#initScheme" );
		Environment env = scheme.getEnvironment();
		SchemeUtils.defineVar( env, this, frameName );

		textualIncrementalAddon.initScheme( env );
	}

	public static Scheme staticInitScheme( Scheme scheme ) {
		logInfo( "KawaPad#staticInitScheme" );
		SchemeSecretary.initializeSchemeForCurrentThreadStatic( scheme );
		Environment env = scheme.getEnvironment();
		
		if ( ! SchemeUtils.isDefined(env, FLAG_DONE_INIT_PULSAR_SCRATCHPAD ) ) {
			SchemeUtils.defineVar(env, true, FLAG_DONE_INIT_PULSAR_SCRATCHPAD );  

			SchemeUtils.defineVar(env, false, "frame"  );
			SchemeUtils.defineVar(env, false, "scheme" );

			SchemeUtils.defineVar(env, 
					Pair.makeList( (List)SchemeUtils.<String,IString>convertList( 
							Arrays.asList( DEFAULT_LISP_WORDS ),
							(o)->{
								return SchemeUtils.toSchemeString( o );
							}) 
						), "lisp-words");

			SchemeUtils.defineVar(env, Pair.makeList( (List)SchemeUtils.<String,IString>convertList( 
										Arrays.asList( DEFAULT_LISP_WORDS ),
										(o)->{
											return SchemeUtils.toSchemeString( o );
										}) 
									), "default-lisp-words"
					);
			
			SchemeUtils.defineVar(env, new Procedure3() {
				@Override
				public Object apply3(Object arg1, Object arg2, Object arg3) throws Throwable {
					eventHandlers.register( (Symbol)arg1, (Symbol)arg2, (Procedure) arg3 );
					return EmptyList.emptyList;
				}
			}, "register-event-handler");
			SchemeUtils.defineVar(env, new Procedure2() {
				@Override
				public Object apply2(Object arg1, Object arg2 ) throws Throwable {
					eventHandlers.unregister((Symbol)arg1,(Symbol)arg2 );
					return EmptyList.emptyList;
				}
			}, "unregister-event-handler");
			SchemeUtils.defineVar(env, new Procedure1() {
				@Override
				public Object apply1(Object arg1 ) throws Throwable {
					return prettify( scheme.getEnvironment(), SchemeUtils.anyToString(SchemeUtils.prettyPrint(arg1)));
				}
			}, "pretty-print");
			SchemeUtils.defineVar(env, new Procedure1() {
				@Override
				public Object apply1(Object arg1 ) throws Throwable {
					return prettify( scheme.getEnvironment(), SchemeUtils.anyToString(arg1));
				}
			}, "prettify");

			try {
				logInfo( "Loading [KawaPad internal]/kawapad-extension.scm" );
				SchemeUtils.execScheme( KawaPad.class, scheme, "kawapad-extension.scm" );
			} catch (Throwable e) {
				logError( "Ignored an error : ", e);
			}

			// Read user's configuration file. If any problem is occurred, print its
			// stacktrace in the stderr, and then continue the process.
			try {
				File initFile = getInitFile();
				logInfo( "Loading " + initFile.getName() );
				if ( initFile.exists() ) {
					SchemeUtils.execScheme( scheme, new FileInputStream( initFile ), initFile.getPath() );
				} else {
					logInfo( "The user configuration file \"" + initFile.getPath() + "\" does not exist. Ignored." );
				}
			} catch (Throwable e) {
				logError( "Ignored an error : ", e);
			}


		}
		return scheme;
	}

	


	////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//
	// Defining GUI
	//
	////////////////////////////////////////////////////////////////////////////////////////////////////////////

	static void purgeKeyFromActionMap( ActionMap actionMap, Object key ) {
		actionMap.remove(key);
		if ( actionMap.getParent() != null )
			purgeKeyFromActionMap(actionMap.getParent(), key );
	}
	
	protected Container scratchPadRoot;
	protected JTextPane textPane;
	protected PulsarScratchPadListener textPaneController;
	protected JScrollPane scrollPane; 
	protected JMenuBar menuBar;

	public JTextPane getTextPane() {
		return textPane;
	}
	@Override
	public MenuBar getMenuBar() {
		return super.getMenuBar();
	}

	public Container getScratchPadRootPane() {
		return getContentPane();
	}
	
	{
		textPane = new KawaPadTextPane( this ) {
			// Special thanks go to tips4java
			// https://tips4java.wordpress.com/2009/01/25/no-wrap-text-pane/
			public boolean getScrollableTracksViewportWidth() {
				// return getUI().getPreferredSize(this).width 
				//      <= getParent().getSize().width;
				return getUI().getPreferredSize(this).width 
						< getParent().getSize().width;
			}
		};
		scrollPane = new JScrollPane( textPane );
		textPaneController = new PulsarScratchPadListener();
		scratchPadRoot = new JPanel( new BorderLayout() );
		getContentPane().add(scratchPadRoot );
		
		scratchPadRoot.add( scrollPane, BorderLayout.CENTER );
        textPane.setFont( new Font("monospaced", Font.PLAIN, 12));
        
        /*
		 * (Sun, 07 Oct 2018 23:50:37 +0900) CREATING_KEYMAP
		 * 
		 * THIS IS VERY IMPORTANT. I SPEND THREE SLEEPLESS NIGHTS TO FIND THIS OPERATION
		 * IS NECESSARY. This keymap object is SHARED as default! Those key handlers on
		 * a keymap object will be definitely overridden unless you explicitly create a
		 * new keymap object.
		 * 
		 * See CREATING_KEYMAP
		 */
        textPane.setKeymap( JTextComponent.addKeymap( getFrameName(), textPane.getKeymap() ) );
        
		DefaultCaret dc = new DefaultCaret() {
		    @Override
		    public void paint(Graphics g) {

		        if (isVisible()) {

		            JTextComponent comp = getComponent();
		            if (comp == null) {
		                return;
		            }

		            Rectangle r = null;
		            try {
		                r = comp.modelToView(getDot());
		                if (r == null) {
		                    return;
		                }
		            } catch (BadLocationException e) {
		                return;
		            }
		            if (isVisible()) {
		                g.setXORMode(Color.MAGENTA );
		                g.fillRect(r.x, r.y , r.width+5, r.height);
		            }
		        }
		    }
		};
		dc.setBlinkRate(500);
		textPane.setCaret( dc );


		// This action intercepts our customization so delete it.
		purgeKeyFromActionMap( textPane.getActionMap(), DefaultEditorKit.insertTabAction );
		
		{
//			Action inserBreakAction = textPane.getActionMap().get( DefaultEditorKit.insertBreakAction );
			Action newInsertBreakAction = new TextAction( DefaultEditorKit.insertBreakAction ) {
				@Override
				public void actionPerformed(ActionEvent e) {
//					logInfo("YEAH!");
		            JTextComponent target = getTextComponent(e);
		            if (target != null) {
		                if ((! target.isEditable()) || (! target.isEnabled())) {
		                    UIManager.getLookAndFeel().provideErrorFeedback(target);
		                    return;
		                }
		                
		                
		                try {
		                	getUndoManager().startGroup();
		                	getUndoManager().setSuspended(true);

		                	String text = target.getText();
		                	int pos = target.getCaretPosition();
		                	String indentString = calculateIndentSize(text, pos, getLispWords());
		                	target.replaceSelection( "\n" + indentString );
		                } finally {
		                	getUndoManager().setSuspended(false);
		                	getUndoManager().startGroup();
		                }
		            }
				}

			};
//			purgeKeyFromActionMap( textPane.getActionMap(), DefaultEditorKit.insertBreakAction );
			textPane.getActionMap().put(DefaultEditorKit.insertBreakAction, newInsertBreakAction );
		}
	}
	
	public static final String calculateIndentSize( String text, int pos, Collection<String> lispWords ) {
		return SimpleSchemePrettifier.calculateIndentSize( text, pos, lispWords );
	}

	///////////////////////////////////////////////////////////////////////////////////
	private final GroupedUndoManager undoManager;
	{
		// https://stackoverflow.com/questions/2547404/using-undo-and-redo-for-jtextarea
//		undoManager = new InsignificantUndoManager();
//		undoManager = new LazyGroupedUndoManager();
//		undoManager = new SimpleCompoundUndoManager();
		this.undoManager = new CompoundGroupedUndoManager();
//		undoManager = new OriginalCompoundUndoManager( textPane );
	}
	public GroupedUndoManager getUndoManager() {
		return undoManager;
	}
	
	private abstract static class UndoRedoAction extends AbstractAction {
		protected final GroupedUndoManager undoManager;
		protected UndoRedoAction( String name,  GroupedUndoManager undoManager ) {
			super(name);
			this.undoManager = undoManager;
		}
	}

	public final Action UNDO_ACTION = new UndoAction( "Undo", getUndoManager() );
	static class UndoAction extends UndoRedoAction {
		public UndoAction(String name, GroupedUndoManager manager ) {
			super(name,manager);
		}

		public void actionPerformed(ActionEvent actionEvent) {
			logInfo( "do UNDO" );
			try {
				undoManager.undo();
			} catch (CannotUndoException e) {
				if ( DEBUG_UNDO_BUFFER )
					logError("could not undo", e);
				else
					logInfo( "could not undo" );
				// showMessage(actionEvent.getSource());
			}
		}
		{
			putValue( Action2.NAME, "Undo" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_Z , KeyEvent.CTRL_MASK ));
			putValue( Action.MNEMONIC_KEY , (int) 'u' );
		}
	}

	public final Action REDO_ACTION = new RedoAction( "Redo", getUndoManager() );
	static class RedoAction extends UndoRedoAction {
		public RedoAction(String name, GroupedUndoManager manager) {
			super(name,manager);
		}
		public void actionPerformed(ActionEvent actionEvent) {
			logInfo( "do REDO" );
			try {
				undoManager.redo();
			} catch (CannotRedoException e) {
				if ( DEBUG_UNDO_BUFFER )
					logError("could not redo", e);
				else
					logInfo( "could not redo" );
//				showMessage(actionEvent.getSource());
			}
		}
		{
			putValue( Action2.NAME, "Redo" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_Z , KeyEvent.CTRL_MASK | KeyEvent.SHIFT_MASK ));
			putValue( Action.MNEMONIC_KEY , (int) 'r' );
		}
	}

	public final Action DEBUG_ACTION = new DebugAction( "Debug" );
	class DebugAction extends AbstractAction {
		public DebugAction(String string) {
			super(string);
		}
		@Override
		public void actionPerformed(ActionEvent e) {
			getUndoManager().dump();
		}
		{
			putValue( Action2.NAME, "Debug" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_BACK_QUOTE, KeyEvent.CTRL_MASK | KeyEvent.ALT_MASK ));
			putValue( Action.MNEMONIC_KEY , (int) 'd' );
		}
	}
	
	public final Action PASTE_ACTION = new PasteAction();
    class PasteAction extends TextAction {

        /** Create this object with the appropriate identifier. */
        public PasteAction() {
            super(DefaultEditorKit.pasteAction);
        }

        /**
         * The operation to perform when this action is triggered.
         *
         * @param e the action event
         */
        public void actionPerformed(ActionEvent e) {
        	logInfo("KawaPad.PasteAction.actionPerformed()");
            JTextComponent target = getTextComponent(e);
            if (target != null) {
            	try {
            		getUndoManager().startGroup();
            		getUndoManager().setSuspended(true);
            		target.paste();
            	} finally {
            		getUndoManager().setSuspended(false);
            		getUndoManager().startGroup();
            	}
            }
        }
		{
			putValue( Action2.NAME, "Paste" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_V , KeyEvent.CTRL_MASK ));
			putValue( Action.MNEMONIC_KEY , (int) 'p' );
		}
    }
    {
    	textPane.getActionMap().put( DefaultEditorKit.pasteAction , PASTE_ACTION );
    }

    public Action KEYMAP_DEFAULT = new DefaultKeyTypedAction() {
		@Override
		public void actionPerformed(ActionEvent e) {
			super.actionPerformed(e);
			
			JTextPane target = (JTextPane) getTextComponent(e);
            if ((target != null) && (e != null)) {
                String content = e.getActionCommand();
//                logInfo( "typed : " + content );
                switch ( content ) {
                	case " " :
                		getUndoManager().startGroup();
                		break;
                		
                	case "(" :
                	case ")" :
                		int pos = textPane.getCaretPosition() -1;
                		logInfo( "caret : " + pos );
                		KawaPadHighlighter.highlightMatchingParenthesis( textPane, pos ); 
//						SwingUtilities.invokeLater(hilightRunnable);
                		break;
                		
                	default :
                		break;
                }
                
				eventHandlers.invokeEventHandler( KawaPad.this, EventHandlers.TYPED, target, SchemeUtils.toSchemeString( content ) );
            }
		}
	};
	
	public static final int lookupParenthesis( String text, int position, int step ) {
		if ( text == null )
			throw new NullPointerException();
		
		if ( step == 0 )
			throw new IllegalArgumentException();
		
		while ( 0<= position && position < text.length() ) {
			char c = text.charAt( position );
			if ( c == '(' || c == ')' ) {
				return position;
			} else {
				position += step;
			}
		}
		return -1;
	}
	
	public static final int lookupCorrespondingParenthesis( String text, int position ) {
		ParserState parserState = 
				SimpleSchemeParenthesisChecker.lookupParenthesis( text, position );
		if ( parserState.isFound() ) {
			return parserState.getIterator().getIndex();
		} else {
			return -1;
		}
	
	}
	
	
	////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// 
	// TextualIncrement ( TEXTUAL_INCREMENT )
	//
	////////////////////////////////////////////////////////////////////////////////////////////////////////////

	static class TextualIncrementalAddon {
		public void initGui( JMenu file, JMenu edit, JMenu view, JMenu scheme ) {
			edit.add( TEXTUAL_INCREMENT_ACTION );
			edit.add( TEXTUAL_DECREMENT_ACTION );
		}
		
		public void initScheme(Environment env) {
			SchemeUtils.defineVar( env, new Procedure2() {
				@Override
				public Object apply2(Object arg1, Object arg2) throws Throwable {
					addIncrementalSymbol( 
						SchemeUtils.anyToString( arg1 ),
						SchemeUtils.anyToString( arg2 ));
					return Values.empty;
				}
			}, "add-incremental-keyword" );
			SchemeUtils.defineVar( env, new Procedure1() {
				@Override
				public Object apply1(Object arg1) throws Throwable {
					deleteIncrementalSymbol( 
						SchemeUtils.anyToString( arg1 ));
					return Values.empty;
				}
			}, "delete-incremental-keyword" );
		}

		public static final String TEXTUAL_INCREMENT = "textual-increment-action";
		public static final String TEXTUAL_DECREMENT = "textual-decrement-action";
		static class IncrementalSymbol {
			final String from;
			final String to;
			final Pattern fromPattern;
			final Pattern toPattern;
			IncrementalSymbol(String from, String to) {
				super();
				if ( from == null  )
					throw new NullPointerException( "from is null" );
				if ( to == null  )
					throw new NullPointerException( "to is null");
				
				this.from = from;
				this.to = to;
				
				this.fromPattern = Pattern.compile( "\\b" + from + "\\b" );
				this.toPattern   = Pattern.compile( "\\b" + to   + "\\b" );
			}
			@Override
			public boolean equals(Object obj) {
				if ( obj instanceof IncrementalSymbol )
					return this.from.equals( ((IncrementalSymbol)obj).from );
				else
					return false;
			}
			@Override
			public int hashCode() {
				return from.hashCode();
			}
			@Override
			public String toString() {
				return this.getClass().getName() + "(" + this.from + "->" + this.to + ")";
			}
		}
		LinkedList<IncrementalSymbol> incrementalSymbols = new LinkedList<>();
		
		void addIncrementalSymbol0( String from, String to ) {
			incrementalSymbols.add( new IncrementalSymbol( from, to ) );
		}
		void deleteIncrementalSymbol0( String from ) {
			incrementalSymbols.remove( new IncrementalSymbol( from, null ) );
		}

		public void addIncrementalSymbol( String from, String to ) {
			addIncrementalSymbol0( from, to );
		}
		public void deleteIncrementalSymbol( String from ) {
			deleteIncrementalSymbol0( from );
		}
		static Pattern NUMBER_PATTERN = Pattern.compile( "[0-9\\/\\.\\-\\+]+" );

		String zeroPad( String s, int len ) {
			StringBuilder sb = new StringBuilder();
			int slen = s.length();
			for ( int i=0; i<len-slen; i++ ) {
				sb.append( '0' );
			}
			sb.append(s);
			return sb.toString();
		}
		
		String fixedFloatAdd( String s, int n, boolean doZeroPad ) {
			String wnp; // whole-number part 
			String fp;  // fraction part
			int fpLen = -1;
			{
				int tmpPos = s.indexOf( '.' );
				if ( tmpPos < 0 ) {
					wnp = s;
					fp ="";
					fpLen = 0;
				} else {
					wnp = s.substring( 0,tmpPos );
					fp =  s.substring( tmpPos+1 );
					fpLen = s.length() - tmpPos - 1;
				}
			}
			int totalLen = wnp.length() + fp.length();
			int inValue  = Integer.parseInt( wnp+fp );
			int outValue = inValue + n;
			if ( ( inValue < 0 ) && !( outValue < 0 ) ) {
				totalLen --;
			} else if ( ( 0 <= inValue ) && !( 0 <= outValue ) ) {
				totalLen ++;
			}
			
			String resultString = String.valueOf( outValue );
			if ( doZeroPad && fp.length() !=0 ) {
				resultString = zeroPad( resultString, totalLen );
				{
					int minusPos = resultString.indexOf( '-' );
					if ( 0<minusPos ) {
						resultString = 
								"-" + 
										resultString.substring( 0, minusPos ) + 
										resultString.substring( minusPos+1, resultString.length() );
					}
				}
			}
			
			if ( fp.length() == 0 ) {
				return resultString;
			} else {
				return  ""
						+ resultString.substring( 0, resultString.length() - fpLen ) 
						+ "." 
						+ resultString.substring(    resultString.length() - fpLen ); 
			}
			
			
		}
		String fractionAdd(String numStr, int n) {
			String[] ss = numStr.split( "\\/" );
			ss[0] = String.valueOf( fixedFloatAdd( ss[0], n, !(1<ss.length) ) ); 
			return String.join( "/", ss );
		}

		void replace(JTextComponent target, int direction ) {
			String str = target.getText();
			Caret caret = target.getCaret();
			
			int pos = caret.getDot();
			if ( str.length() < pos )
				pos = str.length();
			
			int beginPos = -1;
			{
				for ( int i=pos; 0<=i; i-- ) {
					if ( Character.isWhitespace( str.charAt( i ) ) ) {
						beginPos = i;
						break;
					}
				}
				if ( beginPos == -1 )
					beginPos = 0;
			}
			int endPos = -1;
			{
				Matcher m = Pattern.compile("$", Pattern.MULTILINE ).matcher( str +"\n" );
				if ( m.find( pos ) ) {
					endPos = m.start();
				}
				
				if ( endPos == -1 )
					endPos = str.length();
			}
			
			if ( endPos <= beginPos )
				return;
			
			logInfo( "TextualIncrementAction:" + beginPos + ":" + endPos );
			String targetStr = str.substring( beginPos , endPos );

			String foundSubstr=null;
			int foundBeginPos=Integer.MAX_VALUE;
			int foundEndPos =Integer.MAX_VALUE;

			{
				if ( foundSubstr == null ) {
					Matcher m = NUMBER_PATTERN.matcher( targetStr );
					if ( m.find() ) {
						Scheme scheme = ((KawaPadTextPane)target).getKawaPad().schemeSecretary.getExecutive();
						
						synchronized ( scheme ) {
							try {
								String numStr = targetStr.substring( m.start(), m.end());
								String strResult = fractionAdd( numStr, direction );
								
								if ( strResult != null ) {
									foundSubstr = strResult.toString();
									foundBeginPos = m.start() + beginPos;
									foundEndPos   = m.end()   + beginPos;
								}
							} catch (Throwable e) {
								logError( "failed to increment the number", e );
							}
						}
						 
					}
				}
				if ( foundSubstr == null ) {
					for ( IncrementalSymbol s : incrementalSymbols ) {
						Pattern pattern;
						String substr;
						if ( 0 <= direction ) {
							pattern = s.fromPattern;
							substr = s.to;
						} else {
							pattern = s.toPattern;
							substr = s.from;
						}
						
						Matcher m = pattern.matcher( targetStr );
						if ( m.find() ) {
							int tempBeginPos = m.start() + beginPos;
							int tempEndPos   = m.end()   + beginPos;
							
							if ( tempBeginPos < foundBeginPos ) {
								foundBeginPos = tempBeginPos;
								foundEndPos   = tempEndPos;
								foundSubstr   = substr;
							}
						}
					}
				}
			}
			
			KawaPad kawaPad = ((KawaPadTextPane)target).getKawaPad();
			
			if ( foundSubstr != null ) {
				kawaPad.getUndoManager().startGroup();
				kawaPad.getUndoManager().setSuspended( true );
				try {
					caret.setDot( foundBeginPos );
					caret.moveDot( foundEndPos );
					target.replaceSelection( foundSubstr );
					caret.setDot( foundBeginPos + foundSubstr.length() );
					caret.moveDot( foundBeginPos );
				} finally {
					kawaPad.getUndoManager().setSuspended( false );
					kawaPad.getUndoManager().startGroup();
				}
			}
		}

		@Deprecated
		void replace2(JTextComponent target, int direction ) {
			String str = target.getText();
			Caret caret = target.getCaret();
			
			int pos = caret.getDot();
			if ( str.length() < pos )
				pos = str.length();
			
			int beginPos = -1;
			{
				for ( int i=pos; 0<=i; i-- ) {
					if ( Character.isWhitespace( str.charAt( i ) ) ) {
						beginPos = i;
						break;
					}
				}
				if ( beginPos == -1 )
					beginPos = 0;
			}
			int endPos = -1;
			{
				Matcher m = Pattern.compile("$", Pattern.MULTILINE ).matcher( str +"\n" );
				if ( m.find( pos ) ) {
					endPos = m.start();
				}
				
				if ( endPos == -1 )
					endPos = str.length();
			}
			
			if ( endPos <= beginPos )
				return;
			
			logInfo( "TextualIncrementAction:" + beginPos + ":" + endPos );
			String targetStr = str.substring( beginPos , endPos );

			String foundSubstr=null;
			int foundBeginPos=Integer.MAX_VALUE;
			int foundEndPos =Integer.MAX_VALUE;

			{
				if ( foundSubstr == null ) {
					Matcher m = NUMBER_PATTERN.matcher( targetStr );
					if ( m.find() ) {
						Scheme scheme = ((KawaPadTextPane)target).getKawaPad().schemeSecretary.getExecutive();
						
						synchronized ( scheme ) {
							try {
								String numStr = targetStr.substring( m.start(), m.end());
								String strResult = null;
								{
									String[] ss = numStr.split( "\\/" );
									{
										String[] sss = ss[0].split( "\\." );
										{
											int lastPos = sss.length-1;
											if ( direction < 0 ) {
												sss[lastPos] = String.valueOf( Integer.valueOf( sss[lastPos] ) - 1 ); 
											} else {
												sss[lastPos] = String.valueOf( Integer.valueOf( sss[lastPos] ) + 1 ); 
											}
										}
										ss[0] = String.join( ".", sss );
									}
									strResult = String.join( "/", ss );
								}
								
								if ( strResult != null ) {
									foundSubstr = strResult.toString();
									foundBeginPos = m.start() + beginPos;
									foundEndPos   = m.end()   + beginPos;
								}
							} catch (Throwable e) {
								logError( "failed to increment the number", e );
							}
						}
						 
					}
				}
				if ( foundSubstr == null ) {
					for ( IncrementalSymbol s : incrementalSymbols ) {
						Pattern pattern;
						String substr;
						if ( direction < 0 ) {
							pattern = s.fromPattern;
							substr = s.to;
						} else {
							pattern = s.toPattern;
							substr = s.from;
						}
						
						Matcher m = pattern.matcher( targetStr );
						if ( m.find() ) {
							int tempBeginPos = m.start() + beginPos;
							int tempEndPos   = m.end()   + beginPos;
							
							if ( tempBeginPos < foundBeginPos ) {
								foundBeginPos = tempBeginPos;
								foundEndPos   = tempEndPos;
								foundSubstr   = substr;
							}
						}
					}
				}
			}
			
			KawaPad kawaPad = ((KawaPadTextPane)target).getKawaPad();
			
			if ( foundSubstr != null ) {
				kawaPad.getUndoManager().startGroup();
				kawaPad.getUndoManager().setSuspended( true );
				try {
					caret.setDot( foundBeginPos );
					caret.moveDot( foundEndPos );
					target.replaceSelection( foundSubstr );
					caret.setDot( foundBeginPos + foundSubstr.length() );
					caret.moveDot( foundBeginPos );
				} finally {
					kawaPad.getUndoManager().setSuspended( false );
					kawaPad.getUndoManager().startGroup();
				}
			}
		}		
		class TextualIncrementAction extends TextAction {
			
			/** Create this object with the appropriate identifier. */
	        public TextualIncrementAction() {
	            super(TEXTUAL_INCREMENT);
	        }

	        /**
	         * The operation to perform when this action is triggered.
	         *
	         * @param e the action event
	         */
	        public void actionPerformed(ActionEvent e) {
	        	logInfo("TextualIncrementAction.actionPerformed()");
	            JTextComponent target = getTextComponent(e);
	            if (target != null) {
	            	replace( target, 1);
	            }
	        }

			{
				putValue( Action2.NAME, "Increment the Nearest Number" );
				putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_G , KeyEvent.CTRL_MASK ));
				putValue( Action.MNEMONIC_KEY , (int) 'i' );
			}
	    }
		{
			addIncrementalSymbol( "foo", "bar" );
			addIncrementalSymbol( "bar", "baz" );
			addIncrementalSymbol( "baz", "foo" );
		}
		class TextualDecrementAction extends TextAction {
			/** Create this object with the appropriate identifier. */
	        public TextualDecrementAction( ) {
	            super(TEXTUAL_DECREMENT);
	        }

	        public void actionPerformed(ActionEvent e) {
	        	logInfo("TextualDecrementAction.actionPerformed()");
	            JTextComponent target = getTextComponent(e);
	            if (target != null) {
	            	replace( target, -1);
	            }
	        }
			{
				putValue( Action2.NAME, "Decrement the Nearest Number" );
				putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_D , KeyEvent.CTRL_MASK ));
				putValue( Action.MNEMONIC_KEY , (int) 'd' );
			}
	    }
		
		final TextualIncrementAction TEXTUAL_INCREMENT_ACTION = new TextualIncrementAction();
		final TextualDecrementAction TEXTUAL_DECREMENT_ACTION = new TextualDecrementAction();
	}
	TextualIncrementalAddon textualIncrementalAddon = new TextualIncrementalAddon(); 
	

	
	
	////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// 
	//  CREATING_KEYMAP
	//
	////////////////////////////////////////////////////////////////////////////////////////////////////////////

	{
		
		
        /*
		 * (Sun, 07 Oct 2018 23:50:37 +0900) CREATING_KEYMAP
		 * 
		 * THIS IS VERY IMPORTANT: I SPEND THREE SLEEPLESS NIGHTS TO FIND THAT THIS
		 * CAUSES THE PROBLEM!
		 * 
		 * See the tag CREATING_KEYMAP .
		 */
		textPane.getKeymap().setDefaultAction( KEYMAP_DEFAULT );
		
//		purgeKeyFromActionMap( textPane.getActionMap(), DefaultEditorKit.insertBreakAction );
//		purgeKeyFromActionMap( textPane.getActionMap(), DefaultEditorKit.defaultKeyTypedAction );
//		purgeKeyFromActionMap( textPane.getActionMap(), DefaultEditorKit.insertContentAction );
//		textPane.getActionMap().put(DefaultEditorKit.defaultKeyTypedAction, newKeyTypedAction );
//		for ( Object o : textPane.getActionMap().getParent().getParent(). allKeys() ) {
//			logInfo(o );
//		}
//		textPane.getActionMap().put("UNDO", UNDO_ACTION );
//		textPane.getActionMap().put("REDO", REDO_ACTION );

//		undoManager.addEdit(anEdit)
		textPane.getDocument().addUndoableEditListener( getUndoManager() );
	}
	
	boolean fileModified = false;
	File filePath = null;
	static final FileFilter SCHEME_FILE_FILTER = new FileFilter() {
		@Override
		public String getDescription() {
			return "Scheme File (*.scm)";
		}
		
		@Override
		public boolean accept(File f) {
			return f.getName().endsWith(".scm");
		}
	};
	public void openNewProc() {
		fileModified = false;
		filePath = null;
		getUndoManager().discardAllEdits();
		textPane.setText("");
//		JOptionPane.showMessageDialog( this, "OPEN NEW" );
	}
	public void openNew() throws IOException {
		if ( ! confirmSave( ConfirmType.CREATE_NEW_FILE) ) {
			return;
		}
		openNewProc();
	}

	private void openFileProc(File filePath) throws IOException {
		String s = new String( Files.readAllBytes( filePath.toPath() ),  Charset.defaultCharset() );
		setTextProc( filePath, s );
	}
	/**
	 *  
	 * @param filePath
	 *     null when it opens a newly created document.
	 * @param s
	 *     the text to show on the editor.
	 */
	private void setTextProc(File filePath, String s) {
//		this.undoManager.discardAllEdits();
		this.textPane.setText( s );
		this.filePath = filePath;
		this.fileModified = false;
		
		/*
		 * Discard edits after set text or CTRL-Z to clear all text 
		 * which is not supposed to be. (Tue, 09 Oct 2018 03:04:23 +0900)
		 */
		this.getUndoManager().discardAllEdits();
//		JOptionPane.showMessageDialog(this, "OPEN FILE PROC" + file );
	}
	public void openFile( File filePath ) throws IOException {
		if ( ! confirmSave( ConfirmType.OPEN_FILE ) ) {
			return;
		}
		openFileProc( filePath );
	}
	
	public void openFile() throws IOException {
		if ( ! confirmSave( ConfirmType.OPEN_FILE ) ) {
			return;
		}
		JFileChooser fc = new JFileChooser();
		fc.addChoosableFileFilter( SCHEME_FILE_FILTER );
		fc.setMultiSelectionEnabled(false);
		int i = fc.showOpenDialog(this);
		if ( i == JFileChooser.APPROVE_OPTION ) {
			openFileProc( fc.getSelectedFile() );
		}
	}
	static class ConfirmType { 
		static final ConfirmType OPEN_FILE = new ConfirmType( 
			"Do you save the changes before closing the current document?", 
			"Open a file" );
		static final ConfirmType CREATE_NEW_FILE = new ConfirmType( 
			"Do you save the changes before closing the current document?", 
			"Create a new file" );
		static final ConfirmType CLOSE_WINDOW = new ConfirmType( 
				"Do you save the changes before closing the current document?", 
				"Closing the current window" );
		final String caption;
		final String title;
		public ConfirmType(String caption, String title) {
			this.caption = caption;
			this.title = title;
		}
	}
	public boolean confirmSave( ConfirmType confirmType ) throws IOException {
		if ( fileModified ) {
			int i = JOptionPane.showConfirmDialog( this, 
					confirmType.caption,
					confirmType.title , JOptionPane.YES_NO_CANCEL_OPTION  );
			if ( i == JOptionPane.YES_OPTION ) {
				if ( filePath == null ) {
					return saveFileAs();
				} else {
					saveFile();
					return true;
				}
			} else if ( i == JOptionPane.NO_OPTION ) {
				return true; 
			} else {
				return false;
			}
		} else {
			return true;
		}
	}

	private void saveFileProc(File filePath) throws IOException {
		Files.write(filePath.toPath(), textPane.getText().getBytes( Charset.defaultCharset() ), StandardOpenOption.CREATE , StandardOpenOption.TRUNCATE_EXISTING );
		this.fileModified = false;
		this.filePath = filePath;
//		JOptionPane.showMessageDialog(this, "SAVE FILE!" + file );
	}

	public void saveFile() throws IOException {
		if ( filePath != null )
			saveFileProc( filePath );
		else
			saveFileAs();
	}

	public boolean saveFileAs() throws IOException {
		JFileChooser fc = new JFileChooser();
		fc.addChoosableFileFilter( SCHEME_FILE_FILTER );
		fc.setMultiSelectionEnabled(false);
		fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
		int i = fc.showSaveDialog(this);
		if ( i == JFileChooser.APPROVE_OPTION ) {
			saveFileProc( fc.getSelectedFile());
			return true;
		} else {
			return false;
		}
	}
	

	Action OPEN_FILE_NEW = new AbstractAction() {
		@Override
		public void actionPerformed(ActionEvent e) {
			try {
				openNew();
			} catch (IOException e1) {
				logError("", e1);
			}
		}
		{
			putValue( Action2.NAME, "Open New" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_N , KeyEvent.CTRL_MASK | KeyEvent.SHIFT_MASK ));
			putValue( Action.MNEMONIC_KEY , (int) 'n' );
		}        
	};

	Action OPEN_FILE = new AbstractAction() {
		@Override
		public void actionPerformed(ActionEvent e) {
			try {
				openFile();
			} catch (IOException e1) {
				logError("", e1);
			}
		}
		{
			putValue( Action2.NAME, "Open" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_O , KeyEvent.CTRL_MASK ));
			putValue( Action.MNEMONIC_KEY , (int) 'o' );
		}
	};
	Action SAVE_FILE = new AbstractAction() {
		@Override
		public void actionPerformed(ActionEvent e) {
			try {
				saveFile();
			} catch (IOException e1) {
				logError("", e1);
			}
		}
		{
			putValue( Action2.NAME, "Save" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_S , KeyEvent.CTRL_MASK ));
			putValue( Action.MNEMONIC_KEY , (int) 'o' );
		}
	};
	Action SAVE_FILE_AS = new AbstractAction() {
		@Override
		public void actionPerformed(ActionEvent e) {
			try {
				saveFileAs();
			} catch (IOException e1) {
				logError("", e1);
			}
		}
		{
			putValue( Action2.NAME, "Save as" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_S , KeyEvent.CTRL_MASK | KeyEvent.SHIFT_MASK ));
			putValue( Action.MNEMONIC_KEY , (int) 'o' );
		}
	};
	
	
	
	{
		if ( false ) {
			System.err.println( "-------------------------" );
			for ( Object o : textPane.getActionMap().getParent().allKeys() ) {
				if ( o != null )
					logInfo( o.toString() );
			}
			System.err.println( "==========================" );
			
			for ( Object o : textPane.getActionMap().allKeys() ) {
				if ( o != null )
					logInfo( o.toString() );
			}
		}
	}
	
	{
		menuBar = new JMenuBar();
		setJMenuBar(menuBar);

		JMenu fileMenuItem = new JMenu( "File" );
		fileMenuItem.setMnemonic('f');
		menuBar.add( fileMenuItem );
		
		JMenu editMenuItem = new JMenu( "Edit" );
		editMenuItem.setMnemonic('e');
		menuBar.add( editMenuItem );

		JMenu viewMenuItem = new JMenu( "View" );
		viewMenuItem.setMnemonic('v');
//		menuBar.add( viewMenuItem );

		JMenu schemeMenuItem = new JMenu( "Scheme" );
		schemeMenuItem.setMnemonic('r');
		menuBar.add( schemeMenuItem );

		///
		
		fileMenuItem.add( new JMenuItem( NEW_SCRATCHPAD_ACTION ) );
		fileMenuItem.add( new JMenuItem( OPEN_FILE_NEW ) );
		fileMenuItem.add( new JMenuItem( OPEN_FILE ) );
		fileMenuItem.add( new JMenuItem( SAVE_FILE ) );
		fileMenuItem.add( new JMenuItem( SAVE_FILE_AS ) ); 

		schemeMenuItem.add( new JMenuItem( EVALUATE_REPLACE_ACTION ) );
		schemeMenuItem.add( new JMenuItem( EVALUATE_ACTION ) );
		schemeMenuItem.add( new JMenuItem( RUN_ACTION ) );
		schemeMenuItem.add( new JMenuItem( INTERRUPT_ACTION ) );
		schemeMenuItem.addSeparator();
		schemeMenuItem.add( new JMenuItem( RESET_ACTION ) );
		
		editMenuItem.add( new JMenuItem( UNDO_ACTION ) );
		editMenuItem.add( new JMenuItem( REDO_ACTION ) );
		editMenuItem.add( new JMenuItem( DEBUG_ACTION ) );
		editMenuItem.add( new JMenuItem( PASTE_ACTION ) );

		editMenuItem.addSeparator();

		editMenuItem.add( new JMenuItem( textPane.getActionMap().get( DefaultEditorKit.deletePrevCharAction )  ));
		editMenuItem.add( new JMenuItem( INCREASE_INDENT_ACTION ) );
		editMenuItem.add( new JMenuItem( DECREASE_INDENT_ACTION ) );
		editMenuItem.add( new JMenuItem( PRETTIFY_ACTION ) );


		// TEXTUAL_INCREMENT
		{
			textualIncrementalAddon.initGui( fileMenuItem, editMenuItem, viewMenuItem, schemeMenuItem );
		}
		
//		editMenuItem.addSeparator();
//
//		editMenuItem.add( new JMenuItem( SIMPLE_PARENTHESIS_JUMP_LEFT_ACTION ) );
//		editMenuItem.add( new JMenuItem( SIMPLE_PARENTHESIS_JUMP_RIGHT_ACTION ) );
//		editMenuItem.add( new JMenuItem( SIMPLE_PARENTHESIS_SELECT_JUMP_LEFT_ACTION ) );
//		editMenuItem.add( new JMenuItem( SIMPLE_PARENTHESIS_SELECT_JUMP_RIGHT_ACTION ) );
//		editMenuItem.add( new JMenuItem( PARENTHESIS_JUMP_LEFT_ACTION ) );
//		editMenuItem.add( new JMenuItem( PARENTHESIS_JUMP_RIGHT_ACTION ) );
//		editMenuItem.add( new JMenuItem( PARENTHESIS_SELECT_JUMP_LEFT_ACTION ) );
//		editMenuItem.add( new JMenuItem( PARENTHESIS_SELECT_JUMP_RIGHT_ACTION ) );
//		editMenuItem.add( new JMenuItem( PARENTHESIS_SELECT_ACTION ) );
//		editMenuItem.add( new JMenuItem( PARENTHESIS_DESELECT_ACTION ) );

		Action2.processMenuBar( menuBar );
	}

	
	{
		setSize( new Dimension( 500, 500 ) );
		setDefaultCloseOperation( JFrame.DO_NOTHING_ON_CLOSE );
		
		this.addWindowListener( new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent we) {
				boolean result;
				try {
					result = confirmSave( ConfirmType.CLOSE_WINDOW );
				} catch (IOException e) {
					logError( "" , e );
					result = false;
				}
				if ( result ) {
					onCloseWindow();
					KawaPad.this.setVisible(false);
					KawaPad.this.dispose();
				} else {
					// Stay open
				}
			}
		});
		
		setVisible(true);
	}
	
	private static final class WordJumpAction extends AbstractAction {
		private int direction;
		public WordJumpAction(int direction) {
			this.direction = direction;
		}
		public void actionPerformed(ActionEvent ae) {
		    JTextComponent ta = (JTextComponent)ae.getSource();
		    int p0 = ta.getCaretPosition();
		    String text = ta.getText();
		    
		    int p1 = lookup(p0, text, direction);
		    
		    // Then, jump to there.
		    if ( p1 < 0 ) {
		    	ta.setCaretPosition(0);
		    } else {
		    	ta.setCaretPosition(p1);
		    }
		}
		private static boolean isExclusiveBoundary( char ch ) {
			return Character.isWhitespace(ch); 
		}
		private static boolean isInclusiveBoundary( char ch ) {
			return ch == '"' || ch == ')' || ch == '('  ; 
		}
		static int lookup(int p0, String text, int direction ) {
			int length = text.length();
		    if ( length <= p0 ) {
		    	p0 = length-1;
		    }
		    int p1;

		    if ( isInclusiveBoundary( text.charAt( p0 ))) {
		    	p1 = p0 + direction;
		    } else if ( isExclusiveBoundary( text.charAt( p0 ) ) ) {
	    		// Look up a non-space character.
	    		// In case the character on the current position is a space-character,
	    		// this loop immediately breaks and the next loop will start on
	    		// the current position.
	    		p1 = -1;
	    		for ( int i=p0; 0<=i&&i<length; i+=direction ) {
	    			char ch = text.charAt( i );
	    			if ( ! isExclusiveBoundary(ch) ) {
	    				p1 = i;
	    				break;
	    			}
	    		}
		    } else {
		    	p0 += direction * 1;
		    	if ( 
		    			isExclusiveBoundary( text.charAt( p0 ) ) ||
		    			isInclusiveBoundary( text.charAt( p0 ) ) )
		    	{
		    		p1 = p0;
		    	} else {
		    		// Look up the nearest space character.
		    		p1 = -1;
		    		for ( int i=p0; 0<=i&&0<length; i+=direction ) {
		    			char ch = text.charAt( i );
		    			if ( isExclusiveBoundary(ch) ) {
		    				p1 = i - direction;
		    				break;
		    			}
		    			if ( isInclusiveBoundary(ch) ) {
		    				p1 = i  - direction;
		    				break;
		    			}
		    		}
		    	}
		    }
			return p1;
		}
	}
	
	public void addKeyStroke( JComponent component, Action action ) {
		Object name = action.getValue( Action.NAME );
		component.getInputMap().put( (KeyStroke) action.getValue( Action.ACCELERATOR_KEY ), name );
		component.getActionMap().put( name, action );
	}
	{
		ActionMap map = this.textPane.getActionMap();
		
		addKeyStroke( this.textPane, SIMPLE_PARENTHESIS_JUMP_LEFT_ACTION );
		addKeyStroke( this.textPane, SIMPLE_PARENTHESIS_JUMP_RIGHT_ACTION );
		addKeyStroke( this.textPane, SIMPLE_PARENTHESIS_SELECT_JUMP_LEFT_ACTION );
		addKeyStroke( this.textPane, SIMPLE_PARENTHESIS_SELECT_JUMP_RIGHT_ACTION );
		addKeyStroke( this.textPane, PARENTHESIS_JUMP_LEFT_ACTION );
		addKeyStroke( this.textPane, PARENTHESIS_JUMP_RIGHT_ACTION );
		addKeyStroke( this.textPane, PARENTHESIS_SELECT_JUMP_LEFT_ACTION );
		addKeyStroke( this.textPane, PARENTHESIS_SELECT_JUMP_RIGHT_ACTION );
		addKeyStroke( this.textPane, PARENTHESIS_SELECT_ACTION );
		addKeyStroke( this.textPane, PARENTHESIS_SELECT_2_ACTION );
		addKeyStroke( this.textPane, PARENTHESIS_DESELECT_ACTION );
		addKeyStroke( this.textPane, PARENTHESIS_DESELECT_2_ACTION );

		map.put( DefaultEditorKit.nextWordAction, new WordJumpAction(1));
		map.put( DefaultEditorKit.previousWordAction, new WordJumpAction(-1));
	}
	
	protected void onCloseWindow() {
	}

	static {
		ActionMap actionMap = new JTextPane().getActionMap();
		
		// Dump
		if ( false ) 
			for ( Object o : actionMap.allKeys() ) {
				logInfo( o == null ? null : o.toString() );
			}
		
		actionMap.get( DefaultEditorKit.deletePrevCharAction ).putValue(Action2.NAME, "Backspace");
//		actionMap.get( DefaultEditorKit.copyAction ).putValue(Action2.NAME, "Backspace");
	}

	
	
	////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// 
	// factory
	//
	////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	public static KawaPad createStaticInstance() {
		SchemeSecretary schemeSecretary = new SchemeSecretary();
		registerGlobalSchemeInitializer( schemeSecretary );
		schemeSecretary.newScheme();
		KawaPad kawaPad = new KawaPad( schemeSecretary, "Scheme Scratch Pad" );
		kawaPad.init();
		return kawaPad;
	}
	public static void main(String[] args) throws IOException {
		PulsarLogger.init();
		if ( 0 < args.length  ) {
			start( new File( args[0] ) );
		} else {
			start();		
		}
	}
	public static void start(File f) throws IOException {
		KawaPad kawaPad = createStaticInstance();
		if ( f != null )
			kawaPad.openFile( f );
	}
	public static void start() throws IOException {
		start( null );
	}

	public static final String[] DEFAULT_LISP_WORDS = {
			// ??
			"let", "letrec", "let*", "letrec*","set!",
			"do","if", "cond", "else", "#t", "#f", "begin", "apply","define", 
			"not", "and", "or","lambda", "eval",
			
			// https://www.gnu.org/software/kawa/Numerical-types.html
			"number?",
			"quantity?",
			"complex?",
			"real?",
			"rational?",
			"integer?",
			"longv",
			"int?",
			"short?",
			"byte?",
			"ulong?",
			"uint?",
			"ushort?",
			"ubyte?",
			"double?",
			"float?",
			
			// https://www.gnu.org/software/kawa/Standard-Types.html
			"Object?",
			"symbol?",
			"keyword?",
			"list?",
			"pair?",
			"string?",
			"String?",
			"character?",
			"vector?",
			"procedure?",
			"input-port?",
			"output-port?",
			"parameter?",
			"dynamic?",
			// https://srfi.schemers.org/srfi-1/srfi-1.html
			"cons", "list",
			"xcons", "cons*", "make-list", "list-tabulate", 
			"list-copy", "circular-list", "iota",

			"pair?", "null?",
			"proper-list?", "circular-list?", "dotted-list?", 
			"not-pair?", "null-list?",
			"list=",

			"car", "cdr", "cddadr", "cddddr", "list-ref",
			"first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth",
			"car+cdr",
			"take",       "drop",
			"take-right", "drop-right",
			"take!",      "drop-right!", 
			"split-at",   "split-at!", 
			"last", "last-pair",

			"length", "length+",
			"append",  "concatenate",  "reverse",
			"append!", "concatenate!", "reverse!",
			"append-reverse", "append-reverse!",
			"zip", "unzip1", "unzip2", "unzip3", "unzip4", "unzip5",
			"count",

			"map", "for-each",
			"fold",       "unfold",       "pair-fold",       "reduce", 
			"fold-right", "unfold-right", "pair-fold-right", "reduce-right", 
			"append-map", "append-map!",
			"map!", "pair-for-each", "filter-map", "map-in-order",

			"filter",  "partition",  "remove",
			"filter!", "partition!", "remove!", 

			"member", "memq", "memv",
			"find", "find-tail", 
			"any", "every",
			"list-index",
			"take-while", "drop-while", "take-while!",
			"span", "break", "span!", "break!",

			"delete",  "delete-duplicates", 
			"delete!", "delete-duplicates!",

			"assoc", "assq", "assv",
			"alist-cons", "alist-copy",
			"alist-delete", "alist-delete!",

			"lset<=", "lset=", "lset-adjoin",
			"lset-union",			"lset-union!",
			"lset-intersection",		"lset-intersection!",
			"lset-difference",		        "lset-difference!",
			"lset-xor",			"lset-xor!",
			"lset-diff+intersection",	        "lset-diff+intersection!",

			"set-car!", "set-cdr!",

			// kawa
			"import",
			"require",
			"load",
			"load-relative",
			
			};

	public static final String[] DEFAULT_LISP_WORDS_OLD = {
			"defun",
			"define",
			"defmacro",
			"set!",
			"lambda",
			"lambda*",
			"if",
			"case",
			"let",
			"flet",
			"let*",
			"letrec",
			"do",
			"do*",
			"define-syntax",
			"let-syntax",
			"letrec-syntax",
			"destructuring-bind",
			"defpackage",
			"defparameter",
			"defstruct",
			"deftype",
			"defvar",
			"do-all-symbols",
			"do-external-symbols",
			"do-symbols",
			"dolist",
			"dotimes",
			"ecase",
			"etypecase",
			"eval-when",
			"labels",
			"macrolet",
			"multiple-value-bind",
			"multiple-value-call",
			"multiple-value-prog1",
			"multiple-value-setq",
			"prog1",
			"progv",
			"typecase",
			"unless",
			"unwind-protect",
			"when",
			"with-input-from-string",
			"with-open-file",
			"with-open-stream",
			"with-output-to-string",
			"with-package-iterator",
			"define-condition",
			"handler-bind",
			"handler-case",
			"restart-bind",
			"restart-case",
			"with-simple-restart",
			"store-value",
			"use-value",
			"muffle-warning",
			"abort",
			"continue",
			"with-slots",
			"with-slots*",
			"with-accessors",
			"with-accessors*",
			"defclass",
			"defmethod",
			"print-unreadable-object",};

}

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

package ats.kawapad;

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
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringReader;
import java.lang.invoke.MethodHandles;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
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
import javax.swing.Timer;
import javax.swing.UIManager;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultCaret;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.DefaultEditorKit.DefaultKeyTypedAction;
import javax.swing.text.JTextComponent;
import javax.swing.text.TextAction;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;

import ats.kawapad.SimpleSchemeParser.ParserState;
import ats.kawapad.lib.CompoundGroupedUndoManager;
import ats.kawapad.lib.GroupedUndoManager;
import ats.pulsar.lib.PulsarLogger;
import ats.pulsar.lib.SchemeUtils;
import ats.pulsar.lib.SimpleSchemeIndentChanger;
import ats.pulsar.lib.SimpleSchemePrettifier;
import ats.pulsar.lib.secretary.Invokable;
import ats.pulsar.lib.secretary.SecretaryMessage;
import ats.pulsar.lib.secretary.SecretaryMessage.NoReturnNoThrow;
import ats.pulsar.lib.secretary.scheme.SchemeSecretary;
import ats.pulsar.lib.swing.Action2;
import gnu.expr.Language;
import gnu.kawa.io.InPort;
import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.Procedure1;
import gnu.mapping.Procedure2;
import gnu.mapping.Procedure3;
import gnu.mapping.Symbol;
import kawa.standard.Scheme;

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

	public static final Map<Object,Object> memoMap = new HashMap<Object,Object>();

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
		schemeSecretary.registerSchemeInitializer( kawaPad, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
			@Override
			public void execute0( Scheme scheme, Object[] args ) {
				logInfo( "eventinvokeEventHandler of KawaPad#registerSchemeInitializer " );
//				eventHandlers.invokeEventHandler( kawaPad, EventHandlers.INIT );
				eventHandlers.invokeEventHandler( kawaPad, EventHandlers.CREATE );
			}
		});
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
		schemeSecretary.registerSchemeInitializer( null, staticInitializer01 );
		schemeSecretary.registerSchemeFinalizer(null, new NoReturnNoThrow<Scheme>() {
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

	@Override
	public void dispose() {
		unregisterLocalSchemeInitializers( this.schemeSecretary, this );
		super.dispose();
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
					LOGGER.log( Level.INFO, "run");
					// ==== WORKAROUND SEE acvpoeov === (Tue, 23 Jul 2019 11:37:32 +0900) //
//					schemeSecretary.initializeSchemeForCurrentThread();
					// ==== WORKAROUND SEE acvpoeov === (Tue, 23 Jul 2019 11:37:32 +0900) //
					r.run();
				} finally {
					logInfo("end");
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
						undoManager.startGroup();
						undoManager.setSuspended(true);
						int selectionEnd = textPane.getSelectionEnd();
						textPane.getDocument().insertString( selectionEnd, result, null);
						textPane.setSelectionEnd( selectionEnd + result.length() );
						textPane.setSelectionStart(selectionEnd + 1 );
					} finally {
						undoManager.setSuspended(false);
						undoManager.startGroup();
					}
				} else {
					try {
						undoManager.startGroup();
						undoManager.setSuspended(true);
						int dot = textPane.getCaret().getDot();
						textPane.getDocument().insertString( dot, result, null);
						textPane.getCaret().moveDot(dot);
					} finally {
						undoManager.setSuspended(false);
						undoManager.startGroup();
					}
				}
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
//			System.err.println("PulsarScratchPadTextPaneController.caretUpdate()");
			if ( ! undoManager.isSuspended() ) {
				updateHighlightLater();
				eventHandlers.invokeEventHandler( KawaPad.this, EventHandlers.CARET,  KawaPad.this);
				eventHandlers.invokeEventHandler( KawaPad.this, EventHandlers.CHANGE,  KawaPad.this);
			}
		}
		//DocumentListener
		public void insertUpdate(DocumentEvent e) {
			fileModified = true;
//			System.err.println("PulsarScratchPadTextPaneController.insertUpdate()");
			if ( ! undoManager.isSuspended() ) {
				updateHighlightLater();
				eventHandlers.invokeEventHandler( KawaPad.this, EventHandlers.INSERT,  KawaPad.this);
				eventHandlers.invokeEventHandler( KawaPad.this, EventHandlers.CHANGE,  KawaPad.this);
			}
		}
		public void removeUpdate(DocumentEvent e) {
			fileModified = true;
//			System.err.println("PulsarScratchPadTextPaneController.removeUpdate()");
			if ( ! undoManager.isSuspended() ) {
				updateHighlightLater();
				eventHandlers.invokeEventHandler( KawaPad.this, EventHandlers.REMOVE,  KawaPad.this);
				eventHandlers.invokeEventHandler( KawaPad.this, EventHandlers.CHANGE,  KawaPad.this);
			}
		}
		public void changedUpdate(DocumentEvent e) {
//			fileModified = true;
//			System.err.println("PulsarScratchPadTextPaneController.changedUpdate() : ignored");
			if ( ! undoManager.isSuspended() ) {
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

	
	
	
//	public final AbstractAction EVALUATE_ACTION = new ExecuteAction();
//	private final class ExecuteAction extends AbstractAction {
//		@Override
//		public void actionPerformed(ActionEvent e) {
//			//	JOptionPane.showMessageDialog( JPulsarScratchPad.this, "", "AAAA" , JOptionPane.INFORMATION_MESSAGE  );
//			threadManager.startScratchPadThread( new Runnable() {
//				@Override
//				public void run() {
//					boolean isThereSelection=true;
//					Object resultObject=null;
//					String result = null;
//					try {
//						String text = textPane.getSelectedText();
//						if ( text == null ) {
//							text = textPane.getText();
//							isThereSelection = false;
//						}
//						resultObject = executeScheme(text);
//						textPane.getActionMap();
//
//						result = prettyPrint( resultObject );
//
//					} catch (Throwable e1) {
//						ByteArrayOutputStream out = new ByteArrayOutputStream();
//						PrintStream pout = new PrintStream( out );
//						e1.printStackTrace( pout );
//						pout.flush();
//						result = new String( out.toByteArray() );
//					}
//					result = "\n#|\n" + result + "\n|#\n"; 
//					logInfo( result );
//
//					SwingUtilities.invokeLater( new InsertTextToTextPane(result, isThereSelection ) );
//				}
//
//			});
//		}
//		{
//			putValue( Action2.NAME, "Execute" );
//			putValue( Action.MNEMONIC_KEY, (int)'e' );
//			putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_E, ActionEvent.CTRL_MASK) );
//		}
//	}

	final class ExecuteRunnable implements Runnable {
		boolean insertText;
		public ExecuteRunnable(boolean insertText) {
			super();
			this.insertText = insertText;
		}

		@Override
		public void run() {
			Object resultObject=null;
			String result = null;
			boolean errorOccured = false;
			try {
				String text;
				{
					String s = textPane.getSelectedText();
					if ( s == null ) {
						s = textPane.getText();
					}
					text = s;
				}
				resultObject =
						schemeSecretary.executeSecretarially( new SecretaryMessage<Scheme, Object, Throwable>() {
							@Override
							public Object execute(Scheme scheme, Object[] args) throws Throwable {
								return executeScheme( scheme, KawaPad.this,  text );
							} 
						}, Invokable.NOARG );

				textPane.getActionMap();
//				System.out.println("==========================");
//				System.out.println( resultObject.getClass() );
//				System.out.println("==========================");
				result = SchemeUtils.anyToString( SimpleSchemePrettifier.prettyPrint( resultObject ) );

			} catch (Throwable e1) {
				errorOccured = true;
				ByteArrayOutputStream out = new ByteArrayOutputStream();
				PrintStream pout = new PrintStream( out );
				e1.printStackTrace( pout );
				pout.flush();
				result = new String( out.toByteArray() );
			}
			result = "\n#|\n" + result + "\n|#\n"; 
			logInfo( result );

			if ( insertText || errorOccured )
				SwingUtilities.invokeLater( new InsertTextToTextPane(result) );
		}
	}

	public final AbstractAction RESET_ACTION = new ResetAction();
	private final class ResetAction extends AbstractAction {
		@Override
		public void actionPerformed(ActionEvent e) {
			schemeSecretary.newScheme();
		}
		{
			putValue( Action2.NAME, "Reset" );
			putValue( Action.MNEMONIC_KEY, (int)'s' );
//			putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_E, ActionEvent.CTRL_MASK) );
		}
	}

	public final AbstractAction EVALUATE_ACTION = new ExecuteAction();
	private final class ExecuteAction extends AbstractAction {
		@Override
		public void actionPerformed(ActionEvent e) {
			//	JOptionPane.showMessageDialog( JPulsarScratchPad.this, "", "AAAA" , JOptionPane.INFORMATION_MESSAGE  );
			threadManager.startScratchPadThread( new ExecuteRunnable( true ) );
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
			threadManager.startScratchPadThread( new ExecuteRunnable( false ) );
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
				undoManager.startGroup();
				undoManager.setSuspended(true);
				formatProc( textPane, new TextFilter() {
					@Override
					String process(String text) {
						return SimpleSchemeIndentChanger.changeIndentRelativeMultiline( text, difference );
					}
				});
			} finally {
				undoManager.setSuspended(false);
				undoManager.startGroup();

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
	public static final String prettify( Scheme scheme, String text ) {
		return prettify( getLispWords0( scheme ), text );
	}
	public final String prettify( String text ) {
		return schemeSecretary.executeSecretarially( new SecretaryMessage.NoThrow<Scheme, String>() {
			@Override
			public String execute0( Scheme scheme, Object[] args ) {
				return prettify( scheme, text );
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

	static Object executeScheme( Scheme scheme, KawaPad kawaPad, String text) throws Throwable {
		synchronized ( scheme ) {
			StringReader reader = new StringReader(text);
			try {
			
				SchemeUtils.putVar( scheme , "scheme", scheme  );
				SchemeUtils.putVar( scheme , "frame", kawaPad );
				
				return scheme .eval( new InPort(reader) ); 
//				return scheme.eval( 
//						"(let (( frame "+ frameName +"))\n" +						
//								text +
//								"\n)"
//						);
			} finally {
				SchemeUtils.putVar( scheme , "scheme", false );
				SchemeUtils.putVar( scheme , "frame", false );
				reader.close();
			}
		}
	}

	////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// 
	// Defining an interface the for scheme interpreter. 
	//
	////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/*
	 *  
	 * 
	 */

	private static List<String> FALLBACK_LISP_WORDS = Arrays.asList("let","lambda" );

	public Collection<String> getLispWords() {
		return schemeSecretary.executeSecretarially( new SecretaryMessage.NoThrow<Scheme, Collection<String>>() {
			@Override
			public Collection<String> execute0(Scheme scheme, Object[] args) {
				return getLispWords0( scheme );
			}
		});
	}

	public static Collection<String> getLispWords0( Scheme scheme ) {
		Collection<String> lispWords = FALLBACK_LISP_WORDS;
		try {
			Object object = scheme.getEnvironment().get( Symbol.valueOf("lisp-words") );
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
	public String getTitleString() {
		return "HELLO";
		
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
			map.put( Symbol.valueOf(CREATE),      new HashMap<>() );
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
//					logInfo( "eventHandlers.invokeEventHandler(inner)" );
					try {
						SchemeUtils.putVar( scheme, "scheme", scheme );
						SchemeUtils.putVar( scheme, "frame",  kawaPad );

						for( Entry<Symbol,SchemeProcedure> e :  getEventType(eventTypeID).entrySet() ) {
							try {
								e.getValue().invoke( args );
							} catch ( Throwable t ) {
								logError("invoking event handlers : ", t);
							}
						}

					} finally {
						SchemeUtils.putVar( scheme, "scheme", false );
						SchemeUtils.putVar( scheme, "frame", false );
					}
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
		SchemeUtils.defineVar( scheme, frameName, this );
	}

	public static Scheme staticInitScheme( Scheme scheme ) {
		logInfo( "KawaPad#staticInitScheme" );

		if ( ! SchemeUtils.isDefined(scheme, FLAG_DONE_INIT_PULSAR_SCRATCHPAD ) ) {
			SchemeUtils.defineVar(scheme, FLAG_DONE_INIT_PULSAR_SCRATCHPAD, true );  

			SchemeUtils.defineVar(scheme, "frame", false );
			SchemeUtils.defineVar(scheme, "scheme", false );

			SchemeUtils.defineVar(scheme, "lisp-words",
					Pair.makeList( (List)SchemeUtils.<String,IString>convertList( 
							Arrays.asList( SimpleSchemePrettifier.LISP_WORDS ),
							(o)->{
								return SchemeUtils.toSchemeString( o );
							}) 
							)
					);
			SchemeUtils.defineVar(scheme, "default-lisp-words",
					Pair.makeList( (List)SchemeUtils.<String,IString>convertList( 
							Arrays.asList( SimpleSchemePrettifier.LISP_WORDS ),
							(o)->{
								return SchemeUtils.toSchemeString( o );
							}) 
							)
					);
			
			SchemeUtils.defineVar(scheme, "register-event-handler", new Procedure3() {
				@Override
				public Object apply3(Object arg1, Object arg2, Object arg3) throws Throwable {
					eventHandlers.register( (Symbol)arg1, (Symbol)arg2, (Procedure) arg3 );
					return EmptyList.emptyList;
				}
			});
			SchemeUtils.defineVar(scheme, "unregister-event-handler", new Procedure2() {
				@Override
				public Object apply2(Object arg1, Object arg2 ) throws Throwable {
					eventHandlers.unregister((Symbol)arg1,(Symbol)arg2 );
					return EmptyList.emptyList;
				}
			});
			SchemeUtils.defineVar(scheme, "pretty-print", new Procedure1() {
				@Override
				public Object apply1(Object arg1 ) throws Throwable {
					return prettify( scheme, SchemeUtils.anyToString(SimpleSchemePrettifier.prettyPrint(arg1)));
				}
			});
			SchemeUtils.defineVar(scheme, "prettify", new Procedure1() {
				@Override
				public Object apply1(Object arg1 ) throws Throwable {
					return prettify( scheme, SchemeUtils.anyToString(arg1));
				}
			});
			
			try {
				logInfo( "scheme.eval( InPort.openFile( getInitFile() ) );" );
				scheme.eval( InPort.openFile( getInitFile() ) );
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
		textPane = new JTextPane() {
			// Special thanks go to tips4java
			// https://tips4java.wordpress.com/2009/01/25/no-wrap-text-pane/
			public boolean getScrollableTracksViewportWidth() {
//				return getUI().getPreferredSize(this).width 
//						<= getParent().getSize().width;
				return getUI().getPreferredSize(this).width 
						< getParent().getSize().width;
			}
		};
		scrollPane = new JScrollPane( textPane );
		textPaneController = new PulsarScratchPadListener();
		scratchPadRoot = new JPanel( new BorderLayout() );
		getContentPane().add(scratchPadRoot );
		
		scratchPadRoot.add( scrollPane, BorderLayout.CENTER );
        textPane.setFont(new Font("monospaced", Font.PLAIN, 12));
        
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
		                	undoManager.startGroup();
		                	undoManager.setSuspended(true);

		                	String text = target.getText();
		                	int pos = target.getCaretPosition();
		                	String indentString = calculateIndentSize(text, pos, getLispWords());
		                	target.replaceSelection( "\n" + indentString );
		                } finally {
		                	undoManager.setSuspended(false);
		                	undoManager.startGroup();
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
	GroupedUndoManager undoManager;
	{
		// https://stackoverflow.com/questions/2547404/using-undo-and-redo-for-jtextarea
//		undoManager = new InsignificantUndoManager();
//		undoManager = new LazyGroupedUndoManager();
//		undoManager = new SimpleCompoundUndoManager();
		undoManager = new CompoundGroupedUndoManager();
//		undoManager = new OriginalCompoundUndoManager( textPane );
	}

	private abstract static class UndoRedoAction extends AbstractAction {
		protected final GroupedUndoManager undoManager;
		protected UndoRedoAction( String name,  GroupedUndoManager undoManager ) {
			super(name);
			this.undoManager = undoManager;
		}
	}

	public final Action UNDO_ACTION = new UndoAction( "Undo", undoManager );
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

	public final Action REDO_ACTION = new RedoAction( "Redo", undoManager );
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
			undoManager.dump();
		}
		{
			putValue( Action2.NAME, "Debug" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_D , KeyEvent.CTRL_MASK ));
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
            		undoManager.startGroup();
            		undoManager.setSuspended(true);
            		target.paste();
            	} finally {
            		undoManager.setSuspended(false);
            		undoManager.startGroup();
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
                		undoManager.startGroup();
                		break;
                		
                	case "(" :
                	case ")" :
                		int pos = textPane.getCaretPosition() -1;
                		logInfo( "caret : " + pos );
                		SwingUtilities.invokeLater(new Runnable() {
							@Override
							public void run() {
								updateHighlight();
								KawaPadHighlighter.highlightMatchingParenthesis( textPane, pos  ); 

								Timer t = new Timer(300 , new ActionListener() {
									@Override
									public void actionPerformed(ActionEvent e) {
										updateHighlight(); 
									}
								});
								t.setRepeats(false);
								t.start();
							}
						});
                		break;
                		
                	default :
                		break;
                }
                
				eventHandlers.invokeEventHandler( KawaPad.this, EventHandlers.TYPED, target, SchemeUtils.toSchemeString( content ) );
            }
		}
	};
	
	public static final int lookupCorrespondingParenthesis( String text, int position ) {
		ParserState parserState = 
				SimpleSchemeParenthesisChecker.lookupParenthesis( text, position );
		if ( parserState.isFound() ) {
			return parserState.getIterator().getIndex();
		} else {
			return -1;
		}
	
	}

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
		textPane.getDocument().addUndoableEditListener( undoManager );
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
		undoManager.discardAllEdits();
		textPane.setText("");
//		JOptionPane.showMessageDialog( this, "OPEN NEW" );
	}
	public void openNew() throws IOException {
		if ( ! confirmSave( ConfirmType.OPEN_FILE ) ) {
			return;
		}
		openNewProc();
	}
	private void openFileProc(File filePath) throws IOException {
		String s = new String( Files.readAllBytes( filePath.toPath() ),  Charset.defaultCharset() );
		
//		this.undoManager.discardAllEdits();
		this.textPane.setText( s );
		this.filePath = filePath;
		this.fileModified = false;
		
		/*
		 * Discard edits after set text or CTRL-Z to clear all text 
		 * which is not supposed to be. (Tue, 09 Oct 2018 03:04:23 +0900)
		 */
		this.undoManager.discardAllEdits();
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
				"Do you save the changes before openning?", 
				"Open a file" );
		static final ConfirmType CLOSE_WINDOW = new ConfirmType( 
				"Do you save the changes before closing?", 
				"Closing the current document" );
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

		JMenu schemeMenuItem = new JMenu( "Run" );
		schemeMenuItem.setMnemonic('r');
		menuBar.add( schemeMenuItem );

		///
		
		fileMenuItem.add( new JMenuItem( NEW_SCRATCHPAD_ACTION ) );
		fileMenuItem.add( new JMenuItem( OPEN_FILE_NEW ) );
		fileMenuItem.add( new JMenuItem( OPEN_FILE ) );
		fileMenuItem.add( new JMenuItem( SAVE_FILE ) );
		fileMenuItem.add( new JMenuItem( SAVE_FILE_AS ) );

		schemeMenuItem.add( new JMenuItem( RESET_ACTION ) );
		schemeMenuItem.addSeparator();
		schemeMenuItem.add( new JMenuItem( EVALUATE_ACTION ) );
		schemeMenuItem.add( new JMenuItem( RUN_ACTION ) );
		schemeMenuItem.add( new JMenuItem( INTERRUPT_ACTION ) );

		editMenuItem.add( new JMenuItem( UNDO_ACTION ) );
		editMenuItem.add( new JMenuItem( REDO_ACTION ) );
		editMenuItem.add( new JMenuItem( DEBUG_ACTION ) );
		editMenuItem.add( new JMenuItem( PASTE_ACTION ) );

		editMenuItem.add( new JMenuItem( textPane.getActionMap().get( DefaultEditorKit.deletePrevCharAction )  ));
		editMenuItem.add( new JMenuItem( INCREASE_INDENT_ACTION ) );
		editMenuItem.add( new JMenuItem( DECREASE_INDENT_ACTION ) );
		editMenuItem.add( new JMenuItem( PRETTIFY_ACTION ) );

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
					result = confirmSave( ConfirmType.OPEN_FILE );
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
	
	public static KawaPad createStaticInstance() {
		SchemeSecretary schemeSecretary = new SchemeSecretary();
		registerGlobalSchemeInitializer( schemeSecretary );
		schemeSecretary.newScheme();
		return new KawaPad( schemeSecretary, "Scheme Scratch Pad" );
	}
	public static void main(String[] args) throws IOException {
		PulsarLogger.init();
		KawaPad kawaPad = createStaticInstance();
		if ( 0 < args.length  ) {
			kawaPad.openFile( new File( args[0] ) );
		}
	}
}
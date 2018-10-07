package ats.pulsar.editor;

import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.MenuBar;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
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
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
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
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultCaret;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.DefaultEditorKit.DefaultKeyTypedAction;
import javax.swing.text.JTextComponent;
import javax.swing.text.TextAction;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;

import ats.pulsar.Pulsar;
import ats.pulsar.SchemeUtils;
import ats.pulsar.editor.lib.CompoundGroupedUndoManager;
import ats.pulsar.editor.lib.GroupedUndoManager;
import gnu.expr.Language;
import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.Pair;
import gnu.lists.PrintConsumer;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.Procedure2;
import gnu.mapping.Procedure3;
import gnu.mapping.Symbol;
import kawa.standard.Scheme;

public class PulsarScratchPad extends JFrame {
	private static final String FLAG_DONE_INIT_PULSAR_SCRATCHPAD = "flag-done-init-pulsar-scratchpad";
	private static final boolean DEBUG_UNDO_BUFFER = false;
	private static final Logger LOGGER = Logger.getLogger(Pulsar.class.getName());
	static void logError( String msg, Throwable e ) {
		LOGGER.log(Level.SEVERE, msg, e);
		//		System.err.println( msg );
	}
	static void logInfo( String msg ) {
		//        Logger.getLogger(Pulsar.class.getName()).log(Level.INFO, msg);
		System.err.println( msg );
	}

	private final String frameName = newFrameName();
	public String getFrameName() {
		return this.frameName;
	}
	
	private Scheme scheme;
	private static Environment environment = null;
	public PulsarScratchPad( Scheme scheme ) {
		super( "Pulsar Scheme Scratch Pad" );
		this.scheme = scheme;
		if ( environment == null )
			environment = scheme.getEnvironment();
		initScheme( scheme );
		initFrame();
	}
	
	public final AbstractAction NEW_SCRATCHPAD_ACTION = new AbstractAction() {
		@Override
		public void actionPerformed(ActionEvent e) {
			new PulsarScratchPad( scheme );
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
					System.out.println("run");
					r.run();
				} finally {
					System.out.println("end");
					removeScratchPadThread( this );
				}
			}
			@Override
			public void interrupt() {
				System.out.println("interrupted");
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
			System.out.println("interruptScratchPadThreads");
			synchronized ( scratchPadThreadList ) {
				for ( Thread t : scratchPadThreadList ) {
					System.out.println( "interrupt start" );
					t.interrupt();
					System.out.println( "interrpt end" );
				}
			}
		}
	}
	
	final ScratchPadThreadManager threadManager = new ScratchPadThreadManager();
	public ScratchPadThreadManager getThreadManager() {
		return threadManager;
	}

	///////////////////////////////////////////////////////////////////////////////////

	private final class InsertTextToTextPane implements Runnable {
		private final String result;
		private boolean isThereSelection;
		private InsertTextToTextPane(String result, boolean isThereSelection) {
			this.result = result;
			this.isThereSelection = isThereSelection;
		}
		
		@Override
		public void run() {
			try {
				if ( isThereSelection ) {
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
						textPane.getDocument().insertString( textPane.getText().length(), result, null);
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
				eventHandlers.invokeEventHandler( PulsarScratchPad.this, scheme, EventHandlers.CARET,  PulsarScratchPad.this);
				eventHandlers.invokeEventHandler( PulsarScratchPad.this, scheme, EventHandlers.CHANGE,  PulsarScratchPad.this);
			}
		}
		//DocumentListener
		public void insertUpdate(DocumentEvent e) {
			fileModified = true;
//			System.err.println("PulsarScratchPadTextPaneController.insertUpdate()");
			if ( ! undoManager.isSuspended() ) {
				updateHighlightLater();
				eventHandlers.invokeEventHandler( PulsarScratchPad.this, scheme, EventHandlers.INSERT,  PulsarScratchPad.this);
				eventHandlers.invokeEventHandler( PulsarScratchPad.this, scheme, EventHandlers.CHANGE,  PulsarScratchPad.this);
			}
		}
		public void removeUpdate(DocumentEvent e) {
			fileModified = true;
//			System.err.println("PulsarScratchPadTextPaneController.removeUpdate()");
			if ( ! undoManager.isSuspended() ) {
				updateHighlightLater();
				eventHandlers.invokeEventHandler( PulsarScratchPad.this, scheme, EventHandlers.REMOVE,  PulsarScratchPad.this);
				eventHandlers.invokeEventHandler( PulsarScratchPad.this, scheme, EventHandlers.CHANGE,  PulsarScratchPad.this);
			}
		}
		public void changedUpdate(DocumentEvent e) {
			fileModified = true;
//			System.err.println("PulsarScratchPadTextPaneController.changedUpdate() : ignored");
			if ( ! undoManager.isSuspended() ) {
				eventHandlers.invokeEventHandler( PulsarScratchPad.this, scheme, EventHandlers.ATTRIBUTE,  PulsarScratchPad.this);
				eventHandlers.invokeEventHandler( PulsarScratchPad.this, scheme, EventHandlers.CHANGE,  PulsarScratchPad.this);
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
	private String getLispWordPatternString() {
		return PulsarScratchPadHighlighter.lispWordToPatternString( getLispWords( scheme ) ); 
	}
	public void updateHighlight() {
		PulsarScratchPadHighlighter.resetStyles( textPane );
		PulsarScratchPadHighlighter.highlightSyntax( textPane, getLispWordPatternString() );
	}
	public void highlightMatchningParentheses() {
		PulsarScratchPadHighlighter.highlightMatchingParenthesis( textPane, textPane.getCaretPosition() );
	}

	
	
	
	public final AbstractAction EXECUTE_ACTION = new ExecuteAction();
	private final class ExecuteAction extends AbstractAction {
		@Override
		public void actionPerformed(ActionEvent e) {
			//	JOptionPane.showMessageDialog( JPulsarScratchPad.this, "", "AAAA" , JOptionPane.INFORMATION_MESSAGE  );
			threadManager.startScratchPadThread( new Runnable() {
				@Override
				public void run() {
					boolean isThereSelection=true;
					Object resultObject=null;
					String result = null;
					try {
						String text = textPane.getSelectedText();
						if ( text == null ) {
							text = textPane.getText();
							isThereSelection = false;
						}
						resultObject = executeScheme(text);
						textPane.getActionMap();

						ByteArrayOutputStream out = new ByteArrayOutputStream();
						PrintConsumer pconsumer = new PrintConsumer( out, false );
						SchemeUtils.toString( kawa.lib.kawa.pprint.pprint.apply2( resultObject, pconsumer ) );
						pconsumer.flush();
						result = new String( out.toByteArray() );

					} catch (Throwable e1) {
						ByteArrayOutputStream out = new ByteArrayOutputStream();
						PrintStream pout = new PrintStream( out );
						e1.printStackTrace( pout );
						pout.flush();
						result = new String( out.toByteArray() );
					}
					result = "\n#|\n" + result + "\n|#\n"; 
					logInfo( result );

					SwingUtilities.invokeLater( new InsertTextToTextPane(result, isThereSelection ) );
				}
			});
		}
		{
			putValue( Action2.NAME, "Execute" );
			putValue( Action.MNEMONIC_KEY, (int)'e' );
			putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_E, ActionEvent.CTRL_MASK) );
		}
	}

	public final AbstractAction INTERRUPT_ACTION = new InterruptAction();
	private final class InterruptAction extends AbstractAction {
		@Override
		public void actionPerformed(ActionEvent e) {
			threadManager.interruptScratchPadThreads();
		}
		{
			putValue( Action2.NAME, "Interrupt" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_Q, ActionEvent.CTRL_MASK) );
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
					return SimpleSchemePrettifier.prettify( getLispWords( scheme ), text );
				}

			});
		}
	}

	public final AbstractAction PRETTIFY_ACTION = new PrettifyAction() {
		{
			putValue( Action2.NAME, "Correct Indentation" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_I , KeyEvent.CTRL_MASK ));
			putValue( Action.MNEMONIC_KEY , (int) 'i' );
		}
	};

	Object executeScheme(String text) throws Throwable {
		synchronized ( scheme ) {
			try {
				Environment.setCurrent(environment);
				SchemeUtils.putVar(scheme, "scheme", scheme );
				SchemeUtils.putVar(scheme, "frame", this );
				return scheme.eval( text );
//				return scheme.eval( 
//						"(let (( frame "+ frameName +"))\n" +						
//								text +
//								"\n)"
//						);
			} finally {
				SchemeUtils.putVar(scheme, "scheme", false );
				SchemeUtils.putVar(scheme, "frame", false );
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
	
	public static Collection<String> getLispWords( Scheme scheme ) {
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
		private final Environment environment;
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
				Environment.setCurrent(environment);
				Language.setCurrentLanguage(this.language);
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
		private static final String INIT      = "init";
		private static final String CARET     = "caret";
		private static final String INSERT    = "insert";
		private static final String REMOVE    = "remove";
		private static final String ATTRIBUTE = "attribute";
		private static final String CHANGE    = "change";
		private static final String TYPED     = "typed";

		final Map<Symbol,Map<Symbol,SchemeProcedure>> map = new HashMap<>();
		{
			map.put( Symbol.valueOf(INIT),      new HashMap<>() );
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
		public void invokeEventHandler( PulsarScratchPad frame, Scheme scheme, String eventTypeID, Object ...args ) {
			synchronized ( scheme ) {
				try {
//					Environment.setCurrent( Environment.getGlobal()  );
//					System.out.println( "invokeEventHandler" + frame.frameName );
					
					SchemeUtils.putVar( scheme, "scheme", scheme );
					SchemeUtils.putVar( scheme, "frame",  frame );

					for( Entry<Symbol,SchemeProcedure> e :  getEventType(eventTypeID).entrySet() ) {
						try {
//							e.getValue().procedure.setProperty(Symbol.valueOf("scheme") , scheme );
//							e.getValue().procedure.setProperty(Symbol.valueOf("frame") , frame );
//							
//							SchemeUtils.putVar( e.getValue().environment, "scheme", scheme );
//							SchemeUtils.putVar( e.getValue().environment, "frame", frame );
							
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
	
	public static void initScheme( Scheme scheme ) {
		Environment.setCurrent(environment);
		if ( ! SchemeUtils.isDefined(scheme, FLAG_DONE_INIT_PULSAR_SCRATCHPAD ) ) {
			logInfo("initScheme");
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
		}
	}
	
	private void initFrame() {
		SchemeUtils.defineVar(scheme, frameName, this );
		eventHandlers.invokeEventHandler( this, scheme, EventHandlers.INIT,  this);
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
	
	Container scratchPadRoot;
	JTextPane textPane;
	PulsarScratchPadListener textPaneController;
	JScrollPane scrollPane; 
	JMenuBar menuBar;

	public JTextPane getTextPane() {
		return textPane;
	}
	@Override
	public MenuBar getMenuBar() {
		return super.getMenuBar();
	}
	
	{
		textPane = new JTextPane();
		scrollPane = new JScrollPane( textPane );
		textPaneController = new PulsarScratchPadListener();
		scratchPadRoot = getContentPane();
		scratchPadRoot.add( scrollPane );
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
//					System.out.println("YEAH!");
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
		                	String indentString = SimpleSchemePrettifier.calculateIndentSize(text, pos, getLispWords( scheme ));
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
			System.out.println( "do UNDO" );
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
			System.out.println( "do REDO" );
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
        	System.out.println("PulsarScratchPad.PasteAction.actionPerformed()");
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
//                System.out.println( "typed : " + content );
                switch ( content ) {
                	case " " :
                		undoManager.startGroup();
                		break;
                		
                	case "(" :
                	case ")" :
                		int pos = textPane.getCaretPosition() -1;
                		System.out.println( "caret : " + pos );
                		SwingUtilities.invokeLater(new Runnable() {
							@Override
							public void run() {
								updateHighlight();
								PulsarScratchPadHighlighter.highlightMatchingParenthesis( textPane, pos  ); 

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
                
				eventHandlers.invokeEventHandler( PulsarScratchPad.this, scheme, EventHandlers.TYPED, SchemeUtils.toSchemeString( content )  );
            }
		}
	};

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
//			System.out.println(o );
//		}
//		textPane.getActionMap().put("UNDO", UNDO_ACTION );
//		textPane.getActionMap().put("REDO", REDO_ACTION );

//		undoManager.addEdit(anEdit)
		textPane.getDocument().addUndoableEditListener( undoManager );
	}
	
	boolean fileModified = false;
	String filePath;
	public void openFile( String filePath ) {
		JOptionPane.showMessageDialog(this, "YEAH!");
	}
	
	public void confirmSave() {
		if ( fileModified ) {
			int i = JOptionPane.showConfirmDialog( this, "Do you save the changes to a file before loading?", "Load a file", JOptionPane.YES_NO_CANCEL_OPTION  );
			if ( i != JOptionPane.YES_OPTION ) {
				return;
			}
		}
	}

	Action OPEN_NEW = new AbstractAction() {
		@Override
		public void actionPerformed(ActionEvent e) {
			openFile( "" );
		}
		{
			putValue( Action2.NAME, "Open" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_O , KeyEvent.CTRL_MASK ));
			putValue( Action.MNEMONIC_KEY , (int) 'o' );
		}
	};

	Action OPEN_FILE = new AbstractAction() {
		@Override
		public void actionPerformed(ActionEvent e) {
			openFile( "" );
		}
		{
			putValue( Action2.NAME, "Open" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_O , KeyEvent.CTRL_MASK ));
			putValue( Action.MNEMONIC_KEY , (int) 'o' );
		}
	};
	
	
	
	
	
	
	
	
	
	{
		if ( false ) {
			System.err.println( "-------------------------" );
			for ( Object o : textPane.getActionMap().getParent().allKeys() ) {
				if ( o != null )
					System.out.println( o.toString() );
			}
			System.err.println( "==========================" );
			
			for ( Object o : textPane.getActionMap().allKeys() ) {
				if ( o != null )
					System.out.println( o.toString() );
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

		JMenu schemeMenuItem = new JMenu( "Scheme" );
		schemeMenuItem.setMnemonic('s');
		menuBar.add( schemeMenuItem );

		///
		
		fileMenuItem.add( new JMenuItem( NEW_SCRATCHPAD_ACTION ) );
		fileMenuItem.add( new JMenuItem( OPEN_FILE ) );

		schemeMenuItem.add( new JMenuItem( EXECUTE_ACTION ) );
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
		setDefaultCloseOperation( JFrame.DISPOSE_ON_CLOSE );
		
		setVisible(true);
	}
	

	static {
		ActionMap actionMap = new JTextPane().getActionMap();
		
		// Dump
		if ( false ) 
			for ( Object o : actionMap.allKeys() ) {
				System.out.println(o );
			}
		
		actionMap.get( DefaultEditorKit.deletePrevCharAction ).putValue(Action2.NAME, "Backspace");
//		actionMap.get( DefaultEditorKit.copyAction ).putValue(Action2.NAME, "Backspace");
	}
	
	public static void main(String[] args) {
		Scheme scheme = new Scheme();
		new PulsarScratchPad( scheme );
	}
}
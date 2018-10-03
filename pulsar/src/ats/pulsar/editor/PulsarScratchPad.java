package ats.pulsar.editor;

import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultCaret;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.DefaultEditorKit.DefaultKeyTypedAction;
import javax.swing.text.JTextComponent;
import javax.swing.text.TextAction;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoManager;

import ats.pulsar.Pulsar;
import ats.pulsar.SchemeUtils;
import ats.pulsar.editor.CompoundUndoManager.RelaxUndoableEdit;
import ats.pulsar.editor.PulsarScratchPadTextPaneController.PulsarScratchPadTextPaneListener;
import gnu.lists.IString;
import gnu.lists.Pair;
import gnu.lists.PrintConsumer;
import gnu.mapping.Symbol;
import kawa.standard.Scheme;

public class PulsarScratchPad extends JFrame {
	private static final Logger LOGGER = Logger.getLogger(Pulsar.class.getName());
	static void logError( String msg, Throwable e ) {
		LOGGER.log(Level.SEVERE, msg, e);
		//		System.err.println( msg );
	}
	static void logInfo( String msg ) {
		//        Logger.getLogger(Pulsar.class.getName()).log(Level.INFO, msg);
		System.err.println( msg );
	}

	Scheme scheme;
	public PulsarScratchPad( Scheme scheme ) {
		super( "Pulsar Scheme Scratch Pad" );
		this.scheme = scheme;
	}

	private final class InsertTextToTextPane implements Runnable {
		private final String result;
		private boolean isThereSelection;
		private InsertTextToTextPane(String result, boolean isThereSelection) {
			this.result = result;
			this.isThereSelection = isThereSelection;
		}
		@Override
		public void run() {
        	undoManager.setEnabled(true);
        	undoManager.startCompoundEdit();
        	try {
    			try {
    				if ( isThereSelection ) {
    					int selectionEnd = textPane.getSelectionEnd();
    					textPane.getDocument().insertString( selectionEnd, result, null);
    					textPane.setSelectionEnd( selectionEnd + result.length() );
    					textPane.setSelectionStart(selectionEnd + 1 );
    				} else {
    					textPane.getDocument().insertString( textPane.getText().length(), result, null);
    				}

    			} catch (BadLocationException e1) {
    				e1.printStackTrace();
    			}
        	} finally {
        		undoManager.endCompoundEdit();
            	undoManager.setEnabled(false);
        	}

		}
	}

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
	ArrayDeque<Thread> scratchPadThreadList = new ArrayDeque<>();
	void addScratchPadThread( Thread t ) {
		synchronized ( scratchPadThreadList ) {
			scratchPadThreadList.add( t );
		}
	}
	void startScratchPadThread( Runnable r ) {
		Thread t = new ScratchPadThread(r);
		addScratchPadThread(t);
		t.start();
	}
	void removeScratchPadThread( Thread t ) {
		synchronized ( scratchPadThreadList ) {
			scratchPadThreadList.remove( t );
		}
	}
	void interruptScratchPadThreads() {
		System.out.println("interruptScratchPadThreads");
		synchronized ( scratchPadThreadList ) {
			for ( Thread t : scratchPadThreadList ) {
				System.out.println( "interrupt start" );
				t.interrupt();
				System.out.println( "interrpt end" );
			}
		}
	}
	

	///////////////////////////////////////////////////////////////////////////////////

	public final AbstractAction EXECUTE_ACTION = new ExecuteAction();
	private final class ExecuteAction extends AbstractAction {
		@Override
		public void actionPerformed(ActionEvent e) {
			//	JOptionPane.showMessageDialog( JPulsarScratchPad.this, "", "AAAA" , JOptionPane.INFORMATION_MESSAGE  );
			startScratchPadThread( new Runnable() {
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
						resultObject = scheme.eval( text );
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
			interruptScratchPadThreads();
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

	public static void formatProc(JTextComponent textPane, TextFilter filter ) {
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

	private class ChangeIndentAction extends AbstractAction {
		int difference;
		public ChangeIndentAction(int difference) {
			super();
			this.difference = difference;
		}
		@Override
		public void actionPerformed(ActionEvent e) {
        	undoManager.setEnabled(true);
        	undoManager.startCompoundEdit();
        	try {
    			formatProc( textPane, new TextFilter() {
    				@Override
    				String process(String text) {
    					return SimpleSchemeIndentChanger.changeIndentRelativeMultiline( text, difference );
    				}
    			});
        	} finally {
        		undoManager.endCompoundEdit();
            	undoManager.setEnabled(false);
        	}

			//	JOptionPane.showMessageDialog( JPulsarScratchPad.this, "", "AAAA" , JOptionPane.INFORMATION_MESSAGE  );
		}
	}
	
	public final AbstractAction INCREASE_INDENT_ACTION = new ChangeIndentAction( +2 ) {
		{
			putValue( Action2.NAME, "Increase Indentation" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_TAB , 0 ) ) ;
			putValue( Action.MNEMONIC_KEY , (int) 'c' );
		}
	};
	public final AbstractAction DECREASE_INDENT_ACTION = new ChangeIndentAction( -2 ) {
		{
			putValue( Action2.NAME, "Decrease Indentation" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_TAB , KeyEvent.SHIFT_MASK ) );
			putValue( Action.MNEMONIC_KEY , (int) 'd' );
		}
	};

	static List<String> FALLBACK_LISP_WORDS = Arrays.asList("let","lambda" );
	
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

	
	private class PrettifyAction extends AbstractAction {
		@Override
		public void actionPerformed(ActionEvent e) {
        	undoManager.setEnabled(true);
        	undoManager.startCompoundEdit();
        	try {

        		formatProc( textPane, new TextFilter() {
        			@Override
        			String process(String text) {
        				return SimpleSchemePrettifier.prettify( getLispWords( scheme ), text );
        			}

        		});

        		//	JOptionPane.showMessageDialog( JPulsarScratchPad.this, "", "AAAA" , JOptionPane.INFORMATION_MESSAGE  );

        	} finally {
        		undoManager.endCompoundEdit();
            	undoManager.setEnabled(false);
        	}
		}
	}

	public final AbstractAction PRETTIFY_ACTION = new PrettifyAction() {
		{
			putValue( Action2.NAME, "Correct Indentation" );
			putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_I , KeyEvent.CTRL_MASK ));
			putValue( Action.MNEMONIC_KEY , (int) 'i' );
		}
	};
	
	public static void initScheme( Scheme scheme ) {
		SchemeUtils.defineVar(scheme, "lisp-words",  
				Pair.makeList( (List)SchemeUtils.<String,IString>convertList( 
						Arrays.asList( SimpleSchemePrettifier.LISP_WORDS ),
						(o)->{
							return SchemeUtils.toSchemeString( o );
						}) 
						)
				);
	}


	///////////////////////////////////////////////////////////////////////////////////

	static void purgeKeyFromActionMap( ActionMap actionMap, Object key ) {
		actionMap.remove(key);
		if ( actionMap.getParent() != null )
			purgeKeyFromActionMap(actionMap.getParent(), key );
	}
	
	Container scratchPadRoot;
	JTextPane textPane;
	JScrollPane scrollPane; 
	PulsarScratchPadTextPaneController controller;
	{
		textPane = new JTextPane();
		scrollPane = new JScrollPane( textPane );
		controller = PulsarScratchPadTextPaneController.create( textPane, new PulsarScratchPadTextPaneListener() {
			@Override
			public Collection<String> getLispWords() {
				return PulsarScratchPad.getLispWords( scheme );
			}
		});
		scratchPadRoot = getContentPane();
		scratchPadRoot.add( scrollPane );
        textPane.setFont(new Font("monospaced", Font.PLAIN, 12));
        
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
		                
		                {
		                	String text = target.getText();
		                	int pos = target.getCaretPosition();
		                	String indentString = SimpleSchemePrettifier.calculateIndentSize(text, pos, getLispWords( scheme ));
		                	undoManager.setEnabled(true);
		                	undoManager.startCompoundEdit();
		                	try {
		                		target.replaceSelection( "\n" + indentString );
		                	} finally {
		                		undoManager.endCompoundEdit();
			                	undoManager.setEnabled(false);
		                	}
		                }
		            }
				}

			};
//			purgeKeyFromActionMap( textPane.getActionMap(), DefaultEditorKit.insertBreakAction );
			textPane.getActionMap().put(DefaultEditorKit.insertBreakAction, newInsertBreakAction );
		}
	}
	///////////////////////////////////////////////////////////////////////////////////
	CompoundUndoManager undoManager;
	{
		// https://stackoverflow.com/questions/2547404/using-undo-and-redo-for-jtextarea
		undoManager = new CompoundUndoManager( textPane );
	}

	private abstract static class UndoRedoAction extends AbstractAction {
		protected final UndoManager undoManager;
		protected UndoRedoAction( String name,  UndoManager undoManager ) {
			super(name);
			this.undoManager = undoManager;
		}
	}

	Action UNDO_ACTION = new UndoAction( "Undo", undoManager );
	public static class UndoAction extends UndoRedoAction {
		public UndoAction(String name, UndoManager manager ) {
			super(name,manager);
		}

		public void actionPerformed(ActionEvent actionEvent) {
			try {
				undoManager.undo();
			} catch (CannotUndoException e) {
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

	Action REDO_ACTION = new RedoAction( "Redo", undoManager );
	public static class RedoAction extends UndoRedoAction {
		public RedoAction(String name, UndoManager manager) {
			super(name,manager);
		}
		public void actionPerformed(ActionEvent actionEvent) {
			try {
				undoManager.redo();
			} catch (CannotRedoException e) {
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

	
	{
		
		{
			Action newKeyTypedAction = new DefaultKeyTypedAction() {
				@Override
				public void actionPerformed(ActionEvent e) {
//            		System.out.println( "keytyped" );
					undoManager.setEnabled(true);
					undoManager.startCompoundEdit();
					try {
						super.actionPerformed(e);
					} finally {
						undoManager.endCompoundEdit();
						undoManager.setEnabled(false);
					}

					JTextPane target = (JTextPane) getTextComponent(e);
		            if ((target != null) && (e != null)) {
		                String content = e.getActionCommand();
		                System.out.println( "typed : " + content );
		                switch ( content ) {
		                	case "(" :
		                	case ")" :
		                		int pos = textPane.getCaretPosition() -1;
//		                		System.out.println( "caret : " + pos );
		                		SwingUtilities.invokeLater(new Runnable() {
									@Override
									public void run() {
										controller.lookupMatchingParenthesis( textPane, pos  ); 
										undoManager.addEdit( new RelaxUndoableEdit( "Typed", true ) );
									}
								});
		                		break;
		                		
		                	default :
//		                		SwingUtilities.invokeLater(new Runnable() {
//									@Override
//									public void run() {
//										System.out.println( "default" );
//										undoManager.addEdit( new RelaxUndoableEdit( "Typed", true ) );
//									}
//								});
		                		break;
		                }
		            }
				}
			};
//			purgeKeyFromActionMap( textPane.getActionMap(), DefaultEditorKit.insertBreakAction );
//			purgeKeyFromActionMap( textPane.getActionMap(), DefaultEditorKit.defaultKeyTypedAction );
//			purgeKeyFromActionMap( textPane.getActionMap(), DefaultEditorKit.insertContentAction );
//			textPane.getActionMap().put(DefaultEditorKit.defaultKeyTypedAction, newKeyTypedAction );
//			for ( Object o : textPane.getActionMap().getParent().getParent(). allKeys() ) {
//				System.out.println(o );
//			}
			textPane.getKeymap().setDefaultAction( newKeyTypedAction  );

//			undoManager.addEdit(anEdit)
			textPane.getDocument().addUndoableEditListener( undoManager );

		}
		
		
		
//		System.err.println( "-------------------------" );
//		for ( Object o : textPane.getActionMap().getParent().allKeys() ) {
//			if ( o != null )
//				System.out.println( o.toString() );
//		}
//		System.err.println( "==========================" );
//		
//		for ( Object o : textPane.getActionMap().allKeys() ) {
//			if ( o != null )
//				System.out.println( o.toString() );
//		}
	}

	{
		JMenuBar menuBar = new JMenuBar();
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

		schemeMenuItem.add( new JMenuItem( EXECUTE_ACTION ) );
		schemeMenuItem.add( new JMenuItem( INTERRUPT_ACTION ) );

		editMenuItem.add( new JMenuItem( UNDO_ACTION ) );
		editMenuItem.add( new JMenuItem( REDO_ACTION ) );

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
		if ( false ) 
			for ( Object o : actionMap.allKeys() ) {
				System.out.println(o );
			}
		actionMap.get( DefaultEditorKit.deletePrevCharAction ).putValue(Action2.NAME, "Backspace");
//		actionMap.get( DefaultEditorKit.copyAction ).putValue(Action2.NAME, "Backspace");
	}
	public static void main(String[] args) {
		Scheme scheme = new Scheme();
		PulsarScratchPad.initScheme(scheme);
		new PulsarScratchPad( scheme );
	}
}
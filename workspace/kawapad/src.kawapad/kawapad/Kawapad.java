package kawapad;

// === Table of Contents === 
//
// Search these keywords to go to the proper code block :
//    - CONTENT_ASSIST
//
// KeyStroke setting
//    - putAcceleratorKeyList

// The order of modifiers on KeyStroke is always as "ctrl", "alt" and then, "shift".

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Font;
import java.awt.FontFormatException;
import java.awt.Graphics;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.lang.invoke.MethodHandles;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.logging.Level;

import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.JViewport;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.plaf.FontUIResource;
import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import javax.swing.text.DefaultCaret;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.DefaultEditorKit.DefaultKeyTypedAction;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Document;
import javax.swing.text.Highlighter.HighlightPainter;
import javax.swing.text.JTextComponent;
import javax.swing.text.Segment;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;

import gnu.mapping.Symbol;
import gnu.mapping.Values;
import gnu.mapping.WrongArguments;
import kawapad.KawapadSelection.ExpandParenthesisSelector;
import kawapad.KawapadSelection.SearchNextWordTransformer;
import kawapad.KawapadSelection.ShrinkParenthesisSelector;
import kawapad.KawapadSelection.SideParenthesisSelector;
import kawapad.lib.undomanagers.GroupedUndoManager;
import kawapad.lib.undomanagers.UndoManagers;
import lamu.lib.apps.ApplicationComponent;
import lamu.lib.evaluators.AsyncEvaluator;
import lamu.lib.evaluators.AsyncThreadManager;
import lamu.lib.evaluators.EvaluatorReceiver;
import lamu.lib.evaluators.MultipleEvaluatorMenuListener;
import lamu.lib.evaluators.MultiplexEvaluator;
import lamu.lib.evaluators.SchemeEvaluatorUtils;
import lamu.lib.evaluators.SchemeResult;
import lamu.lib.evaluators.ThreadManager;
import lamu.lib.helps.LamuDocument;
import lamu.lib.kawautils.SchemePrinter;
import lamu.lib.kawautils.SchemeValues;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure1;
import lamu.lib.kawautils.procedures.MultipleNamedProcedure2;
import lamu.lib.kawautils.procedures.MultipleNamedProcedureN;
import lamu.lib.log.Logger;
import lamu.lib.log.SimpleConsole;
import lamu.lib.swing.AcceleratorKeyList;
import lamu.lib.swing.Action2;
import lamu.lib.swing.AutomatedActionField;
import lamu.lib.swing.MenuInitializer;
import lamu.lib.swing.TextAction2;
import lamu.lib.swing.doc.ActionDocumentFormatter;
import lamu.lib.threads.LamuThreadLocal;

/**
 */
public class Kawapad extends JTextPane implements MenuInitializer, ApplicationComponent {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    public static interface KawapadListener {
        public abstract void process( Kawapad kawadapd );
    }
    
    /**
     * This is a map for debugging or something. This map is intended to be used for
     * keeping values from Scheme.
     * 
     * The current environment is frequently scrapped and replaced in Kawapad/Pulsar
     * system. In Scheme, you cannot keep the same value between those multiple
     * environments. This map is intended to be used as a place to keep values
     * without environments. (Sat, 17 Aug 2019 13:10:44 +0900)
     */
    public static final Map<Object,Object> memoMap = new HashMap<Object,Object>();
    
    ////////////////////////////////////////////////////////////////////////////

    public static boolean ENABLED_CONFIRMATION = true;
    private static final boolean DEBUG_UNDO_BUFFER = true;
    static final boolean DEBUG = false;
    static final boolean DEBUG_PARENTHESIS = false;
    private static final boolean ENABLED_PARENTHESIS_HIGHLIGHT = true;
    static final boolean ENABLED_SHOW_CORRESPONDING_PARENTHESES = true;
    static final boolean DEFAULT_CARET = true;
    // ADDED (Fri, 06 Sep 2019 01:05:27 +0900)
    private static final boolean ENABLED_SYNTAX_HIGHLIGHTING = true;
    
    /**
     *  See {@link DeselectAction}
     *   (Sun, 31 May 2020 17:30:02 +0900)
     */
    private static final String EDITORKIT_UNSELECT = "unselect".intern();

    ////////////////////////////////////////////////////////////////////////////

    static volatile int uniqueIDCounter = 0;

    static String getUniqueID( int uniqueIDCounter ) {
        return "kawapad-" + uniqueIDCounter;
    }
    synchronized static String newUniqueID() {
        return "kawapad-" + ( uniqueIDCounter ++ );
    }

    ////////////////////////////////////////////////////////////////////////////
    private ApplicationComponent parentApplicationComponent;
    @Override
    public ApplicationComponent getParentApplicationComponent() {
        return this.parentApplicationComponent;
    }
    @Override
    public void setParentApplicationComponent(ApplicationComponent parentApplicationComponent) {
        this.parentApplicationComponent = parentApplicationComponent;
    }
    boolean initProcessed = false;
    @Override
    public synchronized void processInit() {
        if ( initProcessed )
            return;
        initProcessed = true;
    }
    
    @Override
    public void processQuit() {
    }
    
    {
        // logInfo("****************8" + getEditorKit().toString() );
    }
    
    ////////////////////////////////////////////////////////////////////////////

    Kawapad kawapad=this;
    @Deprecated
    String instanceID = newUniqueID();
    @Deprecated
    public String getInstanceID() {
        return instanceID;
    }

    ////////////////////////////////////////////////////////////////////////////

    // This is no longer necessary. (Thu, 26 Mar 2020 04:24:28 +0900)
    //  // This must be done before the constructor and other initializers.
    //  // (Sun, 15 Sep 2019 09:16:53 +0900)
    //  {
    //      this.setDocument( new SyntaxHighlighterStyledDocument() );
    //  }
    //    
    
    ////////////////////////////////////////////////////////////////////////////
    // 
    ////////////////////////////////////////////////////////////////////////////
    static ArrayList<Kawapad> kawapadList = new ArrayList<>();
    private final ThreadManager threadManager = new AsyncThreadManager();
    public ThreadManager getThreadManager() {
        return threadManager;
    }

    protected MultiplexEvaluator multiplexEvaluator;
    public MultiplexEvaluator getEvaluator() {
        return multiplexEvaluator;
    }
    private final MultipleEvaluatorMenuListener multipleEvaluatorMenuListener = new MultipleEvaluatorMenuListener();
    public MultipleEvaluatorMenuListener getMultipleEvaluatorMenuListener() {
        return multipleEvaluatorMenuListener;
    }
    
    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
//        g.drawRect(100, 100 , 100, 100);
    }
    
    private final ArrayDeque<Object> resultHistoryQueue = new ArrayDeque<Object>();
    public synchronized void addResultHistory( Object value ) {
        if (    value != null         && 
                value != Values.empty && 
            ! ( value instanceof KawapadHistoryObject) ) 
        {
            this.resultHistoryQueue.addFirst( value );
        }
        
        // Limit the length of the result history queue.
        while ( 32 < this.resultHistoryQueue.size() ) {
            this.resultHistoryQueue.removeLast();
        }
    }
    public synchronized List<Object> getResultHistoryAsScheme( int size ) {
        return PairFactory.makeList( KawapadHistoryPair.FACTORY, getResultHistory().subList(0, size ) );
    }
    public synchronized List<Object> getResultHistoryAsScheme() {
        return PairFactory.makeList( KawapadHistoryPair.FACTORY, getResultHistory());
    }
    public synchronized ArrayList<Object> getResultHistory() {
        ArrayList<Object> arrayList = new ArrayList<Object>( this.resultHistoryQueue );
        return arrayList;
    }
    public synchronized void setResultHistory( Collection<Object> history ) {
        this.resultHistoryQueue.clear();
        this.resultHistoryQueue.addAll( history );
    }
    
    
    public synchronized void clearResultHistory() {
        this.resultHistoryQueue.clear();
    }
    public static final Object RESULT_HISTORY_MARKER = "KAWAPAD_RESULT_HISTORY";

    
    public Kawapad( MultiplexEvaluator multiplexEvaluator ) {
        super();
        if ( multiplexEvaluator == null )
            throw new NullPointerException();
        this.multiplexEvaluator = multiplexEvaluator;
        this.multiplexEvaluator.addListener( multipleEvaluatorMenuListener );
        
        // init font
        // kawapad.setFont( new Font("monospaced", Font.PLAIN, 12));
        
        // See https://stackoverflow.com/questions/49818079/java-selected-text-over-highlighted-text
        // (Mon, 16 Sep 2019 14:20:03 +0900)
        ((DefaultHighlighter)this.getHighlighter()).setDrawsLayeredHighlights( true );
        
        /*
         * (Sun, 07 Oct 2018 23:50:37 +0900) CREATING_KEYMAP
         * 
         * THIS IS VERY IMPORTANT. I SPENT THREE SLEEPLESS NIGHTS TO FIND THIS OPERATION
         * IS NECESSARY. This keymap object is SHARED as default! Those key handlers on
         * a keymap object will be definitely overridden unless you explicitly create a
         * new keymap object.
         * 
         * See CREATING_KEYMAP
         */
        kawapad.setKeymap( JTextComponent.addKeymap( this.instanceID, kawapad.getKeymap() ) );
        
        // This action intercepts our customization so delete it.
        AcceleratorKeyList.purgeKeyFromActionMap( kawapad, DefaultEditorKit.insertTabAction );
        
        
        // Added (Sun, 31 May 2020 17:28:45 +0900)
        // Remove the default unselect action. 
        // This default action effectively prevents our customization.
        if ( false ){
            // see BUG_UNSELECT (Sun, 31 May 2020 19:06:39 +0900)
            AcceleratorKeyList.purgeKeyFromActionMap( kawapad, EDITORKIT_UNSELECT );
            AcceleratorKeyList.dump(kawapad.getActionMap());
        }
        
        
        this.syntaxHighlighter = new KawapadSyntaxHighlighter( this );

//        MOVED TO EDITOR KIT (Sun, 29 Mar 2020 04:07:39 +0900) >>>
//        if ( ENABLED_SYNTAX_HIGHLIGHTING ) {
//            ((AbstractDocument)getDocument()).setDocumentFilter(getSyntaxHighlighter());
//        }
//      MOVED TO EDITOR KIT (Sun, 29 Mar 2020 04:07:39 +0900) <<<
        
        // https://stackoverflow.com/questions/6189599/automatically-causing-a-subclassed-jpanels-resources-to-be-released
        this.addHierarchyListener( new HierarchyListener() {
            @Override
            public void hierarchyChanged(HierarchyEvent e) {
                if ( 0 != ( e.getChangeFlags() & HierarchyEvent.DISPLAYABILITY_CHANGED ) ) {
                    if ( ((Component)e.getSource()).isDisplayable() ) {
                        synchronized ( Kawapad.class ) {
                            kawapadList.add( Kawapad.this ); 
                        }
                    } else {
                        synchronized ( Kawapad.class ) {
                            kawapadList.remove( Kawapad.this ); 
                        }
                    }
                }
            }
        });
        
        AcceleratorKeyList.processAcceleratorKeys( this );

        this.setEditorKit( new KawapadStyledEditorKit() {
            @Override
            public Document createDefaultDocument() {
                SyntaxHighlighterStyledDocument document = (SyntaxHighlighterStyledDocument) super.createDefaultDocument();
                
                document.addDocumentListener( kawapadDocumentListener );
                
                if ( ENABLED_SYNTAX_HIGHLIGHTING ) {
                    document.setDocumentFilter(getSyntaxHighlighter());
                }
                document.addUndoableEditListener( getUndoManager() );
                
                return document;
            }
        });
        
        //////////////////////////////////////////////////
        
        // this causes NPE and this is not used anymore; removed 
        // (Sun, 19 Apr 2020 18:02:45 +0900)
        if ( false ) {
            File f = Kawapad.getExtFile();
            if ( f.isFile() && f.exists() ) {
                SchemeResult result = multiplexEvaluator.evaluate( null, f );
                if ( result.isSucceeded() ) {
                    Object value = result.getValue();
                    if ( value instanceof Kawapad.KawapadListener ) {
                        ((Kawapad.KawapadListener)value).process(Kawapad.this);
                    } else {
                        logWarn( "a value of an unsupported type; ignored." );
                    }
                }
            }
        }
    }
    
    public String outputKeyStrokeReference() {
        return ActionDocumentFormatter.formatActions( this );
    }

//    //////////////////////////////////////////////////////////////////////////////////////////
//    //
//    // Thread Manager
//    //
//    //////////////////////////////////////////////////////////////////////////////////////////
//
//    private final ThreadManager threadManager = new KawapadThreadManager();
//    public ThreadManager getThreadManager() {
//        return threadManager;
//    }
    

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // Initialization
    ///////////////////////////////////////////////////////////////////////////////////////////////
    
//    /**
//     * (Sun, 22 Dec 2019 14:44:05 +0900)
//     * This initVariable() is incompleted ... but I think it should not be fixed.
//     * I think this functionarity should rather be deprecated, 
//     * Because :
//     * - it is almost completely unused.
//     * - it makes the current code base more complecated.
//     * 
//     * FIXME
//     */
//    public void initVariable() {
//        logInfo( "initVariable******************************************************************" );
//        Utils.putVar( Environment.getCurrent(), instanceID, Kawapad.this );
//    }
//
//    volatile boolean initializeDone = false;
//    public synchronized void initialize() {
//        if ( initializeDone ) return;
//        initializeDone = true;
//        initVariable();
//        Kawapad.eventHandlers.invokeEventHandler( kawapad, KawapadEventHandlers.CREATE, kawapad );
//    }
//    volatile boolean finalizeDone = false;
//    public synchronized void finalize() {
//        if ( finalizeDone ) return;
//        finalizeDone = true;
//    }

    //////////////////////////////////////////////////////////////////////////////////////////
    //
    // getCurrent (the second generation) (Wed, 03 Jun 2020 17:00:46 +0900)
    //
    //////////////////////////////////////////////////////////////////////////////////////////
    
    private static LamuThreadLocal<Kawapad> threadLocal = new LamuThreadLocal<Kawapad>();
    public static final Kawapad getCurrent() {
        return threadLocal.get();
    }
    static final void setCurrent( Kawapad kawapad ) {
        threadLocal.set( kawapad );
    }
    
    private List<Runnable> threadInitializerList = new ArrayList<Runnable>();
    public void addThreadInitializer( Runnable r ) {
        this.threadInitializerList.add(r);
    }
    public void removeThreadInitializer( Runnable r ) {
        this.threadInitializerList.remove(r);
    }
    
    //////////////////////////////////////////////////////////////////////////////////////////
    //
    // utilities
    //
    //////////////////////////////////////////////////////////////////////////////////////////

    public void evaluate( String schemeScript, EvaluatorReceiver receiver ) {
        if ( schemeScript != null ) {
            MultiplexEvaluator evaluator = this.kawapad.getEvaluator();
            ThreadManager threadManager  = this.kawapad.getThreadManager();
            AsyncEvaluator.executeAsync(
                threadManager, 
                ()->{
                    try {
                        setCurrent( this );
                    } catch ( Throwable e ) {
                        logError("", e);
                    }
                    for ( Runnable t : threadInitializerList ) {
                        try {
                            t.run();
                        } catch ( Throwable e ) {
                            logError("", e);
                        }
                    }
                }, 
                new StringReader( schemeScript ), 
                evaluator, 
                receiver, 
                kawapad.getCurrentFile(), 
                "scratchpad" );
        } else {
            Kawapad.logWarn( "Ignored because currently no text is selected. " );
        }
    }
    public void evaluate( String schemeScript,  boolean doInsertText, boolean doReplaceText, boolean doResetFileModified ) {
        this.evaluate( 
            schemeScript, 
            KawapadUpdater.create( kawapad, schemeScript, doInsertText, doReplaceText, true, doResetFileModified ) );
    }
    
    //////////////////////////////////////////////////////////////////////////////////////////
    //
    // initialization
    //
    //////////////////////////////////////////////////////////////////////////////////////////
    
    // caret
    {
        if ( ! DEFAULT_CARET ) {
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
                            //                        g.setXORMode( Color.WHITE );
                            g.setColor( Color.WHITE );
                            g.fillRect(r.x, r.y , 1, r.height);
                            //                      g.fillRect(r.x, r.y , r.width+5, r.height);
                        }
                    }
                }
            };
            dc.setBlinkRate(400);
            kawapad.setCaret( dc );
        }
    }
    
    //  Action inserBreakAction = textPane.getActionMap().get( DefaultEditorKit.insertBreakAction );
    // INTEGRATED_ACTIONS_DEFAULT (Wed, 11 Sep 2019 08:26:57 +0900)
    public final Action KAWAPAD_INSERT_BREAK_ACTION = new NewInsertBreakTextAction( DefaultEditorKit.insertBreakAction );
    {
        //  purgeKeyFromActionMap( textPane.getActionMap(), DefaultEditorKit.insertBreakAction );
        kawapad.getActionMap().put( DefaultEditorKit.insertBreakAction, KAWAPAD_INSERT_BREAK_ACTION );
    }


    final class NewInsertBreakTextAction extends TextAction2 {
        private NewInsertBreakTextAction(String name) {
            super( name );
        }
        
        @Override
        public void actionPerformed(ActionEvent e) {
            //                  logInfo("YEAH!");
            if (kawapad != null) {
                if ((! kawapad.isEditable()) || (! kawapad.isEnabled())) {
                    UIManager.getLookAndFeel().provideErrorFeedback(kawapad);
                    return;
                }
                
                if ( contentAssistEnabled ) {
                    contentAssistEnabled = false;
                    contentAssist.complete( getCaret() );
                } else {
                    try {
                        kawapad.getUndoManager().startGroup();
                        kawapad.getUndoManager().setSuspended(true);
                        
                        String text = kawapad.getText();
                        int pos = kawapad.getCaretPosition();
                        String indentString = calculateIndentSize(text, pos, kawapad.getLispWordChecker() );
                        kawapad.replaceSelection( "\n" + indentString );
                    } finally {
                        kawapad.getUndoManager().setSuspended(false);
                        kawapad.getUndoManager().endGroup();
                    }
                }
            }
        }
    }
    public static final String calculateIndentSize( String text, int pos, Function<String,Integer> lispWordChecker ) {
        return SchemeIndentationCorrector.calculateIndentSize( text, pos, lispWordChecker );
    }
    
    
    ////////////////////////////////////////////////////////////////////////////

    private static final class WordJumpAction extends TextAction2 {
        private int direction;
        private boolean select;
        public WordJumpAction(String name, int direction, boolean select) {
            super(name);
            this.direction = direction;
            this.select = select;
        }
        public void actionPerformed(ActionEvent ae) {
            JTextComponent ta = (JTextComponent)ae.getSource();
            int p0 = ta.getCaretPosition();
            String text = ta.getText();
            
            int p1 = lookup(p0, text, direction);
            
            // Then, jump to there.
            int to;
            if ( p1 < 0 ) {
                to = 0;
            } else {
                to = p1;
            }
            Caret caret = ta.getCaret();
            if ( select ) {
                caret.moveDot(to);
            } else {
                caret.setDot(to);
            }
            
        }
        private static boolean isExclusiveBoundary( char ch ) {
            return Character.isWhitespace(ch); 
        }
        private static boolean isInclusiveBoundary( char ch ) {
            return ch == '"' || ch == ')' || ch == '('  || ch == '-' || ch == '_' || ch == '.' || ch == '/' || ch == '\\'  ; 
        }
        static int lookup(int p0, String text, int direction ) {
            int length = text.length();
            if ( length <= p0 ) {
                p0 = length-1;
            }
            int p1=-1;

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
                if ( isExclusiveBoundary( text.charAt( p0 ) ) ) {
                    for ( int i=p0; 0<=i&&i<length; i+=direction ) {
                        char ch = text.charAt( i );
                        if ( ! isExclusiveBoundary(ch) ) {
                            p1 = i;
                            break;
                        }
                    }
                } else if ( isInclusiveBoundary( text.charAt( p0 ) ) ) {
                    for ( int i=p0; 0<=i&&i<length; i+=direction ) {
                        char ch = text.charAt( i );
                        if ( ! isInclusiveBoundary(ch) ) {
                            p1 = i;
                            break;
                        }
                    }
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
    
    {
        AcceleratorKeyList.addActionToActionMap( this, new WordJumpAction(DefaultEditorKit.nextWordAction,1,false));
        AcceleratorKeyList.addActionToActionMap( this, new WordJumpAction(DefaultEditorKit.previousWordAction,-1,false));
        AcceleratorKeyList.addActionToActionMap( this, new WordJumpAction(DefaultEditorKit.selectionNextWordAction,1,true));
        AcceleratorKeyList.addActionToActionMap( this, new WordJumpAction(DefaultEditorKit.selectionPreviousWordAction,-1,true));
    }

    ////////////////////////////////////////////////////////////////////////////

    // INTEGRATED_ACTIONS_DEFAULT (Wed, 11 Sep 2019 08:26:57 +0900)
    public final Action DEFAULT_KEY_TYPE_ACTION = new DefaultKeyTypedAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
            boolean shouldProcess = true;
            Kawapad target = (Kawapad) getTextComponent(e);
            if ((target != null) && (e != null)) {
                String content = e.getActionCommand();
                switch ( content ) {
                    case " " :
                        break;
                    case "(" :
                        OPEN_PARENTHESIS_ACTION.actionPerformed( e );
                        shouldProcess= false;
                        break;
                    case ")" :
                        CLOSE_PARENTHESIS_ACTION.actionPerformed( e );
                        shouldProcess= false;
                        break;
                    default :
                        break;
                }
            }

            if ( shouldProcess )
                super.actionPerformed(e);
            
            if ((target != null) && (e != null)) {
                String content = e.getActionCommand();
//                  logInfo( "typed : " + content );
                switch ( content ) {
                    case " " :
//                          getUndoManager().startGroup();
                        break;
                        
                    case "(" :
                    case ")" :
                        kawapadCaretListener.updateMatchingParentheses2(3);
//                        kawapadListener.setCaretUpdateHighlightOffset(-1);
//                        updateHighlightParenthesesLater( target, -1 );
//                        highlightMatchningParentheses( kawapad, 0 );
                        break;
                    default :
                        break;
                }
            }
        }
    };

    ////////////////////////////////////////////////////////////////////////////

    {
        /*
         * (Sun, 07 Oct 2018 23:50:37 +0900) CREATING_KEYMAP
         * 
         * THIS IS VERY IMPORTANT: I SPEND THREE SLEEPLESS NIGHTS TO FIND OUT 
         * THAT THIS IS DEFINITELY NECESSARY TO FIX THE PROBLEM!
         * 
         * See the tag CREATING_KEYMAP .
         */
        kawapad.getKeymap().setDefaultAction( DEFAULT_KEY_TYPE_ACTION );
        
    }

    ////////////////////////////////////////////////////////////////////////////

    // This fix the caption for backspace menu (might be).
    private static JComponent createAncestor() {
        return new JTextPane();
    }
    private static ActionMap staticActionMap = createAncestor().getActionMap();
    private static Action getAction( String name ) {
        //      
        //      // Dump
        //      if ( false ) 
        //          for ( Object o : actionMap.allKeys() ) {
        //              logInfo( o == null ? null : o.toString() );
        //          }
        return staticActionMap.get( name );
    }
    @AutomatedActionField
    public final Action DEFAULT_DELETE_PREV_CHAR = getAction( DefaultEditorKit.deletePrevCharAction );
    {
        DEFAULT_DELETE_PREV_CHAR.putValue( Action2.CAPTION, "Backspace" );
        AcceleratorKeyList.putAcceleratorKeyList( DEFAULT_DELETE_PREV_CHAR, "shift BACK_SPACE", "ctrl shift H" );
    }

    
    @AutomatedActionField
    public final Action DEFAULT_DELETE_NEXT_CHAR = getAction( DefaultEditorKit.deleteNextCharAction );
    {
        DEFAULT_DELETE_PREV_CHAR.putValue( Action2.CAPTION, "Delete" );
        AcceleratorKeyList.putAcceleratorKeyList( DEFAULT_DELETE_PREV_CHAR, "shift DELETE");
    }

    
    //////////////////////////////////////////////////////////////////////////////////////////
    //
    // The Undo Manager
    //
    //////////////////////////////////////////////////////////////////////////////////////////

    private final GroupedUndoManager undoManager = UndoManagers.create();
    public GroupedUndoManager getUndoManager() {
        return undoManager;
    }
    
    private abstract static class UndoRedoAction extends TextAction2 {
        protected final GroupedUndoManager undoManager;
        protected UndoRedoAction( String name,  GroupedUndoManager undoManager ) {
            super(name);
            this.undoManager = undoManager;
        }
    }

    public static final String KAWAPAD_UNDO = "kawapad-undo-action"; 
    public static final String KAWAPAD_REDO = "kawapad-redo-action"; 
    
    static class UndoAction extends Kawapad.UndoRedoAction {
        public UndoAction(String name, GroupedUndoManager manager ) {
            super(name,manager);
        }

        public void actionPerformed(ActionEvent actionEvent) {
//            logInfo( "do UNDO" );
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
            putValue( Action2.CAPTION, "Undo" );
            putValue( Action.MNEMONIC_KEY , (int) 'u' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl Z" );
        }
    }
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action UNDO_ACTION = new UndoAction( KAWAPAD_UNDO, getUndoManager() );

    static class RedoAction extends Kawapad.UndoRedoAction {
        public RedoAction(String name, GroupedUndoManager manager) {
            super(name,manager);
        }
        public void actionPerformed(ActionEvent actionEvent) {
            // logInfo( "do REDO" );
            try {
                undoManager.redo();
            } catch (CannotRedoException e) {
                if ( DEBUG_UNDO_BUFFER )
                    logError("could not redo", e);
                else
                    logInfo( "could not redo" );
//                  showMessage(actionEvent.getSource());
            }
        }
        {
            putValue( Action2.CAPTION, "Redo" );
            putValue( Action.MNEMONIC_KEY , (int) 'r' );
            putValue( Action2.MENU_CATEGORY, "edit" );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl shift Z" );
        }
    }
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action REDO_ACTION = new RedoAction( KAWAPAD_REDO, getUndoManager() );
    
    ////////////////////////////////////////////////////////////////////////////

    {
//          purgeKeyFromActionMap( textPane.getActionMap(), DefaultEditorKit.insertBreakAction );
//          purgeKeyFromActionMap( textPane.getActionMap(), DefaultEditorKit.defaultKeyTypedAction );
//          purgeKeyFromActionMap( textPane.getActionMap(), DefaultEditorKit.insertContentAction );
//          textPane.getActionMap().put(DefaultEditorKit.defaultKeyTypedAction, newKeyTypedAction );
//          for ( Object o : textPane.getActionMap().getParent().getParent(). allKeys() ) {
//              logInfo(o );
//          }
//          textPane.getActionMap().put( UNDO, UNDO_ACTION );
//          textPane.getActionMap().put(REDO, REDO_ACTION );
//          undoManager.addEdit(anEdit)
        
//        MOVED >>> (Sun, 29 Mar 2020 04:02:33 +0900)
//        MOVED TO EditorKit; see the constructor.
//        kawapad.getDocument().addUndoableEditListener( getUndoManager() );
//        MOVED >>> (Sun, 29 Mar 2020 04:02:33 +0900)

    }

    //////////////////////////////////////////////////////////////////////////////////////////
    
    public static final String KAWAPAD_DEBUG = "kawapad-debug";
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action DEBUG_ACTION = new DebugAction( KAWAPAD_DEBUG );
    class DebugAction extends TextAction2 {
        public DebugAction(String string) {
            super(string);
        }
        @Override
        public void actionPerformed(ActionEvent e) {
//              getUndoManager().dump();
        }
        {
            putValue( Action2.CAPTION, "Debug" );
            putValue( Action.MNEMONIC_KEY , (int) 'd' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl alt BACK_QUOTE" );
        }
    }

    // `DefaultEditorKit.unselect` was changed to package private with an unknown reason. 
    public static final String KAWAPAD_DESELECT = EDITORKIT_UNSELECT;  

    /**
     * This class is not working. See BUG_UNSELECT. (Sun, 31 May 2020 19:07:30 +0900)
     */
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action DESELECT_ACTION = new DeselectAction( KAWAPAD_DESELECT );
    class DeselectAction extends TextAction2 {
        public DeselectAction(String string) {
            super(string);
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            JTextComponent target = getTextComponent(e);
            Caret caret = target.getCaret();
            caret.setDot( caret.getDot() );
//            System.err.println( "SSSSSSSSSSSSSSSSSSSSS" );
        }
        {
            putValue( Action2.CAPTION, "Deselect" );
            putValue( Action.MNEMONIC_KEY , (int) 's' );
//            AcceleratorKeyList.putAcceleratorKeyList( this, "ESCAPE" );
        }
    }

    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action PASTE_ACTION = new KawapadPasteAction();
    class KawapadPasteAction extends TextAction2 {

        /** Create this object with the appropriate identifier. */
        public KawapadPasteAction() {
            super(DefaultEditorKit.pasteAction);
        }

        /**
         * The operation to perform when this action is triggered.
         *
         * @param e the action event
         */
        public void actionPerformed(ActionEvent e) {
            logInfo("Kawapad.PasteAction.actionPerformed()");
            JTextComponent target = getTextComponent(e);
            if (target != null) {
                try {
                    getUndoManager().startGroup();
                    getUndoManager().setSuspended(true);
                    target.paste();
                } finally {
                    getUndoManager().setSuspended(false);
                    getUndoManager().endGroup();
                }
            }
        }
        {
            putValue( Action2.CAPTION, "Paste" );
            putValue( Action.MNEMONIC_KEY , (int) 'p' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl V" );
        }
    }

    //////////////////////////////////////////////////////////////////////////////////////////
    //
    // SEARCH_BOX
    //
    //////////////////////////////////////////////////////////////////////////////////////////
    
    final KawapadSearchBox searchBox = new KawapadSearchBox( this );

    public static final String KAWAPAD_SEARCH = "kawapad-search-action"; 
    class SearchAction extends TextAction2 {
        public SearchAction(String name ) {
            super(name);
        }

        public void actionPerformed(ActionEvent actionEvent) {
            boolean visible = ! searchBox.isVisible();
            if ( visible ) {
                searchBox.showSearchBox();
            } else {
                searchBox.hideSearchBox();
            }
        }
        {
            putValue( Action2.CAPTION, "Search" );
            putValue( Action.MNEMONIC_KEY , (int) 's' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "control L" );
        }
    }
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action SEARCH_ACTION = new SearchAction( KAWAPAD_SEARCH );


    
    //////////////////////////////////////////////////////////////////////////////////////////
    //
    // CONTENT_ASSIST
    //
    //////////////////////////////////////////////////////////////////////////////////////////
    
    KawapadContentAssist contentAssist = new KawapadContentAssist( kawapad );
    boolean contentAssistEnabled = false;
    // INTEGRATED_ACTIONS_DEFAULT (Wed, 11 Sep 2019 08:26:57 +0900)
    public final Action DEFAULT_UP_ACTION       =  kawapad.getActionMap().get( DefaultEditorKit.upAction );
    // INTEGRATED_ACTIONS_DEFAULT (Wed, 11 Sep 2019 08:26:57 +0900)
    public final Action DEFAULT_DOWN_ACTION     =  kawapad.getActionMap().get( DefaultEditorKit.downAction );
    // INTEGRATED_ACTIONS_DEFAULT (Wed, 11 Sep 2019 08:26:57 +0900)
    public final Action DEFAULT_PAGE_UP_ACTION       =  kawapad.getActionMap().get( DefaultEditorKit.pageUpAction );
    // INTEGRATED_ACTIONS_DEFAULT (Wed, 11 Sep 2019 08:26:57 +0900)
    public final Action DEFAULT_PAGE_DOWN_ACTION     =  kawapad.getActionMap().get( DefaultEditorKit.pageDownAction );
    // INTEGRATED_ACTIONS_DEFAULT (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action DEFAULT_BACKWARD_ACTION =  kawapad.getActionMap().get( DefaultEditorKit.backwardAction );
    {
        AcceleratorKeyList.putAcceleratorKeyList( DEFAULT_BACKWARD_ACTION, "LEFT", "ctrl B" );
    }

    // INTEGRATED_ACTIONS_DEFAULT (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action DEFAULT_FORWARD_ACTION  =  kawapad.getActionMap().get( DefaultEditorKit.forwardAction );
    {
        AcceleratorKeyList.putAcceleratorKeyList( DEFAULT_FORWARD_ACTION, "RIGHT", "ctrl F" );
    }
    @AutomatedActionField
    public final Action DEFAULT_SELECT_BACKWARD_ACTION =  kawapad.getActionMap().get( DefaultEditorKit.selectionBackwardAction );
    {
        AcceleratorKeyList.putAcceleratorKeyList( DEFAULT_SELECT_BACKWARD_ACTION, "shift LEFT", "ctrl shift B" );
    }
    @AutomatedActionField
    public final Action DEFAULT_SELECT_FORWARD_ACTION =  kawapad.getActionMap().get( DefaultEditorKit.selectionForwardAction   );
    {
        AcceleratorKeyList.putAcceleratorKeyList( DEFAULT_SELECT_FORWARD_ACTION, "shift RIGHT", "ctrl shift F" );
    }
    @AutomatedActionField
    public final Action DEFAULT_SELECT_UP_ACTION =  kawapad.getActionMap().get( DefaultEditorKit.selectionUpAction );
    {
        AcceleratorKeyList.putAcceleratorKeyList( DEFAULT_SELECT_UP_ACTION, "shift UP", "ctrl shift P" );
    }
    @AutomatedActionField
    public final Action DEFAULT_SELECT_DOWN_ACTION =  kawapad.getActionMap().get( DefaultEditorKit.selectionDownAction   );
    {
        AcceleratorKeyList.putAcceleratorKeyList( DEFAULT_SELECT_DOWN_ACTION, "shift DOWN", "ctrl shift N" );
    }
    
    
    // INTEGRATED_ACTIONS_DEFAULT (Wed, 11 Sep 2019 08:26:57 +0900)
    // not used
    public final Action DEFAULT_ENTER_ACTION    =  kawapad.getActionMap().get( DefaultEditorKit.endLineAction );
    class KawapadCursorKeyAction extends TextAction2 {
        int direction;
        Action defaultAction;
        boolean page;
        public KawapadCursorKeyAction(String name, int direction, Action defaultAction, boolean page ) {
            super( name );
            this.direction = direction;
            this.defaultAction = defaultAction;
            this.page = page;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            if ( contentAssistEnabled ) {
                if ( page ) {
                    contentAssist.pageTo( direction );
                } else {
                    contentAssist.moveTo( direction );
                }
            } else {
                defaultAction.actionPerformed( e );
            }
        }
    }
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action KAWAPAD_UP_ACTION   = new KawapadCursorKeyAction( DefaultEditorKit.upAction,   -1, DEFAULT_UP_ACTION, false );
    {
        AcceleratorKeyList.putAcceleratorKeyList( KAWAPAD_UP_ACTION, "UP", "ctrl P" );
    }
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action KAWAPAD_DOWN_ACTION = new KawapadCursorKeyAction( DefaultEditorKit.downAction, +1, DEFAULT_DOWN_ACTION, false );
    {
        AcceleratorKeyList.putAcceleratorKeyList( KAWAPAD_DOWN_ACTION, "DOWN", "ctrl N" );
    }
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action KAWAPAD_PAGE_UP_ACTION   = new KawapadCursorKeyAction( DefaultEditorKit.pageUpAction,   -1, DEFAULT_PAGE_UP_ACTION, true );
    {
        AcceleratorKeyList.putAcceleratorKeyList( KAWAPAD_UP_ACTION, "UP", "ctrl P" );
    }
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action KAWAPAD_PAGE_DOWN_ACTION = new KawapadCursorKeyAction( DefaultEditorKit.pageDownAction, +1, DEFAULT_PAGE_DOWN_ACTION, true );
    {
        AcceleratorKeyList.putAcceleratorKeyList( KAWAPAD_DOWN_ACTION, "DOWN", "ctrl N" );
    }
    
    

    class KawapadScrollAction extends TextAction2 {
        int direction;
        Action defaultAction;
        public KawapadScrollAction(String name, int direction  ) {
            super( name );
            this.direction = direction;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            try {
                JScrollPane c = (JScrollPane)((JComponent)e.getSource()).getParent().getParent();
                JScrollBar sb = c.getVerticalScrollBar();
                int v = sb.getValue() + direction;
                if ( v < sb.getMinimum() ) v = sb.getMinimum();
                else if ( sb.getMaximum() < v ) v = sb.getMaximum();
                sb.setValue( v ); 
            } catch ( NullPointerException|ClassCastException t) {
                logError( "ignored an error ( usually this does not cause a problem. )", t );
            }
        }
    }
    
    public static final String KAWAPAD_SCROLL_DOWN = "kawapad-scroll-down";
    public static final String KAWAPAD_SCROLL_UP = "kawapad-scroll-up";

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action KAWAPAD_SCROLL_UP_ACTION   = new KawapadScrollAction( KAWAPAD_SCROLL_UP,   -12 );
    {
        AcceleratorKeyList.putAcceleratorKeyList( KAWAPAD_SCROLL_UP_ACTION /* , "ctrl UP" */ );
    }
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action KAWAPAD_SCROLL_DOWN_ACTION = new KawapadScrollAction( KAWAPAD_SCROLL_DOWN, +12 );
    {
        AcceleratorKeyList.putAcceleratorKeyList( KAWAPAD_SCROLL_DOWN_ACTION /*, "ctrl DOWN" */ );
    }

    public static final String KAWAPAD_CANCEL_SEARCH_BOX = "kawapad-cancel-search-box";
    
    // INTEGRATED_ACTIONS (Sat, 20 Jun 2020 09:14:25 +0900)
    @AutomatedActionField
    public final Action CANCEL_SEARCH_BOX_ACTION = new TextAction2(KAWAPAD_CANCEL_SEARCH_BOX) {
        @Override
        public void actionPerformed(ActionEvent e) {
            searchBox.hideSearchBox(); 
        }
        {
            putValue( Action2.CAPTION, "Cancel Search" );
            putValue( Action.MNEMONIC_KEY, (int)'s' );
//            AcceleratorKeyList.putAcceleratorKeyList( this, "ESCAPE" );
        }
    };

    public static final String KAWAPAD_DISABLE_CONTENT_ASSIST = "kawapad-disable-content-assist";
    public static final String KAWAPAD_ENABLE_CONTENT_ASSIST = "kawapad-enable-content-assist";
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action DISABLE_CONTENT_ASSIST_ACTION = new TextAction2(KAWAPAD_DISABLE_CONTENT_ASSIST) {
        @Override
        public void actionPerformed(ActionEvent e) {
            if ( contentAssistEnabled ) {
                contentAssistEnabled = false;
                contentAssist.hide();
            }
        }
        {
            putValue( Action2.CAPTION, "Disable Content Assist" );
            putValue( Action.MNEMONIC_KEY, (int)'d' );
//            AcceleratorKeyList.putAcceleratorKeyList( this, "ESCAPE" );
        }
    };

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action ENABLE_CONTENT_ASSIST_ACTION = new TextAction2( KAWAPAD_ENABLE_CONTENT_ASSIST ) {
        @Override
        public void actionPerformed(ActionEvent e) {
            contentAssistEnabled = true;
            contentAssist.updatePopup( kawapad.getCaret() );
        }
        {
            putValue( Action2.CAPTION, "Enable Content Assist" );
            putValue( Action.MNEMONIC_KEY, (int)'e' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl SPACE" );
        }
    };

    
    //////////////////////////////////////////////////////////////////////////////////////////
    //
    //
    //
    //////////////////////////////////////////////////////////////////////////////////////////
    
    private class KawapadDocumentListener implements DocumentListener  {
        //DocumentListener
        public void insertUpdate(DocumentEvent e) {
//            logInfo( "insert" );
//            updateMatchingParentheses2(2);
            kawapad.fileModified = true;
//              System.err.println("PulsarScratchPadTextPaneController.insertUpdate()");
//            kp.updatePopup( Kawapad.this.getCaret() );
        }
        public void removeUpdate(DocumentEvent e) {
            kawapad.fileModified = true;
//              System.err.println("PulsarScratchPadTextPaneController.removeUpdate()");
//            kp.updatePopup( Kawapad.this.getCaret() );
        }
        public void changedUpdate(DocumentEvent e) {
//              fileModified = true;
//              System.err.println("PulsarScratchPadTextPaneController.changedUpdate() : ignored");
//            updatePopup( Kawapad.this.getCaret() );
        }
    }
    
    private class KawapadCaretListener implements CaretListener {
        KawapadCaretListener() {
            super();
        }
        int offsetTTL = 0;
        void updateMatchingParentheses2() {
            int offset = 0;
            if ( 0 < offsetTTL ) {
                offsetTTL--;
                offset = -1;
            }
            if ( ! searchBox.isVisible() )
                KawapadTemporaryParenthesisHighlighter.forceClearHighlightedParenthesis();
            updateHighlight( kawapad, offset );
        }
        private void updateMatchingParentheses2(int i) {
            this.offsetTTL = i;
            updateMatchingParentheses2();
        }
        
        // CaretListener
        public void caretUpdate(CaretEvent e) {
            if ( DEBUG_PARENTHESIS )
                logInfo("PulsarScratchPadTextPaneController.caretUpdate()" + e );

            getParenthesisStack().checkSelectionStack();
            updateMatchingParentheses2();
            
            if ( contentAssistEnabled ) {
                SwingUtilities.invokeLater( new Runnable() {
                    @Override
                    public void run() {
                        contentAssist.updatePopup( kawapad.getCaret() );
                    }
                } );
            } else {
                SwingUtilities.invokeLater( new Runnable() {
                    @Override
                    public void run() {
                        contentAssist.hide();
                    }
                } );
            }

//            if ( popup != null)
//                popup.hide();

        }
    }
    /**
     * 
     * @param kawapad
     * @param point
     * @return
     *     true if the caret position was moved.
     */
    static boolean setCaretPositionIfNotInTheCurrentSelection( JTextComponent kawapad,  Point point ) {
        int p = kawapad.viewToModel( point );
        Caret caret = kawapad.getCaret();
        if ( caret.getDot() == caret.getMark() ||
            ( p < caret.getDot() && p < caret.getMark() ) ||  
            (  caret.getDot() < p && caret.getMark() < p )  
            ) 
        {
            kawapad.setCaretPosition( p );
            return true;
        } else {
            return false;
        }
    }
    
    final ActionEvent KAWAPAD_ACTION_EVENT = new ActionEvent( this, ActionEvent.ACTION_PERFORMED, null );
    private class KawapadMouseListener implements MouseListener {
        @Override
        public void mouseClicked(MouseEvent e) {
            if ( ( e.getButton() & MouseEvent.BUTTON1 ) != 0 ) {
                if ( e.getClickCount() == 2 ) {
                    logInfo("KawapadMouseListener:2");
                    e.consume();
//                LISPWORD_SELECT_CURRENT_ACTION.actionPerformed(KAWAPAD_ACTION_EVENT);
                } else if ( e.getClickCount() == 3 ) {
                    logInfo("KawapadMouseListener:3");
                    e.consume();
                    KawapadSelection.expandSelectedParentheses(kawapad);
                } else if ( e.getClickCount() == 4 ) {
                    logInfo("KawapadMouseListener:4");
                    e.consume();
                    KawapadSelection.expandSelectedParenthesesToTheOuterMost(kawapad);
                }
            } else if ( ( e.getButton() & MouseEvent.BUTTON2 ) != 0 ) {
                if ( e.getClickCount() == 1) {
                    e.consume();
                    if (setCaretPositionIfNotInTheCurrentSelection( kawapad, e.getPoint())) {
                        KawapadSelection.expandSelectedParentheses(kawapad);
                    }
                } else if (e.getClickCount() == 2 ) {
                    // Currently, this block does not work. The reason is unknown.
                    e.consume();
                    if (setCaretPositionIfNotInTheCurrentSelection( kawapad, e.getPoint())) {
                        KawapadSelection.expandSelectedParenthesesToTheOuterMost(kawapad);
                    }
                    if ( false ) {
                        String text = getTextDefault();
                        if ( text == null || text.trim().equals("") ) {
                            logInfo( "no text is selected" );
                        } else {
                            if ( ( e.getModifiers() & MouseEvent.CTRL_MASK) != 0 ) {
                                RUN_ACTION.actionPerformed( KAWAPAD_ACTION_EVENT );
                            } else {
                                EVALUATE_ACTION.actionPerformed( KAWAPAD_ACTION_EVENT );
                            }
                        }
                    }
                } else {
                    e.consume();
                }
            }
        }
        @Override
        public void mousePressed(MouseEvent e) {
        }
        @Override
        public void mouseReleased(MouseEvent e) {
        }
        @Override
        public void mouseEntered(MouseEvent e) {
        }
        @Override
        public void mouseExited(MouseEvent e) {
        }
    }
    private class KawapadMouseWheelListener implements MouseWheelListener {
        @Override
        public void mouseWheelMoved(MouseWheelEvent e) {
            if ((( e.getModifiers() == ( MouseWheelEvent.SHIFT_MASK  | MouseWheelEvent.ALT_MASK ))) && 
                ( e.getScrollType() == MouseWheelEvent.WHEEL_UNIT_SCROLL )) 
            {
//                setCaretPositionIfNotInTheCurrentSelection( kawapad, e.getPoint());                
                if ( e.getUnitsToScroll() < 0 ) { 
                    textualIncrement.TEXTUAL_DECREMENT_ACTION.actionPerformed( KAWAPAD_ACTION_EVENT ); 
                } else {
                    textualIncrement.TEXTUAL_INCREMENT_ACTION.actionPerformed( KAWAPAD_ACTION_EVENT ); 
                }
            } else if ((( e.getModifiers() == MouseWheelEvent.CTRL_MASK )) && 
                (e.getScrollType() == MouseWheelEvent.WHEEL_UNIT_SCROLL ) ) 
            {
                e.consume();
                if ( e.getUnitsToScroll() < 0 ) { 
                    PARENTHESIS_SELECT_LEFT_ACTION.actionPerformed( KAWAPAD_ACTION_EVENT );
//                    textualIncrement.TEXTUAL_DECREMENT_ACTION.actionPerformed(KAWAPAD_ACTION_EVENT); 
                } else {
                    PARENTHESIS_SELECT_RIGHT_ACTION.actionPerformed( KAWAPAD_ACTION_EVENT );
//                    textualIncrement.TEXTUAL_INCREMENT_ACTION.actionPerformed(KAWAPAD_ACTION_EVENT); 
                }
            } else if ((( e.getModifiers() == MouseWheelEvent.ALT_MASK ) ) && 
                (e.getScrollType() == MouseWheelEvent.WHEEL_UNIT_SCROLL ) )
            {
                e.consume();
                
                if ( e.getUnitsToScroll() < 0 ) {
                    PARENTHESIS_EXPAND_SELECTION_ACTION.actionPerformed( KAWAPAD_ACTION_EVENT );
                } else {
                    PARENTHESIS_SHRINK_SELECTION_DYNAMICALLY_ACTION.actionPerformed( KAWAPAD_ACTION_EVENT );
                }
                
            } else {
                kawapad.getParent().dispatchEvent(e);
            }
        }
    }
    private final KawapadDocumentListener kawapadDocumentListener = new KawapadDocumentListener();
    private final KawapadCaretListener kawapadCaretListener = new KawapadCaretListener();
    private final KawapadMouseListener kawapadMouseListener = new KawapadMouseListener();
    private final KawapadMouseWheelListener kawapadMouseWheelListener = new KawapadMouseWheelListener();
    {
//        MOVED TO EditorKit; see the constructor. (Sun, 29 Mar 2020 03:58:28 +0900) 
//        this.getDocument().addDocumentListener( kawapadListener );
        this.addCaretListener( kawapadCaretListener );
        this.addMouseListener( kawapadMouseListener );

        /*
         * MouseListener objects cannot override the default behavior.
         * It is necessary to re-order the listeners. (Tue, 28 Apr 2020 21:43:35 +0900)
         */
        MouseListenerWrapper.wrap( kawapad );
        MouseListenerWrapper.addMouseListenerFirst( kawapad, kawapadMouseListener );
        
        this.addMouseWheelListener(kawapadMouseWheelListener);
    }
    
    //////////////////////////////////////////////////////////////////////////////////////////


    /**
     * getSelectedText()
     * 
     * A method to get selected text which treats its endpoint as inclusive-like.
     * The endpoint of the current selection is to move one unit to the left.    
     * 
     * @param c
     * @return
     */
    static String getSelectedText( JTextComponent c ) {
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
//              caret.setDot( mark );
//              caret.moveDot( dot );
            return s;
        } else {
            if ( dot < mark ) {
//                  caret.setDot( mark + 1 );
//                  caret.moveDot( dot );
                return c.getSelectedText();
            } else {
////                    caret.setDot( dot );
//                  caret.moveDot( dot + 1 );
                return c.getSelectedText();
            }
        }
    }       
    
    String getTextDefault() {
        String schemeScript;
        {
            schemeScript = getSelectedText( this );
//            if ( schemeScript == null ) {
//                schemeScript =  this.getText();
//            }
        }
        return schemeScript;
    }

    //////////////////////////////////////////////////////////////////////////////////////////


    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action SELECT_EVALUATE_ACTION = new EvaluateAlternateAction( "kawapad-select-evaluate" );
    final class EvaluateAlternateAction extends EvaluateAction {
        public EvaluateAlternateAction(String name) {
            super( name );
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            JTextComponent textComponent  = getTextComponent( e );
            Caret caret = textComponent.getCaret();
            boolean goBackward=true;
            try {
                char c = textComponent.getDocument().getText( caret.getDot(), 1 ).charAt( 0 );
                if ( c == '(' ) {
                    goBackward = false;
                }
            } catch (BadLocationException e1) {
                e1.printStackTrace();
            }
            
            if ( goBackward )
                DEFAULT_BACKWARD_ACTION.actionPerformed( e );
            
            PARENTHESIS_EXPAND_SELECTION_ACTION.actionPerformed( e );
            
            // >>> (Wed, 13 Nov 2019 17:08:52 +0900) 
//            kawapad.getThreadManager().startScratchPadThread(
//                KawapadEvaluator.create( kawapad, getTextDefault(), getCurrentDirectory(), getCurrentFile(), true, false, false ) );
            evaluate( getTextDefault(), true, false, false );
            // <<< (Wed, 13 Nov 2019 17:08:52 +0900) 

        }
        {
            putValue( Action2.CAPTION, "Select and Evaluate" );
            putValue( Action.MNEMONIC_KEY, (int)'q' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl ENTER" );
        }
    }
    {
        AcceleratorKeyList.putAcceleratorKeyList( SELECT_EVALUATE_ACTION, "ctrl ENTER" );
    }

    public static final String KAWAPAD_EVALUATE = "kawapad-evaluate";

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action EVALUATE_REPLACE_ACTION = new EvaluateReplaceAction( KAWAPAD_EVALUATE );
    final class EvaluateReplaceAction extends TextAction2 {
        public EvaluateReplaceAction(String name) {
            super( name );
        }
        @Override
        public void actionPerformed(ActionEvent event) {
            String schemeScript;
            {
                schemeScript = getSelectedText();
                if ( schemeScript == null ) {
                    DEFAULT_BACKWARD_ACTION.actionPerformed( event );
                    PARENTHESIS_EXPAND_SELECTION_ACTION.actionPerformed( event );
                    SwingUtilities.invokeLater( new Runnable() {
                        @Override
                        public void run() {
                            String schemeScript2 = getSelectedText();
                            // >>> (Wed, 13 Nov 2019 17:08:52 +0900) 
                            // kawapad.getThreadManager().startScratchPadThread(
                            //    KawapadEvaluator.create( kawapad, schemeScript2, getCurrentDirectory(), getCurrentFile(), true, true, false ) );
                            evaluate( schemeScript2, true, true, false );
                            // <<< (Wed, 13 Nov 2019 17:08:52 +0900) 

                        }
                    });

                } else {
                    // >>> (Wed, 13 Nov 2019 17:08:52 +0900) 
                    // kawapad.getThreadManager().startScratchPadThread( 
                    //   KawapadEvaluator.create( kawapad, schemeScript, getCurrentDirectory(), getCurrentFile(), true, true, false ) );
                    evaluate( schemeScript, true, true, false  );
                    // <<< (Wed, 13 Nov 2019 17:08:52 +0900) 
                }
            }

        }
        {
            putValue( Action2.CAPTION, "Select, Evaluate and Replace" );
            putValue( Action.MNEMONIC_KEY, (int)'t' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl shift ENTER" );
        }
    }

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action EVALUATE_ACTION = new EvaluateAction( KAWAPAD_EVALUATE );
    class EvaluateAction extends TextAction2 {
        public EvaluateAction(String name) {
            super( name );
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            //  JOptionPane.showMessageDialog( JPulsarScratchPad.this, "", "AAAA" , JOptionPane.INFORMATION_MESSAGE  );
            // >>> (Wed, 13 Nov 2019 17:08:52 +0900) 
            // kawapad.getThreadManager().startScratchPadThread( KawapadEvaluator.create( kawapad, getTextDefault(), 
            //                          getCurrentDirectory(), getCurrentFile(), true, false, false ) );
            evaluate( getTextDefault(), true, false, false );
            // <<< (Wed, 13 Nov 2019 17:08:52 +0900) 
        }
        {
            putValue( Action2.CAPTION, "Evaluate" );
            putValue( Action.MNEMONIC_KEY, (int)'e' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl E" );
        }
    }

    public static final String KAWAPAD_RUN = "kawapad-run";


    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action RUN_ACTION = new RunAction( KAWAPAD_RUN );
    final class RunAction extends TextAction2 {
        public RunAction(String name) {
            super( name );
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            //  JOptionPane.showMessageDialog( JPulsarScratchPad.this, "", "AAAA" , JOptionPane.INFORMATION_MESSAGE  );
            
            // >>> (Wed, 13 Nov 2019 17:08:52 +0900) 
            // kawapad.getThreadManager().startScratchPadThread( KawapadEvaluator.create( kawapad, getTextDefault(), getCurrentDirectory(), 
            //     getCurrentFile(), false, false, false ) );
            evaluate( getTextDefault(), false, false, false  );
            // <<< (Wed, 13 Nov 2019 17:08:52 +0900) 
        }
        {
            putValue( Action2.CAPTION, "Run" );
            putValue( Action.MNEMONIC_KEY, (int)'r' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl R" );
        }
    }

    //////////////////////////////////////////////////////////////////////////////////////////

    public void insertText( String t ) {
//          boolean isThereSelection=true;
//          String text = textPane.getSelectedText();
//          if ( text == null ) {
//              text = textPane.getText();
//              isThereSelection = false;
//          }
//          isThereSelection = true;
//          
//          // ??? IS THIS NECESSARY?
//          textPane.getActionMap();
        SwingUtilities.invokeLater( new RunnableInsertTextToTextPane( kawapad, t, true, false ) );
    }
    
    private final class SetTextToTextPane implements Runnable {
        private final File file;
        private final String text;
        private SetTextToTextPane( File file, String text ) {
            this.file = file;
            this.text = text;
        }
        @Override
        public void run() {
            kawapad.setTextProc( file, text );
        }
    }
    public void setNewText( String t ) throws IOException {
        if ( ! kawapad.confirmSave( ConfirmType.OPEN_FILE ) ) {
            return;
        }
        SwingUtilities.invokeLater( new SetTextToTextPane(null, t) );
    }

    public static final String KAWAPAD_INTERRUPT = "kawapad-interrupt";

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action INTERRUPT_ACTION = new InterruptAction( KAWAPAD_INTERRUPT );
    private final class InterruptAction extends TextAction2 {
        public InterruptAction(String name) {
            super( name );
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            kawapad.getThreadManager().interruptAllThreads();
        }
        {
            putValue( Action2.CAPTION, "Interrupt" );
            putValue( Action.MNEMONIC_KEY , (int) 'i' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl K" );
        }
    }
    
    //////////////////////////////////////////////////////////////////////////////////////////
    //
    // Parenthesis
    //
    //////////////////////////////////////////////////////////////////////////////////////////
    static class ParenthesisAction extends TextAction2 {
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
            Kawapad textPane = (Kawapad) getTextComponent(e);
            Caret caret = textPane.getCaret();
            int currDot = caret.getDot();
            int newDot = KawapadSelection.lookupCorrespondingParenthesis2(
                KawapadSelection.getText( textPane.getDocument() ),
                currDot, 
                direction, 
                constantStrategy );
            if ( doSelect ) {
                caret.moveDot( newDot );
            } else {
                caret.setDot( newDot );
            }
        }
    }
    public static final String KAWAPAD_SIMPLE_PARENTHESIS_JUMP_LEFT  = "kawapad-simple-parenthesis-jump-left";
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action SIMPLE_PARENTHESIS_JUMP_LEFT_ACTION =
            new ParenthesisAction( KAWAPAD_SIMPLE_PARENTHESIS_JUMP_LEFT , false, -1, KawapadSelection.LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP )
    {
        {
            putValue( Action2.CAPTION, "Go to the Previous Parenthesis" );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl alt UP", "ctrl alt P" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
        }
    };
    
    public static final String KAWAPAD_SIMPLE_PARENTHESIS_JUMP_RIGHT  = "kawapad-simple-parenthesis-jump-right";

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action SIMPLE_PARENTHESIS_JUMP_RIGHT_ACTION =
            new ParenthesisAction( KAWAPAD_SIMPLE_PARENTHESIS_JUMP_RIGHT, false, +1, KawapadSelection.LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP  )
    {
        {
            putValue( Action2.CAPTION, "Go to the Next Parenthesis" );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl alt DOWN", "ctrl alt N" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
        }
    };
    
    public static final String KAWAPAD_SIMPLE_PARENTHESIS_JUMP_SELECT_LEFT="kawapad-simple-parenthesis-select-jump-left";
    // NOT USED (Mon, 09 Sep 2019 07:03:07 +0900)
    // INTEGRATED_ACTIONS_DEPRECATED (Wed, 11 Sep 2019 08:26:57 +0900)
    public final Action SIMPLE_PARENTHESIS_JUMP_SELECT_LEFT_ACTION =
            new ParenthesisAction( KAWAPAD_SIMPLE_PARENTHESIS_JUMP_SELECT_LEFT, true, -1, KawapadSelection.LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP )
    {
        {
            putValue( Action2.CAPTION, "Select the Previous Parenthesis" );
            putValue( Action.ACCELERATOR_KEY, AcceleratorKeyList.getKeyStroke(KeyEvent.VK_LEFT, KeyEvent.ALT_MASK |KeyEvent.SHIFT_MASK ) );
            // NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED
            AcceleratorKeyList.putAcceleratorKeyList( this, "alt shift LEFT" );
            // NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
        }
    };
    public static final String KAWAPAD_SIMPLE_PARENTHESIS_JUMP_SELECT_RIGHT = "kawapad-simple-parenthesis-select-jump-right";
    // NOT USED (Mon, 09 Sep 2019 07:03:07 +0900)
    // INTEGRATED_ACTIONS_DEPRECATED (Wed, 11 Sep 2019 08:26:57 +0900)
    public final Action SIMPLE_PARENTHESIS_JUMP_SELECT_RIGHT_ACTION =
            new ParenthesisAction( KAWAPAD_SIMPLE_PARENTHESIS_JUMP_SELECT_RIGHT, true, +1, KawapadSelection.LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP  )
    {
        {
            putValue( Action2.CAPTION, "Select the Next Parenthesis" );
            putValue( Action.ACCELERATOR_KEY, AcceleratorKeyList.getKeyStroke(KeyEvent.VK_RIGHT, KeyEvent.ALT_MASK | KeyEvent.SHIFT_MASK ) );
            // NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED
            AcceleratorKeyList.putAcceleratorKeyList( this, "alt shift RIGHT" );
            // NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
        }
    };
    
    public static final String KAWAPAD_PARENTHESIS_JUMP_LEFT = "kawapad-parenthesis-jump-left"; 
    // NOT USED (Mon, 09 Sep 2019 07:03:07 +0900)
    // INTEGRATED_ACTIONS_DEPRECATED (Wed, 11 Sep 2019 08:26:57 +0900)
    public final Action PARENTHESIS_JUMP_LEFT_ACTION =
            new ParenthesisAction( KAWAPAD_PARENTHESIS_JUMP_LEFT, false, -1, KawapadSelection.LCP2_STRATEGY_DYNAMIC )
    {
        {
            putValue( Action2.CAPTION, "Lookup the Corresponding Parenthesis on the Left" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
            // NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl alt LEFT" );
            // NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED

        }
    };
    public static final String KAWAPAD_PARENTHESIS_JUMP_RIGHT = "kawapad-parenthesis-jump-right"; 

    // NOT USED (Mon, 09 Sep 2019 07:03:07 +0900)
    // INTEGRATED_ACTIONS_DEPRECATED (Wed, 11 Sep 2019 08:26:57 +0900)
    public final Action PARENTHESIS_JUMP_RIGHT_ACTION = 
            new ParenthesisAction( KAWAPAD_PARENTHESIS_JUMP_RIGHT, false, +1, KawapadSelection.LCP2_STRATEGY_DYNAMIC  )
    {
        {
            putValue( Action2.CAPTION, "Lookup the Corresponding Parenthesis on the Right" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
            // NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl alt RIGHT" );
            // NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED
        }
    };
    
    public static final String KAWAPAD_PARENTHESIS_JUMP_SELECT_LEFT = "kawapad-parenthesis-sel-jump-left";
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action PARENTHESIS_JUMP_SELECT_LEFT_ACTION =
            new ParenthesisAction( KAWAPAD_PARENTHESIS_JUMP_SELECT_LEFT, true, -1, KawapadSelection.LCP2_STRATEGY_DYNAMIC  )
    {
        {
            putValue( Action2.CAPTION, "Lookup the Pair of Parenthesis on the Left and Select" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "alt shift UP", "alt shift P" );
        }
    };
    
    public static final String KAWAPAD_PARENTHESIS_JUMP_SELECT_RIGHT = "kawapad-parenthesis-sel-jump-right";

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action PARENTHESIS_JUMP_SELECT_RIGHT_ACTION =
            new ParenthesisAction( KAWAPAD_PARENTHESIS_JUMP_SELECT_RIGHT, true, +1, KawapadSelection.LCP2_STRATEGY_DYNAMIC  )
    {
        {
            putValue( Action2.CAPTION, "Lookup the Pair of Parenthesis on the Right and Select" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "alt shift DOWN", "alt shift N" );
        }
    };
    
    public static final String KAWAPAD_PARENTHESIS_JUMP = "kawapad-parenthesis-jump"; 
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action PARENTHESIS_JUMP_ACTION = new TextAction2( KAWAPAD_PARENTHESIS_JUMP ) 
    {
        @Override
        public void actionPerformed(ActionEvent e) {
            Kawapad textPane = (Kawapad) getTextComponent(e);
            Caret caret = textPane.getCaret();
            KawapadSelection.JUMP_TO_CORRESPONDING_PARENTHESIS.transform( getParenthesisStack(),
                KawapadSelection.getText( textPane.getDocument() ),
                caret );
        }
        {
            putValue( Action2.CAPTION, "Lookup the Corresponding Parenthesis on the Left" );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl J" );

        }
    };

    public static final String KAWAPAD_PARENTHESIS_SELECT_JUMP = "kawapad-parenthesis-select-jump"; 
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action PARENTHESIS_SELECT_JUMP_ACTION = new TextAction2( KAWAPAD_PARENTHESIS_SELECT_JUMP ) 
    {
        @Override
        public void actionPerformed(ActionEvent e) {
            Kawapad textPane = (Kawapad) getTextComponent(e);
            Caret caret = textPane.getCaret();
            KawapadSelection.JUMP_AND_SELECT_TO_CORRESPONDING_PARENTHESIS.transform( getParenthesisStack(),
                KawapadSelection.getText( textPane.getDocument() ),
                caret );
        }
        {
            putValue( Action2.CAPTION, "Lookup the Corresponding Parenthesis on the Left" );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl shift J" );

        }
    };

    
    public void resetHorzScrollPos() {
        if ( false ) {
            Container c = kawapad.getParent();
            if ( c instanceof JViewport && c.getParent() instanceof JScrollPane  ) {
                JScrollPane pane = ((JScrollPane)c.getParent());
                pane.getHorizontalScrollBar().setValue( 0 );
            }
        }
    }
    public void moveToSelection() {
        try {
            Rectangle dot = kawapad.modelToView( kawapad.getCaret().getDot() );
            Rectangle mark = kawapad.modelToView( kawapad.getCaret().getMark() );
            Rectangle r = new Rectangle( 
                Math.min( dot.x, mark.x ), 
                Math.min( dot.y, mark.y ), 
                Math.max( dot.x + dot.width, mark.x + mark.width ), 
                Math.max( dot.y + dot.height, mark.y + mark.height ) );
            r.width = r.width - r.x;
            r.height = r.height - r.y;
            SwingUtilities.invokeLater( new Runnable() {
                @Override
                public void run() {
                    kawapad.scrollRectToVisible( r );
                }
            });
//            logInfo( ""+ r  );
        } catch (BadLocationException e) {
            logError( "", e );
        }
    }

    public static final String KAWAPAD_LISPWORD_SELECT_CURRENT = "kawapad-lispword-select-current";

    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action LISPWORD_SELECT_CURRENT_ACTION = new TextAction2( KAWAPAD_LISPWORD_SELECT_CURRENT ) {
        CaretTransformer transformer = KawapadSelection.LISPWORD_SELECT_CURRENT_TRANSFORMER;
        @Override
        public void actionPerformed(ActionEvent e) {
            JTextComponent t = getTextComponent( e );
            Document document = t.getDocument();
            Caret caret = t.getCaret();
            transformer.transform( getParenthesisStack(), document, caret );
            moveToSelection();
        }
        {
            putValue( Action2.CAPTION, "Select the Word on the Cursor." );
            // AcceleratorKeyList.putAcceleratorKeyList( this, "alt UP", "alt P" );
            AcceleratorKeyList.putAcceleratorKeyList( this );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
        }
    };
    
    class LispWordSelectAction extends TextAction2 {
        CaretTransformer transformer;
        int direction;
        private LispWordSelectAction( String name, CaretTransformer transformer, int direction ) {
            super( name );
            this.transformer = transformer;
            this.direction = direction;
        }
        
        @Override
        public void actionPerformed(ActionEvent e) {
            JTextComponent t = getTextComponent( e );
            Caret caret = t.getCaret();
            Document document = t.getDocument();
            boolean b=false;
            try {
                if ( caret.getDot() != caret.getMark() ) {
                    b = false;
                } else {
                    char c = document.getText( caret.getDot(), 1 ).charAt( 0 );
                    b = ( Character.isAlphabetic( c ) || Character.isDigit( c ) );
                }
            } catch (BadLocationException e1) {
                logError( "safely ignored.", e1 );
            }
            if ( b ) {
                LISPWORD_SELECT_CURRENT_ACTION.actionPerformed( e );
                if ( direction < 0 ) {
                    if ( caret.getMark() < caret.getDot() ) {
                        swapCaretDirection(caret);
                    }
                } else if ( 0 < direction ) {
                    if ( caret.getDot() < caret.getMark() ) {
                        swapCaretDirection(caret);
                    }
                }
            } else {
                resetHorzScrollPos();
                transformer.transform( getParenthesisStack(), document, caret );
                moveToSelection();
            }
        }

        public void swapCaretDirection(Caret caret) {
            int mark = caret.getMark();
            int dot = caret.getDot();
            caret.setDot( dot );
            caret.moveDot( mark );
        }
    }

    
    public static final String KAWAPAD_LISPWORD_SELECT_RIGHT = "kawapad-lisp-word-select-right";

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action LISPWORD_SELECT_RIGHT_ACTION = 
        new LispWordSelectAction( KAWAPAD_LISPWORD_SELECT_RIGHT, KawapadSelection.LISPWORD_SELECT_RIGHT_TRANSFORMER, +1 )
    {
        {
            putValue( Action2.CAPTION, "Select the Word on the Cursor." );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl RIGHT", "alt F" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
        }
    };
    
    public static final String KAWAPAD_LISPWORD_SELECT_LEFT = "kawapad-lisp-word-select-left";

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action LISPWORD_SELECT_LEFT_ACTION = 
        new LispWordSelectAction( KAWAPAD_LISPWORD_SELECT_LEFT, KawapadSelection.LISPWORD_SELECT_LEFT_TRANSFORMER , -1 ) 
    {
        {
            putValue( Action2.CAPTION, "Select the Word on the Cursor." );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl LEFT", "alt B" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
        }
    };

    
    private class LispWordSwapAction extends TextAction2 {
        int direction;
        private LispWordSwapAction(String name, int direction ) {
            super( name );
            this.direction = direction;
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            JTextComponent t = getTextComponent( e );
            Caret caret = t.getCaret();
            Document document = t.getDocument();
            if ( caret.getDot() == caret.getMark() ) {
                LISPWORD_SELECT_CURRENT_ACTION.actionPerformed( e );
            } else {
                KawapadSelection.lispwordSwapWords( document, caret, direction );
            }
        }
    }
    public static final String KAWAPAD_LISPWORD_SWAP_LEFT = "kawapad-lisp-word-swap-left";
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action LISPWORD_SWAP_LEFT_ACTION = new LispWordSwapAction( KAWAPAD_LISPWORD_SWAP_LEFT, -1 ){
        {
            putValue( Action2.CAPTION, "Swap the Word on the Left." );
            AcceleratorKeyList.putAcceleratorKeyList( this  );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
        }
    };

    public static final String KAWAPAD_LISPWORD_SWAP_RIGHT = "kawapad-lisp-word-swap-right";
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action LISPWORD_SWAP_RIGHT_ACTION = new LispWordSwapAction( KAWAPAD_LISPWORD_SWAP_RIGHT, +1 ){
        {
            putValue( Action2.CAPTION, "Swap the Word on the Right." );
            AcceleratorKeyList.putAcceleratorKeyList( this  );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
        }
    };

    public static final String KAWAPAD_UNSELECT = "kawapad-unselect";
    
    // INTEGRATED_ACTIONS 
    @AutomatedActionField
    public final Action UNSELECT_ACTION = new TextAction2(KAWAPAD_UNSELECT) {
        @Override
        public void actionPerformed(ActionEvent e) {
            JTextComponent textComponent = getTextComponent( e );
            Caret caret = textComponent.getCaret();
            caret.setDot( caret.getDot() );
        }
        {
            putValue( Action2.CAPTION, "Unselect" );
//            AcceleratorKeyList.putAcceleratorKeyList( this, "ESCAPE" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
        }
    };


    public static final String KAWAPAD_ESCAPE = "kawapad-escape";
    
    // INTEGRATED_ACTIONS 
    @AutomatedActionField
    public final Action UNSELECT_ESCAPE = new TextAction2(KAWAPAD_ESCAPE) {
        @Override
        public void actionPerformed(ActionEvent e) {
            UNSELECT_ACTION.actionPerformed( e );
            DISABLE_CONTENT_ASSIST_ACTION.actionPerformed( e );
            CANCEL_SEARCH_BOX_ACTION.actionPerformed(e); // ADDED (Sat, 20 Jun 2020 09:09:34 +0900)
        }
        {
            putValue( Action2.CAPTION, "Unselect" );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ESCAPE" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
        }
    };

    //////////////////////////////////////////////////////////////////////////////////////////

    private class ParenthesisSwapAction extends TextAction2 {
        int direction;
        private ParenthesisSwapAction(String name, int direction ) {
            super( name );
            this.direction = direction;
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            JTextComponent t = getTextComponent( e );
            Caret caret = t.getCaret();
            Document document = t.getDocument();
            if ( caret.getDot() == caret.getMark() ) {
                PARENTHESIS_EXPAND_SELECTION_ACTION.actionPerformed( e );
            } else {
                KawapadSelection.parenthesisSwapWords( document, caret, direction );
            }
        }
    }
    public static final String KAWAPAD_PARENTHESIS_SWAP_LEFT = "kawapad-parenthesis-swap-left";
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action PARENTHESIS_SWAP_LEFT_ACTION = new ParenthesisSwapAction( KAWAPAD_PARENTHESIS_SWAP_LEFT, -1 ){
        {
            putValue( Action2.CAPTION, "Swap the Left Parentesis Pair" );
            AcceleratorKeyList.putAcceleratorKeyList( this  );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
        }
    };

    public static final String KAWAPAD_PARENTHESIS_SWAP_RIGHT = "kawapad-parenthesis-swap-right";
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action PARENTHESIS_SWAP_RIGHT_ACTION = new ParenthesisSwapAction( KAWAPAD_PARENTHESIS_SWAP_RIGHT, +1 ){
        {
            putValue( Action2.CAPTION, "Swap the Right Parentsis Pair" );
            AcceleratorKeyList.putAcceleratorKeyList( this );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
        }
    };
    
    
    //////////////////////////////////////////////////////////////////////////////////////////

    private class ParenthesisExtendSelectionAction extends TextAction2 {
        int direction;
        private ParenthesisExtendSelectionAction(String name, int direction ) {
            super( name );
            this.direction = direction;
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            JTextComponent t = getTextComponent( e );
            Caret caret = t.getCaret();
            Document document = t.getDocument();
            if ( caret.getDot() == caret.getMark() ) {
                PARENTHESIS_EXPAND_SELECTION_ACTION.actionPerformed( e );
                KawapadSelection.setCaretDirection( caret, direction );
            } else {
                KawapadSelection.parenthesisExtendSelection( document, caret, direction );
            }
        }
    }
    public static final String KAWAPAD_PARENTHESIS_EXTEND_SELECTION_LEFT = "kawapad-parenthesis-extend-left";
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action PARENTHESIS_EXTEND_SELECTION_LEFT_ACTION = new ParenthesisExtendSelectionAction( KAWAPAD_PARENTHESIS_EXTEND_SELECTION_LEFT, -1 ){
        {
            putValue( Action2.CAPTION, "Extend the Left Parentesis Pair" );
            AcceleratorKeyList.putAcceleratorKeyList( this, "alt shift LEFT", "alt shift B" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
        }
    };

    public static final String KAWAPAD_PARENTHESIS_EXTEND_SELECTION_RIGHT = "kawapad-parenthesis-extend-right";
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action PARENTHESIS_EXTEND_SELECTION_RIGHT_ACTION = new ParenthesisExtendSelectionAction( KAWAPAD_PARENTHESIS_EXTEND_SELECTION_RIGHT, +1 ){
        {
            putValue( Action2.CAPTION, "Extend the Right Parentsis Pair" );
            AcceleratorKeyList.putAcceleratorKeyList( this, "alt shift RIGHT", "alt shift F" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
        }
    };

    //////////////////////////////////////////////////////////////////////////////////////////
    
    private class LispWordExtendSelectionAction extends TextAction2 {
        int direction;
        private LispWordExtendSelectionAction(String name, int direction ) {
            super( name );
            this.direction = direction;
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            JTextComponent t = getTextComponent( e );
            Caret caret = t.getCaret();
            Document document = t.getDocument();
            if ( caret.getDot() == caret.getMark() ) {
                LISPWORD_SELECT_CURRENT_ACTION.actionPerformed( e );
            } else {
                KawapadSelection.lispwordExtendSelection( document, caret, direction );
            }
        }
    }
    public static final String KAWAPAD_LISPWORD_EXTEND_SELECTION_LEFT = "kawapad-lisp-word-extend-selection-left";
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action LISPWORD_EXTEND_SELECTION_LEFT_ACTION = new LispWordExtendSelectionAction( KAWAPAD_LISPWORD_EXTEND_SELECTION_LEFT, -1 ){
        {
            putValue( Action2.CAPTION, "Extend the Word on the Left." );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl shift LEFT", "alt shift B" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
        }
    };

    public static final String KAWAPAD_LISPWORD_EXTEND_SELECTION_RIGHT = "kawapad-lisp-word-extend-selection-right";
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action LISPWORD_EXTEND_SELECTION_RIGHT_ACTION = new LispWordExtendSelectionAction( KAWAPAD_LISPWORD_EXTEND_SELECTION_RIGHT, +1 ){
        {
            putValue( Action2.CAPTION, "Extend the Word on the Right." );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl shift RIGHT", "alt shift F" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
        }
    };
    
    
    //////////////////////////////////////////////////////////////////////////////////////////
    
    
    
    
    //////////////////////////////////////////////////////////////////////////////////////////
    //
    // Parenthesis Action 2
    //
    //////////////////////////////////////////////////////////////////////////////////////////
    
    private final KawapadParenthesisStack parenthesisStack = new KawapadParenthesisStack();
    public KawapadParenthesisStack getParenthesisStack() {
        return parenthesisStack;
    }
    
    class ParenthesisExpandSelectionAction extends TextAction2 {
        ExpandParenthesisSelector transformer = new ExpandParenthesisSelector();
        ParenthesisExpandSelectionAction(String name) {
            super(name);
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            JTextComponent c = getTextComponent(e);
            transformer.transform( getParenthesisStack(), c.getDocument(), c.getCaret() );
            moveToSelection();
//            SchemeParentheses.expandSelectedParentheses( kawapad );
        }
    }
    public static final String KAWAPAD_PARENTHESIS_EXPAND_SELECTION= "kawapad-parenthesis-expand-selection";
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action PARENTHESIS_EXPAND_SELECTION_ACTION = new ParenthesisExpandSelectionAction( KAWAPAD_PARENTHESIS_EXPAND_SELECTION ) {
        {
            putValue( Action2.CAPTION, "Select Inside the Current Parentheses" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "alt UP", "alt P" );
        }
    };
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    // NOT USED (Mon, 09 Sep 2019 06:53:32 +0900)
    class OldParenthesisExpandSelectionAction extends TextAction2 {
        OldParenthesisExpandSelectionAction(String name) {
            super(name);
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            JTextComponent textComponent = (JTextComponent) getTextComponent(e);
            String text  = textComponent.getText();
            Caret caret  = textComponent.getCaret();
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
                posL =  WordJumpAction.lookup( leftPos,  text , -1 );
                posR = WordJumpAction.lookup( rightPos, text ,  1 );
                
                if ( 0<=posL && 0<=posR ) {
                    synchronized ( getParenthesisStack() ) {
                        try {
                            getParenthesisStack().setLocked( true );
                            caret.setDot(posL);
                            caret.moveDot(posR);
                            getParenthesisStack().push(currMark, currDot);
                            return;
                        } finally {
                            getParenthesisStack().setLocked( false );
                        }
                    }
                }
            }
        }
    }
    
    public static final String KAWAPAD_OLD_PARENTHESIS_EXPAND_SELECTION = "kawapad-old-parenthesis-expand-selection-action";
    
    // NOT USED (Mon, 09 Sep 2019 06:53:32 +0900)
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    public final Action OLD_PARENTHESIS_EXPAND_SELECTION_ACTION = new OldParenthesisExpandSelectionAction( KAWAPAD_OLD_PARENTHESIS_EXPAND_SELECTION ) {
        {
            putValue( Action2.CAPTION, "Deselect Inside the Current Parentheses" );
            putValue( Action.ACCELERATOR_KEY, AcceleratorKeyList.getKeyStroke( KeyEvent.VK_UP, KeyEvent.CTRL_MASK | KeyEvent.ALT_MASK ) );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
//          NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED 
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl alt UP" );
//          NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED 
        }
    };

    
    class ParenthesisSelectSideAction extends TextAction2 {
        int direction;
        CaretTransformer caretTransformer;
        ParenthesisSelectSideAction(String name, int direction ) {
            super(name);
            this.direction = direction;
            this.caretTransformer = new SideParenthesisSelector( direction );
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            resetHorzScrollPos();
            JTextComponent textComponent = (JTextComponent) getTextComponent(e);
            Caret caret  = textComponent.getCaret();
            int dot = caret.getDot();
            int mark = caret.getMark();
            if ( dot == mark ) {
//                PARENTHESIS_SELECT_ACTION.actionPerformed( e );
                // DO SAME WITH THE FOLLOWING (Sun, 08 Sep 2019 01:55:30 +0900)
                boolean result;
                result = caretTransformer.transform( 
                    getParenthesisStack(), 
                    KawapadSelection.getText( textComponent.getDocument() ), 
                    textComponent.getCaret() );
                if ( ! result ) {
                    PARENTHESIS_EXPAND_SELECTION_ACTION.actionPerformed( e );
                }
            } else {
                // reverse the direction of the selection.
                if ( this.direction < 0 && mark < dot ) {
                    caret.setDot( dot );
                    caret.moveDot( mark );
                } else if ( 0 < this.direction && dot < mark ) {
                    caret.setDot( dot );
                    caret.moveDot( mark );
                } else {
                    boolean result;
                    result = caretTransformer.transform( 
                                getParenthesisStack(), 
                                KawapadSelection.getText( textComponent.getDocument() ), 
                                textComponent.getCaret() );
                    if ( ! result ) {
                        PARENTHESIS_EXPAND_SELECTION_ACTION.actionPerformed( e );
                    }
                }
            }
            moveToSelection();
        }
    }

    public static final String KAWAPAD_SELECT_LEFT_PARENTHESES = "kawapad-select-left-parentheses";
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action PARENTHESIS_SELECT_LEFT_ACTION = new ParenthesisSelectSideAction( KAWAPAD_SELECT_LEFT_PARENTHESES, -1 ) {
        {
            putValue( Action2.CAPTION, "Select the Parentheses on the Left Side" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "alt LEFT", "alt B" );
        }
    };

    public static final String KAWAPAD_SELECT_RIGHT_PARENTHESES = "kawapad-select-right-parentheses";

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action PARENTHESIS_SELECT_RIGHT_ACTION = new ParenthesisSelectSideAction(KAWAPAD_SELECT_RIGHT_PARENTHESES,+1) {
        {
            putValue( Action2.CAPTION, "Select the Parentheses on the Left Side" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "alt RIGHT", "alt F");
        }
    };

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    class ParenthesisShrinkSelectionBySearchAction extends TextAction2 {
        ShrinkParenthesisSelector selector = new ShrinkParenthesisSelector();
        ParenthesisShrinkSelectionBySearchAction(String name) {
            super(name);
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            JTextComponent textComponent = (JTextComponent) getTextComponent(e);
            Document document = textComponent.getDocument();
            Caret caret  = textComponent.getCaret();
            int currDot  = caret.getDot();
            int currMark = caret.getMark();
            if ( currDot == currMark ) {
                // This sequence effectively select the lower pair of parenthesis.
                PARENTHESIS_EXPAND_SELECTION_ACTION.actionPerformed( e );
                boolean expansionSucceeded = true;
                if ( caret.getDot() == caret.getMark() ) {
                    expansionSucceeded = false;
                }
                if ( expansionSucceeded )
                    PARENTHESIS_SHRINK_SELECTION_BY_SEARCH_ACTION.actionPerformed( e );
                return;
            }
            selector.transform( getParenthesisStack(), document, caret );
//            SchemeParentheses.shrinkSelection( 
//                getParenthesisStack(),
//                SchemeParentheses.getText( textComponent.getDocument() ),
//                caret );
        }
    }
    
    public static final String KAWAPAD_PARENTHESIS_SHRINK_SELECTION = "kawapad-select-parentheses-shrink";
    
    // THIS IS *USED* BUT NOT REGISTERED DIRECTLY AS AN ACCELERATOR (Thu, 19 Sep 2019 07:43:28 +0900)
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    public final Action PARENTHESIS_SHRINK_SELECTION_BY_SEARCH_ACTION = 
        new ParenthesisShrinkSelectionBySearchAction( KAWAPAD_PARENTHESIS_SHRINK_SELECTION)
    {
        {
            putValue( Action2.CAPTION, "Select Parentheses Inside the Current Selection" );
            putValue( Action.ACCELERATOR_KEY, AcceleratorKeyList.getKeyStroke( KeyEvent.VK_DOWN, KeyEvent.SHIFT_MASK | KeyEvent.ALT_MASK ) );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
//          NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED 
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl alt DOWN", "ctrl alt N");
//          NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED 
        }
    };
    

    class ParenthesisShrinkSelectionByStackAction extends TextAction2 {
        ParenthesisShrinkSelectionByStackAction(String name) {
            super(name);
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            JTextComponent textComponent = (JTextComponent) getTextComponent(e);
            synchronized ( getParenthesisStack() ) {
                try {
                    getParenthesisStack().setLocked( true );
                    if ( textComponent.getSelectedText() != null ) {
                        if ( ! getParenthesisStack().isEmpty() ) {
                            KawapadParenthesisStack.Element elem = getParenthesisStack().pop();
                            Caret caret = textComponent.getCaret();
                            caret.setDot( elem.mark );
                            caret.moveDot( elem.dot );
                        } else {
                            PARENTHESIS_SHRINK_SELECTION_BY_SEARCH_ACTION.actionPerformed( e );
                        }
                    } else {
                        getParenthesisStack().clear();
                    }
                } finally {
                    getParenthesisStack().setLocked( false );
                }
            }
        }
    }
    
    public static final String KAWAPAD_PARENTHESIS_SHRINK_SELECTION_BY_STACK = "kawapad-parenthesis-deselect";

    // THIS IS *USED* BUT NOT REGISTERED DIRECTLY AS AN ACCELERATOR (Thu, 19 Sep 2019 07:43:28 +0900)
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
//    @AutomatedActionField
    public final Action PARENTHESIS_SHRINK_SELECTION_BY_STACK_ACTION = new ParenthesisShrinkSelectionByStackAction( KAWAPAD_PARENTHESIS_SHRINK_SELECTION_BY_STACK ) {
        {
            putValue( Action2.CAPTION, "Deselect Inside the Current Parentheses" );
            putValue( Action.ACCELERATOR_KEY, AcceleratorKeyList.getKeyStroke( KeyEvent.VK_DOWN, KeyEvent.ALT_MASK | KeyEvent.SHIFT_MASK ) );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
//              NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED 
            AcceleratorKeyList.putAcceleratorKeyList( this, "alt shift DOWN" );
//              NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED NOT USED 
        }
    };


    class ParenthesisShrinkSelectionDynamically extends TextAction2 {
        ParenthesisShrinkSelectionDynamically(String name) {
            super(name);
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            JTextComponent textComponent = (JTextComponent) getTextComponent(e);
            synchronized ( getParenthesisStack() ) {
                if ( ! getParenthesisStack().isEmpty() ) {
                    try {
                        getParenthesisStack().setLocked( true );
                        
                        KawapadParenthesisStack.Element elem = getParenthesisStack().peek();
                        int stackL = Math.min( elem.dot, elem.mark );
                        int stackR = Math.max( elem.dot, elem.mark );
                        
                        Caret caret = textComponent.getCaret();
                        int caretL = Math.min( caret.getDot(), caret.getMark() );
                        int caretR = Math.max( caret.getDot(), caret.getMark() );
                        
                        logInfo( String.format( "ParenthesisShrinkSelectionDynamically: caret %d=>%d stack %d=>%d", caretL, caretR, stackL, stackR  ) );
                        // Check if the range which is denoted by the current stack element 
                        // is inside the range which is denoted by the current caret.
                        //           |<=L  stack span R=>|  
                        // |<=L     selection span             R=>|  
                        if ( (caretL <= stackL ) && (caretL <= stackR ) && 
                             (stackL <= caretR ) && (stackR <= caretR ))
                        {
                            PARENTHESIS_SHRINK_SELECTION_BY_STACK_ACTION.actionPerformed( e );
                        } else {
                            PARENTHESIS_SHRINK_SELECTION_BY_SEARCH_ACTION.actionPerformed( e );
                        }
                    } finally {
                        getParenthesisStack().setLocked( false );
                    }
                } else {
                    PARENTHESIS_SHRINK_SELECTION_BY_SEARCH_ACTION.actionPerformed( e );
                }
            }
        }
    }

    public static final String KAWAPAD_PARENTHESIS_SHRINK_SELECTION_DYNAMICALLY = "kawapad-parenthesis-shrink-selection-dynamically";

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action PARENTHESIS_SHRINK_SELECTION_DYNAMICALLY_ACTION = 
            new ParenthesisShrinkSelectionDynamically( KAWAPAD_PARENTHESIS_SHRINK_SELECTION_DYNAMICALLY )
    {
        {
            putValue( Action2.CAPTION, "Deselect Inside the Current Parentheses" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "alt DOWN", "alt N" );
        }
    };

    //////////////////////////////////////////////////////////////////////////////////////////
    //
    // Formatter
    //
    //////////////////////////////////////////////////////////////////////////////////////////

    abstract class TextFilter {
        abstract String process( String text );
    }

    void formatProc(JTextComponent textPane, TextFilter filter ) {
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
            beginIndex = SchemeIndentChanger.lookupLineStart(text, min  );
            endIndex = SchemeIndentChanger.lookupLineEnd(text, max );
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
                endIndex = SchemeIndentChanger.lookupLineEnd(text, min+1 );
                postfix = "\n";
            } else {
                beginIndex = SchemeIndentChanger.lookupLineStart(text, min  );
                endIndex   = SchemeIndentChanger.lookupLineEnd(text, max );
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
            int spaces = SchemeIndentChanger.countFirstSpaces( textPane.getText().substring( beginIndex ) );
            textPane.setCaretPosition(  beginIndex + spaces );
//              textPane.moveCaretPosition( beginIndex + spaces );
        }
    }

    public static final String KAWAPAD_FORMAT = "kawapad-format";

    private class FormatAction extends TextAction2 {
        int difference;
        public FormatAction(String name, int difference) {
            super(name);
            this.difference = difference;
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            //  JOptionPane.showMessageDialog( JPulsarScratchPad.this, "", "AAAA" , JOptionPane.INFORMATION_MESSAGE  );
            try {
                kawapad.getUndoManager().startGroup();
                kawapad.getUndoManager().setSuspended(true);
                formatProc( kawapad, new TextFilter() {
                    @Override
                    String process(String text) {
                        return SchemeIndentChanger.changeIndentRelativeMultiline( text, difference );
                    }
                });
            } finally {
                kawapad.getUndoManager().setSuspended(false);
                kawapad.getUndoManager().endGroup();

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

    public static final String KAWAPAD_SHIFT_INDENT_RIGHT = "kawapad-shift-indent-right";

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action SHIFT_INDENT_RIGHT_ACTION = new FormatAction( KAWAPAD_SHIFT_INDENT_RIGHT,  +2 ) {
        {
            putValue( Action2.CAPTION, "Shift Left" );
            putValue( Action.MNEMONIC_KEY , (int) 'c' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "TAB" );
        }
    };

    public static final String KAWAPAD_SHIFT_INDENT_LEFT = "kawapad-shift-indent-left";

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action SHIFT_INDENT_LEFT_ACTION = new FormatAction( KAWAPAD_SHIFT_INDENT_LEFT, -2 ) {
        {
            putValue( Action2.CAPTION, "Shift Right" );
            putValue( Action.MNEMONIC_KEY , (int) 'd' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "shift TAB" );
        }
    };
    
    private class IndentationCorrectorAction extends TextAction2 {
        public IndentationCorrectorAction(String name) {
            super( name );
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            //  JOptionPane.showMessageDialog( JPulsarScratchPad.this, "", "AAAA" , JOptionPane.INFORMATION_MESSAGE  );
            formatProc( kawapad, new TextFilter() {
                @Override
                String process(String text) {
                    return correctIndentation( kawapad, text );
                }
            });
        }
    }
    
    public static final String correctIndentation( Kawapad kawapad, String text ) {
        return SchemeIndentationCorrector.correctIndentation( kawapad.getLispWordChecker(), text );
    }

    public static final String KAWAPAD_INDENTATION_CORRECTOR = "kawapad-indentation-corrector";

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action INDENTATION_CORRECTOR_ACTION = new IndentationCorrectorAction( KAWAPAD_INDENTATION_CORRECTOR ) {
        {
            putValue( Action2.CAPTION, "Correct Indentation" );
            putValue( Action.MNEMONIC_KEY , (int) 'i' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl I" );
        }
    };

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private class SurroundByParenthesesAction extends TextAction2 {
        public SurroundByParenthesesAction(String name) {
            super( name );
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            Kawapad kawapad = (Kawapad) getTextComponent( e );
            Caret caret = kawapad.getCaret();
            int dot = caret.getDot();
            int mark = caret.getMark();
            Document document = kawapad.getDocument();
            if ( dot == mark ) { 
                //
            } else {
                try {
                    if ( dot < mark ) {
                        document.insertString( mark, ")" , null );
                        document.insertString( dot,  "(" , null );
                    } else {
                        document.insertString( dot,  ")" , null );
                        document.insertString( mark, "(" , null );
                    }
                } catch (BadLocationException e1) {
                    e1.printStackTrace();
                }
            }
        }
    }

    public static final String KAWAPAD_SURROUND_BY_PARENTHESES = "kawapad-surround-by-parentheses";

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action SURROUND_BY_PARENTHESES_ACTION = new SurroundByParenthesesAction( KAWAPAD_SURROUND_BY_PARENTHESES ) {
        {
            putValue( Action2.CAPTION, "Surround by Parentheses" );
            putValue( Action.MNEMONIC_KEY , (int) 'o' );
            AcceleratorKeyList.putAcceleratorKeyList( this );
        }
    };

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private class OpenCloseParenthesis extends TextAction2 {
        private char objectParenthesis;
        private char correspondingParenthesis;
        public OpenCloseParenthesis(String name, char objectParenthesis, char correspondingParenthesis ) {
            super( name );
            this.objectParenthesis = objectParenthesis;
            this.correspondingParenthesis = correspondingParenthesis;
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            Kawapad kawapad = (Kawapad) getTextComponent( e );
            Caret caret = kawapad.getCaret();
            int dot = caret.getDot();
            int mark = caret.getMark();
            Document document = kawapad.getDocument();
            try {
                if ( dot == mark ) { 
                    char c = document.getText( dot, 1 ).charAt( 0 );
                    if ( c == objectParenthesis ) {
                        Segment text = KawapadSelection.getText( document );
                        int pos = KawapadSelection.lookupCorrespondingParenthesis1( text, dot );
                        if ( pos < 0 ) {
                            
                        } else {
                            if ( dot < pos ) {
                                document.insertString( pos, String.valueOf( correspondingParenthesis ) , null );
                                document.insertString( dot, String.valueOf( objectParenthesis        ) , null );
                            } else {
                                document.insertString( dot, String.valueOf( objectParenthesis        ) , null );
                                document.insertString( pos, String.valueOf( correspondingParenthesis ) , null );
                            }
                        }
                    } else if ( c == correspondingParenthesis ) {
//                        String insertText = e.getActionCommand();
                        document.insertString( dot, "()", null );
                        caret.setDot( dot + 1 ); 
                    } else {
//                        String insertText = e.getActionCommand();
                        document.insertString( dot, "()", null );
                        caret.setDot( dot + 1 ); 
                    }
                } else {
                    SURROUND_BY_PARENTHESES_ACTION.actionPerformed( e );
                }
            } catch (BadLocationException e1) {
                logError( "", e1 );
            }
        }
    }
    public static final String KAWAPAD_OPEN_PARENTHESIS = "kawapad-open-parenthesis";

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action OPEN_PARENTHESIS_ACTION = new OpenCloseParenthesis( KAWAPAD_OPEN_PARENTHESIS, '(',')') {
        {
            putValue( Action2.CAPTION, "Open Parenthesis" );
            putValue( Action.MNEMONIC_KEY , (int) 'o' );
            AcceleratorKeyList.putAcceleratorKeyList( this );
        }
    };

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////

   
    public static final String KAWAPAD_CLOSE_PARENTHESIS = "kawapad-close-parenthesis";

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action CLOSE_PARENTHESIS_ACTION = new OpenCloseParenthesis( KAWAPAD_CLOSE_PARENTHESIS, ')', '(') {
        {
            putValue( Action2.CAPTION, "Close Parenthesis" );
            putValue( Action.MNEMONIC_KEY , (int) 'c' );
            AcceleratorKeyList.putAcceleratorKeyList( this );
        }
    };
    
    
    class DeleteCharAction extends TextAction2 {
        Action defaultAction;
        int offset;
        DeleteCharAction(String name, Action defaultAction, int offset ) {
            super( name );
            this.defaultAction = defaultAction;
            this.offset = offset;
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            Kawapad kawapad = (Kawapad) getTextComponent( e );
            Caret caret = kawapad.getCaret();
            int dot = caret.getDot();
            int mark = caret.getMark();
            Document document = kawapad.getDocument();
            if ( dot != mark ) {
                defaultAction.actionPerformed( e );
            } else {
                try {
                    int dotWithOffset = dot + offset;
                    char c = document.getText( dotWithOffset, 1 ).charAt( 0 );
                    if ( c == '(' || c == ')' ) {
                        Segment text = KawapadSelection.getText( document );
                        int pos = KawapadSelection.lookupCorrespondingParenthesis1( text, dotWithOffset );
                        if ( pos < 0 ) {
                            
                        } else {
                            if ( dotWithOffset < pos ) {
                                document.remove( pos, 1 );
                                document.remove( dotWithOffset , 1 );
                                if ( offset != 0 ) 
                                    caret.setDot( dotWithOffset );
                            } else {
                                document.remove( dotWithOffset, 1 );
                                document.remove( pos, 1 );
                                if ( offset != 0 ) 
                                    caret.setDot( dotWithOffset - 1);
                            }
                        }
                    } else {
                        defaultAction.actionPerformed( e );
                    }
                } catch (BadLocationException e1) {
                    logError( "", e1 );
                }
            }
        }
    };
    
    @AutomatedActionField
    public final Action DELETE_NEXT_CHAR = new DeleteCharAction( DefaultEditorKit.deleteNextCharAction, DEFAULT_DELETE_NEXT_CHAR,  0) {
        {
            putValue( Action2.CAPTION, "Delete with the Corresponding Parenthesis" );
            putValue( Action.MNEMONIC_KEY , (int) 'c' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "DELETE" );
        }
    };
    

    @AutomatedActionField
    public final Action DELETE_PREV_CHAR = new DeleteCharAction( DefaultEditorKit.deletePrevCharAction, DEFAULT_DELETE_PREV_CHAR, -1) {
        {
            putValue( Action2.CAPTION, "Delete Previous with the Corresponding Parenthesis" );
            putValue( Action.MNEMONIC_KEY , (int) 'c' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "BACK_SPACE" );
        }
    };

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private class SearchTextAction extends TextAction2 {
        int direction;
        boolean wordSearch;
        private SearchTextAction(String name, int direction, boolean wordSearch ) {
            super( name );
            this.direction = direction;
            this.wordSearch = wordSearch;
        }
        
        @Override
        public void actionPerformed(ActionEvent e) {
            JTextComponent c = getTextComponent( e );
            Document document = c.getDocument();
            Segment text = KawapadSelection.getText( document );
            Caret caret = c.getCaret();
            
            if ( searchBox.isVisible() ) {
                String word = searchBox.getText();
                SearchNextWordTransformer transformer = 
                    new SearchNextWordTransformer( word, wordSearch, direction );
                transformer.transform( getParenthesisStack(), text, caret );
            } else {
                
                if ( caret.getDot() == caret.getMark() ) {
                    KawapadSelection.LISPWORD_SELECT_CURRENT_TRANSFORMER.transform( getParenthesisStack(), text, caret );
                    return;
                } else {
                    String word = KawapadTemporarySearchHighlighter.getCurrentWord( text, caret );
                    if ( word == null ) {
                        return;
                    } else {
                        SearchNextWordTransformer transformer = 
                            new SearchNextWordTransformer( word, wordSearch, direction );
                        transformer.transform( getParenthesisStack(), text, caret );
                    }
                }
            }
        }
    }

    public static final String KAWAPAD_SEARCH_NEXT = "kawapad-search-next";
    
    // INTEGRATED_ACTIONS 
    @AutomatedActionField
    public final Action SEARCH_NEXT_ACTION = new SearchTextAction( KAWAPAD_SEARCH_NEXT, +1, true ) {
        {
            putValue( Action2.CAPTION, "Unselect" );
            AcceleratorKeyList.putAcceleratorKeyList( this, "control DOWN",  "control 8" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
        }
    };
    
    public static final String KAWAPAD_SEARCH_PREV = "kawapad-search-prev";
    
    // INTEGRATED_ACTIONS 
    @AutomatedActionField
    public final Action SEARCH_PREV_ACTION = new SearchTextAction( KAWAPAD_SEARCH_PREV, -1, true  ) {
        {
            putValue( Action2.CAPTION, "Unselect" );
            AcceleratorKeyList.putAcceleratorKeyList( this, "control UP", "control 3" );
//              putValue( Action.MNEMONIC_KEY , (int) 'd' );
        }
    };



    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 
    // Defining an interface the for scheme interpreter. 
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    //////////////////////////////////////////////////////////////////////////////////////////
    // 
    // Init 
    // 
    //////////////////////////////////////////////////////////////////////////////////////////

    public static File getExtFile() {
        return new File( System.getProperty("user.home"), ".kawapad/kawapad-extension.scm" );
    }
    public static File getInitFile() {
        return new File( System.getProperty("user.home"), ".kawapad/kawapad-initialization.scm" );
    }

    /**
     * This file is executed only once when Kawapad class is loaded to the current VM.
     * This file is executed before Kawapad is initialized; therefore, in this file 
     * the most Kawapad API is not available because at the time of execution, 
     * Kawapad is not initialized yet. This can only be used for initializing various 
     * classes. 
     */
    static {
        SchemeEvaluatorUtils.executeStatic( null, "kawapad initialization",  getInitFile() );
    }
    
    public static class ConsoleObject {
        static String formatLogger(Object[] args) {
            return String.join( " ", SchemeValues.toStringList( Arrays.asList( args ) ) );
        }
        public void info( Object ... args ){
            SimpleConsole.getConsole().addText( formatLogger( args ) );
        }
        public void warn( Object ... args ){
            SimpleConsole.getConsole().addText( formatLogger( args ) );
        }
        public void error( Object ... args ){
            SimpleConsole.getConsole().addText( formatLogger( args ) );
        }
        public void clear(){
            SimpleConsole.getConsole().clearText();
        }
        public void setText( Object ... args ){
            SimpleConsole.getConsole().setText( formatLogger( args ) );
        }
    }
    public final ConsoleObject console = new ConsoleObject(); 

    ////////////////////////////////////////////////////////////////////////////
    public static Font loadFont(String filePath, float fontSize) throws FontFormatException, IOException {
        Font font = Font.createFont(Font.TRUETYPE_FONT, new File( filePath )).deriveFont( fontSize );
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        ge.registerFont(font);
        return font;
    }
    
    /**
     * <h3>Example1</h3>
     * <pre>
     *  (kawapad.Kawapad:setUIFont 
     *     (java.awt.Font "monospaced" java.awt.Font:PLAIN 12 )))
     * </pre>
     * <h3>Example2</h3>
     * <pre>
     *   (if #f
     *     (kawapad.Kawapad:setUIFont 
     *       (kawapad.Kawapad:loadFont 
     *         "~/Documents/fonts/Datalook Regular.ttf" 20 )))
     * </pre>
     * 
     * @param f
     *    A Font object to set.
     */
    public static void setUIFont( Font f ){
        setUIFont( new javax.swing.plaf.FontUIResource( f ) );
    }

    // https://stackoverflow.com/questions/7434845/setting-the-default-font-of-swing-program
    public static void setUIFont( javax.swing.plaf.FontUIResource f ){
        java.util.Enumeration keys = javax.swing.UIManager.getDefaults().keys();
        while (keys.hasMoreElements()) {
            Object key = keys.nextElement();
            Object value = javax.swing.UIManager.get(key);
            if (value instanceof javax.swing.plaf.FontUIResource)
                javax.swing.UIManager.put (key, f);
        }
    } 

    ////////////////////////////////////////////////////////////////////////////
    
    
    
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 
    // TextualIncrement ( TEXTUAL_INCREMENT )
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private final KawapadTextualIncrement textualIncrement = new KawapadTextualIncrement();
    /**
     * Returns the current TextualIncrement manager.
     * <pre> 
     *    (kawapad:textual-increment:add-incremental-symbol "" "" )
     * </pre> 
     * 
     * See
     * <ul>
     * <li>{@link KawapadTextualIncrement#addIncrementalSymbol(String, String) }</li>
     * <li>{@link KawapadTextualIncrement#deleteIncrementalSymbol(String)}</li>
     * <li> {@link KawapadTextualIncrement#clearIncrementalSymbol</li>
     * </ul>
     * 
     * @return
     *   returns the current TextualIncrement manager.
     */
    public KawapadTextualIncrement getTextualIncrement() {
        return textualIncrement;
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 
    //  File Management
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////

    boolean fileModified = false;
    File currentFile = null;

    /**
     * Returns the path to the currently editting file  
     * @return
     *    the File object which contains the path to the currently editting file.
     */
    public File getCurrentFile() {
        return currentFile;
    }
    
    /**
     * Returns the path to the working directory for the current project.
     * The working directory is always located to the same directory with the current editting file.
     * @return
     *    the path to the current working directory for the current project. 
     */
    public File getCurrentDirectory() {
        if ( currentFile == null ) {
            try {
                return new File(".").getAbsoluteFile().getCanonicalFile();
            } catch (IOException e) {
                throw new InternalError(e);
            }
        } else {
            return currentFile.getParentFile();
        }
    }

    // ADDED (Fri, 05 Jun 2020 02:09:15 +0900)
    public File resolveFile( File file ) {
        if ( ! file.isAbsolute()  ) {
            file = new File( getCurrentDirectory(), file.getPath() );
        }
        return file;
    }
    
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
        currentFile = null;
        getUndoManager().discardAllEdits();
        kawapad.setText("");
//          JOptionPane.showMessageDialog( this, "OPEN NEW" );
    }
    public void openNew() throws IOException {
        if ( ! confirmSave( ConfirmType.CREATE_NEW_FILE) ) {
            return;
        }
        openNewProc();
    }

    private void openFileProc(File file) throws IOException {
        // ADDED (Fri, 05 Jun 2020 02:09:15 +0900)
        file = resolveFile( file );
        if ( ( ! file.exists() ) || ( ! file.isFile() ) ) {
            file.getParentFile().mkdirs();
            if ( ! file.createNewFile() ) {
                throw new RuntimeException( "cannot create the specified file (" + file.getPath() + ")" );
            }
            setTextProc( file, "\n#| empty file |#\n\n" );
        } else {
            String s = new String( Files.readAllBytes( file.toPath() ),  Charset.defaultCharset() );
            setTextProc( file, s );
        }

    }
    /**
     *  
     * @param filePath
     *     null when it opens a newly created document.
     * @param s
     *     the text to show on the editor.
     */
    void setTextProc( File filePath, String s ) {
//          this.undoManager.discardAllEdits();
        this.kawapad.setText( s );
        this.currentFile = filePath;
        this.fileModified = false;
        
        /*
         * Discard edits after set text or CTRL-Z to clear all text 
         * which is not supposed to be. (Tue, 09 Oct 2018 03:04:23 +0900)
         */
        this.getUndoManager().discardAllEdits();
//          JOptionPane.showMessageDialog(this, "OPEN FILE PROC" + file );
    }
    public void resetFileModifiedStatus() {
        this.fileModified = false;
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
        if ( currentFile != null )
            fc.setCurrentDirectory( currentFile.getParentFile() );
        fc.addChoosableFileFilter( SCHEME_FILE_FILTER );
        fc.setMultiSelectionEnabled(false);
        int i = fc.showOpenDialog(this);
        if ( i == JFileChooser.APPROVE_OPTION ) {
            openFileProc( fc.getSelectedFile() );
        }
    }
    public void openIntro() {
        setCaretPosition( 0 );
        // >>> (Wed, 13 Nov 2019 17:08:52 +0900) 
        // kawapad.getThreadManager().startScratchPadThread( KawapadEvaluator.create( kawapad, "(help about-intro)", 
        // getCurrentDirectory(), getCurrentFile(), true, true, true ));
        // evaluate( "(import (lamu help))(help about-intro)", true, true, true );
        // <<< (Wed, 13 Nov 2019 17:08:52 +0900) 
        
        /*
         *  These have to be separatedly executed. 
         * The reason why it fails if executed together.
         *  (Tue, 21 Apr 2020 00:33:08 +0900)
         */
//        evaluate( "(import (lamu help))", true, true, true );
//        evaluate( "(help about-intro)", true, true, true );

        /*
         * Defining syntax affects to compilation process and `help` is a syntax.
         * At the time of compilation, `help` is not defined so `about-intro` is
         * interpolated as a variable; therefore, it is an undefined variable which
         * causes a compilation error. In order to avoid this problem, `eval` it.
         * (Tue, 21 Apr 2020 21:50:10 +0900)
         */
        evaluate( "(require lamu.help)(eval '(help about-intro))", true, true, true );

    }

    static class ConfirmType { 
        static final Kawapad.ConfirmType OPEN_FILE = new ConfirmType( 
            "Do you save the current document before opening another?", 
            "Open a File" );
        static final Kawapad.ConfirmType CREATE_NEW_FILE = new ConfirmType( 
            "Do you save the current document before creating a new file?", 
            "Creatuing a New File" );
        static final Kawapad.ConfirmType CLOSE_WINDOW = new ConfirmType( 
            "Do you save the current document before closing the window ?", 
            "Closing the Window" );
        
        static final Kawapad.ConfirmType CLOSE = new ConfirmType( // ADDED (Sat, 20 Jun 2020 21:10:06 +0900) 
            "Are you sure you want to close the current document?", 
            "Closing the Window" );
        final String caption;
        final String title;
        public ConfirmType(String caption, String title) {
            this.caption = caption;
            this.title = title;
        }
    }
    
    boolean confirmSaveProc(Kawapad.ConfirmType confirmType) throws IOException {
        // ADDED >> (Sun, 21 Jun 2020 05:07:30 +0900) 
        if ( ! ENABLED_CONFIRMATION )
            return true;
        // <<<

        Object[] options = {
            UIManager.getString("OptionPane.yesButtonText"),
            UIManager.getString("OptionPane.noButtonText"),
            UIManager.getString("OptionPane.cancelButtonText")
        };
        String defaultOption = UIManager.getString("OptionPane.cancelButtonText");
        
        int i = JOptionPane.showOptionDialog( 
            null, 
            confirmType.caption,
            confirmType.title , 
            JOptionPane.YES_NO_CANCEL_OPTION,
            JOptionPane.QUESTION_MESSAGE,
            null,
            options,
            defaultOption );

        if ( i == JOptionPane.YES_OPTION ) {
            if ( currentFile == null ) {
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
    }
    public boolean confirmClose(Kawapad.ConfirmType confirmType) throws IOException {
        // ADDED >> (Sun, 21 Jun 2020 05:07:30 +0900) 
        if ( ! ENABLED_CONFIRMATION )
            return true;
        // <<<
        Object[] options = {
            UIManager.getString("OptionPane.okButtonText"),
            UIManager.getString("OptionPane.cancelButtonText")
        };
        String defaultOption = UIManager.getString("OptionPane.cancelButtonText");
        int i = JOptionPane.showOptionDialog(
            null,
            confirmType.caption,
            confirmType.title , 
            JOptionPane.OK_CANCEL_OPTION,
            JOptionPane.QUESTION_MESSAGE,
            null,
            options,
            defaultOption );

        return i == JOptionPane.OK_OPTION ;
    }

    public boolean confirmSave( Kawapad.ConfirmType confirmType ) throws IOException {
        if ( fileModified ) {
            return confirmSaveProc(confirmType);
        } else {
            return true;
        }
    }

    private void saveFileProc(File filePath) throws IOException {
        Files.write(filePath.toPath(), kawapad.getText().getBytes( Charset.defaultCharset() ), StandardOpenOption.CREATE , StandardOpenOption.TRUNCATE_EXISTING );
        this.fileModified = false;
        this.currentFile = filePath;
//          JOptionPane.showMessageDialog(this, "SAVE FILE!" + file );
    }

    public void saveFile() throws IOException {
        if ( currentFile != null )
            saveFileProc( currentFile );
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
    
    public static final String KAWAPAD_OPEN_NEW = "kawapad-open-new";

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action OPEN_NEW_ACTION = new TextAction2( KAWAPAD_OPEN_NEW ) {
        
        @Override
        public void actionPerformed(ActionEvent e) {
            try {
                openNew();
            } catch (IOException e1) {
                logError("", e1);
            }
        }
        {
            putValue( Action2.CAPTION, "Open New" );
//            putValue( Action.ACCELERATOR_KEY, AcceleratorKeyList.getKeyStroke(KeyEvent.VK_N , KeyEvent.CTRL_MASK | KeyEvent.SHIFT_MASK ));
            putValue( Action.MNEMONIC_KEY , (int) 'n' );
//            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl shift N" );
            AcceleratorKeyList.putAcceleratorKeyList( this );
        }        
    };

    public static final String KAWAPAD_OPEN_FILE = "kawapad-open-file";

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action OPEN_FILE_ACTION = new TextAction2( KAWAPAD_OPEN_FILE ) {
        @Override
        public void actionPerformed(ActionEvent e) {
            try {
                {
                    String text = kawapad.getSelectedText();
                    if ( text != null ) {
                        if ( text.startsWith( "\"" ) )
                            text = text.substring( 1 );
                        if ( text.endsWith( "\"" ) )
                            text = text.substring( 0, text.length()-1 );
                        
                        createKawapadFrame( new File( text ) );
                        return;
                    }
                }
                openFile();
            } catch (IOException e1) {
                logError("", e1);
            }
        }
        {
            putValue( Action2.CAPTION, "Open" );
            putValue( Action.MNEMONIC_KEY , (int) 'o' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl O" );
        }
    };

    public static final String KAWAPAD_SAVE_FILE = "kawapad-save-file";
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action SAVE_FILE_ACTION = new TextAction2( KAWAPAD_SAVE_FILE ) {
        @Override
        public void actionPerformed(ActionEvent e) {
            try {
                saveFile();
            } catch (IOException e1) {
                logError("", e1);
            }
        }
        {
            putValue( Action2.CAPTION, "Save" );
            putValue( Action.MNEMONIC_KEY , (int) 'o' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl S" );
        }
    };
    
    public static final String KAWAPAD_SAVE_FILE_AS = "kawapad-save-file-as";

    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action SAVE_FILE_AS_ACTION = new TextAction2( KAWAPAD_SAVE_FILE_AS ) {
        @Override
        public void actionPerformed(ActionEvent e) {
            try {
                saveFileAs();
            } catch (IOException e1) {
                logError("", e1);
            }
        }
        {
            putValue( Action2.CAPTION, "Save as" );
            putValue( Action.MNEMONIC_KEY , (int) 'o' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl shift S" );
        }
    };
    
    static Window lookupWindow( Component c ) {
        if ( c == null ) {
            return null;
        } else if ( c instanceof Window ) {
            return (Window) c;
        } else {
            return lookupWindow( c.getParent() );
        }
    }

    private void closeWindow() {
        Window parent = lookupWindow( this.getParent() );
        if ( parent != null ) {
            parent.dispatchEvent( new WindowEvent( parent, WindowEvent.WINDOW_CLOSING ));
        }
    }
    
    // INTEGRATED_ACTIONS (Wed, 11 Sep 2019 08:26:57 +0900)
    @AutomatedActionField
    public final Action CLOSE_ACTION = new TextAction2( KAWAPAD_CLOSE ) {
        @Override
        public void actionPerformed(ActionEvent e) {
            closeWindow();
        }
        {
            putValue( Action2.CAPTION, "Close" );
            putValue( Action.MNEMONIC_KEY , (int) 'c' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl W" );
        }
    };
    
    public static final String KAWAPAD_CLOSE = "kawapad-close";
    
    
    public WindowListener createCloseQuery( Runnable onClose ) {
        return new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent we) {
                boolean result;
                try {
                    if ( fileModified ) {
                        result = confirmSave( ConfirmType.CLOSE_WINDOW );
                    } else {
                        result = confirmClose( ConfirmType.CLOSE );
                    }
                } catch (IOException e) {
                    logError( "" , e );
                    result = false;
                }
                if ( result ) {
                    onClose.run();
                } else {
                    // Stay open
                }
            }
        };
    }
    
    public KawapadFrame createKawapadFrame( File f ) throws IOException {
        KawapadFrame kawapadFrame = new KawapadFrame( this.kawapad.getEvaluator(), false, "Kawapad" );
        Kawapad newKawapad = kawapadFrame.getKawapad();
        kawapadFrame.processInit();
        if ( f != null )
            /*
             * NOTE: Calling resolveFile() (Fri, 05 Jun 2020 12:37:33 +0900)
             * 
             * The filename should resolve to the current directory of the current Kawapad,
             * otherwise the new Kawapad resolves the filename to the current directory of
             * itself. It cannot open the specified file properly because the current
             * directory of the new Kawapad is not set yet at this point.
             */
            newKawapad.openFile(
                this.resolveFile(f));
        return kawapadFrame; 
    }


    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 
    // highlighter
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private KawapadSyntaxHighlighter syntaxHighlighter; // See the constructor how this field is initialized.
    public KawapadSyntaxHighlighter getSyntaxHighlighter() {
        return syntaxHighlighter;
    }

    static void updateHighlight( Kawapad kawapad, int offset ) {
        highlightMatchingParentheses(kawapad, offset);
        if ( ! kawapad.searchBox.isVisible() ) {
            highlightCurrentWord(kawapad);
        }
    }
    public static void highlightMatchingParentheses(Kawapad kawapad, int offset) {
        if ( DEBUG_PARENTHESIS ) 
            logInfo( "highlightMatchningParentheses offset=" + offset  );
        if ( ENABLED_PARENTHESIS_HIGHLIGHT )
            // highlight the corresponding parentheses.
            try {
                HighlightPainter highlightPainter = 
                    kawapad.getSyntaxHighlighter().getSyntaxElementList().get( KawapadSyntaxElementType.PARENTHESIS_HIGHLIGHT ).getHighlightPainter();
                Caret caret = kawapad.getCaret();
                if ( caret.getDot() == caret.getMark() ) {
                    KawapadTemporaryParenthesisHighlighter.highlightMatchingParenthesis( kawapad, highlightPainter,  caret.getDot() + offset );
                } else {
                    if ( caret.getMark() < caret.getDot() ) {
                        KawapadTemporaryParenthesisHighlighter.highlightMatchingParenthesis( kawapad, highlightPainter, caret.getDot() + offset -1 );
                    } else {
                        KawapadTemporaryParenthesisHighlighter.highlightMatchingParenthesis( kawapad, highlightPainter, caret.getDot() + offset  );
                    }
                }
            } catch (BadLocationException e) {
                logError( "", e );
            }
    }
    public static void highlightCurrentWord(Kawapad kawapad) throws InternalError {
        // highlight the current word.
        Caret caret = kawapad.getCaret();
        String searchString = 
            KawapadTemporarySearchHighlighter.getCurrentWord( 
                KawapadSelection.getText( kawapad.getDocument() ), 
                caret );
        highlightSpecificWord(kawapad, searchString);
    }
    public static void highlightSpecificWord(Kawapad kawapad,String searchString) throws InternalError {
        // highlight the current word.
        try {
            HighlightPainter highlightPainter = 
                kawapad.getSyntaxHighlighter().getSyntaxElementList().get( KawapadSyntaxElementType.KEYWORD_HIGHLIGHT ).getHighlightPainter();
            KawapadTemporarySearchHighlighter.highlightSearchPatterns( kawapad, highlightPainter, searchString, true );
        } catch (BadLocationException e) {
            logError( "", e );
        }
    }
    
    static void highlightMatchingParenthesesLater( Kawapad kawapad, int offset ) {
//        logError( "log", new Throwable() );

        if ( ENABLED_PARENTHESIS_HIGHLIGHT )
            SwingUtilities.invokeLater( new Runnable() {
                public void run() {
                    updateHighlight( kawapad, offset );
                }
            });
    }
    

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    // The Bridge to the Scheme Interface of Highlighter 
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    static final Comparator<String> KEYWORD_COMPARATOR = new Comparator<String>() {
        @Override
        public int compare(String o1, String o2) {
            int i = o2.length() - o1.length();
            if ( i != 0 )
                return i;
            else
                return o1.compareTo( o2 );
        }
    };
    static void notifySyntaxChangeToAll() {
        synchronized ( Kawapad.class ) {
            for ( Kawapad kawapad : kawapadList ) {
                kawapad.notifySyntaxChange();
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 
    //  Keyword Management
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    private static final boolean isLispWord( String i ) {
        return i.startsWith("lambda")   
            || i.startsWith("let")
            || i.startsWith("define")
            || i.startsWith("with")
            || i.startsWith("call-with") 
            || i.startsWith("syntax")
            || i.startsWith("object")
            || i.startsWith( "(" ); // <<< ADDED (Sun, 31 May 2020 22:42:07 +0900)
    }
    
    final ArrayList<String> lispWordList = new ArrayList<>();
    final Function<String,Integer> lispWordChecker = (i)->{
        if ( i.equals("(")) {
            return 3;
        } else if (lispWordList.contains(i) || isLispWord(i))
            return 2;
        else
            return Integer.MIN_VALUE;
    };
    public Function<String,Integer> getLispWordChecker() {
        return lispWordChecker;
    }
    public void addLispKeyword( String s ) {
        synchronized ( Kawapad.this ) {
            lispWordList.add( s );
            notifySyntaxChangeToAll();
        }
    }
    public void addAllLispKeywords( List<String> s ) {
        synchronized ( Kawapad.this ) {
            lispWordList.addAll( s );
            notifySyntaxChangeToAll();
        }
    }
    public void deleteLispKeyword( String s ) {
        synchronized ( Kawapad.this ) {
            lispWordList.remove( s );
            notifySyntaxChangeToAll();
        }
    }
    public void deleteAllLispKeywords( List<String> s ) {
        synchronized ( Kawapad.this ) {
            lispWordList.removeAll( s );
            notifySyntaxChangeToAll();
        }
    }
    void notifySyntaxChange() {
        synchronized ( Kawapad.this ) {
            this.getSyntaxHighlighter().resetSyntaxElementList();
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 
    //  File Management
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    @Override
    public void initMenu( Map<String,JMenu> menuMap ) {
        JMenu file = menuMap.get( "file" ); 
        JMenu edit = menuMap.get( "edit" ); 
        @SuppressWarnings("unused")
        JMenu view = menuMap.get( "view" );
        JMenu navigate = menuMap.get( "navigate" );
        JMenu scheme = menuMap.get( "scheme" );
        
        file.add( new JMenuItem( kawapad.OPEN_NEW_ACTION ) );
        file.add( new JMenuItem( kawapad.OPEN_FILE_ACTION ) );
        file.add( new JMenuItem( kawapad.SAVE_FILE_ACTION ) );
        file.add( new JMenuItem( kawapad.SAVE_FILE_AS_ACTION ) ); 
        file.add( new JMenuItem( kawapad.CLOSE_ACTION ) ); 
        
        scheme.add( new JMenuItem( kawapad.EVALUATE_REPLACE_ACTION ));
        scheme.add( new JMenuItem( kawapad.SELECT_EVALUATE_ACTION ));
        scheme.add( new JMenuItem( kawapad.EVALUATE_ACTION ) );
        scheme.add( new JMenuItem( kawapad.RUN_ACTION ) );
        scheme.add( new JMenuItem( kawapad.INTERRUPT_ACTION ) );
        
        edit.add( new JMenuItem( kawapad.DEBUG_ACTION ) );
        edit.add( new JMenuItem( kawapad.UNDO_ACTION ) );
        edit.add( new JMenuItem( kawapad.REDO_ACTION ) );
        edit.add( new JMenuItem( kawapad.PASTE_ACTION ) );
        edit.add( new JMenuItem( kawapad.SEARCH_ACTION ) );
//        edit.add( new JMenuItem( kawapad.DESELECT_ACTION ) );
        
        edit.addSeparator();
        
        edit.add( new JMenuItem( kawapad.getActionMap().get( DefaultEditorKit.deletePrevCharAction )  ));
        edit.add( new JMenuItem( kawapad.SHIFT_INDENT_RIGHT_ACTION ) );
        edit.add( new JMenuItem( kawapad.SHIFT_INDENT_LEFT_ACTION ) );
        edit.add( new JMenuItem( kawapad.INDENTATION_CORRECTOR_ACTION ) );
        
        navigate.add( new JMenuItem( kawapad.PARENTHESIS_EXPAND_SELECTION_ACTION ) );
        navigate.add( new JMenuItem( kawapad.PARENTHESIS_SHRINK_SELECTION_DYNAMICALLY_ACTION ) );
        navigate.add( new JMenuItem( kawapad.PARENTHESIS_SELECT_LEFT_ACTION ) );
        navigate.add( new JMenuItem( kawapad.PARENTHESIS_SELECT_RIGHT_ACTION ) );
        navigate.add( new JMenuItem( kawapad.PARENTHESIS_SWAP_LEFT_ACTION ) );
        navigate.add( new JMenuItem( kawapad.PARENTHESIS_SWAP_RIGHT_ACTION ) );
        navigate.addSeparator();
        navigate.add( new JMenuItem( kawapad.LISPWORD_SELECT_CURRENT_ACTION ) );
        navigate.add( new JMenuItem( kawapad.LISPWORD_SELECT_LEFT_ACTION ) );
        navigate.add( new JMenuItem( kawapad.LISPWORD_SELECT_RIGHT_ACTION ) );
        navigate.add( new JMenuItem( kawapad.SIMPLE_PARENTHESIS_JUMP_LEFT_ACTION ) );
        navigate.add( new JMenuItem( kawapad.SIMPLE_PARENTHESIS_JUMP_RIGHT_ACTION ) );
        
        
        // TEXTUAL_INCREMENT
        {
            kawapad.textualIncrement.initMenu( menuMap );
        }
        
//          editMenuItem.addSeparator();
        //
//          editMenuItem.add( new JMenuItem( SIMPLE_PARENTHESIS_JUMP_LEFT_ACTION ) );
//          editMenuItem.add( new JMenuItem( SIMPLE_PARENTHESIS_JUMP_RIGHT_ACTION ) );
//          editMenuItem.add( new JMenuItem( SIMPLE_PARENTHESIS_SELECT_JUMP_LEFT_ACTION ) );
//          editMenuItem.add( new JMenuItem( SIMPLE_PARENTHESIS_SELECT_JUMP_RIGHT_ACTION ) );
//          editMenuItem.add( new JMenuItem( PARENTHESIS_JUMP_LEFT_ACTION ) );
//          editMenuItem.add( new JMenuItem( PARENTHESIS_JUMP_RIGHT_ACTION ) );
//          editMenuItem.add( new JMenuItem( PARENTHESIS_SELECT_JUMP_LEFT_ACTION ) );
//          editMenuItem.add( new JMenuItem( PARENTHESIS_SELECT_JUMP_RIGHT_ACTION ) );
//          editMenuItem.add( new JMenuItem( PARENTHESIS_SELECT_ACTION ) );
//          editMenuItem.add( new JMenuItem( PARENTHESIS_DESELECT_ACTION ) );
        
    }
    
    public void createDefaultMenuBar( JMenuBar menuBar ) {
        JMenu fileMenuItem = new JMenu( "File" );
        fileMenuItem.setMnemonic('f');
        menuBar.add( fileMenuItem );
        
        JMenu editMenuItem = new JMenu( "Edit" );
        editMenuItem.setMnemonic('e');
        menuBar.add( editMenuItem );

        JMenu viewMenuItem = new JMenu( "View" );
        viewMenuItem.setMnemonic('v');
        menuBar.add( viewMenuItem );

        JMenu navigateMenuItem = new JMenu( "Navigate" );
        navigateMenuItem.setMnemonic('n');
        menuBar.add( navigateMenuItem );
//
        JMenu schemeMenuItem = new JMenu( "Scheme" );
        schemeMenuItem.setMnemonic('r');
        menuBar.add( schemeMenuItem );

        Map<String,JMenu> map = new HashMap<>();
        map.put( "file"     , fileMenuItem );
        map.put( "edit"     , editMenuItem );
        map.put( "view"     , viewMenuItem );
        map.put( "navigate" , navigateMenuItem );
        map.put( "scheme"   , schemeMenuItem );
        kawapad.initMenu( map );

        lamu.lib.swing.Action2.processMenuBar( menuBar );
    }
    
    
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 
    //  Function Keys
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////

    class FunctionKeyAction extends TextAction2 {
        private String script;
        FunctionKeyAction(String name, String script, String caption, char mnemonic, String keyStroke) {
            super( name );
            this.script = script;
            putValue( Action2.CAPTION, "Execute " + caption );
            putValue( Action.MNEMONIC_KEY , (int) mnemonic );
            AcceleratorKeyList.putAcceleratorKeyList( this, keyStroke );
        }
        public FunctionKeyAction(int i) {
            this( 
                "function-key-" + i,
                "(function-key-" + i + ")",
                "Execute F" + i,
                (""+i).charAt(0),
                "F" + i );
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            boolean doInsertText;
            if ( ( e.getModifiers() & MouseEvent.SHIFT_MASK) != 0 ) {
                doInsertText = true;
            } else {
                doInsertText = false;
            }
            evaluate( this.script, doInsertText, true, false );
        }
    }

    @AutomatedActionField
    public final Action FUNCTION_KEY1_ACTION = new FunctionKeyAction( 1 );
    @AutomatedActionField
    public final Action FUNCTION_KEY2_ACTION = new FunctionKeyAction( 2 );
    @AutomatedActionField
    public final Action FUNCTION_KEY3_ACTION = new FunctionKeyAction( 3 );
    @AutomatedActionField
    public final Action FUNCTION_KEY4_ACTION = new FunctionKeyAction( 4 );
    @AutomatedActionField
    public final Action FUNCTION_KEY5_ACTION = new FunctionKeyAction( 5 );
    @AutomatedActionField
    public final Action FUNCTION_KEY6_ACTION = new FunctionKeyAction( 6 );
    @AutomatedActionField
    public final Action FUNCTION_KEY7_ACTION = new FunctionKeyAction( 7 );
    @AutomatedActionField
    public final Action FUNCTION_KEY8_ACTION = new FunctionKeyAction( 8 );
    @AutomatedActionField
    public final Action FUNCTION_KEY9_ACTION = new FunctionKeyAction( 9 );
    @AutomatedActionField
    public final Action FUNCTION_KEY10_ACTION = new FunctionKeyAction( 10 );
    @AutomatedActionField
    public final Action FUNCTION_KEY11_ACTION = new FunctionKeyAction( 11 );
    @AutomatedActionField
    public final Action FUNCTION_KEY12_ACTION = new FunctionKeyAction( 12 );
    @AutomatedActionField
    public final Action FUNCTION_KEY13_ACTION = new FunctionKeyAction( 13 );
    @AutomatedActionField
    public final Action FUNCTION_KEY14_ACTION = new FunctionKeyAction( 14 );
    @AutomatedActionField
    public final Action FUNCTION_KEY15_ACTION = new FunctionKeyAction( 15 );
    @AutomatedActionField
    public final Action FUNCTION_KEY16_ACTION = new FunctionKeyAction( 16 );
    @AutomatedActionField
    public final Action FUNCTION_KEY17_ACTION = new FunctionKeyAction( 17 );
    @AutomatedActionField
    public final Action FUNCTION_KEY18_ACTION = new FunctionKeyAction( 18 );
    @AutomatedActionField
    public final Action FUNCTION_KEY19_ACTION = new FunctionKeyAction( 19 );
    @AutomatedActionField
    public final Action FUNCTION_KEY20_ACTION = new FunctionKeyAction( 20 );
    @AutomatedActionField
    public final Action FUNCTION_KEY21_ACTION = new FunctionKeyAction( 21 );
    @AutomatedActionField
    public final Action FUNCTION_KEY22_ACTION = new FunctionKeyAction( 22 );
    @AutomatedActionField
    public final Action FUNCTION_KEY23_ACTION = new FunctionKeyAction( 23 );
    @AutomatedActionField
    public final Action FUNCTION_KEY24_ACTION = new FunctionKeyAction( 24 );

    
    
    
    
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 
    //  Scheme API
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    // ( canonical )
    public static final LamuDocument aboutIntroDoc = new LamuDocument(){{
        setCategory( "kawapad-procedures" );
        setNames( "about-intro" );
        setParameterDescription( "" );
        setReturnValueDescription( "" );
        setShortDescription( "Welcome to Kawapad!" );
        setLongDescription( ""
            + "Kawapad is a simple Lisp Scheme editor which can edit and execute Scheme code "
            + "on the fly. Kawapad includes Java implementation of a powerful computer language Lisp Scheme. "
            + " "
            + "To show all available procedures, execute (help). \n"
            + "To show help of a procedure, execute (help [procedure-name] ) . \n"
            + "" 
            );
    }};

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    public final MultipleNamedProcedure1 prettifyProc = new PrettifyProc(new String[] { "prettify", "pre" });
    public final class PrettifyProc extends MultipleNamedProcedure1 {
        public PrettifyProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply1(Object arg1 ) throws Throwable {
            return SchemeValues.toSchemeString(   
                Kawapad.correctIndentation( Kawapad.this, 
                    SchemePrinter.printSchemeValue( arg1 )));
        }
    }
    public MultipleNamedProcedure1 getPrettify() {
        return prettifyProc;
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    public static final LamuDocument loadFontDoc = new LamuDocument(){{
        setCategory( "kawapad-procedures" );
        setNames( "load-font"  );
        setParameterDescription( "" );
        addParameter( 0, "file-size", "string", null , false, "Specifies the path to the font file. " );
        addParameter( 0, "font-size", "number", null , false, "Specifies its font size. " );
        setReturnValueDescription( "::void" );
        setShortDescription( "Set the main font of the editor." );
        setLongDescription( ""
            + "Kawapad can change its font-face. ||<name/>|| loads a file from the filesystem and "
            + "set it to the font-face of Kawapad. "
            + "" 
            );
    }};

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    public final MultipleNamedProcedure2 loadFontProc = new LoadFontProc(new String[] { "load-font" });
    public final class LoadFontProc extends MultipleNamedProcedure2 {
        public LoadFontProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply2(Object arg1,Object arg2) throws Throwable {
            String filePath = SchemeValues.anyToString( arg1 );
            float  fontSize = SchemeValues.toFloat( arg2 );
            Font font = Kawapad.loadFont( filePath, fontSize );
            Kawapad kawapad = Kawapad.this;
            kawapad.setFont( font );
            return Values.empty;
        }
    }
    public MultipleNamedProcedure2 getLoadFont() {
        return loadFontProc;
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    public static final LamuDocument loadFontUIDoc = new LamuDocument(){{
        setCategory( "kawapad-procedures" );
        setNames( "load-font-ui" );
        setParameterDescription( "" );
        addParameter( 0, "file-size", "string", null , false, "Specifies the path to the font file. " );
        addParameter( 0, "font-size", "number", null , false, "Specifies its font size. " );
        setReturnValueDescription( "::void" );
        setShortDescription( "Set the main font of the ui." );
        setLongDescription( ""
            + "_<name/>_ loads a file from the specified file and "
            + "set it as the default font of the current ui. "
            + "" );
    }};

    public final MultipleNamedProcedure2 loadFontUIProc = new LoadFontUIProc(new String[] { "load-font-ui" });
    public final class LoadFontUIProc extends MultipleNamedProcedure2 {
        public LoadFontUIProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply2(Object arg1,Object arg2) throws Throwable {
            String filePath = SchemeValues.anyToString( arg1 );
            float  fontSize = SchemeValues.toFloat( arg2 );
            Font font = Kawapad.loadFont( filePath, fontSize );
            Kawapad.setUIFont( new FontUIResource(font));
            return Values.empty;
        }
    }
    public MultipleNamedProcedure2 getLoadFontUI() {
        return loadFontUIProc;
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 
    // Note : the `set-syntax-color` procedure may be useless now since now we are using LAF objects to customise UI.
    // We have to investigate it.
    // (Sat, 11 Apr 2020 14:47:27 +0900)
    // 
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    public final MultipleNamedProcedureN setSyntaxColorProc = new SetSyntaxColorProc(new String[] { "set-syntax-color" });
    public final class SetSyntaxColorProc extends MultipleNamedProcedureN {
        public SetSyntaxColorProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply2(Object arg1, Object arg2) throws Throwable {
            SyntaxElement syntaxElement = Kawapad.this.getSyntaxHighlighter().getSyntaxElementList().get(
                KawapadSyntaxElementType.schemeValueOf((Symbol)arg1));
            syntaxElement.setForegroundColor((Color)arg2);
            return Values.empty; 
        }

        @Override
        public Object apply3(Object arg1, Object arg2, Object arg3) throws Throwable {
            SyntaxElement syntaxElement = Kawapad.this.getSyntaxHighlighter().getSyntaxElementList().get(
                KawapadSyntaxElementType.schemeValueOf((Symbol)arg1));
            syntaxElement.setForegroundColor((Color)arg2);
            syntaxElement.setBackgroundColor((Color)arg3);
            return Values.empty; 
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            WrongArguments.checkArgCount( this.getName() , 2, 3, args.length );
            if ( args.length == 2 )
                return apply2( args[0], args[1] );
            else if ( args.length == 3 )
                return apply3( args[0], args[1], args[2] );

            throw new InternalError();
        }
    }
}

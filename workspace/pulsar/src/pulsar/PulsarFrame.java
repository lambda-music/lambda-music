/*
 * Pulsar-Sequencer written by Atsushi Oka 
 * Copyright 2018 Atsushi Oka
 *
 * This file is part of Pulsar-Sequencer. 
 * 
 * Pulsar-Sequencer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Pulsar-Sequencer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Pulsar-Sequencer.  If not, see <https://www.gnu.org/licenses/>.
 */

package pulsar;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.imageio.ImageIO;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JRootPane;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JSplitPane;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.jaudiolibs.jnajack.JackException;

import gnu.mapping.Environment;
import gnu.mapping.Values;
import kawa.standard.Scheme;
import kawapad.KawapadFrame;
import metro.MetroTrack;
import pulsar.Pulsar.TempoTapperTempoNotifier;
import pulsar.lib.scheme.SafeProcedureN;
import pulsar.lib.scheme.SchemeUtils;
import pulsar.lib.scheme.scretary.SchemeSecretary;
import pulsar.lib.secretary.Invokable;
import pulsar.lib.secretary.SecretaryMessage;
import pulsar.lib.swing.Action2;
import pulsar.lib.swing.FlawLayout;
import pulsar.lib.swing.JNamedPanel;

public class PulsarFrame extends KawapadFrame {
    private static final String PULSAR_DEFAULT_CAPTION = "Pulsar - a Lisp Scheme Music Sequencer";

    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) {
        LOGGER.log(Level.SEVERE, msg, e);
    }
    static void logInfo(String msg) {
        LOGGER.log(Level.INFO, msg);
    }
    static void logWarn(String msg) {
        LOGGER.log(Level.WARNING, msg);
    }

    PulsarFrame frame = this;
    
    public static PulsarFrame start(Pulsar pulsar) {
        return new PulsarFrame( pulsar, false, PULSAR_DEFAULT_CAPTION );
    }
    public static PulsarFrame start(Pulsar pulsar, boolean shutdownWhenClose ) {
        return new PulsarFrame( pulsar, shutdownWhenClose, PULSAR_DEFAULT_CAPTION );
    }

    static final int PB_POSITION_MAX = 1024;
    
    Runnable shutdownProc01 = new Runnable() {
        @Override
        public void run() {
//          frame.setVisible( false );
            frame.quit();
        }
    };

    public static void registerLocalSchemeInitializers( SchemeSecretary schemeSecretary, PulsarFrame pulsarGui ) {
        schemeSecretary.registerSchemeInitializer( pulsarGui, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
            @Override
            public void execute0( Scheme scheme, Object[] args ) {
                pulsarGui.initPulsarGui();
            }
        });
        
        schemeSecretary.addShutdownHook(pulsarGui.shutdownProc01);
    }
    
    public static void invokeLocalSchemeInitializers( SchemeSecretary schemeSecretary, PulsarFrame pulsarGui ) {
        schemeSecretary.invokeSchemeInitializers( pulsarGui );
    }
    public static void unregisterLocalSchemeInitializers( SchemeSecretary schemeSecretary, PulsarFrame pulsarGui ) {
        schemeSecretary.unregisterSchemeInitializer( pulsarGui );
        schemeSecretary.removeShutdownHook(pulsarGui.shutdownProc01);
    }
    
    public static void registerGlobalSchemeInitializers( SchemeSecretary schemeSecretary ) {
        schemeSecretary.registerSchemeInitializer( PulsarFrame.class, new SecretaryMessage.NoReturnNoThrow<Scheme>() {
            @Override
            public void execute0( Scheme scheme, Object[] args ) {
                initScheme( scheme );
            }
        });
    }


    public void openFile( File mainFile ) throws IOException {
        pulsar.getSchemeSecretary().executeWithoutSecretarially( new SecretaryMessage.NoReturn<Scheme,IOException>() {
            @Override
            public void execute0(Scheme resource, Object[] args) throws IOException{
                logInfo( "Pulsar#openMainFile()" );
                if ( ! mainFile.isFile() )
                    throw new RuntimeException( "The specified file does not exist (" + mainFile.getPath() + ")" );
                
                frame.getKawapad().openFile( mainFile );
            }
        }, Invokable.NOARG );
    }
    // Is this really necessary to be executed with secretary?
    public void openIntro() throws IOException {
        pulsar.getSchemeSecretary().executeWithoutSecretarially( new SecretaryMessage.NoReturn<Scheme,IOException>() {
            @Override
            public void execute0(Scheme resource, Object[] args) throws IOException{
                logInfo( "Pulsar#openIntro()" );
                frame.getKawapad().openIntro();
            }
        }, Invokable.NOARG );
    }
    
    public void openIntro2() {
        InputStream in = null;
        try {
            try {
                in = PulsarFrame.class.getResourceAsStream( "lib/intro.scm" );
                String s = new String( SchemeUtils.readAll( in ), "UTF-8" );
                frame.getKawapad().setNewText( s );
            } finally {
                if ( in != null )
                    in.close();
            }   
        } catch ( IOException e ) {
            throw new Error( "Internal Error" , e );
        }
    }
    
    //////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////

    private static ThreadLocal<PulsarFrame> threadLocalKawapad = new ThreadLocal<>();
    public static void setCurrent( PulsarFrame pulsarGui ) {
        threadLocalKawapad.set( pulsarGui );
    }
    public static PulsarFrame getCurrent() {
        PulsarFrame currentKawapad = threadLocalKawapad.get();
        if ( currentKawapad == null ) 
            throw new IllegalStateException();
        return currentKawapad;
    }
    private final Runnable threadInitializer = new Runnable() {
        @Override
        public void run() {
            setCurrent( PulsarFrame.this );
        }
    };
    public Runnable threadInitializer() {
        return threadInitializer;
    }

    //////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////
    
    boolean shutdownWhenClose;

    Pulsar pulsar;
    PulsarFrame( Pulsar pulsar, boolean shutdownWhenClose, String caption ) {
        super( pulsar.getSchemeSecretary(), caption );
        
        this.pulsar = pulsar;
        this.shutdownWhenClose = shutdownWhenClose;
        
        frame.getKawapad().addThreadInitializer( threadInitializer());

        PulsarFrame.registerLocalSchemeInitializers( pulsar.getSchemeSecretary(), PulsarFrame.this );
        //          DELETED >>> INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
        //          PulsarGui.invokeLocalSchemeInitializers( schemeSecretary, PulsarGui.this );
        //          DELETED <<< INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
        
        this.kawapad.addThreadInitializer( pulsar.getThreadInitializer() );
        this.kawapad.addVariableInitializer( pulsar.getVariableInitializer() );
        
        initGui();
        initGuiMenu();
        initGuiIcon();
        initGuiProgressBar();
        initGuiFocus();
        initPulsarGui();
        
    }
    
    private void initPulsarGui() {
        Pulsar.createTimer(pulsar, 1000, 20, new Invokable() {
            @Override
            public Object invoke(Object... args) {
                if ( pulsar.isOpened() ) {
                    MetroTrack track = pulsar.searchTrack( "main" );
                    
                    //  This happens quite often so let us ignore it. (Mon, 29 Jul 2019 12:21:50 +0900)
                    //  Additionally this code have never been executed. Just added this for describing the concept.
                    //  if ( track == null ) {
                    //      logWarn( "" );
                    //  }
                    
                    if ( track != null && pb_position != null ) {
                        double value=0;
                        synchronized ( track.getLock() ) {
                            value = track.getTrackPosition();
                        }
                        pb_position.setValue((int) (value * PulsarFrame.PB_POSITION_MAX) );
                        pb_position.repaint();
                        pb_position.revalidate();
                    }
                }
                return Values.empty;
            }
        });
    }

    enum TempoRange {
        WIDE  ("Wide",   0,1000), 
        NORMAL("Normal", 0,500), 
        SLOW  ("Slow",   0,100), 
        MEDIUM("Medium", 100,250),
        FAST  ("Fast",   250,500);
        private String caption;
        private int min;
        private int max;
        private TempoRange(String caption, int min, int max ) {
            this.caption = caption;
            this.min = min;
            this.max = max;
        }
        public final Action createAction( PulsarFrame gui ) {
            return new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    gui.guiSetTempoRange( TempoRange.this );
                }
                {
                    putValue( Action2.NAME,  caption );
                    putValue( Action.MNEMONIC_KEY, (int)caption.charAt(0) );
//                  putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.CTRL_MASK) );
                }
            };
        };
    }
    
    static void initScheme(Scheme scheme) {
        logInfo("PulsarGui#initScheme=======================================");
        //////////////////////////////////////////////////////
        Environment env = scheme.getEnvironment();
        
        SchemeUtils.defineVar( env, new SafeProcedureN("gui-get-pane") {
            // TODO ???
            @Override
            public Object applyN(Object[] args) throws Throwable {
                logInfo("gui-get-pane");
                return getCurrent().userPane;
            }
        }, "gui-get-pane");
        SchemeUtils.defineVar( env, new SafeProcedureN("gui-get-frame") {
            // TODO ???
            @Override
            public Object applyN(Object[] args) throws Throwable {
                logInfo("gui-get-frame");
                return getCurrent().frame;
            }
        }, "gui-get-frame");
        SchemeUtils.defineVar( env, new SafeProcedureN("gui-set-progress-pos") {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                if ( 0 < args.length ) {
                    double value = SchemeUtils.toDouble(args[0]);
                    getCurrent().pb_position.setValue((int) (value * PB_POSITION_MAX) );
                }
                return Invokable.NO_RESULT;
            }
        }, "gui-set-progress-pos");
        SchemeUtils.defineVar( env, new SafeProcedureN("gui-clear") {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                getCurrent().guiClear();
                return Invokable.NO_RESULT;
            }
        }, "gui-clear");
        SchemeUtils.defineVar( env, new SafeProcedureN("gui-divider-location") {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                ArrayList<Object> argList = new ArrayList<Object>( Arrays.asList( args ) );
                if ( 1 == argList.size() ) {
                    Object object = argList.get(0);
                    if ( object instanceof JSplitPane ) {
                        JSplitPane pane = (JSplitPane) object;
                        return SchemeUtils.toSchemeNumber( pane.getDividerLocation() );
                    } else {
                        return SchemeUtils.toSchemeNumber( -1 );
                    }
                } else if ( 2 == argList.size() ) {
                    Object object = argList.get(0);
                    if ( object instanceof JSplitPane ) {
                        JSplitPane pane = (JSplitPane) object;
                        int location = SchemeUtils.toInteger( argList.get(1) );
                        pane.setDividerLocation( location );
                        pane.revalidate();
                        return SchemeUtils.toSchemeNumber( pane.getDividerLocation() );
                    } else {
                        return SchemeUtils.toSchemeNumber( -1 );
                    }

                } else {
                    throw new RuntimeException( 
                            "Invalid argument error\n"+
                            "usage : (gui-divider-location! [pane])" );
                }
            }
        }, "gui-divider-location");

        SchemeUtils.defineVar( env, new SafeProcedureN("gui-frame-height") {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                PulsarFrame pulsarGui = getCurrent();
                Dimension size = pulsarGui.frame.getSize();
                if ( 0 == args.length ) {
                    
                } else {
                    size.height = SchemeUtils.toInteger(args[0]);
                    pulsarGui.frame.setSize(size);
                    pulsarGui.frame.revalidate();
                }
                return SchemeUtils.toSchemeNumber( size.height );
            }
            
        }, "gui-frame-height");
        SchemeUtils.defineVar( env, new SafeProcedureN("gui-frame-width") {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                PulsarFrame pulsarGui = getCurrent();
                Dimension size = pulsarGui.frame.getSize();
                if ( 0 == args.length ) {
                    
                } else {
                    size.width = SchemeUtils.toInteger(args[0]);
                    pulsarGui.frame.setSize(size);
                    pulsarGui.frame.revalidate();
                }
                return SchemeUtils.toSchemeNumber( size.width );
            }
            
        }, "gui-frame-width");
        SchemeUtils.defineVar( env, new SafeProcedureN("gui-frame-left") {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                PulsarFrame pulsarGui = getCurrent();
                Point pos = pulsarGui.frame.getLocation();
                if ( 0 == args.length ) {
                } else {
                    pos.x = SchemeUtils.toInteger(args[0]);
                    pulsarGui.frame.setLocation( pos);
                    pulsarGui.frame.revalidate();
                }
                return SchemeUtils.toSchemeNumber( pos.x );
            }
            
        }, "gui-frame-left");
        SchemeUtils.defineVar( env, new SafeProcedureN("gui-frame-top") {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                PulsarFrame pulsarGui = getCurrent();
                Point pos = pulsarGui.frame.getLocation();
                if ( 0 == args.length ) {
                } else {
                    pos.y = SchemeUtils.toInteger(args[0]);
                    pulsarGui.frame.setLocation( pos);
                    pulsarGui.frame.revalidate();
                }
                return SchemeUtils.toSchemeNumber( pos.y );
            }
            
        }, "gui-frame-top");

        SchemeUtils.defineVar( env, new SafeProcedureN("gui-frame-divider-position") {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                PulsarFrame pulsarGui = getCurrent();
                ArrayList<Object> argList = new ArrayList<Object>( Arrays.asList( args ) );
                if ( 0 == argList.size() ) {
                    if ( pulsarGui.rootPane instanceof JSplitPane ) {
                        JSplitPane pane = (JSplitPane) pulsarGui.rootPane;
                        return SchemeUtils.toSchemeNumber( pane.getDividerLocation() );
                    } else {
                        return SchemeUtils.toSchemeNumber( -1 );
                    }
                } else if ( 1 == argList.size() ) {
                    if ( pulsarGui.rootPane instanceof JSplitPane ) {
                        JSplitPane pane = (JSplitPane) pulsarGui.rootPane;
                        int location = SchemeUtils.toInteger( argList.get(0) );
                        pane.setDividerLocation( location );
                        pulsarGui.frame.revalidate();
                        return SchemeUtils.toSchemeNumber( pane.getDividerLocation() );
                    } else {
                        return SchemeUtils.toSchemeNumber( -1 );
                    }
                } else {
                    throw new RuntimeException( 
                            "Invalid argument error\n"+
                            "usage : (gui-panel-divider-position! [pane])" );
                }
            }
        }, "gui-frame-divider-position");

        SchemeUtils.defineVar( env, new SafeProcedureN("gui-insert-text") {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                StringBuilder sb = new StringBuilder();
                for ( Object o : args ) {
                    sb.append( o.toString() ).append( " " );
                }
                getCurrent().frame.getKawapad().insertText( sb.toString().trim() );
                return Invokable.NO_RESULT;
            }
        }, "gui-insert-text");
    }
    
    
    //Create the "cards".
    JComponent rootPane; 
    JPanel staticPaneOuter;
    JScrollPane userPaneOuter;
    transient boolean isComboBoxUpdating = false;
    JComboBox<String> cb_relatedFiles;
    JTextField tf_currentFile;
    JSlider sl_tempoSlider;
    
    public void quit() {
        super.quit();
    }

    public void guiClear() {
        userPane.removeAll();
        PulsarFramePackage.guiFlowLayout( userPane );
        frame.invalidate();
        frame.revalidate();
        // frame.pack();
    }

    public void guiSetTempoRange( PulsarFrame.TempoRange tempoRange ) {
        this.sl_tempoSlider.setMaximum( tempoRange.max );
        this.sl_tempoSlider.setMinimum( tempoRange.min );
    }
    
    
    public final Action NEW_SCRATCHPAD = new AbstractAction() {
        @SuppressWarnings("unused")
        @Override
        public void actionPerformed(ActionEvent e) {
            try {
                KawapadFrame scratchPad = frame.getKawapad().createKawapadFrame( null );
            } catch (IOException e1) {
                logError( "", e1 );
            }
        }
        {
            putValue( Action2.NAME, "New Scratchpad" );
            putValue( Action.MNEMONIC_KEY, (int)'n' );
            putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_F4, ActionEvent.SHIFT_MASK) );
        }
    };


    public final Action RESET_SEQUENCER = new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
            pulsar.close();
        }
        {
            putValue( Action2.NAME, "Reset the Sequencer" );
            putValue( Action.MNEMONIC_KEY, (int)'r' );
            putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_F5, 0 ) );
        }
    };

    public final Action QUIT_SEQUENCER = new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
            quit();
        }
        {
            putValue( Action2.NAME, "Quit" );
            putValue( Action.MNEMONIC_KEY, (int)'q' );
            putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_Q, ActionEvent.CTRL_MASK) );
        }
    };

    public final Action TOGGLE_PLAYING_ACTION = new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
            pulsar.togglePlaying();
        }
        {
            putValue( Action2.NAME, "Play/Stop" );
            putValue( Action.MNEMONIC_KEY, (int)'P' );
            putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_SPACE, ActionEvent.CTRL_MASK) );
        }
    };
    public final Action RESET_PLAYING_ACTION = new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
            pulsar.rewind();
        }
        {
            putValue( Action2.NAME, "Reset" );
            putValue( Action.MNEMONIC_KEY, (int)'R' );
            putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_HOME, ActionEvent.CTRL_MASK) );
        }
    };
    public final Action TAP_TEMPO_ACTION = new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
            pulsar.tempoTapper.tap();
        }
        {
            putValue( Action2.NAME, "Tap Tempo" );
            putValue( Action.MNEMONIC_KEY, (int)'T' );
            putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_SPACE, ActionEvent.CTRL_MASK | ActionEvent.SHIFT_MASK ) );
        }
    };
    
    
    
    // THIS IS NOT WORKING. (Tue, 06 Aug 2019 10:04:23 +0900)
    // This doesn't work and I don't know why. It maybe because GTK thing. 
    void initGuiIcon() {
        try  {
            ArrayList<Image> list = new ArrayList<>();
            list.add( ImageIO.read( PulsarFrame.class.getResource("pulsar-32x32.png")));
            list.add( ImageIO.read( PulsarFrame.class.getResource("pulsar-64x64.png")));
            list.add( ImageIO.read( PulsarFrame.class.getResource("pulsar-500x500.png")));
            list.add( ImageIO.read( PulsarFrame.class.getResource("pulsar-512x512.png")));
            this.setIconImages(list);
            //              JFrame.setDefaultLookAndFeelDecorated(true);
        } catch ( Exception e ) {
            logError("failed to load icon", e );
        }
    }
    
    // INIT_03
    /**
     * This method is called whenever the frame is created.
     */
    @Override
    public void init(){
        super.init();
    }
    
    @Override
    public void dispose() {
        super.dispose();
        PulsarFrame.unregisterLocalSchemeInitializers( kawapad.getSchemeSecretary(), PulsarFrame.this );
        if ( shutdownWhenClose )
            pulsar.shutdown();
    }
    
    void initGuiMenu() {
        //          JMenuBar menuBar = new JMenuBar();
        //          setJMenuBar(menuBar);
        
        JMenuBar menuBar = getJMenuBar();
        
        {
            JMenu m = menuBar.getMenu(0); // File
            m.addSeparator();
            m.add( new JMenuItem( QUIT_SEQUENCER ) );
        }
        
        {
            JMenu m = new JMenu( "Sequencer" );
            m.setMnemonic( 's' );
            
            m.add( new JMenuItem( TOGGLE_PLAYING_ACTION ) );
            m.add( new JMenuItem( RESET_PLAYING_ACTION ) );
            m.add( new JMenuItem( TAP_TEMPO_ACTION ) );
            m.addSeparator();
            m.add( new JMenuItem( RESET_SEQUENCER ) );
            
            
            //              m.add( new JMenuItem( NEW_SCRATCHPAD ) );
            //              m.add( new JMenuItem( SET_MAIN_FILE_ACTION ) );
            //              m.add( new JMenuItem( CLEAR_MAIN_FILE_ACTION ) );
            //              m.add( new JMenuItem( EDIT_SCRATCHPAD ) );
            
            menuBar.add( m );
        }
        {
            JMenu m = new JMenu( "View" );
            m.setMnemonic( 'v' );
            
            {
                JMenu mm = new JMenu( "Tempo Range" );
                for ( PulsarFrame.TempoRange r :  TempoRange.values() ) {
                    mm.add( new JMenuItem( r.createAction( PulsarFrame.this ) ) );
                }
                m.add( mm );
            }
            
            menuBar.add( m );
        }
        
        Action2.processMenuBar(menuBar);
    }
    
    @Override
    protected void onCloseWindow() {
        pulsar.close();
        //          System.exit(0);
    }
    
    JComponent pulsarRootPane;
    JPanel staticPane;
    JNamedPanel userPane;
    void initGui() {
        //          this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setDefaultCloseOperation( JFrame.DO_NOTHING_ON_CLOSE );
        
        staticPane = new JPanel() {
            @Override
            public Dimension getPreferredSize() {
                return new Dimension(500,180);
            }
        };
        
        PulsarFramePackage.guiBorderLayout( staticPane );
        
        JPanel staticPaneOuter = staticPane; 
        
        userPane   = new JNamedPanel() {
            @Override
            public Dimension getMinimumSize() {
                return new Dimension(0,0);
            }
        };
        userPane.setLayout(new FlawLayout());
        
        //Create and set up the content pane.
        JScrollPane userPaneOuter = new JScrollPane( userPane );
        //          userPaneOuter.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED );
        //          userPaneOuter.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED );
        userPaneOuter.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER );
        userPaneOuter.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER );
        // SEE TAG_PACK_TWICE
        
        pulsarRootPane  = new JPanel( new BorderLayout() );
        pulsarRootPane.add( staticPaneOuter, BorderLayout.PAGE_START);
        pulsarRootPane.add( userPaneOuter, BorderLayout.CENTER );
        pulsarRootPane.setMaximumSize( new Dimension( 500, 400 ));
        
        this.scratchPadRoot.remove( this.scrollPane );
        this.scratchPadRoot.add( this.scrollPane, BorderLayout.CENTER );
        this.scratchPadRoot.add( this.pulsarRootPane, BorderLayout.PAGE_START );
        this.scratchPadRoot.revalidate();
        
        // Tempo Button
        staticPane.add( new JPusarFilePanel(), BorderLayout.PAGE_START );
        staticPane.add( createStartStopButton(), BorderLayout.LINE_END );
        staticPane.add( createTempoTapButton(), BorderLayout.CENTER );
        staticPane.add( createRewindButton(), BorderLayout.LINE_START );
        
        
        //          REMOVED >>> (Mon, 08 Jul 2019 22:06:16 +0900)
        //          staticPane.add( createCueButton(), BorderLayout.PAGE_END );
        //          REMOVED <<< (Mon, 08 Jul 2019 22:06:16 +0900)
        
        // createEmptyBorder( top left bottom right )
        //              staticPane.setBorder( BorderFactory.createEmptyBorder(10,20,5,20) );
        //              userPane.setBorder(   BorderFactory.createEmptyBorder(5,20,20,20) );
        
        //              ((JComponent)pulsarRootPane).setBorder( BorderFactory.createEmptyBorder() );
        ((JComponent)pulsarRootPane).setBorder( BorderFactory.createEmptyBorder(10,10,10,10) );
        
        // frame.setMaximizedBounds(new Rectangle(0, 0, 400, 1000));
        this.pack();
        this.setSize( 750, 750 );
        this.setVisible(true);
        this.addComponentListener( new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent e) {
                JRootPane pane = PulsarFrame.this.getRootPane();
                for (Component c : pane.getComponents() ) {
                    c.revalidate();
                }
                pane.revalidate();
                pane.validate();
                
            }
        });
        
        PulsarFrame.this.rootPane = pulsarRootPane;
        PulsarFrame.this.staticPane = staticPane;
        PulsarFrame.this.userPane = userPane;
        PulsarFrame.this.staticPaneOuter = staticPaneOuter;
        PulsarFrame.this.userPaneOuter = userPaneOuter;
    }
    
    JProgressBar pb_position = new JProgressBar() {
        @Override
        public Dimension getPreferredSize() {
            Dimension s = super.getPreferredSize();
            s.height = 40;
            return s;
        }
    };
    void initGuiProgressBar() {
        PulsarFrame.this.pb_position = pb_position; 
        add( pb_position , BorderLayout.PAGE_END );
        pb_position.setBorder( BorderFactory.createCompoundBorder(
            BorderFactory.createEmptyBorder(10,10,10,10),
            pb_position.getBorder()) );
        //              pb_position.setBorder( BorderFactory.createEmptyBorder(0,0,0,0) );
        pb_position.setMaximum( PB_POSITION_MAX );
        pb_position.setMinimum(0);
    }
    
    void initGuiFocus() {
        SwingUtilities.invokeLater( new Runnable() {
            @Override
            public void run() {
                kawapad.requestFocus();
            }
        });
    }

    /*
     * === Meaning of these variable name for panels ===
     * ex) panel_p0_1_2
     * These numbers denote its position.
     *    ___________________
     *    |                 |
     *    |        1        |
     *    |_________________|
     *    |   |         |   |
     *    | 4 |    0    | 2 |
     *    |___|_________|___|
     *    |                 |
     *    |        3        |
     *    |_________________|
     * 
     * The variable name "panel_p0_1_2" denotes that 
     * it is the panel on the position No.2 inside the
     * panel on the position No.1 inside the panel on 
     * the position No.0 .
     * 
     */


    // XXX REMOVE THIS
    private class JPusarFilePanel extends JPanel {
        public JPusarFilePanel() {
            super( PulsarFramePackage.newLayout() );
        }
                
        JSliderPanel panel_slider = new JSliderPanel();
        {
//          add( panel_slider, BorderLayout.PAGE_END );
            add( panel_slider, BorderLayout.CENTER );
        }
        class JSliderPanel extends JPanel {
            public JSliderPanel() {
                super( PulsarFramePackage.newLayout() );
            }
            
            JSlider sl_tempoSlider = new JSlider();
            {
                PulsarFrame.this.sl_tempoSlider = sl_tempoSlider;
//              add( sl_tempoSlider, BorderLayout.PAGE_END );
                add( sl_tempoSlider, BorderLayout.CENTER );
                sl_tempoSlider.setBorder( BorderFactory.createEmptyBorder() );
                sl_tempoSlider.setMinimum(1);
                sl_tempoSlider.setMaximum(1000);
                sl_tempoSlider.setPaintTicks(true);
                sl_tempoSlider.setPaintTrack( true);
                sl_tempoSlider.setMajorTickSpacing(100);
                sl_tempoSlider.setMinorTickSpacing(25);
                sl_tempoSlider.setPreferredSize( new Dimension(500, 80));
                Dictionary<Integer,JLabel> labelTables = new Hashtable<>();
                labelTables.put(10, new JLabel( "10" ));
                labelTables.put(50, new JLabel( "50" ));
                labelTables.put(100, new JLabel( "100" ));
                labelTables.put(150, new JLabel( "150" ));
                labelTables.put(200, new JLabel( "200" ));
                labelTables.put(300, new JLabel( "300" ));
                labelTables.put(400, new JLabel( "400" ));
                labelTables.put(500, new JLabel( "500" ));
                labelTables.put(750, new JLabel( "750" ));
                labelTables.put(1000, new JLabel( "1000" ));

                sl_tempoSlider.setLabelTable( labelTables );
                sl_tempoSlider.setPaintLabels(true);
                sl_tempoSlider.addChangeListener(new ChangeListener() {
                    @Override
                    public void stateChanged(ChangeEvent e) {
                        try {
                            //                      logInfo( "TempoSlider : " + ((JSlider)e.getSource()).getValue() );
                            pulsar.tempoTapper.setBeatsPerMinute( ((JSlider)e.getSource()).getValue() );
                        } catch (JackException e1) {
                            logError("", e1);
                        }
                    }
                });
                
                sl_tempoSlider.setBorder(
                        BorderFactory.createCompoundBorder(
                                BorderFactory.createTitledBorder("TEMPO"),
                                sl_tempoSlider.getBorder()
                                )
                        );
                
                // InitializeTempoSlider
                pulsar.tempoTapper.registerNotifier( new TempoTapperTempoNotifier() {
                    @Override
                    public void notifyTempo(double beatPerMinute) {
                        sl_tempoSlider.setValue( (int)beatPerMinute );
                    }
                });
                
                //
                guiSetTempoRange( TempoRange.NORMAL );
            }
        }
    }

    void initializeTempoSlider() {
    }

    private JButton createStartStopButton() {
        JButton b = new JButton( "■|▶" ) {
            @Override
            public Dimension getPreferredSize() {
                Dimension s = super.getPreferredSize();
                s.width =  75;
                return s;
            }
        };
        b.addActionListener( new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                pulsar.togglePlaying();
            }
        });
        return b;
    }
    
    private JButton createRewindButton() {
        JButton b = new JButton( "||◀" ) {
            @Override
            public Dimension getPreferredSize() {
                Dimension s = super.getPreferredSize();
                s.width =  75;
                return s;
            }
        };
        b.addActionListener( new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                pulsar.rewind();
            }
        });
        return b;
    }
    
    private JButton createTempoTapButton() {
        JButton tempoTapButton = new JButton( "TEMPO" );
        tempoTapButton.addActionListener( new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                pulsar.tempoTapper.tap();
            }
        });

        pulsar.tempoTapper.registerNotifier( new TempoTapperTempoNotifier() {
            @Override
            public void notifyTempo(double beatPerMinute) {
                tempoTapButton.setText( String.format( "Tempo=%.2f", beatPerMinute  ) );
            }
        } );

        tempoTapButton.setPreferredSize(new Dimension(200, 100));
        tempoTapButton.setMargin(new Insets(20, 20, 20, 20));

        return tempoTapButton;
    }
}

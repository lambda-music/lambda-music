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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.io.IOException;
import java.io.InputStream;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.logging.Level;

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
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import gnu.mapping.Environment;
import gnu.mapping.Values;
import kawa.standard.Scheme;
import kawapad.KawapadFrame;
import lamu.lib.app.ApplicationComponent;
import lamu.lib.log.Logger;
import lamu.lib.log.SimpleConsole;
import lamu.lib.scheme.EvaluatorReceiver;
import lamu.lib.scheme.SchemeEngine;
import lamu.lib.scheme.SchemeEvaluator.SchemeEngineListener;
import lamu.lib.scheme.SchemeResult;
import lamu.lib.scheme.SchemeUtils;
import lamu.lib.secretary.Invokable;
import lamu.lib.swing.AcceleratorKeyList;
import lamu.lib.swing.Action2;
import lamu.lib.swing.AutomatedActionField;
import lamu.utils.lib.FlawLayout;
import lamu.utils.lib.JNamedPanel;
import lamu.utils.lib.PulsarGuiUtils;
import lamu.utils.lib.PulsarSharedTimer;

public class PulsarFrame extends KawapadFrame implements ApplicationComponent {
    private static final String PULSAR_DEFAULT_CAPTION = "Pulsar - a Lisp Scheme Music Sequencer";
    private static final boolean ENABLED_USER_PANE = true; // (Fri, 27 Sep 2019 12:18:01 +0900)
    private static final boolean ENABLED_TEMPO_TITLE = false; // (Fri, 27 Sep 2019 12:18:01 +0900)
    
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }

    /////////////////////////////////////////////////////

    private ApplicationComponent parentApplicationComponent;
    @Override
    public ApplicationComponent getParentApplicationComponent() {
        return this.parentApplicationComponent;
    }
    @Override
    public void setParentApplicationComponent(ApplicationComponent parentApplicationComponent) {
        this.parentApplicationComponent = parentApplicationComponent;
    }

    @Override
    public void processInit() {
        super.processInit();
        initPulsarGui();
    }
    
    boolean quitProcessed = false;
    @Override
    public synchronized void processQuit() {
        if ( quitProcessed )
            return;
        quitProcessed = true;
        
        if ( this.timerHandle != null ) {
            this.timerHandle.run();
        }
        
        this.getKawapad().evaluate( "(close)", EvaluatorReceiver.REPORT_ERROR );
//        pulsar.close();
        
        super.processQuit();
        // System.exit(0);
    }

    /////////////////////////////////////////////////////

    PulsarFrame frame = this;
    
    static final int PB_POSITION_MAX = 1024;
    
    public static void registerGlobalSchemeInitializers( SchemeEngine schemeEngine ) {
        schemeEngine.getEvaluatorManager().getPrimaryEvaluator().registerSchemeInitializer( new SchemeEngineListener() {
            @Override
            public void execute( Scheme scheme ) {
                initScheme( scheme );
            }
        });
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
    
    public static PulsarFrame create(
            SchemeEngine schemeEngine,
            boolean shutdownWhenClose,
            String caption ) {
        return new PulsarFrame( schemeEngine, shutdownWhenClose, caption );
    }


    PulsarFrame( SchemeEngine schemeEngine, boolean shutdownWhenClose, String caption ) {
        super( schemeEngine, shutdownWhenClose, caption == null ? PULSAR_DEFAULT_CAPTION : caption );
        
        //          DELETED >>> INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
        //          PulsarGui.invokeLocalSchemeInitializers( schemeSecretary, PulsarGui.this );
        //          DELETED <<< INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
//      this.kawapad.getThreadInitializerCollection().addThreadInitializer( this.pulsar.getThreadInitializer() );
        
        initGui();
        initGuiMenu();
        initGuiIcon();
        initGuiProgressBar();
        initGuiFocus();
//        initPulsarGui();
        
        AcceleratorKeyList.processAcceleratorKeys( this.getRootPane() );
    }

    /* 
     * This may be deprecated in a near future date.
     * (Sun, 12 Apr 2020 06:50:00 +0900)  
     */
    private final PulsarGui gui = new PulsarGui(this);
    public PulsarGui getGui() {
        return gui;
    }

    Runnable timerHandle=null;
    private void initPulsarGui() {
        Invokable invokable2 = new Invokable() {
            transient int counter = 0;
            transient double lastPosition = 0.0d;
            transient double position = 0.0d;
            transient double velo = 0.0d;

            void setValue( double p ) {
                while ( 0<=p && 1 < p ) {
                    p = p - 1;
                }
                pb_position.setValue((int) (p * PulsarFrame.PB_POSITION_MAX) );
                pb_position.repaint();
                pb_position.revalidate();
            }

            @Override
            public Object invoke(Object... args) {
                counter ++;
                if ( 100 < counter ) {
                    counter = 0;
                    String schemeScript = "(if (open?) (get-track-position (get-main-track)) #f)";
                    EvaluatorReceiver receiver = new EvaluatorReceiver() {
                        @Override
                        public void receive(SchemeResult schemeResult) {
                            if ( schemeResult.isSucceeded() ) {
                                String v = schemeResult.getValueAsString();
                                if ( "#f".equals( v ) ) {
                                    //
                                } else {
                                    lastPosition = position;
                                    position = Double.parseDouble( v );

                                    double positionEx;
                                    if ( position < lastPosition ) {
                                        positionEx = position + Math.ceil( lastPosition );
                                    } else {
                                        positionEx = position;
                                    }
                                    velo = (positionEx - lastPosition) / 100;
                                }
                            } else {
//                            	SimpleConsole.getConsole().addText( schemeResult.getError());
                            }
                        }
                    };

                    Runnable runnable = Logger.temporaryDisable(
                            SchemeEngine.createEvaluationRunner( 
                                    kawapad.getThreadInitializerCollection(), 
                                    schemeScript, 
                                    getKawapad().getSchemeEngine().getEvaluatorManager().getCurrentEvaluator(), 
                                    receiver, 
                                    kawapad.getCurrentDirectory(), 
                                    kawapad.getCurrentFile(), 
                                    "scratchpad" ));
                    runnable.run();
                }
                double p = position + ( velo * (double)counter);
//                System.err.println( "position:" + p );
                setValue( p );

                return Values.empty;
            }
        };
        this.timerHandle = PulsarSharedTimer.createTimer( getKawapad().getThreadInitializerCollection(), 5000, 20, invokable2 );
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
                    putValue( Action2.CAPTION,  caption );
                    putValue( Action.MNEMONIC_KEY, (int)caption.charAt(0) );
//                  putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.CTRL_MASK) );
                }
            };
        };
    }

    transient boolean updatingTempoDisplay = false;
    transient boolean updatingTempoDisplay_slider = false;
    transient boolean updatingTempoDisplay_button = false;
    protected synchronized void setTempoDisplay( double bpm ) {
        if ( updatingTempoDisplay )
            return;

        try {
            updatingTempoDisplay = true;

            if ( ! updatingTempoDisplay_button && this.tapTempoButton != null ) {

                try {
                    this.tapTempoButton.setText( String.format( "Tempo=%.2f", bpm ) );
                } catch ( Throwable t ) {
                    logError( "ignored", t );
                }
            }

            if ( ! updatingTempoDisplay_slider && this.sl_tempoSlider != null ) {
                try {
                    this.sl_tempoSlider.setValue( (int)bpm );
                } catch ( Throwable t ) {
                    logError( "ignored", t );
                }
            }
        } finally {
            updatingTempoDisplay = false;
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    static void initScheme(Scheme scheme) {
        logInfo("PulsarGui#initScheme=======================================");
        //////////////////////////////////////////////////////
        Environment env = scheme.getEnvironment();
    }
    
   
    //Create the "cards".
    JComponent rootPane; 
    JPanel staticPaneOuter;
    JScrollPane userPaneOuter;
    transient boolean isComboBoxUpdating = false;
    JComboBox<String> cb_relatedFiles;
    JTextField tf_currentFile;
    transient JSlider sl_tempoSlider;
    
    public void requestClose() {
        super.requestClose();
    }

    public void guiClear() {
        userPane.removeAll();
        PulsarGuiUtils.guiFlowLayout( userPane );
        frame.invalidate();
        frame.revalidate();
        // frame.pack();
    }

    public void guiSetTempoRange( PulsarFrame.TempoRange tempoRange ) {
        this.sl_tempoSlider.setMaximum( tempoRange.max );
        this.sl_tempoSlider.setMinimum( tempoRange.min );
    }
    
    @AutomatedActionField
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
            putValue( Action2.CAPTION, "New Scratchpad" );
            putValue( Action.MNEMONIC_KEY, (int)'n' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "shift F4" );
        }
    };

    @AutomatedActionField
    public final Action RESET_SEQUENCER = new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
            // (Sun, 15 Dec 2019 19:26:48 +0900) INDIRECT_PULSAR_ACCESS
            getKawapad().evaluate( "(close)", false, false, false );
            // pulsar.close();
        }
        {
            putValue( Action2.CAPTION, "Reset the Sequencer" );
            putValue( Action.MNEMONIC_KEY, (int)'r' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "F5" );
        }
    };

    @AutomatedActionField
    public final Action QUIT_SEQUENCER = new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
            requestClose();
        }
        {
            putValue( Action2.CAPTION, "Quit" );
            putValue( Action.MNEMONIC_KEY, (int)'q' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl Q" );
        }
    };

    @AutomatedActionField
    public final Action TOGGLE_PLAYING_ACTION = new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
//            pulsar.togglePlaying();
            getKawapad().evaluate( "(set-playing)", EvaluatorReceiver.REPORT_ERROR ); 
        }
        {
            putValue( Action2.CAPTION, "Play/Stop" );
            putValue( Action.MNEMONIC_KEY, (int)'P' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "alt ENTER" );
        }
    };
    @AutomatedActionField
    public final Action RESET_PLAYING_ACTION = new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
//            pulsar.rewind();
            getKawapad().evaluate( "(rewind)", EvaluatorReceiver.REPORT_ERROR ); 
        }
        {
            putValue( Action2.CAPTION, "Reset" );
            putValue( Action.MNEMONIC_KEY, (int)'R' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "alt HOME" );
        }
    };
    @AutomatedActionField
    public final Action TAP_TEMPO_ACTION = new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
//            pulsar.getTempoTapper().tap();
            getKawapad().evaluate( "(tap-tempo)", new EvaluatorReceiver() {
                @Override
                public void receive(SchemeResult schemeResult) {
                	if ( schemeResult.isSucceeded() ) {
                		double bpm = Double.parseDouble( schemeResult.getValueAsString() );
                		if ( 0<=bpm ) {
                			setTempoDisplay( bpm );
                		}
                	} else {
                    	SimpleConsole.getConsole().addText( schemeResult.getError());
                	}
                }
            } ); 
        }
        {
            putValue( Action2.CAPTION, "Tap Tempo" );
            putValue( Action.MNEMONIC_KEY, (int)'T' );
            AcceleratorKeyList.putAcceleratorKeyList( this, "alt ESCAPE" );
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
    
    
    @Override
    public void dispose() {
        super.dispose();
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
                    Action action = r.createAction( PulsarFrame.this );
                    mm.add( new JMenuItem( action ) );
                    AcceleratorKeyList.addAction( frame.getRootPane(), action ); 
                }
                m.add( mm );
            }
            
            menuBar.add( m );
        }
        
        Action2.processMenuBar(menuBar);
    }
    
    JComponent pulsarRootPane;
    JPanel staticPane;
    JNamedPanel userPane;
    transient JButton tapTempoButton;
    void initGui() {
        // this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setDefaultCloseOperation( JFrame.DO_NOTHING_ON_CLOSE );
        
        staticPane = new JPanel() {
            @Override
            public Dimension getPreferredSize() {
                return new Dimension(500,110);
            }
        };
//        staticPane.setBorder( BorderFactory.createEmptyBorder() );
        
        PulsarGuiUtils.guiBorderLayout( staticPane );
        
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
        if ( ENABLED_USER_PANE ) 
            pulsarRootPane.add( userPaneOuter, BorderLayout.CENTER );
        pulsarRootPane.setMaximumSize( new Dimension( 500, 400 ));
        
        this.scratchPadRoot.remove( this.scrollPane );
        this.scratchPadRoot.add( this.scrollPane, BorderLayout.CENTER );
        this.scratchPadRoot.add( this.pulsarRootPane, BorderLayout.PAGE_START );
        this.scratchPadRoot.revalidate();
        
        // Tempo Button
        staticPane.add( createFilePanel(),  BorderLayout.PAGE_START );
        staticPane.add( createStartStopButton(), BorderLayout.LINE_END );
        this.tapTempoButton = createTapTempoButton();
        staticPane.add( tapTempoButton, BorderLayout.CENTER );
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
            s.height = 10;
            return s;
        }
    };
    void initGuiProgressBar() {
        PulsarFrame.this.pb_position = pb_position; 
        add( pb_position , BorderLayout.PAGE_END );
//        pb_position.setBorder( BorderFactory.createCompoundBorder(
//            BorderFactory.createEmptyBorder(10,10,10,10),
//            pb_position.getBorder()) );
        pb_position.setBorder( BorderFactory.createEmptyBorder(0,0,0,0) );
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


    JPusarFilePanel createFilePanel() {
        return new JPusarFilePanel();
    }

    // XXX REMOVE THIS
    private class JPusarFilePanel extends JPanel {
        public JPusarFilePanel() {
            super( PulsarGuiUtils.newLayout() );
        }
                
        JSliderPanel panel_slider = new JSliderPanel();
        {
//          add( panel_slider, BorderLayout.PAGE_END );
            add( panel_slider, BorderLayout.CENTER );
        }
        class JSliderPanel extends JPanel {
            public JSliderPanel() {
                super( PulsarGuiUtils.newLayout() );
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
                sl_tempoSlider.setMajorTickSpacing(50);
                sl_tempoSlider.setMinorTickSpacing(25);
                sl_tempoSlider.setPreferredSize( new Dimension(500, 50));
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
                sl_tempoSlider.setPaintLabels( true );
                if ( ENABLED_TEMPO_TITLE )
                    sl_tempoSlider.setBorder( BorderFactory.createTitledBorder("TEMPO") );
                
                //
                guiSetTempoRange( TempoRange.NORMAL );

                /*
                 * (Fri, 10 Jan 2020 11:09:27 +0900)
                 * 
                 *  Setting a change listener to a slider should be done after setting its value;
                 *  otherwise the listener will be invoked and it causes the script to be executed
                 *  before the Scheme engine is initialized. 
                 */
                sl_tempoSlider.addChangeListener( new ChangeListener() {
                    @Override
                    public void stateChanged( ChangeEvent e ) {
                        
                        PulsarFrame pulsarFrame = PulsarFrame.this;
                        synchronized ( pulsarFrame ) {
                            try {
                                pulsarFrame.updatingTempoDisplay_slider = true;
                                int value = ((JSlider)e.getSource()).getValue();
                                pulsarFrame.setTempoDisplay( value );
                                pulsarFrame.getKawapad().evaluate( "(set-tempo " + value + ")", EvaluatorReceiver.REPORT_ERROR );
                            } finally {
                                pulsarFrame.updatingTempoDisplay_slider = false;
                            }
                        }
                    }
                });
            }
        }
    }

    void initializeTempoSlider() {
    }

    private JButton createStartStopButton() {
        JButton b = new JButton( "[]>>" ) {
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
//                pulsar.togglePlaying();
                kawapad.evaluate( "", false, false, false );
            }
        });
        return b;
    }
    
    private JButton createRewindButton() {
        JButton b = new JButton( "||<<" ) {
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
                // INDIRECT_PULSAR_ACCESS (Sun, 15 Dec 2019 19:26:48 +0900) >>>
                // pulsar.rewind();
                getKawapad().evaluate( "(rewind)", false, false, false );
                // (Sun, 15 Dec 2019 19:26:48 +0900) <<<

            }
        });
        return b;
    }


    private JButton createTapTempoButton() {
        JButton tapTempoButton = new JButton( "TEMPO" );
        tapTempoButton.addActionListener( TAP_TEMPO_ACTION );
        tapTempoButton.setPreferredSize(new Dimension(200, 100));
        tapTempoButton.setMargin(new Insets(20, 20, 20, 20));
        return tapTempoButton;
    }
}

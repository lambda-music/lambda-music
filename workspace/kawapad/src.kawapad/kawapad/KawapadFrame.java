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
import java.awt.Container;
import java.awt.Dimension;
import java.awt.HeadlessException;
import java.awt.MenuBar;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import lamu.lib.ForceLoadingClass;
import lamu.lib.Version;
import lamu.lib.apps.ApplicationComponent;
import lamu.lib.apps.ApplicationVessel;
import lamu.lib.evaluators.MultiplexEvaluator;
import lamu.lib.helps.LamuAbstractDocument;
import lamu.lib.log.LogFormatter;
import lamu.lib.log.Logger;
import lamu.lib.swing.AcceleratorKeyList;
import lamu.lib.swing.Action2;

public class KawapadFrame extends JFrame implements ApplicationComponent {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    
    //////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////

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
        this.kawapad.processInit();
    }
    
    boolean quitProcessed  = false;
    @Override
    public synchronized void processQuit() {
        if ( quitProcessed )
            return;
        quitProcessed = true;
        
        KawapadFrame.this.kawapad.processQuit();
        KawapadFrame.this.setVisible(false);
        SwingUtilities.invokeLater( new Runnable( ) {
            @Override
            public void run() {
                KawapadFrame.this.dispose();
            }
        });
    }

    //////////////////////////////////////////////////////////////////////////////////////////
    //
    // getCurrent (the second generation) (Wed, 03 Jun 2020 17:00:46 +0900)
    //
    //////////////////////////////////////////////////////////////////////////////////////////
    
    private static ThreadLocal<KawapadFrame> threadLocal = new ThreadLocal<KawapadFrame>();
    public static final KawapadFrame getCurrent() {
        return threadLocal.get();
    }
    static final void setCurrent( KawapadFrame kawapadFrame ) {
        threadLocal.set( kawapadFrame );
    }


    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    // Defining GUI
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    // private Scheme scheme;

    protected Kawapad kawapad;
    public Kawapad getKawapad() {
        return kawapad;
    }

    protected Container scratchPadRoot;
    protected JScrollPane scrollPane; 
    protected JMenuBar menuBar;
    @Override
    public MenuBar getMenuBar() {
        return super.getMenuBar();
    }

    public Container getScratchPadRootPane() {
        return getContentPane();
    }
    
    public final AbstractAction NEW_SCRATCHPAD_ACTION = new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
            try {
                kawapad.createKawapadFrame( null );
            } catch (IOException e1) {
                logError( "", e1 );
            }
        }
        {
            putValue( Action2.CAPTION, "Create a New Scratchpad" );
            putValue( Action.MNEMONIC_KEY, (int)'n' );
            putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_N, ActionEvent.CTRL_MASK | ActionEvent.SHIFT_MASK ) );
            AcceleratorKeyList.putAcceleratorKeyList( this, "ctrl shift N" );
        }
    };

    private boolean shutdownWhenClose=true;
    public boolean isShutdownWhenClose() {
        return shutdownWhenClose;
    }
    public void setShutdownWhenClose(boolean shutdownWhenClose) {
        this.shutdownWhenClose = shutdownWhenClose;
    }

    public KawapadFrame( 
            MultiplexEvaluator multiplexEvaluator, 
            boolean shutdownWhenClose,
            String title 
            ) throws HeadlessException 
    {
        super(title);
        this.setDefaultCloseOperation( DO_NOTHING_ON_CLOSE );
        this.setShutdownWhenClose( shutdownWhenClose );

        
//      DELETED >>> INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
//      invokeLocalSchemeInitializers( schemeSecretary, this);
//      DELETED <<< INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
        
        this.kawapad = new Kawapad( multiplexEvaluator ) {
            // Special thanks go to tips4java
            // https://tips4java.wordpress.com/2009/01/25/no-wrap-text-pane/
            public boolean getScrollableTracksViewportWidth() {
                // return getUI().getPreferredSize(this).width 
                //      <= getParent().getSize().width;
                return getUI().getPreferredSize(this).width 
                        < getParent().getSize().width;
            }
        };
        
        this.kawapad.addThreadInitializer( ()->{
            setCurrent( this );
        });
        
//      this.kawapad.getThreadInitializerCollection().addThreadInitializer( this.getThreadInitializer() );


//      Color foreground = Color.green;
//      Color background = Color.black;
//      Color selectionForeground = Color.white;
//      Color selectionBackground = Color.blue;
//
//      kawapad.setBackground( background );
//      kawapad.setForeground( foreground );
//      kawapad.setSelectedTextColor( selectionForeground );
//      kawapad.setSelectionColor( selectionBackground );
//      
        // kawapad.setFont(  Font.decode( "Bitstream Vera Sans Mono 12" ) );
        
//      int MARGIN = 5;
//      {
//          Border b = BorderFactory.createLineBorder( background, MARGIN  );
//          kawapad.setBorder( b );
//      }
        
        scrollPane = new JScrollPane( kawapad );
        scratchPadRoot = new JPanel( new BorderLayout() );
        getContentPane().add( scratchPadRoot );
        scratchPadRoot.add( scrollPane, BorderLayout.CENTER );
        
        {
            if ( false ) {
                System.err.println( "-------------------------" );
                for ( Object o : kawapad.getActionMap().getParent().allKeys() ) {
                    if ( o != null )
                        logInfo( o.toString() );
                }
                System.err.println( "==========================" );
                
                for ( Object o : kawapad.getActionMap().allKeys() ) {
                    if ( o != null )
                        logInfo( o.toString() );
                }
            }
        }
        
        //////////////////////////////////////////////////////////////////////////////////////////
        

        {
            menuBar = new JMenuBar();
            JMenu fileMenu = new JMenu( "File" );
            fileMenu.setMnemonic('f');
            menuBar.add( fileMenu );
            fileMenu.add( new JMenuItem( NEW_SCRATCHPAD_ACTION ) );
            
            JMenu editMenu = new JMenu( "Edit" );
            editMenu.setMnemonic('e');
            menuBar.add( editMenu );
            
            JMenu viewMenu = new JMenu( "View" );
            viewMenu.setMnemonic('v');
            menuBar.add( viewMenu );
            
            JMenu navigateMenu = new JMenu( "Navigate" );
            navigateMenu.setMnemonic('a');
            menuBar.add( navigateMenu );
            
            JMenu schemeMenu = new JMenu( "Scheme" );
            schemeMenu.setMnemonic('r');
            menuBar.add( schemeMenu );

            JMenu serverMenu = new JMenu( "Server" );
            serverMenu.setMnemonic('i');
            menuBar.add( serverMenu );
            
            Map<String,JMenu> map = new HashMap<>();
            map.put( "file"     , fileMenu );
            map.put( "edit"     , editMenu );
            map.put( "view"     , viewMenu );
            map.put( "navigate" , navigateMenu );
            map.put( "scheme"   , schemeMenu );
            map.put( "server"   , serverMenu );
            kawapad.initMenu( map );
            
            Action2.processMenuBar( menuBar );
            setJMenuBar( menuBar );
            
            kawapad.getMultipleEvaluatorMenuListener().getServerMenuList().add( serverMenu );
            kawapad.getMultipleEvaluatorMenuListener().notifyUpdate( kawapad.getEvaluator() );
        }

        {
            setSize( new Dimension( 640, 480 ) );
            setDefaultCloseOperation( JFrame.DO_NOTHING_ON_CLOSE );
            
            this.addWindowListener( kawapad.createCloseQuery( new Runnable() {
                @Override
                public void run() {
                    if ( KawapadFrame.this.isShutdownWhenClose() ) {
                        KawapadFrame.this.requestQuit();
                    } else {
                        KawapadFrame.this.processQuit();
                    }
                }
            }));
            
            setVisible(true);
        }
        
        SwingUtilities.invokeLater( new Runnable() {
            @Override
            public void run() {
                kawapad.requestFocus();
            }
        } );
    }
    
    public void requestClose() {
        this.dispatchEvent( new WindowEvent(this, WindowEvent.WINDOW_CLOSING ));
    }

    /**
     * Kawapad#init() must be called whenever any Kawapad instance is created. Due
     * to INIT_03 (see the comment in the source code), the constructor cannot call
     * this method directory; we decided to mandate the users to call this method
     * manually.
     */

//  ADDED >>> (Tue, 06 Aug 2019 09:29:54 +0900)
//    private volatile boolean initialized =false;
//    public synchronized void init() {
//        if ( initialized ) {
//            return;
//        }
//        this.initialized = true;
//        
////      ADDED >>> (Tue, 06 Aug 2019 08:47:14 +0900)
//        /*
//         * At that time, I didn't realize that creation of a frame should be done in a INIT_03
//         * different way from the creation of a scheme object. (Tue, 06 Aug 2019 08:47:14 +0900) 
//         */
//        // MODIFIED (Mon, 02 Sep 2019 06:16:35 +0900) >>>
//        // Calling eventhanders is done inside Kawapad 
////      Kawapad.eventHandlers.invokeEventHandler( kawaPad, EventHandlers.CREATE,  kawaPad );
//        this.kawapad.initialize();
//        // MODIFIED (Mon, 02 Sep 2019 06:16:35 +0900) <<<
////      ADDED <<< (Tue, 06 Aug 2019 08:47:14 +0900)
//    }
//  ADDED <<< (Tue, 06 Aug 2019 09:29:54 +0900)
    
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 
    // factory
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    public static KawapadFrame createStaticInstance( ) {
        MultiplexEvaluator multiplexEvaluator = MultiplexEvaluator.createLocal();
        KawapadFrame kawapadFrame = new KawapadFrame( multiplexEvaluator, true, "Scheme Scratch Pad" );
        ApplicationVessel v = new ApplicationVessel("KawapadVessel");
        v.add( kawapadFrame );
        kawapadFrame.setParentApplicationComponent( kawapadFrame );
        v.requestInit();
        return kawapadFrame;
    }
    public static void main(String[] args) throws IOException {
        System.err.println( "*** Welcome to Kawapad *** " );
        System.err.println( "VERSION : " + Version.get( KawapadFrame.class ) );
        LogFormatter.init();
        if ( 0 < args.length  ) {
            if ( args[0].equals( "--version" ) ) {
                System.out.println( Version.get( KawapadFrame.class ) );
                return;
            } else if ( args[0].equals( "--output-reference" ) ) {
                outputDocument();
            } else if ( args[0].equals( "--output-keystroke-reference" ) ) {
                outputKeyStrokeReference();
            } else {
                start( new File( args[0] ) );
            }
        } else {
            start();        
        }
    }
    
    public static void outputKeyStrokeReference() throws IOException {
        KawapadFrame kawapadFrame = createStaticInstance( );
        try {
            Thread.sleep( 2048 );
        } catch ( InterruptedException e ) {
        }
        System.out.println( kawapadFrame.getKawapad().outputKeyStrokeReference() );
        System.out.flush();
        kawapadFrame.requestClose();
    }

    public static void outputDocument() throws IOException {
        ForceLoadingClass.force( Kawapad.class );
        
        
        KawapadFrame kawapadFrame = createStaticInstance();
        try {
            Thread.sleep( 2048 );
        } catch ( InterruptedException e ) {
        }
        LamuAbstractDocument.outputReference( "kawapad-procedures", null ); 
        
        kawapadFrame.requestClose();
    }
    
    public static void start(File f ) throws IOException {
        KawapadFrame kawapadFrame = createStaticInstance( );
        if ( f != null )
            kawapadFrame.kawapad.openFile( f );
        else
            kawapadFrame.kawapad.openIntro();
    }
    public static void start() throws IOException {
        start( null );
    }
}

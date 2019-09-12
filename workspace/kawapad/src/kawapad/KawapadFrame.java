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
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

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

import kawapad.Kawapad.KawaVariableInitializer;
import pulsar.lib.PulsarLogger;
import pulsar.lib.scheme.DescriptiveDocumentCategory;
import pulsar.lib.scheme.DescriptiveHelp;
import pulsar.lib.scheme.scretary.SchemeSecretary;
import pulsar.lib.swing.Action2;

public class KawapadFrame extends JFrame {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    
    public String getFrameName() {
        return this.kawapad.getInstanceID();
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
        }
    };


    public KawapadFrame( SchemeSecretary schemeSecretary, String title ) throws HeadlessException {
        super(title);
        this.setDefaultCloseOperation( DO_NOTHING_ON_CLOSE );
        
//      DELETED >>> INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
//      invokeLocalSchemeInitializers( schemeSecretary, this);
//      DELETED <<< INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900)
        
        kawapad = new Kawapad( schemeSecretary ) {
            // Special thanks go to tips4java
            // https://tips4java.wordpress.com/2009/01/25/no-wrap-text-pane/
            public boolean getScrollableTracksViewportWidth() {
                // return getUI().getPreferredSize(this).width 
                //      <= getParent().getSize().width;
                return getUI().getPreferredSize(this).width 
                        < getParent().getSize().width;
            }
        };
        

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
        
        kawapad.addVariableInitializer( new KawaVariableInitializer() {
            @Override
            public void initializeVariable(Map<String, Object> variables ) {
                variables.put( "frame", KawapadFrame.this );
            }
        });
        
//      int MARGIN = 5;
//      {
//          Border b = BorderFactory.createLineBorder( background, MARGIN  );
//          kawapad.setBorder( b );
//      }
        
        scrollPane = new JScrollPane( kawapad );
        scratchPadRoot = new JPanel( new BorderLayout() );
        getContentPane().add(scratchPadRoot );
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
            JMenu fileMenuItem = new JMenu( "File" );
            fileMenuItem.setMnemonic('f');
            menuBar.add( fileMenuItem );
            fileMenuItem.add( new JMenuItem( NEW_SCRATCHPAD_ACTION ) );
            
            JMenu editMenuItem = new JMenu( "Edit" );
            editMenuItem.setMnemonic('e');
            menuBar.add( editMenuItem );
            
            JMenu viewMenuItem = new JMenu( "View" );
            viewMenuItem.setMnemonic('v');
            // menuBar.add( viewMenuItem );
            
            JMenu schemeMenuItem = new JMenu( "Scheme" );
            schemeMenuItem.setMnemonic('r');
            menuBar.add( schemeMenuItem );
            
            kawapad.initMenu( fileMenuItem, editMenuItem, viewMenuItem, schemeMenuItem );
            
            Action2.processMenuBar( menuBar );
            setJMenuBar( menuBar );
        }

        
        {
            setSize( new Dimension( 640, 480 ) );
            setDefaultCloseOperation( JFrame.DO_NOTHING_ON_CLOSE );
            
            this.addWindowListener( kawapad.createCloseQuery( new Runnable() {
                @Override
                public void run() {
                    KawapadFrame.this.onCloseWindow();
                    KawapadFrame.this.setVisible(false);
                    KawapadFrame.this.dispose();
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
    
    public void quit() {
        this.dispatchEvent( new WindowEvent(this, WindowEvent.WINDOW_CLOSING ));
    }

    /**
     * Kawapad#init() must be called whenever any Kawapad instance is created. Due
     * to INIT_03 (see the comment in the source code), the constructor cannot call
     * this method directory; we decided to mandate the users to call this method
     * manually.
     */

//  ADDED >>> (Tue, 06 Aug 2019 09:29:54 +0900)
    public void init() {
//      ADDED >>> (Tue, 06 Aug 2019 08:47:14 +0900)
        /*
         * At that time, I didn't realize that creation of a frame should be done in a INIT_03
         * different way from the creation of a scheme object. (Tue, 06 Aug 2019 08:47:14 +0900) 
         */
        // MODIFIED (Mon, 02 Sep 2019 06:16:35 +0900) >>>
        // Calling eventhanders is done inside Kawapad 
//      Kawapad.eventHandlers.invokeEventHandler( kawaPad, EventHandlers.CREATE,  kawaPad );
        this.kawapad.initialize();
        // MODIFIED (Mon, 02 Sep 2019 06:16:35 +0900) <<<
//      ADDED <<< (Tue, 06 Aug 2019 08:47:14 +0900)
    }
//  ADDED <<< (Tue, 06 Aug 2019 09:29:54 +0900)
    
    @Override
    public void dispose() {
        this.kawapad.finalize();
        super.dispose();
    }

    protected void onCloseWindow() {
    }
    


    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 
    // factory
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    public static KawapadFrame createStaticInstance() {
        SchemeSecretary schemeSecretary = new SchemeSecretary();
        DescriptiveHelp.registerGlobalSchemeInitializer( schemeSecretary );
        Kawapad.registerGlobalIntroSchemeInitializer( schemeSecretary );
        Kawapad.registerGlobalSchemeInitializer( schemeSecretary );
        schemeSecretary.newScheme();
        KawapadFrame kawapadFrame = new KawapadFrame( schemeSecretary, "Scheme Scratch Pad" );
        kawapadFrame.init();
        return kawapadFrame;
    }
    public static void main(String[] args) throws IOException {
        PulsarLogger.init();
        if ( 0 < args.length  ) {
            if ( args[0].equals( "--output-reference" ) ) {
                outputDocument();
            } else {
                start( new File( args[0] ) );
            }
        } else {
            start();        
        }
    }
    public static void outputDocument() throws IOException {
        KawapadFrame kawapadFrame = createStaticInstance();
        DescriptiveDocumentCategory.outputReference( kawapadFrame.kawapad.getSchemeSecretary(), "kawapad-procedures", null );
        try {
            Thread.sleep( 2048 );
        } catch ( InterruptedException e ) {
        }
        kawapadFrame.quit();
    }
    public static void start(File f) throws IOException {
        KawapadFrame kawapadFrame = createStaticInstance();
        if ( f != null )
            kawapadFrame.kawapad.openFile( f );
        else
            kawapadFrame.kawapad.openIntro();
    }
    public static void start() throws IOException {
        start( null );
        
    }
}

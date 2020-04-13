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

import java.io.File;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;

import javax.swing.JComboBox;

import org.jaudiolibs.jnajack.JackException;

import gnu.mapping.Procedure;
import gnu.mapping.Symbol;
import lamu.lib.app.ApplicationComponent;
import lamu.lib.log.Logger;
import lamu.lib.scheme.InvokableSchemeProcedure;
import lamu.lib.scheme.SchemeUtils;
import lamu.lib.secretary.Invokable;
import metro.Metro;
import metro.MetroPort;
import metro.MetroSequence;
import metro.MetroTrack;
import pulsar.PulsarLib.PulsarLibDelegator;

/**
 * Pulsar is a MIDI sequencer program which is controlled by a powerful computer
 * language which is called Scheme. Pulsar is written in JAVA and uses JACK
 * Audio Connection Kit to receive and send MIDI messages; Pulsar runs on any
 * Linux boxes, hopefully OSX and Windows as well.
 * <p>
 * Pulsar dynamically composes music by writing programs by Scheme. Scheme is a
 * dialect of a computer language which is called "lisp". Pulsar could be used
 * to develop automatic DJ application or automatic backing track generators
 * which composes on the fly.
 * <h3>Workflow</h3>
 * <i>TODO</i>
 * <p>
 * <h3>Document Model</h3>
 * This application has a slightly different concept of open/close files
 * comparative to those general music players. When a user opens a scheme script
 * on Pulsar, Pulsar merely executes it without any clean ups nor
 * initializations. For example, if a user opens an empty scheme script while
 * Pulsar is playing music, nothing will be happened and keeps the former state.
 * <p>
 * In Pulsar, scripts should implicitly initialize the state of the sequencer if
 * it is necessary because Pulsar does not do it automatically.
 * <p>
 * Pulsar has two important parameters :
 * <p>
 * <ul>
 * <li>main-file
 * <li>main-invokable
 * </ul>
 * <p>
 * The <code>main-file</code> is a file path to the main file which Pulsar is
 * currently referring. Pulsar keeps checking the timestamp of the file and
 * trying to detect file modification. Whenever Pulsar detects any timestamp
 * update on the file, Pulsar automatically reads it and execute.
 * <p>
 * The <code>main-invokable</code> is the invokable which initializes the state
 * of the sequencer. This method is the place where Pulsar starts a new song.
 * <p>
 * If a script file sets <code>main-invokable</code>, this is effectively the
 * application <i>opens</i> a new-file in the sense of general applications. A
 * script file could also leave <code>main-invokable</code> untouched.
 * <p>
 * This behavior is designed to be useful in some scenarios. For example, when a
 * user repeatedly updates/modifies the script file repeatedly in order to check
 * how it sounds, this behavior might help the user. If Pulse initializes the
 * sequencer whenever it reads a file, the music must restart from the
 * beginning. Users usually prefer it to keep the music playing and modify the
 * music slightly to see what will happen.
 * <p>
 * This behavior could probably be used as implementing sub-modules.
 * <h3>Pulsar API<h3>
 * 
 * @author Atsushi Oka
 */
public class Pulsar extends Metro implements PulsarLib, PulsarLibDelegator, ApplicationComponent {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
    static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
    static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }

    static String messageWarnIgnoredMissingSyncTrack( Object arg ) {
        return "could not find a track which name was " + arg + " ... ignored.";
    }

    public static final String DOCS_ID = "pulsar-procedures";

    static {
        /*
         *  Replace the main track id when Pulsar class is loaded to VM.
         *  Since here, the entire system can refer the main track as a Scheme's symbol.
         *  (Tue, 07 Jan 2020 20:05:06 +0900)
         */
        Metro.setMainTrackId( Symbol.valueOf( "main" ) );
    }

    //////////////////////////////////////////////////////////////////////////////////////////
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
        this.init();
    }
    @Override
    public void processQuit() {
    }

    static long shutdownWait = 1024;
    
    //////////////////////////////////////////////////////////////////////////////////////////
    
    public static Pulsar getCurrent() {
        return (Pulsar)Metro.getCurrent();
    }
    public static boolean isPresent() {
        return Metro.isPresent();
    }

    //////////////////////////////////////////////////////////////////////////////////////////

    @Override
    protected void onCreateThread() {
        super.onCreateThread();
//      registerLocalSchemeInitializers( this.schemeSecretary, this );
    }
    
    /**
     * Creates an instance of Pulsar object without opening any specific scheme
     * file. When a user creates an object by this constructor, the sequencer
     * remains closed after the application boots up. The user must explicitly
     * open a file to use the application.
     * 
     * (Sun, 24 Nov 2019 12:45:26 +0900) This comment is extremely outdated.
     * 
     */
    public Pulsar() {
        super();
    }
    
    public void init() {
    }

    
//  @Override
//  public void open(String clientName) throws JackException {
//      super.open(clientName);
//      newScheme();
//  }
    
//  PulsarGui pulsarGui;
//  SchemeHttp schemeHttp;

    boolean isQuitting = false;
    
    /**
     * Notify every interface to shutdown the application. Then shutdown. 
     */
    public void quit() {
        isQuitting = true;
        shutdown();
    }

    @Override
    public void close() {
        super.close();
    }
    
    public void shutdown() {
        close();
        execCleanupHook();
    }

    /**
     * This field specifies the invokable to reset all of the states inside the
     * sequencer and effectively this method starts a song. Whenever a user call
     * {@link Pulsar#rewind()}, this invokable will be invoked.
     */
    transient Invokable mainProcedure = null;

    /**
     * Sets the main-invokable object.
     * 
     * @see Pulsar#mainProcedure 
     */
    public void setMainProcedure( Invokable mainProcedure ) {
        this.mainProcedure = mainProcedure;
    }
    
    /**
     * Returns the main-invokable object.
     * 
     * @see Pulsar#mainProcedure 
     */
    public Invokable getMainProcedure() {
        return mainProcedure;
    }
    
    
    /**
     * {@link Pulsar#rewind()} method resets the state of the object and calls main
     * invokable to back to the state of beginning of the project. This method:9
     * effectively invoke the main invokable. See {@link Pulsar#mainProcedure}
     */
    public void rewind() { 
        logInfo( "===rewind" );
        setPlaying(false);
        clearTracks();
        if ( mainProcedure != null )
            mainProcedure.invoke();
    }

    
    /**
     * This hook objects will be invoked whenever reset() method is called.
     */
    final Collection<Runnable> cleanupHook = new LinkedList<>();
    
    Object getCleanUpHookLock() {
        // TODO COUNTERMEASURE_FOR_LOCKING (Mon, 23 Sep 2019 08:33:32 +0900)
        return getMetroLock();
    }
    
    /**
     * Add a hook that will be invoked whenever reset() method is called.
     */
    public void addCleanupHook( Runnable runnable ) {
        synchronized ( getCleanUpHookLock() ) { 
            cleanupHook.add( runnable );
        }
    }
    public void execCleanupHook( ) {
        synchronized ( getCleanUpHookLock() ) {
            for ( Iterator<Runnable> i =cleanupHook.iterator(); i.hasNext(); ) {
                Runnable runnable = i.next();
                try {
                    runnable.run();
                } catch ( Throwable e ) {
                    logError("CLEANUP HOOK: IGNORED AN EXCEPTION", e);
                }
            }
            cleanupHook.clear();
        }
    }
    

    public File getConfigDir() {
        final File configDir = new File( System.getProperty("user.home"), ".pulsar" );
        if ( ! configDir.isDirectory() ) {
            if (! configDir.mkdir() ) {
                logInfo( "WARNING : Failed to create the config directory." );
            }
        }
        return configDir;
    }

    public File getConfigFile() {
        // Configuration Directory
        final File configFile = new File( getConfigDir(), "init.scm" );
        if ( ! configFile.isFile() ) {
            try {
                configFile.createNewFile();
            } catch (IOException e) {
                logInfo( "WARNING : Failed to create the main config file." );
                e.printStackTrace();
            }
        }
        return configFile;
    }

    /**
     * This mechanism is currently not used. See {@link #readHistoryFile(JComboBox)}
     * @return
     */
    public File getHistoryFile() {
        // Configuration Directory
        final File historyFile = new File( getConfigDir(), "history.txt" );
        if ( ! historyFile.isFile() ) {
            try {
                historyFile.createNewFile();
            } catch (IOException e) {
                logError( "WARNING : Failed to create the history file.", e );
            }
        }
        return historyFile;
    }

    interface TempoTapperTempoNotifier {
        void notifyTempo( double beatPerMinute );
    }
    private final TempoTapper tempoTapper = new TempoTapper();
    public TempoTapper getTempoTapper() {
        return tempoTapper;
    }
    
    @Override
    public void setBeatsPerMinute(double beatsPerMinute) throws JackException {
        super.setBeatsPerMinute( beatsPerMinute );
    }
    
    interface ConnectProcedure {
        void apply( Pulsar pulsar, String from, String to ) throws JackException;
        ConnectProcedure CONNECT = new ConnectProcedure() {
            @Override
            public void apply(Pulsar pulsar, String from, String to) throws JackException {
                pulsar.connectPort(from, to);
            }
        };
        ConnectProcedure DISCONNECT = new ConnectProcedure() {
            @Override
            public void apply(Pulsar pulsar, String from, String to) throws JackException {
                pulsar.disconnectPort(from, to);
            }
        };
    }
    
    static void connectProc(Pulsar pulsar, Object[] args, ConnectProcedure proc ) throws JackException {
        ArrayDeque<Object> deque = new ArrayDeque<>( Arrays.asList( args ) );
        while ( 0 < deque.size() ) {
            Object fromObj = deque.pop();
            Object toObj = deque.pop();
            if ( fromObj == null || toObj == null ) {
                break;
            }
            String from = fromObj != null ? SchemeUtils.toString( fromObj ) : null;
            String to   = toObj   != null ? SchemeUtils.toString( toObj   ) : null;
            proc.apply(pulsar, from, to );
        }
    }
    
    @Override
    public MetroTrack createTrack(Object name, Collection<Object> tags, MetroSequence sequence) {
        return new PulsarTrack( this, name, tags, sequence );
    }

    public MetroTrack createTrack( Object name, Collection<Object> tags, Procedure procedure ) {
        return this.createTrack( name, tags, new SchemeSequence( InvokableSchemeProcedure.createSecretarillyInvokable( procedure ) ) );
    }
    
    public MetroTrack createRecordingTrack( Object name, Collection<Object> tags, List<MetroPort> inputPorts, List<MetroPort> outputPorts,
            double recordLength, boolean looper ) 
    {
        return this.createTrack( name, tags, SchemeSequenceRecorder.createSchemeSequenceRecorder( inputPorts, outputPorts, recordLength, looper ) );
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //
    //
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    final PulsarLibImplementation pulsarLibImplementation = new PulsarLibImplementation() {
        @Override
        protected Pulsar getPulsar() {
            return Pulsar.this;
        }
    };
    @Override
    public PulsarLib getPulsarLibImplementation() {
        return pulsarLibImplementation;
    }
    
}
    

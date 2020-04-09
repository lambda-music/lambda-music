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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;

import javax.swing.JComboBox;

import org.jaudiolibs.jnajack.JackException;

import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Procedure;
import gnu.mapping.Symbol;
import gnu.math.IntNum;
import kawa.standard.Scheme;
import lamu.lib.app.ApplicationComponent;
import lamu.lib.log.Logger;
import lamu.lib.scheme.InvokableSchemeProcedure;
import lamu.lib.scheme.SchemeEngine;
import lamu.lib.scheme.SchemeEvaluator.SchemeEngineListener;
import lamu.lib.scheme.SchemeUtils;
import lamu.lib.scheme.proc.MultipleNamedProcedure0;
import lamu.lib.scheme.proc.MultipleNamedProcedure2;
import lamu.lib.secretary.Invokable;
import lamu.utils.lib.MersenneTwisterFast;
import metro.EventListenable;
import metro.Metro;
import metro.MetroPort;
import metro.MetroSequence;
import metro.MetroSyncType;
import metro.MetroTrack;

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
public final class Pulsar extends Metro implements ApplicationComponent {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE,   msg, e   ); }
    static void logInfo (String msg             ) { LOGGER.log(Level.INFO,     msg      ); }
    static void logWarn (String msg             ) { LOGGER.log(Level.WARNING,  msg      ); }

    static String messageWarnIgnoredMissingSyncTrack( Object arg ) {
        return "could not find a track which name was " + arg + " ... ignored.";
    }

    public static void registerSchemeInitializers( SchemeEngine schemeEngine ) {
        schemeEngine.getSchemeEvaluator().registerSchemeInitializer( new SchemeEngineListener() {
            @Override
            public void execute( Scheme scheme ) {
//                removed (Thu, 09 Apr 2020 13:24:16 +0900) 
//                PulsarLib.initScheme( scheme.getEnvironment() );
            }
        });
    }
    
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
    public Pulsar( SchemeEngine schemeEngine ) {
        super();
        this.schemeEngine = schemeEngine;
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
     * reset() method resets the scheme environment.
     */
    public void reset() {
        logInfo("===Pulsar.reset()");
        newScheme();
        execCleanupHook();
        close();
    }
    
    private final SchemeEngine schemeEngine;
    public SchemeEngine getSchemeEngine() {
        return schemeEngine;
    }
    
    private void newScheme() {
        logInfo("Pulsar#newScheme() "); 
        this.getSchemeEngine().getSchemeEvaluator().reset();
    }

    MersenneTwisterFast random = new MersenneTwisterFast( new int[] { 
            (int) System.currentTimeMillis(),
            0x123, 0x234, 0x345, 0x456,
    });
    
    
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

    private static final class TagSearchIsProcedure extends MultipleNamedProcedure2 {
        private final Object value;
        TagSearchIsProcedure(Object value) {
            this.value = value;
        }
        @Override
        public Object apply2( Object arg1, Object arg2 ) throws Throwable {
            return value.equals( arg1 );
        }
    }

    final static class TrackProcedure extends MultipleNamedProcedure0 {
        final LList pair;
        TrackProcedure( LList pair ) {
            this.pair = pair;
        }

        @Override
        public Object apply0() throws Throwable {
            return pair;
        }
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
    
    interface ConnectProc {
        void apply( Pulsar pulsar, String from, String to ) throws JackException;
        ConnectProc CONNECT = new ConnectProc() {
            @Override
            public void apply(Pulsar pulsar, String from, String to) throws JackException {
                pulsar.connectPort(from, to);
            }
        };
        ConnectProc DISCONNECT = new ConnectProc() {
            @Override
            public void apply(Pulsar pulsar, String from, String to) throws JackException {
                pulsar.disconnectPort(from, to);
            }
        };
    }
    
    static void connectProc(Pulsar pulsar, Object[] args, ConnectProc proc ) throws JackException {
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

    static final class PulsarEventListener implements EventListenable.Listener {
        private final Pulsar pulsar;
        private final Procedure procedure;
        PulsarEventListener( Pulsar pulsar, Procedure procedure) {
            this.pulsar = pulsar;
            this.procedure = procedure;
        }
        
        @Override
        public void occured(Object parent, Object type) {
            this.pulsar.getThreadInitializerCollection().initialize();
            try {
                procedure.apply2( parent, type );
            } catch (Throwable e) {
                logError( "ignored", e );
            }
        }
        
        @Override
        public boolean equals(Object obj) {
            if ( obj instanceof PulsarEventListener ) {
                return this.procedure == ((PulsarEventListener)obj).procedure;
            } else {
                return false;
            }
        }
        @Override
        public int hashCode() {
            return procedure.hashCode();
        }
        @Override
        public String toString() {
            return String.format( "#PulsarEventListener-%x#" , this.hashCode() );
        }
    }
    /**
     *  
     * @param object
     * @return
     *    a newly created list which can safely be modified.
     */
    static List<Object> readParamTrackName( Object object ) { 
        object = SchemeUtils.schemeNullCheck( object );
        
        if ( object instanceof Pair ) {
            return new ArrayList<>( (Pair)object );
        } else {
            return new ArrayList<>( Arrays.asList( SchemeUtils.schemeNullCheck( object ) ) );
        }
    }
    
    /**
     * Wrap another invokable object in order to filter the undesirable arguments for
     * readParamTrackSearcher().
     * 
     * The tags property of MetroTrack accepts descendants of Collection class. In
     * the general use case of MetroTrack in Pulsar, it presumes that tags are LList
     * objects. The list object in ||tags|| property is passed to clients directly.
     * But since the MetroTrack accepts all types of Collection class descendants,
     * MetroTrack forces Pulsar to support tags objects which is other than LList.
     * 
     * Avoid unnecessary duplication of passed lists, we check the type of each list.
     * And let is pass through when it is an LList list, and convert it to LList when
     * it is a general Collection list. (Thu, 22 Aug 2019 12:18:27 +0900) 
     * 
     * @param i
     *     
     * @return
     */
    static Invokable readParamSearchTrackFilter( Invokable i ) {
        return new Invokable() {
            @Override
            public Object invoke(Object... args) {
                if ( 0 < args.length ) {
                    args[1] = filterArg( args[1] );
                }
                return i.invoke( args );
            }
            Object filterArg(Object arg1) {
                if ( arg1 == null ) {
                    return EmptyList.emptyList;
                } else if ( arg1 instanceof LList ) {
                    return arg1;
                } else if ( arg1 instanceof Collection ) {
                    if (((Collection)arg1).isEmpty()) {
                        return EmptyList.emptyList;
                    } else {
                        return Pair.makeList( Arrays.asList(((Collection)arg1).toArray()));
                    }
                } else {
                    return arg1;
                }
            }
        };
    }
    
    /**
     * XXX 
     * 
     * @param object
     * @return
     */
    static Procedure readParamTrackSearcher( Object object ) { 
        object = SchemeUtils.schemeNullCheck( object );
        // TAG SEARCH
        if ( object instanceof Procedure ) {
            return (Procedure) object;
        } else if ( object instanceof Symbol || object instanceof IString ) {
            return new TagSearchIsProcedure(object);
        } else {
            throw new IllegalArgumentException( "unsupported type of the argument (" + object + ")" );
        }
    }
    
    
    /**
     * This method used be `searchTrackCombo()`.
     * This was renamed at (Wed, 06 Nov 2019 07:38:28 +0900). 
     * @param arg
     * @return
     */
    List<MetroTrack> readParamSearchTrack(Object arg) {
        return 
                searchTrack(
                    readParamSearchTrackFilter(
                        InvokableSchemeProcedure.createSecretarillyInvokable( readParamTrackSearcher( arg ) )));
    }

    // TODO
    List<MetroTrack> readParamCreateTrack( Object object ) {
        if ( object instanceof MetroTrack ) {
            return Arrays.<MetroTrack>asList((MetroTrack)object);
        } else if ( object instanceof Procedure ) {
            return Arrays.asList( createTrack( null, null, (Procedure)object ));
        } else if ( object instanceof LList ) {
            if ( ((LList)object).isEmpty() ) {
                return Collections.emptyList();
            } else {
                if ( object instanceof Pair ) {
                    if (((Pair)object).getCar() instanceof MetroTrack) {
                        return (List<MetroTrack>) object;
                    } else if ( NoteListParser.isNotationList(object) ) {
                        return Arrays.asList( createTrack( null, null, new TrackProcedure( (Pair) object ) ) );
                    } else {
                        return readParamSearchTrack( object );
                    }
                } else {
                    throw new IllegalArgumentException("unknown type of argument (" + object + ")" ) ;
                }
            } 
        } else {
            return readParamSearchTrack( object );
        }
    }
    
    static double readParamSyncOffset(Object object) {
        return SchemeUtils.toDouble( object );
    }
    static MetroSyncType readParamSyncType(Object object) {
        object = SchemeUtils.schemeNullCheck(object);
        if ( object == null ) {
            return MetroSyncType.IMMEDIATE;
        } else {
            return MetroSyncType.toSyncType( SchemeUtils.toString( object ) );
        }
    }
    static Procedure readParamProcedure(Object arg) {
        if ( arg  == null ) {
            return null;
        } else if ( arg instanceof Procedure ) {
            return (Procedure) arg;
        } else if ( arg  instanceof Pair ) {
            return new TrackProcedure((Pair)arg);
        } else if ( arg  instanceof Number ) {
            return new TrackProcedure( createRestBar(((Number)arg).intValue() ) );
        } else {
            throw new IllegalArgumentException( "unsupported type of the argument (" + arg + ")" );
        }
    }

    private static LList createRestBar(int intValue) {
        return 
                LList.makeList( Arrays.asList( 
                    LList.makeList( Arrays.asList(
                        Pair.make( Symbol.valueOf( "type" ),  Symbol.valueOf( "len" ) ),
                        Pair.make( Symbol.valueOf( "val" ),   IntNum.valueOf( intValue ))))));
    }
    protected static List<Object> readParamPortName( Object arg ) {
        if ( arg instanceof Pair ) {
            return ((Pair)arg);
        } else {
            return Arrays.asList( arg );
        }
    }
    protected List<MetroPort> readParamPort( Object arg, List<MetroPort> portList ) {
        if ( arg instanceof Pair ) {
            List<MetroPort> list = new ArrayList<>();
            for ( Object o : ((Pair)arg) ) {
                list.addAll( readParamPort( o, portList ) );
            }
            return list;
        } else if ( arg instanceof MetroPort ) {
            return Arrays.asList( (MetroPort)arg );
        } else if ( arg instanceof IString || arg instanceof Symbol ) {
            MetroPort port = readParamNameToPort( portList, arg );
            if ( port == null ) {
                logWarn( "unsupported type of a value (" + arg + ")" );
                return Collections.EMPTY_LIST;
            } else {
                return Arrays.asList( port );
            }
        } else {
            logWarn( "unsupported type of a value (" + arg + ")" );
            return Collections.EMPTY_LIST;
        }
    }
    
    private static MetroPort readParamNameToPort( List<MetroPort> portList, Object arg ) {
        if ( arg instanceof MetroPort ) 
            return (MetroPort)arg;
        
        for ( MetroPort p : portList ) {
            if ( p.getName().equals( arg ) ) 
                return p;
        }
        return null;
    }
    
}
    

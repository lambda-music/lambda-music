/*
 * Metro Musical Sequencing Framework written by Atsushi Oka 
 * Copyright 2018 Atsushi Oka
 *
 * This file is part of Metro Musical Sequencing Framework. 
 * 
 * Metro Musical Sequencing Framework is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Metro Musical Sequencing Framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Metro Musical Sequencing Framework.  If not, see <https://www.gnu.org/licenses/>.
 */

package metro;

// KEYQWORD LIST : 
//     COUNTERMEASURE_FOR_LOCKING

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;

import org.jaudiolibs.jnajack.Jack;
import org.jaudiolibs.jnajack.JackClient;
import org.jaudiolibs.jnajack.JackException;
import org.jaudiolibs.jnajack.JackMidi;
import org.jaudiolibs.jnajack.JackOptions;
import org.jaudiolibs.jnajack.JackPortFlags;
import org.jaudiolibs.jnajack.JackPortType;
import org.jaudiolibs.jnajack.JackPosition;
import org.jaudiolibs.jnajack.JackProcessCallback;
import org.jaudiolibs.jnajack.JackShutdownCallback;
import org.jaudiolibs.jnajack.JackStatus;
import org.jaudiolibs.jnajack.JackTimebaseCallback;
import org.jaudiolibs.jnajack.JackTransportState;

import lamu.lib.Invokable;
import lamu.lib.log.Logger;

/**
 * 
 * @author Ats Oka
 */
public class Metro implements MetroReft,MetroMant,MetroPutt,MetroGett,MetroRemt,MetroLock, 
    JackProcessCallback, JackShutdownCallback, JackTimebaseCallback, Runnable 
{
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    private final Object lock = new Object();
    
    public static Object mainTrackName = new Object();
    public static synchronized void setMainTrackName( Object name ) {
        Metro.mainTrackName = name;
    }
    public static synchronized Object getMainTrackName() {
        return Metro.mainTrackName;
    }
    
    
    /**
     * The every routines which access to the `tracks` field must synchronize
     * to the object which can be retrieved by this getMetroLock() method. 
     */
    @Override
    public final Object getMetroLock() {
        // TODO COUNTERMEASURE_FOR_LOCKING (Mon, 23 Sep 2019 08:33:32 +0900)
        return Metro.this.lock;
    }
 
    public Metro() {
    }
    
    static final boolean DEBUG = false;

    private static final int MAX_TRACK_LOOP_COUNT = 128;
    
    protected Jack jack = null;
    protected JackClient client = null;
    protected Thread thread = null;
    protected final ArrayList<MetroPort> inputPortList = new ArrayList<MetroPort>();
    protected final ArrayList<MetroPort> outputPortList = new ArrayList<MetroPort>();
    
    private final JackMidi.Event midiEvent = new JackMidi.Event();
//  private BlockingQueue<String> debugQueue = new LinkedBlockingQueue<String>();
//    private StringBuilder sb = new StringBuilder();
    
    private final ArrayList<MetroTrackManipulator>  messageQueue = new ArrayList<MetroTrackManipulator>();
    private final ArrayList<MetroTrack> tracks = new ArrayList<MetroTrack>();

    // Number 1 for Snapshots for process()
    private final ArrayList<MetroTrackManipulator> messageQueueSnapshot1 = new ArrayList<MetroTrackManipulator>();
    private final ArrayList<MetroTrack> tracksSnapshot1 = new ArrayList<MetroTrack>(256);
    private final ArrayList<MetroTrack> finalTracksSnapshot1 = new ArrayList<MetroTrack>(256);
    private final ArrayList<MetroMidiEvent> finalInputMidiEvents1 = new ArrayList<MetroMidiEvent>(1024);
    private final ArrayList<MetroMidiEvent> finalOutputMidiEvents1 = new ArrayList<MetroMidiEvent>(1024);
    private final ArrayList<MetroTrack> removingTracksSnapshot1 = new ArrayList<MetroTrack>(256);

    private volatile MetroTrack mainTrack = null; 
    /**
     * A field which tracks a reference to the main track by best effort.
     * This field occasionally points to an outdated main track.
     * This field is not used by the internal process; This is intended to
     * be used as a cache of the referencde to the main track in order to 
     * avoid searching the main track frequently.  
     */
    public MetroTrack getMainTrack() {
        return mainTrack;
    }
    
    // zero means that to get the current bpm from Jack Transport.
    private double beatsPerMinute = 60;
    public double getBeatsPerMinute() {
        return beatsPerMinute;
    }
    public void setBeatsPerMinute(double beatsPerMinute) throws MetroException {
        beatsPerMinute = beatsPerMinute < 1 ? 1 : beatsPerMinute;
        this.beatsPerMinute = beatsPerMinute;
        this.resetMeasureLengthInFrames();
//        EXPERIMENTALY REMOVED (Tue, 05 May 2020 08:51:48 +0900) 
//        reprepareTrack( beatsPerMinute, this.beatsPerMinute );
    }

    private long beatsPerBar = 4;
    public long getBeatsPerBar() {
        return beatsPerBar;
    }
    public void setBeatsPerBar(long beatsPerBar) throws MetroException {
        this.beatsPerBar = beatsPerBar;
        // reprepareTrack();
    }
    
    public double getBarsPerSecond() {
        return beatsPerMinute/240d;
    }

    /**
     * This property can control when a track creates the next sequence pattern. The
     * value of this property denotes the ratio of measure to the threshold length;
     * setting 1 causes it to be the same length as the length of a measure in the
     * current tempo, and setting 0.5 to be half.
     */
    private double updateSequenceThreshold = 1.0d/ 32.0d;
    public void setUpdateSequenceThreshold(double value) {
        this.updateSequenceThreshold = value;
    }
    public double getUpdateSequenceThreshold() {
        return updateSequenceThreshold;
    }


    private volatile boolean playing = false;
    private void checkState() {
        if ( ! isOpened )
            throw new IllegalStateException( "not opened" );
    }

    public boolean getPlaying() {
        checkState();
        return this.playing;
    }
    public void setPlaying( boolean playing ) {
        checkState();
        this.playing = playing;
    }
    public boolean togglePlaying() {
        checkState();
        this.playing = ! this.playing;
        return this.playing ; 
    }

    public MetroPort createInputPort(Object portName) throws MetroException {
        return createPort(portName, this.inputPortList, JackPortFlags.JackPortIsInput);
    }
    public MetroPort createOutputPort(Object portName) throws MetroException {
        return createPort(portName, this.outputPortList, JackPortFlags.JackPortIsOutput);
    }
    public boolean destroyOutputPort(MetroPort port) throws MetroException {
        return destroyPort(port, JackPortFlags.JackPortIsOutput, this.outputPortList);
    }
    public boolean destroyInputPort(MetroPort port) throws MetroException {
        return destroyPort(port, JackPortFlags.JackPortIsInput, this.inputPortList);
    }

    public List<MetroPort> getInputPorts() {
        checkState();
        return new ArrayList<MetroPort>( this.inputPortList );
    }
    public List<MetroPort> getOutputPorts()  {
        checkState();
        return new ArrayList<MetroPort>( this.outputPortList );
    }
    
    private MetroPort createPort(Object portName, List<MetroPort> list, JackPortFlags flag) throws MetroException {
        if ( portName == null )
            throw new NullPointerException();
        checkState();
        try {
            MetroPort port = new MetroPort( 
                portName,
                flag,
                this.client.registerPort( 
                    Utils.anyToString(portName), 
                    JackPortType.MIDI, 
                    flag
                    )
                );
            list.add( port );
            return port;
        } catch ( JackException e ) {
            throw new MetroException(e);
        }
    }
    private boolean destroyPort(MetroPort targetPort, JackPortFlags portFlag, ArrayList<MetroPort> list) throws MetroException {
        if ( targetPort == null )
            throw new NullPointerException();
        checkState();
        if ( ! targetPort.jackPortFlag.equals( portFlag ) ) {
            throw new IllegalArgumentException();
        }
        try {
            boolean result = this.client.unregisterPort( targetPort.jackPort ) == 0;
            if (result){
                list.remove( targetPort );
                return true;
            } else {
                return false;
            }
        } catch ( JackException e ) {
            throw new MetroException(e);
        }
    }
    
    private void clearPorts() {
        this.inputPortList.clear();
        this.outputPortList.clear();
    }
    
    private static List<MetroPort> searchPort( List<MetroPort> ports, Invokable condition ) {
        List<MetroPort> result = new ArrayList<>();
        for ( MetroPort port : ports ) {
            if ( ! Boolean.FALSE.equals( condition.invoke( port.name ) ) ) {
                result.add( port );
            }
        }
        return result;
    }
    private static List<MetroPort> searchPort( List<MetroPort> ports, Object name ) {
        return searchPort( ports, new Invokable() {
            @Override
            public Object invoke(Object... args) {
                return name.equals( args[0] );
            }
        });
    }
    
    public List<MetroPort> searchInputPort( Invokable condition ) {
        return searchPort( this.getInputPorts(), condition );
    }
    public List<MetroPort> searchOutputPort( Invokable condition ) {
        return searchPort( this.getOutputPorts(), condition );
    }
    public List<MetroPort> searchInputPort( Object name ) {
        return searchPort( this.getInputPorts(), name );
    }
    public List<MetroPort> searchOutputPort( Object name ) {
        return searchPort( this.getOutputPorts(), name );
    }
    
    /**
     * For debug-purpose only.
     */
    public void dumpTracks() {
        logWarn( "Dump all tracks ===> " );
        
        List<MetroTrack> tempAllTracks = replicateAllTracks();
        for ( Iterator<MetroTrack> i=tempAllTracks.iterator(); i.hasNext(); ) {
            MetroTrack track = i.next();
            logWarn( "" + track.getName() );
        }
        logWarn( "Dump all tracks <=== " );
    }
    
    /*
     * This duplicates the "this.tracks" object inside a synchronized block. This
     * effectively make these algorithms managing "this.track" get outside from
     * the synchronized blocks.
     */
    public List<MetroTrack> replicateAllTracks() {
        checkState();
        synchronized( getMetroLock() ) {
            return new ArrayList<MetroTrack>( this.tracks );
        }
    }



    // INIT_02 (Sat, 03 Aug 2019 15:47:41 +0900) 
    /**
     * The method {@link #open(String)} creates a server instance and starts it. An
     * instance of Metro can be reused; you can reopen the instance of Metro
     * after closing it. 
     * 
     * <p>
     * NOTE : The reason that the {@link #open(String)} method throws an exception
     * when it is called duplicately in the meantime the {@link #close()} method
     * just ignores it.
     * 
     * When a user duplicately start a server, this maybe because of mistakenly
     * starting the server without any clean up the before state; therefore it
     * should report it to the user.
     * 
     * When a user duplicately stop a server, this maybe because of the user want to
     * clean up the server's state. In this case, the user usually want to stop the
     * server no matter it is already isOpened or not.
     */

    private volatile boolean isOpened = false;
    public void open( String clientName ) throws MetroException {
        logInfo( isOpened + "===open()");
        if ( isOpened )
            throw new RuntimeException( "Already Started" );
        
        logInfo("===open()");
        
        this.isOpened = true;
        
        // Create a Jack client.
        EnumSet<JackStatus> status = EnumSet.noneOf(JackStatus.class);
        try {
            this.jack = Jack.getInstance();
            this.client = this.jack.openClient( clientName , EnumSet.of(JackOptions.JackNoStartServer ), status);
            if (!status.isEmpty()) {
                logInfo("JACK client status : " + status);
            }

            // Create a thread.
//          this.sequence.setParent( this );
            this.activate();

//            // this thread should not be controlled by the open/close commands.
//            // (Sat, 03 Aug 2019 16:45:11 +0900)  
            initThread();
            
        } catch (JackException ex) {
            if (!status.isEmpty()) {
                logInfo("JACK exception client status : " + status);
            }
            throw new MetroException(ex);
        }
    }
    public boolean isOpened() {
        return isOpened;
    }
    
    // INIT_02
    void initThread() {
        this.thread = new Thread( this , "MetroSequencer" );
        this.thread.start();
//        INIT_02
//        this.postMessage( new Runnable() {
//          @Override
//          public void run() {
//              onCreateThread();
//          }
//      });
    }
    // INIT_02
    void finalizeThread() {
        if ( thread != null ) {
            thread.interrupt();
            try {
                thread.join();
                // if any exception occurred, we do not set null on the variable.
                thread = null;
            } catch (InterruptedException e) {
                logError("", e);
            }
        }
    }

    /*
     * (Thu, 25 Jul 2019 06:05:39 +0900)
     * 
     * This method is called when the system create a thread. This method is
     * intended to use as a hook to initialize the thread which the system create.
     * 
     * Background : especially in kawa scheme, Environment and Language objects are
     * kept in thread local variables. When a scheme object is accessed by
     * multiple threads, the chances are that the threads miss the current
     * language object and the current environment object. I am not sure it is by
     * specification or a bug; it means that every thread must be properly
     * initialized by setting those thread local variables.
     * 
     * This ignorant thread eventually call scheme functions later time and causes
     * error due to missing thread local objects.
     *
     * 2. (Thu, 08 Aug 2019 02:22:45 +0900)
     * NOT USED 
     */
    protected void onCreateThread() {
    }
    
    /*
     * Metro object guarantees that a closed Metro object can be reopened.
     * (Thu, 08 Aug 2019 02:22:45 +0900)
     */
    public void close() {
        logInfo( "===close() isOpened=" + isOpened );
        if ( ! isOpened )
            return;
        
        logInfo("===close()");
        try {
            try {
                clearTracks();
            } catch ( Exception e ) {
                logError("", e);
            }
            try {
                clearPorts();
            } catch ( Exception e ) {
                logError("", e);
            }
            try {
                finalizeThread();
            } catch ( Exception e ) {
                logError("", e);
            }
            if ( this.client != null ) {
                this.client.close();
            }
        } finally {
            this.isOpened = false;
            this.client = null;
            this.thread = null;
        }
    }
    
    
//    // ???
//    public void refreshTracks() {
//        checkState();
//        synchronized ( this.getMetroLock() ) {
//            for ( MetroTrack s : this.tracks ) {
//                s.clearBuffer();
//            }
//            for ( MetroTrack track : this.tracks  ) {
//                try {
//                    track.checkBuffer( this,  0 /* XXX */, this.client, this.position );
//                } catch (JackException e) {
//                    logError("", e);
//                }
//            }
//        }
//    }
    /**
     * This method foreces to clear the all currently playing tracks. Note that the
     * clean up procedures of the tracks are not properly called.
     */
    public void clearTracks() {
        checkState();
        synchronized ( this.getMetroLock() ) {
            this.tracks.clear();
        }
    }

    /**
     * Note that this method is called by another thread.
     */
    @Override
    public void run()  {
        logInfo("Metro.run()");
        //
        logInfo("Metro.run() : exited");
    }
    public static <T> List<T> createSnapshot(List<T> list) {
        return list.isEmpty() ? Collections.EMPTY_LIST : new ArrayList<>( list );
    }
    
    private String[] getAvailableOutputPorts_impl() throws JackException {
        return this.jack.getPorts( this.client, "", JackPortType.MIDI, EnumSet.of( JackPortFlags.JackPortIsOutput ) );
    }
    private String[] getAvailableInputPorts_impl() throws JackException {
        return this.jack.getPorts( this.client, "", JackPortType.MIDI, EnumSet.of( JackPortFlags.JackPortIsInput ) );
    }

    public List<String> getAvailableOutputPorts() throws MetroException {
        try {
            checkState();
            String[] ports = getAvailableOutputPorts_impl();
            return new ArrayList<String>( Arrays.asList( ports ) );
        } catch (JackException e) {
            throw new MetroException(e);
        }
    }
    public List<String> getAvailableInputPorts() throws MetroException {
        try {
            checkState();
            String[] ports = getAvailableInputPorts_impl();
            return new ArrayList<String>( Arrays.asList( ports ) );
        } catch (JackException e) {
            throw new MetroException(e);
        }
    }

    private void activate() throws JackException {
        this.client.setProcessCallback(this);
        this.client.setTimebaseCallback(this, false);
        this.client.onShutdown(this);
        this.client.activate();
        
        logInfo( "========================");
        logInfo( "a list of all Jack ports" );
        logInfo( "// INPUT //");
        {
            String[] ports = getAvailableInputPorts_impl();
            for ( String s : ports ) {
                logInfo( s );
            }
        }
        logInfo( "// OUTPUT //");
        {
            String[] ports = getAvailableOutputPorts_impl();
            for ( String s : ports ) {
                logInfo( s );
            }
        }
        logInfo( "========================");
        logInfo( "" );
    }
    
    public void connectPort( Map<String, String> map ) throws MetroException {
        checkState();

        for ( Entry<String, String> portName : map.entrySet() ) {
            String key = portName.getKey();
            String value = portName.getValue();
            if ( key != null && value != null ) {
                try {
                    connectPort(key, value);
                } catch ( MetroException e ) {
                    logError("",e);
                }
            }
        }
    }
    public void connectPort(String from, String to) throws MetroException {
        checkState();

        try {
            this.jack.connect( this.client, from, to);
        } catch ( JackException e ) {
            throw new MetroException( "Error occured while connecting from " + from + " to " + to, e );
        }
    }
    public void disconnectPort(String from, String to) throws MetroException {
        checkState();

        try {
            this.jack.disconnect( this.client, from, to);
        } catch ( JackException e ) {
            throw new MetroException( "Error occured while connecting from " + from + " to " + to, e );
        }
    }
    
    /**
     * This method handles JACK's <code>process</code> event.
     * This is possibly called by JACK's thread.   
     */
    @Override
    public boolean process(JackClient client, int nframes) {
        long l_nframes = nframes;
        if ( ! this.playing ) {
            return true;
        }
        
        long measureLengthInFrames;
        try {
            measureLengthInFrames = getMeasureLengthInFrames();
        } catch (MetroException e1) {
            logError("", e1);
            return true;
        }

        try {
            for ( MetroPort p : Metro.this.outputPortList )
                JackMidi.clearBuffer( p.jackPort );

            ArrayList<MetroTrackManipulator> messageQueueSnapshot = this.messageQueueSnapshot1;
            ArrayList<MetroTrack> tracksSnapshot = this.tracksSnapshot1;
            ArrayList<MetroTrack> finalTracksSnapshot = this.finalTracksSnapshot1;
            ArrayList<MetroMidiEvent> finalInputMidiEventList = this.finalInputMidiEvents1;
            ArrayList<MetroMidiEvent> finalOutputMidiEventList = this.finalOutputMidiEvents1;
            ArrayList<MetroTrack> removingTracksSnapshot = this.removingTracksSnapshot1; 

            messageQueueSnapshot.clear();
            tracksSnapshot.clear();
            finalTracksSnapshot.clear();
            finalInputMidiEventList.clear();
            finalOutputMidiEventList.clear();
            removingTracksSnapshot.clear();

            {
                for ( Iterator<MetroPort> pi = this.inputPortList.iterator(); pi.hasNext(); ) {
                    MetroPort inputPort = pi.next();

                    int eventCount = JackMidi.getEventCount( inputPort.jackPort );
                    for (int i = 0; i < eventCount; ++i) {
                        JackMidi.eventGet( this.midiEvent, inputPort.jackPort, i );
                        int size = this.midiEvent.size();
                        byte[] data = new byte[size];
                        this.midiEvent.read( data );
                        finalInputMidiEventList.add( new DefaultMetroMidiEvent( this.midiEvent.time(), inputPort, data ) );
                    }
                }

                synchronized ( this.getMetroLock() ) {
                    tracksSnapshot.addAll( this.tracks );
                    messageQueueSnapshot.addAll( this.messageQueue );
                    this.messageQueue.clear();
                }

                // 1. messages
                //  (Fri, 12 Jun 2020 16:00:50 +0900)
                if ( ! messageQueueSnapshot.isEmpty() ) {
                    ArrayList<MetroTrack> registeringTracksTemp = new ArrayList<MetroTrack>();
                    ArrayList<MetroTrack> unregisteringTracksTemp = new ArrayList<MetroTrack>();
                    ArrayList<MetroTrack> removingTracksTemp = new ArrayList<MetroTrack>();

                    // Note that processing `tracksSnapshot`, not the `finalTracksSnapshot`.
                    for ( MetroTrackManipulator message : messageQueueSnapshot ) {
                        registeringTracksTemp.clear();
                        unregisteringTracksTemp.clear();
                        try {
                            // 1.1 Process
                            message.manipulateTracks( 
                                tracksSnapshot,
                                registeringTracksTemp, 
                                removingTracksTemp, 
                                unregisteringTracksTemp );
                        } catch ( Throwable e ) {
                            logError( "An error occured in a message object", e);
                        }
                        // 1.2. Preparing the final track list. 
                        tracksSnapshot.addAll( registeringTracksTemp );
                        // 1.3. Preparing the final track list.
                        tracksSnapshot.removeAll( unregisteringTracksTemp );
                        // 1.4 Reflect it to the removing track list.
                        removingTracksSnapshot.addAll( removingTracksTemp );
                    }
                }

                // 2 
                finalTracksSnapshot.addAll( tracksSnapshot );
                
                // 3. tracks
                {
                    int trackLoopCount = 0;
                    ArrayList<MetroTrack> currTrackList = tracksSnapshot;
                    for(;;) {
                        if ( MAX_TRACK_LOOP_COUNT <trackLoopCount ) {
                            logError( "=== ERROR:" + MAX_TRACK_LOOP_COUNT  + "<trackLoopCount ===", new Exception() );
                            break;
                        }
                        ArrayList<MetroTrack> nextTrackList = new ArrayList<>();

                        for ( MetroTrack track : currTrackList ) {
                            track.inputMidiEvents.addAll( finalInputMidiEventList );
                            track.outputMidiEvents.clear();
                            track.registeringTracks.clear();
                            track.removingTracks.clear(); 
                            track.unregisteringTracks.clear();
                            // 3.1 Process
                            track.getSequence().advanceCursor( this, track, 
                                l_nframes, 
                                measureLengthInFrames, 
                                track.inputMidiEvents,
                                track.outputMidiEvents, 
                                finalTracksSnapshot, 
                                track.registeringTracks, 
                                track.removingTracks, 
                                track.unregisteringTracks );

                            // 3.2. Preparing the final track list. 
                            finalTracksSnapshot.addAll( track.registeringTracks );
                            // 3.3. Preparing the final track list.
                            finalTracksSnapshot.removeAll( track.unregisteringTracks );
                            // 3.4 Reflect it to the removing track list.
                            removingTracksSnapshot.addAll( track.removingTracks );
                            // 3.5 Prepare the next track.
                            nextTrackList.addAll( track.registeringTracks );
                            nextTrackList.removeAll( track.unregisteringTracks );
                        }
                        
                        if ( nextTrackList.isEmpty() ) {
                            break;
                        } else {
                            currTrackList= nextTrackList;
                            trackLoopCount++;
                            continue;
                        }
                    }


                    // 4. Preparing the final track list.
                    for ( MetroTrack track : finalTracksSnapshot ) {
                        finalOutputMidiEventList.addAll( track.outputMidiEvents ); 
                    }

                    // sort the every event 
                    finalOutputMidiEventList.sort( MetroMidiEvent.COMPARATOR );

                    if ( ! finalOutputMidiEventList.isEmpty() )
                        if ( DEBUG ) logInfo( finalOutputMidiEventList.toString() );

                    //                logInfo( "output count : "+ this.outputMidiEventList.size() );

                    // output the events
                    for ( MetroMidiEvent e : finalOutputMidiEventList ) {
                        JackMidi.eventWrite(
                            e.getPort().jackPort, 
                            (int)e.getMidiOffset(), 
                            e.getMidiData(), 
                            e.getMidiData().length 
                            );
                    }
                }
                
                for ( MetroTrack t : removingTracksSnapshot ) {
                    t.remove( this, null );
                }

                synchronized ( this.getMetroLock() ) {
                    this.tracks.clear();
                    this.tracks.addAll( finalTracksSnapshot );
                }
            }
            return true;
        } catch (JackException ex) {
            logError( "ERROR" , ex);
            return false;
        } catch (Throwable t) {
            logError( "ERROR" , t);
            return true;
        }
    }
    public MetroPort outputPortNumberToPort(int i) {
        if ( 0<=i && i < outputPortList.size()  ) 
            return outputPortList.get( i );
        else
            throw new IllegalArgumentException();
    }
    public MetroPort inputPortNumberToPort(int i) {
        if ( 0<=i && i < inputPortList.size()  ) 
            return inputPortList.get( i );
        else
            throw new IllegalArgumentException();
    }
    
    public void notifyTrackChange( Object message ) {
        // TODO
        // THIS METHOD IS TO BE REMOVED (Fri, 12 Jun 2020 17:37:35 +0900)
    }
    
    
    /**
     * Post a new message which is denoted by a runnable objects. The messages are
     * invoked by the main thread of this Metro instance.
     * 
     * @param manipulator
     *            the procedure to run at a later time.
     */
    public void postMessage( List<MetroTrackManipulator> manipulators ) {
        checkState();

        if ( DEBUG )
            logInfo( "****** postMessage 1");
        
        synchronized ( getMetroLock() ) {
            this.messageQueue.addAll( manipulators );
            this.notifyTrackChange("update");
        }
    }

    /**
     * {@link #putTracks(List)} and {@link #getTracks(MetroTrackSelector)} are redirected to this methos. 
     */
    @Override
    public void manipulateTrack( List<MetroTrackManipulator> manipulators ) {
        postMessage( manipulators );
    }
    
    @Override
    public void referTracks( List<MetroTrackSelector> trackSelectors, List<MetroTrack> selectedTracks ) {
        MetroTrackSelector.executeSelector(this, trackSelectors, selectedTracks);
    }
    
    @Override
    public void clientShutdown(JackClient client) {
        logInfo("JackAudio: Java MIDI thru test shutdown");
    }
    
    @Override
    public void updatePosition(
        JackClient invokingClient, 
        JackTransportState state, int nframes, JackPosition position,
            boolean newPosition) {
        logInfo("==========JackAudio: the current position changed ============== " + position );
        
    }

    private final JackPosition jackPosition = new JackPosition();
    private long measureLengthInFrames = -1;
    public void resetMeasureLengthInFrames() throws MetroException {
        synchronized ( this.getMetroLock() ) {
            this.measureLengthInFrames=-1;
        }
    }
    
    public long getMeasureLengthInFrames() throws MetroException {
        synchronized ( this.getMetroLock() ) {
            if ( this.measureLengthInFrames < 0 ) {
                this.measureLengthInFrames = calcMeasureLengthInFrames( this, this.client, this.jackPosition );
            }
            return this.measureLengthInFrames;
        }
    }

    public static long calcMeasureLengthInFrames( Metro metro, JackClient client, JackPosition position) throws MetroException {
        // logInfo("Metro.offerNewBuffer()" + this.buffers.size() );
        // beat per minute
        double bpm;
        // beat per bar 
        double bpb;
        long frameRate;
        try {
            client.transportQuery( position );
            bpm = position.getBeatsPerMinute();
            bpb = position.getBeatsPerBar();
            frameRate = position.getFrameRate();
            
            // logInfo( "framerate" + frameRate + " bpb="+ bpb );
        } catch (JackException e) {
            throw new MetroException(e);
        }
        
        double ownBeatPerMinute = metro.getBeatsPerMinute();
        long ownBeatPerBar    = metro.getBeatsPerBar();
        
        if ( 0 < ownBeatPerMinute ) {
            bpm = ownBeatPerMinute;
            bpb = ownBeatPerBar;
        }
        
        double beatInSecond = 1.0d / ( bpm / 60.0d /*seconds*/ );
        long barInFrames = (long) (beatInSecond * frameRate * bpb);
        return barInFrames;
    }
    
    
    
}

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
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
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
public class Metro implements  MetroLock, JackProcessCallback, JackShutdownCallback, JackTimebaseCallback, Runnable {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    private final Object lock = new Object();
    
    public static Object mainTrackId = new Object();
    public static synchronized void setMainTrackId( Object id ) {
        Metro.mainTrackId = id;
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
    
    protected Jack jack = null;
    protected JackClient client = null;
    protected Thread thread = null;
    protected final ArrayList<MetroPort> inputPortList = new ArrayList<MetroPort>();
    protected final ArrayList<MetroPort> outputPortList = new ArrayList<MetroPort>();
    
    private final JackMidi.Event midiEvent = new JackMidi.Event();
//  private BlockingQueue<String> debugQueue = new LinkedBlockingQueue<String>();
//    private StringBuilder sb = new StringBuilder();
    
    private final ArrayList<MetroTrack> tracks = new ArrayList<MetroTrack>();
    private final ArrayList<MetroTrack> registeredTracks = new ArrayList<MetroTrack>();
    private final ArrayList<MetroTrack> unregisteredTracks = new ArrayList<MetroTrack>();
    private final ArrayList<Runnable>  messageQueue = new ArrayList<Runnable>();
    
    private ArrayList<MetroTrack> tracksSnapshot = new ArrayList<MetroTrack>();
    private ArrayList<MetroMidiEvent> inputMidiEventList = new ArrayList<MetroMidiEvent>();
    private ArrayList<MetroMidiEvent> outputMidiEventList = new ArrayList<MetroMidiEvent>();

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
    
    public Collection<MetroTrack> getTrackByTag( String tag ) {
        return searchTracksByTag( tag );
    }
    
    
/*  
    public MetroTrack addTrack(
            String name, Collection<String> tags, MetroSequence sequence,
            SyncType syncType, MetroTrack syncTrack, double syncOffset ) 
    {
        MetroTrack track = createTrack(name, tags, sequence, syncType, syncTrack, syncOffset);
        registerTrack(track);
        return track;
    }
    public MetroTrack deleteTrack(
            MetroTrack track, SyncType syncType, MetroTrack syncTrack, double syncOffset ) 
    {
        switch ( syncType ) {
            case IMMEDIATE:
                unregisterTrack( track );
                break;
            case PARALLEL:
                track.removeGracefully( null );
                break;
            case SERIAL : 
                break;
        }
        registerTrack(track);
        return track;
    }
*/
    
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

//  if ( name.endsWith("!" ) ) {
//      if ( "last!".equals( name )) {
//          if ( tempAllTracks.size() == 0 ) {
//              if ( DEBUG ) logInfo( "searchTrack() last! null (last! was specified but the current track contains no element)" );
//              return null;
//          } else {
//              if ( DEBUG ) logInfo( "searchTrack() last!" );
//              return tempAllTracks.get( tempAllTracks.size() -1 );
//          }
//      } else if ( "first!".equals( name )) {
//          if ( tempAllTracks.size() == 0 ) {
//              if ( DEBUG ) logInfo( "searchTrack() first! null (first! was specified but the current track contains no element)" );
//              return null;
//          } else {
//              if ( DEBUG ) logInfo( "searchTrack() first!" );
//              return tempAllTracks.get( tempAllTracks.size() -1 );
//          }
//      } else {
//          return tempAllTracks.get( Integer.parseInt( name.substring(0,name.length()-1) ) );
//      }
//  } else {

    
    public List<MetroTrack> searchTrack( Invokable invokable ) {
        List<MetroTrack> tempAllTracks = replicateAllTracks();
        List<MetroTrack> resultAllTracks = new ArrayList<>();

        for ( Iterator<MetroTrack> i=tempAllTracks.iterator(); i.hasNext(); ) {
            MetroTrack track = i.next();
            if ( Boolean.FALSE.equals( invokable.invoke( track.getName(), track.getTags()) ) ) {
                continue;
            } else {
                resultAllTracks.add( track );
            }
        }
        if ( DEBUG )
            logInfo( "searchTrack() null" );
        //          logWarn( "searchTrack() WARNING \"" + name + "\"  was not found." );
        return resultAllTracks;
    }
    
    public List<MetroTrack> searchTrack( Object name ) {
        if ( name == null ) throw new NullPointerException( "name was null" );
        List<MetroTrack> list = searchTrack( new Invokable() {
            @Override
            public Object invoke(Object... args) {
                return name.equals( args[0] );
            }
        });
        return list;
    }

    private List<MetroTrack> searchTracksByTagImpl( String tag, 
            List<MetroTrack> allTracks, 
            ArrayList<MetroTrack> resultTracks ) 
    {
        for ( Iterator<MetroTrack> i = allTracks.iterator(); i.hasNext();  ) {
             MetroTrack track = i.next();
             if ( track.getTags().contains( tag ) ) {
                 resultTracks.add( track );
             }
        }
        return resultTracks;
    }

    public Collection<MetroTrack> searchTracksByTag( String tag ) {
        return searchTracksByTagImpl(tag, replicateAllTracks(), new ArrayList<>() );
    }

    public Collection<MetroTrack> searchTracksByTagSet( Collection<String> tagSet ) {
        HashSet<MetroTrack> resultTracks = new HashSet<>();
        List<MetroTrack> tempAllTracks = replicateAllTracks();
        for ( String tag : tagSet ) {
            resultTracks.addAll( searchTracksByTagImpl( tag, tempAllTracks, new ArrayList<MetroTrack>() ) );
        }
        return resultTracks;
    }
    
//  This method was removed. (Mon, 29 Jul 2019 11:37:24 +0900)
//  At the time of removing, there were two occurrences of referring this method.
//  These are inlined. 
//  public void removeTrack( String name ) {
//      removeTrack( getTrack( name ) );
//  }
//  This method was removed. (Mon, 29 Jul 2019 09:55:02 +0900)
//  At the time of removing, no method refers this.
//  public void removeTrack( Object nameObject ) {
//      removeTrack( getTrack( nameObject ) );
//  }

//  This method was removed at (Mon, 29 Jul 2019 09:13:06 +0900)
//  public MetroTrack createTrack(  
//          String name, Collection<String> tags, MetroSequence sequence,
//          SyncType syncType, Object syncTrackObject, double syncOffset) 
//  {
//      MetroTrack syncTrack = getTrack( syncTrackObject ); 
//      return new MetroTrack( this, name, tags, sequence, syncType, syncTrack, syncOffset );
//  }
    
    
//  @Deprecated
//  public void enableTrackByTag( String tag, boolean enabled ) {
//      for ( MetroTrack track : searchTrackByTag( tag ) ) {
//          track.setTrackEnabled( enabled );
//      }
//  }


    /**
     * This method removes the specified track directory. This method requires
     * inherently dangerous operation and may cause a
     * ConcurrentModificationException to be thrown. This method should not be used.
     * 
     * @param name
     */
//  removed (Mon, 29 Jul 2019 10:30:21 +0900)
//  @Deprecated
//  public void removeLogicDirect( String name ) {
//      Iterator<MetroTrack> iterator = lookupTrackDirect(name);
//      if ( iterator != null  ) {
//          iterator.remove();
//      }
//  }

    /**
     * This method removes the specified track directory. This method requires
     * inherently dangerous operation and may cause a
     * ConcurrentModificationException to be thrown. This method should not be used.
     * 
     * @param name
     */
//  removed (Mon, 29 Jul 2019 10:30:21 +0900)
//  @Deprecated
//  private Iterator<MetroTrack> lookupTrackDirect( String name ) {
//      name = name.intern();
//      for ( Iterator<MetroTrack> i=tracks.iterator(); i.hasNext(); ) {
//          MetroTrack track = i.next();
//          if ( track.name == name ) {
//              return i;
//          }
//      }
//      return null;
//  }

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
    public void clearTracks() {
        checkState();
        synchronized ( this.getMetroLock() ) {
            this.tracks.clear();
            this.notifyTrackChange("update");
        }
    }

    /**
     * Note that this method is called by another thread.
     */
    @Override
    public void run()  {
        logInfo("Metro.run()");

        while ( true ) {
            //          logInfo("Metro.run()");
            //          String s = this.debugQueue.take();
            //          System.err.println( s );

            try {
                long barLengthInFrames;
                List<MetroTrack> tracks2;
                List<Runnable> messageQueue2;
//                List<MetroTrack> registeredTracks2;

                synchronized ( this.getMetroLock() ) {
                    barLengthInFrames = this.getOneBarLengthInFrames();

                    this.tracks.removeAll( this.unregisteredTracks );
                    this.tracks.addAll( this.registeredTracks );
                
                    tracks2 = createSnapshot(this.tracks);
                    messageQueue2 = createSnapshot(this.messageQueue);
//                    registeredTracks2 = createSnapshot(this.registeredTracks);
                    
                    /*
                     * Note that it is necessary to duplicate `registeredTracks` since the "prepare" method possiblly modify the
                     * "registeredTrack" in its event handlers. 
                     */

                    this.unregisteredTracks.clear();
                    this.registeredTracks.clear();
                    this.messageQueue.clear();  
                }
                
                {
                    for ( MetroTrack track : tracks2  ) {
                        track.processBuffer( this,  barLengthInFrames );
                    }
                    for ( Runnable r : messageQueue2 ) {
                        try {
                            r.run();
                        } catch ( Throwable e ) {
                            logError( "An error occured in a message object", e);
                        }
                    }
//                    for ( MetroTrack track : registeredTracks2 ) {
//                        // ADDED (Sun, 30 Sep 2018 12:39:32 +0900)
//                        track.processBuffer( Metro.this,  barLengthInFrames );
//                    }
                }
                
                synchronized ( this.getMetroLock()) {
                    // METRO_LOCK_WAIT
                    // XXX ??? why not zero?? (Thu, 01 Aug 2019 11:49:55 +0900)
                    /*
                     * Note : originally this loop was designed to be executed only when other
                     * threads eagerly call getMetroLocl().notifyEvent(). But later, it appeared to
                     * be like that checking if the number of the buffers is sufficient only sometime
                     * causes buffer-underflow when it comes to fast tempo music.     
                     * 
                     * Therefore, I changed this value to 1 to obtain faster looping.
                     * 
                     * Also note that looping excessively fast causes holding getMetroLock() longer time;
                     * this very likely leads Pulsar works jumpy. 
                     */
                    this.getMetroLock().wait();
                }
                
            } catch ( InterruptedException e ) {
                // ignore
                logWarn( "interrupted" );
                break;
            } catch ( Throwable e ) {
                logError("", e);
            }
            //              logInfo( this.tracks.size() );
            //              Thread.sleep(0);
        }
        logInfo("Metro.run() : exited");
    }
    public <T> List<T> createSnapshot(List<T> list) {
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
        
        try {
            for ( MetroPort p : Metro.this.outputPortList )
                JackMidi.clearBuffer( p.jackPort );
            
            ArrayList<MetroMidiEvent> inputMidiEventList2 = this.inputMidiEventList;
            ArrayList<MetroMidiEvent> outputMidiEventList2 = this.outputMidiEventList;
            
            inputMidiEventList2.clear();
            outputMidiEventList2.clear();
            tracksSnapshot.clear();
            
            for ( Iterator<MetroPort> pi = this.inputPortList.iterator(); pi.hasNext(); ) {
                MetroPort inputPort = pi.next();
                
                int eventCount = JackMidi.getEventCount( inputPort.jackPort );
                for (int i = 0; i < eventCount; ++i) {
                    JackMidi.eventGet( this.midiEvent, inputPort.jackPort, i );
                    int size = this.midiEvent.size();
                    byte[] data = new byte[size];
                    this.midiEvent.read( data );
                    inputMidiEventList2.add( new DefaultMetroMidiEvent( this.midiEvent.time(), inputPort, data ) );
                }
            }

            synchronized ( this.getMetroLock() ) {
//                logInfo( "===" );
                tracksSnapshot.addAll( this.tracks );
            }
            
            {
                for ( MetroTrack track : tracksSnapshot ) {
                    track.progressCursor( this, l_nframes, inputMidiEventList2, outputMidiEventList2 );
                }
//                logInfo( "" );
                
                // sort the every event 
                outputMidiEventList2.sort( MetroMidiEvent.COMPARATOR );
                
                if ( ! outputMidiEventList2.isEmpty() )
                    if ( DEBUG ) logInfo( outputMidiEventList2.toString() );

//                logInfo( "output count : "+ this.outputMidiEventList.size() );
                
                // output the events
                for ( MetroMidiEvent e : outputMidiEventList2 ) {
                    JackMidi.eventWrite(
                            e.getPort().jackPort, 
                            (int)e.getMidiOffset(), 
                            e.getMidiData(), 
                            e.getMidiData().length 
                            );
                }
                
//              JackMidi.eventWrite( 
//                      Metro.this.outputPortList.get( e.getOutputPortNo() ), 
//                      e.getOffsetInFrames() - this.cursor, 
//                      e.getData(), 
//                      e.getData().length 
//                      );
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
    
    // @see METRO_LOCK_WAIT
    /**
     * <b>The following description is obsolete.(Tue, 12 May 2020 02:35:12 +0900)</b><br/>
     * <p>
     * Any caller of the methods {@link #registerTrack(MetroTrack)} and
     * {@link #unregisterTrack(MetroTrack)} has responsibility to call
     * {@link #notifyTrackChange(Object)} method after calling. When multiple tracks are
     * registered/unregistered at once, the caller must call
     * {@link #notifyTrackChange(Object)} at least once a session and not necessarily call
     * every time registering a track.
     * <p>
     * It is preferable that a user who performs any bulk-registering calls
     * {@link #notifyTrackChange(Object)} method only once after the registering a set of
     * track and they should avoid to redundantly call {@link #notifyTrackChange(Object)}
     * multiple times.
     * <p>
     * Any caller of this method should place the calling inside a synchronized
     * block of an object which you can retrieve by {@link #getMetroLock()} method;
     * otherwise you will get an unexpected result.
     * <p>
     * 
     * <pre>
     * synchronized (getMetroLock()) {
     *  MetroTrack track = createTrack(...);
     *  registerTrack(track);
     *  notifyCheckBuffer();
     * }
     * </pre>
     * @param message TODO
     * @param track
     */

    public void notifyTrackChange( Object message ) {
        synchronized ( this.getMetroLock() ) {
            if ( entrantCount == 0 ) {
                this.getMetroLock().notify();
            }
        }
    }
    private volatile int entrantCount =0;
    public void enterTrackChangeBlock() {
        synchronized ( this.getMetroLock() ) {
            this.entrantCount ++;
        }
    }
    public void leaveTrackChangeBlock() {
        synchronized ( this.getMetroLock() ) {
            this.entrantCount --;
            if ( this.entrantCount < 0 ) {
                logWarn( String.format( 
                    "this.entrantCount < 0 %d Corrected the invalid value of entrantCount.",
                    this.entrantCount ) );
                this.entrantCount = 0;
            }
        }
    }

    /*
     * NOTE :
     * 
     * Note that  There is no `this.logics`. Instead of that all of MetroSequence objects are 
     * wrapped by Track and stored as `this.tracks`.   
     * (Sat, 18 Aug 2018 19:03:18 +0900)
     */

    /*
     * NOTE : DON'T FORGET THIS
     * 
     * Two methods registerTrack() and unregisterTrack() are registering 
     * the given track object via two queues : registeredTracks / unregisteredTracks.
     * These lists are referred by Metro.run() method which is executed by another thread. 
     * 
     */
    
    /**
     * Register a track and play.
     * 
     */
    
    
    /**
     * This registers the specified track.  This method should be called with
     * the same protocol with {@link #registerTrack(MetroTrack)} method. 
     * @see {@link #registerTrack(MetroTrack)}
     * @param track
     */
    public void registerTrack( MetroTrack track ) {
        checkState();
        if ( DEBUG ) 
            logInfo( "MetroTrack#registerTrack(MetroTrack)" );
        
        if ( track == null )
            throw new NullPointerException( "track is null" );
        
        synchronized (getMetroLock()) {
            this.registeredTracks.add( track );
            // ADDED (Sun, 10 May 2020 23:10:26 +0900)
            notifyTrackChange("register-track");
        }
        if ( Metro.mainTrackId.equals( track.getName() ) ) {
            this.mainTrack = track;
        }
    }
    public void registerTrack( Collection<MetroTrack> trackList ) {
        checkState();
        if ( DEBUG ) 
            logInfo( "MetroTrack#registerTrack(Collection)" );
        
        if ( trackList == null )
            throw new NullPointerException( "the passed list is null" );
        
        synchronized (getMetroLock()) {
            this.registeredTracks.addAll( trackList );
            // ADDED (Sun, 10 May 2020 23:10:26 +0900)
            notifyTrackChange("register-track");
        }
        for ( MetroTrack track : trackList ) {
            if ( Metro.mainTrackId.equals( track.getName() ) ) {
                this.mainTrack = track;
                break;
            }
        }
    }
    
    /**
     * This method unregister the specified track.  This method should be called with
     * the same protocol with {@link #registerTrack(MetroTrack)} method. 
     * @see {@link #registerTrack(MetroTrack)}
     * @param track
     */
    public void unregisterTrack( MetroTrack track ) {
        checkState();

        if ( DEBUG ) { 
            logInfo( "****** DESTROYED a track is destroyed " + ( track == null ? "(null track)" : track.getUniqueID() ) );
        }
        if ( track == null )
            throw new NullPointerException( "track was null" );
        synchronized (getMetroLock()) {
            this.unregisteredTracks.add( track );
            // ADDED (Sun, 10 May 2020 23:10:26 +0900)
            notifyTrackChange("unregister-track");
        }
    }
    public void unregisterTrack( Collection<MetroTrack> trackList ) {
        checkState();

        if ( DEBUG ) 
            logInfo( "MetroTrack#unregisterTrack(Collection)" );

        if ( trackList == null )
            throw new NullPointerException();
        
        synchronized (getMetroLock()) {
            this.unregisteredTracks.addAll( trackList );
        }
    }
    
    /**
     * Post a new message which is denoted by a runnable objects. The messages are
     * invoked by the main thread of this Metro instance.
     * 
     * @param runnable
     *            the procedure to run at a later time.
     */
    public void postMessage( Runnable runnable ) {
        checkState();

        if ( DEBUG )
            logInfo( "****** postMessage 1");
        synchronized ( getMetroLock() ) {
            this.messageQueue.add( runnable );
            this.notifyTrackChange("update");
        }
    }

    private void removeFormerTrack(MetroTrack track ) {
        MetroSyncType syncType;
        MetroTrack syncTrack;
        double syncOffset;
        if (track instanceof MetroSyncTrack ) {
            MetroSyncTrack st = (MetroSyncTrack) track;
            syncType = st.getSyncType();
            syncTrack = st.getSyncTrack();
            syncOffset = st.getSyncOffset();
        } else {
            syncType = MetroSyncType.IMMEDIATE;
            syncTrack = null;
            syncOffset = 0.0d;
        }
        removeTrack( searchTrack( track.getName() ), syncType, syncTrack, syncOffset );
    }
    private void removeFormerTrack(Collection<MetroTrack> trackList ) {
        for ( MetroTrack track : trackList ) {
            removeFormerTrack( track );
        }
    }
        
    public void putTrack(List<MetroTrack> trackList, MetroSyncType syncType, MetroTrack syncTrack, double syncOffset) {
        // TODO
        putTrack( trackList );
    }

    public void putTrack( Collection<MetroTrack> trackList )  {
        removeFormerTrack( trackList );
        registerTrack( trackList );
    }
    public void removeTrack( Collection<MetroTrack> trackList, MetroSyncType syncType, MetroTrack syncTrack, double syncOffset )  {
        for ( MetroTrack track : trackList ) {
            track.remove(this, syncType, syncTrack, syncOffset);
        }
    }

    @Override
    public void clientShutdown(JackClient client) {
        logInfo("JackAudio: Java MIDI thru test shutdown");
    }
    
    @Override
    public void updatePosition(JackClient invokingClient, JackTransportState state, int nframes, JackPosition position,
            boolean newPosition) {
        logInfo("JackAudio: the current position changed " + position );
        
    }

    private final JackPosition jackPosition = new JackPosition();
    public long getOneBarLengthInFrames() throws MetroException {
        synchronized ( this.getMetroLock() ) {
            return calcOneBarLengthInFrames( this, this.client, this.jackPosition );
        }
    }

    public static long calcOneBarLengthInFrames( Metro metro, JackClient client, JackPosition position) throws MetroException {
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

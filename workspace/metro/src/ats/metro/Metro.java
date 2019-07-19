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

package ats.metro;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.jaudiolibs.jnajack.Jack;
import org.jaudiolibs.jnajack.JackClient;
import org.jaudiolibs.jnajack.JackException;
import org.jaudiolibs.jnajack.JackMidi;
import org.jaudiolibs.jnajack.JackOptions;
import org.jaudiolibs.jnajack.JackPort;
import org.jaudiolibs.jnajack.JackPortFlags;
import org.jaudiolibs.jnajack.JackPortType;
import org.jaudiolibs.jnajack.JackPosition;
import org.jaudiolibs.jnajack.JackProcessCallback;
import org.jaudiolibs.jnajack.JackShutdownCallback;
import org.jaudiolibs.jnajack.JackStatus;
import org.jaudiolibs.jnajack.JackTimebaseCallback;
import org.jaudiolibs.jnajack.JackTransportState;

import ats.metro.MetroTrack.SyncType;

/**
 * 
 * @author Ats Oka
 */
public class Metro implements MetroLock, JackProcessCallback, JackShutdownCallback, JackTimebaseCallback, Runnable {
	public final Object lock = new Object();
	
	/**
	 * The every routines which access to the `tracks` field must synchronize
	 * to the object which can be retrieved by this getMetroLock() method. 
	 */
	@Override
	public final Object getMetroLock() {
		return lock;
	}
	
	static final Logger LOGGER = Logger.getLogger(Metro.class.getName());
	static void logError(String msg, Throwable e) {
		LOGGER.log(Level.SEVERE, msg, e);
	}
	static void logInfo(String msg) {
		// LOGGER.log(Level.INFO, msg);
		System.err.println(msg);
	}
	static void logWarn(String msg) {
		LOGGER.log(Level.WARNING, msg);
	}
    
	static final boolean DEBUG = false;
	
	protected Jack jack = null;
    protected JackClient client = null;
    protected Thread thread = null;
    protected final List<JackPort> inputPortList = new ArrayList<JackPort>();
    protected final List<JackPort> outputPortList = new ArrayList<JackPort>();;
    

    private final JackMidi.Event midiEvent = new JackMidi.Event();
//	private BlockingQueue<String> debugQueue = new LinkedBlockingQueue<String>();
//    private StringBuilder sb = new StringBuilder();
    
    protected final List<MetroTrack> tracks = new ArrayList<MetroTrack>();
    protected final List<MetroTrack> registeredTracks = new ArrayList<MetroTrack>();
    protected final List<MetroTrack> unregisteredTracks = new ArrayList<MetroTrack>();
	private final List<Runnable>  messageQueue = new ArrayList<Runnable>();
	protected MetroLogicList logicList = new MetroLogicList(tracks);
	
    private ArrayList<MetroAbstractMidiEvent> inputMidiEventList = new ArrayList<MetroAbstractMidiEvent>();
    private ArrayList<MetroAbstractMidiEvent> outputMidiEventList = new ArrayList<MetroAbstractMidiEvent>();
	private JackPosition position = new JackPosition();

	// zero means that to get the current bpm from Jack Transport.
	private double beatsPerMinute = 60;
	public double getBeatsPerMinute() {
		return beatsPerMinute;
	}
	public void setBeatsPerMinute(double beatsPerMinute) throws JackException {
		beatsPerMinute = beatsPerMinute < 0 ? 0 : beatsPerMinute;
		this.beatsPerMinute = beatsPerMinute;
		reprepareTrack( beatsPerMinute, this.beatsPerMinute );
	}

	private long beatsPerBar = 4;
	public long getBeatsPerBar() {
		return beatsPerBar;
	}
	public void setBeatsPerBar(long beatsPerBar) throws JackException {
		this.beatsPerBar = beatsPerBar;
		// reprepareTrack();
	}

	private transient boolean playing = false;

	public boolean getPlaying() {
		return this.playing;
	}
	public void setPlaying( boolean playing ) {
		this.playing = playing;
	}
	public boolean togglePlaying() {
		this.playing = ! this.playing;
		return this.playing ; 
	}
	public MetroLogicList getLogicList() {
		return logicList;
	}
	
	public static Metro startClient( String clientName, MetroSequence sequence ) throws JackException {
		try {
            Metro metro = new Metro();
            metro.putSequence( "main", sequence );
            metro.open( clientName );
            return metro;
        } catch (JackException ex) {
            logError( null, ex);
            throw ex;
        }
	}
    
	
	public void createInputPort(String inputPortName) throws JackException {
		this.inputPortList.add( 
				this.client.registerPort(
		    		inputPortName, 
		    		JackPortType.MIDI, JackPortFlags.JackPortIsInput
		    	)
			);
	}
	public void createOutputPort(String outputPortName) throws JackException {
		this.outputPortList.add( 
				this.client.registerPort(
		    		outputPortName, 
		    		JackPortType.MIDI, JackPortFlags.JackPortIsOutput
		    	)
			);
	}
	
	/*
	 * Note that  There is no `this.logics`. Instead of that all of MetroSequence objects are 
	 * wrapped by Track and stored as `this.tracks`.   
	 * (Sat, 18 Aug 2018 19:03:18 +0900)
	 */
	public void putSequence( String name, MetroSequence sequence ) {
		putSequence( name, null, sequence, SyncType.PARALLEL, null, 0.0d );
	}
	public void putSequence( String name, Collection<String> tags, MetroSequence sequence, SyncType syncType, Object syncTrackObject, double syncOffset ) {
		putTrack( createTrack( name, tags, sequence, syncType, syncTrackObject, syncOffset ) );
	}
	public void putSequence( String name, Collection<String> tags, MetroSequence sequence, SyncType syncType, MetroTrack syncTrack, double syncOffset ) {
		putTrack( createTrack( name, tags, sequence, syncType, syncTrack, syncOffset ) );
	}
	public MetroTrack getTrack( Object nameObject ) {
		if ( nameObject == null ) {
			return null;
		} else if ( nameObject instanceof MetroTrack ) {
			return (MetroTrack) nameObject;
		} else if ( nameObject instanceof String ) {
			return searchTrack( (String) nameObject );
		} else {
			throw new RuntimeException( "Unsupported object on syncTrack" );
		}
	}
	public MetroTrack getTrack( String name ) {
		return searchTrack( name );
	}
	public void putTrack( MetroTrack track ) {
		registerTrack( track );
	}
	public void removeTrack( MetroTrack track ) {
		unregisterTrack( track );
	}
	public void removeTrack( String name ) {
		removeTrack( getTrack( name ) );
	}
	public void removeTrack( Object nameObject ) {
		removeTrack( getTrack( nameObject ) );
	}
	public void removeTrackAll( Collection<MetroTrack> tracks ) {
		unregisterTrackAll( tracks );
	}

	public MetroTrack createTrack(  
			String name, Collection<String> tags, MetroSequence sequence,
			SyncType syncType, MetroTrack syncTrack, double syncOffset) 
	{
		return new MetroTrack( this, name, tags, sequence, syncType, syncTrack, syncOffset );
	}
	public MetroTrack createTrack(  
			String name, Collection<String> tags, MetroSequence sequence,
			SyncType syncType, Object syncTrackObject, double syncOffset) 
	{
		MetroTrack syncTrack = getTrack( syncTrackObject ); 
		return new MetroTrack( this, name, tags, sequence, syncType, syncTrack, syncOffset );
	}
	
	private MetroTrack searchTrack( String name ) {
		if ( "last!".equals( name )) {
			if ( tracks.size() == 0 ) {
				if ( DEBUG )
					logInfo( "searchTrack() last! null (last! was specified but the current track contains no element)" );
				return null;
			} else
				if ( DEBUG )
					logInfo( "searchTrack() last!" );
				return tracks.get( tracks.size() -1 );
		} else {
			name = name.intern();
			for ( Iterator<MetroTrack> i=tracks.iterator(); i.hasNext(); ) {
				MetroTrack track = i.next();
				if ( track.name == name ) {
					return track;
				}
			}
			if ( DEBUG )
				logInfo( "searchTrack() null" );
//			logWarn( "searchTrack() WARNING \"" + name + "\"  was not found." );
			return null;
		}
	}
	Collection<MetroTrack> searchTrackByTag( String tag ) {
		ArrayList<MetroTrack> list = new ArrayList<>(); 
		for ( Iterator<MetroTrack> i = tracks.iterator(); i.hasNext();  ) {
			 MetroTrack track = i.next();
			 if ( track.getTrackTags().contains( tag ) ) {
				 list.add( track );
			 }
		}
		return list;
	}
	
	public Collection<MetroTrack> getTrackByTag( String tag ) {
		return searchTrackByTag(tag);
	}
	public Collection<MetroTrack> getTrackByTags( Collection<String> tags ) {
		HashSet<MetroTrack> set = new HashSet<>();
		for ( String tag : tags ) {
			set.addAll( searchTrackByTag( tag ) );
		}
		return set;
	}


	public void removeLogicDirect( String name ) {
		Iterator<MetroTrack> iterator = lookupTrackDirect(name);
		if ( iterator != null  ) {
			iterator.remove();
		}
	}
	private Iterator<MetroTrack> lookupTrackDirect( String name ) {
		name = name.intern();
		for ( Iterator<MetroTrack> i=tracks.iterator(); i.hasNext(); ) {
			MetroTrack track = i.next();
			if ( track.name == name ) {
				return i;
			}
		}
		return null;
	}


	public void enableTrackByTag( String tag, boolean enabled ) {
		for ( MetroTrack track : searchTrackByTag( tag ) ) {
			track.setTrackEnabled( enabled );
		}
	}

	protected volatile boolean running = false;
    public void open( String clientName ) throws JackException {
    	logInfo( running + "===open()");
    	if ( running )
    		throw new RuntimeException( "Already Started" );
    	
    	logInfo("===open()");
    	
    	this.running = true;
    	
    	// Create a Jack client.
        EnumSet<JackStatus> status = EnumSet.noneOf(JackStatus.class);
        try {
            this.jack = Jack.getInstance();
            this.client = this.jack.openClient( clientName , EnumSet.of(JackOptions.JackNoStartServer ), status);
            if (!status.isEmpty()) {
                logInfo("JACK client status : " + status);
            }
        } catch (JackException ex) {
            if (!status.isEmpty()) {
                logInfo("JACK exception client status : " + status);
            }
            throw ex;
        }

        // Create a thread.
//    	this.sequence.setParent( this );
        this.activate();
        this.thread = new Thread( this );
        this.thread.start();
    }
    
    public void close() {
    	logInfo( running + "===close()");
    	if ( ! running )
    		return;
    	
    	logInfo("===close()");
    	
    	try {
    		try {
    			clearTracks();
    		} catch ( Exception e ) {
				logError("", e);
    		}
    		if ( thread != null ) {
    			thread.interrupt();
    			try {
    				thread.join();
    			} catch (InterruptedException e) {
    				logError("", e);
    			}
    		}
    		if ( this.client != null ) {
    			this.client.close();
    		}
    	} finally {
    		this.running = false;
    		this.client = null;
    		this.thread = null;
    	}
    }
    
    /*
     * NOTE : 
     * The reason that the start() method throws an exception when it is called
	 * duplicately in the meantime the stop() method just ignores it.
	 * 
	 * When a user duplicately start a server, this maybe because of mistakenly
	 * starting the server without any clean up the before state; therefore it
	 * should report it to the user.
	 * 
	 * When a user duplicately stop a server, this maybe because of the user want to
	 * clean up the server's state. In this case, the user usually want to stop the
	 * server no matter it is already running or not.
	 */
    
    // ???
    public void refreshTracks() {
		synchronized ( this.lock ) {
			for ( MetroTrack s : this.tracks ) {
				s.clearBuffer();
			}
        	for ( MetroTrack track : this.tracks  ) {
        		try {
					track.checkBuffer( this,  this.client, this.position );
				} catch (JackException e) {
					logError("", e);
				}
        	}
		}
    }
    public void clearTracks() {
		synchronized ( this.lock ) {
			this.tracks.clear();
			this.notifyCheckBuffer();
		}
    }
    
	public void run()  {
		try {
			logInfo("Metro.run()");
	        while ( true ) {
	//        	logInfo("Metro.run()");
	//        	String s = this.debugQueue.take();
	//        	System.err.println( s );
	        	
				synchronized ( this.lock ) {
					for ( MetroTrack track : this.tracks  ) {
						track.checkBuffer( this,  this.client, this.position );
					}

					for ( Runnable r : this.messageQueue ) {
						try {
							r.run();
						} catch ( Throwable e ) {
							logError( "An error occured in a message object", e);
						}
					}
					
					
					this.tracks.removeAll(this.unregisteredTracks );
					this.tracks.addAll( this.registeredTracks );
					
					if ( ! this.registeredTracks.isEmpty() ) {
						int barInFrames = Metro.calcBarInFrames( this, client, position);
						for ( MetroTrack track : this.registeredTracks ) {
							track.prepare( barInFrames );
							// ADDED (Sun, 30 Sep 2018 12:39:32 +0900)
							track.checkBuffer( this,  this.client, this.position );
						}
					}		
					this.unregisteredTracks.clear();
					this.registeredTracks.clear();
					this.messageQueue.clear(); // FIXED (Sun, 30 Sep 2018 01:41:24 +0900) 
					
					this.lock.wait( 1 );
				}
//				logInfo( this.tracks.size() );
//	        	Thread.sleep(0);
	        }
		} catch ( InterruptedException e ) {
			// ignore
		} catch ( Throwable e ) {
			throw new RuntimeException( e );
		}
	}
	
	void reprepareTrack(double prevBeatsPerMinute, double beatsPerMinute) throws JackException {
		synchronized ( this.lock ) {
			// int barInFrames = Metro.calcBarInFrames( this, this.client, this.position );
			for ( MetroTrack track : this.tracks ) {
				track.reprepare( this, this.client, this.position, prevBeatsPerMinute, beatsPerMinute );
			}
		}
	}
	
	public List<String> getOutputPorts() throws JackException {
    	String[] ports = this.jack.getPorts( this.client, "", JackPortType.MIDI, EnumSet.of( JackPortFlags.JackPortIsOutput ) );
    	return new ArrayList<String>( Arrays.asList( ports ) );
	}
	public List<String> getInputPorts() throws JackException {
    	String[] ports = this.jack.getPorts( this.client, "", JackPortType.MIDI, EnumSet.of( JackPortFlags.JackPortIsInput ) );
    	return new ArrayList<String>( Arrays.asList( ports ) );
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
        	String[] ports = this.jack.getPorts( this.client, "", JackPortType.MIDI, EnumSet.of( JackPortFlags.JackPortIsInput ) );
        	for ( String s : ports ) {
        		logInfo( s );
        	}
        }
        logInfo( "// OUTPUT //");
        {
        	String[] ports = this.jack.getPorts( this.client, "", JackPortType.MIDI, EnumSet.of( JackPortFlags.JackPortIsOutput ) );
        	for ( String s : ports ) {
        		logInfo( s );
        	}
        }
    }
    
	public void connectPort( Map<String, String> map ) {
		for ( Entry<String, String> portName : map.entrySet() ) {
			String key = portName.getKey();
			String value = portName.getValue();
			if ( key != null && value != null ) {
				try {
					connectPort(key, value);
				} catch (JackException e ) {
					e.printStackTrace();
				}
			}
		}
	}
	public void connectPort(String source, String desitination) throws JackException {
		try {
			this.jack.connect( this.client, source, desitination);
		} catch ( JackException e ) {
			throw new JackException( "Error occured while connecting from " + source + " to " + desitination, e );
		}
	}
    
	public void clearAllPorts() throws JackException {
		Metro metro = this;
		for ( JackPort p : metro.outputPortList ) {
			JackMidi.clearBuffer( p );
		}
	}

    @Override
    public boolean process(JackClient client, int nframes) {
    	if ( ! this.playing ) {
    		return true;
    	}
    	
		try {
            // JackMidi.clearBuffer(this.outputPort);
            for ( JackPort p : Metro.this.outputPortList )
            	JackMidi.clearBuffer( p );
            
            
            this.inputMidiEventList.clear();
        	this.outputMidiEventList.clear();
            
        	int inputPortNo = 0;
        	for ( Iterator<JackPort> pi = this.inputPortList.iterator(); pi.hasNext(); ) {
        		JackPort inputPort = pi.next();
        		
                int eventCount = JackMidi.getEventCount( inputPort );
                for (int i = 0; i < eventCount; ++i) {
                	JackMidi.eventGet( this.midiEvent, inputPort, i );
                	int size = this.midiEvent.size();
                	byte[] data = new byte[size];
                	this.midiEvent.read( data );
                	this.inputMidiEventList.add( new MetroStaticMidiEvent( inputPortNo, this.midiEvent.time(), data ) );

//                    this.sb.setLength(0);
//                    this.sb.append(this.midiEvent.time());
//                    this.sb.append(": ");
//                    for (int j = 0; j < size; j++) {
//                        this.sb.append((j == 0) ? "" : ", ");
//                        this.sb.append(this.data[j] & 0xFF);
//                    }
//                     debugQueue.offer(sb.toString());

//                    if ( 0 < Metro.this.outputPortList.size() )
//                    	JackMidi.eventWrite( Metro.this.outputPortList.get( 0 ), this.midiEvent.time(), this.data, this.midiEvent.size());
                }
//                logInfo( "ev:" + eventCount + " this.outputMidiEventList  : " + this.outputMidiEventList.size() );
        	}

            synchronized ( this.lock ) {
                if ( 0 < this.inputMidiEventList.size() )
	            	for ( MetroTrack track : this.tracks ) {
	            		track.sequence.processDirect( this, this.inputMidiEventList, this.outputMidiEventList );
	            	}
                
                if ( true )
	            	for ( MetroTrack track : this.tracks ) {
	            		track.progressCursor( nframes, this.outputMidiEventList );
	            	}
                
            	this.outputMidiEventList.sort( MetroAbstractMidiEvent.COMPARATOR );
            	
            	if ( ! this.outputMidiEventList.isEmpty() )
            		if ( DEBUG ) logInfo( this.outputMidiEventList.toString() );

            	for ( MetroAbstractMidiEvent e : this.outputMidiEventList ) {
            		JackMidi.eventWrite( 
            				Metro.this.outputPortList.get( e.getOutputPortNo() ), 
            				e.getMidiOffset(), 
            				e.getMidiData(), 
            				e.getMidiData().length 
            				);
            	}
            	
//    			JackMidi.eventWrite( 
//    					Metro.this.outputPortList.get( e.getOutputPortNo() ), 
//    					e.getOffsetInFrames() - this.cursor, 
//    					e.getData(), 
//    					e.getData().length 
//    					);
            }
            
            return true;
        } catch (JackException ex) {
            logError( "ERROR" , ex);
            return false;
        }
    }
    
    /*
     * NOTE : DON'T FORGET THIS
     * 
     * Two methods registerTrack() and unregisterTrack() are registering 
     * the given track object via two queues : registeredTracks / unregisteredTracks.
     * These lists are referred by Metro.run() method which is executed by another thread. 
     * 
     */
    protected void registerTrack( MetroTrack track ) {
		if ( DEBUG ) 
			logInfo( "****** CREATED a new track is created " + track.id );
		synchronized ( this.lock ) {
			MetroTrack foundTrack = null;
			{
				for ( MetroTrack track0 : this.tracks  ) {
					if ( track0.name.equals( track.name ) ) {
						foundTrack = track0;
						break;
					}
				}
			}

			this.registeredTracks.add( track );
			if ( foundTrack != null) {
				this.unregisteredTracks.add( foundTrack );
			}
			this.notifyCheckBuffer();
		}
	}
	protected void unregisterTrack( MetroTrack track ) {
		if ( DEBUG ) 
			logInfo( "****** DESTROYED a track is destroyed " + ( track == null ? "(null track)" : track.id ) );
		synchronized ( this.lock ) {
			this.unregisteredTracks.add( track );
			this.notifyCheckBuffer();
		}
	}
	protected void unregisterTrackAll( Collection<MetroTrack> tracks ) {
		synchronized ( this.lock ) {
			this.unregisteredTracks.addAll( tracks );
			this.notifyCheckBuffer();
		}
	}
    protected void postMessage( Runnable runnable ) {
		if ( DEBUG )
			logInfo( "****** postMessage 1");
		synchronized ( this.lock ) {
			this.messageQueue.add( runnable );
			this.notifyCheckBuffer();
		}
	}

	protected void notifyCheckBuffer() {
		synchronized ( this.lock ) {
			this.lock.notify();
		}
	}

//	public void registerTrackImmediately(Track track, Track syncTrack, 
//			double offset) 
//	{
//		if ( DEBUG ) 
//			logInfo( "****** CREATED a new track is created " + track.id );
//		
//		synchronized ( this.lock ) {
//			this.tracks.add( track );
//			track.prepare(barInFrames);
//		}
//	}


    @Override
    public void clientShutdown(JackClient client) {
        logInfo("Java MIDI thru test shutdown");
    }
	@Override
	public void updatePosition(JackClient invokingClient, JackTransportState state, int nframes, JackPosition position,
			boolean newPosition) {
		// logInfo("*****************************************TEMPO");
		
	}

	public static int calcBarInFrames( Metro metro, JackClient client, JackPosition position) throws JackException {
		// logInfo("Metro.offerNewBuffer()" + this.buffers.size() );
		// beat per minute
		double bpm;
		// beat per bar 
		double bpb;
		int frameRate;
		try {
			client.transportQuery( position );
			bpm = position.getBeatsPerMinute();
			bpb = position.getBeatsPerBar();
			frameRate = position.getFrameRate();
			
			// logInfo( "framerate" + frameRate + " bpb="+ bpb );
		} catch (JackException e) {
			throw e;
		}
		
		double ownBeatPerMinute = metro.getBeatsPerMinute();
		long ownBeatPerBar    = metro.getBeatsPerBar();
		
		if ( 0 < ownBeatPerMinute ) {
			bpm = ownBeatPerMinute;
			bpb = ownBeatPerBar;
		}
		
		double beatInSecond = 1.0d / ( bpm / 60.0d /*seconds*/ );
		int barInFrames = (int) (beatInSecond * frameRate * bpb);
		return barInFrames;
	}
	
	
	
}

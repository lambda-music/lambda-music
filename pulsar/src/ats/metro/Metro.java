/*
 * MIDI support for JNAJack - Example MIDI thru JACK client application.
 * Copyright (C) 2014 Neil C Smith, derived from code by Salvatore Isaja
 * 
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License,
 * or (at your option) any later version.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this work; if not, see http://www.gnu.org/licenses/
 */
package ats.metro;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.EnumSet;
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

import ats.metro.MetroNoteEventBufferSequence.SyncType;


public class Metro implements MetroLock, JackProcessCallback, JackShutdownCallback, JackTimebaseCallback, Runnable {
	public final Object lock = new Object();
	@Override
	public final Object getMetroLock() {
		return lock;
	}
    static void logInfo( String msg ) {
    	System.err.println( msg );
		// Logger.getLogger(Metro.class.getName()).log(Level.INFO, msg );
    }
    static void logError( String msg, Throwable e ) {
		Logger.getLogger(Metro.class.getName()).log(Level.SEVERE, msg, e);
    }
    
	//	private final static boolean DEBUG = true;
	static final boolean DEBUG = false;
	
	protected Jack jack = null;
    protected JackClient client = null;
    protected Thread thread = null;
    protected final List<JackPort> inputPortList = new ArrayList<JackPort>();
    protected final List<JackPort> outputPortList = new ArrayList<JackPort>();;
    

    private final JackMidi.Event midiEvent = new JackMidi.Event();
//	private BlockingQueue<String> debugQueue = new LinkedBlockingQueue<String>();
//    private StringBuilder sb = new StringBuilder();
    
    protected List<MetroNoteEventBufferSequence> sequences = new ArrayList<MetroNoteEventBufferSequence>();
    protected List<MetroNoteEventBufferSequence> registeredSequences = new ArrayList<MetroNoteEventBufferSequence>();
    protected List<MetroNoteEventBufferSequence> unregisteredSeqences = new ArrayList<MetroNoteEventBufferSequence>();
	private List<Runnable>  messageQueue = new ArrayList<Runnable>();
	protected MetroLogicList logicList = new MetroLogicList(sequences);
	
    private ArrayList<MetroMidiEvent> inputMidiEventList = new ArrayList<MetroMidiEvent>();
    private ArrayList<MetroMidiEvent> outputMidiEventList = new ArrayList<MetroMidiEvent>();
	private JackPosition position = new JackPosition();

	protected MetroLogic logic;
	
	// zero means that to get the current bpm from Jack Transport.
	private double beatsPerMinute = 60;
	public double getBeatsPerMinute() {
		return beatsPerMinute;
	}
	public void setBeatPerMinute(double barPerMinute) throws JackException {
		this.beatsPerMinute = barPerMinute < 0 ? 0 : barPerMinute;
		reprepareSequence();
	}

	private long beatsPerBar = 4;
	public long getBeatsPerBar() {
		return beatsPerBar;
	}
	public void setBeatsPerBar(long beatsPerBar) throws JackException {
		this.beatsPerBar = beatsPerBar;
		reprepareSequence();
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
	
	public static Metro startClient( String clientName, MetroLogic logic ) throws JackException {
		try {
            Metro metro = new Metro();
            metro.addLogic( "main", logic );
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
	 * Note that  There is no `this.logics`. Instead of that all of Logic objects are 
	 * wrapped by MetroNoteEventBufferSequence and stored as `this.sequences`.   
	 * (Sat, 18 Aug 2018 19:03:18 +0900)
	 */
	public void addLogic( String name, MetroLogic logic ) {
		addLogic( name, null, logic, SyncType.PARALLEL, null, 0.0d );
	}

	public void addLogic( String name, Collection<String> tags, MetroLogic logic, SyncType syncType, Object syncSequenceObj, double syncOffset ) {
		MetroNoteEventBufferSequence syncSequence;
		if ( syncSequenceObj == null ) {
			syncSequence = null;
		} else if ( syncSequenceObj instanceof MetroNoteEventBufferSequence ) {
			syncSequence = (MetroNoteEventBufferSequence) syncSequenceObj;
		} else if ( syncSequenceObj instanceof String ) {
			syncSequence = searchSequence( (String) syncSequenceObj );
//			logInfo( "syncSequenceObj" + syncSequenceObj );
//			logInfo( "syncSequence" + syncSequence );
		} else {
			throw new RuntimeException( "Unsupported object on syncSequence" );
		}
		
		MetroNoteEventBufferSequence sequence = createSequence(name, tags, logic, syncType, syncOffset, syncSequence);
		logic.setPlayer( sequence );
		registerSequence( sequence );
	}
	public MetroNoteEventBufferSequence createSequence( 
			String name, Collection<String> tags, MetroLogic logic,
			SyncType syncType, double syncOffset, MetroNoteEventBufferSequence syncSequence) 
	{
		return new MetroNoteEventBufferSequence( this, name, tags, logic, syncType, syncSequence, syncOffset );
	}

	
	
	private MetroNoteEventBufferSequence searchSequence( String name ) {
		if ( "last!".equals(name )) {
			if ( sequences.size() == 0 ) {
				logInfo( "searchSequence() WARNING last! was specified but sequence contains no element." );
				return null;
			} else
				return sequences.get( sequences.size() -1 );
		} else {
			name = name.intern();
			for ( Iterator<MetroNoteEventBufferSequence> i=sequences.iterator(); i.hasNext(); ) {
				MetroNoteEventBufferSequence sequence = i.next();
				if ( sequence.name == name ) {
					return sequence;
				}
			}
			logInfo( "searchSequence() WARNING \"" + name + "\"  was not found." );
			return null;
		}
	}

	public void removeLogic( String name ) {
		Iterator<MetroNoteEventBufferSequence> iterator = lookupSequence(name);
		if ( iterator != null  ) {
			iterator.remove();
		}
	}
	public boolean hasLogic( String name ) {
		return lookupSequence(name) != null;
	}
	private Iterator<MetroNoteEventBufferSequence> lookupSequence( String name ) {
		name = name.intern();
		for ( Iterator<MetroNoteEventBufferSequence> i=sequences.iterator(); i.hasNext(); ) {
			MetroNoteEventBufferSequence sequence = i.next();
			if ( sequence.name == name ) {
				return i;
			}
		}
		return null;
	}
	public void removeAllLogic() {
		this.sequences.clear();
	}
	
	public void enableSequenceByTag( String tag, boolean enabled ) {
		for ( Iterator<MetroNoteEventBufferSequence> i = sequences.iterator(); i.hasNext();  ) {
			 MetroNoteEventBufferSequence sequence = i.next();
			 if ( sequence.getPlayerTags().contains( tag ) ) {
				 sequence.setPlayerEnabled( enabled );
			 }
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
//    	this.logic.setParent( this );
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
    			clearSequences();
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
    public void refreshSequences() {
		synchronized ( this.lock ) {
			for ( MetroNoteEventBufferSequence s : this.sequences ) {
				s.clearBuffer();
			}
        	for ( MetroNoteEventBufferSequence sequence : this.sequences  ) {
        		try {
					sequence.checkBuffer( this,  this.client, this.position );
				} catch (JackException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
        	}
		}
    }
    public void clearSequences() {
		synchronized ( this.lock ) {
			this.sequences.clear();
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
	        	for ( MetroNoteEventBufferSequence sequence : this.sequences  ) {
	        		sequence.checkBuffer( this,  this.client, this.position );
	        	}
				synchronized ( this.lock ) {
					for ( Runnable r : this.messageQueue ) {
						try {
							r.run();
						} catch ( Throwable e ) {
							logError( "An error occured in a message object", e);
						}
					}
					
					
					this.sequences.removeAll(this.unregisteredSeqences );
					this.sequences.addAll( this.registeredSequences );
					
					if ( ! this.registeredSequences.isEmpty() ) {
						int barInFrames = Metro.calcBarInFrames( this, client, position);
						for ( MetroNoteEventBufferSequence sequence : this.registeredSequences ) {
							sequence.prepare( barInFrames );
						}
					}		
					this.unregisteredSeqences.clear();
					this.registeredSequences.clear();
					
					this.lock.wait( 1 );
				}
//				logInfo( this.sequences.size() );
//	        	Thread.sleep(0);
	        }
		} catch ( InterruptedException e ) {
			// ignore
		} catch ( Throwable e ) {
			throw new RuntimeException( e );
		}
	}
	
	void reprepareSequence() throws JackException {
		synchronized ( this.lock ) {
			// int barInFrames = Metro.calcBarInFrames( this, this.client, this.position );
			for ( MetroNoteEventBufferSequence sequence : this.sequences ) {
				sequence.reprepare( this, this.client, this.position );
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
                	this.inputMidiEventList.add( new MetroMidiEvent( inputPortNo, this.midiEvent.time(), data ) );

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
	            	for ( MetroNoteEventBufferSequence sequence : this.sequences ) {
	            		sequence.logic.processInputMidiBuffer( this, this.inputMidiEventList, this.outputMidiEventList );
	            	}
                
                if ( true )
	            	for ( MetroNoteEventBufferSequence sequence : this.sequences ) {
	            		sequence.progressCursor( nframes, this.outputMidiEventList );
	            	}
                
            	this.outputMidiEventList.sort( MetroMidiEvent.COMPARATOR );
            	
            	if ( ! this.outputMidiEventList.isEmpty() )
            		if ( DEBUG ) logInfo( this.outputMidiEventList.toString() );

            	for ( MetroMidiEvent e : this.outputMidiEventList ) {
            		JackMidi.eventWrite( 
            				Metro.this.outputPortList.get( e.getOutputPortNo() ), 
            				e.getOffset(), 
            				e.getData(), 
            				e.getData().length 
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
     * Two methods registerSequence() and unregisterSequence() are registering 
     * the given sequence object via two cues : registeredSequences / unregisteredSequences.
     * These lists are referred by Metro.run() method which is executed by another thread. 
     * 
     */
    protected void registerSequence( MetroNoteEventBufferSequence sequence ) {
		if ( DEBUG ) 
			logInfo( "****** CREATED a new sequence is created " + sequence.id );
		synchronized ( this.lock ) {
			this.registeredSequences.add( sequence );
			this.notifyCheckBuffer();
		}
	}
	protected void unregisterSequence( MetroNoteEventBufferSequence sequence ) {
		if ( DEBUG ) 
			logInfo( "****** DESTROYED a sequence is destroyed " + sequence.id );
		synchronized ( this.lock ) {
			this.unregisteredSeqences.add( sequence );
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

//	public void registerSequenceImmediately(MetroNoteEventBufferSequence sequence, MetroNoteEventBufferSequence syncSequence, 
//			double offset) 
//	{
//		if ( DEBUG ) 
//			logInfo( "****** CREATED a new sequence is created " + sequence.id );
//		
//		synchronized ( this.lock ) {
//			this.sequences.add( sequence );
//			sequence.prepare(barInFrames);
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

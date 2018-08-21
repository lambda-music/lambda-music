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


public class Metro implements JackProcessCallback, JackShutdownCallback, JackTimebaseCallback, Runnable {
//	private final static boolean DEBUG = true;
	static final boolean DEBUG = false;
	
	protected Jack jack = null;
    protected JackClient client = null;
    protected final List<JackPort> inputPortList = new ArrayList<JackPort>();
    protected final List<JackPort> outputPortList = new ArrayList<JackPort>();;
    

    private final JackMidi.Event midiEvent = new JackMidi.Event();
//	private BlockingQueue<String> debugQueue = new LinkedBlockingQueue<String>();
//    private StringBuilder sb = new StringBuilder();
    
    protected List<MetroNoteEventBufferSequence> sequences = new ArrayList<MetroNoteEventBufferSequence>();
    protected List<MetroNoteEventBufferSequence> registeredSequences = new ArrayList<MetroNoteEventBufferSequence>();
    protected List<MetroNoteEventBufferSequence> unregisteredSeqences = new ArrayList<MetroNoteEventBufferSequence>();
    private ArrayList<MetroMidiEvent> inputMidiEventList = new ArrayList<MetroMidiEvent>();
    private ArrayList<MetroMidiEvent> outputMidiEventList = new ArrayList<MetroMidiEvent>();
	private JackPosition position = new JackPosition();

	protected MetroLogic logic;
	
	// zero means that to get the current bpm from Jack Transport.
	private double beatsPerMinute = 60;
	public double getBeatsPerMinute() {
		return beatsPerMinute;
	}
	public void setBeatsPerMinute(double barPerMinute) throws JackException {
		this.beatsPerMinute = barPerMinute < 0 ? 0 : barPerMinute;
		prepareSequence();
	}

	private long beatsPerBar = 4;
	public long getBeatsPerBar() {
		return beatsPerBar;
	}
	public void setBeatsPerBar(long beatsPerBar) throws JackException {
		this.beatsPerBar = beatsPerBar;
		prepareSequence();
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
	
	public static Metro startClient( String clientName, MetroLogic logic ) throws JackException {
		try {
            Metro metro = new Metro();
            metro.putLogic( "main", logic );
            metro.start( clientName );
            return metro;
        } catch (JackException ex) {
            Logger.getLogger(Metro.class.getName()).log(Level.SEVERE, null, ex);
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
	public void putLogicOld( String name, MetroLogic logic ) {
		MetroNoteEventBufferSequence sequence = new MetroNoteEventBufferSequence( this, name, logic, SyncType.PARALLEL , null, 0.0d );
		this.sequences.add( sequence );
	}
	
	/*
	 * Note that  There is no `this.logics`. Instead of that all of Logic objects are 
	 * wrapped by MetroNoteEventBufferSequence and stored as `this.sequences`.   
	 * (Sat, 18 Aug 2018 19:03:18 +0900)
	 */
	public void putLogic( String name, MetroLogic logic ) {
		putLogic( name, logic, SyncType.PARALLEL, null, 0.0d );
	}

	public void putLogic( String name, MetroLogic logic, SyncType syncType, Object syncSequenceObj, double syncOffset ) {
		
		MetroNoteEventBufferSequence syncSequence;
		if ( syncSequenceObj == null ) {
			syncSequence = null;
		} else if ( syncSequenceObj instanceof MetroNoteEventBufferSequence ) {
			syncSequence = (MetroNoteEventBufferSequence) syncSequenceObj;
		} else if ( syncSequenceObj instanceof String ) {
			syncSequence = searchSequence( (String) syncSequenceObj );
//			System.out.println( "syncSequenceObj" + syncSequenceObj );
//			System.out.println( "syncSequence" + syncSequence );
		} else {
			throw new RuntimeException( "Unsupported object on syncSequence" );
		}
		
		MetroNoteEventBufferSequence sequence = 
				new MetroNoteEventBufferSequence( this, name, logic, syncType, syncSequence, syncOffset );
		registerSequence( sequence );
	}
	
	
	private MetroNoteEventBufferSequence searchSequence( String name ) {
		if ( "last!".equals(name )) {
			if ( sequences.size() == 0 )
				return null;
			else
				return sequences.get( sequences.size() -1 );
		} else {
			name = name.intern();
			for ( Iterator<MetroNoteEventBufferSequence> i=sequences.iterator(); i.hasNext(); ) {
				MetroNoteEventBufferSequence sequence = i.next();
				if ( sequence.name == name ) {
					return sequence;
				}
			}
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

    public void start( String clientName ) throws JackException {
    	// Create a Jack client.
        EnumSet<JackStatus> status = EnumSet.noneOf(JackStatus.class);
        try {
            this.jack = Jack.getInstance();
            this.client = this.jack.openClient( clientName , EnumSet.of(JackOptions.JackNoStartServer), status);
            if (!status.isEmpty()) {
                System.out.println("JACK client status : " + status);
            }
        } catch (JackException ex) {
            if (!status.isEmpty()) {
                System.out.println("JACK exception client status : " + status);
            }
            throw ex;
        }

        // Create a thread.
//    	this.logic.setParent( this );
        this.activate();
        new Thread( this ).start();
    }
    
    public void clearSequences() {
		synchronized ( this.sequences ) {
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
    public void resetSequences() {
		synchronized ( this.sequences ) {
			this.sequences.clear();
			this.notifyCheckBuffer();
		}
    }
    
	public void run()  {
		try {
			System.out.println("Metro.run()");
	        while ( true ) {
	//        	System.out.println("Metro.run()");
	//        	String s = this.debugQueue.take();
	//        	System.err.println( s );
	        	for ( MetroNoteEventBufferSequence sequence : this.sequences  ) {
	        		sequence.checkBuffer( this,  this.client, this.position );
	        	}
				synchronized ( this.sequences ) {
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
					
					this.sequences.wait( 1 );
				}
//				System.out.println( this.sequences.size() );
//	        	Thread.sleep(0);
	        }
		} catch ( Throwable e ) {
			throw new RuntimeException( e );
		}
	}
	
	void prepareSequence() throws JackException {
		synchronized ( this.sequences ) {
			// int barInFrames = Metro.calcBarInFrames( this, this.client, this.position );
			for ( MetroNoteEventBufferSequence sequence : this.sequences ) {
				sequence.reprepare( this, this.client, this.position );
			}
		}
	}

    private void activate() throws JackException {
        this.client.setProcessCallback(this);
        this.client.setTimebaseCallback(this, false);
        this.client.onShutdown(this);
        this.client.activate();
        
        System.err.println( "========================");
        System.err.println( "a list of all Jack ports" );
        System.err.println( "// INPUT //");
        {
        	String[] ports = this.jack.getPorts( this.client, "", JackPortType.MIDI, EnumSet.of( JackPortFlags.JackPortIsInput ) );
        	for ( String s : ports ) {
        		System.err.println( s );
        	}
        }
        System.err.println( "// OUTPUT //");
        {
        	String[] ports = this.jack.getPorts( this.client, "", JackPortType.MIDI, EnumSet.of( JackPortFlags.JackPortIsOutput ) );
        	for ( String s : ports ) {
        		System.err.println( s );
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
//                System.err.println( "ev:" + eventCount + " this.outputMidiEventList  : " + this.outputMidiEventList.size() );
        	}
        		

            if ( 0 < this.inputMidiEventList.size() ) {
            	for ( MetroNoteEventBufferSequence sequence : this.sequences ) {
            		sequence.progressCursor( nframes, this.outputMidiEventList );
            		sequence.logic.processInputMidiBuffer( this, this.inputMidiEventList, this.outputMidiEventList );
            	}
            }
            
            synchronized ( this.sequences ) {
            	for ( MetroNoteEventBufferSequence sequence : this.sequences ) {
            		sequence.progressCursor( nframes, this.outputMidiEventList );
            	}
            	this.outputMidiEventList.sort( MetroMidiEvent.COMPARATOR );
            	
            	if ( ! this.outputMidiEventList.isEmpty() )
            		if ( DEBUG ) System.err.println( this.outputMidiEventList );

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
            System.err.println("ERROR : " + ex);
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
     * 
     * 
     */
    protected void registerSequence( MetroNoteEventBufferSequence sequence ) {
		if ( DEBUG ) 
			System.err.println( "****** CREATED a new sequence is created " + sequence.id );
		this.registeredSequences.add( sequence );
		this.notifyCheckBuffer();
	}
	protected void unregisterSequence( MetroNoteEventBufferSequence sequence ) {
		if ( DEBUG ) 
			System.err.println( "****** DESTROYED a sequence is destroyed " + sequence.id );
		this.unregisteredSeqences.add( sequence );
		this.notifyCheckBuffer();
	}

	protected void notifyCheckBuffer() {
		synchronized ( this.sequences ) {
			this.sequences.notify();
		}
	}

//	public void registerSequenceImmediately(MetroNoteEventBufferSequence sequence, MetroNoteEventBufferSequence syncSequence, 
//			double offset) 
//	{
//		if ( DEBUG ) 
//			System.err.println( "****** CREATED a new sequence is created " + sequence.id );
//		
//		synchronized ( this.sequences ) {
//			this.sequences.add( sequence );
//			sequence.prepare(barInFrames);
//		}
//	}


    @Override
    public void clientShutdown(JackClient client) {
        System.out.println("Java MIDI thru test shutdown");
    }
	@Override
	public void updatePosition(JackClient invokingClient, JackTransportState state, int nframes, JackPosition position,
			boolean newPosition) {
		System.err.println("*****************************************TEMPO");
		
	}

	public static int calcBarInFrames( Metro metro, JackClient client, JackPosition position) throws JackException {
		// System.out.println("Metro.offerNewBuffer()" + this.buffers.size() );
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
			
			// System.out.println( "framerate" + frameRate + " bpb="+ bpb );
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

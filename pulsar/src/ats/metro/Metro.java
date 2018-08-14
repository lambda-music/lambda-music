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
import java.util.List;
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


public class Metro implements JackProcessCallback, JackShutdownCallback, JackTimebaseCallback, Runnable {
//	private final static boolean DEBUG = true;
	static final boolean DEBUG = false;
	
	private final Jack jack;
    private final JackClient client;
    private final JackPort inputPort;
    // private final JackPort outputPort;
//    private final List<JackPort> inputPortList;
    private final List<JackPort> outputPortList;
    
    private final JackMidi.Event midiEvent = new JackMidi.Event();
//	private BlockingQueue<String> debugQueue = new LinkedBlockingQueue<String>();
//    private StringBuilder sb = new StringBuilder();
    
    private List<MetroNoteEventBufferSequence> sequences = new ArrayList<MetroNoteEventBufferSequence>();
    List<MetroNoteEventBufferSequence> registeredSequences = new ArrayList<MetroNoteEventBufferSequence>();
    List<MetroNoteEventBufferSequence> unregisteredSeqences = new ArrayList<MetroNoteEventBufferSequence>();
    private ArrayList<MetroMidiEvent> inputMidiEventList = new ArrayList<MetroMidiEvent>();
    private ArrayList<MetroMidiEvent> outputMidiEventList = new ArrayList<MetroMidiEvent>();
	private JackPosition position = new JackPosition();

	private MetroMasterLogic logic;
	
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

    // private MetroLogic logic;
    
	public static void startClient( MetroMasterLogic logic ) {
		try {
            Metro metro = new Metro( logic );
            metro.start();
        } catch (Exception ex) {
            Logger.getLogger(Metro.class.getName()).log(Level.SEVERE, null, ex);
        }
	}
    
	public Metro( MetroMasterLogic logic ) throws JackException {
		this.logic = logic;
		this.sequences.add( new MetroNoteEventBufferSequence( this, null, logic, 0.0d ) );
		
        EnumSet<JackStatus> status = EnumSet.noneOf(JackStatus.class);
        try {
            this.jack = Jack.getInstance();
            this.client = this.jack.openClient( logic.clientName() , EnumSet.of(JackOptions.JackNoStartServer), status);
            if (!status.isEmpty()) {
                System.out.println("JACK client status : " + status);
            }
            
            this.inputPort = this.client.registerPort("MIDI in", JackPortType.MIDI, JackPortFlags.JackPortIsInput);
            // this.outputPort = this.client.registerPort("MIDI out", JackPortType.MIDI, JackPortFlags.JackPortIsOutput);
            this.outputPortList = new ArrayList<JackPort>();
            	
            for ( String outputPortName : logic.outputPortNameList() ) {
                this.outputPortList.add( 
                		this.client.registerPort(
	                		outputPortName, 
	                		JackPortType.MIDI, JackPortFlags.JackPortIsOutput
	                	)
            		);
            }

            for ( String inputPortName : logic.inputPortNameList() ) {
                this.outputPortList.add( 
                		this.client.registerPort(
	                		inputPortName, 
	                		JackPortType.MIDI, JackPortFlags.JackPortIsInput
	                	)
            		);
            }
            
            
            
        } catch (JackException ex) {
            if (!status.isEmpty()) {
                System.out.println("JACK exception client status : " + status);
            }
            throw ex;
        }

    }

    public void start() throws JackException, InterruptedException {
    	this.logic.initialize();
    	this.logic.setParent( this );
    	
        this.activate();
        new Thread( this ).start();
//    	this.run();
    }
    
    public void clearSequences() {
		synchronized ( this.sequences ) {
			for ( MetroNoteEventBufferSequence s : this.sequences ) {
				s.clearBuffer();
			}
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
				}
//				System.out.println( this.sequences.size() );
	        	Thread.sleep(500);
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
        
        {
        	String[] ports = this.jack.getPorts( this.client, "", JackPortType.MIDI, EnumSet.of( JackPortFlags.JackPortIsInput ) );
        	for ( String s : ports ) {
        		System.out.println( s );
        	}
        }
        /*
         * At this point, `(MetroMasterLogic)sequence.getLogic()` 
         *   always returns a MetroMasterLogic instance. 
         */
        for ( MetroNoteEventBufferSequence sequence : this.sequences ) {
        	for ( Entry<String, String> outputPortName : ((MetroMasterLogic)sequence.getLogic()).optionalConnection() ) {
        		String key = outputPortName.getKey();
        		String value = outputPortName.getValue();
        		if ( key != null && value != null )
        			this.jack.connect( this.client, key, value);
        	}
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
		try {
            // JackMidi.clearBuffer(this.outputPort);
            for ( JackPort p : Metro.this.outputPortList )
            	JackMidi.clearBuffer( p );
            
            
            this.inputMidiEventList.clear();
        	this.outputMidiEventList.clear();
            
//        	for ( Iterator<> i = this.inputPortList )
        	
            int eventCount = JackMidi.getEventCount( this.inputPort );
            for (int i = 0; i < eventCount; ++i) {
                JackMidi.eventGet( this.midiEvent, this.inputPort, i );
                int size = this.midiEvent.size();
                byte[] data = new byte[size];
                this.midiEvent.read( data );
                this.inputMidiEventList.add( new MetroMidiEvent( 0 /* TODO */, this.midiEvent.time(), data ) );

//                this.sb.setLength(0);
//                this.sb.append(this.midiEvent.time());
//                this.sb.append(": ");
//                for (int j = 0; j < size; j++) {
//                    this.sb.append((j == 0) ? "" : ", ");
//                    this.sb.append(this.data[j] & 0xFF);
//                }
//                 debugQueue.offer(sb.toString());

//                if ( 0 < Metro.this.outputPortList.size() )
//                	JackMidi.eventWrite( Metro.this.outputPortList.get( 0 ), this.midiEvent.time(), this.data, this.midiEvent.size());
            }
//            System.err.println( "ev:" + eventCount + " this.outputMidiEventList  : " + this.outputMidiEventList.size() );

            if ( 0 < this.inputMidiEventList.size() )
            	this.logic.processInputMidiBuffer( this.inputMidiEventList, this.outputMidiEventList );
            
            synchronized ( this.sequences ) {
            	for ( MetroNoteEventBufferSequence sequence : this.sequences ) {
            		sequence.progressCursor( nframes, this.outputMidiEventList );
            	}
            	this.outputMidiEventList.sort( MetroMidiEvent.COMPARATOR );
            	
            	if ( ! this.outputMidiEventList.isEmpty() )
            		if ( DEBUG ) System.out.println( this.outputMidiEventList );

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
            System.out.println("ERROR : " + ex);
            return false;
        }
    }
    
	void registerSequence( MetroNoteEventBufferSequence sequence ) {
		if ( DEBUG ) 
			System.err.println( "****** CREATED a new sequence is created " + sequence.id );
		this.registeredSequences.add( sequence );
	}
	void unregisterSequence( MetroNoteEventBufferSequence sequence ) {
		if ( DEBUG ) 
			System.err.println( "****** DESTROYED a sequence is destroyed " + + sequence.id );
		this.unregisteredSeqences.add( sequence );
	}

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

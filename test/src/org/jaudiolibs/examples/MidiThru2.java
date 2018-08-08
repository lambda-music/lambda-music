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
package org.jaudiolibs.examples;

import java.util.EnumSet;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
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
import org.jaudiolibs.jnajack.JackProcessCallback;
import org.jaudiolibs.jnajack.JackShutdownCallback;
import org.jaudiolibs.jnajack.JackStatus;

public class MidiThru2 implements JackProcessCallback, JackShutdownCallback {
	protected final Object mutex = new Object();

    private final class MidiThruProc extends Thread {
		private volatile boolean running = true;
		
		private boolean flag = true;
		private boolean proc() {
			synchronized ( mutex ) {
		        try {
		    		byte[] data = flag ? new byte[] { (byte)144, 48, 64 } : new byte[] { (byte)128, 48, 64 };
		    		flag = ! flag;
		    		
		            JackMidi.clearBuffer(outputPort);
		            JackMidi.eventWrite(outputPort, 0, data, data.length );
		            System.out.println( outputPort );
		            return true;
		        } catch (JackException ex) {
		            System.out.println("ERROR : " + ex);
		            return false;
		        }
			}
		}
		public void run() {
    		while ( running ) {
    			try {
    				proc();
					Thread.sleep(1000);
				} catch (InterruptedException e) {
					running = false;
					e.printStackTrace();
				}
    		}
    	}

		public void notifyStop() {
    		running = false;
    	}
	}

	private final static boolean DEBUG = true;

    private final JackClient client;
    private final JackPort inputPort;
    private final JackPort outputPort;
    private final JackMidi.Event midiEvent;
    
    private byte[] data;

	private BlockingQueue<String> debugQueue;
    private StringBuilder sb;
    
    
    private MidiThruProc midiThruProc = new MidiThruProc();

    public static void main(String[] args) {
        try {
            MidiThru2 midiSource = new MidiThru2();
            midiSource.activate();
            while (true) {
                if (DEBUG) {
                    String msg = midiSource.debugQueue.take();
                    System.out.println(msg);
                } else {
                    Thread.sleep(100000);
                }
            }
        } catch (Exception ex) {
            Logger.getLogger(MidiThru2.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    private MidiThru2() throws JackException {
        EnumSet<JackStatus> status = EnumSet.noneOf(JackStatus.class);
        try {
            Jack jack = Jack.getInstance();
            client = jack.openClient("Java MIDI thru test", EnumSet.of(JackOptions.JackNoStartServer), status);
            if (!status.isEmpty()) {
                System.out.println("JACK client status : " + status);
            }
            inputPort = client.registerPort("MIDI in", JackPortType.MIDI, JackPortFlags.JackPortIsInput);
            outputPort = client.registerPort("MIDI out", JackPortType.MIDI, JackPortFlags.JackPortIsOutput);
            midiEvent = new JackMidi.Event();
            if (DEBUG) {
                debugQueue = new LinkedBlockingQueue<String>();
                sb = new StringBuilder();
            }
            
            // >>>
            midiThruProc.start();
            // <<<
        } catch (JackException ex) {
            if (!status.isEmpty()) {
                System.out.println("JACK exception client status : " + status);
            }
            throw ex;
        }

    }

    private void activate() throws JackException {
        client.setProcessCallback(this);
        client.onShutdown(this);
        client.activate();
    }

    @Override
    public boolean process(JackClient client, int nframes) {
		synchronized ( mutex ) {

			try {
	            JackMidi.clearBuffer(outputPort);
	            int eventCount = JackMidi.getEventCount(inputPort);
	            for (int i = 0; i < eventCount; ++i) {
	                JackMidi.eventGet(midiEvent, inputPort, i);
	                int size = midiEvent.size();
	                if (data == null || data.length < size) {
	                    data = new byte[size];
	                }
	                midiEvent.read(data);
	
	                if (DEBUG) {
	                    sb.setLength(0);
	                    sb.append(midiEvent.time());
	                    sb.append(": ");
	                    for (int j = 0; j < size; j++) {
	                        sb.append((j == 0) ? "" : ", ");
	                        sb.append(data[j] & 0xFF);
	                    }
	                    debugQueue.offer(sb.toString());
	                }
	
	                JackMidi.eventWrite(outputPort, midiEvent.time(), data, midiEvent.size());
	            }
	            System.out.println( nframes );
	            // midiThruProc.proc();
	            return true;
	        } catch (JackException ex) {
	            System.out.println("ERROR : " + ex);
	            return false;
	        }
		}
    }

    @Override
    public void clientShutdown(JackClient client) {
        System.out.println("Java MIDI thru test shutdown");
    }
}

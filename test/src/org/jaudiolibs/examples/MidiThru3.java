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

public class MidiThru3 implements JackProcessCallback, JackShutdownCallback {

	private final static boolean DEBUG = true;

    private final JackClient client;
    private final JackPort inputPort;
    private final JackPort outputPort;
    private final JackMidi.Event midiEvent;
    
    private byte[] data;

	private BlockingQueue<String> debugQueue;
    private StringBuilder sb;
    
    public static void main(String[] args) {
        try {
            MidiThru3 midiSource = new MidiThru3();
            midiSource.activate();
            while (true) {
                if (DEBUG) {
                    String msg = midiSource.debugQueue.take();
                    System.out.print( msg );
                } else {
                    Thread.sleep(100000);
                }
            }
        } catch (Exception ex) {
            Logger.getLogger(MidiThru3.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    private MidiThru3() throws JackException {
        EnumSet<JackStatus> status = EnumSet.noneOf(JackStatus.class);
        try {
            Jack jack = Jack.getInstance();
            client = jack.openClient("Java MIDI thru test", EnumSet.of(JackOptions.JackNoStartServer), status);
            if (!status.isEmpty()) {
                System.out.print( "JACK client status : " + status );
            }
            inputPort = client.registerPort("MIDI in", JackPortType.MIDI, JackPortFlags.JackPortIsInput);
            outputPort = client.registerPort("MIDI out", JackPortType.MIDI, JackPortFlags.JackPortIsOutput);
            midiEvent = new JackMidi.Event();
            if (DEBUG) {
                debugQueue = new LinkedBlockingQueue<String>();
                sb = new StringBuilder();
            }
            
        } catch (JackException ex) {
            if (!status.isEmpty()) {
                System.out.print( "JACK exception client status : " + status );
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
		try {
            JackMidi.clearBuffer(outputPort);
            int eventCount = JackMidi.getEventCount(inputPort);
//            System.out.println("eventCount"+ eventCount );
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
            midiProc.proc();
            
            return true;
        } catch (JackException ex) {
            System.out.print( "ERROR : " + ex );
            return false;
        }
    }

    MidiProcessor1 midiProc = new MidiProcessor1();
    class MidiProcessor1 {
    	boolean flg = false;
    	int ctr = 0;
        private void proc() throws JackException {
        	ctr++;
        	if ( 10 < ctr ) {
        		ctr = 0;
        		flg = ! flg;
        		byte[] bs = flg ? new byte[] { (byte)144, 48, 64 } : new byte[] { (byte)128, 48, 64 };
        		JackMidi.eventWrite(outputPort, 0, bs, bs.length );
                System.out.println( bs );

        	}
//        	byte[] bs  = new byte[] {  };
//            JackMidi.eventWrite(outputPort, midiEvent.time(), data, midiEvent.size());
    	}
    }

	@Override
    public void clientShutdown(JackClient client) {
        System.out.print( "Java MIDI thru test shutdown" );
    }
}

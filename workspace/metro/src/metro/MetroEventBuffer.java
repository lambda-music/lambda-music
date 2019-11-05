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

import static metro.Metro.*;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.jaudiolibs.jnajack.JackException;

/**
 * This class represents all events which should be processed in a bar. 
 * 
 * @author Ats Oka
 */
public class MetroEventBuffer extends MetroBufferedToNonBufferedMidiReceiver<MetroMidiEvent,byte[]> implements Iterable<MetroEvent> {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    public MetroEventBuffer() {
        super( MetroMidiMessage.getInstance() );
    }
    
    private double length = 1.0d;
    private boolean prepared = false;
    private int barLengthInFrames=-1;
    private int lengthInFrames = -1;
    private final List<MetroEvent> list = new ArrayList<MetroEvent>(10);
    public double getLength() {
        return length;
    }
    public void setLength(double length) {
        if (DEBUG) 
            logInfo( "setLength():" + length );
        this.length = length;
    }
    public double getActualLength() {
        double max = 0;
        for ( MetroEvent e : this )
            if ( max < e.getBarOffset() ) 
                max = e.getBarOffset();
        
        return max;
    }
    public int getBarLengthInFrames() {
        if ( ! prepared )
            throw new RuntimeException("not prepared");
        return barLengthInFrames;
    }
    public int getLengthInFrames() {
        if ( ! prepared )
            throw new RuntimeException("not prepared");
        return lengthInFrames;
    }
    
    public void prepare( int barLengthInFrames, boolean doSort ) throws JackException {
        if ( doSort && false )
            this.list.sort( MetroEvent.BAR_OFFSET_COMPARATOR );
        
//        int barLengthInFrames = Metro.calcBarInFrames( metro, client, position );
        this.calcBarOffsetInFrames( barLengthInFrames );
    }
    
    private void calcBarOffsetInFrames( int barLengthInFrames ) {
//      System.out.println("MetroMidiEventBuffer.calcInFrames() barInFrames="  + barInFrames );
        for ( MetroEvent e : this ) {
            e.calcBarOffsetInFrames( barLengthInFrames );
        }
//      System.out.println( "this.length " + this.length  );
        this.barLengthInFrames = barLengthInFrames;
        this.lengthInFrames = (int) (this.length * (double)barLengthInFrames);
        this.prepared = true;
        
        if ( DEBUG ) 
            logInfo( "MetroMidiEventBuffer.calcInFrames() barInFrames="  + barLengthInFrames + " / lengthInFrames=" + this.lengthInFrames  + "/ length=" + this.length);
    }
    
    @Override
    public Iterator<MetroEvent> iterator() {
        return this.list.iterator();
    }
    
    public int size() {
        return this.list.size();
    }

    public final void event( MetroEvent event ) {
        // Add it to the list.
        this.list.add(event);
    }
    
    @Override
    public final MetroMidiEvent convertResult( String id, double offset, MetroPort outputPort, byte[] data ) {
//        logInfo( "midiEvent:" + SchemeUtils.bytesToString( data ) );
        // Create an event object.
        DefaultMetroEventMidiEvent event = new DefaultMetroEventMidiEvent( id, offset, outputPort, data );
        
        // Add it to the list.
        this.list.add(event);
        
        return event;
    }
    
    public void noteHit( double offset, MetroPort outputPort, int channel, int note, double velocity ) {
        noteHit( offset, outputPort, channel, note, velocity, -1 );
    }
    public void noteHit( double offset, MetroPort outputPort, int channel, int note, double velocity, double duration ) {
        if ( duration < 0 )
            duration = 0.0025d;
        
        noteOn(  offset,            outputPort, channel, note, velocity );
        noteOff( offset + duration, outputPort, channel, note, velocity );
    }

    public void exec( double offset, Runnable runnable ) {
        MetroMessageEvent event = new MetroMessageEvent( "exec", offset, runnable );

        this.list.add( event );
    }

    public void dump() {
        logInfo( "length         : " + this.length        );
        logInfo( "lengthInFrames : " + this.lengthInFrames);
        int i = 0;
        for ( MetroEvent e : this ) {
            logInfo( "    No" + i);
            logInfo( e.dump( "    " ));
            i++;
        }
        logInfo( "    END");
    }
    
}





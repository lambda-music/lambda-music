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

import java.util.Collection;
import java.util.Comparator;
import java.util.List;

/**
 * This interface represents actual MIDI events. THe objects that implements
 * this interface are usually generated by the conversion from
 * MetroAbstractEvent objects. 
 * 
 * @author ats
 */
public interface MetroMidiEvent extends Comparable<MetroMidiEvent>, MetroEventOutput {
    public static final Comparator<? super MetroMidiEvent> COMPARATOR = new Comparator<MetroMidiEvent>() {
        @Override
        public int compare(MetroMidiEvent o1, MetroMidiEvent o2) {
            return MetroMidiEvent.compare( o1, o2 );
//            return o1.compareTo( o2 );
        }
    };
    
    public static int compare( MetroMidiEvent o1, MetroMidiEvent o2 ) {
        long of1 = o1.getMidiOffset();
        long of2 = o2.getMidiOffset();
        if ( of1 != of2 ) {
            if ( of1 < of2 ) {
                return -1;
            } else {
                return 1;
            }
        }
        
        byte[] b1 = o1.getMidiData();
        byte[] b2 = o2.getMidiData();
        
        byte m1_channel = (byte) ( 0b01111 & b1[0] );
        byte m2_channel = (byte) ( 0b01111 & b2[0] );
        if ( m1_channel != m2_channel ) {
            if ( m1_channel < m2_channel ) {
                return -1;
            } else {
                return 1;
            }
        }
        
        byte m1_status = (byte) ( 0b011110000 & b1[0] );
        byte m2_status = (byte) ( 0b011110000 & b2[0] );
        if ( m1_status != m2_status ) {
            // 0b1000xxxx == Note Off
            // 0b1001xxxx == Note On
            if ( m1_status == 0b10010000 )
                return -1;
            else 
                return 1;
        }
        
        return 0;
    }

    long      getMidiOffset();
    void      setMidiOffset( long midiOffset );
    void      moveMidiOffset( long offset );

    MetroPort getPort();
    void      setPort( MetroPort port );
    
    byte[]    getMidiData();
    void      setMidiData( byte[] midiData );
    
    default MetroMidi getMidi() {
        return MetroMidi.getMidi( getMidiCommand());
    }
    default int getMidiCommand() {
        return MetroMidi.getMidiCommand( getMidiData());
    }
    
    public default int compareTo( MetroMidiEvent o ) {
        return (int) (this.getMidiOffset() - o.getMidiOffset());
    }
    
    /**
     * This method adds the current event to the buffer.
     * See {@link MetroEventOutput#processOutput(Collection, List, List)}.
     */
    @Override
    public default <T extends MetroEventOutput> void processOutput(Collection<T> output, List<MetroTrack> registeringTrackList, List<MetroTrack> unregisteringTrackList) {
        output.add((T)this);
    }
}

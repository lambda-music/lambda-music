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

import java.util.Comparator;

/**
 * This interface represents actual MIDI events. THe objects that implements
 * this interface are usually generated by the conversion from
 * MetroAbstractEvent objects. 
 * 
 * @author ats
 */
public interface MetroMidiEvent extends Comparable<MetroMidiEvent> {
    public static final Comparator<? super MetroMidiEvent> COMPARATOR = new Comparator<MetroMidiEvent>() {
        @Override
        public int compare(MetroMidiEvent o1, MetroMidiEvent o2) {
            return o1.compareTo( o2 );
        }
    };

    int       getMidiOffset();
    void      setMidiOffset( int midiOffset );
    void      moveMidiOffset( int offset );

    MetroPort getPort();
    void      setPort( MetroPort port );
    
    byte[]    getMidiData();
    void      setMidiData( byte[] midiData );
    
    public default int compareTo( MetroMidiEvent o ) {
        return this.getMidiOffset() - o.getMidiOffset();
    }
}

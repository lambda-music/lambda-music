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

import java.util.Arrays;

public class DefaultMetroMidiEvent extends DefaultMetroEvent implements MetroMidiEvent {
    public DefaultMetroMidiEvent( String id, double offset, MetroPort outputPort, byte[] data ) {
        super( id, offset );
        this.outputPort = outputPort;
        this.midiData = data;
    }
    
    private int midiOffset;
    @Override
    public final int getMidiOffset() {
        return midiOffset;
    }
    @Override
    public final void setMidiOffset(int midiOffset) {
        this.midiOffset = midiOffset;
    }
    @Override
    public void calcMidiOffset(int from) {
        this.setMidiOffset( this.getBarOffsetInFrames() - from );
    }

    MetroPort outputPort;
    @Override
    public final MetroPort getOutputPort() {
        return outputPort;
    }
    @Override
    public void setOutputPort(MetroPort outputPort) {
        this.outputPort = outputPort;
    }

    byte[] midiData;
    @Override
    public byte[] getMidiData() {
        return midiData;
    }
    @Override
    public void setMidiData(byte[] midiData) {
        this.midiData = midiData;
    }
    
    @Override
    public void process(Metro metro) {
    }
    
//    @Override
//    public void process(Metro metro, int from, int to, int nframes, List<MetroAbstractMidiEvent> eventList) {
//        calcMidiOffset( from );
//        eventList.add( this );
//    }
    
    public void dumpProc( String prefix, StringBuilder sb ) {
        super.dumpProc(prefix, sb);
        sb.append(prefix).append( "      outputPortNo: " + outputPort ).append( "\n" );
        sb.append(prefix).append( "              data: " + Arrays.toString( midiData ) ).append( "\n" );
    }
}

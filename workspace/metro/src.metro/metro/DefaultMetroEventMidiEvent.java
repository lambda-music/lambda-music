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
import java.util.Collection;
import java.util.List;

class DefaultMetroEventMidiEvent extends DefaultMetroEvent implements MetroMidiEvent, MetroEvent, MetroEventOutput {
    public DefaultMetroEventMidiEvent( String id, double offset, MetroPort outputPort, byte[] data ) {
        super( id, offset );
        this.outputPort = outputPort;
        this.midiData = data;
    }
    public DefaultMetroEventMidiEvent( String id, int offsetInFrames, MetroPort outputPort, byte[] data ) {
        super( id, offsetInFrames );
        this.outputPort = outputPort;
        this.midiData = data;
    }
    
    private volatile long midiOffset;
    @Override
    public final long getMidiOffset() {
        return midiOffset;
    }
    @Override
    public final void setMidiOffset(long midiOffset) {
        this.midiOffset = midiOffset;
    }
    @Override
    public void moveMidiOffset(long offset) {
        this.midiOffset += offset;
    }
    public void calcMidiOffset( long cursor ) {
        this.setMidiOffset( this.getBarOffsetInFrames() - cursor );
    }

    private volatile MetroPort outputPort;
    @Override
    public final MetroPort getPort() {
        return outputPort;
    }
    @Override
    public void setPort(MetroPort port) {
        this.outputPort = port;
    }

    private volatile byte[] midiData;
    @Override
    public byte[] getMidiData() {
        return midiData;
    }
    @Override
    public void setMidiData(byte[] midiData) {
        this.midiData = midiData;
    }
    
    @Override
    public void process(Metro metro, long cursor) {
        calcMidiOffset( cursor );
    }
    
    /**
     * This method adds the current event to the buffer.
     * See {@link MetroEventOutput#processOutput(Collection, List, List, List, List)}.
     */
    @Override
    public void processOutput(
        Collection<MetroMidiEvent> output, 
        List<MetroTrack> tracks, 
        List<MetroTrack> registeringTracks, List<MetroTrack> finalizingTracks, List<MetroTrack> unregisteringTracks) 
    {
        output.add(this);
    }
    
//    @Override
//    public void process(Metro metro, int from, int to, int nframes, List<MetroAbstractMidiEvent> eventList) {
//        calcMidiOffset( from );
//        eventList.add( this );
//    }
    
    @Override
    public void dumpProc( String prefix, StringBuilder sb ) {
        super.dumpProc(prefix, sb);
        sb.append(prefix).append( "      outputPortNo: " + outputPort ).append( "\n" );
        sb.append(prefix).append( "              data: " + Arrays.toString( midiData ) ).append( "\n" );
    }
}

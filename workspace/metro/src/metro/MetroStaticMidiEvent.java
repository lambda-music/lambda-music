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

public final class MetroStaticMidiEvent implements MetroMidiEvent {
    private MetroPort outputPort;
    private int midiOffset;
    private byte[] midiData;
    public MetroStaticMidiEvent( MetroPort outputPort, int offset, byte[] data ) {
        this.outputPort = outputPort;
        this.midiOffset = offset;
        this.midiData = data;
    }
    @Override
    public MetroPort getOutputPort() {
        return outputPort;
    }
    @Override
    public void setOutputPort( MetroPort outputPort ) {
        this.outputPort = outputPort;
    }
    @Override
    public int getMidiOffset() {
        return midiOffset;
    }
    @Override
    public void setMidiOffset(int midiOffset) {
        this.midiOffset = midiOffset;
    }
    @Override
    public byte[] getMidiData() {
        return midiData;
    }
    @Override
    public void setMidiData(byte[] midiData) {
        this.midiData = midiData;
    }
    @Override
    public String toString() {
        return "offset : " + Integer.toString( this.midiOffset ) ;
    }
}

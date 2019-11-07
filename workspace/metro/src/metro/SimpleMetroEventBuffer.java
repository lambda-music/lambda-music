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

/**
 * This class represents all events which should be processed in a bar. 
 * 
 * @author Ats Oka
 */
public class SimpleMetroEventBuffer extends MetroBufferedToNonBufferedMidiReceiver<MetroMidiEvent,byte[]> implements MetroBufferedMidiReceiver<MetroMidiEvent> {
    int cursorOffset=0;
    public int getCursorOffset() {
        return cursorOffset;
    }

    public void setCursorOffset(int cursorOffset) {
        this.cursorOffset = cursorOffset;
    }
    int oneBarLengthInFrames=1;
    public int getOneBarLengthInFrames() {
        return oneBarLengthInFrames;
    }

    public void setOneBarLengthInFrames(int oneBarLengthInFrames) {
        this.oneBarLengthInFrames = oneBarLengthInFrames;
    }

    public SimpleMetroEventBuffer( int oneBarLengthInFrames ) {
        super( MetroMidiMessage.getInstance() );
        this.oneBarLengthInFrames = oneBarLengthInFrames;
    }
    public SimpleMetroEventBuffer() {
        super( MetroMidiMessage.getInstance() );
    }
    
    Collection resultList;
    public <T> Collection<T> getResultList() {
        return resultList;
    }

    public <T> void setResultList(Collection<T> resultList) {
        this.resultList = resultList;
    }

    @Override
    public final MetroMidiEvent convertResult( String id, double offset, MetroPort outputPort, byte[] data ) {
        int midiOffset = ((int)(offset * (double)this.oneBarLengthInFrames)) - this.cursorOffset;
        // Create an event object.
        DefaultMetroMidiEvent event = new DefaultMetroMidiEvent( midiOffset, outputPort, data );
        if ( resultList != null )
            resultList.add( event );
        return event;
    }
}





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

import java.lang.invoke.MethodHandles;
import java.util.Collection;
import java.util.logging.Level;

import quartz.lib.log.PulsarLogger;

/**
 * This class represents all events which should be processed in a bar. 
 * 
 * @author Ats Oka
 */
public class SimpleMetroEventBuffer extends MetroBufferedToNonBufferedMidiReceiver<MetroMidiEvent,byte[]>  {
    static final PulsarLogger LOGGER = PulsarLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

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
    public final MetroMidiEvent receive( String id, double offset, MetroPort outputPort, byte[] data ) {
        if ( data == null )
            return null;
        
        int midiOffset = ((int)(offset * (double)this.oneBarLengthInFrames)) - this.cursorOffset;
        // Create an event object.
        DefaultMetroMidiEvent event = new DefaultMetroMidiEvent( midiOffset, outputPort, data );
        
        if ( resultList != null )
            resultList.add( event );
        
        return event;
    }

    @Override
    public MetroMidiEvent exec(double offset, Runnable runnable) {
        logWarn( "called an unimplemented method - exec" );
        return null;
    }

    @Override
    public MetroMidiEvent event(double offset, MetroMidiEvent event) {
        logWarn( "called an unimplemented method - event" );
        return null;
    }

    @Override
    public MetroMidiEvent length(double length) {
        logWarn( "called an unimplemented method - length" );
        return null;
    }
}





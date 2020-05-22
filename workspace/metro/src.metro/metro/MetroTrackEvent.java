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
import java.util.List;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public class MetroTrackEvent extends DefaultMetroEvent implements MetroEventOutput {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    public static final String PUT_TRACKS="PUT_TRACKS";
    public static final String REMOVE_TRACKS="REMOVE_TRACKS";
    
    private final String operation;
    private final MetroTrackManipulator trackManipulator;
    public MetroTrackEvent( String id, double offset, String operation, MetroTrackManipulator trackManipulator ) {
        super(id, offset);
        this.operation = operation;
        this.trackManipulator = trackManipulator;
    }
    public String getOperation() {
        return operation;
    }
    @Override
    public String toString() {
        return "(MidiTrackEvent " + getTypeName() + ")";
    }
    @Override
    public void process(Metro metro, long cursor) {
    }
    
    /**
     * This method adds the current track  to the track list.
     * See {@link MetroEventOutput#processOutput(Collection, List, List, List)}.
     */
    @Override
    public void processOutput(
        Collection<MetroMidiEvent> output, 
        List<MetroTrack> tracks, 
        List<MetroTrack> registeringTracks, 
        List<MetroTrack> unregisteringTracks)
    {
        trackManipulator.createTracks(tracks, registeringTracks, unregisteringTracks);
    }
}

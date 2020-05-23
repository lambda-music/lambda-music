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
import java.util.List;

public class MetroMessageEvent extends DefaultMetroEvent {
    public static final class MetroRunnableMessage implements MetroMessage {
        private final Runnable runnable;
        public MetroRunnableMessage(Runnable runnable) {
            this.runnable = runnable;
        }
        @Override
        public void executeMessage(
            Metro metro, 
            List<MetroTrack> tracks, 
            long measureLengthInFrames) 
        {
            runnable.run();
        }
    }
    private final MetroMessage message;
    public MetroMessageEvent( String id, double offset, Runnable runnable ) {
        super(id, offset);
        this.message = new MetroRunnableMessage(runnable);
    }
    public MetroMessageEvent( String id, double offset, MetroMessage message ) {
        super(id, offset);
        this.message = message;
    }
    public void execute( Metro metro ) {
        metro.postMessage( message );
    }
    @Override
    public void process(Metro metro, long cursor) {
        execute( metro );
    }
    @Override
    public void processOutput(
        Collection<MetroMidiEvent> output, 
        List<MetroTrack> tracks,
        List<MetroTrack> registeringTracks, 
        List<MetroTrack> unregisteringTracks) 
    {
    }
}

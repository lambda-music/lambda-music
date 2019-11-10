/*
 * Pulsar-Sequencer written by Atsushi Oka 
 * Copyright 2018 Atsushi Oka
 *
 * This file is part of Pulsar-Sequencer. 
 * 
 * Pulsar-Sequencer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Pulsar-Sequencer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Pulsar-Sequencer.  If not, see <https://www.gnu.org/licenses/>.
 */

package pulsar;

import java.util.Collection;

import gnu.lists.LList;
import metro.Metro;
import metro.MetroBufferedMidiReceiver;
import metro.MetroTrack;
import pulsar.lib.scheme.scretary.SchemeSecretary;
import pulsar.lib.secretary.Invokable;

public class PulsarNoteListParser extends NoteListParser {
    private static final PulsarNoteListParser INSTANCE = new PulsarNoteListParser();
    private PulsarNoteListParser() {
        this.putAll( PulsarSpecialNoteListParsers.getElements() );
        this.putAll( PulsarMidiNoteListParsers.getElements() );
    }
    public static final PulsarNoteListParser getInstance() {
        return INSTANCE;
    }
    
    // MOVED FROM SchemeSequence (Wed, 06 Nov 2019 17:07:05 +0900)
    public static <T> void invokable2receiver( Metro metro, MetroTrack track, Invokable invokable, MetroBufferedMidiReceiver<T> receiver, Collection<T> result ) {
        SchemeSecretary.initializeSchemeForCurrentThreadStatic( ((Pulsar)metro).getSchemeSecretary().getExecutive() );
        ((Pulsar)metro).threadInializer.run();
        
        // Call the invokable to get a note list of the next measure.
        Collection<Object> notations = (Collection<Object>)invokable.invoke();
        
        // Parse the retrieved list to execute.
        notations2receiver( metro, track, receiver, notations, result );
    }

    // MOVED FROM SchemeSequence (Wed, 06 Nov 2019 17:07:05 +0900)
    public static <T> void notations2receiver(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T> receiver, Collection<Object> notations, Collection<T> result ) {
//      return SchemeNoteParser0.parse(metro, scheme, pattern, buf, true );
//      return SchemeNoteParser1.parse(metro, scheme, pattern, buf, true );
//      return PulsarNoteParser2.parse(metro, track, pattern, buf, true );
        INSTANCE.parse( metro, track, notations, receiver, result );
    }

    // MOVED FROM SchemeSequence (Wed, 06 Nov 2019 17:07:05 +0900)
    public static <T> void notation2receiver(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T> receiver, LList notation, Collection<T> result ) {
        INSTANCE.parseNotation( metro, track, notation, receiver, result );
    }
}

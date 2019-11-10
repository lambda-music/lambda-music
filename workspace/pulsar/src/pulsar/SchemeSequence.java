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

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Procedure;
import metro.Metro;
import metro.MetroBufferedMidiReceiver;
import metro.MetroEvent;
import metro.MetroMidiEvent;
import metro.MetroSequence;
import metro.MetroTrack;
import pulsar.lib.NullList;
import pulsar.lib.scheme.SafeProcedureN;
import pulsar.lib.secretary.Invokable;

public class SchemeSequence implements MetroSequence, SchemeSequenceReadable, Invokable {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) {
        LOGGER.log(Level.SEVERE, msg, e);
    }
    static void logInfo(String msg) {
        LOGGER.log(Level.INFO, msg);
    }
    static void logWarn(String msg) {
        LOGGER.log(Level.WARNING, msg);
    }
    
    static final class SchemeSequenceDefaultProcedure extends SafeProcedureN {
        private final LList notations;
        SchemeSequenceDefaultProcedure(LList notations) {
            this.notations = notations;
        }
        
        @Override
        public Object applyN(Object[] args) throws Throwable {
            return notations;
        }
    }
    public static Procedure asProcedure(Object v) {
        if ( v instanceof Procedure ) {
            return (Procedure)v;
        } else if ( v instanceof LList ) {
            return new SchemeSequenceDefaultProcedure( (LList)v );
        } else {
            logWarn( "An invalid object was passed to the track procedure parameter. We ignored it. " );
            return null;
        }
    }

    /*
     * (list
     *     (cons 1.0  (lambda() ... ))
     *     (cons 2.0  (lambda() ... ))
     *     (cons 2.5  (lambda() ... )))
     */
    static ArrayList<Pair> parseListOfPairs( Pair pair ) {
        ArrayList<Pair> pairs = new ArrayList<Pair>();
        for ( Object pp : pair ) {
            pairs.add( (Pair)pp ); 
        }
        return pairs;
    }

    /*
     * Note (XXX_SYNC_01):
     * It is not necessary to refer to the Kawa scheme instance to invoke the invokable.
     * It is necessary because 
     * 
     */
    final Invokable invokable;
    public SchemeSequence ( Invokable procedure ) {
        this.invokable = procedure;
    }
    @Override
    public Object invoke(Object... args) {
        return invoke( args );
    }

    @Override
    public void processDirect( Metro metro, int nframes, int totalCursor, List<MetroMidiEvent> in, List<MetroMidiEvent> out) {
        // out.addAll( in ); TODO ******************************
//        MetroMidi.receiveMidiMessage( MetroMidiReceiver.LoggingToError.getInstance(), in );
//        System.err.println( "in.size()" + in.size());
//        System.err.println( "out.size()" + out.size());
    }


    @Override
    public void processBuffered( Metro metro, MetroTrack track, MetroBufferedMidiReceiver<MetroEvent> receiver ) {
        // System.out.println("Metro.sequence.new MetroSequence() {...}.initBuffer()" );
//      buf.humanize( 0.0d, 3 );
        try {
            PulsarNoteListParser.invokable2receiver(metro, track, invokable, receiver, NullList.NULL_LIST ) ;
        } catch ( RuntimeException e ) {
            System.err.println(e );
            LOGGER.log(Level.SEVERE, "", e);
        }
    }

    @Override
    public LList readMusic() {
        throw new UnsupportedOperationException();
    }
}

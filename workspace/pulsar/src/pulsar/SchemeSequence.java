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

import gnu.lists.AbstractSequence;
import gnu.lists.Pair;
import metro.Metro;
import metro.MetroAbstractMidiEvent;
import metro.MetroEventBuffer;
import metro.MetroSequence;
import metro.MetroTrack;
import pulsar.lib.secretary.Invokable;

public class SchemeSequence extends MetroSequence {
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
    final Invokable procedure;
    public SchemeSequence ( Invokable procedure ) {
        this.procedure = procedure;
    }
    public Invokable getProcedure() {
        return procedure;
    }

    @Override
    public void processDirect(Metro metro, List<MetroAbstractMidiEvent> in, List<MetroAbstractMidiEvent> out) {
        // out.addAll( in ); TODO ******************************
        System.err.println( "in.size()" + in.size());
        System.err.println( "out.size()" + out.size());
    }


    @Override
    public boolean processBuffered( Metro metro, MetroTrack track, MetroEventBuffer buf ) {
        // System.out.println("Metro.sequence.new MetroSequence() {...}.initBuffer()" );
//      buf.humanize( 0.0d, 3 );
        boolean result = false;
        try {
            result = scheme2buf(metro, track, procedure, buf);
        } catch ( RuntimeException e ) {
            System.err.println(e );
            LOGGER.log(Level.SEVERE, "", e);
        }
        return result;
    }
    private static final NoteListParser PARSER = PulsarNoteListParser.getInstance();
    public static boolean scheme2buf( Metro metro, MetroTrack track, Invokable procedure, MetroEventBuffer buf) {
        // Call the invokable to get a note list of the next measure.
        AbstractSequence<Object> pattern = (AbstractSequence<Object>)procedure.invoke();
        
        // Parse the retrieved list to execute.
        
//      return SchemeNoteParser0.parse(metro, scheme, pattern, buf, true );
//      return SchemeNoteParser1.parse(metro, scheme, pattern, buf, true );
//      return PulsarNoteParser2.parse(metro, track, pattern, buf, true );
        return PARSER.parse( metro, track, pattern, buf, true );
    }
}

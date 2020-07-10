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
import java.util.Collection;
import java.util.logging.Level;

import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Procedure;
import lamu.lib.Invokable;
import lamu.lib.kawautils.SchemeInvokable;
import lamu.lib.kawautils.SchemePrinter;
import lamu.lib.kawautils.procedures.MultipleNamedProcedureN;
import lamu.lib.logging.Logger;
import metro.Metro;
import metro.MetroBufferedMidiReceiver;
import metro.MetroBufferedSequence;
import metro.MetroCollector;
import metro.MetroTraceableSequence;
import metro.MetroTrack;

public class SchemeSequence extends MetroBufferedSequence implements Invokable, MetroTraceableSequence {
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
    
    static final class SchemeSequenceDefaultProcedure extends MultipleNamedProcedureN {
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
    
    final Runnable initializer;

    /*
     * Note (XXX_SYNC_01):
     * It is not necessary to refer to the Kawa scheme instance to invoke the invokable.
     * It is necessary because 
     * 
     */
    final Invokable invokable;
    public SchemeSequence ( Runnable initializer, Invokable procedure ) {
        this.initializer = initializer;
        this.invokable = procedure;
    }
    public SchemeSequence( Runnable initializer, Procedure procedure ) {
        this.initializer = initializer;
        this.invokable = SchemeInvokable.create(procedure);
    }
    @Override
    public Object invoke(Object... args) {
        return invokable.invoke( args );
    }
    
    @Override
    public <T> void generateBuffer(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T> buffer) {
        // System.out.println("Metro.sequence.new MetroSequence() {...}.initBuffer()" );
//      buf.humanize( 0.0d, 3 );
        try {
//            SchemeEvaluator.initializeCurrentThread( ((Pulsar)metro).getSchemeEngine().getSchemeEvaluator().getScheme() );
//            ((Pulsar)metro).getThreadInitializer().run();
            
            // MODIFIED (Sun, 19 Apr 2020 09:48:14 +0900) TODO UPDATE THE DOCUMENTATION
            // metro.getThreadInitializerCollection().run();
            Pulsar.setCurrentMetro(metro);

            
            // Call the invokable to get a note list of the next measure.
            Collection<Object> notations = (Collection<Object>)invokable.invoke();
            
            if ( tracingEnabled ) {
            	LOGGER.log(Level.SEVERE, SchemePrinter.printSchemeValue(notations));
            }
            
            // Parse the retrieved list to execute.
            // MOVED FROM SchemeSequence (Wed, 06 Nov 2019 17:07:05 +0900)
            // MOVED AGAIN FROM NoteListParser (Thu, 02 Jan 2020 18:00:29 +0900)
            PulsarNoteListParser.getInstance().parseAll( metro, track, notations, buffer, (MetroCollector<T>) MetroCollector.NULL );
            
        } catch ( Exception e ) {
            LOGGER.log(Level.SEVERE, buffer.toString() , e);
        }
    }
    
    private boolean tracingEnabled=false;
    @Override
    public boolean isTracingEnabled() {
    	return tracingEnabled;
    }
    @Override
    public void setTracingEnabled(boolean tracingEnabled) {
    	this.tracingEnabled = tracingEnabled;
    }

}

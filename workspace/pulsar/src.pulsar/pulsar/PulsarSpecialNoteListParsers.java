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

import static pulsar.NoteListCommon.DEFAULT_NOTE_LENGTH;
import static pulsar.NoteListCommon.DEFAULT_VALUE_DOUBLE_0;
import static pulsar.NoteListCommon.ID_CHANNEL;
import static pulsar.NoteListCommon.ID_ID;
import static pulsar.NoteListCommon.ID_LENGTH;
import static pulsar.NoteListCommon.ID_LONG;
import static pulsar.NoteListCommon.ID_NOTE;
import static pulsar.NoteListCommon.ID_OFFSET;
import static pulsar.NoteListCommon.ID_PORT;
import static pulsar.NoteListCommon.ID_PROCEDURE;
import static pulsar.NoteListCommon.ID_SYNC_OFFSET;
import static pulsar.NoteListCommon.ID_SYNC_TRACK_ID;
import static pulsar.NoteListCommon.ID_TAGS;
import static pulsar.NoteListCommon.ID_VALUE;
import static pulsar.NoteListCommon.ID_VELOCITY;
import static pulsar.NoteListCommon.NULL;
import static pulsar.NoteListCommon.S2J_DOUBLE;
import static pulsar.NoteListCommon.S2J_PROCEDURE;
import static pulsar.NoteListCommon.THRU;
import static pulsar.NoteListCommon.list;
import static pulsar.NoteListCommon.readMapChannel;
import static pulsar.NoteListCommon.readMapCollection;
import static pulsar.NoteListCommon.readMapDoubleValueBarLength;
import static pulsar.NoteListCommon.readMapEnabled;
import static pulsar.NoteListCommon.readMapNewId;
import static pulsar.NoteListCommon.readMapNote;
import static pulsar.NoteListCommon.readMapNoteLength;
import static pulsar.NoteListCommon.readMapOffset;
import static pulsar.NoteListCommon.readMapPort;
import static pulsar.NoteListCommon.readMapProcedure;
import static pulsar.NoteListCommon.readMapSyncType;
import static pulsar.NoteListCommon.readMapVelocity;
import static pulsar.NoteListCommon.s;
import static pulsar.NoteListCommon.writeMapDoubleValue;
import static pulsar.NoteListCommon.writeMapType;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.logging.Level;

import gnu.lists.LList;
import gnu.mapping.Procedure;
import gnu.mapping.Symbol;
import lamu.lib.kawautils.SchemeInvokable;
import lamu.lib.kawautils.SchemeValues;
import lamu.lib.log.Logger;
import metro.Metro;
import metro.MetroBufferedMidiReceiver;
import metro.MetroBufferedTrack;
import metro.MetroCollector;
import metro.MetroPort;
import metro.MetroSyncTrack;
import metro.MetroSyncType;
import metro.MetroTrack;

/**
 * Defines special note events. 
 *  
 * @author ats
 */
public class PulsarSpecialNoteListParsers {
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
     * XXX this value is inconsistent now (Mon, 29 Jul 2019 09:17:14 +0900)
     */
    public static final String SEQ_BASE = "seq-base";


    static MetroPort getPortOld( Metro metro, Map<Symbol,Object> map ) {
        if ( map.containsKey( ID_PORT ) ) {
            Object o = map.get( ID_PORT );
            if ( o instanceof MetroPort ) {
                return (MetroPort)o;
            } else if ( o instanceof Number ) {
                int i = ((Number)o).intValue();
                List<MetroPort> list = metro.getOutputPorts();
                if ( i<0  ||  list.size() <= i )
                    throw new IllegalStateException( i + " is not proper. list size=" + list.size() );
                return list.get(i);
            } else {
                List<MetroPort> portList = metro.searchOutputPort( o );
                if ( portList.isEmpty() ) {
                    throw new IllegalArgumentException( "'" + o + "' does not exists" );
                }
                return portList.get( 0 );
            }
        } else {
            List<MetroPort> list = metro.getOutputPorts();
            if ( list.isEmpty() )
                throw new IllegalStateException();
            return list.get(0);
        }
    }

    /**
     * Returns a collection object which contains parser elements defined in this class. 
     * @return
     */
    public static Collection<NoteListParserElement> getElements() {
        return Collections.unmodifiableList( elements );
    }


    static final <V> V getValue( Map<String,Object> map, String key, V defaultValue, Function<Object,V> converter ) {
        if ( map.containsKey(key) )
            return converter.apply( map.get(key) );
        else
            return defaultValue;
    }
    
    static ArrayList<SpecialNoteListParserElement> elements = new ArrayList<>();
    static void register( SpecialNoteListParserElement info ) {
        elements.add( info );
    }
    
    static abstract class SpecialNoteListParserElement extends NoteListParserElement {
        Symbol shortName;
        Symbol longName;
        String shortDescription;
        String longDescription;
        List<NoteListParserElementParameter> parameters = Collections.EMPTY_LIST;
        @Override
        public Symbol getShortName() {
            return shortName;
        }
        @Override
        public Symbol getLongName() {
            return longName;
        }
        @Override
        public String getShortDescription() {
            return shortDescription;
        }
        @Override
        public String getLongDescription() {
            return longDescription;
        }
        @Override
        public List<NoteListParserElementParameter> getParameters() {
            return parameters;
        }
        public void setParameters( NoteListParserElementParameter ... params ) {
            this.parameters = new ArrayList<>( Arrays.asList( params ) );
        }
        public Symbol name() {
            return getShortName();
        }
    }

    public static final VoidEventParser PARSER_VOID = new VoidEventParser();
    static { register( PARSER_VOID ); }
    public static final class VoidEventParser extends SpecialNoteListParserElement {
        {
            // RENAMED (Thu, 01 Aug 2019 13:09:08 +0900)
            this.shortName  = s( "nop" );
            this.longName   = s( "no-operation" );
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                      "") 
                );

        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T> buffer, NoteListMap map, MetroCollector<T> result) {
        }
    }
    
    static String code( String s ) {
        return "`" + s + "`";
    }
    static String code( Symbol s ) {
        return "`" + SchemeValues.toString(s) + "`";
    }
    
    static final String ABOUT_MEASURE_LENGTH = "The number should be a real number which unit is a measure length. ";

    public static final NoteEventParser PARSER_NOTE = new NoteEventParser();
    static { register( PARSER_NOTE ); }
    public static class NoteEventParser extends SpecialNoteListParserElement {
        {
            this.shortName = s( "note" );
            this.longName  = s( "note-on-off" );
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                      "") 
                );
            this.shortDescription = "This denotes a musical note. ";
            this.longDescription = "This notation object causes the sequencer to automatically send "
                    + "both Note On MIDI event and Note Off MIDI event. ";
            
            this.setParameters(
                new NoteListParserElementParameter.Default(ID_PORT, ID_LONG, 
                    "string|number",
                    "0", 
                    "Specifies the port to output the current notation object. "
                    + "Passing a string value causes it to select the port which corresponds to the name. "
                    + "Passing a number to this parameter is treated as port creation number; "
                    + "every port is internally numbered by the order of creation and "
                    + "when a number is passed to this parameter, "
                    + "the system try to find the corresponding port by the passed creation number. "
                    ),
                new NoteListParserElementParameter.Default( ID_CHANNEL,  ID_LONG, 
                    "number",
                    "0", 
                    ID_CHANNEL + " specifies the MIDI channel of the current notation object."
                    ),
                new NoteListParserElementParameter.Default(ID_OFFSET, ID_LONG, 
                    "number",
                    "0.0", 
                    code( ID_OFFSET ) + " specifies the location of the current notation object. "
                    + ABOUT_MEASURE_LENGTH
                    + "Specify 0.0 to the head of the measure and 1.0 to the head of the next measure. "
                    ),
                new NoteListParserElementParameter.Default(ID_NOTE, ID_LONG, 
                    "number",
                    "60(C4)",
                    code( ID_NOTE ) + " specifies the pitch of the current notation object. "
                            + "The number should be a integral number which conforms MIDI note number system. "
                            + "That is, specifying 60 results getting C4. "
                            + "In Pulsar, it is available to use predefined identifiers which cover from A0 to G9. "
                    ),
                new NoteListParserElementParameter.Default( ID_VELOCITY,ID_LONG,
                    "number",
                    "0.5",
                    code( ID_VELOCITY ) + " specifies the MIDI velocity value of the current notation object. "
                            + "The number should be a real number which ranges from 0.0 to 1.0. "
                            + "Passing 0.0 causes the MIDI velocity value to become 0 "
                            + "and passing 1.0 causes it to become 127. "
                            + ""
                    ),
                new NoteListParserElementParameter.Default( ID_LENGTH, ID_LONG,
                    "number",
                    ""+DEFAULT_NOTE_LENGTH + "d", 
                    code( ID_LENGTH ) + " specifies the note length of the current notation object. "
                            + "The note length is the distance between the note-on event and the note-off event. "
                            + ABOUT_MEASURE_LENGTH
                            + ""
                    )
                );

        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T> buffer, NoteListMap map, MetroCollector<T> result) {
            boolean enabled      = readMapEnabled( map );
//          boolean enabled      = map.containsKey( ID_ENABLED     ) ? SchemeUtils.toBoolean( map.get( ID_ENABLED ) ) : true;
            if ( ! enabled )
                return;

            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            double offset    = readMapOffset( map );  
            int note         = readMapNote( map );   
            double velocity  = readMapVelocity( map );
            double length    = readMapNoteLength( map );

//            int channel      = map.containsKey( ID_CHANNEL  ) ? SchemeUtils.toInteger(      map.get(ID_CHANNEL   ) ) : 0; 
//            double offset    = map.containsKey( ID_OFFSET   ) ? SchemeUtils.toDouble(       map.get(ID_OFFSET    ) ) : 0.0d;  
//            int note         = map.containsKey( ID_NOTE     ) ? SchemeUtils.toInteger(      map.get(ID_NOTE      ) ) : 60;  
//            double velocity  = map.containsKey( ID_VELOCITY ) ? SchemeUtils.toDouble(       map.get(ID_VELOCITY  ) ) : 1d;
//            double length    = map.containsKey( ID_LENGTH   ) ? SchemeUtils.toDouble(       map.get(ID_LENGTH    ) ) : -1d; // this becomes the default value.
            
//          System.err.println("velocity");
//          System.err.println(velocity);
//          System.err.println ( map );

            // This is not necessary anymore.
//            if ( length < 0 )
//                length = DEFAULT_NOTE_LENGTH;

            result.add( buffer.noteOn( offset            , port, channel, note, velocity ) );
            result.add( buffer.noteOff( offset + length  , port, channel, note, velocity ) );
        }
    }

    public static final BarEventParser PARSER_BAR = new BarEventParser();
    static { register( PARSER_BAR ); } 
    static final class BarEventParser extends SpecialNoteListParserElement {
        {
            this.shortName = s( "len" );
            this.longName  = s( "length" );
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                      "") 
                );
            this.shortDescription = "<name/> specifies the measure length. ";
            this.longDescription = "This notation object specifies the total measure length of the current notation object set. ";
            
            this.setParameters(
                new NoteListParserElementParameter.Default(ID_VALUE, ID_LONG, 
                    "number",
                    "0.0", 
                    "Specifies the length of the measure. "
                    + ABOUT_MEASURE_LENGTH
                    + ""
                    
                    )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T> buffer, NoteListMap map, MetroCollector<T> result) {
//            double value    =  map.containsKey( ID_VALUE ) ? SchemeUtils.toDouble( map.get( ID_VALUE ) ) : -1.0d;
//            if ( value <= 0 ) {
//                LOGGER.log( Level.WARNING, "a `len` note was found but 'val was missing; this probably a bug. " );
//                value = 0.0d;
//            }
            
            // (Fri, 01 Nov 2019 06:02:11 +0900) 
            // Now the bar length default to 1.0d. See the comment of DEFAULT_BAR_LENGTH.  
            double value    =  readMapDoubleValueBarLength( map );

            // LOGGER.log( Level.INFO, "a len note = " + value );
            result.add(((MetroBufferedMidiReceiver<T>)buffer).length( value ));
        }
        public LList length(double value ) {
            return list(
                writeMapType( name() ),
                writeMapDoubleValue( value )
            );
        }
    }
    
    
    public static final ExecEventParser PARSER_EXEC = new ExecEventParser();
    static { register( PARSER_EXEC ); } 
    static final class ExecEventParser extends SpecialNoteListParserElement {
        {
            this.shortName = s( "exec" );
            this.longName  = s( "execute" );
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                      "") 
                );
            
            this.shortDescription = "<name/> invokes the specific procedure. ";
            this.longDescription = "This notation object specifies the total measure length of the current notation object set. ";
            
            this.setParameters(
                new NoteListParserElementParameter.Default(ID_VALUE, ID_LONG, 
                    "number",
                    "0.0", 
                    "Specifies the length of the measure. "
                    + ABOUT_MEASURE_LENGTH
                    + ""
                    
                    )
                );

        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T> buffer, NoteListMap map, MetroCollector<T> result) {
            double offset        = readMapOffset( map );
            Procedure procedure0 = readMapProcedure( map );
//            double offset        = map.containsKey( ID_OFFSET   )  ? SchemeUtils.toDouble(     map.get( ID_OFFSET    ) ) : 0.0d;
//            Procedure procedure0 = map.containsKey( ID_PROCEDURE ) ?                (Procedure)map.get( ID_PROCEDURE )   : null;
            
            Object[] args = {};
            // See the note ... XXX_SYNC_01
            ((MetroBufferedMidiReceiver<T>) buffer).exec( offset, 
                    SchemeInvokable.createRunnable( procedure0, args ));
        }
        
        public LList exec(double offset, Runnable runnable) {
            throw new UnsupportedOperationException();
        }
    }

    
    static abstract class TrackEventParser extends SpecialNoteListParserElement {
        MetroTrack searchSyncTrack( Pulsar pulsar, Object id ) {
            List<MetroTrack> trackList = pulsar.searchTrack( id );
            if ( trackList.isEmpty() ) {
                logWarn( "PARSER_PUT : syncTrackId '" + id + "' was not found and it was ignored. " );
                return null;
            } else {
                return trackList.get( 0 );
            }
        }
        
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T> buffer, NoteListMap map, MetroCollector<T> result) {
            Pulsar pulsar = ((Pulsar)metro);
            double offset         = readMapOffset( map );
            Object id             = readMapNewId( map );
            List<Object> tags     = readMapCollection( ID_TAGS, map ); 
            MetroSyncType styp    = readMapSyncType( map );
            Object stra           = map.get( ID_SYNC_TRACK_ID, THRU, NULL );
            double soff           = map.get( ID_SYNC_OFFSET, S2J_DOUBLE, DEFAULT_VALUE_DOUBLE_0 );
            Procedure procedure   = map.get( ID_PROCEDURE, S2J_PROCEDURE, (NoteListValueGenerator<Procedure>)NoteListValueGenerator.NULL );

//            List<Object> el = Collections.emptyList();
//            double offset         = getValue( map, ID_OFFSET, 0.0d, (v)-> SchemeUtils.toDouble( v )   );
////            String id             = getValue( map, "id",   null, (v)-> SchemeUtils.anyToString(    SchemeUtils.schemeNullCheck( v ) ) );
//            Object _id            = getValue( map, "id",   null, (v)-> v );
//            List<Object> tags     = getValue( map, "tags", el,   (v)-> new ArrayList<Object>( (Pair)v ) );
//            MetroSyncType syty         = getValue( map, "syty", MetroSyncType.SERIAL, (v)-> MetroSyncType.toSyncType( SchemeUtils.schemeSymbolToJavaString( SchemeUtils.schemeNullCheck( v ) ) ) );
//            String syid           = getValue( map, "syid", null, (v)-> SchemeUtils.anyToString(    SchemeUtils.schemeNullCheck( v ) ) );
//            double syof           = getValue( map, "syof", 0.0d, (v)-> SchemeUtils.toDouble( v )   );
//            Procedure procedure   = getValue( map, ID_PROCEDURE, null, (v)->{
//                return SchemeSequence.asProcedure( v );
//            });
//            if ( _id == null ) {
//                // MODIFIED (Tue, 23 Jul 2019 05:55:01 +0900)
//                // id = createDefaultId( pulsar.getScheme() );
//                _id = createTempNewId();
//            }
            
            
            logInfo( "NoteListParserElement: id=" + id );
            
            // See the note ... XXX_SYNC_01
            ((MetroBufferedMidiReceiver<T>) buffer).exec( offset, new Runnable() {
                @Override
                public void run() {
                    logInfo( "NoteListParserElement: exec" );
                    
                    // synchronized block added at (Mon, 29 Jul 2019 13:36:52 +0900)
                    synchronized ( metro.getMetroLock() ) {                       
                        processTrack( pulsar, id, tags, procedure, styp, stra, soff );
                    }
                }
            });
        }


        abstract void processTrack( Pulsar pulsar, Object id, List<Object> tags, Procedure procedure,
                MetroSyncType syncType, Object syncTrackId, double syncOffset );
    }
    
//  private static final Procedure SCHEME_NOP = new ProcedureN() {
//  };
    public static final PutEventParser PARSER_PUT = new PutEventParser();
    static { register( PARSER_PUT ); }
    static final class PutEventParser extends TrackEventParser {
        {
            // RENAMED (Thu, 01 Aug 2019 13:09:08 +0900)
            // RENAMED AGAIN (Thu, 10 Oct 2019 04:55:11 +0900)
            this.shortName = s( "putt" );
            this.longName  = s( "put-track" );
            this.parameters = Arrays.asList();
        }
        @Override
        void processTrack( Pulsar pulsar, Object id, List<Object> tags, Procedure procedure,
                MetroSyncType syncType, Object syncTrackId, double syncOffset ) {
            logInfo( "putt id:" + id );
            
            // if the target procedure is null, just ignore it.
            if ( procedure == null )
                return;
            
//            SchemeSequence sequence = pulsar.asSequence( procedure );
//            SchemeSequence sequence = pulsar.asSequence( procedure );
            synchronized ( pulsar.getMetroLock() ) {
                MetroTrack syncTrack;
                if ( syncTrackId == null )
                    syncTrack = null;
                else
                    syncTrack = searchSyncTrack( pulsar, syncTrackId );
                
                if ( !(syncTrack instanceof MetroSyncTrack)) {
                    // is throwing exception here allowed?? (Tue, 05 May 2020 16:27:20 +0900) 
                    throw new IllegalArgumentException("syncType must be a sync track" );
                }
                
                try {
                    MetroBufferedTrack track = PulsarTrack.createTrack( id, tags, procedure );
                    track.setSyncStatus( syncType, (MetroSyncTrack) syncTrack, syncOffset );
                    pulsar.registerTrack( track );
                } finally {
                    pulsar.notifyTrackChange();
                }
            }
        }
    }
    
    public static final RemoveEventParser PARSER_REMOVE = new RemoveEventParser();
    static { register( PARSER_REMOVE ); }
    static final class RemoveEventParser extends TrackEventParser {
        {
            // RENAMED (Thu, 01 Aug 2019 13:09:08 +0900)
            // RENAMED AGAIN (Thu, 10 Oct 2019 04:55:11 +0900)
            this.shortName = s( "remt" );
            this.longName  = s( "remove-track" );
            this.parameters = Arrays.asList();
        }
        @Override
        void processTrack( Pulsar pulsar, Object id, List<Object> tags, Procedure procedure,
                MetroSyncType syncType, Object syncTrackId, double syncOffset ) {

            logInfo( "remt id:" + id );
            synchronized ( pulsar.getMetroLock() ) {
                List<MetroTrack> trackList;
                MetroTrack syncTrack;

                if ( id == null ) 
                    trackList = null;
                else
                    trackList = pulsar.searchTrack( id );
                
                if ( syncTrackId == null )
                    syncTrack = null;
                else
                    syncTrack = searchSyncTrack( pulsar, syncTrackId );
                
                if ( trackList == null ) {
                    logWarn( "RemoveEventParser: the track (" + id + ") does not exist" );
                    return;
                }
                
                if ( syncTrackId != null && syncTrack == null ) {
                    logWarn( "RemoveEventParser: the sync-track (" + id + ") does not exist" );
                }
                
                try {
                    pulsar.removeTrack( trackList, syncType, syncTrack, syncOffset );
                } finally {
                    pulsar.notifyTrackChange();
                }
                
            }
        }
    }

    
    static abstract class AbstractRemoveEventParser extends SpecialNoteListParserElement {
        abstract void removeTrackProc( Metro metro, MetroTrack track );
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T> buffer, NoteListMap map, MetroCollector<T> result) {
            double offset            = readMapOffset( map );
            Collection argTrackList  = readMapCollection( ID_ID , map );
            Collection<String> tags  = readMapCollection( ID_TAGS, map );

//            List<String> el = Collections.emptyList();
//            double offset            = getValue( map, ID_OFFSET, 0.0d, (v)-> SchemeUtils.toDouble( v )   );
//            Collection argTrackList  = getValue( map, "id",   null, (v)-> v instanceof Pair ? (Collection)v : (Collection)LList.makeList(Arrays.asList( v ) ));
//            Collection<String> tags  = getValue( map, "tags", el,   (v)-> SchemeUtils.symbolListToStringList((Pair)v ) );
            
            if ( ( argTrackList != null ) && ! argTrackList.isEmpty() ) {
                ((MetroBufferedMidiReceiver<T>) buffer).exec( offset, new Runnable() {
                    @Override
                    public void run() {
                        // synchronized block added at (Mon, 29 Jul 2019 13:36:52 +0900)
                        synchronized ( metro.getMetroLock() ) {
                            try {
                                for ( Object v : argTrackList ) {
                                    v = SchemeValues.schemeNullCheck(v);
                                    
                                    List<MetroTrack> trackList=null;
                                    // I think this is not correct anymore. 
                                    // MODIFIED >>> (Sun, 15 Sep 2019 11:16:06 +0900)
                                    // if ( v instanceof IString ) {
                                    //     String id = v==null ? null : SchemeUtils.anyToString( v );
                                    //     track = metro.searchTrack( id );
                                    // } else if ( v instanceof MetroTrack ) {
                                    //     track = (MetroTrack) v ;
                                    // }
                                    if ( v instanceof MetroTrack ) {
                                        trackList = Arrays.asList(  (MetroTrack) v );
                                    } else {
                                        trackList = metro.searchTrack( v );
                                    }
                                    // MODIFIED <<< (Sun, 15 Sep 2019 11:16:06 +0900)

    
                                    if ( ! trackList.isEmpty() ) {
                                        for ( MetroTrack track : trackList ) {
                                            removeTrackProc( metro, track );
                                        }
                                    } else {
                                        logWarn( "PARSER_KILL : the passed value '"+v+"' was improper. We ignored it." );
                                    }
                                }
                            } finally {
                                metro.notifyTrackChange();
                            }
                        }
                    }
                });
            }

            if ( tags != null ) {
                ((MetroBufferedMidiReceiver<T>) buffer).exec( offset, new Runnable() {
                    @Override
                    public void run() {
                        // synchronized block added at (Mon, 29 Jul 2019 13:36:52 +0900)
                        synchronized ( metro.getMetroLock() ) {
                            for ( MetroTrack t : metro.searchTracksByTagSet(tags) ) {
                                removeTrackProc( metro, t );
                            }
                        }
                    }
                } );
            }
        }
    }

    
    public static final KillEventParser PARSER_KILL = new KillEventParser();
    static { register( PARSER_KILL ); }
    static final class KillEventParser extends AbstractRemoveEventParser {
        {
            // RENAMED (Thu, 01 Aug 2019 13:09:08 +0900)
            this.shortName = s( "kil" );
            this.longName  = s( "kill-track" );
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                      "") 
                );
//          this.longName = "Remove the specified track";
        }
        @Override
        void removeTrackProc(Metro metro, MetroTrack track) {
            synchronized ( metro.getMetroLock() ) {
                try {
                    metro.unregisterTrack(track);
                } finally {
                    metro.notifyTrackChange();
                }
            }
        }
    }
    
    public static final DeleteEventParser PARSER_DELETE = new DeleteEventParser();
    static { register( PARSER_DELETE ); }
    static final class DeleteEventParser extends AbstractRemoveEventParser {
        {
            this.shortName = s( "del" );
            this.longName = s( "delete-track" );
            this.parameters = Arrays.asList();
        }
        @Override
        void removeTrackProc(Metro metro, MetroTrack track) {
            track.removeGracefully(metro);
//            ((MetroBufferedTrack) track).removeGracefully(this);
        }
    }

    public static final EndEventParser PARSER_END = new EndEventParser();
    static { register( PARSER_END ); } 
    static final class EndEventParser extends SpecialNoteListParserElement {
        {
            this.shortName = s( "end" );
            this.longName  = s( "end-track" );
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                      "") 
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T> buffer, NoteListMap map, MetroCollector<T> result) {
            buffer.end();
        }
    }
}

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

package lamu;

import static lamu.NoteListCommon.*;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;

import gnu.lists.LList;
import gnu.mapping.Symbol;
import lamu.lib.log.PulsarLogger;
import metro.Metro;
import metro.MetroBufferedMidiReceiver;
import metro.MetroCollector;
import metro.MetroMidi;
import metro.MetroMidi.MetroMidiAllNoteOff;
import metro.MetroMidi.MetroMidiAllSoundOff;
import metro.MetroMidi.MetroMidiChannelPressure;
import metro.MetroMidi.MetroMidiClock;
import metro.MetroMidi.MetroMidiContinue;
import metro.MetroMidi.MetroMidiControlBalance;
import metro.MetroMidi.MetroMidiControlBankSelect;
import metro.MetroMidi.MetroMidiControlBreathController;
import metro.MetroMidi.MetroMidiControlChange;
import metro.MetroMidi.MetroMidiControlDataDecrement;
import metro.MetroMidi.MetroMidiControlDataEntryMsb;
import metro.MetroMidi.MetroMidiControlDataIncrement;
import metro.MetroMidi.MetroMidiControlEffect1;
import metro.MetroMidi.MetroMidiControlEffect2;
import metro.MetroMidi.MetroMidiControlEffect3;
import metro.MetroMidi.MetroMidiControlEffect4;
import metro.MetroMidi.MetroMidiControlEffect5;
import metro.MetroMidi.MetroMidiControlEffectController1;
import metro.MetroMidi.MetroMidiControlEffectController2;
import metro.MetroMidi.MetroMidiControlExpression;
import metro.MetroMidi.MetroMidiControlFootController;
import metro.MetroMidi.MetroMidiControlGeneralPurpose01;
import metro.MetroMidi.MetroMidiControlGeneralPurpose02;
import metro.MetroMidi.MetroMidiControlGeneralPurpose03;
import metro.MetroMidi.MetroMidiControlGeneralPurpose04;
import metro.MetroMidi.MetroMidiControlHold2;
import metro.MetroMidi.MetroMidiControlLegatoSwitch;
import metro.MetroMidi.MetroMidiControlModulation;
import metro.MetroMidi.MetroMidiControlNrpnLsb;
import metro.MetroMidi.MetroMidiControlNrpnMsb;
import metro.MetroMidi.MetroMidiControlPan;
import metro.MetroMidi.MetroMidiControlPedalSwitch;
import metro.MetroMidi.MetroMidiControlPortamento;
import metro.MetroMidi.MetroMidiControlPortamentoSwitch;
import metro.MetroMidi.MetroMidiControlPortamentoTime;
import metro.MetroMidi.MetroMidiControlRpnLsb;
import metro.MetroMidi.MetroMidiControlRpnMsb;
import metro.MetroMidi.MetroMidiControlSostenutoSwitch;
import metro.MetroMidi.MetroMidiControlSoundController1;
import metro.MetroMidi.MetroMidiControlSoundController10;
import metro.MetroMidi.MetroMidiControlSoundController2;
import metro.MetroMidi.MetroMidiControlSoundController3;
import metro.MetroMidi.MetroMidiControlSoundController4;
import metro.MetroMidi.MetroMidiControlSoundController5;
import metro.MetroMidi.MetroMidiControlSoundController6;
import metro.MetroMidi.MetroMidiControlSoundController7;
import metro.MetroMidi.MetroMidiControlSoundController8;
import metro.MetroMidi.MetroMidiControlSoundController9;
import metro.MetroMidi.MetroMidiControlSustainPedal;
import metro.MetroMidi.MetroMidiControlVolume;
import metro.MetroMidi.MetroMidiEndOfExclusive;
import metro.MetroMidi.MetroMidiKeyPressure;
import metro.MetroMidi.MetroMidiLocalControls;
import metro.MetroMidi.MetroMidiMonoModeOn;
import metro.MetroMidi.MetroMidiNoteOff;
import metro.MetroMidi.MetroMidiNoteOn;
import metro.MetroMidi.MetroMidiOmniModeOff;
import metro.MetroMidi.MetroMidiOmniModeOn;
import metro.MetroMidi.MetroMidiPitchBend;
import metro.MetroMidi.MetroMidiPolyModeOn;
import metro.MetroMidi.MetroMidiProgramChange;
import metro.MetroMidi.MetroMidiReset;
import metro.MetroMidi.MetroMidiResetAllControllers;
import metro.MetroMidi.MetroMidiSongPositionPointer;
import metro.MetroMidi.MetroMidiSongSelect;
import metro.MetroMidi.MetroMidiStart;
import metro.MetroMidi.MetroMidiStop;
import metro.MetroMidiMessageGen;
import metro.MetroPort;
import metro.MetroTrack;

/**
 * Defines MIDI events.
 *  
 * See : 
 *    http://nickfever.com/music/midi-cc-list
 * @author ats
 *
 */
public class PulsarMidiNoteListParsers {
    static final PulsarLogger LOGGER = PulsarLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) {
        LOGGER.log(Level.SEVERE, msg, e);
    }
    static void logInfo(String msg) {
        LOGGER.log(Level.INFO, msg);
    }
    static void logWarn(String msg) {
        LOGGER.log(Level.WARNING, msg);
    }
    
    /**
     * 
     */
    public static Collection<NoteListParserElement> getElements() {
        return Collections.unmodifiableList( elements );
    }

    
    static ArrayList<MidiNoteListParserElement> elements = new ArrayList<>();
    static void register( MidiNoteListParserElement info ) {
        elements.add( info );
    }
    
    static abstract class MidiNoteListParserElement<MIDI extends MetroMidi> extends NoteListParserElement {
        protected MIDI midi;
        protected List<NoteListParserElementParameter> parameters;
        
//        public abstract boolean parseEvent( Metro metro, MetroTrack track, MetroEventBuffer<T>  receiver, NoteListMap map  );
        @Override
        public Symbol getShortName() {
            return s( this.midi.getShortName() );
        }
        @Override
        public Symbol getLongName() {
            return s( this.midi.getLongName() );
        }
        @Override
        public String getShortDescription() {
            return midi.getShortDescription();
        }
        @Override
        public String getLongDescription() {
            return midi.getLongDescription();
        }
        @Override
        public List<NoteListParserElementParameter> getParameters() {
            return parameters;
        }
        public void setParameters(List<NoteListParserElementParameter> parameters) {
            this.parameters = parameters;
        }
        
        public Symbol name() {
            return getShortName();
        }
    }
    

    
    //////////////////////////////////////////////////////////////////////////////////////////

    public static final MetroNoteParserError PARSER_ERROR = new MetroNoteParserError();
    public static final class MetroNoteParserError extends MidiNoteListParserElement<MetroMidiNoteOn> {
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            result.add( buffer.error( offset, port, "unknown error" ) );
        }

        public LList error(double offset, MetroPort port, String message) {
            return list(
                writeMapType( name() ),
                writeMapOffset( offset ),
                writeMapPort( port ),
                writeMapString( ID_MESSAGE, message )
            );
        }
    }

    //////////////////////////////////////////////////////////////////////////////////////////
    
    public static final MetroNoteParserNoteOn PARSER_NOTE_ON = new MetroNoteParserNoteOn();
    public static final class MetroNoteParserNoteOn extends MidiNoteListParserElement<MetroMidiNoteOn> {
        {
            this.midi = MetroMidi.MIDI_NOTE_ON;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            boolean enabled      = readMapEnabled( map );
            if ( ! enabled )
                return;

            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int note         = readMapNote( map );  
            double velocity  = readMapVelocity( map );
            
            result.add( buffer.noteOn( offset, port, channel, note, velocity ) );
        }
        public LList noteOn(double offset, MetroPort port, int channel, int note, double velocity) {
            return list(
                writeMapType( name() ),
                writeMapOffset( offset ), 
                writeMapPort( port ),
                writeMapChannel( channel ), 
                writeMapNote( note ),
                writeMapVelocity( velocity )
            );
        }
        public LList noteOn(double offset, MetroPort port, int channel, int note, int velocity ) {
            return list(
                writeMapType( name() ),
                writeMapOffset( offset ), 
                writeMapPort( port ),
                writeMapChannel( channel ), 
                writeMapNote( note ),
                writeMapVelocity( MetroMidiMessageGen.i2dVelocity( velocity ) )
            );
        }
    }
    static { register( PARSER_NOTE_ON ); }

    public static final MetroNoteParserNoteOff PARSER_NOTE_OFF = new MetroNoteParserNoteOff();
    public static final class MetroNoteParserNoteOff extends MidiNoteListParserElement<MetroMidiNoteOff> {
        {
            this.midi = MetroMidi.MIDI_NOTE_OFF;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            boolean enabled      = readMapEnabled( map );
            if ( ! enabled )
                return;

            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int note         = readMapNote( map );  
            double velocity  = readMapVelocity( map );
            
            result.add( buffer.noteOff( offset, port, channel, note, velocity ) );
        }
        public LList noteOff(double offset, MetroPort port, int channel, int note, double velocity) {
            return list(
                writeMapType( name() ),
                writeMapOffset( offset ),
                writeMapPort( port ),
                writeMapChannel( channel ), 
                writeMapNote( note ),  
                writeMapVelocity( velocity )
                );
        }
        public LList noteOff(double offset, MetroPort port, int channel, int note, int velocity) {
            return list(
                writeMapType( name() ),
                writeMapOffset( offset ),
                writeMapPort( port ),
                writeMapChannel( channel ), 
                writeMapNote( note ),  
                writeMapVelocity( MetroMidiMessageGen.i2dVelocity( velocity ) )
                );
        }
    }
    static { register( PARSER_NOTE_OFF ); }

    public static final MetroNoteParserKeyPressure PARSER_KEY_PRESSURE = new MetroNoteParserKeyPressure();
    public static final class MetroNoteParserKeyPressure extends MidiNoteListParserElement<MetroMidiKeyPressure> {
        {
            this.midi = MetroMidi.MIDI_KEY_PRESSURE;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int note         = readMapNote( map );  
            double pressure  = readMapDoubleValue( map );

            result.add( buffer.keyPressure( offset , port, channel, note, pressure ) );
        }
        public LList keyPressure(double offset, MetroPort port, int channel, int note, double pressure) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapNote(note),
                writeMapDoubleValue(pressure)
            );
        }
        public LList keyPressure(double offset, MetroPort port, int channel, int note, int pressure) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset), 
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapNote(note),  
                writeMapDoubleValue( MetroMidiMessageGen.i2dPressure( pressure ))
            );
        }
    }
    static { register( PARSER_KEY_PRESSURE ); }

    public static final MetroNoteParserControlChange PARSER_CONTROL_CHANGE = new MetroNoteParserControlChange();
    public static final class MetroNoteParserControlChange extends MidiNoteListParserElement<MetroMidiControlChange>        {
        {
            this.midi = MetroMidi.MIDI_CONTROL_CHANGE;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset     = readMapOffset( map );  
            MetroPort port    = readMapPort( map );
            int channel       = readMapChannel( map ); 
            int controlNumber = readMapKey( map ); 
            int controlValue  = readMapIntegerValueDefault0( map ); 

            result.add( buffer.controlChange( offset, port, channel, controlNumber, controlValue ) );
        }
        public LList controlChange(double offset, MetroPort port, int channel, int controlNumber, int controlValue) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapKey(controlNumber), 
                writeMapIntegerValueDefault0(controlValue)
            );
        }
    }
    static { register( PARSER_CONTROL_CHANGE ); }

    public static final MetroNoteParserProgramChange  PARSER_PROGRAM_CHANGE = new MetroNoteParserProgramChange(); 
    public static final class MetroNoteParserProgramChange extends MidiNoteListParserElement<MetroMidiProgramChange>        {
        {
            this.midi = MetroMidi.MIDI_PROGRAM_CHANGE;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map );
            int value        = readMapIntegerValueDefault0( map );
//            int value        = map.get( ID_VALUE, S2J_INTEGER, DEFAULT_VALUE_INTEGER_0 );
//            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.programChange( offset , port, channel, value ) );
        }
        public LList programChange(double offset, MetroPort port, int channel, int programNumber) {
            return list(
                    writeMapType( name() ),
                    writeMapOffset(offset),  
                    writeMapPort(port),
                    writeMapChannel(channel),
                    writeMapIntegerValueDefault0(programNumber)
                );
        }
    }
    static { register( PARSER_PROGRAM_CHANGE ); }

    public static final MetroNoteParserChannelPressure  PARSER_CHANNEL_PRESSURE = new MetroNoteParserChannelPressure(); 
    public static final class MetroNoteParserChannelPressure extends MidiNoteListParserElement<MetroMidiChannelPressure>        {
        {
            this.midi = MetroMidi.MIDI_CHANNEL_PRESSURE;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            double pressureValue     = readMapDoubleValue( map );

            result.add( buffer.channelPressure( offset , port, channel, pressureValue ) );
        }
        public LList channelPressure(double offset, MetroPort port, int channel, double pressureValue) {
            return list(
                    writeMapType( name() ),
                    writeMapOffset(offset),  
                    writeMapPort(port),
                    writeMapChannel(channel), 
                    writeMapDoubleValue(pressureValue)
                );
        }
        public LList channelPressure(double offset, MetroPort port, int channel, int pressureValue) {
            return list(
                    writeMapType( name() ),
                    writeMapOffset(offset),  
                    writeMapPort(port),
                    writeMapChannel(channel), 
                    writeMapDoubleValue( MetroMidiMessageGen.i2dPressure( pressureValue ))
                );
        }
    }
    static { register( PARSER_CHANNEL_PRESSURE ); }

    public static final MetroNoteParserPitchBend  PARSER_PITCH_BEND = new MetroNoteParserPitchBend(); 
    public static final class MetroNoteParserPitchBend extends MidiNoteListParserElement<MetroMidiPitchBend>        {
        {
            this.midi = MetroMidi.MIDI_PITCH_BEND;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset         = readMapOffset( map );  
            MetroPort port        = readMapPort( map );
            int channel           = readMapChannel( map ); 
            double pitchBendValue = readMapDoubleValue( map );

            result.add( buffer.pitchBend( offset , port, channel, pitchBendValue ) );
        }
        public LList pitchBend(double offset, MetroPort port, int channel, double pitchBendValue) {
            return list(
                    writeMapType( name() ),
                    writeMapOffset(offset),  
                    writeMapPort(port),
                    writeMapChannel(channel), 
                    writeMapDoubleValue(pitchBendValue)
                );
        }
        public LList pitchBend(double offset, MetroPort port, int channel, int pitchBendValue) {
            return list(
                    writeMapType( name() ),
                    writeMapOffset(offset),  
                    writeMapPort(port),
                    writeMapChannel(channel), 
                    writeMapDoubleValue( MetroMidiMessageGen.i2dPitchBend( pitchBendValue ))
                );
        }
    }
    static { register( PARSER_PITCH_BEND ); }

    /*
     * Channel Mode Control Change 
     */
    public static final MetroNoteParserAllSoundOff  PARSER_ALL_SOUND_OFF = new MetroNoteParserAllSoundOff(); 
    public static final class MetroNoteParserAllSoundOff extends MidiNoteListParserElement<MetroMidiAllSoundOff>        {
        {
            this.midi = MetroMidi.MIDI_ALL_SOUND_OFF;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 

            result.add( buffer.cc_allSoundOff( offset , port, channel ) );
        }
        public LList cc_allSoundOff(double offset, MetroPort port, int channel) {
            return list(
                    writeMapType( name() ),
                    writeMapOffset(offset),
                    writeMapPort(port),
                    writeMapChannel(channel)
                );
        }
    }
    static { register( PARSER_ALL_SOUND_OFF ); }

    public static final MetroNoteParserResetAllController  PARSER_RESET_ALL_CONTROLLERS = new MetroNoteParserResetAllController(); 
    public static final class MetroNoteParserResetAllController extends MidiNoteListParserElement<MetroMidiResetAllControllers>     {
        {
            this.midi = MetroMidi.MIDI_RESET_ALL_CONTROLLERS;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 

            result.add( buffer.cc_resetAllControllers( offset , port, channel ) );
        }
        public LList cc_resetAllControllers(double offset, MetroPort port, int channel) {
            return list(
                    writeMapType( name() ),
                    writeMapOffset(offset),
                    writeMapPort(port),
                    writeMapChannel(channel)
                );
        }
    }
    static { register( PARSER_RESET_ALL_CONTROLLERS ); }

    public static final MetroNoteParserLocalControls  PARSER_LOCAL_CONTROLS = new MetroNoteParserLocalControls(); 
    public static final class MetroNoteParserLocalControls extends MidiNoteListParserElement<MetroMidiLocalControls> {
        {
            this.midi = MetroMidi.MIDI_LOCAL_CONTROLS;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map );
            boolean on       = readMapBooleanValueDefaultFalse( map );
//            boolean on       = map.get( ID_VALUE , S2J_BOOLEAN, DEFAULT_VALUE_FALSE );
//          boolean on       = map.containsKey( ID_VALUE    ) ? SchemeUtils.toBoolean(      map.get(ID_VALUE     ) ) : false; 

            result.add( buffer.cc_localControls( offset , port, channel, on ) );
        }
        public LList cc_localControls(double offset, MetroPort port, int channel, boolean on) {
            return list(
                    writeMapType( name() ),
                    writeMapOffset(offset),  
                    writeMapPort(port),
                    writeMapChannel(channel),
                    writeMapBooleanValueDefaultFalse(on)
                );
        }
    }
    static { register( PARSER_LOCAL_CONTROLS ); }

    public static final MetroNoteParserAllNoteOff  PARSER_ALL_NOTE_OFF = new MetroNoteParserAllNoteOff(); 
    public static final class MetroNoteParserAllNoteOff extends MidiNoteListParserElement<MetroMidiAllNoteOff> {
        {
            this.midi = MetroMidi.MIDI_ALL_NOTE_OFF;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 

            result.add( buffer.cc_allNoteOff( offset , port, channel ) );
        }
        public LList cc_allNoteOff(double offset, MetroPort port, int channel) {
            return list(
                    writeMapType( name() ),
                    writeMapOffset(offset),
                    writeMapPort(port),
                    writeMapChannel(channel)
                );
        }
    }
    static { register( PARSER_ALL_NOTE_OFF ); }

    public static final MetroNoteParserOmniModeOff  PARSER_OMNI_MODE_OFF = new MetroNoteParserOmniModeOff(); 
    public static final class MetroNoteParserOmniModeOff extends MidiNoteListParserElement<MetroMidiOmniModeOff> {
        {
            this.midi = MetroMidi.MIDI_OMNI_MODE_OFF;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 

            result.add( buffer.cc_omniModeOff( offset , port, channel ) );
        }
        public LList cc_omniModeOff(double offset, MetroPort port, int channel) {
            return list(
                    writeMapType( name() ),
                    writeMapOffset(offset),
                    writeMapPort(port),
                    writeMapChannel(channel)
                );
        }
    }
    static { register( PARSER_OMNI_MODE_OFF ); }

    public static final MetroNoteParserOmniModeOn  PARSER_OMNI_MODE_ON = new MetroNoteParserOmniModeOn(); 
    public static final class MetroNoteParserOmniModeOn extends MidiNoteListParserElement<MetroMidiOmniModeOn> {
        {
            this.midi = MetroMidi.MIDI_OMNI_MODE_ON;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 

            result.add( buffer.cc_omniModeOn( offset , port, channel ) );
        }
        public LList cc_omniModeOn(double offset, MetroPort port, int channel) {
            return list(
                    writeMapType( name() ),
                    writeMapOffset(offset),
                    writeMapPort(port),
                    writeMapChannel(channel)
                );
        }
    }
    static { register( PARSER_OMNI_MODE_ON ); }

    // TODO : Isn't this mono-mode-on? This seems incorrect.  
    public static final MetroNoteParserMonoModeOn  PARSER_MONO_MODE_OFF = new MetroNoteParserMonoModeOn(); 
    public static final class MetroNoteParserMonoModeOn extends MidiNoteListParserElement<MetroMidiMonoModeOn> {
        {
            this.midi = MetroMidi.MIDI_MONO_MODE_ON;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 

            result.add( buffer.cc_monoModeOn( offset , port, channel ) );
        }
        public LList cc_monoModeOn(double offset, MetroPort port, int channel) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel)
                );
        }
    }
    static { register( PARSER_MONO_MODE_OFF ); }

    public static final MetroNoteParserPolyModeOn  PARSER_POLY_MODE_ON = new MetroNoteParserPolyModeOn(); 
    public static final class MetroNoteParserPolyModeOn extends MidiNoteListParserElement<MetroMidiPolyModeOn> {
        {
            this.midi = MetroMidi.MIDI_POLY_MODE_ON;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 

            result.add( buffer.cc_polyModeOn( offset , port, channel ) );
        }
        public LList cc_polyModeOn(double offset, MetroPort port, int channel) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),
                writeMapPort(port),
                writeMapChannel(channel)
                );
        }
    }
    static { register( PARSER_POLY_MODE_ON ); }

    
    // TODO : the channel value is not necessary. Remove it from SchemeNoteParser , too. 
    public static final MetroNoteParserSongPositionPointer  PARSER_SONG_POSITION_POINTER = new MetroNoteParserSongPositionPointer(); 
    public static final class MetroNoteParserSongPositionPointer extends MidiNoteListParserElement<MetroMidiSongPositionPointer> {
        {
            this.midi = MetroMidi.MIDI_SONG_POSITION_POINTER;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
//          int channel      = readMapChannel( map );
            int pos        = readMapIntegerValueDefault0( map );
//            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.songPositionPointer( offset , port, pos ) );
        }
        public LList songPositionPointer(double offset, MetroPort port, int pos) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapIntegerValueDefault0(pos)
            );
        }
    }
    static { register( PARSER_SONG_POSITION_POINTER ); }

    // TODO : the channel value is not necessary. Remove it from SchemeNoteParser , too. 
    public static final MetroNoteParserSongSelect  PARSER_SONG_SELECT = new MetroNoteParserSongSelect(); 
    public static final class MetroNoteParserSongSelect extends MidiNoteListParserElement<MetroMidiSongSelect> {
        {
            this.midi = MetroMidi.MIDI_SONG_SELECT;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
//          int channel      = readMapChannel( map );
            int songNumber        = readMapIntegerValueDefault0( map );
//            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.songSelect( offset , port, songNumber ) );
        }
        public LList songSelect(double offset, MetroPort port, int songNumber) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapIntegerValueDefault0(songNumber)
            );
        }
    }
    static { register( PARSER_SONG_SELECT ); }

    public static final MetroNoteParserEndOfExclusive  PARSER_END_OF_EXCLUSIVE = new MetroNoteParserEndOfExclusive(); 
    public static final class MetroNoteParserEndOfExclusive extends MidiNoteListParserElement<MetroMidiEndOfExclusive> {
        {
            this.midi = MetroMidi.MIDI_END_OF_EXCLUSIVE;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
//            int channel      = readMapChannel( map ); 

            result.add( buffer.endOfExclusive( offset , port ) );
        }
        public LList endOfExclusive(double offset, MetroPort port) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),
                writeMapPort(port)
                );
        }
    }
    static { register( PARSER_END_OF_EXCLUSIVE ); }

    public static final MetroNoteParserClock  PARSER_CLOCK = new MetroNoteParserClock(); 
    public static final class MetroNoteParserClock extends MidiNoteListParserElement<MetroMidiClock> {
        {
            this.midi = MetroMidi.MIDI_CLOCK;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
//            int channel      = readMapChannel( map ); 

            result.add( buffer.clock( offset , port ) );
        }
        public LList clock(double offset, MetroPort port) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),
                writeMapPort(port)
            );
        }
    }
    static { register( PARSER_CLOCK ); }

    
    public static final MetroNoteParserStart  PARSER_START = new MetroNoteParserStart(); 
    public static final class MetroNoteParserStart extends MidiNoteListParserElement<MetroMidiStart> {
        {
            this.midi = MetroMidi.MIDI_START;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
//            int channel      = readMapChannel( map ); 

            result.add( buffer.start( offset , port ) );
        }
        public LList start(double offset, MetroPort port) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),
                writeMapPort(port)
                );
        }
    }
    static { register( PARSER_START ); }

    public static final MetroNoteParserContinue  PARSER_CONTINUE = new MetroNoteParserContinue(); 
    public static final class MetroNoteParserContinue extends MidiNoteListParserElement<MetroMidiContinue> {
        {
            this.midi = MetroMidi.MIDI_CONTINUE;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
//            int channel      = readMapChannel( map ); 

            result.add( buffer.cont( offset , port ) );
        }
        public LList cont(double offset, MetroPort port) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port)
                );
        }
    }
    static { register( PARSER_CONTINUE ); }

    public static final MetroNoteParserStop  PARSER_STOP = new MetroNoteParserStop(); 
    public static final class MetroNoteParserStop extends MidiNoteListParserElement<MetroMidiStop> {
        {
            this.midi = MetroMidi.MIDI_STOP;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
//            int channel      = readMapChannel( map ); 

            result.add( buffer.stop( offset , port ) );
        }
        public LList stop(double offset, MetroPort port) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port)
                );
        }
    }
    static { register( PARSER_STOP ); }

    public static final MetroNoteParserReset  PARSER_RESET = new MetroNoteParserReset(); 
    public static final class MetroNoteParserReset extends MidiNoteListParserElement<MetroMidiReset> {
        {
            this.midi = MetroMidi.MIDI_RESET;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
//            int channel      = readMapChannel( map ); 

            result.add( buffer.reset( offset , port ) );
        }
        public LList reset(double offset, MetroPort port) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port)
                );
        }
    }
    static { register( PARSER_RESET ); }

    public static final MetroNoteParserControlBankSelect PARSER_BANK_SELECT  = new MetroNoteParserControlBankSelect();
    public static final class MetroNoteParserControlBankSelect extends MidiNoteListParserElement<MetroMidiControlBankSelect> {
        {
            this.midi = MetroMidi.MIDI_BANK_SELECT;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_bankSelect( offset, port, channel, value ) );
        }
        public LList cc_bankSelect(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value));
        }
    }
    static { register( PARSER_BANK_SELECT ); }

    public static final MetroNoteParserControlModulation PARSER_MODULATION  = new MetroNoteParserControlModulation();
    public static final class MetroNoteParserControlModulation extends MidiNoteListParserElement<MetroMidiControlModulation> {
        {
            this.midi = MetroMidi.MIDI_MODULATION;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_modulation( offset, port, channel, value ) );
        }
        public LList cc_modulation(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
                );
        }
    }
    static { register( PARSER_MODULATION ); }

    public static final MetroNoteParserControlBreathController PARSER_BREATH_CTRL  = new MetroNoteParserControlBreathController();
    public static final class MetroNoteParserControlBreathController extends MidiNoteListParserElement<MetroMidiControlBreathController> {
        {
            this.midi = MetroMidi.MIDI_BREATH_CTRL;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_breathController( offset, port, channel, value ) );
        }
        public LList cc_breathController(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
                );
        }
    }
    static { register( PARSER_BREATH_CTRL ); }

    public static final MetroNoteParserControlFootController PARSER_FOOT_CTRL  = new MetroNoteParserControlFootController();
    public static final class MetroNoteParserControlFootController extends MidiNoteListParserElement<MetroMidiControlFootController> {
        {
            this.midi = MetroMidi.MIDI_FOOT_CTRL;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_footController( offset, port, channel, value ) );
        }
        public LList cc_footController(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
                );
        }
    }
    static { register( PARSER_FOOT_CTRL ); }

    public static final MetroNoteParserControlPortamentoTime PARSER_PORTAMENTO_TIME  = new MetroNoteParserControlPortamentoTime();
    public static final class MetroNoteParserControlPortamentoTime extends MidiNoteListParserElement<MetroMidiControlPortamentoTime> {
        {
            this.midi = MetroMidi.MIDI_PORTAMENTO_TIME;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_portamentoTime( offset, port, channel, value ) );
        }
        public LList cc_portamentoTime(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_PORTAMENTO_TIME ); }

    public static final MetroNoteParserControlDataEntryMsb PARSER_DATA_ENTRY_MSB  = new MetroNoteParserControlDataEntryMsb();
    public static final class MetroNoteParserControlDataEntryMsb extends MidiNoteListParserElement<MetroMidiControlDataEntryMsb> {
        {
            this.midi = MetroMidi.MIDI_DATA_ENTRY_MSB;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_dataEntryMsb( offset, port, channel, value ) );
        }
        public LList cc_dataEntryMsb(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_DATA_ENTRY_MSB ); }

    public static final MetroNoteParserControlVolume PARSER_VOLUME  = new MetroNoteParserControlVolume();
    public static final class MetroNoteParserControlVolume extends MidiNoteListParserElement<MetroMidiControlVolume> {
        {
            this.midi = MetroMidi.MIDI_VOLUME;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_volume( offset, port, channel, value ) );
        }
        public LList cc_volume(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_VOLUME ); }

    public static final MetroNoteParserControlBalance PARSER_BALANCE  = new MetroNoteParserControlBalance();
    public static final class MetroNoteParserControlBalance extends MidiNoteListParserElement<MetroMidiControlBalance> {
        {
            this.midi = MetroMidi.MIDI_BALANCE;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_balance( offset, port, channel, value ) );
        }
        public LList cc_balance(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_BALANCE ); }

    public static final MetroNoteParserControlPan PARSER_PAN  = new MetroNoteParserControlPan();
    public static final class MetroNoteParserControlPan extends MidiNoteListParserElement<MetroMidiControlPan> {
        {
            this.midi = MetroMidi.MIDI_PAN;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_pan( offset, port, channel, value ) );
        }
        public LList cc_pan(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_PAN ); }

    public static final MetroNoteParserControlExpression PARSER_EXPRESSION  = new MetroNoteParserControlExpression();
    public static final class MetroNoteParserControlExpression extends MidiNoteListParserElement<MetroMidiControlExpression> {
        {
            this.midi = MetroMidi.MIDI_EXPRESSION;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_expression( offset, port, channel, value ) );
        }
        public LList cc_expression(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_EXPRESSION ); }

    public static final MetroNoteParserControlEffectController1 PARSER_EFFECT_CTRL_1  = new MetroNoteParserControlEffectController1();
    public static final class MetroNoteParserControlEffectController1 extends MidiNoteListParserElement<MetroMidiControlEffectController1> {
        {
            this.midi = MetroMidi.MIDI_EFFECT_CTRL_1;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_effectController1( offset, port, channel, value ) );
        }
        public LList cc_effectController1(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_EFFECT_CTRL_1 ); }

    public static final MetroNoteParserControlEffectController2 PARSER_EFFECT_CTRL_2  = new MetroNoteParserControlEffectController2();
    public static final class MetroNoteParserControlEffectController2 extends MidiNoteListParserElement<MetroMidiControlEffectController2> {
        {
            this.midi = MetroMidi.MIDI_EFFECT_CTRL_2;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_effectController2( offset, port, channel, value ) );
        }
        public LList cc_effectController2(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_EFFECT_CTRL_2 ); }

    public static final MetroNoteParserControlSustainPedal PARSER_SUSTAIN_PEDAL  = new MetroNoteParserControlSustainPedal();
    public static final class MetroNoteParserControlSustainPedal extends MidiNoteListParserElement<MetroMidiControlSustainPedal> {
        {
            this.midi = MetroMidi.MIDI_SUSTAIN_PEDAL;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_sustainPedal( offset, port, channel, value ) );
        }
        public LList cc_sustainPedal(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_SUSTAIN_PEDAL ); }

    public static final MetroNoteParserControlPortamentoSwitch PARSER_PORTAMENTO_SWITCH  = new MetroNoteParserControlPortamentoSwitch();
    public static final class MetroNoteParserControlPortamentoSwitch extends MidiNoteListParserElement<MetroMidiControlPortamentoSwitch> {
        {
            this.midi = MetroMidi.MIDI_PORTAMENTO_SWITCH;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_portamentoSwitch( offset, port, channel, value ) );
        }
        public LList cc_portamentoSwitch(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_PORTAMENTO_SWITCH ); }

    public static final MetroNoteParserControlSostenutoSwitch PARSER_SOSTENUTO_SWITCH  = new MetroNoteParserControlSostenutoSwitch();
    public static final class MetroNoteParserControlSostenutoSwitch extends MidiNoteListParserElement<MetroMidiControlSostenutoSwitch> {
        {
            this.midi = MetroMidi.MIDI_SOSTENUTO_SWITCH;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_sostenutoSwitch( offset, port, channel, value ) );
        }
        public LList cc_sostenutoSwitch(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_SOSTENUTO_SWITCH ); }

    public static final MetroNoteParserControlPedalSwitch PARSER_SOFT_PEDAL_SWITCH  = new MetroNoteParserControlPedalSwitch();
    public static final class MetroNoteParserControlPedalSwitch extends MidiNoteListParserElement<MetroMidiControlPedalSwitch> {
        {
            this.midi = MetroMidi.MIDI_SOFT_PEDAL_SWITCH;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_pedalSwitch( offset, port, channel, value ) );
        }
        public LList cc_pedalSwitch(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
                );
        }
    }
    static { register( PARSER_SOFT_PEDAL_SWITCH ); }

    public static final MetroNoteParserControlLegatoSwitch PARSER_LEGATO_FOOTSWITCH  = new MetroNoteParserControlLegatoSwitch();
    public static final class MetroNoteParserControlLegatoSwitch extends MidiNoteListParserElement<MetroMidiControlLegatoSwitch> {
        {
            this.midi = MetroMidi.MIDI_LEGATO_FOOTSWITCH;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_legatoSwitch( offset, port, channel, value ) );
        }
        public LList cc_legatoSwitch(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_LEGATO_FOOTSWITCH ); }

    public static final MetroNoteParserControlHold2 PARSER_HOLD_2  = new MetroNoteParserControlHold2();
    public static final class MetroNoteParserControlHold2 extends MidiNoteListParserElement<MetroMidiControlHold2> {
        {
            this.midi = MetroMidi.MIDI_HOLD_2;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_hold2( offset, port, channel, value ) );
        }
        public LList cc_hold2(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_HOLD_2 ); }

    public static final MetroNoteParserControlSoundController1 PARSER_SOUND_CTRL_01  = new MetroNoteParserControlSoundController1();
    public static final class MetroNoteParserControlSoundController1 extends MidiNoteListParserElement<MetroMidiControlSoundController1> {
        {
            this.midi = MetroMidi.MIDI_SOUND_CTRL_01;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_soundController1( offset, port, channel, value ) );
        }
        public LList cc_soundController1(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_SOUND_CTRL_01 ); }

    public static final MetroNoteParserControlSoundController2 PARSER_SOUND_CTRL_02  = new MetroNoteParserControlSoundController2();
    public static final class MetroNoteParserControlSoundController2 extends MidiNoteListParserElement<MetroMidiControlSoundController2> {
        {
            this.midi = MetroMidi.MIDI_SOUND_CTRL_02;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_soundController2( offset, port, channel, value ) );
        }
        public LList cc_soundController2(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_SOUND_CTRL_02 ); }

    public static final MetroNoteParserControlSoundController3 PARSER_SOUND_CTRL_03  = new MetroNoteParserControlSoundController3();
    public static final class MetroNoteParserControlSoundController3 extends MidiNoteListParserElement<MetroMidiControlSoundController3> {
        {
            this.midi = MetroMidi.MIDI_SOUND_CTRL_03;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_soundController3( offset, port, channel, value ) );
        }
        public LList cc_soundController3(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_SOUND_CTRL_03 ); }

    public static final MetroNoteParserControlSoundController4 PARSER_SOUND_CTRL_04  = new MetroNoteParserControlSoundController4();
    public static final class MetroNoteParserControlSoundController4 extends MidiNoteListParserElement<MetroMidiControlSoundController4> {
        {
            this.midi = MetroMidi.MIDI_SOUND_CTRL_04;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_soundController4( offset, port, channel, value ) );
        }
        public LList cc_soundController4(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_SOUND_CTRL_04 ); }

    public static final MetroNoteParserControlSoundController5 PARSER_SOUND_CTRL_05  = new MetroNoteParserControlSoundController5();
    public static final class MetroNoteParserControlSoundController5 extends MidiNoteListParserElement<MetroMidiControlSoundController5> {
        {
            this.midi = MetroMidi.MIDI_SOUND_CTRL_05;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_soundController5( offset, port, channel, value ) );
        }
        public LList cc_soundController5(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_SOUND_CTRL_05 ); }

    public static final MetroNoteParserControlSoundController6 PARSER_SOUND_CTRL_06  = new MetroNoteParserControlSoundController6();
    public static final class MetroNoteParserControlSoundController6 extends MidiNoteListParserElement<MetroMidiControlSoundController6> {
        {
            this.midi = MetroMidi.MIDI_SOUND_CTRL_06;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_soundController6( offset, port, channel, value ) );
        }
        public LList cc_soundController6(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_SOUND_CTRL_06 ); }

    public static final MetroNoteParserControlSoundController7 PARSER_SOUND_CTRL_07  = new MetroNoteParserControlSoundController7();
    public static final class MetroNoteParserControlSoundController7 extends MidiNoteListParserElement<MetroMidiControlSoundController7> {
        {
            this.midi = MetroMidi.MIDI_SOUND_CTRL_07;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_soundController7( offset, port, channel, value ) );
        }
        public LList cc_soundController7(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_SOUND_CTRL_07 ); }

    public static final MetroNoteParserControlSoundController8 PARSER_SOUND_CTRL_08  = new MetroNoteParserControlSoundController8();
    public static final class MetroNoteParserControlSoundController8 extends MidiNoteListParserElement<MetroMidiControlSoundController8> {
        {
            this.midi = MetroMidi.MIDI_SOUND_CTRL_08;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_soundController8( offset, port, channel, value ) );
        }
        public LList cc_soundController8(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_SOUND_CTRL_08 ); }

    public static final MetroNoteParserControlSoundController9 PARSER_SOUND_CTRL_09  = new MetroNoteParserControlSoundController9();
    public static final class MetroNoteParserControlSoundController9 extends MidiNoteListParserElement<MetroMidiControlSoundController9> {
        {
            this.midi = MetroMidi.MIDI_SOUND_CTRL_09;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_soundController9( offset, port, channel, value ) );
        }
        public LList cc_soundController9(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_SOUND_CTRL_09 ); }

    public static final MetroNoteParserControlSoundController10 PARSER_SOUND_CTRL_10  = new MetroNoteParserControlSoundController10();
    public static final class MetroNoteParserControlSoundController10 extends MidiNoteListParserElement<MetroMidiControlSoundController10> {
        {
            this.midi = MetroMidi.MIDI_SOUND_CTRL_10;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_soundController10( offset, port, channel, value ) );
        }
        public LList cc_soundController10(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_SOUND_CTRL_10 ); }

    public static final MetroNoteParserControlGeneralPurpose01 PARSER_GENERAL_PURPOSE_01  = new MetroNoteParserControlGeneralPurpose01();
    public static final class MetroNoteParserControlGeneralPurpose01 extends MidiNoteListParserElement<MetroMidiControlGeneralPurpose01> {
        {
            this.midi = MetroMidi.MIDI_GENERAL_PURPOSE_01;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_generalPurpose01( offset, port, channel, value ) );
        }
        public LList cc_generalPurpose01(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_GENERAL_PURPOSE_01 ); }

    public static final MetroNoteParserControlGeneralPurpose02 PARSER_GENERAL_PURPOSE_02  = new MetroNoteParserControlGeneralPurpose02();
    public static final class MetroNoteParserControlGeneralPurpose02 extends MidiNoteListParserElement<MetroMidiControlGeneralPurpose02> {
        {
            this.midi = MetroMidi.MIDI_GENERAL_PURPOSE_02;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_generalPurpose02( offset, port, channel, value ) );
        }
        public LList cc_generalPurpose02(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_GENERAL_PURPOSE_02 ); }

    public static final MetroNoteParserControlGeneralPurpose03 PARSER_GENERAL_PURPOSE_03  = new MetroNoteParserControlGeneralPurpose03();
    public static final class MetroNoteParserControlGeneralPurpose03 extends MidiNoteListParserElement<MetroMidiControlGeneralPurpose03> {
        {
            this.midi = MetroMidi.MIDI_GENERAL_PURPOSE_03;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_generalPurpose03( offset, port, channel, value ) );
        }
        public LList cc_generalPurpose03(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_GENERAL_PURPOSE_03 ); }

    public static final MetroNoteParserControlGeneralPurpose04 PARSER_GENERAL_PURPOSE_04  = new MetroNoteParserControlGeneralPurpose04();
    public static final class MetroNoteParserControlGeneralPurpose04 extends MidiNoteListParserElement<MetroMidiControlGeneralPurpose04> {
        {
            this.midi = MetroMidi.MIDI_GENERAL_PURPOSE_04;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_generalPurpose04( offset, port, channel, value ) );
        }
        public LList cc_generalPurpose04(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_GENERAL_PURPOSE_04 ); }

    public static final MetroNoteParserControlPortamento PARSER_PORTAMENTO_CC_CTRL  = new MetroNoteParserControlPortamento();
    public static final class MetroNoteParserControlPortamento extends MidiNoteListParserElement<MetroMidiControlPortamento> {
        {
            this.midi = MetroMidi.MIDI_PORTAMENTO_CC_CTRL;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_portamento( offset, port, channel, value ) );
        }
        public LList cc_portamento(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_PORTAMENTO_CC_CTRL ); }

    public static final MetroNoteParserControlEffect1 PARSER_EFFECT_1_DEPTH  = new MetroNoteParserControlEffect1();
    public static final class MetroNoteParserControlEffect1 extends MidiNoteListParserElement<MetroMidiControlEffect1> {
        {
            this.midi = MetroMidi.MIDI_EFFECT_1_DEPTH;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_effect1( offset, port, channel, value ) );
        }
        public LList cc_effect1(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_EFFECT_1_DEPTH ); }

    public static final MetroNoteParserControlEffect2 PARSER_EFFECT_2_DEPTH  = new MetroNoteParserControlEffect2();
    public static final class MetroNoteParserControlEffect2 extends MidiNoteListParserElement<MetroMidiControlEffect2> {
        {
            this.midi = MetroMidi.MIDI_EFFECT_2_DEPTH;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_effect2( offset, port, channel, value ) );
        }
        public LList cc_effect2(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_EFFECT_2_DEPTH ); }

    public static final MetroNoteParserControlEffect3 PARSER_EFFECT_3_DEPTH  = new MetroNoteParserControlEffect3();
    public static final class MetroNoteParserControlEffect3 extends MidiNoteListParserElement<MetroMidiControlEffect3> {
        {
            this.midi = MetroMidi.MIDI_EFFECT_3_DEPTH;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_effect3( offset, port, channel, value ) );
        }
        public LList cc_effect3(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_EFFECT_3_DEPTH ); }

    public static final MetroNoteParserControlEffect4 PARSER_EFFECT_4_DEPTH  = new MetroNoteParserControlEffect4();
    public static final class MetroNoteParserControlEffect4 extends MidiNoteListParserElement<MetroMidiControlEffect4> {
        {
            this.midi = MetroMidi.MIDI_EFFECT_4_DEPTH;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_effect4( offset, port, channel, value ) );
        }
        public LList cc_effect4(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_EFFECT_4_DEPTH ); }

    public static final MetroNoteParserControlEffect5 PARSER_EFFECT_5_DEPTH  = new MetroNoteParserControlEffect5();
    public static final class MetroNoteParserControlEffect5 extends MidiNoteListParserElement<MetroMidiControlEffect5> {
        {
            this.midi = MetroMidi.MIDI_EFFECT_5_DEPTH;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_effect5( offset, port, channel, value ) );
        }
        public LList cc_effect5(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_EFFECT_5_DEPTH ); }

    public static final MetroNoteParserControlDataIncrement PARSER_DATA_INCREMENT  = new MetroNoteParserControlDataIncrement();
    public static final class MetroNoteParserControlDataIncrement extends MidiNoteListParserElement<MetroMidiControlDataIncrement> {
        {
            this.midi = MetroMidi.MIDI_DATA_INCREMENT;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_dataIncrement( offset, port, channel, value ) );
        }
        public LList cc_dataIncrement(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_DATA_INCREMENT ); }

    public static final MetroNoteParserControlDataDecrement PARSER_DATA_DECREMENT  = new MetroNoteParserControlDataDecrement();
    public static final class MetroNoteParserControlDataDecrement extends MidiNoteListParserElement<MetroMidiControlDataDecrement> {
        {
            this.midi = MetroMidi.MIDI_DATA_DECREMENT;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_dataDecrement( offset, port, channel, value ) );
        }
        public LList cc_dataDecrement(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_DATA_DECREMENT ); }

    public static final MetroNoteParserControlNrpnLsb PARSER_NRPN_LSB  = new MetroNoteParserControlNrpnLsb();
    public static final class MetroNoteParserControlNrpnLsb extends MidiNoteListParserElement<MetroMidiControlNrpnLsb> {
        {
            this.midi = MetroMidi.MIDI_NRPN_LSB;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_nrpnLsb( offset, port, channel, value ) );
        }
        public LList cc_nrpnLsb(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_NRPN_LSB ); }

    public static final MetroNoteParserControlNrpnMsb PARSER_NRPN_MSB  = new MetroNoteParserControlNrpnMsb();
    public static final class MetroNoteParserControlNrpnMsb extends MidiNoteListParserElement<MetroMidiControlNrpnMsb> {
        {
            this.midi = MetroMidi.MIDI_NRPN_MSB;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_nrpnMsb( offset, port, channel, value ) );
        }
        public LList cc_nrpnMsb(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_NRPN_MSB ); }

    public static final MetroNoteParserControlRpnLsb PARSER_RPN_LSB  = new MetroNoteParserControlRpnLsb();
    public static final class MetroNoteParserControlRpnLsb extends MidiNoteListParserElement<MetroMidiControlRpnLsb> {
        {
            this.midi = MetroMidi.MIDI_RPN_LSB;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T>  buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_rpnLsb( offset, port, channel, value ) );
        }
        public LList cc_rpnLsb(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_RPN_LSB ); }


    public static final MetroNoteParserControlRpnMsb PARSER_RPN_MSB  = new MetroNoteParserControlRpnMsb();
    public static final class MetroNoteParserControlRpnMsb extends MidiNoteListParserElement<MetroMidiControlRpnMsb> {
        {
            this.midi = MetroMidi.MIDI_RPN_MSB;
            this.parameters = Arrays.asList(
//              new NoteListParserElementParameter.Default(
//                  "","","","",
//                  "" )
                );
        }
        @Override
        public <T> void parseEvent(Metro metro, MetroTrack track, MetroBufferedMidiReceiver<T> buffer, NoteListMap map, MetroCollector<T> result ) {
            double offset    = readMapOffset( map );  
            MetroPort port   = readMapPort( map );
            int channel      = readMapChannel( map ); 
            int value        = readMapIntegerValueDefault0( map );

            result.add( buffer.cc_rpnMsb( offset, port, channel, value ) );
        }
        public LList cc_rpnMsb(double offset, MetroPort port, int channel, int value) {
            return list(
                writeMapType( name() ),
                writeMapOffset(offset),  
                writeMapPort(port),
                writeMapChannel(channel), 
                writeMapIntegerValueDefault0(value)
            );
        }
    }
    static { register( PARSER_RPN_MSB ); }

    public static void main(String[] args) {
        String name = PulsarMidiNoteListParsers.class.getName();
        for( java.lang.reflect.Field f : PulsarMidiNoteListParsers.class.getFields() ) {
            if ( java.lang.reflect.Modifier.isStatic( f.getModifiers() ) )  {
                System.out.println( name + "." +  f.getName() );
            }
        }
    }
}

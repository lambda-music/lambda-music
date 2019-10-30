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

/**
 * 
 * @see MetroBufferedMidiReceiver
 * 
 * @author Ats Oka
 * @param <T>
 */
public interface MetroMidiReceiver<T> {
    // basic
    public T error( String string );
    public T noteOn(int channel, int note, double velocity );
    public T noteOn(int channel, int note, int velocity );
    public T noteOff(int channel, int note, double velocity);
    public T noteOff(int channel, int note, int velocity );
    public T keyPressure(int channel, int note, double pressure);
    public T keyPressure(int channel, int note, int pressure);
    public T controlChange(int channel, int controlNumber, int controlValue );
    public T programChange( int ch, int programNumber );
    public T channelPressure( int ch, double pressureValue );
    public T channelPressure( int ch, int pressureValue );
    public T pitchBend( int ch, double pitchBendValue );
    public T pitchBend( int ch, int pitchBendValue );

    public T cc_allSoundOff( int ch ); 
    public T cc_resetAllControllers( int ch ); 
    public T cc_localControls( int ch, boolean on ); 
    public T cc_allNoteOff( int ch ); 
    public T cc_omniModeOff( int ch ); 
    public T cc_omniModeOn( int ch ); 
    public T cc_monoModeOn( int ch ); 
    public T cc_polyModeOn( int ch ); 

    // system
    public T songPositionPointer( int pos ); 
    public T songSelect( int songNumber ); 
    public T endOfExclusive(); 
    public T clock(); 
    public T start(); 
    public T cont(); 
    public T stop(); 
    public T reset();
    
    // control change
    public T cc_bankSelect( int channel, int value ) ;
    public T cc_modulation( int channel, int value ) ;
    public T cc_breathController( int channel, int value ) ;
    public T cc_footController( int channel, int value ) ;
    public T cc_portamentoTime( int channel, int value ) ;
    public T cc_dataEntryMsb( int channel, int value ) ;
    public T cc_volume( int channel, int value ) ;
    public T cc_balance( int channel, int value ) ;
    public T cc_pan( int channel, int value ) ;
    public T cc_expression( int channel, int value ) ;
    public T cc_effectController1( int channel, int value ) ;
    public T cc_effectController2( int channel, int value ) ;
    public T cc_sustainPedal( int channel, int value ) ;
    public T cc_portamentoSwitch( int channel, int value ) ;
    public T cc_sostenutoSwitch( int channel, int value ) ;
    public T cc_pedalSwitch( int channel, int value ) ;
    public T cc_legatoSwitch( int channel, int value ) ;
    public T cc_hold2( int channel, int value ) ;
    public T cc_soundController1( int channel, int value ) ;
    public T cc_soundController2( int channel, int value ) ;
    public T cc_soundController3( int channel, int value ) ;
    public T cc_soundController4( int channel, int value ) ;
    public T cc_soundController5( int channel, int value ) ;
    public T cc_soundController6( int channel, int value ) ;
    public T cc_soundController7( int channel, int value ) ;
    public T cc_soundController8( int channel, int value ) ;
    public T cc_soundController9( int channel, int value ) ;
    public T cc_soundController10( int channel, int value ) ;
    public T cc_generalPurpose01( int channel, int value ) ;
    public T cc_generalPurpose02( int channel, int value ) ;
    public T cc_generalPurpose03( int channel, int value ) ;
    public T cc_generalPurpose04( int channel, int value ) ;
    public T cc_portamento( int channel, int value ) ;
    public T cc_effect1( int channel, int value ) ;
    public T cc_effect2( int channel, int value ) ;
    public T cc_effect3( int channel, int value ) ;
    public T cc_effect4( int channel, int value ) ;
    public T cc_effect5( int channel, int value ) ;
    public T cc_dataIncrement( int channel, int value ) ;
    public T cc_dataDecrement( int channel, int value ) ;
    public T cc_nrpnLsb( int channel, int value ) ;
    public T cc_nrpnMsb( int channel, int value ) ;
    public T cc_rpnLsb( int channel, int value ) ;
    public T cc_rpnMsb( int channel, int value ) ;
    
    static abstract class Default<T> implements MetroMidiReceiver<T> {
        protected abstract T defaultValue();
        @Override
        public T error(String string) {
            return defaultValue();
        }
        @Override
        public T noteOn(int channel, int note, int velocity) {
            return defaultValue();
        }
        @Override
        public T noteOff(int channel, int note, int velocity) {
            return defaultValue();
        }
        @Override
        public T keyPressure(int channel, int note, int pressure) {
            return defaultValue();
        }
        @Override
        public T controlChange(int channel, int controlNumber, int controlValue) {
            return defaultValue();
        }
        @Override
        public T programChange(int ch, int programNumber) {
            return defaultValue();
        }
        @Override
        public T channelPressure(int ch, int pressureValue) {
            return defaultValue();
        }
        @Override
        public T pitchBend(int ch, int pitchBendValue) {
            return defaultValue();
        }
        @Override
        public T noteOn(int channel, int note, double velocity) {
            return defaultValue();
        }
        @Override
        public T noteOff(int channel, int note, double velocity) {
            return defaultValue();
        }
        @Override
        public T keyPressure(int channel, int note, double pressure) {
            return defaultValue();
        }
        @Override
        public T channelPressure(int ch, double pressureValue) {
            return defaultValue();
        }
        @Override
        public T pitchBend(int ch, double pitchBendValue) {
            return defaultValue();
        }

        
        @Override
        public T cc_allSoundOff(int ch) {
            return defaultValue();
        }
        @Override
        public T cc_resetAllControllers(int ch) {
            return defaultValue();
        }
        @Override
        public T cc_localControls(int ch, boolean on) {
            return defaultValue();
        }
        @Override
        public T cc_allNoteOff(int ch) {
            return defaultValue();
        }
        @Override
        public T cc_omniModeOff(int ch) {
            return defaultValue();
        }
        @Override
        public T cc_omniModeOn(int ch) {
            return defaultValue();
        }
        @Override
        public T cc_monoModeOn(int ch) {
            return defaultValue();
        }
        @Override
        public T cc_polyModeOn(int ch) {
            return defaultValue();
        }
        @Override
        public T songPositionPointer(int pos) {
            return defaultValue();
        }
        @Override
        public T songSelect(int songNumber) {
            return defaultValue();
        }
        @Override
        public T endOfExclusive() {
            return defaultValue();
        }
        @Override
        public T clock() {
            return defaultValue();
        }
        @Override
        public T start() {
            return defaultValue();
        }
        @Override
        public T cont() {
            return defaultValue();
        }
        @Override
        public T stop() {
            return defaultValue();
        }
        @Override
        public T reset() {
            return defaultValue();
        }

        @Override
        public T cc_bankSelect( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_modulation( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_breathController( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_footController( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_portamentoTime( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_dataEntryMsb( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_volume( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_balance( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_pan( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_expression( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_effectController1( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_effectController2( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_sustainPedal( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_portamentoSwitch( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_sostenutoSwitch( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_pedalSwitch( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_legatoSwitch( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_hold2( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_soundController1( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_soundController2( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_soundController3( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_soundController4( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_soundController5( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_soundController6( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_soundController7( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_soundController8( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_soundController9( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_soundController10( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_generalPurpose01( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_generalPurpose02( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_generalPurpose03( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_generalPurpose04( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_portamento( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_effect1( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_effect2( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_effect3( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_effect4( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_effect5( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_dataIncrement( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_dataDecrement( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_nrpnLsb( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_nrpnMsb( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_rpnLsb( int channel, int value ) {
            return defaultValue();
        }
        @Override
        public T cc_rpnMsb( int channel, int value ) {
            return defaultValue();
        }
    }
    
    static abstract class Formatter<T> implements MetroMidiReceiver<T> {
        protected abstract T format( String method, String params, Object ... args );
        @Override
        public T error(String string) {
            return format("error" , "String string", string );
        }
        @Override
        public T noteOn(int channel, int note, int velocity) {
            return format( "noteOn", "int channel, int note, int velocity",channel,note,velocity );
        }
        @Override
        public T noteOff(int channel, int note, int velocity) {
            return format( "noteOff", "int channel, int note, int velocity",channel,note,velocity );
        }
        @Override
        public T keyPressure(int channel, int note, int pressure) {
            return format( "keyPressure", "int channel, int note, int pressure",channel,note,pressure );
        }
        @Override
        public T controlChange(int channel, int controlNumber, int controlValue) {
            return format( "controlChange", "int channel, int controlNumber, int controlValue",channel,controlNumber,controlValue );
        }
        @Override
        public T programChange(int ch, int programNumber) {
            return format( "programChange", "int ch, int programNumber",ch,programNumber );
        }
        @Override
        public T channelPressure(int ch, int pressureValue) {
            return format( "channelPressure", "int ch, int pressureValue",ch,pressureValue );
        }
        @Override
        public T pitchBend(int ch, int pitchBendValue) {
            return format( "pitchBend", "int ch, int pitchBendValue",ch,pitchBendValue );
        }
        @Override
        public T noteOn(int channel, int note, double velocity) {
            return format( "noteOn", "int channel, int note, double velocity",channel,note,velocity );
        }
        @Override
        public T noteOff(int channel, int note, double velocity) {
            return format( "noteOff", "int channel, int note, double velocity",channel,note,velocity );
        }
        @Override
        public T keyPressure(int channel, int note, double pressure) {
            return format( "keyPressure", "int channel, int note, double pressure",channel,note,pressure );
        }
        @Override
        public T channelPressure(int ch, double pressureValue) {
            return format( "channelPressure", "int ch, double pressureValue",ch,pressureValue );
        }
        @Override
        public T pitchBend(int ch, double pitchBendValue) {
            return format( "pitchBend", "int ch, double pitchBendValue",ch,pitchBendValue );
        }

        
        @Override
        public T cc_allSoundOff(int ch) {
            return format( "cc_allSoundOff", "int ch",ch );
        }
        @Override
        public T cc_resetAllControllers(int ch) {
            return format( "cc_resetAllControllers", "int ch",ch );
        }
        @Override
        public T cc_localControls(int ch, boolean on) {
            return format( "cc_localControls", "int ch, boolean on",ch,on );
        }
        @Override
        public T cc_allNoteOff(int ch) {
            return format( "cc_allNoteOff", "int ch",ch );
        }
        @Override
        public T cc_omniModeOff(int ch) {
            return format( "cc_omniModeOff", "int ch",ch );
        }
        @Override
        public T cc_omniModeOn(int ch) {
            return format( "cc_omniModeOn", "int ch",ch );
        }
        @Override
        public T cc_monoModeOn(int ch) {
            return format( "cc_monoModeOn", "int ch",ch );
        }
        @Override
        public T cc_polyModeOn(int ch) {
            return format( "cc_polyModeOn", "int ch",ch );
        }
        @Override
        public T songPositionPointer(int pos) {
            return format( "songPositionPointer", "int pos",pos );
        }
        @Override
        public T songSelect(int songNumber) {
            return format( "songSelect", "int songNumber",songNumber );
        }
        @Override
        public T endOfExclusive() {
            return format( "endOfExclusive", "" );
        }
        @Override
        public T clock() {
            return format( "clock", "" );
        }
        @Override
        public T start() {
            return format( "start", "" );
        }
        @Override
        public T cont() {
            return format( "cont", "" );
        }
        @Override
        public T stop() {
            return format( "stop", "" );
        }
        @Override
        public T reset() {
            return format( "reset", "" );
        }

        @Override
        public T cc_bankSelect( int channel, int value ) {
            return format( "cc_bankSelect", " int channel, int value ", channel,value  );
        }
        @Override
        public T cc_modulation( int channel, int value ) {
            return format( "cc_modulation", " int channel, int value ", channel,value  );
        }
        @Override
        public T cc_breathController( int channel, int value ) {
            return format( "cc_breathController", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_footController( int channel, int value ) {
            return format( "cc_footController", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_portamentoTime( int channel, int value ) {
            return format( "cc_portamentoTime", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_dataEntryMsb( int channel, int value ) {
            return format( "cc_dataEntryMsb", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_volume( int channel, int value ) {
            return format( "cc_volume", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_balance( int channel, int value ) {
            return format( "cc_balance", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_pan( int channel, int value ) {
            return format( "cc_pan", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_expression( int channel, int value ) {
            return format( "cc_expression", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_effectController1( int channel, int value ) {
            return format( "cc_effectController1", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_effectController2( int channel, int value ) {
            return format( "cc_effectController2", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_sustainPedal( int channel, int value ) {
            return format( "cc_sustainPedal", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_portamentoSwitch( int channel, int value ) {
            return format( "cc_portamentoSwitch", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_sostenutoSwitch( int channel, int value ) {
            return format( "cc_sostenutoSwitch", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_pedalSwitch( int channel, int value ) {
            return format( "cc_pedalSwitch", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_legatoSwitch( int channel, int value ) {
            return format( "cc_legatoSwitch", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_hold2( int channel, int value ) {
            return format( "cc_hold2", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_soundController1( int channel, int value ) {
            return format( "cc_soundController1", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_soundController2( int channel, int value ) {
            return format( "cc_soundController2", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_soundController3( int channel, int value ) {
            return format( "cc_soundController3", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_soundController4( int channel, int value ) {
            return format( "cc_soundController4", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_soundController5( int channel, int value ) {
            return format( "cc_soundController5", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_soundController6( int channel, int value ) {
            return format( "cc_soundController6", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_soundController7( int channel, int value ) {
            return format( "cc_soundController7", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_soundController8( int channel, int value ) {
            return format( "cc_soundController8", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_soundController9( int channel, int value ) {
            return format( "cc_soundController9", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_soundController10( int channel, int value ) {
            return format( "cc_soundController10", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_generalPurpose01( int channel, int value ) {
            return format( "cc_generalPurpose01", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_generalPurpose02( int channel, int value ) {
            return format( "cc_generalPurpose02", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_generalPurpose03( int channel, int value ) {
            return format( "cc_generalPurpose03", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_generalPurpose04( int channel, int value ) {
            return format( "cc_generalPurpose04", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_portamento( int channel, int value ) {
            return format( "cc_portamento", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_effect1( int channel, int value ) {
            return format( "cc_effect1", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_effect2( int channel, int value ) {
            return format( "cc_effect2", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_effect3( int channel, int value ) {
            return format( "cc_effect3", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_effect4( int channel, int value ) {
            return format( "cc_effect4", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_effect5( int channel, int value ) {
            return format( "cc_effect5", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_dataIncrement( int channel, int value ) {
            return format( "cc_dataIncrement", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_dataDecrement( int channel, int value ) {
            return format( "cc_dataDecrement", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_nrpnLsb( int channel, int value ) {
            return format( "cc_nrpnLsb", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_nrpnMsb( int channel, int value ) {
            return format( "cc_nrpnMsb", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_rpnLsb( int channel, int value ) {
            return format( "cc_rpnLsb", " int channel, int value ",channel,value  );
        }
        @Override
        public T cc_rpnMsb( int channel, int value ) {
            return format( "cc_rpnMsb", " int channel, int value ", channel, value  );
        }
    }
    public static class LoggingToError extends Formatter<String> {
        private static final LoggingToError INSTANCE = new LoggingToError();
        public static LoggingToError getInstance() {
            return INSTANCE;
        }
        @Override
        protected String format(String method, String params, Object... args) {
            StringBuilder sb = new StringBuilder();
            for ( int i=0; i<args.length; i++ ) {
                sb.append( "%s , " );
            }
            if ( sb.length() != 0 ) 
                sb.setLength( sb.length() - 3 );
            
            String format = sb.toString();
            return method + "(" + params + ") " + String.format( format, args );
        }
    }
}

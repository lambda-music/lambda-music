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
}
